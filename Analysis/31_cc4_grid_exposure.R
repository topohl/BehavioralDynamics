# Stage 31 -- CC4 grid-associated period, descriptive and manually invoked.
# Actual insertion/removal times were not recorded. The activity scan produces
# unverified candidates only; it never produces exposure labels or effect tests.
# Run from repo root with MMM_GRID31_RUN_ID set to a NEW run name. Optional:
# MMM_GRID31_CENSOR_MANIFEST=<released spatial exclusion manifest version> applies
# its CC4 censor records (opt-in recording-duration censoring; unset = no censoring);
# MMM_GRID31_OUTPUT_ROOT=<absolute isolated root> writes <root>/<run ID> instead.

source("Analysis/_pipeline_setup.R")
for (helper in c("project_paths.R", "E9_SIS_AnimalPos-functions.R",
                 "animalpos_preprocessing_helpers.R", "cc4_grid_exposure_helpers.R",
                 "cc4_whole_inactive_helpers.R")) source_mmm_helper(helper)

run_cc4_grid_exposure <- function(data_dir, output_dir, corrections_file,
                                 corrections_sha256, batches = paste0("B", 1:6), make_plots = TRUE,
                                 censor_manifest = NULL) {
  if (file.exists(output_dir)) stop("Output already exists; choose a new run ID: ", output_dir, call. = FALSE)
  if (!length(batches) || anyDuplicated(batches) || !all(batches %in% paste0("B", 1:6))) stop("Invalid batches.", call. = FALSE)
  excluded_file <- file.path(data_dir, "raw_data", "excluded_animals.csv")
  raw_files <- file.path(data_dir, "raw_data", batches, paste0("E9_SIS_", batches, "_CC4_AnimalPos.csv"))
  code_files <- file.path(MMM_REPO_ROOT, c("Analysis/31_cc4_grid_exposure.R", "Functions/cc4_grid_exposure_helpers.R",
    "Functions/cc4_whole_inactive_helpers.R",
    "Functions/animalpos_preprocessing_helpers.R", "Functions/E9_SIS_AnimalPos-functions.R",
    "Functions/behavioral_dynamics_helpers.R", "Functions/project_paths.R", "Functions/behavior_analysis_config.R",
    "Analysis/_pipeline_setup.R"))
  # Opt-in censoring reads a released manifest version only (pinned content hash).
  censor_files <- character()
  if (!is.null(censor_manifest)) {
    if (!is.character(censor_manifest) || length(censor_manifest) != 1L || is.na(censor_manifest) || !nzchar(censor_manifest))
      stop("censor_manifest must be one manifest version.", call. = FALSE)
    source_mmm_helper("spatial_exclusion_manifest.R")
    if (!censor_manifest %in% names(SXM_RELEASED) || identical(unname(SXM_RELEASED[censor_manifest]), "PENDING"))
      stop("Censor manifest ", censor_manifest, " is not a released version.", call. = FALSE)
    censor_files <- c(sxm_path(censor_manifest), file.path(MMM_REPO_ROOT, "Functions/spatial_exclusion_manifest.R"))
  }
  inputs <- c(raw_files, excluded_file, corrections_file, code_files, censor_files)
  if (!all(file.exists(inputs))) stop("Missing required input: ", paste(inputs[!file.exists(inputs)], collapse = ", "), call. = FALSE)
  hashes <- mmm_file_sha256(inputs)
  if (anyNA(hashes)) stop("SHA256 hashing requires the existing digest package.", call. = FALSE)
  if (length(corrections_sha256) != 1 || is.na(corrections_sha256) ||
      !identical(unname(mmm_file_sha256(corrections_file)), corrections_sha256)) stop("Cage correction hash mismatch.", call. = FALSE)
  censor <- NULL
  if (!is.null(censor_manifest)) {
    m <- sxm_read(censor_manifest)
    m <- m[m$action == "censor" & m$CC == "CC4" & m$Batch %in% batches, ]
    censor <- tibble::tibble(Batch = m$Batch, CageChange = m$CC, System = m$System, CageEpisodeID = m$CageEpisodeID,
      CensorFrom = as.POSIXct(sxm_time(m$from), origin = "1970-01-01", tz = "UTC"), CensorRecord = m$record_id,
      ManifestFrom = m$from, Animals = m$animals)
  }
  corrections <- readr::read_csv(corrections_file, col_types = readr::cols(.default = readr::col_character()))
  excluded <- trimws(readLines(excluded_file, warn = FALSE)); excluded <- excluded[nzchar(excluded)]
  recordings <- coverage <- profiles <- accounting <- applied <- list()
  ms <- function(x) round(as.numeric(x) * 1000)   # logger clock resolution
  for (batch in batches) {
    result <- preprocess_animalpos_file(batch, "CC4", excluded, file.path(data_dir, "raw_data"), output_dir,
                                       write_output = FALSE, phase_policy = "full_recording")
    if (is.null(result)) stop("Empty CC4 recording: ", batch, call. = FALSE)
    d <- grid31_correct_systems(result$data, corrections)
    d$AnimalNum <- canonical_animal_id(d$AnimalID)
    if (anyNA(d$AnimalNum) || any(!nzchar(d$AnimalNum))) stop("Invalid canonical identity.", call. = FALSE)
    alias <- d %>% dplyr::distinct(AnimalID, AnimalNum) %>% dplyr::count(AnimalNum)
    if (any(alias$n != 1)) stop("Ambiguous raw identity aliases in ", batch, call. = FALSE)
    # Fail closed: a censor record must name an uncorrected cage episode of this
    # recording, with the manifest's animals, a time inside the session, no vendor
    # row after that time, and the cage's last vendor record at that time (within
    # 5 ms: the vendor export closes each tag at its last read). Rows are never dropped.
    for (i in which(censor$Batch == batch)) {
      label <- paste0("Censor record ", censor$CensorRecord[i], " (", censor$CageEpisodeID[i], "): ")
      ix <- d$CageChange == censor$CageChange[i] & d$System == censor$System[i]
      if (!any(ix)) stop(label, "cage episode not in the data.", call. = FALSE)
      if (!identical(ix, d$CageChange == censor$CageChange[i] & d$System_raw == censor$System[i]))
        stop(label, "a cage correction moves animals into or out of the cage episode.", call. = FALSE)
      if (!setequal(unique(d$AnimalNum[ix]), canonical_animal_id(strsplit(censor$Animals[i], ";", fixed = TRUE)[[1]])))
        stop(label, "animals differ from the manifest.", call. = FALSE)
      if (!(ms(censor$CensorFrom[i]) > ms(min(d$DateTime)) && ms(censor$CensorFrom[i]) < ms(max(d$DateTime))))
        stop(label, "censor time lies outside the session.", call. = FALSE)
      if (any(ms(d$DateTime[ix]) > ms(censor$CensorFrom[i]))) stop(label, "vendor rows after the censor time.", call. = FALSE)
      if (ms(censor$CensorFrom[i]) - max(ms(d$DateTime[ix])) > 5)
        stop(label, "the censor time is not the cage's last vendor record (", format(max(d$DateTime[ix]), "%Y-%m-%d %H:%M:%OS3", tz = "UTC"), ").", call. = FALSE)
    }
    d$Sex <- if (batch %in% c("B1", "B2", "B5")) "Male" else "Female"
    d$SourceFile <- basename(raw_files[match(batch, batches)])
    cov <- grid31_phase_coverage(d, censor)
    prof <- grid31_clock_profiles(d, cov, censor)
    cov <- cov %>% dplyr::left_join(dplyr::distinct(d, AnimalID, AnimalNum, Sex), by = "AnimalID")
    prof$animals <- prof$animals %>% dplyr::left_join(dplyr::distinct(d, AnimalID, AnimalNum, Sex), by = "AnimalID")
    prof$cages$Sex <- d$Sex[1]
    if (!is.null(censor)) {
      # New columns last, so every legacy column keeps its position.
      cov <- dplyr::relocate(cov, dplyr::any_of(GRID31_CENSOR_COLUMNS), .after = dplyr::last_col())
      prof$animals <- dplyr::relocate(prof$animals, dplyr::any_of(GRID31_CENSOR_COLUMNS), .after = dplyr::last_col())
      prof$cages <- dplyr::relocate(prof$cages, dplyr::any_of(GRID31_CENSOR_COLUMNS), .after = dplyr::last_col())
      end <- max(d$DateTime)
      for (i in which(censor$Batch == batch)) {
        from <- censor$CensorFrom[i]
        a <- prof$animals %>% dplyr::filter(CageChange == censor$CageChange[i], System == censor$System[i], CensoredSec > 0)
        g <- prof$cages %>% dplyr::filter(CageChange == censor$CageChange[i], System == censor$System[i], CensoredAnimals > 0)
        hours <- sum(pmax(0, pmin(as.numeric(a$BinEnd), as.numeric(end)) - pmax(as.numeric(a$BinStart), as.numeric(from)))) / 3600
        animals_n <- dplyr::n_distinct(a$AnimalID)
        if (!isTRUE(all.equal(hours, animals_n * (as.numeric(end) - as.numeric(from)) / 3600)))
          stop("Censored bins do not tile the censored span of ", censor$CageEpisodeID[i], call. = FALSE)
        i5 <- unique(cov$BlockEnd[cov$System == censor$System[i] & cov$PhaseLabel == "I5"])
        applied[[length(applied)+1L]] <- tibble::tibble(RecordId = censor$CensorRecord[i], CageEpisodeID = censor$CageEpisodeID[i],
          ManifestVersion = censor_manifest, ManifestFrom = censor$ManifestFrom[i], From = from, RecordingEnd = end,
          Animals = animals_n, CensoredAnimalBinsInRecording = sum(a$BinStart < end),
          CensoredCageBinsInRecording = sum(g$BinStart < end), CensoredAnimalHours = hours,
          CensoredHoursPerAnimal = (as.numeric(end) - as.numeric(from)) / 3600,
          VendorRowsAfterFrom = sum(ms(d$DateTime[d$CageChange == censor$CageChange[i] & d$System == censor$System[i]]) > ms(from)),
          PhaseTrimmedSpanEnd = if (length(i5) == 1L) i5 else as.POSIXct(NA, tz = "UTC"))
      }
    }
    recordings[[batch]] <- d; coverage[[batch]] <- cov; profiles[[batch]] <- prof
    result$data <- NULL
    accounting[[batch]] <- tibble::as_tibble(result)
    message(batch, ": retained ", nrow(d), " genuine rows, A5=", sum(d$ConsecActive == 5), "; grid times unknown")
  }
  cov <- dplyr::bind_rows(coverage)
  cages <- dplyr::bind_rows(lapply(profiles, `[[`, "cages"))
  animals <- dplyr::bind_rows(lapply(profiles, `[[`, "animals"))
  candidates <- grid31_candidates(cages)
  whole <- grid31_whole_inactive(animals)
  uncertainty <- grid31_whole_inactive_uncertainty(whole$cages)
  # Fail before writing if any source changed during the read.
  if (!identical(hashes, mmm_file_sha256(inputs))) stop("Input changed during Stage 31.", call. = FALSE)
  if (!dir.create(output_dir, recursive = TRUE)) stop("Could not create new output directory.", call. = FALSE)
  for (sub in c("full_recording", "tables", "audit", "figures")) dir.create(file.path(output_dir, sub))
  write_table <- function(x, name, sub = "tables") {
    # Six decimals avoid the legacy millisecond serialization truncation/ties.
    for (nm in names(x)) if (inherits(x[[nm]], "POSIXct")) x[[nm]] <- format(x[[nm]], "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    readr::write_csv(x, file.path(output_dir, sub, name), na = "")
  }
  write_table(tibble::tibble(Path = inputs, SHA256 = hashes), "input_manifest.csv", "audit")
  write_table(dplyr::bind_rows(accounting), "row_accounting.csv", "audit")
  for (batch in batches) write_table(recordings[[batch]], paste0("E9_SIS_", batch, "_CC4_AnimalPos_full_recording.csv"), "full_recording")
  write_table(cov, "phase_animal_coverage.csv", "audit")
  write_table(animals, "animal_clock_profiles_5min.csv")
  write_table(cages, "cage_clock_profiles_5min.csv")
  write_table(whole$animals, "whole_inactive_animal_activity.csv")
  write_table(whole$cages, "whole_inactive_cage_activity.csv")
  write_table(whole$animal_pairs, "whole_inactive_animal_changes_from_I2.csv")
  write_table(whole$cage_pairs, "whole_inactive_cage_changes_from_I2.csv")
  write_table(whole$population, "whole_inactive_population.csv", "audit")
  write_table(uncertainty$summary, "whole_inactive_sex_summary.csv")
  write_table(uncertainty$status, "whole_inactive_bootstrap_status.csv", "audit")
  write_table(uncertainty$draws, "whole_inactive_bootstrap_draws.csv", "audit")
  write_table(candidates$eligibility, "candidate_scan_eligibility.csv", "audit")
  write_table(candidates$scan, "all_two_hour_candidate_scores.csv", "audit")
  write_table(candidates$candidates, "activity_inferred_candidates_UNVERIFIED.csv")
  # This is a review scaffold, not a populated exposure schedule. No candidate
  # timestamp is copied into actual insertion/removal fields.
  timing <- cov %>% dplyr::filter(Phase == "Inactive", PhaseNumber %in% 3:5) %>%
    dplyr::distinct(Batch, CageChange, System, PhaseLabel, BlockStart, BlockEnd) %>%
    dplyr::mutate(GridInsertedAt = NA_character_, GridRemovedAt = NA_character_, TimingStatus = "not_recorded",
                  TimingEvidence = NA_character_, ExposureAnalysisAllowed = FALSE)
  if (!is.null(censor)) {
    # CensoredFrom: the cage's censor time, for each phase that ends after it.
    timing <- timing %>% dplyr::left_join(censor[c("Batch", "CageChange", "System", "CensorFrom")], by = c("Batch", "CageChange", "System")) %>%
      dplyr::mutate(CensoredFrom = dplyr::if_else(CensorFrom < BlockEnd, CensorFrom, as.POSIXct(NA, tz = "UTC"))) %>% dplyr::select(-CensorFrom)
    applied <- dplyr::bind_rows(applied)
    if (!nrow(applied)) applied <- tibble::tibble(RecordId = character(), CageEpisodeID = character(), ManifestVersion = character(),
      ManifestFrom = character(), From = as.POSIXct(character(), tz = "UTC"), RecordingEnd = From, Animals = integer(),
      CensoredAnimalBinsInRecording = integer(), CensoredCageBinsInRecording = integer(), CensoredAnimalHours = numeric(),
      CensoredHoursPerAnimal = numeric(), VendorRowsAfterFrom = integer(), PhaseTrimmedSpanEnd = From)
    write_table(applied, "censoring_applied.csv", "audit")
  }
  write_table(timing, "grid_timing_review.csv")
  if (isTRUE(make_plots)) {
    wa <- whole$animals %>% dplyr::filter(IncludedInPrimary)
    wc <- whole$cages %>% dplyr::filter(IncludedInPrimary)
    if (nrow(wa)) {
      p <- ggplot2::ggplot(wa, ggplot2::aes(PhaseLabel, CrossingsPerHour, group = AnimalKey)) +
        ggplot2::geom_line(colour = "grey65", linewidth = 0.3, alpha = 0.6) +
        ggplot2::geom_point(colour = "grey55", size = 0.7, alpha = 0.7) +
        ggplot2::geom_line(data = wc, ggplot2::aes(y = CageMeanCrossingsPerHour, group = CageID, colour = System), linewidth = 0.85) +
        ggplot2::geom_point(data = wc, ggplot2::aes(y = CageMeanCrossingsPerHour, group = CageID, colour = System), size = 1.7) +
        ggplot2::facet_wrap(~Sex + Batch, ncol = 3, scales = "fixed") + ggplot2::theme_bw(base_size = 11) +
        ggplot2::labs(title = "Whole inactive-phase activity: I2 to I5", subtitle = "06:30-18:30; same complete cages and animals across all four phases",
          x = "Inactive phase (I2 reference)", y = "RFID position changes / animal-hour", colour = "Cage",
          caption = "Grey: individual animals. Colour: cage means. SuPH-associated phase differences; exact grid times unknown.")
      ggplot2::ggsave(file.path(output_dir, "figures", "whole_inactive_animal_cage_trajectories.png"), p, width = 12, height = 8, dpi = 160)
    }
    us <- uncertainty$summary
    if (nrow(us)) {
      pd <- us %>% dplyr::filter(Role == "main_phase_contrast")
      p <- ggplot2::ggplot(pd, ggplot2::aes(Measure, Estimate, colour = Sex)) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey55", linetype = 2) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower95, ymax = Upper95), width = 0.15, na.rm = TRUE) +
        ggplot2::geom_point(size = 2.7) + ggplot2::facet_wrap(~Sex) + ggplot2::theme_bw(base_size = 11) +
        ggplot2::labs(title = "Paired whole-phase activity changes from I2", subtitle = "Cages weighted equally within batch; batches weighted equally within sex",
          x = "Inactive-phase contrast", y = "Change in RFID position changes / animal-hour",
          caption = "Pointwise 95% intervals: 2,000 whole-cage bootstrap draws within fixed batches. Descriptive, not a causal grid effect.") +
        ggplot2::theme(legend.position = "none")
      ggplot2::ggsave(file.path(output_dir, "figures", "whole_inactive_changes_from_I2.png"), p, width = 10, height = 6, dpi = 160)
    }
    for (batch in batches) for (phase in c("Inactive", "Active")) {
      pdat <- cages %>% dplyr::filter(Batch == batch, Phase == phase, SessionSpansWholeBlock,
                                     if (phase == "Inactive") PhaseNumber %in% 2:5 else PhaseNumber %in% 1:5) %>%
        dplyr::mutate(Rate = ifelse(CompleteAnimalBracketing, CrossingsPerExpectedAnimalHour, NA_real_),
                      ClockAxis = MinutesFromPhaseStart / 60 + if (phase == "Inactive") 6.5 else 18.5)
      p <- ggplot2::ggplot(pdat, ggplot2::aes(ClockAxis, Rate, colour = PhaseLabel, group = PhaseLabel)) +
        ggplot2::geom_line(linewidth = 0.4, na.rm = TRUE) + ggplot2::facet_wrap(~System, ncol = 2) +
        ggplot2::scale_x_continuous(breaks = if (phase == "Inactive") c(6.5, 9, 12, 15, 18.5) else c(18.5, 21, 24, 27, 30.5),
          labels = if (phase == "Inactive") c("06:30", "09:00", "12:00", "15:00", "18:30") else c("18:30", "21:00", "00:00", "03:00", "06:30")) +
        ggplot2::theme_bw(base_size = 10) + ggplot2::labs(title = paste(batch, phase, "CC4 clock profiles"),
          subtitle = "Grid insertion/removal times unknown; five-minute bins; descriptive only",
          x = "Logger clock (no timezone conversion)", y = "Position changes / expected animal-hour", colour = "Phase",
          caption = "Gaps: an animal's observed span does not bracket the bin. Bracketing does not establish continuous tracking.")
      ggplot2::ggsave(file.path(output_dir, "figures", paste0(batch, "_", tolower(phase), "_clock_profiles.png")),
                     p, width = 11, height = 8, dpi = 160)
    }
  }
  status <- c("Stage 31: descriptive CC4 grid-associated period",
    "Main comparison: fixed 06:30-18:30 I2-I5 activity in the same complete animal/cage roster; I2 reference.",
    "Paired phase changes: equal cage means within batch, equal batch means within sex; whole-cage bootstrap conditional on observed batches.",
    "95% intervals are pointwise, not simultaneous or multiplicity-adjusted; no p-values or grid-effect claims.",
    "Timing status: actual grid insertion/removal times were not recorded.",
    "Exposure-aligned effects, p-values, phenotype models, and social metrics: NOT RUN.",
    "Activity-selected candidate windows are not exposure observations and cannot validate an activity effect.",
    "I2 is a same-clock descriptive reference; it does not isolate grid, handling, SuPH, or time-since-cage-change effects.",
    "No automatic timing assignment; an empty candidate table is not evidence of no grid exposure.",
    "Full recording retains partial I1/I6. Whole-phase plots use session clock span, with animal bracketing flagged separately.",
    "Counts are RFID position changes, not distance, sleep, or validated sociability.",
    "Five-minute scan; fixed two-hour candidate duration from user protocol; top three positive non-overlapping candidates per cage/day.",
    "Existing canonical preprocessed data, Stage 01 metrics, and Stage 29/30 releases are unchanged.")
  if (!is.null(censor)) {
    clock <- function(x) { s <- ms(x); paste0(format(as.POSIXct(s %/% 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                              ifelse(s %% 1000 == 0, "", sprintf(".%03d", as.integer(s %% 1000)))) }
    status <- c(status, paste0("Censoring (opt-in, spatial exclusion manifest ", censor_manifest, "): ",
      if (nrow(applied)) "unobserved time after a cage episode's censor time keeps its zero-filled counts and every row, but its cage rate (CrossingsPerExpectedAnimalHour) is NA where CensoredAnimals > 0; whole phases are labelled censored_after_last_read_<record>; eligibility and estimates are unchanged (audit/censoring_applied.csv)."
      else "no censor record for the batches run."))
    for (i in seq_len(nrow(applied))) status <- c(status, sprintf(paste0("Censoring %s: %s is unobserved from %s (its last read) to span_end. ",
      "Here span_end is the end of the full CC4 recording, %s (%.1f h censored per animal), not Stage 00b's %s (end of I5, the phase-trimmed span; %.1f h)."),
      applied$RecordId[i], applied$CageEpisodeID[i], applied$ManifestFrom[i], clock(applied$RecordingEnd[i]), applied$CensoredHoursPerAnimal[i],
      clock(applied$PhaseTrimmedSpanEnd[i]), (as.numeric(applied$PhaseTrimmedSpanEnd[i]) - as.numeric(applied$From[i])) / 3600))
  }
  writeLines(status, file.path(output_dir, "README.txt"))
  files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
  write_table(tibble::tibble(File = substring(files, nchar(output_dir) + 2), SHA256 = mmm_file_sha256(files)), "output_manifest.csv", "audit")
  invisible(list(output_dir = output_dir, coverage = cov, candidates = candidates, cages = cages,
                 whole_inactive = whole, whole_inactive_uncertainty = uncertainty))
}

if (sys.nframe() == 0L) {
  source_mmm_helper("behavior_analysis_config.R")
  run_id <- Sys.getenv("MMM_GRID31_RUN_ID")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,63}$", run_id)) stop("Set MMM_GRID31_RUN_ID to a new short run name.", call. = FALSE)
  project <- mmm_project_root()
  spec <- MMM_BEHAVIOR_CONFIG$data_versions[[MMM_BEHAVIOR_CONFIG$data_versions$release]]$supporting_files$cage_label_corrections
  if (is.null(spec)) stop("Configured data version lacks a cage-correction registry.", call. = FALSE)
  censor_manifest <- Sys.getenv("MMM_GRID31_CENSOR_MANIFEST")
  output_root <- Sys.getenv("MMM_GRID31_OUTPUT_ROOT")
  # A censored run is a new analysis variant: it goes to an isolated root only.
  if (nzchar(censor_manifest) && !nzchar(output_root))
    stop("MMM_GRID31_CENSOR_MANIFEST needs MMM_GRID31_OUTPUT_ROOT (an isolated root).", call. = FALSE)
  runs <- if (nzchar(output_root)) mmm_isolated_output_root_check(output_root, project) else
    file.path(project, "analysis_ready", "analyses", "cc4_grid_exposure")
  run_cc4_grid_exposure(file.path(project, "MMMSociability"), file.path(runs, run_id),
    file.path(project, spec$file), spec$sha256, censor_manifest = if (nzchar(censor_manifest)) censor_manifest else NULL)
}
