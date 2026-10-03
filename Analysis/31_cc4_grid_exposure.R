# Stage 31 -- CC4 grid-associated period, descriptive and manually invoked.
# Actual insertion/removal times were not recorded. The activity scan produces
# unverified candidates only; it never produces exposure labels or effect tests.
# Run from repo root with MMM_GRID31_RUN_ID set to a NEW run name.

source("Analysis/_pipeline_setup.R")
for (helper in c("project_paths.R", "E9_SIS_AnimalPos-functions.R",
                 "animalpos_preprocessing_helpers.R", "cc4_grid_exposure_helpers.R",
                 "cc4_whole_inactive_helpers.R")) source_mmm_helper(helper)

run_cc4_grid_exposure <- function(data_dir, output_dir, corrections_file,
                                 corrections_sha256, batches = paste0("B", 1:6), make_plots = TRUE) {
  if (file.exists(output_dir)) stop("Output already exists; choose a new run ID: ", output_dir, call. = FALSE)
  if (!length(batches) || anyDuplicated(batches) || !all(batches %in% paste0("B", 1:6))) stop("Invalid batches.", call. = FALSE)
  excluded_file <- file.path(data_dir, "raw_data", "excluded_animals.csv")
  raw_files <- file.path(data_dir, "raw_data", batches, paste0("E9_SIS_", batches, "_CC4_AnimalPos.csv"))
  code_files <- file.path(MMM_REPO_ROOT, c("Analysis/31_cc4_grid_exposure.R", "Functions/cc4_grid_exposure_helpers.R",
    "Functions/cc4_whole_inactive_helpers.R",
    "Functions/animalpos_preprocessing_helpers.R", "Functions/E9_SIS_AnimalPos-functions.R",
    "Functions/behavioral_dynamics_helpers.R", "Functions/project_paths.R", "Functions/behavior_analysis_config.R",
    "Analysis/_pipeline_setup.R"))
  inputs <- c(raw_files, excluded_file, corrections_file, code_files)
  if (!all(file.exists(inputs))) stop("Missing required input: ", paste(inputs[!file.exists(inputs)], collapse = ", "), call. = FALSE)
  hashes <- mmm_file_sha256(inputs)
  if (anyNA(hashes)) stop("SHA256 hashing requires the existing digest package.", call. = FALSE)
  if (length(corrections_sha256) != 1 || is.na(corrections_sha256) ||
      !identical(unname(mmm_file_sha256(corrections_file)), corrections_sha256)) stop("Cage correction hash mismatch.", call. = FALSE)
  corrections <- readr::read_csv(corrections_file, col_types = readr::cols(.default = readr::col_character()))
  excluded <- trimws(readLines(excluded_file, warn = FALSE)); excluded <- excluded[nzchar(excluded)]
  recordings <- coverage <- profiles <- accounting <- list()
  for (batch in batches) {
    result <- preprocess_animalpos_file(batch, "CC4", excluded, file.path(data_dir, "raw_data"), output_dir,
                                       write_output = FALSE, phase_policy = "full_recording")
    if (is.null(result)) stop("Empty CC4 recording: ", batch, call. = FALSE)
    d <- grid31_correct_systems(result$data, corrections)
    d$AnimalNum <- canonical_animal_id(d$AnimalID)
    if (anyNA(d$AnimalNum) || any(!nzchar(d$AnimalNum))) stop("Invalid canonical identity.", call. = FALSE)
    alias <- d %>% dplyr::distinct(AnimalID, AnimalNum) %>% dplyr::count(AnimalNum)
    if (any(alias$n != 1)) stop("Ambiguous raw identity aliases in ", batch, call. = FALSE)
    d$Sex <- if (batch %in% c("B1", "B2", "B5")) "Male" else "Female"
    d$SourceFile <- basename(raw_files[match(batch, batches)])
    cov <- grid31_phase_coverage(d)
    prof <- grid31_clock_profiles(d, cov)
    cov <- cov %>% dplyr::left_join(dplyr::distinct(d, AnimalID, AnimalNum, Sex), by = "AnimalID")
    prof$animals <- prof$animals %>% dplyr::left_join(dplyr::distinct(d, AnimalID, AnimalNum, Sex), by = "AnimalID")
    prof$cages$Sex <- d$Sex[1]
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
  run_cc4_grid_exposure(file.path(project, "MMMSociability"),
    file.path(project, "analysis_ready", "analyses", "cc4_grid_exposure", run_id),
    file.path(project, spec$file), spec$sha256)
}
