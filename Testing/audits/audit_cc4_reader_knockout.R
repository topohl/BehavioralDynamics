# ================================================================
# Audit: CC4 reader knockout -- would a whole-session dead R3 or R5 bias the Stage 31-31d crossing outcomes?
# MMMSociability -- Testing/audits/audit_cc4_reader_knockout.R
# ================================================================
# UNREGISTERED, isolated, group-blind audit (2026-10-14). Definitions: Functions/cc4_reader_knockout.R. Stages 29 and
# 31-31d, S13 and every registered or frozen output stay as they are; nothing is refitted and no model is fitted. The
# block below is the design; it was written before any run and is copied into every run's README.txt.
#
# PRE-DECLARED DESIGN (cc4_reader_knockout v1.0; fixed before any run, not changed after seeing results)
# Question. Two CC4 cage episodes lost one antenna for the whole session: B2|sys.5|CC4 (R3) and B5|sys.1|CC4 (R5, the
#   house). srs_v1.1 measured how much the vendor position stream depends on these antennas only in the Stage 29
#   windows (CC4: A1 and I2); there the active crossing rate shift was material and the light one not. Stages 31-31d
#   count crossings over the whole CC4 recording, including the grid-associated I3-I5 and A3-A5. This audit measures,
#   on the healthy CC4 board-recordings, how far deleting R3 or R5 moves those crossing outcomes.
# Units. Every animal of a healthy CC4 board-recording (Batch x board): all CC4 board-recordings except the cage episodes
#   that the spatial exclusion manifest v1.1 marks exclude_spatial for CC4, which must be exactly B2|sys.5|CC4 and
#   B5|sys.1|CC4 (otherwise the run stops). The animals are Stage 31's roster: everyone in the batch's CC4 vendor export
#   after the canonical exclusions (raw_data/excluded_animals.csv) and the v2 cage-label corrections; the board is the
#   corrected System label, which must be the board holding most of the animal's raw reads (otherwise the run stops).
#   An animal-phase is complete when Stage 31 deems it so: the session's and the animal's first and last vendor rows
#   bracket the whole 12-h block (EligiblePhase of grid31_whole_phase() on the vendor export). Completeness is fixed from
#   the export for all three variants, so a knockout never changes the units (whether a variant's own stream would
#   bracket the block is kept as a diagnostic).
# Variants. baseline: the vendor AnimalPos stream emulated from all of the animal's raw reads (apr_reads(),
#   apr_positions(gate = "vendor"): the reconstructed vendor estimator, unchanged). ko_R3 and ko_R5: the same after
#   deleting the reads at PositionID 3 or 5 on the animal's own board (as srs_v1.1: every animal of the recording at once;
#   off-board reads kept).
# Crossings. A vendor record whose PositionID differs from the same animal's previous record, counted in the phase of
#   the later record; a previous record before the phase counts; the first record never (Stage 31).
# Parity gate. The baseline must reproduce Stage 31's crossing counts per animal x phase (A1-A5, I2-I5) on the healthy
#   board-recordings. Stage 31's counts come from Stage 31's own functions on the raw vendor exports
#   (MMMSociability/raw_data/<B>/E9_SIS_<B>_CC4_AnimalPos.csv, preprocess_animalpos_file(phase_policy =
#   "full_recording"), the v2 cage-label corrections, as Analysis/31_cc4_grid_exposure.R prepares them), and these must
#   equal the frozen Stage 31 run whole_inactive_20260929_v1 exactly (crossings, vendor rows and completeness per
#   animal-phase). A release run fails when fewer than 95% of the healthy animal-phases match Stage 31 exactly, when the
#   pooled count differs by more than 1% (CKR_PARITY_MIN_EXACT_SHARE, CKR_PARITY_MAX_POOLED_REL_DIFF), or when the
#   frozen-run comparison is not exact. A failed gate is investigated, never loosened.
# Outcomes per animal and variant. CrossingsPerHour per phase (crossings / 12 h; NA for an incomplete phase, never 0).
#   Active family: reference_rate = A2; post_rate = A3-A5 pooled (count / 36 h); log_rate_ratio =
#   log((post count + 0.5) / 36 h) - log((reference count + 0.5) / 12 h). Inactive family likewise with I2 and I3-I5.
#   The three family outcomes need all four phases of the family complete. rate_A1 and rate_I2 alone (the Stage 29
#   windows, for continuity with srs_v1.1) need only that phase complete.
# Shift, scale and materiality. shift = knockout - baseline per animal and outcome. Scale per outcome: sd() of the
#   residuals of lm(baseline ~ Batch) over the healthy animals of that outcome (group-blind; the formula of srs_v1.1's
#   light-window scale, applied to post-SIS CC4 baselines, so its SD units are not those of srs_v1.1, which used CC1
#   light windows and the frozen Stage 29 standardizers). Material when the median |shift| >= 0.2 scale SD or the 90th
#   percentile of |shift| >= 0.5 scale SD (the srs_v1.1 rule).
# Not read. No group label, CombZ or outcome table; no Stage 29 value. No model is fitted and nothing is refitted.
# Context (not gated). The two excluded cage episodes are processed too and reported separately (their baseline already
#   lacks the antenna). The baseline records are compared with the vendor export record by record, and rate_A1 /
#   rate_I2 with the srs_v1.1 knockout table (srs_continuity.csv).
# END OF THE PRE-DECLARED DESIGN
#
# Run from the repository root (one R process, one recording at a time):
#   Rscript Testing/audits/audit_cc4_reader_knockout.R [--run-id=<new id>] [--output-root=<absolute>] [--recordings=<regex>]
# Default output root <Behavior>/RANM/cc4_reader_knockout (mmm_isolated_output_root_check()); writes runs/<run_id>/ only,
# built in runs/.tmp_<run_id>/, under the protection guard (Functions/protected_outputs.R). The default run id is
# ckr_v1.0_<commit7> and needs committed, clean code; ids starting with "dev" may run on uncommitted code and with
# --recordings (a regex on B1_CC4 ... B6_CC4).
# Inputs (read-only): the six CC4 vendor exports and raw MultiMouseMonitor logs, excluded_animals.csv, the v2 cage-label
# corrections (sha256 pinned in the configuration), manifest v1.1 (content-pinned), the frozen Stage 31 run's animal
# 5-min bins (checked against its output manifest) and the srs_v1.1 knockout table (checked against its output hashes).

CKR_SCRIPT <- "Testing/audits/audit_cc4_reader_knockout.R"
CKR_CODE <- c(CKR_SCRIPT, "Functions/cc4_reader_knockout.R", "Functions/animalpos_reconstruction.R", "Functions/animalpos_preprocessing_helpers.R",
              "Functions/antenna_log_helpers.R", "Functions/cc4_grid_exposure_helpers.R", "Functions/cc4_whole_inactive_helpers.R",
              "Functions/antenna_spatial_reconstruction.R", "Functions/spatial_exclusion_manifest.R", "Functions/protected_outputs.R",
              "Functions/qc_run_protection.R", "Functions/frozen_input_guard.R", "Functions/project_paths.R",
              "Functions/behavioral_dynamics_helpers.R", "Functions/behavior_analysis_config.R", "Analysis/_pipeline_setup.R",
              "Analysis/31_cc4_grid_exposure.R")
CKR_MANIFEST_VERSION <- "v1.1"
CKR_EXPECTED_EXCLUDED <- c("B2|sys.5|CC4", "B5|sys.1|CC4")   # declared before any run; the manifest must agree
CKR_STAGE31_RUN <- "whole_inactive_20260929_v1"              # the frozen Stage 31 run that Stages 31b-31d read
CKR_SRS_RUN <- "srs_v1.1_9b18624"
CKR_BATCHES <- paste0("B", 1:6)
CKR_FILE_DOCS <- c(
  "README.txt" = "the pre-declared design, the parity results and the materiality table",
  "tables/parity.csv" = "baseline emulation vs Stage 31 (gated on the healthy board-recordings), Stage 31 reference vs the frozen run (gated), record-by-record stream parity (context)",
  "tables/animal_phase.csv" = "variant x animal x phase (A1-A5, I2-I5): crossings, vendor records, completeness, hours, rate, Stage 31's counts",
  "tables/animal_family.csv" = "variant x animal x family: reference / post counts and hours, reference_rate, post_rate, log_rate_ratio, A1 / I2 alone",
  "tables/scales.csv" = "family x outcome: the group-blind scale (residual SD of lm(baseline ~ Batch), healthy animals)",
  "tables/shifts.csv" = "reader x family x outcome x healthy animal: baseline, knockout, shift, shift in scale SD",
  "tables/materiality_cc4.csv" = "reader x family x outcome: n, scale, median shift, median and 90th-percentile |shift| in scale SD, material, rule",
  "tables/animals.csv" = "animal: keys, healthy, tag, reads (on / off board, at R3 / R5 on board), modal read board, record-by-record stream parity",
  "tables/recordings.csv" = "recording: raw log used and ignored, clock-fold rows, events, vendor rows, animals, runtime",
  "tables/srs_continuity.csv" = "rate_A1 / rate_I2 here against the srs_v1.1 knockout table (CC4 Stage 29 windows); not gated",
  "manifest/code_hashes.csv" = "sha256 and git status of every code file",
  "manifest/input_hashes.csv" = "sha256 and size of every input (vendor exports, raw logs, corrections, Stage 31 and srs tables, manifest)",
  "manifest/run.csv" = "run record: commit, code state, parameters, gate, timing",
  "manifest/session_info.txt" = "sessionInfo()",
  "manifest/protection_snapshot_before.csv" = "protected locations before the run (protection guard)",
  "manifest/protection_check.csv" = "differences after the run (protection guard; none may be a change)",
  "manifest/output_files_sha256.csv" = "sha256 of every other output file")

ckr_args <- function(args) {
  known <- "^--(run-id|output-root|recordings)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  out <- list(run_id = val("run-id"), output_root = val("output-root"), recordings = val("recordings"))
  for (k in names(out)) if (!is.na(out[[k]]) && !nzchar(out[[k]])) stop("Empty value of --", gsub("_", "-", k), call. = FALSE)
  out
}

ckr_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

# The design block of this script's header, for the README (one source of truth, pinned by the code hash).
ckr_design_lines <- function(script) {
  x <- readLines(script, warn = FALSE)
  i <- grep("^# PRE-DECLARED DESIGN", x); j <- grep("^# END OF THE PRE-DECLARED DESIGN", x)
  if (length(i) != 1L || length(j) != 1L || j <= i) stop("The script header lacks its pre-declared design block.", call. = FALSE)
  sub("^# ?", "", x[i:(j - 1L)])
}

# The vendor rows of one CC4 recording exactly as Analysis/31_cc4_grid_exposure.R prepares them (its lines 55-62 and 81):
# the full recording, the canonical exclusions, the cage-label corrections (System_raw kept), AnimalNum and Sex.
ckr_vendor_rows <- function(batch, raw_dir, excluded, corrections) {
  result <- preprocess_animalpos_file(batch, "CC4", excluded, raw_dir, NA_character_, write_output = FALSE, phase_policy = "full_recording")
  if (is.null(result)) stop("Empty CC4 recording: ", batch, call. = FALSE)
  d <- grid31_correct_systems(result$data, corrections)
  d$AnimalNum <- canonical_animal_id(d$AnimalID)
  if (anyNA(d$AnimalNum) || any(!nzchar(d$AnimalNum))) stop("Invalid canonical identity in ", batch, call. = FALSE)
  alias <- dplyr::count(dplyr::distinct(d, AnimalID, AnimalNum), AnimalNum)
  if (any(alias$n != 1)) stop("Ambiguous raw identity aliases in ", batch, call. = FALSE)
  d$Sex <- if (batch %in% c("B1", "B2", "B5")) "Male" else "Female"
  d
}

# One CC4 recording: Stage 31's rows, the raw log (with the read offsets), the tag map of the export, then
# ckr_process_recording(). raw_dir: <project>/MMMSociability/raw_data; raw_root: the raw MultiMouseMonitor logs.
ckr_recording <- function(batch, raw_dir, raw_root, excluded, corrections, excluded_episodes) {
  t0 <- Sys.time()
  d <- ckr_vendor_rows(batch, raw_dir, excluded, corrections)
  rec <- rfid_read_recording(raw_root, batch, "CC4", keep_read_offsets = TRUE)
  tags <- rfid_animalpos_tag_map(file.path(raw_dir, batch, sprintf("E9_SIS_%s_CC4_AnimalPos.csv", batch)))
  res <- ckr_process_recording(d, rec$events, tags, batch, excluded_episodes)
  res$recording <- data.table::data.table(recording = paste0(batch, "_CC4"), Batch = batch, raw_log = basename(rec$path),
                                          raw_logs_ignored = paste(rec$ignored, collapse = ";"), clock_fold_rows_dropped = rec$clock_fold_rows_dropped,
                                          events = nrow(rec$events), vendor_rows = nrow(d), animals = nrow(res$animals),
                                          healthy_animals = sum(res$animals$healthy), seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  res
}

# The frozen Stage 31 run's animal 5-min bins, checked against the run's own output manifest; only the columns compared.
ckr_read_live_stage31 <- function(run_dir, batches) {
  f <- file.path(run_dir, "tables", "animal_clock_profiles_5min.csv")
  om <- data.table::fread(file.path(run_dir, "audit", "output_manifest.csv"), colClasses = "character")
  pinned <- om[File == "tables/animal_clock_profiles_5min.csv"]$SHA256
  if (length(pinned) != 1L || !identical(mmm_file_sha256(f), pinned)) stop("The frozen Stage 31 bins differ from their run's output manifest: ", f, call. = FALSE)
  x <- data.table::fread(f, select = c("Batch", "System", "AnimalID", "PhaseLabel", "Crossings", "ReadCount", "SessionSpansWholeBlock",
                                       "AnimalSpansWholeBlock", "SpanOverlapSec"), colClasses = c(AnimalID = "character"))
  x[Batch %in% batches]
}

# The srs_v1.1 knockout table (Stage 29 windows), checked against the run's output hashes; CC4 rows, crossing rates only.
ckr_read_srs_shifts <- function(srs_dir, batches) {
  f <- file.path(srs_dir, "tables", "knockout_shifts.csv")
  om <- data.table::fread(file.path(srs_dir, "manifest", "output_files_sha256.csv"), colClasses = "character")
  pinned <- om[file == "tables/knockout_shifts.csv"]$sha256
  if (length(pinned) != 1L || !identical(mmm_file_sha256(f), pinned)) stop("The srs knockout table differs from its run's output hashes: ", f, call. = FALSE)
  x <- data.table::fread(f, select = c("AnimalNum", "CC", "Batch", "window", "crossing_rate_baseline", paste0("crossing_rate_ko_R", CKR_READERS)),
                         colClasses = c(AnimalNum = "character"))
  x[, AnimalNum := canonical_animal_id(AnimalNum)]
  x[CC == "CC4" & Batch %in% batches]
}

ckr_parity_lines <- function(parity) {
  sprintf("  %s: %d %s, %d exact (%.2f%%), pooled difference %.4f%%, %s", parity$comparison, parity$n, parity$units, parity$exact_matches,
          100 * parity$exact_share, 100 * parity$pooled_rel_diff, ifelse(parity$gated, ifelse(parity$pass, "PASS", "FAIL"), "context, not gated"))
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble); library(tidyr) })
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "behavior_analysis_config.R", "animalpos_preprocessing_helpers.R", "antenna_log_helpers.R",
              "animalpos_reconstruction.R", "cc4_grid_exposure_helpers.R", "cc4_whole_inactive_helpers.R", "antenna_spatial_reconstruction.R",
              "spatial_exclusion_manifest.R", "cc4_reader_knockout.R", "protected_outputs.R")) source_mmm_helper(h)
  for (pkg in c("digest", "readr", "tidyr")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Package ", pkg, " is needed.", call. = FALSE)
  a <- ckr_args(commandArgs(trailingOnly = TRUE))
  started <- Sys.time()
  project_root <- mmm_project_root()
  head_commit <- ckr_git(c("rev-parse", "HEAD"))[1]
  if (!grepl("^[0-9a-f]{40}$", head_commit)) stop("Could not read the git commit: ", head_commit, call. = FALSE)
  run_id <- if (is.na(a$run_id)) paste0(CKR_VERSION, "_", substr(head_commit, 1, 7)) else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  dev <- startsWith(run_id, "dev")
  if (!is.na(a$recordings) && !dev) stop("--recordings is for dev runs only.", call. = FALSE)
  if (!identical(c(CKR_MATERIAL_MEDIAN_SD, CKR_MATERIAL_P90_SD), c(ASR_MATERIAL_MEDIAN_SD, ASR_MATERIAL_P90_SD)))
    stop("The materiality thresholds differ from srs_v1.1's.", call. = FALSE)
  man_file <- file.path(SXM_DIR, sprintf("spatial_analysis_exclusions_%s.csv", CKR_MANIFEST_VERSION))
  dirty <- ckr_git(c("status", "--porcelain", "--untracked-files=all", "--", CKR_CODE, man_file))
  if (!dev && length(dirty)) stop("A release run needs committed, clean code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "cc4_reader_knockout") else a$output_root,
                                         project_root)
  out_dir <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out_dir)) stop("Run folder exists; choose a new --run-id: ", out_dir, call. = FALSE)
  if (file.exists(tmp)) stop("A partial run folder exists; inspect and remove it first: ", tmp, call. = FALSE)
  mmm_assert_output_path_budget(c(file.path(tmp, names(CKR_FILE_DOCS)), file.path(out_dir, names(CKR_FILE_DOCS))), source_label = "ckr audit")

  # the declared units: the manifest's CC4 exclude_spatial episodes must be the two declared ones
  man <- sxm_read(CKR_MANIFEST_VERSION)
  excluded_episodes <- sort(man[action == "exclude_spatial" & CC == "CC4"]$CageEpisodeID)
  if (!identical(excluded_episodes, sort(CKR_EXPECTED_EXCLUDED)))
    stop("Manifest ", CKR_MANIFEST_VERSION, " excludes other CC4 episodes than declared: ", paste(excluded_episodes, collapse = ", "), call. = FALSE)
  batches <- CKR_BATCHES
  if (!is.na(a$recordings)) batches <- batches[grepl(a$recordings, paste0(batches, "_CC4"))]
  if (!length(batches)) stop("--recordings matches no CC4 recording.", call. = FALSE)
  raw_dir <- file.path(project_root, "MMMSociability", "raw_data")
  raw_root <- mmm_raw_antenna_root(project_root)
  spec <- MMM_BEHAVIOR_CONFIG$data_versions[[MMM_BEHAVIOR_CONFIG$data_versions$release]]$supporting_files$cage_label_corrections
  if (is.null(spec)) stop("The configured data version lacks a cage-correction registry.", call. = FALSE)
  corrections_file <- file.path(project_root, spec$file)
  live31 <- file.path(project_root, "analysis_ready", "analyses", "cc4_grid_exposure", CKR_STAGE31_RUN)
  srs_dir <- file.path(dirname(project_root), "RANM", "single_read_spatial", "runs", CKR_SRS_RUN)
  vendor_files <- file.path(raw_dir, batches, sprintf("E9_SIS_%s_CC4_AnimalPos.csv", batches))
  fixed <- c(vendor_files, file.path(raw_dir, "excluded_animals.csv"), corrections_file, file.path(live31, "tables", "animal_clock_profiles_5min.csv"),
             file.path(live31, "audit", "output_manifest.csv"), file.path(srs_dir, "tables", "knockout_shifts.csv"),
             file.path(srs_dir, "manifest", "output_files_sha256.csv"), attr(man, "path"))
  miss <- fixed[!file.exists(fixed)]
  if (length(miss)) stop("Missing input(s):\n  ", paste(miss, collapse = "\n  "), call. = FALSE)
  if (!identical(unname(mmm_file_sha256(corrections_file)), spec$sha256)) stop("Cage correction hash mismatch.", call. = FALSE)

  # protection guard: before any write
  guard <- mmm_protection_guard_begin(base, run_id, "cc4_reader_knockout", project_root)
  logs <- unlist(lapply(batches, function(b) rfid_raw_log_candidates(raw_root, b, "CC4")))
  inputs <- data.table(role = c(rep("CC4 vendor AnimalPos export", length(vendor_files)), "canonical animal exclusions", "v2 cage-label corrections",
                                paste("Stage 31 animal 5-min bins,", CKR_STAGE31_RUN), paste("Stage 31 output manifest,", CKR_STAGE31_RUN),
                                paste("srs knockout shifts,", CKR_SRS_RUN), paste("srs output hashes,", CKR_SRS_RUN),
                                paste("spatial exclusion manifest", CKR_MANIFEST_VERSION), rep("CC4 raw log candidate", length(logs))),
                       path = c(fixed, logs))
  inputs[, `:=`(sha256 = mmm_file_sha256(path), bytes = file.size(path))]
  excluded <- trimws(readLines(file.path(raw_dir, "excluded_animals.csv"), warn = FALSE)); excluded <- excluded[nzchar(excluded)]
  corrections <- readr::read_csv(corrections_file, col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  live <- ckr_read_live_stage31(live31, batches)
  srs <- ckr_read_srs_shifts(srs_dir, batches)

  res <- list()
  for (b in batches) {
    res[[b]] <- ckr_recording(b, raw_dir, raw_root, excluded, corrections, excluded_episodes)
    invisible(gc())
    message(sprintf("%s_CC4: %d animals (%d healthy), %.1f s", b, res[[b]]$recording$animals, res[[b]]$recording$healthy_animals, res[[b]]$recording$seconds))
  }
  ap <- rbindlist(lapply(res, `[[`, "animal_phase")); animals <- rbindlist(lapply(res, `[[`, "animals"))
  s31 <- rbindlist(lapply(res, `[[`, "stage31")); recs <- rbindlist(lapply(res, `[[`, "recording"))
  af <- ckr_family_outcomes(ap)
  sh <- ckr_shifts(af)
  sc <- ckr_scales(sh)
  mt <- ckr_materiality(sh, sc)
  sh <- merge(sh, sc[, .(family, outcome, scale_sd)], by = c("family", "outcome"))[, shift_sd := shift / scale_sd]
  sh <- sh[order(reader, match(family, names(CKR_FAMILIES)), match(outcome, CKR_OUTCOMES), CageEpisodeID, AnimalNum)]
  setcolorder(sh, c("reader", "family", "outcome"))
  parity <- rbind(
    ckr_parity(ap[healthy == TRUE], "baseline emulation vs Stage 31 crossings, healthy CC4 board-recordings (release gate)", gated = TRUE),
    ckr_parity(ap[healthy == FALSE], "baseline emulation vs Stage 31 crossings, excluded cage episodes", gated = FALSE),
    ckr_stage31_live_compare(s31, live, paste("Stage 31 reference (current code on the raw exports) vs the frozen Stage 31 run", CKR_STAGE31_RUN)),
    ckr_stream_parity(animals[healthy == TRUE], "baseline vendor records vs the vendor export, record by record, healthy"),
    ckr_stream_parity(animals[healthy == FALSE], "baseline vendor records vs the vendor export, record by record, excluded cage episodes"))
  gate_pass <- isTRUE(all(parity[gated == TRUE]$pass))
  if (!gate_pass && !dev) {
    stop("Parity gate failed; nothing was written. Investigate (never loosen the gate):\n", paste(ckr_parity_lines(parity), collapse = "\n"), call. = FALSE)
  }
  cont <- ckr_srs_continuity(sh, srs)

  dir.create(file.path(tmp, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "manifest"), showWarnings = FALSE)
  tabs <- list(parity = parity, animal_phase = ap, animal_family = af, scales = sc, shifts = sh, materiality_cc4 = mt, animals = animals,
               recordings = recs, srs_continuity = cont)
  documented <- sub("^tables/(.*)[.]csv$", "\\1", grep("^tables/", names(CKR_FILE_DOCS), value = TRUE))
  if (!setequal(names(tabs), documented)) stop("Tables and their documentation differ.", call. = FALSE)
  for (nm in names(tabs)) fwrite(tabs[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  code <- data.table(file = c(CKR_CODE, man_file))
  code[, `:=`(sha256 = mmm_file_sha256(file.path(MMM_REPO_ROOT, file)),
              git_status = vapply(file, function(f) { s <- ckr_git(c("status", "--porcelain", "--untracked-files=all", "--", f))
                                                      if (length(s)) trimws(substr(s[1], 1, 2)) else "clean" }, ""))]
  fwrite(code, file.path(tmp, "manifest", "code_hashes.csv"))
  fwrite(inputs, file.path(tmp, "manifest", "input_hashes.csv"))
  finished <- Sys.time()
  run <- data.table(
    item = c("run_id", "version", "status", "git_commit", "code_clean", "dev_run", "recordings", "manifest_version", "manifest_sha256",
             "excluded_episodes", "knockout_readers", "phases", "parity_gate", "parity_gate_pass", "materiality_rule", "scale",
             "stage31_reference_run", "srs_run", "project_root", "raw_root", "output_root", "seconds_per_recording", "started", "finished", "R"),
    value = c(run_id, CKR_VERSION, "UNREGISTERED reader-knockout audit; isolated; group-blind; no model fitted", head_commit,
              as.character(!length(dirty)), as.character(dev), paste(paste0(batches, "_CC4"), collapse = " "), CKR_MANIFEST_VERSION,
              attr(man, "sha256"), paste(excluded_episodes, collapse = " "), paste0("R", CKR_READERS, collapse = " "), paste(CKR_PHASES, collapse = " "),
              sprintf("exact share >= %.2f and pooled relative difference <= %.2f (healthy); frozen Stage 31 run exact", CKR_PARITY_MIN_EXACT_SHARE,
                      CKR_PARITY_MAX_POOLED_REL_DIFF), as.character(gate_pass), CKR_MATERIALITY_RULE, CKR_SCALE_SOURCE, CKR_STAGE31_RUN, CKR_SRS_RUN,
              project_root, raw_root, base, paste(sprintf("%s %.0f", recs$recording, recs$seconds), collapse = "; "),
              format(started, "%Y-%m-%d %H:%M:%S"), format(finished, "%Y-%m-%d %H:%M:%S"), R.version.string))
  fwrite(run, file.path(tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(tmp, "manifest", "session_info.txt"))
  mt_print <- as.data.frame(mt[, .(reader, family, outcome, n_animals, n_board_recordings, scale_sd = signif(scale_sd, 4), median_shift = signif(median_shift, 4),
                                   median_abs_shift_sd = round(median_abs_shift_sd, 3), p90_abs_shift_sd = round(p90_abs_shift_sd, 3), material)])
  mt_lines <- local({ op <- options(width = 250); on.exit(options(op)); capture.output(print(mt_print, row.names = FALSE)) })
  w <- max(nchar(names(CKR_FILE_DOCS)))
  writeLines(c(paste("CC4 READER KNOCKOUT", CKR_VERSION, "(UNREGISTERED; isolated; group-blind)"), "",
               paste("run:", run_id, "| commit:", head_commit, "| code clean:", !length(dirty), "| dev run:", dev),
               paste("recordings:", paste(paste0(batches, "_CC4"), collapse = " "), "| excluded cage episodes:", paste(excluded_episodes, collapse = ", ")),
               if (!gate_pass) c("", "PARITY GATE FAILED (dev run): these results are not a release; see tables/parity.csv.") else character(0), "",
               ckr_design_lines(file.path(MMM_REPO_ROOT, CKR_SCRIPT)), "",
               "RESULTS", "Parity:", ckr_parity_lines(parity), "",
               sprintf("Healthy units: %d animals on %d board-recordings (%s).", animals[healthy == TRUE, .N], animals[healthy == TRUE, uniqueN(CageEpisodeID)],
                       paste(unique(sc$scale_source), collapse = "; ")),
               "Materiality (healthy CC4 board-recordings; shifts = knockout - baseline):", mt_lines, "",
               "The rates count thresholded displacement events of the vendor estimate (>= 200 grid units), not locomotion; they are not a",
               "proximity, occupancy or resource measure. No group label, CombZ score or outcome table was read; no model was fitted.", "",
               "Files:", paste(formatC(names(CKR_FILE_DOCS), width = -w), CKR_FILE_DOCS, sep = "  ")),
             file.path(tmp, "README.txt"))
  mmm_protection_guard_end(guard, file.path(tmp, "manifest"))
  outs <- setdiff(list.files(tmp, recursive = TRUE), "manifest/output_files_sha256.csv")
  fwrite(data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
         file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (!setequal(list.files(tmp, recursive = TRUE), names(CKR_FILE_DOCS))) stop("Unexpected file set in ", tmp, call. = FALSE)
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  print(parity[, .(comparison, n, exact_share, pooled_rel_diff, gated, pass)])
  print(mt_print)
  message("cc4 reader knockout done: ", out_dir)
}
