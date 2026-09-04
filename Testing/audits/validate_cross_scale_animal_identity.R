# ================================================================
# Post-Stage-01 cross-scale animal identity validator (S: DRIVE REQUIRED)
# ================================================================
# Reads the actual Stage 01 outputs for every fixed-width scale plus
# phase_based, plus (if present) Stage 03's and Stage 09's canonical
# animal-level tables, and checks the identity invariants required after an
# animal-ID correction:
#   - every AnimalNum is already in canonical form
#   - the canonical animal roster is identical across scales
#   - exactly one Group and one Sex per AnimalNum, within and across scales
#   - Stage 03 and Stage 09 inherit the same identity map as Stage 01
#
# The expected roster and expected phenotype (SUS/CON/RES) are derived from
# the RAW PREPROCESSED Stage 01 input (*_preprocessed.csv AnimalID values)
# plus the SUS/CON reference files -- NEVER from any already-derived scale's
# Group column, so a corrupted derived rebuild cannot poison the definition of
# what we expect to see. Nothing is hardcoded (e.g. no baked-in "111").
# Observed-vs-expected counts and per-scale phenotype mismatches are
# reported, not silently enforced.
#
# This script requires the S: drive and real pipeline outputs; it is NOT part
# of the portable contract-test suite. The pure invariant-checking logic it
# calls into (Functions/animal_identity_invariants_helpers.R) is covered
# separately by Testing/tests/test_animal_identity_invariants_engine.R using
# tempdir()/in-memory fixtures.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

source("Analysis/_pipeline_setup.R")
source("Functions/animal_identity_invariants_helpers.R")

run_cross_scale_identity_validation <- function(base_dir = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID",
                                                sus_animals_file = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv",
                                                con_animals_file = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv",
                                                preprocessed_dir = file.path(base_dir, "MMMSociability/preprocessed_data"),
                                                assign_unlisted_animals_as_res = TRUE,
                                                scales = c("10sec", "1min", "5min", "10min", "30min"),
                                                reference_scale = "10min",
                                                stage03_resolution = "10min",
                                                stage09_resolution = "10min",
                                                write_report = TRUE) {
  derived_metrics_root <- file.path(base_dir, "analysis_ready/03_derived_metrics")

  read_scale <- function(scale_label) {
    path <- file.path(derived_metrics_root, paste0(scale_label, "_based"), "all_behavior_metrics.csv")
    if (!file.exists(path)) {
      warning("Missing Stage 01 output for scale '", scale_label, "': ", path, call. = FALSE)
      return(NULL)
    }
    read_csv(path, show_col_types = FALSE, progress = FALSE, col_types = cols(AnimalNum = col_character()))
  }

  scale_data <- setNames(lapply(scales, read_scale), scales)
  phase_path <- file.path(derived_metrics_root, "phase_based", "all_behavior_metrics.csv")
  if (file.exists(phase_path)) {
    scale_data[["phase"]] <- read_csv(phase_path, show_col_types = FALSE, progress = FALSE, col_types = cols(AnimalNum = col_character()))
  } else {
    warning("Missing Stage 01 phase_based output: ", phase_path, call. = FALSE)
  }
  scale_data <- compact(scale_data)
  if (length(scale_data) == 0L) {
    stop("No Stage 01 derived-metrics outputs were found under: ", derived_metrics_root,
         ". Run a full Stage 01 rebuild first (see Analysis/01_build_multiscale_behavior_metrics.R).", call. = FALSE)
  }
  if (!reference_scale %in% names(scale_data)) {
    warning("Requested reference_scale '", reference_scale, "' is unavailable; using '", names(scale_data)[[1]], "' instead.", call. = FALSE)
    reference_scale <- names(scale_data)[[1]]
  }

  read_animal_id_list <- function(path) {
    if (!file.exists(path)) {
      warning("Reference animal-ID file not found: ", path, call. = FALSE)
      return(character())
    }
    canonical_animal_id(read_lines(path, progress = FALSE)) %>% discard(~ is.na(.x) || .x == "") %>% unique()
  }
  sus_ids <- read_animal_id_list(sus_animals_file)
  con_ids <- read_animal_id_list(con_animals_file)

  # Expected roster/phenotype come from the RAW preprocessed input's AnimalID
  # column, not from any derived scale's Group column -- see the file header.
  preprocessed_files <- list.files(preprocessed_dir, pattern = "_preprocessed\\.csv$", full.names = TRUE)
  if (length(preprocessed_files) == 0L) {
    stop(
      "No preprocessed input files found under: ", preprocessed_dir,
      ". The expected roster/phenotype must come from raw preprocessed input, ",
      "not from a derived scale's Group column, so this cannot proceed without them.",
      call. = FALSE
    )
  }
  raw_animal_ids <- unlist(lapply(preprocessed_files, function(f) {
    dat <- tryCatch(
      read_csv(f, show_col_types = FALSE, progress = FALSE, col_types = cols_only(AnimalID = col_character())),
      error = function(e) { warning("Could not read AnimalID from ", f, ": ", conditionMessage(e), call. = FALSE); NULL }
    )
    if (is.null(dat)) character() else dat$AnimalID
  }), use.names = FALSE)

  expected_phenotype <- derive_expected_phenotype_from_preprocessed(
    raw_animal_ids, sus_ids, con_ids, assign_unlisted_as_res = assign_unlisted_animals_as_res
  )
  expected_roster <- unique(expected_phenotype$AnimalNum)

  stage03_path <- file.path(behavior_stage_tables(base_dir, "03", "movement_phase_stats", stage03_resolution), "raw_movement_animal_level_endpoints.csv")
  stage03_dat <- if (file.exists(stage03_path)) read_csv(stage03_path, show_col_types = FALSE, progress = FALSE, col_types = cols(AnimalNum = col_character())) else NULL

  stage09_path <- file.path(behavior_stage_tables(base_dir, "09", "early_prediction", stage09_resolution), "model_ladder_input.csv")
  stage09_dat <- if (file.exists(stage09_path)) read_csv(stage09_path, show_col_types = FALSE, progress = FALSE, col_types = cols(AnimalNum = col_character())) else NULL

  downstream <- compact(list(stage03 = stage03_dat, stage09 = stage09_dat))
  if (length(downstream) == 0L) {
    message("Neither Stage 03 nor Stage 09 canonical output was found; skipping downstream inheritance checks.\n",
            "Tried:\n- ", stage03_path, "\n- ", stage09_path)
  }

  result <- validate_cross_scale_animal_identity(
    scale_data = scale_data,
    downstream = downstream,
    expected_phenotype = expected_phenotype,
    expected_roster = expected_roster,
    reference_scale = reference_scale
  )

  if (isTRUE(write_report)) {
    out_dir <- file.path(derived_metrics_root, "qc")
    ensure_dir(out_dir)
    write_csv(result$format_checks, file.path(out_dir, "cross_scale_identity_format_checks.csv"))
    write_csv(result$group_sex_checks_within_scale, file.path(out_dir, "cross_scale_identity_group_sex_within_scale.csv"))
    write_csv(result$roster_comparison_across_scales, file.path(out_dir, "cross_scale_identity_roster_comparison.csv"))
    write_csv(result$cross_scale_group_sex_conflicts, file.path(out_dir, "cross_scale_identity_group_sex_conflicts.csv"))
    write_csv(result$downstream_inheritance_checks, file.path(out_dir, "cross_scale_identity_downstream_inheritance.csv"))
    write_csv(result$expected_phenotype_checks, file.path(out_dir, "cross_scale_identity_expected_phenotype_checks.csv"))
    write_csv(expected_phenotype, file.path(out_dir, "cross_scale_identity_expected_phenotype_from_preprocessed.csv"))
    write_csv(result$roster_count_summary, file.path(out_dir, "cross_scale_identity_roster_count_summary.csv"))
    message("Wrote cross-scale identity validation report to: ", out_dir)
  }

  if (!isTRUE(result$all_checks_passed)) {
    warning("Cross-scale animal identity validation FAILED. See the report tables for details.", call. = FALSE)
  } else {
    message("Cross-scale animal identity validation PASSED.")
  }

  invisible(result)
}

.cross_scale_identity_file_is_main <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_flag <- sub("^--file=", "", file_arg[grepl("^--file=", file_arg)])
  length(file_flag) > 0 && grepl("validate_cross_scale_animal_identity\\.R$", file_flag[[1]])
})

if (.cross_scale_identity_file_is_main) {
  result <- run_cross_scale_identity_validation()
  print(result$roster_count_summary)
}
