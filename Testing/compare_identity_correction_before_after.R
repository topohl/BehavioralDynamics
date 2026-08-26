# ================================================================
# Real, parameterized identity-correction before/after comparison
# MMMSociability
# ================================================================
# Replaces the old compare_stage03_identity_correction.R, which hardcoded a
# Windows Temp baseline, covered only Stage 03, and hardcoded
# significance_threshold_crossed = FALSE / interpretation_changed = FALSE.
#
# This compares BOTH Stage 03 (raw movement/phase statistics) and Stage 09
# (early prediction model ladder) manuscript-relevant tables between an old
# (pre-identity-correction) baseline snapshot and the current data, using the
# comparison engine in Functions/identity_correction_comparison_helpers.R.
#
# Baseline root resolution (first match wins; there is no hardcoded default):
#   1. the `baseline_root` argument to run_identity_correction_comparison()
#   2. Rscript Testing/compare_identity_correction_before_after.R --baseline-root=<path>
#   3. options(mmm.identity_baseline_root = "<path>")
#   4. Sys.setenv(MMM_IDENTITY_BASELINE_ROOT = "<path>")
#
# The baseline root must be a preserved copy of the SAME kind of root as
# base_dir below (i.e. a directory whose child is `analysis_ready/`). See the
# deliverables report for the exact command to create one.
#
# baseline_status is REQUIRED context, not decoration: a snapshot of the
# current S: tree is NOT a pristine pre-identity baseline (10min was patched
# in place on 2026-08-12 while other scales were not), so this tool never
# infers "pristine_pre_identity" on its own. Pass one of:
#   pristine_pre_identity          -- independently verified to predate the correction
#   mixed_partial_identity_repair  -- known partial/inconsistent repair state
#   unknown_historical_state       -- default; provenance not established
# along with baseline_source_note describing how/when the baseline was made.
#
# Outputs are written under the NEW data's own audit/ folders on the data
# drive (never into the git repository), so private/raw result data is never
# committed.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
})

source("Analysis/_pipeline_setup.R")
source("Functions/identity_correction_comparison_helpers.R")

resolve_identity_baseline_root <- function(explicit = NULL) {
  if (!is.null(explicit) && nzchar(explicit)) return(explicit)

  cli_args <- commandArgs(trailingOnly = TRUE)
  cli_match <- cli_args[startsWith(cli_args, "--baseline-root=")]
  if (length(cli_match) > 0) return(sub("^--baseline-root=", "", cli_match[[1]]))

  opt <- getOption("mmm.identity_baseline_root", default = NA_character_)
  if (!is.na(opt) && nzchar(opt)) return(opt)

  env <- Sys.getenv("MMM_IDENTITY_BASELINE_ROOT", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) return(env)

  NA_character_
}

# Valid baseline_status values. "pristine_pre_identity" is never inferred
# automatically anywhere in this file -- it can only be reached by the caller
# explicitly passing it, having verified the baseline predates the identity
# correction (e.g. a backup from before 2026-08-12 for this dataset).
IDENTITY_BASELINE_STATUSES <- c("pristine_pre_identity", "mixed_partial_identity_repair", "unknown_historical_state")

resolve_identity_baseline_status <- function(explicit = NULL) {
  if (!is.null(explicit) && nzchar(explicit)) return(explicit)
  cli_args <- commandArgs(trailingOnly = TRUE)
  cli_match <- cli_args[startsWith(cli_args, "--baseline-status=")]
  if (length(cli_match) > 0) return(sub("^--baseline-status=", "", cli_match[[1]]))
  opt <- getOption("mmm.identity_baseline_status", default = NA_character_)
  if (!is.na(opt) && nzchar(opt)) return(opt)
  "unknown_historical_state"
}

resolve_identity_baseline_source_note <- function(explicit = NULL) {
  if (!is.null(explicit) && nzchar(explicit)) return(explicit)
  cli_args <- commandArgs(trailingOnly = TRUE)
  cli_match <- cli_args[startsWith(cli_args, "--baseline-source-note=")]
  if (length(cli_match) > 0) return(sub("^--baseline-source-note=", "", cli_match[[1]]))
  opt <- getOption("mmm.identity_baseline_source_note", default = NA_character_)
  if (!is.na(opt) && nzchar(opt)) return(opt)
  NA_character_
}

#' Current code commit SHA, if this is a git working copy. Never fabricated;
#' NA (with a warning) if git or the repo is unavailable. Also reports
#' whether the working tree has uncommitted changes relative to that SHA.
current_code_commit_info <- function() {
  sha <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (length(sha) != 1L || !nzchar(sha) || grepl("fatal", sha, ignore.case = TRUE)) {
    warning("Could not determine the current git commit SHA; recording NA.", call. = FALSE)
    return(list(sha = NA_character_, dirty = NA))
  }
  status <- tryCatch(
    suppressWarnings(system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  list(sha = sha, dirty = length(status) > 0L)
}

#' Resolve one registered table on one side (baseline or current), trying the
#' canonical analysis_ready/pipeline/ location first and the documented
#' pre-migration legacy location second, exactly mirroring how the live
#' pipeline (Analysis/16_manuscript_behavior_report.R) resolves the same
#' sources -- just rooted at whichever `root` is passed in.
resolve_registered_table <- function(root, stage, table_name, legacy_subfolder, legacy_filename, resolution_10min) {
  legacy_filename <- if (is.na(legacy_filename)) table_name else legacy_filename
  if (identical(stage, "03")) {
    canonical <- file.path(behavior_stage_tables(root, "03", "movement_phase_stats", resolution_10min), paste0(table_name, ".csv"))
    legacy <- file.path(root, "analysis_ready/03_primary_raw_movement_phase_stats", paste0(resolution_10min, "_based"), legacy_subfolder, paste0(table_name, ".csv"))
  } else if (identical(stage, "09")) {
    canonical <- file.path(behavior_stage_tables(root, "09", "early_prediction", resolution_10min), paste0(table_name, ".csv"))
    legacy <- file.path(root, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder", paste0(resolution_10min, "_based"), "tables", legacy_subfolder, paste0(legacy_filename, ".csv"))
  } else {
    stop("Unknown stage in registry: ", stage, call. = FALSE)
  }
  resolve_behavior_artifact(canonical_path = canonical, legacy_paths = legacy, required = FALSE, source_id = paste0("stage", stage, ":", table_name))
}

read_registered_table <- function(resolved) {
  if (!isTRUE(resolved$exists)) return(NULL)
  suppressWarnings(readr::read_csv(resolved$path, show_col_types = FALSE, progress = FALSE))
}

#' Run the full before/after comparison. This is the function to call/source;
#' it performs no work at source() time.
#'
#' @param baseline_status One of "pristine_pre_identity",
#'   "mixed_partial_identity_repair", "unknown_historical_state". Defaults to
#'   "unknown_historical_state" -- NEVER auto-inferred as pristine. Pass
#'   "pristine_pre_identity" only when you have independently verified the
#'   baseline predates the identity correction; pass
#'   "mixed_partial_identity_repair" for a snapshot of a tree known to be
#'   partially repaired (e.g. the current S: state, where 10min was patched
#'   2026-08-12 but other scales were not).
#' @param baseline_source_note Free-text provenance note describing how/when
#'   the baseline was captured (e.g. "robocopy snapshot of S: taken
#'   2026-08-26, after the 2026-08-12 in-place 10min repair"). Strongly
#'   recommended; a warning is raised if omitted, because an undocumented
#'   baseline is exactly what produced the fake audit this tool replaces.
run_identity_correction_comparison <- function(baseline_root = NULL,
                                               baseline_status = NULL,
                                               baseline_source_note = NULL,
                                               new_base_dir = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID",
                                               resolution_10min = "10min",
                                               significance_threshold = 0.05,
                                               write_outputs = TRUE) {
  baseline_root <- resolve_identity_baseline_root(baseline_root)
  if (is.na(baseline_root) || !nzchar(baseline_root)) {
    stop(
      "No identity-correction baseline root was supplied; refusing to guess. Provide one via:\n",
      "  - run_identity_correction_comparison(baseline_root = \"<path>\"),\n",
      "  - Rscript Testing/compare_identity_correction_before_after.R --baseline-root=<path>,\n",
      "  - options(mmm.identity_baseline_root = \"<path>\"), or\n",
      "  - Sys.setenv(MMM_IDENTITY_BASELINE_ROOT = \"<path>\").\n",
      "There is no default baseline; a pre-identity-correction snapshot must be preserved explicitly before rebuilding.",
      call. = FALSE
    )
  }
  if (!dir.exists(baseline_root)) {
    stop("Identity-correction baseline root does not exist or is not reachable: ", baseline_root, call. = FALSE)
  }

  baseline_status <- resolve_identity_baseline_status(baseline_status)
  if (!baseline_status %in% IDENTITY_BASELINE_STATUSES) {
    stop(
      "Invalid baseline_status '", baseline_status, "'. Must be one of: ",
      paste(IDENTITY_BASELINE_STATUSES, collapse = ", "), call. = FALSE
    )
  }
  baseline_source_note <- resolve_identity_baseline_source_note(baseline_source_note)
  if (is.na(baseline_source_note)) {
    warning(
      "No baseline_source_note was supplied. Document how/when this baseline was captured ",
      "(baseline_source_note argument, --baseline-source-note=, or options(mmm.identity_baseline_source_note=...)) ",
      "so the audit output is not an unlabeled snapshot of unknown provenance.",
      call. = FALSE
    )
  }
  if (!dir.exists(new_base_dir)) {
    stop("Current (new) data root does not exist or is not reachable: ", new_base_dir, call. = FALSE)
  }
  if (normalizePath(baseline_root, winslash = "/", mustWork = FALSE) ==
      normalizePath(new_base_dir, winslash = "/", mustWork = FALSE)) {
    stop(
      "Baseline root and current data root resolve to the same location (",
      normalizePath(baseline_root, winslash = "/", mustWork = FALSE),
      "); refusing to silently compare a dataset with itself.",
      call. = FALSE
    )
  }

  registry <- identity_comparison_table_registry

  resolved_pairs <- registry %>%
    mutate(
      old_resolved = pmap(
        list(table_name, stage, legacy_subfolder, legacy_filename),
        function(tn, st, ls, lf) resolve_registered_table(baseline_root, st, tn, ls, lf, resolution_10min)
      ),
      new_resolved = pmap(
        list(table_name, stage, legacy_subfolder, legacy_filename),
        function(tn, st, ls, lf) resolve_registered_table(new_base_dir, st, tn, ls, lf, resolution_10min)
      ),
      old_path = map_chr(old_resolved, "path"),
      new_path = map_chr(new_resolved, "path"),
      old_exists = map_lgl(old_resolved, "exists"),
      new_exists = map_lgl(new_resolved, "exists")
    )

  same_path <- with(resolved_pairs, old_exists & new_exists &
    normalizePath(old_path, winslash = "/", mustWork = FALSE) == normalizePath(new_path, winslash = "/", mustWork = FALSE))
  if (any(same_path)) {
    stop(
      "Refusing to silently compare a file with itself for: ",
      paste(resolved_pairs$table_name[same_path], collapse = ", "),
      call. = FALSE
    )
  }

  detail <- pmap_dfr(resolved_pairs, function(stage, table_name, key_cols_str, n_col, estimate_col,
                                              effect_size_col, p_raw_col, p_adjusted_col,
                                              old_path, new_path, old_exists, new_exists, ...) {
    old_dat <- if (old_exists) suppressWarnings(read_csv(old_path, show_col_types = FALSE, progress = FALSE)) else NULL
    new_dat <- if (new_exists) suppressWarnings(read_csv(new_path, show_col_types = FALSE, progress = FALSE)) else NULL
    compare_identity_table(
      old_dat, new_dat, table_name = table_name, stage = stage,
      key_cols = split_key_cols(key_cols_str),
      n_col = n_col, estimate_col = estimate_col, effect_size_col = effect_size_col,
      p_raw_col = p_raw_col, p_adjusted_col = p_adjusted_col,
      significance_threshold = significance_threshold
    )
  })

  animal_keyed_tables <- c("raw_movement_animal_level_endpoints", "model_ladder_input", "primary_prediction_predictions")
  composition <- pmap_dfr(resolved_pairs %>% filter(table_name %in% animal_keyed_tables), function(table_name, old_path, new_path, old_exists, new_exists, ...) {
    old_dat <- if (old_exists) suppressWarnings(read_csv(old_path, show_col_types = FALSE, progress = FALSE)) else NULL
    new_dat <- if (new_exists) suppressWarnings(read_csv(new_path, show_col_types = FALSE, progress = FALSE)) else NULL
    compare_animal_composition(old_dat, new_dat, table_name)
  })

  summary_tbl <- summarise_identity_comparison(detail)
  stage09_headline <- stage09_headline_summary(detail)

  resolution_notes <- resolved_pairs %>%
    select(stage, table_name, old_exists, new_exists, old_path, new_path)

  code_commit <- current_code_commit_info()
  audit_metadata <- tibble(
    baseline_root = baseline_root,
    baseline_status = baseline_status,
    baseline_source_note = baseline_source_note,
    new_base_dir = new_base_dir,
    comparison_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    code_commit_sha = code_commit$sha,
    code_working_tree_dirty = code_commit$dirty
  )
  if (identical(baseline_status, "mixed_partial_identity_repair") || identical(baseline_status, "unknown_historical_state")) {
    message(
      "baseline_status = '", baseline_status, "': this comparison does NOT isolate the effect of the identity ",
      "correction and must not be reported as a clean before/after. See identity_correction_audit_metadata.csv."
    )
  }

  result <- list(
    detail = detail,
    summary = summary_tbl,
    stage09_headline = stage09_headline,
    animal_composition = composition,
    resolution_notes = resolution_notes,
    audit_metadata = audit_metadata,
    baseline_root = baseline_root,
    baseline_status = baseline_status,
    new_base_dir = new_base_dir
  )

  if (isTRUE(write_outputs)) {
    out_dir <- behavior_stage_audit(new_base_dir, "09", "early_prediction", resolution_10min)
    ensure_dir(out_dir)
    readr::write_csv(detail, file.path(out_dir, "identity_correction_before_after_detail.csv"), na = "NA")
    readr::write_csv(summary_tbl, file.path(out_dir, "identity_correction_before_after_summary.csv"), na = "NA")
    readr::write_csv(stage09_headline, file.path(out_dir, "stage09_identity_correction_headline_summary.csv"), na = "NA")
    readr::write_csv(composition, file.path(out_dir, "identity_correction_animal_composition.csv"), na = "NA")
    readr::write_csv(resolution_notes, file.path(out_dir, "identity_correction_source_resolution.csv"), na = "NA")
    readr::write_csv(audit_metadata, file.path(out_dir, "identity_correction_audit_metadata.csv"), na = "NA")
    message("Wrote before/after comparison outputs to: ", out_dir)
  }

  invisible(result)
}

# Auto-run only when this file is the one passed to Rscript directly, not
# when it is source()-d by another script/test that just wants the functions
# and registry defined.
.identity_comparison_file_is_main <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_flag <- sub("^--file=", "", file_arg[grepl("^--file=", file_arg)])
  length(file_flag) > 0 && grepl("compare_identity_correction_before_after\\.R$", file_flag[[1]])
})

if (.identity_comparison_file_is_main) {
  result <- run_identity_correction_comparison()
  print(result$summary)
}
