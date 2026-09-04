# Portable regression tests for the identity-correction comparison DRIVER
# (Testing/audits/compare_identity_correction_before_after.R): baseline resolution,
# fail-closed behavior, and the never-compare-a-file-with-itself guard. Runs
# entirely against tempdir() fixtures; no S: drive access required.

suppressPackageStartupMessages({
  library(readr)
})

Sys.unsetenv("MMM_IDENTITY_BASELINE_ROOT")
options(mmm.identity_baseline_root = NULL)

source("Testing/audits/compare_identity_correction_before_after.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

write_stub_table <- function(root, stage_dir, filename, df) {
  dir.create(file.path(root, stage_dir), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, file.path(root, stage_dir, filename))
}

# ------------------------------------------------------------------
# Test 1: no baseline supplied anywhere -> fail closed with a clear message.
# ------------------------------------------------------------------
new_root_1 <- file.path(tempdir(), paste0("mmm_cmp_new1_", as.integer(runif(1, 1, 1e9))))
dir.create(new_root_1, recursive = TRUE, showWarnings = FALSE)
err1 <- tryCatch({
  run_identity_correction_comparison(new_base_dir = new_root_1, write_outputs = FALSE)
  NULL
}, error = function(e) e)
check(!is.null(err1), "Test 1: expected an error when no baseline root is supplied anywhere")
check(grepl("No identity-correction baseline root", conditionMessage(err1)), "Test 1: error message should explain how to supply a baseline root")
unlink(new_root_1, recursive = TRUE)

# ------------------------------------------------------------------
# Test 2: a baseline root that does not exist -> fail closed.
# ------------------------------------------------------------------
new_root_2 <- file.path(tempdir(), paste0("mmm_cmp_new2_", as.integer(runif(1, 1, 1e9))))
dir.create(new_root_2, recursive = TRUE, showWarnings = FALSE)
nonexistent_baseline <- file.path(tempdir(), paste0("mmm_cmp_does_not_exist_", as.integer(runif(1, 1, 1e9))))
err2 <- tryCatch({
  run_identity_correction_comparison(baseline_root = nonexistent_baseline, new_base_dir = new_root_2, write_outputs = FALSE)
  NULL
}, error = function(e) e)
check(!is.null(err2), "Test 2: expected an error when the baseline root does not exist")
unlink(new_root_2, recursive = TRUE)

# ------------------------------------------------------------------
# Test 3: baseline root identical to the new root -> refuse to compare a
# dataset with itself.
# ------------------------------------------------------------------
same_root <- file.path(tempdir(), paste0("mmm_cmp_same_", as.integer(runif(1, 1, 1e9))))
dir.create(same_root, recursive = TRUE, showWarnings = FALSE)
err3 <- tryCatch({
  run_identity_correction_comparison(baseline_root = same_root, new_base_dir = same_root, write_outputs = FALSE)
  NULL
}, error = function(e) e)
check(!is.null(err3), "Test 3: expected an error when baseline_root == new_base_dir")
check(grepl("itself", conditionMessage(err3), ignore.case = TRUE), "Test 3: error message should mention refusing to compare with itself")
unlink(same_root, recursive = TRUE)

# ------------------------------------------------------------------
# Test 4: a real (synthetic) canonical + canonical pair -> comparison runs,
# detects the deliberate difference, and writes outputs under the NEW root's
# own audit/ folder (never into the git repo).
# ------------------------------------------------------------------
baseline_root_4 <- file.path(tempdir(), paste0("mmm_cmp_base4_", as.integer(runif(1, 1, 1e9))))
new_root_4 <- file.path(tempdir(), paste0("mmm_cmp_new4_", as.integer(runif(1, 1, 1e9))))

old_assoc <- data.frame(feature = c("Movement_mean", "Movement_rmssd", "Entropy_acf1"),
                         n = c(58, 58, 58), spearman_rho = c(0.10, -0.20, 0.05),
                         spearman_p = c(0.40, 0.10, 0.70), spearman_p_bh = c(0.40, 0.15, 0.70))
new_assoc <- data.frame(feature = c("Movement_mean", "Movement_rmssd", "Entropy_acf1"),
                         n = c(57, 58, 58), spearman_rho = c(0.32, -0.20, 0.05),
                         spearman_p = c(0.03, 0.10, 0.70), spearman_p_bh = c(0.05, 0.15, 0.70))

write_stub_table(baseline_root_4, "analysis_ready/pipeline/09_early_prediction/10min/tables", "primary_movement_entropyacf1_associations.csv", old_assoc)
write_stub_table(new_root_4, "analysis_ready/pipeline/09_early_prediction/10min/tables", "primary_movement_entropyacf1_associations.csv", new_assoc)

result4 <- run_identity_correction_comparison(
  baseline_root = baseline_root_4, new_base_dir = new_root_4, write_outputs = TRUE,
  baseline_status = "mixed_partial_identity_repair",
  baseline_source_note = "synthetic fixture for test_compare_identity_correction_driver.R"
)

assoc_detail <- result4$detail[result4$detail$table == "primary_movement_entropyacf1_associations" & result4$detail$column == "spearman_rho", ]
movement_mean_row <- assoc_detail[assoc_detail$row_key == "Movement_mean", ]
check(nrow(movement_mean_row) == 1L, "Test 4: expected a Movement_mean spearman_rho comparison row")
check(isTRUE(all.equal(movement_mean_row$absolute_difference, 0.22)), "Test 4: expected absolute_difference of 0.22 for Movement_mean spearman_rho")

resolution_notes_row <- result4$resolution_notes[result4$resolution_notes$table_name == "primary_movement_entropyacf1_associations", ]
check(all(resolution_notes_row$old_exists, resolution_notes_row$new_exists), "Test 4: both sides of the synthetic fixture should have resolved")

out_dir_4 <- behavior_stage_audit(new_root_4, "09", "early_prediction", "10min")
expected_outputs <- c(
  "identity_correction_before_after_detail.csv",
  "identity_correction_before_after_summary.csv",
  "stage09_identity_correction_headline_summary.csv",
  "identity_correction_animal_composition.csv",
  "identity_correction_source_resolution.csv",
  "identity_correction_audit_metadata.csv"
)
for (f in expected_outputs) {
  check(file.exists(file.path(out_dir_4, f)), paste0("Test 4: expected output file missing: ", f))
}
check(normalizePath(out_dir_4, winslash = "/") != normalizePath(getwd(), winslash = "/"),
      "Test 4: outputs must be written under the data root, not the git working directory")
check(!grepl(normalizePath(getwd(), winslash = "/"), normalizePath(out_dir_4, winslash = "/"), fixed = TRUE),
      "Test 4: outputs must not land anywhere inside the git repository")

meta4 <- result4$audit_metadata
check(identical(meta4$baseline_status, "mixed_partial_identity_repair"), "Test 4: audit_metadata must record the supplied baseline_status")
check(identical(meta4$baseline_source_note, "synthetic fixture for test_compare_identity_correction_driver.R"), "Test 4: audit_metadata must record the supplied baseline_source_note")
check(identical(meta4$baseline_root, baseline_root_4), "Test 4: audit_metadata must record the baseline root")
check(!is.na(meta4$comparison_timestamp) && nzchar(meta4$comparison_timestamp), "Test 4: audit_metadata must record a comparison timestamp")
check("code_commit_sha" %in% names(meta4), "Test 4: audit_metadata must include a code_commit_sha field (NA is acceptable outside a git repo)")

unlink(baseline_root_4, recursive = TRUE)
unlink(new_root_4, recursive = TRUE)

# ------------------------------------------------------------------
# Test 5: baseline_status defaults to 'unknown_historical_state' and is NEVER
# silently inferred as 'pristine_pre_identity'; invalid values are rejected.
# ------------------------------------------------------------------
baseline_root_5 <- file.path(tempdir(), paste0("mmm_cmp_base5_", as.integer(runif(1, 1, 1e9))))
new_root_5 <- file.path(tempdir(), paste0("mmm_cmp_new5_", as.integer(runif(1, 1, 1e9))))
dummy <- data.frame(feature = "Movement_mean", n = 10, spearman_rho = 0.1, spearman_p = 0.5, spearman_p_bh = 0.5)
write_stub_table(baseline_root_5, "analysis_ready/pipeline/09_early_prediction/10min/tables", "primary_movement_entropyacf1_associations.csv", dummy)
write_stub_table(new_root_5, "analysis_ready/pipeline/09_early_prediction/10min/tables", "primary_movement_entropyacf1_associations.csv", dummy)

result5_default <- run_identity_correction_comparison(baseline_root = baseline_root_5, new_base_dir = new_root_5, write_outputs = FALSE)
check(identical(result5_default$audit_metadata$baseline_status, "unknown_historical_state"),
      "Test 5: with no baseline_status supplied, the default must be 'unknown_historical_state', never 'pristine_pre_identity'")

err5 <- tryCatch({
  run_identity_correction_comparison(baseline_root = baseline_root_5, new_base_dir = new_root_5, write_outputs = FALSE, baseline_status = "definitely_clean")
  NULL
}, error = function(e) e)
check(!is.null(err5), "Test 5: an invalid baseline_status value must be rejected")

result5_pristine <- run_identity_correction_comparison(baseline_root = baseline_root_5, new_base_dir = new_root_5, write_outputs = FALSE, baseline_status = "pristine_pre_identity")
check(identical(result5_pristine$audit_metadata$baseline_status, "pristine_pre_identity"),
      "Test 5: baseline_status = 'pristine_pre_identity' must be honored ONLY when the caller explicitly passes it")

unlink(baseline_root_5, recursive = TRUE)
unlink(new_root_5, recursive = TRUE)

cat("Identity correction comparison driver contract checks: PASS\n")
