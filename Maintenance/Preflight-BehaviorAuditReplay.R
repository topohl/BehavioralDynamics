# Read-only mechanical preflight for the 37 historical audit replays.
# Does not source audits or create a replay directory.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 5L) {
  stop("Usage: Rscript Maintenance/Preflight-BehaviorAuditReplay.R ",
       "<RFID-project-root> <replay-id> [baseline-root] ",
       "[baseline-status] [baseline-source-note]", call. = FALSE)
}
source("Functions/project_paths.R")
project_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
run_id <- args[[2L]]
baseline_root <- if (length(args) >= 3L) args[[3L]] else ""
baseline_status <- if (length(args) >= 4L) args[[4L]] else ""
baseline_note <- if (length(args) >= 5L) args[[5L]] else ""
run_path <- dirname(mmm_behavior_audit_replay_path(
  "preflight", project_root, run_id))

plan_file <- tempfile(fileext = ".csv")
plan_error <- tempfile(fileext = ".txt")
plan_status <- suppressWarnings(system2(
  "Rscript", "Maintenance/Get-BehaviorAuditReplayPlan.R",
  stdout = plan_file, stderr = plan_error))
if (!identical(plan_status, 0L)) {
  error_text <- paste(readLines(plan_error, warn = FALSE), collapse = " | ")
  unlink(c(plan_file, plan_error))
  stop("Replay dependency plan failed: ",
       error_text,
       call. = FALSE)
}
plan <- read.csv(plan_file, stringsAsFactors = FALSE, check.names = FALSE)
unlink(c(plan_file, plan_error))
if (nrow(plan) != 37L) stop("Expected 37 queued audit scripts.", call. = FALSE)

roots <- c("03_derived_metrics", "06_behavioral_dynamics",
           "12_systems_neuroscience_summary")
sources <- vapply(roots, mmm_behavior_numbered_source_root,
                  character(1), project_root = project_root)
source_ok <- dir.exists(sources)
fresh <- !file.exists(run_path) && !dir.exists(run_path)
baseline_ok <- nzchar(baseline_root) && dir.exists(baseline_root) &&
  dir.exists(file.path(baseline_root, "analysis_ready")) &&
  !identical(normalizePath(baseline_root, winslash = "/", mustWork = TRUE),
             project_root) &&
  baseline_status %in% c("pristine_pre_identity",
                         "mixed_partial_identity_repair",
                         "unknown_historical_state") && nzchar(baseline_note)

cat("replay_id:", run_id, "\n")
cat("replay_path:", run_path, "\n")
cat("fresh_replay_path:", fresh, "\n")
for (i in seq_along(roots)) {
  cat("retained_source:", roots[[i]], source_ok[[i]], sources[[i]], "\n")
}
cat("queued_scripts:", nrow(plan), "\n")
cat("baseline_supplied_with_status_and_note:", baseline_ok, "\n")
cat("mechanically_ready:", fresh && all(source_ok) && baseline_ok, "\n")
if (!fresh || !all(source_ok)) {
  stop("Replay path or retained source preflight failed.", call. = FALSE)
}
if (!baseline_ok) {
  cat("identity_comparison: BLOCKED until an independent baseline, its ",
      "status, and a source note are supplied.\n", sep = "")
  stop("All-script replay preflight is incomplete.", call. = FALSE)
}
