# Historical replay path contract. No audit calculation or live S: write.
source("Functions/project_paths.R")
root <- file.path(tempdir(), paste0("historical_audit_replay_",
                                    as.integer(runif(1L, 1L, 1e9))))
ready <- file.path(root, "analysis_ready")
old03 <- file.path(ready, "03_derived_metrics")
old06 <- file.path(ready, "06_behavioral_dynamics")
old12 <- file.path(ready, "12_systems_neuroscience_summary")
for (path in c(old03, file.path(old06, "dyadic_contacts"),
               file.path(old12, "5min_based"))) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
same_path <- function(a, b) identical(normalizePath(a, winslash = "/", mustWork = FALSE),
                                      normalizePath(b, winslash = "/", mustWork = FALSE))
stopifnot(same_path(mmm_behavior_numbered_source_root("03_derived_metrics", root), old03),
          same_path(mmm_behavior_numbered_source_root("06_behavioral_dynamics", root), old06),
          same_path(mmm_behavior_numbered_source_root("12_systems_neuroscience_summary", root), old12))

out <- mmm_behavior_audit_replay_output_root(
  "first_night_candidate_set_scores", root, "review_20260924")
stopifnot(identical(out, file.path(ready, "analyses", "historical_audit_replays",
                                   "review_20260924", "first_night_candidate_set_scores")),
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, "../escape"), silent = TRUE),
            "try-error"),
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, ""), silent = TRUE),
            "try-error"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(mmm_behavior_audit_replay_output_root(
  "first_night_candidate_set_scores", root, "review_20260924"), silent = TRUE),
  "try-error"))

archived06 <- file.path(ready, "history", "original_layout", "06_behavioral_dynamics")
dir.create(dirname(archived06), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.rename(old06, archived06))
receipt_dir <- file.path(ready, "_migration_control", "numbered_root_archive")
dir.create(receipt_dir, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(
  root = "06_behavioral_dynamics",
  source_root_rel = "06_behavioral_dynamics",
  archive_root_rel = "history/original_layout/06_behavioral_dynamics",
  state = "activated", files = 1L, bytes = 1L,
  manifest_sha256 = paste(rep("c", 64L), collapse = "")),
  file.path(receipt_dir, "06_behavioral_dynamics.json"), auto_unbox = TRUE)
stopifnot(same_path(mmm_behavior_numbered_source_root("06_behavioral_dynamics", root),
                    archived06))

script <- readLines("Testing/audits/audit_first_night_candidate_set_scores.R",
                    warn = FALSE)
invisible(parse(text = script))
stopifnot(any(grepl('anchor_long_path <- file.path(HIST_AUDIT,', script,
                   fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root(', script,
                   fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', script,
                     fixed = TRUE)))
cat("Historical audit replay source and fresh-output paths: PASS\n")
