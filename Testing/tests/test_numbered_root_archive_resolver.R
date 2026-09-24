# Archive receipt routing is tested only in a temporary fixture. No live
# numbered output root or scientific file is moved by this test.
source("Functions/project_paths.R")

root <- file.path(tempdir(), paste0("numbered_archive_resolver_",
                                    as.integer(runif(1L, 1L, 1e9))))
ready <- file.path(root, "analysis_ready")
group <- "dyadic_contacts"
numbered <- "06_behavioral_dynamics"
old_root <- file.path(ready, numbered)
old_group <- mmm_behavior_output_group_root(group, "current", root)
semantic <- mmm_behavior_output_group_root(group, "semantic", root)
archived_root <- file.path(ready, "history", "original_layout", numbered)
archived_group <- file.path(archived_root, "dyadic_contacts")
archive_receipt <- file.path(ready, "_migration_control",
                             "numbered_root_archive", paste0(numbered, ".json"))
activation_receipt <- file.path(ready, "_migration_control", paste0(group, ".json"))
dir.create(old_group, recursive = TRUE, showWarnings = FALSE)
dir.create(semantic, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(archive_receipt), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(
  group = group, state = "activated", files = 1L,
  target_root_rel = "analyses/dyadic_contacts",
  group_plan_sha256 = paste(rep("a", 64L), collapse = ""),
  contract_sha256 = paste(rep("b", 64L), collapse = ""),
  source_retained = TRUE), activation_receipt, auto_unbox = TRUE)

check_error <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
write_archive_receipt <- function(state, archive_rel = paste0(
                                    "history/original_layout/", numbered)) {
  jsonlite::write_json(list(
    root = numbered, source_root_rel = numbered,
    archive_root_rel = archive_rel, state = state,
    files = 1L, bytes = 10L,
    manifest_sha256 = paste(rep("c", 64L), collapse = "")),
    archive_receipt, auto_unbox = TRUE)
}

stopifnot(identical(mmm_behavior_retained_source_root(group, root), old_group),
          identical(mmm_behavior_output_active_root(group, root), semantic),
          identical(mmm_behavior_numbered_writer_root(numbered, root), old_root))

# An unreceipted archive, including one made while the original still exists,
# must not silently select either copy.
dir.create(archived_root, recursive = TRUE, showWarnings = FALSE)
stopifnot(check_error(mmm_behavior_output_active_root(group, root)),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)))
stopifnot(file.rename(archived_root, paste0(archived_root, "_unreceipted")))

write_archive_receipt("prepared")
stopifnot(identical(mmm_behavior_output_active_root(group, root), semantic),
          identical(mmm_behavior_retained_source_root(group, root), old_group),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)))
write_archive_receipt("transferring")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))
write_archive_receipt("activated")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))

dir.create(dirname(archived_root), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.rename(old_root, archived_root))
stopifnot(identical(mmm_behavior_retained_source_root(group, root), archived_group),
          identical(mmm_behavior_output_active_root(group, root), semantic),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)))

# A group without its own semantic activation still resolves the retained file
# inside the archive when the root-level archive receipt is activated.
other_group <- "history_social_networks_1min"
other_archived <- file.path(archived_root, "social_networks", "1min_based")
dir.create(other_archived, recursive = TRUE, showWarnings = FALSE)
stopifnot(identical(mmm_behavior_output_active_root(other_group, root),
                    other_archived))

dir.create(old_root, recursive = TRUE, showWarnings = FALSE)
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))
stopifnot(file.rename(old_root, paste0(old_root, "_recreated")))
write_archive_receipt("activated", "history/original_layout/another_root")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))
write_archive_receipt("unknown")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))

contracts <- c(
  "Analysis/14_systems_neuroscience_summary_dashboard.R",
  "Analysis/_archive/08_early_prediction_models.R",
  "Testing/legacy/run_behavioral_dynamics_pipeline.R",
  "Testing/legacy/run_full_systems_behavior_pipeline.R")
for (script in contracts) {
  code <- readLines(script, warn = FALSE)
  invisible(parse(text = code))
  stopifnot(any(grepl('mmm_behavior_numbered_writer_root(', code,
                     fixed = TRUE)))
}
cat("Numbered root archive receipt resolver: PASS\n")
