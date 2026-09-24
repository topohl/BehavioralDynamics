# Exercise Stage 10's read-only parity audit after an archive cutover using
# empty temporary files. No live RFID output or scientific table is opened.
source("Functions/project_paths.R")
run_fixture <- function() {
# A short temporary root keeps archived filenames under Windows MAX_PATH.
root <- file.path(utils::shortPathName(tempdir()), "s10")
stopifnot(!file.exists(root))
dir.create(root)
on.exit({
  stopifnot(identical(normalizePath(dirname(root), winslash = "/"),
                      normalizePath(tempdir(), winslash = "/")),
            identical(basename(root), "s10"))
  unlink(root, recursive = TRUE)
}, add = TRUE)
ready <- file.path(root, "analysis_ready")
columns <- c("group", "source_rel", "target_root_rel", "target_file")
all_plans <- rbind(
  utils::read.csv("docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv",
                  check.names = FALSE)[, columns],
  utils::read.csv("docs/BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv",
                  check.names = FALSE)[, columns])
plans <- all_plans[startsWith(all_plans$source_rel,
                             "06_behavioral_dynamics/"), , drop = FALSE]
stopifnot(nrow(plans) == 1459L, !anyDuplicated(plans$source_rel))
touch <- function(path, content = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path, useBytes = TRUE)
}
for (i in seq_len(nrow(plans))) {
  # This audit inspects candidate paths and one map header, never table values.
  touch(file.path(ready, plans$source_rel[[i]]))
  touch(file.path(ready, plans$target_root_rel[[i]], plans$target_file[[i]]))
}
supporting <- all_plans[all_plans$group %in% c("nonlinear_dynamics_5min",
                                               "systems_phenotyping_5min"),
                        , drop = FALSE]
for (i in seq_len(nrow(supporting))) {
  touch(file.path(ready, supporting$source_rel[[i]]))
  touch(file.path(ready, supporting$target_root_rel[[i]],
                  supporting$target_file[[i]]))
}
old_map_rel <- "06_behavioral_dynamics/proteomics_integration_output_dir_map.csv"
touch(file.path(ready, old_map_rel), "source,target")

# Every semantic group is active, while the retained originals have been
# moved to history/original_layout. The receipts are synthetic path fixtures.
dir.create(file.path(ready, "_migration_control", "numbered_root_archive"),
           recursive = TRUE, showWarnings = FALSE)
for (group in unique(c(plans$group, supporting$group))) {
  rows <- all_plans[all_plans$group == group, , drop = FALSE]
  stopifnot(length(unique(rows$target_root_rel)) == 1L)
  jsonlite::write_json(list(
    group = group, state = "activated", files = nrow(rows),
    target_root_rel = rows$target_root_rel[[1L]],
    group_plan_sha256 = paste(rep("a", 64L), collapse = ""),
    contract_sha256 = paste(rep("b", 64L), collapse = ""),
    source_retained = TRUE),
    file.path(ready, "_migration_control", paste0(group, ".json")),
    auto_unbox = TRUE)
}
old_root <- file.path(ready, "06_behavioral_dynamics")
archived_root <- file.path(ready, "history", "original_layout",
                           "06_behavioral_dynamics")
dir.create(dirname(archived_root), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.rename(old_root, archived_root), !dir.exists(old_root))
jsonlite::write_json(list(
  root = "06_behavioral_dynamics",
  source_root_rel = "06_behavioral_dynamics",
  archive_root_rel = "history/original_layout/06_behavioral_dynamics",
  state = "activated", files = 1460L, bytes = 1L,
  manifest_sha256 = paste(rep("c", 64L), collapse = "")),
  file.path(ready, "_migration_control", "numbered_root_archive",
            "06_behavioral_dynamics.json"), auto_unbox = TRUE)

recorded_old_map <- normalizePath(
  file.path(normalizePath(root, winslash = "/", mustWork = TRUE),
            "analysis_ready", old_map_rel), winslash = "/", mustWork = FALSE)
source_audit <- file.path(ready, "pipeline", "10_systems_prediction",
                          "10min", "tables", "feature_source_audit.csv")
touch(source_audit, paste0("source_file,loaded_as_feature_table\n\"",
                           recorded_old_map, "\",FALSE"))
result <- suppressWarnings(system2(
  "Rscript", c("Testing/audits/audit_stage10_semantic_discovery_parity.R",
               shQuote(root)), stdout = TRUE, stderr = TRUE))
status <- attr(result, "status")
if (!is.null(status) && status != 0L) {
  stop("Archived Stage 10 parity fixture failed: ",
       paste(result, collapse = " | "), call. = FALSE)
}
stopifnot(any(grepl("606 remaining filtered candidate paths", result,
                   fixed = TRUE)))
cat("Stage 10 archived-location discovery parity fixture: PASS\n")
}
run_fixture()
