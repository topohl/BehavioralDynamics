# Receipt routing for the top-level trees added to the original-layout archive
# after 03/06/12. Temporary fixture only; no live path is read or written.
source("Functions/project_paths.R")

root <- normalizePath(file.path(tempdir(), paste0("original_layout_roots_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
receipts <- file.path(ready, "_migration_control", "numbered_root_archive")
dir.create(receipts, recursive = TRUE)
check_error <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
archive_receipt <- function(root_name, state) {
  jsonlite::write_json(list(
    root = root_name, source_root_rel = root_name,
    archive_root_rel = paste0("history/original_layout/", root_name), state = state,
    files = 1L, bytes = 1L, manifest_sha256 = strrep("c", 64L),
    reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
    reader_queue_sha256 = strrep("f", 64L)),
    file.path(receipts, paste0(root_name, ".json")), auto_unbox = TRUE)
}
archive_now <- function(root_name) {
  dir.create(file.path(ready, "history", "original_layout"), recursive = TRUE,
             showWarnings = FALSE)
  stopifnot(file.rename(file.path(ready, root_name),
                        file.path(ready, "history", "original_layout", root_name)))
  archive_receipt(root_name, "activated")
}
group_receipt <- function(group, target) {
  jsonlite::write_json(list(
    group = group, state = "activated", files = 1L, target_root_rel = target,
    group_plan_sha256 = strrep("a", 64L), contract_sha256 = strrep("b", 64L),
    source_retained = TRUE),
    file.path(ready, "_migration_control", paste0(group, ".json")), auto_unbox = TRUE)
}

# 1. A tree with no output group follows its receipt through every state.
tree <- "16_manuscript_behavior_report"
old <- file.path(ready, tree)
archived <- file.path(ready, "history", "original_layout", tree)
dir.create(file.path(old, "10min_based"), recursive = TRUE)
stopifnot(tree %in% MMM_NUMBERED_BEHAVIOR_ROOTS,
          identical(mmm_behavior_numbered_source_root(tree, root), old),
          identical(mmm_behavior_guard_numbered_output_path(old, root), old),
          check_error(mmm_behavior_numbered_source_root("not_a_root", root)))
archive_receipt(tree, "prepared")
stopifnot(identical(mmm_behavior_numbered_source_root(tree, root), old),
          check_error(mmm_behavior_guard_numbered_output_path(old, root)))
archive_receipt(tree, "transferring")
stopifnot(check_error(mmm_behavior_numbered_source_root(tree, root)))
archive_now(tree)
stopifnot(identical(mmm_behavior_numbered_source_root(tree, root), archived),
          check_error(mmm_behavior_guard_numbered_output_path(
            file.path(archived, "10min_based"), root)),
          check_error(mmm_behavior_guard_numbered_output_path(
            file.path(old, "10min_based"), root)))
dir.create(old)   # a writer on another branch recreated the old root
stopifnot(check_error(mmm_behavior_numbered_source_root(tree, root)))
unlink(old, recursive = TRUE)
unlink(file.path(receipts, paste0(tree, ".json")))
stopifnot(check_error(mmm_behavior_numbered_source_root(tree, root)))  # archive without receipt
archive_receipt(tree, "activated")

# 2. Five-minute Stage 11-13 branches and non-5-minute supporting branches are
#    read from the retained original root.
for (spec in list(c("sleep_like_inactivity", "16_sleep_like_inactivity_metrics"),
                  c("adaptation_kinetics", "15_behavioral_adaptation_kinetics"))) {
  dir.create(file.path(ready, spec[[2]], "5min_based", "tables"), recursive = TRUE)
  stopifnot(identical(mmm_phase_analysis_resolution_root(spec[[1]], "5min_based", root),
                      file.path(ready, spec[[2]], "5min_based")))
  archive_now(spec[[2]])
  stopifnot(identical(mmm_phase_analysis_resolution_root(spec[[1]], "5min_based", root),
                      file.path(ready, "history", "original_layout", spec[[2]], "5min_based")))
}
dir.create(file.path(ready, "13_nonlinear_systems_dynamics", "5min_based"), recursive = TRUE)
archive_now("13_nonlinear_systems_dynamics")
stopifnot(identical(mmm_supporting_resolution_root("nonlinear_dynamics", "10min_based", root),
                    file.path(ready, "history", "original_layout",
                              "13_nonlinear_systems_dynamics", "10min_based")),
          check_error(mmm_behavior_guard_numbered_output_path(
            mmm_supporting_resolution_root("nonlinear_dynamics", "10min_based", root), root)))

# 3. The May 2026 QC snapshot and the proteomics inputs: the original until the
#    copy is activated, then the copy, also after the original is archived.
for (spec in list(c("history_tracking_integrity_10sec", "00_qc_tracking_integrity",
                    "history/tracking_integrity/10sec"),
                  c("proteomics_module_scores", "proteomics",
                    "foundations/proteomics_module_scores"))) {
  group <- spec[[1]]; original <- file.path(ready, spec[[2]]); copy <- file.path(ready, spec[[3]])
  dir.create(original, recursive = TRUE)
  stopifnot(identical(mmm_behavior_output_active_root(group, root), original))
  dir.create(copy, recursive = TRUE)
  stopifnot(check_error(mmm_behavior_output_active_root(group, root)))  # copy without receipt
  group_receipt(group, spec[[3]])
  stopifnot(identical(mmm_behavior_output_active_root(group, root), copy))
  archive_now(spec[[2]])
  stopifnot(identical(mmm_behavior_output_active_root(group, root), copy),
            identical(mmm_behavior_retained_source_root(group, root),
                      file.path(ready, "history", "original_layout", spec[[2]])))
}
stopifnot(identical(mmm_tracking_qc_historical_root(root),
                    file.path(ready, "history", "tracking_integrity", "10sec")))
cat("Original-layout roots beyond 03/06/12 follow their receipts: PASS\n")
