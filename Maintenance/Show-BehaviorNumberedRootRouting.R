# Read-only report of how R resolves one numbered behavioral root and its
# groups, whether writers are refused, and whether R can open every file of
# its archive manifest at the retained location. Run from the repository root:
#
#   Rscript Maintenance/Show-BehaviorNumberedRootRouting.R 03_derived_metrics <manifest.csv>
#
# The guard is called as a pure function; nothing is created or written.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: <root name> <archive manifest CSV>", call. = FALSE)
root_name <- args[[1L]]
manifest <- args[[2L]]
source("Functions/project_paths.R")
if (!root_name %in% MMM_NUMBERED_BEHAVIOR_ROOTS) stop("Unknown numbered root: ", root_name, call. = FALSE)
project <- mmm_project_root()
ready <- file.path(project, "analysis_ready")
show <- function(label, expr) {
  value <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  cat(sprintf("%-40s %s\n", label, paste(value, collapse = " ")))
}
receipt <- file.path(ready, "_migration_control", "numbered_root_archive", paste0(root_name, ".json"))
show("receipt state", if (file.exists(receipt))
  jsonlite::fromJSON(receipt, simplifyVector = FALSE)[["state"]] else "none")
show("original present", dir.exists(file.path(ready, root_name)))
show("archive present", dir.exists(file.path(ready, "history", "original_layout", root_name)))
retained <- tryCatch(mmm_behavior_numbered_source_root(root_name, project),
                     error = function(e) NA_character_)
show("retained numbered source", mmm_behavior_numbered_source_root(root_name, project))
# Every output group declared in mmm_behavior_output_group_root().
source_text <- paste(deparse(mmm_behavior_output_group_root), collapse = " ")
groups <- regmatches(source_text, gregexpr("[a-z0-9_]+(?= = c\\(current)", source_text,
                                           perl = TRUE))[[1L]]
if (length(groups) < 30L) stop("Could not read the output group list.", call. = FALSE)
for (group in groups) {
  current <- mmm_behavior_output_group_root(group, "current", project)
  if (!startsWith(paste0(current, "/"), paste0(file.path(ready, root_name), "/"))) next
  show(paste0("active root ", group), mmm_behavior_output_active_root(group, project))
}
if (identical(root_name, "03_derived_metrics")) {
  show("Stage 01 default output", mmm_derived_metrics_output_root(project))
}
show("guard old root (expect refusal)",
     mmm_behavior_guard_numbered_output_path(file.path(ready, root_name, "probe.csv"), project))
show("guard archive (expect refusal)",
     mmm_behavior_guard_numbered_output_path(
       file.path(ready, "history", "original_layout", root_name, "probe.csv"), project))
rows <- utils::read.csv(manifest, colClasses = "character")
if (!is.na(retained)) {
  paths <- file.path(retained, rows$relative_path)
  show("manifest files", nrow(rows))
  show("R can open at retained location", sum(file.exists(paths)))
  show("longest retained path (chars)", max(nchar(paths)))
}
