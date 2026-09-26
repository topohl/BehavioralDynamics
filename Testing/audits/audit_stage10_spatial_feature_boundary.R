# Read-only, data-dependent check for the Stage 10 / Stage 19 path boundary.
# Run from the repository root. This does not execute either scientific stage.
source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")

root <- mmm_project_root()
audit_path <- file.path(root, "analysis_ready", "pipeline",
                        "10_systems_prediction", "10min", "tables",
                        "feature_source_audit.csv")
if (!file.exists(audit_path)) stop("Missing recorded Stage 10 feature-source audit: ", audit_path)
audit <- read.csv(audit_path, stringsAsFactors = FALSE)
stopifnot(all(c("source_file", "loaded_as_feature_table") %in% names(audit)))
paths <- gsub("\\\\", "/", audit$source_file)
loaded <- as.logical(audit$loaded_as_feature_table)
if (anyNA(loaded)) stop("Stage 10 feature-source audit has missing load decisions")

spatial_models <- grepl("/analysis_ready/04_model_outputs/spatial_occupancy/", paths,
                        fixed = TRUE)
spatial_figures <- grepl("/analysis_ready/05_figures/spatial_occupancy/", paths,
                         fixed = TRUE)
if (sum(spatial_models) != 4L || any(loaded[spatial_models | spatial_figures])) {
  stop("Recorded Stage 10 run does not support excluding Stage 19 model/figure trees")
}
# The recorded paths are provenance of the 2026-09-22 run. A numbered root may
# since have been archived, so check each file at its retained location.
retained_path <- function(path) {
  for (numbered in MMM_NUMBERED_BEHAVIOR_ROOTS) {
    old <- paste0(normalizePath(file.path(root, "analysis_ready", numbered),
                                winslash = "/", mustWork = FALSE), "/")
    if (startsWith(path, old)) {
      return(file.path(mmm_behavior_numbered_source_root(numbered, root),
                       substring(path, nchar(old) + 1L)))
    }
  }
  path
}
present <- vapply(paths[loaded], function(path) file.exists(retained_path(path)),
                  logical(1))
if (sum(loaded) != 176L || !all(present)) {
  stop("Recorded Stage 10 loaded-feature source set changed or is no longer present")
}

src10 <- readLines("Analysis/10_systems_feature_prediction_ladder.R", warn = FALSE)
start <- grep("^feature_search_dirs <- c\\($", src10)
if (length(start) != 1L) stop("Cannot locate Stage 10 feature search declaration")
ends <- grep("^\\)$", src10)
end <- ends[ends > start][1]
if (is.na(end)) stop("Cannot locate end of Stage 10 feature search declaration")
search_block <- paste(src10[start:end], collapse = "\n")
if (grepl("04_model_outputs|05_figures", search_block)) {
  stop("Stage 10 still scans Stage 19 model or figure trees")
}

src19 <- paste(readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE),
               collapse = "\n")
audit_files <- c("raw_position_metadata_assignment_qc.csv",
                 "canonical_identity_conflicts.csv",
                 "canonical_identity_alias_merge.csv",
                 "canonical_identity_summary.csv",
                 "phase_boundary_and_gap_rule_audit.csv",
                 "group_sex_label_contract.csv",
                 "spatial_occupancy_run_log.csv", "session_info.txt")
for (name in audit_files) {
  line <- grep(name, strsplit(src19, "\n", fixed = TRUE)[[1]], fixed = TRUE,
               value = TRUE)
  if (length(line) != 1L || !grepl("DIR_AUDIT", line, fixed = TRUE)) {
    stop("Stage 19 audit artifact is not routed through DIR_AUDIT: ", name)
  }
}

cat("PASS: four recorded spatial model candidates were rejected; 176 loaded ",
    "sources remain present; Stage 10 excludes spatial result trees; Stage 19 ",
    "audit paths are explicit\n", sep = "")
