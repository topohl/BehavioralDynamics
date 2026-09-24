# Read-only comparison of retained originals with receipt-selected semantic
# output groups. This does not execute Stage 10 models or write S:.
# Run from the repository root with the RFID project root as the sole argument.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[[1L]])) {
  stop("Usage: Rscript Testing/audits/audit_stage10_semantic_discovery_parity.R <RFID-project-root>")
}
base_dir <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")

code <- as.list(parse(file = "Analysis/10_systems_feature_prediction_ladder.R"))
assignment <- function(name) {
  hit <- Filter(function(expr) is.call(expr) &&
                  identical(expr[[1L]], as.name("<-")) &&
                  identical(expr[[2L]], as.name(name)), code)
  if (length(hit) != 1L) stop("Missing or duplicate Stage 10 assignment: ", name)
  hit[[1L]]
}
for (name in c("stage10_scan_feature_paths", "clean_name")) eval(assignment(name))
bin_level <- "10min_based"
eval(assignment("feature_search_groups"))
eval(assignment("feature_search_dirs"))
stopifnot(length(feature_search_groups) == 18L,
          length(feature_search_dirs) == 25L)

scan <- function(dirs) {
  paths <- unique(unlist(lapply(dirs[dir.exists(dirs)], list.files,
                                pattern = "\\.(csv|tsv|xlsx|xls)$",
                                recursive = TRUE, full.names = TRUE),
                         use.names = FALSE))
  normalizePath(paths, winslash = "/", mustWork = TRUE)
}
ready <- file.path(base_dir, "analysis_ready")
numbered_root <- mmm_behavior_numbered_source_root("06_behavioral_dynamics", base_dir)
other_dirs <- feature_search_dirs[-seq_along(feature_search_groups)]
candidate_paths <- scan(c(numbered_root, other_dirs))
for (name in c("self_artifact_stubs", "is_self_artifact", "self_dir_markers",
               "in_self_dir", "excluded")) eval(assignment(name))
numbered_filtered <- candidate_paths[!excluded]

columns <- c("group", "source_rel", "target_root_rel", "target_file")
plans <- rbind(
  utils::read.csv("docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv",
                  check.names = FALSE)[, columns],
  utils::read.csv("docs/BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv",
                  check.names = FALSE)[, columns])
plans <- plans[startsWith(plans$source_rel, "06_behavioral_dynamics/"), , drop = FALSE]
source_prefix <- paste0(normalizePath(numbered_root, winslash = "/", mustWork = TRUE), "/")
old_map_rel <- "06_behavioral_dynamics/proteomics_integration_output_dir_map.csv"
from_original <- startsWith(numbered_filtered, source_prefix)
filtered_rel <- paste0("06_behavioral_dynamics/",
                       substring(numbered_filtered[from_original],
                                 nchar(source_prefix) + 1L))
mapped_index <- match(filtered_rel, plans$source_rel)
if (any(is.na(mapped_index) & filtered_rel != old_map_rel)) {
  stop("Unmapped retained Stage 10 candidate source after archive routing.")
}
current <- numbered_filtered
mapped <- which(from_original)[!is.na(mapped_index)]
current[mapped] <- file.path(ready,
                            plans$target_root_rel[mapped_index[!is.na(mapped_index)]],
                            plans$target_file[mapped_index[!is.na(mapped_index)]])
current <- normalizePath(current, winslash = "/", mustWork = TRUE)
numbered_all <- list.files(numbered_root, all.files = TRUE, recursive = TRUE,
                           full.names = TRUE, include.dirs = FALSE)
numbered_all <- numbered_all[file.info(numbered_all)$isdir %in% FALSE &
                               basename(numbered_all) != "Thumbs.db"]
numbered_rel <- paste0("06_behavioral_dynamics/",
                       substring(normalizePath(numbered_all, winslash = "/"),
                                 nchar(normalizePath(numbered_root, winslash = "/")) + 2L))
stopifnot(nrow(plans) == 1459L, length(numbered_rel) == 1460L,
          !anyDuplicated(plans$source_rel),
          setequal(numbered_rel, c(plans$source_rel, old_map_rel)))

target <- normalizePath(file.path(ready, plans$target_root_rel, plans$target_file),
                        winslash = "/", mustWork = TRUE)
stopifnot(!anyDuplicated(target))
roots <- unique(file.path(ready, plans$target_root_rel))
for (root in roots) {
  actual <- list.files(root, all.files = TRUE, recursive = TRUE,
                       full.names = TRUE, include.dirs = FALSE)
  actual <- normalizePath(actual[file.info(actual)$isdir %in% FALSE &
                                   basename(actual) != "Thumbs.db"],
                          winslash = "/", mustWork = TRUE)
  expected <- target[startsWith(target, paste0(normalizePath(root, winslash = "/"), "/"))]
  stopifnot(setequal(actual, expected))
}

proposed <- stage10_scan_feature_paths(feature_search_groups, base_dir,
                                       other_dirs)
semantic_self <- clean_name(tools::file_path_sans_ext(basename(proposed))) %in%
  self_artifact_stubs
semantic_self_dir <- vapply(proposed, function(path)
  any(vapply(self_dir_markers, function(marker)
    grepl(marker, path, fixed = TRUE), logical(1))), logical(1))
proposed <- proposed[!(semantic_self | semantic_self_dir)]

old_map <- normalizePath(file.path(numbered_root,
                                   "proteomics_integration_output_dir_map.csv"),
                         winslash = "/", mustWork = TRUE)
recorded_old_map <- normalizePath(file.path(ready, old_map_rel),
                                  winslash = "/", mustWork = FALSE)
audit <- utils::read.csv(file.path(ready, "pipeline", "10_systems_prediction",
                                   "10min", "tables", "feature_source_audit.csv"))
record <- audit[audit$source_file == recorded_old_map, , drop = FALSE]
header <- names(utils::read.csv(old_map, nrows = 0L, check.names = FALSE))
stopifnot(nrow(record) == 1L, isFALSE(record$loaded_as_feature_table[[1L]]),
          !"AnimalNum" %in% header, sum(current == old_map) == 1L,
          length(current) == 607L, length(proposed) == 606L,
          identical(current[current != old_map], proposed),
          !anyDuplicated(proposed), all(file.exists(proposed)))

cat("PASS: 1,459 numbered files mapped to 18 exact semantic groups; ",
    "one old non-feature metadata map remains.\n", sep = "")
cat("PASS: Stage 10's 606 remaining filtered candidate paths have identical order and paths ",
    "under semantic discovery; scientific models were not run.\n", sep = "")
