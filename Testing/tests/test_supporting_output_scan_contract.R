# Exercise Stage 10's actual feature-root expression without running its models.
source("Functions/project_paths.R")
lines <- readLines("Analysis/10_systems_feature_prediction_ladder.R", warn = FALSE)
start <- grep("^feature_search_dirs <- c\\($", lines)
stopifnot(length(start) == 1L)
end <- start + which(lines[start:length(lines)] == ")")[1] - 1L
stopifnot(is.finite(end))
expression <- parse(text = paste(lines[start:end], collapse = "\n"))

base_dir <- file.path(tempdir(), "supporting_scan_contract")
ready <- file.path(base_dir, "analysis_ready")
groups <- c(nonlinear_dynamics_5min = "nonlinear_dynamics",
            systems_phenotyping_5min = "systems_phenotyping")
for (group in names(groups)) {
  old <- mmm_behavior_output_group_root(group, "current", base_dir)
  dir.create(file.path(old, "tables"), recursive = TRUE, showWarnings = FALSE)
  writeLines("AnimalNum,value\n1,1", file.path(old, "tables", "feature.csv"))
}
eval(expression)
for (group in names(groups)) {
  old <- mmm_behavior_output_group_root(group, "current", base_dir)
  stopifnot(old %in% feature_search_dirs)
}

dir.create(file.path(ready, "_migration_control"), recursive = TRUE,
           showWarnings = FALSE)
for (group in names(groups)) {
  old <- mmm_behavior_output_group_root(group, "current", base_dir)
  new <- mmm_behavior_output_group_root(group, "semantic", base_dir)
  dir.create(file.path(new, "tables"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(old, "tables", "feature.csv"),
            file.path(new, "tables", "feature.csv"))
  target <- sub(paste0("^", normalizePath(ready, winslash = "/"), "/"),
                "", normalizePath(new, winslash = "/"))
  record <- list(group = group, state = "activated", files = 1L,
                 group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                 contract_sha256 = paste(rep("b", 64), collapse = ""),
                 target_root_rel = target, source_retained = TRUE)
  jsonlite::write_json(record,
                       file.path(ready, "_migration_control", paste0(group, ".json")),
                       auto_unbox = TRUE)
}
eval(expression)
for (group in names(groups)) {
  old <- mmm_behavior_output_group_root(group, "current", base_dir)
  new <- mmm_behavior_output_group_root(group, "semantic", base_dir)
  stopifnot(new %in% feature_search_dirs, !old %in% feature_search_dirs,
            sum(feature_search_dirs == new) == 1L)
  candidates <- unlist(lapply(feature_search_dirs[dir.exists(feature_search_dirs)],
                              list.files, pattern = "\\.(csv|tsv|xlsx|xls)$",
                              recursive = TRUE, full.names = TRUE),
                       use.names = FALSE)
  stopifnot(sum(startsWith(candidates, new)) == 1L,
            sum(startsWith(candidates, old)) == 0L)
}
cat("PASS: Stage 10 selects one supporting feature root per activated group\n")
