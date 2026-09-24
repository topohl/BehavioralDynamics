# Exercise Stage 10's actual feature-root expression without running its models.
source("Functions/project_paths.R")
lines <- readLines("Analysis/10_systems_feature_prediction_ladder.R", warn = FALSE)
start <- grep("^feature_search_dirs <- c\\($", lines)
stopifnot(length(start) == 1L)
end <- start + which(lines[start:length(lines)] == ")")[1] - 1L
stopifnot(is.finite(end))
expression <- parse(text = paste(lines[start:end], collapse = "\n"))
group_source <- readLines("Analysis/10_systems_feature_prediction_ladder.R", warn = FALSE)
group_start <- grep("^feature_search_groups <- c\\($", group_source)
stopifnot(length(group_start) == 1L)
group_end <- group_start + which(group_source[group_start:length(group_source)] == ")")[1L] - 1L
stopifnot(is.finite(group_end))
group_expression <- parse(text = paste(group_source[group_start:group_end], collapse = "\n"))

base_dir <- file.path(tempdir(), "supporting_scan_contract")
ready <- file.path(base_dir, "analysis_ready")
groups <- c(nonlinear_dynamics_5min = "nonlinear_dynamics",
            systems_phenotyping_5min = "systems_phenotyping")
for (group in names(groups)) {
  old <- mmm_behavior_output_group_root(group, "current", base_dir)
  dir.create(file.path(old, "tables"), recursive = TRUE, showWarnings = FALSE)
  writeLines("AnimalNum,value\n1,1", file.path(old, "tables", "feature.csv"))
}
eval(group_expression)
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

# The Stage 09 model input is required from its canonical pipeline location.
# Exercise Stage 10's actual resolver call with a legacy-only decoy.
source("Analysis/_pipeline_setup.R")
resolver_start <- grep("^input_08b_resolution <- resolve_behavior_artifact\\($", lines)
stopifnot(length(resolver_start) == 1L)
resolver_end <- resolver_start + which(lines[resolver_start:length(lines)] == ")")[1L] - 1L
resolver_expr <- parse(text = paste(lines[resolver_start:resolver_end], collapse = "\n"))
input_root <- file.path(tempdir(), paste0("stage10_canonical_input_contract_",
                                         as.integer(runif(1L, 1L, 1e9))))
canonical <- file.path(behavior_stage_tables(input_root, "09", "early_prediction",
                                             "10min_based"), "model_ladder_input.csv")
legacy <- file.path(input_root, "analysis_ready", "06_behavioral_dynamics",
                    "early_prediction_model_ladder", "10min_based", "tables",
                    "model_ladder_input.csv")
dir.create(dirname(legacy), recursive = TRUE, showWarnings = FALSE)
writeLines("legacy decoy", legacy)
resolver_env <- new.env(parent = globalenv())
resolver_env$input_08b <- canonical
missing_canonical <- tryCatch({
  eval(resolver_expr, envir = resolver_env)
  NULL
}, error = conditionMessage)
stopifnot(is.character(missing_canonical),
          grepl("Missing required behavioral artifact", missing_canonical,
                fixed = TRUE))
dir.create(dirname(canonical), recursive = TRUE, showWarnings = FALSE)
writeLines("canonical input", canonical)
eval(resolver_expr, envir = resolver_env)
stopifnot(identical(resolver_env$input_08b_resolution$path, canonical),
          identical(resolver_env$input_08b_resolution$resolution, "canonical"))
cat("PASS: Stage 10 requires the canonical Stage 09 model input\n")
