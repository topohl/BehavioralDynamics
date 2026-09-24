# Exercise Stage 10's actual candidate-path rewrite without running models.
source("Functions/project_paths.R")
stage10 <- as.list(parse(file = "Analysis/10_systems_feature_prediction_ladder.R"))
assignment <- function(name) {
  hit <- Filter(function(expr) is.call(expr) &&
                  identical(expr[[1L]], as.name("<-")) &&
                  identical(expr[[2L]], as.name(name)), stage10)
  stopifnot(length(hit) == 1L)
  hit[[1L]]
}
eval(assignment("route_activated_feature_sources"))
eval(assignment("stage10_scan_feature_paths"))

root <- file.path(tempdir(), paste0("stage10_candidate_routing_",
                                    as.integer(runif(1L, 1L, 1e9))))
ready <- file.path(root, "analysis_ready")
old <- file.path(mmm_behavior_output_group_root("dyadic_contacts", "current", root),
                 "tables", "feature.csv")
new <- file.path(mmm_behavior_output_group_root("dyadic_contacts", "semantic", root),
                 "tables", "feature.csv")
historical <- file.path(ready, "06_behavioral_dynamics", "temporal_instability",
                        "1min_based", "tables", "feature.csv")
for (path in c(old, historical)) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("AnimalNum,value\n1,2", path)
}
plan <- file.path(root, "migration_plan.csv")
utils::write.csv(data.frame(
  group = "dyadic_contacts",
  source_rel = "06_behavioral_dynamics/dyadic_contacts/tables/feature.csv",
  target_root_rel = "analyses/dyadic_contacts",
  target_file = "tables/feature.csv", gate = "ready"),
  plan, row.names = FALSE)
paths <- c(old, historical)
before <- route_activated_feature_sources(paths, root, plan)
stopifnot(identical(before, normalizePath(paths, winslash = "/")))

dir.create(dirname(new), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(old, new))
control <- file.path(ready, "_migration_control")
dir.create(control, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(
  group = "dyadic_contacts", state = "activated", files = 1L,
  target_root_rel = "analyses/dyadic_contacts",
  group_plan_sha256 = paste(rep("a", 64L), collapse = ""),
  contract_sha256 = paste(rep("b", 64L), collapse = ""),
  source_retained = TRUE),
  file.path(control, "dyadic_contacts.json"), auto_unbox = TRUE)
after <- route_activated_feature_sources(paths, root, plan)
stopifnot(identical(after,
                    normalizePath(c(new, historical), winslash = "/")),
          identical(basename(after), basename(before)))

# A source audit must identify exact paths. Equal basenames in different
# historical families cannot cause a false loaded label.
audit_expr <- assignment("feature_source_audit")
audit_env <- new.env(parent = globalenv())
audit_env$tibble <- tibble::tibble
audit_env$candidate_paths <- after
audit_env$candidate_feature_tables <- setNames(list(tibble::tibble(value = 1)),
                                               after[[1L]])
eval(audit_expr, envir = audit_env)
stopifnot(identical(audit_env$feature_source_audit$loaded_as_feature_table,
                    c(TRUE, FALSE)))

unlink(new)
missing_target <- tryCatch({
  route_activated_feature_sources(paths, root, plan)
  NULL
}, error = conditionMessage)
stopifnot(is.character(missing_target),
          grepl("missing or renamed destination", missing_target, fixed = TRUE))

# Historical resolution readers and Stage 10 share the same receipt switch.
history_group <- "history_temporal_instability_1min"
history_copy <- file.path(mmm_behavior_output_group_root(history_group, "semantic", root),
                          "tables", "feature.csv")
stopifnot(length(mmm_behavior_historical_resolution_groups) == 9L,
          !anyDuplicated(mmm_behavior_historical_resolution_groups))
for (group in unname(mmm_behavior_historical_resolution_groups)) {
  stopifnot(identical(mmm_behavior_output_active_root(group, root),
                      mmm_behavior_output_group_root(group, "current", root)))
}
stopifnot(identical(normalizePath(mmm_temporal_instability_resolution_root("1min_based", root),
                                  winslash = "/"), dirname(dirname(historical))),
          identical(mmm_behavior_route_historical_feature_sources(historical, root),
                    normalizePath(historical, winslash = "/")))
dir.create(dirname(history_copy), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(historical, history_copy))
stopifnot(inherits(try(mmm_temporal_instability_resolution_root("1min_based", root),
                       silent = TRUE), "try-error"))
jsonlite::write_json(list(
  group = history_group, state = "activated", files = 1L,
  target_root_rel = "history/temporal_instability/1min",
  group_plan_sha256 = paste(rep("c", 64L), collapse = ""),
  contract_sha256 = paste(rep("d", 64L), collapse = ""),
  source_retained = TRUE),
  file.path(control, paste0(history_group, ".json")), auto_unbox = TRUE)
stopifnot(identical(normalizePath(mmm_temporal_instability_resolution_root("1min_based", root),
                                  winslash = "/"),
                    normalizePath(dirname(dirname(history_copy)), winslash = "/")),
          identical(mmm_behavior_route_historical_feature_sources(historical, root),
                    normalizePath(history_copy, winslash = "/")),
          any(grepl("mmm_behavior_route_historical_feature_sources\\(candidate_paths, base_dir\\)",
                    readLines("Analysis/10_systems_feature_prediction_ladder.R"))))
stopifnot(file.copy(old, new))
scanned <- stage10_scan_feature_paths(c(history_group, "dyadic_contacts"), root,
                                       character())
stopifnot(identical(scanned, normalizePath(c(new, history_copy), winslash = "/")))
new_semantic_feature <- file.path(dirname(new), "new_feature.csv")
writeLines("AnimalNum,value\n1,3", new_semantic_feature)
stopifnot(identical(stage10_scan_feature_paths(
  c(history_group, "dyadic_contacts"), root, character()),
  normalizePath(c(new, new_semantic_feature, history_copy), winslash = "/")))
unlink(new_semantic_feature)
unlink(new)
writeLines("AnimalNum,value\n1,222", history_copy)
stopifnot(inherits(try(mmm_behavior_route_historical_feature_sources(historical, root),
                       silent = TRUE), "try-error"))
unlink(history_copy)
stopifnot(inherits(try(mmm_behavior_route_historical_feature_sources(historical, root),
                       silent = TRUE), "try-error"))

# Fresh migration manifests can appear in the numbered discovery root after
# the saved source audit. Exercise the production exclusion expressions so
# these metadata files never become candidate feature tables.
guard <- new.env(parent = globalenv())
guard$`%>%` <- magrittr::`%>%`
guard$str_replace_all <- stringr::str_replace_all
guard$candidate_paths <- file.path(ready, "06_behavioral_dynamics", "dyadic_contacts",
                                   "tables", c("input_output_manifest.csv",
                                               "output_figure_inventory.csv",
                                               "output_folder_summary.csv",
                                               "output_manifest.csv",
                                               "animal_features.csv"))
for (name in c("clean_name", "self_artifact_stubs", "is_self_artifact",
               "self_dir_markers", "in_self_dir", "excluded")) {
  eval(assignment(name), envir = guard)
}
stopifnot(identical(unname(guard$excluded), c(TRUE, TRUE, TRUE, TRUE, FALSE)))
cat("Stage 10 candidate-path routing and exact source audit: PASS\n")
