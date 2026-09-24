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
cat("Stage 10 candidate-path routing and exact source audit: PASS\n")
