# Static replay DAG only; no audit script is sourced or executed.
plan_file <- tempfile(fileext = ".csv")
warnings_file <- tempfile(fileext = ".txt")
status <- suppressWarnings(system2(
  "Rscript", "Maintenance/Get-BehaviorAuditReplayPlan.R",
  stdout = plan_file, stderr = warnings_file))
stopifnot(identical(status, 0L))
plan <- read.csv(plan_file, stringsAsFactors = FALSE, check.names = FALSE)
queue <- read.csv("docs/behavior_output_archive_audit_script_queue.csv",
                  stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(nrow(plan) == 37L,
          identical(sort(plan$script), sort(queue$script)),
          identical(plan$replay_order, seq_len(nrow(plan))),
          !anyDuplicated(plan$output_id[nzchar(plan$output_id)]),
          all(plan$review_state == "path_prepared_unvalidated"))
for (i in seq_len(nrow(plan))) {
  deps <- plan$prerequisite_scripts[[i]]
  if (!nzchar(deps)) next
  deps <- strsplit(deps, ";", fixed = TRUE)[[1L]]
  stopifnot(all(match(deps, plan$script) < i))
}
find <- function(script) plan[plan$script == paste0("Testing/audits/", script), ]
stopifnot(identical(find("audit_first_night_candidate_set_decision.R")$
                      prerequisite_output_ids,
                    "first_night_candidate_set_scores;first_night_candidate_set_effects"),
          identical(find("audit_first_night_domain_scores_v2.R")$
                      prerequisite_output_ids, "first_night_time_anchor"),
          identical(find("audit_hmm_state_architecture_construct_comparison.R")$
                      prerequisite_output_ids,
                    "hmm_architecture_components;hmm_architecture_redundancy"))

bad_queue <- tempfile(fileext = ".csv")
write.csv(rbind(queue, queue[1L, , drop = FALSE]), bad_queue,
          row.names = FALSE, na = "")
bad_output <- tempfile(fileext = ".csv")
bad_status <- suppressWarnings(system2(
  "Rscript", c("Maintenance/Get-BehaviorAuditReplayPlan.R", bad_queue),
  stdout = bad_output, stderr = warnings_file))
stopifnot(!identical(bad_status, 0L))
cat("Historical audit replay DAG: PASS\n")
