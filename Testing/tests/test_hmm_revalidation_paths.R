env <- new.env(parent = baseenv())
source("Functions/hmm_revalidation_paths.R", local = env)
project <- file.path(tempdir(), "hmm_revalidation_path_fixture")
run_dir <- file.path(project, "analysis_ready", "analyses",
                     "hmm_revalidation_runs", "current_stage08_review")
expected <- "step6_longitudinal_gapaware_contrasts.csv"
companion <- "hmm_cross_optimum_gapaware_claim_verdicts.csv"
resolved <- env$mmm_hmm_revalidation_output_dir(
  project, run_dir, expected, allowed_existing = companion)
stopifnot(identical(resolved,
                    normalizePath(run_dir, winslash = "/", mustWork = FALSE)))
dir.create(run_dir, recursive = TRUE)
writeLines("companion", file.path(run_dir, companion))
stopifnot(identical(env$mmm_hmm_revalidation_output_dir(
  project, run_dir, expected, allowed_existing = companion), resolved))
writeLines("existing scientific result", file.path(run_dir, expected))
stopifnot(inherits(try(env$mmm_hmm_revalidation_output_dir(
  project, run_dir, expected, allowed_existing = companion), silent = TRUE),
  "try-error"))
unlink(file.path(run_dir, expected))
writeLines("unexpected", file.path(run_dir, "other.csv"))
stopifnot(inherits(try(env$mmm_hmm_revalidation_output_dir(
  project, run_dir, expected, allowed_existing = companion), silent = TRUE),
  "try-error"))
old_dir <- file.path(project, "analysis_ready", "12_systems_neuroscience_summary",
                     "5min_based", "audit_hmm_state_architecture")
stopifnot(inherits(try(env$mmm_hmm_revalidation_output_dir(
  project, old_dir, expected), silent = TRUE), "try-error"),
  inherits(try(env$mmm_hmm_revalidation_output_dir(
    project, file.path(tempdir(), "outside"), expected), silent = TRUE),
    "try-error"))
for (script in c("Testing/audits/audit_step6_longitudinal_gapaware_robustness.R",
                 "Testing/audits/audit_hmm_cross_optimum_gapaware.R")) {
  code <- paste(readLines(script, warn = FALSE), collapse = "\n")
  stopifnot(grepl("mmm_hmm_revalidation_output_dir", code, fixed = TRUE),
            grepl("mmm_derived_metrics_output_root(PROJ)", code, fixed = TRUE),
            !grepl("OUT <- file.path(PROJ, \"analysis_ready/12_systems_neuroscience_summary",
                   code, fixed = TRUE))
}
cat("HMM revalidation path and overwrite guards: PASS\n")
