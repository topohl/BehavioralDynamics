# The preflight may inspect paths but must not create any replay output.
root <- file.path(tempdir(), paste0("audit_preflight_",
                                   as.integer(runif(1L, 1L, 1e9))))
ready <- file.path(root, "analysis_ready")
for (name in c("03_derived_metrics", "06_behavioral_dynamics",
               "12_systems_neuroscience_summary")) {
  dir.create(file.path(ready, name), recursive = TRUE, showWarnings = FALSE)
}
baseline <- file.path(root, "baseline")
dir.create(file.path(baseline, "analysis_ready"), recursive = TRUE)
script <- "Maintenance/Preflight-BehaviorAuditReplay.R"
invoke <- function(id, extra = character()) {
  stdout <- tempfile(fileext = ".txt")
  stderr <- tempfile(fileext = ".txt")
  status <- suppressWarnings(system2(
    "Rscript", c(script, shQuote(root), id, extra),
    stdout = stdout, stderr = stderr))
  result <- list(status = status, output = readLines(stdout, warn = FALSE))
  unlink(c(stdout, stderr))
  result
}

blocked <- invoke("first_review")
stopifnot(!identical(blocked$status, 0L),
          any(grepl("queued_scripts: 37", blocked$output, fixed = TRUE)),
          any(grepl("mechanically_ready: FALSE", blocked$output, fixed = TRUE)))
complete <- invoke("second_review", c(shQuote(baseline),
                                      "unknown_historical_state",
                                      "fixture_baseline_provenance"))
stopifnot(identical(complete$status, 0L),
          any(grepl("mechanically_ready: TRUE", complete$output, fixed = TRUE)))
same_root <- invoke("third_review", c(shQuote(root),
                                     "unknown_historical_state",
                                     "not_independent"))
stopifnot(any(grepl("mechanically_ready: FALSE", same_root$output,
                    fixed = TRUE)))
stopifnot(!dir.exists(file.path(ready, "analyses", "historical_audit_replays")))

dir.create(file.path(ready, "analyses", "historical_audit_replays",
                     "occupied_review"), recursive = TRUE)
occupied <- invoke("occupied_review")
stopifnot(!identical(occupied$status, 0L))
unsafe <- invoke("../escape")
stopifnot(!identical(unsafe$status, 0L))
cat("Historical audit replay preflight: PASS\n")
