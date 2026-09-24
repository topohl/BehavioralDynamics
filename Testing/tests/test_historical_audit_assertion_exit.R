# Exercise the actual final assertion branches without running scientific audits.
suppressPackageStartupMessages(library(dplyr))
cases <- c(
  "audit_first_night_candidate_set_scores.R" = "areg",
  "audit_first_night_candidate_set_effects.R" = "areg",
  "audit_first_night_domain_scores_v2.R" = "leak",
  "audit_first_night_heatmap_v2.R" = "asrt",
  "audit_first_night_window_sensitivity.R" = "leak")

for (script in names(cases)) {
  parsed <- parse(file = file.path("Testing", "audits", script))
  branches <- Filter(function(x) is.call(x) &&
    identical(x[[1L]], as.name("if")) &&
    grepl('result == "FAIL"', paste(deparse(x[[2L]]), collapse = ""),
          fixed = TRUE), as.list(parsed))
  stopifnot(length(branches) == 1L)
  branch <- branches[[1L]]
  register <- cases[[script]]
  e <- new.env(parent = globalenv())
  e[[register]] <- data.frame(result = "PASS")
  stopifnot(!inherits(try(eval(branch, envir = e), silent = TRUE), "try-error"))
  e[[register]] <- data.frame(result = "FAIL")
  stopifnot(inherits(try(eval(branch, envir = e), silent = TRUE), "try-error"))
}
cat("Historical audit failed assertions return errors: PASS\n")
