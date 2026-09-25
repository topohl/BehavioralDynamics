# The cookie-habituation runner must restore its dataset options and working
# directory, so later Exp9 stages in the same session read Exp9 inputs again.
#
# The runner is never executed. Its top level is checked by parsing, and only
# its local() block is evaluated, with source() replaced by a stub that loads
# nothing and MMM_REPO_ROOT pointing at an empty temporary directory. (On
# 2026-09-25 an earlier version of this test ran the runner itself and it
# reached the live cookiehab tree through its getwd() fallback.)
runner <- "Analysis/run_cookiehab_preprocessing_and_metrics.R"
exprs <- as.list(parse(runner, keep.source = FALSE))
head_name <- function(e) if (is.call(e) && is.name(e[[1L]])) as.character(e[[1L]]) else ""
calls_in <- function(e) {
  if (!is.call(e)) return(character())
  c(head_name(e), unlist(lapply(as.list(e)[-1L], calls_in)))
}
is_local <- vapply(exprs, function(e) identical(head_name(e), "local"), logical(1))
stopifnot(sum(is_local) == 1L)
# A top-level on.exit() in a source()d file fires immediately, so options,
# setwd and on.exit must all sit inside the one local() block.
stopifnot(!any(c("options", "setwd", "on.exit") %in%
                 unlist(lapply(exprs[!is_local], calls_in))),
          all(c("options", "setwd", "on.exit", "source") %in%
                calls_in(exprs[[which(is_local)]])))

root <- normalizePath(file.path(tempdir(), paste0("cookiehab_runner_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
dir.create(root)
run_block <- function(fail_at = 0L) {
  env <- new.env(parent = globalenv())
  env$MMM_REPO_ROOT <- root
  env$analysis_dir <- file.path(root, "Analysis")
  seen <- list()
  env$source <- function(file, ...) {
    seen[[length(seen) + 1L]] <<- list(
      file = basename(file), dataset = getOption("mmm.dataset_id"),
      metrics = getOption("mmm.derived_metrics_dir"),
      wd = normalizePath(getwd(), winslash = "/"))
    if (length(seen) == fail_at) stop("stub stage failed", call. = FALSE)
    invisible(NULL)
  }
  completed <- tryCatch({ eval(exprs[[which(is_local)]], env); TRUE },
                        error = function(e) FALSE)
  list(completed = completed, seen = seen)
}

option_names <- c("mmm.dataset_id", "mmm.preprocessed_dir", "mmm.cookiehab_preprocessed_dir",
                  "mmm.derived_metrics_dir", "mmm.dyadic_contacts_dir")
options(mmm.dataset_id = "exp9")
before_options <- options()[option_names]
before_wd <- getwd()

ran <- run_block()
stopifnot(ran$completed,
          identical(vapply(ran$seen, `[[`, "", "file"),
                    c("01_preprocess_cookiehab_animalpos.R",
                      "01_build_multiscale_behavior_metrics.R",
                      "02_build_dyadic_rfid_contacts.R")),
          all(vapply(ran$seen, function(x) identical(x$dataset, "cookiehab"), logical(1))),
          all(vapply(ran$seen, function(x) grepl("/cookiehab/analysis_ready/", x$metrics,
                                                 fixed = TRUE), logical(1))),
          all(vapply(ran$seen, function(x) identical(x$wd, root), logical(1))),
          identical(options()[option_names], before_options),
          identical(getwd(), before_wd))

# A failing stage still restores the options and the working directory.
failed <- run_block(fail_at = 2L)
stopifnot(!failed$completed, length(failed$seen) == 2L,
          identical(options()[option_names], before_options),
          identical(getwd(), before_wd))
options(mmm.dataset_id = NULL)
cat("Cookiehab runner restores options and working directory: PASS\n")
