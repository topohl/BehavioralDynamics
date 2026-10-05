# The RFID data root is configured in one place.
#
#   1. Functions/project_paths.R holds the documented default exactly once, as MMM_PROJECT_ROOT_DEFAULT, and no
#      user-home or UNC path;
#   2. no other helper in Functions/ repeats the root;
#   3. every Analysis/ script takes the root from mmm_project_root(); the listed exceptions keep the literal.
# Lifted from the retired Stage 27 contract test on 2026-10-05 so the check outlives it.
#
# Portable: reads repository files only.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

RFID_ROOT_LITERAL <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
code_lines <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }

cat("1. Functions/project_paths.R\n")
pp <- readLines("Functions/project_paths.R", warn = FALSE)
root_lines <- grep("S:/Lab_Member", pp, fixed = TRUE)
root_code_lines <- root_lines[!grepl("^\\s*#", pp[root_lines])]
check(length(root_code_lines) == 1L,
      paste0("the data-root default must appear exactly once in executable code in Functions/project_paths.R; found ",
             length(root_code_lines), " at line(s) ", paste(root_code_lines, collapse = ", ")))
check(grepl("MMM_PROJECT_ROOT_DEFAULT", pp[root_code_lines - 1L]) || grepl("MMM_PROJECT_ROOT_DEFAULT", pp[root_code_lines]),
      "the single data-root literal must be the MMM_PROJECT_ROOT_DEFAULT constant")
for (pat in c("C:/Users", "C:\\\\Users", "c:/Users", "/c/Users", "//mdc-berlin", "\\\\\\\\mdc-berlin")) {
  check(!any(grepl(pat, pp, fixed = TRUE)), paste0("Functions/project_paths.R contains a maintainer-specific path pattern '", pat, "'"))
}
ok("one documented default, no user-home or UNC path")

cat("\n2. other helpers\n")
helpers <- setdiff(list.files("Functions", pattern = "[.][Rr]$", full.names = TRUE), "Functions/project_paths.R")
for (f in helpers) {
  hit <- grep(RFID_ROOT_LITERAL, code_lines(f), fixed = TRUE)
  check(!length(hit), paste0(f, " repeats the RFID data root; call mmm_project_root() instead"))
}
ok(sprintf("%d helpers do not repeat the data root", length(helpers)))

cat("\n3. stage scripts\n")
# Stage 09 is treated as registered and stays unedited; the cookie runner names its own dataset tree; the Stage 09
# LOAO re-renderer reads the environment variable with the literal as its default.
ROOT_LITERAL_EXCEPTIONS <- c("Analysis/09_early_prediction_model_ladder.R",
                             "Analysis/run_cookiehab_preprocessing_and_metrics.R",
                             "Analysis/_supporting/render_stage09_loao_plot.R")
stage_scripts <- c(list.files("Analysis", pattern = "[.][Rr]$", full.names = TRUE),
                   list.files("Analysis/_supporting", pattern = "[.][Rr]$", full.names = TRUE))
check(all(file.exists(ROOT_LITERAL_EXCEPTIONS)), "every root-literal exception names an existing script")
for (f in setdiff(stage_scripts, ROOT_LITERAL_EXCEPTIONS)) {
  hit <- grep(RFID_ROOT_LITERAL, readLines(f, warn = FALSE), fixed = TRUE)
  check(!length(hit), paste0(f, " hard-codes the RFID data root at line(s) ", paste(hit, collapse = ", "),
                             "; it must call mmm_project_root() instead"))
}
ok(sprintf("%d stage scripts take the data root from mmm_project_root() (%d listed exceptions)",
           length(setdiff(stage_scripts, ROOT_LITERAL_EXCEPTIONS)), length(ROOT_LITERAL_EXCEPTIONS)))

cat("\nPASS: data root literals\n")
