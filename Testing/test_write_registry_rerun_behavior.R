# Investigates whether the ".mmm_output_write_registry rerun bug" is
# reproducible under the ACTUAL pipeline entrypoint. Every Analysis/*.R stage
# script begins with `source(.pipeline_setup)`, i.e. Analysis/_pipeline_setup.R,
# which unconditionally re-sources Functions/behavioral_dynamics_helpers.R via
# source_mmm_helper(). That helper file recreates
# `.mmm_output_write_registry <- new.env(parent = emptyenv())` at its own top
# level every time it is sourced.
#
# This test proves, rather than assumes, what that means for a same-session
# rerun of a stage script, and fails loudly (via stop()) if the bug turns out
# to be reproducible, so it cannot be silently ignored.

suppressPackageStartupMessages({
  library(tibble)
  library(readr)
})

# ------------------------------------------------------------------
# Part 1: the actual pipeline entrypoint pattern -- re-source
# Analysis/_pipeline_setup.R exactly as a rerun stage script would.
# ------------------------------------------------------------------
source("Analysis/_pipeline_setup.R")
registry_env_run1 <- .mmm_output_write_registry

tmp <- tempfile(fileext = ".csv")
x_tbl <- tibble(x = 1)
write_table(x_tbl, tmp)
content_run1 <- read_csv(tmp, show_col_types = FALSE)

# Simulate rerunning the stage script in the same R session: its first action
# is always re-sourcing _pipeline_setup.R.
source("Analysis/_pipeline_setup.R")
registry_env_run2 <- .mmm_output_write_registry

registry_env_recreated <- !identical(registry_env_run1, registry_env_run2)

# Same variable name as before but genuinely different content, as would
# happen if upstream data changed between runs.
x_tbl <- tibble(x = 2)
write_table(x_tbl, tmp)
content_run2 <- read_csv(tmp, show_col_types = FALSE)

rerun_bug_reproducible <- identical(content_run1$x, content_run2$x)

cat("== Part 1: normal top-level script rerun (re-sources _pipeline_setup.R) ==\n")
cat("Registry environment recreated on re-source:", registry_env_recreated, "\n")
cat("Second run's write reflected the new content:", !rerun_bug_reproducible, "\n")

unlink(tmp)

if (!registry_env_recreated || rerun_bug_reproducible) {
  stop(
    "REPRODUCIBLE: the write-registry rerun bug occurs under the normal pipeline ",
    "entrypoint (re-sourcing Analysis/_pipeline_setup.R). Do not leave registry ",
    "lifecycle semantics alone; investigate before relying on this result.",
    call. = FALSE
  )
}

cat(
  "NOT REPRODUCIBLE under the normal pipeline entrypoint: re-sourcing ",
  "Analysis/_pipeline_setup.R (as every Analysis/*.R script does at startup) ",
  "recreates .mmm_output_write_registry as a fresh empty environment via ",
  "Functions/behavioral_dynamics_helpers.R, so a genuine top-level rerun never ",
  "lets a stale registry block a fresh write.\n",
  sep = ""
)

# ------------------------------------------------------------------
# Part 2 (contrast, documents intended behavior): WITHOUT re-sourcing between
# writes -- e.g. pasting/re-running code fragments in one interactive session
# without restarting from the script's top -- a second write of a
# same-named variable to the same path IS intentionally skipped. This is the
# designed same-session duplicate-write guard, not a bug, and confirms the
# mechanism still works as documented.
# ------------------------------------------------------------------
tmp2 <- tempfile(fileext = ".csv")
a <- tibble(y = 1)
write_table(a, tmp2)
a <- tibble(y = 2)
write_table(a, tmp2)
content2 <- read_csv(tmp2, show_col_types = FALSE)
same_session_skip_observed <- identical(as.numeric(content2$y), 1)

cat("\n== Part 2: same-session duplicate write with no re-source in between ==\n")
cat("Second same-expression write to the same path was skipped (by design):", same_session_skip_observed, "\n")
unlink(tmp2)

if (!same_session_skip_observed) {
  stop("The documented same-session duplicate-write guard did not behave as expected; investigate write_table().", call. = FALSE)
}

cat("\nConclusion: the write-registry rerun bug is NOT reproducible under the normal\n")
cat("pipeline entrypoint. Registry lifecycle semantics are left unchanged; the\n")
cat("actual Stage 09 duplicate-write problem (same content written under two\n")
cat("candidate paths within a single run) was fixed at the call sites instead.\n")
