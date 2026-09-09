# Regression tests for figure mirroring.
#
# WHY THIS EXISTS
#
# harmonize_analysis_outputs() and mirror_plot_to_standard_folder() both copied
# a figure into a classified folder under
#
#     if (!file.exists(dst)) file.copy(src, dst, overwrite = FALSE, ...)
#
# That is correct exactly once. On every later run the authored figure is
# rewritten and the mirror is left untouched, so figures/x.svg and
# figures/publication_panels/x.svg drift apart with no warning. The mirror is
# not dead weight - manuscript/Fig1_behavior_candidates/build_fig1_candidates.R
# reads panels from figures/publication_panels/ - so a stale mirror means a
# manuscript builder can pick up a figure that no longer matches its data.
#
# These tests pin the corrected semantics. Portable: tempdir() only.

suppressPackageStartupMessages({ library(purrr) })

source("Analysis/_pipeline_setup.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

check(exists("mmm_refresh_mirror_copy", mode = "function"),
      "mmm_refresh_mirror_copy() is not available from the helpers")

root <- file.path(tempdir(), paste0("mirror_test_", Sys.getpid()))
dir.create(file.path(root, "figures", "publication_panels"),
           recursive = TRUE, showWarnings = FALSE)
src <- file.path(root, "figures", "panel.svg")
dst <- file.path(root, "figures", "publication_panels", "panel.svg")

# ---------------------------------------------------------------------------
cat("\n[1] the documented staleness scenario: A -> mirror -> B -> mirror\n")
# ---------------------------------------------------------------------------

writeLines("FIGURE-A", src)
check(isTRUE(mmm_refresh_mirror_copy(src, dst)), "first mirror was not written")
check(file.exists(dst), "mirror was not created")
check(identical(readLines(dst, warn = FALSE), "FIGURE-A"),
      "mirror does not equal source A")
ok("source A mirrored")

# Regenerate the authored figure with different content.
writeLines("FIGURE-B-REGENERATED", src)
check(isTRUE(mmm_refresh_mirror_copy(src, dst)),
      "second mirror pass reported no write; the stale-mirror bug is back")
check(identical(readLines(dst, warn = FALSE), "FIGURE-B-REGENERATED"),
      "MIRROR IS STALE: destination still holds the old figure after the source was regenerated")
check(identical(unname(tools::md5sum(src)), unname(tools::md5sum(dst))),
      "mirror is not byte-identical to the regenerated source")
ok("regenerated source B replaced the mirror byte-for-byte")

# ---------------------------------------------------------------------------
cat("\n[2] an already-current mirror is a no-op, not a rewrite\n")
# ---------------------------------------------------------------------------

check(isFALSE(mmm_refresh_mirror_copy(src, dst)),
      "an already byte-identical mirror was rewritten; copying is not idempotent")
check(identical(readLines(dst, warn = FALSE), "FIGURE-B-REGENERATED"),
      "no-op path corrupted the mirror")
ok("byte-identical mirror is skipped")

# ---------------------------------------------------------------------------
cat("\n[3] no temp artefacts are left behind\n")
# ---------------------------------------------------------------------------

leftovers <- list.files(dirname(dst), pattern = "mmmtmp", full.names = TRUE)
check(length(leftovers) == 0L,
      paste0("staging temp file(s) left behind: ", paste(leftovers, collapse = ", ")))
ok("no .mmmtmp staging files remain")

# ---------------------------------------------------------------------------
cat("\n[4] a missing source is a no-op, not an error and not a deletion\n")
# ---------------------------------------------------------------------------

absent <- file.path(root, "figures", "never_written.svg")
check(isFALSE(mmm_refresh_mirror_copy(absent, dst)),
      "a missing source should be a silent no-op")
check(file.exists(dst) && identical(readLines(dst, warn = FALSE), "FIGURE-B-REGENERATED"),
      "a missing source must not disturb an existing mirror")
ok("missing source leaves the mirror intact")

# ---------------------------------------------------------------------------
cat("\n[5] neither mirror call site reuses the copy-if-absent pattern\n")
# ---------------------------------------------------------------------------

helpers <- readLines("Functions/behavioral_dynamics_helpers.R", warn = FALSE)
code <- helpers[!grepl("^\\s*#", helpers)]

# The exact defect: a file.exists(dst) guard wrapping a mirror copy.
bad <- grep("if \\(!file\\.exists\\(dst\\)\\)", code)
check(length(bad) == 0L,
      paste0("a copy-if-destination-absent guard is back at line(s) ",
             paste(bad, collapse = ", "),
             " - that is exactly what makes mirrors go stale"))

for (fn in c("harmonize_analysis_outputs", "mirror_plot_to_standard_folder")) {
  i <- grep(paste0("^", fn, " <- function"), helpers)
  check(length(i) == 1L, paste0("cannot locate ", fn, "()"))
  close_rel <- which(helpers[i:length(helpers)] == "}")[1]
  body <- helpers[i:(i + close_rel - 1L)]
  check(any(grepl("mmm_refresh_mirror_copy", body, fixed = TRUE)),
        paste0(fn, "() no longer routes its copy through mmm_refresh_mirror_copy()"))
  check(!any(grepl("overwrite = FALSE", body, fixed = TRUE)),
        paste0(fn, "() still contains an overwrite = FALSE copy"))
}
ok("both mirror sites route through the verified refresh helper")

unlink(root, recursive = TRUE)
cat("\nFigure mirror refresh checks: PASS\n")
