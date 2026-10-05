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
# figures/publication_panels/x.svg drift apart with no warning. A builder that
# reads panels from figures/publication_panels/ (the Figure 1 candidate builder
# did until its retirement on 2026-10-05) would then pick up a figure that no
# longer matches its data.
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

# The Stage 14 output root contains "systems". Its name must not turn every
# publication figure into QC, and already categorized figures must stay put.
cat("\n[6] figure roles follow the figures/ subtree, not its ancestor\n")
stage14_like <- file.path(root, "12_systems_neuroscience_summary", "5min_based")
pub <- file.path(stage14_like, "figures", "publication_panels", "Fig_systems_effect_size_heatmap.svg")
qc <- file.path(stage14_like, "figures", "qc", "Fig_chip_loss_diagnostics.svg")
plain <- file.path(stage14_like, "figures", "Fig_behavior_overview.svg")
dir.create(dirname(pub), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(qc), recursive = TRUE, showWarnings = FALSE)
writeLines("publication fixture", pub)
writeLines("QC fixture", qc)
writeLines("uncategorized fixture", plain)
check(identical(classify_output_figure(pub), "publication_panels"),
      "the ancestor name overrode a publication figure's canonical folder")
check(identical(classify_output_figure(qc), "qc"), "canonical QC folder lost its role")
check(identical(classify_output_figure(plain), "publication_panels"),
      "an uncategorized figure was classified from the project ancestor")
check(identical(classify_output_figure(file.path(stage14_like, "figures", "Fig_integrated_systems_dashboard.svg")),
                "publication_panels"),
      "the word systems in a dashboard name was treated as QC")
inventory <- harmonize_analysis_outputs(stage14_like)
pub_row <- inventory[inventory$file == basename(pub), ]
qc_row <- inventory[inventory$file == basename(qc), ]
check(nrow(pub_row) == 1L && identical(pub_row$figure_class[[1]], "publication_panels"),
      "the written inventory misclassified the publication figure")
check(nrow(qc_row) == 1L && identical(qc_row$figure_class[[1]], "qc"),
      "the written inventory misclassified the QC figure")
check(grepl("figures/publication_panels/", gsub("\\\\", "/", pub_row$harmonized_path[[1]]), fixed = TRUE),
      "the publication figure's harmonized path points to another folder")
check(file.exists(file.path(stage14_like, "figures", "publication_panels", basename(plain))),
      "the uncategorized publication figure was not mirrored")
dashboard_base <- file.path(stage14_like, "figures", "Fig_integrated_systems_dashboard")
writeLines("dashboard fixture", paste0(dashboard_base, ".svg"))
mirror_plot_to_standard_folder(dashboard_base)
check(file.exists(file.path(stage14_like, "figures", "publication_panels",
                            "Fig_integrated_systems_dashboard.svg")),
      "the figure writer mirrored a systems dashboard into the wrong folder")
ok("classifier and inventory preserve publication and QC roles")

# A future semantic dashboard copy omits byte-identical legacy mirrors and
# regenerates its index from the copied authored files without a science rerun.
cat("\n[7] a copied dashboard rebuilds figure metadata without new mirrors\n")
semantic <- file.path(root, "analysis_ready", "analyses", "systems_dashboard", "5min")
semantic_pub <- file.path(semantic, "figures", "publication_panels", "Fig_dashboard_example.svg")
semantic_qc <- file.path(semantic, "figures", "qc", "Fig_chip_loss_example.svg")
semantic_root_figure <- file.path(semantic, "figures", "Fig_integrated_systems_dashboard.svg")
dir.create(dirname(semantic_pub), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(semantic_qc), recursive = TRUE, showWarnings = FALSE)
writeLines("authored publication figure", semantic_pub)
writeLines("authored QC figure", semantic_qc)
writeLines("authored root figure", semantic_root_figure)
semantic_index <- harmonize_analysis_outputs(semantic, copy_figures = FALSE)
check(nrow(semantic_index) == 3L,
      "semantic metadata included absent or historical mirror figures")
check(setequal(semantic_index$figure_class, c("publication_panels", "qc")),
      "semantic figure classes do not match authored folders")
root_row <- semantic_index[semantic_index$file == basename(semantic_root_figure), ]
check(nrow(root_row) == 1L &&
        identical(gsub("\\\\", "/", root_row$harmonized_path[[1]]),
                  "figures/Fig_integrated_systems_dashboard.svg"),
      "copy-free metadata points to an absent mirror instead of the authored root figure")
check(file.exists(file.path(semantic, "tables", "output_figure_inventory.csv")) &&
        file.exists(file.path(semantic, "tables", "output_folder_summary.csv")),
      "semantic figure metadata was not regenerated")
check(length(list.files(file.path(semantic, "figures"), pattern = "\\.svg$",
                        recursive = TRUE)) == 3L,
      "metadata refresh made a new mirror")
ok("semantic figure metadata is regenerated from authored files only")

unlink(root, recursive = TRUE)
cat("\nFigure mirror refresh checks: PASS\n")
