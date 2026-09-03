# Regression guard for the legacy Windows MAX_PATH (260) boundary.
#
# The Stage 15 sensitivity branch previously produced a 264-character deepest
# figure path and failed with an opaque graphics-device "cannot open file".
# This test reconstructs the canonical Stage 15 output trees at the NORMAL
# project root and asserts every expected output path stays inside the budget,
# so adding a longer filename later cannot silently walk the tree back to the
# boundary.
#
# It is intentionally independent of the current user name and of any temporary
# working directory: the project root comes from the same option/default the
# production scripts use.

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

source("Analysis/_pipeline_setup.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

project_root <- getOption(
  "mmm.project_root",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
)
check(!grepl("AppData|Temp|/tmp/", project_root, ignore.case = TRUE),
      "the test must run against the canonical project root, not a temp dir")

base_output_dir <- file.path(project_root, "analysis_ready", "06_behavioral_dynamics")

# The dataset labels Stage 15 derives from its source filenames.
labels <- c(
  "m_neuron_neuropil_primary_all_replicates",
  "m_neuron_neuropil_sensitivity_flagged_replicates_removed"
)
slugs <- mmm_output_dir_slug(labels)
mmm_assert_unique_output_slugs(labels, slugs, "Stage 15 fixture labels")

check(length(unique(slugs)) == length(slugs), "primary and sensitivity slugs must be distinct")
check(all(nchar(slugs) <= MMM_MAX_OUTPUT_SLUG_CHARS),
      paste0("every slug must fit the ", MMM_MAX_OUTPUT_SLUG_CHARS, "-character slug budget"))

# Every relative path Stage 15 is expected to write, per dataset. Derived from
# the actual primary-dataset output tree so the test tracks reality rather than
# a hand-picked file.
subsets <- c("all", "female", "male")
rel_paths <- c(
  "tables/behavior_proteomics_id_crosswalk.csv",
  "manifest/output_manifest.csv",
  unlist(lapply(subsets, function(s) c(
    file.path(s, "tables", paste0("behavior_proteomics_merged_", s, ".csv")),
    file.path(s, "tables", paste0("behavior_proteomics_axis_correlations_", s, ".csv")),
    file.path(s, "tables", paste0("behavior_proteomics_feature_correlations_", s, ".csv")),
    file.path(s, "tables", paste0("behavior_proteomics_axis_inventory_", s, ".csv")),
    # harmonize_analysis_outputs() re-files figures into publication_panels or qc
    # depending on classify_output_figure(), so BOTH placements must be budgeted:
    # the classification differs between datasets and the deeper one dominates.
    file.path(s, "figures", paste0("strongest_axis_behavior_proteomics_relationship_", s, ".svg")),
    file.path(s, "figures/qc", paste0("strongest_axis_behavior_proteomics_relationship_", s, ".svg")),
    file.path(s, "figures/publication_panels", paste0("strongest_axis_behavior_proteomics_relationship_", s, ".svg")),
    file.path(s, "figures/publication_panels", paste0("strongest_axis_behavior_proteomics_relationship_", s, ".pdf")),
    file.path(s, "figures/publication_panels", paste0("strongest_axis_behavior_proteomics_relationship_", s, ".png")),
    file.path(s, "figures/publication_panels", paste0("behavior_proteomics_axis_heatmap_", s, ".svg")),
    file.path(s, "figures/publication_panels", paste0("behavior_proteomics_axis_heatmap_", s, ".pdf")),
    file.path(s, "figures/publication_panels", paste0("behavior_proteomics_axis_heatmap_", s, ".png"))
  )))
)

expected <- unlist(lapply(slugs, function(sl) {
  file.path(base_output_dir, paste0("proteomics_", sl), rel_paths)
}))

# Also include whatever the live tree actually contains, so real filenames that
# the fixture list does not anticipate are still covered.
live <- character()
for (sl in slugs) {
  d <- file.path(base_output_dir, paste0("proteomics_", sl))
  if (dir.exists(d)) live <- c(live, list.files(d, recursive = TRUE, full.names = TRUE))
}
all_paths <- unique(c(expected, live))
cat("checked", length(all_paths), "Stage 15 paths (",
    length(expected), "expected +", length(live), "live )\n")

worst <- all_paths[which.max(nchar(all_paths))]
cat("longest Stage 15 path:", nchar(worst), "chars\n  ", worst, "\n", sep = "")

mmm_assert_output_path_budget(all_paths, source_label = "Stage 15 output tree")
check(max(nchar(all_paths)) <= MMM_MAX_OUTPUT_PATH_CHARS,
      paste0("longest Stage 15 path (", max(nchar(all_paths)), ") must be <= ",
             MMM_MAX_OUTPUT_PATH_CHARS))
check(max(nchar(all_paths)) < 260L,
      "longest Stage 15 path must be strictly under the legacy MAX_PATH of 260")

# The margin must be real, not one character.
margin <- 260L - max(nchar(all_paths))
cat("margin below legacy MAX_PATH:", margin, "chars\n")
check(margin >= 20L, paste0("path-length margin below 260 must be >= 20 chars; got ", margin))

# The pre-fix construction must be demonstrably over budget, so this test would
# have caught the original defect.
old_worst <- file.path(base_output_dir,
                       paste0("proteomics_integration_", labels[2]),
                       "male/figures/publication_panels/strongest_axis_behavior_proteomics_relationship_male.svg")
check(nchar(old_worst) > 260L,
      "the pre-fix sensitivity path must exceed 260 (documents the original defect)")
cat("pre-fix sensitivity path would have been", nchar(old_worst), "chars (over MAX_PATH)\n")

# Stage 16 downstream tree must also stay inside budget.
s16 <- file.path(project_root, "analysis_ready", "16_manuscript_behavior_report")
if (dir.exists(s16)) {
  f16 <- list.files(s16, recursive = TRUE, full.names = TRUE)
  if (length(f16)) {
    cat("longest Stage 16 path:", max(nchar(f16)), "chars\n")
    mmm_assert_output_path_budget(f16, source_label = "Stage 16 output tree")
  }
}

cat("Output path-length regression checks: PASS\n")
