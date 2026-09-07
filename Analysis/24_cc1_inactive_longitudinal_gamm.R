# ================================================================
# SECONDARY - full CC1 Inactive-phase longitudinal GAMM
# MMMSociability
# ================================================================
# Mirrors Analysis/21 for the Inactive phase. Estimand:
#   Across the Inactive blocks that FOLLOW the acute Active response within the
#   first cage-change period, do CON / RES / SUS differ in inactive-phase
#   locomotor organization, and does that change block to block?
#
# Every Inactive block keeps its identity (InactiveBlock = 1..N) and its own
# 06:30-18:30 clock axis; blocks are never overlaid.
#
# PRIMARY shape structure is the parsimonious additive decomposition; the full
# Group x InactiveBlock shape interaction is a declared sensitivity.
# AR1 restarts at animal, InactiveBlock and slot gaps.
#
# Not sleep, not circadian disruption. Own multiplicity families, never merged
# with any Active stage.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(ggplot2); library(mgcv)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
for (h in c("phase_classification_helpers.R", "animalpos_preprocessing_helpers.R",
            "first_night_window_helpers.R", "acute_active_window_helpers.R",
            "acute_phase_window_helpers.R", "gamm_group_inference_helpers.R",
            "gamm_auc_helpers.R", "gamm_diagnostics_helpers.R",
            "mmm_publication_theme.R", "stratified_gamm_driver.R",
            "stratified_stage_runner.R")) source_mmm_helper(h)

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"; bin_size_sec <- 600L; target_cc <- "CC1"

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "24", "cc1_inactive_longitudinal_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)

cat("SECONDARY: full CC1 Inactive-phase GAMM |", bin_level, "\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

blocks <- mmm_select_cc_phase_blocks(raw, bin_size_sec, cage_change = target_cc,
                                     phase = "Inactive") %>%
  rename(InactiveBlock = "PhaseBlock", TimeWithinInactiveHours = "TimeWithinPhaseHours")

qc <- mmm_phase_window_qc(blocks, block_cols = "InactiveBlock",
                          start_col = "block_window_start", end_col = "block_window_end")
write_csv(qc, file.path(dirs$tables, "cc1_inactive_block_qc.csv"))
n_blocks <- n_distinct(blocks$InactiveBlock)
cat("  rows: ", nrow(blocks), " | animals: ", n_distinct(blocks$AnimalNum),
    " | Inactive blocks: ", n_blocks, " | windows: ", nrow(qc),
    " | mean coverage: ", round(mean(qc$coverage), 4),
    " | mean zero fraction: ", round(mean(qc$zero_fraction), 4), "\n", sep = "")

res <- mmm_run_stratified_gamm(blocks, stratum_col = "InactiveBlock",
                               time_col = "TimeWithinInactiveHours",
                               ar_block_cols = "InactiveBlock", window_hours = 12)
post <- mmm_stratified_postprocess(
  res, stratum_col = "InactiveBlock",
  global_family_id = paste0("SECONDARY_CC1_INACTIVE__local_contrasts__2Sex_x_",
                            n_blocks, "Blocks_x_3Contrasts"),
  stage_id = "stage24_cc1", phase_label = "Inactive")

# Inactive Gaussian-log1p inference is not adequate for an ~89% zero response;
# stamp every inferential table so the caveat travels with the numbers.
INACTIVE_STATUS <- "MODEL_INADEQUATE__DO_NOT_INTERPRET"
INACTIVE_REASON <- paste("Gaussian log1p GAMM on a ~89% zero response;",
  "response-distribution misspecification, not measurement incompleteness",
  "(window coverage is complete). Retained as diagnostic provenance only.")
for (nm in c("adaptation","localization","omnibus","group_auc","resp_auc","sex_moderation")) {
  post[[nm]] <- mmm_stamp_status(post[[nm]], INACTIVE_STATUS, INACTIVE_REASON)
}
for (nm in c("contrasts","trajectory","pointwise","parametric","smooth","stratum_avg")) {
  res[[nm]] <- mmm_stamp_status(res[[nm]], INACTIVE_STATUS, INACTIVE_REASON)
}
mmm_stratified_write(res, post, dirs, prefix = "cc1_inactive")
write_csv(mmm_animal_empirical_auc(blocks, time_col = "TimeWithinInactiveHours",
            group_cols = c("AnimalNum", "Group", "Sex", "Batch", "InactiveBlock")),
          file.path(dirs$tables, "cc1_inactive_animal_empirical_auc.csv"))

figs <- mmm_stratified_figures(res, post, dirs, stratum_col = "InactiveBlock",
                               x_lab = MMM_X_LAB_INACTIVE, stratum_prefix = "Block",
                               file_prefix = "cc1_inactive",
                               traj_file = "cc1_inactive_trajectories_by_block.svg")
write_csv(figs, file.path(dirs$audit, "cc1_inactive_figure_inventory.csv"))

write_output_manifest(
  output_dir, script_name = "24_cc1_inactive_longitudinal_gamm.R",
  analysis_name = "SECONDARY: full CC1 Inactive-phase longitudinal GAMM", bin_level = bin_level,
  key_parameters = list(cage_change = target_cc, n_inactive_blocks = n_blocks,
                        k = MMM_GAMM_K, primary_shape = "parsimonious additive"),
  primary_tables = c("tables/cc1_inactive_gamm_auc_contrasts.csv",
                     "tables/cc1_inactive_adaptation_auc_contrasts.csv"),
  primary_figures = c("figures/cc1_inactive_trajectories_by_block.svg",
                      "figures/cc1_inactive_adaptation_auc.svg"))

cat("\nOmnibus:\n")
print(as.data.frame(post$omnibus %>% dplyr::select(Sex, term, df, F_stat, p_raw, p_bh)),
      row.names = FALSE, digits = 4)
cat("\nAdaptation AUC (last block - first block):\n")
print(as.data.frame(post$adaptation %>%
  dplyr::select(Sex, scope, contrast_name, estimate, ci_low, ci_high, p_raw)),
  row.names = FALSE, digits = 3)
cat("\nLocal contrasts with q_BH_global < 0.05:",
    sum(post$localization$q_BH_global24 < 0.05), "of", nrow(post$localization), "\n")
cat("Done ->", output_dir, "\n")
