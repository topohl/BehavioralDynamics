# ================================================================
# SECONDARY A - full CC1 Active-phase longitudinal GAMM
# MMMSociability
# ================================================================
# Estimand:
#   Across ALL Active blocks of the first cage-change period, do CON / RES / SUS
#   differ in Movement level, within-night trajectory shape, and night-to-night
#   adaptation?
#
# Canonical replacement for the question the archived _firstChangeActive GAMM
# attempted. That analysis overlaid all four CC1 Active nights on one 0-12 h
# axis with no night term. Here ActiveNight is explicit, interacts with Group,
# and every figure is faceted by night.
#
# PRIMARY shape structure is the parsimonious additive decomposition; the full
# Group x ActiveNight shape interaction is retained as a declared sensitivity
# (see *_shape_model_comparison.csv).
#
# SECONDARY stage. Families are separate from the Stage 20 six-test family:
#   local contrasts   2 Sex x N nights x 3 = conservative global family (primary)
#                     plus labelled within-Sex and within-night localizations
#   omnibus           Group x ActiveNight, its own family
#   adaptation        planned last-night minus first-night AUC contrasts
# CC1 ActiveNight 1 IS the Stage 20 data; this is not independent replication.
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
output_dir <- behavior_stage_dir(project_root, "21", "cc1_active_longitudinal_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)

cat("SECONDARY A: full CC1 Active-phase GAMM |", bin_level, "\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

nights <- mmm_select_cc_phase_blocks(raw, bin_size_sec, cage_change = target_cc,
                                     phase = "Active") %>%
  rename(ActiveNight = "PhaseBlock", TimeWithinActiveHours = "TimeWithinPhaseHours")

qc <- mmm_phase_window_qc(nights, block_cols = "ActiveNight",
                          start_col = "block_window_start", end_col = "block_window_end")
write_csv(qc, file.path(dirs$tables, "cc1_active_block_qc.csv"))
n_nights <- n_distinct(nights$ActiveNight)
cat("  rows: ", nrow(nights), " | animals: ", n_distinct(nights$AnimalNum),
    " | Active nights: ", n_nights, " | windows: ", nrow(qc),
    " | mean coverage: ", round(mean(qc$coverage), 4), "\n", sep = "")

res <- mmm_run_stratified_gamm(nights, stratum_col = "ActiveNight",
                               time_col = "TimeWithinActiveHours",
                               ar_block_cols = "ActiveNight", window_hours = 12)

post <- mmm_stratified_postprocess(
  res, stratum_col = "ActiveNight",
  global_family_id = paste0("SECONDARY_CC1_ACTIVE__local_contrasts__2Sex_x_",
                            n_nights, "Nights_x_3Contrasts"),
  stage_id = "stage21_cc1", phase_label = "Active")

mmm_stratified_write(res, post, dirs, prefix = "cc1_active")
write_csv(mmm_animal_empirical_auc(nights, time_col = "TimeWithinActiveHours",
            group_cols = c("AnimalNum", "Group", "Sex", "Batch", "ActiveNight")),
          file.path(dirs$tables, "cc1_active_animal_empirical_auc.csv"))

figs <- mmm_stratified_figures(res, post, dirs, stratum_col = "ActiveNight",
                               x_lab = MMM_X_LAB_ACTIVE, stratum_prefix = "Night",
                               file_prefix = "cc1_active",
                               traj_file = "cc1_active_trajectories_by_night.svg")
write_csv(figs, file.path(dirs$audit, "cc1_active_figure_inventory.csv"))

write_output_manifest(
  output_dir, script_name = "21_cc1_active_longitudinal_gamm.R",
  analysis_name = "SECONDARY A: full CC1 Active-phase longitudinal GAMM", bin_level = bin_level,
  key_parameters = list(cage_change = target_cc, n_active_nights = n_nights,
                        k = MMM_GAMM_K, primary_shape = "parsimonious additive"),
  primary_tables = c("tables/cc1_active_gamm_auc_contrasts.csv",
                     "tables/cc1_active_adaptation_auc_contrasts.csv"),
  primary_figures = c("figures/cc1_active_trajectories_by_night.svg",
                      "figures/cc1_active_adaptation_auc.svg"))

cat("\nOmnibus (primary, parsimonious):\n")
print(as.data.frame(post$omnibus %>% dplyr::select(Sex, term, df, F_stat, p_raw, p_bh)),
      row.names = FALSE, digits = 4)
cat("\nAdaptation AUC (last night - first night):\n")
print(as.data.frame(post$adaptation %>%
  dplyr::select(Sex, scope, estimate, ci_low, ci_high, p_raw)), row.names = FALSE, digits = 3)
cat("\nShape-model comparison (full minus parsimonious):\n")
print(as.data.frame(post$shape_comparison %>%
  dplyr::select(Sex, delta_aic_full_minus_primary, delta_edf_full_minus_primary, delta_dev_expl)),
  row.names = FALSE, digits = 4)
cat("\nLocal contrasts with q_BH_global < 0.05:",
    sum(post$localization$q_BH_global24 < 0.05), "of", nrow(post$localization), "\n")
cat("Done ->", output_dir, "\n")
