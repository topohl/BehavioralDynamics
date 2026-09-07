# ================================================================
# SECONDARY - repeated post-active Inactive response across CC1..CC4
# MMMSociability
# ================================================================
# Mirrors Analysis/22 for the Inactive phase. Estimand:
#   Does inactive-phase locomotor organization after regrouping change across
#   successive cage changes, and does that change differ by later phenotype?
#
# ONE comparable window per cage change: the first scheduled Inactive block
# FOLLOWING that cage change's acute Active block, 06:30 -> 18:30, exactly 12 h.
# CageChange is a FACTOR so adaptation may be non-monotonic.
#
# Not sleep, not circadian disruption. Own multiplicity families; never merged
# with the Active stages. Active-vs-Inactive phase specificity requires a formal
# Phase interaction test, which this stage does not perform.
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
bin_level <- "10min_based"; bin_size_sec <- 600L

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "25", "repeated_cagechange_inactive_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)

cat("SECONDARY: repeated post-active Inactive response CC1-CC4 |", bin_level, "\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

cc_levels <- sort(unique(as.character(raw$CageChange)))
acute <- purrr::map_dfr(cc_levels, function(cc)
  mmm_select_acute_phase_window(raw, bin_size_sec, cage_change = cc, phase = "Inactive") %>%
    mutate(CageChangeWindow = cc))

qc <- mmm_phase_window_qc(acute, block_cols = "CageChangeWindow") %>%
  rename(CageChange = "CageChangeWindow")
write_csv(qc, file.path(dirs$tables, "allcc_inactive_window_qc.csv"))
parity <- qc %>% group_by(.data$CageChange) %>%
  summarise(n_animals = n_distinct(.data$AnimalNum), n_windows = n(),
            expected_bins = first(.data$expected_bins), mean_coverage = mean(.data$coverage),
            min_coverage = min(.data$coverage), mean_zero_fraction = mean(.data$zero_fraction),
            n_complete = sum(.data$window_complete), .groups = "drop")
write_csv(parity, file.path(dirs$tables, "allcc_inactive_cohort_window_parity.csv"))
print(as.data.frame(parity), row.names = FALSE, digits = 4)
stopifnot(n_distinct(acute$window_hours) == 1L, n_distinct(acute$expected_slots) == 1L)

model_dat <- acute %>% mutate(CageChange = factor(.data$CageChangeWindow, levels = cc_levels))
res <- mmm_run_stratified_gamm(model_dat, stratum_col = "CageChange", time_col = "TimeHours",
                               ar_block_cols = "CageChange", window_hours = 12)
post <- mmm_stratified_postprocess(
  res, stratum_col = "CageChange",
  global_family_id = "SECONDARY_ALLCC_INACTIVE__local_contrasts__2Sex_x_4CC_x_3Contrasts",
  stage_id = "stage25_allcc", phase_label = "Inactive")

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
mmm_stratified_write(res, post, dirs, prefix = "allcc_inactive")
write_csv(mmm_animal_empirical_auc(acute, time_col = "TimeHours",
            group_cols = c("AnimalNum", "Group", "Sex", "Batch", "CageChangeWindow")),
          file.path(dirs$tables, "allcc_inactive_animal_empirical_auc.csv"))

figs <- mmm_stratified_figures(res, post, dirs, stratum_col = "CageChange",
                               x_lab = MMM_X_LAB_INACTIVE, stratum_prefix = "",
                               file_prefix = "allcc_inactive")
write_csv(figs, file.path(dirs$audit, "allcc_inactive_figure_inventory.csv"))

write_output_manifest(
  output_dir, script_name = "25_repeated_cagechange_inactive_gamm.R",
  analysis_name = "SECONDARY: repeated post-active Inactive response across CC1-CC4",
  bin_level = bin_level,
  key_parameters = list(cage_changes = paste(cc_levels, collapse = ","), window_hours = 12,
                        phase = "Inactive", k = MMM_GAMM_K,
                        primary_shape = "parsimonious additive"),
  primary_tables = c("tables/allcc_inactive_gamm_auc_contrasts.csv",
                     "tables/allcc_inactive_adaptation_auc_contrasts.csv"),
  primary_figures = c("figures/allcc_inactive_trajectories.svg",
                      "figures/allcc_inactive_adaptation_auc.svg"))

cat("\nOmnibus:\n")
print(as.data.frame(post$omnibus %>% dplyr::select(Sex, term, df, F_stat, p_raw, p_bh)),
      row.names = FALSE, digits = 4)
cat("\nAdaptation AUC (CC4 - CC1):\n")
print(as.data.frame(post$adaptation %>%
  dplyr::select(Sex, scope, contrast_name, estimate, ci_low, ci_high, p_raw)),
  row.names = FALSE, digits = 3)
cat("\nLocal contrasts with q_BH_global < 0.05:",
    sum(post$localization$q_BH_global24 < 0.05), "of", nrow(post$localization), "\n")
cat("Done ->", output_dir, "\n")
