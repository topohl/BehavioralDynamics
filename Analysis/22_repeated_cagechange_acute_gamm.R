# ================================================================
# SECONDARY B - repeated acute Active response across CC1..CC4
# MMMSociability
# ================================================================
# Estimand:
#   Does the acute Movement response to regrouping change across successive
#   cage changes, and does that change differ among later CON / RES / SUS?
#
# ONE comparable clock-anchored acute window per cage change (first Active
# block, 18:30 -> 06:30, exactly 12 h). CageChange is a FACTOR so adaptation may
# be non-monotonic; a linear trend is reported secondarily only.
#
# PRIMARY shape structure is the parsimonious additive decomposition. The full
# Group x CageChange shape interaction (the previous primary) is retained as a
# declared sensitivity.
#
# This stage also exports its CC1 marginal prediction purely as a CROSS-MODEL
# SENSITIVITY to Stage 20. It never substitutes for Stage 20's first-response
# estimate: fitting CC2-CC4 jointly lets later data influence the CC1 curve
# through shared smooths, shared smoothing parameters, AR1 and the covariance.
# audit/cc1_cross_model_prediction_audit.csv quantifies exactly that.
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
output_dir <- behavior_stage_dir(project_root, "22", "repeated_cagechange_acute_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)
diag_dir <- file.path(dirs$figure_root, "diagnostic"); ensure_dir(diag_dir)

cat("SECONDARY B: repeated acute Active response CC1-CC4 |", bin_level, "\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

cc_levels <- sort(unique(as.character(raw$CageChange)))
acute <- purrr::map_dfr(cc_levels, function(cc)
  mmm_select_acute_phase_window(raw, bin_size_sec, cage_change = cc, phase = "Active") %>%
    mutate(CageChangeWindow = cc))

qc <- mmm_phase_window_qc(acute, block_cols = "CageChangeWindow") %>%
  rename(CageChange = "CageChangeWindow")
write_csv(qc, file.path(dirs$tables, "allcc_active_window_qc.csv"))
parity <- qc %>% group_by(.data$CageChange) %>%
  summarise(n_animals = n_distinct(.data$AnimalNum), n_windows = n(),
            expected_bins = first(.data$expected_bins), mean_coverage = mean(.data$coverage),
            min_coverage = min(.data$coverage), n_complete = sum(.data$window_complete),
            .groups = "drop")
write_csv(parity, file.path(dirs$tables, "allcc_active_cohort_window_parity.csv"))
print(as.data.frame(parity), row.names = FALSE, digits = 4)
stopifnot(n_distinct(acute$window_hours) == 1L, n_distinct(acute$expected_slots) == 1L)

model_dat <- acute %>% mutate(CageChange = factor(.data$CageChangeWindow, levels = cc_levels))
res <- mmm_run_stratified_gamm(model_dat, stratum_col = "CageChange", time_col = "TimeHours",
                               ar_block_cols = "CageChange", window_hours = 12)
post <- mmm_stratified_postprocess(
  res, stratum_col = "CageChange",
  global_family_id = "SECONDARY_ALLCC_ACTIVE__local_contrasts__2Sex_x_4CC_x_3Contrasts",
  stage_id = "stage22_allcc", phase_label = "Active")

mmm_stratified_write(res, post, dirs, prefix = "allcc_active")
write_csv(mmm_animal_empirical_auc(acute, time_col = "TimeHours",
            group_cols = c("AnimalNum", "Group", "Sex", "Batch", "CageChangeWindow")),
          file.path(dirs$tables, "allcc_active_animal_empirical_auc.csv"))

# ------------------------------------------------ secondary numeric CC trend
trend <- purrr::map_dfr(sort(unique(as.character(model_dat$Sex))), function(sx) {
  d <- mmm_prepare_stratified(model_dat %>% filter(.data$Sex == sx), "CageChange", "TimeHours") %>%
    mutate(CCnum = as.numeric(sub("^\\D*", "", as.character(.data$Stratum)))) %>%
    mmm_order_for_ar1(block_cols = "Stratum") %>% droplevels()
  d$.ar_start <- mmm_build_ar_start(d, block_cols = "Stratum")
  form <- stats::as.formula(paste0(
    "y_log ~ Batch + Group * CCnum + s(TimeAxis, k = ", MMM_GAMM_K, ", bs = 'tp')",
    " + s(TimeAxis, by = GroupOrd, k = ", MMM_GAMM_K, ", bs = 'tp')",
    " + s(TimeAxis, by = StratumOrd, k = ", MMM_GAMM_K, ", bs = 'tp') + s(AnimalNum, bs = 're')"))
  fit <- mmm_fit_gamm_ar1(form, d)
  as.data.frame(summary(fit$model)$p.table) %>% tibble::rownames_to_column("term") %>%
    as_tibble() %>% filter(grepl("CCnum", .data$term)) %>%
    mutate(Sex = sx, rho = fit$rho, model = "secondary_linear_CC_trend", .before = 1)
})
write_csv(trend, file.path(dirs$tables, "allcc_active_secondary_linear_trend.csv"))

# ------------------------------------------------ CC1 cross-model audit (section 13)
s20_tbl <- file.path(behavior_stage_dir(project_root, "20", "first_night_gamm", bin_level),
                     "tables", "first_active_trajectory_predictions.csv")
audit_written <- FALSE
if (file.exists(s20_tbl)) {
  s20 <- read_csv(s20_tbl, show_col_types = FALSE, progress = FALSE) %>%
    filter(.data$variant == "primary") %>%
    transmute(Sex = .data$Sex, Group = .data$Group, TimeAxis = .data$TimeAxis,
              fit_stage20 = .data$fit, se_stage20 = .data$se)

  cc1_pred <- function(variant_key) {
    res$trajectory %>%
      filter(.data$variant == variant_key, .data$stratum == cc_levels[1]) %>%
      transmute(Sex = .data$Sex, Group = .data$Group, TimeAxis = .data$TimeAxis,
                fit = .data$fit, se = .data$se)
  }
  joined <- s20 %>%
    inner_join(cc1_pred("full_shape") %>% rename(fit_s22_old = "fit", se_s22_old = "se"),
               by = c("Sex", "Group", "TimeAxis")) %>%
    inner_join(cc1_pred("primary") %>% rename(fit_s22_new = "fit", se_s22_new = "se"),
               by = c("Sex", "Group", "TimeAxis")) %>%
    mutate(diff_old = .data$fit_s22_old - .data$fit_stage20,
           diff_new = .data$fit_s22_new - .data$fit_stage20)

  # AUC of each CC1 curve on the shared grid.
  auc_of <- function(df, col) {
    w <- mmm_trapz_weights(df$TimeAxis)
    sum(w * df[[col]])
  }
  audit <- joined %>% group_by(.data$Sex, .data$Group) %>%
    summarise(
      n_grid = dplyr::n(),
      rmse_old = sqrt(mean(.data$diff_old^2)), rmse_new = sqrt(mean(.data$diff_new^2)),
      mean_abs_diff_old = mean(abs(.data$diff_old)), mean_abs_diff_new = mean(abs(.data$diff_new)),
      max_abs_diff_old = max(abs(.data$diff_old)), max_abs_diff_new = max(abs(.data$diff_new)),
      AUC_stage20 = auc_of(dplyr::pick(dplyr::everything()), "fit_stage20"),
      AUC_stage22_old = auc_of(dplyr::pick(dplyr::everything()), "fit_s22_old"),
      AUC_stage22_revised = auc_of(dplyr::pick(dplyr::everything()), "fit_s22_new"),
      .groups = "drop") %>%
    mutate(AUC_difference_old = .data$AUC_stage22_old - .data$AUC_stage20,
           AUC_difference_revised = .data$AUC_stage22_revised - .data$AUC_stage20,
           discrepancy_reduced = abs(.data$AUC_difference_revised) < abs(.data$AUC_difference_old))
  write_csv(audit, file.path(dirs$audit, "cc1_cross_model_prediction_audit.csv"))
  write_csv(joined, file.path(dirs$audit, "cc1_cross_model_pointwise.csv"))
  audit_written <- TRUE

  # Raw-input identity proof: the CC1 rows are the same in both stages.
  s20_rows <- mmm_select_first_night_window(raw, bin_size_sec = bin_size_sec) %>%
    arrange(.data$AnimalNum, .data$BinStart)
  s22_rows <- acute %>% filter(.data$CageChangeWindow == cc_levels[1]) %>%
    arrange(.data$AnimalNum, .data$BinStart)
  raw_identical <- nrow(s20_rows) == nrow(s22_rows) &&
    identical(as.character(s20_rows$AnimalNum), as.character(s22_rows$AnimalNum)) &&
    identical(s20_rows$BinStart, s22_rows$BinStart) &&
    isTRUE(all.equal(s20_rows$Movement, s22_rows$Movement, tolerance = 0))
  write_csv(tibble(check = "raw CC1 input rows identical between Stage20 and Stage22",
                   n_rows_stage20 = nrow(s20_rows), n_rows_stage22 = nrow(s22_rows),
                   identical = raw_identical),
            file.path(dirs$audit, "cc1_raw_input_identity_check.csv"))

  long <- joined %>%
    tidyr::pivot_longer(c("fit_stage20", "fit_s22_old", "fit_s22_new"),
                        names_to = "model", values_to = "fit") %>%
    mutate(model = recode(.data$model, fit_stage20 = "Stage20 standalone",
                          fit_s22_old = "Stage22 full-shape", fit_s22_new = "Stage22 parsimonious"))
  p_audit <- ggplot(long, aes(TimeAxis, fit, colour = model)) +
    geom_line(linewidth = 0.5) + facet_grid(Sex ~ Group) +
    scale_colour_manual(values = c("Stage20 standalone" = "#000000",
                                   "Stage22 full-shape" = "#D55E00",
                                   "Stage22 parsimonious" = "#0072B2")) +
    labs(x = MMM_X_LAB_ACTIVE, y = "Predicted log1p(Movement)",
         title = "CC1 prediction: standalone vs joint models (diagnostic)") +
    theme_mmm_pub()
  mmm_save_pub(p_audit, file.path(diag_dir, "cc1_cross_model_predictions.svg"), 170, 90)

  cat("\nCC1 cross-model audit (RMSE vs Stage20):\n")
  print(as.data.frame(audit %>% dplyr::select(Sex, Group, rmse_old, rmse_new,
        AUC_difference_old, AUC_difference_revised, discrepancy_reduced)),
        row.names = FALSE, digits = 3)
  cat("Raw CC1 input rows identical:", raw_identical, "\n")
} else {
  cat("\n  NOTE: Stage 20 predictions not found; run Analysis/20 first for the CC1 audit.\n")
}

figs <- mmm_stratified_figures(res, post, dirs, stratum_col = "CageChange",
                               x_lab = MMM_X_LAB_ACTIVE, stratum_prefix = "",
                               file_prefix = "allcc_active")
write_csv(figs, file.path(dirs$audit, "allcc_active_figure_inventory.csv"))

write_output_manifest(
  output_dir, script_name = "22_repeated_cagechange_acute_gamm.R",
  analysis_name = "SECONDARY B: repeated acute Active response across CC1-CC4",
  bin_level = bin_level,
  key_parameters = list(cage_changes = paste(cc_levels, collapse = ","), window_hours = 12,
                        k = MMM_GAMM_K, primary_shape = "parsimonious additive"),
  primary_tables = c("tables/allcc_active_gamm_auc_contrasts.csv",
                     "tables/allcc_active_adaptation_auc_contrasts.csv"),
  primary_figures = c("figures/allcc_active_trajectories.svg",
                      "figures/allcc_active_adaptation_auc.svg"))

cat("\nOmnibus (primary, parsimonious):\n")
print(as.data.frame(post$omnibus %>% dplyr::select(Sex, term, df, F_stat, p_raw, p_bh)),
      row.names = FALSE, digits = 4)
cat("\nAdaptation AUC (CC4 - CC1):\n")
print(as.data.frame(post$adaptation %>%
  dplyr::select(Sex, scope, contrast_name, estimate, ci_low, ci_high, p_raw)),
  row.names = FALSE, digits = 3)
cat("\nShape comparison (full minus parsimonious):\n")
print(as.data.frame(post$shape_comparison %>%
  dplyr::select(Sex, delta_aic_full_minus_primary, delta_edf_full_minus_primary)),
  row.names = FALSE, digits = 4)
cat("\nDone ->", output_dir, "\n")
