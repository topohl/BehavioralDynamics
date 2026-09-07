# ================================================================
# PRIMARY canonical first-night GAMM (Active)
# MMMSociability
# ================================================================
# Estimand:
#   During the FIRST Active block after the first cage change (18:30 inclusive
#   -> 06:30 exclusive, exactly 12 h), do CON / RES / SUS differ in Movement
#   level and within-window trajectory shape?
#
# This is the direct first-response estimator and uses exactly the Stage 09
# physical window. It is deliberately STANDALONE: fitting CC2-CC4 jointly would
# let later observations influence the CC1 curve through shared smooths,
# shared smoothing parameters, AR1 estimation and the model covariance.
# Stage 22 exports its own CC1 marginal as a cross-model sensitivity; that
# never substitutes for this stage (see audit/cc1_cross_model_prediction_audit.csv).
#
# PRIMARY family: 2 Sex x 3 Group contrasts = 6 tests, BH within.
# contrast_role records the biological hierarchy without altering that
# correction: SUS-RES is the primary phenotype contrast; the other two are
# contextual controls.
#
# Stage 09 and first_night_window_helpers.R are not modified.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(ggplot2); library(mgcv)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
for (h in c("phase_classification_helpers.R", "animalpos_preprocessing_helpers.R",
            "first_night_window_helpers.R", "acute_active_window_helpers.R",
            "acute_phase_window_helpers.R", "gamm_group_inference_helpers.R",
            "gamm_auc_helpers.R", "gamm_diagnostics_helpers.R",
            "mmm_publication_theme.R", "single_window_stage_runner.R")) source_mmm_helper(h)

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"
bin_size_sec <- 600L

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "20", "first_night_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)
diag_dir <- file.path(dirs$figure_root, "diagnostic"); ensure_dir(diag_dir)

cat("PRIMARY first-night Active GAMM |", bin_level, "\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

selected <- mmm_select_first_night_window(raw, bin_size_sec = bin_size_sec) %>%
  mutate(TimeHours = .data$elapsed_hours_in_window)
qc <- mmm_first_night_window_qc(selected)
write_csv(qc, file.path(dirs$tables, "first_active_window_qc.csv"))
cat("  window: ", nrow(selected), " rows | ", n_distinct(selected$AnimalNum),
    " animals | ", sum(qc$window_complete), " complete\n", sep = "")

res <- mmm_run_single_window_stage(
  selected, dirs,
  family_id = "PRIMARY_FIRST_ACTIVE__2Sex_x_3GroupContrasts",
  phase_label = "Active", stage_tag = "first_active")
stopifnot(nrow(res$primary_contrasts) == 6L)

# ---------------------------------------------------------------- sex moderation
sex_mod <- mmm_sex_moderation(
  res$auc_contrasts %>% dplyr::select("Sex", "contrast",
                                      estimate = "AUC_diff_log1p", se = "AUC_diff_SE"),
  by_cols = "contrast") %>%
  mutate(quantity = "first_active_AUC_log1p", .before = 1)

# ---------------------------------------------------------------- write tables
w <- function(x, f, d = dirs$tables) write_csv(x, file.path(d, f))
w(res$primary_contrasts, "first_active_primary_contrasts.csv")
w(res$contrasts, "first_active_all_variant_contrasts.csv")
w(res$group_auc, "first_active_gamm_group_auc.csv")
w(res$auc_contrasts, "first_active_gamm_auc_contrasts.csv")
w(res$resp_auc, "first_active_response_scale_group_auc.csv")
w(res$resp_auc_diff, "first_active_response_scale_auc_contrasts.csv")
w(res$auc_audit, "first_active_auc_vs_window_average_audit.csv")
w(res$ci_width, "first_active_ci_width_diagnostics.csv")
w(res$batch_diag_auc, "first_active_batch_weighting_diagnostic.csv")
w(res$batch_diag_contr, "first_active_batch_weighting_contrasts.csv")
w(sex_mod, "first_active_sex_moderation_contrasts.csv")
w(res$empirical, "first_active_animal_empirical_auc.csv")
w(res$trajectory, "first_active_trajectory_predictions.csv")
w(res$pointwise, "first_active_pairwise_trajectory_differences.csv")
w(res$spec, "first_active_model_specification.csv")
w(res$parametric, "first_active_parametric_terms.csv")
w(res$smooth, "first_active_smooth_terms.csv")
w(res$diagnostics, "first_active_model_diagnostics.csv", dirs$audit)
w(res$k_check, "first_active_k_check.csv", dirs$audit)
w(res$acf, "first_active_residual_acf.csv", dirs$audit)
w(res$ar1, "first_active_ar1_sequence_proof.csv", dirs$audit)

batch_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "no_batch"),
  by_cols = c("Sex", "contrast"), label = "batch_adjusted_vs_unadjusted")
scale_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "raw_scale"),
  by_cols = c("Sex", "contrast"), label = "log1p_vs_raw_gaussian")
k_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "k8"),
  by_cols = c("Sex", "contrast"), label = "k6_vs_k8")
w(batch_sens, "first_active_batch_sensitivity.csv")
w(scale_sens, "first_active_response_scale_sensitivity.csv")
w(k_sens, "first_active_k_sensitivity.csv")

# ---------------------------------------------------------------- figures
traj <- res$trajectory %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS))

# Observed 30-min group means, purely visual; they do not enter the model.
obs <- selected %>%
  mutate(bin30 = floor(.data$TimeHours * 2) / 2 + 0.25,
         Group = factor(.data$Group, levels = MMM_GROUP_LEVELS)) %>%
  group_by(.data$Sex, .data$Group, .data$bin30) %>%
  summarise(obs_mean = mean(log1p(.data$Movement), na.rm = TRUE), .groups = "drop")

p_traj <- ggplot(traj, aes(TimeAxis, fit, colour = Group, fill = Group)) +
  geom_point(data = obs, aes(bin30, obs_mean), size = 0.5, alpha = 0.35,
             inherit.aes = FALSE, colour = "grey55") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Sex) +
  mmm_scale_colour_group() + mmm_scale_fill_group() +
  scale_x_continuous(breaks = seq(0, 12, 3), limits = c(0, 12), expand = c(0.01, 0)) +
  mmm_scale_y_log1p() +
  labs(x = MMM_X_LAB_ACTIVE) +
  theme_mmm_pub()
mmm_save_pub(p_traj, file.path(dirs$figure_root, "first_active_trajectories.svg"), 180, 70)

p_auc <- res$auc_contrasts %>%
  mutate(contrast = factor(.data$contrast, levels = names(MMM_CONTRAST_COLOURS))) %>%
  ggplot(aes(AUC_diff_log1p, contrast, colour = contrast)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey55") +
  geom_errorbarh(aes(xmin = AUC_diff_CI_low, xmax = AUC_diff_CI_high),
                 height = 0, linewidth = 0.4) +
  geom_point(size = 1.5) +
  facet_wrap(~ Sex) +
  mmm_scale_colour_contrast(guide = "none") +
  labs(x = expression(paste(Delta, " AUC, log1p(Movement) x h")), y = NULL) +
  theme_mmm_pub()
mmm_save_pub(p_auc, file.path(dirs$figure_root, "first_active_auc_contrasts.svg"), 130, 55)

# Diagnostics kept out of the manuscript-facing folder.
p_qq <- ggplot(res$residuals, aes(theoretical_q, sample_q)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(size = 0.25, alpha = 0.3) + facet_wrap(~ Sex) +
  labs(x = "Theoretical quantile", y = "Deviance residual", title = "Residual QQ (diagnostic)") +
  theme_mmm_pub()
mmm_save_pub(p_qq, file.path(diag_dir, "first_active_residual_qq.svg"), 130, 65)

p_rf <- ggplot(res$residuals, aes(fitted, resid_deviance)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(size = 0.25, alpha = 0.3) + facet_wrap(~ Sex) +
  labs(x = "Fitted", y = "Deviance residual", title = "Residual vs fitted (diagnostic)") +
  theme_mmm_pub()
mmm_save_pub(p_rf, file.path(diag_dir, "first_active_residual_vs_fitted.svg"), 130, 65)

p_acf <- ggplot(res$acf, aes(lag, mean_acf, colour = stage)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_hline(yintercept = c(-0.2, 0.2), colour = "grey75", linewidth = 0.25, linetype = 2) +
  geom_line(linewidth = 0.5) + geom_point(size = 1) + facet_wrap(~ Sex) +
  scale_colour_manual(values = c(before_AR1 = "#D55E00", after_AR1 = "#0072B2")) +
  labs(x = "Lag", y = "Mean within-sequence residual ACF", title = "Residual ACF (diagnostic)") +
  theme_mmm_pub()
mmm_save_pub(p_acf, file.path(diag_dir, "first_active_residual_acf.svg"), 130, 65)

write_output_manifest(
  output_dir, script_name = "20_first_night_gamm.R",
  analysis_name = "PRIMARY canonical first-night Active GAMM", bin_level = bin_level,
  key_parameters = list(window_hours = 12, k = MMM_GAMM_K, method = MMM_GAMM_METHOD,
                        primary_family = "2 Sex x 3 Group contrasts = 6 tests (BH)"),
  primary_tables = c("tables/first_active_primary_contrasts.csv",
                     "tables/first_active_gamm_auc_contrasts.csv"),
  primary_figures = c("figures/first_active_trajectories.svg",
                      "figures/first_active_auc_contrasts.svg"))

cat("\nPRIMARY six-test family:\n")
print(as.data.frame(res$primary_contrasts %>%
  dplyr::select(Sex, contrast, contrast_role, AUC_diff_log1p, AUC_diff_per_hour,
                AUC_diff_CI_low, AUC_diff_CI_high, p_raw, p_bh)), row.names = FALSE, digits = 3)
cat("\nAUC vs window-average audit (max rel gap):",
    signif(max(res$auc_audit$rel_gap), 3), "\n")
cat("\nCI-width diagnostics (ratio vs primary):\n")
print(as.data.frame(res$ci_width %>% group_by(variant) %>%
  summarise(mean_ratio = mean(ratio_vs_primary), .groups = "drop")), row.names = FALSE, digits = 4)
cat("\nDone ->", output_dir, "\n")
