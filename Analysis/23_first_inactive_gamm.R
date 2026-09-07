# ================================================================
# SECONDARY - first post-active Inactive-phase GAMM
# MMMSociability
# ================================================================
# Estimand:
#   During the first scheduled Inactive block FOLLOWING the acute Active
#   response to the first cage change (06:30 inclusive -> 18:30 exclusive,
#   exactly 12 h), do CON / RES / SUS differ in Movement level and
#   within-window trajectory shape?
#
# Terminology: "Inactive phase", "inactive-phase locomotor organization".
# This is NOT sleep, circadian disruption or sleep disruption; none of those is
# independently validated by RFID Movement.
#
# SECONDARY stage with its OWN six-test BH family. It is never merged with the
# Stage 20 Active family: Active and Inactive are biologically distinct
# prespecified analyses.
#
# QC GATE (section 18): Inactive Movement is heavily zero-inflated. The stage
# writes a full QC table and a Group-wise completeness test; read
# audit/first_inactive_qc_gate.csv before making any biological claim.
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
            "mmm_publication_theme.R", "single_window_stage_runner.R")) source_mmm_helper(h)

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"; bin_size_sec <- 600L; target_cc <- "CC1"

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "23", "first_inactive_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)
diag_dir <- file.path(dirs$figure_root, "diagnostic"); ensure_dir(diag_dir)

cat("SECONDARY: first post-active Inactive GAMM |", bin_level, "\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

selected <- mmm_select_acute_phase_window(raw, bin_size_sec, cage_change = target_cc,
                                          phase = "Inactive")

# ---------------------------------------------------------------- QC gate
qc <- mmm_phase_window_qc(selected, block_cols = "target_cage_change")
write_csv(qc, file.path(dirs$tables, "first_inactive_window_qc.csv"))

gate <- purrr::map_dfr(sort(unique(qc$Sex)), function(sx) {
  q <- qc %>% filter(.data$Sex == sx)
  cov_p <- if (stats::var(q$coverage) == 0) NA_real_ else
    stats::kruskal.test(q$coverage ~ factor(q$Group))$p.value
  tibble(Sex = sx,
         n_animals = nrow(q),
         mean_coverage = mean(q$coverage), min_coverage = min(q$coverage),
         coverage_constant = stats::var(q$coverage) == 0,
         coverage_by_group_kruskal_p = cov_p,
         zero_fraction_overall = mean(q$zero_fraction),
         zero_fraction_by_group_kruskal_p =
           stats::kruskal.test(q$zero_fraction ~ factor(q$Group))$p.value,
         completeness_phenotype_dependent =
           !is.na(cov_p) && cov_p < 0.05)
})
write_csv(gate, file.path(dirs$audit, "first_inactive_qc_gate.csv"))
cat("\nQC GATE:\n"); print(as.data.frame(gate), row.names = FALSE, digits = 4)
if (any(gate$completeness_phenotype_dependent, na.rm = TRUE)) {
  cat("\n  *** WARNING: Inactive measurement completeness is phenotype-dependent.\n",
      "      Biological claims from this stage must be withheld pending review.\n", sep = "")
} else {
  cat("\n  Coverage is not phenotype-dependent; the completeness gate passes.\n",
      "  NOTE: Movement is heavily zero-inflated in the Inactive phase; see\n",
      "  audit/first_inactive_model_diagnostics.csv for response-model adequacy.\n", sep = "")
}

res <- mmm_run_single_window_stage(
  selected, dirs,
  family_id = "SECONDARY_FIRST_INACTIVE__2Sex_x_3GroupContrasts",
  phase_label = "Inactive", stage_tag = "first_inactive")
stopifnot(nrow(res$primary_contrasts) == 6L)

# ---------------------------------------------------------------- inference status
# The Gaussian-log1p response model is not adequate for an ~89%-zero response
# (see audit/first_inactive_model_diagnostics.csv). Every inferential table is
# stamped so a q-value can never be lifted out of a CSV without the caveat.
# These are retained as diagnostic provenance only. The response family is NOT
# replaced in this pass; see the Inactive model-selection audit.
INACTIVE_STATUS <- "MODEL_INADEQUATE__DO_NOT_INTERPRET"
INACTIVE_REASON <- paste(
  "Gaussian log1p GAMM on a ~89% zero response; residual skew 2.7-3.1,",
  "kurtosis 9.6-12.4, QQ correlation 0.70-0.76, deviance explained ~0.04.",
  "Window coverage is 1.000 in all 111 animals, so this is response-distribution",
  "misspecification, not measurement incompleteness.")
for (nm in c("primary_contrasts", "contrasts", "group_auc", "auc_contrasts",
             "resp_auc", "resp_auc_diff", "pointwise", "trajectory", "parametric", "smooth")) {
  res[[nm]] <- mmm_stamp_status(res[[nm]], INACTIVE_STATUS, INACTIVE_REASON)
}

sex_mod <- mmm_sex_moderation(
  res$auc_contrasts %>% dplyr::select("Sex", "contrast",
                                      estimate = "AUC_diff_log1p", se = "AUC_diff_SE"),
  by_cols = "contrast") %>% mutate(quantity = "first_inactive_AUC_log1p", .before = 1) %>%
  mmm_stamp_status(INACTIVE_STATUS, INACTIVE_REASON)

w <- function(x, f, d = dirs$tables) write_csv(x, file.path(d, f))
w(res$primary_contrasts, "first_inactive_primary_contrasts.csv")
w(res$contrasts, "first_inactive_all_variant_contrasts.csv")
w(res$group_auc, "first_inactive_gamm_group_auc.csv")
w(res$auc_contrasts, "first_inactive_gamm_auc_contrasts.csv")
w(res$resp_auc, "first_inactive_response_scale_group_auc.csv")
w(res$resp_auc_diff, "first_inactive_response_scale_auc_contrasts.csv")
w(res$auc_audit, "first_inactive_auc_vs_window_average_audit.csv")
w(res$ci_width, "first_inactive_ci_width_diagnostics.csv")
w(res$batch_diag_auc, "first_inactive_batch_weighting_diagnostic.csv")
w(res$batch_diag_contr, "first_inactive_batch_weighting_contrasts.csv")
w(sex_mod, "first_inactive_sex_moderation_contrasts.csv")
w(res$empirical, "first_inactive_animal_empirical_auc.csv")
w(res$trajectory, "first_inactive_trajectory_predictions.csv")
w(res$pointwise, "first_inactive_pairwise_trajectory_differences.csv")
w(res$spec, "first_inactive_model_specification.csv")
w(res$parametric, "first_inactive_parametric_terms.csv")
w(res$smooth, "first_inactive_smooth_terms.csv")
w(res$diagnostics, "first_inactive_model_diagnostics.csv", dirs$audit)
w(res$k_check, "first_inactive_k_check.csv", dirs$audit)
w(res$acf, "first_inactive_residual_acf.csv", dirs$audit)
w(res$ar1, "first_inactive_ar1_sequence_proof.csv", dirs$audit)

for (nm in c("no_batch", "raw_scale", "k8")) {
  w(mmm_sensitivity_compare(
      res$contrasts %>% filter(.data$variant == "primary"),
      res$contrasts %>% filter(.data$variant == nm),
      by_cols = c("Sex", "contrast"), label = paste0("primary_vs_", nm)),
    paste0("first_inactive_sensitivity_", nm, ".csv"))
}

# ---------------------------------------------------------------- figures
traj <- res$trajectory %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS))
p_traj <- ggplot(traj, aes(TimeAxis, fit, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.7) + facet_wrap(~ Sex) +
  mmm_scale_colour_group() + mmm_scale_fill_group() +
  scale_x_continuous(breaks = seq(0, 12, 3), limits = c(0, 12), expand = c(0.01, 0)) +
  mmm_scale_y_log1p(response_breaks = c(0, 0.25, 0.5, 1, 2)) +
  labs(x = MMM_X_LAB_INACTIVE) + theme_mmm_pub()
mmm_save_pub(p_traj, file.path(dirs$figure_root, "first_inactive_trajectories.svg"), 180, 70)

p_auc <- res$auc_contrasts %>%
  mutate(contrast = factor(.data$contrast, levels = names(MMM_CONTRAST_COLOURS))) %>%
  ggplot(aes(AUC_diff_log1p, contrast, colour = contrast)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey55") +
  geom_errorbarh(aes(xmin = AUC_diff_CI_low, xmax = AUC_diff_CI_high), height = 0, linewidth = 0.4) +
  geom_point(size = 1.5) + facet_wrap(~ Sex) +
  mmm_scale_colour_contrast(guide = "none") +
  labs(x = "AUC difference, log1p(Movement) x h", y = NULL) + theme_mmm_pub()
mmm_save_pub(p_auc, file.path(dirs$figure_root, "first_inactive_auc_contrasts.svg"), 130, 55)

p_qq <- ggplot(res$residuals, aes(theoretical_q, sample_q)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(size = 0.25, alpha = 0.3) + facet_wrap(~ Sex) +
  labs(x = "Theoretical quantile", y = "Deviance residual",
       title = "Inactive residual QQ (diagnostic; response is ~89% zeros)") +
  theme_mmm_pub()
mmm_save_pub(p_qq, file.path(diag_dir, "first_inactive_residual_qq.svg"), 130, 65)

write_output_manifest(
  output_dir, script_name = "23_first_inactive_gamm.R",
  analysis_name = "SECONDARY: first post-active Inactive-phase GAMM", bin_level = bin_level,
  key_parameters = list(window_hours = 12, phase = "Inactive", k = MMM_GAMM_K,
                        family = "2 Sex x 3 Group contrasts = 6 tests (BH), separate from Active"),
  primary_tables = c("tables/first_inactive_primary_contrasts.csv",
                     "tables/first_inactive_gamm_auc_contrasts.csv"),
  primary_figures = c("figures/first_inactive_trajectories.svg",
                      "figures/first_inactive_auc_contrasts.svg"))

cat("\nInactive six-test family:\n")
print(as.data.frame(res$primary_contrasts %>%
  dplyr::select(Sex, contrast, contrast_role, AUC_diff_log1p, AUC_diff_CI_low, AUC_diff_CI_high, p_raw, p_bh)),
  row.names = FALSE, digits = 3)
cat("\nModel adequacy (zero fraction / QQ correlation):\n")
print(as.data.frame(res$diagnostics %>%
  dplyr::select(Sex, zero_fraction, dev_expl, qq_cor, resid_skew, resid_kurtosis, rho)),
  row.names = FALSE, digits = 3)
cat("\nDone ->", output_dir, "\n")
