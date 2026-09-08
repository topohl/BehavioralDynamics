# ================================================================
# First post-active Inactive phase - Markov-binomial activation GAMM
# MMMSociability
# ================================================================
# PRIMARY ESTIMAND (replaces the retired Gaussian log1p magnitude model):
#   During the first scheduled Inactive block following the acute Active
#   response to CC1 (06:30 inclusive -> 18:30 exclusive, exactly 12 h), what is
#   the probability that a 10-min bin contains locomotor activity
#   (ActiveBin = 1{Movement > 0}), how does it evolve across the phase, and does
#   its organization differ among phenotype groups?
#
# Serial dependence is modelled EXPLICITLY as a first-order state term
# (PrevState in {START, INACTIVE, ACTIVE}); there is no AR1.
#
# The manuscript quantity is the MARGINAL probability p(t) obtained by running
# the fitted transition probabilities forward from START, not a conditional
# transition probability.
#
# Terminology: inactive-phase locomotor activation / activity-bin probability.
# NOT sleep, sleep fragmentation, circadian disruption or rest disturbance.
#
# The retired Gaussian outputs are preserved under
# audit/gaussian_log1p_invalid/ and remain stamped
# MODEL_INADEQUATE__DO_NOT_INTERPRET. They are provenance only.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble); library(mgcv)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
for (h in c("phase_classification_helpers.R", "animalpos_preprocessing_helpers.R",
            "acute_active_window_helpers.R", "acute_phase_window_helpers.R",
            "gamm_group_inference_helpers.R", "gamm_auc_helpers.R",
            "inactive_markov_gamm_helpers.R", "inactive_markov_stage_runner.R")) source_mmm_helper(h)

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"; bin_size_sec <- 600L; target_cc <- "CC1"

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "23", "first_inactive_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)
invalid_dir <- file.path(dirs$audit, "gaussian_log1p_invalid"); ensure_dir(invalid_dir)

cat("Stage 23 | first post-active Inactive Markov-binomial GAMM |", bin_level, "\n")

# ---- quarantine the retired Gaussian artefacts (move, never delete)
moved <- 0L
for (f in list.files(dirs$tables, pattern = "^first_inactive_.*\\.csv$", full.names = TRUE)) {
  nm <- basename(f)
  if (grepl("markov|activation|probability_", nm)) next
  x <- tryCatch(read_csv(f, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
  if (!is.null(x) && "inference_status" %in% names(x)) {
    file.rename(f, file.path(invalid_dir, nm)); moved <- moved + 1L
  }
}
if (moved > 0) cat("  quarantined", moved, "retired Gaussian tables ->", invalid_dir, "\n")

# ---- window
raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))
selected <- mmm_select_acute_phase_window(raw, bin_size_sec, cage_change = target_cc,
                                          phase = "Inactive")

qc <- mmm_phase_window_qc(selected, block_cols = "target_cage_change")
write_csv(qc, file.path(dirs$tables, "first_inactive_window_qc.csv"))
cat("  window: ", nrow(selected), " rows | ", n_distinct(selected$AnimalNum),
    " animals | coverage ", round(mean(qc$coverage), 4),
    " | zero fraction ", round(mean(qc$zero_fraction), 4), "\n", sep = "")

# ---- fit
res <- mmm_run_inactive_markov_stage(
  selected, block_cols = "target_cage_change", stage_id = "stage23_first_inactive",
  phase_label = "Inactive", stratum_col = NULL)

stopifnot(nrow(res$primary_contrasts) == 6L)

# ---- adequacy gate: a hard stop for manuscript use
cat("\n=== MARKOV ADEQUACY GATE ===\n")
print(as.data.frame(res$adequacy %>% dplyr::select(check, value, passed)), row.names = FALSE)
gate_status <- unique(res$adequacy$overall_status)
cat("  VERDICT:", gate_status, "\n")
res$primary_contrasts <- res$primary_contrasts %>%
  mutate(model_adequacy_status = gate_status,
         inference_status = ifelse(gate_status == "READY", "READY", "NEEDS_REVIEW"))

# ---- write
w <- function(x, f, d = dirs$tables) if (!is.null(x) && nrow(x) > 0) write_csv(x, file.path(d, f))
sex_mod <- mmm_sex_moderation(
  res$primary_contrasts %>% dplyr::select("Sex", "contrast",
    estimate = "AUC_diff_prob", se = "AUC_diff_delta_SE"),
  by_cols = "contrast") %>%
  mutate(quantity = "first_inactive_activity_probability_AUC", .before = 1)
w(sex_mod, "first_inactive_markov_sex_moderation_contrasts.csv")
w(res$primary_contrasts, "first_inactive_markov_auc_contrasts.csv")
w(res$group_auc, "first_inactive_markov_group_auc.csv")
w(res$trajectory, "first_inactive_markov_probability_trajectory.csv")
w(res$spec, "first_inactive_markov_model_specification.csv")
w(res$parametric, "first_inactive_markov_parametric_terms.csv")
w(res$observed_trans, "first_inactive_observed_transitions.csv")
w(res$adequacy, "first_inactive_markov_adequacy_gate.csv", dirs$audit)
w(res$calibration, "first_inactive_markov_calibration.csv", dirs$audit)
w(res$decile, "first_inactive_markov_decile_calibration.csv", dirs$audit)
w(res$trans_calib, "first_inactive_markov_transition_calibration.csv", dirs$audit)
w(res$resid_dep, "first_inactive_markov_residual_dependence.csv", dirs$audit)
w(res$trans_matrix, "first_inactive_markov_transition_matrices.csv", dirs$audit)
w(res$state_proof, "first_inactive_prev_state_proof.csv", dirs$audit)

# ---- descriptive positive-magnitude summary (secondary, section 21)
pos <- selected %>% filter(.data$Movement > 0) %>%
  group_by(.data$Sex, .data$Group) %>%
  summarise(n_positive_bins = dplyr::n(), median = stats::median(.data$Movement),
            q25 = stats::quantile(.data$Movement, 0.25),
            q75 = stats::quantile(.data$Movement, 0.75),
            mean = mean(.data$Movement), max = max(.data$Movement),
            frac_even = mean(.data$Movement %% 2 == 0), .groups = "drop") %>%
  mutate(quantity_class = "descriptive_only",
         note = "positive-count magnitude; even-count heaping reflects out-and-back RFID transitions")
w(pos, "first_inactive_positive_magnitude_descriptive.csv")

write_output_manifest(
  output_dir, script_name = "23_first_inactive_gamm.R",
  analysis_name = "First post-active Inactive Markov-binomial activation GAMM",
  bin_level = bin_level,
  key_parameters = list(window_hours = 12, phase = "Inactive", family = "binomial(logit)",
                        serial_model = "explicit first-order PrevState",
                        family_bh = "2 Sex x 3 Group contrasts = 6 tests"),
  primary_tables = c("tables/first_inactive_markov_auc_contrasts.csv",
                     "tables/first_inactive_markov_probability_trajectory.csv"))

cat("\n=== activity-probability AUC by group ===\n")
print(as.data.frame(res$group_auc %>%
  dplyr::select(Sex, Group, AUC_prob, AUC_prob_low, AUC_prob_high, expected_active_bin_percent)),
  row.names = FALSE, digits = 4)
cat("\n=== planned contrasts (6-test BH family) ===\n")
print(as.data.frame(res$primary_contrasts %>%
  dplyr::select(Sex, contrast, contrast_role, diff_active_bin_percent_points,
                diff_pct_pts_ci_low, diff_pct_pts_ci_high, AUC_diff_p, p_bh)),
  row.names = FALSE, digits = 3)
cat("\nDone ->", output_dir, "\n")
