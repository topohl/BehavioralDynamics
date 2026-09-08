# ================================================================
# Repeated post-active Inactive response across CC1..CC4 - Markov-binomial
# MMMSociability
# ================================================================
# Mirrors Analysis/24 across cage changes. Estimand:
#   Does inactive-phase locomotor activation after regrouping change across
#   successive cage changes, and does that change differ by later phenotype?
#
# ONE comparable window per cage change: the first scheduled Inactive block
# FOLLOWING that cage change's acute Active block, 06:30 -> 18:30, exactly 12 h.
# CageChange is a FACTOR so adaptation may be non-monotonic.
#
# PRIMARY is parsimonious (no three-way interaction). The full
# Group x CageChange x PrevState transition interaction and the full
# Group x CageChange trajectory-shape interaction are SECONDARY sensitivities.
#
# PrevState sequences restart at animal, CageChange and slot gaps.
# Active-vs-Inactive phase specificity would require a formal Phase interaction
# test, which this stage does not perform.
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
            "stratified_gamm_driver.R",
            "inactive_markov_gamm_helpers.R", "inactive_markov_stage_runner.R")) source_mmm_helper(h)

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"; bin_size_sec <- 600L

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "25", "repeated_cagechange_inactive_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)
invalid_dir <- file.path(dirs$audit, "gaussian_log1p_invalid"); ensure_dir(invalid_dir)

cat("Stage 25 | repeated post-active Inactive CC1-CC4, Markov-binomial |", bin_level, "\n")

moved <- 0L
for (f in list.files(dirs$tables, pattern = "^allcc_inactive_.*[.]csv$", full.names = TRUE)) {
  x <- tryCatch(read_csv(f, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
  if (!is.null(x) && "inference_status" %in% names(x) &&
      any(x$inference_status == "MODEL_INADEQUATE__DO_NOT_INTERPRET", na.rm = TRUE)) {
    file.rename(f, file.path(invalid_dir, basename(f))); moved <- moved + 1L
  }
}
if (moved > 0) cat("  quarantined", moved, "retired Gaussian tables\n")

raw <- read_csv(input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

cc_levels <- sort(unique(as.character(raw$CageChange)))
acute <- purrr::map_dfr(cc_levels, function(cc)
  mmm_select_acute_phase_window(raw, bin_size_sec, cage_change = cc, phase = "Inactive") %>%
    mutate(CageChangeWindow = cc))
qc <- mmm_phase_window_qc(acute, block_cols = "CageChangeWindow")
write_csv(qc, file.path(dirs$tables, "allcc_inactive_window_qc.csv"))
parity <- qc %>% group_by(.data$CageChangeWindow) %>%
  summarise(n_animals = n_distinct(.data$AnimalNum), n_windows = dplyr::n(),
            expected_bins = dplyr::first(.data$expected_bins),
            mean_coverage = mean(.data$coverage), mean_zero_fraction = mean(.data$zero_fraction),
            .groups = "drop")
write_csv(parity, file.path(dirs$tables, "allcc_inactive_cohort_window_parity.csv"))
print(as.data.frame(parity), row.names = FALSE, digits = 4)
stopifnot(n_distinct(acute$expected_slots) == 1L)

res <- mmm_run_inactive_markov_stage(
  acute, block_cols = "CageChangeWindow", stage_id = "stage25_allcc_inactive",
  phase_label = "Inactive", stratum_col = "CageChangeWindow")

cat("\n=== MARKOV ADEQUACY GATE ===\n")
print(as.data.frame(res$adequacy %>% dplyr::select(check, value, passed)), row.names = FALSE)
gate_status <- unique(res$adequacy$overall_status); cat("  VERDICT:", gate_status, "\n")

w <- function(x, f, d = dirs$tables) if (!is.null(x) && nrow(x) > 0) write_csv(x, file.path(d, f))
res$localization <- res$localization %>% mutate(model_adequacy_status = gate_status)
res$adaptation <- res$adaptation %>% mutate(model_adequacy_status = gate_status)
w(res$localization, "allcc_inactive_markov_auc_contrasts.csv")
w(res$localization, "allcc_inactive_markov_multiplicity_localization.csv")
w(res$adaptation, "allcc_inactive_markov_adaptation_auc_contrasts.csv")
sex_mod <- mmm_sex_moderation(
  res$adaptation %>% dplyr::select("Sex", "scope", "contrast_name", "estimate", "se"),
  by_cols = c("scope", "contrast_name")) %>%
  mutate(quantity = "allcc_inactive_adaptation_probability_AUC", .before = 1)
w(sex_mod, "allcc_inactive_markov_sex_moderation_contrasts.csv")
w(res$adaptation %>% dplyr::select(any_of(c("family_id","family_key","biological_question","phase",
    "Sex","scope","contrast_name","tests_in_family","estimate","se","ci_low","ci_high","p_raw",
    "q_BH_family","q_BH_all14_sensitivity","all14_role","family_provenance"))),
  "allcc_inactive_markov_adaptation_multiplicity_registry.csv")
w(res$group_auc, "allcc_inactive_markov_group_auc.csv")
w(res$trajectory, "allcc_inactive_markov_probability_trajectory.csv")
w(res$spec, "allcc_inactive_markov_model_specification.csv")
w(res$parametric, "allcc_inactive_markov_parametric_terms.csv")
w(res$observed_trans, "allcc_inactive_observed_transitions.csv")
w(res$adequacy, "allcc_inactive_markov_adequacy_gate.csv", dirs$audit)
w(res$calibration, "allcc_inactive_markov_calibration.csv", dirs$audit)
w(res$decile, "allcc_inactive_markov_decile_calibration.csv", dirs$audit)
w(res$trans_calib, "allcc_inactive_markov_transition_calibration.csv", dirs$audit)
w(res$resid_dep, "allcc_inactive_markov_residual_dependence.csv", dirs$audit)
w(res$trans_matrix, "allcc_inactive_markov_transition_matrices.csv", dirs$audit)
w(res$state_proof, "allcc_inactive_prev_state_proof.csv", dirs$audit)

write_output_manifest(
  output_dir, script_name = "25_repeated_cagechange_inactive_gamm.R",
  analysis_name = "Repeated post-active Inactive response CC1-CC4, Markov-binomial",
  bin_level = bin_level,
  key_parameters = list(cage_changes = paste(cc_levels, collapse = ","),
                        family = "binomial(logit)", serial_model = "explicit PrevState"),
  primary_tables = c("tables/allcc_inactive_markov_adaptation_auc_contrasts.csv",
                     "tables/allcc_inactive_markov_auc_contrasts.csv"))

cat("\n=== adaptation (CC4 - CC1), by family ===\n")
print(as.data.frame(res$adaptation %>%
  dplyr::select(family_key, Sex, scope, estimate, ci_low, ci_high, p_raw, q_BH_family)),
  row.names = FALSE, digits = 3)
cat("\nDone ->", output_dir, "\n")
