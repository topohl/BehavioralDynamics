# ================================================================
# All CC1 Inactive blocks - Markov-binomial activation GAMM
# MMMSociability
# ================================================================
# Mirrors Analysis/23 with an explicit repeated stratum. Estimand:
#   Across the Inactive blocks that follow the acute Active response within CC1,
#   how does the probability of a 10-min bin containing locomotor activity
#   evolve, and does its organization differ by phenotype or across blocks?
#
# PRIMARY is parsimonious: Group*InactiveBlock + PrevState + Group:PrevState +
# InactiveBlock:PrevState, with additive Group and Block difference smooths.
# The full Group x Block x PrevState transition interaction and the full
# Group x Block trajectory-shape interaction are declared SECONDARY
# sensitivities and are not promoted.
#
# PrevState sequences restart at animal, InactiveBlock and slot gaps.
# Adaptation uses the frozen three-family architecture (A/B/C + all-14 check).
# Not sleep, not circadian disruption.
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
bin_level <- "10min_based"; bin_size_sec <- 600L; target_cc <- "CC1"

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "24", "cc1_inactive_longitudinal_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)
invalid_dir <- file.path(dirs$audit, "gaussian_log1p_invalid"); ensure_dir(invalid_dir)

cat("Stage 24 | all CC1 Inactive blocks, Markov-binomial |", bin_level, "\n")

moved <- 0L
for (f in list.files(dirs$tables, pattern = "^cc1_inactive_.*[.]csv$", full.names = TRUE)) {
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

blocks <- mmm_select_cc_phase_blocks(raw, bin_size_sec, cage_change = target_cc,
                                     phase = "Inactive") %>%
  rename(InactiveBlock = "PhaseBlock", TimeHours = "TimeWithinPhaseHours")
qc <- mmm_phase_window_qc(blocks, block_cols = "InactiveBlock",
                          start_col = "block_window_start", end_col = "block_window_end")
write_csv(qc, file.path(dirs$tables, "cc1_inactive_block_qc.csv"))
n_blocks <- n_distinct(blocks$InactiveBlock)
cat("  rows: ", nrow(blocks), " | animals: ", n_distinct(blocks$AnimalNum),
    " | Inactive blocks: ", n_blocks, " | coverage ", round(mean(qc$coverage), 4), "\n", sep = "")

res <- mmm_run_inactive_markov_stage(
  blocks, block_cols = "InactiveBlock", stage_id = "stage24_cc1_inactive",
  phase_label = "Inactive", stratum_col = "InactiveBlock")

cat("\n=== MARKOV ADEQUACY GATE ===\n")
print(as.data.frame(res$adequacy %>% dplyr::select(check, value, passed)), row.names = FALSE)
gate_status <- unique(res$adequacy$overall_status); cat("  VERDICT:", gate_status, "\n")

w <- function(x, f, d = dirs$tables) if (!is.null(x) && nrow(x) > 0) write_csv(x, file.path(d, f))
res$localization <- res$localization %>% mutate(model_adequacy_status = gate_status)
res$adaptation <- res$adaptation %>% mutate(model_adequacy_status = gate_status)
w(res$localization, "cc1_inactive_markov_auc_contrasts.csv")
w(res$localization, "cc1_inactive_markov_multiplicity_localization.csv")
w(res$adaptation, "cc1_inactive_markov_adaptation_auc_contrasts.csv")
sex_mod <- mmm_sex_moderation(
  res$adaptation %>% dplyr::select("Sex", "scope", "contrast_name", "estimate", "se"),
  by_cols = c("scope", "contrast_name")) %>%
  mutate(quantity = "cc1_inactive_adaptation_probability_AUC", .before = 1)
w(sex_mod, "cc1_inactive_markov_sex_moderation_contrasts.csv")
w(res$adaptation %>% dplyr::select(any_of(c("family_id","family_key","biological_question","phase",
    "Sex","scope","contrast_name","tests_in_family","estimate","se","ci_low","ci_high","p_raw",
    "q_BH_family","q_BH_all14_sensitivity","all14_role","family_provenance"))),
  "cc1_inactive_markov_adaptation_multiplicity_registry.csv")
w(res$group_auc, "cc1_inactive_markov_group_auc.csv")
w(res$trajectory, "cc1_inactive_markov_probability_trajectory.csv")
w(res$spec, "cc1_inactive_markov_model_specification.csv")
w(res$parametric, "cc1_inactive_markov_parametric_terms.csv")
w(res$observed_trans, "cc1_inactive_observed_transitions.csv")
w(res$adequacy, "cc1_inactive_markov_adequacy_gate.csv", dirs$audit)
w(res$calibration, "cc1_inactive_markov_calibration.csv", dirs$audit)
w(res$decile, "cc1_inactive_markov_decile_calibration.csv", dirs$audit)
w(res$trans_calib, "cc1_inactive_markov_transition_calibration.csv", dirs$audit)
w(res$resid_dep, "cc1_inactive_markov_residual_dependence.csv", dirs$audit)
w(res$trans_matrix, "cc1_inactive_markov_transition_matrices.csv", dirs$audit)
w(res$state_proof, "cc1_inactive_prev_state_proof.csv", dirs$audit)

write_output_manifest(
  output_dir, script_name = "24_cc1_inactive_longitudinal_gamm.R",
  analysis_name = "All CC1 Inactive blocks, Markov-binomial activation GAMM",
  bin_level = bin_level,
  key_parameters = list(cage_change = target_cc, n_inactive_blocks = n_blocks,
                        family = "binomial(logit)", serial_model = "explicit PrevState"),
  primary_tables = c("tables/cc1_inactive_markov_adaptation_auc_contrasts.csv",
                     "tables/cc1_inactive_markov_auc_contrasts.csv"))

cat("\n=== adaptation (last block - first block), by family ===\n")
print(as.data.frame(res$adaptation %>%
  dplyr::select(family_key, Sex, scope, estimate, ci_low, ci_high, p_raw, q_BH_family)),
  row.names = FALSE, digits = 3)
cat("\nDone ->", output_dir, "\n")
