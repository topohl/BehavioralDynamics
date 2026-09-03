# ================================================================
# Manuscript Behavioral Report Exporter
# MMMSociability
# ================================================================
# EXPORT/ASSEMBLY ONLY.
#
# This script reads canonical upstream tables and assembles manuscript-facing
# CSV/XLSX artifacts. It does not fit models, calculate p-values, redo
# multiplicity corrections, or recalculate canonical statistics.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(openxlsx)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"),
  file.path(dirname(tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) getwd())), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min"
legacy_bin_level <- "10min_based"
analysis_ready_dir <- behavior_analysis_ready_dir(base_dir)
stage09_dir <- behavior_stage_dir(base_dir, "09", "early_prediction", bin_level)
legacy_stage09_dir <- file.path(
  analysis_ready_dir, "06_behavioral_dynamics", "early_prediction_model_ladder", legacy_bin_level
)
stage03_dir <- behavior_stage_dir(base_dir, "03", "movement_phase_stats", bin_level)
legacy_stage03_dir <- file.path(
  analysis_ready_dir, "03_primary_raw_movement_phase_stats", legacy_bin_level
)
qc_dir <- file.path(base_dir, "analysis_ready", "00_qc_tracking_integrity")
output_dir <- behavior_manuscript_dir(base_dir, "behavior")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

artifact_paths <- c(
  workbook = file.path(output_dir, "Behavioral_Source_Data.xlsx"),
  primary = file.path(output_dir, "primary_results.csv"),
  supplementary = file.path(output_dir, "supplementary_results.csv"),
  animal_source = file.path(output_dir, "animal_level_source_data.csv"),
  prediction_source = file.path(output_dir, "prediction_source_data.csv"),
  movement_source = file.path(output_dir, "movement_phase_source_data.csv"),
  provenance = file.path(output_dir, "provenance.csv"),
  validation = file.path(output_dir, "validation.csv"),
  manifest = file.path(output_dir, "manifest.csv")
)

canonical_features <- c("Movement_mean", "Movement_rmssd", "Entropy_acf1")
generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# Entropy_acf1 reporting contract, defined ONCE.
#
# The wording used to be "BH-supported; bootstrap CI narrowly includes zero" and
# was hard-coded in five separate places, so the manuscript text, the assembled
# registry, the audit row and two export checks could only agree by hand. That
# claim no longer matches the canonical Stage 09 result: Entropy_acf1 is
# rho = -0.175, raw p = 0.0667, BH p = 0.0667 and bootstrap CI
# [-0.351, +0.017]. It is NOT BH-supported, and the interval includes zero, so
# the association is consistently null rather than significant-but-fragile.
#
# The contract below is still a real guard: it fails if Entropy_acf1 ever
# becomes BH-significant or its bootstrap interval stops covering zero. Keeping
# the wording in one constant means all five consumers move together.
entropy_robustness_wording <- "Not BH-supported; bootstrap CI includes zero"
entropy_contract_expectation <- "BH p >= 0.05 with a bootstrap interval that includes zero"

source_registry <- tribble(
  ~source_id, ~stage, ~required, ~source_script, ~artifact, ~canonical_path, ~legacy_path, ~role, ~source_notes,
  "s09_associations", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_movement_entropyacf1_associations.csv", file.path(stage09_dir, "tables/primary_movement_entropyacf1_associations.csv"), file.path(legacy_stage09_dir, "tables/statistics/primary_movement_entropyacf1_associations.csv"), "canonical statistical result", "Three canonical feature associations.",
  "s09_prediction_performance", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_prediction_performance.csv", file.path(stage09_dir, "tables/primary_prediction_performance.csv"), file.path(legacy_stage09_dir, "tables/models/primary_prediction_performance.csv"), "canonical statistical result", "Fixed a priori primary prediction registry performance.",
  "s09_permutation", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_prediction_permutation_test.csv", file.path(stage09_dir, "tables/primary_prediction_permutation_test.csv"), file.path(legacy_stage09_dir, "tables/models/primary_prediction_permutation_test.csv"), "canonical statistical result", "Full-refit outcome permutation results.",
  "s09_sex_interactions", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_feature_sex_interactions.csv", file.path(stage09_dir, "tables/primary_feature_sex_interactions.csv"), file.path(legacy_stage09_dir, "tables/statistics/primary_feature_sex_interactions.csv"), "canonical statistical result", "Formal feature-by-Sex interactions.",
  "s09_sex_stratified", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_movement_entropyacf1_correlations_by_sex.csv", file.path(stage09_dir, "tables/primary_movement_entropyacf1_correlations_by_sex.csv"), file.path(legacy_stage09_dir, "tables/statistics/primary_movement_entropyacf1_correlations_by_sex.csv"), "supplementary statistical result", "Descriptive sex-stratified associations.",
  "s09_model_registry", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_prediction_model_registry.csv", file.path(stage09_dir, "tables/primary_prediction_model_registry.csv"), file.path(legacy_stage09_dir, "tables/documentation/primary_prediction_model_registry.csv"), "audit/provenance", "Fixed a priori model definitions.",
  "s09_feature_dictionary", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "readout_dictionary.csv", file.path(stage09_dir, "tables/readout_dictionary.csv"), file.path(legacy_stage09_dir, "tables/documentation/readout_dictionary.csv"), "audit/provenance", "Feature definitions and manuscript roles.",
  "s09_model_input", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "model_ladder_input.csv", file.path(stage09_dir, "tables/model_ladder_input.csv"), file.path(legacy_stage09_dir, "tables/models/model_ladder_input.csv"), "manuscript source data", "Canonical one-row-per-animal Stage 09 model input.",
  "s09_prediction_source", "09", TRUE, "Analysis/09_early_prediction_model_ladder.R", "primary_prediction_predictions.csv", file.path(stage09_dir, "tables/primary_prediction_predictions.csv"), file.path(legacy_stage09_dir, "tables/models/matched_ladder_loo_predictions.csv"), "manuscript source data", "Held-out predictions; legacy fallback contains the identical behavior-only models in the matched ladder.",
  "s03_animal_endpoints", "03", TRUE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_animal_level_endpoints.csv", file.path(stage03_dir, "tables/raw_movement_animal_level_endpoints.csv"), file.path(legacy_stage03_dir, "tables/raw_movement_animal_level_endpoints.csv"), "manuscript source data", "Animal-level movement observations underlying Stage 03 manuscript-facing panels.",
  "s03_pairwise", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_pairwise_wilcox_stats_corrected.csv", file.path(stage03_dir, "tables/raw_movement_pairwise_wilcox_stats_corrected.csv"), file.path(legacy_stage03_dir, "stats_tables/raw_movement_pairwise_wilcox_stats_corrected.csv"), "supplementary statistical result", "Planned Holm-adjusted pairwise results.",
  "s03_group_summary", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_group_summary.csv", file.path(stage03_dir, "tables/raw_movement_group_summary.csv"), file.path(legacy_stage03_dir, "tables/raw_movement_group_summary.csv"), "supplementary statistical result", "Descriptive group summaries.",
  "s03_lm", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_one_way_lm_stats_corrected.csv", file.path(stage03_dir, "tables/raw_movement_one_way_lm_stats_corrected.csv"), file.path(legacy_stage03_dir, "stats_tables/raw_movement_one_way_lm_stats_corrected.csv"), "supplementary statistical result", "Secondary one-way models.",
  "s03_lmm", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_repeated_lmm_cagechange_phase_by_sex.csv", file.path(stage03_dir, "tables/raw_movement_repeated_lmm_cagechange_phase_by_sex.csv"), file.path(legacy_stage03_dir, "stats_tables/raw_movement_repeated_lmm_cagechange_phase_by_sex.csv"), "supplementary statistical result", "Repeated-measures models.",
  "s03_correlations_phase", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_combz_correlations_by_sex_phase.csv", file.path(stage03_dir, "tables/raw_movement_combz_correlations_by_sex_phase.csv"), file.path(legacy_stage03_dir, "stats_tables/raw_movement_combz_correlations_by_sex_phase.csv"), "supplementary statistical result", "Descriptive phase associations.",
  "s03_correlations_cagechange", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_combz_correlations_cagechange_phase.csv", file.path(stage03_dir, "tables/raw_movement_combz_correlations_cagechange_phase.csv"), file.path(legacy_stage03_dir, "stats_tables/raw_movement_combz_correlations_cagechange_phase.csv"), "supplementary statistical result", "Descriptive cage-change/phase associations.",
  "s03_filter_qc", "03", FALSE, "Analysis/03_primary_raw_movement_phase_stats.R", "raw_movement_phase_filter_qc.csv", file.path(stage03_dir, "tables/raw_movement_phase_filter_qc.csv"), file.path(legacy_stage03_dir, "tables/raw_movement_phase_filter_qc.csv"), "diagnostic/QC", "Minimum-bin retention counts.",
  "qc_animal_summary", "00", FALSE, "Analysis/00_qc_tracking_integrity.R", "tracking_qc_by_animal.csv", file.path(qc_dir, "tables/tracking_qc_by_animal.csv"), NA_character_, "diagnostic/QC", "Tracking integrity summary.",
  "qc_manual_review", "00", FALSE, "Analysis/00_qc_tracking_integrity.R", "suggested_animals_for_manual_tracking_review.csv", file.path(qc_dir, "tables/suggested_animals_for_manual_tracking_review.csv"), NA_character_, "diagnostic/QC", "Manual-review suggestions only."
) %>%
  mutate(
    resolved = pmap(
      list(canonical_path, legacy_path, required, source_id),
      ~ resolve_behavior_artifact(..1, ..2, required = ..3, source_id = ..4)
    ),
    path = map_chr(resolved, "path"),
    path_resolution = map_chr(resolved, "resolution"),
    exists = map_lgl(resolved, "exists")
  ) %>%
  select(-resolved) %>%
  mutate(
    status = case_when(
      exists ~ "available",
      required ~ "missing_required",
      TRUE ~ "missing_optional"
    )
  )

legacy_sources_used <- source_registry %>% filter(path_resolution == "legacy_fallback")
if (nrow(legacy_sources_used) > 0L) {
  warning(
    "Stage 16 used documented legacy fallback paths for: ",
    paste(legacy_sources_used$source_id, collapse = ", "),
    ". Canonical paths remain preferred and the fallback is recorded in provenance.",
    call. = FALSE
  )
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
source_hash_before <- setNames(
  map_chr(source_registry$path, sha256_file),
  source_registry$source_id
)

missing_core <- source_registry %>% filter(required, !exists)
if (nrow(missing_core) > 0L) {
  stop(
    "Stage 16 aborted: required canonical Stage 09 source(s) are absent:\n",
    paste0("- ", missing_core$path, collapse = "\n"),
    call. = FALSE
  )
}

read_source <- function(source_id) {
  row <- source_registry %>% filter(.data$source_id == !!source_id)
  if (nrow(row) != 1L) stop("Unknown source_id: ", source_id, call. = FALSE)
  if (!row$exists[[1]]) return(tibble())
  readr::read_csv(row$path[[1]], show_col_types = FALSE)
}

na_num <- NA_real_
na_int <- NA_integer_
na_chr <- NA_character_

audit_columns <- c(
  "claim_id", "reporting_role", "analysis_domain", "endpoint", "time_window",
  "bin_level", "biological_unit", "n_animals", "sex", "contrast_or_model",
  "estimate", "effect_size_type", "effect_size", "ci_low", "ci_high",
  "p_raw", "p_adjusted", "adjustment_method", "multiplicity_family",
  "validation_scheme", "robustness_status", "source_script", "source_table", "notes"
)

extra_columns <- c(
  "source_row_key", "model_id", "model_label", "predictors", "rmse", "mae",
  "pearson_r", "spearman_rho", "repeated_cv_mean_r2", "repeated_cv_mean_rmse",
  "repeated_cv_mean_mae", "cv_r2_q025", "cv_r2_q975", "interval_type",
  "permutation_numerator", "permutation_denominator", "permutation_display"
)

conform_results <- function(dat) {
  all_cols <- c(audit_columns, extra_columns)
  missing_cols <- setdiff(all_cols, names(dat))
  integer_fields <- c("n_animals", "permutation_numerator", "permutation_denominator")
  numeric_fields <- c(
    "estimate", "effect_size", "ci_low", "ci_high", "p_raw", "p_adjusted",
    "rmse", "mae", "pearson_r", "spearman_rho", "repeated_cv_mean_r2",
    "repeated_cv_mean_rmse", "repeated_cv_mean_mae", "cv_r2_q025", "cv_r2_q975"
  )
  for (nm in missing_cols) {
    dat[[nm]] <- if (nm %in% integer_fields) {
      NA_integer_
    } else if (nm %in% numeric_fields) {
      NA_real_
    } else {
      NA_character_
    }
  }
  dat %>% select(all_of(all_cols))
}

assoc <- read_source("s09_associations")
performance <- read_source("s09_prediction_performance")
permutation <- read_source("s09_permutation")
sex_interactions <- read_source("s09_sex_interactions")
sex_stratified <- read_source("s09_sex_stratified")
model_registry <- read_source("s09_model_registry")
feature_dictionary_raw <- read_source("s09_feature_dictionary")
model_input <- read_source("s09_model_input")
prediction_source_raw <- read_source("s09_prediction_source")
stage03_animal_endpoints <- read_source("s03_animal_endpoints")
stage03_identity_conflicts <- stage03_animal_endpoints %>%
  distinct(AnimalNum, Sex, Group) %>%
  group_by(AnimalNum) %>%
  summarise(
    n_groups = n_distinct(Group, na.rm = TRUE),
    n_sexes = n_distinct(Sex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_groups > 1L | n_sexes > 1L)
if (nrow(stage03_identity_conflicts) > 0L) {
  stop("Stage 16 refuses to export contradictory Stage 03 animal Group/Sex metadata.", call. = FALSE)
}

required_model_input_columns <- c(
  "AnimalNum", "Sex", "Group", "outcome", canonical_features,
  "early_observation_hours", "contains_short_duration_epoch"
)
missing_model_input_columns <- setdiff(required_model_input_columns, names(model_input))
if (length(missing_model_input_columns) > 0L) {
  stop(
    "Stage 09 model input is missing manuscript source-data columns: ",
    paste(missing_model_input_columns, collapse = ", "),
    call. = FALSE
  )
}

animal_level_source_data <- model_input %>%
  transmute(
    AnimalID = as.character(AnimalNum),
    Sex = as.character(Sex),
    Group = as.character(Group),
    CombZ = outcome,
    Movement_mean,
    Movement_rmssd,
    Entropy_acf1,
    early_observation_hours,
    contains_short_duration_epoch,
    source_id = "s09_model_input"
  ) %>%
  arrange(AnimalID)
if (anyDuplicated(animal_level_source_data$AnimalID)) {
  stop("Primary animal-level source data must contain exactly one row per animal.", call. = FALSE)
}

if ("AdjustmentSet" %in% names(prediction_source_raw)) {
  prediction_source_data <- prediction_source_raw %>%
    filter(
      AdjustmentSet == "Behavior only",
      ModelFamily %in% c("Movement mean", "Primary behavior family")
    ) %>%
    transmute(
      AnimalID = as.character(AnimalNum),
      observed_CombZ = observed,
      predicted_CombZ = predicted,
      model_id = recode(
        ModelFamily,
        "Movement mean" = "movement_mean",
        "Primary behavior family" = "primary_behavior_family"
      ),
      validation_scheme = "Leave-one-animal-out",
      Sex = as.character(Sex),
      Group = as.character(Group),
      source_id = "s09_prediction_source"
    )
} else {
  prediction_source_data <- prediction_source_raw %>%
    filter(Model %in% c("movement_mean", "primary_behavior_family")) %>%
    transmute(
      AnimalID = as.character(AnimalNum),
      observed_CombZ = observed,
      predicted_CombZ = predicted,
      model_id = Model,
      validation_scheme = "Leave-one-animal-out",
      Sex = as.character(Sex),
      Group = as.character(Group),
      source_id = "s09_prediction_source"
    )
}
prediction_source_data <- prediction_source_data %>% arrange(model_id, AnimalID)
if (!setequal(unique(prediction_source_data$model_id), c("movement_mean", "primary_behavior_family")) ||
    anyDuplicated(prediction_source_data[c("AnimalID", "model_id")])) {
  stop("Prediction source data must uniquely cover the headline and plotted canonical models.", call. = FALSE)
}

required_stage03_source_columns <- c(
  "AnimalNum", "Sex", "Group", "CageChange", "PhaseClass",
  "mean_movement", "n_bins", "ScopeType", "Endpoint"
)
missing_stage03_source_columns <- setdiff(required_stage03_source_columns, names(stage03_animal_endpoints))
if (length(missing_stage03_source_columns) > 0L) {
  stop(
    "Stage 03 animal-level source is missing columns: ",
    paste(missing_stage03_source_columns, collapse = ", "),
    call. = FALSE
  )
}
movement_phase_source_data <- stage03_animal_endpoints %>%
  transmute(
    AnimalID = as.character(AnimalNum),
    Sex = as.character(Sex),
    Group = as.character(Group),
    cage_change = as.character(CageChange),
    phase = as.character(PhaseClass),
    movement_measure = "mean_movement",
    value = mean_movement,
    n_bins,
    scope_type = ScopeType,
    endpoint = Endpoint,
    source_id = "s03_animal_endpoints"
  ) %>%
  arrange(scope_type, endpoint, Sex, Group, AnimalID)

# Fail-closed identity checks for the canonical manuscript registry.
if (!setequal(assoc$feature, canonical_features) || length(unique(assoc$feature)) != 3L) {
  stop("Canonical Stage 09 association feature set is not exactly Movement_mean, Movement_rmssd, Entropy_acf1.", call. = FALSE)
}
if (!setequal(sex_interactions$feature, canonical_features) || length(unique(sex_interactions$feature)) != 3L) {
  stop("Canonical Stage 09 sex-interaction feature set does not match the three primary features.", call. = FALSE)
}
dictionary_primary <- feature_dictionary_raw %>%
  filter(str_detect(manuscript_role, regex("^Primary", ignore_case = TRUE))) %>%
  pull(readout)
if (!setequal(dictionary_primary, canonical_features) || length(unique(dictionary_primary)) != 3L) {
  stop("Stage 09 feature dictionary does not identify exactly the canonical three primary features.", call. = FALSE)
}
if ("Movement_x_EntropyACF1" %in% dictionary_primary) {
  stop("Movement_x_EntropyACF1 must not be a primary manuscript feature.", call. = FALSE)
}

intended_models <- c(
  "movement_mean", "primary_behavior_family",
  "movement_mean_sex", "primary_behavior_family_sex"
)
missing_models <- setdiff(intended_models, performance$model_id)
if (length(missing_models) > 0L) {
  stop("Missing canonical Stage 09 prediction model(s): ", paste(missing_models, collapse = ", "), call. = FALSE)
}
primary_model_registry <- model_registry %>% filter(model_id %in% intended_models)
if (nrow(primary_model_registry) != length(intended_models)) {
  stop("Primary manuscript prediction registry contains missing or duplicate model rows.", call. = FALSE)
}
if (any(str_detect(coalesce(primary_model_registry$predictors, ""), regex("(^|[^A-Za-z])Group([^A-Za-z]|$)")))) {
  stop("A primary manuscript prediction model contains Group.", call. = FALSE)
}

permutation_presentation <- permutation %>%
  mutate(
    permutation_numerator = as.integer(round(empirical_p * (n_permutations + 1L))),
    permutation_denominator = as.integer(n_permutations + 1L),
    permutation_display = paste0(permutation_numerator, "/", permutation_denominator),
    decoded_p = permutation_numerator / permutation_denominator
  )
if (any(abs(permutation_presentation$decoded_p - permutation_presentation$empirical_p) > 1e-12)) {
  stop("Could not preserve an exact empirical permutation numerator/denominator from the upstream p value.", call. = FALSE)
}

performance_joined <- performance %>%
  left_join(
    permutation_presentation %>%
      select(
        model, observed_statistic_name, observed_statistic, null_median,
        null_q025, null_q975, empirical_p, n_permutations, cv_scheme, seed,
        permutation_numerator, permutation_denominator, permutation_display
      ),
    by = c("model_id" = "model")
  )

permuted_models <- performance_joined %>% filter(!is.na(empirical_p))
if (any(abs(permuted_models$cv_r2 - permuted_models$observed_statistic) > 1e-12) ||
    any(abs(permuted_models$permutation_p - permuted_models$empirical_p) > 1e-12)) {
  stop("Stage 09 performance and full-refit permutation tables disagree.", call. = FALSE)
}

feature_labels <- feature_dictionary_raw %>%
  filter(readout %in% canonical_features) %>%
  select(feature = readout, feature_label = display_label)

entropy_assoc_source <- assoc %>% filter(feature == "Entropy_acf1")
if (
  nrow(entropy_assoc_source) != 1L ||
  !is.finite(entropy_assoc_source$spearman_p_bh) || entropy_assoc_source$spearman_p_bh < 0.05 ||
  !is.finite(entropy_assoc_source$spearman_boot_ci_low) || entropy_assoc_source$spearman_boot_ci_low > 0 ||
  !is.finite(entropy_assoc_source$spearman_boot_ci_high) || entropy_assoc_source$spearman_boot_ci_high < 0
) {
  stop(
    "Entropy_acf1 reporting contract changed: expected ", entropy_contract_expectation,
    ". Observed BH p = ",
    if (nrow(entropy_assoc_source) == 1L) signif(entropy_assoc_source$spearman_p_bh, 4) else NA,
    ", bootstrap CI [",
    if (nrow(entropy_assoc_source) == 1L) signif(entropy_assoc_source$spearman_boot_ci_low, 4) else NA, ", ",
    if (nrow(entropy_assoc_source) == 1L) signif(entropy_assoc_source$spearman_boot_ci_high, 4) else NA,
    "]. If the canonical Stage 09 result genuinely changed, update ",
    "entropy_robustness_wording and entropy_contract_expectation together.",
    call. = FALSE
  )
}

primary_associations <- assoc %>%
  left_join(feature_labels, by = "feature") %>%
  transmute(
    claim_id = paste0("S09_ASSOC_", feature),
    reporting_role = "PRIMARY",
    analysis_domain = "Prospective feature association",
    endpoint = Outcome,
    time_window = "First active 12 h after first cage change",
    bin_level = BinLevel,
    biological_unit = "Animal",
    n_animals = as.integer(n),
    sex = "Pooled",
    contrast_or_model = paste0(feature_label, " association with later CombZ"),
    estimate = spearman_rho,
    effect_size_type = "Spearman rho",
    effect_size = spearman_rho,
    ci_low = spearman_boot_ci_low,
    ci_high = spearman_boot_ci_high,
    p_raw = spearman_p,
    p_adjusted = spearman_p_bh,
    adjustment_method = "Benjamini-Hochberg",
    multiplicity_family = "Three canonical primary feature associations",
    validation_scheme = "Animal-level association; not cross-validated",
    robustness_status = case_when(
      feature == "Entropy_acf1" ~ entropy_robustness_wording,
      TRUE ~ Evidence
    ),
    source_script = "Analysis/09_early_prediction_model_ladder.R",
    source_table = "tables/primary_movement_entropyacf1_associations.csv",
    notes = case_when(
      feature == "Movement_mean" ~ "Primary prospective predictor association.",
      TRUE ~ "Supporting association feature; not demonstrated as an independent predictive contributor."
    ),
    source_row_key = feature,
    spearman_rho = spearman_rho
  ) %>%
  conform_results()

primary_predictions <- performance_joined %>%
  filter(model_id %in% intended_models) %>%
  transmute(
    claim_id = paste0("S09_PRED_", model_id),
    reporting_role = "PRIMARY",
    analysis_domain = "Prospective prediction",
    endpoint = "CombZ",
    time_window = "First active 12 h after first cage change",
    bin_level = bin_level,
    biological_unit = "Animal",
    n_animals = as.integer(n_animals),
    sex = if_else(str_detect(model_id, "_sex$"), "Pooled; Sex-adjusted", "Pooled"),
    contrast_or_model = model_label,
    estimate = cv_r2,
    effect_size_type = "LOAO out-of-sample R2 versus training-fold mean",
    effect_size = cv_r2,
    ci_low = na_num,
    ci_high = na_num,
    p_raw = empirical_p,
    p_adjusted = na_num,
    adjustment_method = if_else(is.na(empirical_p), "Not applicable", "None; empirical full-refit outcome permutation"),
    multiplicity_family = if_else(is.na(empirical_p), "Not applicable", "Model-specific full-refit outcome permutation"),
    validation_scheme = validation_scheme,
    robustness_status = paste0(
      "Repeated grouped 5-fold CV mean R2 = ", format(repeated_cv_mean_r2, digits = 16, trim = TRUE),
      "; 2.5-97.5% resampling quantiles = [",
      format(cv_r2_q025, digits = 16, trim = TRUE), ", ",
      format(cv_r2_q975, digits = 16, trim = TRUE), "]"
    ),
    source_script = "Analysis/09_early_prediction_model_ladder.R",
    source_table = "tables/models/primary_prediction_performance.csv",
    notes = case_when(
      model_id == "movement_mean" ~ paste0(
        "Main prospective prediction. Full-refit outcome permutation empirical p = ", permutation_display,
        " (approximately ", format(empirical_p, digits = 4, trim = TRUE), "); internal validation, not external validation."
      ),
      model_id == "primary_behavior_family" ~ paste0(
        "Adding Movement_rmssd and Entropy_acf1 did not improve the point-estimate out-of-sample R2 relative to Movement_mean (",
        format(cv_r2, digits = 16, trim = TRUE), " versus ",
        format(performance_joined$cv_r2[performance_joined$model_id == "movement_mean"], digits = 16, trim = TRUE),
        "); no model-comparison significance test was performed. Full-refit permutation p = ", permutation_display, "."
      ),
      TRUE ~ "Sex-adjusted prediction sensitivity; no full-refit permutation was specified for this sensitivity model."
    ),
    source_row_key = model_id,
    model_id = model_id,
    model_label = model_label,
    predictors = predictors,
    rmse = rmse,
    mae = mae,
    pearson_r = pearson_r,
    spearman_rho = spearman_rho,
    repeated_cv_mean_r2 = repeated_cv_mean_r2,
    repeated_cv_mean_rmse = repeated_cv_mean_rmse,
    repeated_cv_mean_mae = repeated_cv_mean_mae,
    cv_r2_q025 = cv_r2_q025,
    cv_r2_q975 = cv_r2_q975,
    interval_type = interval_type,
    permutation_numerator = permutation_numerator,
    permutation_denominator = permutation_denominator,
    permutation_display = permutation_display
  ) %>%
  conform_results()

primary_interactions <- sex_interactions %>%
  left_join(feature_labels, by = "feature") %>%
  transmute(
    claim_id = paste0("S09_SEXINT_", feature),
    reporting_role = "PRIMARY",
    analysis_domain = "Formal feature-by-Sex interaction",
    endpoint = "CombZ",
    time_window = "First active 12 h after first cage change",
    bin_level = bin_level,
    biological_unit = "Animal",
    n_animals = as.integer(n),
    sex = "Pooled interaction test",
    contrast_or_model = paste0("CombZ ~ ", feature, " * Sex"),
    estimate = interaction_estimate,
    effect_size_type = "Feature-by-Sex interaction coefficient",
    effect_size = interaction_estimate,
    ci_low = interaction_ci_low,
    ci_high = interaction_ci_high,
    p_raw = interaction_p,
    p_adjusted = interaction_p_bh,
    adjustment_method = "Benjamini-Hochberg",
    multiplicity_family = interaction_test_family,
    validation_scheme = "Pooled linear-model interaction test",
    robustness_status = "No evidence that the association differed by sex.",
    source_script = "Analysis/09_early_prediction_model_ladder.R",
    source_table = "tables/statistics/primary_feature_sex_interactions.csv",
    notes = "Sex-stratified associations are descriptive estimates and are not interaction tests.",
    source_row_key = feature
  ) %>%
  conform_results()

primary_results <- bind_rows(
  primary_associations,
  primary_predictions,
  primary_interactions
) %>%
  arrange(match(claim_id, c(
    paste0("S09_ASSOC_", canonical_features),
    paste0("S09_PRED_", intended_models),
    paste0("S09_SEXINT_", canonical_features)
  )))

# Prediction-validation sheet: a direct, non-inferential presentation of all
# five fixed manuscript-facing performance rows, including the mean-only reference.
prediction_validation <- performance_joined %>%
  transmute(
    model_id, model_label, reporting_role, predictors, n_animals,
    validation_scheme,
    loao_cv_r2 = cv_r2,
    loao_rmse = rmse,
    loao_mae = mae,
    loao_pearson_r = if_else(model_id == "mean_only", NA_real_, pearson_r),
    loao_spearman_rho = if_else(model_id == "mean_only", NA_real_, spearman_rho),
    repeated_5fold_mean_r2 = repeated_cv_mean_r2,
    repeated_5fold_mean_rmse = repeated_cv_mean_rmse,
    repeated_5fold_mean_mae = repeated_cv_mean_mae,
    cv_r2_q025, cv_r2_q975, interval_type,
    permutation_scheme = cv_scheme,
    permutation_observed_statistic = observed_statistic,
    permutation_null_median = null_median,
    permutation_null_q025 = null_q025,
    permutation_null_q975 = null_q975,
    permutation_empirical_p = empirical_p,
    permutation_n = n_permutations,
    permutation_seed = seed,
    permutation_numerator,
    permutation_denominator,
    permutation_display,
    interpretation = case_when(
      model_id == "mean_only" ~ paste0(
        "Intercept-only reference; LOAO Pearson/Spearman correlations are not interpretable because each prediction ",
        "is the mean of all outcomes except the held-out animal. R2, RMSE, and MAE are retained."
      ),
      model_id == "movement_mean" ~ "Main prospective prediction; internal validation, not external validation.",
      model_id == "primary_behavior_family" ~ "Canonical three-feature supporting model; point-estimate R2 did not improve relative to Movement_mean; no model-comparison test was performed.",
      TRUE ~ "Sex-adjusted sensitivity analysis."
    )
  )

supplementary_parts <- list()

supplementary_parts$sex_stratified <- sex_stratified %>%
  transmute(
    claim_id = paste0("S09_SEXSTRAT_", Sex, "_", feature),
    reporting_role = "SECONDARY",
    analysis_domain = "Descriptive sex-stratified feature association",
    endpoint = "CombZ",
    time_window = "First active 12 h after first cage change",
    bin_level = bin_level,
    biological_unit = "Animal",
    n_animals = as.integer(n),
    sex = as.character(Sex),
    contrast_or_model = feature_label,
    estimate = spearman_rho,
    effect_size_type = "Spearman rho",
    effect_size = spearman_rho,
    ci_low = na_num,
    ci_high = na_num,
    p_raw = spearman_p,
    p_adjusted = spearman_p_bh_within_sex,
    adjustment_method = "Benjamini-Hochberg within sex",
    multiplicity_family = "Three canonical features within each sex",
    validation_scheme = "Descriptive stratified association; not an interaction test",
    robustness_status = "Formal pooled feature-by-Sex interaction is the inferential test.",
    source_script = "Analysis/09_early_prediction_model_ladder.R",
    source_table = "tables/statistics/primary_movement_entropyacf1_correlations_by_sex.csv",
    notes = CorrelationUse,
    source_row_key = paste(Sex, feature, sep = "|")
  ) %>%
  conform_results()

s03_pairwise <- read_source("s03_pairwise")
if (nrow(s03_pairwise) > 0L) {
  supplementary_parts$s03_pairwise <- s03_pairwise %>%
    transmute(
      claim_id = paste0("S03_WILCOX_", row_number()),
      reporting_role = "SECONDARY",
      analysis_domain = "Stage 03 phenotype/group characterization: pairwise Wilcoxon",
      endpoint = as.character(Endpoint),
      time_window = if_else(ScopeType == "cage_change_by_phase", paste(CageChange, PhaseClass), as.character(Endpoint)),
      bin_level = bin_level,
      biological_unit = "Animal",
      n_animals = as.integer(n1 + n2),
      sex = as.character(Sex),
      contrast_or_model = as.character(contrast),
      estimate = estimate_diff,
      effect_size_type = "Difference in group mean movement (group2 - group1); Wilcoxon p value",
      effect_size = estimate_diff,
      ci_low = na_num,
      ci_high = na_num,
      p_raw = p_raw,
      p_adjusted = p_holm_panel,
      adjustment_method = "Holm within prespecified three-contrast panel",
      multiplicity_family = paste(ScopeType, Endpoint, Sex, CageChange, PhaseClass, sep = " | "),
      validation_scheme = "Secondary/descriptive Stage 03 scan",
      robustness_status = "Panel-wise Holm adjustment; no global family-wise correction exported.",
      source_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      source_table = "stats_tables/raw_movement_pairwise_wilcox_stats_corrected.csv",
      notes = "Exact p values retained; no p<0.10 trend or dagger annotation.",
      source_row_key = paste(ScopeType, Endpoint, Sex, CageChange, PhaseClass, contrast, sep = "|")
    ) %>%
    conform_results()
}

s03_group_summary <- read_source("s03_group_summary")
if (nrow(s03_group_summary) > 0L) {
  supplementary_parts$s03_group_summary <- s03_group_summary %>%
    transmute(
      claim_id = paste0("S03_GROUPMEAN_", row_number()),
      reporting_role = "SECONDARY",
      analysis_domain = "Stage 03 group distribution summary",
      endpoint = as.character(Endpoint),
      time_window = if_else(ScopeType == "cage_change_by_phase", paste(CageChange, PhaseClass), as.character(Endpoint)),
      bin_level = bin_level,
      biological_unit = "Animal",
      n_animals = as.integer(n_animals),
      sex = as.character(Sex),
      contrast_or_model = as.character(Group),
      estimate = mean_movement,
      effect_size_type = "Group mean movement",
      effect_size = mean_movement,
      ci_low = ci95_low,
      ci_high = ci95_high,
      p_raw = na_num,
      p_adjusted = na_num,
      adjustment_method = "Not applicable",
      multiplicity_family = "Not applicable",
      validation_scheme = "Descriptive animal-level summary",
      robustness_status = "95% mean interval uses the group-specific t critical value.",
      source_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      source_table = "tables/raw_movement_group_summary.csv",
      notes = "No p value is associated with a descriptive group mean row.",
      source_row_key = paste(ScopeType, Endpoint, Sex, Group, CageChange, PhaseClass, sep = "|")
    ) %>%
    conform_results()
}

s03_lm <- read_source("s03_lm")
if (nrow(s03_lm) > 0L) {
  supplementary_parts$s03_lm <- s03_lm %>%
    transmute(
      claim_id = paste0("S03_LM_", row_number()),
      reporting_role = "SECONDARY",
      analysis_domain = "Stage 03 one-way LM",
      endpoint = as.character(Endpoint),
      time_window = if_else(ScopeType == "cage_change_by_phase", paste(CageChange, PhaseClass), as.character(Endpoint)),
      bin_level = bin_level,
      biological_unit = "Animal",
      n_animals = na_int,
      sex = as.character(Sex),
      contrast_or_model = test,
      estimate = na_num,
      effect_size_type = "Omnibus Group F-test p value; coefficient not exported upstream",
      effect_size = na_num,
      ci_low = na_num,
      ci_high = na_num,
      p_raw = p_raw,
      p_adjusted = p_holm_panel,
      adjustment_method = "Holm within the exported panel grouping",
      multiplicity_family = paste(ScopeType, Endpoint, Sex, CageChange, PhaseClass, sep = " | "),
      validation_scheme = "Secondary Stage 03 omnibus model",
      robustness_status = "Preserved upstream LM output.",
      source_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      source_table = "stats_tables/raw_movement_one_way_lm_stats_corrected.csv",
      notes = "n and coefficient estimates are unavailable in the upstream LM table.",
      source_row_key = paste(ScopeType, Endpoint, Sex, CageChange, PhaseClass, test, sep = "|")
    ) %>%
    conform_results()
}

s03_lmm <- read_source("s03_lmm")
if (nrow(s03_lmm) > 0L) {
  p_col <- intersect(c("Pr(>F)", "p.value"), names(s03_lmm))[1]
  if (is.na(p_col)) stop("Stage 03 LMM table has no recognizable p-value column.", call. = FALSE)
  supplementary_parts$s03_lmm <- s03_lmm %>%
    mutate(.p_value = .data[[p_col]]) %>%
    transmute(
      claim_id = paste0("S03_LMM_", row_number()),
      reporting_role = "SECONDARY",
      analysis_domain = "Stage 03 repeated-measures LMM",
      endpoint = "Mean movement",
      time_window = "All cage-change-by-phase epochs",
      bin_level = bin_level,
      biological_unit = "Animal (random intercept)",
      n_animals = na_int,
      sex = as.character(Sex),
      contrast_or_model = term,
      estimate = na_num,
      effect_size_type = "Repeated-measures LMM omnibus term p value",
      effect_size = na_num,
      ci_low = na_num,
      ci_high = na_num,
      p_raw = .p_value,
      p_adjusted = na_num,
      adjustment_method = "None in upstream LMM table",
      multiplicity_family = "Not adjusted in upstream LMM table",
      validation_scheme = "log1p(mean_movement) ~ Group * PhaseClass * CageChange + (1 | AnimalNum), by Sex",
      robustness_status = "Preserved upstream repeated-measures LMM output.",
      source_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      source_table = "stats_tables/raw_movement_repeated_lmm_cagechange_phase_by_sex.csv",
      notes = "n and coefficient estimates are unavailable in the upstream LMM table.",
      source_row_key = paste(Sex, term, sep = "|")
    ) %>%
    conform_results()
}

map_stage03_correlations <- function(dat, source_table, prefix, time_window_fun) {
  if (nrow(dat) == 0L) return(tibble())
  mapped <- bind_rows(
    dat %>% mutate(.method = "Pearson r", .estimate = pearson_r, .p_raw = pearson_p, .p_bh = pearson_p_bh),
    dat %>% mutate(.method = "Spearman rho", .estimate = spearman_rho, .p_raw = spearman_p, .p_bh = spearman_p_bh)
  ) %>%
    mutate(.row_id = row_number())
  mapped$.time_window <- time_window_fun(mapped)
  mapped %>%
    transmute(
      claim_id = paste0(prefix, .row_id),
      reporting_role = "SECONDARY",
      analysis_domain = "Stage 03 descriptive movement-CombZ association",
      endpoint = "CombZ",
      time_window = .time_window,
      bin_level = bin_level,
      biological_unit = "Animal",
      n_animals = as.integer(n_animals),
      sex = as.character(Sex),
      contrast_or_model = paste(.method, "for", Endpoint),
      estimate = .estimate,
      effect_size_type = .method,
      effect_size = .estimate,
      ci_low = na_num,
      ci_high = na_num,
      p_raw = .p_raw,
      p_adjusted = .p_bh,
      adjustment_method = "Benjamini-Hochberg as exported upstream",
      multiplicity_family = "All rows of the corresponding Stage 03 correlation table",
      validation_scheme = "Descriptive association; not prospective prediction",
      robustness_status = "Secondary/descriptive Stage 03 scan.",
      source_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      source_table = source_table,
      notes = if_else(is.na(skipped_reason), "Group shown for characterization only.", paste0("Skipped upstream: ", skipped_reason)),
      source_row_key = paste(Sex, Endpoint, .method, sep = "|")
    ) %>%
    conform_results()
}

s03_cor_phase <- read_source("s03_correlations_phase")
supplementary_parts$s03_cor_phase <- map_stage03_correlations(
  s03_cor_phase,
  "stats_tables/raw_movement_combz_correlations_by_sex_phase.csv",
  "S03_COR_PHASE_",
  function(dat) as.character(dat$Endpoint)
)
s03_cor_cc <- read_source("s03_correlations_cagechange")
supplementary_parts$s03_cor_cc <- map_stage03_correlations(
  s03_cor_cc,
  "stats_tables/raw_movement_combz_correlations_cagechange_phase.csv",
  "S03_COR_CC_",
  function(dat) paste(dat$CageChange, dat$PhaseClass)
)

supplementary_results <- bind_rows(supplementary_parts) %>%
  arrange(analysis_domain, claim_id)

qc_summary <- read_source("qc_animal_summary")
qc_review <- read_source("qc_manual_review")
stage03_filter_qc <- read_source("s03_filter_qc")
if (nrow(qc_summary) > 0L) {
  if (nrow(qc_review) > 0L) {
    review_keys <- intersect(c("AnimalID", "Sex", "Group"), names(qc_review))
    qc_summary <- qc_summary %>%
      left_join(
        qc_review %>% select(all_of(review_keys), any_of("comment")),
        by = review_keys
      )
  }
  qc_animal_rows <- qc_summary %>%
    mutate(
      qc_record_type = "Stage 00 animal-level tracking QC",
      qc_reporting_status = "Diagnostic/manual-review status only; no automatic exclusion decision is encoded by Stage 00.",
      source_table = "analysis_ready/00_qc_tracking_integrity/tables/tracking_qc_by_animal.csv"
    )
} else if (nrow(qc_review) > 0L) {
  qc_animal_rows <- qc_review %>%
    mutate(
      qc_record_type = "Stage 00 suggested manual tracking review",
      qc_reporting_status = "Suggested manual review only; not an automatic exclusion decision.",
      source_table = "analysis_ready/00_qc_tracking_integrity/tables/suggested_animals_for_manual_tracking_review.csv"
    )
} else {
  qc_animal_rows <- tibble(
    qc_record_type = "Stage 00 animal-level tracking QC",
    status = "missing_optional",
    reason = "No Stage 00 animal-level QC source was available; no exclusions were fabricated.",
    qc_reporting_status = "Unavailable",
    source_table = "analysis_ready/00_qc_tracking_integrity/tables/tracking_qc_by_animal.csv"
  )
}
if (nrow(stage03_filter_qc) > 0L) {
  qc_filter_rows <- stage03_filter_qc %>%
    mutate(
      qc_record_type = "Stage 03 phase-filter retention QC",
      qc_reporting_status = "Counts before and after the predefined minimum-bin filter; not an animal exclusion decision.",
      source_table = "analysis_ready/03_primary_raw_movement_phase_stats/10min_based/tables/raw_movement_phase_filter_qc.csv"
    )
} else {
  qc_filter_rows <- tibble(
    qc_record_type = "Stage 03 phase-filter retention QC",
    status = "missing_optional",
    reason = "Stage 03 phase-filter QC table was unavailable; no counts were fabricated.",
    qc_reporting_status = "Unavailable",
    source_table = "analysis_ready/03_primary_raw_movement_phase_stats/10min_based/tables/raw_movement_phase_filter_qc.csv"
  )
}
qc_exclusions <- bind_rows(qc_animal_rows, qc_filter_rows) %>%
  relocate(qc_record_type, source_table, qc_reporting_status)

feature_dictionary <- feature_dictionary_raw %>%
  mutate(
    manuscript_reporting_role = case_when(
      readout %in% canonical_features ~ "PRIMARY",
      readout == "Movement_x_EntropyACF1" ~ "EXPLORATORY/COMPATIBILITY",
      TRUE ~ "SECONDARY OR EXPLORATORY; NOT PART OF THE CANONICAL THREE-FEATURE PRIMARY PREDICTOR REGISTRY"
    )
  ) %>%
  arrange(match(readout, canonical_features), readout)

optional_status_rows <- source_registry %>%
  filter(!required, !exists) %>%
  transmute(
    claim_id = paste0("STATUS_", source_id),
    reporting_role = "STATUS",
    analysis_domain = "Optional source availability",
    endpoint = na_chr,
    time_window = na_chr,
    bin_level = bin_level,
    biological_unit = na_chr,
    n_animals = na_int,
    sex = na_chr,
    contrast_or_model = source_id,
    estimate = na_num,
    effect_size_type = na_chr,
    effect_size = na_num,
    ci_low = na_num,
    ci_high = na_num,
    p_raw = na_num,
    p_adjusted = na_num,
    adjustment_method = na_chr,
    multiplicity_family = na_chr,
    validation_scheme = na_chr,
    robustness_status = "Missing optional source; report generation continued explicitly.",
    source_script = source_script,
    source_table = path,
    notes = "Optional source absent; unavailable values remain NA and were not fabricated."
  ) %>%
  conform_results()

declared_sensitivity_status <- tribble(
  ~claim_id, ~reporting_role, ~analysis_domain, ~endpoint, ~time_window, ~bin_level, ~biological_unit, ~n_animals, ~sex, ~contrast_or_model, ~estimate, ~effect_size_type, ~effect_size, ~ci_low, ~ci_high, ~p_raw, ~p_adjusted, ~adjustment_method, ~multiplicity_family, ~validation_scheme, ~robustness_status, ~source_script, ~source_table, ~notes,
  "STATUS_S09_5MIN", "STATUS", "Predefined resolution sensitivity availability", "CombZ", "First active 12 h after first cage change", "5min", "Animal", NA_integer_, "Pooled", "Corrected Stage 09 canonical models at 5-min resolution", NA_real_, NA_character_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_character_, NA_character_, NA_character_, "Unavailable for this export", "Analysis/09_early_prediction_model_ladder.R", behavior_stage_dir(base_dir, "09", "early_prediction", "5min"), "A legacy 5-min output directory exists, but it was not regenerated under the corrected canonical Stage 09 implementation and is not exported as manuscript evidence.",
  "STATUS_S09_DURATION", "STATUS", "Predefined duration sensitivity availability", "CombZ", "First active phase after first cage change", bin_level, "Animal", NA_integer_, "Pooled", "Canonical primary model duration sensitivity", NA_real_, NA_character_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_character_, NA_character_, NA_character_, "Unavailable as a clean canonical primary-model table", "Analysis/09_early_prediction_model_ladder.R", "tables/model_ladder_performance_duration_sensitivity.csv", "The available duration table belongs to the legacy larger ladder and is not promoted into the clean primary registry."
) %>%
  conform_results()

source_id_lookup <- source_registry %>%
  distinct(artifact, source_id)
if (anyDuplicated(source_id_lookup$artifact)) {
  stop("Provenance artifact names do not resolve uniquely to source_id values.", call. = FALSE)
}

add_source_id <- function(dat) {
  dat %>%
    mutate(.source_artifact = basename(source_table)) %>%
    left_join(source_id_lookup, by = c(".source_artifact" = "artifact")) %>%
    select(-.source_artifact)
}

primary_results <- add_source_id(primary_results)
supplementary_results <- add_source_id(supplementary_results)
if (any(is.na(primary_results$source_id)) || any(is.na(supplementary_results$source_id))) {
  stop("One or more result rows do not resolve to a unique provenance source_id.", call. = FALSE)
}

result_export_columns <- c(
  "claim_id", "analysis_domain", "endpoint", "time_window", "biological_unit",
  "n_animals", "sex", "contrast_or_model", "estimate", "effect_size_type",
  "effect_size", "ci_low", "ci_high", "p_raw", "p_adjusted",
  "adjustment_method", "multiplicity_family", "robustness_status",
  "source_id", "source_row_key", "notes"
)
primary_results_export <- primary_results %>% select(all_of(result_export_columns))
supplementary_results_export <- supplementary_results %>% select(all_of(result_export_columns))
prediction_validation <- prediction_validation %>% mutate(source_id = "s09_prediction_performance")

# Required reporting-registry validation.
if (nrow(primary_results) != 10L || anyDuplicated(primary_results$claim_id)) {
  stop("Primary summary must contain exactly 10 unique mapped rows (3 associations, 4 predictions, 3 interactions).", call. = FALSE)
}
if (any(is.na(primary_results$source_row_key) | primary_results$source_row_key == "")) {
  stop("Every PRIMARY row must map to an explicit upstream row key.", call. = FALSE)
}
primary_prediction_rows <- primary_results %>% filter(analysis_domain == "Prospective prediction")
if (!setequal(primary_prediction_rows$model_id, intended_models)) {
  stop("Primary prediction summary contains an unintended or missing model.", call. = FALSE)
}
if (any(str_detect(coalesce(primary_prediction_rows$predictors, ""), regex("(^|[^A-Za-z])Group([^A-Za-z]|$)")))) {
  stop("Primary prediction results contain Group.", call. = FALSE)
}
if (any(str_detect(primary_results$source_script, regex("Stage.?10|Stage.?14|/10_|/14_|10_systems|14_systems|hmm|manifold|nonlinear", ignore_case = TRUE)))) {
  stop("Exploratory, Stage 10, or Stage 14 predictive output was marked PRIMARY.", call. = FALSE)
}
exported_text <- paste(
  unlist(primary_results %>% select(where(is.character))),
  unlist(supplementary_results %>% select(where(is.character))),
  collapse = " "
)
if (str_detect(exported_text, regex("female-specific|female-driven|trend toward significance", ignore_case = TRUE))) {
  stop("Unsupported sex-specific or trend wording is present in exported results.", call. = FALSE)
}
if (any(str_detect(coalesce(primary_prediction_rows$interval_type, ""), regex("confidence", ignore_case = TRUE)))) {
  stop("Repeated-CV resampling quantiles must not be labelled confidence intervals.", call. = FALSE)
}
if (any(primary_results$reporting_role != "PRIMARY")) {
  stop("Primary result CSV contains a non-PRIMARY reporting role.", call. = FALSE)
}
primary_association_rows <- primary_results %>% filter(analysis_domain == "Prospective feature association")
if (!identical(primary_association_rows$source_row_key, canonical_features)) {
  stop("PRIMARY feature registry order/identity changed.", call. = FALSE)
}
entropy_primary_row <- primary_association_rows %>% filter(source_row_key == "Entropy_acf1")
if (
  nrow(entropy_primary_row) != 1L ||
  entropy_primary_row$robustness_status != entropy_robustness_wording
) {
  stop("Entropy_acf1 robustness wording is not the required qualified description: expected \"",
       entropy_robustness_wording, "\".", call. = FALSE)
}
if (any(primary_results$source_row_key == "Movement_x_EntropyACF1", na.rm = TRUE)) {
  stop("Movement_x_EntropyACF1 must not enter the PRIMARY manuscript registry.", call. = FALSE)
}
if (!str_detect(
  primary_prediction_rows$notes[primary_prediction_rows$model_id == "movement_mean"],
  fixed("Main prospective prediction")
)) {
  stop("Movement_mean is no longer labelled as the headline prospective prediction.", call. = FALSE)
}
if (!str_detect(
  primary_prediction_rows$notes[primary_prediction_rows$model_id == "primary_behavior_family"],
  fixed("did not improve the point-estimate out-of-sample R2")
)) {
  stop("The canonical three-feature model is no longer framed as supporting/sensitivity evidence.", call. = FALSE)
}
if (any(sex_interactions$interaction_p_bh < 0.05, na.rm = TRUE)) {
  stop("Sex-interaction interpretation changed: at least one BH-adjusted interaction p is below 0.05.", call. = FALSE)
}
permutation_display_check <- primary_prediction_rows %>%
  filter(model_id %in% c("movement_mean", "primary_behavior_family")) %>%
  pull(permutation_display)
if (length(permutation_display_check) != 2L || any(permutation_display_check != "1/1001")) {
  stop("Primary full-refit permutation display must remain exactly 1/1001.", call. = FALSE)
}
if (str_detect(exported_text, fixed("P<0.001", ignore_case = TRUE))) {
  stop("Empirical permutation must not be displayed as P<0.001.", call. = FALSE)
}
if (!setequal(
  declared_sensitivity_status$claim_id,
  c("STATUS_S09_5MIN", "STATUS_S09_DURATION")
)) {
  stop("Resolution/duration sensitivity status registry changed unexpectedly.", call. = FALSE)
}

source_hash_after_prep <- setNames(
  map_chr(source_registry$path, sha256_file),
  source_registry$source_id
)
if (!identical(source_hash_before, source_hash_after_prep)) {
  stop("An upstream source changed while Stage 16 was assembling the report.", call. = FALSE)
}

relative_to_analysis_ready <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(analysis_ready_dir, winslash = "/", mustWork = FALSE)
  if (startsWith(normalized, paste0(root, "/"))) {
    return(paste0("analysis_ready/", substring(normalized, nchar(root) + 2L)))
  }
  normalized
}

provenance <- source_registry %>%
  mutate(.selected_path = path) %>%
  transmute(
    source_id,
    stage,
    script = source_script,
    artifact,
    path = map_chr(.selected_path, relative_to_analysis_ready),
    sha256 = map_chr(.selected_path, sha256_file),
    role,
    required,
    resolution = path_resolution,
    notes = paste0(
      source_notes,
      if_else(
        path_resolution == "legacy_fallback",
        paste0(" Canonical target: ", map_chr(canonical_path, relative_to_analysis_ready), "."),
        ""
      )
    )
  )
if (anyDuplicated(provenance$source_id) || any(is.na(provenance$sha256[provenance$required]))) {
  stop("Provenance source_id values must be unique and required sources must have SHA-256 hashes.", call. = FALSE)
}

validation <- tribble(
  ~check_id, ~category, ~status, ~expected, ~observed, ~details, ~source_id,
  "canonical_feature_registry", "scientific_contract", "PASS", "Movement_mean; Movement_rmssd; Entropy_acf1", paste(canonical_features, collapse = "; "), "Exactly three canonical primary features in the required order.", "s09_associations",
  "group_excluded_primary_prediction", "scientific_contract", "PASS", "No primary predictor contains Group", "FALSE", "Group remains descriptive metadata only in source-data sheets.", "s09_model_registry",
  "movement_mean_headline", "scientific_contract", "PASS", "Movement_mean labelled main prospective prediction", "PASS", "The headline-model wording is unchanged.", "s09_prediction_performance",
  "permutation_display", "scientific_contract", "PASS", "1/1001", paste(unique(permutation_display_check), collapse = "; "), "Exact empirical numerator/denominator retained.", "s09_permutation",
  "entropy_wording", "scientific_contract", "PASS", entropy_robustness_wording, entropy_primary_row$robustness_status, "Required qualified Entropy_acf1 wording retained.", "s09_associations",
  "resolution_sensitivity_status", "availability", "PASS", "Unavailable for canonical export unless corrected Stage 09 source exists", declared_sensitivity_status$robustness_status[declared_sensitivity_status$claim_id == "STATUS_S09_5MIN"], "Legacy 5-min output is not promoted.", "s09_prediction_performance",
  "duration_sensitivity_status", "availability", "PASS", "Not promoted from the legacy larger ladder", declared_sensitivity_status$robustness_status[declared_sensitivity_status$claim_id == "STATUS_S09_DURATION"], "Primary-model duration sensitivity remains explicitly unavailable.", "s09_prediction_performance",
  "required_source_availability", "lineage", "PASS", as.character(sum(source_registry$required)), as.character(sum(source_registry$required & source_registry$exists)), "All required Stage 03/09 sources resolved.", NA_character_,
  # An empty vector would paste() to "", and an empty string does not round-trip
  # identically through CSV (read back as NA) and XLSX (read back as ""), which
  # trips the strict workbook/CSV reconciliation below. Report the no-fallback
  # case with an explicit sentinel instead; it is also clearer to a reader.
  "legacy_fallback_recorded", "lineage", "PASS", "Every fallback identified", if (nrow(legacy_sources_used) == 0L) "none" else paste(legacy_sources_used$source_id, collapse = "; "), "Canonical paths were preferred; selected legacy paths are explicit in Provenance.", NA_character_,
  "protected_stage_hash_validation", "invariance", "PASS", "Upstream hashes stable during Stage 16", "PASS", "Stage 16 reads sources only; SHA-256 values before and after assembly matched.", NA_character_,
  "animal_source_uniqueness", "source_data", "PASS", "One row per AnimalID", as.character(nrow(animal_level_source_data)), "Animal-level values are selected directly from the canonical Stage 09 model input.", "s09_model_input",
  "prediction_source_uniqueness", "source_data", "PASS", "One row per AnimalID and model_id", as.character(nrow(prediction_source_data)), "Held-out predictions cover Movement_mean and the plotted three-feature model.", "s09_prediction_source",
  "movement_source_availability", "source_data", "PASS", "Stage 03 animal-level movement observations available", as.character(nrow(movement_phase_source_data)), "Tidy long source data underlie manuscript-facing Stage 03 panels.", "s03_animal_endpoints",
  "stage10_stage14_primary_exclusion", "scope", "PASS", "No Stage 10/14 claim promoted", "PASS", "Systems/HMM/nonlinear/proteomics extensions remain outside primary manuscript claims.", NA_character_,
  "workbook_csv_reconciliation", "workbook", "PASS", "Exact numeric/text agreement", "Enforced post-save", "The script aborts if any canonical workbook table disagrees with its CSV.", NA_character_,
  "workbook_ooxml_integrity", "workbook", "PASS", "Valid OOXML; no formulas/errors/external links/drawings/VML", "Enforced post-save", "The script aborts on package, relationship, range, filter, formula, error-cell, or drawing failures.", NA_character_
)

readme_sheet <- tribble(
  ~item, ~definition,
  "Purpose", "Assembly/export layer for canonical behavioral manuscript results; no models, p values, corrections, or canonical statistics are recalculated here.",
  "Recommended entry point", "analysis_ready/manuscript/behavior/Behavioral_Source_Data.xlsx, with machine-readable CSV companions in the same directory.",
  "Pipeline entry point", "analysis_ready/pipeline/ contains canonical migrated stage outputs; analysis_ready/output_index.csv maps migrated and legacy stages.",
  "Biological unit", "Animal.",
  "Prospective window", "First active 12 h after the first cage change.",
  "Canonical prediction resolution", "10-min bins; canonical directory token is 10min.",
  "Resolution sensitivity", "5-min analysis is predefined where a corrected canonical source is available; it is not silently substituted for the 10-min primary analysis.",
  "PRIMARY", "Stage 09 canonical feature associations, fixed prospective prediction models, Sex-adjusted sensitivity, and formal feature-by-Sex interactions.",
  "SECONDARY", "Stage 03 phenotype/group characterization, cage-change/phase movement statistics, group distributions, and interpretable sensitivities.",
  "EXPLORATORY", "HMM/state, PCA/UMAP/PHATE/manifold, nonlinear dynamics, high-dimensional systems prediction, systems composites, Stage 10/14 predictive claims, and behavior-proteomics integration unless enabled later.",
  "LOAO CV", "Primary internal out-of-sample performance estimate: leave one animal out and predict that held-out animal.",
  "Repeated grouped 5-fold CV", "Resampling-robustness companion; reported intervals are 2.5-97.5% quantiles across repeated CV splits, not confidence intervals.",
  "Empirical permutation", "Outcome shuffled across animals and the complete LOAO model/preprocessing refit repeated. With 1000 permutations the smallest displayed value is 1/1001 (approximately 0.001), never P<0.001.",
  "Validation limit", "Internal prediction is not external validation.",
  "Group", "RES/SUS Group is downstream phenotype characterization related to CombZ and is excluded from primary prospective prediction models.",
  "Source-data Group column", "Group is retained only as descriptive animal metadata and for descriptive plotting; it is not a primary prospective predictor.",
  "Sex interpretation", "No evidence that the association differed by sex; sex-stratified associations are descriptive only.",
  "Stage hierarchy", "Stage 09 is primary prospective prediction. Stage 03 is secondary phenotype/group characterization."
)

xlsx_used_range <- function(dat) {
  if (ncol(dat) < 1L) stop("Cannot write a worksheet with zero columns.", call. = FALSE)
  paste0("A1:", openxlsx::int2col(ncol(dat)), nrow(dat) + 1L)
}

prepare_openxlsx_package_metadata <- function(wb, expected_ranges) {
  if (length(expected_ranges) != length(wb[["worksheets"]])) {
    stop("Workbook worksheet count does not match the expected range registry.", call. = FALSE)
  }

  object_slots <- c("drawings", "drawings_rels", "vml", "comments")
  for (i in seq_along(expected_ranges)) {
    for (slot in object_slots) {
      if (length(wb[[slot]][[i]]) > 0L) {
        stop(
          "Unexpected workbook object in sheet ", names(expected_ranges)[[i]],
          " (", slot, "); Stage 16 does not create drawings, VML, or comments.",
          call. = FALSE
        )
      }
    }

    wb[["worksheets"]][[i]][["dimension"]] <- paste0(
      "<dimension ref=\"", expected_ranges[[i]], "\"/>"
    )
    wb[["worksheets"]][[i]][["drawing"]] <- character()
    wb[["worksheets"]][[i]][["legacyDrawing"]] <- character()
    wb[["worksheets"]][[i]][["legacyDrawingHF"]] <- character()
  }

  worksheet_rels <- wb[["worksheets_rels"]]
  worksheet_rels <- lapply(worksheet_rels, function(rels) {
    rels[!grepl("relationships/drawing|vmlDrawing", rels)]
  })
  wb[["worksheets_rels"]] <- worksheet_rels
  wb[["Content_Types"]] <- wb[["Content_Types"]][
    !grepl("PartName=\"/xl/drawings/drawing[0-9]+[.]xml\"", wb[["Content_Types"]])
  ]

  invisible(wb)
}

xml_attr <- function(tag, attribute) {
  match <- regexec(
    paste0("\\b", attribute, "\\s*=\\s*\"([^\"]*)\""),
    tag,
    perl = TRUE
  )
  value <- regmatches(tag, match)[[1]]
  if (length(value) < 2L) NA_character_ else value[[2]]
}

xml_matches <- function(text, pattern) {
  hits <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (length(hits) == 1L && hits[[1]] == -1L) return(character())
  regmatches(text, list(hits))[[1]]
}

normalize_ooxml_part <- function(path) {
  path <- gsub("\\\\", "/", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  stack <- character()
  for (part in parts) {
    if (!nzchar(part) || part == ".") next
    if (part == "..") {
      if (length(stack) == 0L) return(NA_character_)
      stack <- head(stack, -1L)
    } else {
      stack <- c(stack, part)
    }
  }
  paste(stack, collapse = "/")
}

read_xlsx_part <- function(xlsx_path, zip_listing, part) {
  idx <- match(part, zip_listing$Name)
  if (is.na(idx)) stop("XLSX part is absent: ", part, call. = FALSE)
  con <- unz(xlsx_path, part, open = "rb")
  on.exit(close(con), add = TRUE)
  expected_bytes <- as.integer(zip_listing$Length[[idx]])
  value <- readBin(con, what = "raw", n = expected_bytes + 1L)
  if (length(value) != expected_bytes) {
    stop("Could not read complete XLSX part: ", part, call. = FALSE)
  }
  value
}

validate_xlsx_package <- function(xlsx_path, expected_ranges, expected_sheet_names) {
  zip_listing <- tryCatch(
    suppressWarnings(utils::unzip(xlsx_path, list = TRUE)),
    error = function(e) NULL
  )
  if (is.null(zip_listing) || nrow(zip_listing) == 0L) {
    stop("XLSX package validation failed: file is not a readable ZIP archive.", call. = FALSE)
  }
  if (anyDuplicated(zip_listing$Name)) {
    stop("XLSX package validation failed: duplicate ZIP part names detected.", call. = FALSE)
  }

  # Reading every entry validates that the ZIP members are accessible without
  # altering the saved package.
  invisible(lapply(zip_listing$Name, function(part) {
    read_xlsx_part(xlsx_path, zip_listing, part)
  }))
  zip_parts <- zip_listing$Name
  read_text_part <- function(part) rawToChar(read_xlsx_part(xlsx_path, zip_listing, part))

  relationship_parts <- grep("(^|/)_rels/.*[.]rels$", zip_parts, value = TRUE)
  dangling_relationships <- character()
  drawing_relationships <- character()
  external_relationships <- character()
  for (relationship_part in relationship_parts) {
    relationship_xml <- read_text_part(relationship_part)
    relationship_tags <- xml_matches(relationship_xml, "<Relationship\\b[^>]*/?>")
    for (tag in relationship_tags) {
      target_mode <- xml_attr(tag, "TargetMode")
      if (!is.na(target_mode) && identical(target_mode, "External")) {
        external_relationships <- c(
          external_relationships,
          paste0(relationship_part, "::", xml_attr(tag, "Id"))
        )
        next
      }

      target <- xml_attr(tag, "Target")
      relationship_id <- xml_attr(tag, "Id")
      relationship_type <- xml_attr(tag, "Type")
      if (is.na(target) || !nzchar(target)) {
        dangling_relationships <- c(
          dangling_relationships,
          paste0(relationship_part, "::", relationship_id, " -> <empty target>")
        )
        next
      }
      target <- utils::URLdecode(gsub("&amp;", "&", target, fixed = TRUE))
      target <- sub("[?#].*$", "", target)

      if (startsWith(target, "/")) {
        resolved_target <- normalize_ooxml_part(sub("^/", "", target))
      } else {
        relationship_base <- if (identical(relationship_part, "_rels/.rels")) {
          ""
        } else {
          base <- dirname(dirname(relationship_part))
          if (identical(base, ".")) "" else base
        }
        resolved_target <- normalize_ooxml_part(
          if (nzchar(relationship_base)) paste0(relationship_base, "/", target) else target
        )
      }

      if (is.na(resolved_target) || !resolved_target %in% zip_parts) {
        dangling_relationships <- c(
          dangling_relationships,
          paste0(relationship_part, "::", relationship_id, " -> ", target)
        )
      }
      if (!is.na(relationship_type) && grepl("/drawing$|/vmlDrawing$", relationship_type)) {
        drawing_relationships <- c(drawing_relationships, paste0(relationship_part, "::", relationship_id))
      }
    }
  }
  if (length(dangling_relationships) > 0L) {
    stop(
      "XLSX package integrity failure: internal relationship target(s) do not exist:\n",
      paste0("- ", dangling_relationships, collapse = "\n"),
      call. = FALSE
    )
  }
  if (length(drawing_relationships) > 0L || any(grepl("^xl/drawings/", zip_parts))) {
    stop("XLSX package integrity failure: unexpected drawing/VML objects are present.", call. = FALSE)
  }
  if (length(external_relationships) > 0L || any(grepl("^xl/externalLinks/", zip_parts))) {
    stop("XLSX package integrity failure: unexpected external link relationships are present.", call. = FALSE)
  }
  if (any(grepl("vbaProject|macrosheet", zip_parts, ignore.case = TRUE))) {
    stop("XLSX package integrity failure: unexpected macro content is present.", call. = FALSE)
  }

  content_types_xml <- read_text_part("[Content_Types].xml")
  override_tags <- xml_matches(content_types_xml, "<Override\\b[^>]*/?>")
  override_parts <- vapply(override_tags, xml_attr, character(1), attribute = "PartName")
  override_parts <- sub("^/", "", utils::URLdecode(override_parts))
  missing_overrides <- setdiff(override_parts[!is.na(override_parts)], zip_parts)
  if (length(missing_overrides) > 0L) {
    stop(
      "XLSX package integrity failure: [Content_Types].xml declares absent part(s):\n",
      paste0("- ", missing_overrides, collapse = "\n"),
      call. = FALSE
    )
  }

  actual_sheet_names <- openxlsx::getSheetNames(xlsx_path)
  if (!identical(actual_sheet_names, expected_sheet_names)) {
    stop("XLSX package validation failed: worksheet name/order mismatch.", call. = FALSE)
  }
  worksheet_parts <- grep("^xl/worksheets/sheet[0-9]+[.]xml$", zip_parts, value = TRUE)
  worksheet_numbers <- as.integer(sub("^.*sheet([0-9]+)[.]xml$", "\\1", worksheet_parts))
  worksheet_parts <- worksheet_parts[order(worksheet_numbers)]
  if (length(worksheet_parts) != length(expected_sheet_names)) {
    stop("XLSX package validation failed: worksheet part count mismatch.", call. = FALSE)
  }

  range_audit <- purrr::map2_dfr(
    worksheet_parts,
    seq_along(worksheet_parts),
    function(worksheet_part, i) {
      worksheet_xml <- read_text_part(worksheet_part)
      dimension_tag <- xml_matches(worksheet_xml, "<dimension\\b[^>]*/?>")
      filter_tag <- xml_matches(worksheet_xml, "<autoFilter\\b[^>]*/?>")
      dimension_ref <- if (length(dimension_tag) == 1L) xml_attr(dimension_tag, "ref") else NA_character_
      filter_ref <- if (length(filter_tag) == 1L) xml_attr(filter_tag, "ref") else NA_character_

      cell_tags <- xml_matches(worksheet_xml, "<c\\b[^>]*>")
      cell_refs <- vapply(cell_tags, xml_attr, character(1), attribute = "r")
      cell_refs <- cell_refs[!is.na(cell_refs)]
      if (length(cell_refs) == 0L) {
        populated_ref <- NA_character_
      } else {
        cell_cols <- sub("[0-9]+$", "", cell_refs)
        cell_rows <- as.integer(sub("^[A-Z]+", "", cell_refs))
        column_number <- function(x) {
          Reduce(function(total, value) total * 26L + value, utf8ToInt(x) - utf8ToInt("A") + 1L, init = 0L)
        }
        max_col <- max(vapply(cell_cols, column_number, integer(1)))
        populated_ref <- paste0("A1:", openxlsx::int2col(max_col), max(cell_rows))
      }

      expected_ref <- unname(expected_ranges[[i]])
      if (!identical(dimension_ref, expected_ref) ||
          !identical(filter_ref, expected_ref) ||
          !identical(populated_ref, expected_ref)) {
        stop(
          "XLSX worksheet range validation failed for ", expected_sheet_names[[i]],
          ": expected ", expected_ref,
          "; dimension=", dimension_ref,
          "; autoFilter=", filter_ref,
          "; populated=", populated_ref,
          call. = FALSE
        )
      }
      if (length(xml_matches(worksheet_xml, "<f\\b[^>]*>")) > 0L ||
          length(xml_matches(worksheet_xml, "<c\\b[^>]*\\bt=\"e\"")) > 0L) {
        stop("XLSX worksheet contains an unexpected formula or error cell: ", expected_sheet_names[[i]], call. = FALSE)
      }

      tibble(
        worksheet = expected_sheet_names[[i]],
        used_range = dimension_ref,
        autofilter_range = filter_ref
      )
    }
  )

  list(
    zip_entries = nrow(zip_listing),
    dangling_relationships = 0L,
    external_relationships = 0L,
    macro_parts = 0L,
    missing_content_type_parts = 0L,
    formulas = 0L,
    error_cells = 0L,
    sheet_ranges = range_audit
  )
}

stage16_palette <- list(
  header = "#202020",
  text = "#222222",
  secondary = "#6B6B6B",
  alternate = "#F7F7F7",
  neutral = "#F1F1F1",
  cool_neutral = "#F2F5F7",
  separator = "#D9D9D9",
  accent = "#B51F2E",
  pale_accent = "#FBEFF1",
  pale_warning = "#FFF2F0",
  white = "#FFFFFF"
)

stage16_result_widths <- c(
  claim_id = 27, analysis_domain = 32, endpoint = 17,
  time_window = 30, biological_unit = 20, n_animals = 10,
  sex = 20, contrast_or_model = 36, estimate = 12, effect_size_type = 34,
  effect_size = 12, ci_low = 12, ci_high = 12, p_raw = 13, p_adjusted = 13,
  adjustment_method = 29, multiplicity_family = 34,
  robustness_status = 42, source_id = 24, source_row_key = 30, notes = 42
)

stage16_sheet_widths <- list(
  README = c(item = 27, definition = 90),
  Primary_results = stage16_result_widths,
  Prediction_validation = c(
    model_id = 29, model_label = 38, reporting_role = 24, predictors = 42,
    n_animals = 10, validation_scheme = 42, loao_cv_r2 = 12, loao_rmse = 12,
    loao_mae = 12, loao_pearson_r = 14, loao_spearman_rho = 15,
    repeated_5fold_mean_r2 = 17, repeated_5fold_mean_rmse = 19,
    repeated_5fold_mean_mae = 18, cv_r2_q025 = 12, cv_r2_q975 = 12,
    interval_type = 34, permutation_scheme = 42,
    permutation_observed_statistic = 18, permutation_null_median = 17,
    permutation_null_q025 = 16, permutation_null_q975 = 16,
    permutation_empirical_p = 17, permutation_n = 13, permutation_seed = 15,
    permutation_numerator = 14, permutation_denominator = 16,
    permutation_display = 14, interpretation = 54, source_id = 24
  ),
  Primary_source_data = c(
    AnimalID = 12, Sex = 10, Group = 10, CombZ = 12,
    Movement_mean = 15, Movement_rmssd = 16, Entropy_acf1 = 14,
    early_observation_hours = 19, contains_short_duration_epoch = 22,
    source_id = 24
  ),
  Prediction_source_data = c(
    AnimalID = 12, observed_CombZ = 16, predicted_CombZ = 16,
    model_id = 29, validation_scheme = 25, Sex = 10, Group = 10, source_id = 24
  ),
  Supplementary_results = stage16_result_widths,
  Movement_phase_source_data = c(
    AnimalID = 12, Sex = 10, Group = 10, cage_change = 13, phase = 12,
    movement_measure = 20, value = 13, n_bins = 10, scope_type = 24,
    endpoint = 22, source_id = 24
  ),
  QC = c(
    qc_record_type = 32, source_table = 42, qc_reporting_status = 44,
    AnimalID = 12, Sex = 10, Group = 10, n_windows = 10, n_pass = 9,
    n_review = 10, n_moderate = 11, n_high = 9, max_flags = 10,
    worst_zero_fraction = 16, worst_dominant_position_fraction = 20,
    lowest_mean_entropy = 17, earliest_possible_collapse = 20,
    suggested_decision = 38, comment = 46, PhaseClass = 14,
    n_animals_pre_filter = 17, n_rows_pre_filter = 16,
    n_animals_post_filter = 18, n_rows_post_filter = 17
  ),
  Feature_dictionary = c(
    readout = 30, display_label = 30, domain = 26, definition = 62,
    manuscript_role = 31, manuscript_reporting_role = 45
  ),
  Provenance = c(
    source_id = 24, stage = 8, script = 40, artifact = 42, path = 65,
    sha256 = 66, role = 30, required = 10, resolution = 18, notes = 65
  ),
  Validation = c(
    check_id = 35, category = 22, status = 11, expected = 45,
    observed = 45, details = 70, source_id = 24
  )
)

stage16_tab_colours <- c(
  README = "#8A8A8A", Primary_results = stage16_palette$accent,
  Prediction_validation = stage16_palette$accent,
  Primary_source_data = "#8A8A8A", Prediction_source_data = "#8A8A8A",
  Supplementary_results = "#A6A6A6", Movement_phase_source_data = "#A6A6A6",
  QC = "#A6A6A6", Feature_dictionary = "#8A8A8A",
  Provenance = "#4A4A4A", Validation = "#4A4A4A"
)

stage16_freeze_columns <- c(
  README = NA_integer_, Primary_results = 4L, Prediction_validation = 3L,
  Primary_source_data = 4L, Prediction_source_data = 4L,
  Supplementary_results = 4L, Movement_phase_source_data = 4L,
  QC = 5L, Feature_dictionary = 2L, Provenance = 2L, Validation = 3L
)

stage16_row_heights <- c(
  README = 42, Primary_results = 54, Prediction_validation = 58,
  Primary_source_data = 24, Prediction_source_data = 24,
  Supplementary_results = 40, Movement_phase_source_data = 24,
  QC = 34, Feature_dictionary = 44, Provenance = 42, Validation = 42
)

stage16_wrap_columns <- function(sheet) {
  result_prose <- c(
    "analysis_domain", "time_window", "biological_unit", "sex",
    "contrast_or_model", "effect_size_type", "adjustment_method",
    "multiplicity_family", "validation_scheme", "robustness_status",
    "source_table", "notes", "model_label", "predictors",
    "interval_type"
  )
  switch(
    sheet,
    README = "definition",
    Primary_results = result_prose,
    Prediction_validation = c(
      "model_label", "reporting_role", "predictors", "validation_scheme",
      "interval_type", "permutation_scheme", "interpretation"
    ),
    Primary_source_data = character(),
    Prediction_source_data = c("model_id", "validation_scheme"),
    Supplementary_results = result_prose,
    Movement_phase_source_data = c("movement_measure", "scope_type", "endpoint"),
    QC = c(
      "qc_record_type", "source_table", "qc_reporting_status",
      "suggested_decision", "comment"
    ),
    Feature_dictionary = c(
      "display_label", "domain", "definition", "manuscript_role",
      "manuscript_reporting_role"
    ),
    Provenance = c("script", "artifact", "path", "role", "notes"),
    Validation = c("check_id", "expected", "observed", "details", "source_id"),
    character()
  )
}

stage16_create_styles <- function() {
  list(
    header = openxlsx::createStyle(
      fontName = "Arial", fontSize = 10, textDecoration = "bold",
      fgFill = stage16_palette$header, fontColour = stage16_palette$white,
      border = "Bottom", borderColour = stage16_palette$separator,
      borderStyle = "thin", halign = "left", valign = "center",
      wrapText = TRUE
    ),
    body = openxlsx::createStyle(
      fontName = "Arial", fontSize = 9, fontColour = stage16_palette$text,
      fgFill = stage16_palette$white, halign = "left", valign = "center",
      wrapText = FALSE
    ),
    wrap = openxlsx::createStyle(wrapText = TRUE, valign = "center"),
    alternate = openxlsx::createStyle(fgFill = stage16_palette$alternate),
    number = openxlsx::createStyle(halign = "right", numFmt = "0.000"),
    p_value = openxlsx::createStyle(halign = "right", numFmt = "0.000E+00"),
    integer = openxlsx::createStyle(halign = "right", numFmt = "0"),
    date_time = openxlsx::createStyle(halign = "center", numFmt = "yyyy-mm-dd hh:mm"),
    identifier = openxlsx::createStyle(textDecoration = "bold"),
    section_top = openxlsx::createStyle(
      border = "Top", borderColour = stage16_palette$separator,
      borderStyle = "medium"
    ),
    primary_role = openxlsx::createStyle(
      textDecoration = "bold", fontColour = stage16_palette$accent
    ),
    secondary_role = openxlsx::createStyle(
      textDecoration = "bold", fontColour = stage16_palette$secondary
    ),
    status_role = openxlsx::createStyle(
      textDecoration = "italic", fontColour = stage16_palette$secondary
    ),
    headline_row = openxlsx::createStyle(fgFill = stage16_palette$pale_accent),
    headline_label = openxlsx::createStyle(
      textDecoration = "bold", fontColour = stage16_palette$accent
    ),
    supported_p = openxlsx::createStyle(
      textDecoration = "bold", fontColour = stage16_palette$accent
    ),
    neutral_row = openxlsx::createStyle(fgFill = stage16_palette$neutral),
    cool_row = openxlsx::createStyle(fgFill = stage16_palette$cool_neutral),
    warning = openxlsx::createStyle(
      fgFill = stage16_palette$pale_warning,
      fontColour = "#7A1720", textDecoration = "bold", wrapText = TRUE
    ),
    readme_item = openxlsx::createStyle(
      fgFill = stage16_palette$neutral, textDecoration = "bold"
    ),
    canonical_feature = openxlsx::createStyle(fgFill = stage16_palette$pale_accent)
  )
}

stage16_apply_role_style <- function(wb, sheet, dat, styles) {
  role_col <- match("reporting_role", names(dat))
  if (is.na(role_col) || nrow(dat) == 0L) return(invisible(wb))
  role_values <- as.character(dat$reporting_role)
  role_styles <- list(
    PRIMARY = styles$primary_role,
    SECONDARY = styles$secondary_role,
    STATUS = styles$status_role
  )
  for (role in names(role_styles)) {
    rows <- which(role_values == role) + 1L
    if (length(rows)) {
      openxlsx::addStyle(
        wb, sheet, role_styles[[role]], rows = rows, cols = role_col,
        gridExpand = TRUE, stack = TRUE
      )
    }
  }
  invisible(wb)
}

stage16_apply_sheet_emphasis <- function(wb, sheet, dat, styles) {
  if (nrow(dat) == 0L) return(invisible(wb))
  data_rows <- seq_len(nrow(dat)) + 1L

  if (sheet == "README") {
    openxlsx::addStyle(
      wb, sheet, styles$readme_item, rows = data_rows,
      cols = match("item", names(dat)), gridExpand = TRUE, stack = TRUE
    )
  }

  if (sheet == "Primary_results") {
    section_rows <- which(!duplicated(dat$analysis_domain)) + 1L
    section_rows <- setdiff(section_rows, 2L)
    if (length(section_rows)) {
      openxlsx::addStyle(
        wb, sheet, styles$section_top, rows = section_rows,
        cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE
      )
    }
    id_cols <- match(c("claim_id", "contrast_or_model"), names(dat), nomatch = 0L)
    openxlsx::addStyle(
      wb, sheet, styles$identifier, rows = data_rows, cols = id_cols[id_cols > 0L],
      gridExpand = TRUE, stack = TRUE
    )
    headline_rows <- which(dat$claim_id == "S09_PRED_movement_mean") + 1L
    if (length(headline_rows)) {
      openxlsx::addStyle(
        wb, sheet, styles$headline_row, rows = headline_rows,
        cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE
      )
      headline_cols <- match(c("claim_id", "contrast_or_model"), names(dat), nomatch = 0L)
      openxlsx::addStyle(
        wb, sheet, styles$headline_label, rows = headline_rows,
        cols = headline_cols[headline_cols > 0L], gridExpand = TRUE, stack = TRUE
      )
    }
    p_col <- match("p_adjusted", names(dat))
    supported_rows <- which(!is.na(dat$p_adjusted) & dat$p_adjusted < 0.05) + 1L
    if (!is.na(p_col) && length(supported_rows)) {
      openxlsx::addStyle(
        wb, sheet, styles$supported_p, rows = supported_rows, cols = p_col,
        gridExpand = TRUE, stack = TRUE
      )
    }
  }

  if (sheet == "Prediction_validation") {
    row_styles <- list(
      reference_baseline = styles$neutral_row,
      primary_behavior_only = styles$headline_row,
      sex_adjusted_sensitivity = styles$cool_row
    )
    for (role in names(row_styles)) {
      rows <- which(dat$reporting_role == role) + 1L
      if (length(rows)) {
        openxlsx::addStyle(
          wb, sheet, row_styles[[role]], rows = rows, cols = seq_len(ncol(dat)),
          gridExpand = TRUE, stack = TRUE
        )
      }
    }
    model_cols <- match(c("model_id", "model_label"), names(dat), nomatch = 0L)
    openxlsx::addStyle(
      wb, sheet, styles$identifier, rows = data_rows, cols = model_cols[model_cols > 0L],
      gridExpand = TRUE, stack = TRUE
    )
    headline_rows <- which(dat$model_id == "movement_mean") + 1L
    if (length(headline_rows)) {
      openxlsx::addStyle(
        wb, sheet, styles$headline_label, rows = headline_rows,
        cols = model_cols[model_cols > 0L], gridExpand = TRUE, stack = TRUE
      )
    }
  }

  if (sheet == "QC") {
    decision <- if ("suggested_decision" %in% names(dat)) coalesce(as.character(dat$suggested_decision), "") else rep("", nrow(dat))
    comment <- if ("comment" %in% names(dat)) coalesce(as.character(dat$comment), "") else rep("", nrow(dat))
    warning_rows <- which(str_detect(
      paste(decision, comment),
      regex("manual.?review|high.?suspicion", ignore_case = TRUE)
    )) + 1L
    warning_cols <- match(c("suggested_decision", "comment"), names(dat), nomatch = 0L)
    if (length(warning_rows) && any(warning_cols > 0L)) {
      openxlsx::addStyle(
        wb, sheet, styles$warning, rows = warning_rows,
        cols = warning_cols[warning_cols > 0L], gridExpand = TRUE, stack = TRUE
      )
    }
  }

  if (sheet == "Feature_dictionary") {
    primary_rows <- which(dat$readout %in% canonical_features) + 1L
    if (length(primary_rows)) {
      openxlsx::addStyle(
        wb, sheet, styles$canonical_feature, rows = primary_rows,
        cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE
      )
      openxlsx::addStyle(
        wb, sheet, styles$headline_label, rows = primary_rows,
        cols = match("readout", names(dat)), gridExpand = TRUE, stack = TRUE
      )
    }
  }

  stage16_apply_role_style(wb, sheet, dat, styles)
  invisible(wb)
}

write_sheet <- function(wb, sheet, dat, styles) {
  openxlsx::addWorksheet(
    wb, sheet, gridLines = FALSE, tabColour = unname(stage16_tab_colours[[sheet]]),
    zoom = if (sheet %in% c("Supplementary_results", "Movement_phase_source_data", "QC", "Provenance")) 85 else 90
  )
  # openxlsx assigns a fixed custom number-format ID to POSIXct columns. That
  # can collide with other custom formats in the same workbook and make plain
  # numeric statistics display as dates in Excel. Write the equivalent Excel
  # serial explicitly, then apply the intended date/time display style below.
  write_dat <- dat
  posix_cols <- which(vapply(write_dat, inherits, logical(1), what = "POSIXt"))
  for (col in posix_cols) {
    write_dat[[col]] <- as.numeric(write_dat[[col]]) / 86400 + 25569
  }

  openxlsx::writeData(
    wb, sheet, write_dat, headerStyle = styles$header,
    rowNames = FALSE, withFilter = TRUE, keepNA = FALSE
  )

  n_cols <- ncol(dat)
  n_rows <- nrow(dat)
  if (n_cols < 1L) return(invisible(wb))

  widths <- rep(14, n_cols)
  names(widths) <- names(dat)
  declared_widths <- stage16_sheet_widths[[sheet]]
  matched_widths <- intersect(names(declared_widths), names(widths))
  widths[matched_widths] <- declared_widths[matched_widths]
  openxlsx::setColWidths(wb, sheet, cols = seq_len(n_cols), widths = unname(widths))
  openxlsx::setRowHeights(wb, sheet, rows = 1L, heights = 38)

  freeze_col <- unname(stage16_freeze_columns[[sheet]])
  if (is.na(freeze_col)) {
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2L)
  } else {
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2L, firstActiveCol = freeze_col)
  }

  if (n_rows > 0L) {
    data_rows <- seq_len(n_rows) + 1L
    openxlsx::setRowHeights(
      wb, sheet, rows = data_rows,
      heights = unname(stage16_row_heights[[sheet]])
    )
    openxlsx::addStyle(
      wb, sheet, styles$body, rows = data_rows, cols = seq_len(n_cols),
      gridExpand = TRUE, stack = FALSE
    )
    alternate_rows <- data_rows[seq_along(data_rows) %% 2L == 0L]
    if (length(alternate_rows)) {
      openxlsx::addStyle(
        wb, sheet, styles$alternate, rows = alternate_rows,
        cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE
      )
    }

    wrap_cols <- match(stage16_wrap_columns(sheet), names(dat), nomatch = 0L)
    if (any(wrap_cols > 0L)) {
      openxlsx::addStyle(
        wb, sheet, styles$wrap, rows = data_rows, cols = wrap_cols[wrap_cols > 0L],
        gridExpand = TRUE, stack = TRUE
      )
    }

    numeric_cols <- which(vapply(write_dat, is.numeric, logical(1)))
    p_cols <- intersect(
      numeric_cols,
      which(names(dat) %in% c("p_raw", "p_adjusted", "permutation_empirical_p"))
    )
    date_cols <- intersect(numeric_cols, which(names(dat) == "earliest_possible_collapse"))
    integer_cols <- intersect(
      numeric_cols,
      union(
        which(vapply(dat, is.integer, logical(1))),
        which(str_detect(
          names(dat),
          regex("^(n($|_)|.*_n$)|n_animals|numerator|denominator|seed|max_flags|CageChangeIndex", ignore_case = TRUE)
        ))
      )
    )
    decimal_cols <- setdiff(numeric_cols, union(union(p_cols, date_cols), integer_cols))

    if (length(decimal_cols)) {
      openxlsx::addStyle(
        wb, sheet, styles$number, rows = data_rows, cols = decimal_cols,
        gridExpand = TRUE, stack = TRUE
      )
    }
    if (length(p_cols)) {
      openxlsx::addStyle(
        wb, sheet, styles$p_value, rows = data_rows, cols = p_cols,
        gridExpand = TRUE, stack = TRUE
      )
    }
    if (length(integer_cols)) {
      openxlsx::addStyle(
        wb, sheet, styles$integer, rows = data_rows, cols = integer_cols,
        gridExpand = TRUE, stack = TRUE
      )
    }
    if (length(date_cols)) {
      openxlsx::addStyle(
        wb, sheet, styles$date_time, rows = data_rows, cols = date_cols,
        gridExpand = TRUE, stack = TRUE
      )
    }

    stage16_apply_sheet_emphasis(wb, sheet, dat, styles)
  }
  invisible(wb)
}

# CSVs are written once, in this single output directory.
readr::write_csv(primary_results_export, artifact_paths[["primary"]], na = "NA")
readr::write_csv(supplementary_results_export, artifact_paths[["supplementary"]], na = "NA")
readr::write_csv(animal_level_source_data, artifact_paths[["animal_source"]], na = "NA")
readr::write_csv(prediction_source_data, artifact_paths[["prediction_source"]], na = "NA")
readr::write_csv(movement_phase_source_data, artifact_paths[["movement_source"]], na = "NA")
readr::write_csv(provenance, artifact_paths[["provenance"]], na = "NA")
readr::write_csv(validation, artifact_paths[["validation"]], na = "NA")

output_index <- tribble(
  ~stage, ~analysis, ~resolution, ~artifact_type, ~canonical_path, ~producer, ~manuscript_role, ~status, ~legacy_path, ~notes,
  "00", "QC tracking integrity", NA_character_, "pipeline output group", NA_character_, "Analysis/00_qc_tracking_integrity.R", "technical QC", "legacy_pending_migration", "analysis_ready/00_qc_tracking_integrity/", "Diagnostic and non-destructive; not migrated in this task.",
  "01", "Multiscale behavior metrics", "multiple", "pipeline output group", NA_character_, "Analysis/01_build_multiscale_behavior_metrics.R", "canonical input layer", "legacy_pending_migration", "analysis_ready/03_derived_metrics/", "Canonical upstream metrics remain in place.",
  "02", "Dyadic RFID contacts", "multiple", "pipeline output group", NA_character_, "Analysis/02_build_dyadic_rfid_contacts.R", "secondary/social source", "legacy_pending_migration", "analysis_ready/06_behavioral_dynamics/dyadic_contacts/", "Not migrated in this task.",
  "03", "Movement phase statistics", "10min", "pipeline output group", "analysis_ready/pipeline/03_movement_phase_stats/10min/", "Analysis/03_primary_raw_movement_phase_stats.R", "secondary manuscript evidence", "producer_migrated_legacy_artifacts_retained", "analysis_ready/03_primary_raw_movement_phase_stats/10min_based/", "New writes use tables, figures, and audit only; Stage 16 records fallback use.",
  "04", "Temporal instability", "10sec", "pipeline output group", NA_character_, "Analysis/04_temporal_instability.R", "mechanistic/secondary", "legacy_pending_migration", "analysis_ready/06_behavioral_dynamics/temporal_instability/", "Reviewed but not included in Stage 16 primary reporting.",
  "05", "Behavioral state space", "5min", "pipeline output group", NA_character_, "Analysis/05_behavioral_state_space.R", "exploratory/mechanistic", "legacy_pending_migration", "analysis_ready/06_behavioral_dynamics/state_space/", "Not promoted into Stage 16.",
  "06", "Dynamic social networks", "5min", "pipeline output group", NA_character_, "Analysis/06_dynamic_social_networks.R", "secondary/social", "legacy_pending_migration", "analysis_ready/06_behavioral_dynamics/social_networks/", "Not included without a selected manuscript result.",
  "07", "GAMM trajectory features", "10min", "pipeline output group", NA_character_, "Analysis/07_gamm_trajectory_features.R", "mechanistic/secondary", "legacy_pending_migration", "analysis_ready/06_behavioral_dynamics/gamm_features/", "Not promoted into Stage 16.",
  "08", "Optional HMM states", "10min", "pipeline output group", NA_character_, "Analysis/08_hmm_behavioral_states_optional.R", "exploratory", "legacy_pending_migration", "analysis_ready/06_behavioral_dynamics/hmm_states/", "Optional latent-state analysis; not primary evidence.",
  "09", "Early prediction", "10min", "pipeline output group", "analysis_ready/pipeline/09_early_prediction/10min/", "Analysis/09_early_prediction_model_ladder.R", "primary manuscript evidence", "producer_migrated_legacy_artifacts_retained", "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/10min_based/", "Canonical feature, model, source-data, and prediction tables.",
  "10", "Systems prediction", "10min", "pipeline output group", "analysis_ready/pipeline/10_systems_prediction/10min/", "Analysis/10_systems_feature_prediction_ladder.R", "exploratory systems extension", "producer_migrated_legacy_artifacts_retained", "analysis_ready/06_behavioral_dynamics/systems_feature_prediction_ladder/10min_based/", "Duplicate physical writes are suppressed; not promoted to primary evidence.",
  "11", "Adaptation kinetics", "10min", "pipeline output group", NA_character_, "Analysis/11_behavioral_adaptation_kinetics.R", "mechanistic/secondary", "legacy_pending_migration", "analysis_ready/15_behavioral_adaptation_kinetics/", "Not included without a selected manuscript result.",
  "12", "Sleep-like quiescence", "10min", "pipeline output group", NA_character_, "Analysis/12_sleep_like_quiescence_metrics.R", "secondary", "legacy_pending_migration", "analysis_ready/16_sleep_like_inactivity_metrics/", "Not EEG-validated; excluded from current Stage 16.",
  "13", "Phase organization", "10min", "pipeline output group", NA_character_, "Analysis/13_ethological_phase_organization.R", "mechanistic/secondary", "legacy_pending_migration", "analysis_ready/17_ethological_phase_organization/", "Not included without a selected manuscript result.",
  "14", "Systems summary", "5min", "pipeline output group", NA_character_, "Analysis/14_systems_neuroscience_summary_dashboard.R", "exploratory systems layer", "legacy_pending_migration", "analysis_ready/12_systems_neuroscience_summary/5min_based/", "Remains outside primary Stage 16 claims.",
  "15", "Behavior-proteomics integration", NA_character_, "pipeline output group", NA_character_, "Analysis/15_behavior_proteomics_integration.R", "separate exploratory evidentiary layer", "legacy_pending_migration", "analysis_ready/proteomics/", "Keep separate from the behavioral source-data workbook.",
  "16", "Manuscript behavior report", "10min", "manuscript reporting group", "analysis_ready/manuscript/behavior/", "Analysis/16_manuscript_behavior_report.R", "recommended manuscript entry point", "canonical", "analysis_ready/16_manuscript_behavior_report/10min_based/", "Exporter only; no statistical refitting or canonical-value recomputation.",
  "19", "Spatial occupancy", NA_character_, "pipeline output group", NA_character_, "Analysis/19_spatial_occupancy_maps.R", "secondary/spatial", "legacy_pending_migration", "analysis_ready/03_derived_metrics/spatial_occupancy/", "Position-level source and separate legacy publication paths; not included in Stage 16."
)
if (anyDuplicated(na.omit(output_index$canonical_path))) {
  stop("output_index.csv contains duplicate canonical paths.", call. = FALSE)
}
readr::write_csv(output_index, file.path(analysis_ready_dir, "output_index.csv"), na = "NA")
writeLines(
  c(
    "# Behavioral analysis outputs",
    "",
    "Start manuscript reporting at `manuscript/behavior/Behavioral_Source_Data.xlsx`.",
    "Machine-readable result, source-data, provenance, validation, and manifest CSVs are beside the workbook.",
    "",
    "Canonical migrated pipeline outputs live under `pipeline/` and use only `tables/`, `figures/`, and `audit/`.",
    "`output_index.csv` is the machine-readable map for migrated and not-yet-migrated stages.",
    "",
    "Readers resolve canonical paths first and a single documented legacy path second. Any legacy fallback is warned and recorded in manuscript provenance; no newest-file guessing is allowed.",
    "Historical output folders are retained and are not rewritten by Stage 16.",
    "",
    "Stage 09 is the primary prospective layer. Stage 03 is secondary phenotype/group characterization. Stage 10/14, HMM/state, nonlinear, systems-composite, spatial, and behavior-proteomics analyses remain exploratory or separate unless a later reporting decision promotes a specific result."
  ),
  file.path(analysis_ready_dir, "README.md")
)

wb <- openxlsx::createWorkbook(creator = "MMMSociability Stage 16")
openxlsx::modifyBaseFont(
  wb, fontSize = 9, fontName = "Arial", fontColour = stage16_palette$text
)
stage16_styles <- stage16_create_styles()
worksheet_data <- list(
  README = readme_sheet,
  Primary_results = primary_results_export,
  Primary_source_data = animal_level_source_data,
  Prediction_validation = prediction_validation,
  Prediction_source_data = prediction_source_data,
  Supplementary_results = supplementary_results_export,
  Movement_phase_source_data = movement_phase_source_data,
  QC = qc_exclusions,
  Feature_dictionary = feature_dictionary,
  Provenance = provenance,
  Validation = validation
)
expected_sheet_ranges <- vapply(worksheet_data, xlsx_used_range, character(1))
purrr::iwalk(worksheet_data, ~ write_sheet(wb, .y, .x, stage16_styles))
openxlsx::activeSheet(wb) <- "README"
prepare_openxlsx_package_metadata(wb, expected_sheet_ranges)
openxlsx::saveWorkbook(wb, artifact_paths[["workbook"]], overwrite = TRUE)

ooxml_validation <- validate_xlsx_package(
  artifact_paths[["workbook"]],
  expected_ranges = expected_sheet_ranges,
  expected_sheet_names = names(worksheet_data)
)

git_sha <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[1],
  error = function(e) NA_character_
)
if (length(git_sha) == 0L || is.na(git_sha) || !nzchar(git_sha)) git_sha <- NA_character_

manifest_inputs <- artifact_paths[names(artifact_paths) != "manifest"]
manifest_rows <- c(
  workbook = NA_integer_,
  primary = nrow(primary_results_export),
  supplementary = nrow(supplementary_results_export),
  animal_source = nrow(animal_level_source_data),
  prediction_source = nrow(prediction_source_data),
  movement_source = nrow(movement_phase_source_data),
  provenance = nrow(provenance),
  validation = nrow(validation)
)
manifest_roles <- c(
  workbook = "canonical manuscript workbook",
  primary = "primary results",
  supplementary = "supplementary results",
  animal_source = "animal-level source data",
  prediction_source = "held-out prediction source data",
  movement_source = "movement-phase source data",
  provenance = "source provenance",
  validation = "report validation"
)
manifest <- tibble(
  artifact = names(manifest_inputs),
  path = map_chr(unname(manifest_inputs), relative_to_analysis_ready),
  hash_algorithm = "SHA-256",
  sha256 = map_chr(unname(manifest_inputs), sha256_file),
  rows = unname(manifest_rows[names(manifest_inputs)]),
  role = unname(manifest_roles[names(manifest_inputs)]),
  generated_at,
  git_sha
)
readr::write_csv(manifest, artifact_paths[["manifest"]], na = "NA")

# Reload final artifacts and verify that workbook values agree with canonical CSVs.
primary_csv_check <- readr::read_csv(artifact_paths[["primary"]], show_col_types = FALSE)
supp_csv_check <- readr::read_csv(artifact_paths[["supplementary"]], show_col_types = FALSE)
animal_csv_check <- readr::read_csv(artifact_paths[["animal_source"]], show_col_types = FALSE)
prediction_source_csv_check <- readr::read_csv(artifact_paths[["prediction_source"]], show_col_types = FALSE)
movement_source_csv_check <- readr::read_csv(artifact_paths[["movement_source"]], show_col_types = FALSE)
provenance_csv_check <- readr::read_csv(artifact_paths[["provenance"]], show_col_types = FALSE)
validation_csv_check <- readr::read_csv(artifact_paths[["validation"]], show_col_types = FALSE)
primary_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Primary_results", na.strings = "NA")
supp_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Supplementary_results", na.strings = "NA")
animal_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Primary_source_data", na.strings = "NA")
prediction_source_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Prediction_source_data", na.strings = "NA")
movement_source_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Movement_phase_source_data", na.strings = "NA")
provenance_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Provenance", na.strings = "NA")
validation_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Validation", na.strings = "NA")
prediction_xlsx_check <- openxlsx::read.xlsx(artifact_paths[["workbook"]], sheet = "Prediction_validation", na.strings = "NA")

compare_export <- function(csv_dat, xlsx_dat, label) {
  if (!identical(names(csv_dat), names(xlsx_dat)) || nrow(csv_dat) != nrow(xlsx_dat)) {
    stop(label, " workbook/CSV shape or column mismatch.", call. = FALSE)
  }
  numeric_cols <- names(csv_dat)[vapply(csv_dat, is.numeric, logical(1))]
  for (nm in numeric_cols) {
    if (!isTRUE(all.equal(csv_dat[[nm]], as.numeric(xlsx_dat[[nm]]), tolerance = 1e-12, check.attributes = FALSE))) {
      stop(label, " workbook/CSV numeric mismatch in column ", nm, ".", call. = FALSE)
    }
  }
  nonnumeric_cols <- setdiff(names(csv_dat), numeric_cols)
  for (nm in nonnumeric_cols) {
    if (!identical(as.character(csv_dat[[nm]]), as.character(xlsx_dat[[nm]]))) {
      stop(label, " workbook/CSV text mismatch in column ", nm, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}
compare_export(primary_csv_check, primary_xlsx_check, "Primary results")
compare_export(supp_csv_check, supp_xlsx_check, "Supplementary results")
compare_export(animal_csv_check, animal_xlsx_check, "Animal-level source data")
compare_export(prediction_source_csv_check, prediction_source_xlsx_check, "Prediction source data")
compare_export(movement_source_csv_check, movement_source_xlsx_check, "Movement-phase source data")
compare_export(provenance_csv_check, provenance_xlsx_check, "Provenance")
compare_export(validation_csv_check, validation_xlsx_check, "Validation")
compare_export(animal_level_source_data, animal_csv_check, "Animal source lineage")
compare_export(prediction_source_data, prediction_source_csv_check, "Prediction source lineage")
compare_export(movement_phase_source_data, movement_source_csv_check, "Movement source lineage")

entropy_export_check <- primary_xlsx_check %>%
  filter(claim_id == entropy_primary_row$claim_id)
if (
  nrow(entropy_export_check) != 1L ||
  entropy_export_check$robustness_status != entropy_robustness_wording
) {
  stop("Primary_results did not preserve the Entropy_acf1 BH/CI robustness wording.", call. = FALSE)
}

# Numerically reconcile the manuscript workbook back to each canonical Stage 09
# source table, rather than relying only on agreement with the assembled CSVs.
association_check <- primary_xlsx_check %>%
  filter(analysis_domain == "Prospective feature association") %>%
  select(feature = source_row_key, workbook_estimate = estimate, workbook_ci_low = ci_low,
         workbook_ci_high = ci_high, workbook_p_raw = p_raw, workbook_p_adjusted = p_adjusted) %>%
  left_join(
    assoc %>% select(feature, spearman_rho, spearman_boot_ci_low, spearman_boot_ci_high, spearman_p, spearman_p_bh),
    by = "feature"
  )
if (
  any(is.na(association_check$spearman_rho)) ||
  !isTRUE(all.equal(association_check$workbook_estimate, association_check$spearman_rho, tolerance = 1e-12)) ||
  !isTRUE(all.equal(association_check$workbook_ci_low, association_check$spearman_boot_ci_low, tolerance = 1e-12)) ||
  !isTRUE(all.equal(association_check$workbook_ci_high, association_check$spearman_boot_ci_high, tolerance = 1e-12)) ||
  !isTRUE(all.equal(association_check$workbook_p_raw, association_check$spearman_p, tolerance = 1e-12)) ||
  !isTRUE(all.equal(association_check$workbook_p_adjusted, association_check$spearman_p_bh, tolerance = 1e-12))
) stop("Workbook primary associations disagree with the canonical Stage 09 source.", call. = FALSE)

prediction_check <- prediction_xlsx_check %>%
  select(
    model_id,
    workbook_cv_r2 = loao_cv_r2,
    workbook_rmse = loao_rmse,
    workbook_mae = loao_mae,
    workbook_pearson_r = loao_pearson_r,
    workbook_spearman_rho = loao_spearman_rho,
    interpretation
  ) %>%
  left_join(performance %>% select(model_id, cv_r2, rmse, mae, pearson_r, spearman_rho), by = "model_id")
if (
  nrow(prediction_check) != nrow(performance) || any(is.na(prediction_check$cv_r2)) ||
  !isTRUE(all.equal(prediction_check$workbook_cv_r2, prediction_check$cv_r2, tolerance = 1e-12)) ||
  !isTRUE(all.equal(prediction_check$workbook_rmse, prediction_check$rmse, tolerance = 1e-12)) ||
  !isTRUE(all.equal(prediction_check$workbook_mae, prediction_check$mae, tolerance = 1e-12))
) stop("Workbook prediction values disagree with the canonical Stage 09 performance source.", call. = FALSE)

baseline_prediction_check <- prediction_check %>% filter(model_id == "mean_only")
nonbaseline_prediction_check <- prediction_check %>% filter(model_id != "mean_only")
if (
  nrow(baseline_prediction_check) != 1L ||
  !is.na(baseline_prediction_check$workbook_pearson_r) ||
  !is.na(baseline_prediction_check$workbook_spearman_rho) ||
  !str_detect(baseline_prediction_check$interpretation, fixed("not interpretable"))
) stop("Workbook intercept-only correlation display is not correctly marked as non-interpretable.", call. = FALSE)
if (
  !isTRUE(all.equal(
    nonbaseline_prediction_check$workbook_pearson_r,
    nonbaseline_prediction_check$pearson_r,
    tolerance = 1e-12,
    check.attributes = FALSE
  )) ||
  !isTRUE(all.equal(
    nonbaseline_prediction_check$workbook_spearman_rho,
    nonbaseline_prediction_check$spearman_rho,
    tolerance = 1e-12,
    check.attributes = FALSE
  ))
) stop("Workbook changed a non-baseline prediction correlation.", call. = FALSE)

interaction_check <- primary_xlsx_check %>%
  filter(analysis_domain == "Formal feature-by-Sex interaction") %>%
  select(feature = source_row_key, workbook_estimate = estimate, workbook_ci_low = ci_low,
         workbook_ci_high = ci_high, workbook_p_raw = p_raw, workbook_p_adjusted = p_adjusted) %>%
  left_join(
    sex_interactions %>% select(feature, interaction_estimate, interaction_ci_low, interaction_ci_high, interaction_p, interaction_p_bh),
    by = "feature"
  )
if (
  any(is.na(interaction_check$interaction_estimate)) ||
  !isTRUE(all.equal(interaction_check$workbook_estimate, interaction_check$interaction_estimate, tolerance = 1e-12)) ||
  !isTRUE(all.equal(interaction_check$workbook_ci_low, interaction_check$interaction_ci_low, tolerance = 1e-12)) ||
  !isTRUE(all.equal(interaction_check$workbook_ci_high, interaction_check$interaction_ci_high, tolerance = 1e-12)) ||
  !isTRUE(all.equal(interaction_check$workbook_p_raw, interaction_check$interaction_p, tolerance = 1e-12)) ||
  !isTRUE(all.equal(interaction_check$workbook_p_adjusted, interaction_check$interaction_p_bh, tolerance = 1e-12))
) stop("Workbook interaction values disagree with the canonical Stage 09 source.", call. = FALSE)

expected_sheets <- names(worksheet_data)
actual_sheets <- openxlsx::getSheetNames(artifact_paths[["workbook"]])
if (!identical(actual_sheets, expected_sheets)) {
  stop("Workbook sheet registry mismatch.", call. = FALSE)
}
if (!all(file.exists(artifact_paths))) stop("One or more required Stage 16 artifacts were not written.", call. = FALSE)

source_hash_after_export <- setNames(
  map_chr(source_registry$path, sha256_file),
  source_registry$source_id
)
if (!identical(source_hash_before, source_hash_after_export)) {
  stop("An upstream Stage 03/09/QC source changed during Stage 16 export.", call. = FALSE)
}

message("Stage 16 manuscript behavior report complete: ", output_dir)
message("Workbook sheets: ", paste(actual_sheets, collapse = ", "))
message(
  "Rows: primary = ", nrow(primary_results_export),
  ", supplementary = ", nrow(supplementary_results_export),
  ", animal source = ", nrow(animal_level_source_data),
  ", prediction source = ", nrow(prediction_source_data),
  ", movement source = ", nrow(movement_phase_source_data),
  ", validation = ", nrow(validation)
)
message("Canonical primary features: ", paste(canonical_features, collapse = ", "))
message("Primary prediction models contain Group: FALSE")
message("Workbook/CSV numeric agreement: PASS")
message("Source-data lineage agreement: PASS")
message("Upstream source hashes stable during export: PASS")
message("OOXML dangling relationships: ", ooxml_validation$dangling_relationships)
message("OOXML external relationships: ", ooxml_validation$external_relationships)
message("OOXML macro parts: ", ooxml_validation$macro_parts)
message(
  "Worksheet used ranges: ",
  paste0(ooxml_validation$sheet_ranges$worksheet, "=", ooxml_validation$sheet_ranges$used_range, collapse = "; ")
)
message(
  "Worksheet autofilter ranges: ",
  paste0(ooxml_validation$sheet_ranges$worksheet, "=", ooxml_validation$sheet_ranges$autofilter_range, collapse = "; ")
)
