# ================================================================
# Early Behavioral Prediction Model Ladder
# MMMSociability
# ================================================================
# Goal:
#   Test whether early first-active-phase behavior after the first cage
#   change predicts later composite stress burden (CombZ), with explicit
#   model comparison between conventional magnitude features and temporal
#   organization features.
#
# Biological use case:
#   P25 first cage change, first 12 h active phase, bin size set below.
#   Primary features: Movement_mean and Entropy_acf1.
#
# Main question:
#   Does early entropy persistence refine prediction of later CombZ beyond
#   movement, while keeping CON/RES/SUS grouping descriptive unless explicitly
#   used as an adjustment/sensitivity term?
#
# Input expectation:
#   Run Analysis/03_build_multiscale_behavior_metrics.R first.
#   The endpoint file must contain AnimalNum and CombZ or your selected
#   outcome column.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
})

source("C:/Users/topohl/Documents/GitHub/MMMSociability/Functions/behavioral_dynamics_helpers.R")
source("C:/Users/topohl/Documents/GitHub/MMMSociability/Functions/behavioral_dynamics_stats_helpers.R")
source("C:/Users/topohl/Documents/GitHub/MMMSociability/Functions/duration_normalization_helpers.R")

# ------------------------------------------------
# USER INPUT
# ------------------------------------------------

bin_level <- "10min_based"
base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
input_file <- file.path(base_dir, "analysis_ready/03_derived_metrics", bin_level, "all_behavior_metrics.csv")
output_dir <- file.path(base_dir, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder", bin_level)

# Endpoint file should contain one row per animal or repeated rows with a stable endpoint.
# If NULL, the script tries to read the outcome from input_file.
endpoint_file <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
endpoint_excel_sheet <- "zScore"  # sheet name for Excel endpoint files; NULL = first sheet
outcome_col <- "CombZ"

# Primary prospective window: first 12 h active phase after the first cage change.
early_phase_pattern <- "active|dark|night"
first_cage_change_only <- TRUE
early_window_hours <- 12
bin_size_min <- 10
max_early_bins_per_animal <- early_window_hours * 60 / bin_size_min

# Optional column overrides. Leave NULL for automatic detection by helper functions.
animal_col <- NULL
time_col <- NULL
group_col <- NULL
sex_col <- NULL
phase_col <- NULL
cage_col <- NULL
movement_col <- NULL
entropy_col <- NULL
proximity_col <- "ProximityFraction"

# Model options.
n_prediction_permutations <- 5000
n_bootstrap <- 5000
set.seed(123)

group_colors <- c(
  "CON" = "#3d3b6e",
  "RES" = "#C6C3BB",
  "SUS" = "#e63947",
  "All" = "grey55"
)
group_levels <- c("CON", "RES", "SUS")
group_shape_values <- c("CON" = 21, "RES" = 22, "SUS" = 24, "All" = 21)

# ------------------------------------------------
# SMALL HELPERS
# ------------------------------------------------

ensure_dir_safe <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

write_text_file <- function(lines, path) {
  ensure_dir_safe(dirname(path))
  writeLines(lines, con = path)
  invisible(path)
}

safe_scale <- function(x) {
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = method))
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "p=NA",
    p < 0.001 ~ "p<.001",
    TRUE ~ paste0("p=", sub("^0", "", formatC(p, format = "f", digits = 3)))
  )
}

make_publication_theme <- function(base_size = 7) {
  make_nature_theme(base_size = base_size) +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_line(linewidth = 0.15, colour = "grey92"),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0, size = base_size + 1),
      plot.subtitle = element_text(hjust = 0, size = base_size)
    )
}

feature_display_labels <- c(
  "Movement_mean" = "Movement",
  "Entropy_acf1" = "Entropy persistence",
  "Movement_x_EntropyACF1" = "Movement x entropy"
)

model_display_labels <- c(
  "Mean only" = "Mean only",
  "Sex + Group" = "Sex + group",
  "Movement only" = "Movement",
  "Entropy ACF1 only" = "Entropy persistence",
  "Movement + Entropy ACF1" = "Movement + entropy",
  "Movement x Entropy ACF1" = "Movement x entropy",
  "Full behavior compact" = "Compact behavior"
)

matched_ladder_model_labels <- c(
  "Mean only" = "Mean only",
  "Movement mean" = "Movement mean",
  "Movement + entropy persistence" = "Movement + entropy persistence",
  "Primary behavior family" = "Primary behavior family",
  "Compact behavior dynamics" = "Compact behavior dynamics"
)

matched_ladder_adjustment_labels <- c(
  "Behavior only" = "Behavior only",
  "Behavior + Sex" = "Behavior + Sex",
  "Behavior + Sex + Group" = "Behavior + Sex + Group"
)

matched_ladder_use_labels <- c(
  "Behavior only" = "Primary prospective evidence",
  "Behavior + Sex" = "Sex-adjusted sensitivity",
  "Behavior + Sex + Group" = "Group-adjusted contextual/supplementary analyses"
)

matched_ladder_fill_values <- c(
  "Primary prospective evidence" = "#2F4858",
  "Sex-adjusted sensitivity" = "#7A8F6A",
  "Group-adjusted contextual/supplementary analyses" = "#8A817C"
)

classify_model_reporting_use <- function(model_name) {
  case_when(
    model_name == "Mean only" ~ "Reference baseline",
    model_name %in% c("Sex + Group") ~ "Descriptive group/sex adjustment",
    model_name %in% c(
      "Movement only",
      "Entropy ACF1 only",
      "Movement + Entropy ACF1",
      "Movement x Entropy ACF1",
      "Full behavior compact"
    ) ~ "Behavior plus sex/group adjustment",
    TRUE ~ "Sensitivity"
  )
}

wilcox_effect_r <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  wt <- suppressWarnings(try(stats::wilcox.test(y, x, exact = FALSE), silent = TRUE))
  if (inherits(wt, "try-error")) return(NA_real_)
  z <- stats::qnorm(wt$p.value / 2, lower.tail = FALSE) * sign(median(y, na.rm = TRUE) - median(x, na.rm = TRUE))
  z / sqrt(length(x) + length(y))
}

get_first_cage_change <- function(x) {
  ux <- unique(as.character(x))
  cc_num <- suppressWarnings(as.numeric(str_extract(ux, "\\d+")))
  if (any(is.finite(cc_num))) ux[which.min(ifelse(is.finite(cc_num), cc_num, Inf))] else sort(ux)[1]
}

impute_numeric <- function(dat, cols) {
  dat %>%
    mutate(across(all_of(cols), ~{
      med <- median(.x, na.rm = TRUE)
      if (!is.finite(med)) med <- 0
      replace_na(.x, med)
    }))
}

prediction_metrics <- function(observed, predicted) {
  ok <- is.finite(observed) & is.finite(predicted)
  y <- observed[ok]
  p <- predicted[ok]
  baseline <- mean(y, na.rm = TRUE)
  tibble(
    n = length(y),
    pearson_r = safe_cor(y, p, "pearson"),
    spearman_rho = safe_cor(y, p, "spearman"),
    rmse = sqrt(mean((y - p)^2, na.rm = TRUE)),
    mae = mean(abs(y - p), na.rm = TRUE),
    baseline_rmse = sqrt(mean((y - baseline)^2, na.rm = TRUE)),
    cv_r2_vs_mean = 1 - sum((y - p)^2, na.rm = TRUE) / sum((y - baseline)^2, na.rm = TRUE)
  )
}

permutation_prediction_p <- function(observed, predicted, n_perm = 5000, seed = 123) {
  ok <- is.finite(observed) & is.finite(predicted)
  y <- observed[ok]
  p <- predicted[ok]
  if (length(y) < 4 || sd(y) == 0 || sd(p) == 0) return(NA_real_)
  r_obs <- suppressWarnings(cor(y, p, method = "pearson"))
  set.seed(seed)
  r_null <- replicate(n_perm, suppressWarnings(cor(y, sample(p), method = "pearson")))
  (sum(abs(r_null) >= abs(r_obs), na.rm = TRUE) + 1) / (sum(is.finite(r_null)) + 1)
}

make_formula <- function(outcome = "outcome", predictors) {
  if (length(predictors) == 0) as.formula(paste0(outcome, " ~ 1")) else as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
}

sanitize_model_predictors <- function(predictors, dat, outcome = "outcome") {
  predictors <- unique(as.character(predictors))
  predictors <- predictors[!is.na(predictors) & predictors != ""]
  known_endpoint_cols <- if (exists("endpoint_cols")) endpoint_cols else character(0)
  leakage_cols <- unique(c(
    outcome,
    "outcome", "observed", "predicted", "residual", "abs_residual",
    outcome_col,
    known_endpoint_cols
  ))
  predictors <- setdiff(predictors, leakage_cols)
  predictors <- predictors[predictors %in% names(dat)]
  predictors
}

loo_lm_predict <- function(dat, predictors, model_name) {
  predictors <- sanitize_model_predictors(predictors, dat, outcome = "outcome")
  pred <- rep(NA_real_, nrow(dat))
  coef_rows <- list()

  for (i in seq_len(nrow(dat))) {
    train <- dat[-i, , drop = FALSE]
    test <- dat[i, , drop = FALSE]
    form <- make_formula("outcome", predictors)
    fit <- try(lm(form, data = train), silent = TRUE)
    if (inherits(fit, "try-error")) next
    pred[i] <- tryCatch(as.numeric(predict(fit, newdata = test)), error = function(e) NA_real_)
    coef_rows[[i]] <- broom_like_coef(fit, held_out_animal = dat$AnimalNum[i], model_name = model_name)
  }

  pred_tbl <- dat %>%
    transmute(
      AnimalNum, Group, Sex, BinLevel, ProximityInput,
      observed = outcome,
      predicted = pred,
      residual = observed - predicted,
      abs_residual = abs(residual),
      Model = model_name
    )

  list(predictions = pred_tbl, coefficients = bind_rows(coef_rows))
}

broom_like_coef <- function(fit, held_out_animal, model_name) {
  sm <- try(summary(fit)$coefficients, silent = TRUE)
  if (inherits(sm, "try-error")) return(tibble())
  tibble(
    held_out_animal = held_out_animal,
    Model = model_name,
    term = rownames(sm),
    estimate = sm[, 1],
    std_error = sm[, 2],
    statistic = sm[, 3],
    p_value = sm[, 4]
  )
}

bootstrap_correlation_ci <- function(dat, x_col, y_col = "outcome", n_boot = 5000, method = "spearman", seed = 123) {
  if (!x_col %in% names(dat)) {
    return(tibble(feature = x_col, n = nrow(dat), estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  ok_dat <- dat %>% filter(is.finite(.data[[x_col]]), is.finite(.data[[y_col]]))
  if (nrow(ok_dat) < 4 || sd(ok_dat[[x_col]]) == 0 || sd(ok_dat[[y_col]]) == 0) {
    return(tibble(feature = x_col, n = nrow(ok_dat), estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  set.seed(seed)
  boots <- replicate(n_boot, {
    idx <- sample(seq_len(nrow(ok_dat)), replace = TRUE)
    safe_cor(ok_dat[[x_col]][idx], ok_dat[[y_col]][idx], method = method)
  })
  tibble(
    feature = x_col,
    n = nrow(ok_dat),
    estimate = safe_cor(ok_dat[[x_col]], ok_dat[[y_col]], method = method),
    ci_low = quantile(boots, 0.025, na.rm = TRUE, names = FALSE),
    ci_high = quantile(boots, 0.975, na.rm = TRUE, names = FALSE),
    n_bootstrap = n_boot,
    method = method
  )
}

correlation_test_stats <- function(dat, x_col, y_col, label = NULL) {
  ok <- is.finite(dat[[x_col]]) & is.finite(dat[[y_col]])
  x <- dat[[x_col]][ok]
  y <- dat[[y_col]][ok]
  out_label <- if (is.null(label)) x_col else label
  if (length(x) < 4 || sd(x) == 0 || sd(y) == 0) {
    return(tibble(
      label = out_label,
      n = length(x),
      pearson_r = NA_real_,
      pearson_p = NA_real_,
      spearman_rho = NA_real_,
      spearman_p = NA_real_
    ))
  }
  pearson <- suppressWarnings(cor.test(x, y, method = "pearson"))
  spearman <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  tibble(
    label = out_label,
    n = length(x),
    pearson_r = unname(pearson$estimate),
    pearson_p = pearson$p.value,
    spearman_rho = unname(spearman$estimate),
    spearman_p = spearman$p.value
  )
}

partial_r_from_lm <- function(dat, feature, covariates, outcome = "outcome") {
  needed <- c(outcome, feature, covariates)
  needed <- needed[needed %in% names(dat)]
  d <- dat %>% select(all_of(needed)) %>% drop_na()
  if (nrow(d) < 6 || !feature %in% names(d)) return(NA_real_)
  covariates <- sanitize_model_predictors(covariates, d, outcome = outcome)
  covariates <- setdiff(covariates, feature)
  if (length(covariates) == 0) return(safe_cor(d[[feature]], d[[outcome]], "pearson"))
  ry <- residuals(lm(make_formula(outcome, covariates), data = d))
  rx <- residuals(lm(make_formula(feature, covariates), data = d))
  safe_cor(rx, ry, "pearson")
}

# ------------------------------------------------
# LOAD DATA
# ------------------------------------------------

raw_dat <- read_behavior_table(input_file)
if (!proximity_col %in% names(raw_dat)) proximity_col <- "Proximity"

behav <- standardize_behavior_columns(
  raw_dat,
  animal_col = animal_col,
  time_col = time_col,
  group_col = group_col,
  sex_col = sex_col,
  phase_col = phase_col,
  cage_col = cage_col,
  movement_col = movement_col,
  entropy_col = entropy_col,
  proximity_col = proximity_col
) %>%
  mutate(
    Group = factor(as.character(Group), levels = unique(c(group_levels, sort(unique(as.character(Group))))))
  )

ensure_dir_safe(output_dir)
ensure_dir_safe(file.path(output_dir, "tables"))
ensure_dir_safe(file.path(output_dir, "tables", "documentation"))
ensure_dir_safe(file.path(output_dir, "tables", "design"))
ensure_dir_safe(file.path(output_dir, "tables", "features"))
ensure_dir_safe(file.path(output_dir, "tables", "models"))
ensure_dir_safe(file.path(output_dir, "tables", "statistics"))
ensure_dir_safe(file.path(output_dir, "tables", "sensitivity"))
ensure_dir_safe(file.path(output_dir, "figures"))
ensure_dir_safe(file.path(output_dir, "figures", "publication"))
output_dirs <- analysis_output_dirs(output_dir)
write_output_manifest(
  output_dir,
  script_name = "08b_early_prediction_model_ladder.R",
  analysis_name = "early prediction model ladder",
  primary_tables = c(
    "tables/model_ladder_performance.csv",
    "tables/model_ladder_performance_duration_sensitivity.csv",
    "tables/model_ladder_repeated_grouped_kfold_performance.csv",
    "tables/models/matched_ladder_performance.csv",
    "tables/models/matched_ladder_repeated_grouped_kfold_performance.csv",
    "tables/documentation/matched_ladder_predictor_audit.csv",
    "tables/prediction_interpretation_constraints.csv",
    "tables/model_ladder_incremental_summary.csv",
    "tables/primary_movement_entropyacf1_associations.csv",
    "tables/statistics/primary_movement_entropyacf1_correlations_by_sex.csv",
    "tables/models/model_ladder_prediction_correlations.csv",
    "tables/models/matched_ladder_prediction_correlations.csv",
    "tables/primary_feature_group_summary.csv",
    "tables/primary_feature_group_contrasts_descriptive.csv",
    "tables/documentation/readout_dictionary.csv",
    "tables/documentation/model_specification_dictionary.csv",
    "tables/documentation/output_table_catalog.csv"
  ),
  primary_figures = c(
    "figures/publication/model_ladder_cv_r2.svg",
    "figures/publication/matched_ladder_behavior_only_cv_r2.svg",
    "figures/publication/matched_ladder_behavior_plus_sex_cv_r2.svg",
    "figures/publication/matched_ladder_behavior_plus_sex_group_cv_r2.svg",
    "figures/publication/matched_ladder_covariate_comparison_cv_r2.svg",
    "figures/publication/behavior_only_repeated_cv_ladder.svg",
    "figures/publication/primary_movement_entropyacf1_vs_combz.svg",
    "figures/publication/model_ladder_prediction_correlations.svg",
    "figures/publication/matched_ladder_prediction_correlations.svg"
  ),
  notes = c("Main prediction claim should use the ladder performance plus duration-sensitivity companion table.")
)

write_text_file(
  c(
    "08b early prediction model ladder",
    "",
    "Purpose:",
    "This analysis tests whether early first-active-phase behavioral features predict later CombZ.",
    "",
    "Recommended reading order:",
    "1. tables/documentation/analysis_readme.txt",
    "2. tables/documentation/model_specification_dictionary.csv",
    "3. tables/documentation/readout_dictionary.csv",
    "4. tables/model_ladder_repeated_grouped_kfold_performance.csv",
    "5. tables/model_ladder_performance.csv",
    "6. tables/models/matched_ladder_performance.csv",
    "7. tables/documentation/matched_ladder_predictor_audit.csv",
    "8. figures/publication/matched_ladder_covariate_comparison_cv_r2.svg",
    "9. figures/publication/behavior_only_repeated_cv_ladder.svg",
    "10. figures/publication/model_ladder_prediction_correlations.svg",
    "11. figures/publication/matched_ladder_prediction_correlations.svg",
    "12. figures/publication/primary_movement_entropyacf1_vs_combz.svg",
    "",
    "Interpretation:",
    "Behavior-only repeated grouped CV is the primary prospective evidence.",
    "Matched ladders separate behavior-only, Sex-adjusted, and Sex + Group-adjusted sensitivity analyses using identical behavior feature sets.",
    "CON/RES/SUS group labels are shown for interpretation and descriptive summaries.",
    "Models containing Group should be treated as descriptive adjustment/sensitivity analyses."
  ),
  file.path(output_dir, "tables", "documentation", "analysis_readme.txt")
)

epoch_duration_qc <- write_epoch_duration_qc(behav, output_dir, metric_source = "08b_early_prediction_model_ladder", bin_size_sec = infer_bin_size_sec(behav))

# ------------------------------------------------
# EARLY WINDOW: FIRST CAGE CHANGE, FIRST 12 h ACTIVE PHASE
# ------------------------------------------------

if (first_cage_change_only && "CageChange" %in% names(behav)) {
  first_cc <- get_first_cage_change(behav$CageChange)
  behav <- behav %>% filter(as.character(CageChange) == first_cc)
} else {
  first_cc <- "all"
}

has_active_phase <- any(str_detect(str_to_lower(as.character(behav$Phase)), early_phase_pattern))
early_dat <- if (has_active_phase) {
  behav %>% filter(str_detect(str_to_lower(as.character(Phase)), early_phase_pattern))
} else {
  behav
}

early_dat <- early_dat %>%
  group_by(AnimalNum, Phase) %>%
  arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(early_rank = row_number()) %>%
  filter(early_rank <= max_early_bins_per_animal) %>%
  ungroup() %>%
  mutate(BinLevel = bin_level, ProximityInput = proximity_col)

write_table(early_dat, file.path(output_dir, "tables", "early_window_rows_used.csv"))
write_table(early_dat, file.path(output_dir, "tables", "design", "early_window_rows_used.csv"))
write_table(filter_short_duration_epochs(early_dat, epoch_duration_qc), file.path(output_dir, "tables", "early_window_rows_used_excluding_short_duration.csv"))
write_table(filter_short_duration_epochs(early_dat, epoch_duration_qc), file.path(output_dir, "tables", "design", "early_window_rows_used_excluding_short_duration.csv"))

window_design_tbl <- early_dat %>%
  group_by(AnimalNum, Group, Sex, Phase) %>%
  summarise(
    n_bins = n(),
    approx_hours = n_bins * bin_size_min / 60,
    first_time_index = min(TimeIndex, na.rm = TRUE),
    last_time_index = max(TimeIndex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    BinLevel = bin_level,
    FirstCageChangeOnly = first_cage_change_only,
    FirstCageChange = first_cc,
    TargetWindowHours = early_window_hours
  )

write_table(window_design_tbl, file.path(output_dir, "tables", "early_window_design_by_animal.csv"))
write_table(window_design_tbl, file.path(output_dir, "tables", "design", "early_window_design_by_animal.csv"))

# ------------------------------------------------
# FEATURE EXTRACTION
# ------------------------------------------------

feature_long <- early_dat %>%
  pivot_longer(cols = c(Movement, Entropy, Proximity), names_to = "Metric", values_to = "Value") %>%
  group_by(AnimalNum, Group, Sex, Metric) %>%
  arrange(TimeIndex, .by_group = TRUE) %>%
  summarise(calc_instability_metrics(Value), .groups = "drop")

early_duration_by_animal <- early_dat %>%
  join_duration_qc(epoch_duration_qc) %>%
  distinct(AnimalNum, Group, Sex, CageChange, Phase, .keep_all = TRUE) %>%
  group_by(AnimalNum, Group, Sex) %>%
  summarise(
    early_observed_bins = sum(observed_bins, na.rm = TRUE),
    early_observation_hours = sum(total_observation_duration_hours, na.rm = TRUE),
    min_duration_completeness_fraction = min(duration_completeness_fraction, na.rm = TRUE),
    contains_short_duration_epoch = any(short_epoch %in% TRUE | cage_change_duration_class == "short", na.rm = TRUE),
    .groups = "drop"
  )

feature_wide <- feature_long %>%
  pivot_wider(
    id_cols = c(AnimalNum, Group, Sex),
    names_from = Metric,
    values_from = c(mean, sd, cv, fano, rmssd, acf1, p95, max),
    names_glue = "{Metric}_{.value}"
  ) %>%
  mutate(
    BinLevel = bin_level,
    ProximityInput = proximity_col,
    EarlyWindow = paste0("first_", early_window_hours, "h_active_first_cage_change"),
    Movement_z = safe_scale(Movement_mean),
    EntropyACF1_z = safe_scale(Entropy_acf1),
    Movement_x_EntropyACF1 = Movement_z * EntropyACF1_z
  ) %>%
  left_join(early_duration_by_animal, by = c("AnimalNum", "Group", "Sex"))

write_table(feature_long, file.path(output_dir, "tables", "early_behavior_features_long.csv"))
write_table(feature_wide, file.path(output_dir, "tables", "early_behavior_features_wide.csv"))
write_table(feature_wide %>% filter(!contains_short_duration_epoch %in% TRUE), file.path(output_dir, "tables", "early_behavior_features_wide_excluding_short_duration.csv"))
write_table(feature_long, file.path(output_dir, "tables", "features", "early_behavior_features_long.csv"))
write_table(feature_wide, file.path(output_dir, "tables", "features", "early_behavior_features_wide.csv"))
write_table(feature_wide %>% filter(!contains_short_duration_epoch %in% TRUE), file.path(output_dir, "tables", "features", "early_behavior_features_wide_excluding_short_duration.csv"))

readout_dictionary <- tibble(
  readout = c(
    "Movement_mean",
    "Movement_rmssd",
    "Movement_acf1",
    "Entropy_mean",
    "Entropy_rmssd",
    "Entropy_acf1",
    "Proximity_mean",
    "Proximity_rmssd",
    "Proximity_acf1",
    "Movement_x_EntropyACF1",
    "outcome",
    "Group",
    "Sex",
    "early_observation_hours",
    "contains_short_duration_epoch"
  ),
  display_label = c(
    "Mean movement",
    "Movement RMSSD",
    "Movement ACF1",
    "Mean entropy",
    "Entropy RMSSD",
    "Entropy persistence",
    "Mean proximity",
    "Proximity RMSSD",
    "Proximity ACF1",
    "Movement x entropy persistence",
    outcome_col,
    "CON/RES/SUS group",
    "Sex",
    "Observed early-window hours",
    "Short-duration epoch flag"
  ),
  domain = c(
    "Psychomotor magnitude",
    "Psychomotor dynamics",
    "Psychomotor temporal persistence",
    "Spatial organization",
    "Spatial organization dynamics",
    "Temporal organization",
    "Social organization",
    "Social dynamics",
    "Social temporal persistence",
    "Interaction term",
    "Endpoint",
    "Endpoint-derived/descriptive grouping",
    "Covariate",
    "Duration/QC",
    "Duration/QC"
  ),
  definition = c(
    "Animal-level mean movement in the early window.",
    "Root mean squared successive difference of movement across early-window bins.",
    "Lag-1 autocorrelation of movement across early-window bins.",
    "Animal-level mean position entropy in the early window.",
    "Root mean squared successive difference of entropy across early-window bins.",
    "Lag-1 autocorrelation of entropy; higher values indicate stronger persistence of spatial organization across bins.",
    "Animal-level mean selected proximity input in the early window.",
    "Root mean squared successive difference of proximity across early-window bins.",
    "Lag-1 autocorrelation of proximity across early-window bins.",
    "Product of z-scored Movement_mean and z-scored Entropy_acf1.",
    "Later stress burden endpoint used as the prediction target.",
    "Displayed for biological interpretation; not required for behavior-only prediction claims.",
    "Sex covariate used in adjusted/sensitivity models.",
    "Approximate duration contributing to the early-window feature estimates.",
    "TRUE when an animal includes a short-duration epoch flagged by duration QC."
  ),
  manuscript_role = c(
    "Primary baseline feature",
    "Compact behavior feature",
    "Compact behavior feature",
    "Compact behavior feature",
    "Compact behavior feature",
    "Primary temporal organization feature",
    "Compact behavior feature",
    "Compact behavior feature",
    "Compact behavior feature",
    "Exploratory interaction",
    "Prediction target",
    "Descriptive grouping and plotting",
    "Adjustment/sensitivity covariate",
    "QC/sensitivity",
    "QC/sensitivity"
  )
)
write_table(readout_dictionary, file.path(output_dir, "tables", "documentation", "readout_dictionary.csv"))

# ------------------------------------------------
# ENDPOINT HANDLING
# ------------------------------------------------

endpoint_dat <- NULL
if (!is.null(endpoint_file) && file.exists(endpoint_file)) {
  ext <- tools::file_ext(endpoint_file) %>% tolower()
  endpoint_raw <- if (ext %in% c("xlsx", "xls") && !is.null(endpoint_excel_sheet)) {
    readxl::read_excel(endpoint_file, sheet = endpoint_excel_sheet)
  } else {
    read_behavior_table(endpoint_file)
  }
  endpoint_animal_col <- first_existing_col(endpoint_raw, c("AnimalNum", "Animal", "MouseID", "Mouse", "ID", "RFID", "animal_id"), TRUE, "endpoint animal column")
  endpoint_dat <- endpoint_raw %>%
    transmute(AnimalNum = .data[[endpoint_animal_col]], outcome = suppressWarnings(as.numeric(.data[[outcome_col]]))) %>%
    group_by(AnimalNum) %>%
    summarise(outcome = first(na.omit(outcome)), .groups = "drop")
} else if (outcome_col %in% names(raw_dat)) {
  endpoint_animal_col <- first_existing_col(raw_dat, c("AnimalNum", "Animal", "MouseID", "Mouse", "ID", "RFID", "animal_id"), TRUE, "endpoint animal column")
  endpoint_dat <- raw_dat %>%
    group_by(AnimalNum = .data[[endpoint_animal_col]]) %>%
    summarise(outcome = first(na.omit(suppressWarnings(as.numeric(.data[[outcome_col]])))), .groups = "drop")
}

if (is.null(endpoint_dat)) {
  stop("No endpoint data found. Set endpoint_file or ensure outcome_col is present in the input file.")
}

model_dat <- feature_wide %>%
  left_join(endpoint_dat, by = "AnimalNum") %>%
  filter(is.finite(outcome)) %>%
  mutate(
    Sex = factor(as.character(Sex)),
    Group = factor(as.character(Group), levels = unique(c(group_levels, sort(unique(as.character(Group))))))
  )

if (nrow(model_dat) < 8) stop("Fewer than 8 animals with endpoint data. Model ladder not reliable.")

write_table(model_dat, file.path(output_dir, "tables", "model_ladder_input.csv"))
write_table(model_dat, file.path(output_dir, "tables", "models", "model_ladder_input.csv"))

# ------------------------------------------------
# PRIMARY FEATURE ASSOCIATIONS
# ------------------------------------------------

primary_features <- c("Movement_mean", "Entropy_acf1", "Movement_x_EntropyACF1")
primary_features <- primary_features[primary_features %in% names(model_dat)]

primary_assoc <- map_dfr(primary_features, function(fc) {
  cor_s <- suppressWarnings(cor.test(model_dat[[fc]], model_dat$outcome, method = "spearman", exact = FALSE))
  cor_p <- suppressWarnings(cor.test(model_dat[[fc]], model_dat$outcome, method = "pearson"))
  boot <- bootstrap_correlation_ci(model_dat, fc, "outcome", n_bootstrap, method = "spearman")
  tibble(
    feature = fc,
    n = sum(is.finite(model_dat[[fc]]) & is.finite(model_dat$outcome)),
    spearman_rho = unname(cor_s$estimate),
    spearman_p = cor_s$p.value,
    pearson_r = unname(cor_p$estimate),
    pearson_p = cor_p$p.value,
    spearman_boot_ci_low = boot$ci_low,
    spearman_boot_ci_high = boot$ci_high,
    partial_r_controlling_movement = if_else(fc == "Entropy_acf1", partial_r_from_lm(model_dat, fc, c("Movement_mean", "Sex", "Group")), NA_real_),
    BinLevel = bin_level,
    Outcome = outcome_col
  )
}) %>%
  mutate(
    spearman_p_bh = p.adjust(spearman_p, method = "BH"),
    Evidence = case_when(
      spearman_p_bh < 0.05 & sign(spearman_boot_ci_low) == sign(spearman_boot_ci_high) ~ "FDR-supported; bootstrap CI excludes zero",
      spearman_p < 0.05 ~ "nominal",
      TRUE ~ "uncertain"
    )
  ) %>%
  arrange(spearman_p_bh)

write_table(primary_assoc, file.path(output_dir, "tables", "primary_movement_entropyacf1_associations.csv"))
write_table(primary_assoc, file.path(output_dir, "tables", "statistics", "primary_movement_entropyacf1_associations.csv"))

primary_group_summary <- model_dat %>%
  select(AnimalNum, Group, Sex, outcome, all_of(primary_features)) %>%
  pivot_longer(cols = all_of(primary_features), names_to = "feature", values_to = "value") %>%
  group_by(feature, Group) %>%
  summarise(
    n_animals = n_distinct(AnimalNum[is.finite(value)]),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    sem = sd / sqrt(n_animals),
    median = median(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE, names = FALSE),
    q75 = quantile(value, 0.75, na.rm = TRUE, names = FALSE),
    mean_outcome = mean(outcome, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    feature_label = recode(feature, !!!feature_display_labels),
    Group = factor(as.character(Group), levels = group_levels),
    ReportingRole = "Descriptive CON/RES/SUS distribution; group labels are not needed for the primary behavior-only prediction claim"
  ) %>%
  arrange(feature, Group)

primary_group_contrasts <- model_dat %>%
  select(AnimalNum, Group, all_of(primary_features)) %>%
  pivot_longer(cols = all_of(primary_features), names_to = "feature", values_to = "value") %>%
  filter(is.finite(value), !is.na(Group), as.character(Group) %in% group_levels) %>%
  group_by(feature) %>%
  group_modify(~{
    pairs <- list(c("CON", "RES"), c("CON", "SUS"), c("RES", "SUS"))
    map_dfr(pairs, function(pair) {
      x <- .x$value[as.character(.x$Group) == pair[1]]
      y <- .x$value[as.character(.x$Group) == pair[2]]
      if (length(x) < 2 || length(y) < 2) {
        return(tibble(
          contrast = paste0(pair[2], "-", pair[1]),
          n_ref = length(x),
          n_comp = length(y),
          median_ref = median(x, na.rm = TRUE),
          median_comp = median(y, na.rm = TRUE),
          median_difference = NA_real_,
          wilcox_p = NA_real_,
          effect_r = NA_real_,
          status = "skipped_low_n"
        ))
      }
      wt <- suppressWarnings(wilcox.test(y, x, exact = FALSE))
      tibble(
        contrast = paste0(pair[2], "-", pair[1]),
        n_ref = length(x),
        n_comp = length(y),
        median_ref = median(x, na.rm = TRUE),
        median_comp = median(y, na.rm = TRUE),
        median_difference = median(y, na.rm = TRUE) - median(x, na.rm = TRUE),
        wilcox_p = wt$p.value,
        effect_r = wilcox_effect_r(x, y),
        status = "tested"
      )
    })
  }) %>%
  ungroup() %>%
  group_by(feature) %>%
  mutate(
    wilcox_p_bh_within_feature = p.adjust(wilcox_p, method = "BH"),
    feature_label = recode(feature, !!!feature_display_labels),
    ReportingRole = "Descriptive group contrast; use as visualization/statistical context, not as independent prospective prediction"
  ) %>%
  ungroup()

write_table(primary_group_summary, file.path(output_dir, "tables", "primary_feature_group_summary.csv"))
write_table(primary_group_contrasts, file.path(output_dir, "tables", "primary_feature_group_contrasts_descriptive.csv"))
write_table(primary_group_summary, file.path(output_dir, "tables", "statistics", "primary_feature_group_summary.csv"))
write_table(primary_group_contrasts, file.path(output_dir, "tables", "statistics", "primary_feature_group_contrasts_descriptive.csv"))

# Sex-specific associations, because the biological effect appears female-specific.
sex_specific_assoc <- model_dat %>%
  group_by(Sex) %>%
  group_modify(~{
    map_dfr(primary_features, function(fc) {
      if (nrow(.x) < 4 || sd(.x[[fc]], na.rm = TRUE) == 0 || sd(.x$outcome, na.rm = TRUE) == 0) {
        return(tibble(feature = fc, n = nrow(.x), spearman_rho = NA_real_, spearman_p = NA_real_))
      }
      ct <- suppressWarnings(cor.test(.x[[fc]], .x$outcome, method = "spearman", exact = FALSE))
      tibble(feature = fc, n = nrow(.x), spearman_rho = unname(ct$estimate), spearman_p = ct$p.value)
    })
  }) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(spearman_p_bh_within_sex = p.adjust(spearman_p, method = "BH")) %>%
  ungroup() %>%
  mutate(BinLevel = bin_level, Outcome = outcome_col)

write_table(sex_specific_assoc, file.path(output_dir, "tables", "sex_specific_primary_associations.csv"))
write_table(sex_specific_assoc, file.path(output_dir, "tables", "statistics", "sex_specific_primary_associations.csv"))

primary_correlation_stats_by_sex <- model_dat %>%
  group_by(Sex) %>%
  group_modify(~{
    map_dfr(primary_features, function(fc) {
      correlation_test_stats(.x, fc, "outcome", label = fc)
    })
  }) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(
    pearson_p_bh_within_sex = p.adjust(pearson_p, method = "BH"),
    spearman_p_bh_within_sex = p.adjust(spearman_p, method = "BH"),
    feature = label,
    feature_label = recode(feature, !!!feature_display_labels),
    CorrelationUse = "Feature-to-outcome correlation plotted in primary faceted figure"
  ) %>%
  ungroup() %>%
  select(Sex, feature, feature_label, n, pearson_r, pearson_p, pearson_p_bh_within_sex, spearman_rho, spearman_p, spearman_p_bh_within_sex, CorrelationUse)

write_table(primary_correlation_stats_by_sex, file.path(output_dir, "tables", "primary_movement_entropyacf1_correlations_by_sex.csv"))
write_table(primary_correlation_stats_by_sex, file.path(output_dir, "tables", "statistics", "primary_movement_entropyacf1_correlations_by_sex.csv"))

# ------------------------------------------------
# MODEL LADDER: EXPLICIT INCREMENTAL PREDICTION
# ------------------------------------------------

candidate_covars <- c("Sex", "Group")
candidate_covars <- candidate_covars[candidate_covars %in% names(model_dat)]

model_specs <- list(
  "Mean only" = character(0),
  "Sex + Group" = candidate_covars,
  "Movement only" = c(candidate_covars, "Movement_mean"),
  "Entropy ACF1 only" = c(candidate_covars, "Entropy_acf1"),
  "Movement + Entropy ACF1" = c(candidate_covars, "Movement_mean", "Entropy_acf1"),
  "Movement x Entropy ACF1" = c(candidate_covars, "Movement_mean", "Entropy_acf1", "Movement_x_EntropyACF1"),
  "Full behavior compact" = c(candidate_covars, "Movement_mean", "Movement_rmssd", "Movement_acf1", "Entropy_mean", "Entropy_rmssd", "Entropy_acf1", "Proximity_mean", "Proximity_rmssd", "Proximity_acf1")
)

model_specs <- map(model_specs, sanitize_model_predictors, dat = model_dat, outcome = "outcome")

model_predictor_audit <- imap_dfr(model_specs, function(predictors, model_name) {
  known_endpoint_cols <- if (exists("endpoint_cols")) endpoint_cols else character(0)
  tibble(
    Model = model_name,
    DisplayModel = recode(model_name, !!!model_display_labels),
    Predictors = paste(predictors, collapse = " + "),
    n_predictors = length(predictors),
    ModelType = classify_model_reporting_use(model_name),
    UsesGroup = "Group" %in% predictors,
    UsesSex = "Sex" %in% predictors,
    UsesBehavior = any(str_detect(predictors, "Movement|Entropy|Proximity")),
    PrimaryInterpretation = case_when(
      model_name == "Mean only" ~ "Reference baseline only.",
      model_name == "Sex + Group" ~ "Descriptive endpoint-group/sex adjustment; not a prospective behavior model.",
      "Group" %in% predictors ~ "Adjusted/sensitivity model. Do not headline as behavior-only evidence because Group may reflect later phenotype.",
      TRUE ~ "Behavioral predictor set."
    ),
    contains_outcome_like_predictor = any(predictors %in% unique(c("outcome", outcome_col, known_endpoint_cols)))
  )
})
write_table(model_predictor_audit, file.path(output_dir, "tables", "model_ladder_predictor_audit.csv"))
write_table(model_predictor_audit, file.path(output_dir, "tables", "documentation", "model_specification_dictionary.csv"))
write_table(model_predictor_audit, file.path(output_dir, "tables", "models", "model_ladder_predictor_audit.csv"))

numeric_predictors <- unique(unlist(model_specs))
numeric_predictors <- numeric_predictors[numeric_predictors %in% names(model_dat) & sapply(model_dat[numeric_predictors], is.numeric)]
model_dat_imputed <- impute_numeric(model_dat, numeric_predictors)

ladder_results <- imap(model_specs, ~loo_lm_predict(model_dat_imputed, .x, .y))
ladder_predictions <- map_dfr(ladder_results, "predictions")
ladder_coefficients <- map_dfr(ladder_results, "coefficients")

ladder_performance <- ladder_predictions %>%
  group_by(Model) %>%
  group_modify(~prediction_metrics(.x$observed, .x$predicted)) %>%
  ungroup() %>%
  mutate(
    prediction_permutation_p = map_dbl(Model, ~{
      pdat <- ladder_predictions %>% filter(Model == .x)
      permutation_prediction_p(pdat$observed, pdat$predicted, n_prediction_permutations, seed = 123)
    }),
    BinLevel = bin_level,
    Outcome = outcome_col,
    ReportingUse = classify_model_reporting_use(Model),
    DisplayModel = recode(Model, !!!model_display_labels),
    UsesGroupOrSex = str_detect(Model, "Sex|Group") | Model %in% c(
      "Movement only",
      "Entropy ACF1 only",
      "Movement + Entropy ACF1",
      "Movement x Entropy ACF1",
      "Full behavior compact"
    )
  ) %>%
  arrange(desc(cv_r2_vs_mean), rmse)

run_ladder_duration_set <- function(dat, analysis_set) {
  if (nrow(dat) < 8 || sum(is.finite(dat$outcome)) < 8) {
    return(list(
      predictions = tibble(),
      coefficients = tibble(),
      performance = tibble(
        Model = names(model_specs),
        n = sum(is.finite(dat$outcome)),
        pearson_r = NA_real_,
        spearman_rho = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        cv_r2_vs_mean = NA_real_,
        prediction_permutation_p = NA_real_,
        BinLevel = bin_level,
        Outcome = outcome_col,
        DurationAnalysisSet = analysis_set,
        DurationSensitivityStatus = "skipped_too_few_animals"
      )
    ))
  }

  dat_imp <- impute_numeric(dat, numeric_predictors)
  fits <- imap(model_specs, ~loo_lm_predict(dat_imp, .x, .y))
  preds <- map_dfr(fits, "predictions") %>% mutate(DurationAnalysisSet = analysis_set)
  coefs <- map_dfr(fits, "coefficients") %>% mutate(DurationAnalysisSet = analysis_set)
  perf <- preds %>%
    group_by(Model) %>%
    group_modify(~prediction_metrics(.x$observed, .x$predicted)) %>%
    ungroup() %>%
    mutate(
      prediction_permutation_p = map_dbl(Model, ~{
        pdat <- preds %>% filter(Model == .x)
        permutation_prediction_p(pdat$observed, pdat$predicted, n_prediction_permutations, seed = 123)
      }),
      BinLevel = bin_level,
      Outcome = outcome_col,
      DurationAnalysisSet = analysis_set,
      DurationSensitivityStatus = "fit"
    ) %>%
    arrange(desc(cv_r2_vs_mean), rmse)

  list(predictions = preds, coefficients = coefs, performance = perf)
}

full_duration_ladder <- list(
  predictions = ladder_predictions %>% mutate(DurationAnalysisSet = "full"),
  coefficients = ladder_coefficients %>% mutate(DurationAnalysisSet = "full"),
  performance = ladder_performance %>%
    mutate(DurationAnalysisSet = "full", DurationSensitivityStatus = "fit")
)

no_short_model_dat <- model_dat %>%
  filter(!contains_short_duration_epoch %in% TRUE)
no_short_duration_ladder <- run_ladder_duration_set(no_short_model_dat, "excluding_short_duration")

ladder_predictions_duration_sensitivity <- bind_rows(
  full_duration_ladder$predictions,
  no_short_duration_ladder$predictions
)
ladder_coefficients_duration_sensitivity <- bind_rows(
  full_duration_ladder$coefficients,
  no_short_duration_ladder$coefficients
)
ladder_performance_duration_sensitivity <- bind_rows(
  full_duration_ladder$performance,
  no_short_duration_ladder$performance
) %>%
  group_by(Model) %>%
  mutate(
    full_cv_r2 = cv_r2_vs_mean[DurationAnalysisSet == "full"][1],
    full_pearson_r = pearson_r[DurationAnalysisSet == "full"][1],
    full_rmse = rmse[DurationAnalysisSet == "full"][1],
    delta_cv_r2_vs_full = cv_r2_vs_mean - full_cv_r2,
    delta_pearson_r_vs_full = pearson_r - full_pearson_r,
    delta_rmse_vs_full = rmse - full_rmse
  ) %>%
  ungroup() %>%
  select(-full_cv_r2, -full_pearson_r, -full_rmse)

baseline_rmse <- ladder_performance %>% filter(Model == "Mean only") %>% pull(rmse)
movement_rmse <- ladder_performance %>% filter(Model == "Movement only") %>% pull(rmse)
combined_rmse <- ladder_performance %>% filter(Model == "Movement + Entropy ACF1") %>% pull(rmse)

incremental_summary <- ladder_performance %>%
  mutate(
    delta_rmse_vs_mean_only = rmse - baseline_rmse,
    delta_rmse_vs_movement_only = rmse - movement_rmse,
    relative_rmse_vs_movement_only = rmse / movement_rmse,
    Interpretation = case_when(
      Model == "Movement + Entropy ACF1" & rmse < movement_rmse ~ "Entropy ACF1 improves prediction beyond movement",
      Model == "Movement + Entropy ACF1" & rmse >= movement_rmse ~ "No incremental prediction beyond movement",
      TRUE ~ NA_character_
    )
  )

write_table(ladder_predictions, file.path(output_dir, "tables", "model_ladder_loo_predictions.csv"))
write_table(ladder_coefficients, file.path(output_dir, "tables", "model_ladder_loo_coefficients.csv"))
write_table(ladder_performance, file.path(output_dir, "tables", "model_ladder_performance.csv"))
write_table(incremental_summary, file.path(output_dir, "tables", "model_ladder_incremental_summary.csv"))
write_table(ladder_predictions_duration_sensitivity, file.path(output_dir, "tables", "model_ladder_loo_predictions_duration_sensitivity.csv"))
write_table(ladder_coefficients_duration_sensitivity, file.path(output_dir, "tables", "model_ladder_loo_coefficients_duration_sensitivity.csv"))
write_table(ladder_performance_duration_sensitivity, file.path(output_dir, "tables", "model_ladder_performance_duration_sensitivity.csv"))
write_table(ladder_predictions, file.path(output_dir, "tables", "models", "model_ladder_loo_predictions.csv"))
write_table(ladder_coefficients, file.path(output_dir, "tables", "models", "model_ladder_loo_coefficients.csv"))
write_table(ladder_performance, file.path(output_dir, "tables", "models", "model_ladder_performance.csv"))
write_table(incremental_summary, file.path(output_dir, "tables", "models", "model_ladder_incremental_summary.csv"))
write_table(ladder_predictions_duration_sensitivity, file.path(output_dir, "tables", "sensitivity", "model_ladder_loo_predictions_duration_sensitivity.csv"))
write_table(ladder_coefficients_duration_sensitivity, file.path(output_dir, "tables", "sensitivity", "model_ladder_loo_coefficients_duration_sensitivity.csv"))
write_table(ladder_performance_duration_sensitivity, file.path(output_dir, "tables", "sensitivity", "model_ladder_performance_duration_sensitivity.csv"))

# ------------------------------------------------
# GROUPED K-FOLD COMPANION: BEHAVIOR-ONLY PRIMARY CLAIM
# ------------------------------------------------

make_grouped_folds <- function(dat, k = 5, repeats = 100, group_col = "AnimalNum", seed = 123) {
  set.seed(seed)
  ids <- unique(dat[[group_col]])
  k <- min(k, length(ids))
  map_dfr(seq_len(repeats), function(rep_i) {
    shuffled <- sample(ids)
    fold_id <- rep(seq_len(k), length.out = length(shuffled))
    tibble(Repeat = rep_i, Fold = fold_id, !!group_col := shuffled)
  })
}

kfold_lm_predict <- function(dat, predictors, model_name, fold_map) {
  predictors <- sanitize_model_predictors(predictors, dat, outcome = "outcome")
  pred_rows <- vector("list", nrow(fold_map))
  for (i in seq_len(nrow(fold_map))) {
    held_out_id <- fold_map$AnimalNum[i]
    repeat_i <- fold_map$Repeat[i]
    fold_i <- fold_map$Fold[i]
    train <- dat %>% filter(AnimalNum != held_out_id)
    test <- dat %>% filter(AnimalNum == held_out_id)
    fit <- try(lm(make_formula("outcome", predictors), data = train), silent = TRUE)
    pred <- if (inherits(fit, "try-error")) NA_real_ else tryCatch(as.numeric(predict(fit, newdata = test)), error = function(e) NA_real_)
    pred_rows[[i]] <- test %>%
      transmute(
        Repeat = repeat_i,
        Fold = fold_i,
        AnimalNum, Group, Sex,
        observed = outcome,
        predicted = pred,
        Model = model_name
      )
  }
  bind_rows(pred_rows)
}

summarise_repeated_cv <- function(pred_tbl, analysis_set) {
  if (nrow(pred_tbl) == 0) return(tibble())
  pred_tbl %>%
    group_by(Model, Repeat) %>%
    group_modify(~prediction_metrics(.x$observed, .x$predicted)) %>%
    ungroup() %>%
    group_by(Model) %>%
    summarise(
      n_repeats = n_distinct(Repeat),
      mean_pearson_r = mean(pearson_r, na.rm = TRUE),
      median_pearson_r = median(pearson_r, na.rm = TRUE),
      mean_spearman_rho = mean(spearman_rho, na.rm = TRUE),
      mean_rmse = mean(rmse, na.rm = TRUE),
      mean_mae = mean(mae, na.rm = TRUE),
      mean_cv_r2 = mean(cv_r2_vs_mean, na.rm = TRUE),
      cv_r2_ci_low = quantile(cv_r2_vs_mean, 0.025, na.rm = TRUE, names = FALSE),
      cv_r2_ci_high = quantile(cv_r2_vs_mean, 0.975, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    mutate(
      BinLevel = bin_level,
      Outcome = outcome_col,
      CVScheme = "repeated_grouped_kfold_leave_animals_intact",
      DurationAnalysisSet = analysis_set
    )
}

behavior_predictors <- c("Movement_mean", "Movement_rmssd", "Entropy_acf1")
compact_behavior_predictors <- c(
  "Movement_mean", "Movement_rmssd", "Movement_acf1",
  "Entropy_mean", "Entropy_rmssd", "Entropy_acf1",
  "Proximity_mean"
)
behavior_only_specs <- list(
  "Behavior-only: mean" = character(0),
  "Behavior-only: movement mean" = "Movement_mean",
  "Behavior-only: movement + entropy persistence" = c("Movement_mean", "Entropy_acf1"),
  "Behavior-only: primary family" = behavior_predictors,
  "Behavior-only: compact dynamics" = compact_behavior_predictors
) %>%
  map(sanitize_model_predictors, dat = model_dat, outcome = "outcome")

behavior_group_specs <- map(behavior_only_specs, ~unique(c(.x, candidate_covars))) %>%
  map(sanitize_model_predictors, dat = model_dat, outcome = "outcome") %>%
  set_names(str_replace(names(behavior_only_specs), "Behavior-only", "Behavior + group/sex"))

cv_specs <- c(behavior_only_specs, behavior_group_specs)
cv_specs <- map(cv_specs, sanitize_model_predictors, dat = model_dat, outcome = "outcome")

behavior_cv_model_dictionary <- imap_dfr(cv_specs, function(predictors, model_name) {
  tibble(
    Model = model_name,
    Predictors = paste(predictors, collapse = " + "),
    n_predictors = length(predictors),
    ModelFamily = if_else(str_detect(model_name, "^Behavior-only"), "Behavior only", "Behavior + sex/group"),
    CVScheme = "Repeated grouped 5-fold CV; animal is the held-out unit",
    ManuscriptUse = if_else(
      str_detect(model_name, "^Behavior-only"),
      "Primary prospective behavior-only evidence",
      "Sensitivity/descriptive adjustment"
    ),
    InterpretationGuardrail = if_else(
      str_detect(model_name, "^Behavior-only"),
      "May support early behavior predicting later stress burden.",
      "Do not use as the central prospective claim if Group reflects later stress phenotype."
    )
  )
})
write_table(behavior_cv_model_dictionary, file.path(output_dir, "tables", "documentation", "behavior_cv_model_dictionary.csv"))

cv_predictors <- unique(unlist(cv_specs))
cv_predictors <- cv_predictors[cv_predictors %in% names(model_dat) & sapply(model_dat[cv_predictors], is.numeric)]
cv_model_dat <- impute_numeric(model_dat, cv_predictors)
fold_map <- make_grouped_folds(cv_model_dat, k = 5, repeats = 100, seed = 321)
repeated_cv_predictions <- imap_dfr(cv_specs, ~kfold_lm_predict(cv_model_dat, .x, .y, fold_map))
repeated_cv_performance <- summarise_repeated_cv(repeated_cv_predictions, "full")

repeated_cv_no_short_predictions <- tibble()
repeated_cv_no_short_performance <- tibble()
if (nrow(no_short_model_dat) >= 8) {
  no_short_cv_dat <- impute_numeric(no_short_model_dat, cv_predictors)
  no_short_fold_map <- make_grouped_folds(no_short_cv_dat, k = 5, repeats = 100, seed = 322)
  repeated_cv_no_short_predictions <- imap_dfr(cv_specs, ~kfold_lm_predict(no_short_cv_dat, .x, .y, no_short_fold_map))
  repeated_cv_no_short_performance <- summarise_repeated_cv(repeated_cv_no_short_predictions, "excluding_short_duration")
}

repeated_cv_performance_all <- bind_rows(repeated_cv_performance, repeated_cv_no_short_performance) %>%
  group_by(Model) %>%
  mutate(
    full_mean_cv_r2 = mean_cv_r2[DurationAnalysisSet == "full"][1],
    delta_mean_cv_r2_vs_full = mean_cv_r2 - full_mean_cv_r2
  ) %>%
  ungroup() %>%
  select(-full_mean_cv_r2) %>%
  mutate(
    ReportingUse = if_else(str_detect(Model, "^Behavior-only"), "Primary prospective behavior-only evidence", "Sensitivity: descriptive sex/group adjustment"),
    ModelFamily = if_else(str_detect(Model, "^Behavior-only"), "Behavior only", "Behavior + sex/group")
  )

prediction_interpretation_constraints <- tibble(
  Constraint = c(
    "Primary evidence",
    "Group-label circularity",
    "Matched behavior ladders",
    "Sex-adjusted matched ladder",
    "Sex + group matched ladder",
    "Behavior + group models",
    "Cross-validation unit",
    "Permutation testing",
    "Duration robustness",
    "Allowed language"
  ),
  Interpretation = c(
    "Use behavior-only models to support early behavior predicting later stress burden.",
    "RES/SUS group labels may be derived from CombZ, so group terms should not be treated as independent prospective predictors.",
    "Use the matched behavior-only ladder as the clearest prospective behavior result because it excludes Sex and Group covariates.",
    "Use the matched Behavior + Sex ladder as a secondary/main-text sensitivity analysis.",
    "Use the matched Behavior + Sex + Group ladder only as contextual/supplementary adjustment because Group derives from later endpoint structure.",
    "Use behavior + group/sex models as descriptive adjustment/sensitivity analyses, not as the central claim.",
    "Grouped folds keep all observations from an animal together; the animal is the biological unit.",
    "Permutation p-values test full-pipeline prediction strength for the final observed predictions.",
    "Main-text claims require consistent full-data and excluding-short-duration performance.",
    "Use predictive/associative wording; avoid causal or biomarker language without external validation."
  ),
  ManuscriptUse = c(
    "Main Results",
    "Methods/Limitations",
    "Main Results",
    "Main Results sensitivity",
    "Supplementary/context",
    "Supplementary",
    "Methods",
    "Methods/Statistics",
    "Reviewer robustness",
    "Discussion"
  )
)

write_table(repeated_cv_predictions, file.path(output_dir, "tables", "model_ladder_repeated_grouped_kfold_predictions.csv"))
write_table(repeated_cv_performance_all, file.path(output_dir, "tables", "model_ladder_repeated_grouped_kfold_performance.csv"))
write_table(prediction_interpretation_constraints, file.path(output_dir, "tables", "prediction_interpretation_constraints.csv"))
write_table(repeated_cv_predictions, file.path(output_dir, "tables", "models", "model_ladder_repeated_grouped_kfold_predictions.csv"))
write_table(repeated_cv_performance_all, file.path(output_dir, "tables", "models", "model_ladder_repeated_grouped_kfold_performance.csv"))
write_table(prediction_interpretation_constraints, file.path(output_dir, "tables", "documentation", "prediction_interpretation_constraints.csv"))

# ------------------------------------------------
# MATCHED LADDERS: BEHAVIOR ONLY VS COVARIATE SENSITIVITY
# ------------------------------------------------

matched_ladder_behavior_sets <- list(
  "Mean only" = character(0),
  "Movement mean" = "Movement_mean",
  "Movement + entropy persistence" = c("Movement_mean", "Entropy_acf1"),
  "Primary behavior family" = c("Movement_mean", "Entropy_acf1", "Movement_x_EntropyACF1"),
  "Compact behavior dynamics" = c(
    "Movement_mean", "Movement_rmssd", "Movement_acf1",
    "Entropy_mean", "Entropy_rmssd", "Entropy_acf1",
    "Proximity_mean", "Proximity_rmssd", "Proximity_acf1"
  )
)

matched_ladder_adjustments <- list(
  "Behavior only" = character(0),
  "Behavior + Sex" = intersect("Sex", names(model_dat)),
  "Behavior + Sex + Group" = intersect(c("Sex", "Group"), names(model_dat))
)

matched_ladder_specs <- imap(matched_ladder_adjustments, function(covars, adjustment) {
  imap(matched_ladder_behavior_sets, function(behavior_predictors, model_family) {
    sanitize_model_predictors(unique(c(covars, behavior_predictors)), model_dat, outcome = "outcome")
  })
})

matched_ladder_predictor_audit <- imap_dfr(matched_ladder_specs, function(specs, adjustment) {
  imap_dfr(specs, function(predictors, model_family) {
    behavior_predictors <- setdiff(predictors, c("Sex", "Group"))
    tibble(
      AdjustmentSet = adjustment,
      ModelFamily = model_family,
      DisplayModel = recode(model_family, !!!matched_ladder_model_labels),
      Predictors = paste(predictors, collapse = " + "),
      BehaviorPredictors = paste(behavior_predictors, collapse = " + "),
      Covariates = paste(intersect(predictors, c("Sex", "Group")), collapse = " + "),
      n_behavior_predictors = length(behavior_predictors),
      n_covariates = length(intersect(predictors, c("Sex", "Group"))),
      UsesSex = "Sex" %in% predictors,
      UsesGroup = "Group" %in% predictors,
      ManuscriptUse = unname(matched_ladder_use_labels[adjustment]),
      InterpretationGuardrail = case_when(
        adjustment == "Behavior only" ~ "Primary prospective behavior-only evidence; no sex or endpoint-derived group covariates.",
        adjustment == "Behavior + Sex" ~ "Secondary/main-text sensitivity asking whether behavior predicts beyond sex.",
        adjustment == "Behavior + Sex + Group" ~ "Contextual/supplementary only because RES/SUS group labels derive from later endpoint structure related to CombZ.",
        TRUE ~ NA_character_
      )
    )
  })
})

write_table(matched_ladder_predictor_audit, file.path(output_dir, "tables", "matched_ladder_predictor_audit.csv"))
write_table(matched_ladder_predictor_audit, file.path(output_dir, "tables", "models", "matched_ladder_predictor_audit.csv"))
write_table(matched_ladder_predictor_audit, file.path(output_dir, "tables", "documentation", "matched_ladder_predictor_audit.csv"))

matched_ladder_predictors <- unique(unlist(matched_ladder_specs))
matched_ladder_numeric_predictors <- matched_ladder_predictors[
  matched_ladder_predictors %in% names(model_dat) & sapply(model_dat[matched_ladder_predictors], is.numeric)
]
matched_ladder_model_dat <- impute_numeric(model_dat, matched_ladder_numeric_predictors)

matched_ladder_results <- imap(matched_ladder_specs, function(specs, adjustment) {
  imap(specs, function(predictors, model_family) {
    res <- loo_lm_predict(matched_ladder_model_dat, predictors, model_family)
    list(
      predictions = res$predictions %>%
        mutate(
          AdjustmentSet = adjustment,
          ModelFamily = model_family,
          DisplayModel = recode(model_family, !!!matched_ladder_model_labels),
          ManuscriptUse = unname(matched_ladder_use_labels[adjustment])
        ),
      coefficients = res$coefficients %>%
        mutate(
          AdjustmentSet = adjustment,
          ModelFamily = model_family,
          ManuscriptUse = unname(matched_ladder_use_labels[adjustment])
        )
    )
  })
})

matched_ladder_predictions <- matched_ladder_results %>%
  unlist(recursive = FALSE) %>%
  map_dfr("predictions")
matched_ladder_coefficients <- matched_ladder_results %>%
  unlist(recursive = FALSE) %>%
  map_dfr("coefficients")

matched_ladder_performance <- matched_ladder_predictions %>%
  group_by(AdjustmentSet, ModelFamily) %>%
  group_modify(~prediction_metrics(.x$observed, .x$predicted)) %>%
  ungroup() %>%
  mutate(
    prediction_permutation_p = map2_dbl(AdjustmentSet, ModelFamily, ~{
      pdat <- matched_ladder_predictions %>% filter(AdjustmentSet == .x, ModelFamily == .y)
      permutation_prediction_p(pdat$observed, pdat$predicted, n_prediction_permutations, seed = 123)
    }),
    BinLevel = bin_level,
    Outcome = outcome_col,
    DisplayModel = recode(ModelFamily, !!!matched_ladder_model_labels),
    AdjustmentSet = factor(AdjustmentSet, levels = names(matched_ladder_adjustment_labels)),
    ManuscriptUse = unname(matched_ladder_use_labels[as.character(AdjustmentSet)]),
    ReportingPriority = case_when(
      as.character(AdjustmentSet) == "Behavior only" ~ "primary",
      as.character(AdjustmentSet) == "Behavior + Sex" ~ "secondary_main_text_sensitivity",
      as.character(AdjustmentSet) == "Behavior + Sex + Group" ~ "supplementary_contextual",
      TRUE ~ "context"
    ),
    InterpretationGuardrail = case_when(
      as.character(AdjustmentSet) == "Behavior only" ~ "Primary prospective behavior-only model.",
      as.character(AdjustmentSet) == "Behavior + Sex" ~ "Sensitivity model adjusted for sex.",
      as.character(AdjustmentSet) == "Behavior + Sex + Group" ~ "Do not frame as primary prospective evidence because Group is endpoint-derived.",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(AdjustmentSet, desc(cv_r2_vs_mean), rmse)

write_table(matched_ladder_predictions, file.path(output_dir, "tables", "matched_ladder_loo_predictions.csv"))
write_table(matched_ladder_coefficients, file.path(output_dir, "tables", "matched_ladder_loo_coefficients.csv"))
write_table(matched_ladder_performance, file.path(output_dir, "tables", "matched_ladder_performance.csv"))
write_table(matched_ladder_predictions, file.path(output_dir, "tables", "models", "matched_ladder_loo_predictions.csv"))
write_table(matched_ladder_coefficients, file.path(output_dir, "tables", "models", "matched_ladder_loo_coefficients.csv"))
write_table(matched_ladder_performance, file.path(output_dir, "tables", "models", "matched_ladder_performance.csv"))

matched_cv_specs <- list()
for (adjustment in names(matched_ladder_specs)) {
  for (model_family in names(matched_ladder_specs[[adjustment]])) {
    matched_cv_specs[[paste(adjustment, model_family, sep = ": ")]] <- matched_ladder_specs[[adjustment]][[model_family]]
  }
}
matched_cv_model_dat <- impute_numeric(model_dat, matched_ladder_numeric_predictors)
matched_cv_fold_map <- make_grouped_folds(matched_cv_model_dat, k = 5, repeats = 100, seed = 421)
matched_cv_predictions <- imap_dfr(matched_cv_specs, ~kfold_lm_predict(matched_cv_model_dat, .x, .y, matched_cv_fold_map)) %>%
  separate_wider_delim(Model, delim = ": ", names = c("AdjustmentSet", "ModelFamily"), too_few = "align_start") %>%
  mutate(
    DisplayModel = recode(ModelFamily, !!!matched_ladder_model_labels),
    ManuscriptUse = unname(matched_ladder_use_labels[AdjustmentSet])
  )
matched_cv_performance <- matched_cv_predictions %>%
  unite("Model", AdjustmentSet, ModelFamily, sep = ": ", remove = FALSE) %>%
  summarise_repeated_cv("full") %>%
  separate_wider_delim(Model, delim = ": ", names = c("AdjustmentSet", "ModelFamily"), too_few = "align_start") %>%
  mutate(
    DisplayModel = recode(ModelFamily, !!!matched_ladder_model_labels),
    ManuscriptUse = unname(matched_ladder_use_labels[AdjustmentSet]),
    ReportingPriority = case_when(
      AdjustmentSet == "Behavior only" ~ "primary",
      AdjustmentSet == "Behavior + Sex" ~ "secondary_main_text_sensitivity",
      AdjustmentSet == "Behavior + Sex + Group" ~ "supplementary_contextual",
      TRUE ~ "context"
    )
  )

write_table(matched_cv_predictions, file.path(output_dir, "tables", "matched_ladder_repeated_grouped_kfold_predictions.csv"))
write_table(matched_cv_performance, file.path(output_dir, "tables", "matched_ladder_repeated_grouped_kfold_performance.csv"))
write_table(matched_cv_predictions, file.path(output_dir, "tables", "models", "matched_ladder_repeated_grouped_kfold_predictions.csv"))
write_table(matched_cv_performance, file.path(output_dir, "tables", "models", "matched_ladder_repeated_grouped_kfold_performance.csv"))

# ------------------------------------------------
# PREDICTION CORRELATION STATS FOR PLOTTED LADDERS
# ------------------------------------------------

ladder_prediction_correlation_stats <- ladder_predictions %>%
  group_by(Model) %>%
  group_modify(~correlation_test_stats(.x, "observed", "predicted", label = .y$Model)) %>%
  ungroup() %>%
  mutate(
    pearson_p_bh = p.adjust(pearson_p, method = "BH"),
    spearman_p_bh = p.adjust(spearman_p, method = "BH"),
    DisplayModel = recode(Model, !!!model_display_labels),
    CorrelationUse = "Observed later outcome versus LOO predicted outcome for each adjusted ladder model"
  ) %>%
  left_join(
    ladder_performance %>% select(Model, rmse, mae, cv_r2_vs_mean, prediction_permutation_p, ReportingUse),
    by = "Model"
  ) %>%
  select(Model, DisplayModel, n, pearson_r, pearson_p, pearson_p_bh, spearman_rho, spearman_p, spearman_p_bh, rmse, mae, cv_r2_vs_mean, prediction_permutation_p, ReportingUse, CorrelationUse)

matched_ladder_prediction_correlation_stats <- matched_ladder_predictions %>%
  group_by(AdjustmentSet, ModelFamily) %>%
  group_modify(~correlation_test_stats(.x, "observed", "predicted", label = .y$ModelFamily)) %>%
  ungroup() %>%
  group_by(AdjustmentSet) %>%
  mutate(
    pearson_p_bh_within_adjustment = p.adjust(pearson_p, method = "BH"),
    spearman_p_bh_within_adjustment = p.adjust(spearman_p, method = "BH")
  ) %>%
  ungroup() %>%
  mutate(
    DisplayModel = recode(ModelFamily, !!!matched_ladder_model_labels),
    ManuscriptUse = unname(matched_ladder_use_labels[as.character(AdjustmentSet)]),
    CorrelationUse = "Observed later outcome versus LOO predicted outcome for each matched ladder model"
  ) %>%
  left_join(
    matched_ladder_performance %>%
      mutate(AdjustmentSet = as.character(AdjustmentSet)) %>%
      select(AdjustmentSet, ModelFamily, rmse, mae, cv_r2_vs_mean, prediction_permutation_p, ReportingPriority),
    by = c("AdjustmentSet", "ModelFamily")
  ) %>%
  select(AdjustmentSet, ModelFamily, DisplayModel, n, pearson_r, pearson_p, pearson_p_bh_within_adjustment, spearman_rho, spearman_p, spearman_p_bh_within_adjustment, rmse, mae, cv_r2_vs_mean, prediction_permutation_p, ReportingPriority, ManuscriptUse, CorrelationUse)

write_table(ladder_prediction_correlation_stats, file.path(output_dir, "tables", "model_ladder_prediction_correlations.csv"))
write_table(ladder_prediction_correlation_stats, file.path(output_dir, "tables", "models", "model_ladder_prediction_correlations.csv"))
write_table(matched_ladder_prediction_correlation_stats, file.path(output_dir, "tables", "matched_ladder_prediction_correlations.csv"))
write_table(matched_ladder_prediction_correlation_stats, file.path(output_dir, "tables", "models", "matched_ladder_prediction_correlations.csv"))

# ------------------------------------------------
# FIGURES
# ------------------------------------------------

primary_plot_stats <- primary_correlation_stats_by_sex %>%
  mutate(
    Feature = factor(feature_label, levels = unname(feature_display_labels[primary_features])),
    stat_label = paste0("rho=", round(spearman_rho, 2), "\n", format_p(spearman_p))
  )

p_primary <- model_dat %>%
  select(AnimalNum, Group, Sex, outcome, all_of(primary_features)) %>%
  pivot_longer(cols = all_of(primary_features), names_to = "Feature", values_to = "Value") %>%
  mutate(
    Feature = factor(recode(Feature, !!!feature_display_labels), levels = unname(feature_display_labels[primary_features])),
    Group = factor(as.character(Group), levels = group_levels)
  ) %>%
  ggplot(aes(Value, outcome)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.38, alpha = 0.10, colour = "grey25", fill = "grey70") +
  geom_point(aes(colour = Group, fill = Group, shape = Group), size = 1.65, stroke = 0.25, alpha = 0.9) +
  geom_text(
    data = primary_plot_stats,
    aes(x = -Inf, y = Inf, label = stat_label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.15,
    size = 1.65,
    lineheight = 0.9,
    colour = "grey20"
  ) +
  facet_grid(Sex ~ Feature, scales = "free_x") +
  labs(
    title = "Early behavioral organization aligns with later stress burden",
    subtitle = paste0("Each panel reports Spearman rho and nominal p; CON/RES/SUS are shown for interpretation, not as required predictors"),
    x = "Early feature value",
    y = outcome_col
  ) +
  scale_colour_manual(values = group_colors, drop = FALSE) +
  scale_fill_manual(values = group_colors, drop = FALSE) +
  scale_shape_manual(values = group_shape_values, drop = FALSE) +
  make_publication_theme(base_size = 6) +
  theme(legend.box.spacing = unit(0.5, "mm"))

save_plot_svg_pdf(p_primary, file.path(output_dir, "figures", "publication", "primary_movement_entropyacf1_vs_combz"), width = 183, height = 102)

p_ladder <- ladder_performance %>%
  mutate(
    DisplayModel = factor(DisplayModel, levels = rev(DisplayModel[order(cv_r2_vs_mean)])),
    ReportingUse = factor(ReportingUse, levels = c("Reference baseline", "Behavior plus sex/group adjustment", "Descriptive group/sex adjustment", "Sensitivity"))
  ) %>%
  ggplot(aes(cv_r2_vs_mean, DisplayModel, fill = ReportingUse)) +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dashed", colour = "grey55") +
  geom_col(width = 0.62, colour = "grey20", linewidth = 0.18) +
  labs(
    title = "Adjusted prediction ladder",
    subtitle = "Leave-one-animal-out linear models; group-adjusted models are sensitivity/context analyses",
    x = "Cross-validated R2 vs mean-only baseline",
    y = NULL
  ) +
  scale_fill_manual(values = c(
    "Reference baseline" = "grey82",
    "Behavior plus sex/group adjustment" = "#7A8F6A",
    "Descriptive group/sex adjustment" = "#B8B1A5",
    "Sensitivity" = "grey65"
  ), drop = FALSE) +
  make_publication_theme(base_size = 7) +
  theme(panel.grid.major.y = element_blank())

save_plot_svg_pdf(p_ladder, file.path(output_dir, "figures", "publication", "model_ladder_cv_r2"), width = 89, height = 82)

make_matched_ladder_plot <- function(perf_tbl, adjustment_filter = NULL, title = "Prediction ladder", subtitle = NULL, facet = FALSE) {
  plot_tbl <- perf_tbl %>%
    filter(if (is.null(adjustment_filter)) TRUE else as.character(AdjustmentSet) == adjustment_filter) %>%
    mutate(
      DisplayModel = factor(DisplayModel, levels = rev(unname(matched_ladder_model_labels))),
      ManuscriptUse = factor(ManuscriptUse, levels = unname(matched_ladder_use_labels)),
      AdjustmentSet = factor(as.character(AdjustmentSet), levels = names(matched_ladder_adjustment_labels))
    )

  p <- plot_tbl %>%
    ggplot(aes(cv_r2_vs_mean, DisplayModel, fill = ManuscriptUse)) +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dashed", colour = "grey55") +
    geom_col(width = 0.62, colour = "grey20", linewidth = 0.18) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Cross-validated R2 vs mean-only baseline",
      y = NULL
    ) +
    scale_fill_manual(values = matched_ladder_fill_values, drop = FALSE) +
    make_publication_theme(base_size = 7) +
    theme(panel.grid.major.y = element_blank())

  if (isTRUE(facet)) {
    p <- p + facet_grid(. ~ AdjustmentSet)
  }
  p
}

p_matched_behavior_only <- make_matched_ladder_plot(
  matched_ladder_performance,
  adjustment_filter = "Behavior only",
  title = "Behavior-only prediction ladder",
  subtitle = "Primary prospective model: behavior predictors only"
)
save_plot_svg_pdf(p_matched_behavior_only, file.path(output_dir, "figures", "publication", "matched_ladder_behavior_only_cv_r2"), width = 89, height = 82)

p_matched_behavior_sex <- make_matched_ladder_plot(
  matched_ladder_performance,
  adjustment_filter = "Behavior + Sex",
  title = "Sex-adjusted prediction ladder",
  subtitle = "Sensitivity model: Sex + identical behavior predictors"
)
save_plot_svg_pdf(p_matched_behavior_sex, file.path(output_dir, "figures", "publication", "matched_ladder_behavior_plus_sex_cv_r2"), width = 89, height = 82)

p_matched_behavior_sex_group <- make_matched_ladder_plot(
  matched_ladder_performance,
  adjustment_filter = "Behavior + Sex + Group",
  title = "Sex + group-adjusted prediction ladder",
  subtitle = "Contextual only: Group is endpoint-derived and not primary prospective evidence"
)
save_plot_svg_pdf(p_matched_behavior_sex_group, file.path(output_dir, "figures", "publication", "matched_ladder_behavior_plus_sex_group_cv_r2"), width = 89, height = 82)

p_matched_ladder_combined <- make_matched_ladder_plot(
  matched_ladder_performance,
  title = "Matched prediction ladders by covariate adjustment",
  subtitle = "Identical behavior feature families; only Sex and endpoint-derived Group covariates differ",
  facet = TRUE
) +
  theme(legend.position = "top")
save_plot_svg_pdf(p_matched_ladder_combined, file.path(output_dir, "figures", "publication", "matched_ladder_covariate_comparison_cv_r2"), width = 183, height = 82)

p_behavior_cv <- repeated_cv_performance_all %>%
  filter(DurationAnalysisSet == "full") %>%
  mutate(
    Model = str_replace(Model, "^Behavior-only: ", ""),
    Model = str_replace(Model, "^Behavior \\+ group/sex: ", ""),
    Model = str_to_sentence(Model),
    Model = factor(Model, levels = rev(unique(Model[order(mean_cv_r2)]))),
    ModelFamily = factor(ModelFamily, levels = c("Behavior only", "Behavior + sex/group"))
  ) %>%
  ggplot(aes(mean_cv_r2, Model, colour = ModelFamily)) +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = cv_r2_ci_low, xmax = cv_r2_ci_high), orientation = "y", width = 0, linewidth = 0.35, alpha = 0.75) +
  geom_point(size = 1.8) +
  facet_grid(. ~ ModelFamily, scales = "free_y", space = "free_y") +
  labs(
    title = "Behavior-only prediction is the primary prospective test",
    subtitle = "Repeated grouped 5-fold cross-validation; points show mean, bars show 95% repeat interval",
    x = "Mean cross-validated R2 vs mean-only baseline",
    y = NULL
  ) +
  scale_colour_manual(values = c("Behavior only" = "#2F4858", "Behavior + sex/group" = "#8A817C"), drop = FALSE) +
  make_publication_theme(base_size = 7) +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")

save_plot_svg_pdf(p_behavior_cv, file.path(output_dir, "figures", "publication", "behavior_only_repeated_cv_ladder"), width = 183, height = 82)

best_model <- ladder_performance %>% slice_max(cv_r2_vs_mean, n = 1, with_ties = FALSE) %>% pull(Model)
best_pred <- ladder_predictions %>% filter(Model == best_model)
best_perf <- ladder_performance %>% filter(Model == best_model)

p_pred <- best_pred %>%
  mutate(Group = factor(as.character(Group), levels = group_levels)) %>%
  ggplot(aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.25, linetype = "dashed", colour = "grey45") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.45, alpha = 0.10, colour = "grey20", fill = "grey70") +
  geom_point(aes(colour = Group, fill = Group, shape = Group), size = 1.8, stroke = 0.25, alpha = 0.88) +
  facet_grid(. ~ Sex) +
  labs(
    title = paste0("Observed vs predicted stress burden: ", recode(best_model, !!!model_display_labels)),
    subtitle = paste0("LOO r=", round(best_perf$pearson_r, 2), ", CV R2=", round(best_perf$cv_r2_vs_mean, 2), ", ", format_p(best_perf$prediction_permutation_p)),
    x = paste0("Observed ", outcome_col),
    y = paste0("Predicted ", outcome_col)
  ) +
  scale_colour_manual(values = group_colors, drop = FALSE) +
  scale_fill_manual(values = group_colors, drop = FALSE) +
  scale_shape_manual(values = group_shape_values, drop = FALSE) +
  make_publication_theme(base_size = 7)

save_plot_svg_pdf(p_pred, file.path(output_dir, "figures", "publication", "best_model_observed_vs_predicted"), width = 89, height = 78)

ladder_prediction_plot_tbl <- ladder_predictions %>%
  left_join(
    ladder_prediction_correlation_stats %>%
      mutate(
        prediction_stat_label = paste0(
          "r=", round(pearson_r, 2),
          ", rho=", round(spearman_rho, 2),
          "\nCV R2=", round(cv_r2_vs_mean, 2),
          ", ", format_p(prediction_permutation_p)
        )
      ) %>%
      select(Model, DisplayModel, prediction_stat_label),
    by = "Model"
  ) %>%
  mutate(
    DisplayModel = factor(DisplayModel, levels = model_display_labels[names(model_display_labels) %in% unique(Model)]),
    Group = factor(as.character(Group), levels = group_levels)
  )

p_ladder_prediction_correlations <- ladder_prediction_plot_tbl %>%
  ggplot(aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.22, linetype = "dashed", colour = "grey50") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.32, colour = "grey25") +
  geom_point(aes(colour = Group, fill = Group, shape = Group), size = 1.35, stroke = 0.22, alpha = 0.86) +
  geom_text(
    data = ladder_prediction_plot_tbl %>% distinct(DisplayModel, prediction_stat_label),
    aes(x = -Inf, y = Inf, label = prediction_stat_label),
    inherit.aes = FALSE,
    hjust = -0.04,
    vjust = 1.15,
    size = 1.55,
    lineheight = 0.9,
    colour = "grey20"
  ) +
  facet_wrap(~DisplayModel, scales = "free", ncol = 4) +
  labs(
    title = "Observed versus predicted outcome for every adjusted ladder model",
    subtitle = "Panel statistics are prediction correlations: Pearson r, Spearman rho, cross-validated R2, and permutation p",
    x = paste0("Observed ", outcome_col),
    y = paste0("LOO predicted ", outcome_col)
  ) +
  scale_colour_manual(values = group_colors, drop = FALSE) +
  scale_fill_manual(values = group_colors, drop = FALSE) +
  scale_shape_manual(values = group_shape_values, drop = FALSE) +
  make_publication_theme(base_size = 6) +
  theme(legend.position = "top")

save_plot_svg_pdf(p_ladder_prediction_correlations, file.path(output_dir, "figures", "publication", "model_ladder_prediction_correlations"), width = 183, height = 112)

matched_prediction_plot_tbl <- matched_ladder_predictions %>%
  left_join(
    matched_ladder_prediction_correlation_stats %>%
      mutate(
        AdjustmentSet = as.character(AdjustmentSet),
        prediction_stat_label = paste0(
          "r=", round(pearson_r, 2),
          ", rho=", round(spearman_rho, 2),
          "\nCV R2=", round(cv_r2_vs_mean, 2),
          ", ", format_p(prediction_permutation_p)
        )
      ) %>%
      select(AdjustmentSet, ModelFamily, prediction_stat_label),
    by = c("AdjustmentSet", "ModelFamily")
  ) %>%
  mutate(
    AdjustmentSet = factor(as.character(AdjustmentSet), levels = names(matched_ladder_adjustment_labels)),
    DisplayModel = factor(recode(ModelFamily, !!!matched_ladder_model_labels), levels = unname(matched_ladder_model_labels)),
    Group = factor(as.character(Group), levels = group_levels)
  )

p_matched_prediction_correlations <- matched_prediction_plot_tbl %>%
  ggplot(aes(observed, predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.22, linetype = "dashed", colour = "grey50") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.32, colour = "grey25") +
  geom_point(aes(colour = Group, fill = Group, shape = Group), size = 1.25, stroke = 0.20, alpha = 0.84) +
  geom_text(
    data = matched_prediction_plot_tbl %>% distinct(AdjustmentSet, DisplayModel, prediction_stat_label),
    aes(x = -Inf, y = Inf, label = prediction_stat_label),
    inherit.aes = FALSE,
    hjust = -0.04,
    vjust = 1.15,
    size = 1.45,
    lineheight = 0.9,
    colour = "grey20"
  ) +
  facet_grid(AdjustmentSet ~ DisplayModel, scales = "free") +
  labs(
    title = "Prediction correlations across matched ladders",
    subtitle = "Rows isolate covariate adjustment; columns keep identical behavior feature families",
    x = paste0("Observed ", outcome_col),
    y = paste0("LOO predicted ", outcome_col)
  ) +
  scale_colour_manual(values = group_colors, drop = FALSE) +
  scale_fill_manual(values = group_colors, drop = FALSE) +
  scale_shape_manual(values = group_shape_values, drop = FALSE) +
  make_publication_theme(base_size = 5.5) +
  theme(legend.position = "top")

save_plot_svg_pdf(p_matched_prediction_correlations, file.path(output_dir, "figures", "publication", "matched_ladder_prediction_correlations"), width = 183, height = 132)

# ------------------------------------------------
# TEXT SUMMARY FOR RESULTS WRITING
# ------------------------------------------------

combined_row <- incremental_summary %>% filter(Model == "Movement + Entropy ACF1")
movement_row <- incremental_summary %>% filter(Model == "Movement only")
entropy_row <- primary_assoc %>% filter(feature == "Entropy_acf1")
movement_assoc_row <- primary_assoc %>% filter(feature == "Movement_mean")

results_summary <- tibble(
  Result = c(
    "Primary prospective window",
    "Movement association",
    "Entropy ACF1 association",
    "Incremental model comparison"
  ),
  Text = c(
    paste0("Features were extracted from the first ", early_window_hours, " h of the active phase after the first cage change using ", bin_level, " bins."),
    if (nrow(movement_assoc_row) > 0) paste0("Early movement correlated with later ", outcome_col, " (Spearman rho=", round(movement_assoc_row$spearman_rho, 3), ", BH ", format_p(movement_assoc_row$spearman_p_bh), ").") else "Movement association unavailable.",
    if (nrow(entropy_row) > 0) paste0("Early entropy ACF1 correlated with later ", outcome_col, " (Spearman rho=", round(entropy_row$spearman_rho, 3), ", BH ", format_p(entropy_row$spearman_p_bh), "; bootstrap CI ", round(entropy_row$spearman_boot_ci_low, 3), " to ", round(entropy_row$spearman_boot_ci_high, 3), ").") else "Entropy ACF1 association unavailable.",
    if (nrow(combined_row) > 0 && nrow(movement_row) > 0) paste0("Adding entropy ACF1 to movement changed LOO RMSE from ", round(movement_row$rmse, 3), " to ", round(combined_row$rmse, 3), " and CV R2 from ", round(movement_row$cv_r2_vs_mean, 3), " to ", round(combined_row$cv_r2_vs_mean, 3), ".") else "Incremental comparison unavailable."
  )
)

write_table(results_summary, file.path(output_dir, "tables", "results_summary_text.csv"))
write_table(results_summary, file.path(output_dir, "tables", "documentation", "results_summary_text.csv"))

output_table_catalog <- tibble(
  file = c(
    "tables/documentation/analysis_readme.txt",
    "tables/documentation/readout_dictionary.csv",
    "tables/documentation/model_specification_dictionary.csv",
    "tables/documentation/behavior_cv_model_dictionary.csv",
    "tables/design/early_window_design_by_animal.csv",
    "tables/features/early_behavior_features_wide.csv",
    "tables/statistics/primary_movement_entropyacf1_associations.csv",
    "tables/statistics/primary_movement_entropyacf1_correlations_by_sex.csv",
    "tables/statistics/primary_feature_group_summary.csv",
    "tables/models/model_ladder_performance.csv",
    "tables/models/model_ladder_prediction_correlations.csv",
    "tables/models/model_ladder_repeated_grouped_kfold_performance.csv",
    "tables/models/matched_ladder_performance.csv",
    "tables/models/matched_ladder_prediction_correlations.csv",
    "tables/models/matched_ladder_repeated_grouped_kfold_performance.csv",
    "tables/documentation/matched_ladder_predictor_audit.csv",
    "tables/sensitivity/model_ladder_performance_duration_sensitivity.csv",
    "tables/documentation/results_summary_text.csv"
  ),
  category = c(
    "documentation", "documentation", "documentation", "documentation",
    "design", "features", "statistics", "statistics", "statistics", "models",
    "models", "models", "models", "models", "models", "documentation", "sensitivity", "documentation"
  ),
  contains = c(
    "Plain-text guide to the analysis folder and recommended reading order.",
    "Definitions and manuscript roles for generated readouts.",
    "LOO model ladder predictors, model type, and interpretation guardrails.",
    "Repeated grouped CV model definitions and manuscript-use labels.",
    "Animal-level early-window bin counts, timing, phase, and duration.",
    "Animal-level early-window feature matrix used for prediction.",
    "Primary feature-to-outcome correlations with bootstrap CIs and FDR correction.",
    "Sex-stratified feature-to-outcome correlations used for the primary faceted figure annotations.",
    "Descriptive CON/RES/SUS distribution of primary early features.",
    "Leave-one-animal-out model performance table.",
    "Observed-versus-predicted correlation statistics for every adjusted LOO ladder model.",
    "Repeated grouped CV performance; primary prospective behavior-only evidence.",
    "Matched LOO behavior-only, Sex-adjusted, and Sex + Group-adjusted ladder performance.",
    "Observed-versus-predicted correlation statistics for every matched ladder model.",
    "Matched repeated grouped CV companion performance for the three covariate-adjustment ladders.",
    "Predictor audit for matched ladders, including covariates and manuscript-use labels.",
    "Duration robustness table comparing full data with short-duration exclusions.",
    "Short manuscript-ready text snippets generated from the current run."
  ),
  manuscript_use = c(
    "Start here",
    "Methods/readout definitions",
    "Methods/model specification",
    "Methods/model specification",
    "Methods/QC",
    "Methods/source data",
    "Main or supplement",
    "Main figure statistics",
    "Descriptive group context",
    "Supplement/model comparison",
    "Prediction correlation figure statistics",
    "Main model-performance result",
    "Main/sensitivity/supplement split",
    "Prediction correlation figure statistics",
    "Robustness/supplement",
    "Methods/model specification",
    "Robustness/supplement",
    "Drafting aid"
  )
)
write_table(output_table_catalog, file.path(output_dir, "tables", "documentation", "output_table_catalog.csv"))
write_table(output_table_catalog, file.path(output_dir, "tables", "output_table_catalog.csv"))

if (exists("harmonize_analysis_outputs")) harmonize_analysis_outputs(output_dir)

message("Early prediction model ladder complete. Tables and publication figures written to: ", output_dir)
