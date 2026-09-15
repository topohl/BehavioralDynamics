# Single source of truth for the Figure 1 prediction contract.
#
# Every parameter the frozen manuscript bridge reports about the canonical
# prediction is defined here and read from here by BOTH the analysis that
# produces the numbers (Analysis/09_early_prediction_model_ladder.R) and the
# builders that describe them (Analysis/_supporting/build_figure1_*.R).
#
# The defect this closes: the repeated grouped-CV seed used to be a bare literal
# at its call site. It was the one prediction parameter with no name to
# reference, so the bundle builders transcribed the make_grouped_folds()
# function DEFAULT (123) rather than the value actually passed (521), and the
# frozen contract shipped a seed that reproduces nothing. Two seeds legitimately
# coexist in this analysis and they are easy to confuse:
#
#   repeated_cv_seed              521   fold assignment for the repeated
#                                       grouped five-fold companion
#   association_bootstrap_seed    123   resampling for the Spearman bootstrap
#                                       confidence intervals
#
# Both are correct. Neither may be substituted for the other.

FIGURE1_PREDICTION_CONTRACT <- list(
  # repeated grouped k-fold companion
  repeated_cv_k = 5L,
  repeated_cv_repeats = 100L,
  repeated_cv_group_col = "AnimalNum",
  repeated_cv_seed = 521L,
  repeated_cv_interval = "2.5-97.5% quantiles across repeated CV splits",

  # leave-one-animal-out primary estimate
  loo_scheme = "leave-one-animal-out",
  loo_detail = "complete refit per held-out animal",

  # full-refit outcome permutation
  outcome_permutation_draws = 1000L,
  outcome_permutation_seed = 20260811L,

  # association bootstrap - a DIFFERENT seed, legitimately 123
  association_bootstrap_resamples = 5000L,
  association_bootstrap_seed = 123L,
  association_method = "spearman",

  # the headline prospective model: behaviour only, no outcome-derived term
  headline_model_id = "movement_mean",
  headline_model_label = "Movement mean",
  headline_model_predictors = "Movement_mean"
)

# The sentence the manuscript bridge and export bundle both report. Derived,
# never retyped, so the contract cannot drift from the call site again.
figure1_repeated_cv_description <- function(x = FIGURE1_PREDICTION_CONTRACT) {
  sprintf(
    "fixed a priori model registry; %s primary; repeated grouped %d-fold (k=%d, %d repeats, group=%s, seed %d) companion",
    x$loo_scheme, x$repeated_cv_k, x$repeated_cv_k, x$repeated_cv_repeats,
    x$repeated_cv_group_col, x$repeated_cv_seed
  )
}

figure1_permutation_description <- function(x = FIGURE1_PREDICTION_CONTRACT) {
  sprintf("full-refit outcome permutation, %d draws, seed %d; p = 1/%d",
          x$outcome_permutation_draws, x$outcome_permutation_seed,
          x$outcome_permutation_draws + 1L)
}

figure1_association_description <- function(x = FIGURE1_PREDICTION_CONTRACT) {
  sprintf("Spearman with %d-sample bootstrap CI (percentile, seed %d); BH over three prespecified features",
          x$association_bootstrap_resamples, x$association_bootstrap_seed)
}

# Label integrity.
#
# The legacy adjusted ladder carries "only" in several model keys while every
# model in it also includes Sex and Group. The machine keys are stable
# identifiers and downstream tables join on them, so they are NOT renamed; what
# must never be misleading is the human-facing label a reader sees. This asserts
# that no model whose predictors include Group or Sex is displayed with a label
# claiming it is a single-predictor or behaviour-only model.
FIGURE1_BEHAVIOUR_ONLY_LABEL_PATTERN <- "(^|[^[:alnum:]])(only|alone|behaviou?r[- ]only)([^[:alnum:]]|$)"

figure1_assert_label_integrity <- function(audit,
                                           model_col = "Model",
                                           label_col = "DisplayModel",
                                           group_col = "UsesGroup",
                                           sex_col = "UsesSex",
                                           n_pred_col = "n_predictors") {
  need <- c(model_col, label_col, group_col, sex_col)
  missing <- setdiff(need, names(audit))
  if (length(missing)) {
    stop("Label-integrity audit is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  uses_group <- as.logical(audit[[group_col]])
  uses_sex <- as.logical(audit[[sex_col]])
  label <- audit[[label_col]]

  # Rule 1, a prohibition: an adjusted model may not claim to be "only" or
  # "alone". "Mean only" is exempt because it genuinely has no predictors.
  claims_only <- grepl(FIGURE1_BEHAVIOUR_ONLY_LABEL_PATTERN, label,
                       ignore.case = TRUE)
  if (n_pred_col %in% names(audit)) {
    claims_only <- claims_only & as.numeric(audit[[n_pred_col]]) > 0
  }
  bad_only <- which((uses_group | uses_sex) & claims_only)

  # Rule 2, a positive disclosure requirement, and the one that matters more.
  # Avoiding the word "only" is not enough: a bare "Movement" on a model whose
  # formula is Sex + Group + Movement_mean reads as behaviour-only to every
  # reader, which is exactly how the original defect presented. A label for an
  # adjusted model must NAME the adjustment it carries.
  bad_group <- which(uses_group & !grepl("group", label, ignore.case = TRUE))
  bad_sex <- which(uses_sex & !grepl("sex", label, ignore.case = TRUE))

  fmt <- function(idx) paste(sprintf("%s -> '%s'", audit[[model_col]][idx],
                                     label[idx]), collapse = "; ")
  msgs <- character(0)
  if (length(bad_only))
    msgs <- c(msgs, paste0("claims to be behaviour-only while adjusted: ",
                           fmt(bad_only)))
  if (length(bad_group))
    msgs <- c(msgs, paste0("uses the outcome-derived Group term without saying ",
                           "so in the label: ", fmt(bad_group)))
  if (length(bad_sex))
    msgs <- c(msgs, paste0("uses Sex without saying so in the label: ",
                           fmt(bad_sex)))
  if (length(msgs)) {
    stop("Misleading model label(s). ", paste(msgs, collapse = " | "),
         call. = FALSE)
  }
  invisible(TRUE)
}

# A model may only be exported as prospective behaviour-only / headline evidence
# if it carries no outcome-derived term. Group is derived from CombZ, so a model
# using it can never be headline evidence however it is labelled.
figure1_assert_headline_is_behaviour_only <- function(registry,
                                                      id_col = "model_id",
                                                      group_col = "uses_group",
                                                      role_col = "reporting_role",
                                                      headline_roles = "primary_behavior_only") {
  need <- c(id_col, group_col, role_col)
  missing <- setdiff(need, names(registry))
  if (length(missing)) {
    stop("Headline-export audit is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  headline <- registry[[role_col]] %in% headline_roles
  bad <- which(headline & as.logical(registry[[group_col]]))
  if (length(bad)) {
    stop("A model using the outcome-derived Group term is marked as ",
         "prospective behaviour-only evidence: ",
         paste(registry[[id_col]][bad], collapse = ", "), call. = FALSE)
  }
  if (!FIGURE1_PREDICTION_CONTRACT$headline_model_id %in% registry[[id_col]]) {
    stop("The declared headline model '",
         FIGURE1_PREDICTION_CONTRACT$headline_model_id,
         "' is absent from the primary prediction registry.", call. = FALSE)
  }
  invisible(TRUE)
}
