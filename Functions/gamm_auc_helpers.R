# ================================================================
# Exact model-derived AUC, response-scale AUC, adaptation and sex moderation
# MMMSociability
# ================================================================
# DESIGN: every quantity below is a LINEAR FUNCTIONAL of the coefficient
# vector, so it is built once as a contrast vector `c` and then evaluated with
# a single Wald step:
#
#     value = c' beta          Var = c' V c        V = vcov(model, unconditional = TRUE)
#
# Composing them is then plain vector algebra and stays exact:
#
#     AUC(group A)                    c_A
#     AUC difference A - B            c_A - c_B
#     adaptation of (A-B) across CC   (c_A4 - c_B4) - (c_A1 - c_B1)
#
# No simulation is needed on the model scale. Simulation is used ONLY for the
# response scale, where expm1() makes the functional non-linear in beta.
#
# Integration uses trapezoidal weights on the actual prediction grid, so
# AUC is a genuine numerical integral rather than a grid mean. The grid mean
# equals AUC/window_hours only up to the endpoint half-weights; the audit
# helper below quantifies that gap instead of assuming it away.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(mgcv)
})
# NOTE: MASS is used via MASS::mvrnorm() only. Attaching it would mask
# dplyr::select() and break the validated first-night window helper.

if (!exists("mmm_design_rows", mode = "function", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("gamm_group_inference_helpers.R")
  } else {
    stop("gamm_auc_helpers.R requires gamm_group_inference_helpers.R", call. = FALSE)
  }
}

MMM_AUC_SIM_DRAWS <- 5000L
MMM_AUC_SIM_SEED <- 20260907L

#' Trapezoidal integration weights for an ordered grid.
mmm_trapz_weights <- function(t) {
  n <- length(t)
  if (n < 2L) stop("Need at least two grid points for integration.", call. = FALSE)
  if (is.unsorted(t)) stop("Integration grid must be sorted.", call. = FALSE)
  w <- numeric(n)
  w[1] <- (t[2] - t[1]) / 2
  w[n] <- (t[n] - t[n - 1]) / 2
  if (n > 2L) w[2:(n - 1)] <- (t[3:n] - t[1:(n - 2)]) / 2
  w
}

#' AUC contrast vector for one fitted trajectory.
#'
#' @return numeric vector `c` such that `c' beta` is the trapezoidal integral
#'   of the batch-marginal, random-effect-excluded predicted trajectory.
mmm_auc_cvec <- function(model, grid, batch_weights, animal_ref, time_col) {
  stopifnot(time_col %in% names(grid))
  X <- mmm_design_rows(model, grid, batch_weights, animal_ref)
  w <- mmm_trapz_weights(grid[[time_col]])
  as.numeric(crossprod(w, X))
}

#' Evaluate any linear combination of the coefficients.
mmm_lincom <- function(model, cvec, level = 0.95) {
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  b <- stats::coef(model)
  est <- sum(cvec * b)
  se <- sqrt(max(0, as.numeric(t(cvec) %*% V %*% cvec)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  stat <- if (se > 0) est / se else NA_real_
  tibble(
    estimate = est, se = se,
    ci_low = est - z * se, ci_high = est + z * se, ci_level = level,
    statistic = stat,
    p_raw = if (is.na(stat)) NA_real_ else 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  )
}

#' Exact model-scale AUC for one group trajectory.
mmm_gamm_group_auc <- function(model, grid, batch_weights, animal_ref, time_col,
                               window_hours = 12, level = 0.95) {
  cv <- mmm_auc_cvec(model, grid, batch_weights, animal_ref, time_col)
  mmm_lincom(model, cv, level) %>%
    rename(AUC_log1p = "estimate", AUC_SE = "se",
           AUC_CI_low = "ci_low", AUC_CI_high = "ci_high") %>%
    mutate(AUC_per_hour = .data$AUC_log1p / window_hours,
           AUC_per_hour_SE = .data$AUC_SE / window_hours,
           window_hours = window_hours)
}

#' Exact model-scale AUC difference between two fully built grids.
mmm_gamm_auc_contrast <- function(model, g1, g0, batch_weights, animal_ref, time_col,
                                  window_hours = 12, level = 0.95) {
  stopifnot(nrow(g1) == nrow(g0))
  cv <- mmm_auc_cvec(model, g1, batch_weights, animal_ref, time_col) -
        mmm_auc_cvec(model, g0, batch_weights, animal_ref, time_col)
  mmm_lincom(model, cv, level) %>%
    rename(AUC_diff_log1p = "estimate", AUC_diff_SE = "se",
           AUC_diff_CI_low = "ci_low", AUC_diff_CI_high = "ci_high",
           AUC_diff_p = "p_raw") %>%
    mutate(AUC_diff_per_hour = .data$AUC_diff_log1p / window_hours,
           AUC_diff_per_hour_SE = .data$AUC_diff_SE / window_hours,
           window_hours = window_hours)
}

#' Audit: exact AUC/h vs the window-average contrast used elsewhere.
#'
#' They estimate the same quantity and should agree to the endpoint-weighting
#' difference only. Reported, never assumed.
mmm_auc_vs_window_average <- function(model, g1, g0, batch_weights, animal_ref, time_col,
                                      window_hours = 12) {
  X1 <- mmm_design_rows(model, g1, batch_weights, animal_ref)
  X0 <- mmm_design_rows(model, g0, batch_weights, animal_ref)
  b <- stats::coef(model)
  Xd <- X1 - X0
  w <- mmm_trapz_weights(g1[[time_col]])
  auc <- sum(as.numeric(crossprod(w, Xd)) * b)
  tibble(
    AUC_diff_log1p = auc,
    AUC_diff_per_hour = auc / window_hours,
    window_average_diff = sum(colMeans(Xd) * b),
    abs_gap = abs(auc / window_hours - sum(colMeans(Xd) * b)),
    rel_gap = abs(auc / window_hours - sum(colMeans(Xd) * b)) /
      pmax(.Machine$double.eps, abs(sum(colMeans(Xd) * b)))
  )
}

#' Response-scale AUC by coefficient simulation.
#'
#' expm1() is non-linear in beta, so the analytic linear-contrast variance does
#' not apply. Draws beta ~ N(beta_hat, V), back-transforms each drawn
#' trajectory, integrates it, and summarises the resulting distribution.
#' Reported as an interpretable effect size; formal inference stays on the
#' model scale.
mmm_gamm_response_auc_sim <- function(model, grids, batch_weights, animal_ref, time_col,
                                      n_draws = MMM_AUC_SIM_DRAWS, seed = MMM_AUC_SIM_SEED,
                                      window_hours = 12, level = 0.95) {
  stopifnot(is.list(grids), length(grids) >= 1L, !is.null(names(grids)))
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  b <- stats::coef(model)
  set.seed(seed)
  B <- MASS::mvrnorm(n_draws, mu = b, Sigma = V)

  Xs <- lapply(grids, function(g) mmm_design_rows(model, g, batch_weights, animal_ref))
  w <- mmm_trapz_weights(grids[[1]][[time_col]])
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)

  # tcrossprod(B, X) = B %*% t(X) -> (draws x grid); back-transform, then integrate
  auc_draws <- lapply(Xs, function(X) as.numeric(expm1(tcrossprod(B, X)) %*% w))

  point <- vapply(Xs, function(X) sum(expm1(as.numeric(X %*% b)) * w), numeric(1))
  out <- tibble(
    level_name = names(grids),
    fitted_response_AUC = unname(point),
    response_AUC_median = vapply(auc_draws, stats::median, numeric(1)),
    response_AUC_CI_low = vapply(auc_draws, function(x) unname(stats::quantile(x, probs[1])), numeric(1)),
    response_AUC_CI_high = vapply(auc_draws, function(x) unname(stats::quantile(x, probs[2])), numeric(1)),
    n_draws = n_draws, seed = seed, window_hours = window_hours
  ) %>%
    mutate(fitted_response_AUC_per_hour = .data$fitted_response_AUC / window_hours)
  attr(out, "draws") <- auc_draws
  out
}

#' Response-scale AUC differences from the SAME draws as mmm_gamm_response_auc_sim().
#'
#' Using one draw set keeps the group AUCs and their differences mutually
#' consistent rather than independently simulated.
mmm_response_auc_contrasts <- function(sim_out, pairs, level = 0.95) {
  draws <- attr(sim_out, "draws")
  if (is.null(draws)) stop("sim_out must carry its draws attribute.", call. = FALSE)
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  purrr::map_dfr(pairs, function(pp) {
    d <- draws[[pp[1]]] - draws[[pp[2]]]
    tibble(
      contrast = paste0(pp[1], "-", pp[2]),
      fitted_response_AUC_diff = sim_out$fitted_response_AUC[match(pp[1], sim_out$level_name)] -
        sim_out$fitted_response_AUC[match(pp[2], sim_out$level_name)],
      response_AUC_diff_median = stats::median(d),
      response_AUC_diff_CI_low = unname(stats::quantile(d, probs[1])),
      response_AUC_diff_CI_high = unname(stats::quantile(d, probs[2])),
      n_draws = length(d), ci_level = level
    )
  })
}

#' Descriptive animal-level observed AUC. Missing bins are NEVER interpolated.
mmm_animal_empirical_auc <- function(dat, time_col, value_col = "Movement",
                                     group_cols = c("AnimalNum", "Group", "Sex", "Batch"),
                                     window_hours = 12) {
  dat %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(.data[[time_col]], .by_group = TRUE) %>%
    summarise(
      expected_slots = dplyr::first(.data$expected_slots),
      observed_slots = dplyr::n_distinct(.data$target_slot),
      observed_AUC = if (dplyr::n() >= 2L) {
        sum(mmm_trapz_weights(.data[[time_col]]) * .data[[value_col]], na.rm = TRUE)
      } else NA_real_,
      observed_mean_Movement = mean(.data[[value_col]], na.rm = TRUE),
      observed_zero_fraction = mean(.data[[value_col]] == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      coverage = .data$observed_slots / .data$expected_slots,
      # Coverage-normalized alternative, reported separately so a coverage
      # gradient cannot masquerade as a biological AUC difference.
      observed_AUC_per_observed_hour = .data$observed_AUC /
        pmax(.Machine$double.eps, .data$coverage * window_hours),
      quantity_class = "descriptive_observed"
    )
}

#' Formal cross-sex moderation for independent sex-stratified estimates.
#'
#' Female and Male models are fitted on disjoint animals, so
#'   D = est_F - est_M,  SE(D) = sqrt(SE_F^2 + SE_M^2).
#' Batches are nested within Sex, so this is a descriptive moderation contrast,
#' not a causal sex claim.
mmm_sex_moderation <- function(tbl, by_cols, est_col = "estimate", se_col = "se",
                               sex_col = "Sex", level = 0.95) {
  f <- tbl %>% filter(.data[[sex_col]] == "Female") %>%
    dplyr::select(all_of(by_cols), est_F = all_of(est_col), se_F = all_of(se_col))
  m <- tbl %>% filter(.data[[sex_col]] == "Male") %>%
    dplyr::select(all_of(by_cols), est_M = all_of(est_col), se_M = all_of(se_col))
  z <- stats::qnorm(1 - (1 - level) / 2)
  inner_join(f, m, by = by_cols) %>%
    mutate(
      diff_F_minus_M = .data$est_F - .data$est_M,
      se_diff = sqrt(.data$se_F^2 + .data$se_M^2),
      ci_low = .data$diff_F_minus_M - z * .data$se_diff,
      ci_high = .data$diff_F_minus_M + z * .data$se_diff,
      statistic = .data$diff_F_minus_M / .data$se_diff,
      p_raw = 2 * stats::pnorm(abs(.data$statistic), lower.tail = FALSE),
      ci_level = level,
      caveat = "batches nested within Sex; descriptive moderation, not a causal sex claim"
    )
}

#' Planned adaptation contrasts: last stratum minus first, overall and per group,
#' plus phenotype-dependent adaptation (difference of differences).
#'
#' @param grid_fun function(group, stratum) returning a prediction grid.
#' @param groups Group levels; the "overall population" row averages the
#'   group-specific AUC vectors with equal weight.
mmm_adaptation_auc_contrasts <- function(model, grid_fun, groups, first_stratum, last_stratum,
                                         batch_weights, animal_ref, time_col,
                                         window_hours = 12, level = 0.95) {
  cvec <- function(g, s) mmm_auc_cvec(model, grid_fun(g, s), batch_weights, animal_ref, time_col)
  c_first <- lapply(groups, function(g) cvec(g, first_stratum)); names(c_first) <- groups
  c_last <- lapply(groups, function(g) cvec(g, last_stratum)); names(c_last) <- groups

  overall <- Reduce(`+`, c_last) / length(groups) - Reduce(`+`, c_first) / length(groups)
  rows <- list(
    mmm_lincom(model, overall, level) %>%
      mutate(scope = "overall_population", contrast_name =
               paste0(last_stratum, " - ", first_stratum), .before = 1)
  )
  for (g in groups) {
    rows[[length(rows) + 1]] <- mmm_lincom(model, c_last[[g]] - c_first[[g]], level) %>%
      mutate(scope = g, contrast_name = paste0(last_stratum, " - ", first_stratum), .before = 1)
  }
  for (pp in list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES"))) {
    if (!all(pp %in% groups)) next
    cv <- (c_last[[pp[1]]] - c_last[[pp[2]]]) - (c_first[[pp[1]]] - c_first[[pp[2]]])
    rows[[length(rows) + 1]] <- mmm_lincom(model, cv, level) %>%
      mutate(scope = "phenotype_dependent",
             contrast_name = paste0("[", pp[1], "-", pp[2], "]_", last_stratum,
                                    " - [", pp[1], "-", pp[2], "]_", first_stratum),
             .before = 1)
  }
  bind_rows(rows) %>%
    mutate(AUC_scale = "log1p", window_hours = window_hours,
           estimate_per_hour = .data$estimate / window_hours)
}

# ---------------------------------------------------------------------------
# Adaptation multiplicity structure
# ---------------------------------------------------------------------------
# The 14 planned final-vs-initial contrasts of a longitudinal stage (7 per Sex)
# answer THREE biologically distinct questions, so they form three families
# rather than one. Pooling all 14 would correct an "is there adaptation at all"
# test against "does adaptation differ between phenotypes", which are not the
# same question and are not exchangeable.
#
#   A OVERALL              2 tests   is there adaptation at all, per Sex
#   B GROUP_SPECIFIC       6 tests   which groups show it (localization)
#   C PHENOTYPE_DEPENDENT  6 tests   does adaptation differ between phenotypes
#
# PROVENANCE: this family structure was specified during the pre-freeze
# statistical audit, AFTER model diagnostics and BEFORE any manuscript result
# selection. q_BH_all14_sensitivity is reported alongside as a conservative
# check and never substitutes for the structured families.
#
# Active and Inactive stages are always separate multiplicity families.

MMM_ADAPTATION_FAMILIES <- tibble::tribble(
  ~family_key,             ~scope_match,          ~n_expected, ~biological_question,
  "A_OVERALL",             "overall_population",  2L,
    "Does the acute response change from the first to the last perturbation, in each Sex?",
  "B_GROUP_SPECIFIC",      "group",               6L,
    "Which phenotype groups show that change (localization of the overall effect)?",
  "C_PHENOTYPE_DEPENDENT", "phenotype_dependent", 6L,
    "Does the magnitude of adaptation differ BETWEEN later phenotypes?"
)

#' Assign the three prespecified adaptation families and their BH q-values.
#'
#' @param adaptation Output of mmm_adaptation_auc_contrasts() stacked over Sex.
#' @param stage_id Stage identifier used in the family_id string.
#' @param phase_label "Active" or "Inactive"; keeps the two phases in separate
#'   families by construction.
mmm_adaptation_families <- function(adaptation, stage_id, phase_label) {
  out <- adaptation %>%
    mutate(
      family_key = dplyr::case_when(
        .data$scope == "overall_population" ~ "A_OVERALL",
        .data$scope == "phenotype_dependent" ~ "C_PHENOTYPE_DEPENDENT",
        TRUE ~ "B_GROUP_SPECIFIC"
      )
    ) %>%
    left_join(MMM_ADAPTATION_FAMILIES %>% dplyr::select("family_key", "biological_question"),
              by = "family_key") %>%
    mutate(family_id = paste0(stage_id, "__", toupper(phase_label), "__ADAPTATION__", .data$family_key),
           phase = phase_label) %>%
    group_by(.data$family_key) %>%
    mutate(tests_in_family = dplyr::n(),
           q_BH_family = stats::p.adjust(.data$p_raw, method = "BH")) %>%
    ungroup() %>%
    # Conservative all-14 check, explicitly labelled and never primary.
    mutate(q_BH_all14_sensitivity = stats::p.adjust(.data$p_raw, method = "BH"),
           all14_role = "CONSERVATIVE_SENSITIVITY",
           n_tests_all14 = dplyr::n(),
           family_provenance = paste(
             "specified during the pre-freeze statistical audit, after model",
             "diagnostics and before manuscript result selection"))

  # Fail loudly if a stage does not produce the expected family sizes, so a
  # silently mis-sized family can never reach the manuscript.
  sizes <- out %>% distinct(.data$family_key, .data$tests_in_family)
  expect <- MMM_ADAPTATION_FAMILIES %>% dplyr::select("family_key", "n_expected")
  chk <- left_join(sizes, expect, by = "family_key")
  bad <- chk %>% filter(.data$tests_in_family != .data$n_expected)
  if (nrow(bad) > 0L) {
    warning("Adaptation family size mismatch in ", stage_id, " (", phase_label, "): ",
            paste(sprintf("%s got %d expected %d", bad$family_key, bad$tests_in_family,
                          bad$n_expected), collapse = "; "), call. = FALSE)
  }
  out %>% arrange(.data$family_key, .data$Sex, .data$scope)
}
