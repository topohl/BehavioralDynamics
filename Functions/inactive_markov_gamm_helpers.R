# ================================================================
# Inactive-phase Markov-binomial GAMM machinery
# MMMSociability
# ================================================================
# The Inactive-phase response is ~89% zeros with strong state dependence
# (P(active | prev inactive) ~ 0.062 vs P(active | prev active) ~ 0.500), so a
# Gaussian log1p magnitude model is the wrong estimand. The primary Inactive
# quantity here is instead
#
#     ActiveBin = 1{Movement > 0}
#
# modelled as a first-order state-dependent binomial GAMM. Serial dependence is
# part of the biological process and enters EXPLICITLY as PrevState rather than
# being absorbed into a residual AR1 (which bam() supports for Gaussian only).
#
# TERMINOLOGY (fixed): "inactive-phase locomotor activation",
# "probability of locomotor activity during the Inactive phase",
# "activity-bin probability". NOT sleep, sleep fragmentation, circadian
# disruption or rest disturbance.
#
# THE MANUSCRIPT QUANTITY IS THE MARGINAL PROBABILITY, NOT A TRANSITION
# PROBABILITY. predict(PrevState = INACTIVE) is an activation probability and
# predict(PrevState = ACTIVE) is a persistence probability; neither is "the
# probability of activity". The marginal is obtained by running the fitted
# transition probabilities forward from the START state:
#
#     p_0 = P(Active_0 = 1 | PrevState = START)
#     p_t = p01_t (1 - p_{t-1}) + p11_t p_{t-1}
#
# That recursion is NON-LINEAR in beta, so a naive lpmatrix Wald SE is invalid.
# Uncertainty uses coefficient simulation (display) and a numerical delta
# method (formal inference); the two are cross-validated.
#
# Batch is marginalized EQUALLY and on the PROBABILITY scale: with a logit link
# the batch-average of probabilities is the marginal quantity, whereas
# averaging linear predictors would be a different (log-odds-average) estimand.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble); library(mgcv)
})

for (.h in c("gamm_group_inference_helpers.R", "gamm_auc_helpers.R")) {
  .probe <- if (.h == "gamm_group_inference_helpers.R") "mmm_design_rows" else "mmm_trapz_weights"
  if (!exists(.probe, mode = "function", inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) source_mmm_helper(.h)
    else stop("inactive_markov_gamm_helpers.R requires ", .h, call. = FALSE)
  }
}
rm(.h, .probe)

MMM_PREV_STATE_LEVELS <- c("START", "INACTIVE", "ACTIVE")
MMM_MARKOV_SIM_DRAWS <- 5000L
MMM_MARKOV_SIM_SEED <- 20260907L

# ---------------------------------------------------------------------------
# 1. State construction
# ---------------------------------------------------------------------------

#' Build ActiveBin and the three-level PrevState history.
#'
#' PrevState is START at the first scheduled slot of a window AND at the first
#' observation after a real missing-slot gap; otherwise it is INACTIVE/ACTIVE
#' according to the immediately preceding CONTIGUOUS slot. Sequences never
#' bridge animals, windows, ActiveNight/InactiveBlock boundaries, CageChange
#' boundaries, or missing slots.
#'
#' @param block_cols Window-identifying keys in addition to AnimalNum.
mmm_build_prev_state <- function(dat, block_cols = character(), slot_col = "target_slot",
                                 value_col = "Movement") {
  keys <- c("AnimalNum", block_cols)
  missing_keys <- setdiff(keys, names(dat))
  if (length(missing_keys)) {
    stop("mmm_build_prev_state() missing key column(s): ",
         paste(missing_keys, collapse = ", "), call. = FALSE)
  }
  dat %>%
    mutate(ActiveBin = as.integer(.data[[value_col]] > 0)) %>%
    arrange(across(all_of(c(keys, slot_col)))) %>%
    group_by(across(all_of(keys))) %>%
    mutate(
      .prev_slot = dplyr::lag(.data[[slot_col]]),
      .prev_bin = dplyr::lag(.data$ActiveBin),
      .contiguous = !is.na(.data$.prev_slot) &
        (.data[[slot_col]] - .data$.prev_slot) == 1L,
      PrevState = dplyr::case_when(
        !.data$.contiguous ~ "START",
        .data$.prev_bin == 1L ~ "ACTIVE",
        TRUE ~ "INACTIVE"
      )
    ) %>%
    ungroup() %>%
    mutate(PrevState = factor(.data$PrevState, levels = MMM_PREV_STATE_LEVELS)) %>%
    select(-any_of(c(".prev_slot", ".prev_bin", ".contiguous")))
}

#' Evidence that the state construction obeys its contract.
mmm_prev_state_proof <- function(dat, block_cols = character(), slot_col = "target_slot") {
  keys <- c("AnimalNum", block_cols)
  per_block <- dat %>% group_by(across(all_of(keys))) %>%
    summarise(n = dplyr::n(),
              n_start = sum(.data$PrevState == "START"),
              first_slot_is_start =
                .data$PrevState[which.min(.data[[slot_col]])] == "START",
              n_slot_gaps = sum(diff(sort(.data[[slot_col]])) != 1L),
              .groups = "drop")
  tibble(
    n_rows = nrow(dat),
    n_blocks = nrow(per_block),
    block_keys = paste(keys, collapse = " + "),
    every_block_starts_with_START = all(per_block$first_slot_is_start),
    # One START per block, plus one for each interior gap.
    n_START = sum(dat$PrevState == "START"),
    n_START_expected = nrow(per_block) + sum(per_block$n_slot_gaps),
    start_count_matches = sum(dat$PrevState == "START") ==
      nrow(per_block) + sum(per_block$n_slot_gaps),
    n_INACTIVE = sum(dat$PrevState == "INACTIVE"),
    n_ACTIVE = sum(dat$PrevState == "ACTIVE"),
    bridges_animals = FALSE, bridges_windows = FALSE, bridges_gaps = FALSE
  )
}

#' Observed transition frequencies within a stratification.
mmm_observed_transitions <- function(dat, by = c("Sex", "Group")) {
  dat %>%
    group_by(across(all_of(c(by, "PrevState")))) %>%
    summarise(n = dplyr::n(), n_active = sum(.data$ActiveBin),
              p_active = mean(.data$ActiveBin), .groups = "drop")
}

# ---------------------------------------------------------------------------
# 2. Marginal probability recursion
# ---------------------------------------------------------------------------

#' Per-batch design matrices for one prediction grid, animal RE removed.
#'
#' Returns a list of matrices (one per batch) rather than a batch-average,
#' because the probability-scale average must be taken AFTER the inverse link.
mmm_prob_design_list <- function(model, grid, batch_levels, animal_ref,
                                 exclude = "s(AnimalNum)") {
  lapply(batch_levels, function(b) {
    nd <- grid
    nd$Batch <- factor(b, levels = levels(model$model$Batch))
    nd$AnimalNum <- factor(animal_ref, levels = levels(model$model$AnimalNum))
    X <- mgcv::predict.bam(model, newdata = nd, type = "lpmatrix")
    for (sm in model$smooth) if (sm$label %in% exclude) X[, sm$first.para:sm$last.para] <- 0
    X
  })
}

#' Batch-equal marginal probability for a set of design matrices.
.mmm_pbar <- function(Xlist, beta) {
  Reduce(`+`, lapply(Xlist, function(X) stats::plogis(as.numeric(X %*% beta)))) / length(Xlist)
}

#' Build the three design-matrix sets the recursion needs, once per group.
#'
#' @param slot_grid tibble with one row per SCHEDULED slot, carrying TimeHours
#'   and every model factor except PrevState/Batch/AnimalNum.
mmm_markov_design_sets <- function(model, slot_grid, batch_levels, animal_ref) {
  mk <- function(state) {
    g <- slot_grid
    g$PrevState <- factor(state, levels = MMM_PREV_STATE_LEVELS)
    mmm_prob_design_list(model, g, batch_levels, animal_ref)
  }
  list(START = mk("START"), INACTIVE = mk("INACTIVE"), ACTIVE = mk("ACTIVE"))
}

#' Run the marginal recursion for one coefficient vector.
#'
#' @return list(p = marginal probability per slot, p01 = activation, p11 = persistence)
mmm_markov_marginal <- function(design_sets, beta) {
  p0_all <- .mmm_pbar(design_sets$START, beta)
  p01 <- .mmm_pbar(design_sets$INACTIVE, beta)
  p11 <- .mmm_pbar(design_sets$ACTIVE, beta)
  n <- length(p01)
  p <- numeric(n)
  p[1] <- p0_all[1]                       # first scheduled slot: PrevState = START
  if (n > 1L) for (t in 2:n) p[t] <- p01[t] * (1 - p[t - 1]) + p11[t] * p[t - 1]
  list(p = p, p01 = p01, p11 = p11)
}

#' Activity-probability AUC (probability-hours) from a marginal trajectory.
mmm_markov_auc <- function(p, time_hours) sum(mmm_trapz_weights(time_hours) * p)

# ---------------------------------------------------------------------------
# 3. Uncertainty: coefficient simulation and numerical delta method
# ---------------------------------------------------------------------------

#' Coefficient draws shared by every quantity of one model.
mmm_markov_draws <- function(model, n_draws = MMM_MARKOV_SIM_DRAWS, seed = MMM_MARKOV_SIM_SEED) {
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  set.seed(seed)
  MASS::mvrnorm(n_draws, mu = stats::coef(model), Sigma = V)
}

#' Simulated marginal trajectory + AUC for one group.
mmm_markov_sim_group <- function(design_sets, B, time_hours, level = 0.95) {
  n <- length(time_hours)
  # draws x slots for each conditioning state, on the probability scale.
  pmat <- function(Xlist) {
    Reduce(`+`, lapply(Xlist, function(X) stats::plogis(tcrossprod(B, X)))) / length(Xlist)
  }
  P0 <- pmat(design_sets$START); P01 <- pmat(design_sets$INACTIVE); P11 <- pmat(design_sets$ACTIVE)
  P <- matrix(NA_real_, nrow = nrow(B), ncol = n)
  P[, 1] <- P0[, 1]
  if (n > 1L) for (t in 2:n) P[, t] <- P01[, t] * (1 - P[, t - 1]) + P11[, t] * P[, t - 1]
  w <- mmm_trapz_weights(time_hours)
  auc <- as.numeric(P %*% w)
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  qs <- function(M) t(apply(M, 2, stats::quantile, probs = probs, na.rm = TRUE))
  list(
    trajectory = tibble(
      TimeHours = time_hours,
      p = colMeans(P), p_median = apply(P, 2, stats::median),
      p_low = qs(P)[, 1], p_high = qs(P)[, 2],
      p01 = colMeans(P01), p01_low = qs(P01)[, 1], p01_high = qs(P01)[, 2],
      p11 = colMeans(P11), p11_low = qs(P11)[, 1], p11_high = qs(P11)[, 2]
    ),
    auc_draws = auc,
    auc = tibble(
      AUC_prob = mean(auc), AUC_prob_median = stats::median(auc),
      AUC_prob_low = unname(stats::quantile(auc, probs[1])),
      AUC_prob_high = unname(stats::quantile(auc, probs[2])),
      AUC_prob_sim_sd = stats::sd(auc),
      mean_active_bin_probability = mean(auc) / max(time_hours),
      expected_active_bin_percent = 100 * mean(auc) / max(time_hours)
    )
  )
}

#' Numerical-gradient delta method for a scalar function of beta.
#'
#' Central differences with a coefficient-scaled step, so the gradient is
#' stable for both large and near-zero coefficients.
mmm_numeric_delta <- function(model, fn, level = 0.95, rel_h = 1e-4) {
  b <- stats::coef(model)
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  h <- pmax(rel_h * abs(b), rel_h)
  grad <- numeric(length(b))
  for (j in seq_along(b)) {
    bp <- b; bm <- b
    bp[j] <- bp[j] + h[j]; bm[j] <- bm[j] - h[j]
    grad[j] <- (fn(bp) - fn(bm)) / (2 * h[j])
  }
  est <- fn(b)
  se <- sqrt(max(0, as.numeric(t(grad) %*% V %*% grad)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  stat <- if (se > 0) est / se else NA_real_
  tibble(estimate = est, se = se, ci_low = est - z * se, ci_high = est + z * se,
         ci_level = level, statistic = stat,
         p_raw = if (is.na(stat)) NA_real_ else 2 * stats::pnorm(abs(stat), lower.tail = FALSE))
}

#' AUC-difference between two groups, by both uncertainty routes.
#'
#' The delta-method p is used for formal multiplicity; the simulation interval
#' is the manuscript display interval. Their agreement is reported, and a
#' material disagreement is a STOP condition.
mmm_markov_auc_contrast <- function(model, ds1, ds0, time_hours, B,
                                    label1, label0, level = 0.95) {
  gfun <- function(beta) {
    mmm_markov_auc(mmm_markov_marginal(ds1, beta)$p, time_hours) -
      mmm_markov_auc(mmm_markov_marginal(ds0, beta)$p, time_hours)
  }
  dm <- mmm_numeric_delta(model, gfun, level = level)

  sim1 <- mmm_markov_sim_group(ds1, B, time_hours, level)$auc_draws
  sim0 <- mmm_markov_sim_group(ds0, B, time_hours, level)$auc_draws
  d <- sim1 - sim0
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  win <- max(time_hours)

  tibble(
    contrast = paste0(label1, "-", label0),
    group_comp = label1, group_ref = label0,
    AUC_diff_prob = dm$estimate,
    AUC_diff_delta_SE = dm$se,
    AUC_diff_delta_ci_low = dm$ci_low, AUC_diff_delta_ci_high = dm$ci_high,
    AUC_diff_p = dm$p_raw,
    AUC_diff_sim_median = stats::median(d),
    AUC_diff_sim_ci_low = unname(stats::quantile(d, probs[1])),
    AUC_diff_sim_ci_high = unname(stats::quantile(d, probs[2])),
    AUC_diff_sim_SD = stats::sd(d),
    se_ratio_sim_over_delta = stats::sd(d) / pmax(.Machine$double.eps, dm$se),
    diff_active_bin_percent_points = 100 * dm$estimate / win,
    diff_pct_pts_ci_low = 100 * dm$ci_low / win,
    diff_pct_pts_ci_high = 100 * dm$ci_high / win,
    window_hours = win, ci_level = level
  )
}

# ---------------------------------------------------------------------------
# 4. Adequacy gate
# ---------------------------------------------------------------------------

#' Calibration and discrimination for a fitted binomial model.
mmm_markov_calibration <- function(model, dat, label) {
  p <- as.numeric(stats::fitted(model))
  y <- as.integer(dat$ActiveBin)
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  lp <- stats::qlogis(p)
  recal <- tryCatch(stats::glm(y ~ lp, family = stats::binomial()),
                    error = function(e) NULL)
  tibble(
    model_label = label,
    n_obs = length(y), observed_rate = mean(y), predicted_rate = mean(p),
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    calibration_intercept = if (is.null(recal)) NA_real_ else unname(stats::coef(recal)[1]),
    calibration_slope = if (is.null(recal)) NA_real_ else unname(stats::coef(recal)[2]),
    auc_roc = {
      r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
      if (n1 == 0 || n0 == 0) NA_real_ else (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    }
  )
}

#' Observed vs predicted activity probability by predicted-risk decile.
mmm_markov_decile_calibration <- function(model, dat, label, n_bins = 10L) {
  p <- as.numeric(stats::fitted(model))
  tibble(model_label = label, p_hat = p, y = as.integer(dat$ActiveBin),
         PrevState = as.character(dat$PrevState)) %>%
    mutate(bin = dplyr::ntile(.data$p_hat, n_bins)) %>%
    group_by(.data$model_label, .data$bin) %>%
    summarise(n = dplyr::n(), mean_predicted = mean(.data$p_hat),
              observed = mean(.data$y), .groups = "drop") %>%
    mutate(abs_gap = abs(.data$mean_predicted - .data$observed))
}

#' Transition-specific calibration: p01 and p11 separately.
mmm_markov_transition_calibration <- function(model, dat, label) {
  p <- as.numeric(stats::fitted(model))
  tibble(model_label = label, PrevState = as.character(dat$PrevState),
         p_hat = p, y = as.integer(dat$ActiveBin)) %>%
    group_by(.data$model_label, .data$PrevState) %>%
    summarise(n = dplyr::n(), mean_predicted = mean(.data$p_hat),
              observed = mean(.data$y),
              brier = mean((.data$p_hat - .data$y)^2), .groups = "drop") %>%
    mutate(quantity = dplyr::case_when(
      .data$PrevState == "INACTIVE" ~ "p01_activation",
      .data$PrevState == "ACTIVE" ~ "p11_persistence",
      TRUE ~ "p0_start"), abs_gap = abs(.data$mean_predicted - .data$observed))
}

#' Lag-1 residual serial dependence within uninterrupted sequences.
#'
#' Computed for a model WITH and WITHOUT PrevState; the explicit state term
#' should remove most first-order structure.
mmm_markov_residual_dependence <- function(model, dat, block_cols, label,
                                           slot_col = "target_slot") {
  r <- as.numeric(stats::residuals(model, type = "pearson"))
  if (length(r) != nrow(dat)) return(tibble())
  keys <- c("AnimalNum", block_cols)
  key_chr <- do.call(paste, c(lapply(keys, function(k) as.character(dat[[k]])), sep = "\r"))
  slot <- as.integer(dat[[slot_col]])
  new_seq <- c(TRUE, key_chr[-1] != key_chr[-length(key_chr)]) |
    c(FALSE, diff(slot) != 1L)
  seq_id <- cumsum(new_seq)
  vals <- unlist(lapply(split(r, seq_id), function(x) {
    if (length(x) < 3L) return(NA_real_)
    stats::cor(x[-1], x[-length(x)], use = "complete.obs")
  }), use.names = FALSE)
  tibble(model_label = label, n_sequences = sum(!is.na(vals)),
         lag1_mean_resid_acf = mean(vals, na.rm = TRUE),
         lag1_median_resid_acf = stats::median(vals, na.rm = TRUE),
         lag1_mean_abs_resid_acf = mean(abs(vals), na.rm = TRUE))
}

#' Observed vs model-predicted transition matrix within a stratification.
mmm_markov_transition_matrices <- function(model, dat, by = c("Sex", "Group")) {
  p <- as.numeric(stats::fitted(model))
  tibble(dat %>% dplyr::select(all_of(by), "PrevState", "ActiveBin"), p_hat = p) %>%
    filter(.data$PrevState != "START") %>%
    group_by(across(all_of(c(by, "PrevState")))) %>%
    summarise(n = dplyr::n(),
              observed_p_active = mean(.data$ActiveBin),
              predicted_p_active = mean(.data$p_hat),
              gap = mean(.data$p_hat) - mean(.data$ActiveBin), .groups = "drop")
}

#' Apply the prespecified adequacy criteria and return a pass/fail verdict.
mmm_markov_adequacy_gate <- function(calib, resid_with, resid_without,
                                     trans_calib, se_ratios,
                                     slope_tol = c(0.8, 1.25),
                                     intercept_tol = 0.25,
                                     resid_tol = 0.15,
                                     transition_gap_tol = 0.05,
                                     se_ratio_tol = c(0.5, 2.0)) {
  checks <- tibble(
    check = c("calibration_slope_in_range", "calibration_intercept_near_zero",
              "residual_lag1_below_tol", "residual_reduced_vs_no_prevstate",
              "transition_calibration_gap_small", "delta_vs_simulation_SE_concordant"),
    value = c(
      paste(round(range(calib$calibration_slope), 3), collapse = " to "),
      paste(round(range(calib$calibration_intercept), 3), collapse = " to "),
      round(max(abs(resid_with$lag1_mean_resid_acf)), 4),
      paste0(round(max(abs(resid_without$lag1_mean_resid_acf)), 3), " -> ",
             round(max(abs(resid_with$lag1_mean_resid_acf)), 3)),
      round(max(trans_calib$abs_gap), 4),
      paste(round(range(se_ratios), 3), collapse = " to ")
    ),
    passed = c(
      all(calib$calibration_slope >= slope_tol[1] & calib$calibration_slope <= slope_tol[2]),
      all(abs(calib$calibration_intercept) <= intercept_tol),
      max(abs(resid_with$lag1_mean_resid_acf)) <= resid_tol,
      max(abs(resid_with$lag1_mean_resid_acf)) < max(abs(resid_without$lag1_mean_resid_acf)),
      max(trans_calib$abs_gap) <= transition_gap_tol,
      all(se_ratios >= se_ratio_tol[1] & se_ratios <= se_ratio_tol[2])
    )
  )
  checks %>% mutate(
    verdict = ifelse(all(.data$passed), "PASS", "NEEDS_REVIEW"),
    overall_status = ifelse(all(.data$passed), "READY", "NEEDS_REVIEW")
  )
}
