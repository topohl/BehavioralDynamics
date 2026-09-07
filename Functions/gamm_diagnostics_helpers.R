# ================================================================
# GAMM diagnostics: CI-width decomposition, batch weighting, model checks
# MMMSociability
# ================================================================
# These exist to EXPLAIN model behaviour, never to select a model. In
# particular the no-AR1 and conditional-covariance variants are diagnostics
# only: narrower ribbons are not a reason to adopt them.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(mgcv)
})

if (!exists("mmm_design_rows", mode = "function", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("gamm_group_inference_helpers.R")
  } else {
    stop("gamm_diagnostics_helpers.R requires gamm_group_inference_helpers.R", call. = FALSE)
  }
}

#' Pointwise 95% CI half-widths for one trajectory under a chosen covariance.
#'
#' @param unconditional TRUE = smoothing-parameter-corrected Bayesian
#'   covariance (the inferential default); FALSE = conditional-on-lambda,
#'   which is narrower but understates smoothing-parameter uncertainty.
mmm_traj_ci_width <- function(model, grid, batch_weights, animal_ref,
                              unconditional = TRUE, level = 0.95) {
  X <- mmm_design_rows(model, grid, batch_weights, animal_ref)
  V <- mgcv::vcov.gam(model, unconditional = unconditional)
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  2 * z * se
}

#' Summarise CI width for a named set of diagnostic variants.
#'
#' @param variants named list; each element is
#'   list(model =, batch_weights =, unconditional =, note =).
#' @param grid_fun function(group) returning that group's prediction grid.
mmm_ci_width_diagnostics <- function(variants, grid_fun, groups, animal_ref, sex_label) {
  purrr::map_dfr(names(variants), function(vn) {
    v <- variants[[vn]]
    purrr::map_dfr(groups, function(g) {
      w <- mmm_traj_ci_width(v$model, grid_fun(g), v$batch_weights, animal_ref,
                             unconditional = isTRUE(v$unconditional))
      tibble(Sex = sex_label, Group = g, variant = vn,
             mean_CI_width = mean(w), median_CI_width = stats::median(w),
             min_CI_width = min(w), max_CI_width = max(w),
             note = v$note %||% NA_character_)
    })
  }) %>%
    group_by(.data$Sex, .data$Group) %>%
    mutate(ratio_vs_primary = .data$mean_CI_width /
             .data$mean_CI_width[.data$variant == "A_primary"][1]) %>%
    ungroup()
}

#' Equal-batch vs animal-count-batch weighting comparison.
#'
#' Batch is an experimental blocking factor with approximately balanced planned
#' n, so the two weightings answer slightly different questions:
#'   equal weight  -> the average batch, i.e. the designed experiment
#'   count weight  -> the average animal, i.e. the realised cohort
mmm_batch_weighting_diagnostic <- function(model, grid_fun, groups, animal_ref, time_col,
                                           batch_counts, sex_label, window_hours = 12) {
  equal_w <- setNames(rep(1, length(batch_counts)), names(batch_counts))
  weightings <- list(equal_per_batch = equal_w, animal_count_per_batch = batch_counts)

  traj_auc <- purrr::map_dfr(names(weightings), function(wn) {
    bw <- weightings[[wn]]
    purrr::map_dfr(groups, function(g) {
      gr <- grid_fun(g)
      auc <- mmm_gamm_group_auc(model, gr, bw, animal_ref, time_col, window_hours)
      width <- mmm_traj_ci_width(model, gr, bw, animal_ref)
      tibble(Sex = sex_label, Group = g, weighting = wn,
             AUC_log1p = auc$AUC_log1p, AUC_SE = auc$AUC_SE,
             AUC_CI_low = auc$AUC_CI_low, AUC_CI_high = auc$AUC_CI_high,
             mean_CI_width = mean(width))
    })
  })

  contrasts <- purrr::map_dfr(names(weightings), function(wn) {
    bw <- weightings[[wn]]
    purrr::map_dfr(list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES")), function(pp) {
      ct <- mmm_gamm_auc_contrast(model, grid_fun(pp[1]), grid_fun(pp[2]), bw, animal_ref,
                                  time_col, window_hours)
      tibble(Sex = sex_label, contrast = paste0(pp[1], "-", pp[2]), weighting = wn,
             AUC_diff_log1p = ct$AUC_diff_log1p, AUC_diff_SE = ct$AUC_diff_SE,
             AUC_diff_CI_low = ct$AUC_diff_CI_low, AUC_diff_CI_high = ct$AUC_diff_CI_high,
             AUC_diff_p = ct$AUC_diff_p)
    })
  })

  list(group_auc = traj_auc, contrasts = contrasts)
}

#' Mean within-sequence residual ACF, lags 1..max_lag.
#'
#' Sequences are the AR1 blocks, so no lag is ever computed across a boundary.
mmm_residual_acf <- function(model, ar_start, max_lag = 6L, label = NA_character_,
                             use_whitened = TRUE) {
  # For a bam() fitted with rho=, residuals() returns UN-whitened residuals, so
  # their ACF is unchanged by the AR1 term and the "after" diagnostic would be
  # meaningless. mgcv stores the AR1-standardised residuals in $std.rsd; use
  # those whenever they are present so before/after is a like-for-like check.
  std <- if (isTRUE(use_whitened)) model$std.rsd else NULL
  r <- if (!is.null(std) && length(std) == length(ar_start)) {
    as.numeric(std)
  } else {
    as.numeric(stats::residuals(model, type = "pearson"))
  }
  resid_kind <- if (!is.null(std) && length(std) == length(ar_start)) "ar1_standardised" else "pearson"
  if (length(r) != length(ar_start)) return(tibble())
  seq_id <- cumsum(ar_start)
  purrr::map_dfr(seq_len(max_lag), function(L) {
    vals <- unlist(lapply(split(r, seq_id), function(x) {
      if (length(x) <= L) return(NA_real_)
      stats::cor(x[-seq_len(L)], x[seq_len(length(x) - L)], use = "complete.obs")
    }), use.names = FALSE)
    tibble(model_label = label, lag = L, residual_kind = resid_kind,
           mean_acf = mean(vals, na.rm = TRUE),
           median_acf = stats::median(vals, na.rm = TRUE),
           mean_abs_acf = mean(abs(vals), na.rm = TRUE),
           n_sequences = sum(!is.na(vals)))
  })
}

#' Full diagnostic record for one primary model, before and after AR1.
mmm_model_diagnostics <- function(fit, dat, label, response_col) {
  m <- fit$model
  s <- summary(m)
  kc <- tryCatch(as.data.frame(mgcv::k.check(m)), error = function(e) NULL)
  kc_tbl <- if (is.null(kc)) tibble() else {
    kc %>% tibble::rownames_to_column("smooth") %>% as_tibble() %>%
      mutate(model_label = label, .before = 1)
  }
  r <- as.numeric(stats::residuals(m, type = "deviance"))
  qq <- stats::qqnorm(r, plot.it = FALSE)
  summary_row <- tibble(
    model_label = label,
    rho = fit$rho, ar1_applied = fit$ar1_applied,
    dev_expl = unname(s$dev.expl), r_sq_adj = unname(s$r.sq),
    scale_est = unname(s$scale), edf_total = sum(m$edf),
    n_obs = nrow(dat),
    zero_fraction = mean(dat[[response_col]] == 0, na.rm = TRUE),
    resid_sd = stats::sd(r, na.rm = TRUE),
    resid_skew = mean((r - mean(r))^3) / stats::sd(r)^3,
    resid_kurtosis = mean((r - mean(r))^4) / stats::sd(r)^4,
    qq_cor = stats::cor(qq$x, qq$y, use = "complete.obs"),
    shapiro_p = tryCatch(stats::shapiro.test(sample(r, min(5000, length(r))))$p.value,
                         error = function(e) NA_real_),
    converged = isTRUE(m$converged),
    n_warnings = length(m$outer.info$conv %||% character(0))
  )
  acf_before <- mmm_residual_acf(fit$model_stage1, dat$.ar_start, label = paste0(label, "|before_AR1")) %>%
    mutate(stage = "before_AR1", .before = 1)
  acf_after <- mmm_residual_acf(m, dat$.ar_start, label = paste0(label, "|after_AR1")) %>%
    mutate(stage = "after_AR1", .before = 1)
  list(summary = summary_row, k_check = kc_tbl, acf = bind_rows(acf_before, acf_after),
       residuals = tibble(model_label = label,
                          fitted = as.numeric(stats::fitted(m)),
                          resid_deviance = r,
                          theoretical_q = qq$x, sample_q = qq$y))
}
