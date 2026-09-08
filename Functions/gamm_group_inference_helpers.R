# ================================================================
# Shared GAMM group-inference machinery
# MMMSociability
# ================================================================
# Used by the three Active-window GAMM stages:
#   Analysis/20_first_night_gamm.R                 PRIMARY   (CC1 night 1)
#   Analysis/21_cc1_active_longitudinal_gamm.R     SECONDARY A (CC1 nights 1..N)
#   Analysis/22_repeated_cagechange_acute_gamm.R   SECONDARY B (CC1..CC4 acute)
#
# WHY THE MODEL IS PARAMETERIZED THE WAY IT IS
# The conceptual specification is
#     Batch + Group * <Stratum> + s(Time) + s(Time, by = Group x Stratum) + s(Animal)
# `bs = "sz"` is the obvious mgcv spelling of the factor-smooth part, but it is
# NOT identifiable alongside the parametric Group * Stratum term: the sz basis
# carries a non-zero constant per factor-level combination, so it absorbs the
# level offsets. Measured on the real CC1 male data this showed up as a
# rank-deficient parametric table (Group 1 df instead of 2, Stratum 0 df
# instead of 3).
#
# The identifiable equivalent used here is an ORDERED factor `by=` smooth. With
# an ordered factor mgcv drops the reference level and fits centred DIFFERENCE
# smooths, which carry no constant and therefore leave the parametric main
# effects and interaction at full rank (verified: Group 2 df, Stratum 3 df,
# interaction 6 df). Same estimand, correct algebra.
#
# AR1 is fitted in two stages, exactly as the archived GAMM did, but with
# correct block boundaries supplied by acute_active_window_helpers.R:
#   stage 1  fit without AR1
#   rho      lag-1 correlation of stage-1 Pearson residuals, computed only
#            WITHIN sequences (never across an AR.start boundary)
#   stage 2  refit with rho and AR.start
# `AR.start` must be a column of `data`; passing it through a wrapper's `...`
# fails, because bam() evaluates it non-standardly.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(mgcv)
})

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

MMM_GAMM_K <- 6L
MMM_GAMM_METHOD <- "fREML"

# The three planned group contrasts, in a fixed order, shared by every stage.
MMM_GROUP_PAIRS <- list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES"))

# Biological hierarchy of the planned contrasts. This labels the contrasts; it
# never alters any multiplicity family.
MMM_CONTRAST_ROLE <- c(
  "SUS-RES" = "PRIMARY_PHENOTYPE",
  "SUS-CON" = "CONTEXTUAL_CONTROL",
  "RES-CON" = "CONTEXTUAL_CONTROL"
)

# Batch marginalization policy for MANUSCRIPT-FACING population predictions.
# Batch is an experimental blocking factor with approximately balanced planned
# n, so the target estimand is "an average experimental batch", not "the average
# animal in the realised cohort". Count weighting is retained as a sensitivity.
# Batch composition is effectively common to all groups within a Sex, so this
# choice cancels in every pairwise contrast and moves only group-level levels.
MMM_BATCH_WEIGHTING <- "equal"

#' Stamp an inference-status flag onto every row of a result table.
#'
#' Used to mark Inactive-phase Gaussian-log1p results as
#' MODEL_INADEQUATE__DO_NOT_INTERPRET so no downstream reader can pick a
#' q-value out of a CSV without the caveat travelling with it.
mmm_stamp_status <- function(x, status, reason = NA_character_) {
  if (is.null(x) || nrow(x) == 0L) return(x)
  x %>% mutate(inference_status = status, inference_status_reason = reason)
}

#' Batch marginalization weights under a named scheme.
mmm_batch_weights <- function(d, scheme = c("equal", "count")) {
  scheme <- match.arg(scheme)
  counts <- table(dplyr::distinct(d, AnimalNum, Batch)$Batch)
  counts <- counts[counts > 0]
  if (scheme == "equal") stats::setNames(rep(1, length(counts)), names(counts)) else counts
}

#' Ordered interaction factor for identifiable difference smooths.
#'
#' @param ... factors to cross. The first level of the crossed factor is the
#'   reference and gets no difference smooth.
mmm_ordered_cross <- function(...) {
  f <- interaction(..., drop = TRUE, sep = ".")
  factor(f, levels = levels(f), ordered = TRUE)
}

#' Lag-1 residual correlation computed strictly within AR1 sequences.
mmm_estimate_rho <- function(model, ar_start) {
  r <- as.numeric(stats::residuals(model, type = "pearson"))
  if (length(r) != length(ar_start)) return(NA_real_)
  # A pair (i-1, i) is usable only when i does not open a new sequence.
  usable <- which(c(FALSE, !ar_start[-1]))
  if (length(usable) < 10L) return(NA_real_)
  stats::cor(r[usable], r[usable - 1L], use = "complete.obs")
}

#' Two-stage AR1 GAMM fit.
#'
#' @param formula Model formula.
#' @param data Data frame that MUST contain the `.ar_start` logical column and
#'   already be ordered by (AnimalNum, block keys, slot).
#' @param use_ar1 Set FALSE to return the stage-1 fit only.
#' @return list(model, model_stage1, rho, ar1_applied, fit_seconds)
mmm_fit_gamm_ar1 <- function(formula, dat, use_ar1 = TRUE, rho_floor = 0.05) {
  stopifnot(".ar_start" %in% names(dat))
  t0 <- Sys.time()
  stage1 <- mgcv::bam(formula, data = dat, method = MMM_GAMM_METHOD, discrete = TRUE)
  rho <- mmm_estimate_rho(stage1, dat$.ar_start)
  applied <- isTRUE(use_ar1) && is.finite(rho) && abs(rho) > rho_floor
  model <- stage1
  if (applied) {
    # AR.start must be a BARE column reference: bam() evaluates it inside the
    # model frame, where a local variable named `data` would resolve to
    # base::data() instead of the argument.
    model <- mgcv::bam(formula, data = dat, method = MMM_GAMM_METHOD, discrete = TRUE,
                       rho = rho, AR.start = .ar_start)
  }
  list(
    model = model,
    model_stage1 = stage1,
    rho = rho,
    ar1_applied = applied,
    fit_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

#' Batch-marginal design rows with the animal random effect removed.
#'
#' Returns ONE design row per row of `grid`, averaged over the batch
#' composition supplied in `batch_weights`. The SAME weights are used for every
#' group, so batch composition can never masquerade as a group difference.
#' Columns belonging to `exclude` smooths are zeroed, which removes the animal
#' random effect from population-level predictions.
mmm_design_rows <- function(model, grid, batch_weights, animal_ref, exclude = "s(AnimalNum)") {
  stopifnot(is.data.frame(grid), nrow(grid) > 0L)
  batches <- names(batch_weights)
  w <- as.numeric(batch_weights) / sum(as.numeric(batch_weights))

  acc <- NULL
  for (i in seq_along(batches)) {
    nd <- grid
    nd$Batch <- factor(batches[i], levels = levels(model$model$Batch) %||% batches)
    nd$AnimalNum <- factor(animal_ref, levels = levels(model$model$AnimalNum))
    X <- mgcv::predict.bam(model, newdata = nd, type = "lpmatrix")
    acc <- if (is.null(acc)) w[i] * X else acc + w[i] * X
  }
  # Zero the excluded smooth blocks (they cancel in contrasts anyway, but must
  # not contribute to a population-level trajectory).
  for (sm in model$smooth) {
    if (sm$label %in% exclude) acc[, sm$first.para:sm$last.para] <- 0
  }
  acc
}

#' Population trajectory with pointwise CI.
mmm_gamm_trajectory <- function(model, grid, batch_weights, animal_ref, level = 0.95) {
  X <- mmm_design_rows(model, grid, batch_weights, animal_ref)
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  b <- stats::coef(model)
  fit <- as.numeric(X %*% b)
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  grid %>% mutate(fit = fit, se = se, lower = fit - z * se, upper = fit + z * se,
                  ci_level = level)
}

#' Pointwise predicted difference between two factor settings.
#'
#' `setting1` / `setting0` are named lists of columns to overwrite in `grid`.
mmm_gamm_pointwise_diff <- function(model, grid, setting1, setting0,
                                    batch_weights, animal_ref, level = 0.95) {
  g1 <- grid; for (nm in names(setting1)) g1[[nm]] <- setting1[[nm]]
  g0 <- grid; for (nm in names(setting0)) g0[[nm]] <- setting0[[nm]]
  X1 <- mmm_design_rows(model, g1, batch_weights, animal_ref)
  X0 <- mmm_design_rows(model, g0, batch_weights, animal_ref)
  Xd <- X1 - X0
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  b <- stats::coef(model)
  diff <- as.numeric(Xd %*% b)
  se <- sqrt(pmax(0, rowSums((Xd %*% V) * Xd)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  grid %>% mutate(diff = diff, se = se, lower = diff - z * se, upper = diff + z * se,
                  ci_level = level)
}

#' Window-averaged predicted contrast (the planned inferential quantity).
#'
#' The average over the prediction grid of the predicted difference, i.e. the
#' mean trajectory separation across the window. Exact: the average of a linear
#' functional is the linear functional of the averaged design row, so this is
#' a single Wald test on cbar' beta with no simulation.
#' Window-averaged contrast between two FULLY BUILT grids.
#'
#' Needed whenever the ordered interaction column varies row by row (e.g.
#' averaging a Group contrast across several Active nights), which a
#' column-overwrite setting cannot express. `g1` and `g0` must have the same
#' number of rows and differ only in the group-identifying columns.
mmm_gamm_avg_contrast_grids <- function(model, g1, g0, batch_weights, animal_ref, level = 0.95) {
  stopifnot(nrow(g1) == nrow(g0), nrow(g1) > 0L)
  X1 <- mmm_design_rows(model, g1, batch_weights, animal_ref)
  X0 <- mmm_design_rows(model, g0, batch_weights, animal_ref)
  cbar <- colMeans(X1 - X0)
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  b <- stats::coef(model)
  est <- sum(cbar * b)
  se <- sqrt(max(0, as.numeric(t(cbar) %*% V %*% cbar)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  stat <- if (se > 0) est / se else NA_real_
  tibble(
    estimate = est, se = se,
    ci_low = est - z * se, ci_high = est + z * se, ci_level = level,
    statistic = stat,
    p_raw = if (is.na(stat)) NA_real_ else 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  )
}

mmm_gamm_avg_contrast <- function(model, grid, setting1, setting0,
                                  batch_weights, animal_ref, level = 0.95) {
  g1 <- grid; for (nm in names(setting1)) g1[[nm]] <- setting1[[nm]]
  g0 <- grid; for (nm in names(setting0)) g0[[nm]] <- setting0[[nm]]
  X1 <- mmm_design_rows(model, g1, batch_weights, animal_ref)
  X0 <- mmm_design_rows(model, g0, batch_weights, animal_ref)
  cbar <- colMeans(X1 - X0)
  V <- mgcv::vcov.gam(model, unconditional = TRUE)
  b <- stats::coef(model)
  est <- sum(cbar * b)
  se <- sqrt(max(0, as.numeric(t(cbar) %*% V %*% cbar)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  stat <- if (se > 0) est / se else NA_real_
  tibble(
    estimate = est, se = se,
    ci_low = est - z * se, ci_high = est + z * se, ci_level = level,
    statistic = stat,
    p_raw = if (is.na(stat)) NA_real_ else 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  )
}

#' The three planned group contrasts, evaluated at one stratum setting.
#'
#' @param stratum_setting Named list fixing the non-Group factors (e.g.
#'   list(ActiveNight = "2")); may be empty for a single-stratum model.
#' @param cross_col Name of the ordered interaction column the by-smooths use,
#'   or NULL when the model has only Group difference smooths.
mmm_gamm_group_contrasts <- function(model, grid, batch_weights, animal_ref,
                                     stratum_setting = list(),
                                     cross_col = NULL,
                                     cross_levels = NULL,
                                     level = 0.95) {
  pairs <- list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES"))
  make_setting <- function(g) {
    s <- stratum_setting
    s$Group <- factor(g, levels = levels(model$model$Group))
    if (!is.null(cross_col)) {
      key <- paste(c(g, unlist(lapply(stratum_setting, as.character))), collapse = ".")
      s[[cross_col]] <- factor(key, levels = cross_levels, ordered = TRUE)
    }
    s
  }
  purrr::map_dfr(pairs, function(pp) {
    mmm_gamm_avg_contrast(model, grid, make_setting(pp[1]), make_setting(pp[2]),
                          batch_weights, animal_ref, level = level) %>%
      mutate(contrast = paste0(pp[1], "-", pp[2]), group_comp = pp[1], group_ref = pp[2],
             .before = 1)
  })
}

#' Attach a BH-adjusted p-value across an explicitly declared family.
#'
#' The family is named in the output so a reader can never mistake which tests
#' were corrected together.
mmm_bh_family <- function(tbl, family_id, p_col = "p_raw") {
  tbl %>%
    mutate(
      family_id = family_id,
      n_tests_in_family = sum(!is.na(.data[[p_col]])),
      p_bh = stats::p.adjust(.data[[p_col]], method = "BH")
    )
}

#' Parametric-term ANOVA table as a tidy tibble.
mmm_gamm_parametric_table <- function(model, label) {
  a <- anova(model)
  pt <- a$pTerms.table
  if (is.null(pt)) return(tibble())
  as.data.frame(pt) %>%
    tibble::rownames_to_column("term") %>%
    as_tibble() %>%
    rename(df = "df", F_stat = "F", p_raw = "p-value") %>%
    mutate(model_label = label, .before = 1)
}

#' Smooth-term table as a tidy tibble.
mmm_gamm_smooth_table <- function(model, label) {
  s <- summary(model)$s.table
  if (is.null(s)) return(tibble())
  as.data.frame(s) %>%
    tibble::rownames_to_column("smooth") %>%
    as_tibble() %>%
    rename(ref_df = "Ref.df", p_raw = "p-value") %>%
    mutate(model_label = label, .before = 1)
}

#' One-row model specification / diagnostics record.
mmm_gamm_spec_row <- function(fit, label, formula, data, response_scale, batch_adjusted, role) {
  m <- fit$model
  sm <- summary(m)
  tibble(
    model_label = label,
    role = role,
    response_scale = response_scale,
    batch_adjusted = batch_adjusted,
    formula = paste(deparse(formula), collapse = " "),
    engine = "mgcv::bam",
    method = MMM_GAMM_METHOD,
    discrete = TRUE,
    k_time = MMM_GAMM_K,
    n_obs = nrow(data),
    n_animals = dplyr::n_distinct(data$AnimalNum),
    n_batches = dplyr::n_distinct(data$Batch),
    rho_estimated = fit$rho,
    ar1_applied = fit$ar1_applied,
    dev_expl = unname(sm$dev.expl),
    r_sq_adj = unname(sm$r.sq),
    scale_est = unname(sm$scale),
    aic = tryCatch(stats::AIC(m), error = function(e) NA_real_),
    edf_total = sum(m$edf),
    fit_seconds = fit$fit_seconds
  )
}

#' Compare a primary model against a sensitivity variant on the planned contrasts.
#'
#' Reports direction agreement and magnitude change, never a significance-based
#' model choice.
mmm_sensitivity_compare <- function(primary, sensitivity, by_cols, label) {
  primary %>%
    select(all_of(by_cols), est_primary = "estimate", lo_primary = "ci_low",
           hi_primary = "ci_high", p_primary = "p_raw") %>%
    inner_join(
      sensitivity %>% select(all_of(by_cols), est_sens = "estimate",
                             lo_sens = "ci_low", hi_sens = "ci_high",
                             p_sens = "p_raw"),
      by = by_cols
    ) %>%
    mutate(
      comparison = label,
      abs_diff = .data$est_sens - .data$est_primary,
      rel_change = ifelse(.data$est_primary == 0, NA_real_,
                          (.data$est_sens - .data$est_primary) / abs(.data$est_primary)),
      same_direction = sign(.data$est_primary) == sign(.data$est_sens),
      primary_ci_contains_sens = .data$est_sens >= .data$lo_primary & .data$est_sens <= .data$hi_primary,
      cis_overlap = pmax(.data$lo_primary, .data$lo_sens) <= pmin(.data$hi_primary, .data$hi_sens)
    )
}
