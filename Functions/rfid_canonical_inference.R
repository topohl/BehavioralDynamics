# ================================================================
# Canonical behavioural inference engine (Exp9 SIS RFID)
# MMMSociability -- Functions/rfid_canonical_inference.R
# ================================================================
# Implements MMM_BEHAVIOR_CONFIG$fitting, $contrasts and $sensitivities exactly:
#   * lmerTest::lmer(REML = TRUE, bobyqa, check.rankX = "stop.deficient"), messages captured,
#   * a pre-fit full-rank assertion on model.matrix(nobars(formula), droplevels(data)),
#   * Kenward-Roger tests via lmerTest::contest(), KR-adjusted covariances via pbkrtest::vcovAdj,
#   * named L-vectors on the explicit numeric coding (order-invariant coefficient matching).
# ================================================================

MMM_CI_Q2B_ROWS <- c("g_RS:c2:sex_c", "g_RS:c3:sex_c", "g_RS:c4:sex_c")
MMM_CI_Q2A_ROWS <- c("g_RS:c2", "g_RS:c3", "g_RS:c4")
MMM_CI_ALPHA <- 0.05

mmm_ci_code_design <- function(d) {
  d <- data.table::copy(d)
  d[, `:=`(sex_c = ifelse(Sex == "Female", 0.5, -0.5),
           g_RS = data.table::fcase(Group == "RES", 0.5, Group == "SUS", -0.5, default = NA_real_),
           g_SIS = ifelse(Group == "CON", -0.5, 0.5),
           cc = as.integer(sub("^CC", "", CC)), Batch = factor(Batch))]
  d[, `:=`(c2 = as.numeric(cc == 2), c3 = as.numeric(cc == 3), c4 = as.numeric(cc == 4), cc1 = cc - 1,
           d1 = as.numeric(cc == 1), d2 = as.numeric(cc == 2), d3 = as.numeric(cc == 3), d4 = as.numeric(cc == 4),
           isCON = as.numeric(Group == "CON"))]
  d[, conCage := as.numeric(all(Group == "CON")), by = CageEpisodeID]
  d[, sisCage := 1 - conCage]
  d[, Session := paste(Batch, CC, sep = ":")]
  d[]
}

# Interaction labels depend on variable order in the formula ("g_RS:c2:sex_c" vs "c2:g_RS:sex_c");
# coefficients are matched on the sorted set of their components.
mmm_ci_canon <- function(x) vapply(strsplit(x, ":", fixed = TRUE), function(v) paste(sort(v), collapse = ":"), "")
mmm_ci_match <- function(wanted, fit_names) {
  i <- match(mmm_ci_canon(wanted), mmm_ci_canon(fit_names))
  if (anyNA(i)) stop("Contrast coefficients not in the model: ", paste(wanted[is.na(i)], collapse = ", "), call. = FALSE)
  fit_names[i]
}
mmm_ci_L <- function(fit_names, weights) {
  weights <- unlist(weights)
  nm <- mmm_ci_match(names(weights), fit_names)
  L <- stats::setNames(rep(0, length(fit_names)), fit_names); L[nm] <- weights; L
}

#' Fit with message capture, a pre-fit rank assertion and (optionally) the frozen expected rank.
mmm_ci_fit <- function(formula, data, label, engine = c("lmer", "lm", "glmmTMB"), dispformula = NULL, expected_rank = NULL) {
  engine <- match.arg(engine)
  data <- droplevels(data.table::as.data.table(data))
  f <- stats::as.formula(formula)
  fixed_f <- if (engine == "lm") f else stats::as.formula(paste(deparse(f[[2]]), "~", paste(deparse(reformulas::nobars(f)[[3]]), collapse = " ")))
  X0 <- stats::model.matrix(fixed_f, data)
  rank0 <- qr(X0)$rank
  if (rank0 < ncol(X0)) stop("Rank-deficient design in ", label, ": rank ", rank0, " of ", ncol(X0), call. = FALSE)
  if (!is.null(expected_rank) && ncol(X0) != expected_rank)
    stop("Design of ", label, " has ", ncol(X0), " columns; frozen expected rank is ", expected_rank, call. = FALSE)
  msgs <- character()
  handler_w <- function(w) { msgs <<- c(msgs, paste("warning:", conditionMessage(w))); invokeRestart("muffleWarning") }
  handler_m <- function(m) { msgs <<- c(msgs, paste("message:", trimws(conditionMessage(m)))); invokeRestart("muffleMessage") }
  # fitting$failure_rule: an erroring fit becomes an explicit FAILED model (fit = NULL, error kept in the registry);
  # every downstream helper returns FAILED rows for it and the run continues, so no failure disappears silently.
  fit <- tryCatch(withCallingHandlers({
    switch(engine,
           lmer = lmerTest::lmer(f, data = data, REML = TRUE,
                                 control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5), check.rankX = "stop.deficient")),
           lm = stats::lm(f, data = data),
           glmmTMB = glmmTMB::glmmTMB(f, dispformula = if (is.null(dispformula)) ~1 else stats::as.formula(dispformula),
                                      data = data, REML = TRUE))
  }, warning = handler_w, message = handler_m), error = function(e) e)
  fit_error <- if (inherits(fit, "error")) conditionMessage(fit) else NA_character_
  if (!is.na(fit_error)) fit <- NULL
  if (!is.null(fit) && engine == "lmer" && length(attr(lme4::getME(fit, "X"), "col.dropped"))) stop("lme4 dropped columns in ", label, call. = FALSE)
  singular <- if (!is.null(fit) && engine == "lmer") lme4::isSingular(fit) else NA
  # convergence-class conditions: lme4 convergence checks (incl. "nearly unidentifiable") and glmmTMB optimiser / Hessian problems
  pd_hess <- if (!is.null(fit) && engine == "glmmTMB") isTRUE(fit$sdr$pdHess) else NA
  converged <- is.na(fit_error) && !any(grepl("converge|unidentifiable|non-positive-definite", msgs, ignore.case = TRUE)) && !isFALSE(pd_hess)
  info <- data.table::data.table(model_id = label, engine = engine, formula = paste(deparse(f, width.cutoff = 500L), collapse = " "),
    dispformula = if (is.null(dispformula)) NA_character_ else dispformula,
    n_obs = nrow(data), n_animals = data.table::uniqueN(data$AnimalNum),
    n_cage_episodes = if ("CageEpisodeID" %in% names(data)) data.table::uniqueN(data$CageEpisodeID) else NA_integer_,
    n_batches = data.table::uniqueN(data$Batch), n_fixed_cols = ncol(X0), rank = rank0, expected_rank = expected_rank %||% NA_integer_,
    singular = singular, converged = converged,
    messages = paste(unique(msgs), collapse = " | "))
  # a persistent convergence problem fails the fit unless the converging optimizers agree (lmer: five lme4 optimizers;
  # glmmTMB: nlminb vs optim BFGS); an error always fails it
  out <- list(fit = fit, info = info, data = data)
  oc <- if (!is.null(fit) && !converged && engine == "lmer") mmm_ci_optimizer_check(out) else
        if (!is.null(fit) && !converged && engine == "glmmTMB") mmm_ci_optimizer_check_glmmtmb(out) else NULL
  out$info[, `:=`(optimizer_check_agree = if (is.null(oc)) NA else oc$agree,
                  failed = !is.na(fit_error) || (!is.null(oc) && !isTRUE(oc$agree)),
                  optimizer_check_messages = if (is.null(oc)) NA_character_ else oc$messages,
                  error = fit_error)]
  out
}

mmm_ci_fit_failed <- function(m) isTRUE(m$info$failed) || is.null(m$fit)

#' fitting$failure_rule: rows from a FAILED fit carry no estimate or p.
mmm_ci_mark_failed <- function(x, m) {
  failed <- mmm_ci_fit_failed(m)
  if (failed) for (cl in intersect(c("estimate", "se", "df", "statistic", "ci_low", "ci_high", "p_raw", "F", "df1", "df2"), names(x))) data.table::set(x, j = cl, value = NA_real_)
  x[, status := if (failed) "FAILED" else "OK"][]
}

#' FAILED placeholder row (same columns as the successful row) for a model whose fit errored.
mmm_ci_failed_row <- function(m, estimand, type = c("contrast", "joint"), weights = NULL, rows = NULL) {
  type <- match.arg(type)
  x <- if (type == "contrast") { w <- unlist(weights)
    data.table::data.table(model_id = m$info$model_id, estimand = estimand, L = paste(sprintf("%s=%g", names(w), w), collapse = "; "),
      estimate = NA_real_, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_raw = NA_real_,
      test = "KR t", df_method = "Kenward-Roger")
  } else data.table::data.table(model_id = m$info$model_id, estimand = estimand, rows = paste(rows, collapse = "; "),
      F = NA_real_, df1 = NA_real_, df2 = NA_real_, p_raw = NA_real_, test = "KR joint F", df_method = "Kenward-Roger")
  x[, status := "FAILED"][]
}

#' One-row KR contrast.
mmm_ci_contrast <- function(m, weights, estimand, level = 0.95) {
  if (is.null(m$fit)) return(mmm_ci_failed_row(m, estimand, "contrast", weights = weights))
  fit <- m$fit; b <- lme4::fixef(fit)
  L <- mmm_ci_L(names(b), weights)
  ct <- lmerTest::contest(fit, L, joint = FALSE, ddf = "Kenward-Roger", confint = TRUE, level = level)
  w <- unlist(weights)
  data.table::data.table(model_id = m$info$model_id, estimand = estimand,
    L = paste(sprintf("%s=%g", names(w), w), collapse = "; "),
    estimate = ct$Estimate, se = ct$`Std. Error`, df = ct$df, statistic = ct$`t value`,
    ci_low = ct$lower, ci_high = ct$upper, p_raw = ct$`Pr(>|t|)`, test = "KR t", df_method = "Kenward-Roger") |> mmm_ci_mark_failed(m)
}

#' Joint KR F test of several rows (each row a single coefficient).
mmm_ci_joint <- function(m, rows, estimand) {
  if (is.null(m$fit)) return(mmm_ci_failed_row(m, estimand, "joint", rows = rows))
  fit <- m$fit; b <- lme4::fixef(fit)
  L <- do.call(rbind, lapply(rows, function(r) mmm_ci_L(names(b), stats::setNames(1, r))))
  ct <- lmerTest::contest(fit, L, joint = TRUE, ddf = "Kenward-Roger")
  data.table::data.table(model_id = m$info$model_id, estimand = estimand, rows = paste(rows, collapse = "; "),
    F = ct$`F value`, df1 = ct$NumDF, df2 = ct$DenDF, p_raw = ct$`Pr(>F)`, test = "KR joint F", df_method = "Kenward-Roger") |> mmm_ci_mark_failed(m)
}

mmm_ci_holm <- function(p) stats::p.adjust(p, method = "holm", n = length(p))   # declared m kept when a member FAILED (NA)

#' Optimizer check (config fitting$optimizer_check): refit with lme4's built-in optimizers (the allFit set without optional
#' packages) directly from the stored formula and data; lme4::allFit's update() cannot re-evaluate these calls.
mmm_ci_optimizer_check <- function(m) {
  if (is.null(m$fit)) return(data.table::data.table(n_ok = 0L, n_opt = 0L, agree = NA, messages = "fit FAILED; no optimizer check"))
  opts <- list(bobyqa = list("bobyqa", list()), Nelder_Mead = list("Nelder_Mead", list()), nlminbwrap = list("nlminbwrap", list()),
               nloptwrap.NLOPT_LN_NELDERMEAD = list("nloptwrap", list(algorithm = "NLOPT_LN_NELDERMEAD")),
               nloptwrap.NLOPT_LN_BOBYQA = list("nloptwrap", list(algorithm = "NLOPT_LN_BOBYQA")))
  f <- stats::as.formula(m$info$formula)
  res <- lapply(names(opts), function(o) { msgs <- character()
    fit <- tryCatch(withCallingHandlers(
      lme4::lmer(f, data = m$data, REML = TRUE, control = lme4::lmerControl(optimizer = opts[[o]][[1]], optCtrl = opts[[o]][[2]])),
      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") },
      message = function(x) { msgs <<- c(msgs, trimws(conditionMessage(x))); invokeRestart("muffleMessage") }), error = function(e) e)
    if (inherits(fit, "error")) return(list(optimizer = o, ok = FALSE, msgs = conditionMessage(fit)))
    conv <- c(msgs, fit@optinfo$conv$lme4$messages)
    list(optimizer = o, ok = !any(grepl("converge", conv, ignore.case = TRUE)), loglik = as.numeric(stats::logLik(fit)),
         fixef = lme4::fixef(fit), msgs = paste(unique(conv), collapse = " | ")) })
  ok <- vapply(res, `[[`, TRUE, "ok")
  msgs <- paste(sprintf("%s: %s", vapply(res, `[[`, "", "optimizer"), vapply(res, function(r) r$msgs %||% "", "")), collapse = " || ")
  if (sum(ok) < 2) return(data.table::data.table(n_ok = sum(ok), n_opt = length(ok), agree = NA, messages = msgs))
  ll <- vapply(res[ok], `[[`, 0, "loglik"); fx <- do.call(rbind, lapply(res[ok], `[[`, "fixef"))
  rg <- apply(fx, 2, function(v) max(v) - min(v))
  rel <- max(rg / pmax(abs(colMeans(fx)), 1e-8))
  se_scaled <- max(rg / sqrt(diag(as.matrix(stats::vcov(m$fit))))[colnames(fx)])
  data.table::data.table(n_ok = sum(ok), n_opt = length(ok), optimizers_ok = paste(vapply(res[ok], `[[`, "", "optimizer"), collapse = "; "),
    loglik_range = diff(range(ll)), max_fixef_range_over_se = se_scaled, max_rel_fixef_diff = rel,
    agree = diff(range(ll)) < 1e-6 & se_scaled < 0.01, messages = msgs)
}

#' glmmTMB analogue of the optimizer check: refit with optim(BFGS) and compare with the default nlminb fit
#' (same agreement rule: logLik range < 1e-6 and every conditional fixed effect within 0.01 SE). Runs only for a
#' glmmTMB fit with a convergence-class problem.
mmm_ci_optimizer_check_glmmtmb <- function(m) {
  f <- stats::as.formula(m$info$formula)
  disp <- if (is.na(m$info$dispformula)) ~1 else stats::as.formula(m$info$dispformula)
  alt <- tryCatch(suppressWarnings(glmmTMB::glmmTMB(f, dispformula = disp, data = m$data, REML = TRUE,
                    control = glmmTMB::glmmTMBControl(optimizer = stats::optim, optArgs = list(method = "BFGS")))), error = function(e) e)
  if (inherits(alt, "error")) return(data.table::data.table(n_ok = 1L, n_opt = 2L, agree = NA, messages = paste("optim BFGS refit failed:", conditionMessage(alt))))
  b0 <- glmmTMB::fixef(m$fit)$cond; b1 <- glmmTMB::fixef(alt)$cond
  se <- sqrt(diag(as.matrix(stats::vcov(m$fit)$cond)))
  ll <- c(as.numeric(stats::logLik(m$fit)), as.numeric(stats::logLik(alt)))
  sc <- max(abs(b1[names(b0)] - b0) / se)
  data.table::data.table(n_ok = 2L, n_opt = 2L, optimizers_ok = "nlminb; optim BFGS", loglik_range = diff(range(ll)),
    max_fixef_range_over_se = sc, max_rel_fixef_diff = NA_real_, agree = isTRUE(diff(range(ll)) < 1e-6 && sc < 0.01),
    messages = paste("pdHess nlminb:", isTRUE(m$fit$sdr$pdHess), "| BFGS:", isTRUE(alt$sdr$pdHess)))
}

#' KR-adjusted covariance of selected coefficients.
mmm_ci_vcov_kr <- function(m, rows) {
  if (is.null(m$fit)) return(matrix(NA_real_, length(rows), length(rows), dimnames = list(rows, rows)))
  V <- as.matrix(pbkrtest::vcovAdj(methods::as(m$fit, "lmerMod")))
  nm <- names(lme4::fixef(m$fit)); dimnames(V) <- list(nm, nm)
  r <- mmm_ci_match(rows, nm); V[r, r, drop = FALSE]
}

# ---------------------------------------------------------------- heteroscedastic comparators
#' A for Q1: difference of stratified estimates (all variance components sex-specific).
mmm_ci_het_A_q1 <- function(rs_f, rs_m) {
  d <- rs_f$estimate - rs_m$estimate; se <- sqrt(rs_f$se^2 + rs_m$se^2)
  df <- (rs_f$se^2 + rs_m$se^2)^2 / (rs_f$se^4 / rs_f$df + rs_m$se^4 / rs_m$df)
  data.table::data.table(estimate = d, se = se, df = df, statistic = d / se,
    ci_low = d - stats::qt(0.975, df) * se, ci_high = d + stats::qt(0.975, df) * se,
    p_raw = 2 * stats::pt(-abs(d / se), df), test = "stratified difference (KR SEs), Welch-Satterthwaite df")
}

#' A for Q2b: 3-df Wald on b_F - b_M of g_RS:ck with KR-adjusted V_F + V_M, F(3, nu); plus per-component differences.
mmm_ci_het_A_q2b <- function(m_f, m_m, nu) {
  if (is.null(m_f$fit) || is.null(m_m$fit))
    return(list(joint = data.table::data.table(F = NA_real_, df1 = 3, df2 = nu, p_raw = NA_real_, test = "stratified-difference Wald F(3, nu); a stratum fit FAILED", status = "FAILED"),
                components = data.table::data.table(component = MMM_CI_Q2B_ROWS, estimate = NA_real_, se = NA_real_)))
  rF <- mmm_ci_match(MMM_CI_Q2A_ROWS, names(lme4::fixef(m_f$fit))); rM <- mmm_ci_match(MMM_CI_Q2A_ROWS, names(lme4::fixef(m_m$fit)))
  bF <- lme4::fixef(m_f$fit)[rF]; bM <- lme4::fixef(m_m$fit)[rM]
  VF <- mmm_ci_vcov_kr(m_f, MMM_CI_Q2A_ROWS); VM <- mmm_ci_vcov_kr(m_m, MMM_CI_Q2A_ROWS)
  dd <- unname(bF - bM); V <- VF + VM; W <- as.numeric(t(dd) %*% solve(V) %*% dd)
  list(joint = data.table::data.table(F = W / 3, df1 = 3, df2 = nu, p_raw = stats::pf(W / 3, 3, nu, lower.tail = FALSE),
                                      test = "stratified-difference Wald F(3, nu) with KR covariances; nu = smaller stratum KR df"),
       components = data.table::data.table(component = MMM_CI_Q2B_ROWS, estimate = dd, se = sqrt(diag(V))))
}

#' B: glmmTMB with dispformula ~ sex_c; Wald z (one row / components) or chi-square (joint).
mmm_ci_glmmtmb_tests <- function(g, one = NULL, rows = NULL) {
  if (mmm_ci_fit_failed(g)) {
    out <- list()
    if (!is.null(one)) out$one <- data.table::data.table(estimate = NA_real_, se = NA_real_, df = Inf, statistic = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                                                     p_raw = NA_real_, test = "glmmTMB fit FAILED", status = "FAILED")
    if (!is.null(rows)) { out$joint <- data.table::data.table(F = NA_real_, df1 = length(rows), df2 = Inf, p_raw = NA_real_, test = "glmmTMB fit FAILED", status = "FAILED")
      out$components <- data.table::data.table(component = rows, estimate = NA_real_, se = NA_real_) }
    return(out)
  }
  b <- glmmTMB::fixef(g$fit)$cond; V <- as.matrix(stats::vcov(g$fit)$cond)
  out <- list()
  if (!is.null(one)) { L <- mmm_ci_L(names(b), one); e <- sum(L * b); se <- sqrt(as.numeric(t(L) %*% V %*% L))
    out$one <- data.table::data.table(estimate = e, se = se, df = Inf, statistic = e / se, ci_low = e - stats::qnorm(0.975) * se,
      ci_high = e + stats::qnorm(0.975) * se, p_raw = 2 * stats::pnorm(-abs(e / se)), test = "glmmTMB REML, dispformula ~ sex_c, Wald z") }
  if (!is.null(rows)) { nm <- mmm_ci_match(rows, names(b)); idx <- match(nm, names(b)); bb <- b[idx]; VV <- V[idx, idx]
    W <- as.numeric(t(bb) %*% solve(VV) %*% bb)
    out$joint <- data.table::data.table(F = W / length(rows), df1 = length(rows), df2 = Inf,
      p_raw = stats::pchisq(W, length(rows), lower.tail = FALSE), test = "glmmTMB REML, dispformula ~ sex_c, Wald chi-square")
    out$components <- data.table::data.table(component = rows, estimate = unname(bb), se = sqrt(diag(VV))) }
  out
}

#' Shift rule (config: material_dependence$shift and the robustness label SIGN_CHANGE floor).
mmm_ci_shift_material <- function(prim_est, prim_se, alt_est) {
  shift <- (alt_est - prim_est) / prim_se
  abs(shift) >= 0.5 | (sign(alt_est) != sign(prim_est) & abs(alt_est) >= 0.5 * prim_se & abs(prim_est) >= 0.5 * prim_se)
}

# ---------------------------------------------------------------- shared-zone D1 / D2
#' D1 at CC1: CR2 cluster-robust by cage epoch on the lmer working model, Satterthwaite df (clubSandwich).
mmm_ci_cr2 <- function(m, coef) {
  if (mmm_ci_fit_failed(m)) return(data.table::data.table(estimate = NA_real_, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = NA_real_,
    ci_high = NA_real_, p_raw = NA_real_, n_clusters = data.table::uniqueN(m$data$CageEpisodeID), test = "working-model fit FAILED", status = "FAILED"))
  fit <- methods::as(m$fit, "lmerMod")
  V <- clubSandwich::vcovCR(fit, cluster = m$data$CageEpisodeID, type = "CR2")
  ct <- clubSandwich::coef_test(fit, vcov = V, test = "Satterthwaite", coefs = coef)
  e <- ct$beta; se <- ct$SE; df <- ct$df_Satt
  data.table::data.table(estimate = e, se = se, df = df, statistic = e / se, ci_low = e - stats::qt(0.975, df) * se,
    ci_high = e + stats::qt(0.975, df) * se, p_raw = ct$p_Satt, n_clusters = data.table::uniqueN(m$data$CageEpisodeID),
    test = "CR2 by cage epoch on the lmer working model, Satterthwaite df")
}

#' D1 longitudinal: OLS fixed part with two-way (animal, cage epoch) cluster-robust SEs; Wald F for Q2b.
mmm_ci_twoway_q2b <- function(formula_fixed, data) {
  fit <- tryCatch(stats::lm(stats::as.formula(formula_fixed), data = data), error = function(e) e)
  if (inherits(fit, "error")) return(data.table::data.table(F = NA_real_, df1 = 3, df2 = NA_real_, p_raw = NA_real_,
    test = paste("OLS two-way fit FAILED:", conditionMessage(fit)), status = "FAILED"))
  V <- sandwich::vcovCL(fit, cluster = ~ AnimalNum + CageEpisodeID, type = "HC3", cadjust = TRUE, multi0 = FALSE)
  ev <- eigen(V, symmetric = TRUE); if (any(ev$values < 0)) V <- ev$vectors %*% diag(pmax(ev$values, 0)) %*% t(ev$vectors)
  dimnames(V) <- list(names(stats::coef(fit)), names(stats::coef(fit)))
  rn <- mmm_ci_match(MMM_CI_Q2B_ROWS, names(stats::coef(fit)))
  b <- stats::coef(fit)[rn]; VV <- V[rn, rn]
  W <- as.numeric(t(b) %*% solve(VV) %*% b); G <- min(data.table::uniqueN(data$AnimalNum), data.table::uniqueN(data$CageEpisodeID))
  data.table::data.table(F = W / 3, df1 = 3, df2 = G - 1, p_raw = stats::pf(W / 3, 3, G - 1, lower.tail = FALSE),
    test = "OLS, two-way cluster-robust (animal, cage epoch; HC3, cadjust), Wald F(3, G-1)")
}

#' D2: within-cage dyadic fixed-effect models; coefficients reported per unit s_RS (beta) and as 2 x beta (RR vs SS).
mmm_ci_d2 <- function(dy, longitudinal = FALSE) {
  dy <- data.table::copy(dy)
  f <- if (!longitudinal) frac ~ 0 + factor(CageEpisodeID) + s_RS + s_RS:sex_c else
    frac ~ 0 + factor(CageEpisodeID) + s_RS + s_RS:(c2 + c3 + c4) + s_RS:sex_c + s_RS:(c2 + c3 + c4):sex_c
  fit <- stats::lm(f, data = dy); X <- stats::model.matrix(fit)
  if (qr(X)$rank < ncol(X)) stop("Rank-deficient dyadic design", call. = FALSE)
  # CR2 by cage epoch (config D2$se): the dyad-robust meat is anti-conservative with within-cage fixed effects
  # (null calibration: evidence/implementation_2026-09-27/d2_calibration).
  V <- clubSandwich::vcovCR(fit, cluster = dy$CageEpisodeID, type = "CR2"); G <- length(unique(c(dy$A, dy$B)))
  one <- function(cf, lab) { cn <- mmm_ci_match(cf, names(stats::coef(fit)))
    ct <- clubSandwich::coef_test(fit, vcov = V, test = "Satterthwaite", coefs = cn); e <- ct$beta; se <- ct$SE; df <- ct$df_Satt
    data.table::data.table(estimand = lab, coef = cf, beta = e, beta_se = se, estimate_RR_vs_SS = 2 * e, se_RR_vs_SS = 2 * se, df = df,
      ci_low = 2 * (e - stats::qt(.975, df) * se), ci_high = 2 * (e + stats::qt(.975, df) * se), p_raw = NA_real_) }
  if (!longitudinal) {
    res <- data.table::rbindlist(list(one("s_RS", "RR_vs_SS_sexavg_CC1"), one("s_RS:sex_c", "DiD_RR_vs_SS_CC1")))
  } else {
    rows <- c("s_RS:c2:sex_c", "s_RS:c3:sex_c", "s_RS:c4:sex_c")
    comp <- data.table::rbindlist(lapply(rows, function(r) one(r, paste0("Q2b_dyad_component_", sub("s_RS:(c[0-9]):sex_c", "\\1", r)))))
    rn <- mmm_ci_match(rows, names(stats::coef(fit)))
    wt <- clubSandwich::Wald_test(fit, constraints = clubSandwich::constrain_zero(rn), vcov = V, test = "HTZ")
    res <- data.table::rbindlist(list(comp, data.table::data.table(estimand = "Q2b_dyad_joint", coef = paste(rows, collapse = "; "),
      F = wt$Fstat, df1 = wt$df_num, df2 = wt$df_denom, p_raw = NA_real_)), fill = TRUE)   # p withheld: sensitivities$p_value_policy (no p for D2)
  }
  res[, `:=`(n_dyads = nrow(dy), n_animals = G, n_cages = data.table::uniqueN(dy$CageEpisodeID),
             test = "within-cage dyadic FE (unweighted), CR2 by cage epoch, Satterthwaite t / HTZ Wald F")]
  res[]
}

#' Robustness label (config: sensitivities$robustness_label_rule; first match wins).
mmm_ci_robust_label <- function(prim_est, prim_se, alt_est) {
  shift <- (alt_est - prim_est) / prim_se
  data.table::fcase(sign(alt_est) != sign(prim_est) & abs(alt_est) >= 0.5 * prim_se & abs(prim_est) >= 0.5 * prim_se, "SIGN_CHANGE",
                    abs(shift) >= 1, "LARGE_SHIFT", default = "COMPATIBLE")
}
