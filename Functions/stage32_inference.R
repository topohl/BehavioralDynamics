# ================================================================
# Stage 32: mixed-model modules A-E, sensitivities, Holm (Exp9 SIS RFID)
# MMMSociability -- Functions/stage32_inference.R
# ================================================================
# Pure constants and functions for Analysis/32_behavior_exposure_adaptation.R, implementing sections 3, 4, 6 and 7 of the
# FROZEN registry docs/STAGE32_REGISTRY_v1.0.md (sha256 cfbd954c..., commit 0970d32). Sourcing this file reads and writes
# nothing. Tier: POST HOC relative to the original experiment (decision basis POST_HOC_CONTEXT), registered before fitting.
#
# Engine: the frozen Stage 29 engine Functions/rfid_canonical_inference.R, unchanged (mmm_ci_code_design, mmm_ci_fit,
# mmm_ci_contrast, mmm_ci_joint, mmm_ci_holm, mmm_ci_L): lmerTest REML, bobyqa, Kenward-Roger through contest.
#   * registry section 3 failure rule: a fit error (including a rank / expected-rank stop) or non-convergence (the engine's
#     converged flag FALSE, i.e. any lme4 convergence-class message; the optimizer-check verdict is recorded, as in 29b) makes
#     the model FAILED: its rows carry no estimate; a Holm family keeps its declared m.
#   * declared KR fallback: if Kenward-Roger fails numerically for a contrast (lmerTest's "Unable to compute Kenward-Roger"
#     warning, an error, or a non-finite KR SE / df), the row is recomputed with Satterthwaite df through contest and flagged
#     ddf_fallback = TRUE (df_method "Satterthwaite"); if that also fails the row is FAILED.
#   * singular fits are kept and flagged.
# Coding (registry section 3): g_SIS = +1/2 SIS, -1/2 CON; sex_c = +1/2 F, -1/2 M; Batch treatment-coded; CC enters as the
# c2, c3, c4 treatment dummies (CC1 reference; identical design to a CC factor); phase = 0..3 for A1..A4 (light models: light
# index 0..2 for L1..L3); phase_f = factor(phase); conCage / sisCage per CageEpisodeID; isCON per animal;
# group3 = factor(CON [reference], RES, SUS); CombZ_wb = CombZ minus its batch mean over the 87 SIS animals.
# Estimation-only rows (registry section 6) carry no statistic and no p.
# Requires: data.table, lme4, lmerTest, pbkrtest; Functions/rfid_canonical_inference.R sourced first.
# ================================================================

S32I_PRIMARY_METRICS <- c("crossing_rate", "shared_zone_use")
S32I_SECONDARY_METRICS <- c("occupancy_dispersion", "fragmentation")
S32I_METRIC_LABEL <- c(crossing_rate = "position-change rate", shared_zone_use = "social-spatial overlap",
                       occupancy_dispersion = "occupancy dispersion", fragmentation = "fragmentation",
                       light_crossing_rate = "light-phase position-change rate")
S32I_KR_FALLBACK_PATTERN <- "Unable to compute Kenward-Roger"
S32I_CAGE2 <- "(0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID)"
S32I_RE_C_EXP <- paste("(1 + phase || AnimalNum) +", S32I_CAGE2, "+ (0 + phase | CageEpisodeID)")
S32I_RE_C_CZ <- "(1 + phase || AnimalNum) + (1 + phase || CageEpisodeID)"
S32I_FORMULAS <- list(
  A_EXP = paste("y ~ Batch + g_SIS + g_SIS:sex_c +", S32I_CAGE2),
  A_EXP_COMMON = "y ~ Batch + g_SIS + g_SIS:sex_c + (1 | CageEpisodeID)",
  A_DEC = paste("y ~ Batch + group3RES + group3SUS + group3RES:sex_c + group3SUS:sex_c +", S32I_CAGE2),
  A_CZ = "y ~ Batch + CombZ_wb + CombZ_wb:sex_c + (1 | CageEpisodeID)",
  B_EXP = paste("y ~ Batch + g_SIS + c2 + c3 + c4 + g_SIS:(c2 + c3 + c4) + g_SIS:sex_c + (c2 + c3 + c4):sex_c +",
                "g_SIS:(c2 + c3 + c4):sex_c + (1 | AnimalNum) +", S32I_CAGE2, "+ (0 + isCON | Batch)"),
  C_EXP = paste("y ~ Batch + c2 + c3 + c4 + g_SIS + phase + g_SIS:phase + g_SIS:(c2 + c3 + c4) +", S32I_RE_C_EXP),
  C_CZ = paste("y ~ Batch + c2 + c3 + c4 + CombZ_wb + phase + CombZ_wb:phase + CombZ_wb:(c2 + c3 + c4) +", S32I_RE_C_CZ),
  C_EXP_PHASExCC = paste("y ~ Batch + c2 + c3 + c4 + g_SIS + phase + g_SIS:phase + g_SIS:(c2 + c3 + c4) + phase:(c2 + c3 + c4) +",
                         "g_SIS:phase:(c2 + c3 + c4) +", S32I_RE_C_EXP),
  C_EXP_PHASExSEX = paste("y ~ Batch + c2 + c3 + c4 + g_SIS + phase + g_SIS:phase + g_SIS:(c2 + c3 + c4) + g_SIS:phase:sex_c +", S32I_RE_C_EXP),
  C_CZ_PHASExCC = paste("y ~ Batch + c2 + c3 + c4 + CombZ_wb + phase + CombZ_wb:phase + CombZ_wb:(c2 + c3 + c4) + phase:(c2 + c3 + c4) +",
                        "CombZ_wb:phase:(c2 + c3 + c4) +", S32I_RE_C_CZ),
  C_CZ_PHASExSEX = paste("y ~ Batch + c2 + c3 + c4 + CombZ_wb + phase + CombZ_wb:phase + CombZ_wb:(c2 + c3 + c4) + CombZ_wb:phase:sex_c +", S32I_RE_C_CZ),
  C_EXP_PHASEF = paste("y ~ Batch + c2 + c3 + c4 + g_SIS + phase_f + g_SIS:phase_f + g_SIS:(c2 + c3 + c4) +", S32I_RE_C_EXP),
  C_CZ_PHASEF = paste("y ~ Batch + c2 + c3 + c4 + CombZ_wb + phase_f + CombZ_wb:phase_f + CombZ_wb:(c2 + c3 + c4) +", S32I_RE_C_CZ),
  D_EXP = paste("y ~ Batch + c2 + c3 + g_SIS + phase_f + g_SIS:phase_f + g_SIS:(c2 + c3) +", S32I_RE_C_EXP),
  D_CZ = paste("y ~ Batch + c2 + c3 + CombZ_wb + phase_f + CombZ_wb:phase_f + CombZ_wb:(c2 + c3) +", S32I_RE_C_CZ))
S32I_FORMULAS$B_EXP_RATE <- sub("(1 | AnimalNum)", "(1 + cc1 || AnimalNum)", S32I_FORMULAS$B_EXP, fixed = TRUE)
S32I_FORMULAS$B_EXP_COMMON <- sub(S32I_CAGE2, "(1 | CageEpisodeID)", S32I_FORMULAS$B_EXP, fixed = TRUE)
S32I_FORMULAS$B_EXP_RATE_COMMON <- sub(S32I_CAGE2, "(1 | CageEpisodeID)", S32I_FORMULAS$B_EXP_RATE, fixed = TRUE)
S32I_FORMULAS$C_EXP_ANIMALCC <- paste(S32I_FORMULAS$C_EXP, "+ (1 | AnimalNum:CC)")
S32I_FORMULAS$C_CZ_ANIMALCC <- paste(S32I_FORMULAS$C_CZ, "+ (1 | AnimalNum:CC)")
S32I_FORMULAS$C_EXP_SEPSLOPE <- sub("(0 + phase | CageEpisodeID)", "(0 + conCage:phase | CageEpisodeID) + (0 + sisCage:phase | CageEpisodeID)",
                                    S32I_FORMULAS$C_EXP, fixed = TRUE)
S32I_RANK <- c(A_EXP = 8L, A_EXP_COMMON = 8L, A_DEC = 10L, A_CZ = 8L, B_EXP = 20L, B_EXP_RATE = 20L, B_EXP_COMMON = 20L, B_EXP_RATE_COMMON = 20L,
               C_EXP = 15L, C_CZ = 15L, C_EXP_PHASExCC = 21L, C_EXP_PHASExSEX = 16L, C_CZ_PHASExCC = 21L, C_CZ_PHASExSEX = 16L,
               C_EXP_PHASEF = 19L, C_CZ_PHASEF = 19L, D_EXP = 17L, D_CZ = 17L, C_EXP_ANIMALCC = 15L, C_CZ_ANIMALCC = 15L, C_EXP_SEPSLOPE = 15L)
S32I_REPRO_TOL <- 1e-6
S32I_REPRO_COLS <- c("estimate", "se", "df", "ci_low", "ci_high")
S32I_CAVEAT_CON <- paste("CON = 6 intact groups (3 per sex, one per batch); KR df small; Exposure x Sex is batch-confounded (sex nested in batch);",
                         "SIS - CON confounds social instability with regrouping and the CC1 platform")
S32I_CAVEAT_CZ <- "direction: behaviour ~ CombZ_wb (within SIS; CombZ centred within batch)"

# ---------------------------------------------------------------- design
#' Coded design table (all animals and phases first, so that conCage / sisCage use every animal of a cage episode).
#' Needs AnimalNum, Sex, Group, CC, Batch, CageEpisodeID, phase (numeric), CombZ_wb (NA for CON).
s32i_code <- function(d) {
  d <- mmm_ci_code_design(data.table::as.data.table(d))
  d[, group3 := factor(Group, levels = c("CON", "RES", "SUS"))]
  # group3 treatment-coded against CON as explicit columns: R codes a factor:covariate term without the covariate main effect
  # by all-level indicators (group3CON:sex_c added), which Batch makes rank-deficient because sex is nested in batch
  d[, `:=`(group3RES = as.numeric(Group == "RES"), group3SUS = as.numeric(Group == "SUS"))]
  d[, phase_f := factor(phase, levels = 0:3)]
  d[]
}

#' Analysis rows for one metric: finite outcome (and finite CombZ_wb when needed), y = the metric; ordered as Stage 29
#' (AnimalNum, CC, then phase) so that the A1 fits reproduce the frozen Stage 29 exposure fits.
s32i_rows <- function(d, metric, need_cz = FALSE) {
  x <- data.table::copy(d[is.finite(get(metric))])
  if (need_cz) x <- x[is.finite(CombZ_wb)]
  x[, y := get(metric)]
  data.table::setorderv(x, c("AnimalNum", "CC", "phase"))
  x[, Batch := droplevels(factor(Batch))][]
}

#' Batch-balanced weights: 1/n_batch on each non-reference Batch dummy (the mean of the batch-specific intercepts).
s32i_batch_weights <- function(batch) {
  bl <- sort(unique(as.character(batch))); if (length(bl) < 2L) return(list())
  as.list(stats::setNames(rep(1 / length(bl), length(bl) - 1L), paste0("Batch", bl[-1])))
}

# ---------------------------------------------------------------- fit and contrasts (frozen engine)
#' Fit with the frozen engine; any stop becomes a FAILED stub; non-convergence FAILS the model (registry section 3).
s32i_fit <- function(formula, data, model_id, expected_rank) {
  m <- tryCatch(mmm_ci_fit(formula, data, model_id, expected_rank = expected_rank), error = function(e) e)
  if (inherits(m, "error")) {
    info <- data.table::data.table(model_id = model_id, engine = "lmer", formula = formula, dispformula = NA_character_, n_obs = nrow(data),
      n_animals = data.table::uniqueN(data$AnimalNum), n_cage_episodes = data.table::uniqueN(data$CageEpisodeID), n_batches = data.table::uniqueN(data$Batch),
      n_fixed_cols = NA_integer_, rank = NA_integer_, expected_rank = expected_rank, singular = NA, converged = FALSE, messages = "",
      optimizer_check_agree = NA, failed = TRUE, optimizer_check_messages = NA_character_, error = conditionMessage(m))
    return(list(fit = NULL, info = info, data = data, failure_reason = paste("fit stopped:", conditionMessage(m))))
  }
  reason <- NA_character_
  if (isTRUE(m$info$failed)) reason <- if (!is.na(m$info$error)) paste("fit error:", m$info$error) else "engine failure rule: non-converged and optimizers disagree"
  else if (!isTRUE(m$info$converged)) { reason <- "registry section 3: non-convergence (engine converged = FALSE)"; m$info[, failed := TRUE] }
  m$failure_reason <- reason
  m
}
s32i_failed <- function(m) is.null(m$fit) || isTRUE(m$info$failed)

.s32i_na_cols <- c("estimate", "se", "df", "statistic", "ci_low", "ci_high", "p_raw", "F", "df1", "df2")
.s32i_fail_row <- function(x, reason) { for (cl in intersect(.s32i_na_cols, names(x))) data.table::set(x, j = cl, value = NA_real_)
  x[, `:=`(status = "FAILED", failure_reason = reason)][] }

#' Satterthwaite contest (the declared KR fallback), same columns as mmm_ci_contrast().
s32i_contrast_satt <- function(m, weights, estimand, level = 0.95) {
  fit <- m$fit; b <- lme4::fixef(fit); L <- mmm_ci_L(names(b), weights); w <- unlist(weights)
  ct <- lmerTest::contest(fit, L, joint = FALSE, ddf = "Satterthwaite", confint = TRUE, level = level)
  data.table::data.table(model_id = m$info$model_id, estimand = estimand, L = paste(sprintf("%s=%g", names(w), w), collapse = "; "),
    estimate = ct$Estimate, se = ct$`Std. Error`, df = ct$df, statistic = ct$`t value`, ci_low = ct$lower, ci_high = ct$upper,
    p_raw = ct$`Pr(>|t|)`, test = "Satterthwaite t (declared KR fallback)", df_method = "Satterthwaite", status = "OK")
}
s32i_joint_satt <- function(m, rows, estimand) {
  fit <- m$fit; b <- lme4::fixef(fit)
  L <- do.call(rbind, lapply(rows, function(r) mmm_ci_L(names(b), stats::setNames(1, r))))
  ct <- lmerTest::contest(fit, L, joint = TRUE, ddf = "Satterthwaite")
  data.table::data.table(model_id = m$info$model_id, estimand = estimand, rows = paste(rows, collapse = "; "), F = ct$`F value`, df1 = ct$NumDF,
    df2 = ct$DenDF, p_raw = ct$`Pr(>F)`, test = "Satterthwaite joint F (declared KR fallback)", df_method = "Satterthwaite", status = "OK")
}

#' Evaluate a KR computation; on numeric KR failure recompute with Satterthwaite (flag ddf_fallback). Returns one row.
s32i_kr <- function(m, kr_call, satt_call, failed_row) {
  if (s32i_failed(m)) return(.s32i_fail_row(failed_row(), m$failure_reason %||% "model FAILED")[, `:=`(ddf_fallback = FALSE, kr_messages = "")][])
  msgs <- character()
  x <- tryCatch(withCallingHandlers(kr_call(), warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }),
                error = function(e) e)
  bad <- inherits(x, "error") || any(grepl(S32I_KR_FALLBACK_PATTERN, msgs, fixed = TRUE)) ||
    (("df" %in% names(x)) && !all(is.finite(c(x$df, x$se)))) || (("df2" %in% names(x)) && !all(is.finite(c(x$df2, x$F))))
  kr_msg <- paste(unique(c(msgs, if (inherits(x, "error")) paste("KR error:", conditionMessage(x)))), collapse = " | ")
  if (!bad) { x[, `:=`(ddf_fallback = FALSE, failure_reason = NA_character_, kr_messages = kr_msg)]; return(x[]) }
  y <- tryCatch(satt_call(), error = function(e) e)
  if (inherits(y, "error")) return(.s32i_fail_row(failed_row(), paste("KR and Satterthwaite both failed:", conditionMessage(y)))[, `:=`(ddf_fallback = TRUE, kr_messages = kr_msg)][])
  y[, `:=`(ddf_fallback = TRUE, failure_reason = NA_character_, kr_messages = kr_msg)][]
}
# an L-vector naming a coefficient that is not in the model is a code error: it stops the run (checked outside the KR
# error handler so that it can never be reported as a KR failure)
s32i_contrast <- function(m, weights, estimand) {
  if (!s32i_failed(m)) invisible(mmm_ci_L(names(lme4::fixef(m$fit)), weights))
  s32i_kr(m, function() mmm_ci_contrast(m, weights, estimand), function() s32i_contrast_satt(m, weights, estimand),
          function() mmm_ci_failed_row(m, estimand, "contrast", weights = weights))
}
s32i_joint <- function(m, rows, estimand) {
  if (!s32i_failed(m)) invisible(mmm_ci_match(rows, names(lme4::fixef(m$fit))))
  s32i_kr(m, function() mmm_ci_joint(m, rows, estimand), function() s32i_joint_satt(m, rows, estimand),
          function() mmm_ci_failed_row(m, estimand, "joint", rows = rows))
}

#' Variance components as "grp:var=vcov" text and a few named components.
s32i_varcomp <- function(m) {
  if (is.null(m$fit)) return(NA_character_)
  vc <- as.data.frame(lme4::VarCorr(m$fit))
  paste(sprintf("%s:%s%s=%.6g", vc$grp, vc$var1, ifelse(is.na(vc$var2), "", paste0(",", vc$var2)), vc$vcov), collapse = "; ")
}

# ---------------------------------------------------------------- jobs
#' One estimand specification. tier: primary (tested), secondary (tested, H13 only), estimation, sensitivity.
s32i_E <- function(estimand, weights, hypothesis_id = NA_character_, tier = "estimation") list(estimand = estimand, weights = weights, hypothesis_id = hypothesis_id, tier = tier)
s32i_J <- function(estimand, rows, hypothesis_id = NA_character_, tier = "estimation") list(estimand = estimand, rows = rows, hypothesis_id = hypothesis_id, tier = tier)

#' A model job: data subset (a function of the coded design), formula key, estimands, joint tests, metadata.
s32i_job <- function(model_id, module, metric, population, window, fkey, rows_fun, estimands = list(), joints = list(),
                     variant = "primary", requires = NA_character_, direction = NA_character_)
  list(model_id = model_id, module = module, metric = metric, population = population, window = window, fkey = fkey,
       rows_fun = rows_fun, estimands = estimands, joints = joints, variant = variant, requires = requires, direction = direction)

#' Row selectors on the coded design table `d` (all animals; phase-level rows).
s32i_sel <- list(
  A1_CC1 = function(d) d[phase_type == "active" & k == 1L & CC == "CC1"],
  A1_ALL = function(d) d[phase_type == "active" & k == 1L],
  C_ROWS = function(d) d[phase_type == "active" & in_clean_set == TRUE],
  D_ROWS = function(d) d[phase_type == "active" & in_clean_set == TRUE & CC %in% c("CC1", "CC2", "CC3")],
  E_ROWS = function(d) d[phase_type == "light" & in_clean_set == TRUE])
s32i_sis <- function(f) function(d) f(d)[Group != "CON"]

#' The registered job list (registry sections 4, 6, 7). `hyp` maps (module, metric) to the hypothesis ids.
s32i_jobs <- function(pm = S32I_PRIMARY_METRICS, sm = S32I_SECONDARY_METRICS) {
  H <- list(A_EXP = c(crossing_rate = "H01", shared_zone_use = "H02"), A_CZ = c(crossing_rate = "H03", shared_zone_use = "H04"),
            B_EXP = c(crossing_rate = "H05", shared_zone_use = "H06"), C_EXP = c(crossing_rate = "H07", shared_zone_use = "H08"),
            C_CZ = c(crossing_rate = "H09", shared_zone_use = "H10"))
  h <- function(mod, k) if (k %in% names(H[[mod]])) H[[mod]][[k]] else NA_character_
  tr <- function(mod, k) if (k %in% names(H[[mod]])) "primary" else "estimation"
  J <- list(); add <- function(j) J[[length(J) + 1L]] <<- j
  cc_rows <- c("g_SIS:c2", "g_SIS:c3", "g_SIS:c4")
  for (k in c(pm, sm)) {
    # A-exp (EXPOSURE_CC1) and its reproduction estimands
    add(s32i_job(paste0("A_EXP|", k), "A", k, "all RFID (111)", "A1 of CC1", "A_EXP", s32i_sel$A1_CC1,
      list(s32i_E("SIS_minus_CON", list(g_SIS = 1), h("A_EXP", k), tr("A_EXP", k)),
           s32i_E("SIS_minus_CON_x_sex", list(`g_SIS:sex_c` = 1))), direction = "SIS - CON"))
    # A-cz
    add(s32i_job(paste0("A_CZ|", k), "A", k, "SIS (87)", "A1 of CC1", "A_CZ", s32i_sis(s32i_sel$A1_CC1),
      list(s32i_E("CombZ_wb_slope", list(CombZ_wb = 1), h("A_CZ", k), tr("A_CZ", k)),
           s32i_E("CombZ_wb_slope_x_sex", list(`CombZ_wb:sex_c` = 1))), direction = S32I_CAVEAT_CZ))
    # B-exp (EXPOSURE_TR)
    est <- list()
    for (cc in 2:4) est[[length(est) + 1L]] <- s32i_E(paste0("SIS_minus_CON_change_CC1_to_CC", cc), stats::setNames(list(1), paste0("g_SIS:c", cc)))
    for (cc in 1:4) {
      est[[length(est) + 1L]] <- s32i_E(paste0("SC_sexavg_TR_CC", cc), if (cc == 1) list(g_SIS = 1) else stats::setNames(list(1, 1), c("g_SIS", paste0("g_SIS:c", cc))))
      est[[length(est) + 1L]] <- s32i_E(paste0("DiD_SC_TR_CC", cc), if (cc == 1) list(`g_SIS:sex_c` = 1) else stats::setNames(list(1, 1), c("g_SIS:sex_c", paste0("g_SIS:c", cc, ":sex_c"))))
    }
    for (cc in 2:4) est[[length(est) + 1L]] <- s32i_E(paste0("Exposure_x_CC", cc, "_x_Sex"), stats::setNames(list(1), paste0("g_SIS:c", cc, ":sex_c")))
    add(s32i_job(paste0("B_EXP|", k), "B", k, "all RFID (111)", "A1 of CC1-CC4", if (k == "crossing_rate") "B_EXP_RATE" else "B_EXP", s32i_sel$A1_ALL, est,
      if (k %in% pm) list(s32i_J("Exposure_x_CC_joint", cc_rows, h("B_EXP", k), "primary")) else list(), direction = "SIS - CON"))
    # C-exp, C-cz
    ce <- list(s32i_E("SIS_minus_CON_phase_slope", list(`g_SIS:phase` = 1), h("C_EXP", k), tr("C_EXP", k)))
    if (k %in% pm) ce <- c(ce, list(s32i_E("shared_phase_slope", list(phase = 1)), s32i_E("SIS_minus_CON_A1_CC1", list(g_SIS = 1))))
    add(s32i_job(paste0("C_EXP|", k), "C", k, "all RFID (111)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_EXP", s32i_sel$C_ROWS, ce, direction = "SIS - CON"))
    cz <- list(s32i_E("CombZ_wb_phase_slope", list(`CombZ_wb:phase` = 1), h("C_CZ", k), tr("C_CZ", k)))
    if (k %in% pm) cz <- c(cz, list(s32i_E("SIS_phase_slope_at_CombZ_wb_0", list(phase = 1)), s32i_E("CombZ_wb_slope_A1_CC1", list(CombZ_wb = 1))))
    add(s32i_job(paste0("C_CZ|", k), "C", k, "SIS (87)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_CZ", s32i_sis(s32i_sel$C_ROWS), cz, direction = S32I_CAVEAT_CZ))
  }
  for (k in pm) {
    # A-dec (estimation only)
    add(s32i_job(paste0("A_DEC|", k), "A", k, "all RFID (111)", "A1 of CC1", "A_DEC", s32i_sel$A1_CC1,
      list(s32i_E("RES_minus_CON", list(group3RES = 1)), s32i_E("SUS_minus_CON", list(group3SUS = 1)),
           s32i_E("RES_minus_SUS", list(group3RES = 1, group3SUS = -1))), direction = "three-level decomposition (descriptive)"))
    # C secondary (estimation only; only if the primary C model is interpretable = not FAILED)
    add(s32i_job(paste0("C_EXP_PHASExCC|", k), "C_secondary", k, "all RFID (111)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_EXP_PHASExCC", s32i_sel$C_ROWS,
      c(list(s32i_E("SIS_minus_CON_phase_slope_CC1", list(`g_SIS:phase` = 1))),
        lapply(2:4, function(cc) s32i_E(paste0("SIS_minus_CON_phase_slope_change_CC1_to_CC", cc), stats::setNames(list(1), paste0("g_SIS:phase:c", cc))))),
      requires = paste0("C_EXP|", k), direction = "SIS - CON"))
    add(s32i_job(paste0("C_EXP_PHASExSEX|", k), "C_secondary", k, "all RFID (111)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_EXP_PHASExSEX", s32i_sel$C_ROWS,
      list(s32i_E("SIS_minus_CON_phase_slope_x_sex", list(`g_SIS:phase:sex_c` = 1))), requires = paste0("C_EXP|", k), direction = "SIS - CON"))
    add(s32i_job(paste0("C_CZ_PHASExCC|", k), "C_secondary", k, "SIS (87)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_CZ_PHASExCC", s32i_sis(s32i_sel$C_ROWS),
      c(list(s32i_E("CombZ_wb_phase_slope_CC1", list(`CombZ_wb:phase` = 1))),
        lapply(2:4, function(cc) s32i_E(paste0("CombZ_wb_phase_slope_change_CC1_to_CC", cc), stats::setNames(list(1), paste0("CombZ_wb:phase:c", cc))))),
      requires = paste0("C_CZ|", k), direction = S32I_CAVEAT_CZ))
    add(s32i_job(paste0("C_CZ_PHASExSEX|", k), "C_secondary", k, "SIS (87)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_CZ_PHASExSEX", s32i_sis(s32i_sel$C_ROWS),
      list(s32i_E("CombZ_wb_phase_slope_x_sex", list(`CombZ_wb:phase:sex_c` = 1))), requires = paste0("C_CZ|", k), direction = S32I_CAVEAT_CZ))
    # D (estimation only): categorical phase, CC1-CC3 rows; L-vectors built from the data (batch levels) in s32i_d_estimands()
    add(s32i_job(paste0("D_EXP|", k), "D", k, "all RFID (111)", "A1-A4 CC1-CC3", "D_EXP", s32i_sel$D_ROWS, "D_EXP", direction = "SIS - CON"))
    add(s32i_job(paste0("D_CZ|", k), "D", k, "SIS (87)", "A1-A4 CC1-CC3", "D_CZ", s32i_sis(s32i_sel$D_ROWS), "D_CZ", direction = S32I_CAVEAT_CZ))
    # sensitivities (section 7; estimates and CIs only)
    pe <- list(s32i_E("SIS_minus_CON_phase_slope", list(`g_SIS:phase` = 1), h("C_EXP", k), "sensitivity"))
    pz <- list(s32i_E("CombZ_wb_phase_slope", list(`CombZ_wb:phase` = 1), h("C_CZ", k), "sensitivity"))
    catE <- function(pre, hid) list(s32i_E("A1_to_A2_change", stats::setNames(list(1), paste0(pre, ":phase_f1")), hid, "sensitivity"),
                                    s32i_E("A2_to_A4_plateau_change", stats::setNames(list(1, -1), paste0(pre, c(":phase_f3", ":phase_f1"))), hid, "sensitivity"),
                                    s32i_E("A1_to_A4_change", stats::setNames(list(1), paste0(pre, ":phase_f3")), hid, "sensitivity"),
                                    s32i_E("linear_trend_of_categorical", stats::setNames(list(-0.1, 0.1, 0.3), paste0(pre, c(":phase_f1", ":phase_f2", ":phase_f3"))), hid, "sensitivity"))
    add(s32i_job(paste0("SENS_C_PHASEF_EXP|", k), "C", k, "all RFID (111)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_EXP_PHASEF", s32i_sel$C_ROWS, catE("g_SIS", h("C_EXP", k)),
                 variant = "categorical phase (phase_f)", direction = "SIS - CON"))
    add(s32i_job(paste0("SENS_C_PHASEF_CZ|", k), "C", k, "SIS (87)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_CZ_PHASEF", s32i_sis(s32i_sel$C_ROWS), catE("CombZ_wb", h("C_CZ", k)),
                 variant = "categorical phase (phase_f)", direction = S32I_CAVEAT_CZ))
    add(s32i_job(paste0("SENS_C_STRICT_EXP|", k), "C", k, "all RFID (111)", "strict completeness", "C_EXP", function(d) s32i_sel$C_ROWS(d)[complete_strict == TRUE], pe,
                 variant = "strict completeness (tolerance 0)", direction = "SIS - CON"))
    add(s32i_job(paste0("SENS_C_STRICT_CZ|", k), "C", k, "SIS (87)", "strict completeness", "C_CZ", function(d) s32i_sis(s32i_sel$C_ROWS)(d)[complete_strict == TRUE], pz,
                 variant = "strict completeness (tolerance 0)", direction = S32I_CAVEAT_CZ))
    add(s32i_job(paste0("SENS_C_ANIMALCC_EXP|", k), "C", k, "all RFID (111)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_EXP_ANIMALCC", s32i_sel$C_ROWS, pe,
                 variant = "added (1 | AnimalNum:CC)", direction = "SIS - CON"))
    add(s32i_job(paste0("SENS_C_ANIMALCC_CZ|", k), "C", k, "SIS (87)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_CZ_ANIMALCC", s32i_sis(s32i_sel$C_ROWS), pz,
                 variant = "added (1 | AnimalNum:CC)", direction = S32I_CAVEAT_CZ))
    add(s32i_job(paste0("SENS_C_SEPSLOPE_EXP|", k), "C", k, "all RFID (111)", "A1-A4 CC1-CC3; A1-A2 CC4", "C_EXP_SEPSLOPE", s32i_sel$C_ROWS, pe,
                 variant = "separate CON / SIS cage phase-slope variances", direction = "SIS - CON"))
    add(s32i_job(paste0("SENS_A_COMMON|", k), "A", k, "all RFID (111)", "A1 of CC1", "A_EXP_COMMON", s32i_sel$A1_CC1,
                 list(s32i_E("SIS_minus_CON", list(g_SIS = 1), h("A_EXP", k), "sensitivity")), variant = "one common cage variance (1 | CageEpisodeID)", direction = "SIS - CON"))
    add(s32i_job(paste0("SENS_B_COMMON|", k), "B", k, "all RFID (111)", "A1 of CC1-CC4", if (k == "crossing_rate") "B_EXP_RATE_COMMON" else "B_EXP_COMMON", s32i_sel$A1_ALL,
                 lapply(2:4, function(cc) s32i_E(paste0("SIS_minus_CON_change_CC1_to_CC", cc), stats::setNames(list(1), paste0("g_SIS:c", cc)), h("B_EXP", k), "sensitivity")),
                 list(s32i_J("Exposure_x_CC_joint", cc_rows, h("B_EXP", k), "sensitivity")), variant = "one common cage variance (1 | CageEpisodeID)", direction = "SIS - CON"))
  }
  # E: light-phase position-change rate (estimation only); light index in `phase`
  add(s32i_job("E_EXP|light_crossing_rate", "E", "light_crossing_rate", "all RFID (111)", "L1-L3 CC1-CC3; L1 CC4", "C_EXP", s32i_sel$E_ROWS,
    list(s32i_E("SIS_minus_CON_L1_CC1", list(g_SIS = 1)), s32i_E("SIS_minus_CON_light_index_slope", list(`g_SIS:phase` = 1))), direction = "SIS - CON"))
  add(s32i_job("E_CZ|light_crossing_rate", "E", "light_crossing_rate", "SIS (87)", "L1-L3 CC1-CC3; L1 CC4", "C_CZ", s32i_sis(s32i_sel$E_ROWS),
    list(s32i_E("CombZ_wb_slope_L1_CC1", list(CombZ_wb = 1)), s32i_E("CombZ_wb_light_index_slope", list(`CombZ_wb:phase` = 1))), direction = S32I_CAVEAT_CZ))
  J
}

#' Module D L-vectors (registry section 4 D): the mean of A2, A3, A4 averaged over CC1-CC3, plus the categorical phase
#' profile (batch-balanced, CC1-CC3-averaged). `batch` = the analysis rows' Batch.
s32i_d_estimands <- function(kind = c("D_EXP", "D_CZ"), batch) {
  kind <- match.arg(kind); bw <- s32i_batch_weights(batch); third <- 1 / 3
  pf <- function(pre, j) if (j == 0) list() else stats::setNames(list(1), paste0(pre, "phase_f", j))
  if (kind == "D_EXP") {
    out <- list(s32i_E("later_phase_level_SIS_minus_CON", list(g_SIS = 1, `g_SIS:phase_f1` = third, `g_SIS:phase_f2` = third, `g_SIS:phase_f3` = third,
                                                              `g_SIS:c2` = third, `g_SIS:c3` = third)))
    for (j in 0:3) {
      out[[length(out) + 1L]] <- s32i_E(paste0("SIS_minus_CON_A", j + 1), c(list(g_SIS = 1), pf("g_SIS:", j), list(`g_SIS:c2` = third, `g_SIS:c3` = third)))
      for (g in c(CON = -0.5, SIS = 0.5)) {
        w <- c(list(`(Intercept)` = 1), bw, list(c2 = third, c3 = third, g_SIS = g), pf("", j),
               if (j > 0) stats::setNames(list(g), paste0("g_SIS:phase_f", j)), list(`g_SIS:c2` = g * third, `g_SIS:c3` = g * third))
        out[[length(out) + 1L]] <- s32i_E(paste0(if (g < 0) "CON" else "SIS", "_mean_A", j + 1), w)
      }
    }
  } else {
    out <- list(s32i_E("later_phase_level_CombZ_wb_slope", list(CombZ_wb = 1, `CombZ_wb:phase_f1` = third, `CombZ_wb:phase_f2` = third, `CombZ_wb:phase_f3` = third,
                                                               `CombZ_wb:c2` = third, `CombZ_wb:c3` = third)))
    for (j in 0:3) {
      out[[length(out) + 1L]] <- s32i_E(paste0("CombZ_wb_slope_A", j + 1), c(list(CombZ_wb = 1), pf("CombZ_wb:", j), list(`CombZ_wb:c2` = third, `CombZ_wb:c3` = third)))
      out[[length(out) + 1L]] <- s32i_E(paste0("SIS_mean_A", j + 1, "_at_CombZ_wb_0"), c(list(`(Intercept)` = 1), bw, list(c2 = third, c3 = third), pf("", j)))
    }
  }
  out
}

#' Run one job. Returns list(info, estimates, joints, diag). `status_of` = model statuses so far (for `requires`).
s32i_run_job <- function(job, d, status_of = character(), formulas = S32I_FORMULAS, ranks = S32I_RANK) {
  x <- job$rows_fun(d)
  if (identical(job$metric, "light_crossing_rate")) x <- s32i_rows(x, "crossing_rate", need_cz = grepl("_CZ", job$fkey)) else
    x <- s32i_rows(x, job$metric, need_cz = grepl("_CZ|^D_CZ", job$fkey))
  meta <- data.table::data.table(model_id = job$model_id, module = job$module, metric = job$metric, population = job$population, window = job$window,
                                 variant = job$variant, fkey = job$fkey, direction = job$direction)
  est_specs <- if (is.character(job$estimands)) s32i_d_estimands(job$estimands, x$Batch) else job$estimands
  req_status <- if (!is.na(job$requires) && job$requires %in% names(status_of)) status_of[[job$requires]] else "missing"
  if (!is.na(job$requires) && !identical(req_status, "OK")) {
    reason <- paste("not run: registry section 4 C secondary requires an interpretable primary model;", job$requires, "is", req_status)
    info <- cbind(meta, data.table::data.table(formula = formulas[[job$fkey]], n_obs = nrow(x), n_animals = data.table::uniqueN(x$AnimalNum),
      n_cage_episodes = data.table::uniqueN(x$CageEpisodeID), n_batches = data.table::uniqueN(x$Batch), rank = NA_integer_, expected_rank = ranks[[job$fkey]],
      singular = NA, converged = NA, optimizer_check_agree = NA, status = "NOT_RUN", failure_reason = reason, messages = "", error = NA_character_))
    est <- data.table::rbindlist(lapply(est_specs, function(e) data.table::data.table(model_id = job$model_id, estimand = e$estimand, status = "NOT_RUN", failure_reason = reason)))
    return(list(info = info, estimates = est, joints = data.table::data.table(), diag = info))
  }
  m <- s32i_fit(formulas[[job$fkey]], x, job$model_id, ranks[[job$fkey]])
  status <- if (s32i_failed(m)) "FAILED" else "OK"
  est <- data.table::rbindlist(lapply(est_specs, function(e)
    s32i_contrast(m, e$weights, e$estimand)[, `:=`(hypothesis_id = e$hypothesis_id, tier = e$tier)]), fill = TRUE)
  jt <- data.table::rbindlist(lapply(job$joints, function(j)
    s32i_joint(m, j$rows, j$estimand)[, `:=`(hypothesis_id = j$hypothesis_id, tier = j$tier)]), fill = TRUE)
  finish <- function(tb) {
    if (!nrow(tb)) return(tb)
    tb <- data.table::copy(tb)
    tb[, tested := tier %in% c("primary", "secondary")]
    if ("statistic" %in% names(tb)) tb[tested == FALSE, statistic := NA_real_]
    tb[tested == FALSE, p_raw := NA_real_]   # registry section 6: estimation-only rows carry no p
    tb[, `:=`(n_obs = nrow(x), n_animals = data.table::uniqueN(x$AnimalNum), n_cage_episodes = data.table::uniqueN(x$CageEpisodeID))]
    cbind(meta[rep(1L, nrow(tb)), !"model_id"], tb)
  }
  est <- finish(est); jt <- finish(jt)
  i <- m$info
  info <- cbind(meta, data.table::data.table(formula = formulas[[job$fkey]], n_obs = i$n_obs, n_animals = i$n_animals, n_cage_episodes = i$n_cage_episodes,
    n_batches = i$n_batches, rank = i$rank, expected_rank = i$expected_rank, singular = i$singular, converged = i$converged,
    optimizer_check_agree = i$optimizer_check_agree, status = status, failure_reason = m$failure_reason %||% NA_character_,
    messages = i$messages, error = i$error))
  n_con_cage <- if ("conCage" %in% names(x)) data.table::uniqueN(x[conCage == 1, CageEpisodeID]) else NA_integer_
  diag <- cbind(info, data.table::data.table(
    n_con_animals = sum(!duplicated(x$AnimalNum) & x$Group == "CON"), n_sis_animals = sum(!duplicated(x$AnimalNum) & x$Group != "CON"),
    n_con_cage_episodes = n_con_cage, reml_criterion = if (is.null(m$fit)) NA_real_ else as.numeric(lme4::REMLcrit(m$fit)),
    residual_sd = if (is.null(m$fit)) NA_real_ else stats::sigma(m$fit), variance_components = s32i_varcomp(m),
    n_ddf_fallback = sum(c(est$ddf_fallback, jt$ddf_fallback) %in% TRUE),
    kr_messages = paste(unique(c(est$kr_messages, jt$kr_messages)[nzchar(c(est$kr_messages, jt$kr_messages) %||% "")]), collapse = " | "),
    optimizer_check_messages = i$optimizer_check_messages))
  list(info = info, estimates = est, joints = jt, diag = diag)
}

#' Run all jobs in order (C secondary after their primary). Returns list(models, estimates, joints, diagnostics).
s32i_run_all <- function(d, jobs = s32i_jobs(), progress = function(...) invisible(NULL)) {
  st <- character(); R <- list()
  for (j in jobs) { t0 <- Sys.time(); r <- s32i_run_job(j, d, st); st[[j$model_id]] <- r$info$status; R[[j$model_id]] <- r
    progress(j$model_id, r$info$status, as.numeric(difftime(Sys.time(), t0, units = "secs"))) }
  list(models = data.table::rbindlist(lapply(R, `[[`, "info"), fill = TRUE), estimates = data.table::rbindlist(lapply(R, `[[`, "estimates"), fill = TRUE),
       joints = data.table::rbindlist(lapply(R, `[[`, "joints"), fill = TRUE), diagnostics = data.table::rbindlist(lapply(R, `[[`, "diag"), fill = TRUE))
}

# ---------------------------------------------------------------- multiplicity and sensitivities
#' Holm per family over the declared members (m kept when a member FAILED: NA p stays NA; mmm_ci_holm).
s32i_holm <- function(hyp, p_by_id) {
  h <- data.table::copy(data.table::as.data.table(hyp)); h[, p_raw := unname(p_by_id[HypothesisID])]
  h[, p_holm := NA_real_]
  for (f in setdiff(unique(h$FamilyID), "none")) { i <- which(h$FamilyID == f)
    if (length(i) != 2L) stop("Family ", f, " does not have the declared m = 2 members.", call. = FALSE)
    h[i, p_holm := mmm_ci_holm(p_raw)] }
  h[, declared_m := ifelse(FamilyID == "none", NA_integer_, 2L)]
  h[]
}

#' Reference (primary-model) row of a sensitivity row: the same metric and estimand in the registered primary model
#' (C-exp / C-cz / A-exp / B-exp); categorical-phase rows are compared through their linear-trend contrast only.
S32I_CATEGORICAL_ESTIMANDS <- c("A1_to_A2_change", "A2_to_A4_plateau_change", "A1_to_A4_change", "linear_trend_of_categorical")
s32i_reference_key <- function(model_id, estimand) {
  ref_model <- sub("^SENS_C_[A-Z]+_(EXP|CZ)[|]", "C_\\1|", model_id)
  ref_model <- sub("^SENS_A_COMMON[|]", "A_EXP|", ref_model); ref_model <- sub("^SENS_B_COMMON[|]", "B_EXP|", ref_model)
  slope <- ifelse(grepl("^C_CZ[|]", ref_model), "CombZ_wb_phase_slope", "SIS_minus_CON_phase_slope")
  data.table::data.table(ref_model_id = ref_model, ref_estimand = ifelse(estimand %in% S32I_CATEGORICAL_ESTIMANDS, slope, estimand),
                         comparable = !(estimand %in% setdiff(S32I_CATEGORICAL_ESTIMANDS, "linear_trend_of_categorical")))
}
#' Sensitivity rows: each sensitivity estimate against its reference row (shift in primary SEs, robustness label, sign).
s32i_sensitivities <- function(est, joints) {
  s <- data.table::copy(est[tier == "sensitivity"])
  s <- cbind(s, s32i_reference_key(s$model_id, s$estimand))
  prim <- est[, .(ref_model_id = model_id, ref_estimand = estimand, primary_estimate = estimate, primary_se = se,
                  primary_ci_low = ci_low, primary_ci_high = ci_high, primary_status = status)]
  x <- merge(s, prim, by = c("ref_model_id", "ref_estimand"), all.x = TRUE, sort = FALSE)
  if (nrow(x) != nrow(s)) stop("Sensitivity reference merge changed the row count.", call. = FALSE)
  x[, shift_se := ifelse(comparable, (estimate - primary_estimate) / primary_se, NA_real_)]
  x[, robustness_label := ifelse(comparable & status == "OK" & primary_status == "OK", mmm_ci_robust_label(primary_estimate, primary_se, estimate), NA_character_)]
  x[, same_sign := ifelse(comparable & is.finite(estimate) & is.finite(primary_estimate), sign(estimate) == sign(primary_estimate), NA)]
  x[, ci_excludes_zero := ifelse(is.finite(ci_low), ci_low > 0 | ci_high < 0, NA)]
  js <- if (nrow(joints)) joints[tier == "sensitivity"] else joints
  list(contrasts = x[], joints = js)
}

#' Reproduction gate (registry section 8): recomputed rows vs the frozen Stage 29 exposure rows (estimate, se, df, CI).
s32i_repro <- function(est, frozen, pairs, tol = S32I_REPRO_TOL, cols = S32I_REPRO_COLS) {
  data.table::rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
    p <- pairs[i]; a <- est[model_id == p$model_id & estimand == p$estimand]
    b <- frozen[model_id == p$frozen_model_id & estimand == p$frozen_estimand & (is.na(sex) | sex == "")]
    if (nrow(a) != 1L || nrow(b) != 1L)
      return(data.table::data.table(model_id = p$model_id, estimand = p$estimand, frozen_model_id = p$frozen_model_id, frozen_estimand = p$frozen_estimand,
                                    column = NA_character_, recomputed = NA_real_, frozen = NA_real_, abs_diff = NA_real_, passed = FALSE))
    data.table::rbindlist(lapply(cols, function(k) { va <- as.numeric(a[[k]]); vb <- as.numeric(b[[k]])
      data.table::data.table(model_id = p$model_id, estimand = p$estimand, frozen_model_id = p$frozen_model_id, frozen_estimand = p$frozen_estimand,
                             column = k, recomputed = va, frozen = vb, abs_diff = abs(va - vb), passed = is.finite(va) && is.finite(vb) && abs(va - vb) <= tol) }))
  }))
}
s32i_repro_pairs <- function(metrics = c(S32I_PRIMARY_METRICS, S32I_SECONDARY_METRICS)) {
  data.table::rbindlist(lapply(metrics, function(k) data.table::rbindlist(list(
    data.table::data.table(model_id = paste0("A_EXP|", k), estimand = c("SIS_minus_CON", "SIS_minus_CON_x_sex"),
                           frozen_model_id = paste0("EXPOSURE_CC1|", k), frozen_estimand = c("SC_sexavg_CC1", "DiD_SC_CC1"), gate = "A-exp = EXPOSURE_CC1"),
    data.table::data.table(model_id = paste0("B_EXP|", k), estimand = c(paste0("SC_sexavg_TR_CC", 1:4), paste0("DiD_SC_TR_CC", 1:4)),
                           frozen_model_id = paste0("EXPOSURE_TR|", k), frozen_estimand = c(paste0("SC_sexavg_TR_CC", 1:4), paste0("DiD_SC_TR_CC", 1:4)),
                           gate = "B-exp = EXPOSURE_TR")))))
}

# ---------------------------------------------------------------- descriptives
#' Group x Sex x CC x phase n / mean / sd (CON, RES, SUS and pooled SIS), plus CON cage means, for the used rows.
s32i_descriptives <- function(long, metrics = c(S32W_METRICS)) {
  x <- data.table::as.data.table(long)[complete_primary == TRUE]
  one <- function(v) list(n = sum(is.finite(v)), mean = if (any(is.finite(v))) mean(v, na.rm = TRUE) else NA_real_,
                          sd = if (sum(is.finite(v)) > 1) stats::sd(v, na.rm = TRUE) else NA_real_)
  g <- data.table::rbindlist(lapply(metrics, function(mt) {   # (`k` is a column of `long`: the loop variable must not be k)
    x[, v_ := x[[mt]]]
    a <- x[, c(one(v_), list(level = "group")), by = .(Group, Sex, CC, phase, phase_type)]
    b <- x[Group != "CON", c(one(v_), list(level = "group")), by = .(Sex, CC, phase, phase_type)][, Group := "SIS"]
    cg <- x[Group == "CON", c(one(v_), list(level = "CON_cage_mean")), by = .(CageEpisodeID, Sex, CC, phase, phase_type)][, Group := "CON"]
    is_light_rate <- mt == "crossing_rate"
    data.table::rbindlist(list(a, b, cg), fill = TRUE)[, metric := ifelse(phase_type == "light" & is_light_rate, "light_crossing_rate", mt)]
  }), fill = TRUE)
  g[, metric_label := S32I_METRIC_LABEL[metric]]
  data.table::setcolorder(g, c("metric", "metric_label", "level", "Group", "Sex", "CC", "phase", "phase_type", "CageEpisodeID", "n", "mean", "sd"))
  g[order(metric, level, Group, Sex, CC, phase_type, phase)]
}
