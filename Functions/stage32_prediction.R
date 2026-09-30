# ================================================================
# Stage 32 Module F: early prediction beyond Batch, and H13 (Exp9 SIS RFID)
# MMMSociability -- Functions/stage32_prediction.R
# ================================================================
# Pure constants and functions for Analysis/32_behavior_exposure_adaptation.R, implementing section 5 of the FROZEN registry
# docs/STAGE32_REGISTRY_v1.0.md (sha256 cfbd954c..., commit 0970d32). Sourcing this file reads and writes nothing.
#
# Data: the frozen Stage 09 input pipeline/09_early_prediction/10min/tables/model_ladder_input.csv (sha da78f80e); Stage 09
# is not re-run. Main population SIS (87); all animals (111) for continuity (estimation only).
# Models (OLS, fitted inside each training fold, no standardisation, median imputation from the training fold if any NA):
#   (A) CombZ ~ Batch;  (B) CombZ ~ <set>;  (C) CombZ ~ Batch + <set>
#   sets: movement_mean = Movement_mean; primary_behavior_family = Movement_mean + Movement_rmssd + Entropy_acf1.
# Held-out batch (LOBO; any fold in which a test animal's batch is absent from the training fold): the batch intercept is the
# mean of the training batch effects, i.e. beta0 + (1/K) sum of the K - 1 training Batch coefficients (reference = 0).
# R2_cv = 1 - SSE / SST, SST about the full-sample mean. Primary: LOCO by CC1 CageEpisodeID; Delta R2_LOCO = R2(C) - R2(A).
# Tests H11 / H12: 1000 within-Batch permutations of CombZ (seed 20260811), each refitting A and C (LOCO); one-sided
# p = (#{Delta_null >= Delta_obs} + 1) / 1001. The same permuted vectors are used for both behaviour sets (paired).
# Fitting inside folds is exact OLS through a per-fold linear operator (pred_test = P %*% y_train): the design and the
# imputation medians do not depend on CombZ, so permutations reuse the operators (asserted against lm() in the tests).
# H13 (secondary single test): CombZ ~ Batch + Movement_mean + Movement_mean:sex_c, SIS only, OLS via the frozen engine
# (mmm_ci_fit engine = "lm"), CR2 by CC1 CageEpisodeID with Satterthwaite df (clubSandwich; Stage 29 CONTINUOUS route).
# Requires: data.table, clubSandwich; Functions/rfid_canonical_inference.R (mmm_ci_fit).
# ================================================================

S32P_SETS <- list(movement_mean = "Movement_mean", primary_behavior_family = c("Movement_mean", "Movement_rmssd", "Entropy_acf1"))
S32P_HYP <- c(movement_mean = "H11", primary_behavior_family = "H12")
S32P_N_PERM <- 1000L
S32P_PERM_SEED <- 20260811L
S32P_CV_SEED <- 521L
S32P_CV_K <- 5L
S32P_CV_REPEATS <- 100L
S32P_RNG <- list(kind = "Mersenne-Twister", normal_kind = "Inversion", sample_kind = "Rejection")
S32P_TIE_TOL <- 1e-12        # a null draw within 1e-12 of the observed Delta R2 counts as >= (conservative)
S32P_H13_FORMULA <- "CombZ ~ Batch + Movement_mean + Movement_mean:sex_c"
S32P_H13_RANK <- 8L

s32p_set_rng <- function(seed) { RNGkind(S32P_RNG$kind, S32P_RNG$normal_kind, S32P_RNG$sample_kind); set.seed(seed) }

#' Design matrix of one model for rows `d` given the training batch levels (sorted; first = reference). Unseen batches get
#' 1/K in every non-reference dummy (mean of the training batch intercepts).
s32p_X <- function(d, vars, batch, train_levels) {
  X <- matrix(1, nrow(d), 1L, dimnames = list(NULL, "(Intercept)"))
  if (isTRUE(batch)) {
    nr <- train_levels[-1]; K <- length(train_levels)
    B <- matrix(0, nrow(d), length(nr), dimnames = list(NULL, paste0("Batch", nr)))
    for (j in seq_along(nr)) B[, j] <- as.numeric(d$Batch == nr[j])
    unseen <- !(d$Batch %in% train_levels); if (any(unseen)) B[unseen, ] <- 1 / K
    X <- cbind(X, B)
  }
  if (length(vars)) X <- cbind(X, as.matrix(d[, vars, with = FALSE]))
  X
}

#' Per-fold linear operators for one model: list of (test, train, P) with pred[test] = P %*% y[train].
#' model: "A" (Batch), "B" (set), "C" (Batch + set). Median imputation from the training fold (only if any NA).
s32p_operators <- function(d, fold, model, vars) {
  d <- data.table::as.data.table(d)
  use_b <- model %in% c("A", "C"); v <- if (model == "A") character() else vars
  lapply(sort(unique(fold)), function(g) {
    te <- which(fold == g); tr <- which(fold != g)
    dtr <- data.table::copy(d[tr]); dte <- data.table::copy(d[te])
    for (k in v) if (anyNA(dtr[[k]]) || anyNA(dte[[k]])) { med <- stats::median(dtr[[k]], na.rm = TRUE)
      data.table::set(dtr, which(is.na(dtr[[k]])), k, med); data.table::set(dte, which(is.na(dte[[k]])), k, med) }
    lev <- sort(unique(as.character(dtr$Batch)))
    Xtr <- s32p_X(dtr, v, use_b, lev); Xte <- s32p_X(dte, v, use_b, lev)
    if (qr(Xtr)$rank < ncol(Xtr)) stop("Rank-deficient training design (fold ", g, ", model ", model, ").", call. = FALSE)
    list(test = te, train = tr, P = Xte %*% qr.solve(Xtr, diag(nrow(Xtr))), n_unseen_batch = sum(!(dte$Batch %in% lev)))
  })
}
s32p_predict <- function(ops, y) { pred <- rep(NA_real_, length(y)); for (o in ops) pred[o$test] <- as.numeric(o$P %*% y[o$train]); pred }
s32p_r2 <- function(y, pred) 1 - sum((y - pred)^2) / sum((y - mean(y))^2)

#' Fold vectors: LOCO (CC1 CageEpisodeID), LOAO, LOBO; repeated cage-grouped k-fold (groups shuffled, assigned round-robin).
s32p_folds <- function(d) list(LOCO = as.integer(factor(d$CageEpisodeID)), LOAO = seq_len(nrow(d)), LOBO = as.integer(factor(d$Batch)))
s32p_grouped_kfold <- function(groups, k = S32P_CV_K, repeats = S32P_CV_REPEATS, seed = S32P_CV_SEED) {
  s32p_set_rng(seed); g <- sort(unique(as.character(groups)))
  lapply(seq_len(repeats), function(r) { f <- integer(length(g)); f[sample.int(length(g))] <- rep_len(seq_len(k), length(g))
    as.integer(f[match(as.character(groups), g)]) })
}

#' Permutation index matrix (n x n_perm): within Batch (strata in sorted order) or unrestricted.
s32p_perm_index <- function(batch, n_perm = S32P_N_PERM, seed = S32P_PERM_SEED, within = TRUE) {
  s32p_set_rng(seed); n <- length(batch); b <- as.character(batch); lev <- sort(unique(b))
  vapply(seq_len(n_perm), function(i) { idx <- seq_len(n)
    if (within) { for (l in lev) { pos <- which(b == l); idx[pos] <- pos[sample.int(length(pos))] } } else idx <- sample.int(n)
    idx }, integer(n))
}
s32p_perm_p <- function(null, obs, tol = S32P_TIE_TOL) (sum(null >= obs - tol) + 1) / (length(null) + 1)

#' Module F for one population: performance (R2 per model x set x scheme; Delta R2 = C - A), held-out predictions, and
#' (if perms) the within-Batch and unrestricted permutation nulls of Delta R2_LOCO for each set.
s32p_module <- function(d, population, perms = TRUE, sets = S32P_SETS) {
  d <- data.table::as.data.table(d); y <- d$CombZ
  if (anyNA(y)) stop("CombZ has missing values in ", population, call. = FALSE)
  folds <- s32p_folds(d); cvf <- s32p_grouped_kfold(d$CageEpisodeID)
  perf <- list(); held <- list(); cvr <- list(); ops_loco <- list()
  for (sn in names(sets)) for (mdl in c("A", "B", "C")) {
    if (mdl == "A" && sn != names(sets)[1]) next          # model A does not depend on the set
    key <- if (mdl == "A") "A" else paste(mdl, sn, sep = "|")
    for (sc in names(folds)) {
      ops <- s32p_operators(d, folds[[sc]], mdl, sets[[sn]]); pr <- s32p_predict(ops, y)
      if (sc == "LOCO") ops_loco[[key]] <- ops
      perf[[length(perf) + 1L]] <- data.table::data.table(population = population, model = mdl, behaviour_set = if (mdl == "A") "none (Batch only)" else sn,
        scheme = sc, r2 = s32p_r2(y, pr), n = length(y), n_folds = length(ops), n_unseen_batch_predictions = sum(vapply(ops, `[[`, 0L, "n_unseen_batch")))
      held[[length(held) + 1L]] <- data.table::data.table(population = population, model = mdl, behaviour_set = if (mdl == "A") "none (Batch only)" else sn,
        scheme = sc, AnimalNum = d$AnimalNum, Batch = d$Batch, CageEpisodeID = d$CageEpisodeID, CombZ = y, prediction = pr)
    }
    r2cv <- vapply(cvf, function(f) s32p_r2(y, s32p_predict(s32p_operators(d, f, mdl, sets[[sn]]), y)), 0)
    cvr[[key]] <- r2cv
    perf[[length(perf) + 1L]] <- data.table::data.table(population = population, model = mdl, behaviour_set = if (mdl == "A") "none (Batch only)" else sn,
      scheme = "grouped_5fold_x100", r2 = mean(r2cv), r2_median = stats::median(r2cv), r2_q025 = unname(stats::quantile(r2cv, 0.025)),
      r2_q975 = unname(stats::quantile(r2cv, 0.975)), n = length(y), n_folds = S32P_CV_K)
  }
  P <- data.table::rbindlist(perf, fill = TRUE)
  dl <- list()
  for (sn in names(sets)) for (sc in c(names(folds), "grouped_5fold_x100")) {
    a <- P[model == "A" & scheme == sc, r2]; cc <- P[model == "C" & behaviour_set == sn & scheme == sc, r2]
    row <- data.table::data.table(population = population, model = "C - A", behaviour_set = sn, scheme = sc, r2 = cc - a, n = length(y))
    if (sc == "grouped_5fold_x100") { dd <- cvr[[paste("C", sn, sep = "|")]] - cvr[["A"]]
      row[, `:=`(r2_median = stats::median(dd), r2_q025 = unname(stats::quantile(dd, 0.025)), r2_q975 = unname(stats::quantile(dd, 0.975)))] }
    dl[[length(dl) + 1L]] <- row
  }
  P <- data.table::rbindlist(c(list(P), dl), fill = TRUE)
  NULLS <- data.table::data.table()
  if (isTRUE(perms)) {
    NULLS <- data.table::rbindlist(lapply(c(within_batch = TRUE, unrestricted = FALSE), function(w) {
      idx <- s32p_perm_index(d$Batch, within = w)
      data.table::rbindlist(lapply(seq_len(ncol(idx)), function(i) { yp <- y[idx[, i]]
        ra <- s32p_r2(yp, s32p_predict(ops_loco[["A"]], yp))
        data.table::rbindlist(lapply(names(sets), function(sn) { rc <- s32p_r2(yp, s32p_predict(ops_loco[[paste("C", sn, sep = "|")]], yp))
          data.table::data.table(draw = i, behaviour_set = sn, r2_A = ra, r2_C = rc, delta_r2 = rc - ra) })) }))
    }), idcol = "null")
    NULLS[, `:=`(null = ifelse(null == "within_batch", "within_batch", "unrestricted"), population = population)]
  }
  list(performance = P, heldout = data.table::rbindlist(held), nulls = NULLS)
}

#' H11 / H12 rows and the unrestricted-null summary (estimation only; no p) from a module result.
s32p_tests <- function(res, sets = S32P_SETS, hyp = S32P_HYP) {
  P <- res$performance; N <- res$nulls
  data.table::rbindlist(lapply(names(sets), function(sn) {
    obs <- P[model == "C - A" & behaviour_set == sn & scheme == "LOCO", r2]
    wb <- N[null == "within_batch" & behaviour_set == sn, delta_r2]; ur <- N[null == "unrestricted" & behaviour_set == sn, delta_r2]
    data.table::data.table(hypothesis_id = hyp[[sn]], behaviour_set = sn, population = P$population[1], estimand = "Delta R2_LOCO = R2_LOCO(C) - R2_LOCO(A)",
      r2_loco_A = P[model == "A" & scheme == "LOCO", r2], r2_loco_B = P[model == "B" & behaviour_set == sn & scheme == "LOCO", r2],
      r2_loco_C = P[model == "C" & behaviour_set == sn & scheme == "LOCO", r2], delta_r2_loco = obs,
      n_perm = length(wb), n_null_ge_obs = sum(wb >= obs - S32P_TIE_TOL), p_raw = s32p_perm_p(wb, obs),
      null_within_median = stats::median(wb), null_within_q95 = unname(stats::quantile(wb, 0.95)), null_within_q975 = unname(stats::quantile(wb, 0.975)),
      null_unrestricted_median = stats::median(ur), null_unrestricted_q95 = unname(stats::quantile(ur, 0.95)),
      null_unrestricted_q975 = unname(stats::quantile(ur, 0.975)), n = P$n[1],
      test = "one-sided within-Batch permutation of CombZ (1000 draws, seed 20260811), A and C refitted per draw (LOCO by CC1 CageEpisodeID)")
  }))
}

#' H13: CombZ ~ Batch + Movement_mean + Movement_mean:sex_c (SIS), OLS; CR2 by CC1 CageEpisodeID, Satterthwaite df.
s32p_h13 <- function(d) {
  d <- data.table::copy(data.table::as.data.table(d)); d[, Batch := droplevels(factor(Batch))]
  m <- tryCatch(mmm_ci_fit(S32P_H13_FORMULA, d, "H13|Movement_mean", engine = "lm", expected_rank = S32P_H13_RANK), error = function(e) e)
  base <- data.table::data.table(hypothesis_id = "H13", model_id = "H13|Movement_mean", estimand = "Movement_mean:sex_c", formula = S32P_H13_FORMULA,
                                 n = nrow(d), n_clusters = data.table::uniqueN(d$CageEpisodeID))
  if (inherits(m, "error") || mmm_ci_fit_failed(m))
    return(cbind(base, data.table::data.table(estimate = NA_real_, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
      p_raw = NA_real_, se_ols = NA_real_, status = "FAILED", failure_reason = if (inherits(m, "error")) conditionMessage(m) else m$info$error)))
  V <- clubSandwich::vcovCR(m$fit, cluster = d$CageEpisodeID, type = "CR2")
  ct <- clubSandwich::coef_test(m$fit, vcov = V, test = "Satterthwaite", coefs = "Movement_mean:sex_c")
  e <- ct$beta; se <- ct$SE; df <- ct$df_Satt
  cbind(base, data.table::data.table(estimate = e, se = se, df = df, statistic = e / se, ci_low = e - stats::qt(0.975, df) * se,
    ci_high = e + stats::qt(0.975, df) * se, p_raw = ct$p_Satt, se_ols = summary(m$fit)$coefficients["Movement_mean:sex_c", 2], status = "OK",
    failure_reason = NA_character_, test = "OLS; CR2 by CC1 CageEpisodeID, Satterthwaite df (clubSandwich)"))
}
