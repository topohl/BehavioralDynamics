# Contract test for Stage 32 Module F and H13 (Functions/stage32_prediction.R; registry v1.0 section 5).
#
# Synthetic data in memory only: nothing here reads project data (the frozen Stage 09 input is not read) or any S: path, and
# no Analysis/ runner is sourced or executed. Checks:
#   1. per-fold operators: held-out predictions = lm() refitted on the training fold + predict() for models A, B, C (LOCO,
#      LOAO); training-fold median imputation; LOBO / unseen batch = mean of the training batch effects;
#   2. R2_cv = 1 - SSE / SST with SST about the full-sample mean; Delta R2 = C - A;
#   3. permutations: within-Batch index vectors keep every batch's animals, are reproducible (seed 20260811) and are shared
#      by both behaviour sets; p = (#{null >= obs} + 1) / 1001 with the tie tolerance; the unrestricted null differs;
#   4. repeated cage-grouped 5-fold: cages never split, 5 folds, reproducible (seed 521);
#   5. s32p_module / s32p_tests shapes (1000 + 1000 draws per set) and the null draws = direct LOCO refits;
#   6. H13 = direct lm() + clubSandwich CR2 by cage, Satterthwaite df.
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage32_prediction.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
for (h in c("rfid_canonical_inference.R", "stage32_prediction.R")) source_mmm_helper(h)
fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
near <- function(a, b, tol = 1e-9) length(a) == length(b) && all(abs(a - b) <= tol * pmax(1, abs(b)))

set.seed(11)
sexb <- c(B1 = -0.5, B2 = -0.5, B3 = 0.5, B4 = 0.5, B5 = -0.5, B6 = 0.5)
d <- rbindlist(lapply(names(sexb), function(b) data.table(AnimalNum = paste0(b, "_", 1:14), Batch = b, sex_c = sexb[[b]],
                                                          CageEpisodeID = paste0(b, "|sys.", rep(1:4, length.out = 14)))))
d[, `:=`(Movement_mean = rnorm(.N, 3.6, 0.6), Movement_rmssd = rnorm(.N, 4.6, 0.8), Entropy_acf1 = runif(.N, 0.3, 0.6))]
bm <- c(B1 = -0.4, B2 = 0.5, B3 = -0.4, B4 = -0.2, B5 = -0.4, B6 = 0.3)
d[, CombZ := bm[Batch] - 0.15 * (Movement_mean - 3.6) + rnorm(.N, 0, 0.6)]
setorder(d, AnimalNum)
S <- S32P_SETS$primary_behavior_family

# ---------------------------------------------------------------- 1. operators = lm() refits
lm_pred <- function(dd, form, fold) { pr <- rep(NA_real_, nrow(dd))
  for (g in sort(unique(fold))) { tr <- fold != g; f <- lm(form, data = dd[tr]); pr[!tr] <- predict(f, newdata = dd[!tr]) }; pr }
folds <- s32p_folds(d)
forms <- list(A = CombZ ~ Batch, B = CombZ ~ Movement_mean + Movement_rmssd + Entropy_acf1, C = CombZ ~ Batch + Movement_mean + Movement_rmssd + Entropy_acf1)
for (mdl in c("A", "B", "C")) for (sc in c("LOCO", "LOAO")) {
  pr <- s32p_predict(s32p_operators(d, folds[[sc]], mdl, S), d$CombZ)
  check(near(pr, lm_pred(d, forms[[mdl]], folds[[sc]])), paste("operator predictions = lm refits:", mdl, sc)) }
# LOBO: held-out batch intercept = mean of the training batch effects
lobo <- s32p_predict(s32p_operators(d, folds$LOBO, "C", S), d$CombZ)
man <- rep(NA_real_, nrow(d))
for (b in sort(unique(d$Batch))) { tr <- d$Batch != b; f <- lm(forms$C, data = d[tr]); cf <- coef(f)
  bi <- cf[["(Intercept)"]] + mean(c(0, cf[grepl("^Batch", names(cf))]))
  man[!tr] <- bi + as.matrix(d[!tr, S, with = FALSE]) %*% cf[S] }
check(near(lobo, man), "LOBO: held-out batch intercept = beta0 + mean of the training batch effects (reference 0)")
loboA <- s32p_predict(s32p_operators(d, folds$LOBO, "A", S), d$CombZ)
manA <- vapply(seq_len(nrow(d)), function(i) mean(d[Batch != d$Batch[i], mean(CombZ), by = Batch]$V1), 0)
check(near(loboA, manA), "LOBO for Batch-only: the unweighted mean of the training batch means")
dn <- copy(d); dn[c(3, 40), Movement_rmssd := NA]
prn <- s32p_predict(s32p_operators(dn, folds$LOCO, "B", S), dn$CombZ)
mann <- rep(NA_real_, nrow(dn))
for (g in sort(unique(folds$LOCO))) { tr <- folds$LOCO != g; dtr <- copy(dn[tr]); dte <- copy(dn[!tr]); med <- median(dtr$Movement_rmssd, na.rm = TRUE)
  dtr[is.na(Movement_rmssd), Movement_rmssd := med]; dte[is.na(Movement_rmssd), Movement_rmssd := med]
  mann[!tr] <- predict(lm(forms$B, data = dtr), newdata = dte) }
check(near(prn, mann), "median imputation from the training fold")
ok("per-fold operators = lm refits (A, B, C; LOCO, LOAO, LOBO; imputation)")

# ---------------------------------------------------------------- 2. R2
y <- d$CombZ; p <- y + rnorm(length(y), 0, 0.1)
check(near(s32p_r2(y, p), 1 - sum((y - p)^2) / sum((y - mean(y))^2)), "R2_cv = 1 - SSE / SST (full-sample mean)")
p0 <- s32p_predict(s32p_operators(d, folds$LOAO, "B", character()), y)
check(near(s32p_r2(y, p0), 1 - (nrow(d) / (nrow(d) - 1))^2), "intercept-only LOAO R2 = 1 - (n / (n - 1))^2")
ok("R2 definition")

# ---------------------------------------------------------------- 3. permutations and p
I1 <- s32p_perm_index(d$Batch); I2 <- s32p_perm_index(d$Batch)
check(identical(dim(I1), c(nrow(d), 1000L)) && identical(I1, I2), "1000 within-Batch index vectors, reproducible with seed 20260811")
check(all(apply(I1, 2, function(ix) all(d$Batch[ix] == d$Batch))) && all(apply(I1, 2, function(ix) setequal(ix, seq_len(nrow(d))))), "within-Batch: every draw permutes animals only within their batch")
U <- s32p_perm_index(d$Batch, within = FALSE)
check(!identical(U, I1) && any(apply(U, 2, function(ix) any(d$Batch[ix] != d$Batch))), "the unrestricted null permutes across batches")
check(near(s32p_perm_p(c(0.1, 0.2, 0.3), 0.2), 3 / 4) && near(s32p_perm_p(c(0.1, 0.2 - 1e-13), 0.2), 2 / 3) && near(s32p_perm_p(rep(0, 1000), 1), 1 / 1001),
      "p = (#{null >= obs} + 1) / (n + 1); a draw within 1e-12 counts as >=")
ok("permutations and p")

# ---------------------------------------------------------------- 4. grouped 5-fold
cv <- s32p_grouped_kfold(d$CageEpisodeID)
check(length(cv) == 100L && identical(cv, s32p_grouped_kfold(d$CageEpisodeID)) && all(vapply(cv, function(f) length(unique(f)) == 5L, TRUE)) &&
      all(vapply(cv, function(f) all(d[, uniqueN(f[.I]), by = CageEpisodeID]$V1 == 1L), TRUE)), "100 x grouped 5-fold: 5 folds, cages never split, seed 521 reproducible")
ok("repeated cage-grouped 5-fold")

# ---------------------------------------------------------------- 5. module and tests
res <- s32p_module(d, "SIS", perms = TRUE); te <- s32p_tests(res)
P <- res$performance
check(near(P[model == "C - A" & behaviour_set == "movement_mean" & scheme == "LOCO", r2],
           P[model == "C" & behaviour_set == "movement_mean" & scheme == "LOCO", r2] - P[model == "A" & scheme == "LOCO", r2]), "Delta R2 = R2(C) - R2(A)")
check(nrow(res$nulls) == 4000L && all(res$nulls[, .N, by = .(null, behaviour_set)]$N == 1000L), "1000 within-Batch + 1000 unrestricted draws per behaviour set")
i <- 17L; yp <- d$CombZ[I1[, i]]
dp <- copy(d)[, CombZ := yp]
dA <- s32p_r2(yp, lm_pred(dp, forms$A, folds$LOCO)); dC <- s32p_r2(yp, lm_pred(dp, CombZ ~ Batch + Movement_mean, folds$LOCO))
check(near(res$nulls[null == "within_batch" & behaviour_set == "movement_mean" & draw == i, delta_r2], dC - dA), "a null draw = direct LOCO refits of A and C on the permuted CombZ")
check(identical(res$nulls[null == "within_batch" & behaviour_set == "movement_mean", r2_A], res$nulls[null == "within_batch" & behaviour_set == "primary_behavior_family", r2_A]),
      "both behaviour sets see the same permuted vectors (paired)")
wb <- res$nulls[null == "within_batch" & behaviour_set == "primary_behavior_family", delta_r2]
check(nrow(te) == 2L && identical(te$hypothesis_id, c("H11", "H12")) && near(te$p_raw[2], (sum(wb >= te$delta_r2_loco[2] - 1e-12) + 1) / 1001) &&
      all(te$p_raw > 0 & te$p_raw <= 1), "H11 / H12 rows: observed Delta R2_LOCO and the one-sided within-Batch p")
check(all(c("LOCO", "LOAO", "LOBO", "grouped_5fold_x100") %in% P$scheme) && all(is.finite(P[scheme == "grouped_5fold_x100" & model != "C - A", r2_q975])),
      "performance: LOCO, LOAO, LOBO, grouped 5-fold (mean and 2.5 / 97.5 %)")
nop <- s32p_module(d, "ALL", perms = FALSE)
check(nrow(nop$nulls) == 0L && identical(nop$performance[, .(model, behaviour_set, scheme, r2)], P[, .(model, behaviour_set, scheme, r2)]), "perms = FALSE: identical performance, no draws")
ok("module, nulls and H11 / H12 rows")

# ---------------------------------------------------------------- 6. H13
h <- s32p_h13(d)
dd <- copy(d)[, Batch := factor(Batch)]
f <- lm(CombZ ~ Batch + Movement_mean + Movement_mean:sex_c, data = dd)
V <- clubSandwich::vcovCR(f, cluster = dd$CageEpisodeID, type = "CR2"); ct <- clubSandwich::coef_test(f, vcov = V, test = "Satterthwaite", coefs = "Movement_mean:sex_c")
check(identical(h$status, "OK") && near(h$estimate, ct$beta) && near(h$se, ct$SE) && near(h$df, ct$df_Satt) && near(h$p_raw, ct$p_Satt) &&
      near(h$ci_low, ct$beta - qt(0.975, ct$df_Satt) * ct$SE) && h$n_clusters == uniqueN(d$CageEpisodeID), "H13 = lm + CR2 by cage, Satterthwaite df")
hb <- s32p_h13(copy(d)[, Movement_mean := 1])
check(identical(hb$status, "FAILED") && is.na(hb$estimate), "a rank-deficient H13 design is FAILED")
ok("H13")
cat("test_stage32_prediction: all checks passed\n")
