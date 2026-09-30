# Contract test for the Stage 32 mixed-model modules A-E, sensitivities and Holm (Functions/stage32_inference.R; registry v1.0).
#
# Synthetic data in memory only (6 batches, sex nested in batch, one intact CON cage of 4 per batch, SIS regrouped into new
# cages at every CC, the registered clean phase set): nothing here reads project data or any S: path, and no Analysis/ runner
# is sourced or executed. Checks:
#   1. coding: g_SIS, sex_c, c2-c4, conCage / sisCage, group3 indicators, phase / phase_f; row selectors and counts;
#   2. registered formulas and ranks (A-exp / B-exp = the frozen config EXPOSURE_CC1 / EXPOSURE_TR);
#   3. A-exp, A-cz, C-exp, C-cz, B-exp joint: estimates, SE, KR df, CI, p = a direct lmerTest::lmer + contest(KR) fit;
#   4. D L-vectors: SIS mean - CON mean = SIS - CON per phase; the later-phase level = the mean of the A2-A4 contrasts;
#      A-dec RES - SUS = RES - CON minus SUS - CON;
#   5. failure rule: rank stop -> FAILED rows, non-convergence -> FAILED, unknown coefficient -> stop (code error),
#      KR -> Satterthwaite fallback flagged (warning, non-finite df, error) and FAILED when both fail; NOT_RUN for C secondary;
#   6. the complete registered job list runs on the synthetic data (every estimand name exists); estimation-only rows carry
#      no statistic / p; Holm keeps m = 2; sensitivity reference rows; reproduction comparison helper.
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage32_inference.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
for (h in c("behavior_analysis_config.R", "rfid_canonical_inference.R", "stage32_windows.R", "stage32_inference.R")) source_mmm_helper(h)
fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
near <- function(a, b, tol = 1e-8) length(a) == length(b) && all(abs(a - b) <= tol * pmax(1, abs(b)))
T0 <- Sys.time()

# ---------------------------------------------------------------- synthetic design (structure of the Exp9 RFID cohort)
make_design <- function(seed = 7, n_sis_cages = 2L, cage_n = 4L) {
  set.seed(seed)
  sexb <- c(B1 = "Male", B2 = "Male", B3 = "Female", B4 = "Female", B5 = "Male", B6 = "Female")
  an <- rbindlist(lapply(names(sexb), function(b) data.table(AnimalNum = paste0(b, "x", sprintf("%02d", seq_len(cage_n * (1L + n_sis_cages)))), Batch = b, Sex = sexb[[b]],
    Group = c(rep("CON", cage_n), sample(rep(c("RES", "SUS"), length.out = cage_n * n_sis_cages))))))
  an[, `:=`(u0 = rnorm(.N, 0, 2), u1 = rnorm(.N, 0, 0.6), CombZ = rnorm(.N) - 0.8 * (Group == "SUS"))]
  an[Group != "CON", CombZ_wb := CombZ - mean(CombZ), by = Batch]
  bef <- c(B1 = 0, B2 = 2, B3 = -1, B4 = 1, B5 = -2, B6 = 0.5)
  ph <- rbind(data.table(CC = rep(paste0("CC", 1:3), each = 7), phase = rep(c("A1", "A2", "A3", "A4", "L1", "L2", "L3"), 3)),
              data.table(CC = "CC4", phase = c("A1", "L1", "A2")))
  ph[, `:=`(phase_type = ifelse(substr(phase, 1, 1) == "A", "active", "light"), k = as.integer(substr(phase, 2, 2)))]
  ph[, phase_index := k - 1L]
  rows <- rbindlist(lapply(paste0("CC", 1:4), function(cc) {
    cages <- rbindlist(lapply(names(sexb), function(b) { a <- an[Batch == b]
      sis <- sample(a[Group != "CON", AnimalNum])
      rbind(data.table(AnimalNum = a[Group == "CON", AnimalNum], System = "sys.1"),
            data.table(AnimalNum = sis, System = paste0("sys.", 1L + rep(seq_len(n_sis_cages), each = cage_n)))) }))
    cages[, CC := cc]; cages }))
  d <- merge(merge(rows, an, by = "AnimalNum"), ph, by = "CC", allow.cartesian = TRUE)
  d[, CageEpisodeID := paste(Batch, System, CC, sep = "|")]
  ce <- unique(d[, .(CageEpisodeID)])[, `:=`(v0 = rnorm(.N, 0, 1.5), v1 = rnorm(.N, 0, 0.4), w0 = rnorm(.N, 0, 0.03))]
  d <- merge(d, ce, by = "CageEpisodeID")
  gs <- ifelse(d$Group == "CON", -0.5, 0.5); ccn <- as.integer(sub("CC", "", d$CC))
  d[, crossing_rate := 25 + bef[Batch] + 1.5 * (ccn - 1) - 2 * phase_index + 0.8 * gs * phase_index + u0 + u1 * phase_index + v0 + v1 * phase_index -
      0.5 * CombZ * phase_index * (Group != "CON") - 12 * (phase_type == "light") + rnorm(.N, 0, 2)]
  d[, shared_zone_use := pmin(0.9, pmax(0.02, 0.25 + 0.02 * gs - 0.01 * phase_index + w0 + rnorm(.N, 0, 0.04)))]
  d[, occupancy_dispersion := 2 + 0.02 * phase_index + rnorm(.N, 0, 0.1)]
  d[, fragmentation := pmin(0.95, pmax(0.05, 0.3 + 0.01 * phase_index + rnorm(.N, 0, 0.05)))]
  d[AnimalNum == an[Group != "CON"][1, AnimalNum] & CC == "CC1" & phase == "A1", shared_zone_use := NA_real_]
  d[, `:=`(in_clean_set = TRUE, complete_primary = TRUE, complete_strict = !(CC == "CC2" & phase == "A4" & System == "sys.2"), phase = as.numeric(phase_index),
           phase_label = phase)]
  d[, c("u0", "u1", "v0", "v1", "w0") := NULL]
  s32i_code(d)[]
}
D <- make_design()

# ---------------------------------------------------------------- 1. coding and selectors
check(all(D[Group == "CON", g_SIS] == -0.5) && all(D[Group != "CON", g_SIS] == 0.5) && all(D[Sex == "Female", sex_c] == 0.5), "g_SIS and sex_c coding")
check(all(D[Group == "CON", conCage] == 1) && all(D[Group != "CON", conCage] == 0) && all(D$sisCage == 1 - D$conCage), "conCage / sisCage per cage episode")
check(all(D$group3RES == (D$Group == "RES")) && all(D$group3SUS == (D$Group == "SUS")) && identical(levels(D$phase_f), as.character(0:3)), "group3 indicators and phase_f")
n_an <- uniqueN(D$AnimalNum)
check(nrow(s32i_sel$A1_CC1(D)) == n_an && nrow(s32i_sel$A1_ALL(D)) == 4L * n_an && nrow(s32i_sel$C_ROWS(D)) == 14L * n_an &&
      nrow(s32i_sel$D_ROWS(D)) == 12L * n_an && nrow(s32i_sel$E_ROWS(D)) == 10L * n_an, "row selectors: A1 CC1, A1 CC1-CC4, C, D, E")
ok("coding and row selectors")

# ---------------------------------------------------------------- 2. formulas and ranks
fsub <- function(f) gsub("AnimalID", "AnimalNum", f); CF <- MMM_BEHAVIOR_CONFIG$models
check(identical(S32I_FORMULAS$A_EXP, fsub(CF$EXPOSURE_CC1$formula)) && identical(S32I_FORMULAS$B_EXP, fsub(CF$EXPOSURE_TR$formula)) &&
      identical(S32I_FORMULAS$B_EXP_RATE, fsub(sub("(1 | AnimalID)", CF$EXPOSURE_TR$crossing_rate_animal_term, CF$EXPOSURE_TR$formula, fixed = TRUE))),
      "A-exp / B-exp formulas = frozen EXPOSURE_CC1 / EXPOSURE_TR")
check(grepl("(0 + phase | CageEpisodeID)", S32I_FORMULAS$C_EXP, fixed = TRUE) && grepl("(1 + phase || AnimalNum)", S32I_FORMULAS$C_EXP, fixed = TRUE) &&
      grepl("(1 + phase || CageEpisodeID)", S32I_FORMULAS$C_CZ, fixed = TRUE) && grepl("(1 | AnimalNum:CC)", S32I_FORMULAS$C_EXP_ANIMALCC, fixed = TRUE) &&
      grepl("(0 + conCage:phase | CageEpisodeID) + (0 + sisCage:phase | CageEpisodeID)", S32I_FORMULAS$C_EXP_SEPSLOPE, fixed = TRUE) &&
      grepl("(1 | CageEpisodeID) + (0 + isCON | Batch)", S32I_FORMULAS$B_EXP_COMMON, fixed = TRUE), "registered random structures")
sel_for <- list(A_EXP = "A1_CC1", A_EXP_COMMON = "A1_CC1", A_DEC = "A1_CC1", A_CZ = "A1_CC1", B_EXP = "A1_ALL", B_EXP_RATE = "A1_ALL", B_EXP_COMMON = "A1_ALL",
                B_EXP_RATE_COMMON = "A1_ALL", D_EXP = "D_ROWS", D_CZ = "D_ROWS")
for (f in names(S32I_RANK)) { x <- s32i_sel[[sel_for[[f]] %||% "C_ROWS"]](D); if (grepl("CZ", f)) x <- x[Group != "CON"]
  X <- model.matrix(as.formula(paste("crossing_rate ~", deparse1(reformulas::nobars(as.formula(S32I_FORMULAS[[f]]))[[3]]))), droplevels(x))
  check(ncol(X) == S32I_RANK[[f]] && qr(X)$rank == ncol(X), paste("full rank and declared rank:", f, ncol(X))) }
ok("formulas = frozen config; every design full rank with its declared rank")

# ---------------------------------------------------------------- 3. estimates = direct lmerTest KR fits
direct <- function(fkey, rows, metric, L) {
  x <- s32i_rows(rows, metric, need_cz = grepl("CZ", fkey))
  fit <- lmerTest::lmer(as.formula(S32I_FORMULAS[[fkey]]), data = x, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
  b <- lme4::fixef(fit); Lv <- setNames(rep(0, length(b)), names(b)); Lv[names(L)] <- unlist(L)
  lmerTest::contest(fit, Lv, joint = FALSE, ddf = "Kenward-Roger", confint = TRUE) }
cmp_row <- function(r, ct, what) check(near(r$estimate, ct$Estimate) && near(r$se, ct$`Std. Error`) && near(r$df, ct$df) && near(r$ci_low, ct$lower) &&
                                         near(r$ci_high, ct$upper) && near(r$p_raw, ct$`Pr(>|t|)`) && identical(r$status, "OK") && isFALSE(r$ddf_fallback), what)
J <- s32i_jobs(); jid <- vapply(J, `[[`, "", "model_id"); getj <- function(id) J[[which(jid == id)]]
rA <- s32i_run_job(getj("A_EXP|crossing_rate"), D)
cmp_row(rA$estimates[estimand == "SIS_minus_CON"], direct("A_EXP", s32i_sel$A1_CC1(D), "crossing_rate", list(g_SIS = 1)), "A-exp g_SIS = direct KR fit")
check(identical(rA$estimates[estimand == "SIS_minus_CON", hypothesis_id], "H01") && isTRUE(rA$estimates[estimand == "SIS_minus_CON", tested]) &&
      is.na(rA$estimates[estimand == "SIS_minus_CON_x_sex", p_raw]) && is.na(rA$estimates[estimand == "SIS_minus_CON_x_sex", statistic]),
      "H01 tested; g_SIS:sex_c estimation only (no statistic, no p)")
rZ <- s32i_run_job(getj("A_CZ|shared_zone_use"), D)
cmp_row(rZ$estimates[estimand == "CombZ_wb_slope"], direct("A_CZ", s32i_sel$A1_CC1(D)[Group != "CON"], "shared_zone_use", list(CombZ_wb = 1)), "A-cz CombZ_wb slope (overlap, non-NA rows) = direct")
check(rZ$info$n_animals == uniqueN(D[Group != "CON", AnimalNum]) - 1L, "A-cz overlap uses the finite SIS rows only")
rC <- s32i_run_job(getj("C_EXP|crossing_rate"), D)
cmp_row(rC$estimates[estimand == "SIS_minus_CON_phase_slope"], direct("C_EXP", s32i_sel$C_ROWS(D), "crossing_rate", list(`g_SIS:phase` = 1)), "C-exp g_SIS:phase = direct")
rCz <- s32i_run_job(getj("C_CZ|crossing_rate"), D)
cmp_row(rCz$estimates[estimand == "CombZ_wb_phase_slope"], direct("C_CZ", s32i_sel$C_ROWS(D)[Group != "CON"], "crossing_rate", list(`CombZ_wb:phase` = 1)), "C-cz CombZ_wb:phase = direct")
rB <- s32i_run_job(getj("B_EXP|shared_zone_use"), D)
xb <- s32i_rows(s32i_sel$A1_ALL(D), "shared_zone_use")
fb <- lmerTest::lmer(as.formula(S32I_FORMULAS$B_EXP), data = xb, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
bn <- names(lme4::fixef(fb)); Lj <- t(sapply(c("g_SIS:c2", "g_SIS:c3", "g_SIS:c4"), function(r) as.numeric(bn == r)))
cj <- lmerTest::contest(fb, Lj, joint = TRUE, ddf = "Kenward-Roger"); jr <- rB$joints
check(nrow(jr) == 1L && near(jr$F, cj$`F value`) && near(jr$df2, cj$DenDF) && near(jr$p_raw, cj$`Pr(>F)`) && jr$df1 == 3 && identical(jr$hypothesis_id, "H06"),
      "B-exp joint KR F(3) of g_SIS:(c2 + c3 + c4) = direct (H06)")
check(nrow(rB$estimates) == 14L && all(is.na(rB$estimates$p_raw)), "B-exp: 14 estimation-only L-vectors (components, SC_sexavg / DiD per CC, x Sex)")
ok("A-exp, A-cz, C-exp, C-cz, B-exp joint = direct lmerTest Kenward-Roger fits")

# ---------------------------------------------------------------- 4. L-vector identities
rD <- s32i_run_job(getj("D_EXP|crossing_rate"), D); e <- rD$estimates
for (j in 1:4) check(near(e[estimand == paste0("SIS_mean_A", j), estimate] - e[estimand == paste0("CON_mean_A", j), estimate], e[estimand == paste0("SIS_minus_CON_A", j), estimate]),
                     paste("D: SIS mean - CON mean = SIS - CON at A", j))
check(near(e[estimand == "later_phase_level_SIS_minus_CON", estimate], mean(e[estimand %in% paste0("SIS_minus_CON_A", 2:4), estimate])), "D: later-phase level = mean of A2-A4 SIS - CON")
check(nrow(e) == 13L && all(is.na(e$p_raw)) && rD$info$n_obs == 12L * n_an, "D: 13 estimation-only rows on CC1-CC3 rows")
rDz <- s32i_run_job(getj("D_CZ|crossing_rate"), D); ez <- rDz$estimates
check(near(ez[estimand == "later_phase_level_CombZ_wb_slope", estimate], mean(ez[estimand %in% paste0("CombZ_wb_slope_A", 2:4), estimate])), "D (cz): later-phase slope = mean of A2-A4 slopes")
x1 <- s32i_rows(s32i_sel$D_ROWS(D)[Group != "CON"], "crossing_rate", TRUE)
fz <- lmerTest::lmer(as.formula(S32I_FORMULAS$D_CZ), data = x1, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
nd <- unique(x1[, .(Batch)])[, `:=`(c2 = 0, c3 = 0, CombZ_wb = 0, phase_f = factor(2, levels = 0:3))]
Xn <- model.matrix(reformulas::nobars(as.formula(S32I_FORMULAS$D_CZ))[-2], nd)
m_cc1 <- mean(Xn %*% lme4::fixef(fz))                                              # batch-balanced CC1 A3 mean at CombZ_wb = 0
nd2 <- copy(nd)[, c2 := 1]; nd3 <- copy(nd)[, c3 := 1]
m_all <- mean(c(m_cc1, mean(model.matrix(reformulas::nobars(as.formula(S32I_FORMULAS$D_CZ))[-2], nd2) %*% lme4::fixef(fz)),
                mean(model.matrix(reformulas::nobars(as.formula(S32I_FORMULAS$D_CZ))[-2], nd3) %*% lme4::fixef(fz))))
check(near(ez[estimand == "SIS_mean_A3_at_CombZ_wb_0", estimate], m_all, 1e-6), "D (cz): batch-balanced, CC1-CC3-averaged phase mean = the prediction average")
rDec <- s32i_run_job(getj("A_DEC|crossing_rate"), D); ed <- rDec$estimates
check(rDec$info$rank == 10L && near(ed[estimand == "RES_minus_SUS", estimate], ed[estimand == "RES_minus_CON", estimate] - ed[estimand == "SUS_minus_CON", estimate]),
      "A-dec: rank 10; RES - SUS = (RES - CON) - (SUS - CON)")
ok("D later-phase level and phase profile, A-dec decomposition")

# ---------------------------------------------------------------- 5. failure rule and KR fallback
bad <- s32i_fit("y ~ Batch + g_SIS + isCON + (1 | CageEpisodeID)", s32i_rows(s32i_sel$A1_CC1(D), "crossing_rate"), "rankfail", 8L)
check(s32i_failed(bad) && grepl("Rank-deficient", bad$failure_reason), "a rank-deficient design is FAILED (fit stopped)")
fr <- s32i_contrast(bad, list(g_SIS = 1), "x")
check(identical(fr$status, "FAILED") && is.na(fr$estimate) && is.na(fr$p_raw), "rows of a FAILED model carry no estimate")
real_fit <- mmm_ci_fit
assign("mmm_ci_fit", function(...) { m <- real_fit(...); m$info[, converged := FALSE]; m }, envir = globalenv())
nc <- s32i_fit(S32I_FORMULAS$A_EXP, s32i_rows(s32i_sel$A1_CC1(D), "crossing_rate"), "nonconv", 8L)
assign("mmm_ci_fit", real_fit, envir = globalenv())
check(s32i_failed(nc) && grepl("non-convergence", nc$failure_reason) && identical(s32i_contrast(nc, list(g_SIS = 1), "x")$status, "FAILED"),
      "non-convergence (engine converged = FALSE) makes the model FAILED")
mA <- s32i_fit(S32I_FORMULAS$A_EXP, s32i_rows(s32i_sel$A1_CC1(D), "crossing_rate"), "okfit", 8L)
check(inherits(tryCatch(s32i_contrast(mA, list(g_SISx = 1), "typo"), error = identity), "error"), "an L-vector naming a missing coefficient stops (code error, never a KR failure)")
satt <- s32i_contrast_satt(mA, list(g_SIS = 1), "SIS_minus_CON")
kr_warn <- s32i_kr(mA, function() { warning("Unable to compute Kenward-Roger t-test: using Satterthwaite instead"); mmm_ci_contrast(mA, list(g_SIS = 1), "SIS_minus_CON") },
                   function() s32i_contrast_satt(mA, list(g_SIS = 1), "SIS_minus_CON"), function() mmm_ci_failed_row(mA, "SIS_minus_CON", "contrast", weights = list(g_SIS = 1)))
check(isTRUE(kr_warn$ddf_fallback) && identical(kr_warn$df_method, "Satterthwaite") && near(kr_warn$df, satt$df) && near(kr_warn$estimate, satt$estimate) &&
      grepl("Unable to compute", kr_warn$kr_messages), "lmerTest's KR fallback warning -> Satterthwaite row flagged ddf_fallback")
kr_nan <- s32i_kr(mA, function() mmm_ci_contrast(mA, list(g_SIS = 1), "SIS_minus_CON")[, df := NaN], function() s32i_contrast_satt(mA, list(g_SIS = 1), "SIS_minus_CON"),
                  function() mmm_ci_failed_row(mA, "SIS_minus_CON", "contrast", weights = list(g_SIS = 1)))
check(isTRUE(kr_nan$ddf_fallback) && is.finite(kr_nan$df), "a non-finite KR df -> Satterthwaite fallback")
kr_both <- s32i_kr(mA, function() stop("pbkrtest failed"), function() stop("satt failed"), function() mmm_ci_failed_row(mA, "SIS_minus_CON", "contrast", weights = list(g_SIS = 1)))
check(identical(kr_both$status, "FAILED") && is.na(kr_both$estimate) && grepl("both failed", kr_both$failure_reason), "KR and Satterthwaite both failing -> FAILED row")
kr_ok <- s32i_contrast(mA, list(g_SIS = 1), "SIS_minus_CON")
check(isFALSE(kr_ok$ddf_fallback) && identical(kr_ok$df_method, "Kenward-Roger"), "a normal KR contrast is not flagged")
jn <- s32i_joint(mA, c("g_SIS", "g_SIS:sex_c"), "j")
check(identical(jn$status, "OK") && jn$df1 == 2 && isFALSE(jn$ddf_fallback), "joint KR F through the frozen engine")
nr <- s32i_run_job(getj("C_EXP_PHASExCC|crossing_rate"), D, status_of = c(`C_EXP|crossing_rate` = "FAILED"))
check(identical(nr$info$status, "NOT_RUN") && all(nr$estimates$status == "NOT_RUN") && grepl("requires an interpretable primary", nr$info$failure_reason),
      "C secondary is NOT_RUN when its primary model FAILED")
ok("failure rule, declared KR -> Satterthwaite fallback, NOT_RUN rule")

# ---------------------------------------------------------------- 6. the complete job list; Holm; sensitivities; reproduction helper
RA <- s32i_run_all(D, J)
check(nrow(RA$models) == length(J) && !anyDuplicated(RA$models$model_id), "one model row per registered job")
check(all(RA$models$status %in% c("OK", "FAILED", "NOT_RUN")) && sum(RA$models$status == "OK") >= length(J) - 3L,
      paste("synthetic fits: statuses", paste(RA$models[, .N, by = status][, paste(status, N)], collapse = ", ")))
check(!any(grepl("both failed", RA$estimates$failure_reason)), "no contrast failed for a code reason")
tested <- RA$estimates[tested == TRUE]
check(setequal(tested$hypothesis_id, c("H01", "H02", "H03", "H04", "H07", "H08", "H09", "H10")) && nrow(tested) == 8L &&
      identical(sort(RA$joints[tier == "primary", hypothesis_id]), c("H05", "H06")), "tested rows: H01-H04, H07-H10 (t) and H05, H06 (joint F)")
check(all(is.na(RA$estimates[tested == FALSE, p_raw])) && all(is.na(RA$estimates[tested == FALSE, statistic])), "every estimation-only / sensitivity row has no statistic and no p")
check(all(RA$estimates[model_id == "E_EXP|light_crossing_rate", n_obs] == 10L * n_an) && all(RA$estimates[grepl("^C_EXP[|]", model_id), n_obs] %in% c(14L * n_an, 14L * n_an - 1L)),
      "E uses L1-L3 CC1-CC3 + L1 CC4; C uses A1-A4 CC1-CC3 + A1-A2 CC4")
strict_n <- RA$models[model_id == "SENS_C_STRICT_EXP|crossing_rate", n_obs]
check(strict_n == 14L * n_an - nrow(D[CC == "CC2" & phase_label == "A4" & System == "sys.2"]), "strict sensitivity drops the strict-incomplete animal-phases")
hyp <- data.table(HypothesisID = sprintf("H%02d", 1:5), FamilyID = c("F1", "F1", "F2", "F2", "none"))
ho <- s32i_holm(hyp, c(H01 = 0.01, H02 = 0.04, H03 = NA, H04 = 0.03, H05 = 0.2))
check(near(ho$p_holm[1:2], p.adjust(c(0.01, 0.04), "holm")) && is.na(ho$p_holm[3]) && near(ho$p_holm[4], 0.06) && is.na(ho$p_holm[5]),
      "Holm per family of m = 2; an NA member keeps m = 2; the single test is unadjusted")
check(inherits(tryCatch(s32i_holm(hyp[1:3], c(H01 = 0.1, H02 = 0.2, H03 = 0.3)), error = identity), "error"), "a family with a member count other than 2 stops")
S <- s32i_sensitivities(RA$estimates, RA$joints)$contrasts
check(all(S[model_id == "SENS_C_STRICT_EXP|crossing_rate", ref_model_id] == "C_EXP|crossing_rate") && all(S[grepl("^SENS_B_COMMON", model_id), grepl("^B_EXP", ref_model_id)]) &&
      all(S[grepl("^SENS_A_COMMON", model_id), grepl("^A_EXP", ref_model_id)]) && all(is.finite(S[comparable == TRUE & status == "OK", primary_estimate])),
      "sensitivity rows find their primary-model reference")
check(all(is.na(S[estimand %in% c("A1_to_A2_change", "A2_to_A4_plateau_change"), shift_se])) && all(is.finite(S[estimand == "linear_trend_of_categorical" & status == "OK", shift_se])),
      "categorical-phase rows compared through the linear-trend contrast only")
pairs <- s32i_repro_pairs()
check(nrow(pairs) == 40L && pairs[gate == "A-exp = EXPOSURE_CC1", .N] == 8L && pairs[gate == "B-exp = EXPOSURE_TR", .N] == 32L, "reproduction pairs: 8 A-exp, 32 B-exp")
fz <- RA$estimates[model_id %in% c("A_EXP|crossing_rate", "B_EXP|crossing_rate") & estimand %in% c("SIS_minus_CON", "SC_sexavg_TR_CC2"),
                   .(model_id = sub("^A_EXP", "EXPOSURE_CC1", sub("^B_EXP", "EXPOSURE_TR", model_id)), estimand = ifelse(estimand == "SIS_minus_CON", "SC_sexavg_CC1", estimand),
                     estimate, se, df, ci_low, ci_high, sex = "")]
rp <- s32i_repro(RA$estimates, fz, pairs[model_id %in% c("A_EXP|crossing_rate", "B_EXP|crossing_rate") & estimand %in% c("SIS_minus_CON", "SC_sexavg_TR_CC2")])
check(nrow(rp) == 10L && all(rp$passed), "reproduction helper: identical rows pass")
fz2 <- copy(fz)[, estimate := estimate + 2e-6]
check(!any(s32i_repro(RA$estimates, fz2, pairs[model_id == "A_EXP|crossing_rate" & estimand == "SIS_minus_CON"])[column == "estimate", passed]), "reproduction helper: a 2e-6 shift fails")
ds <- s32i_descriptives(data.table(D)[, `:=`(phase = phase_label)])
check(all(c("group", "CON_cage_mean") %in% ds$level) && all(c("CON", "RES", "SUS", "SIS") %in% ds$Group) && "light_crossing_rate" %in% ds$metric, "descriptives: groups, pooled SIS, CON cage means, light rate")
ok(sprintf("complete job list (%d models) on synthetic data; Holm; sensitivities; reproduction helper; descriptives (%.1f min)", length(J),
           as.numeric(difftime(Sys.time(), T0, units = "mins"))))
cat("test_stage32_inference: all checks passed\n")
