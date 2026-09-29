# Contract test for the Stage 30 exploratory-screen helpers (Functions/stage30_screen.R).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data, a label list or an outcome table, and no
# Analysis/ stage is sourced. The frozen registry JSON (docs/stage30/stage30_registry_v1.0.json) is read as the
# specification only. Checks:
#   1. registry specification: 48 discovery + 16 POOL entries, families, focal terms, engines, measure mapping;
#   2. positional inactivity on hand-made runs (a run reaching the threshold counts in full; 39.9 s does not qualify);
#   3. recomputation comparison (tolerance, NA pattern), registry count parsers;
#   4. formula edits (Movement adjustment incl. the INT / L-POOL M:sex_c rule, Batch drop);
#   5. the three registered routes equal direct calls (CR2 Satterthwaite via clubSandwich, KR t, KR joint F);
#   6. failure rule: rank-deficient / expected-rank mismatch -> FAILED_RANK, erroring fit -> FAILED, never a stop;
#   7. BH with FAILED p = 1 and m unchanged; classification order D, A, B, C with every reason code;
#   8. DRY outcomes: label permutation within Batch x Sex preserves counts and is seeded; synthetic CombZ; CombZ_wb;
#   9. design gate (rank and CR2 df) and the placeholder outcomes;
#  10. the screen on a synthetic design shaped like Exp9 (6 batches, sex nested in batch, cookie B2-B6): statuses,
#      output schema, POOL without p, L components, declared sensitivities, LOBO rules (same-sex batches; Batch dropped
#      when one batch remains), influence, nested LOBO prediction, a broken entry does not stop the run;
#  11. Stage 29 carry-over selection and DRY masking (key columns only); run-mode stamp; group-blind select-list guard;
#      wording flags incl. the sex-specific permission (INT q, class A/B, direction);
#  12. labels from the sus/con lists, registered label counts, the label pre-flight (no CombZ) and the CombZ-only
#      outcome read (synthetic tables only);
#  13. KR fidelity: a forced lmerTest Kenward-Roger -> Satterthwaite fallback makes the fit FAILED (never reported as KR);
#  14. run-once and release helpers: REAL output-root conflicts (any commit), release path / manifest checks, package gate.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage30_screen.R            (committed helper)
#   MMM_STAGE30_SCREEN_HELPER=<path> Rscript ...          (a proposed copy)

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("rfid_canonical_inference.R")
.h <- Sys.getenv("MMM_STAGE30_SCREEN_HELPER"); if (nzchar(.h)) source(.h) else source_mmm_helper("stage30_screen.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
near <- function(a, b, tol = 1e-10) isTRUE(all(abs(a - b) <= tol))

# ---------------------------------------------------------------- 1. registry specification
reg <- jsonlite::fromJSON(file.path(MMM_REPO_ROOT, "docs", "stage30", "stage30_registry_v1.0.json"), simplifyVector = FALSE)
SPEC <- s30sc_spec(reg)
check(nrow(SPEC) == 64 && SPEC[role == "DISCOVERY", .N] == 48 && SPEC[role == "ESTIMATION_ONLY", .N] == 16, "64 entries: 48 discovery + 16 POOL")
check(all(attr(SPEC, "checks")), "every registry specification check passes")
check(identical(SPEC[role == "DISCOVERY", .N, by = family][order(family), N], c(3L, 3L, 9L, 15L, 6L, 6L, 6L)), "local family sizes (COOKIE-CAT 3, COOKIE-CONT 3, SCREEN-CONT 9, SCREEN-L 15, SLEEP-* 6)")
check(!anyNA(SPEC$measure_col) && all(SPEC[block == "inactivity", !is.na(sens_col) & !is.na(movadj_col)]) && all(SPEC[block == "cookie", sens_col == "dcookie45"]),
      "every measure mapped; inactivity and cookie entries carry their sensitivity and Movement columns")
check(identical(SPEC[id == "SCREEN-L-PCR-INT", focal_terms][[1]], c("CombZ_wb:c2:sex_c", "CombZ_wb:c3:sex_c", "CombZ_wb:c4:sex_c")) &&
      identical(SPEC[id == "SLEEP-CAT-IA40L-M", focal_terms][[1]], "g_RS") && identical(SPEC[id == "COOKIE-CONT-DC60-INT", focal_terms][[1]], "x:sex_c"), "focal terms")
check(all(SPEC[engine == "CR2", is.finite(expected_df_cr2)]) && SPEC[engine == "CR2", .N] == 24, "24 CR2 entries, each with expected_df_cr2")
bad <- copy(reg); bad$hypotheses[[1]]$measure <- "unknown measure"
check(grepl("without a column mapping", errmsg(s30sc_spec(bad))), "an unmapped measure stops the specification")
bad <- copy(reg); bad$multiplicity$local_families$`SCREEN-CONT` <- bad$multiplicity$local_families$`SCREEN-CONT`[-1]
check(grepl("families_equal", errmsg(s30sc_spec(bad))), "a family / hypothesis mismatch stops the specification")
ok("registry specification (48 + 16, families, focal terms, engines, measures)")

# ---------------------------------------------------------------- 2. positional inactivity on hand-made runs
runs <- data.table(AnimalNum = c(rep("A1", 4), "A2"), CC = "CC1", s = c(0, 39.9, 79.9, 179.9, 0), e = c(39.9, 79.9, 179.9, 200, 43200), pos = c(1L, 2L, 3L, 4L, 5L))
ia <- s30sc_inactivity(runs)
check(near(ia[AnimalNum == "A1", T_obs], 200) && near(ia[AnimalNum == "A1", frac40], 140 / 200, 1e-12) && near(ia[AnimalNum == "A1", frac60], 100 / 200, 1e-12),
      "a >= 40 s run counts in full, a 39.9 s run does not; denominator = observed seconds")
check(ia[AnimalNum == "A2", frac40 == 1 && frac60 == 1], "a zero-event window (one run) has fraction 1")
ok("positional inactivity (clip-then-qualify runs; full-interval scoring)")

# ---------------------------------------------------------------- 3. comparison and parsers
a <- data.table(k = 1:3, v = c(1, 2, NA), s = c("x", "y", "z")); b <- copy(a)
check(all(s30sc_compare(a, b, "k", c("v", "s"))$passed), "identical tables pass")
b[2, v := 2 + 2e-9]; check(!s30sc_compare(a, b, "k", "v")$passed, "a difference above 1e-9 fails")
b <- copy(a); b[3, v := 0]; check(!s30sc_compare(a, b, "k", "v")$passed, "a changed NA pattern fails")
b <- copy(a); b[1, s := "q"]; check(!s30sc_compare(a, b, "k", "s")$passed, "a changed text value fails")
check(identical(s30sc_parse_cage_sizes("19 x 4, 3 x 3, 2 x 1 (singletons OQ770 B1|sys.2|CC1)"), c(`4` = 19L, `3` = 3L, `1` = 2L)), "cage-size parser")
gc_ <- s30sc_parse_group_counts("RES 28, SUS 18 (B3 7/9, B4 9/6, B6 12/3)")
check(identical(gc_$Batch, c("B3", "B4", "B6")) && identical(gc_$RES, c(7L, 9L, 12L)) && identical(gc_$SUS, c(9L, 6L, 3L)), "group-count parser")
check(identical(s30sc_parse_batches("3 (B1, B2, B5)")$batches, c("B1", "B2", "B5")) && s30sc_parse_batches(6)$n == 6, "batch parser")
ok("recomputation comparison and registry count parsers")

# ---------------------------------------------------------------- 4. formulas
check(identical(s30sc_movadj_formula("CombZ ~ Batch + x", "CONT", "F"), "CombZ ~ Batch + M + x"), "CONT F: + M")
check(identical(s30sc_movadj_formula("CombZ ~ Batch + x + x:sex_c", "CONT", "INT"), "CombZ ~ Batch + M + M:sex_c + x + x:sex_c"), "CONT INT: + M + M:sex_c")
check(identical(s30sc_movadj_formula("y ~ Batch + g_RS + (1 | CageEpisodeID)", "CAT", "POOL"), "y ~ Batch + M + g_RS + (1 | CageEpisodeID)"), "CAT POOL: + M only")
lp <- SPEC[id == "SLEEP-L-IA40A-POOL", formula]; check(grepl("^y ~ Batch \\+ M \\+ M:sex_c \\+ CombZ_wb", s30sc_movadj_formula(lp, "L", "POOL")), "L POOL: + M + M:sex_c")
check(grepl("^y ~ Batch \\+ M \\+ CombZ_wb", s30sc_movadj_formula(SPEC[id == "SLEEP-L-IA40A-F", formula], "L", "F")), "L F: + M")
check(identical(s30sc_drop_batch("CombZ ~ Batch + x"), "CombZ ~ x") && !is.na(errmsg(s30sc_drop_batch("CombZ ~ x"))), "Batch drop")
check(identical(s30sc_fixed_formula("y ~ Batch + g_RS + g_RS:sex_c + (1 | CageEpisodeID)"), "y ~ Batch + g_RS + g_RS:sex_c"), "fixed part")
ok("formula edits")

# ---------------------------------------------------------------- synthetic Exp9-shaped data
set.seed(42)
BATCH_SEX <- c(B1 = "Male", B2 = "Male", B3 = "Female", B4 = "Female", B5 = "Male", B6 = "Female")
mk_batch <- function(b) {
  an <- data.table(AnimalNum = sprintf("%s_%02d", b, 1:20), Batch = b, Sex = BATCH_SEX[[b]], SIS = rep(c(TRUE, FALSE), c(16, 4)))
  rbindlist(lapply(1:4, function(cc) { x <- copy(an)
    x[SIS == TRUE, cage := sample(rep(1:4, each = 4))]; x[SIS == FALSE, cage := 5L]
    x[, `:=`(CC = paste0("CC", cc), cc = cc, System = paste0("sys.", cage))]
    x[, CageEpisodeID := paste(Batch, System, CC, sep = "|")] }))
}
W <- rbindlist(lapply(names(BATCH_SEX), mk_batch))
W[, `:=`(AnimalID = AnimalNum, sex_c = ifelse(Sex == "Female", 0.5, -0.5), c2 = as.numeric(cc == 2), c3 = as.numeric(cc == 3), c4 = as.numeric(cc == 4), cc1 = cc - 1)]
W[, u := rnorm(1), by = AnimalNum]
W[, `:=`(crossing_rate = exp(3.3 + 0.2 * u + rnorm(.N, 0, 0.2)), light_phase_crossing_rate = exp(1.5 + 0.2 * u + rnorm(.N, 0, 0.3)),
         occupancy_dispersion = 2 + 0.1 * u + rnorm(.N, 0, 0.2), fragmentation = plogis(-0.5 + 0.2 * u + rnorm(.N, 0, 0.3)), shared_zone_use = runif(.N, 0.1, 0.4))]
W[, `:=`(posinact40_active = plogis(2.8 - 0.03 * (crossing_rate - 27) + rnorm(.N, 0, 0.1)), posinact40_light = plogis(5.5 - 0.2 * (light_phase_crossing_rate - 4) + rnorm(.N, 0, 0.3)))]
W[, `:=`(posinact60_active = posinact40_active - runif(.N, 0.01, 0.03), posinact60_light = posinact40_light - runif(.N, 0, 0.003))]
W[AnimalNum %in% c("B1_01", "B1_02") & CC == "CC1", shared_zone_use := NA_real_]
cook <- unique(W[Batch != "B1" & CC == "CC4", .(AnimalNum, AnimalID, Batch, Sex, sex_c, SIS, CageEpisodeID = paste(Batch, System, sep = "|"))])
cook[, `:=`(PRE60 = rpois(.N, 5), PRE45 = rpois(.N, 5))][, `:=`(POST60 = PRE60 + rpois(.N, 20), POST45 = PRE45 + rpois(.N, 25))]
cook[, `:=`(dcookie60 = POST60 - PRE60, dcookie45 = POST45 - PRE45)]
cook <- merge(cook, W[CC == "CC1", .(AnimalNum, cc1_crossing_rate = crossing_rate)], by = "AnimalNum")
D0 <- list(W = W, K = cook)

# ---------------------------------------------------------------- 8. DRY outcomes
AN <- unique(W[SIS == TRUE, .(AnimalNum, Batch, Sex)]); AN <- merge(AN, W[SIS == TRUE & CC == "CC1", .(AnimalNum, cage_CC1 = CageEpisodeID)], by = "AnimalNum")
AN[, Group := rep(c("RES", "SUS"), length.out = .N), by = Batch]
p1 <- s30sc_permute_labels(AN[, .(AnimalNum, Batch, Sex, Group)], seed = 7L); p2 <- s30sc_permute_labels(AN[, .(AnimalNum, Batch, Sex, Group)], seed = 7L)
cnt0 <- AN[, .(nR = sum(Group == "RES")), by = .(Batch, Sex)][order(Batch)]; cnt1 <- p1[, .(nR = sum(g_RS > 0)), by = .(Batch, Sex)][order(Batch)]
check(identical(cnt0, cnt1), "label permutation preserves RES/SUS counts within Batch x Sex")
check(identical(p1, p2), "label permutation is seeded (identical on repeat)")
mm <- merge(AN[, .(AnimalNum, g0 = ifelse(Group == "RES", 0.5, -0.5))], p1[, .(AnimalNum, g_RS)], by = "AnimalNum")
check(any(mm$g0 != mm$g_RS), "labels are actually permuted")
cz <- s30sc_synthetic_combz(AN, seed = 8L); check(identical(cz, s30sc_synthetic_combz(AN, seed = 8L)) && nrow(cz) == nrow(AN) && all(is.finite(cz$CombZ)), "synthetic CombZ is seeded and complete")
AN <- merge(merge(AN[, !"Group"], p1[, .(AnimalNum, g_RS)], by = "AnimalNum"), cz, by = "AnimalNum")
AN <- s30sc_combz_wb(AN)
check(AN[, abs(mean(CombZ_wb)), by = Batch][, max(V1)] < 1e-12 && AN[, uniqueN(CombZ_wb) == .N], "CombZ_wb: batch-centred, one value per animal")
D <- s30sc_attach_outcomes(D0, AN[, .(AnimalNum, CombZ, CombZ_wb, g_RS)])
check(all(is.na(D$W[SIS == FALSE, CombZ])) && all(is.finite(D$W[SIS == TRUE, CombZ])) && all(is.finite(D$K[SIS == TRUE, g_RS])), "outcomes attach to SIS animals only")
ok("DRY outcomes (seeded permutation within Batch x Sex; synthetic CombZ; CombZ_wb)")

# ---------------------------------------------------------------- 5. routes equal direct calls
e <- SPEC[id == "SCREEN-CONT-OCC-F"]; d <- s30sc_hyp_data(D, e)
fr <- s30sc_fit(d, e$formula, "CR2", e$id, "x", expected_rank = 4L)
f0 <- lm(CombZ ~ Batch + x, data = d); V0 <- clubSandwich::vcovCR(f0, cluster = d$CageEpisodeID, type = "CR2")
c0 <- clubSandwich::coef_test(f0, vcov = V0, test = "Satterthwaite", coefs = "x")
check(fr$row$status == "OK" && near(fr$row$estimate, c0$beta) && near(fr$row$se, c0$SE) && near(fr$row$df, c0$df_Satt) && near(fr$row$p_raw, c0$p_Satt) &&
      near(fr$row$ci_low, c0$beta - qt(.975, c0$df_Satt) * c0$SE) && near(fr$row$ols_p, summary(f0)$coefficients["x", 4]), "CR2 route = clubSandwich Satterthwaite; OLS p alongside")
e <- SPEC[id == "SLEEP-CAT-IA40A-INT"]; d <- s30sc_hyp_data(D, e)
fr <- s30sc_fit(d, e$formula, "KR t", e$id, "g_RS:sex_c", expected_rank = 8L)
m0 <- lmerTest::lmer(y ~ Batch + g_RS + g_RS:sex_c + (1 | CageEpisodeID), data = d, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa"))
L <- setNames(as.numeric(names(lme4::fixef(m0)) == "g_RS:sex_c"), names(lme4::fixef(m0)))
k0 <- lmerTest::contest(m0, L, joint = FALSE, ddf = "Kenward-Roger")
check(fr$row$status == "OK" && near(fr$row$estimate, k0$Estimate, 1e-8) && near(fr$row$df, k0$df, 1e-6) && near(fr$row$p_raw, k0$`Pr(>|t|)`, 1e-8), "KR t route = lmerTest::contest (Kenward-Roger)")
e <- SPEC[id == "SCREEN-L-OCC-M"]; d <- s30sc_hyp_data(D, e)
fr <- s30sc_fit(d, e$formula, "KR F(3)", e$id, e$focal_terms[[1]], expected_rank = 10L)
m0 <- lmerTest::lmer(as.formula(e$formula), data = d, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa"))
nm <- names(lme4::fixef(m0)); LL <- t(sapply(e$focal_terms[[1]], function(r) as.numeric(nm == mmm_ci_match(r, nm))))
j0 <- lmerTest::contest(m0, LL, joint = TRUE, ddf = "Kenward-Roger")
check(fr$row$status == "OK" && near(fr$row$F, j0$`F value`, 1e-8) && near(fr$row$p_raw, j0$`Pr(>F)`, 1e-8) && nrow(fr$components) == 3 && all(fr$components$status == "OK"),
      "KR F(3) route = lmerTest::contest joint; three KR components")
fs <- s30sc_fit(d, e$formula, "KR F(3)", e$id, e$focal_terms[[1]], kr_se_only = TRUE)
check(near(fs$components$estimate, fr$components$estimate, 1e-10) && near(fs$components$se, fr$components$se, 1e-8), "KR-adjusted SE shortcut (LOBO) = KR contrast SEs")
ok("CR2, KR t and KR F(3) routes equal direct calls")

# ---------------------------------------------------------------- 6. failure rule
e <- SPEC[id == "SCREEN-CONT-OCC-F"]; d <- s30sc_hyp_data(D, e); d[, x := as.numeric(Batch)]
fr <- s30sc_fit(d, e$formula, "CR2", e$id, "x", expected_rank = 4L)
check(fr$row$status == "FAILED_RANK" && grepl("Rank-deficient", fr$row$error), "rank-deficient design -> FAILED_RANK (mmm_ci_fit stop caught)")
d <- s30sc_hyp_data(D, e); fr <- s30sc_fit(d, e$formula, "CR2", e$id, "x", expected_rank = 5L)
check(fr$row$status == "FAILED_RANK" && grepl("frozen expected rank", fr$row$error), "expected-rank mismatch -> FAILED_RANK")
fr <- s30sc_fit(d, "CombZ ~ Batch + no_such_column", "CR2", e$id, "no_such_column")
check(fr$row$status == "FAILED" && !is.na(fr$row$error), "an erroring fit -> FAILED, no stop")
e <- SPEC[id == "SLEEP-CAT-IA40A-F"]; d <- s30sc_hyp_data(D, e); d[, g_RS := 0.5]
fr <- s30sc_fit(d, e$formula, "KR t", e$id, "g_RS", expected_rank = 4L)
check(fr$row$status == "FAILED_RANK", "KR route: a constant label -> FAILED_RANK")
ok("failure rule (FAILED_RANK / FAILED, never a stop)")

# ---------------------------------------------------------------- 7. classification
cl <- s30sc_classify(status = c("FAILED", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "FAILED_RANK"),
                     p_raw = c(NA, 0.001, 0.001, 0.001, 0.02, 0.001, 0.001, 0.3, 0.01, 0.001, NA),
                     q_local = c(NA, 0.01, 0.01, 0.01, 0.2, 0.01, 0.01, 0.5, 0.03, 0.02, NA),
                     block = c("broad", "inactivity", "inactivity", "broad", "broad", "broad", "broad", "broad", "inactivity", "inactivity", "cookie"),
                     question = c("CONT", "CONT", "L", "CONT", "CONT", "CONT", "CONT", "CONT", "CAT", "L", "CAT"),
                     movadj_status = c(NA, "OK", "OK", NA, NA, NA, NA, NA, "FAILED", "OK", NA),
                     movadj_ci_includes_0 = c(NA, TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     movadj_joint_p = c(NA, NA, 0.2, NA, NA, NA, NA, NA, NA, 0.01, NA),
                     lobo_sign_stable = c(NA, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, NA),
                     any_sign_change = c(NA, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, NA))
check(identical(cl$reason_code, c("D-FIT", "D-MOVEMENT", "D-MOVEMENT", "A", "B-MULT", "B-ROBUST", "B-ROBUST", "C", "D-MOVEMENT", "A", "D-FIT")),
      "classification D (fit; Movement CI; Movement joint p; Movement fit failed), A, B-MULT, B-ROBUST (LOBO; SIGN_CHANGE), C")
check(grepl("Movement-adjusted model FAILED; D\\(i\\) not evaluable, treated conservatively", cl$reason_detail[9]) && grepl("convention C2", cl$reason_detail[6]),
      "declared conventions in reason_detail (D-MOVEMENT on a FAILED Movement fit; FAILED LOBO refit)")
cl2 <- s30sc_classify("OK", 0.001, 0.01, "broad", "CONT", NA, NA, NA, TRUE, NA)
check(identical(cl2$reason_code, "B-ROBUST"), "an unevaluable sensitivity block (any_sign_change NA) cannot give A")
q <- p.adjust(c(0.01, 1, 0.04), "BH"); check(near(q, c(0.03, 1, 0.06)), "BH with a FAILED member at p = 1, m unchanged")
ok("classification rules and multiplicity")

# ---------------------------------------------------------------- 9. design gate
ph <- s30sc_placeholder_outcomes(D0 <- list(W = W, K = cook), seed = 3L)
check(identical(ph, s30sc_placeholder_outcomes(D0, seed = 3L)) && ph[, max(abs(mean(CombZ_wb))), by = Batch][, max(V1)] < 1e-12, "placeholder outcomes seeded and batch-centred")
Dp <- s30sc_attach_outcomes(D0, ph)
g1 <- s30sc_design_gate(Dp, SPEC[id %in% c("SCREEN-CONT-OCC-F", "SLEEP-L-IA40A-INT", "COOKIE-CAT-DC60-M")])
check(all(g1$rank_ok) && !g1[engine == "CR2", df_ok], "design gate: synthetic ranks match; synthetic CR2 df differs from the Exp9 expected df")
sp <- copy(SPEC[id == "SCREEN-CONT-OCC-F"]); sp[, expected_df_cr2 := g1[id == "SCREEN-CONT-OCC-F", design_df_cr2]]
g2 <- s30sc_design_gate(Dp, sp); check(g2$passed, "design gate passes when expected_df_cr2 equals the design df")
ph2 <- copy(ph); ph2[, CombZ := CombZ * 3 + 1]; g3 <- s30sc_design_gate(s30sc_attach_outcomes(D0, ph2), sp)
check(near(g3$design_df_cr2, g2$design_df_cr2, 1e-9), "CR2 df_Satt does not depend on the outcome (design-fixed)")
sp[, expected_rank := 5L]; check(!s30sc_design_gate(Dp, sp)$rank_ok, "design gate detects a rank mismatch")
ok("design gate (rank; design-fixed CR2 df)")

# ---------------------------------------------------------------- 10. the screen on a synthetic design
ids <- c("SCREEN-CONT-OCC-F", "SCREEN-CONT-OCC-INT", "SCREEN-CONT-OCC-POOL", "SCREEN-L-PCR-F", "SCREEN-L-OCC-POOL", "SLEEP-CONT-IA40A-M",
         "SLEEP-CAT-IA40L-F", "SLEEP-CAT-IA40L-INT", "SLEEP-L-IA40A-INT", "COOKIE-CONT-DC60-M", "COOKIE-CAT-DC60-M", "COOKIE-CAT-DC60-POOL")
sp <- SPEC[id %in% ids]
broken <- copy(SPEC[id == "SCREEN-CONT-FRAG-M"]); broken[, measure_col := "not_a_column"]
sp <- rbind(sp, broken)
S <- s30sc_run_screen(D, sp, force_nested = TRUE, log = function(...) invisible(NULL))
M <- S$master; P <- S$pool
check(nrow(M) == 10 && nrow(P) == 3, "10 discovery rows and 3 POOL rows")
check(all(M[id != "SCREEN-CONT-FRAG-M", status] == "OK") && all(P$status == "OK"), "every well-formed entry fits OK")
check(M[id == "SCREEN-CONT-FRAG-M", status == "FAILED" && reason_code == "D-FIT" && p_for_multiplicity == 1] && nrow(S$failures) == 1, "a broken entry becomes FAILED (p = 1, D-FIT) and the run continues")
check(M[, all(family_m == .N), by = family][, all(V1)] && all(M$global_m == nrow(M)), "family m and global m are the realised counts")
check(all(c("p_raw", "q_local_bh", "q_global_bh_descriptive", "classification", "reason_code", "wording_flags", "lobo_sign_stable", "robustness_labels",
            "movadj_estimate", "movadj_joint_p", "calibration_verdict", "low_df_label", "df_cr2_matches_expected", "rank_matches_expected") %in% names(M)), "master schema")
check(!any(c("p_raw", "ols_p_descriptive", "q_local_bh", "statistic") %in% names(P)) && all(P$flag == "ESTIMATION_ONLY") && all(grepl("pooled across sexes \\(estimation only\\)", P$wording_flags)),
      "POOL rows: ESTIMATION_ONLY, no p, no test statistic, no classification")
check(M[question == "L", all(is.na(estimate) & is.finite(p_raw) & df1 == 3)] && S$components[id == "SCREEN-L-OCC-POOL", .N == 3 && all(is.na(statistic))], "L: joint F(3); POOL L: 3 components, no statistic")
check(S$components[id == "SCREEN-L-PCR-F", sum(is_max_abs_t) == 1] && M[id == "SCREEN-L-PCR-F", optimizer_check_run != "no"], "L component table; PCR optimizer check always run")
sv <- S$sensitivities
check(setequal(sv[id == "SLEEP-CAT-IA40L-F", unique(sensitivity_id)], c("POSINACT60", "MOVADJ")) && setequal(sv[id == "COOKIE-CONT-DC60-M", unique(sensitivity_id)], c("OLS_T", "DCOOKIE45", "ALT_KR", "MOVADJ")) &&
      setequal(sv[id == "COOKIE-CAT-DC60-M", unique(sensitivity_id)], c("DCOOKIE45", "ALT_CR2", "MOVADJ")) && setequal(sv[id == "SCREEN-CONT-OCC-F", unique(sensitivity_id)], "OLS_T"),
      "declared sensitivities per block")
check(sv[id == "SLEEP-L-IA40A-INT" & sensitivity_id == "MOVADJ", .N == 3 && sum(used_for_rule_D) == 1 && is.finite(joint_p_rule_D_only[used_for_rule_D])] &&
      grepl("M:sex_c", sv[id == "SLEEP-L-IA40A-INT" & sensitivity_id == "MOVADJ", formula][1]), "L Movement-adjusted: components + joint KR F p (rule D only), INT + M:sex_c")
check(all(sv[status == "OK", robustness_label %in% c("COMPATIBLE", "LARGE_SHIFT", "SIGN_CHANGE")]) && is.finite(M[id == "SLEEP-CONT-IA40A-M", movadj_estimate]), "robustness labels and Movement-adjusted estimate")
lb <- S$lobo
check(setequal(lb[id == "SCREEN-CONT-OCC-F", left_out_batch], c("B3", "B4", "B6")) && setequal(lb[id == "SCREEN-CONT-OCC-INT", left_out_batch], names(BATCH_SEX)),
      "LOBO: within-sex leaves out same-sex batches only; INT all batches")
check(lb[id == "COOKIE-CAT-DC60-M", .N == 2 && all(batch_term_dropped) && all(status == "OK")] && lb[id == "COOKIE-CONT-DC60-M", all(!grepl("Batch", formula))], "LOBO: one remaining batch -> Batch dropped")
check(lb[id == "SLEEP-L-IA40A-INT", .N == 18 && sum(is_classification_component) == 6] && M[id == "SLEEP-L-IA40A-INT", lobo_n_refits == 6 && nchar(lobo_signs) > 0], "LOBO for L: three components per refit; sign summary")
check(lb[id == "SCREEN-CONT-OCC-F", all(is.finite(df) & low_df_label %in% c("LOW_DF", "NOT_LOW_DF") & df_method == "Satterthwaite (CR2)")] &&
      lb[id == "COOKIE-CONT-DC60-M", all(low_df_label == ifelse(df < 4, "LOW_DF", "NOT_LOW_DF"))] &&
      lb[id == "SLEEP-CAT-IA40L-F", all(is.finite(df) & is.na(low_df_label))] && lb[id == "SLEEP-L-IA40A-INT", all(is.na(df) & is.na(low_df_label))],
      "LOBO: CR2 refits keep the Satterthwaite df and a LOW_DF label; KR t refits the KR df; L refits KR SE only")
check(M[id == "SLEEP-L-IA40A-INT", length(strsplit(lobo_signs_by_component, "; ")[[1]]) == 3 && grepl("CombZ_wb:c2:sex_c B1:", lobo_signs_by_component) &&
        grepl("CombZ_wb:c4:sex_c ", lobo_leverage_batches_by_component)] && M[id == "SCREEN-CONT-OCC-F", lobo_signs_by_component == paste("x", lobo_signs)],
      "LOBO master summary per component (L: all three components) and for single-focal rows")
check(all(c("int_q_local_same_cell", "sex_specific_direction_consistent", "sex_specific_wording_allowed", "sex_specific_basis", "fit_messages") %in% names(M)) &&
      M[sex_analysis %in% c("F", "M"), all(!sex_specific_wording_allowed | (int_q_local_same_cell < 0.05 & classification %in% c("A", "B") & sex_specific_direction_consistent))] &&
      M[id == "SCREEN-L-PCR-F", is.na(int_q_local_same_cell) && isFALSE(sex_specific_wording_allowed)] && M[sex_analysis == "INT", all(is.na(sex_specific_wording_allowed))],
      "sex-specific permission columns (no INT in the run -> not allowed; INT rows NA)")
inf <- S$influence
check(inf[id == "SCREEN-CONT-OCC-F", method == "stats::dfbeta (lm), scaled by the primary CR2 SE" & is.finite(max_abs_dfbeta_over_se)] &&
      inf[id == "SLEEP-L-IA40A-INT", .N == 3 && all(n_units == 96 & is.finite(max_abs_dfbeta_over_se))], "influence: dfbeta (lm) and case deletion by animal (lmer)")
check(setequal(S$nested$id, c("COOKIE-CONT-DC60-M", "COOKIE-CAT-DC60-M")) && S$nested[, .N] == 4 && all(!S$nested$triggered_by_class_A[M[match(S$nested$id, id), classification] != "A"]), "nested LOBO prediction (forced code exercise) on the cookie hypotheses")
check(S$nested[id == "COOKIE-CAT-DC60-M", all(auc_res_vs_sus_heldout >= 0 & auc_res_vs_sus_heldout <= 1)] && S$nested[id == "COOKIE-CONT-DC60-M", all(is.na(auc_res_vs_sus_heldout))] &&
      near(s30sc_auc(c(3, 2, 1, 1), c(0.5, 0.5, -0.5, -0.5)), 1) && near(s30sc_auc(c(1, 1), c(0.5, -0.5)), 0.5) && is.na(s30sc_auc(1:3, rep(0.5, 3))),
      "nested prediction: held-out within-batch AUC for CAT only (ties count 1/2)")
check(all(M$classification %in% c("A", "B", "C", "D")) && all(nzchar(M$reason_code)), "every discovery row is classified with a reason code")
check(M[id == "SLEEP-CAT-IA40L-F", grepl("LIGHT_PHASE_SATURATED", wording_flags) && grepl("in females", wording_flags)] &&
      M[id == "COOKIE-CAT-DC60-M", grepl("locomotor challenge-response", wording_flags)], "wording: light-phase saturation, 'in females', cookie phenotype")
eb <- copy(SPEC[id == "COOKIE-CAT-DC60-M"]); eb[, formula := "y ~ g_RS + Batch + (1 | CageEpisodeID)"]
xb <- s30sc_run_entry(D, eb)
check(xb$primary$row$status == "OK" && any(grepl("^LOBO:", xb$block_errors)) && isFALSE(xb$lobo$summary$lobo_sign_stable) && nrow(xb$sens) > 0,
      "a post-primary block error (LOBO) is recorded, keeps the primary result and blocks class A")
ok("screen on a synthetic Exp9-shaped design (statuses, schema, POOL, L, sensitivities, LOBO, influence, nested prediction)")

# ---------------------------------------------------------------- 11. carry-over, stamp, guard, wording
td <- file.path(tempdir(), "s30sc_carry"); dir.create(td, showWarnings = FALSE)
k4 <- c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation")
est <- rbindlist(lapply(k4, function(k) rbind(
  data.table(model_id = paste0("CC1_POOLED|", k), estimand = c("Q1", "RS_sexavg_CC1", "Q2c"), construct = k, sex = "", estimate = 1, se = 0.5, df = 20, ci_low = 0, ci_high = 2, p_raw = c(0.3, NA, NA), test = "KR t"),
  data.table(model_id = paste0("CC1_BY_SEX|", k, "|", c("Female", "Male")), estimand = "RS_by_sex_CC1", construct = k, sex = c("Female", "Male"), estimate = 1, se = 0.5, df = 20, ci_low = 0, ci_high = 2, p_raw = 0.4, test = "KR t"))))
fwrite(est, file.path(td, "estimates.csv"))
fwrite(data.table(population = rep(c("ALL_RFID", "SIS_ONLY"), each = 12), predictor = rep(rep(c("crossing_rate", "shared_zone_use", "fragmentation"), each = 4), 2),
                  estimand = c("slope_sexavg", "slope_DiD_F_minus_M", "slope_Female", "slope_Male"), estimate = 1, se = 1, df = 80, ci_low = -1, ci_high = 3, se_cr2 = 1, df_cr2 = 9, n = 87, p_raw = NA, role = "x"),
       file.path(td, "continuous_estimates.csv"))
fwrite(data.table(model_id = c("LIGHT_CC1_POOLED", "LIGHT_CC1_POOLED", "LIGHT_CC1_BY_SEX|Female", "LIGHT_CC1_BY_SEX|Male", "LIGHT_TR_BY_SEX|Male"),
                  estimand = c("RS_sexavg_CC1_light", "Q1_light", "RS_by_sex_CC1_light", "RS_by_sex_CC1_light", "RS_by_sex_TR_CC2_light"), construct = "light_phase_crossing_rate",
                  sex = c("", "", "Female", "Male", "Male"), estimate = 2, se = 1, df = 30, ci_low = 0, ci_high = 4, p_raw = NA, test = "KR t"), file.path(td, "exposure_estimates.csv"))
fwrite(data.table(family_id = c("P-CC1", "FU-CC1|crossing_rate"), member = c("Q1|crossing_rate", "RS_by_sex_CC1|crossing_rate|Female"), p_adjusted = c(0.6, 0.8)), file.path(td, "multiplicity.csv"))
co <- s30sc_carry_over(td, mask_values = FALSE); cm <- s30sc_carry_over(td, mask_values = TRUE)
check(co[question == "CONT", .N] == 16 && co[question == "CAT", .N] == 20 && co[question == "CAT", uniqueN(construct)] == 5 && all(co$status == "CARRY_OVER"),
      "carry-over: CONT crossing_rate + shared_zone_use (16 rows), CAT all 5 metrics (20 rows), status CARRY_OVER")
check(co[member == "Q1|crossing_rate", stage29_p_adjusted_holm == 0.6] && all(is.finite(co$estimate)) && all(is.na(cm$estimate)) && all(is.na(cm$p_raw)) && all(cm$values_masked),
      "carry-over values as exported (with Stage 29 Holm p); DRY masking withholds every value")
tk <- file.path(tempdir(), "s30sc_carry_keys"); dir.create(tk, showWarnings = FALSE)   # key columns only: the DRY path never needs a value column
for (f in names(S30SC_CARRY_KEY_COLS)) fwrite(fread(file.path(td, f), select = S30SC_CARRY_KEY_COLS[[f]]), file.path(tk, f))
ck <- s30sc_carry_over(tk, mask_values = TRUE)
check(isTRUE(all.equal(ck, cm, check.attributes = FALSE)) && all(is.na(ck$estimate)) && all(is.na(ck$stage29_p_adjusted_holm)),
      "DRY carry-over reads only the key columns (key-only files give the identical masked table)")
unlink(tk, recursive = TRUE)
f <- s30sc_write(data.table(a = 1, l = list(c("x", "y"))), td, "w.csv", S30SC_RUN_MODE_DRY); w <- fread(f)
check(identical(names(w)[1], "run_mode") && w$run_mode == "DRY_RUN_NOT_RESULTS" && w$l == "x; y", "run-mode stamp first; list columns flattened")
check(grepl("blindness violated", errmsg(s30sc_assert_blind(c("AnimalNum", "Group"), "test"))) && grepl("CombZ", errmsg(s30sc_assert_blind(c("CombZ"), "t"))) &&
      isTRUE(s30sc_assert_blind(S30SC_REF_COLS, "ref")) && isTRUE(s30sc_assert_blind(S30SC_COOKIE_ANIMAL_COLS, "ck")), "group-blind select-list guard")
eF <- SPEC[id == "SLEEP-CONT-IA40A-F"]; eM <- SPEC[id == "SLEEP-CONT-IA40A-M"]
wf <- s30sc_wording(eF, "D-MOVEMENT", FALSE, 0.01, FALSE)
check(grepl("INT q < 0.05 but this sex's association is not supported", wf) && !grepl("specific' allowed", wf) && grepl("never describe as independent sleep biology", wf),
      "wording: INT q < 0.05 but the row not supported -> no sex-specific wording; D-MOVEMENT flag")
check(grepl("'female-specific' allowed, in the direction of this row only", s30sc_wording(eF, "A", FALSE, 0.01, TRUE)) &&
      grepl("'male-specific' allowed", s30sc_wording(eM, "B-MULT", FALSE, 0.02, TRUE)), "wording: sex-specific allowed only when the permission is TRUE")
check(grepl("no 'female-specific'", s30sc_wording(eF, "C", TRUE, 0.2)) && grepl("LOW_DF", s30sc_wording(eF, "C", TRUE, 0.2)) &&
      !grepl("allowed", s30sc_wording(eF, "A", FALSE, 0.2, TRUE)), "wording: no sex-specific wording with INT q >= 0.05 (even if a permission were passed); LOW_DF flag")
# permission: INT = F - M (sex_c = +1/2 F, -1/2 M) -> F: same sign as INT; M: opposite sign; INT q < 0.05; row A or B
ss <- s30sc_sex_specific_allowed(sex_analysis = c("F", "F", "M", "M", "F", "F", "M", "INT", "F"),
                                 classification = c("A", "B", "B", "A", "C", "D", "A", "A", "A"),
                                 estimate_row = c(0.4, -0.4, -0.3, 0.3, 0.4, 0.4, -0.3, 0.5, 0.4),
                                 estimate_int = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                                 int_ok = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), int_q = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.2))
check(identical(ss, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, FALSE)),
      "sex-specific permission: direction (F same sign, M opposite sign), class A/B only, INT OK and q < 0.05; INT rows NA")
ok("carry-over selection and masking; run-mode stamp; blindness guard; wording flags")
unlink(td, recursive = TRUE)

# ---------------------------------------------------------------- 12. label lists and the REAL outcome join (synthetic tables)
an <- data.table(AnimalNum = c("1", "2", "3", "4", "5", "6"), Batch = c("B3", "B3", "B3", "B2", "B2", "B2"), Sex = c(rep("Female", 3), rep("Male", 3)))
lb <- s30sc_labels_from_lists(an, sus = c("2", "5"), con = "9")
check(identical(lb$Group, c("RES", "SUS", "RES", "RES", "SUS", "RES")) && identical(s30sc_labels_from_lists(an, "2", "1")$Group[1], "CON"), "Group from the lists: CON > SUS > RES")
pops <- list(SIS_CC1 = list(res_sus_counts_by_batch = list(B2 = "2/1", B3 = "2/1", total = "4/2", note = "x")), COOKIE_SIS = list(group_counts = "RES 4, SUS 2"),
             COOKIE_SIS_F = list(group_counts = "RES 2, SUS 1 (B3 2/1)"), COOKIE_SIS_M = list(group_counts = "RES 2, SUS 1 (B2 2/1)"))
lc <- s30sc_label_count_checks(lb, an[, .(AnimalNum, Batch, Sex)], pops)
check(nrow(lc) == 5 && all(lc$passed), "registered label counts reproduced")
pops$COOKIE_SIS_M$group_counts <- "RES 1, SUS 2 (B2 1/2)"
check(!all(s30sc_label_count_checks(lb, an[, .(AnimalNum, Batch, Sex)], pops)$passed), "a count mismatch is detected")
anr <- lb[, .(AnimalNum, Batch, Sex, g_RS = ifelse(Group == "RES", 0.5, -0.5))]
# label pre-flight (before outcome contact): every RFID animal incl. the con-list animal "9"; never a CombZ column
an_all <- s30sc_labels_from_lists(rbind(an, data.table(AnimalNum = "9", Batch = "B2", Sex = "Male")), sus = c("2", "5"), con = "9")
labt <- data.table(AnimalNum = an_all$AnimalNum, Sex = an_all$Sex, Batch = c(3L, 3L, 3L, 2L, 2L, 2L, 2L), outcome_group = an_all$Group)
lt <- s30sc_label_table_checks(an_all[, .(AnimalNum, Batch, Sex, Group)], labt)
check(nrow(lt) == 5 && all(lt$passed), "label pre-flight: unique, complete, Sex/Batch agree, outcome_group = list label, con animals CON")
check(grepl("never CombZ", errmsg(s30sc_label_table_checks(an_all, cbind(labt, CombZ = 0)))), "label pre-flight refuses a table carrying CombZ")
z <- copy(labt); z[2, outcome_group := "RES"]; check(!s30sc_label_table_checks(an_all, z)[grepl("outcome_group", gate), passed], "pre-flight: outcome_group disagreeing with the lists fails")
z <- copy(labt)[-4]; check(!s30sc_label_table_checks(an_all, z)[grepl("every RFID animal present", gate), passed], "pre-flight: an animal absent from the table fails")
z <- copy(labt); z[1, Sex := "Male"]; check(!s30sc_label_table_checks(an_all, z)[grepl("Sex and Batch", gate), passed], "pre-flight: a Sex disagreement fails")
z <- copy(labt); z[7, outcome_group := "RES"]; check(!s30sc_label_table_checks(an_all, z)[grepl("con-list animals", gate), passed], "pre-flight: a con-list animal not CON fails")
z <- rbind(labt, labt[1]); check(!s30sc_label_table_checks(an_all, z)[grepl("unique", gate), passed], "pre-flight: a duplicated AnimalNum fails")
# outcome read (after the pre-flight): exactly AnimalNum + CombZ
czt <- data.table(AnimalNum = c(an$AnimalNum, "9"), CombZ = c(rnorm(6), NA))
ro <- s30sc_real_outcome(anr, czt)
check(all(ro$checks$passed) && nrow(ro$an) == 6 && all(is.finite(ro$an$CombZ)), "REAL outcome read: complete for every SIS animal (CON CombZ may be NA)")
check(grepl("exactly AnimalNum and CombZ", errmsg(s30sc_real_outcome(anr, cbind(czt, outcome_group = "x")))), "outcome read selects exactly AnimalNum and CombZ")
z <- copy(czt); z[3, CombZ := NA]; check(!s30sc_real_outcome(anr, z)$checks[grepl("finite", gate), passed], "REAL: a missing CombZ fails")
z <- copy(czt)[-4]; check(!s30sc_real_outcome(anr, z)$checks[grepl("finite", gate), passed], "REAL: an animal absent from the CombZ table fails")
z <- rbind(czt, czt[1]); check(!s30sc_real_outcome(anr, z)$checks[grepl("unique", gate), passed], "REAL: a duplicated AnimalNum fails")
ok("label lists, registered counts, label pre-flight (no CombZ) and the CombZ-only outcome read (synthetic tables)")

# ---------------------------------------------------------------- 13. KR fidelity: forced Kenward-Roger -> Satterthwaite fallback
e <- SPEC[id == "SLEEP-CAT-IA40A-INT"]; d <- s30sc_hyp_data(D, e)
eL <- SPEC[id == "SCREEN-L-OCC-M"]; dL <- s30sc_hyp_data(D, eL)
ns <- asNamespace("pbkrtest"); orig <- list(Lb_ddf = get("Lb_ddf", ns), KRmodcomp = get("KRmodcomp", ns))
utils::assignInNamespace("Lb_ddf", function(...) stop("forced KR failure (test)"), ns = "pbkrtest")
utils::assignInNamespace("KRmodcomp", function(...) stop("forced KR failure (test)"), ns = "pbkrtest")
res13 <- tryCatch({
  m0 <- lmerTest::lmer(y ~ Batch + g_RS + g_RS:sex_c + (1 | CageEpisodeID), data = d, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa"))
  L0 <- setNames(as.numeric(names(lme4::fixef(m0)) == "g_RS:sex_c"), names(lme4::fixef(m0)))
  w0 <- character(); withCallingHandlers(lmerTest::contest(m0, L0, joint = FALSE, ddf = "Kenward-Roger"), warning = function(w) { w0 <<- c(w0, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(w0 = w0, t = s30sc_fit(d, e$formula, "KR t", e$id, "g_RS:sex_c", expected_rank = 8L),
       Fj = s30sc_fit(dL, eL$formula, "KR F(3)", eL$id, eL$focal_terms[[1]], expected_rank = 10L))
}, finally = { utils::assignInNamespace("Lb_ddf", orig$Lb_ddf, ns = "pbkrtest"); utils::assignInNamespace("KRmodcomp", orig$KRmodcomp, ns = "pbkrtest") })
check(any(grepl(S30SC_KR_FALLBACK_PATTERN, res13$w0, fixed = TRUE)), "test mechanism: with pbkrtest disabled, lmerTest::contest falls back to Satterthwaite with a warning only")
check(identical(res13$t$row$status, "FAILED") && grepl("KR unavailable: lmerTest fell back to Satterthwaite", res13$t$row$error) && is.na(res13$t$row$p_raw) && is.na(res13$t$row$df),
      "KR t: the fallback makes the fit FAILED (no Satterthwaite p or df reported)")
check(grepl("KR unavailable", res13$t$row$error) && grepl("Unable to compute Kenward-Roger t-test", res13$t$row$fit_messages), "KR t: error names the fallback; the warning is kept in fit_messages")
check(res13$Fj$row$status == "FAILED" && grepl("KR unavailable", res13$Fj$row$error) && is.null(res13$Fj$components), "KR F(3): the fallback (components) makes the fit FAILED; no partial components kept")
check(identical(get("Lb_ddf", ns), orig$Lb_ddf) && identical(get("KRmodcomp", ns), orig$KRmodcomp) &&
      s30sc_fit(d, e$formula, "KR t", e$id, "g_RS:sex_c", expected_rank = 8L)$row$status == "OK", "pbkrtest restored; the same fit is OK again")
M13 <- s30sc_classify("FAILED", NA, 1, "inactivity", "CAT", NA, NA, NA, NA, NA)
check(M13$reason_code == "D-FIT", "a KR-fallback FAILED primary is D-FIT (enters multiplicity with p = 1)")
ok("KR fidelity: a Kenward-Roger -> Satterthwaite fallback is FAILED, never reported as KR")

# ---------------------------------------------------------------- 14. run-once / release / package helpers
tr <- file.path(tempdir(), "s30sc_outroot"); dir.create(tr, showWarnings = FALSE)
check(!length(s30sc_real_out_conflicts(file.path(tr, "absent"))) && !length(s30sc_real_out_conflicts(tr)), "no conflict in an absent / empty output root")
dir.create(file.path(tr, "v0.9_old")); dir.create(file.path(tr, "notes")); check(!length(s30sc_real_out_conflicts(tr)), "unrelated entries do not block")
dir.create(file.path(tr, ".tmp_v1.0_abc1234")); check(identical(s30sc_real_out_conflicts(tr), ".tmp_v1.0_abc1234"), "an aborted REAL staging folder (any commit) blocks")
dir.create(file.path(tr, "v1.0_def5678")); check(setequal(s30sc_real_out_conflicts(tr), c(".tmp_v1.0_abc1234", "v1.0_def5678")), "an earlier REAL output at another commit blocks")
unlink(tr, recursive = TRUE)
check(identical(s30sc_release_commit("S:/x/analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507"), "b2ce507") &&
      identical(s30sc_release_commit("S:\\x\\analysis_ready\\pipeline\\29_canonical_behavior_releases\\v101_dv2_b2ce507\\"), "b2ce507") &&
      is.na(s30sc_release_commit("C:/ev/cage_fix_rerun/out")) && is.na(s30sc_release_commit("S:/x/analysis_ready/pipeline/29_canonical_behavior_releases/v100_dv1_0555c90")) &&
      is.na(s30sc_release_commit("S:/x/analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_B2CE507")), "release folder path -> commit7 (v101_dv2 only)")
mf <- data.table(file = c(names(S30SC_S29_TABLE_SHA256), "other.csv"), bytes = 1, sha256 = c(unname(S30SC_S29_TABLE_SHA256), "x"))
check(s30sc_release_manifest_check(mf)$passed && !s30sc_release_manifest_check(mf[-2])$passed &&
      !s30sc_release_manifest_check(copy(mf)[1, sha256 := "0"])$passed && !s30sc_release_manifest_check(rbind(mf, mf[1][, sha256 := "0"]))$passed,
      "release output_manifest check: every carried table listed once with the gated SHA-256")
pg <- s30sc_package_gate("lme4 2.0.1; lmerTest 3.2.1; pbkrtest 0.5.5; glmmTMB 1.1.14; clubSandwich 0.7.0; Matrix 1.7.5; reformulas 0.4.4", "R version 4.5.1 (x)",
                         r_now = "R version 4.5.1 (x)", version_of = function(p) c(lme4 = "2.0.1", lmerTest = "3.2.1", pbkrtest = "0.5.5", clubSandwich = "0.7.0", Matrix = "1.7.5", reformulas = "0.4.4")[[p]])
check(all(pg$passed) && nrow(pg) == 7, "package gate: identical versions pass")
pg2 <- s30sc_package_gate("lme4 2.0.1; lmerTest 3.2.1", "R version 4.5.1 (x)", r_now = "R version 4.5.2 (y)", version_of = function(p) "2.0.1")
check(!pg2[item == "R", passed] && pg2[item == "lme4", passed] && !pg2[item == "pbkrtest", passed] && !any(s30sc_package_gate(NA, NA)$passed),
      "package gate: an R / package difference or a missing release entry fails")
check(length(S30SC_DRIVER_CONVENTIONS) >= 9 && all(grepl("^C[0-9] ", S30SC_DRIVER_CONVENTIONS)), "driver conventions declared")
ok("run-once, release-folder and package-version helpers")
cat("test_stage30_screen: all checks passed\n")
