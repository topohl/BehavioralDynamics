# Contract tests for the canonical inference engine's failure rule and p-value policy
# (Functions/rfid_canonical_inference.R; config fitting$failure_rule and sensitivities$p_value_policy).
#
# Synthetic data only: nothing here reads project data or sources an Analysis/ stage.
# Checks: an erroring fit becomes an explicit FAILED model instead of stopping the run; every downstream
# helper returns FAILED rows with no estimate or p; successful fits are unchanged; Holm keeps the declared m;
# D2 joint rows carry no p.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("rfid_canonical_inference.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

set.seed(20260928)
d <- data.table(AnimalNum = sprintf("A%02d", 1:48), Batch = factor(rep(c("B1", "B2", "B3"), each = 16)),
                CageEpisodeID = rep(sprintf("C%02d", 1:12), each = 4), g_RS = rep(c(0.5, -0.5), 24), sex_c = rep(c(0.5, 0.5, -0.5, -0.5), 12))
d[, y := 10 + as.integer(Batch) + 0.8 * g_RS + rnorm(12, sd = 0.7)[as.integer(factor(CageEpisodeID))] + rnorm(.N)]

# 1. a successful fit is unchanged and not FAILED
m <- mmm_ci_fit("y ~ Batch + g_RS + (1 | CageEpisodeID)", d, "ok_fit", expected_rank = 4)
check(!isTRUE(m$info$failed) && !is.null(m$fit), "a successful fit must not be FAILED")
ref <- lmerTest::lmer(y ~ Batch + g_RS + (1 | CageEpisodeID), data = d, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
ct <- lmerTest::contest(ref, c(0, 0, 0, 1), joint = FALSE, ddf = "Kenward-Roger")
x <- mmm_ci_contrast(m, list(g_RS = 1), "RS")
check(abs(x$estimate - ct$Estimate) < 1e-10 && abs(x$p_raw - ct$`Pr(>|t|)`) < 1e-10 && x$status == "OK", "successful contrast equals a direct lmerTest KR contrast")
ok("successful lmer fit and KR contrast unchanged")

# 2. an erroring lmer fit becomes an explicit FAILED model (missing grouping factor errors inside lmer, after the rank check)
mf <- mmm_ci_fit("y ~ Batch + g_RS + (1 | no_such_grouping)", d, "error_fit")
check(isTRUE(mf$info$failed) && is.null(mf$fit) && !is.na(mf$info$error) && nzchar(mf$info$error), "erroring lmer must return a FAILED stub with the error kept")
check(identical(mf$info$model_id, "error_fit") && mf$info$n_obs == 48, "FAILED stub keeps its registry fields")
ok("erroring lmer fit -> FAILED stub (error recorded)")

# 3. every downstream helper returns FAILED rows for it
xc <- mmm_ci_contrast(mf, list(g_RS = 1), "RS")
xj <- mmm_ci_joint(mf, c("g_RS"), "J")
check(xc$status == "FAILED" && is.na(xc$estimate) && is.na(xc$p_raw) && identical(names(xc), names(x)), "contrast on a FAILED model: FAILED row, same columns")
check(xj$status == "FAILED" && is.na(xj$F) && is.na(xj$p_raw), "joint test on a FAILED model: FAILED row")
cr <- mmm_ci_cr2(mf, "g_RS"); check(cr$status == "FAILED" && is.na(cr$p_raw), "CR2 on a FAILED model: FAILED row")
oc <- mmm_ci_optimizer_check(mf); check(is.na(oc$agree), "optimizer check on a FAILED model: NA agreement")
V <- mmm_ci_vcov_kr(mf, "g_RS"); check(all(is.na(V)), "KR covariance of a FAILED model is NA")
ha <- mmm_ci_het_A_q2b(mf, m, 30); check(ha$joint$status == "FAILED" && is.na(ha$joint$p_raw), "heteroscedastic A with a FAILED stratum: FAILED row")
ok("contrast, joint, CR2, optimizer check, KR covariance and comparator A all return FAILED rows")

# 4. glmmTMB errors are handled the same way
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  g <- mmm_ci_fit("y ~ Batch + g_RS + (1 | no_such_grouping)", d, "error_tmb", engine = "glmmTMB", dispformula = "~ sex_c")
  check(isTRUE(g$info$failed) && is.null(g$fit), "erroring glmmTMB must return a FAILED stub")
  tb <- mmm_ci_glmmtmb_tests(g, one = list(g_RS = 1), rows = "g_RS")
  check(tb$one$status == "FAILED" && is.na(tb$one$p_raw) && tb$joint$status == "FAILED", "glmmTMB tests on a FAILED model: FAILED rows")
  g2 <- mmm_ci_fit("y ~ Batch + g_RS + (1 | CageEpisodeID)", d, "ok_tmb", engine = "glmmTMB", dispformula = "~ sex_c")
  check(!isTRUE(g2$info$failed) && isTRUE(g2$info$converged), "a converged glmmTMB fit is not FAILED")
  ok("glmmTMB: erroring fit -> FAILED; converged fit unchanged")
}

# 5. Holm keeps the declared m when a member FAILED
p <- mmm_ci_holm(c(0.01, NA)); check(abs(p[1] - 0.02) < 1e-12 && is.na(p[2]), "Holm with a FAILED member keeps m = 2")
ok("Holm keeps the declared family size")

# 6. p-value policy: D2 joint rows carry F and df but no p
dy <- CJ(cage = sprintf("K%02d", 1:10), i = 1:4, j = 1:4)[i < j]
dy[, `:=`(A = paste0(cage, "_", i), B = paste0(cage, "_", j), CageEpisodeID = cage)]
dy[, s_RS := ifelse(i %% 2 == 1, 0.5, -0.5) + ifelse(j %% 2 == 1, 0.5, -0.5)]
dy[, sex_c := ifelse(as.integer(sub("K", "", cage)) <= 5, 0.5, -0.5)]
dy <- rbindlist(lapply(1:4, function(cc) copy(dy)[, `:=`(c2 = as.numeric(cc == 2), c3 = as.numeric(cc == 3), c4 = as.numeric(cc == 4),
                                                         CageEpisodeID = paste0(cage, "_cc", cc))]))
dy[, frac := 0.25 + 0.01 * s_RS + rnorm(.N, sd = 0.03)]
r2 <- mmm_ci_d2(dy, longitudinal = TRUE)
check(all(is.na(r2$p_raw)), "D2 rows (incl. the joint Q2b analogue) carry no p")
check(is.finite(r2[estimand == "Q2b_dyad_joint", F]), "D2 joint row keeps its F statistic")
ok("D2 joint row: F kept, p withheld (p_value_policy)")

cat("Canonical inference failure rule and p-value policy: PASS\n")
