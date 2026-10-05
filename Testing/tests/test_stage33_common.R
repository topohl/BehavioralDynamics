# Stage 33 shared helpers (Functions/stage33_common.R): synthetic checks only.
#
#   1. resampling indices: declared order, one seed per stream, reproducible, multiplicities;
#   2. CR2: closed form = clubSandwich for a clustered mean; CR1; CR2-Welch; contrasts without p-values;
#   3. frozen-engine KR wrapper drops p and statistic; CR2 on the lmer fit by any cluster;
#   4. moment ICC, residual SD, per-SD scale, gate rows, outcome-free assertion, estimate rows.
# Portable: no S: drive, no live data; sources Functions/ only.

suppressPackageStartupMessages({ library(data.table) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
must_error <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")

for (f in c("Functions/rfid_canonical_inference.R", "Functions/stage32_inference.R", "Functions/stage33_common.R")) source(f)

cat("1. resampling\n")
cells <- c(B1_SIS = 4L, B1_CON = 1L, B2_SIS = 4L)
r1 <- s33_resample_index(cells, B = 50L, seed = 33010101L)
r2 <- s33_resample_index(cells, B = 50L, seed = 33010101L)
check(identical(r1, r2), "same seed, same indices")
check(identical(names(r1), names(cells)) && all(vapply(r1, nrow, 1L) == 50L), "one B x G matrix per cell in the declared order")
check(all(r1$B1_CON == 1L), "a one-cage cell always draws its cage")
set.seed(33010101L); manual <- matrix(sample.int(4L, 4L * 50L, replace = TRUE), nrow = 50L, byrow = TRUE)
check(identical(r1$B1_SIS, manual), "first cell = matrix(sample.int(G, G*B, replace = TRUE), nrow = B, byrow = TRUE) after set.seed")
U <- s33_multiplicity(r1$B1_SIS, 4L)
check(all(rowSums(U) == 4L) && ncol(U) == 4L, "multiplicities count whole cages")
S <- c(10, 20, 30, 40); n <- c(4, 4, 3, 4)
m <- s33_resampled_means(U, S, n)
check(isTRUE(all.equal(m[1], sum(U[1, ] * S) / sum(U[1, ] * n))), "resampled mean = (U S) / (U n)")
check(must_error(s33_resample_index(c(a = 0L), 10L, 1L)), "an empty cell is refused")
ok("indices reproducible and in the declared order; multiplicities and resampled means")

cat("\n2. CR2 and companions\n")
set.seed(1); g <- rep(1:6, times = c(4, 4, 3, 4, 2, 4)); y <- rnorm(length(g)) + g / 3
cr <- s33_cr2_mean(y, g)
check(isTRUE(all.equal(cr$se^2, s33_cr2_mean_closed(y, g), tolerance = 1e-10)), "closed-form CR2 variance = clubSandwich")
check(cr$ci_low < cr$estimate && cr$estimate < cr$ci_high && cr$n_clusters == 6L, "CR2 mean interval")
check(is.na(s33_cr2_mean(y[g <= 2], g[g <= 2])$se), "fewer than 3 clusters: no interval")
c1 <- s33_cr1_mean(y, g); check(isTRUE(all.equal(c1$df, 5)), "CR1 uses t(G - 1)")
w <- s33_cr2_welch(y[g <= 3], g[g <= 3], y[g > 3], g[g > 3])
check(is.finite(w$se) && isTRUE(all.equal(w$estimate, mean(y[g <= 3]) - mean(y[g > 3]))), "CR2-Welch difference of clustered means")
d <- data.frame(y = y, x = rnorm(length(g)), g = g)
fit <- lm(y ~ x, d)
lc <- s33_cr2_contrast(fit, d$g, list(c(x = 1)))
check(!any(c("p_val", "p_Satt", "t_stat") %in% names(lc)) && isTRUE(all.equal(lc$estimate, unname(coef(fit)["x"]))), "CR2 contrast without p-value")
check(must_error(s33_cr2_contrast(fit, d$g, list(c(z = 1)))), "a contrast naming an absent coefficient stops")
ok("closed form, CR1, CR2-Welch and CR2 contrasts")

cat("\n3. frozen engine wrappers\n")
set.seed(2); dd <- data.table(AnimalNum = sprintf("A%02d", 1:48), Batch = factor(rep(c("B1", "B2", "B3"), each = 16)),
                              CageEpisodeID = rep(sprintf("c%02d", 1:12), each = 4))
dd[, x := rnorm(.N)][, y := 0.5 * x + rnorm(.N) + rep(rnorm(12, sd = 0.5), each = 4)]
m <- s33_kr_fit("y ~ Batch + x + (1 | CageEpisodeID)", dd, "t1", expected_rank = 4L)
kc <- s33_kr_contrast(m, list(x = 1), "slope")
check(!any(c("p_raw", "statistic") %in% names(kc)) && kc$interval_method %in% c("KR", "Satterthwaite_fallback"), "KR wrapper drops p and statistic")
check(kc$ci_low < kc$estimate && kc$estimate < kc$ci_high, "KR interval")
check(must_error(s33_kr_contrast(m, list(nope = 1), "bad")), "an absent coefficient stops")
cr <- s33_kr_fit_cr2(m, list(x = 1), "CageEpisodeID")
check(isTRUE(all.equal(cr$estimate, kc$estimate)) && cr$n_clusters == 12L, "CR2 on the frozen-engine lmer fit")
mf <- s33_kr_fit("y ~ Batch + x + (1 | CageEpisodeID)", dd, "t2", expected_rank = 9L)
check(s32i_failed(mf) && s33_kr_contrast(mf, list(x = 1), "slope")$status == "FAILED", "a rank mismatch is a FAILED model with a FAILED row")
ok("KR wrapper, CR2 on lmer, FAILED rows kept")

cat("\n4. scales, gates, outcome-free assertion, estimate rows\n")
yy <- c(1, 2, 3, 7, 8, 9); cl <- c(1, 1, 1, 2, 2, 2)
check(s33_moment_icc(yy, cl) > 0.9 && s33_moment_icc(c(1, 9, 1, 9), c(1, 1, 2, 2)) < 0, "moment ICC is signed")
check(isTRUE(all.equal(s33_resid_sd(c(1, 3, 5, 7), c(1, 1, 2, 2)), sqrt(4 / 3))), "residual SD about group means, n - 1")
check(isTRUE(all.equal(s33_per_sd(0.1, S33_SD_RATE), 0.51698)), "per frozen SD")
g1 <- s33_gate_rows("GX-1", "fixture", c(TRUE, TRUE)); g2 <- s33_gate_rows("GX-2", "fixture", c(TRUE, NA))
g3 <- s33_gate_rows("GX-3", "fixture", logical(), n_expected = 0L)
check(isTRUE(g1$passed) && !isTRUE(g2$passed) && !isTRUE(g3$passed), "gates: pass, NA fails, vacuous fails")
check(must_error(s33_stop_on_gates(rbind(g1, g2))) && isTRUE(s33_stop_on_gates(g1)), "a failed hard gate stops")
ne <- s33_gate_not_evaluated("X1", "lags identical", "partner absent")
check(!ne$evaluated && is.na(ne$passed) && isTRUE(s33_stop_on_gates(ne)), "not-evaluated gates never stop")
check(must_error(s33_assert_outcome_free(data.table(AnimalNum = "a", CombZ = 1))), "an outcome column is refused")
check(must_error(s33_assert_outcome_free(data.table(AnimalNum = "a", condition = "SUS"))), "RES/SUS labels are refused")
check(isTRUE(s33_assert_outcome_free(data.table(AnimalNum = "a", condition = "SIS"))), "SIS/CON condition passes")
r <- s33_row("within_cc1_cage", "animal_within_cohort", estimate = 1, metric_id = "crossing_rate")
check(identical(names(r), S33_ESTIMATE_COLUMNS) && r$tier == S33_TIER && r$metric_label == "RFID position-change rate", "estimate rows carry the declared columns")
check(must_error(s33_row("x", "nonsense")), "an undeclared level is refused")
ok("moment ICC, residual SD, per-SD, gates, assertion, rows")

cat("\nPASS: stage 33 shared helpers\n")
