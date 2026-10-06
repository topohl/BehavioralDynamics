# Stage 33 module D (Functions/stage33_d_hourly.R): synthetic checks only.
#
#   1. the 36-window table (hour, cumulative, A1, since-start windows from the lag);
#   2. separation algebra: S, N, E, W, D for all six cohorts and within sex; E = 0 when the noise floor exceeds S;
#   3. per-draw separation with every multiplicity 1 equals the point value; duplicated cages are separate clusters;
#   4. cohort summaries: no interval below 3 cages or for the CON cage; resampling range inside the cage-mean range;
#   5. Kendall W and tau-b on known ranks; the 864 single-cage reference count; no SIS-minus-CON column.
# Portable: no S: drive, no live data; sources Functions/ only.

suppressPackageStartupMessages({ library(data.table) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
for (f in c("Functions/stage33_common.R", "Functions/stage33_d_hourly.R")) source(f)

cat("1. windows\n")
a <- as.POSIXct("2023-04-24 18:30:00", tz = "UTC"); rs <- a - 6.086 * 3600
W <- s33d_windows("f.csv", a, rs)
check(nrow(W) == 36L && identical(W[window_set == "clock_hour", window_id], sprintf("hr%02d", 1:12)), "36 windows with hr01-hr12")
check(all(W[window_set == "cumulative", start] == a) && W[window_id == "A1", as.numeric(end - start, units = "hours")] == 12, "cumulative windows from 18:30; A1 = 12 h")
check(identical(W[window_set == "since_start", k_since_start], 7:17), "since-start k = ceiling(lag) .. floor(lag + 12) - 1")
check(identical(attr(W$start, "tzone"), "UTC") && !any(grepl("^H[0-9]{2}$", W$window_id)), "UTC times; no H01-H13 labels")
W2 <- s33d_windows("f.csv", a, a - 2.754 * 3600)
check(identical(W2[window_set == "since_start", k_since_start], 3:13), "B1 lag gives k = 3..13")
ok("hour, cumulative, A1 and since-start windows")

cat("\n2. separation algebra\n")
cs <- data.table(Batch = paste0("B", 1:6), Sex = c("Male", "Male", "Female", "Female", "Male", "Female"),
                 m = c(20, 22, 30, 28, 24, 26), v = rep(1, 6), ss = rep(90, 6), n = rep(10L, 6))
s <- s33d_separation(cs, "all6")
check(isTRUE(all.equal(s$S, sd(cs$m))) && isTRUE(all.equal(s$N, 1)) && isTRUE(all.equal(s$E, sqrt(sd(cs$m)^2 - 1))), "all six: S, N, E")
check(isTRUE(all.equal(s$W, sqrt(540 / 54))) && isTRUE(all.equal(s$D, s$E / s$W)), "pooled within-cohort SD and D = E / W")
sw <- s33d_separation(cs, "within_sex")
dev2 <- cs[, (m - mean(m))^2, by = Sex]$V1
check(isTRUE(all.equal(sw$S, sqrt(sum(dev2) / 4))), "within sex: deviations from the sex means over K - 2")
check(identical(s33d_separation(copy(cs)[, v := 1000], "all6")$E, 0), "noise floor above S gives E = 0")
check(nrow(cs[Sex == "Female"]) == 3L && isTRUE(all.equal(s33d_separation(cs, "Female")$S, sd(c(30, 28, 26)))), "one-sex scope")
ok("S, N, E, W and D")

cat("\n3. per-draw separation\n")
set.seed(7); B <- 5L
mk <- function(G, n) list(S = matrix(runif(G * 3, 10, 40) * n, G, 3), Q = NULL, n = matrix(n, G, 3))
stats_by <- lapply(setNames(paste0("B", 1:6), paste0("B", 1:6)), function(b) {
  y <- matrix(rnorm(16 * 3, 20 + as.integer(sub("B", "", b)), 4), 16, 3); g <- rep(1:4, each = 4)
  list(S = rowsum(y, g), Q = rowsum(y^2, g), n = matrix(4, 4, 3), y = y, g = g) })
U1 <- lapply(stats_by, function(z) matrix(1L, B, 4))
Dd <- s33d_draw_D(stats_by, U1, S33_SEX_OF_COHORT, "all6")
pt <- vapply(1:3, function(w) { cst <- rbindlist(lapply(names(stats_by), function(b) { z <- stats_by[[b]]; y <- z$y[, w]
  data.table(Batch = b, Sex = S33_SEX_OF_COHORT[[b]], m = mean(y), v = s33_cr2_mean_closed(y, z$g), ss = sum((y - mean(y))^2), n = length(y)) }))
  s33d_separation(cst, "all6")$D }, 0)
check(all(abs(sweep(Dd, 2, pt)) < 1e-10), "multiplicity 1 reproduces the point D in every window")
Ud <- lapply(stats_by, function(z) matrix(c(2L, 0L, 1L, 1L), B, 4, byrow = TRUE))
Dd2 <- s33d_draw_D(stats_by, Ud, S33_SEX_OF_COHORT, "all6")
z <- stats_by$B1; y <- z$y[c(1:4, 1:4, 9:16), 1]; g <- c(rep("a", 4), rep("a2", 4), rep(c("c", "d"), each = 4))
check(isTRUE(all.equal(s33_cr2_mean_closed(y, g) > 0, TRUE)) && all(is.finite(Dd2)), "a duplicated cage counts as a separate cluster")
ok("per-draw D")

cat("\n4. cohort summaries\n")
y <- c(10, 12, 14, 20, 22, 24, 30, 31, 33, 18, 19, 21); cg <- rep(c("c1", "c2", "c3", "c4"), each = 3)
U <- s33_multiplicity(s33_resample_index(c(x = 4L), 200L, 1L)$x, 4L); colnames(U) <- c("c1", "c2", "c3", "c4")
sm <- s33d_summary(y, cg, U, "SIS")
cm <- tapply(y, cg, mean)
check(sm$ci_method == "CR2_Satterthwaite_cc1_cage" && is.na(sm$ci_none_reason) && sm$ci_low < sm$mean && sm$mean < sm$ci_high, "SIS: CR2 interval")
check(sm$resamp_low >= min(cm) && sm$resamp_high <= max(cm), "resampling range inside the cage-mean range")
s2 <- s33d_summary(y[1:6], cg[1:6], NULL, "SIS"); s3 <- s33d_summary(y[1:4], rep("con", 4), NULL, "CON")
check(s2$ci_method == "none" && s2$ci_none_reason == "fewer than 3 informative cages" && is.na(s2$ci_low), "fewer than 3 cages: no interval")
check(s3$ci_method == "none" && s3$ci_none_reason == "single CON cage", "CON: no interval")
check(s33d_summary(y, cg, NULL, "SIS", "cc3_cage")$ci_method == "CR2_Satterthwaite_cc3_cage", "the CR2 label names the cage level")
cw <- s33d_cage_weighted_means(data.table(Batch = "B1", CC = "CC1", CageEpisodeID = c("a", "a", "b", "c", "c"),
                                          occ = c(0.2, 0.4, NA, 0.6, NA)), "occ")
check(nrow(cw) == 1L && isTRUE(all.equal(cw$m, mean(c(0.3, 0.6)))), "S1 cage-weighted mean skips a cage without a finite value")
check(isTRUE(all.equal(sm$mean_cage_weighted, mean(cm))), "cage-weighted mean")
ok("CR2, CR1, ranges, cage-weighted means")

cat("\n5. rank statistics, reference count, column names\n")
r <- cbind(CC1 = 1:6, CC2 = 1:6, CC3 = 1:6, CC4 = 1:6)
Wk <- 12 * sum((rowSums(r) - 4 * 7 / 2)^2) / (4^2 * (6^3 - 6))
check(isTRUE(all.equal(Wk, 1)), "Kendall W = 1 for identical rankings")
check(isTRUE(all.equal(cor(1:6, c(2, 1, 3, 4, 6, 5), method = "kendall"), 1 - 2 * 2 / 15)), "tau-b on two swaps")
check(prod(c(2, 4, 4, 3, 3, 3)) == 864, "864 single-cage reference sets (one 4-animal SIS cage per cohort)")
bad <- grep("(?i)minus|diff|delta|contrast|g_sis", c(names(sm), "standardized_divergence_D", "between_cohort_sd_S",
                                                     "dev_from_6cohort_cc_mean", "dev_from_own_sex_3cohort_cc_mean"), perl = TRUE, value = TRUE)
check(!length(bad), paste("no SIS-minus-CON column:", paste(bad, collapse = ",")))
check(identical(s33d_rank_desc(c(5, 9, 1)), c(2, 1, 3)), "rank 1 = highest")
ok("Kendall W, tau-b, 864 sets, column names")

cat("\nPASS: stage 33 module D\n")
