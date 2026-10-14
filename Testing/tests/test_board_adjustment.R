# ===========================================================================
# Technical board adjustment of read-dependent measures (Testing/audits/audit_board_adjustment_read_dependent.R).
# Portable: synthetic cage episodes only.
#
#  1. board offsets from SIS cages within Batch x CC recover known board effects; an unidentified design is refused
#  2. CON - SIS: the adjusted contrast recovers a known CON effect whatever board the CON cage sat on; the unadjusted
#     contrast carries the board difference
#  3. the offsets' extra variance (a' V a) equals the variance over simulated offsets
#  4. SUS - RES within cages removes cage (and board) effects; cluster-robust standard error by cage
#  5. a recording without a SIS cage is left out and listed; its board weights do not enter the offsets' variance
#  6. CR1 against sandwich::vcovCL; the within-estimator small-sample factor; the adjusted SUS - RES adds a' V a
#  7. leave-one-batch-out re-estimates the offsets for the adjusted version
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Testing", "audits", "audit_board_adjustment_read_dependent.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
source("Testing/audits/audit_board_adjustment_read_dependent.R")   # definitions only (main-guarded)

# synthetic design: 6 batches x 4 CC, 5 boards each; the CON cage rotates over boards; y = recording effect + board effect
# (1: 0, 2: 1, 3: -2, 4: -1.5, 5: 0.5) + 3 for CON + small noise
set.seed(1)
bfx <- c(0, 1, -2, -1.5, 0.5)
ce <- CJ(Batch = paste0("B", 1:6), CC = paste0("CC", 1:4), board = 1:5)
ce[, rec_fx := rep(rnorm(24, 0, 2), each = 5)]
ce[, con_board := ((as.integer(factor(Batch)) + as.integer(factor(CC))) %% 5) + 1]
ce[, cage_type := ifelse(board == con_board, "CON", "SIS")]
ce[, y := rec_fx + bfx[board] + 3 * (cage_type == "CON") + rnorm(.N, 0, 0.01)]

# 1
off <- badj_board_offsets(ce[cage_type == "SIS"])
check(all(abs(off$coef - bfx) < 0.02) && off$rank == off$p, "1: SIS-only offsets recover the board effects")
check(inherits(tryCatch(badj_board_offsets(ce[cage_type == "SIS" & board == 2]), error = function(e) e), "error"), "1: an unidentified design is refused")

# 2
adj <- badj_con_contrast(ce, off); un <- badj_con_contrast(ce)
check(all(abs(adj$per_recording$d - 3) < 0.05), "2: the adjusted CON - SIS contrast recovers the CON effect in every recording")
expect_un <- ce[, .(d = 3 + bfx[board[cage_type == "CON"]] - mean(bfx[board[cage_type == "SIS"]])), by = .(Batch, CC)]
check(all(abs(merge(un$per_recording, expect_un, by = c("Batch", "CC"))[, d.x - d.y]) < 0.05), "2: the unadjusted contrast carries the board difference")
check(nrow(adj$contrasts) == 5 && abs(adj$contrasts[CC == "pooled (batch means over CC)"]$estimate - 3) < 0.05 && nrow(badj_con_lobo(ce)) == 6,
      "2: per CC, pooled over batch means, leave-one-batch-out")

# 3. exact extra variance against simulated offsets
V <- matrix(c(0.04, 0.01, 0, 0, 0.01, 0.09, 0.02, 0, 0, 0.02, 0.05, 0.01, 0, 0, 0.01, 0.03), 4, 4)
off2 <- list(coef = off$coef, vcov = V)
cc1 <- ce[CC == "CC1"]
base <- badj_con_contrast(cc1, off2)
w <- colMeans(as.matrix(base$board_weights[, .(w2, w3, w4, w5)]))
draws <- MASS::mvrnorm(20000, off2$coef[as.character(2:5)], V)
sim <- apply(draws, 1, function(b) { cf <- c(`1` = 0, stats::setNames(b, 2:5)); mean(cc1[, .(d = mean(y[cage_type == "CON"] - cf[as.character(board[cage_type == "CON"])]) -
                                                                                        mean(y[cage_type == "SIS"] - cf[as.character(board[cage_type == "SIS"])])), by = Batch]$d) })
check(abs(stats::var(sim) / as.numeric(t(w) %*% V %*% w) - 1) < 0.05, "3: a' V a equals the variance over simulated offsets")
dd <- base$per_recording[CC == "CC1"]$d; se1 <- base$contrasts[CC == "CC1"]$se
check(abs(se1^2 - (stats::var(dd) / length(dd) + as.numeric(t(w) %*% V %*% w))) < 1e-10,
      "3: the adjusted SE adds a' V a to the between-batch variance of the adjusted contrasts")

# 4. SUS - RES within cages: cage effects of +-5 and SUS effect 2; unbalanced SUS shares make the unadjusted estimate biased
an <- data.table(CageEpisodeID = rep(paste0("c", 1:8), each = 4))
an[, cage_fx := rep(c(5, -5, 4, -4, 3, -3, 2, -2), each = 4)]
an[, SUS := c(1, 1, 1, 0,  0, 0, 0, 1,  1, 1, 1, 0,  0, 0, 0, 1,  1, 1, 0, 0,  1, 0, 0, 0,  1, 1, 0, 0,  1, 0, 0, 0)]
an[, y := cage_fx + 2 * SUS + rnorm(.N, 0, 0.01)]
wc <- badj_sus_contrast(an, within_cage = TRUE); uc <- badj_sus_contrast(an)
check(abs(wc$estimate - 2) < 0.05 && wc$n_cages == 8 && is.finite(wc$se), "4: within-cage SUS - RES recovers the effect")
check(abs(uc$estimate - 2) > 0.5, "4: the unadjusted contrast is confounded by cage effects here")

# 5. a recording without a SIS cage in the population is left out and listed, and its board weights do not enter a' V a
ce5 <- ce[!(Batch == "B1" & CC == "CC2" & cage_type == "SIS")]
r5 <- badj_con_contrast(ce5, off2)
check(identical(r5$dropped$Batch, "B1") && identical(r5$dropped$CC, "CC2") && r5$contrasts[CC == "CC2"]$n == 5L &&
        !nrow(r5$board_weights[Batch == "B1" & CC == "CC2"]), "5: the incomplete recording is dropped, listed, and has no weights")
dd5 <- r5$per_recording[CC == "CC2"]$d; w5 <- colMeans(as.matrix(r5$board_weights[CC == "CC2", .(w2, w3, w4, w5)]))
check(abs(r5$contrasts[CC == "CC2"]$se^2 - (stats::var(dd5) / 5 + as.numeric(t(w5) %*% V %*% w5))) < 1e-10, "5: a' V a from the recordings in the estimate only")
check(abs(r5$contrasts[CC == "pooled (batch means over CC)"]$estimate - mean(r5$per_recording[, .(m = mean(d)), by = Batch]$m)) < 1e-12,
      "5: a batch keeps its available cage changes in the pooled value")

# 6. CR1 against sandwich::vcovCL (HC1), and the within-estimator convention; SUS - RES adjusted adds a' V a
an[, board := rep(c(1:5, 2, 3, 4), each = 4)]
fit <- stats::lm(y ~ SUS, an)
check(abs(badj_cr1(fit, an$CageEpisodeID)[2, 2] - sandwich::vcovCL(fit, cluster = ~CageEpisodeID, type = "HC1")[2, 2]) < 1e-12, "6: CR1 equals vcovCL HC1")
fe <- stats::lm(y ~ SUS + factor(CageEpisodeID), an)
n6 <- nrow(an); kf <- length(stats::coef(fe))
check(abs(badj_cr1(fe, an$CageEpisodeID, k_small = 2)[2, 2] - sandwich::vcovCL(fe, cluster = ~CageEpisodeID, type = "HC1")[2, 2] * (n6 - kf) / (n6 - 2)) < 1e-12,
      "6: within-cage CR1 counts only the non-fixed-effect coefficients")
ua <- badj_sus_contrast(an); aa <- badj_sus_contrast(an, off = off2)
a6 <- vapply(2:5, function(b) mean(an$board[an$SUS == 1] == b) - mean(an$board[an$SUS == 0] == b), numeric(1))
check(abs(aa$estimate - (ua$estimate - sum(a6 * off2$coef[as.character(2:5)]))) < 1e-10, "6: the adjusted SUS - RES subtracts the offsets")
yv <- copy(an)[, y := y - off2$coef[as.character(board)]]
check(abs(aa$se^2 - (badj_cr1(stats::lm(y ~ SUS, yv), yv$CageEpisodeID)[2, 2] + as.numeric(t(a6) %*% V %*% a6))) < 1e-10, "6: and adds a' V a")

# 7. leave-one-batch-out: the adjusted version re-estimates the offsets without the batch
lo <- badj_con_lobo(ce, adjust = TRUE)
x7 <- ce[Batch != "B1"]
check(nrow(lo) == 6L && abs(lo[left_out == "B1"]$estimate -
        badj_con_contrast(x7, badj_board_offsets(x7[cage_type == "SIS"]))$contrasts[CC == "pooled (batch means over CC)"]$estimate) < 1e-12,
      "7: leave-one-batch-out with offsets re-estimated")

cat("test_board_adjustment: all checks passed\n")
