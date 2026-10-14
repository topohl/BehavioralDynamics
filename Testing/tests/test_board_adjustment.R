# ===========================================================================
# Technical board adjustment of read-dependent measures (Testing/audits/audit_board_adjustment_read_dependent.R).
# Portable: synthetic cage episodes only.
#
#  1. board offsets from SIS cages within Batch x CC recover known board effects; an unidentified design is refused
#  2. CON - SIS: the adjusted contrast recovers a known CON effect whatever board the CON cage sat on; the unadjusted
#     contrast carries the board difference
#  3. the offsets' extra variance (a' V a) equals the variance over simulated offsets
#  4. SUS - RES within cages removes cage (and board) effects; cluster-robust standard error by cage
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
check(nrow(adj$contrasts) == 5 && abs(adj$contrasts[CC == "pooled (batch means over CC)"]$estimate - 3) < 0.05 && nrow(adj$leave_one_batch_out) == 6,
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

cat("test_board_adjustment: all checks passed\n")
