# ===========================================================================
# Single-read spatial reconstruction helpers (Functions/antenna_spatial_reconstruction.R). Portable: synthetic reads
# only; nothing on the lab drive. Expected values are worked out by hand in the comments.
#
#  1. same-millisecond reads: one observation; the held reader wins a tie, otherwise the lowest PositionID
#  2. observation age: unsupported time before the first read, in gaps longer than delta and after the last read; a
#     read before the span seeds the age
#  3. elementary segments: cut at both bases' assignment changes and at the cuts; positions; supported seconds
#  4. sums by block with a censoring time; the event x read cross-table
#  5. intervals cut at block edges (a cut at a start or an end adds nothing)
#  6. Stage 29 window runs: the record before the window seeds, records clip and equal positions merge
#  7. window metrics through mmm_bf_window_table(): crossing rate, dispersion, shared zone
#  8. vendor records of a synthetic tag (apr_positions(), gate vendor) without the closing record
#  9. materiality: median and 90th percentile of |shift| against the scale
# 10. cage-episode key in Stage 29's format
# 11. position changes per block at reader and column resolution, after the seed and before a censoring time
# 12. 1-s grid state (position, age of the latest read, length of its read gap) and gap classes
# 13. board missingness: unknown vs supported seconds, cage-mates at the same reader, censored cells left out
# 14. shares of observed time with the unsupported time as its own category, and of supported time
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_spatial_reconstruction.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, check.attributes = FALSE))
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/animalpos_reconstruction.R")
source("Functions/rfid_binfree_metrics.R")
source("Functions/antenna_measurement.R")
source("Functions/antenna_spatial_reconstruction.R")

# 1. Reads (ms, reader): 1000 R2+R3 (first, no held reader: lowest = 2); 2000 R2; 3000 R5+R1+R5 (held 2 not among them:
#    lowest = 1); 4000 R6+R6 (one reader: no tie). Then held among the tied: 0 R4, 10 R1+R4 -> 4.
r <- asr_resolve_ties(c(1000, 1000, 2000, 3000, 3000, 3000, 4000, 4000), c(2, 3, 2, 5, 1, 5, 6, 6))
check(identical(r$t_ms, c(1000, 2000, 3000, 4000)) && identical(r$pid, c(2L, 2L, 1L, 6L)) && identical(r$tie, c(TRUE, FALSE, TRUE, FALSE)),
      "1: tie resolution without a held reader among the tied")
r <- asr_resolve_ties(c(0, 10, 10), c(4, 1, 4))
check(identical(r$pid, c(4L, 4L)) && identical(r$tie, c(FALSE, TRUE)), "1: the held reader wins a tie")
r <- asr_resolve_ties(c(0, 10, 10, 20, 20), c(4, 1, 3, 3, 1))
check(identical(r$pid, c(4L, 1L, 1L)), "1: consecutive ties resolve in time order (held 1 from the first tie wins the second)")
check(nrow(asr_resolve_ties(numeric(), integer())) == 0L, "1: no reads")
check(inherits(tryCatch(asr_resolve_ties(c(2, 1), c(1, 1)), error = function(e) e), "error"), "1: unsorted times are refused")

# 2. Reads at 10, 11, 20, 100 s; span from 0; delta 5. Unsupported: [0,10) (no read yet), [16,20), [25,100), [105,Inf).
#    Supported in [0,120): [10,16) + [20,25) + [100,105) = 16 s; in [12,30): [12,16) + [20,25) = 9 s.
rt <- c(10, 11, 20, 100)
check(near(asr_supported_seconds(rt, 5, 0, 120, from = 0), 16), "2: supported seconds over the span")
check(near(asr_supported_seconds(rt, 5, c(12, 0), c(30, 10), from = 0), c(9, 0)), "2: supported seconds of segments; none before the first read")
check(near(asr_supported_seconds(rt, 9, 11, 20, from = 0), 9), "2: a gap equal to delta is fully supported")
#    A read before the span seeds the age: reads at -3 and 2, delta 4: age 3 at 0 -> supported [0,1), [2,6); 5 s of [0,10).
check(near(asr_supported_seconds(c(-3, 2), 4, 0, 10, from = 0), 5), "2: a seed read before the span")
check(near(asr_supported_seconds(numeric(), 60, 0, 100, from = 0), 0), "2: no reads: nothing supported")

# 3. Event basis R1 [0,30), R2 [30,60); read basis R1 [0,25), R2 [25,60); reads at 0, 25, 30, 40; cuts at 50 and 55
#    (block edge, censoring); delta 10. Segments [0,25) e1 r1, [25,30) e1 r2, [30,50) e2 r2, [50,55) and [55,60) e2 r2.
#    Supported (delta 10): unsupported [10,25) and [50,Inf) -> 10, 5, 20, 0, 0.
ev_iv <- data.table(PositionID = c(1L, 2L), start = c(0, 30), end = c(30, 60))
rd_iv <- data.table(PositionID = c(1L, 2L), start = c(0, 25), end = c(25, 60))
seg <- asr_animal_segments(ev_iv, rd_iv, c(0, 25, 30, 40), 0, 60, cuts = c(50, 55), bounds = 10)
check(near(seg$a, c(0, 25, 30, 50, 55)) && near(seg$b, c(25, 30, 50, 55, 60)), "3: segment edges")
check(identical(seg$pos_e, c(1L, 1L, 2L, 2L, 2L)) && identical(seg$pos_r, c(1L, 2L, 2L, 2L, 2L)), "3: positions of both bases")
check(near(seg$sup_10, c(10, 5, 20, 0, 0)), "3: supported seconds per segment")
seg2 <- asr_animal_segments(data.table(PositionID = 3L, start = 20, end = 60), rd_iv, c(0, 25), 0, 60, bounds = 10)
check(identical(seg2$pos_e[seg2$a < 20], 0L), "3: before an animal's first event-basis assignment the event position is 0")
check(identical(asr_position_at(ev_iv, c(-1, 0, 29.9, 30, 60)), c(0L, 1L, 1L, 2L, 0L)), "3: position at, outside the intervals 0")

# 4. Blocks starting at 0 (block 100) and 50 (block 101); censored from 55. Event basis: block 100 R1 30 s (sup 15),
#    R2 20 s (sup 20); block 101 R2 5 s (sup 0). Read basis: block 100 R1 25, R2 25. Cross-table block 100: (1,1) 25,
#    (1,2) 5, (2,2) 20; block 101: (2,2) 5. Censored: block 101, 5 s.
ag <- asr_aggregate(seg, data.table(block = c(100, 101), bs = c(0, 50)), bounds = 10, censor_from = 55)
oe <- ag$occupancy[basis == "event"][order(block, PositionID)]
check(near(oe$seconds, c(30, 20, 5)) && near(oe$sup_10, c(15, 20, 0)) && identical(oe$PositionID, c(1L, 2L, 2L)), "4: event-basis sums")
orr <- ag$occupancy[basis == "read" & block == 100][order(PositionID)]
check(near(orr$seconds, c(25, 25)), "4: read-basis sums")
xt <- ag$crosstab[order(block, pos_e, pos_r)]
check(near(xt$seconds, c(25, 5, 20, 5)) && identical(xt$pos_r, c(1L, 2L, 2L, 2L)), "4: cross-table")
check(identical(ag$censored$block, 101) && near(ag$censored$seconds, 5), "4: censored time")
check(near(sum(ag$occupancy[basis == "event"]$seconds) + sum(ag$censored$seconds), 60), "4: event basis + censored = span")

# 5. R3 [0,100), R4 [100,130) cut at 50, 100, 120: R3 [0,50) [50,100), R4 [100,120) [120,130).
ci <- asr_cut_intervals(data.table(PositionID = c(3L, 4L), start = c(0, 100), end = c(100, 130)), c(50, 100, 120))
check(near(ci$start, c(0, 50, 100, 120)) && near(ci$end, c(50, 100, 120, 130)) && identical(ci$PositionID, c(3L, 3L, 4L, 4L)), "5: cuts")
ci <- asr_cut_intervals(data.table(PositionID = 1L, start = 10, end = 20), c(0, 30))
check(nrow(ci) == 1L && near(ci$start, 10) && near(ci$end, 20), "5: cuts outside an interval add nothing")
ci <- asr_cut_intervals(data.table(PositionID = 1L, start = 10, end = 40), c(15, 20, 25))
check(near(ci$start, c(10, 15, 20, 25)) && near(ci$end, c(15, 20, 25, 40)), "5: several cuts in one interval")

# 6. Records at 5 (R1), 20 (R2), 30 (R2), 50 (R3); window [10,40): R1 [0,10), R2 [10,30) (merged) in window seconds.
wr <- asr_window_runs(data.table(t = c(5, 20, 30, 50), PositionID = c(1L, 2L, 2L, 3L)), 10, 40)
check(near(wr$s, c(0, 10)) && near(wr$e, c(10, 30)) && identical(wr$pos, c(1L, 2L)), "6: window runs")
check(nrow(asr_window_runs(data.table(t = 50, PositionID = 1L), 10, 40)) == 0L, "6: a record after the window gives no run")

# 7. Animal A: R1 [0,1800), R2 [1800,3600) -> 1 change in 1 h, dispersion 1 bit; B: R1 the whole hour -> 0, 0 bits.
#    Same position for 1800 of 3600 shared seconds: shared zone 0.5 for both.
runs <- data.table(AnimalNum = c("A", "A", "B"), CC = "CC1", SourceFile = "f", System = "sys.1", s = c(0, 1800, 0), e = c(1800, 3600, 3600), pos = c(1L, 2L, 1L))
wm <- asr_window_metrics(runs)[order(AnimalNum)]
check(near(wm$crossing_rate, c(1, 0)) && near(wm$occupancy_dispersion, c(1, 0)) && near(wm$shared_zone_use, c(0.5, 0.5)), "7: window metrics")

# 8. Reads at 0 and 100 ms at x 0 (R1) and at 700, 800, 1400 ms at x 300 (R4), y 0. The first window closes at 700
#    (mean x 0: first record at 0 ms, R1); the next at 1400 (reads 700 and 800, mean x 300, 300 units away: a move
#    stamped at 700 ms, R4); the closing record is dropped.
vr <- asr_vendor_records(c(0, 100, 700, 800, 1400), c(0, 0, 300, 300, 300), c(0, 0, 0, 0, 0))
check(near(vr$t, c(0, 0.7)) && identical(vr$PositionID, c(1L, 4L)), "8: vendor records")
#    Without the R4 reads only 0 and 100 ms remain: no window closes, no record.
check(nrow(asr_vendor_records(c(0, 100), c(0, 0), c(0, 0))) == 0L, "8: no record without a closing read")

# 9. Shifts -1, 0, 1, 2, 3: |x| = 1, 0, 1, 2, 3 -> median 1, 90th percentile (type 7) 2.6, median shift 1.
m <- asr_materiality(c(-1, 0, 1, 2, 3, NA), sd = 10)
check(m$n == 5L && near(m$median_abs_shift, 1) && near(m$p90_abs_shift, 2.6) && near(m$median_shift, 1) && identical(m$material, FALSE),
      "9: not material at sd 10 (1 < 2 and 2.6 < 5)")
check(identical(asr_materiality(c(-1, 0, 1, 2, 3), sd = 4)$material, TRUE), "9: material at sd 4 (median 1 >= 0.8)")
check(identical(asr_materiality(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 10), sd = 15)$material, FALSE) &&
        identical(asr_materiality(c(0, 0, 0, 0, 0, 0, 0, 0, 10, 10), sd = 15)$material, TRUE), "9: the 90th-percentile arm (0.5 SD)")
check(is.na(asr_materiality(numeric(), 1)$material), "9: no shifts")

# 10. Cage-episode key.
check(identical(asr_cage_episode(c("B5", "B2"), c(4, 2), c("CC1", "CC3")), c("B5|sys.4|CC1", "B2|sys.2|CC3")), "10: Batch|System|CC")

# 11. Changes per block: R1 [0,10), R2 [10,20), R6 [20,30), R1 [30,40); blocks from 0 and 25. Reader changes at 10, 20 (block
#     1) and 30 (block 2); column states 1, 2, 2, 1 merge to 1, 2, 1: changes at 10 (block 1) and 30 (block 2). A censoring
#     time at 25 removes the change at 30. The seed state at S0 is never a change.
iv <- data.table(PositionID = c(1L, 2L, 6L, 1L), start = c(0, 10, 20, 30), end = c(10, 20, 30, 40))
bk <- data.table(block = c(1, 2), bs = c(0, 25))
check(identical(asr_interval_changes(iv, bk, 0)$changes, c(2L, 1L)), "11: reader changes per block")
check(identical(asr_interval_changes(iv, bk, 0, resolution = "column")$changes, c(1L, 1L)), "11: column changes per block")
check(identical(asr_interval_changes(iv, bk, 0, censor_from = 25)$changes, c(2L, 0L)), "11: no change after the censoring time")
check(identical(asr_interval_changes(iv[1], bk, 0)$changes, c(0L, 0L)), "11: one state, no change")

# 12. Grid state: reads at 10, 20, 100; R1 [10,20), R2 [20,Inf). Cells 5.5, 15.5, 50.5, 150.5: age NA, 5.5, 30.5, 50.5; gap NA,
#     10, 80, Inf (after the last read); position 0, 1, 2, 2.
gs <- asr_grid_state(data.table(PositionID = c(1L, 2L), start = c(10, 20), end = c(20, Inf)), c(10, 20, 100), c(5.5, 15.5, 50.5, 150.5))
check(identical(gs$pos, c(0L, 1L, 2L, 2L)) && near(gs$age[-1], c(5.5, 30.5, 50.5)) && is.na(gs$age[1]) &&
        near(gs$gap[2:3], c(10, 80)) && is.infinite(gs$gap[4]) && is.na(gs$gap[1]), "12: grid state")
check(identical(asr_gap_class(c(NA, 100, 400, 2000, 4000, 20000, Inf)),
                c("before the first read", "60-300s", "300-1800s", "1800-3600s", "3600-10800s", ">10800s", "open (no later read)")), "12: gap classes")

# 13. Missingness on 4 cells, bound 60. A: R5 (age 10), R5 (100), R5 (100), R1 (10); B: R5 (100), R5 (10), R1 (10), R1 (100).
#     A supported at R5 (cell 1): B at R5, not supported -> same 1, same supported 0, mate supported anywhere 0. A unknown at R5
#     (cells 2, 3): cell 2 B at R5 supported, cell 3 B at R1 supported -> seconds 2, same 1, same supported 1, anywhere 2.
#     A supported at R1 (cell 4): B at R1 unsupported -> same 1, 0, 0. A's unknown cells lie in a 200-s gap.
st <- list(A = list(pos = c(5L, 5L, 5L, 1L), age = c(10, 100, 100, 10), gap = c(20, 200, 200, 20)),
           B = list(pos = c(5L, 5L, 1L, 1L), age = c(100, 10, 10, 100), gap = c(300, 20, 20, 300)))
bm <- asr_board_missingness(st, rep(1, 4), rep(TRUE, 4), bound = 60)
a5u <- bm$colocation[AnimalNum == "A" & pos == 5 & state == "unknown"]
a5s <- bm$colocation[AnimalNum == "A" & pos == 5 & state == "supported"]
check(a5u$seconds == 2 && a5u$mate_same_reader == 1 && a5u$mate_same_reader_supported == 1 && a5u$mate_supported_anywhere == 2, "13: unknown at R5")
check(a5s$seconds == 1 && a5s$mate_same_reader == 1 && a5s$mate_same_reader_supported == 0 && a5s$mate_supported_anywhere == 0, "13: supported at R5")
check(identical(bm$gaps[AnimalNum == "A"]$gap_class, "60-300s") && bm$gaps[AnimalNum == "A"]$unknown_s == 2, "13: gap class of the unknown time")
bm2 <- asr_board_missingness(st, rep(1, 4), c(TRUE, TRUE, FALSE, FALSE), bound = 60)
check(sum(bm2$colocation[AnimalNum == "A"]$seconds) == 2, "13: censored cells are left out")

# 14. Shares with the unknown category: group g1 observed 100 s; columns 1 and 2 with LOCF 60 / 40, <= 60 s 30 / 20 and
#     <= 300 s 50 / 30. LOCF 0.6 / 0.4, unknown 0; max60s 0.3 / 0.2, unknown 0.5, of supported time 0.6 / 0.4; max300s
#     0.5 / 0.3, unknown 0.2, of supported 0.625 / 0.375. Group g2 observed 50 s without any assignment: unknown 1.
oc <- data.table(g = "g1", column = c(1L, 2L), seconds = c(60, 40), sup_60 = c(30, 20), sup_300 = c(50, 30))
sh <- asr_shares_with_unknown(oc, data.table(g = c("g1", "g2"), observed_s = c(100, 50)), by = "g", unit = "column")
g1 <- function(b, u) sh[g == "g1" & bound == b & unit == u]
check(near(g1("LOCF", "1")$share_all, 0.6) && near(g1("LOCF", "unknown")$share_all, 0), "14: LOCF shares")
check(near(g1("max60s", "1")$share_all, 0.3) && near(g1("max60s", "unknown")$share_all, 0.5) && near(g1("max60s", "1")$share_supported, 0.6),
      "14: max60s, both denominators")
check(near(g1("max300s", "2")$share_supported, 0.375) && near(g1("max300s", "unknown")$share_all, 0.2), "14: max300s")
check(all(abs(sh[g == "g2"]$share_all - 1) < 1e-12) && all(sh[g == "g2"]$unit == "unknown"), "14: a group without assigned time is all unknown")
check(all(sh[, near(sum(share_all), 1), by = .(g, bound)]$V1), "14: units plus unknown sum to 1")

cat("test_antenna_spatial_reconstruction: all checks passed\n")
