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

cat("test_antenna_spatial_reconstruction: all checks passed\n")
