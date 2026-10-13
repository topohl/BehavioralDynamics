# ===========================================================================
# Measurement-validation helpers (Functions/antenna_measurement.R). Portable: synthetic detections only; nothing on
# the lab drive. Expected values are worked out by hand in the comments.
#
#  1. read times from MsgValue1 gaps; empty offsets
#  2. detection union, support-age exceedance (incl. the time before the first detection), gaps, overlap seconds
#  3. bounded support and runs cut to it; delta = Inf keeps the carry-forward from the first detection
#  4. runs: tau = 0 is raw carry-forward; the flip filter collapses A-B-A visits shorter than tau
#  5. transitions: geometry classes, gap (negative when two readers read at once), visit durations, returns
#  6. occupancy proportions, dispersion and dominant share agree with mmm_bf_animal_metrics()
#  7. dyad co-assignment: shift table against brute force; independence expectation by hand
#  8. concurrent tags per event: same board and reader, positive overlap only
#  9. system rows of a synthetic UTF-16LE log
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_measurement.R")) && dir.exists(file.path(repo, "Analysis")))) {
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
source("Functions/rfid_binfree_metrics.R")
source("Functions/antenna_measurement.R")

# 1. Events at t = 10 and 20 s with offsets "0,115,415" and "0": reads at 10, 10.115, 10.530 and 20; an empty field
#    adds no read.
rt <- amv_read_times(c(10, 20, 30), c("0,115,415", "0", ""))
check(identical(rt$ev, c(1L, 1L, 1L, 2L)), "1: event index of the reads")
check(near(rt$t_read, c(10, 10.115, 10.530, 20)), "1: read times")

# 2. Detections [0,1) [0.5,2) [5,6) [20,21): union [0,2) [5,6) [20,21).
u <- amv_union(c(5, 0, 0.5, 20), c(6, 1, 2, 21))
check(near(u$s, c(0, 5, 20)) && near(u$e, c(2, 6, 21)), "2: union")
check(nrow(amv_union(c(0, 1), c(1, 2))) == 1, "2: touching intervals merge")
# Window [1, 30): gaps [2,5) (3 s), [6,20) (14 s), [21,30) (9 s, open end). Age > 2: 1 + 12 + 7 = 20; age > 0: 26;
# age > 10: 4.
check(near(amv_age_exceedance(u, 1, 30, c(0, 2, 10)), c(26, 20, 4)), "2: age exceedance")
# Window [-10, 30): before the first detection (10 s) the age is undefined and counts for every bound.
check(near(amv_age_exceedance(u, -10, 30, c(0, 2, 10)), c(36, 30, 14)), "2: undefined age before the first detection")
g <- amv_gaps(u, 1, 30, min_s = 5)
check(near(g$s, c(6, 21)) && near(g$e, c(20, 30)), "2: gaps of at least 5 s")
check(near(amv_overlap_seconds(data.table(s = c(0, 10), e = c(4, 12)), data.table(s = c(3, 11), e = c(11, 20))), 1 + 1 + 1),
      "2: overlap seconds")

# 3. Support with delta 3 in [1, 30): [0,5) [5,9) [20,24) -> union [0,9) [20,24) -> clipped [1,9) [20,24).
sp <- amv_support(u, 1, 30, 3)
check(near(sp$s, c(1, 20)) && near(sp$e, c(9, 24)), "3: bounded support")
check(near(amv_support(u, -10, 30, Inf)$s, 0) && near(amv_support(u, -10, 30, Inf)$e, 30), "3: unbounded support starts at the first detection")
rr <- data.table(s = c(0, 4, 10), e = c(4, 10, 30), pos = c(1L, 2L, 3L))
cl <- amv_clip_runs(rr, data.table(s = c(1, 20), e = c(9, 24)))
check(near(cl$s, c(1, 4, 20)) && near(cl$e, c(4, 9, 24)) && identical(cl$pos, c(1L, 2L, 3L)), "3: runs cut to the support")

# 4. Detections at 0 (R1), 10 (R2), 11.5 (R1), 30 (R5); window [5, 40).
#    tau 0: R1 [0,5) seeds, R2 [10,11.5), R1 [11.5,30), R5 [30,40).
#    tau 2: the R2 visit lasts 1.5 s < 2 and returns to R1 -> R1 [0,30), R5 [30,40).
t4 <- c(0, 10, 11.5, 30); p4 <- c(1L, 2L, 1L, 5L)
r0 <- amv_runs(t4, p4, 5, 40, tau = 0)
check(near(r0$s, c(0, 5, 6.5, 25)) && near(r0$e, c(5, 6.5, 25, 35)) && identical(r0$pos, c(1L, 2L, 1L, 5L)), "4: raw carry-forward runs")
r2 <- amv_runs(t4, p4, 5, 40, tau = 2)
check(near(r2$s, c(0, 25)) && identical(r2$pos, c(1L, 5L)), "4: flip filter collapses the short visit")

# 5. Events (start, end, reader): (0, 2, R1) (1.5, 1.6, R5) (2.1, 4, R1) (4, 4.1, R1) (9, 9.5, R3) (12, 12.1, R4).
#    Visits R1 [0..2], R5 [1.5], R1 [2.1..4.1], R3 [9], R4 [12].
#    Changes: 1.5 R1->R5 row, gap 1.5 - 2 = -0.5 (both read at once), from_dur 1.5, to_dur 0.6, returns TRUE;
#             2.1 R5->R1 row, gap 0.5, from_dur 0.6, to_dur 6.9, returns FALSE (next goes to R3);
#             9 R1->R3 far (200), gap 9 - 4.1 = 4.9, from_dur 6.9, to_dur 3, returns FALSE;
#             12 R3->R4 col1, gap 2.5, from_dur 3, to_dur NA, returns NA.
tr <- amv_transitions(c(0, 1.5, 2.1, 4, 9, 12), c(2, 1.6, 4, 4.1, 9.5, 12.1), c(1L, 5L, 1L, 1L, 3L, 4L))
check(nrow(tr) == 4, "5: number of changes")
check(identical(as.character(tr$geometry), c("row", "row", "far", "col1")), "5: geometry")
check(near(tr$gap_s, c(-0.5, 0.5, 4.9, 2.5)), "5: gaps")
check(near(tr$from_dur_s, c(1.5, 0.6, 6.9, 3)), "5: from durations")
check(near(tr$to_dur_s[1:3], c(0.6, 6.9, 3)) && is.na(tr$to_dur_s[4]), "5: to durations")
check(identical(tr$returns, c(TRUE, FALSE, FALSE, NA)), "5: returns")
check(identical(as.character(amv_move_geometry(c(1L, 1L, 2L, 1L), c(6L, 2L, 5L, 1L))), c("diag1", "col1", "diag1", NA)),
      "5: diagonal neighbour and same reader")
check(nrow(amv_transitions(c(0, 1), c(0.1, 1.1), c(3L, 3L))) == 0, "5: no change")

# 6. Runs R1 30 s, R2 10 s, R1 20 s, R6 40 s (100 s): p_R1 0.5, p_R2 0.1, p_R6 0.4; columns C1 0.5, C2 0.5;
#    upper row 0.4; dispersion -(0.5 log2 0.5 + 0.1 log2 0.1 + 0.4 log2 0.4); dominant 0.5.
r6 <- data.table(AnimalNum = "A", CC = "CC1", SourceFile = "f", System = "sys.1", s = c(0, 30, 40, 60), e = c(30, 40, 60, 100),
                 pos = c(1L, 2L, 1L, 6L))
oc <- amv_occupancy(r6)
check(near(oc$p_R1, 0.5) && near(oc$p_R2, 0.1) && near(oc$p_R6, 0.4) && near(oc$p_C1, 0.5) && near(oc$p_C2, 0.5), "6: proportions")
check(near(oc$upper_row_share, 0.4) && near(oc$dominant_share, 0.5), "6: upper row and dominant share")
check(near(oc$occupancy_dispersion, -(0.5 * log2(0.5) + 0.1 * log2(0.1) + 0.4 * log2(0.4))), "6: dispersion")
fm <- mmm_bf_animal_metrics(r6, 39.3970988275363)
check(near(oc$occupancy_dispersion, fm$occupancy_dispersion) && near(oc$dominant_share, fm$dominant_share), "6: agrees with the frozen metric")

# 7. Two animals on a 10-cell grid (1-s cells, window 10 s).
#    a: R1 cells 0-4, R2 cells 5-9;  b: R1 cells 0-1, R5 2-3, R2 4-9, unsupported in cells 8-9.
#    Observed (shift 0): joint 8 cells; same reader: cells 0,1,5,6,7 = 5; same column: cells 0-3 (C1) and 5-7 (C2) = 7;
#    same or neighbouring column: 8 (cell 4: a C1, b C2).
#    Independence over the 8 joint cells: a R1 5/8, R2 3/8; b R1 2/8, R5 2/8, R2 4/8 -> same 5/8*2/8 + 3/8*4/8 = 22/64;
#    columns a C1 5/8, C2 3/8; b C1 4/8, C2 4/8 -> same column 32/64; neighbouring columns 1.
ta <- amv_grid_track(data.table(s = c(0, 5), e = c(5, 10), pos = c(1L, 2L)), data.table(s = 0, e = 10), 10)
tb <- amv_grid_track(data.table(s = c(0, 2, 4), e = c(2, 4, 10), pos = c(1L, 5L, 2L)), data.table(s = 0, e = 8), 10)
check(identical(ta$pos, c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L)) && sum(tb$supported) == 8, "7: grid tracks")
st <- amv_dyad_shift_table(ta, tb, 0:9)
check(near(st$joint[1], 8) && near(st$same[1], 5) && near(st$same_col[1], 7) && near(st$adj_col[1], 8), "7: observed co-assignment")
brute <- function(k) {
  idx <- ((0:9 + k) %% 10) + 1L
  sb <- tb$supported[idx]; pb <- tb$pos[idx]; j <- ta$supported & sb
  ca <- amv_column(pmax(ta$pos, 1L)); cb <- amv_column(pmax(pb, 1L))
  c(sum(j), sum(j & ta$pos == pb), sum(j & ca == cb), sum(j & abs(ca - cb) <= 1))
}
bt <- t(vapply(0:9, brute, numeric(4)))
check(near(as.matrix(st[, .(joint, same, same_col, adj_col)]), bt), "7: every shift equals brute force")
ind <- amv_dyad_independence(ta, tb)
check(near(ind$same, 22 / 64) && near(ind$same_col, 32 / 64) && near(ind$adj_col, 1), "7: independence expectation")

# 8. Concurrency. Board 1 reader 3: T1 [0, 2], T2 [1, 1.5], T3 [1.2, 3], T1 [2.5, 2.6]; T2 [5, 6]; reader 4: T4 [0, 9];
#    board 2 reader 3: T5 [0, 9]. Overlaps on the same board and reader with another tag:
#    T1 [0,2]: T2, T3 -> 2;  T2 [1,1.5]: T1, T3 -> 2;  T3 [1.2,3]: T1 (both events), T2 -> 2 distinct tags;
#    T1 [2.5,2.6]: T3 -> 1;  T2 [5,6]: none -> 0;  T4: other reader -> 0;  T5: other board -> 0.
#    T6 [3, 4] touches T3's end only (no positive overlap) -> 0.
cc <- amv_concurrency(board = c(1, 1, 1, 1, 1, 1, 2, 1), pid = c(3, 3, 3, 3, 3, 4, 3, 3), t = c(0, 1, 1.2, 2.5, 5, 0, 0, 3),
                      e = c(2, 1.5, 3, 2.6, 6, 9, 9, 4), tag = c("T1", "T2", "T3", "T1", "T2", "T4", "T5", "T6"))
check(identical(cc, c(2L, 2L, 2L, 1L, 0L, 0L, 0L, 0L)), "8: concurrent tags per event")

# 9. System rows of a synthetic UTF-16LE log: the version and start rows, not the detections.
dir <- tempfile("amv_"); dir.create(dir)
S0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
dev <- sprintf("#ID-Device;AM1-%d;%g;%g;0;SAM", 1:8, c(0, 0, 100, 100, 200, 200, 300, 300), c(0, 116, 0, 116, 0, 116, 0, 116))
rows <- c(sprintf("%s;;;;;;;;;;version;22:04:2022;1.0.8147.28429;", excel(S0)), sprintf("%s;;;Control;;;;;;;start;x.xlsx;;", excel(S0 + 1)),
          sprintf("%s;T1;unknown;AM1-3;75;;;1;;;;0;;", excel(S0 + 2)))
path <- file.path(dir, "log.csv")
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, dev, rows), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
sm <- amv_system_messages(path)
check(identical(sm$SystemMsg, c("version", "start")) && identical(sm$unitLabel, c("", "Control")) && identical(sm$file_order, 1:2),
      "9: system rows")
check(near(sm$t, c(S0, S0 + 1), tol = 1e-6), "9: system row times")
unlink(dir, recursive = TRUE)

cat("test_antenna_measurement: all checks passed\n")
