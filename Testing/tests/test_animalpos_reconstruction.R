# ===========================================================================
# Reconstructed PhenoSys AnimalPos estimator (Functions/animalpos_reconstruction.R). Portable: synthetic reads and a
# synthetic UTF-16LE raw log under tempdir(); nothing on the lab drive. Expected values are worked out by hand in the
# comments.
#
#  1. vendor gate on a hand-worked stream: strict > 500 ms trigger, strict purge, one evaluation per arrival, window
#     mean of all held reads, stamp = oldest read, origin placeholder, inclusive >= 200 from the last WRITTEN position,
#     closing record; evaluation and mean-change counts
#  2. the boundary cases: a read exactly 500 ms after the oldest one joins the window and does not trigger it
#  3. gate "none": a record per change of the floor-snapped cell, first evaluation written, no placeholder or closing
#  4. reads from a raw log: MsgValue1 gaps accumulate to read times; the default reader output is unchanged
# ===========================================================================
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "animalpos_reconstruction.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/animalpos_reconstruction.R")

# One tag's reads (t ms, x, y):
#  r1 0 (0,0)  r2 100 (0,0)  r3 400 (0,0)
#  r4 700 (200,0): 700-0 > 500 -> evaluate r1..r3, mean (0,0) @0; first: 0 < 200 from origin -> write origin (0,0) @0;
#                  purge t < 200 -> oldest r3 (400)
#  r5 800 (200,0): 800-400 = 400 -> nothing
#  r6 1000 (200,0): 600 -> evaluate r3..r5 = (133.33, 0) @400; 133.3 from (0,0) -> no; purge t < 500 -> oldest r4 (700)
#  r7 1300 (300,116): 600 -> evaluate r4..r6 = (200, 0) @700; exactly 200 -> WRITE (200, 0); purge t < 800 -> oldest r5
#  r8 1350 (300,116): 550 -> evaluate r5..r7 = (233.33, 38.67) @800; 51 from (200,0) -> no; purge t < 850 -> oldest r6
#  r9 1600 (300,116): 600 -> evaluate r6..r8 = (266.67, 77.33) @1000; 102 -> no; purge t < 1100 -> oldest r7
#  end: closing record (200, 0) @1600 (the last read)
t1 <- c(0, 100, 400, 700, 800, 1000, 1300, 1350, 1600)
x1 <- c(0, 0, 0, 200, 200, 200, 300, 300, 300); y1 <- c(0, 0, 0, 0, 0, 0, 116, 116, 116)

# 1 -------------------------------------------------------------------------
v <- apr_positions(t1, x1, y1, gate = "vendor")
check(identical(v$t_ms, c(0, 700, 1600)) && identical(v$kind, c("first", "move", "closing")), "1: records at 0 (first), 700 (move), 1600 (closing)")
check(all(v$x == c(0, 200, 200)) && all(v$y == c(0, 0, 0)), "1: origin placeholder, the exact-200 move, the closing repeat")
check(identical(v$n_reads, c(3L, 3L, 0L)) && identical(v$n_antennas, c(1L, 1L, 0L)), "1: window sizes")
check(attr(v, "evaluations") == 5 && attr(v, "mean_changes") == 4, "1: five evaluations, four changes of the mean")
s <- apr_window_schedule(t1)
check(identical(s$s, c(1L, 3L, 4L, 5L, 6L)) && identical(s$j, c(4L, 6L, 7L, 8L, 9L)), "1: the evaluation schedule")
# Threshold 200.5: (200, 0) @700 is not written, so the reference stays at the origin and (233.33, 38.67) @800 lies 236.5
# from it -> written there.
v199 <- apr_positions(t1, x1, y1, gate = "vendor", threshold = 200.5)
check(identical(v199$t_ms, c(0, 800, 1600)) && isTRUE(all.equal(v199$x[2], 700 / 3)),
      "1: above 200 the exact two-column move is not written; the next mean is measured from the unchanged reference")
cat("1 vendor gate: PASS\n")

# 2 -------------------------------------------------------------------------
# r1 0 (0,0), r2 500 (300,0): 500 - 0 is not > 500 -> nothing; r3 501 (300,0): 501 > 500 -> evaluate r1..r2 = (150, 0) @0
# (the read at exactly +500 is inside the window); first: 150 < 200 -> origin; purge t < 1 -> oldest r2; closing @501.
v2 <- apr_positions(c(0, 500, 501), c(0, 300, 300), c(0, 0, 0), gate = "vendor")
check(identical(v2$t_ms, c(0, 501)) && v2$n_reads[1] == 2L && v2$x[1] == 0 && attr(v2, "evaluations") == 1,
      "2: a read at exactly +500 ms joins the window and does not trigger it; trigger strictly > 500")
check(nrow(apr_positions(c(0, 500), c(0, 300), c(0, 0))) == 0, "2: no evaluation without a read more than 500 ms after the oldest")
v2b <- apr_positions(c(0, 300, 600, 900), c(300, 300, 300, 300), c(0, 0, 0, 0))
check(v2b$x[1] == 300 && v2b$kind[1] == "first" && attr(v2b, "evaluations") == 2, "2: a first mean >= 200 from the origin is written as it is")
cat("2 window boundaries: PASS\n")

# 3 -------------------------------------------------------------------------
# Same stream, gate none: cells of the five evaluations R1 (0,0), R2 (133.3,0), R3 (200,0), R3 (233.3,38.7), R3 (266.7,77.3)
# -> records at 0 (R1), 400 (R2), 700 (R3).
n3 <- apr_positions(t1, x1, y1, gate = "none")
check(identical(n3$t_ms, c(0, 400, 700)) && identical(n3$PositionID, c(1L, 2L, 3L)) && identical(n3$kind, c("first", "move", "move")),
      "3: one record per change of the floor-snapped cell")
check(isTRUE(all.equal(n3$x[2], 400 / 3)) && n3$x_export[2] == 133.3 && n3$n_antennas[2] == 2L, "3: the record keeps the unrounded mean and its window")
check(attr(n3, "evaluations") == 5 && attr(n3, "mean_changes") == 4, "3: evaluation and mean-change counts as for the vendor gate")
check(!any(n3$kind == "closing") && n3$x[1] == 0, "3: no closing record; the first record is the first mean")
# Span counts at the boundary: reads R1 at -2000/-1900 ms, R2 at 100/300/700 ms, span from 0. The arrival at 100 evaluates the R1
# window @-2000 (before the span); the arrival at 700 evaluates (100, 300) = R2 @100: one change of the mean and of the cell,
# from the position held at the span start, counted in both (as Stage 01 counts the change from the seed).
nb <- apr_positions(c(-2000, -1900, 100, 300, 700), c(0, 0, 100, 100, 100), rep(0, 5), gate = "none", span = c(0, 1e6))
check(attr(nb, "evaluations_in_span") == 1 && attr(nb, "mean_changes_in_span") == 1 && attr(nb, "cell_changes_in_span") == 1,
      "3: the change from the held position into the span is counted for the mean and for the cell")
nf <- apr_positions(c(100, 300, 700), c(100, 100, 100), rep(0, 3), gate = "none", span = c(0, 1e6))
check(attr(nf, "evaluations_in_span") == 1 && attr(nf, "mean_changes_in_span") == 0 && attr(nf, "cell_changes_in_span") == 0,
      "3: a first record is not a change")
cat("3 no gate: PASS\n")

# 4 -------------------------------------------------------------------------
dir <- tempfile("apr_"); dir.create(dir)
S0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
dev <- sprintf("#ID-Device;AM1-%d;%g;%g;0;SAM", 1:8, c(0, 0, 100, 100, 200, 200, 300, 300), c(0, 116, 0, 116, 0, 116, 0, 116))
ev <- function(t, tag, ant, gaps) sprintf("%s;%s;unknown;AM1-%d;%d;;;%d;;;;%s;;", excel(t), tag, ant, sum(gaps), length(gaps), paste(gaps, collapse = ","))
rows <- c(ev(S0, "T1", 1, c(0, 75, 80)), ev(S0 + 0.2, "T1", 3, c(0, 70)), ev(S0 + 0.1, "T2", 5, 0))   # written at event end
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
path <- file.path(dir, "log.csv")
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, dev, rows), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
lg <- rfid_read_raw_log(path, keep_read_offsets = TRUE)
r <- apr_reads(lg$events)
t0 <- round(S0 * 1000)
check(identical(r$tag, c("T1", "T1", "T1", "T1", "T1", "T2")) && identical(r$t_ms - t0, c(0, 75, 155, 200, 270, 100)),
      "4: read times are the event start plus the accumulated MsgValue1 gaps, ordered per tag by time")
check(identical(r$gx, c(0, 0, 0, 100, 100, 200)), "4: each read takes its antenna's coordinate")
d0 <- rfid_read_raw_log(path)
check(identical(names(d0$events), c("t", "reads", "tag", "label", "unit", "dur_sec", "gx", "gy", "board", "antenna", "PositionID")),
      "4: the default reader output keeps its columns")
check(identical(d0$events, lg$events[, names(d0$events)]), "4: the opt-in only adds columns")
check(nrow(apr_reads(lg$events[0, , drop = FALSE])) == 0, "4: no events give an empty read table")
bad <- lg$events; bad$read_offsets_ms[1] <- "0,75"
check(inherits(try(apr_reads(bad), silent = TRUE), "try-error"), "4: a gap list that disagrees with the read count is refused")
unlink(dir, recursive = TRUE)
cat("4 reads: PASS\n")

# 5 -------------------------------------------------------------------------
# avg500 recording (antenna_dv_build_recording_window). Antenna -> cell: 1 R1, 3 R2, 5 R3. Reads (s from 18:30):
#  T1: -3600.0/-3599.9/-3599.8 R1; -3599.0 R1 -> evaluates the R1 window @-3600.0: first record, cell 1
#      -10.0/-9.9 R2; +5.0 R2 -> evaluates (R2, R2) @-10.0: cell 2 (written before the span: the 18:30 seed, age 10 s)
#      +100.0/+100.1/+100.2 R3 -> the arrival at +100.0 evaluates (+5.0) = R2: no change
#      +101.0 R3 -> evaluates the three R3 reads @+100.0: cell 3, in the span; +200.0 R2 -> evaluates (+101.0) = R3: no change
#  T2 (no read before the span): +60.0/+60.1 R1; +61.0 R1 -> first record @+60.0, cell 1

suppressPackageStartupMessages(library(data.table))
source("Functions/antenna_data_version.R")
canon <- function(x) ifelse(grepl("^[0-9]+$", x), sub("^0+(?=.)", "", x, perl = TRUE), x)
dir <- tempfile("apr5_"); dir.create(file.path(dir, "B3", "raw"), recursive = TRUE)
evg <- function(t, tag, ant, gaps) sprintf("%s;%s;unknown;AM1-%d;%d;;;%d;;;;%s;;", excel(S0 + t), tag, ant, sum(gaps), length(gaps), paste(gaps, collapse = ","))
rows <- c(evg(-3600, "T1", 1, c(0, 100, 100)), evg(-3599, "T1", 1, 0), evg(-10, "T1", 3, c(0, 100)), evg(5, "T1", 3, 0),
          evg(60, "T2", 1, c(0, 100)), evg(61, "T2", 1, 0), evg(100, "T1", 5, c(0, 100, 100)), evg(101, "T1", 5, 0), evg(200, "T1", 3, 0))
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, dev, rows), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]),
         file.path(dir, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23.csv"))
rec <- rfid_read_recording(dir, "B3", "CC1", keep_read_offsets = TRUE)
vendor <- data.frame(
  DateTime = c("2023-04-24T18:31:05.644Z", "2023-04-24T19:00:00.000Z", "2023-04-25T07:00:00.000Z"),
  AnimalID = c("OR1", "0004", "OR1"), System = "sys.1", PositionID = c(1L, 2L, 3L), CageChange = "CC1", Batch = "B3",
  Phase = c("Active", "Active", "Inactive"), ConsecActive = c(1L, 1L, 0L), ConsecInactive = c(0L, 0L, 2L),
  HalfHoursElapsed = c(0L, 1L, 25L), is_synthetic_preprocessing_row = FALSE, synthetic_row_source = NA_character_)
b <- antenna_dv_build_recording_window(vendor, rec, tibble(AnimalNum = c("OR1", "4"), tag = c("T1", "T2")), canon = canon)
or1 <- b$rows[b$rows$AnimalID == "OR1", ]
check(identical(names(b$rows), ANTENNA_DV_COLUMNS), "5: the 12 vendor columns")
check(identical(or1$PositionID, c(2L, 3L)) && identical(or1$DateTime, c("2023-04-24T18:30:00.000Z", "2023-04-24T18:31:40.000Z")) &&
        or1$is_synthetic_preprocessing_row[1] && or1$synthetic_row_source[1] == ANTENNA_DV_SEED_SOURCE_HELD,
      "5: OR1 holds R2 at 18:30 (seed) and changes to R3 at the first read of its window (+100 s)")
s <- b$seeds[b$seeds$AnimalNum == "OR1", ]
check(abs(s$seed_detection_age_s - 10) < 1e-3 && isTRUE(all.equal(s$seed_emission_age_s, 10)) && s$seed_window_reads == 2L,   # Excel-serial precision
      "5: detection age 10 s, the held cell set by a 2-read window 10 s before 18:30")
t2 <- b$rows[b$rows$AnimalID == "0004", ]
check(identical(t2$PositionID, 1L) && t2$DateTime == "2023-04-24T18:31:00.000Z" && !b$seeds$seeded[b$seeds$AnimalNum == "4"],
      "5: animal 4 has no read before the span and starts at its first window")
a <- b$audit[b$audit$AnimalNum == "OR1", ]
check(a$evaluations_in_span == 3 && a$mean_changes_in_span == 1 && a$cell_changes_in_span == 1 && a$cell_changes_to_unread_cell == 0 &&
        a$reads_total == 12 && a$reads_off_board == 0 && a$rows_in_span == 1, "5: in-span evaluations 3, mean changes 1, cell changes 1; 12 reads")
a4 <- b$audit[b$audit$AnimalNum == "4", ]
check(a4$cell_changes_in_span == 0 && a4$mean_changes_in_span == 0 && a4$rows_in_span == 1, "5: animal 4's first record is a row but not a change")
check(identical(antenna_dv_readme_lines("raw_antenna_avg500_20261013", "20261013", ANTENNA_DV_VERSIONS$avg500)[1:2],
                c("Exp9 RFID raw-antenna data version 'raw_antenna_avg500_20261013' (built 20261013)", "")) &&
        any(grepl("WITHOUT its 200-unit displacement gate", antenna_dv_readme_lines("x", "y", ANTENNA_DV_VERSIONS$avg500))),
      "5: the avg500 README names the estimator and the missing gate")
unlink(dir, recursive = TRUE)
cat("5 avg500 recording: PASS\n")
cat("AnimalPos reconstruction checks: PASS\n")
