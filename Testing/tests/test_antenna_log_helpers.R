# ===========================================================================
# Raw antenna log helpers (Functions/antenna_log_helpers.R).
# Portable: a synthetic UTF-16LE log under tempdir(); nothing is read from or written to the lab drive.
#
#  1. file selection: one recording with detections per batch and cage change; initScan files and empty sessions ignored
#  2. reading: UTF-16LE with BOM, antenna grid from the #ID-Device rows, rows sorted by event start
#  3. flip filter: short returns to the previous antenna are removed, real moves are kept
#  4. carry-forward intervals: seeded before the span, clipped, consecutive states merged
#  5. tag map from the AnimalPos export; per-animal intervals: cage = majority board, off-board reads dropped, audit
#  6. union coverage and interval shares
# ===========================================================================
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_log_helpers.R")) &&
           dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/antenna_log_helpers.R")
canon <- function(x) ifelse(grepl("^[0-9]+$", x), sub("^0+(?=.)", "", x, perl = TRUE), x)

tmp <- tempfile("rawlog_"); dir.create(file.path(tmp, "B3", "raw"), recursive = TRUE)
excel <- function(posix_sec) sprintf("%.10f", posix_sec / 86400 + RFID_EXCEL_EPOCH_DAYS)
T0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
grid <- data.frame(ant = 1:8, x = c(0, 0, 100, 100, 200, 200, 300, 300), y = c(0, 116, 0, 116, 0, 116, 0, 116))
dev <- unlist(lapply(1:2, function(b) sprintf("#ID-Device;AM%d-%d;%g;%g;0;SAM", b, grid$ant, grid$x, grid$y)))
ev <- function(t, tag, label, board, ant, ms = 300) sprintf("%s;%s;%s;AM%d-%d;%d;;;2;;;;0;;", excel(t), tag, label, board, ant, ms)
rows <- c(
  ev(T0 - 600, "TAG1", "OR1_sys.1", 1, 1),        # seed for animal OR1 at R1 (ant 1 = 0,0)
  ev(T0 + 100, "TAG1", "OR1_sys.1", 1, 2),        # R5 (0,116)
  ev(T0 + 101, "TAG1", "OR1_sys.1", 1, 4),        # 1-s flip to R6 ...
  ev(T0 + 102, "TAG1", "OR1_sys.1", 1, 2),        # ... and back to R5
  ev(T0 + 3600, "TAG1", "OR1_sys.1", 1, 7),       # real move to R4 (300,0)
  ev(T0 + 3700, "TAG1", "OR1_sys.1", 2, 1),       # off-board read (board 2) - dropped
  ev(T0 + 50, "TAG2", "0004_sys.2", 2, 3),        # animal 0004 -> 4, no seed, starts at R2 (100,0)
  ev(T0 + 7200, "TAG9", "OR9_sys.1", 1, 1),       # not in the roster
  "45040.1;;;Control;0;;;;;;Start;;;")             # a control row without a tag
rows <- rows[c(1, 7, 2, 4, 3, 5, 6, 8, 9)]          # in order of event end, with a 1-s swap, as the device writes
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
txt <- paste(c(hdr, dev, rows), collapse = "\r\n")
path <- file.path(tmp, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23.csv")
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(txt, from = "UTF-8", to = "UTF-16LE", toRaw = TRUE)[[1]]), path)
writeLines("x", file.path(tmp, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23_initScan.csv"))

# 1 -------------------------------------------------------------------------
check(identical(rfid_raw_log_candidates(tmp, "B3", "CC1"), path), "1: the recording must be found, initScan ignored")
check(fails(rfid_raw_log_candidates(tmp, "B3", "CC2")), "1: a missing recording must fail")
rec <- rfid_read_recording(tmp, "B3", "CC1")
check(identical(rec$path, path) && length(rec$ignored) == 0 && nrow(rec$events) == 8, "1: the recording is read")
# A later session that only opened the configuration (start/end markers, no detections) is ignored.
empty <- file.path(tmp, "B3", "raw", "MultiMouseMonitor-25.01.07-Batch3_change_1_24Apr23.csv")
writeBin(iconv(paste(c(hdr, dev, "45664.66;;;Control;;;;;;;start;SIS.xlsx;;", "45664.67;;;Control;;;;;;;end;;;"),
                     collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], empty)
rec <- rfid_read_recording(tmp, "B3", "CC1")
check(identical(rec$path, path) && identical(rec$ignored, basename(empty)), "1: an empty session is ignored and named")
invisible(file.copy(path, sub("24Apr23.csv$", "24Apr23_copy.csv", path)))
check(fails(rfid_read_recording(tmp, "B3", "CC1")), "1: two recordings with detections must fail")
unlink(c(empty, sub("24Apr23.csv$", "24Apr23_copy.csv", path)))
cat("1 file selection: PASS\n")

# 2 -------------------------------------------------------------------------
lg <- rfid_read_raw_log(path)
check(nrow(lg$devices) == 16 && all(table(lg$devices$board) == 8), "2: 8 antennas on each of 2 boards")
check(identical(lg$devices$PositionID[lg$devices$board == 1], c(1L, 5L, 2L, 6L, 3L, 7L, 4L, 8L)),
      "2: antenna 1..8 (x 0,0,100,100,..., y 0,116,...) map to readers R1,R5,R2,R6,R3,R7,R4,R8")
check(nrow(lg$events) == 8, "2: the control row is not an event")
check(all(abs(lg$events$dur_sec - 0.3) < 1e-9), "2: event durations in seconds")
check(abs(min(lg$events$t) - (T0 - 600)) < 1e-3, "2: Excel serial times decode to POSIX seconds")
bad <- file.path(tmp, "bad.csv")
writeBin(iconv(paste(c(hdr, "#ID-Device;AM1-1;50;0;0;SAM", rows[1]), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], bad)
check(fails(rfid_read_raw_log(bad)), "2: an antenna off the 4 x 2 grid must fail")
check(identical(rfid_clock_fold_rows(c(100, 200, 4000, 4100, 600, 700, 4050, 4200, 4300)),
                c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)),
      "2: rows logged after the clock went back are flagged until it catches up")
check(!any(rfid_clock_fold_rows(c(100, 98, 105, 104, 200))), "2: seconds of logging jitter are not a clock fall-back")
check(lg$clock_fold_rows_dropped == 0, "2: no fall-back in the synthetic log")
cat("2 reading: PASS\n")

# 3 -------------------------------------------------------------------------
tt <- c(0, 10, 11, 12, 100, 101, 200)
pp <- c(1L, 5L, 6L, 5L, 2L, 2L, 3L)
check(identical(rfid_flip_filter(tt, pp, 2), c(1L, 5L, 5L, 5L, 2L, 2L, 3L)), "3: a 1-s A-B-A flip is removed")
check(identical(rfid_flip_filter(tt, pp, 0), pp), "3: no filter at 0 s")
check(identical(rfid_flip_filter(c(0, 1, 2, 3, 4), c(1L, 2L, 1L, 2L, 1L), 2), rep(1L, 5)), "3: repeated flicker collapses")
check(identical(rfid_flip_filter(c(0, 10, 11, 20), c(1L, 2L, 3L, 3L), 2), c(1L, 2L, 3L, 3L)),
      "3: a short run that does not return is a real move and is kept")
cat("3 flip filter: PASS\n")

# 4 -------------------------------------------------------------------------
iv <- rfid_carry_forward_intervals(c(-5, 10, 20, 30), c(1L, 1L, 2L, 3L), 0, 25)
check(identical(iv$PositionID, c(1L, 2L)) && identical(iv$start, c(0, 20)) && identical(iv$end, c(20, 25)),
      "4: seeded at the span start, merged, clipped at the span end")
iv2 <- rfid_carry_forward_intervals(c(10, 20), c(4L, 4L), 0, 30)
check(identical(iv2$start, 10) && identical(iv2$end, 30), "4: without a seed the assignment starts at the first read")
check(nrow(rfid_carry_forward_intervals(numeric(), integer(), 0, 30)) == 0, "4: no reads, no intervals")
cat("4 carry-forward: PASS\n")

# 5 -------------------------------------------------------------------------
# Tag -> animal comes from the vendor AnimalPos export of the same recording (Batch 1's raw logs say "unknown").
ap_path <- file.path(tmp, "E9_SIS_B3_CC1_AnimalPos.csv")
writeLines(c("DateTime;Animal;RFID;AM;xPos;yPos;zPos",
             "24.04.2023 18:00:00.000;OR1_sys.1;TAG1;SAM;0.0;0.0;0.0",
             "24.04.2023 18:01:00.000;OR1_sys.1;TAG1;SAM;300.0;0.0;0.0",
             "24.04.2023 18:02:00.000;0004_sys.1;TAG2;SAM;100.0;0.0;0.0",
             "24.04.2023 18:03:00.000;OR9_sys.1;TAG9;SAM;0.0;116.0;0.0"), ap_path)
tags <- rfid_animalpos_tag_map(ap_path, canon)
check(identical(tags$AnimalNum, c("OR1", "4", "OR9")) && identical(tags$tag, c("TAG1", "TAG2", "TAG9")),
      "5: the tag map resolves AnimalPos labels to canonical animals")
bad_ap <- file.path(tmp, "bad_ap.csv")
writeLines(c(readLines(ap_path), "24.04.2023 18:04:00.000;OR1_sys.1;TAG5;SAM;0.0;0.0;0.0"), bad_ap)
check(fails(rfid_animalpos_tag_map(bad_ap, canon)), "5: an animal with two tags must fail")
writeLines(c(readLines(ap_path), "24.04.2023 18:04:00.000;OR7_sys.1;TAG1;SAM;0.0;0.0;0.0"), bad_ap)
check(fails(rfid_animalpos_tag_map(bad_ap, canon)), "5: a tag shared by two animals must fail")

roster <- tibble(AnimalNum = c("OR1", "4"), System_label = c("sys.1", "sys.1"))
r <- rfid_raw_occupancy_intervals(lg$events, roster, tags, T0, T0 + 12 * 3600, 2, canon = canon)
a1 <- r$intervals %>% filter(AnimalNum == "OR1")
check(identical(a1$PositionID, c(1L, 5L, 4L)), "5: OR1 is seeded at R1, moves to R5 (flip removed), then R4")
check(isTRUE(all.equal(a1$start, c(T0, T0 + 100, T0 + 3600))) && isTRUE(all.equal(tail(a1$end, 1), T0 + 12 * 3600)),
      "5: interval bounds")
au <- r$audit
check(au$Board[au$AnimalNum == "OR1"] == 1 && au$events_off_board[au$AnimalNum == "OR1"] == 1,
      "5: cage = majority board; the board-2 read is dropped and counted")
check(au$seeded[au$AnimalNum == "OR1"] && isTRUE(all.equal(au$seed_lag_seconds[au$AnimalNum == "OR1"], 600)),
      "5: seed and its lag")
check(au$reader_changes_raw[au$AnimalNum == "OR1"] == 3 && au$reader_changes_after_flip_filter[au$AnimalNum == "OR1"] == 1,
      "5: reader changes before and after the flip filter")
check(au$Board[au$AnimalNum == "4"] == 2 && !au$board_matches_label[au$AnimalNum == "4"] && !au$seeded[au$AnimalNum == "4"],
      "5: animal 4 sits on board 2 against its sys.1 label and is not seeded")
check(all(au$log_label_matches_tag_map), "5: log labels, where present, agree with the tag map")
or_label <- lg$events %>% mutate(label = ifelse(tag == "TAG2", "OR4_sys.2", label))
check(isTRUE(rfid_raw_occupancy_intervals(or_label, roster, tags, T0, T0 + 3600, 2, canon = canon)$audit %>%
               filter(AnimalNum == "4") %>% pull(log_label_matches_tag_map)),
      "5: an OR-prefixed numeric label (OR1545 for 1545) is the same animal")
check(!"OR9" %in% r$intervals$AnimalNum, "5: animals outside the roster are ignored")
check(isTRUE(all.equal(sum(a1$end - a1$start), 12 * 3600)), "5: a seeded animal is assigned the whole span")

unlabelled <- lg$events %>% mutate(label = "unknown")
r_unl <- rfid_raw_occupancy_intervals(unlabelled, roster, tags, T0, T0 + 12 * 3600, 2, canon = canon)
check(identical(r_unl$intervals, r$intervals) && all(r_unl$audit$log_labels == "none") &&
        all(is.na(r_unl$audit$log_label_matches_tag_map)),
      "5: an unlabelled log (Batch 1) gives the same intervals through the tag map")
mislabelled <- lg$events %>% mutate(label = ifelse(tag == "TAG2", "OR7_sys.2", label))
check(isFALSE(rfid_raw_occupancy_intervals(mislabelled, roster, tags, T0, T0 + 3600, 2, canon = canon)$audit %>%
                filter(AnimalNum == "4") %>% pull(log_label_matches_tag_map)),
      "5: a log label that disagrees with the tag map is flagged")
check(fails(rfid_raw_occupancy_intervals(lg$events, tibble(AnimalNum = "OR8", System_label = "sys.1"), tags,
                                         T0, T0 + 3600, 2, canon = canon)),
      "5: a roster animal without a tag must fail")
check(identical(rfid_label_animal(c("OR426_sys.5", "00317_sys.2", "OQ772-sys.1"), canon), c("OR426", "317", "OQ772")),
      "5: vendor labels resolve to canonical animal IDs")
cat("5 per-animal intervals: PASS\n")

# 6 -------------------------------------------------------------------------
check(rfid_union_seconds(c(0, 5, 20), c(10, 12, 25)) == 17, "6: overlapping detections counted once")
check(rfid_union_seconds(numeric(), numeric()) == 0, "6: no detections")
sh <- rfid_interval_shares(a1, T0, T0 + 7200)
check(isTRUE(all.equal(sum(sh), 1)) && isTRUE(all.equal(sh[c(1, 5, 4)], c(100, 3500, 3600) / 7200)),
      "6: shares within a sub-window")
cat("6 coverage and shares: PASS\n")

unlink(tmp, recursive = TRUE)
cat("Raw antenna helper checks: PASS\n")
