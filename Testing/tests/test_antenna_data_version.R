# ===========================================================================
# Raw-antenna data versions (Functions/antenna_data_version.R).
# Portable: a synthetic UTF-16LE raw log, vendor file and tag map under tempdir(); nothing on the lab drive.
#
#  1. schema and order: the 12 vendor columns, sorted by time, millisecond timestamps, System = sys.<board>
#  2. seeds: one flagged row per animal at the span start (18:30) holding the reader of its last earlier detection,
#     with the detection age in the seed table; an animal without an earlier detection is not seeded
#  3. calendar: Phase/ConsecActive/ConsecInactive copied per 12-h block from the vendor file (vendor numbering kept)
#  4. change-only stream: no repeated reader per animal; the flip filter removes a short return, the unfiltered
#     version keeps it
#  5. the driver never writes into the live data root
# ===========================================================================
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_data_version.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/antenna_data_version.R")
canon <- function(x) ifelse(grepl("^[0-9]+$", x), sub("^0+(?=.)", "", x, perl = TRUE), x)

tmp <- tempfile("dv_"); dir.create(file.path(tmp, "B3", "raw"), recursive = TRUE)
S0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
grid <- data.frame(ant = 1:8, x = c(0, 0, 100, 100, 200, 200, 300, 300), y = c(0, 116, 0, 116, 0, 116, 0, 116))
dev <- sprintf("#ID-Device;AM1-%d;%g;%g;0;SAM", grid$ant, grid$x, grid$y)
ev <- function(t, tag, ant) sprintf("%s;%s;unknown;AM1-%d;300;;;2;;;;0;;", excel(t), tag, ant)
rows <- c(ev(S0 - 300, "T1", 1),                  # T1 last detection before the span: R1, 300 s earlier
          ev(S0 + 60, "T2", 3),                   # T2 has no earlier detection
          ev(S0 + 100, "T1", 2),                  # R5
          ev(S0 + 101.0004, "T1", 4),             # 1-s flip to R6 ...
          ev(S0 + 102, "T1", 2),                  # ... back to R5
          ev(S0 + 43300, "T1", 7))                # a move to R4 in the next (inactive) block
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
path <- file.path(tmp, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23.csv")
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, dev, rows), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
rec <- rfid_read_recording(tmp, "B3", "CC1")
tags <- tibble(AnimalNum = c("OR1", "4"), tag = c("T1", "T2"))
# Vendor file: two blocks (A1, then I2 - the vendor numbering skips the dropped pre-18:30 block), two animals.
vendor <- data.frame(
  DateTime = c("2023-04-24T18:31:05.644Z", "2023-04-24T19:00:00.000Z", "2023-04-25T07:00:00.000Z"),
  AnimalID = c("OR1", "0004", "OR1"), System = "sys.1", PositionID = c(1L, 2L, 3L), CageChange = "CC1", Batch = "B3",
  Phase = c("Active", "Active", "Inactive"), ConsecActive = c(1L, 1L, 0L), ConsecInactive = c(0L, 0L, 2L),
  HalfHoursElapsed = c(0L, 1L, 25L), is_synthetic_preprocessing_row = FALSE, synthetic_row_source = NA_character_)

b2 <- antenna_dv_build_recording(vendor, rec, tags, 2, canon = canon)
b0 <- antenna_dv_build_recording(vendor, rec, tags, 0, canon = canon)
r2 <- b2$rows; r0 <- b0$rows

# 1 -------------------------------------------------------------------------
check(identical(names(r2), ANTENNA_DV_COLUMNS), "1: the 12 vendor columns in vendor order")
check(!is.unsorted(r2$DateTime) && all(grepl("^\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d\\.\\d{3}Z$", r2$DateTime)),
      "1: sorted, ISO timestamps with milliseconds")
check(all(r2$System == "sys.1") && all(r2$AnimalID %in% c("OR1", "0004")), "1: System from the board, vendor ID spelling")
check(r2$DateTime[r2$AnimalID == "OR1"][2] == "2023-04-24T18:31:40.000Z", "1: detection time to the millisecond")
cat("1 schema and order: PASS\n")

# 2 -------------------------------------------------------------------------
seed <- r2[r2$is_synthetic_preprocessing_row, ]
check(nrow(seed) == 1 && seed$AnimalID == "OR1" && seed$DateTime == "2023-04-24T18:30:00.000Z" && seed$PositionID == 1L &&
        seed$synthetic_row_source == ANTENNA_DV_SEED_SOURCE, "2: OR1 is seeded at 18:30 with R1, flagged")
s <- b2$seeds
check(isTRUE(all.equal(s$seed_detection_age_s[s$AnimalNum == "OR1"], 300)), "2: the seed's detection age is kept (300 s)")
check(!s$seeded[s$AnimalNum == "4"] && is.na(s$seed_detection_age_s[s$AnimalNum == "4"]) &&
        r2$DateTime[r2$AnimalID == "0004"][1] == "2023-04-24T18:31:00.000Z", "2: animal 4 starts at its first detection, unseeded")
cat("2 seeds: PASS\n")

# 3 -------------------------------------------------------------------------
late <- r2[r2$AnimalID == "OR1" & r2$PositionID == 4L, ]
check(nrow(late) == 1 && late$Phase == "Inactive" && late$ConsecInactive == 2L && late$ConsecActive == 0L,
      "3: the inactive block keeps the vendor numbering (I2)")
check(all(r2$Phase[r2$DateTime < "2023-04-25T06:30"] == "Active") && all(r2$ConsecActive[r2$Phase == "Active"] == 1L),
      "3: the first block is A1")
check(identical(r2$HalfHoursElapsed, as.integer(animalpos_half_hours_elapsed(antenna_dv_parse_time(r2$DateTime)))),
      "3: HalfHoursElapsed from the shared helper")
check(fails <- inherits(try(antenna_dv_vendor_calendar(transform(vendor, Phase = c("Active", "Inactive", "Inactive"))), silent = TRUE), "try-error"),
      "3: inconsistent vendor labels within a block must fail")
cat("3 calendar: PASS\n")

# 4 -------------------------------------------------------------------------
or1_2 <- r2$PositionID[r2$AnimalID == "OR1"]; or1_0 <- r0$PositionID[r0$AnimalID == "OR1"]
check(identical(or1_2, c(1L, 5L, 4L)), "4: filtered: seed R1, R5, R4 (the 1-s flip to R6 removed)")
check(identical(or1_0, c(1L, 5L, 6L, 5L, 4L)), "4: unfiltered: the flip to R6 and back is kept")
check(all(tapply(r0$PositionID, r0$AnimalID, function(p) all(diff(p) != 0))), "4: no repeated reader per animal")
check(identical(antenna_dv_change_rows(c(10, 10.0002, 20), c(1L, 2L, 2L))$PositionID, 2L),
      "4: changes within one millisecond keep the later reader and merge repeats")
cat("4 change-only stream: PASS\n")

# 5 -------------------------------------------------------------------------
drv <- paste(readLines("Formatting/build_raw_antenna_data_versions.R", warn = FALSE), collapse = "\n")
check(grepl("RANT_ROOT lies inside the live data root", drv, fixed = TRUE) && grepl("Version folder exists (immutable)", drv, fixed = TRUE),
      "5: the driver refuses the live root and existing versions")
cat("5 driver guards: PASS\n")

unlink(tmp, recursive = TRUE)
cat("Raw-antenna data version checks: PASS\n")
