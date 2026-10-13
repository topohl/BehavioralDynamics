# ===========================================================================
# Relocation rule of the raw-antenna data versions (rfid_move_rule(), Functions/antenna_log_helpers.R; version ge200 in
# Functions/antenna_data_version.R). Portable: synthetic UTF-16LE raw log under tempdir(); nothing on the lab drive.
#
#  1. the rule: grid distances; >= 200 records two-column moves and never a row change or a neighbouring column;
#     strict > 200 skips exact two-column moves; the reference is the held position (no creeping); flicker never moves
#  2. ge200 on a synthetic recording: the seed is the position held at 18:30 (not the last detection), with the
#     detection age and the relocation age; rows are relocations only, consecutive rows >= 200 apart, also when two
#     detections fall into one millisecond
#  3. the default path (no rule) is byte-identical to the code before the rule existed (golden hashes recorded at
#     b23cb51 on the same fixture), and the audit columns Stage 19 writes are unchanged
#  4. the version registry and the README text: flip2s/noflip README bytes equal their 2026-10-12 builds
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
sha_csv <- function(x) { f <- tempfile(fileext = ".csv"); data.table::fwrite(x, f); digest::digest(file = f, algo = "sha256") }

# Synthetic recording: one board; antenna -> PositionID: 1 R1, 2 R5, 3 R2, 4 R6, 5 R3, 6 R7, 7 R4, 8 R8.
fixture <- function(extra_rows = character()) {
  canon <- function(x) ifelse(grepl("^[0-9]+$", x), sub("^0+(?=.)", "", x, perl = TRUE), x)
  dir <- tempfile("mr_"); dir.create(file.path(dir, "B3", "raw"), recursive = TRUE)
  S0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
  excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
  grid <- data.frame(ant = 1:8, x = c(0, 0, 100, 100, 200, 200, 300, 300), y = c(0, 116, 0, 116, 0, 116, 0, 116))
  dev <- sprintf("#ID-Device;AM1-%d;%g;%g;0;SAM", grid$ant, grid$x, grid$y)
  ev <- function(t, tag, ant) sprintf("%s;%s;unknown;AM1-%d;300;;;2;;;;0;;", excel(t), tag, ant)
  rows <- c(ev(S0 - 21600, "T1", 1),   # first detection: R1, 6 h before 18:30
            ev(S0 - 3000, "T1", 3),    # R2 (100 from R1)
            ev(S0 - 300, "T1", 4),     # R6 (153.2 from R1): the last detection before the span
            ev(S0 + 60, "T2", 3),      # T2: no earlier detection
            ev(S0 + 100, "T1", 3),     # R2
            ev(S0 + 101.0004, "T1", 5),# R3 (200 from R1): relocation
            ev(S0 + 102, "T1", 3),     # R2 (100 from R3)
            ev(S0 + 200, "T1", 1),     # R1 (200 from R3): relocation
            ev(S0 + 3700, "T2", 8),    # T2: R8 (231.2 from R2): relocation
            vapply(extra_rows, function(r) eval(parse(text = r)), ""),   # in time order: the log's clock-fold guard
            ev(S0 + 43300, "T1", 8))   # R8 in the next (inactive) block (316.6 from R1): relocation
  hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
  path <- file.path(dir, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23.csv")
  writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, dev, rows), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
  vendor <- data.frame(
    DateTime = c("2023-04-24T18:31:05.644Z", "2023-04-24T19:00:00.000Z", "2023-04-25T07:00:00.000Z"),
    AnimalID = c("OR1", "0004", "OR1"), System = "sys.1", PositionID = c(1L, 2L, 3L), CageChange = "CC1", Batch = "B3",
    Phase = c("Active", "Active", "Inactive"), ConsecActive = c(1L, 1L, 0L), ConsecInactive = c(0L, 0L, 2L),
    HalfHoursElapsed = c(0L, 1L, 25L), is_synthetic_preprocessing_row = FALSE, synthetic_row_source = NA_character_)
  list(dir = dir, rec = rfid_read_recording(dir, "B3", "CC1"), tags = tibble(AnimalNum = c("OR1", "4"), tag = c("T1", "T2")),
       vendor = vendor, canon = canon, S0 = S0)
}

# 1 -------------------------------------------------------------------------
d <- function(p, q) round(rfid_grid_distance(p, q), 1)
check(identical(c(d(1, 2), d(1, 5), d(1, 6), d(1, 3), d(1, 7), d(1, 4)), c(100, 116, 153.2, 200, 231.2, 300)), "1: grid distances")
check(all(animalpos_position_id(RFID_GRID_X, RFID_GRID_Y) == 1:8), "1: RFID_GRID_X/Y invert animalpos_position_id()")
r200 <- rfid_move_rule(200); r200s <- rfid_move_rule(200, inclusive = FALSE)
f <- function(pid, rule = r200) rfid_move_threshold_filter(pid, rule)
check(identical(f(c(1L, 5L, 2L, 6L, 1L)), rep(1L, 5)), "1: a row change, a neighbouring column and the diagonal never move")
check(identical(f(c(1L, 3L)), c(1L, 3L)) && identical(f(c(1L, 3L), r200s), c(1L, 1L)) && identical(f(c(1L, 7L), r200s), c(1L, 7L)),
      "1: >= 200 records an exact two-column move, > 200 only longer ones")
check(identical(f(c(1L, 2L, 3L)), c(1L, 1L, 3L)) && identical(f(c(1L, 2L, 1L, 2L, 5L, 6L, 5L)), rep(1L, 7)),
      "1: the reference is the held position: a walk relocates at two columns, flicker never")
check(identical(f(c(3L, 3L, 4L, 4L), rfid_move_rule(1)), c(3L, 3L, 4L, 4L)), "1: a threshold of 1 keeps every change")
check(inherits(try(rfid_move_rule(0), silent = TRUE), "try-error") && inherits(try(rfid_move_rule(200, NA), silent = TRUE), "try-error") &&
        inherits(try(rfid_move_threshold_filter(1:2, list(threshold = 200)), silent = TRUE), "try-error"), "1: invalid rules are refused")
cat("1 relocation rule: PASS\n")

# 2 -------------------------------------------------------------------------
fx <- fixture()
b <- antenna_dv_build_recording(fx$vendor, fx$rec, fx$tags, 0, canon = fx$canon, move_rule = ANTENNA_DV_VERSIONS$ge200$move_rule)
or1 <- b$rows[b$rows$AnimalID == "OR1", ]
check(identical(or1$PositionID, c(1L, 3L, 1L, 8L)), "2: OR1 rows are the seed (R1) and the relocations R3, R1, R8")
check(or1$is_synthetic_preprocessing_row[1] && or1$DateTime[1] == "2023-04-24T18:30:00.000Z" && or1$synthetic_row_source[1] == ANTENNA_DV_SEED_SOURCE_HELD,
      "2: the 18:30 seed holds the position held since the first detection, flagged as held")
s <- b$seeds[b$seeds$AnimalNum == "OR1", ]
check(s$seed_PositionID == 1L && s$seed_detection_PositionID == 6L && isTRUE(all.equal(s$seed_detection_age_s, 300)) &&
        isTRUE(all.equal(s$seed_emission_age_s, 21600)) && isTRUE(s$seed_from_first_detection),
      "2: seed R1 (not the last detection R6), detection age 300 s, relocation age 21600 s, from the first detection")
check(!b$seeds$seeded[b$seeds$AnimalNum == "4"] && identical(b$rows$PositionID[b$rows$AnimalID == "0004"], c(2L, 8L)),
      "2: animal 4 starts unseeded at its first detection and relocates once")
b0 <- antenna_dv_build_recording(fx$vendor, fx$rec, fx$tags, 0, canon = fx$canon)
check(identical(b$seeds$seed_detection_age_s, b0$seeds$seed_detection_age_s) && identical(b$seeds$seeded, b0$seeds$seeded),
      "2: seeded flags and detection ages equal the version without the rule")
check(all(c("reader_changes_after_move_rule", "seed_held_PositionID", "seed_emission_lag_seconds", "move_rule") %in% names(b$audit)),
      "2: the audit names the rule and its seed")
unlink(fx$dir, recursive = TRUE)
# Two detections in one millisecond while OR1 holds R1: R3 (a relocation from R1) and then R5 (231.2 from R3, 116 from R1).
# Without collapsing the millisecond first, both would relocate and the millisecond rounding would leave the step
# R1 -> R5 (116 units) in the rows.
fx <- fixture(c('ev(S0 + 5000.0001, "T1", 5)', 'ev(S0 + 5000.0003, "T1", 2)'))
b <- antenna_dv_build_recording(fx$vendor, fx$rec, fx$tags, 0, canon = fx$canon, move_rule = ANTENNA_DV_VERSIONS$ge200$move_rule)
or1 <- b$rows[b$rows$AnimalID == "OR1", ]
check(all(rfid_grid_distance(or1$PositionID[-1], or1$PositionID[-nrow(or1)]) >= 200), "2: consecutive rows stay >= 200 apart within a millisecond")
check(!any(grepl("19:53:20", or1$DateTime)) && identical(or1$PositionID, c(1L, 3L, 1L, 8L)),
      "2: the millisecond keeps its last detection (R5, not a relocation from R1), so no row is written")
unlink(fx$dir, recursive = TRUE)
cat("2 ge200 recording: PASS\n")

# 3 -------------------------------------------------------------------------
GOLDEN <- list(`2` = c(rows = "7da47486ce351fa39cde0d74966982674d608951ed55f5570ec1c2d8817c8cbe",
                       seeds = "40f4740cdaa938d447360e9cdeff0af7ea138f862b2d606ac25bc6c3433731c3",
                       audit = "cdfcd20058c37e24bdec73f47d6f3198cb9682bb84ec7b197fd663879ca884c2"),
               `0` = c(rows = "50d605dcfb7e209b94c2af922090e7300702cf154c7c3f68ac9ea58f1e62037b",
                       seeds = "40f4740cdaa938d447360e9cdeff0af7ea138f862b2d606ac25bc6c3433731c3",
                       audit = "2c124e1938a1daa5bf969b36ed7c7535ba21548429b66a135bfb8197d0e17290"))
fx <- fixture()
for (m in names(GOLDEN)) {
  b <- antenna_dv_build_recording(fx$vendor, fx$rec, fx$tags, as.numeric(m), canon = fx$canon)
  got <- c(rows = sha_csv(b$rows), seeds = sha_csv(b$seeds), audit = sha_csv(b$audit))
  check(identical(got, GOLDEN[[m]]), paste0("3: min_flip ", m, " without a rule must equal the b23cb51 output (", paste(names(got)[got != GOLDEN[[m]]], collapse = ", "), ")"))
}
AUDIT_COLS <- c("AnimalNum", "tag", "System_label", "Board", "board_matches_label", "log_labels", "log_label_matches_tag_map", "events_in_span",
                "events_off_board", "detected_seconds", "span_seconds", "reader_changes_raw", "reader_changes_after_flip_filter", "seeded",
                "seed_lag_seconds", "assigned_seconds")
occ <- rfid_raw_occupancy_intervals(fx$rec$events, tibble(AnimalNum = c("OR1", "4"), System_label = "sys.1"), fx$tags,
                                    fx$S0, fx$S0 + 86400, 2, canon = fx$canon)
check(identical(names(occ$audit), AUDIT_COLS), "3: the occupancy audit (Stage 19's raw_antenna_occupancy_audit) keeps its columns")
unlink(fx$dir, recursive = TRUE)
cat("3 default path unchanged: PASS\n")

# 4 -------------------------------------------------------------------------
readme_sha <- function(v) digest::digest(paste0(paste(antenna_dv_readme_lines(paste0("raw_antenna_", v, "_20261012"), "20261012",
                                                                             ANTENNA_DV_VERSIONS[[v]]), collapse = "\r\n"), "\r\n"),
                                        algo = "sha256", serialize = FALSE)
check(readme_sha("flip2s") == "49f34194070952e4b987edee5c8567a5af46eba334a835678678e65d7c8d5b7f" &&
        readme_sha("noflip") == "8924580a83cc81814b05f06dc553c34616e098db8462a56e7fa7002c0bddb5a8",
      "4: the flip2s and noflip README bytes equal their 2026-10-12 builds")
g <- ANTENNA_DV_VERSIONS$ge200
check(identical(names(ANTENNA_DV_VERSIONS), c("flip2s", "noflip", "ge200")) && ANTENNA_DV_VERSIONS$flip2s$min_flip_sec == 2 &&
        ANTENNA_DV_VERSIONS$noflip$min_flip_sec == 0 && is.null(ANTENNA_DV_VERSIONS$flip2s$move_rule) && is.null(ANTENNA_DV_VERSIONS$noflip$move_rule) &&
        g$min_flip_sec == 0 && g$move_rule$threshold == 200 && isTRUE(g$move_rule$inclusive) && g$root == "RANV",
      "4: registry: flip2s 2 s, noflip 0 s, ge200 >= 200 inclusive without a flip filter, in RANV")
rd <- antenna_dv_readme_lines("raw_antenna_ge200_20261013", "20261013", g, git_commit = "0123456789abcdef")
check(any(grepl(rfid_move_rule_label(g$move_rule), rd, fixed = TRUE)) && any(grepl("seed_emission_age_s", rd)) && grepl("code 0123456", rd[1]),
      "4: the ge200 README states the rule, the seed ages and the code commit")
drv <- paste(readLines("Formatting/build_raw_antenna_data_versions.R", warn = FALSE), collapse = "\n")
check(grepl("getOption(\"mmm.antenna_dv_versions\", c(\"flip2s\", \"noflip\"))", drv, fixed = TRUE) && grepl("belong in another root", drv, fixed = TRUE) &&
        grepl("A partial build exists", drv, fixed = TRUE) && !grepl("v == \"flip2s\"", drv, fixed = TRUE) &&
        grepl("git status failed", drv, fixed = TRUE) && grepl("test builds must not write into a version root", drv, fixed = TRUE),
      "4: the builder defaults to flip2s and noflip, guards each version's root, builds atomically, fails closed without git, and keeps test builds out of version roots")
cat("4 registry and README: PASS\n")
cat("Relocation rule checks: PASS\n")
