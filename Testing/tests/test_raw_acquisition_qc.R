# ===========================================================================
# Raw RFID acquisition QC (Functions/antenna_acquisition_qc.R, Stage 00b). Portable: synthetic data and a synthetic
# UTF-16LE raw log under tempdir(); nothing on the lab drive. Expected values are worked out by hand in the comments.
#
#  1. antenna table: the documented grid; a wrong coordinate or a missing board is caught
#  2. overlapping events on a board (touching events do not overlap)
#  3. polling-lattice shares of within-event read gaps
#  4. co-reads: the same tag at a neighbour within 1 s, per tag (another tag never counts)
#  5. homologous expectation: pooled over the other boards, leaving out boards where the pair's reader read nothing
#  6. best-neighbour ratio over informative pairs only
#  7. silent stretches and their expected co-read minutes
#  8. support age and its classes at the boundaries (coverage)
#  9. same-reader co-assignment against independence
# 10. board tail silence and the removal signature
# 11. rule evaluation on injected cases: every hard failure and warning, the classification of whole-session dropouts
#     (confirmed / probable / undecidable), schema, proposed exclusions, the frozen-S13 annotation, group-blindness
# 12. end to end on a synthetic recording with a dead antenna: zero-read detection, expected co-reads, placement-scan
#     evidence, integrity counts, coverage shares at exact cell counts
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_acquisition_qc.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, check.attributes = FALSE))
canonical_animal_id <- function(x) {   # as Functions/behavioral_dynamics_helpers.R
  x <- toupper(gsub("\\s+", "", trimws(as.character(x))))
  num <- grepl("^[0-9]+$", x); x[num] <- sub("^0+(?=.)", "", x[num], perl = TRUE)
  ifelse(is.na(x) | !nzchar(x), NA_character_, x)
}
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/animalpos_reconstruction.R")
source("Functions/antenna_data_version.R")
source("Functions/antenna_measurement.R")
source("Functions/antenna_acquisition_qc.R")

# 1 -------------------------------------------------------------------------------------------------------------------
e <- aqc_expected_devices()
check(identical(e$PositionID, c(1, 5, 2, 6, 3, 7, 4, 8)), "1: antenna k -> reader")
dev <- do.call(rbind, lapply(1:5, function(b) data.frame(board = b, e)))
check(isTRUE(aqc_device_check(dev)$devices_expected) && isTRUE(aqc_device_check(dev)$devices_on_grid), "1: documented grid passes")
bad <- dev; bad$gx[bad$board == 2 & bad$antenna == 3] <- 200
check(!aqc_device_check(bad)$devices_on_grid, "1: a moved antenna is caught")
check(!aqc_device_check(dev[dev$board != 5, ])$devices_expected, "1: a missing board is caught")

# 2. Board 1: [0,2] [1,1.5] [5,6] -> 1, 1, 0; board 2 [0.5,1.2] -> 0; touching [10,11] [11,12] -> 0, 0.
oc <- aqc_board_overlap_counts(c(1, 1, 1, 2, 1, 1), c(0, 1, 5, 0.5, 10, 11), c(2, 1.5, 6, 1.2, 11, 12))
check(identical(oc, c(1L, 1L, 0L, 0L, 0L, 0L)), "2: overlap counts")

# 3. Gaps 114.5 (k1), 414.5 and 416 (k4), 514.5 (k5), 300 (off the lattice): on 0.8, k1 0.2, k4 0.4, k5 0.2, mean 351.9.
la <- aqc_lattice(c(114.5, 414.5, 416, 514.5, 300))
check(near(la$on_lattice_share, 0.8) && near(la$lattice_k1, 0.2) && near(la$lattice_k4, 0.4) && near(la$lattice_k5, 0.2), "3: lattice shares")
check(near(la$mean_gap_ms, 351.9) && la$gaps == 5, "3: mean gap")

# 4. Board 1, tag A: R2 @10, R3 @10.5, R2 @20, R1 @30, R5 @30.9; tag B: R3 @20.3.
#    A's R2 @10 co-reads R3 (0.5 s); A's R2 @20 does not (A's R3 is 9.5 s away; B's R3 @20.3 is another tag).
#    A's R3 @10.5 co-reads R2; R1 @30 and R5 @30.9 co-read each other (column partners); B's R3 has no partner read.
rd <- data.table(tag = c("A", "A", "A", "A", "A", "B"), board = 1L, pid = c(2L, 3L, 2L, 1L, 5L, 3L), t = c(10, 10.5, 20, 30, 30.9, 20.3), block = 1)
fl <- aqc_coread_flags(rd)
setorder(fl, tag, t)
check(identical(fl$co_R3, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)), "4: co-reads at R3 (per tag only)")
check(identical(fl$co_R2, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)), "4: co-reads at R2")
check(identical(fl$co_R5, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)) && identical(fl$co_R1, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
      "4: column partners")
cn <- aqc_coread_counts(fl, by = c("board", "block"))
check(cn[j == 2 & k == 3]$O == 1 && cn[j == 2 & k == 3]$N == 2, "4: O and N for R2 -> R3")
check(cn[j == 3 & k == 2]$O == 1 && cn[j == 3 & k == 2]$N == 2, "4: O and N for R3 -> R2 (both tags' R3 reads count in N)")
check(cn[j == 1 & k == 5]$O == 1 && cn[j == 5 & k == 1]$O == 1, "4: column partner counts")
check(!nrow(cn[j == 2 & k == 4]) || cn[j == 2 & k == 4]$O == 0, "4: R4 is a neighbour of R2's row but was not read")

# 5. Pair R2 -> R3 on boards 1-3: (O, N) = (0, 100) with board 1's R3 dead, (50, 100), (30, 200).
#    board 1: (50 + 30) / 300 -> E = 26.67; board 2: board 1 left out -> 30 / 200 -> E = 15; board 3: 50 / 100 -> E = 100.
ct <- data.table(board = 1:3, Phase = "Active", j = 2L, k = 3L, type = "row", O = c(0, 50, 30), N = c(100, 100, 200))
he <- aqc_homologous_expectation(ct, data.table(board = 1L, PositionID = 3L), group = "Phase")
setorder(he, board)
check(near(he$p_hom, c(80 / 300, 30 / 200, 50 / 100)) && near(he$E, c(100 * 80 / 300, 15, 100)), "5: homologous expectation")

# 6. Reader 3 with pairs (O, E): R2 (0, 50), R4 (10, 20), R7 (5, 10; not informative) -> 2 informative pairs, best 0.5.
bn <- aqc_best_neighbour(data.table(board = 1, block = 1, k = 3L, j = c(2L, 4L, 7L), O = c(0, 10, 5), E = c(50, 20, 10)), min_E = 15)
check(bn$informative_pairs == 2 && near(bn$best_ratio, 0.5), "6: best-neighbour ratio")
bn0 <- aqc_best_neighbour(data.table(board = 1, block = 1, k = 3L, j = 2L, O = 0, E = 5))
check(bn0$informative_pairs == 0 && is.na(bn0$best_ratio), "6: no informative pair -> undecidable (NA)")

# 7. Minute 1 read, minutes 2-71 silent (70), minute 72 read, minutes 73-82 silent (10, too short).
#    P(co-read) 0.1 per minute; minutes 1-36 active -> stretch 2-71, 35 active minutes, E 7.0, E active 3.5.
ss <- aqc_silent_stretches(c(1, rep(0, 70), 2, rep(0, 10)), rep(log(0.9), 82), rep(c(TRUE, FALSE), c(36, 46)))
check(nrow(ss) == 1 && ss$first == 2 && ss$last == 71 && ss$minutes == 70 && ss$active_minutes == 35, "7: stretch bounds")
check(near(ss$E_minutes, 7) && near(ss$E_active_minutes, 3.5), "7: expected co-read minutes")
check(nrow(aqc_silent_stretches(c(1, 1, 1), rep(0, 3), rep(TRUE, 3))) == 0, "7: no stretch")

# 8. Reads at 1 and 9: ages at 0.5 (none yet), 5, 10, 70, 400 -> NA, 4, 1, 61, 391; classes at the boundaries.
ag <- aqc_age_at(c(0.5, 5, 10, 70, 400), c(9, 1))
check(is.na(ag[1]) && near(ag[-1], c(4, 1, 61, 391)), "8: support age")
check(identical(as.character(aqc_age_class(c(2, 2.01, 10, 60, 60.5, 300, 301))),
                c("le2", "2_10", "2_10", "10_60", "60_300", "60_300", "gt300")), "8: right-closed age classes")

# 9. Seconds: a R5 R5 R1 R1, b R5 R1 R1 (none) -> 3 joint seconds; observed R5 1, R1 1; shares a R5 2/3 R1 1/3,
#    b R5 1/3 R1 2/3 -> expected R5 = R1 = 2/9 * 3 = 2/3.
P <- cbind(c(5L, 5L, 1L, 1L), c(5L, 1L, 1L, 0L))
ca <- aqc_coassign_by_reader(P, P > 0)
check(near(ca$observed[c(1, 5)], c(1, 1)) && near(ca$expected[c(1, 5)], c(2 / 3, 2 / 3)) && all(ca$pair_seconds == 3), "9: co-assignment")
check(nrow(aqc_coassign_by_reader(P[, 1, drop = FALSE], P[, 1, drop = FALSE] > 0)) == 0, "9: one animal -> nothing")

# 10. Tags A (100, 200) and B (150, 190), span end 4000 -> last 200, tail 3800, spread 10 s.
bt <- aqc_board_tail(data.table(tag = c("A", "A", "B", "B"), t = c(100, 200, 150, 190)), 4000)
check(near(bt$tail_silence_s, 3800) && bt$tags == 2 && near(bt$last_read_spread_s, 10), "10: board tail")

# 11 ------------------------------------------------------------------------------------------------------------------
k <- function(src, cc = "CC3") data.table(SourceFile = src, Batch = "B9", CC = cc)
R <- rbind(cbind(k("r1"), data.table(devices = 40L, boards = "1|2|3|4|5", devices_expected = TRUE, devices_on_grid = TRUE, duplicate_events = 0L, stall_gaps = 0L,
                                     clock_fold_rows_dropped = 0L)),
           cbind(k("r2"), data.table(devices = 40L, boards = "1|2|3|4|5", devices_expected = TRUE, devices_on_grid = FALSE, duplicate_events = 3L, stall_gaps = 2L,
                                     clock_fold_rows_dropped = 5L)))
R[, `:=`(reads_ne_offsets = 0L, single_read_dur_ne_75ms = 0L, roster_missing_tag = 0L, roster_undetected = 0L, board_label_mismatch = 0L,
         nonfinite_times = 0L, backward_jumps_gt1800s = 0L, multi_read_dur_ne_read_span = 0L, zero_gaps = 0L, max_within_event_gap_s = 1.0,
         recording_silences_ge60 = 0L, recording_silence_s = 0, unpolled_pause_s_in_span = 0,
         span_covered_start = TRUE, span_covered_end = c(TRUE, FALSE), uncovered_s = c(0, 500))]
B <- rbind(cbind(k(c("r1", "r2", "r3", "r4")), data.table(board = 1L, mean_gap_ms = c(380, 381, 379, 450), lattice_k1 = 0.25)),
           cbind(k(c("r1", "r2")), data.table(board = 2L, mean_gap_ms = 400, lattice_k1 = 0.2)))
B[, `:=`(nonroster_tags_ge100 = 0L, nonroster_events = 0L, tail_silence_s = NA_real_, removal_signature = FALSE, last_read = 0, tags = 4L, last_read_spread_s = NA_real_)]
B[SourceFile == "r1" & board == 2, `:=`(tail_silence_s = 5000, removal_signature = TRUE, last_read_spread_s = 60)]
B[SourceFile == "r2" & board == 2, `:=`(tail_silence_s = 4000, removal_signature = FALSE, last_read_spread_s = 3000)]
# r1 dropouts: board 1 R3 bracketed with a healthy history -> confirmed; board 2 R3 too little neighbour traffic -> undecidable;
# board 3 R5 not bracketed (0 scan reads), no history -> probable; board 4 R3 no scans at all, healthy history -> probable;
# board 5 R3 zero reads in the scan before, healthy history -> probable (bracketing alone decides)
Q <- rbind(cbind(k("r1"), data.table(board = c(1L, 2L, 3L, 4L, 5L), PositionID = c(3L, 3L, 5L, 3L, 3L), zero_read_session = TRUE,
                                     E_session = c(500, 20, 300, 300, 300), E_tagmin = c(500, 20, 300, 300, 300),
                                     reads_before = c(50L, 9L, 0L, NA, 0L), reads_after = c(NA, NA, 0L, NA, NA), session_best_ratio = 0, reads_rel_homologous = 0)),
           cbind(k(c("r2", "r3", "r4")), data.table(board = 1L, PositionID = 3L, zero_read_session = FALSE, E_session = 400, E_tagmin = 400,
                                                    reads_before = 10L, reads_after = 10L, session_best_ratio = 1.0, reads_rel_homologous = 1)),
           cbind(k(c("r2", "r3")), data.table(board = c(4L, 4L, 5L, 5L)[1:2], PositionID = 3L, zero_read_session = FALSE, E_session = 400, E_tagmin = 400,
                                              reads_before = 10L, reads_after = 10L, session_best_ratio = 1.0, reads_rel_homologous = 1)),
           cbind(k(c("r2", "r3")), data.table(board = 5L, PositionID = 3L, zero_read_session = FALSE, E_session = 400, E_tagmin = 400,
                                              reads_before = 10L, reads_after = 10L, session_best_ratio = 1.0, reads_rel_homologous = 1)))
QB <- cbind(k(c("r2", "r1", "r1")), data.table(board = 1L, PositionID = c(2L, 3L, 4L), block = 1, Phase = c("Active", "Active", "Inactive"),
                                               informative_pairs = 2L, best_ratio = c(0.01, 0, 0), reads = 5L))
ST <- cbind(k("r1"), data.table(board = 1L, PositionID = 2L, E_active_minutes = c(250, 250, 50), dropout_session = c(FALSE, TRUE, FALSE),
                                minutes = 70L, active_minutes = 70L))
A <- cbind(k("r1"), data.table(AnimalNum = c("OR112", "X2", "X3"), board = 1L, initscan_board = c(2L, 1L, NA), initscan_matches_board = c(FALSE, TRUE, NA),
                               log_label_matches_tag_map = c(NA, FALSE, TRUE), labels = c(NA, "Y_sys.1", "X3_sys.1"), events_off_board = c(0L, 0L, 3L)))
AB <- cbind(k("r3"), data.table(AnimalNum = "X2", board = 1L, block = 1:100, Phase = "Active", reads = 100L, cagemate_median_reads = 100,
                                reads_rel_cagemates = c(0.1, rep(1, 99)), age_gt300s_share = seq(0, 0.99, length.out = 100),
                                share_seconds_differ_tau2 = seq(0, 0.99, length.out = 100), share_column_differs_tau2 = 0))
AB <- rbind(AB, cbind(k("r2"), data.table(AnimalNum = "X9", board = 2L, block = 101:105, Phase = "Active", reads = 0L, cagemate_median_reads = NA_real_,
                                          reads_rel_cagemates = NA_real_, age_gt300s_share = 1, share_seconds_differ_tau2 = 1, share_column_differs_tau2 = 1)))
tab <- list(recording_qc = R, board_qc = B, reader_qc = Q, reader_block_qc = QB, stretches = ST, animal_qc = A, animal_block_qc = AB,
            basis_crosstab = data.table(x = 1:3), coassign_by_reader = data.table(x = 1:2))
ev <- aqc_evaluate(tab)
f <- ev$flags; m <- ev$manifest
nf <- function(id) nrow(f[check_id == id])
expected_n <- c(H01 = 1, H02 = 0, H03 = 1, H04 = 0, H05 = 0, H06 = 4, W02 = 1, W03 = 1, W07 = 2, W08 = 1, W09 = 1, W10 = 1, W11 = 1, W12 = 1,
                W13 = 1, W14 = 1, W15 = 1, W16 = 1, W17 = 1)
for (id in names(expected_n)) check(nf(id) == expected_n[[id]], paste0("11: ", id, " flags ", nf(id), ", expected ", expected_n[[id]]))
h6 <- f[check_id == "H06"][order(board)]
check(identical(h6$board, c(1L, 3L, 4L, 5L)) && identical(h6$confidence, c("high", "moderate", "moderate", "moderate")) &&
        h6$classification[1] == "confirmed acquisition failure: whole-session channel dropout" &&
        all(h6$classification[-1] == "probable acquisition failure: whole-session channel dropout"),
      "11: dropout classification (confirmed only when bracketed by scan reads and healthy elsewhere)")
check(all(f[check_id %in% c("W14", "W15")]$board == 1L), "11: unobserved blocks are not ranked")
check(grepl("r1 2 3", m[check_id == "H06"]$detail), "11: an untestable silent reader is named, not flagged")
check(identical(f[check_id == "W07"][order(value)]$classification,
                c("board silent to the span end (fault or animals off the antennas; undecidable)", "probable removal of the animals (all tags stop within 10 min)")),
      "11: removal signature")
check(nrow(f[check_id == "W09" & SourceFile == "r1"]) == 0, "11: the dead reader's blocks are not repeated as W09")
check(identical(names(f), c(AQC_FLAG_COLUMNS, "in_frozen_S13")) && identical(names(m), AQC_MANIFEST_COLUMNS), "11: schema")
check(all(m$severity %in% c("hard_failure", "warning", "diagnostic")) && !anyDuplicated(m$check_id) &&
        all(m$status %in% c("pass", "flagged", "not_evaluated", "info", "undecidable")), "11: manifest values")
tab_u <- tab; tab_u$reader_qc <- Q[board == 2 | zero_read_session == FALSE]
check(aqc_evaluate(tab_u)$manifest[check_id == "H06"]$status == "undecidable", "11: only untestable silent readers -> H06 undecidable, not pass")
check(identical(m[check_id %in% c("H02", "H06", "D01")]$status, c("pass", "flagged", "info")), "11: statuses")
check(nrow(ev$exclusions) == 7 && all(ev$exclusions$status == "proposed; not applied; the frozen S13 list is unchanged"), "11: proposed exclusions")
check(any(ev$exclusions$animals_in_frozen_S13 == "OR112"), "11: frozen S13 annotated")
check(isTRUE(f[check_id == "W11"]$in_frozen_S13), "11: S13 annotation on flags")
# group-blind: no group label, CombZ or outcome table is used by the QC code
for (p in c("Functions/antenna_acquisition_qc.R", "Analysis/00b_qc_raw_acquisition.R")) {
  pd <- utils::getParseData(parse(p, keep.source = TRUE))
  sym <- pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB")]
  str <- pd$text[pd$token == "STR_CONST"]
  check(!any(sym %in% c("Group", "outcome_group", "GroupSex", "CombZ")) &&
          !any(grepl("sus_animals|con_animals|later_outcome_combz|outcome_group", str)), paste("11: group-blind", p))
}

# 12 ------------------------------------------------------------------------------------------------------------------
tmp <- tempfile("aqc_"); dir.create(file.path(tmp, "B3", "raw"), recursive = TRUE)
S0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
devrows <- unlist(lapply(1:5, function(b) sprintf("#ID-Device;AM%d-%d;%g;%g;0;SAM", b, e$antenna, e$gx, e$gy)))
evt <- function(t, tag, board, ant, gaps) data.frame(t = t, end = t + if (length(gaps) > 1) sum(gaps) / 1000 else 0.075,
  row = sprintf("%s;%s;unknown;AM%d-%d;%d;;;%d;;;;%s;;", excel(t), tag, board, ant, if (length(gaps) > 1) sum(gaps) else 75L, length(gaps), paste(gaps, collapse = ",")))
ant_of <- function(pid) e$antenna[e$PositionID == pid]
E <- list()
for (b in 1:2) {
  ta <- c("T1", "T3")[b]; tb <- c("T2", "T4")[b]
  E[[length(E) + 1]] <- evt(S0 - 100, ta, b, ant_of(2), 0); E[[length(E) + 1]] <- evt(S0 - 100, tb, b, ant_of(1), 0)
  for (i in 1:200) {
    t <- S0 + 5 + 70 * i                                                           # never at a minute boundary
    E[[length(E) + 1]] <- evt(t, ta, b, ant_of(2), c(0, 415))                     # R2, two reads
    if (b == 2) E[[length(E) + 1]] <- evt(t + 0.2, ta, b, ant_of(3), 0)           # R3 co-read on board 2 only (board 1: R3 dead)
  }
  for (i in 0:1333) {
    t <- S0 + 15 + 60 * i
    E[[length(E) + 1]] <- evt(t, tb, b, ant_of(1), 0); E[[length(E) + 1]] <- evt(t + 0.3, tb, b, ant_of(5), 0)
  }
  E[[length(E) + 1]] <- evt(S0 + 86000, ta, b, ant_of(2), 0); E[[length(E) + 1]] <- evt(S0 + 86000, tb, b, ant_of(1), 0)
}
E <- do.call(rbind, E); E <- E[order(E$end, E$t), ]
sysrows <- c(sprintf("%s;;;;;;;;;;version;22:04:2022;1.0.8147.28429;", excel(S0 - 3600)), sprintf("%s;;;Control;;;;;;;start;x.xlsx;;", excel(S0 - 3600)))
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
path <- file.path(tmp, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23.csv")
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, devrows, sysrows, E$row, sprintf("%s;;;Control;;;;;;;end;;;", excel(S0 + 86001))), collapse = "\r\n"),
                                        "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
rec <- rfid_read_recording(tmp, "B3", "CC1", keep_read_offsets = TRUE)
tags <- tibble(AnimalNum = c("OR1", "OR2", "OR3", "OR4"), tag = c("T1", "T2", "T3", "T4"))
vendor <- data.frame(DateTime = c("2023-04-24T18:31:00.000Z", "2023-04-25T07:00:00.000Z"), AnimalID = "OR1", System = "sys.1", PositionID = 1L,
                     CageChange = "CC1", Batch = "B3", Phase = c("Active", "Inactive"), ConsecActive = c(1L, 0L), ConsecInactive = c(0L, 1L),
                     HalfHoursElapsed = 0L, is_synthetic_preprocessing_row = FALSE, synthetic_row_source = NA_character_)
vendor <- rbind(vendor, transform(vendor, AnimalID = "OR2"), transform(vendor, AnimalID = "OR3", System = "sys.2"), transform(vendor, AnimalID = "OR4", System = "sys.2"))
initscan <- data.table::CJ(board = 1:5, PositionID = 1:8)[, `:=`(reads_before = 7L, reads_after = NA_integer_)]
res <- aqc_process_recording(vendor, rec, tags, amv_system_messages(path), initscan, data.table(tag = character(), initscan_board = integer()),
                             "B3", "CC1", "E9_SIS_B3_CC1_AnimalPos_preprocessed.csv")
rq <- res$recording_qc
check(rq$reads_ne_offsets == 0 && rq$single_read_dur_ne_75ms == 0 && rq$duplicate_events == 0 && rq$board_label_mismatch == 0 &&
        rq$roster_undetected == 0 && rq$backward_jumps_gt1s == 0 && rq$sessions == 1 && isTRUE(rq$devices_on_grid), "12: integrity of the synthetic log")
q1 <- res$reader_qc[board == 1 & PositionID == 3]; q2 <- res$reader_qc[board == 2 & PositionID == 3]
check(isTRUE(q1$zero_read_session) && q1$reads == 0 && !isTRUE(q2$zero_read_session) && q2$reads == 200, "12: the dead antenna is found")
# board 2: every R2 read (400) and every R2 tag-minute (200: one event per minute, 70 s apart) has an R3 co-read within 1 s,
# so p = q = 1; board 1 has 400 R2 reads in 200 tag-minutes -> E = 400 co-reads, 200 co-read tag-minutes
check(near(q1$E_session, 400) && near(q1$E_tagmin, 200) && q1$O_session == 0 && q1$reads_before == 7, "12: expected co-reads and placement-scan evidence")
check(nrow(res$stretches[board == 1 & PositionID == 3]) == 1 && isTRUE(res$stretches[board == 1 & PositionID == 3]$dropout_session),
      "12: the whole-session dropout is reported once, as a dropout row")
check(nrow(res$stretch_calibration) > 0 && all(res$animal_qc$seed_age_s > 0), "12: calibration table; seed ages positive")
ab <- res$animal_block_qc[AnimalNum == "OR2"][order(block)]
# OR2: seed read 100 s before the span; reads every 60 s (R1, R5 0.3 s later) from S0 + 15 to S0 + 79995.3, last at S0 + 86000.
# block 1: cells 0-14 have age > 60 (100.5 ... 114.5) -> 15 / 43200; block 2 (cells 43200 ... 85999): age > 300 from cell 80295
# -> 5705 / 42800.
check(near(ab$age_gt60s_share[1], 15 / 43200) && near(ab$age_gt300s_share[1], 0), "12: coverage block 1")
check(near(ab$age_gt300s_share[2], 5705 / 42800), "12: coverage block 2 (exact cell count)")
check(all(res$animal_block_qc$share_seconds_differ_tau2[res$animal_block_qc$AnimalNum == "OR2"] == 0), "12: no basis difference for alternating column partners")
ev12 <- aqc_evaluate(res)
check(nrow(ev12$flags[check_id == "H06"]) == 1 && ev12$flags[check_id == "H06"]$classification == "probable acquisition failure: whole-session channel dropout",
      "12: without other recordings a bracketed dropout is probable, not confirmed")
unlink(tmp, recursive = TRUE)
cat("test_raw_acquisition_qc: all checks passed\n")
