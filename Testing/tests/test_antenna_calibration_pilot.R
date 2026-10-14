# ===========================================================================
# Pre-registered analysis of the one-day RFID calibration pilot (Functions/antenna_calibration_pilot.R, criteria
# acp_criteria_v1.1, schedule_one_day_v2). Portable: synthetic UTF-16LE MultiMouseMonitor logs, sheets and annotations under tempdir(), written
# as test_raw_acquisition_qc.R section 12 does; nothing on the lab drive; the runner's main block is never sourced or run.
# Expected values are worked out by hand in the comments. Sections follow T1-T16 of the analysis plan:
#
#  T1  detection classes, P1s, P10s and e by hand (reads every 414.5 ms: reliable; none: unread; alternate seconds: border)
#  T2  clapper alignment: a 2.3-s offset is recovered; a missing touch falls back to the sheet (flagged); a spurious touch
#      is ignored
#  T3  no carry-over: the previous placement's R1 read does not seed the next R8 placement
#  T4  event vs read basis on a hand-built overlap (long R5 events, one R6 read at 3 s): shares by hand
#  T5  basis decision: READ at D = +0.2, TIE equivalent at +-0.01, EVENT at -0.2, inconclusive when the boards disagree
#  T6  tau sweep: 0.3-0.8 s flicker removed at tau 1; a 5-s visit kept at tau 2 and removed at tau 10
#  T7  competition: time-sharing, collision, starvation (a 70-s gap); SR and CS/C4
#  T8  board call: difference at ratio 0.8 with 5% repeatability, no difference at 1.0, undecided when REF drift flips it
#  T9  shielding pairs unread in both states are excluded
#  T10 known-empty counting and the Poisson bound
#  T11 truth: nearest centre, 2 admissible readers at midpoints and 4 at the junction, the phase pairing table
#  T12 schedule hash pin; a swapped order is flagged; the drop order is honoured (sheet checks)
#  T13 projection test and the day-2 triggers at hand-chosen numbers
#  T14 criteria hash pin enforced; thresholds and comparators as written
#  T15 group-blind: no group label, CombZ or outcome symbol in the parsed code
#  T16 output-root guard refuses Raw Data, MMMdata and RANM/measurement_validation; a run folder is never reused
#  E2E a synthetic pilot day (the pinned schedule; T1 on boards 2 and 4) through acp_analyse() with the designed outcomes
#  V2  the amendment of 2026-10-15: schedule v2 runs every programme 4-2-2-4 and C twice per board (pooled per board by
#      10-s blocks with ids unique per placement); criteria v1.1 differ from v1.0 only in G07, G08 and the day-2 handling;
#      a board substitution needs an explicit authorization and the runner aborts on an unauthorized one
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble) })
data.table::setDTthreads(1L)
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_calibration_pilot.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
near <- function(a, b, tol = 1e-6) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1, check.attributes = FALSE))
canonical_animal_id <- function(x) {   # as Functions/behavioral_dynamics_helpers.R
  x <- toupper(gsub("\\s+", "", trimws(as.character(x))))
  num <- grepl("^[0-9]+$", x); x[num] <- sub("^0+(?=.)", "", x[num], perl = TRUE)
  ifelse(is.na(x) | !nzchar(x), NA_character_, x)
}
options(mmm.project_root = file.path(normalizePath(tempdir(), winslash = "/"), "Exp", "Analysis", "Behavior", "RFID"))
for (f in c("animalpos_preprocessing_helpers.R", "antenna_log_helpers.R", "animalpos_reconstruction.R", "antenna_measurement.R",
            "antenna_spatial_reconstruction.R", "antenna_acquisition_qc.R", "project_paths.R", "antenna_calibration_pilot.R")) {
  source(file.path("Functions", f))
}
crit <- acp_criteria()
sched <- acp_read_schedule()
tmp <- tempfile("acp_"); dir.create(tmp)

# ---- synthetic log writer (as test_raw_acquisition_qc.R section 12) ---------------------------------------------------
E_DEV <- aqc_expected_devices()
ant_of <- function(pid) E_DEV$antenna[match(pid, E_DEV$PositionID)]
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
LOG_HDR <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
TAGS <- data.table(label = c("T1", "T2", "T3", "T4", "T5", "T6", "C", "spare"), tag = sprintf("90000000000%04d", 1:8))
#' Reads (label, board, pid, t_ms) -> log event rows: consecutive reads of one tag at one antenna at most 1000 ms apart
#' form one event (MsgValue1 = the gaps, first 0; a single read lasts 75 ms); rows sorted by end time.
log_events <- function(rd) {
  rd <- data.table::copy(rd)[order(label, board, pid, t_ms)]
  n <- nrow(rd)
  rd[, ev := cumsum(c(TRUE, diff(t_ms) > 1000 | label[-1] != label[-n] | board[-1] != board[-n] | pid[-1] != pid[-n]))]
  ev <- rd[, .(start = t_ms[1], n = .N, gaps = paste(c(0, diff(t_ms)), collapse = ","), dur = if (.N > 1) t_ms[.N] - t_ms[1] else 75), by = .(ev, label, board, pid)]
  ev[, row := sprintf("%s;%s;unknown;AM%d-%d;%.0f;;;%d;;;;%s;;", excel(start / 1000), TAGS$tag[match(label, TAGS$label)], board, ant_of(pid), dur, n, gaps)]
  ev[order(start + dur, start)]
}
write_log <- function(path, rd, s0, s1) {
  ev <- log_events(rd)
  devrows <- unlist(lapply(1:5, function(b) sprintf("#ID-Device;AM%d-%d;%g;%g;0;SAM", b, E_DEV$antenna, E_DEV$gx, E_DEV$gy)))
  sysrows <- c(sprintf("%s;;;;;;;;;;version;22:04:2022;1.0.8147.28429;", excel(s0 - 60)), sprintf("%s;;;Control;;;;;;;start;x.xlsx;;", excel(s0)))
  txt <- paste(c(LOG_HDR, devrows, sysrows, ev$row, sprintf("%s;;;Control;;;;;;;end;;;", excel(s1))), collapse = "\r\n")
  writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
  invisible(path)
}
#' Read times (ms) every 414/415 ms (mean 414.5, lattice k4) from t0 (s) until before t1 (s).
cadence <- function(t0, t1, every = 1) {
  s <- round(t0 * 1000); n <- floor((t1 - t0) * 1000 / (414.5 * every) - 1e-9)
  if (n < 0) return(numeric())
  s + c(0, cumsum(rep(c(414, 415) * every, length.out = n)))
}
rd_of <- function(label, board, pid, t_ms) data.table(label = label, board = as.integer(board), pid = as.integer(pid), t_ms = as.numeric(t_ms))
fmt <- function(s) format(as.POSIXct(s, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%OS3")
S0 <- as.numeric(as.POSIXct("2026-11-03 08:00:00", tz = "UTC"))
win1 <- function(id, ws, we, start = ws, end = we, tag = "T1", board = 2L, unit_type = "P", pair_id = "P01") data.table(
  placement_id = id, board = board, phys_board = board, tag_label = tag, start = start, end = end, ws = ws, we = we, T = we - ws,
  use = TRUE, unit_type = unit_type, pair_id = pair_id)

# ---- a small synthetic log for T1, T3, T4 and T6 ------------------------------------------------------------------------
A0 <- S0 + 100
small <- rbind(
  rd_of("T1", 2, 6, cadence(A0, A0 + 60)),                                       # T1 reliable: R6 every 414.5 ms for 60 s
  rd_of("T2", 2, 6, unlist(lapply(seq(0, 58, by = 2), function(k) A0 + k + 0.1)) * 1000),   # alternate seconds, one read each
  rd_of("T1", 4, 1, (A0 + 200) * 1000),                                          # T3: an R1 read before the R8 placement
  rd_of("T1", 4, 8, cadence(A0 + 220, A0 + 260)),                                # T3: R8 from 220 s (window 215-275)
  rd_of("T3", 2, 5, c(cadence(A0 + 400, A0 + 408), cadence(A0 + 410, A0 + 420))),   # T4: R5 events 400-408 and 410-420
  rd_of("T3", 2, 6, (A0 + 403) * 1000))                                          # T4: one R6 read at 3 s
# T6: static flicker at R5 with R6 runs of 0.3-0.8 s (T4 tag at board 4), and a 5-s visit to R6 (T5 tag at board 4)
fl_r6 <- c(0.3, 0.45, 0.8, 0.4, 0.6)
fl <- rd_of("T4", 4, 5, cadence(A0 + 600, A0 + 660))
for (k in seq_along(fl_r6)) {
  a <- A0 + 600 + 10 * k
  fl <- fl[!(t_ms >= a * 1000 & t_ms <= (a + fl_r6[k]) * 1000)]
  fl <- rbind(fl, rd_of("T4", 4, 6, a * 1000), rd_of("T4", 4, 5, round((a + fl_r6[k]) * 1000)))   # an R6 run of exactly fl_r6[k] s
}
vis <- rbind(rd_of("T5", 4, 5, c(cadence(A0 + 800, A0 + 820), cadence(A0 + 825, A0 + 840))), rd_of("T5", 4, 6, cadence(A0 + 820, A0 + 825)))
small <- rbind(small, fl, vis)
small_path <- write_log(file.path(tmp, "small.csv"), small, S0, S0 + 2000)
ss <- acp_read_session(small_path, TAGS)
check(nrow(ss$reads) == nrow(small) && all(ss$reads$tag_label %in% TAGS$label) && ss$n_sessions == 1L && isTRUE(ss$device_check$devices_on_grid),
      "setup: the small log is read back read for read")

# T1 ---------------------------------------------------------------------------------------------------------------------
# window [A0, A0 + 60): T1 at R6 every 414.5 ms -> 145 reads (0 ... 59.688 s), every 1-s and 10-s bin read: P1s = P10s = 1,
# r = 145 / 60, e = r / 2.45 with lambda_sat 2.45; all 144 within-event gaps 414/415 ms: lattice k4, missed share 0.
# T2 (alternate seconds: reads at 0.1, 2.1, ... 58.1): 30 of 60 1-s bins -> P1s 0.5 (border), P10s 1.
# T6 tag T6 never read -> unread, Gmax 60.
w_t1 <- rbind(win1("t1a", A0, A0 + 60, tag = "T1", pair_id = "P01"), win1("t1b", A0, A0 + 60, tag = "T2", pair_id = "P02"),
              win1("t1c", A0, A0 + 60, tag = "T6", pair_id = "P03"))
tr_t1 <- cbind(w_t1[, .(placement_id, board, phys_board, pair_id, unit_type, tag_label, x = 100, y = 116, role = "single")],
               acp_position_truth(w_t1[, .(phys_board, x = 100, y = 116)], acp_survey_centres(ss$reads, NULL, 2L)))
det1 <- acp_detection(ss$reads, w_t1, tr_t1, crit, lambda_sat = data.table(board = 2L, lambda_sat = 2.45))
d0 <- det1[PositionID == 0L][order(placement_id)]
check(identical(d0$class, c("reliable", "border", "unread")), "T1: classes reliable / border / unread")
check(d0$reads[1] == 145 && near(d0$P1s[1], 1) && near(d0$P10s[1], 1) && near(d0$r[1], 145 / 60) && near(d0$e[1], 145 / 60 / 2.45), "T1: reads, P1s, P10s, r, e")
check(near(d0$lattice_k1[1], 0) && d0$gaps_n[1] == 144 && near(d0$missed_share[1], 0) && near(d0$mean_gap_ms[1], (72 * 414 + 72 * 415) / 144), "T1: within-event lattice")
check(near(d0$P1s[2], 0.5) && near(d0$P10s[2], 1) && near(d0$gmax[3], 60) && d0$reads[3] == 0, "T1: border P1s 0.5; the unread tag has Gmax = T")
check(all(det1[placement_id == "t1a" & PositionID %in% c(1:5, 7:8)]$reads == 0) && det1[placement_id == "t1a" & PositionID == 6L]$reads == 145,
      "T1: per-antenna rows; reader 0 is the admissible set (R6)")

# T2 ---------------------------------------------------------------------------------------------------------------------
# Clapper reads at 1000, 1100, 1200 (5 reads each, 2 s), a spurious touch at 1150; the touch for 1300 is missing.
# Sheet times are 2.3 s early. Offset = median(2.3, 2.3, 2.3) = 2.3; B's end falls back to 1297.7 + 2.3 = 1300.
cl <- rbindlist(lapply(c(1000, 1100, 1150, 1200), function(a) data.table(tag_label = "C", board = 5L, PositionID = 4L, t = a + (0:4) * 0.4145)))
tc <- acp_clapper_touches(cl)
check(nrow(tc) == 4 && near(tc$on, c(1000, 1100, 1150, 1200)) && all(tc$n_reads == 5) && near(tc$off, c(1000, 1100, 1150, 1200) + 4 * 0.4145),
      "T2: clapper reads grouped into touches")
sh2 <- data.table(placement_id = c("A", "B"), status = "executed", pc_start_t = c(1000, 1200) - 2.3, pc_end_t = c(1100, 1300) - 2.3)
al <- acp_align(sh2, tc)
check(near(al$offset_s, 2.3) && al$n_matched == 3L, "T2: the 2.3-s offset is recovered from the matched touches")
p <- al$placements[order(placement_id)]
check(near(p$start, c(1000, 1200)) && near(p$end, c(1100, 1300)) && identical(p$end_source, c("clapper", "sheet")) && identical(p$start_source, c("clapper", "clapper")),
      "T2: a missing touch falls back to sheet + offset, flagged as source sheet")
check(identical(al$touches[order(on)]$matched, c(TRUE, TRUE, FALSE, TRUE)), "T2: the spurious touch is not used (unmatched)")

# T3 ---------------------------------------------------------------------------------------------------------------------
# Previous placement: T1 read at R1 at A0 + 200. Next placement (R8) starts at A0 + 210, window [215, 275) s; first R8 read
# at 220 s = window second 5. Both bases: cells 1-50 (mids 0.05-4.95 s) unassigned, then R8; the R1 read is never used.
w3 <- win1("t3", A0 + 215, A0 + 275, start = A0 + 210, end = A0 + 280, board = 4L)
g3 <- acp_reconstruct(ss$events, ss$reads, w3, tau = c(0, 2))
check(all(g3[cell <= 50]$pos == 0L) && all(g3[cell > 50 & cell <= 445]$pos == 8L) && !any(g3$pos == 1L), "T3: no carry-over from the previous placement")
w3b <- win1("t3b", A0 + 215, A0 + 275, start = A0 + 190, end = A0 + 280, board = 4L)   # had the placement started before the R1 read:
check(all(acp_reconstruct(ss$events, ss$reads, w3b, tau = 0)[cell <= 50]$pos == 1L), "T3: (control) an own earlier read does seed")

# T4 ---------------------------------------------------------------------------------------------------------------------
# T3 tag, window [400, 420) (start = ws): R5 reads 400-407.875 (one event) and 410-419.947 (a second event: the gap
# 407.875 -> 410 exceeds 1 s); one R6 read at 403.000. Event basis: R5 [0,3), R6 [3,10) (held until the next R5 event
# start), R5 [10,20): correct 130 of 200 cells = 0.65 at tau 0 and 2 (the R6 run lasts 7 s). Read basis: the R6 read
# lasts until the next R5 read at 403.316 -> cells 3.05, 3.15, 3.25 at R6 with tau 0 (197/200 = 0.985); at tau 2 the 0.316-s
# run is removed: 1.0.
w4 <- win1("t4", A0 + 400, A0 + 420, tag = "T3")
g4 <- acp_reconstruct(ss$events, ss$reads, w4, tau = c(0, 2))
c4 <- acp_confusion(g4, data.table(placement_id = "t4", tag_label = "T3", adm = acp_mask(5L)))
v4 <- function(b, tt) c4[basis == b & tau == tt]$correct_reader
check(near(v4("event", 0), 0.65) && near(v4("event", 2), 0.65), "T4: the event basis holds R6 until the next R5 event start")
check(near(v4("read", 0), 0.985) && near(v4("read", 2), 1), "T4: the read basis returns to R5 at the next read")
check(near(c4[basis == "event" & tau == 2]$col_pm1, 0.35) && near(c4[basis == "event" & tau == 2]$correct_column, 0.65) &&
        near(c4[basis == "event" & tau == 2]$unassigned, 0), "T4: the R6 time counts as +-1 column")

# T5 ---------------------------------------------------------------------------------------------------------------------
mk_units <- function(d2, d4, sd = 0.01) {
  set.seed(5)
  u <- rbindlist(lapply(c(2L, 4L), function(b) {
    d <- if (b == 2L) d2 else d4
    rbind(data.table(board = b, unit_type = "static", unit_id = paste0("s", 1:4, "_", b), cluster = paste0("s", 1:4, "_", b)),
          data.table(board = b, unit_type = "crossing", unit_id = paste0("c", 1:24, "_", b), cluster = paste0("p", rep(1:8, each = 3), "_", b)),
          data.table(board = b, unit_type = "visit", unit_id = paste0("v", 1:12, "_", b), cluster = paste0("v", 1:12, "_", b)))[, d := d + rnorm(.N, 0, sd)]
  }))
  u
}
ctr <- data.table(board = rep(c(2L, 4L), each = 11), cluster = paste0("ctr", 1:22), d = 0)
dec5 <- function(d2, d4) acp_decide_basis(acp_basis_statistics(mk_units(d2, d4), ctr, crit, B = 1000L), crit)[statistic == "D_col"]$decision
check(dec5(0.2, 0.2) == "READ" && dec5(-0.2, -0.2) == "EVENT", "T5: READ at +0.2, EVENT at -0.2")
check(dec5(0.01, 0.01) == "TIE_equivalent" && dec5(-0.01, -0.01) == "TIE_equivalent", "T5: TIE equivalent at +-0.01")
check(dec5(0.3, -0.1) == "TIE_inconclusive", "T5: inconclusive when the boards disagree (D_col 0.1, but D_col(b4) < 0)")
check(dec5(0.2, -0.2) == "TIE_equivalent", "T5: (literal ordering) a pooled CI inside +-0.05 is equivalent even with boards of opposite sign")
st5 <- acp_basis_statistics(mk_units(0.2, 0.2, sd = 0), ctr, crit, B = 200L)
check(near(st5[statistic == "D"]$estimate, 0.2) && near(st5[statistic == "D"]$lo, 0.2) && near(st5[statistic == "D_ctr"]$estimate, 0), "T5: D by hand (no noise)")
ctr_bad <- copy(ctr)[, d := c(rep(-0.2, 11), rep(-0.2, 11))]
check(acp_decide_basis(acp_basis_statistics(mk_units(0.2, 0.2), ctr_bad, crit, B = 500L), crit)[statistic == "D_col"]$decision == "TIE_inconclusive",
      "T5: READ needs non-inferiority at the centres (D_ctr lower CI >= -0.02)")
check(acp_decide_basis(data.table(statistic = "D", estimate = 0.2, lo = 0.1, hi = 0.3, n_units = 1L), crit)[statistic == "D_col"]$decision == "not_obtained",
      "T5: a missing statistic -> not_obtained")

# T6 ---------------------------------------------------------------------------------------------------------------------
# Flicker: T4 at R5 (board 4) from 600 s with five single R6 reads starting runs of 0.3, 0.45, 0.8, 0.4, 0.6 s (the next
# R5 read ends each run): 10 raw changes; a run is removed when shorter than tau: tau 0.5 keeps 0.8 and 0.6 (4 changes),
# tau 1 removes all. The visit: T5 at R5, R6 for 5 s (820-825), back at R5: kept at tau
# 2 and 5 (the run lasts 5.0 s, not shorter), removed at tau 10.
w6 <- rbind(win1("t6", A0 + 605, A0 + 655, start = A0 + 600, end = A0 + 660, tag = "T4", board = 4L, unit_type = "P", pair_id = "P14"))
f6 <- acp_flicker(ss$events, ss$reads, w6, crit)$changes[basis == "read"]
check(all(f6$changes_raw == 10) && identical(f6$changes_kept, c(4L, 0L, 0L, 0L, 0L, 0L)), "T6: static flicker removed at tau 1 (read basis)")
r6 <- acp_flicker(ss$events, ss$reads, w6, crit)$runs[basis == "read"]
# every interior run flanked by one reader is listed: the five R6 runs and the four R5 runs between them (10 s less the flicker)
check(nrow(r6) == 9 && near(r6[at == 6L]$run_s, fl_r6, tol = 2e-3) && near(r6[at == 5L]$run_s, 10 - fl_r6[1:4], tol = 2e-3), "T6: A-B-A runs and their lengths")
w6v <- win1("t6v", A0 + 805, A0 + 835, start = A0 + 800, end = A0 + 840, tag = "T5", board = 4L, unit_type = "M_visits", pair_id = "M-visits-1")
u6 <- data.table(unit_id = "v1", placement_id = "t6v", board = 4L, unit_type = "visit", cluster = "v1", us = A0 + 817, ue = A0 + 828, from = 5L, to = 6L,
                 t_cross = A0 + 820, t_back = A0 + 825, speed = "visit", visit = 1L, dwell_nominal = 5)
v6 <- acp_visits(ss$events, ss$reads, w6v, u6, crit)
check(identical(v6[basis == "read"][order(tau)]$kept, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)) &&
        identical(v6[basis == "event"][order(tau)]$kept, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)), "T6: the 5-s visit is kept at tau 2 and 5, removed at 10")
check(near(v6[basis == "read" & tau == 2]$measured_s, 5, tol = 2e-3) && near(v6$true_dwell[1], 5), "T6: measured run length against the true dwell")
# tau* on built inputs: removal >= 0.99 on both boards with >= 50 changes, >= 7 of 8 five-second visits kept
fc <- data.table(placement_id = "x", board = rep(c(2L, 4L), each = 6), pair_id = "P14", basis = "read", tau = rep(acp_set(crit, "B11a"), 2),
                 changes_raw = 60L, changes_kept = rep(c(30L, 0L, 0L, 0L, 0L, 0L), 2))
vv <- data.table(basis = "read", dwell_nominal = 5, tau = rep(acp_set(crit, "B11a"), each = 8), kept = rep(c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE), each = 8))
dt6 <- acp_decide_tau(list(changes = rbind(fc, copy(fc)[, basis := "event"])), rbind(vv, copy(vv)[, basis := "event"]),
                      data.table(PositionID = 0L, pair_id = "P07", P1s = 1), crit)
check(dt6[statistic == "tau_star_read"]$decision == "tau_star" && near(dt6[statistic == "tau_star_read"]$estimate, 1), "T6: tau* = 1")
dt6b <- acp_decide_tau(list(changes = copy(fc)[, changes_raw := 40L][, changes_kept := 0L]), vv, data.table(PositionID = 0L, pair_id = "P07", P1s = 1), crit)
check(dt6b[statistic == "tau_star_read"]$decision == "not_decidable", "T6: fewer than 50 changes on a board -> not decidable (D5)")
dt6c <- acp_decide_tau(list(changes = fc), copy(vv)[, kept := FALSE], data.table(PositionID = 0L, pair_id = "P07", P1s = 1), crit)
check(dt6c[statistic == "tau_star_read"]$decision == "range" && near(dt6c[statistic == "tau_star_read"]$lo, 1) && is.na(dt6c[statistic == "tau_star_read"]$hi),
      "T6: no tau qualifies -> a range")

# T7 ---------------------------------------------------------------------------------------------------------------------
# Board 2 C run with regular reads: sentinels T5 (R8) and T6 (R4) at 2 reads/s in every period; C1: T1 at R5 at 2.4/s
# (288 reads in 120 s); C4: T1-T4 at R5 at 0.6/s each (180 reads each, 720 in all; TR = (720.5 / 300) / (288.5 / 120)
# = 0.9983; MS = 0.25); CS: T1, T2 at R5 and T3, T4 at R6 at 1.2/s (180 each): CS/C4 = (180.5 / 150) / (180.5 / 300) = 2;
# SR = (1200.5 / 300) / (480.5 / 120) = 0.9994.
reg <- function(label, pid, a, b, rate) data.table(tag_label = label, board = 2L, PositionID = as.integer(pid), t = seq(a, b - 1e-9, by = 1 / rate))
c_w <- data.table(placement_id = c("c0", "c1", "c4", "cs", "ce"), pair_id = ACP_C_PERIODS, ws = c(0, 100, 300, 700, 900), we = c(60, 220, 600, 850, 960))
c_truth <- function() {
  tr <- merge(c_w[, .(placement_id, pair_id)], as.data.table(ACP_C_ROLES), by = "pair_id", allow.cartesian = TRUE)
  cbind(tr[, .(placement_id, board = 2L, phys_board = 2L, pair_id, unit_type = "C", tag_label, role)],
        acp_position_truth(tr[, .(phys_board = 2L, x, y)], acp_survey_centres(NULL, NULL, 2L)))
}
c_reads <- function(c4_rate = 0.6, gap_T4 = FALSE, sent_c4 = 2) {
  r <- rbind(
    rbindlist(lapply(seq_len(nrow(c_w)), function(i) rbind(reg("T5", 8, c_w$ws[i], c_w$we[i], if (c_w$pair_id[i] == "C4") sent_c4 else 2),
                                                          reg("T6", 4, c_w$ws[i], c_w$we[i], if (c_w$pair_id[i] == "C4") sent_c4 else 2)))),
    reg("T1", 5, 100, 220, 2.4),
    rbindlist(lapply(c("T1", "T2", "T3", "T4"), function(tg) reg(tg, 5, 300, 600, c4_rate))),
    reg("T1", 5, 700, 850, 1.2), reg("T2", 5, 700, 850, 1.2), reg("T3", 6, 700, 850, 1.2), reg("T4", 6, 700, 850, 1.2))
  if (gap_T4) r <- r[!(tag_label == "T4" & t >= 400 & t < 470)]
  r
}
comp_dec <- function(...) acp_decide_competition(acp_competition(c_reads(...), c_w, c_truth(), crit, B = 300L), crit)[board == "2"]
d7 <- comp_dec()
check(d7[statistic == "competition_call"]$decision == "time_sharing" && near(d7[statistic == "TR"]$estimate, (720.5 / 300) / (288.5 / 120)) &&
        near(d7[statistic == "MS"]$estimate, 0.25), "T7: time-sharing (TR and MS by hand)")
check(near(d7[statistic == "CS_C4"]$estimate, 2) && d7[statistic == "CS_C4"]$decision == "per_antenna", "T7: CS/C4 = 2 -> sharing per antenna")
check(near(d7[statistic == "SR"]$estimate, (1200.5 / 300) / (480.5 / 120)) && d7[statistic == "SR"]$decision == "none", "T7: SR = 1 with a CI inside [0.85, 1.15]")
check(comp_dec(c4_rate = 0.3)[statistic == "competition_call"]$decision == "collision", "T7: TR 0.5 -> collision")
d7g <- comp_dec(gap_T4 = TRUE)
check(d7g[statistic == "competition_call"]$decision == "capture_or_starvation" && d7g[statistic == "max_gap_C4_s"]$estimate >= 70,
      "T7: a 70-s gap of one tag in C4 -> starvation")
check(comp_dec(sent_c4 = 1.4)[statistic == "SR"]$decision == "board_level", "T7: sentinels at 0.7 x baseline under load -> board-level competition")

# T8 ---------------------------------------------------------------------------------------------------------------------
# 21 pairs: true-reader rates r2 = 2 /s and r4 = 1.6 /s (ratio 0.8; e 0.8 and 0.64, not saturated); co-reads 30 and
# 30 x ratio per 60 s; REF-x replicates on each board at 1.0 /s within 5% (60, 63, 57 reads), REF-z centred (e 0.98: not
# informative). R_board = exp(mean of 21 x log(96.5 / 120.5) and REF-x log(180.5 / 180.5) = 0).
mk_det <- function(ratio = 0.8, refx2 = c(60, 63, 57), refx4 = c(60, 63, 57)) {
  base <- rbindlist(lapply(c(2L, 4L), function(b) data.table(board = b, pair_id = ACP_BOARD_PAIRS, placement_id = paste0(ACP_BOARD_PAIRS, "_", b),
                                                              reads = if (b == 2L) 120 else round(120 * ratio), T = 60, P1s = 1)))
  ref <- rbindlist(lapply(c(2L, 4L), function(b) rbind(
    data.table(board = b, pair_id = "REFx", placement_id = paste0("REFx", 1:3, "_", b), reads = if (b == 2L) refx2 else refx4, T = 60, P1s = 1),
    data.table(board = b, pair_id = "REFz", placement_id = paste0("REFz", 1:3, "_", b), reads = 98, T = 40, P1s = 1))))
  d <- rbind(base, ref)
  d[, `:=`(PositionID = 0L, r = reads / T, e = (reads / T) / 2.5, adm_readers = "6", ws = 3600 * (match(placement_id, unique(placement_id)) %% 7), we = 0)]
  d[, we := ws + T]
  d[, mean_gap_ms := 414.5]
  d[]
}
co8 <- function(d, ratio) d[, .(placement_id, coread_true = as.integer(ifelse(board == 2L, 30L, round(30 * ratio))))]
board_dec <- function(ratio) { d <- mk_det(ratio); ref <- acp_ref_repeatability(d, crit); br <- acp_board_ratio(d, co8(d, ratio), ref, crit, B = 500L)
  list(dec = acp_decide_board(br, ref, crit), br = br, ref = ref) }
b8 <- board_dec(0.8)
check(near(b8$br$stats[statistic == "R_board"]$estimate, exp(mean(c(rep(log(96.5 / 120.5), 21), log(180.5 / 180.5))))) &&
        b8$dec[statistic == "board_call"]$decision == "difference", "T8: ratio 0.8 with 5% repeatability -> hardware difference")
check(all(b8$ref$excess_sd == 0) && near(b8$ref$late_early[1], 57.5 / 60.5), "T8: REF excess SD (counting noise exceeds the spread) and late/early")
check(!b8$br$pairs[pair_id == "REFz"]$informative && b8$br$pairs[pair_id == "P01"]$informative, "T8: saturated REF-z not informative")
check(board_dec(1)$dec[statistic == "board_call"]$decision == "no_difference", "T8: ratio 1 -> no difference on the bench")
st8 <- data.table(statistic = c("R_board", "R_board_drift_adjusted", "coread_ratio"), estimate = c(1, 1, 1), lo = c(0.95, 0.95, 0.9), hi = c(1.05, 1.05, 1.1), n_units = 21L)
ref_ok <- data.table(board = c(2L, 4L), replicates = 3L, excess_sd = 0.05, late_early = 1, refx_informative = TRUE)
check(acp_decide_board(list(stats = st8, by_group = data.table()), ref_ok, crit)[statistic == "board_call"]$decision == "no_difference", "T8: ratio near 1 -> no difference")
st8b <- data.table(statistic = c("R_board", "R_board_drift_adjusted", "coread_ratio"), estimate = c(0.8, 0.97, 0.9), lo = c(0.74, 0.9, 0.8), hi = c(0.86, 1.04, 1.02), n_units = 21L)
d8b <- acp_decide_board(list(stats = st8b, by_group = data.table()), ref_ok, crit)
check(d8b[statistic == "board_call"]$decision == "undecided" && d8b[statistic == "board_call_drift_adjusted"]$decision != "difference", "T8: undecided when REF drift flips the call")
check(acp_decide_board(list(stats = copy(st8b)[statistic == "R_board_drift_adjusted", `:=`(estimate = 0.8, lo = 0.74, hi = 0.86)], by_group = data.table()),
                       copy(ref_ok)[board == 4L, excess_sd := 0.12], crit)[statistic == "board_call"]$decision == "undecided",
      "T8: REF excess SD above 0.10 on a board -> no difference call")

# T9 ---------------------------------------------------------------------------------------------------------------------
# HCx and HCz on boards 2 and 4; HCz on board 4 unread on and off (P1s 0) -> excluded. Included log ratios:
# HCx b2 log(114.5 / 120.5), HCz b2 log(108.5 / 120.5), HCx b4 log(60.5 / 60.5) -> house ratio exp(mean).
sh9 <- data.table(board = rep(c(2L, 4L), each = 4), pair_id = rep(c("H01", "H02", "H03", "H04"), 2), PositionID = 0L, T = 60,
                  reads = c(114, 120, 108, 120, 60, 60, 0, 0), P1s = c(1, 1, 1, 1, 0.9, 0.9, 0, 0))
s9 <- acp_shielding(sh9, crit, B = 300L)
check(identical(s9$pairs[order(board, pair)]$included, c(TRUE, TRUE, TRUE, FALSE)), "T9: the pair unread in both states is excluded")
check(near(s9$stats$estimate, exp(mean(c(log(114.5 / 120.5), log(108.5 / 120.5), 0)))) && s9$stats$n_units == 3, "T9: pooled ratio over the included pairs")
d9 <- acp_decide_shielding(s9, sh9[, class := "reliable"], crit)
check(d9[statistic == "house_ratio"]$decision != "no_shielding", "T9: P1s(on, HCz) on board 4 below 0.95 -> not 'no shielding'")

# T10 --------------------------------------------------------------------------------------------------------------------
# Session span [0, 7200): boards 1, 3, 5 idle all of it (2 h each); board 2 idle while board 4 is in visit V2 [1000, 2000)
# and in the baseline [0, 600); board 4 idle during V1 (first start 2000 to last end 3000) and the baseline. Events: one T3 read on board 1 (a
# phantom), tag C on board 5 (the clapper, excluded), T1 on board 2 at 2200 (not idle), T2 on board 4 at 2600 (phantom).
w10 <- data.table(use = TRUE, visit = c("S", "V1", "V2"), block = c("BASELINE", "P1", "P1"), phys_board = c(NA, 2L, 4L), start = c(0, 2000, 1000), end = c(600, 3000, 2000))
w10 <- rbind(w10, data.table(use = TRUE, visit = "V1", block = "P1", phys_board = 2L, start = 2500, end = 2600))
idle <- acp_idle_intervals(w10, c(0, 7200))
check(near(idle[board == 2L, sum(e - s)], 1600) && near(idle[board == 4L, sum(e - s)], 600 + 1000) && near(idle[board == 1L, sum(e - s)], 7200),
      "T10: idle time per board")
ev10 <- data.table(board = c(1L, 5L, 2L, 4L), t = c(100, 50, 2200, 2600), tag_label = c("T3", "C", "T1", "T2"))
ke <- acp_known_empty(ev10, idle, crit)
H <- (3 * 7200 + 1600 + 1600) / 3600
check(ke[is.na(board)]$events == 2 && near(ke[is.na(board)]$board_hours, H) && identical(ke[is.na(board)]$tags, "T3|T2"), "T10: phantom events counted per board-hour")
check(near(ke[is.na(board)]$upper_per_h, qchisq(0.95, 6) / 2 / H) && acp_decide_phantom(ke, crit)[statistic == "phantom_events"]$decision == "events_rule",
      "T10: Poisson upper bound; any event adds an events-minimum rule")
ke0 <- acp_known_empty(ev10[tag_label == "C"], idle, crit)
check(ke0[is.na(board)]$events == 0 && near(ke0[is.na(board)]$upper_per_h * H, qchisq(0.95, 2) / 2) && near(qchisq(0.95, 2) / 2 / 14.4, 0.208, tol = 1e-3) &&
        acp_decide_phantom(ke0, crit)[statistic == "phantom_events"]$decision == "real_tags", "T10: 0 events -> about 3 / board-hours (0.21 at 14.4 h)")

# T11 --------------------------------------------------------------------------------------------------------------------
cen <- acp_survey_centres(NULL, NULL, 2L)
pt <- acp_position_truth(data.table(phys_board = 2L, x = c(0, 50, 150, 50, 25, 75, 0), y = c(0, 116, 0, 58, 116, 0, 29)), cen)
check(identical(pt$n_adm, c(1L, 2L, 2L, 4L, 1L, 1L, 1L)) && identical(pt$true_pid[c(1, 5, 6, 7)], c(1L, 5L, 2L, 1L)), "T11: nearest centre; 2 admissible at midpoints, 4 at the junction")
check(identical(pt$adm_readers[c(2, 3, 4)], c("5|6", "2|3", "1|2|5|6")) && identical(pt$boundary[2:3], c("R5|R6", "R2|R3")) &&
        identical(pt$geometry[2:3], c("col1", "col1")) && identical(pt$phase_pairing[2:3], c("cross", "within")), "T11: boundary, geometry, phase pairing")
cen2 <- copy(cen)[PositionID == 5L, cx := 2][PositionID == 6L, cx := 99]
check(acp_position_truth(data.table(phys_board = 2L, x = 50, y = 116), cen2)$n_adm == 2L &&
        acp_position_truth(data.table(phys_board = 2L, x = 40, y = 116), cen2)$n_adm == 1L, "T11: measured centres; within 5 grid units of the minimum")
pp <- outer(1:8, 1:8, acp_phase_pairing)
grp <- c("B", "A", "A", "B", "A", "B", "B", "A")   # R1 ch1 B, R2 ch3 A, R3 ch5 A, R4 ch7 B, R5 ch2 A, R6 ch4 B, R7 ch6 B, R8 ch8 A
check(all(pp == ifelse(outer(grp, grp, "=="), "within", "cross")), "T11: the phase pairing table of the reader map")
check(identical(acp_phase_pairing(c(5L, 6L, 7L, 1L, 1L), c(6L, 7L, 8L, 2L, 5L)), c("cross", "within", "cross", "cross", "cross")), "T11: the boundaries of the protocol")
ann11 <- data.table(placement_id = "m", kind = "cross", t_pc = fmt(c(10, 20, 30)), from_reader = c("R5", "R6", "R7"), to_reader = c("R6", "R7", "R8"))
a11 <- acp_check_annotation(ann11)
w11 <- data.table(placement_id = "m", tag_label = "T1", ws = 5, we = 35, use = TRUE)
mt <- acp_movement_truth(a11$annotation, w11, 0.1, 0.2)
check(a11$ok && nrow(mt) == 300 && identical(mt[cell %in% c(1, 50, 51, 150, 151, 300)]$true_pid, c(5L, 5L, 6L, 6L, 7L, 8L)), "T11: movement truth from the crossings")
check(sum(!mt$eval) == 3 * 4 && !mt[cell == 50]$eval && mt[cell == 48]$eval, "T11: +-0.2 s around each crossing not evaluated")
check(!acp_check_annotation(copy(ann11)[3, from_reader := "R6"])$ok && !acp_check_annotation(copy(ann11)[3, to_reader := "R5"])$ok,
      "T11: discontinuous or non-neighbour crossings are refused")

# T12 --------------------------------------------------------------------------------------------------------------------
check(identical(attr(sched, "sha256"), ACP_SCHEDULE_SHA256) && nrow(sched) == 118 && sum(sched$required) == 89 &&
        identical(sched[unit_type == "C", .N, by = .(board, block)][order(board, block)]$N, rep(5L, 4)), "T12: the pinned schedule v2 (118 rows; two C runs per board)")
bytes <- readBin(ACP_SCHEDULE_FILE, "raw", file.size(ACP_SCHEDULE_FILE))
if (any(bytes == as.raw(13L))) check(identical(digest::digest(ACP_SCHEDULE_FILE, algo = "sha256", file = TRUE), ACP_SCHEDULE_FILE_SHA256_CRLF),
                                     "T12: a CRLF checkout has the bytes written by R on Windows (ACP_SCHEDULE_FILE_SHA256_CRLF)")
lf <- file.path(tmp, "schedule_lf.csv"); writeBin(charToRaw(paste0(paste(sub("\r$", "", readLines(ACP_SCHEDULE_FILE)), collapse = "\n"), "\n")), lf)
check(identical(acp_content_sha256(lf), ACP_SCHEDULE_SHA256), "T12: the pin ignores line endings")
bad <- file.path(tmp, "schedule_bad.csv"); x <- readLines(ACP_SCHEDULE_FILE); i13 <- grep("\"P09\"", x, fixed = TRUE)[1]; x[i13] <- sub("\"P09\"", "\"P90\"", x[i13], fixed = TRUE); writeLines(x, bad)
check(inherits(try(acp_read_schedule(bad), silent = TRUE), "try-error"), "T12: an edited schedule is refused")
mk_sheet <- function() {
  s <- sched[required == TRUE]
  t0 <- S0 + c(0, cumsum(sched$slot_s))[match(s$order, sched$order)]
  data.table(placement_id = as.character(s$order), order = as.character(s$order), board = s$board, pair_id = s$pair_id, status = "executed",
             pc_start = fmt(t0 + 1), pc_end = fmt(t0 + 1 + pmax(s$seconds, 60)), clapper_ok = "TRUE", drop_step = "", deviation = "")
}
sh12 <- mk_sheet()
k12 <- acp_check_sheet(sh12, sched)
check(k12$ok && !nrow(k12$problems) && all(k12$sheet$use), "T12: a complete sheet passes")
sw <- copy(sh12); i <- which(sw$pair_id %in% c("P09", "P06"))[1:2]
sw[i, `:=`(pc_start = rev(pc_start), pc_end = rev(pc_end))]
ks <- acp_check_sheet(sw, sched)
check(ks$ok && identical(sort(ks$problems[check == "order_deviation"]$placement_id), sort(sw$placement_id[i])), "T12: a swapped order is flagged (two rows)")
roof <- sched[pair_id %in% c("H05", "H06")]$order
d1 <- copy(sh12)[order %in% roof, `:=`(status = "dropped", drop_step = "1")]
check(acp_check_sheet(d1, sched)$ok, "T12: drop step 1 (H05/H06 on both boards) is honoured")
d1b <- copy(sh12)[order %in% roof[sched[order %in% roof]$board == "2"], `:=`(status = "dropped", drop_step = "1")]
check(!acp_check_sheet(d1b, sched)$ok && "drop_one_board" %in% acp_check_sheet(d1b, sched)$problems$check, "T12: a drop on one board only is refused")
d2 <- copy(sh12)[pair_id == "CS", drop_step := "2"]
check("drop_order" %in% acp_check_sheet(d2, sched)$problems$check, "T12: drop step 2 before step 1 is refused")
d12 <- copy(sh12)[order %in% roof, `:=`(status = "dropped", drop_step = "1")][pair_id == "CS", drop_step := "2"]
check(acp_check_sheet(d12, sched)$ok, "T12: drop steps 1 and 2 in order pass")
dp <- copy(sh12)[pair_id == "P06", `:=`(status = "dropped", drop_step = "1")]
check("drop_outside_order" %in% acp_check_sheet(dp, sched)$problems$check, "T12: dropping a placement outside the drop order is refused")
check("missing" %in% acp_check_sheet(sh12[-5], sched)$problems$check, "T12: a placement neither executed nor dropped is an error")
ov <- copy(sh12); j <- which(ov$pair_id == "P01")[1]; ov[j + 1L, pc_start := ov$pc_start[j]]
check("overlap" %in% acp_check_sheet(ov, sched)$problems$check, "T12: overlapping placements on one board are an error")
rp <- rbind(copy(sh12), copy(sh12)[pair_id == "P01"][1][, `:=`(placement_id = paste0(placement_id, "r"), pc_start = fmt(S0 + 20000), pc_end = fmt(S0 + 20070))])
kr <- acp_check_sheet(rp, sched)
check(kr$sheet[placement_id == sh12[pair_id == "P01"]$placement_id[1]]$superseded && kr$sheet[grepl("r$", placement_id)]$use, "T12: a repeat supersedes its original")
check("board_mismatch" %in% acp_check_sheet(copy(sh12)[which(pair_id == "REFz")[1], board := ifelse(board == "4", "2", "4")], sched)$problems$check, "T12: a placement on the wrong board is an error")

# T13 --------------------------------------------------------------------------------------------------------------------
# D_col 0.03 [-0.01, 0.07]: h = 0.04, h / sqrt(2) = 0.0283; thresholds inside: 0 and 0.05 (-0.05 outside); |0.03 - 0| = 0.03
# resolvable, |0.03 - 0.05| = 0.02 not. D_col 0.025 [-0.015, 0.065]: 0.025 and 0.025 < 0.0283 -> P fails.
fac <- acp_th(crit, "P1")
pj <- acp_project(0.03, -0.01, 0.07, c(-0.05, 0, 0.05), fac)
check(identical(pj$inside, c(FALSE, TRUE, TRUE)) && identical(pj$resolvable, c(FALSE, TRUE, FALSE)) && near(fac, 1 / sqrt(2), 1e-12), "T13: projection test by hand")
row13 <- function(...) acp_row(crit, "A42", ...)
base13 <- function(est, lo, hi, dec = "TIE_inconclusive") rbind(row13("D_col", dec, est, lo, hi),
  acp_row(crit, "B10l", "board_call", "no_difference"), acp_row(crit, "B10f", "board_call_drift_adjusted", "no_difference"),
  acp_row(crit, "B10e", "REF_excess_sd", NA, 0.05, board = 2), acp_row(crit, "B10e", "REF_excess_sd", NA, 0.05, board = 4),
  acp_row(crit, "B10e", "REF_late_early", NA, 1.05, board = 2), acp_row(crit, "B10e", "REF_late_early", NA, 0.9, board = 4),
  acp_row(crit, "B10e", "REFx_informative", NA, 1), acp_row(crit, "B11b", "tau_star_read", "tau_star", 1),
  acp_row(crit, "B4a", "competition_call", "time_sharing", board = 2), acp_row(crit, "B4a", "competition_call", "time_sharing", board = 4),
  acp_row(crit, "B16a", "transfer", "caveat"))
tg <- function(d, m = character()) { x <- acp_decide_day2(d, crit, m); stats::setNames(x$triggered, x$trigger) }
z <- tg(base13(0.03, -0.01, 0.07))
check(z[["D1"]] && !any(z[c("D2", "D3", "D4", "D5", "D6", "D7")]) && z[["recommendation"]], "T13: D1 when P holds for an inconclusive basis")
check(!tg(base13(0.025, -0.015, 0.065))[["D1"]] && !tg(base13(0.025, -0.015, 0.065))[["recommendation"]], "T13: no day 2 when P fails (a narrow miss)")
check(!tg(base13(0.03, -0.01, 0.07, "TIE_equivalent"))[["D1"]], "T13: a decided basis is never a reason")
check(tg(base13(0.025, -0.015, 0.065), "a board failed the channel check")[["D2"]], "T13: D2 for a missing endpoint")
d3 <- base13(0.2, 0.1, 0.3, "READ"); d3[statistic == "REF_excess_sd" & board == "4", estimate := 0.12]
check(tg(d3)[["D3"]], "T13: D3 when the REF excess SD exceeds 0.10")
d3b <- base13(0.2, 0.1, 0.3, "READ"); d3b[statistic == "REF_late_early" & board == "2", estimate := 1.2]
check(tg(d3b)[["D3"]] && !tg(base13(0.2, 0.1, 0.3, "READ"))[["D3"]], "T13: D3 when late/early differs by more than 15%")
d4 <- rbind(base13(0.2, 0.1, 0.3, "READ")[statistic != "board_call"], acp_row(crit, "B10l", "board_call", "undecided"),
            acp_row(crit, "B10a", "R_board", NA, 0.93, 0.85, 1.01), acp_row(crit, "B10c", "coread_ratio", NA, 0.9, 0.8, 1.0))
d4[statistic == "board_call_drift_adjusted", decision := "undecided"]
check(tg(d4)[["D4"]], "T13: D4 (R_board 0.93 [0.85, 1.01]: |0.93 - 1| = 0.07 > 0.057)")
d4[statistic == "R_board", `:=`(estimate = 0.95, lo = 0.87, hi = 1.03)]
check(!tg(d4)[["D4"]], "T13: no D4 when P fails at 0.9 and 1.0 (0.05 < 0.057)")
d5 <- base13(0.2, 0.1, 0.3, "READ"); d5[statistic == "tau_star_read", decision := "not_decidable"]
check(tg(d5)[["D5"]], "T13: D5 when tau is not decidable for lack of data")
d6 <- base13(0.2, 0.1, 0.3, "READ"); d6[statistic == "competition_call" & board == "4", decision := "capture_or_starvation"]
check(tg(d6)[["D6"]], "T13: D6 for capture/starvation on one board only")
d6b <- rbind(base13(0.2, 0.1, 0.3, "READ")[!(statistic == "competition_call" & board == "2")], acp_row(crit, "B4a", "competition_call", "intermediate", board = 2),
             acp_row(crit, "B4a", "TR", NA, 0.8, 0.74, 0.86, board = 2), acp_row(crit, "B4b", "MS", NA, 0.3, 0.28, 0.32, board = 2))
check(tg(d6b)[["D6"]], "T13: D6 for an intermediate competition call when P holds (TR 0.8 [0.74, 0.86]: |0.8 - 0.85| = 0.05 > 0.042)")
d6b[statistic == "TR", `:=`(lo = 0.72, hi = 0.88)]
check(!tg(d6b)[["D6"]], "T13: no D6 when P fails (TR 0.8 [0.72, 0.88]: 0.05 < 0.057)")
check(!tg(base13(0.2, 0.1, 0.3, "READ"))[["D7"]], "T13: a transfer caveat is never a reason for day 2")

# T14 --------------------------------------------------------------------------------------------------------------------
check(identical(attr(crit, "sha256"), ACP_CRITERIA_SHA256) && identical(acp_content_sha256(ACP_CRITERIA_FILE), ACP_CRITERIA_SHA256),
      "T14: the criteria file has the pinned content hash")
cx <- readLines(ACP_CRITERIA_FILE); i14 <- grep("^B1a,", cx); cx[i14] <- sub(">=,0.95,", ">=,0.94,", cx[i14], fixed = TRUE)
bad_c <- file.path(tmp, "criteria_bad.csv"); writeLines(cx, bad_c)
check(inherits(try(acp_criteria(bad_c), silent = TRUE), "try-error"), "T14: a criteria file with a changed threshold is refused")
crlf_c <- file.path(tmp, "criteria_crlf.csv"); writeBin(charToRaw(paste0(paste(readLines(ACP_CRITERIA_FILE), collapse = "\r\n"), "\r\n")), crlf_c)
check(identical(attr(acp_criteria(crlf_c), "sha256"), ACP_CRITERIA_SHA256), "T14: the pin ignores line endings")
check(near(acp_th(crit, "A20"), 0.05) && near(acp_th(crit, "B10h"), 1.11) && acp_th(crit, "G02") == 10000 && acp_th(crit, "G03") == 20261014 &&
        identical(acp_set(crit, "B7a"), c(10, 30, 60, 300)) && identical(acp_set(crit, "A09"), c("P09", "P10", "P11", "REFx")), "T14: thresholds as written")
check(identical(acp_test(crit, "B1a", c(57 / 60, 0.94, NA)), c(TRUE, FALSE, FALSE)) && identical(acp_test(crit, "B4a", c(0.85, 1.15, 0.84, 1.16)), c(TRUE, TRUE, FALSE, FALSE)) &&
        identical(acp_test(crit, "A21", c(0, 1e-6)), c(FALSE, TRUE)) && identical(acp_test(crit, "B15a", c(0, 1)), c(TRUE, FALSE)), "T14: comparators applied as written")
check(all(crit$comparator %in% ACP_COMPARATORS) && all(paste0("B", 1:16) %in% crit$criterion) && all(paste0("D", 1:7) %in% crit$outcome),
      "T14: every criterion B1-B16 and every day-2 trigger D1-D7 has a row")

# T15 --------------------------------------------------------------------------------------------------------------------
for (p in c("Functions/antenna_calibration_pilot.R", "Testing/audits/analyse_rfid_calibration_pilot.R")) {
  pd <- utils::getParseData(parse(p, keep.source = TRUE))
  sym <- pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB")]
  str <- pd$text[pd$token == "STR_CONST"]
  check(!any(sym %in% c("Group", "outcome_group", "GroupSex", "CombZ", "Outcome", "sus_animals", "con_animals")) &&
          !any(grepl("sus_animals|con_animals|later_outcome_combz|outcome_group|CombZ_|GroupSex", str)), paste("T15: group-blind", p))
}

# T16 --------------------------------------------------------------------------------------------------------------------
pr <- getOption("mmm.project_root")
beh <- dirname(pr); expdir <- dirname(dirname(beh))
refuse <- function(path) inherits(try(acp_run_dirs(path, "acp_test"), silent = TRUE), "try-error")
check(refuse(file.path(expdir, "Raw Data", "Behavior", "RFID", "calibration")) && refuse(file.path(expdir, "Raw Data", "Behavior", "RFID", "MMMdata", "x")) &&
        refuse(file.path(beh, "RANM", "measurement_validation")) && refuse(file.path(pr, "analysis_ready")) && refuse("relative/RANM/calibration"),
      "T16: Raw Data, MMMdata, measurement validation, the live root and relative paths are refused")
ok_base <- file.path(beh, "RANM", "calibration")
rd16 <- acp_run_dirs(ok_base, "acp_test")
check(identical(rd16$out, file.path(normalizePath(ok_base, winslash = "/", mustWork = FALSE), "acp_test")) && grepl("/[.]tmp_acp_test$", rd16$tmp), "T16: the isolated root is accepted")
dir.create(rd16$out, recursive = TRUE)
check(refuse(ok_base) && inherits(try(acp_run_dirs(ok_base, "acp_test"), silent = TRUE), "try-error"), "T16: an existing run folder is refused")
check(inherits(try(acp_run_dirs(ok_base, "bad id"), silent = TRUE), "try-error"), "T16: a bad run id is refused")
unlink(rd16$out, recursive = TRUE)

# E2E --------------------------------------------------------------------------------------------------------------------
# A synthetic pilot day on the pinned schedule. Every row starts 3 s into its slot and lasts its recorded seconds (CHECK
# 100 s; BREAK its slot less 6 s). Read model: reads every 414.5 ms at the true reader (a centre is saturated on both
# boards); quarter points: the near reader with a 1.05-s pause every 20 s (an event split) and one crosstalk read at the
# far reader every 8 s; midpoints: the lower reader with a 2-read run at the other reader every 5 s (the lower reader's
# reads in the first 0.45 s are left out, so runs last 0.45-0.87 s on board 2 and 0.45-0.97 s on board 4, all below 1 s);
# off the centres board 4 reads the near/lower reader every 518 ms (0.8 x board 2); house on: 19 of 20 reads; roof: one
# read every 3 s; movement: reads at the true reader from 0.3 s after a crossing to 0.2 s after the next; C: sentinels
# continuous, load tags time-sharing. Survey sweeps at 10 grid units/s with read zones of +-30 units (board 4 R6: 6 units
# to the right). Clapper touches at every start and end, sheet times 2.3 s early, one spurious touch in the break.
sim_day <- function(sched, S0) {
  s <- copy(sched)
  s[, slot0 := S0 + c(0, cumsum(slot_s))[seq_len(.N)]]
  s[, `:=`(a = slot0 + 3, z = slot0 + 3 + ifelse(block == "CHECK", 100, ifelse(seconds == 0, slot_s - 6, pmin(seconds, slot_s - 6))))]
  R <- list(); ann <- list(); sv <- list()
  add <- function(label, board, pid, t_ms) if (length(t_ms)) R[[length(R) + 1L]] <<- rd_of(label, board, pid, t_ms)
  touch <- function(a) add("C", 5, 4, round((a + (0:4) * 0.4145) * 1000))
  for (i in seq_len(nrow(s))) {
    r <- s[i]; a <- r$a; z <- r$z; b <- suppressWarnings(as.integer(r$board))
    if (r$n_tags > 0 || r$block %in% c("BASELINE", "BREAK")) { touch(a); touch(z) }
    if (r$block == "CHECK") for (k in 0:15) add("T1", c(2, 4)[k %/% 8 + 1], k %% 8 + 1, cadence(a + 2 + 5 * k + 0.5, a + 2 + 5 * k + 4.5))
    if (r$block == "SURVEY") for (bb in c(2L, 4L)) {
      t0 <- a + 5 + (bb == 4L) * 220
      sweeps <- list(list(ax = "x", fix = 0), list(ax = "x", fix = 116), list(ax = "y", fix = 0), list(ax = "y", fix = 100), list(ax = "y", fix = 200), list(ax = "y", fix = 300))
      for (k in seq_along(sweeps)) {
        sw <- sweeps[[k]]; len <- if (sw$ax == "x") 300 else 116
        for (pid in 1:8) {
          if (sw$ax == "x" && RFID_GRID_Y[pid] != sw$fix) next
          if (sw$ax == "y" && RFID_GRID_X[pid] != sw$fix) next
          c0 <- if (sw$ax == "x") RFID_GRID_X[pid] else RFID_GRID_Y[pid]
          sh <- if (bb == 4L && pid == 6L && sw$ax == "x") 6 else 0     # board 4 R6 read zone centred 6 units to the right
          add("T1", bb, pid, cadence(t0 + max(0, c0 + sh - 30) / 10, t0 + min(len, c0 + sh + 30) / 10))
        }
        sv[[length(sv) + 1L]] <- data.table(board = bb, sweep = paste0("s", k), t_pc = fmt(c(t0, t0 + len / 10)),
                                            x = if (sw$ax == "x") c(0, 300) else sw$fix, y = if (sw$ax == "x") sw$fix else c(0, 116))
        t0 <- t0 + len / 10 + 5
      }
    }
    if (r$analysed && r$unit_type %in% ACP_SINGLE_TYPES) {
      tg <- if (r$unit_type == "T0") r$tags else "T1"
      x <- r$x; y <- r$y
      d <- sqrt((RFID_GRID_X - x)^2 + (RFID_GRID_Y - y)^2); o <- order(d); p1 <- o[1]; p2 <- o[2]
      centre <- d[p1] == 0; mid <- abs(d[p1] - d[p2]) < 1e-9
      ev4 <- if (b == 4L) 1.25 else 1
      if (r$pair_id %in% c("H05", "H06")) add(tg, b, 5, round((a + seq(0.5, z - a, by = 3)) * 1000))
      else if (centre) add(tg, b, p1, if (r$pair_id %in% c("H01", "H03")) { v <- cadence(a, z); v[seq_along(v) %% 20 != 0] } else cadence(a, z))
      else if (mid) {
        A <- min(p1, p2); Bp <- max(p1, p2)
        v <- cadence(a, z, ev4); fl_t <- seq(a + 2, z - 1, by = 5)
        inrun <- Reduce(`|`, lapply(fl_t, function(f) v >= f * 1000 & v < (f + 0.45) * 1000), FALSE)
        add(tg, b, A, v[!inrun]); add(tg, b, Bp, round(c(fl_t, fl_t + 0.4145) * 1000))
      } else {
        v <- cadence(a, z, ev4); pause <- Reduce(`|`, lapply(seq(a + 10, z, by = 20), function(f) v >= f * 1000 & v < (f + 1.05) * 1000), FALSE)
        add(tg, b, p1, v[!pause]); add(tg, b, p2, round((seq(a + 3.3, z - 1, by = 8)) * 1000))
      }
    }
    if (r$unit_type %in% c("M_pass", "M_visits")) {
      if (r$unit_type == "M_pass") {
        trav <- if (grepl("slow", r$pair_id)) 2 else 0.5
        seq_r <- c(5, 6, 7, 8, 7, 6, 5, 6, 7, 8, 7, 6, 5)
        tc <- a + 8 + (seq_len(12) - 1) * (trav + 5) + trav / 2
        fr <- seq_r[1:12]; to <- seq_r[2:13]
      } else {
        dw <- acp_visit_dwells(r$position); tc <- numeric(); fr <- integer(); to <- integer(); t <- a + 8
        for (dd in dw) { tc <- c(tc, t + 0.5, t + 1.5 + dd); fr <- c(fr, 5L, 6L); to <- c(to, 6L, 5L); t <- t + 2 + dd + 10 }
      }
      bounds <- c(a, tc, z); rr <- c(fr[1], to)
      for (k in seq_along(rr)) add("T1", b, rr[k], cadence(if (k == 1) a else bounds[k] + 0.3, if (k == length(rr)) z else bounds[k + 1] + 0.2))
      ann[[length(ann) + 1L]] <- data.table(placement_id = as.character(r$order), kind = "cross", t_pc = fmt(tc), from_reader = paste0("R", fr), to_reader = paste0("R", to))
    }
    if (r$unit_type == "C") {
      add("T5", b, 8, cadence(a, z)); add("T6", b, 4, cadence(a, z))
      if (r$pair_id == "C1") add("T1", b, 5, cadence(a, z))
      if (r$pair_id == "C4") for (k in 1:4) add(paste0("T", k), b, 5, cadence(a + (k - 1) * 0.4145, z, every = 4))
      if (r$pair_id == "CS") { add("T1", b, 5, cadence(a, z, 2)); add("T2", b, 5, cadence(a + 0.4145, z, 2)); add("T3", b, 6, cadence(a, z, 2)); add("T4", b, 6, cadence(a + 0.4145, z, 2)) }
    }
  }
  touch(s[block == "BREAK"]$a + 400)                                                    # a spurious touch in the break
  rows <- s[n_tags > 0 | block %in% c("BASELINE", "BREAK")]
  sheet <- rows[, .(placement_id = as.character(order), order = as.character(order), board = ifelse(is.na(board), "", board), pair_id, status = "executed",
                    pc_start = fmt(a - 2.3), pc_end = fmt(z - 2.3), clapper_ok = "TRUE", drop_step = "", deviation = "")]
  list(reads = rbindlist(R), sheet = sheet, annotation = rbindlist(ann), survey = rbindlist(sv), span = c(S0 + 1200, max(s$z) + 60))
}
day <- sim_day(sched, S0)
day_dir <- file.path(tmp, "day"); dir.create(day_dir)
write_log(file.path(day_dir, "MultiMouseMonitor-26.11.03-calibration.csv"), day$reads, day$span[1], day$span[2])
fwrite(day$sheet, file.path(day_dir, "placement_sheet.csv")); fwrite(day$annotation, file.path(day_dir, "annotation_movement.csv"))
fwrite(day$survey, file.path(day_dir, "annotation_survey.csv")); fwrite(TAGS, file.path(day_dir, "tags.csv"))
ses <- acp_read_session(file.path(day_dir, "MultiMouseMonitor-26.11.03-calibration.csv"), acp_read_tags(file.path(day_dir, "tags.csv")))
shk <- acp_read_sheet(file.path(day_dir, "placement_sheet.csv"), sched)
ank <- acp_read_annotation(file.path(day_dir, "annotation_movement.csv"), shk$sheet)
svy <- acp_read_survey(file.path(day_dir, "annotation_survey.csv"))
check(shk$ok && ank$ok && nrow(ses$reads) == nrow(day$reads), "E2E: the synthetic day's inputs pass their checks")
bh <- data.table(board = rep(1:5, each = 24), mean_gap_ms = 414.5 + rep(c(-2, 2), 60), lattice_k1 = rep(c(0, 0.01), 60))
res <- acp_analyse(sched, shk, ses, ank, svy, bh, crit, B = 200L)
dc <- res$decisions
dv <- function(stat, b = "pooled") dc[statistic == stat & board == b]$decision
check(near(res$alignment$offset_s, 2.3, 1e-3) && res$alignment$fallback_times == 0L && all(res$windows$start_source == "clapper"), "E2E: clapper alignment of every sheet time")
# analysed windows are the recorded seconds less 10 s: 60 s, and 300 s for the static-flicker placement P14
check(near(max(abs(res$windows[block == "P1"][, T - (seconds - 10)])), 0, 2e-3) && near(res$windows[block == "P1" & pair_id == "P14"]$T, c(300, 300), 2e-3) &&
        all(res$channel_check$passed), "E2E: analysed windows (60 s; P14 300 s); channel check passed")
cx6 <- res$centres[PositionID == 6L][order(board)]$cx
check(near(cx6[1], 100, 3) && near(cx6[2] - cx6[1], 6, 0.5) && all(res$centres$source_x == "survey") && all(res$centres$source_y == "survey"),
      "E2E: survey centres (read-zone midpoints; board 4 R6 measured 6 units off the grid)")
check(dv("D_col") == "READ" && dc[statistic == "D_col"]$estimate > 0.1 && dv("resolution") == "reader", "E2E: READ favoured; reader-level resolution")
check(dv("fresh_age_s") == "fresh_le_Gmax_bound" && dv("tau_star_read") == "tau_star" && near(dc[statistic == "tau_star_read"]$estimate, 1), "E2E: fresh age 2 s; tau* = 1 s (read basis)")
check(all(dc[statistic == "competition_call"]$decision == "time_sharing") && all(dc[statistic == "CS_C4"]$decision == "per_antenna") &&
        all(dc[statistic == "SR"]$decision == "none") && near(dc[statistic == "delta_s" & board == "4"]$estimate, 10), "E2E: competition, split, load, delta")
# informative pairs: P09-P14 and REF-x (off the centres: board 4 / board 2 about 0.8) and the roof pairs H05, H06 (ratio 1);
# the centres (P01-P08, H01-H04, REF-z) are saturated on both boards. P15 (R2|R3 midpoint) counts both admissible readers,
# including flicker-run reads the simulation does not scale by board, so its board-4 efficiency sits at the saturation
# bound (e about 0.9): its flag is left to the rule (B10k, B1b), which every pair must follow.
bp <- res$board_ratio$pairs
inf <- bp[informative == TRUE]
rule <- !(acp_test(crit, "B10k", bp$e_2) & acp_test(crit, "B10k", bp$e_4)) & !(acp_test(crit, "B1b", bp$P1s_2) & acp_test(crit, "B1b", bp$P1s_4)) & is.finite(bp$log_ratio)
check(identical(bp$informative, rule) && all(c(sprintf("P%02d", 9:14), "H05", "H06", "REFx") %in% inf$pair_id) &&
        !any(c(sprintf("P%02d", 1:8), sprintf("H%02d", 1:4), "REFz") %in% inf$pair_id) &&
        all(abs(exp(inf[!pair_id %in% c("H05", "H06", "P15")]$log_ratio) - 0.8) < 0.03) && all(inf[pair_id %in% c("H05", "H06")]$log_ratio == 0),
      "E2E: informative pairs of the board ratio")
check(dv("house_ratio") == "no_shielding" && dv("board_call") == "difference" && near(dc[statistic == "R_board"]$estimate, exp(mean(inf$log_ratio))),
      "E2E: no shielding; board difference")
z16 <- res$cadence[board %in% c(2L, 4L)]
check(dv("phantom_events") == "real_tags" && dv("transfer") == (if (any(abs(c(z16$z_gap, z16$z_k1)) > 3)) "caveat" else "transfer") &&
        all(grepl("^(eligible|T1_within)$", dc[criterion == "B14"]$decision)), "E2E: phantom, transfer, tags")
check(!any(dc$decision %in% "not_obtained") && !nrow(res$missing) && !res$day2[trigger == "recommendation"]$triggered, "E2E: every endpoint obtained; no day 2")
check(nrow(res$movement_units[unit_type == "crossing"]) == 48 && nrow(res$movement_units[unit_type == "visit"]) == 24 &&
        data.table::uniqueN(res$movement_units[unit_type == "crossing"]$cluster) == 16, "E2E: 24 crossings in 8 passes and 12 visits per board")
check(all(res$known_empty$events == 0) && near(sum(res$known_empty[!is.na(board)]$board_hours), res$known_empty[is.na(board)]$board_hours), "E2E: known-empty time")

# V2 ---------------------------------------------------------------------------------------------------------------------
# board order 4-2-2-4 by halves for every programme, and two identical C runs per board
first <- function(prog) sched[grepl(prog, block) & board %in% c("2", "4"), .(t = min(order)), by = board][order(t)]$board
check(identical(first("^P1$"), c("4", "2")) && identical(first("^P2$"), c("2", "4")) && identical(first("^M1$"), c("4", "2")) &&
        identical(first("^M2$"), c("2", "4")) && identical(first("^H1-"), c("4", "2")) && identical(first("^H2-"), c("2", "4")) &&
        identical(first("^Ca$"), c("4", "2")) && identical(first("^Cb$"), c("2", "4")), "V2: every programme half and C run is ordered 4-2-2-4")
cr <- sched[unit_type == "C"]
check(all(cr[, .(k = data.table::uniqueN(paste(pair_id, position, x, y, orientation, height, n_tags, tags, seconds))), by = pair_id]$k == 1L),
      "V2: the C runs are identical in slots, orientations, tags and durations on both boards")
# the two runs of a board are pooled: blocks unique per placement, per-tag rows per run
cp <- res$competition
check(all(cp$per_tag[period == "C4" & tag_label == "T1", .N, by = board]$N == 2L) &&
        all(cp$blocks[, data.table::uniqueN(sub("_[0-9]+$", "", block)), by = .(board, period)]$V1 == 2L) &&
        !anyDuplicated(cp$blocks[, .(board, period, tag_label, block)]), "V2: two C runs per board, their 10-s blocks kept apart and pooled")
k0 <- acp_th(crit, "G06")
tr_hand <- cp$per_tag[board == 4, .(r = sum(reads), d = sum(T)), by = .(period, tag_label)]
tr4 <- ((tr_hand[period == "C4" & tag_label %in% c("T1", "T2", "T3", "T4"), sum(r)] + k0) / tr_hand[period == "C4" & tag_label == "T1"]$d) /
  ((tr_hand[period == "C1" & tag_label == "T1"]$r + k0) / tr_hand[period == "C1" & tag_label == "T1"]$d)
check(near(cp$stats[board == 4 & statistic == "TR"]$estimate, tr4, 1e-9), "V2: TR on board 4 pools the reads and durations of both runs")
# criteria v1.1 against v1.0: only G07, G08 and the day-2 handling differ; every threshold of v1.0 is kept
c10 <- data.table::fread(file.path(ACP_DIR, "acp_criteria_v1.0.csv"), colClasses = "character", na.strings = NULL)
c11 <- data.table::fread(ACP_CRITERIA_FILE, colClasses = "character", na.strings = NULL)
same <- merge(c10, c11, by = "rule_id", suffixes = c("_10", "_11"))
diff_ids <- same[Reduce(`|`, lapply(setdiff(names(c10), "rule_id"), function(k) same[[paste0(k, "_10")]] != same[[paste0(k, "_11")]]))]$rule_id
check(identical(sort(diff_ids), c("G07", "G08")) && identical(sort(setdiff(c10$rule_id, c11$rule_id)), c("DB1", "DB2")) &&
        identical(sort(setdiff(c11$rule_id, c10$rule_id)), c("DC1", "DC2", "DC3")) && identical(ACP_VERSION, "acp_criteria_v1.1") &&
        grepl("never pooled", c11[rule_id == "DC1"]$statistic) &&
        (grepl("separate confirmation experiment", res$day2[trigger == "recommendation"]$reason) || !res$day2[trigger == "recommendation"]$triggered) &&
        grepl("separate confirmation experiment", paste(deparse(acp_decide_day2), collapse = " "), fixed = TRUE), "V2: criteria v1.1 = v1.0 except G07, G08 and DB1-DB2 -> DC1-DC3 (day 2 never pooled)")
# board substitution: only an explicit, allowed authorization
sub_f <- function(rows, nm) { f <- file.path(tmp, nm); data.table::fwrite(rows, f); f }
ok_sub <- data.table(role = "4", physical_board = "3", reason = "board 4 channel check failed twice", authorized_by = "Tobias Pohl", authorized_on = "2026-11-03")
b3 <- acp_read_substitution(sub_f(ok_sub, "sub_ok.csv"))
check(identical(unname(b3[c("2", "4")]), c(2L, 3L)) && nrow(attr(b3, "substitution")) == 1L, "V2: an authorized substitute (board 3 for role 4) maps the role")
check(inherits(try(acp_read_substitution(sub_f(copy(ok_sub)[, physical_board := "1"], "sub_b1.csv")), silent = TRUE), "try-error") &&
        inherits(try(acp_read_substitution(sub_f(copy(ok_sub)[, reason := ""], "sub_nr.csv")), silent = TRUE), "try-error") &&
        inherits(try(acp_read_substitution(sub_f(copy(ok_sub)[, authorized_on := "03.11.2026"], "sub_nd.csv")), silent = TRUE), "try-error") &&
        inherits(try(acp_read_substitution(sub_f(copy(ok_sub)[, role := "3"], "sub_r3.csv")), silent = TRUE), "try-error") &&
        inherits(try(acp_read_substitution(sub_f(copy(ok_sub)[, extra := "x"], "sub_col.csv")), silent = TRUE), "try-error"),
      "V2: a substitute other than the allowed one, without a reason or date, for an unknown role, or with other columns is refused")
on3 <- copy(sh12)[board == "4", board := "3"]
check("board_mismatch" %in% acp_check_sheet(on3, sched)$problems$check && !"board_mismatch" %in% acp_check_sheet(on3, sched, b3)$problems$check,
      "V2: board 3 in the sheet is a mismatch unless the substitution is authorized")
rmain <- paste(deparse(Filter(function(e) is.call(e) && identical(e[[1]], as.name("if")),
                              as.list(parse("Testing/audits/analyse_rfid_calibration_pilot.R", keep.source = FALSE)))[[1]]), collapse = "\n")
check(grepl("board_mismatch", rmain, fixed = TRUE) && grepl("never substituted silently", rmain, fixed = TRUE) &&
        grepl("acp_read_substitution(subst_path)", rmain, fixed = TRUE) && grepl("boards = boards", rmain, fixed = TRUE) &&
        regexpr("board_mismatch", rmain, fixed = TRUE) < regexpr("acp_analyse(", rmain, fixed = TRUE),
      "V2: the runner aborts on an unauthorized board before the analysis and passes an authorized mapping on")

unlink(tmp, recursive = TRUE)
cat("test_antenna_calibration_pilot: all checks passed\n")
