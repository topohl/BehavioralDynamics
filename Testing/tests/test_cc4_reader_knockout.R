# ===========================================================================
# CC4 reader knockout (Functions/cc4_reader_knockout.R, Testing/audits/audit_cc4_reader_knockout.R). Portable: synthetic
# tables and a synthetic UTF-16LE raw log with its vendor export, all under tempdir(); nothing on the lab drive; the
# audit's main block never runs. Expected values are worked out by hand.
#
#  1. crossings per phase: a previous record before the phase counts (boundary-straddling), a record at a block start
#     belongs to the new block, the first record never counts; equal to Stage 31's own functions on the same rows
#  2. the knockout deletes only reader k's reads on the animal's own board
#  3. family pooling (A2 | A3-A5, I2 | I3-I5), the log rate ratio with the 0.5 continuity, NA for an incomplete family
#  4. scale (residual SD of lm(y ~ Batch)) and materiality (median 0.2 SD, 90th percentile 0.5 SD) on toy tables; the
#     exact materiality columns
#  5. parity gate thresholds (95% exact, 1% pooled) and the frozen-run comparison
#  6. healthy units: the two declared episodes equal manifest v1.1's CC4 exclude_spatial records and are the only ones
#     left out
#  7. end to end: a synthetic B2 CC4 recording (raw log + vendor export written by hand) through the audit's per-recording
#     function: parity with Stage 31, crossings and knockout shifts by hand, healthy flags, materiality, a wrong board
#     label refused
#  8. group-blind and pure: no group, CombZ or outcome file in the code; the helper does no I/O and does not read the
#     spatial manifest; the design block is in the audit's header; the arguments
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble); library(tidyr) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "cc4_reader_knockout.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1, check.attributes = FALSE))
fails <- function(expr, pattern) { e <- tryCatch({ force(expr); NULL }, error = identity); inherits(e, "error") && grepl(pattern, conditionMessage(e)) }
source("Analysis/_pipeline_setup.R")
for (h in c("animalpos_preprocessing_helpers.R", "antenna_log_helpers.R", "animalpos_reconstruction.R", "cc4_grid_exposure_helpers.R",
            "cc4_whole_inactive_helpers.R", "antenna_spatial_reconstruction.R", "spatial_exclusion_manifest.R", "cc4_reader_knockout.R")) source_mmm_helper(h)
source("Testing/audits/audit_cc4_reader_knockout.R")   # definitions only (main-guarded)
tt <- function(x) as.POSIXct(x, tz = "UTC")
# Stage-31-shaped vendor rows (as preprocess_animalpos_file(phase_policy = "full_recording") returns them, corrected)
stage31_rows <- function(x, batch) {
  x <- x[order(x$DateTime), ]
  cn <- animalpos_phase_counters(x$DateTime)
  tibble::as_tibble(data.frame(DateTime = x$DateTime, AnimalID = x$AnimalID, System = x$System, PositionID = as.integer(x$PositionID),
                               CageChange = "CC4", Batch = batch, Phase = cn$Phase, ConsecActive = as.numeric(cn$ConsecActive),
                               ConsecInactive = as.numeric(cn$ConsecInactive), System_raw = x$System, AnimalNum = x$AnimalID,
                               Sex = if (batch %in% c("B1", "B2", "B5")) "Male" else "Female", stringsAsFactors = FALSE))
}

# ---- 1. crossings per phase ---------------------------------------------------------------------------------------------
# A: I1 13:00 R1, I1 18:29 R2 (crossing in I1), A1 18:30:10 R3 (crossing in A1; its previous record lies in I1),
#    18:30:20 R3 (none), I6 R3 (none). B: R4 in I1 and A1, R8 at exactly 06:30:00 (= the start of I2: a crossing in I2).
mk <- function(id, sys, times, pos) data.frame(DateTime = tt(times), AnimalID = id, System = sys, PositionID = pos, stringsAsFactors = FALSE)
d1 <- stage31_rows(rbind(
  mk("A", "sys.1", c("2022-11-09 13:00:00", "2022-11-09 18:29:00", "2022-11-09 18:30:10", "2022-11-09 18:30:20", "2022-11-14 12:00:00"), c(1, 2, 3, 3, 3)),
  mk("B", "sys.2", c("2022-11-09 13:00:00", "2022-11-10 06:00:00", "2022-11-10 06:30:00", "2022-11-14 12:00:00"), c(4, 4, 8, 8))), "B1")
ref1 <- ckr_stage31_phases(d1)
check(setequal(ref1$blocks$PhaseLabel, CKR_PHASES) && nrow(ref1$phases) == 18L && all(ref1$phases$eligible), "1: nine whole phases, all complete")
exp1 <- data.table(AnimalNum = rep(c("A", "B"), each = 9), PhaseLabel = rep(CKR_PHASES, 2),
                   crossings = c(1L, rep(0L, 8), 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L))
m1 <- merge(exp1, ref1$phases, by = c("AnimalNum", "PhaseLabel"))
check(nrow(m1) == 18L && identical(m1$crossings, m1$stage31_crossings), "1: Stage 31's own counts equal the hand count")
for (a in c("A", "B")) {
  x <- d1[d1$AnimalNum == a, ]
  pc <- merge(ckr_phase_counts(as.numeric(x$DateTime), x$PositionID, ref1$blocks), ref1$phases[AnimalNum == a], by = "PhaseLabel")
  check(identical(pc$crossings, pc$stage31_crossings) && identical(pc$records, pc$stage31_records), paste("1: ckr_phase_counts equals Stage 31 for animal", a))
}
a1 <- as.numeric(ref1$blocks[PhaseLabel == "A1"]$BlockStart)
pc <- ckr_phase_counts(a1 + c(5, 10), c(7L, 2L), ref1$blocks)
check(pc[PhaseLabel == "A1"]$crossings == 1L && pc[PhaseLabel == "A1"]$records == 2L && sum(pc$crossings) == 1L, "1: the first record never counts")
check(fails(ckr_phase_counts(c(a1 + 10, a1 + 5), c(1L, 2L), ref1$blocks), "sorted"), "1: unsorted records are refused")

# ---- 2. knockout of one reader on the own board ------------------------------------------------------------------------
r <- data.table(tag = "T", t_ms = 1:8 * 1000, gx = 0, gy = 0, board = c(1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L), PositionID = c(3L, 5L, 1L, 3L, 5L, 3L, 8L, 2L))
check(identical(ckr_knockout_reads(r, 1L, 3L)$t_ms, r$t_ms[-c(1, 6)]), "2: ko R3 deletes the own board's R3 reads; board 2's R3 read stays")
check(identical(ckr_knockout_reads(r, 1L, 5L)$t_ms, r$t_ms[-2]), "2: ko R5 deletes the own board's R5 read only")
check(identical(ckr_knockout_reads(r, 2L, 3L)$t_ms, r$t_ms[-4]), "2: the own board decides which reads go")
v <- ckr_variant_reads(r, 1L)
check(identical(names(v), c("baseline", "ko_R3", "ko_R5")) && nrow(v$baseline) == 8L && identical(v$baseline$t_ms, r$t_ms), "2: variants; baseline keeps every read")

# ---- 3. family pooling and the log rate ratio --------------------------------------------------------------------------
ap3 <- data.table(variant = "baseline", Batch = "B1", AnimalNum = "X", healthy = TRUE, PhaseLabel = CKR_PHASES,
                  crossings = c(30L, 10L, 5L, 7L, 0L, 4L, 8L, 2L, 1L), eligible = TRUE)
f3 <- ckr_family_outcomes(ap3)
fa <- f3[family == "Active"]; fi <- f3[family == "Inactive"]
check(near(fa$reference_rate, 10 / 12) && near(fa$post_rate, 12 / 36) && near(fa$log_rate_ratio, log(12.5 / 36) - log(10.5 / 12)) &&
        near(fa$single_rate, 30 / 12) && fa$single_phase == "A1" && fa$post_phases == "A3;A4;A5", "3: Active family by hand (A2 | A3-A5, +0.5)")
check(near(fi$reference_rate, 4 / 12) && near(fi$post_rate, 11 / 36) && near(fi$log_rate_ratio, log(11.5 / 36) - log(4.5 / 12)) &&
        near(fi$single_rate, 4 / 12) && fi$post_hours == 36 && fi$reference_hours == 12, "3: Inactive family by hand (I2 | I3-I5)")
f3b <- ckr_family_outcomes(copy(ap3)[PhaseLabel == "A4", eligible := FALSE][PhaseLabel == "I2", eligible := FALSE])
check(!f3b[family == "Active"]$family_complete && is.na(f3b[family == "Active"]$log_rate_ratio) && is.na(f3b[family == "Active"]$post_rate) &&
        near(f3b[family == "Active"]$single_rate, 30 / 12), "3: an incomplete phase leaves the family NA (not 0); A1 alone stays")
check(is.na(f3b[family == "Inactive"]$single_rate) && is.na(f3b[family == "Inactive"]$reference_rate), "3: an incomplete I2 is NA for I2 alone and its family")
f3z <- ckr_family_outcomes(copy(ap3)[, crossings := 0L])
check(near(f3z[family == "Active"]$log_rate_ratio, -log(3)) && near(f3z[family == "Active"]$reference_rate, 0), "3: zero counts: log(0.5/36) - log(0.5/12)")
L3 <- ckr_outcome_long(f3)
check(setequal(L3$outcome, CKR_OUTCOMES) && nrow(L3) == 8L && near(L3[outcome == "rate_I2"]$value, 4 / 12), "3: long form names the single phases rate_A1 and rate_I2")

# ---- 4. scale and materiality -----------------------------------------------------------------------------------------
y <- c(1:5, 11:15); bt <- rep(c("B1", "B2"), each = 5)
s4 <- ckr_scale(y, bt)
check(near(s4$scale_sd, sqrt(20 / 9)) && identical(s4$scale_source, CKR_SCALE_SOURCE), "4: sd() of the residuals of lm(y ~ Batch), by hand")
s4b <- ckr_scale(1:5, rep("B1", 5))
check(near(s4b$scale_sd, sd(1:5)) && grepl("one batch", s4b$scale_source), "4: one batch: the SD of y, labelled as a dev fallback")
toy <- function(sh, out) data.table(reader = 3L, family = "Active", outcome = out, Batch = "B1", board = 1L,
                                    CageEpisodeID = paste0("B1|sys.", rep(1:2, length.out = length(sh)), "|CC4"), AnimalNum = paste0("a", seq_along(sh)),
                                    baseline = 1, knockout = 1 + sh, shift = sh)
sh4 <- rbind(toy(c(0.2, 0.2, 0.2, -0.2, 0.1), "reference_rate"), toy(c(0.19, 0.19, 0.19, 0, 0), "post_rate"),
             toy(c(rep(0, 9), 6), "log_rate_ratio"), toy(c(rep(0, 9), 4), "rate_A1"))
sc4 <- data.table(family = "Active", outcome = c("reference_rate", "post_rate", "log_rate_ratio", "rate_A1"), scale_sd = 1)
mt4 <- ckr_materiality(sh4, sc4)
check(identical(names(mt4), c("reader", "family", "outcome", "n_animals", "n_board_recordings", "scale_sd", "median_shift", "median_abs_shift_sd",
                              "p90_abs_shift_sd", "material", "rule")) && is.integer(mt4$reader) && is.logical(mt4$material), "4: exactly the declared columns")
check(identical(mt4$outcome, c("reference_rate", "post_rate", "log_rate_ratio", "rate_A1")), "4: declared outcome order")
check(isTRUE(mt4[outcome == "reference_rate"]$material) && near(mt4[outcome == "reference_rate"]$median_abs_shift_sd, 0.2) &&
        near(mt4[outcome == "reference_rate"]$median_shift, 0.2), "4: median |shift| of exactly 0.2 SD is material")
check(identical(mt4[outcome == "post_rate"]$material, FALSE) && near(mt4[outcome == "post_rate"]$p90_abs_shift_sd, 0.19), "4: 0.19 SD everywhere is not")
check(isTRUE(mt4[outcome == "log_rate_ratio"]$material) && near(mt4[outcome == "log_rate_ratio"]$p90_abs_shift_sd, 0.6, 1e-9) &&
        identical(mt4[outcome == "rate_A1"]$material, FALSE) && near(mt4[outcome == "rate_A1"]$p90_abs_shift_sd, 0.4, 1e-9),
      "4: the 90th percentile (type 7) decides when the median is 0: 0.6 SD material, 0.4 SD not")
check(all(mt4$n_animals == c(5L, 5L, 10L, 10L)) && all(mt4$n_board_recordings == 2L) && all(mt4$rule == CKR_MATERIALITY_RULE), "4: counts and the rule text")
check(identical(c(CKR_MATERIAL_MEDIAN_SD, CKR_MATERIAL_P90_SD), c(ASR_MATERIAL_MEDIAN_SD, ASR_MATERIAL_P90_SD)), "4: the srs_v1.1 thresholds")
sc4b <- ckr_scales(rbind(toy(c(0, 0, 0, 0, 0), "reference_rate")[, `:=`(Batch = "B1", baseline = 1:5)],
                         toy(c(0, 0, 0, 0, 0), "reference_rate")[, `:=`(Batch = "B2", baseline = 11:15, AnimalNum = paste0("b", 1:5))]))
check(nrow(sc4b) == 1L && near(sc4b$scale_sd, sqrt(20 / 9)) && sc4b$n_animals == 10L && sc4b$n_batches == 2L, "4: ckr_scales takes each animal's baseline once")

# ---- 5. parity gate ---------------------------------------------------------------------------------------------------
check(identical(c(CKR_PARITY_MIN_EXACT_SHARE, CKR_PARITY_MAX_POOLED_REL_DIFF), c(0.95, 0.01)), "5: the declared thresholds")
check(ckr_parity_pass(95 / 100, 10 / 1000) && !ckr_parity_pass(94 / 100, 0) && !ckr_parity_pass(1, 11 / 1000) && !ckr_parity_pass(NA, 0),
      "5: >= 95% exact and <= 1% pooled pass; below or above fails; NA fails")
apP <- function(diff) data.table(variant = "baseline", stage31_crossings = rep(5L, 20), crossings = 5L + diff, records = 1L, stage31_records = 1L,
                                  eligible = TRUE, brackets_variant = TRUE)
p1 <- ckr_parity(apP(c(1L, rep(0L, 19))), "x", TRUE)
p2 <- ckr_parity(apP(c(1L, 1L, rep(0L, 18))), "x", TRUE)
p3 <- ckr_parity(apP(c(-2L, rep(0L, 19))), "x", TRUE)
check(p1$pass && near(p1$exact_share, 0.95) && near(p1$pooled_rel_diff, 0.01), "5: one of 20 off by 1 (total 100): passes at the thresholds")
check(!p2$pass && near(p2$exact_share, 0.9), "5: two of 20 off: fails the exact share")
check(!p3$pass && near(p3$pooled_rel_diff, 0.02) && near(p3$exact_share, 0.95), "5: a 2% pooled difference fails")
p0 <- ckr_parity(apP(0L)[0], "x", TRUE)
check(p0$n == 0L && is.na(p0$pass) && !isTRUE(all(p0$pass)), "5: no animal-phase: pass NA, and a gate over it does not pass")
ph1 <- data.table(Batch = "B1", System = "sys.1", AnimalID = "A", PhaseLabel = CKR_PHASES, stage31_crossings = 1:9, stage31_records = 2L, eligible = TRUE)
lv1 <- data.table(Batch = "B1", System = "sys.1", AnimalID = "A", PhaseLabel = rep(CKR_PHASES, each = 2), Crossings = rep(c(0L, 1L), 9) * rep(1:9, each = 2),
                  ReadCount = 1L, SessionSpansWholeBlock = TRUE, AnimalSpansWholeBlock = TRUE, SpanOverlapSec = 300)
check(ckr_stage31_live_compare(ph1, lv1, "x")$pass, "5: the frozen-run comparison sums the 5-min bins")
check(!ckr_stage31_live_compare(ph1, copy(lv1)[2, Crossings := 2L], "x")$pass && !ckr_stage31_live_compare(ph1, copy(lv1)[3, SpanOverlapSec := 200], "x")$pass,
      "5: one crossing or one incomplete bin off fails the exact frozen-run comparison")

# ---- 6. healthy units -------------------------------------------------------------------------------------------------
m11 <- sxm_read("v1.1")
check(setequal(m11[action == "exclude_spatial" & CC == "CC4"]$CageEpisodeID, CKR_EXPECTED_EXCLUDED) && length(CKR_EXPECTED_EXCLUDED) == 2L,
      "6: the declared episodes are manifest v1.1's CC4 exclude_spatial records")
ro <- CJ(Batch = paste0("B", 1:6), board = 1:5)[, .(Batch, System = paste0("sys.", board), AnimalNum = paste0(Batch, "_", board))]
u6 <- ckr_units(ro, CKR_EXPECTED_EXCLUDED)
check(identical(sort(u6[healthy == FALSE]$CageEpisodeID), sort(CKR_EXPECTED_EXCLUDED)) && sum(u6$healthy) == 28L && identical(u6$board, ro[, as.integer(sub("sys.", "", System, fixed = TRUE))]),
      "6: exactly the two episodes are left out; 28 healthy board-recordings")
check(fails(ckr_units(ro[, System := "5"], character()), "sys"), "6: a System label without a board is refused")

# ---- 7. end to end: a synthetic B2 CC4 recording ------------------------------------------------------------------------
# Single reads every 600 s from 2023-12-01 13:00 (I1) to 2023-12-06 11:00 (I6), 709 per tag, so each read but the last is
# one vendor record at its own time and the last read closes the stream. Board 1: OR901 alternates R1/R3, OR902 R5/R8
# (R5 lies 116 units from the origin, so its first record is the origin placeholder), OR904 stays at R2 (100 units: only
# the origin placeholder, never a move). Board 5 (B2|sys.5|CC4, an excluded episode): OR903 alternates R1/R3.
tmp <- tempfile("ckr_")
raw_root <- file.path(tmp, "raw"); raw_dir <- file.path(tmp, "raw_data")
dir.create(file.path(raw_root, "B2", "raw"), recursive = TRUE); dir.create(file.path(raw_dir, "B2"), recursive = TRUE)
S0 <- as.numeric(tt("2023-12-01 13:00:00")); n <- 709L; k <- 0:(n - 1L)
an <- data.table(AnimalID = c("OR901", "OR902", "OR903", "OR904"), board = c(1L, 1L, 5L, 1L), tag = c("TA01", "TA02", "TA03", "TA04"), off_ms = c(0L, 200L, 400L, 600L))
pid <- list(OR901 = ifelse(k %% 2 == 0, 1L, 3L), OR902 = ifelse(k %% 2 == 0, 5L, 8L), OR903 = ifelse(k %% 2 == 0, 1L, 3L), OR904 = rep(2L, n))
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
ev <- rbindlist(lapply(seq_len(nrow(an)), function(i) data.table(t = S0 + 600 * k + an$off_ms[i] / 1000, tag = an$tag[i], board = an$board[i],
                                                                pos = pid[[an$AnimalID[i]]])))[order(t)]
rows <- sprintf("%s;%s;unknown;AM%d-%d;75;;;1;;;;0;;", excel(ev$t), ev$tag, ev$board, ev$pos)
dev <- unlist(lapply(1:5, function(b) sprintf("#ID-Device;AM%d-%d;%g;%g;0;SAM", b, 1:8, RFID_GRID_X, RFID_GRID_Y)))
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, dev, rows), collapse = "\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]),
         file.path(raw_root, "B2", "raw", "MultiMouseMonitor-23.12.01-Batch2_change_4_01Dec23.csv"))
# the vendor export, written by hand from the estimator's rule (not with apr_positions())
xy <- function(p) cbind(RFID_GRID_X[p], RFID_GRID_Y[p])
vrows <- unlist(lapply(seq_len(nrow(an)), function(i) {
  p <- pid[[an$AnimalID[i]]]
  rec <- xy(p[1:(n - 1L)]); rec[1, ] <- c(0, 0)                              # the first record: the origin placeholder (or R1 itself)
  if (an$AnimalID[i] == "OR904") rec <- rec[1, , drop = FALSE]             # R2 never moves 200 units from the origin
  ts <- S0 + 600 * (seq_len(nrow(rec)) - 1L)
  rec <- rbind(rec, rec[nrow(rec), ]); ts <- c(ts, S0 + 600 * (n - 1L))     # the closing record at the last read
  sprintf("%s.%03d;%s_sys.%d;%s;SAM;%.1f;%.1f;0.0", format(tt(ts + 0) , "%d.%m.%Y %H:%M:%S"), an$off_ms[i], an$AnimalID[i], an$board[i], an$tag[i], rec[, 1], rec[, 2])
}))
writeLines(c("DateTime;Animal;RFID;AM;xPos;yPos;zPos", vrows), file.path(raw_dir, "B2", "E9_SIS_B2_CC4_AnimalPos.csv"))
corr <- tibble::tibble(Batch = character(), CageChange = character(), AnimalID = character(), old_System = character(), new_System = character(), evidence = character())
res <- ckr_recording("B2", raw_dir, raw_root, character(), corr, CKR_EXPECTED_EXCLUDED)
ap <- res$animal_phase
check(nrow(ap) == 4L * 9L * 3L && setequal(ap$variant, c("baseline", "ko_R3", "ko_R5")) && all(ap$eligible) && all(ap$hours == 12), "7: 4 animals x 9 phases x 3 variants, all complete")
check(identical(res$animals[order(AnimalNum)]$healthy, c(TRUE, TRUE, FALSE, TRUE)), "7: the board-5 animal belongs to the excluded episode B2|sys.5|CC4")
check(all(res$animals$stream_identical) && all(res$animals$vendor_records == res$animals$emulated_records), "7: the emulated baseline equals the hand-written export record by record")
pr <- ckr_parity(ap[healthy == TRUE], "x", TRUE)
check(pr$pass && pr$exact_share == 1 && pr$pooled_rel_diff == 0 && pr$records_matches == pr$n && pr$eligibility_matches == pr$n, "7: parity with Stage 31 is exact")
cr <- function(v, a) ap[variant == v & AnimalNum == a]$crossings
check(all(cr("baseline", "OR901") == 72L) && all(cr("baseline", "OR902") == 72L) && all(cr("baseline", "OR904") == 0L) &&
        all(ap[variant == "baseline"]$crossings == ap[variant == "baseline"]$stage31_crossings), "7: 72 crossings per 12-h phase at 600-s alternation, by hand")
check(all(cr("ko_R3", "OR901") == 0L) && all(cr("ko_R5", "OR901") == 72L) && all(cr("ko_R5", "OR902") == 0L) && all(cr("ko_R3", "OR902") == 72L) &&
        all(cr("ko_R3", "OR903") == 0L), "7: deleting R3 (R5) removes exactly the R1/R3 (R5/R8) alternation")
check(all(ap$brackets_variant), "7: every variant's stream still brackets the nine phases")
check(res$animals[AnimalNum == "OR901"]$reads_R3_on_board == 354L && res$animals[AnimalNum == "OR902"]$reads_R5_on_board == 355L &&
        all(res$animals$modal_read_board == res$animals$board) && res$recording$raw_log == "MultiMouseMonitor-23.12.01-Batch2_change_4_01Dec23.csv",
      "7: read counts per knocked reader, the board check and the raw log used")
af <- ckr_family_outcomes(ap)
lrr <- function(v) af[variant == v & AnimalNum == "OR901" & family == "Active"]$log_rate_ratio
check(near(lrr("baseline"), log(216.5 / 36) - log(72.5 / 12)) && near(lrr("ko_R3"), -log(3)), "7: log rate ratio by hand")
sh <- ckr_shifts(af)
check(!any(sh$AnimalNum == "OR903") && setequal(sh$AnimalNum, c("OR901", "OR902", "OR904")) && nrow(sh) == 2L * 8L * 3L, "7: shifts on healthy animals only")
check(near(sh[reader == 3L & outcome == "rate_A1" & AnimalNum == "OR901"]$shift, -6) && near(sh[reader == 3L & outcome == "rate_A1" & AnimalNum == "OR902"]$shift, 0) &&
        near(sh[reader == 5L & outcome == "post_rate" & AnimalNum == "OR902"][order(family)]$shift, c(-6, -6)) &&
        near(sh[reader == 5L & family == "Inactive" & outcome == "rate_I2" & AnimalNum == "OR904"]$shift, 0), "7: rate shifts by hand (both families)")
sc <- ckr_scales(sh)
mt <- ckr_materiality(sh, sc)
m7 <- mt[reader == 3L & outcome == "rate_A1"]
check(near(sc[outcome == "rate_A1"]$scale_sd, sqrt(12)) && grepl("one batch", sc[outcome == "rate_A1"]$scale_source) && m7$n_animals == 3L &&
        m7$n_board_recordings == 1L && near(m7$median_shift, 0) && near(m7$p90_abs_shift_sd, 4.8 / sqrt(12)) && isTRUE(m7$material),
      "7: scale (one batch: SD of 6, 6, 0), shifts -6, 0, 0: median 0, 90th percentile 4.8 -> material")
check(nrow(mt) == 16L && setequal(mt[family == "Active"]$outcome, c("reference_rate", "post_rate", "log_rate_ratio", "rate_A1")) &&
        setequal(mt[family == "Inactive"]$outcome, c("reference_rate", "post_rate", "log_rate_ratio", "rate_I2")), "7: 2 readers x 2 families x 4 outcomes")
lv7 <- copy(ap[variant == "baseline"])[, .(Batch, System, AnimalID = AnimalNum, PhaseLabel, Crossings = stage31_crossings, ReadCount = stage31_records,
                                           SessionSpansWholeBlock = TRUE, AnimalSpansWholeBlock = TRUE, SpanOverlapSec = 300)]
check(ckr_stage31_live_compare(res$stage31, lv7, "x")$pass, "7: the Stage 31 reference table feeds the frozen-run comparison")
srs7 <- data.table(AnimalNum = c("OR901", "OR902", "OR904"), CC = "CC4", Batch = "B2", window = "active", crossing_rate_baseline = c(6, 6, 0),
                   crossing_rate_ko_R3 = c(0, 6, 0), crossing_rate_ko_R5 = c(6, 0, 0))
ct <- ckr_srs_continuity(sh, srs7)
check(near(ct[reader == 3L & outcome == "rate_A1"]$max_abs_diff_shift, 0) && ct[reader == 3L & outcome == "rate_A1"]$n_both == 3L &&
        ct[reader == 5L & outcome == "rate_I2"]$n_both == 0L, "7: continuity with an srs-shaped table")
# a System label that is not the board holding the reads is refused (the knockout would act on the wrong board)
d7 <- ckr_vendor_rows("B2", raw_dir, character(), corr)
d7$System[d7$AnimalID == "OR904"] <- "sys.2"
rec7 <- rfid_read_recording(raw_root, "B2", "CC4", keep_read_offsets = TRUE)
check(fails(ckr_process_recording(d7, rec7$events, rfid_animalpos_tag_map(file.path(raw_dir, "B2", "E9_SIS_B2_CC4_AnimalPos.csv")), "B2", CKR_EXPECTED_EXCLUDED),
            "not the board holding most of its reads"), "7: a wrong board label stops the run")
unlink(tmp, recursive = TRUE)

# ---- 8. group-blind, pure, declared -----------------------------------------------------------------------------------
code_lines <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
files <- c("Functions/cc4_reader_knockout.R", "Testing/audits/audit_cc4_reader_knockout.R")
for (f in files) {
  x <- code_lines(f)
  check(!any(grepl("sus_animals|con_animals|later_outcome|outcome_group|experimental_condition|canonical_outcomes|mmm_combz|combz[a-z0-9_]*[.](csv|xlsx|rds)|ReferenceGroup|phenotype",
                   x, ignore.case = TRUE)), paste0("8: ", f, " reads no group, CombZ or outcome file"))
  check(!any(grepl("\\bGroup\\b|\\bCondition\\b", x)), paste0("8: ", f, " uses no group or condition column"))
}
helper <- readLines("Functions/cc4_reader_knockout.R", warn = FALSE)
check(!any(grepl("spatial_exclusion_manifest|spatial_analysis_exclusions|sxm_read", helper)), "8: the helper does not read the spatial manifest")
check(!any(grepl("fread\\(|fwrite\\(|read_csv|read_delim|readLines\\(|writeLines\\(|readRDS|saveRDS|dir\\.create|file\\.create|write\\.csv|read\\.csv|sink\\(|system2?\\(",
                 code_lines("Functions/cc4_reader_knockout.R"))), "8: the helper does no I/O")
check(!file.exists("Functions/rfid_cc4_reader_knockout.R") && !any(grepl("^rfid_", basename(files))), "8: not a Functions/rfid_*.R file (Stage 33's frozen glob)")
dl <- ckr_design_lines(CKR_SCRIPT)
check(any(grepl("^Units\\.", dl)) && any(grepl("^Parity gate\\.", dl)) && any(grepl("^Shift, scale and materiality\\.", dl)) &&
        any(grepl("95%", dl)) && any(grepl("0.2 scale SD", dl)), "8: the pre-declared design block is in the audit header")
ar <- ckr_args(c("--run-id=dev_x", "--recordings=B1_CC4"))
check(identical(ar$run_id, "dev_x") && identical(ar$recordings, "B1_CC4") && is.na(ar$output_root), "8: arguments")
check(fails(ckr_args("--resume"), "Unknown") && fails(ckr_args(c("--run-id=a", "--run-id=b")), "once") && fails(ckr_args("--run-id="), "Empty"),
      "8: unknown, repeated and empty arguments are refused")

# the audit's protection guard: after the isolated-root check and before the first write; ended before the output hashes
main <- paste(deparse(Filter(function(e) is.call(e) && identical(e[[1]], as.name("if")),
                             as.list(parse("Testing/audits/audit_cc4_reader_knockout.R", keep.source = FALSE)))[[1]]), collapse = "\n")
pos <- function(p) regexpr(p, main, fixed = TRUE)[1]
check(pos("mmm_isolated_output_root_check") > 0 && pos("mmm_isolated_output_root_check") < pos("mmm_protection_guard_begin") &&
        pos("mmm_protection_guard_begin") < pos("dir.create") && pos("mmm_protection_guard_begin") < pos("fwrite") &&
        pos("mmm_protection_guard_end") > pos("README.txt") && pos("mmm_protection_guard_end") < pos("outs <- setdiff(list.files(tmp") &&
        pos("mmm_protection_guard_end") < pos("file.rename"),
      "9: the protection guard starts before the first write and ends before the output hashes and the rename")

cat("test_cc4_reader_knockout: all checks passed\n")
