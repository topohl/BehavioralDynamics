# Contract test for the Stage 30 canonical-movement wrapper (Functions/stage30_movement.R).
#
# Synthetic files in tempdir() only: nothing here reads project data or sources an Analysis/ stage.
# Checks:
#   1. the explicit-file reader is the canonical reader (values identical to mmm_evs_read_preprocessed), reads csv.gz and non-CC
#      change labels, and keeps the Group guard (file header and in-memory input);
#   2. the wrapper stream is the canonical stream (mmm_evs_build_stream: pos and seeds identical), including the raw seed;
#   3. hand-computed windows: carry-forward into the window, an event exactly at the window start is not counted, an event at the
#      window end is excluded, tied reads count once (run boundaries), mid-dwell starts, carry-forward after the last read, a seeded
#      late animal, explicit missingness before the file, the file-end rule (NA by default, canonical value with na_if_file_ends_early = FALSE);
#   4. the unseeded stream refuses (strict) or flags (non-strict) exactly the animal-windows a seed can change, and equals the seeded
#      stream elsewhere;
#   5. on the canonical 18:30 active / light windows the wrapper equals mmm_bf_window_table() column for column;
#   6. several windows per SourceFile in one call equal separate calls; the stream object is not modified; the exposed window
#      streams give run boundaries equal to n_events;
#   7. window-table validation; 8. the round trip truncates to the millisecond exactly like the preprocessing writer.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root.

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
for (h in c("behavior_analysis_config.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "stage30_movement.R")) source_mmm_helper(h)

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
P <- function(x) as.POSIXct(x, tz = "UTC")
CRIT <- MMM_BEHAVIOR_CONFIG$metrics$fragmentation$bout_criterion_s

# ---------------------------------------------------------------- synthetic data
root <- file.path(tempdir(), "s30mv_test"); unlink(root, recursive = TRUE)
pre_dir <- file.path(root, "pre"); raw_dir <- file.path(root, "raw"); gz_dir <- file.path(root, "gz")
for (d in c(pre_dir, file.path(raw_dir, "B1"), file.path(raw_dir, "B2"), gz_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
iso <- function(x) paste0(sub(" ", "T", x), "Z")
b1 <- data.table(
  DateTime = iso(c("2022-10-28 18:30:10.000", "2022-10-28 18:40:00.000", "2022-10-28 18:50:00.000", "2022-10-28 19:00:00.000",
                   "2022-10-28 19:00:00.000", "2022-10-28 20:00:00.500",
                   "2022-10-28 18:45:00.000", "2022-10-28 19:30:00.000",
                   "2022-10-28 18:30:10.000", "2022-10-29 18:30:30.000")),
  AnimalID = c(rep("0001", 6), "OR9", "OR9", "0003", "0003"),
  System = c(rep("sys.1", 8), "sys.2", "sys.2"),
  PositionID = c(1L, 2L, 2L, 3L, 4L, 1L, 5L, 6L, 8L, 7L), CageChange = "CC1", Batch = "B1")
fwrite(b1, file.path(pre_dir, "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"))
# raw: OR9 was last read on grid (100, 116) -> PositionID 6 before t0; also an off-grid read that the seed must ignore
writeLines(c("DateTime;Animal;RFID;AM;xPos;yPos;zPos",
             "28.10.2022 18:20:00.000;OR9_sys.1;X9;SAM;100.0;116.0;0.0",
             "28.10.2022 18:25:00.000;OR9_sys.1;X9;SAM;150.0;50.0;0.0",
             "28.10.2022 18:30:10.000;0001_sys.1;X1;SAM;0.0;0.0;0.0"),
           file.path(raw_dir, "B1", "E9_SIS_B1_CC1_AnimalPos.csv"))
# a post-EPM-style session stored as csv.gz (change label without CC number); plain csv if R.utils (needed by fread for .gz) is absent
GZ <- if (requireNamespace("R.utils", quietly = TRUE)) ".gz" else ""
b2 <- data.table(DateTime = iso(c("2023-02-22 18:30:52.427", "2023-02-22 18:40:00.000", "2023-02-23 17:10:00.000", "2023-02-23 17:20:00.000", "2023-02-24 06:00:00.000")),
                 AnimalID = "OR20", System = "sys.3", PositionID = c(2L, 3L, 4L, 3L, 1L), CageChange = "postEPM", Batch = "B2")
fwrite(b2, file.path(gz_dir, paste0("E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv", GZ)))
writeLines(c("DateTime;Animal;RFID;AM;xPos;yPos;zPos", "22.02.2023 18:29:00.000;OR20_sys.3;X20;SAM;100.0;0.0;0.0"),
           file.path(raw_dir, "B2", "E9_SIS_B2_postEPM_AnimalPos.csv"))
f_b1 <- file.path(pre_dir, "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"); f_b2 <- file.path(gz_dir, paste0("E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv", GZ))

# ---------------------------------------------------------------- 1. reader
x1 <- s30mv_read_preprocessed(f_b1); x0 <- mmm_evs_read_preprocessed(pre_dir)
check(identical(unclass(x1)[names(x1)], unclass(x0)[names(x0)]), "explicit-file reader must equal the canonical reader")
x2 <- s30mv_read_preprocessed(f_b2)
check(identical(unique(x2$SourceFile), "E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv") && identical(unique(x2$CageChange), "postEPM") &&
      inherits(x2$DateTime, "POSIXct"), "csv.gz with a non-CC change label is read; SourceFile drops .gz")
fwrite(cbind(b1, Group = "SUS"), file.path(root, "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"))
e <- errmsg(s30mv_read_preprocessed(file.path(root, "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv")))
check(!is.na(e) && grepl("Unexpected Group column", e), "a Group column in the file header must stop the read")
e <- errmsg(s30mv_build_stream(cbind(x1, Group = "SUS"), raw_dir))
check(!is.na(e) && grepl("Group", e), "a Group column in in-memory input must stop the stream")
ok("reader = canonical reader; csv.gz and postEPM names; Group guard")

# ---------------------------------------------------------------- 2. stream
st0 <- mmm_evs_build_stream(pre_dir, raw_dir)
sS <- s30mv_build_stream(x1, raw_dir); sU <- s30mv_build_stream(x1, raw_dir = NULL)
check(identical(sS$pos, st0$pos) && identical(sS$seeds, st0$seeds), "wrapper stream must equal mmm_evs_build_stream (pos and seeds)")
check(nrow(sS$seeds) == 1 && sS$seeds$AnimalNum == "OR9" && sS$seeds$PositionID == 6L && sS$seeds$DateTime == P("2022-10-28 18:30:10"),
      "late animal OR9 is seeded at t0 with the last on-grid raw position (6)")
check(!any(sU$pos$is_seed), "raw_dir = NULL builds an unseeded stream")
sG <- s30mv_build_stream(x2, raw_dir)
check(nrow(sG$seeds) == 0 && identical(sG$file_span$CC, "CCpostEPM"), "no late animal -> no seed; CC label derived as in mmm_evs_windows")
ok("stream = canonical stream; seed recovered from raw")

# ---------------------------------------------------------------- 3. hand-computed windows
W <- data.table(SourceFile = "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv",
                window_id = c("a_carry", "b_start_at_event", "c_tie_at_end", "d_mid_dwell", "e_carry_after_last", "f_file_end", "i_before_file", "g_before_late_read", "h_after_late_read"),
                start = P(c("2022-10-28 18:35:00", "2022-10-28 18:40:00", "2022-10-28 18:40:00", "2022-10-28 18:45:30.25", "2022-10-28 19:00:00",
                            "2022-10-29 18:30:00", "2022-10-28 18:00:00", "2022-10-28 18:31:00", "2022-10-28 19:10:00")),
                end = P(c("2022-10-28 18:55:00", "2022-10-28 19:00:00", "2022-10-28 19:00:01", "2022-10-28 19:45:30.25", "2022-10-28 20:00:00",
                          "2022-10-29 19:30:00", "2022-10-28 18:20:00", "2022-10-28 18:44:00", "2022-10-28 19:50:00")))
pos_before <- data.table::copy(sS$pos)
m <- s30mv_window_metrics(sS, W)
check(identical(sS$pos, pos_before), "the stream object must not be modified")
g <- function(w, a, col) m[window_id == w & AnimalNum == a][[col]]
check(g("a_carry", "1", "n_events") == 1 && g("a_carry", "1", "obs_s") == 1200 && g("a_carry", "1", "crossing_rate") == 3, "carry-forward into the window; 1 change")
check(g("a_carry", "OR9", "n_events") == 1 && g("a_carry", "OR9", "obs_s") == 1200, "seeded late animal: seed position until its first read, then 1 change")
check(g("a_carry", "3", "n_events") == 0 && g("a_carry", "3", "crossing_rate") == 0, "no change -> rate 0")
check(g("b_start_at_event", "1", "n_events") == 0 && g("b_start_at_event", "1", "obs_s") == 1200, "event at the window start and at the window end not counted")
check(g("c_tie_at_end", "1", "n_events") == 1 && g("c_tie_at_end", "1", "obs_s") == 1201, "two tied reads at one instant count as one run boundary")
check(g("d_mid_dwell", "1", "n_events") == 1 && g("d_mid_dwell", "1", "obs_s") == 3600, "mid-dwell start: occupied position carried in, 1 change")
check(g("e_carry_after_last", "3", "n_events") == 0 && g("e_carry_after_last", "3", "obs_s") == 3600, "carry-forward after the last read to the window end")
check(all(m[window_id == "f_file_end", status] == "file_ends_before_window_end") && all(is.na(m[window_id == "f_file_end", crossing_rate])),
      "window beyond the file end: NA by default")
mk <- s30mv_window_metrics(sS, W[window_id == "f_file_end"], na_if_file_ends_early = FALSE)
check(mk[AnimalNum == "1", obs_s] == 3600 && mk[AnimalNum == "1", n_events] == 0 && mk[AnimalNum == "3", n_events] == 1,
      "na_if_file_ends_early = FALSE returns the canonical carry-forward value")
check(nrow(m[window_id == "i_before_file"]) == 3 && all(m[window_id == "i_before_file", status] == "not_observed_in_window") &&
      all(m[window_id == "i_before_file", obs_s] == 0) && all(is.na(m[window_id == "i_before_file", n_events])), "explicit missingness before the file")
check(g("g_before_late_read", "OR9", "obs_s") == 780 && g("g_before_late_read", "OR9", "n_events") == 0 && isTRUE(g("g_before_late_read", "OR9", "known_at_start")),
      "seeded animal observed from the seed")
check(!"Group" %in% names(m) && nrow(m) == 27, "one row per animal x window, no Group column")
ok("hand-computed windows (carry-forward, clip, ties, mid-dwell, seed, missingness, file end)")

# ---------------------------------------------------------------- 4. unseeded stream
e <- errmsg(s30mv_window_metrics(sU, W))
check(!is.na(e) && grepl("seed", e), "strict: an unseeded stream must refuse windows a seed can change")
mu <- s30mv_window_metrics(sU, W, strict_seed = FALSE)
xs <- merge(m, mu, by = c("AnimalNum", "window_id"), suffixes = c("", ".u"))
check(setequal(xs[status.u == "seed_unavailable", paste(AnimalNum, window_id)],
               c("OR9 a_carry", "OR9 b_start_at_event", "OR9 c_tie_at_end", "OR9 g_before_late_read")),
      "exactly the windows starting before OR9's first read (18:45) and ending after t0 are flagged")
check(all(xs[status.u != "seed_unavailable", same(n_events, n_events.u) & same(obs_s, obs_s.u) & same(crossing_rate, crossing_rate.u)]),
      "unseeded equals seeded wherever no seed can matter")
check(nrow(s30mv_window_metrics(sU, W[window_id == "h_after_late_read"])) == 3, "strict mode passes for windows after every first read")
ok("seed step: refused / flagged only where a seed can change the window")

# ---------------------------------------------------------------- 5. canonical windows
cw <- st0$windows
Wc <- rbind(cw[, .(SourceFile, window_id = "active", start = active_start, end = active_end)], cw[, .(SourceFile, window_id = "light", start = light_start, end = light_end)])
mc <- s30mv_window_metrics(sS, Wc, full = TRUE)
for (wt in c("active", "light")) {
  ref <- mmm_bf_window_table(mmm_evs_window_stream(st0, wt), CRIT)$animals
  z <- merge(ref, mc[window_id == wt], by = c("AnimalNum", "CC"), suffixes = c(".r", ".w"))
  cols <- setdiff(names(ref), c("AnimalNum", "CC"))
  check(nrow(z) == nrow(ref) && all(vapply(cols, function(k) identical(z[[paste0(k, ".r")]], z[[paste0(k, ".w")]]), TRUE)),
        paste("wrapper must equal mmm_bf_window_table on the canonical", wt, "window"))
}
ok("canonical active / light windows: identical to mmm_bf_window_table")

# ---------------------------------------------------------------- 6. several windows per SourceFile, several files
sAll <- s30mv_build_stream(rbind(x1, x2), raw_dir)
Wm <- rbind(W[window_id %in% c("a_carry", "d_mid_dwell")],
            data.table(SourceFile = "E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv", window_id = "a_carry",
                       start = P("2023-02-23 17:00:00"), end = P("2023-02-23 18:00:00")))
mAll <- s30mv_window_metrics(sAll, Wm)
sep <- rbind(s30mv_window_metrics(sS, W[window_id == "a_carry"]), s30mv_window_metrics(sS, W[window_id == "d_mid_dwell"]),
             s30mv_window_metrics(sG, Wm[SourceFile == "E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv"]))
z <- merge(mAll, sep, by = c("SourceFile", "AnimalNum", "window_id"), suffixes = c("", ".s"))
check(nrow(z) == nrow(mAll) && nrow(mAll) == 7 && all(same(z$n_events, z$n_events.s) & same(z$obs_s, z$obs_s.s)), "one call with several windows = separate calls")
check(mAll[SourceFile == "E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv", n_events] == 2 && mAll[SourceFile == "E9_SIS_B2_postEPM_AnimalPos_preprocessed.csv", obs_s] == 3600,
      "post-EPM-style window long after file start: position carried in, 2 changes")
ok("several windows and files per call")

wsl <- s30mv_window_streams(sS, W[window_id %in% c("b_start_at_event", "c_tie_at_end")])
eb <- mmm_bf_events_from_runs(wsl$b_start_at_event$runs); ec <- mmm_bf_events_from_runs(wsl$c_tie_at_end$runs)
check(nrow(eb[AnimalNum == "1"]) == 0 && nrow(wsl$b_start_at_event$events[AnimalNum == "1" & t == 0]) == 1,
      "window streams: a change at the window start is in ws$events but not a run boundary")
check(nrow(ec[AnimalNum == "1"]) == 1 && nrow(wsl$c_tie_at_end$events[AnimalNum == "1" & t == 1200]) == 2 &&
      nrow(ec) == sum(m[window_id == "c_tie_at_end", n_events]), "window streams: run boundaries = n_events; tied reads give 2 event rows, 1 boundary")
ok("window streams exposed for further canonical-stream metrics")

# ---------------------------------------------------------------- 7. window-table validation
bad <- function(w) !is.na(errmsg(s30mv_window_metrics(sS, w)))
check(bad(W[1][, end := start]), "end <= start must fail")
check(bad(rbind(W[1], W[1])), "duplicate (SourceFile, window_id) must fail")
check(bad(W[1][, SourceFile := "E9_SIS_B9_CC1_AnimalPos_preprocessed.csv"]), "unknown SourceFile must fail")
check(bad(W[1][, start := as.character(start)]), "character start must fail")
check(bad(W[1][, `:=`(start = as.POSIXct("2022-10-28 18:35:00", tz = "Europe/Berlin"), end = as.POSIXct("2022-10-28 18:55:00", tz = "Europe/Berlin"))]),
      "a non-UTC window must fail (the logger clock is stored as UTC)")
ok("window-table validation")

# ---------------------------------------------------------------- 8. round trip = preprocessing writer representation
memd <- data.frame(DateTime = as.POSIXct(c("28.10.2022 18:30:10.508", "28.10.2022 18:31:00.117"), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"),
                   AnimalID = "0001", System = "sys.1", PositionID = c(1L, 2L), CageChange = "CC1", Batch = "B1")
rt <- s30mv_roundtrip(list(E9_SIS_B1_CC1_AnimalPos_preprocessed.csv = memd), dir = file.path(root, "rt"))
wf <- file.path(root, "wr", "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"); dir.create(dirname(wf))
old <- options(digits.secs = 3); readr::write_csv(memd, wf); options(old)
check(identical(rt$DateTime, s30mv_read_preprocessed(wf)$DateTime), "round trip equals the preprocessing writer + canonical reader")
check(all(abs(as.numeric(rt$DateTime) - as.numeric(memd$DateTime)) < 1e-3), "round trip changes timestamps by < 1 ms")
# one-call entry point with in-memory preprocess_animalpos_file()-style data equals the explicit path
b1m <- as.data.frame(b1); b1m$DateTime <- as.POSIXct(sub("Z$", "", sub("T", " ", b1$DateTime)), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
m1 <- s30mv_movement(list(E9_SIS_B1_CC1_AnimalPos_preprocessed.csv = b1m), W, raw_dir = raw_dir)
check(identical(m1[, .(AnimalNum, window_id, n_events, obs_s, crossing_rate, status)], m[, .(AnimalNum, window_id, n_events, obs_s, crossing_rate, status)]),
      "s30mv_movement(in-memory list) equals read -> build -> window metrics")
check(!is.na(errmsg(s30mv_movement(b1m, W, raw_dir))), "an unnamed data.frame must be refused (use a list named by SourceFile)")
ok("round trip reproduces the file representation; one-call entry point")

unlink(root, recursive = TRUE)
cat("test_stage30_movement_wrapper: all checks passed\n")
