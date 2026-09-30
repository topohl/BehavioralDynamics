# Contract test for the Stage 32 phase windows, coverage rule and window metrics (Functions/stage32_windows.R; registry v1.0).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data or any S: path, and no Analysis/ runner is
# sourced or executed. Checks:
#   1. phase windows: A_k / L_k offsets from the 18:30 anchor, 12-h length, block labels (L_k = I(k+1)), clean phase set;
#   2. board observation intervals: attribution through the v2 System of the AnimalNum, unattributed records reported;
#   3. coverage rule (a)-(d): block kept, board start (A1: t0), board end within 10 min (primary) / 0 (strict), clean set,
#      explicit B5|sys.1|CC4 exclusion from L2; the manifest reasons;
#   4. window metrics through the unchanged canonical wrapper, one window at a time: crossing_rate = an independent count
#      oracle in A1 / A2 / L1; metrics NA for incomplete animal-phases; the comparison helper.
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage32_windows.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
for (h in c("behavior_analysis_config.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "stage30_movement.R", "stage32_windows.R")) source_mmm_helper(h)
fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
utc <- function(x) as.POSIXct(x, tz = "UTC")

# ---------------------------------------------------------------- 1. phase windows
fw <- data.table(SourceFile = c("E9_SIS_B5_CC4_AnimalPos_preprocessed.csv", "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"), Batch = c("B5", "B1"),
                 CC = c("CC4", "CC1"), t0 = utc(c("2023-12-02 18:30:02", "2022-10-28 18:31:05")), active_start = utc(c("2023-12-02 18:30:00", "2022-10-28 18:30:00")))
pw <- s32w_phase_windows(fw)
check(nrow(pw) == 16L && all(as.numeric(pw$end) - as.numeric(pw$start) == 12 * 3600), "8 phases per file, 12 h each")
a <- pw[SourceFile == fw$SourceFile[2]]
check(identical(a[phase == "A1", start], utc("2022-10-28 18:30:00")) && identical(a[phase == "L1", start], utc("2022-10-29 06:30:00")) &&
      identical(a[phase == "A2", start], utc("2022-10-29 18:30:00")) && identical(a[phase == "L3", end], utc("2022-10-31 18:30:00")) &&
      identical(a[phase == "A4", end], utc("2022-11-01 06:30:00")), "A_k = anchor + 24(k-1) h; L_k = anchor + 24(k-1) h + 12 h")
check(identical(a[phase == "L1", block], "I2") && identical(a[phase == "L4", block], "I5") && identical(a[phase == "A3", block], "A3"), "L_k = block I(k+1)")
check(identical(a[phase == "A3", phase_index], 2L) && identical(a[phase == "L2", phase_index], 1L), "phase index 0..3 / light index 0..3")
check(identical(s32w_in_clean(c("CC1", "CC1", "CC3", "CC4", "CC4", "CC4", "CC4"), c("A4", "L4", "L3", "A1", "L1", "A2", "L2")),
                c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)), "clean phase set: CC1-CC3 A1-A4 L1-L3; CC4 A1 L1 A2")
check(identical(s32w_block_label(c("Active", "Inactive"), c(3, 0), c(0, 2)), c("A3", "I2")) &&
      inherits(tryCatch(s32w_block_label("Light", 1, 0), error = identity), "error"), "block labels")
ok("phase windows, blocks, clean set")

# ---------------------------------------------------------------- 2. board intervals
sf <- fw$SourceFile[2]
an <- data.table(SourceFile = sf, Batch = "B1", CC = "CC1", System = c("sys.1", "sys.1", "sys.2"), AnimalNum = c("11", "12", "21"))
rr <- data.table(SourceFile = sf, AnimalNum = c("11", "12", "12", "21", "21", "99"),
                 t = utc(c("2022-10-28 15:00:00", "2022-11-01 12:00:00", "2022-10-28 15:30:00", "2022-10-28 16:00:00", "2022-11-01 06:25:00", "2022-11-01 13:00:00")))
bd <- s32w_board_intervals(rr, an)
check(nrow(bd) == 2L && identical(bd[System == "sys.1", board_first], utc("2022-10-28 15:00:00")) && identical(bd[System == "sys.1", board_last], utc("2022-11-01 12:00:00")) &&
      identical(bd[System == "sys.2", board_last], utc("2022-11-01 06:25:00")) && identical(bd[System == "sys.1", board_n_animals], 2L), "board first/last raw record per SourceFile x System")
un <- attr(bd, "unattributed"); check(nrow(un) == 1L && un$n_records == 1L && un$labels == "99", "records of animals without a v2 System are unattributed")
check(inherits(tryCatch(s32w_board_intervals(rr, rbind(an, data.table(SourceFile = sf, Batch = "B1", CC = "CC1", System = "sys.3", AnimalNum = "11"))), error = identity), "error"),
      "an animal on two boards within a file stops")
ok("board observation intervals from raw records")

# ---------------------------------------------------------------- 3. coverage rule
pw1 <- pw[SourceFile == sf]
kept <- data.table(SourceFile = sf, block = c("A1", "I2", "A2", "I3", "A3", "I4", "A4"), n_rows = 10L)
cov <- s32w_coverage(an, pw1, kept, bd)
check(nrow(cov) == 3L * 8L, "one row per animal x phase")
x2 <- cov[System == "sys.2"]
check(x2[phase == "A4", board_ends_ok_primary] && !x2[phase == "A4", board_ends_ok_strict] && x2[phase == "A4", complete_primary] && !x2[phase == "A4", complete_strict],
      "board ending 5 min before the phase end: complete (10-min tolerance), not strict")
check(all(!cov[phase == "L4", complete_primary]) && all(grepl("\\(a\\)", cov[phase == "L4", reason_primary])), "L4 at CC1: block not kept and outside the clean set")
check(all(cov[System == "sys.1" & in_clean_set == TRUE, complete_primary & complete_strict]), "board observed through the phases: complete in every clean phase")
bd2 <- copy(bd); bd2[System == "sys.2", board_last := utc("2022-11-01 06:19:00")]
check(!s32w_coverage(an, pw1, kept, bd2)[System == "sys.2" & phase == "A4", complete_primary], "board ending 11 min before the phase end: incomplete")
bd3 <- copy(bd); bd3[System == "sys.2", board_first := utc("2022-10-28 18:31:10")]   # after t0 = 18:31:05
c3 <- s32w_coverage(an, pw1, kept, bd3)
check(!c3[System == "sys.2" & phase == "A1", complete_primary] && c3[System == "sys.2" & phase == "A2", complete_primary], "A1 requires the board start <= t0; later phases <= the phase start")
kept2 <- kept[block != "A3"]
check(!any(s32w_coverage(an, pw1, kept2, bd)[phase == "A3", complete_primary]), "(a) a block missing from the v2 file makes the phase incomplete")
# explicit exclusion
sf5 <- fw$SourceFile[1]
an5 <- data.table(SourceFile = sf5, Batch = "B5", CC = "CC4", System = c("sys.1", "sys.1", "sys.2"), AnimalNum = c("314", "OR620", "999"))
bd5 <- data.table(SourceFile = sf5, System = c("sys.1", "sys.2"), board_first = utc("2023-12-02 13:00:00"), board_last = utc(c("2023-12-07 13:00:00", "2023-12-07 13:00:00")),
                  board_n_records = 10L, board_n_animals = 2L)
kept5 <- data.table(SourceFile = sf5, block = c("A1", "I2", "A2", "I3", "A3", "I4", "A4", "I5"), n_rows = 1L)
c5 <- s32w_coverage(an5, pw[SourceFile == sf5], kept5, bd5)
check(identical(sort(c5[explicit_exclusion == TRUE, unique(phase)]), sort(c("L2", "A3", "L3", "A4", "L4"))) && c5[explicit_exclusion == TRUE, uniqueN(AnimalNum)] == 2L &&
      !any(c5[AnimalNum == "999", explicit_exclusion]), "explicit exclusion: B5|sys.1|CC4 animals from L2 onward only")
check(all(c5[phase %in% c("A1", "L1", "A2"), complete_primary]) && !any(c5[!(phase %in% c("A1", "L1", "A2")), complete_primary]), "CC4: only A1, L1, A2 are used")
cnt <- s32w_coverage_counts(c5)
check(cnt[phase == "A1", n_complete_primary] == 3L && cnt[phase == "L2", n_explicit_exclusion] == 2L, "coverage counts")
ok("coverage rule (a)-(d), tolerance, strict variant, explicit exclusion")

# ---------------------------------------------------------------- 4. window metrics through the canonical wrapper
set.seed(32)
t0 <- utc("2022-10-28 18:30:05"); file_end <- utc("2022-10-30 06:29:00")
ids <- c("11", "12", "21", "22"); sysm <- c(`11` = "sys.1", `12` = "sys.1", `21` = "sys.2", `22` = "sys.2")
rows <- rbindlist(lapply(ids, function(id) {
  tt <- sort(c(t0, t0 + sort(sample(1:(as.numeric(file_end) - as.numeric(t0)), 400))))
  data.table(DateTime = tt, AnimalID = id, System = sysm[[id]], PositionID = sample(1:8, length(tt), TRUE), Batch = "B1", CageChange = "CC1")
}))
rows[, DateTime := as.POSIXct(round(as.numeric(DateTime), 3), origin = "1970-01-01", tz = "UTC")]
tmp <- file.path(normalizePath(tempdir(), winslash = "/"), "s32w"); unlink(tmp, recursive = TRUE); dir.create(tmp)
pre <- s30mv_roundtrip(list(E9_SIS_B1_CC1_AnimalPos_preprocessed.csv = as.data.frame(rows)), dir = tmp)
st <- s30mv_build_stream(pre, raw_dir = NULL)
fwin <- mmm_evs_windows(pre); pws <- s32w_phase_windows(fwin)[phase %in% c("A1", "L1", "A2")]
met <- s32w_window_metrics(st, pws, 39.3970988275363)
check(nrow(met) == 12L && setequal(unique(met$window_id), c("A1", "L1", "A2")), "one row per animal x window")
oracle <- function(id, a, b) {                       # independent count of PositionID changes in [a, b) and carry-forward observed hours
  p <- pre[AnimalNum == id][order(DateTime)]; ch <- which(p$PositionID[-1] != p$PositionID[-nrow(p)]) + 1L
  n <- sum(p$DateTime[ch] >= a & p$DateTime[ch] < b)
  obs <- as.numeric(b) - max(as.numeric(a), as.numeric(min(p$DateTime)))
  n / (obs / 3600) }
for (w in c("A1", "L1", "A2")) for (id in ids) {
  r <- met[window_id == w & AnimalNum == id]; ww <- pws[phase == w]
  check(abs(r$crossing_rate - oracle(id, ww$start, ww$end)) < 1e-9, paste("crossing_rate oracle", w, id)) }
direct <- s30mv_window_metrics(st, pws[phase == "A2", .(SourceFile, window_id = phase, start, end)], bout_criterion_s = 39.3970988275363,
                               na_if_file_ends_early = FALSE, full = TRUE)
check(isTRUE(all.equal(met[window_id == "A2"][order(AnimalNum)]$shared_zone_use, direct[order(AnimalNum)]$shared_zone_use)) &&
      isTRUE(all.equal(met[window_id == "A2"][order(AnimalNum)]$occupancy_dispersion, direct[order(AnimalNum)]$occupancy_dispersion)), "window-by-window call = a direct wrapper call")
check(all(met[window_id == "A2", status] == "file_ends_before_window_end") && all(is.finite(met[window_id == "A2", crossing_rate])),
      "na_if_file_ends_early = FALSE keeps the final-phase metrics (completeness is the coverage rule)")
# long table: metrics NA unless complete
bdw <- data.table(SourceFile = unique(pre$SourceFile), System = c("sys.1", "sys.2"), board_first = t0 - 3600, board_last = c(file_end + 3600, utc("2022-10-29 12:00:00")),
                  board_n_records = 1L, board_n_animals = 2L)
keptw <- data.table(SourceFile = unique(pre$SourceFile), block = c("A1", "I2", "A2"), n_rows = 1L)
covw <- s32w_coverage(unique(st$pos[is_seed == FALSE, .(SourceFile, Batch = "B1", CC = "CC1", System, AnimalNum)]), pws, keptw, bdw)
lw <- s32w_long(met, covw)
check(all(is.na(lw[System == "sys.2" & phase %in% c("L1", "A2"), crossing_rate])) && all(is.finite(lw[System == "sys.2" & phase == "A1", crossing_rate])) &&
      all(is.finite(lw[System == "sys.1", crossing_rate])), "metrics NA for animal-phases whose board stops early (no carry-forward coverage)")
check(identical(lw[, .N, by = .(CageEpisodeID, phase)][, unique(N)], 2L) && all(lw$n_in_cage == 2L), "n_in_cage per cage episode")
cmp <- s32w_compare(data.table(AnimalNum = c("a", "b"), CC = "CC1", v = c(1, NA), f = c(TRUE, FALSE)), data.table(AnimalNum = c("a", "b"), CC = "CC1", v = c(1 + 1e-10, NA), f = c(TRUE, FALSE)),
                    c("AnimalNum", "CC"), c("v", "f"))
check(all(cmp$passed), "compare: within tolerance, identical NA pattern, logical equality")
cmp2 <- s32w_compare(data.table(AnimalNum = "a", CC = "CC1", v = 1), data.table(AnimalNum = "a", CC = "CC1", v = 1 + 1e-6), c("AnimalNum", "CC"), "v")
check(!cmp2$passed, "compare: a difference above 1e-9 fails")
ok("window metrics one window at a time; count oracle; NA outside coverage; comparison helper")
cat("test_stage32_windows: all checks passed\n")
