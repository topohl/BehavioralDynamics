# Stage 09 resolution contract.
#
# Stage 09 is the primary manuscript analysis at 10-min bins, with 5-min declared
# as its resolution sensitivity. This test locks the properties that must hold at
# BOTH resolutions, so a sensitivity run can never quietly become a different
# analysis:
#
#   1. the committed default stays the 10-min canonical primary;
#   2. expected slots are 72 at 10 min and 144 at 5 min;
#   3. both resolutions select the SAME 12-h clock window;
#   4. neither bridges into a second night;
#   5. missing bins reduce coverage; they never shift or back-fill the window;
#   6. exact Active/Inactive classification is unaffected by resolution.
#
# Runs entirely in memory. No S: drive access required.

suppressPackageStartupMessages({
  library(dplyr); library(tibble)
})

source("Analysis/_pipeline_setup.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")

check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)

STAGE09 <- "Analysis/09_early_prediction_model_ladder.R"
check(file.exists(STAGE09), "Stage 09 source must be present")

# Load the production definitions for a given declared bin level, exactly as the
# other Stage 09 harnesses do: evaluate only the named top-level assignments.
load_stage09 <- function(bin_level_value) {
  old <- Sys.getenv("MMM_STAGE09_BIN_LEVEL", unset = NA_character_)
  Sys.setenv(MMM_STAGE09_BIN_LEVEL = bin_level_value)
  on.exit({
    if (is.na(old)) Sys.unsetenv("MMM_STAGE09_BIN_LEVEL") else Sys.setenv(MMM_STAGE09_BIN_LEVEL = old)
  }, add = TRUE)
  wanted <- c("bin_level", "early_window_hours", "bin_size_min", "bin_seconds",
              "expected_early_bins_per_animal", "active_phase_values",
              "inactive_phase_values", "normalize_phase_label", "is_active_phase",
              "select_primary_active_window")
  env <- new.env(parent = globalenv())
  for (e in parse(STAGE09)) {
    if (!is.call(e) || length(e) < 3) next
    if (!as.character(e[[1]]) %in% c("<-", "=")) next
    nm <- tryCatch(as.character(e[[2]]), error = function(err) "")
    if (length(nm) == 1L && nm %in% wanted) tryCatch(eval(e, envir = env), error = function(err) {})
  }
  missing <- setdiff(wanted, ls(env))
  check(length(missing) == 0L,
        paste0("Stage 09 must define at bin_level=", bin_level_value, ": ",
               paste(missing, collapse = ", ")))
  env
}

# ------------------------------------------------------------------
# 1. The committed default is the 10-min canonical primary.
# ------------------------------------------------------------------
Sys.unsetenv("MMM_STAGE09_BIN_LEVEL")
def <- load_stage09_default <- local({
  wanted <- c("bin_level", "bin_size_min", "bin_seconds", "expected_early_bins_per_animal",
              "early_window_hours")
  env <- new.env(parent = globalenv())
  for (e in parse(STAGE09)) {
    if (!is.call(e) || length(e) < 3) next
    if (!as.character(e[[1]]) %in% c("<-", "=")) next
    nm <- tryCatch(as.character(e[[2]]), error = function(err) "")
    if (length(nm) == 1L && nm %in% wanted) tryCatch(eval(e, envir = env), error = function(err) {})
  }
  env
})
check(identical(def$bin_level, "10min_based"),
      "1: the committed Stage 09 default must remain 10min_based")
check(def$bin_size_min == 10, "1: default bin_size_min must be 10")
check(def$bin_seconds == 600, "1: default bin_seconds must be 600")
check(def$expected_early_bins_per_animal == 72, "1: default expected slots must be 72")
check(def$early_window_hours == 12, "1: the window must remain 12 h")

# ------------------------------------------------------------------
# 2. Expected slots scale with resolution; the window duration does not.
# ------------------------------------------------------------------
e10 <- load_stage09("10min_based")
e05 <- load_stage09("5min_based")

check(e10$bin_size_min == 10 && e10$bin_seconds == 600,
      "2: 10min_based must derive 10 min / 600 s")
check(e05$bin_size_min == 5 && e05$bin_seconds == 300,
      "2: 5min_based must derive 5 min / 300 s")
check(e10$expected_early_bins_per_animal == 72,
      "2: 10-min window must expect exactly 72 slots")
check(e05$expected_early_bins_per_animal == 144,
      "2: 5-min window must expect exactly 144 slots")
check(e10$early_window_hours == e05$early_window_hours &&
      e10$early_window_hours == 12,
      "2: both resolutions must use the same 12 h window")
check(e10$expected_early_bins_per_animal * e10$bin_seconds ==
      e05$expected_early_bins_per_animal * e05$bin_seconds,
      "2: expected slots x bin seconds must be the same span at both resolutions")

# ------------------------------------------------------------------
# 3/4/5. Same clock window, no bridging into night 2, missing bins reduce
#        coverage rather than shifting or back-filling the window.
# ------------------------------------------------------------------
# One synthetic session. The Active block runs 18:30 -> 06:30; the following
# Inactive block then a SECOND Active night follow. An animal that first appears
# late must still be measured against the 18:30 anchor.
# Block 0 starts at 06:30 and is Inactive; Active blocks are ODD and start at 18:30.
blk <- 1L
active_start <- blk * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC
t0 <- as.POSIXct(active_start, origin = "1970-01-01", tz = "UTC")

make_session <- function(bin_sec) {
  n_night1 <- 12 * 3600 / bin_sec
  # night 1 Active, then 12 h Inactive, then night 2 Active
  tibble(
    BinStart = c(t0 + (seq_len(n_night1) - 1L) * bin_sec,
                 t0 + 12 * 3600 + (seq_len(n_night1) - 1L) * bin_sec,
                 t0 + 24 * 3600 + (seq_len(n_night1) - 1L) * bin_sec),
    Phase = rep(c("Active", "Inactive", "Active"), each = n_night1),
    AnimalNum = "A1", SourceFile = "S1"
  )
}

for (res in c("10min_based", "5min_based")) {
  env <- if (res == "10min_based") e10 else e05
  bs <- env$bin_seconds
  expected <- env$expected_early_bins_per_animal
  sel <- env$select_primary_active_window
  dat <- make_session(bs)

  # complete animal
  out <- sel(dat, window_hours = env$early_window_hours, bin_size_seconds = bs)
  check(nrow(out) == expected,
        paste0("3 (", res, "): a complete animal must yield exactly ", expected, " rows"))
  check(all(out$target_slot >= 1L & out$target_slot <= expected),
        paste0("3 (", res, "): target slots must lie in 1..", expected))
  check(!any(duplicated(out$target_slot)),
        paste0("3 (", res, "): target slots must be unique"))
  check(format(min(out$target_window_start), "%H:%M") == "18:30",
        paste0("3 (", res, "): the window must start at 18:30"))
  check(format(min(out$target_window_end), "%H:%M") == "06:30",
        paste0("3 (", res, "): the window must end at 06:30"))
  check(max(out$elapsed_sec_in_window) < 12 * 3600,
        paste0("4 (", res, "): no row may reach 12 elapsed hours"))
  check(all(as.character(out$Phase) == "Active"),
        paste0("4 (", res, "): only Active rows may be selected"))
  # night 2 must never be reached
  check(max(out$BinStart) < t0 + 12 * 3600,
        paste0("4 (", res, "): the window must not bridge into the second night"))

  # late-starting animal: window anchor must NOT shift, coverage must drop
  drop_n <- 6L
  late <- dat[-seq_len(drop_n), ]
  out_late <- sel(late, window_hours = env$early_window_hours, bin_size_seconds = bs)
  check(format(min(out_late$target_window_start), "%H:%M") == "18:30",
        paste0("5 (", res, "): a late first observation must not shift the 18:30 anchor"))
  check(min(out_late$target_slot) == drop_n + 1L,
        paste0("5 (", res, "): leading absence must appear as missing leading slots"))
  check(nrow(out_late) == expected - drop_n,
        paste0("5 (", res, "): missing bins must reduce coverage, not be back-filled"))
  check(max(out_late$target_slot) == expected,
        paste0("5 (", res, "): the window end must not extend to compensate for a late start"))

  # interior gap: coverage drops, slots keep their absolute index
  gap <- dat[-c(20L, 21L), ]
  out_gap <- sel(gap, window_hours = env$early_window_hours, bin_size_seconds = bs)
  check(nrow(out_gap) == expected - 2L,
        paste0("5 (", res, "): an interior gap must reduce the observed slot count"))
  check(!any(out_gap$target_slot %in% c(20L, 21L)),
        paste0("5 (", res, "): an interior gap must leave those slot indices absent"))
  check(max(out_gap$target_slot) == expected,
        paste0("5 (", res, "): an interior gap must not shift later slots earlier"))
}

# ------------------------------------------------------------------
# 6. Exact phase classification is resolution-independent.
# ------------------------------------------------------------------
for (env in list(e10, e05)) {
  f <- env$is_active_phase
  check(f("Active") && f("active") && f(" Active "), "6: Active must be included")
  check(!f("Inactive") && !f("inactive") && !f(" Inactive "),
        "6: Inactive must be EXCLUDED at every resolution")
  check(f("dark") && f("night"), "6: dark/night must be included")
  check(!f("light") && !f("day"), "6: light/day must be excluded")
}

# ------------------------------------------------------------------
# 7. An unusable bin_level must be detectable, not silently default to 10 min.
# ------------------------------------------------------------------
# The harness above evaluates only top-level assignments, so the production
# `if (...) stop(...)` guard is not reachable from it. Assert the two halves
# separately: the derivation yields a non-finite value for an unparseable
# level, and the guard that turns that into an error is present in the source.
bad_env <- load_stage09("not_a_resolution")
check(!is.finite(bad_env$bin_size_min),
      "7: an unparseable bin_level must not derive a usable bin size")
src <- paste(readLines(STAGE09, warn = FALSE), collapse = "\n")
check(grepl("could not derive a bin size from bin_level", src, fixed = TRUE),
      "7: Stage 09 must stop() when bin_size_min cannot be derived")
check(grepl("Stage 09 bin-size mismatch", src, fixed = TRUE),
      "7: Stage 09 must stop() when the declared bin size disagrees with the data")
cat("PASS: Stage 09 resolution contract (72 slots at 10 min, 144 at 5 min, same 12 h clock window)\n")
