# ================================================================
# Canonical first-night (first post-regrouping Active) window selector
# MMMSociability
# ================================================================
# One source of truth for the prespecified first-night exposure window:
#
#     first cage change
#     first Active phase block of that cage change
#     18:30 inclusive -> 06:30 exclusive
#     exactly 12 h
#
# The anchor is a property of the experimental CLOCK, not of any animal: an
# animal whose first genuine read falls at 19:10 is still scored against a
# window starting at 18:30, so partial coverage is MEASURED rather than hidden.
# The window is never shifted later and missing night-1 slots are never
# back-filled from night 2.
#
# This mirrors the validated selector in
# Analysis/09_early_prediction_model_ladder.R :: select_primary_active_window().
# Stage 09 itself is deliberately left untouched so its validated artifacts do
# not become stale for a refactor; Testing/tests/test_first_night_window_parity.R
# asserts byte-for-byte agreement between the two implementations by evaluating
# Stage 09's real function definition out of its source file.
#
# Row-count selection (`local_bin <= 12h/bin`) is NOT equivalent and must not be
# used: it consumes a fixed NUMBER of bins, so whenever night-1 bins are missing
# it reaches into the second dark block. On the current 111-animal data the count
# rule agrees with this clock window for only 50/111 animals at 10-min bins and
# 33/111 at 5-min bins.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

if (!exists("mmm_is_active_phase", mode = "function", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("phase_classification_helpers.R")
  } else {
    stop("first_night_window_helpers.R requires phase_classification_helpers.R.", call. = FALSE)
  }
}
if (!exists("animalpos_phase_block_index", mode = "function", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("animalpos_preprocessing_helpers.R")
  } else {
    stop("first_night_window_helpers.R requires animalpos_preprocessing_helpers.R.", call. = FALSE)
  }
}

MMM_FIRST_NIGHT_WINDOW_HOURS <- 12

#' Label of the first cage change present in `x`.
#'
#' Numeric-aware so "CC1" wins over "CC10"; falls back to lexical order when no
#' digits are present. Matches Stage 09's get_first_cage_change().
mmm_first_cage_change <- function(x) {
  ux <- unique(as.character(x))
  cc_num <- suppressWarnings(as.numeric(str_extract(ux, "[0-9]+")))
  if (any(is.finite(cc_num))) ux[which.min(ifelse(is.finite(cc_num), cc_num, Inf))] else sort(ux)[1]
}

#' Per-session clock anchor for the first Active phase block.
#'
#' @param active_dat Rows already restricted to the first cage change AND to
#'   exact Active phase, carrying `BinStart` and the session column.
#' @param session_col Session identifier column; `SourceFile` when present,
#'   else `Batch`. The anchor is per session because each batch was run on its
#'   own calendar dates.
mmm_first_night_anchors <- function(active_dat, session_col = NULL) {
  session_col <- session_col %||% if ("SourceFile" %in% names(active_dat)) "SourceFile" else "Batch"
  if (!session_col %in% names(active_dat)) {
    stop("mmm_first_night_anchors() needs a session column; tried SourceFile and Batch.", call. = FALSE)
  }
  if (!"BinStart" %in% names(active_dat)) {
    stop("mmm_first_night_anchors() requires a BinStart timestamp column.", call. = FALSE)
  }
  active_dat %>%
    mutate(.mmm_session = as.character(.data[[session_col]]),
           .mmm_block = animalpos_phase_block_index(.data$BinStart)) %>%
    group_by(.mmm_session) %>%
    summarise(target_phase_block = min(.mmm_block, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      target_window_start = as.POSIXct(
        target_phase_block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
        origin = "1970-01-01", tz = "UTC"
      ),
      target_window_end = target_window_start + MMM_FIRST_NIGHT_WINDOW_HOURS * 3600
    )
}

#' Select the canonical first-night window.
#'
#' @param dat Bin-level data with AnimalNum, Phase, CageChange, BinStart and a
#'   session column.
#' @param bin_size_sec Bin width, used only to derive `target_slot`; the window
#'   itself is defined by elapsed clock time, never by bin count.
#' @param window_hours Window length; 12 by contract.
#' @param first_cage_change Optional explicit label; inferred when NULL.
#' @return Selected rows plus target_phase_block, target_window_start,
#'   target_window_end, elapsed_sec_in_window, target_slot,
#'   elapsed_hours_in_window (for trajectory x-axes) and expected_slots.
mmm_select_first_night_window <- function(dat,
                                         bin_size_sec,
                                         window_hours = MMM_FIRST_NIGHT_WINDOW_HOURS,
                                         first_cage_change = NULL,
                                         session_col = NULL) {
  stopifnot(is.data.frame(dat), is.finite(bin_size_sec), bin_size_sec > 0)
  for (needed in c("AnimalNum", "Phase", "CageChange", "BinStart")) {
    if (!needed %in% names(dat)) {
      stop("mmm_select_first_night_window() requires column: ", needed, call. = FALSE)
    }
  }
  mmm_assert_phase_classifiable(dat$Phase, "first-night input Phase column")
  first_cc <- first_cage_change %||% mmm_first_cage_change(dat$CageChange)

  # Exact Active membership. A permissive substring test would admit Inactive.
  act <- dat %>% filter(as.character(.data$CageChange) == first_cc, mmm_is_active_phase(.data$Phase))
  if (nrow(act) == 0L) {
    stop("No exact-Active rows found for cage change ", first_cc,
         "; the first-night window cannot be built.", call. = FALSE)
  }

  session_col <- session_col %||% if ("SourceFile" %in% names(act)) "SourceFile" else "Batch"
  anchors <- mmm_first_night_anchors(act, session_col = session_col)

  act %>%
    mutate(.mmm_session = as.character(.data[[session_col]])) %>%
    left_join(anchors, by = c(".mmm_session" = ".mmm_session")) %>%
    mutate(
      elapsed_sec_in_window = as.numeric(difftime(.data$BinStart, .data$target_window_start, units = "secs")),
      target_slot = as.integer(.data$elapsed_sec_in_window %/% bin_size_sec) + 1L
    ) %>%
    filter(.data$elapsed_sec_in_window >= 0,
           .data$elapsed_sec_in_window < window_hours * 3600) %>%
    mutate(
      # Clock elapsed time, so missing bins leave REAL temporal gaps on a
      # trajectory axis instead of being closed up by row rank.
      elapsed_hours_in_window = .data$elapsed_sec_in_window / 3600,
      expected_slots = as.integer(window_hours * 3600 / bin_size_sec),
      first_cage_change = first_cc,
      window_hours = window_hours,
      bin_size_sec = bin_size_sec,
      window_definition = paste0(
        "fixed clock window: first Active phase block after first cage change, ",
        "18:30 inclusive to 06:30 exclusive"
      )
    ) %>%
    select(-.mmm_session) %>%
    arrange(.data$AnimalNum, .data$target_slot)
}

#' Per-animal first-night window QC.
#'
#' Coverage is measured against the fixed expected slot count; leading,
#' interior and trailing gaps are reported separately because a late first read
#' is a different phenomenon from a mid-window dropout.
mmm_first_night_window_qc <- function(selected) {
  if (nrow(selected) == 0L) return(tibble())
  expected <- selected$expected_slots[1]
  selected %>%
    group_by(across(any_of(c("AnimalNum", "Group", "Sex")))) %>%
    summarise(
      target_window_start = first(.data$target_window_start),
      target_window_end = first(.data$target_window_end),
      window_hours = first(.data$window_hours),
      bin_size_sec = first(.data$bin_size_sec),
      expected_slots = expected,
      observed_slots = n_distinct(.data$target_slot),
      first_target_slot = min(.data$target_slot),
      last_target_slot = max(.data$target_slot),
      first_bin_start = min(.data$BinStart),
      last_bin_start = max(.data$BinStart),
      max_elapsed_hours = max(.data$elapsed_hours_in_window),
      missing_leading_slots = min(.data$target_slot) - 1L,
      missing_trailing_slots = expected - max(.data$target_slot),
      missing_interior_slots = (max(.data$target_slot) - min(.data$target_slot) + 1L) - n_distinct(.data$target_slot),
      max_internal_gap_slots = {
        s <- sort(unique(.data$target_slot))
        if (length(s) < 2L) 0L else as.integer(max(diff(s)) - 1L)
      },
      .groups = "drop"
    ) %>%
    mutate(
      coverage_fraction = .data$observed_slots / .data$expected_slots,
      window_complete = .data$observed_slots == .data$expected_slots,
      no_interior_missing = .data$missing_interior_slots == 0L,
      no_trailing_missing = .data$missing_trailing_slots == 0L
    )
}
