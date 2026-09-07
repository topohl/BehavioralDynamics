# ================================================================
# Phase-generic acute-window selectors (Active AND Inactive)
# MMMSociability
# ================================================================
# Generalizes Functions/acute_active_window_helpers.R from Active-only to
# either scheduled phase, without changing any existing semantics.
#
# CLOCK ARITHMETIC (from animalpos_preprocessing_helpers.R)
#   block b spans [b*12h + 06:30, (b+1)*12h + 06:30)
#     even b -> 06:30-18:30  = Inactive
#     odd  b -> 18:30-06:30  = Active
#
# WINDOW DEFINITIONS
#   Active   : the FIRST Active block after the cage change.
#              18:30 inclusive -> 06:30 exclusive, exactly 12 h.
#   Inactive : the first scheduled Inactive block IMMEDIATELY FOLLOWING that
#              Active block, i.e. block index (first Active block + 1).
#              06:30 inclusive -> 18:30 exclusive, exactly 12 h.
#
# The Inactive anchor is deliberately derived from the Active anchor rather
# than from "the first row labelled Inactive". A cage change can be followed by
# a partial Inactive block BEFORE the first full Active response; taking the
# earliest Inactive rows would silently select that pre-response block. Keying
# off the Active anchor guarantees the Inactive window is the post-active one.
#
# CONTRACTS
#   * Functions/first_night_window_helpers.R is NOT modified.
#   * Functions/acute_active_window_helpers.R is NOT modified; its Active
#     results are reproduced exactly by this file (parity test).
#   * Missing slots stay missing; no window is extended into the next block.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

for (.needed in c("phase_classification_helpers.R", "animalpos_preprocessing_helpers.R",
                  "acute_active_window_helpers.R")) {
  .probe <- switch(.needed,
    "phase_classification_helpers.R" = "mmm_is_active_phase",
    "animalpos_preprocessing_helpers.R" = "animalpos_phase_block_index",
    "acute_active_window_helpers.R" = "mmm_match_cage_change")
  if (!exists(.probe, mode = "function", inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
      source_mmm_helper(.needed)
    } else {
      stop("acute_phase_window_helpers.R requires ", .needed, call. = FALSE)
    }
  }
}
rm(.needed, .probe)

MMM_PHASE_WINDOW_HOURS <- 12

#' Phase membership test by name.
mmm_is_phase <- function(x, phase) {
  phase <- match.arg(phase, c("Active", "Inactive"))
  if (phase == "Active") mmm_is_active_phase(x) else mmm_is_inactive_phase(x)
}

#' Rows of ONE cage change with the global phase-block index attached.
.mmm_cc_rows <- function(dat, cage_change = NULL, session_col = NULL) {
  for (needed in c("AnimalNum", "Phase", "CageChange", "BinStart")) {
    if (!needed %in% names(dat)) {
      stop("phase-window selection requires column: ", needed, call. = FALSE)
    }
  }
  mmm_assert_phase_classifiable(dat$Phase, "phase-window input Phase column")
  cc <- mmm_match_cage_change(dat$CageChange, cage_change)
  sub <- dat %>% filter(as.character(.data$CageChange) == cc)
  if (nrow(sub) == 0L) stop("No rows for cage change ", cc, ".", call. = FALSE)
  session_col <- session_col %||% if ("SourceFile" %in% names(sub)) "SourceFile" else "Batch"
  if (!session_col %in% names(sub)) {
    stop("phase-window selection needs a session column; tried SourceFile and Batch.", call. = FALSE)
  }
  list(
    rows = sub %>% mutate(
      .mmm_session = as.character(.data[[session_col]]),
      .mmm_block = animalpos_phase_block_index(.data$BinStart)
    ),
    cage_change = cc,
    session_col = session_col
  )
}

#' Per-session anchor blocks: the first Active block of the cage change, and
#' the Inactive block immediately after it.
mmm_phase_anchor_blocks <- function(prepared) {
  prepared$rows %>%
    filter(mmm_is_active_phase(.data$Phase)) %>%
    group_by(.data$.mmm_session) %>%
    summarise(active_block = min(.data$.mmm_block, na.rm = TRUE), .groups = "drop") %>%
    mutate(inactive_block = .data$active_block + 1L)
}

#' Select the acute window for a specified cage change and phase.
#'
#' @param phase "Active" (first Active block after the cage change) or
#'   "Inactive" (the Inactive block immediately following that Active block).
#' @return Selected rows plus target_cage_change, target_phase, target_phase_block,
#'   target_window_start/end, elapsed_sec_in_window, target_slot, TimeHours,
#'   expected_slots.
mmm_select_acute_phase_window <- function(dat,
                                          bin_size_sec,
                                          cage_change = NULL,
                                          phase = c("Active", "Inactive"),
                                          window_hours = MMM_PHASE_WINDOW_HOURS,
                                          session_col = NULL) {
  phase <- match.arg(phase)
  stopifnot(is.data.frame(dat), is.finite(bin_size_sec), bin_size_sec > 0)
  prepared <- .mmm_cc_rows(dat, cage_change = cage_change, session_col = session_col)
  anchors <- mmm_phase_anchor_blocks(prepared)

  anchors <- anchors %>%
    mutate(target_phase_block = if (phase == "Active") .data$active_block else .data$inactive_block,
           target_window_start = mmm_block_window_start(.data$target_phase_block),
           target_window_end = .data$target_window_start + window_hours * 3600)

  # Parity guard: an Active anchor must be an odd block, an Inactive anchor even.
  expected_parity <- if (phase == "Active") 1L else 0L
  bad <- anchors %>% filter(.data$target_phase_block %% 2L != expected_parity)
  if (nrow(bad) > 0L) {
    stop("Phase-block parity violated for ", phase, " in cage change ",
         prepared$cage_change, " (sessions: ", paste(bad$.mmm_session, collapse = ", "), ")",
         call. = FALSE)
  }

  out <- prepared$rows %>%
    filter(mmm_is_phase(.data$Phase, phase)) %>%
    left_join(anchors %>% select(".mmm_session" = ".mmm_session", "target_phase_block",
                                 "target_window_start", "target_window_end"),
              by = ".mmm_session") %>%
    mutate(
      elapsed_sec_in_window = as.numeric(difftime(.data$BinStart, .data$target_window_start, units = "secs")),
      target_slot = as.integer(.data$elapsed_sec_in_window %/% bin_size_sec) + 1L
    ) %>%
    filter(.data$elapsed_sec_in_window >= 0,
           .data$elapsed_sec_in_window < window_hours * 3600) %>%
    mutate(
      elapsed_hours_in_window = .data$elapsed_sec_in_window / 3600,
      TimeHours = .data$elapsed_hours_in_window,
      expected_slots = as.integer(window_hours * 3600 / bin_size_sec),
      target_cage_change = prepared$cage_change,
      target_phase = phase,
      window_hours = window_hours,
      bin_size_sec = bin_size_sec,
      window_definition = paste0(
        "fixed clock window: first ", phase, " block ",
        if (phase == "Active") "after cage change " else "following the first Active block of cage change ",
        prepared$cage_change, ", ",
        if (phase == "Active") "18:30 inclusive to 06:30 exclusive" else "06:30 inclusive to 18:30 exclusive"
      )
    ) %>%
    select(-any_of(c(".mmm_block", ".mmm_session"))) %>%
    arrange(.data$AnimalNum, .data$target_slot)

  if (nrow(out) == 0L) {
    stop("No rows selected for ", phase, " window of cage change ", prepared$cage_change, call. = FALSE)
  }
  out
}

#' Select EVERY block of one phase inside a cage-change period.
#'
#' For Active this is every Active block. For Inactive it is every Inactive
#' block that FOLLOWS the first Active block, so a pre-response Inactive block
#' is never counted as block 1.
#'
#' @return Selected rows plus PhaseBlock (1-based within session),
#'   phase_block_index, block_window_start/end, target_slot,
#'   TimeWithinPhaseHours, expected_slots.
mmm_select_cc_phase_blocks <- function(dat,
                                       bin_size_sec,
                                       cage_change = NULL,
                                       phase = c("Active", "Inactive"),
                                       window_hours = MMM_PHASE_WINDOW_HOURS,
                                       session_col = NULL) {
  phase <- match.arg(phase)
  stopifnot(is.data.frame(dat), is.finite(bin_size_sec), bin_size_sec > 0)
  prepared <- .mmm_cc_rows(dat, cage_change = cage_change, session_col = session_col)
  anchors <- mmm_phase_anchor_blocks(prepared)

  ph_rows <- prepared$rows %>%
    filter(mmm_is_phase(.data$Phase, phase)) %>%
    left_join(anchors, by = ".mmm_session")

  # Inactive blocks must start at or after the post-active anchor.
  if (phase == "Inactive") {
    ph_rows <- ph_rows %>% filter(.data$.mmm_block >= .data$inactive_block)
  } else {
    ph_rows <- ph_rows %>% filter(.data$.mmm_block >= .data$active_block)
  }
  if (nrow(ph_rows) == 0L) {
    stop("No ", phase, " blocks found for cage change ", prepared$cage_change, call. = FALSE)
  }

  block_index <- ph_rows %>%
    distinct(.data$.mmm_session, .data$.mmm_block) %>%
    arrange(.data$.mmm_session, .data$.mmm_block) %>%
    group_by(.data$.mmm_session) %>%
    mutate(PhaseBlock = row_number(), n_phase_blocks_in_session = n()) %>%
    ungroup()

  ph_rows %>%
    left_join(block_index, by = c(".mmm_session", ".mmm_block")) %>%
    mutate(
      phase_block_index = .data$.mmm_block,
      block_window_start = mmm_block_window_start(.data$.mmm_block),
      block_window_end = .data$block_window_start + window_hours * 3600,
      elapsed_sec_in_block = as.numeric(difftime(.data$BinStart, .data$block_window_start, units = "secs")),
      target_slot = as.integer(.data$elapsed_sec_in_block %/% bin_size_sec) + 1L
    ) %>%
    filter(.data$elapsed_sec_in_block >= 0,
           .data$elapsed_sec_in_block < window_hours * 3600) %>%
    mutate(
      TimeWithinPhaseHours = .data$elapsed_sec_in_block / 3600,
      expected_slots = as.integer(window_hours * 3600 / bin_size_sec),
      target_cage_change = prepared$cage_change,
      target_phase = phase,
      window_hours = window_hours,
      bin_size_sec = bin_size_sec,
      window_definition = paste0("every ", phase, " block within cage change ",
                                 prepared$cage_change, " at or after the first Active block")
    ) %>%
    select(-any_of(c(".mmm_block", ".mmm_session", "active_block", "inactive_block"))) %>%
    arrange(.data$AnimalNum, .data$PhaseBlock, .data$target_slot)
}

#' Phase-window QC by animal x block, including the zero-inflation summaries
#' that matter for Inactive-phase Movement.
mmm_phase_window_qc <- function(selected, block_cols = character(),
                                start_col = "target_window_start",
                                end_col = "target_window_end",
                                value_col = "Movement") {
  if (nrow(selected) == 0L) return(tibble())
  expected <- selected$expected_slots[1]
  group_cols <- c("AnimalNum", "Group", "Sex", "Batch", block_cols)
  selected %>%
    group_by(across(any_of(group_cols))) %>%
    summarise(
      scheduled_block_start = first(.data[[start_col]]),
      scheduled_block_end = first(.data[[end_col]]),
      expected_bins = expected,
      observed_bins = n_distinct(.data$target_slot),
      leading_gaps = min(.data$target_slot) - 1L,
      trailing_gaps = expected - max(.data$target_slot),
      interior_gaps = (max(.data$target_slot) - min(.data$target_slot) + 1L) - n_distinct(.data$target_slot),
      zero_fraction = mean(.data[[value_col]] == 0, na.rm = TRUE),
      mean_value = mean(.data[[value_col]], na.rm = TRUE),
      median_value = stats::median(.data[[value_col]], na.rm = TRUE),
      var_value = stats::var(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      coverage = .data$observed_bins / .data$expected_bins,
      missing_slot_fraction = 1 - .data$coverage,
      window_complete = .data$observed_bins == .data$expected_bins
    )
}
