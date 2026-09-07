# ================================================================
# Acute Active-window selectors (generalized from the canonical first night)
# MMMSociability
# ================================================================
# Two selectors, both clock-anchored, both built on the SAME phase-block
# arithmetic that Functions/first_night_window_helpers.R uses:
#
#   mmm_select_acute_active_window()
#       first Active block after a SPECIFIED cage change,
#       18:30 inclusive -> 06:30 exclusive, exactly 12 h.
#       With cage_change = the first cage change this is row-for-row identical
#       to mmm_select_first_night_window(); Testing/tests/
#       test_acute_active_window_parity.R asserts that against the real
#       production definition.
#
#   mmm_select_cc_active_nights()
#       EVERY Active block inside one cage-change period, each one kept as its
#       own block with an explicit ActiveNight index and its own 0-12 h clock
#       axis. Nights are never overlaid and never concatenated.
#
# WHY THIS FILE EXISTS
# The archived GAMM (Analysis/_archive/04_gamm_...R) selected all Active rows
# of CC1, reset every night to 0-12 h via a per-phase-block minimum, and put no
# night term in the model, so four nights were superimposed on one axis. These
# selectors retain block identity so that mistake is not reproducible.
#
# CONTRACTS
#   * Stage 09 is not modified, and first_night_window_helpers.R is not
#     modified. This file is purely additive.
#   * Time is always elapsed clock time from a SCHEDULED block start, never a
#     row index, so missing bins leave real gaps on a trajectory axis.
#   * A window is never extended into a later Active block.
#   * Missing observations stay missing; nothing is back-filled.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

for (.needed_helper in c("phase_classification_helpers.R", "animalpos_preprocessing_helpers.R")) {
  .probe <- switch(.needed_helper,
    "phase_classification_helpers.R" = "mmm_is_active_phase",
    "animalpos_preprocessing_helpers.R" = "animalpos_phase_block_index")
  if (!exists(.probe, mode = "function", inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
      source_mmm_helper(.needed_helper)
    } else {
      stop("acute_active_window_helpers.R requires ", .needed_helper, call. = FALSE)
    }
  }
}
rm(.needed_helper, .probe)

MMM_ACUTE_WINDOW_HOURS <- 12

#' Label of the cage change matching `target`, numeric-aware.
#'
#' Mirrors mmm_first_cage_change() so "CC2" is matched by 2, "cc2" or "CC2".
mmm_match_cage_change <- function(x, target = NULL) {
  ux <- unique(as.character(x))
  ux <- ux[!is.na(ux)]
  if (length(ux) == 0L) stop("No cage-change labels present.", call. = FALSE)
  if (is.null(target)) {
    cc_num <- suppressWarnings(as.numeric(str_extract(ux, "[0-9]+")))
    return(if (any(is.finite(cc_num))) ux[which.min(ifelse(is.finite(cc_num), cc_num, Inf))] else sort(ux)[1])
  }
  target_chr <- as.character(target)
  hit <- ux[ux == target_chr]
  if (length(hit) == 1L) return(hit)
  t_num <- suppressWarnings(as.numeric(str_extract(target_chr, "[0-9]+")))
  u_num <- suppressWarnings(as.numeric(str_extract(ux, "[0-9]+")))
  hit <- ux[is.finite(u_num) & is.finite(t_num) & u_num == t_num]
  if (length(hit) == 1L) return(hit)
  stop("Cage change '", target_chr, "' is not uniquely present. Available: ",
       paste(ux, collapse = ", "), call. = FALSE)
}

#' Exact-Active rows of ONE cage change, with the global phase-block index.
#'
#' The block index comes from animalpos_phase_block_index(), i.e. from the
#' experimental clock, so consecutive Active nights get distinct block numbers
#' even after Inactive rows are dropped.
.mmm_cc_active_rows <- function(dat, cage_change = NULL, session_col = NULL) {
  for (needed in c("AnimalNum", "Phase", "CageChange", "BinStart")) {
    if (!needed %in% names(dat)) {
      stop("acute Active-window selection requires column: ", needed, call. = FALSE)
    }
  }
  mmm_assert_phase_classifiable(dat$Phase, "acute Active-window input Phase column")
  cc <- mmm_match_cage_change(dat$CageChange, cage_change)

  act <- dat %>% filter(as.character(.data$CageChange) == cc, mmm_is_active_phase(.data$Phase))
  if (nrow(act) == 0L) {
    stop("No exact-Active rows found for cage change ", cc, ".", call. = FALSE)
  }
  session_col <- session_col %||% if ("SourceFile" %in% names(act)) "SourceFile" else "Batch"
  if (!session_col %in% names(act)) {
    stop("acute Active-window selection needs a session column; tried SourceFile and Batch.", call. = FALSE)
  }
  list(
    act = act %>% mutate(
      .mmm_session = as.character(.data[[session_col]]),
      .mmm_block = animalpos_phase_block_index(.data$BinStart)
    ),
    cage_change = cc,
    session_col = session_col
  )
}

#' Scheduled clock start of a phase block index.
mmm_block_window_start <- function(block_index) {
  as.POSIXct(block_index * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
             origin = "1970-01-01", tz = "UTC")
}

#' Select the acute (first) Active block after a specified cage change.
#'
#' Generalization of mmm_select_first_night_window(): identical arithmetic,
#' with the cage change as a parameter instead of hard-wired to the first one.
#' The anchor is per SESSION, not per animal, because each batch ran on its own
#' calendar dates; an animal whose first read is late is scored against the
#' scheduled 18:30 start so partial coverage is measured, never hidden.
#'
#' @return Selected rows plus target_cage_change, target_phase_block,
#'   target_window_start/end, elapsed_sec_in_window, target_slot,
#'   elapsed_hours_in_window, TimeHours, expected_slots.
mmm_select_acute_active_window <- function(dat,
                                           bin_size_sec,
                                           cage_change = NULL,
                                           window_hours = MMM_ACUTE_WINDOW_HOURS,
                                           session_col = NULL) {
  stopifnot(is.data.frame(dat), is.finite(bin_size_sec), bin_size_sec > 0)
  prepared <- .mmm_cc_active_rows(dat, cage_change = cage_change, session_col = session_col)
  act <- prepared$act

  anchors <- act %>%
    group_by(.data$.mmm_session) %>%
    summarise(target_phase_block = min(.data$.mmm_block, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      target_window_start = mmm_block_window_start(.data$target_phase_block),
      target_window_end = .data$target_window_start + window_hours * 3600
    )

  act %>%
    left_join(anchors, by = ".mmm_session") %>%
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
      window_hours = window_hours,
      bin_size_sec = bin_size_sec,
      window_definition = paste0(
        "fixed clock window: first Active phase block after cage change ",
        prepared$cage_change, ", 18:30 inclusive to 06:30 exclusive"
      )
    ) %>%
    select(-any_of(c(".mmm_block", ".mmm_session"))) %>%
    arrange(.data$AnimalNum, .data$target_slot)
}

#' Select EVERY Active block inside one cage-change period.
#'
#' Each Active block keeps its own identity (`ActiveNight`, 1-based within the
#' session) and its own scheduled 0-12 h axis (`TimeWithinActiveHours`). Nights
#' are neither overlaid nor concatenated: block N of a session always carries
#' ActiveNight == N, and TimeWithinActiveHours is elapsed clock time from THAT
#' block's own scheduled 18:30 start.
#'
#' @return Selected rows plus ActiveNight, active_phase_block,
#'   block_window_start/end, elapsed_sec_in_block, target_slot,
#'   TimeWithinActiveHours, expected_slots, n_active_nights_in_session.
mmm_select_cc_active_nights <- function(dat,
                                        bin_size_sec,
                                        cage_change = NULL,
                                        window_hours = MMM_ACUTE_WINDOW_HOURS,
                                        session_col = NULL) {
  stopifnot(is.data.frame(dat), is.finite(bin_size_sec), bin_size_sec > 0)
  prepared <- .mmm_cc_active_rows(dat, cage_change = cage_change, session_col = session_col)
  act <- prepared$act

  # ActiveNight is the rank of the phase block WITHIN the session, so it is a
  # property of the experimental clock, not of any animal's data coverage.
  night_index <- act %>%
    distinct(.data$.mmm_session, .data$.mmm_block) %>%
    arrange(.data$.mmm_session, .data$.mmm_block) %>%
    group_by(.data$.mmm_session) %>%
    mutate(
      ActiveNight = row_number(),
      n_active_nights_in_session = n()
    ) %>%
    ungroup()

  act %>%
    left_join(night_index, by = c(".mmm_session", ".mmm_block")) %>%
    mutate(
      active_phase_block = .data$.mmm_block,
      block_window_start = mmm_block_window_start(.data$.mmm_block),
      block_window_end = .data$block_window_start + window_hours * 3600,
      elapsed_sec_in_block = as.numeric(difftime(.data$BinStart, .data$block_window_start, units = "secs")),
      target_slot = as.integer(.data$elapsed_sec_in_block %/% bin_size_sec) + 1L
    ) %>%
    filter(.data$elapsed_sec_in_block >= 0,
           .data$elapsed_sec_in_block < window_hours * 3600) %>%
    mutate(
      TimeWithinActiveHours = .data$elapsed_sec_in_block / 3600,
      expected_slots = as.integer(window_hours * 3600 / bin_size_sec),
      target_cage_change = prepared$cage_change,
      window_hours = window_hours,
      bin_size_sec = bin_size_sec,
      window_definition = paste0(
        "every Active phase block within cage change ", prepared$cage_change,
        "; each block on its own 18:30-inclusive to 06:30-exclusive 12 h clock axis"
      )
    ) %>%
    select(-any_of(c(".mmm_block", ".mmm_session"))) %>%
    arrange(.data$AnimalNum, .data$ActiveNight, .data$target_slot)
}

#' Per-animal x block window QC.
#'
#' Coverage is measured against the fixed expected slot count. Leading,
#' interior and trailing gaps are reported separately because a late first read
#' is a different phenomenon from a mid-window dropout.
#'
#' @param selected Output of either selector.
#' @param block_cols Grouping columns identifying one window, e.g.
#'   "ActiveNight" for the multi-night selector or "target_cage_change" for the
#'   acute selector.
#' @param start_col,end_col Scheduled window bounds to report.
mmm_active_window_qc <- function(selected,
                                 block_cols = character(),
                                 start_col = "target_window_start",
                                 end_col = "target_window_end") {
  if (nrow(selected) == 0L) return(tibble())
  expected <- selected$expected_slots[1]
  group_cols <- c("AnimalNum", "Group", "Sex", "Batch", block_cols)
  selected %>%
    group_by(across(any_of(group_cols))) %>%
    summarise(
      scheduled_block_start = first(.data[[start_col]]),
      scheduled_block_end = first(.data[[end_col]]),
      window_hours = first(.data$window_hours),
      bin_size_sec = first(.data$bin_size_sec),
      expected_bins = expected,
      observed_bins = n_distinct(.data$target_slot),
      first_target_slot = min(.data$target_slot),
      last_target_slot = max(.data$target_slot),
      first_bin_start = min(.data$BinStart),
      last_bin_start = max(.data$BinStart),
      leading_gaps = min(.data$target_slot) - 1L,
      trailing_gaps = expected - max(.data$target_slot),
      interior_gaps = (max(.data$target_slot) - min(.data$target_slot) + 1L) - n_distinct(.data$target_slot),
      max_internal_gap_slots = {
        s <- sort(unique(.data$target_slot))
        if (length(s) < 2L) 0L else as.integer(max(diff(s)) - 1L)
      },
      .groups = "drop"
    ) %>%
    mutate(
      coverage = .data$observed_bins / .data$expected_bins,
      window_complete = .data$observed_bins == .data$expected_bins,
      no_interior_missing = .data$interior_gaps == 0L,
      no_trailing_missing = .data$trailing_gaps == 0L
    )
}

#' AR1 restart vector for mgcv::bam(AR.start = ...).
#'
#' TRUE marks the first row of a new independent residual sequence. Sequences
#' restart at a new animal, at a new value of ANY supplied block key, and at a
#' missing target slot, so an AR1 process is never bridged across an Inactive
#' phase, a cage-change boundary, a different Active night, or a data gap.
#'
#' The data MUST already be sorted by c("AnimalNum", block_cols, slot_col);
#' mmm_order_for_ar1() does that and is the intended companion.
mmm_build_ar_start <- function(dat, block_cols = character(), slot_col = "target_slot") {
  stopifnot(is.data.frame(dat), nrow(dat) > 0L, slot_col %in% names(dat))
  keys <- c("AnimalNum", block_cols)
  missing_keys <- setdiff(keys, names(dat))
  if (length(missing_keys)) {
    stop("mmm_build_ar_start() missing key column(s): ", paste(missing_keys, collapse = ", "), call. = FALSE)
  }
  key_chr <- do.call(paste, c(lapply(keys, function(k) as.character(dat[[k]])), sep = "\r"))
  slot <- as.integer(dat[[slot_col]])
  new_block <- c(TRUE, key_chr[-1] != key_chr[-length(key_chr)])
  slot_gap <- c(FALSE, diff(slot) != 1L)
  new_block | slot_gap
}

#' Canonical ordering required before mmm_build_ar_start().
mmm_order_for_ar1 <- function(dat, block_cols = character(), slot_col = "target_slot") {
  dat %>% arrange(across(all_of(c("AnimalNum", block_cols, slot_col))))
}

#' Evidence table proving how the AR1 sequences were constructed.
#'
#' Saved alongside every model so the correlation boundaries are auditable
#' without re-running anything.
mmm_ar1_sequence_proof <- function(dat, ar_start, block_cols = character(), slot_col = "target_slot") {
  seq_len_tbl <- as.integer(table(cumsum(ar_start)))
  keys <- c("AnimalNum", block_cols)
  n_blocks <- n_distinct(do.call(paste, c(lapply(keys, function(k) as.character(dat[[k]])), sep = "\r")))
  tibble(
    n_rows = nrow(dat),
    n_animals = n_distinct(dat$AnimalNum),
    block_keys = paste(keys, collapse = " + "),
    n_blocks = n_blocks,
    n_sequences = length(seq_len_tbl),
    # Every block starts one sequence; anything beyond that is a within-block
    # restart forced by a missing target slot.
    n_gap_restarts = length(seq_len_tbl) - n_blocks,
    min_seq_len = min(seq_len_tbl),
    median_seq_len = stats::median(seq_len_tbl),
    max_seq_len = max(seq_len_tbl),
    slot_col = slot_col,
    restarts_on = paste(c("new AnimalNum", paste0("new ", block_cols), "missing slot gap"), collapse = "; "),
    bridges_inactive_phase = FALSE,
    bridges_cage_change = !any(c("target_cage_change", "CageChange") %in% block_cols),
    bridges_active_night = !("ActiveNight" %in% block_cols)
  )
}
