# ================================================================
# AnimalPos preprocessing helpers: measurement events vs aggregation boundaries
# MMMSociability
# ================================================================
# The historical preprocessing path represented phase/day/half-hour boundaries
# by INSERTING synthetic rows that copied an animal's most recent PositionID to
# the boundary timestamp (compute_phase_transitions / compute_day_transitions /
# add_half_hour_transitions). Those synthetic rows accounted for 11.91% of the
# preprocessed data, carried a copied PositionID, and were indistinguishable
# from genuine detector-derived rows.
#
# A phase/day/bin boundary is NOT a position observation. These helpers replace
# the row-insertion approach with:
#   * deterministic phase metadata computed from the timestamp alone; and
#   * boundary tables used during interval aggregation (see Stage 01).
#
# Nothing here inserts, copies, or synthesises a PositionID.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Active phase runs 18:30 -> 06:30; Inactive runs 06:30 -> 18:30.
ANIMALPOS_ACTIVE_START_SEC   <- 18.5 * 3600  # 18:30
ANIMALPOS_INACTIVE_START_SEC <-  6.5 * 3600  # 06:30
ANIMALPOS_PHASE_LENGTH_SEC   <- 12 * 3600

#' Parse AnimalPos raw DateTime preserving fractional seconds.
#'
#' Raw exports carry millisecond precision ("28.10.2022 15:44:45.508"). The
#' historical parser used format "%d.%m.%Y %H:%M:%S", which silently discarded
#' the fractional part and collapsed sub-second-separated genuine reads onto the
#' same clock second (495 such collisions in the historical output). "%OS"
#' retains the fraction.
parse_animalpos_datetime <- function(x) {
  as.POSIXct(as.character(x), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
}

#' Phase-block index for a timestamp.
#'
#' Phase membership and phase-block identity are pure functions of wall-clock
#' time, so they never require a row to exist at the boundary. Blocks are
#' indexed on a 12 h grid anchored at 06:30 UTC: even blocks are Inactive
#' (06:30->18:30), odd blocks are Active (18:30->06:30).
animalpos_phase_block_index <- function(datetime) {
  shifted <- as.numeric(datetime) - ANIMALPOS_INACTIVE_START_SEC
  floor(shifted / ANIMALPOS_PHASE_LENGTH_SEC)
}

animalpos_phase_from_block <- function(block_index) {
  ifelse(block_index %% 2 == 0, "Inactive", "Active")
}

#' Phase label directly from a timestamp (18:30-06:30 = Active).
animalpos_phase_label <- function(datetime) {
  animalpos_phase_from_block(animalpos_phase_block_index(datetime))
}

#' Start/end instants of the phase block containing `datetime`.
animalpos_phase_block_bounds <- function(datetime) {
  b <- animalpos_phase_block_index(datetime)
  start <- as.POSIXct(b * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
                      origin = "1970-01-01", tz = "UTC")
  tibble(block_index = b, block_start = start,
         block_end = start + ANIMALPOS_PHASE_LENGTH_SEC,
         Phase = animalpos_phase_from_block(b))
}

#' Deterministic ConsecActive / ConsecInactive phase counters.
#'
#' Reproduces the historical count_phases() numbering without needing a row at
#' each boundary. The historical implementation walked the globally
#' time-sorted rows and incremented a counter whenever consecutive ROWS changed
#' Phase, which only worked because synthetic boundary rows guaranteed a row at
#' every transition. Here the counters are derived from the contiguous sequence
#' of 12 h phase blocks spanned by the session, so a block containing no rows
#' for a given animal still occupies its place in the numbering.
#'
#' @param datetime POSIXct vector (a single session's rows).
#' @return tibble(Phase, PhaseBlockIndex, ConsecActive, ConsecInactive)
animalpos_phase_counters <- function(datetime) {
  stopifnot(length(datetime) > 0)
  b <- animalpos_phase_block_index(datetime)
  # Contiguous block sequence across the session span, so numbering does not
  # depend on which blocks happen to contain rows.
  all_blocks <- seq(min(b), max(b))
  ph <- animalpos_phase_from_block(all_blocks)
  ca <- cumsum(ph == "Active")
  ci <- cumsum(ph == "Inactive")
  lut <- tibble(
    PhaseBlockIndex = all_blocks,
    Phase = ph,
    ConsecActive = ifelse(ph == "Active", ca, 0L),
    ConsecInactive = ifelse(ph == "Inactive", ci, 0L)
  )
  tibble(PhaseBlockIndex = b) %>% left_join(lut, by = "PhaseBlockIndex")
}

#' Aggregation-boundary instants inside [from, to].
#'
#' Used by Stage 01 to split state intervals at boundaries mathematically
#' instead of materialising a synthetic position row at each boundary.
animalpos_boundary_instants <- function(from, to, period_sec) {
  stopifnot(period_sec > 0)
  a <- as.numeric(from); b <- as.numeric(to)
  empty <- as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC")
  if (!is.finite(a) || !is.finite(b) || b <= a) return(empty)
  # Generate candidates then filter with exact comparisons. Epsilon nudges are
  # unusable here: at epoch magnitudes (~1.7e9) a double cannot represent a
  # 1e-9 offset, so such a nudge is silently a no-op.
  first <- floor(a / period_sec) * period_sec
  cuts <- seq(first, b + period_sec, by = period_sec)
  cuts <- cuts[cuts > a & cuts < b]
  if (length(cuts) == 0) return(empty)
  as.POSIXct(cuts, origin = "1970-01-01", tz = "UTC")
}

#' Phase-boundary instants strictly inside (from, to).
animalpos_phase_boundary_instants <- function(from, to) {
  a <- as.numeric(from); b <- as.numeric(to)
  empty <- as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC")
  if (!is.finite(a) || !is.finite(b) || b <= a) return(empty)
  P <- ANIMALPOS_PHASE_LENGTH_SEC
  first <- floor((a - ANIMALPOS_INACTIVE_START_SEC) / P) * P + ANIMALPOS_INACTIVE_START_SEC
  cuts <- seq(first, b + P, by = P)
  cuts <- cuts[cuts > a & cuts < b]
  if (length(cuts) == 0) return(empty)
  as.POSIXct(cuts, origin = "1970-01-01", tz = "UTC")
}

#' Contiguous phase-block lookup table spanning a session.
#'
#' Built once per session from the full timestamp range so that block numbering
#' is identical no matter which subset of rows or interval pieces it is later
#' applied to. This is what keeps phase metadata consistent after intervals are
#' split at boundaries.
animalpos_phase_block_lut <- function(datetime) {
  b <- animalpos_phase_block_index(datetime)
  all_blocks <- seq(min(b, na.rm = TRUE), max(b, na.rm = TRUE))
  ph <- animalpos_phase_from_block(all_blocks)
  tibble(
    PhaseBlockIndex = all_blocks,
    Phase = ph,
    ConsecActive = ifelse(ph == "Active", cumsum(ph == "Active"), 0L),
    ConsecInactive = ifelse(ph == "Inactive", cumsum(ph == "Inactive"), 0L)
  )
}

#' Apply a session phase-block LUT to arbitrary timestamps.
animalpos_apply_phase_lut <- function(datetime, lut) {
  tibble(PhaseBlockIndex = animalpos_phase_block_index(datetime)) %>%
    left_join(lut, by = "PhaseBlockIndex")
}

#' Vectorised equivalent of find_id().
#'
#' find_id() snapped one coordinate pair per call inside a rowwise() pipeline.
#' The snapping rule is reproduced exactly: y < 116 -> 0 else 116; x < 100 -> 0,
#' < 200 -> 100, < 300 -> 200, >= 300 -> 300.
animalpos_position_id <- function(x_pos, y_pos) {
  x <- suppressWarnings(as.numeric(x_pos)); y <- suppressWarnings(as.numeric(y_pos))
  yy <- ifelse(y < 116, 0, 116)
  xx <- ifelse(x < 100, 0, ifelse(x < 200, 100, ifelse(x < 300, 200, 300)))
  ax <- c(0, 100, 200, 300, 0, 100, 200, 300)
  ay <- c(0, 0, 0, 0, 116, 116, 116, 116)
  out <- match(paste(xx, yy), paste(ax, ay))
  out[is.na(x) | is.na(y)] <- NA_integer_
  as.integer(out)
}


#' Half-hours elapsed on the session clock grid.
#'
#' The legacy `count_half_hours_elapsed()` anchored on `min(DateTime)` computed
#' separately for every animal x system pair. That was only self-consistent
#' because a synthetic boundary row at the first half-hour mark gave every pair
#' the same anchor; with synthetic rows removed, each animal's first genuine
#' read differs and the same wall-clock instant would receive different indices
#' for different animals. This version anchors once per session, on the phase
#' block containing the earliest observation, so the index is a property of the
#' clock rather than of any animal's detection history.
animalpos_half_hours_elapsed <- function(datetime) {
  if (length(datetime) == 0) return(numeric(0))
  anchor <- animalpos_phase_block_bounds(min(datetime, na.rm = TRUE))$block_start
  floor(as.numeric(difftime(datetime, anchor, units = "mins")) / 30)
}

#' Preprocess one AnimalPos session WITHOUT inserting synthetic position rows.
#'
#' Production replacement for preprocess_file(). Differences, all deliberate:
#'   * fractional seconds retained (parse_animalpos_datetime);
#'   * PositionID snapped vectorised rather than rowwise;
#'   * Phase / ConsecActive / ConsecInactive derived from the timestamp via
#'     animalpos_phase_counters() instead of walking row-to-row transitions,
#'     so no boundary row is required;
#'   * compute_phase_transitions / compute_day_transitions /
#'     add_half_hour_transitions are NOT called -- no copied PositionID rows;
#'   * explicit provenance columns are emitted.
#' remove_phases() and count_half_hours_elapsed() are reused unchanged so epoch
#' inclusion semantics are preserved.
preprocess_animalpos_file <- function(batch, change, excl_animals,
                                      raw_dir, output_dir,
                                      remove_phases_fn = remove_phases,
                                      write_output = TRUE) {
  filename <- paste0("E9_SIS_", batch, "_", change, "_AnimalPos")
  csv_path <- file.path(raw_dir, batch, paste0(filename, ".csv"))
  if (!file.exists(csv_path)) {
    warning("File ", csv_path, " does not exist. Skipping.", call. = FALSE)
    return(invisible(NULL))
  }

  data <- suppressWarnings(readr::read_delim(csv_path, delim = ";", show_col_types = FALSE, progress = FALSE))
  n_raw_rows <- nrow(data)

  data <- data %>%
    dplyr::mutate(DateTime = parse_animalpos_datetime(DateTime)) %>%
    dplyr::select(-dplyr::any_of(c("RFID", "AM", "zPos"))) %>%
    tidyr::separate(Animal, into = c("AnimalID", "System"), sep = "[-_]", extra = "merge", fill = "right") %>%
    dplyr::mutate(PositionID = animalpos_position_id(xPos, yPos)) %>%
    dplyr::select(DateTime, AnimalID, System, PositionID) %>%
    dplyr::filter(!is.na(DateTime), !AnimalID %in% excl_animals) %>%
    dplyr::arrange(DateTime, AnimalID, System) %>%
    dplyr::mutate(CageChange = change, Batch = batch)
  n_after_exclusions <- nrow(data)

  if (nrow(data) == 0) {
    warning("No usable rows for ", filename, call. = FALSE)
    return(invisible(NULL))
  }

  # Deterministic phase metadata -- no boundary rows involved.
  counters <- animalpos_phase_counters(data$DateTime)
  data <- data %>%
    dplyr::mutate(Phase = counters$Phase,
                  ConsecActive = as.numeric(counters$ConsecActive),
                  ConsecInactive = as.numeric(counters$ConsecInactive))

  n_before_remove_phases <- nrow(data)
  data <- remove_phases_fn(data)
  n_after_remove_phases <- nrow(data)

  data <- data %>%
    dplyr::mutate(HalfHoursElapsed = animalpos_half_hours_elapsed(DateTime)) %>%
    dplyr::mutate(is_synthetic_preprocessing_row = FALSE,
                  synthetic_row_source = NA_character_) %>%
    dplyr::arrange(DateTime, AnimalID, System)

  if (isTRUE(write_output)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    out_path <- file.path(output_dir, paste0(filename, "_preprocessed.csv"))
    # Keep millisecond precision in the written file.
    old_opt <- options(digits.secs = 3); on.exit(options(old_opt), add = TRUE)
    readr::write_csv(data, out_path)
    message("Wrote ", out_path, " (", nrow(data), " genuine rows)")
  }

  invisible(list(
    data = data, batch = batch, cage_change = change,
    n_raw_rows = n_raw_rows, n_after_exclusions = n_after_exclusions,
    n_before_remove_phases = n_before_remove_phases,
    n_after_remove_phases = n_after_remove_phases,
    n_synthetic_rows = 0L
  ))
}

#' Split state intervals at an arbitrary set of boundary instants.
#'
#' Generic interval/boundary intersection: one implementation serves 10 s,
#' 1 min, 5 min, 10 min, 30 min, phase and day boundaries. A state interval
#' spanning a boundary is divided into abutting pieces whose durations sum to
#' the original, with no synthetic position event created at the boundary.
#'
#' @param intervals data frame with `IntervalStart`, `IntervalEnd` (POSIXct).
#' @param period_sec optional fixed-width period to split on.
#' @param split_phase split at 06:30/18:30 phase boundaries.
#' @param split_day split at midnight.
#' Reference (loop) implementation, retained so the vectorised production
#' path can be cross-validated against it in the test suite. Do not call
#' from analysis code: it is O(n) in R-level iterations.
animalpos_split_intervals_reference <- function(intervals, period_sec = NULL,
                                               split_phase = FALSE, split_day = FALSE) {
  if (nrow(intervals) == 0) return(intervals)
  starts <- as.numeric(intervals$IntervalStart)
  ends   <- as.numeric(intervals$IntervalEnd)

  cut_list <- lapply(seq_len(nrow(intervals)), function(i) {
    cuts <- numeric(0)
    if (!is.null(period_sec)) {
      cuts <- c(cuts, as.numeric(animalpos_boundary_instants(intervals$IntervalStart[i], intervals$IntervalEnd[i], period_sec)))
    }
    if (isTRUE(split_phase)) {
      cuts <- c(cuts, as.numeric(animalpos_phase_boundary_instants(intervals$IntervalStart[i], intervals$IntervalEnd[i])))
    }
    if (isTRUE(split_day)) {
      cuts <- c(cuts, as.numeric(animalpos_boundary_instants(intervals$IntervalStart[i], intervals$IntervalEnd[i], 86400)))
    }
    sort(unique(cuts))
  })

  n_pieces <- vapply(cut_list, length, integer(1)) + 1L
  row_idx <- rep(seq_len(nrow(intervals)), n_pieces)
  out <- intervals[row_idx, , drop = FALSE]
  new_start <- numeric(length(row_idx)); new_end <- numeric(length(row_idx))
  pos <- 1L
  for (i in seq_len(nrow(intervals))) {
    edges <- c(starts[i], cut_list[[i]], ends[i])
    k <- length(edges) - 1L
    new_start[pos:(pos + k - 1L)] <- edges[1:k]
    new_end[pos:(pos + k - 1L)]   <- edges[2:(k + 1L)]
    pos <- pos + k
  }
  out$IntervalStart <- as.POSIXct(new_start, origin = "1970-01-01", tz = "UTC")
  out$IntervalEnd   <- as.POSIXct(new_end,   origin = "1970-01-01", tz = "UTC")
  out$DurationSec   <- new_end - new_start
  out[out$DurationSec > 0, , drop = FALSE]
}
#' Split half-open intervals on one periodic boundary grid.
#'
#' The grid is `offset_sec + k * period_sec` for integer k. A cut is emitted only
#' when it lies STRICTLY inside an interval, so an interval that already ends on
#' a boundary is left whole and no zero-length piece is produced. Duration is
#' conserved exactly: the pieces of an interval partition it.
#'
#' This is the single interval/boundary intersection used by the pipeline --
#' time-bin splitting, phase splitting and day splitting are the same operation
#' with different grids.
#'
#' @param intervals data frame with POSIXct IntervalStart / IntervalEnd.
#' @param period_sec grid period in seconds.
#' @param offset_sec grid phase offset in seconds since the epoch.
#' @return `intervals` with rows duplicated per piece and IntervalStart,
#'   IntervalEnd and DurationSec rewritten.
animalpos_split_intervals_one_grid <- function(intervals, period_sec, offset_sec = 0) {
  n <- nrow(intervals)
  if (n == 0 || is.null(period_sec)) return(intervals)
  a <- as.numeric(intervals$IntervalStart)
  b <- as.numeric(intervals$IntervalEnd)

  # k0 = index of the first grid point strictly after a.
  k0 <- floor((a - offset_sec) / period_sec) + 1
  # Number of grid points strictly inside (a, b).
  n_cuts <- pmax(0, ceiling((b - offset_sec) / period_sec) - k0)
  n_cuts[!is.finite(n_cuts)] <- 0
  n_pieces <- as.integer(n_cuts) + 1L

  row_idx <- rep.int(seq_len(n), n_pieces)
  j       <- sequence(n_pieces) - 1L
  k0r     <- rep.int(k0, n_pieces)
  ar      <- rep.int(a, n_pieces)
  br      <- rep.int(b, n_pieces)
  ncr     <- rep.int(as.integer(n_cuts), n_pieces)

  new_start <- ifelse(j == 0L,   ar, offset_sec + (k0r + j - 1) * period_sec)
  new_end   <- ifelse(j == ncr,  br, offset_sec + (k0r + j)     * period_sec)

  out <- intervals[row_idx, , drop = FALSE]
  out$IntervalStart <- as.POSIXct(new_start, origin = "1970-01-01", tz = "UTC")
  out$IntervalEnd   <- as.POSIXct(new_end,   origin = "1970-01-01", tz = "UTC")
  out$DurationSec   <- new_end - new_start
  out[is.finite(out$DurationSec) & out$DurationSec > 0, , drop = FALSE]
}

#' Split intervals on any combination of the pipeline's boundary grids.
#'
#' Grids are applied one after another. That is equivalent to merging all cut
#' points first, because a piece produced by an earlier grid never spans one of
#' that grid's boundaries, and later grids only subdivide further.
animalpos_split_intervals <- function(intervals, period_sec = NULL,
                                     split_phase = FALSE, split_day = FALSE) {
  if (nrow(intervals) == 0) return(intervals)
  if (!is.null(period_sec)) {
    intervals <- animalpos_split_intervals_one_grid(intervals, period_sec, 0)
  }
  if (isTRUE(split_phase)) {
    intervals <- animalpos_split_intervals_one_grid(
      intervals, ANIMALPOS_PHASE_LENGTH_SEC, ANIMALPOS_INACTIVE_START_SEC)
  }
  if (isTRUE(split_day)) {
    intervals <- animalpos_split_intervals_one_grid(intervals, 86400, 0)
  }
  intervals
}

