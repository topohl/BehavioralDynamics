# ================================================================
# Raw-antenna data versions: the raw MultiMouseMonitor logs written in the schema of the preprocessed AnimalPos files
# MMMSociability
# ================================================================
# Pure definitions (needs dplyr/tibble attached, Functions/antenna_log_helpers.R, animalpos_preprocessing_helpers.R and
# canonical_animal_id()). The driver is Formatting/build_raw_antenna_data_versions.R; it writes only outside the live
# data root.
#
# A raw-antenna data version holds, for every vendor-preprocessed recording, a file with the same name and the same
# 12 columns, so Stages 01/02 and the Stage 29 event stream read it unchanged. Each row is a CHANGE of antenna reader
# of one animal (its first detection at a new reader), after the optional flip filter (min_flip_sec; 0 = none):
#   - span: the vendor file's phase blocks, from the start of the first (18:30) to the end of the last, clipped to the
#     end of the raw recording; the phase labels (Phase, ConsecActive, ConsecInactive) are copied block by block from
#     the vendor file and HalfHoursElapsed uses the shared helper, so both versions share one calendar;
#   - seed: every animal gets a row at the span start holding the reader of its last detection before the span,
#     marked is_synthetic_preprocessing_row = TRUE, synthetic_row_source = "raw_seed_last_detection_before_span";
#     the age of that detection is kept in seed_provenance.csv (a seed carried over hours is weaker evidence than a
#     fresh detection); an animal without any earlier detection starts at its first detection (seeded = FALSE);
#   - System = sys.<antenna board> (the cage that read the tag); AnimalID keeps the vendor file's spelling;
#   - times are written to the millisecond like the vendor files; changes that fall in the same millisecond keep the
#     later reader.
# Optional relocation rule (move_rule = rfid_move_rule(); version ge200): a detection becomes the animal's position only
# if its antenna lies >= 200 grid units from the position it holds, the vendor's emission threshold applied to the
# antennas. The rule runs from the animal's first detection in the recording, so the seed is the position held at the
# span start (synthetic_row_source = "raw_seed_held_position_at_span_start"); seed_detection_age_s stays the age of the
# last detection before the span, which lies within 200 units of the held position, and seed_emission_age_s is the age
# of the relocation that set it.
# ================================================================

ANTENNA_DV_COLUMNS <- c("DateTime", "AnimalID", "System", "PositionID", "CageChange", "Batch", "Phase", "ConsecActive",
                        "ConsecInactive", "HalfHoursElapsed", "is_synthetic_preprocessing_row", "synthetic_row_source")
ANTENNA_DV_SEED_SOURCE <- "raw_seed_last_detection_before_span"
ANTENNA_DV_SEED_SOURCE_HELD <- "raw_seed_held_position_at_span_start"

# The raw-antenna data versions (decided 2026-10-12/13). root: the parallel project root the builder writes the version
# into (noflip is built in RANT and copied, hash-checked, into RANU, the root it runs in).
ANTENNA_DV_VERSIONS <- list(
  flip2s = list(min_flip_sec = 2, move_rule = NULL, root = "RANT"),
  noflip = list(min_flip_sec = 0, move_rule = NULL, root = "RANT"),
  ge200 = list(min_flip_sec = 0, move_rule = rfid_move_rule(threshold = 200, inclusive = TRUE), root = "RANV"))

#' README.txt lines of a version folder (written joined by CRLF). Versions without a relocation rule keep the text
#' of their 2026-10-12 build byte for byte.
antenna_dv_readme_lines <- function(dir_name, build_date, spec, git_commit = NULL) {
  head <- c(sprintf("Exp9 RFID raw-antenna data version '%s' (built %s%s)", dir_name, build_date,
                    if (is.null(git_commit)) "" else paste0("; code ", substr(git_commit, 1, 7))),
            "",
            "The raw MultiMouseMonitor antenna logs in the schema of the vendor preprocessed AnimalPos files (same 24 file names,",
            "same 12 columns), so Stages 01/02 and the Stage 29 event stream read them unchanged. Built by",
            "Formatting/build_raw_antenna_data_versions.R with Functions/antenna_data_version.R and antenna_log_helpers.R.")
  tail <- c("Calendar: the vendor v2 file's phase blocks; Phase/ConsecActive/ConsecInactive copied per block; HalfHoursElapsed from",
            "the shared helper. Inputs: vendor v2 preprocessed files (roster, spelling, calendar), raw_data AnimalPos exports (tag map),",
            "raw logs. The repeated daylight-saving hour of B1 CC1 is dropped; empty software sessions are ignored (build_audit.csv).",
            "Immutable: a change needs a new version folder.")
  if (is.null(spec$move_rule)) {
    return(c(head,
      sprintf("Flip filter: %s.", if (spec$min_flip_sec > 0) sprintf("a run at one antenna shorter than %g s that returns to the antenna it came from is reassigned to it", spec$min_flip_sec) else "none (every antenna change counts)"),
      "Rows: one row per change of antenna reader per animal (its first detection at the new reader); System = sys.<antenna board>.",
      "Seeds: every animal has a row at the span start (18:30 of the cage-change day) with the reader of its last detection",
      "before it, marked is_synthetic_preprocessing_row = TRUE, synthetic_row_source = raw_seed_last_detection_before_span;",
      "seed_provenance.csv gives the age of that detection (seed_detection_age_s). No vendor position is ever used as a seed.",
      tail))
  }
  c(head,
    "Purpose: the vendor AnimalPos emission threshold applied to the raw detections, to separate the construct (relocations",
    "of two or more antenna columns) from the data source. The vendor records a position when the mean of the tag's reads in a",
    "500-ms window lies >= 200 grid units from its last recorded position (reconstructed exactly from the raw reads,",
    "2026-10-13); this version applies the same threshold to the antenna of each detection, without the averaging.",
    sprintf("Relocation rule: %s. Row changes (116 units) and moves to a neighbouring column (100 or 153.2 units) are never", rfid_move_rule_label(spec$move_rule)),
    "recorded. The rule runs from each animal's first detection in the recording; detections in the same millisecond keep the last.",
    sprintf("Flip filter: %s.", if (spec$min_flip_sec > 0) sprintf("%g s, before the relocation rule", spec$min_flip_sec) else "none (the threshold acts on every detection)"),
    "Rows: one row per relocation per animal (its first detection at least the threshold from the held position);",
    "System = sys.<antenna board>; consecutive rows of an animal are always at least the threshold apart.",
    "Seeds: every animal has a row at the span start (18:30 of the cage-change day) with the position it holds then, marked",
    "is_synthetic_preprocessing_row = TRUE, synthetic_row_source = raw_seed_held_position_at_span_start. seed_provenance.csv:",
    "seed_detection_age_s = age of the last detection before the span (within the threshold of the held position; the same",
    "evidence age as in the other raw versions), seed_emission_age_s = age of the relocation that set the held position,",
    "seed_detection_PositionID, seed_from_first_detection. No vendor position is ever used as a seed.",
    tail)
}

#' Vendor-file calendar: span bounds and the per-block phase labels to copy.
antenna_dv_parse_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")   # as Stage 29's mmm_evs_read_preprocessed
}

antenna_dv_vendor_calendar <- function(vendor) {
  t <- antenna_dv_parse_time(vendor$DateTime)
  if (anyNA(t)) stop("Unparseable vendor DateTime.", call. = FALSE)
  blocks <- vendor %>%
    dplyr::mutate(PhaseBlockIndex = animalpos_phase_block_index(t)) %>%
    dplyr::distinct(PhaseBlockIndex, Phase, ConsecActive, ConsecInactive)
  if (anyDuplicated(blocks$PhaseBlockIndex)) stop("Vendor phase labels are not unique per 12-h block.", call. = FALSE)
  b <- sort(blocks$PhaseBlockIndex)
  if (!isTRUE(all(diff(b) == 1))) {
    stop("Vendor phase blocks are not contiguous.", call. = FALSE)
  }
  list(blocks = blocks,
       span_start = min(blocks$PhaseBlockIndex) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
       span_end = (max(blocks$PhaseBlockIndex) + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC)
}

#' Format POSIX seconds like the vendor files: 2023-04-24T18:31:05.644Z (truncation-free rounding to the ms).
antenna_dv_format_time <- function(t) {
  ms <- round(t * 1000)
  sec <- floor(ms / 1000)
  paste0(format(as.POSIXct(sec, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
         sprintf(".%03dZ", as.integer(ms - sec * 1000)))
}

#' One animal's change stream at millisecond resolution from its carry-forward intervals (start, PositionID):
#' changes in the same millisecond keep the later reader, and repeats of the same reader are merged.
antenna_dv_change_rows <- function(start, pos) {
  if (!length(start)) return(data.frame(t_ms = numeric(), PositionID = integer()))
  t_ms <- round(start * 1000)
  keep <- c(t_ms[-1] != t_ms[-length(t_ms)], TRUE)          # last of each millisecond
  t_ms <- t_ms[keep]; pos <- pos[keep]
  keep <- c(TRUE, pos[-1] != pos[-length(pos)])               # drop repeats created by the step above
  data.frame(t_ms = t_ms[keep], PositionID = as.integer(pos[keep]))
}

#' Builds one recording. Returns list(rows (12 columns, vendor order), seeds, audit).
#' vendor: the vendor-preprocessed file of the recording (data frame as read); recording: rfid_read_recording();
#' tags: rfid_animalpos_tag_map(); min_flip_sec: 0 disables the flip filter; move_rule: NULL or rfid_move_rule().
antenna_dv_build_recording <- function(vendor, recording, tags, min_flip_sec, canon = canonical_animal_id, move_rule = NULL) {
  cal <- antenna_dv_vendor_calendar(vendor)
  recording_end <- max(recording$events$t + recording$events$dur_sec)
  span_end <- min(cal$span_end, recording_end)
  ids <- vendor %>% dplyr::distinct(AnimalID, System) %>% dplyr::mutate(AnimalNum = canon(AnimalID))
  if (anyDuplicated(ids$AnimalNum)) stop("An animal has two spellings or two System labels in the vendor file.", call. = FALSE)
  roster <- ids %>% dplyr::transmute(AnimalNum, System_label = System)
  built <- rfid_raw_occupancy_intervals(recording$events, roster, tags, cal$span_start, span_end, min_flip_sec, canon = canon,
                                        move_rule = move_rule)
  if (any(is.na(built$audit$Board))) {
    stop("No raw detections in the span for ", paste(built$audit$AnimalNum[is.na(built$audit$Board)], collapse = ", "), call. = FALSE)
  }
  rows <- built$intervals %>%
    dplyr::group_by(AnimalNum, Board) %>%
    dplyr::group_modify(~ antenna_dv_change_rows(.x$start, .x$PositionID)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ids %>% dplyr::select(AnimalNum, AnimalID), by = "AnimalNum") %>%
    dplyr::left_join(built$audit %>% dplyr::select(AnimalNum, seeded), by = "AnimalNum") %>%
    dplyr::mutate(t = t_ms / 1000,
                  seed_row = seeded & abs(t - cal$span_start) < 1e-9,
                  PhaseBlockIndex = animalpos_phase_block_index(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))) %>%
    dplyr::left_join(cal$blocks, by = "PhaseBlockIndex")
  if (anyNA(rows$Phase)) stop("A raw row falls outside the vendor phase blocks.", call. = FALSE)
  first <- vendor[1, , drop = FALSE]
  out <- rows %>%
    dplyr::arrange(t_ms, AnimalID) %>%
    dplyr::transmute(
      DateTime = antenna_dv_format_time(t),
      AnimalID, System = paste0("sys.", Board), PositionID,
      CageChange = first$CageChange, Batch = first$Batch, Phase,
      ConsecActive = as.integer(ConsecActive), ConsecInactive = as.integer(ConsecInactive),
      HalfHoursElapsed = as.integer(animalpos_half_hours_elapsed(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))),
      is_synthetic_preprocessing_row = seed_row,
      synthetic_row_source = ifelse(seed_row, if (is.null(move_rule)) ANTENNA_DV_SEED_SOURCE else ANTENNA_DV_SEED_SOURCE_HELD,
                                    NA_character_))
  if (!is.null(move_rule)) {
    step <- rows %>% dplyr::arrange(AnimalNum, t_ms) %>% dplyr::group_by(AnimalNum) %>%
      dplyr::summarise(below = { d <- rfid_grid_distance(PositionID[-1], PositionID[-dplyr::n()])
                                 sum(if (move_rule$inclusive) d < move_rule$threshold - 1e-9 else d <= move_rule$threshold + 1e-9) },
                       .groups = "drop")
    if (any(step$below > 0)) stop("Consecutive rows closer than the relocation threshold for ", paste(step$AnimalNum[step$below > 0], collapse = ", "), call. = FALSE)
  }
  seeds <- built$audit %>%
    dplyr::left_join(ids %>% dplyr::select(AnimalNum, AnimalID), by = "AnimalNum") %>%
    dplyr::transmute(AnimalID, AnimalNum, Board, seeded,
                     seed_detection_age_s = ifelse(seeded, seed_lag_seconds, NA_real_),
                     seed_PositionID = vapply(AnimalNum, function(a) {
                       r <- rows[rows$AnimalNum == a & rows$seed_row, , drop = FALSE]
                       if (nrow(r)) r$PositionID[1] else NA_integer_ }, integer(1)),
                     first_change_after_span_start_s = vapply(AnimalNum, function(a) {
                       r <- rows[rows$AnimalNum == a & !rows$seed_row, , drop = FALSE]
                       if (nrow(r)) min(r$t) - cal$span_start else NA_real_ }, numeric(1)))
  if (!is.null(move_rule)) {
    seeds <- seeds %>% dplyr::left_join(
      built$audit %>% dplyr::transmute(AnimalNum, seed_detection_PositionID, seed_emission_age_s = seed_emission_lag_seconds,
                                       seed_from_first_detection), by = "AnimalNum")
  }
  audit <- built$audit %>%
    dplyr::mutate(span_start = cal$span_start, span_end = span_end, recording_end = recording_end,
                  vendor_span_end = cal$span_end, min_flip_sec = min_flip_sec,
                  clock_fold_rows_dropped = recording$clock_fold_rows_dropped,
                  raw_logs_ignored = paste(recording$ignored, collapse = "|"))
  list(rows = as.data.frame(out[, ANTENNA_DV_COLUMNS]), seeds = seeds, audit = audit)
}

#' Board placement in an initScan file (placement scan at the cage change): the board that read each tag most.
#' Returns tag, initscan_board, initscan_reads (empty if the file holds no detections).
antenna_dv_initscan_boards <- function(path) {
  log <- tryCatch(rfid_read_raw_log(path), error = function(e) NULL)
  if (is.null(log) || !nrow(log$events)) return(data.frame(tag = character(), initscan_board = integer(), initscan_reads = integer(), initscan_board_share = numeric()))
  log$events %>%
    dplyr::count(tag, board, name = "n") %>%
    dplyr::group_by(tag) %>%
    dplyr::summarise(initscan_board = board[which.max(n)], initscan_reads = sum(n),
                     initscan_board_share = max(n) / sum(n), .groups = "drop") %>%
    as.data.frame()
}

#' Board stability: the majority board of each animal in every phase block of the span (detections, not intervals).
antenna_dv_board_by_block <- function(events, tags, roster_ids, span_start, span_end) {
  events %>%
    dplyr::inner_join(tags %>% dplyr::filter(AnimalNum %in% roster_ids), by = "tag") %>%
    dplyr::filter(t >= span_start, t < span_end) %>%
    dplyr::mutate(PhaseBlockIndex = animalpos_phase_block_index(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))) %>%
    dplyr::count(AnimalNum, PhaseBlockIndex, board, name = "n") %>%
    dplyr::group_by(AnimalNum, PhaseBlockIndex) %>%
    dplyr::summarise(block_board = board[which.max(n)], block_board_share = max(n) / sum(n), .groups = "drop")
}
