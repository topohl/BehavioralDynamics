# ================================================================
# Raw antenna logs: per-animal reader occupancy from the PhenoSoft MultiMouseMonitor recordings
# MMMSociability
# ================================================================
# Used by Analysis/19_spatial_occupancy_maps.R. Pure definitions: sourcing this file reads no data and writes no
# files. Needs dplyr/tibble (attached by the caller) and animalpos_position_id() from
# Functions/animalpos_preprocessing_helpers.R.
#
# Why the raw logs. The vendor AnimalPos export writes a position only after the tag's position estimate has moved
# >= 200 grid units, so moves to the other row of a column (116 units) or to a neighbouring column (100 units) are
# never recorded. The raw logs hold every detection event: antenna, start time and duration. Each cage-change
# recording is one file, Raw Data/Behavior/RFID/MMMdata/B<n>/raw/MultiMouseMonitor-...-Batch<n>_change_<k>_....csv,
# spanning the same seconds as its AnimalPos export (checked 2026-10-12 for all 24 recordings).
#
# File format. UTF-16LE with a BOM; ';'-separated; a header row, then '#ID-Device;AM<board>-<antenna>;x;y;z;...'
# rows giving every antenna's grid coordinates, then event rows. DateTime is an Excel serial day of the local clock
# (read as UTC, like every other Exp9 timestamp); IdRFID is the tag, mapped to the animal through the vendor
# AnimalPos export of the same recording (rfid_animalpos_tag_map()), because IdLabel is "unknown" throughout
# Batch 1; elsewhere IdLabel (e.g. OR426_sys.5) is checked against that map. unitLabel is AM<board>-<antenna>, and
# the board is the physical cage. Rows are written when an event ENDS, so they are sorted here by start time.
#
# Occupancy rule. Each animal is assigned the antenna of its latest detection until its next detection (the same
# carry-forward rule as for vendor records, at full resolution), seeded by its last detection before the window.
# A tag sitting on a border is read alternately by two antennas; a run at one antenna that lasts less than
# `min_flip_sec` and returns to the antenna it came from is reassigned to that antenna (rfid_flip_filter()).
#
# Relocation rule (optional, rfid_move_rule()). The vendor AnimalPos stream records a new position only when the
# tag's position estimate lies >= 200 grid units (Euclidean) from the last recorded position; its estimate is the
# mean of the reads in a 500-ms window, so it can fall between antennas (reconstructed exactly from the raw reads on
# 2026-10-13). rfid_move_threshold_filter() applies the same >= 200 rule to the antennas of the detections themselves:
# a detection becomes the held position only if its antenna lies at least the threshold from the held position.
# ================================================================

# Excel serial day 25569 = 1970-01-01.
RFID_EXCEL_EPOCH_DAYS <- 25569

#' Candidate raw logs of a batch and cage change, e.g. ("B3", "CC1"): MultiMouseMonitor-...-Batch<n>_change_<k>_....csv.
#' initScan files are not recordings. Fails if there is none.
rfid_raw_log_candidates <- function(raw_root, batch, cage_change) {
  b <- as.integer(sub("^B", "", batch)); k <- as.integer(sub("^CC", "", cage_change))
  dir <- file.path(raw_root, paste0("B", b), "raw")
  hits <- list.files(dir, pattern = sprintf("Batch%d_change_%d_.*[.]csv$", b, k), full.names = TRUE)
  hits <- hits[!grepl("initScan", basename(hits), fixed = TRUE)]
  if (!length(hits)) stop("No raw recording for ", batch, " ", cage_change, " in ", dir, call. = FALSE)
  hits
}

#' The one recording of a batch and cage change, read. Candidate files without any detection event are sessions in
#' which the software was only opened (e.g. MultiMouseMonitor-25.01.07-Batch6_change_4_19Apr24.csv, start and end
#' markers only); they are ignored and named in `ignored`. Fails unless exactly one candidate holds detections.
#' Returns list(path, ignored, events, devices). keep_read_offsets and drop_clock_fold are passed to rfid_read_raw_log().
rfid_read_recording <- function(raw_root, batch, cage_change, keep_read_offsets = FALSE, drop_clock_fold = TRUE) {
  paths <- rfid_raw_log_candidates(raw_root, batch, cage_change)
  # a candidate that cannot be read counts as holding no detections; the count check below still applies
  logs <- lapply(paths, function(p) tryCatch(rfid_read_raw_log(p, keep_read_offsets = keep_read_offsets, drop_clock_fold = drop_clock_fold),
                                             error = function(e) list(events = tibble::tibble(), devices = NULL)))
  has_events <- vapply(logs, function(l) nrow(l$events) > 0, logical(1))
  if (sum(has_events) != 1) {
    stop("Expected exactly one raw recording with detections for ", batch, " ", cage_change, "; found ",
         sum(has_events), " among ", paste(basename(paths), collapse = ", "), call. = FALSE)
  }
  c(list(path = paths[has_events], ignored = basename(paths[!has_events])), logs[[which(has_events)]])
}

#' Reads one raw log. Returns list(events, devices): events with t (POSIX seconds of the event start), dur_sec,
#' reads, tag, label, board, antenna, gx, gy, PositionID; devices with board, antenna, gx, gy, PositionID.
#' keep_read_offsets = TRUE (opt-in) adds two event columns: read_offsets_ms, the MsgValue1 field as written (the
#'   comma-separated times of the event's reads in ms after the event start; the first is 0), and file_order, the
#'   event's row in the log. The read-level times are what the PhenoSys position estimate averages (see
#'   Functions/animalpos_reconstruction.R).
#' drop_clock_fold = FALSE (diagnostics only) keeps the rows logged while the clock repeated an hour (see below).
#' With the defaults the result is exactly that of the earlier one-argument version.
rfid_read_raw_log <- function(path, keep_read_offsets = FALSE, drop_clock_fold = TRUE) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  txt <- iconv(list(bytes), from = "UTF-16LE", to = "UTF-8")
  if (is.na(txt)) stop("Cannot decode ", path, " as UTF-16LE.", call. = FALSE)
  lines <- strsplit(txt, "\r?\n")[[1]]
  lines[1] <- sub(paste0("^", intToUtf8(0xFEFF)), "", lines[1])   # byte-order mark
  header <- strsplit(lines[1], ";", fixed = TRUE)[[1]]
  need <- c("DateTime", "IdRFID", "IdLabel", "unitLabel", "eventDuration", "senseRFIDrecords")
  if (!all(need %in% header)) stop("Unexpected raw log header in ", path, call. = FALSE)

  dev_lines <- lines[startsWith(lines, "#ID-Device;")]
  dev <- do.call(rbind, strsplit(dev_lines, ";", fixed = TRUE))
  devices <- tibble::tibble(unit = dev[, 2], gx = as.numeric(dev[, 3]), gy = as.numeric(dev[, 4])) %>%
    dplyr::mutate(board = as.integer(sub("^AM([0-9]+)-.*$", "\\1", unit)),
                  antenna = as.integer(sub("^AM[0-9]+-", "", unit)),
                  PositionID = animalpos_position_id(gx, gy))
  if (anyNA(devices$PositionID) || any(!(devices$gx %in% c(0, 100, 200, 300))) || any(!(devices$gy %in% c(0, 116)))) {
    stop("Raw log ", basename(path), " has antennas off the 4 x 2 grid.", call. = FALSE)
  }
  dup <- devices %>% dplyr::count(board, PositionID) %>% dplyr::filter(n > 1)
  if (nrow(dup) > 0) stop("Raw log ", basename(path), " maps two antennas of one board to one reader.", call. = FALSE)

  body <- lines[-1]
  body <- body[nzchar(body) & !startsWith(body, "#")]
  if (keep_read_offsets && !("MsgValue1" %in% header)) stop("Raw log ", basename(path), " has no MsgValue1 column.", call. = FALSE)
  d <- data.table::fread(text = c(paste(header, collapse = ";"), body), sep = ";", select = c(need, if (keep_read_offsets) "MsgValue1"),
                         colClasses = "character", fill = TRUE, showProgress = FALSE)
  if (keep_read_offsets) d$file_order <- seq_len(nrow(d))
  d <- tibble::as_tibble(d) %>%
    dplyr::filter(nzchar(IdRFID), grepl("^AM[0-9]+-[1-8]$", unitLabel))
  extra <- if (keep_read_offsets) d[, c("MsgValue1", "file_order")] else NULL
  d <- d %>%
    dplyr::transmute(t = (as.numeric(DateTime) - RFID_EXCEL_EPOCH_DAYS) * 86400,
                     raw_dur_sec = as.numeric(eventDuration) / 1000,
                     reads = as.integer(senseRFIDrecords), tag = IdRFID, label = IdLabel, unit = unitLabel)
  if (keep_read_offsets) { d$read_offsets_ms <- extra$MsgValue1; d$file_order <- extra$file_order }
  if (any(!is.finite(d$t))) stop("Raw log ", basename(path), " has unreadable event times.", call. = FALSE)
  # Clock fall-back: the pipeline keeps the logger's wall clock, so the hour logged twice when daylight saving ends
  # (B1 CC1, 2022-10-30 02:00-03:00) is dropped rather than interleaved with the first pass (rfid_clock_fold_rows()).
  fold <- if (drop_clock_fold) rfid_clock_fold_rows(d$t + d$raw_dur_sec) else logical(nrow(d))
  d <- d[!fold, , drop = FALSE] %>%
    dplyr::mutate(dur_sec = pmax(raw_dur_sec, 0)) %>%
    dplyr::select(-raw_dur_sec) %>%
    dplyr::inner_join(devices, by = "unit")
  list(events = d, devices = devices, clock_fold_rows_dropped = sum(fold))
}

#' Rows logged after the logger clock went back, until it caught up again. Rows are written when an event ends, so
#' in file order their end times only move forward, apart from seconds of jitter. A row ending more than
#' `jump_sec` before the latest end logged so far marks a clock reset; it and the following rows are flagged until
#' the clock passes that latest end again. `end` is in file order; returns a logical vector.
rfid_clock_fold_rows <- function(end, jump_sec = 1800) {
  fold <- logical(length(end))
  if (length(end) < 2) return(fold)
  latest <- cummax(end)
  k <- 2L
  while (k <= length(end)) {
    if (end[k] < latest[k - 1L] - jump_sec) {
      caught_up <- which(end[k:length(end)] >= latest[k - 1L])
      stop_at <- if (length(caught_up)) k + caught_up[1] - 2L else length(end)
      fold[k:stop_at] <- TRUE
      k <- stop_at + 1L
    } else {
      k <- k + 1L
    }
  }
  fold
}

#' Animal of a vendor label such as "OR426_sys.5" or "00317_sys.2", as a canonical ID (canonical_animal_id()).
rfid_label_animal <- function(label, canon = canonical_animal_id) canon(sub("[_-]sys.*$", "", label))

#' Removes border flicker: a run at antenna B between two runs at the same antenna A that lasts less than
#' `min_flip_sec` (from its start to the start of the next run) is reassigned to A; repeated until stable, so
#' A-B-A-B-A collapses to A. `t` must be sorted. Returns the filtered antenna sequence.
rfid_flip_filter <- function(t, pid, min_flip_sec) {
  if (!is.finite(min_flip_sec) || min_flip_sec <= 0 || length(pid) < 3) return(pid)
  repeat {
    r <- rle(pid); n <- length(r$lengths)
    if (n < 3) break
    first <- cumsum(c(1L, head(r$lengths, -1L)))
    run_len <- c(t[first[-1]], Inf) - t[first]
    inner <- seq.int(2L, n - 1L)
    flip <- inner[run_len[inner] < min_flip_sec & r$values[inner - 1L] == r$values[inner + 1L]]
    if (!length(flip)) break
    r$values[flip] <- r$values[flip - 1L]
    pid <- inverse.rle(r)
  }
  pid
}

# Grid coordinates of the readers by PositionID (R1-R4 at y = 0, R5-R8 at y = 116; the inverse of
# animalpos_position_id()).
RFID_GRID_X <- c(0, 100, 200, 300, 0, 100, 200, 300)
RFID_GRID_Y <- c(0, 0, 0, 0, 116, 116, 116, 116)

#' A relocation rule: a detection replaces the held position only if its antenna lies `threshold` grid units or more
#' (Euclidean; strictly more when inclusive = FALSE) from the held position. The vendor rule is threshold 200,
#' inclusive. On the antenna grid this records moves of two or more columns and never a row change (116 units) or a
#' move to a neighbouring column (100 or 153.2 units).
rfid_move_rule <- function(threshold = 200, inclusive = TRUE) {
  if (!is.numeric(threshold) || length(threshold) != 1 || !is.finite(threshold) || threshold <= 0)
    stop("threshold must be one positive number.", call. = FALSE)
  if (!is.logical(inclusive) || length(inclusive) != 1 || is.na(inclusive)) stop("inclusive must be TRUE or FALSE.", call. = FALSE)
  structure(list(threshold = threshold, inclusive = inclusive), class = "rfid_move_rule")
}

#' One-line description of a relocation rule (README and audit).
rfid_move_rule_label <- function(rule) {
  sprintf("Euclidean distance %s %g grid units from the held position (antenna coordinates)", if (rule$inclusive) ">=" else ">", rule$threshold)
}

#' Euclidean grid distance between readers (PositionID vectors).
rfid_grid_distance <- function(p, q) sqrt((RFID_GRID_X[p] - RFID_GRID_X[q])^2 + (RFID_GRID_Y[p] - RFID_GRID_Y[q])^2)

#' Held position after each detection under a relocation rule. `pid` is one animal's time-sorted detection sequence;
#' the first detection sets the held position; each later detection replaces it only if it lies at least the threshold
#' from it (the reference is the held position, never the previous detection). Returns an integer vector like `pid`.
rfid_move_threshold_filter <- function(pid, rule) {
  if (!inherits(rule, "rfid_move_rule")) stop("rule must come from rfid_move_rule().", call. = FALSE)
  if (!length(pid)) return(integer(0))
  r <- rle(as.integer(pid))
  held <- integer(length(r$values)); cur <- r$values[1]
  for (k in seq_along(r$values)) {
    d <- rfid_grid_distance(r$values[k], cur)
    if (if (rule$inclusive) d >= rule$threshold - 1e-9 else d > rule$threshold + 1e-9) cur <- r$values[k]
    held[k] <- cur
  }
  rep(held, r$lengths)
}

#' Carry-forward occupancy intervals of one animal in [span_start, span_end). `t` (sorted) and `pid` are its
#' detections on its own board after the flip filter. Seeded by the last detection before span_start; without
#' one, assignment starts at the first detection. Consecutive equal states are merged.
rfid_carry_forward_intervals <- function(t, pid, span_start, span_end) {
  before <- which(t < span_start)
  inside <- which(t >= span_start & t < span_end)
  start <- c(if (length(before)) span_start else numeric(0), t[inside])
  state <- c(if (length(before)) pid[max(before)] else integer(0), pid[inside])
  if (!length(start)) return(tibble::tibble(PositionID = integer(), start = numeric(), end = numeric()))
  keep <- c(TRUE, state[-1] != state[-length(state)])
  start <- start[keep]; state <- state[keep]
  tibble::tibble(PositionID = as.integer(state), start = start, end = c(start[-1], span_end))
}

#' RFID tag of every animal in a vendor AnimalPos export (raw_data/B<n>/E9_SIS_B<n>_CC<k>_AnimalPos.csv: Animal such
#' as OQ772_sys.1 and RFID). The raw logs' own IdLabel cannot be used for this: Batch 1 was recorded without animal
#' labels ("unknown"). Fails if an animal has two tags or a tag two animals.
rfid_animalpos_tag_map <- function(path, canon = canonical_animal_id) {
  a <- data.table::fread(path, sep = ";", select = c("Animal", "RFID"), colClasses = "character", showProgress = FALSE)
  map <- tibble::as_tibble(a) %>%
    dplyr::filter(nzchar(RFID), nzchar(Animal)) %>%
    dplyr::distinct(AnimalNum = rfid_label_animal(Animal, canon), tag = RFID)
  bad_animal <- map %>% dplyr::count(AnimalNum) %>% dplyr::filter(n > 1)
  bad_tag <- map %>% dplyr::count(tag) %>% dplyr::filter(n > 1)
  if (nrow(bad_animal) > 0 || nrow(bad_tag) > 0) {
    stop("Ambiguous tag map in ", basename(path), ": ",
         paste(c(bad_animal$AnimalNum, bad_tag$tag), collapse = ", "), call. = FALSE)
  }
  map
}

#' Occupancy intervals of every roster animal in one recording.
#'
#' events  rfid_read_raw_log()$events
#' roster  one row per animal expected in this recording: AnimalNum, System_label (vendor system label, e.g. sys.3)
#' tags    rfid_animalpos_tag_map() of the same recording: AnimalNum, tag
#' span_start, span_end  analysis span in POSIX seconds
#' Each animal's cage is the board that holds most of its detections inside the span; detections on other boards
#' are dropped and counted. Where the log carries animal labels, the audit checks them against the tag map.
#' move_rule  NULL (default: every change of antenna counts) or rfid_move_rule(): the rule then runs over all of the
#'   animal's on-board detections from its first one, so the position at span_start is the held position, not the
#'   antenna of the last detection; detections in the same millisecond keep the last one first (positions are written
#'   at millisecond resolution). The audit then gains reader_changes_after_move_rule (in-span changes, not counting the
#'   step from the seed to the first in-span position), seed_detection_PositionID, seed_held_PositionID,
#'   seed_emission_lag_seconds (age of the relocation that set the held position), seed_from_first_detection and
#'   move_rule.
#' Returns list(intervals, audit).
rfid_raw_occupancy_intervals <- function(events, roster, tags, span_start, span_end, min_flip_sec,
                                         canon = canonical_animal_id, move_rule = NULL) {
  missing_tag <- setdiff(roster$AnimalNum, tags$AnimalNum)
  if (length(missing_tag) > 0) {
    stop("No RFID tag for roster animal(s) ", paste(missing_tag, collapse = ", "), call. = FALSE)
  }
  ev <- events %>% dplyr::inner_join(tags %>% dplyr::filter(AnimalNum %in% roster$AnimalNum), by = "tag")
  strip_or <- function(x) sub("^OR(?=[0-9]+$)", "", x, perl = TRUE)
  out <- vector("list", nrow(roster)); aud <- vector("list", nrow(roster))
  for (i in seq_len(nrow(roster))) {
    a <- roster$AnimalNum[i]
    x <- ev[ev$AnimalNum == a, , drop = FALSE]
    x <- x[order(x$t), , drop = FALSE]
    in_span <- x$t >= span_start & x$t < span_end
    board <- if (any(in_span)) as.integer(names(which.max(table(x$board[in_span])))) else NA_integer_
    on <- !is.na(board) & x$board == board
    xs <- x[on, , drop = FALSE]
    filtered <- rfid_flip_filter(xs$t, xs$PositionID, min_flip_sec)
    if (is.null(move_rule)) {
      iv <- rfid_carry_forward_intervals(xs$t, filtered, span_start, span_end)
    } else {
      last_in_ms <- if (nrow(xs)) c(round(xs$t[-1] * 1000) != round(xs$t[-nrow(xs)] * 1000), TRUE) else logical(0)
      th <- xs$t[last_in_ms]; fh <- filtered[last_in_ms]
      held <- rfid_move_threshold_filter(fh, move_rule)
      iv <- rfid_carry_forward_intervals(th, held, span_start, span_end)
      pre <- which(th < span_start)
      if (length(pre) && any(th >= span_start & round(th * 1000) == round(span_start * 1000)))   # never in the 24 recordings
        stop("Animal ", a, ": a detection in the span-start millisecond would merge with the seed row.", call. = FALSE)
      seed_i <- if (length(pre)) max(pre) else NA_integer_
      changed_at <- which(c(TRUE, diff(held) != 0))           # detections at which the held position took a new value
      set_i <- if (is.na(seed_i)) NA_integer_ else max(changed_at[changed_at <= seed_i])
      move_audit <- tibble::tibble(
        reader_changes_after_move_rule = sum(diff(held[th >= span_start & th < span_end]) != 0),
        seed_detection_PositionID = if (is.na(seed_i)) NA_integer_ else as.integer(fh[seed_i]),
        seed_held_PositionID = if (is.na(seed_i)) NA_integer_ else as.integer(held[seed_i]),
        seed_emission_lag_seconds = if (is.na(seed_i)) NA_real_ else span_start - th[set_i],
        seed_from_first_detection = if (is.na(seed_i)) NA else set_i == 1L,
        move_rule = rfid_move_rule_label(move_rule))
    }
    raw_changes <- sum(diff(xs$PositionID[xs$t >= span_start & xs$t < span_end]) != 0)
    kept_changes <- sum(diff(filtered[xs$t >= span_start & xs$t < span_end]) != 0)
    seed_t <- if (any(xs$t < span_start)) max(xs$t[xs$t < span_start]) else NA_real_
    det <- xs[xs$t >= span_start & xs$t < span_end, , drop = FALSE]
    labelled <- x$label[!(x$label %in% c("", "unknown"))]
    out[[i]] <- if (nrow(iv)) dplyr::mutate(iv, AnimalNum = a, Board = board) else NULL
    base_audit <- tibble::tibble(
      AnimalNum = a, tag = tags$tag[tags$AnimalNum == a], System_label = roster$System_label[i], Board = board,
      board_matches_label = !is.na(board) && identical(paste0("sys.", board), roster$System_label[i]),
      log_labels = if (length(labelled)) paste(sort(unique(labelled)), collapse = "|") else "none",
      # "OR1545" in a log and "1545" in AnimalPos name one animal (B3 CC3)
      log_label_matches_tag_map = if (length(labelled)) all(strip_or(rfid_label_animal(labelled, canon)) == strip_or(a)) else NA,
      events_in_span = sum(in_span), events_off_board = sum(in_span & !on),
      detected_seconds = rfid_union_seconds(det$t, pmin(det$t + det$dur_sec, span_end)),
      span_seconds = span_end - span_start,
      reader_changes_raw = raw_changes, reader_changes_after_flip_filter = kept_changes,
      seeded = !is.na(seed_t), seed_lag_seconds = span_start - seed_t,
      assigned_seconds = if (nrow(iv)) sum(iv$end - iv$start) else 0)
    aud[[i]] <- if (is.null(move_rule)) base_audit else dplyr::bind_cols(base_audit, move_audit)
  }
  list(intervals = dplyr::bind_rows(out), audit = dplyr::bind_rows(aud))
}

#' Seconds covered by at least one of the intervals [s, e) (detection events overlap when two antennas read a tag).
rfid_union_seconds <- function(s, e) {
  if (!length(s)) return(0)
  o <- order(s); s <- s[o]; e <- e[o]
  reach <- cummax(e)
  grp <- cumsum(c(TRUE, s[-1] > reach[-length(reach)]))
  sum(tapply(e, grp, max) - tapply(s, grp, min))
}

#' Shares of time per reader of one animal's intervals, clipped to [from, to); all 8 readers, summing to 1.
rfid_interval_shares <- function(intervals, from, to) {
  s <- pmax(intervals$start, from); e <- pmin(intervals$end, to)
  sec <- tapply(pmax(e - s, 0), factor(intervals$PositionID, levels = 1:8), sum)
  sec[is.na(sec)] <- 0
  if (sum(sec) <= 0) return(rep(NA_real_, 8))
  as.numeric(sec) / sum(sec)
}
