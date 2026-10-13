# ================================================================
# PhenoSys AnimalPos position estimate, reconstructed from the raw RFID reads
# MMMSociability -- Functions/animalpos_reconstruction.R
# ================================================================
# Pure definitions; needs animalpos_position_id() (Functions/animalpos_preprocessing_helpers.R). The algorithm was
# reconstructed from the data (2026-10-13), not taken from vendor documentation; its verification against the vendor
# exports is documented in docs/PHENOSYS_ANIMALPOS_RECONSTRUCTION.md.
#
# Reads. A raw event row (rfid_read_raw_log(keep_read_offsets = TRUE)) holds one tag's consecutive reads at one
# antenna: its start (DateTime) and MsgValue1, the gaps between its reads in ms (first 0), so read k lies at
# round(start in ms) + gap_1 + ... + gap_k. Each read takes the antenna's grid coordinate (x 0/100/200/300, y 0/116).
# A tag's reads on all boards are pooled and ordered by time, then by the event's start and row in the log, then by
# their order in the event. Times are integer milliseconds.
#
# Estimator (the 500-ms window mean). s = the oldest read still held. When read j arrives and t_j - t_s > 500 ms
# (strictly), the window s..j-1 is EVALUATED: its mean (x, y) over all those reads, unrounded, stamped t_s. Then every
# read with t < t_j - 500 ms (strictly) is dropped and s moves to the first read kept. One window is evaluated per
# arriving read; windows that close at the same arrival are dropped unevaluated. Nothing is evaluated without a new
# read, so a gap in the reads produces no estimate; the last window of a tag is never evaluated.
#
# Gate "vendor" (the AnimalPos export). The reference starts at the origin (0, 0). The first evaluation is always
# written: its mean if that lies >= 200 grid units (Euclidean) from the origin, otherwise the origin itself, which then
# stays the reference. Later evaluations are written when their mean lies >= 200 units from the last WRITTEN position,
# which they then become. A closing record at the tag's last read repeats the last written position. The export shows
# x and y rounded to 0.1; the decision uses the unrounded mean.
#
# Gate "none" (variant without the displacement gate). Every evaluation is an estimate. Its cell is the export's
# floor-snap of the mean (rounded to 0.1, then animalpos_position_id(), as the vendor arm's preprocessing does); a
# record is written when the cell differs from the last written cell, and the first evaluation is always written. No
# origin placeholder and no closing record (both exist only because of the gate). The numbers of evaluations and of
# changes of the unsnapped mean are returned as attributes, because they measure a different quantity than cell changes.
# ================================================================

APR_WINDOW_MS <- 500
APR_THRESHOLD <- 200

#' Per-read table of one recording's events (rfid_read_raw_log(keep_read_offsets = TRUE)$events or a subset), sorted by
#' tag and read order. Columns: tag, t_ms, gx, gy, board, antenna, PositionID, event (row in `events`), read (1..reads).
#' allow_negative_gaps = TRUE only for logs read with drop_clock_fold = FALSE: an event that spans the moment the logger
#' clock fell back (daylight saving ends) lists a gap of about -1 h; its read times then follow the logged wall clock.
apr_reads <- function(events, allow_negative_gaps = FALSE) {
  if (!all(c("read_offsets_ms", "file_order") %in% names(events))) stop("events need read_offsets_ms and file_order (keep_read_offsets = TRUE).", call. = FALSE)
  gaps <- strsplit(events$read_offsets_ms, ",", fixed = TRUE)
  n <- lengths(gaps)
  if (any(n != events$reads)) stop("MsgValue1 lists a different number of reads than senseRFIDrecords in ", sum(n != events$reads), " events.", call. = FALSE)
  ev <- rep(seq_len(nrow(events)), n)
  g <- as.numeric(unlist(gaps, use.names = FALSE))
  if (anyNA(g) || (!allow_negative_gaps && any(g < 0))) stop("Unreadable or negative read gaps in MsgValue1.", call. = FALSE)
  start <- cumsum(n) - n                                    # reads before each event (integer(0) for no events)
  cum <- cumsum(g) - rep(c(0, cumsum(g))[start + 1L], n)    # within-event cumulative gap
  r <- data.frame(tag = events$tag[ev], t_ms = round(events$t[ev] * 1000) + cum, gx = events$gx[ev], gy = events$gy[ev],
                  board = events$board[ev], antenna = events$antenna[ev], PositionID = events$PositionID[ev],
                  event = ev, read = sequence(n), ev_t = events$t[ev], ev_order = events$file_order[ev], stringsAsFactors = FALSE)
  r <- r[order(r$tag, r$t_ms, r$ev_t, r$ev_order, r$read), , drop = FALSE]
  r$ev_t <- NULL; r$ev_order <- NULL
  rownames(r) <- NULL
  r
}

#' The windows evaluated for one tag: indices s (oldest read) and j (the arriving read that closes the window); the
#' window holds reads s..j-1. `t_ms` sorted, integer ms.
apr_window_schedule <- function(t_ms, window_ms = APR_WINDOW_MS) {
  n <- length(t_ms)
  out_s <- integer(n); out_j <- integer(n); m <- 0L
  s <- 1L
  for (j in seq.int(2L, length.out = max(n - 1L, 0L))) {
    if (t_ms[j] - t_ms[s] > window_ms) {
      m <- m + 1L; out_s[m] <- s; out_j[m] <- j
      lim <- t_ms[j] - window_ms
      while (s < j && t_ms[s] < lim) s <- s + 1L
    }
  }
  list(s = out_s[seq_len(m)], j = out_j[seq_len(m)])
}

#' Position records of one tag under the reconstructed estimator. t_ms (sorted), gx, gy: its reads (apr_reads()).
#' gate "vendor": the AnimalPos records (kind first/move/closing); gate "none": the cell-change records (kind
#' first/move). Columns: t_ms (window start; the last read for a closing record), x, y (unrounded mean), x_export,
#' y_export (rounded to 0.1), PositionID (floor-snap of the export values), n_reads, n_antennas (distinct coordinates in
#' the window), kind. Attributes: evaluations, mean_changes (changes of the unsnapped mean between evaluations), and,
#' for span = c(from_ms, to_ms), evaluations_in_span, mean_changes_in_span and (gate "none") cell_changes_in_span: the
#' evaluations stamped in [from, to), and the changes of the mean or of the cell from the previous evaluation to one
#' stamped in [from, to) -- the change from the position held at `from` counts, as in Stage 01; a first record does not.
apr_positions <- function(t_ms, gx, gy, gate = c("vendor", "none"), window_ms = APR_WINDOW_MS, threshold = APR_THRESHOLD,
                          eps = 1e-6, span = NULL) {
  gate <- match.arg(gate)
  if (is.unsorted(t_ms)) stop("t_ms must be sorted.", call. = FALSE)
  w <- apr_window_schedule(t_ms, window_ms)
  cx <- c(0, cumsum(gx)); cy <- c(0, cumsum(gy)); k <- w$j - w$s
  mx <- (cx[w$j] - cx[w$s]) / k; my <- (cy[w$j] - cy[w$s]) / k
  ne <- length(mx)
  changed <- if (ne > 1) c(FALSE, mx[-1] != mx[-ne] | my[-1] != my[-ne]) else logical(ne)
  mean_changes <- sum(changed)
  in_span <- if (is.null(span)) NULL else t_ms[w$s] >= span[1] & t_ms[w$s] < span[2]
  if (gate == "vendor") {
    keep <- logical(ne); rx <- numeric(ne); ry <- numeric(ne); lx <- 0; ly <- 0
    for (e in seq_len(ne)) {
      far <- sqrt((mx[e] - lx)^2 + (my[e] - ly)^2) >= threshold - eps
      if (far) { lx <- mx[e]; ly <- my[e] }
      if (e == 1L || far) { keep[e] <- TRUE; rx[e] <- lx; ry[e] <- ly }
    }
    kind <- ifelse(seq_len(ne) == 1L, "first", "move")[keep]
    x <- rx[keep]; y <- ry[keep]
  } else {
    cell <- animalpos_position_id(round(mx, 1), round(my, 1))
    keep <- c(rep(TRUE, min(ne, 1L)), if (ne > 1) cell[-1] != cell[-ne] else logical(0))
    kind <- ifelse(seq_len(ne) == 1L, "first", "move")[keep]
    x <- mx[keep]; y <- my[keep]
  }
  sel <- which(keep)
  n_ant <- vapply(sel, function(e) length(unique(paste(gx[w$s[e]:(w$j[e] - 1L)], gy[w$s[e]:(w$j[e] - 1L)]))), integer(1))
  out <- data.frame(t_ms = t_ms[w$s[sel]], x = x, y = y, n_reads = k[sel], n_antennas = n_ant, kind = kind, stringsAsFactors = FALSE)
  if (gate == "vendor" && nrow(out)) {
    out <- rbind(out, data.frame(t_ms = t_ms[length(t_ms)], x = out$x[nrow(out)], y = out$y[nrow(out)], n_reads = 0L,
                                 n_antennas = 0L, kind = "closing", stringsAsFactors = FALSE))
  }
  out$x_export <- round(out$x, 1); out$y_export <- round(out$y, 1)
  out$PositionID <- animalpos_position_id(out$x_export, out$y_export)
  attr(out, "evaluations") <- ne
  attr(out, "mean_changes") <- mean_changes
  if (!is.null(span)) {
    attr(out, "evaluations_in_span") <- sum(in_span)
    attr(out, "mean_changes_in_span") <- sum(changed & in_span)
    if (gate == "none") attr(out, "cell_changes_in_span") <- sum(c(FALSE, keep[-1]) & in_span)   # keep[-1]: cell changes
  }
  out
}

#' Vendor AnimalPos export of a recording, parsed for comparison: AnimalNum, System (vendor label, e.g. sys.3), tag,
#' t_ms, x, y (as exported). path: raw_data/B<n>/E9_SIS_B<n>_CC<k>_AnimalPos.csv.
apr_read_vendor_export <- function(path, canon = canonical_animal_id) {
  a <- data.table::fread(path, sep = ";", select = c("DateTime", "Animal", "RFID", "xPos", "yPos"), colClasses = "character", showProgress = FALSE)
  a <- a[nzchar(a$RFID) & nzchar(a$Animal), ]
  dt <- as.POSIXct(a$DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  if (anyNA(dt)) stop("Unparseable vendor DateTime in ", basename(path), call. = FALSE)
  data.frame(AnimalNum = rfid_label_animal(a$Animal, canon), System = paste0("sys.", sub("^.*[_-]sys[.]", "", a$Animal)), tag = a$RFID,
             t_ms = round(as.numeric(dt) * 1000), x = as.numeric(a$xPos), y = as.numeric(a$yPos), stringsAsFactors = FALSE)
}

#' Record-by-record comparison of emulated and vendor records of one tag: exact match on (t_ms, x and y at 0.1).
#' Returns list(matched, vendor_only, emulated_only) as data frames.
apr_compare_records <- function(emulated, vendor) {
  ke <- paste(emulated$t_ms, round(emulated$x, 1), round(emulated$y, 1))
  kv <- paste(vendor$t_ms, round(vendor$x, 1), round(vendor$y, 1))
  list(matched = vendor[kv %in% ke, , drop = FALSE], vendor_only = vendor[!(kv %in% ke), , drop = FALSE],
       emulated_only = emulated[!(ke %in% kv), , drop = FALSE])
}
