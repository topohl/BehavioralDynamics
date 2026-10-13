# ================================================================
# Raw RFID acquisition QC: integrity, reader health, detection support and reconstruction sensitivity
# MMMSociability -- Functions/antenna_acquisition_qc.R
# ================================================================
# Used by Analysis/00b_qc_raw_acquisition.R (Stage 00b, manual diagnostic). Pure definitions: sourcing this file reads
# no data and writes no files. Needs data.table and, at call time, Functions/antenna_log_helpers.R (rfid_flip_filter,
# rfid_carry_forward_intervals) and Functions/antenna_measurement.R (amv_*).
#
# Principles (2026-10-13):
#  * Group-blind. No CON/RES/SUS label, CombZ or outcome is read. A reader is compared with the same reader on the
#    other boards of the same recording (homologous), a board with its own history, an animal with its cage-mates in
#    the same 12-h block.
#  * Occupancy and read counts are behaviour as much as acquisition. The cage has resources (lab statement: R1 food,
#    R5 house, R8 water; the house end is supported by light-phase resting in 120 of 120 cage x CC, the food and water
#    rows are inferred). No check assumes uniform occupancy or read rates, low reads at a resource reader are not a
#    failure, and read counts are never equated with occupancy.
#  * A reader has failed only if it stays silent while its neighbours read tags it would co-read: a tag at one antenna
#    is also read within about a second by its row neighbours and column partner (crosstalk), so neighbour reads predict
#    co-reads. The co-read rates come from the homologous readers, never from an assumed uniformity.
#  * Hard failures are demonstrable acquisition defects; warnings are poor support, strong reconstruction sensitivity or
#    unexplained differences; everything else is a diagnostic. No threshold is set from group effects, CombZ, prediction
#    or significance. Thresholds come from acquisition distributions and are listed with their basis in aqc_rules().
# ================================================================

AQC_RULES_VERSION <- "aqc_rules_v0.2_provisional"

# Reader layout: grid coordinates and the lab-reported resources.
AQC_LAYOUT <- data.frame(
  PositionID = 1:8, reader = paste0("R", 1:8),
  x = c(0, 100, 200, 300, 0, 100, 200, 300), y = c(0, 0, 0, 0, 116, 116, 116, 116),
  column = c(1, 2, 3, 4, 1, 2, 3, 4),
  resource = c("food", "", "", "", "house", "", "", "water"),
  resource_status = c("lab-reported; row inferred from dark-phase visits", "", "", "",
                      "lab-reported; resting end supported by light-phase occupancy", "", "",
                      "lab-reported; row inferred from dark-phase visits"),
  stringsAsFactors = FALSE)

AQC_AGE_BREAKS <- c(2, 10, 60, 300)        # support-age classes (s since the last read)
AQC_COREAD_WINDOW_S <- 1                   # a co-read: the same tag read at the neighbour within 1 s
AQC_MIN_PAIR_EXPECTATION <- 30             # an informative neighbour pair: >= 30 expected co-reads
AQC_DEAD_CHANNEL_MIN_E <- 100              # whole-session silence with >= 100 expected co-read tag-minutes: testable
AQC_HEALTHY_ELSEWHERE <- 0.25              # median best co-read ratio of the same antenna in other recordings (healthy 1st percentile 0.27)
AQC_BEST_RATIO_WARN <- 0.02                # best-neighbour co-read ratio below this in an active block: warning
AQC_STRETCH_MIN_MINUTES <- 60              # silent stretches scanned for intermittent failure
AQC_STRETCH_WARN_E <- 200                  # expected active minutes with a co-read during a silent stretch
AQC_TAIL_SILENCE_S <- 3600                 # board silence reaching the span end
AQC_REMOVAL_SPREAD_S <- 600                # all tags of a board stopping within 10 min: removal signature
AQC_LOW_READS_REL <- 0.25                  # animal-block reads below 0.25 x the cage-mate median
AQC_NONROSTER_MIN_EVENTS <- 100            # an untracked tag present in the cage
AQC_STALL_GAP_S <- 1.1                     # within-event read gap above the ~1-s event-closing rule
AQC_UNUSUAL_QUANTILE <- 0.99               # "unusual" = above this quantile of all units of the same kind and phase

#' Antenna index (1-8 on a board) -> grid coordinates and reader, as the log's #ID-Device table documents.
aqc_expected_devices <- function() {
  a <- 1:8
  data.frame(antenna = a, gx = 100 * ((a - 1) %/% 2), gy = 116 * ((a - 1) %% 2),
             PositionID = ifelse((a - 1) %% 2 == 0, (a - 1) %/% 2 + 1, (a - 1) %/% 2 + 5))
}

#' The neighbour readers of each reader on the 4 x 2 grid: row neighbours (100 units), the column partner (116) and
#' diagonals (153). Returns data.table(k, j, type) with j a neighbour of k.
aqc_neighbours <- function() {
  L <- AQC_LAYOUT
  g <- data.table::CJ(k = 1:8, j = 1:8)[k != j]
  g[, `:=`(dx = abs(L$x[k] - L$x[j]), dy = abs(L$y[k] - L$y[j]))]
  g <- g[dx <= 100 & dy <= 116]
  g[, type := ifelse(dy == 0, "row", ifelse(dx == 0, "column", "diagonal"))]
  g[, .(k, j, type)][]
}

# For each middle-column reader k, the row neighbours on both sides: a direct change between them skips k. End-column
# readers have no such pair that skips only them (a diagonal change also bypasses the middle reader), so they get none.
AQC_SKIP_PAIRS <- data.frame(PositionID = c(2, 3, 6, 7), a = c(1, 2, 5, 6), b = c(3, 4, 7, 8))

#' Checks of a raw log's antenna table (rfid_read_raw_log()$devices): 40 devices, boards 1-5, every antenna at its
#' documented grid position. Returns a one-row data.frame.
aqc_device_check <- function(devices) {
  e <- aqc_expected_devices()
  m <- merge(as.data.frame(devices), e, by = "antenna", suffixes = c("", "_expected"))
  data.frame(devices = nrow(devices), boards = paste(sort(unique(devices$board)), collapse = "|"),
             devices_expected = nrow(devices) == 40 && setequal(unique(devices$board), 1:5),
             devices_on_grid = nrow(m) == nrow(devices) && all(m$gx == m$gx_expected & m$gy == m$gy_expected &
                                                                 m$PositionID == m$PositionID_expected))
}

#' Number of OTHER events of the same board that overlap each event in time ([s, e], positive overlap), by sorted
#' start and end times. Integer vector in input order.
aqc_board_overlap_counts <- function(board, s, e) {
  out <- integer(length(s))
  for (b in unique(board)) {
    i <- which(board == b)
    ss <- sort(s[i]); ee <- sort(e[i])
    starts_before_end <- findInterval(e[i], ss, left.open = TRUE)   # starts < e
    ends_until_start <- findInterval(s[i], ee)                      # ends <= s
    out[i] <- starts_before_end - ends_until_start - 1L
  }
  pmax(out, 0L)
}

#' Shares of within-event read gaps (ms) on the polling lattice k * 100 + 14.5 ms (+-3 ms), k = 1..9, the share on any
#' lattice point, the mean gap and the number of gaps. One-row data.frame.
aqc_lattice <- function(gap_ms) {
  gap_ms <- gap_ms[is.finite(gap_ms) & gap_ms > 0]
  k <- round((gap_ms - 14.5) / 100)
  on <- abs(gap_ms - (k * 100 + 14.5)) <= 3 & k >= 1 & k <= 9
  sh <- vapply(1:9, function(q) if (length(gap_ms)) mean(on & k == q) else NA_real_, 0)
  cbind(data.frame(gaps = length(gap_ms), mean_gap_ms = if (length(gap_ms)) mean(gap_ms) else NA_real_,
                   on_lattice_share = if (length(gap_ms)) mean(on) else NA_real_),
        as.data.frame(t(stats::setNames(sh, paste0("lattice_k", 1:9)))))
}

#' Co-reads: for every read (tag, board, PositionID, t), whether the same tag was read within `window` seconds at each
#' neighbour reader k of the same board. Returns the reads with one logical column per reader (co_R1..co_R8; FALSE for
#' the read's own reader and non-neighbours).
aqc_coread_flags <- function(reads, window = AQC_COREAD_WINDOW_S) {
  r <- data.table::copy(reads)
  data.table::setorder(r, tag, board, t)
  nb <- aqc_neighbours()
  for (k in 1:8) {
    col <- paste0("co_R", k)
    data.table::set(r, j = col, value = FALSE)
    js <- nb$j[nb$k == k]
    r[, (col) := {
      tk <- t[pid == k]
      if (!length(tk)) rep(FALSE, .N) else {
        idx <- findInterval(t, tk)
        left <- ifelse(idx >= 1, t - tk[pmax(idx, 1L)], Inf)
        right <- ifelse(idx < length(tk), tk[pmin(idx + 1L, length(tk))] - t, Inf)
        pid %in% js & pmin(left, right) <= window
      }
    }, by = .(tag, board)]
  }
  r[]
}

#' Co-read counts by board, block (and any other grouping) and directed neighbour pair: N = reads at j, O = reads at j
#' with a co-read at k. `flags` from aqc_coread_flags() with a `block` column.
aqc_coread_counts <- function(flags, by = c("board", "block")) {
  nb <- aqc_neighbours()
  n_j <- flags[, .(N = .N), by = c(by, "pid")]
  o <- data.table::rbindlist(lapply(1:8, function(kk) {
    col <- paste0("co_R", kk)
    js <- nb$j[nb$k == kk]
    flags[pid %in% js, .(O = sum(get(col))), by = c(by, "pid")][, k := kk]
  }))
  data.table::setnames(o, "pid", "j"); data.table::setnames(n_j, "pid", "j")
  out <- merge(o, n_j, by = c(by, "j"), all.x = TRUE)
  merge(out, nb, by = c("k", "j"))[]
}

#' Homologous co-read probabilities: for each board, p(j -> k) pooled over the OTHER boards of the same recording and
#' grouping, leaving out every board on which j or k read nothing at all (`dead`: data.table(board, PositionID)).
#' `counts`: aqc_coread_counts() summed to (board, group..., j, k). Returns counts with p_hom and E = N * p_hom.
aqc_homologous_expectation <- function(counts, dead = data.table::data.table(board = integer(), PositionID = integer()),
                                       group = character()) {
  cc <- data.table::copy(counts)
  cc[, ok := TRUE]
  if (nrow(dead)) {
    key_dead <- paste(dead$board, dead$PositionID)
    cc[paste(board, j) %in% key_dead | paste(board, k) %in% key_dead, ok := FALSE]
  }
  tot <- cc[ok == TRUE, .(O_all = sum(O), N_all = sum(N)), by = c(group, "j", "k")]
  own <- cc[ok == TRUE, .(O_own = sum(O), N_own = sum(N)), by = c("board", group, "j", "k")]
  cc <- merge(cc, tot, by = c(group, "j", "k"), all.x = TRUE)
  cc <- merge(cc, own, by = c("board", group, "j", "k"), all.x = TRUE)
  cc[is.na(O_own), `:=`(O_own = 0, N_own = 0)]
  cc[, p_hom := ifelse(N_all - N_own > 0, (O_all - O_own) / (N_all - N_own), NA_real_)]
  cc[, E := N * p_hom]
  cc[, c("ok", "O_all", "N_all", "O_own", "N_own") := NULL][]
}

#' Best-neighbour co-read ratio of each reader k per unit: max over neighbours j with E >= min_E of O / E, and the number
#' of informative neighbours. A failed antenna is low for every neighbour; an animal resting next to it lowers only the
#' adjacent pair. `expect`: aqc_homologous_expectation() output.
aqc_best_neighbour <- function(expect, by = c("board", "block"), min_E = AQC_MIN_PAIR_EXPECTATION) {
  expect[, .(informative_pairs = sum(E >= min_E, na.rm = TRUE),
             best_ratio = if (any(E >= min_E, na.rm = TRUE)) max((O / E)[E >= min_E], na.rm = TRUE) else NA_real_,
             O_total = sum(O), E_total = sum(E, na.rm = TRUE)),
         by = c(by, "k")][]
}

#' Silent stretches of one reader: runs of at least min_minutes consecutive minutes without a read. `reads_k`: reads
#' per minute at k; `log_q`: per minute, sum over neighbours j of n_jm * log(1 - p_hom(j,k)) (so the probability of no
#' co-read in the minute is exp(log_q)); `active`: whether the minute is in an active phase. Returns data.frame(first,
#' last (minute indices), minutes, active_minutes, E_minutes, E_active_minutes).
aqc_silent_stretches <- function(reads_k, log_q, active, min_minutes = AQC_STRETCH_MIN_MINUTES) {
  z <- reads_k == 0
  r <- rle(z)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  keep <- r$values & r$lengths >= min_minutes
  if (!any(keep)) return(data.frame(first = integer(), last = integer(), minutes = integer(), active_minutes = integer(),
                                    E_minutes = numeric(), E_active_minutes = numeric()))
  p_co <- 1 - exp(log_q)
  do.call(rbind, lapply(which(keep), function(q) {
    i <- starts[q]:ends[q]
    data.frame(first = starts[q], last = ends[q], minutes = length(i), active_minutes = sum(active[i]),
               E_minutes = sum(p_co[i]), E_active_minutes = sum(p_co[i][active[i]]))
  }))
}

#' Support-age class of seconds: (0,2], (2,10], (10,60], (60,300], >300 s since the last read; NA without a read yet.
aqc_age_class <- function(age, breaks = AQC_AGE_BREAKS) {
  lab <- c(paste0("le", breaks[1]), paste0(breaks[-length(breaks)], "_", breaks[-1]), paste0("gt", breaks[length(breaks)]))
  factor(cut(age, c(-Inf, breaks, Inf), labels = lab, right = TRUE), levels = lab)
}

#' Seconds since the last read at each grid midpoint (`mid`, same time origin as `read_t`); NA before the first read.
aqc_age_at <- function(mid, read_t) {
  read_t <- sort(read_t)
  i <- findInterval(mid, read_t)
  out <- rep(NA_real_, length(mid))
  ok <- i > 0
  out[ok] <- mid[ok] - read_t[i[ok]]
  out
}

#' Same-reader co-assignment per reader on a 1-s grid: `pos` is a seconds x animals integer matrix of assigned readers
#' (0 = none), `ok` a logical matrix of the seconds that count (e.g. supported). For each reader z: observed animal-pair
#' seconds both at z, and the expectation if the animals' positions were independent given their own shares of z over
#' the counted seconds (sum over pairs of p_a(z) p_b(z) x jointly counted seconds). Returns data.frame(PositionID,
#' pair_seconds, observed, expected).
aqc_coassign_by_reader <- function(pos, ok) {
  na <- ncol(pos)
  if (na < 2) return(data.frame(PositionID = integer(), pair_seconds = numeric(), observed = numeric(), expected = numeric()))
  res <- matrix(0, 8, 3)
  for (a in 1:(na - 1)) for (b in (a + 1):na) {
    j <- ok[, a] & ok[, b] & pos[, a] > 0 & pos[, b] > 0
    if (!any(j)) next
    pa <- tabulate(pos[j, a], 8) / sum(j); pb <- tabulate(pos[j, b], 8) / sum(j)
    obs <- tabulate(pos[j, a][pos[j, a] == pos[j, b]], 8)
    res[, 1] <- res[, 1] + sum(j); res[, 2] <- res[, 2] + obs; res[, 3] <- res[, 3] + pa * pb * sum(j)
  }
  data.frame(PositionID = 1:8, pair_seconds = res[, 1], observed = res[, 2], expected = res[, 3])
}

#' Tail silence of a board: the time from its last read to the span end, and the spread of its tags' last reads (a short
#' spread with all tags stopping fits removal of the animals rather than a board fault). `reads`: data.table(tag, t).
aqc_board_tail <- function(reads, span_end) {
  if (!nrow(reads)) return(data.frame(last_read = NA_real_, tail_silence_s = NA_real_, tags = 0L, last_read_spread_s = NA_real_))
  lr <- reads[, .(last = max(t)), by = tag]
  data.frame(last_read = max(lr$last), tail_silence_s = span_end - max(lr$last), tags = nrow(lr),
             last_read_spread_s = max(lr$last) - min(lr$last))
}

#' The rules catalogue: one row per check with its severity, level, threshold and the justification of the threshold.
aqc_rules <- function() {
  r <- function(check_id, severity, level, check, metric, comparator, threshold, unit, affects, rationale)
    data.frame(check_id, severity, level, check, metric, comparator, threshold = as.character(threshold), unit, affects, rationale,
               stringsAsFactors = FALSE)
  rbind(
    r("H01", "hard_failure", "recording", "antenna table: 40 devices on boards 1-5 at the documented grid positions",
      "devices_expected & devices_on_grid", "==", "TRUE", "", "all position measures",
      "a wrong mapping makes every reader-level quantity wrong; the documented grid is identical in all logs"),
    r("H02", "hard_failure", "recording", "read records are well formed: MsgValue1 holds one gap per read; single reads written as 75 ms",
      "reads_ne_offsets + single_read_dur_ne_75ms", "==", 0, "events", "read times, detection support",
      "the read times are rebuilt from these fields"),
    r("H03", "hard_failure", "recording", "no duplicate detection events (tag, antenna, start)", "duplicate_events", "==", 0, "events",
      "read counts, transitions", "a duplicated event double-counts reads and can create zero-length visits"),
    r("H04", "hard_failure", "recording", "every roster animal has one tag, is detected in the span and sits on the board of its v2 cage label",
      "roster_missing_tag + roster_undetected + board_label_mismatch", "==", 0, "animals", "everything for that animal",
      "missing or misassigned data"),
    r("H05", "hard_failure", "recording", "times are usable: finite, and no backward jump of the logger clock larger than 1,800 s remains after the daylight-saving fold is removed",
      "nonfinite_times + backward_jumps_gt1800s", "==", 0, "events", "all time-based measures",
      "unrecoverable timestamp corruption; the DST fold itself is removed by the reader and reported as W03"),
    r("H06", "hard_failure", "reader", "whole-session channel dropout: a reader with no read in the session while its board reads and its neighbours' traffic predicts >= 100 co-read tag-minutes",
      "reads == 0 & E_tagmin", ">=", AQC_DEAD_CHANNEL_MIN_E, "expected co-read tag-minutes",
      "reader and row occupancy, column occupancy (its column), dispersion, dominant share, co-assignment, reader changes (bypass jumps inflate >= 2-column and vendor displacement counts), fragmentation, detection-based activity",
      paste("tag-minutes, not reads, are the unit because reads of a resting tag cluster; 'confirmed' needs reads of the antenna in a placement scan",
            "next to the session and a median best co-read ratio >= 0.25 in its other recordings (healthy reader-sessions: 1st percentile 0.27).",
            "Below 100 expected tag-minutes a silent reader is undecidable. Provisional until calibrated (docs/RFID_CALIBRATION_PROTOCOL.md)")),
    r("W01", "warning", "recording", "multi-read event duration differs from its read span", "multi_read_dur_ne_read_span", ">", 0, "events",
      "event durations", "format inconsistency without loss of read times"),
    r("W02", "warning", "recording", "acquisition stall: a within-event read gap > 1.1 s or a zero gap",
      "stall_gaps + zero_gaps", ">", 0, "gaps", "read times near the stall (delivered late)",
      "events close after ~1 s without a read, so a longer within-event gap means late delivery"),
    r("W03", "warning", "recording", "daylight-saving fold: the repeated hour is dropped", "clock_fold_rows_dropped", ">", 0, "rows",
      "the dropped hour", "the hour is unobserved, not corrupted"),
    r("W04", "warning", "recording", "recording-wide silence >= 60 s inside the span", "recording_silences_ge60", ">", 0, "silences",
      "all measures in the silence", "no tag read anywhere; possible polling outage, cannot be confirmed from the log"),
    r("W05", "warning", "recording", "unpolled pause between acquisition sessions inside the span", "unpolled_pause_s_in_span", ">", 0, "s",
      "all measures in the pause", "explicit stop/start rows bound time without polling"),
    r("W06", "warning", "board", "an untracked tag (not in the roster) with >= 100 events is present in the cage",
      "nonroster_events", ">=", AQC_NONROSTER_MIN_EVENTS, "events",
      "co-assignment, read competition, cage density", "an animal is present that the analyses do not see"),
    r("W07", "warning", "board", "board silent from its last read to the span end for >= 1 h (removal signature: all tags stop within 10 min)",
      "tail_silence_s", ">=", AQC_TAIL_SILENCE_S, "s", "everything after the last read (carry-forward would fill it)",
      "a board fault cannot be told from removal of the animals; the time is unobserved either way"),
    r("W08", "warning", "reader", "intermittent silence: a stretch >= 60 min without reads while the tags present at the neighbours predict >= 200 active minutes with a co-read",
      "E_active_minutes", ">=", AQC_STRETCH_WARN_E, "minutes",
      "reader occupancy and transitions in the stretch",
      paste("expected minutes from tag-minutes at the neighbours and homologous co-read rates per tag-minute; only active minutes count, so",
            "silences inside light phases (resting) cannot trigger it. Provisional: compare the run's non-failure maximum and the whole-session",
            "dropouts' values (stretches.csv, dropout_session) and the model calibration (stretch_calibration.csv)")),
    r("W09", "warning", "reader_block", "low best-neighbour co-read ratio in an active block with >= 2 informative neighbour pairs",
      "best_ratio", "<", AQC_BEST_RATIO_WARN, "ratio",
      "reader occupancy in the block",
      "healthy active blocks had a minimum of 0.05 in the scoping scan; light blocks are only reported (a resting animal next to an antenna can make it 0)"),
    r("W10", "warning", "animal_block", "animal read far less than its cage-mates in the block", "reads / cage-mate median", "<", AQC_LOW_READS_REL, "ratio",
      "that animal's detection-based measures", "possible tag problem or a resting place out of range; compared within the cage, never across boards"),
    r("W11", "warning", "animal", "placement scan (initScan) board differs from the recording board", "initscan_board != board", "==", "TRUE", "",
      "cage assignment", "possible cage error or tag swap"),
    r("W12", "warning", "animal", "logged animal label differs from the tag map", "log_label_matches_tag_map", "==", "FALSE", "", "identity", "identity error"),
    r("W13", "warning", "animal", "detections on another board during the span", "events_off_board", ">", 0, "events", "cage assignment", "tag read outside its cage"),
    r("W14", "warning", "animal_block", "poor detection support (review rank): share of time with the carried position older than 300 s is in the top 1% of observed animal-blocks of the same phase",
      "age_gt300s_share", ">", paste0("q", AQC_UNUSUAL_QUANTILE, " of the same phase"), "share",
      "occupancy and co-assignment of that animal-block",
      "no external threshold exists; a relative rank for review (always about 1% of units; depends on the recordings in the run); unobserved blocks (after a board's last read, dropout boards) are left out; does not set the run status"),
    r("W15", "warning", "animal_block", "strong reconstruction sensitivity (review rank): share of seconds assigned to different readers by event-start and single-read carry-forward is in the top 1% of observed animal-blocks",
      "share_seconds_differ", ">", paste0("q", AQC_UNUSUAL_QUANTILE, " of the same phase"), "share",
      "reader-level occupancy of that animal-block", "as W14: a relative rank for review; does not set the run status"),
    r("W16", "warning", "board", "board acquisition signature differs from its own history (mean within-event read gap or lattice k1 share, |z| > 3 over the board's recordings)",
      "abs(z)", ">", 3, "z", "read-count and detection comparisons of that board-recording",
      "boards differ in hardware (stable across batches); a change against the same board's history is unexplained"),
    r("W17", "warning", "recording", "analysis span not covered by the raw log (first event later, or last read earlier, than the span bounds by more than 60 s)",
      "uncovered_s", ">", 60, "s", "all measures in the uncovered time", "an early stop or late start would otherwise shorten the span silently"),
    r("D01", "diagnostic", "reader", "resource-related occupancy and read asymmetry (R1 food, R5 house, R8 water)", "shares", "", "", "", "",
      "expected biology and physics; reported, never flagged"),
    r("D02", "diagnostic", "board", "board read and crosstalk level relative to the homologous boards", "ratios", "", "", "", "read-based measures",
      "boards 3 and 4 are less sensitive in every recording (hardware); a covariate, not an exclusion"),
    r("D03", "diagnostic", "animal_block", "carry-forward support (support-age classes), detected share, gaps", "shares", "", "", "", "",
      "continuous; no external threshold"),
    r("D04", "diagnostic", "animal_block", "reader switching: changes by geometry, short returns, changes before/after the 2-s flip filter", "counts", "", "", "", "",
      "measurement and behaviour are confounded; continuous"),
    r("D05", "diagnostic", "board_block", "event-start vs single-read reconstruction by reader and support age", "seconds", "", "", "", "",
      "basis sensitivity; continuous"),
    r("D06", "diagnostic", "board_block", "same-reader co-assignment observed vs independence expectation by reader", "ratio", "", "", "", "",
      "co-assignment and read competition influence each other; not evidence of separation or avoidance")
  )
}

#' Acquisition QC tables of one recording.
#'
#' vendor     v2 preprocessed file of the recording (data frame as read; roster, System label, phase calendar)
#' recording  rfid_read_recording(keep_read_offsets = TRUE)
#' tags       rfid_animalpos_tag_map() of the recording
#' sysmsg     amv_system_messages() of the recording's log
#' initscan   data.table(board, PositionID, reads_before, reads_after): antenna reads in the placement scan before the
#'            session (same cage change) and in the next cage change's placement scan (NA when there is none)
#' initscan_boards  data.table(tag, initscan_board) from antenna_dv_initscan_boards() (may be empty)
#' Returns a named list of data.tables; times are POSIX seconds.
aqc_process_recording <- function(vendor, recording, tags, sysmsg, initscan, initscan_boards, batch, cc, source_file) {
  cal <- antenna_dv_vendor_calendar(vendor)
  ev <- data.table::as.data.table(recording$events)
  ev[, `:=`(fo = file_order, e_log = t + dur_sec)]
  S0 <- cal$span_start; S1 <- min(cal$span_end, max(ev$e_log))
  roster <- unique(data.table::as.data.table(vendor)[, .(AnimalID, System)])[, AnimalNum := canonical_animal_id(AnimalID)]
  tags <- data.table::as.data.table(tags)
  ev <- merge(ev, tags, by = "tag", all.x = TRUE)
  ev[, in_span := t >= S0 & t < S1]
  own <- ev[in_span == TRUE, .N, by = .(tag, board)][order(tag, -N, board)][, .SD[1], by = tag][, .(tag, own_board = board)]
  ev <- merge(ev, own, by = "tag", all.x = TRUE)
  ev[, `:=`(roster = !is.na(AnimalNum) & AnimalNum %in% roster$AnimalNum, on_board = !is.na(own_board) & board == own_board)]
  data.table::setorder(ev, t, fo)
  rt <- amv_read_times(ev$t, ev$read_offsets_ms)
  i_ev <- rt$ev   # inside rt[...], `ev` is rt's column
  data.table::set(rt, j = c("tag", "board", "pid", "AnimalNum", "roster", "on_board"),
                  value = list(ev$tag[i_ev], ev$board[i_ev], ev$PositionID[i_ev], ev$AnimalNum[i_ev], ev$roster[i_ev], ev$on_board[i_ev]))
  rt[, wg := { d <- c(NA_real_, diff(t_read)); d[c(TRUE, ev[-1] != ev[-.N])] <- NA_real_; d }]
  span_read <- rt[, .(first = min(t_read), last = max(t_read)), by = ev]
  ev[, e := t]
  ev[span_read$ev, e := span_read$last]   # last read: the end used for detection support
  ev[, overlap := NA_integer_]
  ev[in_span == TRUE, overlap := aqc_board_overlap_counts(board, t, e_log)]
  key <- data.table::data.table(SourceFile = source_file, Batch = batch, CC = cc)
  blk_of <- function(t) animalpos_phase_block_index(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))
  blocks <- data.table::data.table(block = seq(blk_of(S0), blk_of(S1 - 1e-6)))
  blocks[, `:=`(bs = pmax(S0, block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC),
                be = pmin(S1, (block + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC))]
  blocks <- merge(blocks, data.table::as.data.table(cal$blocks)[, .(block = PhaseBlockIndex, Phase)], by = "block", all.x = TRUE)
  blocks[, block_no := seq_len(.N)]
  rt[, block := blk_of(t_read)]
  rt_span <- rt[t_read >= S0 & t_read < S1 & !is.na(board)]
  boards <- 1:5

  # recording-level integrity -------------------------------------------------------------------------------------------
  n_off <- tabulate(rt$ev, nbins = nrow(ev))
  multi <- ev$reads >= 2
  fo_order <- ev[order(fo)]
  sm <- data.table::as.data.table(sysmsg)
  starts <- sort(sm[SystemMsg == "start"]$t); ends <- sort(sm[SystemMsg == "end"]$t)
  pauses <- if (length(starts) > 1 && length(ends) >= length(starts) - 1) {
    data.table::data.table(s = ends[seq_len(length(starts) - 1)], e = starts[-1])
  } else data.table::data.table(s = numeric(), e = numeric())
  pause_in_span <- if (nrow(pauses)) sum(pmax(0, pmin(pauses$e, S1) - pmax(pauses$s, S0))) else 0
  dev <- aqc_device_check(recording$devices)
  maj <- ev[in_span == TRUE & roster == TRUE, .N, by = .(AnimalNum, board)][order(AnimalNum, -N, board)][, .SD[1], by = AnimalNum]
  maj <- merge(roster[, .(AnimalNum, System)], maj[, .(AnimalNum, board)], by = "AnimalNum", all.x = TRUE)
  sp <- ev[in_span == TRUE]
  rec_sil <- amv_gaps(amv_union(sp$t, sp$e_log), S0, S1, 60)
  recording_qc <- cbind(key, dev, data.table::data.table(
    raw_log = basename(recording$path), raw_logs_ignored = paste(recording$ignored, collapse = "|"),
    span_start = S0, span_end = S1, vendor_span_end = cal$span_end, first_event = min(ev$t), last_read = max(ev$e),
    span_covered_start = min(ev$t) <= S0 + 60, span_covered_end = max(ev$e_log) >= cal$span_end - 60,
    uncovered_s = max(0, min(ev$t) - S0) + max(0, cal$span_end - max(ev$e_log)),
    events = nrow(ev), events_in_span = nrow(sp), reads = sum(ev$reads),
    reads_ne_offsets = sum(ev$reads != n_off), single_read_dur_ne_75ms = sum(ev$reads == 1 & abs(ev$dur_sec - 0.075) > 1e-6),
    multi_read_dur_ne_read_span = sum(multi & abs(ev$dur_sec - (ev$e - ev$t)) > 0.002),
    duplicate_events = sum(duplicated(ev[, .(tag, unit, t)])),
    nonfinite_times = sum(!is.finite(ev$t)), backward_jumps_gt1s = sum(fo_order$e_log < cummax(fo_order$e_log) - 1),
    backward_jumps_gt1800s = sum(fo_order$e_log < cummax(fo_order$e_log) - 1800),
    clock_fold_rows_dropped = recording$clock_fold_rows_dropped,
    stall_gaps = sum(rt$wg > AQC_STALL_GAP_S, na.rm = TRUE), zero_gaps = sum(rt$wg == 0, na.rm = TRUE),
    max_within_event_gap_s = max(rt$wg, na.rm = TRUE),
    sessions = length(starts), unpolled_pause_s_in_span = pause_in_span,
    recording_silences_ge60 = nrow(rec_sil), recording_silence_s = sum(rec_sil$e - rec_sil$s),
    roster_animals = nrow(roster), roster_missing_tag = sum(!roster$AnimalNum %in% tags$AnimalNum),
    roster_undetected = sum(!roster$AnimalNum %in% ev$AnimalNum[ev$in_span]),
    board_label_mismatch = sum(is.na(maj$board) | paste0("sys.", maj$board) != maj$System),
    unknown_tag_events = sum(is.na(ev$AnimalNum)), unknown_tags = data.table::uniqueN(ev$tag[is.na(ev$AnimalNum)]),
    nonroster_tags_in_span = data.table::uniqueN(sp$tag[!sp$roster]), events_nonroster_in_span = sum(!sp$roster)))

  # board level ---------------------------------------------------------------------------------------------------------
  board_qc <- data.table::rbindlist(lapply(boards, function(b) {
    x <- sp[board == b]; r <- rt_span[board == b]
    sil <- amv_gaps(amv_union(x$t, x$e_log), S0, S1, 60)
    tail <- aqc_board_tail(r[, .(tag, t = t_read)], S1)
    iso_b <- x[overlap %in% 0]
    iso_idx <- which(ev$in_span & ev$board == b & ev$overlap %in% 0)
    sig <- aqc_lattice(rt[!is.na(wg) & ev %in% iso_idx]$wg * 1000)
    ms <- iso_b[reads >= 2, .(rate = sum(reads - 1) / sum(e - t))]$rate
    cbind(data.table::data.table(board = b, reads = nrow(r), roster_reads = sum(r$roster), events = nrow(x),
      tags_present = data.table::uniqueN(x$tag), nonroster_events = sum(!x$roster),
      nonroster_tags_ge100 = sum(x[roster == FALSE, .N, by = tag]$N >= AQC_NONROSTER_MIN_EVENTS),
      first_read_rel_h = if (nrow(r)) (min(r$t_read) - S0) / 3600 else NA_real_,
      silences_ge60 = nrow(sil), silence_s = sum(sil$e - sil$s), longest_silence_s = if (nrow(sil)) max(sil$e - sil$s) else 0,
      silent_share = sum(sil$e - sil$s) / (S1 - S0)),
      data.table::as.data.table(tail),
      data.table::data.table(isolated_events = nrow(iso_b), isolated_single_read_share = if (nrow(iso_b)) mean(iso_b$reads == 1) else NA_real_,
                             within_event_read_rate = if (length(ms) && is.finite(ms)) ms else NA_real_),
      data.table::as.data.table(sig))
  }))
  board_qc[, `:=`(removal_signature = !is.na(tail_silence_s) & tail_silence_s >= AQC_TAIL_SILENCE_S & last_read_spread_s <= AQC_REMOVAL_SPREAD_S,
                  reads_rel_recording_median = reads / stats::median(reads))]

  # co-reads, homologous expectations, best-neighbour ratios -----------------------------------------------------------
  flags <- aqc_coread_flags(rt_span[, .(tag, board, pid, t = t_read, block)])
  flags <- merge(flags, blocks[, .(block, Phase)], by = "block")
  flags[, minute := floor((t - S0) / 60) + 1L]
  cnt <- aqc_coread_counts(flags, by = c("board", "block", "Phase"))
  # tag-minutes: a tag read at reader j in a minute, and whether any of those reads was co-read at each neighbour. Reads
  # of a resting tag come in long clusters, so the tag-minute (not the read) is the unit of evidence for silent-reader tests.
  tm <- flags[, lapply(.SD, any), by = .(board, tag, minute, pid, Phase), .SDcols = paste0("co_R", 1:8)]
  rm(flags)
  allrd <- data.table::CJ(board = boards, PositionID = 1:8)
  allrd <- merge(allrd, rt_span[, .N, by = .(board, PositionID = pid)], by = c("board", "PositionID"), all.x = TRUE)[is.na(N), N := 0L]
  board_reads <- rt_span[, .(board_reads = .N), by = board]
  dead <- merge(allrd, board_reads, by = "board")[N == 0 & board_reads > 0, .(board, PositionID)]
  ph <- cnt[, .(O = sum(O), N = sum(N)), by = .(board, Phase, j, k, type)]
  ph <- aqc_homologous_expectation(ph, dead, group = "Phase")
  cnt <- merge(cnt, ph[, .(board, Phase, j, k, p_hom)], by = c("board", "Phase", "j", "k"), all.x = TRUE)
  cnt[, E := N * p_hom]
  bn <- aqc_best_neighbour(cnt, by = c("board", "block", "Phase"))
  data.table::setnames(bn, "k", "PositionID")
  sess <- cnt[, .(O_session = sum(O), E_session = sum(E, na.rm = TRUE)), by = .(board, PositionID = k)]
  sess_best <- aqc_best_neighbour(cnt[, .(O = sum(O), E = sum(E, na.rm = TRUE)), by = .(board, j, k)], by = "board")
  data.table::setnames(sess_best, c("k", "best_ratio", "informative_pairs"), c("PositionID", "session_best_ratio", "session_informative_pairs"))
  sess_best[, c("O_total", "E_total") := NULL]
  phm <- aqc_coread_counts(tm, by = c("board", "Phase"))
  phm <- aqc_homologous_expectation(phm, dead, group = "Phase")
  sess_tm <- phm[, .(O_tagmin = sum(O), E_tagmin = sum(E, na.rm = TRUE)), by = .(board, PositionID = k)]

  # minute grid: reads per reader, tags present per reader, and the probability of a co-read in each minute
  M <- ceiling((S1 - S0) / 60)
  rt_span[, minute := floor((t_read - S0) / 60) + 1L]
  min_phase <- blocks$Phase[match(blk_of(S0 + (seq_len(M) - 0.5) * 60), blocks$block)]
  active_min <- min_phase %in% "Active"
  q_tab <- phm[is.finite(p_hom), .(board, Phase, j, k, q = p_hom)]
  calib <- list()
  stretches <- data.table::rbindlist(lapply(boards, function(b) {
    nm <- matrix(0L, M, 8); nt <- matrix(0L, M, 8); co_obs <- matrix(FALSE, M, 8)
    cnts <- rt_span[board == b, .N, by = .(minute, pid)]
    if (nrow(cnts)) nm[cbind(cnts$minute, cnts$pid)] <- cnts$N
    tb <- tm[board == b]
    tcnt <- tb[, .N, by = .(minute, pid)]
    if (nrow(tcnt)) nt[cbind(tcnt$minute, tcnt$pid)] <- tcnt$N
    for (kk in 1:8) co_obs[unique(tb[get(paste0("co_R", kk)) == TRUE]$minute), kk] <- TRUE
    data.table::rbindlist(lapply(1:8, function(kk) {
      lq <- numeric(M)
      for (ph_name in c("Active", "Inactive")) {
        qk <- q_tab[board == b & Phase == ph_name & k == kk]
        rows <- which(min_phase %in% ph_name)
        for (q in seq_len(nrow(qk))) lq[rows] <- lq[rows] + nt[rows, qk$j[q]] * log1p(-min(qk$q[q], 1 - 1e-9))
      }
      is_dead <- nrow(dead[board == b & PositionID == kk]) > 0
      if (is_dead) {
        p_co <- 1 - exp(lq)
        return(data.table::data.table(board = b, PositionID = kk, first = 1L, last = M, minutes = M, active_minutes = sum(active_min),
                                      E_minutes = sum(p_co), E_active_minutes = sum(p_co[active_min]), dropout_session = TRUE))
      }
      cl <- data.table::data.table(Phase = min_phase, p = 1 - exp(lq), obs = co_obs[, kk])[!is.na(Phase)]
      calib[[length(calib) + 1L]] <<- cl[, .(minutes = .N, predicted = sum(p), observed = sum(obs)),
                                          by = .(Phase, bin = cut(p, c(-Inf, 0.1, 0.3, 0.5, 0.7, 0.9, 0.99, Inf)))][, `:=`(board = b, PositionID = kk)]
      s <- aqc_silent_stretches(nm[, kk], lq, active_min)
      if (!nrow(s)) return(NULL)
      data.table::data.table(board = b, PositionID = kk, s, dropout_session = FALSE)
    }))
  }), fill = TRUE)
  if (nrow(stretches)) {
    stretches[, `:=`(start = S0 + (first - 1) * 60, end = S0 + last * 60)]
    stretches[, light_only := active_minutes == 0]   # entirely inside light phases
  }
  stretch_calibration <- data.table::rbindlist(calib)

  # reader level --------------------------------------------------------------------------------------------------------
  ev_reader <- sp[, .(events = .N, single_read_share = mean(reads == 1), roster_events = sum(roster)), by = .(board, PositionID)]
  gaps_reader <- rt_span[!is.na(wg), .(mean_within_event_gap_ms = mean(wg) * 1000), by = .(board, PositionID = pid)]
  reader_qc <- Reduce(function(a, b) merge(a, b, by = c("board", "PositionID"), all.x = TRUE),
                      list(allrd[, .(board, PositionID, reads = N)], ev_reader, gaps_reader, sess, sess_tm, sess_best,
                           data.table::as.data.table(initscan)))
  reader_qc[, zero_read_session := reads == 0 & board %in% board_reads[board_reads > 0]$board]
  reader_qc[, crosstalk_in_ratio := ifelse(E_session > 0, O_session / E_session, NA_real_)]
  hom <- reader_qc[zero_read_session == FALSE, .(reads_hom_median = as.numeric(stats::median(reads))), by = PositionID]
  reader_qc <- merge(reader_qc, hom, by = "PositionID", all.x = TRUE)
  reader_qc[, reads_rel_homologous := reads / reads_hom_median]

  # animal level --------------------------------------------------------------------------------------------------------
  ncell <- floor(S1 - S0); mid <- seq(0.5, ncell, by = 1)
  sec_block <- blk_of(S0 + mid)
  ab <- list(); arb <- list(); bx <- list(); trans <- list()
  pos_e2 <- list(); age_m <- list(); board_of <- c()
  for (a in roster$AnimalNum) {
    x <- ev[AnimalNum == a & on_board == TRUE]
    r <- rt[AnimalNum == a & on_board == TRUE, .(t = t_read, PositionID = pid)]
    data.table::setorder(r, t)
    b_own <- x$own_board[1]; board_of[a] <- b_own
    det <- amv_union(x$t, x$e)
    tr <- amv_transitions(x$t, x$e, x$PositionID)
    tr[, block := blk_of(t)]
    tr <- tr[t >= S0 & t < S1]
    full <- data.table::data.table(s = 0, e = ncell)
    g_e0 <- amv_grid_track(amv_runs(x$t, x$PositionID, S0, S1, 0), full, ncell)$pos
    g_e2 <- amv_grid_track(amv_runs(x$t, x$PositionID, S0, S1, 2), full, ncell)$pos
    g_r0 <- amv_grid_track(amv_runs(r$t, r$PositionID, S0, S1, 0), full, ncell)$pos
    g_r2 <- amv_grid_track(amv_runs(r$t, r$PositionID, S0, S1, 2), full, ncell)$pos
    age <- aqc_age_at(S0 + mid, r$t)
    pos_e2[[a]] <- g_e2; age_m[[a]] <- age
    d <- data.table::data.table(block = sec_block, e0 = g_e0, e2 = g_e2, r0 = g_r0, r2 = g_r2, age = age, acls = aqc_age_class(age))
    for (k in seq_len(nrow(blocks))) {
      bk <- blocks$block[k]; bs <- blocks$bs[k]; be <- blocks$be[k]
      db <- d[block == bk]
      rb <- r[t >= bs & t < be]; xb <- x[t >= bs & t < be]; tb <- tr[block == bk]
      g60 <- amv_gaps(det, bs, be, 60)
      geo <- table(factor(tb$geometry, levels = AMV_GEOMETRY_LEVELS))
      both2 <- db$e2 > 0 & db$r2 > 0; both0 <- db$e0 > 0 & db$r0 > 0
      unsup <- function(bound) mean(is.na(db$age) | db$age > bound)
      chg <- function(v) { v <- v[v > 0]; if (length(v) < 2) 0L else sum(v[-1] != v[-length(v)]) }
      ab[[length(ab) + 1L]] <- data.table::data.table(
        AnimalNum = a, board = b_own, block = bk, block_no = blocks$block_no[k], Phase = blocks$Phase[k], block_s = be - bs,
        reads = nrow(rb), events = nrow(xb), single_read_events = sum(xb$reads == 1),
        detected_s = amv_overlap_seconds(det, data.table::data.table(s = bs, e = be)),
        no_read_yet_share = mean(is.na(db$age)), age_gt2s_share = unsup(2), age_gt10s_share = unsup(10),
        age_gt60s_share = unsup(60), age_gt300s_share = unsup(300),
        gaps_ge60 = nrow(g60), gaps_ge60_s = sum(g60$e - g60$s), longest_gap_s = if (nrow(g60)) max(g60$e - g60$s) else 0,
        changes_raw = nrow(tb), t(stats::setNames(as.numeric(geo), paste0("changes_", AMV_GEOMETRY_LEVELS))),
        changes_simultaneous = sum(tb$gap_s < 0), returns_lt2s = sum(tb$returns %in% TRUE & tb$to_dur_s < 2),
        returns_lt10s = sum(tb$returns %in% TRUE & tb$to_dur_s < 10),
        changes_event_tau2 = chg(db$e2), changes_read_tau0 = chg(db$r0), changes_read_tau2 = chg(db$r2),
        share_seconds_differ_tau2 = if (any(both2)) mean(db$e2[both2] != db$r2[both2]) else NA_real_,
        share_column_differs_tau2 = if (any(both2)) mean(amv_column(db$e2[both2]) != amv_column(db$r2[both2])) else NA_real_,
        share_seconds_differ_tau0 = if (any(both0)) mean(db$e0[both0] != db$r0[both0]) else NA_real_)
      ok <- !is.na(db$age)
      arb[[length(arb) + 1L]] <- data.table::data.table(AnimalNum = a, board = b_own, block = bk, Phase = blocks$Phase[k], PositionID = 1:8,
        locf_event_s = tabulate(db$e2, 8), supp300_event_s = tabulate(db$e2[ok & db$age <= 300], 8),
        supp60_event_s = tabulate(db$e2[ok & db$age <= 60], 8), supp10_event_s = tabulate(db$e2[ok & db$age <= 10], 8),
        locf_read_s = tabulate(db$r2, 8), supp60_read_s = tabulate(db$r2[ok & db$age <= 60], 8),
        reads = tabulate(rb$PositionID, 8))
      bx[[length(bx) + 1L]] <- db[e2 > 0 & r2 > 0, .(seconds = .N), by = .(event_pos = e2, read_pos = r2, age_class = acls)][
        , `:=`(AnimalNum = a, board = b_own, block = bk, Phase = blocks$Phase[k])]
      trans[[length(trans) + 1L]] <- tb[, .N, by = .(from, to)][, `:=`(board = b_own, block = bk)]
    }
  }
  addkey <- function(d) cbind(key[rep(1, nrow(d))], d)
  animal_block <- addkey(data.table::rbindlist(ab))
  animal_block[, cagemate_median_reads := vapply(seq_len(.N), function(i) if (.N > 1) stats::median(reads[-i]) else NA_real_, 0),
               by = .(board, block)]
  animal_block[, reads_rel_cagemates := reads / cagemate_median_reads]
  animal_reader_block <- addkey(data.table::rbindlist(arb))
  basis_crosstab <- addkey(data.table::rbindlist(bx))

  # changes that go around each reader, relative to the homologous boards
  trans <- data.table::rbindlist(trans)
  tot <- trans[, .(changes = sum(N)), by = .(board, block)]
  sk <- data.table::rbindlist(lapply(seq_len(nrow(AQC_SKIP_PAIRS)), function(q) {
    kk <- AQC_SKIP_PAIRS$PositionID[q]; a <- AQC_SKIP_PAIRS$a[q]; b <- AQC_SKIP_PAIRS$b[q]
    trans[(from == a & to == b) | (from == b & to == a), .(skip = sum(N)), by = .(board, block)][, PositionID := kk]
  }))
  sk <- merge(data.table::CJ(board = boards, block = blocks$block, PositionID = AQC_SKIP_PAIRS$PositionID), sk, by = c("board", "block", "PositionID"), all.x = TRUE)
  sk[is.na(skip), skip := 0]
  sk <- merge(sk, tot, by = c("board", "block"), all.x = TRUE)
  sk[, skip_share := ifelse(!is.na(changes) & changes > 0, skip / changes, NA_real_)]
  sk[, skip_rel_homologous := vapply(seq_len(.N), function(i) skip_share[i] / stats::median(skip_share[-i], na.rm = TRUE), 0),
     by = .(block, PositionID)]
  reader_block_qc <- merge(bn, sk[, .(board, block, PositionID, skip, changes, skip_share, skip_rel_homologous)],
                           by = c("board", "block", "PositionID"), all = TRUE)
  rb_reads <- rt_span[, .(reads = .N), by = .(board, block, PositionID = pid)]
  reader_block_qc <- merge(reader_block_qc, rb_reads, by = c("board", "block", "PositionID"), all.x = TRUE)
  reader_block_qc[is.na(reads), reads := 0L]
  reader_block_qc <- merge(reader_block_qc, rt_span[, .(board_block_reads = .N), by = .(board, block)], by = c("board", "block"), all.x = TRUE)
  reader_block_qc[, Phase := blocks$Phase[match(block, blocks$block)]]
  zb <- reader_block_qc[, .(zero_read_blocks = sum(reads == 0 & board_block_reads > 0, na.rm = TRUE)), by = .(board, PositionID)]
  reader_qc <- merge(reader_qc, zb, by = c("board", "PositionID"), all.x = TRUE)
  reader_qc <- merge(reader_qc, data.table::as.data.table(AQC_LAYOUT)[, .(PositionID, reader, column, resource)], by = "PositionID")

  # same-reader co-assignment by reader (event basis, tau 2): all assigned seconds, and supported (<= 60 s) seconds only
  co <- list()
  for (b in boards) {
    ids <- names(board_of)[board_of %in% b]
    if (length(ids) < 2) next
    P <- do.call(cbind, pos_e2[ids]); A <- do.call(cbind, age_m[ids])
    for (k in seq_len(nrow(blocks))) {
      i <- which(sec_block == blocks$block[k])
      Pi <- P[i, , drop = FALSE]; Ai <- A[i, , drop = FALSE]
      all_ok <- Pi > 0
      s60 <- all_ok & !is.na(Ai) & Ai <= 60
      co[[length(co) + 1L]] <- rbind(
        data.table::data.table(aqc_coassign_by_reader(Pi, all_ok), seconds = "all"),
        data.table::data.table(aqc_coassign_by_reader(Pi, s60), seconds = "supported_le60s"))[
          , `:=`(board = b, block = blocks$block[k], Phase = blocks$Phase[k], animals = length(ids))]
    }
  }
  coassign <- addkey(data.table::rbindlist(co))

  # animal level (identity and placement)
  lab <- ev[roster == TRUE & !(label %in% c("", "unknown")), .(labels = paste(sort(unique(label)), collapse = "|")), by = AnimalNum]
  animal_qc <- merge(maj[, .(AnimalNum, System, board)], lab, by = "AnimalNum", all.x = TRUE)
  animal_qc <- merge(animal_qc, tags[, .(AnimalNum, tag)], by = "AnimalNum", all.x = TRUE)
  ib <- data.table::as.data.table(initscan_boards)
  if (!nrow(ib)) ib <- data.table::data.table(tag = character(), initscan_board = integer())
  animal_qc <- merge(animal_qc, ib[, .(tag, initscan_board)], by = "tag", all.x = TRUE)
  offb <- ev[in_span == TRUE & roster == TRUE & on_board == FALSE, .(events_off_board = .N), by = AnimalNum]
  animal_qc <- merge(animal_qc, offb, by = "AnimalNum", all.x = TRUE)
  animal_qc[is.na(events_off_board), events_off_board := 0L]
  strip_or <- function(z) sub("^OR(?=[0-9]+$)", "", z, perl = TRUE)
  animal_qc[, log_label_matches_tag_map := vapply(seq_len(.N), function(i) if (is.na(labels[i])) NA else
    all(strip_or(rfid_label_animal(strsplit(labels[i], "|", fixed = TRUE)[[1]])) == strip_or(AnimalNum[i])), logical(1))]
  animal_qc[, `:=`(board_matches_label = !is.na(board) & paste0("sys.", board) == System,
                   initscan_matches_board = ifelse(is.na(initscan_board), NA, initscan_board == board))]
  seed <- rt[roster == TRUE & on_board == TRUE & t_read < S0, .(last_before_span = max(t_read)), by = AnimalNum]
  animal_qc <- merge(animal_qc, seed, by = "AnimalNum", all.x = TRUE)
  animal_qc[, seed_age_s := S0 - last_before_span][, last_before_span := NULL]

  nonroster <- sp[roster == FALSE, .(events = .N, reads = sum(reads), board = as.integer(names(which.max(table(board))))), by = .(tag, AnimalNum)]
  silences <- rbind(rec_sil[, .(level = "recording", board = NA_integer_, s, e)],
                    data.table::rbindlist(lapply(boards, function(b) { x <- sp[board == b]
                      amv_gaps(amv_union(x$t, x$e_log), S0, S1, 60)[, .(level = "board", board = b, s, e)] })))
  silences[, dur_s := e - s]
  list(recording_qc = recording_qc, board_qc = addkey(board_qc), reader_qc = addkey(reader_qc), reader_block_qc = addkey(reader_block_qc),
       stretches = addkey(stretches), animal_qc = addkey(animal_qc), animal_block_qc = animal_block, animal_reader_block = animal_reader_block,
       basis_crosstab = basis_crosstab, coassign_by_reader = coassign, nonroster_tags = addkey(nonroster), silences = addkey(silences),
       stretch_calibration = addkey(stretch_calibration),
       system_messages = addkey(sm), blocks = addkey(blocks))
}

#' Reads per board and reader in placement-scan (initScan) logs; zero for antennas without reads, nothing for files
#' without detections. Returns data.table(board, PositionID, reads).
aqc_initscan_antenna_reads <- function(paths) {
  out <- data.table::CJ(board = 1:5, PositionID = 1:8)[, reads := 0L]
  for (p in paths) {
    lg <- tryCatch(rfid_read_raw_log(p), error = function(e) NULL)
    if (is.null(lg) || !nrow(lg$events)) next
    n <- data.table::as.data.table(lg$events)[, .(n = sum(reads)), by = .(board, PositionID)]
    out <- merge(out, n, by = c("board", "PositionID"), all.x = TRUE)
    out[!is.na(n), reads := reads + as.integer(n)][, n := NULL]
  }
  out[]
}

# The frozen Stage 29 S13 hardware exclusion (Functions/behavior_analysis_config.R, Analysis/29:331): an annotation only;
# QC never changes it.
AQC_FROZEN_S13 <- data.frame(AnimalNum = c("314", "318", "OR620", "OR630", "OR112", "OR141"), CC = c(rep("CC4", 4), "CC3", "CC3"),
                             cage = c(rep("B5|sys.1|CC4", 4), "B2|sys.2|CC3", "B2|sys.2|CC3"), stringsAsFactors = FALSE)

AQC_FLAG_COLUMNS <- c("check_id", "severity", "level", "SourceFile", "Batch", "CC", "board", "PositionID", "reader", "resource",
                      "AnimalNum", "block", "Phase", "value", "threshold", "classification", "confidence", "evidence",
                      "metrics_compromised", "proposed_action")
AQC_MANIFEST_COLUMNS <- c("rules_version", "check_id", "severity", "level", "check", "metric", "comparator", "threshold", "unit",
                          "affects", "rationale", "status", "n_checked", "n_flagged", "detail")

#' Evaluates the rules on the combined QC tables of all recordings (each element of `tab` the row-bound tables of
#' aqc_process_recording()). Group-blind: reads no group, CombZ or outcome. Returns list(flags, manifest, exclusions,
#' board_history).
aqc_evaluate <- function(tab, rules = aqc_rules()) {
  L <- data.table::as.data.table(AQC_LAYOUT)[, .(PositionID, reader, resource)]
  fl <- list()
  add <- function(d) if (nrow(d)) fl[[length(fl) + 1L]] <<- d
  mk <- function(d, check_id, value, threshold, classification, confidence, evidence, action) {
    rl <- rules[rules$check_id == check_id, ]
    if (!nrow(d)) return(invisible(NULL))
    out <- data.table::data.table(check_id = check_id, severity = rl$severity, level = rl$level,
      SourceFile = d$SourceFile, Batch = d$Batch, CC = d$CC,
      board = if ("board" %in% names(d)) as.integer(d$board) else NA_integer_,
      PositionID = if ("PositionID" %in% names(d)) as.integer(d$PositionID) else NA_integer_,
      AnimalNum = if ("AnimalNum" %in% names(d)) as.character(d$AnimalNum) else NA_character_,
      block = if ("block" %in% names(d)) as.numeric(d$block) else NA_real_,
      Phase = if ("Phase" %in% names(d)) as.character(d$Phase) else NA_character_,
      value = as.character(value), threshold = as.character(threshold), classification = classification, confidence = confidence,
      evidence = evidence, metrics_compromised = rl$affects, proposed_action = action)
    out <- merge(out, L, by = "PositionID", all.x = TRUE, sort = FALSE)
    add(out[, ..AQC_FLAG_COLUMNS])
  }
  R <- tab$recording_qc; B <- tab$board_qc; Q <- tab$reader_qc; QB <- tab$reader_block_qc; ST <- tab$stretches
  A <- tab$animal_qc; AB <- tab$animal_block_qc
  none <- "none: review"

  # hard failures
  d <- R[!(devices_expected & devices_on_grid)]
  mk(d, "H01", paste(d$devices, d$boards), "40 devices, boards 1-5, documented grid", "invalid mapping", "high", "antenna table", "stop: fix the mapping first")
  d <- R[reads_ne_offsets + single_read_dur_ne_75ms > 0]
  mk(d, "H02", d$reads_ne_offsets + d$single_read_dur_ne_75ms, 0, "malformed read records", "high", "MsgValue1 / eventDuration", "stop")
  d <- R[duplicate_events > 0]
  mk(d, "H03", d$duplicate_events, 0, "duplicate events", "high", "tag, antenna, start", "stop: deduplicate")
  d <- R[roster_missing_tag + roster_undetected + board_label_mismatch > 0]
  mk(d, "H04", paste(d$roster_missing_tag, d$roster_undetected, d$board_label_mismatch, sep = "/"), 0, "missing or misassigned animal data", "high",
     "missing tag / undetected / board differs from the v2 label", "stop for the animals concerned")
  d <- R[nonfinite_times + backward_jumps_gt1800s > 0]
  mk(d, "H05", d$nonfinite_times + d$backward_jumps_gt1800s, 0, "unrecoverable timestamps", "high", "file-order end times", "stop")

  # H06: whole-session channel dropouts, with the evidence from other recordings and the placement scans
  dead <- Q[zero_read_session == TRUE]
  if (nrow(dead)) {
    hist <- Q[zero_read_session == FALSE, .(other_recordings = .N,
                                            other_best_ratio_min = if (all(is.na(session_best_ratio))) NA_real_ else min(session_best_ratio, na.rm = TRUE),
                                            other_best_ratio_median = if (all(is.na(session_best_ratio))) NA_real_ else stats::median(session_best_ratio, na.rm = TRUE),
                                            other_reads_rel_hom_median = stats::median(reads_rel_homologous, na.rm = TRUE)), by = .(board, PositionID)]
    dead <- merge(dead, hist, by = c("board", "PositionID"), all.x = TRUE)
    animals <- A[, .(animals = paste(sort(AnimalNum), collapse = ",")), by = .(SourceFile, board)]
    dead <- merge(dead, animals, by = c("SourceFile", "board"), all.x = TRUE)
    dead[, testable := E_tagmin >= AQC_DEAD_CHANNEL_MIN_E]
    dead[, bracketed := (reads_before %in% NA | reads_before > 0) & (reads_after %in% NA | reads_after > 0) & !(reads_before %in% NA & reads_after %in% NA)]
    # confirmed: the antenna read in a placement scan next to the session and co-read normally in other recordings
    dead[, healthy_elsewhere := (other_best_ratio_median >= AQC_HEALTHY_ELSEWHERE) %in% TRUE]
    dead[, classification := ifelse(!testable, "silent reader, undecidable (neighbour traffic too low to expect co-reads)",
      ifelse(bracketed & healthy_elsewhere, "confirmed acquisition failure: whole-session channel dropout",
             "probable acquisition failure: whole-session channel dropout"))]
    dead[, confidence := ifelse(!testable, "low", ifelse(bracketed & healthy_elsewhere, "high", "moderate"))]
    dead[, evidence := sprintf(paste("0 reads in the session while the board read; neighbours predict %.0f co-read tag-minutes (%.0f co-reads) at",
      "homologous rates; placement scan before: %s reads, next placement scan: %s; the same antenna in %s other recordings: best co-read",
      "ratio median %.2f (min %.2f), reads %.2f x the homologous median; animals on the board: %s"),
      E_tagmin, E_session, ifelse(is.na(reads_before), "none", reads_before), ifelse(is.na(reads_after), "none", paste(reads_after, "reads")),
      ifelse(is.na(other_recordings), 0L, other_recordings), other_best_ratio_median, other_best_ratio_min, other_reads_rel_hom_median, animals)]
    dead[, action := ifelse(testable, paste("exclude the board-recording from spatial measures needing complete reader coverage;",
      "keep non-spatial and detection-independent measures; frozen S13 unchanged"), none)]
    mk(dead[testable == TRUE], "H06", round(dead[testable == TRUE]$E_tagmin), AQC_DEAD_CHANNEL_MIN_E,
       dead[testable == TRUE]$classification, dead[testable == TRUE]$confidence, dead[testable == TRUE]$evidence, dead[testable == TRUE]$action)
  }

  # warnings
  d <- R[multi_read_dur_ne_read_span > 0]
  mk(d, "W01", d$multi_read_dur_ne_read_span, 0, "format inconsistency", "high", "duration vs read span", "none: read times unaffected")
  d <- R[stall_gaps + zero_gaps > 0]
  mk(d, "W02", paste(d$stall_gaps, d$zero_gaps, sep = "/"), 0, "acquisition stall (late delivery)", "high",
     sprintf("within-event gaps > %.1f s / zero gaps; max within-event gap %.2f s", AQC_STALL_GAP_S, d$max_within_event_gap_s),
     "read times near the stall are delivery times")
  d <- R[clock_fold_rows_dropped > 0]
  mk(d, "W03", d$clock_fold_rows_dropped, 0, "daylight-saving fold removed", "high", "repeated hour dropped by the reader", "treat the hour as unobserved")
  d <- R[recording_silences_ge60 > 0]
  mk(d, "W04", round(d$recording_silence_s), 60, "recording-wide silence", "low", "no tag read on any board", "none: review")
  d <- R[unpolled_pause_s_in_span > 0]
  mk(d, "W05", round(d$unpolled_pause_s_in_span), 0, "unpolled pause between sessions", "high", "stop/start rows", "treat the pause as unobserved")
  if ("span_covered_start" %in% names(R)) {
    d <- R[!(span_covered_start & span_covered_end)]
    mk(d, "W17", round(d$uncovered_s), 60, "analysis span not covered by the raw log", "high",
       "first event after the span start or last read before the span end (more than 60 s)", "treat the uncovered time as unobserved")
  }
  d <- B[nonroster_tags_ge100 > 0]
  mk(d, "W06", d$nonroster_events, AQC_NONROSTER_MIN_EVENTS, "untracked tag present", "high", "tags read in the cage that are not in the roster",
     "report cage density; co-assignment and read competition include an unseen animal")
  d <- B[!is.na(tail_silence_s) & tail_silence_s >= AQC_TAIL_SILENCE_S]
  mk(d, "W07", round(d$tail_silence_s), AQC_TAIL_SILENCE_S,
     ifelse(d$removal_signature, "probable removal of the animals (all tags stop within 10 min)", "board silent to the span end (fault or animals off the antennas; undecidable)"),
     ifelse(d$removal_signature, "moderate", "low"),
     sprintf("last read %s; %d tags; their last reads within %.0f s", format(as.POSIXct(d$last_read, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M", tz = "UTC"),
             d$tags, d$last_read_spread_s),
     "treat the time after the last read as unobserved (no carry-forward)")
  if (nrow(ST)) {
    d <- ST[dropout_session == FALSE & E_active_minutes >= AQC_STRETCH_WARN_E]
    mk(d, "W08", round(d$E_active_minutes), AQC_STRETCH_WARN_E, "intermittent silence", "moderate",
       sprintf("%d min without reads (%d active)", d$minutes, d$active_minutes), "review; exclude the stretch from reader-level measures if confirmed")
  }
  dead_keys <- paste(Q[zero_read_session == TRUE]$SourceFile, Q[zero_read_session == TRUE]$board, Q[zero_read_session == TRUE]$PositionID)
  d <- QB[Phase == "Active" & informative_pairs >= 2 & best_ratio < AQC_BEST_RATIO_WARN & !(paste(SourceFile, board, PositionID) %in% dead_keys)]
  mk(d, "W09", signif(d$best_ratio, 3), AQC_BEST_RATIO_WARN, "low co-read ratio in an active block", "moderate",
     sprintf("%d informative neighbour pairs; %d reads", d$informative_pairs, d$reads), "review")
  d <- AB[!is.na(reads_rel_cagemates) & reads_rel_cagemates < AQC_LOW_READS_REL]
  mk(d, "W10", signif(d$reads_rel_cagemates, 3), AQC_LOW_READS_REL, "animal read far less than its cage-mates", "low",
     sprintf("%d reads; cage-mate median %.0f", d$reads, d$cagemate_median_reads), "review the animal-block")
  d <- A[initscan_matches_board %in% FALSE]
  mk(d, "W11", d$initscan_board, d$board, "placement scan board differs", "moderate", "initScan majority board vs recording board", "review cage assignment")
  d <- A[log_label_matches_tag_map %in% FALSE]
  mk(d, "W12", d$labels, d$AnimalNum, "label differs from tag map", "high", "IdLabel vs AnimalPos tag map", "review identity")
  d <- A[events_off_board > 0]
  mk(d, "W13", d$events_off_board, 0, "reads off the home board", "moderate", "events on another board in the span", "review")
  # W14 and W15 rank observed animal-blocks only: blocks reaching past a board's last read (tail >= 1 h) and boards with a
  # whole-session dropout are known to be unobserved or incomplete and are left out of the reference distribution
  AB <- data.table::copy(AB)
  tails <- B[!is.na(tail_silence_s) & tail_silence_s >= AQC_TAIL_SILENCE_S, .(SourceFile, board, last_read)]
  AB[, observed := TRUE]
  if (nrow(tails)) {
    AB <- merge(AB, tails, by = c("SourceFile", "board"), all.x = TRUE)
    AB[!is.na(last_read) & ((block + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC) > last_read, observed := FALSE]
    AB[, last_read := NULL]
  }
  h6 <- if (length(fl)) data.table::rbindlist(fl, use.names = TRUE)[check_id == "H06"] else data.table::data.table(SourceFile = character(), board = integer())
  AB[paste(SourceFile, board) %in% paste(h6$SourceFile, h6$board), observed := FALSE]
  q14 <- AB[observed == TRUE, .(q = stats::quantile(age_gt300s_share, AQC_UNUSUAL_QUANTILE, na.rm = TRUE)), by = Phase]
  d <- merge(AB[observed == TRUE], q14, by = "Phase")[age_gt300s_share > q]
  mk(d, "W14", signif(d$age_gt300s_share, 3), signif(d$q, 3), "poor detection support", "low",
     sprintf("share of time with the position older than 300 s; %d reads in the block", d$reads), "report the support; sensitivity with bounded carry-forward")
  q15 <- AB[observed == TRUE, .(q = stats::quantile(share_seconds_differ_tau2, AQC_UNUSUAL_QUANTILE, na.rm = TRUE)), by = Phase]
  d <- merge(AB[observed == TRUE], q15, by = "Phase")[!is.na(share_seconds_differ_tau2) & share_seconds_differ_tau2 > q]
  mk(d, "W15", signif(d$share_seconds_differ_tau2, 3), signif(d$q, 3), "strong reconstruction sensitivity", "low",
     sprintf("column differs in %.3f of seconds", d$share_column_differs_tau2), "report both reconstructions")

  # W16: board acquisition signature against the same board's other recordings (leave one out)
  bh <- B[, .(SourceFile, Batch, CC, board, mean_gap_ms, lattice_k1)]
  bh[, `:=`(z_gap = vapply(seq_len(.N), function(i) (mean_gap_ms[i] - mean(mean_gap_ms[-i])) / stats::sd(mean_gap_ms[-i]), 0),
            z_k1 = vapply(seq_len(.N), function(i) (lattice_k1[i] - mean(lattice_k1[-i])) / stats::sd(lattice_k1[-i]), 0)), by = board]
  d <- bh[abs(z_gap) > 3 | abs(z_k1) > 3]
  mk(d, "W16", sprintf("z_gap %.1f, z_k1 %.1f", d$z_gap, d$z_k1), 3, "board signature differs from its history", "low",
     sprintf("mean within-event gap %.0f ms, lattice k1 share %.3f", d$mean_gap_ms, d$lattice_k1), "review; compare with any acquisition change")

  flags <- if (length(fl)) data.table::rbindlist(fl, use.names = TRUE) else
    data.table::as.data.table(stats::setNames(replicate(length(AQC_FLAG_COLUMNS), character(), simplify = FALSE), AQC_FLAG_COLUMNS))
  flags[, in_frozen_S13 := !is.na(AnimalNum) & paste(AnimalNum, CC) %in% paste(AQC_FROZEN_S13$AnimalNum, AQC_FROZEN_S13$CC)]

  # manifest: one row per check
  n_checked <- c(H01 = nrow(R), H02 = nrow(R), H03 = nrow(R), H04 = nrow(R), H05 = nrow(R), H06 = nrow(Q),
                 W01 = nrow(R), W02 = nrow(R), W03 = nrow(R), W04 = nrow(R), W05 = nrow(R), W06 = nrow(B), W07 = nrow(B),
                 W08 = if (nrow(ST)) sum(!ST$dropout_session) else 0L, W09 = nrow(QB[Phase == "Active" & informative_pairs >= 2]), W10 = sum(!is.na(AB$reads_rel_cagemates)),
                 W11 = sum(!is.na(A$initscan_matches_board)), W12 = sum(!is.na(A$log_label_matches_tag_map)), W13 = nrow(A),
                 W14 = sum(AB$observed), W15 = sum(AB$observed & !is.na(AB$share_seconds_differ_tau2)), W16 = nrow(B),
                 W17 = if ("span_covered_start" %in% names(R)) nrow(R) else 0L,
                 D01 = nrow(Q), D02 = nrow(B), D03 = nrow(AB), D04 = nrow(AB), D05 = nrow(tab$basis_crosstab), D06 = nrow(tab$coassign_by_reader))
  man <- data.table::as.data.table(rules)
  man[, n_checked := as.integer(n_checked[check_id])]
  nf <- flags[, .N, by = check_id]
  man[, n_flagged := ifelse(check_id %in% nf$check_id, nf$N[match(check_id, nf$check_id)], 0L)]
  man[, status := ifelse(severity == "diagnostic", "info", ifelse(n_checked == 0, "not_evaluated", ifelse(n_flagged > 0, "flagged", "pass")))]
  man[, `:=`(rules_version = AQC_RULES_VERSION, detail = "")]
  untestable <- Q[zero_read_session == TRUE & E_tagmin < AQC_DEAD_CHANNEL_MIN_E]
  if (nrow(untestable)) {
    man[check_id == "H06", detail := paste("silent but undecidable:", paste(untestable$SourceFile, untestable$board, untestable$PositionID, collapse = "; "))]
    man[check_id == "H06" & status == "pass", status := "undecidable"]
  }

  # proposed exclusions (never applied by the QC)
  ex <- list()
  h6 <- flags[check_id == "H06"]
  if (nrow(h6)) ex[[1]] <- h6[, .(SourceFile, Batch, CC, board, scope = "spatial measures needing complete reader coverage (reader, row and column occupancy, dispersion, dominant share, co-assignment, reader changes, displacement counts)",
                                  from = "span start", to = "span end", reason = paste("H06", classification), confidence)]
  w17 <- flags[check_id == "W17"]
  if (nrow(w17)) ex[[3]] <- w17[, .(SourceFile, Batch, CC, board = NA_integer_, scope = "all measures in the uncovered time",
                                    from = "span start or last read", to = "first event or span end", reason = "W17 span not covered", confidence)]
  w7 <- flags[check_id == "W07"]
  if (nrow(w7)) ex[[2]] <- w7[, .(SourceFile, Batch, CC, board, scope = "all position measures after the board's last read (no carry-forward)",
                                  from = "last read", to = "span end", reason = paste("W07", classification), confidence)]
  exclusions <- if (length(ex)) data.table::rbindlist(ex) else data.table::data.table()
  if (nrow(exclusions)) {
    an <- A[, .(animals = paste(sort(AnimalNum), collapse = ","),
                animals_in_frozen_S13 = paste(sort(AnimalNum[paste(AnimalNum, CC) %in% paste(AQC_FROZEN_S13$AnimalNum, AQC_FROZEN_S13$CC)]), collapse = ",")),
            by = .(SourceFile, board)]
    exclusions <- merge(exclusions, an, by = c("SourceFile", "board"), all.x = TRUE)
    exclusions[, status := "proposed; not applied; the frozen S13 list is unchanged"]
  }
  list(flags = flags, manifest = man[, ..AQC_MANIFEST_COLUMNS], exclusions = exclusions, board_history = bh)
}
