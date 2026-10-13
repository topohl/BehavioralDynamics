# ================================================================
# Measurement validation of the raw antenna logs: detection completeness, reader switching, reconstruction methods
# MMMSociability -- Functions/antenna_measurement.R
# ================================================================
# Used by Testing/audits/audit_antenna_measurement_validation.R (UNREGISTERED measurement validation, 2026-10-13).
# Pure definitions: sourcing this file reads no data and writes no files. Needs data.table, and from
# Functions/antenna_log_helpers.R rfid_flip_filter(), rfid_carry_forward_intervals() and RFID_GRID_X / RFID_GRID_Y.
#
# What the log records. Each row is a detection event: one tag read by one antenna one or more times in a row, with
# its start, its duration (first to last read; a single read is written as 75 ms) and the gaps between its reads in
# ms (MsgValue1, the first 0). Only reads are written. A poll that did not read a tag leaves no row, so a time without
# rows is a time without RECORDED detections, never an observed absence.
#
# Support age. At time u the age of an animal's carried-forward position is the time since the end of its latest
# detection (0 during a detection; undefined before its first one). The position is "supported" while the age is at
# most a bound delta, and "uncertain" afterwards.
#
# Reconstructions return runs in the format of the frozen bin-free metrics (Functions/rfid_binfree_metrics.R): s, e
# (seconds since the window start) and pos (reader).
#   A  raw carry-forward: every change of reader counts; each position is held until the next detection (data version
#      noflip);
#   B  the flip filter rfid_flip_filter() at tau seconds (tau = 2: data version flip2s) and carry-forward; with a finite
#      delta, the runs are cut to the supported time and the uncertain time is left out.
# Both run on detection-event starts (the data versions' rule) or, given read times (amv_read_times()), on single reads.
# An event is a run of reads at one antenna that ends after about 1 s without a read, and events of one tag at
# neighbouring antennas overlap, so a short event that starts inside a long one holds its antenna until the next event
# start, while the long one keeps being read; carry-forward from reads avoids that.
# ================================================================

AMV_GEOMETRY_LEVELS <- c("row", "col1", "diag1", "far")

#' Class of a move between readers p and q (PositionID): "row" (same column, other row: 116 grid units), "col1"
#' (neighbouring column, same row: 100), "diag1" (neighbouring column, other row: 153.2), "far" (two or more columns:
#' >= 200). NA for p == q.
amv_move_geometry <- function(p, q) {
  dx <- abs(RFID_GRID_X[p] - RFID_GRID_X[q]); dy <- abs(RFID_GRID_Y[p] - RFID_GRID_Y[q])
  g <- ifelse(dx == 0 & dy > 0, "row", ifelse(dx == 100 & dy == 0, "col1", ifelse(dx == 100, "diag1", ifelse(dx >= 200, "far", NA_character_))))
  factor(g, levels = AMV_GEOMETRY_LEVELS)
}

#' Column (1-4) of a reader.
amv_column <- function(p) as.integer(RFID_GRID_X[p] / 100 + 1)

#' Read times of events: the event start t (POSIX seconds) plus the cumulative MsgValue1 gaps. Returns data.table(ev,
#' t_read), ev indexing the events; an event with an empty MsgValue1 has no reads.
amv_read_times <- function(t, read_offsets_ms) {
  g <- strsplit(as.character(read_offsets_ms), ",", fixed = TRUE)
  n <- lengths(g)
  off <- as.numeric(unlist(g, use.names = FALSE))
  if (anyNA(off)) stop("Unreadable read offsets.", call. = FALSE)
  ev <- rep.int(seq_along(t), n)
  cs <- cumsum(off)
  before <- c(0, cs)[c(0L, cumsum(n))[seq_along(t)] + 1L]   # sum of all offsets of the earlier events
  data.table::data.table(ev = ev, t_read = t[ev] + (cs - before[ev]) / 1000)
}

#' Union of the intervals [s, e): sorted, disjoint data.table(s, e); touching intervals are merged.
amv_union <- function(s, e) {
  if (!length(s)) return(data.table::data.table(s = numeric(), e = numeric()))
  o <- order(s); s <- s[o]; e <- e[o]
  reach <- cummax(e)
  grp <- cumsum(c(TRUE, s[-1] > reach[-length(reach)]))
  last <- c(grp[-1] != grp[-length(grp)], TRUE)
  data.table::data.table(s = s[c(TRUE, grp[-1] != grp[-length(grp)])], e = reach[last])
}

#' Seconds of [ws, we) at which the support age exceeds each bound in x. det: amv_union() of the animal's detections,
#' including those before ws. Before the first detection the age is undefined and counts as exceeding every bound.
amv_age_exceedance <- function(det, ws, we, x) {
  g0 <- c(-Inf, det$e); g1 <- c(det$s, Inf)   # the gaps; the age is 0 at g0
  vapply(x, function(b) sum(pmax(0, pmin(g1, we) - pmax(g0 + b, ws))), numeric(1))
}

#' The gaps of a detection union inside [ws, we) that last at least min_s: data.table(s, e). The time before the first
#' and after the last detection counts as a gap.
amv_gaps <- function(det, ws, we, min_s = 0) {
  g <- data.table::data.table(s = pmax(c(-Inf, det$e), ws), e = pmin(c(det$s, Inf), we))
  g[e - s >= min_s & e > s]
}

#' Total seconds in both of two sets of disjoint intervals (data.table(s, e) each).
amv_overlap_seconds <- function(a, b) {
  if (!nrow(a) || !nrow(b)) return(0)
  a <- data.table::copy(a); b <- data.table::copy(b)
  data.table::setkey(b, s, e)
  o <- data.table::foverlaps(a, b, by.x = c("s", "e"), type = "any", nomatch = NULL)
  sum(pmax(0, pmin(o$e, o$i.e) - pmax(o$s, o$i.s)))
}

#' Supported time in [ws, we): the detections extended by delta seconds. With delta = Inf, all time from the first
#' detection on (unbounded carry-forward). det: amv_union() of the detections.
amv_support <- function(det, ws, we, delta) {
  if (!nrow(det)) return(data.table::data.table(s = numeric(), e = numeric()))
  if (!is.finite(delta)) return(data.table::data.table(s = max(ws, min(det$s)), e = we)[e > s])
  u <- amv_union(det$s, det$e + delta)
  u[, `:=`(s = pmax(s, ws), e = pmin(e, we))][e > s]
}

#' Runs of one animal in [ws, we), in seconds since ws: detections at times t (sorted) on readers pid, the flip filter
#' at tau seconds over the whole sequence (0: none), then carry-forward from the latest detection before ws.
amv_runs <- function(t, pid, ws, we, tau = 0) {
  f <- rfid_flip_filter(t, pid, tau)
  iv <- rfid_carry_forward_intervals(t, f, ws, we)
  data.table::data.table(s = iv$start - ws, e = iv$end - ws, pos = as.integer(iv$PositionID))
}

#' Runs cut to a set of disjoint intervals (same time origin): the pieces inside, with the other columns kept.
amv_clip_runs <- function(runs, keep) {
  if (!nrow(runs) || !nrow(keep)) return(runs[0])
  k <- data.table::copy(keep)[, .(ks = s, ke = e)]
  data.table::setkey(k, ks, ke)
  o <- data.table::foverlaps(data.table::copy(runs), k, by.x = c("s", "e"), by.y = c("ks", "ke"), type = "any", nomatch = NULL)
  o[, `:=`(s = pmax(s, ks), e = pmin(e, ke))][e > s][, c("ks", "ke") := NULL][]
}

#' Changes of reader in one animal's detections (t start, e end, pid; sorted by t). A visit is a run of detections on
#' one reader. One row per change: t (start of the new visit), from, to, geometry (amv_move_geometry()), gap_s (start
#' of the new visit minus the latest end of the previous visit's detections; negative when both readers read the tag at
#' once), from_dur_s (start of the previous visit to the change), to_dur_s (the change to the next change; NA for the
#' last) and returns (the next change goes back to `from`; NA for the last).
amv_transitions <- function(t, e, pid) {
  empty <- data.table::data.table(t = numeric(), from = integer(), to = integer(), geometry = factor(levels = AMV_GEOMETRY_LEVELS),
                                  gap_s = numeric(), from_dur_s = numeric(), to_dur_s = numeric(), returns = logical())
  if (length(pid) < 2) return(empty)
  r <- rle(as.integer(pid)); k <- length(r$lengths)
  if (k < 2) return(empty)
  first <- cumsum(c(1L, utils::head(r$lengths, -1L)))
  grp <- rep.int(seq_len(k), r$lengths)
  vend <- data.table::data.table(g = grp, e = e)[, .(e = max(e)), by = g]$e
  vs <- t[first]; d <- diff(vs)
  from <- r$values[-k]; to <- r$values[-1]
  data.table::data.table(t = vs[-1], from = from, to = to, geometry = amv_move_geometry(from, to), gap_s = vs[-1] - vend[-k],
                         from_dur_s = d, to_dur_s = c(d[-1], NA_real_),
                         returns = c(if (k >= 3) r$values[3:k] == r$values[1:(k - 2)] else logical(0), NA))
}

#' Occupancy of runs (one animal-window): seconds and proportions per reader (8), per column (4) and in the upper row
#' (y = 116), with the dispersion (Shannon log2 entropy) and dominant share as mmm_bf_animal_metrics() defines them.
amv_occupancy <- function(runs) {
  sec <- tapply(runs$e - runs$s, factor(runs$pos, levels = 1:8), sum)
  sec[is.na(sec)] <- 0
  tot <- sum(sec)
  p <- if (tot > 0) as.numeric(sec) / tot else rep(NA_real_, 8)
  pc <- c(p[1] + p[5], p[2] + p[6], p[3] + p[7], p[4] + p[8])
  pp <- p[p > 0]
  c(list(obs_s = tot, occupancy_dispersion = if (tot > 0) -sum(pp * log2(pp)) else NA_real_, dominant_share = if (tot > 0) max(p) else NA_real_,
         upper_row_share = sum(p[5:8]), column_dispersion = if (tot > 0) { q <- pc[pc > 0]; -sum(q * log2(q)) } else NA_real_),
    stats::setNames(as.list(p), paste0("p_R", 1:8)), stats::setNames(as.list(pc), paste0("p_C", 1:4)))
}

#' Reader of each animal on a grid of `step`-second cells covering [0, len): the position at each cell midpoint from
#' the runs (0 where no run covers it), and the support flag from `supp` (intervals in the same time origin).
amv_grid_track <- function(runs, supp, len, step = 1) {
  mid <- seq(step / 2, len, by = step)
  pos <- integer(length(mid))
  if (nrow(runs)) {
    i <- findInterval(mid, runs$s)
    ok <- i > 0
    ok[ok] <- mid[ok] < runs$e[i[ok]]
    pos[ok] <- runs$pos[i[ok]]
  }
  sup <- logical(length(mid))
  if (nrow(supp)) {
    j <- findInterval(mid, supp$s)
    ok <- j > 0
    ok[ok] <- mid[ok] < supp$e[j[ok]]
    sup <- ok
  }
  list(pos = pos, supported = sup & pos > 0)
}

#' Circular cross-correlation sums of two indicator vectors for every shift k (0-based): sum_t a[t] * b[t + k], with
#' t + k taken modulo the length. Computed with the FFT; returns a numeric vector, element k + 1 for shift k.
amv_circular_crossprod <- function(a, b) {
  n <- length(a)
  round(Re(stats::fft(Conj(stats::fft(as.numeric(a))) * stats::fft(as.numeric(b)), inverse = TRUE)) / n, 6)
}

#' Co-assignment of a dyad on a common grid (amv_grid_track() of both animals). For every shift k in `shifts` (cells;
#' b is moved by k with wrap-around, 0 = observed) the jointly supported cells and the cells in which both are on the
#' same reader, in the same column, or in the same or a neighbouring column. Returns data.table(shift, joint, same,
#' same_col, adj_col).
amv_dyad_shift_table <- function(ta, tb, shifts) {
  sa <- ta$supported; sb <- tb$supported
  pick <- function(v) v[(shifts %% length(sa)) + 1L]
  joint <- pick(amv_circular_crossprod(sa, sb))
  same <- 0
  for (z in 1:8) {
    a <- sa & ta$pos == z; b <- sb & tb$pos == z
    if (any(a) && any(b)) same <- same + pick(amv_circular_crossprod(a, b))
  }
  ca <- ifelse(sa, amv_column(pmax(ta$pos, 1L)), 0L); cb <- ifelse(sb, amv_column(pmax(tb$pos, 1L)), 0L)
  same_col <- 0; adj_col <- 0
  for (x in 1:4) for (y in 1:4) {
    if (abs(x - y) > 1) next
    a <- ca == x; b <- cb == y
    if (!any(a) || !any(b)) next
    v <- pick(amv_circular_crossprod(a, b))
    adj_col <- adj_col + v
    if (x == y) same_col <- same_col + v
  }
  data.table::data.table(shift = shifts, joint = joint, same = same, same_col = same_col, adj_col = adj_col)
}

#' Expected same-reader share of a dyad if the two positions were independent within the jointly supported cells:
#' sum over readers of p_a(z) p_b(z), the proportions taken over those cells. Same for columns and neighbouring columns.
amv_dyad_independence <- function(ta, tb) {
  j <- ta$supported & tb$supported
  if (!any(j)) return(list(same = NA_real_, same_col = NA_real_, adj_col = NA_real_))
  pa <- tabulate(ta$pos[j], 8) / sum(j); pb <- tabulate(tb$pos[j], 8) / sum(j)
  qa <- pa[1:4] + pa[5:8]; qb <- pb[1:4] + pb[5:8]
  adj <- outer(1:4, 1:4, function(x, y) abs(x - y) <= 1)
  list(same = sum(pa * pb), same_col = sum(qa * qb), adj_col = sum(outer(qa, qb) * adj))
}

#' Concurrent tags per event: for each event, the number of OTHER tags with an event on the same board and reader that
#' overlaps it in time (positive overlap). Returns an integer vector in input order.
amv_concurrency <- function(board, pid, t, e, tag) {
  x <- data.table::data.table(row = seq_along(t), board = board, pid = pid, s = t, e = pmax(e, t), tag = tag)
  y <- data.table::copy(x)
  data.table::setkey(y, board, pid, s, e)
  o <- data.table::foverlaps(x, y, by.x = c("board", "pid", "s", "e"), type = "any", nomatch = NULL)
  o <- o[tag != i.tag & pmin(e, i.e) > pmax(s, i.s), .(n = data.table::uniqueN(tag)), by = .(row = i.row)]
  out <- integer(length(t))
  out[o$row] <- o$n
  out
}

#' System rows of a raw log (rows without a tag): t (POSIX seconds), unitLabel, SystemMsg, MsgValue1-3, file_order.
amv_system_messages <- function(path) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  txt <- iconv(list(bytes), from = "UTF-16LE", to = "UTF-8")
  if (is.na(txt)) stop("Cannot decode ", path, " as UTF-16LE.", call. = FALSE)
  lines <- strsplit(txt, "\r?\n")[[1]]
  lines[1] <- sub(paste0("^", intToUtf8(0xFEFF)), "", lines[1])
  body <- lines[-1]
  body <- body[nzchar(body) & !startsWith(body, "#")]
  d <- data.table::fread(text = c(lines[1], body), sep = ";", colClasses = "character", fill = TRUE, showProgress = FALSE)
  d[, file_order := seq_len(.N)]
  d <- d[!nzchar(IdRFID)]
  d[, .(t = (as.numeric(DateTime) - RFID_EXCEL_EPOCH_DAYS) * 86400, unitLabel, SystemMsg, MsgValue1, MsgValue2, MsgValue3, file_order)]
}
