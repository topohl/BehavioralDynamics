# ================================================================
# Single-read spatial reconstruction with observation age, and the dependence of the vendor position stream on one
# antenna
# MMMSociability -- Functions/antenna_spatial_reconstruction.R
# ================================================================
# Used by Testing/audits/build_single_read_spatial_reconstruction.R (UNREGISTERED; isolated output root; Stage 19 is not
# replaced). Pure definitions: sourcing this file reads no data and writes no files. Needs data.table and, from
# Functions/antenna_log_helpers.R, rfid_flip_filter() and rfid_carry_forward_intervals(); asr_vendor_records() needs
# apr_positions() (Functions/animalpos_reconstruction.R); asr_window_metrics() needs mmm_bf_window_table()
# (Functions/rfid_binfree_metrics.R).
#
# Two bases with identical parameters. Each animal holds the reader of its latest observation on its own board (the
# board with most of its detection events in the span, as in Stage 19); border flips shorter than 2 s are removed
# (rfid_flip_filter(), as in Stage 19 and data version flip2s); the last observation before the span seeds it.
#   event basis  observations = detection-event starts: Stage 19 and the data versions (rfid_raw_occupancy_intervals()).
#   read basis   observations = single reads at their logged times: the event start plus the cumulative MsgValue1 gaps,
#                on the millisecond grid of apr_reads() (the times that reproduce the vendor records to the ms). Reads of
#                one tag at two readers in the same millisecond (about 0.03% of reads, always on one board) are one
#                observation: the reader held before them if it is among them, otherwise the lowest PositionID.
# Observation age at time u: u minus the animal's latest read on its board, for either basis (undefined before its
# first read). The age is the uncertainty of an assignment: a bounded reconstruction keeps an assignment only while the
# age is at most delta; carry-forward (LOCF) keeps it until the next observation.
#
# Vendor-stream dependence (knockout). The vendor AnimalPos records are a function of the reads (apr_positions(),
# gate "vendor"). Deleting one reader's reads on a healthy board and recomputing the records shows how much the Stage 29
# window metrics depend on that antenna: the shift between the two streams, against the frozen standardizer SD.
# ================================================================

ASR_VERSION <- "srs_v1.0"
ASR_TAU_S <- 2                          # flip filter, as Stage 19 and data version flip2s
ASR_BOUNDS_S <- c(2, 10, 60, 300)       # carry-forward bounds on the observation age (s); LOCF = no bound
ASR_KNOCKOUT_READERS <- c(3L, 5L)       # the readers that failed whole sessions (Stage 00b H06)
ASR_BOUT_CRITERION_S <- 39.3970988275363  # Stage 29 frozen bout criterion
# Materiality of a knockout shift, declared before the knockout was run and never tuned: the median absolute shift of
# a construct reaches 0.2 standardizer SD, or its 90th percentile reaches 0.5 SD (the frozen CC1 SIS residual SDs of
# MMM_BEHAVIOR_CONFIG$metrics). Scale only; no group or outcome is involved.
ASR_MATERIAL_MEDIAN_SD <- 0.2
ASR_MATERIAL_P90_SD <- 0.5

#' Stage 29's cage-episode key: "B5|sys.4|CC1".
asr_cage_episode <- function(batch, board, cc) paste(batch, paste0("sys.", board), cc, sep = "|")

#' One observation per millisecond. t_ms: sorted read times (ms); pid: their readers. Reads in the same millisecond at
#' one reader collapse; at different readers they resolve to the reader of the previous observation if it is among
#' them, otherwise to the lowest PositionID. Returns data.table(t_ms, pid, tie) with tie = TRUE where readers differed.
asr_resolve_ties <- function(t_ms, pid) {
  n <- length(t_ms)
  if (!n) return(data.table::data.table(t_ms = numeric(), pid = integer(), tie = logical()))
  if (is.unsorted(t_ms)) stop("t_ms must be sorted.", call. = FALSE)
  pid <- as.integer(pid)
  first <- which(c(TRUE, t_ms[-1] != t_ms[-n]))
  last <- c(first[-1] - 1L, n)
  res <- pid[first]
  tie <- logical(length(first))
  for (k in which(last > first)) {   # in time order, so the previous observation is already resolved
    m <- pid[first[k]:last[k]]
    if (all(m == m[1])) next
    tie[k] <- TRUE
    held <- if (k > 1L) res[k - 1L] else NA_integer_
    res[k] <- if (!is.na(held) && held %in% m) held else min(m)
  }
  data.table::data.table(t_ms = t_ms[first], pid = res, tie = tie)
}

#' The time at which the observation age exceeds `delta`, from `from` on: sorted, disjoint intervals [us, ue) and the
#' cumulative length before each (cl). `t`: sorted read times (s), possibly starting before `from`. Before the first read
#' the age is undefined and counts as exceeding every bound.
asr_unsupported <- function(t, delta, from) {
  if (!length(t)) return(list(us = from, ue = Inf, cl = 0))
  if (is.unsorted(t)) stop("t must be sorted.", call. = FALSE)
  n <- length(t)
  gi <- which(diff(t) > delta)
  lead <- t[1] > from
  us <- c(if (lead) from, t[gi] + delta, t[n] + delta)
  ue <- c(if (lead) t[1], t[gi + 1L], Inf)
  len <- ue - us
  list(us = us, ue = ue, cl = c(0, cumsum(len[-length(len)])))
}

#' Length of the unsupported set (asr_unsupported()) up to each x (up to an additive constant shared by all x).
asr_unsupported_upto <- function(U, x) {
  k <- findInterval(x, U$us)
  out <- numeric(length(x))
  i <- k > 0
  out[i] <- U$cl[k[i]] + pmax(0, pmin(x[i], U$ue[k[i]]) - U$us[k[i]])
  out
}

#' Seconds of each [a, b) at which the observation age is at most `delta`.
asr_supported_seconds <- function(read_t, delta, a, b, from = min(a)) {
  U <- asr_unsupported(read_t, delta, from)
  (b - a) - (asr_unsupported_upto(U, b) - asr_unsupported_upto(U, a))
}

#' Reader held by intervals iv (PositionID, start, end; disjoint, sorted) at each time a; 0 where none.
asr_position_at <- function(iv, a) {
  p <- integer(length(a))
  if (!nrow(iv)) return(p)
  k <- findInterval(a, iv$start)
  ok <- k > 0
  ok[ok] <- a[ok] < iv$end[k[ok]]
  p[ok] <- as.integer(iv$PositionID[k[ok]])
  p
}

#' Elementary segments of one animal on [S0, S1): cut at every assignment change of either basis and at `cuts` (block
#' edges, a censoring time). ev_iv, rd_iv: carry-forward intervals (PositionID, start, end) of the event and read bases.
#' read_t: sorted times of the animal's reads on its board (including those before S0). Returns data.table(a, b, pos_e,
#' pos_r, sup_<bound>...): pos 0 = unassigned; sup_<d> = seconds of the segment with an observation age <= d.
asr_animal_segments <- function(ev_iv, rd_iv, read_t, S0, S1, cuts = numeric(), bounds = ASR_BOUNDS_S) {
  br <- sort(unique(c(S0, S1, ev_iv$start, rd_iv$start, cuts)))
  br <- br[br >= S0 & br <= S1]
  a <- br[-length(br)]; b <- br[-1]
  seg <- data.table::data.table(a = a, b = b, pos_e = asr_position_at(ev_iv, a), pos_r = asr_position_at(rd_iv, a))
  for (d in bounds) data.table::set(seg, j = paste0("sup_", d), value = asr_supported_seconds(read_t, d, a, b, from = S0))
  seg
}

#' Sums of segments by block. blocks: data.table(block, bs) sorted by bs (block starts; every segment lies in one block
#' because the block edges are cuts). censor_from: segments from this time on are censored (left out, counted).
#' Returns list(occupancy: block, basis, PositionID, seconds, sup_*; crosstab: block, pos_e, pos_r, seconds, sup_*;
#' censored: block, seconds).
asr_aggregate <- function(seg, blocks, bounds = ASR_BOUNDS_S, censor_from = Inf) {
  sc <- c("seconds", paste0("sup_", bounds))
  s <- data.table::copy(seg)
  s[, `:=`(block = blocks$block[findInterval(a, blocks$bs)], seconds = b - a)]
  cens <- s[a >= censor_from, .(seconds = sum(seconds)), by = block]
  s <- s[a < censor_from]
  occ <- rbind(s[pos_e > 0, lapply(.SD, sum), by = .(block, PositionID = pos_e), .SDcols = sc][, basis := "event"],
               s[pos_r > 0, lapply(.SD, sum), by = .(block, PositionID = pos_r), .SDcols = sc][, basis := "read"])
  xt <- s[, lapply(.SD, sum), by = .(block, pos_e, pos_r), .SDcols = sc]
  list(occupancy = occ[], crosstab = xt[], censored = cens[])
}

#' Intervals (PositionID, start, end) cut at `cuts`; pieces keep their PositionID.
asr_cut_intervals <- function(iv, cuts) {
  if (!nrow(iv)) return(data.table::data.table(PositionID = integer(), start = numeric(), end = numeric()))
  iv <- data.table::as.data.table(iv)[, .(PositionID, start, end)]
  cuts <- sort(unique(cuts))
  k0 <- findInterval(iv$start, cuts); k1 <- findInterval(iv$end, cuts, left.open = TRUE)
  n_extra <- pmax(0L, k1 - k0)
  if (!any(n_extra > 0L)) return(iv)
  rows <- rep.int(seq_len(nrow(iv)), n_extra + 1L)
  piece <- sequence(n_extra + 1L) - 1L
  lo <- iv$start[rows]; hi <- iv$end[rows]
  inner_lo <- piece > 0L; inner_hi <- piece < n_extra[rows]
  lo[inner_lo] <- cuts[k0[rows][inner_lo] + piece[inner_lo]]
  hi[inner_hi] <- cuts[k0[rows][inner_hi] + piece[inner_hi] + 1L]
  data.table::data.table(PositionID = iv$PositionID[rows], start = lo, end = hi)[end > start]
}

# ---------------------------------------------------------------- vendor-stream dependence (knockout)

#' Vendor position records of one tag as a change stream: data.table(t (s), PositionID) from apr_positions(gate =
#' "vendor") without the closing record. t_ms sorted (apr_reads()).
asr_vendor_records <- function(t_ms, gx, gy) {
  if (!length(t_ms)) return(data.table::data.table(t = numeric(), PositionID = integer()))
  p <- apr_positions(t_ms, gx, gy, gate = "vendor")
  p <- p[p$kind != "closing", , drop = FALSE]
  data.table::data.table(t = p$t_ms / 1000, PositionID = as.integer(p$PositionID))
}

#' Carry-forward runs of a record stream in [ws, we), in seconds since ws, as Stage 29 builds them
#' (mmm_evs_window_stream()): each record holds until the next one or the window end; the latest record before ws seeds;
#' equal neighbouring positions merge. Returns data.table(s, e, pos).
asr_window_runs <- function(rec, ws, we) {
  if (!nrow(rec)) return(data.table::data.table(s = numeric(), e = numeric(), pos = integer()))
  nx <- c(rec$t[-1], Inf)
  s <- pmax(rec$t, ws); e <- pmin(nx, we)
  keep <- e > s
  r <- data.table::data.table(s = s[keep] - ws, e = e[keep] - ws, pos = as.integer(rec$PositionID[keep]))
  if (!nrow(r)) return(r[, .(s, e, pos)])
  r[, rid := data.table::rleid(pos)]
  r[, .(s = min(s), e = max(e), pos = pos[1]), by = rid][, rid := NULL][]
}

#' Stage 29 window metrics of runs: data.table(AnimalNum, CC, SourceFile, System, s, e, pos) -> one row per animal
#' (crossing_rate, occupancy_dispersion, fragmentation, shared_zone_use, ...), mmm_bf_window_table().
asr_window_metrics <- function(runs, bout_criterion_s = ASR_BOUT_CRITERION_S) {
  mmm_bf_window_table(list(runs = data.table::as.data.table(runs)), bout_criterion_s)$animals
}

#' Size of the shifts x (knockout minus baseline) of one construct against its scale sd: quartiles of x, the median and
#' 90th percentile of |x|, and whether the channel matters (median |x| >= med_k sd or p90 |x| >= p90_k sd).
asr_materiality <- function(x, sd, med_k = ASR_MATERIAL_MEDIAN_SD, p90_k = ASR_MATERIAL_P90_SD) {
  x <- x[is.finite(x)]
  if (!length(x) || !is.finite(sd) || sd <= 0) {
    return(data.table::data.table(n = length(x), median_shift = NA_real_, q25_shift = NA_real_, q75_shift = NA_real_,
                                  median_abs_shift = NA_real_, p90_abs_shift = NA_real_, max_abs_shift = NA_real_, scale_sd = sd,
                                  median_abs_in_sd = NA_real_, p90_abs_in_sd = NA_real_, material = NA))
  }
  a <- abs(x)
  med <- stats::median(a); p90 <- stats::quantile(a, 0.9, names = FALSE, type = 7)
  data.table::data.table(n = length(x), median_shift = stats::median(x), q25_shift = stats::quantile(x, 0.25, names = FALSE),
                         q75_shift = stats::quantile(x, 0.75, names = FALSE), median_abs_shift = med, p90_abs_shift = p90,
                         max_abs_shift = max(a), scale_sd = sd, median_abs_in_sd = med / sd, p90_abs_in_sd = p90 / sd,
                         material = med >= med_k * sd || p90 >= p90_k * sd)
}
