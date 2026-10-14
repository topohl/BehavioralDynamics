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
# srs_v1.1 (2026-10-14, after review): the per-recording work is asr_process_recording() (pure, testable end to end);
# observed time per animal-block is the explicit denominator; under a carry-forward bound the unsupported time is a
# category of its own ("unknown"), reported beside the shares of supported time (asr_shares_with_unknown()); the
# unsupported time is described by the reader last assigned, the length of its read gap and the cage-mates' positions
# (asr_board_missingness()). The bounds (60 s, 300 s) are not validated biological thresholds.
#
# Vendor-stream dependence (knockout). The vendor AnimalPos records are a function of the reads (apr_positions(),
# gate "vendor"). Deleting one reader's reads on a healthy board and recomputing the records shows how much the Stage 29
# window metrics depend on that antenna: the shift between the two streams, against the frozen standardizer SD.
# ================================================================

ASR_VERSION <- "srs_v1.1"
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

# ---------------------------------------------------------------- srs_v1.1: denominators, unknown time, missingness

ASR_UNKNOWN_BOUNDS_S <- c(60, 300)        # bounded carry-forward compared with unrestricted LOCF (not validated thresholds)
ASR_GAP_CLASS_BREAKS <- c(60, 300, 1800, 3600, 10800)   # gap lengths (s) of the unsupported time: 1-5 min, 5-30 min, ...

#' Phase blocks of a span: block index, start bs and end be (clipped to [S0, S1)), Phase, PhaseNumber, block_no and
#' Stage 19's CC4 crop flag. cal: antenna_dv_vendor_calendar(); cc: "CC1".."CC4".
asr_blocks <- function(cal, S0, S1, cc) {
  blk_of <- function(t) animalpos_phase_block_index(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))
  blocks <- data.table::data.table(block = seq(blk_of(S0), blk_of(S1 - 1e-6)))
  blocks[, `:=`(bs = pmax(S0, block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC),
                be = pmin(S1, (block + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC))]
  cb <- data.table::as.data.table(cal$blocks)[, .(block = as.numeric(PhaseBlockIndex), Phase,
                                                  PhaseNumber = ifelse(Phase == "Active", as.integer(ConsecActive), as.integer(ConsecInactive)))]
  blocks <- merge(blocks, cb, by = "block", all.x = TRUE)
  if (anyNA(blocks$Phase)) stop("A block in the span has no vendor phase label.", call. = FALSE)
  blocks[, `:=`(block_no = seq_len(.N), stage19_window = !(cc == "CC4" & PhaseNumber > 2L))]
  blocks[]
}

#' Changes of position of carry-forward intervals (PositionID, start, end; as rfid_carry_forward_intervals() returns them)
#' per block, at reader or column resolution: the starts of every state after the first, after merging equal neighbouring
#' states, that fall after S0 and before censor_from. blocks: data.table(block, bs). Returns data.table(block, changes).
asr_interval_changes <- function(iv, blocks, S0, censor_from = Inf, resolution = c("reader", "column")) {
  resolution <- match.arg(resolution)
  out <- data.table::data.table(block = blocks$block, changes = 0L)
  if (nrow(iv) < 2) return(out)
  st <- if (resolution == "column") amv_column(iv$PositionID) else as.integer(iv$PositionID)
  keep <- c(TRUE, st[-1] != st[-length(st)])
  t <- iv$start[keep][-1]
  t <- t[t > S0 & t < censor_from]
  if (!length(t)) return(out)
  k <- tabulate(findInterval(t, blocks$bs), nbins = nrow(blocks))
  out[, changes := as.integer(k)]
  out[]
}

#' Gap-length class of the gap that holds an unsupported second.
asr_gap_class <- function(gap, breaks = ASR_GAP_CLASS_BREAKS) {
  lab <- c(sprintf("%g-%gs", utils::head(breaks, -1), breaks[-1]), sprintf(">%gs", utils::tail(breaks, 1)))
  out <- as.character(cut(gap, c(breaks, Inf), labels = lab, right = TRUE))
  out[is.infinite(gap)] <- "open (no later read)"
  out[is.na(gap)] <- "before the first read"
  out
}

#' Per-second state of one animal on the 1-s grid u (cell midpoints): carry-forward reader of `iv` (0 = none), age of the
#' latest read (NA before the first), and the length of the read gap holding the second (Inf after the last read).
asr_grid_state <- function(iv, read_t, u) {
  L <- findInterval(u, read_t)
  has <- L > 0
  age <- rep(NA_real_, length(u)); age[has] <- u[has] - read_t[L[has]]
  nxt <- c(read_t, Inf)[L + 1L]
  gap <- rep(NA_real_, length(u)); gap[has] <- nxt[has] - read_t[L[has]]
  list(pos = asr_position_at(iv, u), age = age, gap = gap)
}

#' Missingness of one board on the 1-s grid. states: named list (one per animal) of asr_grid_state() over the same cells;
#' cell_block: block of each cell; valid: logical per cell (FALSE = censored or outside). An animal's second is unknown at
#' bound d when its age exceeds d (or before its first read). For every second it also counts the cage-mates assigned to
#' the same reader (any age), the cage-mates supported (age <= d) at the same reader, and the cage-mates supported anywhere.
#' Returns list(colocation: AnimalNum, block, pos, state (supported / unknown), seconds, mate_same_reader, mate_same_reader_supported,
#' mate_supported_anywhere [counts of seconds with >= 1 such mate], mates; gaps: AnimalNum, block, pos, gap_class, unknown_s).
asr_board_missingness <- function(states, cell_block, valid, bound = 60) {
  ids <- names(states); m <- length(ids)
  P <- vapply(states, function(s) s$pos, integer(length(cell_block)))
  A <- vapply(states, function(s) s$age, numeric(length(cell_block)))
  if (!is.matrix(P)) { P <- matrix(P, ncol = m); A <- matrix(A, ncol = m) }
  supp <- !is.na(A) & A <= bound
  co <- list(); gp <- list()
  for (j in seq_len(m)) {
    ok <- valid
    others <- setdiff(seq_len(m), j)
    same <- if (length(others)) rowSums(P[, others, drop = FALSE] == P[, j] & P[, j] > 0) else integer(nrow(P))
    same_s <- if (length(others)) rowSums((P[, others, drop = FALSE] == P[, j] & P[, j] > 0) & supp[, others, drop = FALSE]) else integer(nrow(P))
    any_s <- if (length(others)) rowSums(supp[, others, drop = FALSE]) else integer(nrow(P))
    d <- data.table::data.table(block = cell_block, pos = P[, j], state = ifelse(supp[, j], "supported", "unknown"),
                                same = same > 0, same_s = same_s > 0, any_s = any_s > 0, gap = states[[j]]$gap)[ok]
    co[[j]] <- d[, .(seconds = .N, mate_same_reader = sum(same), mate_same_reader_supported = sum(same_s), mate_supported_anywhere = sum(any_s)),
                 by = .(block, pos, state)][, `:=`(AnimalNum = ids[j], mates = length(others))]
    gp[[j]] <- d[state == "unknown", .(unknown_s = .N), by = .(block, pos, gap_class = asr_gap_class(gap))][, AnimalNum := ids[j]]
  }
  list(colocation = data.table::rbindlist(co), gaps = data.table::rbindlist(gp))
}

#' Shares of observed time by column (or reader) under a carry-forward bound, with the unsupported time as its own
#' category. occ: seconds by `by` x `unit` (column `seconds` = LOCF, columns `sup_<d>` = seconds with an observation age
#' <= d); observed: data.table(by..., observed_s), the denominator (span time after masks and censoring). For each group and
#' bound (LOCF, max<d>s): share_all = seconds / observed time, where the units plus "unknown" sum to 1; share_supported =
#' seconds / supported time, where the units sum to 1. For LOCF "supported" means assigned (unknown = before the first read).
asr_shares_with_unknown <- function(occ, observed, by, unit = "column", bounds = ASR_UNKNOWN_BOUNDS_S) {
  occ <- data.table::as.data.table(occ); observed <- data.table::as.data.table(observed)
  cols <- c(LOCF = "seconds", stats::setNames(paste0("sup_", bounds), paste0("max", bounds, "s")))
  data.table::rbindlist(lapply(names(cols), function(b) {
    x <- occ[, .(s = sum(get(cols[[b]]))), by = c(by, unit)]
    data.table::setnames(x, unit, "unit")
    x[, unit := as.character(unit)]
    tot <- merge(observed, x[, .(supported_s = sum(s)), by = by], by = by, all.x = TRUE)
    tot[is.na(supported_s), supported_s := 0]
    x <- merge(x, tot, by = by)
    u <- tot[, c(by, "observed_s", "supported_s"), with = FALSE][, `:=`(unit = "unknown", s = observed_s - supported_s)]
    x <- rbind(x, u, use.names = TRUE)
    x[, `:=`(bound = b, share_all = s / observed_s, share_supported = ifelse(unit == "unknown", NA_real_, s / supported_s))]
    x[]
  }))
}

#' One recording through both bases, the missingness grid and (optionally) the vendor-stream knockout. Pure: reads and
#' writes no file. vendor: the v2 preprocessed file (roster, System labels, phase calendar; columns as read with
#' colClasses = "character"); recording: rfid_read_recording(keep_read_offsets = TRUE); tags: rfid_animalpos_tag_map();
#' man: a manifest from sxm_read(); batch, cc, source_file: the recording's keys. Besides this file it needs
#' antenna_data_version.R (antenna_dv_vendor_calendar, antenna_dv_parse_time), animalpos_reconstruction.R (apr_reads,
#' apr_positions), animalpos_preprocessing_helpers.R, antenna_log_helpers.R, antenna_measurement.R (amv_column),
#' rfid_event_stream.R (mmm_evs_windows), rfid_binfree_metrics.R, spatial_exclusion_manifest.R and canonical_animal_id().
#' Returns a named list of data.tables keyed by SourceFile, Batch, CC, board, System and CageEpisodeID, and `runs` (the
#' read-basis product).
asr_process_recording <- function(vendor, recording, tags, man, batch, cc, source_file, knockout = TRUE) {
  vendor <- data.table::as.data.table(vendor)
  tags <- data.table::as.data.table(tags)
  cal <- antenna_dv_vendor_calendar(vendor)
  ev <- recording$events
  S0 <- cal$span_start; S1 <- min(cal$span_end, max(ev$t + ev$dur_sec))
  ids <- unique(vendor[, .(AnimalID, System_label = System)])
  ids[, AnimalNum := canonical_animal_id(AnimalID)]
  if (anyDuplicated(ids$AnimalNum)) stop(source_file, ": an animal has two spellings or two System labels.", call. = FALSE)
  roster <- tibble::as_tibble(ids[, .(AnimalNum, System_label)])

  # event basis: Stage 19's function and parameters
  eb <- rfid_raw_occupancy_intervals(ev[, setdiff(names(ev), c("read_offsets_ms", "file_order"))], roster, tibble::as_tibble(tags), S0, S1, ASR_TAU_S)
  if (anyNA(eb$audit$Board)) stop(source_file, ": an animal has no raw detection in the span.", call. = FALSE)
  board_of <- stats::setNames(as.integer(eb$audit$Board), eb$audit$AnimalNum)
  ev_iv_all <- data.table::as.data.table(eb$intervals)

  # phase blocks (identical edges for both bases), Stage 19's CC4 crop, the manifest masks
  blocks <- asr_blocks(cal, S0, S1, cc)
  edges <- blocks$bs[-1]

  # reads on the millisecond grid (apr_reads(): the times that reproduce the vendor records), roster animals only
  rd <- data.table::as.data.table(apr_reads(ev))[, .(tag, t_ms, gx, gy, board, PositionID)]
  rd[, ord := .I]
  rd <- merge(rd, tags[AnimalNum %in% ids$AnimalNum], by = "tag")
  data.table::setorder(rd, ord)

  # Stage 29 windows of this recording (first active phase and the following light phase), as mmm_evs_windows() sets them;
  # Stage 29's stream starts at the file's first record t0, so its active window holds data from t0 on
  pre <- data.table::data.table(SourceFile = source_file, DateTime = antenna_dv_parse_time(vendor$DateTime), Batch = batch, CageChange = cc)
  w29 <- mmm_evs_windows(pre)
  wins <- data.table::data.table(window = c("active", "light"), ws = pmax(as.numeric(c(w29$active_start, w29$light_start)), as.numeric(w29$t0)),
                                 we = as.numeric(c(w29$active_end, w29$light_end)))
  # the v2 vendor records (no synthetic preprocessing rows), to check the emulated baseline record for record
  syn <- if ("is_synthetic_preprocessing_row" %in% names(vendor)) toupper(vendor$is_synthetic_preprocessing_row) %in% "TRUE" else FALSE
  v2 <- data.table::data.table(AnimalNum = canonical_animal_id(vendor$AnimalID), t = as.numeric(antenna_dv_parse_time(vendor$DateTime)),
                               PositionID = as.integer(vendor$PositionID))[!syn]

  occ <- list(); xt <- list(); cens <- list(); anim <- list(); runs_out <- list(); kruns <- list(); vpar <- list(); agr <- list()
  ablk <- list(); keep_iv <- list()
  for (a in names(board_of)) {
    b_own <- board_of[[a]]
    excluded <- sxm_spatial_excluded(man, batch, cc, b_own)
    cfrom <- sxm_censor_from(man, batch, cc, b_own)
    cuts <- c(edges, if (is.finite(cfrom) && cfrom > S0 && cfrom < S1) cfrom)
    ra <- rd[AnimalNum == a & board == b_own]
    obs <- asr_resolve_ties(ra$t_ms, ra$PositionID)
    t_obs <- obs$t_ms / 1000
    filt <- rfid_flip_filter(t_obs, obs$pid, ASR_TAU_S)
    rd_iv <- data.table::as.data.table(rfid_carry_forward_intervals(t_obs, filt, S0, S1))
    read_t <- unique(ra$t_ms) / 1000
    ev_iv <- ev_iv_all[AnimalNum == a, .(PositionID, start, end)]
    keep_iv[[a]] <- list(rd_iv = rd_iv, read_t = read_t, cfrom = cfrom)
    seg <- asr_animal_segments(ev_iv, rd_iv, read_t, S0, S1, cuts)
    ag <- asr_aggregate(seg, blocks[, .(block, bs)], censor_from = cfrom)
    # at each in-span observation (before censoring): does each basis hold the reader just read? (timestamp consistency,
    # partly built into the bases; not localisation accuracy)
    io <- which(t_obs >= S0 & t_obs < min(S1, cfrom))
    agr[[a]] <- data.table::data.table(block = blocks$block[findInterval(t_obs[io], blocks$bs)], pid = obs$pid[io],
                                       pe = asr_position_at(ev_iv, t_obs[io]), pr = asr_position_at(rd_iv, t_obs[io]))[
      , .(observations = .N, event_holds_read_reader = sum(pe == pid), read_holds_read_reader = sum(pr == pid),
          event_holds_read_column = sum(amv_column(pmax(pe, 1L)) == amv_column(pid) & pe > 0),
          read_holds_read_column = sum(amv_column(pmax(pr, 1L)) == amv_column(pid) & pr > 0)), by = block][, AnimalNum := a]
    occ[[a]] <- ag$occupancy[, AnimalNum := a]; xt[[a]] <- ag$crosstab[, AnimalNum := a]
    cens[[a]] <- ag$censored[, AnimalNum := a]
    # per animal-block: the observed time (the denominator: block time before the censoring time) and the changes of
    # position of both bases at reader and column resolution
    bt <- blocks[, .(block, observed_s = pmax(0, pmin(be, cfrom) - bs))]
    for (nm in c("event_reader", "event_column", "read_reader", "read_column")) {
      iv <- if (startsWith(nm, "event")) ev_iv else rd_iv
      ch <- asr_interval_changes(iv, blocks[, .(block, bs)], S0, cfrom, resolution = sub("^.*_", "", nm))
      bt <- merge(bt, data.table::setnames(ch, "changes", paste0("changes_", nm)), by = "block")
    }
    bt <- merge(bt, data.table::data.table(block = blocks$block[findInterval(t_obs[io], blocks$bs)])[, .(observations = .N), by = block],
                by = "block", all.x = TRUE)
    bt[is.na(observations), observations := 0L]
    ablk[[a]] <- bt[, AnimalNum := a]
    # the read-basis product: runs cut at the block edges (and the censoring time) with their supported seconds
    rp <- asr_cut_intervals(rd_iv, cuts)
    rp <- rp[start < cfrom]
    for (dd in ASR_BOUNDS_S) data.table::set(rp, j = paste0("sup_", dd), value = asr_supported_seconds(read_t, dd, rp$start, rp$end, from = S0))
    rp[, `:=`(AnimalNum = a, block = blocks$block[findInterval(start, blocks$bs)])]
    runs_out[[a]] <- rp
    in_span <- t_obs >= S0 & t_obs < S1
    chg <- function(v) if (length(v) < 2) 0L else sum(v[-1] != v[-length(v)])
    anim[[a]] <- data.table::data.table(
      AnimalNum = a, board = b_own, System_label_v2 = ids$System_label[ids$AnimalNum == a], spatial_excluded = excluded,
      movement_excluded = sxm_movement_excluded(man, batch, cc, b_own), censor_from = cfrom,
      reads_on_board = nrow(ra), reads_off_board = sum(rd$AnimalNum == a) - nrow(ra),
      observations = nrow(obs), tie_observations = sum(obs$tie), tie_observations_in_span = sum(obs$tie & in_span),
      read_seeded = any(t_obs < S0), read_seed_age_s = if (any(t_obs < S0)) S0 - max(t_obs[t_obs < S0]) else NA_real_,
      changes_read_tau0_in_span = chg(obs$pid[in_span]), changes_read_tau2_in_span = chg(filt[in_span]))
    if (!knockout) next
    # knockout: the vendor stream from all of the animal's reads (all boards, as the vendor pools them), and without one
    # reader's reads on its own board (its few off-board reads are kept)
    rall <- rd[AnimalNum == a]
    variants <- list(baseline = rall)
    for (k in ASR_KNOCKOUT_READERS) variants[[paste0("ko_R", k)]] <- rall[!(board == b_own & PositionID == k)]
    for (v in names(variants)) {
      x <- variants[[v]]
      rec_v <- asr_vendor_records(x$t_ms, x$gx, x$gy)
      if (v == "baseline") for (w in seq_len(nrow(wins))) {
        e <- rec_v[t >= wins$ws[w] - 5e-4 & t < wins$we[w] - 5e-4]; o <- v2[AnimalNum == a & t >= wins$ws[w] - 5e-4 & t < wins$we[w] - 5e-4]
        vpar[[length(vpar) + 1L]] <- data.table::data.table(AnimalNum = a, window = wins$window[w], records_emulated = nrow(e), records_v2 = nrow(o),
          identical = nrow(e) == nrow(o) && all(e$PositionID == o$PositionID) && all(abs(e$t - o$t) <= 1.5e-3))
      }
      for (w in seq_len(nrow(wins))) {
        r <- asr_window_runs(rec_v, wins$ws[w], wins$we[w])
        if (!nrow(r)) next
        kn <- x[t_ms >= wins$ws[w] * 1000 & t_ms < wins$we[w] * 1000, .N]
        kruns[[length(kruns) + 1L]] <- r[, `:=`(AnimalNum = a, CC = cc, SourceFile = source_file, System = paste0("sys.", b_own),
                                                variant = v, window = wins$window[w], reads_in_window = kn)]
      }
    }
  }

  # missingness on the 1-s grid, board by board (cage-mates share the grid and the censoring time)
  ncell <- floor(S1 - S0)
  u <- S0 + 0.5 + seq_len(ncell) - 1
  cell_block <- blocks$block[findInterval(u, blocks$bs)]
  colo <- list(); gaps <- list()
  for (b in sort(unique(board_of))) {
    ids_b <- names(board_of)[board_of == b]
    st <- stats::setNames(lapply(ids_b, function(a) asr_grid_state(keep_iv[[a]]$rd_iv, keep_iv[[a]]$read_t, u)), ids_b)
    valid <- u < keep_iv[[ids_b[1]]]$cfrom
    for (d in ASR_UNKNOWN_BOUNDS_S) {
      bm <- asr_board_missingness(st, cell_block, valid, bound = d)
      colo[[length(colo) + 1L]] <- bm$colocation[, bound_s := d]
      gaps[[length(gaps) + 1L]] <- bm$gaps[, bound_s := d]
    }
    rm(st)
  }

  key <- data.table::data.table(SourceFile = source_file, Batch = batch, CC = cc)
  addkeys <- function(x) {
    x <- cbind(key[rep(1L, nrow(x))], x)
    x[, `:=`(board = board_of[AnimalNum])]
    x[, `:=`(System = paste0("sys.", board), CageEpisodeID = asr_cage_episode(Batch, board, CC))]
    x
  }
  ph <- blocks[, .(block, block_no, Phase, PhaseNumber, stage19_window)]
  res <- list()
  res$occupancy <- merge(addkeys(data.table::rbindlist(occ)), ph, by = "block")
  res$crosstab <- merge(addkeys(data.table::rbindlist(xt)), ph, by = "block")
  res$censored <- addkeys(data.table::rbindlist(cens))
  res$read_agreement <- merge(addkeys(data.table::rbindlist(agr)), ph, by = "block")
  res$animal_block <- merge(addkeys(data.table::rbindlist(ablk)), ph, by = "block")
  res$colocation <- merge(addkeys(data.table::rbindlist(colo)), ph, by = "block")
  res$gaps <- merge(addkeys(data.table::rbindlist(gaps)), ph, by = "block")
  res$animals <- addkeys(data.table::rbindlist(anim))[, `:=`(span_start = S0, span_end = S1)]
  res$animals <- merge(res$animals, data.table::as.data.table(eb$audit)[, .(AnimalNum, tag, events_in_span, events_off_board,
                                                                              event_seeded = seeded, event_seed_age_s = seed_lag_seconds,
                                                                              changes_event_raw = reader_changes_raw,
                                                                              changes_event_tau2 = reader_changes_after_flip_filter)], by = "AnimalNum")
  res$blocks <- cbind(key[rep(1L, nrow(blocks))], blocks)
  if (knockout) {
    # window metrics of every variant: all animals of the recording together. Cage-mates share the variant: a knockout
    # deletes reader k from every animal's reads on its own board, so each board loses that antenna at once
    KR <- data.table::rbindlist(kruns)
    km <- KR[, asr_window_metrics(.SD[, .(AnimalNum, CC, SourceFile, System, s, e, pos)]), by = .(variant, window)]
    km <- merge(km, unique(KR[, .(variant, window, AnimalNum, reads_in_window)]), by = c("variant", "window", "AnimalNum"), all.x = TRUE)
    km[, board := board_of[AnimalNum]]
    km[, CageEpisodeID := asr_cage_episode(batch, board, CC)]
    km[, Batch := batch]
    res$knockout_metrics <- km
    res$vendor_record_parity <- cbind(key[rep(1L, length(vpar))], data.table::rbindlist(vpar))[, `:=`(board = board_of[AnimalNum])][
      , CageEpisodeID := asr_cage_episode(Batch, board, CC)][]
  }
  res$runs <- addkeys(data.table::rbindlist(runs_out))
  res
}
