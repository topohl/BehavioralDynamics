# ================================================================
# Audit: measurement validation of the raw antenna logs (detection, switching, reconstruction, metric sensitivity)
# MMMSociability -- Testing/audits/audit_antenna_measurement_validation.R
# ================================================================
# UNREGISTERED measurement validation (decided 2026-10-13); descriptive only, no tests of group effects, no thresholds
# chosen by any outcome. Reads (never writes): the 24 raw MultiMouseMonitor recordings, the vendor AnimalPos exports
# (tag map) and the v2 preprocessed files (roster, phase calendar). Writes one folder per recording to
# <OUT>/per_recording/<Batch>_<CC>/; a folder whose DONE marker names the same code hashes is skipped, so an interrupted
# run resumes. Then the combined tables go to <OUT>/tables/.
# OUT = MMM_AMV_OUT, default <Behavior>/RANM/measurement_validation; refused inside the live root or a data-version
# root. MMM_AMV_ONLY = a regex on "B3_CC1" etc. (test runs; the combined tables are then not written).
#
# Per recording (definitions in Functions/antenna_measurement.R):
#   integrity            events, reads, MsgValue1 consistency, durations, file-order clock jumps, duplicates, tags
#   system_messages      rows without a tag (version, start, ...)
#   silences             stretches of >= 60 s without any recorded detection: whole recording, and per board
# Detected time and the support age run from an event's first to its last read (a single read is a point; the log writes
# it as 75 ms, which is kept only for the concurrency count).
#   animal_block         per animal x 12-h phase block: reads, events, detected seconds, support-age exceedance,
#                        gaps >= 60 / 600 s and how much of them falls in board / recording silences, inter-read gaps,
#                        reader changes by geometry, simultaneous reads, returning visits (A-B-A) by duration
#   animal_reader_block  per animal x reader x block: carry-forward seconds (method A), reads, detected seconds
#   reader_concurrency   per board x reader x phase x concurrent other tags: events, reads, single-read events,
#                        summed duration and read intervals of multi-read events
#   coassign_density     per animal x block x mates assigned the same reader (method A, all tags on the board):
#                        seconds, reads, seconds with at least one read
#   reversal_hist        returning visits and all visits by geometry, phase and log10 duration bin
#   window_metrics       Stage 29 windows (active, light): per animal x basis x tau x delta, the frozen metrics and
#                        occupancy (basis: carry-forward from event starts, or from single reads; AMV_RECON)
#   window_dyads         Stage 29 windows: per dyad x (basis, tau, delta) the co-assigned cells (1-s grid), the
#                        independence expectation and circular-shift nulls; exact frozen same_s / obs_s for tau 0 and 2
#   window_basis         Stage 29 windows: per animal and tau, how often the two bases assign different readers
# Run from the repository root:  Rscript Testing/audits/audit_antenna_measurement_validation.R
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble) })

AMV_TAU <- c(0, 0.5, 1, 2, 5, 10, 20)
AMV_DELTA <- c(Inf, 300, 60, 30, 10, 5)
AMV_AGE_BOUNDS <- c(1, 2, 5, 10, 30, 60, 300, 1800)
AMV_REVERSAL_BOUNDS <- c(0.5, 1, 2, 5, 10, 20)
# The frozen criterion and the S11 alternatives (Functions/behavior_analysis_config.R)
AMV_BOUT_CRITERIA <- c(declared = 39.3970988275363, half = 19.6985494137682, double = 78.7941976550726, sibly_2process = 149.84699971049)
# Reconstructions: carry-forward from detection-event starts (the data versions' rule) at every tau, and from single reads
# (each read's antenna holds until the next read) at tau 0 and 2.
AMV_RECON <- rbind(data.table(basis = "event_start", tau = AMV_TAU), data.table(basis = "read", tau = c(0, 2)))
AMV_COASSIGN <- data.table(basis = c(rep("event_start", 6), rep("read", 3)), tau = c(0, 2, 0, 2, 2, 10, 0, 2, 2),
                           delta = c(Inf, Inf, 60, 60, 10, Inf, Inf, Inf, 60))
AMV_SHIFTS_SHORT <- c(-(12:1) * 300, (1:12) * 300)        # +-5 to 60 min
AMV_SHIFTS_LONG <- seq(1800, 43200 - 1800, by = 1800)     # 30 min to 11.5 h
AMV_HIST_BREAKS <- seq(-2, 4.5, by = 0.1)                 # log10 seconds

# Expected antenna grid: AM<board>-<k> at x = 100 * floor((k - 1) / 2), y = 116 * ((k - 1) %% 2)
amv_expected_grid <- function(antenna) list(x = 100 * ((antenna - 1) %/% 2), y = 116 * ((antenna - 1) %% 2))

amv_process_recording <- function(vf, RAW_DIR, TAG_DIR) {
  batch <- sub("^E9_SIS_(B[0-9]+)_.*$", "\\1", basename(vf)); cc <- sub("^.*_(CC[0-9]+)_.*$", "\\1", basename(vf))
  src <- basename(vf)
  vendor <- fread(vf, colClasses = "character")
  cal <- antenna_dv_vendor_calendar(vendor)
  tags <- as.data.table(rfid_animalpos_tag_map(file.path(TAG_DIR, batch, sprintf("E9_SIS_%s_%s_AnimalPos.csv", batch, cc))))
  rec <- rfid_read_recording(RAW_DIR, batch, cc, keep_read_offsets = TRUE)
  ev <- as.data.table(rec$events)
  ev[, `:=`(fo = file_order, e_log = t + dur_sec)]   # e_log: the end as logged (a single read is written as 75 ms)
  S0 <- cal$span_start; S1 <- min(cal$span_end, max(ev$e_log))
  roster <- unique(vendor[, .(AnimalID)])[, AnimalNum := canonical_animal_id(AnimalID)]
  ev <- merge(ev, tags, by = "tag", all.x = TRUE)
  ev[, in_span := t >= S0 & t < S1]
  own <- ev[in_span == TRUE, .N, by = .(tag, board)][order(tag, -N, board)][, .SD[1], by = tag][, .(tag, own_board = board)]
  ev <- merge(ev, own, by = "tag", all.x = TRUE)
  ev[, `:=`(roster = !is.na(AnimalNum) & AnimalNum %in% roster$AnimalNum, on_board = !is.na(own_board) & board == own_board)]
  setorder(ev, t, fo)
  if (!all(roster$AnimalNum %in% ev$AnimalNum[ev$in_span])) stop(src, ": a roster animal has no detection in the span.", call. = FALSE)
  rt <- amv_read_times(ev$t, ev$read_offsets_ms)
  i_ev <- rt$ev   # inside rt[...], `ev` is rt's column
  set(rt, j = c("tag", "board", "pid", "AnimalNum", "use"),
      value = list(ev$tag[i_ev], ev$board[i_ev], ev$PositionID[i_ev], ev$AnimalNum[i_ev], ev$roster[i_ev] & ev$on_board[i_ev]))
  rt[, wg := { d <- c(NA_real_, diff(t_read)); d[c(TRUE, ev[-1] != ev[-.N])] <- NA_real_; d }]   # gaps between reads of one event
  blk_of <- function(t) animalpos_phase_block_index(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))
  blocks <- data.table(block = seq(blk_of(S0), blk_of(S1 - 1e-6)))
  blocks[, `:=`(bs = pmax(S0, block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC),
                be = pmin(S1, (block + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC))]
  blocks <- merge(blocks, as.data.table(cal$blocks)[, .(block = PhaseBlockIndex, Phase)], by = "block", all.x = TRUE)
  blocks[, block_no := seq_len(.N)]
  key <- data.table(SourceFile = src, Batch = batch, CC = cc)

  # integrity -------------------------------------------------------------------------------------------------------
  n_off <- tabulate(rt$ev, nbins = nrow(ev))
  rspan <- rt[, .(span = max(t_read) - min(t_read)), by = ev]
  multi <- ev$reads >= 2
  dspan <- numeric(nrow(ev)); dspan[rspan$ev] <- rspan$span
  ev[, e := t + dspan]   # last read: the end used for detected time, support age and gaps
  dev <- as.data.table(rec$devices)
  g <- amv_expected_grid(dev$antenna)
  fo_order <- ev[order(fo)]
  integrity <- cbind(key, data.table(
    raw_log = basename(rec$path), raw_logs_ignored = paste(rec$ignored, collapse = "|"), clock_fold_rows_dropped = rec$clock_fold_rows_dropped,
    span_start = S0, span_end = S1, vendor_span_end = cal$span_end, recording_first = min(ev$t), recording_end = max(ev$e),
    events = nrow(ev), events_in_span = sum(ev$in_span), reads = sum(ev$reads),
    reads_ne_offsets = sum(ev$reads != n_off), single_read_dur_ne_75ms = sum(ev$reads == 1 & abs(ev$dur_sec - 0.075) > 1e-6),
    multi_read_dur_ne_read_span = sum(multi & abs(ev$dur_sec - dspan) > 0.002), single_read_events = sum(ev$reads == 1),
    max_event_dur_s = max(ev$dur_sec), max_within_event_read_gap_s = max(rt$wg, na.rm = TRUE),
    within_event_read_gap_q99_s = as.numeric(quantile(rt$wg, 0.99, na.rm = TRUE)),
    file_order_end_jumps_back_gt1s = sum(fo_order$e < cummax(fo_order$e) - 1),
    duplicate_events = sum(duplicated(ev[, .(tag, unit, t)])),
    events_unknown_tag = sum(is.na(ev$AnimalNum)), unknown_tags = uniqueN(ev$tag[is.na(ev$AnimalNum)]),
    tags_in_span = uniqueN(ev$tag[ev$in_span]), roster_animals = nrow(roster),
    nonroster_tags_in_span = uniqueN(ev$tag[ev$in_span & !ev$roster]), events_nonroster_in_span = sum(ev$in_span & !ev$roster),
    roster_events_off_board_in_span = sum(ev$in_span & ev$roster & !ev$on_board),
    boards = paste(sort(unique(dev$board)), collapse = "|"), devices = nrow(dev),
    devices_on_expected_grid = all(dev$gx == g$x & dev$gy == g$y), events_before_span = sum(ev$t < S0), events_after_span = sum(ev$t >= S1)))

  sm <- amv_system_messages(rec$path)
  system_messages <- cbind(key, sm[, .(t, rel_span_start_h = (t - S0) / 3600, clock = antenna_dv_format_time(t), unitLabel, SystemMsg, MsgValue1, MsgValue2, MsgValue3, file_order)])

  # silences ---------------------------------------------------------------------------------------------------------
  sp <- ev[in_span == TRUE]
  rec_sil <- amv_gaps(amv_union(sp$t, sp$e), S0, S1, 60)
  board_sil <- rbindlist(lapply(sort(unique(sp$board)), function(b) {
    x <- sp[board == b]; amv_gaps(amv_union(x$t, x$e), S0, S1, 60)[, board := b] }))
  silences <- rbindlist(list(rec_sil[, `:=`(level = "recording", board = NA_integer_)], board_sil[, level := "board"]), use.names = TRUE)
  silences[, `:=`(dur_s = e - s, clock_start = antenna_dv_format_time(s), clock_end = antenna_dv_format_time(e))]
  silences <- cbind(key[rep(1, nrow(silences))], silences)

  # per animal ---------------------------------------------------------------------------------------------------------
  ab <- list(); arb <- list(); rh <- list()
  for (a in roster$AnimalNum) {
    x <- ev[AnimalNum == a & on_board == TRUE]
    b_own <- x$own_board[1]
    det <- amv_union(x$t, x$e)
    r <- sort(rt[use == TRUE & AnimalNum == a]$t_read)
    tr <- amv_transitions(x$t, x$e, x$PositionID)
    tr[, block := blk_of(t)]
    runs <- amv_runs(x$t, x$PositionID, S0, S1, 0)
    bsil <- silences[level == "board" & board == b_own, .(s, e)]; rsil <- silences[level == "recording", .(s, e)]
    for (k in seq_len(nrow(blocks))) {
      bs <- blocks$bs[k]; be <- blocks$be[k]
      rb <- r[r >= bs & r < be]; d <- diff(rb)
      xb <- x[t >= bs & t < be]
      g60 <- amv_gaps(det, bs, be, 60); g600 <- amv_gaps(det, bs, be, 600)
      ex <- amv_age_exceedance(det, bs, be, AMV_AGE_BOUNDS)
      tb <- tr[t >= bs & t < be]
      geo <- table(factor(tb$geometry, levels = AMV_GEOMETRY_LEVELS))
      revs <- unlist(lapply(AMV_REVERSAL_BOUNDS, function(u) {
        z <- table(factor(tb$geometry[tb$returns %in% TRUE & tb$to_dur_s < u], levels = AMV_GEOMETRY_LEVELS))
        stats::setNames(c(sum(z), as.numeric(z)), paste0("reversals_lt", u, "s_", c("all", AMV_GEOMETRY_LEVELS))) }))
      ab[[length(ab) + 1L]] <- data.table(
        AnimalNum = a, board = b_own, block = blocks$block[k], block_no = blocks$block_no[k], Phase = blocks$Phase[k], block_s = be - bs,
        reads = length(rb), events = nrow(xb), single_read_events = sum(xb$reads == 1),
        detected_s = amv_overlap_seconds(det, data.table(s = bs, e = be)),
        t(stats::setNames(ex, paste0("age_gt", AMV_AGE_BOUNDS, "s_s"))),
        gaps_ge60 = nrow(g60), gaps_ge60_s = sum(g60$e - g60$s), gaps_ge600 = nrow(g600), gaps_ge600_s = sum(g600$e - g600$s),
        gaps_ge60_in_board_silence_s = amv_overlap_seconds(g60, bsil), gaps_ge60_in_recording_silence_s = amv_overlap_seconds(g60, rsil),
        longest_gap_s = if (nrow(g60)) max(g60$e - g60$s) else NA_real_,
        read_gap_median_s = if (length(d)) median(d) else NA_real_, read_gap_q90_s = if (length(d)) quantile(d, 0.9, names = FALSE) else NA_real_,
        read_gap_q99_s = if (length(d)) quantile(d, 0.99, names = FALSE) else NA_real_,
        read_gaps_le0.6s_share = if (length(d)) mean(d <= 0.6) else NA_real_,
        changes = nrow(tb), t(stats::setNames(as.numeric(geo), paste0("changes_", AMV_GEOMETRY_LEVELS))),
        changes_simultaneous = sum(tb$gap_s < 0), changes_gap_le1s = sum(tb$gap_s >= 0 & tb$gap_s <= 1),
        changes_gap_1to10s = sum(tb$gap_s > 1 & tb$gap_s <= 10), changes_gap_gt10s = sum(tb$gap_s > 10),
        t(revs))
      # per reader
      rr <- amv_clip_runs(runs, data.table(s = bs - S0, e = be - S0))
      locf <- tapply(rr$e - rr$s, factor(rr$pos, levels = 1:8), sum); locf[is.na(locf)] <- 0
      rd <- tabulate(rt[use == TRUE & AnimalNum == a & t_read >= bs & t_read < be]$pid, 8)
      dsec <- vapply(1:8, function(p) { y <- xb[PositionID == p]; if (!nrow(y)) 0 else amv_overlap_seconds(amv_union(y$t, pmin(y$e, be)), data.table(s = bs, e = be)) }, 0)
      arb[[length(arb) + 1L]] <- data.table(AnimalNum = a, board = b_own, block = blocks$block[k], block_no = blocks$block_no[k], Phase = blocks$Phase[k],
                                            PositionID = 1:8, locf_s = as.numeric(locf), reads = rd, detected_s = dsec)
    }
    tr[, Phase := blocks$Phase[match(block, blocks$block)]]
    tr <- tr[!is.na(Phase) & t >= S0 & t < S1]
    lb <- function(v) cut(log10(pmax(v, 10^AMV_HIST_BREAKS[1])), AMV_HIST_BREAKS, right = FALSE, include.lowest = TRUE)
    rh[[a]] <- rbind(tr[returns %in% TRUE, .(kind = "returning", n = .N), by = .(Phase, geometry, bin = lb(to_dur_s))],
                     tr[!is.na(to_dur_s), .(kind = "all", n = .N), by = .(Phase, geometry, bin = lb(to_dur_s))])
  }
  animal_block <- cbind(key[rep(1, length(ab))], rbindlist(ab))
  animal_reader_block <- cbind(key[rep(1, length(arb))], rbindlist(arb))
  reversal_hist <- rbindlist(rh)[, .(n = sum(n)), by = .(Phase, kind, geometry, bin)]
  reversal_hist <- cbind(key[rep(1, nrow(reversal_hist))], reversal_hist)

  # reader concurrency -------------------------------------------------------------------------------------------------
  sp <- ev[in_span == TRUE]
  sp[, conc := amv_concurrency(board, PositionID, t, e_log, tag)]   # logged spans: single reads count as 75 ms
  sp[, Phase := blocks$Phase[match(blk_of(t), blocks$block)]]
  reader_concurrency <- sp[, .(events = .N, reads = sum(reads), single_read_events = sum(reads == 1), multi_read_events = sum(reads >= 2),
                               multi_read_dur_s = sum(dur_sec[reads >= 2]), multi_read_intervals = sum(reads[reads >= 2] - 1L)),
                           by = .(board, PositionID, Phase, concurrent_tags = pmin(conc, 3L))]
  reader_concurrency <- cbind(key[rep(1, nrow(reader_concurrency))], reader_concurrency)

  # co-assignment density (method A, every tag with >= 100 events on the board) -----------------------------------------
  ncell <- floor(S1 - S0)
  present <- ev[in_span == TRUE & on_board == TRUE, .N, by = .(tag, own_board, AnimalNum, roster)][N >= 100]
  cd <- list()
  for (b in sort(unique(present$own_board))) {
    pb <- present[own_board == b]
    pos <- vapply(pb$tag, function(tg) {
      x <- ev[tag == tg & on_board == TRUE]
      amv_grid_track(amv_runs(x$t, x$PositionID, S0, S1, 0), data.table(s = 0, e = ncell), ncell)$pos }, integer(ncell))
    for (i in which(pb$roster)) {
      k <- rowSums(pos[, -i, drop = FALSE] == pos[, i] & pos[, i] > 0)
      rd <- tabulate(floor(rt[use == TRUE & tag == pb$tag[i] & t_read >= S0 & t_read < S0 + ncell]$t_read - S0) + 1L, ncell)
      blk <- blk_of(S0 + seq_len(ncell) - 0.5)
      cd[[length(cd) + 1L]] <- data.table(AnimalNum = pb$AnimalNum[i], board = b, block = blk, mates_same_reader = pmin(k, 3L), assigned = pos[, i] > 0, reads = rd)[
        assigned == TRUE, .(seconds = .N, reads = sum(reads), seconds_with_read = sum(reads > 0)), by = .(AnimalNum, board, block, mates_same_reader)]
    }
  }
  coassign_density <- rbindlist(cd)
  coassign_density[, Phase := blocks$Phase[match(block, blocks$block)]]
  coassign_density <- cbind(key[rep(1, nrow(coassign_density))], coassign_density)

  # Stage 29 windows ---------------------------------------------------------------------------------------------------
  t0 <- min(antenna_dv_parse_time(vendor$DateTime))
  ws <- as.numeric(as.POSIXct(paste(format(t0, "%Y-%m-%d", tz = "UTC"), "18:30:00"), tz = "UTC"))
  if (abs(ws - S0) > 1e-6) stop(src, ": the Stage 29 anchor is not the span start.", call. = FALSE)
  wins <- data.table(window = c("active", "light"), ws = c(ws, ws + 43200), we = c(ws + 43200, ws + 86400))[we <= S1 + 1e-6]
  wm <- list(); wd <- list(); wbs <- list()
  for (w in seq_len(nrow(wins))) {
    W0 <- wins$ws[w]; W1 <- wins$we[w]; len <- W1 - W0
    seqs <- lapply(roster$AnimalNum, function(a) ev[AnimalNum == a & on_board == TRUE, .(t, e, PositionID, board)])
    names(seqs) <- roster$AnimalNum
    dets <- lapply(seqs, function(x) amv_union(x$t, x$e))
    rseqs <- lapply(roster$AnimalNum, function(a) setorder(rt[use == TRUE & AnimalNum == a, .(t = t_read, PositionID = pid)], t))
    names(rseqs) <- roster$AnimalNum
    runs_all <- lapply(seq_len(nrow(AMV_RECON)), function(k) rbindlist(lapply(roster$AnimalNum, function(a) {
      x <- if (AMV_RECON$basis[k] == "read") rseqs[[a]] else seqs[[a]]
      amv_runs(x$t, x$PositionID, W0, W1, AMV_RECON$tau[k])[, `:=`(AnimalNum = a, CC = cc, SourceFile = src, System = paste0("sys.", seqs[[a]]$board[1]))] })))
    names(runs_all) <- paste(AMV_RECON$basis, AMV_RECON$tau)
    supp <- lapply(AMV_DELTA, function(dl) lapply(dets, function(dt) amv_support(dt, W0, W1, dl)[, `:=`(s = s - W0, e = e - W0)]))
    names(supp) <- as.character(AMV_DELTA)
    for (tk in c(0, 2)) for (a in roster$AnimalNum) {   # event-start against read-level carry-forward, same tau
      ga <- amv_grid_track(runs_all[[paste("event_start", tk)]][AnimalNum == a], data.table(s = 0, e = len), len)$pos
      gb <- amv_grid_track(runs_all[[paste("read", tk)]][AnimalNum == a], data.table(s = 0, e = len), len)$pos
      both <- ga > 0 & gb > 0
      wbs[[length(wbs) + 1L]] <- data.table(window = wins$window[w], AnimalNum = a, tau = tk, seconds = sum(both),
        share_seconds_differ = mean(ga[both] != gb[both]), share_column_differs = mean(amv_column(ga[both]) != amv_column(gb[both])),
        max_abs_reader_share_diff = max(abs(tabulate(ga[both], 8) - tabulate(gb[both], 8))) / sum(both))
    }
    for (ti in seq_len(nrow(AMV_RECON))) {
      R <- runs_all[[ti]]
      fm <- mmm_bf_animal_metrics(R, AMV_BOUT_CRITERIA[["declared"]])
      for (cn in names(AMV_BOUT_CRITERIA)[-1]) {
        f2 <- mmm_bf_animal_metrics(R, AMV_BOUT_CRITERIA[[cn]])[, .(AnimalNum, v = fragmentation, n_b = n_bouts)]
        fm <- merge(fm, setnames(f2, c("v", "n_b"), paste0(c("fragmentation_", "n_bouts_"), cn)), by = "AnimalNum")
      }
      for (di in seq_along(AMV_DELTA)) {
        occ <- rbindlist(lapply(roster$AnimalNum, function(a) {
          rr <- R[AnimalNum == a]
          if (is.finite(AMV_DELTA[di])) rr <- amv_clip_runs(rr, supp[[di]][[a]])
          as.data.table(c(list(AnimalNum = a), amv_occupancy(rr))) }))
        wm[[length(wm) + 1L]] <- merge(fm[, !c("obs_s", "occupancy_dispersion", "dominant_share")], occ, by = "AnimalNum")[
          , `:=`(window = wins$window[w], basis = AMV_RECON$basis[ti], tau = AMV_RECON$tau[ti], delta = AMV_DELTA[di], window_s = len,
                 crossing_rate_supported = n_events / (obs_s / 3600))]
      }
    }
    # dyads within each board (tracked roster animals only, as Stage 29)
    for (b in sort(unique(runs_all[[1]]$System))) {
      ids <- sort(unique(runs_all[[1]][System == b]$AnimalNum))
      if (length(ids) < 2) next
      pairs <- t(utils::combn(ids, 2))
      exact <- rbindlist(lapply(c("event_start 0", "event_start 2", "read 0", "read 2"), function(rk)
        mmm_bf_dyads(runs_all[[rk]][System == b])[, recon := rk]))
      for (ci in seq_len(nrow(AMV_COASSIGN))) {
        basis <- AMV_COASSIGN$basis[ci]; tau <- AMV_COASSIGN$tau[ci]; dl <- AMV_COASSIGN$delta[ci]
        R <- runs_all[[paste(basis, tau)]]
        tracks <- lapply(ids, function(a) amv_grid_track(R[AnimalNum == a], supp[[as.character(dl)]][[a]], len))
        names(tracks) <- ids
        for (p in seq_len(nrow(pairs))) {
          ta <- tracks[[pairs[p, 1]]]; tb <- tracks[[pairs[p, 2]]]
          st <- amv_dyad_shift_table(ta, tb, c(0, AMV_SHIFTS_SHORT, AMV_SHIFTS_LONG))
          st[, share := same / joint]; st[, share_col := same_col / joint]; st[, share_adj := adj_col / joint]
          sh <- st[shift %in% AMV_SHIFTS_SHORT]; lg <- st[shift %in% AMV_SHIFTS_LONG]
          ind <- amv_dyad_independence(ta, tb)
          ex <- exact[recon == paste(basis, tau) & A == pairs[p, 1] & B == pairs[p, 2]]
          wd[[length(wd) + 1L]] <- data.table(
            window = wins$window[w], System = b, A = pairs[p, 1], B = pairs[p, 2], basis = basis, tau = tau, delta = dl,
            joint_s = st$joint[1], same_s = st$same[1], same_col_s = st$same_col[1], adj_col_s = st$adj_col[1],
            indep_same = ind$same, indep_same_col = ind$same_col, indep_adj_col = ind$adj_col,
            short_null_same_mean = mean(sh$share, na.rm = TRUE), short_null_same_q95 = quantile(sh$share, 0.95, na.rm = TRUE, names = FALSE),
            short_null_same_col_mean = mean(sh$share_col, na.rm = TRUE), short_null_adj_col_mean = mean(sh$share_adj, na.rm = TRUE),
            long_null_same_mean = mean(lg$share, na.rm = TRUE), long_null_same_q95 = quantile(lg$share, 0.95, na.rm = TRUE, names = FALSE),
            long_null_same_col_mean = mean(lg$share_col, na.rm = TRUE), long_null_adj_col_mean = mean(lg$share_adj, na.rm = TRUE),
            exact_same_s = if (is.infinite(dl) && nrow(ex)) ex$same_s else NA_real_, exact_obs_s = if (is.infinite(dl) && nrow(ex)) ex$obs_s else NA_real_)
        }
      }
    }
  }
  window_metrics <- cbind(key[rep(1, length(wm))], rbindlist(wm, use.names = TRUE))
  window_dyads <- cbind(key[rep(1, length(wd))], rbindlist(wd))
  window_basis <- cbind(key[rep(1, length(wbs))], rbindlist(wbs))
  list(integrity = integrity, system_messages = system_messages, silences = silences, animal_block = animal_block,
       animal_reader_block = animal_reader_block, reader_concurrency = reader_concurrency, coassign_density = coassign_density,
       reversal_hist = reversal_hist, window_metrics = window_metrics, window_dyads = window_dyads, window_basis = window_basis)
}

if (sys.nframe() == 0L) {
  data.table::setDTthreads(1L)
  for (f in c("Functions/project_paths.R", "Functions/animalpos_preprocessing_helpers.R", "Functions/behavioral_dynamics_helpers.R",
              "Functions/antenna_log_helpers.R", "Functions/animalpos_reconstruction.R", "Functions/antenna_data_version.R",
              "Functions/rfid_binfree_metrics.R", "Functions/antenna_measurement.R")) source(f)
  LIVE <- MMM_PROJECT_ROOT_DEFAULT
  BEH <- dirname(LIVE)
  OUT <- Sys.getenv("MMM_AMV_OUT", unset = file.path(BEH, "RANM", "measurement_validation"))
  ONLY <- Sys.getenv("MMM_AMV_ONLY", unset = "")
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  parts <- toupper(strsplit(norm(OUT), "/", fixed = TRUE)[[1]])
  if (startsWith(tolower(norm(OUT)), tolower(norm(LIVE))) || any(parts %in% c(toupper(basename(norm(LIVE))), "RANT", "RANU", "RANV", "RANA")))
    stop("MMM_AMV_OUT lies inside the live root or a data-version root: ", OUT, call. = FALSE)
  VENDOR_DIR <- file.path(LIVE, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "preprocessed_data")
  TAG_DIR <- file.path(LIVE, "MMMSociability", "raw_data")
  RAW_DIR <- file.path(dirname(dirname(dirname(LIVE))), "Raw Data", "Behavior", "RFID", "MMMdata")
  for (d in c(VENDOR_DIR, TAG_DIR, RAW_DIR)) if (!dir.exists(d)) stop("Missing input folder: ", d, call. = FALSE)
  code <- c("Testing/audits/audit_antenna_measurement_validation.R", "Functions/antenna_measurement.R", "Functions/antenna_log_helpers.R",
            "Functions/antenna_data_version.R", "Functions/animalpos_reconstruction.R", "Functions/rfid_binfree_metrics.R",
            "Functions/animalpos_preprocessing_helpers.R")
  code_sha <- vapply(code, function(f) digest::digest(file = f, algo = "sha256"), "")
  marker <- paste(sprintf("%s %s", code_sha, code), collapse = "\n")
  vendor_files <- sort(list.files(VENDOR_DIR, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$", full.names = TRUE))
  if (length(vendor_files) != 24) stop("Expected 24 vendor files, found ", length(vendor_files), call. = FALSE)
  ids <- sub("^E9_SIS_(B[0-9]+)_(CC[0-9]+)_.*$", "\\1_\\2", basename(vendor_files))
  if (nzchar(ONLY)) { vendor_files <- vendor_files[grepl(ONLY, ids)]; ids <- ids[grepl(ONLY, ids)] }
  for (i in seq_along(vendor_files)) {
    d <- file.path(OUT, "per_recording", ids[i])
    done <- file.path(d, "DONE.txt")
    if (file.exists(done) && identical(paste(readLines(done), collapse = "\n"), marker)) { message(ids[i], ": done, skipped"); next }
    t_start <- Sys.time()
    res <- amv_process_recording(vendor_files[i], RAW_DIR, TAG_DIR)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    for (nm in names(res)) fwrite(res[[nm]], file.path(d, paste0(nm, ".csv")))
    writeLines(marker, done)
    message(sprintf("%s: %.1f min", ids[i], as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
    rm(res); gc(verbose = FALSE)
  }
  if (!nzchar(ONLY)) {
    tab <- file.path(OUT, "tables"); dir.create(tab, recursive = TRUE, showWarnings = FALSE)
    for (nm in c("integrity", "system_messages", "silences", "animal_block", "animal_reader_block", "reader_concurrency", "coassign_density",
                 "reversal_hist", "window_metrics", "window_dyads", "window_basis"))
      fwrite(rbindlist(lapply(file.path(OUT, "per_recording", ids, paste0(nm, ".csv")), fread), use.names = TRUE, fill = TRUE), file.path(tab, paste0(nm, ".csv")))
    fwrite(data.frame(item = c("status", "git_commit", code, "finished"),
                      value = c("UNREGISTERED measurement validation; descriptive", system2("git", c("-C", getwd(), "rev-parse", "HEAD"), stdout = TRUE)[1],
                                code_sha, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), file.path(tab, "provenance.csv"))
  }
}
