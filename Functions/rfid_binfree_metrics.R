# ================================================================
# Canonical bin-free window metrics (Exp9 SIS RFID)
# MMMSociability -- Functions/rfid_binfree_metrics.R
# ================================================================
# Inputs are the window streams of Functions/rfid_event_stream.R (events and merged
# same-position occupancy runs, times in seconds since the window start). Definitions
# follow MMM_BEHAVIOR_CONFIG$metrics and reproduce the audit's group-blind study
# (bout-occupancy-study/03_window_metrics.R; final-report/01_colocation_excess_and_unit.R).
# No binning; no Group column is read.
# ================================================================

MMM_BF_KEY <- c("AnimalNum", "CC")

#' Events are the run boundaries (every run after the first starts with a crossing).
mmm_bf_events_from_runs <- function(runs) {
  r <- data.table::copy(runs); data.table::setorder(r, AnimalNum, CC, s)
  r[, .(t = s[-1], from = pos[-.N], to = pos[-1]), by = MMM_BF_KEY]
}

#' Clip runs to [a, b) seconds since window start (cumulative windows).
mmm_bf_clip_runs <- function(runs, a = 0, b = Inf) {
  r <- data.table::copy(runs[e > a & s < b]); r[, `:=`(s = pmax(s, a), e = pmin(e, b))]
  r[e > s]
}

#' A->B->A reversals: a run shorter than max_s between two runs at the same position is
#' merged back into its neighbours (greedy, non-overlapping). Sensitivity S12.
mmm_bf_reversal_filter <- function(runs, max_s = 20) {
  r <- data.table::copy(runs); data.table::setorder(r, AnimalNum, CC, s)
  r[, `:=`(prev = data.table::shift(pos), nxt = data.table::shift(pos, type = "lead"), dur = e - s), by = MMM_BF_KEY]
  r[, flag := !is.na(prev) & !is.na(nxt) & prev == nxt & dur < max_s]
  r[, flag := flag & !data.table::shift(flag, fill = FALSE), by = MMM_BF_KEY]
  r[flag == TRUE, pos := prev]
  r[, rid := data.table::rleid(pos), by = MMM_BF_KEY]
  r[, .(SourceFile = SourceFile[1], System = System[1], s = min(s), e = max(e), pos = pos[1]), by = c(MMM_BF_KEY, "rid")][, rid := NULL][]
}

#' Crossing rate, occupancy dispersion and bout metrics per animal-window.
mmm_bf_animal_metrics <- function(runs, bout_criterion_s) {
  obs <- runs[, .(obs_s = sum(e - s), n_positions_occupied = data.table::uniqueN(pos)), by = MMM_BF_KEY]
  occ <- runs[, .(sec = sum(e - s)), by = c(MMM_BF_KEY, "pos")][
    , { p <- sec / sum(sec); .(occupancy_dispersion = -sum(p * log2(p)), dominant_share = max(p)) }, by = MMM_BF_KEY]
  ev <- mmm_bf_events_from_runs(runs)
  data.table::setorder(ev, AnimalNum, CC, t)
  ev[, gap := t - data.table::shift(t), by = MMM_BF_KEY]
  ev[, bout := cumsum(is.na(gap) | gap > bout_criterion_s), by = MMM_BF_KEY]
  B <- ev[, .(n = .N), by = c(MMM_BF_KEY, "bout")]
  bm <- B[, .(n_bouts = .N, n_events = sum(n), fragmentation = mean(n == 1), events_per_bout = mean(n)), by = MMM_BF_KEY]
  out <- Reduce(function(a, b) merge(a, b, by = MMM_BF_KEY, all.x = TRUE), list(obs, occ, bm))
  out[is.na(n_events), `:=`(n_events = 0L, n_bouts = 0L)]
  out[, `:=`(crossing_rate = n_events / (obs_s / 3600), bout_rate_h = n_bouts / (obs_s / 3600))]
  out[]
}

#' Dyadic overlap within cage epochs (SourceFile x System): seconds at the same position
#' and seconds both observed. Returns one row per dyad-window.
mmm_bf_dyads <- function(runs) {
  runs[, { r <- .SD; ids <- sort(unique(r$AnimalNum))
    if (length(ids) < 2) NULL else {
      pp <- t(utils::combn(ids, 2))
      data.table::rbindlist(lapply(seq_len(nrow(pp)), function(k) {
        a <- r[AnimalNum == pp[k, 1]]; b <- r[AnimalNum == pp[k, 2]]
        m <- merge(a[, .(pos, s1 = s, e1 = e)], b[, .(pos, s2 = s, e2 = e)], by = "pos", allow.cartesian = TRUE)
        same <- sum(pmax(0, pmin(m$e1, m$e2) - pmax(m$s1, m$s2)))
        obs <- max(0, min(max(a$e), max(b$e)) - max(min(a$s), min(b$s)))
        data.table::data.table(A = pp[k, 1], B = pp[k, 2], same_s = same, obs_s = obs)
      })) } },
    by = .(SourceFile, System, CC), .SDcols = c("AnimalNum", "s", "e", "pos")]
}

#' Shared zone use per animal-window = sum(same) / sum(obs) over tracked cage-mates.
mmm_bf_shared_zone <- function(dyads, animals) {
  l <- data.table::rbindlist(list(dyads[, .(AnimalNum = A, CC, same_s, obs_s)], dyads[, .(AnimalNum = B, CC, same_s, obs_s)]))
  z <- l[, .(shared_zone_use = if (sum(obs_s) > 0) sum(same_s) / sum(obs_s) else NA_real_,
             n_tracked_mates = .N, dyadic_obs_s = sum(obs_s)), by = MMM_BF_KEY]
  z <- merge(animals[, ..MMM_BF_KEY], z, by = MMM_BF_KEY, all.x = TRUE)
  z[is.na(n_tracked_mates), n_tracked_mates := 0L]
  z[]
}

#' Hardware flag: window in which the animal never occupies one of the 8 positions while
#' its cage epoch has an antenna with no occupancy at all (dead/near-dead antenna).
mmm_bf_hardware_flags <- function(runs, dead = data.frame(SourceFile = character(), System = character())) {
  d <- runs[, .(n_pos = data.table::uniqueN(pos)), by = c(MMM_BF_KEY, "SourceFile", "System")]
  d[, hardware_flag := n_pos < 8]
  d[, .(AnimalNum, CC, hardware_flag)]
}

#' All canonical window metrics for one window stream (optionally clipped / perturbed).
mmm_bf_window_table <- function(ws, bout_criterion_s, clip_h = NULL, reversal = FALSE) {
  runs <- ws$runs
  if (!is.null(clip_h)) runs <- mmm_bf_clip_runs(runs, 0, clip_h * 3600)
  if (isTRUE(reversal)) runs <- mmm_bf_reversal_filter(runs)
  am <- mmm_bf_animal_metrics(runs, bout_criterion_s)
  dy <- mmm_bf_dyads(runs)
  sz <- mmm_bf_shared_zone(dy, am)
  cage <- unique(runs[, .(AnimalNum, CC, SourceFile, System)])
  hw <- mmm_bf_hardware_flags(runs)
  out <- Reduce(function(a, b) merge(a, b, by = MMM_BF_KEY, all.x = TRUE), list(am, sz, hw, cage))
  list(animals = out[], dyads = dy[])
}
