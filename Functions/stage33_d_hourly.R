# ================================================================
# Stage 33 module D - hourly separation of cohort means in the first active phase after CC1, and recurrence across CC
# MMMSociability
# ================================================================
# Plan section 11 (docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md). Post hoc, descriptive: no CombZ, no RES/SUS,
# no SIS-minus-CON quantity anywhere. Every product is outcome-free, so the whole module runs in Phase 2; Phase 3 only
# returns the tables. Definitions only. Needs the frozen stream helpers (Functions/stage30_movement.R, stage32_windows.R,
# rfid_event_stream.R, rfid_binfree_metrics.R, behavior_analysis_config.R) and canonical_animal_id() in the calling
# environment, as the runner provides them.
# ================================================================

S33D_OUTCOME_FREE_PRODUCTS <- c("d01_window_metrics_animal", "d02_cohort_window_means", "d03_separation_by_window",
                                "d04_separation_argmax", "d05_cohort_cc_table", "d06_rank_correlations",
                                "d07_individual_stability", "d08_recording_start_lags", "d_hourly_additivity",
                                "d_light_alignment")
S33D_PLAN_LAGS_CC1 <- c(B1 = 2.754, B2 = 3.075, B3 = 6.086, B4 = 5.207, B5 = 4.719, B6 = 4.219)
S33D_SATT_DF <- c("1,1,4,4" = 1.98, "2,2,3,3" = 2.86, "3,4,4,4" = 2.95, "4,4,4,4" = 3.00)
S33D_RS_SPAN <- list(all6 = 7:13, Female = 7:15, Male = 5:13)

# ---------------------------------------------------------------- windows of one file
#' The 36 windows of one CC1 file: hr01-hr12, cum01-cum12, A1 and rs<k> = [rec_start + k h, + 1 h) for
#' k = ceiling(lag) .. floor(lag + 12) - 1 (11 windows). All POSIXct in UTC.
s33d_windows <- function(source_file, active_start, rec_start) {
  a <- active_start
  lag <- as.numeric(difftime(a, rec_start, units = "hours"))
  ks <- seq(ceiling(lag), floor(lag + 12) - 1)
  mk <- function(id, set, s, e, h = NA_integer_, k = NA_integer_)
    data.table::data.table(SourceFile = source_file, window_id = id, window_set = set, start = s, end = e, hour_index = h, k_since_start = k)
  W <- data.table::rbindlist(c(
    lapply(1:12, function(h) mk(sprintf("hr%02d", h), "clock_hour", a + (h - 1) * 3600, a + h * 3600, h = h)),
    lapply(1:12, function(h) mk(sprintf("cum%02d", h), "cumulative", a, a + h * 3600, h = h)),
    list(mk("A1", "a1", a, a + 43200)),
    lapply(ks, function(k) mk(sprintf("rs%02d", k), "since_start", rec_start + k * 3600, rec_start + (k + 1) * 3600, k = k))))
  attr(W$start, "tzone") <- "UTC"; attr(W$end, "tzone") <- "UTC"
  W[, `:=`(lag_h = lag, rec_start_utc = rec_start)][]
}

#' Records per tracked animal-hour in [s, e).
s33d_rate_records <- function(rr, s, e, n_animals) rr[t >= s & t < e, .N] / n_animals / as.numeric(difftime(e, s, units = "hours"))

#' Light-schedule alignment and recording-start burst of one raw file (records of the tracked animals only).
s33d_raw_checks <- function(rr, active_start, rec_start, tracked) {
  rt <- rr[AnimalNum %in% tracked]; nA <- length(tracked); last <- max(rr$t)
  nights <- data.table::rbindlist(lapply(1:5, function(j) {
    an <- active_start + (j - 1) * 86400
    if (an + 13 * 3600 > last) return(NULL)
    data.table::data.table(night = j, rate_1730_1830 = s33d_rate_records(rt, an - 3600, an, nA),
                           rate_1830_1930 = s33d_rate_records(rt, an, an + 3600, nA),
                           rate_0530_0630 = s33d_rate_records(rt, an + 11 * 3600, an + 12 * 3600, nA),
                           rate_0630_0730 = s33d_rate_records(rt, an + 12 * 3600, an + 13 * 3600, nA))
  }))
  nights[, `:=`(dark_onset_ratio = rate_1830_1930 / rate_1730_1830, light_onset_ratio = rate_0530_0630 / rate_0630_0730,
                in_gate = night >= 2L)]
  nights[, passed := !in_gate | (dark_onset_ratio >= 3 & light_onset_ratio >= 3)]
  first_bin <- s33d_rate_records(rt, active_start, active_start + 600, nA)
  burst <- data.table::data.table(n_tracked = nA,
    burst_first_hour_per_animal = s33d_rate_records(rt, rec_start, rec_start + 3600, nA),
    burst_second_hour_per_animal = s33d_rate_records(rt, rec_start + 3600, rec_start + 7200, nA),
    same_clock_next_day_per_animal = s33d_rate_records(rt, rec_start + 86400, rec_start + 90000, nA),
    change_day_1730_1830_per_animal = s33d_rate_records(rt, active_start - 3600, active_start, nA))
  burst[, burst_ratio := burst_first_hour_per_animal / same_clock_next_day_per_animal][, burst_flag := burst_ratio < 2]
  list(nights = nights[, first_10min_bin_rate := data.table::fifelse(night == 1L, first_bin, NA_real_)], burst = burst)
}

#' One v2 file: stream with raw seeds, windows, metrics, A1 boundary events, raw checks. `windows` = TRUE builds the 36
#' CC1 windows; for CC2-CC4 (sensitivity S7) only hr01-hr12 and A1 are built.
s33d_file <- function(v2_file, raw_dir, raw_file, tracked, cc1 = TRUE, bout_s = 39.3970988275363) {
  pre <- s30mv_read_preprocessed(v2_file)
  st <- s30mv_build_stream(pre, raw_dir)
  FW <- mmm_evs_windows(pre)
  rr <- s32w_read_raw_records(raw_file, FW$SourceFile)
  rec_start <- min(rr$t)
  W <- s33d_windows(FW$SourceFile, FW$active_start, rec_start)
  if (!cc1) W <- W[window_set %in% c("clock_hour", "a1")]
  M <- s30mv_window_metrics(st, W[, .(SourceFile, window_id, start, end)], bout_criterion_s = bout_s,
                            na_if_file_ends_early = FALSE, strict_seed = TRUE, full = TRUE)
  M <- merge(M, W[, .(window_id, window_set, hour_index, k_since_start, lag_h, rec_start_utc)], by = "window_id")
  ws <- s30mv_window_streams(st, W[window_id == "A1", .(SourceFile, window_id, start, end)])[["A1"]]
  ev <- mmm_bf_events_from_runs(ws$runs)
  rs_edges <- if (cc1) as.numeric(difftime(c(W[window_set == "since_start", start], max(W[window_set == "since_start", end])),
                                            FW$active_start, units = "secs")) else numeric()
  edges <- data.table::as.data.table(ev)[, .(n_boundary_hour_edges = sum(t %in% (3600 * 1:11)), n_boundary_rs_edges = sum(t %in% rs_edges)),
                                         by = AnimalNum]
  rc <- s33d_raw_checks(rr, FW$active_start, rec_start, tracked)
  boards <- rr[, .(first = min(t)), by = AnimalNum]
  list(metrics = M, edges = edges, raw = rc, FW = FW, rec_start = rec_start, n_unparsed = attr(rr, "n_unparsed") %s33or% 0L,
       seed_files = unique(st$seeds$raw_file), stream_ids = unique(st$pos$AnimalID), first_reads = boards,
       raw_labels = unique(rr$AnimalNum))
}

# ---------------------------------------------------------------- cohort summaries
#' Cage sufficient statistics of y within one cohort (cages in sort order): sum S, sum of squares Q, n (finite y only).
s33d_cage_stats <- function(y, cage, cages = sort(unique(cage))) {
  ok <- is.finite(y)
  data.table::data.table(cage = cages,
    S = vapply(cages, function(g) sum(y[ok & cage == g]), 0), Q = vapply(cages, function(g) sum(y[ok & cage == g]^2), 0),
    n = vapply(cages, function(g) sum(ok & cage == g), 0))
}

#' One cohort-set-metric-window summary: mean, spread, CR2 (SIS), CR1, resampling range, cage-weighted mean (S1).
s33d_summary <- function(y, cage, U = NULL, exposure = "SIS") {
  ok <- is.finite(y); yy <- y[ok]; cc <- cage[ok]
  cs <- if (length(yy)) data.table::data.table(y = yy, g = cc)[, .(m = mean(y), n = .N), keyby = g] else data.table::data.table(g = character(), m = numeric(), n = integer())
  out <- data.table::data.table(n_animals = length(yy), n_cages = nrow(cs), cage_sizes = paste(sort(cs$n), collapse = ","),
    mean = if (length(yy)) mean(yy) else NA_real_, sd_animals = if (length(yy) > 1L) stats::sd(yy) else NA_real_,
    min = if (length(yy)) min(yy) else NA_real_, max = if (length(yy)) max(yy) else NA_real_,
    cage_means = paste(signif(cs$m, 8), collapse = ";"), se_cr2 = NA_real_, df_satt = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    ci_method = "none", se_cr1 = NA_real_, ci_cr1_low = NA_real_, ci_cr1_high = NA_real_, resamp_low = NA_real_, resamp_high = NA_real_,
    mean_cage_weighted = if (nrow(cs)) mean(cs$m) else NA_real_, ci_cage_t_low = NA_real_, ci_cage_t_high = NA_real_)
  if (exposure != "SIS") { out[, ci_method := "none: single CON cage"]; return(out) }
  if (nrow(cs) < 3L) { out[, ci_method := "none: < 3 cages"]; return(out) }
  cr <- s33_cr2_mean(yy, cc)
  out[, `:=`(se_cr2 = cr$se, df_satt = cr$df, ci_low = cr$ci_low, ci_high = cr$ci_high, ci_method = "CR2_Satterthwaite_cc_cage")]
  c1 <- s33_cr1_mean(yy, cc); out[, `:=`(se_cr1 = c1$se, ci_cr1_low = c1$ci_low, ci_cr1_high = c1$ci_high)]
  ti <- s33_t_interval(mean(cs$m), stats::sd(cs$m) / sqrt(nrow(cs)), nrow(cs) - 1L)
  out[, `:=`(ci_cage_t_low = ti$ci_low, ci_cage_t_high = ti$ci_high)]
  if (!is.null(U)) {
    st <- s33d_cage_stats(y, cage, colnames(U))
    den <- as.vector(U %*% st$n); mstar <- as.vector(U %*% st$S) / den; mstar[den == 0] <- NA_real_
    p <- s33_percentile(mstar); out[, `:=`(resamp_low = p$lo, resamp_high = p$hi)]
  }
  out
}

#' Separation of SIS cohort means for one window (method of moments): S, N (CR2 noise floor), E, W, D = E / W, L.
#' `cs` = cohort rows with m, v (CR2 variance), ss (within-cohort sum of squares), n, Sex. scope: all6/all5, within_sex,
#' Female, Male.
s33d_separation <- function(cs, scope) {
  x <- if (scope %in% c("Female", "Male")) cs[Sex == scope] else cs
  K <- nrow(x); if (K < 2L) return(list(S = NA_real_, N = NA_real_, E = NA_real_, W = NA_real_, D = NA_real_, L = NA_real_))
  S <- if (scope == "within_sex") sqrt(sum(x[, (m - mean(m))^2, by = Sex]$V1) / (K - data.table::uniqueN(x$Sex))) else stats::sd(x$m)
  N <- sqrt(mean(x$v)); E <- sqrt(max(0, S^2 - N^2)); W <- sqrt(sum(x$ss) / (sum(x$n) - K))
  list(S = S, N = N, E = E, W = W, D = if (is.finite(W) && W > 0) E / W else NA_real_, L = if (all(x$m > 0)) stats::sd(log(x$m)) else NA_real_)
}

#' Per-draw separation for every window of a set: arrays of cage statistics per cohort (S, Q: G x Wn; n: G x Wn) and the
#' cohort's B x G multiplicities. Returns a B x Wn matrix of D* (drawn copies are separate clusters).
s33d_draw_D <- function(stats_by_cohort, U_by_cohort, sexes, scope) {
  co <- names(stats_by_cohort); if (scope %in% c("Female", "Male")) co <- co[sexes[co] == scope]
  K <- length(co); Wn <- ncol(stats_by_cohort[[co[1]]]$S); B <- nrow(U_by_cohort[[co[1]]])
  M <- V <- SSm <- Nn <- array(NA_real_, c(B, Wn, K))
  for (j in seq_len(K)) { s <- stats_by_cohort[[co[j]]]; U <- U_by_cohort[[co[j]]]
    Nst <- U %*% s$n; m <- (U %*% s$S) / Nst; SS <- U %*% s$Q - Nst * m^2
    v <- matrix(0, B, Wn)
    for (g in seq_len(nrow(s$S))) { ng <- matrix(s$n[g, ], B, Wn, byrow = TRUE)
      v <- v + U[, g] * (matrix(s$S[g, ], B, Wn, byrow = TRUE) - ng * m)^2 / (1 - ng / Nst) }
    M[, , j] <- m; V[, , j] <- v / Nst^2; SSm[, , j] <- SS; Nn[, , j] <- Nst }
  if (scope == "within_sex") { sx <- sexes[co]
    dev <- M; for (s in unique(sx)) { k <- which(sx == s); dev[, , k] <- M[, , k] - array(rowMeans(M[, , k, drop = FALSE], dims = 2), c(B, Wn, length(k))) }
    S2 <- rowSums(dev^2, dims = 2) / (K - length(unique(sx)))
  } else { mb <- rowMeans(M, dims = 2); S2 <- rowSums((M - array(mb, c(B, Wn, K)))^2, dims = 2) / (K - 1) }
  N2 <- rowMeans(V, dims = 2); W <- sqrt(rowSums(SSm, dims = 2) / (rowSums(Nn, dims = 2) - K))
  D <- sqrt(pmax(0, S2 - N2)) / W; D[!is.finite(D)] <- NA_real_
  D
}

# ---------------------------------------------------------------- Phase 2 engine, part 1: files, gates, d02-d04
#' Rank vector with 1 = highest.
s33d_rank_desc <- function(x) rank(-x, ties.method = "average")

s33d_engine <- function(design, ctx) {
  G <- list(); gate <- function(id, name, ok, detail = "", hard = TRUE) G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, hard = hard, detail = detail)
  an <- data.table::copy(design$animals); ep <- data.table::copy(design$episodes)
  sexes <- S33_SEX_OF_COHORT
  # ---- per-file engine (checkpointed): CC1 with 36 windows; CC2-CC4 hourly A1 (S7)
  files <- data.table::CJ(Batch = S33_COHORTS, CC = S33_CC)
  res <- lapply(seq_len(nrow(files)), function(i) {
    b <- files$Batch[i]; cc <- files$CC[i]
    v2 <- ctx$inputs[[paste0("v2_", b, "_", cc)]]; raw <- ctx$inputs[[paste0("raw_", b, "_", cc)]]
    tracked <- ep[Batch == b & CC == cc, AnimalNum]
    s33_checkpoint(ctx, "D", paste0(b, "_", cc),
                   function() s33d_file(v2, ctx$inputs$raw_dir, raw, tracked, cc1 = cc == "CC1"),
                   key_extra = list(v2 = digest::digest(file = v2, algo = "sha256"), raw = digest::digest(file = raw, algo = "sha256"),
                                    cc1 = cc == "CC1", tracked = sort(tracked)))
  })
  names(res) <- paste(files$Batch, files$CC, sep = "_")
  cc1 <- res[paste0(S33_COHORTS, "_CC1")]
  M <- data.table::rbindlist(lapply(cc1, function(r) r$metrics), fill = TRUE)
  M[, AnimalNum := as.character(AnimalNum)]
  lab <- an[pop_tracked_111 == TRUE, .(AnimalNum, Sex, Exposure = condition, CageEpisodeID_CC1 = cc1_cage)]
  M <- merge(M, lab, by = "AnimalNum")
  # ---- gates on windows and metrics
  wins <- M[, .(n = data.table::uniqueN(window_id)), by = SourceFile]
  a1s <- M[window_id == "A1", .(a1_start = min(window_start), a1_end = max(window_end)), by = SourceFile]
  wchk <- merge(M[, .(SourceFile, window_id, window_set, window_start, window_end)], a1s, by = "SourceFile")
  gate("GD-9", "36 windows per CC1 file; no H01-H13 label; no window before 18:30; since-start windows inside A1",
       c(nrow(wins) == 6L, wins$n == 36L, !any(grepl("^H(0[1-9]|1[0-3])$", M$window_id)), wchk$window_start >= wchk$a1_start,
         wchk[window_set == "since_start", window_end <= a1_end]))
  gate("GD-10", "3,996 animal-window rows (111 x 36), all status ok",
       c(nrow(M) == 3996L, M$status == "ok", data.table::uniqueN(M$AnimalNum) == 111L))
  A <- M[window_id == "A1"]; H <- M[window_set == "clock_hour"]; Cm <- M[window_set == "cumulative"]
  a1b <- an[pop_tracked_111 == TRUE, .(AnimalNum, rate_b = crossing_rate, szu_b = shared_zone_use)]
  ab <- merge(A[, .(AnimalNum, n_events, obs_s, crossing_rate, shared_zone_use, dyadic_obs_s)], a1b, by = "AnimalNum")
  ab <- merge(ab, ep[CC == "CC1", .(AnimalNum, ev_b = n_events, obs_b = obs_s)], by = "AnimalNum")
  gate("GD-12", "recomputed A1 = bundle (obs_s, rate, occupancy 1e-9; events exact; occupancy NA only for OQ770, OQ771)",
       c(nrow(ab) == 111L, abs(ab$obs_s - ab$obs_b) < 1e-9, abs(ab$crossing_rate - ab$rate_b) < 1e-9, ab$n_events == ab$ev_b,
         setequal(ab[!is.finite(shared_zone_use), AnimalNum], S33_SINGLE_CC1),
         ab[is.finite(shared_zone_use), abs(shared_zone_use - szu_b) < 1e-9]))
  # additivity audit (per animal)
  edges <- data.table::rbindlist(lapply(cc1, function(r) r$edges))
  hs <- H[, .(sum_hourly_n_events = sum(n_events), sum_hourly_obs_s = sum(obs_s),
              sum_hourly_same_s = sum(shared_zone_use * dyadic_obs_s, na.rm = TRUE)), by = AnimalNum]
  cs <- H[order(hour_index)][, .(hour_index, ev_cum = cumsum(n_events), obs_cum = cumsum(obs_s)), by = AnimalNum]
  ccm <- merge(cs, Cm[, .(AnimalNum, hour_index, n_events, obs_s)], by = c("AnimalNum", "hour_index"))
  cum_ok <- ccm[, .(cum_ok = all(ev_cum == n_events) && max(abs(obs_cum - obs_s)) < 1e-6), by = AnimalNum]
  add <- merge(A[, .(AnimalNum, Batch, n_events_A1_recomputed = n_events, obs_s_A1_recomputed = obs_s, crossing_rate, shared_zone_use,
                     same_s_A1 = shared_zone_use * dyadic_obs_s)], hs, by = "AnimalNum")
  add <- merge(add, edges, by = "AnimalNum", all.x = TRUE)
  add[is.na(n_boundary_hour_edges), n_boundary_hour_edges := 0L][is.na(n_boundary_rs_edges), n_boundary_rs_edges := 0L]
  add <- merge(add, ab[, .(AnimalNum, rate_b, szu_b, obs_b)], by = "AnimalNum")
  add <- merge(add, cum_ok, by = "AnimalNum")
  add[, `:=`(n_events_A1_bundle = round(rate_b * obs_b / 3600), obs_s_A1_bundle = obs_b)]
  add[, `:=`(events_pass = (sum_hourly_n_events + n_boundary_hour_edges == n_events_A1_recomputed) & (n_events_A1_recomputed == n_events_A1_bundle),
             obs_abs_diff_s = abs(sum_hourly_obs_s - obs_s_A1_recomputed), rate_abs_diff_vs_bundle = abs(crossing_rate - rate_b),
             szu_abs_diff_vs_bundle = abs(shared_zone_use - szu_b),
             same_s_abs_diff = data.table::fifelse(is.finite(same_s_A1), abs(sum_hourly_same_s - same_s_A1), 0),
             cum12_equals_A1 = cum_ok)]
  add[, all_pass := events_pass & obs_abs_diff_s < 1e-6 & rate_abs_diff_vs_bundle < 1e-9 & same_s_abs_diff < 1e-6 & cum12_equals_A1]
  add[, c("rate_b", "szu_b", "obs_b", "cum_ok") := NULL]
  gate("GD-11", "additivity: hourly events + edge events = A1 = bundle; hourly obs sum = A1 (1e-6 s); cumulative = running sums; same seconds add up",
       c(nrow(add) == 111L, add$all_pass))
  # complements (rate outside the window but inside A1; for cum_h that is hours h+1..12)
  M <- merge(M, A[, .(AnimalNum, evA = n_events, obsA = obs_s)], by = "AnimalNum")
  M[, `:=`(n_events_complement = evA - n_events, obs_s_complement = obsA - obs_s)]
  M[, rate_complement := data.table::fifelse(obs_s_complement > 0, n_events_complement / (obs_s_complement / 3600), NA_real_)]
  M[, c("evA", "obsA") := NULL]
  list(M = M, res = res, add = add, gates = G)
}

# ---------------------------------------------------------------- Phase 2 engine, part 2: lags, light, resampling, d02
s33d_lags_light <- function(eng, design) {
  G <- list(); gate <- function(id, name, ok, detail = "", hard = TRUE) G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, hard = hard, detail = detail)
  ep <- design$episodes; res <- eng$res
  lagt <- data.table::rbindlist(lapply(names(res), function(k) { r <- res[[k]]; b <- sub("_.*$", "", k); cc <- sub("^.*_", "", k)
    fr <- r$first_reads; anchor <- r$FW$active_start; trk <- ep[Batch == b & CC == cc, AnimalNum]
    lag_tr <- as.numeric(difftime(anchor, fr[AnimalNum %in% trk, first], units = "hours"))
    data.table::data.table(Batch = b, CC = cc, SourceFile = r$FW$SourceFile, raw_first_utc = r$rec_start, anchor_utc = anchor,
      lag_h = as.numeric(difftime(anchor, r$rec_start, units = "hours")),
      animal_first_read_lag_min_h = min(lag_tr), animal_first_read_lag_max_h = max(lag_tr),
      file_t0_utc = r$FW$t0, t0_offset_s = as.numeric(difftime(r$FW$t0, anchor, units = "secs")),
      dst_state = if (as.POSIXlt(as.POSIXct(paste(format(anchor, "%Y-%m-%d"), "12:00:00"), tz = "Europe/Berlin"))$isdst > 0) "summer" else "winter",
      n_raw_labels = length(r$raw_labels), n_tracked = length(trk),
      untracked_labels = paste(sort(setdiff(r$raw_labels, trk)), collapse = ";")) [, names(r$raw$burst) := r$raw$burst] }))
  lagt <- merge(lagt, design$lags[, .(Batch, CC, board_first_delay_h)], by = c("Batch", "CC"))
  gate("GD-13", "CC1 lags = the plan (0.001 h)", abs(lagt[CC == "CC1"][order(Batch), lag_h] - S33D_PLAN_LAGS_CC1) <= 0.001)
  untr <- lagt[untracked_labels != "", .(Batch, CC, untracked_labels)]
  gate("GD-7", "untracked raw labels only at CC1 (OQ751, OQ752, OR567, 655); no excluded animal in a stream",
       c(nrow(untr) > 0L, all(untr$CC == "CC1"), setequal(unlist(strsplit(untr$untracked_labels, ";")), c("OQ751", "OQ752", "OR567", "655")),
         !any(unlist(lapply(res, function(r) r$stream_ids)) %in% design$sets$EXCLUDED)))
  light <- data.table::rbindlist(lapply(names(res), function(k) data.table::copy(res[[k]]$raw$nights)[, `:=`(Batch = sub("_.*$", "", k), CC = sub("^.*_", "", k))]))
  light <- merge(light, lagt[, .(Batch, CC, dst_state, n_tracked)], by = c("Batch", "CC"))
  gate("GD-15", "light alignment: every night after A1 has 18:30-19:30 / 17:30-18:30 >= 3 and 05:30-06:30 / 06:30-07:30 >= 3 (24 files)",
       c(data.table::uniqueN(light[, .(Batch, CC)]) == 24L, light[in_gate == TRUE, .N] > 0L, light$passed))
  gate("GD-16", "recording-start burst ratio >= 2 (recorded, not stopping)", !lagt$burst_flag, hard = FALSE,
       detail = paste(lagt[burst_flag == TRUE, paste(Batch, CC)], collapse = ","))
  list(lagt = lagt, light = light, gates = G)
}

#' One resampling stream (seed D_resample): 24 cells in the order CC1 B1..B6, CC2 .., CC3 .., CC4 .., SIS cages sorted.
s33d_resampling <- function(design, ctx) {
  G <- list(); gate <- function(id, name, ok, detail = "") G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, detail = detail)
  sis_ep <- design$episodes[condition == "SIS"]
  cells <- unlist(lapply(S33_CC, function(cc) stats::setNames(vapply(S33_COHORTS, function(b)
    data.table::uniqueN(sis_ep[Batch == b & CC == cc, CageEpisodeID]), 1L), paste(cc, S33_COHORTS, sep = "_"))))
  IDX <- s33_resample_index(cells, ctx$B[["nonparametric"]], ctx$seeds[["D_resample"]])
  U <- lapply(names(IDX), function(k) { b <- sub("^CC[0-9]_", "", k); cc <- sub("_B[0-9]$", "", k)
    u <- s33_multiplicity(IDX[[k]], cells[[k]]); colnames(u) <- sort(unique(sis_ep[Batch == b & CC == cc, CageEpisodeID])); u })
  names(U) <- names(IDX)
  # GD-18: two-way weights with CCk multiplicity 1 = one-way; exact 35-multiset percentiles; 864 single-cage sets
  x1 <- sis_ep[Batch == "B3" & CC == "CC1"]; U1 <- U[["CC1_B3"]]
  w1 <- U1[, match(x1$CageEpisodeID, colnames(U1))]
  one <- as.vector(w1 %*% x1$crossing_rate) / rowSums(w1)
  st3 <- s33d_cage_stats(x1$crossing_rate, x1$CageEpisodeID, colnames(U1))
  oneb <- as.vector(U1 %*% st3$S) / as.vector(U1 %*% st3$n)
  grid4 <- as.matrix(expand.grid(rep(list(1:4), 4)))
  ms <- unique(t(apply(grid4, 1, function(r) tabulate(r, 4))))
  wts <- apply(ms, 1, function(u) stats::dmultinom(u, prob = rep(0.25, 4)))
  enum_m <- as.vector(ms %*% st3$S) / as.vector(ms %*% st3$n)
  oo <- order(enum_m); cdf <- cumsum(wts[oo]); ex <- enum_m[oo][c(which(cdf >= 0.025)[1], which(cdf >= 0.975)[1])]
  dv <- sort(unique(signif(enum_m, 12))); mc <- s33_percentile(oneb)
  within_one <- function(a, b) abs(findInterval(a, dv) - findInterval(b, dv)) <= 1L
  four <- sis_ep[CC == "CC1", .(n = .N), by = .(Batch, CageEpisodeID)][n == 4L, .N, by = Batch]
  gate("GD-18", "resampling: two-way with CCk multiplicity 1 = one-way; exact 35-multiset percentiles within one distinct value; 864 single-cage sets",
       c(isTRUE(all.equal(one, oneb, tolerance = 1e-12)), nrow(ms) == 35L, abs(sum(wts) - 1) < 1e-12,
         within_one(mc$lo, ex[1]), within_one(mc$hi, ex[2]), prod(four$N) == 864L))
  list(U = U, cells = cells, gates = G)
}

#' d02: cohort means per window (CC1 windows; S7 hourly windows of CC2-CC4), SIS with CR2 / CR1 / resampling range,
#' CON with min-max only; plus the GD-17 check.
s33d_window_means <- function(eng, design, U) {
  G <- list(); gate <- function(id, name, ok, detail = "") G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, detail = detail)
  sexes <- S33_SEX_OF_COHORT; M <- eng$M; ep <- design$episodes
  M7 <- data.table::rbindlist(lapply(eng$res[!grepl("_CC1$", names(eng$res))], function(r) r$metrics), fill = TRUE)
  M7[, AnimalNum := as.character(AnimalNum)]
  M7 <- merge(M7, ep[, .(AnimalNum, CC, Exposure = condition, cage = CageEpisodeID)], by = c("AnimalNum", "CC"))
  summ <- function(X, cage_col, CCk) data.table::rbindlist(lapply(S33_COHORTS, function(b) data.table::rbindlist(lapply(c("SIS", "CON"), function(e)
    data.table::rbindlist(lapply(c("crossing_rate", "shared_zone_use"), function(met) {
      xb <- X[Batch == b & Exposure == e]
      Uk <- if (e == "SIS") U[[paste(CCk, b, sep = "_")]] else NULL
      out <- xb[, s33d_summary(get(met), get(cage_col), Uk, e), by = .(window_set, window_id, hour_index, k_since_start, lag_h)]
      out[, `:=`(Batch = b, Sex = sexes[[b]], Exposure = e, metric = met, CC = CCk)] }))))))
  d02 <- rbind(summ(M, "CageEpisodeID_CC1", "CC1"),
               data.table::rbindlist(lapply(c("CC2", "CC3", "CC4"), function(k) summ(M7[CC == k], "cage", k))), fill = TRUE)
  d02[, known_untracked_in_cohort := data.table::fifelse(Exposure == "SIS" & Batch == "B1", 6L,
                                     data.table::fifelse(Exposure == "SIS" & Batch %in% c("B4", "B6") & CC == "CC1", 1L, 0L))]
  d02[, `:=`(in_common_span = window_set == "since_start" & k_since_start %in% S33D_RS_SPAN$all6,
             in_sex_span = window_set == "since_start" & k_since_start %in% unlist(S33D_RS_SPAN[sexes[Batch]]))]
  d02[window_set == "since_start", in_sex_span := k_since_start %in% S33D_RS_SPAN[[sexes[[Batch[1]]]]], by = Batch]
  d02[, t_since_rec_start_mid_h := data.table::fifelse(window_set == "since_start", k_since_start + 0.5,
                                   data.table::fifelse(window_set == "clock_hour", lag_h + hour_index - 0.5, NA_real_))]
  d02[, clock_label := data.table::fifelse(window_set == "clock_hour",
        sprintf("%02d:30-%02d:30", (18L + hour_index - 1L) %% 24L, (18L + hour_index) %% 24L), NA_character_)]
  comp <- M[, .(complement_mean = mean(rate_complement, na.rm = TRUE)), by = .(Batch, Exposure, window_id)]
  d02 <- merge(d02, comp[, .(Batch, Exposure, window_id, metric = "crossing_rate", CC = "CC1", complement_mean)],
               by = c("Batch", "Exposure", "window_id", "metric", "CC"), all.x = TRUE)
  d02[, note := data.table::fifelse(Exposure == "CON", "the single CON cage of the cohort; no interval",
                data.table::fifelse(ci_method == "none: < 3 cages", "fewer than 3 informative cages; no interval",
                                    "CR2 interval reflects within-cohort cage sampling only; resampling range about 80% coverage with 4 cages"))]
  chk <- data.table::rbindlist(lapply(S33_COHORTS, function(b) { x <- M[Batch == b & Exposure == "SIS" & window_id == "A1"]
    cr <- s33_cr2_mean(x$crossing_rate, x$CageEpisodeID_CC1)
    data.table::data.table(Batch = b, closed = sqrt(s33_cr2_mean_closed(x$crossing_rate, x$CageEpisodeID_CC1)), club = cr$se, df = cr$df,
                           sizes = paste(sort(as.integer(table(x$CageEpisodeID_CC1))), collapse = ",")) }))
  chk7 <- data.table::rbindlist(lapply(S33_COHORTS, function(b) { x <- ep[Batch == b & CC == "CC2" & condition == "SIS"]
    cr <- s33_cr2_mean(x$crossing_rate, x$CageEpisodeID)
    data.table::data.table(Batch = b, closed = sqrt(s33_cr2_mean_closed(x$crossing_rate, x$CageEpisodeID)), club = cr$se, df = cr$df,
                           sizes = paste(sort(as.integer(table(x$CageEpisodeID))), collapse = ",")) }))
  chk <- rbind(chk, chk7)
  chk[, df_expected := S33D_SATT_DF[sizes]]
  gate("GD-17", "closed-form CR2 SE = clubSandwich (1e-10); Satterthwaite df as declared for the cage-size patterns (0.01)",
       c(abs(chk$closed - chk$club) < 1e-10, is.finite(chk$df_expected), abs(chk$df - chk$df_expected) < 0.01))
  list(d02 = d02, gates = G)
}

# ---------------------------------------------------------------- Phase 2 engine, part 3: separation (d03, d04)
#' Per-cohort statistics of the SIS rate per window: mean m, CR2 variance v (cage-weighted variant: variance of the cage
#' means / G), within-cohort sum of squares ss, n.
s33d_cohort_stats <- function(M, wins, variant = "primary", cohorts = S33_COHORTS) {
  data.table::rbindlist(lapply(cohorts, function(b) {
    x <- M[Batch == b & Exposure == "SIS" & window_id %in% wins & is.finite(crossing_rate)]
    x[, { y <- crossing_rate; g <- CageEpisodeID_CC1; cm <- tapply(y, g, mean)
          if (variant == "S1_cage_weighted") list(m = mean(cm), v = stats::var(cm) / length(cm), ss = sum((y - mean(y))^2), n = length(y))
          else list(m = mean(y), v = if (data.table::uniqueN(g) >= 3L) s33_cr2_mean_closed(y, g) else NA_real_, ss = sum((y - mean(y))^2), n = length(y)) },
      by = window_id][, `:=`(Batch = b, Sex = S33_SEX_OF_COHORT[[b]])] }))
}

s33d_separation_tables <- function(eng, U, design) {
  M <- eng$M; sexes <- S33_SEX_OF_COHORT
  hr <- sprintf("hr%02d", 1:12); cum <- c(sprintf("cum%02d", 1:11), "A1"); scopes4 <- c("all6", "within_sex", "Female", "Male")
  sep_rows <- function(set, wins, scopes, roles, variant, cohorts = S33_COHORTS, idcol = "window_id") {
    Mx <- if (idcol == "window_id") M else data.table::copy(M)[window_set == "since_start", window_id := sprintf("rs%02d", k_since_start)]
    cst <- s33d_cohort_stats(Mx[Batch %in% cohorts], wins, variant, cohorts)
    cmp <- Mx[Batch %in% cohorts & Exposure == "SIS" & window_id %in% wins, .(cm = mean(rate_complement, na.rm = TRUE)), by = .(Batch, window_id)]
    con <- Mx[Batch %in% cohorts & Exposure == "CON" & window_id %in% wins, .(con_m = mean(crossing_rate)), by = .(Batch, window_id)]
    data.table::rbindlist(lapply(wins, function(w) data.table::rbindlist(lapply(seq_along(scopes), function(i) {
      sc <- scopes[[i]]; x <- cst[window_id == w]; xs_ <- if (sc %in% c("Female", "Male")) x[Sex == sc] else x
      s <- s33d_separation(x, sc)
      xc <- merge(xs_, cmp[window_id == w], by = "Batch"); xs <- merge(xs_, con[window_id == w], by = "Batch")
      cr <- con[window_id == w & (if (sc %in% c("Female", "Male")) sexes[Batch] == sc else TRUE)]
      con_S <- if (nrow(cr) < 2L) NA_real_ else if (sc == "within_sex")
        sqrt(sum(cr[, (con_m - mean(con_m))^2, by = .(sx = sexes[Batch])]$V1) / (nrow(cr) - data.table::uniqueN(sexes[cr$Batch]))) else stats::sd(cr$con_m)
      rbind(data.table::data.table(metric = "crossing_rate", variant = variant, window_set = set, window_id = w, exposure = "SIS", scope = sc,
        role = roles[[i]], n_cohorts = nrow(xs_), standardized_divergence_D = s$D, excess_sd_E = s$E, between_cohort_sd_S = s$S,
        noise_floor_sd_N = s$N, within_cohort_animal_sd_W = s$W, sd_log_cohort_means_L = s$L,
        rho_with_complement = if (nrow(xc) >= 3L) stats::cor(xc$m, xc$cm, method = "spearman") else NA_real_,
        rho_SIS_vs_CON_order = if (nrow(xs) >= 3L) stats::cor(xs$m, xs$con_m, method = "spearman") else NA_real_),
        data.table::data.table(metric = "crossing_rate", variant = variant, window_set = set, window_id = w, exposure = "CON", scope = sc,
          role = "supplementary", n_cohorts = nrow(cr), between_cohort_sd_S = con_S), fill = TRUE) }))))
  }
  rsid <- function(ks) sprintf("rs%02d", ks)
  d03 <- rbind(
    sep_rows("clock_hour", hr, scopes4, c("primary", "primary", "supplementary", "supplementary"), "primary"),
    sep_rows("cumulative", cum, scopes4, rep("supplementary", 4), "primary"),
    sep_rows("clock_hour", hr, scopes4, rep("supplementary", 4), "S1_cage_weighted"),
    sep_rows("clock_hour", hr, c("all6", "within_sex"), rep("supplementary", 2), "S3_without_B1", cohorts = S33_COHORTS[-1]),
    sep_rows("since_start", rsid(S33D_RS_SPAN$all6), c("within_sex", "all6"), c("primary", "supplementary"), "primary", idcol = "k"),
    sep_rows("since_start", rsid(S33D_RS_SPAN$Female), "Female", "primary", "primary", idcol = "k"),
    sep_rows("since_start", rsid(S33D_RS_SPAN$Male), "Male", "primary", "primary", idcol = "k"), fill = TRUE)
  d03[variant == "S3_without_B1" & scope == "all6", scope := "all_without_B1"]
  # like-for-like single-cage reference for the CON rows (CC1, 864 sets of one 4-animal SIS cage per cohort)
  sis1 <- design$episodes[CC == "CC1" & condition == "SIS"]
  four <- sis1[, .N, by = CageEpisodeID][N == 4L, CageEpisodeID]
  for (w in c(hr, cum)) {
    cm <- M[window_id == w & Exposure == "SIS" & CageEpisodeID_CC1 %in% four, .(m = mean(crossing_rate)), by = .(Batch, CageEpisodeID_CC1)]
    grid <- as.matrix(expand.grid(lapply(S33_COHORTS, function(b) cm[Batch == b, m])))
    sds <- apply(grid, 1, stats::sd)
    d03[exposure == "CON" & scope == "all6" & window_id == w & variant == "primary",
        `:=`(ref_single_cage_sd_mean = mean(sds), ref_single_cage_sd_p025 = unname(stats::quantile(sds, 0.025, type = 7)),
             ref_single_cage_sd_p975 = unname(stats::quantile(sds, 0.975, type = 7)), ref_n_sets = nrow(grid))]
  }
  d03[, note := data.table::fifelse(exposure == "CON", "between-cohort SD of the six single CON cages; compare with the single-cage reference only",
                                    "descriptive dispersion of cohort means; no interval (six cohorts)")]
  # argmax of D under CC1-cage resampling (within cohort) and leave one cohort out
  arg_rows <- function(set, wins, sc, label) {
    Mx <- if (set == "since_start") data.table::copy(M)[window_set == "since_start", window_id := sprintf("rs%02d", k_since_start)] else M
    stats_by <- lapply(stats::setNames(S33_COHORTS, S33_COHORTS), function(b) {
      x <- Mx[Batch == b & Exposure == "SIS" & window_id %in% wins]; cg <- colnames(U[[paste0("CC1_", b)]])
      agg <- x[, .(S = sum(crossing_rate), Q = sum(crossing_rate^2), n = .N), by = .(g = CageEpisodeID_CC1, window_id)]
      mk <- function(col) { m <- matrix(0, length(cg), length(wins), dimnames = list(cg, wins))
        m[cbind(match(agg$g, cg), match(agg$window_id, wins))] <- agg[[col]]; m }
      list(S = mk("S"), Q = mk("Q"), n = mk("n")) })
    U_by <- lapply(stats::setNames(S33_COHORTS, S33_COHORTS), function(b) U[[paste0("CC1_", b)]])
    Dd <- s33d_draw_D(stats_by, U_by, S33_SEX_OF_COHORT, sc)
    zero <- apply(Dd, 1, function(r) all(!is.finite(r) | r == 0))
    am <- apply(Dd[!zero, , drop = FALSE], 1, function(r) which.max(replace(r, !is.finite(r), -Inf)))
    cst <- s33d_cohort_stats(Mx, wins)
    pt <- vapply(wins, function(w) s33d_separation(cst[window_id == w], sc)$D, 0)
    loco <- vapply(S33_COHORTS, function(b) { if (sc %in% c("Female", "Male") && S33_SEX_OF_COHORT[[b]] != sc) return(NA_character_)
      d <- vapply(wins, function(w) s33d_separation(cst[window_id == w & Batch != b], sc)$D, 0)
      if (all(!is.finite(d))) NA_character_ else wins[which.max(replace(d, !is.finite(d), -Inf))] }, "")
    data.table::data.table(window_set = set, scope = sc, variant = label, window_id = wins, D_point = pt,
      rank_point = rank(-pt, ties.method = "min", na.last = "keep"),
      share_argmax_resampling = tabulate(am, length(wins)) / max(1L, length(am)), n_draws_used = length(am), n_draws_all_zero = sum(zero),
      loco_argmax_count = vapply(wins, function(w) sum(loco == w, na.rm = TRUE), 0L),
      loco_left_out = vapply(wins, function(w) paste(names(loco)[loco %in% w], collapse = ","), ""))
  }
  d04 <- rbind(
    data.table::rbindlist(lapply(scopes4, function(sc) arg_rows("clock_hour", hr, sc, "all_windows"))),
    data.table::rbindlist(lapply(c("all6", "within_sex"), function(sc) arg_rows("clock_hour", hr[-1], sc, "S8_without_hr01"))),
    arg_rows("since_start", rsid(S33D_RS_SPAN$all6), "within_sex", "all_windows"),
    arg_rows("since_start", rsid(S33D_RS_SPAN$all6), "all6", "all_windows"),
    arg_rows("since_start", rsid(S33D_RS_SPAN$Female), "Female", "all_windows"),
    arg_rows("since_start", rsid(S33D_RS_SPAN$Male), "Male", "all_windows"))
  list(d03 = d03, d04 = d04)
}

# ---------------------------------------------------------------- d05 cohort x CC, d06 ranks and lags, d07 individual
s33d_cc_tables <- function(design, U, lagt) {
  ep <- data.table::copy(design$episodes); sexes <- S33_SEX_OF_COHORT
  rows5 <- list()
  for (b in S33_COHORTS) for (cc in S33_CC) for (e in c("SIS", "CON")) for (met in c("crossing_rate", "shared_zone_use")) {
    x <- ep[Batch == b & CC == cc & condition == e]
    s <- s33d_summary(x[[met]], x$CageEpisodeID, if (e == "SIS") U[[paste(cc, b, sep = "_")]] else NULL, e)
    x5 <- x[in_S13 == FALSE]; s5 <- s33d_summary(x5[[met]], x5$CageEpisodeID, NULL, e)
    unt <- if (e == "SIS" && b == "B1") 6L else if (e == "SIS" && b %in% c("B4", "B6") && cc == "CC1") 1L else 0L
    s[, `:=`(Batch = b, Sex = sexes[[b]], CC = cc, exposure_set = e, metric = met, n_hardware_flagged = sum(x$hardware_flag %in% TRUE),
             known_untracked_in_cohort = unt, n_S13 = sum(x$in_S13), mean_S13 = s5$mean, ci_low_S13 = s5$ci_low, ci_high_S13 = s5$ci_high)]
    rows5[[length(rows5) + 1L]] <- s
  }
  d05 <- data.table::rbindlist(rows5)
  d05[, `:=`(dev_from_6cohort_cc_mean = mean - mean(mean), rank_of_6 = s33d_rank_desc(mean)), by = .(CC, exposure_set, metric)]
  d05[, `:=`(dev_from_own_sex_3cohort_cc_mean = mean - mean(mean), rank_of_3_within_sex = s33d_rank_desc(mean)), by = .(CC, exposure_set, metric, Sex)]
  d05 <- merge(d05, lagt[, .(Batch, CC, SourceFile, rec_start_utc = raw_first_utc, lag_h, board_first_delay_h, t0_offset_s, dst_state)],
               by = c("Batch", "CC"))
  # d06: rank recurrence of cohort means, CC1 vs CCk; Kendall W; tau-b; lag association
  cm <- function(x) x[, .(m = mean(get(met_cur), na.rm = TRUE)), by = .(Batch, CC)]
  rows <- list(); add_row <- function(...) rows[[length(rows) + 1L]] <<- data.table::data.table(...)
  rank_txt <- function(v) paste(names(sort(v, decreasing = TRUE)), collapse = ">")
  for (met_cur in c("crossing_rate", "shared_zone_use")) for (e in c("SIS", "CON")) {
    base <- ep[condition == e]
    variants <- list(raw = base, sex_centred = base, without_B1 = base[Batch != "B1"], S13 = base[in_S13 == FALSE], S1_cage_weighted = base)
    if (met_cur == "crossing_rate" && e == "SIS") for (b in S33_COHORTS) variants[[paste0("loco_", b)]] <- base[Batch != b]
    for (vn in names(variants)) {
      x <- variants[[vn]]
      mm <- if (vn == "S1_cage_weighted") x[, .(cg = mean(get(met_cur), na.rm = TRUE)), by = .(Batch, CC, CageEpisodeID)][, .(m = mean(cg)), by = .(Batch, CC)] else cm(x)
      if (vn == "sex_centred") mm[, m := m - mean(m), by = .(CC, sx = sexes[Batch])]
      wide <- data.table::dcast(mm, Batch ~ CC, value.var = "m")
      for (k in 2:4) { a <- stats::setNames(wide$CC1, wide$Batch); bb <- stats::setNames(wide[[paste0("CC", k)]], wide$Batch)
        ra <- s33d_rank_desc(a); rb <- s33d_rank_desc(bb)
        add_row(metric = met_cur, exposure_set = e, variant = vn, comparison = paste0("CC1_vs_CC", k), coefficient = "spearman_rho",
                n_units = length(a), value = stats::cor(a, bb, method = "spearman"), max_abs_rank_shift = max(abs(ra - rb)),
                n_cohorts_same_rank = sum(ra == rb), ranks_first = rank_txt(a), ranks_second = rank_txt(bb))
        add_row(metric = met_cur, exposure_set = e, variant = vn, comparison = paste0("CC1_vs_CC", k), coefficient = "kendall_tau_b",
                n_units = length(a), value = stats::cor(a, bb, method = "kendall")) }
      R <- as.matrix(wide[, S33_CC, with = FALSE]); rk <- apply(R, 2, s33d_rank_desc); n <- nrow(R)
      Wk <- 12 * sum((rowSums(rk) - 4 * (n + 1) / 2)^2) / (4^2 * (n^3 - n))
      add_row(metric = met_cur, exposure_set = e, variant = vn, comparison = "kendall_W_CC1_CC4", coefficient = "kendall_W", n_units = n, value = Wk)
    }
  }
  # lag alignment (E7): per CC Spearman of lag and SIS mean, raw and sex-centred; double-centred over the 24 cells
  mm <- ep[condition == "SIS", .(m = mean(crossing_rate)), by = .(Batch, CC)]
  lm2 <- merge(mm, lagt[, .(Batch, CC, lag_h)], by = c("Batch", "CC"))
  for (cc in S33_CC) { x <- lm2[CC == cc]
    add_row(metric = "crossing_rate", exposure_set = "SIS", variant = "raw", comparison = paste0("lag_vs_mean_", cc), coefficient = "spearman_rho",
            n_units = nrow(x), value = stats::cor(x$lag_h, x$m, method = "spearman"))
    xs <- data.table::copy(x)[, `:=`(lag_c = lag_h - mean(lag_h), m_c = m - mean(m)), by = .(sx = sexes[Batch])]
    add_row(metric = "crossing_rate", exposure_set = "SIS", variant = "sex_centred", comparison = paste0("lag_vs_mean_", cc), coefficient = "spearman_rho",
            n_units = nrow(xs), value = stats::cor(xs$lag_c, xs$m_c, method = "spearman")) }
  dc <- data.table::copy(lm2)
  for (v in c("lag_h", "m")) { dc[, (v) := get(v) - mean(get(v)), by = Batch]; dc[, (v) := get(v) - mean(get(v)), by = CC] }
  add_row(metric = "crossing_rate", exposure_set = "SIS", variant = "raw", comparison = "lag_vs_mean_double_centred", coefficient = "pearson_r",
          n_units = nrow(dc), value = stats::cor(dc$lag_h, dc$m))
  add_row(metric = "crossing_rate", exposure_set = "SIS", variant = "raw", comparison = "lag_vs_mean_double_centred", coefficient = "spearman_rho",
          n_units = nrow(dc), value = stats::cor(dc$lag_h, dc$m, method = "spearman"))
  d06 <- data.table::rbindlist(rows, fill = TRUE)
  # S9: two-way (CC1 x CCk) cage-resampling ranges for the SIS rho rows (raw, crossing_rate and occupancy)
  sis <- ep[condition == "SIS"]
  for (met_cur in c("crossing_rate", "shared_zone_use")) for (k in 2:4) {
    cck <- paste0("CC", k); stat <- matrix(NA_real_, nrow(U[["CC1_B1"]]), 6); dropped <- 0L
    ms1 <- msk <- matrix(NA_real_, nrow(U[["CC1_B1"]]), 6)
    for (j in seq_along(S33_COHORTS)) { b <- S33_COHORTS[j]
      x1 <- sis[Batch == b & CC == "CC1"]; xk <- sis[Batch == b & CC == cck]
      xx <- merge(x1[, .(AnimalNum, y1 = get(met_cur), c1 = CageEpisodeID)], xk[, .(AnimalNum, yk = get(met_cur), ck = CageEpisodeID)], by = "AnimalNum")
      xx <- xx[is.finite(y1) & is.finite(yk)]
      U1 <- U[[paste0("CC1_", b)]]; Uk <- U[[paste0(cck, "_", b)]]
      Wt <- U1[, match(xx$c1, colnames(U1)), drop = FALSE] * Uk[, match(xx$ck, colnames(Uk)), drop = FALSE]
      rs <- rowSums(Wt); ms1[, j] <- as.vector(Wt %*% xx$y1) / rs; msk[, j] <- as.vector(Wt %*% xx$yk) / rs }
    ok <- stats::complete.cases(ms1, msk)
    rho <- vapply(which(ok), function(i) stats::cor(ms1[i, ], msk[i, ], method = "spearman"), 0)
    p <- s33_percentile(rho)
    d06[metric == met_cur & exposure_set == "SIS" & variant == "raw" & comparison == paste0("CC1_vs_CC", k) & coefficient == "spearman_rho",
        `:=`(resampling_low = p$lo, resampling_high = p$hi, n_draws_used = sum(ok),
             note = paste0("within-cohort two-way cage-sampling sensitivity; ", sum(!ok), " draws dropped (a zero-weight cohort)"))]
  }
  # d07: individual stability within cohort (SIS animals), Spearman per cohort and pooled within-cohort z Pearson
  d07 <- data.table::rbindlist(lapply("crossing_rate", function(met) data.table::rbindlist(lapply(2:4, function(k) {
    cck <- paste0("CC", k)
    xx <- merge(sis[CC == "CC1", .(AnimalNum, Batch, y1 = get(met), c1 = CageEpisodeID)], sis[CC == cck, .(AnimalNum, yk = get(met), ck = CageEpisodeID)], by = "AnimalNum")
    xx <- xx[is.finite(y1) & is.finite(yk)]
    per <- xx[, { v <- if (.N >= 3L) stats::cor(y1, yk, method = "spearman") else NA_real_
      U1 <- U[[paste0("CC1_", Batch[1])]]; Uk <- U[[paste0(cck, "_", Batch[1])]]
      Wt <- U1[, match(c1, colnames(U1)), drop = FALSE] * Uk[, match(ck, colnames(Uk)), drop = FALSE]
      dr <- apply(Wt, 1, function(w) { if (sum(w > 0) < 3L) return(NA_real_); idx <- rep(seq_along(w), w)
        if (data.table::uniqueN(y1[idx]) < 2L || data.table::uniqueN(yk[idx]) < 2L) return(NA_real_)
        stats::cor(y1[idx], yk[idx], method = "spearman") })
      p <- s33_percentile(dr)
      list(n_animals = .N, coefficient = "spearman_rho", value = v, resampling_low = p$lo, resampling_high = p$hi, n_finite_draws = sum(is.finite(dr))) },
      by = .(scope = Batch)]
    z <- data.table::copy(xx)[, `:=`(z1 = (y1 - mean(y1)) / stats::sd(y1), zk = (yk - mean(yk)) / stats::sd(yk)), by = Batch]
    pooled <- data.table::data.table(scope = "pooled_within_cohort", n_animals = nrow(z), coefficient = "pearson_r_within_z",
                                     value = stats::cor(z$z1, z$zk), resampling_low = NA_real_, resampling_high = NA_real_, n_finite_draws = NA_integer_)
    rbind(per, pooled)[, `:=`(CC_pair = paste0("CC1_vs_", cck), metric = met, level_note = "individual, within cohort")] }))))
  list(d05 = d05, d06 = d06, d07 = d07)
}

# ---------------------------------------------------------------- entry points
s33d_phase2 <- function(design, ctx) {
  eng <- s33d_engine(design, ctx)
  ll <- s33d_lags_light(eng, design)
  rs <- s33d_resampling(design, ctx)
  wm <- s33d_window_means(eng, design, rs$U)
  sp <- s33d_separation_tables(eng, rs$U, design)
  cc <- s33d_cc_tables(design, rs$U, ll$lagt)
  d01 <- eng$M[, .(AnimalNum, Sex, Batch, Exposure, CageEpisodeID_CC1, System, SourceFile, window_set, window_id,
                   window_start_utc = window_start, window_end_utc = window_end, window_s, hour_index, k_since_start, lag_h,
                   rec_start_utc, n_events, obs_s, obs_share, crossing_rate, n_events_complement, obs_s_complement, rate_complement,
                   shared_zone_use, dyadic_obs_s, n_tracked_mates, status, seeded, first_read_utc = first_read, known_at_start)]
  data.table::setorderv(d01, c("Batch", "AnimalNum", "window_set", "window_id"))
  d08 <- ll$lagt
  products <- list(d01_window_metrics_animal = d01, d02_cohort_window_means = wm$d02, d03_separation_by_window = sp$d03,
                   d04_separation_argmax = sp$d04, d05_cohort_cc_table = cc$d05, d06_rank_correlations = cc$d06,
                   d07_individual_stability = cc$d07, d08_recording_start_lags = d08,
                   d_hourly_additivity = eng$add, d_light_alignment = ll$light)
  products <- s33d_annotate(products)
  products <- lapply(products, function(x) { x <- data.table::copy(x); x[, tier := S33_TIER]; x })
  for (nm in names(products)) s33_assert_outcome_free(products[[nm]], nm)
  gates <- data.table::rbindlist(c(eng$gates, ll$gates, rs$gates, wm$gates))
  list(products = products, gates = gates, state = list())
}

s33d_phase3 <- function(design_out, p2, ctx) {
  list(tables = p2$products[S33_TABLES$D], audit = p2$products[S33_AUDIT_TABLES$D],
       gates = data.table::data.table(gate_id = character(), gate = character(), passed = logical(), hard = logical(), evaluated = logical(),
                                      n_expected = integer(), n_checked = integer(), n_ok = integer(), detail = character()),
       checkpoints = data.table::data.table())
}

#' Section-7 columns on the module D tables (level, units, lead, interval_basis, metric_label).
s33d_annotate <- function(p) {
  unit_of <- function(met) data.table::fifelse(met == "crossing_rate", "position changes/hour", "shared RFID-position occupancy (share)")
  lab_of <- function(met) unname(S33_METRIC_LABEL[met])
  basis_of <- function(method) data.table::fifelse(grepl("^CR2", method), "conditional_on_cohorts", "none")
  p$d01_window_metrics_animal[, `:=`(level = "descriptive_record", metric_label = lab_of("crossing_rate"), units = "position changes/hour",
                                      lead = FALSE, interval_basis = "none")]
  p$d02_cohort_window_means[, `:=`(level = "cohort", metric_label = lab_of(metric), units = unit_of(metric), lead = FALSE,
                                    interval_basis = basis_of(ci_method))]
  p$d03_separation_by_window[, `:=`(level = "cohort", metric_label = lab_of(metric),
                                     units = "D: ratio to the within-cohort animal SD; S, N, E, W: position changes/hour",
                                     lead = variant == "primary" & window_set == "clock_hour" & exposure == "SIS" & scope %in% c("all6", "within_sex"),
                                     interval_basis = "none")]
  p$d04_separation_argmax[, `:=`(level = "cohort", metric_label = lab_of("crossing_rate"), units = "share of resampling draws",
                                  lead = variant == "all_windows" & window_set == "clock_hour" & scope %in% c("all6", "within_sex"),
                                  interval_basis = "conditional_on_cohorts")]
  p$d05_cohort_cc_table[, `:=`(level = "cohort", metric_label = lab_of(metric), units = unit_of(metric), lead = FALSE,
                                interval_basis = basis_of(ci_method))]
  p$d06_rank_correlations[, `:=`(level = "cohort", metric_label = lab_of(metric), units = "rank statistic over six cohorts",
                                  lead = variant == "raw" & exposure_set == "SIS" & metric == "crossing_rate" &
                                    coefficient %in% c("spearman_rho", "kendall_W") & !grepl("^lag", comparison),
                                  interval_basis = data.table::fifelse(is.finite(resampling_low), "conditional_on_cohorts", "none"))]
  p$d07_individual_stability[, `:=`(level = "animal_within_cohort", metric_label = lab_of(metric), units = "correlation", lead = FALSE,
                                     interval_basis = data.table::fifelse(is.finite(resampling_low), "conditional_on_cohorts", "none"))]
  p$d08_recording_start_lags[, `:=`(level = "descriptive_record", metric_label = "recording-start lag", units = "hours", lead = FALSE,
                                     interval_basis = "none")]
  p
}
