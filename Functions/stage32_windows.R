# ================================================================
# Stage 32: phase windows, the registered coverage rule and window metrics (Exp9 SIS RFID)
# MMMSociability -- Functions/stage32_windows.R
# ================================================================
# Pure constants and functions for Analysis/32_behavior_exposure_adaptation.R, implementing sections 1-2 of the FROZEN
# registry docs/STAGE32_REGISTRY_v1.0.md (sha256 cfbd954c..., commit 0970d32). Sourcing this file reads and writes nothing.
# No function here reads or returns a Group, RES/SUS or CombZ column.
#
# Phases (logger clock, tz UTC; anchor = 18:30 on the calendar day of each file's first timestamp, mmm_evs_windows()):
#   active phase k = [anchor + 24(k-1) h, anchor + 24(k-1) h + 12 h)       -> preprocessing block A<k>
#   light phase k  = [anchor + 24(k-1) h + 12 h, anchor + 24k h)             -> preprocessing block I<k+1> (Light_k = I(k+1))
# Clean phase set: CC1-CC3 A1-A4 and L1-L3; CC4 A1, L1, A2 only.
# Coverage rule (registry section 2), per animal x phase:
#   board observation interval = [first, last] raw record (raw_data AnimalPos) of any animal on the board (SourceFile x
#   System, v2 board-based System); carry-forward beyond the board's last raw record is NOT coverage.
#   complete  <=>  (a) the phase block is kept in the v2 preprocessed file
#                  (b) board first record <= phase start (A1: <= t0, as in Stage 29)
#                  (c) board last record >= phase end - tolerance (primary 600 s; strict sensitivity 0 s)
#                  (d) the phase is in the clean phase set
#              and the animal-phase is not the explicit exclusion B5|sys.1|CC4 from L2 onward (314, 318, OR620, OR630).
# Window metrics: s30mv_window_metrics(full = TRUE, na_if_file_ends_early = FALSE) (Functions/stage30_movement.R,
# unchanged), called once per phase label; metrics of an incomplete animal-phase are set NA (never used).
# Requires: data.table; rfid_event_stream.R, rfid_binfree_metrics.R, stage30_movement.R, canonical_animal_id().
# ================================================================

S32W_PHASES <- data.table::data.table(
  phase = c(paste0("A", 1:4), paste0("L", 1:4)),
  phase_type = rep(c("active", "light"), each = 4L),
  k = rep(1:4, 2L),
  offset_s = c((0:3) * 86400, (0:3) * 86400 + 43200),
  block = c(paste0("A", 1:4), paste0("I", 2:5)))
S32W_PHASES[, phase_index := k - 1L]                  # active: phase 0..3 (A1..A4); light: light index 0..3 (L1..L4)
S32W_PHASE_S <- 43200
S32W_CLEAN <- list(CC1 = c("A1", "A2", "A3", "A4", "L1", "L2", "L3"), CC2 = c("A1", "A2", "A3", "A4", "L1", "L2", "L3"),
                   CC3 = c("A1", "A2", "A3", "A4", "L1", "L2", "L3"), CC4 = c("A1", "L1", "A2"))
S32W_TOL_PRIMARY_S <- 600
S32W_TOL_STRICT_S <- 0
S32W_EXCLUSION <- list(CageEpisodeID = "B5|sys.1|CC4", from_phase = "L2", animals = c("314", "318", "OR620", "OR630"))
S32W_METRICS <- c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation")
S32W_KEEP_COLS <- c("n_events", "obs_s", "crossing_rate", "obs_share", "status", "n_positions_occupied", "occupancy_dispersion",
                    "dominant_share", "n_bouts", "fragmentation", "events_per_bout", "bout_rate_h", "shared_zone_use",
                    "n_tracked_mates", "dyadic_obs_s", "hardware_flag", "first_read", "last_read", "file_t0", "file_last")
# A1 gate columns against ebb_v101 B1_animal_longitudinal (group-blind columns only)
S32W_REF_COLS <- c("AnimalNum", "CC", "CageEpisodeID", "n_in_cage", "n_tracked_mates", "hardware_flag", "obs_s", "n_events", "n_bouts",
                   "crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation", "events_per_bout", "light_phase_crossing_rate")

#' Phase windows per SourceFile from the canonical file windows (mmm_evs_windows()): one row per SourceFile x phase.
s32w_phase_windows <- function(file_windows, phases = S32W_PHASES) {
  fw <- data.table::as.data.table(file_windows)
  need <- c("SourceFile", "Batch", "CC", "t0", "active_start")
  miss <- setdiff(need, names(fw)); if (length(miss)) stop("file windows lack: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!identical(attr(fw$active_start, "tzone"), "UTC")) stop("active_start must be POSIXct UTC (logger clock).", call. = FALSE)
  out <- data.table::rbindlist(lapply(seq_len(nrow(phases)), function(i) {
    p <- phases[i]
    fw[, .(SourceFile, Batch, CC, t0, anchor = active_start, phase = p$phase, phase_type = p$phase_type, k = p$k,
           phase_index = p$phase_index, block = p$block, offset_s = p$offset_s,
           start = active_start + p$offset_s, end = active_start + p$offset_s + S32W_PHASE_S)]
  }))
  attr(out$start, "tzone") <- "UTC"; attr(out$end, "tzone") <- "UTC"
  data.table::setorder(out, SourceFile, offset_s)
  out[]
}

#' Clean-phase membership (registry section 1).
s32w_in_clean <- function(CC, phase, clean = S32W_CLEAN)
  mapply(function(cc, p) !is.null(clean[[cc]]) && p %in% clean[[cc]], as.character(CC), as.character(phase), USE.NAMES = FALSE)

#' Preprocessing block label of each preprocessed row: Active -> A<ConsecActive>, Inactive -> I<ConsecInactive>.
s32w_block_label <- function(Phase, ConsecActive, ConsecInactive) {
  if (!all(Phase %in% c("Active", "Inactive"))) stop("Unexpected Phase values: ", paste(setdiff(unique(Phase), c("Active", "Inactive")), collapse = ","), call. = FALSE)
  ifelse(Phase == "Active", paste0("A", ConsecActive), paste0("I", ConsecInactive))
}

#' Blocks kept per SourceFile, from the preprocessed files' own Phase / ConsecActive / ConsecInactive columns.
s32w_read_blocks <- function(files) {
  data.table::rbindlist(lapply(files, function(f) {
    d <- data.table::fread(f, select = c("Phase", "ConsecActive", "ConsecInactive"))
    d[, .(n_rows = .N), by = .(block = s32w_block_label(Phase, ConsecActive, ConsecInactive))][, SourceFile := sub("[.]gz$", "", basename(f))][]
  }))[, .(SourceFile, block, n_rows)]
}

#' Raw AnimalPos records of one raw file (the canonical seed source): record time and canonical AnimalNum (same parser
#' as the canonical seed step, MMM_EVS_SEED_PARSER). Rows whose time does not parse are dropped (counted).
s32w_read_raw_records <- function(raw_file, source_file, parser = MMM_EVS_SEED_PARSER) {
  d <- data.table::fread(raw_file, sep = ";", select = c("DateTime", "Animal"), colClasses = "character")
  d[, t := as.POSIXct(DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")]
  n_bad <- sum(is.na(d$t))
  d <- d[!is.na(t), .(SourceFile = source_file, AnimalNum = canonical_animal_id(sub(parser, "", Animal)), t)]
  attr(d, "n_unparsed") <- n_bad
  d
}

#' Board observation intervals: raw records attributed to boards through the v2 System of their AnimalNum in that
#' SourceFile (`animal_system`: SourceFile, AnimalNum, System). Records of animals without a v2 System in the file are
#' not attributed (returned as attr "unattributed").
s32w_board_intervals <- function(raw_records, animal_system) {
  rr <- data.table::as.data.table(raw_records); as_ <- unique(data.table::as.data.table(animal_system)[, .(SourceFile, AnimalNum, System)])
  if (anyDuplicated(as_[, .(SourceFile, AnimalNum)])) stop("An animal has more than one System within a SourceFile.", call. = FALSE)
  m <- merge(rr, as_, by = c("SourceFile", "AnimalNum"), all.x = TRUE)
  un <- m[is.na(System), .(n_records = .N, labels = paste(sort(unique(AnimalNum)), collapse = ";")), by = SourceFile]
  b <- m[!is.na(System), .(board_first = min(t), board_last = max(t), board_n_records = .N, board_n_animals = data.table::uniqueN(AnimalNum)),
         by = .(SourceFile, System)]
  attr(b$board_first, "tzone") <- "UTC"; attr(b$board_last, "tzone") <- "UTC"
  attr(b, "unattributed") <- un
  b[]
}

#' The per-animal x phase coverage manifest (registry section 2).
#' animals: SourceFile, Batch, CC, System, AnimalNum (the stream's animals); pw: s32w_phase_windows(); kept: s32w_read_blocks();
#' boards: s32w_board_intervals().
s32w_coverage <- function(animals, pw, kept, boards, tol_s = S32W_TOL_PRIMARY_S, strict_s = S32W_TOL_STRICT_S,
                          exclusion = S32W_EXCLUSION, clean = S32W_CLEAN) {
  an <- unique(data.table::as.data.table(animals)[, .(SourceFile, Batch, CC, System, AnimalNum)])
  if (anyDuplicated(an[, .(SourceFile, AnimalNum)])) stop("Animal listed twice within a SourceFile.", call. = FALSE)
  g <- merge(an, data.table::as.data.table(pw)[, !c("Batch", "CC")], by = "SourceFile", allow.cartesian = TRUE)
  kb <- unique(data.table::as.data.table(kept)[, .(SourceFile, block, block_kept = TRUE)])
  g <- merge(g, kb, by = c("SourceFile", "block"), all.x = TRUE); g[is.na(block_kept), block_kept := FALSE]
  g <- merge(g, data.table::as.data.table(boards)[, .(SourceFile, System, board_first, board_last, board_n_records)], by = c("SourceFile", "System"), all.x = TRUE)
  g[, CageEpisodeID := paste(Batch, System, CC, sep = "|")]
  g[, obs_start_rule := data.table::fifelse(phase == "A1", t0, start)]
  g[, board_starts_ok := !is.na(board_first) & board_first <= obs_start_rule]
  g[, board_end_margin_s := as.numeric(board_last) - as.numeric(end)]
  g[, `:=`(board_ends_ok_primary = !is.na(board_last) & board_end_margin_s >= -tol_s,
           board_ends_ok_strict = !is.na(board_last) & board_end_margin_s >= -strict_s)]
  g[, in_clean_set := s32w_in_clean(CC, phase, clean)]
  from_off <- S32W_PHASES[phase == exclusion$from_phase, offset_s]
  g[, explicit_exclusion := CageEpisodeID == exclusion$CageEpisodeID & offset_s >= from_off & AnimalNum %in% exclusion$animals]
  g[, complete_primary := block_kept & board_starts_ok & board_ends_ok_primary & in_clean_set & !explicit_exclusion]
  g[, complete_strict := block_kept & board_starts_ok & board_ends_ok_strict & in_clean_set & !explicit_exclusion]
  g[, reason_primary := s32w_reason(block_kept, board_starts_ok, board_ends_ok_primary, in_clean_set, explicit_exclusion)]
  g[, reason_strict := s32w_reason(block_kept, board_starts_ok, board_ends_ok_strict, in_clean_set, explicit_exclusion)]
  data.table::setorder(g, SourceFile, System, AnimalNum, offset_s)
  cols <- c("SourceFile", "Batch", "CC", "System", "CageEpisodeID", "AnimalNum", "phase", "phase_type", "k", "phase_index", "block",
            "t0", "start", "end", "obs_start_rule", "block_kept", "board_first", "board_last", "board_n_records", "board_starts_ok",
            "board_end_margin_s", "board_ends_ok_primary", "board_ends_ok_strict", "in_clean_set", "explicit_exclusion",
            "complete_primary", "complete_strict", "reason_primary", "reason_strict")
  g[, cols, with = FALSE]
}

s32w_reason <- function(kept, starts, ends, clean, excl) {
  r <- character(length(kept))
  add <- function(cond, txt) r[cond] <<- ifelse(nzchar(r[cond]), paste(r[cond], txt, sep = "; "), txt)
  add(!kept, "(a) block not kept in v2"); add(!starts, "(b) board starts after the phase start")
  add(!ends, "(c) board ends before the phase end - tolerance"); add(!clean, "(d) outside the clean phase set")
  add(excl, "explicit exclusion B5|sys.1|CC4 from L2")
  r[!nzchar(r)] <- "complete"
  r
}

#' Canonical window metrics for every phase label, one label (window_id) at a time, through the unchanged Stage 30 wrapper.
s32w_window_metrics <- function(stream, pw, bout_criterion_s) {
  labs <- unique(pw$phase)
  data.table::rbindlist(lapply(labs, function(p) {
    W <- pw[phase == p, .(SourceFile, window_id = phase, start, end)]
    s30mv_window_metrics(stream, W, bout_criterion_s = bout_criterion_s, na_if_file_ends_early = FALSE, strict_seed = TRUE, full = TRUE)
  }), fill = TRUE)
}

#' Long animal x CC x phase table: coverage manifest + metrics; metrics NA unless complete_primary. Also returns the
#' completeness flags so that the strict variant can subset. No Group / CombZ column.
s32w_long <- function(metrics, coverage, metric_cols = S32W_METRICS) {
  m <- data.table::as.data.table(metrics)
  if (anyDuplicated(m[, .(SourceFile, AnimalNum, window_id)])) stop("Duplicate animal-window rows in the metrics.", call. = FALSE)
  keep <- intersect(S32W_KEEP_COLS, names(m))
  x <- merge(data.table::as.data.table(coverage), m[, c("SourceFile", "AnimalNum", "window_id", keep), with = FALSE],
             by.x = c("SourceFile", "AnimalNum", "phase"), by.y = c("SourceFile", "AnimalNum", "window_id"), all.x = TRUE)
  if (nrow(x) != nrow(coverage)) stop("Metric merge changed the coverage row count.", call. = FALSE)
  if (anyNA(x$status)) stop("A coverage row has no canonical window row.", call. = FALSE)
  x[, metrics_used := complete_primary]
  for (k in intersect(c(metric_cols, "n_events", "obs_s", "n_bouts", "events_per_bout", "n_positions_occupied", "dominant_share",
                        "bout_rate_h", "dyadic_obs_s", "obs_share"), names(x)))
    data.table::set(x, which(!x$complete_primary), k, NA)
  x[complete_primary == FALSE, `:=`(n_tracked_mates = NA_integer_, hardware_flag = NA)]
  x[, n_in_cage := .N, by = .(CageEpisodeID, phase)]
  data.table::setorder(x, AnimalNum, CC, phase_type, k)
  x[]
}

#' Coverage counts per CC x phase (complete primary / strict), and the clean-set gate inputs.
s32w_coverage_counts <- function(cov) {
  cov[, .(n_animals = data.table::uniqueN(AnimalNum), in_clean_set = in_clean_set[1], n_complete_primary = sum(complete_primary),
          n_complete_strict = sum(complete_strict), n_explicit_exclusion = sum(explicit_exclusion), n_block_kept = sum(block_kept),
          n_board_ends_ok_primary = sum(board_ends_ok_primary), n_board_ends_ok_strict = sum(board_ends_ok_strict),
          min_board_end_margin_s = min(board_end_margin_s, na.rm = TRUE)), by = .(CC, phase)][order(CC, phase)]
}

#' Compare recomputed rows with a reference table on `cols` (numeric within tol, others exactly; identical NA pattern).
s32w_compare <- function(rec, ref, key, cols, tol = 1e-9) {
  a <- data.table::as.data.table(rec); b <- data.table::as.data.table(ref)
  m <- merge(a[, c(key, cols), with = FALSE], b[, c(key, cols), with = FALSE], by = key, suffixes = c(".rec", ".ref"), all = TRUE)
  data.table::rbindlist(lapply(cols, function(k) {
    x <- m[[paste0(k, ".rec")]]; y <- m[[paste0(k, ".ref")]]
    na_ok <- identical(is.na(x), is.na(y))
    if (is.numeric(x) && is.numeric(y)) { d <- abs(x - y); mx <- if (all(is.na(d))) 0 else max(d, na.rm = TRUE)
      data.table::data.table(column = k, n = length(x), n_na = sum(is.na(x)), max_abs_diff = mx, passed = na_ok && mx <= tol)
    } else { eq <- identical(as.character(x), as.character(y))
      data.table::data.table(column = k, n = length(x), n_na = sum(is.na(x)), max_abs_diff = if (eq) 0 else NA_real_, passed = na_ok && eq) }
  }))
}
