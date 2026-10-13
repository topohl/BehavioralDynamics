# Single-read spatial reconstruction (srs_v1.0): UNREGISTERED, isolated, group-blind; Stage 19 is not replaced.
# Per recording: the event-based reconstruction of Stage 19 (same function and parameters) and the single-read
# reconstruction, with the observation age of every second, cut at the same phase-block edges and masked by the same
# spatial exclusion manifest; the vendor position stream with one reader's reads deleted on healthy boards (how much the
# Stage 29 window metrics depend on that antenna); and the board, cage-episode, batch, system and cage-change keys.
# Then parity with Stage 19 and Stage 29, summaries and figures. Definitions: Functions/antenna_spatial_reconstruction.R;
# documentation: docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.md.
#
# Run from the repository root (one R process, about 40 min):
#   Rscript Testing/audits/build_single_read_spatial_reconstruction.R [--run-id=<new id>] [--output-root=<absolute>]
#           [--recordings=<regex>] [--resume]
# Default output root <Behavior>/RANM/single_read_spatial; refused inside the live project root, the raw data and the
# data-version roots (mmm_isolated_output_root_check()). Writes runs/<run_id>/ only (built in runs/.tmp_<run_id>/). The
# default run id is srs_v1.0_<commit>; it needs committed code. Ids starting with "dev" may run on uncommitted code and
# with --recordings. Reads no group label, CombZ or outcome: the Stage 29 table is read for its keys and the metric
# values the knockout is checked against.

srs_args <- function(args) {
  known <- "^--(run-id|output-root|recordings)=|^--resume$"
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  list(run_id = val("run-id"), output_root = val("output-root"), recordings = val("recordings"), resume = "--resume" %in% args)
}

SRS_CODE <- c("Testing/audits/build_single_read_spatial_reconstruction.R", "Functions/antenna_spatial_reconstruction.R",
              "Functions/spatial_exclusion_manifest.R", "Functions/antenna_log_helpers.R", "Functions/animalpos_reconstruction.R",
              "Functions/antenna_data_version.R", "Functions/rfid_event_stream.R", "Functions/rfid_binfree_metrics.R",
              "Functions/animalpos_preprocessing_helpers.R", "Functions/behavioral_dynamics_helpers.R", "Functions/project_paths.R",
              "Functions/behavior_analysis_config.R", "Functions/antenna_measurement.R", "Analysis/_pipeline_setup.R")
SRS_MANIFEST_VERSION <- "v1.0"
SRS_PER_RECORDING <- c("occupancy", "crosstab", "censored", "animals", "blocks", "knockout_metrics", "vendor_record_parity", "read_agreement")
SRS_LONGEST_RELATIVE <- c(file.path("rec", "B6_CC4", "read_basis_runs.csv.gz"), file.path("tables", "knockout_materiality.csv"),
                          file.path("figures", "srs05_knockout_shifts.png"), file.path("manifest", "output_files_sha256.csv"))

srs_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

# One recording into <tmp>/rec/<id>/, with a DONE marker naming the code and input hashes.
srs_recording <- function(vf, cfg, man, marker) {
  id <- sub("^E9_SIS_(B[0-9]+)_(CC[0-9]+)_.*$", "\\1_\\2", basename(vf))
  batch <- sub("_.*$", "", id); cc <- sub("^.*_", "", id)
  d <- file.path(cfg$tmp, "rec", id); done <- file.path(d, "DONE.txt")
  tag_file <- file.path(cfg$tag_dir, batch, sprintf("E9_SIS_%s_%s_AnimalPos.csv", batch, cc))
  logs <- rfid_raw_log_candidates(cfg$raw_root, batch, cc)
  inputs <- c(vf, tag_file, logs)
  input_tab <- data.table::data.table(SourceFile = basename(vf), role = c("v2 preprocessed", "tag map", rep("raw log candidate", length(logs))),
                                      input = inputs, sha256 = mmm_file_sha256(inputs), bytes = file.size(inputs))
  marker <- c(marker, paste(input_tab$sha256, basename(input_tab$input)))
  if (file.exists(done) && identical(readLines(done), marker)) { message(id, ": done, kept"); return(invisible(id)) }
  t_start <- Sys.time()
  vendor <- data.table::fread(vf, colClasses = "character")
  cal <- antenna_dv_vendor_calendar(vendor)
  tags <- data.table::as.data.table(rfid_animalpos_tag_map(tag_file))
  rec <- rfid_read_recording(cfg$raw_root, batch, cc, keep_read_offsets = TRUE)
  ev <- rec$events
  S0 <- cal$span_start; S1 <- min(cal$span_end, max(ev$t + ev$dur_sec))
  ids <- unique(data.table::as.data.table(vendor)[, .(AnimalID, System_label = System)])
  ids[, AnimalNum := canonical_animal_id(AnimalID)]
  if (anyDuplicated(ids$AnimalNum)) stop(id, ": an animal has two spellings or two System labels.", call. = FALSE)
  roster <- tibble::as_tibble(ids[, .(AnimalNum, System_label)])

  # event basis: Stage 19's function and parameters
  eb <- rfid_raw_occupancy_intervals(ev[, setdiff(names(ev), c("read_offsets_ms", "file_order"))], roster, tags, S0, S1, ASR_TAU_S)
  if (anyNA(eb$audit$Board)) stop(id, ": an animal has no raw detection in the span.", call. = FALSE)
  board_of <- stats::setNames(as.integer(eb$audit$Board), eb$audit$AnimalNum)
  ev_iv_all <- data.table::as.data.table(eb$intervals)

  # phase blocks (identical edges for both bases), Stage 19's CC4 crop, the manifest masks
  blk_of <- function(t) animalpos_phase_block_index(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"))
  blocks <- data.table::data.table(block = seq(blk_of(S0), blk_of(S1 - 1e-6)))
  blocks[, `:=`(bs = pmax(S0, block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC),
                be = pmin(S1, (block + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC))]
  cb <- data.table::as.data.table(cal$blocks)[, .(block = as.numeric(PhaseBlockIndex), Phase,
                                                  PhaseNumber = ifelse(Phase == "Active", as.integer(ConsecActive), as.integer(ConsecInactive)))]
  blocks <- merge(blocks, cb, by = "block", all.x = TRUE)
  if (anyNA(blocks$Phase)) stop(id, ": a block in the span has no vendor phase label.", call. = FALSE)
  blocks[, `:=`(block_no = seq_len(.N), stage19_window = !(cc == "CC4" & PhaseNumber > 2L))]
  edges <- blocks$bs[-1]

  # reads on the millisecond grid (apr_reads(): the times that reproduce the vendor records), roster animals only
  rd <- data.table::as.data.table(apr_reads(ev))[, .(tag, t_ms, gx, gy, board, PositionID)]
  rd[, ord := .I]
  rd <- merge(rd, tags[AnimalNum %in% ids$AnimalNum], by = "tag")
  data.table::setorder(rd, ord)
  rm(rec); invisible(gc())

  # Stage 29 windows of this recording (first active phase and the following light phase), as mmm_evs_windows() sets them
  pre <- data.table::data.table(SourceFile = basename(vf), DateTime = antenna_dv_parse_time(vendor$DateTime), Batch = batch, CageChange = cc)
  w29 <- mmm_evs_windows(pre)
  # Stage 29's stream starts at the file's first record t0 (a few seconds to minutes after 18:30), so its active window
  # holds data from t0 on
  wins <- data.table::data.table(window = c("active", "light"), ws = pmax(as.numeric(c(w29$active_start, w29$light_start)), as.numeric(w29$t0)),
                                 we = as.numeric(c(w29$active_end, w29$light_end)))

  # the v2 vendor records (no synthetic preprocessing rows), to check the emulated baseline record for record
  syn <- if ("is_synthetic_preprocessing_row" %in% names(vendor)) toupper(vendor$is_synthetic_preprocessing_row) == "TRUE" else FALSE
  v2 <- data.table::data.table(AnimalNum = canonical_animal_id(vendor$AnimalID), t = as.numeric(antenna_dv_parse_time(vendor$DateTime)),
                               PositionID = as.integer(vendor$PositionID))[!syn]
  occ <- list(); xt <- list(); cens <- list(); anim <- list(); runs_out <- list(); kruns <- list(); vpar <- list(); agr <- list()
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
    seg <- asr_animal_segments(ev_iv, rd_iv, read_t, S0, S1, cuts)
    ag <- asr_aggregate(seg, blocks[, .(block, bs)], censor_from = cfrom)
    # at each in-span observation (before censoring): does each basis hold the reader just read?
    io <- which(t_obs >= S0 & t_obs < min(S1, cfrom))
    ag_read <- data.table::data.table(block = blocks$block[findInterval(t_obs[io], blocks$bs)], pid = obs$pid[io],
                                      pe = asr_position_at(ev_iv, t_obs[io]), pr = asr_position_at(rd_iv, t_obs[io]))[
      , .(observations = .N, event_holds_read_reader = sum(pe == pid), read_holds_read_reader = sum(pr == pid),
          event_holds_read_column = sum(amv_column(pmax(pe, 1L)) == amv_column(pid) & pe > 0),
          read_holds_read_column = sum(amv_column(pmax(pr, 1L)) == amv_column(pid) & pr > 0)), by = block]
    agr[[a]] <- ag_read[, AnimalNum := a]
    occ[[a]] <- ag$occupancy[, AnimalNum := a]; xt[[a]] <- ag$crosstab[, AnimalNum := a]
    cens[[a]] <- ag$censored[, AnimalNum := a]
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
      censor_from = cfrom, reads_on_board = nrow(ra), reads_off_board = sum(rd$AnimalNum == a) - nrow(ra),
      observations = nrow(obs), tie_observations = sum(obs$tie), tie_observations_in_span = sum(obs$tie & in_span),
      read_seeded = any(t_obs < S0), read_seed_age_s = if (any(t_obs < S0)) S0 - max(t_obs[t_obs < S0]) else NA_real_,
      changes_read_tau0_in_span = chg(obs$pid[in_span]), changes_read_tau2_in_span = chg(filt[in_span]))
    # knockout: the vendor stream from all of the animal's reads (all boards, as the vendor pools them), and without one
    # reader's reads on its own board
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
        kruns[[length(kruns) + 1L]] <- r[, `:=`(AnimalNum = a, CC = cc, SourceFile = basename(vf), System = paste0("sys.", b_own),
                                                variant = v, window = wins$window[w], reads_in_window = kn)]
      }
    }
  }
  key <- data.table::data.table(SourceFile = basename(vf), Batch = batch, CC = cc)
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
  res$animals <- addkeys(data.table::rbindlist(anim))[, `:=`(span_start = S0, span_end = S1)]
  res$animals <- merge(res$animals, data.table::as.data.table(eb$audit)[, .(AnimalNum, tag, events_in_span, events_off_board,
                                                                              event_seeded = seeded, event_seed_age_s = seed_lag_seconds,
                                                                              changes_event_raw = reader_changes_raw,
                                                                              changes_event_tau2 = reader_changes_after_flip_filter)], by = "AnimalNum")
  res$blocks <- cbind(key[rep(1L, nrow(blocks))], blocks)
  # window metrics of every variant: all animals of the recording together. Cage-mates share the variant: a knockout
  # deletes reader k from every animal's reads on its own board (its few off-board reads are kept), so each board
  # loses that antenna at once; boards are independent.
  KR <- data.table::rbindlist(kruns)
  km <- KR[, asr_window_metrics(.SD[, .(AnimalNum, CC, SourceFile, System, s, e, pos)]), by = .(variant, window)]
  km <- merge(km, unique(KR[, .(variant, window, AnimalNum, reads_in_window)]), by = c("variant", "window", "AnimalNum"), all.x = TRUE)
  km[, board := board_of[AnimalNum]]
  km[, CageEpisodeID := asr_cage_episode(batch, board, CC)]
  km[, Batch := batch]
  res$knockout_metrics <- km
  res$vendor_record_parity <- cbind(key[rep(1L, length(vpar))], data.table::rbindlist(vpar))[, `:=`(board = board_of[AnimalNum])][
    , CageEpisodeID := asr_cage_episode(Batch, board, CC)][]
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (nm in SRS_PER_RECORDING) data.table::fwrite(res[[nm]], file.path(d, paste0(nm, ".csv")))
  ro <- addkeys(data.table::rbindlist(runs_out))
  data.table::fwrite(ro, file.path(d, "read_basis_runs.csv.gz"))
  data.table::fwrite(input_tab, file.path(d, "inputs.csv"))
  writeLines(marker, done)
  message(sprintf("%s: %.1f min", id, as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
  invisible(id)
}

# ---------------------------------------------------------------- summaries (group-blind)
# scales: data.table(window, construct, scale_sd, scale_source); active = the frozen Stage 29 standardizers, light = the
# group-blind rule (residual SD of lm(y ~ Batch) over the CC1 baseline light windows of all healthy animals)
srs_scale <- function(scales, w, cn) scales[window == w & construct == cn]$scale_sd
srs_summaries <- function(tab, stage19, stage29, qc_animals, scales) {
  L <- data.table::data.table(PositionID = 1:8, reader = paste0("R", 1:8), column = amv_column(1:8),
                              resource = c("food", "", "", "", "house", "", "", "water"))
  out <- list()
  ok_sp <- function(d) d[!sxm_spatial_excluded(MAN, Batch, CC, board)]
  occ <- data.table::copy(tab$occupancy); xt <- data.table::copy(tab$crosstab)
  sc <- c("seconds", paste0("sup_", ASR_BOUNDS_S))

  # 1. parity of the event basis with Stage 19 (its phase table; its CC4 crop)
  e19 <- occ[basis == "event" & stage19_window == TRUE, .(seconds = sum(seconds)), by = .(SourceFile, Phase, PhaseNumber, AnimalNum, PositionID)]
  p19 <- merge(e19, stage19, by = c("SourceFile", "Phase", "PhaseNumber", "AnimalNum", "PositionID"), all = TRUE)
  p19[is.na(seconds), seconds := 0][is.na(PositionSeconds), PositionSeconds := 0]
  out$parity_stage19 <- data.table::data.table(
    rows = nrow(p19), animal_phases = data.table::uniqueN(p19[, .(SourceFile, Phase, PhaseNumber, AnimalNum)]),
    only_here = nrow(p19[seconds > 0 & PositionSeconds == 0]), only_stage19 = nrow(p19[PositionSeconds > 0 & seconds == 0]),
    max_abs_diff_s = max(abs(p19$seconds - p19$PositionSeconds)), sum_abs_diff_s = sum(abs(p19$seconds - p19$PositionSeconds)),
    total_s = sum(p19$PositionSeconds))
  out$parity_stage19_worst <- p19[order(-abs(seconds - PositionSeconds))][1:min(20, .N)]

  # 2. knockout baseline against the registered Stage 29 values (the emulated vendor stream reproduces them?), on the
  # healthy boards and on the three dead-channel boards separately
  km <- tab$knockout_metrics
  dead <- MAN[action == "exclude_spatial", paste(Batch, CC, board)]
  par29 <- function(base) {
  s29a <- merge(base[window == "active", .(AnimalNum, CC, crossing_rate, occupancy_dispersion, fragmentation, shared_zone_use)],
                stage29[, .(AnimalNum, CC, crossing_rate_29 = crossing_rate, occupancy_dispersion_29 = occupancy_dispersion,
                            fragmentation_29 = fragmentation, shared_zone_use_29 = shared_zone_use)], by = c("AnimalNum", "CC"))
  s29l <- merge(base[window == "light", .(AnimalNum, CC, crossing_rate)], stage29[, .(AnimalNum, CC, light_29 = light_phase_crossing_rate)],
                by = c("AnimalNum", "CC"))
  pr <- function(x, y) c(n = sum(is.finite(x) & is.finite(y)), max_abs_diff = max(abs(x - y), na.rm = TRUE),
                         median_abs_diff = stats::median(abs(x - y), na.rm = TRUE), share_within_1pct_sd = NA)
  out$parity_stage29 <- data.table::rbindlist(list(
    data.table::data.table(construct = "crossing_rate", t(pr(s29a$crossing_rate, s29a$crossing_rate_29)), scale_sd = srs_scale(scales, "active", "crossing_rate")),
    data.table::data.table(construct = "occupancy_dispersion", t(pr(s29a$occupancy_dispersion, s29a$occupancy_dispersion_29)), scale_sd = srs_scale(scales, "active", "occupancy_dispersion")),
    data.table::data.table(construct = "fragmentation", t(pr(s29a$fragmentation, s29a$fragmentation_29)), scale_sd = srs_scale(scales, "active", "fragmentation")),
    data.table::data.table(construct = "shared_zone_use", t(pr(s29a$shared_zone_use, s29a$shared_zone_use_29)), scale_sd = srs_scale(scales, "active", "shared_zone_use")),
    data.table::data.table(construct = "light_phase_crossing_rate", t(pr(s29l$crossing_rate, s29l$light_29)), scale_sd = srs_scale(scales, "light", "crossing_rate"))))
  }
  out$parity_stage29 <- rbind(par29(km[variant == "baseline" & !paste(Batch, CC, board) %in% dead])[, boards := "healthy"],
                              par29(km[variant == "baseline" & paste(Batch, CC, board) %in% dead])[, boards := "dead channel"])
  out$parity_stage29[, max_abs_diff_in_sd := max_abs_diff / scale_sd][, share_within_1pct_sd := NULL]
  # counts of animal-windows that differ from Stage 29, per construct: exactly equal, and within 0.01 scale SD
  cm <- merge(km[variant == "baseline"], stage29[, .(AnimalNum, CC, cr29 = crossing_rate, od29 = occupancy_dispersion, fr29 = fragmentation,
                                                      sz29 = shared_zone_use, lr29 = light_phase_crossing_rate)], by = c("AnimalNum", "CC"))
  cm[, boards := ifelse(paste(Batch, CC, board) %in% dead, "dead channel", "healthy")]
  cnt <- function(d, w, cn, x, y) d[window == w, .(window = w, construct = cn, compared = sum(is.finite(get(x)) & is.finite(get(y))),
    exact = sum(abs(get(x) - get(y)) < 1e-9, na.rm = TRUE), within_0.01sd = sum(abs(get(x) - get(y)) <= 0.01 * srs_scale(scales, w, if (cn == "light_phase_crossing_rate") "crossing_rate" else cn), na.rm = TRUE)), by = boards]
  out$parity_stage29_counts <- data.table::rbindlist(list(cnt(cm, "active", "crossing_rate", "crossing_rate", "cr29"),
    cnt(cm, "active", "occupancy_dispersion", "occupancy_dispersion", "od29"), cnt(cm, "active", "fragmentation", "fragmentation", "fr29"),
    cnt(cm, "active", "shared_zone_use", "shared_zone_use", "sz29"), cnt(cm, "light", "light_phase_crossing_rate", "crossing_rate", "lr29")))
  gt <- function(x, y, cn) { d <- abs(x - y) / srs_scale(scales, "active", cn); !is.na(d) & d > 0.01 }
  out$parity_stage29_any <- cm[window == "active", .(animal_windows = .N,
    differ_gt_0.01sd_any = sum(gt(crossing_rate, cr29, "crossing_rate") | gt(occupancy_dispersion, od29, "occupancy_dispersion") |
                               gt(fragmentation, fr29, "fragmentation") | gt(shared_zone_use, sz29, "shared_zone_use"))), by = boards]
  vp <- tab$vendor_record_parity
  vp[, boards := ifelse(paste(Batch, CC, board) %in% dead, "dead channel", "healthy")]
  out$vendor_record_parity_summary <- vp[, .(animal_windows = .N, identical = sum(identical), records_emulated = sum(records_emulated),
                                             records_v2 = sum(records_v2)), by = .(boards, window)]

  # 3. knockout shifts on healthy boards (dead boards and the censored episode left out) against the frozen scales
  kw <- data.table::dcast(km[!paste(Batch, CC, board) %in% dead],
                          AnimalNum + CC + Batch + board + CageEpisodeID + window ~ variant,
                          value.var = c("crossing_rate", "occupancy_dispersion", "fragmentation", "shared_zone_use", "n_positions_occupied"))
  cons <- c("crossing_rate", "occupancy_dispersion", "fragmentation", "shared_zone_use")
  ko <- data.table::rbindlist(lapply(paste0("ko_R", ASR_KNOCKOUT_READERS), function(v) data.table::rbindlist(lapply(c("active", "light"), function(w) {
    data.table::rbindlist(lapply(cons, function(cn) {
      x <- kw[window == w]
      sh <- x[[paste0(cn, "_", v)]] - x[[paste0(cn, "_baseline")]]
      sdv <- srs_scale(scales, w, cn)
      rel <- if (cn == "crossing_rate") stats::median(x[[paste0(cn, "_", v)]] / x[[paste0(cn, "_baseline")]], na.rm = TRUE) else NA_real_
      data.table::data.table(variant = v, window = w, construct = if (cn == "crossing_rate" && w == "light") "light_phase_crossing_rate" else cn,
                             asr_materiality(sh, sdv), median_ratio_knockout_to_baseline = rel)
    })) }))))
  out$knockout_materiality <- ko
  hf <- km[!paste(Batch, CC, board) %in% dead, .(animal_windows = .N, n_positions_lt8_share = mean(n_positions_occupied < 8)), by = .(variant, window)]
  out$knockout_hardware_flag <- hf
  kc <- data.table::melt(kw, id.vars = c("AnimalNum", "CC", "Batch", "board", "CageEpisodeID", "window"), variable.name = "var", value.name = "value")
  out$knockout_shifts <- kw
  # the affected cage episodes themselves (their vendor stream already lacks the antenna): values as context
  out$affected_episode_windows <- km[paste(Batch, CC, board) %in% dead & variant == "baseline",
                                     .(CageEpisodeID, AnimalNum, window, crossing_rate, occupancy_dispersion, fragmentation, shared_zone_use,
                                       n_positions_occupied, reads_in_window)]

  # 4. occupancy at reader and column resolution, both bases, carry-forward and bounded, pooled and by cage episode
  so <- ok_sp(occ)
  so[, column := L$column[PositionID]]
  long <- data.table::melt(so, measure.vars = sc, variable.name = "bound", value.name = "s")
  long[, bound := factor(sub("^seconds$", "LOCF", sub("^sup_", "<= ", as.character(bound))), levels = c("LOCF", paste("<=", rev(ASR_BOUNDS_S))))]
  rd_pool <- long[, .(s = sum(s)), by = .(Phase, basis, bound, PositionID)]
  rd_pool[, share := s / sum(s), by = .(Phase, basis, bound)]
  rd_pool <- merge(rd_pool, L, by = "PositionID")
  col_pool <- long[, .(s = sum(s)), by = .(Phase, basis, bound, column)][, share := s / sum(s), by = .(Phase, basis, bound)]
  out$occupancy_reader_pooled <- rd_pool[order(Phase, basis, bound, PositionID)]
  out$occupancy_column_pooled <- col_pool[order(Phase, basis, bound, column)]
  ce <- long[, .(s = sum(s)), by = .(CageEpisodeID, Batch, CC, board, Phase, basis, bound, PositionID)]
  ce[, share := s / sum(s), by = .(CageEpisodeID, Phase, basis, bound)]
  cc_ <- long[, .(s = sum(s)), by = .(CageEpisodeID, Batch, CC, board, Phase, basis, bound, column)]
  cc_[, share := s / sum(s), by = .(CageEpisodeID, Phase, basis, bound)]
  q <- function(v) list(cage_episodes = length(v), median = stats::median(v), q25 = stats::quantile(v, 0.25, names = FALSE),
                        q75 = stats::quantile(v, 0.75, names = FALSE), min = min(v), max = max(v))

  # 5. read minus event basis, LOCF: by reader (R1 food, R5 house, R8 water) and by column, across cage episodes
  dr <- data.table::dcast(ce[bound == "LOCF"], CageEpisodeID + Batch + CC + board + Phase + PositionID ~ basis, value.var = "share", fill = 0)
  dr[, diff := read - event]
  out$read_minus_event_reader <- merge(dr[, c(q(diff), list(pooled_event = NA_real_)), by = .(Phase, PositionID)], L, by = "PositionID")[order(Phase, PositionID)]
  pe <- rd_pool[bound == "LOCF", .(Phase, PositionID, basis, share)]
  pe <- data.table::dcast(pe, Phase + PositionID ~ basis, value.var = "share")
  out$read_minus_event_reader[pe, on = .(Phase, PositionID), `:=`(pooled_event = i.event, pooled_read = i.read, pooled_diff = i.read - i.event)]
  dc <- data.table::dcast(cc_[bound == "LOCF"], CageEpisodeID + Batch + CC + board + Phase + column ~ basis, value.var = "share", fill = 0)
  dc[, diff := read - event]
  out$read_minus_event_column <- dc[, q(diff), by = .(Phase, column)][order(Phase, column)]
  out$read_minus_event_by_episode <- dr

  # 6. reassignment between the bases at reader and column resolution, by phase, age class, board and cage change
  x2 <- ok_sp(xt)[pos_e > 0 & pos_r > 0]
  x2[, `:=`(age_le2 = sup_2, age_2_10 = sup_10 - sup_2, age_10_60 = sup_60 - sup_10, age_60_300 = sup_300 - sup_60, age_gt300 = seconds - sup_300)]
  x2[, `:=`(diff_reader = pos_e != pos_r, diff_column = amv_column(pos_e) != amv_column(pos_r))]
  agecols <- c("age_le2", "age_2_10", "age_10_60", "age_60_300", "age_gt300")
  ra <- function(by) x2[, .(seconds = sum(seconds), reassigned_reader = sum(seconds[diff_reader]), reassigned_column = sum(seconds[diff_column])), by = by][
    , `:=`(reader_share = reassigned_reader / seconds, column_share = reassigned_column / seconds, persists_at_column = reassigned_column / reassigned_reader)][]
  out$reassignment_by_phase <- ra("Phase")
  out$reassignment_by_board <- ra(c("Phase", "board"))[order(Phase, board)]
  out$reassignment_by_cc <- ra(c("Phase", "CC"))[order(Phase, CC)]
  ep <- ra(c("Phase", "CageEpisodeID"))
  out$reassignment_episode_spread <- ep[, c(q(reader_share), list(col_median = stats::median(column_share), col_q25 = stats::quantile(column_share, 0.25, names = FALSE),
                                                                     col_q75 = stats::quantile(column_share, 0.75, names = FALSE), col_max = max(column_share))), by = Phase]
  ag <- data.table::melt(x2, measure.vars = agecols, variable.name = "age_class", value.name = "s")
  out$reassignment_by_age <- ag[, .(seconds = sum(s), reassigned_reader = sum(s[diff_reader]), reassigned_column = sum(s[diff_column])), by = .(Phase, age_class)][
    , `:=`(reader_share = reassigned_reader / seconds, column_share = reassigned_column / seconds,
           share_of_reader_reassignment = reassigned_reader / sum(reassigned_reader)), by = Phase][]
  fl <- x2[diff_reader == TRUE, .(seconds = sum(seconds)), by = .(Phase, pos_e, pos_r)][, share := seconds / sum(seconds), by = Phase]
  out$reassignment_flow <- fl[order(Phase, -seconds)]

  # 7. carry-forward bound: supported share and the change of occupancy against LOCF, pooled and by cage episode
  sup <- long[, .(s = sum(s)), by = .(Phase, basis, bound)]
  sup[, supported_share := s / s[bound == "LOCF"], by = .(Phase, basis)]
  out$bound_supported_share <- sup[order(Phase, basis, bound)]
  bc <- merge(col_pool, col_pool[bound == "LOCF", .(Phase, basis, column, share_locf = share)], by = c("Phase", "basis", "column"))
  bc[, change_vs_locf := share - share_locf]
  out$bound_sensitivity_column <- bc[order(Phase, basis, column, bound)]
  br <- merge(rd_pool, rd_pool[bound == "LOCF", .(Phase, basis, PositionID, share_locf = share)], by = c("Phase", "basis", "PositionID"))
  br[, change_vs_locf := share - share_locf]
  out$bound_sensitivity_reader <- br[order(Phase, basis, PositionID, bound)]
  ccl <- merge(cc_, cc_[bound == "LOCF", .(CageEpisodeID, Phase, basis, column, share_locf = share)], by = c("CageEpisodeID", "Phase", "basis", "column"))
  ccl[, change := share - share_locf]
  out$bound_sensitivity_episode_spread <- ccl[bound != "LOCF", .(max_abs_change = max(abs(change))), by = .(CageEpisodeID, Phase, basis, bound)][
    , q(max_abs_change), by = .(Phase, basis, bound)][order(Phase, basis, bound)]

  # 8. observation gaps: the age of the time each reader holds (read basis, LOCF), and by cage change
  og <- so[basis == "read"]
  og[, `:=`(age_le10 = sup_10, age_10_60 = sup_60 - sup_10, age_60_300 = sup_300 - sup_60, age_gt300 = seconds - sup_300)]
  out$observation_age_by_reader <- merge(og[, .(seconds = sum(seconds), age_le10 = sum(age_le10), age_10_60 = sum(age_10_60), age_60_300 = sum(age_60_300),
                                                age_gt300 = sum(age_gt300)), by = .(Phase, PositionID)][
    , `:=`(le10_share = age_le10 / seconds, gt60_share = (age_60_300 + age_gt300) / seconds, gt300_share = age_gt300 / seconds,
           share_of_all_gt60 = (age_60_300 + age_gt300) / sum(age_60_300 + age_gt300)), by = Phase], L, by = "PositionID")[order(Phase, PositionID)]
  out$observation_age_by_cc <- og[, .(seconds = sum(seconds), gt60 = sum(age_60_300 + age_gt300), gt300 = sum(age_gt300)), by = .(Phase, CC)][
    , `:=`(gt60_share = gt60 / seconds, gt300_share = gt300 / seconds)][order(Phase, CC)]
  out$observation_age_by_board <- og[, .(seconds = sum(seconds), gt60 = sum(age_60_300 + age_gt300)), by = .(Phase, board)][, gt60_share := gt60 / seconds][order(Phase, board)]

  # 9. board as an acquisition factor: the same read-dependent quantities by board within each recording (descriptive)
  bd <- so[basis == "read", .(seconds = sum(seconds), sup60 = sum(sup_60)), by = .(Batch, CC, board, Phase)]
  rdh <- tab$animals[!sxm_spatial_excluded(MAN, Batch, CC, board), .(reads = sum(reads_on_board), animals = .N), by = .(Batch, CC, board)]
  bd <- merge(bd, rdh, by = c("Batch", "CC", "board"))
  bd[, supported_le60_share := sup60 / seconds]
  bd[, rel_to_recording_median := supported_le60_share / stats::median(supported_le60_share), by = .(Batch, CC, Phase)]
  out$board_support <- bd[order(Phase, Batch, CC, board)]
  out$board_support_summary <- bd[, .(board_recordings = .N, supported_le60_median = stats::median(supported_le60_share),
                                      rel_median = stats::median(rel_to_recording_median), rel_q25 = stats::quantile(rel_to_recording_median, 0.25, names = FALSE),
                                      rel_q75 = stats::quantile(rel_to_recording_median, 0.75, names = FALSE)), by = .(Phase, board)][order(Phase, board)]

  # 10. metadata: keys of this build against Stage 00b, Stage 19 and Stage 29
  an <- tab$animals[, .(SourceFile, Batch, CC, AnimalNum, board, System, CageEpisodeID, System_label_v2)]
  m <- merge(an, qc_animals[, .(SourceFile, AnimalNum, board_00b = board, System_00b = System)], by = c("SourceFile", "AnimalNum"), all = TRUE)
  m <- merge(m, stage29[, .(SourceFile, AnimalNum, System_29 = System, CageEpisodeID_29 = CageEpisodeID)], by = c("SourceFile", "AnimalNum"), all = TRUE)
  m <- merge(m, unique(stage19[, .(SourceFile, AnimalNum, System_19 = System)]), by = c("SourceFile", "AnimalNum"), all = TRUE)
  m[, `:=`(board_eq_00b = board == board_00b, System_eq_v2_label = System == System_label_v2, System_eq_19 = System == System_19,
           episode_eq_29 = CageEpisodeID == CageEpisodeID_29)]
  out$metadata_consistency <- m[order(SourceFile, AnimalNum)]
  out$metadata_consistency_summary <- data.table::data.table(
    animal_recordings = nrow(m), missing_here = sum(is.na(m$board)), board_eq_00b = sum(m$board_eq_00b, na.rm = TRUE),
    board_ne_00b = sum(!m$board_eq_00b, na.rm = TRUE), System_eq_v2_label = sum(m$System_eq_v2_label, na.rm = TRUE),
    System_ne_v2_label = sum(!m$System_eq_v2_label, na.rm = TRUE), System_eq_stage19 = sum(m$System_eq_19, na.rm = TRUE),
    System_ne_stage19 = sum(!m$System_eq_19, na.rm = TRUE), in_stage29 = sum(!is.na(m$CageEpisodeID_29)),
    episode_eq_stage29 = sum(m$episode_eq_29, na.rm = TRUE), episode_ne_stage29 = sum(!m$episode_eq_29, na.rm = TRUE))

  # 11. ties and seeds
  out$ties_and_seeds <- tab$animals[, .(animals = .N, observations = sum(observations), tie_observations = sum(tie_observations),
                                        tie_share = sum(tie_observations) / sum(observations), read_seeded = sum(read_seeded), event_seeded = sum(event_seeded),
                                        median_read_seed_age_s = stats::median(read_seed_age_s, na.rm = TRUE)), by = .(Batch, CC)]
  out$censoring <- tab$censored[, .(censored_s = sum(seconds)), by = .(SourceFile, CageEpisodeID, AnimalNum)]
  out
}

srs_figures <- function(sm, dir) {
  th <- ggplot2::theme_bw(base_size = 9) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  sv <- function(p, n, w = 7, h = 4) ggplot2::ggsave(file.path(dir, paste0(n, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
  bcol <- c(event = MMM_PALETTE_DIVERGING[["low"]], read = MMM_PALETTE_DIVERGING[["high"]])
  r <- sm$occupancy_reader_pooled[bound == "LOCF"]
  r[, lab := factor(paste0(reader, ifelse(nzchar(resource), paste0("\n", resource), "")), levels = unique(paste0(reader, ifelse(nzchar(resource), paste0("\n", resource), ""))[order(PositionID)]))]
  p <- ggplot2::ggplot(r, ggplot2::aes(lab, share, fill = basis)) + ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.75) +
    ggplot2::scale_fill_manual(values = bcol) + ggplot2::facet_wrap(~Phase) + th +
    ggplot2::labs(x = NULL, y = "share of carry-forward time", title = "Reader occupancy: event-start and single-read bases (LOCF, tau 2 s)",
                  subtitle = "pooled over animals; the three dead-channel cage episodes left out (manifest v1.0)")
  sv(p, "srs01_reader_occupancy_by_basis", 8, 4)
  d <- data.table::copy(sm$read_minus_event_by_episode)
  d[, reader := factor(paste0("R", PositionID), levels = paste0("R", 1:8))]
  p <- ggplot2::ggplot(d, ggplot2::aes(reader, diff)) + ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_boxplot(outlier.size = 0.6, fill = "grey85") + ggplot2::facet_wrap(~Phase) + th +
    ggplot2::labs(x = "reader (R1 food, R5 house, R8 water)", y = "share read basis - share event basis", title = "Read minus event basis by reader",
                  subtitle = "one value per cage episode (LOCF)")
  sv(p, "srs02_read_minus_event_by_reader", 8, 4)
  a <- data.table::melt(sm$reassignment_by_age, id.vars = c("Phase", "age_class"), measure.vars = c("reader_share", "column_share"),
                        variable.name = "resolution", value.name = "share")
  p <- ggplot2::ggplot(a, ggplot2::aes(age_class, share, fill = resolution)) + ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.75) +
    ggplot2::scale_fill_manual(values = c(reader_share = "grey35", column_share = "grey70"), labels = c("reader", "column"), name = NULL) +
    ggplot2::facet_wrap(~Phase) + th + ggplot2::labs(x = "observation age of the second", y = "share of seconds assigned differently",
                                                     title = "Where the bases disagree: by observation age, at reader and column resolution")
  sv(p, "srs03_reassignment_by_age", 8, 4)
  b <- data.table::copy(sm$bound_sensitivity_column)
  p <- ggplot2::ggplot(b, ggplot2::aes(bound, share, colour = factor(column), group = interaction(column, basis), linetype = basis)) +
    ggplot2::geom_line() + ggplot2::geom_point(size = 1.2) + ggplot2::facet_wrap(~Phase) +
    ggplot2::scale_colour_grey(start = 0.1, end = 0.7, name = "column") + th +
    ggplot2::labs(x = "carry-forward bound on the observation age", y = "share of the retained time", title = "Column occupancy against the carry-forward bound")
  sv(p, "srs04_column_occupancy_by_bound", 8, 4)
  k <- data.table::copy(sm$knockout_shifts)
  kl <- data.table::rbindlist(lapply(c("crossing_rate", "occupancy_dispersion", "fragmentation", "shared_zone_use"), function(cn)
    data.table::rbindlist(lapply(paste0("ko_R", ASR_KNOCKOUT_READERS), function(v)
      k[, .(construct = cn, variant = v, window, shift = get(paste0(cn, "_", v)) - get(paste0(cn, "_baseline")))]))))
  sds <- sm$knockout_materiality[, .(construct = sub("light_phase_", "", construct), window, variant, scale_sd)]
  kl <- merge(kl, unique(sds), by = c("construct", "window", "variant"))
  kl[, shift_sd := shift / scale_sd]
  p <- ggplot2::ggplot(kl, ggplot2::aes(construct, shift_sd, fill = variant)) + ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = c(-1, 1) * ASR_MATERIAL_P90_SD, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_boxplot(outlier.size = 0.5) + ggplot2::scale_fill_manual(values = c(ko_R3 = "grey80", ko_R5 = "grey50")) +
    ggplot2::facet_wrap(~window) + th + ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "knockout - baseline, in standardizer SD", title = "Vendor-stream metrics with one antenna's reads deleted (healthy boards)",
                  subtitle = "dashed: 0.5 SD (materiality: median |shift| >= 0.2 SD or 90th percentile >= 0.5 SD)")
  sv(p, "srs05_knockout_shifts", 8, 4)
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble); library(ggplot2) })
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "manuscript_palette.R", "behavior_analysis_config.R", "animalpos_preprocessing_helpers.R",
              "antenna_log_helpers.R", "animalpos_reconstruction.R", "antenna_data_version.R", "antenna_measurement.R",
              "rfid_event_stream.R", "rfid_binfree_metrics.R", "antenna_spatial_reconstruction.R", "spatial_exclusion_manifest.R")) source_mmm_helper(h)
  a <- srs_args(commandArgs(trailingOnly = TRUE))
  project_root <- mmm_project_root()
  head_commit <- srs_git(c("rev-parse", "HEAD"))[1]
  run_id <- if (is.na(a$run_id)) paste0(ASR_VERSION, "_", substr(head_commit, 1, 7)) else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  dev <- startsWith(run_id, "dev")
  if (!is.na(a$recordings) && !dev) stop("--recordings is for dev runs only.", call. = FALSE)
  dirty <- srs_git(c("status", "--porcelain", "--untracked-files=all", "--", SRS_CODE, SXM_DIR))
  if (!dev && length(dirty)) stop("A release run needs committed code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "single_read_spatial") else a$output_root,
                                         project_root)
  out_dir <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out_dir)) stop("Run folder exists; choose a new --run-id: ", out_dir, call. = FALSE)
  if (file.exists(tmp) && !a$resume) stop("A partial run exists; inspect it or continue with --resume: ", tmp, call. = FALSE)
  mmm_assert_output_path_budget(c(file.path(tmp, SRS_LONGEST_RELATIVE), file.path(out_dir, SRS_LONGEST_RELATIVE)), source_label = "srs build")
  MAN <- sxm_read(SRS_MANIFEST_VERSION, allow_unreleased = dev)
  cfg <- list(tmp = tmp, raw_root = mmm_raw_antenna_root(project_root), tag_dir = file.path(project_root, "MMMSociability", "raw_data"))
  vendor_dir <- file.path(project_root, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "preprocessed_data")
  f19 <- file.path(project_root, "analysis_ready", "analyses", "spatial_occupancy", "tables", "all_position_occupancy_by_phase.csv")
  f29 <- file.path(project_root, "analysis_ready", "pipeline", "29_canonical_behavior_releases", "v101_dv2_b2ce507", "tables", "canonical_window_metrics.csv")
  fqc <- file.path(dirname(project_root), "RANM", "raw_acquisition_qc", "runs", "raqc_20261013_b1783bf", "tables", "animal_qc.csv")
  for (f in c(vendor_dir, cfg$tag_dir, cfg$raw_root, f19, f29, fqc)) if (!file.exists(f)) stop("Missing input: ", f, call. = FALSE)
  vendor_files <- sort(list.files(vendor_dir, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$", full.names = TRUE))
  if (length(vendor_files) != 24) stop("Expected 24 v2 preprocessed files.", call. = FALSE)
  ids <- sub("^E9_SIS_(B[0-9]+)_(CC[0-9]+)_.*$", "\\1_\\2", basename(vendor_files))
  if (!is.na(a$recordings)) { keep <- grepl(a$recordings, ids); vendor_files <- vendor_files[keep]; ids <- ids[keep] }
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  code <- data.table::data.table(file = c(SRS_CODE, file.path(SXM_DIR, basename(attr(MAN, "path")))))
  code[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  marker <- paste(code$sha256, code$file)
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (vf in vendor_files) srs_recording(vf, cfg, MAN, marker)

  rdtab <- function(nm) data.table::rbindlist(lapply(ids, function(id) {
    f <- file.path(tmp, "rec", id, paste0(nm, ".csv"))
    data.table::fread(f, colClasses = if ("AnimalNum" %in% names(data.table::fread(f, nrows = 0))) c(AnimalNum = "character"))
  }), use.names = TRUE, fill = TRUE)
  tab <- stats::setNames(lapply(SRS_PER_RECORDING, rdtab), SRS_PER_RECORDING)
  inputs <- rdtab("inputs")
  # read-only inputs from the live root: keys and values only (no Group, CombZ or outcome column is selected)
  stage19 <- data.table::fread(f19, select = c("SourceFile", "Phase", "PhaseNumber", "AnimalNum", "System", "PositionID", "PositionSeconds"),
                               colClasses = c(AnimalNum = "character"), dec = if (grepl(";", readLines(f19, n = 1))) "," else ".")
  stage19 <- stage19[SourceFile %in% basename(vendor_files), .(PositionSeconds = sum(PositionSeconds)), by = .(SourceFile, Phase, PhaseNumber, AnimalNum, System, PositionID)]
  stage19[, AnimalNum := canonical_animal_id(AnimalNum)]
  stage29 <- data.table::fread(f29, select = c("AnimalNum", "CC", "SourceFile", "System", "CageEpisodeID", "crossing_rate", "occupancy_dispersion",
                                                "fragmentation", "shared_zone_use", "light_phase_crossing_rate"), colClasses = c(AnimalNum = "character"))
  stage29 <- stage29[SourceFile %in% basename(vendor_files)][, AnimalNum := canonical_animal_id(AnimalNum)]
  qc_animals <- data.table::fread(fqc, select = c("SourceFile", "AnimalNum", "board", "System"), colClasses = c(AnimalNum = "character"))
  qc_animals <- qc_animals[SourceFile %in% basename(vendor_files)][, AnimalNum := canonical_animal_id(AnimalNum)]
  M <- MMM_BEHAVIOR_CONFIG$metrics
  cons <- c("crossing_rate", "occupancy_dispersion", "fragmentation", "shared_zone_use")
  # Active window: the frozen Stage 29 standardizers (inherited; the CC1 SIS residual SDs of the registered release).
  # Light window: no frozen standardizer exists, so the residual SD of lm(y ~ Batch) over the CC1 baseline light windows of
  # all healthy animals (CON and SIS alike; no label is read). Declared with the materiality rule.
  kb <- tab$knockout_metrics[variant == "baseline" & window == "light" & CC == "CC1" & !sxm_spatial_excluded(MAN, Batch, CC, board)]
  if (data.table::uniqueN(kb$Batch) < 2) {   # dev subsets without CC1 recordings: every baseline light window
    kb <- tab$knockout_metrics[variant == "baseline" & window == "light" & !sxm_spatial_excluded(MAN, Batch, CC, board)]
  }
  light_scale <- function(cn) { d <- kb[is.finite(get(cn))]
    if (data.table::uniqueN(d$Batch) >= 2) stats::sd(stats::residuals(stats::lm(stats::reformulate("Batch", cn), data = d))) else stats::sd(d[[cn]]) }
  scales <- rbind(data.table::data.table(window = "active", construct = cons, scale_sd = vapply(cons, function(cn) M[[cn]]$standardizer_sd, 0),
                                         scale_source = "frozen Stage 29 standardizer"),
                  data.table::data.table(window = "light", construct = cons, scale_sd = vapply(cons, light_scale, 0),
                                         scale_source = "residual SD of lm(y ~ Batch), CC1 baseline light windows, all healthy animals"))
  light_sd <- srs_scale(scales, "light", "crossing_rate")
  sm <- srs_summaries(tab, stage19, stage29, qc_animals, scales)
  sm$knockout_scales <- scales
  for (dd in c("tables", "manifest", "figures")) dir.create(file.path(tmp, dd), showWarnings = FALSE)
  for (nm in SRS_PER_RECORDING) data.table::fwrite(tab[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  for (nm in names(sm)) data.table::fwrite(sm[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  srs_figures(sm, file.path(tmp, "figures"))
  data.table::fwrite(code, file.path(tmp, "manifest", "code_hashes.csv"))
  extra <- data.table::data.table(SourceFile = NA_character_, role = c("Stage 19 phase table", "Stage 29 v1.0.1 window metrics", "Stage 00b animal_qc"),
                                  input = c(f19, f29, fqc))
  extra[, `:=`(sha256 = mmm_file_sha256(input), bytes = file.size(input))]
  data.table::fwrite(unique(rbind(inputs, extra)[, .(input, role, sha256, bytes)]), file.path(tmp, "manifest", "input_hashes.csv"))
  data.table::fwrite(MAN, file.path(tmp, "manifest", "spatial_exclusions_used.csv"))
  run <- data.table::data.table(item = c("run_id", "version", "git_commit", "code_clean", "dev_run", "recordings", "manifest_version", "manifest_sha256",
                                         "tau_s", "bounds_s", "knockout_readers", "materiality", "light_phase_scale_sd", "project_root", "raw_root", "started", "finished", "R"),
                                value = c(run_id, ASR_VERSION, head_commit, as.character(!length(dirty)), as.character(dev), paste(ids, collapse = " "),
                                          SRS_MANIFEST_VERSION, attr(MAN, "sha256"), ASR_TAU_S, paste(ASR_BOUNDS_S, collapse = " "),
                                          paste0("R", ASR_KNOCKOUT_READERS, collapse = " "),
                                          sprintf("median |shift| >= %.1f SD or 90th percentile >= %.1f SD", ASR_MATERIAL_MEDIAN_SD, ASR_MATERIAL_P90_SD),
                                          sprintf("%.4f", light_sd), project_root, cfg$raw_root, started, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string))
  data.table::fwrite(run, file.path(tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(tmp, "manifest", "session_info.txt"))
  writeLines(c(paste("SINGLE-READ SPATIAL RECONSTRUCTION", ASR_VERSION, "(UNREGISTERED; isolated; Stage 19 unchanged)"), "",
               paste("run:", run_id, "| commit:", head_commit, "| manifest:", SRS_MANIFEST_VERSION), "",
               "rec/<recording>/read_basis_runs.csv.gz  the reconstruction: read-basis runs per animal, cut at the phase-block edges,",
               "                                         with the supported seconds at each carry-forward bound (sup_2 ... sup_300)",
               "tables/occupancy.csv                    both bases: seconds per animal, block and reader, LOCF and bounded",
               "tables/crosstab.csv                     event-basis reader x read-basis reader seconds (with observation age)",
               "tables/knockout_*.csv                   vendor-stream metrics with one reader deleted on healthy boards",
               "tables/parity_*.csv                     event basis against Stage 19; knockout baseline against Stage 29",
               "tables/metadata_consistency*.csv        board, system and cage-episode keys against Stage 00b, 19 and 29",
               "No group label, CombZ or outcome was read. Specification: docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.md."),
             file.path(tmp, "README.txt"))
  outs <- list.files(tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
                     file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  message("srs build done: ", out_dir)
}
