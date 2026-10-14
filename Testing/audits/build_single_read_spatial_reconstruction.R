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
SRS_MANIFEST_VERSION <- "v1.1"
SRS_PER_RECORDING <- c("occupancy", "crosstab", "censored", "animals", "blocks", "knockout_metrics", "vendor_record_parity", "read_agreement",
                       "animal_block", "colocation", "gaps")
SRS_LONGEST_RELATIVE <- c(file.path("rec", "B6_CC4", "read_basis_runs.csv.gz"), file.path("tables", "knockout_materiality.csv"),
                          file.path("figures", "srs06_column_shares_with_unknown.png"), file.path("manifest", "output_files_sha256.csv"))

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
  tags <- data.table::as.data.table(rfid_animalpos_tag_map(tag_file))
  rec <- rfid_read_recording(cfg$raw_root, batch, cc, keep_read_offsets = TRUE)
  res <- asr_process_recording(vendor, rec, tags, man, batch, cc, basename(vf))
  rm(rec); invisible(gc())
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (nm in SRS_PER_RECORDING) data.table::fwrite(res[[nm]], file.path(d, paste0(nm, ".csv")))
  data.table::fwrite(res$runs, file.path(d, "read_basis_runs.csv.gz"))
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

# ---------------------------------------------------------------- srs_v1.1 summaries: denominators, unknown time, R5,
# missingness, change rates, movement exclusions (group-blind)
srs_summaries_v11 <- function(tab, man) {
  out <- list()
  L <- data.table::data.table(PositionID = 1:8, reader = paste0("R", 1:8), column = amv_column(1:8),
                              resource = c("food", "", "", "", "house", "", "", "water"))
  ok <- function(d) d[!sxm_spatial_excluded(man, Batch, CC, board)]
  q <- function(v) { v <- v[is.finite(v)]   # a unit with no supported time has no supported share
    if (!length(v)) return(list(n = 0L, median = NA_real_, q25 = NA_real_, q75 = NA_real_, min = NA_real_, max = NA_real_))
    list(n = length(v), median = stats::median(v), q25 = stats::quantile(v, 0.25, names = FALSE),
         q75 = stats::quantile(v, 0.75, names = FALSE), min = min(v), max = max(v)) }
  occ <- ok(data.table::copy(tab$occupancy))
  occ[, column := L$column[PositionID]]
  ab <- ok(data.table::copy(tab$animal_block))
  # denominators: observed time = block time before the censoring time, per animal-block; summed over the grouping
  obs_phase <- ab[, .(observed_s = sum(observed_s)), by = Phase]
  obs_ep <- ab[, .(observed_s = sum(observed_s)), by = .(CageEpisodeID, Batch, CC, board, Phase)]
  for (res_unit in c("column", "PositionID")) {
    nm <- if (res_unit == "column") "column" else "reader"
    pooled <- data.table::rbindlist(lapply(c("event", "read"), function(bs)
      asr_shares_with_unknown(occ[basis == bs], obs_phase, by = "Phase", unit = res_unit)[, basis := bs]))
    out[[paste0("shares_unknown_", nm, "_pooled")]] <- pooled[order(Phase, basis, bound, unit)]
    ep <- data.table::rbindlist(lapply(c("event", "read"), function(bs)
      asr_shares_with_unknown(occ[basis == bs], obs_ep, by = c("CageEpisodeID", "Batch", "CC", "board", "Phase"), unit = res_unit)[, basis := bs]))
    out[[paste0("shares_unknown_", nm, "_by_episode")]] <- ep
    out[[paste0("shares_unknown_", nm, "_episode_spread")]] <- ep[, c(q(share_all), list(supported_median = stats::median(share_supported, na.rm = TRUE),
                                                                                         supported_q25 = stats::quantile(share_supported, 0.25, na.rm = TRUE, names = FALSE),
                                                                                         supported_q75 = stats::quantile(share_supported, 0.75, na.rm = TRUE, names = FALSE))),
                                                                  by = .(Phase, basis, bound, unit)][order(Phase, basis, bound, unit)]
  }
  # R5 (house): shares by basis and bound, all observed time and supported time; read minus event per cage episode
  r5 <- out$shares_unknown_reader_by_episode[unit == "5"]
  r5w <- data.table::dcast(r5, CageEpisodeID + Batch + CC + board + Phase + bound ~ basis, value.var = c("share_all", "share_supported"), fill = 0)
  r5w[, `:=`(diff_all = share_all_read - share_all_event, diff_supported = share_supported_read - share_supported_event)]
  out$r5_basis_difference_by_episode <- r5w
  out$r5_basis_difference_spread <- rbind(r5w[, c(list(denominator = "observed time"), q(diff_all)), by = .(Phase, bound)],
                                          r5w[, c(list(denominator = "supported time"), q(diff_supported)), by = .(Phase, bound)])[order(Phase, bound, denominator)]
  out$r5_basis_difference_by_board <- r5w[bound == "max60s", .(cage_episodes = .N, diff_all_median = stats::median(diff_all, na.rm = TRUE),
                                                              diff_supported_median = stats::median(diff_supported, na.rm = TRUE)), by = .(Phase, board)][order(Phase, board)]
  out$r5_basis_difference_by_cc <- r5w[bound == "max60s", .(cage_episodes = .N, diff_all_median = stats::median(diff_all, na.rm = TRUE),
                                                           diff_supported_median = stats::median(diff_supported, na.rm = TRUE)), by = .(Phase, CC)][order(Phase, CC)]
  # support: share of observed time with a read <= 60 s (and <= 300 s) old, by phase, cage change, board, cage episode, animal
  sp <- occ[basis == "read", .(sup60 = sum(sup_60), sup300 = sum(sup_300)), by = .(SourceFile, Batch, CC, board, CageEpisodeID, AnimalNum, block, Phase)]
  # every observed animal-block stays in the denominator (an unassigned or unsupported one adds observed time, no support)
  sp <- merge(ab[, .(SourceFile, Batch, CC, board, CageEpisodeID, AnimalNum, block, Phase, observed_s)],
              sp[, .(SourceFile, AnimalNum, block, sup60, sup300)], by = c("SourceFile", "AnimalNum", "block"), all.x = TRUE)
  sp[is.na(sup60), `:=`(sup60 = 0, sup300 = 0)]
  agg <- function(by) sp[, .(observed_h = sum(observed_s) / 3600, supported_le60_share = sum(sup60) / sum(observed_s),
                             supported_le300_share = sum(sup300) / sum(observed_s)), by = by]
  out$support_by_phase <- agg("Phase")
  out$support_by_cc <- agg(c("Phase", "CC"))[order(Phase, CC)]
  out$support_by_board <- agg(c("Phase", "board"))[order(Phase, board)]
  spe <- agg(c("Phase", "CageEpisodeID", "Batch", "CC", "board"))
  spa <- agg(c("Phase", "CageEpisodeID", "AnimalNum"))
  out$support_spread <- rbind(spe[, c(list(unit = "cage episode"), q(supported_le60_share)), by = Phase],
                              spa[, c(list(unit = "animal"), q(supported_le60_share)), by = Phase])
  # missingness: unknown time by the reader last assigned and the length of its read gap; co-location with cage-mates
  gp <- ok(data.table::copy(tab$gaps))
  gp[, reader := ifelse(pos > 0, paste0("R", pos), "none")]
  out$unknown_by_reader_gap <- gp[, .(unknown_s = sum(unknown_s)), by = .(bound_s, Phase, reader, gap_class)][
    , share_of_phase_unknown := unknown_s / sum(unknown_s), by = .(bound_s, Phase)][order(bound_s, Phase, reader, gap_class)]
  out$unknown_by_gap <- gp[, .(unknown_s = sum(unknown_s)), by = .(bound_s, Phase, gap_class)][
    , share := unknown_s / sum(unknown_s), by = .(bound_s, Phase)][order(bound_s, Phase, gap_class)]
  co <- ok(data.table::copy(tab$colocation))
  co[, reader := ifelse(pos > 0, paste0("R", pos), "none")]
  colo <- co[mates > 0, .(seconds = sum(seconds), mate_same_reader = sum(mate_same_reader), mate_same_reader_supported = sum(mate_same_reader_supported),
                          mate_supported_anywhere = sum(mate_supported_anywhere)), by = .(bound_s, Phase, reader, state)]
  colo[, `:=`(mate_same_reader_share = mate_same_reader / seconds, mate_same_reader_supported_share = mate_same_reader_supported / seconds,
              mate_supported_anywhere_share = mate_supported_anywhere / seconds)]
  out$colocation_by_reader_state <- colo[order(bound_s, Phase, reader, state)]
  uc <- co[state == "unknown", .(unknown_s = sum(seconds)), by = .(bound_s, Phase, CC, board)]
  ob <- ab[, .(observed_s = sum(observed_s)), by = .(Phase, CC, board)][, .(bound_s = ASR_UNKNOWN_BOUNDS_S), by = .(Phase, CC, board, observed_s)]
  uc <- merge(ob, uc, by = c("bound_s", "Phase", "CC", "board"), all.x = TRUE)
  uc[is.na(unknown_s), unknown_s := 0]
  out$unknown_by_cc_board <- uc[, unknown_share := unknown_s / observed_s][order(bound_s, Phase, CC, board)]
  # changes of position per observed hour, both bases, reader and column resolution (descriptive; not locomotion)
  cr <- ab[, .(observed_h = sum(observed_s) / 3600, event_reader = sum(changes_event_reader), event_column = sum(changes_event_column),
               read_reader = sum(changes_read_reader), read_column = sum(changes_read_column)), by = .(Phase, CageEpisodeID, AnimalNum)]
  crl <- data.table::melt(cr, id.vars = c("Phase", "CageEpisodeID", "AnimalNum", "observed_h"), variable.name = "basis_resolution", value.name = "changes")
  crl <- crl[observed_h > 0][, per_h := changes / observed_h]
  out$change_rates <- crl[, c(list(pooled_per_h = sum(changes) / sum(observed_h)), q(per_h)), by = .(Phase, basis_resolution)][order(Phase, basis_resolution)]
  # movement exclusions (manifest exclude_movement): the Stage 29 animal-windows left out of new primary analyses
  if (!is.null(tab$knockout_metrics)) {
    w <- unique(tab$knockout_metrics[variant == "baseline", .(Batch, CC, board, CageEpisodeID, AnimalNum, window)])
    # window-specific: only the ACTIVE windows of an exclude_movement cage episode leave new primary analyses of the crossing
    # rate and fragmentation; its light windows stay (the light-phase rate is evaluated separately)
    w[, episode_movement_excluded := sxm_movement_excluded(man, Batch, CC, board)]
    w[, movement_excluded := episode_movement_excluded & window == "active"]
    w[, primary_use := ifelse(movement_excluded, "left out of new primary analyses of crossing rate and fragmentation; kept in the sensitivity analysis",
                              "kept (light-phase crossing rate evaluated separately)")]
    out$movement_exclusion_windows <- w[episode_movement_excluded == TRUE][order(CageEpisodeID, AnimalNum, window)]
  }
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
  u <- data.table::copy(sm$shares_unknown_column_pooled)[basis == "read"]
  u[, unit := factor(ifelse(unit == "unknown", "unknown", paste0("C", unit)), levels = c("C1", "C2", "C3", "C4", "unknown"))]
  u[, bound := factor(bound, levels = c("LOCF", "max300s", "max60s"))]
  p <- ggplot2::ggplot(u, ggplot2::aes(bound, share_all, fill = unit)) + ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = c(C1 = "grey20", C2 = "grey45", C3 = "grey65", C4 = "grey82", unknown = MMM_PALETTE_DIVERGING[["high"]]), name = NULL) +
    ggplot2::facet_wrap(~Phase) + th +
    ggplot2::labs(x = "carry-forward", y = "share of observed time", title = "Column occupancy with unsupported time as its own category (read basis)",
                  subtitle = "LOCF: unrestricted; max300s / max60s: positions older than the bound are unknown, not redistributed")
  sv(p, "srs06_column_shares_with_unknown", 8, 4)
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
  sm <- c(sm, srs_summaries_v11(tab, MAN))
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
               "No group label, CombZ or outcome was read. Specification: docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.1.md."),
             file.path(tmp, "README.txt"))
  outs <- list.files(tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
                     file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  message("srs build done: ", out_dir)
}
