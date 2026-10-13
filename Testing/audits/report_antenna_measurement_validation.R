# ================================================================
# Report: measurement validation of the raw antenna logs -- summaries, cage-level descriptions and figures
# MMMSociability -- Testing/audits/report_antenna_measurement_validation.R
# ================================================================
# UNREGISTERED measurement validation (2026-10-13); descriptive only. No p-values, no model fits, no threshold chosen by
# any outcome. Reads <OUT>/tables/ written by Testing/audits/audit_antenna_measurement_validation.R, the CombZ table
# (group and sex), and read-only the preprocessed streams of the vendor v2 version and of the raw-antenna data versions
# (frozen Stage 29 stream and metric functions, for the method comparison and the parity check). Writes
# <OUT>/summary/*.csv and <OUT>/figures/*.png. OUT = MMM_AMV_OUT (default <Behavior>/RANM/measurement_validation).
# Cage-level intervals: percentile bootstrap over cages (cage epoch = SourceFile x board) within group x sex, 2000
# draws, seed 20261013; cage-mates are never treated as independent units.
# Run from the repository root:  Rscript Testing/audits/report_antenna_measurement_validation.R
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

AMV_SEED <- 20261013
AMV_BOOT <- 2000
AMV_GROUP_COLS <- c(CON = "#7F7F7F", RES = "#2C7FB8", SUS = "#D95F0E")

#' Cage-level description of an animal-level value: the mean per cage, then per stratum the number of cages and animals,
#' the mean of the cage means and a percentile bootstrap interval over cages.
amv_cage_summary <- function(d, value, strata, cage = "cage", seed = AMV_SEED, B = AMV_BOOT) {
  d <- d[is.finite(get(value))]
  cg <- d[, .(v = mean(get(value)), n_animals = .N), by = c(strata, cage)]
  set.seed(seed)
  cg[, {
    m <- mean(v)
    bt <- if (.N >= 2) replicate(B, mean(sample(v, .N, replace = TRUE))) else NA_real_
    .(cages = .N, animals = sum(n_animals), cage_mean = m, boot_lo = quantile(bt, 0.025, na.rm = TRUE, names = FALSE),
      boot_hi = quantile(bt, 0.975, na.rm = TRUE, names = FALSE), cage_min = min(v), cage_max = max(v))
  }, by = strata]
}

if (sys.nframe() == 0L) {
  data.table::setDTthreads(1L)
  for (f in c("Functions/project_paths.R", "Functions/behavioral_dynamics_helpers.R", "Functions/rfid_event_stream.R",
              "Functions/rfid_binfree_metrics.R", "Functions/animalpos_preprocessing_helpers.R", "Functions/antenna_log_helpers.R",
              "Functions/antenna_measurement.R")) source(f)
  LIVE <- MMM_PROJECT_ROOT_DEFAULT
  BEH <- dirname(LIVE)
  OUT <- Sys.getenv("MMM_AMV_OUT", unset = file.path(BEH, "RANM", "measurement_validation"))
  TAB <- file.path(OUT, "tables"); SUM <- file.path(OUT, "summary"); FIG <- file.path(OUT, "figures")
  dir.create(SUM, showWarnings = FALSE); dir.create(FIG, showWarnings = FALSE)
  rd <- function(n) {   # window_metrics carries CC twice (the recording key and the frozen metric key; equal by construction)
    x <- fread(file.path(TAB, paste0(n, ".csv")), colClasses = if (n %in% c("integrity", "silences", "system_messages", "reader_concurrency", "reversal_hist"))
      NULL else c(AnimalNum = "character"))
    x[, which(!duplicated(names(x))), with = FALSE]
  }
  wsum <- function(x, n) fwrite(x, file.path(SUM, paste0(n, ".csv")))
  grp <- fread(file.path(LIVE, "analysis_ready/canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv"),
               colClasses = c(AnimalNum = "character"))[, .(AnimalNum = canonical_animal_id(AnimalNum), Group = outcome_group, Sex)]
  # the tables as written; the animal key is canonical already
  ab <- merge(rd("animal_block"), grp, by = "AnimalNum", all.x = TRUE)
  ab[, cage := paste(SourceFile, board)]
  ab[, `:=`(GroupSex = paste(Group, Sex), cage_type = fifelse(Group == "CON", "CON cage", "SIS cage"))]

  # 1. Detection completeness and gaps ------------------------------------------------------------------------------------
  integ <- rd("integrity"); wsum(integ, "01_integrity_by_recording")
  sm <- fread(file.path(TAB, "system_messages.csv"), colClasses = "character")
  wsum(sm[, .N, by = .(SystemMsg, unitLabel)][order(-N)], "01_system_message_types")
  sil <- rd("silences")
  wsum(sil[, .(n_ge60 = .N, s_ge60 = sum(dur_s), n_ge300 = sum(dur_s >= 300), s_ge300 = sum(dur_s[dur_s >= 300]), longest_s = max(dur_s)),
           by = .(SourceFile, level, board)], "01_silences_by_recording")
  wsum(sil[level == "recording" & dur_s >= 300], "01_recording_silences_ge300s")
  age_cols <- grep("^age_gt", names(ab), value = TRUE)
  pa <- ab[, c(list(block_s = sum(block_s), reads = sum(reads), events = sum(events), single_read_events = sum(single_read_events),
                    detected_s = sum(detected_s), gaps_ge60_s = sum(gaps_ge60_s), gaps_ge600_s = sum(gaps_ge600_s),
                    in_board_sil = sum(gaps_ge60_in_board_silence_s), in_rec_sil = sum(gaps_ge60_in_recording_silence_s),
                    changes = sum(changes), simultaneous = sum(changes_simultaneous), reversals_lt2s = sum(reversals_lt2s_all),
                    reversals_lt2s_row = sum(reversals_lt2s_row), reversals_lt2s_col1 = sum(reversals_lt2s_col1), changes_far = sum(changes_far)),
               lapply(.SD, sum)), by = .(SourceFile, Batch, CC, AnimalNum, board, cage, Phase, Group, Sex, GroupSex), .SDcols = age_cols]
  pa[, `:=`(reads_per_h = reads / (block_s / 3600), events_per_h = events / (block_s / 3600), detected_share = detected_s / block_s,
            single_read_share = single_read_events / events, gap60_share = gaps_ge60_s / block_s, gap600_share = gaps_ge600_s / block_s,
            gap60_in_board_silence_share = in_board_sil / gaps_ge60_s, gap60_in_recording_silence_share = in_rec_sil / gaps_ge60_s,
            changes_per_h = changes / (block_s / 3600), simultaneous_share = simultaneous / changes,
            reversals_lt2s_per_h = reversals_lt2s / (block_s / 3600), reversal_lt2s_share = 2 * reversals_lt2s / changes,
            changes_per_kread = 1000 * changes / reads, far_changes_per_h = changes_far / (block_s / 3600))]
  for (a in age_cols) pa[, (sub("_s$", "_share", a)) := get(a) / block_s]
  wsum(pa, "01_detection_by_animal_phase")
  dmeas <- c("reads_per_h", "detected_share", "age_gt10s_share", "age_gt60s_share", "age_gt300s_share", "gap600_share", "single_read_share")
  ds <- rbindlist(lapply(dmeas, function(m) amv_cage_summary(pa[!is.na(Group)], m, c("Phase", "Sex", "Group"))[, measure := m]))
  wsum(ds, "01_detection_cage_summary_by_group")
  pooled_gap <- pa[, .(gap60_s = sum(gaps_ge60_s), in_board_silence_s = sum(in_board_sil), in_recording_silence_s = sum(in_rec_sil)), by = Phase]
  pooled_gap[, `:=`(board_share = in_board_silence_s / gap60_s, recording_share = in_recording_silence_s / gap60_s)]
  wsum(pooled_gap, "01_gap_attribution_pooled")
  wsum(pa[, c(list(animals = .N), lapply(.SD, median)), by = .(Batch, Phase), .SDcols = dmeas], "01_detection_median_by_batch")
  wsum(pa[, c(list(animals = .N), lapply(.SD, median)), by = .(CC, Phase), .SDcols = dmeas], "01_detection_median_by_cc")
  wsum(pa[, c(list(animals = .N), lapply(.SD, median)), by = .(board, Phase), .SDcols = dmeas], "01_detection_median_by_board")

  # readers: detection density per carry-forward second, by reader position
  arb <- rd("animal_reader_block")
  rp <- arb[, .(locf_s = sum(locf_s), reads = sum(reads), detected_s = sum(detected_s)), by = .(SourceFile, board, Phase, PositionID)]
  rp[, `:=`(reads_per_locf_s = reads / locf_s, detected_per_locf_s = detected_s / locf_s)]
  wsum(rp, "01_reader_density_by_board")
  wsum(rp[locf_s > 600, .(boards = .N, median_reads_per_locf_s = median(reads_per_locf_s), q25 = quantile(reads_per_locf_s, 0.25),
                          q75 = quantile(reads_per_locf_s, 0.75), median_detected_per_locf_s = median(detected_per_locf_s),
                          locf_share = sum(locf_s)), by = .(Phase, PositionID)][, locf_share := locf_share / sum(locf_share), by = Phase][order(Phase, PositionID)],
       "01_reader_density_by_position")
  rc <- rd("reader_concurrency")
  wsum(rc[, .(events = sum(events), single_read_share = sum(single_read_events) / sum(events),
              mean_read_interval_s = sum(multi_read_dur_s) / sum(multi_read_intervals)), by = .(Phase, concurrent_tags)][order(Phase, concurrent_tags)],
       "01_reader_concurrency")
  cd <- merge(rd("coassign_density"), grp, by = "AnimalNum", all.x = TRUE)
  shr <- cd[, .(shared_reader_share = sum(seconds[mates_same_reader >= 1]) / sum(seconds),
                p_read_alone = sum(seconds_with_read[mates_same_reader == 0]) / sum(seconds[mates_same_reader == 0]),
                p_read_shared = sum(seconds_with_read[mates_same_reader >= 1]) / sum(seconds[mates_same_reader >= 1])), by = .(SourceFile, AnimalNum, Phase)]
  pa <- merge(pa, shr, by = c("SourceFile", "AnimalNum", "Phase"), all.x = TRUE)
  wsum(pa, "01_detection_by_animal_phase")
  dm2 <- c("shared_reader_share", "p_read_alone", "p_read_shared", "changes_per_h", "reversals_lt2s_per_h", "reversal_lt2s_share")
  wsum(rbindlist(lapply(dm2, function(m) amv_cage_summary(pa[!is.na(Group)], m, c("Phase", "Sex", "Group"))[, measure := m])), "01_density_cage_summary_by_group")
  wsum(pa[, .(animals = .N, spearman_reads_vs_changes = cor(reads_per_h, changes_per_h, method = "spearman"),
              spearman_reads_vs_reversals = cor(reads_per_h, reversals_lt2s_per_h, method = "spearman"),
              spearman_shared_vs_reads = cor(shared_reader_share, reads_per_h, method = "spearman", use = "complete.obs"),
              spearman_shared_vs_changes = cor(shared_reader_share, changes_per_h, method = "spearman", use = "complete.obs")), by = Phase],
       "01_detection_switching_correlations")
  wsum(cd[, .(seconds = sum(seconds), p_read_second = sum(seconds_with_read) / sum(seconds), reads_per_s = sum(reads) / sum(seconds)),
          by = .(Phase, mates_same_reader)][order(Phase, mates_same_reader)], "01_coassign_density_pooled")
  wsum(cd[!is.na(Group), .(seconds = sum(seconds), p_read_second = sum(seconds_with_read) / sum(seconds)),
          by = .(Phase, Sex, Group, mates_same_reader)][, time_share := seconds / sum(seconds), by = .(Phase, Sex, Group)][order(Phase, Sex, Group, mates_same_reader)],
       "01_coassign_density_by_group")

  # 2. Switching ----------------------------------------------------------------------------------------------------------
  geo <- c("row", "col1", "diag1", "far")
  sw <- ab[, c(list(block_s = sum(block_s), changes = sum(changes)), lapply(.SD, sum)), by = .(Phase),
           .SDcols = c(paste0("changes_", geo), "changes_simultaneous", "changes_gap_le1s", "changes_gap_1to10s", "changes_gap_gt10s",
                       grep("^reversals_lt", names(ab), value = TRUE))]
  wsum(sw, "02_switching_pooled")
  rh <- rd("reversal_hist")
  wsum(rh[, .(n = sum(n)), by = .(Phase, kind, geometry, bin)], "02_visit_duration_hist")

  # 3-4. Reconstructions and metric sensitivity ---------------------------------------------------------------------------
  wm <- merge(rd("window_metrics"), grp, by = "AnimalNum", all.x = TRUE)
  wm <- merge(wm, unique(ab[, .(SourceFile, AnimalNum, System = paste0("sys.", board))]), by = c("SourceFile", "AnimalNum"), all.x = TRUE)
  wm[, delta := as.numeric(delta)]
  wsum(wm, "03_window_metrics_all")
  ms <- c("crossing_rate", "fragmentation", "fragmentation_half", "fragmentation_double", "fragmentation_sibly_2process", "events_per_bout",
          "occupancy_dispersion", "dominant_share", "upper_row_share", "column_dispersion", "p_C1", "crossing_rate_supported")
  ref <- wm[basis == "event_start" & tau == 2 & is.infinite(delta)]   # reference: the flip2s rule
  sens <- rbindlist(lapply(ms, function(m) wm[, {
    j <- merge(.SD[, .(SourceFile, AnimalNum, v = get(m))], ref[window == .BY$window, .(SourceFile, AnimalNum, r = get(m))], by = c("SourceFile", "AnimalNum"))
    .(metric = m, animals = .N, median = median(get(m), na.rm = TRUE), q25 = quantile(get(m), 0.25, na.rm = TRUE), q75 = quantile(get(m), 0.75, na.rm = TRUE),
      spearman_vs_tau2 = suppressWarnings(cor(j$v, j$r, method = "spearman", use = "complete.obs")),
      median_rel_change_vs_tau2 = median((j$v - j$r) / abs(j$r), na.rm = TRUE))
  }, by = .(window, basis, tau, delta)]))
  wsum(sens, "04_metric_sensitivity")
  wsum(wm[, .(supported_share_median = median(obs_s / window_s), supported_share_q10 = quantile(obs_s / window_s, 0.1)), by = .(window, basis, tau, delta)],
       "03_supported_share")
  wb <- rd("window_basis")
  wsum(wb[, .(animal_windows = .N, share_seconds_differ_median = median(share_seconds_differ), share_seconds_differ_q90 = quantile(share_seconds_differ, 0.9),
              share_column_differs_median = median(share_column_differs), max_abs_reader_share_diff_median = median(max_abs_reader_share_diff),
              max_abs_reader_share_diff_max = max(max_abs_reader_share_diff)), by = .(window, tau)], "03_event_start_vs_read_basis")

  # the data versions through the frozen Stage 29 stream (parity check and comparison)
  dvs <- data.table(version = c("vendor", "noflip", "flip2s", "ge200", "avg500"),
                    pre = c(file.path(LIVE, "MMMSociability/data_versions/v2_cage_label_correction_2026-09-28/preprocessed_data"),
                            file.path(BEH, "RANU/MMMSociability/data_versions/raw_antenna_noflip_20261012/preprocessed_data"),
                            file.path(BEH, "RANT/MMMSociability/data_versions/raw_antenna_flip2s_20261012/preprocessed_data"),
                            file.path(BEH, "RANV/MMMSociability/data_versions/raw_antenna_ge200_20261013/preprocessed_data"),
                            file.path(BEH, "RANA/MMMSociability/data_versions/raw_antenna_avg500_20261013/preprocessed_data")))
  dv_file <- file.path(SUM, "03_data_version_metrics.csv")   # reused on reruns unless MMM_AMV_RECOMPUTE_DV=1
  dv <- if (file.exists(dv_file) && Sys.getenv("MMM_AMV_RECOMPUTE_DV") != "1") fread(dv_file, colClasses = c(AnimalNum = "character")) else
    rbindlist(lapply(seq_len(nrow(dvs)), function(i) {
    st <- mmm_evs_build_stream(dvs$pre[i], file.path(LIVE, "MMMSociability", "raw_data"))
    rbindlist(lapply(c("active", "light"), function(w) {
      w_s <- mmm_evs_window_stream(st, w)
      wt <- mmm_bf_window_table(w_s, 39.3970988275363)
      oc <- w_s$runs[, as.data.table(amv_occupancy(.SD)), by = .(AnimalNum, CC)]
      merge(wt$animals, oc[, !c("obs_s", "occupancy_dispersion", "dominant_share")], by = c("AnimalNum", "CC"))[, `:=`(version = dvs$version[i], window = w)] }))
  }))
  dv[, AnimalNum := canonical_animal_id(AnimalNum)]
  wsum(dv, "03_data_version_metrics")
  par <- merge(dv[version %in% c("noflip", "flip2s"), .(version, window, CC, AnimalNum, crossing_rate, occupancy_dispersion, fragmentation, shared_zone_use)],
               merge(wm[basis == "event_start" & is.infinite(delta) & tau %in% c(0, 2), .(tau, window, CC, AnimalNum, crossing_rate, occupancy_dispersion, fragmentation)],
                     data.table(tau = c(0, 2), version = c("noflip", "flip2s")), by = "tau"),
               by = c("version", "window", "CC", "AnimalNum"), suffixes = c("_dv", "_amv"))
  wsum(par[, .(windows = .N, max_abs_crossing_rate = max(abs(crossing_rate_dv - crossing_rate_amv)),
               max_abs_dispersion = max(abs(occupancy_dispersion_dv - occupancy_dispersion_amv)),
               max_abs_fragmentation = max(abs(fragmentation_dv - fragmentation_amv), na.rm = TRUE)), by = .(version, window)], "03_parity_with_data_versions")

  # 4b. Co-assignment: per animal-window ratio of sums over tracked mates
  dy <- rd("window_dyads")
  dy[, delta := as.numeric(delta)]
  long <- rbindlist(list(dy[, .(AnimalNum = A, window, CC, SourceFile, System, basis, tau, delta, joint_s, same_s, same_col_s, adj_col_s, indep_same, indep_same_col,
                                  indep_adj_col, short_null_same_mean, long_null_same_mean, short_null_same_col_mean, long_null_same_col_mean,
                                  short_null_adj_col_mean, long_null_adj_col_mean, exact_same_s, exact_obs_s)],
                         dy[, .(AnimalNum = B, window, CC, SourceFile, System, basis, tau, delta, joint_s, same_s, same_col_s, adj_col_s, indep_same, indep_same_col,
                                  indep_adj_col, short_null_same_mean, long_null_same_mean, short_null_same_col_mean, long_null_same_col_mean,
                                  short_null_adj_col_mean, long_null_adj_col_mean, exact_same_s, exact_obs_s)]))
  co <- long[, .(mates = .N, joint_s = sum(joint_s),
                 same = sum(same_s) / sum(joint_s), same_col = sum(same_col_s) / sum(joint_s), adj_col = sum(adj_col_s) / sum(joint_s),
                 exp_indep_same = sum(indep_same * joint_s) / sum(joint_s), exp_short_same = sum(short_null_same_mean * joint_s) / sum(joint_s),
                 exp_long_same = sum(long_null_same_mean * joint_s) / sum(joint_s),
                 exp_indep_same_col = sum(indep_same_col * joint_s) / sum(joint_s), exp_short_same_col = sum(short_null_same_col_mean * joint_s) / sum(joint_s),
                 exp_indep_adj_col = sum(indep_adj_col * joint_s) / sum(joint_s), exp_short_adj_col = sum(short_null_adj_col_mean * joint_s) / sum(joint_s),
                 shared_zone_use_exact = sum(exact_same_s) / sum(exact_obs_s)),
             by = .(AnimalNum, window, CC, SourceFile, System, basis, tau, delta)]
  co[, `:=`(excess_short_same = same - exp_short_same, excess_indep_same = same - exp_indep_same, excess_short_same_col = same_col - exp_short_same_col,
            excess_short_adj_col = adj_col - exp_short_adj_col)]
  co <- merge(co, grp, by = "AnimalNum", all.x = TRUE)
  wsum(co, "04_coassignment_by_animal")
  wsum(co[, .(animals = .N, same_median = median(same), exp_indep_median = median(exp_indep_same), exp_short_median = median(exp_short_same),
              exp_long_median = median(exp_long_same), excess_short_median = median(excess_short_same),
              r2_same_on_indep = suppressWarnings(cor(same, exp_indep_same)^2), r2_same_on_short = suppressWarnings(cor(same, exp_short_same)^2),
              same_col_median = median(same_col), adj_col_median = median(adj_col),
              max_abs_grid_vs_exact = suppressWarnings(max(abs(same - shared_zone_use_exact), na.rm = TRUE)),
              share_below_short_null = mean(same < exp_short_same), share_col_below_short_null = mean(same_col < exp_short_same_col),
              share_adj_below_short_null = mean(adj_col < exp_short_adj_col), r2_same_col_on_short = suppressWarnings(cor(same_col, exp_short_same_col)^2)),
          by = .(window, basis, tau, delta)], "04_coassignment_summary")
  sz_dv <- dv[, .(version, window, CC, AnimalNum, shared_zone_use)]
  co_cmp <- merge(co[, .(basis, tau, delta, window, CC, AnimalNum, same, excess_short_same)], sz_dv[version == "vendor", .(window, CC, AnimalNum, vendor = shared_zone_use)],
                  by = c("window", "CC", "AnimalNum"))
  wsum(co_cmp[, .(animals = .N, spearman_same_vs_vendor = cor(same, vendor, method = "spearman", use = "complete.obs"),
                  spearman_excess_vs_vendor = cor(excess_short_same, vendor, method = "spearman", use = "complete.obs")), by = .(window, basis, tau, delta)],
       "04_coassignment_vs_vendor")

  # 5. Group, cage and system ---------------------------------------------------------------------------------------------
  wm[, cage := paste(SourceFile, System)]
  key_m <- c("crossing_rate", "fragmentation", "occupancy_dispersion", "dominant_share")
  gs <- rbindlist(lapply(key_m, function(m) amv_cage_summary(wm[!is.na(Group) & is.infinite(delta)], m, c("window", "CC", "basis", "tau", "Sex", "Group"))[, measure := m]))
  wsum(gs, "05_metric_cage_summary_by_group")
  # male CON vs SIS within batch (CC1 active), per tau: cage means
  cm <- wm[!is.na(Group) & is.infinite(delta) & window == "active", .(v = mean(crossing_rate), Group = Group[1], Sex = Sex[1]), by = .(Batch, CC, cage, basis, tau)]
  cm[, cage_type := fifelse(Group == "CON", "CON", "SIS")]
  within <- cm[, .(con = mean(v[cage_type == "CON"]), sis = mean(v[cage_type == "SIS"]), sis_cages = sum(cage_type == "SIS")), by = .(Batch, CC, basis, tau, Sex)]
  within[, `:=`(diff = con - sis, ratio = con / sis)]
  wsum(within, "05_con_minus_sis_within_batch_crossing")
  dmeas3 <- c("reads_per_h", "age_gt60s_share", "changes_per_h", "changes_per_kread", "far_changes_per_h", "reversals_lt2s_per_h", "shared_reader_share",
              "p_read_alone", "p_read_shared")
  dcov <- pa[!is.na(Group), c(list(Group = Group[1], Sex = Sex[1]), lapply(.SD, mean)), by = .(Batch, CC, Phase, cage), .SDcols = dmeas3]
  dcov[, cage_type := fifelse(Group == "CON", "CON", "SIS")]
  dl <- melt(dcov, id.vars = c("Batch", "CC", "Phase", "cage", "Group", "Sex", "cage_type"), variable.name = "measure")
  wsum(dcast(dl[, .(v = mean(value, na.rm = TRUE)), by = .(Batch, CC, Phase, Sex, measure, cage_type)], Batch + CC + Phase + Sex + measure ~ cage_type, value.var = "v")[
    , ratio_con_sis := CON / SIS][], "05_con_vs_sis_detection_within_batch")
  wsum(co[!is.na(Group), .(cage_mean_excess = mean(excess_short_same), cage_mean_same = mean(same), Group = Group[1], Sex = Sex[1]), by = .(window, CC, basis, tau, delta, SourceFile, System)],
       "05_coassignment_cage_means")
  # system (board): SIS cages only, each cage relative to the mean of its batch x CC; and where the CON cage sat
  bc <- pa[Group %in% c("RES", "SUS"), .(reads_h = mean(reads_per_h), chg_h = mean(changes_per_h)), by = .(Batch, CC, board, Phase)]
  bc[, `:=`(rel_reads = reads_h / mean(reads_h), rel_changes = chg_h / mean(chg_h)), by = .(Batch, CC, Phase)]
  wsum(bc[, .(sis_cage_epochs = .N, rel_reads_median = median(rel_reads), rel_reads_min = min(rel_reads), rel_reads_max = max(rel_reads),
              rel_changes_median = median(rel_changes)), by = .(board, Phase)][order(Phase, board)], "05_board_effect_sis_cages")
  wsum(dcast(unique(pa[Group == "CON", .(Batch, CC, board)]), Batch ~ CC, value.var = "board"), "05_con_cage_board")
  # each CON cage against the SIS cages on the same board in the other cage changes of its batch (board-matched; mixes CC)
  cg <- pa[!is.na(Group), .(chg = mean(changes_per_h), reads = mean(reads_per_h), Sex = Sex[1], con = all(Group == "CON")), by = .(Batch, CC, board, Phase, cage)]
  bm <- cg[con == TRUE, {
    s <- cg[con == FALSE & Batch == .BY$Batch & board == .BY$board & Phase == .BY$Phase & CC != .BY$CC]
    .(Sex = Sex, con_changes_per_h = chg, sis_same_board_changes_per_h = mean(s$chg), sis_cages = nrow(s), ratio_changes = chg / mean(s$chg),
      con_reads_per_h = reads, sis_same_board_reads_per_h = mean(s$reads), ratio_reads = reads / mean(s$reads)) }, by = .(Batch, CC, board, Phase)]
  wsum(bm, "05_con_vs_same_board_sis")
  wsum(bm[, .(con_cages = .N, median_ratio_changes = median(ratio_changes), cages_below_1 = sum(ratio_changes < 1),
              median_ratio_reads = median(ratio_reads)), by = .(Sex, Phase)], "05_con_vs_same_board_sis_summary")
  message("summaries written to ", SUM)

  # Figures ---------------------------------------------------------------------------------------------------------------
  source("Functions/manuscript_palette.R")
  GCOL <- MMM_PALETTE_GROUP
  MCOL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6")   # reference categorical slots 1-7
  th <- theme_minimal(base_size = 9) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), legend.position = "top")
  sv <- function(p, n, w = 7, h = 4.5) ggsave(file.path(FIG, paste0(n, ".png")), p, width = w, height = h, dpi = 200, bg = "white")

  # F2 visit durations
  vh <- rh[, .(n = sum(n)), by = .(Phase, kind, geometry, bin)]
  vh[, lo := as.numeric(sub("^[\\[(]([^,]+),.*$", "\\1", bin))]
  vh[, share := n / sum(n), by = .(Phase, kind, geometry)]
  vh[, geometry := factor(geometry, levels = geo, labels = c("other row, same column (116)", "neighbouring column (100)", "diagonal neighbour (153)", "2+ columns (>= 200)"))]
  p <- ggplot(vh[kind == "returning"], aes(10^(lo + 0.05), share, colour = geometry)) + geom_line(linewidth = 0.6) +
    geom_vline(xintercept = c(0.5, 1, 2, 5, 10, 20), linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 60, 600), labels = c("0.1", "0.5", "1", "2", "5", "10", "20", "60", "600")) +
    scale_colour_manual(values = MCOL[1:4], name = NULL) + facet_wrap(~Phase) + th +
    labs(x = "duration of the visit to B before returning to A (s, log scale); dashed: flip-filter thresholds tested",
         y = "share of returning visits (0.1-log10 bins)", title = "A -> B -> A visits by reader geometry, all 24 recordings")
  sv(p, "F2_returning_visit_durations")

  # F3 support age
  sa <- melt(pa[!is.na(Group)], id.vars = c("cage", "Phase", "Sex", "Group", "AnimalNum"), measure.vars = patterns("^age_gt.*_share$"), variable.name = "bound", value.name = "share")
  sa[, bound_s := as.numeric(sub("^age_gt([0-9.]+)s_share$", "\\1", bound))]
  sa <- sa[, .(share = mean(share)), by = .(cage, Phase, Sex, Group, bound_s)][, .(share = mean(share), lo = min(share), hi = max(share)), by = .(Phase, Sex, Group, bound_s)]
  p <- ggplot(sa, aes(bound_s, share, colour = Group)) + geom_line(linewidth = 0.6) + geom_point(size = 1.2) +
    scale_x_log10(breaks = c(1, 2, 5, 10, 30, 60, 300, 1800)) + scale_colour_manual(values = GCOL) + facet_grid(Sex ~ Phase) + th +
    labs(x = "time since the latest detection, bound (s, log scale)", y = "share of time the carried position is older (mean of cage means)",
         title = "How much of the carry-forward assignment rests on old detections")
  sv(p, "F3_support_age")

  # F4 reader detection density
  rpp <- rp[locf_s > 600]
  rpp[, reader := factor(paste0("R", PositionID), levels = paste0("R", c(5:8, 1:4)))]
  p <- ggplot(rpp, aes(reader, reads_per_locf_s)) + geom_boxplot(outlier.size = 0.4, width = 0.6, fill = "grey92") + facet_wrap(~Phase) + th +
    labs(x = "reader (R5-R8 upper row y = 116, R1-R4 lower row y = 0; columns x = 0-300)", y = "reads per carry-forward second at the reader",
         title = "Detection density by reader position (one point per board x recording x phase)")
  sv(p, "F4_reader_detection_density")

  # F5 detection when cage-mates are assigned to the same reader
  cdp <- cd[, .(p_read_second = sum(seconds_with_read) / sum(seconds), seconds = sum(seconds)), by = .(Phase, mates_same_reader)]
  p <- ggplot(cdp, aes(factor(mates_same_reader, labels = c("0", "1", "2", "3+")), p_read_second)) + geom_col(width = 0.6, fill = MCOL[1]) +
    facet_wrap(~Phase) + th + labs(x = "cage-mates assigned (carry-forward) to the same reader", y = "share of seconds with at least one read",
                                   title = "Reads per animal drop when animals share a reader (one tag read at a time)")
  sv(p, "F5_detection_by_shared_reader", w = 6, h = 3.5)

  # F6 filtering and support sensitivity
  fs <- sens[basis == "event_start" & metric %in% c("crossing_rate", "fragmentation", "occupancy_dispersion", "dominant_share", "upper_row_share", "crossing_rate_supported")]
  fs[, delta_lab := factor(ifelse(is.infinite(delta), "unbounded", paste0(delta, " s")), levels = c("unbounded", "300 s", "60 s", "30 s", "10 s", "5 s"))]
  p <- ggplot(fs[window == "active"], aes(pmax(tau, 0.25), median, colour = delta_lab)) + geom_line(linewidth = 0.5) + geom_point(size = 1) +
    scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 5, 10, 20), labels = c("0", "0.5", "1", "2", "5", "10", "20")) +
    scale_colour_manual(values = c("#0d366b", "#256abf", "#3987e5", "#6da7ec", "#9ec5f4", "#cde2fb"), name = "carry-forward bound") +
    facet_wrap(~metric, scales = "free_y") + th +
    labs(x = "flip-filter threshold tau (s, log scale; 0 = raw carry-forward)", y = "median over animal-windows (CC1-CC4, active)",
         title = "Metric medians across reconstruction parameters")
  sv(p, "F6_filter_sensitivity_medians", w = 8, h = 5)
  p <- ggplot(fs[window == "active"], aes(pmax(tau, 0.25), spearman_vs_tau2, colour = delta_lab)) + geom_line(linewidth = 0.5) + geom_point(size = 1) +
    scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 5, 10, 20), labels = c("0", "0.5", "1", "2", "5", "10", "20")) +
    scale_colour_manual(values = c("#0d366b", "#256abf", "#3987e5", "#6da7ec", "#9ec5f4", "#cde2fb"), name = "carry-forward bound") +
    facet_wrap(~metric) + th + labs(x = "flip-filter threshold tau (s, log scale)", y = "Spearman rho with tau = 2 s, unbounded",
                                    title = "Rank agreement with the 2-s reference")
  sv(p, "F6_filter_sensitivity_rank", w = 8, h = 5)

  # F7 column occupancy by method (CC1 active)
  occm <- wm[CC == "CC1" & window == "active" & ((basis == "event_start" & tau %in% c(0, 2) & is.infinite(delta)) | (basis == "event_start" & tau == 2 & delta == 60) |
                                                  (basis == "read" & tau == 2 & is.infinite(delta))),
             .(method = fifelse(basis == "read", "raw reads, tau 2 s", fifelse(is.infinite(delta), paste0("raw events, tau ", tau, " s"), "raw events, tau 2 s, bounded 60 s")),
               AnimalNum, Group, Sex, p_C1, p_C2, p_C3, p_C4)]
  occm <- rbind(occm, merge(dv[CC == "CC1" & window == "active" & version %in% c("vendor", "ge200"), .(method = version, AnimalNum, p_C1, p_C2, p_C3, p_C4)],
                            grp, by = "AnimalNum"), use.names = TRUE)
  om <- melt(occm[!is.na(Group)], id.vars = c("method", "AnimalNum", "Group", "Sex"), variable.name = "column", value.name = "share")
  om <- om[, .(share = mean(share)), by = .(method, Group, Sex, column)]
  p <- ggplot(om, aes(column, share, colour = method, group = method)) + geom_line(linewidth = 0.5) + geom_point(size = 1.2) +
    scale_colour_manual(values = MCOL[1:6], name = NULL) + facet_grid(Sex ~ Group) + th + guides(colour = guide_legend(nrow = 2)) +
    labs(x = "column (C1 = x 0 ... C4 = x 300)", y = "mean share of assigned time", title = "Column occupancy by reconstruction, CC1 first active phase")
  sv(p, "F7_column_occupancy_by_method", w = 8, h = 4.5)

  # F8 co-assignment against its chance expectation
  cop <- co[(basis == "event_start" & tau == 2 & delta %in% c(Inf, 60)) | (basis == "read" & tau == 2 & is.infinite(delta))]
  cop[, combo := factor(paste0(ifelse(basis == "read", "reads", "events"), ", tau ", tau, " s, ", ifelse(is.infinite(delta), "unbounded", paste0("bounded ", delta, " s"))))]
  p <- ggplot(cop[!is.na(Group)], aes(exp_short_same, same, colour = Group)) + geom_abline(slope = 1, intercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_point(size = 0.7, alpha = 0.7) + scale_colour_manual(values = GCOL) + facet_grid(window ~ combo) + coord_equal() + th +
    labs(x = "expected same-reader share (time-shift null, +-5-60 min)", y = "observed same-reader share",
         title = "Same-reader co-assignment against its chance expectation (one point per animal-window)")
  sv(p, "F8_coassignment_vs_expectation", w = 8, h = 5.5)

  # F9 CON minus SIS within batch, crossing rate by method (CC1 active)
  dvc <- merge(dv[window == "active" & CC == "CC1", .(version, AnimalNum, crossing_rate, SourceFile, System)], grp, by = "AnimalNum")
  dvc <- dvc[, .(v = mean(crossing_rate), Group = Group[1], Sex = Sex[1]), by = .(version, SourceFile, System)]
  amc <- wm[window == "active" & CC == "CC1" & is.infinite(delta) & !is.na(Group) & (basis == "event_start" & tau %in% c(0, 2, 10, 20) | basis == "read" & tau == 2),
            .(v = mean(crossing_rate), Group = Group[1], Sex = Sex[1]),
            by = .(version = fifelse(basis == "read", "raw reads tau 2 s", paste0("raw tau ", tau, " s")), SourceFile, System)]
  allc <- rbind(dvc[version %in% c("vendor", "ge200", "avg500")], amc)
  allc[, Batch := sub("^E9_SIS_(B[0-9]+)_.*$", "\\1", SourceFile)]
  wc <- allc[, .(con = mean(v[Group == "CON"]), sis = mean(v[Group != "CON"])), by = .(version, Batch, Sex)][, log_ratio := log2(con / sis)]
  wc[, version := factor(version, levels = c("vendor", "ge200", "avg500", "raw tau 0 s", "raw tau 2 s", "raw tau 10 s", "raw tau 20 s", "raw reads tau 2 s"))]
  wsum(wc, "05_con_vs_sis_crossing_by_method_cc1")
  p <- ggplot(wc, aes(version, log_ratio, colour = Sex, group = interaction(Batch, Sex))) + geom_hline(yintercept = 0, colour = "grey50") +
    geom_line(linewidth = 0.3, alpha = 0.6) + geom_point(size = 1.8) + geom_text(aes(label = Batch), size = 2.3, nudge_x = 0.25, show.legend = FALSE) +
    scale_colour_manual(values = MCOL[c(5, 1)]) + th + theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = NULL, y = "log2(CON cage mean / SIS cage mean), within batch", title = "CON vs SIS position-change rate per batch, CC1 first active phase (3 CON cages per sex)")
  sv(p, "F9_con_vs_sis_by_method", w = 7, h = 4.5)

  # F1 raw detections and reconstructions, 10 min of one SIS cage (B3 CC1, 20:00-20:10 of the first night; the first SIS
  # board and its two lowest animal IDs: a fixed choice, not a selected example)
  suppressPackageStartupMessages({ library(dplyr); library(tibble) })
  RAW_DIR <- file.path(dirname(dirname(dirname(LIVE))), "Raw Data", "Behavior", "RFID", "MMMdata")
  tags <- as.data.table(rfid_animalpos_tag_map(file.path(LIVE, "MMMSociability/raw_data/B3/E9_SIS_B3_CC1_AnimalPos.csv")))
  ev <- as.data.table(rfid_read_recording(RAW_DIR, "B3", "CC1")$events)[, e := t + dur_sec][, fo := .I]
  ev <- merge(ev, tags, by = "tag")
  setorder(ev, t, fo)
  bmap <- ev[, .N, by = .(AnimalNum, board)][order(AnimalNum, -N)][, .SD[1], by = AnimalNum]
  bmap <- merge(bmap, grp, by = "AnimalNum")
  b_sis <- min(bmap[Group != "CON"]$board)
  ex_ids <- sort(bmap[board == b_sis]$AnimalNum)[1:2]
  W0 <- as.numeric(as.POSIXct("2023-04-24 20:00:00", tz = "UTC")); W1 <- W0 + 600
  vend <- fread(file.path(LIVE, "MMMSociability/data_versions/v2_cage_label_correction_2026-09-28/preprocessed_data/E9_SIS_B3_CC1_AnimalPos_preprocessed.csv"),
                select = c("DateTime", "AnimalID", "PositionID"), colClasses = c(AnimalID = "character"))
  vend[, `:=`(t = as.numeric(as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")), AnimalNum = canonical_animal_id(AnimalID))]
  ylev <- c(1, 5, 2, 6, 3, 7, 4, 8)                       # by column, lower row first
  ylab <- sprintf("C%d %s (R%d)", c(1, 1, 2, 2, 3, 3, 4, 4), rep(c("low", "up"), 4), ylev)
  yof <- function(p) match(p, ylev)
  segs <- list(); steps <- list(); unc <- list()
  for (a in ex_ids) {
    x <- ev[AnimalNum == a & board == b_sis]
    segs[[a]] <- x[t < W1 & e > W0, .(AnimalNum = a, s = (pmax(t, W0) - W0) / 60, e = (pmin(e, W1) - W0) / 60, y = yof(PositionID))]
    for (m in list(list("raw carry-forward (tau 0)", 0, -0.22), list("flip filter tau 2 s", 2, 0), list("flip filter tau 10 s", 10, 0.22))) {
      r <- amv_runs(x$t, x$PositionID, W0, W1, m[[2]])
      steps[[paste(a, m[[1]])]] <- r[, .(AnimalNum = a, method = m[[1]], s = s / 60, e = e / 60, y = yof(pos) + m[[3]])]
    }
    v <- vend[AnimalNum == a][order(t)]
    hold <- v[t <= W0][.N]; vin <- v[t > W0 & t < W1]
    vv <- rbind(hold[, .(t = W0, PositionID)], vin[, .(t, PositionID)])
    steps[[paste(a, "vendor")]] <- vv[, .(AnimalNum = a, method = "vendor AnimalPos", s = (t - W0) / 60, e = (c(t[-1], W1) - W0) / 60, y = yof(PositionID) + 0.4)]
    g <- amv_gaps(amv_union(x$t, x$e), W0, W1, 10)
    unc[[a]] <- g[, .(AnimalNum = a, s = (s + 10 - W0) / 60, e = (e - W0) / 60)]
  }
  segs <- rbindlist(segs); steps <- rbindlist(steps); unc <- rbindlist(unc)
  steps[, method := factor(method, levels = c("raw carry-forward (tau 0)", "flip filter tau 2 s", "flip filter tau 10 s", "vendor AnimalPos"))]
  p <- ggplot() +
    geom_rect(data = unc, aes(xmin = s, xmax = e, ymin = 0.4, ymax = 8.6), fill = "grey90") +
    geom_segment(data = steps, aes(x = s, xend = e, y = y, yend = y, colour = method), linewidth = 0.6) +
    geom_segment(data = segs, aes(x = s, xend = pmax(e, s + 0.004), y = y, yend = y), linewidth = 2.2, colour = "black") +
    scale_y_continuous(breaks = 1:8, labels = ylab) + scale_colour_manual(values = MCOL[c(1, 2, 3, 7)], name = NULL) +
    facet_wrap(~AnimalNum, ncol = 1) + th +
    labs(x = "minutes after 20:00, B3 CC1 first night", y = NULL,
         title = "Raw detections (black) and reconstructed positions (coloured, offset per method)",
         subtitle = "grey: no detection for more than 10 s (carried position 'uncertain' under a 10-s bound)")
  sv(p, "F1_raw_vs_reconstructed_example", w = 9, h = 6.5)
  fwrite(rbind(segs[, .(AnimalNum, layer = "detection", s, e, y)], steps[, .(AnimalNum, layer = as.character(method), s, e, y)]), file.path(SUM, "F1_example_source_data.csv"))
  message("figures written to ", FIG)
}
