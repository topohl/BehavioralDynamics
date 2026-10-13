# Stage 00b -- raw RFID acquisition QC (manual diagnostic; not part of the runner).
# Reads the raw MultiMouseMonitor logs, the v2 preprocessed files (roster, phase calendar), the vendor AnimalPos
# exports (tag map) and the placement scans. Writes a new run-id folder only: <root>/runs/<run_id>/, built in
# <root>/runs/.tmp_<run_id>/ and renamed at the end. <root> is analysis_ready/quality_control/raw_acquisition/ of the
# project root (a clean, committed code tree is then required) or an isolated --output-root.
#   Rscript Analysis/00b_qc_raw_acquisition.R --run-id=raqc_YYYYMMDD
#   Rscript Analysis/00b_qc_raw_acquisition.R --run-id=dev01 --output-root=<absolute folder> [--recordings=B2_CC[34]] [--resume]
# Group-blind (no group, CombZ or outcome is read). Flags are review items and proposed exclusions, never applied;
# the frozen S13 list is unchanged. Rules, thresholds and their basis: aqc_rules() in Functions/antenna_acquisition_qc.R;
# documentation: docs/RFID_ACQUISITION_QC.md.

source("Analysis/_pipeline_setup.R")
for (helper in c("project_paths.R", "tracking_qc_run_config.R", "manuscript_palette.R", "animalpos_preprocessing_helpers.R", "antenna_log_helpers.R",
                 "animalpos_reconstruction.R", "antenna_data_version.R", "antenna_measurement.R", "antenna_acquisition_qc.R")) {
  source_mmm_helper(helper)
}

RAQC_CODE <- c("Analysis/00b_qc_raw_acquisition.R", "Functions/antenna_acquisition_qc.R", "Functions/antenna_measurement.R",
               "Functions/antenna_log_helpers.R", "Functions/antenna_data_version.R", "Functions/animalpos_reconstruction.R",
               "Functions/animalpos_preprocessing_helpers.R", "Functions/project_paths.R", "Functions/tracking_qc_run_config.R",
               "Analysis/_pipeline_setup.R", "Functions/behavioral_dynamics_helpers.R", "Functions/manuscript_palette.R")
RAQC_TABLES <- c("recording_qc", "board_qc", "reader_qc", "reader_block_qc", "stretches", "animal_qc", "animal_block_qc",
                 "animal_reader_block", "basis_crosstab", "coassign_by_reader", "nonroster_tags", "silences", "system_messages", "blocks")

raqc_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

raqc_code_hashes <- function() {
  f <- file.path(MMM_REPO_ROOT, RAQC_CODE)
  data.frame(file = RAQC_CODE, sha256 = mmm_file_sha256(f),
             blob = vapply(RAQC_CODE, function(x) { b <- raqc_git(c("rev-parse", paste0("HEAD:", x))); if (length(b) == 1 && grepl("^[0-9a-f]{40}$", b)) b else NA_character_ }, ""),
             stringsAsFactors = FALSE)
}

# One recording: the QC tables, written to <tmp>/rec/<id>/ with a DONE marker naming the code hashes.
raqc_recording <- function(vf, cfg, tag_dir, marker, tmp) {
  id <- sub("^E9_SIS_(B[0-9]+)_(CC[0-9]+)_.*$", "\\1_\\2", basename(vf))
  batch <- sub("_.*$", "", id); cc <- sub("^.*_", "", id)
  d <- file.path(tmp, "rec", id)
  done <- file.path(d, "DONE.txt")
  if (file.exists(done) && identical(readLines(done), marker)) { message(id, ": done, kept"); return(invisible(id)) }
  t0 <- Sys.time()
  vendor <- data.table::fread(vf, colClasses = "character")
  tags <- rfid_animalpos_tag_map(file.path(tag_dir, batch, sprintf("E9_SIS_%s_%s_AnimalPos.csv", batch, cc)))
  rec <- rfid_read_recording(cfg$raw_root, batch, cc, keep_read_offsets = TRUE)
  sm <- amv_system_messages(rec$path)
  b <- as.integer(sub("B", "", batch)); k <- as.integer(sub("CC", "", cc))
  scan <- function(kk) list.files(file.path(cfg$raw_root, batch, "raw"), pattern = sprintf("Batch%d_change_%d_.*initScan[.]csv$", b, kk), full.names = TRUE)
  before <- scan(k); after <- if (k < 4) scan(k + 1L) else character()
  ib <- aqc_initscan_antenna_reads(before); ia <- aqc_initscan_antenna_reads(after)
  initscan <- merge(ib[, .(board, PositionID, reads_before = if (length(before)) reads else NA_integer_)],
                    ia[, .(board, PositionID, reads_after = if (length(after)) reads else NA_integer_)], by = c("board", "PositionID"))
  initb <- data.table::rbindlist(lapply(before, antenna_dv_initscan_boards))
  if (nrow(initb)) initb <- initb[order(tag, -initscan_reads)][, .SD[1], by = tag]
  res <- aqc_process_recording(vendor, rec, tags, sm, initscan, initb, batch, cc, basename(vf))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(res)) data.table::fwrite(res[[nm]], file.path(d, paste0(nm, ".csv")))
  writeLines(marker, done)
  message(sprintf("%s: %.1f min", id, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  invisible(id)
}

# Group-blind summaries for the report (no group, CombZ or outcome is read).
raqc_summaries <- function(tab, ev) {
  L <- data.table::as.data.table(AQC_LAYOUT)[, .(PositionID, reader, column, resource)]
  dead <- ev$flags[check_id == "H06", .(SourceFile, board)]
  live <- function(d) d[!paste(SourceFile, board) %in% paste(dead$SourceFile, dead$board)]
  arb <- live(tab$animal_reader_block)
  rs <- arb[, .(locf_event_s = sum(locf_event_s), supp300 = sum(supp300_event_s), supp60 = sum(supp60_event_s), supp10 = sum(supp10_event_s),
                locf_read_s = sum(locf_read_s), reads = sum(reads)), by = .(Phase, PositionID)]
  rs[, `:=`(locf_share = locf_event_s / sum(locf_event_s), read_share = reads / sum(reads)), by = Phase]
  rs[, `:=`(supported_le300_share = supp300 / locf_event_s, supported_le60_share = supp60 / locf_event_s, supported_le10_share = supp10 / locf_event_s,
            reads_per_locf_s = reads / locf_event_s)]
  reader_summary <- merge(rs, L, by = "PositionID")[order(Phase, PositionID)]
  pb <- arb[, .(locf = sum(locf_event_s), supp60 = sum(supp60_event_s), reads = sum(reads)), by = .(SourceFile, board, Phase, PositionID)]
  pb[, `:=`(supported_le60_share = supp60 / locf, reads_per_locf_s = reads / locf)]
  reader_spread <- pb[locf > 600, .(board_recordings = .N, supported_le60_median = stats::median(supported_le60_share),
                                    supported_le60_q25 = stats::quantile(supported_le60_share, 0.25), supported_le60_q75 = stats::quantile(supported_le60_share, 0.75),
                                    reads_per_locf_s_median = stats::median(reads_per_locf_s)), by = .(Phase, PositionID)]
  reader_spread <- merge(reader_spread, L, by = "PositionID")[order(Phase, PositionID)]
  # column occupancy under carry-forward bounds (event basis) and on the read basis
  arb[, column := L$column[PositionID]]
  col <- arb[, .(locf_event = sum(locf_event_s), le300 = sum(supp300_event_s), le60 = sum(supp60_event_s), le10 = sum(supp10_event_s),
                 locf_read = sum(locf_read_s), read_le60 = sum(supp60_read_s)), by = .(Phase, column)]
  col_sens <- data.table::melt(col, id.vars = c("Phase", "column"), variable.name = "reconstruction", value.name = "seconds")
  col_sens[, share := seconds / sum(seconds), by = .(Phase, reconstruction)]
  # event-start vs single-read basis: reassignment by event reader, support age, phase; and where it comes from
  bx <- tab$basis_crosstab
  basis_reader <- bx[, .(seconds = sum(seconds), reassigned = sum(seconds[read_pos != event_pos]),
                         other_column = sum(seconds[L$column[read_pos] != L$column[event_pos]])), by = .(Phase, event_pos, age_class)]
  basis_reader[, `:=`(reassigned_share = reassigned / seconds, other_column_share = other_column / seconds)]
  basis_reader <- merge(basis_reader, L[, .(event_pos = PositionID, reader, resource)], by = "event_pos")[order(Phase, event_pos, age_class)]
  basis_recording <- bx[, .(seconds = sum(seconds), reassigned = sum(seconds[read_pos != event_pos])), by = .(SourceFile, board, Phase)]
  basis_recording[, `:=`(reassigned_share = reassigned / seconds, share_of_all_reassigned = reassigned / sum(reassigned)), by = Phase]
  basis_flow <- bx[read_pos != event_pos, .(seconds = sum(seconds)), by = .(Phase, event_pos, read_pos)][order(Phase, -seconds)]
  basis_flow[, share := seconds / sum(seconds), by = Phase]
  # same-reader co-assignment by reader
  co <- live(tab$coassign_by_reader)[, .(observed = sum(observed), expected = sum(expected), pair_seconds = sum(pair_seconds)), by = .(Phase, PositionID, seconds)]
  co[, obs_exp := observed / expected]
  co <- merge(co, L, by = "PositionID")[order(Phase, seconds, PositionID)]
  # changes before and after switching correction, per hour (animal-block medians)
  ab <- tab$animal_block_qc
  sw <- ab[, .(animal_blocks = .N, raw_events_per_h = stats::median(changes_raw / (block_s / 3600)),
               event_tau2_per_h = stats::median(changes_event_tau2 / (block_s / 3600)),
               read_tau0_per_h = stats::median(changes_read_tau0 / (block_s / 3600)), read_tau2_per_h = stats::median(changes_read_tau2 / (block_s / 3600)),
               returns_lt2s_share = sum(returns_lt2s) / sum(changes_raw)), by = Phase]
  # completeness of spatial measures per board-recording
  comp <- ab[, .(animal_seconds = sum(block_s), supported_le60 = sum((1 - age_gt60s_share) * block_s), supported_le300 = sum((1 - age_gt300s_share) * block_s)),
             by = .(SourceFile, Batch, CC, board, Phase)]
  comp[, `:=`(supported_le60_share = supported_le60 / animal_seconds, supported_le300_share = supported_le300 / animal_seconds)]
  comp[, dead_channel := paste(SourceFile, board) %in% paste(dead$SourceFile, dead$board)]
  tail <- ev$flags[check_id == "W07", .(SourceFile, board, unobserved_after = value)]
  comp <- merge(comp, tail, by = c("SourceFile", "board"), all.x = TRUE)
  list(reader_summary = reader_summary, reader_spread = reader_spread, column_sensitivity = col_sens, basis_by_reader = basis_reader,
       basis_by_recording = basis_recording, basis_flow = basis_flow, coassign_by_reader_summary = co, switching_summary = sw,
       spatial_completeness = comp)
}

raqc_figures <- function(tab, ev, sm, fig_dir) {
  L <- AQC_LAYOUT
  th <- ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "top",
                                                            strip.text = ggplot2::element_text(face = "bold"))
  sv <- function(p, n, w = 7, h = 4.5) ggplot2::ggsave(file.path(fig_dir, paste0(n, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
  lab <- function(p) ifelse(nzchar(L$resource[p]), paste0(L$reader[p], "\n", L$resource[p], "*"), L$reader[p])
  # 1. layout
  lay <- data.frame(L, label = ifelse(nzchar(L$resource), paste0(L$reader, "\n", L$resource, "*"), L$reader))
  p <- ggplot2::ggplot(lay, ggplot2::aes(x, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = nzchar(resource)), width = 92, height = 106, colour = "grey40") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.2, lineheight = 0.9) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = "grey95", `TRUE` = "#cde2fb"), guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(0, 100, 200, 300), labels = paste0("C", 1:4, "\nx = ", c(0, 100, 200, 300))) +
    ggplot2::scale_y_continuous(breaks = c(0, 116), labels = c("y = 0", "y = 116")) + ggplot2::coord_equal() + th +
    ggplot2::labs(x = NULL, y = NULL, title = "Antenna layout of every board (4 x 2 grid, grid units)",
                  caption = paste("* lab-reported resource. House end: light-phase resting at C1 in 120/120 cage x CC (R5 top in 110).",
                                  "Food (R1) and water (R8) rows inferred from dark-phase visits; no record of the physical orientation exists.", sep = "\n"))
  sv(p, "qc01_layout", 6, 3.6)
  # 2. reader health: session co-read ratio per board-recording and reader
  q <- data.table::copy(tab$reader_qc)
  q[, rec := paste(Batch, CC)]
  q[, ratio := pmin(pmax(crosstalk_in_ratio, 1 / 64), 64)]
  q[, reader_lab := factor(lab(PositionID), levels = lab(c(5:8, 1:4)))]
  p <- ggplot2::ggplot(q, ggplot2::aes(reader_lab, rec, fill = log2(ratio))) + ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_point(data = q[zero_read_session == TRUE], shape = 4, size = 2.5, stroke = 1) +
    ggplot2::scale_fill_gradient2(low = MMM_PALETTE_DIVERGING[["low"]], mid = "white", high = MMM_PALETTE_DIVERGING[["high"]], midpoint = 0, name = "log2 co-reads observed / expected") +
    ggplot2::facet_wrap(~ paste("board", board), nrow = 1) + th +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6), axis.text.y = ggplot2::element_text(size = 6)) +
    ggplot2::labs(x = NULL, y = NULL, title = "Reader health: co-reads at each reader against its homologous expectation (whole session)",
                  subtitle = "x = no read in the session (H06). Expectations come from the same reader pair on the other boards; occupancy is not assumed uniform.")
  sv(p, "qc02_reader_health", 11, 6.5)
  # 3. board signatures across recordings
  bh <- data.table::copy(ev$board_history)
  bh[, rec := paste(Batch, CC)]
  p <- ggplot2::ggplot(bh, ggplot2::aes(factor(board), mean_gap_ms)) + ggplot2::geom_boxplot(outlier.shape = NA, fill = "grey92") +
    ggplot2::geom_jitter(width = 0.15, size = 0.8) + th +
    ggplot2::labs(x = "board", y = "mean within-event read gap (ms)", title = "Board acquisition signature across the 24 recordings",
                  subtitle = "isolated events only (no other event on the board); stable per board, i.e. hardware")
  sv(p, "qc03_board_signature", 5, 4)
  # 4. support of the carried-forward position by reader
  rs <- data.table::copy(sm$reader_spread)
  rs[, reader_lab := factor(lab(PositionID), levels = lab(1:8))]
  p <- ggplot2::ggplot(rs, ggplot2::aes(reader_lab, supported_le60_median)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = supported_le60_q25, ymax = supported_le60_q75), size = 0.3) +
    ggplot2::facet_wrap(~Phase) + ggplot2::coord_cartesian(ylim = c(0, 1)) + th +
    ggplot2::labs(x = NULL, y = "share of carry-forward time with a read <= 60 s old", title = "How much assigned time at each reader rests on recent reads",
                  subtitle = "median and IQR over board-recordings; dead-channel boards left out")
  sv(p, "qc04_support_by_reader", 8, 4)
  # 5. column occupancy under carry-forward bounds
  cs <- data.table::copy(sm$column_sensitivity)
  cs[, reconstruction := factor(reconstruction, levels = c("locf_event", "le300", "le60", "le10", "locf_read", "read_le60"),
                                labels = c("events, unbounded", "events, <= 300 s", "events, <= 60 s", "events, <= 10 s", "reads, unbounded", "reads, <= 60 s"))]
  p <- ggplot2::ggplot(cs, ggplot2::aes(factor(column), share, colour = reconstruction, group = reconstruction)) + ggplot2::geom_line() + ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_colour_manual(values = c("#0d366b", "#256abf", "#5598e7", "#9ec5f4", "#eb6834", "#f2a07b"), name = NULL) +
    ggplot2::facet_wrap(~Phase) + th + ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::labs(x = "column (C1 = house end ... C4 = water end)", y = "share of counted time", title = "Column occupancy against the age of the supporting read")
  sv(p, "qc05_column_sensitivity", 8, 4.2)
  # 6. event-start vs single-read basis by reader and support age
  br <- data.table::copy(sm$basis_by_reader)
  br[, reader_lab := factor(lab(event_pos), levels = lab(1:8))]
  br[, age_class := factor(age_class, levels = levels(aqc_age_class(numeric())), labels = c("<= 2", "2-10", "10-60", "60-300", "> 300"))]
  p <- ggplot2::ggplot(br, ggplot2::aes(reader_lab, reassigned_share, fill = age_class)) + ggplot2::geom_col(position = "dodge", width = 0.8) +
    ggplot2::scale_fill_manual(values = c("#cde2fb", "#9ec5f4", "#5598e7", "#256abf", "#0d366b"), name = "time since the last read (s)") +
    ggplot2::facet_wrap(~Phase) + th +
    ggplot2::labs(x = "reader assigned by event-start carry-forward", y = "share of its seconds assigned elsewhere by single-read carry-forward",
                  title = "Where event-start and single-read reconstruction disagree (2-s flip filter on both)")
  sv(p, "qc06_basis_by_reader", 9, 4.5)
  # 7. same-reader co-assignment observed vs expected by reader
  co <- data.table::copy(sm$coassign_by_reader_summary)
  co[, reader_lab := factor(lab(PositionID), levels = lab(1:8))]
  p <- ggplot2::ggplot(co[expected > 3600], ggplot2::aes(reader_lab, obs_exp, colour = seconds, group = seconds)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50") + ggplot2::geom_point(size = 2) + ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = c(all = "#0d366b", supported_le60s = "#eb6834"), name = NULL) + ggplot2::facet_wrap(~Phase) + th +
    ggplot2::labs(x = NULL, y = "observed / expected same-reader pair-seconds", title = "Same-reader co-assignment against independence, by reader",
                  subtitle = "diagnostic only: co-assignment and read competition influence each other; not evidence of avoidance")
  sv(p, "qc07_coassign_by_reader", 8, 4)
  # 8. silent stretches: expected co-read minutes, with the whole-session dropouts for comparison
  st <- data.table::copy(tab$stretches)
  if (nrow(st)) {
    dd <- tab$reader_qc[zero_read_session == TRUE]
    p <- ggplot2::ggplot(st, ggplot2::aes(pmax(E_active_minutes, 0.01))) + ggplot2::geom_histogram(bins = 60, fill = "grey60") +
      ggplot2::geom_vline(xintercept = AQC_STRETCH_WARN_E, linetype = "dashed") +
      ggplot2::geom_vline(data = dd, ggplot2::aes(xintercept = E_session), colour = "#d03b3b") +
      ggplot2::scale_x_log10() + th +
      ggplot2::labs(x = "expected active minutes with a co-read (log scale); red: expected co-reads of the whole-session dropouts", y = "silent stretches >= 60 min",
                    title = "Intermittent-failure scan: silent stretches against neighbour traffic", subtitle = "dashed: W08 warning threshold")
    sv(p, "qc08_silent_stretches", 7, 4)
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble); library(ggplot2) })
  data.table::setDTthreads(1L)
  project_root <- mmm_project_root()
  cfg <- mmm_raw_acquisition_qc_run_config(project_root, commandArgs(trailingOnly = TRUE))
  if (cfg$require_clean_code) {
    dirty <- raqc_git(c("status", "--porcelain", "--untracked-files=all", "--", RAQC_CODE))
    if (!is.null(attr(dirty, "status")) || length(dirty)) stop("A run into the project root needs committed code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  }
  vendor_dir <- file.path(project_root, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "preprocessed_data")
  tag_dir <- file.path(project_root, "MMMSociability", "raw_data")
  for (d in c(vendor_dir, tag_dir, cfg$raw_root)) if (!dir.exists(d)) stop("Missing input folder: ", d, call. = FALSE)
  vendor_files <- sort(list.files(vendor_dir, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$", full.names = TRUE))
  if (length(vendor_files) != 24) stop("Expected 24 v2 preprocessed files, found ", length(vendor_files), call. = FALSE)
  ids <- sub("^E9_SIS_(B[0-9]+)_(CC[0-9]+)_.*$", "\\1_\\2", basename(vendor_files))
  if (!is.na(cfg$recordings)) { keep <- grepl(cfg$recordings, ids); vendor_files <- vendor_files[keep]; ids <- ids[keep] }
  if (!length(ids)) stop("--recordings matches no recording.", call. = FALSE)
  planned <- c(file.path(cfg$tmp_dir, "rec", ids, "animal_reader_block.csv"),
               file.path(cfg$output_dir, "rec", ids, "animal_reader_block.csv"),
               file.path(cfg$output_dir, "manifest", "output_files_sha256.csv"), file.path(cfg$output_dir, "tables", "coassign_by_reader_summary.csv"))
  mmm_assert_output_path_budget(planned, source_label = "Stage 00b")
  dir.create(cfg$tmp_dir, recursive = TRUE, showWarnings = FALSE)
  code <- raqc_code_hashes()
  marker <- paste(code$sha256, code$file)
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (vf in vendor_files) raqc_recording(vf, cfg, tag_dir, marker, cfg$tmp_dir)

  tab <- stats::setNames(lapply(RAQC_TABLES, function(nm) data.table::rbindlist(lapply(ids, function(id) {
    suppressWarnings(data.table::fread(file.path(cfg$tmp_dir, "rec", id, paste0(nm, ".csv")),
                                       colClasses = c(AnimalNum = "character", tag = "character")))
  }), use.names = TRUE, fill = TRUE)), RAQC_TABLES)
  for (nm in c("animal_qc", "animal_block_qc", "animal_reader_block", "basis_crosstab"))
    if ("AnimalNum" %in% names(tab[[nm]])) tab[[nm]][, AnimalNum := as.character(AnimalNum)]
  ev <- aqc_evaluate(tab)
  sm <- raqc_summaries(tab, ev)
  out_tab <- file.path(cfg$tmp_dir, "tables"); out_man <- file.path(cfg$tmp_dir, "manifest"); out_fig <- file.path(cfg$tmp_dir, "figures", "qc")
  for (d in c(out_tab, out_man, out_fig)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (nm in RAQC_TABLES) data.table::fwrite(tab[[nm]], file.path(out_tab, paste0(nm, ".csv")))
  for (nm in names(sm)) data.table::fwrite(sm[[nm]], file.path(out_tab, paste0(nm, ".csv")))
  data.table::fwrite(ev$board_history, file.path(out_tab, "board_history.csv"))
  data.table::fwrite(ev$flags, file.path(out_man, "qc_flags.csv"))
  data.table::fwrite(ev$manifest, file.path(out_man, "qc_manifest.csv"))
  data.table::fwrite(ev$exclusions, file.path(out_man, "qc_proposed_exclusions.csv"))
  data.table::fwrite(aqc_rules(), file.path(out_man, "qc_rules.csv"))
  data.table::fwrite(code, file.path(out_man, "code_hashes.csv"))
  raqc_figures(tab, ev, sm, out_fig)
  batches <- sub("_CC[0-9]+$", "", ids)
  inputs <- c(vendor_files, file.path(tag_dir, batches, paste0("E9_SIS_", ids, "_AnimalPos.csv")),
              file.path(cfg$raw_root, sub("_CC[0-9]+$", "", tab$recording_qc$Batch), "raw", tab$recording_qc$raw_log))
  inputs <- unique(inputs[file.exists(inputs)])
  data.table::fwrite(data.frame(input = inputs, sha256 = mmm_file_sha256(inputs), bytes = file.size(inputs)), file.path(out_man, "input_hashes.csv"))
  status <- if (any(ev$manifest$severity == "hard_failure" & ev$manifest$status == "flagged")) "FAIL" else
    if (any(ev$manifest$status == "flagged")) "WARN" else "PASS"
  head_commit <- raqc_git(c("rev-parse", "HEAD"))[1]
  run <- data.frame(item = c("run_id", "qc_status", "rules_version", "git_commit", "code_clean", "isolated", "recordings", "project_root", "raw_root",
                             "data_version", "started", "finished", "R"),
                    value = c(cfg$run_id, status, AQC_RULES_VERSION, head_commit,
                              as.character(!length(raqc_git(c("status", "--porcelain", "--untracked-files=all", "--", RAQC_CODE)))),
                              as.character(cfg$isolated), paste(ids, collapse = " "), project_root, cfg$raw_root,
                              "v2_cage_label_correction_2026-09-28", started, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string))
  data.table::fwrite(run, file.path(out_man, "qc_run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(out_man, "session_info.txt"))
  counts <- ev$manifest[, .(checks = .N, flagged = sum(status == "flagged"), units_flagged = sum(n_flagged)), by = severity]
  writeLines(c("PROVISIONAL RAW RFID ACQUISITION QC (Stage 00b; manual diagnostic)", "",
               paste("run:", cfg$run_id, "| status:", status, "| rules:", AQC_RULES_VERSION, "| commit:", head_commit),
               paste("recordings:", length(ids)), "",
               capture.output(print(counts)), "",
               "Hard failures are demonstrable acquisition defects; warnings are review items; diagnostics are continuous values.",
               "Flags and proposed exclusions are never applied by this stage; the frozen S13 list is unchanged.",
               "No group label, CombZ or outcome was read.",
               "Files: manifest/qc_manifest.csv (one row per check), manifest/qc_flags.csv (one row per flagged unit),",
               "manifest/qc_proposed_exclusions.csv, manifest/qc_rules.csv, tables/ (QC tables and summaries), figures/qc/.",
               "Documentation: docs/RFID_ACQUISITION_QC.md in the MMMSociability repository."), file.path(cfg$tmp_dir, "README.txt"))
  outs <- list.files(cfg$tmp_dir, recursive = TRUE)
  outs <- outs[!startsWith(outs, "rec/")]
  data.table::fwrite(data.frame(file = outs, sha256 = mmm_file_sha256(file.path(cfg$tmp_dir, outs)), bytes = file.size(file.path(cfg$tmp_dir, outs))),
                     file.path(out_man, "output_files_sha256.csv"))
  if (!file.rename(cfg$tmp_dir, cfg$output_dir)) stop("Could not rename ", cfg$tmp_dir, " to ", cfg$output_dir, call. = FALSE)
  message("Stage 00b ", status, ": ", cfg$output_dir)
}
