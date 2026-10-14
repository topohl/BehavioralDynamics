# ================================================================
# Stage 19 candidate (s19c): group-blind column and reader occupancy from one single-read reconstruction release
# MMMSociability -- Functions/spatial_occupancy_candidate.R
# ================================================================
# Used by Testing/audits/build_spatial_occupancy_candidate.R (UNREGISTERED; isolated output root; Stage 19, its outputs
# and the manuscript figures are not touched). Pure definitions: sourcing this file reads no data and writes no files.
# Needs data.table and, at call time, asr_shares_with_unknown() (Functions/antenna_spatial_reconstruction.R) and
# amv_column() (Functions/antenna_measurement.R, with RFID_GRID_X / RFID_GRID_Y from Functions/antenna_log_helpers.R).
#
# Input: the per-animal tables of one srs release (occupancy, animal_block, animals, blocks, censored, read_agreement,
# gaps, colocation): seconds per animal, 12-h phase block, reader and basis under unrestricted carry-forward (LOCF) and
# with an observation age <= 60 s / <= 300 s, and the observed time of every animal-block. Nothing is reconstructed again.
# Representation (specification v1.1, S3 and S4):
#   * four columns primary (C1 = R1 food + R5 house, C2 = R2 + R6, C3 = R3 + R7, C4 = R4 + R8 water); the eight readers
#     are diagnostics; time without support is "unknown" and is never redistributed;
#   * share_all = seconds / observed time (units plus unknown sum to 1); share_supported = seconds / supported time
#     (units sum to 1; NA when nothing is supported); both are always reported;
#   * LOCF (unknown only before an animal's first read) is the historical reference; max300s and max60s are sensitivity;
#   * "supported" means recently detected (a read <= d s old), not a validated location; column or reader occupancy is
#     not resource engagement and not proximity;
#   * Phase = Active | Light (the vendor label Active | Inactive is kept as Phase_vendor); the two are never pooled;
#   * block sets (decision of 2026-10-15): stage19_window (Stage 19's blocks; CC4 keeps A1, I2 and A2 only) is the primary
#     window; cc4_grid_period (the CC4 blocks after it, I3 onward: the grid-exposure period) is reported separately and
#     never pooled with it. The two sets are disjoint, so every per-block row belongs to exactly one. Whole-span pooling
#     is computed only in memory, to check against the srs release tables (gates G12, G12b, G13).
# Shares over several blocks or animals are pooled by seconds, never averaged; an equal-weight animal mean is given
# beside the pooled share where a table says so. Exclusions arrive as columns (spatial_excluded, censor_from): excluded
# cage episodes stay in the animal tables, flagged, and are left out of every summary and figure input. No group label,
# no model and no test statistic.
# ================================================================

SOC_VERSION <- "s19c_v0.2"
SOC_BOUNDS <- c("LOCF", "max300s", "max60s")
SOC_SUPPORT_CRITERION <- c(LOCF = "none (unrestricted LOCF)", max300s = "observation age <= 300 s",
                           max60s = "observation age <= 60 s")
SOC_BASIS_LABEL <- c(event = "event-start (historical reference)", read = "single-read (provisional candidate)",
                     both = "observation age is basis-independent (equal in both bases; gate)",
                     `read - event` = "single-read minus event-start")
SOC_BLOCK_SETS <- c("stage19_window", "cc4_grid_period")   # disjoint; never pooled together (whole_span: gates only)
SOC_SEX_BY_BATCH <- c(B1 = "Male", B2 = "Male", B5 = "Male", B3 = "Female", B4 = "Female", B6 = "Female")
SOC_READER_RESOURCE <- c("food", "", "", "", "house", "", "", "water")   # lab-reported (Stage 00b AQC_LAYOUT)
SOC_DENOMINATOR <- c(share_all = "observed time (share_all)", share_supported = "supported time (share_supported)")
SOC_UNKNOWN_NOTE <- "unknown: no read within the bound (LOCF: before the first read); not redistributed"
SOC_INTERPRETATION <- paste("column or reader occupancy is not resource engagement, proximity or a validated location;",
                            "supported = recently detected (a read <= d s old), not validated")
# Column names that would carry a group label or a group-defining design key; none may appear in an output.
SOC_FORBIDDEN_COLUMNS <- c("Group", "group", "ReferenceGroup", "CombZ", "fixed_cage", "SUS", "RES", "CON", "Condition")
SOC_LEVELS <- list(block_set = SOC_BLOCK_SETS, Phase = c("Active", "Light"), basis = c("event", "read"), bound = SOC_BOUNDS,
                   unit = c(paste0("C", 1:4), paste0("R", 1:8), "unknown"), last_reader = c(paste0("R", 1:8), "none"),
                   denominator = unname(SOC_DENOMINATOR), resolution = c("column", "reader"))
# Leading metadata columns of every output table, in this order (those a table has).
SOC_META_ORDER <- c("run_id", "srs_run_id", "reconstruction", "tau_s", "basis", "basis_label", "bound", "support_criterion",
                    "denominator", "block_set", "level", "window", "SourceFile", "raw_log", "Batch", "Sex", "CC", "board", "System",
                    "CageEpisodeID", "AnimalNum", "tag", "n_animals_present", "n_tags_present", "n_tracked", "spatial_excluded",
                    "censor_from")

# ---------------------------------------------------------------- layout, labels, keys

#' Reader layout: PositionID, reader (R1-R8), column (amv_column()), unit C1-C4 of that column (column_unit), the
#' lab-reported resource (R1 food, R5 house, R8 water), reader_label ("R1 food") and map coordinates ReaderX, ReaderY.
soc_reader_layout <- function() {
  L <- data.table::data.table(PositionID = 1:8, reader = paste0("R", 1:8), column = amv_column(1:8), resource = SOC_READER_RESOURCE,
                              ReaderX = RFID_GRID_X[1:8], ReaderY = RFID_GRID_Y[1:8])
  L[, `:=`(column_unit = paste0("C", column), reader_label = ifelse(nzchar(resource), paste(reader, resource), reader))]
  L[]
}

#' Column layout: column 1-4, unit (C1-C4), unit_readers ("R1/R5") and resource_note ("C1 = R1 food + R5 house").
soc_column_layout <- function() {
  L <- soc_reader_layout()
  L[, .(unit = paste0("C", column[1]), unit_readers = paste(reader, collapse = "/"),
        resource_note = paste0("C", column[1], " = ", paste(reader_label, collapse = " + "))), by = column]
}

#' Sex from the batch (B1, B2, B5 male; B3, B4, B6 female; sex is nested in batch). Stops on an unknown batch.
soc_sex <- function(batch) {
  s <- unname(SOC_SEX_BY_BATCH[as.character(batch)])
  if (anyNA(s)) stop("Unknown batch: ", paste(unique(batch[is.na(s)]), collapse = ", "), call. = FALSE)
  s
}

#' Logger-clock time (POSIX seconds, labelled UTC) as text with milliseconds; "none" for Inf or NA.
soc_utc <- function(t) {
  t <- as.numeric(t)
  out <- rep("none", length(t))
  ok <- is.finite(t)
  # %OS3 truncates; half a millisecond makes it round
  out[ok] <- format(as.POSIXct(t[ok] + 5e-4, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%OS3")
  out
}

#' Labels of a basis or a comparison of bases (SOC_BASIS_LABEL; other values pass through).
soc_basis_label <- function(basis) { out <- unname(SOC_BASIS_LABEL[as.character(basis)]); ifelse(is.na(out), as.character(basis), out) }

#' Support criterion of a bound ("max60s" -> "observation age <= 60 s"); a comparison "max60s - LOCF" names both.
soc_support_criterion <- function(bound) {
  vapply(as.character(bound), function(b) {
    parts <- strsplit(b, " - ", fixed = TRUE)[[1]]
    lab <- unname(SOC_SUPPORT_CRITERION[parts])
    if (anyNA(lab)) b else paste(lab, collapse = " minus ")
  }, character(1), USE.NAMES = FALSE)
}

#' Phase blocks of the release (blocks.csv) with Phase_vendor (Active / Inactive), Phase (Active / Light), the k-th
#' phase of its class in the recording (phase_index_in_class, by block_no), the narrowest block set, the block
#' length and its start and end as text.
soc_blocks <- function(blocks) {
  b <- data.table::copy(data.table::as.data.table(blocks))
  if (any(!b$Phase %in% c("Active", "Inactive"))) stop("A block has a vendor phase label other than Active / Inactive.", call. = FALSE)
  if (anyDuplicated(b[, .(SourceFile, block)]) || anyDuplicated(b[, .(SourceFile, Phase, PhaseNumber)]))
    stop("Blocks must be unique per recording and per vendor phase label.", call. = FALSE)
  data.table::setorder(b, SourceFile, block_no)
  b[, Phase_vendor := Phase]
  b[, Phase := ifelse(Phase_vendor == "Active", "Active", "Light")]
  b[, phase_index_in_class := seq_len(.N), by = .(SourceFile, Phase)]
  b[, `:=`(block_set = ifelse(stage19_window, "stage19_window", "cc4_grid_period"), block_s = be - bs,
           block_start_utc = soc_utc(bs), block_end_utc = soc_utc(be))]
  b[]
}

#' A srs table keyed by SourceFile and block, with the block labels of soc_blocks() in place of its own Phase,
#' PhaseNumber, block_no and stage19_window (which must agree with blocks.csv).
soc_attach_blocks <- function(d, bl) {
  d <- data.table::copy(data.table::as.data.table(d))
  own <- intersect(c("Phase", "PhaseNumber", "block_no", "stage19_window"), names(d))
  ref <- bl[, .(SourceFile, block, .Phase = Phase_vendor, .PhaseNumber = PhaseNumber, .block_no = block_no, .w = stage19_window)]
  chk <- merge(unique(d[, c("SourceFile", "block", own), with = FALSE]), ref, by = c("SourceFile", "block"), all.x = TRUE)
  bad <- is.na(chk$.block_no) |
    (if ("Phase" %in% own) chk$Phase != chk$.Phase else FALSE) | (if ("PhaseNumber" %in% own) chk$PhaseNumber != chk$.PhaseNumber else FALSE) |
    (if ("block_no" %in% own) chk$block_no != chk$.block_no else FALSE) | (if ("stage19_window" %in% own) chk$stage19_window != chk$.w else FALSE)
  if (any(bad)) stop(sum(bad), " recording-block(s) of a table disagree with blocks.csv.", call. = FALSE)
  if (length(own)) d[, (own) := NULL]
  merge(d, bl[, .(SourceFile, block, block_no, Phase, Phase_vendor, PhaseNumber, phase_index_in_class, stage19_window, block_set)],
        by = c("SourceFile", "block"))
}

#' Animal-recording metadata: SourceFile, AnimalNum, raw_log, Batch, Sex, CC, board, System, CageEpisodeID, tag, the
#' presence counts of the cage episode (n_animals_present, n_tags_present, n_tracked), spatial_excluded, movement_excluded,
#' censor_from (text) and censor_from_s (POSIX seconds, Inf without censoring). raw_logs: SourceFile, raw_log;
#' presence: CageEpisodeID and the three counts (no other column is taken).
soc_animal_meta <- function(animals, presence, raw_logs) {
  a <- data.table::as.data.table(animals)[, .(SourceFile, AnimalNum, Batch, CC, board, System, CageEpisodeID, tag,
                                              spatial_excluded, movement_excluded, censor_from_s = as.numeric(censor_from))]
  a[, Sex := soc_sex(Batch)]
  a <- merge(a, unique(data.table::as.data.table(raw_logs)[, .(SourceFile, raw_log)]), by = "SourceFile", all.x = TRUE)
  a <- merge(a, data.table::as.data.table(presence)[, .(CageEpisodeID, n_animals_present, n_tags_present, n_tracked)],
             by = "CageEpisodeID", all.x = TRUE)
  a[, censor_from := soc_utc(censor_from_s)]
  a[]
}

#' Rows of a per-block table in one block set ("stage19_window": Stage 19's blocks; "cc4_grid_period": the CC4 blocks
#' after them; "whole_span": all blocks, used only by the gates that check against the srs release).
soc_in_set <- function(d, set) {
  switch(set, stage19_window = d[stage19_window == TRUE], cc4_grid_period = d[stage19_window == FALSE], whole_span = d,
         stop("Unknown block set: ", set, call. = FALSE))
}

# ---------------------------------------------------------------- shares with the unknown category

#' Shares of observed and of supported time per group of `observed` (data.table(by..., observed_s), unique groups),
#' every bound (LOCF, max300s, max60s) and every unit, zero-completed: occ holds one basis' seconds by `by` x `unit_col`
#' (columns seconds, sup_60, sup_300). Built with asr_shares_with_unknown(). Returns by..., bound, unit (prefix + unit
#' value, or "unknown"), seconds, observed_s, supported_s, share_all (NA when observed_s = 0) and share_supported (NA for
#' unknown and when supported_s = 0). |unknown| < 1e-6 s (floating sums) is set to 0; the smallest unknown before that
#' is attribute "min_unknown_s".
soc_shares <- function(occ, observed, by, unit_col, units, prefix) {
  observed <- data.table::as.data.table(observed)
  if (anyDuplicated(observed[, by, with = FALSE])) stop("soc_shares(): `observed` has duplicate groups.", call. = FALSE)
  ulev <- c(paste0(prefix, units), "unknown")
  sh <- asr_shares_with_unknown(occ, observed, by = by, unit = unit_col)
  sh[unit != "unknown", unit := paste0(prefix, unit)]
  if (any(!sh$unit %in% ulev)) stop("soc_shares(): unit outside ", paste(ulev, collapse = ", "), call. = FALSE)
  g <- unique(observed[, by, with = FALSE])
  g[, .gid := .I]
  grid <- merge(g, data.table::CJ(.gid = g$.gid, bound = SOC_BOUNDS, unit = ulev, sorted = FALSE), by = ".gid")
  grid <- merge(grid, sh[unit == "unknown", c(by, "bound", "observed_s", "supported_s"), with = FALSE], by = c(by, "bound"), all.x = TRUE)
  grid <- merge(grid, sh[, c(by, "bound", "unit", "s"), with = FALSE], by = c(by, "bound", "unit"), all.x = TRUE)
  grid[is.na(s), s := 0]
  min_unknown <- if (nrow(grid)) min(grid[unit == "unknown"]$s) else NA_real_
  grid[unit == "unknown" & abs(s) < 1e-6, s := 0]
  grid[, `:=`(share_all = ifelse(observed_s > 0, s / observed_s, NA_real_),
              share_supported = ifelse(unit != "unknown" & supported_s > 0, s / supported_s, NA_real_))]
  data.table::setnames(grid, "s", "seconds")
  grid[, `:=`(.b = match(bound, SOC_BOUNDS), .u = match(unit, ulev))]
  data.table::setorderv(grid, c(".gid", ".b", ".u"))
  grid[, c(".gid", ".b", ".u") := NULL]
  data.table::setcolorder(grid, c(by, "bound", "unit", "seconds", "observed_s", "supported_s", "share_all", "share_supported"))
  data.table::setattr(grid, "min_unknown_s", min_unknown)
  grid[]
}

#' soc_shares() for both bases at column (C1-C4) or reader (R1-R8) resolution; adds `basis`. occ: srs occupancy rows
#' (PositionID, basis, seconds, sup_*) carrying the `by` columns.
soc_shares_both <- function(occ, observed, by, resolution = c("column", "reader")) {
  resolution <- match.arg(resolution)
  o <- data.table::copy(data.table::as.data.table(occ))
  if (resolution == "column") o[, column := amv_column(PositionID)]
  spec <- if (resolution == "column") list(col = "column", units = 1:4, prefix = "C") else list(col = "PositionID", units = 1:8, prefix = "R")
  out <- lapply(c("event", "read"), function(b) {
    s <- soc_shares(o[basis == b], observed, by, spec$col, spec$units, spec$prefix)
    list(tab = s[, basis := b], min_unknown = attr(s, "min_unknown_s"))
  })
  res <- data.table::rbindlist(lapply(out, `[[`, "tab"))
  data.table::setattr(res, "min_unknown_s", min(vapply(out, `[[`, numeric(1), "min_unknown"), na.rm = TRUE))
  res
}

#' Seconds-pooled shares over the blocks of each block set: occ and ab (animal_block rows with observed_s) carry `by`
#' and stage19_window. Returns soc_shares_both() rows with block_set.
soc_pool <- function(occ, ab, by, resolution, sets = SOC_BLOCK_SETS) {
  data.table::rbindlist(lapply(sets, function(s) {
    a <- soc_in_set(ab, s)
    soc_shares_both(soc_in_set(occ, s), a[, .(observed_s = sum(observed_s)), by = by], by, resolution)[, block_set := s]
  }))
}

#' Spread of a vector (finite values): n, median, quartiles, min, max, with an optional name prefix.
soc_spread <- function(v, prefix = "") {
  v <- v[is.finite(v)]
  out <- if (!length(v)) list(n = 0L, median = NA_real_, q25 = NA_real_, q75 = NA_real_, min = NA_real_, max = NA_real_) else
    list(n = length(v), median = stats::median(v), q25 = stats::quantile(v, 0.25, names = FALSE),
         q75 = stats::quantile(v, 0.75, names = FALSE), min = min(v), max = max(v))
  stats::setNames(out, paste0(prefix, names(out)))
}

#' Occupancy organisation (S4) of shares p over units (shares of supported time): entropy in bits, -sum(p log2 p) over
#' p > 0 (the formula of mmm_bf_animal_metrics(), Functions/rfid_binfree_metrics.R), the dominant unit and its share.
#' NA without supported time.
soc_organisation <- function(p, unit) {
  ok <- is.finite(p) & p > 0
  if (!any(ok)) return(list(column_entropy_bits = NA_real_, dominant_unit = NA_character_, dominant_share = NA_real_))
  list(column_entropy_bits = -sum(p[ok] * log2(p[ok])), dominant_unit = unit[ok][which.max(p[ok])], dominant_share = max(p[ok]))
}

#' Differences between bases and bounds of a share table (soc_shares_both() rows with `keys`): read - event at each bound,
#' and max300s - LOCF, max60s - LOCF within each basis, for both denominators. Returns keys, unit, denominator,
#' comparison, basis (basis compared), bound (bound compared), value_a, value_b, diff (rows where either value is NA
#' are dropped: the supported share of "unknown" is undefined).
soc_compare <- function(sh, keys) {
  long <- data.table::melt(data.table::as.data.table(sh), id.vars = c(keys, "basis", "bound", "unit"),
                           measure.vars = c("share_all", "share_supported"), variable.name = "denominator", value.name = "v")
  long[, denominator := unname(SOC_DENOMINATOR[as.character(denominator)])]
  pairs <- data.table::rbindlist(list(
    data.table::data.table(comparison = "read - event", basis = "read - event", bound = SOC_BOUNDS, basis_a = "read", bound_a = SOC_BOUNDS,
                           basis_b = "event", bound_b = SOC_BOUNDS),
    data.table::CJ(basis_a = c("event", "read"), bound_a = c("max300s", "max60s"))[
      , .(comparison = paste(bound_a, "- LOCF"), basis = basis_a, bound = paste(bound_a, "- LOCF"), basis_a, bound_a, basis_b = basis_a, bound_b = "LOCF")]))
  data.table::rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
    p <- pairs[i]
    a <- long[basis == p$basis_a & bound == p$bound_a, c(keys, "unit", "denominator", "v"), with = FALSE]
    b <- long[basis == p$basis_b & bound == p$bound_b, c(keys, "unit", "denominator", "v"), with = FALSE]
    m <- merge(a, b, by = c(keys, "unit", "denominator"), suffixes = c("_a", "_b"))
    m <- m[!is.na(v_a) & !is.na(v_b)]
    m[, `:=`(comparison = p$comparison, basis = p$basis, bound = p$bound, value_a = v_a, value_b = v_b, diff = v_a - v_b)]
    m[, c("v_a", "v_b") := NULL][]
  }))
}

#' Parity of the event basis (LOCF reader seconds per animal-phase) with Stage 19's phase table. mine and s19: SourceFile,
#' Phase (vendor label), PhaseNumber, AnimalNum, PositionID with `seconds` (mine) or `PositionSeconds` (s19). Returns
#' one row: rows, animal_phases, only_here, only_stage19, max_abs_diff_s, sum_abs_diff_s, total_s.
soc_parity_stage19 <- function(mine, s19) {
  k <- c("SourceFile", "Phase", "PhaseNumber", "AnimalNum", "PositionID")
  p <- merge(data.table::as.data.table(mine)[, c(k, "seconds"), with = FALSE], data.table::as.data.table(s19)[, c(k, "PositionSeconds"), with = FALSE],
             by = k, all = TRUE)
  p[is.na(seconds), seconds := 0][is.na(PositionSeconds), PositionSeconds := 0]
  data.table::data.table(rows = nrow(p), animal_phases = data.table::uniqueN(p[, .(SourceFile, Phase, PhaseNumber, AnimalNum)]),
                         only_here = sum(p$seconds > 0 & p$PositionSeconds == 0), only_stage19 = sum(p$PositionSeconds > 0 & p$seconds == 0),
                         max_abs_diff_s = if (nrow(p)) max(abs(p$seconds - p$PositionSeconds)) else NA_real_,
                         sum_abs_diff_s = sum(abs(p$seconds - p$PositionSeconds)), total_s = sum(p$PositionSeconds))
}

#' Run metadata (meta: run_id, srs_run_id, reconstruction, tau_s), basis_label and support_criterion added; the
#' metadata columns first (SOC_META_ORDER), rows ordered by `keys` (SOC_LEVELS give the order of block sets, phases,
#' bases, bounds, units, readers).
soc_finish <- function(d, meta, keys = character(0)) {
  d <- data.table::copy(data.table::as.data.table(d))
  d[, `:=`(run_id = meta$run_id, srs_run_id = meta$srs_run_id, reconstruction = meta$reconstruction, tau_s = meta$tau_s)]
  if ("basis" %in% names(d) && !"basis_label" %in% names(d)) d[, basis_label := soc_basis_label(basis)]
  if ("bound" %in% names(d) && !"support_criterion" %in% names(d)) d[, support_criterion := soc_support_criterion(bound)]
  data.table::setcolorder(d, intersect(SOC_META_ORDER, names(d)))
  keys <- intersect(keys, names(d))
  if (length(keys)) {
    tmp <- paste0(".o", seq_along(keys))
    for (i in seq_along(keys)) {
      v <- d[[keys[i]]]
      lev <- SOC_LEVELS[[keys[i]]]
      data.table::set(d, j = tmp[i], value = if (is.null(lev)) v else factor(v, levels = unique(c(lev, sort(unique(as.character(v)))))))
    }
    data.table::setorderv(d, tmp, na.last = TRUE)
    d[, (tmp) := NULL]
  }
  d[]
}

#' Column order: the metadata columns (SOC_META_ORDER), then `cols`, then the rest; names a table lacks are skipped.
soc_colorder <- function(d, cols) data.table::setcolorder(d, unique(intersect(c(SOC_META_ORDER, cols), names(d))))

#' One row of the gate table. type "gate": pass must be TRUE for the run to publish; "diagnostic": reported only.
soc_gate_row <- function(id, check, pass, value, criterion, type = "gate") {
  data.table::data.table(gate_id = id, type = type, check = check, pass = if (type == "gate") isTRUE(pass) else NA,
                         value = paste(value, collapse = "; "), criterion = criterion)
}

#' Compact number for gate values.
soc_num <- function(x, digits = 4) ifelse(is.finite(x), trimws(formatC(x, digits = digits, format = "g")), as.character(x))

# ---------------------------------------------------------------- the candidate

#' All candidate tables, figure inputs and gates from the srs tables. Pure (no file access).
#'   tab: srs tables occupancy, animal_block, animals, blocks, censored, read_agreement, gaps, colocation (AnimalNum as
#'        character, canonical);
#'   ref: presence (CageEpisodeID, n_animals_present, n_tags_present, n_tracked), raw_logs (SourceFile, raw_log,
#'        raw_logs_ignored, clock_fold_events_dropped), stage19 (SourceFile, Phase, PhaseNumber, AnimalNum, PositionID,
#'        PositionSeconds, observation_seconds; keys and seconds only), and the srs reference tables
#'        shares_unknown_column_pooled, shares_unknown_reader_pooled, support_by_phase, parity_stage19;
#'   meta: run_id, srs_run_id, reconstruction, tau_s;
#'   expected: animal_flags (SourceFile, AnimalNum, spatial_excluded, movement_excluded, censor_from_s) from the
#'        released exclusion version, records (record_id, action, CageEpisodeID, reader, resource, from), and optional
#'        counts (animal_recordings, kept_animal_recordings, animal_blocks, observed_animal_blocks, stage19_cells).
#' Returns list(tables = named list of data.tables, figures = named list of figure inputs, gates = data.table).
soc_build <- function(tab, ref, meta, expected) {
  need <- c("occupancy", "animal_block", "animals", "blocks", "censored", "read_agreement", "gaps", "colocation")
  if (length(setdiff(need, names(tab)))) stop("soc_build(): missing srs table(s) ", paste(setdiff(need, names(tab)), collapse = ", "), call. = FALSE)
  gates <- list()
  add <- function(...) gates[[length(gates) + 1L]] <<- soc_gate_row(...)
  out <- list(); fig <- list()
  L <- soc_reader_layout(); CL <- soc_column_layout()
  unit_info <- rbind(CL[, .(unit, unit_readers, resource_note)],
                     L[, .(unit = reader, unit_readers = reader, resource_note = ifelse(nzchar(resource), paste(reader, "=", resource), ""))],
                     data.table::data.table(unit = "unknown", unit_readers = "-", resource_note = SOC_UNKNOWN_NOTE))
  bl <- soc_blocks(tab$blocks)
  am <- soc_animal_meta(tab$animals, ref$presence, ref$raw_logs)
  am_cols <- am[, .(SourceFile, AnimalNum, raw_log, Batch, Sex, CC, board, System, CageEpisodeID, tag, n_animals_present, n_tags_present,
                    n_tracked, spatial_excluded, censor_from)]
  ep_cols <- unique(am[, .(SourceFile, raw_log, Batch, Sex, CC, board, System, CageEpisodeID, n_animals_present, n_tags_present, n_tracked,
                           spatial_excluded, censor_from)])
  occ <- soc_attach_blocks(tab$occupancy, bl)
  ab <- soc_attach_blocks(tab$animal_block, bl)
  akey <- c("SourceFile", "AnimalNum", "block")
  ekey <- c("CageEpisodeID", "SourceFile", "Batch", "CC", "board", "System")
  battr <- c("block_no", "Phase", "Phase_vendor", "PhaseNumber", "phase_index_in_class", "stage19_window", "block_set")
  ex_ep <- unique(am[spatial_excluded == TRUE]$CageEpisodeID)
  keep <- function(d) d[!CageEpisodeID %in% ex_ep]
  occ_ok <- keep(occ); ab_ok <- keep(ab)

  # ---- exclusions, censoring, keys
  ef <- merge(am[, .(SourceFile, AnimalNum, spatial_excluded, movement_excluded, censor_from_s)],
              data.table::as.data.table(expected$animal_flags), by = c("SourceFile", "AnimalNum"), all = TRUE, suffixes = c("", "_expected"))
  same_cf <- (is.infinite(ef$censor_from_s) & is.infinite(ef$censor_from_s_expected)) | abs(ef$censor_from_s - ef$censor_from_s_expected) < 1e-3
  add("G03", "exclusion flags and censoring times of the srs release equal the released exclusion version",
      !anyNA(ef$spatial_excluded_expected) && !anyNA(ef$spatial_excluded) && all(ef$spatial_excluded == ef$spatial_excluded_expected) &&
        all(ef$movement_excluded == ef$movement_excluded_expected) && all(same_cf %in% TRUE),
      c(paste("spatially excluded cage episodes:", paste(sort(ex_ep), collapse = ", ")),
        paste("censored animals:", sum(is.finite(am$censor_from_s)))),
      "per animal-recording: spatial_excluded, movement_excluded and censor_from (within 1 ms) identical")
  add("G03b", "exclusion is a cage-episode property", all(am[, data.table::uniqueN(spatial_excluded), by = CageEpisodeID]$V1 == 1L),
      paste(length(ex_ep), "cage episodes excluded"), "every animal of an episode has the same spatial_excluded")
  if (!is.null(expected$counts)) {
    got <- c(animal_recordings = nrow(am), kept_animal_recordings = nrow(am[spatial_excluded == FALSE]), animal_blocks = nrow(ab),
             observed_animal_blocks = nrow(ab[observed_s > 0]))
    k <- unlist(expected$counts)[intersect(names(expected$counts), names(got))]
    add("G04", "row counts of the release", length(k) > 0 && all(got[names(k)] == k),
        paste(names(got), got, sep = " = "), paste("expected", paste(names(k), k, sep = " = ", collapse = ", ")))
  }
  add("G17", "presence counts joined for every animal-recording", !anyNA(am$n_animals_present) && !anyNA(am$n_tags_present) && !anyNA(am$n_tracked),
      paste(sum(is.na(am$n_animals_present)), "animal-recordings without presence counts"), "n_animals_present, n_tags_present, n_tracked all present")
  add("G18", "raw log known for every recording", !anyNA(am$raw_log) && all(nzchar(am$raw_log)),
      paste(data.table::uniqueN(am[!is.na(raw_log)]$SourceFile), "of", data.table::uniqueN(am$SourceFile), "recordings"), "Stage 19 audit names the raw log")
  cens <- data.table::as.data.table(tab$censored)[, .(censored_s = sum(seconds)), by = akey]
  abx <- merge(ab[, c(akey, "CageEpisodeID", "CC", "observed_s", battr), with = FALSE], bl[, .(SourceFile, block, block_start_utc, block_end_utc, block_s)],
               by = c("SourceFile", "block"))
  abx <- merge(abx, cens, by = akey, all.x = TRUE)
  abx[is.na(censored_s), censored_s := 0]
  cens_ep <- unique(merge(cens, am[, .(SourceFile, AnimalNum, CageEpisodeID)], by = c("SourceFile", "AnimalNum"))$CageEpisodeID)
  cens_exp <- unique(am[is.finite(censor_from_s)]$CageEpisodeID)
  add("G19", "censoring: observed + censored time = block length; censored time only where a censoring time is set; none in stage19_window",
      max(abs(abx$observed_s + abx$censored_s - abx$block_s)) < 1e-3 && all(cens_ep %in% cens_exp) &&
        sum(abx[stage19_window == TRUE]$censored_s) == 0,
      c(sprintf("max |observed + censored - block| = %s s", soc_num(max(abs(abx$observed_s + abx$censored_s - abx$block_s)))),
        sprintf("censored %s s in %s", soc_num(sum(abx$censored_s), 8), paste(cens_ep, collapse = ", "))),
      "|diff| < 1 ms; censored episodes have a censoring time; 0 s censored inside stage19_window")
  lab_ok <- bl[, all(ifelse(Phase == "Active", phase_index_in_class == PhaseNumber, phase_index_in_class == PhaseNumber - 1L))]
  add("G21", "phase labels: Light = vendor Inactive; k-th active = vendor PhaseNumber k, k-th light = vendor PhaseNumber k + 1", lab_ok,
      paste(nrow(bl), "blocks;", paste(names(table(bl$Phase)), table(bl$Phase), collapse = ", ")), "every recording")
  add("G16", "column mapping: C1 = R1 + R5 ... C4 = R4 + R8 (amv_column)", identical(L$column, c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L)) &&
        identical(CL$unit_readers, c("R1/R5", "R2/R6", "R3/R7", "R4/R8")), paste(CL$resource_note, collapse = "; "), "fixed layout")

  # ---- animal-block shares (both resolutions) and their consistency
  obs_ab <- ab[, c(akey, "observed_s"), with = FALSE]
  col_ab <- soc_shares_both(occ, obs_ab, akey, "column")
  rd_ab <- soc_shares_both(occ, obs_ab, akey, "reader")
  lc <- merge(occ[, .(L = sum(seconds)), by = c(akey, "basis")],
              rbind(data.table::copy(obs_ab)[, basis := "event"], data.table::copy(obs_ab)[, basis := "read"]), by = c(akey, "basis"), all = TRUE)
  lc[is.na(L), L := 0]
  sd_ <- data.table::as.data.table(tab$animals)[, .(SourceFile, AnimalNum, event = event_seeded, read = read_seeded)]
  lc <- merge(lc, data.table::melt(sd_, id.vars = c("SourceFile", "AnimalNum"), variable.name = "basis", value.name = "seeded",
                                   variable.factor = FALSE), by = c("SourceFile", "AnimalNum", "basis"))
  d5 <- if (nrow(lc[seeded == TRUE])) max(abs(lc[seeded == TRUE]$L - lc[seeded == TRUE]$observed_s)) else 0
  add("G05", "LOCF seconds = observed time per animal-block, both bases (an unseeded animal-recording: LOCF <= observed time)",
      d5 < 1e-6 && all(lc[seeded == FALSE]$L <= lc[seeded == FALSE]$observed_s + 1e-6),
      sprintf("max |LOCF - observed| = %s s over %d seeded animal-block-bases; %d unseeded animal-block-bases", soc_num(d5), nrow(lc[seeded == TRUE]),
              nrow(lc[seeded == FALSE])), "< 1e-6 s where seeded")
  sp <- occ[, .(s60 = sum(sup_60), s300 = sum(sup_300)), by = c(akey, "basis")]
  spw <- data.table::dcast(sp, SourceFile + AnimalNum + block ~ basis, value.var = c("s60", "s300"), fill = 0)
  for (cn in c("s60_event", "s60_read", "s300_event", "s300_read")) if (!cn %in% names(spw)) data.table::set(spw, j = cn, value = 0)
  dsup <- if (nrow(spw)) max(abs(spw$s60_event - spw$s60_read), abs(spw$s300_event - spw$s300_read)) else 0
  add("G06", "supported time (<= 60 s, <= 300 s) equal in both bases per animal-block", dsup < 1e-6, sprintf("max |event - read| = %s s", soc_num(dsup)), "< 1e-6 s")
  # NA pattern: share_all NA exactly when nothing is observed; share_supported NA for unknown, and for every unit when nothing is supported
  sums <- function(d) d[, .(sa = sum(share_all), ss = sum(share_supported, na.rm = TRUE), o = observed_s[1], su = supported_s[1],
                            na_ok = (all(is.na(share_all)) == (observed_s[1] <= 0)) && (all(!is.na(share_all)) == (observed_s[1] > 0)) &&
                              is.na(share_supported[unit == "unknown"]) &&
                              (if (supported_s[1] > 0) all(!is.na(share_supported[unit != "unknown"])) else all(is.na(share_supported)))),
                        by = c(akey, "basis", "bound")]
  sc <- sums(col_ab); sr <- sums(rd_ab)
  s1 <- max(abs(c(sc[o > 0]$sa, sr[o > 0]$sa) - 1))
  s2 <- max(abs(c(sc[su > 0]$ss, sr[su > 0]$ss) - 1))
  add("G07", "share_all (units + unknown) sums to 1 per animal-block, basis and bound, column and reader resolution", s1 < 1e-9,
      sprintf("max |sum - 1| = %s", soc_num(s1)), "< 1e-9 where observed time > 0")
  add("G08", "share_supported (units) sums to 1 where supported time > 0; NA without support, for unknown, and share_all NA without observed time",
      s2 < 1e-9 && all(sc$na_ok) && all(sr$na_ok),
      sprintf("max |sum - 1| = %s; animal-block-bounds without support: %d (column)", soc_num(s2), nrow(sc[su <= 0])), "< 1e-9; NA pattern as stated")
  rdc <- rd_ab[unit != "unknown"][, cu := paste0("C", amv_column(as.integer(sub("^R", "", unit))))][, .(s_r = sum(seconds)), by = c(akey, "basis", "bound", "cu")]
  data.table::setnames(rdc, "cu", "unit")
  cr <- merge(rdc, col_ab[unit != "unknown", c(akey, "basis", "bound", "unit", "seconds"), with = FALSE], by = c(akey, "basis", "bound", "unit"))
  add("G09", "column seconds = sum of their two readers' seconds", max(abs(cr$s_r - cr$seconds)) < 1e-6,
      sprintf("max |diff| = %s s", soc_num(max(abs(cr$s_r - cr$seconds)))), "< 1e-6 s")
  mu <- min(attr(col_ab, "min_unknown_s"), attr(rd_ab, "min_unknown_s"))
  add("G20", "unknown time is never negative beyond floating noise", mu > -1e-6, sprintf("min unknown before rounding = %s s", soc_num(mu)), "> -1e-6 s")
  lu <- col_ab[unit == "unknown" & bound == "LOCF", .(s = sum(seconds)), by = basis]
  add("D01", "LOCF unknown time (unseeded time before an animal's first read), by basis", NA,
      paste(lu$basis, soc_num(lu$s), sep = ": "), "0 when every animal-recording is seeded", type = "diagnostic")

  # ---- T1 / T2: animal x phase block x basis x bound x unit
  ab_block <- function(sh) {
    d <- merge(sh, abx[, c(akey, battr, "block_start_utc", "block_end_utc", "censored_s"), with = FALSE], by = akey)
    d <- merge(d, am_cols, by = c("SourceFile", "AnimalNum"))
    d <- merge(d, unit_info, by = "unit", all.x = TRUE)
    d
  }
  t1 <- ab_block(col_ab)
  soc_colorder(t1, c("block", battr, "block_start_utc", "block_end_utc", "observed_s",
                                "censored_s", "supported_s", "unit", "unit_readers", "resource_note", "seconds", "share_all", "share_supported"))
  out$occupancy_animal_phase_column <- soc_finish(t1, meta, c("SourceFile", "AnimalNum", "block_no", "basis", "bound", "unit"))
  t2 <- ab_block(rd_ab)
  t2[, `:=`(reader = unit, column = paste0("C", amv_column(match(unit, L$reader))), resource = L$resource[match(unit, L$reader)])]
  t2[unit == "unknown", `:=`(reader = "unknown", column = "-", resource = "")]
  out$occupancy_animal_phase_reader <- soc_finish(t2, meta, c("SourceFile", "AnimalNum", "block_no", "basis", "bound", "unit"))

  # ---- T3 / T3r: animal x window (seconds-pooled over blocks)
  win <- function(res) {
    pc <- soc_pool(occ, ab, c("SourceFile", "AnimalNum", "Phase"), res)[, window := "phase_class"]
    o1 <- occ[CC == "CC1" & block_no == 1L]; a1 <- ab[CC == "CC1" & block_no == 1L]
    w1 <- soc_pool(o1, a1, c("SourceFile", "AnimalNum", "Phase"), res)[, window := "cc1_active1"]
    d <- rbind(pc, w1)
    nb <- data.table::rbindlist(lapply(SOC_BLOCK_SETS, function(s) {
      x <- soc_in_set(abx, s)
      rbind(x[, .(n_blocks = .N, censored_s = sum(censored_s), window = "phase_class"), by = .(SourceFile, AnimalNum, Phase)],
            x[CC == "CC1" & block_no == 1L, .(n_blocks = .N, censored_s = sum(censored_s), window = "cc1_active1"),
              by = .(SourceFile, AnimalNum, Phase)])[, block_set := s]
    }))
    d <- merge(d, nb, by = c("SourceFile", "AnimalNum", "Phase", "window", "block_set"))
    d <- merge(d, am_cols, by = c("SourceFile", "AnimalNum"))
    d <- merge(d, unit_info, by = "unit", all.x = TRUE)
    d[, window_note := ifelse(window == "phase_class", "all phases of the class in the block set, pooled by seconds (Stage 19 Full_phase on stage19_window)",
                              "CC1 first active phase, the whole 12 h (Stage 19 primary window); identical in both block sets")]
    soc_colorder(d, c("Phase", "window_note", "n_blocks", "observed_s", "censored_s", "supported_s",
                                 "unit", "unit_readers", "resource_note", "seconds", "share_all", "share_supported"))
    d
  }
  t3 <- win("column"); t3r <- win("reader")
  out$occupancy_animal_window_column <- soc_finish(t3, meta, c("block_set", "window", "SourceFile", "AnimalNum", "Phase", "basis", "bound", "unit"))
  out$occupancy_animal_window_reader <- soc_finish(t3r, meta, c("block_set", "window", "SourceFile", "AnimalNum", "Phase", "basis", "bound", "unit"))

  # ---- T4: support, reads, changes and agreement per animal-block
  sup <- occ[basis == "read", .(supported_le60_s = sum(sup_60), supported_le300_s = sum(sup_300)), by = akey]
  t4 <- merge(abx, sup, by = akey, all.x = TRUE)
  t4[is.na(supported_le60_s), `:=`(supported_le60_s = 0, supported_le300_s = 0)]
  t4 <- merge(t4, ab[, c(akey, "observations", "changes_event_reader", "changes_event_column", "changes_read_reader", "changes_read_column"), with = FALSE],
              by = akey)
  ra <- data.table::as.data.table(tab$read_agreement)[, .(SourceFile, AnimalNum, block, agreement_reads = observations, event_holds_read_reader,
                                                          read_holds_read_reader, event_holds_read_column, read_holds_read_column)]
  t4 <- merge(t4, ra, by = akey, all.x = TRUE)
  an_x <- data.table::as.data.table(tab$animals)[, .(SourceFile, AnimalNum, read_seeded, read_seed_age_s, event_seeded, event_seed_age_s,
                                                     tie_observations_in_span, observations_in_recording = observations)]
  t4 <- merge(t4, an_x, by = c("SourceFile", "AnimalNum"))
  t4 <- merge(t4, unique(data.table::as.data.table(ref$raw_logs)[, .(SourceFile, raw_logs_ignored, clock_fold_events_dropped)]), by = "SourceFile", all.x = TRUE)
  t4 <- merge(t4, am_cols[, !c("CageEpisodeID", "CC")], by = c("SourceFile", "AnimalNum"))
  h <- t4$observed_s / 3600
  rate <- function(x) ifelse(h > 0, x / h, NA_real_)
  frac <- function(x, d) ifelse(d > 0, x / d, NA_real_)
  t4[, `:=`(unknown_le60_s = observed_s - supported_le60_s, unknown_le300_s = observed_s - supported_le300_s)]
  t4[abs(unknown_le60_s) < 1e-6, unknown_le60_s := 0][abs(unknown_le300_s) < 1e-6, unknown_le300_s := 0]
  t4[, `:=`(supported_le60_share_of_observed_time = frac(supported_le60_s, observed_s), supported_le300_share_of_observed_time = frac(supported_le300_s, observed_s),
            unknown_le60_share_of_observed_time = frac(unknown_le60_s, observed_s), unknown_le300_share_of_observed_time = frac(unknown_le300_s, observed_s),
            reads_per_observed_h = rate(observations), changes_event_reader_per_observed_h = rate(changes_event_reader),
            changes_event_column_per_observed_h = rate(changes_event_column), changes_read_reader_per_observed_h = rate(changes_read_reader),
            changes_read_column_per_observed_h = rate(changes_read_column),
            event_holds_read_reader_share = frac(event_holds_read_reader, agreement_reads), read_holds_read_reader_share = frac(read_holds_read_reader, agreement_reads),
            event_holds_read_column_share = frac(event_holds_read_column, agreement_reads), read_holds_read_column_share = frac(read_holds_read_column, agreement_reads),
            basis = "both", bound = "max60s; max300s",
            note = "support = a read <= d s old (recently detected, not validated); changes are reconstruction changes per observed hour, not locomotion; agreement at a read is timestamp consistency, not accuracy")]
  t4[, support_criterion := "observation age <= 60 s (_le60_) and <= 300 s (_le300_)"]
  out$support_animal_phase <- soc_finish(t4, meta, c("SourceFile", "AnimalNum", "block_no"))

  # ---- T5: unknown time by the reader last assigned and the gap length (read basis, 1-s grid)
  gp <- soc_attach_blocks(tab$gaps, bl)
  gp[, `:=`(bound = paste0("max", bound_s, "s"), last_reader = ifelse(pos > 0, paste0("R", pos), "none"))]
  gp[, last_reader_resource := ifelse(pos > 0, L$resource[pmax(pos, 1L)], "")]
  gp <- merge(gp, abx[, c(akey, "observed_s"), with = FALSE], by = akey)
  gp[, `:=`(unknown_share_of_observed_time = ifelse(observed_s > 0, unknown_s / observed_s, NA_real_),
            unknown_share_of_animal_block_unknown_time = unknown_s / sum(unknown_s)), by = c(akey, "bound")]
  t5 <- merge(gp[, c(akey, battr, "bound", "last_reader", "last_reader_resource", "gap_class", "unknown_s", "observed_s",
                     "unknown_share_of_observed_time", "unknown_share_of_animal_block_unknown_time"), with = FALSE], am_cols, by = c("SourceFile", "AnimalNum"))
  data.table::setnames(t5, "unknown_s", "unknown_s_grid")
  t5[, `:=`(basis = "read", note = "1-s grid of the read basis (within about 10 s of the exact unknown time per block); the reader last assigned is where the animal was last read, not where it was")]
  out$unknown_by_last_reader_gap <- soc_finish(t5, meta, c("SourceFile", "AnimalNum", "block_no", "bound", "last_reader", "gap_class"))

  # ---- T6: cage-mate context of unknown and supported time (missingness context; not proximity or affiliation)
  co <- keep(soc_attach_blocks(tab$colocation, bl))[mates > 0]
  co[, `:=`(bound = paste0("max", bound_s, "s"), reader = ifelse(pos > 0, paste0("R", pos), "none"))]
  t6 <- data.table::rbindlist(lapply(SOC_BLOCK_SETS, function(s) {
    x <- soc_in_set(co, s)
    agg <- function(by) x[, .(n_animals = data.table::uniqueN(paste(Batch, AnimalNum)), n_animal_recordings = data.table::uniqueN(paste(SourceFile, AnimalNum)),
                             seconds_grid = sum(seconds), mate_same_reader_s = sum(mate_same_reader),
                             mate_same_reader_supported_s = sum(mate_same_reader_supported), mate_supported_anywhere_s = sum(mate_supported_anywhere)), by = by]
    rbind(agg(c("Phase", "bound", "reader", "state"))[, `:=`(level = "pooled over cage episodes", CageEpisodeID = "all")],
          agg(c(ekey, "Phase", "bound", "reader", "state"))[, level := "cage episode"], fill = TRUE)[, block_set := s]
  }))
  t6[, `:=`(mate_same_reader_share_of_seconds = mate_same_reader_s / seconds_grid, mate_same_reader_supported_share_of_seconds = mate_same_reader_supported_s / seconds_grid,
            mate_supported_anywhere_share_of_seconds = mate_supported_anywhere_s / seconds_grid,
            resource = ifelse(reader %in% L$reader, L$resource[match(reader, L$reader)], ""), basis = "read",
            note = "missingness context on the 1-s grid: a cage-mate assigned to (or read at) the same reader; not proximity or affiliation")]
  t6 <- merge(t6, ep_cols[, .(CageEpisodeID, Sex, n_animals_present, n_tags_present, n_tracked)], by = "CageEpisodeID", all.x = TRUE)
  out$unknown_cagemate_context <- soc_finish(t6, meta, c("block_set", "level", "CageEpisodeID", "Phase", "bound", "reader", "state"))

  # ---- T7: cage episode x phase block and x phase class (pooled by seconds and equal-weight animal mean)
  col_ok <- merge(col_ab, ab[, c(akey, "CageEpisodeID", "Phase", "stage19_window"), with = FALSE], by = akey)[!CageEpisodeID %in% ex_ep]
  amean <- function(d, by) d[, .(n_animals_observed = sum(observed_s > 0), n_animals_supported = sum(supported_s > 0),
                                 share_all_animal_mean = mean(share_all, na.rm = TRUE), share_supported_animal_mean = mean(share_supported, na.rm = TRUE)), by = by]
  eb <- soc_shares_both(occ_ok, ab_ok[, .(observed_s = sum(observed_s)), by = c(ekey, "block")], c(ekey, "block"), "column")
  eb <- merge(eb, amean(col_ok, c("CageEpisodeID", "block", "basis", "bound", "unit")), by = c("CageEpisodeID", "block", "basis", "bound", "unit"))
  eb <- merge(eb, bl[, c("SourceFile", "block", battr), with = FALSE], by = c("SourceFile", "block"))
  eb[, level := "phase block"]
  ep_col <- soc_pool(occ_ok, ab_ok, c(ekey, "Phase"), "column")
  t3_ok <- t3[window == "phase_class" & spatial_excluded == FALSE]
  ep <- merge(ep_col, amean(t3_ok, c("CageEpisodeID", "Phase", "block_set", "basis", "bound", "unit")), by = c("CageEpisodeID", "Phase", "block_set", "basis", "bound", "unit"))
  ep[, level := "phase class"]
  t7 <- rbind(eb, ep, fill = TRUE)
  t7 <- merge(t7, ep_cols[, .(CageEpisodeID, raw_log, Sex, n_animals_present, n_tags_present, n_tracked, spatial_excluded, censor_from)], by = "CageEpisodeID")
  t7 <- merge(t7, unit_info, by = "unit", all.x = TRUE)
  data.table::setnames(t7, c("seconds", "share_all", "share_supported"), c("seconds_pooled", "share_all_pooled", "share_supported_pooled"))
  soc_colorder(t7, c("block", battr[battr != "block_set"], "observed_s", "supported_s", "unit", "unit_readers",
                                "resource_note", "seconds_pooled", "share_all_pooled", "share_supported_pooled"))
  # per-block rows (block_set = the narrowest set holding the block) and pooled phase-class rows (block_set = the set pooled
  # over) are separate tables, so block_set has one meaning per table
  out$cage_episode_block_column <- soc_finish(t7[level == "phase block"], meta, c("block_set", "CageEpisodeID", "block_no", "Phase", "basis", "bound", "unit"))
  out$cage_episode_window_column <- soc_finish(t7[level == "phase class"], meta, c("block_set", "CageEpisodeID", "Phase", "basis", "bound", "unit"))

  # ---- T8: occupancy organisation (S4) on the shares of supported time, with the unknown share
  org <- function(d, by) d[, c(soc_organisation(share_supported[unit != "unknown"], unit[unit != "unknown"]),
                               list(unknown_share_all = share_all[unit == "unknown"], observed_s = observed_s[1], supported_s = supported_s[1])), by = by]
  o1 <- merge(org(col_ab, c(akey, "basis", "bound")), abx[, c(akey, battr), with = FALSE], by = akey)[, level := "phase block"]
  o2 <- org(t3[window == "phase_class"], c("SourceFile", "AnimalNum", "Phase", "block_set", "basis", "bound"))[, level := "phase class"]
  t8 <- merge(rbind(o1, o2, fill = TRUE), am_cols, by = c("SourceFile", "AnimalNum"))
  t8[, note := "S4, descriptive: entropy (bits, max 2) and dominant-column share over C1-C4 of supported time; the unknown share is reported beside it, never folded in"]
  out$organisation_animal_block <- soc_finish(t8[level == "phase block"], meta, c("block_set", "SourceFile", "AnimalNum", "block_no", "Phase", "basis", "bound"))
  out$organisation_animal_window <- soc_finish(t8[level == "phase class"], meta, c("block_set", "SourceFile", "AnimalNum", "Phase", "basis", "bound"))

  # ---- T9 and the basis comparison: per cage episode (phase class), spread, pooled, by board and by CC
  ep_rd <- soc_pool(occ_ok, ab_ok, c(ekey, "Phase"), "reader")
  cmp_ep <- rbind(soc_compare(ep_col, c(ekey, "Phase", "block_set"))[, resolution := "column"],
                  soc_compare(ep_rd, c(ekey, "Phase", "block_set"))[, resolution := "reader"])
  pool_by <- function(by, res) soc_pool(occ_ok, ab_ok, by, res)
  P_col <- pool_by("Phase", "column"); P_rd <- pool_by("Phase", "reader")
  cmp_pool <- rbind(soc_compare(P_col, c("Phase", "block_set"))[, resolution := "column"], soc_compare(P_rd, c("Phase", "block_set"))[, resolution := "reader"])
  # n_animals: distinct animals; n_animal_recordings: animal x cage-change recordings (an animal counts once per CC)
  cnt <- function(by) data.table::rbindlist(lapply(SOC_BLOCK_SETS, function(s) soc_in_set(ab_ok, s)[observed_s > 0, .(n_cage_episodes = data.table::uniqueN(CageEpisodeID),
    n_animals = data.table::uniqueN(paste(Batch, AnimalNum)), n_animal_recordings = data.table::uniqueN(paste(SourceFile, AnimalNum))), by = by][, block_set := s]))
  ck <- c("Phase", "block_set", "comparison", "basis", "bound", "resolution", "unit", "denominator")
  spread_ep <- cmp_ep[, soc_spread(diff, "episode_diff_"), by = ck]
  t9 <- merge(cmp_pool, spread_ep, by = ck, all = TRUE)
  t9 <- merge(t9, cnt("Phase"), by = c("Phase", "block_set"), all.x = TRUE)
  t9[, level := "pooled over animals (seconds) and spread over cage episodes"]
  t9 <- merge(t9, unit_info, by = "unit", all.x = TRUE)
  data.table::setnames(t9, c("value_a", "value_b", "diff"), c("pooled_value_a", "pooled_value_b", "pooled_diff"))
  t9[, value_note := "read - event: a = read, b = event; maxNs - LOCF: a = bounded, b = LOCF (same basis)"]
  out$basis_bound_sensitivity <- soc_finish(t9, meta, c("block_set", "Phase", "comparison", "basis", "bound", "denominator", "resolution", "unit"))
  t9e <- merge(cmp_ep, ep_cols[, .(CageEpisodeID, raw_log, Sex, n_animals_present, n_tags_present, n_tracked, spatial_excluded, censor_from)], by = "CageEpisodeID")
  t9e[, level := "cage episode (phase class, pooled by seconds)"]
  out$basis_bound_sensitivity_by_episode <- soc_finish(t9e, meta, c("block_set", "CageEpisodeID", "Phase", "comparison", "basis", "bound", "denominator", "resolution", "unit"))

  re <- function(d) d[comparison == "read - event" & (resolution == "column" | unit == "R5")]
  bc_ep <- re(cmp_ep)
  bc <- list(
    merge(re(cmp_pool), cnt("Phase"), by = c("Phase", "block_set"))[, `:=`(level = "pooled over animals (seconds)", CC = "all", board = "all", CageEpisodeID = "all")],
    bc_ep[, .(n_cage_episodes = .N, value_a = NA_real_, value_b = NA_real_, diff = NA_real_, level = "cage-episode spread",
              CC = "all", board = "all", CageEpisodeID = "all", sp = list(soc_spread(diff, "episode_"))), by = ck],
    merge(bc_ep[, c(ck, "value_a", "value_b", "diff", "CC", "board", "CageEpisodeID"), with = FALSE],
          ep_cols[, .(CageEpisodeID, SourceFile, raw_log, Batch, Sex, System, n_animals_present, n_tags_present, n_tracked)], by = "CageEpisodeID")[
      , `:=`(n_cage_episodes = 1L, level = "cage episode", board = as.character(board))])
  for (st in c("board", "CC")) {
    pc <- rbind(soc_compare(pool_by(c("Phase", st), "column"), c("Phase", st, "block_set"))[, resolution := "column"],
                soc_compare(pool_by(c("Phase", st), "reader"), c("Phase", st, "block_set"))[, resolution := "reader"])
    pc <- merge(re(pc), cnt(c("Phase", st)), by = c("Phase", st, "block_set"))
    md <- bc_ep[, .(sp = list(soc_spread(diff, "episode_"))), by = c(ck, st)]
    pc <- merge(pc, md, by = c(ck, st), all.x = TRUE)
    pc[, `:=`(level = paste("by", st), CageEpisodeID = "all")]
    if (st == "board") pc[, `:=`(board = as.character(board), CC = "all")] else pc[, board := "all"]
    bc[[length(bc) + 1L]] <- pc
  }
  bcd <- data.table::rbindlist(bc, fill = TRUE)
  bcd[, board := as.character(board)]
  spcols <- names(soc_spread(numeric(0), "episode_"))
  bcd[, (spcols) := lapply(spcols, function(nm) vapply(sp, function(z) if (is.null(z)) NA_real_ else as.numeric(z[[nm]]), numeric(1)))]
  bcd[, sp := NULL]
  data.table::setnames(bcd, c("value_a", "value_b", "diff"), c("read_share", "event_share", "read_minus_event"))
  bcd <- merge(bcd, unit_info, by = "unit", all.x = TRUE)
  bcd[, row_set := ifelse(resolution == "column", "column resolution (primary)", "R5 (house) at reader level (diagnostic)")]
  soc_colorder(bcd, c("Phase", "row_set", "resolution", "unit", "unit_readers", "resource_note",
                                 "n_cage_episodes", "n_animals", "n_animal_recordings", "event_share", "read_share", "read_minus_event"))
  lv <- c("pooled over animals (seconds)", "cage-episode spread", "by board", "by CC", "cage episode")
  out$basis_comparison_column <- soc_finish(bcd, meta, c("block_set", "Phase", "bound", "denominator", "resolution", "unit", "board", "CC", "CageEpisodeID"))
  out$basis_comparison_column <- out$basis_comparison_column[order(match(level, lv))]

  # ---- unsupported (unknown) time: shares by stratum, spread over cage episodes and animals (bounded unknown is the same
  # in both bases; the read basis is reported)
  uk <- merge(col_ab[basis == "read" & unit == "unknown", c(akey, "bound", "seconds", "observed_s", "supported_s"), with = FALSE],
              ab[, unique(c(akey, ekey, "Phase", "stage19_window")), with = FALSE], by = akey)[!CageEpisodeID %in% ex_ep]
  ut <- data.table::rbindlist(lapply(SOC_BLOCK_SETS, function(s) {
    x <- soc_in_set(uk, s)
    data.table::rbindlist(lapply(list(pooled = character(0), CC = "CC", board = "board", `CC x board` = c("CC", "board")), function(st) {
      by <- c("Phase", "bound", st)
      tot <- x[, .(n_cage_episodes = data.table::uniqueN(CageEpisodeID[observed_s > 0]), n_animals = data.table::uniqueN(paste(Batch, AnimalNum)[observed_s > 0]),
                   n_animal_recordings = data.table::uniqueN(paste(SourceFile, AnimalNum)[observed_s > 0]),
                   observed_s = sum(observed_s), supported_s = sum(supported_s), unknown_s = sum(seconds)), by = by]
      e <- x[, .(u = sum(seconds) / sum(observed_s)), by = c(by, "CageEpisodeID")][, soc_spread(u, "episode_unknown_share_of_observed_time_"), by = by]
      a <- x[, .(u = sum(seconds) / sum(observed_s)), by = c(by, "SourceFile", "AnimalNum")][, soc_spread(u, "animal_unknown_share_of_observed_time_"), by = by]
      r <- merge(merge(tot, e, by = by), a, by = by)
      if (!"CC" %in% names(r)) r[, CC := "all"]
      if (!"board" %in% names(r)) r[, board := "all"] else r[, board := as.character(board)]
      r[, level := if (length(st)) paste("by", paste(st, collapse = " x ")) else "pooled"]
    }), fill = TRUE)[, block_set := s]
  }))
  ut[, `:=`(unknown_share_of_observed_time = unknown_s / observed_s, supported_share_of_observed_time = supported_s / observed_s, basis = "read",
            basis_label = "single-read; the bounded unknown time is identical in the event basis (gate G06)")]
  soc_colorder(ut, c("Phase", "n_cage_episodes", "n_animals", "n_animal_recordings", "observed_s", "supported_s", "unknown_s",
                                "unknown_share_of_observed_time", "supported_share_of_observed_time"))
  out$unsupported_time_summary <- soc_finish(ut, meta, c("block_set", "level", "Phase", "bound", "CC", "board"))
  out$unsupported_time_summary <- out$unsupported_time_summary[order(match(level, c("pooled", "by CC", "by board", "by CC x board")))]

  # ---- column shares by bound (LOCF vs 300 vs 60), both denominators, pooled and by CC
  P_col_cc <- pool_by(c("Phase", "CC"), "column")
  cs <- rbind(data.table::copy(P_col)[, `:=`(level = "pooled", CC = "all")], data.table::copy(P_col_cc)[, level := "by CC"], use.names = TRUE)
  cs <- merge(cs, cs[bound == "LOCF", .(Phase, CC, block_set, basis, unit, a0 = share_all, s0 = share_supported)], by = c("Phase", "CC", "block_set", "basis", "unit"))
  cs[, `:=`(share_of_observed_time_minus_LOCF = share_all - a0, share_of_supported_time_minus_LOCF = share_supported - s0)][, c("a0", "s0") := NULL]
  data.table::setnames(cs, c("share_all", "share_supported", "observed_s", "supported_s"), c("share_of_observed_time", "share_of_supported_time", "observed_time_s", "supported_time_s"))
  cs <- merge(cs, unit_info, by = "unit", all.x = TRUE)
  cs <- merge(cs, rbind(cnt(c("Phase", "CC")), cnt("Phase")[, CC := "all"], use.names = TRUE), by = c("Phase", "CC", "block_set"), all.x = TRUE)
  soc_colorder(cs, c("Phase", "n_cage_episodes", "n_animals", "n_animal_recordings", "unit", "unit_readers", "resource_note", "seconds",
                                "observed_time_s", "supported_time_s", "share_of_observed_time", "share_of_supported_time"))
  out$column_shares_by_bound <- soc_finish(cs, meta, c("block_set", "level", "CC", "Phase", "basis", "bound", "unit"))
  out$column_shares_by_bound <- out$column_shares_by_bound[order(match(level, c("pooled", "by CC")))]

  # ---- observation gaps: gap classes, last reader (house association), grid against exact unknown time
  gk <- keep(gp)
  rl <- occ_ok[basis == "read", .(reader_locf_s = sum(seconds)), by = .(SourceFile, AnimalNum, block, PositionID, Phase, stage19_window)]
  og <- data.table::rbindlist(lapply(SOC_BLOCK_SETS, function(s) {
    x <- soc_in_set(gk, s); r <- soc_in_set(rl, s); o <- soc_in_set(ab_ok, s); u <- soc_in_set(uk, s)
    ob <- o[, .(observed_s = sum(observed_s)), by = Phase]
    tot <- x[, .(phase_unknown_s_grid = sum(unknown_s)), by = .(Phase, bound)]
    lvl <- function(by, name) {
      d <- x[, .(unknown_s_grid = sum(unknown_s)), by = c("Phase", "bound", by)]
      d <- merge(merge(d, tot, by = c("Phase", "bound")), ob, by = "Phase")
      d[, `:=`(share_of_phase_unknown_time = unknown_s_grid / phase_unknown_s_grid, share_of_observed_time = unknown_s_grid / observed_s, level = name)]
    }
    g1 <- lvl("gap_class", "gap class"); g2 <- lvl("last_reader", "last reader"); g3 <- lvl(c("last_reader", "gap_class"), "last reader x gap class")
    rr <- r[, .(reader_locf_s = sum(reader_locf_s)), by = .(Phase, last_reader = paste0("R", PositionID))]
    g2 <- merge(g2, rr, by = c("Phase", "last_reader"), all.x = TRUE)
    g2[, unknown_share_of_reader_locf_time := unknown_s_grid / reader_locf_s]
    ex <- u[bound != "LOCF", .(unknown_s_exact = sum(seconds)), by = .(Phase, bound)]
    g0 <- merge(merge(tot, ex, by = c("Phase", "bound")), ob, by = "Phase")
    g0[, `:=`(unknown_s_grid = phase_unknown_s_grid, grid_minus_exact_s = phase_unknown_s_grid - unknown_s_exact,
              share_of_observed_time = phase_unknown_s_grid / observed_s, share_of_phase_unknown_time = 1, level = "total")]
    rbind(g0, g1, g2, g3, fill = TRUE)[, block_set := s]
  }), fill = TRUE)
  og[, `:=`(last_reader_resource = ifelse(last_reader %in% L$reader, L$resource[match(last_reader, L$reader)], NA_character_), basis = "read",
            note = "1-s grid of the read basis; the reader last assigned is where the animal was last read, not where it was while unread")]
  data.table::setnames(og, "observed_s", "phase_observed_time_s")
  og <- merge(og, cnt("Phase"), by = c("Phase", "block_set"), all.x = TRUE)
  soc_colorder(og, c("Phase", "last_reader", "last_reader_resource", "gap_class", "unknown_s_grid",
                                "share_of_phase_unknown_time", "share_of_observed_time", "reader_locf_s", "unknown_share_of_reader_locf_time"))
  out$observation_gap_summary <- soc_finish(og, meta, c("block_set", "level", "Phase", "bound", "last_reader", "gap_class"))
  out$observation_gap_summary <- out$observation_gap_summary[order(match(level, c("total", "gap class", "last reader", "last reader x gap class")))]
  gd <- merge(gp[, .(g = sum(unknown_s)), by = c(akey, "bound")],
              col_ab[basis == "read" & unit == "unknown" & bound != "LOCF", c(akey, "bound", "seconds"), with = FALSE], by = c(akey, "bound"), all.y = TRUE)
  gd[is.na(g), g := 0]
  add("D02", "unknown time on the 1-s grid (gaps) against the exact unknown time, per animal-block", NA,
      sprintf("max |grid - exact| = %s s; median %s s", soc_num(max(abs(gd$g - gd$seconds))), soc_num(stats::median(abs(gd$g - gd$seconds)))),
      "grid cells are counted whole; about 10 s per 12-h block expected", type = "diagnostic")

  # ---- T10: candidate against the original Stage 19 shares, animal level, stage19_window
  s19 <- data.table::as.data.table(ref$stage19)
  mine19 <- occ[basis == "event" & stage19_window == TRUE, .(seconds = sum(seconds)), by = .(SourceFile, Phase = Phase_vendor, PhaseNumber, AnimalNum, PositionID)]
  par <- soc_parity_stage19(mine19, s19)
  add("G10", "event-basis LOCF on stage19_window reproduces Stage 19's phase table (reader seconds per animal-phase)",
      par$max_abs_diff_s < 1e-6 && par$only_here == 0 && par$only_stage19 == 0 &&
        (is.null(ref$parity_stage19) || (par$rows == ref$parity_stage19$rows && par$animal_phases == ref$parity_stage19$animal_phases)) &&
        (is.null(expected$counts$stage19_cells) || par$rows == expected$counts$stage19_cells),
      sprintf("%d cells, %d animal-phases, max |diff| %s s, only here %d, only Stage 19 %d", par$rows, par$animal_phases, soc_num(par$max_abs_diff_s),
              par$only_here, par$only_stage19), "max |diff| < 1e-6 s; same cells and animal-phases as srs parity_stage19.csv")
  s19b <- merge(s19, bl[, .(SourceFile, Phase = Phase_vendor, PhaseNumber, block, Phase_cand = Phase, block_no, stage19_window)],
                by = c("SourceFile", "Phase", "PhaseNumber"))
  s19o <- unique(s19b[, .(SourceFile, AnimalNum, block, observation_seconds)])
  s19oo <- merge(s19o, ab[, c(akey, "observed_s"), with = FALSE], by = akey)
  add("G11", "Stage 19 observation seconds = candidate observed time per animal-phase (stage19_window)",
      !anyDuplicated(s19o[, akey, with = FALSE]) && nrow(s19oo) == nrow(s19o) && max(abs(s19oo$observation_seconds - s19oo$observed_s)) < 1e-6,
      sprintf("%d animal-phases; max |diff| %s s", nrow(s19o), soc_num(max(abs(s19oo$observation_seconds - s19oo$observed_s)))), "< 1e-6 s")
  s19b[, `:=`(Phase = Phase_cand, column = amv_column(PositionID))]
  s19w <- function(d) {
    o <- unique(d[, .(SourceFile, AnimalNum, block, observation_seconds, Phase)])[, .(stage19_observation_s = sum(observation_seconds)), by = .(SourceFile, AnimalNum, Phase)]
    r <- d[, .(s = sum(PositionSeconds)), by = .(SourceFile, AnimalNum, Phase, unit = paste0("R", PositionID))]
    cc <- d[, .(s = sum(PositionSeconds)), by = .(SourceFile, AnimalNum, Phase, unit = paste0("C", column))]
    x <- merge(rbind(r[, resolution := "reader"], cc[, resolution := "column"]), o, by = c("SourceFile", "AnimalNum", "Phase"))
    x[, stage19_share := s / stage19_observation_s][, s := NULL][]
  }
  s19_pc <- s19w(s19b[stage19_window == TRUE])[, window := "phase_class"]
  s19_c1 <- s19w(s19b[block_no == 1L & SourceFile %in% am[CC == "CC1"]$SourceFile])[, window := "cc1_active1"]
  cand <- rbind(t3[block_set == "stage19_window", .(SourceFile, AnimalNum, Phase, window, basis, bound, unit, share_all, share_supported, observed_s, resolution = "column")],
                t3r[block_set == "stage19_window", .(SourceFile, AnimalNum, Phase, window, basis, bound, unit, share_all, share_supported, observed_s, resolution = "reader")])[unit != "unknown"]
  cw <- data.table::dcast(cand, SourceFile + AnimalNum + Phase + window + resolution + unit ~ basis + bound, value.var = c("share_all", "share_supported"))
  obsw <- unique(cand[, .(SourceFile, AnimalNum, Phase, window, observed_s)])
  t10 <- merge(merge(cw, obsw, by = c("SourceFile", "AnimalNum", "Phase", "window")), rbind(s19_pc, s19_c1),
               by = c("SourceFile", "AnimalNum", "Phase", "window", "resolution", "unit"), all = TRUE)
  t10[, `:=`(event_LOCF_minus_stage19 = share_all_event_LOCF - stage19_share, read_LOCF_minus_stage19 = share_all_read_LOCF - stage19_share,
             read_max60s_supported_minus_stage19 = share_supported_read_max60s - stage19_share,
             read_max300s_supported_minus_stage19 = share_supported_read_max300s - stage19_share,
             event_max60s_supported_minus_stage19 = share_supported_event_max60s - stage19_share)]
  # under LOCF both denominators coincide; keep one LOCF column per basis
  t10[, c("share_supported_event_LOCF", "share_supported_read_LOCF") := NULL]
  t10 <- merge(t10, am_cols, by = c("SourceFile", "AnimalNum"))
  t10[, `:=`(block_set = "stage19_window", basis = "event and read (wide columns)", bound = "LOCF, max300s, max60s (wide columns)",
             support_criterion = "share_all_* / share_supported_*: LOCF none; max300s observation age <= 300 s; max60s <= 60 s",
             stage19_source = "Stage 19 all_position_occupancy_by_phase.csv (keys and seconds only); share = PositionSeconds / observation_seconds, pooled by seconds")]
  add("G10b", "the candidate's event-basis LOCF shares equal Stage 19's shares (animal x window x unit)",
      max(abs(t10$event_LOCF_minus_stage19), na.rm = TRUE) < 1e-9 && !anyNA(t10$event_LOCF_minus_stage19),
      sprintf("%d rows; max |diff| %s", nrow(t10), soc_num(max(abs(t10$event_LOCF_minus_stage19), na.rm = TRUE))), "< 1e-9, no row without a partner")
  out$stage19_comparison_animal_window <- soc_finish(t10, meta, c("window", "SourceFile", "AnimalNum", "Phase", "resolution", "unit"))

  # ---- T11: exclusions and censoring per record
  rec <- data.table::as.data.table(expected$records)
  t11 <- data.table::rbindlist(lapply(seq_len(nrow(rec)), function(i) {
    r <- rec[i]; a <- abx[CageEpisodeID == r$CageEpisodeID]
    data.table::data.table(record_id = r$record_id, action = r$action, CageEpisodeID = r$CageEpisodeID, reader = r$reader, resource = r$resource,
                           from = r$from, animal_recordings = data.table::uniqueN(a$AnimalNum), animal_blocks = nrow(a),
                           animal_blocks_stage19_window = nrow(a[stage19_window == TRUE]), observed_s = sum(a$observed_s), censored_s = sum(a$censored_s),
                           censored_s_stage19_window = sum(a[stage19_window == TRUE]$censored_s),
                           effect_in_candidate = switch(r$action,
                             exclude_spatial = "kept in the animal tables (spatial_excluded = TRUE); left out of every summary, figure and pooled gate",
                             censor = "observed time ends at the censoring time (srs); censored seconds are neither occupancy nor unknown time",
                             exclude_movement = "no effect here: this candidate has no movement outcome (the episode is spatially excluded anyway)",
                             "unknown action"))
  }))
  t11 <- merge(t11, ep_cols[, .(CageEpisodeID, SourceFile, Batch, Sex, CC, board, System, n_animals_present, n_tags_present, n_tracked, censor_from)],
               by = "CageEpisodeID", all.x = TRUE)
  out$exclusions_censoring <- soc_finish(t11, meta, c("CageEpisodeID"))
  out$exclusions_censoring <- out$exclusions_censoring[order(record_id)]

  # ---- whole-span pooling against the srs release tables
  rcmp <- function(mine, srs, prefix) {
    s <- data.table::copy(data.table::as.data.table(srs))
    s[, `:=`(Phase = ifelse(Phase == "Inactive", "Light", Phase), unit = ifelse(unit == "unknown", "unknown", paste0(prefix, unit)))]
    m <- merge(mine[block_set == "whole_span"], s, by = c("Phase", "basis", "bound", "unit"), suffixes = c("", "_srs"))
    list(n = nrow(m), n_srs = nrow(s), ds = max(abs(m$seconds - m$s) / m$observed_s), da = max(abs(m$share_all - m$share_all_srs)),
         dsu = max(abs(m$share_supported - m$share_supported_srs), na.rm = TRUE), dobs = max(abs(m$observed_s - m$observed_s_srs)),
         dsup = max(abs(m$supported_s - m$supported_s_srs)))
  }
  P_ws_col <- soc_pool(occ_ok, ab_ok, "Phase", "column", sets = "whole_span")   # in memory only: the srs check
  P_ws_rd <- soc_pool(occ_ok, ab_ok, "Phase", "reader", sets = "whole_span")
  for (x in list(list("G12", "column", P_ws_col, ref$shares_unknown_column_pooled, "C"), list("G12b", "reader", P_ws_rd, ref$shares_unknown_reader_pooled, "R"))) {
    if (is.null(x[[4]])) next
    r <- rcmp(x[[3]], x[[4]], x[[5]])
    add(x[[1]], paste0("whole-span pooling (", x[[2]], "s, excluded episodes left out) reproduces srs shares_unknown_", x[[2]], "_pooled.csv"),
        r$n == r$n_srs && r$ds < 1e-9 && r$da < 1e-9 && r$dsu < 1e-9 && r$dobs < 1e-6 && r$dsup < 1e-6,
        sprintf("%d of %d rows matched; max |share_all| diff %s, |share_supported| %s, |seconds| / observed %s, |observed| %s s, |supported| %s s",
                r$n, r$n_srs, soc_num(r$da), soc_num(r$dsu), soc_num(r$ds), soc_num(r$dobs), soc_num(r$dsup)), "every row; shares < 1e-9; seconds within 1e-6 s")
  }
  sbp <- data.table::rbindlist(lapply("whole_span", function(s) {
    u <- soc_in_set(uk, s)
    u[, .(observed_h = sum(observed_s) / 3600, sup = sum(supported_s)), by = .(Phase, bound)][
      , .(observed_h = observed_h[1], supported_le60_share = sup[bound == "max60s"] / (observed_h[1] * 3600),
          supported_le300_share = sup[bound == "max300s"] / (observed_h[1] * 3600)), by = Phase]
  }))
  if (!is.null(ref$support_by_phase)) {
    sr <- data.table::copy(data.table::as.data.table(ref$support_by_phase))[, Phase := ifelse(Phase == "Inactive", "Light", Phase)]
    m <- merge(sbp, sr, by = "Phase", suffixes = c("", "_srs"))
    d <- max(abs(m$observed_h - m$observed_h_srs), abs(m$supported_le60_share - m$supported_le60_share_srs), abs(m$supported_le300_share - m$supported_le300_share_srs))
    add("G13", "whole-span support by phase reproduces srs support_by_phase.csv", nrow(m) == nrow(sr) && d < 1e-9,
        paste(sprintf("%s: %.1f h, <= 60 s %.4f, <= 300 s %.4f", m$Phase, m$observed_h, m$supported_le60_share, m$supported_le300_share), collapse = "; "),
        "observed hours and both support shares within 1e-9")
  }

  # ---- figure inputs (excluded episodes are not in any of them)
  fig$f1 <- out$column_shares_by_bound[basis == "read" & block_set == "stage19_window"]
  fig$f2 <- cmp_ep[comparison == "read - event" & resolution == "column" & block_set == "stage19_window" & unit != "unknown" &
                     ((bound == "LOCF" & denominator == SOC_DENOMINATOR[["share_all"]]) | (bound == "max60s" & denominator == SOC_DENOMINATOR[["share_supported"]]))]
  fm <- pool_by(c("Phase", "CC"), "reader")[block_set == "stage19_window"]
  fig$f3 <- merge(fm[basis == "read" & bound == "max60s" & unit != "unknown"], L[, .(unit = reader, ReaderX, ReaderY, reader_label)], by = "unit")
  fig$f3_unknown <- fm[basis == "read" & bound == "max60s" & unit == "unknown", .(Phase, CC, unknown_share_all = share_all)]
  fig$f4 <- out$support_animal_phase[spatial_excluded == FALSE]
  s19_ok <- s19b[stage19_window == TRUE & !paste(SourceFile, AnimalNum) %in% am[spatial_excluded == TRUE, paste(SourceFile, AnimalNum)]]
  s19_ok <- merge(s19_ok, am[, .(SourceFile, AnimalNum, CC)], by = c("SourceFile", "AnimalNum"))
  f5s <- s19_ok[, .(s = sum(PositionSeconds)), by = .(Phase, CC, unit = paste0("R", PositionID))]
  f5o <- unique(s19_ok[, .(SourceFile, AnimalNum, block, observation_seconds, Phase, CC)])[, .(o = sum(observation_seconds)), by = .(Phase, CC)]
  f5s <- merge(f5s, f5o, by = c("Phase", "CC"))[, .(Phase, CC, unit, share = s / o, series = "Stage 19 (= event LOCF), share of observed time")]
  fig$f5 <- rbind(f5s, fm[basis == "read" & bound == "LOCF" & unit != "unknown", .(Phase, CC, unit, share = share_all, series = "read LOCF, share of observed time")],
                  fm[basis == "read" & bound == "max60s" & unit != "unknown", .(Phase, CC, unit, share = share_supported, series = "read <= 60 s, share of supported time")])
  fig$f6 <- out$observation_gap_summary[level == "last reader x gap class" & block_set == "stage19_window"]
  fig$excluded_episodes <- ex_ep

  # ---- no group column in any output; no excluded episode in any summary or figure input
  bad_cols <- unique(unlist(lapply(out, function(d) intersect(names(d), SOC_FORBIDDEN_COLUMNS))))
  add("G15", "no group label or group-defining design key in any output table", !length(bad_cols),
      if (length(bad_cols)) paste(bad_cols, collapse = ", ") else paste(length(out), "tables checked"), paste("none of", paste(SOC_FORBIDDEN_COLUMNS, collapse = ", ")))
  animal_tables <- c("occupancy_animal_phase_column", "occupancy_animal_phase_reader", "occupancy_animal_window_column", "occupancy_animal_window_reader",
                     "support_animal_phase", "unknown_by_last_reader_gap", "organisation_animal_block", "organisation_animal_window",
                     "stage19_comparison_animal_window", "exclusions_censoring")
  leaks <- c(unlist(lapply(setdiff(names(out), animal_tables), function(nm) if ("CageEpisodeID" %in% names(out[[nm]]) && any(out[[nm]]$CageEpisodeID %in% ex_ep)) nm)),
             unlist(lapply(c("f2", "f4"), function(nm) if (any(fig[[nm]]$CageEpisodeID %in% ex_ep)) nm)))
  with_ep <- Filter(function(nm) "CageEpisodeID" %in% names(out[[nm]]), setdiff(names(out), animal_tables))
  add("G14", "spatially excluded cage episodes absent from the summary tables and figure inputs that carry CageEpisodeID", !length(leaks),
      if (length(leaks)) paste(leaks, collapse = ", ") else paste0(length(with_ep), " summary tables with CageEpisodeID (", paste(with_ep, collapse = ", "),
                                                                    ") and figure inputs f2, f4 checked"),
      "no excluded episode id; tables without CageEpisodeID are checked by G14b")
  # G14b: the observed time of every pooled summary and figure input equals the kept animal-blocks' observed time, so no
  # excluded episode contributes to a table that does not name episodes
  ko <- data.table::rbindlist(lapply(SOC_BLOCK_SETS, function(s) soc_in_set(ab_ok, s)[, .(o = sum(observed_s)), by = .(Phase, CC, board)][, block_set := s]))
  ko[, board := as.character(board)]
  den_ok <- function(d, col, by, label) {
    u <- unique(data.table::as.data.table(d)[, c(by, col), with = FALSE])
    ref <- ko[block_set %in% u$block_set, .(o = sum(o)), by = by]   # the block sets the input covers
    m <- merge(u, ref, by = by, all = TRUE)
    ok <- !anyDuplicated(u[, by, with = FALSE]) && !anyNA(m[[col]]) && !anyNA(m$o) && all(abs(m[[col]] - m$o) <= 1e-6 * pmax(1, m$o))
    if (ok) NULL else label
  }
  cs_t <- out$column_shares_by_bound; ut_t <- out$unsupported_time_summary; og_t <- out$observation_gap_summary
  den_bad <- c(den_ok(cs_t[level == "pooled"], "observed_time_s", c("block_set", "Phase"), "column_shares_by_bound (pooled)"),
               den_ok(cs_t[level == "by CC"], "observed_time_s", c("block_set", "Phase", "CC"), "column_shares_by_bound (by CC), F1"),
               den_ok(ut_t[level == "pooled"], "observed_s", c("block_set", "Phase"), "unsupported_time_summary (pooled)"),
               den_ok(ut_t[level == "by CC"], "observed_s", c("block_set", "Phase", "CC"), "unsupported_time_summary (by CC)"),
               den_ok(ut_t[level == "by board"], "observed_s", c("block_set", "Phase", "board"), "unsupported_time_summary (by board)"),
               den_ok(ut_t[level == "by CC x board"], "observed_s", c("block_set", "Phase", "CC", "board"), "unsupported_time_summary (by CC x board)"),
               den_ok(og_t, "phase_observed_time_s", c("block_set", "Phase"), "observation_gap_summary"),
               den_ok(fm[, block_set := "stage19_window"], "observed_s", c("block_set", "Phase", "CC"), "F3/F5 candidate series (stage19_window)"),
               den_ok(f5o[, `:=`(block_set = "stage19_window", observed_s = o)], "observed_s", c("block_set", "Phase", "CC"), "F5 Stage 19 series"))
  add("G14b", "the observed time of every pooled summary and of the F1/F3/F5 inputs equals that of the kept animal-blocks (no excluded episode contributes)",
      !length(den_bad), if (length(den_bad)) paste("differs:", paste(den_bad, collapse = "; ")) else "9 denominators checked by block_set x Phase (x CC, board)",
      "equal within 1e-6 relative")
  list(tables = out, figures = fig, gates = data.table::rbindlist(gates))
}
