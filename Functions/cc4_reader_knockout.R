# ================================================================
# CC4 reader knockout: how much a whole-session dead R3 or R5 moves the Stage 31-31d crossing outcomes
# MMMSociability -- Functions/cc4_reader_knockout.R
# ================================================================
# Used by Testing/audits/audit_cc4_reader_knockout.R (UNREGISTERED; isolated output root; group-blind; the design is
# declared in its header). Pure definitions: sourcing this file reads no data and writes no files. Needs data.table, dplyr
# and tidyr (attached by the caller), and from other helpers:
#   animalpos_preprocessing_helpers.R  animalpos_phase_block_index(), animalpos_position_id()
#   animalpos_reconstruction.R         apr_reads(), apr_positions(): the vendor position model, used unchanged
#   cc4_grid_exposure_helpers.R        grid31_phase_coverage(), grid31_clock_profiles() (Stage 31)
#   cc4_whole_inactive_helpers.R       grid31_whole_phase() (Stage 31 whole phases, as Stage 31b uses them)
#   antenna_spatial_reconstruction.R   asr_materiality(): the srs_v1.1 materiality rule
#
# Definitions
#   phases     the nine whole 12-h phases of Stage 31b: A1-A5 (18:30-06:30) and I2-I5 (06:30-18:30), numbered as Stage 31
#              numbers the CC4 recording (contiguous 12-h blocks from its first vendor row; I1 and I6 are partial)
#   record     a row of the vendor position stream (AnimalPos): one animal's estimate written at a time t
#   crossing   a record whose PositionID differs from the same animal's previous record, counted in the phase that holds
#              the later record; a previous record before the phase counts; the first record never counts (Stage 31)
#   eligible   Stage 31's EligiblePhase on the vendor export: the session's and the animal's first and last vendor rows
#              bracket the whole block (grid31_whole_phase()). Fixed from the export for every variant, so a knockout
#              never changes the units; whether a variant's own stream would bracket the block is kept as a diagnostic
#   variants   baseline: the vendor stream emulated from all of the animal's reads (all boards, as the vendor pools them);
#              ko_R<k>: the same with reader k's reads on the animal's own board deleted (its off-board reads kept), for
#              every animal of the recording at once (as srs_v1.1)
#   families   Active: reference A2, post A3-A5; Inactive: reference I2, post I3-I5 (Stage 31c/31d); A1 and I2 alone are
#              the Stage 29 windows (continuity with srs_v1.1)
#   outcomes   reference_rate = reference crossings / 12 h; post_rate = post crossings / 36 h; log_rate_ratio =
#              log((post + 0.5) / 36 h) - log((reference + 0.5) / 12 h); the three need all four phases of the family
#              eligible. rate_A1 / rate_I2 = crossings / 12 h, needing that phase eligible. Ineligible -> NA, never 0
#   shift      knockout minus baseline per animal and outcome; scale = residual SD (sd() of the residuals, as srs_v1.1)
#              of lm(baseline ~ Batch) over the healthy animals of that outcome; material when the median |shift| reaches
#              0.2 scale SD or its 90th percentile reaches 0.5 scale SD (srs_v1.1)
# The rates count thresholded displacement events of the vendor estimate (>= 200 grid units), not locomotion, and are
# not a proximity or occupancy measure.
# ================================================================

CKR_VERSION <- "ckr_v1.0"
CKR_READERS <- c(3L, 5L)                     # the readers that failed whole CC4 sessions (B2 board 5: R3; B5 board 1: R5)
CKR_PHASES <- c(paste0("A", 1:5), paste0("I", 2:5))
CKR_FAMILIES <- list(Active = list(reference = "A2", post = c("A3", "A4", "A5"), single = "A1"),
                     Inactive = list(reference = "I2", post = c("I3", "I4", "I5"), single = "I2"))
CKR_OUTCOMES <- c("reference_rate", "post_rate", "log_rate_ratio", "rate_A1", "rate_I2")
CKR_PHASE_HOURS <- 12
CKR_CONTINUITY <- 0.5                        # added to both counts of the log rate ratio
# Parity gate of the baseline emulation against Stage 31 (healthy board-recordings): a release run fails when fewer than
# 95% of the animal-phases match Stage 31's crossing count exactly, or the pooled count differs by more than 1%.
CKR_PARITY_MIN_EXACT_SHARE <- 0.95
CKR_PARITY_MAX_POOLED_REL_DIFF <- 0.01
CKR_STREAM_TOLERANCE_S <- 1.5e-3             # record-by-record comparison with the export (as srs_v1.1)
# Materiality (srs_v1.1, declared before its knockout and unchanged here)
CKR_MATERIAL_MEDIAN_SD <- 0.2
CKR_MATERIAL_P90_SD <- 0.5
CKR_MATERIALITY_RULE <- sprintf("material when median |shift| >= %.1f scale SD or 90th percentile of |shift| >= %.1f scale SD (srs_v1.1 rule)",
                                CKR_MATERIAL_MEDIAN_SD, CKR_MATERIAL_P90_SD)
CKR_SCALE_SOURCE <- paste("residual SD of lm(baseline ~ Batch) over the healthy CC4 animals of the outcome (group-blind; post-SIS values,",
                          "so not in the SD units of srs_v1.1, which used CC1 light windows and the frozen Stage 29 standardizers)")

#' Cage-episode key of a CC4 cage: "B2|sys.5|CC4".
ckr_cage_episode <- function(batch, system) paste(batch, system, "CC4", sep = "|")

#' Units of one recording. roster: Batch, System (sys.<board>), AnimalNum (one row per animal). excluded_episodes: the
#' cage-episode keys left out of the knockout (the caller passes the excluded CC4 episodes). Adds board, CageEpisodeID and
#' healthy (FALSE for an excluded episode).
ckr_units <- function(roster, excluded_episodes) {
  u <- data.table::copy(data.table::as.data.table(roster))
  if (!all(c("Batch", "System", "AnimalNum") %in% names(u))) stop("A roster needs Batch, System and AnimalNum.", call. = FALSE)
  if (anyNA(u$System) || !all(grepl("^sys[.][0-9]+$", u$System))) stop("System labels must read sys.<board>.", call. = FALSE)
  if (anyDuplicated(u[, .(Batch, AnimalNum)])) stop("An animal appears twice in the roster.", call. = FALSE)
  u[, `:=`(board = as.integer(sub("^sys[.]", "", System)), CageEpisodeID = ckr_cage_episode(Batch, System))]
  u[, healthy := !CageEpisodeID %in% excluded_episodes]
  u[]
}

#' Reads of one animal without reader k on its own board: rows with board == own_board and PositionID == reader are
#' deleted; reads at that reader on another board and every other reader are kept. reads: data.table (board, PositionID, ...).
ckr_knockout_reads <- function(reads, own_board, reader) {
  r <- data.table::as.data.table(reads)
  r[!(board == own_board & PositionID == reader)]
}

#' The variants of one animal's reads: baseline and ko_R<k> for each reader.
ckr_variant_reads <- function(reads, own_board, readers = CKR_READERS) {
  out <- list(baseline = data.table::as.data.table(reads))
  for (k in readers) out[[paste0("ko_R", k)]] <- ckr_knockout_reads(reads, own_board, k)
  out
}

#' Vendor position records of one tag's reads (t_ms sorted, gx, gy; apr_reads()), gate "vendor", with the first and the
#' closing record: data.table(t (s), PositionID, kind).
ckr_emulate <- function(reads) {
  if (!nrow(reads)) return(data.table::data.table(t = numeric(), PositionID = integer(), kind = character()))
  p <- apr_positions(reads$t_ms, reads$gx, reads$gy, gate = "vendor")
  data.table::data.table(t = p$t_ms / 1000, PositionID = as.integer(p$PositionID), kind = as.character(p$kind))
}

#' Records and crossings per phase block of one animal's record stream (t sorted, in s; pid its PositionIDs), by Stage
#' 31's rule. blocks: data.table(PhaseBlockIndex, PhaseLabel). Every block gets a row (zero when it holds no record).
ckr_phase_counts <- function(t, pid, blocks) {
  if (is.unsorted(t)) stop("Record times must be sorted.", call. = FALSE)
  n <- length(t)
  cross <- if (n > 1L) c(FALSE, pid[-1] != pid[-n]) else logical(n)
  x <- data.table::data.table(PhaseBlockIndex = as.numeric(animalpos_phase_block_index(t)), cross = cross)
  cnt <- x[, .(records = .N, crossings = sum(cross)), by = PhaseBlockIndex]
  out <- merge(data.table::as.data.table(blocks)[, .(PhaseBlockIndex = as.numeric(PhaseBlockIndex), PhaseLabel)], cnt,
               by = "PhaseBlockIndex", all.x = TRUE)
  out[is.na(records), `:=`(records = 0L, crossings = 0L)]
  out[, `:=`(records = as.integer(records), crossings = as.integer(crossings))]
  out[]
}

#' Stage 31's own crossing counts and eligibility of one CC4 recording, through Stage 31's functions: d = the vendor rows
#' as Stage 31 prepares them (preprocess_animalpos_file(phase_policy = "full_recording"), the cage-label corrections,
#' AnimalNum, Sex). Returns list(phases: one row per animal x nine phases with stage31_crossings, stage31_records and
#' eligible (EligiblePhase); blocks: PhaseBlockIndex, PhaseLabel, Phase, BlockStart, BlockEnd, SessionSpansWholeBlock).
ckr_stage31_phases <- function(d) {
  cov <- grid31_phase_coverage(d)
  prof <- grid31_clock_profiles(d, cov)
  bins <- dplyr::left_join(prof$animals, dplyr::distinct(d, AnimalID, AnimalNum, Sex), by = "AnimalID")
  wp <- dplyr::bind_rows(grid31_whole_phase(bins, "Active", 1:5, 2L)$animals, grid31_whole_phase(bins, "Inactive", 2:5, 2L)$animals)
  wp <- data.table::as.data.table(wp)[, .(Batch, System, AnimalID, AnimalNum, Sex, PhaseLabel, BlockStart, BlockEnd,
                                          stage31_crossings = as.integer(RecordedCrossings), eligible = EligiblePhase)]
  rc <- data.table::as.data.table(bins)[PhaseLabel %in% CKR_PHASES, .(stage31_records = as.integer(sum(ReadCount))), by = .(System, AnimalID, PhaseLabel)]
  wp <- merge(wp, rc, by = c("System", "AnimalID", "PhaseLabel"))
  blocks <- unique(data.table::as.data.table(cov)[PhaseLabel %in% CKR_PHASES,
                                                  .(PhaseBlockIndex = as.numeric(PhaseBlockIndex), PhaseLabel, Phase, BlockStart, BlockEnd, SessionSpansWholeBlock)])
  if (!setequal(blocks$PhaseLabel, CKR_PHASES) || anyDuplicated(blocks$PhaseLabel)) stop("The recording lacks one of the nine whole phases.", call. = FALSE)
  if (nrow(wp) != data.table::uniqueN(wp$AnimalNum) * length(CKR_PHASES)) stop("Stage 31 phase rows are incomplete.", call. = FALSE)
  list(phases = wp[], blocks = blocks[order(PhaseBlockIndex)])
}

#' One CC4 recording: Stage 31's counts (ckr_stage31_phases(d)) and, for every animal of d, the vendor stream emulated
#' from its raw reads in each variant, counted by the same rule. d: Stage 31's vendor rows (see ckr_stage31_phases());
#' events: rfid_read_recording(keep_read_offsets = TRUE)$events of the recording; tags: rfid_animalpos_tag_map() of its
#' export (AnimalNum, tag); excluded_episodes: cage-episode keys whose animals are flagged healthy = FALSE (they are
#' processed too, as context). Stops when an animal's System label is not the board holding most of its reads (the
#' knockout acts on that board). Returns list(animal_phase, animals, stage31 = ckr_stage31_phases(d)$phases).
ckr_process_recording <- function(d, events, tags, batch, excluded_episodes, readers = CKR_READERS, tolerance_s = CKR_STREAM_TOLERANCE_S) {
  ref <- ckr_stage31_phases(d)
  blocks <- ref$blocks
  dd <- data.table::as.data.table(d)
  roster <- unique(dd[, .(Batch, Sex, System, AnimalNum)])
  if (any(roster$Batch != batch)) stop("Vendor rows of another batch than ", batch, ".", call. = FALSE)
  roster <- ckr_units(roster, excluded_episodes)
  tg <- unique(data.table::as.data.table(tags)[AnimalNum %in% roster$AnimalNum, .(AnimalNum, tag)])
  miss <- setdiff(roster$AnimalNum, tg$AnimalNum)
  if (length(miss) || anyDuplicated(tg$AnimalNum)) stop(batch, ": no single RFID tag for ", paste(c(miss, tg$AnimalNum[duplicated(tg$AnimalNum)]), collapse = ", "), call. = FALSE)
  roster <- merge(roster, tg, by = "AnimalNum")
  rd <- data.table::as.data.table(apr_reads(events[events$tag %in% roster$tag, , drop = FALSE]))[, .(tag, t_ms, gx, gy, board, PositionID)]
  data.table::setkey(rd, tag)   # stable: the reads of a tag stay in time order
  vend <- dd[, .(AnimalNum, t = as.numeric(DateTime), PositionID = as.integer(PositionID))]
  data.table::setorder(vend, AnimalNum, t)
  variants <- c("baseline", paste0("ko_R", readers))
  counts <- list(); spans <- list(); anim <- list()
  for (i in seq_len(nrow(roster))) {
    a <- roster$AnimalNum[i]; own <- roster$board[i]
    ra <- rd[.(roster$tag[i]), nomatch = NULL]
    tb <- tabulate(ra$board, nbins = max(c(ra$board, own, 1L)))
    modal <- if (nrow(ra)) which.max(tb) else NA_integer_
    if (!identical(as.integer(modal), as.integer(own)))
      stop(batch, " ", a, ": the System label (board ", own, ") is not the board holding most of its reads (", modal, ").", call. = FALSE)
    vs <- ckr_variant_reads(ra, own, readers)
    for (v in variants) {
      rec <- ckr_emulate(vs[[v]])
      counts[[length(counts) + 1L]] <- ckr_phase_counts(rec$t, rec$PositionID, blocks)[, `:=`(variant = v, AnimalNum = a)]
      spans[[length(spans) + 1L]] <- data.table::data.table(variant = v, AnimalNum = a, first_t = if (nrow(rec)) rec$t[1] else NA_real_,
                                                            last_t = if (nrow(rec)) rec$t[nrow(rec)] else NA_real_, records_total = nrow(rec))
      if (v == "baseline") {
        vr <- vend[AnimalNum == a]
        same_n <- nrow(vr) == nrow(rec)
        dt <- if (same_n && nrow(vr)) max(abs(vr$t - rec$t)) else NA_real_
        stream <- data.table::data.table(vendor_records = nrow(vr), emulated_records = nrow(rec), stream_max_abs_dt_s = dt,
                                         stream_identical = same_n && all(vr$PositionID == rec$PositionID) && (!nrow(vr) || dt <= tolerance_s))
      }
    }
    an <- data.table::data.table(AnimalNum = a, tag = roster$tag[i], reads = nrow(ra), reads_on_board = sum(ra$board == own),
                                 reads_off_board = sum(ra$board != own), modal_read_board = as.integer(modal))
    for (k in readers) data.table::set(an, j = paste0("reads_R", k, "_on_board"), value = sum(ra$board == own & ra$PositionID == k))
    anim[[i]] <- cbind(an, stream)
  }
  keys <- roster[, .(AnimalNum, Batch, Sex, board, System, CageEpisodeID, healthy)]
  sp <- data.table::rbindlist(spans)
  sess <- sp[, .(s_first = suppressWarnings(min(first_t, na.rm = TRUE)), s_last = suppressWarnings(max(last_t, na.rm = TRUE))), by = variant]
  ap <- merge(data.table::rbindlist(counts), sp[, .(variant, AnimalNum, first_t, last_t)], by = c("variant", "AnimalNum"))
  ap <- merge(ap, sess, by = "variant")
  ap <- merge(ap, blocks[, .(PhaseBlockIndex, Phase, bs = as.numeric(BlockStart), be = as.numeric(BlockEnd))], by = "PhaseBlockIndex")
  # would this variant's own stream bracket the block (Stage 31's rule on the emulated records)? Diagnostic only
  ap[, brackets_variant := !is.na(first_t) & s_first <= bs & s_last >= be & first_t <= bs & last_t >= be]
  ap <- merge(ap, ref$phases[, .(AnimalNum, PhaseLabel, BlockStart, BlockEnd, eligible, stage31_crossings, stage31_records)],
              by = c("AnimalNum", "PhaseLabel"))
  ap <- merge(ap, keys, by = "AnimalNum")
  ap[, `:=`(CC = "CC4", hours = ifelse(eligible, CKR_PHASE_HOURS, NA_real_))]
  ap[, rate := crossings / hours]
  ap[, variant := factor(variant, levels = variants)]
  data.table::setorder(ap, variant, CageEpisodeID, AnimalNum, PhaseBlockIndex)
  ap[, variant := as.character(variant)]
  ap <- ap[, .(variant, Batch, Sex, CC, board, System, CageEpisodeID, AnimalNum, healthy, PhaseLabel, Phase, BlockStart, BlockEnd,
               crossings, records, eligible, hours, rate, brackets_variant, stage31_crossings, stage31_records)]
  animals <- merge(keys, data.table::rbindlist(anim), by = "AnimalNum")[, CC := "CC4"]
  data.table::setcolorder(animals, c("Batch", "Sex", "CC", "board", "System", "CageEpisodeID", "AnimalNum", "healthy"))
  list(animal_phase = ap[], animals = animals[order(CageEpisodeID, AnimalNum)], stage31 = ref$phases)
}

#' Family outcomes per variant x animal (ap: animal_phase rows). One row per variant x animal x family: the reference
#' and post counts and hours, reference_rate, post_rate, log_rate_ratio (all NA unless every phase of the family is
#' eligible) and the single Stage 29 window phase (A1 or I2) with its rate (NA unless that phase is eligible).
ckr_family_outcomes <- function(ap, families = CKR_FAMILIES, continuity = CKR_CONTINUITY, phase_hours = CKR_PHASE_HOURS) {
  ap <- data.table::as.data.table(ap)
  keys <- intersect(c("variant", "Batch", "Sex", "board", "System", "CageEpisodeID", "AnimalNum", "healthy"), names(ap))
  data.table::rbindlist(lapply(names(families), function(fam) {
    f <- families[[fam]]
    ap[, {
      ir <- match(f$reference, PhaseLabel); ip <- match(f$post, PhaseLabel); is1 <- match(f$single, PhaseLabel)
      complete <- !anyNA(c(ir, ip)) && all(eligible[c(ir, ip)])
      rc <- crossings[ir]; pc <- sum(crossings[ip])
      rh <- if (complete) phase_hours else NA_real_
      ph <- if (complete) phase_hours * length(f$post) else NA_real_
      s_ok <- !is.na(is1) && isTRUE(eligible[is1])
      list(family = fam, reference_phase = f$reference, post_phases = paste(f$post, collapse = ";"), family_complete = complete,
           reference_count = rc, reference_hours = rh, post_count = pc, post_hours = ph,
           reference_rate = rc / rh, post_rate = pc / ph,
           log_rate_ratio = log((pc + continuity) / ph) - log((rc + continuity) / rh),
           single_phase = f$single, single_eligible = s_ok, single_count = crossings[is1],
           single_rate = if (s_ok) crossings[is1] / phase_hours else NA_real_)
    }, by = keys]
  }))
}

#' Long form of the family outcomes: one row per variant x animal x family x outcome (CKR_OUTCOMES; the single phase as
#' rate_A1 or rate_I2), with its value.
ckr_outcome_long <- function(af) {
  af <- data.table::as.data.table(af)
  keys <- intersect(c("variant", "Batch", "Sex", "board", "System", "CageEpisodeID", "AnimalNum", "healthy", "family"), names(af))
  out <- data.table::rbindlist(list(
    af[, c(.SD, list(outcome = "reference_rate", value = reference_rate)), .SDcols = keys],
    af[, c(.SD, list(outcome = "post_rate", value = post_rate)), .SDcols = keys],
    af[, c(.SD, list(outcome = "log_rate_ratio", value = log_rate_ratio)), .SDcols = keys],
    af[, c(.SD, list(outcome = paste0("rate_", single_phase), value = single_rate)), .SDcols = keys]))
  out[]
}

#' Knockout shifts of the healthy animals: one row per reader x family x outcome x animal with baseline, knockout and
#' shift = knockout - baseline; only animals with a finite value in both.
ckr_shifts <- function(af, readers = CKR_READERS) {
  L <- ckr_outcome_long(af)[healthy == TRUE]
  w <- data.table::dcast(L, Batch + board + CageEpisodeID + AnimalNum + family + outcome ~ variant, value.var = "value")
  data.table::rbindlist(lapply(readers, function(k) {
    v <- paste0("ko_R", k)
    if (!v %in% names(w)) stop("No ", v, " variant in the family outcomes.", call. = FALSE)
    x <- w[is.finite(baseline) & is.finite(get(v))]
    x[, .(reader = as.integer(k), family, outcome, Batch, board, CageEpisodeID, AnimalNum, baseline, knockout = get(v), shift = get(v) - baseline)]
  }))
}

#' Scale of one outcome: sd() of the residuals of lm(y ~ Batch) (the formula of srs_v1.1's group-blind light-window scale,
#' applied here to the post-SIS CC4 baselines; srs used CC1 baseline light windows and, for the active window, the frozen
#' Stage 29 standardizers, so SD units are not comparable between the two audits); with one
#' batch (dev subsets) the SD of y, labelled.
ckr_scale <- function(y, batch) {
  ok <- is.finite(y); y <- y[ok]; batch <- as.character(batch[ok])
  if (length(y) < 2L) return(list(scale_sd = NA_real_, scale_source = "fewer than two animals"))
  if (length(unique(batch)) >= 2L) {
    return(list(scale_sd = stats::sd(stats::residuals(stats::lm(y ~ factor(batch)))), scale_source = CKR_SCALE_SOURCE))
  }
  list(scale_sd = stats::sd(y), scale_source = "SD of the baseline outcome (one batch only: a dev subset, not the declared scale)")
}

#' Scales per family x outcome from the baseline values of the shift table (each animal once).
ckr_scales <- function(sh) {
  b <- unique(data.table::as.data.table(sh)[, .(family, outcome, Batch, AnimalNum, baseline)])
  if (anyDuplicated(b[, .(family, outcome, Batch, AnimalNum)])) stop("An animal has two baseline values for one outcome.", call. = FALSE)
  b[, { s <- ckr_scale(baseline, Batch)
        list(n_animals = .N, n_batches = data.table::uniqueN(Batch), scale_sd = s$scale_sd, scale_source = s$scale_source) },
    by = .(family, outcome)]
}

#' Materiality per reader x family x outcome (asr_materiality(), the srs_v1.1 rule) with exactly the columns reader,
#' family, outcome, n_animals, n_board_recordings, scale_sd, median_shift, median_abs_shift_sd, p90_abs_shift_sd, material,
#' rule.
ckr_materiality <- function(sh, scales) {
  m <- merge(data.table::as.data.table(sh), data.table::as.data.table(scales)[, .(family, outcome, scale_sd)], by = c("family", "outcome"))
  out <- m[, { r <- asr_materiality(shift, scale_sd[1], CKR_MATERIAL_MEDIAN_SD, CKR_MATERIAL_P90_SD)
               list(n_animals = as.integer(r$n), n_board_recordings = data.table::uniqueN(CageEpisodeID[is.finite(shift)]), scale_sd = scale_sd[1],
                    median_shift = r$median_shift, median_abs_shift_sd = r$median_abs_in_sd, p90_abs_shift_sd = r$p90_abs_in_sd,
                    material = as.logical(r$material), rule = CKR_MATERIALITY_RULE) }, by = .(reader, family, outcome)]
  out[, reader := as.integer(reader)]
  out <- out[order(reader, match(family, names(CKR_FAMILIES)), match(outcome, CKR_OUTCOMES))]
  out[, .(reader, family, outcome, n_animals, n_board_recordings, scale_sd, median_shift, median_abs_shift_sd, p90_abs_shift_sd, material, rule)]
}

#' TRUE when a comparison passes the parity gate: exact share >= min_exact and pooled relative difference <= max_rel.
ckr_parity_pass <- function(exact_share, pooled_rel_diff, min_exact = CKR_PARITY_MIN_EXACT_SHARE, max_rel = CKR_PARITY_MAX_POOLED_REL_DIFF) {
  is.finite(exact_share) & is.finite(pooled_rel_diff) & exact_share >= min_exact & pooled_rel_diff <= max_rel
}

#' Parity row of the baseline emulation against Stage 31 (ap: animal_phase rows, any variants; the baseline rows are
#' compared): exact share of animal-phases with equal crossings, pooled relative difference, records and eligibility.
#' Without any animal-phase, pass is NA (a gated comparison with no units never passes).
ckr_parity <- function(ap, comparison, gated, min_exact = CKR_PARITY_MIN_EXACT_SHARE, max_rel = CKR_PARITY_MAX_POOLED_REL_DIFF) {
  b <- data.table::as.data.table(ap)[variant == "baseline"]
  ref <- sum(b$stage31_crossings); emu <- sum(b$crossings)
  share <- if (nrow(b)) sum(b$crossings == b$stage31_crossings) / nrow(b) else NA_real_
  rel <- if (ref > 0) abs(emu - ref) / ref else if (emu == 0) 0 else Inf
  data.table::data.table(comparison = comparison, units = "animal-phases", n = nrow(b), exact_matches = sum(b$crossings == b$stage31_crossings),
                         exact_share = share, total_reference = ref, total_compared = emu, pooled_rel_diff = rel,
                         max_abs_diff = if (nrow(b)) max(abs(b$crossings - b$stage31_crossings)) else NA_real_,
                         records_matches = sum(b$records == b$stage31_records), eligibility_matches = sum(b$brackets_variant == b$eligible),
                         gated = gated, threshold_min_exact_share = min_exact, threshold_max_pooled_rel_diff = max_rel,
                         pass = if (nrow(b)) ckr_parity_pass(share, rel, min_exact, max_rel) else NA)
}

#' Record-by-record parity of the baseline streams with the vendor export (animals table): not gated.
ckr_stream_parity <- function(animals, comparison) {
  a <- data.table::as.data.table(animals)
  ref <- sum(a$vendor_records); emu <- sum(a$emulated_records)
  share <- if (nrow(a)) sum(a$stream_identical) / nrow(a) else NA_real_
  rel <- if (ref > 0) abs(emu - ref) / ref else NA_real_
  data.table::data.table(comparison = comparison, units = "animals", n = nrow(a), exact_matches = sum(a$stream_identical), exact_share = share,
                         total_reference = ref, total_compared = emu, pooled_rel_diff = rel,
                         max_abs_diff = if (nrow(a)) max(abs(a$vendor_records - a$emulated_records)) else NA_real_,
                         records_matches = sum(a$vendor_records == a$emulated_records), eligibility_matches = NA_integer_, gated = FALSE,
                         threshold_min_exact_share = NA_real_, threshold_max_pooled_rel_diff = NA_real_, pass = NA)
}

#' Stage 31 reference (ckr_stage31_phases()$phases of the recordings) against a frozen Stage 31 run's animal 5-min bins
#' (Batch, System, AnimalID, PhaseLabel, Crossings, ReadCount, SessionSpansWholeBlock, AnimalSpansWholeBlock,
#' SpanOverlapSec): crossings, records and eligibility must be equal for every animal-phase (gated, exact).
ckr_stage31_live_compare <- function(ref_phases, live_bins, comparison) {
  lv <- data.table::as.data.table(live_bins)[PhaseLabel %in% CKR_PHASES,
    .(live_crossings = sum(Crossings), live_records = sum(ReadCount),
      live_eligible = all(SessionSpansWholeBlock & AnimalSpansWholeBlock & SpanOverlapSec >= 300)), by = .(Batch, System, AnimalID, PhaseLabel)]
  m <- merge(data.table::as.data.table(ref_phases)[, .(Batch, System, AnimalID, PhaseLabel, stage31_crossings, stage31_records, eligible)],
             lv, by = c("Batch", "System", "AnimalID", "PhaseLabel"), all = TRUE)
  both <- m[!is.na(stage31_crossings) & !is.na(live_crossings)]
  ref <- sum(both$live_crossings); cmp <- sum(both$stage31_crossings)
  share <- if (nrow(m)) sum(both$stage31_crossings == both$live_crossings) / nrow(m) else NA_real_
  rel <- if (ref > 0) abs(cmp - ref) / ref else if (cmp == 0) 0 else Inf
  ok <- nrow(m) > 0 && nrow(both) == nrow(m) && all(both$stage31_crossings == both$live_crossings) &&
    all(both$stage31_records == both$live_records) && all(both$eligible == both$live_eligible)
  data.table::data.table(comparison = comparison, units = "animal-phases", n = nrow(m), exact_matches = sum(both$stage31_crossings == both$live_crossings),
                         exact_share = share, total_reference = ref, total_compared = cmp, pooled_rel_diff = rel,
                         max_abs_diff = if (nrow(both)) max(abs(both$stage31_crossings - both$live_crossings)) else NA_real_,
                         records_matches = sum(both$stage31_records == both$live_records), eligibility_matches = sum(both$eligible == both$live_eligible),
                         gated = TRUE, threshold_min_exact_share = 1, threshold_max_pooled_rel_diff = 0, pass = ok)
}

#' Continuity with srs_v1.1 (not gated): rate_A1 and rate_I2 here against the srs knockout table's CC4 crossing rates in
#' the Stage 29 active and light windows (AnimalNum, Batch, CC, window, crossing_rate_baseline, crossing_rate_ko_R<k>),
#' per reader: animals compared, largest baseline and shift differences, median shifts and the correlation of the shifts.
ckr_srs_continuity <- function(sh, srs, readers = CKR_READERS) {
  s <- data.table::as.data.table(srs)[CC == "CC4"]
  s[, outcome := c(active = "rate_A1", light = "rate_I2")[window]]
  data.table::rbindlist(lapply(readers, function(k) {
    x <- data.table::as.data.table(sh)[reader == k & outcome %in% c("rate_A1", "rate_I2"), .(outcome, Batch, AnimalNum, baseline, shift)]
    y <- s[!is.na(outcome), .(outcome, Batch, AnimalNum, srs_baseline = crossing_rate_baseline,
                              srs_shift = get(paste0("crossing_rate_ko_R", k)) - crossing_rate_baseline)]
    m <- merge(x, y, by = c("outcome", "Batch", "AnimalNum"), all = TRUE)
    m[, { both <- !is.na(baseline) & !is.na(srs_baseline)
          list(reader = as.integer(k), n_here = sum(!is.na(baseline)), n_srs = sum(!is.na(srs_baseline)), n_both = sum(both),
               max_abs_diff_baseline = if (any(both)) max(abs(baseline[both] - srs_baseline[both])) else NA_real_,
               max_abs_diff_shift = if (any(both)) max(abs(shift[both] - srs_shift[both])) else NA_real_,
               median_shift_here = stats::median(shift[both]), median_shift_srs = stats::median(srs_shift[both]),
               shift_correlation = if (sum(both) > 2) suppressWarnings(stats::cor(shift[both], srs_shift[both])) else NA_real_) },
      by = outcome][, .(reader, outcome, n_here, n_srs, n_both, max_abs_diff_baseline, max_abs_diff_shift, median_shift_here, median_shift_srs,
                        shift_correlation)]
  }))
}
