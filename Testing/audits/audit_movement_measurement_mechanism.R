# ================================================================
# Audit: how the RFID processing variants measure movement (averaging x displacement threshold), CC1 first dark phase
# MMMSociability -- Testing/audits/audit_movement_measurement_mechanism.R
# ================================================================
# UNREGISTERED methodological sensitivity analysis (decided 2026-10-13); descriptive only, no tests of group effects.
# Inputs (read only), one per processing variant:
#   noflip  raw antenna changes, no averaging, no threshold            RANU, data version raw_antenna_noflip_20261012
#   ge200   no averaging, >= 200-unit threshold                        RANV, raw_antenna_ge200_20261013
#   avg500  500-ms window mean, no threshold                           RANA, raw_antenna_avg500_20261013
#   vendor  500-ms window mean, >= 200-unit threshold (AnimalPos)      RFID, v2 preprocessed files
#   flip2s  raw antenna changes with the 2-s flip filter (reference)   RANT, raw_antenna_flip2s_20261012
# For every CC1 animal in the Stage 09 window (the first dark phase after CC1, 18:30-06:30): the number of position
# transitions (changes of reader cell in the stream, as Stage 01 counts them) and their rate per hour; short reversals
# A -> B -> A whose visit to B lasts < 2 s or < 10 s; the Stage 09 feature Movement_mean (model_ladder_input.csv);
# Spearman correlations between variants; distributions by group, sex, batch and cage; for avg500 the estimator's own
# counts (build_audit.csv). Writes <Behavior>/RANT/comparison_vendor_raw/averaging_vs_threshold/ (MMM_MECH_AUDIT_OUT).
# Run from the repository root:  Rscript Testing/audits/audit_movement_measurement_mechanism.R
# ================================================================

suppressPackageStartupMessages({ library(data.table) })

MECH_VARIANTS <- data.table(
  variant = c("noflip", "ge200", "avg500", "vendor", "flip2s"),
  averaging = c("none", "none", "500 ms", "500 ms", "none (2-s flip filter)"),
  threshold = c("none", ">= 200", "none", ">= 200", "none"),
  root = c("RANU", "RANV", "RANA", "RFID", "RANT"),
  stream = c("RANU/MMMSociability/data_versions/raw_antenna_noflip_20261012/preprocessed_data",
             "RANV/MMMSociability/data_versions/raw_antenna_ge200_20261013/preprocessed_data",
             "RANA/MMMSociability/data_versions/raw_antenna_avg500_20261013/preprocessed_data",
             "RFID/MMMSociability/data_versions/v2_cage_label_correction_2026-09-28/preprocessed_data",
             "RANT/MMMSociability/data_versions/raw_antenna_flip2s_20261012/preprocessed_data"))

# Transitions and short reversals of one animal's stream inside [ws, we): t (s, sorted), pos (cell); the cell held at ws
# is the last row at or before ws.
mech_animal <- function(t, pos, ws, we) {
  held <- if (any(t <= ws)) pos[max(which(t <= ws))] else NA_integer_
  inside <- t > ws & t < we
  tt <- c(ws, t[inside]); pp <- c(held, pos[inside])
  keep <- !is.na(pp); tt <- tt[keep]; pp <- pp[keep]
  ch <- c(FALSE, pp[-1] != pp[-length(pp)])
  tc <- tt[ch]; pc <- pp[ch]; prev <- pp[which(ch) - 1L]
  n <- length(tc)
  # A -> B (at tc[k]) -> back to A (at tc[k+1]): the visit to B lasts tc[k+1] - tc[k]
  rev_dur <- if (n >= 2) { back <- pc[-1] == prev[-n]; (tc[-1] - tc[-n])[back] } else numeric(0)
  dwell <- diff(c(tc, we))
  data.table(transitions = n, rate_per_h = n / ((we - ws) / 3600), reversals_lt2s = sum(rev_dur < 2), reversals_lt10s = sum(rev_dur < 10),
             reversal_dur_median_s = if (length(rev_dur)) median(rev_dur) else NA_real_, dwell_median_s = if (n) median(dwell) else NA_real_)
}

if (sys.nframe() == 0L) {
  data.table::setDTthreads(1L)
  source("Functions/project_paths.R")
  BEH <- dirname(MMM_PROJECT_ROOT_DEFAULT)
  OUT <- Sys.getenv("MMM_MECH_AUDIT_OUT", unset = file.path(BEH, "RANT", "comparison_vendor_raw", "averaging_vs_threshold"))
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  hx <- parse("Functions/behavioral_dynamics_helpers.R", keep.source = TRUE)
  htxt <- vapply(attr(hx, "srcref"), function(s) paste(as.character(s), collapse = "\n"), "")
  canon <- eval(hx[[which(startsWith(htxt, "canonical_animal_id <- function(x)"))]][[3]], globalenv())
  cz <- fread(file.path(MMM_PROJECT_ROOT_DEFAULT, "analysis_ready/canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv"),
              colClasses = c(AnimalNum = "character"))[, .(AnimalNum = canon(AnimalNum), Group = outcome_group, Sex)]

  per <- rbindlist(lapply(seq_len(nrow(MECH_VARIANTS)), function(i) {
    v <- MECH_VARIANTS[i]
    files <- list.files(file.path(BEH, v$stream), pattern = "_CC1_AnimalPos_preprocessed[.]csv$", full.names = TRUE)
    rbindlist(lapply(files, function(f) {
      d <- fread(f, select = c("DateTime", "AnimalID", "System", "PositionID"), colClasses = c(AnimalID = "character"))
      d[, t := as.numeric(as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))]
      ws <- as.numeric(as.POSIXct(paste(format(as.POSIXct(min(d$t), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = "UTC"), "18:30:00"), tz = "UTC"))
      setorder(d, AnimalID, t)
      d[, c(list(System = System[1]), mech_animal(t, PositionID, ws, ws + 12 * 3600)), by = AnimalID][
        , `:=`(variant = v$variant, Batch = sub("^E9_SIS_(B[0-9]+)_.*$", "\\1", basename(f)), AnimalNum = canon(AnimalID))]
    }))
  }))
  feat <- rbindlist(lapply(seq_len(nrow(MECH_VARIANTS)), function(i) {
    v <- MECH_VARIANTS[i]
    x <- fread(file.path(BEH, v$root, "analysis_ready/pipeline/09_early_prediction/10min/tables/model_ladder_input.csv"),
               select = c("AnimalNum", "Movement_mean", "Movement_rmssd", "Entropy_acf1"), colClasses = c(AnimalNum = "character"))
    x[, `:=`(AnimalNum = canon(AnimalNum), variant = v$variant)][]
  }))
  per <- merge(merge(per, feat, by = c("variant", "AnimalNum"), all.x = TRUE), cz, by = "AnimalNum", all.x = TRUE)
  per <- merge(per, MECH_VARIANTS[, .(variant, averaging, threshold)], by = "variant")
  fwrite(per, file.path(OUT, "per_animal.csv"))

  summ <- per[, .(animals = .N, transitions_total = sum(transitions), rate_per_h_median = median(rate_per_h),
                  rate_per_h_q25 = quantile(rate_per_h, 0.25), rate_per_h_q75 = quantile(rate_per_h, 0.75),
                  reversals_lt2s_per_h = sum(reversals_lt2s) / (.N * 12), reversals_lt10s_per_h = sum(reversals_lt10s) / (.N * 12),
                  share_transitions_in_lt2s_reversals = 2 * sum(reversals_lt2s) / sum(transitions),
                  reversal_dur_median_s = median(reversal_dur_median_s, na.rm = TRUE), dwell_median_s = median(dwell_median_s, na.rm = TRUE),
                  Movement_mean_median = median(Movement_mean, na.rm = TRUE)), by = .(variant, averaging, threshold)]
  fwrite(summ, file.path(OUT, "summary_by_variant.csv"))

  w <- dcast(per, AnimalNum ~ variant, value.var = "Movement_mean")
  wt <- dcast(per, AnimalNum ~ variant, value.var = "rate_per_h")
  vv <- MECH_VARIANTS$variant
  cors <- rbindlist(lapply(vv, function(a) rbindlist(lapply(vv, function(b) if (a < b) data.table(
    a = a, b = b, n = sum(complete.cases(w[[a]], w[[b]])),
    spearman_Movement_mean = cor(w[[a]], w[[b]], method = "spearman", use = "complete.obs"),
    pearson_Movement_mean = cor(w[[a]], w[[b]], use = "complete.obs"),
    spearman_rate_per_h = cor(wt[[a]], wt[[b]], method = "spearman", use = "complete.obs"),
    mean_abs_rank_shift = mean(abs(rank(w[[a]]) - rank(w[[b]])), na.rm = TRUE))))))
  fwrite(cors, file.path(OUT, "correlations.csv"))

  dist <- rbindlist(list(
    per[, .(by = "Group x Sex", level = paste(Group, Sex), n = .N, Movement_mean_median = median(Movement_mean, na.rm = TRUE),
            q25 = quantile(Movement_mean, 0.25, na.rm = TRUE), q75 = quantile(Movement_mean, 0.75, na.rm = TRUE), rate_per_h_median = median(rate_per_h)), by = variant],
    per[, .(by = "Batch", level = Batch, n = .N, Movement_mean_median = median(Movement_mean, na.rm = TRUE),
            q25 = quantile(Movement_mean, 0.25, na.rm = TRUE), q75 = quantile(Movement_mean, 0.75, na.rm = TRUE), rate_per_h_median = median(rate_per_h)), by = variant],
    per[, .(by = "Cage (Batch System)", level = paste(Batch, System), n = .N, Movement_mean_median = median(Movement_mean, na.rm = TRUE),
            q25 = quantile(Movement_mean, 0.25, na.rm = TRUE), q75 = quantile(Movement_mean, 0.75, na.rm = TRUE), rate_per_h_median = median(rate_per_h)), by = variant]))
  fwrite(dist, file.path(OUT, "distributions.csv"))
  # Share of the animal-level variance of Movement_mean between batches and between cages (descriptive, by variant)
  part <- per[!is.na(Movement_mean), {
    tot <- var(Movement_mean); b <- tapply(Movement_mean, Batch, mean); cg <- tapply(Movement_mean, paste(Batch, System), mean)
    .(between_batch_share = var(b[Batch]) / tot, between_cage_share = var(cg[paste(Batch, System)]) / tot) }, by = variant]
  fwrite(part, file.path(OUT, "variance_shares.csv"))

  au <- fread(file.path(BEH, "RANA/MMMSociability/data_versions/raw_antenna_avg500_20261013/build_audit.csv"))
  est <- au[, .(animal_windows = .N, reads_in_span = sum(reads_in_span), evaluations_in_span = sum(evaluations_in_span),
                mean_changes_in_span = sum(mean_changes_in_span), cell_changes_in_span = sum(cell_changes_in_span),
                cell_changes_to_unread_cell = sum(cell_changes_to_unread_cell), reads_off_board = sum(reads_off_board))]
  est[, `:=`(evaluations_per_read = evaluations_in_span / reads_in_span, cell_changes_per_mean_change = cell_changes_in_span / mean_changes_in_span,
             unread_cell_share = cell_changes_to_unread_cell / cell_changes_in_span)]
  fwrite(est, file.path(OUT, "avg500_estimator_counts.csv"))
  fwrite(data.frame(item = c("status", "git_commit", "script_sha256", "finished"),
                    value = c("UNREGISTERED methodological sensitivity; descriptive", system2("git", c("-C", getwd(), "rev-parse", "HEAD"), stdout = TRUE)[1],
                              digest::digest(file = "Testing/audits/audit_movement_measurement_mechanism.R", algo = "sha256"),
                              format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), file.path(OUT, "provenance.csv"))
  options(width = 220)
  print(summ); print(cors[a %in% c("avg500", "noflip", "ge200", "vendor") & b %in% c("avg500", "noflip", "ge200", "vendor")]); print(part); print(est)
}
