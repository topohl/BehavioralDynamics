# ================================================================
# Audit: the reconstructed PhenoSys AnimalPos estimator against the vendor exports, all 24 recordings
# MMMSociability -- Testing/audits/audit_phenosys_reconstruction.R
# ================================================================
# Reads (never writes): the raw MultiMouseMonitor logs and the vendor AnimalPos exports (raw_data/) under the live data
# root. Writes: <parent of the live root>/RANT/comparison_vendor_raw/phenosys_reconstruction/ (environment variables:
# MMM_APR_AUDIT_OUT = another output folder; MMM_APR_AUDIT_ONLY = a file-name pattern for a partial run).
#   summary_by_recording.csv   vendor, emulated and matched records per recording (exact on time to the ms, x and y at 0.1)
#   unmatched_records.csv      every unmatched record: recording, animal, system, time, distance to the recording's first
#                              and last read, the nearest emulated/vendor record of the tag at the same coordinates
#   unmatched_by_group.csv     unmatched records by recording, animal, system and clock hour
#   clock_fold_check.csv       recordings whose log repeats an hour (daylight saving ends), re-run with the repeated
#                              rows kept and interleaved by wall clock
#   provenance.csv             git commit, sha256 of this script and of the estimator, R version, time
# The estimator: Functions/animalpos_reconstruction.R (apr_positions(gate = "vendor")).
# Run from the repository root:  Rscript Testing/audits/audit_phenosys_reconstruction.R
# ================================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(data.table) })
source("Functions/project_paths.R")
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/behavioral_dynamics_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/animalpos_reconstruction.R")

apr_audit_recording <- function(raw_dir, tag_dir, batch, cc, drop_clock_fold = TRUE) {
  export <- file.path(tag_dir, batch, sprintf("E9_SIS_%s_%s_AnimalPos.csv", batch, cc))
  vendor <- apr_read_vendor_export(export)
  rec <- rfid_read_recording(raw_dir, batch, cc, keep_read_offsets = TRUE, drop_clock_fold = drop_clock_fold)
  reads <- apr_reads(rec$events, allow_negative_gaps = !drop_clock_fold)   # 5 B1 CC1 events span the clock fall-back
  reads <- reads[reads$tag %in% vendor$tag, , drop = FALSE]   # the vendor writes no record for a tag missing from its export
  first_read <- min(reads$t_ms); last_read <- max(reads$t_ms)
  per_tag <- lapply(split(reads, reads$tag), function(r) {
    e <- apr_positions(r$t_ms, r$gx, r$gy, gate = "vendor")
    v <- vendor[vendor$tag == r$tag[1], , drop = FALSE]
    cmp <- apr_compare_records(e, v)
    nearest <- function(a, b) vapply(seq_len(nrow(a)), function(i) {
      same <- b$t_ms[round(b$x, 1) == round(a$x[i], 1) & round(b$y, 1) == round(a$y[i], 1)]
      if (length(same)) same[which.min(abs(same - a$t_ms[i]))] - a$t_ms[i] else NA_real_ }, numeric(1))
    vo <- cmp$vendor_only; eo <- cmp$emulated_only
    un <- rbind(
      if (nrow(vo)) data.frame(side = "vendor_only", tag = r$tag[1], AnimalNum = vo$AnimalNum, System = vo$System, t_ms = vo$t_ms,
                               x = vo$x, y = vo$y, nearest_other_at_same_xy_ms = nearest(vo, e)),
      if (nrow(eo)) data.frame(side = "emulated_only", tag = r$tag[1], AnimalNum = v$AnimalNum[1], System = v$System[1], t_ms = eo$t_ms,
                               x = eo$x, y = eo$y, nearest_other_at_same_xy_ms = nearest(eo, v)))
    list(n = data.frame(tag = r$tag[1], vendor = nrow(v), emulated = nrow(e), matched = nrow(cmp$matched)), unmatched = un)
  })
  un <- dplyr::bind_rows(lapply(per_tag, `[[`, "unmatched"))
  if (nrow(un)) un <- un %>% dplyr::mutate(Batch = batch, CageChange = cc,
                                            time_utc = format(as.POSIXct(t_ms / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%OS3"),
                                            s_after_first_read = (t_ms - first_read) / 1000, s_before_last_read = (last_read - t_ms) / 1000, .before = 1)
  n <- dplyr::bind_rows(lapply(per_tag, `[[`, "n"))
  list(summary = data.frame(Batch = batch, CageChange = cc, raw_log = basename(rec$path), clock_fold_rows_dropped = rec$clock_fold_rows_dropped,
                            tags = nrow(n), reads = nrow(reads), vendor = sum(n$vendor), emulated = sum(n$emulated), matched = sum(n$matched),
                            vendor_tags_without_reads = length(setdiff(unique(vendor$tag), unique(reads$tag)))),
       unmatched = un)
}

if (sys.nframe() == 0L) {
  LIVE_ROOT <- MMM_PROJECT_ROOT_DEFAULT
  RAW_DIR <- file.path(dirname(dirname(dirname(LIVE_ROOT))), "Raw Data", "Behavior", "RFID", "MMMdata")
  TAG_DIR <- file.path(LIVE_ROOT, "MMMSociability", "raw_data")
  OUT <- Sys.getenv("MMM_APR_AUDIT_OUT", unset = file.path(dirname(LIVE_ROOT), "RANT", "comparison_vendor_raw", "phenosys_reconstruction"))
  if (startsWith(tolower(normalizePath(OUT, winslash = "/", mustWork = FALSE)), tolower(normalizePath(LIVE_ROOT, winslash = "/"))))
    stop("The audit must not write inside the live root.", call. = FALSE)
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  pre <- sort(list.files(file.path(LIVE_ROOT, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "preprocessed_data"),
                         pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$"))
  only <- Sys.getenv("MMM_APR_AUDIT_ONLY")
  if (nzchar(only)) pre <- pre[grepl(only, pre)]
  res <- list(); fold <- list()
  for (f in pre) {
    b <- sub("^E9_SIS_(B[0-9]+)_.*$", "\\1", f); cc <- sub("^.*_(CC[0-9]+)_.*$", "\\1", f)
    a <- apr_audit_recording(RAW_DIR, TAG_DIR, b, cc)
    res[[f]] <- a
    message(sprintf("%s %s: vendor %d, emulated %d, matched %d", b, cc, a$summary$vendor, a$summary$emulated, a$summary$matched))
    if (a$summary$clock_fold_rows_dropped > 0) {
      k <- apr_audit_recording(RAW_DIR, TAG_DIR, b, cc, drop_clock_fold = FALSE)
      fold[[f]] <- data.frame(mode = c("repeated hour dropped (pipeline default)", "repeated hour kept, interleaved by wall clock"),
                              rbind(a$summary, k$summary))
      message(sprintf("  repeated hour kept: vendor %d, emulated %d, matched %d", k$summary$vendor, k$summary$emulated, k$summary$matched))
    }
  }
  s <- dplyr::bind_rows(lapply(res, `[[`, "summary"))
  s$vendor_only <- s$vendor - s$matched; s$emulated_only <- s$emulated - s$matched
  un <- dplyr::bind_rows(lapply(res, `[[`, "unmatched"))
  data.table::fwrite(s, file.path(OUT, "summary_by_recording.csv"))
  data.table::fwrite(un, file.path(OUT, "unmatched_records.csv"))
  if (nrow(un)) data.table::fwrite(un %>% dplyr::mutate(hour = substr(time_utc, 1, 13)) %>%
                                     dplyr::count(Batch, CageChange, AnimalNum, System, side, hour, name = "records"),
                                   file.path(OUT, "unmatched_by_group.csv"))
  data.table::fwrite(dplyr::bind_rows(fold), file.path(OUT, "clock_fold_check.csv"))
  sha <- function(p) digest::digest(file = p, algo = "sha256")
  data.table::fwrite(data.frame(item = c("git_commit", "script_sha256", "estimator_sha256", "reader_sha256", "r_version", "finished"),
    value = c(system2("git", c("-C", getwd(), "rev-parse", "HEAD"), stdout = TRUE)[1], sha("Testing/audits/audit_phenosys_reconstruction.R"),
              sha("Functions/animalpos_reconstruction.R"), sha("Functions/antenna_log_helpers.R"), R.version.string,
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), file.path(OUT, "provenance.csv"))
  cat(sprintf("TOTAL vendor %d, emulated %d, matched %d (%.4f%%), vendor-only %d, emulated-only %d\n", sum(s$vendor), sum(s$emulated),
              sum(s$matched), 100 * sum(s$matched) / sum(s$vendor), sum(s$vendor_only), sum(s$emulated_only)))
}
