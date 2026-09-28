# ================================================================
# Stage 30 canonical movement in caller-defined windows (Exp9 SIS RFID)
# MMMSociability -- Functions/stage30_movement.R
# ================================================================
# Measure: the Stage 29 RFID position-change stream. A position change is a change of the floor-snapped PositionID
# between consecutive vendor position records of one animal within SourceFile x System (vendor-defined position
# records: >= 200 grid units). Per animal x window the wrapper returns
#   n_events       number of position changes in the window,
#   obs_s          observed seconds (sum of occupancy-run durations inside the window),
#   crossing_rate  position changes per observed hour (identifier kept from Stage 29).
#
# Everything that defines the measure is delegated, unmodified, to the frozen Stage 29 code path:
#   Functions/rfid_event_stream.R    mmm_evs_read_preprocessed  reader + Group guard
#                                    mmm_evs_seeds              pre-window seeds (raw_data)
#                                    mmm_evs_build_stream       event definition, carry-forward bookkeeping
#                                    mmm_evs_window_stream      window clip, carry-forward to the window end, runs
#   Functions/rfid_binfree_metrics.R mmm_bf_animal_metrics      n_events / obs_s / crossing_rate (as in mmm_bf_window_table)
#                                    mmm_bf_window_table        all canonical window metrics (full = TRUE)
# The wrapper only (a) reads an explicit file list or in-memory data, (b) injects the caller's windows as the "active"
# window of the stream object (the columns mmm_evs_window_stream reads), one window label at a time, and (c) adds
# explicit missingness and coverage flags. It never defines an event, a clip or a denominator.
#
# Reuse without modification: mmm_evs_read_preprocessed and mmm_evs_build_stream are called with their own bodies in a
# child environment in which only their input collaborators are replaced (s30mv_with): list.files -> the explicit file
# list; mmm_evs_read_preprocessed -> the caller's data; mmm_evs_windows -> unused (windows come from the caller).
# Testing/tests/test_stage30_movement_wrapper.R asserts identity with the direct canonical calls.
#
# Requires: rfid_event_stream.R, rfid_binfree_metrics.R, behavior_analysis_config.R, canonical_animal_id()
# (behavioral_dynamics_helpers.R). No Group column is read or returned. No vendor ActivityIndex, no raw-detection layer.
#
# Usage (Stage 30 driver):
#   pre <- s30mv_read_preprocessed(files)                 # canonical preprocessed files (csv / csv.gz), e.g. data version v2 + post-EPM
#   st  <- s30mv_build_stream(pre, raw_dir)               # raw_dir/<Batch>/E9_SIS_<B>_<change>_AnimalPos.csv for seeds
#   m   <- s30mv_window_metrics(st, windows)              # windows: SourceFile, window_id, start, end (POSIXct, tz UTC)
#   or  m <- s30mv_movement(pre_or_named_list, windows, raw_dir)
# Completeness is the caller's rule: status, obs_share, known_at_start, file_covers_start / file_covers_end are returned.
# ================================================================

S30MV_PRE_COLS <- c("DateTime", "AnimalID", "System", "PositionID", "Batch", "CageChange", "SourceFile", "AnimalNum", "is_seed")

#' Copy of a canonical function whose free variables `...` are replaced; the body is unchanged.
s30mv_with <- function(fun, ...) {
  env <- new.env(parent = environment(fun))
  ov <- list(...)
  for (nm in names(ov)) assign(nm, ov[[nm]], envir = env)
  environment(fun) <- env
  fun
}

#' Read canonical preprocessed files (csv or csv.gz, any change label) with the unmodified canonical reader.
#' SourceFile is the file name without ".gz", so the canonical seed step finds raw_dir/<Batch>/<name without _preprocessed>.
s30mv_read_preprocessed <- function(files) {
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  if (anyDuplicated(sub("[.]gz$", "", basename(files)))) stop("Duplicate SourceFile names in the file list.", call. = FALSE)
  reader <- s30mv_with(mmm_evs_read_preprocessed, list.files = function(...) files)
  pre <- reader(pre_dir = dirname(files[1]))
  pre[, SourceFile := sub("[.]gz$", "", SourceFile)]
  pre[]
}

#' In-memory preprocess_animalpos_file() output -> the canonical file representation.
#' preprocess_animalpos_file() keeps full raw precision in memory, but its written file (the representation Stage 29
#' reads) is truncated to the millisecond by readr, i.e. up to 0.9999 ms earlier in about half of the rows (W3 step 1).
#' The data are therefore written with the function's own writer lines (animalpos_preprocessing_helpers.R:254-255) and
#' read back with the canonical reader. `data_list` is named by SourceFile ("E9_SIS_<B>_<change>_AnimalPos_preprocessed.csv").
s30mv_roundtrip <- function(data_list, dir = tempfile("s30mv_pre_")) {
  if (is.null(names(data_list)) || !all(grepl("_AnimalPos_preprocessed[.]csv$", names(data_list))))
    stop("data_list must be named by SourceFile (..._AnimalPos_preprocessed.csv).", call. = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, names(data_list))
  for (i in seq_along(paths)) {
    if ("Group" %in% names(data_list[[i]])) stop("Unexpected Group column in ", names(data_list)[i], call. = FALSE)
    old_opt <- options(digits.secs = 3)
    readr::write_csv(data_list[[i]], paths[i])
    options(old_opt)
  }
  s30mv_read_preprocessed(paths)
}

#' Empty seed table with the columns mmm_evs_seeds() returns (used when no seed exists or raw_dir is NULL).
s30mv_empty_seeds <- function(pre) {
  data.table::data.table(DateTime = pre$DateTime[0], AnimalID = character(), System = character(), PositionID = integer(),
                         Batch = character(), CageChange = character(), SourceFile = character(), AnimalNum = character(),
                         is_seed = logical(), SeedReadTime = pre$DateTime[0], raw_file = character())
}

#' Canonical stream from preprocessed data: the body of mmm_evs_build_stream() on the caller's data.
#' raw_dir = NULL builds the stream without seeds; s30mv_window_metrics() then refuses (or flags) every animal-window a
#' seed could affect, so an unseeded result is only returned where it equals the seeded one.
s30mv_build_stream <- function(pre, raw_dir = NULL, parser = MMM_EVS_SEED_PARSER) {
  if (!data.table::is.data.table(pre)) stop("pre must be a data.table (s30mv_read_preprocessed / s30mv_roundtrip).", call. = FALSE)
  if ("Group" %in% names(pre)) stop("Unexpected Group column in the preprocessed data.", call. = FALSE)
  miss <- setdiff(S30MV_PRE_COLS, names(pre))
  if (length(miss)) stop("Preprocessed data lack: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!inherits(pre$DateTime, "POSIXct") || !identical(attr(pre$DateTime, "tzone"), "UTC")) stop("DateTime must be POSIXct UTC.", call. = FALSE)
  if (any(pre$is_seed)) stop("Input already contains seed rows.", call. = FALSE)
  input_files <- attr(pre, "input_files")
  pre <- pre[, S30MV_PRE_COLS, with = FALSE]          # exactly the canonical reader's columns, in its order
  attr(pre, "input_files") <- input_files
  one <- pre[, .(nb = data.table::uniqueN(Batch), nc = data.table::uniqueN(CageChange)), by = SourceFile]
  if (any(one$nb != 1 | one$nc != 1)) stop("Each SourceFile must have one Batch and one CageChange.", call. = FALSE)
  empty <- s30mv_empty_seeds(pre)
  seeds_fun <- if (is.null(raw_dir)) function(pre, raw_dir, parser) empty else function(pre, raw_dir, parser) {
    s <- mmm_evs_seeds(pre, raw_dir, parser = parser)
    if (!ncol(s)) empty else s
  }
  build <- s30mv_with(mmm_evs_build_stream, mmm_evs_read_preprocessed = function(pre_dir) pre,
                      mmm_evs_seeds = seeds_fun, mmm_evs_windows = function(pre) NULL)
  st <- build(pre_dir = NULL, raw_dir = raw_dir, parser = parser)
  st$windows <- NULL
  st$file_span <- pre[, .(file_t0 = min(DateTime), file_last = max(DateTime), Batch = Batch[1],
                          CC = paste0("CC", sub("^CC", "", as.character(CageChange[1])))), by = SourceFile]   # CC as in mmm_evs_windows()
  st$seeded <- !is.null(raw_dir)
  st
}

#' Validate the caller's window table: SourceFile, window_id, start, end (POSIXct UTC), end > start, unique per SourceFile.
s30mv_window_table <- function(windows, stream) {
  w <- data.table::as.data.table(windows)
  miss <- setdiff(c("SourceFile", "window_id", "start", "end"), names(w))
  if (length(miss)) stop("Window table lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (!inherits(w$start, "POSIXct") || !inherits(w$end, "POSIXct")) stop("start/end must be POSIXct.", call. = FALSE)
  # the logger clock is stored as UTC throughout the pipeline; a local-time POSIXct would silently shift the window
  if (!identical(attr(w$start, "tzone"), "UTC") || !identical(attr(w$end, "tzone"), "UTC"))
    stop("start/end must be POSIXct with tz = 'UTC' (logger clock).", call. = FALSE)
  w <- w[, .(SourceFile = as.character(SourceFile), window_id = as.character(window_id), start, end)]
  if (anyNA(w)) stop("Window table has missing values.", call. = FALSE)
  if (any(w$end <= w$start)) stop("Every window needs end > start.", call. = FALSE)
  if (anyDuplicated(w[, .(SourceFile, window_id)])) stop("Duplicate (SourceFile, window_id) in the window table.", call. = FALSE)
  unk <- setdiff(w$SourceFile, stream$file_span$SourceFile)
  if (length(unk)) stop("Windows for SourceFiles not in the stream: ", paste(unk, collapse = ", "), call. = FALSE)
  merge(w, stream$file_span, by = "SourceFile")
}

#' One validated window label (one row per SourceFile) injected as the "active" window of the stream object, exactly the
#' columns mmm_evs_window_stream() reads; returns its output unchanged (events, runs in seconds since the window start).
s30mv_inject_window <- function(stream, w) {
  if (anyDuplicated(w$SourceFile)) stop("One window per SourceFile per injection.", call. = FALSE)
  st <- stream
  st$windows <- w[, .(SourceFile, Batch, CC, t0 = file_t0, active_start = start, active_end = end, light_start = start, light_end = end)]
  # mmm_evs_window_stream() warns (min/max of empty vectors) only when no animal occupies the window at all; those warnings
  # are dropped in exactly that case (the rows are then returned as explicit missingness); any other warning is re-raised.
  wlog <- list()
  ws <- withCallingHandlers(mmm_evs_window_stream(st, "active"),
                            warning = function(cnd) { wlog[[length(wlog) + 1L]] <<- cnd; invokeRestart("muffleWarning") })
  if (nrow(ws$runs)) for (cnd in wlog) warning(cnd)
  ws
}

#' Canonical window streams for caller-defined windows, one mmm_evs_window_stream() result per window_id: the hook for
#' further metrics on the same representation. The canonical event set consistent with n_events is
#' mmm_bf_events_from_runs(ws$runs) (run boundaries); ws$events additionally lists a change exactly at the window start and
#' every row of tied reads.
s30mv_window_streams <- function(stream, windows) {
  W <- s30mv_window_table(windows, stream)
  lapply(split(W, by = "window_id"), function(w) s30mv_inject_window(stream, w))
}

#' Per animal x window: n_events, obs_s, crossing_rate (canonical), plus coverage flags and explicit missingness.
#' na_if_file_ends_early: crossing_rate (and all metrics) NA when the SourceFile's last read precedes the window end
#'   (the canonical carry-forward would otherwise extend the last position past the recording; Stage 29 light-phase rule,
#'   MMM_BEHAVIOR_CONFIG$windows$light_phase$coverage_rule).
#' strict_seed: with an unseeded stream, stop if a seed could change any returned animal-window; FALSE -> NA + status.
#' full: return every mmm_bf_window_table() column (shared zone, dyad-based columns), not only mmm_bf_animal_metrics().
s30mv_window_metrics <- function(stream, windows,
                                 bout_criterion_s = MMM_BEHAVIOR_CONFIG$metrics$fragmentation$bout_criterion_s,
                                 na_if_file_ends_early = TRUE, strict_seed = TRUE, full = FALSE) {
  W <- s30mv_window_table(windows, stream)
  # logical-vector subsetting: a `col == value` filter would add a data.table auto-index attribute to the caller's stream
  is_read <- !stream$pos$is_seed
  reads <- stream$pos[is_read, .(first_read = min(DateTime), last_read = max(DateTime)), by = .(SourceFile, AnimalNum)]
  sys <- unique(stream$pos[, .(SourceFile, AnimalNum, System)])
  if (anyDuplicated(sys[, .(SourceFile, AnimalNum)])) stop("An animal carries more than one System within a SourceFile.", call. = FALSE)
  key_chk <- unique(merge(sys, stream$file_span[, .(SourceFile, CC)], by = "SourceFile")[, .(AnimalNum, CC, SourceFile)])
  if (anyDuplicated(key_chk[, .(AnimalNum, CC)])) stop("(AnimalNum, CC) must identify one SourceFile (canonical key MMM_BF_KEY).", call. = FALSE)
  seeded_an <- unique(stream$pos[!is_read, .(SourceFile, AnimalNum, seeded = TRUE)])
  grid <- merge(merge(reads, sys, by = c("SourceFile", "AnimalNum")), seeded_an, by = c("SourceFile", "AnimalNum"), all.x = TRUE)
  grid[is.na(seeded), seeded := FALSE]
  out <- data.table::rbindlist(lapply(split(W, by = "window_id"), function(w) {
    ws <- s30mv_inject_window(stream, w)
    am <- if (!nrow(ws$runs)) NULL else if (isTRUE(full)) mmm_bf_window_table(ws, bout_criterion_s)$animals[, !c("SourceFile", "System")]
          else mmm_bf_animal_metrics(ws$runs, bout_criterion_s)
    g <- merge(grid, w, by = "SourceFile")
    if (!is.null(am)) {
      if (nrow(data.table::fsetdiff(am[, .(AnimalNum, CC)], g[, .(AnimalNum, CC)])))
        stop("internal: canonical window rows without a stream animal", call. = FALSE)
      g <- merge(g, am, by = c("AnimalNum", "CC"), all.x = TRUE)
    } else g[, `:=`(obs_s = NA_real_, n_events = NA_integer_, crossing_rate = NA_real_)]
    g
  }), fill = TRUE)
  out[, window_s := as.numeric(end) - as.numeric(start)]
  out[, `:=`(file_covers_start = file_t0 <= start, file_covers_end = file_last >= end,
             known_at_start = (seeded & file_t0 <= start) | first_read <= start,
             seed_irrelevant = first_read <= file_t0 | first_read <= start | file_t0 >= end)]
  # explicit missingness: an animal of the SourceFile without any occupancy in the window keeps its row (obs_s = 0, metrics NA)
  out[, status := data.table::fifelse(is.na(obs_s), "not_observed_in_window", "ok")]
  out[status == "not_observed_in_window", obs_s := 0]
  out[status == "ok" & !file_covers_end, status := "file_ends_before_window_end"]
  if (!isTRUE(stream$seeded)) {
    bad <- out[seed_irrelevant == FALSE]
    if (nrow(bad) && isTRUE(strict_seed))
      stop(nrow(bad), " animal-windows could change with a pre-window seed (first read after the window start); supply raw_dir.", call. = FALSE)
    out[seed_irrelevant == FALSE, status := "seed_unavailable"]
  }
  metric_cols <- setdiff(names(out), c(names(grid), names(W), "window_s", "file_covers_start", "file_covers_end",
                                       "known_at_start", "seed_irrelevant", "status", "AnimalNum", "CC"))
  if (isTRUE(na_if_file_ends_early)) for (k in metric_cols) data.table::set(out, which(out$status == "file_ends_before_window_end"), k, NA)
  for (k in metric_cols) data.table::set(out, which(out$status == "seed_unavailable"), k, NA)
  out[, obs_share := obs_s / window_s]
  data.table::setnames(out, c("start", "end"), c("window_start", "window_end"))
  first <- c("SourceFile", "Batch", "CC", "System", "AnimalNum", "window_id", "window_start", "window_end", "window_s",
             "n_events", "obs_s", "crossing_rate", "obs_share", "status", "known_at_start", "seeded", "first_read", "last_read",
             "file_t0", "file_last", "file_covers_start", "file_covers_end", "seed_irrelevant")
  data.table::setcolorder(out, c(first, setdiff(names(out), first)))
  data.table::setorder(out, SourceFile, window_id, System, AnimalNum)
  if ("Group" %in% names(out)) stop("internal: Group column in output", call. = FALSE)
  out[]
}

#' One call. `pre` is either the canonical reader's data.table (s30mv_read_preprocessed) or a list of
#' preprocess_animalpos_file()$data tables named by SourceFile (then round-tripped through the canonical writer/reader).
#' raw_dir: folder <raw_dir>/<Batch>/<SourceFile without _preprocessed> for the canonical seed step (NULL: unseeded, guarded).
s30mv_movement <- function(pre, windows, raw_dir = NULL, ...) {
  if (!data.table::is.data.table(pre)) {
    if (is.data.frame(pre) || !is.list(pre))
      stop("pass the reader's data.table or a list of preprocess_animalpos_file()$data named by SourceFile.", call. = FALSE)
    pre <- s30mv_roundtrip(pre)
  }
  s30mv_window_metrics(s30mv_build_stream(pre, raw_dir), windows, ...)
}
