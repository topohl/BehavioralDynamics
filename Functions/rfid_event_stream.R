# ================================================================
# Canonical RFID event stream and occupancy intervals (bin-free)
# MMMSociability -- Functions/rfid_event_stream.R
# ================================================================
# Rebuilds, per animal, the exact change-only position stream that Stage 01 uses:
#   preprocessed reads + one pre-window seed per late animal (Stage 01 rule,
#   Analysis/01_build_multiscale_behavior_metrics.R:585-633), and derives
#   * crossing events (PositionID change between consecutive reads, Stage 01 :466-473),
#   * carry-forward occupancy intervals,
# clipped to analysis windows. No binning, no Group column.
#
# One deliberate difference from Stage 01: the raw-label parser strips both
# "_sys." and "-sys." suffixes (Stage 01 :605 strips only "_sys."), so the 8 late
# B1 CC2 animals whose raw labels are hyphenated receive their seed. See
# MMM_BEHAVIOR_CONFIG$event_stream.
# ================================================================

MMM_EVS_GRID <- data.frame(xPos = c(0, 100, 200, 300, 0, 100, 200, 300),
                           yPos = c(0, 0, 0, 0, 116, 116, 116, 116), SeedPositionID = 1:8)
MMM_EVS_SEED_PARSER <- "[-_]sys[.].*$"
MMM_EVS_STAGE01_PARSER <- "_sys[.].*$"

mmm_evs_read_preprocessed <- function(pre_dir) {
  files <- list.files(pre_dir, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$", full.names = TRUE)
  if (!length(files)) stop("No preprocessed files in ", pre_dir, call. = FALSE)
  out <- data.table::rbindlist(lapply(files, function(f) {
    # Check the file header, not the selected columns: select= would drop a Group column before the check.
    if ("Group" %in% names(data.table::fread(f, nrows = 0))) stop("Unexpected Group column in ", f, call. = FALSE)
    d <- data.table::fread(f, select = c("DateTime", "AnimalID", "System", "PositionID", "Batch", "CageChange"),
                           colClasses = c(AnimalID = "character"))
    d[, SourceFile := basename(f)]
    d
  }))
  if (!inherits(out$DateTime, "POSIXct"))
    out[, DateTime := as.POSIXct(DateTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
  attr(out$DateTime, "tzone") <- "UTC"
  out[, AnimalNum := canonical_animal_id(AnimalID)]
  out[, is_seed := FALSE]
  attr(out, "input_files") <- files
  out[]
}

#' Pre-window seeds: the last on-grid raw read before the file's first timestamp t0,
#' stamped at t0, for every animal whose first preprocessed read is later than t0.
mmm_evs_seeds <- function(pre, raw_dir, parser = MMM_EVS_SEED_PARSER) {
  grid <- data.table::as.data.table(MMM_EVS_GRID)
  data.table::rbindlist(lapply(split(pre, by = "SourceFile"), function(p) {
    src <- p$SourceFile[1]; t0 <- min(p$DateTime)
    late <- p[, .(first_read = min(DateTime)), by = AnimalNum][first_read > t0]
    if (!nrow(late)) return(NULL)
    raw_file <- file.path(raw_dir, p$Batch[1], sub("_preprocessed[.]csv$", ".csv", src))
    raw <- data.table::fread(raw_file, sep = ";", select = c("DateTime", "Animal", "xPos", "yPos"), colClasses = "character")
    raw[, DT := as.POSIXct(DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")]
    raw[, AnimalNum := canonical_animal_id(sub(parser, "", Animal))]
    raw[, `:=`(xPos = suppressWarnings(as.numeric(xPos)), yPos = suppressWarnings(as.numeric(yPos)))]
    raw <- raw[!is.na(DT) & DT < t0 & AnimalNum %in% late$AnimalNum]
    raw <- merge(raw, grid, by = c("xPos", "yPos"))
    if (!nrow(raw)) return(NULL)
    s <- raw[order(-DT)][, .SD[1], by = AnimalNum][, .(AnimalNum, SeedPositionID, SeedReadTime = DT)]
    first_rows <- p[order(DateTime)][, .SD[1], by = AnimalNum]
    s <- merge(first_rows[, .(AnimalNum, AnimalID, System, Batch, CageChange, SourceFile)], s, by = "AnimalNum")
    s[, .(DateTime = t0, AnimalID, System, PositionID = as.integer(SeedPositionID), Batch, CageChange,
          SourceFile, AnimalNum, is_seed = TRUE, SeedReadTime, raw_file = raw_file)]
  }), fill = TRUE)
}

#' Analysis windows per SourceFile. The primary active window is 18:30 of the calendar
#' day of the file's first timestamp (preprocessed files start at the first Active block),
#' for exactly 12 h; the light-phase window is the following 12 h.
mmm_evs_windows <- function(pre) {
  f <- pre[, .(t0 = min(DateTime), Batch = Batch[1], CageChange = CageChange[1]), by = SourceFile]
  f[, ws := as.POSIXct(paste(format(t0, "%Y-%m-%d", tz = "UTC"), "18:30:00"), tz = "UTC")]
  lag <- as.numeric(f$t0) - as.numeric(f$ws)
  if (any(lag < 0 | lag >= 3600)) stop("A preprocessed file does not start within the first hour after 18:30.", call. = FALSE)
  f[, `:=`(active_start = ws, active_end = ws + 12 * 3600, light_start = ws + 12 * 3600, light_end = ws + 24 * 3600)]
  f[, CC := paste0("CC", sub("^CC", "", as.character(CageChange)))]
  f[, .(SourceFile, Batch, CC, t0, active_start, active_end, light_start, light_end)]
}

#' Full canonical stream: reads + seeds, ordered, with per-row next-read times.
mmm_evs_build_stream <- function(pre_dir, raw_dir, parser = MMM_EVS_SEED_PARSER) {
  pre <- mmm_evs_read_preprocessed(pre_dir)
  seeds <- mmm_evs_seeds(pre, raw_dir, parser = parser)
  pos <- data.table::rbindlist(list(pre, seeds[, !c("SeedReadTime", "raw_file")]), fill = TRUE)
  data.table::setorder(pos, SourceFile, System, AnimalNum, DateTime, -is_seed)
  pos[, prevPos := data.table::shift(PositionID), by = .(SourceFile, System, AnimalNum)]
  pos[, changed := is.finite(prevPos) & PositionID != prevPos]
  pos[, nextT := data.table::shift(DateTime, type = "lead"), by = .(SourceFile, System, AnimalNum)]
  list(pos = pos, seeds = seeds, windows = mmm_evs_windows(pre), input_files = attr(pre, "input_files"))
}

#' Events and merged same-position occupancy runs inside a window type.
#' Times are returned in seconds since the window start. An animal's last row in the
#' file is carried forward to the window end (Stage 01 carry-forward).
mmm_evs_window_stream <- function(stream, window = c("active", "light")) {
  window <- match.arg(window)
  w <- stream$windows[, .(SourceFile, CC, Batch,
                          ws = if (window == "active") active_start else light_start,
                          we = if (window == "active") active_end else light_end)]
  pos <- merge(stream$pos, w[, .(SourceFile, CC, ws, we)], by = "SourceFile")
  pos[is.na(nextT), nextT := we]
  ev <- pos[changed == TRUE & DateTime >= ws & DateTime < we,
            .(SourceFile, System, AnimalNum, CC, t = as.numeric(DateTime) - as.numeric(ws), from = prevPos, to = PositionID)]
  occ <- pos[, .(SourceFile, System, AnimalNum, CC, PositionID,
                 s = pmax(as.numeric(DateTime), as.numeric(ws)) - as.numeric(ws),
                 e = pmin(as.numeric(nextT), as.numeric(we)) - as.numeric(ws))][e > s]
  data.table::setorder(occ, AnimalNum, CC, s)
  occ[, rid := data.table::rleid(PositionID), by = .(AnimalNum, CC)]
  runs <- occ[, .(SourceFile = SourceFile[1], System = System[1], s = min(s), e = max(e), pos = PositionID[1]),
              by = .(AnimalNum, CC, rid)][, rid := NULL]
  data.table::setorder(ev, AnimalNum, CC, t)
  list(events = ev, runs = runs[], windows = w, window_type = window)
}
