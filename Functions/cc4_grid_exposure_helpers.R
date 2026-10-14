# Stage 31: descriptive CC4 clock profiles and activity-selected candidate windows.
# No candidate is an observed grid timestamp; no effect test uses these windows.

grid31_required <- function(x, cols, label) {
  missing <- setdiff(cols, names(x))
  if (length(missing)) stop(label, " lacks: ", paste(missing, collapse = ", "), call. = FALSE)
}

# Opt-in recording-duration censoring: one CensorFrom per cage episode (Batch,
# CageChange, System); time after it is unobserved, never zero activity. These
# columns appear only when a censor table is supplied; no row is ever dropped.
GRID31_CENSOR_COLUMNS <- c("CensorFrom", "CensorRecord", "CensoredSec", "CensoredAnimals")

grid31_join_censor <- function(x, censor) {
  grid31_required(censor, c("Batch", "CageChange", "System", "CensorFrom", "CensorRecord"), "Censor table")
  if (!inherits(censor$CensorFrom, "POSIXct") || anyNA(censor$CensorFrom) || anyNA(censor$CensorRecord) ||
      any(!nzchar(censor$CensorRecord)) || anyDuplicated(censor[c("Batch", "CageChange", "System")]))
    stop("Invalid censor table: one non-missing CensorFrom and CensorRecord per cage episode.", call. = FALSE)
  x %>% dplyr::select(-dplyr::any_of(GRID31_CENSOR_COLUMNS)) %>%
    dplyr::left_join(censor[c("Batch", "CageChange", "System", "CensorFrom", "CensorRecord")],
                     by = c("Batch", "CageChange", "System"))
}

# Seconds of [start, end) at or after the censor time; 0 without a censor time.
grid31_censored_sec <- function(start, end, from) {
  from <- as.numeric(from); from[is.na(from)] <- Inf
  pmax(0, as.numeric(end) - pmax(as.numeric(start), from))
}

# Apply only an explicitly supplied, provenance-checked correction registry.
grid31_correct_systems <- function(d, corrections) {
  grid31_required(corrections, c("Batch", "CageChange", "AnimalID", "old_System", "new_System", "evidence"), "Corrections")
  c <- corrections[corrections$CageChange == "CC4" & corrections$Batch %in% d$Batch, ]
  if (anyDuplicated(c[c("Batch", "CageChange", "AnimalID")])) stop("Duplicate cage corrections.", call. = FALSE)
  d$System_raw <- d$System
  for (i in seq_len(nrow(c))) {
    ix <- d$Batch == c$Batch[i] & d$CageChange == c$CageChange[i] & d$AnimalID == c$AnimalID[i]
    if (!any(ix) || any(d$System[ix] != c$old_System[i]) ||
        is.na(c$new_System[i]) || !grepl("^sys[.][0-9]+$", c$new_System[i]) ||
        is.na(c$evidence[i]) || !nzchar(c$evidence[i])) stop("Unresolved cage correction for ", c$AnimalID[i], call. = FALSE)
    d$System[ix] <- c$new_System[i]
  }
  d
}

# Cartesian animal x phase coverage includes zero-read phases. Bracketing only
# describes the animal's observed timestamp span, never continuous sensor health.
# With a censor table, CensoredSec is the part of the phase block after CensorFrom.
grid31_phase_coverage <- function(d, censor = NULL) {
  grid31_required(d, c("Batch", "CageChange", "AnimalID", "System", "DateTime", "Phase", "ConsecActive", "ConsecInactive"), "Positions")
  d$PhaseBlockIndex <- animalpos_phase_block_index(d$DateTime)
  lut <- animalpos_phase_block_lut(d$DateTime)
  lut$BlockStart <- as.POSIXct(lut$PhaseBlockIndex * 43200 + 23400, origin = "1970-01-01", tz = "UTC")
  lut$BlockEnd <- lut$BlockStart + 43200
  lut$PhaseNumber <- ifelse(lut$Phase == "Active", lut$ConsecActive, lut$ConsecInactive)
  lut$PhaseLabel <- paste0(substr(lut$Phase, 1, 1), lut$PhaseNumber)
  lut$SessionSpansWholeBlock <- min(d$DateTime) <= lut$BlockStart & max(d$DateTime) >= lut$BlockEnd
  roster <- d %>% dplyr::group_by(Batch, CageChange, System, AnimalID) %>%
    dplyr::summarise(AnimalFirstRead = min(DateTime), AnimalLastRead = max(DateTime), .groups = "drop")
  observed <- d %>% dplyr::group_by(System, AnimalID, PhaseBlockIndex) %>%
    dplyr::summarise(ReadCount = dplyr::n(), FirstRead = min(DateTime), LastRead = max(DateTime),
                     MaxWithinBlockReadGapSec = if (dplyr::n() > 1) max(diff(sort(as.numeric(DateTime)))) else NA_real_, .groups = "drop")
  coverage <- tidyr::crossing(roster, lut) %>%
    dplyr::left_join(observed, by = c("System", "AnimalID", "PhaseBlockIndex")) %>%
    dplyr::mutate(ReadCount = tidyr::replace_na(ReadCount, 0L),
                  AnimalSpanOverlapSec = pmax(0, pmin(as.numeric(AnimalLastRead), as.numeric(BlockEnd)) -
                                               pmax(as.numeric(AnimalFirstRead), as.numeric(BlockStart))),
                  AnimalSpansWholeBlock = AnimalSpanOverlapSec >= 43200)
  if (is.null(censor)) return(coverage)
  grid31_join_censor(coverage, censor) %>% dplyr::mutate(CensoredSec = grid31_censored_sec(BlockStart, BlockEnd, CensorFrom))
}

# Five-minute clock bins. A crossing is a PositionID change at the later read,
# including a previous read before the bin/phase boundary. No synthetic reads,
# no crossing on the first read, and no carry-forward beyond an animal's last read.
# With a censor table: per animal-bin CensoredSec = max(0, BinEnd - max(BinStart,
# CensorFrom)), per cage-bin CensoredAnimals; a cage-bin with a censored animal has
# no rate (NA). Counts stay zero-filled and finite.
grid31_clock_profiles <- function(d, coverage, censor = NULL) {
  if (is.null(censor) && any(GRID31_CENSOR_COLUMNS %in% names(coverage)))
    stop("Coverage was built with a censor table; pass the same table to the clock profiles.", call. = FALSE)
  roster_check <- d %>% dplyr::distinct(AnimalID, System) %>% dplyr::count(AnimalID)
  if (any(roster_check$n != 1)) stop("An animal has multiple systems within CC4.", call. = FALSE)
  conflicts <- d %>% dplyr::group_by(AnimalID, System, DateTime) %>%
    dplyr::summarise(npos = dplyr::n_distinct(PositionID), .groups = "drop")
  if (any(conflicts$npos > 1)) stop("Conflicting simultaneous positions; resolve before profiling.", call. = FALSE)
  stream <- d %>% dplyr::arrange(AnimalID, System, DateTime) %>%
    dplyr::group_by(AnimalID, System) %>%
    dplyr::mutate(Crossing = !is.na(dplyr::lag(PositionID)) & PositionID != dplyr::lag(PositionID)) %>% dplyr::ungroup()
  stream$PhaseBlockIndex <- animalpos_phase_block_index(stream$DateTime)
  stream$BinIndex <- as.integer(floor((as.numeric(stream$DateTime) - (stream$PhaseBlockIndex * 43200 + 23400)) / 300))
  counts <- stream %>% dplyr::group_by(System, AnimalID, PhaseBlockIndex, BinIndex) %>%
    dplyr::summarise(ReadCount = dplyr::n(), Crossings = sum(Crossing), .groups = "drop")
  bins <- tidyr::crossing(coverage %>% dplyr::select(-ReadCount, -FirstRead, -LastRead, -MaxWithinBlockReadGapSec,
                                                     -dplyr::any_of(GRID31_CENSOR_COLUMNS)), BinIndex = 0:143) %>%
    dplyr::mutate(BinStart = BlockStart + BinIndex * 300, BinEnd = BinStart + 300) %>%
    dplyr::left_join(counts, by = c("System", "AnimalID", "PhaseBlockIndex", "BinIndex")) %>%
    dplyr::mutate(ReadCount = tidyr::replace_na(ReadCount, 0L), Crossings = tidyr::replace_na(Crossings, 0L),
                  SpanOverlapSec = pmax(0, pmin(as.numeric(AnimalLastRead), as.numeric(BinEnd)) -
                                         pmax(as.numeric(AnimalFirstRead), as.numeric(BinStart))),
                  MinutesFromPhaseStart = BinIndex * 5)
  if (!is.null(censor)) bins <- grid31_join_censor(bins, censor) %>%
    dplyr::mutate(CensoredSec = grid31_censored_sec(BinStart, BinEnd, CensorFrom))
  cages <- bins %>% dplyr::group_by(Batch, CageChange, System, Phase, PhaseLabel, PhaseNumber, PhaseBlockIndex,
                                   BinIndex, BinStart, BinEnd, MinutesFromPhaseStart, SessionSpansWholeBlock) %>%
    dplyr::summarise(ExpectedAnimals = dplyr::n(), AnimalsWithReads = sum(ReadCount > 0),
                     AnimalsSpanningBin = sum(SpanOverlapSec >= 300), ReadCount = sum(ReadCount), Crossings = sum(Crossings),
                     .groups = "drop") %>%
    dplyr::mutate(CompleteAnimalBracketing = ExpectedAnimals == AnimalsSpanningBin,
                  CrossingsPerExpectedAnimalHour = Crossings / (ExpectedAnimals * 300 / 3600))
  if (!is.null(censor)) {
    # A zero-filled count after the censor time is unobserved time, not inactivity.
    censored <- bins %>% dplyr::group_by(Batch, CageChange, System, PhaseBlockIndex, BinIndex) %>%
      dplyr::summarise(CensorFrom = dplyr::first(CensorFrom), CensorRecord = dplyr::first(CensorRecord),
                       CensoredAnimals = sum(CensoredSec > 0), .groups = "drop")
    cages <- cages %>% dplyr::left_join(censored, by = c("Batch", "CageChange", "System", "PhaseBlockIndex", "BinIndex")) %>%
      dplyr::mutate(CrossingsPerExpectedAnimalHour = ifelse(CensoredAnimals > 0, NA_real_, CrossingsPerExpectedAnimalHour))
  }
  list(animals = bins, cages = cages)
}

# Scan all 121 five-minute-aligned two-hour windows within each I3-I5 phase.
# Rank excess crossings over the SAME clock slots on I2, retaining the full scan
# as an audit. Select at most three non-overlapping positive candidates per cage
# and day for visual review. This is outcome-based localisation, not inference.
grid31_candidates <- function(cages) {
  scans <- list(); selected <- list(); eligibility <- list()
  groups <- cages %>% dplyr::filter(Phase == "Inactive", PhaseNumber %in% 3:5) %>%
    dplyr::group_by(Batch, CageChange, System, PhaseNumber) %>% dplyr::group_split()
  for (g in groups) {
    g <- g[order(g$BinIndex), ]
    ref <- cages %>% dplyr::filter(Batch == g$Batch[1], CageChange == g$CageChange[1],
                                  System == g$System[1], Phase == "Inactive", PhaseNumber == 2) %>% dplyr::arrange(BinIndex)
    key <- g[1, c("Batch", "CageChange", "System", "PhaseLabel", "PhaseNumber")]
    ok <- nrow(g) == 144 && nrow(ref) == 144 && all(g$SessionSpansWholeBlock) && all(ref$SessionSpansWholeBlock) &&
      all(g$CompleteAnimalBracketing) && all(ref$CompleteAnimalBracketing) && all(g$ExpectedAnimals == ref$ExpectedAnimals)
    # A censored cage-day (or reference) is never scanned and says why.
    censored <- if ("CensoredAnimals" %in% names(cages)) sort(unique(c(g$CensorRecord[g$CensoredAnimals > 0],
                                                                         ref$CensorRecord[ref$CensoredAnimals > 0]))) else character()
    ok <- ok && !length(censored)
    eligibility[[length(eligibility)+1L]] <- dplyr::mutate(key, EligibleForCandidateScan = ok,
      Reason = if (ok) "clock_span_and_animal_bracketing_pass; sensor_health_unverified"
               else if (length(censored)) paste0("censored_", paste(censored, collapse = "_"))
               else "incomplete_phase_or_animal_bracketing_or_reference")
    if (!ok) next
    windows <- lapply(0:120, function(i) {
      ix <- (i+1):(i+24)
      dplyr::mutate(key, BinIndex = i, CandidateStart = g$BinStart[i+1], CandidateEnd = g$BinStart[i+1] + 7200,
        ReferenceStart = ref$BinStart[i+1], ReferenceEnd = ref$BinStart[i+1] + 7200,
        Crossings = sum(g$Crossings[ix]), ReferenceCrossings = sum(ref$Crossings[ix]), ExpectedAnimals = g$ExpectedAnimals[1],
        ExcessCrossingsPerAnimalHour = (Crossings - ReferenceCrossings) / (ExpectedAnimals * 2),
        TimingStatus = "activity_inferred_unverified", EvidenceRole = "candidate_localisation_only_no_effect_test")
    })
    scan <- dplyr::bind_rows(windows)
    scans[[length(scans)+1L]] <- scan
    ranked <- scan[order(-scan$ExcessCrossingsPerAnimalHour, scan$BinIndex), ]
    keep <- integer()
    for (j in seq_len(nrow(ranked))) {
      if (ranked$ExcessCrossingsPerAnimalHour[j] <= 0 || length(keep) == 3) break
      if (!length(keep) || all(abs(ranked$BinIndex[j] - ranked$BinIndex[keep]) >= 24)) keep <- c(keep, j)
    }
    if (length(keep)) selected[[length(selected)+1L]] <- dplyr::mutate(ranked[keep, ], CandidateRank = seq_along(keep))
  }
  empty <- cages[0, c("Batch", "CageChange", "System", "PhaseLabel", "PhaseNumber")]
  empty <- dplyr::mutate(empty, BinIndex = integer(), CandidateStart = as.POSIXct(character(), tz = "UTC"),
    CandidateEnd = CandidateStart, ReferenceStart = CandidateStart, ReferenceEnd = CandidateStart,
    Crossings = integer(), ReferenceCrossings = integer(), ExpectedAnimals = integer(),
    ExcessCrossingsPerAnimalHour = numeric(), TimingStatus = character(), EvidenceRole = character())
  scan <- if (length(scans)) dplyr::bind_rows(scans) else empty
  candidates <- if (length(selected)) dplyr::bind_rows(selected) else dplyr::mutate(scan[0, ], CandidateRank = integer())
  list(scan = scan, candidates = candidates, eligibility = dplyr::bind_rows(eligibility))
}
