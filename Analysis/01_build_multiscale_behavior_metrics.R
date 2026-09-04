# ================================================================
# Build multiscale animal-level behavior metrics from RFID position data
# MMMSociability
# ================================================================
# Goal:
#   Produce canonical all_behavior_metrics.csv files used by downstream
#   behavioral dynamics scripts.
#
# Input:
#   preprocessed_data/*_preprocessed.csv
#
# Output:
#   analysis_ready/03_derived_metrics/{10sec,1min,5min,10min,30min}_based/all_behavior_metrics.csv
#   analysis_ready/03_derived_metrics/phase_based/all_behavior_metrics.csv
#   analysis_ready/03_derived_metrics/qc/multiscale_behavior_metrics_qc.csv
#   analysis_ready/03_derived_metrics/qc/animal_group_sex_assignment_qc.csv
#   analysis_ready/03_derived_metrics/qc/reference_ids_not_found_in_preprocessed_data.csv
#   analysis_ready/03_derived_metrics/qc/group_sex_assignment_summary.csv
#
# Recovering after an animal-identity correction (e.g. a canonical_animal_id()
# change in Functions/behavioral_dynamics_helpers.R):
#   The only supported recovery path is a full rerun of this script from the
#   preprocessed position data, which regenerates every fixed-width scale plus
#   phase_based from scratch. There is no supported in-place repair of a single
#   existing derived-metrics file. A forensic/testing-only single-file repair
#   helper (never equivalent to a full rebuild) lives in
#   Testing/audits/repair_existing_metrics_identity_utility.R.
#
# Key definitions:
#   Movement                  = number of RFID position transitions per animal/bin
#   MovementDistance          = summed Manhattan grid distance moved per animal/bin
#   Entropy                   = Shannon entropy of position occupancy seconds
#   ProximitySeconds          = summed same-position dyadic contact seconds
#   ProximityFraction         = ProximitySeconds / dyadic_observation_seconds
#   Proximity                 = backward-compatible alias for ProximityFraction
#   AdjacentProximitySeconds  = summed adjacent-position dyadic seconds
#   AdjacentProximityFraction = AdjacentProximitySeconds / dyadic_observation_seconds
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(parallel)
  library(future)
  library(furrr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"),
  file.path(dirname(tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) getwd())), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("duration_normalization_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")

# ------------------------------------------------
# USER INPUT
# ------------------------------------------------

existing_default_input_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/MMMSociability/preprocessed_data"
existing_default_output_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/03_derived_metrics"

input_dir <- getOption("mmm.preprocessed_dir", existing_default_input_dir)
output_root <- getOption("mmm.derived_metrics_dir", existing_default_output_root)
dataset_id <- getOption("mmm.dataset_id", "sis_cc")

# Optional animal reference lists. These are one-ID-per-line CSV/text files.
# Matching is done after the shared canonical animal-identity normalization, so
# numeric aliases such as 0004/4 and 00303/303 are handled consistently while
# alphanumeric IDs such as OR004 remain intact.
sus_animals_file <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv"
con_animals_file <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv"

# If TRUE, animals not listed as CON or SUS are assigned RES. This is appropriate
# only when all remaining animals in the preprocessed files are stress-exposed
# resilient animals.
assign_unlisted_animals_as_res <- TRUE

bin_specs <- tibble::tribble(
  ~bin_label, ~bin_size_sec,
  "10sec",    10,
  "1min",     60,
  "5min",     300,
  "10min",    600,
  "30min",    1800
)
requested_bin_labels <- getOption("mmm.bin_labels", bin_specs$bin_label)
if (!all(requested_bin_labels %in% bin_specs$bin_label)) {
  stop(
    "Unknown requested bin label(s): ",
    paste(setdiff(requested_bin_labels, bin_specs$bin_label), collapse = ", "),
    call. = FALSE
  )
}
bin_specs <- bin_specs %>% filter(bin_label %in% requested_bin_labels)

metadata_file <- NULL

# Diagnostic-only gap threshold.
#
# HISTORY: long_gap_threshold_sec was introduced as a QC flag ("retained but
# flagged in QC") and two days later silently repurposed into a metric-validity
# rule -- intervals longer than an hour were dropped from every metric, and
# position changes separated by more than an hour were not counted as movement.
# Neither exclusion was ever justified in writing.
#
# The exclusion was also provably dead for occupancy: preprocessing used to
# insert a synthetic row at every half-hour mark, so no interval could exceed
# 1800 s and the > 3600 s test never fired. Now that synthetic rows are gone,
# intervals span the real time between reads, so leaving the exclusion active
# would begin silently deleting genuine occupancy time that was previously
# retained. The flag is therefore computed for QC only and never filters.
system_event_gap_threshold_sec <- 3600
exclude_long_gaps_from_metrics <- FALSE

use_multicore <- getOption("mmm.use_multicore", TRUE)
n_cores <- getOption("mmm.n_cores", max(1L, parallel::detectCores(logical = TRUE) - 1L))

if (use_multicore && requireNamespace("furrr", quietly = TRUE) && requireNamespace("future", quietly = TRUE)) {
  future::plan(future::multisession, workers = n_cores)
  map_dfr_parallel <- furrr::future_map_dfr
  map_parallel <- furrr::future_map
  pmap_parallel <- furrr::future_pmap
  message("Parallel processing enabled with ", n_cores, " workers.")
} else {
  map_dfr_parallel <- purrr::map_dfr
  map_parallel <- purrr::map
  pmap_parallel <- purrr::pmap
  message("Parallel processing disabled; using sequential purrr.")
}

ensure_dir(output_root)
ensure_dir(file.path(output_root, "qc"))
analysis_output_dirs(output_root)
write_output_manifest(
  output_root,
  script_name = "01_build_multiscale_behavior_metrics.R",
  analysis_name = "canonical multiscale behavior metrics",
  primary_tables = c(
    "10sec_based/all_behavior_metrics.csv",
    "1min_based/all_behavior_metrics.csv",
    "5min_based/all_behavior_metrics.csv",
    "10min_based/all_behavior_metrics.csv",
    "30min_based/all_behavior_metrics.csv",
    "qc/multiscale_behavior_metrics_qc.csv",
    "qc/animal_group_sex_assignment_qc.csv"
  ),
  notes = c("Downstream scripts should prefer normalized ProximityFraction and per-hour rate fields when comparing unequal durations.")
)

# ------------------------------------------------
# POSITION GEOMETRY
# ------------------------------------------------

position_grid <- tibble(
  PositionID = 1:8,
  x_grid = rep(1:4, times = 2),
  y_grid = rep(1:2, each = 4)
)

pair_distance_lookup <- tidyr::crossing(
  PositionID_A = position_grid$PositionID,
  PositionID_B = position_grid$PositionID
) %>%
  left_join(position_grid %>% rename(PositionID_A = PositionID, x_A = x_grid, y_A = y_grid), by = "PositionID_A") %>%
  left_join(position_grid %>% rename(PositionID_B = PositionID, x_B = x_grid, y_B = y_grid), by = "PositionID_B") %>%
  mutate(
    grid_distance = abs(x_A - x_B) + abs(y_A - y_B),
    same_position = PositionID_A == PositionID_B,
    adjacent_position = grid_distance == 1
  ) %>%
  select(PositionID_A, PositionID_B, grid_distance, same_position, adjacent_position)

calc_entropy <- function(seconds_by_position) {
  x <- seconds_by_position[is.finite(seconds_by_position) & seconds_by_position > 0]
  if (length(x) == 0 || sum(x) <= 0) return(NA_real_)
  p <- x / sum(x)
  -sum(p * log2(p))
}

safe_divide <- function(num, den) {
  ifelse(is.finite(den) & den > 0, num / den, NA_real_)
}

read_animal_id_list <- function(path, label) {
  if (is.null(path) || !file.exists(path)) {
    warning("Animal reference file not found for ", label, ": ", path, call. = FALSE)
    return(character())
  }

  readr::read_lines(path, progress = FALSE) %>%
    canonical_animal_id() %>%
    purrr::discard(~ is.na(.x) || .x == "") %>%
    unique()
}

assign_batch_sex <- function(batch) {
  batch_norm <- toupper(stringr::str_trim(as.character(batch)))
  dplyr::case_when(
    batch_norm %in% c("B1", "B2", "B5") ~ "Male",
    batch_norm %in% c("B3", "B4", "B6") ~ "Female",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------
# LOAD PREPROCESSED POSITION DATA
# ------------------------------------------------

read_preprocessed_position_file <- function(path) {
  dat <- readr::read_csv(path, show_col_types = FALSE)
  required <- c("DateTime", "AnimalID", "System", "PositionID")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop("Missing required columns in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }

  dat %>%
    mutate(
      DateTime = suppressWarnings(as.POSIXct(DateTime, tz = "UTC")),
      AnimalID_raw = as.character(AnimalID),
      AnimalNum = canonical_animal_id(AnimalID_raw),
      # Keep AnimalID as a compatibility alias for the canonical analysis ID.
      # The unmodified source spelling remains available in AnimalID_raw.
      AnimalID = AnimalNum,
      System = as.character(System),
      PositionID = suppressWarnings(as.integer(PositionID)),
      SourceFile = basename(path),
      Dataset = if ("Dataset" %in% names(.)) as.character(Dataset) else dataset_id,
      RawParadigm = if ("RawParadigm" %in% names(.)) as.character(RawParadigm) else NA_character_,
      Batch = if ("Batch" %in% names(.)) as.character(Batch) else str_extract(basename(path), "B[0-9]+"),
      CageChange = if ("CageChange" %in% names(.)) as.character(CageChange) else coalesce(str_extract(basename(path), "CC[0-9]+"), NA_character_),
      Epoch = if ("Epoch" %in% names(.)) as.character(Epoch) else CageChange,
      Phase = if ("Phase" %in% names(.)) as.character(Phase) else if_else(
        format(DateTime, "%H:%M", tz = "UTC") >= "18:30" | format(DateTime, "%H:%M", tz = "UTC") < "06:30",
        "Active", "Inactive"
      ),
      Group = if ("Group" %in% names(.)) as.character(Group) else NA_character_,
      Sex = if ("Sex" %in% names(.)) as.character(Sex) else NA_character_,
      ConsecActive = if ("ConsecActive" %in% names(.)) suppressWarnings(as.integer(ConsecActive)) else NA_integer_,
      ConsecInactive = if ("ConsecInactive" %in% names(.)) suppressWarnings(as.integer(ConsecInactive)) else NA_integer_,
      HalfHoursElapsed = if ("HalfHoursElapsed" %in% names(.)) suppressWarnings(as.numeric(HalfHoursElapsed)) else NA_real_,
      MinutesOfDay = if ("MinutesOfDay" %in% names(.)) suppressWarnings(as.integer(MinutesOfDay)) else NA_integer_,
      CookieWindowPrimary = if ("CookieWindowPrimary" %in% names(.)) as.logical(CookieWindowPrimary) else NA,
      CookieSubWindow = if ("CookieSubWindow" %in% names(.)) as.character(CookieSubWindow) else NA_character_,
      CookieHabEpoch = if ("CookieHabEpoch" %in% names(.)) as.character(CookieHabEpoch) else NA_character_
    ) %>%
    filter(!is.na(DateTime), !is.na(AnimalID), !is.na(System), is.finite(PositionID), PositionID > 0) %>%
    arrange(SourceFile, Batch, CageChange, System, DateTime, AnimalID)
}

files <- list.files(input_dir, pattern = "_preprocessed\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No preprocessed files found in ", input_dir, call. = FALSE)

message("Found ", length(files), " preprocessed files.")
all_pos <- map_dfr_parallel(files, read_preprocessed_position_file, .progress = TRUE)

if (!is.null(metadata_file) && file.exists(metadata_file)) {
  meta <- read_behavior_table(metadata_file)
  meta_animal_col <- first_existing_col(meta, c("AnimalID", "AnimalNum", "Animal", "MouseID", "Mouse", "ID", "RFID"), TRUE, "metadata animal column")
  meta_group_col <- first_existing_col(meta, c("Group", "Phenotype", "Condition", "Treatment", "StressGroup"), FALSE, "metadata group column")
  meta_sex_col <- first_existing_col(meta, c("Sex", "sex"), FALSE, "metadata sex column")

  meta_small <- meta %>%
    transmute(
      AnimalNum = canonical_animal_id(.data[[meta_animal_col]]),
      MetaGroup = if (!is.na(meta_group_col)) as.character(.data[[meta_group_col]]) else NA_character_,
      MetaSex = if (!is.na(meta_sex_col)) as.character(.data[[meta_sex_col]]) else NA_character_
    ) %>%
    distinct(AnimalNum, .keep_all = TRUE)

  all_pos <- all_pos %>%
    left_join(meta_small, by = "AnimalNum") %>%
    mutate(Group = coalesce(Group, MetaGroup), Sex = coalesce(Sex, MetaSex)) %>%
    select(-MetaGroup, -MetaSex)
}

# ------------------------------------------------
# ASSIGN GROUP AND SEX METADATA
# ------------------------------------------------

sus_animals <- read_animal_id_list(sus_animals_file, "SUS")
con_animals <- read_animal_id_list(con_animals_file, "CON")

reference_overlap <- intersect(sus_animals, con_animals)
if (length(reference_overlap) > 0) {
  stop(
    "The following normalized AnimalIDs occur in both SUS and CON reference files: ",
    paste(reference_overlap, collapse = ", "),
    call. = FALSE
  )
}

all_pos <- all_pos %>%
  mutate(
    AnimalID_norm = canonical_animal_id(AnimalNum),
    Batch_norm = toupper(str_trim(Batch)),
    ReferenceGroup = case_when(
      AnimalID_norm %in% sus_animals ~ "SUS",
      AnimalID_norm %in% con_animals ~ "CON",
      assign_unlisted_animals_as_res ~ "RES",
      TRUE ~ NA_character_
    ),
    ReferenceSex = assign_batch_sex(Batch),
    Group = coalesce(ReferenceGroup, Group),
    Sex = coalesce(ReferenceSex, Sex)
  )

# Raw aliases may map many-to-one, but phenotype and sex must be unique on the
# canonical analysis identity before any metric aggregation occurs.
identity_conflicts <- all_pos %>%
  distinct(AnimalNum, Group, Sex) %>%
  group_by(AnimalNum) %>%
  summarise(
    n_groups = n_distinct(Group, na.rm = TRUE),
    n_sexes = n_distinct(Sex, na.rm = TRUE),
    groups = paste(sort(unique(na.omit(Group))), collapse = "|"),
    sexes = paste(sort(unique(na.omit(Sex))), collapse = "|"),
    .groups = "drop"
  ) %>%
  filter(n_groups > 1L | n_sexes > 1L)
if (nrow(identity_conflicts) > 0L) {
  stop(
    "Canonical animal identity has conflicting Group or Sex metadata:\n",
    paste(utils::capture.output(print(identity_conflicts, n = Inf)), collapse = "\n"),
    call. = FALSE
  )
}

animal_group_sex_qc <- all_pos %>%
  distinct(AnimalID_raw, AnimalNum, AnimalID_norm, Batch, Batch_norm, Sex, Group, ReferenceGroup, ReferenceSex) %>%
  arrange(Batch_norm, Group, AnimalID_norm)

write_table(
  animal_group_sex_qc,
  file.path(output_root, "qc", "animal_group_sex_assignment_qc.csv")
)

reference_ids_not_found <- tibble(
  AnimalID_norm = c(sus_animals, con_animals),
  ReferenceGroup = c(rep("SUS", length(sus_animals)), rep("CON", length(con_animals)))
) %>%
  distinct() %>%
  anti_join(all_pos %>% distinct(AnimalID_norm), by = "AnimalID_norm") %>%
  arrange(ReferenceGroup, AnimalID_norm)

write_table(
  reference_ids_not_found,
  file.path(output_root, "qc", "reference_ids_not_found_in_preprocessed_data.csv")
)

group_sex_assignment_summary <- animal_group_sex_qc %>%
  count(Batch_norm, Sex, Group, name = "n_animals") %>%
  arrange(Batch_norm, Sex, Group)

write_table(
  group_sex_assignment_summary,
  file.path(output_root, "qc", "group_sex_assignment_summary.csv")
)

if (nrow(reference_ids_not_found) > 0) {
  warning(
    nrow(reference_ids_not_found),
    " reference AnimalIDs were not found in the preprocessed data. Check qc/reference_ids_not_found_in_preprocessed_data.csv",
    call. = FALSE
  )
}

if (any(is.na(animal_group_sex_qc$Sex))) {
  warning("Some animals could not be assigned Sex from Batch. Check qc/animal_group_sex_assignment_qc.csv", call. = FALSE)
}

# ------------------------------------------------
# INTERVAL RECONSTRUCTION
# ------------------------------------------------

last_meta_before <- function(dat_sys, t0) {
  dat_sys %>%
    filter(DateTime <= t0) %>%
    slice_tail(n = 1) %>%
    transmute(
      SourceFile = first(SourceFile),
      Dataset = first(Dataset),
      RawParadigm = first(RawParadigm),
      Batch = first(Batch),
      CageChange = first(CageChange),
      Epoch = first(Epoch),
      System = first(System),
      Phase = first(Phase),
      ConsecActive = first(ConsecActive),
      ConsecInactive = first(ConsecInactive),
      MinutesOfDay = first(MinutesOfDay),
      CookieWindowPrimary = first(CookieWindowPrimary),
      CookieSubWindow = first(CookieSubWindow),
      CookieHabEpoch = first(CookieHabEpoch)
    )
}

make_occupancy_intervals_one_system <- function(dat_sys) {
  dat_sys <- dat_sys %>% arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))
  event_times <- sort(unique(dat_sys$DateTime))
  if (length(animals) == 0 || length(event_times) < 2) return(tibble())

  out <- vector("list", length(event_times) - 1)
  current_pos <- rep(NA_integer_, length(animals)); names(current_pos) <- animals
  current_group <- rep(NA_character_, length(animals)); names(current_group) <- animals
  current_sex <- rep(NA_character_, length(animals)); names(current_sex) <- animals

  for (i in seq_len(length(event_times) - 1)) {
    t0 <- event_times[i]
    t1 <- event_times[i + 1]
    duration_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    if (!is.finite(duration_sec) || duration_sec <= 0) next

    updates <- dat_sys %>% filter(DateTime == t0)
    if (nrow(updates) > 0) {
      for (j in seq_len(nrow(updates))) {
        a <- updates$AnimalID[j]
        current_pos[a] <- updates$PositionID[j]
        if (!is.na(updates$Group[j])) current_group[a] <- updates$Group[j]
        if (!is.na(updates$Sex[j])) current_sex[a] <- updates$Sex[j]
      }
    }

    valid <- is.finite(current_pos) & current_pos > 0
    if (!any(valid)) next

    interval_meta <- last_meta_before(dat_sys, t0)
    out[[i]] <- tibble(
      AnimalNum = names(current_pos)[valid],
      AnimalID = names(current_pos)[valid],
      PositionID = as.integer(current_pos[valid]),
      Group = current_group[names(current_pos)[valid]],
      Sex = current_sex[names(current_pos)[valid]],
      IntervalStart = t0,
      IntervalEnd = t1,
      DurationSec = duration_sec,
      SystemEventGap = duration_sec > system_event_gap_threshold_sec
    ) %>%
      bind_cols(interval_meta[rep(1, sum(valid)), ])
  }

  bind_rows(out)
}

make_movement_events <- function(pos_tbl) {
  pos_tbl %>%
    arrange(SourceFile, Batch, CageChange, System, AnimalID, DateTime) %>%
    group_by(SourceFile, Batch, CageChange, System, AnimalID) %>%
    mutate(
      PrevDateTime = lag(DateTime),
      PrevPositionID = lag(PositionID),
      # Kept as provenance/QC only. It used to gate movement validity via
      # "<= long_gap_threshold_sec", which deleted every first position change
      # following a long rest. AnimalPos carries no read-duration field, so a
      # long interval since the previous position change is not evidence that
      # the next change is invalid -- it is the expected signature of an animal
      # that stayed put and then moved.
      time_since_last_genuine_position_event_sec =
        as.numeric(difftime(DateTime, PrevDateTime, units = "secs")),
      PositionChanged = is.finite(PrevPositionID) &
        PositionID != PrevPositionID
    ) %>%
    ungroup() %>%
    filter(PositionChanged) %>%
    left_join(
      pair_distance_lookup %>% rename(PrevPositionID = PositionID_A, PositionID = PositionID_B),
      by = c("PrevPositionID", "PositionID")
    ) %>%
    transmute(
      SourceFile, Batch, CageChange, System, Phase, ConsecActive, ConsecInactive,
      AnimalNum = AnimalID,
      AnimalID,
      Group,
      Sex,
      EventTime = DateTime,
      time_since_last_genuine_position_event_sec,
      MovementEvent = 1,
      MovementDistanceEvent = grid_distance
    )
}

make_dyadic_intervals_one_system <- function(dat_sys) {
  dat_sys <- dat_sys %>% arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))
  event_times <- sort(unique(dat_sys$DateTime))
  if (length(animals) < 2 || length(event_times) < 2) return(tibble())

  animal_pairs <- combn(animals, 2, simplify = FALSE)
  out <- vector("list", length(event_times) - 1)
  current_pos <- rep(NA_integer_, length(animals)); names(current_pos) <- animals

  for (i in seq_len(length(event_times) - 1)) {
    t0 <- event_times[i]
    t1 <- event_times[i + 1]
    duration_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    if (!is.finite(duration_sec) || duration_sec <= 0) next

    updates <- dat_sys %>% filter(DateTime == t0)
    if (nrow(updates) > 0) {
      for (j in seq_len(nrow(updates))) {
        current_pos[updates$AnimalID[j]] <- updates$PositionID[j]
      }
    }

    pair_tbl <- map_dfr(animal_pairs, function(p) {
      pos_a <- current_pos[p[1]]
      pos_b <- current_pos[p[2]]
      if (!is.finite(pos_a) || !is.finite(pos_b)) return(tibble())
      tibble(Focal = p[1], Partner = p[2], PositionID_A = as.integer(pos_a), PositionID_B = as.integer(pos_b))
    })
    if (nrow(pair_tbl) == 0) next

    interval_meta <- last_meta_before(dat_sys, t0)
    out[[i]] <- pair_tbl %>%
      mutate(
        IntervalStart = t0,
        IntervalEnd = t1,
        DurationSec = duration_sec,
        SystemEventGap = duration_sec > system_event_gap_threshold_sec
      ) %>%
      bind_cols(interval_meta[rep(1, nrow(pair_tbl)), ]) %>%
      left_join(pair_distance_lookup, by = c("PositionID_A", "PositionID_B"))
  }

  bind_rows(out)
}

message("Building occupancy intervals...")
occupancy_intervals <- all_pos %>%
  group_by(SourceFile, Batch, CageChange, System) %>%
  group_split() %>%
  map_dfr_parallel(make_occupancy_intervals_one_system, .progress = TRUE)

message("\nBuilding movement events...")
movement_events <- make_movement_events(all_pos)

message("Building dyadic proximity intervals...")
dyadic_intervals <- all_pos %>%
  group_by(SourceFile, Batch, CageChange, System) %>%
  group_split() %>%
  map_dfr_parallel(make_dyadic_intervals_one_system, .progress = TRUE)

# ------------------------------------------------
# AGGREGATION BOUNDARIES
# ------------------------------------------------
# Phase is a grouping variable, so an interval spanning 06:30 or 18:30 has to be
# split there and each piece labelled with the phase it actually falls in.
# Preprocessing used to guarantee a row at every boundary by inserting synthetic
# position observations; the boundary is now applied as an interval intersection
# using the same generic splitter as time binning.
#
# Labels come from the phase blocks that carry genuine observations in the same
# session, so they are exactly the labels preprocessing assigned -- they are not
# recomputed, because remove_phases() deletes whole phase blocks and recomputing
# from the surviving timestamps would renumber the survivors.
#
# Pieces landing in a phase block with no observations at all are dropped. Those
# blocks were deliberately removed by remove_phases() (the first Inactive phase,
# and any phase beyond the fourth), so they lie outside the analysis window.
# Before this refactor such spans were removed only as a side effect of the
# > 1 h LongGap exclusion; the exclusion is gone, so the epoch boundary is now
# enforced on its own terms and reported below rather than inferred from a
# duration threshold.

phase_block_reference <- all_pos %>%
  mutate(PhaseBlockIndex = animalpos_phase_block_index(DateTime)) %>%
  distinct(SourceFile, PhaseBlockIndex, Phase, ConsecActive, ConsecInactive)

.dup_phase_blocks <- phase_block_reference %>%
  count(SourceFile, PhaseBlockIndex) %>%
  filter(n > 1)
if (nrow(.dup_phase_blocks) > 0) {
  stop("Phase labels are not unique within a session for ", nrow(.dup_phase_blocks),
       " (SourceFile, PhaseBlockIndex) combinations; phase metadata is inconsistent.",
       call. = FALSE)
}

epoch_inclusion_audit <- list()

apply_phase_boundaries <- function(intervals, label) {
  if (nrow(intervals) == 0) return(intervals)
  n_before   <- nrow(intervals)
  sec_before <- sum(intervals$DurationSec, na.rm = TRUE)

  split <- animalpos_split_intervals(intervals, period_sec = NULL, split_phase = TRUE)
  if (abs(sum(split$DurationSec, na.rm = TRUE) - sec_before) > 1e-3) {
    stop("Phase splitting did not conserve duration for ", label, call. = FALSE)
  }

  labelled <- split %>%
    mutate(PhaseBlockIndex = animalpos_phase_block_index(IntervalStart)) %>%
    select(-any_of(c("Phase", "ConsecActive", "ConsecInactive"))) %>%
    left_join(phase_block_reference, by = c("SourceFile", "PhaseBlockIndex"))

  unobserved <- is.na(labelled$Phase)
  epoch_inclusion_audit[[label]] <<- tibble(
    interval_table                     = label,
    n_intervals_before_split           = n_before,
    n_pieces_after_split               = nrow(split),
    n_pieces_in_unobserved_phase_block = sum(unobserved),
    seconds_before_split               = sec_before,
    seconds_in_unobserved_phase_blocks = sum(labelled$DurationSec[unobserved], na.rm = TRUE),
    seconds_retained                   = sum(labelled$DurationSec[!unobserved], na.rm = TRUE)
  )
  message(sprintf(
    "  %s: %d intervals -> %d phase-bounded pieces; dropped %d pieces (%.1f h) lying in phase blocks with no observations",
    label, n_before, nrow(split), sum(unobserved),
    sum(labelled$DurationSec[unobserved], na.rm = TRUE) / 3600))

  labelled %>%
    filter(!is.na(Phase)) %>%
    select(-PhaseBlockIndex)
}

message("
Applying phase boundaries to interval tables...")
occupancy_intervals <- apply_phase_boundaries(occupancy_intervals, "occupancy")
dyadic_intervals    <- apply_phase_boundaries(dyadic_intervals, "dyadic")

# The bin-level aggregations below reuse the full interval/event tables. Running
# them through furrr would export those large objects to every worker.
if (use_multicore && requireNamespace("future", quietly = TRUE)) {
  future::plan(future::sequential)
  message("\nParallel interval reconstruction complete; aggregating scales sequentially to avoid exporting large interval tables.")
}

# ------------------------------------------------
# AGGREGATION HELPERS
# ------------------------------------------------

add_time_bin <- function(dat, time_col, bin_size_sec) {
  dat %>%
    mutate(
      BinStart = as.POSIXct(
        floor(as.numeric(.data[[time_col]]) / bin_size_sec) * bin_size_sec,
        origin = "1970-01-01",
        tz = "UTC"
      )
    )
}

# Retained as an explicit, auditable no-op so that any future attempt to
# reintroduce an interval-validity filter has to be a deliberate edit rather
# than a silent flag flip. SystemEventGap is diagnostic metadata only.
filter_metric_intervals <- function(dat) {
  if (isTRUE(exclude_long_gaps_from_metrics)) {
    stop("exclude_long_gaps_from_metrics = TRUE would silently drop genuine ",
         "occupancy time. Excluding intervals by duration is a scientific ",
         "decision that must be made explicitly, not via this flag.",
         call. = FALSE)
  }
  dat
}

add_phase_number <- function(dat) {
  dat %>%
    mutate(
      PhaseNumber = case_when(
        Phase == "Active" ~ ConsecActive,
        Phase == "Inactive" ~ ConsecInactive,
        TRUE ~ NA_integer_
      )
    )
}

crop_after_second_cc4_phase <- function(dat) {
  dat %>%
    filter(!(toupper(CageChange) == "CC4" &
               tolower(Phase) %in% c("active", "inactive") &
               !is.na(PhaseNumber) &
               PhaseNumber > 2L))
}

split_intervals_to_bins <- function(dat, bin_size_sec) {
  dat <- dat %>%
    filter_metric_intervals() %>%
    filter(
      !is.na(IntervalStart),
      !is.na(IntervalEnd),
      is.finite(DurationSec),
      DurationSec > 0
    )

  if (nrow(dat) == 0) {
    return(dat %>%
             mutate(
               BinSizeSec = bin_size_sec,
               BinStart = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
             ) %>%
             slice(0))
  }

  # One generic interval/boundary intersection is used for every aggregation
  # boundary in the pipeline (time bins here, phase boundaries above). The test
  # suite cross-validates it against both a loop reference and the pre-refactor
  # Stage 01 binning arithmetic, at every bin size in use.
  out <- animalpos_split_intervals_one_grid(dat, bin_size_sec, 0)
  out$BinSizeSec <- bin_size_sec
  out$BinStart <- as.POSIXct(
    floor(as.numeric(out$IntervalStart) / bin_size_sec) * bin_size_sec,
    origin = "1970-01-01", tz = "UTC"
  )
  out
}

make_time_index <- function(dat) {
  dat %>%
    group_by(SourceFile, Batch, CageChange, System) %>%
    mutate(TimeIndex = as.numeric(difftime(BinStart, min(BinStart, na.rm = TRUE), units = "secs")) / first(BinSizeSec)) %>%
    ungroup()
}

collapse_character_annotation <- function(x) {
  x <- unique(stats::na.omit(as.character(x)))
  if (length(x) == 0) return(NA_character_)
  if (length(x) == 1) return(x)
  paste(sort(x), collapse = "|")
}

add_cookiehab_bin_annotations <- function(out, bin_size_sec) {
  if (!identical(dataset_id, "cookiehab")) return(out)
  if (!all(c("CookieWindowPrimary", "CookieHabEpoch") %in% names(occupancy_intervals))) return(out)

  annotation_tbl <- occupancy_intervals %>%
    split_intervals_to_bins(bin_size_sec) %>%
    group_by(SourceFile, Batch, CageChange, System, BinSizeSec, BinStart, AnimalNum, AnimalID) %>%
    summarise(
      Dataset = collapse_character_annotation(Dataset),
      RawParadigm = collapse_character_annotation(RawParadigm),
      Epoch = collapse_character_annotation(Epoch),
      MinutesOfDay = if (all(is.na(MinutesOfDay))) NA_integer_ else as.integer(min(MinutesOfDay, na.rm = TRUE)),
      CookieWindowPrimary = any(CookieWindowPrimary, na.rm = TRUE),
      CookieSubWindow = collapse_character_annotation(CookieSubWindow),
      CookieHabEpoch = collapse_character_annotation(CookieHabEpoch),
      .groups = "drop"
    ) %>%
    mutate(
      MinutesOfDay = if_else(is.infinite(MinutesOfDay), NA_integer_, MinutesOfDay),
      CookieSubWindow = na_if(CookieSubWindow, "")
    )

  out %>%
    left_join(
      annotation_tbl,
      by = c("SourceFile", "Batch", "CageChange", "System", "BinSizeSec", "BinStart", "AnimalNum", "AnimalID")
    )
}

summarise_occupancy_by_bin <- function(bin_size_sec) {
  occupancy_intervals %>%
    split_intervals_to_bins(bin_size_sec) %>%
    add_phase_number() %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum, AnimalID, Group, Sex, PositionID) %>%
    summarise(PositionSeconds = sum(DurationSec, na.rm = TRUE), .groups = "drop") %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum, AnimalID, Group, Sex) %>%
    summarise(
      observation_seconds = sum(PositionSeconds, na.rm = TRUE),
      Entropy = calc_entropy(PositionSeconds),
      DominantPosition = PositionID[which.max(PositionSeconds)],
      n_positions_visited = sum(PositionSeconds > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    make_time_index()
}

summarise_movement_by_bin <- function(bin_size_sec) {
  if (nrow(movement_events) == 0) return(tibble())
  movement_events %>%
    add_time_bin("EventTime", bin_size_sec) %>%
    add_phase_number() %>%
    mutate(BinSizeSec = bin_size_sec) %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum, AnimalID) %>%
    summarise(
      Movement = sum(MovementEvent, na.rm = TRUE),
      MovementDistance = sum(MovementDistanceEvent, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_proximity_by_bin <- function(bin_size_sec) {
  if (nrow(dyadic_intervals) == 0) return(tibble())

  dyad_split <- dyadic_intervals %>% split_intervals_to_bins(bin_size_sec)

  dyad_long <- dyad_split %>%
    add_phase_number() %>%
    mutate(
      same_position_seconds = if_else(same_position, DurationSec, 0),
      adjacent_seconds = if_else(adjacent_position, DurationSec, 0),
      weighted_grid_distance = grid_distance * DurationSec
    ) %>%
    select(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, DurationSec,
           same_position_seconds, adjacent_seconds, weighted_grid_distance, Focal, Partner) %>%
    bind_rows(
      dyad_split %>%
        add_phase_number() %>%
        mutate(
          same_position_seconds = if_else(same_position, DurationSec, 0),
          adjacent_seconds = if_else(adjacent_position, DurationSec, 0),
          weighted_grid_distance = grid_distance * DurationSec
        ) %>%
        transmute(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, DurationSec,
                  same_position_seconds, adjacent_seconds, weighted_grid_distance,
                  Focal = Partner, Partner = Focal)
    )

  dyad_long %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum = Focal) %>%
    summarise(
      dyadic_observation_seconds = sum(DurationSec, na.rm = TRUE),
      ProximitySeconds = sum(same_position_seconds, na.rm = TRUE),
      AdjacentProximitySeconds = sum(adjacent_seconds, na.rm = TRUE),
      ProximityFraction = safe_divide(ProximitySeconds, dyadic_observation_seconds),
      AdjacentProximityFraction = safe_divide(AdjacentProximitySeconds, dyadic_observation_seconds),
      Proximity = ProximityFraction,
      AdjacentProximity = AdjacentProximityFraction,
      MeanGridDistanceToOthers = safe_divide(sum(weighted_grid_distance, na.rm = TRUE), dyadic_observation_seconds),
      n_dyadic_intervals = n(),
      .groups = "drop"
    )
}

ensure_metric_columns <- function(out) {
  defaults <- list(
    Movement = NA_real_,
    MovementDistance = NA_real_,
    ProximitySeconds = NA_real_,
    ProximityFraction = NA_real_,
    AdjacentProximitySeconds = NA_real_,
    AdjacentProximityFraction = NA_real_,
    MeanGridDistanceToOthers = NA_real_,
    dyadic_observation_seconds = NA_real_,
    n_dyadic_intervals = NA_integer_
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(out)) out[[nm]] <- defaults[[nm]]
  }

  out
}

standardize_metric_output <- function(out, bin_label) {
  out %>%
    ensure_metric_columns() %>%
    mutate(
      Movement = replace_na(Movement, 0),
      MovementDistance = replace_na(MovementDistance, 0),
      ProximitySeconds = replace_na(ProximitySeconds, NA_real_),
      ProximityFraction = replace_na(ProximityFraction, NA_real_),
      AdjacentProximitySeconds = replace_na(AdjacentProximitySeconds, NA_real_),
      AdjacentProximityFraction = replace_na(AdjacentProximityFraction, NA_real_),
      Proximity = ProximityFraction,
      AdjacentProximity = AdjacentProximityFraction,
      MeanGridDistanceToOthers = replace_na(MeanGridDistanceToOthers, NA_real_),
      total_observation_duration_hours = observation_seconds / 3600,
      MovementPerHour = if_else(is.finite(total_observation_duration_hours) & total_observation_duration_hours > 0,
                                Movement / total_observation_duration_hours, NA_real_),
      MovementDistancePerHour = if_else(is.finite(total_observation_duration_hours) & total_observation_duration_hours > 0,
                                        MovementDistance / total_observation_duration_hours, NA_real_),
      ProximitySecondsPerHour = if_else(is.finite(total_observation_duration_hours) & total_observation_duration_hours > 0,
                                        ProximitySeconds / total_observation_duration_hours, NA_real_),
      Group = if_else(is.na(Group) | Group == "", "All", Group),
      Sex = if_else(is.na(Sex) | Sex == "", "All", Sex),
      BinLabel = bin_label
    )
}

build_bin_metrics <- function(bin_label, bin_size_sec) {
  message("Aggregating animal-level metrics for ", bin_label, " bins...")

  occ <- summarise_occupancy_by_bin(bin_size_sec)
  mov <- summarise_movement_by_bin(bin_size_sec)
  prox <- summarise_proximity_by_bin(bin_size_sec)

  out <- occ %>%
    left_join(mov, by = c("SourceFile", "Batch", "CageChange", "System", "Phase", "PhaseNumber", "BinSizeSec", "BinStart", "AnimalNum", "AnimalID")) %>%
    left_join(prox, by = c("SourceFile", "Batch", "CageChange", "System", "Phase", "PhaseNumber", "BinSizeSec", "BinStart", "AnimalNum")) %>%
    crop_after_second_cc4_phase() %>%
    standardize_metric_output(bin_label) %>%
    add_cookiehab_bin_annotations(bin_size_sec) %>%
    select(
      AnimalNum, AnimalID, Batch, CageChange, System, Group, Sex, Phase,
      any_of(c("Dataset", "RawParadigm", "Epoch", "MinutesOfDay", "CookieWindowPrimary", "CookieSubWindow", "CookieHabEpoch")),
      BinLabel, BinSizeSec, BinStart, TimeIndex,
      Movement, MovementPerHour, MovementDistance, MovementDistancePerHour,
      Proximity, ProximitySeconds, ProximityFraction,
      ProximitySecondsPerHour,
      AdjacentProximity, AdjacentProximitySeconds, AdjacentProximityFraction,
      MeanGridDistanceToOthers,
      Entropy, DominantPosition, n_positions_visited,
      observation_seconds, dyadic_observation_seconds, n_dyadic_intervals,
      SourceFile
    ) %>%
    arrange(Batch, CageChange, System, AnimalNum, BinStart)

  out_dir <- file.path(output_root, paste0(bin_label, "_based"))
  ensure_dir(out_dir)
  write_table(out, file.path(out_dir, "all_behavior_metrics.csv"))
  write_epoch_duration_qc(out, out_dir, metric_source = paste0("03_", bin_label, "_behavior_metrics"), bin_size_sec = bin_size_sec)
  out
}

build_phase_metrics <- function() {
  message("Aggregating animal-level metrics by phase...")

  occ <- occupancy_intervals %>%
    filter_metric_intervals() %>%
    mutate(PhaseNumber = case_when(Phase == "Active" ~ ConsecActive, Phase == "Inactive" ~ ConsecInactive, TRUE ~ NA_integer_)) %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum, AnimalID, Group, Sex, PositionID) %>%
    summarise(PositionSeconds = sum(DurationSec, na.rm = TRUE), .groups = "drop") %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum, AnimalID, Group, Sex) %>%
    summarise(
      observation_seconds = sum(PositionSeconds, na.rm = TRUE),
      Entropy = calc_entropy(PositionSeconds),
      DominantPosition = PositionID[which.max(PositionSeconds)],
      n_positions_visited = sum(PositionSeconds > 0, na.rm = TRUE),
      .groups = "drop"
    )

  mov <- movement_events %>%
    mutate(PhaseNumber = case_when(Phase == "Active" ~ ConsecActive, Phase == "Inactive" ~ ConsecInactive, TRUE ~ NA_integer_)) %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum, AnimalID) %>%
    summarise(Movement = sum(MovementEvent, na.rm = TRUE), MovementDistance = sum(MovementDistanceEvent, na.rm = TRUE), .groups = "drop")

  prox <- dyadic_intervals %>%
    filter_metric_intervals() %>%
    mutate(
      PhaseNumber = case_when(Phase == "Active" ~ ConsecActive, Phase == "Inactive" ~ ConsecInactive, TRUE ~ NA_integer_),
      same_position_seconds = if_else(same_position, DurationSec, 0),
      adjacent_seconds = if_else(adjacent_position, DurationSec, 0),
      weighted_grid_distance = grid_distance * DurationSec
    ) %>%
    select(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, DurationSec,
           same_position_seconds, adjacent_seconds, weighted_grid_distance, Focal, Partner) %>%
    bind_rows(
      dyadic_intervals %>%
        mutate(
          PhaseNumber = case_when(Phase == "Active" ~ ConsecActive, Phase == "Inactive" ~ ConsecInactive, TRUE ~ NA_integer_),
          same_position_seconds = if_else(same_position, DurationSec, 0),
          adjacent_seconds = if_else(adjacent_position, DurationSec, 0),
          weighted_grid_distance = grid_distance * DurationSec
        ) %>%
        transmute(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, DurationSec,
                  same_position_seconds, adjacent_seconds, weighted_grid_distance,
                  Focal = Partner, Partner = Focal)
    ) %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum = Focal) %>%
    summarise(
      dyadic_observation_seconds = sum(DurationSec, na.rm = TRUE),
      ProximitySeconds = sum(same_position_seconds, na.rm = TRUE),
      AdjacentProximitySeconds = sum(adjacent_seconds, na.rm = TRUE),
      ProximityFraction = safe_divide(ProximitySeconds, dyadic_observation_seconds),
      AdjacentProximityFraction = safe_divide(AdjacentProximitySeconds, dyadic_observation_seconds),
      Proximity = ProximityFraction,
      AdjacentProximity = AdjacentProximityFraction,
      MeanGridDistanceToOthers = safe_divide(sum(weighted_grid_distance, na.rm = TRUE), dyadic_observation_seconds),
      n_dyadic_intervals = n(),
      .groups = "drop"
    )

  out <- occ %>%
    left_join(mov, by = c("SourceFile", "Batch", "CageChange", "System", "Phase", "PhaseNumber", "AnimalNum", "AnimalID")) %>%
    left_join(prox, by = c("SourceFile", "Batch", "CageChange", "System", "Phase", "PhaseNumber", "AnimalNum")) %>%
    crop_after_second_cc4_phase() %>%
    group_by(SourceFile, Batch, CageChange, System, AnimalNum) %>%
    arrange(Phase, PhaseNumber, .by_group = TRUE) %>%
    mutate(TimeIndex = row_number() - 1L) %>%
    ungroup() %>%
    mutate(BinSizeSec = NA_real_, BinStart = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")) %>%
    standardize_metric_output("phase") %>%
    select(
      AnimalNum, AnimalID, Batch, CageChange, System, Group, Sex, Phase,
      PhaseNumber, BinLabel, BinSizeSec, BinStart, TimeIndex,
      Movement, MovementPerHour, MovementDistance, MovementDistancePerHour,
      Proximity, ProximitySeconds, ProximityFraction,
      ProximitySecondsPerHour,
      AdjacentProximity, AdjacentProximitySeconds, AdjacentProximityFraction,
      MeanGridDistanceToOthers,
      Entropy, DominantPosition, n_positions_visited,
      observation_seconds, dyadic_observation_seconds, n_dyadic_intervals,
      SourceFile
    ) %>%
    arrange(Batch, CageChange, System, AnimalNum, TimeIndex)

  out_dir <- file.path(output_root, "phase_based")
  ensure_dir(out_dir)
  write_table(out, file.path(out_dir, "all_behavior_metrics.csv"))
  write_epoch_duration_qc(out, out_dir, metric_source = "03_phase_behavior_metrics", bin_size_sec = NA_real_)
  out
}

# ------------------------------------------------
# WRITE OUTPUTS
# ------------------------------------------------

all_bin_outputs <- purrr::pmap(
  list(bin_specs$bin_label, bin_specs$bin_size_sec),
  build_bin_metrics
)
names(all_bin_outputs) <- bin_specs$bin_label
phase_output <- build_phase_metrics()

export_cookiehab_primary_window_metrics <- function(bin_outputs) {
  if (!identical(dataset_id, "cookiehab")) return(invisible(NULL))

  export_scales <- intersect(c("5min", "10min"), names(bin_outputs))
  if (length(export_scales) == 0) {
    warning("Neither 5min nor 10min outputs are available; cookiehab primary-window export skipped.", call. = FALSE)
    return(invisible(NULL))
  }

  out_dir <- file.path(output_root, "cookiehab_primary_I2_17_18")
  ensure_dir(out_dir)

  note <- tibble(
    note = c(
      "5min is recommended for CookieSubWindow analyses because the 17:45 planned transition aligns with 5-minute bins.",
      "10min is exported as a companion table for comparability with the main 10-minute metric scale.",
      "Coverage QC is derived from metric observation_seconds, not raw-row timestamp span."
    )
  )
  write_table(note, file.path(out_dir, "cookiehab_primary_I2_17_18_scale_note.csv"))

  exported <- purrr::map(export_scales, function(source_label) {
    dat <- bin_outputs[[source_label]]
    if (is.null(dat) || !"CookieWindowPrimary" %in% names(dat)) {
      warning("CookieWindowPrimary is not available in ", source_label, " output; skipping that scale.", call. = FALSE)
      return(NULL)
    }

    primary <- dat %>%
      filter(CookieWindowPrimary %in% TRUE)

    scale_dir <- file.path(out_dir, source_label)
    ensure_dir(scale_dir)

    write_table(
      primary,
      file.path(scale_dir, paste0("all_behavior_metrics_cookiehab_I2_17_18_", source_label, ".csv"))
    )

    per_animal_qc <- primary %>%
      group_by(SourceFile, Batch, System, AnimalID, Group, Sex, BinLabel) %>%
      summarise(
        n_bins = n_distinct(BinStart),
        coverage_minutes = sum(observation_seconds, na.rm = TRUE) / 60,
        min_bin_start = min(BinStart, na.rm = TRUE),
        max_bin_start = max(BinStart, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(SourceFile, Batch, System, AnimalID, BinLabel)

    summary_qc <- primary %>%
      summarise(
        BinLabel = source_label,
        qc_basis = "derived_metric_observation_seconds",
        n_rows = n(),
        n_animals = n_distinct(AnimalID),
        n_bins = n_distinct(BinStart),
        coverage_minutes = sum(observation_seconds, na.rm = TRUE) / 60,
        n_batches = n_distinct(Batch),
        n_systems = n_distinct(System),
        n_groups = n_distinct(Group),
        n_sexes = n_distinct(Sex)
      )

    qc_counts <- primary %>%
      count(BinLabel, Batch, System, Group, Sex, name = "n_rows") %>%
      arrange(BinLabel, Batch, System, Group, Sex)

    write_table(per_animal_qc, file.path(scale_dir, paste0("cookiehab_primary_I2_17_18_per_animal_coverage_qc_", source_label, ".csv")))
    write_table(summary_qc, file.path(scale_dir, paste0("cookiehab_primary_I2_17_18_summary_qc_", source_label, ".csv")))
    write_table(qc_counts, file.path(scale_dir, paste0("cookiehab_primary_I2_17_18_batch_system_group_sex_counts_", source_label, ".csv")))

    list(primary = primary, per_animal_qc = per_animal_qc, summary_qc = summary_qc, counts = qc_counts)
  })

  exported <- purrr::compact(exported)
  if (length(exported) > 0) {
    write_table(
      purrr::map_dfr(exported, "summary_qc"),
      file.path(out_dir, "cookiehab_primary_I2_17_18_summary_qc.csv")
    )
    write_table(
      purrr::map_dfr(exported, "per_animal_qc"),
      file.path(out_dir, "cookiehab_primary_I2_17_18_per_animal_coverage_qc.csv")
    )
    write_table(
      purrr::map_dfr(exported, "counts"),
      file.path(out_dir, "cookiehab_primary_I2_17_18_batch_system_group_sex_counts.csv")
    )
  }

  invisible(exported)
}

export_cookiehab_primary_window_metrics(all_bin_outputs)

qc_tbl <- bind_rows(
  map2_dfr(bin_specs$bin_label, all_bin_outputs, function(label, dat) {
    tibble(
      output = paste0(label, "_based"),
      n_rows = nrow(dat),
      n_animals = n_distinct(dat$AnimalNum),
      n_batches = n_distinct(dat$Batch),
      n_cage_changes = n_distinct(dat$CageChange),
      n_systems = n_distinct(dat$System),
      total_observation_hours = sum(dat$observation_seconds, na.rm = TRUE) / 3600,
      max_observation_seconds = max(dat$observation_seconds, na.rm = TRUE),
      n_rows_observation_seconds_gt_bin = sum(dat$observation_seconds > dat$BinSizeSec + 1e-6, na.rm = TRUE),
      max_dyadic_observation_seconds = max(dat$dyadic_observation_seconds, na.rm = TRUE),
      missing_proximity_fraction = mean(is.na(dat$ProximityFraction)),
      missing_proximity_seconds = mean(is.na(dat$ProximitySeconds))
    )
  }),
  tibble(
    output = "phase_based",
    n_rows = nrow(phase_output),
    n_animals = n_distinct(phase_output$AnimalNum),
    n_batches = n_distinct(phase_output$Batch),
    n_cage_changes = n_distinct(phase_output$CageChange),
    n_systems = n_distinct(phase_output$System),
    total_observation_hours = sum(phase_output$observation_seconds, na.rm = TRUE) / 3600,
    max_observation_seconds = max(phase_output$observation_seconds, na.rm = TRUE),
    n_rows_observation_seconds_gt_bin = NA_integer_,
    max_dyadic_observation_seconds = max(phase_output$dyadic_observation_seconds, na.rm = TRUE),
    missing_proximity_fraction = mean(is.na(phase_output$ProximityFraction)),
    missing_proximity_seconds = mean(is.na(phase_output$ProximitySeconds))
  )
)

write_table(qc_tbl, file.path(output_root, "qc", "multiscale_behavior_metrics_qc.csv"))

# ------------------------------------------------
# PROVENANCE / EPOCH-INCLUSION AUDITS
# ------------------------------------------------
# What the phase-boundary intersection did, and how much time fell in phase
# blocks that carry no observations (and is therefore outside the analysis
# window). Reported explicitly because it used to be removed silently by the
# > 1 h interval exclusion.
if (length(epoch_inclusion_audit) > 0) {
  write_table(bind_rows(epoch_inclusion_audit),
              file.path(output_root, "qc", "phase_boundary_epoch_inclusion_audit.csv"))
}

# Movement provenance. time_since_last_genuine_position_event_sec used to gate
# movement validity (events with a gap > 1 h were discarded). It is now recorded
# and never filtered, so this table shows exactly how many events the old rule
# would have deleted.
if (nrow(movement_events) > 0) {
  movement_gap_audit <- movement_events %>%
    mutate(gap_bucket = cut(
      time_since_last_genuine_position_event_sec,
      breaks = c(-Inf, 60, 300, 1800, 3600, 7200, Inf),
      labels = c("<=1min", "1-5min", "5-30min", "30-60min", "1-2h", ">2h"),
      right = TRUE)) %>%
    count(gap_bucket, name = "n_events") %>%
    mutate(fraction = n_events / sum(n_events))
  write_table(movement_gap_audit,
              file.path(output_root, "qc", "movement_event_gap_provenance.csv"))

  n_retired_rule_would_drop <- sum(
    movement_events$time_since_last_genuine_position_event_sec > system_event_gap_threshold_sec,
    na.rm = TRUE)
  movement_rule_audit <- tibble(
    n_movement_events_total = nrow(movement_events),
    n_events_retained_by_retiring_gap_rule = n_retired_rule_would_drop,
    fraction_retained_by_retiring_gap_rule = n_retired_rule_would_drop / nrow(movement_events),
    system_event_gap_threshold_sec = system_event_gap_threshold_sec,
    gap_rule_filters_movement = FALSE,
    exclude_long_gaps_from_metrics = exclude_long_gaps_from_metrics
  )
  write_table(movement_rule_audit,
              file.path(output_root, "qc", "movement_gap_rule_retirement_audit.csv"))
  message("
Movement events retained by retiring the > ",
          system_event_gap_threshold_sec, " s gap rule: ",
          n_retired_rule_would_drop, " of ", nrow(movement_events),
          sprintf(" (%.3f%%)", 100 * n_retired_rule_would_drop / nrow(movement_events)))
}

# Per animal x session count of genuine position events, kept as QC only.
if (nrow(all_pos) > 0) {
  position_event_counts <- all_pos %>%
    group_by(SourceFile, Batch, CageChange, System, AnimalNum, AnimalID) %>%
    summarise(n_position_events = n(),
              first_event = min(DateTime, na.rm = TRUE),
              last_event = max(DateTime, na.rm = TRUE),
              .groups = "drop")
  write_table(position_event_counts,
              file.path(output_root, "qc", "genuine_position_event_counts.csv"))
}

bad_bin_qc <- qc_tbl %>%
  filter(output != "phase_based", n_rows_observation_seconds_gt_bin > 0)

if (nrow(bad_bin_qc) > 0) {
  warning(
    "Some fixed-width outputs still have observation_seconds greater than BinSizeSec. ",
    "Check qc/multiscale_behavior_metrics_qc.csv",
    call. = FALSE
  )
}

if (exists("harmonize_analysis_outputs")) harmonize_analysis_outputs(output_root)

message("Multiscale behavior metric export complete.")
message("Primary downstream file pattern: ", file.path(output_root, "<scale>_based", "all_behavior_metrics.csv"))
