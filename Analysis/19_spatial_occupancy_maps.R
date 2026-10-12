# ================================================================
# Spatial RFID reader-occupancy maps for SIS home-cage behavior
# MMMSociability
# ================================================================
# Goal:
#   Build reader-position occupancy maps of the home cage. Since 2026-10-12 the occupancy comes from the raw
#   antenna logs (OCCUPANCY_SOURCE); the preprocessed AnimalPos files below supply the roster, groups and phase
#   calendar, and their own reconstruction is kept as a comparison. Not from all_behavior_metrics.csv.
#
# Why this script reads the prior dataset:
#   all_behavior_metrics.csv is already collapsed to Movement, Entropy,
#   Proximity, DominantPosition, and n_positions_visited. It no longer contains
#   the full PositionID-by-bin distribution needed for true occupancy maps.
#
# Intended raw input:
#   *_AnimalPos_preprocessed.csv files with at least:
#     DateTime, AnimalID, System, PositionID
#   Filename carries metadata, for example:
#     E9_SIS_B1_CC2_AnimalPos_preprocessed.csv
#     -> Batch = B1, CageChange = CC2
#
# Metadata logic mirrors 01_build_multiscale_behavior_metrics.R:
#   - Sex is inferred from Batch: B1/B2/B5 = Male; B3/B4/B6 = Female
#   - Group is inferred from sus_animals.csv and con_animals.csv
#   - Remaining listed animals are assigned RES if ASSIGN_UNLISTED_AS_RES = TRUE
#
# Important interpretation:
#   These are discrete RFID reader-occupancy maps, not continuous x/y tracking.
#   Vendor AnimalPos records are written only after a >= 200-unit move, so they never record a change of row within
#   a column and miss most moves to a neighbouring column; their 4 x 2 row split is the row an animal arrived on.
#   The raw antenna logs record every detection: occupancy there assigns each animal the antenna of its latest
#   detection until the next (about 80% of the time has no read), seeded before the window, without rounding, with
#   short border flips removed (RAW_FLIP_FILTER_SEC) and the cage taken from the antenna board.
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lmerTest)
  library(emmeans)
  library(lubridate)
})

# Shared measurement-event / aggregation-boundary helpers. This script is
# otherwise standalone, so the repository root is resolved explicitly.
.mmm_repo <- Sys.getenv("MMM_REPO_DIR", unset = "C:/Users/topohl/Documents/GitHub/MMMSociability")
.mmm_helper <- file.path(.mmm_repo, "Functions", "animalpos_preprocessing_helpers.R")
if (!file.exists(.mmm_helper)) {
  stop("Cannot locate animalpos_preprocessing_helpers.R at ", .mmm_helper,
       ". Set MMM_REPO_DIR.", call. = FALSE)
}
source(.mmm_helper)

# Canonical animal identity. Stage 19 must use the SAME contract as Stage 01,
# 03 and 09 rather than an independent normalization rule, otherwise numeric
# aliases such as 0004 and 4 survive as separate animals with different Group
# assignments.
.mmm_identity <- file.path(.mmm_repo, "Functions", "behavioral_dynamics_helpers.R")
if (!file.exists(.mmm_identity)) {
  stop("Cannot locate behavioral_dynamics_helpers.R at ", .mmm_identity,
       ". Set MMM_REPO_DIR.", call. = FALSE)
}
source(.mmm_identity)

.mmm_paths <- file.path(.mmm_repo, "Functions", "project_paths.R")
if (!file.exists(.mmm_paths)) {
  stop("Cannot locate project_paths.R at ", .mmm_paths, ". Set MMM_REPO_DIR.",
       call. = FALSE)
}
source(.mmm_paths)

# Reader grid, time-weighted animal/group summaries and the cage-map plots.
.mmm_cage_maps <- file.path(.mmm_repo, "Functions", "rfid_cage_map_helpers.R")
if (!file.exists(.mmm_cage_maps)) {
  stop("Cannot locate rfid_cage_map_helpers.R at ", .mmm_cage_maps, ". Set MMM_REPO_DIR.", call. = FALSE)
}
source(.mmm_cage_maps)

# Raw antenna logs: reading, flip filter and carry-forward intervals.
.mmm_raw_antenna <- file.path(.mmm_repo, "Functions", "rfid_raw_antenna_helpers.R")
if (!file.exists(.mmm_raw_antenna)) {
  stop("Cannot locate rfid_raw_antenna_helpers.R at ", .mmm_raw_antenna, ". Set MMM_REPO_DIR.", call. = FALSE)
}
source(.mmm_raw_antenna)

# -----------------------------
# User options
# -----------------------------

RAW_POSITION_DIR <- file.path(mmm_project_root(), "MMMSociability", "preprocessed_data")

# Source of the occupancy itself. "raw_antenna": every detection event of the raw MultiMouseMonitor logs (one
# recording per batch and cage change, read-only). "animalpos": the vendor position records of RAW_POSITION_DIR,
# as before 2026-10-12. The AnimalPos files always supply the roster, the groups and the phase calendar, and the
# source not chosen is still computed for the comparison table.
OCCUPANCY_SOURCE <- "raw_antenna"
# The vendor AnimalPos exports of the same recordings map RFID tags to animals (the raw logs of Batch 1 carry no
# animal labels); Stage 01 reads the same folder for its seeds.
RAW_TAG_MAP_DIR <- file.path(mmm_project_root(), "MMMSociability", "raw_data")
RAW_ANTENNA_DIR <- getOption("mmm.raw_antenna_dir",
                             "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/MMMdata")
# Border flicker: a tag between two antennas is read by both in turn (17% of all raw reader changes return to the
# previous antenna within 2 s). Such runs shorter than this are reassigned to the antenna the tag returned to. The
# map barely depends on it (CC1 first active phase: group means move <= 0.03 pp at 2 s, <= 0.5 pp at 30 s); the
# sensitivity is written to the audit for these thresholds.
RAW_FLIP_FILTER_SEC <- 2
RAW_FLIP_SENSITIVITY_SEC <- c(0, 2, 5, 10, 30)

SUS_ANIMALS_FILE <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv"
CON_ANIMALS_FILE <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv"
ASSIGN_UNLISTED_AS_RES <- TRUE

BIN_SIZE_SEC <- 1800
BIN_LABEL <- "30min"
# Diagnostic-only gap threshold.
#
# This was a metric-validity rule: occupancy intervals longer than an hour were
# dropped. It was inert while preprocessing injected a synthetic position row at
# every half-hour mark (no interval could exceed 1800 s). With genuine-only
# input, 1,538 intervals covering 3,528 h (34.1% of observed duration) exceed
# it, concentrated during rest. AnimalPos has no read-duration field, so wide
# event spacing is not evidence of dropout. Kept as QC only; never filters.
SYSTEM_EVENT_GAP_THRESHOLD_SEC <- 3600
EXCLUDE_LONG_GAPS <- FALSE

MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW <- 60
EPS_LOGIT <- 1e-3
PRIMARY_CAGE_CHANGE <- 1
PRIMARY_PHASE <- "Active"
PRIMARY_WINDOW_H <- 12
PRIMARY_PHASE_NUMBER <- 1L

GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS <- c("Female", "Male")

# 4 x 2 grid: R1-R4 along the long axis at y = 0, R5-R8 at y = 116 (Functions/rfid_cage_map_helpers.R).
POSITION_MAP <- RFID_READER_MAP

# Spatial resolution of the cage maps and models: "reader" (the 4 x 2 grid) or "column" (four long-axis columns,
# each pooling its two readers). The vendor position stream records a position only after a >= 200-unit move, so it
# never records a change of row within a column (0 of 592,161 changes; rows are 116 units apart) and its row split is
# the row an animal arrived on: female RES - CON +11.3 / -9.6 percentage points at R1 / R5 from AnimalPos, about
# +1 / +1 from the raw logs (check of 2026-10-12). Reader resolution therefore requires the raw antenna source.
CAGE_MAP_RESOLUTION <- "reader"
if (!OCCUPANCY_SOURCE %in% c("raw_antenna", "animalpos")) stop("Unknown OCCUPANCY_SOURCE.", call. = FALSE)
if (OCCUPANCY_SOURCE == "animalpos" && CAGE_MAP_RESOLUTION == "reader") {
  stop("AnimalPos records cannot resolve the two rows of a column; use CAGE_MAP_RESOLUTION = \"column\".", call. = FALSE)
}

# Models: per sex, one linear mixed model per reader (or column) on the centred log-ratio of the animal's shares of
# time, with a cage random effect; this pseudo-count (seconds) is added to every reader before taking logs.
MODEL_PSEUDO_SECONDS <- 1

# -----------------------------
# Output folders
# -----------------------------

# These were relative paths until 2026-09-21, while RAW_POSITION_DIR above is
# absolute. That inconsistency meant the stage read from the lab drive but wrote
# wherever the working directory happened to be: run from the repository root it
# silently deposited 186 MB into the checkout and left the real outputs on the
# lab drive untouched since 2026-08-28, while still exiting 0. Anchor the
# outputs to the same RFID root the input is pinned to.
RFID_ROOT <- mmm_project_root()

spatial_groups <- c("spatial_tables", "spatial_audit", "spatial_models", "spatial_figures")
mmm_behavior_output_assert_uniform_layout(spatial_groups, project_root = RFID_ROOT,
                                          producer = "Stage 19 spatial")
# An unactivated group would resolve to its retained original; the guard
# refuses that path once the numbered root is under archive control.
DIR_DERIVED <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("spatial_tables", project_root = RFID_ROOT), RFID_ROOT)
DIR_AUDIT   <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("spatial_audit", project_root = RFID_ROOT), RFID_ROOT)
DIR_MODELS  <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("spatial_models", project_root = RFID_ROOT), RFID_ROOT)
DIR_FIGS    <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("spatial_figures", project_root = RFID_ROOT), RFID_ROOT)
DIR_PUBTAB  <- file.path(RFID_ROOT, "publication_ready/tables/spatial_occupancy")
DIR_PUBFIG  <- file.path(RFID_ROOT, "publication_ready/figures/single_panels/spatial_occupancy")

purrr::walk(
  c(DIR_DERIVED, DIR_AUDIT, DIR_MODELS, DIR_FIGS, DIR_PUBTAB, DIR_PUBFIG),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -----------------------------
# Helpers
# -----------------------------

message2 <- function(...) message(sprintf(...))

safe_divide <- function(num, den) ifelse(is.finite(den) & den > 0, num / den, NA_real_)
safe_logit <- function(x, eps = EPS_LOGIT) qlogis(pmin(pmax(x, eps), 1 - eps))

# Retained only so that any remaining caller routes through the shared
# contract. canonical_animal_id() additionally strips leading zeros from
# purely numeric IDs, which is what collapses 0003/3 and 0004/4 to one
# animal each. Reference ID lists are normalized the same way, so a list
# entry written as 0004 still matches a raw row written as 4.
normalize_animal_id <- function(x) canonical_animal_id(x)

read_animal_id_list <- function(path, label) {
  if (is.null(path) || !file.exists(path)) {
    warning("Animal reference file not found for ", label, ": ", path, call. = FALSE)
    return(character())
  }

  readr::read_lines(path, progress = FALSE) %>%
    normalize_animal_id() %>%
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

infer_phase_from_time <- function(dt) {
  hhmm <- format(dt, "%H:%M", tz = "UTC")
  if_else(hhmm >= "18:30" | hhmm < "06:30", "Active", "Inactive")
}

extract_batch_from_filename <- function(path) stringr::str_extract(basename(path), "B[0-9]+")
extract_cc_from_filename <- function(path) stringr::str_extract(basename(path), "CC[0-9]+")

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
      AnimalID_source = as.character(AnimalID),
      AnimalNum = canonical_animal_id(AnimalID),
      AnimalID = AnimalNum,
      System = as.character(System),
      PositionID = suppressWarnings(as.integer(PositionID)),
      SourceFile = basename(path),
      Batch = if ("Batch" %in% names(.)) as.character(Batch) else extract_batch_from_filename(path),
      CageChange = if ("CageChange" %in% names(.)) as.character(CageChange) else extract_cc_from_filename(path),
      Phase = if ("Phase" %in% names(.)) as.character(Phase) else infer_phase_from_time(DateTime),
      Group = if ("Group" %in% names(.)) as.character(Group) else NA_character_,
      Sex = if ("Sex" %in% names(.)) as.character(Sex) else NA_character_,
      ConsecActive = if ("ConsecActive" %in% names(.)) suppressWarnings(as.integer(ConsecActive)) else NA_integer_,
      ConsecInactive = if ("ConsecInactive" %in% names(.)) suppressWarnings(as.integer(ConsecInactive)) else NA_integer_
    ) %>%
    filter(!is.na(DateTime), !is.na(AnimalID), !is.na(System), is.finite(PositionID), PositionID %in% POSITION_MAP$PositionID) %>%
    arrange(SourceFile, Batch, CageChange, System, DateTime, AnimalID)
}

# Carry-forward occupancy of one system (cage) from vendor position records. Between consecutive event times of the
# system, every animal that has a record so far holds the position of its latest record at or before the interval
# start; the interval's metadata come from the system's latest record at or before that start. Vectorised with
# findInterval(): the former loop (one dplyr filter per event; about an hour for all files) gave the same rows in
# the same order (checked on 2026-10-12 against the loop).
make_occupancy_intervals_one_system <- function(dat_sys) {
  dat_sys <- dat_sys %>% arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))
  event_times <- sort(unique(dat_sys$DateTime))
  if (length(animals) == 0 || length(event_times) < 2) return(tibble())

  t0 <- event_times[-length(event_times)]
  t1 <- event_times[-1]
  t_all <- as.numeric(dat_sys$DateTime)
  meta <- dat_sys[findInterval(as.numeric(t0), t_all),
                  c("SourceFile", "Batch", "CageChange", "System", "Phase", "ConsecActive", "ConsecInactive")]

  # The latest non-missing label so far. as.character() is required: assigning a factor element into a character
  # vector stores its integer code ("RES" -> "2"), which later factor() calls map to NA.
  carry_label <- function(x) {
    x <- as.character(x)
    last <- cummax(seq_along(x) * !is.na(x))
    out <- rep(NA_character_, length(x))
    out[last > 0] <- x[last[last > 0]]
    out
  }
  held <- bind_rows(lapply(seq_along(animals), function(k) {
    rows <- which(dat_sys$AnimalID == animals[k])
    latest <- findInterval(as.numeric(t0), t_all[rows])   # the animal's latest record at or before each start
    i <- which(latest > 0)
    tibble(i = i, animal_rank = k, PositionID = as.integer(dat_sys$PositionID[rows][latest[i]]),
           Group = carry_label(dat_sys$Group[rows])[latest[i]], Sex = carry_label(dat_sys$Sex[rows])[latest[i]])
  })) %>%
    filter(is.finite(PositionID), PositionID > 0) %>%
    arrange(i, animal_rank)
  if (nrow(held) == 0) return(tibble())

  duration_sec <- as.numeric(difftime(t1[held$i], t0[held$i], units = "secs"))
  tibble(
    AnimalNum = animals[held$animal_rank],
    AnimalID = animals[held$animal_rank],
    PositionID = held$PositionID,
    Group = held$Group,
    Sex = held$Sex,
    IntervalStart = t0[held$i],
    IntervalEnd = t1[held$i],
    DurationSec = duration_sec,
    SystemEventGap = duration_sec > SYSTEM_EVENT_GAP_THRESHOLD_SEC
  ) %>%
    bind_cols(meta[held$i, ]) %>%
    filter(is.finite(DurationSec), DurationSec > 0)
}

# Retained as an explicit, auditable guard so that reinstating a duration-based
# exclusion has to be a deliberate edit rather than a silent flag flip.
assert_no_gap_exclusion <- function() {
  if (isTRUE(EXCLUDE_LONG_GAPS)) {
    stop("EXCLUDE_LONG_GAPS = TRUE would silently drop genuine reconstructed ",
         "occupancy (34.1% of observed duration in the current dataset). ",
         "Excluding intervals by event spacing is a scientific decision that ",
         "must be made explicitly, not via this flag.", call. = FALSE)
  }
  invisible(TRUE)
}

split_intervals_to_bins <- function(dat, bin_size_sec) {
  assert_no_gap_exclusion()

  dat <- dat %>%
    filter(!is.na(IntervalStart), !is.na(IntervalEnd), is.finite(DurationSec), DurationSec > 0)

  if (nrow(dat) == 0) {
    return(dat %>%
      mutate(
        BinSizeSec = bin_size_sec,
        BinStart = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
      ) %>%
      slice(0))
  }

  # Same generic interval/boundary intersection used by Stage 01 and Stage 02;
  # cross-validated in the test suite against a loop reference and against the
  # pre-refactor binning arithmetic.
  out <- animalpos_split_intervals_one_grid(dat, bin_size_sec, 0)
  out$BinSizeSec <- bin_size_sec
  out$BinStart <- as.POSIXct(
    floor(as.numeric(out$IntervalStart) / bin_size_sec) * bin_size_sec,
    origin = "1970-01-01", tz = "UTC"
  )
  out
}

add_time_index <- function(dat) {
  dat %>%
    group_by(SourceFile, Batch, CageChange, System) %>%
    mutate(TimeIndex = as.numeric(difftime(BinStart, min(BinStart, na.rm = TRUE), units = "secs")) / first(BinSizeSec)) %>%
    ungroup()
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

complete_reader_positions <- function(dat, group_cols) {
  dat %>%
    group_by(across(all_of(group_cols))) %>%
    tidyr::complete(
      PositionID = POSITION_MAP$PositionID,
      fill = list(PositionSeconds = 0)
    ) %>%
    mutate(
      observation_seconds = sum(PositionSeconds, na.rm = TRUE),
      occupancy_fraction = safe_divide(PositionSeconds, observation_seconds)
    ) %>%
    ungroup()
}

save_svg <- function(plot, filename, width = 7, height = 5) {
  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height, units = "in", device = "svg")
  message2("Saved: %s", filename)
}

write_csv2 <- function(x, path) {
  readr::write_csv(x, path, na = "")
  message2("Saved: %s", path)
}

# -----------------------------
# Load raw prior position data
# -----------------------------

if (!dir.exists(RAW_POSITION_DIR)) {
  stop(
    "RAW_POSITION_DIR does not exist: ", RAW_POSITION_DIR,
    "\nSet RAW_POSITION_DIR near the top of this script to the folder containing *_AnimalPos_preprocessed.csv files.",
    call. = FALSE
  )
}

position_files <- list.files(
  RAW_POSITION_DIR,
  pattern = "AnimalPos.*preprocessed\\.csv$|_preprocessed\\.csv$",
  full.names = TRUE
)

if (length(position_files) == 0) {
  stop("No preprocessed AnimalPos CSV files found in RAW_POSITION_DIR: ", RAW_POSITION_DIR, call. = FALSE)
}

message("Found ", length(position_files), " preprocessed position files.")
message("Example filename metadata parsing: E9_SIS_B1_CC2_AnimalPos_preprocessed.csv -> Batch B1, CageChange CC2")

all_pos <- purrr::map_dfr(position_files, read_preprocessed_position_file, .progress = TRUE)

sus_animals <- read_animal_id_list(SUS_ANIMALS_FILE, "SUS")
con_animals <- read_animal_id_list(CON_ANIMALS_FILE, "CON")

overlap <- intersect(sus_animals, con_animals)
if (length(overlap) > 0) {
  stop("Animal IDs found in both SUS and CON reference files: ", paste(overlap, collapse = ", "), call. = FALSE)
}

all_pos <- all_pos %>%
  mutate(
    AnimalID_raw = AnimalID_source,
    AnimalID_norm = canonical_animal_id(AnimalID),
    Batch_norm = toupper(str_trim(Batch)),
    ReferenceGroup = case_when(
      AnimalID_norm %in% sus_animals ~ "SUS",
      AnimalID_norm %in% con_animals ~ "CON",
      ASSIGN_UNLISTED_AS_RES ~ "RES",
      TRUE ~ NA_character_
    ),
    ReferenceSex = assign_batch_sex(Batch),
    Group = coalesce(ReferenceGroup, Group),
    Sex = coalesce(ReferenceSex, Sex),
    Group = factor(as.character(Group), levels = GROUP_LEVELS),
    Sex = factor(as.character(Sex), levels = SEX_LEVELS),
    PhaseClass = factor(Phase, levels = c("Active", "Inactive")),
    CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChange, "\\d+")))
  )

metadata_qc <- all_pos %>%
  distinct(AnimalID_raw, AnimalID_norm, Batch, CageChange, System, Sex, Group, ReferenceGroup, ReferenceSex, SourceFile) %>%
  arrange(Batch, CageChange, System, AnimalID_norm)
write_csv2(metadata_qc, file.path(DIR_AUDIT, "raw_position_metadata_assignment_qc.csv"))

# ------------------------------------------------
# CANONICAL ANIMAL IDENTITY CONTRACT
# ------------------------------------------------
# Aliases such as 0004 and 4 refer to one animal. Before canonicalization
# Stage 19 carried them as separate identities, and because the SUS reference
# list stores 0004 the bare spelling fell through to the unlisted default and
# was labelled RES. The contract below fails closed rather than silently
# averaging or splitting an animal.

identity_conflicts <- all_pos %>%
  distinct(AnimalNum, Group, Sex, Batch_norm) %>%
  group_by(AnimalNum) %>%
  summarise(
    n_groups = dplyr::n_distinct(as.character(Group)),
    n_sexes = dplyr::n_distinct(as.character(Sex)),
    n_batches = dplyr::n_distinct(Batch_norm),
    groups_seen = paste(sort(unique(as.character(Group))), collapse = '|'),
    sexes_seen = paste(sort(unique(as.character(Sex))), collapse = '|'),
    batches_seen = paste(sort(unique(Batch_norm)), collapse = '|'),
    .groups = 'drop'
  ) %>%
  filter(n_groups > 1 | n_sexes > 1 | n_batches > 1)
write_csv2(identity_conflicts, file.path(DIR_AUDIT, 'canonical_identity_conflicts.csv'))
if (nrow(identity_conflicts) > 0) {
  stop('Canonical animal identity conflict for ', nrow(identity_conflicts),
       ' animal(s): ', paste(identity_conflicts$AnimalNum, collapse = ', '),
       '. Aliases that map to one canonical animal must agree on Group, Sex and Batch.',
       call. = FALSE)
}

alias_merge_qc <- all_pos %>%
  distinct(AnimalNum, AnimalID_source) %>%
  group_by(AnimalNum) %>%
  summarise(n_raw_spellings = dplyr::n(),
            raw_spellings = paste(sort(unique(AnimalID_source)), collapse = '|'),
            .groups = 'drop')
write_csv2(alias_merge_qc, file.path(DIR_AUDIT, 'canonical_identity_alias_merge.csv'))

identity_roster <- all_pos %>% distinct(AnimalNum, Group, Sex)
identity_summary <- tibble::tibble(
  n_canonical_animals = dplyr::n_distinct(all_pos$AnimalNum),
  n_raw_spellings = dplyr::n_distinct(all_pos$AnimalID_source),
  n_aliases_merged = dplyr::n_distinct(all_pos$AnimalID_source) - dplyr::n_distinct(all_pos$AnimalNum),
  n_animals_with_multiple_spellings = sum(alias_merge_qc$n_raw_spellings > 1),
  n_CON = sum(identity_roster$Group == 'CON', na.rm = TRUE),
  n_RES = sum(identity_roster$Group == 'RES', na.rm = TRUE),
  n_SUS = sum(identity_roster$Group == 'SUS', na.rm = TRUE),
  n_Female = sum(identity_roster$Sex == 'Female', na.rm = TRUE),
  n_Male = sum(identity_roster$Sex == 'Male', na.rm = TRUE),
  identity_conflicts = nrow(identity_conflicts)
)
write_csv2(identity_summary, file.path(DIR_AUDIT, 'canonical_identity_summary.csv'))
message('Stage 19 canonical identity: ', identity_summary$n_canonical_animals,
        ' animals from ', identity_summary$n_raw_spellings, ' raw spellings (',
        identity_summary$n_aliases_merged, ' merged); ',
        identity_summary$n_CON, ' CON / ', identity_summary$n_RES, ' RES / ',
        identity_summary$n_SUS, ' SUS; ', identity_summary$n_Female, ' Female / ',
        identity_summary$n_Male, ' Male.')

if (nrow(identity_roster) != identity_summary$n_canonical_animals) {
  stop('Canonical roster is not one row per animal after identity resolution.', call. = FALSE)
}

if (any(is.na(all_pos$Group))) warning("Some rows have missing Group after reference assignment.", call. = FALSE)
if (any(is.na(all_pos$Sex))) warning("Some rows have missing Sex after batch-based assignment.", call. = FALSE)

# -----------------------------
# Build occupancy intervals and binned position occupancy
# -----------------------------

message("Building occupancy intervals from AnimalPos position records...")
occupancy_intervals_animalpos <- all_pos %>%
  group_by(SourceFile, Batch, CageChange, System) %>%
  group_split() %>%
  purrr::map_dfr(make_occupancy_intervals_one_system, .progress = TRUE)

if (nrow(occupancy_intervals_animalpos) == 0) {
  stop("No occupancy intervals could be reconstructed. Check DateTime, AnimalID, System, and PositionID columns.", call. = FALSE)
}

# ------------------------------------------------
# AGGREGATION BOUNDARIES
# ------------------------------------------------
# Phase and PhaseNumber are grouping variables, so an interval spanning 06:30 or
# 18:30 must be split there and each piece labelled with the phase it actually
# falls in. Previously an interval inherited Phase from the row preceding its
# start, which was only safe because preprocessing injected a synthetic position
# row at every boundary. No synthetic row is created here: the boundary is an
# interval intersection, and the labels come from the phase blocks that carry
# genuine observations in the same session.

phase_block_reference <- all_pos %>%
  mutate(PhaseBlockIndex = animalpos_phase_block_index(DateTime)) %>%
  distinct(SourceFile, PhaseBlockIndex, Phase, ConsecActive, ConsecInactive)

.dup_blocks <- phase_block_reference %>% count(SourceFile, PhaseBlockIndex) %>% filter(n > 1)
if (nrow(.dup_blocks) > 0) {
  stop("Phase labels are not unique within a session for ", nrow(.dup_blocks),
       " (SourceFile, PhaseBlockIndex) combinations.", call. = FALSE)
}

# Splits intervals at the phase boundaries, labels every piece with its phase block and drops pieces in blocks
# without observations. Returns list(intervals, audit).
bound_to_phase_blocks <- function(intervals, source_label) {
  sec_before <- sum(intervals$DurationSec, na.rm = TRUE)
  n_before <- nrow(intervals)
  intervals <- animalpos_split_intervals(intervals, period_sec = NULL, split_phase = TRUE)
  if (abs(sum(intervals$DurationSec, na.rm = TRUE) - sec_before) > 1e-3) {
    stop("Phase splitting did not conserve occupancy interval duration (", source_label, ").", call. = FALSE)
  }
  intervals <- intervals %>%
    mutate(PhaseBlockIndex = animalpos_phase_block_index(IntervalStart)) %>%
    select(-any_of(c("Phase", "ConsecActive", "ConsecInactive"))) %>%
    left_join(phase_block_reference, by = c("SourceFile", "PhaseBlockIndex"))
  n_unobs <- sum(is.na(intervals$Phase))
  sec_unobs <- sum(intervals$DurationSec[is.na(intervals$Phase)], na.rm = TRUE)
  audit <- tibble(
    source                             = source_label,
    n_intervals_before_split           = n_before,
    n_pieces_after_split               = nrow(intervals),
    n_pieces_created_by_phase_split    = nrow(intervals) - n_before,
    seconds_before_split               = sec_before,
    seconds_after_split                = sum(intervals$DurationSec, na.rm = TRUE),
    n_pieces_in_unobserved_phase_block = n_unobs,
    seconds_in_unobserved_phase_blocks = sec_unobs,
    system_event_gap_threshold_sec     = SYSTEM_EVENT_GAP_THRESHOLD_SEC,
    exclude_long_gaps                  = EXCLUDE_LONG_GAPS,
    n_intervals_over_gap_threshold     = sum(intervals$SystemEventGap, na.rm = TRUE),
    seconds_over_gap_threshold         = sum(intervals$DurationSec[intervals$SystemEventGap %in% TRUE], na.rm = TRUE)
  )
  message(sprintf(
    "  %s occupancy intervals: %d -> %d phase-bounded pieces; dropped %d pieces (%.1f h) in phase blocks with no observations",
    source_label, n_before, nrow(intervals), n_unobs, sec_unobs / 3600))
  intervals <- intervals %>% filter(!is.na(Phase)) %>% select(-PhaseBlockIndex)
  # A piece lies inside one block if its start and a point just before its end do; for raw detections, which are
  # timed below the millisecond, that point is taken at most half a piece before the end.
  probe_end <- as.numeric(intervals$IntervalEnd) - pmin(1e-3, intervals$DurationSec / 2)
  straddling <- animalpos_phase_block_index(intervals$IntervalStart) !=
    animalpos_phase_block_index(as.POSIXct(probe_end, origin = "1970-01-01", tz = "UTC"))
  straddle <- sum(straddling)
  if (straddle > 0) {
    print(intervals[which(straddling)[seq_len(min(5, straddle))], c("SourceFile", "AnimalNum", "IntervalStart",
                                                                      "IntervalEnd", "DurationSec")])
    stop(straddle, " occupancy interval piece(s) still span a phase boundary after splitting (", source_label, ").",
         call. = FALSE)
  }
  list(intervals = intervals, audit = audit)
}

# ------------------------------------------------
# RAW ANTENNA OCCUPANCY
# ------------------------------------------------
# One raw recording per batch and cage change (Functions/rfid_raw_antenna_helpers.R). The analysis span of a
# recording is its observed phase blocks, from the start of the first to the end of the last, clipped to the end of
# the raw recording. Every roster animal is seeded by its last detection before the span, and its cage is the
# antenna board that holds most of its detections (the vendor System label is kept in the audit).
recording_spans <- phase_block_reference %>%
  group_by(SourceFile) %>%
  summarise(first_block = min(PhaseBlockIndex), last_block = max(PhaseBlockIndex), .groups = "drop") %>%
  mutate(span_start = first_block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
         span_end = (last_block + 1) * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC) %>%
  left_join(all_pos %>% distinct(SourceFile, Batch, CageChange), by = "SourceFile") %>%
  arrange(Batch, CageChange)
if (anyDuplicated(recording_spans$SourceFile)) stop("A preprocessed file maps to more than one batch/cage change.", call. = FALSE)

# Flip-filter sensitivity is computed for the primary window: the first observed active block of the primary cage
# change.
primary_block <- phase_block_reference %>%
  left_join(recording_spans %>% select(SourceFile, CageChange), by = "SourceFile") %>%
  filter(suppressWarnings(as.integer(str_extract(CageChange, "\\d+"))) == PRIMARY_CAGE_CHANGE,
         Phase == PRIMARY_PHASE, ConsecActive == PRIMARY_PHASE_NUMBER) %>%
  transmute(SourceFile, window_start = PhaseBlockIndex * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
            window_end = window_start + ANIMALPOS_PHASE_LENGTH_SEC)

message("Building occupancy intervals from the raw antenna logs...")
raw_interval_list <- vector("list", nrow(recording_spans))
raw_audit_list <- vector("list", nrow(recording_spans))
flip_sensitivity_list <- list()
for (i in seq_len(nrow(recording_spans))) {
  rs <- recording_spans[i, ]
  raw_log <- rfid_read_recording(RAW_ANTENNA_DIR, rs$Batch, rs$CageChange)
  raw_path <- raw_log$path
  ignored_logs <- paste(raw_log$ignored, collapse = "|")   # empty sessions of the same cage change
  fold_dropped <- raw_log$clock_fold_rows_dropped             # rows of a repeated (daylight-saving) hour
  tag_map <- rfid_animalpos_tag_map(file.path(RAW_TAG_MAP_DIR, rs$Batch,
                                               sprintf("E9_SIS_%s_%s_AnimalPos.csv", rs$Batch, rs$CageChange)))
  recording_end <- max(raw_log$events$t + raw_log$events$dur_sec)
  span_end <- min(rs$span_end, recording_end)
  roster_i <- all_pos %>%
    filter(SourceFile == rs$SourceFile) %>%
    distinct(AnimalNum, AnimalID, Group, Sex, System_label = System)
  if (anyDuplicated(roster_i$AnimalNum)) {
    stop("An animal carries more than one System label or group in ", rs$SourceFile, call. = FALSE)
  }
  built <- rfid_raw_occupancy_intervals(raw_log$events, roster_i, tag_map, rs$span_start, span_end,
                                        RAW_FLIP_FILTER_SEC)
  if (any(is.na(built$audit$Board))) {
    stop("No raw detections in the analysis span for animal(s) ",
         paste(built$audit$AnimalNum[is.na(built$audit$Board)], collapse = ", "), " in ", rs$SourceFile, call. = FALSE)
  }
  raw_interval_list[[i]] <- built$intervals %>%
    left_join(roster_i %>% select(AnimalNum, AnimalID, Group, Sex), by = "AnimalNum") %>%
    transmute(
      SourceFile = rs$SourceFile, Batch = rs$Batch, CageChange = rs$CageChange, System = paste0("sys.", Board),
      AnimalNum, AnimalID, PositionID, Group, Sex,
      IntervalStart = as.POSIXct(start, origin = "1970-01-01", tz = "UTC"),
      IntervalEnd = as.POSIXct(end, origin = "1970-01-01", tz = "UTC"),
      DurationSec = end - start,
      SystemEventGap = DurationSec > SYSTEM_EVENT_GAP_THRESHOLD_SEC
    )
  raw_audit_list[[i]] <- built$audit %>%
    mutate(SourceFile = rs$SourceFile, Batch = rs$Batch, CageChange = rs$CageChange, raw_log = basename(raw_path),
           raw_logs_ignored = ignored_logs, clock_fold_events_dropped = fold_dropped,
           span_start = as.POSIXct(rs$span_start, origin = "1970-01-01", tz = "UTC"),
           span_end = as.POSIXct(span_end, origin = "1970-01-01", tz = "UTC"),
           recording_end = as.POSIXct(recording_end, origin = "1970-01-01", tz = "UTC"),
           read_coverage = detected_seconds / span_seconds, .before = 1)
  pw <- primary_block %>% filter(SourceFile == rs$SourceFile)
  if (nrow(pw) == 1) {
    for (flip_sec in RAW_FLIP_SENSITIVITY_SEC) {
      b <- rfid_raw_occupancy_intervals(raw_log$events, roster_i, tag_map, pw$window_start, pw$window_end, flip_sec)
      flip_sensitivity_list[[paste(i, flip_sec)]] <- b$intervals %>%
        group_by(AnimalNum) %>%
        group_modify(~ tibble(PositionID = 1:8,
                              occupancy_fraction = rfid_interval_shares(.x, pw$window_start, pw$window_end))) %>%
        ungroup() %>%
        mutate(flip_filter_sec = flip_sec)
    }
  }
  message(sprintf("  %s %s: %s, %d animals, read coverage %.1f%%", rs$Batch, rs$CageChange, basename(raw_path),
                  nrow(roster_i), 100 * mean(raw_audit_list[[i]]$read_coverage)))
  rm(raw_log)
}
occupancy_intervals_raw <- bind_rows(raw_interval_list)
raw_antenna_audit <- bind_rows(raw_audit_list)
write_csv2(raw_antenna_audit, file.path(DIR_AUDIT, "raw_antenna_occupancy_audit.csv"))
rm(raw_interval_list)

flip_filter_sensitivity <- bind_rows(flip_sensitivity_list) %>%
  left_join(all_pos %>% distinct(AnimalNum, Group, Sex) %>%
              mutate(Group = as.character(Group), Sex = as.character(Sex)), by = "AnimalNum") %>%
  group_by(Sex, Group, flip_filter_sec, PositionID) %>%
  summarise(n_animals = n_distinct(AnimalNum), mean_occupancy = mean(occupancy_fraction, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(Sex, Group, PositionID) %>%
  mutate(change_vs_no_filter = mean_occupancy - mean_occupancy[flip_filter_sec == 0]) %>%
  ungroup() %>%
  left_join(POSITION_MAP %>% select(PositionID, ReaderLabel), by = "PositionID")
write_csv2(flip_filter_sensitivity, file.path(DIR_AUDIT, "raw_flip_filter_sensitivity_primary_window.csv"))

bounded <- list(animalpos = bound_to_phase_blocks(occupancy_intervals_animalpos, "animalpos"),
                raw_antenna = bound_to_phase_blocks(occupancy_intervals_raw, "raw_antenna"))
rm(occupancy_intervals_animalpos, occupancy_intervals_raw)
phase_boundary_audit <- bind_rows(bounded$animalpos$audit, bounded$raw_antenna$audit)
write_csv2(phase_boundary_audit,
           file.path(DIR_AUDIT, "phase_boundary_and_gap_rule_audit.csv"))

COMPARISON_SOURCE <- setdiff(c("raw_antenna", "animalpos"), OCCUPANCY_SOURCE)
occupancy_intervals <- bounded[[OCCUPANCY_SOURCE]]$intervals
comparison_intervals <- bounded[[COMPARISON_SOURCE]]$intervals
rm(bounded)

position_occupancy_by_bin <- occupancy_intervals %>%
  split_intervals_to_bins(BIN_SIZE_SEC) %>%
  add_phase_number() %>%
  group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum, AnimalID, Group, Sex, PositionID) %>%
  summarise(PositionSeconds = sum(DurationSec, na.rm = TRUE), .groups = "drop") %>%
  complete_reader_positions(c("SourceFile", "Batch", "CageChange", "System", "Phase", "PhaseNumber", "BinSizeSec", "BinStart", "AnimalNum", "AnimalID", "Group", "Sex")) %>%
  add_time_index() %>%
  crop_after_second_cc4_phase() %>%
  mutate(
    BinLabel = BIN_LABEL,
    PhaseClass = factor(Phase, levels = c("Active", "Inactive")),
    CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChange, "\\d+")))
  ) %>%
  left_join(POSITION_MAP, by = "PositionID") %>%
  arrange(Batch, CageChange, System, AnimalNum, BinStart, PositionID)

write_csv2(position_occupancy_by_bin, file.path(DIR_DERIVED, "all_position_occupancy_by_bin.csv"))
write_csv2(position_occupancy_by_bin, file.path(DIR_PUBTAB, "all_position_occupancy_by_bin.csv"))

# Phase-level occupancy for full active/inactive summaries.
occupancy_by_phase <- function(intervals) {
  intervals %>%
    { assert_no_gap_exclusion(); . } %>%
    add_phase_number() %>%
    group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum, AnimalID, Group, Sex, PositionID) %>%
    summarise(PositionSeconds = sum(DurationSec, na.rm = TRUE), .groups = "drop") %>%
    complete_reader_positions(c("SourceFile", "Batch", "CageChange", "System", "Phase", "PhaseNumber", "AnimalNum", "AnimalID", "Group", "Sex")) %>%
    crop_after_second_cc4_phase() %>%
    mutate(
      BinLabel = "phase",
      BinSizeSec = NA_real_,
      BinStart = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
      TimeIndex = PhaseNumber,
      PhaseClass = factor(Phase, levels = c("Active", "Inactive")),
      CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChange, "\\d+")))
    ) %>%
    left_join(POSITION_MAP, by = "PositionID") %>%
    arrange(Batch, CageChange, System, AnimalNum, Phase, PhaseNumber, PositionID)
}
position_occupancy_by_phase <- occupancy_by_phase(occupancy_intervals)
comparison_occupancy_by_phase <- occupancy_by_phase(comparison_intervals)
rm(comparison_intervals)

write_csv2(position_occupancy_by_phase, file.path(DIR_DERIVED, "all_position_occupancy_by_phase.csv"))
write_csv2(position_occupancy_by_phase, file.path(DIR_PUBTAB, "all_position_occupancy_by_phase.csv"))

# -----------------------------
# Analysis table for maps and models
# -----------------------------

occ_animal_bin <- position_occupancy_by_bin %>%
  group_by(AnimalNum, AnimalID, Batch, Sex, Group, System, CageChange, CageChangeIndex, Phase, PhaseNumber, PhaseClass) %>%
  mutate(TimeWithinPhaseHours = as.numeric(difftime(BinStart, min(BinStart, na.rm = TRUE), units = "hours"))) %>%
  ungroup() %>%
  mutate(
    Window = case_when(
      CageChangeIndex == PRIMARY_CAGE_CHANGE &
        PhaseClass == PRIMARY_PHASE &
        PhaseNumber == PRIMARY_PHASE_NUMBER &
        TimeWithinPhaseHours < PRIMARY_WINDOW_H ~ "CC1_active1_first12h",
      TRUE ~ NA_character_
    ),
    SummaryScale = "30min_bin",
    occupancy_fraction_logit = safe_logit(occupancy_fraction),
    low_read_window = observation_seconds < MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW,
    PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
    Group = factor(as.character(Group), levels = GROUP_LEVELS)
  )

# Every phase belongs to the longitudinal "Full_phase" window. The first CC1 active phase is ALSO carried as
# "CC1_active1_fullphase", the fallback primary window. It used to carry only that label, which silently left it out
# of every longitudinal (Full_phase) map, contrast and model.
occ_animal_phase_all <- position_occupancy_by_phase %>%
  mutate(
    Window = "Full_phase",
    SummaryScale = "phase",
    TimeWithinPhaseHours = NA_real_,
    occupancy_fraction_logit = safe_logit(occupancy_fraction),
    low_read_window = observation_seconds < MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW,
    PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
    Group = factor(as.character(Group), levels = GROUP_LEVELS)
  )

occ_animal_phase <- bind_rows(
  occ_animal_phase_all,
  occ_animal_phase_all %>%
    filter(CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE,
           PhaseNumber == PRIMARY_PHASE_NUMBER) %>%
    mutate(Window = "CC1_active1_fullphase")
)

occ_animal <- bind_rows(
  occ_animal_bin %>% filter(!is.na(Window)),
  occ_animal_phase
)

# ------------------------------------------------
# GROUP / SEX LABEL CONTRACT
# ------------------------------------------------
# Group and Sex must still be semantic labels here. They previously arrived as
# integer factor codes carried through the interval builder, which every
# downstream factor() call then turned into NA, emptying the animal-level and
# group-level summaries. Fail closed rather than publish empty strata.

label_contract <- function(dat, label) {
  g <- as.character(dat$Group); s <- as.character(dat$Sex)
  bad_g <- setdiff(unique(g[!is.na(g)]), GROUP_LEVELS)
  bad_s <- setdiff(unique(s[!is.na(s)]), SEX_LEVELS)
  if (length(bad_g) > 0) {
    stop(label, ': Group holds non-semantic value(s): ', paste(bad_g, collapse = ', '),
         '. Expected only ', paste(GROUP_LEVELS, collapse = '/'), '.', call. = FALSE)
  }
  if (length(bad_s) > 0) {
    stop(label, ': Sex holds non-semantic value(s): ', paste(bad_s, collapse = ', '),
         '. Expected only ', paste(SEX_LEVELS, collapse = '/'), '.', call. = FALSE)
  }
  if (any(is.na(g))) stop(label, ': ', sum(is.na(g)), ' row(s) have a missing Group.', call. = FALSE)
  if (any(is.na(s))) stop(label, ': ', sum(is.na(s)), ' row(s) have a missing Sex.', call. = FALSE)
  invisible(TRUE)
}
label_contract(occ_animal, 'occ_animal')

label_roster <- occ_animal %>% distinct(AnimalNum, Group, Sex)
label_contract_summary <- tibble::tibble(
  n_animals = dplyr::n_distinct(occ_animal$AnimalNum),
  n_CON = sum(as.character(label_roster$Group) == 'CON'),
  n_RES = sum(as.character(label_roster$Group) == 'RES'),
  n_SUS = sum(as.character(label_roster$Group) == 'SUS'),
  n_Female = sum(as.character(label_roster$Sex) == 'Female'),
  n_Male = sum(as.character(label_roster$Sex) == 'Male'),
  group_values = paste(sort(unique(as.character(label_roster$Group))), collapse = '|'),
  sex_values = paste(sort(unique(as.character(label_roster$Sex))), collapse = '|'),
  n_rows_missing_group = sum(is.na(occ_animal$Group)),
  n_rows_missing_sex = sum(is.na(occ_animal$Sex))
)
write_csv2(label_contract_summary, file.path(DIR_AUDIT, 'group_sex_label_contract.csv'))
message('Stage 19 label contract: ', label_contract_summary$n_animals, ' animals; ',
        label_contract_summary$n_CON, ' CON / ', label_contract_summary$n_RES, ' RES / ',
        label_contract_summary$n_SUS, ' SUS; ', label_contract_summary$n_Female,
        ' Female / ', label_contract_summary$n_Male, ' Male.')

if (nrow(label_roster) != label_contract_summary$n_animals) {
  stop('Group/Sex labels are not one row per animal.', call. = FALSE)
}

primary_window_label <- if (any(occ_animal$Window == "CC1_active1_first12h")) "CC1_active1_first12h" else "CC1_active1_fullphase"

write_csv2(occ_animal, file.path(DIR_DERIVED, "animal_level_reader_occupancy_for_maps.csv"))

# -----------------------------
# Summaries and contrasts
# -----------------------------

# Animal-level occupancy is time-weighted and has ONE row per animal x window x cage change x phase class x reader:
# the 30-min bins of the primary window, or the phases of Full_phase, are pooled by seconds
# (rfid_animal_reader_occupancy()). Until 2026-10-12 each bin's fraction was averaged unweighted, and Full_phase kept
# one row per phase, so the group means, SDs, n and Cohen's d below counted an animal once per phase. Units observed
# for less than MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW seconds are still dropped first.
animal_reader_keys <- c("AnimalNum", "AnimalID", "Batch", "Sex", "Group", "System", "CageChangeIndex", "PhaseClass",
                        "Window", "SummaryScale", "ReaderX", "ReaderY", "ReaderLabel", "ColumnID")
occ_animal_used <- occ_animal %>% filter(!low_read_window)

animal_reader_occ <- occ_animal_used %>%
  rfid_animal_reader_occupancy(animal_reader_keys) %>%
  mutate(
    occupancy_fraction_logit = safe_logit(occupancy_fraction),
    low_read_window = FALSE
  )

write_csv2(animal_reader_occ, file.path(DIR_DERIVED, "animal_level_reader_occupancy_summary.csv"))

# Both summaries fail closed (rfid_assert_one_row_per_animal) if an animal ever contributes more than one row.
group_occ <- rfid_group_reader_occupancy(
  animal_reader_occ,
  c("Sex", "Group", "CageChangeIndex", "PhaseClass", "Window", "ReaderX", "ReaderY", "ReaderLabel")
)

write_csv2(group_occ, file.path(DIR_DERIVED, "group_level_reader_occupancy_summary.csv"))
write_csv2(group_occ, file.path(DIR_PUBTAB, "group_level_reader_occupancy_summary.csv"))

contrast_specs <- tribble(
  ~contrast, ~group_a, ~group_b,
  "RES_minus_CON", "RES", "CON",
  "SUS_minus_CON", "SUS", "CON",
  "SUS_minus_RES", "SUS", "RES"
)

contrast_occ <- rfid_reader_occupancy_contrasts(
  animal_reader_occ,
  c("Sex", "CageChangeIndex", "PhaseClass", "Window", "ReaderX", "ReaderY", "ReaderLabel"),
  contrast_specs
)

write_csv2(contrast_occ, file.path(DIR_DERIVED, "reader_occupancy_group_contrasts_effect_sizes.csv"))
write_csv2(contrast_occ, file.path(DIR_PUBTAB, "reader_occupancy_group_contrasts_effect_sizes.csv"))

# Long-axis profile (CAGE_MAP_RESOLUTION): the same summaries with each column's two readers pooled per animal.
# Effect sizes are computed on the pooled shares, not combined from the reader-level ones.
animal_column_keys <- setdiff(animal_reader_keys, c("ReaderX", "ReaderY", "ReaderLabel", "ColumnID"))
animal_column_occ <- rfid_animal_column_occupancy(animal_reader_occ, animal_column_keys)
write_csv2(animal_column_occ, file.path(DIR_DERIVED, "animal_level_column_occupancy_summary.csv"))

group_column_occ <- rfid_group_reader_occupancy(
  animal_column_occ, c("Sex", "Group", "CageChangeIndex", "PhaseClass", "Window", "ColumnX", "ColumnLabel"),
  unit = "ColumnID")
write_csv2(group_column_occ, file.path(DIR_DERIVED, "group_level_column_occupancy_summary.csv"))
write_csv2(group_column_occ, file.path(DIR_PUBTAB, "group_level_column_occupancy_summary.csv"))

contrast_column_occ <- rfid_reader_occupancy_contrasts(
  animal_column_occ, c("Sex", "CageChangeIndex", "PhaseClass", "Window", "ColumnX", "ColumnLabel"),
  contrast_specs, unit = "ColumnID")
write_csv2(contrast_column_occ, file.path(DIR_DERIVED, "column_occupancy_group_contrasts_effect_sizes.csv"))
write_csv2(contrast_column_occ, file.path(DIR_PUBTAB, "column_occupancy_group_contrasts_effect_sizes.csv"))

# ------------------------------------------------
# SOURCE COMPARISON
# ------------------------------------------------
# The same animal-level shares from both sources, phase-level and time-weighted: the first CC1 active phase and every
# cage change x phase class. The AnimalPos side is the pre-2026-10-12 reconstruction (vendor records carried forward
# from each animal's first record in the file, off-grid records floor-snapped by preprocessing).
phase_animal_shares <- function(by_phase, source_label) {
  d <- by_phase %>%
    filter(observation_seconds >= MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW) %>%
    mutate(Group = as.character(Group), Sex = as.character(Sex), PhaseClass = as.character(PhaseClass))
  bind_rows(
    d %>% mutate(Window = "Full_phase"),
    d %>%
      filter(CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE, PhaseNumber == PRIMARY_PHASE_NUMBER) %>%
      mutate(Window = "CC1_active1_fullphase")
  ) %>%
    rfid_animal_reader_occupancy(c("AnimalNum", "Sex", "Group", "CageChangeIndex", "PhaseClass", "Window")) %>%
    mutate(source = source_label)
}
source_shares <- bind_rows(phase_animal_shares(position_occupancy_by_phase, OCCUPANCY_SOURCE),
                           phase_animal_shares(comparison_occupancy_by_phase, COMPARISON_SOURCE)) %>%
  select(AnimalNum, Sex, Group, CageChangeIndex, PhaseClass, Window, PositionID, source, occupancy_fraction) %>%
  tidyr::pivot_wider(names_from = source, values_from = occupancy_fraction)

source_comparison <- source_shares %>%
  group_by(Sex, Group, CageChangeIndex, PhaseClass, Window, PositionID) %>%
  summarise(n_animals = sum(is.finite(raw_antenna) & is.finite(animalpos)),
            mean_raw_antenna = mean(raw_antenna, na.rm = TRUE), mean_animalpos = mean(animalpos, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(difference_raw_minus_animalpos = mean_raw_antenna - mean_animalpos) %>%
  left_join(POSITION_MAP %>% select(PositionID, ReaderLabel), by = "PositionID")
source_agreement <- source_shares %>%
  filter(is.finite(raw_antenna), is.finite(animalpos)) %>%
  group_by(CageChangeIndex, PhaseClass, Window, PositionID) %>%
  summarise(n_animals = n(), pearson_r = cor(raw_antenna, animalpos),
            mean_abs_difference = mean(abs(raw_antenna - animalpos)), .groups = "drop") %>%
  left_join(POSITION_MAP %>% select(PositionID, ReaderLabel), by = "PositionID")
write_csv2(source_comparison, file.path(DIR_AUDIT, "occupancy_source_comparison_group_means.csv"))
write_csv2(source_agreement, file.path(DIR_AUDIT, "occupancy_source_agreement_by_reader.csv"))
rm(comparison_occupancy_by_phase)

# -----------------------------
# Figures
# -----------------------------
# Cage maps (Functions/rfid_cage_map_helpers.R) draw each cage at the true reader spacing, R1-R4 on top, with the
# value printed in every tile, at CAGE_MAP_RESOLUTION: the 4 x 2 reader grid (default) or four long-axis columns.
# Occupancy uses one fixed one-hue scale (0 to >= 60%) in every map, so group, animal and longitudinal maps compare by
# colour. Differences use the shared diverging scale and grey tile outlines (MMM_TILE_BORDER): percentage points take
# their own symmetric limit, Cohen's d the fixed limit (MMM_DIVERGING_LIMITS). The QC figure stays at reader level.

if (!CAGE_MAP_RESOLUTION %in% c("column", "reader")) {
  stop("CAGE_MAP_RESOLUTION must be \"column\" or \"reader\".", call. = FALSE)
}
by_column <- CAGE_MAP_RESOLUTION == "column"
map_animal_occ <- if (by_column) animal_column_occ else animal_reader_occ
map_group_occ <- if (by_column) group_column_occ else group_occ
map_contrast_occ <- if (by_column) contrast_column_occ else contrast_occ
map_unit_label <- if (by_column) "ColumnLabel" else "ReaderLabel"
map_unit_levels <- if (by_column) RFID_COLUMN_MAP$ColumnLabel else POSITION_MAP$ReaderLabel
map_uniform <- if (by_column) 1 / nrow(RFID_COLUMN_MAP) else RFID_UNIFORM_OCCUPANCY

PRIMARY_WINDOW_TEXT <- c(
  CC1_active1_first12h = "CC1, first active phase (18:30-06:30) after the first SIS cage change",
  CC1_active1_fullphase = "CC1, first active phase after the first SIS cage change"
)[[primary_window_label]]
RESOLUTION_NOTE <- if (by_column) "Each tile pools a column's two readers (R1/R5 ... R4/R8)." else ""
SOURCE_NOTE <- if (OCCUPANCY_SOURCE == "raw_antenna") {
  paste0("Raw antenna logs: each animal is assigned the antenna of its latest detection until the next one, seeded ",
         "before the window; border flips shorter than ", RAW_FLIP_FILTER_SEC, " s are removed; cage = antenna board.")
} else {
  "Vendor AnimalPos position records, each carried forward to the next record."
}
OCCUPANCY_CAPTION <- trimws(paste0(
  "Share of each animal's time at each reader, pooled over the window by seconds. ", SOURCE_NOTE, " ",
  sprintf("Uniform use would be %.1f%% per tile. ", 100 * map_uniform), RESOLUTION_NOTE))

save_figure_pair <- function(plot, name, size) {
  save_svg(plot, file.path(DIR_FIGS, name), width = size[["width"]], height = size[["height"]])
  save_svg(plot, file.path(DIR_PUBFIG, name), width = size[["width"]], height = size[["height"]])
}

as_map_labels <- function(dat) {
  dat %>%
    mutate(
      Group = factor(as.character(Group), levels = GROUP_LEVELS),
      Sex = factor(as.character(Sex), levels = SEX_LEVELS)
    )
}

occupancy_tile_text <- function(dat, value_col, suffix = "%") {
  dat %>%
    mutate(
      tile_label = rfid_format_percent(.data[[value_col]], suffix = suffix),
      tile_ink = rfid_ink_for_fill(rfid_occupancy_fill(.data[[value_col]]))
    )
}

strip_heatmap_theme <- function() {
  make_figure_theme(base_size = 7) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 6),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.spacing = unit(2.5, "mm")
    )
}

# QC: reader use of every cage epoch (Batch x CageChange x System), all animals pooled, as recorded.
qc_reader_batch_system <- position_occupancy_by_bin %>%
  group_by(Batch, CageChange, System, PositionID, ReaderX, ReaderY, ReaderLabel) %>%
  summarise(PositionSeconds = sum(PositionSeconds, na.rm = TRUE), .groups = "drop") %>%
  group_by(Batch, CageChange, System) %>%
  mutate(reader_fraction = PositionSeconds / sum(PositionSeconds, na.rm = TRUE)) %>%
  ungroup()

p_qc <- qc_reader_batch_system %>%
  mutate(ReaderLabel = factor(ReaderLabel, levels = POSITION_MAP$ReaderLabel),
         System = factor(System, levels = rev(sort(unique(System))))) %>%
  ggplot(aes(x = ReaderLabel, y = System, fill = reader_fraction)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  rfid_scale_fill_occupancy(name = "Share of\ncage time") +
  facet_grid(Batch ~ CageChange) +
  labs(
    title = "RFID reader use by cage epoch (QC)",
    subtitle = "All animals of a cage epoch pooled, by recorded reader; check reader or system imbalance first",
    x = "Reader (R1-R4 row y = 0, R5-R8 row y = 116)", y = NULL
  ) +
  strip_heatmap_theme() +
  theme(axis.text.x = element_text(size = 5), axis.text.y = element_text(size = 5))

save_figure_pair(p_qc, "qc_reader_fraction_by_batch_cagechange_system.svg", c(width = 7.2, height = 7.4))

# QC: the same 4 x 2 group means from both sources, first CC1 active phase.
source_map <- source_comparison %>%
  filter(Window == "CC1_active1_fullphase") %>%
  select(-ReaderLabel) %>%
  tidyr::pivot_longer(c(mean_raw_antenna, mean_animalpos), names_to = "source", values_to = "occ") %>%
  mutate(
    source = factor(ifelse(source == "mean_raw_antenna", "Raw antenna logs", "AnimalPos records"),
                    levels = c("Raw antenna logs", "AnimalPos records")),
    panel = factor(paste(Sex, Group), levels = paste(rep(SEX_LEVELS, each = length(GROUP_LEVELS)), GROUP_LEVELS))
  ) %>%
  left_join(POSITION_MAP, by = "PositionID") %>%
  occupancy_tile_text("occ", suffix = "")
if (nrow(source_map) > 0) {
  source_layout <- rfid_cage_map_layout(
    paste("Group means of the animals' shares of time (%) in the first CC1 active phase. Raw antenna logs: as in the",
          "other maps. AnimalPos: vendor records (written only after a >= 200-unit move, so row changes and most",
          "neighbour moves are missing) carried forward from each animal's first record, off-grid records rounded",
          "down by preprocessing; the reconstruction used until 2026-10-12."),
    n_col = 6, n_row = 2, panel_w = 1.45, extra_w = 1.9, notes = FALSE)
  p_source <- rfid_cage_map(source_map, "occ", label_col = "tile_label", ink_col = "tile_ink", reader_ids = TRUE,
                            label_pt = 5.5) +
    rfid_scale_fill_occupancy() +
    facet_grid(source ~ panel) +
    labs(title = "Cage use by data source (QC)", subtitle = PRIMARY_WINDOW_TEXT, caption = source_layout$caption)
  save_figure_pair(p_source, "qc_occupancy_source_comparison_cc1_first_active.svg", source_layout$size)
}

# Primary window: group means.
primary_group_occ <- map_group_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE) %>%
  as_map_labels() %>%
  occupancy_tile_text("mean_occupancy")

if (nrow(primary_group_occ) > 0) {
  primary_n <- primary_group_occ %>%
    group_by(Sex, Group) %>%
    summarise(note = paste0("n = ", max(n_animals)), .groups = "drop")

  layout <- rfid_cage_map_layout(paste(OCCUPANCY_CAPTION, "Tiles show the mean over animals."),
                                 n_col = 3, n_row = 2, panel_w = 2.0, extra_w = 1.25)
  p_primary <- rfid_cage_map(primary_group_occ, "mean_occupancy", resolution = CAGE_MAP_RESOLUTION,
                             label_col = "tile_label", ink_col = "tile_ink", reader_ids = TRUE,
                             notes = primary_n, label_pt = 6.5) +
    rfid_scale_fill_occupancy() +
    facet_grid(Sex ~ Group) +
    labs(title = "Cage use by group", subtitle = PRIMARY_WINDOW_TEXT, caption = layout$caption)

  save_figure_pair(p_primary, "primary_cc1_first_active_reader_occupancy_by_sex_group.svg", layout$size)
}

# Primary window: group differences, in percentage points and as Cohen's d.
primary_contrasts <- map_contrast_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE) %>%
  mutate(
    Sex = factor(as.character(Sex), levels = SEX_LEVELS),
    contrast_label = rfid_contrast_label(contrast),
    delta_pp = 100 * delta_occupancy,
    delta_label = rfid_format_signed(delta_pp, digits = 1),
    d_label = rfid_format_signed(cohens_d, digits = 2)
  )

if (nrow(primary_contrasts) > 0) {
  contrast_n <- primary_contrasts %>%
    group_by(Sex, contrast_label) %>%
    summarise(note = paste0("n = ", max(n_a), " vs ", max(n_b)), .groups = "drop")
  contrast_caption <- paste("CON animals live in CON-only cages, so contrasts with CON compare cages as well as",
                            "groups.", RESOLUTION_NOTE)

  # occupancy differences: a measure no other figure shares, so its own symmetric limit (whole percentage points)
  lim_delta <- max(abs(primary_contrasts$delta_pp), na.rm = TRUE)
  lim_delta <- if (!is.finite(lim_delta) || lim_delta == 0) 5 else ceiling(lim_delta)

  diff_layout <- rfid_cage_map_layout(
    paste("Difference of group means of the animals' shares of time; positive = more time in the first-named",
          "group.", contrast_caption),
    n_col = 3, n_row = 2, panel_w = 2.0, extra_w = 1.35)
  p_diff <- rfid_cage_map(primary_contrasts, "delta_pp", resolution = CAGE_MAP_RESOLUTION,
                          label_col = "delta_label", reader_ids = TRUE, notes = contrast_n,
                          tile_border = MMM_TILE_BORDER, label_pt = 6.5) +
    mmm_scale_fill_diverging("own", limit = lim_delta, name = "Difference\n(percentage\npoints)") +
    facet_grid(Sex ~ contrast_label) +
    labs(title = "Cage use: group differences", subtitle = PRIMARY_WINDOW_TEXT, caption = diff_layout$caption)
  save_figure_pair(p_diff, "primary_cc1_first_active_reader_occupancy_difference_maps.svg", diff_layout$size)

  d_layout <- rfid_cage_map_layout(
    paste("Cohen's d of the animals' shares of time (pooled SD); positive = more time in the first-named group.",
          "Descriptive; the mixed-model contrasts (cage random effect, BH) are in the model tables.", contrast_caption),
    n_col = 3, n_row = 2, panel_w = 2.0, extra_w = 1.35)
  p_diff_d <- rfid_cage_map(primary_contrasts, "cohens_d", resolution = CAGE_MAP_RESOLUTION,
                            label_col = "d_label", reader_ids = TRUE, notes = contrast_n,
                            tile_border = MMM_TILE_BORDER, label_pt = 6.5) +
    mmm_scale_fill_diverging("smd", name = "Cohen's d") +   # the shared limit (MMM_DIVERGING_LIMITS)
    facet_grid(Sex ~ contrast_label) +
    labs(title = "Cage use: standardized group differences", subtitle = PRIMARY_WINDOW_TEXT,
         caption = d_layout$caption)
  save_figure_pair(p_diff_d, "primary_cc1_first_active_reader_occupancy_effect_size_maps.svg", d_layout$size)
}

# Primary window: every animal, one row per cage, so a cage-wide preference shows as such.
primary_animal_occ <- map_animal_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE) %>%
  as_map_labels() %>%
  occupancy_tile_text("occupancy_fraction", suffix = "") %>%
  mutate(Cage = paste(Batch, System)) %>%
  group_by(Cage) %>%
  mutate(Slot = dense_rank(paste(as.integer(Group), AnimalNum))) %>%
  ungroup()

for (sex in SEX_LEVELS) {
  d_sex <- primary_animal_occ %>% filter(Sex == sex)
  if (nrow(d_sex) == 0) next
  panels <- d_sex %>% distinct(Cage, Slot, AnimalNum, Group)
  animal_notes <- panels %>% transmute(Cage, Slot, note = paste(AnimalNum, Group))
  animal_outlines <- panels %>% transmute(Cage, Slot, outline = unname(MMM_PALETTE_GROUP[as.character(Group)]))

  animal_layout <- rfid_cage_map_layout(
    paste(OCCUPANCY_CAPTION, "Rows are cages (Batch, ", if (OCCUPANCY_SOURCE == "raw_antenna") "antenna board" else "System label",
          "); outline colour = group; tile values in %."),
    n_col = max(panels$Slot), n_row = n_distinct(panels$Cage), panel_w = 1.3, extra_w = 1.45, strip_h = 0)
  p_animal <- rfid_cage_map(d_sex, "occupancy_fraction", resolution = CAGE_MAP_RESOLUTION,
                            label_col = "tile_label", ink_col = "tile_ink",
                            notes = animal_notes, note_side = "top", outlines = animal_outlines,
                            label_pt = 5, note_pt = 5) +
    rfid_scale_fill_occupancy() +
    facet_grid(Cage ~ Slot) +
    labs(title = paste0("Cage use by animal: ", tolower(sex), "s"), subtitle = PRIMARY_WINDOW_TEXT,
         caption = animal_layout$caption) +
    theme(strip.text.x = element_blank())

  save_figure_pair(
    p_animal, paste0("primary_cc1_first_active_reader_occupancy_by_animal_", tolower(sex), ".svg"),
    animal_layout$size)
}

# Longitudinal: group means at every cage change, as cage maps, one figure per phase class.
longitudinal_occ <- map_group_occ %>%
  filter(Window == "Full_phase") %>%
  as_map_labels() %>%
  occupancy_tile_text("mean_occupancy", suffix = "") %>%
  mutate(
    CageChange = factor(paste0("CC", CageChangeIndex), levels = paste0("CC", sort(unique(CageChangeIndex)))),
    SexGroup = factor(paste(Sex, Group), levels = paste(rep(SEX_LEVELS, each = length(GROUP_LEVELS)), GROUP_LEVELS)),
    MapUnit = factor(.data[[map_unit_label]], levels = map_unit_levels)
  )

for (phase_class in c("Active", "Inactive")) {
  d_phase <- longitudinal_occ %>% filter(PhaseClass == phase_class)
  if (nrow(d_phase) == 0) next
  long_notes <- d_phase %>%
    group_by(SexGroup, CageChange) %>%
    summarise(note = paste0("n = ", max(n_animals)), .groups = "drop")

  long_layout <- rfid_cage_map_layout(
    paste(OCCUPANCY_CAPTION, "CC4 keeps phase numbers <= 2 (its first two active phases and first inactive phase). Tile values in %."),
    n_col = nlevels(droplevels(d_phase$CageChange)), n_row = nlevels(droplevels(d_phase$SexGroup)),
    panel_w = 1.45, extra_w = 1.55)
  p_long_map <- rfid_cage_map(d_phase, "mean_occupancy", resolution = CAGE_MAP_RESOLUTION,
                              label_col = "tile_label", ink_col = "tile_ink", notes = long_notes, label_pt = 5) +
    rfid_scale_fill_occupancy() +
    facet_grid(SexGroup ~ CageChange) +
    labs(title = paste0("Cage use across SIS cage changes: ", tolower(phase_class), " phases"),
         subtitle = "All phases of each cage change pooled by seconds per animal; group means over animals",
         caption = long_layout$caption)

  save_figure_pair(
    p_long_map, paste0("longitudinal_reader_occupancy_cage_maps_", tolower(phase_class), ".svg"), long_layout$size)
}

# Longitudinal, compact: spatial unit x cage change.
if (nrow(longitudinal_occ) > 0) {
  p_long <- longitudinal_occ %>%
    ggplot(aes(x = CageChange, y = MapUnit, fill = mean_occupancy)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    geom_text(aes(label = tile_label, colour = tile_ink), size = 4.5 / .pt) +
    scale_colour_identity() +
    rfid_scale_fill_occupancy() +
    scale_y_discrete(limits = rev(map_unit_levels), name = NULL) +
    facet_grid(Sex + PhaseClass ~ Group) +
    labs(
      title = "Cage use across SIS cage changes",
      subtitle = "All phases of each cage change pooled by seconds per animal; group means over animals (%)",
      x = "Cage change"
    ) +
    strip_heatmap_theme()

  save_figure_pair(p_long, "longitudinal_reader_occupancy_position_by_cagechange.svg",
                   c(width = 6.6, height = if (by_column) 4.6 else 6.4))
}

longitudinal_contrasts <- map_contrast_occ %>%
  filter(Window == "Full_phase") %>%
  mutate(
    Sex = factor(as.character(Sex), levels = SEX_LEVELS),
    contrast_label = rfid_contrast_label(contrast),
    CageChange = factor(paste0("CC", CageChangeIndex), levels = paste0("CC", sort(unique(CageChangeIndex)))),
    MapUnit = factor(.data[[map_unit_label]], levels = map_unit_levels)
  )

if (nrow(longitudinal_contrasts) > 0) {

  p_long_d <- longitudinal_contrasts %>%
    ggplot(aes(x = CageChange, y = MapUnit, fill = cohens_d)) +
    geom_tile(colour = MMM_TILE_BORDER, linewidth = 0.25) +
    mmm_scale_fill_diverging("smd", name = "Cohen's d") +   # the shared limit (MMM_DIVERGING_LIMITS)
    scale_y_discrete(limits = rev(map_unit_levels), name = NULL) +
    facet_grid(Sex + PhaseClass ~ contrast_label) +
    labs(
      title = "Spatial redistribution effect sizes across SIS cage changes",
      subtitle = "Cohen's d of the animals' shares of time; positive = more time in the first-named group",
      x = "Cage change"
    ) +
    strip_heatmap_theme()

  save_figure_pair(p_long_d, "longitudinal_reader_occupancy_effect_size_heatmap.svg",
                   c(width = 6.6, height = if (by_column) 4.6 else 6.4))
}

# -----------------------------
# Mixed-effects statistics
# -----------------------------
# One linear mixed model per sex and spatial unit (reader, or column at column resolution), the animal being the unit
# of analysis. Until 2026-10-12 a single model put all eight logit shares, both sexes and Batch in one formula: rank
# deficient (each batch is single-sex), blind to the cage, and treating eight shares that sum to 1 as independent.
#  - Response: centred log-ratio (clr) of the animal's time at the unit, log(seconds + MODEL_PSEUDO_SECONDS) minus the
#    mean of these logs over the animal's units. A group difference in clr is the log of the fold change in time at
#    that unit relative to the animal's average unit (fold_change = exp(estimate)).
#  - Sexes are modelled separately, with Batch as a fixed effect (Group and Batch are crossed within each sex).
#  - Cage random intercept: Batch x System (System = antenna board for raw occupancy). CON animals live in CON-only
#    cages (three per sex), so contrasts with CON are between-cage contrasts.
#  - Longitudinal: Group x cage change + Batch, random intercepts for the animal and the cage epoch.
#  - Contrasts RES - CON, SUS - CON, SUS - RES from emmeans with Kenward-Roger df. p-values are BH-adjusted within
#    each model family (primary window; longitudinal), across all its contrasts; the F-tests (Kenward-Roger) are
#    BH-adjusted per term within the family. Exploratory, outside the registered analyses.

model_unit <- if (CAGE_MAP_RESOLUTION == "column") "ColumnID" else "PositionID"
model_unit_label <- if (CAGE_MAP_RESOLUTION == "column") "ColumnLabel" else "ReaderLabel"
model_animal_occ <- (if (CAGE_MAP_RESOLUTION == "column") animal_column_occ else animal_reader_occ) %>%
  mutate(UnitID = as.integer(as.character(.data[[model_unit]])), UnitLabel = .data[[model_unit_label]],
         Group = factor(as.character(Group), levels = GROUP_LEVELS),
         Sex = factor(as.character(Sex), levels = SEX_LEVELS)) %>%
  group_by(AnimalNum, Window, CageChangeIndex, PhaseClass) %>%
  mutate(n_units = n(),
         log_seconds = log(PositionSeconds + MODEL_PSEUDO_SECONDS),
         clr = log_seconds - mean(log_seconds)) %>%
  ungroup()
if (any(model_animal_occ$n_units != if (CAGE_MAP_RESOLUTION == "column") 4L else 8L)) {
  stop("Every animal x window must carry all spatial units before the log-ratio transform.", call. = FALSE)
}

contrast_weights <- list(RES_minus_CON = c(-1, 1, 0), SUS_minus_CON = c(-1, 0, 1), SUS_minus_RES = c(0, -1, 1))

# Fits one model; returns list(fit, anova, contrasts) or NULL with a warning.
fit_clr_model <- function(d, formula, by, label) {
  d <- droplevels(d)
  if (nrow(d) == 0 || !setequal(levels(d$Group), GROUP_LEVELS)) {
    warning("Skipping model ", label, ": all three groups are needed.", call. = FALSE)
    return(NULL)
  }
  fit <- tryCatch(
    lmerTest::lmer(formula, data = d, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa")),
    error = function(e) { warning("Model failed for ", label, ": ", conditionMessage(e), call. = FALSE); NULL })
  if (is.null(fit)) return(NULL)
  singular <- lme4::isSingular(fit)
  an <- as.data.frame(anova(fit, ddf = "Kenward-Roger")) %>%
    rownames_to_column("term") %>%
    rename(p_value = `Pr(>F)`, F_value = `F value`)
  emm <- if (is.null(by)) emmeans::emmeans(fit, ~ Group, lmer.df = "kenward-roger") else
    emmeans::emmeans(fit, as.formula(paste("~ Group |", by)), lmer.df = "kenward-roger")
  ct <- as.data.frame(summary(emmeans::contrast(emm, method = contrast_weights), infer = c(TRUE, TRUE))) %>%
    rename(estimate = estimate, se = SE, ci_low = lower.CL, ci_high = upper.CL, t_ratio = t.ratio, p_value = p.value) %>%
    mutate(fold_change = exp(estimate), fold_change_ci_low = exp(ci_low), fold_change_ci_high = exp(ci_high))
  info <- tibble(model = label, formula = paste(deparse(formula), collapse = " "), n_animals = n_distinct(d$AnimalNum),
                 n_cages = n_distinct(d[[if ("CageEpoch" %in% names(d)) "CageEpoch" else "Cage"]]),
                 singular = singular, df_method = "Kenward-Roger")
  list(fit = fit, anova = bind_cols(info[rep(1, nrow(an)), ], an), contrasts = bind_cols(info[rep(1, nrow(ct)), ], ct))
}

run_model_family <- function(cells, make_data, formula, by, family) {
  res <- purrr::pmap(cells, function(...) {
    cell <- tibble(...)
    label <- paste(family, paste(unlist(cell), collapse = "_"), sep = "_")
    out <- fit_clr_model(make_data(cell), formula, by, label)
    if (is.null(out)) return(NULL)
    out$anova <- bind_cols(cell[rep(1, nrow(out$anova)), ], out$anova)
    out$contrasts <- bind_cols(cell[rep(1, nrow(out$contrasts)), ], out$contrasts)
    out
  })
  res <- res[!vapply(res, is.null, logical(1))]
  anova_tab <- bind_rows(lapply(res, `[[`, "anova")) %>%
    group_by(term) %>% mutate(p_adj_BH = p.adjust(p_value, method = "BH")) %>% ungroup()
  contrast_tab <- bind_rows(lapply(res, `[[`, "contrasts")) %>%
    mutate(p_adj_BH = p.adjust(p_value, method = "BH"))
  fits <- setNames(lapply(res, `[[`, "fit"), vapply(res, function(r) r$anova$model[1], character(1)))
  list(anova = anova_tab, contrasts = contrast_tab, fits = fits)
}

write_model_family <- function(fam, label) {
  write_csv2(fam$anova, file.path(DIR_MODELS, paste0(label, "_anova.csv")))
  write_csv2(fam$contrasts, file.path(DIR_MODELS, paste0(label, "_emmeans_group_contrasts_by_position.csv")))
  write_csv2(fam$contrasts, file.path(DIR_PUBTAB, paste0(label, "_emmeans_group_contrasts_by_position.csv")))
  saveRDS(fam$fits, file.path(DIR_MODELS, paste0(label, "_lmer_fit.rds")))
}

units <- model_animal_occ %>% distinct(UnitID, UnitLabel) %>% arrange(UnitID)

primary_model_data <- model_animal_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE) %>%
  mutate(Cage = paste(Batch, System), Batch = factor(Batch))
primary_family <- run_model_family(
  tidyr::expand_grid(Sex = SEX_LEVELS, UnitLabel = units$UnitLabel),
  function(cell) primary_model_data %>% filter(Sex == cell$Sex, UnitLabel == cell$UnitLabel),
  clr ~ Group + Batch + (1 | Cage), by = NULL, family = "primary")
write_model_family(primary_family, "primary_cc1_first_active_reader_occupancy")

longitudinal_model_data <- model_animal_occ %>%
  filter(Window == "Full_phase") %>%
  mutate(CageChange = factor(paste0("CC", CageChangeIndex)), CageEpoch = paste(Batch, CageChange, System),
         Batch = factor(Batch), PhaseClass = as.character(PhaseClass))
longitudinal_family <- run_model_family(
  tidyr::expand_grid(Sex = SEX_LEVELS, PhaseClass = c("Active", "Inactive"), UnitLabel = units$UnitLabel),
  function(cell) longitudinal_model_data %>%
    filter(Sex == cell$Sex, PhaseClass == cell$PhaseClass, UnitLabel == cell$UnitLabel),
  clr ~ Group * CageChange + Batch + (1 | AnimalNum) + (1 | CageEpoch), by = "CageChange", family = "longitudinal")
write_model_family(longitudinal_family, "longitudinal_full_phase_reader_occupancy")

message(sprintf("Models: %d primary and %d longitudinal fits (%d and %d singular); %d of %d primary and %d of %d longitudinal contrasts with BH q < 0.05.",
                length(primary_family$fits), length(longitudinal_family$fits),
                sum(vapply(primary_family$fits, lme4::isSingular, logical(1))),
                sum(vapply(longitudinal_family$fits, lme4::isSingular, logical(1))),
                sum(primary_family$contrasts$p_adj_BH < 0.05, na.rm = TRUE), nrow(primary_family$contrasts),
                sum(longitudinal_family$contrasts$p_adj_BH < 0.05, na.rm = TRUE), nrow(longitudinal_family$contrasts)))

# -----------------------------
# Run log
# -----------------------------

log_tbl <- tibble(
  item = c(
    "raw_position_dir",
    "occupancy_source",
    "raw_antenna_dir",
    "raw_tag_map_dir",
    "raw_flip_filter_sec",
    "cage_map_resolution",
    "n_position_files",
    "bin_label",
    "bin_size_sec",
    "primary_window_label",
    "n_raw_position_rows",
    "n_interval_rows",
    "n_position_occupancy_rows",
    "n_animals",
    "n_batches",
    "n_cage_changes",
    "interpretation"
  ),
  value = c(
    RAW_POSITION_DIR,
    OCCUPANCY_SOURCE,
    RAW_ANTENNA_DIR,
    RAW_TAG_MAP_DIR,
    as.character(RAW_FLIP_FILTER_SEC),
    CAGE_MAP_RESOLUTION,
    as.character(length(position_files)),
    BIN_LABEL,
    as.character(BIN_SIZE_SEC),
    primary_window_label,
    as.character(nrow(all_pos)),
    as.character(nrow(occupancy_intervals)),
    as.character(nrow(position_occupancy_by_bin)),
    as.character(n_distinct(all_pos$AnimalID)),
    as.character(n_distinct(all_pos$Batch)),
    as.character(n_distinct(all_pos$CageChange)),
    paste("Discrete RFID reader occupancy (", OCCUPANCY_SOURCE, "), carried forward between detections; not continuous density tracking")
  )
)

write_csv2(log_tbl, file.path(DIR_AUDIT, "spatial_occupancy_run_log.csv"))

sink(file.path(DIR_AUDIT, "session_info.txt"))
print(sessionInfo())
sink()

message("Spatial RFID occupancy analysis complete.")
message("Primary biological window used: ", primary_window_label)
message("Check QC figures before interpreting group spatial redistribution maps.")
