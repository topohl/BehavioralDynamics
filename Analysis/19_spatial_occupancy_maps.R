# ================================================================
# Spatial RFID reader-occupancy maps for SIS home-cage behavior
# MMMSociability
# ================================================================
# Goal:
#   Build reader-position occupancy maps from the PRIOR position-level RFID
#   dataset, not from all_behavior_metrics.csv.
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
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lmerTest)
  library(emmeans)
  library(lubridate)
})

# -----------------------------
# User options
# -----------------------------

RAW_POSITION_DIR <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/MMMSociability/preprocessed_data"

SUS_ANIMALS_FILE <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv"
CON_ANIMALS_FILE <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv"
ASSIGN_UNLISTED_AS_RES <- TRUE

BIN_SIZE_SEC <- 1800
BIN_LABEL <- "30min"
LONG_GAP_THRESHOLD_SEC <- 3600
EXCLUDE_LONG_GAPS <- TRUE

MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW <- 60
EPS_LOGIT <- 1e-3
PRIMARY_CAGE_CHANGE <- 1
PRIMARY_PHASE <- "Active"
PRIMARY_WINDOW_H <- 12

GROUP_LEVELS <- c("CON", "RES", "SUS")

POSITION_MAP <- tibble::tibble(
  PositionID = 1:8,
  ReaderX = c(0, 100, 200, 300, 0, 100, 200, 300),
  ReaderY = c(0,   0,   0,   0, 116, 116, 116, 116)
)

# -----------------------------
# Output folders
# -----------------------------

DIR_DERIVED <- "analysis_ready/03_derived_metrics/spatial_occupancy"
DIR_MODELS  <- "analysis_ready/04_model_outputs/spatial_occupancy"
DIR_FIGS    <- "analysis_ready/05_figures/spatial_occupancy"
DIR_PUBTAB  <- "publication_ready/tables/spatial_occupancy"
DIR_PUBFIG  <- "publication_ready/figures/single_panels/spatial_occupancy"

purrr::walk(
  c(DIR_DERIVED, DIR_MODELS, DIR_FIGS, DIR_PUBTAB, DIR_PUBFIG),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# -----------------------------
# Helpers
# -----------------------------

message2 <- function(...) message(sprintf(...))

safe_divide <- function(num, den) ifelse(is.finite(den) & den > 0, num / den, NA_real_)
safe_logit <- function(x, eps = EPS_LOGIT) qlogis(pmin(pmax(x, eps), 1 - eps))

normalize_animal_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", "") %>%
    toupper()
}

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
      AnimalID = as.character(AnimalID),
      AnimalNum = AnimalID,
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

last_meta_before <- function(dat_sys, t0) {
  dat_sys %>%
    filter(DateTime <= t0) %>%
    slice_tail(n = 1) %>%
    transmute(
      SourceFile = first(SourceFile),
      Batch = first(Batch),
      CageChange = first(CageChange),
      System = first(System),
      Phase = first(Phase),
      ConsecActive = first(ConsecActive),
      ConsecInactive = first(ConsecInactive)
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
      LongGap = duration_sec > LONG_GAP_THRESHOLD_SEC
    ) %>%
      bind_cols(interval_meta[rep(1, sum(valid)), ])
  }

  bind_rows(out)
}

split_intervals_to_bins <- function(dat, bin_size_sec) {
  if (EXCLUDE_LONG_GAPS && "LongGap" %in% names(dat)) dat <- dat %>% filter(!LongGap)

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

  interval_start_num <- as.numeric(dat$IntervalStart)
  interval_end_num <- as.numeric(dat$IntervalEnd)
  bin_start_num <- floor(interval_start_num / bin_size_sec) * bin_size_sec
  bin_end_num <- floor((interval_end_num - 1e-7) / bin_size_sec) * bin_size_sec
  n_bins <- pmax(0L, as.integer((bin_end_num - bin_start_num) / bin_size_sec) + 1L)

  keep <- n_bins > 0
  dat <- dat[keep, , drop = FALSE]
  interval_start_num <- interval_start_num[keep]
  interval_end_num <- interval_end_num[keep]
  bin_start_num <- bin_start_num[keep]
  n_bins <- n_bins[keep]

  row_idx <- rep(seq_len(nrow(dat)), n_bins)
  bin_offsets <- sequence(n_bins) - 1L
  split_bin_start_num <- rep(bin_start_num, n_bins) + bin_offsets * bin_size_sec
  split_start_num <- pmax(rep(interval_start_num, n_bins), split_bin_start_num)
  split_end_num <- pmin(rep(interval_end_num, n_bins), split_bin_start_num + bin_size_sec)
  split_duration_sec <- split_end_num - split_start_num

  out <- dat[row_idx, , drop = FALSE]
  out$IntervalStart <- as.POSIXct(split_start_num, origin = "1970-01-01", tz = "UTC")
  out$IntervalEnd <- as.POSIXct(split_end_num, origin = "1970-01-01", tz = "UTC")
  out$DurationSec <- split_duration_sec
  out$BinSizeSec <- bin_size_sec
  out$BinStart <- as.POSIXct(split_bin_start_num, origin = "1970-01-01", tz = "UTC")

  out %>% filter(is.finite(DurationSec), DurationSec > 0)
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

cohens_d <- function(x, g1, g0) {
  x1 <- x[g1]
  x0 <- x[g0]
  x1 <- x1[is.finite(x1)]
  x0 <- x0[is.finite(x0)]
  if (length(x1) < 2 || length(x0) < 2) return(NA_real_)
  s_pool <- sqrt(((length(x1) - 1) * var(x1) + (length(x0) - 1) * var(x0)) / (length(x1) + length(x0) - 2))
  if (!is.finite(s_pool) || s_pool == 0) return(NA_real_)
  (mean(x1) - mean(x0)) / s_pool
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
    AnimalID_raw = AnimalID,
    AnimalID_norm = normalize_animal_id(AnimalID),
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
    Group = factor(Group, levels = GROUP_LEVELS),
    Sex = factor(Sex),
    PhaseClass = factor(Phase, levels = c("Active", "Inactive")),
    CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChange, "\\d+")))
  )

metadata_qc <- all_pos %>%
  distinct(AnimalID_raw, AnimalID_norm, Batch, CageChange, System, Sex, Group, ReferenceGroup, ReferenceSex, SourceFile) %>%
  arrange(Batch, CageChange, System, AnimalID_norm)
write_csv2(metadata_qc, file.path(DIR_DERIVED, "raw_position_metadata_assignment_qc.csv"))

if (any(is.na(all_pos$Group))) warning("Some rows have missing Group after reference assignment.", call. = FALSE)
if (any(is.na(all_pos$Sex))) warning("Some rows have missing Sex after batch-based assignment.", call. = FALSE)

# -----------------------------
# Build occupancy intervals and binned position occupancy
# -----------------------------

message("Building occupancy intervals from raw position events...")
occupancy_intervals <- all_pos %>%
  group_by(SourceFile, Batch, CageChange, System) %>%
  group_split() %>%
  purrr::map_dfr(make_occupancy_intervals_one_system, .progress = TRUE)

if (nrow(occupancy_intervals) == 0) {
  stop("No occupancy intervals could be reconstructed. Check DateTime, AnimalID, System, and PositionID columns.", call. = FALSE)
}

position_occupancy_by_bin <- occupancy_intervals %>%
  split_intervals_to_bins(BIN_SIZE_SEC) %>%
  add_phase_number() %>%
  group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum, AnimalID, Group, Sex, PositionID) %>%
  summarise(PositionSeconds = sum(DurationSec, na.rm = TRUE), .groups = "drop") %>%
  group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, BinSizeSec, BinStart, AnimalNum, AnimalID, Group, Sex) %>%
  mutate(
    observation_seconds = sum(PositionSeconds, na.rm = TRUE),
    occupancy_fraction = safe_divide(PositionSeconds, observation_seconds)
  ) %>%
  ungroup() %>%
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
position_occupancy_by_phase <- occupancy_intervals %>%
  { if (EXCLUDE_LONG_GAPS) filter(., !LongGap) else . } %>%
  add_phase_number() %>%
  group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum, AnimalID, Group, Sex, PositionID) %>%
  summarise(PositionSeconds = sum(DurationSec, na.rm = TRUE), .groups = "drop") %>%
  group_by(SourceFile, Batch, CageChange, System, Phase, PhaseNumber, AnimalNum, AnimalID, Group, Sex) %>%
  mutate(
    observation_seconds = sum(PositionSeconds, na.rm = TRUE),
    occupancy_fraction = safe_divide(PositionSeconds, observation_seconds)
  ) %>%
  ungroup() %>%
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

write_csv2(position_occupancy_by_phase, file.path(DIR_DERIVED, "all_position_occupancy_by_phase.csv"))
write_csv2(position_occupancy_by_phase, file.path(DIR_PUBTAB, "all_position_occupancy_by_phase.csv"))

# -----------------------------
# Analysis table for maps and models
# -----------------------------

occ_animal <- position_occupancy_by_bin %>%
  group_by(AnimalNum, AnimalID, Batch, Sex, Group, System, CageChange, CageChangeIndex, Phase, PhaseClass) %>%
  mutate(TimeWithinPhaseHours = as.numeric(difftime(BinStart, min(BinStart, na.rm = TRUE), units = "hours"))) %>%
  ungroup() %>%
  mutate(
    Window = case_when(
      CageChangeIndex == PRIMARY_CAGE_CHANGE & PhaseClass == PRIMARY_PHASE & TimeWithinPhaseHours < PRIMARY_WINDOW_H ~ "CC1_first_active_first12h",
      CageChangeIndex == PRIMARY_CAGE_CHANGE & PhaseClass == PRIMARY_PHASE ~ "CC1_first_active_fullphase",
      TRUE ~ "Full_phase"
    ),
    occupancy_fraction_logit = safe_logit(occupancy_fraction),
    low_read_window = observation_seconds < MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW,
    PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
    Group = factor(Group, levels = GROUP_LEVELS)
  )

primary_window_label <- if (any(occ_animal$Window == "CC1_first_active_first12h")) "CC1_first_active_first12h" else "CC1_first_active_fullphase"

write_csv2(occ_animal, file.path(DIR_DERIVED, "animal_level_reader_occupancy_for_maps.csv"))

# -----------------------------
# Summaries and contrasts
# -----------------------------

group_occ <- occ_animal %>%
  filter(!low_read_window) %>%
  group_by(Sex, Group, CageChangeIndex, PhaseClass, Window, PositionID, ReaderX, ReaderY) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    mean_occupancy = mean(occupancy_fraction, na.rm = TRUE),
    sd_occupancy = sd(occupancy_fraction, na.rm = TRUE),
    se_occupancy = sd_occupancy / sqrt(n_animals),
    .groups = "drop"
  )

write_csv2(group_occ, file.path(DIR_DERIVED, "group_level_reader_occupancy_summary.csv"))
write_csv2(group_occ, file.path(DIR_PUBTAB, "group_level_reader_occupancy_summary.csv"))

contrast_specs <- tribble(
  ~contrast, ~group_a, ~group_b,
  "RES_minus_CON", "RES", "CON",
  "SUS_minus_CON", "SUS", "CON",
  "SUS_minus_RES", "SUS", "RES"
)

contrast_occ <- occ_animal %>%
  filter(!low_read_window) %>%
  mutate(Group_chr = as.character(Group)) %>%
  group_by(Sex, CageChangeIndex, PhaseClass, Window, PositionID, ReaderX, ReaderY) %>%
  group_modify(~ {
    d <- .x
    purrr::pmap_dfr(contrast_specs, function(contrast, group_a, group_b) {
      xa <- d$occupancy_fraction[d$Group_chr == group_a]
      xb <- d$occupancy_fraction[d$Group_chr == group_b]
      tibble(
        contrast = contrast,
        group_a = group_a,
        group_b = group_b,
        n_a = sum(d$Group_chr == group_a),
        n_b = sum(d$Group_chr == group_b),
        mean_a = mean(xa, na.rm = TRUE),
        mean_b = mean(xb, na.rm = TRUE),
        delta_occupancy = mean_a - mean_b,
        cohens_d = cohens_d(d$occupancy_fraction, d$Group_chr == group_a, d$Group_chr == group_b)
      )
    })
  }) %>%
  ungroup()

write_csv2(contrast_occ, file.path(DIR_DERIVED, "reader_occupancy_group_contrasts_effect_sizes.csv"))
write_csv2(contrast_occ, file.path(DIR_PUBTAB, "reader_occupancy_group_contrasts_effect_sizes.csv"))

# -----------------------------
# Plot helpers
# -----------------------------

cage_map_theme <- function() {
  theme_classic(base_size = 8) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 9),
      plot.subtitle = element_text(size = 7),
      plot.margin = margin(4, 4, 4, 4)
    )
}

plot_cage_tiles <- function(dat, fill_col, title, subtitle = NULL, fill_label = NULL) {
  ggplot(dat, aes(x = ReaderX, y = ReaderY, fill = .data[[fill_col]])) +
    geom_tile(width = 88, height = 88, color = "white", linewidth = 0.35) +
    geom_text(aes(label = as.character(PositionID)), size = 2.3, color = "black") +
    coord_fixed(expand = TRUE) +
    scale_y_reverse() +
    labs(title = title, subtitle = subtitle, fill = fill_label) +
    cage_map_theme()
}

# -----------------------------
# Figures
# -----------------------------

qc_reader_batch_system <- position_occupancy_by_bin %>%
  group_by(Batch, CageChange, System, PositionID, ReaderX, ReaderY) %>%
  summarise(PositionSeconds = sum(PositionSeconds, na.rm = TRUE), .groups = "drop") %>%
  group_by(Batch, CageChange, System) %>%
  mutate(reader_fraction = PositionSeconds / sum(PositionSeconds, na.rm = TRUE)) %>%
  ungroup()

p_qc <- qc_reader_batch_system %>%
  mutate(PositionID = factor(PositionID, levels = POSITION_MAP$PositionID)) %>%
  ggplot(aes(x = PositionID, y = interaction(Batch, CageChange, System, sep = " / "), fill = reader_fraction)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", name = "Reader\nfraction") +
  labs(
    title = "RFID reader-use QC by batch, cage change, and system",
    subtitle = "Check reader/system imbalance before biological interpretation",
    x = "Reader position", y = "Batch / CageChange / System"
  ) +
  theme_classic(base_size = 8) +
  theme(axis.text.y = element_text(size = 5), plot.title = element_text(face = "bold", size = 9))

save_svg(p_qc, file.path(DIR_FIGS, "qc_reader_fraction_by_batch_cagechange_system.svg"), width = 8, height = 6)
save_svg(p_qc, file.path(DIR_PUBFIG, "qc_reader_fraction_by_batch_cagechange_system.svg"), width = 8, height = 6)

primary_group_occ <- group_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE)

if (nrow(primary_group_occ) > 0) {
  p_primary <- plot_cage_tiles(
    primary_group_occ,
    fill_col = "mean_occupancy",
    title = "Reader occupancy after first SIS cage change",
    subtitle = paste0(primary_window_label, "; occupancy based on reconstructed position intervals"),
    fill_label = "Mean\noccupancy"
  ) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, NA)) +
    facet_grid(Sex ~ Group)

  save_svg(p_primary, file.path(DIR_FIGS, "primary_cc1_first_active_reader_occupancy_by_sex_group.svg"), width = 7.5, height = 4.8)
  save_svg(p_primary, file.path(DIR_PUBFIG, "primary_cc1_first_active_reader_occupancy_by_sex_group.svg"), width = 7.5, height = 4.8)
}

primary_contrasts <- contrast_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE)

if (nrow(primary_contrasts) > 0) {
  lim_delta <- max(abs(primary_contrasts$delta_occupancy), na.rm = TRUE)
  if (!is.finite(lim_delta) || lim_delta == 0) lim_delta <- 0.05

  p_diff <- plot_cage_tiles(
    primary_contrasts,
    fill_col = "delta_occupancy",
    title = "Group differences in reader occupancy",
    subtitle = paste0(primary_window_label, "; positive values indicate higher occupancy in numerator group"),
    fill_label = "Delta\noccupancy"
  ) +
    scale_fill_gradient2(limits = c(-lim_delta, lim_delta), oob = scales::squish) +
    facet_grid(Sex ~ contrast)

  save_svg(p_diff, file.path(DIR_FIGS, "primary_cc1_first_active_reader_occupancy_difference_maps.svg"), width = 8.2, height = 4.8)
  save_svg(p_diff, file.path(DIR_PUBFIG, "primary_cc1_first_active_reader_occupancy_difference_maps.svg"), width = 8.2, height = 4.8)
}

longitudinal_occ <- group_occ %>%
  filter(Window == "Full_phase") %>%
  mutate(PositionID = factor(PositionID, levels = POSITION_MAP$PositionID), CageChangeIndex = factor(CageChangeIndex))

if (nrow(longitudinal_occ) > 0) {
  p_long <- longitudinal_occ %>%
    ggplot(aes(x = CageChangeIndex, y = PositionID, fill = mean_occupancy)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "viridis", name = "Mean\noccupancy") +
    facet_grid(Sex + PhaseClass ~ Group) +
    labs(
      title = "Longitudinal reader occupancy across SIS cage changes",
      subtitle = "Animal-level normalized occupancy by 30-min bins",
      x = "Cage change", y = "Reader position"
    ) +
    theme_classic(base_size = 8) +
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold", size = 7), plot.title = element_text(face = "bold", size = 9))

  save_svg(p_long, file.path(DIR_FIGS, "longitudinal_reader_occupancy_position_by_cagechange.svg"), width = 8.5, height = 6.2)
  save_svg(p_long, file.path(DIR_PUBFIG, "longitudinal_reader_occupancy_position_by_cagechange.svg"), width = 8.5, height = 6.2)
}

longitudinal_contrasts <- contrast_occ %>%
  filter(Window == "Full_phase") %>%
  mutate(PositionID = factor(PositionID, levels = POSITION_MAP$PositionID), CageChangeIndex = factor(CageChangeIndex))

if (nrow(longitudinal_contrasts) > 0) {
  lim_d <- max(abs(longitudinal_contrasts$cohens_d), na.rm = TRUE)
  if (!is.finite(lim_d) || lim_d == 0) lim_d <- 1
  lim_d <- min(lim_d, 3)

  p_long_d <- longitudinal_contrasts %>%
    ggplot(aes(x = CageChangeIndex, y = PositionID, fill = cohens_d)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_gradient2(limits = c(-lim_d, lim_d), oob = scales::squish, name = "Cohen's d") +
    facet_grid(Sex + PhaseClass ~ contrast) +
    labs(
      title = "Spatial redistribution effect sizes across SIS cage changes",
      subtitle = "Positive d indicates higher reader occupancy in numerator group",
      x = "Cage change", y = "Reader position"
    ) +
    theme_classic(base_size = 8) +
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold", size = 7), plot.title = element_text(face = "bold", size = 9))

  save_svg(p_long_d, file.path(DIR_FIGS, "longitudinal_reader_occupancy_effect_size_heatmap.svg"), width = 8.5, height = 6.2)
  save_svg(p_long_d, file.path(DIR_PUBFIG, "longitudinal_reader_occupancy_effect_size_heatmap.svg"), width = 8.5, height = 6.2)
}

# -----------------------------
# Mixed-effects statistics
# -----------------------------

run_lmer_occupancy <- function(dat, label) {
  d <- dat %>%
    filter(!low_read_window) %>%
    mutate(
      PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
      CageChangeIndex = factor(CageChangeIndex),
      AnimalNum = factor(AnimalNum),
      Batch = factor(Batch),
      Sex = factor(Sex),
      Group = factor(Group, levels = GROUP_LEVELS),
      PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive"))
    ) %>%
    filter(!is.na(occupancy_fraction_logit), !is.na(Group), !is.na(Sex), !is.na(PositionID)) %>%
    droplevels()

  if (nrow(d) == 0 || n_distinct(d$Group) < 2 || n_distinct(d$AnimalNum) < 3) {
    warning("Skipping model ", label, ": insufficient data.")
    return(NULL)
  }

  rhs <- "Group * PositionID"
  if (n_distinct(d$Sex) >= 2) rhs <- paste(rhs, "* Sex")
  if (n_distinct(d$PhaseClass) >= 2 && n_distinct(d$CageChangeIndex) >= 2) rhs <- paste(rhs, "* PhaseClass * CageChangeIndex")
  if (n_distinct(d$Batch) >= 2) rhs <- paste(rhs, "+ Batch")
  form <- as.formula(paste("occupancy_fraction_logit ~", rhs, "+ (1 | AnimalNum)"))

  fit <- tryCatch(
    lmerTest::lmer(form, data = d, REML = FALSE, control = lme4::lmerControl(optimizer = "bobyqa")),
    error = function(e) {
      warning("Model failed for ", label, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(fit)) return(NULL)

  anova_tab <- as.data.frame(anova(fit)) %>%
    rownames_to_column("term") %>%
    mutate(
      model = label,
      p_adj_BH = p.adjust(`Pr(>F)`, method = "BH"),
      p_adj_Holm = p.adjust(`Pr(>F)`, method = "holm")
    )

  write_csv2(anova_tab, file.path(DIR_MODELS, paste0(label, "_anova.csv")))

  contrast_tab <- tryCatch({
    emm_formula <- if (n_distinct(d$Sex) >= 2) ~ Group | PositionID + Sex else ~ Group | PositionID
    emm <- emmeans::emmeans(fit, emm_formula)
    as.data.frame(emmeans::contrast(emm, method = "pairwise", adjust = "none")) %>%
      mutate(
        model = label,
        p_adj_BH = p.adjust(p.value, method = "BH"),
        p_adj_Holm = p.adjust(p.value, method = "holm")
      )
  }, error = function(e) {
    warning("Contrasts failed for ", label, ": ", conditionMessage(e))
    tibble()
  })

  if (nrow(contrast_tab) > 0) {
    write_csv2(contrast_tab, file.path(DIR_MODELS, paste0(label, "_emmeans_group_contrasts_by_position.csv")))
    write_csv2(contrast_tab, file.path(DIR_PUBTAB, paste0(label, "_emmeans_group_contrasts_by_position.csv")))
  }

  saveRDS(fit, file.path(DIR_MODELS, paste0(label, "_lmer_fit.rds")))
  fit
}

primary_model_data <- occ_animal %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE)

fit_primary <- run_lmer_occupancy(primary_model_data, "primary_cc1_first_active_reader_occupancy")
fit_long <- run_lmer_occupancy(occ_animal %>% filter(Window == "Full_phase"), "longitudinal_full_phase_reader_occupancy")

# -----------------------------
# Run log
# -----------------------------

log_tbl <- tibble(
  item = c(
    "raw_position_dir",
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
    "Discrete RFID reader occupancy from reconstructed position intervals; not continuous density tracking"
  )
)

write_csv2(log_tbl, file.path(DIR_DERIVED, "spatial_occupancy_run_log.csv"))

sink(file.path(DIR_DERIVED, "session_info.txt"))
print(sessionInfo())
sink()

message("Spatial RFID occupancy analysis complete.")
message("Primary biological window used: ", primary_window_label)
message("Check QC figures before interpreting group spatial redistribution maps.")
