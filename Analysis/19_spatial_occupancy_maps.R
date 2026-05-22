# ================================================================
# Spatial RFID reader-occupancy maps for SIS home-cage behavior
# MMMSociability
# ================================================================
# Goal:
#   Quantify where animals are detected across the 8 RFID reader positions
#   during the SIS protocol, with explicit cage-change, phase, sex, group,
#   cage, and batch handling.
#
# Important interpretation:
#   These are discrete RFID reader-occupancy maps, not continuous x/y tracking
#   or true spatial density estimates. POSITION_MAP is a schematic reader map.
#
# Primary biological window:
#   Cage change 1, first active phase, first 12 h, matching the early SIS
#   post-perturbation window used in the behavioral prediction analyses.
#
# Outputs:
#   analysis_ready/03_derived_metrics/spatial_occupancy/
#   analysis_ready/04_model_outputs/spatial_occupancy/
#   analysis_ready/05_figures/spatial_occupancy/
#   publication_ready/tables/spatial_occupancy/
#   publication_ready/figures/single_panels/spatial_occupancy/
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lmerTest)
  library(emmeans)
})

# -----------------------------
# User options
# -----------------------------

# Optional: set manually if auto-detection picks the wrong file.
# Examples:
# INPUT_FILE <- "analysis_ready/02_clean_data/rfid_positions_clean.csv"
# INPUT_FILE <- "analysis_ready/03_derived_metrics/position_level_data.rds"
INPUT_FILE <- NULL

MIN_READS_PER_ANIMAL_WINDOW <- 20
EPS_LOGIT <- 1e-3
PRIMARY_CAGE_CHANGE <- 1
PRIMARY_PHASE <- "Active"
PRIMARY_WINDOW_H <- 12

GROUP_LEVELS <- c("CON", "RES", "SUS")
GROUP_COLORS <- c(CON = "#3d3b6e", RES = "#C6C3BB", SUS = "#e63947")

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
# Helper functions
# -----------------------------

message2 <- function(...) message(sprintf(...))

clean_colname <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

pick_col <- function(nms, regex, required = FALSE, label = regex) {
  hit <- nms[stringr::str_detect(tolower(nms), regex)]
  if (length(hit) == 0) {
    if (required) stop("Could not find required column for: ", label, call. = FALSE)
    return(NA_character_)
  }
  hit[[1]]
}

read_any_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  message2("Reading input: %s", path)
  if (ext == "rds") return(readRDS(path))
  if (ext %in% c("csv", "txt")) return(readr::read_csv(path, show_col_types = FALSE))
  if (ext %in% c("tsv")) return(readr::read_tsv(path, show_col_types = FALSE))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Input is Excel, but package 'readxl' is not installed.", call. = FALSE)
    }
    return(readxl::read_excel(path))
  }
  stop("Unsupported input extension: ", ext, call. = FALSE)
}

find_candidate_input <- function() {
  if (!is.null(INPUT_FILE)) {
    if (!file.exists(INPUT_FILE)) stop("INPUT_FILE does not exist: ", INPUT_FILE, call. = FALSE)
    return(INPUT_FILE)
  }

  search_roots <- c(
    "analysis_ready",
    "data",
    "Data",
    "output",
    "Output",
    "results",
    "Results",
    "."
  )
  search_roots <- search_roots[dir.exists(search_roots)]

  files <- purrr::map_chr(
    search_roots,
    ~ list.files(.x, pattern = "\\.(csv|tsv|txt|rds|xlsx|xls)$", recursive = TRUE, full.names = TRUE)
  ) %>% unlist(use.names = FALSE) %>% unique()

  if (length(files) == 0) {
    stop("No candidate input files found. Set INPUT_FILE manually at the top of this script.", call. = FALSE)
  }

  score_file <- function(f) {
    nm <- tolower(f)
    score <- 0
    score <- score + 4 * stringr::str_detect(nm, "position|animalpos|reader|rfid|antenna")
    score <- score + 2 * stringr::str_detect(nm, "clean|processed|filtered|agg|derived")
    score <- score + 2 * stringr::str_detect(nm, "analysis_ready")
    score <- score - 4 * stringr::str_detect(nm, "supp|figure|model|stat|contrast|prediction|gamm|lmm|summary")
    score
  }

  ranked <- tibble(file = files, score = purrr::map_dbl(files, score_file)) %>%
    arrange(desc(score), file)

  best <- ranked$file[[1]]
  message2("Auto-selected input candidate: %s", best)
  message("If this is wrong, set INPUT_FILE manually at the top of the script.")
  best
}

standardize_columns <- function(dat) {
  names(dat) <- clean_colname(names(dat))
  nms <- names(dat)
  nms_low <- tolower(nms)

  col_animal <- pick_col(nms, "^(animalnum|animal_id|animalid|mouse_id|mouseid|rfid|tag|transponder)$|animal|mouse", TRUE, "AnimalNum")
  col_group  <- pick_col(nms, "^(group|condition|phenotype|stress_group)$", TRUE, "Group")
  col_sex    <- pick_col(nms, "^(sex|gender)$", TRUE, "Sex")
  col_batch  <- pick_col(nms, "^(batch|cohort|experiment|run)$", FALSE, "Batch")
  col_cage   <- pick_col(nms, "^(cage|cageid|cage_id|homecage)$", FALSE, "Cage")
  col_cc     <- pick_col(nms, "^(cagechange|cage_change|change|cc|regrouping|regroupingday)$|cage.*change", TRUE, "CageChange")
  col_phase  <- pick_col(nms, "^(phase|phaseclass|lightdark|light_dark|cycle)$", TRUE, "Phase")
  col_pos    <- pick_col(nms, "^(positionid|position|reader|readerid|antenna|antennaid|coil|coilid|grid|gridposition)$|position", TRUE, "PositionID")

  col_datetime <- pick_col(nms, "datetime|date_time|timestamp|time_stamp|date.*time", FALSE, "DateTime")
  col_time_h   <- pick_col(nms, "hour.*within|hours.*within|time_h|timehours|elapsed_h|elapsedhours", FALSE, "time_hours")
  col_halfhour <- pick_col(nms, "halfhour|half_hour|th_scaled|timebin|bin", FALSE, "HalfHour")

  out <- dat %>%
    mutate(
      AnimalNum = as.character(.data[[col_animal]]),
      Group = as.character(.data[[col_group]]),
      Sex = as.character(.data[[col_sex]]),
      Batch = if (!is.na(col_batch)) as.character(.data[[col_batch]]) else "Batch_unknown",
      Cage = if (!is.na(col_cage)) as.character(.data[[col_cage]]) else "Cage_unknown",
      CageChange = as.character(.data[[col_cc]]),
      Phase = as.character(.data[[col_phase]]),
      PositionID = suppressWarnings(as.integer(stringr::str_extract(as.character(.data[[col_pos]]), "\\d+")))
    )

  if (!is.na(col_datetime)) {
    out <- out %>% mutate(DateTime_raw = as.character(.data[[col_datetime]]))
  } else {
    out <- out %>% mutate(DateTime_raw = NA_character_)
  }

  if (!is.na(col_time_h)) {
    out <- out %>% mutate(TimeHours_raw = suppressWarnings(as.numeric(.data[[col_time_h]])))
  } else if (!is.na(col_halfhour)) {
    hh <- suppressWarnings(as.numeric(.data[[col_halfhour]]))
    # Most existing scripts use half-hour bins; convert bin index to hours.
    out <- out %>% mutate(TimeHours_raw = hh * 0.5)
  } else {
    out <- out %>% mutate(TimeHours_raw = NA_real_)
  }

  out %>%
    mutate(
      Group = toupper(stringr::str_trim(Group)),
      Group = dplyr::recode(Group,
        CONTROL = "CON", CTRL = "CON", C = "CON",
        RESILIENT = "RES", R = "RES",
        SUSCEPTIBLE = "SUS", S = "SUS",
        .default = Group
      ),
      Group = factor(Group, levels = GROUP_LEVELS),
      Sex = stringr::str_to_title(stringr::str_trim(Sex)),
      Sex = dplyr::recode(Sex,
        M = "Male", MALE = "Male", m = "Male", male = "Male",
        F = "Female", FEMALE = "Female", f = "Female", female = "Female",
        .default = Sex
      ),
      PhaseClass = case_when(
        stringr::str_detect(tolower(Phase), "inactive|light|day") ~ "Inactive",
        stringr::str_detect(tolower(Phase), "active|dark|night") ~ "Active",
        TRUE ~ as.character(Phase)
      ),
      PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive")),
      CageChangeIndex = suppressWarnings(as.integer(stringr::str_extract(CageChange, "\\d+"))),
      CageChangeIndex = if_else(is.na(CageChangeIndex), suppressWarnings(as.integer(CageChange)), CageChangeIndex),
      PositionID = as.integer(PositionID)
    )
}

add_time_within_window <- function(dat) {
  has_datetime <- any(!is.na(dat$DateTime_raw))

  if (has_datetime) {
    parsed <- suppressWarnings(lubridate::ymd_hms(dat$DateTime_raw, quiet = TRUE))
    parsed2 <- suppressWarnings(lubridate::ymd_hm(dat$DateTime_raw, quiet = TRUE))
    parsed <- if_else(is.na(parsed), parsed2, parsed)

    dat <- dat %>% mutate(DateTime_parsed = parsed)

    if (any(!is.na(dat$DateTime_parsed))) {
      dat <- dat %>%
        group_by(AnimalNum, CageChangeIndex, PhaseClass) %>%
        mutate(
          TimeWithinPhaseHours = as.numeric(difftime(DateTime_parsed, min(DateTime_parsed, na.rm = TRUE), units = "hours"))
        ) %>%
        ungroup()
      return(dat)
    }
  }

  if (any(!is.na(dat$TimeHours_raw))) {
    dat <- dat %>%
      group_by(AnimalNum, CageChangeIndex, PhaseClass) %>%
      mutate(TimeWithinPhaseHours = TimeHours_raw - min(TimeHours_raw, na.rm = TRUE)) %>%
      ungroup()
    return(dat)
  }

  dat %>% mutate(TimeWithinPhaseHours = NA_real_)
}

safe_logit <- function(x, eps = EPS_LOGIT) qlogis(pmin(pmax(x, eps), 1 - eps))

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
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = "svg"
  )
}

write_csv2 <- function(x, path) {
  readr::write_csv(x, path, na = "")
  message2("Saved: %s", path)
}

# -----------------------------
# Load and prepare data
# -----------------------------

input_file <- find_candidate_input()
raw0 <- read_any_table(input_file)

raw <- raw0 %>%
  standardize_columns() %>%
  add_time_within_window() %>%
  filter(!is.na(AnimalNum), !is.na(Group), !is.na(Sex), !is.na(CageChangeIndex), !is.na(PhaseClass)) %>%
  filter(PositionID %in% POSITION_MAP$PositionID) %>%
  left_join(POSITION_MAP, by = "PositionID")

if (nrow(raw) == 0) {
  stop("No usable RFID position rows after standardization/filtering. Check PositionID, Phase, CageChange, Group, and Sex columns.", call. = FALSE)
}

raw <- raw %>%
  mutate(
    Window = case_when(
      CageChangeIndex == PRIMARY_CAGE_CHANGE &
        PhaseClass == PRIMARY_PHASE &
        !is.na(TimeWithinPhaseHours) &
        TimeWithinPhaseHours < PRIMARY_WINDOW_H ~ "CC1_first_active_first12h",
      CageChangeIndex == PRIMARY_CAGE_CHANGE & PhaseClass == PRIMARY_PHASE ~ "CC1_first_active_fullphase",
      TRUE ~ "Full_phase"
    )
  )

# If no usable first-12h timing exists, use full first active phase as fallback.
if (!any(raw$Window == "CC1_first_active_first12h")) {
  warning("No rows classified as CC1_first_active_first12h. Timing information may be missing. Primary plots will fall back to CC1_first_active_fullphase.")
}

primary_window_label <- if (any(raw$Window == "CC1_first_active_first12h")) {
  "CC1_first_active_first12h"
} else {
  "CC1_first_active_fullphase"
}

# -----------------------------
# Animal-level occupancy
# -----------------------------

base_keys <- c("AnimalNum", "Batch", "Sex", "Group", "Cage", "CageChange", "CageChangeIndex", "Phase", "PhaseClass", "Window")

occ_counts <- raw %>%
  count(across(all_of(base_keys)), PositionID, ReaderX, ReaderY, name = "n_reads")

occ_animal <- occ_counts %>%
  group_by(across(all_of(base_keys))) %>%
  tidyr::complete(
    PositionID = POSITION_MAP$PositionID,
    fill = list(n_reads = 0L)
  ) %>%
  ungroup() %>%
  select(-ReaderX, -ReaderY) %>%
  left_join(POSITION_MAP, by = "PositionID") %>%
  group_by(across(all_of(base_keys))) %>%
  mutate(
    total_reads = sum(n_reads, na.rm = TRUE),
    occupancy_fraction = if_else(total_reads > 0, n_reads / total_reads, NA_real_),
    occupancy_fraction_logit = safe_logit(occupancy_fraction),
    low_read_window = total_reads < MIN_READS_PER_ANIMAL_WINDOW
  ) %>%
  ungroup() %>%
  mutate(
    PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
    Group = factor(Group, levels = GROUP_LEVELS),
    PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive"))
  )

write_csv2(occ_animal, file.path(DIR_DERIVED, "animal_level_reader_occupancy.csv"))
write_csv2(occ_animal, file.path(DIR_PUBTAB, "animal_level_reader_occupancy.csv"))

# -----------------------------
# QC tables
# -----------------------------

qc_reader_batch_cage <- raw %>%
  count(Batch, Cage, PositionID, ReaderX, ReaderY, name = "n_reads") %>%
  group_by(Batch, Cage) %>%
  mutate(
    total_reads_cage_batch = sum(n_reads),
    reader_fraction = n_reads / total_reads_cage_batch
  ) %>%
  ungroup()

qc_animal_window_reads <- occ_animal %>%
  distinct(AnimalNum, Batch, Sex, Group, Cage, CageChange, CageChangeIndex, PhaseClass, Window, total_reads, low_read_window)

write_csv2(qc_reader_batch_cage, file.path(DIR_DERIVED, "qc_reader_counts_by_batch_cage.csv"))
write_csv2(qc_animal_window_reads, file.path(DIR_DERIVED, "qc_total_reads_by_animal_window.csv"))

# -----------------------------
# Summary tables
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

# 1. Reader QC by batch/cage/position
p_qc <- qc_reader_batch_cage %>%
  mutate(PositionID = factor(PositionID, levels = POSITION_MAP$PositionID)) %>%
  ggplot(aes(x = PositionID, y = interaction(Batch, Cage, sep = " / "), fill = reader_fraction)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", name = "Reader\nfraction") +
  labs(
    title = "RFID reader-use QC by batch and cage",
    subtitle = "Detects possible reader/cage/batch imbalance before biological interpretation",
    x = "Reader position", y = "Batch / Cage"
  ) +
  theme_classic(base_size = 8) +
  theme(
    axis.text.y = element_text(size = 5),
    plot.title = element_text(face = "bold", size = 9)
  )

save_svg(p_qc, file.path(DIR_FIGS, "qc_reader_fraction_by_batch_cage.svg"), width = 7.5, height = 5.5)
save_svg(p_qc, file.path(DIR_PUBFIG, "qc_reader_fraction_by_batch_cage.svg"), width = 7.5, height = 5.5)

# 2. Primary CC1 first active occupancy maps by Sex x Group
primary_group_occ <- group_occ %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE)

if (nrow(primary_group_occ) > 0) {
  p_primary <- plot_cage_tiles(
    primary_group_occ,
    fill_col = "mean_occupancy",
    title = "Reader occupancy after first SIS cage change",
    subtitle = paste0(primary_window_label, "; animal-level normalized occupancy"),
    fill_label = "Mean\noccupancy"
  ) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, NA)) +
    facet_grid(Sex ~ Group)

  save_svg(p_primary, file.path(DIR_FIGS, "primary_cc1_first_active_reader_occupancy_by_sex_group.svg"), width = 7.5, height = 4.8)
  save_svg(p_primary, file.path(DIR_PUBFIG, "primary_cc1_first_active_reader_occupancy_by_sex_group.svg"), width = 7.5, height = 4.8)
}

# 3. Primary difference maps
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

# 4. Longitudinal position x cage-change heatmap by phase/sex/group
longitudinal_occ <- group_occ %>%
  filter(Window == "Full_phase") %>%
  mutate(
    PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
    CageChangeIndex = factor(CageChangeIndex)
  )

if (nrow(longitudinal_occ) > 0) {
  p_long <- longitudinal_occ %>%
    ggplot(aes(x = CageChangeIndex, y = PositionID, fill = mean_occupancy)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "viridis", name = "Mean\noccupancy") +
    facet_grid(Sex + PhaseClass ~ Group) +
    labs(
      title = "Longitudinal reader occupancy across SIS cage changes",
      subtitle = "Animal-level normalized full-phase occupancy",
      x = "Cage change", y = "Reader position"
    ) +
    theme_classic(base_size = 8) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 7),
      plot.title = element_text(face = "bold", size = 9)
    )

  save_svg(p_long, file.path(DIR_FIGS, "longitudinal_reader_occupancy_position_by_cagechange.svg"), width = 8.5, height = 6.2)
  save_svg(p_long, file.path(DIR_PUBFIG, "longitudinal_reader_occupancy_position_by_cagechange.svg"), width = 8.5, height = 6.2)
}

# 5. Longitudinal contrast heatmap: Cohen's d by position and cage change
longitudinal_contrasts <- contrast_occ %>%
  filter(Window == "Full_phase") %>%
  mutate(
    PositionID = factor(PositionID, levels = POSITION_MAP$PositionID),
    CageChangeIndex = factor(CageChangeIndex)
  )

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
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 7),
      plot.title = element_text(face = "bold", size = 9)
    )

  save_svg(p_long_d, file.path(DIR_FIGS, "longitudinal_reader_occupancy_effect_size_heatmap.svg"), width = 8.5, height = 6.2)
  save_svg(p_long_d, file.path(DIR_PUBFIG, "longitudinal_reader_occupancy_effect_size_heatmap.svg"), width = 8.5, height = 6.2)
}

# 6. Individual-animal primary QC maps
primary_individual <- occ_animal %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE)

if (nrow(primary_individual) > 0) {
  p_ind <- plot_cage_tiles(
    primary_individual,
    fill_col = "occupancy_fraction",
    title = "Individual-animal reader occupancy QC",
    subtitle = paste0(primary_window_label, "; inspect outliers and low-read windows"),
    fill_label = "Occupancy"
  ) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, NA)) +
    facet_wrap(~ Sex + Group + AnimalNum, ncol = 6)

  save_svg(p_ind, file.path(DIR_FIGS, "qc_individual_primary_reader_occupancy_maps.svg"), width = 10, height = 8)
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
    filter(!is.na(occupancy_fraction_logit), !is.na(Group), !is.na(Sex), !is.na(PositionID))

  if (nrow(d) == 0 || n_distinct(d$Group) < 2 || n_distinct(d$AnimalNum) < 3) {
    warning("Skipping model ", label, ": insufficient data.")
    return(NULL)
  }

  # Adaptive formula to avoid over-parameterization in small or incomplete datasets.
  form <- if (n_distinct(d$CageChangeIndex) >= 2 && n_distinct(d$PhaseClass) >= 2) {
    occupancy_fraction_logit ~ Group * PositionID * PhaseClass * CageChangeIndex * Sex + Batch + (1 | AnimalNum)
  } else if (n_distinct(d$Sex) >= 2) {
    occupancy_fraction_logit ~ Group * PositionID * Sex + Batch + (1 | AnimalNum)
  } else {
    occupancy_fraction_logit ~ Group * PositionID + Batch + (1 | AnimalNum)
  }

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

  # Position-resolved group contrasts where possible. This may be large, so keep it explicit.
  contrast_tab <- tryCatch({
    emm <- emmeans::emmeans(fit, ~ Group | PositionID + Sex, mode = "df.error")
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
    write_csv2(contrast_tab, file.path(DIR_MODELS, paste0(label, "_emmeans_group_contrasts_by_position_sex.csv")))
    write_csv2(contrast_tab, file.path(DIR_PUBTAB, paste0(label, "_emmeans_group_contrasts_by_position_sex.csv")))
  }

  saveRDS(fit, file.path(DIR_MODELS, paste0(label, "_lmer_fit.rds")))
  fit
}

primary_model_data <- occ_animal %>%
  filter(Window == primary_window_label, CageChangeIndex == PRIMARY_CAGE_CHANGE, PhaseClass == PRIMARY_PHASE)

fit_primary <- run_lmer_occupancy(primary_model_data, "primary_cc1_first_active_reader_occupancy")
fit_long <- run_lmer_occupancy(occ_animal %>% filter(Window == "Full_phase"), "longitudinal_full_phase_reader_occupancy")

# -----------------------------
# Session info and reproducibility log
# -----------------------------

log_tbl <- tibble(
  item = c(
    "input_file",
    "primary_window_label",
    "n_raw_rows_after_filtering",
    "n_animals",
    "n_batches",
    "n_cages",
    "min_reads_per_animal_window",
    "interpretation"
  ),
  value = c(
    input_file,
    primary_window_label,
    as.character(nrow(raw)),
    as.character(n_distinct(raw$AnimalNum)),
    as.character(n_distinct(raw$Batch)),
    as.character(n_distinct(raw$Cage)),
    as.character(MIN_READS_PER_ANIMAL_WINDOW),
    "Discrete RFID reader occupancy; not continuous density tracking"
  )
)

write_csv2(log_tbl, file.path(DIR_DERIVED, "spatial_occupancy_run_log.csv"))

sink(file.path(DIR_DERIVED, "session_info.txt"))
print(sessionInfo())
sink()

message("Spatial RFID occupancy analysis complete.")
message("Primary biological window used: ", primary_window_label)
message("Check QC figures before interpreting group spatial redistribution maps.")
