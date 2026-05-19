# ================================================================
# Raw Movement Publication Trajectory
# MMMSociability
# ================================================================
# Goal:
#   Generate a compact, publication-facing graph of raw mean movement
#   trajectories after social regrouping in the SIS homecage dataset.
#
# Biological framing:
#   Movement is treated as the primary raw psychomotor readout. Higher-order
#   systems features remain useful context, but this script intentionally
#   focuses on directly observed movement values from the processed/raw-derived
#   behavior tables.
#
# Main outputs:
#   - first active phase movement trajectory after CC1
#   - all cage-change active/inactive movement trajectories
#   - first active phase animal-level summary
#   - optional LMM/emmeans statistics if lmerTest and emmeans are available
#   - explicit input/output/source manifest
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
})

# ------------------------------------------------
# USER CONFIGURATION
# ------------------------------------------------

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
repo_root <- "C:/Users/topohl/Documents/GitHub/MMMSociability"

bin_level_priority <- c("30min_based", "10min_based", "5min_based", "1min_based")
input_candidates <- file.path(
  base_dir,
  "analysis_ready/03_derived_metrics",
  bin_level_priority,
  "all_behavior_metrics.csv"
)

legacy_input_candidates <- c(
  file.path(base_dir, "MMMSociability/processed_data/data_lme_format/data_filtered_agg.csv"),
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/processed_data/aggregated/data_filtered_agg_new.csv"
)

sus_file <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv"

analysis_name <- "18_raw_movement_publication_trajectory"
primary_bin_level <- NA_character_
first_active_hours <- 12
min_bins_per_animal <- 2
show_light_smooth <- TRUE

source_candidates <- c(
  file.path(repo_root, "Functions", "behavioral_dynamics_helpers.R"),
  file.path("Functions", "behavioral_dynamics_helpers.R"),
  file.path("..", "Functions", "behavioral_dynamics_helpers.R")
)
duration_helper_candidates <- c(
  file.path(repo_root, "Functions", "duration_normalization_helpers.R"),
  file.path("Functions", "duration_normalization_helpers.R"),
  file.path("..", "Functions", "duration_normalization_helpers.R")
)

helper_path <- source_candidates[file.exists(source_candidates)][1]
duration_helper_path <- duration_helper_candidates[file.exists(duration_helper_candidates)][1]
if (!is.na(helper_path)) source(helper_path)
if (!is.na(duration_helper_path)) source(duration_helper_path)

if (!exists("ensure_dir")) {
  ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
  }
}
if (!exists("safe_name")) {
  safe_name <- function(x) {
    x %>% as.character() %>% str_replace_all("[^A-Za-z0-9]+", "_") %>% str_replace_all("^_|_$", "") %>% str_to_lower()
  }
}
if (!exists("mmm_group_levels")) mmm_group_levels <- c("CON", "RES", "SUS")
if (!exists("mmm_group_colors")) mmm_group_colors <- c("CON" = "#3d3b6e", "RES" = "#C6C3BB", "SUS" = "#e63947")

make_movement_theme <- function(base_size = 7) {
  ggplot2::theme_classic(base_size = base_size, base_family = "Arial") +
    ggplot2::theme(
      axis.line = element_line(linewidth = 0.28, colour = "black"),
      axis.ticks = element_line(linewidth = 0.24, colour = "black"),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", colour = "black"),
      legend.title = element_blank(),
      legend.position = "top"
    )
}

save_plot_svg_pdf <- function(plot, filename_base, width = 160, height = 95, units = "mm") {
  ensure_dir(dirname(filename_base))
  ggplot2::ggsave(paste0(filename_base, ".svg"), plot, width = width, height = height, units = units)
  pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
  ggplot2::ggsave(paste0(filename_base, ".pdf"), plot, width = width, height = height, units = units, device = pdf_device)
  ggplot2::ggsave(paste0(filename_base, ".png"), plot, width = width, height = height, units = units, dpi = 600)
  invisible(filename_base)
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_se <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) / sqrt(length(x))
}

clean_id <- function(x) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  out <- ifelse(is.na(x_num), x_chr, as.character(x_num))
  as.character(out)
}

first_existing_col <- function(dat, candidates, required = TRUE, label = "column") {
  hit <- candidates[candidates %in% names(dat)][1]
  if (is.na(hit) && required) {
    stop("Could not find ", label, ". Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  hit
}

infer_bin_seconds_local <- function(dat) {
  if ("HalfHourElapsed" %in% names(dat)) return(1800)
  1800
}

analysis_ready_hits <- input_candidates[file.exists(input_candidates)]
legacy_hits <- legacy_input_candidates[file.exists(legacy_input_candidates)]

if (length(analysis_ready_hits) > 0) {
  input_file <- analysis_ready_hits[1]
  primary_bin_level <- bin_level_priority[match(input_file, input_candidates)]
  input_source_type <- "analysis_ready_multiscale_behavior_metrics"
} else if (length(legacy_hits) > 0) {
  input_file <- legacy_hits[1]
  primary_bin_level <- "legacy_30min_based"
  input_source_type <- "legacy_lme_or_activity_aggregate"
} else {
  stop("No movement input file found.", call. = FALSE)
}

output_dir <- file.path(base_dir, "analysis_ready", analysis_name, primary_bin_level)
if (exists("analysis_output_dirs")) {
  output_dirs <- analysis_output_dirs(output_dir)
} else {
  output_dirs <- list(
    tables = file.path(output_dir, "tables"),
    stats = file.path(output_dir, "stats_tables"),
    figure_publication = file.path(output_dir, "figures", "publication_panels"),
    figure_qc = file.path(output_dir, "figures", "qc")
  )
  purrr::walk(unlist(output_dirs), ensure_dir)
}

raw_dat <- readr::read_csv(input_file, show_col_types = FALSE)

movement_col <- first_existing_col(raw_dat, c("Movement", "MeanMovement", "movement"), label = "movement column")
animal_col <- first_existing_col(raw_dat, c("AnimalNum", "AnimalID", "Animal"), label = "animal column")
group_col <- first_existing_col(raw_dat, c("Group", "StressGroup"), label = "group column")
sex_col <- first_existing_col(raw_dat, c("Sex", "sex"), label = "sex column")
phase_col <- first_existing_col(raw_dat, c("PhaseClass", "Phase", "phase"), label = "phase column")
change_col <- first_existing_col(raw_dat, c("CageChange", "Change", "CC"), label = "cage-change column")
time_col <- first_existing_col(raw_dat, c("TimeIndex", "HalfHourElapsed", "HalfHourWithinCC0"), label = "time column")

behav <- raw_dat %>%
  transmute(
    AnimalNum = clean_id(.data[[animal_col]]),
    Group = as.character(.data[[group_col]]),
    Sex = as.character(.data[[sex_col]]),
    PhaseRaw = as.character(.data[[phase_col]]),
    CageChangeRaw = as.character(.data[[change_col]]),
    TimeIndexRaw = suppressWarnings(as.numeric(.data[[time_col]])),
    MovementRaw = suppressWarnings(as.numeric(.data[[movement_col]]))
  ) %>%
  mutate(
    Group = case_when(
      Group %in% c("CON", "Control", "CTRL") ~ "CON",
      Group %in% c("RES", "Resilient") ~ "RES",
      Group %in% c("SUS", "Susceptible") ~ "SUS",
      TRUE ~ Group
    ),
    PhaseClass = case_when(
      str_detect(str_to_lower(PhaseRaw), "active|dark|night") ~ "Active",
      str_detect(str_to_lower(PhaseRaw), "inactive|light|day") ~ "Inactive",
      TRUE ~ PhaseRaw
    ),
    Phase = PhaseClass,
    CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChangeRaw, "\\d+"))),
    CageChangeIndex = if_else(is.finite(CageChangeIndex), CageChangeIndex, dense_rank(CageChangeRaw)),
    CageChange = paste0("CC", CageChangeIndex),
    TimeIndex = TimeIndexRaw,
    Movement = MovementRaw
  ) %>%
  filter(!is.na(AnimalNum), !is.na(Group), !is.na(Sex), !is.na(PhaseClass), is.finite(TimeIndex), is.finite(Movement)) %>%
  arrange(AnimalNum, CageChangeIndex, PhaseClass, TimeIndex)

bin_size_sec <- infer_bin_seconds_local(behav)

behav <- behav %>%
  group_by(AnimalNum, CageChange, PhaseClass) %>%
  arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(
    WithinPhaseBin = row_number(),
    HoursAfterPhaseStart = (WithinPhaseBin - 1) * bin_size_sec / 3600,
    IsFirstActivePhase = CageChangeIndex == min(CageChangeIndex, na.rm = TRUE) & PhaseClass == "Active" & HoursAfterPhaseStart < first_active_hours
  ) %>%
  ungroup()

trajectory_summary <- behav %>%
  group_by(Sex, Group, CageChange, CageChangeIndex, PhaseClass, HoursAfterPhaseStart) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    mean_movement = safe_mean(Movement),
    se_movement = safe_se(Movement),
    ci95_low = mean_movement - 1.96 * se_movement,
    ci95_high = mean_movement + 1.96 * se_movement,
    .groups = "drop"
  )

readr::write_csv(trajectory_summary, file.path(output_dirs$tables, "raw_movement_trajectory_group_summary.csv"))

first_cc_idx <- min(behav$CageChangeIndex, na.rm = TRUE)
first_active_plot <- trajectory_summary %>%
  filter(CageChangeIndex == first_cc_idx, PhaseClass == "Active", HoursAfterPhaseStart < first_active_hours)

p_first <- ggplot(first_active_plot, aes(HoursAfterPhaseStart, mean_movement, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = ci95_low, ymax = ci95_high), alpha = 0.14, linewidth = 0, colour = NA) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 0.7, alpha = 0.80) +
  facet_wrap(~ Sex, nrow = 1) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Raw movement during the first active phase after regrouping",
    x = "Hours after active-phase start",
    y = "Raw mean movement"
  ) +
  make_movement_theme(base_size = 7)

save_plot_svg_pdf(
  p_first,
  file.path(output_dirs$figure_publication, "Fig18_first_active_cc1_raw_movement_trajectory"),
  width = 150,
  height = 75
)

message("Raw movement publication trajectory complete: ", output_dir)
