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

# Prefer the same multiscale analysis-ready table used by the later scripts.
# The script checks these in order and uses the first existing file.
bin_level_priority <- c("30min_based", "10min_based", "5min_based", "1min_based")
input_candidates <- file.path(
  base_dir,
  "analysis_ready/03_derived_metrics",
  bin_level_priority,
  "all_behavior_metrics.csv"
)

# Fallback to the legacy LME/GAMM aggregate if the analysis-ready table is absent.
legacy_input_candidates <- c(
  file.path(base_dir, "MMMSociability/processed_data/data_lme_format/data_filtered_agg.csv"),
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/processed_data/aggregated/data_filtered_agg_new.csv"
)

sus_file <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv"

analysis_name <- "18_raw_movement_publication_trajectory"
primary_bin_level <- NA_character_
first_active_hours <- 12
min_bins_per_animal <- 2

# If TRUE, plot raw group means as the dominant signal and add only light smoothing.
# This avoids making the main result look model-driven.
show_light_smooth <- TRUE

# ------------------------------------------------
# SHARED HELPERS
# ------------------------------------------------

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
      legend.position = "top",
      legend.key.width = unit(6, "mm"),
      legend.key.height = unit(3.5, "mm"),
      plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 2)),
      plot.subtitle = element_text(hjust = 0, colour = "grey25", margin = margin(b = 3)),
      plot.caption = element_text(hjust = 0, colour = "grey30", size = rel(0.85)),
      plot.margin = margin(4, 4, 4, 4),
      panel.spacing = unit(1.1, "lines")
    )
}

save_plot_svg_pdf <- function(plot, filename_base, width = 160, height = 95, units = "mm") {
  ensure_dir(dirname(filename_base))
  ggplot2::ggsave(paste0(filename_base, ".svg"), plot, width = width, height = height, units = units)
  pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
  ggplot2::ggsave(paste0(filename_base, ".pdf"), plot, width = width, height = height, units = units, device = pdf_device)
  ggplot2::ggsave(paste0(filename_base, ".png"), plot, width = width, height = height, units = units, dpi = 600)
  if (exists("mirror_plot_to_standard_folder")) mirror_plot_to_standard_folder(filename_base)
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
  if (exists("infer_bin_size_sec")) {
    out <- suppressWarnings(try(infer_bin_size_sec(dat), silent = TRUE))
    if (!inherits(out, "try-error") && is.finite(out)) return(as.numeric(out))
  }
  if ("BinSizeSec" %in% names(dat)) {
    out <- median(dat$BinSizeSec[is.finite(dat$BinSizeSec)], na.rm = TRUE)
    if (is.finite(out)) return(as.numeric(out))
  }
  if ("BinLevel" %in% names(dat)) {
    lvl <- unique(as.character(dat$BinLevel))[1]
    if (str_detect(lvl, "10sec")) return(10)
    if (str_detect(lvl, "1min")) return(60)
    if (str_detect(lvl, "5min")) return(300)
    if (str_detect(lvl, "10min")) return(600)
    if (str_detect(lvl, "30min")) return(1800)
  }
  if ("HalfHourElapsed" %in% names(dat)) return(1800)
  1800
}

# ------------------------------------------------
# INPUT DETECTION AND OUTPUT STRUCTURE
# ------------------------------------------------

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
  stop("No movement input file found. Checked: ", paste(c(input_candidates, legacy_input_candidates), collapse = " | "), call. = FALSE)
}

output_dir <- file.path(base_dir, "analysis_ready", analysis_name, primary_bin_level)
output_dirs <- if (exists("analysis_output_dirs")) analysis_output_dirs(output_dir) else {
  dirs <- list(
    root = output_dir,
    tables = file.path(output_dir, "tables"),
    stats = file.path(output_dir, "stats_tables"),
    qc = file.path(output_dir, "qc"),
    figure_publication = file.path(output_dir, "figures", "publication_panels"),
    figure_supplementary = file.path(output_dir, "figures", "supplementary"),
    figure_qc = file.path(output_dir, "figures", "qc")
  )
  purrr::walk(unlist(dirs), ensure_dir)
  dirs
}

if (exists("write_output_manifest")) {
  write_output_manifest(
    output_dir,
    script_name = "18_raw_movement_publication_trajectory.R",
    analysis_name = "raw movement publication trajectory",
    primary_tables = c(
      "tables/input_source_manifest.csv",
      "tables/raw_movement_trajectory_group_summary.csv",
      "tables/first_active_phase_animal_summary.csv",
      "stats_tables/first_active_phase_lmm_contrasts.csv"
    ),
    primary_figures = c(
      "figures/publication_panels/Fig18_first_active_cc1_raw_movement_trajectory.svg",
      "figures/publication_panels/Fig18_all_cage_changes_raw_movement_trajectory.svg",
      "figures/publication_panels/Fig18_first_active_cc1_animal_summary.svg"
    ),
    notes = c(
      "Movement is treated as the primary raw psychomotor endpoint.",
      "Missing RFID/dropout periods must remain missing and should not be converted to zero.",
      "Active is interpreted as the 12 h dark/night phase; inactive as the 12 h light/day phase."
    )
  )
}

# ------------------------------------------------
# LOAD AND STANDARDIZE DATA
# ------------------------------------------------

raw_dat <- readr::read_csv(input_file, show_col_types = FALSE)

movement_col <- first_existing_col(raw_dat, c("Movement", "MeanMovement", "mean_movement", "movement", "ActivityIndex"), label = "movement column")
animal_col <- first_existing_col(raw_dat, c("AnimalNum", "AnimalID", "Animal", "RFID", "MouseID"), label = "animal column")
group_col <- first_existing_col(raw_dat, c("Group", "StressGroup", "TreatmentGroup"), label = "group column")
sex_col <- first_existing_col(raw_dat, c("Sex", "sex"), label = "sex column")
phase_col <- first_existing_col(raw_dat, c("PhaseClass", "Phase", "phase"), label = "phase column")
change_col <- first_existing_col(raw_dat, c("CageChange", "Change", "CC", "CageChangeIndex"), label = "cage-change column")
time_col <- first_existing_col(raw_dat, c("TimeIndex", "HalfHourElapsed", "HalfHourWithinCC0", "BinIndex", "bin_index", "time_bin"), label = "time column")

behav <- raw_dat %>%
  transmute(
    AnimalNum = clean_id(.data[[animal_col]]),
    Group = as.character(.data[[group_col]]),
    Sex = as.character(.data[[sex_col]]),
    PhaseRaw = as.character(.data[[phase_col]]),
    CageChangeRaw = as.character(.data[[change_col]]),
    TimeIndexRaw = suppressWarnings(as.numeric(.data[[time_col]])),
    MovementRaw = suppressWarnings(as.numeric(.data[[movement_col]])),
    BinLevel = primary_bin_level,
    InputSourceType = input_source_type
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
    CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChangeRaw, "\\d+"))),
    CageChangeIndex = if_else(is.finite(CageChangeIndex), CageChangeIndex, dense_rank(CageChangeRaw)),
    CageChange = paste0("CC", CageChangeIndex),
    TimeIndex = TimeIndexRaw,
    Movement = MovementRaw
  )

# If legacy data used old CON/non-CON labels, enforce SUS/RES from the reference list.
if (file.exists(sus_file) && !all(c("CON", "RES", "SUS") %in% unique(behav$Group))) {
  sus_ids <- readr::read_csv(sus_file, col_names = "AnimalNum", show_col_types = FALSE) %>%
    mutate(AnimalNum = clean_id(AnimalNum)) %>%
    pull(AnimalNum)
  behav <- behav %>%
    mutate(Group = case_when(
      Group == "CON" ~ "CON",
      AnimalNum %in% sus_ids ~ "SUS",
      TRUE ~ "RES"
    ))
}

behav <- behav %>%
  filter(!is.na(AnimalNum), !is.na(Group), !is.na(Sex), !is.na(PhaseClass), is.finite(TimeIndex), is.finite(Movement)) %>%
  mutate(
    Group = factor(Group, levels = unique(c(mmm_group_levels, sort(unique(Group))))),
    PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive")),
    Sex = factor(Sex),
    CageChange = factor(CageChange, levels = paste0("CC", sort(unique(CageChangeIndex))))
  ) %>%
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

if (nrow(behav) == 0) stop("No finite movement rows after standardization.", call. = FALSE)

# ------------------------------------------------
# SOURCE / QC TABLES
# ------------------------------------------------

source_manifest <- tibble(
  Field = c(
    "script", "input_file", "input_source_type", "selected_bin_level", "movement_column",
    "animal_column", "group_column", "sex_column", "phase_column", "cage_change_column",
    "time_column", "bin_size_sec", "first_active_hours", "output_dir", "generated_at"
  ),
  Value = c(
    "Analysis/18_raw_movement_publication_trajectory.R", input_file, input_source_type, primary_bin_level, movement_col,
    animal_col, group_col, sex_col, phase_col, change_col, time_col, as.character(bin_size_sec),
    as.character(first_active_hours), output_dir, as.character(Sys.time())
  )
)
readr::write_csv(source_manifest, file.path(output_dirs$tables, "input_source_manifest.csv"))

coverage_qc <- behav %>%
  group_by(BinLevel, AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(
    n_bins = n(),
    observed_hours = n_bins * bin_size_sec / 3600,
    first_time_index = min(TimeIndex, na.rm = TRUE),
    last_time_index = max(TimeIndex, na.rm = TRUE),
    mean_movement = safe_mean(Movement),
    .groups = "drop"
  ) %>%
  mutate(
    low_coverage_flag = n_bins < min_bins_per_animal,
    short_12h_phase_flag = observed_hours < first_active_hours * 0.50
  )
readr::write_csv(coverage_qc, file.path(output_dirs$tables, "raw_movement_coverage_qc.csv"))

# Duration helper QC if available.
if (exists("write_epoch_duration_qc")) {
  duration_qc <- behav %>%
    mutate(BinSizeSec = bin_size_sec) %>%
    write_epoch_duration_qc(output_dir, metric_source = analysis_name, bin_size_sec = bin_size_sec)
}

# ------------------------------------------------
# GROUP TRAJECTORY SUMMARY
# ------------------------------------------------

trajectory_summary <- behav %>%
  group_by(BinLevel, Sex, Group, CageChange, CageChangeIndex, PhaseClass, HoursAfterPhaseStart) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    n_bins = n(),
    mean_movement = safe_mean(Movement),
    se_movement = safe_se(Movement),
    ci95_low = mean_movement - 1.96 * se_movement,
    ci95_high = mean_movement + 1.96 * se_movement,
    .groups = "drop"
  ) %>%
  filter(n_animals >= 1)

readr::write_csv(trajectory_summary, file.path(output_dirs$tables, "raw_movement_trajectory_group_summary.csv"))

first_active <- behav %>%
  filter(IsFirstActivePhase)

first_active_animal_summary <- first_active %>%
  group_by(BinLevel, AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(
    n_bins = n(),
    observed_hours = n_bins * bin_size_sec / 3600,
    mean_movement = safe_mean(Movement),
    auc_movement = sum(Movement, na.rm = TRUE) * bin_size_sec / 3600,
    .groups = "drop"
  ) %>%
  filter(n_bins >= min_bins_per_animal)

readr::write_csv(first_active_animal_summary, file.path(output_dirs$tables, "first_active_phase_animal_summary.csv"))

first_active_group_summary <- first_active_animal_summary %>%
  group_by(BinLevel, Sex, Group) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    mean_movement = safe_mean(mean_movement),
    se_movement = safe_se(mean_movement),
    mean_auc = safe_mean(auc_movement),
    se_auc = safe_se(auc_movement),
    .groups = "drop"
  )
readr::write_csv(first_active_group_summary, file.path(output_dirs$tables, "first_active_phase_group_summary.csv"))

# ------------------------------------------------
# OPTIONAL LMM STATISTICS
# ------------------------------------------------

fit_stats_available <- requireNamespace("lmerTest", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)

if (fit_stats_available && nrow(first_active_animal_summary) > 0) {
  suppressPackageStartupMessages({
    library(lmerTest)
    library(emmeans)
  })

  stats_dat <- first_active_animal_summary %>%
    mutate(Group = factor(as.character(Group), levels = mmm_group_levels), Sex = factor(Sex)) %>%
    filter(!is.na(Group), is.finite(mean_movement))

  if (n_distinct(stats_dat$Group) >= 2 && n_distinct(stats_dat$AnimalNum) >= 4) {
    fit <- lmerTest::lmer(log1p(mean_movement) ~ Group * Sex + (1 | AnimalNum), data = stats_dat)
    emm <- emmeans::emmeans(fit, specs = ~ Group | Sex)
    contrasts <- emmeans::contrast(
      emm,
      method = list(
        "RES - CON" = c(-1, 1, 0),
        "SUS - CON" = c(-1, 0, 1),
        "SUS - RES" = c(0, -1, 1)
      ),
      adjust = "none"
    ) %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(
        p.value_raw = p.value,
        p.adjust_holm_within_table = p.adjust(p.value_raw, method = "holm"),
        p.adjust_bh_within_table = p.adjust(p.value_raw, method = "BH")
      )

    emm_tbl <- as.data.frame(emm) %>% as_tibble()
    readr::write_csv(contrasts, file.path(output_dirs$stats, "first_active_phase_lmm_contrasts.csv"))
    readr::write_csv(emm_tbl, file.path(output_dirs$stats, "first_active_phase_lmm_emmeans.csv"))
  }
}

# ------------------------------------------------
# FIGURES
# ------------------------------------------------

first_cc_idx <- min(behav$CageChangeIndex, na.rm = TRUE)
first_active_plot <- trajectory_summary %>%
  filter(CageChangeIndex == first_cc_idx, PhaseClass == "Active", HoursAfterPhaseStart < first_active_hours)

p_first <- ggplot(first_active_plot, aes(HoursAfterPhaseStart, mean_movement, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = ci95_low, ymax = ci95_high), alpha = 0.14, linewidth = 0, colour = NA) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 0.7, alpha = 0.80) +
  {if (show_light_smooth) geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.55, linewidth = 0.45, linetype = "solid", alpha = 0.70) else NULL} +
  facet_wrap(~ Sex, nrow = 1) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  scale_x_continuous(breaks = seq(0, first_active_hours, by = 2), limits = c(0, first_active_hours)) +
  labs(
    title = "Raw movement during the first active phase after regrouping",
    subtitle = "Group mean ± 95% CI; active phase corresponds to the 12 h dark/night phase",
    x = "Hours after active-phase start",
    y = "Raw mean movement",
    caption = paste0("Input: ", basename(input_file), " | bin: ", primary_bin_level)
  ) +
  make_movement_theme(base_size = 7)

save_plot_svg_pdf(
  p_first,
  file.path(output_dirs$figure_publication, "Fig18_first_active_cc1_raw_movement_trajectory"),
  width = 150,
  height = 75
)

p_all <- trajectory_summary %>%
  ggplot(aes(HoursAfterPhaseStart, mean_movement, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = ci95_low, ymax = ci95_high), alpha = 0.11, linewidth = 0, colour = NA) +
  geom_line(linewidth = 0.45) +
  facet_grid(Sex + PhaseClass ~ CageChange) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Raw movement trajectories across SIS cage changes",
    subtitle = "Active and inactive phases shown separately to preserve circadian structure",
    x = "Hours after phase start",
    y = "Raw mean movement",
    caption = paste0("Input: ", basename(input_file), " | bin: ", primary_bin_level)
  ) +
  make_movement_theme(base_size = 6.5)

save_plot_svg_pdf(
  p_all,
  file.path(output_dirs$figure_publication, "Fig18_all_cage_changes_raw_movement_trajectory"),
  width = 190,
  height = 120
)

p_summary <- first_active_animal_summary %>%
  ggplot(aes(Group, mean_movement, colour = Group, fill = Group)) +
  geom_boxplot(width = 0.25, outlier.shape = NA, alpha = 0.65, linewidth = 0.25) +
  geom_jitter(width = 0.07, size = 1.0, alpha = 0.80) +
  facet_wrap(~ Sex, nrow = 1) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Animal-level raw movement summary",
    subtitle = "First active phase after first regrouping",
    x = NULL,
    y = "Mean raw movement per animal"
  ) +
  make_movement_theme(base_size = 7) +
  theme(legend.position = "none")

save_plot_svg_pdf(
  p_summary,
  file.path(output_dirs$figure_publication, "Fig18_first_active_cc1_animal_summary"),
  width = 95,
  height = 70
)

# QC plot: animal coverage per cage change and phase.
p_qc <- coverage_qc %>%
  ggplot(aes(CageChange, observed_hours, colour = Group)) +
  geom_hline(yintercept = first_active_hours, linewidth = 0.25, linetype = "dashed", colour = "grey55") +
  geom_point(position = position_jitter(width = 0.08, height = 0), size = 0.9, alpha = 0.75) +
  facet_grid(Sex ~ PhaseClass) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Movement input coverage QC",
    subtitle = "Dashed line marks 12 h; low coverage suggests chip loss/dropout or shorter cage-change epochs",
    x = NULL,
    y = "Observed hours per animal/phase"
  ) +
  make_movement_theme(base_size = 7)

save_plot_svg_pdf(
  p_qc,
  file.path(output_dirs$figure_qc, "Fig18_raw_movement_coverage_qc"),
  width = 140,
  height = 90
)

if (exists("harmonize_analysis_outputs")) harmonize_analysis_outputs(output_dir)

message("Raw movement publication trajectory complete: ", output_dir)
