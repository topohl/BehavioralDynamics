# ================================================================
# Raw Movement Publication Trajectory
# MMMSociability
# ================================================================
# Goal:
#   Generate publication-facing raw mean movement trajectories after SIS
#   social regrouping, using 10 min bins by default.
#
# Biological framing:
#   Movement is treated as the primary raw psychomotor readout. The main
#   figure emphasizes the first 12 h active phase after the first regrouping.
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

# Use 10 min bins as the default movement readout. This is a good compromise:
# less noisy than 1/5 min, less over-smoothed than 30 min.
bin_level_priority <- c("10min_based", "5min_based", "30min_based", "1min_based")
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
show_stats_on_plot <- TRUE

# ------------------------------------------------
# HELPERS
# ------------------------------------------------

source_candidates <- c(
  file.path(repo_root, "Functions", "behavioral_dynamics_helpers.R"),
  file.path("Functions", "behavioral_dynamics_helpers.R"),
  file.path("..", "Functions", "behavioral_dynamics_helpers.R")
)
helper_path <- source_candidates[file.exists(source_candidates)][1]
if (!is.na(helper_path)) source(helper_path)

if (!exists("ensure_dir")) {
  ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
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
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0, colour = "grey25"),
      plot.caption = element_text(hjust = 0, colour = "grey35", size = rel(0.85))
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

format_p <- function(p) {
  case_when(
    is.na(p) ~ "p = NA",
    p < 0.001 ~ "p < 0.001",
    TRUE ~ paste0("p = ", sprintf("%.3f", p))
  )
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

infer_bin_seconds_local <- function(bin_level) {
  lvl <- as.character(bin_level)
  if (str_detect(lvl, "10sec")) return(10)
  if (str_detect(lvl, "1min")) return(60)
  if (str_detect(lvl, "5min")) return(300)
  if (str_detect(lvl, "10min")) return(600)
  if (str_detect(lvl, "30min")) return(1800)
  if (str_detect(lvl, "legacy")) return(1800)
  600
}

pairwise_wilcox_stats <- function(dat) {
  groups <- c("CON", "RES", "SUS")
  pairs <- list(c("CON", "RES"), c("CON", "SUS"), c("RES", "SUS"))
  purrr::map_dfr(pairs, function(pp) {
    x <- dat$mean_movement[as.character(dat$Group) == pp[1]]
    y <- dat$mean_movement[as.character(dat$Group) == pp[2]]
    if (sum(is.finite(x)) < 2 || sum(is.finite(y)) < 2) {
      p <- NA_real_
    } else {
      p <- suppressWarnings(wilcox.test(x, y, exact = FALSE)$p.value)
    }
    tibble(
      contrast = paste(pp[2], "-", pp[1]),
      group1 = pp[1],
      group2 = pp[2],
      n1 = sum(is.finite(x)),
      n2 = sum(is.finite(y)),
      mean1 = safe_mean(x),
      mean2 = safe_mean(y),
      estimate_diff = safe_mean(y) - safe_mean(x),
      p.value = p
    )
  })
}

# ------------------------------------------------
# INPUT DETECTION
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
  stop("No movement input file found.", call. = FALSE)
}

bin_size_sec <- infer_bin_seconds_local(primary_bin_level)

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

# ------------------------------------------------
# LOAD AND STANDARDIZE DATA
# ------------------------------------------------

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
  mutate(
    Group = factor(Group, levels = mmm_group_levels),
    PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive")),
    Sex = factor(Sex)
  ) %>%
  arrange(AnimalNum, CageChangeIndex, PhaseClass, TimeIndex)

# ------------------------------------------------
# TABLES
# ------------------------------------------------

source_manifest <- tibble(
  Field = c("script", "input_file", "input_source_type", "selected_bin_level", "bin_size_sec", "movement_column", "output_dir"),
  Value = c("Analysis/18_raw_movement_publication_trajectory.R", input_file, input_source_type, primary_bin_level, as.character(bin_size_sec), movement_col, output_dir)
)
readr::write_csv(source_manifest, file.path(output_dirs$tables, "input_source_manifest.csv"))

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

first_active_animal_summary <- behav %>%
  filter(IsFirstActivePhase) %>%
  group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
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
  group_by(Sex, Group) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    mean_movement = safe_mean(mean_movement),
    se_movement = safe_se(mean_movement),
    mean_auc = safe_mean(auc_movement),
    se_auc = safe_se(auc_movement),
    .groups = "drop"
  )
readr::write_csv(first_active_group_summary, file.path(output_dirs$tables, "first_active_phase_group_summary.csv"))

pairwise_stats <- first_active_animal_summary %>%
  group_by(Sex) %>%
  group_modify(~ pairwise_wilcox_stats(.x)) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(p.adjust_holm = p.adjust(p.value, method = "holm")) %>%
  ungroup()
readr::write_csv(pairwise_stats, file.path(output_dirs$stats, "first_active_phase_pairwise_wilcox_stats.csv"))

anova_stats <- first_active_animal_summary %>%
  group_by(Sex) %>%
  group_modify(~ {
    dd <- .x %>% filter(is.finite(mean_movement), !is.na(Group))
    if (n_distinct(dd$Group) < 2 || nrow(dd) < 4) {
      return(tibble(test = "one_way_lm", p.value = NA_real_))
    }
    fit <- lm(log1p(mean_movement) ~ Group, data = dd)
    a <- anova(fit)
    tibble(test = "one_way_lm_log1p_movement", p.value = a$`Pr(>F)`[1])
  }) %>%
  ungroup()
readr::write_csv(anova_stats, file.path(output_dirs$stats, "first_active_phase_one_way_lm_stats.csv"))

stats_labels <- pairwise_stats %>%
  filter(contrast %in% c("RES - CON", "SUS - CON", "SUS - RES")) %>%
  mutate(label_piece = paste0(contrast, ": ", format_p(p.adjust_holm))) %>%
  group_by(Sex) %>%
  summarise(stats_label = paste(label_piece, collapse = "\n"), .groups = "drop")

plot_y <- trajectory_summary %>%
  filter(CageChangeIndex == min(behav$CageChangeIndex, na.rm = TRUE), PhaseClass == "Active", HoursAfterPhaseStart < first_active_hours) %>%
  group_by(Sex) %>%
  summarise(
    x = 0.2,
    y = max(ci95_high, mean_movement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(y = y + 0.08 * abs(y))

stats_annot <- stats_labels %>% left_join(plot_y, by = "Sex")
readr::write_csv(stats_annot, file.path(output_dirs$stats, "first_active_phase_plot_stats_labels.csv"))

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
  {if (show_stats_on_plot && nrow(stats_annot) > 0) geom_text(data = stats_annot, aes(x = x, y = y, label = stats_label), inherit.aes = FALSE, hjust = 0, vjust = 1, size = 2.0, colour = "black", lineheight = 0.92) else NULL} +
  facet_wrap(~ Sex, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  scale_x_continuous(breaks = seq(0, first_active_hours, by = 2), limits = c(0, first_active_hours)) +
  labs(
    title = "Raw movement during the first active phase after regrouping",
    subtitle = paste0("10 min bins; lines show group mean ± 95% CI; statistics are animal-level Wilcoxon tests with Holm correction"),
    x = "Hours after active-phase start",
    y = "Raw mean movement",
    caption = paste0("Input: ", basename(input_file), " | selected bin level: ", primary_bin_level)
  ) +
  make_movement_theme(base_size = 7)

save_plot_svg_pdf(
  p_first,
  file.path(output_dirs$figure_publication, "Fig18_first_active_cc1_raw_movement_trajectory_with_stats"),
  width = 165,
  height = 85
)

p_summary <- first_active_animal_summary %>%
  ggplot(aes(Group, mean_movement, colour = Group, fill = Group)) +
  geom_boxplot(width = 0.25, outlier.shape = NA, alpha = 0.65, linewidth = 0.25) +
  geom_jitter(width = 0.07, size = 1.0, alpha = 0.80) +
  {if (show_stats_on_plot && nrow(stats_annot) > 0) geom_text(data = stats_annot, aes(x = 1, y = y, label = stats_label), inherit.aes = FALSE, hjust = 0, vjust = 1, size = 2.0, colour = "black", lineheight = 0.92) else NULL} +
  facet_wrap(~ Sex, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Animal-level first-active-phase raw movement",
    subtitle = "Same endpoint used for the statistics shown in the trajectory panel",
    x = NULL,
    y = "Mean raw movement per animal"
  ) +
  make_movement_theme(base_size = 7) +
  theme(legend.position = "none")

save_plot_svg_pdf(
  p_summary,
  file.path(output_dirs$figure_publication, "Fig18_first_active_cc1_animal_summary_with_stats"),
  width = 115,
  height = 80
)

message("Raw movement publication trajectory complete: ", output_dir)
message("Selected input: ", input_file)
message("Selected bin level: ", primary_bin_level, " (", bin_size_sec, " sec bins)")
