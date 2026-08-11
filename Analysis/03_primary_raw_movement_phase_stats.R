# ================================================================
# Secondary Broad Raw Movement Phase Statistics
# MMMSociability
# ================================================================
# Goal:
#   Corrected statistics and plotting for raw mean movement endpoints:
#   1) overall movement,
#   2) overall active/inactive movement,
#   3) cage-change x active/inactive movement.
#
# Manuscript role and correction relative to 18b:
#   - Stage 09 is the primary prospective prediction analysis; this Stage 03
#     scan is secondary phenotype/group characterization.
#   - Plotted p-values are Holm-adjusted within the displayed panel only
#     across the three planned pairwise contrasts: RES-CON, SUS-CON, SUS-RES.
#   - No wider global family-wise correction is exported when
#     export_global_family_corrections = FALSE.
#   - Optional repeated-measures LMMs are exported separately for inference
#     across all cage-change/phase epochs.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(ggsignif)
})

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level_priority <- c("10min_based", "5min_based", "30min_based", "1min_based")
input_candidates <- file.path(base_dir, "analysis_ready/03_derived_metrics", bin_level_priority, "all_behavior_metrics.csv")
analysis_name <- "03_primary_raw_movement_phase_stats"
min_bins_per_animal <- 2
export_global_family_corrections <- FALSE
combz_endpoint_file <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
combz_endpoint_sheet <- "zScore"

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"),
  file.path(dirname(tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) getwd())), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)

if (!exists("ensure_dir")) {
  ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
  }
}
if (!exists("mmm_group_levels")) mmm_group_levels <- c("CON", "RES", "SUS")
if (!exists("mmm_group_colors")) mmm_group_colors <- c("CON" = "#3d3b6e", "RES" = "#C6C3BB", "SUS" = "#e63947")

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

safe_first_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

clean_id <- function(x) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  ifelse(is.na(x_num), x_chr, as.character(x_num))
}

first_existing_col <- function(dat, candidates, label = "column") {
  hit <- candidates[candidates %in% names(dat)][1]
  if (is.na(hit)) stop("Could not find ", label, ". Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  hit
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "NA",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

format_p_inline <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "NA",
    p < 0.001 ~ "< 0.001",
    TRUE ~ paste0("= ", sprintf("%.3f", p))
  )
}

safe_correlation_tests <- function(dat, x_col = "mean_movement", y_col = "CombZ") {
  dd <- dat %>%
    filter(is.finite(.data[[x_col]]), is.finite(.data[[y_col]]))

  n_animals <- n_distinct(dd$AnimalNum)
  skip_reason <- NA_character_
  if (n_animals < 4) {
    skip_reason <- "n < 4"
  } else if (sd(dd[[x_col]], na.rm = TRUE) == 0 || sd(dd[[y_col]], na.rm = TRUE) == 0) {
    skip_reason <- "zero variance"
  }

  if (!is.na(skip_reason)) {
    return(tibble(
      n_animals = n_animals,
      pearson_r = NA_real_,
      pearson_p = NA_real_,
      spearman_rho = NA_real_,
      spearman_p = NA_real_,
      skipped_reason = skip_reason
    ))
  }

  pearson <- tryCatch(
    suppressWarnings(cor.test(dd[[x_col]], dd[[y_col]], method = "pearson")),
    error = function(e) NULL
  )
  spearman <- tryCatch(
    suppressWarnings(cor.test(dd[[x_col]], dd[[y_col]], method = "spearman", exact = FALSE)),
    error = function(e) NULL
  )

  tibble(
    n_animals = n_animals,
    pearson_r = if (is.null(pearson)) NA_real_ else unname(pearson$estimate),
    pearson_p = if (is.null(pearson)) NA_real_ else pearson$p.value,
    spearman_rho = if (is.null(spearman)) NA_real_ else unname(spearman$estimate),
    spearman_p = if (is.null(spearman)) NA_real_ else spearman$p.value,
    skipped_reason = NA_character_
  )
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

pairwise_wilcox_stats <- function(dat, value_col = "mean_movement") {
  planned_pairs <- list(c("CON", "RES"), c("CON", "SUS"), c("RES", "SUS"))
  purrr::map_dfr(planned_pairs, function(pp) {
    x <- dat[[value_col]][as.character(dat$Group) == pp[1]]
    y <- dat[[value_col]][as.character(dat$Group) == pp[2]]
    p <- if (sum(is.finite(x)) >= 2 && sum(is.finite(y)) >= 2) {
      suppressWarnings(wilcox.test(x, y, exact = FALSE)$p.value)
    } else {
      NA_real_
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
      p_raw = p
    )
  })
}

contrast_levels <- c("RES - CON", "SUS - CON", "SUS - RES")

make_theme <- function(base_size = 7) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(
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

save_plot <- function(plot, filename_base, width = 170, height = 100, units = "mm") {
  ensure_dir(dirname(filename_base))
  ggsave(paste0(filename_base, ".svg"), plot, width = width, height = height, units = units)
  pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
  ggsave(paste0(filename_base, ".pdf"), plot, width = width, height = height, units = units, device = pdf_device)
  ggsave(paste0(filename_base, ".png"), plot, width = width, height = height, units = units, dpi = 600)
  invisible(filename_base)
}

# ------------------------------------------------
# LOAD DATA
# ------------------------------------------------

hits <- input_candidates[file.exists(input_candidates)]
if (length(hits) == 0) stop("No all_behavior_metrics.csv found in analysis_ready/03_derived_metrics.", call. = FALSE)
input_file <- hits[1]
bin_level <- bin_level_priority[match(input_file, input_candidates)]
output_dir <- file.path(base_dir, "analysis_ready", analysis_name, bin_level)

if (exists("analysis_output_dirs")) {
  dirs <- analysis_output_dirs(output_dir)
} else {
  dirs <- list(
    tables = file.path(output_dir, "tables"),
    stats = file.path(output_dir, "stats_tables"),
    figure_publication = file.path(output_dir, "figures", "publication_panels")
  )
  purrr::walk(unlist(dirs), ensure_dir)
}

raw_dat <- readr::read_csv(input_file, show_col_types = FALSE)
movement_col <- first_existing_col(raw_dat, c("Movement", "MeanMovement", "movement"), "movement column")
animal_col <- first_existing_col(raw_dat, c("AnimalNum", "AnimalID", "Animal"), "animal column")
group_col <- first_existing_col(raw_dat, c("Group", "StressGroup"), "group column")
sex_col <- first_existing_col(raw_dat, c("Sex", "sex"), "sex column")
phase_col <- first_existing_col(raw_dat, c("PhaseClass", "Phase", "phase"), "phase column")
change_col <- first_existing_col(raw_dat, c("CageChange", "Change", "CC"), "cage-change column")

if (exists("write_output_manifest")) {
  write_output_manifest(
    output_dir,
    script_name = "03_primary_raw_movement_phase_stats.R",
    analysis_name = "secondary raw movement phenotype/group characterization",
    input_files = input_file,
    output_directory = output_dir,
    bin_level = bin_level,
    key_parameters = list(
      min_bins_per_animal = min_bins_per_animal,
      movement_column = movement_col,
      panel_p_adjustment = "Holm within displayed endpoint panel",
      export_global_family_corrections = export_global_family_corrections
    ),
    primary_tables = c(
      "tables/raw_movement_phase_filter_qc.csv",
      "tables/raw_movement_animal_level_endpoints.csv",
      "tables/raw_movement_combz_input_overall_active_inactive.csv",
      "stats_tables/raw_movement_combz_correlations_by_sex_phase.csv",
      "stats_tables/raw_movement_combz_correlations_cagechange_phase.csv",
      "tables/raw_movement_group_summary.csv",
      "tables/raw_movement_overall_active_inactive_panel_counts.csv"
    ),
    primary_figures = c(
      "figures/publication_panels/raw_movement_active_inactive_vs_combz.svg",
      "figures/publication_panels/Fig18c_overall_active_inactive_mean_movement_corrected_stats.svg",
      "figures/publication_panels/Fig18c_cage_change_phase_mean_movement_corrected_stats.svg",
      "figures/publication_panels/Fig18c_cage_change_phase_pairwise_pvalue_heatmap_corrected.svg",
      "figures/supplementary/raw_movement_cagechange_phase_vs_combz.svg"
    )
  )
}

behav <- raw_dat %>%
  transmute(
    AnimalNum = clean_id(.data[[animal_col]]),
    Group = as.character(.data[[group_col]]),
    Sex = as.character(.data[[sex_col]]),
    PhaseRaw = as.character(.data[[phase_col]]),
    CageChangeRaw = as.character(.data[[change_col]]),
    Movement = suppressWarnings(as.numeric(.data[[movement_col]]))
  ) %>%
  mutate(
    PhaseNorm = str_to_lower(str_trim(PhaseRaw)),
    Group = case_when(
      Group %in% c("CON", "Control", "CTRL") ~ "CON",
      Group %in% c("RES", "Resilient") ~ "RES",
      Group %in% c("SUS", "Susceptible") ~ "SUS",
      TRUE ~ Group
    ),
    PhaseClass = case_when(
      PhaseNorm %in% c("inactive", "light", "day") ~ "Inactive",
      PhaseNorm %in% c("active", "dark", "night") ~ "Active",
      str_detect(PhaseNorm, "\\binactive\\b|\\blight\\b|\\bday\\b") ~ "Inactive",
      str_detect(PhaseNorm, "\\bactive\\b|\\bdark\\b|\\bnight\\b") ~ "Active",
      TRUE ~ PhaseRaw
    ),
    CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChangeRaw, "\\d+"))),
    CageChangeIndex = if_else(is.finite(CageChangeIndex), CageChangeIndex, dense_rank(CageChangeRaw)),
    CageChange = paste0("CC", CageChangeIndex),
    Group = factor(Group, levels = mmm_group_levels),
    Sex = factor(Sex),
    PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive"))
  ) %>%
  filter(!is.na(AnimalNum), !is.na(Group), !is.na(Sex), !is.na(PhaseClass), is.finite(Movement))

readr::write_csv(
  tibble(Field = c("script", "input_file", "bin_level", "movement_column", "output_dir"),
         Value = c("Analysis/03_primary_raw_movement_phase_stats.R", input_file, bin_level, movement_col, output_dir)),
  file.path(dirs$manifest, "input_source_manifest.csv")
)

# ------------------------------------------------
# ANIMAL-LEVEL ENDPOINTS
# ------------------------------------------------

overall_endpoint <- behav %>%
  group_by(AnimalNum, Group, Sex) %>%
  summarise(n_bins = n(), mean_movement = safe_mean(Movement), .groups = "drop") %>%
  mutate(ScopeType = "overall_all_phases", Endpoint = "Overall", CageChange = "All", CageChangeIndex = 0L, PhaseClass = "All")

phase_endpoint <- behav %>%
  group_by(AnimalNum, Group, Sex, PhaseClass) %>%
  summarise(n_bins = n(), mean_movement = safe_mean(Movement), .groups = "drop") %>%
  mutate(ScopeType = "overall_by_phase", Endpoint = as.character(PhaseClass), CageChange = "All", CageChangeIndex = 0L)

cc_phase_endpoint <- behav %>%
  group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(n_bins = n(), mean_movement = safe_mean(Movement), .groups = "drop") %>%
  mutate(ScopeType = "cage_change_by_phase", Endpoint = paste(CageChange, PhaseClass, sep = "_"))

movement_endpoints_pre_filter <- bind_rows(overall_endpoint, phase_endpoint, cc_phase_endpoint) %>%
  mutate(
    Endpoint = as.character(Endpoint),
    PhaseClass = as.character(PhaseClass),
    CageChange = as.character(CageChange)
  )

phase_filter_qc <- movement_endpoints_pre_filter %>%
  filter(ScopeType == "overall_by_phase") %>%
  group_by(Sex, PhaseClass) %>%
  summarise(
    n_animals_pre_filter = n_distinct(AnimalNum),
    n_rows_pre_filter = n(),
    n_animals_post_filter = n_distinct(AnimalNum[n_bins >= min_bins_per_animal]),
    n_rows_post_filter = sum(n_bins >= min_bins_per_animal),
    .groups = "drop"
  )
readr::write_csv(phase_filter_qc, file.path(dirs$tables, "raw_movement_phase_filter_qc.csv"))

movement_endpoints <- movement_endpoints_pre_filter %>%
  filter(n_bins >= min_bins_per_animal) %>%
  mutate(
    Endpoint = as.character(Endpoint),
    PhaseClass = as.character(PhaseClass),
    CageChange = as.character(CageChange)
  )
readr::write_csv(movement_endpoints, file.path(dirs$tables, "raw_movement_animal_level_endpoints.csv"))

# ------------------------------------------------
# DESCRIPTIVE RAW MOVEMENT x COMBZ ASSOCIATIONS
# ------------------------------------------------

if (!file.exists(combz_endpoint_file)) {
  stop("CombZ endpoint file not found: ", combz_endpoint_file, call. = FALSE)
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl to read CombZ endpoint Excel files.", call. = FALSE)
}

combz_raw <- readxl::read_excel(combz_endpoint_file, sheet = combz_endpoint_sheet)
combz_animal_col <- first_existing_col(combz_raw, c("ID", "AnimalNum", "Animal", "MouseID", "Mouse", "RFID", "animal_id"), "CombZ animal column")
combz_col <- first_existing_col(combz_raw, c("CombZ", "combz", "Comb_Z", "CompositeZ"), "CombZ column")

combz_endpoints <- combz_raw %>%
  transmute(
    AnimalNum = clean_id(.data[[combz_animal_col]]),
    CombZ = suppressWarnings(as.numeric(.data[[combz_col]]))
  ) %>%
  filter(!is.na(AnimalNum)) %>%
  group_by(AnimalNum) %>%
  summarise(CombZ = safe_first_finite(CombZ), .groups = "drop")

movement_endpoints_combz <- movement_endpoints %>%
  left_join(combz_endpoints, by = "AnimalNum")

combz_main_input <- movement_endpoints_combz %>%
  filter(
    ScopeType %in% c("overall_by_phase"),
    Endpoint %in% c("Active", "Inactive"),
    is.finite(mean_movement),
    is.finite(CombZ)
  ) %>%
  mutate(
    Sex = factor(as.character(Sex)),
    Endpoint = factor(as.character(Endpoint), levels = c("Active", "Inactive")),
    PhaseClass = factor(as.character(PhaseClass), levels = c("Active", "Inactive")),
    Group = factor(as.character(Group), levels = mmm_group_levels)
  )

readr::write_csv(
  combz_main_input,
  file.path(dirs$tables, "raw_movement_combz_input_overall_active_inactive.csv")
)

combz_cor_main <- combz_main_input %>%
  group_by(Sex, Endpoint, PhaseClass) %>%
  group_modify(~ safe_correlation_tests(.x)) %>%
  ungroup() %>%
  mutate(
    pearson_p_bh = p.adjust(pearson_p, method = "BH"),
    spearman_p_bh = p.adjust(spearman_p, method = "BH")
  )

readr::write_csv(
  combz_cor_main,
  file.path(dirs$stats, "raw_movement_combz_correlations_by_sex_phase.csv")
)

combz_main_plot_stats <- combz_cor_main %>%
  mutate(
    Endpoint = factor(as.character(Endpoint), levels = c("Active", "Inactive")),
    Sex = factor(as.character(Sex), levels = levels(combz_main_input$Sex)),
    stat_label = paste0(
      "r = ", if_else(is.na(pearson_r), "NA", sprintf("%.2f", pearson_r)), "\n",
      "p ", format_p_inline(pearson_p), "\n",
      "n = ", n_animals
    )
  )

p_combz_main <- ggplot(combz_main_input, aes(mean_movement, CombZ)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.38, alpha = 0.10, colour = "grey25", fill = "grey70") +
  geom_point(aes(colour = Group, fill = Group), shape = 21, size = 1.65, stroke = 0.25, alpha = 0.9) +
  geom_text(
    data = combz_main_plot_stats,
    aes(x = -Inf, y = Inf, label = stat_label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.15,
    size = 1.65,
    lineheight = 0.9,
    colour = "grey20"
  ) +
  facet_wrap(vars(Endpoint, Sex), nrow = 2, scales = "free") +
  labs(
    title = "Broad raw movement is descriptively associated with stress burden",
    subtitle = "Pearson correlations by sex and phase; CON/RES/SUS are shown for interpretation only",
    x = "Mean raw movement per animal",
    y = "CombZ"
  ) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  make_publication_theme(base_size = 6) +
  theme(legend.box.spacing = unit(0.5, "mm"), aspect.ratio = 1)

save_plot(
  p_combz_main,
  file.path(dirs$figure_publication, "raw_movement_active_inactive_vs_combz"),
  width = 183,
  height = 125
)

combz_cc_input <- movement_endpoints_combz %>%
  filter(
    ScopeType == "cage_change_by_phase",
    is.finite(mean_movement),
    is.finite(CombZ)
  ) %>%
  mutate(
    Sex = factor(as.character(Sex)),
    PhaseClass = factor(as.character(PhaseClass), levels = c("Active", "Inactive")),
    CageChange = factor(as.character(CageChange), levels = unique(as.character(CageChange[order(CageChangeIndex)]))),
    Group = factor(as.character(Group), levels = mmm_group_levels)
  )

combz_cor_cc <- combz_cc_input %>%
  group_by(Sex, CageChange, CageChangeIndex, PhaseClass, Endpoint) %>%
  group_modify(~ safe_correlation_tests(.x)) %>%
  ungroup() %>%
  mutate(
    pearson_p_bh = p.adjust(pearson_p, method = "BH"),
    spearman_p_bh = p.adjust(spearman_p, method = "BH")
  )

readr::write_csv(
  combz_cor_cc,
  file.path(dirs$stats, "raw_movement_combz_correlations_cagechange_phase.csv")
)

combz_cc_plot_stats <- combz_cor_cc %>%
  mutate(
    Sex = factor(as.character(Sex), levels = levels(combz_cc_input$Sex)),
    CageChange = factor(as.character(CageChange), levels = levels(combz_cc_input$CageChange)),
    PhaseClass = factor(as.character(PhaseClass), levels = c("Active", "Inactive")),
    stat_label = paste0(
      "r = ", if_else(is.na(pearson_r), "NA", sprintf("%.2f", pearson_r)), "\n",
      "p ", format_p_inline(pearson_p), "\n",
      "n = ", n_animals
    )
  )

p_combz_cc <- ggplot(combz_cc_input, aes(mean_movement, CombZ)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.32, alpha = 0.10, colour = "grey25", fill = "grey70") +
  geom_point(aes(colour = Group, fill = Group), shape = 21, size = 1.35, stroke = 0.22, alpha = 0.85) +
  geom_text(
    data = combz_cc_plot_stats,
    aes(x = -Inf, y = Inf, label = stat_label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.15,
    size = 1.35,
    lineheight = 0.85,
    colour = "grey20"
  ) +
  facet_grid(Sex + PhaseClass ~ CageChange, scales = "free") +
  labs(
    title = "Cage-change phase raw movement associations with CombZ",
    subtitle = "Supplementary descriptive Pearson correlations; group is shown only as point colour",
    x = "Mean raw movement per animal",
    y = "CombZ"
  ) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  make_publication_theme(base_size = 5.5) +
  theme(legend.box.spacing = unit(0.5, "mm"), aspect.ratio = 1)

save_plot(
  p_combz_cc,
  file.path(dirs$figure_supplementary, "raw_movement_cagechange_phase_vs_combz"),
  width = 220,
  height = 150
)

inactive_post <- phase_filter_qc %>%
  filter(PhaseClass == "Inactive") %>%
  summarise(total = sum(n_rows_post_filter, na.rm = TRUE)) %>%
  pull(total)

if (length(inactive_post) == 0 || !is.finite(inactive_post) || inactive_post == 0) {
  warning(
    "No Inactive rows remain after applying min_bins_per_animal = ", min_bins_per_animal,
    " in overall_by_phase endpoints. See tables/raw_movement_phase_filter_qc.csv",
    call. = FALSE
  )
}

group_summary <- movement_endpoints %>%
  group_by(ScopeType, Endpoint, Sex, Group, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    se_movement = safe_se(mean_movement),
    mean_movement = safe_mean(mean_movement),
    ci_half_width = if_else(
      n_animals > 1L,
      stats::qt(0.975, df = n_animals - 1L) * se_movement,
      NA_real_
    ),
    ci95_low = mean_movement - ci_half_width,
    ci95_high = mean_movement + ci_half_width,
    .groups = "drop"
  ) %>%
  select(-ci_half_width)
readr::write_csv(group_summary, file.path(dirs$tables, "raw_movement_group_summary.csv"))

# ------------------------------------------------
# CORRECTED STATS
# ------------------------------------------------

pairwise_stats <- movement_endpoints %>%
  group_by(ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  group_modify(~ pairwise_wilcox_stats(.x)) %>%
  ungroup() %>%
  group_by(ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  mutate(
    p_holm_panel = p.adjust(p_raw, method = "holm"),
    p_bh_panel = p.adjust(p_raw, method = "BH")
  ) %>%
  ungroup() %>%
  mutate(
    contrast = factor(contrast, levels = contrast_levels),
    stars_panel = sig_stars(p_holm_panel)
  )

if (isTRUE(export_global_family_corrections)) {
  pairwise_stats <- pairwise_stats %>%
    group_by(ScopeType, Sex) %>%
    mutate(
      p_holm_family_scope_sex = p.adjust(p_raw, method = "holm"),
      p_bh_family_scope_sex = p.adjust(p_raw, method = "BH")
    ) %>%
    ungroup() %>%
    mutate(stars_family = sig_stars(p_holm_family_scope_sex))
}

readr::write_csv(pairwise_stats, file.path(dirs$stats, "raw_movement_pairwise_wilcox_stats_corrected.csv"))

panel_label_stats <- pairwise_stats %>%
  transmute(
    ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass,
    contrast, p_label_panel_holm = p_holm_panel, stars_panel,
    p_raw, p_bh_panel
  )
readr::write_csv(panel_label_stats, file.path(dirs$stats, "raw_movement_pairwise_panel_label_values.csv"))

if (isTRUE(export_global_family_corrections)) {
  global_correction_stats <- pairwise_stats %>%
    transmute(
      ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass,
      contrast, p_raw,
      p_holm_family_scope_sex, p_bh_family_scope_sex,
      stars_family
    )
  readr::write_csv(global_correction_stats, file.path(dirs$stats, "raw_movement_pairwise_global_corrections_supplement.csv"))
}

lm_stats <- movement_endpoints %>%
  group_by(ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  group_modify(~ {
    dd <- .x %>%
      filter(is.finite(mean_movement), !is.na(Group)) %>%
      mutate(Group = droplevels(factor(Group, levels = mmm_group_levels)))

    if (nrow(dd) < 4 || nlevels(dd$Group) < 2) {
      return(tibble(test = "one_way_lm_log1p_movement", p_raw = NA_real_))
    }

    out <- tryCatch({
      fit <- lm(log1p(mean_movement) ~ Group, data = dd)
      a <- anova(fit)
      tibble(test = "one_way_lm_log1p_movement", p_raw = a$`Pr(>F)`[1])
    }, error = function(e) {
      tibble(test = "one_way_lm_log1p_movement", p_raw = NA_real_)
    })

    out
  }) %>%
  ungroup() %>%
  group_by(ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  mutate(p_holm_panel = p.adjust(p_raw, method = "holm")) %>%
  ungroup()

if (isTRUE(export_global_family_corrections)) {
  lm_stats <- lm_stats %>%
    group_by(ScopeType, Sex) %>%
    mutate(p_holm_family_scope_sex = p.adjust(p_raw, method = "holm")) %>%
    ungroup()
}

readr::write_csv(lm_stats, file.path(dirs$stats, "raw_movement_one_way_lm_stats_corrected.csv"))

# Optional repeated-measures model across all cage-change x phase epochs, sex-specific.
if (requireNamespace("lmerTest", quietly = TRUE)) {
  repeated_lmm_stats <- cc_phase_endpoint %>%
    mutate(
      Group = factor(Group, levels = mmm_group_levels),
      PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive")),
      CageChangeIndexF = factor(CageChangeIndex)
    ) %>%
    group_by(Sex) %>%
    group_modify(~ {
      dd <- .x %>%
        filter(is.finite(mean_movement), !is.na(Group), !is.na(PhaseClass), !is.na(CageChangeIndexF)) %>%
        mutate(
          Group = droplevels(Group),
          PhaseClass = droplevels(PhaseClass),
          CageChangeIndexF = droplevels(CageChangeIndexF)
        )

      if (
        n_distinct(dd$AnimalNum) < 4 ||
          nlevels(dd$Group) < 2 ||
          nlevels(dd$PhaseClass) < 2 ||
          nlevels(dd$CageChangeIndexF) < 2
      ) {
        return(tibble(term = NA_character_, p.value = NA_real_))
      }

      out <- tryCatch({
        fit <- lmerTest::lmer(log1p(mean_movement) ~ Group * PhaseClass * CageChangeIndexF + (1 | AnimalNum), data = dd)
        as.data.frame(anova(fit)) %>%
          rownames_to_column("term") %>%
          as_tibble() %>%
          select(term, everything())
      }, error = function(e) {
        tibble(term = NA_character_, p.value = NA_real_)
      })

      out
    }) %>%
    ungroup()
  readr::write_csv(repeated_lmm_stats, file.path(dirs$stats, "raw_movement_repeated_lmm_cagechange_phase_by_sex.csv"))
}

hits_panel <- pairwise_stats %>%
  filter(!is.na(p_holm_panel), p_holm_panel < 0.10) %>%
  arrange(p_holm_panel)
readr::write_csv(hits_panel, file.path(dirs$stats, "raw_movement_hits_panel_holm_padj_lt_0p10.csv"))

if (isTRUE(export_global_family_corrections)) {
  hits_family <- pairwise_stats %>%
    filter(!is.na(p_holm_family_scope_sex), p_holm_family_scope_sex < 0.10) %>%
    arrange(p_holm_family_scope_sex)
  readr::write_csv(hits_family, file.path(dirs$stats, "raw_movement_hits_family_holm_padj_lt_0p10.csv"))
}

# ------------------------------------------------
# FIGURES
# ------------------------------------------------

# 1) Overall / Active / Inactive endpoint figure.
overall_phase_plot <- movement_endpoints %>%
  filter(ScopeType %in% c("overall_all_phases", "overall_by_phase")) %>%
  mutate(Endpoint = factor(Endpoint, levels = c("Overall", "Active", "Inactive")))

overall_panel_counts <- overall_phase_plot %>%
  count(Sex, Endpoint, name = "n_animals")
readr::write_csv(overall_panel_counts, file.path(dirs$tables, "raw_movement_overall_active_inactive_panel_counts.csv"))

overall_y_range <- overall_phase_plot %>%
  group_by(Sex, Endpoint) %>%
  summarise(
    y_max = max(mean_movement, na.rm = TRUE),
    y_min = min(mean_movement, na.rm = TRUE),
    .groups = "drop"
  )

overall_sig_bars <- pairwise_stats %>%
  filter(
    ScopeType %in% c("overall_all_phases", "overall_by_phase"),
    !is.na(p_holm_panel),
    p_holm_panel < 0.05
  ) %>%
  mutate(
    Endpoint = factor(as.character(Endpoint), levels = c("Overall", "Active", "Inactive")),
    Sex_key = as.character(Sex)
  ) %>%
  left_join(
    overall_y_range %>% mutate(
      Endpoint = factor(as.character(Endpoint), levels = c("Overall", "Active", "Inactive")),
      Sex_key = as.character(Sex)
    ) %>% select(Sex_key, Endpoint, y_max, y_min),
    by = c("Sex_key", "Endpoint")
  ) %>%
  select(-Sex_key) %>%
  distinct(Sex, Endpoint, contrast, .keep_all = TRUE) %>%
  mutate(
    contrast = factor(contrast, levels = contrast_levels),
    xmin = case_when(
      contrast == "RES - CON" ~ 1,
      contrast == "SUS - CON" ~ 1,
      contrast == "SUS - RES" ~ 2
    ),
    xmax = case_when(
      contrast == "RES - CON" ~ 2,
      contrast == "SUS - CON" ~ 3,
      contrast == "SUS - RES" ~ 3
    ),
    annotation = paste0("p=", format_p(p_holm_panel), sig_stars(p_holm_panel))
  ) %>%
  arrange(Sex, Endpoint, xmin, xmax) %>%
  group_by(Sex, Endpoint) %>%
  mutate(
    bar_idx = row_number(),
    y_range = pmax(y_max - y_min, abs(y_max) * 0.1, 0.01),
    y_position = y_max + bar_idx * 0.22 * y_range
  ) %>%
  ungroup()

overall_blank <- if (nrow(overall_sig_bars) > 0) {
  overall_sig_bars %>%
    group_by(Sex, Endpoint) %>%
    summarise(y_blank = max(y_position, na.rm = TRUE) * 1.10, .groups = "drop") %>%
    mutate(Group = factor("CON", levels = mmm_group_levels))
} else {
  overall_phase_plot %>%
    group_by(Sex, Endpoint) %>%
    summarise(y_blank = max(mean_movement, na.rm = TRUE) * 1.05, .groups = "drop") %>%
    mutate(Group = factor("CON", levels = mmm_group_levels))
}

p_overall <- ggplot(overall_phase_plot, aes(Group, mean_movement, colour = Group, fill = Group)) +
  #geom_boxplot(width = 0.24, outlier.shape = NA, alpha = 0.55, linewidth = 0.25) +
  geom_jitter(width = 0.06, size = 1.5, alpha = 0.6, shape = 16) +
  geom_blank(data = overall_blank, aes(y = y_blank), inherit.aes = FALSE) +
  geom_signif(
    data = overall_sig_bars,
    aes(xmin = xmin, xmax = xmax, y_position = y_position, annotations = annotation),
    manual = TRUE,
    inherit.aes = FALSE,
    tip_length = 0.02,
    textsize = 2.0,
    vjust = 0.4
  ) +
  facet_grid(Sex ~ Endpoint, scales = "free_y", drop = FALSE) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Overall raw movement endpoints",
    subtitle = "Holm-adjusted within each prespecified\nthree-contrast panel",
    x = NULL,
    y = "Mean raw movement per animal",
    caption = paste0("Input: ", basename(input_file), " | bin level: ", bin_level)
  ) +
  make_theme(base_size = 7) +
  theme(legend.position = "none")

save_plot(p_overall, file.path(dirs$figure_publication, "Fig18c_overall_active_inactive_mean_movement_corrected_stats"), width = 60, height = 60)

# 2) Cage-change x phase plot: show only panel-Holm p < 0.05 rows to avoid label clutter.
cc_plot <- group_summary %>% filter(ScopeType == "cage_change_by_phase")

cc_labels <- pairwise_stats %>%
  filter(ScopeType == "cage_change_by_phase", !is.na(p_holm_panel), p_holm_panel < 0.05) %>%
  mutate(label_piece = paste0(contrast, " ", stars_panel)) %>%
  group_by(Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(stats_label = paste(label_piece, collapse = "\n"), .groups = "drop")

cc_y <- cc_plot %>%
  group_by(Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(y = max(ci95_high, mean_movement, na.rm = TRUE), .groups = "drop") %>%
  mutate(y = y + 0.10 * abs(y))

cc_annot <- cc_labels %>% left_join(cc_y, by = c("Sex", "CageChange", "CageChangeIndex", "PhaseClass"))

p_cc <- ggplot(cc_plot, aes(CageChange, mean_movement, colour = Group, group = Group)) +
  geom_line(linewidth = 0.45, position = position_dodge(width = 0.20)) +
  geom_point(size = 1.4, position = position_dodge(width = 0.20)) +
  geom_errorbar(aes(ymin = ci95_low, ymax = ci95_high), width = 0.12, linewidth = 0.25, position = position_dodge(width = 0.20)) +
  geom_text(data = cc_annot, aes(x = CageChange, y = y, label = stats_label), inherit.aes = FALSE, size = 1.8, colour = "black", lineheight = 0.9, vjust = 0) +
  facet_grid(Sex ~ PhaseClass, scales = "free_y", drop = FALSE) +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Raw movement by cage change and phase",
    subtitle = "Displayed comparisons are Holm-adjusted within each prespecified three-contrast panel; the wider scan is secondary/descriptive",
    x = "Cage change",
    y = "Mean raw movement per animal"
  ) +
  make_theme(base_size = 7)

save_plot(p_cc, file.path(dirs$figure_publication, "Fig18c_cage_change_phase_mean_movement_corrected_stats"), width = 178, height = 102)

# 3) Heatmap of panel-adjusted p-values.
heatmap_tbl <- pairwise_stats %>%
  filter(ScopeType == "cage_change_by_phase") %>%
  mutate(
    p_plot = -log10(pmax(p_holm_panel, 1e-6)),
    ContrastPhase = paste(contrast, PhaseClass, sep = " | ")
  )

p_heat <- ggplot(heatmap_tbl, aes(CageChange, ContrastPhase, fill = p_plot)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  geom_text(aes(label = stars_panel), size = 2.4, colour = "black") +
  facet_wrap(~ Sex, nrow = 1) +
  scale_fill_gradient(low = "white", high = "grey20", name = "-log10 panel-Holm p") +
  labs(
    title = "Movement group-difference scan",
    subtitle = "Cage-change x phase pairwise tests; p-values are Holm-adjusted within each displayed panel",
    x = "Cage change",
    y = NULL
  ) +
  make_theme(base_size = 7)

save_plot(p_heat, file.path(dirs$figure_publication, "Fig18c_cage_change_phase_pairwise_pvalue_heatmap_corrected"), width = 165, height = 96)

message("Corrected broad raw movement phase statistics complete: ", output_dir)
message("Selected input: ", input_file)
message("Selected bin level: ", bin_level)
message("Audit rows with panel-Holm p < 0.10 (not publication trend annotations): ", nrow(hits_panel))
if (isTRUE(export_global_family_corrections)) {
  message("Family-Holm hits p < 0.10: ", nrow(hits_family))
} else {
  message("Global family-wise correction: disabled (panel-wise Holm only).")
}
