# ================================================================
# Corrected Broad Raw Movement Phase Statistics
# MMMSociability
# ================================================================
# Goal:
#   Corrected statistics and plotting for raw mean movement endpoints:
#   1) overall movement,
#   2) overall active/inactive movement,
#   3) cage-change x active/inactive movement.
#
# Important correction relative to 18b:
#   - Plotted p-values are Holm-adjusted within the displayed panel only
#     across the three planned pairwise contrasts: RES-CON, SUS-CON, SUS-RES.
#   - Wider family-wise corrections are still exported separately, but are
#     not used as the panel label because that makes the figure misleadingly
#     conservative and hard to read.
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
})

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
repo_root <- "C:/Users/topohl/Documents/GitHub/MMMSociability"
bin_level_priority <- c("10min_based", "5min_based", "30min_based", "1min_based")
input_candidates <- file.path(base_dir, "analysis_ready/03_derived_metrics", bin_level_priority, "all_behavior_metrics.csv")
analysis_name <- "18c_raw_movement_broad_phase_stats_corrected"
min_bins_per_animal <- 2
show_raw_p_in_labels <- FALSE

helper_candidates <- c(
  file.path(repo_root, "Functions", "behavioral_dynamics_helpers.R"),
  file.path("Functions", "behavioral_dynamics_helpers.R"),
  file.path("..", "Functions", "behavioral_dynamics_helpers.R")
)
helper_path <- helper_candidates[file.exists(helper_candidates)][1]
if (!is.na(helper_path)) source(helper_path)

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

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "†",
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

make_theme <- function(base_size = 7) {
  theme_classic(base_size = base_size, base_family = "Arial") +
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
  ggsave(paste0(filename_base, ".pdf"), plot, width = width, height = height, units = units)
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
    Group = factor(Group, levels = mmm_group_levels),
    Sex = factor(Sex),
    PhaseClass = factor(PhaseClass, levels = c("Active", "Inactive"))
  ) %>%
  filter(!is.na(AnimalNum), !is.na(Group), !is.na(Sex), !is.na(PhaseClass), is.finite(Movement))

readr::write_csv(
  tibble(Field = c("script", "input_file", "bin_level", "movement_column", "output_dir"),
         Value = c("Analysis/18c_raw_movement_broad_phase_stats_corrected.R", input_file, bin_level, movement_col, output_dir)),
  file.path(dirs$tables, "input_source_manifest.csv")
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

movement_endpoints <- bind_rows(overall_endpoint, phase_endpoint, cc_phase_endpoint) %>%
  filter(n_bins >= min_bins_per_animal) %>%
  mutate(
    Endpoint = as.character(Endpoint),
    PhaseClass = as.character(PhaseClass),
    CageChange = as.character(CageChange)
  )
readr::write_csv(movement_endpoints, file.path(dirs$tables, "raw_movement_animal_level_endpoints.csv"))

group_summary <- movement_endpoints %>%
  group_by(ScopeType, Endpoint, Sex, Group, CageChange, CageChangeIndex, PhaseClass) %>%
  summarise(
    n_animals = n_distinct(AnimalNum),
    mean_movement = safe_mean(mean_movement),
    se_movement = safe_se(mean_movement),
    ci95_low = mean_movement - 1.96 * se_movement,
    ci95_high = mean_movement + 1.96 * se_movement,
    .groups = "drop"
  )
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
  group_by(ScopeType, Sex) %>%
  mutate(
    p_holm_family_scope_sex = p.adjust(p_raw, method = "holm"),
    p_bh_family_scope_sex = p.adjust(p_raw, method = "BH")
  ) %>%
  ungroup() %>%
  mutate(
    stars_panel = sig_stars(p_holm_panel),
    stars_family = sig_stars(p_holm_family_scope_sex)
  )
readr::write_csv(pairwise_stats, file.path(dirs$stats, "raw_movement_pairwise_wilcox_stats_corrected.csv"))

lm_stats <- movement_endpoints %>%
  group_by(ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  group_modify(~ {
    dd <- .x %>% filter(is.finite(mean_movement), !is.na(Group))
    if (n_distinct(dd$Group) < 2 || nrow(dd) < 4) return(tibble(test = "one_way_lm_log1p_movement", p_raw = NA_real_))
    fit <- lm(log1p(mean_movement) ~ Group, data = dd)
    a <- anova(fit)
    tibble(test = "one_way_lm_log1p_movement", p_raw = a$`Pr(>F)`[1])
  }) %>%
  ungroup() %>%
  group_by(ScopeType, Endpoint, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
  mutate(p_holm_panel = p.adjust(p_raw, method = "holm")) %>%
  ungroup() %>%
  group_by(ScopeType, Sex) %>%
  mutate(p_holm_family_scope_sex = p.adjust(p_raw, method = "holm")) %>%
  ungroup()
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
      dd <- .x %>% filter(is.finite(mean_movement), !is.na(Group), !is.na(PhaseClass), !is.na(CageChangeIndexF))
      if (n_distinct(dd$Group) < 2 || n_distinct(dd$AnimalNum) < 4) return(tibble(term = NA_character_, p.value = NA_real_))
      fit <- lmerTest::lmer(log1p(mean_movement) ~ Group * PhaseClass * CageChangeIndexF + (1 | AnimalNum), data = dd)
      as.data.frame(anova(fit)) %>%
        rownames_to_column("term") %>%
        as_tibble() %>%
        select(term, everything())
    }) %>%
    ungroup()
  readr::write_csv(repeated_lmm_stats, file.path(dirs$stats, "raw_movement_repeated_lmm_cagechange_phase_by_sex.csv"))
}

hits_panel <- pairwise_stats %>%
  filter(!is.na(p_holm_panel), p_holm_panel < 0.10) %>%
  arrange(p_holm_panel)
readr::write_csv(hits_panel, file.path(dirs$stats, "raw_movement_hits_panel_holm_padj_lt_0p10.csv"))

hits_family <- pairwise_stats %>%
  filter(!is.na(p_holm_family_scope_sex), p_holm_family_scope_sex < 0.10) %>%
  arrange(p_holm_family_scope_sex)
readr::write_csv(hits_family, file.path(dirs$stats, "raw_movement_hits_family_holm_padj_lt_0p10.csv"))

# ------------------------------------------------
# FIGURES
# ------------------------------------------------

# 1) Overall / Active / Inactive endpoint figure.
overall_phase_plot <- movement_endpoints %>%
  filter(ScopeType %in% c("overall_all_phases", "overall_by_phase")) %>%
  mutate(Endpoint = factor(Endpoint, levels = c("Overall", "Active", "Inactive")))

overall_labels <- pairwise_stats %>%
  filter(ScopeType %in% c("overall_all_phases", "overall_by_phase")) %>%
  mutate(
    Endpoint = factor(Endpoint, levels = c("Overall", "Active", "Inactive")),
    p_for_label = if (show_raw_p_in_labels) p_raw else p_holm_panel,
    label_piece = paste0(contrast, ": p", ifelse(show_raw_p_in_labels, "", "Holm"), "=", format_p(p_for_label))
  ) %>%
  group_by(Sex, Endpoint) %>%
  summarise(stats_label = paste(label_piece, collapse = "\n"), .groups = "drop")

overall_y <- overall_phase_plot %>%
  group_by(Sex, Endpoint) %>%
  summarise(y = max(mean_movement, na.rm = TRUE), .groups = "drop") %>%
  mutate(x = 0.72, y = y + 0.15 * abs(y))

overall_annot <- overall_labels %>% left_join(overall_y, by = c("Sex", "Endpoint"))

p_overall <- ggplot(overall_phase_plot, aes(Group, mean_movement, colour = Group, fill = Group)) +
  geom_boxplot(width = 0.24, outlier.shape = NA, alpha = 0.55, linewidth = 0.25) +
  geom_jitter(width = 0.06, size = 0.9, alpha = 0.75) +
  geom_text(data = overall_annot, aes(x = x, y = y, label = stats_label), inherit.aes = FALSE, hjust = 0, vjust = 1, size = 1.85, colour = "black", lineheight = 0.90) +
  facet_grid(Sex ~ Endpoint, scales = "free_y") +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  scale_fill_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Overall raw movement endpoints",
    subtitle = "Animal-level means; displayed p-values are Holm-adjusted within each panel across the three planned contrasts",
    x = NULL,
    y = "Mean raw movement per animal",
    caption = paste0("Input: ", basename(input_file), " | bin level: ", bin_level)
  ) +
  make_theme(base_size = 7) +
  theme(legend.position = "none")

save_plot(p_overall, file.path(dirs$figure_publication, "Fig18c_overall_active_inactive_mean_movement_corrected_stats"), width = 180, height = 108)

# 2) Cage-change x phase plot: show only panel-Holm hits to avoid label clutter.
cc_plot <- group_summary %>% filter(ScopeType == "cage_change_by_phase")

cc_labels <- pairwise_stats %>%
  filter(ScopeType == "cage_change_by_phase", stars_panel != "") %>%
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
  facet_grid(Sex ~ PhaseClass, scales = "free_y") +
  scale_colour_manual(values = mmm_group_colors, drop = FALSE) +
  labs(
    title = "Raw movement by cage change and phase",
    subtitle = "Labels show panel-Holm hits only: * <0.05, † <0.10. Family-wise p-values are exported separately.",
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
message("Panel-Holm hits p < 0.10: ", nrow(hits_panel))
message("Family-Holm hits p < 0.10: ", nrow(hits_family))
