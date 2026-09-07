# ================================================================
# SECONDARY A - full CC1 Active-phase longitudinal GAMM
# MMMSociability
# ================================================================
# Estimand:
#   Across ALL Active phases occurring during the first cage-change period, do
#   CON / RES / SUS differ in Movement level, within-night trajectory shape,
#   and night-to-night adaptation?
#
# This is the clean canonical replacement for the question the archived
# _firstChangeActive GAMM approximately attempted. That analysis took all four
# CC1 Active nights, reset each night to 0-12 h, OVERLAID them, and put no
# night term in the model. Here every night keeps its identity: ActiveNight is
# an explicit factor, it interacts with Group, and the trajectory figure is
# faceted by night so the overlay error is not reproducible.
#
# SECONDARY. Its families are separate from the PRIMARY first-night six-test
# family in Analysis/20_first_night_gamm.R:
#   family 1  per-night pairwise contrasts: 2 Sex x N nights x 3 contrasts, BH
#   family 2  formal Group x ActiveNight interaction tests, BH within
# CC1 ActiveNight 1 IS the primary first-night data; this is not independent
# replication (see Testing/tests/test_acute_active_window_parity.R).
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(ggplot2); library(mgcv)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("acute_active_window_helpers.R")
source_mmm_helper("gamm_group_inference_helpers.R")
source_mmm_helper("stratified_gamm_driver.R")

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"
bin_size_sec <- 600L
target_cc <- "CC1"
group_colors <- c(CON = "#3d3b6e", RES = "#C6C3BB", SUS = "#e63947")

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "21", "cc1_active_longitudinal_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)

cat("SECONDARY A: full CC1 Active-phase GAMM |", bin_level, "\n")

# ------------------------------------------------ 28.1 select CC1 Active blocks
raw <- read_csv(
  input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE
)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

nights <- mmm_select_cc_active_nights(raw, bin_size_sec = bin_size_sec, cage_change = target_cc)

qc <- mmm_active_window_qc(nights, block_cols = "ActiveNight",
                           start_col = "block_window_start", end_col = "block_window_end")
write_csv(qc, file.path(dirs$tables, "cc1_active_block_qc.csv"))

n_nights <- n_distinct(nights$ActiveNight)
cat("  rows: ", nrow(nights), " | animals: ", n_distinct(nights$AnimalNum),
    " | Active nights: ", n_nights, "\n", sep = "")
cat("  animal x night windows: ", nrow(qc), " | complete: ", sum(qc$window_complete),
    " | mean coverage: ", round(mean(qc$coverage), 4), "\n", sep = "")

# The historical four-night structure must reappear from canonical data.
per_animal_nights <- qc %>% count(AnimalNum, name = "n_nights")
cat("  nights per animal: ", paste(sprintf("%d nights x %d animals",
    as.integer(names(table(per_animal_nights$n_nights))), as.integer(table(per_animal_nights$n_nights))),
    collapse = "; "), "\n", sep = "")

# ------------------------------------------------ 28.2 model
res <- mmm_run_stratified_gamm(
  nights,
  stratum_col = "ActiveNight",
  time_col = "TimeWithinActiveHours",
  ar_block_cols = "ActiveNight",   # AR1 restarts at animal, night, and gaps
  window_hours = 12
)

# ------------------------------------------------ 28.3 questions + families
# CC1-Q3 family: per-night pairwise contrasts.
q3 <- res$contrasts %>%
  filter(.data$variant == "primary") %>%
  mmm_bh_family(paste0("SECONDARY_CC1__pernight_contrasts__2Sex_x_", n_nights, "Nights_x_3Contrasts"))

# CC1-Q1: Group differences averaged across the whole CC1 Active period.
q1 <- res$stratum_avg %>%
  filter(.data$variant == "primary") %>%
  mmm_bh_family("SECONDARY_CC1__period_averaged_contrasts__2Sex_x_3Contrasts")

# CC1-Q2 family: the formal Group x ActiveNight tests, kept separate.
q2 <- mmm_interaction_tests(res$parametric %>% filter(.data$variant == "primary"), "ActiveNight") %>%
  group_by(.data$term) %>%
  group_modify(~ mmm_bh_family(.x, paste0("SECONDARY_CC1__omnibus__", .y$term))) %>%
  ungroup()

# CC1-Q4: within-night shape differences live in the by-smooth table.
q4 <- res$smooth %>%
  filter(.data$variant == "primary", grepl("Cross", .data$smooth, fixed = TRUE))

write_csv(q3, file.path(dirs$tables, "cc1_group_by_night_contrasts.csv"))
write_csv(q1, file.path(dirs$tables, "cc1_period_averaged_contrasts.csv"))
write_csv(q2, file.path(dirs$tables, "cc1_group_x_night_tests.csv"))
write_csv(q4, file.path(dirs$tables, "cc1_trajectory_shape_tests.csv"))
write_csv(res$trajectory, file.path(dirs$tables, "cc1_trajectory_predictions.csv"))
write_csv(res$pointwise, file.path(dirs$tables, "cc1_pairwise_trajectory_differences.csv"))
write_csv(res$spec, file.path(dirs$tables, "cc1_model_specification.csv"))
write_csv(res$parametric, file.path(dirs$tables, "cc1_parametric_terms.csv"))
write_csv(res$smooth, file.path(dirs$tables, "cc1_smooth_terms.csv"))
write_csv(res$ar1, file.path(dirs$audit, "cc1_ar1_sequence_proof.csv"))

# ------------------------------------------------ 30/31 sensitivity
batch_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "no_batch"),
  by_cols = c("Sex", "stratum", "contrast"), label = "batch_adjusted_vs_unadjusted")
scale_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "raw_scale"),
  by_cols = c("Sex", "stratum", "contrast"), label = "log1p_vs_raw_gaussian")
write_csv(batch_sens, file.path(dirs$tables, "cc1_batch_sensitivity.csv"))
write_csv(scale_sens, file.path(dirs$tables, "cc1_response_scale_sensitivity.csv"))

# ------------------------------------------------ 28.4 figures
traj <- res$trajectory %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = names(group_colors)),
         night_lab = paste("Active night", .data$stratum))

p1 <- ggplot(traj, aes(TimeAxis, fit, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_grid(Sex ~ night_lab) +
  scale_colour_manual(values = group_colors) + scale_fill_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(0, 12, 3), limits = c(0, 12)) +
  labs(title = paste0("Full ", target_cc, " Active period: trajectories by Active night"),
       subtitle = "Each night on its own 0-12 h clock axis. Nights are modelled separately, never overlaid.",
       x = "Hours into that Active night", y = "Predicted log1p(Movement)") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "cc1_active_trajectories_by_night.svg"), p1,
       width = 3 + 2.2 * n_nights, height = 5)

p2 <- q3 %>% mutate(night_lab = paste("Night", .data$stratum)) %>%
  ggplot(aes(estimate, contrast, colour = p_bh < 0.05)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.15) +
  geom_point(size = 2) +
  facet_grid(Sex ~ night_lab) +
  scale_colour_manual(values = c(`FALSE` = "grey45", `TRUE` = "#e63947"), guide = "none") +
  labs(title = paste0(target_cc, ": night-averaged group contrasts"),
       subtitle = paste0("Secondary family: 2 Sex x ", n_nights,
                         " nights x 3 contrasts, BH within. Red = BH < 0.05."),
       x = "Mean predicted difference over that night, log1p(Movement)", y = NULL) +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "cc1_group_contrasts_by_night.svg"), p2,
       width = 3 + 2.2 * n_nights, height = 5)

# Adaptation summary: the contrast trajectory ACROSS nights, which is what the
# Group x ActiveNight interaction formally tests.
p3 <- q3 %>% mutate(night = as.integer(.data$stratum)) %>%
  ggplot(aes(night, estimate, colour = contrast, group = contrast)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = contrast), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = sort(unique(as.integer(q3$stratum)))) +
  labs(title = paste0(target_cc, ": do group differences change across Active nights?"),
       subtitle = "Formal evidence is the Group x ActiveNight test in cc1_group_x_night_tests.csv, not the per-night pattern",
       x = "Active night within CC1", y = "Night-averaged group contrast") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "cc1_adaptation_summary.svg"), p3, width = 9, height = 4)

write_output_manifest(
  output_dir,
  script_name = "21_cc1_active_longitudinal_gamm.R",
  analysis_name = "SECONDARY A: full CC1 Active-phase longitudinal GAMM",
  bin_level = bin_level,
  key_parameters = list(cage_change = target_cc, n_active_nights = n_nights,
                        k = MMM_GAMM_K, method = MMM_GAMM_METHOD,
                        secondary_family = paste0("2 Sex x ", n_nights, " nights x 3 contrasts (BH)")),
  primary_tables = c("tables/cc1_group_by_night_contrasts.csv",
                     "tables/cc1_group_x_night_tests.csv"),
  primary_figures = c("figures/cc1_active_trajectories_by_night.svg",
                      "figures/cc1_adaptation_summary.svg")
)

cat("\nCC1-Q2 formal tests (primary, Batch-adjusted):\n")
print(as.data.frame(q2 %>% select(Sex, term, df, F_stat, p_raw, p_bh)), row.names = FALSE, digits = 4)
cat("\nCC1-Q3 per-night contrasts with BH < 0.05:\n")
sig <- q3 %>% filter(.data$p_bh < 0.05) %>% select(Sex, stratum, contrast, estimate, ci_low, ci_high, p_raw, p_bh)
if (nrow(sig) == 0) cat("  none\n") else print(as.data.frame(sig), row.names = FALSE, digits = 4)
cat("\nDone ->", output_dir, "\n")
