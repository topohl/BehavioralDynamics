# ================================================================
# SECONDARY B - repeated acute response across CC1..CC4
# MMMSociability
# ================================================================
# Estimand:
#   Does the acute Movement response to regrouping change across successive
#   cage changes, and does that change differ among later CON / RES / SUS
#   phenotypes?
#
# This is the preferred "all cage changes together" GAMM. It does NOT stack
# every observation from every cage-change interval. It takes ONE comparable
# acute window per cage change: the first exact Active block after that cage
# change, 18:30 inclusive -> 06:30 exclusive, exactly 12 h, selected by the
# generalized clock-anchored selector in
# Functions/acute_active_window_helpers.R. For CC1 that selector is row-for-row
# identical to the validated canonical first-night selector
# (Testing/tests/test_acute_active_window_parity.R).
#
# SECONDARY. Separate families from the PRIMARY six-test first-night family:
#   family 1  acute-window pairwise contrasts: 2 Sex x 4 CC x 3 = 24 tests, BH
#   family 2  formal Group x CageChange interaction tests, BH within
# The CC1 acute window IS the primary first-night data; this is not an
# independent replication.
#
# CageChange is a FACTOR in the primary model so adaptation may be
# non-monotonic. A numeric trend is reported secondarily only.
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
group_colors <- c(CON = "#3d3b6e", RES = "#C6C3BB", SUS = "#e63947")

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "22", "repeated_cagechange_acute_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)

cat("SECONDARY B: repeated acute response CC1-CC4 |", bin_level, "\n")

# ------------------------------------------------ 29.1 one acute window per CC
raw <- read_csv(
  input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE
)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

cc_levels <- sort(unique(as.character(raw$CageChange)))
cat("  cage changes present:", paste(cc_levels, collapse = ", "), "\n")

acute <- purrr::map_dfr(cc_levels, function(cc) {
  mmm_select_acute_active_window(raw, bin_size_sec = bin_size_sec, cage_change = cc) %>%
    mutate(CageChangeWindow = cc)
})

qc <- mmm_active_window_qc(acute, block_cols = "CageChangeWindow") %>%
  rename(CageChange = "CageChangeWindow")
write_csv(qc, file.path(dirs$tables, "allcc_acute_window_qc.csv"))

cat("  rows: ", nrow(acute), " | animals: ", n_distinct(acute$AnimalNum), "\n", sep = "")
parity <- qc %>% group_by(.data$CageChange) %>%
  summarise(n_animals = n_distinct(.data$AnimalNum), n_windows = n(),
            expected_bins = first(.data$expected_bins),
            mean_coverage = mean(.data$coverage), min_coverage = min(.data$coverage),
            n_complete = sum(.data$window_complete), .groups = "drop")
write_csv(parity, file.path(dirs$tables, "allcc_cohort_window_parity.csv"))
print(as.data.frame(parity), row.names = FALSE, digits = 4)

# Every window must be the same physical duration and use the same clock logic.
stopifnot(n_distinct(acute$window_hours) == 1L, n_distinct(acute$expected_slots) == 1L)

# ------------------------------------------------ 29.2 model (CageChange = factor)
acute_model_dat <- acute %>% mutate(CageChange = factor(.data$CageChangeWindow, levels = cc_levels))

res <- mmm_run_stratified_gamm(
  acute_model_dat,
  stratum_col = "CageChange",
  time_col = "TimeHours",
  ar_block_cols = "CageChange",   # AR1 restarts at animal, cage change, and gaps
  window_hours = 12
)

# ------------------------------------------------ 29.3 questions + families
# ALLCC-Q4 family: 2 Sex x 4 CC x 3 contrasts = 24 planned tests.
q4 <- res$contrasts %>%
  filter(.data$variant == "primary") %>%
  mmm_bh_family("SECONDARY_ALLCC__acute_contrasts__2Sex_x_4CC_x_3Contrasts")
cat("\n  ALLCC-Q4 family size:", unique(q4$n_tests_in_family), "tests\n")

# ALLCC-Q1: Group differences averaged across all four acute windows.
q1 <- res$stratum_avg %>%
  filter(.data$variant == "primary") %>%
  mmm_bh_family("SECONDARY_ALLCC__window_averaged_contrasts__2Sex_x_3Contrasts")

# ALLCC-Q2 / Q3: formal omnibus tests, a separate family from the contrasts.
q23 <- mmm_interaction_tests(res$parametric %>% filter(.data$variant == "primary"), "CageChange") %>%
  group_by(.data$term) %>%
  group_modify(~ mmm_bh_family(.x, paste0("SECONDARY_ALLCC__omnibus__", .y$term))) %>%
  ungroup()

# ALLCC-Q5: trajectory-shape differences across Group x CageChange.
q5 <- res$smooth %>%
  filter(.data$variant == "primary", grepl("Cross", .data$smooth, fixed = TRUE))

write_csv(q4, file.path(dirs$tables, "allcc_group_by_cc_contrasts.csv"))
write_csv(q1, file.path(dirs$tables, "allcc_window_averaged_contrasts.csv"))
write_csv(q23, file.path(dirs$tables, "allcc_group_x_cc_tests.csv"))
write_csv(q5, file.path(dirs$tables, "allcc_trajectory_shape_tests.csv"))
write_csv(res$trajectory, file.path(dirs$tables, "allcc_trajectory_predictions.csv"))
write_csv(res$pointwise, file.path(dirs$tables, "allcc_pairwise_trajectory_differences.csv"))
write_csv(res$spec, file.path(dirs$tables, "allcc_model_specification.csv"))
write_csv(res$parametric, file.path(dirs$tables, "allcc_parametric_terms.csv"))
write_csv(res$smooth, file.path(dirs$tables, "allcc_smooth_terms.csv"))
write_csv(res$ar1, file.path(dirs$audit, "allcc_ar1_sequence_proof.csv"))

# ------------------------------------------------ 29.4 secondary numeric trend
# Reported only as a supplement; the factor model above stays primary so that
# non-monotonic adaptation remains representable.
trend_rows <- purrr::map_dfr(sort(unique(as.character(acute_model_dat$Sex))), function(sx) {
  d <- acute_model_dat %>%
    filter(.data$Sex == sx, !is.na(.data$Movement)) %>%
    mutate(y_log = log1p(.data$Movement),
           Group = factor(.data$Group, levels = c("CON", "RES", "SUS")),
           Batch = factor(.data$Batch), AnimalNum = factor(.data$AnimalNum),
           CCnum = as.numeric(sub("^\\D*", "", as.character(.data$CageChange))),
           TimeAxis = .data$TimeHours,
           Cross = mmm_ordered_cross(.data$Group, factor(.data$CageChange))) %>%
    mmm_order_for_ar1(block_cols = "CageChange") %>% droplevels()
  d$.ar_start <- mmm_build_ar_start(d, block_cols = "CageChange")
  form <- stats::as.formula(paste0(
    "y_log ~ Batch + Group * CCnum + s(TimeAxis, k = ", MMM_GAMM_K, ", bs = 'tp')",
    " + s(TimeAxis, by = Cross, k = ", MMM_GAMM_K, ", bs = 'tp') + s(AnimalNum, bs = 're')"))
  fit <- mmm_fit_gamm_ar1(form, d)
  ct <- summary(fit$model)$p.table
  as.data.frame(ct) %>% tibble::rownames_to_column("term") %>% as_tibble() %>%
    filter(grepl("CCnum", .data$term)) %>%
    mutate(Sex = sx, rho = fit$rho, ar1_applied = fit$ar1_applied,
           model = "secondary_linear_CC_trend", .before = 1)
})
write_csv(trend_rows, file.path(dirs$tables, "allcc_secondary_linear_trend.csv"))

# ------------------------------------------------ 30/31 sensitivity
batch_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "no_batch"),
  by_cols = c("Sex", "stratum", "contrast"), label = "batch_adjusted_vs_unadjusted")
scale_sens <- mmm_sensitivity_compare(
  res$contrasts %>% filter(.data$variant == "primary"),
  res$contrasts %>% filter(.data$variant == "raw_scale"),
  by_cols = c("Sex", "stratum", "contrast"), label = "log1p_vs_raw_gaussian")
write_csv(batch_sens, file.path(dirs$tables, "allcc_batch_sensitivity.csv"))
write_csv(scale_sens, file.path(dirs$tables, "allcc_response_scale_sensitivity.csv"))

# ------------------------------------------------ figures
traj <- res$trajectory %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = names(group_colors)))

p1 <- ggplot(traj, aes(TimeAxis, fit, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_grid(Sex ~ stratum) +
  scale_colour_manual(values = group_colors) + scale_fill_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(0, 12, 3), limits = c(0, 12)) +
  labs(title = "Acute response after each cage change (first Active block, 12 h)",
       subtitle = "One comparable clock-anchored window per cage change; CC1 is the canonical first night",
       x = "Hours into the acute Active block", y = "Predicted log1p(Movement)") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "allcc_acute_trajectories.svg"), p1, width = 11, height = 5)

p2 <- ggplot(q4, aes(estimate, contrast, colour = p_bh < 0.05)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.15) +
  geom_point(size = 2) +
  facet_grid(Sex ~ stratum) +
  scale_colour_manual(values = c(`FALSE` = "grey45", `TRUE` = "#e63947"), guide = "none") +
  labs(title = "Acute-window group contrasts by cage change",
       subtitle = "Secondary family: 2 Sex x 4 CC x 3 contrasts = 24 tests, BH within. Red = BH < 0.05.",
       x = "Mean predicted difference over the 12 h acute window, log1p(Movement)", y = NULL) +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "allcc_group_contrasts_by_cc.svg"), p2, width = 11, height = 5)

p3 <- q4 %>% mutate(cc_num = as.integer(sub("^\\D*", "", .data$stratum))) %>%
  ggplot(aes(cc_num, estimate, colour = contrast, fill = contrast, group = contrast)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = seq_along(cc_levels), labels = cc_levels) +
  labs(title = "Repeated-perturbation adaptation across successive cage changes",
       subtitle = "Formal evidence is the Group x CageChange test in allcc_group_x_cc_tests.csv, not this pattern",
       x = "Cage change", y = "Acute-window group contrast") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "allcc_adaptation_summary.svg"), p3, width = 9, height = 4)

write_output_manifest(
  output_dir,
  script_name = "22_repeated_cagechange_acute_gamm.R",
  analysis_name = "SECONDARY B: repeated acute response across CC1-CC4",
  bin_level = bin_level,
  key_parameters = list(cage_changes = paste(cc_levels, collapse = ","),
                        window_hours = 12, k = MMM_GAMM_K, method = MMM_GAMM_METHOD,
                        secondary_family = "2 Sex x 4 CC x 3 contrasts = 24 tests (BH)"),
  primary_tables = c("tables/allcc_group_by_cc_contrasts.csv",
                     "tables/allcc_group_x_cc_tests.csv"),
  primary_figures = c("figures/allcc_acute_trajectories.svg",
                      "figures/allcc_adaptation_summary.svg")
)

cat("\nALLCC-Q2/Q3 formal tests (primary, Batch-adjusted):\n")
print(as.data.frame(q23 %>% select(Sex, term, df, F_stat, p_raw, p_bh)), row.names = FALSE, digits = 4)
cat("\nALLCC-Q4 acute contrasts with BH < 0.05:\n")
sig <- q4 %>% filter(.data$p_bh < 0.05) %>% select(Sex, stratum, contrast, estimate, ci_low, ci_high, p_raw, p_bh)
if (nrow(sig) == 0) cat("  none\n") else print(as.data.frame(sig), row.names = FALSE, digits = 4)
cat("\nDone ->", output_dir, "\n")
