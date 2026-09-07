# ================================================================
# PRIMARY canonical first-night GAMM
# MMMSociability
# ================================================================
# Estimand:
#   During the FIRST Active block after the first cage change (18:30 inclusive
#   -> 06:30 exclusive, exactly 12 h), do CON / RES / SUS differ in Movement
#   level and within-night trajectory shape?
#
# This is the anchor analysis. Two secondary analyses extend it and must not be
# pooled with it:
#   Analysis/21_cc1_active_longitudinal_gamm.R    all CC1 Active nights
#   Analysis/22_repeated_cagechange_acute_gamm.R  acute window after CC1..CC4
#
# PRIMARY inferential family: 2 Sex x 3 Group contrasts = 6 tests, BH within.
# Nothing from the secondary analyses ever enters that family.
#
# Window selection uses the validated Functions/first_night_window_helpers.R
# directly. Stage 09 is not modified.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(mgcv)
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

# ------------------------------------------------ configuration
project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"
bin_size_sec <- 600L
group_colors <- c(CON = "#3d3b6e", RES = "#C6C3BB", SUS = "#e63947")

input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                        "all_behavior_metrics.csv")
output_dir <- behavior_stage_dir(project_root, "20", "first_night_gamm", bin_level)
dirs <- analysis_output_dirs(output_dir)
for (d in list(dirs$root, dirs$tables, dirs$figure_root, dirs$audit)) ensure_dir(d)

cat("PRIMARY first-night GAMM |", bin_level, "\n")

# ------------------------------------------------ data + canonical window
raw <- read_csv(
  input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE
)
stopifnot(all(raw$BinSizeSec == bin_size_sec))

selected <- mmm_select_first_night_window(raw, bin_size_sec = bin_size_sec) %>%
  mutate(TimeHours = .data$elapsed_hours_in_window)

qc <- mmm_first_night_window_qc(selected)
write_csv(qc, file.path(dirs$tables, "first_night_window_qc.csv"))
cat("  window: ", nrow(selected), " rows | ", n_distinct(selected$AnimalNum), " animals | ",
    sum(qc$window_complete), " complete\n", sep = "")

model_dat <- selected %>%
  filter(!is.na(.data$Movement)) %>%
  mutate(
    y_log = log1p(.data$Movement),
    y_raw = .data$Movement,
    Group = factor(.data$Group, levels = c("CON", "RES", "SUS")),
    Batch = factor(.data$Batch),
    AnimalNum = factor(.data$AnimalNum),
    GroupOrd = mmm_ordered_cross(.data$Group)
  )

# ------------------------------------------------ model
# One night per animal, so an AR1 sequence is one animal; it still restarts on
# any missing target slot.
AR_BLOCKS <- character(0)

primary_formula <- function(response, with_batch) {
  rhs <- paste0(
    if (with_batch) "Batch + " else "", "Group",
    " + s(TimeHours, k = ", MMM_GAMM_K, ", bs = 'tp')",
    " + s(TimeHours, by = GroupOrd, k = ", MMM_GAMM_K, ", bs = 'tp')",
    " + s(AnimalNum, bs = 're')"
  )
  stats::as.formula(paste(response, "~", rhs))
}

fit_one <- function(dat_sex, response, with_batch, label, role) {
  d <- mmm_order_for_ar1(dat_sex, block_cols = AR_BLOCKS) %>% droplevels()
  d$.ar_start <- mmm_build_ar_start(d, block_cols = AR_BLOCKS)
  form <- primary_formula(response, with_batch)
  fit <- mmm_fit_gamm_ar1(form, d)
  list(fit = fit, data = d, formula = form, label = label, role = role,
       response = response, with_batch = with_batch)
}

grid_for <- function(d) {
  tibble(TimeHours = seq(0, 12, length.out = 100)) %>%
    tidyr::crossing(Group = factor(levels(d$Group), levels = levels(d$Group))) %>%
    mutate(GroupOrd = factor(as.character(.data$Group), levels = levels(d$GroupOrd), ordered = TRUE))
}

analyse <- function(mo) {
  d <- mo$data
  m <- mo$fit$model
  batch_w <- table(distinct(d, AnimalNum, Batch)$Batch)
  batch_w <- batch_w[batch_w > 0]
  animal_ref <- levels(d$AnimalNum)[1]
  base_grid <- tibble(TimeHours = seq(0, 12, length.out = 100))

  contrasts <- mmm_gamm_group_contrasts(
    m, base_grid, batch_w, animal_ref,
    stratum_setting = list(), cross_col = "GroupOrd",
    cross_levels = levels(d$GroupOrd)
  ) %>% mutate(model_label = mo$label, .before = 1)

  traj <- purrr::map_dfr(levels(d$Group), function(g) {
    gg <- base_grid %>%
      mutate(Group = factor(g, levels = levels(d$Group)),
             GroupOrd = factor(g, levels = levels(d$GroupOrd), ordered = TRUE))
    mmm_gamm_trajectory(m, gg, batch_w, animal_ref)
  }) %>% mutate(model_label = mo$label, .before = 1)

  pw <- purrr::map_dfr(list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES")), function(pp) {
    s1 <- list(Group = factor(pp[1], levels = levels(d$Group)),
               GroupOrd = factor(pp[1], levels = levels(d$GroupOrd), ordered = TRUE))
    s0 <- list(Group = factor(pp[2], levels = levels(d$Group)),
               GroupOrd = factor(pp[2], levels = levels(d$GroupOrd), ordered = TRUE))
    mmm_gamm_pointwise_diff(m, base_grid, s1, s0, batch_w, animal_ref) %>%
      mutate(contrast = paste0(pp[1], "-", pp[2]), .before = 1)
  }) %>% mutate(model_label = mo$label, .before = 1)

  list(contrasts = contrasts, trajectory = traj, pointwise = pw,
       spec = mmm_gamm_spec_row(mo$fit, mo$label, mo$formula, d,
                                response_scale = mo$response,
                                batch_adjusted = mo$with_batch, role = mo$role),
       parametric = mmm_gamm_parametric_table(m, mo$label),
       smooth = mmm_gamm_smooth_table(m, mo$label),
       ar1 = mmm_ar1_sequence_proof(d, d$.ar_start, block_cols = AR_BLOCKS) %>%
         mutate(model_label = mo$label, .before = 1))
}

sexes <- sort(unique(as.character(model_dat$Sex)))
variants <- tibble::tribble(
  ~key,              ~response, ~with_batch, ~role,
  "primary",         "y_log",   TRUE,        "PRIMARY",
  "no_batch",        "y_log",   FALSE,       "SENSITIVITY_batch",
  "raw_scale",       "y_raw",   TRUE,        "SENSITIVITY_response_scale"
)

all_res <- list()
for (sx in sexes) {
  dsex <- model_dat %>% filter(.data$Sex == sx)
  for (i in seq_len(nrow(variants))) {
    v <- variants[i, ]
    lbl <- paste0("firstnight|", sx, "|", v$key)
    cat("  fitting ", lbl, " ...", sep = "")
    mo <- fit_one(dsex, v$response, v$with_batch, lbl, v$role)
    res <- analyse(mo)
    res$contrasts <- res$contrasts %>% mutate(Sex = sx, variant = v$key, .before = 1)
    res$trajectory <- res$trajectory %>% mutate(Sex = sx, variant = v$key, .before = 1)
    res$pointwise <- res$pointwise %>% mutate(Sex = sx, variant = v$key, .before = 1)
    res$spec <- res$spec %>% mutate(Sex = sx, variant = v$key, .before = 1)
    res$parametric <- res$parametric %>% mutate(Sex = sx, variant = v$key, .before = 1)
    res$smooth <- res$smooth %>% mutate(Sex = sx, variant = v$key, .before = 1)
    res$ar1 <- res$ar1 %>% mutate(Sex = sx, variant = v$key, .before = 1)
    all_res[[lbl]] <- res
    cat(" rho=", round(mo$fit$rho, 3), " ar1=", mo$fit$ar1_applied, "\n", sep = "")
  }
}

bind_part <- function(part) purrr::map_dfr(all_res, part)

contrasts_all <- bind_part("contrasts")
trajectory_all <- bind_part("trajectory")
pointwise_all <- bind_part("pointwise")

# ------------------------------------------------ PRIMARY family: BH over 6
primary_contrasts <- contrasts_all %>%
  filter(.data$variant == "primary") %>%
  mmm_bh_family("PRIMARY_FIRST_NIGHT__2Sex_x_3GroupContrasts")
stopifnot(nrow(primary_contrasts) == 6L)

# Sensitivity variants get their own separate families, never merged with the 6.
sens_contrasts <- contrasts_all %>%
  filter(.data$variant != "primary") %>%
  group_by(.data$variant) %>%
  group_modify(~ mmm_bh_family(.x, paste0("SENSITIVITY_FIRST_NIGHT__", .y$variant))) %>%
  ungroup()

write_csv(primary_contrasts, file.path(dirs$tables, "first_night_primary_contrasts.csv"))
write_csv(sens_contrasts, file.path(dirs$tables, "first_night_sensitivity_contrasts.csv"))
write_csv(trajectory_all, file.path(dirs$tables, "first_night_trajectory_predictions.csv"))
write_csv(pointwise_all, file.path(dirs$tables, "first_night_pairwise_trajectory_differences.csv"))
write_csv(bind_part("spec"), file.path(dirs$tables, "first_night_model_specification.csv"))
write_csv(bind_part("parametric"), file.path(dirs$tables, "first_night_parametric_terms.csv"))
write_csv(bind_part("smooth"), file.path(dirs$tables, "first_night_smooth_terms.csv"))
write_csv(bind_part("ar1"), file.path(dirs$audit, "first_night_ar1_sequence_proof.csv"))

batch_sens <- mmm_sensitivity_compare(
  contrasts_all %>% filter(.data$variant == "primary"),
  contrasts_all %>% filter(.data$variant == "no_batch"),
  by_cols = c("Sex", "contrast"), label = "batch_adjusted_vs_unadjusted")
scale_sens <- mmm_sensitivity_compare(
  contrasts_all %>% filter(.data$variant == "primary"),
  contrasts_all %>% filter(.data$variant == "raw_scale"),
  by_cols = c("Sex", "contrast"), label = "log1p_vs_raw_gaussian")
write_csv(batch_sens, file.path(dirs$tables, "first_night_batch_sensitivity.csv"))
write_csv(scale_sens, file.path(dirs$tables, "first_night_response_scale_sensitivity.csv"))

# ------------------------------------------------ figures
traj_p <- trajectory_all %>% filter(.data$variant == "primary")
p1 <- ggplot(traj_p, aes(TimeHours, fit, colour = Group, fill = Group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Sex) +
  scale_colour_manual(values = group_colors) + scale_fill_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  labs(title = "Canonical first night after CC1 (18:30-06:30, 12 h)",
       subtitle = "mgcv::bam | Batch-adjusted | shared s(TimeHours) + ordered-factor Group difference smooths | ribbon = 95% CI",
       x = "Hours into the first Active block", y = "Predicted log1p(Movement)") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "first_night_trajectories.svg"), p1, width = 9, height = 4)

p2 <- pointwise_all %>% filter(.data$variant == "primary") %>%
  ggplot(aes(TimeHours, diff)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "grey30") +
  geom_line(linewidth = 0.6) +
  facet_grid(contrast ~ Sex) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  labs(title = "First night: pointwise predicted group differences",
       subtitle = "Shaded band = pointwise 95% CI; band excluding 0 marks a timepoint difference",
       x = "Hours into the first Active block", y = "Predicted difference, log1p(Movement)") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "first_night_pairwise_differences.svg"), p2, width = 9, height = 6)

p3 <- ggplot(primary_contrasts, aes(estimate, contrast)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.15) +
  geom_point(size = 2) +
  facet_wrap(~ Sex) +
  labs(title = "First night: 12 h window-averaged group contrasts",
       subtitle = "PRIMARY family: 2 Sex x 3 contrasts = 6 tests, BH within",
       x = "Mean predicted difference over 0-12 h, log1p(Movement)", y = NULL) +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave(file.path(dirs$figure_root, "first_night_contrasts.svg"), p3, width = 8, height = 3.5)

write_output_manifest(
  output_dir,
  script_name = "20_first_night_gamm.R",
  analysis_name = "PRIMARY canonical first-night GAMM",
  bin_level = bin_level,
  key_parameters = list(window_hours = 12, k = MMM_GAMM_K, method = MMM_GAMM_METHOD,
                        primary_family = "2 Sex x 3 Group contrasts = 6 tests (BH)"),
  primary_tables = c("tables/first_night_primary_contrasts.csv",
                     "tables/first_night_model_specification.csv"),
  primary_figures = c("figures/first_night_trajectories.svg",
                      "figures/first_night_contrasts.svg")
)

cat("\nPRIMARY contrasts (BH over the 6-test family):\n")
print(as.data.frame(primary_contrasts %>%
  select(Sex, contrast, estimate, ci_low, ci_high, p_raw, p_bh)), row.names = FALSE, digits = 4)
cat("\nDone ->", output_dir, "\n")
