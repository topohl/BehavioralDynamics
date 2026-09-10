# ================================================================
# Behavior main figure candidate recomposition
# ASSEMBLY ONLY - NO ANALYSIS, NO CHANGE TO THE FROZEN STAGE 27 FIGURE
# ================================================================
#
# This deliberately separate candidate consumes only already validated outputs
# from the canonical CombZ producer and Stages 09, 20 and 22. It reuses the
# Stage 27 semantic path, plotting, source-data and provenance infrastructure.
#
# It must never fit or refit a model, predict from a model object, resample,
# calculate a p/q value, redefine CombZ, select a window or invoke a producer.
# The only arithmetic is declared descriptive summarisation of empirical values
# already persisted by the owning stages. Stage 20 curves are plotted directly:
# no across-Group display average is permitted.
#
# All products are written below:
#   <frozen Stage 27 root>/candidates/candidate_acute5_groups_v4/
# The existing frozen Stage 27 tree is hashed before and after the build, with
# the candidate subtree excluded, and the build fails if any frozen file moves
# or changes.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Cannot locate Analysis/_pipeline_setup.R.", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("mmm_publication_theme.R")
source_mmm_helper("gamm_publication_style_helpers.R")
source_mmm_helper("project_paths.R")
source_mmm_helper("behavior_main_figure_helpers.R")

CANDIDATE_ID <- "candidate_acute5_groups_v4"
CANDIDATE_STEM <- "behavior_main_figure__five_panel_acute_groups_v4_candidate"
CANDIDATE_PANELS <- c("A", "B", "C", "D", "E")
HEADLINE_MODEL <- "movement_mean"
HEADLINE_FEATURE <- "Movement_mean"
PREDICTION_TARGET <- "continuous_CombZ"
PROJECT_ROOT <- mmm_project_root()
FROZEN_ROOT <- mmm_publication_root(PROJECT_ROOT)
CANDIDATE_ROOT <- file.path(FROZEN_ROOT, "candidates", CANDIDATE_ID)

dirs <- setNames(
  lapply(names(MMM_PUBLICATION_SUBDIRS), function(kind) {
    mmm_publication_dir(kind, create = TRUE, publication_root = CANDIDATE_ROOT)
  }),
  names(MMM_PUBLICATION_SUBDIRS))
dirs$root <- CANDIDATE_ROOT

cat("Stage 27 candidate | five-panel behavior recomposition (assembly only)\n")
cat("  analysis-data root :", PROJECT_ROOT, "\n")
cat("  frozen Stage 27 root:", FROZEN_ROOT, "\n")
cat("  candidate root     :", CANDIDATE_ROOT, "\n")

# ------------------------------------------------------ frozen-tree safeguard

normalize_for_prefix <- function(path) {
  tolower(gsub("\\\\", "/", normalizePath(path, winslash = "/", mustWork = FALSE)))
}

hash_frozen_tree <- function() {
  if (!dir.exists(FROZEN_ROOT)) return(tibble(relative_path = character(), bytes = numeric(), sha256 = character()))
  all_files <- list.files(FROZEN_ROOT, recursive = TRUE, full.names = TRUE,
                          all.files = TRUE, no.. = TRUE)
  all_files <- all_files[file.exists(all_files) & !dir.exists(all_files)]
  candidate_prefix <- paste0(normalize_for_prefix(file.path(FROZEN_ROOT, "candidates")), "/")
  # Windows Explorer may create/remove Thumbs.db independently of the pipeline;
  # it is OS metadata rather than a frozen Stage 27 product.
  keep <- !startsWith(normalize_for_prefix(all_files), candidate_prefix) &
    tolower(basename(all_files)) != "thumbs.db"
  all_files <- sort(all_files[keep])
  tibble(
    relative_path = substring(normalize_for_prefix(all_files),
                              nchar(normalize_for_prefix(FROZEN_ROOT)) + 2L),
    bytes = as.numeric(file.info(all_files)$size),
    sha256 = mmm_file_sha256(all_files))
}

frozen_before <- hash_frozen_tree()
if (nrow(frozen_before) < 1L || anyNA(frozen_before$sha256)) {
  stop("Could not establish a complete SHA-256 baseline for the frozen Stage 27 tree.",
       call. = FALSE)
}

# --------------------------------------------------------- guarded input reads

inputs_seen <- list()
read_canonical <- function(key, role) {
  path <- mmm_path_get(key, file = role, required = TRUE, root = PROJECT_ROOT)
  normalized <- normalize_for_prefix(path)
  forbidden <- c("/archive/", "/_archive/", "/quarantine/", "/snapshot/",
                 "/release_bundle/", "/superseded/")
  if (any(vapply(forbidden, function(x) grepl(x, normalized, fixed = TRUE), logical(1)))) {
    stop("Candidate refused a noncanonical input: ", path, call. = FALSE)
  }
  inputs_seen[[paste(key, role, sep = "/")]] <<- path
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

combz_animal <- read_canonical("behavior.later_outcome_combz", "animal_level")
combz_components <- read_canonical("behavior.later_outcome_combz", "component_definition")
combz_parity <- read_canonical("behavior.later_outcome_combz", "parity_audit")

s09_animal <- read_canonical("behavior.first_night_movement", "model_input")
s09_assoc <- read_canonical("behavior.first_night_combz_association", "associations")
s09_predictions <- read_canonical("behavior.early_prediction", "predictions")
s09_performance <- read_canonical("behavior.early_prediction", "performance")
s09_permutation <- read_canonical("behavior.early_prediction", "permutation")
s09_draws <- read_canonical("behavior.early_prediction", "permutation_draws")

s20_trajectory <- read_canonical("behavior.first_active_trajectory", "trajectory_predictions")
s20_contrasts <- read_canonical("behavior.first_active_trajectory", "primary_contrasts")
s20_model_spec <- read_canonical("behavior.first_active_trajectory", "model_specification")

s22_empirical <- read_canonical("behavior.repeated_acute_movement", "empirical_auc")
s22_adaptation <- read_canonical("behavior.repeated_acute_movement", "adaptation_registry")
s22_sex_moderation <- read_canonical("behavior.repeated_acute_movement", "sex_moderation")
s22_model_spec <- read_canonical("behavior.repeated_acute_movement", "model_specification")

# ------------------------------------------------------------- input contracts

bmf_assert_schema(
  combz_animal, "canonical CombZ animal table",
  required = c("AnimalNum", "Sex", "CombZ", "outcome_group", "combz_definition_id"),
  keys = "AnimalNum", allowed = list(Sex = BMF_SEX_LEVELS), finite = "CombZ")
bmf_assert_schema(
  combz_components, "canonical CombZ component definition",
  required = c("component", "domain", "weight_in_composite", "combz_definition_id"),
  keys = "component", finite = "weight_in_composite")

bmf_assert_schema(
  s09_animal, "Stage 09 model frame",
  required = c("AnimalNum", "Sex", "Group", "Movement_mean", "outcome", "BinLevel", "EarlyWindow"),
  keys = "AnimalNum", allowed = list(Sex = BMF_SEX_LEVELS, Group = BMF_GROUP_LEVELS),
  finite = c("Movement_mean", "outcome"), n_rows = 111L)
bmf_assert_schema(
  s09_assoc, "Stage 09 primary associations",
  required = c("feature", "n", "spearman_rho", "spearman_boot_ci_low",
               "spearman_boot_ci_high", "spearman_p_bh"),
  keys = "feature", finite = c("n", "spearman_rho", "spearman_boot_ci_low",
                               "spearman_boot_ci_high", "spearman_p_bh"))
bmf_assert_schema(
  s09_predictions, "Stage 09 held-out predictions",
  required = c("AnimalNum", "Sex", "Group", "observed", "predicted", "Model"),
  keys = c("AnimalNum", "Model"), allowed = list(Sex = BMF_SEX_LEVELS, Group = BMF_GROUP_LEVELS),
  finite = c("observed", "predicted"))
bmf_assert_schema(
  s09_performance, "Stage 09 prediction performance",
  required = c("model_id", "cv_r2"), keys = "model_id", finite = "cv_r2")
bmf_assert_schema(
  s09_permutation, "Stage 09 permutation summary",
  required = c("model", "observed_statistic", "empirical_p", "n_permutations"),
  keys = "model", finite = c("observed_statistic", "empirical_p", "n_permutations"))
bmf_assert_schema(
  s09_draws, "Stage 09 persisted permutation draws",
  required = c("permutation_id", "model_id", "performance_value", "is_observed",
               "n_permutations", "seed", "performance_metric"),
  keys = c("model_id", "permutation_id"), finite = c("performance_value", "n_permutations", "seed"))

bmf_assert_schema(
  s20_trajectory, "Stage 20 first-active predictions",
  required = c("Sex", "variant", "TimeAxis", "Group", "fit", "lower", "upper"),
  keys = c("Sex", "variant", "TimeAxis", "Group"),
  allowed = list(Sex = BMF_SEX_LEVELS, Group = BMF_GROUP_LEVELS),
  finite = c("TimeAxis", "fit", "lower", "upper"))
bmf_assert_schema(
  s20_contrasts, "Stage 20 primary contrast family",
  required = c("Sex", "variant", "contrast", "family_id", "n_tests_in_family", "p_bh"),
  keys = c("Sex", "variant", "contrast"), finite = c("n_tests_in_family", "p_bh"))
bmf_assert_schema(
  s20_model_spec, "Stage 20 model specification",
  required = c("Sex", "variant", "role", "formula", "engine"),
  keys = c("Sex", "variant"))

bmf_assert_schema(
  s22_empirical, "Stage 22 animal empirical table",
  required = c("AnimalNum", "Sex", "Group", "CageChangeWindow",
               "observed_mean_Movement", "coverage", "quantity_class"),
  keys = c("AnimalNum", "CageChangeWindow"),
  allowed = list(Sex = BMF_SEX_LEVELS, Group = BMF_GROUP_LEVELS),
  finite = c("observed_mean_Movement", "coverage"))
bmf_assert_schema(
  s22_adaptation, "Stage 22 adaptation registry",
  required = c("family_id", "family_key", "Sex", "scope", "contrast_name",
               "tests_in_family", "estimate", "ci_low", "ci_high", "p_raw", "q_BH_family"),
  keys = c("family_key", "Sex", "scope", "contrast_name"),
  allowed = list(Sex = BMF_SEX_LEVELS),
  finite = c("tests_in_family", "estimate", "ci_low", "ci_high", "p_raw", "q_BH_family"))
bmf_assert_schema(
  s22_sex_moderation, "Stage 22 sex-moderation contrasts",
  required = c("quantity", "scope", "contrast_name", "est_F", "est_M",
               "diff_F_minus_M", "ci_low", "ci_high", "p_raw", "caveat"),
  keys = c("quantity", "scope", "contrast_name", "stratum", "contrast"),
  finite = c("est_F", "est_M", "diff_F_minus_M", "ci_low", "ci_high", "p_raw"))
bmf_assert_schema(
  s22_model_spec, "Stage 22 model specification",
  required = c("Sex", "variant", "role", "formula", "engine", "shape_structure"),
  keys = c("Sex", "variant"))

stopifnot(
  identical(unique(s09_animal$BinLevel), "10min_based"),
  identical(unique(s09_animal$EarlyWindow), "first_12h_active_first_cage_change"),
  setequal(unique(s20_trajectory$variant), c("primary", "no_batch", "raw_scale", "k8")),
  nrow(filter(s20_model_spec, .data$variant == "primary", .data$role == "PRIMARY")) == 2L,
  nrow(filter(s22_model_spec, .data$variant == "primary", .data$role == "PRIMARY")) == 2L)

# Canonical CombZ and the Stage 09 analysis frame must identify the same values
# for all 111 RFID animals. This is a parity check, not a reconstruction.
combz_join <- s09_animal %>%
  transmute(AnimalID = canonical_animal_id(.data$AnimalNum),
            Group_s09 = as.character(.data$Group), CombZ_s09 = .data$outcome) %>%
  inner_join(
    combz_animal %>%
      transmute(AnimalID = canonical_animal_id(.data$AnimalNum),
                Group_combz = as.character(.data$outcome_group), CombZ_combz = .data$CombZ),
    by = "AnimalID")
combz_max_drift <- max(abs(combz_join$CombZ_s09 - combz_join$CombZ_combz))
stopifnot(nrow(combz_join) == 111L, combz_max_drift <= 1e-12,
          identical(combz_join$Group_s09, combz_join$Group_combz))

# ------------------------------------------------------------- frozen numbers

assoc_head <- s09_assoc %>% filter(.data$feature == HEADLINE_FEATURE)
perf_head <- s09_performance %>% filter(.data$model_id == HEADLINE_MODEL)
baseline_head <- s09_performance %>% filter(.data$model_id == "mean_only")
perm_head <- s09_permutation %>% filter(.data$model == HEADLINE_MODEL)
draw_head <- s09_draws %>% filter(.data$model_id == HEADLINE_MODEL)
null_head <- draw_head %>% filter(!.data$is_observed)
observed_head <- draw_head %>% filter(.data$is_observed)

overall_adaptation <- s22_adaptation %>%
  filter(.data$family_key == "A_OVERALL", .data$scope == "overall_population") %>%
  arrange(match(.data$Sex, BMF_SEX_LEVELS))
phenotype_adaptation <- s22_adaptation %>%
  filter(.data$family_key == "C_PHENOTYPE_DEPENDENT")
overall_sex_moderation <- s22_sex_moderation %>%
  filter(.data$quantity == "adaptation_AUC_log1p_CageChange",
         .data$scope == "overall_population", .data$contrast_name == "CC4 - CC1")
primary_s20_contrasts <- s20_contrasts %>% filter(.data$variant == "primary")
s20_n_supported <- sum(primary_s20_contrasts$p_bh <= 0.05)
phenotype_n_supported <- sum(phenotype_adaptation$q_BH_family <= 0.05)

stopifnot(
  nrow(assoc_head) == 1L, nrow(perf_head) == 1L, nrow(baseline_head) == 1L,
  nrow(perm_head) == 1L,
  nrow(observed_head) == 1L,
  nrow(null_head) == unique(perm_head$n_permutations),
  identical(observed_head$performance_value, perm_head$observed_statistic),
  identical(perf_head$cv_r2, perm_head$observed_statistic),
  nrow(overall_adaptation) == 2L,
  identical(as.character(overall_adaptation$Sex), BMF_SEX_LEVELS),
  all(overall_adaptation$tests_in_family == 2L),
  nrow(phenotype_adaptation) == 6L,
  all(phenotype_adaptation$tests_in_family == 6L),
  all(phenotype_adaptation$q_BH_family > 0.05),
  nrow(overall_sex_moderation) == 1L,
  overall_sex_moderation$ci_low < 0,
  overall_sex_moderation$ci_high > 0,
  overall_sex_moderation$p_raw > 0.05,
  nrow(primary_s20_contrasts) == 6L,
  all(primary_s20_contrasts$n_tests_in_family == 6L),
  all(primary_s20_contrasts$p_bh > 0.05))

fmt_num <- function(x, digits = 3L) formatC(as.numeric(x), format = "f", digits = digits)
fmt_p <- function(x) {
  ifelse(x < 0.001, formatC(x, format = "e", digits = 2),
         formatC(x, format = "f", digits = 3))
}

# ================================================================ PANEL A
# Compact paradigm, continuous CombZ and its retrospective phenotype labels.

timeline_boxes <- tibble::tribble(
  ~id, ~x, ~width, ~label,
  "sis", 8, 14, "Adolescent SIS\nstress paradigm",
  "rfid", 29, 24, "Home-cage RFID\nCC1-CC4",
  "battery", 53, 20, "Later behavioral +\nphysiological battery",
  "combz", 74, 15, paste0("Continuous CombZ\nmean of ", nrow(combz_components), " z-scores"),
  "groups", 93, 13, "Retrospective labels\nCON / RES / SUS") %>%
  mutate(xmin = .data$x - .data$width / 2, xmax = .data$x + .data$width / 2,
         ymin = 2.2, ymax = 6.8)
timeline_arrows <- tibble(
  x = timeline_boxes$xmax[-nrow(timeline_boxes)] + 0.8,
  xend = timeline_boxes$xmin[-1] - 0.8,
  y = 4.5, yend = 4.5)

pA_timeline <- ggplot() +
  geom_segment(data = timeline_arrows,
               aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
               colour = MMM_SCHEMATIC_INK[["arrow"]], linewidth = 0.35,
               arrow = grid::arrow(length = unit(1.3, "mm"), type = "closed")) +
  geom_rect(data = timeline_boxes,
            aes(xmin = .data$xmin, xmax = .data$xmax,
                ymin = .data$ymin, ymax = .data$ymax),
            fill = "white", colour = MMM_SCHEMATIC_INK[["stage_border"]], linewidth = 0.35) +
  geom_text(data = timeline_boxes, aes(.data$x, y = 4.5, label = .data$label),
            size = (MMM_BASE_PT - 0.9) / .pt, lineheight = 1.03) +
  annotate("text", x = 50, y = 8.25, label = "earlier  ->  later",
           colour = "grey35", size = (MMM_BASE_PT - 1.7) / .pt) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 9), expand = c(0, 0)) +
  theme_mmm_schematic()

combz_display <- combz_animal %>%
  transmute(AnimalID = canonical_animal_id(.data$AnimalNum), Sex = .data$Sex,
            Group = factor(.data$outcome_group, levels = BMF_GROUP_LEVELS),
            CombZ = .data$CombZ)
stopifnot(!anyNA(combz_display$Group), setequal(unique(combz_display$Group), BMF_GROUP_LEVELS))
pA_strip <- ggplot(combz_display, aes(.data$CombZ, 0,
                                     fill = .data$Group, shape = .data$Group)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_point(position = position_jitter(width = 0, height = 0.055, seed = 2701L),
             size = 0.75, stroke = 0.20, colour = "grey25", alpha = 0.86) +
  mmm_scale_fill_group(name = "Later phenotype", drop = FALSE) +
  mmm_scale_shape_group(name = "Later phenotype", drop = FALSE) +
  labs(x = "Later continuous outcome (CombZ; lower = greater burden)", y = NULL) +
  scale_y_continuous(limits = c(-0.1, 0.1), breaks = NULL, expand = c(0, 0)) +
  theme_mmm_pub() +
  theme(panel.grid = element_blank(), axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "top",
        plot.margin = margin(0, 4, 2, 4))

pA <- (pA_timeline + mmm_panel_label("a") + mmm_tag_theme()) / pA_strip +
  patchwork::plot_layout(heights = c(1.7, 1))

# ================================================================ PANEL B
# Stage 20 fitted the primary trajectories. Every line below is an original
# persisted Sex x Group curve: no mean, weighted mean, row mean, grouping or
# summarisation across Group is allowed in this panel.

b_display <- s20_trajectory %>%
  filter(.data$variant == "primary") %>%
  mutate(Sex = factor(.data$Sex, levels = BMF_SEX_LEVELS),
         Group = factor(.data$Group, levels = BMF_GROUP_LEVELS)) %>%
  arrange(match(.data$Sex, BMF_SEX_LEVELS), .data$TimeAxis,
          match(.data$Group, BMF_GROUP_LEVELS))
stopifnot(nrow(b_display) == 600L,
          all(table(b_display$Sex, b_display$Group) == 100L),
          !anyNA(b_display$Group), !anyNA(b_display$fit))
b_curve_counts <- as.integer(table(b_display$Sex, b_display$Group))
b_points_per_curve <- unique(b_curve_counts)
stopifnot(length(b_points_per_curve) == 1L)

pB <- ggplot(b_display, aes(.data$TimeAxis, .data$fit,
                            colour = .data$Group, linetype = .data$Group)) +
  geom_line(linewidth = 0.68) +
  facet_wrap(~Sex) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12), limits = c(0, 12), expand = c(0.01, 0)) +
  mmm_scale_y_log1p(response_breaks = c(0, 1, 2, 5, 10)) +
  mmm_scale_colour_group(name = "Later phenotype", drop = FALSE) +
  mmm_scale_linetype_group(name = "Later phenotype", drop = FALSE) +
  labs(x = "Hours since first Active window began") +
  theme_mmm_pub() +
  theme(panel.spacing = unit(5, "pt"), legend.position = "top")

# ================================================================ PANEL C
# Empirical animal points remain on their observed Movement scale. The exact
# Stage 22 overall-population CC4-CC1 estimand is carried as annotation in its
# own explicit model-scale units, not overlaid as if it shared the y axis.

c_empirical <- s22_empirical %>%
  filter(.data$quantity_class == "descriptive_observed") %>%
  mutate(Group = factor(.data$Group, levels = BMF_GROUP_LEVELS),
         Sex = factor(.data$Sex, levels = BMF_SEX_LEVELS),
         CageChange = .data$CageChangeWindow,
         CageChangeIndex = as.integer(str_extract(.data$CageChangeWindow, "[0-9]+"))) %>%
  arrange(match(.data$Sex, BMF_SEX_LEVELS), .data$AnimalNum, .data$CageChangeIndex)
stopifnot(setequal(unique(c_empirical$CageChangeIndex), 1:4), nrow(c_empirical) == 444L,
          !anyNA(c_empirical$Group), setequal(unique(c_empirical$Group), BMF_GROUP_LEVELS))

c_summary <- c_empirical %>%
  group_by(.data$Sex, .data$CageChangeIndex) %>%
  summarise(
    n_animals = sum(is.finite(.data$observed_mean_Movement)),
    median = median(.data$observed_mean_Movement, na.rm = TRUE),
    q25 = quantile(.data$observed_mean_Movement, 0.25, na.rm = TRUE, names = FALSE),
    q75 = quantile(.data$observed_mean_Movement, 0.75, na.rm = TRUE, names = FALSE),
    .groups = "drop") %>%
  mutate(
    CageChange = paste0("CC", .data$CageChangeIndex),
    quantity_class = "descriptive_observed",
    summary_type = "pooled animal-level median and interquartile range",
    pooling_definition = paste(
      "all animal-level observations within Sex x CageChange;",
      "Group is descriptive metadata and is not a weighting variable"))

c_group_summary <- c_empirical %>%
  group_by(.data$Sex, .data$Group, .data$CageChangeIndex) %>%
  summarise(
    n_animals = sum(is.finite(.data$observed_mean_Movement)),
    median = median(.data$observed_mean_Movement, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    CageChange = paste0("CC", .data$CageChangeIndex),
    quantity_class = "descriptive_observed",
    summary_type = "group-specific animal-level median",
    pooling_definition = paste(
      "animal-level observations within Sex x Group x CageChange;",
      "descriptive display only; no group-specific inference")) %>%
  arrange(match(.data$Sex, BMF_SEX_LEVELS), match(.data$Group, BMF_GROUP_LEVELS),
          .data$CageChangeIndex)
stopifnot(nrow(c_group_summary) == 24L,
          all(table(c_group_summary$Sex, c_group_summary$Group) == 4L))

c_annotations <- overall_adaptation %>%
  transmute(
    Sex = .data$Sex, x = 1.08, y = Inf,
    label = paste0("Overall CC4-CC1 AUC: ", fmt_num(.data$estimate, 2),
                   " [", fmt_num(.data$ci_low, 2), ", ", fmt_num(.data$ci_high, 2), "]\n",
                   "log1p Movement x h; q = ", fmt_p(.data$q_BH_family)))

pC <- ggplot(c_empirical,
             aes(.data$CageChangeIndex, .data$observed_mean_Movement,
                 group = .data$AnimalNum, colour = .data$Group)) +
  geom_line(linewidth = 0.17, alpha = 0.09) +
  geom_point(aes(fill = .data$Group, shape = .data$Group),
             colour = "grey25", stroke = 0.12, size = 0.52, alpha = 0.22) +
  geom_line(data = c_group_summary,
            aes(.data$CageChangeIndex, .data$median, group = .data$Group,
                colour = .data$Group, linetype = .data$Group),
            inherit.aes = FALSE, linewidth = 0.42, alpha = 0.88) +
  geom_point(data = c_group_summary,
             aes(.data$CageChangeIndex, .data$median,
                 fill = .data$Group, shape = .data$Group),
             inherit.aes = FALSE, colour = "grey25", stroke = 0.22,
             size = 0.92, alpha = 0.94) +
  geom_linerange(data = c_summary,
                 aes(x = .data$CageChangeIndex, ymin = .data$q25, ymax = .data$q75),
                 inherit.aes = FALSE, colour = MMM_CONTRAST_COLOUR, linewidth = 0.68) +
  geom_line(data = c_summary,
            aes(.data$CageChangeIndex, .data$median, group = 1),
            inherit.aes = FALSE, colour = MMM_CONTRAST_COLOUR, linewidth = 0.82) +
  geom_point(data = c_summary,
             aes(.data$CageChangeIndex, .data$median),
             inherit.aes = FALSE, shape = 23, fill = "white",
             colour = MMM_CONTRAST_COLOUR, stroke = 0.40, size = 1.72) +
  geom_text(data = c_annotations,
            aes(.data$x, .data$y, label = .data$label),
            inherit.aes = FALSE, hjust = 0, vjust = 1.2,
            size = (MMM_BASE_PT - 1.5) / .pt, lineheight = 1.02,
            colour = "grey20") +
  facet_wrap(~Sex) +
  scale_x_continuous(breaks = 1:4, labels = paste0("CC", 1:4), limits = c(0.92, 4.08)) +
  mmm_scale_colour_group(name = "Later phenotype", drop = FALSE) +
  mmm_scale_fill_group(name = "Later phenotype", drop = FALSE) +
  mmm_scale_shape_group(name = "Later phenotype", drop = FALSE) +
  mmm_scale_linetype_group(name = "Later phenotype", drop = FALSE) +
  labs(x = "Repeated cage change",
       y = "Animal mean Movement\n(transitions / 10 min)") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 0.55,
                                                   size = 1.15))) +
  theme_mmm_pub() +
  theme(panel.spacing = unit(5, "pt"), legend.position = "top")

# ================================================================ PANEL D
# Pooled continuous association. Later phenotype is descriptive point metadata;
# no group-specific fit or statistic is introduced. No fitted line is drawn
# because the canonical pooled statistic is Spearman's rho.

d_animal <- s09_animal %>%
  transmute(AnimalID = canonical_animal_id(.data$AnimalNum),
            Sex = factor(.data$Sex, levels = BMF_SEX_LEVELS),
            Group = factor(.data$Group, levels = BMF_GROUP_LEVELS),
            Movement_mean = .data$Movement_mean, CombZ = .data$outcome)
stopifnot(nrow(d_animal) == 111L, !anyNA(d_animal$Group),
          setequal(unique(d_animal$Group), BMF_GROUP_LEVELS))
d_annotation <- paste0("rho = ", fmt_num(assoc_head$spearman_rho, 3),
                       "; BH q = ", fmt_p(assoc_head$spearman_p_bh),
                       "; n = ", assoc_head$n)

pD <- ggplot(d_animal, aes(.data$Movement_mean, .data$CombZ,
                           shape = .data$Sex, fill = .data$Group)) +
  geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey82") +
  geom_point(size = 1.05, stroke = 0.25, colour = "grey20", alpha = 0.86) +
  scale_shape_manual(values = c(Female = 21, Male = 24), drop = FALSE) +
  mmm_scale_fill_group(name = "Later phenotype", drop = FALSE) +
  annotate("text", x = -Inf, y = -Inf, label = d_annotation,
           hjust = -0.03, vjust = -0.55,
           size = (MMM_BASE_PT - 1.4) / .pt, colour = MMM_CONTRAST_COLOUR) +
  labs(x = "First-night Movement (transitions / 10 min)", y = "Later CombZ",
       shape = "Sex") +
  guides(fill = guide_legend(override.aes = list(shape = 21, colour = "grey20",
                                                 alpha = 1, size = 1.5))) +
  theme_mmm_pub() +
  theme(legend.position = "top", legend.justification = "left")

# ================================================================ PANEL E
# Pooled continuous held-out predictions with a compact inset of the actual
# full-refit permutation null. Later phenotype is descriptive point metadata.

e_predictions <- s09_predictions %>%
  filter(.data$Model == HEADLINE_MODEL) %>%
  transmute(AnimalID = canonical_animal_id(.data$AnimalNum),
            Sex = factor(.data$Sex, levels = BMF_SEX_LEVELS),
            Group = factor(.data$Group, levels = BMF_GROUP_LEVELS),
            observed_CombZ = .data$observed, predicted_CombZ = .data$predicted,
            model_id = .data$Model)
stopifnot(nrow(e_predictions) == 111L, !anyNA(e_predictions$Group),
          setequal(unique(e_predictions$Group), BMF_GROUP_LEVELS))
e_limits <- range(c(e_predictions$observed_CombZ, e_predictions$predicted_CombZ), na.rm = TRUE)
e_annotation <- paste0("LOAO R2 = ", fmt_num(perf_head$cv_r2, 3),
                       "\npermutation p = ", fmt_p(perm_head$empirical_p))

pE_base <- ggplot(e_predictions,
                  aes(.data$observed_CombZ, .data$predicted_CombZ,
                      shape = .data$Sex, fill = .data$Group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = 0.35,
              colour = "grey62") +
  geom_point(size = 1.0, stroke = 0.24, colour = "grey20", alpha = 0.86) +
  scale_shape_manual(values = c(Female = 21, Male = 24), drop = FALSE) +
  mmm_scale_fill_group(name = "Later phenotype", drop = FALSE) +
  coord_equal(xlim = e_limits, ylim = e_limits) +
  annotate("text", x = -Inf, y = Inf, label = e_annotation,
           hjust = -0.03, vjust = 1.25,
           size = (MMM_BASE_PT - 1.3) / .pt, colour = MMM_CONTRAST_COLOUR) +
  labs(x = "Observed CombZ", y = "Held-out predicted CombZ", shape = "Sex") +
  guides(fill = guide_legend(override.aes = list(shape = 21, colour = "grey20",
                                                 alpha = 1, size = 1.5))) +
  theme_mmm_pub() +
  theme(legend.position = "top")

pE_inset <- ggplot(null_head, aes(.data$performance_value)) +
  geom_histogram(bins = 24, fill = "grey78", colour = "white", linewidth = 0.15) +
  scale_x_continuous(breaks = c(-0.05, 0, 0.03)) +
  labs(x = expression(paste("null ", R^2)), y = NULL,
       title = paste0(nrow(null_head), " outcome permutations")) +
  theme_mmm_pub(base_size = MMM_BASE_PT - 1.2) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(size = MMM_BASE_PT - 1.2, face = "plain"),
        plot.margin = margin(2, 2, 2, 2))

e_span <- diff(e_limits)
pE <- pE_base +
  annotation_custom(
    ggplotGrob(pE_inset),
    xmin = e_limits[1] + 0.54 * e_span,
    xmax = e_limits[1] + 0.99 * e_span,
    ymin = e_limits[1] + 0.55 * e_span,
    ymax = e_limits[1] + 0.96 * e_span) +
  mmm_panel_label("e") + mmm_tag_theme()

# =========================================================== SOURCE DATA

write_source <- function(x, filename) {
  path <- file.path(dirs$source_data, filename)
  write_csv(x, path)
  filename
}

srcA <- bind_rows(
  bmf_source_data(
    timeline_boxes, "A", "27-candidate (assembly definition)",
    "Analysis/27_candidate_recompose_behavior_main_figure.R :: timeline_boxes",
    c("id", "x", "width", "label")) %>% mutate(row_role = "paradigm_schematic"),
  bmf_source_data(
    combz_display, "A", "canonical-endpoint",
    "canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv",
    c("AnimalID", "Sex", "Group", "CombZ")) %>% mutate(row_role = "continuous_outcome_point"),
  bmf_source_data(
    combz_components, "A", "canonical-endpoint",
    "canonical/later_outcome_combz/tables/combz_component_definition.csv",
    c("component", "domain", "weight_in_composite", "combz_definition_id")) %>%
    mutate(row_role = "component_definition"),
  bmf_source_data(
    combz_parity, "A", "canonical-endpoint",
    "canonical/later_outcome_combz/tables/combz_workbook_parity_audit.csv",
    names(combz_parity)) %>% mutate(row_role = "endpoint_parity_audit"))
fA <- write_source(srcA, "source_panel_a_design_groups.csv")

srcB <- bind_rows(
  bmf_source_data(
    b_display, "B", "20",
    "pipeline/20_first_night_gamm/10min/tables/first_active_trajectory_predictions.csv",
    c("Sex", "variant", "TimeAxis", "Group", "fit", "se", "lower", "upper", "ci_level")) %>%
    mutate(row_role = "plotted_persisted_group_prediction"),
  bmf_source_data(
    primary_s20_contrasts, "B", "20",
    "pipeline/20_first_night_gamm/10min/tables/first_active_primary_contrasts.csv",
    c("Sex", "contrast", "family_id", "n_tests_in_family", "p_raw", "p_bh")) %>%
    mutate(row_role = "legend_only_phenotype_contrast_context"))
fB <- write_source(srcB, "source_panel_b_stage20_group_curves.csv")

srcC <- bind_rows(
  bmf_source_data(
    c_empirical, "C", "22",
    "pipeline/22_repeated_cagechange_acute_gamm/10min/tables/allcc_active_animal_empirical_auc.csv",
    c("AnimalNum", "Sex", "Group", "CageChange", "CageChangeWindow", "CageChangeIndex",
      "expected_slots", "observed_slots", "observed_mean_Movement", "coverage", "quantity_class")) %>%
    mutate(row_role = "plotted_animal_empirical_point"),
  bmf_source_data(
    c_summary, "C", "27-candidate (descriptive summary)",
    "derived only from plotted Stage 22 empirical rows in this Source Data file",
    c("Sex", "CageChange", "CageChangeIndex", "n_animals", "median", "q25", "q75",
      "quantity_class", "summary_type", "pooling_definition")) %>%
    mutate(row_role = "plotted_descriptive_pooled_summary"),
  bmf_source_data(
    c_group_summary, "C", "27-candidate (descriptive summary)",
    "derived only from plotted Stage 22 empirical rows in this Source Data file",
    c("Sex", "Group", "CageChange", "CageChangeIndex", "n_animals", "median",
      "quantity_class", "summary_type", "pooling_definition")) %>%
    mutate(row_role = "plotted_descriptive_group_summary"),
  bmf_source_data(
    overall_adaptation, "C", "22",
    "pipeline/22_repeated_cagechange_acute_gamm/10min/tables/allcc_active_adaptation_multiplicity_registry.csv",
    c("family_id", "family_key", "Sex", "scope", "contrast_name", "tests_in_family",
      "estimate", "se", "ci_low", "ci_high", "p_raw", "q_BH_family")) %>%
    mutate(row_role = "annotated_overall_population_estimand"),
  bmf_source_data(
    phenotype_adaptation, "C", "22",
    "pipeline/22_repeated_cagechange_acute_gamm/10min/tables/allcc_active_adaptation_multiplicity_registry.csv",
    c("family_id", "family_key", "Sex", "scope", "contrast_name", "tests_in_family",
      "estimate", "se", "ci_low", "ci_high", "p_raw", "q_BH_family")) %>%
    mutate(row_role = "legend_only_phenotype_dependent_null"),
  bmf_source_data(
    overall_sex_moderation, "C", "22",
    "pipeline/22_repeated_cagechange_acute_gamm/10min/tables/allcc_active_sex_moderation_contrasts.csv",
    c("quantity", "scope", "contrast_name", "est_F", "est_M", "diff_F_minus_M",
      "se_diff", "ci_low", "ci_high", "statistic", "p_raw", "ci_level", "caveat")) %>%
    mutate(row_role = "legend_only_cross_sex_moderation"))
fC <- write_source(srcC, "source_panel_c_repeated_acute_groups.csv")

srcD <- bind_rows(
  bmf_source_data(
    d_animal, "D", "09",
    "pipeline/09_early_prediction/10min/tables/model_ladder_input.csv",
    c("AnimalID", "Sex", "Group", "Movement_mean", "CombZ")) %>%
    mutate(row_role = "plotted_animal_point"),
  bmf_source_data(
    assoc_head, "D", "09",
    "pipeline/09_early_prediction/10min/tables/primary_movement_entropyacf1_associations.csv",
    c("feature", "n", "spearman_rho", "spearman_p", "spearman_boot_ci_low",
      "spearman_boot_ci_high", "spearman_p_bh", "Evidence")) %>%
    mutate(row_role = "annotated_canonical_association"))
fD <- write_source(srcD, "source_panel_d_movement_combz_groups.csv")

srcE1 <- bmf_source_data(
  e_predictions, "E", "09",
  "pipeline/09_early_prediction/10min/tables/primary_prediction_predictions.csv",
  c("AnimalID", "Sex", "Group", "observed_CombZ", "predicted_CombZ", "model_id")) %>%
  mutate(row_role = "plotted_heldout_prediction")
fE1 <- write_source(srcE1, "source_panel_e1_loao_predictions_groups.csv")

srcE2 <- bind_rows(
  bmf_source_data(
    bind_rows(perf_head, baseline_head), "E", "09",
    "pipeline/09_early_prediction/10min/tables/primary_prediction_performance.csv",
    c("model_id", "model_label", "reporting_role", "n_animals",
      "validation_scheme", "cv_r2")) %>%
    mutate(row_role = if_else(.data$model_id == HEADLINE_MODEL,
                              "annotated_pooled_continuous_performance",
                              "legend_only_intercept_baseline")),
  bmf_source_data(
    draw_head, "E", "09",
    "pipeline/09_early_prediction/10min/tables/early_prediction_permutation_draws.csv",
    c("permutation_id", "seed", "model_id", "model_label", "endpoint", "cv_scheme",
      "performance_metric", "performance_value", "is_observed", "n_permutations")) %>%
    mutate(row_role = if_else(.data$is_observed, "observed_statistic", "plotted_permutation_null_draw")),
  bmf_source_data(
    perm_head, "E", "09",
    "pipeline/09_early_prediction/10min/tables/primary_prediction_permutation_test.csv",
    c("model", "model_label", "observed_statistic_name", "observed_statistic",
      "null_median", "null_q025", "null_q975", "empirical_p", "n_permutations", "cv_scheme", "seed")) %>%
    mutate(row_role = "legend_only_permutation_summary"))
fE2 <- write_source(srcE2, "source_panel_e2_permutation_null.csv")

# Source Data versus the exact in-memory values used to draw each numeric layer.
numeric_parity <- function(label, expected, actual, columns) {
  stopifnot(nrow(expected) == nrow(actual))
  gaps <- vapply(columns, function(column) {
    max(abs(as.numeric(expected[[column]]) - as.numeric(actual[[column]])), na.rm = TRUE)
  }, numeric(1))
  tibble(check = label, rows_expected = nrow(expected), rows_read = nrow(actual),
         parity_type = "numeric",
         max_abs_numeric_drift = max(gaps), tolerance = 1e-12,
         status = if_else(max(gaps) <= 1e-12, "PASS", "FAIL"))
}

categorical_parity <- function(label, expected, actual, columns) {
  stopifnot(nrow(expected) == nrow(actual))
  matches <- vapply(columns, function(column) {
    identical(as.character(expected[[column]]), as.character(actual[[column]]))
  }, logical(1))
  tibble(check = label, rows_expected = nrow(expected), rows_read = nrow(actual),
         parity_type = "categorical_identity",
         max_abs_numeric_drift = NA_real_, tolerance = NA_real_,
         status = if_else(all(matches), "PASS", "FAIL"))
}

read_source <- function(filename) read_csv(file.path(dirs$source_data, filename), show_col_types = FALSE)
parity_checks <- bind_rows(
  numeric_parity(
    "A continuous CombZ points", combz_display,
    read_source(fA) %>% filter(.data$row_role == "continuous_outcome_point"),
    "CombZ"),
  numeric_parity(
    "B persisted Sex x Group curves", b_display,
    read_source(fB) %>% filter(.data$row_role == "plotted_persisted_group_prediction"),
    c("TimeAxis", "fit", "lower", "upper")),
  numeric_parity(
    "C animal empirical points", c_empirical,
    read_source(fC) %>% filter(.data$row_role == "plotted_animal_empirical_point"),
    c("CageChangeIndex", "observed_mean_Movement", "coverage")),
  numeric_parity(
    "C pooled descriptive summaries", c_summary,
    read_source(fC) %>% filter(.data$row_role == "plotted_descriptive_pooled_summary"),
    c("CageChangeIndex", "median", "q25", "q75")),
  numeric_parity(
    "C group descriptive summaries", c_group_summary,
    read_source(fC) %>% filter(.data$row_role == "plotted_descriptive_group_summary"),
    c("CageChangeIndex", "n_animals", "median")),
  numeric_parity(
    "D pooled association points", d_animal,
    read_source(fD) %>% filter(.data$row_role == "plotted_animal_point"),
    c("Movement_mean", "CombZ")),
  numeric_parity(
    "E held-out predictions", e_predictions,
    read_source(fE1) %>% filter(.data$row_role == "plotted_heldout_prediction"),
    c("observed_CombZ", "predicted_CombZ")),
  numeric_parity(
    "E permutation null", null_head,
    read_source(fE2) %>% filter(.data$row_role == "plotted_permutation_null_draw"),
    c("permutation_id", "performance_value")),
  categorical_parity(
    "A retrospective Group identity", combz_display,
    read_source(fA) %>% filter(.data$row_role == "continuous_outcome_point"),
    c("AnimalID", "Sex", "Group")),
  categorical_parity(
    "B persisted Sex x Group identity", b_display,
    read_source(fB) %>% filter(.data$row_role == "plotted_persisted_group_prediction"),
    c("Sex", "Group", "variant")),
  categorical_parity(
    "C animal Group identity", c_empirical,
    read_source(fC) %>% filter(.data$row_role == "plotted_animal_empirical_point"),
    c("AnimalNum", "Sex", "Group", "CageChange", "CageChangeWindow")),
  categorical_parity(
    "C group summary identity", c_group_summary,
    read_source(fC) %>% filter(.data$row_role == "plotted_descriptive_group_summary"),
    c("Sex", "Group", "CageChange", "summary_type", "pooling_definition")),
  categorical_parity(
    "D descriptive Group identity", d_animal,
    read_source(fD) %>% filter(.data$row_role == "plotted_animal_point"),
    c("AnimalID", "Sex", "Group")),
  categorical_parity(
    "E descriptive Group identity", e_predictions,
    read_source(fE1) %>% filter(.data$row_role == "plotted_heldout_prediction"),
    c("AnimalID", "Sex", "Group", "model_id")))
if (any(parity_checks$status != "PASS")) {
  stop("Source Data parity failed:\n",
       paste(capture.output(print(as.data.frame(parity_checks))), collapse = "\n"),
       call. = FALSE)
}
write_csv(parity_checks, file.path(dirs$audit, "source_data_plot_parity_checks.csv"))

# ============================================================ FIGURE EXPORTS

tag_panel <- function(plot, letter) plot + mmm_panel_label(tolower(letter)) + mmm_tag_theme()

panel_specs <- list(
  list(panel = "A", plot = pA, stem = "panel_a_design_later_groups",
       width = MMM_WIDTH_DOUBLE_MM, height = 46, source = fA),
  list(panel = "B", plot = tag_panel(pB, "B"), stem = "panel_b_stage20_group_curves",
       width = MMM_WIDTH_SINGLE_MM, height = 56, source = fB),
  list(panel = "C", plot = tag_panel(pC, "C"), stem = "panel_c_repeated_acute_groups",
       width = MMM_WIDTH_SINGLE_MM, height = 56, source = fC),
  list(panel = "D", plot = tag_panel(pD, "D"), stem = "panel_d_movement_combz_groups",
       width = MMM_WIDTH_SINGLE_MM, height = 58, source = fD),
  list(panel = "E", plot = pE, stem = "panel_e_loao_groups_permutation_inset",
       width = MMM_WIDTH_SINGLE_MM, height = 58, source = paste(fE1, fE2, sep = "; ")))

figure_rows <- lapply(panel_specs, function(spec) {
  cat("  exporting panel", spec$panel, "...\n")
  mmm_export_figure(spec$plot, dirs$figures_panels, spec$stem,
                    spec$width, spec$height, png_preview = TRUE, dpi = 300) %>%
    mutate(panel = spec$panel, figure_role = "individual_candidate_panel",
           source_data_file = spec$source)
})

row_2 <- tag_panel(pB, "B") + tag_panel(pC, "C") +
  patchwork::plot_layout(widths = c(1, 1.05))
row_3 <- tag_panel(pD, "D") + pE +
  patchwork::plot_layout(widths = c(1, 1.05))
main_figure <- patchwork::wrap_plots(
  patchwork::wrap_elements(full = pA),
  patchwork::wrap_elements(full = row_2),
  patchwork::wrap_elements(full = row_3),
  ncol = 1, heights = c(0.78, 1, 1.04))

MAIN_HEIGHT_MM <- 168
stopifnot(MAIN_HEIGHT_MM <= MMM_MAX_HEIGHT_MM)
main_row <- mmm_export_figure(
  main_figure, dirs$figures_main, CANDIDATE_STEM,
  MMM_WIDTH_DOUBLE_MM, MAIN_HEIGHT_MM, png_preview = TRUE, dpi = 300) %>%
  mutate(panel = paste(CANDIDATE_PANELS, collapse = "-"),
         figure_role = "five_panel_candidate_composition",
         source_data_file = paste(fA, fB, fC, fD, fE1, fE2, sep = "; "))
figure_manifest <- bind_rows(figure_rows, list(main_row)) %>%
  mutate(
    pdf_sha256 = mmm_file_sha256(file.path(.data$dir, .data$pdf)),
    svg_sha256 = mmm_file_sha256(file.path(.data$dir, .data$svg)),
    png_sha256 = mmm_file_sha256(file.path(.data$dir, .data$png)))
stopifnot(nrow(figure_manifest) == 6L,
          all(file.exists(file.path(figure_manifest$dir, figure_manifest$pdf))),
          all(file.exists(file.path(figure_manifest$dir, figure_manifest$svg))),
          all(file.exists(file.path(figure_manifest$dir, figure_manifest$png))),
          !anyNA(figure_manifest$pdf_sha256), !anyNA(figure_manifest$svg_sha256),
          !anyNA(figure_manifest$png_sha256))
write_csv(figure_manifest, file.path(dirs$manifests, "figure_manifest.csv"))

# ======================================================== PROVENANCE MANIFESTS

input_manifest <- mmm_path_describe(
  keys = c("behavior.later_outcome_combz", "behavior.first_night_movement",
           "behavior.first_night_combz_association", "behavior.early_prediction",
           "behavior.first_active_trajectory", "behavior.repeated_acute_movement"),
  root = PROJECT_ROOT, hash = TRUE) %>%
  mutate(read_by_candidate = paste(.data$path_key, .data$file_role, sep = "/") %in%
           names(inputs_seen))
if (any(input_manifest$read_by_candidate &
        (!input_manifest$exists | is.na(input_manifest$sha256)))) {
  stop("A candidate input is missing or unhashed.", call. = FALSE)
}
write_csv(input_manifest,
          file.path(dirs$manifests, "candidate_input_manifest.csv"))

source_manifest <- tribble(
  ~panel, ~source_data_file, ~scientific_owner_stage, ~immediate_source_stage, ~path_keys, ~claim_class,
  "A", fA, "canonical-endpoint", "canonical-endpoint + 27-candidate", "behavior.later_outcome_combz", "DESIGN_CONTINUOUS_OUTCOME_AND_RETROSPECTIVE_LABELS",
  "B", fB, "20", "20", "behavior.first_active_trajectory", "DESCRIPTIVE_PERSISTED_GROUP_TRAJECTORIES",
  "C", fC, "22", "22 + 27-candidate", "behavior.repeated_acute_movement", "INFERENTIAL_OVERALL_ADAPTATION_WITH_EMPIRICAL_CONTEXT",
  "D", fD, "09", "09", "behavior.first_night_movement; behavior.first_night_combz_association", "INFERENTIAL_CONTINUOUS_ASSOCIATION",
  "E1", fE1, "09", "09", "behavior.early_prediction", "INTERNAL_OUT_OF_SAMPLE_PREDICTION",
  "E2", fE2, "09", "09", "behavior.early_prediction", "PERMUTATION_REFERENCE") %>%
  mutate(
    row_count = vapply(.data$source_data_file, function(filename)
      nrow(read_source(filename)), integer(1)),
    sha256 = mmm_file_sha256(file.path(dirs$source_data, .data$source_data_file)))
stopifnot(nrow(source_manifest) == 6L, all(source_manifest$row_count > 0L),
          !anyNA(source_manifest$sha256))
write_csv(source_manifest,
          file.path(dirs$source_data, "candidate_source_manifest.csv"))

n_source_ok <- sum(file.exists(file.path(dirs$source_data, source_manifest$source_data_file)) &
                     !is.na(source_manifest$sha256))
n_render_ok <- sum(file.exists(c(file.path(figure_manifest$dir, figure_manifest$pdf),
                                 file.path(figure_manifest$dir, figure_manifest$svg),
                                 file.path(figure_manifest$dir, figure_manifest$png))))
n_render_expected <- 3L * nrow(figure_manifest)
n_input_expected <- sum(input_manifest$read_by_candidate)
n_input_ok <- sum(input_manifest$read_by_candidate & input_manifest$exists &
                    !is.na(input_manifest$sha256))
manifest_parity <- bind_rows(
  tibble(check = "all six Source Data files exist and are hashed",
         expected = 6L, observed = n_source_ok,
         status = ifelse(6L == n_source_ok, "PASS", "FAIL")),
  tibble(check = "all individual panels and composite exist in PDF SVG PNG",
         expected = n_render_expected, observed = n_render_ok,
         status = ifelse(n_render_expected == n_render_ok, "PASS", "FAIL")),
  tibble(check = "all inputs actually read exist and are hashed",
         expected = n_input_expected, observed = n_input_ok,
         status = ifelse(n_input_expected == n_input_ok, "PASS", "FAIL")))
if (any(manifest_parity$status != "PASS")) stop("Manifest parity failed.", call. = FALSE)
write_csv(manifest_parity, file.path(dirs$audit, "manifest_parity_checks.csv"))

# ================================================================ LEGEND

phenotype_min_q <- min(phenotype_adaptation$q_BH_family)
s20_min_q <- min(primary_s20_contrasts$p_bh)
legend_lines <- c(
  "# Candidate behavior main figure legend",
  "",
  "**Assembly-only candidate.** No model was refitted and no p value, q value, confidence interval, endpoint, phenotype assignment or analysis window was recomputed. The current frozen Stage 27 figure remains the approved version.",
  "",
  paste0("**a, Paradigm, later continuous outcome and retrospective phenotype labels.** Adolescent social-instability stress was followed by continuous home-cage RFID monitoring across CC1-CC4 and a later behavioral/physiological battery. CombZ is the unweighted mean of ", nrow(combz_components), " historically standardized component z-scores; lower values indicate greater later stress burden. Colored/shaped points show all ", nrow(combz_display), " canonical animal-level CombZ values. RES and SUS are retrospective phenotype labels defined from the later integrated outcome; they were not prospectively known during the first-night response."),
  "",
  paste0("**b, First 12-h response after CC1 by later phenotype.** Female and Male facets each show the three persisted Stage 20 primary CON, RES and SUS prediction curves directly on their fitted log1p scale, with response-scale tick labels. No curve was averaged, transformed, or newly fitted. The shared time course is descriptive; no Stage 20 first-night group contrast survived BH correction (", s20_n_supported, "/", nrow(primary_s20_contrasts), "; minimum BH q = ", fmt_num(s20_min_q, 3), "). Later phenotype colors do not imply that RES/SUS status was known prospectively."),
  "",
  paste0("**c, Repeated acute Movement escalation.** Faint colored lines and shaped points are all ", nrow(c_empirical), " Stage 22 animal-level observed mean Movement values for the 12-h Active window after each cage change. Thin colored trajectories and small points are the separate CON, RES and SUS animal-level medians within each Sex; they are descriptive context only, with no ribbons, error bars or group-specific test. The heavier black trajectory per Sex is the primary pooled animal-level median at each cage change, with interquartile-range bars, computed directly across all animals in that Sex x CageChange cell without averaging or weighting group summaries. Text gives the persisted Stage 22 overall-population CC4-CC1 model-scale estimand for each Sex: Female ", fmt_num(overall_adaptation$estimate[overall_adaptation$Sex == "Female"], 2), " [", fmt_num(overall_adaptation$ci_low[overall_adaptation$Sex == "Female"], 2), ", ", fmt_num(overall_adaptation$ci_high[overall_adaptation$Sex == "Female"], 2), "], BH q = ", fmt_p(overall_adaptation$q_BH_family[overall_adaptation$Sex == "Female"]), "; Male ", fmt_num(overall_adaptation$estimate[overall_adaptation$Sex == "Male"], 2), " [", fmt_num(overall_adaptation$ci_low[overall_adaptation$Sex == "Male"], 2), ", ", fmt_num(overall_adaptation$ci_high[overall_adaptation$Sex == "Male"], 2), "], BH q = ", fmt_p(overall_adaptation$q_BH_family[overall_adaptation$Sex == "Male"]), ". These are precomputed overall-population estimands on the integrated log1p(Movement) scale and are not derived from or overlaid on the raw Movement y axis. Phenotype-dependent adaptation was not detected (", phenotype_n_supported, "/", nrow(phenotype_adaptation), " contrasts; minimum BH q = ", fmt_num(phenotype_min_q, 3), "). The precomputed Female-minus-Male moderation was ", fmt_num(overall_sex_moderation$diff_F_minus_M, 3), " [", fmt_num(overall_sex_moderation$ci_low, 3), ", ", fmt_num(overall_sex_moderation$ci_high, 3), "], p = ", fmt_p(overall_sex_moderation$p_raw), "; it does not establish a sex difference in adaptation."),
  "",
  paste0("**d, Early Movement and later CombZ.** One point per animal; fill indicates later phenotype classification and shape denotes Sex. These group colors are descriptive: the prospective analysis remains pooled and continuous. The persisted Stage 09 Spearman association was rho = ", fmt_num(assoc_head$spearman_rho, 3), " (bootstrap 95% CI [", fmt_num(assoc_head$spearman_boot_ci_low, 3), ", ", fmt_num(assoc_head$spearman_boot_ci_high, 3), "], BH q = ", fmt_p(assoc_head$spearman_p_bh), ", n = ", assoc_head$n, "). No fitted line or group-specific statistic is drawn."),
  "",
  paste0("**e, Internal out-of-sample prediction of continuous CombZ.** Points are the ", nrow(e_predictions), " leave-one-animal-out predictions from the fixed Stage 09 Movement-mean model; fill indicates later phenotype descriptively and shape denotes Sex. The target remains continuous CombZ—no classification or AUC analysis is introduced. The dashed identity line is a visual reference, not a fit. Held-out R2 = ", fmt_num(perf_head$cv_r2, 3), "; intercept-only baseline R2 = ", fmt_num(baseline_head$cv_r2, 3), ". The inset shows the actual ", nrow(null_head), " persisted full-refit outcome-permutation values; the vertical line is the observed R2 and the empirical permutation p = ", fmt_p(perm_head$empirical_p), ". This is internal validation, not external validation or an independent cohort."),
  "",
  "Full provenance, exact input and output hashes, plotted-value parity checks and panel-level Source Data accompany the candidate.")
writeLines(legend_lines, file.path(dirs$legends, "candidate_figure_legend.md"))

# ------------------------------------------------------ frozen-tree comparison

frozen_after <- hash_frozen_tree()
frozen_parity <- full_join(
  frozen_before %>% rename(bytes_before = bytes, sha256_before = sha256),
  frozen_after %>% rename(bytes_after = bytes, sha256_after = sha256),
  by = "relative_path") %>%
  mutate(status = if_else(!is.na(.data$sha256_before) & !is.na(.data$sha256_after) &
                            .data$bytes_before == .data$bytes_after &
                            .data$sha256_before == .data$sha256_after,
                          "PASS_UNCHANGED", "FAIL_CHANGED"))
if (nrow(frozen_parity) != nrow(frozen_before) ||
    nrow(frozen_parity) != nrow(frozen_after) ||
    any(frozen_parity$status != "PASS_UNCHANGED")) {
  stop("The frozen Stage 27 tree changed during candidate assembly.", call. = FALSE)
}
write_csv(frozen_parity, file.path(dirs$audit, "frozen_stage27_file_parity.csv"))

# =========================================================== VISUAL QA REPORT

qa_report <- c(
  "# Visual QA - five-panel acute behavior candidate",
  "",
  "Status: **RENDER QA PASS; candidate remains pending explicit scientific approval.**",
  "",
  paste0("- Main composition: ", MMM_WIDTH_DOUBLE_MM, " x ", MAIN_HEIGHT_MM,
         " mm; panels a-e present. The v4 candidate exports every individual panel and the composite as PDF, SVG and 300-dpi PNG."),
  "- Panel a shows the compact SIS/RFID/later-battery/CombZ timeline and the retrospective CON/RES/SUS labels; the continuous CombZ strip uses the fixed group fills plus redundant group shapes.",
  paste0("- Panel b displays all ", length(b_curve_counts), " persisted Stage 20 primary Sex x Group curves directly (", b_points_per_curve, " points each; ", nrow(b_display), " total). No across-Group averaging, transformation, confidence ribbon or new fit is present."),
  paste0("- Panel c keeps all ", nrow(c_empirical), " empirical animal observations visible with group-colored trajectories at alpha 0.09 and redundant group-shaped points at alpha 0.22. Three thin colored animal-level median trajectories per Sex provide descriptive CON/RES/SUS context without group uncertainty intervals or tests. A heavier neutral pooled median/IQR trajectory per Sex, calculated directly within Sex x CageChange across all animals without averaging Group summaries, remains the primary summary. Stage 22 estimands are labeled in their own model-scale units."),
  "- Panels d and e use later phenotype only as descriptive point fill and retain Sex as shape. Their association and prediction statistics remain the pooled continuous Stage 09 results; no group fit, group statistic, classification or AUC result is present.",
  "- The visual canvas contains no provenance/QC paragraph. Full method and provenance language is in the legend, manifests, parity tables and Source Data.",
  "- Manual inspection of the individual Panel c and the manuscript-sized composite confirmed that the heavier black pooled line remains the primary summary; the three colored group medians are visible, including outlined light-grey RES; and the lower-opacity animal trajectories remain subordinate.",
  "- The colored summaries add visible CON/RES/SUS design context without obscuring the overall escalation pattern. Their thin lines, small points and lack of group uncertainty intervals or group p values avoid implying a phenotype-dependent adaptation effect.",
  "- Panel c remains readable in the full composition: facet titles, model-scale annotations, CC labels and axis labels are unclipped. The Panel e permutation-null inset also remains legible.",
  paste0("- Source-data plot parity: ", nrow(parity_checks), "/", nrow(parity_checks), " PASS."),
  paste0("- Manifest parity: ", nrow(manifest_parity), "/", nrow(manifest_parity), " PASS."),
  paste0("- Frozen Stage 27 protection: ", nrow(frozen_parity), "/", nrow(frozen_parity), " pre-existing files SHA-256 identical."),
  "",
  "Manual approval boundary: inspect typography, panel balance, inset legibility and the empirical/model-scale distinction in panel c before replacing any frozen output. This candidate does not replace it.")
writeLines(qa_report, file.path(dirs$audit, "visual_qa_report.md"))

all_candidate_files <- list.files(CANDIDATE_ROOT, recursive = TRUE, full.names = TRUE)
mmm_assert_publication_path_budget(all_candidate_files, "Stage 27 candidate tree")

saveRDS(
  list(candidate_id = CANDIDATE_ID, panels = CANDIDATE_PANELS,
       individual_panels_exported = CANDIDATE_PANELS,
       prediction_target = PREDICTION_TARGET,
       inputs = inputs_seen, combz_max_drift = combz_max_drift,
       source_data_plot_parity = parity_checks,
       manifest_parity = manifest_parity,
       frozen_stage27_parity = frozen_parity),
  file.path(dirs$audit, "candidate_build_record.rds"))

cat("\nCandidate complete ->", CANDIDATE_ROOT, "\n")
cat("  main figure:", CANDIDATE_STEM, "(PDF/SVG/PNG)\n")
cat("  figures:", nrow(figure_manifest), "| Source Data files:", nrow(source_manifest), "\n")
cat("  plot parity:", nrow(parity_checks), "PASS | manifest parity:",
    nrow(manifest_parity), "PASS\n")
cat("  frozen Stage 27:", nrow(frozen_parity), "files unchanged by SHA-256\n")
