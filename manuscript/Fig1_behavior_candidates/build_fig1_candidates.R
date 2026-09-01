# ============================================================================
# Stage candidate panels for a revised E9 Figure 1 behavioral story.
#
# This script does not refit canonical statistical models. It reads/copies
# existing manuscript/pipeline outputs and creates two simple manuscript-facing
# plots directly from Stage 16 source data:
#   01 outcome definition
#   05 held-out predicted vs observed CombZ
#
# Core staging deliberately excludes Stage 11/12-derived systems panels until
# their active/inactive classifier is fixed and those stages are rerun.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tibble)
})

# -----------------------------------------------------------------------------
# Locate repository and shared path helpers
# -----------------------------------------------------------------------------

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) NA_character_
)
script_dir <- if (is.na(script_path) || !nzchar(script_path)) getwd() else dirname(script_path)
repo_candidates <- unique(c(
  normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE),
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
))
repo_root <- repo_candidates[file.exists(file.path(repo_candidates, "Analysis", "_pipeline_setup.R"))][1]
if (is.na(repo_root)) stop("Could not locate repository root containing Analysis/_pipeline_setup.R", call. = FALSE)
source(file.path(repo_root, "Analysis", "_pipeline_setup.R"))

project_root <- Sys.getenv(
  "MMM_BEHAVIOR_PROJECT_ROOT",
  unset = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
)

stage09_dir <- behavior_stage_dir(project_root, "09", "early_prediction", resolution = "10min_based")
stage03_dir <- behavior_stage_dir(project_root, "03", "movement_phase_stats", resolution = "10min_based")
stage14_dir <- file.path(project_root, "analysis_ready", "12_systems_neuroscience_summary", "5min_based")
stage16_dir <- behavior_manuscript_dir(project_root, "behavior")

out_root <- file.path(repo_root, "manuscript", "Fig1_behavior_candidates", "rendered")
out_core <- file.path(out_root, "core")
dir.create(out_core, recursive = TRUE, showWarnings = FALSE)

group_levels <- c("CON", "RES", "SUS")
group_colors <- c("CON" = "#3d3b6e", "RES" = "#C6C3BB", "SUS" = "#e63947")

candidate_status <- tibble(
  panel_id = character(),
  status = character(),
  source = character(),
  destination = character(),
  note = character()
)

record_status <- function(panel_id, status, source = NA_character_, destination = NA_character_, note = NA_character_) {
  candidate_status <<- bind_rows(candidate_status, tibble(
    panel_id = panel_id,
    status = status,
    source = source,
    destination = destination,
    note = note
  ))
  invisible(NULL)
}

save_pair <- function(plot, target_base, width_mm, height_mm) {
  dir.create(dirname(target_base), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(paste0(target_base, ".svg"), plot, width = width_mm, height = height_mm, units = "mm")
  ggplot2::ggsave(paste0(target_base, ".pdf"), plot, width = width_mm, height = height_mm, units = "mm")
  invisible(target_base)
}

copy_pair <- function(panel_id, source_base, target_stem, note = "") {
  copied <- character()
  missing <- character()
  for (ext in c("svg", "pdf")) {
    src <- paste0(source_base, ".", ext)
    dst <- file.path(out_core, paste0(target_stem, ".", ext))
    if (file.exists(src)) {
      ok <- file.copy(src, dst, overwrite = TRUE)
      if (!isTRUE(ok)) stop("Could not copy ", src, " -> ", dst, call. = FALSE)
      copied <- c(copied, dst)
    } else {
      missing <- c(missing, src)
    }
  }
  if (length(copied) > 0) {
    record_status(panel_id, if (length(missing) == 0) "copied_svg_pdf" else "copied_partial",
                  source_base, paste(copied, collapse = " | "),
                  paste(c(note, if (length(missing) > 0) paste("Missing:", paste(missing, collapse = " | ")) else NULL), collapse = " "))
  } else {
    record_status(panel_id, "missing_upstream", source_base, NA_character_,
                  paste(c(note, paste("Missing:", paste(missing, collapse = " | "))), collapse = " "))
  }
  invisible(copied)
}

# -----------------------------------------------------------------------------
# Guardrails: canonical Stage 16 inputs must exist
# -----------------------------------------------------------------------------

animal_source_path <- file.path(stage16_dir, "animal_level_source_data.csv")
prediction_source_path <- file.path(stage16_dir, "prediction_source_data.csv")
stage09_window_contract <- file.path(stage09_dir, "tables", "early_window_contract_summary.csv")
stage09_endpoint_coverage <- file.path(stage09_dir, "tables", "endpoint_coverage_by_animal.csv")

missing_required <- c(animal_source_path, prediction_source_path, stage09_window_contract, stage09_endpoint_coverage)
missing_required <- missing_required[!file.exists(missing_required)]
if (length(missing_required) > 0) {
  stop(
    "Canonical Stage 09/16 inputs are missing. Rerun Stage 09 and Stage 16 before staging Figure 1 candidates:\n- ",
    paste(missing_required, collapse = "\n- "),
    call. = FALSE
  )
}

animal_source <- readr::read_csv(animal_source_path, show_col_types = FALSE)
pred_source <- readr::read_csv(prediction_source_path, show_col_types = FALSE)
endpoint_coverage <- readr::read_csv(stage09_endpoint_coverage, show_col_types = FALSE)

required_animal_cols <- c("AnimalID", "Sex", "Group", "CombZ")
missing_cols <- setdiff(required_animal_cols, names(animal_source))
if (length(missing_cols) > 0) stop("Stage 16 animal source missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
required_pred_cols <- c("AnimalID", "observed_CombZ", "predicted_CombZ", "model_id", "Sex", "Group")
missing_pred_cols <- setdiff(required_pred_cols, names(pred_source))
if (length(missing_pred_cols) > 0) stop("Stage 16 prediction source missing: ", paste(missing_pred_cols, collapse = ", "), call. = FALSE)

if (anyDuplicated(animal_source$AnimalID)) stop("Stage 16 animal source is not one row per animal.", call. = FALSE)
if ("has_finite_outcome" %in% names(endpoint_coverage) && any(!endpoint_coverage$has_finite_outcome)) {
  stop("Stage 09 endpoint coverage is incomplete; do not stage the headline Figure 1 prediction panels.", call. = FALSE)
}
if (!setequal(unique(pred_source$model_id), c("movement_mean", "primary_behavior_family"))) {
  stop("Stage 16 prediction source does not contain exactly the two canonical behavior-only models.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 01. Outcome definition: descriptive only, no circular inferential brackets
# -----------------------------------------------------------------------------

outcome_plot_dat <- animal_source %>%
  mutate(
    Group = factor(as.character(Group), levels = group_levels),
    Sex = factor(as.character(Sex), levels = c("Female", "Male"))
  ) %>%
  filter(is.finite(CombZ), !is.na(Group), !is.na(Sex))

p_outcome <- ggplot(outcome_plot_dat, aes(Group, CombZ, colour = Group)) +
  geom_hline(yintercept = 0, linewidth = 0.25, linetype = "dashed", colour = "grey65") +
  geom_jitter(width = 0.10, height = 0, size = 1.5, alpha = 0.72) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.42, linewidth = 0.45, colour = "grey15") +
  facet_wrap(~Sex, nrow = 1) +
  scale_colour_manual(values = group_colors, drop = FALSE) +
  labs(
    title = "Later integrated stress outcome",
    subtitle = "CON/RES/SUS shown as phenotype context; no inferential group brackets",
    x = NULL,
    y = "Integrated resilience / CombZ (z)"
  ) +
  theme_classic(base_size = 7) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

outcome_base <- file.path(out_core, "01_outcome_definition")
save_pair(p_outcome, outcome_base, 105, 72)
record_status("outcome_definition", "generated_from_stage16", animal_source_path, outcome_base,
              paste0("n=", n_distinct(outcome_plot_dat$AnimalID), "; descriptive phenotype-definition panel."))

# -----------------------------------------------------------------------------
# 02-04. Existing canonical / direct-data panels
# -----------------------------------------------------------------------------

copy_pair(
  "first_active_movement",
  file.path(stage14_dir, "figures", "publication_panels", "Fig_first_active_movement_trajectory_by_group_sex"),
  "02_first_active_movement_trajectory",
  "First active-phase raw movement trajectory after CC1; rerun Stage 14 after current Stage 01/09 corrections before final use."
)

copy_pair(
  "primary_feature_associations",
  file.path(stage09_dir, "figures", "primary_movement_entropyacf1_vs_combz"),
  "03_primary_feature_associations",
  "Fixed a priori Stage 09 feature family."
)

copy_pair(
  "primary_prediction_cv",
  file.path(stage09_dir, "figures", "behavior_only_repeated_cv_ladder"),
  "04_primary_prediction_cv",
  "Use as cross-validation evidence; repeated-CV intervals are resampling quantiles, not confidence intervals."
)

# -----------------------------------------------------------------------------
# 05. Canonical held-out predictions, generated from Stage 16 source data
# -----------------------------------------------------------------------------

model_labels <- c(
  "movement_mean" = "Movement mean",
  "primary_behavior_family" = "Movement + RMSSD + entropy persistence"
)

pred_plot_dat <- pred_source %>%
  mutate(
    Group = factor(as.character(Group), levels = group_levels),
    Sex = factor(as.character(Sex), levels = c("Female", "Male")),
    model_id = factor(model_id, levels = names(model_labels), labels = unname(model_labels))
  ) %>%
  filter(is.finite(observed_CombZ), is.finite(predicted_CombZ), !is.na(model_id))

lims <- range(c(pred_plot_dat$observed_CombZ, pred_plot_dat$predicted_CombZ), na.rm = TRUE)
if (!all(is.finite(lims)) || diff(lims) <= 0) lims <- c(-3, 3)

p_pred <- ggplot(pred_plot_dat, aes(observed_CombZ, predicted_CombZ, colour = Group)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.3, linetype = "dashed", colour = "grey55") +
  geom_point(size = 1.45, alpha = 0.75) +
  facet_grid(Sex ~ model_id) +
  coord_equal(xlim = lims, ylim = lims) +
  scale_colour_manual(values = group_colors, drop = FALSE) +
  labs(
    title = "Held-out prediction of later stress burden",
    subtitle = "Leave-one-animal-out predictions; internal validation",
    x = "Observed CombZ",
    y = "Held-out predicted CombZ"
  ) +
  theme_classic(base_size = 7) +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

pred_base <- file.path(out_core, "05_heldout_prediction")
save_pair(p_pred, pred_base, 145, 105)
record_status("heldout_prediction", "generated_from_stage16", prediction_source_path, pred_base,
              paste0("n animals=", n_distinct(pred_plot_dat$AnimalID), "; two fixed behavior-only models."))

# -----------------------------------------------------------------------------
# 06. Conservative longitudinal phenotype panel from Stage 03
# -----------------------------------------------------------------------------

copy_pair(
  "longitudinal_movement",
  file.path(stage03_dir, "figures", "publication_panels", "Fig18c_cage_change_phase_mean_movement_corrected_stats"),
  "06_longitudinal_movement",
  "Secondary raw-movement phenotype characterization; Stage 09 remains the primary prospective analysis."
)

# -----------------------------------------------------------------------------
# Record optional systems candidates without copying them into the core set.
# -----------------------------------------------------------------------------

optional_sources <- tribble(
  ~panel_id, ~source_base, ~status, ~note,
  "cc1_systems_signature", file.path(stage14_dir, "figures", "publication_panels", "Fig_sis_CC1_first_active_domain_heatmap"), "optional_after_rerun", "Useful alternative if the early multiscale signature adds interpretable information beyond movement.",
  "social_spatial_organization", file.path(stage14_dir, "figures", "publication_panels", "Fig_sis_social_spatial_organization"), "optional_after_rerun", "Potentially more manipulation-proximal than another locomotor panel; promote only if robust.",
  "repeated_adaptation", file.path(stage14_dir, "figures", "publication_panels", "Fig_sis_repeated_active_phase_adaptation"), "blocked_pending_phase_classifier_fix_and_rerun", "Do not stage for manuscript use until Stage 11/12/shared phase classification is fixed and affected outputs are regenerated.",
  "active_inactive_systems", file.path(stage14_dir, "figures", "publication_panels", "Fig_sis_active_inactive_domain_heatmap"), "blocked_pending_phase_classifier_fix_and_rerun", "Contains phase-specific domains; do not stage until the classifier issue is fixed and outputs are regenerated.",
  "rest_state_architecture", file.path(stage14_dir, "figures", "publication_panels", "Fig_sis_rest_or_state_architecture"), "blocked_pending_phase_classifier_fix_and_rerun", "Rest/quiescence interpretation is especially sensitive to the classifier issue."
)

for (i in seq_len(nrow(optional_sources))) {
  x <- optional_sources[i, ]
  record_status(
    x$panel_id,
    x$status,
    x$source_base,
    NA_character_,
    paste0(x$note, " Source currently ", ifelse(file.exists(paste0(x$source_base, ".svg")), "exists", "is missing"), ".")
  )
}

# -----------------------------------------------------------------------------
# Audit output
# -----------------------------------------------------------------------------

candidate_status <- candidate_status %>%
  mutate(generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
readr::write_csv(candidate_status, file.path(out_root, "staging_status.csv"))

message("Figure 1 candidate staging complete.")
message("Core figures: ", out_core)
message("Audit: ", file.path(out_root, "staging_status.csv"))
