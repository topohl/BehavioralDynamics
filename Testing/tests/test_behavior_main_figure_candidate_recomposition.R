# Focused contracts for the assembly-only five-panel behavior figure candidate.
# Runs without sourcing the candidate assembler, so it cannot create outputs.

options(stringsAsFactors = FALSE)

find_repo <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "Analysis", "27_candidate_recompose_behavior_main_figure.R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not locate MMMSociability repository.", call. = FALSE)
    current <- parent
  }
}

repo <- find_repo()
source(file.path(repo, "Analysis", "_pipeline_setup.R"))
source_mmm_helper("mmm_publication_theme.R")
source_mmm_helper("project_paths.R")

candidate_file <- file.path(repo, "Analysis", "27_candidate_recompose_behavior_main_figure.R")
candidate_text <- paste(readLines(candidate_file, warn = FALSE), collapse = "\n")
candidate_expr <- parse(candidate_file)
stopifnot(length(candidate_expr) > 0L)
cat("[1] candidate assembler parses\n  ok  ", length(candidate_expr), " top-level expression(s)\n", sep = "")

required_keys <- c(
  "behavior.later_outcome_combz", "behavior.first_night_movement",
  "behavior.first_night_combz_association", "behavior.early_prediction",
  "behavior.first_active_trajectory", "behavior.repeated_acute_movement")
stopifnot(all(required_keys %in% mmm_path_keys()))
cat("[2] all six semantic input families are registered\n")

spec20 <- .mmm_spec("behavior.first_active_trajectory")
spec22 <- .mmm_spec("behavior.repeated_acute_movement")
stopifnot(
  identical(spec20$producer_stage, "20"),
  identical(spec22$producer_stage, "22"),
  identical(unname(spec20$files[["trajectory_predictions"]]),
            "first_active_trajectory_predictions.csv"),
  identical(unname(spec22$files[["empirical_auc"]]),
            "allcc_active_animal_empirical_auc.csv"),
  identical(unname(spec22$files[["adaptation_registry"]]),
            "allcc_active_adaptation_multiplicity_registry.csv"),
  identical(unname(spec22$files[["sex_moderation"]]),
            "allcc_active_sex_moderation_contrasts.csv"))
cat("  ok  Stage 20 and Stage 22 registry ownership and filenames\n")

# Calls that would violate assembly-only status. Names inside prose/comments do
# not count; the regex requires call syntax.
banned_calls <- c(
  "lm", "glm", "gam", "bam", "lmer", "glmer", "predict", "cor", "cor.test",
  "wilcox.test", "t.test", "kruskal.test", "anova", "p.adjust", "sample",
  "set.seed", "boot", "bootstrap", "emmeans", "contrast", "mvrnorm")
present <- vapply(banned_calls, function(fun) {
  grepl(paste0("(?<![[:alnum:]_.])", gsub("\\.", "\\\\.", fun), "\\s*\\("),
        candidate_text, perl = TRUE)
}, logical(1))
stopifnot(!any(present))
cat("[3] no fitting, prediction, resampling or inferential calls\n  ok  none of ",
    length(banned_calls), " banned calls present\n", sep = "")

stopifnot(
  grepl('CANDIDATE_PANELS <- c\\("A", "B", "C", "D", "E"\\)', candidate_text),
  grepl('CANDIDATE_ID <- "candidate_acute5_groups_v4"', candidate_text, fixed = TRUE),
  grepl('file.path\\(FROZEN_ROOT, "candidates", CANDIDATE_ID\\)', candidate_text),
  grepl('CANDIDATE_STEM <- "behavior_main_figure__five_panel_acute_groups_v4_candidate"', candidate_text),
  grepl('PREDICTION_TARGET <- "continuous_CombZ"', candidate_text, fixed = TRUE),
  !grepl('CANDIDATE_STEM <- "behavior_early_signal_and_prediction_main"', candidate_text),
  !grepl("file.rename\\s*\\(", candidate_text),
  !grepl("unlink\\s*\\(", candidate_text),
  !grepl("system2?\\s*\\(", candidate_text))
cat("[4] new output version is isolated below the frozen Stage 27 candidate subtree\n")

section_between <- function(text, start_marker, end_marker) {
  start <- regexpr(start_marker, text, fixed = TRUE)[1]
  end <- regexpr(end_marker, text, fixed = TRUE)[1]
  stopifnot(start > 0L, end > start)
  substring(text, start, end - 1L)
}
panel_b <- section_between(candidate_text,
                           "# ================================================================ PANEL B",
                           "# ================================================================ PANEL C")
panel_c <- section_between(candidate_text,
                           "# ================================================================ PANEL C",
                           "# ================================================================ PANEL D")
panel_d <- section_between(candidate_text,
                           "# ================================================================ PANEL D",
                           "# ================================================================ PANEL E")
panel_e <- section_between(candidate_text,
                           "# ================================================================ PANEL E",
                           "# =========================================================== SOURCE DATA")

# Panel B must be a direct view of the persisted six Sex x Group trajectories.
# Any future across-Group aggregation fails this test before rendering.
b_collapse_calls <- c("group_by\\s*\\(", "summari[sz]e\\s*\\(",
                      "weighted\\.mean\\s*\\(", "rowMeans\\s*\\(",
                      "(?<![[:alnum:]_.])mean\\s*\\(", "expm1\\s*\\(")
stopifnot(
  grepl("b_display <- s20_trajectory", panel_b, fixed = TRUE),
  grepl("colour = .data$Group", panel_b, fixed = TRUE),
  grepl("linetype = .data$Group", panel_b, fixed = TRUE),
  grepl(".data$fit", panel_b, fixed = TRUE),
  !any(vapply(b_collapse_calls, grepl, logical(1), x = panel_b, perl = TRUE)))
cat("[5] Panel B plots the persisted Stage 20 group curves directly; no Group collapse\n")

c_summary_code <- section_between(panel_c, "c_summary <-", "c_group_summary <-")
c_group_summary_code <- section_between(panel_c, "c_group_summary <-", "c_annotations <-")
c_annotation_code <- section_between(panel_c, "c_annotations <-", "pC <-")
c_plot_start <- regexpr("pC <-", panel_c, fixed = TRUE)[1]
stopifnot(c_plot_start > 0L)
c_plot_code <- substring(panel_c, c_plot_start)
c_forbidden_averages <- c("weighted\\.mean\\s*\\(", "rowMeans\\s*\\(",
                          "(?<![[:alnum:]_.])mean\\s*\\(")
stopifnot(
  grepl("colour = .data$Group", panel_c, fixed = TRUE),
  grepl("fill = .data$Group", panel_c, fixed = TRUE),
  grepl("shape = .data$Group", panel_c, fixed = TRUE),
  grepl("group_by(.data$Sex, .data$CageChangeIndex)", c_summary_code, fixed = TRUE),
  grepl("median(.data$observed_mean_Movement", c_summary_code, fixed = TRUE),
  grepl("quantile(.data$observed_mean_Movement", c_summary_code, fixed = TRUE),
  !grepl(".data$Group", c_summary_code, fixed = TRUE),
  !any(vapply(c_forbidden_averages, grepl, logical(1), x = c_summary_code, perl = TRUE)),
  grepl("group_by(.data$Sex, .data$Group, .data$CageChangeIndex)", c_group_summary_code, fixed = TRUE),
  grepl("median(.data$observed_mean_Movement", c_group_summary_code, fixed = TRUE),
  !any(vapply(c_forbidden_averages, grepl, logical(1), x = c_group_summary_code, perl = TRUE)),
  grepl("c_annotations <- overall_adaptation", c_annotation_code, fixed = TRUE),
  !grepl("median|q25|q75", c_annotation_code, perl = TRUE),
  grepl("geom_line(data = c_summary", c_plot_code, fixed = TRUE),
  grepl("geom_line(data = c_group_summary", c_plot_code, fixed = TRUE),
  grepl("geom_point(data = c_group_summary", c_plot_code, fixed = TRUE),
  grepl("linetype = .data$Group", c_plot_code, fixed = TRUE),
  grepl("family_key == \"A_OVERALL\"", candidate_text, fixed = TRUE),
  grepl("legend_only_phenotype_dependent_null", candidate_text, fixed = TRUE),
  grepl("legend_only_cross_sex_moderation", candidate_text, fixed = TRUE))
cat("[6] Panel C separates pooled median/IQR from three descriptive Group medians\n")

# D/E retain pooled Stage 09 results. Group appears only as descriptive fill;
# there is no Group filter, facet, statistic, model, classification or AUC call.
stopifnot(
  grepl("fill = .data$Group", panel_d, fixed = TRUE),
  grepl("fill = .data$Group", panel_e, fixed = TRUE),
  grepl("assoc_head$spearman_rho", panel_d, fixed = TRUE),
  grepl("perf_head$cv_r2", panel_e, fixed = TRUE),
  !grepl("filter\\s*\\([^)]*Group|facet_[a-z]+\\s*\\([^)]*Group", paste(panel_d, panel_e), perl = TRUE),
  !grepl("roc\\s*\\(|auc\\s*\\(|classif[a-z_]*\\s*\\(", panel_e, perl = TRUE))
cat("[7] Panels D/E use descriptive Group fill while inference remains pooled and continuous\n")

# Required provenance and parity artifacts are part of the source contract.
required_artifacts <- c(
  "source_data_plot_parity_checks.csv", "manifest_parity_checks.csv",
  "frozen_stage27_file_parity.csv", "candidate_input_manifest.csv",
  "candidate_source_manifest.csv", "figure_manifest.csv",
  "candidate_figure_legend.md", "visual_qa_report.md")
stopifnot(all(vapply(required_artifacts, function(x) grepl(x, candidate_text, fixed = TRUE), logical(1))))
cat("[8] source-data, manifest, frozen-tree and visual-QA contracts are present\n")

# If the live analysis root is available, verify that the newly registered
# inputs resolve to the actual canonical producers. This remains read-only.
if (dir.exists(mmm_project_root())) {
  live_paths <- c(
    mmm_path_get("behavior.first_active_trajectory", "trajectory_predictions"),
    mmm_path_get("behavior.first_active_trajectory", "primary_contrasts"),
    mmm_path_get("behavior.repeated_acute_movement", "empirical_auc"),
    mmm_path_get("behavior.repeated_acute_movement", "adaptation_registry"),
    mmm_path_get("behavior.repeated_acute_movement", "sex_moderation"))
  stopifnot(all(file.exists(live_paths)))
  normalized <- tolower(gsub("\\\\", "/", live_paths))
  stopifnot(!any(grepl("archive|quarantine|snapshot|superseded|release_bundle", normalized)))
  stage20 <- read.csv(live_paths[1], stringsAsFactors = FALSE, check.names = FALSE)
  stage20 <- stage20[stage20$variant == "primary", , drop = FALSE]
  stage22 <- read.csv(live_paths[3], stringsAsFactors = FALSE, check.names = FALSE)
  stopifnot(nrow(stage20) == 600L,
            setequal(unique(stage20$Group), c("CON", "RES", "SUS")),
            all(table(stage20$Sex, stage20$Group) == 100L),
            nrow(stage22) == 444L, !anyNA(stage22$Group),
            setequal(unique(stage22$Group), c("CON", "RES", "SUS")))
  cat("[9] live Stage 20/22 inputs resolve canonically with complete Group identity\n  ok  ",
      length(live_paths), " files; 600 Stage 20 and 444 Stage 22 rows\n", sep = "")
} else {
  cat("[9] live data root unavailable; registry-only checks completed\n")
}

# Once rendered, verify the candidate's persisted frozen-tree audit. Before the
# first render this remains a static safeguard check rather than creating data.
candidate_root <- file.path(mmm_publication_root(), "candidates", "candidate_acute5_groups_v4")
frozen_parity_file <- file.path(candidate_root, "audit", "frozen_stage27_file_parity.csv")
if (file.exists(frozen_parity_file)) {
  frozen_parity <- read.csv(frozen_parity_file, stringsAsFactors = FALSE)
  stopifnot(nrow(frozen_parity) > 0L,
            all(frozen_parity$status == "PASS_UNCHANGED"),
            all(frozen_parity$sha256_before == frozen_parity$sha256_after))
  cat("[10] rendered candidate frozen Stage 27 parity is byte-identical\n  ok  ",
      nrow(frozen_parity), " files PASS_UNCHANGED\n", sep = "")

  source_root <- file.path(candidate_root, "source_data")
  source_b <- read.csv(file.path(source_root, "source_panel_b_stage20_group_curves.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  plotted_b <- source_b[source_b$row_role == "plotted_persisted_group_prediction", , drop = FALSE]
  source_c <- read.csv(file.path(source_root, "source_panel_c_repeated_acute_groups.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  plotted_c <- source_c[source_c$row_role == "plotted_animal_empirical_point", , drop = FALSE]
  summary_c <- source_c[source_c$row_role == "plotted_descriptive_pooled_summary", , drop = FALSE]
  group_summary_c <- source_c[source_c$row_role == "plotted_descriptive_group_summary", , drop = FALSE]
  phenotype_c <- source_c[source_c$row_role == "legend_only_phenotype_dependent_null", , drop = FALSE]
  source_d <- read.csv(file.path(source_root, "source_panel_d_movement_combz_groups.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  plotted_d <- source_d[source_d$row_role == "plotted_animal_point", , drop = FALSE]
  source_e <- read.csv(file.path(source_root, "source_panel_e1_loao_predictions_groups.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  plotted_e <- source_e[source_e$row_role == "plotted_heldout_prediction", , drop = FALSE]
  stopifnot(
    nrow(plotted_b) == 600L,
    setequal(unique(plotted_b$Group), c("CON", "RES", "SUS")),
    all(table(plotted_b$Sex, plotted_b$Group) == 100L),
    nrow(plotted_c) == 444L, !anyNA(plotted_c$Group),
    setequal(unique(plotted_c$Group), c("CON", "RES", "SUS")),
    nrow(summary_c) == 8L,
    all(summary_c$quantity_class == "descriptive_observed"),
    all(is.na(summary_c$Group)),
    all(grepl("all animal-level observations within Sex x CageChange",
              summary_c$pooling_definition, fixed = TRUE)),
    all(grepl("Group is descriptive metadata and is not a weighting variable",
              summary_c$pooling_definition, fixed = TRUE)),
    nrow(group_summary_c) == 24L,
    setequal(unique(group_summary_c$Group), c("CON", "RES", "SUS")),
    all(table(group_summary_c$Sex, group_summary_c$Group) == 4L),
    all(group_summary_c$quantity_class == "descriptive_observed"),
    all(group_summary_c$summary_type == "group-specific animal-level median"),
    all(grepl("descriptive display only; no group-specific inference",
              group_summary_c$pooling_definition, fixed = TRUE)),
    nrow(phenotype_c) == 6L,
    sum(phenotype_c$q_BH_family <= 0.05) == 0L,
    round(min(phenotype_c$q_BH_family), 3) == 0.626,
    nrow(plotted_d) == 111L, !anyNA(plotted_d$Group),
    nrow(plotted_e) == 111L, !anyNA(plotted_e$Group))
  for (sex_value in c("Female", "Male")) {
    for (cc_value in 1:4) {
      empirical_cell <- plotted_c[
        plotted_c$Sex == sex_value & plotted_c$CageChangeIndex == cc_value, , drop = FALSE]
      summary_cell <- summary_c[
        summary_c$Sex == sex_value & summary_c$CageChangeIndex == cc_value, , drop = FALSE]
      values <- empirical_cell$observed_mean_Movement
      stopifnot(
        nrow(summary_cell) == 1L,
        summary_cell$n_animals == sum(is.finite(values)),
        abs(summary_cell$median - median(values, na.rm = TRUE)) <= 1e-12,
        abs(summary_cell$q25 - unname(quantile(values, 0.25, na.rm = TRUE))) <= 1e-12,
        abs(summary_cell$q75 - unname(quantile(values, 0.75, na.rm = TRUE))) <= 1e-12)
    }
  }
  for (sex_value in c("Female", "Male")) {
    for (group_value in c("CON", "RES", "SUS")) {
      for (cc_value in 1:4) {
        empirical_cell <- plotted_c[
          plotted_c$Sex == sex_value & plotted_c$Group == group_value &
            plotted_c$CageChangeIndex == cc_value, , drop = FALSE]
        summary_cell <- group_summary_c[
          group_summary_c$Sex == sex_value & group_summary_c$Group == group_value &
            group_summary_c$CageChangeIndex == cc_value, , drop = FALSE]
        values <- empirical_cell$observed_mean_Movement
        stopifnot(
          nrow(summary_cell) == 1L,
          summary_cell$n_animals == sum(is.finite(values)),
          abs(summary_cell$median - median(values, na.rm = TRUE)) <= 1e-12)
      }
    }
  }
  if (exists("stage20", inherits = FALSE)) {
    stage20_order <- order(stage20$Sex, stage20$Group, stage20$TimeAxis)
    plotted_b_order <- order(plotted_b$Sex, plotted_b$Group, plotted_b$TimeAxis)
    stopifnot(
      identical(stage20$Sex[stage20_order], plotted_b$Sex[plotted_b_order]),
      identical(stage20$Group[stage20_order], plotted_b$Group[plotted_b_order]),
      max(abs(stage20$TimeAxis[stage20_order] - plotted_b$TimeAxis[plotted_b_order])) <= 1e-12,
      max(abs(stage20$fit[stage20_order] - plotted_b$fit[plotted_b_order])) <= 1e-12)
  }
  figure_manifest <- read.csv(file.path(candidate_root, "manifests", "figure_manifest.csv"),
                              stringsAsFactors = FALSE)
  stopifnot(nrow(figure_manifest) == 6L,
            setequal(figure_manifest$panel, c("A", "B", "C", "D", "E", "A-B-C-D-E")))
  cat("[11] Panel C Source Data reproduces pooled and Group medians directly from animals\n",
      "  ok  444 Group-complete rows; 8 pooled and 24 Group summaries\n", sep = "")
  cat("[12] v4 exports all five individual panels and the composite\n")
} else {
  stopifnot(grepl("hash_frozen_tree", candidate_text, fixed = TRUE),
            grepl("PASS_UNCHANGED", candidate_text, fixed = TRUE))
  cat("[10] pre-render frozen-tree hashing safeguard is present\n")
}

cat("\nBehavior main figure candidate recomposition contracts: PASS\n")
