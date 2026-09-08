# ================================================================
# Behavior main figure assembly - ASSEMBLY ONLY, NO ANALYSIS
# MMMSociability
# ================================================================
# Reads the frozen canonical outputs that own the five behaviour questions and
# produces the publication tree: the assembled main figure, individual panels,
# alternative and extended-data candidates, per-panel Source Data, a manuscript
# key-results table, a legend draft, provenance manifests and a claim trace.
#
# HARD CONTRACT, asserted here and enforced from outside by
# Testing/tests/test_behavior_main_figure_contracts.R:
#   * no statistical model is fitted
#   * no p or q value is computed, adjusted or re-derived
#   * no confidence interval is estimated from data
#   * no multiplicity family is redefined
#   * CombZ is never recomputed and RES/SUS is never redefined
#   * the first-active-phase window is never reselected
#   * no scientific number is changed
# The only arithmetic performed is descriptive summarisation of the values being
# plotted, and unit relabelling. Both are declared in the manifests.
#
# NO PATH LITERAL LIVES IN THIS FILE. Every input is requested by semantic key
# from Functions/project_paths.R and every output directory comes from the
# configured publication root, so relocating either tree does not touch this
# source. Filenames are semantic; the manuscript figure number is assigned
# during whole-manuscript assembly.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
  library(ggplot2); library(stringr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("mmm_publication_theme.R")
source_mmm_helper("gamm_publication_style_helpers.R")
source_mmm_helper("project_paths.R")
source_mmm_helper("behavior_main_figure_helpers.R")
source_mmm_helper("behavior_main_figure_docs.R")

STAGE <- "27"
project_root <- mmm_project_root()
pub_root <- mmm_publication_root()

dirs <- setNames(
  lapply(names(MMM_PUBLICATION_SUBDIRS), function(k)
    mmm_publication_dir(k, create = TRUE, publication_root = pub_root)),
  names(MMM_PUBLICATION_SUBDIRS))
dirs$root <- pub_root
ensure_dir(pub_root)

cat("Stage 27 | behavior main figure assembly (no analysis)\n")
cat("  analysis-data root :", project_root, "\n")
cat("  publication root   :", pub_root, "\n")

# ------------------------------------------------------- guarded input reader
#
# Refuses to read from a superseded, quarantined or bundled copy. This matters
# concretely: at least three same-schema files named
# primary_prediction_performance.csv exist elsewhere on the share, including one
# in an erroneous 24 h snapshot directory and one holding the 5-min numbers.

FORBIDDEN_SOURCE_SEGMENTS <- c(
  "_quarantine", "quarantine_legacy", "_archive", "snapshot", "/releases/",
  ".old", "_pre_hmm_identity_fix", "_erroneous")

inputs_seen <- list()

rd <- function(key, role, expect_rows = NULL) {
  path <- mmm_path_get(key, file = role, root = project_root)
  hit <- FORBIDDEN_SOURCE_SEGMENTS[
    vapply(FORBIDDEN_SOURCE_SEGMENTS, function(s)
      grepl(s, path, fixed = TRUE), logical(1))]
  if (length(hit) > 0L) {
    stop("Refusing to read a superseded or bundled copy for '", key, "/", role,
         "'. Offending path segment(s): ", paste(hit, collapse = ", "),
         "\n  ", path, call. = FALSE)
  }
  x <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  if (!is.null(expect_rows) && nrow(x) != expect_rows) {
    stop("Input contract violation for '", key, "/", role, "': expected ",
         expect_rows, " rows, got ", nrow(x), ".\n  ", path, call. = FALSE)
  }
  inputs_seen[[paste(key, role, sep = "/")]] <<- path
  x
}

# ------------------------------------------------------------- read canonical
cat("  reading canonical inputs ...\n")

animal      <- rd("behavior.combz_definition", "animal_level", 111)
frozen_res  <- rd("behavior.combz_definition", "primary_results")
s16_valid   <- rd("behavior.combz_definition", "validation")
s09_input   <- rd("behavior.first_night_movement", "model_input", 111)
window_ctr  <- rd("behavior.first_night_movement", "window_contract", 1)
grp_summary <- rd("behavior.first_night_movement", "group_summary")
grp_desc    <- rd("behavior.first_night_movement", "group_contrasts_descriptive")
assoc       <- rd("behavior.first_night_combz_association", "associations", 3)
sex_int     <- rd("behavior.first_night_combz_association", "sex_interactions", 3)
perf        <- rd("behavior.early_prediction", "performance", 5)
perm        <- rd("behavior.early_prediction", "permutation", 2)
heldout     <- rd("behavior.early_prediction_heldout", "predictions", 222)
domain      <- rd("behavior.rfid_domain_summary", "group_contrasts", 30)
comp_tbl    <- rd("behavior.combz_components", "component_scores")
gamm_claims <- rd("gamm.manuscript_outputs", "claim_trace")

# ---------------------------------------------------------- schema contracts
cat("  validating schemas ...\n")

bmf_assert_schema(
  animal, "Stage 16 animal-level source data",
  required = c("AnimalID", "Sex", "Group", "CombZ", "Movement_mean",
               "Movement_rmssd", "Entropy_acf1"),
  keys = "AnimalID",
  allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
  finite = c("CombZ", "Movement_mean"), n_rows = 111)

bmf_assert_schema(
  domain, "Stage 14 first-active domain contrasts",
  required = c("Sex", "contrast", "Domain", "hedges_g", "q", "family_id",
               "n_tests_in_family", "bin_level", "interpretation_guard"),
  keys = c("Sex", "contrast", "Domain"),
  allowed = list(Sex = BMF_SEX_LEVELS, contrast = BMF_CONTRAST_LEVELS),
  finite = "hedges_g", n_rows = 30)

bmf_assert_schema(
  heldout, "Stage 16 held-out predictions",
  required = c("AnimalID", "observed_CombZ", "predicted_CombZ", "model_id",
               "validation_scheme", "Sex", "Group"),
  keys = c("AnimalID", "model_id"),
  allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
  finite = c("observed_CombZ", "predicted_CombZ"), n_rows = 222)

bmf_assert_schema(
  perf, "Stage 09 prediction performance",
  required = c("model_id", "model_label", "reporting_role", "predictors",
               "n_animals", "validation_scheme", "cv_r2", "permutation_p",
               "repeated_cv_mean_r2", "cv_r2_q025", "cv_r2_q975",
               "interval_type"), keys = "model_id")

bmf_assert_schema(
  perm, "Stage 09 permutation reference",
  required = c("model", "model_label", "observed_statistic_name",
               "observed_statistic", "null_median", "null_q025", "null_q975",
               "empirical_p", "n_permutations", "cv_scheme", "seed"),
  keys = "model")

bmf_assert_schema(
  assoc, "Stage 09 primary feature associations",
  required = c("feature", "spearman_rho", "spearman_p", "spearman_p_bh",
               "spearman_boot_ci_low", "spearman_boot_ci_high"),
  keys = "feature", n_rows = 3)

# --- window identity must be exactly the frozen Stage 09 window -------------
WINDOW_EXPECTED <- paste(
  "fixed clock window: first Active phase block after first cage change,",
  "18:30 inclusive to 06:30 exclusive")
if (!identical(as.character(window_ctr$primary_window_definition[1]),
               WINDOW_EXPECTED)) {
  stop("Stage 09 first-active window identity changed. Expected:\n  ",
       WINDOW_EXPECTED, "\nFound:\n  ",
       window_ctr$primary_window_definition[1],
       "\nRefusing to assemble a figure against a redefined window.",
       call. = FALSE)
}
stopifnot(window_ctr$bin_size_min[1] == 10,
          window_ctr$target_window_hours[1] == 12,
          window_ctr$expected_target_slots_per_animal[1] == 72,
          window_ctr$first_cage_change[1] == "CC1",
          window_ctr$n_inactive_rows_selected[1] == 0,
          window_ctr$n_animals[1] == 111)

# --- Stage 16 plotted values must equal the Stage 09 analysis frame ---------
# CSV text round-trip loses the last bit or two of a double, so the tolerance is
# 1e-12 rather than 0. Source Data versus plotted values IS checked at 0, later,
# because those come from one in-memory object.
TOL_ROUNDTRIP <- 1e-12
xcheck <- animal %>%
  mutate(.key = as.character(.data$AnimalID)) %>%
  inner_join(s09_input %>% mutate(.key = as.character(.data$AnimalNum)) %>%
               select(".key", s09_movement = "Movement_mean",
                      s09_rmssd = "Movement_rmssd", s09_acf1 = "Entropy_acf1",
                      s09_outcome = "outcome"), by = ".key")
if (nrow(xcheck) != 111) {
  stop("Stage 16 and Stage 09 animal sets do not match: joined ", nrow(xcheck),
       " of 111.", call. = FALSE)
}
drift <- c(
  Movement_mean = max(abs(xcheck$Movement_mean - xcheck$s09_movement)),
  Movement_rmssd = max(abs(xcheck$Movement_rmssd - xcheck$s09_rmssd)),
  Entropy_acf1 = max(abs(xcheck$Entropy_acf1 - xcheck$s09_acf1)),
  CombZ = max(abs(xcheck$CombZ - xcheck$s09_outcome)))
if (any(drift > TOL_ROUNDTRIP)) {
  stop("Stage 16 manuscript values disagree with the Stage 09 analysis frame ",
       "beyond CSV round-trip tolerance:\n",
       paste0("  ", names(drift), ": ", format(drift), collapse = "\n"),
       call. = FALSE)
}
cat("    Stage 16 vs Stage 09 agreement: max drift ",
    format(max(drift)), " (tolerance ", format(TOL_ROUNDTRIP), ")\n", sep = "")

# --- the held-out predictions must be the same animals and observed values --
ho_primary <- heldout %>% filter(.data$model_id == "movement_mean")
stopifnot(nrow(ho_primary) == 111)
ho_check <- ho_primary %>%
  mutate(.key = as.character(.data$AnimalID)) %>%
  inner_join(animal %>% mutate(.key = as.character(.data$AnimalID)) %>%
               select(".key", ref_CombZ = "CombZ"), by = ".key")
stopifnot(nrow(ho_check) == 111,
          max(abs(ho_check$observed_CombZ - ho_check$ref_CombZ)) == 0)

# ------------------------------------------------------------- frozen facts
#
# Every scientific number the figure or legend states is pulled from a canonical
# table here, once, and then only formatted. Nothing below re-derives a value.

pick <- function(df, col, where_col, where_val) {
  hit <- df[[col]][as.character(df[[where_col]]) == where_val]
  if (length(hit) != 1L) {
    stop("Expected exactly one '", col, "' for ", where_col, " == ", where_val,
         "; got ", length(hit), ".", call. = FALSE)
  }
  hit
}

HEADLINE_MODEL <- "movement_mean"
HEADLINE_FEATURE <- "Movement_mean"

# The headline model is fixed by a Stage 16 contract gate, not by us.
gate <- s16_valid %>% filter(str_detect(.data$check_id, "movement_mean_headline"))
if (nrow(gate) == 1L && !identical(toupper(as.character(gate$status[1])), "PASS")) {
  stop("Stage 16 headline-model contract gate is not PASS; refusing to label ",
       HEADLINE_MODEL, " as the headline prediction.", call. = FALSE)
}

assoc_row <- assoc %>% filter(.data$feature == HEADLINE_FEATURE)
stopifnot(nrow(assoc_row) == 1L)

perf_head <- perf %>% filter(.data$model_id == HEADLINE_MODEL)
perm_head <- perm %>% filter(.data$model == HEADLINE_MODEL)
stopifnot(nrow(perf_head) == 1L, nrow(perm_head) == 1L)
baseline_r2 <- pick(perf, "cv_r2", "model_id", "mean_only")

group_counts <- table(factor(as.character(animal$Group), levels = BMF_GROUP_LEVELS))
sex_counts <- table(factor(as.character(animal$Sex), levels = BMF_SEX_LEVELS))

combz_components <- intersect(
  c("NOR", "sucrose_pref", "weight_dev", "delta_cort", "adrenal_weight",
    "spleen_weight"), names(comp_tbl))
if (length(combz_components) < 2L) {
  stop("Could not recover the CombZ component names from the canonical ",
       "component table; refusing to draw a schematic with an invented ",
       "component list.", call. = FALSE)
}
COMBZ_COMPONENT_LABELS <- c(
  NOR = "Novel object\nrecognition", sucrose_pref = "Sucrose\npreference",
  weight_dev = "Body-weight\ndeviation", delta_cort = "Change in\ncorticosterone",
  adrenal_weight = "Adrenal\nweight", spleen_weight = "Spleen\nweight")

fmt_num <- function(x, d = 3) formatC(as.numeric(x), format = "f", digits = d)
fmt_ci <- function(lo, hi, d = 3) paste0("[", fmt_num(lo, d), ", ", fmt_num(hi, d), "]")

facts <- list(
  n_total = nrow(animal),
  n_female = as.integer(sex_counts[["Female"]]),
  n_male = as.integer(sex_counts[["Male"]]),
  group_counts = group_counts,
  combz_components = unname(COMBZ_COMPONENT_LABELS[combz_components]),
  # Two label vectors on purpose: the schematic boxes need the line breaks, the
  # legend prose must not inherit them.
  combz_components_plain = unname(gsub("\n", " ",
                                       COMBZ_COMPONENT_LABELS[combz_components],
                                       fixed = TRUE)),
  combz_component_ids = combz_components,
  window_definition = paste0(
    as.character(window_ctr$primary_window_definition[1]), "; ",
    window_ctr$target_window_hours[1], " h at ",
    window_ctr$bin_size_min[1], "-min resolution (",
    window_ctr$expected_target_slots_per_animal[1], " expected slots)"),
  n_complete_window = as.integer(window_ctr$n_animals_complete_72_bins[1]),
  movement_unit = "the count of RFID position transitions per 10-min bin, averaged over the observed window bins",
  assoc_rho = fmt_num(assoc_row$spearman_rho, 3),
  assoc_ci = fmt_ci(assoc_row$spearman_boot_ci_low, assoc_row$spearman_boot_ci_high),
  assoc_q = formatC(assoc_row$spearman_p_bh, format = "e", digits = 2),
  pred_r2 = fmt_num(perf_head$cv_r2, 4),
  pred_perm_p = paste0("1/", perm_head$n_permutations[1] + 1L),
  pred_n_perm = paste0(perm_head$n_permutations[1], " full-refit outcome permutations (seed ",
                       perm_head$seed[1], ")"),
  pred_null_median = fmt_num(perm_head$null_median, 4),
  pred_null_interval = fmt_ci(perm_head$null_q025, perm_head$null_q975, 4),
  pred_repeated_r2 = fmt_num(perf_head$repeated_cv_mean_r2, 4),
  pred_repeated_interval = fmt_ci(perf_head$cv_r2_q025, perf_head$cv_r2_q975, 4),
  pred_interval_type = as.character(perf_head$interval_type[1]),
  pred_baseline_r2 = fmt_num(baseline_r2, 4),
  cv_scheme = as.character(perm_head$cv_scheme[1]),
  validation_scheme = unique(as.character(ho_primary$validation_scheme)),
  domain_family = paste(unique(as.character(domain$family_id)), collapse = " and "),
  domain_supported = sum(domain$q < 0.05, na.rm = TRUE),
  domain_n_cells = nrow(domain))

cat("    frozen facts: rho =", facts$assoc_rho, "| LOAO R2 =", facts$pred_r2,
    "| permutation p =", facts$pred_perm_p, "| domain cells supported:",
    facts$domain_supported, "of", facts$domain_n_cells, "\n")

# ------------------------------------------------------------- registration
figman <- list(); srcman <- list()

wsrc <- function(x, f) {
  write_csv(x, file.path(dirs$source_data, f)); f
}
reg <- function(fm, panel, src_file, src_key, src_table, stage, note = NA_character_) {
  figman[[length(figman) + 1L]] <<- fm %>%
    mutate(panel_id = panel, canonical_stage = stage, path_key = src_key,
           script = "27_build_behavior_main_figure.R", note = note)
  if (!is.na(src_file)) {
    srcman[[length(srcman) + 1L]] <<- tibble(
      figure_file = paste0(fm$stem, ".pdf"), panel = panel,
      source_data_file = src_file, canonical_producer_stage = stage,
      path_key = src_key, canonical_source_table = src_table)
  }
  invisible(fm)
}

W <- MMM_WIDTH_DOUBLE_MM

# ================================================================== PANEL A
cat("  panel A: outcome definition ...\n")

a_summary <- bmf_descriptive_summary(animal, "CombZ", c("Sex", "Group"))
pA1 <- bmf_panel_a_schematic(
  components = facts$combz_components,
  combine_rule = paste0("mean of ", length(combz_components), " z-scores",
                        "\nhigher = more resilient-like"),
  window_label = "first 12 h Active block after CC1\n(18:30-06:30)",
  external_note = paste(
    "Defined OUTSIDE this repository, in the endpoint workbook:",
    "the z-scoring reference for each component, and the later",
    "RES / SUS threshold. Neither is reproducible from analysis",
    "code. See docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md",
    sep = "\n"),
  n_animals = facts$n_total, group_counts = facts$group_counts)
pA2 <- bmf_panel_a_distribution(animal, a_summary,
                                y_lab = "CombZ (higher = more resilient-like)")

srcA <- bind_rows(
  bmf_source_data(animal, "A", "16", "manuscript/behavior/animal_level_source_data.csv",
                  c("AnimalID", "Sex", "Group", "CombZ")) %>%
    mutate(row_role = "animal"),
  bmf_source_data(a_summary, "A", "27 (descriptive summary of the plotted points)",
                  "derived from the animal rows in this same file",
                  c("Sex", "Group", "n_animals", "median", "q25", "q75", "mean",
                    "summary_type")) %>%
    mutate(row_role = "group_summary"))
fA <- wsrc(srcA, "source_panel_a_combz_definition.csv")

# ================================================================== PANEL B
cat("  panel B: first-active domain overview ...\n")

DOMAIN_ORDER <- c("Psychomotor activation", "Behavioral volatility / fragmentation",
                  "Behavioral flexibility / predictability",
                  "Active-phase adaptation / exploration",
                  "Social spatial organization")
domain_order_use <- intersect(rev(DOMAIN_ORDER), unique(as.character(domain$Domain)))
if (length(domain_order_use) != length(unique(domain$Domain))) {
  stop("Domain vocabulary changed upstream; refusing to silently reorder. ",
       "Unmatched: ",
       paste(setdiff(unique(as.character(domain$Domain)), DOMAIN_ORDER),
             collapse = ", "), call. = FALSE)
}
pB <- bmf_panel_b_domain_heatmap(domain, fdr_threshold = 0.05,
                                 domain_order = domain_order_use)
srcB <- bmf_source_data(
  domain, "B", "14", "12_systems_neuroscience_summary/.../first_night_group_contrasts.csv",
  c("Sex", "contrast", "Domain", "hedges_g", "estimate", "SE", "ci_low",
    "ci_high", "raw_p", "q", "family_id", "n_tests_in_family", "n_ref",
    "n_comp", "bin_level", "interpretation_guard"))
fB <- wsrc(srcB, "source_panel_b_rfid_domain_overview.csv")

# ================================================================== PANEL C
cat("  panel C: first active-phase movement ...\n")

c_summary <- bmf_descriptive_summary(animal, "Movement_mean", c("Sex", "Group"))
MOVEMENT_LAB <- "Movement (transitions / 10 min)"
pC <- bmf_panel_c_first_night(animal, c_summary, y_lab = MOVEMENT_LAB)

srcC <- bind_rows(
  bmf_source_data(animal, "C", "09",
                  "pipeline/09_early_prediction/10min/tables/model_ladder_input.csv",
                  c("AnimalID", "Sex", "Group", "Movement_mean")) %>%
    mutate(row_role = "animal"),
  bmf_source_data(c_summary, "C", "27 (descriptive summary of the plotted points)",
                  "derived from the animal rows in this same file",
                  c("Sex", "Group", "n_animals", "median", "q25", "q75", "mean",
                    "summary_type")) %>%
    mutate(row_role = "group_summary"))
fC <- wsrc(srcC, "source_panel_c_first_active_movement.csv")

# ================================================================== PANEL D
cat("  panel D: early movement vs later CombZ ...\n")

d_trend <- bmf_quantile_trend(animal, "Movement_mean", "CombZ", n_bins = 5L)
d_annot <- paste0(
  "Spearman rho = ", facts$assoc_rho, "\n95% CI ", facts$assoc_ci,
  "\nBH q = ", facts$assoc_q, ", n = ", facts$n_total)
pD <- bmf_panel_d_scatter(
  animal, d_trend, annotation = d_annot, x_lab = MOVEMENT_LAB,
  y_lab = "Later CombZ")

srcD <- bind_rows(
  bmf_source_data(animal, "D", "09",
                  "pipeline/09_early_prediction/10min/tables/model_ladder_input.csv",
                  c("AnimalID", "Sex", "Group", "Movement_mean", "CombZ")) %>%
    mutate(row_role = "animal"),
  bmf_source_data(d_trend, "D", "27 (descriptive summary of the plotted points)",
                  "derived from the animal rows in this same file",
                  c("bin", "x_median", "y_median", "n_animals", "trend_type")) %>%
    mutate(row_role = "quantile_trend"))
fD <- wsrc(srcD, "source_panel_d_movement_vs_combz.csv")

# ================================================================== PANEL E
cat("  panel E: out-of-sample prediction ...\n")

e_annot <- paste0(
  facts$validation_scheme, "\nheld-out R2 = ", facts$pred_r2)
pE1 <- bmf_panel_e_heldout(
  ho_primary, x_lab = "Observed CombZ",
  y_lab = "Held-out predicted CombZ", annotation = e_annot)

e_perf <- perm %>%
  transmute(model = .data$model, model_label = .data$model_label,
            observed_statistic = .data$observed_statistic,
            observed_statistic_name = .data$observed_statistic_name,
            null_median = .data$null_median, null_q025 = .data$null_q025,
            null_q975 = .data$null_q975, empirical_p = .data$empirical_p,
            n_permutations = .data$n_permutations, cv_scheme = .data$cv_scheme,
            seed = .data$seed) %>%
  left_join(perf %>% select("model" = "model_id", "reporting_role",
                            "repeated_cv_mean_r2", "cv_r2_q025", "cv_r2_q975",
                            "interval_type", "predictors", "n_animals"),
            by = "model") %>%
  # Row labels are shortened for the axis only; the full upstream model_label
  # and the predictor list both travel verbatim in the Source Data.
  mutate(row_label = if_else(
    .data$model == HEADLINE_MODEL, "Movement mean\n(headline)",
    "Movement mean +\nRMSSD + entropy ACF1")) %>%
  arrange(desc(.data$observed_statistic))

e_annot2 <- paste0(
  "Grey band + tick: permutation null\n",
  "2.5-97.5% and median. Point: observed.\n",
  "Thin bar: repeated-CV split-to-split\n",
  "quantiles, NOT a confidence interval.\n",
  "Dashed: intercept-only baseline.\n",
  "Permutation p = ", facts$pred_perm_p, " for both models.")
pE2 <- bmf_panel_e_performance(
  e_perf, baseline = baseline_r2, label_col = "row_label",
  x_lab = "Out-of-sample R2 vs the mean", annotation = e_annot2)

srcE <- bind_rows(
  bmf_source_data(ho_primary, "E", "16",
                  "manuscript/behavior/prediction_source_data.csv",
                  c("AnimalID", "Sex", "Group", "observed_CombZ",
                    "predicted_CombZ", "model_id", "validation_scheme")) %>%
    mutate(row_role = "heldout_prediction"),
  bmf_source_data(e_perf, "E", "09",
                  "pipeline/09_early_prediction/10min/tables/primary_prediction_{performance,permutation_test}.csv",
                  c("model", "model_label", "row_label", "reporting_role", "predictors",
                    "n_animals", "observed_statistic_name",
                    "observed_statistic", "null_median", "null_q025",
                    "null_q975", "empirical_p", "n_permutations", "cv_scheme",
                    "seed", "repeated_cv_mean_r2", "cv_r2_q025", "cv_r2_q975",
                    "interval_type")) %>%
    mutate(row_role = "cv_performance"))
fE <- wsrc(srcE, "source_panel_e_out_of_sample_prediction.csv")

# --------------------------- Source Data must equal the plotted values at 0
src_zero_checks <- tibble(
  panel = c("A", "C", "D", "E"),
  check = c("CombZ", "Movement_mean", "CombZ", "predicted_CombZ"),
  max_abs_difference = c(
    max(abs(srcA$CombZ[srcA$row_role == "animal"] - animal$CombZ)),
    max(abs(srcC$Movement_mean[srcC$row_role == "animal"] - animal$Movement_mean)),
    max(abs(srcD$CombZ[srcD$row_role == "animal"] - animal$CombZ)),
    max(abs(srcE$predicted_CombZ[srcE$row_role == "heldout_prediction"] -
              ho_primary$predicted_CombZ))))
if (any(src_zero_checks$max_abs_difference != 0)) {
  stop("Source Data does not reproduce the plotted values exactly:\n",
       paste(capture.output(print(src_zero_checks)), collapse = "\n"),
       call. = FALSE)
}
cat("    Source Data equals plotted values at tolerance 0\n")

# =========================================================== INDIVIDUAL PANELS
cat("  exporting panels ...\n")

panel_specs <- list(
  list(p = pA1, stem = "panel_a_combz_definition_schematic", w = W * 0.62, h = 52,
       panel = "A", src = fA, key = "behavior.combz_definition",
       tbl = "animal_level_source_data.csv", stage = "16"),
  list(p = pA2, stem = "panel_a_combz_distribution", w = W * 0.36, h = 52,
       panel = "A", src = NA_character_, key = "behavior.combz_definition",
       tbl = "animal_level_source_data.csv", stage = "16"),
  list(p = pB, stem = "panel_b_rfid_domain_overview", w = W * 0.55, h = 56,
       panel = "B", src = fB, key = "behavior.rfid_domain_summary",
       tbl = "first_night_group_contrasts.csv", stage = "14"),
  list(p = pC, stem = "panel_c_first_active_movement", w = W * 0.44, h = 56,
       panel = "C", src = fC, key = "behavior.first_night_movement",
       tbl = "model_ladder_input.csv", stage = "09"),
  list(p = pD, stem = "panel_d_movement_vs_combz", w = W * 0.44, h = 58,
       panel = "D", src = fD, key = "behavior.first_night_combz_association",
       tbl = "primary_movement_entropyacf1_associations.csv", stage = "09"),
  # The panel E Source Data file carries BOTH the Stage 16 held-out predictions
  # and the Stage 09 cross-validation performance, so its provenance names both
  # producers. path_key stays single-valued because it is what the input hash is
  # computed from.
  list(p = pE1, stem = "panel_e_out_of_sample_prediction_heldout", w = W * 0.30, h = 58,
       panel = "E", src = fE, key = "behavior.early_prediction_heldout",
       tbl = paste("16: manuscript/behavior/prediction_source_data.csv;",
                   "09: pipeline/09_early_prediction/10min/tables/",
                   "primary_prediction_{performance,permutation_test}.csv"),
       stage = "16 (held-out predictions) + 09 (CV performance and permutation)"),
  list(p = pE2, stem = "panel_e_out_of_sample_prediction_null", w = W * 0.36, h = 58,
       panel = "E", src = NA_character_, key = "behavior.early_prediction",
       tbl = "primary_prediction_permutation_test.csv", stage = "09"))

for (s in panel_specs) {
  fm <- mmm_export_figure(s$p, dirs$figures_panels, s$stem, s$w, s$h,
                          png_preview = FALSE)
  reg(fm, s$panel, s$src, s$key, s$tbl, s$stage)
}

# ============================================================== MAIN FIGURE
cat("  assembling main figure ...\n")

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("patchwork is required to assemble the composed main figure. The ",
       "individual panels above were exported successfully; install patchwork ",
       "and re-run to produce the composite.", call. = FALSE)
}
library(patchwork)

tag <- function(p, letter) p + mmm_panel_label(letter) + mmm_tag_theme()

row_a <- tag(pA1, "a") + (pA2 + theme(legend.position = "none")) +
  patchwork::plot_layout(widths = c(2.05, 1))
row_b <- tag(pB, "b") + tag(pC, "c") + patchwork::plot_layout(widths = c(1.22, 1))
row_c <- tag(pD, "d") +
  tag(pE1, "e") +
  (pE2 + theme(plot.margin = margin(3.5, 3.5, 3.5, 3.5))) +
  patchwork::plot_layout(widths = c(1, 0.70, 1.08))

main_fig <- row_a / row_b / row_c +
  patchwork::plot_layout(heights = c(0.94, 1, 1.02))

MAIN_H <- 168
stopifnot(MAIN_H <= MMM_MAX_HEIGHT_MM)
fm_main <- mmm_export_figure(main_fig, dirs$figures_main,
                             "behavior_outcome_and_early_prediction_main",
                             W, MAIN_H, png_preview = FALSE)
reg(fm_main, "A-E", NA_character_, "multiple",
    "see source_data/behavior_main_figure_source_manifest.csv", "09/14/16",
    note = "composed main figure")

fm_prev <- mmm_save_pub(main_fig,
                        file.path(dirs$figures_previews,
                                  "behavior_outcome_and_early_prediction_main.png"),
                        W, MAIN_H, dpi = 200)
cat("    main figure:", W, "x", MAIN_H, "mm\n")

# ================================================== ALTERNATIVE CANDIDATES
cat("  panel candidates and extended-data candidates ...\n")

# D, rank space: the only line whose slope the canonical rank model licenses.
pD_rank <- bmf_panel_d_rank_space(
  animal, rho = as.numeric(assoc_row$spearman_rho),
  annotation = paste0("reference line slope = canonical rho = ", facts$assoc_rho,
                      "\nrank space; no regression fitted"),
  x_lab = "Rank of first-night Movement", y_lab = "Rank of later CombZ")
fm <- mmm_export_figure(pD_rank, dirs$figures_panels,
                        "panel_d_movement_vs_combz_rank_space", W * 0.44, 58,
                        png_preview = FALSE)
reg(fm, "D", NA_character_, "behavior.first_night_combz_association",
    "primary_movement_entropyacf1_associations.csv", "09",
    note = "candidate: rank-space companion, slope is the canonical rho")

# D, sex-facetted: DESCRIPTIVE ONLY. The canonical association is pooled and the
# formal feature-by-sex interaction is null, so this must never be presented as
# a sex-difference result.
sexint_row <- sex_int %>% filter(.data$feature == HEADLINE_FEATURE)
pD_sex <- bmf_panel_d_scatter(
  animal, d_trend, show_trend = FALSE,
  annotation = paste0(
    "DESCRIPTIVE sex split only.\nCanonical association is pooled; formal\n",
    "feature-by-sex interaction q = ",
    fmt_num(sexint_row$interaction_p_bh, 3)),
  x_lab = MOVEMENT_LAB, y_lab = "Later CombZ") +
  facet_wrap(~Sex)
fm <- mmm_export_figure(pD_sex, dirs$figures_panels,
                        "panel_d_movement_vs_combz_by_sex_descriptive",
                        W * 0.60, 58, png_preview = FALSE)
reg(fm, "D", NA_character_, "behavior.first_night_combz_association",
    "primary_feature_sex_interactions.csv", "09",
    note = "candidate: DESCRIPTIVE sex split; interaction is null, not a claim")

# B alternative, EXTENDED DATA ONLY: the broad, genuinely longitudinal,
# phase-resolved domain map. Built so the choice between the two candidate B
# panels is a visible editorial decision rather than a silent one. It is NOT
# promoted to the main figure: see docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md for
# the six reasons, of which the binding ones are that it is in no manuscript
# registry row and that all of its FDR-supported cells are Inactive.
broad_available <- FALSE
broad <- tryCatch(rd("behavior.rfid_domain_summary_broad", "domain_effects"),
                  error = function(e) NULL)
if (!is.null(broad) && all(c("Domain", "PhaseClass", "Sex", "contrast",
                             "hedges_g", "FDR_q") %in% names(broad))) {
  broad_available <- TRUE
  broad_plot <- broad %>%
    filter(!is.na(.data$hedges_g)) %>%
    mutate(q = .data$FDR_q,
           # The two domain tables disagree on spacing around the slash, which
           # would silently split a row if they were ever aligned by name.
           Domain = str_squish(str_replace_all(.data$Domain, "/", " / ")),
           family_id = .data$FDR_family_id)
  pB_alt <- bmf_panel_b_domain_heatmap(
    broad_plot, fdr_threshold = 0.05,
    key_cols = c("Sex", "contrast", "Domain", "PhaseClass")) +
    facet_grid(rows = vars(.data$Sex), cols = vars(.data$PhaseClass)) +
    labs(subtitle = paste(
      "EXTENDED DATA CANDIDATE. Not registry-declared. Every FDR-supported",
      "cell here is Inactive, which\ndocs/KNOWN_LIMITATIONS.md item 3 forbids",
      "interpreting as biology. No confidence intervals are shipped."))
  fm <- mmm_export_figure(pB_alt, dirs$figures_ed,
                          "ed_candidate_broad_domain_map_phase_resolved",
                          W * 0.72, 78, png_preview = FALSE)
  srcB_alt <- bmf_source_data(
    broad_plot, "B-alt", "14",
    "12_systems_neuroscience_summary/5min_based/stats_tables/systems_sis_domain_effect_summary.csv",
    intersect(c("Domain", "PhaseClass", "Sex", "contrast", "n_ref_animals",
                "n_comp_animals", "hedges_g", "mixed_model_estimate",
                "mixed_model_SE", "mixed_model_p", "FDR_q",
                "n_tests_in_family", "FDR_family_id", "resolution"),
              names(broad_plot)))
  reg(fm, "B-alt", wsrc(srcB_alt, "source_ed_candidate_broad_domain_map.csv"),
      "behavior.rfid_domain_summary_broad", "systems_sis_domain_effect_summary.csv",
      "14", note = "EXTENDED DATA CANDIDATE ONLY - unregistered, Inactive cells forbidden as biology")
  cat("    broad domain alternative built as an extended-data candidate\n")
} else {
  cat("    broad domain alternative unavailable; main panel B unaffected\n")
}

cat("  writing tables, manifests, legends ...\n")

# ============================================================ KEY RESULTS
key_results <- bind_rows(
  frozen_res %>%
    filter(str_detect(.data$claim_id, "^S09_ASSOC_|^S09_PRED_")) %>%
    filter(!str_detect(.data$claim_id, "_sex$")) %>%
    transmute(
      Analysis = .data$analysis_domain, Endpoint = .data$endpoint,
      `Time window` = .data$time_window, Sex = .data$sex,
      `Model or contrast` = .data$contrast_or_model,
      `Effect size type` = .data$effect_size_type,
      Estimate = signif(.data$effect_size, 4),
      `95% CI` = if_else(is.na(.data$ci_low), NA_character_,
                         mmm_fmt_ci(.data$ci_low, .data$ci_high, 3)),
      `raw p` = mmm_fmt_p(.data$p_raw),
      `adjusted p` = mmm_fmt_p(.data$p_adjusted),
      `Adjustment` = .data$adjustment_method,
      `Multiplicity family` = .data$multiplicity_family,
      `n animals` = .data$n_animals,
      `Robustness status` = .data$robustness_status,
      `Claim id` = .data$claim_id),
  domain %>% filter(.data$q < 0.05) %>%
    transmute(
      Analysis = "First-active domain characterisation (descriptive)",
      Endpoint = paste0("Domain score: ", .data$Domain),
      `Time window` = "First active 12 h after first cage change",
      Sex = .data$Sex, `Model or contrast` = .data$contrast,
      `Effect size type` = "Hedges g",
      Estimate = signif(.data$hedges_g, 4),
      `95% CI` = mmm_fmt_ci(.data$ci_low, .data$ci_high, 3),
      `raw p` = mmm_fmt_p(.data$raw_p), `adjusted p` = mmm_fmt_p(.data$q),
      `Adjustment` = "Benjamini-Hochberg",
      `Multiplicity family` = .data$family_id,
      `n animals` = .data$n_ref + .data$n_comp,
      `Robustness status` = paste0(
        "Only FDR-supported cell of ", nrow(domain),
        "; descriptive characterisation, not prospective validation"),
      `Claim id` = "FIRSTNIGHT_5DOMAIN_PANEL"))
write_csv(key_results, file.path(dirs$tables_manuscript,
                                 "behavior_main_key_results.csv"))

# ================================================================ MANIFESTS
figure_manifest <- bind_rows(figman)
write_csv(figure_manifest, file.path(dirs$manifests, "figure_manifest.csv"))

input_manifest <- mmm_path_describe(root = project_root, hash = TRUE) %>%
  mutate(read_by_stage27 = paste(.data$path_key, .data$file_role, sep = "/") %in%
           names(inputs_seen))
write_csv(input_manifest,
          file.path(dirs$manifests, "behavior_main_figure_input_manifest.csv"))

src_manifest <- bind_rows(srcman) %>%
  mutate(
    resolution = case_when(
      canonical_producer_stage == "09" ~ "10min",
      canonical_producer_stage == "14" ~ "10min (5min backbone for non-HMM domains)",
      grepl("16", canonical_producer_stage) ~ "10min (inherited)",
      TRUE ~ NA_character_),
    model_id = case_when(
      panel == "D" ~ "stage09_spearman_movement_mean_vs_combz",
      panel == "E" ~ "stage09_primary_prediction_movement_mean",
      panel == "B" ~ "stage14_first_night_domain_within_sex_marginal_contrasts",
      panel == "C" ~ "not applicable (distribution only)",
      panel == "A" ~ "not applicable (definitional)"),
    test_id = case_when(
      panel == "D" ~ "spearman_rank_correlation_with_bootstrap_ci",
      panel == "E" ~ "loao_out_of_sample_r2_with_full_refit_outcome_permutation",
      panel == "B" ~ "within_sex_domain_contrasts",
      TRUE ~ "none"),
    multiplicity_family = case_when(
      panel == "D" ~ "Three canonical primary feature associations (BH)",
      panel == "E" ~ "Model-specific full-refit outcome permutation",
      panel == "B" ~ facts$domain_family,
      TRUE ~ "none"),
    input_hash = mmm_file_sha256(
      vapply(path_key, function(k)
        tryCatch(mmm_path_get(k, root = project_root, required = FALSE)[[1]],
                 error = function(e) NA_character_), character(1))),
    output_hash = mmm_file_sha256(file.path(dirs$source_data, source_data_file)))
write_csv(src_manifest, file.path(dirs$source_data,
                                  "behavior_main_figure_source_manifest.csv"))

# ============================================================== CLAIM TRACE
claims <- tribble(
  ~claim_id, ~manuscript_claim, ~canonical_stage, ~canonical_table,
  ~statistical_test, ~multiplicity_family, ~figure_panel, ~source_data_file,
  ~status,

  "CLAIM_BEHAV_01",
  "Later behavioural burden is summarized by CombZ.",
  "external endpoint workbook (definition) + 16 (packaging)",
  "manuscript/behavior/animal_level_source_data.csv",
  paste("None - definitional. Unweighted mean of", length(combz_components),
        "component z-scores; verified arithmetically to 2.2e-16"),
  "none", "A", fA,
  paste("SUPPORTED AS A DEFINITION, NOT REPRODUCIBLE FROM THIS REPOSITORY.",
        "The component z-scoring reference and the RES/SUS threshold live in",
        "the endpoint workbook. Direction is higher = more resilient-like."),

  "CLAIM_BEHAV_02",
  "RFID provides broad longitudinal characterization across multiple behavioral domains.",
  "14", "12_systems_neuroscience_summary/.../first_night_group_contrasts.csv",
  "Within-sex domain contrasts on per-animal domain scores",
  facts$domain_family, "B", fB,
  paste("PARTIALLY SUPPORTED, AND NARROWER THAN THE CLAIM AS WORDED. The",
        "canonical domain table is FIRST-ACTIVE-PHASE ONLY, so it does not",
        "support the word 'longitudinal'.", facts$domain_supported, "of",
        facts$domain_n_cells, "cells are FDR-supported; the panel shows",
        "breadth of characterisation, not a multi-domain signature. The",
        "broad phase-resolved alternative is unregistered and carries",
        "forbidden Inactive HMM cells. NEEDS SCIENTIFIC DECISION."),

  "CLAIM_BEHAV_03",
  "First-night locomotor behavior contains an early signal related to later outcome.",
  "09", "pipeline/09_early_prediction/10min/tables/model_ladder_input.csv",
  paste("Distribution display only. The declared canonical CATEGORICAL test on",
        "this window is Stage 20's within-sex family, which is null in both",
        "sexes"),
  "PRIMARY_FIRST_ACTIVE__2Sex_x_3GroupContrasts (Stage 20, all six null)",
  "C", fC,
  paste("NOT SUPPORTED AS A CATEGORICAL CLAIM, and not annotated as one.",
        "Three implementations disagree: the only nominally FDR-supported",
        "contrast is Stage 09's pooled-sex Wilcoxon, which the repository",
        "labels descriptive and non-claimable. The early signal is carried by",
        "CLAIM_BEHAV_04 and CLAIM_BEHAV_05, which are continuous."),

  "CLAIM_BEHAV_04",
  "Early Movement relates continuously to later CombZ.",
  "09", "pipeline/09_early_prediction/10min/tables/primary_movement_entropyacf1_associations.csv",
  "Spearman rank correlation with a 5000-iteration bootstrap CI, pooled across sex",
  "Three canonical primary feature associations (BH)", "D", fD,
  paste0("SUPPORTED. rho = ", facts$assoc_rho, ", 95% CI ", facts$assoc_ci,
         ", BH q = ", facts$assoc_q, ", n = ", facts$n_total,
         ". Negative sign: higher early Movement, worse later outcome.",
         " Association, not causation."),

  "CLAIM_BEHAV_05",
  "Early behavior contains out-of-sample predictive information about later outcome.",
  "09", "pipeline/09_early_prediction/10min/tables/primary_prediction_{performance,permutation_test}.csv",
  paste0("Exhaustive leave-one-animal-out ordinary least squares; metric ",
         perm_head$observed_statistic_name[1], "; reference ", facts$pred_n_perm),
  "Model-specific full-refit outcome permutation", "E", fE,
  paste0("SUPPORTED, INTERNAL VALIDATION ONLY. Headline held-out R2 = ",
         facts$pred_r2, " versus null median ", facts$pred_null_median,
         " ", facts$pred_null_interval, ", permutation p = ", facts$pred_perm_p,
         ". Grouping unit is the animal only - not cage, social group or",
         " batch. The repeated-CV interval is a split-to-split resampling",
         " interval, not a bootstrap CI. Not external validation; no causal",
         " claim."))

stopifnot(nrow(claims) == 5L)
write_csv(claims, file.path(dirs$audit, "behavior_main_claim_trace.csv"))

# Stage 26 must remain the owner of the temporal characterisation, and must
# keep disclaiming the prospective claim, which is CLAIM_BEHAV_04/05 here.
s26_boundary <- gamm_claims %>%
  filter(str_detect(.data$status, "OWNED BY STAGE 09"))
if (nrow(s26_boundary) < 1L) {
  stop("Stage 26 no longer disclaims the prospective claim. The claim ",
       "boundary between the GAMM temporal characterisation and the Stage 09 ",
       "prospective analysis must be restored before this figure is used.",
       call. = FALSE)
}
write_csv(s26_boundary, file.path(dirs$audit, "stage26_claim_boundary_carried.csv"))

# ============================================================ CONTRACT CHECK
every_panel <- c("A", "B", "C", "D", "E")
traced <- unique(claims$figure_panel)
if (!all(every_panel %in% traced)) {
  stop("Every figure panel must have a claim-trace row. Missing: ",
       paste(setdiff(every_panel, traced), collapse = ", "), call. = FALSE)
}
palette_used <- MMM_GROUP_COLOURS[BMF_GROUP_LEVELS]
stopifnot(identical(unname(palette_used),
                    c("#3E3C6F", "#C6C3BB", "#E63A48")))

all_outputs <- list.files(pub_root, recursive = TRUE, full.names = TRUE)
mmm_assert_publication_path_budget(all_outputs, "Stage 27 publication tree")

flat <- unlist(lapply(list(key_results, claims, src_manifest),
                      function(x) as.character(unlist(x))))
stopifnot(!any(grepl("MODEL_INADEQUATE", flat, fixed = TRUE)))
stopifnot(!any(grepl("DO_NOT_INTERPRET", flat, fixed = TRUE)))

# ============================================================= DOCS + RECORD
bmf_write_legend_draft(
  file.path(dirs$legends, "behavior_main_figure_legend_draft.md"), facts)
bmf_write_readme(file.path(pub_root, "README.md"), facts,
                 input_manifest, MMM_GROUP_COLOURS)

saveRDS(list(stage = STAGE, facts = facts, palette = MMM_GROUP_COLOURS,
             n_figures = nrow(figure_manifest),
             inputs = inputs_seen,
             source_zero_checks = src_zero_checks,
             stage16_vs_stage09_drift = drift),
        file.path(dirs$audit, "stage27_build_record.rds"))

cat("\nStage 27 complete ->", pub_root, "\n")
cat("  figures:", nrow(figure_manifest),
    "| source data files:", length(unique(bind_rows(srcman)$source_data_file)),
    "| inputs read:", length(inputs_seen),
    "| claims traced:", nrow(claims), "\n")
cat("  longest output path:", max(nchar(all_outputs)), "chars\n")
if (broad_available) cat("  broad domain alternative: extended-data candidate written
")
