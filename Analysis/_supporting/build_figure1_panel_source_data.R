#!/usr/bin/env Rscript

# Frozen Figure 1 panel source data.
#
# EXPORT AND VALIDATION ONLY. No statistic is computed here. Every value is
# copied from a canonical table that already owns it, and the only arithmetic is
# the reproduction checks below, which exist to fail loudly if a copied number
# ever stops matching the value the manuscript quotes.
#
# This is the interface the proteomics manuscript renderer consumes. It exists so
# that the renderer reads one small, documented, hash-pinned directory instead of
# scraping arbitrary analysis folders across a network share.
#
# Panel ownership:
#   1a  design and window framework   <- Stage 27 framework table + window contract
#   1b  CombZ and classification      <- canonical later_outcome_combz tables
#   1c  Movement_mean vs CombZ        <- Stage 27 panel c source
#   1d  leave-one-animal-out          <- Stage 27 panel d1 source
#   1e  full-refit permutation null   <- Stage 27 panel d2 source
#   1f  repeated grouped five-fold    <- Stage 27 panel d1 cv_performance rows
#
# 1f carries a point estimate and a percentile range only: the upstream repeated
# CV persists summary rows, not per-repeat values, so no distribution for it
# exists anywhere and none may be invented. It is exported for annotation and
# Extended Data rather than as a main panel.

suppressPackageStartupMessages({ library(dplyr) })

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("figure1_prediction_contract.R")

REPO <- MMM_REPO_ROOT
DATA <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
S27 <- file.path(DATA, "analysis_ready/pipeline/27_behavior_main_figure/source_data")
CBZ <- file.path(DATA, "analysis_ready/canonical/later_outcome_combz/tables")
T10 <- file.path(DATA, "analysis_ready/pipeline/09_early_prediction/10min/tables")
OUT <- file.path(REPO, "results/manuscript_bridge/figure1/figure_source_data")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

rd <- function(dir, f) {
  p <- file.path(dir, f)
  if (!file.exists(p)) stop("missing canonical input: ", p, call. = FALSE)
  utils::read.csv(p, stringsAsFactors = FALSE)
}
near <- function(a, b, tol = 1e-9) isTRUE(abs(a - b) < tol)
must <- function(cond, msg) if (!isTRUE(cond)) stop("FIGURE 1 SOURCE DATA: ", msg, call. = FALSE)

# Provenance stamped into every exported row, so a table separated from this
# directory still names the analysis that owns it.
stamp <- function(d, panel, owner_stage, owner_table) {
  cbind(
    data.frame(figure_panel = panel, scientific_owner_stage = owner_stage,
               owner_table = owner_table, exported_by = "Analysis/_supporting/build_figure1_panel_source_data.R",
               stringsAsFactors = FALSE),
    d
  )
}

git <- function(...) {
  o <- suppressWarnings(system2("git", c("-C", shQuote(REPO), ...),
                                stdout = TRUE, stderr = FALSE))
  if (!length(o)) NA_character_ else o[1]
}
SHA <- git("rev-parse", "HEAD")
BRANCH <- git("rev-parse", "--abbrev-ref", "HEAD")
# Source cleanliness excluding this exporter's own output tree - the artefact
# describes the state of the code that produced it, and writing the artefact
# does not make that code uncommitted. See FC-10.
BUNDLE_TREE <- "results/manuscript_bridge/figure1"
DIRTY <- length(suppressWarnings(system2("git",
  c("-C", shQuote(REPO), "status", "--porcelain", "--untracked-files=all",
    "--", ".", shQuote(paste0(":(exclude)", BUNDLE_TREE, "/**"))),
  stdout = TRUE, stderr = FALSE))) > 0

# ---------------------------------------------------------------- inputs
panel_c <- rd(S27, "source_panel_c_movement_vs_combz.csv")
panel_d1 <- rd(S27, "source_panel_d1_loao_predictions.csv")
panel_d2 <- rd(S27, "source_panel_d2_permutation_null.csv")
framework <- rd(S27, "source_panel_a_framework_combz.csv")
combz <- rd(CBZ, "later_outcome_combz_animal_level.csv")
thresh <- rd(CBZ, "combz_classification_thresholds.csv")
assoc <- rd(T10, "primary_movement_entropyacf1_associations.csv")
perf <- rd(T10, "primary_prediction_performance.csv")
winsum <- rd(T10, "early_window_contract_summary.csv")

HEADLINE <- FIGURE1_PREDICTION_CONTRACT$headline_model_id

# ------------------------------------------------------- 1a design timeline
# Ordering is the scientific content of this panel: the early window closes
# before any outcome component is measured, so the classification cannot have
# existed while the predictor was recorded.
tl <- data.frame(
  step = 1:7,
  event = c("first cage change (CC1)", "early RFID window",
            "social-instability paradigm", "later behavioural outcome tests",
            "terminal physiology", "CombZ computed", "RES/SUS assigned"),
  age_or_time = c("P25", "18:30 inclusive to 06:30 exclusive, first Active block after CC1",
                  "after CC1", "during and after the paradigm", "terminal",
                  "after all components collected", "after CombZ"),
  precedes_outcome = c(TRUE, TRUE, NA, FALSE, FALSE, FALSE, FALSE),
  used_as_predictor = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  used_in_outcome = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  detail = c(
    "reference event for the early window",
    sprintf("%g h, %g-min bins, %d expected slots per animal",
            winsum$target_window_hours[1], winsum$bin_size_min[1],
            as.integer(winsum$expected_target_slots_per_animal[1])),
    "repeated changes of cage composition",
    "novel-object recognition and sucrose preference",
    "corticosterone, adrenal weight, spleen weight",
    "unweighted mean of six within-sex control-referenced z-scores",
    "same-sex control mean minus one control population SD"),
  stringsAsFactors = FALSE)
# Realised coverage travels with the design so the panel can never imply that
# every animal contributed all 72 slots.
tl$n_animals <- c(NA, as.integer(winsum$n_animals[1]), NA, NA, NA, NA, NA)
tl$n_animals_complete_slots <- c(NA, as.integer(winsum$n_animals_complete_72_bins[1]),
                                 NA, NA, NA, NA, NA)
tl$mean_coverage_fraction <- c(NA, winsum$mean_coverage_fraction[1], NA, NA, NA, NA, NA)
tl$min_coverage_fraction <- c(NA, winsum$min_coverage_fraction[1], NA, NA, NA, NA, NA)
f1a <- stamp(tl, "1a", "Analysis/09_early_prediction_model_ladder.R",
             "early_window_contract_summary.csv + figure1_timeline_contract.csv")

# --------------------------------------------- 1b CombZ and classification
b <- combz %>%
  select(AnimalID = AnimalNum, Sex, CombZ, final_group = outcome_group,
         experimental_condition, n_components_present, classification_rule_id) %>%
  left_join(
    thresh %>% select(Sex, n_control, control_reference_mean = control_mean_combz,
                      control_reference_population_sd = control_population_sd_combz,
                      susceptibility_threshold, sd_convention),
    by = "Sex")
b$in_early_window_analysis_set <- b$AnimalID %in% panel_c$AnimalID[panel_c$row_role == "animal"]
f1b <- stamp(b, "1b", "Analysis/build_later_outcome_combz.R",
             "later_outcome_combz_animal_level.csv + combz_classification_thresholds.csv")

# ------------------------------------------- 1c Movement_mean versus CombZ
# One row per biological animal. The descriptive quantile-trend rows Stage 27
# carries for its own plot are dropped: this contract is animal-level.
c_an <- panel_c %>% filter(row_role == "animal") %>%
  select(AnimalID, Sex, Group, Movement_mean, CombZ)
f1c <- stamp(c_an, "1c", "Analysis/09_early_prediction_model_ladder.R",
             "primary_movement_entropyacf1_associations.csv + early_behavior_features_wide.csv")

# -------------------------------------------------- 1d held-out prediction
d_pred <- panel_d1 %>% filter(row_role == "heldout_prediction") %>%
  select(AnimalID, Sex, Group, observed_CombZ, predicted_CombZ, model_id,
         validation_scheme)
f1d <- stamp(d_pred, "1d", "Analysis/09_early_prediction_model_ladder.R",
             "primary_prediction_predictions.csv")

# ---------------------------------------------------- 1e permutation null
e_perm <- panel_d2 %>% filter(model_id == HEADLINE) %>%
  select(permutation_id, seed, model_id, model_label, cv_scheme,
         performance_metric, performance_value, is_observed, n_permutations,
         row_role)
f1e <- stamp(e_perm, "1e", "Analysis/09_early_prediction_model_ladder.R",
             "early_prediction_permutation_draws.csv")

# ------------------------------------------------- 1f repeated grouped CV
# Stage 27 keys the held-out rows on model_id and the summary rows on model,
# leaving model_id NA there; coalesce so downstream has one key.
f_cv <- panel_d1 %>% filter(row_role == "cv_performance") %>%
  mutate(model_id = dplyr::coalesce(model_id, model)) %>%
  select(model_id, model_label, reporting_role, predictors, n_animals,
         repeated_cv_mean_r2, cv_r2_q025, cv_r2_q975, interval_type)
f_cv$repeated_cv_k <- FIGURE1_PREDICTION_CONTRACT$repeated_cv_k
f_cv$repeated_cv_repeats <- FIGURE1_PREDICTION_CONTRACT$repeated_cv_repeats
f_cv$repeated_cv_group_col <- FIGURE1_PREDICTION_CONTRACT$repeated_cv_group_col
f_cv$repeated_cv_seed <- FIGURE1_PREDICTION_CONTRACT$repeated_cv_seed
f_cv$per_repeat_values_available <- FALSE
f1f <- stamp(f_cv, "1f", "Analysis/09_early_prediction_model_ladder.R",
             "model_ladder_repeated_grouped_kfold_performance.csv")

# =====================================================================
# PHASE G assertions. Every one of these is a number the manuscript prints.
# =====================================================================
mv <- function(col) assoc[[col]][assoc$feature == "Movement_mean"]
pf <- function(id, col) perf[[col]][perf$model_id == id]

must(length(unique(f1c$AnimalID)) == 111L, "1c must carry exactly 111 unique animals")
must(nrow(f1c) == 111L, "1c must be one row per animal")
must(!anyDuplicated(f1d$AnimalID), "1d has a duplicate AnimalID")
must(nrow(f1d) == 111L, "1d must carry 111 held-out predictions")
must(!anyDuplicated(f1b$AnimalID), "1b has a duplicate AnimalID")
must(all(f1d$model_id == HEADLINE), "1d must contain only the headline model")

must(near(mv("spearman_rho"), -0.3902639142724438), "Movement rho moved")
must(near(mv("spearman_boot_ci_low"), -0.547434593760078), "Movement CI lower moved")
must(near(mv("spearman_boot_ci_high"), -0.20917806610402842), "Movement CI upper moved")
must(near(mv("spearman_p_bh"), 6.8776554749896e-05), "Movement q moved")

must(near(pf(HEADLINE, "cv_r2"), 0.15939455855319962), "LOAO R2 moved")
must(near(pf("mean_only", "cv_r2"), -0.01826446280991756), "baseline R2 moved")

obs <- f1e$performance_value[f1e$row_role == "observed_statistic"]
nulls <- f1e$performance_value[f1e$row_role == "permutation_draw"]
must(length(nulls) == 1000L, "1e must carry exactly 1000 null draws")
must(length(obs) == 1L, "1e must carry exactly one observed statistic")
must(near(obs, 0.15939455855319962), "1e observed statistic is not the LOAO R2")
must(sum(nulls >= obs) == 0L, "a null draw reached the observed value")
must(near((sum(nulls >= obs) + 1) / (length(nulls) + 1), 1 / 1001), "permutation p is not 1/1001")
must(all(f1e$seed == FIGURE1_PREDICTION_CONTRACT$outcome_permutation_seed),
     "1e permutation seed is not the canonical outcome-permutation seed")

cvh <- f1f[f1f$model_id == HEADLINE, ]
must(near(cvh$repeated_cv_mean_r2, 0.15582272536971034), "repeated CV mean R2 moved")
must(near(cvh$cv_r2_q025, 0.11594027484397953), "repeated CV lower quantile moved")
must(near(cvh$cv_r2_q975, 0.17904334073063455), "repeated CV upper quantile moved")
must(cvh$repeated_cv_seed == 521L, "repeated CV seed metadata must be 521")

must(near(unique(f1b$susceptibility_threshold[f1b$Sex == "Male"]), -0.4366416981697792),
     "male threshold moved")
must(near(unique(f1b$susceptibility_threshold[f1b$Sex == "Female"]), -0.22239084402294293),
     "female threshold moved")
must(all(f1b$n_control == 12L), "control reference must be 12 animals per sex")

# No outcome-derived Group may enter the headline prediction source, and no
# GAMM-derived or sex-specific predictor may enter the main figure.
must(!any(grepl("gamm|Group", names(f1c), ignore.case = TRUE) &
            !names(f1c) %in% c("Group")), "1c carries an unexpected predictor column")
must(identical(sort(setdiff(names(f1d), c("figure_panel", "scientific_owner_stage",
      "owner_table", "exported_by"))),
      sort(c("AnimalID", "Sex", "Group", "observed_CombZ", "predicted_CombZ",
             "model_id", "validation_scheme"))),
     "1d schema drifted")
must(all(f1d$validation_scheme == unique(panel_d1$validation_scheme[panel_d1$row_role == "heldout_prediction"])),
     "1d validation scheme is not uniform")
must(!any(grepl("gamm", unlist(lapply(list(f1c, f1d, f1e), names)), ignore.case = TRUE)),
     "a GAMM-derived column reached a main-figure panel")
must(FIGURE1_PREDICTION_CONTRACT$headline_model_predictors == "Movement_mean",
     "the headline model is not movement-mean")

# 1b is a definition panel, not a validation panel: the six components that
# build CombZ must not be exported as plottable series.
must(!any(c("NOR", "sucrose_pref", "weight_dev", "delta_cort", "adrenal_weight",
            "spleen_weight") %in% names(f1b)),
     "1b must not carry the six CombZ components as plottable columns")

# ---------------------------------------------------------------- write
writeout <- function(d, f) {
  utils::write.csv(d, file.path(OUT, f), row.names = FALSE)
  f
}
files <- c(
  writeout(f1a, "figure1a_timeline_source.csv"),
  writeout(f1b, "figure1b_combz_classification_source.csv"),
  writeout(f1c, "figure1c_movement_combz_source.csv"),
  writeout(f1d, "figure1d_loao_predictions_source.csv"),
  writeout(f1e, "figure1e_permutation_source.csv"),
  writeout(f1f, "figure1f_repeated_cv_source.csv"))

# Every number the figure or its legend prints, with the table it is recoverable
# from. A value absent from here must not appear on the figure.
S <- function(panel, statistic, value, source_file, note = "") data.frame(
  figure_panel = panel, statistic = statistic, value = as.character(value),
  recoverable_from = source_file, note = note, stringsAsFactors = FALSE)
stats <- rbind(
  S("1a", "expected 10-min slots per animal", as.integer(winsum$expected_target_slots_per_animal[1]), "figure1a_timeline_source.csv", "design expectation"),
  S("1a", "animals with all expected slots", as.integer(winsum$n_animals_complete_72_bins[1]), "figure1a_timeline_source.csv", "realised coverage; the remainder are missing leading slots only"),
  S("1a", "mean coverage fraction", winsum$mean_coverage_fraction[1], "figure1a_timeline_source.csv", ""),
  S("1b", "male susceptibility threshold", -0.4366416981697792, "figure1b_combz_classification_source.csv", "same-sex control mean minus one control population SD"),
  S("1b", "female susceptibility threshold", -0.22239084402294293, "figure1b_combz_classification_source.csv", ""),
  S("1b", "animals with CombZ", nrow(f1b), "figure1b_combz_classification_source.csv", "classification set"),
  S("1c", "n animals", nrow(f1c), "figure1c_movement_combz_source.csv", ""),
  S("1c", "Spearman rho", mv("spearman_rho"), "figure1c_movement_combz_source.csv", "owner: primary_movement_entropyacf1_associations.csv"),
  S("1c", "95% CI lower", mv("spearman_boot_ci_low"), "figure1c_movement_combz_source.csv", "5000-sample percentile bootstrap, seed 123"),
  S("1c", "95% CI upper", mv("spearman_boot_ci_high"), "figure1c_movement_combz_source.csv", ""),
  S("1c", "BH q", mv("spearman_p_bh"), "figure1c_movement_combz_source.csv", "BH over three prespecified features"),
  S("1d", "LOAO R2", pf(HEADLINE, "cv_r2"), "figure1d_loao_predictions_source.csv", "movement-mean model"),
  S("1d", "intercept-only baseline R2", pf("mean_only", "cv_r2"), "figure1d_loao_predictions_source.csv", ""),
  S("1d", "n held-out animals", nrow(f1d), "figure1d_loao_predictions_source.csv", "each predicted by a model fitted without it"),
  S("1e", "permutation draws", length(nulls), "figure1e_permutation_source.csv", "full refit per draw"),
  S("1e", "observed LOAO R2", obs, "figure1e_permutation_source.csv", ""),
  S("1e", "empirical p", "1/1001", "figure1e_permutation_source.csv", "no null draw reached the observed value"),
  S("1e", "permutation seed", FIGURE1_PREDICTION_CONTRACT$outcome_permutation_seed, "figure1e_permutation_source.csv", ""),
  S("1f", "repeated grouped CV mean R2", cvh$repeated_cv_mean_r2, "figure1f_repeated_cv_source.csv", ""),
  S("1f", "2.5th percentile across repeats", cvh$cv_r2_q025, "figure1f_repeated_cv_source.csv", "NOT a confidence interval"),
  S("1f", "97.5th percentile across repeats", cvh$cv_r2_q975, "figure1f_repeated_cv_source.csv", ""),
  S("1f", "repeated CV seed", FIGURE1_PREDICTION_CONTRACT$repeated_cv_seed, "figure1f_repeated_cv_source.csv", "fold assignment; distinct from the bootstrap seed 123"))
files <- c(files, writeout(stats, "figure1_panel_statistics.csv"))

# ---------------------------------------------------------------- manifest
sha <- function(p) {
  if (requireNamespace("digest", quietly = TRUE))
    digest::digest(p, algo = "sha256", file = TRUE) else tools::md5sum(p)[[1]]
}
OWNER <- c(
  figure1a_timeline_source.csv = "early_window_contract_summary.csv",
  figure1b_combz_classification_source.csv = "later_outcome_combz_animal_level.csv; combz_classification_thresholds.csv",
  figure1c_movement_combz_source.csv = "source_panel_c_movement_vs_combz.csv",
  figure1d_loao_predictions_source.csv = "source_panel_d1_loao_predictions.csv",
  figure1e_permutation_source.csv = "source_panel_d2_permutation_null.csv",
  figure1f_repeated_cv_source.csv = "source_panel_d1_loao_predictions.csv",
  figure1_panel_statistics.csv = "derived index of the six panel tables")
ANALYSIS <- c(
  figure1a_timeline_source.csv = "Analysis/09_early_prediction_model_ladder.R",
  figure1b_combz_classification_source.csv = "Analysis/build_later_outcome_combz.R",
  figure1c_movement_combz_source.csv = "Analysis/09_early_prediction_model_ladder.R",
  figure1d_loao_predictions_source.csv = "Analysis/09_early_prediction_model_ladder.R",
  figure1e_permutation_source.csv = "Analysis/09_early_prediction_model_ladder.R",
  figure1f_repeated_cv_source.csv = "Analysis/09_early_prediction_model_ladder.R",
  figure1_panel_statistics.csv = "Analysis/_supporting/build_figure1_panel_source_data.R")
man <- do.call(rbind, lapply(files, function(f) {
  p <- file.path(OUT, f)
  data.frame(
    file = f, bytes = file.size(p), sha256 = sha(p), hash_algo = "sha256",
    canonical_upstream_file = unname(OWNER[f]),
    canonical_analysis = unname(ANALYSIS[f]),
    source_commit = SHA, source_branch = BRANCH,
    source_worktree_state = if (DIRTY) "dirty at export time" else "clean",
    creation_state = if (DIRTY) "NOT_AUTHORITATIVE_rerun_from_a_clean_tree" else "authoritative",
    stringsAsFactors = FALSE)
}))
utils::write.csv(man, file.path(OUT, "figure1_panel_source_manifest.csv"),
                 row.names = FALSE)

cat("Figure 1 panel source data written to", OUT, "\n")
cat("  tables:", length(files), "| commit:", substr(SHA, 1, 7),
    "| worktree:", if (DIRTY) "dirty" else "clean", "\n")
cat("  1c animals:", nrow(f1c), "| 1d held-out:", nrow(f1d),
    "| 1e draws:", length(nulls), "| 1b classified:", nrow(f1b), "\n")
cat("  all Phase-G reproduction assertions passed\n")
if (DIRTY) cat("  WARNING: exported from a dirty tree; rerun from a clean tree before freezing\n")
