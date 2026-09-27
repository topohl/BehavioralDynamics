# ================================================================
# Stage 16b - Canonical behavioural result bundle (export layer)
# MMMSociability
# ================================================================
# The ONLY writer of the canonical behaviour bundle that Exp9_manuscript imports.
# It fits nothing. It assembles Stage 29 (characterisation) and Stage 09 (registered
# prediction) outputs into a versioned, hash-frozen bundle:
#   analysis_ready/canonical/behavior_bundle/<bundle_id>/
# and verifies before freezing:
#   * the Stage 29 run used the currently frozen configuration (config sha256),
#   * every Stage 29 input is byte-identical to what the run recorded,
#   * the canonical code has not changed since the Stage 29 run commit,
#   * every Holm family recomputes exactly from its raw p-values and has its declared size.
# A bundle version directory is never overwritten; there is no "latest" pointer.
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("project_paths.R"); source_mmm_helper("behavior_analysis_config.R")

CFG <- MMM_BEHAVIOR_CONFIG; ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
# DRY RUN (code testing only): MMM_STAGE16B_DRY_RUN_DIR = a Stage 29 dry-run folder; the freeze and git gates are skipped,
# the bundle is written inside that folder with status DRY_RUN_NOT_FOR_USE and never registered in analysis_ready.
DRY <- nzchar(Sys.getenv("MMM_STAGE16B_DRY_RUN_DIR"))
S29 <- if (DRY) Sys.getenv("MMM_STAGE16B_DRY_RUN_DIR") else behavior_stage_dir(ROOT, "29", "canonical_behavior")
T29 <- file.path(S29, "tables"); A29 <- file.path(S29, "audit")
S09 <- file.path(AR, "pipeline", "09_early_prediction")
git <- function(...) system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE)
rd <- function(...) data.table::fread(file.path(...))
sha_file <- function(f) digest::digest(file = f, algo = "sha256")

# ---------------------------------------------------------------- currency gates
cfg_sha <- mmm_behavior_config_sha256(CFG)
frozen_dir <- file.path(AR, "canonical", "behavior_config", paste0("v", CFG$meta$config_version))
run <- rd(A29, "run_manifest.csv")
if (!DRY) {
  if (!identical(cfg_sha, readLines(file.path(frozen_dir, "config_sha256.txt"))[1])) stop("Config is not the frozen version.", call. = FALSE)
  if (!identical(run$config_sha256, cfg_sha)) stop("Stage 29 ran with a different config.", call. = FALSE)
  if (isTRUE(run$dry_run)) stop("The Stage 29 run is a dry run.", call. = FALSE)
}
inp <- rd(A29, "run_inputs.csv")
now <- vapply(inp$input, sha_file, "")
if (any(now != inp$sha256)) stop("A Stage 29 input changed since the run: ", paste(inp$input[now != inp$sha256], collapse = ", "), call. = FALSE)
code_files <- c("Analysis/29_canonical_behavior_characterization.R", "Functions/behavior_analysis_config.R", "Functions/rfid_event_stream.R",
                "Functions/rfid_binfree_metrics.R", "Functions/rfid_canonical_inference.R", "Analysis/16b_canonical_behavior_bundle.R")
if (!DRY) {
  if (length(git(c("status", "--porcelain", "--", code_files)))) stop("Canonical code has uncommitted changes.", call. = FALSE)
  changed <- git(c("diff", "--name-only", run$git_commit, "HEAD", "--", setdiff(code_files, "Analysis/16b_canonical_behavior_bundle.R")))
  if (length(changed)) stop("Canonical code changed since the Stage 29 run commit: ", paste(changed, collapse = ", "), call. = FALSE)
}
out_man <- rd(A29, "output_manifest.csv")
if (any(vapply(file.path(T29, out_man$file), sha_file, "") != out_man$sha256)) stop("A Stage 29 output was modified after the run.", call. = FALSE)

# ---------------------------------------------------------------- assemble
EST <- rd(T29, "estimates.csv"); JT <- rd(T29, "joint_tests.csv"); MULT <- rd(T29, "multiplicity.csv")
for (f in unique(MULT$family_id)) { x <- MULT[family_id == f]; re <- stats::p.adjust(x$p_raw, "holm", n = x$declared_m[1])
  if (nrow(x) != x$declared_m[1] || !identical(is.na(re), is.na(x$p_adjusted)) || isTRUE(max(abs(re - x$p_adjusted), na.rm = TRUE) > 1e-12))
    stop("Family ", f, " does not recompute.", call. = FALSE) }
REG29 <- rd(T29, "model_registry.csv")
if (nrow(REG29[failed == TRUE])) message("FAILED models (reported, not estimated): ", paste(REG29[failed == TRUE, model_id], collapse = ", "))
W <- rd(T29, "canonical_window_metrics.csv")
keep_cols <- c("AnimalNum", "Sex", "Batch", "Group", "CC", "CageEpisodeID", "n_in_cage", "n_tracked_mates", "hardware_flag", "obs_s",
               "n_events", "n_bouts", "crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation", "events_per_bout",
               "light_phase_crossing_rate", "CombZ")
B1 <- W[, ..keep_cols]; A1 <- B1[CC == "CC1"]
P09 <- rd(S09, "10min", "tables", "primary_prediction_predictions.csv")[Model == "movement_mean"]
LAD <- rd(S09, "10min", "tables", "model_ladder_input.csv")[, .(AnimalNum = as.character(AnimalNum), Movement_mean)]
P09[, AnimalNum := as.character(AnimalNum)]
A2 <- merge(P09[, .(AnimalNum, Group, Sex, observed_CombZ = observed, heldout_prediction = predicted)], LAD, by = "AnimalNum")
A2[, crossing_rate_equiv_per_h := 6 * Movement_mean]
cz <- rd(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv")[, .(AnimalNum = as.character(AnimalNum), CombZ)]
lad_cz <- merge(rd(S09, "10min", "tables", "model_ladder_input.csv")[, .(AnimalNum = as.character(AnimalNum), outcome)], cz, by = "AnimalNum")
if (nrow(lad_cz) != 111 || max(abs(lad_cz$outcome - lad_cz$CombZ)) > 1e-9) stop("Stage 09 was not run on the canonical CombZ; rerun Stage 09 first.", call. = FALSE)
thr <- rd(AR, "canonical", "later_outcome_combz", "tables", "combz_classification_thresholds.csv")
A4 <- rd(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv")[
  , .(AnimalNum = canonical_animal_id(AnimalNum), Sex, Batch = paste0("B", Batch), Group = outcome_group, CombZ)][, rfid_tracked := AnimalNum %in% canonical_animal_id(W$AnimalNum)]
if (nrow(A4) != 117 || sum(A4$rfid_tracked) != 111) stop("CombZ cohort is not 117 animals with 111 RFID-tracked.", call. = FALSE)
# A0: design timeline for Figure 1a. Design facts only (config$windows, $population); the counts come from the Stage 29 windows.
A0 <- data.table(
  step = 1:8,
  event = c("first cage change (CC1)", "first active phase after CC1", "cage changes CC2-CC4", "active phase after each of CC2-CC4",
            "later behavioural outcome tests", "terminal physiology", "CombZ computed", "RES/SUS assigned"),
  timing = c("P25", "18:30-06:30 (12 h), from an estimated 2.3-8.5 h after the change", "after CC1", "18:30-06:30 (12 h) after each change",
             "during and after the paradigm", "terminal", "after all components", "after CombZ"),
  role = c("reference", "primary window (CC1)", "stressor", "trajectory windows (CC1-CC4)", "outcome", "outcome", "outcome", "classification"),
  detail = c("no RFID recording exists before CC1", "bin-free antenna crossings and shared antenna-zone use",
             "SIS: repeated changes of cage composition; CON: never regrouped", "same window definition at every cage change",
             "novel-object recognition and sucrose preference", "corticosterone, adrenal weight, spleen weight",
             "unweighted mean of six within-sex control-referenced z-scores", "within-sex control mean minus one control population SD"),
  n_animals = c(NA, uniqueN(W[CC == "CC1", AnimalNum]), NA, uniqueN(W$AnimalNum), NA, NA, nrow(A4), nrow(A4)),
  n_windows = c(NA, nrow(W[CC == "CC1"]), NA, nrow(W), NA, NA, NA, NA))
A3 <- rd(S09, "10min", "tables", "early_prediction_permutation_draws.csv")
ASSOC <- rd(S09, "10min", "tables", "primary_movement_entropyacf1_associations.csv")
PERF <- rd(S09, "10min", "tables", "primary_prediction_performance.csv")
PERM <- rd(S09, "10min", "tables", "primary_prediction_permutation_test.csv")
SEXI <- rd(S09, "10min", "tables", "primary_feature_sex_interactions.csv")
S09_5 <- list(assoc = file.path(S09, "5min", "tables", "primary_movement_entropyacf1_associations.csv"),
              perf = file.path(S09, "5min", "tables", "primary_prediction_performance.csv"))

# C2: one estimates table with stable result ids
C2 <- rbindlist(list(
  EST[, .(source = "stage29", model_id, construct, estimand, sex, estimate, se, df, statistic, ci_low, ci_high, p_raw, test, L, status, role)],
  rd(T29, "exposure_estimates.csv")[, .(source = "stage29_exposure", model_id, construct, estimand, sex, estimate, se, df, statistic, ci_low, ci_high, p_raw, test, L)],
  rd(T29, "continuous_estimates.csv")[, .(source = "stage29_continuous", model_id = paste("CONTINUOUS", population, predictor, sep = "|"),
     construct = predictor, estimand = paste(population, estimand, sep = "|"), sex = NA_character_, estimate, se, df, statistic = estimate / se,
     ci_low, ci_high, p_raw = NA_real_, test = "OLS t (CR2 SE in robustness)", L = NA_character_, n = n, se_cr2, df_cr2)],
  rd(T29, "lag_block_estimates.csv")[, .(source = "stage29_lag_block", model_id, construct = metric, estimand = paste(estimand, resolution, sep = "|"),
     sex, estimate, se, df, statistic, ci_low, ci_high, p_raw, test, L)],
  rd(T29, "cumulative_window_estimates.csv")[, .(source = "stage29_cumulative", model_id, construct, estimand = paste0(estimand, "|", window_h, "h"),
     sex, estimate, se, df, statistic, ci_low, ci_high, p_raw, test, L)],
  ASSOC[, .(source = "stage09", model_id = "stage09_association", construct = feature, estimand = "spearman_rho_vs_CombZ", sex = NA_character_,
     estimate = spearman_rho, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = spearman_boot_ci_low, ci_high = spearman_boot_ci_high,
     p_raw = spearman_p, test = "Spearman; 5000-sample percentile bootstrap CI", L = NA_character_, n = n)],
  PERF[, .(source = "stage09", model_id = paste0("stage09_", model_id), construct = model_id, estimand = "LOAO_R2", sex = NA_character_,
     estimate = cv_r2, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = cv_r2_q025, ci_high = cv_r2_q975, p_raw = permutation_p,
     test = "LOAO R2 vs full-sample mean; interval = 2.5-97.5% of repeated grouped 5-fold CV (not a CI)", L = NA_character_, n = n_animals)],
  PERF[, .(source = "stage09", model_id = paste0("stage09_", model_id), construct = model_id, estimand = "repeated_CV_mean_R2", sex = NA_character_,
     estimate = repeated_cv_mean_r2, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = cv_r2_q025, ci_high = cv_r2_q975, p_raw = NA_real_,
     test = "100 x repeated grouped 5-fold CV; interval = quantiles across repeats", L = NA_character_, n = n_animals)],
  SEXI[, .(source = "stage09", model_id = "stage09_feature_x_sex", construct = feature, estimand = "feature_x_Sex_interaction", sex = NA_character_,
     estimate = interaction_estimate, se = interaction_se, df = NA_real_, statistic = NA_real_, ci_low = interaction_ci_low, ci_high = interaction_ci_high,
     p_raw = interaction_p, test = "lm(CombZ ~ feature * Sex), as registered", L = NA_character_, n = n)]
), fill = TRUE)
C2[, result_id := paste(source, construct, estimand, ifelse(is.na(sex), "pooled", sex), sep = "|")]
C2[REG29, on = "model_id", `:=`(n = data.table::fcoalesce(as.numeric(n), as.numeric(i.n_animals)), n_obs = i.n_obs, n_cage_episodes = i.n_cage_episodes)]
if (anyDuplicated(C2$result_id)) stop("Duplicated result ids: ", paste(head(C2$result_id[duplicated(C2$result_id)]), collapse = ", "), call. = FALSE)
C2[, unit := data.table::fcase(construct == "crossing_rate", "crossings/hour", construct == "shared_zone_use", "fraction of dyadic observation time",
       construct == "occupancy_dispersion", "bits", construct == "fragmentation", "proportion of bouts",
       construct == "light_phase_crossing_rate", "crossings/hour", default = NA_character_)]
sdz <- c(crossing_rate = CFG$metrics$crossing_rate$standardizer_sd, shared_zone_use = CFG$metrics$shared_zone_use$standardizer_sd,
         occupancy_dispersion = CFG$metrics$occupancy_dispersion$standardizer_sd, fragmentation = CFG$metrics$fragmentation$standardizer_sd)
C2[construct %in% names(sdz) & source == "stage29", estimate_standardized := estimate / sdz[construct]]

# E: behavioural families + Stage 09 prediction families
E <- rbindlist(list(
  MULT[, .(family_id, tier, member, method, declared_m, realised_m, p_raw, p_adjusted, reject_at_alpha_adjusted, discovery_eligible = TRUE)],
  ASSOC[, .(family_id = "PR1", tier = "prediction (Stage 09, registered)", member = paste0("spearman|", feature), method = "BH", declared_m = 3L,
            realised_m = .N, p_raw = spearman_p, p_adjusted = spearman_p_bh, reject_at_alpha_adjusted = spearman_p_bh < 0.05, discovery_eligible = TRUE)],
  PERM[, .(family_id = "PR2", tier = "prediction (Stage 09)", member = paste0("permutation|", model), method = "holm", declared_m = 2L, realised_m = .N,
           p_raw = empirical_p, p_adjusted = stats::p.adjust(empirical_p, "holm"), reject_at_alpha_adjusted = stats::p.adjust(empirical_p, "holm") < 0.05,
           discovery_eligible = TRUE)],
  SEXI[, .(family_id = "PR3", tier = "prediction (Stage 09, registered)", member = paste0("feature_x_Sex|", feature), method = "BH", declared_m = 3L,
           realised_m = .N, p_raw = interaction_p, p_adjusted = interaction_p_bh, reject_at_alpha_adjusted = interaction_p_bh < 0.05, discovery_eligible = TRUE)]
), fill = TRUE)
if (any(E$declared_m != E$realised_m)) stop("A family's size differs from its declaration.", call. = FALSE)
E0 <- rbindlist(lapply(names(CFG$multiplicity$families), function(f) { x <- CFG$multiplicity$families[[f]]
  data.table(family_id = f, tier = x$tier, question = x$question %||% NA_character_, members = paste(x$members %||% x$rule, collapse = "; "),
             method = x$method, declared_m = x$m) }))
HET <- rd(T29, "heteroscedastic.csv"); ZONE <- rd(T29, "shared_zone_robustness.csv"); LOBS <- rd(T29, "lobo_sign_stability.csv")
R <- rbindlist(list(
  rd(T29, "sensitivities.csv")[, block := "sensitivity"],
  HET[, block := "heteroscedastic"], ZONE[, block := "shared_zone"],
  rd(T29, "lobo.csv")[, block := "lobo"], LOBS[, block := "lobo_sign_stability"],
  rd(T29, "diagnostics.csv")[, block := "diagnostics"],
  rd(T29, "stage09_cv_sensitivities.csv")[, block := "stage09_cv"],
  rd(T29, "stage09_batch_adjusted_sex_interaction.csv")[, block := "stage09_batch_adjusted"]), fill = TRUE)
R[, discovery_eligible := FALSE]

# P: one row per primary/secondary tested estimand with every reporting$primary_fields / q2b_fields element
C0r <- REG29
P <- rbindlist(lapply(c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation"), function(k) {
  tier <- CFG$metrics[[k]]$tier
  rbindlist(lapply(c("Q1", "Q2b"), function(q) {
    mid <- paste0(if (q == "Q1") "CC1_POOLED|" else "TR_POOLED|", k); mi <- C0r[model_id == mid]
    fam <- MULT[member == paste0(q, "|", k)]
    h <- HET[construct == k & estimand == q]
    row <- data.table(construct = k, estimand = q, tier = tier, family_id = fam$family_id, model_id = mid,
      n_animals = mi$n_animals, n_batches = mi$n_batches, n_cage_episodes = mi$n_cage_episodes, n_obs = mi$n_obs,
      p_raw = fam$p_raw, p_holm = fam$p_adjusted, reject_holm = fam$reject_at_alpha_adjusted,
      heteroscedastic_A_p = h[comparator == "A", p_raw], heteroscedastic_A_material = h[comparator == "A", material],
      heteroscedastic_B_p = h[comparator == "B", p_raw], heteroscedastic_B_material = h[comparator == "B", material],
      variance_model_sensitive = any(h$material), config_version = CFG$meta$config_version)
    if (q == "Q1") { e <- EST[model_id == mid & estimand == "Q1"]
      row[, `:=`(estimate = e$estimate, se = e$se, ci_low = e$ci_low, ci_high = e$ci_high, df = e$df, estimate_standardized = e$estimate_standardized)]
      l <- LOBS[construct == k & estimand == "Q1"]; row[, lobo_sign_stability := sprintf("%d/%d", l$n_same_sign, l$required)]
    } else { j <- JT[model_id == mid & estimand == "Q2b"]
      row[, `:=`(F = j$F, df1 = j$df1, df2 = j$df2)]
      l <- LOBS[construct == k & grepl("^Q2b_component", estimand)]
      row[, lobo_sign_stability := paste(sprintf("%s %d/%d", sub("Q2b_component_", "", l$estimand), l$n_same_sign, l$required), collapse = "; ")] }
    if (k == "shared_zone_use") {
      d1 <- ZONE[analysis == if (q == "Q1") "D1_CR2_CC1" else "D1_twoway_TR"]
      d2 <- ZONE[grepl("^D2", analysis) & primary_compared %in% (if (q == "Q1") "Q1" else paste0("Q2b_component_c", 2:4))]
      row[, `:=`(d1_p = d1$p_raw, d1_supports = d1$d1_supports, strong_wording_allowed = isTRUE(fam$reject_at_alpha_adjusted) & isTRUE(d1$d1_supports),
                 d2_same_direction = all(d2$same_direction), d2_broadly_compatible = all(d2$broadly_compatible),
                 d2_clear_contradiction = any(d2$clear_contradiction))]
    }
    row }), fill = TRUE) }), fill = TRUE)
if (nrow(P) != 8) stop("Primary results table is incomplete.", call. = FALSE)
P[, status := ifelse(is.na(p_raw), "FAILED", "OK")]
for (f in S09_5) if (file.exists(f)) R <- rbind(R, rd(f)[, `:=`(block = paste0("stage09_5min|", basename(f)), discovery_eligible = FALSE)], fill = TRUE)
F <- rbindlist(lapply(c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation", "light_phase_crossing_rate"), function(k) {
  m <- CFG$metrics[[k]]
  data.table(metric = k, tier = m$tier, binning = m$binning, needs_binning = FALSE, primary_resolution = "bin-free", sensitivity_resolution = NA_character_,
             bout_criterion_s = m$bout_criterion_s %||% NA_real_, decision_basis = m$decision_basis,
             group_results_seen = CFG$meta$group_results_seen_before_freeze, definition = m$definition) }))
lb <- CFG$metrics$descriptive$lag_block
F <- rbind(F, rbindlist(lapply(c("Movement_rmssd", "Movement_acf1", "Proximity_rmssd", "Proximity_acf1"), function(k)
  data.table(metric = k, tier = "descriptive", binning = "binned", needs_binning = TRUE, primary_resolution = lb[[k]][["primary"]],
             sensitivity_resolution = lb[[k]][["sensitivity"]], bout_criterion_s = NA_real_, decision_basis = "POST_HOC_JUDGEMENT",
             group_results_seen = CFG$meta$group_results_seen_before_freeze, definition = lb$estimator))), fill = TRUE)
G <- rbind(
  W[, .(n_animals = uniqueN(AnimalNum), n_windows = .N, n_missing_shared_zone = sum(!is.finite(shared_zone_use)),
        n_light_phase_incomplete = sum(light_complete %in% FALSE), n_fewer_than_8_positions = sum(hardware_flag %in% TRUE)), by = .(CC, Sex, Batch, Group)],
  data.table(CC = "all", Sex = "Male", Batch = "B1", Group = "untracked", n_animals = 6L, n_windows = 0L),
  fill = TRUE)
C0 <- REG29

# ---------------------------------------------------------------- write (immutable version)
commit <- git("rev-parse", "HEAD")[1]
bundle_id <- sprintf("ebb_v%s_%s_%s", gsub("\\.", "", CFG$meta$config_version), format(Sys.Date(), "%Y%m%d"), substr(commit, 1, 7))
if (DRY) bundle_id <- paste0(bundle_id, "_dry", format(Sys.time(), "%H%M%S"))
STATUS <- if (DRY) "DRY_RUN_NOT_FOR_USE" else "FROZEN"
BROOT <- if (DRY) file.path(S29, "bundle") else file.path(AR, "canonical", "behavior_bundle"); BD <- file.path(BROOT, bundle_id)
if (dir.exists(BD)) stop("Bundle version exists (immutable): ", BD, call. = FALSE)
dir.create(BD, recursive = TRUE)
files <- list(A0_design_timeline = A0, A4_combz_animals = A4, P_primary_results = P, B2_descriptive_summaries = rd(T29, "descriptive_summaries.csv"),
              V_runtime_gates = rbindlist(list(rd(T29, "validation_anchor_vs_stage28.csv")[, gate := "anchor"], rd(T29, "validation_window_coverage.csv")[, gate := "window_coverage"]), fill = TRUE),
              A1_animal_cc1 = A1, B1_animal_longitudinal = B1, A2_prediction_animals = A2, A2b_combz_thresholds = thr, A3_permutation_draws = A3,
              C0_models = C0, C2_estimates = C2, C3_joint_tests = JT, E0_family_registry = E0, E_multiplicity = E, R_robustness = R,
              F_resolution_decisions = F, G_sample_sizes = G,
              D_planned_contrasts = rbindlist(lapply(names(CFG$contrasts), function(n) { x <- CFG$contrasts[[n]]; if (!is.list(x)) return(NULL)
                data.table(contrast_id = n, model = paste(x$model %||% x$models, collapse = "; "),
                           L = paste(c(if (!is.null(x$L)) sprintf("%s=%g", names(unlist(x$L)), unlist(x$L)), x$L_rows), collapse = "; "),
                           meaning = x$meaning %||% NA_character_, role = x$role %||% x$test %||% NA_character_) }), fill = TRUE))
for (n in names(files)) data.table::fwrite(files[[n]], file.path(BD, paste0(n, ".csv")))
if (DRY) writeLines(mmm_behavior_config_json(CFG), file.path(BD, "I_analysis_config.json"), useBytes = TRUE) else
  file.copy(file.path(frozen_dir, "behavior_analysis_config.json"), file.path(BD, "I_analysis_config.json"))
writeLines(cfg_sha, file.path(BD, "I_config_sha256.txt"))
H <- data.table(key = c("bundle_id", "status", "generated_at", "generator", "mmm_git_commit", "mmm_branch", "stage29_run_commit", "config_id",
                        "config_version", "config_sha256", "stage29_started_at", "r_version", "packages", "active_window", "primary_population"),
                value = c(bundle_id, STATUS, format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), "Analysis/16b_canonical_behavior_bundle.R", commit,
                          git("branch", "--show-current")[1], run$git_commit, CFG$meta$config_id, CFG$meta$config_version, cfg_sha, run$started_at,
                          run$r_version, run$packages, CFG$windows$primary$definition, CFG$population$primary$definition))
data.table::fwrite(H, file.path(BD, "H_provenance.csv"))
s09_inputs <- c(file.path(S09, "10min", "tables", c("primary_prediction_predictions.csv", "model_ladder_input.csv", "early_prediction_permutation_draws.csv",
                "primary_movement_entropyacf1_associations.csv", "primary_prediction_performance.csv", "primary_prediction_permutation_test.csv",
                "primary_feature_sex_interactions.csv")), unlist(S09_5)[file.exists(unlist(S09_5))],
                file.path(AR, "canonical", "later_outcome_combz", "tables", "combz_classification_thresholds.csv"))
H2 <- rbind(inp[, .(input, bytes, sha256, consumer = "stage29")],
            data.table(input = s09_inputs, bytes = file.size(s09_inputs), sha256 = vapply(s09_inputs, sha_file, ""), consumer = "stage16b"),
            data.table(input = file.path(T29, out_man$file), bytes = out_man$bytes, sha256 = out_man$sha256, consumer = "stage16b (Stage 29 outputs)"))
data.table::fwrite(H2, file.path(BD, "H2_inputs.csv"))
fl <- list.files(BD, full.names = TRUE)
man <- data.table(file = basename(fl), bytes = file.size(fl), sha256 = vapply(fl, sha_file, ""), schema_version = "1")
data.table::fwrite(man, file.path(BD, "00_manifest.csv"))
Sys.chmod(list.files(BD, full.names = TRUE), mode = "0444")
reg <- file.path(BROOT, "BUNDLE_REGISTRY.csv")
data.table::fwrite(data.table(bundle_id = bundle_id, status = STATUS, manifest_sha256 = sha_file(file.path(BD, "00_manifest.csv")),
                              config_sha256 = cfg_sha, mmm_git_commit = commit, created_at = H[key == "generated_at", value]),
                   reg, append = file.exists(reg))
message("Bundle ", bundle_id, " ", STATUS, " at ", BD)
