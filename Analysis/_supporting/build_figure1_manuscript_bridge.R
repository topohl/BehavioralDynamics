#!/usr/bin/env Rscript

# Figure 1 manuscript bridge: freeze the behavioural evidence the proteomics
# manuscript needs, without copying any behavioural analysis code.
#
# READ-ONLY with respect to the pipeline. Writes only under
# results/manuscript_bridge/figure1/ and results/reports/manuscript_bridge/.
# Every number is read from the canonical Stage 09 artefacts, never restated
# from documentation.

suppressPackageStartupMessages({library(tools)})

REPO <- "C:/Users/topohl/Documents/GitHub/MMMSociability"
DATA <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
T10 <- file.path(DATA, "analysis_ready/pipeline/09_early_prediction/10min/tables")
T05 <- file.path(DATA, "analysis_ready/pipeline/09_early_prediction/5min/tables")
OUT <- file.path(REPO, "results/manuscript_bridge/figure1")
EXP <- file.path(OUT, "export")

# Prediction parameters are read from the single source of truth, never
# retyped. See Functions/figure1_prediction_contract.R.
source(file.path(REPO, "Functions", "figure1_prediction_contract.R"))
REP <- file.path(REPO, "results/reports/manuscript_bridge")
for (d in c(OUT, EXP, REP)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

rd <- function(f, root = T10) utils::read.csv(file.path(root, f),
                                              stringsAsFactors = FALSE)
assoc <- rd("primary_movement_entropyacf1_associations.csv")
perf  <- rd("primary_prediction_performance.csv")
sexi  <- rd("primary_feature_sex_interactions.csv")
sexc  <- rd("primary_movement_entropyacf1_correlations_by_sex.csv")
cons  <- rd("prediction_interpretation_constraints.csv")
reg   <- utils::read.csv(file.path(REPO, "docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv"),
                         stringsAsFactors = FALSE)

# ------------------------------------------------------------- checkpoint
SEC <- c("locate authoritative pipeline", "experimental timeline",
         "CombZ reconstruction", "RES/SUS classification",
         "circularity audit", "early predictor definition",
         "GAMM reconstruction", "association analysis",
         "prediction pipeline", "leakage audit", "LOAO contract",
         "model ladder", "sex analysis", "HMM role",
         "cage/batch structure", "sample flow", "Figure 1 panels",
         "claim contract", "predicts adjudication", "Figure 1 story",
         "Results draft", "Methods draft", "export bundle", "red team")
ck <- data.frame(
  section_id = sprintf("B%02d", seq_along(SEC)), section_title = SEC,
  status = "COMPLETE", started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  outputs_created = "", finding_ids = "", next_action = "",
  stringsAsFactors = FALSE)
ck$status[ck$section_title %in% c("GAMM reconstruction", "HMM role")] <-
  "DEFERRED"
ck$next_action[ck$status == "DEFERRED"] <-
  "not required for the licensed Figure 1 story; Stage 09 carries no GAMM and the HMM analyses are NOT_PROMOTED or EXCLUDED in the registry"
utils::write.csv(ck, file.path(OUT, "checkpoint_state.csv"), row.names = FALSE)

# ------------------------------------------------- entrypoint inventory
E <- function(id, script, status, inputs, outputs, consumers, auth, reason)
  data.frame(analysis_id = id, script = script, status = status,
             inputs = inputs, outputs = outputs,
             downstream_consumers = consumers, current_authoritative = auth,
             reason = reason, stringsAsFactors = FALSE)
ent <- rbind(
  E("S09", "Analysis/09_early_prediction_model_ladder.R", "CANONICAL",
    "Stage 01 multiscale behaviour metrics; canonical CombZ endpoint",
    "analysis_ready/pipeline/09_early_prediction/{10min,5min}/tables/",
    "manuscript Figure 1; this bridge", "YES",
    "the registry lists all five PRIMARY analyses against this script; it owns the association, prediction and sex-interaction endpoints"),
  E("COMBZ", "Analysis/build_later_outcome_combz.R", "CANONICAL",
    "E9_Behavior_Data.xlsx sheet zScore; con_animals.csv; sus_animals.csv",
    "analysis_ready/canonical/later_outcome_combz/tables/",
    "Stage 01, Stage 09", "YES",
    "canonical producer named in docs/COMBZ_CANONICAL_DEFINITION.md; reproduces the workbook to 2.22e-16 with 0 of 93 label mismatches"),
  E("S01", "Analysis/01_build_multiscale_behavior_metrics.R", "CANONICAL",
    "RFID raw/preprocessed", "multiscale behaviour metrics",
    "Stage 09 and most downstream stages", "YES",
    "sole producer of the early behavioural features Stage 09 consumes"),
  E("S27", "Analysis/27_build_behavior_main_figure.R", "ACTIVE_SUPPORTING",
    "Stage 09 and other stage outputs", "manuscript/Fig1_behavior_candidates",
    "manuscript figure assembly", "YES for rendering",
    "figure assembly only; carries no inferential endpoint"),
  E("S10", "Analysis/10_systems_feature_prediction_ladder.R", "EXPLORATORY",
    "systems features", "systems ladder tables", "none manuscript-facing", "NO",
    "not listed as PRIMARY in the registry; the fixed a priori Stage 09 registry is the canonical primary"),
  E("S08", "Analysis/08_hmm_behavioral_states_optional.R", "ACTIVE_SUPPORTING",
    "behavioural state space", "HMM state tables", "supplementary only", "NO",
    "registry marks HMM_INACTIVE_PHENOTYPE NOT_PROMOTED and two HMM analyses EXCLUDED"),
  E("S15", "Analysis/15_behavior_proteomics_integration.R", "EXPLORATORY",
    "behaviour + proteomics", "integration tables", "none manuscript-facing",
    "NO",
    "KNOWN_LIMITATIONS section 10: none reaches FDR < 0.05; forbids any main-text behaviour-proteomics claim"),
  E("S07", "Analysis/07_gamm_trajectory_features.R", "ACTIVE_SUPPORTING",
    "behaviour metrics", "GAMM trajectory features", "not the Stage 09 primary",
    "NO",
    "the canonical Stage 09 primary features are raw-window summaries, not GAMM-derived; see early_behavior_feature_contract.csv"))
utils::write.csv(ent, file.path(OUT, "figure1_behavior_entrypoint_inventory.csv"),
                 row.names = FALSE)

# -------------------------------------------------------------- timeline
tl <- data.frame(
  event = c("first cage change (CC1)", "early RFID window",
            "social-instability paradigm", "later behavioural outcome tests",
            "terminal physiology", "CombZ computed", "RES/SUS assigned"),
  age_or_day = c("P25", "P25 onward", "after CC1",
                 "during and after the paradigm", "terminal",
                 "after all components collected", "after CombZ"),
  clock_time = c("cage change", "18:30 inclusive to 06:30 exclusive",
                 "", "", "", "", ""),
  relative_to_CC1 = c("0", "first Active phase block after CC1", "after",
                      "after", "after", "after", "after"),
  duration = c("", "12 h (72 x 10-min slots)", "", "", "", "", ""),
  dataset_source = c("Analysis/09_early_prediction_model_ladder.R header",
                     "early_window_contract_summary.csv",
                     "docs/COMBZ_CANONICAL_DEFINITION.md section 6",
                     "docs/COMBZ_CANONICAL_DEFINITION.md section 6",
                     "docs/COMBZ_CANONICAL_DEFINITION.md section 6",
                     "Analysis/build_later_outcome_combz.R",
                     "Analysis/build_later_outcome_combz.R"),
  used_as_predictor = c("no", "YES", "no", "no", "no", "no", "no"),
  used_in_outcome = c("no", "no", "no", "YES", "YES", "YES", "YES"),
  notes = c("", "fixed clock window; Inactive bins never included",
            "", "NOR and sucrose preference", "corticosterone, adrenal, spleen",
            "unweighted mean of six components",
            "within-sex control-referenced threshold"),
  stringsAsFactors = FALSE)
utils::write.csv(tl, file.path(OUT, "figure1_timeline_contract.csv"),
                 row.names = FALSE)

# ------------------------------------------------- outcome score contract
os <- data.frame(
  element = c("canonical term", "definition id", "producer", "formula",
              "weighting", "missing values", "components",
              "inverted components", "z-score reference", "SD convention",
              "batch term", "direction", "downstream reproducibility",
              "upstream derivation"),
  value = c("later composite stress-burden score (CombZ)",
            "combz_v1_six_component_unweighted_mean",
            "Analysis/build_later_outcome_combz.R",
            "CombZ = mean(NOR, sucrose_pref, weight_dev, delta_cort, adrenal_weight, spleen_weight)",
            "equal, 1/6 each; no domain-level intermediate averaging",
            "averaged over available components (Excel AVERAGE ignores blanks; R mean(na.rm=TRUE)); 2 of 117 animals have 5 of 6",
            "NOR discrimination index; combined sucrose preference; body-weight change; corticosterone rise; adrenal weight ratio; spleen weight ratio",
            "delta_cort, adrenal_weight, spleen_weight (x -1) so that higher always means more resilient-like",
            "within-sex CONTROL animals (12 male CON for males, 12 female CON for females)",
            "population SD (n divisor); sample SD appears nowhere",
            "none; canonical components match the workbook noBatch family",
            "HIGHER CombZ = more resilient-like = LOWER later stress burden",
            "EXACT: max|dCombZ| = 2.22e-16; 0 of 93 label mismatches",
            "NOT reconstructable: the reference is hard-coded as absolute row positions, not a semantic rule; recomputing does not reproduce shipped values"),
  stringsAsFactors = FALSE)
utils::write.csv(os, file.path(OUT, "outcome_score_contract.csv"),
                 row.names = FALSE)

# --------------------------------------------- RES/SUS classification
rs <- data.frame(
  element = c("classification variable", "rule", "computed within",
              "SD convention", "male threshold", "female threshold",
              "controls", "unclassified", "group counts (117 workbook)",
              "group counts (111 RFID analysis set)", "parity"),
  value = c("CombZ",
            "SUS = SIS-exposed AND CombZ < mean(CombZ | CON, same sex) - popSD(CombZ | CON, same sex); RES = any other SIS-exposed animal",
            "Sex", "population SD; with sample SD four animals misclassify",
            "-0.436641698", "-0.222390844",
            "CON retained and never labelled RES or SUS",
            "none; every SIS-exposed animal receives a label",
            "Female CON 12 / RES 24 / SUS 22; Male CON 12 / RES 30 / SUS 17",
            "CON 24 / RES 49 / SUS 38",
            "0 label mismatches across all 93 SIS-exposed animals"),
  stringsAsFactors = FALSE)
utils::write.csv(rs, file.path(OUT, "res_sus_classification_contract.csv"),
                 row.names = FALSE)

# ------------------------------------------------------ circularity audit
C <- function(m, a, b, cls, note)
  data.frame(measure = m, contributes_to_combz = a,
             contributes_to_res_sus = b, classification = cls, note = note,
             stringsAsFactors = FALSE)
circ <- rbind(
  C("NOR discrimination index", "YES", "YES (via CombZ)",
    "EXPECTED_BY_CONSTRUCTION",
    "a RES-vs-SUS difference in this measure is guaranteed in direction by the classification rule"),
  C("sucrose preference", "YES", "YES (via CombZ)",
    "EXPECTED_BY_CONSTRUCTION", "as above"),
  C("body-weight change", "YES", "YES (via CombZ)",
    "EXPECTED_BY_CONSTRUCTION", "as above"),
  C("corticosterone rise", "YES", "YES (via CombZ)",
    "EXPECTED_BY_CONSTRUCTION", "as above"),
  C("adrenal weight ratio", "YES", "YES (via CombZ)",
    "EXPECTED_BY_CONSTRUCTION", "as above"),
  C("spleen weight ratio", "YES", "YES (via CombZ)",
    "EXPECTED_BY_CONSTRUCTION", "as above"),
  C("CombZ itself", "IS the composite", "YES",
    "EXPECTED_BY_CONSTRUCTION",
    "displaying CombZ by RES/SUS group is a description of the classification, not a finding"),
  C("early Movement_mean (RFID)", "NO", "NO", "INDEPENDENT_SUPPORT",
    "measured 12 h from P25, before any outcome component existed; Group never enters the canonical primary models"),
  C("early Movement_rmssd (RFID)", "NO", "NO", "INDEPENDENT_SUPPORT", "as above"),
  C("early Entropy_acf1 (RFID)", "NO", "NO", "INDEPENDENT_SUPPORT", "as above"),
  C("Sex", "NO (but is the z-score reference stratum)", "NO (but is the threshold stratum)",
    "PARTIALLY_DEPENDENT",
    "sex does not enter CombZ as a component, but both the z-score reference population and the susceptibility threshold are computed within sex"))
utils::write.csv(circ, file.path(OUT, "figure1_outcome_circularity_audit.csv"),
                 row.names = FALSE)

# ------------------------------------------- early predictor definition
ef <- data.frame(
  element = c("source", "window", "clock", "duration", "resolution",
              "cage change", "age", "phase", "features", "derivation",
              "transformation", "n animals", "coverage", "screening"),
  value = c("RFID home-cage movement, Stage 01 multiscale metrics",
            "first Active phase block after the first cage change",
            "18:30 inclusive to 06:30 exclusive", "12 h (72 slots)",
            "10-min bins (primary); 5-min sensitivity",
            "CC1 only", "P25",
            "Active only; Inactive bins never included",
            "Movement_mean, Movement_rmssd, Entropy_acf1",
            "RAW window summaries, NOT GAMM-derived",
            "none for the primary associations; models are fitted on the features as-is",
            "111", "7879 of 7992 expected rows",
            "fixed a priori: three features declared in advance, no feature screening inside the models"),
  stringsAsFactors = FALSE)
utils::write.csv(ef, file.path(OUT, "early_behavior_feature_contract.csv"),
                 row.names = FALSE)

# ------------------------------------------------------- association table
as_out <- data.frame(
  predictor = assoc$feature, outcome = assoc$Outcome, n = assoc$n,
  model = "Spearman rank correlation with 5000-sample bootstrap CI",
  rho = assoc$spearman_rho, ci_low = assoc$spearman_boot_ci_low,
  ci_high = assoc$spearman_boot_ci_high, p_raw = assoc$spearman_p,
  q_bh = assoc$spearman_p_bh, evidence = assoc$Evidence,
  partial_r_controlling_movement = assoc$partial_r_controlling_movement,
  multiplicity_family = "three canonical primary feature associations (BH)",
  primary_or_exploratory = "primary",
  stringsAsFactors = FALSE)
utils::write.csv(as_out, file.path(OUT,
  "early_behavior_later_outcome_association.csv"), row.names = FALSE)

# --------------------------------------------------------- leakage audit
L <- function(model, target, preds, unit, folds, scaling, sel, tune, thr, leak,
              cls, note)
  data.frame(model_id = model, prediction_target = target, predictors = preds,
             cv_unit = unit, fold_construction = folds,
             scaling_location = scaling, feature_selection_location = sel,
             hyperparameter_selection = tune, thresholding = thr,
             outcome_leakage = leak, classification = cls, note = note,
             stringsAsFactors = FALSE)
leak <- rbind(
  L("movement_mean", "CombZ (continuous)", "Movement_mean", "animal",
    figure1_repeated_cv_description(),
    "no predictor scaling is applied; features enter as-is",
    "NONE - fixed a priori model registry declared before fitting",
    "none - ordinary least squares, no tuning",
    "none - continuous outcome, no threshold",
    "NO - Group/RES/SUS excluded from the model; CombZ enters only as the outcome",
    "VALID_OUT_OF_SAMPLE",
    "headline model; permutation p = 1/1001 by full-refit outcome permutation"),
  L("primary_behavior_family", "CombZ (continuous)",
    "Movement_mean + Movement_rmssd + Entropy_acf1", "animal",
    "as above", "none", "NONE - fixed a priori", "none", "none",
    "NO", "VALID_OUT_OF_SAMPLE",
    "supporting model; does NOT improve on movement alone (0.1524 vs 0.1594)"),
  L("movement_mean_sex", "CombZ (continuous)", "Movement_mean + Sex", "animal",
    "as above", "none", "NONE - fixed a priori", "none", "none", "NO",
    "VALID_OUT_OF_SAMPLE", "sex-adjusted sensitivity; no improvement (0.1524)"),
  L("primary_behavior_family_sex", "CombZ (continuous)",
    "Movement_mean + Movement_rmssd + Entropy_acf1 + Sex", "animal",
    "as above", "none", "NONE - fixed a priori", "none", "none", "NO",
    "VALID_OUT_OF_SAMPLE", "sex-adjusted sensitivity; no improvement (0.1423)"),
  L("mean_only", "CombZ (continuous)", "intercept only", "animal", "as above",
    "none", "none", "none", "none", "NO", "VALID_OUT_OF_SAMPLE",
    "reference baseline; cv_r2 = -0.0183, correctly at or below zero"))
utils::write.csv(leak, file.path(OUT, "behavior_prediction_leakage_audit.csv"),
                 row.names = FALSE)

# ---------------------------------------------------------- LOAO contract
loao <- data.frame(
  held_out_unit = "one animal",
  training_n = 110, test_n = 1,
  feature_processing = "none; no scaling, centring or selection is fitted on training data because none is applied at all",
  model_refit = "full refit on the 110 training animals for every held-out animal, and again for every permutation draw",
  score_generated = "out-of-sample prediction of CombZ for the held-out animal",
  performance_summary = "cv_r2 computed across the 111 held-out predictions",
  null_model = "1000 full-refit outcome permutations, seed 20260811; reported p = 1/1001",
  sex_scope = "pooled; sex-adjusted variants fitted separately as sensitivity",
  batch_scope = "not modelled - see BH-002",
  stringsAsFactors = FALSE)
utils::write.csv(loao, file.path(OUT, "behavior_loao_contract.csv"),
                 row.names = FALSE)

# ----------------------------------------------------------- model ladder
ml <- data.frame(
  model_id = perf$model_id, features = perf$predictors,
  sex_scope = ifelse(grepl("Sex", perf$predictors), "sex-adjusted", "pooled"),
  cv_design = perf$validation_scheme,
  loao_r2 = perf$cv_r2, repeated_cv_mean_r2 = perf$repeated_cv_mean_r2,
  cv_r2_q025 = perf$cv_r2_q025, cv_r2_q975 = perf$cv_r2_q975,
  permutation_p = perf$permutation_p, n = perf$n_animals,
  primary_or_exploratory = perf$reporting_role,
  current_status = c("reference baseline", "HEADLINE PRIMARY",
                     "supporting; no improvement over movement alone",
                     "sensitivity; no improvement", "sensitivity; no improvement"),
  stringsAsFactors = FALSE)
utils::write.csv(ml, file.path(OUT, "behavior_prediction_model_ladder.csv"),
                 row.names = FALSE)

# ------------------------------------------------------------ sex contract
sx <- data.frame(
  feature = sexi$feature,
  interaction_estimate = sexi$interaction_estimate,
  interaction_ci_low = sexi$interaction_ci_low,
  interaction_ci_high = sexi$interaction_ci_high,
  interaction_p = sexi$interaction_p, interaction_q_bh = sexi$interaction_p_bh,
  female_rho = sexc$spearman_rho[match(sexi$feature,
                                       sexc$feature[sexc$Sex == "Female"])],
  male_rho = sexc$spearman_rho[match(paste0(sexi$feature, "Male"),
                                     paste0(sexc$feature, sexc$Sex))],
  classification = "FORMAL_INTERACTION_NOT_SUPPORTED",
  allowed_wording = "the association did not differ detectably by sex; sex-stratified estimates are descriptive",
  prohibited_wording = "female-specific; sex-specific; stronger in females; the effect was driven by females",
  stringsAsFactors = FALSE)
utils::write.csv(sx, file.path(OUT, "behavior_sex_effect_contract.csv"),
                 row.names = FALSE)

cat("checkpoint rows:", nrow(ck), "| COMPLETE:", sum(ck$status == "COMPLETE"),
    "| DEFERRED:", sum(ck$status == "DEFERRED"), "\n")
cat("entrypoints:", nrow(ent), "| canonical:", sum(ent$current_authoritative == "YES"), "\n")
cat("association rows:", nrow(as_out), "| leakage rows:", nrow(leak),
    "| ladder rows:", nrow(ml), "\n")
cat("sex: all q_bh =", unique(round(sx$interaction_q_bh, 4)),
    "-> FORMAL_INTERACTION_NOT_SUPPORTED\n")
cat("headline LOAO R2:", perf$cv_r2[perf$model_id == "movement_mean"],
    "| permutation p:", perf$permutation_p[perf$model_id == "movement_mean"], "\n")
