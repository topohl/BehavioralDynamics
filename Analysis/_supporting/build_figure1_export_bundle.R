#!/usr/bin/env Rscript

# Figure 1 export bundle: the frozen artefact the proteomics manuscript repo
# consumes. Carries commit, branch, worktree state and file hashes so a claim can
# be traced back to an exact source state without copying behavioural code.

suppressPackageStartupMessages({library(tools)})

REPO <- "C:/Users/topohl/Documents/GitHub/MMMSociability"
DATA <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
T10 <- file.path(DATA, "analysis_ready/pipeline/09_early_prediction/10min/tables")
OUT <- file.path(REPO, "results/manuscript_bridge/figure1")
EXP <- file.path(OUT, "export")
dir.create(EXP, recursive = TRUE, showWarnings = FALSE)

# Single source of truth for every prediction parameter this bundle reports.
# Sourced rather than retyped: the repeated-CV seed was previously hard-coded
# here as the make_grouped_folds() default (123) instead of the value the
# canonical call site actually passes (521).
source(file.path(REPO, "Functions", "figure1_prediction_contract.R"))

git <- function(...) {
  o <- suppressWarnings(system2("git", c("-C", shQuote(REPO), ...),
                                stdout = TRUE, stderr = FALSE))
  if (!length(o)) NA_character_ else o[1]
}
SHA <- git("rev-parse", "HEAD")
BRANCH <- git("rev-parse", "--abbrev-ref", "HEAD")
# Worktree state of the SOURCE, excluding this bundle's own output tree.
#
# The artefact describes the state of the code that produced it, and that state
# is not made dirty by the act of writing the artefact. Assessing the whole tree
# is what made the previous freeze record "dirty at export time" even though the
# analysis and the builders were fully committed: the bridge builder had just
# written its own files moments earlier. Excluding the output directory makes
# the recorded value mean what a reader takes it to mean - whether the code this
# bundle came from is committed.
BUNDLE_TREE <- "results/manuscript_bridge/figure1"
DIRTY <- length(suppressWarnings(system2("git",
  c("-C", shQuote(REPO), "status", "--porcelain", "--untracked-files=all",
    "--", ".", shQuote(paste0(":(exclude)", BUNDLE_TREE, "/**"))),
  stdout = TRUE, stderr = FALSE))) > 0
NOW <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

perf <- utils::read.csv(file.path(T10, "primary_prediction_performance.csv"),
                        stringsAsFactors = FALSE)
assoc <- utils::read.csv(file.path(T10,
  "primary_movement_entropyacf1_associations.csv"), stringsAsFactors = FALSE)
sexi <- utils::read.csv(file.path(T10, "primary_feature_sex_interactions.csv"),
                        stringsAsFactors = FALSE)
g <- function(f, col) assoc[[col]][assoc$feature == f]
p <- function(m, col) perf[[col]][perf$model_id == m]
fmt <- function(x, d = 3) formatC(x, format = "g", digits = d)

# ------------------------------------------------------------ claim contract
C <- function(id, text, cls, src, n, stat, allow, prohib, panel, status)
  data.frame(claim_id = id, claim_text_candidate = text, claim_class = cls,
             analysis_source = src, biological_n = n, statistical_support = stat,
             allowed_wording = allow, prohibited_wording = prohib,
             figure_panel = panel, status = status, stringsAsFactors = FALSE)
S09 <- "Analysis/09_early_prediction_model_ladder.R"
claims <- rbind(
  C("F1-01",
    "Later outcome is heterogeneous across stress-exposed animals and is summarised by a composite stress-burden score.",
    "DESCRIPTIVE", "Analysis/build_later_outcome_combz.R", "111 (117 in workbook)",
    "descriptive; no test", "later composite stress-burden score; higher = more resilient-like",
    "behavioural score; depression score; higher = worse", "1a", "READY"),
  C("F1-02",
    "Resilient and susceptible animals differ in the measures that define them.",
    "BY_CONSTRUCTION", "Analysis/build_later_outcome_combz.R", "93 stress-exposed",
    "none - the classification thresholds CombZ, so the direction is guaranteed",
    "describes the classification; the groups are defined by these measures",
    "independent validation of the grouping; group difference as a finding", "1a", "READY"),
  C("F1-03",
    paste0("Mean movement in the first active 12 h after the first cage change is negatively associated with later CombZ (rho = ",
           fmt(g("Movement_mean", "spearman_rho")), ", 95% CI [",
           fmt(g("Movement_mean", "spearman_boot_ci_low")), ", ",
           fmt(g("Movement_mean", "spearman_boot_ci_high")), "], q = ",
           fmt(g("Movement_mean", "spearman_p_bh"), 2), ")."),
    "DIRECT_ASSOCIATION", S09, "111",
    figure1_association_description(),
    "associated with; greater early activity corresponded to greater later stress burden",
    "causes; drives; determines; biomarker", "1c", "READY"),
  C("F1-04",
    paste0("Movement RMSSD over the same window is negatively associated with later CombZ (rho = ",
           fmt(g("Movement_rmssd", "spearman_rho")), ", q = ",
           fmt(g("Movement_rmssd", "spearman_p_bh"), 2), ")."),
    "DIRECT_ASSOCIATION", S09, "111", "as F1-03",
    "supporting association", "independent predictive contribution", "1c", "READY"),
  C("F1-05",
    paste0("Entropy ACF1 is FDR-supported at the primary resolution but carries essentially ",
           "no information beyond mean movement (rho = ",
           fmt(g("Entropy_acf1", "spearman_rho")), ", CI excludes zero, q = ",
           fmt(g("Entropy_acf1", "spearman_p_bh"), 2), "; partial r controlling movement = ",
           fmt(g("Entropy_acf1", "partial_r_controlling_movement"), 2), ")."),
    "LIMITATION", S09, "111", "BH-supported; bootstrap CI excludes zero, by 1e-4",
    "FDR-supported univariately; carries essentially no information beyond mean movement",
    "an independent third predictor; robustly significant", "1c", "READY"),
  C("F1-06",
    paste0("A fixed a priori behaviour-only model using mean movement predicts later CombZ out of sample (LOAO R2 = ",
           fmt(p("movement_mean", "cv_r2")), "; repeated grouped 5-fold mean R2 = ",
           fmt(p("movement_mean", "repeated_cv_mean_r2")), " [",
           fmt(p("movement_mean", "cv_r2_q025")), ", ",
           fmt(p("movement_mean", "cv_r2_q975")), "]; permutation p = 1/1001)."),
    "OUT_OF_SAMPLE_PREDICTION", S09, "111",
    "leave-one-animal-out with full-refit outcome permutation",
    "predicts out of sample; prospective prediction; internal validation",
    "externally validated; replicated; causal; biomarker", "1d", "READY"),
  C("F1-07",
    paste0("Adding features or sex does not improve prediction (three-feature R2 = ",
           fmt(p("primary_behavior_family", "cv_r2")), "; +sex ",
           fmt(p("movement_mean_sex", "cv_r2")), " and ",
           fmt(p("primary_behavior_family_sex", "cv_r2")), " versus ",
           fmt(p("movement_mean", "cv_r2")), " for movement alone)."),
    "DESCRIPTIVE", S09, "111",
    "no model-comparison significance test was performed",
    "did not improve on; performed no better than",
    "significantly worse; the three-feature model is inferior", "1d", "READY"),
  C("F1-08",
    paste0("The feature-CombZ associations do not differ detectably by sex (all interaction q = ",
           fmt(unique(sexi$interaction_p_bh), 2),
           "; stratified rho female -0.41, male -0.42 for mean movement)."),
    "INTERACTION", S09, "111 (58 female, 53 male)",
    "formal feature-by-sex interaction, BH over three tests",
    "did not differ detectably by sex; present in both sexes; sex-stratified estimates are descriptive",
    "female-specific; sex-specific; stronger in females; driven by females", "1e", "READY"),
  C("F1-09",
    "The early window precedes every component of the later outcome, so the classification did not exist at the time of recording.",
    "TEMPORAL", "docs/COMBZ_CANONICAL_DEFINITION.md section 6", "111",
    "design fact, not a test",
    "later outcome group; subsequent phenotype classification; prospective",
    "baseline group; predicted group at recording", "1b", "READY"),
  C("F1-10",
    "Validation is internal; cage identity is not represented in the prediction design.",
    "LIMITATION", S09, "111", "none",
    "internal validation; may be optimistic with respect to cage-level structure",
    "externally validated; generalises to new cohorts", "1d", "READY"))
utils::write.csv(claims, file.path(EXP, "figure1_claim_contract.csv"),
                 row.names = FALSE)

# --------------------------------------------------------- methods contract
M <- function(id, topic, value, src)
  data.frame(methods_id = id, topic = topic, value = value,
             authoritative_source = src, stringsAsFactors = FALSE)
meth <- rbind(
  M("FM-01", "early window",
    "first Active phase block after CC1 at P25; 18:30 inclusive to 06:30 exclusive; 12 h; 10-min bins; 72 slots",
    "early_window_contract_summary.csv"),
  M("FM-02", "early features",
    "Movement_mean, Movement_rmssd, Entropy_acf1; raw window summaries, not GAMM-derived; no scaling or transformation",
    S09),
  M("FM-03", "outcome",
    "CombZ = unweighted mean of six z-scored components; higher = more resilient-like",
    "docs/COMBZ_CANONICAL_DEFINITION.md"),
  M("FM-04", "z-score reference",
    "within-sex control animals; population SD; no batch term",
    "docs/COMBZ_CANONICAL_DEFINITION.md section 4"),
  M("FM-05", "classification",
    "SUS = SIS and CombZ < within-sex control mean minus within-sex control population SD; thresholds -0.437 male, -0.222 female",
    "docs/COMBZ_CANONICAL_DEFINITION.md section 5"),
  M("FM-06", "association model",
    figure1_association_description(),
    S09),
  M("FM-07", "prediction design",
    figure1_repeated_cv_description(),
    S09),
  M("FM-08", "permutation",
    figure1_permutation_description(),
    S09),
  M("FM-09", "leakage control",
    "no scaling, centring or feature selection anywhere; outcome-derived group label excluded from every canonical model",
    "prediction_interpretation_constraints.csv"),
  M("FM-10", "sex", "formal feature-by-sex interaction, BH over three tests",
    "primary_feature_sex_interactions.csv"),
  M("FM-11", "n", "111 animals (58 female, 53 male; CON 24, RES 49, SUS 38)",
    "early_window_design_by_animal.csv"),
  M("FM-12", "unresolved",
    "component z-scores cannot be re-derived under one uniform rule (positional per-sex workbook blocks); cage identity absent from the Stage 09 design",
    "docs/COMBZ_CANONICAL_DEFINITION.md section 4a"))
utils::write.csv(meth, file.path(EXP, "figure1_methods_contract.csv"),
                 row.names = FALSE)

file.copy(file.path(OUT, "figure1_timeline_contract.csv"),
          file.path(EXP, "figure1_timeline_contract.csv"), overwrite = TRUE)

# -------------------------------------------------------- source manifest
SRC <- c("primary_movement_entropyacf1_associations.csv",
         "primary_prediction_performance.csv",
         "primary_prediction_permutation_test.csv",
         "primary_feature_sex_interactions.csv",
         "primary_movement_entropyacf1_correlations_by_sex.csv",
         "prediction_interpretation_constraints.csv",
         "early_window_contract_summary.csv",
         "early_window_design_by_animal.csv")
sm <- do.call(rbind, lapply(SRC, function(f) {
  p <- file.path(T10, f)
  data.frame(source_data_file = file.path(
    "analysis_ready/pipeline/09_early_prediction/10min/tables", f),
    exists = file.exists(p),
    bytes = if (file.exists(p)) file.size(p) else NA_integer_,
    sha256 = if (file.exists(p)) tools::md5sum(p)[[1]] else NA_character_,
    hash_algo = "md5", stringsAsFactors = FALSE)
}))
utils::write.csv(sm, file.path(EXP, "figure1_source_data_manifest.csv"),
                 row.names = FALSE)

prov <- data.frame(
  source_repo = "topohl/MMMSociability (local authoritative)",
  source_commit = SHA, source_branch = BRANCH,
  worktree_state = if (DIRTY) "dirty at export time" else "clean",
  analysis_entrypoint = S09,
  source_data_root = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID",
  source_data_file = paste(SRC, collapse = "; "),
  file_hash = paste(substr(sm$sha256, 1, 12), collapse = "; "),
  claim_ids = paste(claims$claim_id, collapse = "; "),
  created_at = NOW, stringsAsFactors = FALSE)
utils::write.csv(prov, file.path(EXP, "figure1_repo_provenance.csv"),
                 row.names = FALSE)

# ------------------------------------------------------------- findings
B <- function(id, sev, dom, desc, ev, impact, dec, act, st)
  data.frame(finding_id = id, severity = sev, domain = dom, description = desc,
             evidence = ev, manuscript_impact = impact, decision = dec,
             action = act, status = st, stringsAsFactors = FALSE)
bh <- rbind(
  B("BH-001", "MATERIAL", "sex",
    "The historical 'stronger/specific in females' reading is not supported by the current canonical analysis.",
    paste0("Formal feature-by-sex interactions all q = ", fmt(unique(sexi$interaction_p_bh), 2),
           "; sex-stratified rho for mean movement is -0.41 (female, n=58) versus -0.42 (male, n=53) - near identical."),
    "Any female-specific or female-stronger wording must be removed from the manuscript.",
    "Report as no detectable sex difference, with stratified estimates as descriptive only.",
    "Encoded in behavior_sex_effect_contract.csv and claim F1-08.", "RESOLVED"),
  B("BH-002", "MATERIAL", "design",
    "Cage identity is not represented in the Stage 09 prediction design, so cage-level dependence is neither modelled nor assessable.",
    "early_window_design_by_animal.csv carries AnimalNum, Group, Sex and window QC columns only; Analysis/09 sets cage_col <- NULL.",
    "The out-of-sample estimate may be optimistic with respect to cage structure; must be stated as a limitation.",
    "Does not invalidate the prediction claim - it is a generalisation concern, not outcome leakage.",
    "Stated in the Results and Methods drafts and in claim F1-10.", "DISCLOSED"),
  B("BH-003", "MATERIAL", "endpoint",
    "CombZ component z-scores are not reproducible in the repository under a single uniform rule.",
    "The workbook encodes its reference population as hard-coded absolute row positions; recomputing block-aware still deviates by up to 2.0.",
    "Methods must not claim CombZ is regenerated from raw data; downstream reproducibility (components -> CombZ -> labels) IS exact.",
    "Carry components verbatim; record external provenance.",
    "Stated in outcome_score_contract.csv and the Methods draft.", "DISCLOSED"),
  B("BH-004", "MINOR", "features",
    paste0("Entropy_acf1 became FDR-supported at the primary resolution with the first-night ",
           "leading-bin fix, but still adds nothing beyond mean movement."),
    paste0("q = ", fmt(g("Entropy_acf1", "spearman_p_bh"), 2),
           "; bootstrap CI excludes zero by 1e-4; partial r controlling movement = ",
           fmt(g("Entropy_acf1", "partial_r_controlling_movement"), 2),
           ". Unstable across builds at 10 min: q = 0.04 (2026-08-11), >= 0.05 (2026-09-21), ",
           fmt(g("Entropy_acf1", "spearman_p_bh"), 2), " now."),
    "May be reported as FDR-supported, but never as an independent third predictor.",
    "Report the univariate association together with the partial correlation.",
    "Claim F1-05; docs/FIRST_NIGHT_LEADING_BIN_GAP.md.", "RESOLVED"),
  B("BH-005", "MINOR", "model ladder",
    "The three-feature model does not improve on movement alone and no model-comparison test was performed.",
    paste0("LOAO R2 ", fmt(p("primary_behavior_family", "cv_r2")), " versus ",
           fmt(p("movement_mean", "cv_r2")), "."),
    "Must not be described as an improvement or as significantly worse.",
    "Report descriptively.", "Claim F1-07.", "RESOLVED"),
  B("BH-006", "MATERIAL", "scope",
    "Behaviour-proteomics integration supports no main-text claim.",
    "KNOWN_LIMITATIONS section 10: no association reaches FDR < 0.05 and the script labels every one exploratory.",
    "The proteomics manuscript's section 4 integration placeholder should not become a positive Results section.",
    "Keep out of the main text in both repositories.",
    "Recorded for the proteomics side.", "DISCLOSED"))
utils::write.csv(bh, file.path(OUT, "figure1_findings_ledger.csv"),
                 row.names = FALSE)

cat("export bundle written to", EXP, "\n")
cat("  claims:", nrow(claims), "| methods:", nrow(meth),
    "| source files hashed:", sum(sm$exists), "of", nrow(sm), "\n")
cat("  commit:", SHA, "| branch:", BRANCH, "| worktree:",
    if (DIRTY) "dirty" else "clean", "\n")
cat("findings ledger rows:", nrow(bh), "\n")
print(table(bh$severity))
