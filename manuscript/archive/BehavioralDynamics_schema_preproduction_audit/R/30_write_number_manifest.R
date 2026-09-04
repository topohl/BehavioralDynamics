# ==============================================================================
# 30_write_number_manifest.R
# Provenance manifest: every number printed in either schema figure, with the
# file it came from and how it was obtained. Nothing in the figures is a
# placeholder; this script is the audit trail.
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr) })

ROOT <- "C:/Users/topohl/Documents/GitHub/MMMSociability/manuscript/archive/BehavioralDynamics_schema_preproduction_audit"
DATA <- "C:/Users/topohl/AppData/Local/Temp/claude/c--Users-topohl-Documents-GitHub/77f6ce3c-a76b-4ee2-89c5-d1210af23338/scratchpad/data"
SRC  <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based"

m <- tibble::tribble(
  ~figure, ~element, ~quantity, ~value, ~source_file, ~how_obtained,

  "both", "L1 example", "movement events in the P3 P3 P4 P6 P6 P2 window", "3",
  "01_build_multiscale_behavior_metrics.R:470-471", "illustrative sequence; count follows PositionID != lag(PositionID)",

  "both", "L3 design", "animals total", "111",
  "systems_sis_domain_scores.csv", "n_distinct(AnimalNum)",
  "both", "L3 design", "animals per group", "CON 24, RES 49, SUS 38",
  "systems_sis_domain_scores.csv", "n_distinct(AnimalNum) by Group",
  "both", "L3 design", "animals per group x sex", "CON 12F/12M, RES 24F/25M, SUS 22F/16M",
  "systems_sis_domain_scores.csv", "n_distinct(AnimalNum) by Group x Sex",
  "both", "L3 epoch", "max 5-min bins per Active epoch", "576",
  "systems_sis_raw_phase_epoch_features.csv", "max(n_bins[PhaseClass=='Active']) = 4 x 144",
  "both", "L3 epoch", "max 5-min bins per Inactive epoch", "432",
  "systems_sis_raw_phase_epoch_features.csv", "max(n_bins[PhaseClass=='Inactive']) = 3 x 144",

  "main", "L4 example", "smooth series 3 3 4 3 4 mean / RMSSD", "3.4 / 0.87",
  "computed in-figure", "mean(x); sqrt(mean(diff(x)^2))",
  "main", "L4 example", "fragmented series 0 8 1 9 0 mean / RMSSD", "3.6 / 7.68",
  "computed in-figure", "mean(x); sqrt(mean(diff(x)^2))",

  "both", "worked example", "AnimalNum", "1545",
  "systems_sis_raw_phase_epoch_features.csv", "selected female SUS animal with an Active Social-spatial score",
  "both", "worked example", "n_bins", "575",
  "systems_sis_raw_phase_epoch_features.csv", "column n_bins, CC1 Active row",
  "both", "worked example", "Proximity_mean", "0.17099879710257854",
  "systems_sis_raw_phase_epoch_features.csv", "column Proximity_mean, verbatim",
  "both", "worked example", "Proximity_rmssd", "0.16760332601561975",
  "systems_sis_raw_phase_epoch_features.csv", "column Proximity_rmssd, verbatim",
  "both", "worked example", "Proximity_acf1", "0.60223043530389275",
  "systems_sis_raw_phase_epoch_features.csv", "column Proximity_acf1, verbatim",
  "both", "worked example", "standardization context n", "58 epochs",
  "systems_sis_raw_phase_epoch_features.csv", "nrow(Sex=='Female' & PhaseClass=='Active' & CageChangeIndex==1)",
  "supp", "worked example", "context mean / SD of Proximity_mean", "0.229848852203793 / 0.032455034320989",
  "systems_sis_raw_phase_epoch_features.csv", "mean/sd within the standardization context",
  "supp", "worked example", "context mean / SD of Proximity_rmssd", "0.175160855405243 / 0.014829991122325",
  "systems_sis_raw_phase_epoch_features.csv", "mean/sd within the standardization context",
  "supp", "worked example", "context mean / SD of Proximity_acf1", "0.652883027831098 / 0.067976823451248",
  "systems_sis_raw_phase_epoch_features.csv", "mean/sd within the standardization context",
  "both", "worked example", "Proximity_mean_z", "-1.8132797062905825",
  "recomputed", "safe_scale within Sex x PhaseClass x CageChangeIndex (14:5039); z columns are NOT exported",
  "both", "worked example", "Proximity_rmssd_z", "-0.5096111877131153",
  "recomputed", "safe_scale within Sex x PhaseClass x CageChangeIndex (14:5039); z columns are NOT exported",
  "both", "worked example", "Proximity_acf1_z", "-0.7451450355507243",
  "recomputed", "safe_scale within Sex x PhaseClass x CageChangeIndex (14:5039); z columns are NOT exported",
  "both", "worked example", "Social spatial organization DomainScore", "-0.76960118320753823",
  "systems_sis_domain_scores.csv", "exported value; recomputation matches to 0 (R/00_verify_provenance.R)",
  "both", "worked example", "chain validation max |exported - recomputed|", "0 (exact) over 882 rows",
  "R/00_verify_provenance.R", "inner_join of exported vs recomputed domain score",

  "both", "target tile", "Hedges g, Female Active Social spatial org, SUS-RES", "-0.10127263580838378",
  "systems_sis_domain_effect_summary.csv", "row: Social spatial organization / Active / Female / SUS-RES",
  "both", "target tile", "mean_difference", "-0.12155670673587333",
  "systems_sis_domain_effect_summary.csv", "same row",
  "both", "target tile", "Welch p", "0.4892415507700073",
  "systems_sis_domain_effect_summary.csv", "same row",
  "both", "target tile", "BH q", "0.5870898609240087",
  "systems_sis_domain_effect_summary.csv", "same row",
  "both", "target tile", "n_ref / n_comp (ROWS)", "96 / 88",
  "systems_sis_domain_effect_summary.csv", "same row; these are row counts, not animal counts",
  "supp", "target tile", "animals behind those rows", "RES 24, SUS 22 (CON 12)",
  "systems_sis_domain_scores.csv", "n_distinct(AnimalNum) in the same slice",
  "supp", "contrast", "Female Inactive RES-CON g / q", "-0.8197393059971586 / 0.00099411135259107",
  "systems_sis_domain_effect_summary.csv", "row: Social spatial organization / Inactive / Female / RES-CON",
  "supp", "contrast", "Female Inactive SUS-CON g / q", "-0.76325889098635 / 0.0019385520486892",
  "systems_sis_domain_effect_summary.csv", "row: Social spatial organization / Inactive / Female / SUS-CON",

  "supp", "W2", "Inactive rows with Stage-12 quiescence values", "0 of 444",
  "systems_sis_raw_phase_epoch_features.csv", "sum(!is.na(inactivity_fraction)) by PhaseClass",
  "supp", "W2", "Active rows with Stage-12 quiescence values", "356 of 444",
  "systems_sis_raw_phase_epoch_features.csv", "sum(!is.na(inactivity_fraction)) by PhaseClass",
  "supp", "W2", "max |exported rest score - (Mov_acf1_z - Mov_rmssd_z)/3|", "1.110e-16 over 444 rows",
  "R/02_verify_zero_fill_mechanism.R", "recomputed vs systems_sis_domain_scores.csv",
  "supp", "W2", "correlation(exported rest score, Mov_acf1_z - Mov_rmssd_z)", "1.0000000000",
  "R/02_verify_zero_fill_mechanism.R", "cor() over the 444 Inactive epochs",
  "supp", "W3", "volatility inputs used: Active with Stage 12", "5 of 5, n = 353",
  "R/01_verify_rest_domain_collapse.R", "rowSums(!is.na(z[, vol_cols])) by PhaseClass",
  "supp", "W3", "volatility inputs used: Active without Stage 12", "3 of 5, n = 88",
  "R/01_verify_rest_domain_collapse.R", "quiescence z is NA and is dropped by score_mean",
  "supp", "W3", "volatility shrink factor, Inactive", "0.600000 (exact, n = 441)",
  "R/02_verify_zero_fill_mechanism.R", "vol / three-term mean; quiescence z is 0 and is retained",
  "supp", "W4", "pseudo-replication factor", "4.0 for every group",
  "systems_sis_domain_scores.csv", "rows per group / distinct animals per group in the target slice",
  "supp", "W8", "Movement values exactly zero in the 10-min input", "56.4% (107999 / 191445)",
  "analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv", "Stage 12 input; drives the inert 20th-percentile threshold",
  "supp", "W8", "minimum per-animal zero fraction", "0.464",
  "analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv", "min over the 111 animals",

  "supp", "mixed model", "GroupRES:PhaseClassInactive estimate / q", "-1.5643102434848963 / 1.342e-6",
  "systems_sis_domain_mixed_model_stats.csv", "lmerTest::lmer, Social spatial organization",
  "supp", "mixed model", "fitted formula", "domain_score ~ Group * Sex * PhaseClass + CageChangeIndex + (1 | AnimalNum)",
  "14_systems_neuroscience_summary_dashboard.R:5064-5126", "extract_lmm_stats, called at 14:5367-5376"
)

m <- m %>% mutate(
  source_root = dplyr::case_when(
    grepl("\\.R(:|$)", source_file) ~ "MMMSociability/Analysis or manuscript/BehavioralDynamics_schema",
    source_file %in% c("computed in-figure", "recomputed") ~ "derived in this figure's scripts",
    TRUE ~ SRC
  ),
  .after = source_file
)

dir.create(file.path(ROOT, "data"), recursive = TRUE, showWarnings = FALSE)
out <- file.path(ROOT, "data", "figure_number_manifest.csv")
write.csv(m, out, row.names = FALSE)
cat("wrote", out, "-", nrow(m), "entries\n")
