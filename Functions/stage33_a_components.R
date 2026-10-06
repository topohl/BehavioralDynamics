# ================================================================
# Stage 33 module A - CombZ components by cohort (A_components/)
# MMMSociability
# ================================================================
# Plan section 8 of the frozen docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md, with sections 0-7 and 13-17. Post hoc,
# descriptive (estimation only); not registered; not a re-test of Stage 29/29b/30/32 hypotheses. The constants that the
# frozen plan takes from its source plan S33A_combz_components_by_cohort_plan_v1.0 (S33A_DATES, S33A_CONTINUITY,
# S33A_PLANNING_TARGETS) are declared here. Definitions only: sourcing this file reads and writes nothing.
#
# Entry points (MODULE CONTRACT, Functions/stage33_common.R):
#   p2  <- s33a_phase2(design, ctx)           outcome-free: the a12 Phase-2 columns (product), gates GA-4a, GA-5, GA-9a,
#                                              GA-13, and the resampling units and index matrices of the six streams
#   out <- s33a_phase3(design_out, p2, ctx)   a01-a13, audit a_data_checks and a_bootstrap_summaries, Phase-3 gates
#                                              (GA-4b, GA-6, GA-7, GA-8, GA-9b, GA-10, GA-11, GA-15, GA-T8) and the cross
#                                              exports (lags, x_RU_sis_cc1_rows, cage_sd_reference_cc1_cages, cc4_cage, tp2)
# GA-4 and GA-9 are evaluated in two parts: a (outcome-free, Phase 2) and b (Phase 3). s33a_outcome_free_extract(out)
# returns the Phase-2 product from the final a12 table, equal to p2$products (re-extraction, plan section 6).
# Needs data.table, digest, clubSandwich and the frozen engine (Functions/rfid_canonical_inference.R and
# Functions/stage32_inference.R) through Functions/stage33_common.R and Functions/stage33_run.R; the two readers
# (s33a_read_phase2, s33a_read_phase3) need readxl and run in the runner only. canonical_animal_id() comes from ctx$canon
# or the calling environment. Interval vocabulary, bases, units and labels follow plan section 7; no p-value or test
# statistic is computed or kept anywhere in this module (a10/a11 are verbatim copies of registered rows).
# ================================================================

# ---------------------------------------------------------------- constants (plan sections 7 and 8)
S33A_OUTCOME_FREE_PRODUCTS <- c("a12_cohort_descriptors_phase2")
# Columns the lint (GC-9) skips in this module: engine messages and the verbatim copies' source path and hash.
S33A_LINT_EXEMPT_COLUMNS <- c("engine_messages", "source_file", "source_sha256")
# Readings of plan section 8 recorded for the README (the core's s33_deviations() lists core rows only; see the report).
S33A_DEVIATIONS <- c(
  "GA-4 and GA-9 are evaluated in two parts: GA-4a and GA-9a outcome-free in Phase 2, GA-4b and GA-9b in Phase 3.",
  paste("S8 (without 692 at CC1; without the 7 tracked C57BL/6J SIS) is applied to E1 (a01: the affected cohorts' SIS means with",
        "per-cohort CR2 by CC1 cage and SIS-minus-CON with the animal Welch interval; no resampling stream is declared for S8), E3 (a05:",
        "E3(a) KR and six-point rows), E4 (a07) and E5 (a09); not to E1b or E2, whose table a03 keeps its declared 390 rows."),
  paste("A cell whose values do not vary (age at CC1 in B1; CON age in most cohorts) or an exact OLS fit has no sampling interval:",
        "the point value is reported with interval_method 'none' and a note instead of a degenerate CR2 or t interval."),
  paste("The delta_cort map slopes b_s are the fitted within-sex slopes of delta_cort on cort_delta (the source plan's definition),",
        "gated against the plan values (1e-7) in GA-10; GA-10 also checks the weight_dev link (sum of episode gains, exact)."),
  "canonical_117 (S1): a01 B1 rows (13 measures, the rate unchanged), a02 rows of the three male cohorts, a03 version canonical_117.",
  "E5 'CR2 by episode' is read as CR2 by cage episode (CageEpisodeID), beside CR2 by animal; E5(b) reports its two level-named terms.",
  "E4 cycle phase: a luteal indicator added to the E3(a) form in B3 and B4 (B6 unrecorded); log10 scale delta = log10(R + 1) - log10(B + 1).")
S33A_ARMS <- c("SIS", "CON")
S33A_SCHEMES <- c("animal", "cc1", "cc4")
S33A_POPULATIONS <- c("tracked_111", "canonical_117")
S33A_SEED_NAMES <- list(tracked_111 = c(animal = "A_animal", cc1 = "A_cc1", cc4 = "A_cc4"),
                        canonical_117 = c(animal = "A_s_animal", cc1 = "A_s_cc1", cc4 = "A_s_cc4"))
S33A_SCHEME_METHOD <- c(animal = "percentile_bootstrap_animal", cc1 = "cage_resampling_range_cc1", cc4 = "cage_resampling_range_cc4")
S33A_DCORT_PARTS <- c("dcort_response_part", "dcort_baseline_part", "dcort_residual_part")
S33A_E1_MEASURES <- c(S33_COMPONENTS, "dcort_response_part", "dcort_baseline_part", "CombZ", "x_RU", "tp1", "tp2", "tp6", "age_cc1")
S33A_Z_MEASURES <- c(S33_COMPONENTS, S33A_DCORT_PARTS, "CombZ")
S33A_SHARE_MEASURES <- c(S33_COMPONENTS, S33A_DCORT_PARTS, "CombZ_total")
S33A_C_VARS <- paste0("c_", c(S33_COMPONENTS, S33A_DCORT_PARTS))
S33A_CORT <- c("cort_baseline", "cort_response", "cort_delta")
S33A_LCORT <- c("lcort_baseline", "lcort_response", "lcort_delta")
S33A_MEAN_VARS <- c(S33A_E1_MEASURES, "dcort_residual_part", S33A_CORT, S33A_LCORT, "gain_cc1")
S33A_ARRAY_VARS <- c(S33A_MEAN_VARS, S33A_C_VARS)
S33A_OFFSET_VARS <- c(setdiff(S33A_E1_MEASURES, "x_RU"), S33A_CORT, S33A_LCORT)
S33A_SLOPE_SIS_VARS <- c(S33A_Z_MEASURES, S33A_CORT, S33A_LCORT, "gain_cc1")
S33A_SLOPE_CON_VARS <- c(S33A_Z_MEASURES, S33A_CORT, S33A_LCORT)
S33A_FEMALE <- names(S33_SEX_OF_COHORT)[S33_SEX_OF_COHORT == "Female"]
S33A_MALE <- names(S33_SEX_OF_COHORT)[S33_SEX_OF_COHORT == "Male"]
S33A_OMIT_SETS <- list(omit_B2_B6 = c("B2", "B6"), omit_B5_B6 = c("B5", "B6"))
# tp2_c rows carry the mechanical coupling of the component with CC1-day weight (plan E3(a))
S33A_MECH_COUPLING <- c(NOR = "none", sucrose_pref = "none", weight_dev = "shared_term", delta_cort = "none",
                        adrenal_weight = "ratio_denominator", spleen_weight = "ratio_denominator", dcort_response_part = "none",
                        dcort_baseline_part = "none", dcort_residual_part = "none", CombZ = "mixed")
# delta_cort map slopes within sex (z per ng/ml), plan section 8 table conventions; GA-10 checks the fitted slopes
S33A_DCORT_SLOPE <- c(Male = -0.0149566, Female = -0.0258772)
S33A_DCORT_SLOPE_TOL <- 1e-7
# raw measures of the components in the workbook sheet master_wide (weight_dev: tp6 - tp2, B6 tp6 - tp3)
S33A_RAW <- data.table::data.table(
  measure = S33_COMPONENTS,
  raw_col = c("nor_d2", "sucrose_preference_pct", NA_character_, "cort_delta_ng_ml", "adrenal_ratio_mg_per_100g", "spleen_ratio_mg_per_100g"),
  raw_unit = c("NOR D2 index", "% sucrose preference", "g (weight change over the weight_dev interval)", "ng/ml",
               "mg per 100 g body weight", "mg per 100 g body weight"))
S33A_MW_COPIES <- c(NOR = "nor", sucrose_pref = "sucrose_pref", weight_dev = "weight_dev", delta_cort = "delta_cort",
                    adrenal_weight = "adrenal_weight", spleen_weight = "spleen_weight")
S33A_A4_TECH_COLS <- c("Batch", "CON_system", "raw_recording_start", "rack", "terminal_method", "rfid_mark_to_cc1_days", "line_counts",
                       "DST_state_at_A1", "placement_median_range", "pre1830_h", "pre1830_is_lower_bound", "elisa_files")
# planned cages of the six untracked B1 SIS (plan section 3; Social Golfer plan)
S33A_UNTRACKED_CC1 <- c("1" = "B1|sys.2|CC1", OQ750 = "B1|sys.2|CC1", OQ751 = "B1|sys.2|CC1",
                        "2" = "B1|sys.5|CC1", OQ752 = "B1|sys.5|CC1", OQ753 = "B1|sys.5|CC1")
S33A_UNTRACKED_CC4 <- c("1" = "B1|sys.1|CC4", OQ752 = "B1|sys.1|CC4", OQ753 = "B1|sys.3|CC4",
                        "2" = "B1|sys.4|CC4", OQ750 = "B1|sys.4|CC4", OQ751 = "B1|sys.5|CC4")
S33A_CC1_SIZES <- c(B1 = "1,1,4,4", B2 = "4,4,4,4", B3 = "4,4,4,4", B4 = "3,4,4,4", B5 = "3,4,4,4", B6 = "3,4,4,4")
S33A_CC4_SIZES_ALL <- c(B1 = "4,4,4,4", B2 = "4,4,4,4", B3 = "4,4,4,4", B4 = "3,4,4,4", B5 = "3,4,4,4", B6 = "3,4,4,4")
S33A_CON_BOARDS <- c(B1 = "sys.3", B2 = "sys.4", B3 = "sys.3", B4 = "sys.3", B5 = "sys.3", B6 = "sys.2")
S33A_SIS_TRACKED <- c(B1 = 10L, B2 = 16L, B3 = 16L, B4 = 15L, B5 = 15L, B6 = 15L)
# registered rows copied verbatim into a10 / a11 (plan sections 2 and 8)
S33A_REGISTERED <- list(multiplicity = c("H01", "H03", "H11", "H13"),
                        estimates = c("A_EXP|crossing_rate", "A_CZ|crossing_rate", "H13|Movement_mean"), n_estimates = 5L)
# plan-quoted values of the reference rows (checked against the copied rows in GA-T8; tolerance = rounding)
S33A_REFERENCE <- data.table::data.table(
  ref = c(rep("H01", 4), rep("H01_RU", 3), rep("H03", 3), rep("S29_parent", 3), "S29_parent_RU", rep("H13", 4)),
  field = c("estimate", "ci_low", "ci_high", "df", "estimate", "ci_low", "ci_high", "estimate", "ci_low", "ci_high",
            "estimate", "ci_low", "ci_high", "estimate", "estimate", "ci_low", "ci_high", "df"),
  value = c(-0.6394, -9.0234, 7.7446, 4.0, -0.1066, -1.5039, 1.2908, -1.1156, -3.0908, 0.8596, -0.0058, -0.0328, 0.0212, -0.0348,
            -0.307, -0.565, -0.050, 9.4),
  tolerance = c(6e-5, 6e-5, 6e-5, 0.06, 6e-5, 6e-5, 6e-5, 6e-5, 6e-5, 6e-5, 6e-5, 6e-5, 6e-5, 3.6e-4, 6e-4, 6e-4, 6e-4, 0.06))
# GA-9: sucrose test windows (Timeline 'SucPref'), test dates and the tp6 offset after CC4 (source plan G9)
S33A_DATES <- data.table::data.table(
  Batch = S33_COHORTS,
  sucpref_text = c("11/11/2022 - 11/16/2022", "02/12/2023 - 02/17/2023", "05/08/2023 - 05/13/2023", "08/08/2023 - 08/13/2023",
                   "12/04/2023 - 12/09/2023", "21-Apr-24 - 26-Apr-24"),
  test_start = as.Date(c("2022-11-11", "2023-02-12", "2023-05-08", "2023-08-08", "2023-12-04", "2024-04-21")),
  test_end = as.Date(c("2022-11-16", "2023-02-17", "2023-05-13", "2023-08-13", "2023-12-09", "2024-04-26")),
  tp6_minus_cc4_d = c(5L, 5L, 5L, 5L, 5L, 8L),
  tp6_relative_to_test = c(rep("sucrose test day 4", 5), "day after the sucrose test"))
# GA-11: continuity with the 2026-10-01 diagnostic (source plan G11; Table 5.5 of BATCH_DECOMPOSITION_REPORT_2026-10-01.md)
S33A_CONTINUITY <- list(
  targets = data.table::data.table(
    target_id = c(paste0("share_mm|", c("delta_cort", "weight_dev", "spleen_weight", "adrenal_weight", "sucrose_pref", "NOR")),
                  "V_over_5_mm", "six_point_mm|estimate", "six_point_mm|ci_low", "six_point_mm|ci_high",
                  paste0("mundlak_mm|within|", c("estimate", "ci_low", "ci_high", "df")),
                  paste0("mundlak_mm|between|", c("estimate", "ci_low", "ci_high", "df")), "cage_sd_reference_cc1|CombZ"),
    value = c(45.04, 32.58, 12.47, 10.41, 9.67, -10.17, -0.3443, -0.5025, -0.7553, -0.2496,
              -0.1059, -0.2557, 0.0438, 79.8, -0.4944, -0.7502, -0.2387, 3.41, 0.1955),
    tolerance = c(rep(0.1, 6), 0.0005, rep(0.001, 3), rep(0.002, 3), 0.1, rep(0.002, 3), 0.01, 0.0005)),
  table_5_5 = data.table::data.table(
    Batch = rep(c("B2", "B6"), each = 8L),
    measure = rep(c("Movement_mean", "CombZ", "delta_cort", "adrenal_weight", "weight_dev", "spleen_weight", "NOR", "sucrose_pref"), 2L),
    offset_total = c(-0.947, 0.613, 1.254, 0.341, 1.179, 0.872, -0.095, 0.165, -1.031, 0.500, 1.640, 0.625, 0.796, 0.134, -0.319, 0.125),
    part_shared_con = c(-0.400, 0.539, 1.055, 0.637, 0.194, 1.125, -0.163, 0.370, 0.617, -0.066, 0.575, -0.841, -0.547, 0.636, -0.887, 0.667),
    part_not_shared = c(-0.546, 0.074, 0.199, -0.295, 0.984, -0.253, 0.067, -0.205, -1.648, 0.566, 1.066, 1.466, 1.343, -0.502, 0.568, -0.542)),
  table_5_5_tolerance = 0.001,
  a4_tolerance = 1e-12)
# GA-15: planning targets of the A plan (source plan G15, verified 2026-10-05)
S33A_PLANNING_TARGETS <- data.table::data.table(
  target_id = c("V|all_six", "six_point|CombZ|estimate",
                paste0("share_total|", S33_COMPONENTS), paste0("share_shared|", S33_COMPONENTS), "share_shared|CombZ_total",
                "repairing_min|CombZ_total", "repairing_max|CombZ_total",
                paste0(c("cage_sd_reference_cc1", "cage_sd_reference_cc4", "cage_sd_reference_cc4_all117", "half_within_sd_sis",
                         "half_within_sd_con", "con_cage_mean_sd_within_sex"), "|CombZ"),
                "r_xbar_tp2", "ols_fe|CombZ|none", "ols_fe|CombZ|tp2_c", "ols_fe|c_weight_dev|none", "ols_fe|c_weight_dev|tp2_c",
                paste0("kr|CombZ|none|", c("estimate", "ci_low", "ci_high")), paste0("kr|CombZ|tp2_c|", c("estimate", "ci_low", "ci_high")),
                paste0("kr|weight_dev|none|", c("estimate", "ci_low", "ci_high")), paste0("kr|weight_dev|tp2_c|", c("estimate", "ci_low", "ci_high")),
                paste0("e5a|none|", c("estimate", "ci_low", "ci_high")), paste0("e5a|ws_c|", c("estimate", "ci_low", "ci_high"))),
  value = c(-1.7222, -0.5019,
            -10.19, 9.67, 32.58, 45.09, 10.37, 12.47, -14.23, 11.76, -9.40, 25.17, -1.45, 27.93, 39.8, -33.5, 51.2,
            0.195, 0.328, 0.307, 0.294, 0.160, 0.333, 0.977, -0.1058, -0.0117, -0.0945, -0.0181,
            -0.1049, -0.2565, 0.0468, -0.0109, -0.1787, 0.1569, -0.5670, -0.8941, -0.2399, -0.1044, -0.4005, 0.1918,
            -0.042, -0.148, 0.064, 0.017, -0.084, 0.117),
  tolerance = c(0.0005, 0.0005, rep(0.05, 12), rep(0.1, 3), rep(0.001, 6), 0.001, rep(0.0005, 4), rep(0.002, 12), rep(0.002, 6)))
S33A_GATE_NAMES <- c(
  "GA-4a" = "counts (design): 117 canonical, 111 tracked, 444 animal-episodes, 4 CON per cohort, tracked SIS 10/16/16/15/15/15",
  "GA-4b" = "counts (outcome groups): tracked CON/RES/SUS 24/53/34, canonical 24/58/35",
  "GA-5" = "cages: CC1 and CC4 cage structure, CON boards, planned cages of the six untracked B1 SIS",
  "GA-6" = "identities: CombZ = sum of the contributions c_k (1e-12); delta_cort parts add up (1e-12); rates = bundle (1e-9)",
  "GA-7" = "missingness exactly as declared",
  "GA-8" = "pooled same-sex CON reference: mean 0 and SD 1 (female delta_cort 0.8826); male adrenal_weight on 11 CON",
  "GA-9a" = "dates (outcome-free): tp2 = CC1 A1 date; tp1 2-5 d before; sucrose test windows = S33A_DATES; test start = CC4 + 2 d",
  "GA-9b" = "dates: tp3-tp5 = CC2-CC4 A1 dates; tp6 5 d after CC4 (8 in B6); tp6 = sucrose test day 4 (B6: day after the test)",
  "GA-10" = "component maps: exact linear maps of the raw measures within sex; delta_cort map slopes = plan values; copies = canonical",
  "GA-11" = "continuity with the 2026-10-01 diagnostic (S33A_CONTINUITY)",
  "GA-13" = "cohort descriptors = a4 technical fields (method, rack, marking, line counts, CON board, recording start)",
  "GA-15" = "planning targets of the A plan (S33A_PLANNING_TARGETS)",
  "GA-T8" = "a10/a11 verbatim copies of the registered Stage 32 rows; reference values = plan values")
S33A_U <- list(
  z = "CON-SD units (pooled same-sex CON reference)", z_ru = "CON-SD units per 6 position changes/hour",
  c_ru = "CON-SD units per 6 position changes/hour (contribution scale)", ru = "RU (6 position changes/hour)",
  ngml_ru = "ng/ml per 6 position changes/hour", lngml = "log10(ng/ml + 1)", lngml_ru = "log10(ng/ml + 1) per 6 position changes/hour",
  g_ru = "g per 6 position changes/hour", gday_ru = "g/day per 6 position changes/hour", rate = "position changes/hour",
  share = "percent of V (SIS between-cohort sum of cross-products of the CC1 A1 rate and CombZ)",
  h03 = "position changes/hour per CombZ unit", s29 = "CombZ units per position change/hour", s29_ru = "CombZ units per 6 position changes/hour",
  h13 = "CombZ units per Movement_mean unit (sex_c coefficient)", mm = "CON-SD units per Movement_mean unit", mm_mean = "Movement_mean units",
  a12_cohort = "per column: n animals; d (age, marking to CC1); g (weights); RU (xbar, 6 position changes/hour); h (lag, placement to 18:30)",
  a12_r = "Pearson r over the six cohort values (all six or within sex)")
# Procedural alternatives per measure (plan 17.2, module A): listed beside the cohort-level rows, never dismissed
S33A_PROCEDURAL <- c(
  NOR = "NOR and sucrose test schedules relative to CC1 identical in all cohorts",
  sucrose_pref = "NOR and sucrose test schedules relative to CC1 identical in all cohorts",
  weight_dev = paste("test state at the final weighing and interval length; weight_dev is a change score from the CC1-day weight",
                     "(CC2-day weight in B6) to the final weighing, and final weights converge within sex"),
  delta_cort = "terminal method (changed at B5) and cohort-specific assay runs; B3 baselines at about 1 ng/ml (assay limit unknown)",
  dcort_response_part = "terminal method (changed at B5) and cohort-specific assay runs",
  dcort_baseline_part = "terminal method (changed at B5) and cohort-specific assay runs; B3 baselines at about 1 ng/ml (assay limit unknown)",
  dcort_residual_part = "terminal method (changed at B5) and cohort-specific assay runs",
  adrenal_weight = "terminal method (changed at B5); organ ratio to body weight",
  spleen_weight = "terminal method (changed at B5); organ ratio to body weight",
  CombZ = "the procedural alternatives of its six components", CombZ_total = "the procedural alternatives of its six components",
  x_RU = "CC1 placement time and CON board position (B2, B6)")
# a12 numeric descriptors whose r with the SIS xbar is printed (Phase 2; Phase 3 adds the tp6 means)
S33A_A12_R_COLS_P2 <- c("n_SIS_tracked", "age_SIS_mean", "age_CON_mean", "tp1_SIS_mean", "tp1_CON_mean", "tp2_SIS_mean", "tp2_CON_mean",
                        "xbar_CON_RU", "lag_cc1_h", "a4_placement_to_1830_h_used_in_r", "rfid_mark_to_cc1_days_median",
                        "n_C57BL6J_tracked_SIS", "n_source_cages_SIS_tracked")
S33A_A12_R_COLS_P3 <- c("tp6_SIS_mean", "tp6_CON_mean")
S33A_LAB <- list(
  con_flag = "single CON cage per cohort; anti-conservative",
  con_phrase = paste("the single CON cage of each cohort (4 animals from one pre-SIS cage, housed together throughout; in B2, B5 and B6",
                     "that pre-SIS cage also supplied 2 SIS; interval anti-conservative or absent)"),
  kr_common = "common residual variance across cohorts",
  range80 = "cage-resampling ranges use about 4 cages per cohort: about 80% coverage",
  scatter = "six cohorts treated as replicates; carried by B2, B6 and B3; nothing about other cohorts",
  weight = "across cohorts the rate travels with CC1-day weight (r 0.98) and age (r 0.62); body size may be cohort biology",
  h01 = "pooled sex-averaged SIS-minus-CON contrast over 6 CON cages; registered, null; per-cohort rows are not slices of a test",
  combz_a05 = paste("total of the component rows: CombZ on the rate, pooled over sexes (animal-weighted), crossed CC1/CC4 cage effects;",
                    "not the registered H03 estimand (rate on CombZ_wb, sex-averaged)"),
  combz_tp2 = "CC1-day weight added; never an adjusted H03",
  decomp = "post hoc decomposition of a within-cohort association whose registered tests (H03, H11) were null",
  h03 = "registered Stage 32 H03 row (rate on CombZ_wb, sex-averaged), copied, not recomputed",
  s29 = "Stage 29 v1.0.1 sex-averaged parent slope (SIS), copied, not recomputed",
  h13 = "registered Stage 32 H13 row (secondary), printed beside the sex-stratified component slopes; no female-minus-male difference is formed",
  s7 = "in-sample slope of the registered H11 model C (CombZ ~ Batch + Movement_mean)",
  continuity = "reproduces the 2026-10-01 diagnostic; not a re-test of Stage 32 H03",
  ws = "start weight shares measurement error with the gain; includes a regression-to-the-mean component",
  con_gain = "gain SD about 0.5 g against 1 g resolution",
  r12 = "alignment over six cohort values (4 df) across many descriptors; not attributable; cohort biology not separable",
  offset_ref = "own-sex 3-cohort mean, own included",
  refsd = "reference SDs are lower bounds for a never-regrouped CON cage and never criteria; an offset below a reference SD is not called noise",
  share = "a share splits a covariance; it is not a share of variance and does not identify an origin",
  repair = "range over the 36 within-sex re-pairings of the CON cages with the cohorts (identity included); descriptive reference only",
  e1 = "E1: mean of present values", e2 = "E2: c_k = z_k / n_present, missing counted 0 (sum of c_k = CombZ)",
  con_between = "between the six CON cages: cage and cohort not separable",
  b3 = "B3 baselines cluster at about 1 ng/ml (assay limit unknown)",
  a10 = "registered Stage 32 v1.1_f4c7a31 result, copied, not recomputed",
  denom = "|V| below a quarter of the all-six V; not interpreted",
  raw = "raw grams, not a component",
  rate_point = "rate SIS-minus-CON quantity: point value only, beside the registered H01 row",
  wd = "weight_dev is a change score from the CC1-day weight (CC2-day weight in B6) to the final weighing; final weights converge within sex",
  a4 = "copied from the 2026-10-01 a4 tables (not re-derived)",
  cr2_cell = "per-cohort CR2 by CC1 cage (Satterthwaite df); no resampling stream for this population",
  S8_drop_HW_A1 = "S8 sensitivity: without the hardware-flagged CC1 animal-episode (692, B6; 7 of 8 RFID positions registered)",
  S8_drop_J7 = "S8 sensitivity: without the 7 tracked C57BL/6J SIS (B1 2, B3 3, B5 2)")

# ---------------------------------------------------------------- small helpers
`%a_or%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
s33a_id <- function(...) paste(..., sep = "|")
s33a_na <- function(x) { x <- as.numeric(x); x[!is.finite(x) & !is.infinite(x)] <- NA_real_; x }
s33a_one <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) == 1L && !is.na(x)) x else NA_real_ }

#' canonical_animal_id() from ctx$canon or the calling environment (the runner provides it).
s33a_canon <- function(ctx) {
  if (is.function(ctx$canon)) return(ctx$canon)
  if (exists("canonical_animal_id", mode = "function")) return(get("canonical_animal_id", mode = "function"))
  stop("Module A needs canonical_animal_id() (ctx$canon or the calling environment).", call. = FALSE)
}

#' Centre the columns of a B x J matrix about the row mean over all cohorts (sex = NULL) or over the same-sex cohorts.
s33a_centre <- function(X, sex = NULL) {
  if (is.null(sex)) return(X - rowMeans(X))
  for (s in unique(sex)) { k <- which(sex == s); X[, k] <- X[, k, drop = FALSE] - rowMeans(X[, k, drop = FALSE]) }
  X
}

#' Sum of cross-products over cohorts (columns), one value per row (replicate); never cov().
s33a_scp <- function(X, Y, sex = NULL) rowSums(s33a_centre(X, sex) * s33a_centre(Y, sex))

#' Cohort-scatter OLS slope of y on x over cohort values (optionally with sex terms), t interval on the residual df.
s33a_scatter_slope <- function(y, x, sex = NULL, level = 0.95) {
  ok <- is.finite(y) & is.finite(x); y <- y[ok]; x <- x[ok]; n <- length(y)
  if (!is.null(sex)) sex <- sex[ok]
  ns <- if (is.null(sex)) 1L else length(unique(sex)); df <- n - ns - 1L
  na <- list(estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n = n)
  if (n < 3L || df < 1L) return(na)
  xc <- if (is.null(sex)) x - mean(x) else x - stats::ave(x, sex)
  yc <- if (is.null(sex)) y - mean(y) else y - stats::ave(y, sex)
  sxx <- sum(xc^2); if (!(sxx > 0)) return(na)
  b <- sum(xc * yc) / sxx; se <- sqrt(sum((yc - b * xc)^2) / df / sxx); ci <- s33_t_interval(b, se, df, level)
  list(estimate = b, se = se, df = df, ci_low = ci$ci_low, ci_high = ci$ci_high, n = n)
}

#' TRUE when the finite values of a cell do not vary (e.g. age at CC1: every B1 animal was 24 d old). Such a cell has a
#' degenerate sampling SD of 0; module A then reports the point value with no interval (plan: no interval is invented).
s33a_no_spread <- function(y) { y <- y[is.finite(y)]; length(y) >= 1L && (length(y) == 1L || max(y) - min(y) == 0) }
S33A_NO_SPREAD_NOTE <- "no spread in the cell (every value equal): point value only, no interval"

#' Interval label of a t interval: 't_<df>' (plan section 7 vocabulary) or 'none' without a finite df.
s33a_t_method <- function(df) if (length(df) == 1L && is.finite(df)) paste0("t_", df) else "none"
#' Interval label of a copied KR row: 'KR', or 'Satterthwaite_fallback' when the registered row used the declared fallback.
s33a_kr_method <- function(fallback) if (isTRUE(fallback == 1)) "Satterthwaite_fallback" else "KR"

#' Mean with a t(n - 1) interval (CON cohort means); a cell without spread gets no interval.
s33a_mean_t <- function(y, level = 0.95) {
  y <- y[is.finite(y)]; n <- length(y); m <- if (n) mean(y) else NA_real_
  if (n < 2L || s33a_no_spread(y)) return(list(estimate = m, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n = n,
                                               no_spread = n >= 1L && s33a_no_spread(y)))
  se <- stats::sd(y) / sqrt(n); ci <- s33_t_interval(m, se, n - 1, level)
  list(estimate = m, se = se, df = n - 1, ci_low = ci$ci_low, ci_high = ci$ci_high, n = n, no_spread = FALSE)
}

#' Animal-level Welch interval of a difference of means (a - b); no statistic. Two cells without spread: no interval.
s33a_welch <- function(a, b, level = 0.95) {
  a <- a[is.finite(a)]; b <- b[is.finite(b)]; na <- length(a); nb <- length(b)
  est <- if (na && nb) mean(a) - mean(b) else NA_real_
  if (na < 2L || nb < 2L || (s33a_no_spread(a) && s33a_no_spread(b)))
    return(list(estimate = est, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  va <- stats::var(a) / na; vb <- stats::var(b) / nb; se <- sqrt(va + vb)
  df <- (va + vb)^2 / (va^2 / (na - 1) + vb^2 / (nb - 1)); ci <- s33_t_interval(est, se, df, level)
  list(estimate = est, se = se, df = df, ci_low = ci$ci_low, ci_high = ci$ci_high)
}

#' Per-cohort CR2 mean of the SIS animals clustered by a cage column (plan: per-cohort CR2; the same helper and cluster
#' convention as module D, s33_cr2_mean, so that 6 x A's x_RU rows equal D's d05 rate rows). A cell without spread
#' would be an exact intercept-only OLS fit (summary.lm 'essentially perfect fit'; CR2 SE 0, Satterthwaite df 0/0):
#' it is not passed to clubSandwich and gets the point value with no interval.
s33a_cr2_cell <- function(y, cluster) {
  ok <- is.finite(y)
  if (any(ok) && s33a_no_spread(y[ok]))
    return(data.table::data.table(estimate = mean(y[ok]), se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                                  n_clusters = data.table::uniqueN(cluster[ok]), n = sum(ok), no_spread = TRUE))
  s33_cr2_mean(y, cluster)[, no_spread := FALSE][]
}

#' Compact text of check values (a_data_checks).
s33a_fmt <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  if (is.numeric(x)) x <- ifelse(is.finite(x), formatC(signif(x, 10), digits = 10, format = "g"), as.character(x))
  x <- trimws(as.character(x)); s <- paste(utils::head(x, 12L), collapse = ";")
  if (length(x) > 12L) s <- paste0(s, ";... (", length(x), " values)")
  s
}

#' One check row (a vector of item results collapses into one row; empty or NA fails).
s33a_chk <- function(gate_id, check, ok, expected = NA, observed = NA, tolerance = NA_real_) {
  ok <- as.logical(ok); n <- length(ok)
  data.table::data.table(gate_id = gate_id, check = check, n_items = n, n_ok_items = sum(ok %in% TRUE),
                         expected = s33a_fmt(expected), observed = s33a_fmt(observed), tolerance = as.numeric(tolerance),
                         ok = n > 0L && !anyNA(ok) && all(ok))
}

#' Value checks against a target table (target_id, value, tolerance) and a named vector of observed values.
s33a_target_checks <- function(gate_id, targets, obs) {
  data.table::rbindlist(lapply(seq_len(nrow(targets)), function(i) {
    t <- targets[i]; o <- if (t$target_id %in% names(obs)) unname(obs[[t$target_id]]) else NA_real_
    s33a_chk(gate_id, paste("target", t$target_id), is.finite(o) && abs(o - t$value) <= t$tolerance, t$value, o, t$tolerance) }))
}

#' Gate rows from check rows (s33_gate_rows; a gate without checks fails).
s33a_gates <- function(ck, ids) {
  data.table::rbindlist(lapply(ids, function(g) { x <- ck[gate_id == g]
    s33_gate_rows(g, S33A_GATE_NAMES[[g]], x$ok, n_expected = nrow(x),
                  detail = if (nrow(x)) sprintf("%d of %d checks passed", sum(x$ok %in% TRUE), nrow(x)) else "no check evaluated") }))
}

#' Column order: descriptors, the declared estimate columns, extras, tier last. A descriptor that is also a declared
#' estimate column (e.g. 'estimand') stays in front and is not repeated. Duplicated column names stop (a code error).
s33a_finish <- function(x, front = character()) {
  x <- data.table::as.data.table(x)
  if (anyDuplicated(names(x))) stop("Module A table with duplicated column(s): ", paste(unique(names(x)[duplicated(names(x))]), collapse = ", "),
                                    call. = FALSE)
  front <- setdiff(intersect(front, names(x)), "tier"); est <- intersect(setdiff(S33_ESTIMATE_COLUMNS, c("tier", front)), names(x))
  rest <- setdiff(names(x), c(front, est, "tier"))
  data.table::setcolorder(x, c(front, est, rest, intersect("tier", names(x))))
  for (k in names(x)) if (is.double(x[[k]])) data.table::set(x, j = k, value = s33a_na(x[[k]]))
  x[]
}

s33a_unit_mean <- function(m) data.table::fcase(
  m %in% S33A_Z_MEASURES, S33A_U$z, m == "x_RU", S33A_U$ru, m %in% c("tp1", "tp2", "tp6", "gain_cc1"), "g", m == "age_cc1", "d",
  m %in% S33A_CORT, "ng/ml", m %in% S33A_LCORT, S33A_U$lngml, m == "Movement_mean", S33A_U$mm_mean, default = NA_character_)
s33a_unit_slope <- function(m) data.table::fcase(
  m %in% S33A_Z_MEASURES, S33A_U$z_ru, m %in% S33A_C_VARS, S33A_U$c_ru, m %in% S33A_CORT, S33A_U$ngml_ru, m %in% S33A_LCORT, S33A_U$lngml_ru,
  m %in% c("gain_cc1", "tp1", "tp2", "tp6"), S33A_U$g_ru, m == "age_cc1", "d per 6 position changes/hour", default = NA_character_)
s33a_metric <- function(m) ifelse(m == "x_RU", "crossing_rate", ifelse(m == "Movement_mean", "Movement_mean", NA_character_))

# ---------------------------------------------------------------- animal frame and component maps (Phase 3)
#' Linear maps of each component on its raw measure within sex (GA-10) and the map slopes b_s (z per raw unit).
#' `a` = design_out$animals (117), `mw` = master_wide raw columns and copies keyed by AnimalNum.
s33a_maps <- function(a, mw) {
  x <- merge(data.table::as.data.table(a), mw, by = "AnimalNum", all.x = TRUE, suffixes = c("", ".mw"))
  x[, wd_raw := data.table::fifelse(Batch == "B6", tp6 - tp3, tp6 - tp2)]
  slopes <- list(); ck <- list()
  fit_map <- function(m, raw, s) {
    y <- x[Sex == s][[m]]; r <- x[Sex == s][[raw]]; ok <- is.finite(y) & is.finite(r)
    if (sum(ok) < 3L) return(list(slope = NA_real_, max_resid = NA_real_, n = sum(ok)))
    f <- stats::lm.fit(cbind(1, r[ok]), y[ok]); list(slope = unname(f$coefficients[2]), max_resid = max(abs(f$residuals)), n = sum(ok))
  }
  for (s in c("Female", "Male")) for (m in S33_COMPONENTS) {
    raw <- if (m == "weight_dev") "wd_raw" else S33A_RAW[measure == m, raw_col]
    f <- fit_map(m, raw, s); slopes[[length(slopes) + 1L]] <- data.table::data.table(Sex = s, measure = m, map_slope = f$slope, n = f$n)
    tol <- if (m == "delta_cort" && s == "Female") 0.02 else 1e-9
    ck[[length(ck) + 1L]] <- s33a_chk("GA-10", sprintf("%s linear in %s within %s (max |resid| <= %g)", m, raw, s, tol),
                                       is.finite(f$max_resid) && f$max_resid <= tol, paste("<=", tol), f$max_resid, tol)
  }
  sl <- data.table::rbindlist(slopes)
  b_dcort <- stats::setNames(sl[measure == "delta_cort", map_slope], sl[measure == "delta_cort", Sex])
  ck[[length(ck) + 1L]] <- s33a_chk("GA-10", "delta_cort map slopes within sex = plan values (M -0.0149566, F -0.0258772)",
    abs(b_dcort[names(S33A_DCORT_SLOPE)] - S33A_DCORT_SLOPE) <= S33A_DCORT_SLOPE_TOL, S33A_DCORT_SLOPE, b_dcort[names(S33A_DCORT_SLOPE)],
    S33A_DCORT_SLOPE_TOL)
  dd <- x[is.finite(cort_delta_ng_ml) | is.finite(cort_response)]
  ck[[length(ck) + 1L]] <- s33a_chk("GA-10", "master_wide cort_delta = cort_response - cort_baseline (1e-9)",
    abs(dd$cort_delta_ng_ml - (dd$cort_response - dd$cort_baseline)) <= 1e-9, "<= 1e-9",
    max(abs(dd$cort_delta_ng_ml - (dd$cort_response - dd$cort_baseline)), na.rm = TRUE), 1e-9)
  cp <- vapply(names(S33A_MW_COPIES), function(m) { u <- x[[m]]; v <- x[[paste0("mw_", S33A_MW_COPIES[[m]])]]
    if (!identical(is.na(u), is.na(v))) return(Inf); max(c(0, abs(u - v)), na.rm = TRUE) }, 0)
  ck[[length(ck) + 1L]] <- s33a_chk("GA-10", "master_wide component copies = canonical components (1e-12)", cp <= 1e-12, "<= 1e-12", cp, 1e-12)
  list(slopes = sl, b_dcort = b_dcort, checks = data.table::rbindlist(ck))
}

#' Animal-level frame of module A: arm, contributions c_k (E2 convention), delta_cort parts, cort scales, gain_cc1 and
#' Movement_mean (S7). `b_dcort` = delta_cort map slopes by sex (z per ng/ml).
s33a_frame <- function(animals, b_dcort, movement = NULL) {
  d <- data.table::copy(data.table::as.data.table(animals))
  d[, arm := data.table::fifelse(condition == "CON", "CON", "SIS")]
  zc <- as.matrix(d[, S33_COMPONENTS, with = FALSE]); np <- rowSums(is.finite(zc)); d[, n_present := np]
  for (k in S33_COMPONENTS) { z <- d[[k]]; data.table::set(d, j = paste0("c_", k), value = ifelse(is.finite(z), z / np, 0)) }
  mR <- d[arm == "CON", .(mR = mean(cort_response, na.rm = TRUE), mB = mean(cort_baseline, na.rm = TRUE)), by = Sex]
  i <- match(d$Sex, mR$Sex); bs <- unname(b_dcort[d$Sex]); dc_ok <- is.finite(d$delta_cort)
  r <- bs * (d$cort_response - mR$mR[i]); q <- -bs * (d$cort_baseline - mR$mB[i]); r[!dc_ok] <- NA_real_; q[!dc_ok] <- NA_real_
  d[, `:=`(dcort_response_part = r, dcort_baseline_part = q, dcort_residual_part = delta_cort - r - q, cort_con_mean_response = mR$mR[i],
           cort_con_mean_baseline = mR$mB[i], dcort_map_slope = bs)]
  for (k in S33A_DCORT_PARTS) { z <- d[[k]]; data.table::set(d, j = paste0("c_", k), value = ifelse(is.finite(z), z / np, 0)) }
  d[, `:=`(cort_delta = cort_response - cort_baseline, lcort_baseline = log10(cort_baseline + 1), lcort_response = log10(cort_response + 1))]
  d[, `:=`(lcort_delta = lcort_response - lcort_baseline, gain_cc1 = tp3 - tp2)]
  if (!is.null(movement)) { mm <- data.table::as.data.table(movement)[, .(AnimalNum, Movement_mean)]
    d <- merge(d, mm, by = "AnimalNum", all.x = TRUE) } else d[, Movement_mean := NA_real_]
  data.table::setkey(d, AnimalNum)
  d[]
}

# ---------------------------------------------------------------- resampling units, cell sums, means (plan section 7)
#' Units of every cell in the declared order (B1 SIS, B1 CON, ..., B6 CON) for the three schemes of one population: SIS
#' animals, CC1 cages or CC4 cages (CEIDs; whole cages), CON always animals within the cohort's one CON cage. Units sorted
#' C-locale (radix). Returns list(<scheme> = list(members = data.table(cell, AnimalNum, unit, unit_id), G = named sizes)).
s33a_units <- function(animals, pop) {
  a <- data.table::as.data.table(animals)
  a <- if (pop == "tracked_111") a[pop_tracked_111 == TRUE] else a[pop_canonical_117 == TRUE]
  a[, arm_ := data.table::fifelse(condition == "CON", "CON", "SIS")]
  out <- list()
  for (sch in S33A_SCHEMES) {
    memb <- list(); G <- integer()
    for (bb in S33_COHORTS) for (ar in S33A_ARMS) {
      x <- a[Batch == bb & arm_ == ar]
      if (!nrow(x)) stop("Module A: empty resampling cell ", bb, " ", ar, call. = FALSE)
      key <- if (ar == "CON" || sch == "animal") x$AnimalNum else if (sch == "cc1") x$cc1_cage else x$cc4_cage
      if (anyNA(key)) stop("Module A: missing ", sch, " unit in ", bb, " ", ar, " (", pop, ")", call. = FALSE)
      u <- sort(unique(key), method = "radix"); cell <- paste0(bb, "_", ar)
      memb[[cell]] <- data.table::data.table(cell = cell, AnimalNum = x$AnimalNum, unit = match(key, u), unit_id = key)
      G[cell] <- length(u)
    }
    out[[sch]] <- list(members = data.table::rbindlist(memb), G = G)
  }
  out
}

#' Per cell: unit sums S and present counts N (G x V) of the variables (missing values count 0 in S and N).
s33a_cell_sums <- function(units_sch, d, vars) {
  m <- units_sch$members; G <- units_sch$G
  x <- merge(m, d[, c("AnimalNum", vars), with = FALSE], by = "AnimalNum", all.x = TRUE, sort = FALSE)
  stats::setNames(lapply(names(G), function(k) {
    y <- x[cell == k]; V <- as.matrix(y[, vars, with = FALSE]); P <- is.finite(V); V[!P] <- 0
    S <- rowsum(V, y$unit, reorder = TRUE); N <- rowsum(P * 1, y$unit, reorder = TRUE)
    ix <- as.character(seq_len(G[[k]]))
    list(S = S[ix, , drop = FALSE], N = N[ix, , drop = FALSE]) }), names(G))
}

#' Means array [replicate, cohort, arm, variable]: (U S) / (U N) per cell; U NULL = the observed means (one replicate).
s33a_means_array <- function(sums, U = NULL) {
  V <- colnames(sums[[1L]]$S); B <- if (is.null(U)) 1L else nrow(U[[1L]])
  A <- array(NA_real_, dim = c(B, length(S33_COHORTS), 2L, length(V)), dimnames = list(NULL, S33_COHORTS, S33A_ARMS, V))
  for (k in names(sums)) {
    p <- strsplit(k, "_", fixed = TRUE)[[1L]]
    u <- if (is.null(U)) matrix(1, 1L, nrow(sums[[k]]$S)) else U[[k]]
    A[, p[1L], p[2L], ] <- (u %*% sums[[k]]$S) / (u %*% sums[[k]]$N)
  }
  A[!is.finite(A)] <- NA_real_
  A
}

#' Every cohort-level statistic of module A from a means array (one column per statistic, one row per replicate). Ids are
#' group|version|estimand|Batch|arm|measure. point = FALSE (resampling) leaves out the rate SIS-minus-CON quantities, which
#' carry no interval (plan section 2).
s33a_stats <- function(A, point = TRUE, sex_of = S33_SEX_OF_COHORT) {
  B <- dim(A)[1L]; co <- dimnames(A)[[2L]]; sx <- unname(sex_of[co]); vars <- dimnames(A)[[4L]]
  out <- list(); put <- function(id, v) out[[id]] <<- as.numeric(v)
  M <- function(arm, v) matrix(A[, , arm, v], nrow = B)
  for (v in intersect(S33A_MEAN_VARS, vars)) {
    S <- M("SIS", v); C <- M("CON", v)
    for (j in seq_along(co)) {
      put(s33a_id("E1", "-", "cohort_mean", co[j], "SIS", v), S[, j]); put(s33a_id("E1", "-", "cohort_mean", co[j], "CON", v), C[, j])
      if (point || v != "x_RU") put(s33a_id("E1", "-", "sis_minus_con_diff", co[j], "SIS_minus_CON", v), S[, j] - C[, j])
    }
  }
  for (v in intersect(c(S33A_OFFSET_VARS, if (point) "x_RU"), vars)) {
    S <- M("SIS", v); C <- M("CON", v); O <- s33a_centre(S, sx); P <- s33a_centre(C, sx); Q <- s33a_centre(S - C, sx)
    for (j in seq_along(co)) {
      put(s33a_id("E1b", "-", "offset_total", co[j], "SIS", v), O[, j]); put(s33a_id("E1b", "-", "part_shared_con", co[j], "CON", v), P[, j])
      put(s33a_id("E1b", "-", "part_not_shared", co[j], "SIS_minus_CON", v), Q[, j])
    }
  }
  xb <- M("SIS", "x_RU"); arms <- c(share_total = "SIS", share_shared = "CON", share_not_shared = "SIS_minus_CON")
  for (ver in c("all_six", "within_sex")) {
    sv <- if (ver == "all_six") NULL else sx
    xc <- s33a_centre(xb, sv); sxx <- rowSums(xc^2); V <- rowSums(xc * s33a_centre(M("SIS", "CombZ"), sv))
    put(s33a_id("E2", ver, "V", "-", "-", "CombZ_total"), V); put(s33a_id("E2", ver, "scp_xx", "-", "SIS", "x_RU"), sxx)
    for (k in S33A_SHARE_MEASURES) {
      vv <- if (k == "CombZ_total") "CombZ" else paste0("c_", k); S <- M("SIS", vv); C <- M("CON", vv)
      sc <- list(share_total = rowSums(xc * s33a_centre(S, sv)), share_shared = rowSums(xc * s33a_centre(C, sv)),
                 share_not_shared = rowSums(xc * s33a_centre(S - C, sv)))
      for (e in names(sc)) { put(s33a_id("E2", ver, e, "-", arms[[e]], k), 100 * sc[[e]] / V)
        put(s33a_id("E2", ver, sub("share", "slope_contrib", e, fixed = TRUE), "-", arms[[e]], k), sc[[e]] / sxx) }
    }
    for (m in intersect(S33A_SLOPE_SIS_VARS, vars))
      put(s33a_id("E3c", ver, "between_cohort_slope", "-", "SIS", m), rowSums(xc * s33a_centre(M("SIS", m), sv)) / sxx)
  }
  xw <- s33a_centre(M("CON", "x_RU"), sx); xa <- s33a_centre(xb)
  for (m in intersect(S33A_SLOPE_CON_VARS, vars)) {
    y <- M("CON", m)
    put(s33a_id("E3d", "con_x_within_sex", "between_cohort_slope", "-", "CON", m), rowSums(xw * s33a_centre(y, sx)) / rowSums(xw^2))
    put(s33a_id("E3d", "con_on_sis_x", "between_cohort_slope", "-", "CON", m), rowSums(xa * s33a_centre(y)) / rowSums(xa^2))
  }
  do.call(cbind, out)
}

#' One resampling stream: multiplicities from the index matrices, resampled means, every statistic, percentile summary.
#' Returns one row per statistic (q025, q975 type 7, n_valid, mean, sd) and the count of replicates with |V_b| < 0.25 |V|.
s33a_boot_stream <- function(sums, idx, G, V_obs) {
  U <- stats::setNames(lapply(names(idx), function(k) s33_multiplicity(idx[[k]], G[[k]])), names(idx))
  st <- s33a_stats(s33a_means_array(sums, U), point = FALSE)
  q <- vapply(seq_len(ncol(st)), function(j) { v <- st[, j]; p <- s33_percentile(v); ok <- is.finite(v)
    c(p$lo, p$hi, sum(ok), if (any(ok)) mean(v[ok]) else NA_real_, if (sum(ok) > 1L) stats::sd(v[ok]) else NA_real_) }, numeric(5))
  res <- data.table::data.table(stat_id = colnames(st), q025 = q[1L, ], q975 = q[2L, ], n_valid = as.integer(q[3L, ]), mean = q[4L, ],
                                sd = q[5L, ], small_V_count = NA_integer_)
  for (ver in names(V_obs)) { id <- s33a_id("E2", ver, "V", "-", "-", "CombZ_total")
    if (id %in% colnames(st)) res[stat_id == id, small_V_count := sum(abs(st[, id]) < 0.25 * abs(V_obs[[ver]]), na.rm = TRUE)] }
  res[]
}

#' Per-scheme percentile limits side by side and the envelope (union) per statistic.
s33a_envelope <- function(bs) {
  w <- data.table::dcast(bs, stat_id ~ scheme, value.var = c("q025", "q975", "n_valid", "sd"))
  for (s in S33A_SCHEMES) for (v in c("q025", "q975", "n_valid", "sd")) if (!paste0(v, "_", s) %in% names(w))
    data.table::set(w, j = paste0(v, "_", s), value = NA_real_)
  lo <- as.matrix(w[, paste0("q025_", S33A_SCHEMES), with = FALSE]); hi <- as.matrix(w[, paste0("q975_", S33A_SCHEMES), with = FALSE])
  anylo <- rowSums(is.finite(lo)) > 0L; anyhi <- rowSums(is.finite(hi)) > 0L
  lo2 <- lo; lo2[!is.finite(lo2)] <- Inf; hi2 <- hi; hi2[!is.finite(hi2)] <- -Inf
  w[, `:=`(envelope_low = ifelse(anylo, apply(lo2, 1L, min), NA_real_), envelope_high = ifelse(anyhi, apply(hi2, 1L, max), NA_real_))]
  w[, envelope_set_by := ifelse(anylo & anyhi, paste0("low:", S33A_SCHEMES[apply(lo2, 1L, which.min)], ";high:",
                                                     S33A_SCHEMES[apply(hi2, 1L, which.max)]), NA_character_)]
  data.table::setkey(w, stat_id)
  w[]
}

#' Bootstrap interval columns of the given statistic ids (NA where a statistic was not resampled; env NULL = none).
s33a_boot_cols <- function(env, ids) {
  if (is.null(env)) {
    na <- rep(NA_real_, length(ids)); ni <- rep(NA_integer_, length(ids))
    return(data.table::data.table(percentile_bootstrap_animal_low = na, percentile_bootstrap_animal_high = na, cage_resampling_range_cc1_low = na,
      cage_resampling_range_cc1_high = na, cage_resampling_range_cc4_low = na, cage_resampling_range_cc4_high = na, envelope_low = na,
      envelope_high = na, envelope_set_by = rep(NA_character_, length(ids)), n_valid_animal = ni, n_valid_cc1 = ni, n_valid_cc4 = ni))
  }
  e <- env[match(ids, env$stat_id)]
  data.table::data.table(percentile_bootstrap_animal_low = e$q025_animal, percentile_bootstrap_animal_high = e$q975_animal,
    cage_resampling_range_cc1_low = e$q025_cc1, cage_resampling_range_cc1_high = e$q975_cc1,
    cage_resampling_range_cc4_low = e$q025_cc4, cage_resampling_range_cc4_high = e$q975_cc4,
    envelope_low = e$envelope_low, envelope_high = e$envelope_high, envelope_set_by = e$envelope_set_by,
    n_valid_animal = as.integer(e$n_valid_animal), n_valid_cc1 = as.integer(e$n_valid_cc1), n_valid_cc4 = as.integer(e$n_valid_cc4))
}

# ---------------------------------------------------------------- cohort-level shares, re-pairings, reference SDs
#' Share rows of one version from a point means array: SCP over the given cohorts (all-cohort or within-sex centring).
s33a_share_rows <- function(Ap, cohorts = S33_COHORTS, sex = FALSE, xvar = "x_RU") {
  co <- cohorts; sx <- if (sex) unname(S33_SEX_OF_COHORT[co]) else NULL
  g <- function(arm, v) matrix(Ap[1L, co, arm, v], nrow = 1L)
  xc <- s33a_centre(g("SIS", xvar), sx); sxx <- sum(xc^2); V <- sum(xc * s33a_centre(g("SIS", "CombZ"), sx))
  data.table::rbindlist(lapply(S33A_SHARE_MEASURES, function(k) {
    vv <- if (k == "CombZ_total") "CombZ" else paste0("c_", k); zv <- if (k == "CombZ_total") "CombZ" else k
    S <- g("SIS", vv); C <- g("CON", vv); zS <- g("SIS", zv); zC <- g("CON", zv)
    scp <- c(sum(xc * s33a_centre(S, sx)), sum(xc * s33a_centre(C, sx)), sum(xc * s33a_centre(S - C, sx)))
    scz <- c(sum(xc * s33a_centre(zS, sx)), sum(xc * s33a_centre(zC, sx)), sum(xc * s33a_centre(zS - zC, sx)))
    data.table::data.table(measure = k, estimand = c("share_total", "share_shared", "share_not_shared"), arm = c("SIS", "CON", "SIS_minus_CON"),
                           scp = scp, V = V, scp_xx = sxx, scp_z = scz, n_cohorts = length(co)) }))
}

#' The 36 within-sex re-pairings of the CON cages with the cohorts (identity first): shared shares, all-six and within-sex.
s33a_repairings <- function(Ap, xvar = "x_RU") {
  perms <- list(1:3, c(1L, 3L, 2L), c(2L, 1L, 3L), c(2L, 3L, 1L), c(3L, 1L, 2L), c(3L, 2L, 1L))
  rows <- list(); pid <- 0L
  for (pf in seq_along(perms)) for (pm in seq_along(perms)) {
    pid <- pid + 1L; Ar <- Ap
    Ar[1L, S33A_FEMALE, "CON", ] <- Ap[1L, S33A_FEMALE[perms[[pf]]], "CON", ]
    Ar[1L, S33A_MALE, "CON", ] <- Ap[1L, S33A_MALE[perms[[pm]]], "CON", ]
    for (ver in c("all_six", "within_sex")) {
      sr <- s33a_share_rows(Ar, sex = ver == "within_sex", xvar = xvar)[estimand == "share_shared"]
      rows[[length(rows) + 1L]] <- sr[, .(version = ver, measure, pairing_id = pid, identity = pf == 1L && pm == 1L,
        female_con_cages_from = paste(S33A_FEMALE[perms[[pf]]], collapse = ","), male_con_cages_from = paste(S33A_MALE[perms[[pm]]], collapse = ","),
        share_shared = 100 * scp / V)]
    }
  }
  data.table::rbindlist(rows)
}

#' Reference SDs of one measure (plan E1b; lower bounds, never criteria). `d` = animal frame of all 117.
s33a_reference_sds <- function(d, m) {
  y <- d[[m]]; x <- d[, .(Batch, Sex, arm, tracked, cc1_cage, cc4_cage, y = y)]
  cage_sd <- function(z, cage_col, min_n) {
    cs <- z[, .(n_tr = .N, mu = if (any(is.finite(y))) mean(y, na.rm = TRUE) else NA_real_), by = c("Batch", cage_col)]
    cs <- cs[n_tr >= min_n & is.finite(mu)]; cs[, dev := mu - mean(mu), by = Batch]
    k <- nrow(cs); J <- data.table::uniqueN(cs$Batch)
    list(sd = if (k > J) sqrt(sum(cs$dev^2) / (k - J)) else NA_real_, n_cages = k, df = k - J, cages = sort(cs[[cage_col]], method = "radix"))
  }
  pooled <- function(z, g) { z <- z[is.finite(y)]; if (nrow(z) <= data.table::uniqueN(z[[g]])) return(NA_real_)
    sqrt(sum((z$y - stats::ave(z$y, z[[g]]))^2) / (nrow(z) - data.table::uniqueN(z[[g]]))) }
  sis_t <- x[arm == "SIS" & tracked == TRUE]; con <- x[arm == "CON"]
  r1 <- cage_sd(sis_t, "cc1_cage", 3L); r4 <- cage_sd(sis_t, "cc4_cage", 3L); r4a <- cage_sd(x[arm == "SIS"], "cc4_cage", 1L)
  cm <- con[, .(mu = mean(y, na.rm = TRUE)), by = .(Batch, Sex)][, dev := mu - mean(mu), by = Sex]
  data.table::data.table(measure = m, cage_sd_reference_cc1 = r1$sd, cage_sd_reference_cc1_n_cages = r1$n_cages,
    cage_sd_reference_cc4 = r4$sd, cage_sd_reference_cc4_n_cages = r4$n_cages, cage_sd_reference_cc4_all117 = r4a$sd,
    cage_sd_reference_cc4_all117_n_cages = r4a$n_cages, half_within_sd_sis = pooled(sis_t, "Batch") / 2,
    half_within_sd_con = pooled(con, "Batch") / 2,
    con_cage_mean_sd_within_sex = if (nrow(cm) > 2L) sqrt(sum(cm$dev^2) / (nrow(cm) - 2L)) else NA_real_,
    cc1_cages = paste(r1$cages, collapse = ";"))
}

# ---------------------------------------------------------------- a01 rows of one cohort x measure (E1, S1, S3, S4, S8)
# S8 (plan section 8 populations 'drops HW_A1, J7'): keep-functions on an animal frame
S33A_S8 <- list(S8_drop_HW_A1 = function(f) !(f$hw_a1 %in% TRUE), S8_drop_J7 = function(f) !(f$J7 %in% TRUE))

#' The a01 rows of one cohort x measure: the SIS mean (envelope; without resampling: CR2 by CC1 cage), per-cohort CR2 by
#' CC1 and by CC4 cage and the KR cohort mean beside it; the CON mean (t(n - 1), flagged); SIS-minus-CON (envelope and the
#' animal Welch interval, flagged; the rate: point value only, plan sections 2 and 8) with its raw companion D / b_s.
#' `est` = point values (SIS, CON, SIS-minus-CON) from the means array, or NULL (means of present values computed here);
#' `env`/`ids` = bootstrap envelope and statistic ids, or NULL (S8 rows); `con` = FALSE leaves out the CON row.
s33a_a01_cell <- function(pop, bb, m, fs, fc, slopes, est = NULL, env = NULL, ids = NULL, kr = NULL, con = TRUE) {
  sx <- S33_SEX_OF_COHORT[[bb]]; ys <- fs[[m]]; yc <- fc[[m]]; fs_ok <- is.finite(ys); fc_ok <- is.finite(yc); boot <- !is.null(env)
  mean_or_na <- function(v) if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
  if (is.null(est)) est <- c(mean_or_na(ys), mean_or_na(yc), mean_or_na(ys) - mean_or_na(yc))
  raw <- slopes[Sex == sx & measure == (if (m %in% c("dcort_response_part", "dcort_baseline_part")) "delta_cort" else m)]
  bslope <- if (nrow(raw) == 1L) raw$map_slope else NA_real_; runit <- if (nrow(raw) == 1L) S33A_RAW[measure == raw$measure, raw_unit] else NA_character_
  com <- list(population = pop, Batch = bb, Sex = sx, measure = m, missing_convention = S33A_LAB$e1, map_slope_z_per_raw = bslope, raw_unit = runit,
              procedural_alternatives = unname(S33A_PROCEDURAL[m]))
  pop_note <- if (pop %in% names(S33A_S8)) S33A_LAB[[pop]] else NA_character_
  # SIS
  c1 <- s33a_cr2_cell(ys, fs$cc1_cage); c4 <- s33a_cr2_cell(ys, fs$cc4_cage); flat <- isTRUE(c1$no_spread)
  bc <- s33a_boot_cols(env, if (boot) ids[1L] else NA_character_)
  prim <- if (flat) list(lo = NA_real_, hi = NA_real_, se = NA_real_, df = NA_real_, im = "none")
    else if (boot) list(lo = bc$envelope_low, hi = bc$envelope_high, se = NA_real_, df = NA_real_, im = "envelope_bootstrap")
    else list(lo = c1$ci_low, hi = c1$ci_high, se = c1$se, df = c1$df, im = if (is.finite(c1$ci_low)) "CR2_Satterthwaite_cc1_cage" else "none")
  krc <- if (!is.null(kr) && nrow(kr) == 1L) kr[, .(kr_estimate, kr_se, kr_df, kr_ci_low, kr_ci_high, kr_interval_method, kr_status, kr_singular)][, kr_label := S33A_LAB$kr_common]
    else data.table::data.table(kr_estimate = NA_real_, kr_se = NA_real_, kr_df = NA_real_, kr_ci_low = NA_real_, kr_ci_high = NA_real_,
                                kr_interval_method = NA_character_, kr_status = NA_character_, kr_singular = NA, kr_label = NA_character_)
  sis <- cbind(do.call(s33_row, c(list(estimand = "cohort_mean", level = "cohort", estimate = est[1L], se = prim$se, df = prim$df, ci_low = prim$lo,
      ci_high = prim$hi, interval_method = prim$im, interval_basis = if (prim$im == "none") NA_character_ else "conditional_on_cohorts",
      units = s33a_unit_mean(m), metric_id = s33a_metric(m), n_animals = sum(fs_ok), n_cages = data.table::uniqueN(fs$cc1_cage[fs_ok]), n_cohorts = 1L,
      n_params = 1L, arm = "SIS", n_cc4_cages = data.table::uniqueN(fs$cc4_cage[fs_ok]),
      interval_note = if (flat) S33A_NO_SPREAD_NOTE else if (boot) S33A_LAB$range80 else S33A_LAB$cr2_cell, note = pop_note), com)),
    bc, krc, data.table::data.table(cr2_cc1_se = c1$se, cr2_cc1_df = c1$df, cr2_cc1_ci_low = c1$ci_low, cr2_cc1_ci_high = c1$ci_high,
      cr2_cc1_n_clusters = as.integer(c1$n_clusters), cr2_cc4_se = c4$se, cr2_cc4_df = c4$df, cr2_cc4_ci_low = c4$ci_low, cr2_cc4_ci_high = c4$ci_high,
      cr2_cc4_n_clusters = as.integer(c4$n_clusters)))
  rows <- list(sis)
  # CON t(n - 1), flagged
  if (con) { ct <- s33a_mean_t(yc)
    rows[[2L]] <- do.call(s33_row, c(list(estimand = "cohort_mean", level = "cohort", estimate = est[2L], se = ct$se, df = ct$df, ci_low = ct$ci_low,
      ci_high = ct$ci_high, interval_method = if (is.finite(ct$ci_low)) paste0("t_", ct$df) else "none",
      interval_basis = if (is.finite(ct$ci_low)) "conditional_on_cohorts" else NA_character_, units = s33a_unit_mean(m), metric_id = s33a_metric(m),
      n_animals = ct$n, n_cages = 1L, n_cohorts = 1L, n_params = 1L, arm = "CON", ci_flag = S33A_LAB$con_flag,
      interval_note = if (isTRUE(ct$no_spread)) S33A_NO_SPREAD_NOTE else NA_character_, note = S33A_LAB$con_phrase), com)) }
  # SIS minus CON
  n_d <- sum(fs_ok) + sum(fc_ok)
  if (m == "x_RU") {
    rows[[length(rows) + 1L]] <- do.call(s33_row, c(list(estimand = "sis_minus_con_diff", level = "cohort", estimate = est[3L], interval_method = "none",
      units = s33a_unit_mean(m), metric_id = s33a_metric(m), n_animals = n_d, n_cohorts = 1L, arm = "SIS_minus_CON",
      note = paste(stats::na.omit(c(S33A_LAB$rate_point, pop_note)), collapse = "; ")), com))
  } else {
    bd <- s33a_boot_cols(env, if (boot) ids[3L] else NA_character_); w <- s33a_welch(ys, yc)
    flat2 <- !is.finite(w$se) && s33a_no_spread(ys) && s33a_no_spread(yc)
    pr2 <- if (flat2) list(lo = NA_real_, hi = NA_real_, se = NA_real_, df = NA_real_, im = "none")
      else if (boot) list(lo = bd$envelope_low, hi = bd$envelope_high, se = NA_real_, df = NA_real_, im = "envelope_bootstrap")
      else list(lo = w$ci_low, hi = w$ci_high, se = w$se, df = w$df, im = if (is.finite(w$ci_low)) "Welch" else "none")
    rows[[length(rows) + 1L]] <- cbind(do.call(s33_row, c(list(estimand = "sis_minus_con_diff", level = "cohort", estimate = est[3L], se = pr2$se,
      df = pr2$df, ci_low = pr2$lo, ci_high = pr2$hi, interval_method = pr2$im, interval_basis = if (pr2$im == "none") NA_character_ else "conditional_on_cohorts",
      units = s33a_unit_mean(m), metric_id = s33a_metric(m), n_animals = n_d, n_cohorts = 1L, arm = "SIS_minus_CON", ci_flag = S33A_LAB$con_flag,
      interval_note = if (flat2) S33A_NO_SPREAD_NOTE else if (boot) S33A_LAB$range80 else NA_character_,
      sis_minus_con_raw = if (is.finite(bslope)) est[3L] / bslope else NA_real_, note = pop_note), com)),
      bd, data.table::data.table(welch_se = w$se, welch_df = w$df, welch_ci_low = w$ci_low, welch_ci_high = w$ci_high))
  }
  data.table::rbindlist(rows, fill = TRUE)
}

# ---------------------------------------------------------------- model rows (frozen engine; no p-values)
#' Rows from a frozen-engine fit and one KR contrast (Satterthwaite fallback flagged; FAILED rows kept).
s33a_kr_row <- function(m, weights, estimand, level, basis, units, metric_id = "crossing_rate", lead = FALSE, n_cages = NA_integer_, ...) {
  k <- s33_kr_contrast(m, weights, estimand); info <- m$info
  msg <- c(info$messages, k$kr_messages, m$failure_reason, k$failure_reason); msg <- unique(msg[!is.na(msg) & nzchar(trimws(msg))])
  msg <- if (length(msg)) paste(msg, collapse = " | ") else NA_character_
  s33_row(estimand = estimand, level = level, estimate = k$estimate, se = k$se, df = k$df, ci_low = k$ci_low, ci_high = k$ci_high,
          interval_method = k$interval_method, interval_basis = basis, units = units, metric_id = metric_id, status = k$status,
          singular = k$singular, ddf_fallback = k$ddf_fallback, lead = lead, n_animals = info$n_animals, n_cages = n_cages,
          n_cohorts = info$n_batches, n_params = info$n_fixed_cols, model_id = info$model_id, formula = info$formula,
          engine_messages = msg, ...)
}

#' OLS with cluster-robust CR2 contrasts of one term for each cluster column (no p-values); FAILED if rank deficient. An
#' exact fit (residuals 0 to rounding, e.g. a constant outcome) has no sampling interval: it is reported with the point
#' value and exact_fit = TRUE instead of being passed to summary.lm / clubSandwich ('essentially perfect fit').
s33a_ols <- function(dat, rhs, term, clusters) {
  fit <- tryCatch(stats::lm(stats::as.formula(paste("y ~", rhs)), data = dat), error = function(e) e)
  bad <- inherits(fit, "error") || qr(stats::model.matrix(fit))$rank < length(stats::coef(fit)) || anyNA(stats::coef(fit))
  none <- stats::setNames(lapply(clusters, function(c) NULL), clusters)
  if (bad) return(list(status = "FAILED", estimate = NA_real_, se = NA_real_, df = NA_real_, n_params = NA_integer_, cr2 = none, exact_fit = NA))
  if (fit$df.residual < 1L || max(abs(stats::residuals(fit))) <= 1e-12 * max(1, abs(dat$y), na.rm = TRUE))
    return(list(status = "OK", estimate = unname(stats::coef(fit)[term]), se = NA_real_, df = NA_real_, n_params = length(stats::coef(fit)),
                cr2 = none, exact_fit = TRUE))
  w <- stats::setNames(1, term)
  cr <- stats::setNames(lapply(clusters, function(cl) s33_cr2_contrast(fit, dat[[cl]], list(w))), clusters)
  list(status = "OK", estimate = unname(stats::coef(fit)[term]), se = sqrt(stats::vcov(fit)[term, term]), df = fit$df.residual,
       n_params = length(stats::coef(fit)), cr2 = cr, exact_fit = FALSE)
}
S33A_EXACT_FIT_NOTE <- "exact fit (no residual variation): point value only, no interval"

#' CR2 columns (estimate, se, df, interval) of one cluster from s33a_ols(), named <prefix>_*.
s33a_cr2_cols <- function(o, cl, prefix) {
  z <- o$cr2[[cl]]
  if (is.null(z)) z <- data.table::data.table(estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n_clusters = NA_integer_)
  x <- data.table::data.table(z$estimate, z$se, z$df, z$ci_low, z$ci_high, as.integer(z$n_clusters))
  data.table::setnames(x, paste0(prefix, c("_estimate", "_se", "_df", "_ci_low", "_ci_high", "_n_clusters")))
  x
}

#' Analysis rows of one measure for the individual-level SIS models: complete outcome and x, covariates centred within
#' cohort among the model's animals.
s33a_model_data <- function(sis, m, xcol = "x_RU") {
  dat <- sis[is.finite(get(m)) & is.finite(get(xcol)), .(AnimalNum, Batch = factor(Batch, levels = S33_COHORTS), Sex, cc1_cage, cc4_cage,
    CageEpisodeID = cc1_cage, y = get(m), x = get(xcol), tp1, tp2, age_cc1)]
  dat[, `:=`(tp2_c = tp2 - mean(tp2), tp1_c = tp1 - mean(tp1), age_c = age_cc1 - mean(age_cc1), xbar_b = mean(x)), by = Batch]
  dat[, `:=`(x_w = x - xbar_b, tp2_w = tp2_c, sex_c = data.table::fifelse(Sex == "Female", 0.5, -0.5))]
  dat[]
}

#' E3(a): within-cohort KR slopes, m ~ 0 + Batch + x (+ tp2_c | tp1_c + age_c) + (1 | cc1_cage) + (1 | cc4_cage).
s33a_e3a_rows <- function(sis, measures, adjust = c("none", "tp2_c", "tp1_c_age_c"), xcol = "x_RU", model = "E3a_KR", variant = "primary",
                          lead_measures = character(), note = NA_character_) {
  rows <- list()
  for (m in measures) for (adj in adjust) {
    dat <- s33a_model_data(sis, m, xcol)
    add <- c(none = "", tp2_c = " + tp2_c", tp1_c_age_c = " + tp1_c + age_c")[[adj]]
    rank <- nlevels(droplevels(dat$Batch)) + 1L + c(none = 0L, tp2_c = 1L, tp1_c_age_c = 2L)[[adj]]
    fit <- s33_kr_fit(paste0("y ~ 0 + Batch + x", add, " + (1 | cc1_cage) + (1 | cc4_cage)"), dat, s33a_id("A", model, variant, m, adj), rank)
    nt <- c(note, if (m == "CombZ") S33A_LAB$combz_a05 else if (m %in% S33A_Z_MEASURES) S33A_LAB$decomp, if (m == "CombZ" && adj != "none") S33A_LAB$combz_tp2)
    rows[[length(rows) + 1L]] <- s33a_kr_row(fit, list(x = 1), "within_cohort_slope", "animal_within_cohort", "conditional_on_cohorts",
      if (xcol == "Movement_mean") S33A_U$mm else s33a_unit_slope(m), metric_id = s33a_metric(if (xcol == "Movement_mean") xcol else "x_RU"),
      lead = adj %in% c("none", "tp2_c") && m %in% lead_measures, n_cages = data.table::uniqueN(dat$cc1_cage),
      model = model, variant = variant, measure = m, arm = "SIS", adjustment = adj,
      mech_coupling = if (adj == "tp2_c") unname(S33A_MECH_COUPLING[m]) else NA_character_, n_cc4_cages = data.table::uniqueN(dat$cc4_cage),
      note = paste(stats::na.omit(nt), collapse = "; "))
  }
  data.table::rbindlist(rows, fill = TRUE)
}

#' E3(a) contribution scale: OLS FE c_k ~ 0 + Batch + x (+ tp2_c), CR2 by CC1 cage (primary) and by CC4 cage.
s33a_ols_fe_rows <- function(sis, measures, adjust = c("none", "tp2_c"), xcol = "x_RU", model = "E3a_OLS_FE", variant = "primary", note = NA_character_) {
  rows <- list()
  for (m in measures) for (adj in adjust) {
    dat <- s33a_model_data(sis, m, xcol)
    o <- s33a_ols(dat, paste0("0 + Batch + x", if (adj == "tp2_c") " + tp2_c" else ""), "x", c("cc1_cage", "cc4_cage"))
    c1 <- o$cr2$cc1_cage; has_ci <- is.finite(c1$ci_low %a_or% NA_real_)
    rows[[length(rows) + 1L]] <- cbind(s33_row("within_cohort_slope", "animal_within_cohort", estimate = o$estimate, se = c1$se %a_or% NA_real_,
      df = c1$df %a_or% NA_real_, ci_low = c1$ci_low %a_or% NA_real_, ci_high = c1$ci_high %a_or% NA_real_,
      interval_method = if (has_ci) "CR2_Satterthwaite_cc1_cage" else "none", interval_basis = if (has_ci) "conditional_on_cohorts" else NA_character_,
      interval_note = if (isTRUE(o$exact_fit)) S33A_EXACT_FIT_NOTE else NA_character_,
      units = if (xcol == "Movement_mean") S33A_U$mm else s33a_unit_slope(m), metric_id = s33a_metric(if (xcol == "Movement_mean") xcol else "x_RU"),
      status = o$status, n_animals = data.table::uniqueN(dat$AnimalNum), n_cages = data.table::uniqueN(dat$cc1_cage), n_cohorts = data.table::uniqueN(dat$Batch),
      n_params = o$n_params, model = model, variant = variant, measure = m, arm = "SIS", adjustment = adj,
      mech_coupling = if (adj == "tp2_c") unname(S33A_MECH_COUPLING[sub("^c_", "", m)]) else NA_character_,
      n_cc4_cages = data.table::uniqueN(dat$cc4_cage), note = note), s33a_cr2_cols(o, "cc4_cage", "cr2_cc4"))
  }
  data.table::rbindlist(rows, fill = TRUE)
}

#' E3(b): CON within its one cage, OLS m ~ 0 + Batch + x (+ tp2_c), t on the residual df; flagged.
s33a_e3b_rows <- function(con, measures, xcol = "x_RU") {
  rows <- list()
  for (m in measures) for (adj in c("none", "tp2_c")) {
    dat <- s33a_model_data(con, m, xcol)
    o <- s33a_ols(dat, paste0("0 + Batch + x", if (adj == "tp2_c") " + tp2_c" else ""), "x", character())
    ci <- s33_t_interval(o$estimate, o$se, o$df); has_ci <- is.finite(ci$ci_low)
    rows[[length(rows) + 1L]] <- s33_row("within_cohort_slope", "animal_within_cohort", estimate = o$estimate, se = o$se, df = o$df,
      ci_low = ci$ci_low, ci_high = ci$ci_high, interval_method = if (has_ci) paste0("t_", o$df) else "none",
      interval_basis = if (has_ci) "conditional_on_cohorts" else NA_character_, interval_note = if (isTRUE(o$exact_fit)) S33A_EXACT_FIT_NOTE else NA_character_,
      units = s33a_unit_slope(m), metric_id = "crossing_rate", status = o$status,
      n_animals = nrow(dat), n_cages = data.table::uniqueN(dat$cc1_cage), n_cohorts = data.table::uniqueN(dat$Batch), n_params = o$n_params,
      model = "E3b_CON_OLS", variant = "primary", measure = m, arm = "CON", adjustment = adj,
      mech_coupling = if (adj == "tp2_c") unname(S33A_MECH_COUPLING[m]) else NA_character_, ci_flag = S33A_LAB$con_flag, note = S33A_LAB$con_phrase)
  }
  data.table::rbindlist(rows, fill = TRUE)
}

#' E3(e): contextual model m ~ x_w + xbar_b (+ tp2_w) (+ sex_c) + (1 | Batch) + (1 | cc1_cage) + (1 | cc4_cage): between-cohort,
#' within-cohort and their difference.
s33a_e3e_rows <- function(sis, measures, xcol = "x_RU", cc4 = TRUE, model = "E3e_contextual_KR", variants = c("primary", "plus_sex_c"),
                          adjust = c("none", "tp2_w"), note = NA_character_) {
  rows <- list()
  for (m in measures) for (adj in adjust) for (vr in variants) {
    dat <- s33a_model_data(sis, m, xcol)
    add <- paste0(if (adj == "tp2_w") " + tp2_w" else "", if (vr == "plus_sex_c") " + sex_c" else "")
    rank <- 3L + (adj == "tp2_w") + (vr == "plus_sex_c")
    re <- if (cc4) " + (1 | Batch) + (1 | cc1_cage) + (1 | cc4_cage)" else " + (1 | Batch) + (1 | cc1_cage)"
    fit <- s33_kr_fit(paste0("y ~ x_w + xbar_b", add, re), dat, s33a_id("A", model, vr, m, adj), rank)
    spec <- list(list(list(xbar_b = 1), "between_cohort_slope", "cohort", "cohort_scatter_model"),
                 list(list(x_w = 1), "within_cohort_slope", "animal_within_cohort", "conditional_on_cohorts"),
                 list(list(xbar_b = 1, x_w = -1), "contextual_cohort_minus_within", "cohort", "cohort_scatter_model"))
    for (s in spec) rows[[length(rows) + 1L]] <- s33a_kr_row(fit, s[[1L]], s[[2L]], s[[3L]], s[[4L]],
      if (xcol == "Movement_mean") S33A_U$mm else s33a_unit_slope(m), metric_id = s33a_metric(if (xcol == "Movement_mean") xcol else "x_RU"),
      n_cages = data.table::uniqueN(dat$cc1_cage), model = model, variant = vr, measure = m, arm = "SIS", adjustment = adj,
      mech_coupling = if (adj == "tp2_w") unname(S33A_MECH_COUPLING[m]) else NA_character_, n_cc4_cages = data.table::uniqueN(dat$cc4_cage),
      note = paste(stats::na.omit(c(note, if (s[[4L]] == "cohort_scatter_model") paste(S33A_LAB$scatter, S33A_LAB$weight, sep = "; "))), collapse = "; "))
  }
  data.table::rbindlist(rows, fill = TRUE)
}

#' Six-point (cohort-scatter) rows for one measure: SIS all six t(4) and within sex t(3) with the envelope, LOBO range and
#' omit sets; CON (E3(d)) within sex t(3) and on the SIS rate t(4), flagged. `cm` = point cohort means (Batch, arm, measure, value).
s33a_scatter_rows <- function(cm, m, env, pop = "tracked_111", lead = FALSE, units = s33a_unit_slope(m), model_sis = "E3c_six_point",
                              model_con = "E3d_CON_six_point", con = TRUE) {
  cmv <- function(arm, v) { x <- cm[arm_ == arm & measure == v]; stats::setNames(x$value, x$Batch)[S33_COHORTS] }
  xs <- cmv("SIS", "x_RU"); ys <- cmv("SIS", m); sx <- unname(S33_SEX_OF_COHORT[S33_COHORTS])
  lobo <- vapply(S33_COHORTS, function(b) s33a_scatter_slope(ys[names(ys) != b], xs[names(xs) != b])$estimate, 0)
  om <- lapply(S33A_OMIT_SETS, function(o) s33a_scatter_slope(ys[!names(ys) %in% o], xs[!names(xs) %in% o]))
  rows <- list()
  for (ver in c("all_six", "within_sex")) {
    f <- s33a_scatter_slope(ys, xs, if (ver == "within_sex") sx else NULL)
    bc <- s33a_boot_cols(env, s33a_id("E3c", ver, "between_cohort_slope", "-", "SIS", m))
    rows[[length(rows) + 1L]] <- cbind(s33_row("between_cohort_slope", "cohort", estimate = f$estimate, se = f$se, df = f$df, ci_low = f$ci_low,
      ci_high = f$ci_high, interval_method = s33a_t_method(f$df), interval_basis = "cohort_scatter_model", units = units, metric_id = "crossing_rate",
      lead = lead && ver == "all_six", n_cohorts = f$n, n_params = if (ver == "all_six") 2L else 3L, model = model_sis, variant = ver,
      measure = m, arm = "SIS", adjustment = "none", population = pop,
      lobo_min = if (ver == "all_six") min(lobo, na.rm = TRUE) else NA_real_, lobo_max = if (ver == "all_six") max(lobo, na.rm = TRUE) else NA_real_,
      omit_B2_B6_estimate = if (ver == "all_six") om$omit_B2_B6$estimate else NA_real_,
      omit_B2_B6_ci_low = if (ver == "all_six") om$omit_B2_B6$ci_low else NA_real_, omit_B2_B6_ci_high = if (ver == "all_six") om$omit_B2_B6$ci_high else NA_real_,
      omit_B5_B6_estimate = if (ver == "all_six") om$omit_B5_B6$estimate else NA_real_,
      omit_B5_B6_ci_low = if (ver == "all_six") om$omit_B5_B6$ci_low else NA_real_, omit_B5_B6_ci_high = if (ver == "all_six") om$omit_B5_B6$ci_high else NA_real_,
      note = paste(S33A_LAB$scatter, S33A_LAB$weight, "no weight adjustment on six points (r 0.977)", sep = "; ")), bc)
  }
  if (con) {
    xc <- cmv("CON", "x_RU"); yc <- cmv("CON", m)
    for (ver in c("con_x_within_sex", "con_on_sis_x")) {
      f <- if (ver == "con_x_within_sex") s33a_scatter_slope(yc, xc, sx) else s33a_scatter_slope(yc, xs)
      bc <- s33a_boot_cols(env, s33a_id("E3d", ver, "between_cohort_slope", "-", "CON", m))
      rows[[length(rows) + 1L]] <- cbind(s33_row("between_cohort_slope", "cohort", estimate = f$estimate, se = f$se, df = f$df, ci_low = f$ci_low,
        ci_high = f$ci_high, interval_method = s33a_t_method(f$df), interval_basis = "cohort_scatter_model", units = units, metric_id = "crossing_rate",
        n_cohorts = f$n, n_params = if (ver == "con_x_within_sex") 3L else 2L, model = model_con, variant = ver, measure = m, arm = "CON",
        adjustment = "none", population = pop, ci_flag = S33A_LAB$con_flag, note = paste(S33A_LAB$con_between, S33A_LAB$con_phrase, sep = "; ")), bc)
    }
  }
  data.table::rbindlist(rows, fill = TRUE)
}

#' S8 six-point SIS rows (all six cohorts, t(4)) on the cohort means of an animal frame without the S8 animals; no
#' resampling stream is declared for S8, so the t interval stands alone.
s33a_s8_six_point <- function(ft, s8, measures, model) {
  f <- data.table::as.data.table(ft)[S33A_S8[[s8]](ft) & arm == "SIS"]
  mean_fin <- function(v) if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
  cmx <- f[, lapply(.SD, mean_fin), by = Batch, .SDcols = c("x_RU", measures)][match(S33_COHORTS, Batch)]
  data.table::rbindlist(lapply(measures, function(m) { s <- s33a_scatter_slope(cmx[[m]], cmx$x_RU)
    s33_row("between_cohort_slope", "cohort", estimate = s$estimate, se = s$se, df = s$df, ci_low = s$ci_low, ci_high = s$ci_high,
            interval_method = if (is.finite(s$df)) paste0("t_", s$df) else "none", interval_basis = "cohort_scatter_model", units = s33a_unit_slope(m),
            metric_id = "crossing_rate", n_cohorts = s$n, n_params = 2L, model = model, variant = s8, measure = m, arm = "SIS", adjustment = "none",
            population = "tracked_111", note = paste(S33A_LAB[[s8]], S33A_LAB$scatter, S33A_LAB$weight, sep = "; ")) }))
}

# ---------------------------------------------------------------- E5 weight gains (plan E5)
#' Episode frame: gain of the interval that contains each A1 (CC1 tp3 - tp2 ... CC4 tp6 - tp5), its start weight, days, rate.
s33a_gain_frame <- function(d, episodes) {
  ep <- data.table::as.data.table(episodes)[, .(AnimalNum, CC, CageEpisodeID, x = crossing_rate / S33_RU, hw = hardware_flag %in% TRUE)]
  w <- d[tracked == TRUE, .(AnimalNum, Batch, Sex, arm, J7, tp1, age_cc1, weight_dev, tp2, tp3, tp4, tp5, tp6,
                            t2 = as.numeric(tp2_date), t3 = as.numeric(tp3_date), t4 = as.numeric(tp4_date), t5 = as.numeric(tp5_date), t6 = as.numeric(tp6_date))]
  g <- merge(ep, w, by = "AnimalNum")
  k <- as.integer(sub("^CC", "", g$CC)); i <- seq_len(nrow(g))
  W <- as.matrix(g[, paste0("tp", 2:6), with = FALSE]); Tm <- as.matrix(g[, paste0("t", 2:6), with = FALSE])
  g[, `:=`(ws = W[cbind(i, k)], we = W[cbind(i, k + 1L)], start_date = as.Date(Tm[cbind(i, k)], origin = "1970-01-01"),
           end_date = as.Date(Tm[cbind(i, k + 1L)], origin = "1970-01-01"), start_tp = paste0("tp", k + 1L), end_tp = paste0("tp", k + 2L))]
  g[, `:=`(gain = we - ws, days = as.numeric(end_date - start_date))][, gain_per_day := gain / days]
  data.table::setorder(g, Batch, AnimalNum, CC)
  g[]
}

#' One E5 model: KR rows of its x terms plus OLS fixed-part CR2 by animal and by cage episode beside them.
s33a_e5_model <- function(g, arm_k, form = c("a", "b", "c", "d"), ws = FALSE, ycol = "gain", variant = "primary", keep = NULL) {
  form <- match.arg(form)
  dat <- g[arm == arm_k & is.finite(get(ycol)) & is.finite(x)]
  if (!is.null(keep)) dat <- dat[keep(dat)]
  dat[, `:=`(y = get(ycol), BxCC = factor(paste(Batch, CC, sep = "_")))]
  an <- unique(dat[, .(AnimalNum, Batch, age_cc1, tp1)])[, `:=`(age_c = age_cc1 - mean(age_cc1), w1_c = tp1 - mean(tp1)), by = Batch]
  dat[an, on = "AnimalNum", `:=`(age_c = i.age_c, w1_c = i.w1_c)]
  dat[, ws_c := ws - mean(ws), by = .(Batch, CC)]
  dat[, xbar_i := mean(x), by = AnimalNum][, x_wi := x - xbar_i]
  ai <- unique(dat[, .(AnimalNum, Batch, xbar_i)])[, x_ib := xbar_i - mean(xbar_i), by = Batch]
  dat[ai, on = "AnimalNum", x_ib := i.x_ib]
  for (cc in S33_CC) data.table::set(dat, j = paste0("x_", tolower(cc)), value = dat$x * (dat$CC == cc))
  ccs <- sort(unique(dat$CC))
  xterms <- switch(form, a = "x", d = "x", b = c("x_wi", "x_ib"), c = paste0("x_", tolower(ccs)))
  covs <- c(if (form != "d") "age_c", "w1_c", if (ws) "ws_c")
  rhs_fixed <- paste(c("0 + BxCC", xterms, covs), collapse = " + ")
  re <- if (form == "d") " + (1 | AnimalNum)" else " + (1 | AnimalNum) + (1 | CageEpisodeID)"
  rank <- nlevels(droplevels(dat$BxCC)) + length(xterms) + length(covs)
  mid <- s33a_id("A", "E5", form, arm_k, variant, if (ws) "ws_c" else "none")
  fit <- s33_kr_fit(paste0("y ~ ", rhs_fixed, re), dat, mid, rank)
  ols <- lapply(xterms, function(tm) s33a_ols(dat, rhs_fixed, tm, c("AnimalNum", "CageEpisodeID")))
  units <- if (ycol == "gain_per_day") S33A_U$gday_ru else S33A_U$g_ru
  rows <- lapply(seq_along(xterms), function(i) {
    tm <- xterms[i]
    est <- if (tm == "x_wi") "within_animal_across_episodes" else if (tm == "x_ib") "between_animals_within_cohort" else "within_cohort_slope"
    lvl <- if (tm == "x_wi") "within_animal" else "animal_within_cohort"
    nt <- c(if (ws) S33A_LAB$ws, if (arm_k == "CON") paste(S33A_LAB$con_flag, S33A_LAB$con_gain, sep = "; "))
    cbind(s33a_kr_row(fit, stats::setNames(list(1), tm), est, lvl, "conditional_on_cohorts", units, n_cages = data.table::uniqueN(dat$CageEpisodeID),
      model = paste0("E5", form), variant = variant, arm = arm_k, term = tm, episode = if (form == "c") toupper(sub("^x_", "", tm)) else "all",
      adjustment = if (ws) "ws_c" else "none", n_obs = nrow(dat), ci_flag = if (arm_k == "CON") S33A_LAB$con_flag else NA_character_,
      note = if (length(nt)) paste(nt, collapse = "; ") else NA_character_),
      s33a_cr2_cols(ols[[i]], "AnimalNum", "cr2_animal"), s33a_cr2_cols(ols[[i]], "CageEpisodeID", "cr2_cage_episode"))
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' All E5 model rows (a09): (a)-(d) with and without ws_c, and the sensitivities of (a).
s33a_e5_rows <- function(g, hw_set) {
  hw_drop <- function(z) !paste(z$AnimalNum, z$CC, sep = "|") %in% hw_set
  out <- list(
    s33a_e5_model(g, "SIS", "a"), s33a_e5_model(g, "SIS", "a", ws = TRUE),
    s33a_e5_model(g, "SIS", "b"), s33a_e5_model(g, "SIS", "b", ws = TRUE),
    s33a_e5_model(g, "SIS", "c"), s33a_e5_model(g, "SIS", "c", ws = TRUE),
    s33a_e5_model(g, "CON", "d"), s33a_e5_model(g, "CON", "d", ws = TRUE),
    s33a_e5_model(g, "SIS", "a", variant = "drop_CC4", keep = function(z) z$CC != "CC4"),
    s33a_e5_model(g, "SIS", "a", ycol = "gain_per_day", variant = "per_day"),
    s33a_e5_model(g, "SIS", "a", variant = "drop_HW_A1", keep = hw_drop),
    s33a_e5_model(g, "SIS", "a", variant = "drop_J7", keep = function(z) !z$J7),
    s33a_e5_model(g, "SIS", "a", variant = "omit_B1", keep = function(z) z$Batch != "B1"))
  data.table::rbindlist(out, fill = TRUE)
}

#' a08: descriptive gains per cohort x arm x episode, the weight_dev interval (exact link) and raw tp6 - tp2.
s33a_gain_table <- function(g, d) {
  rows <- list()
  for (bb in S33_COHORTS) for (ar in S33A_ARMS) {
    x <- g[Batch == bb & arm == ar]; an <- d[tracked == TRUE & Batch == bb & arm == ar]
    for (cc in S33_CC) { y <- x[CC == cc]
      in_wd <- !(bb == "B6" && cc == "CC1")
      state <- if (cc == "CC4") S33A_DATES[Batch == bb, tp6_relative_to_test] else "none"
      rows[[length(rows) + 1L]] <- s33_row("cohort_mean", "cohort", estimate = mean(y$gain), units = "g", n_animals = nrow(y),
        n_cages = data.table::uniqueN(y$CageEpisodeID), n_cohorts = 1L, Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], arm = ar, episode = cc,
        start_tp = y$start_tp[1L], end_tp = y$end_tp[1L], start_date = paste(sort(unique(format(y$start_date))), collapse = ";"),
        end_date = paste(sort(unique(format(y$end_date))), collapse = ";"), interval_days = paste(sort(unique(y$days)), collapse = ";"),
        test_state_at_end = state, in_weight_dev = in_wd, mean_start_weight_g = mean(y$ws), sd_g = stats::sd(y$gain), median_g = stats::median(y$gain),
        n_negative = sum(y$gain < 0), n_zero = sum(y$gain == 0), n_distinct = data.table::uniqueN(y$gain), mean_x_RU = mean(y$x),
        weight_dev_z_mean = NA_real_, identity_abs_error = NA_real_, note = S33A_LAB$raw)
    }
    wide <- data.table::dcast(x, AnimalNum ~ CC, value.var = "gain")
    cc_wd <- if (bb == "B6") c("CC2", "CC3", "CC4") else S33_CC
    s <- rowSums(as.matrix(wide[, cc_wd, with = FALSE])); a2 <- an[match(wide$AnimalNum, an$AnimalNum)]
    ref <- if (bb == "B6") a2$tp6 - a2$tp3 else a2$tp6 - a2$tp2
    rows[[length(rows) + 1L]] <- s33_row("cohort_mean", "cohort", estimate = mean(s), units = "g", n_animals = length(s), n_cohorts = 1L,
      Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], arm = ar, episode = "weight_dev_interval", start_tp = if (bb == "B6") "tp3" else "tp2",
      end_tp = "tp6", in_weight_dev = TRUE, mean_start_weight_g = mean(if (bb == "B6") a2$tp3 else a2$tp2), sd_g = stats::sd(s),
      median_g = stats::median(s), n_negative = sum(s < 0), n_zero = sum(s == 0), n_distinct = data.table::uniqueN(s),
      weight_dev_z_mean = mean(an$weight_dev, na.rm = TRUE), identity_abs_error = max(abs(s - ref)), note = paste(S33A_LAB$raw, S33A_LAB$wd, sep = "; "))
    r6 <- an$tp6 - an$tp2
    rows[[length(rows) + 1L]] <- s33_row("cohort_mean", "cohort", estimate = mean(r6), units = "g", n_animals = length(r6), n_cohorts = 1L,
      Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], arm = ar, episode = "raw_tp6_minus_tp2", start_tp = "tp2", end_tp = "tp6",
      in_weight_dev = bb != "B6", mean_start_weight_g = mean(an$tp2), sd_g = stats::sd(r6), median_g = stats::median(r6), n_negative = sum(r6 < 0),
      n_zero = sum(r6 == 0), n_distinct = data.table::uniqueN(r6), note = S33A_LAB$raw)
  }
  data.table::rbindlist(rows, fill = TRUE)
}

# ---------------------------------------------------------------- descriptors (a12)
#' ELISA reader dates per cohort from the a4 tables (file level; not re-derived).
s33a_elisa_dates <- function(tech, elisa) {
  data.table::rbindlist(lapply(S33_COHORTS, function(bb) {
    f <- trimws(strsplit(tech[Batch == bb, elisa_files] %a_or% "", ",", fixed = TRUE)[[1L]])
    e <- elisa[file %in% f]; bas <- e[grepl("basal", file, ignore.case = TRUE)]; res <- e[!grepl("basal", file, ignore.case = TRUE)]
    data.table::data.table(Batch = bb, elisa_basal_reader_date = paste(sort(unique(bas$reader_run_date)), collapse = ";"),
                           elisa_response_reader_date = paste(sort(unique(res$reader_run_date)), collapse = ";"),
                           elisa_files_matched = nrow(e)) }))
}

#' Pearson r of each numeric column with xbar over the cohorts (all six and within sex), as two summary rows.
s33a_r_rows <- function(tab, cols) {
  xb <- tab$xbar_SIS_RU; sx <- tab$Sex
  rr <- function(v, within) { ok <- is.finite(v) & is.finite(xb); if (sum(ok) < 3L) return(NA_real_)
    a <- v[ok]; b <- xb[ok]; if (within) { a <- a - stats::ave(a, sx[ok]); b <- b - stats::ave(b, sx[ok]) }
    if (stats::sd(a) == 0 || stats::sd(b) == 0) return(NA_real_); sum((a - mean(a)) * (b - mean(b))) / sqrt(sum((a - mean(a))^2) * sum((b - mean(b))^2)) }
  out <- lapply(c(FALSE, TRUE), function(w) { z <- data.table::as.data.table(lapply(stats::setNames(cols, cols), function(k) rr(tab[[k]], w)))
    z[, `:=`(row_type = if (w) "r_with_xbar_within_sex" else "r_with_xbar_all6", level = "cohort", note = S33A_LAB$r12)] })
  data.table::rbindlist(out, fill = TRUE)
}

#' a12 Phase-2 columns (outcome-free) per cohort, plus the r rows of the numeric descriptors with xbar.
s33a_descriptors_p2 <- function(design, inp2) {
  a <- data.table::as.data.table(design$animals); tech <- inp2$a4_tech; el <- s33a_elisa_dates(tech, inp2$a4_elisa)
  dates <- inp2$a1_dates; sp <- inp2$sucpref
  tab <- data.table::rbindlist(lapply(S33_COHORTS, function(bb) {
    x <- a[Batch == bb]; st <- x[condition == "SIS" & tracked == TRUE]; sa <- x[condition == "SIS"]; cn <- x[condition == "CON"]
    tk <- tech[Batch == bb]; mk <- as.numeric(x$cc1_date - x$mark_date); mk <- mk[is.finite(mk)]
    con_src <- unique(cn$src_cage); cc4 <- dates[Batch == bb & CC == "CC4", a1_date]
    w <- unique(stats::na.omit(sp[AnimalNum %in% x$AnimalNum, SucPref]))
    data.table::data.table(Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], row_type = "cohort", level = "descriptive_record",
      n_SIS_tracked = nrow(st), n_SIS_canonical = nrow(sa), n_CON = nrow(cn),
      age_SIS_mean = mean(st$age_cc1), age_SIS_median = stats::median(st$age_cc1), age_SIS_min = min(st$age_cc1), age_SIS_max = max(st$age_cc1),
      age_CON_mean = mean(cn$age_cc1), tp1_SIS_mean = mean(st$tp1), tp1_CON_mean = mean(cn$tp1), tp2_SIS_mean = mean(st$tp2), tp2_CON_mean = mean(cn$tp2),
      xbar_SIS_RU = mean(st$x_RU), xbar_CON_RU = mean(cn$x_RU), con_cc1_board = paste(unique(cn$cc1_board), collapse = ";"),
      sis_cc1_boards = paste(sort(unique(st$cc1_board)), collapse = ","), cc1_date = S33_CC1_DATES[[bb]],
      cc1_recording_start = format(design$lags[Batch == bb & CC == "CC1", rec_start], "%H:%M:%S"),
      lag_cc1_h = design$lags[Batch == bb & CC == "CC1", lag_h],
      a4_placement_median_range = tk$placement_median_range, a4_placement_to_1830_h = suppressWarnings(as.numeric(tk$pre1830_h)),
      a4_placement_is_lower_bound = toupper(tk$pre1830_is_lower_bound) == "TRUE", rack = paste(unique(x$rack), collapse = ";"),
      terminal_method = paste(unique(x$method), collapse = ";"), elisa_basal_reader_date = el[Batch == bb, elisa_basal_reader_date],
      elisa_response_reader_date = el[Batch == bb, elisa_response_reader_date],
      rfid_mark_to_cc1_days = if (length(mk)) paste(sort(unique(mk)), collapse = ";") else NA_character_,
      rfid_mark_to_cc1_days_median = if (length(mk)) stats::median(mk) else NA_real_,
      sucrose_test_window = if (length(w)) paste(w, collapse = ";") else NA_character_,
      sucrose_test_start_minus_cc4_d = as.numeric(S33A_DATES[Batch == bb, test_start] - cc4),
      n_C57BL6J_tracked_SIS = sum(st$line_J == 1L, na.rm = TRUE), n_source_cages_SIS_tracked = data.table::uniqueN(st$src_cage),
      con_source_cage = paste(con_src, collapse = ";"), con_source_cage_also_SIS_n = sum(sa$src_cage %in% con_src),
      dst_state_at_A1 = tk$DST_state_at_A1,
      source_flags = paste(S33A_LAB$a4, "for placement, ELISA reader dates (file level; template carry-over; no file named for B2)",
                           "and DST state; B1 placement is a lower bound; recording start and lag from the raw files", sep = " "))
  }))
  tab[Batch == "B1", a4_placement_to_1830_h_used_in_r := NA_real_][Batch != "B1", a4_placement_to_1830_h_used_in_r := a4_placement_to_1830_h]
  out <- rbind(tab, s33a_r_rows(tab, S33A_A12_R_COLS_P2), fill = TRUE)
  out[, `:=`(metric_label = NA_character_,
             units = data.table::fifelse(row_type == "cohort", S33A_U$a12_cohort, S33A_U$a12_r), lead = FALSE, interval_basis = "none")]
  out[, tier := S33_TIER][]
}

#' a12 in Phase 3: the Phase-2 product with its rows, order and values unchanged, plus the Phase-3 columns (tp6 means,
#' tp6 timing, n_RES / n_SUS as counts only) on the cohort rows and the r of the tp6 means with xbar on the r rows.
#' `p3c` = one row per cohort (Batch and the Phase-3 columns).
s33a_a12_phase3 <- function(prod, p3c) {
  a12 <- data.table::copy(data.table::as.data.table(prod)); p3c <- data.table::as.data.table(p3c)
  coh <- which(a12$row_type == "cohort"); i <- match(a12$Batch[coh], p3c$Batch)
  if (!length(coh) || anyNA(i)) stop("a12: Phase-3 descriptor rows do not match the cohort rows of the Phase-2 product.", call. = FALSE)
  for (k in setdiff(names(p3c), "Batch")) { v <- p3c[[k]]; col <- v[rep(NA_integer_, nrow(a12))]; col[coh] <- v[i]; data.table::set(a12, j = k, value = col) }
  rr <- s33a_r_rows(a12[coh], S33A_A12_R_COLS_P3)
  for (rt in unique(rr$row_type)) { w <- which(a12$row_type == rt)
    for (k in S33A_A12_R_COLS_P3) data.table::set(a12, i = w, j = k, value = rr[row_type == rt][[k]][1L]) }
  a12[, n_RES_SUS_note := data.table::fifelse(row_type == "cohort", "counts only; no comparison", NA_character_)]
  data.table::setcolorder(a12, c(setdiff(names(a12), "tier"), "tier"))
  a12[]
}
S33A_A12_P3_COLS <- c("tp6_SIS_mean", "tp6_CON_mean", "tp6_minus_cc4_d", "tp6_relative_to_sucrose_test", "n_RES_tracked", "n_SUS_tracked",
                      "n_RES_SUS_note")

#' Re-extraction of the outcome-free product from the final tables (plan section 6): a12 without its Phase-3 columns.
s33a_outcome_free_extract <- function(out) {
  x <- data.table::as.data.table(out$tables$a12_cohort_descriptors)
  list(a12_cohort_descriptors_phase2 = x[, setdiff(names(x), S33A_A12_P3_COLS), with = FALSE])
}

# ---------------------------------------------------------------- registered rows (a10, a11) and reference rows
#' Verbatim copies (character) of the registered Stage 32 rows with their source file and sha256 (no tier column).
s33a_registered_copy <- function(mult, est, sha_mult, sha_est) {
  a10 <- data.table::copy(mult[HypothesisID %in% S33A_REGISTERED$multiplicity])
  a11 <- data.table::copy(est[model_id %in% S33A_REGISTERED$estimates])
  a10[, `:=`(source_file = "multiplicity.csv", source_sha256 = sha_mult, copy_note = S33A_LAB$a10)]
  a11[, `:=`(source_file = "estimates.csv", source_sha256 = sha_est, copy_note = S33A_LAB$a10)]
  list(a10 = a10[], a11 = a11[])
}

#' GA-T8: a verbatim copy equals its source rows cell for cell, re-read independently (utils::read.csv, every cell as
#' text, no NA conversion); the copy's own columns source_file, source_sha256 and copy_note are not compared.
s33a_verbatim_check <- function(copy, path, key) {
  if (is.null(path) || length(path) != 1L || !file.exists(path)) return(FALSE)
  src <- utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE, encoding = "UTF-8")
  src <- src[src[[key]] %in% copy[[key]], , drop = FALSE]; cols <- names(src)
  if (!identical(cols, setdiff(names(copy), c("source_file", "source_sha256", "copy_note"))) || nrow(src) != nrow(copy) || !nrow(src)) return(FALSE)
  all(vapply(cols, function(k) { a <- as.character(copy[[k]]); a[is.na(a)] <- ""; identical(enc2utf8(a), enc2utf8(as.character(src[[k]]))) }, TRUE))
}

#' Plan-quoted reference values (H01, H03, Stage 29 parent, H13) taken from the copied rows (ddf_fallback 1 = the
#' registered row used the Satterthwaite fallback).
s33a_reference_values <- function(a11, s29) {
  num <- function(x) suppressWarnings(as.numeric(x))
  h01 <- a11[model_id == "A_EXP|crossing_rate" & estimand == "SIS_minus_CON"]; h03 <- a11[model_id == "A_CZ|crossing_rate" & estimand == "CombZ_wb_slope"]
  h13 <- a11[model_id == "H13|Movement_mean"]; p29 <- s29[population == "SIS_ONLY" & predictor == "crossing_rate" & estimand == "slope_sexavg"]
  one <- function(x, f) if (nrow(x) == 1L && f %in% names(x)) num(x[[f]]) else NA_real_
  fb <- function(x) if (nrow(x) == 1L && "ddf_fallback" %in% names(x)) as.numeric(toupper(trimws(x$ddf_fallback)) == "TRUE") else NA_real_
  v <- c(H01 = list(c(estimate = one(h01, "estimate"), ci_low = one(h01, "ci_low"), ci_high = one(h01, "ci_high"), df = one(h01, "df"), se = one(h01, "se"),
                      ddf_fallback = fb(h01))),
         H03 = list(c(estimate = one(h03, "estimate"), ci_low = one(h03, "ci_low"), ci_high = one(h03, "ci_high"), df = one(h03, "df"), se = one(h03, "se"),
                      ddf_fallback = fb(h03))),
         H13 = list(c(estimate = one(h13, "estimate"), ci_low = one(h13, "ci_low"), ci_high = one(h13, "ci_high"), df = one(h13, "df"), se = one(h13, "se"))),
         S29_parent = list(c(estimate = one(p29, "estimate"), ci_low = one(p29, "ci_low"), ci_high = one(p29, "ci_high"), df = one(p29, "df"),
                             se = one(p29, "se"), n = one(p29, "n"))))
  sc <- function(x, f) { k <- intersect(c("estimate", "ci_low", "ci_high", "se"), names(x)); x[k] <- x[k] * f; x }
  v$H01_RU <- sc(v$H01, 1 / S33_RU); v$S29_parent_RU <- sc(v$S29_parent, S33_RU)
  v
}

# ---------------------------------------------------------------- readers (runner only; readxl)
#' Phase-2 reads, restricted to declared outcome-free columns: Timeline SucPref, Stage 32 A1 dates, a4 technical and ELISA tables.
s33a_read_phase2 <- function(ctx, canon) {
  p <- ctx$inputs; need <- c("timeline", "s32_window_metrics", "a4_batch_technical", "a4_elisa_dates")
  miss <- need[!vapply(need, function(r) !is.null(p[[r]]) && file.exists(p[[r]]), TRUE)]
  if (length(miss)) stop("Module A Phase 2 lacks input(s): ", paste(miss, collapse = ", "), " (the a4 folder is required for module A)", call. = FALSE)
  tl <- s33_read_sheet_cols(p[["timeline"]], "Animals", c("ID", "SucPref"))[!is.na(ID)]
  tl[, AnimalNum := s33_planning_id(ID, canon)]
  wm <- data.table::fread(p[["s32_window_metrics"]], select = c("Batch", "CC", "phase", "start"), colClasses = "character", showProgress = FALSE)
  wm <- unique(wm[phase == "A1", .(Batch = ifelse(grepl("^B", Batch), Batch, paste0("B", Batch)), CC, a1_date = as.Date(substr(start, 1L, 10L)))])
  tech <- data.table::fread(p[["a4_batch_technical"]], select = S33A_A4_TECH_COLS, colClasses = "character", showProgress = FALSE)
  el <- data.table::fread(p[["a4_elisa_dates"]], select = c("file", "reader_run_date"), colClasses = "character", showProgress = FALSE)
  out <- list(sucpref = tl[, .(AnimalNum, SucPref)], a1_dates = wm, a4_tech = tech, a4_elisa = el)
  for (k in names(out)) s33_assert_outcome_free(out[[k]], paste("module A Phase-2 read", k))
  out
}

#' Phase-3 reads: Movement_mean (bundle A2), registered Stage 32 rows and the Stage 29 parent (character, verbatim), a4 means and
#' reference SD, master_wide raw measures and component copies, cycle phase.
s33a_read_phase3 <- function(ctx, canon) {
  p <- ctx$inputs
  sha <- function(role) { v <- ctx$input_sha256
    if (!is.null(v) && !is.null(names(v)) && role %in% names(v)) as.character(v[[role]]) else digest::digest(file = p[[role]], algo = "sha256") }
  mw_cols <- c("animal_id", stats::na.omit(S33A_RAW$raw_col), unname(S33A_MW_COPIES))
  mw <- s33_read_sheet_cols(p[["workbook"]], "master_wide", unique(mw_cols))
  data.table::setnames(mw, unname(S33A_MW_COPIES), paste0("mw_", unname(S33A_MW_COPIES)))
  mw[, AnimalNum := canon(animal_id)][, animal_id := NULL]
  cy <- s33_read_sheet_cols(p[["workbook"]], "corticosterone", c("animal_id", "cycle_phase"))
  cy[, AnimalNum := canon(animal_id)][, animal_id := NULL]
  list(movement = data.table::fread(p[["bundle_a2"]], select = c("AnimalNum", "Movement_mean"), colClasses = list(character = "AnimalNum"), showProgress = FALSE),
       # verbatim: every cell as text, no NA conversion (a10/a11 are copies, GA-T8)
       s32_multiplicity = data.table::fread(p[["s32_multiplicity"]], colClasses = "character", na.strings = NULL, encoding = "UTF-8", showProgress = FALSE),
       s32_estimates = data.table::fread(p[["s32_estimates"]], colClasses = "character", na.strings = NULL, encoding = "UTF-8", showProgress = FALSE),
       s29 = data.table::fread(p[["s29_continuous_estimates"]], colClasses = "character", na.strings = NULL, encoding = "UTF-8", showProgress = FALSE),
       a4_wide = data.table::fread(p[["a4_components_wide"]], showProgress = FALSE), a4_yard = data.table::fread(p[["a4_cage_noise"]], showProgress = FALSE),
       mw = mw, cycle = cy, paths = list(s32_multiplicity = p[["s32_multiplicity"]], s32_estimates = p[["s32_estimates"]]),
       sha = list(s32_multiplicity = sha("s32_multiplicity"), s32_estimates = sha("s32_estimates")))
}

# ---------------------------------------------------------------- Phase-2 gates (outcome-free)
s33a_checks_p2 <- function(design, inp2) {
  a <- data.table::as.data.table(design$animals); ep <- data.table::as.data.table(design$episodes); cg <- data.table::as.data.table(design$cages)
  tr <- a[tracked == TRUE]; ck <- list(); add <- function(...) ck[[length(ck) + 1L]] <<- s33a_chk(...)
  # GA-4a counts
  add("GA-4a", "canonical 117 animals, unique", c(nrow(a) == 117L, !anyDuplicated(a$AnimalNum)), 117L, nrow(a))
  add("GA-4a", "tracked 111 animals, unique", c(nrow(tr) == 111L, !anyDuplicated(tr$AnimalNum)), 111L, nrow(tr))
  add("GA-4a", "444 animal-episodes = 111 x CC1-CC4", c(nrow(ep) == 444L, ep[, .N, by = AnimalNum]$N == 4L,
       setequal(ep$AnimalNum, tr$AnimalNum)), 444L, nrow(ep))
  add("GA-4a", "4 CON per cohort", a[condition == "CON", .N, keyby = Batch]$N == 4L, rep(4L, 6), a[condition == "CON", .N, keyby = Batch]$N)
  ns <- tr[condition == "SIS", .N, keyby = Batch]
  add("GA-4a", "tracked SIS per cohort 10/16/16/15/15/15", identical(stats::setNames(ns$N, ns$Batch)[S33_COHORTS], S33A_SIS_TRACKED),
      S33A_SIS_TRACKED, ns$N)
  add("GA-4a", "age at CC1, tp1 and tp2 present for 117", c(is.finite(a$age_cc1), is.finite(a$tp1), is.finite(a$tp2)), 351L,
      sum(is.finite(a$age_cc1)) + sum(is.finite(a$tp1)) + sum(is.finite(a$tp2)))
  # GA-5 cages
  c1 <- cg[CC == "CC1"]
  sz <- c1[condition == "SIS", .(s = paste(sort(n_tracked), collapse = ",")), keyby = Batch]
  add("GA-5", "CC1 SIS cages: 24 with tracked sizes B1 1,1,4,4; B2, B3 4x4; B4-B6 3,4,4,4",
      c(sum(c1$condition == "SIS") == 24L, identical(stats::setNames(sz$s, sz$Batch)[S33_COHORTS], S33A_CC1_SIZES)), S33A_CC1_SIZES, sz$s)
  cb <- c1[condition == "CON"][order(Batch)]
  add("GA-5", "CC1 CON cages: 6 of 4 on sys.3 except sys.4 (B2) and sys.2 (B6)",
      c(nrow(cb) == 6L, cb$n_tracked == 4L, identical(stats::setNames(cb$board, cb$Batch)[S33_COHORTS], S33A_CON_BOARDS)), S33A_CON_BOARDS, cb$board)
  add("GA-5", "no cage mixes CON and SIS", !grepl("/", cg$condition), 0L, sum(grepl("/", cg$condition)))
  e1 <- ep[CC == "CC1"]; nt <- e1[, .N, by = CageEpisodeID]; e1 <- merge(e1, nt, by = "CageEpisodeID")
  add("GA-5", "n_in_cage = tracked count at CC1 (111)", c(nrow(e1) == 111L, e1$n_in_cage == e1$N), 111L, sum(e1$n_in_cage == e1$N))
  cm <- ep[condition == "CON", .(m = paste(sort(AnimalNum), collapse = ";")), by = .(Batch, CC)][, data.table::uniqueN(m), by = Batch]$V1
  add("GA-5", "CON mates constant over CC1-CC4", c(length(cm) == 6L, cm == 1L), 1L, cm)
  e4 <- ep[CC == "CC4"]; j4 <- merge(tr[, .(AnimalNum, cc4_cage)], e4[, .(AnimalNum, CageEpisodeID)], by = "AnimalNum")
  add("GA-5", "cc4_cage (master_wide) = CC4 CageEpisodeID for the 111 tracked", c(nrow(j4) == 111L, j4$cc4_cage == j4$CageEpisodeID), 111L,
      sum(j4$cc4_cage == j4$CageEpisodeID))
  s4 <- a[condition == "SIS", .N, by = .(Batch, cc4_cage)][, .(s = paste(sort(N), collapse = ","), k = .N), keyby = Batch]
  add("GA-5", "SIS CC4 cages (all 117): 4 per cohort, sizes B1-B3 4x4, B4-B6 3,4,4,4",
      c(s4$k == 4L, identical(stats::setNames(s4$s, s4$Batch)[S33_COHORTS], S33A_CC4_SIZES_ALL)), S33A_CC4_SIZES_ALL, s4$s)
  t4 <- tr[Batch == "B1" & condition == "SIS", .N, by = cc4_cage][, paste(sort(N), collapse = ",")]
  add("GA-5", "B1 tracked SIS CC4 cage sizes 2,2,3,3", t4 == "2,2,3,3", "2,2,3,3", t4)
  c4 <- a[condition == "CON", .(k = data.table::uniqueN(cc4_cage), n = .N), keyby = Batch]
  add("GA-5", "CON: one CC4 cage of 4 per cohort", c(c4$k == 1L, c4$n == 4L), 1L, c4$k)
  ut <- a[AnimalNum %in% names(S33A_UNTRACKED_CC4)]
  add("GA-5", "planned CC4 cages of the six untracked B1 SIS", c(nrow(ut) == 6L, ut$cc4_cage == S33A_UNTRACKED_CC4[ut$AnimalNum]),
      S33A_UNTRACKED_CC4, ut$cc4_cage)
  add("GA-5", "planned CC1 cages of the six untracked B1 SIS (with OQ770 on sys.2, OQ771 on sys.5)",
      c(nrow(ut) == 6L, ut$cc1_cage == S33A_UNTRACKED_CC1[ut$AnimalNum], a[AnimalNum == "OQ770", cc1_cage] == "B1|sys.2|CC1",
        a[AnimalNum == "OQ771", cc1_cage] == "B1|sys.5|CC1"), S33A_UNTRACKED_CC1, ut$cc1_cage)
  s1 <- a[Batch == "B1" & condition == "SIS", .N, by = cc1_cage][, paste(sort(N), collapse = ",")]
  add("GA-5", "canonical B1 SIS CC1 cages (planned) 4,4,4,4", s1 == "4,4,4,4", "4,4,4,4", s1)
  # GA-9a dates (outcome-free)
  dts <- inp2$a1_dates
  d1 <- dts[CC == "CC1"]; m2 <- d1$a1_date[match(a$Batch, d1$Batch)]
  add("GA-9a", "tp2 date = Stage 32 CC1 A1 date (117)", a$tp2_date == m2, "CC1 A1 date", sum(a$tp2_date == m2, na.rm = TRUE))
  lag1 <- as.numeric(m2 - a$tp1_date)
  add("GA-9a", "tp1 2-5 d before CC1 (117)", lag1 >= 2 & lag1 <= 5, "2-5", range(lag1))
  sp <- merge(inp2$sucpref, a[, .(AnimalNum, Batch)], by = "AnimalNum")[!is.na(SucPref)]
  sw <- sp[, .(w = paste(unique(SucPref), collapse = " || ")), keyby = Batch]
  add("GA-9a", "Timeline SucPref windows = S33A_DATES (one per cohort)",
      identical(stats::setNames(sw$w, sw$Batch)[S33_COHORTS], stats::setNames(S33A_DATES$sucpref_text, S33A_DATES$Batch)), S33A_DATES$sucpref_text, sw$w)
  c4d <- dts[CC == "CC4"]; st <- S33A_DATES$test_start - c4d$a1_date[match(S33A_DATES$Batch, c4d$Batch)]
  add("GA-9a", "sucrose test start = CC4 A1 date + 2 d", as.numeric(st) == 2, 2L, as.numeric(st))
  add("GA-9a", "Stage 32 A1 dates: 24 cohort x CC rows", nrow(dts) == 24L && !anyDuplicated(dts[, .(Batch, CC)]), 24L, nrow(dts))
  # GA-13 descriptors = a4 fields
  tech <- inp2$a4_tech[match(S33_COHORTS, Batch)]
  meth <- a[, .(v = paste(unique(method), collapse = ";")), keyby = Batch]
  add("GA-13", "terminal method = a4 terminal_method", meth$v[match(S33_COHORTS, meth$Batch)] == tech$terminal_method, tech$terminal_method, meth$v)
  rk <- a[, .(v = paste(unique(rack), collapse = ";")), keyby = Batch]
  add("GA-13", "rack = a4 rack", rk$v[match(S33_COHORTS, rk$Batch)] == tech$rack, tech$rack, rk$v)
  mk_ok <- vapply(S33_COHORTS, function(bb) { v <- a[Batch == bb, as.numeric(cc1_date - mark_date)]; v <- sort(unique(v[is.finite(v)]))
    t <- tech[Batch == bb, rfid_mark_to_cc1_days]; tv <- if (is.na(t) || !nzchar(t)) numeric() else sort(as.numeric(strsplit(t, ";", fixed = TRUE)[[1L]]))
    identical(as.numeric(v), as.numeric(tv)) }, TRUE)
  add("GA-13", "RFID marking to CC1 (days) = a4 rfid_mark_to_cc1_days", mk_ok, tech$rfid_mark_to_cc1_days, mk_ok)
  lc_ok <- vapply(S33_COHORTS, function(bb) { v <- tr[Batch == bb, .N, by = line]; t <- tech[Batch == bb, line_counts]
    p <- strsplit(trimws(strsplit(t, ";", fixed = TRUE)[[1L]]), ":", fixed = TRUE)
    tv <- stats::setNames(as.integer(vapply(p, `[`, "", 2L)), vapply(p, `[`, "", 1L))
    setequal(names(tv), v$line) && all(tv[v$line] == v$N) }, TRUE)
  add("GA-13", "tracked line counts = a4 line_counts", lc_ok, tech$line_counts, lc_ok)
  cb2 <- a[condition == "CON" & tracked == TRUE, .(v = paste(unique(cc1_board), collapse = ";")), keyby = Batch]
  add("GA-13", "CON CC1 board = a4 CON_system", cb2$v[match(S33_COHORTS, cb2$Batch)] == tech$CON_system, tech$CON_system, cb2$v)
  rs <- format(design$lags[CC == "CC1"][match(S33_COHORTS, Batch), rec_start], "%H:%M:%S")
  add("GA-13", "CC1 recording start (raw file) = a4 raw_recording_start", rs == tech$raw_recording_start, tech$raw_recording_start, rs)
  data.table::rbindlist(ck)
}

# ---------------------------------------------------------------- Phase 2
#' Outcome-free part of module A: Phase-2 reads (or `inputs2`), GA-4a, GA-5, GA-9a, GA-13, the a12 Phase-2 product and the
#' resampling units and index matrices of the six streams (seeds and B from ctx).
s33a_phase2 <- function(design, ctx, inputs2 = NULL) {
  canon <- s33a_canon(ctx)
  s33_assert_outcome_free(design$animals, "module A Phase-2 design")
  inp <- if (is.null(inputs2)) s33a_read_phase2(ctx, canon) else inputs2
  for (k in names(inp)) s33_assert_outcome_free(inp[[k]], paste("module A Phase-2 input", k))
  ck <- s33a_checks_p2(design, inp)
  d12 <- s33a_descriptors_p2(design, inp)
  s33_assert_outcome_free(d12, "a12 Phase-2 product")
  B <- as.integer(ctx$B[["nonparametric"]])
  units <- list(); idx <- list(); streams <- list()
  for (pop in S33A_POPULATIONS) {
    units[[pop]] <- s33a_units(design$animals, pop)
    for (sch in S33A_SCHEMES) {
      seed_name <- S33A_SEED_NAMES[[pop]][[sch]]; seed <- as.integer(ctx$seeds[[seed_name]]); G <- units[[pop]][[sch]]$G
      ix <- s33_resample_index(G, B, seed); idx[[pop]][[sch]] <- ix
      streams[[length(streams) + 1L]] <- data.table::data.table(module = "A", population = pop, scheme = sch, interval_method = S33A_SCHEME_METHOD[[sch]],
        seed_name = seed_name, seed = seed, B = B, rng_kinds = "Mersenne-Twister/Inversion/Rejection",
        unit_order = paste(paste0(names(G), ":", G), collapse = ";"), index_sha256 = digest::digest(ix, algo = "sha256"))
    }
  }
  list(products = list(a12_cohort_descriptors_phase2 = d12), gates = s33a_gates(ck, c("GA-4a", "GA-5", "GA-9a", "GA-13")),
       state = list(inputs2 = inp, checks = ck, units = units, idx = idx, streams = data.table::rbindlist(streams), B = B))
}

# ---------------------------------------------------------------- Phase-3 gates
s33a_checks_p3 <- function(d, design_out, inp2, inp3, maps) {
  ck <- list(); add <- function(...) ck[[length(ck) + 1L]] <<- s33a_chk(...)
  tr <- d[tracked == TRUE]; ep <- data.table::as.data.table(design_out$episodes)
  # GA-4b outcome-group counts
  g1 <- as.integer(table(factor(tr$outcome_group, c("CON", "RES", "SUS")))); g2 <- as.integer(table(factor(d$outcome_group, c("CON", "RES", "SUS"))))
  add("GA-4b", "tracked CON/RES/SUS 24/53/34", identical(g1, c(24L, 53L, 34L)), "24/53/34", g1)
  add("GA-4b", "canonical CON/RES/SUS 24/58/35", identical(g2, c(24L, 58L, 35L)), "24/58/35", g2)
  add("GA-4b", "condition CON = outcome_group CON", (d$condition == "CON") == (d$outcome_group == "CON"), 117L, sum((d$condition == "CON") == (d$outcome_group == "CON")))
  # GA-6 identities
  cs <- rowSums(as.matrix(d[, paste0("c_", S33_COMPONENTS), with = FALSE]))
  add("GA-6", "CombZ = sum of c_k (1e-12, 117)", c(nrow(d) == 117L, abs(d$CombZ - cs) <= 1e-12), "<= 1e-12", max(abs(d$CombZ - cs)), 1e-12)
  add("GA-6", "n_present = n_components_present (117)", d$n_present == d$n_components_present, 117L, sum(d$n_present == d$n_components_present))
  dd <- d[is.finite(delta_cort)]; s3 <- dd$dcort_response_part + dd$dcort_baseline_part + dd$dcort_residual_part
  add("GA-6", "delta_cort = response + baseline + residual parts (1e-12)", abs(dd$delta_cort - s3) <= 1e-12, "<= 1e-12", max(abs(dd$delta_cort - s3)), 1e-12)
  cp <- d$c_dcort_response_part + d$c_dcort_baseline_part + d$c_dcort_residual_part
  add("GA-6", "c_delta_cort = sum of the c-scale parts (1e-12)", abs(d$c_delta_cort - cp) <= 1e-12, "<= 1e-12", max(abs(d$c_delta_cort - cp)), 1e-12)
  e1 <- ep[CC == "CC1"]; r1 <- e1$crossing_rate[match(tr$AnimalNum, e1$AnimalNum)]
  add("GA-6", "A1 rate = bundle B1 CC1 rate (1e-9, 111) and x_RU = rate / 6", c(length(r1) == 111L, abs(tr$crossing_rate - r1) <= 1e-9,
      abs(tr$x_RU - tr$crossing_rate / S33_RU) <= 1e-12), "<= 1e-9", max(abs(tr$crossing_rate - r1)), 1e-9)
  # GA-7 missingness
  na_ids <- function(v) sort(d[!is.finite(get(v)), AnimalNum])
  add("GA-7", "delta_cort missing only for OQ762", identical(na_ids("delta_cort"), "OQ762"), "OQ762", na_ids("delta_cort"))
  add("GA-7", "adrenal_weight missing only for OR126", identical(na_ids("adrenal_weight"), "OR126"), "OR126", na_ids("adrenal_weight"))
  add("GA-7", "cort_response missing only for OQ762", identical(na_ids("cort_response"), "OQ762"), "OQ762", na_ids("cort_response"))
  add("GA-7", "NOR, sucrose_pref, weight_dev, spleen_weight, CombZ, tp1-tp6, cort_baseline complete (117)",
      vapply(c("NOR", "sucrose_pref", "weight_dev", "spleen_weight", "CombZ", paste0("tp", 1:6), "cort_baseline"), function(v) all(is.finite(d[[v]])), TRUE), 0L,
      sum(!is.finite(as.matrix(d[, c("NOR", "sucrose_pref", "weight_dev", "spleen_weight", "CombZ", paste0("tp", 1:6), "cort_baseline"), with = FALSE]))))
  add("GA-7", "Mark date missing only in B6 (all of B6)", c(all(is.na(d[Batch == "B6", mark_date])), !anyNA(d[Batch != "B6", mark_date])), "B6 only",
      d[is.na(mark_date), paste(unique(Batch), collapse = ",")])
  add("GA-7", "dob, cc4_cage, method, rack complete (117); x present for the 111 tracked", c(!anyNA(d$dob), !anyNA(d$cc4_cage), !anyNA(d$method),
      !anyNA(d$rack), all(is.finite(tr$x_RU))), 0L, sum(is.na(d$dob)) + sum(is.na(d$cc4_cage)))
  # GA-8 pooled same-sex CON reference
  for (s in c("Female", "Male")) for (m in S33_COMPONENTS) {
    y <- d[arm == "CON" & Sex == s][[m]]; y <- y[is.finite(y)]; mu <- mean(y); psd <- sqrt(mean((y - mu)^2))
    exp_sd <- if (s == "Female" && m == "delta_cort") 0.8826 else 1; tol <- if (s == "Female" && m == "delta_cort") 1e-4 else 1e-9
    exp_n <- if (s == "Male" && m == "adrenal_weight") 11L else 12L
    add("GA-8", sprintf("%s %s: CON mean 0, population SD %g, n %d", s, m, exp_sd, exp_n), c(abs(mu) <= 1e-9, abs(psd - exp_sd) <= tol, length(y) == exp_n),
        c(0, exp_sd, exp_n), c(mu, psd, length(y)), tol)
  }
  # GA-9b dates
  dts <- inp2$a1_dates
  for (k in 3:5) { cc <- paste0("CC", k - 1L); dk <- dts[CC == cc]; mk <- dk$a1_date[match(d$Batch, dk$Batch)]
    add("GA-9b", sprintf("tp%d date = Stage 32 %s A1 date (117)", k, cc), d[[paste0("tp", k, "_date")]] == mk, cc, sum(d[[paste0("tp", k, "_date")]] == mk, na.rm = TRUE)) }
  off <- as.numeric(d$tp6_date - d$tp5_date); exp_off <- S33A_DATES$tp6_minus_cc4_d[match(d$Batch, S33A_DATES$Batch)]
  add("GA-9b", "tp6 - tp5 = 5 d (B1-B5), 8 d (B6)", off == exp_off, "5 / 8", unique(off))
  ts <- S33A_DATES[match(d$Batch, Batch)]
  rel <- ifelse(d$Batch == "B6", as.numeric(d$tp6_date - ts$test_end) == 1, as.numeric(d$tp6_date - ts$test_start) == 3)
  add("GA-9b", "tp6 = sucrose test day 4 (B1-B5), test end + 1 d (B6)", rel, TRUE, sum(rel, na.rm = TRUE))
  data.table::rbindlist(c(ck, list(maps$checks)))
}

# ---------------------------------------------------------------- Phase 3
#' Phase 3 of module A (plan section 8). `inputs3` replaces the Phase-3 reads (tests and development only).
s33a_phase3 <- function(design_out, p2, ctx, inputs3 = NULL) {
  canon <- s33a_canon(ctx)
  inp <- if (is.null(inputs3)) s33a_read_phase3(ctx, canon) else inputs3
  st2 <- p2$state; B <- st2$B
  if (!identical(as.integer(ctx$B[["nonparametric"]]), B)) stop("Module A: ctx$B differs from the Phase-2 B.", call. = FALSE)
  a <- data.table::as.data.table(design_out$animals)
  mw <- data.table::as.data.table(inp$mw)[AnimalNum %in% a$AnimalNum]
  maps <- s33a_maps(a, mw)
  d <- s33a_frame(a, maps$b_dcort, inp$movement)
  ck3 <- s33a_checks_p3(d, design_out, st2$inputs2, inp, maps)
  obs <- numeric(); cont <- numeric()
  # ---- point means and resampling streams
  log_env <- if (is.environment(ctx$checkpoint_log)) ctx$checkpoint_log else new.env()
  ctx$checkpoint_log <- log_env; before <- ls(log_env)
  frames <- list(tracked_111 = d[tracked == TRUE], canonical_117 = d)
  pt <- list(); Ap <- list(); envs <- list(); bsum <- list()
  for (pop in S33A_POPULATIONS) {
    sums_obs <- s33a_cell_sums(st2$units[[pop]]$animal, frames[[pop]], S33A_ARRAY_VARS)
    Ap[[pop]] <- s33a_means_array(sums_obs); pv <- s33a_stats(Ap[[pop]], point = TRUE); pt[[pop]] <- stats::setNames(pv[1L, ], colnames(pv))
    V_obs <- c(all_six = pt[[pop]][[s33a_id("E2", "all_six", "V", "-", "-", "CombZ_total")]],
               within_sex = pt[[pop]][[s33a_id("E2", "within_sex", "V", "-", "-", "CombZ_total")]])
    res <- list()
    for (sch in S33A_SCHEMES) {
      sums <- s33a_cell_sums(st2$units[[pop]][[sch]], frames[[pop]], S33A_ARRAY_VARS); G <- st2$units[[pop]][[sch]]$G
      ix <- st2$idx[[pop]][[sch]]; seed <- as.integer(ctx$seeds[[S33A_SEED_NAMES[[pop]][[sch]]]])
      r <- s33_checkpoint(ctx, "A", paste0("boot_", pop, "_", sch), function() s33a_boot_stream(sums, ix, G, V_obs),
                          key_extra = list(frame_sha256 = digest::digest(sums, algo = "sha256"), seed = seed, B = B, cells = G,
                                           index_sha256 = digest::digest(ix, algo = "sha256")))
      res[[sch]] <- data.table::copy(r)[, `:=`(population = pop, scheme = sch)]
    }
    bsum[[pop]] <- data.table::rbindlist(res); envs[[pop]] <- s33a_envelope(bsum[[pop]])
  }
  new_cp <- setdiff(ls(log_env), before)
  cps <- if (length(new_cp)) data.table::rbindlist(mget(sort(new_cp), envir = log_env))[module == "A"] else data.table::data.table()
  # Movement_mean point array (S7, GA-11)
  mmf <- data.table::copy(frames$tracked_111)[, x_RU := Movement_mean]
  Ap_mm <- s33a_means_array(s33a_cell_sums(st2$units$tracked_111$animal, mmf, S33A_ARRAY_VARS))
  pt_mm <- { v <- s33a_stats(Ap_mm, point = TRUE); stats::setNames(v[1L, ], colnames(v)) }
  # ---- reference SDs, KR cohort means
  refsd <- data.table::rbindlist(lapply(c(S33A_E1_MEASURES, "dcort_residual_part", S33A_CORT, S33A_LCORT), function(m) s33a_reference_sds(d, m)))
  sis_t <- d[tracked == TRUE & arm == "SIS"]; con_t <- d[tracked == TRUE & arm == "CON"]
  kr_means <- data.table::rbindlist(lapply(c(S33_COMPONENTS, "dcort_response_part", "dcort_baseline_part", "CombZ", "x_RU"), function(m) {
    dat <- sis_t[is.finite(get(m)), .(AnimalNum, Batch = factor(Batch, levels = S33_COHORTS), cc1_cage, cc4_cage, CageEpisodeID = cc1_cage, y = get(m))]
    fit <- s33_kr_fit("y ~ 0 + Batch + (1 | cc1_cage) + (1 | cc4_cage)", dat, s33a_id("A", "KR_cohort_mean", m), length(S33_COHORTS))
    data.table::rbindlist(lapply(S33_COHORTS, function(bb) { k <- s33_kr_contrast(fit, stats::setNames(list(1), paste0("Batch", bb)), paste0("cohort_mean_", bb))
      data.table::data.table(Batch = bb, measure = m, kr_estimate = k$estimate, kr_se = k$se, kr_df = k$df, kr_ci_low = k$ci_low, kr_ci_high = k$ci_high,
                             kr_interval_method = k$interval_method, kr_status = k$status, kr_singular = k$singular) })) }))
  # ---- a01 cohort means
  tabs <- list()
  a01 <- list()
  for (pop in S33A_POPULATIONS) {
    cohorts <- if (pop == "tracked_111") S33_COHORTS else "B1"; meas <- if (pop == "tracked_111") S33A_E1_MEASURES else setdiff(S33A_E1_MEASURES, "x_RU")
    f <- frames[[pop]]
    for (bb in cohorts) for (m in meas) {
      ids <- c(s33a_id("E1", "-", "cohort_mean", bb, "SIS", m), s33a_id("E1", "-", "cohort_mean", bb, "CON", m),
               s33a_id("E1", "-", "sis_minus_con_diff", bb, "SIS_minus_CON", m))
      a01[[length(a01) + 1L]] <- s33a_a01_cell(pop, bb, m, f[Batch == bb & arm == "SIS"], f[Batch == bb & arm == "CON"], maps$slopes,
        est = unname(pt[[pop]][ids]), env = envs[[pop]], ids = ids, kr = if (pop == "tracked_111") kr_means[Batch == bb & measure == m] else NULL)
    }
  }
  # S8: without 692 at CC1 (HW_A1; B6) and, separately, without the 7 tracked C57BL/6J SIS (J7; B1, B3, B5): SIS means with
  # CR2 by CC1 cage and SIS-minus-CON with the animal Welch interval (no resampling stream is declared for S8)
  for (s8 in names(S33A_S8)) {
    ft <- frames$tracked_111; keep <- S33A_S8[[s8]](ft); f <- ft[keep]
    for (bb in sort(unique(ft[!keep, Batch]))) for (m in S33A_E1_MEASURES)
      a01[[length(a01) + 1L]] <- s33a_a01_cell(s8, bb, m, f[Batch == bb & arm == "SIS"], f[Batch == bb & arm == "CON"], maps$slopes, con = FALSE)
  }
  refv <- s33a_reference_values(inp$s32_estimates, inp$s29)
  for (ru in c(FALSE, TRUE)) { v <- if (ru) refv$H01_RU else refv$H01
    a01[[length(a01) + 1L]] <- s33_row("sis_minus_con_diff", "cohort", estimate = v[["estimate"]], se = v[["se"]], df = v[["df"]], ci_low = v[["ci_low"]],
      ci_high = v[["ci_high"]], interval_method = s33a_kr_method(v[["ddf_fallback"]]), interval_basis = "conditional_on_cohorts", units = if (ru) S33A_U$ru else S33A_U$rate,
      metric_id = "crossing_rate", population = "registered_reference", measure = "x_RU", arm = "SIS_minus_CON", reference = if (ru) "H01 (RU = rate / 6)" else "H01",
      note = S33A_LAB$h01) }
  tabs$a01_cohort_component_means <- s33a_finish(data.table::rbindlist(a01, fill = TRUE), c("population", "Batch", "Sex", "measure", "arm", "reference"))
  # ---- a02 offset split
  a02 <- list()
  for (pop in S33A_POPULATIONS) {
    cohorts <- if (pop == "tracked_111") S33_COHORTS else S33A_MALE; meas <- if (pop == "tracked_111") S33A_E1_MEASURES else setdiff(S33A_E1_MEASURES, "x_RU")
    for (bb in cohorts) for (m in meas) {
      ids <- c(offset_total = s33a_id("E1b", "-", "offset_total", bb, "SIS", m), part_shared_con = s33a_id("E1b", "-", "part_shared_con", bb, "CON", m),
               part_not_shared = s33a_id("E1b", "-", "part_not_shared", bb, "SIS_minus_CON", m))
      ie <- abs(pt[[pop]][[ids[[1]]]] - pt[[pop]][[ids[[2]]]] - pt[[pop]][[ids[[3]]]])
      rs <- refsd[measure == m]
      for (e in names(ids)) {
        bc <- if (m == "x_RU") s33a_boot_cols(envs[[pop]], NA_character_) else s33a_boot_cols(envs[[pop]], ids[[e]])
        a02[[length(a02) + 1L]] <- cbind(s33_row(e, "cohort", estimate = pt[[pop]][[ids[[e]]]], ci_low = bc$envelope_low, ci_high = bc$envelope_high,
          interval_method = if (m == "x_RU") "none" else "envelope_bootstrap", interval_basis = if (m == "x_RU") NA_character_ else "conditional_on_cohorts",
          units = s33a_unit_mean(m), metric_id = s33a_metric(m), n_cohorts = 3L, population = pop, Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], measure = m,
          arm = c(offset_total = "SIS", part_shared_con = "CON", part_not_shared = "SIS_minus_CON")[[e]], offset_reference = S33A_LAB$offset_ref,
          identity_abs_error = ie, ci_flag = if (e != "offset_total") S33A_LAB$con_flag else NA_character_,
          note = paste(c(if (m == "x_RU") S33A_LAB$rate_point, S33A_LAB$refsd, if (e != "offset_total") S33A_LAB$con_phrase), collapse = "; ")),
          bc, rs[, .(cage_sd_reference_cc1, cage_sd_reference_cc4, cage_sd_reference_cc4_all117, half_within_sd_sis, half_within_sd_con, con_cage_mean_sd_within_sex)])
      }
    }
  }
  tabs$a02_cohort_offset_split <- s33a_finish(data.table::rbindlist(a02, fill = TRUE), c("population", "Batch", "Sex", "measure", "estimand", "arm"))
  # ---- a03 shares and a04 re-pairings
  rp <- s33a_repairings(Ap$tracked_111)
  rps <- rp[, .(observed = share_shared[identity], repair_min = min(share_shared), repair_median = stats::median(share_shared), repair_max = max(share_shared),
                n_pairings = .N), by = .(version, measure)]
  vers <- list(all_six = list(Ap$tracked_111, S33_COHORTS, FALSE, "x_RU"), within_sex = list(Ap$tracked_111, S33_COHORTS, TRUE, "x_RU"))
  for (b in S33_COHORTS) vers[[paste0("omit_", b)]] <- list(Ap$tracked_111, setdiff(S33_COHORTS, b), FALSE, "x_RU")
  for (o in names(S33A_OMIT_SETS)) vers[[o]] <- list(Ap$tracked_111, setdiff(S33_COHORTS, S33A_OMIT_SETS[[o]]), FALSE, "x_RU")
  vers$canonical_117 <- list(Ap$canonical_117, S33_COHORTS, FALSE, "x_RU"); vers$Movement_mean <- list(Ap_mm, S33_COHORTS, FALSE, "x_RU")
  sh <- lapply(vers, function(v) s33a_share_rows(v[[1]], v[[2]], v[[3]], v[[4]]))
  bsx <- data.table::copy(sh$all_six); w <- sh$within_sex
  bsx[, `:=`(scp = scp - w$scp, V = V - w$V, scp_xx = scp_xx - w$scp_xx, scp_z = scp_z - w$scp_z, n_cohorts = 6L)]
  sh <- c(sh[1:2], list(between_sex = bsx), sh[-(1:2)])
  V_all <- sh$all_six$V[1L]
  a03 <- data.table::rbindlist(lapply(names(sh), function(vn) {
    x <- data.table::copy(sh[[vn]]); x[, version := vn]
    pop_b <- if (vn == "canonical_117") "canonical_117" else if (vn %in% c("all_six", "within_sex")) "tracked_111" else NA_character_
    boot_ver <- if (vn == "within_sex") "within_sex" else "all_six"
    rows <- lapply(seq_len(nrow(x)), function(i) { r <- x[i]
      id <- s33a_id("E2", boot_ver, r$estimand, "-", r$arm, r$measure); ids <- s33a_id("E2", boot_ver, sub("share", "slope_contrib", r$estimand, fixed = TRUE), "-", r$arm, r$measure)
      bc <- if (is.na(pop_b)) s33a_boot_cols(envs$tracked_111, NA_character_) else s33a_boot_cols(envs[[pop_b]], id)
      bs <- if (is.na(pop_b)) s33a_boot_cols(envs$tracked_111, NA_character_) else s33a_boot_cols(envs[[pop_b]], ids)
      sv <- if (is.na(pop_b)) NULL else bsum[[pop_b]][stat_id == s33a_id("E2", boot_ver, "V", "-", "-", "CombZ_total")]
      rr <- if (vn %in% c("all_six", "within_sex") && r$estimand == "share_shared") rps[version == vn & measure == r$measure] else NULL
      cbind(s33_row(r$estimand, "cohort", estimate = 100 * r$scp / r$V, ci_low = bc$envelope_low, ci_high = bc$envelope_high,
        interval_method = if (is.na(pop_b)) "none" else "envelope_bootstrap", interval_basis = if (is.na(pop_b)) NA_character_ else "conditional_on_cohorts",
        units = S33A_U$share, metric_id = if (vn == "Movement_mean") "Movement_mean" else "crossing_rate",
        lead = vn == "all_six" && r$measure %in% S33_COMPONENTS, n_cohorts = r$n_cohorts, version = vn, measure = r$measure, arm = r$arm,
        scp = r$scp, V = r$V, V_over_5 = r$V / 5, slope_contrib_per_RU = r$scp / r$scp_xx, slope_z_per_RU = r$scp_z / r$scp_xx,
        denom_flag = abs(r$V) < 0.25 * abs(V_all), convention = S33A_LAB$e2, procedural_alternatives = unname(S33A_PROCEDURAL[r$measure]),
        repair_min = if (is.null(rr)) NA_real_ else rr$repair_min, repair_median = if (is.null(rr)) NA_real_ else rr$repair_median,
        repair_max = if (is.null(rr)) NA_real_ else rr$repair_max,
        small_V_replicates_animal = if (is.null(sv)) NA_integer_ else sv[scheme == "animal", small_V_count] %a_or% NA_integer_,
        small_V_replicates_cc1 = if (is.null(sv)) NA_integer_ else sv[scheme == "cc1", small_V_count] %a_or% NA_integer_,
        small_V_replicates_cc4 = if (is.null(sv)) NA_integer_ else sv[scheme == "cc4", small_V_count] %a_or% NA_integer_,
        ci_flag = if (r$estimand != "share_total") S33A_LAB$con_flag else NA_character_,
        note = paste(c(S33A_LAB$share, if (abs(r$V) < 0.25 * abs(V_all)) S33A_LAB$denom, if (r$estimand != "share_total") S33A_LAB$con_phrase,
                       if (vn == "Movement_mean") S33A_LAB$continuity), collapse = "; ")),
        bc, bs[, .(slope_contrib_envelope_low = envelope_low, slope_contrib_envelope_high = envelope_high)]) })
    data.table::rbindlist(rows, fill = TRUE) }), fill = TRUE)
  tabs$a03_cohort_covariance_shares <- s33a_finish(a03, c("version", "measure", "estimand", "arm"))
  tabs$a04_con_repairing_reference <- s33a_finish(data.table::rbindlist(lapply(seq_len(nrow(rps)), function(i) { r <- rps[i]
    s33_row("share_shared", "cohort", estimate = r$observed, units = S33A_U$share, metric_id = "crossing_rate", n_cohorts = 6L, version = r$version,
            measure = r$measure, arm = "CON", observed = r$observed, min = r$repair_min, median = r$repair_median, max = r$repair_max,
            n_pairings = r$n_pairings, ci_flag = S33A_LAB$con_flag, note = S33A_LAB$repair) })), c("version", "measure"))
  # ---- a05 slopes
  cm <- data.table::rbindlist(lapply(S33A_ARMS, function(ar) data.table::rbindlist(lapply(S33A_MEAN_VARS, function(v)
    data.table::data.table(Batch = S33_COHORTS, arm_ = ar, measure = v, value = Ap$tracked_111[1L, , ar, v])))))
  a05 <- list(
    s33a_e3a_rows(sis_t, S33A_Z_MEASURES, lead_measures = S33_COMPONENTS),
    s33a_ols_fe_rows(sis_t, c(S33A_C_VARS, "CombZ"), note = S33A_LAB$e2),
    s33a_e3b_rows(con_t, S33A_Z_MEASURES),
    data.table::rbindlist(lapply(S33A_Z_MEASURES, function(m) s33a_scatter_rows(cm, m, envs$tracked_111, lead = m %in% S33_COMPONENTS)), fill = TRUE),
    s33a_e3e_rows(sis_t, S33A_Z_MEASURES),
    s33a_e3a_rows(sis_t, "CombZ", "none", xcol = "Movement_mean", model = "S7_Movement_mean_KR", variant = "S7"),
    s33a_ols_fe_rows(sis_t, "CombZ", "none", xcol = "Movement_mean", model = "S7_Movement_mean_OLS_FE", variant = "S7", note = S33A_LAB$s7),
    s33a_e3e_rows(sis_t, "CombZ", xcol = "Movement_mean", cc4 = FALSE, model = "S7_continuity_contextual_KR", variants = "primary", adjust = "none",
                  note = S33A_LAB$continuity),
    s33a_e3a_rows(sis_t[S33A_S8$S8_drop_HW_A1(sis_t)], S33A_Z_MEASURES, "none", model = "E3a_KR", variant = "S8_drop_HW_A1", note = S33A_LAB$S8_drop_HW_A1),
    s33a_e3a_rows(sis_t[S33A_S8$S8_drop_J7(sis_t)], S33A_Z_MEASURES, "none", model = "E3a_KR", variant = "S8_drop_J7", note = S33A_LAB$S8_drop_J7),
    s33a_s8_six_point(frames$tracked_111, "S8_drop_HW_A1", S33A_Z_MEASURES, "E3c_six_point"),
    s33a_s8_six_point(frames$tracked_111, "S8_drop_J7", S33A_Z_MEASURES, "E3c_six_point"),
    s33a_e3a_rows(sis_t[Sex == "Female"], S33_COMPONENTS, "none", model = "S9_sex_stratified_KR", variant = "S9_Female"),
    s33a_e3a_rows(sis_t[Sex == "Male"], S33_COMPONENTS, "none", model = "S9_sex_stratified_KR", variant = "S9_Male"))
  mm6 <- s33a_scatter_slope(cm[arm_ == "SIS" & measure == "CombZ", value], Ap_mm[1L, , "SIS", "x_RU"])
  a05[[length(a05) + 1L]] <- s33_row("between_cohort_slope", "cohort", estimate = mm6$estimate, se = mm6$se, df = mm6$df, ci_low = mm6$ci_low,
    ci_high = mm6$ci_high, interval_method = s33a_t_method(mm6$df), interval_basis = "cohort_scatter_model", units = S33A_U$mm, metric_id = "Movement_mean",
    n_cohorts = mm6$n, n_params = 2L, model = "S7_Movement_mean_six_point", variant = "S7", measure = "CombZ", arm = "SIS", adjustment = "none",
    population = "tracked_111", note = paste(S33A_LAB$continuity, S33A_LAB$scatter, sep = "; "))
  for (rf in c("H03", "S29_parent", "S29_parent_RU", "H13")) { v <- refv[[rf]]
    a05[[length(a05) + 1L]] <- s33_row("registered_reference", if (rf == "H01") "cohort" else "animal_within_cohort", estimate = v[["estimate"]],
      se = if ("se" %in% names(v)) v[["se"]] else NA_real_, df = if ("df" %in% names(v)) v[["df"]] else NA_real_, ci_low = v[["ci_low"]] %a_or% NA_real_,
      ci_high = v[["ci_high"]] %a_or% NA_real_,
      interval_method = switch(rf, H03 = s33a_kr_method(v[["ddf_fallback"]]), H13 = "CR2_Satterthwaite_cc1_cage", s33a_t_method(v[["df"]])),
      interval_basis = "conditional_on_cohorts", units = switch(rf, H03 = S33A_U$h03, S29_parent = S33A_U$s29, S29_parent_RU = S33A_U$s29_ru, H13 = S33A_U$h13),
      metric_id = if (rf == "H13") "Movement_mean" else "crossing_rate", model = "registered_reference", variant = if (rf == "H13") "S9" else "reference",
      measure = "CombZ", arm = "SIS", reference = rf, note = switch(rf, H03 = S33A_LAB$h03, H13 = S33A_LAB$h13, S33A_LAB$s29)) }
  a05 <- data.table::rbindlist(a05, fill = TRUE)
  ols6 <- a05[model == "E3a_OLS_FE" & variant == "primary"]
  for (adj in c("none", "tp2_c")) { x <- ols6[adjustment == adj]
    err <- abs(sum(x[measure %in% paste0("c_", S33_COMPONENTS), estimate]) - x[measure == "CombZ", estimate])
    a05[model == "E3a_OLS_FE" & variant == "primary" & adjustment == adj & measure == "CombZ", additivity_abs_error := err] }
  tabs$a05_component_slopes <- s33a_finish(a05, c("model", "variant", "measure", "arm", "adjustment", "reference", "population"))
  # ---- a06 wide view
  pick <- function(mod, vr, m, adj, arm_k = "SIS", est = NULL) { x <- a05[model == mod & variant == vr & measure == m & adjustment == adj & arm == arm_k]
    if (!is.null(est)) x <- x[estimand == est]
    if (nrow(x) != 1L) return(list(v = c(NA_real_, NA_real_, NA_real_), im = NA_character_)); list(v = c(x$estimate, x$ci_low, x$ci_high), im = x$interval_method) }
  a06 <- data.table::rbindlist(lapply(S33A_Z_MEASURES, function(m) data.table::rbindlist(lapply(S33A_ARMS, function(ar) {
    na3 <- list(v = c(NA_real_, NA_real_, NA_real_), im = NA_character_)
    if (ar == "SIS") { w1 <- pick("E3a_KR", "primary", m, "none"); w2 <- pick("E3a_KR", "primary", m, "tp2_c"); b1 <- pick("E3c_six_point", "all_six", m, "none")
      be <- a05[model == "E3c_six_point" & variant == "all_six" & measure == m, c(envelope_low, envelope_high)]
      b2 <- pick("E3c_six_point", "within_sex", m, "none"); cx <- pick("E3e_contextual_KR", "primary", m, "none", est = "contextual_cohort_minus_within")
      cw <- pick("E3e_contextual_KR", "primary", m, "tp2_w", est = "within_cohort_slope")
    } else { w1 <- pick("E3b_CON_OLS", "primary", m, "none", "CON"); w2 <- pick("E3b_CON_OLS", "primary", m, "tp2_c", "CON")
      b1 <- pick("E3d_CON_six_point", "con_on_sis_x", m, "none", "CON"); be <- c(NA_real_, NA_real_); b2 <- pick("E3d_CON_six_point", "con_x_within_sex", m, "none", "CON")
      cx <- na3; cw <- na3 }
    if (length(be) != 2L) be <- c(NA_real_, NA_real_)
    data.table::data.table(measure = m, arm = ar, metric_id = "crossing_rate", metric_label = S33_METRIC_LABEL[["crossing_rate"]], units = s33a_unit_slope(m),
      within_cohort_slope_estimate = w1$v[1], within_cohort_slope_ci_low = w1$v[2], within_cohort_slope_ci_high = w1$v[3],
      within_cohort_slope_method = w1$im, within_cohort_slope_basis = "conditional_on_cohorts",
      within_cohort_slope_tp2_estimate = w2$v[1], within_cohort_slope_tp2_ci_low = w2$v[2], within_cohort_slope_tp2_ci_high = w2$v[3],
      between_cohort_slope_estimate = b1$v[1], between_cohort_slope_ci_low = b1$v[2], between_cohort_slope_ci_high = b1$v[3],
      between_cohort_slope_method = b1$im, between_cohort_slope_basis = "cohort_scatter_model",
      between_cohort_slope_regressor = if (ar == "SIS") "SIS cohort mean rate (six cohorts)" else "SIS cohort mean rate; CON cage means as outcome",
      between_cohort_slope_envelope_low = be[1], between_cohort_slope_envelope_high = be[2],
      between_cohort_slope_within_sex_estimate = b2$v[1], between_cohort_slope_within_sex_ci_low = b2$v[2], between_cohort_slope_within_sex_ci_high = b2$v[3],
      contextual_cohort_minus_within_estimate = cx$v[1], contextual_cohort_minus_within_ci_low = cx$v[2], contextual_cohort_minus_within_ci_high = cx$v[3],
      contextual_within_tp2_w_estimate = cw$v[1], contextual_within_tp2_w_ci_low = cw$v[2], contextual_within_tp2_w_ci_high = cw$v[3],
      ci_flag = if (ar == "CON") S33A_LAB$con_flag else NA_character_,
      note = if (m == "CombZ") paste(S33A_LAB$combz_a05, S33A_LAB$continuity, sep = "; ") else if (ar == "SIS") S33A_LAB$decomp else S33A_LAB$con_between) }))))
  tabs$a06_component_slopes_wide <- a06[, tier := S33_TIER][]
  # ---- a07 corticosterone (ng/ml; log10(y + 1))
  a07 <- list(); ft <- frames$tracked_111
  for (bb in S33_COHORTS) for (m in c(S33A_CORT, S33A_LCORT)) {
    fs <- ft[Batch == bb & arm == "SIS"]; fc <- ft[Batch == bb & arm == "CON"]; raw_m <- sub("^l", "", m)
    desc <- function(z, zr) list(sd = stats::sd(z, na.rm = TRUE), median = stats::median(z, na.rm = TRUE), q25 = unname(stats::quantile(z, 0.25, na.rm = TRUE, type = 7)),
                                 q75 = unname(stats::quantile(z, 0.75, na.rm = TRUE, type = 7)), n_below_1ng = if (raw_m == "cort_delta") NA_integer_ else sum(zr < 1, na.rm = TRUE))
    for (ar in c("SIS", "CON")) { z <- if (ar == "SIS") fs[[m]] else fc[[m]]; zr <- if (ar == "SIS") fs[[raw_m]] else fc[[raw_m]]; ds <- desc(z, zr)
      id <- s33a_id("E1", "-", "cohort_mean", bb, ar, m)
      c1 <- if (ar == "SIS") s33a_cr2_cell(z, fs$cc1_cage) else s33a_cr2_cell(numeric(), character())
      c4 <- if (ar == "SIS") s33a_cr2_cell(z, fs$cc4_cage) else s33a_cr2_cell(numeric(), character())
      if (ar == "SIS") { bc <- s33a_boot_cols(envs$tracked_111, id); flat <- s33a_no_spread(z)
        lo <- if (flat) NA_real_ else bc$envelope_low; hi <- if (flat) NA_real_ else bc$envelope_high; im <- if (flat) "none" else "envelope_bootstrap"
        se <- NA_real_; df <- NA_real_ } else {
        ct <- s33a_mean_t(z); bc <- s33a_boot_cols(NULL, NA_character_); flat <- isTRUE(ct$no_spread); lo <- ct$ci_low; hi <- ct$ci_high
        im <- if (is.finite(ct$ci_low)) paste0("t_", ct$df) else "none"; se <- ct$se; df <- ct$df }
      a07[[length(a07) + 1L]] <- cbind(s33_row("cohort_mean", "cohort", estimate = pt$tracked_111[[id]], se = se, df = df, ci_low = lo, ci_high = hi, interval_method = im,
        interval_basis = if (im == "none") NA_character_ else "conditional_on_cohorts", units = s33a_unit_mean(m), n_animals = sum(is.finite(z)), n_cohorts = 1L,
        Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], measure = m, arm = ar, scale = if (m %in% S33A_LCORT) "log10p1" else "raw", sd = ds$sd, median = ds$median,
        q25 = ds$q25, q75 = ds$q75, n_below_1ng = ds$n_below_1ng, ci_flag = if (ar == "CON") S33A_LAB$con_flag else NA_character_,
        interval_note = if (flat) S33A_NO_SPREAD_NOTE else if (ar == "SIS") S33A_LAB$range80 else NA_character_,
        note = if (bb == "B3") S33A_LAB$b3 else NA_character_), bc,
        data.table::data.table(cr2_cc1_se = c1$se, cr2_cc1_df = c1$df, cr2_cc1_ci_low = c1$ci_low, cr2_cc1_ci_high = c1$ci_high,
                               cr2_cc4_se = c4$se, cr2_cc4_df = c4$df, cr2_cc4_ci_low = c4$ci_low, cr2_cc4_ci_high = c4$ci_high))
    }
    idd <- s33a_id("E1", "-", "sis_minus_con_diff", bb, "SIS_minus_CON", m); bd <- s33a_boot_cols(envs$tracked_111, idd); w <- s33a_welch(fs[[m]], fc[[m]])
    a07[[length(a07) + 1L]] <- cbind(s33_row("sis_minus_con_diff", "cohort", estimate = pt$tracked_111[[idd]], ci_low = bd$envelope_low, ci_high = bd$envelope_high,
      interval_method = "envelope_bootstrap", interval_basis = "conditional_on_cohorts", units = s33a_unit_mean(m), n_animals = sum(is.finite(fs[[m]])) + sum(is.finite(fc[[m]])),
      n_cohorts = 1L, Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], measure = m, arm = "SIS_minus_CON", scale = if (m %in% S33A_LCORT) "log10p1" else "raw",
      welch_se = w$se, welch_df = w$df, welch_ci_low = w$ci_low, welch_ci_high = w$ci_high, ci_flag = S33A_LAB$con_flag,
      note = if (bb == "B3") S33A_LAB$b3 else NA_character_), bd)
    for (e in c("offset_total", "part_shared_con", "part_not_shared")) {
      ar <- c(offset_total = "SIS", part_shared_con = "CON", part_not_shared = "SIS_minus_CON")[[e]]; id <- s33a_id("E1b", "-", e, bb, ar, m)
      bc <- s33a_boot_cols(envs$tracked_111, id)
      a07[[length(a07) + 1L]] <- cbind(s33_row(e, "cohort", estimate = pt$tracked_111[[id]], ci_low = bc$envelope_low, ci_high = bc$envelope_high,
        interval_method = "envelope_bootstrap", interval_basis = "conditional_on_cohorts", units = s33a_unit_mean(m), n_cohorts = 3L, Batch = bb,
        Sex = S33_SEX_OF_COHORT[[bb]], measure = m, arm = ar, scale = if (m %in% S33A_LCORT) "log10p1" else "raw", offset_reference = S33A_LAB$offset_ref,
        ci_flag = if (e != "offset_total") S33A_LAB$con_flag else NA_character_, note = if (bb == "B3") S33A_LAB$b3 else NA_character_), bc)
    }
  }
  cort_models <- list(
    s33a_e3a_rows(sis_t, c(S33A_CORT, S33A_LCORT), c("none", "tp2_c"), model = "E4_KR"),
    s33a_e3b_rows(con_t, c(S33A_CORT, S33A_LCORT))[, model := "E4_CON_OLS"],
    data.table::rbindlist(lapply(c(S33A_CORT, S33A_LCORT), function(m) s33a_scatter_rows(cm, m, envs$tracked_111, model_sis = "E4_six_point",
                                                                                        model_con = "E4_CON_six_point")), fill = TRUE),
    s33a_e3a_rows(sis_t[Batch != "B3"], c(S33A_CORT, S33A_LCORT), "none", model = "E4_KR", variant = "S10_omit_B3", note = S33A_LAB$b3),
    s33a_e3a_rows(sis_t[S33A_S8$S8_drop_HW_A1(sis_t)], c(S33A_CORT, S33A_LCORT), "none", model = "E4_KR", variant = "S8_drop_HW_A1", note = S33A_LAB$S8_drop_HW_A1),
    s33a_e3a_rows(sis_t[S33A_S8$S8_drop_J7(sis_t)], c(S33A_CORT, S33A_LCORT), "none", model = "E4_KR", variant = "S8_drop_J7", note = S33A_LAB$S8_drop_J7),
    s33a_s8_six_point(frames$tracked_111, "S8_drop_HW_A1", c(S33A_CORT, S33A_LCORT), "E4_six_point"),
    s33a_s8_six_point(frames$tracked_111, "S8_drop_J7", c(S33A_CORT, S33A_LCORT), "E4_six_point"))
  om3 <- data.table::rbindlist(lapply(c(S33A_CORT, S33A_LCORT), function(m) {
    f <- s33a_scatter_slope(cm[arm_ == "SIS" & measure == m & Batch != "B3", value], cm[arm_ == "SIS" & measure == "x_RU" & Batch != "B3", value])
    s33_row("between_cohort_slope", "cohort", estimate = f$estimate, se = f$se, df = f$df, ci_low = f$ci_low, ci_high = f$ci_high,
            interval_method = s33a_t_method(f$df), interval_basis = "cohort_scatter_model", units = s33a_unit_slope(m), metric_id = "crossing_rate", n_cohorts = f$n,
            n_params = 2L, model = "E4_six_point", variant = "S10_omit_B3", measure = m, arm = "SIS", adjustment = "none", note = S33A_LAB$b3) }))
  cyc <- data.table::as.data.table(inp$cycle)[, .(AnimalNum, cycle_phase)]
  sc <- merge(sis_t[Batch %in% c("B3", "B4")], cyc, by = "AnimalNum", all.x = TRUE)
  sc[, luteal := data.table::fifelse(grepl("lut", tolower(cycle_phase)), 1, data.table::fifelse(grepl("foll", tolower(cycle_phase)), 0, NA_real_))]
  cyc_rows <- data.table::rbindlist(lapply(S33A_CORT, function(m) {
    dat <- s33a_model_data(sc[is.finite(luteal)], m); dat[, luteal := sc$luteal[match(dat$AnimalNum, sc$AnimalNum)]]
    fit <- s33_kr_fit("y ~ 0 + Batch + x + luteal + (1 | cc1_cage) + (1 | cc4_cage)", dat, s33a_id("A", "E4_KR", "S10_cycle_phase", m),
                      nlevels(droplevels(dat$Batch)) + 2L)
    s33a_kr_row(fit, list(x = 1), "within_cohort_slope", "animal_within_cohort", "conditional_on_cohorts", s33a_unit_slope(m),
                n_cages = data.table::uniqueN(dat$cc1_cage), model = "E4_KR", variant = "S10_cycle_phase_B3_B4", measure = m, arm = "SIS", adjustment = "cycle_phase",
                note = "female cycle phase (follicular / luteal) added; B3 and B4 only (B6 unrecorded)") }), fill = TRUE)
  tabs$a07_cort_raw <- s33a_finish(data.table::rbindlist(c(a07, cort_models, list(om3, cyc_rows)), fill = TRUE),
                                   c("model", "variant", "Batch", "Sex", "measure", "arm", "scale", "adjustment"))
  # ---- a08, a09 weight gains
  g <- s33a_gain_frame(d, design_out$episodes)
  tabs$a08_weight_episode_gains <- s33a_finish(s33a_gain_table(g, d), c("Batch", "Sex", "arm", "episode"))
  a09 <- s33a_e5_rows(g, S33_HW_A1)
  e5e <- s33a_scatter_rows(cm, "gain_cc1", envs$tracked_111, con = FALSE, model_sis = "E5e_six_point")[variant == "all_six"]
  e5e[, note := paste(note, "CC1-episode gains (tp3 - tp2); cohort tp2 travels with the rate (r 0.977)", sep = "; ")]
  tabs$a09_weight_gain_models <- s33a_finish(data.table::rbindlist(list(a09, e5e), fill = TRUE), c("model", "variant", "arm", "term", "episode", "adjustment"))
  # ---- a10, a11 registered rows
  rc <- s33a_registered_copy(inp$s32_multiplicity, inp$s32_estimates, inp$sha$s32_multiplicity, inp$sha$s32_estimates)
  tabs$a10_s32_multiplicity_rows <- rc$a10; tabs$a11_s32_estimate_rows <- rc$a11
  # ---- a12 descriptors: the Phase-2 product unchanged (rows, order, values) plus the Phase-3 columns
  p3c <- data.table::rbindlist(lapply(S33_COHORTS, function(bb) { st <- d[tracked == TRUE & Batch == bb & arm == "SIS"]; cn <- d[Batch == bb & arm == "CON"]
    c4 <- st2$inputs2$a1_dates[Batch == bb & CC == "CC4", a1_date]; t6 <- unique(d[Batch == bb, tp6_date])
    data.table::data.table(Batch = bb, tp6_SIS_mean = mean(st$tp6), tp6_CON_mean = mean(cn$tp6), tp6_minus_cc4_d = paste(sort(unique(as.numeric(t6 - c4))), collapse = ";"),
      tp6_relative_to_sucrose_test = S33A_DATES[Batch == bb, tp6_relative_to_test], n_RES_tracked = sum(st$outcome_group == "RES"),
      n_SUS_tracked = sum(st$outcome_group == "SUS")) }))
  tabs$a12_cohort_descriptors <- s33a_a12_phase3(p2$products$a12_cohort_descriptors_phase2, p3c)
  # ---- a13 scheme comparison
  a13 <- list()
  for (pop in S33A_POPULATIONS) { cohorts <- if (pop == "tracked_111") S33_COHORTS else "B1"; meas <- if (pop == "tracked_111") S33A_E1_MEASURES else setdiff(S33A_E1_MEASURES, "x_RU")
    f <- frames[[pop]]
    for (bb in cohorts) for (m in meas) { id <- s33a_id("E1", "-", "cohort_mean", bb, "SIS", m); e <- envs[[pop]][stat_id == id]; fs <- f[Batch == bb & arm == "SIS" & is.finite(get(m))]
      a13[[length(a13) + 1L]] <- data.table::data.table(population = pop, Batch = bb, Sex = S33_SEX_OF_COHORT[[bb]], measure = m, metric_id = s33a_metric(m),
        units = s33a_unit_mean(m), n_animals = nrow(fs), n_cc1_cages = data.table::uniqueN(fs$cc1_cage), n_cc4_cages = data.table::uniqueN(fs$cc4_cage),
        se_animal = e$sd_animal, se_cc1 = e$sd_cc1, se_cc4 = e$sd_cc4, ratio_cc1 = e$sd_cc1 / e$sd_animal, ratio_cc4 = e$sd_cc4 / e$sd_animal,
        width_animal = e$q975_animal - e$q025_animal, width_cc1 = e$q975_cc1 - e$q025_cc1, width_cc4 = e$q975_cc4 - e$q025_cc4,
        envelope_low = e$envelope_low, envelope_high = e$envelope_high, envelope_set_by = e$envelope_set_by,
        cage_range_narrower = (e$q975_cc1 - e$q025_cc1) < (e$q975_animal - e$q025_animal) | (e$q975_cc4 - e$q025_cc4) < (e$q975_animal - e$q025_animal),
        note = S33A_LAB$range80) } }
  a13 <- data.table::rbindlist(a13, fill = TRUE)
  a13[, `:=`(metric_label = unname(S33_METRIC_LABEL[metric_id]), level = "cohort", lead = FALSE, interval_basis = "conditional_on_cohorts")]
  tabs$a13_bootstrap_scheme_comparison <- s33a_finish(a13[, tier := S33_TIER], c("population", "Batch", "Sex", "measure"))
  # ---- GA-11 continuity and GA-15 planning targets
  shmm <- sh$Movement_mean[estimand == "share_total"]
  for (k in S33_COMPONENTS) cont[[paste0("share_mm|", k)]] <- s33a_one(100 * shmm[measure == k, scp] / shmm[measure == k, V])
  cont[["V_over_5_mm"]] <- s33a_one(shmm$V[1L] / 5)
  cont[["six_point_mm|estimate"]] <- s33a_one(mm6$estimate); cont[["six_point_mm|ci_low"]] <- s33a_one(mm6$ci_low); cont[["six_point_mm|ci_high"]] <- s33a_one(mm6$ci_high)
  for (pt_ in c("within", "between")) { x <- a05[model == "S7_continuity_contextual_KR" & estimand == paste0(pt_, "_cohort_slope")]
    for (f in c("estimate", "ci_low", "ci_high", "df")) cont[[paste0("mundlak_mm|", pt_, "|", f)]] <- s33a_one(if (nrow(x) == 1L) x[[f]] else NA_real_) }
  cont[["cage_sd_reference_cc1|CombZ"]] <- s33a_one(refsd[measure == "CombZ", cage_sd_reference_cc1])
  ck11 <- list(s33a_target_checks("GA-11", S33A_CONTINUITY$targets, cont))
  t55 <- S33A_CONTINUITY$table_5_5; pmm <- pt_mm; ptt <- pt$tracked_111
  for (i in seq_len(nrow(t55))) { r <- t55[i]; src <- if (r$measure == "Movement_mean") pmm else ptt; mv <- if (r$measure == "Movement_mean") "x_RU" else r$measure
    o <- c(src[[s33a_id("E1b", "-", "offset_total", r$Batch, "SIS", mv)]], src[[s33a_id("E1b", "-", "part_shared_con", r$Batch, "CON", mv)]],
           src[[s33a_id("E1b", "-", "part_not_shared", r$Batch, "SIS_minus_CON", mv)]])
    e <- c(r$offset_total, r$part_shared_con, r$part_not_shared)
    ck11[[length(ck11) + 1L]] <- s33a_chk("GA-11", sprintf("Table 5.5 split %s %s (0.001)", r$Batch, r$measure), abs(o - e) <= S33A_CONTINUITY$table_5_5_tolerance, e, o,
                                          S33A_CONTINUITY$table_5_5_tolerance) }
  a4w <- data.table::as.data.table(inp$a4_wide)
  a4c <- unlist(lapply(c(S33_COMPONENTS, "CombZ"), function(k) { r <- a4w[match(S33_COHORTS, Batch)]
    c(abs(r[[paste0("mean_SIS_", k)]] - vapply(S33_COHORTS, function(bb) ptt[[s33a_id("E1", "-", "cohort_mean", bb, "SIS", k)]], 0)),
      abs(r[[paste0("mean_CON_", k)]] - vapply(S33_COHORTS, function(bb) ptt[[s33a_id("E1", "-", "cohort_mean", bb, "CON", k)]], 0)),
      abs(r[[paste0("SIS_minus_CON_", k)]] - vapply(S33_COHORTS, function(bb) ptt[[s33a_id("E1", "-", "sis_minus_con_diff", bb, "SIS_minus_CON", k)]], 0))) }))
  ck11[[length(ck11) + 1L]] <- s33a_chk("GA-11", "SIS and CON cohort component and CombZ means and SIS-minus-CON = a4 (1e-12; 126 values)",
                                        c(length(a4c) == 126L, a4c <= S33A_CONTINUITY$a4_tolerance), "<= 1e-12", max(a4c), S33A_CONTINUITY$a4_tolerance)
  a4y <- data.table::as.data.table(inp$a4_yard)
  ck11[[length(ck11) + 1L]] <- s33a_chk("GA-11", "cage_sd_reference_cc1 (CombZ) = a4 yardstick (1e-9; 22 cages, df 16)",
    c(abs(refsd[measure == "CombZ", cage_sd_reference_cc1] - a4y[measure == "CombZ", pooled_within_batch_SD_of_SIS_cage_means]) <= 1e-9,
      refsd[measure == "CombZ", cage_sd_reference_cc1_n_cages] == 22L), a4y[measure == "CombZ", pooled_within_batch_SD_of_SIS_cage_means],
    refsd[measure == "CombZ", cage_sd_reference_cc1], 1e-9)
  all6 <- sh$all_six
  obs[["V|all_six"]] <- s33a_one(all6$V[1L])
  obs[["six_point|CombZ|estimate"]] <- s33a_one(a05[model == "E3c_six_point" & variant == "all_six" & measure == "CombZ", estimate])
  for (k in S33_COMPONENTS) { obs[[paste0("share_total|", k)]] <- s33a_one(100 * all6[measure == k & estimand == "share_total", scp] / all6$V[1L])
    obs[[paste0("share_shared|", k)]] <- s33a_one(100 * all6[measure == k & estimand == "share_shared", scp] / all6$V[1L]) }
  obs[["share_shared|CombZ_total"]] <- s33a_one(100 * all6[measure == "CombZ_total" & estimand == "share_shared", scp] / all6$V[1L])
  obs[["repairing_min|CombZ_total"]] <- s33a_one(rps[version == "all_six" & measure == "CombZ_total", repair_min])
  obs[["repairing_max|CombZ_total"]] <- s33a_one(rps[version == "all_six" & measure == "CombZ_total", repair_max])
  for (k in c("cage_sd_reference_cc1", "cage_sd_reference_cc4", "cage_sd_reference_cc4_all117", "half_within_sd_sis", "half_within_sd_con", "con_cage_mean_sd_within_sex"))
    obs[[paste0(k, "|CombZ")]] <- s33a_one(refsd[measure == "CombZ"][[k]])
  obs[["r_xbar_tp2"]] <- s33a_one(tabs$a12_cohort_descriptors[row_type == "r_with_xbar_all6", tp2_SIS_mean])
  for (adj in c("none", "tp2_c")) { for (m in c("CombZ", "c_weight_dev")) obs[[paste0("ols_fe|", m, "|", adj)]] <- s33a_one(a05[model == "E3a_OLS_FE" & variant == "primary" & measure == m & adjustment == adj, estimate])
    for (m in c("CombZ", "weight_dev")) { x <- a05[model == "E3a_KR" & variant == "primary" & measure == m & adjustment == adj]
      for (f in c("estimate", "ci_low", "ci_high")) obs[[paste0("kr|", m, "|", adj, "|", f)]] <- s33a_one(x[[f]]) } }
  for (wv in c("none", "ws_c")) { x <- a09[model == "E5a" & variant == "primary" & adjustment == wv & arm == "SIS"]
    for (f in c("estimate", "ci_low", "ci_high")) obs[[paste0("e5a|", wv, "|", f)]] <- s33a_one(if (nrow(x) == 1L) x[[f]] else NA_real_) }
  ck15 <- s33a_target_checks("GA-15", S33A_PLANNING_TARGETS, obs)
  # ---- GA-T8 verbatim copies and reference values
  ckt <- list()
  ckt[[1L]] <- s33a_chk("GA-T8", "a10: 4 registered multiplicity rows H01, H03, H11, H13", c(nrow(rc$a10) == 4L, setequal(rc$a10$HypothesisID, S33A_REGISTERED$multiplicity)),
                        S33A_REGISTERED$multiplicity, rc$a10$HypothesisID)
  ckt[[2L]] <- s33a_chk("GA-T8", "a11: 5 registered estimate rows (A_EXP 2, A_CZ 2, H13 1)", c(nrow(rc$a11) == S33A_REGISTERED$n_estimates,
                        setequal(unique(rc$a11$model_id), S33A_REGISTERED$estimates)), S33A_REGISTERED$n_estimates, nrow(rc$a11))
  ckt[[3L]] <- s33a_chk("GA-T8", "a10 = source rows, cell for cell (independent re-read)",
                        s33a_verbatim_check(rc$a10, inp$paths$s32_multiplicity, "HypothesisID"), TRUE, NA)
  ckt[[4L]] <- s33a_chk("GA-T8", "a11 = source rows, cell for cell (independent re-read)",
                        s33a_verbatim_check(rc$a11, inp$paths$s32_estimates, "model_id"), TRUE, NA)
  rv <- S33A_REFERENCE[, .(ok = abs(refv[[ref]][[field]] - value) <= tolerance, o = refv[[ref]][[field]]), by = .(ref, field, value, tolerance)]
  ckt[[5L]] <- s33a_chk("GA-T8", "reference rows = plan values (H01, RU, H03, Stage 29 parent, H13; rounding tolerance)", rv$ok, rv$value, rv$o)
  # GA-10 weight_dev link (plan E5: exact): the weight_dev interval gain = the sum of its episode gains
  wl <- tabs$a08_weight_episode_gains[episode == "weight_dev_interval"]
  ck10 <- s33a_chk("GA-10", "weight_dev interval gain = sum of its episode gains (B1-B5 CC1-CC4, B6 CC2-CC4; 1e-9; 12 cohort x arm cells)",
                   c(nrow(wl) == 12L, is.finite(wl$identity_abs_error) & wl$identity_abs_error <= 1e-9), "<= 1e-9",
                   if (nrow(wl)) max(wl$identity_abs_error) else NA_real_, 1e-9)
  ck_all <- data.table::rbindlist(c(list(data.table::copy(st2$checks)[, phase := "2"], ck3[, phase := "3"], ck10[, phase := "3"]),
                                    lapply(ck11, function(z) z[, phase := "3"]), list(ck15[, phase := "3"]), lapply(ckt, function(z) z[, phase := "3"])),
                                  fill = TRUE)
  ck_all[, tier := S33_TIER]
  gates <- s33a_gates(ck_all[phase == "3"], c("GA-4b", "GA-6", "GA-7", "GA-8", "GA-9b", "GA-10", "GA-11", "GA-15", "GA-T8"))
  # ---- audit
  streams <- data.table::copy(st2$streams)
  bs_all <- data.table::rbindlist(bsum, fill = TRUE)
  audit_b <- rbind(streams[, .(record_type = "stream", population, scheme, interval_method, seed_name, seed, B, rng_kinds, unit_order, index_sha256)],
                   bs_all[, .(record_type = "statistic", population, scheme, stat_id, n_valid, mean, sd, q025, q975, small_V_count)], fill = TRUE)
  audit_b[, tier := S33_TIER]
  for (nm in names(tabs)) if (!nm %in% c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows") && !"tier" %in% names(tabs[[nm]])) tabs[[nm]][, tier := S33_TIER]
  tabs <- tabs[S33_TABLES[["A"]]]
  cross <- list(cage_sd_reference_cc1_cages = strsplit(refsd[measure == "CombZ", cc1_cages], ";", fixed = TRUE)[[1L]],
                x_RU_sis_cc1_rows = tabs$a01_cohort_component_means[population == "tracked_111" & measure == "x_RU" & arm == "SIS",
                                                                     .(Batch, estimate, cr2_cc1_se, cr2_cc1_df, cr2_cc1_ci_low, cr2_cc1_ci_high)],
                lags = tabs$a12_cohort_descriptors[row_type == "cohort", .(Batch, lag_cc1_h)],
                cc4_cage = d[, .(AnimalNum, cc4_cage)], tp2 = d[, .(AnimalNum, tp2, src_cage)])
  ck_cols <- c("module", "step", "checkpoint_key", "file", "reused")
  checkpoints <- if (nrow(cps) && all(ck_cols %in% names(cps))) cps[, ck_cols, with = FALSE] else
    data.table::data.table(module = character(), step = character(), checkpoint_key = character(), file = character(), reused = logical())
  list(tables = tabs, audit = list(a_data_checks = ck_all, a_bootstrap_summaries = audit_b), gates = gates,
       checkpoints = checkpoints, streams = streams, cross = cross)
}
