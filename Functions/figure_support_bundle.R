# ================================================================
# Figure-support bundle v3: helpers (source-data export for manuscript Figure 1, options 3 and 3b, and the Stage 14
# domain-heatmap and HMM figure candidates)
# MMMSociability -- Functions/figure_support_bundle.R
# ================================================================
# Pure constants and functions for Analysis/16c_figure_support_bundle.R (records: docs/FIGURE_SUPPORT_BUNDLE_v1.md, _v2.md, _v3.md).
# Sourcing this file defines constants and functions only; it reads and writes nothing. It needs the generic hash-gate
# and writer utilities of Functions/stage30_figure_bundle.R (s30fb_*), which the caller sources first.
#
# NO INFERENCE. Nothing here fits, tests, refits, adjusts, smooths or predicts, and no p or q value is produced.
#   * F1 / F1b: the descriptive mean of each CON cage's 4 animals (ebb_v101 B1), and a copy of the ebb_v101 B2 CON mean.
#   * F2 / F2b: the six stored CombZ component z-scores exactly as they enter CombZ (canonical CombZ table), their
#     direction from the frozen CombZ definition, and CombZ recomposed from them by the frozen rule. The frozen
#     definition constants and the composite rule are read from the producer Analysis/build_later_outcome_combz.R by
#     parsing it: only the named constant assignments and the four composite lines are evaluated, never the file.
#     The upstream raw -> z standardisation is NOT re-derived (docs/COMBZ_CANONICAL_DEFINITION.md section 4a):
#     raw_value, reference_mean and reference_sd are therefore NA, and F2b says why.
#   * P1 / P1b / P1c (v2): verbatim copies of the post hoc run 29b (registry POSTHOC_CON_CONTRASTS v1.0; the model was
#     fitted there, once): estimates (batch-balanced means, contrasts, Holm, labels), the common-cage-variance sensitivity
#     and the diagnostics. Every cell is carried as text; the written lines are checked to be the source lines plus the
#     three provenance cells. The run is hash-gated on its audit/output_manifest.csv. Nothing is fitted or adjusted here.
#   * D1 (v3): a verbatim copy of the Stage 14 domain-heatmap source table of the run promoted 2026-10-06c (Hedges g, the
#     post hoc tests and markers were computed there); line-level verbatim gate as for P1-P1c.
#   * M1-M4 (v3): the stored Stage 14 HMM figure tables of the same run, with the state display labels joined by the
#     stored semantic label: group mean transition probabilities and their differences (M1, M2; M2 adds the rank Stage 14
#     draws the 8 largest by), and the HMM-resolution sensitivity rows with the Wald interval Stage 14 draws (M4). M3
#     counts the stored Stage 08 mean dwell times in bins doubling from one 10-minute HMM bin: plain counts, where Stage 14
#     drew kernel densities, so nothing is smoothed here either.
#   * E1 (v4): a verbatim copy of Stage 09's sex-stratified feature-to-CombZ correlations (Spearman rho, p and BH q within
#     sex were computed there); hash-pinned, line-level verbatim gate.
#   * E2 / E2b / E2c (v4): verbatim copies of the post hoc run 29c (registry POSTHOC_MOVEMENT_COMBZ_FIT v1.0; the
#     least-squares display line and its band were fitted there, once); hash-gated on its audit/output_manifest.csv.
#   * T1 / T1b (v4): the postnatal age of every animal at each step of the experimental design, computed by date
#     arithmetic (event date - date of birth) from the pinned planning workbook (the file Stage 33 pins) and the ebb_v101
#     cage-change times; checked against the ages the workbook itself records. No model, no p or q value.
# Requires: data.table, digest, tibble (the producer's component table is a tibble::tribble), readxl (v4: the workbook).
# ================================================================

FSB_SCHEMA_VERSION <- "4"
FSB_BUNDLE_PREFIX <- "fsb_v4"   # v1 (fsb_v1_20260930_f06552c), v2 (fsb_v2_20260930_a23d699) and v3 (fsb_v3_20261007_a2acd57), all FROZEN, are never rewritten
FSB_STATUS_REAL <- "FROZEN"
FSB_STATUS_DRY <- "DRY_RUN_NOT_FOR_USE"
FSB_TOL <- 1e-12

FSB_STAGE29 <- list(bundle_id = "ebb_v101_20260929_b2ce507",
                    manifest_sha256 = "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c")
# v2: the post hoc run 29b (Analysis/29b_posthoc_con_contrasts.R, registry POSTHOC_CON_CONTRASTS v1.0), REAL, run once
FSB_POSTHOC <- list(stage_dir = "29b_posthoc_con_contrasts", run_name = "v1.0_7f1da1f",
                    run_commit = "7f1da1f6cea098a97cd0b06e3a37140826dda2f1",
                    output_manifest_sha256 = "4c079ecfb31db159ea7c28379fac48ac6fbb27e6e7af00762840f7fdf0367faa", output_manifest_n = 7L,
                    registry_version = "1.0", registry_sha256 = "a6435d8d84b164671b59486f7a3eb9af2bff29bd251c58866cd4b777f9fa0604",
                    status_counts = "primary_separate_cage_variances OK=4; sensitivity_common_cage_variance OK=4",
                    tier = "POST HOC exploratory")
# bundle table -> post hoc run file (copied verbatim) and its expected row count
FSB_POSTHOC_TABLES <- c(P1_posthoc_con_contrasts = "tables/estimates.csv", P1b_posthoc_sensitivity = "tables/sensitivity_common_cage_variance.csv",
                        P1c_posthoc_diagnostics = "tables/diagnostics.csv")
FSB_POSTHOC_ROWS <- c(P1_posthoc_con_contrasts = 24L, P1b_posthoc_sensitivity = 24L, P1c_posthoc_diagnostics = 8L)
FSB_POSTHOC_ADDED <- c("source_run", "source_file", "source_sha256")
FSB_POSTHOC_DISPLAY_COLS <- c("measure_label", "unit", "estimand_label", "display_note", "caveats", "registered_rs_note")
# v3: the Stage 14 display tables of the run promoted 2026-10-06c (the promotion record lists every file with this SHA-256;
# Stage 14 is not re-run for the bundle) and the Stage 08 10-minute dwell-time table Stage 14 reads
FSB_STAGE14 <- list(
  root_rel = "analyses/systems_dashboard/5min", record = "docs/behavior_output_producer_reruns/stage14_20261006c.csv",
  files = c(domain_heatmap = "stats_tables/systems_sis_domain_heatmap_source_data.csv",
            transition_means = "tables/systems_hmm_transition_probability_summary.csv",
            transition_differences = "tables/systems_hmm_transition_probability_differences.csv",
            resolution_sensitivity = "stats_tables/systems_sis_hmm_resolution_sensitivity.csv",
            state_display_labels = "tables/systems_hmm_state_display_labels.csv"),
  sha256 = c(domain_heatmap = "fa4c613260a157ffd6044ce27ab733d583741a65597a32b7031ca691347d2271",
             transition_means = "0279d418ceca5c6b43f60c02574dca24537712925ac4c63a654af25b42b39837",
             transition_differences = "222208fb9742b4e45b3b2c7c806843765fd5a59e78a534193415cf20d645a568",
             resolution_sensitivity = "d3a79a1afd938e7f63ce0e054125d2affd3a34c9159b42e3ecb772645b58ffa5",
             state_display_labels = "60ec75e62b4b407fef8c3ef8750cd816b821d231df473c3f5cb72e18b1132c29"))
FSB_HMM_DWELL <- list(root_rel = "analyses/hmm_states/10min", file = "tables/hmm_state_dwell_times.csv",
                      sha256 = "be4ba0cf6095726326cad6eb8896e6e84509f41edceb5a64887b265fa16dc03d", bin_size_sec = 600, n_records = 3346L)
FSB_HMM_RESOLUTION <- "10min_based"           # the HMM resolution of the Stage 14 HMM figures (hmm_primary_bin_level)
FSB_HMM_STATES <- as.character(1:4)
FSB_HMM_GROUPS <- c("CON", "RES", "SUS")
FSB_HMM_CONTRASTS <- c("SUS - RES", "SUS - CON", "RES - CON")   # as Stage 14 stores them: comparison - reference
FSB_HMM_FLOW_TOP <- 8L                        # Stage 14 draws the 8 largest mean transition probabilities per Sex x Group
FSB_HMM_PHASE_LABELS <- c(Active = "Dark phase", Inactive = "Light phase")   # as the Stage 14 heatmaps label them
# dwell-time bins in 10-minute HMM bins, doubling from one bin: [1, 2), [2, 4), ..., [64, 128] = 10-20, ..., 640-1280 min
FSB_DWELL_EDGES_BINS <- 2^(0:7)
FSB_D1_ROWS <- c(all_blocks = 84L, cc1_a1 = 42L, cc1_l1 = 42L)   # rows per variant
FSB_D1_VARIANTS <- c("picked", "all5min", "all10min")
FSB_D1_DISPLAY_COLS <- c("window_label", "row_label", "row_description", "display_tier")
# v4: Stage 09's sex-stratified correlations (10-minute resolution, the Stage 09 output root the inventory declares;
# the Movement_mean rows are identical at 5 minutes). Copied verbatim; the file's SHA-256 is pinned.
FSB_STAGE09 <- list(root_rel = "pipeline/09_early_prediction/10min", file = "tables/primary_movement_entropyacf1_correlations_by_sex.csv",
                    sha256 = "aeec1749e8ede6c2300c055f25d5d9f6ca66636d28610dffda2825532fd98819", n_rows = 6L,
                    features = c("Movement_mean", "Movement_rmssd", "Entropy_acf1"), feature = "Movement_mean",
                    # the same run's per-animal input (pinned by the Stage 29 v1.0.1 release run_inputs), read for an identity gate only
                    input_file = "tables/model_ladder_input.csv",
                    input_sha256 = "da78f80e4b7867cbabe7847a7da2667459a690a1b25f8ff97ef919cc8d2426dd")
# v4: the post hoc run 29c (Analysis/29c_posthoc_movement_combz_fit.R, registry POSTHOC_MOVEMENT_COMBZ_FIT v1.0), REAL, run once
FSB_POSTHOC29C <- list(stage_dir = "29c_posthoc_movement_combz_fit", run_name = "v1.0_fd61cd9",
                       run_commit = "fd61cd99787e9ce207a650b8c6a6f1ef3764ac30",
                       output_manifest_sha256 = "fc0d13db4faa936522347ccf56aa001bc021fde2fe2c48d19b30153f3e463ffd", output_manifest_n = 7L,
                       registry_version = "1.0", registry_sha256 = "a617bb5be709b558ed628a61fab0095b598345912c86e9c25070d07b02cb8098",
                       status_counts = "Female OK; Male OK", tier = "POST HOC descriptive display")
FSB_POSTHOC29C_TABLES <- c(E2_movement_combz_fit_lines = "tables/fit_lines.csv", E2b_movement_combz_fit_coefficients = "tables/coefficients.csv",
                           E2c_movement_combz_fit_diagnostics = "tables/diagnostics.csv")
FSB_POSTHOC29C_ROWS <- c(E2_movement_combz_fit_lines = 202L, E2b_movement_combz_fit_coefficients = 4L, E2c_movement_combz_fit_diagnostics = 2L)
# v4: the planning workbook (Exp9 Planning folder beside the endpoint source root) as of 2026-10-12 11:58: the author's new
# Animals column "SIS start" (saved 11:50), with the batch-6 year typo corrected at his request (20 cells 45023 = 2023-04-07
# -> 45389 = 2024-04-07, the batch-6 CC1 date; cell values only, nothing else changed; the 11:50 file is kept as
# Planning/old/E9_Experimental Timeline_2026-10-12_before_B6_SIS_start_fix.xlsx). Stage 33 pinned earlier bytes (2b785435...).
FSB_TIMELINE <- list(file = "E9_Experimental Timeline.xlsx", sheet = "Animals",
                     sha256 = "098f22db860253712e58423c20987c15a5e136ddb37d69bfb9bd03b3da8ec582")
# the design steps with a printed age, in timeline order. source: the workbook column holding the date (sheet = the
# column the workbook itself records the age in, NA if none); cc = the ebb_v101 cage change whose time gives the date.
FSB_DESIGN_EVENTS <- data.table::data.table(
  event_order = 1:8,
  event_id = c("shuffle_start", "shuffle_stop", "sucrose_preference", "social_preference", "open_field", "novel_object",
               "elevated_plus_maze", "samples"),
  event_label = c("Shuffle start", "Shuffle stop", "Sucrose preference test", "Social preference test", "Open field test",
                  "Novel object recognition test", "Elevated plus maze", "Blood and tissue collection"),
  source = c("CC1", "CC4", "SucPref", "SocPref", "OFT", "NOR", "EPM", "End date"),
  definition = c("first cage change (CC1): the date of the CC1 cage-change time (ebb_v101 V_runtime_gates t0)",
                 "end of the active phase after the last regrouping (CC4): the date of the CC4 cage-change time + 1 day",
                 "sucrose preference test window: first to last day, as the workbook records it (SucPref, 'start - end')",
                 "social preference test day (SocPref)", "open field test day (OFT)", "novel object recognition test day (NOR)",
                 "elevated plus maze test day (EPM)", "blood and tissue collection day (End date)"))
FSB_DESIGN_CC1_AGE <- c(22L, 29L)   # Stage 33 gate GC-6l: every one of the 117 animals is 22-29 days old at CC1
FSB_DESIGN_UNIT <- "postnatal day (event date - date of birth, in days)"
# canonical CombZ tables and label lists: the on-disk SHA-256 must equal these (the first three = Stage 30 input_hashes)
FSB_INPUT_SHA256 <- c(
  combz_table = "1f6a2a69c6b0781b8de8da50ecdadf309b62bbc9c82106c0fa91d18119a7de54",
  sus_list = "dea3804b71b479f84900f946e5494b204ed0e8801498da459a6fc781beb9f391",
  con_list = "eddd2ee9c3a182f98211b4cbba689e2bb5f72985aeb178f72b25b7d25db1b75d",
  combz_component_definition = "af3c273434c7457b7b28ba39bbc231afc5f161435cf04b588c9de820fab6f70f",
  combz_classification_thresholds = "e1536f0c6e45a708770b0b78139232ebf9077eed4b884fd3e03524aad861afc9")
# the frozen CombZ definition code: the producer, pinned by its git blob (git hash-object; line-ending independent)
# (version 2 since 708e615; fsb_v1-v4 were built with version 1, blob d850859e at bac46bf)
FSB_COMBZ_CODE <- list(path = "Analysis/build_later_outcome_combz.R", git_blob = "3303304ec9309f6361326c9e7acc7e023c134715",
                       last_commit = "708e615", doc = "docs/COMBZ_CANONICAL_DEFINITION.md")
FSB_COMBZ_CONSTANTS <- c("COMBZ_DEFINITION_ID", "COMBZ_REFERENCE_POP_ID", "COMBZ_CLASSIFICATION_ID", "COMBZ_CORRECTION_ID",
                         "COMBZ_COMPONENTS", "COMBZ_COMPONENT_META", "PARITY_TOL_COMBZ")
# the producer's composite rule, evaluated verbatim (build_later_outcome_combz.R, "recompute the composite")
FSB_COMBZ_RULE <- c("comp_mat <- as.matrix(dat[, COMBZ_COMPONENTS])",
                    "dat$n_components_present <- rowSums(is.finite(comp_mat))",
                    "dat$CombZ <- rowMeans(comp_mat, na.rm = TRUE)",
                    "dat$CombZ[dat$n_components_present == 0L] <- NA_real_")
# call heads a frozen constant's right-hand side may contain (literals, vectors, the tribble); anything else is refused
FSB_CONSTANT_CALLS <- c("c", "::", "tibble", "tribble", "~", "-", "+", "*", "/", "(", "list", "paste", "paste0")

FSB_MEASURES <- c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation")
FSB_SEXES <- c("Female", "Male")
FSB_CCS <- paste0("CC", 1:4)
FSB_SEX_BATCHES <- list(Female = c("B3", "B4", "B6"), Male = c("B1", "B2", "B5"))
FSB_N <- list(con_cages = 3L, con_per_cage = 4L, con_per_sex = 12L, combz_animals = 117L, components = 6L,
              rfid_animals = 111L, con_rfid_animals = 24L)
FSB_TABLES <- c("F1_con_cage_means", "F1b_con_reference_means", "F2_combz_components", "F2b_combz_definition",
                "P1_posthoc_con_contrasts", "P1b_posthoc_sensitivity", "P1c_posthoc_diagnostics",
                "D1_domain_heatmap_cells", "M1_hmm_transition_difference", "M2_hmm_transition_flows",
                "M3_hmm_dwell_distribution", "M4_hmm_resolution_sensitivity",
                "E1_movement_combz_spearman_by_sex", "E2_movement_combz_fit_lines", "E2b_movement_combz_fit_coefficients",
                "E2c_movement_combz_fit_diagnostics", "T1_design_ages", "T1b_design_ages_by_animal")
FSB_GATE_FILE <- "H3_gate_results.csv"
FSB_GATE_COLS <- c("stage", "gate", "passed", "hard", "detail")
FSB_REGISTRY_COLS <- c("bundle_id", "status", "manifest_sha256", "stage29_bundle_id", "combz_table_sha256", "mmm_git_commit", "created_at")
FSB_PROVENANCE_KEYS <- c("bundle_id", "status", "generated_at", "generator", "mmm_git_commit", "mmm_branch", "stage29_bundle_id",
                         "stage29_bundle_manifest_sha256", "combz_table_sha256", "combz_definition_code", "combz_definition_code_git_blob",
                         "posthoc_run_dir", "posthoc_run_commit", "posthoc_output_manifest_sha256", "posthoc_registry_sha256",
                         "stage14_source_dir", "stage14_promotion_record", "stage14_tables", "hmm_dwell_source",
                         "stage09_table", "posthoc29c_run_dir", "posthoc29c_run_commit", "posthoc29c_output_manifest_sha256",
                         "posthoc29c_registry_sha256", "timeline_workbook", "timeline_workbook_sha256", "design_age_unit",
                         "scientific_recomputation", "descriptive_computations", "pre_write_gates_passed", "gate_results")
FSB_SCIENTIFIC_RECOMPUTATION <- paste(
  "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model.",
  "The standardised values are the stored canonical component z-scores exactly as they enter CombZ (the upstream",
  "within-sex CON standardisation is carried through verbatim, not re-derived from raw measures); CombZ is recomposed",
  "from them by the frozen rule of Analysis/build_later_outcome_combz.R and equals the stored CombZ within 1e-12.",
  "P1 / P1b / P1c are verbatim copies of the hash-gated post hoc run 29b (registry POSTHOC_CON_CONTRASTS v1.0), whose",
  "model was fitted there once; nothing is fitted, tested or adjusted in this bundle.",
  "D1 is a verbatim copy of the Stage 14 domain-heatmap source table and M1-M4 are the stored Stage 14 HMM figure tables",
  "of the run promoted 2026-10-06c (display labels joined, a rank, the Wald interval from the stored estimate and SE, and",
  "dwell-time counts in fixed bins); no model is fitted, no test run, nothing smoothed and no p or q value produced here.",
  "E1 is a verbatim copy of Stage 09's sex-stratified correlation table (rho, p and BH q computed there); E2 / E2b / E2c are",
  "verbatim copies of the hash-gated post hoc run 29c (registry POSTHOC_MOVEMENT_COMBZ_FIT v1.0), whose display line and band",
  "were fitted there once; T1 / T1b are ages by date arithmetic from the pinned planning workbook. Nothing is fitted here.")
FSB_DESCRIPTIVE_COMPUTATIONS <- c(
  "F1 cage_mean: arithmetic mean of the measure over the CON animals of one cage epoch (Batch|System|CC) in ebb_v101 B1 (values as stored); n_animals = count of those animals",
  "F1b: n and mean copied from ebb_v101 B2_descriptive_summaries (Group CON); nothing computed",
  "gate only (not exported): mean of the 3 cage means per measure x Sex x CC = the F1b / B2 CON mean within 1e-12 (equal cage sizes)",
  "F2 z = direction x signed_z (sign flip, direction -1 where the frozen definition inverts the sign after standardisation); signed_z = the stored component",
  "F2 heatmap_order_within_sex: rank of CombZ within Sex (ascending; ties by AnimalNum), a layout key; below_threshold = CombZ < the stored within-sex susceptibility_threshold",
  "gate only (not exported): CombZ recomposed from signed_z by the frozen producer rule (mean of the present components) = the stored CombZ within 1e-12",
  "P1 / P1b / P1c: verbatim text copies of the post hoc run tables estimates.csv / sensitivity_common_cage_variance.csv / diagnostics.csv (every cell as stored); source_run, source_file and source_sha256 appended; nothing computed",
  "D1: verbatim text copy of the Stage 14 table systems_sis_domain_heatmap_source_data.csv (every cell as stored; Hedges g, post hoc tests and markers computed by Stage 14); source_run, source_file and source_sha256 appended; nothing computed",
  "M1: the stored Stage 14 group mean transition probabilities and differences (systems_hmm_transition_probability_differences.csv), state display labels joined by the stored semantic label; colour_limit = max |DeltaProbability| over all rows (the figure's own symmetric limit); gate only: DeltaProbability = comparison mean - reference mean within 1e-12",
  "M2: the stored Stage 14 group mean transition probabilities (systems_hmm_transition_probability_summary.csv) with display labels; rank_within_sex_group = rank by mean_probability (descending, ties in stored row order) within Sex x Group; shown = rank <= 8, the transitions Stage 14 draws",
  "M3: number of animal x cage-change x phase x state records of the stored Stage 08 mean dwell time (hmm_state_dwell_times.csv, 10-minute HMM) per bin of mean_dwell_bins doubling from one HMM bin ([1, 2), [2, 4), ..., [64, 128] bins = 10-20 ... 640-1280 min); fraction = count / records of that Sex x Phase x Group x State; n_animals = distinct animals; counts replace Stage 14's kernel density ridges",
  "M4: the stored Stage 14 HMM-resolution sensitivity rows (systems_sis_hmm_resolution_sensitivity.csv) with wald_ci_low / wald_ci_high = mixed_model_estimate -/+ 1.96 x mixed_model_SE, the interval Stage 14 draws",
  "E1: verbatim text copy of the Stage 09 table primary_movement_entropyacf1_correlations_by_sex.csv (10min; every cell as stored); source_run, source_file and source_sha256 appended; gates only (nothing computed): the Movement_mean rows' n = the ebb_v101 A2 animals per sex, and the same Stage 09 run's per-animal input model_ladder_input.csv (pinned by the Stage 29 v1.0.1 release) = A2 Movement_mean and observed_CombZ for every animal within 1e-12",
  "E2 / E2b / E2c: verbatim text copies of the post hoc run 29c tables fit_lines.csv / coefficients.csv / diagnostics.csv (every cell as stored); source_run, source_file and source_sha256 appended; nothing computed",
  "T1b: per animal and design step, date_start / date_end (the date the workbook or the ebb_v101 cage-change time gives; shuffle stop = CC4 date + 1 day) and age_start / age_end = date - date of birth (workbook DoB), in days; gates: every age the workbook records itself (SocPref, OFT, NOR, EPM, sucrose window, Age at the end date) = the computed age; CC1 ages 22-29",
  "T1: per design step, n_animals and age_min / age_max = the minimum of age_start and the maximum of age_end over the 117 CombZ animals")
# T1 event labels are the names of the behavioural tests in the experimental design (e.g. the social preference test), not
# labels of RFID measures; the RFID display-wording rule below is not applied to them (they are fixed constants, gated).
# words never allowed in display text (DESIGN section 0; OPTION3 section 0); identifiers such as crossing_rate are not labels
FSB_FORBIDDEN_DISPLAY <- paste0("antenna|crossing|sleep|confirmatory|preregistered|approach|investigation|consumption|habituation|time near|",
                                "female-specific|sex-specific|acute|sociability|social behaviou?r|social preference|huddling")

# ---------------------------------------------------------------- small utilities
fsb_parse_mode <- function(args) s30fb_parse_mode(args)
fsb_bundle_id <- function(date, commit) {
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("Not a full git commit hash: ", commit, call. = FALSE)
  sprintf("%s_%s_%s", FSB_BUNDLE_PREFIX, format(as.Date(date), "%Y%m%d"), substr(commit, 1, 7))
}
fsb_gate_count <- function(gates) sprintf("%d of %d", sum(gates$passed), nrow(gates))
fsb_check_display <- function(x, cols, what) {
  for (k in cols) { v <- as.character(x[[k]]); bad <- !is.na(v) & grepl(FSB_FORBIDDEN_DISPLAY, v, ignore.case = TRUE)
    if (any(bad)) stop(what, ": banned display wording in ", k, ": ", paste(unique(v[bad]), collapse = " | "), call. = FALSE) }
  invisible(TRUE)
}
#' Canonical ids from a one-id-per-line list (the producer's read_ids: trim, drop blanks and a header line).
fsb_read_id_list <- function(lines) {
  x <- trimws(lines); x <- x[nzchar(x)]; x <- x[!grepl("^(id|animal|animalnum|animal_id)$", tolower(x))]
  ids <- canonical_animal_id(x)
  if (!length(ids) || anyNA(ids)) stop("Identifier list is empty or has an unreadable id.", call. = FALSE)
  unique(ids)
}
fsb_group_from_lists <- function(animal, sus, con) s30fb_group_from_lists(animal, sus, con)

# ---------------------------------------------------------------- frozen CombZ definition (parsed, never executed)
fsb_call_heads <- function(e) {
  if (!is.call(e)) return(character())
  h <- e[[1]]; head <- if (is.name(h)) as.character(h) else if (is.call(h)) as.character(h[[1]]) else ""
  c(head, if (is.call(h)) unlist(lapply(as.list(h)[-1], fsb_call_heads)), unlist(lapply(as.list(e)[-1], fsb_call_heads)))
}
fsb_expr_text <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")

#' Read the frozen CombZ definition from the producer source WITHOUT running it: every top-level assignment to one of
#' FSB_COMBZ_CONSTANTS (exactly one each; right-hand side limited to FSB_CONSTANT_CALLS) is evaluated in an empty
#' environment, and the four composite-rule expressions (FSB_COMBZ_RULE, exactly once each, in order) are returned
#' unevaluated. Returns list(constants = <list>, rule = <expressions>, n_expressions).
fsb_read_combz_definition <- function(path) {
  ex <- parse(file = path, keep.source = FALSE)
  is_assign <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]), TRUE)
  lhs <- rep(NA_character_, length(ex)); lhs[is_assign] <- vapply(ex[is_assign], function(e) as.character(e[[2]]), "")
  env <- new.env(parent = baseenv())
  for (k in FSB_COMBZ_CONSTANTS) {
    i <- which(lhs == k)
    if (length(i) != 1L) stop("Frozen CombZ definition: expected exactly one top-level assignment to ", k, " (found ", length(i), ").", call. = FALSE)
    bad <- setdiff(fsb_call_heads(ex[[i]][[3]]), FSB_CONSTANT_CALLS)
    if (length(bad)) stop("Frozen CombZ definition: ", k, " calls ", paste(bad, collapse = ","), "; refusing to evaluate it.", call. = FALSE)
    assign(k, eval(ex[[i]][[3]], envir = env), envir = env)
  }
  txt <- vapply(ex, fsb_expr_text, "")
  want <- vapply(FSB_COMBZ_RULE, function(s) fsb_expr_text(parse(text = s, keep.source = FALSE)[[1]]), "", USE.NAMES = FALSE)
  pos <- vapply(want, function(w) { i <- which(txt == w); if (length(i) != 1L) NA_integer_ else i }, 1L)
  if (anyNA(pos)) stop("Frozen CombZ definition: composite rule line(s) not found exactly once: ", paste(FSB_COMBZ_RULE[is.na(pos)], collapse = " | "), call. = FALSE)
  if (is.unsorted(pos, strictly = TRUE) || any(diff(pos) != 1L)) stop("Frozen CombZ definition: the composite rule lines are not consecutive and in order.", call. = FALSE)
  cst <- mget(FSB_COMBZ_CONSTANTS, envir = env)
  meta <- as.data.frame(cst$COMBZ_COMPONENT_META, stringsAsFactors = FALSE)
  if (!identical(as.character(meta$component), as.character(cst$COMBZ_COMPONENTS)) || !is.logical(meta$sign_inverted) || anyNA(meta$sign_inverted))
    stop("Frozen CombZ definition: COMBZ_COMPONENT_META does not list COMBZ_COMPONENTS in order with a logical sign_inverted.", call. = FALSE)
  cst$COMBZ_COMPONENT_META <- meta
  list(constants = cst, rule = ex[pos], n_expressions = length(ex))
}

#' CombZ and n_components_present from a signed-z matrix by the frozen producer rule (its own expressions, evaluated in an
#' empty environment holding only `dat` and COMBZ_COMPONENTS).
fsb_combz_recompose <- function(defn, signed) {
  comps <- defn$constants$COMBZ_COMPONENTS
  env <- new.env(parent = baseenv())
  env$dat <- as.data.frame(signed[, comps, drop = FALSE]); env$COMBZ_COMPONENTS <- comps
  for (e in defn$rule) eval(e, envir = env)
  data.frame(CombZ = env$dat$CombZ, n_components_present = as.integer(env$dat$n_components_present))
}

#' Gate rows: the canonical combz_component_definition.csv agrees with the constants parsed from the producer.
fsb_component_definition_gates <- function(defn, compdef) {
  cd <- data.table::as.data.table(compdef); meta <- defn$constants$COMBZ_COMPONENT_META; k <- length(defn$constants$COMBZ_COMPONENTS)
  g <- function(name, ok, detail = "") s30fb_gate_row("3-definition", name, ok, detail)
  rbind(
    g("combz_component_definition.csv components = producer COMBZ_COMPONENTS (same order)", identical(as.character(cd$component), as.character(meta$component)),
      paste(cd$component, collapse = ",")),
    g("combz_component_definition.csv sign_inverted = producer COMBZ_COMPONENT_META", identical(as.logical(cd$sign_inverted), meta$sign_inverted),
      paste(cd$sign_inverted, collapse = ",")),
    g("combz_component_definition.csv xlsx_column / raw_measure / higher_means = producer COMBZ_COMPONENT_META",
      identical(as.character(cd$xlsx_column), as.character(meta$xlsx_column)) && identical(as.character(cd$raw_measure), as.character(meta$raw_measure)) &&
        identical(as.character(cd$higher_means), as.character(meta$higher_means))),
    g("combz_component_definition.csv weight_in_composite = 1 / number of components", isTRUE(all(abs(as.numeric(cd$weight_in_composite) - 1 / k) <= FSB_TOL))),
    g("combz_component_definition.csv combz_definition_id / reference_population_id = producer constants",
      all(cd$combz_definition_id == defn$constants$COMBZ_DEFINITION_ID) && all(cd$reference_population_id == defn$constants$COMBZ_REFERENCE_POP_ID)),
    g("combz_component_definition.csv: component standardisation not reproducible in the repository (recorded, carried verbatim)",
      all(!as.logical(cd$reproducible_in_repository))))
}

# ---------------------------------------------------------------- F1 / F1b: CON cage means and the CON reference mean
#' Metric display label / unit / tier copied from the ebb_v101 configuration `metrics`.
fsb_metric_labels <- function(cfg_metrics, measures = FSB_MEASURES) {
  out <- data.table::rbindlist(lapply(measures, function(m) { x <- cfg_metrics[[m]]
    if (is.null(x$label) || is.null(x$unit) || is.null(x$tier)) stop("ebb_v101 config has no label / unit / tier for ", m, call. = FALSE)
    data.table::data.table(measure = m, measure_label = x$label, unit = x$unit, tier = x$tier) }))
  fsb_check_display(out, c("measure_label", "unit"), "metric labels")
  out[]
}

#' F1: one row per measure x Sex x CC x CON cage epoch; cage_mean = mean over the cage's CON animals (B1 values as stored).
#' Stops on an NA value, a missing measure column or a CON animal without a cage epoch.
fsb_con_cage_means <- function(b1, labels, measures = FSB_MEASURES) {
  b1 <- data.table::as.data.table(b1); miss <- setdiff(c("AnimalNum", "Sex", "Batch", "Group", "CC", "CageEpisodeID", "n_in_cage", measures), names(b1))
  if (length(miss)) stop("B1 lacks column(s): ", paste(miss, collapse = ","), call. = FALSE)
  con <- b1[Group == "CON"]
  if (!nrow(con) || anyNA(con$CageEpisodeID) || any(!nzchar(con$CageEpisodeID))) stop("B1: CON rows missing or without a cage epoch.", call. = FALSE)
  out <- data.table::rbindlist(lapply(measures, function(m) {
    v <- con[[m]]; if (anyNA(v) || !all(is.finite(v))) stop("B1: non-finite CON value in ", m, call. = FALSE)
    con[, .(Group = "CON", n_animals = .N, n_in_cage = paste(sort(unique(n_in_cage)), collapse = ";"), animals = paste(sort(AnimalNum), collapse = ";"),
            cage_mean = mean(get(m))), by = .(Sex, CC, Batch, CageEpisodeID)][, measure := m]
  }))
  out <- merge(out, labels, by = "measure", all.x = TRUE, sort = FALSE)
  if (anyNA(out$measure_label)) stop("F1: no display label for a measure.", call. = FALSE)
  out[, measure := factor(measure, levels = measures)]
  data.table::setorderv(out, c("measure", "Sex", "CC", "Batch")); out[, measure := as.character(measure)]
  out[, .(measure, measure_label, unit, tier, Sex, CC, Batch, CageEpisodeID, Group, n_animals, n_in_cage, animals, cage_mean)]
}

#' F1b: the CON descriptive mean (n, mean) per measure x Sex x CC, copied from ebb_v101 B2.
fsb_con_reference_means <- function(b2, labels, measures = FSB_MEASURES) {
  b2 <- data.table::as.data.table(b2)
  x <- b2[Group == "CON" & construct %in% measures, .(measure = construct, Sex, CC, Group, n = as.integer(n), mean = as.numeric(mean))]
  if (anyDuplicated(x[, .(measure, Sex, CC)])) stop("B2: duplicated CON row.", call. = FALSE)
  x <- merge(x, labels, by = "measure", all.x = TRUE, sort = FALSE)
  x[, measure := factor(measure, levels = measures)]; data.table::setorderv(x, c("measure", "Sex", "CC")); x[, measure := as.character(measure)]
  x[, source := "copied: ebb_v101 B2_descriptive_summaries.csv (Group CON; construct, Sex, CC, n, mean)"]
  x[, .(measure, measure_label, unit, tier, Sex, CC, Group, n, mean, source)]
}

#' One check row per measure x Sex x CC: cage count, cage sizes, batches, and mean of cage means vs the B2 CON mean.
fsb_con_cage_checks <- function(f1, f1b, sex_batches = FSB_SEX_BATCHES, cages_expected = FSB_N$con_cages, per_cage_expected = FSB_N$con_per_cage) {
  f1 <- data.table::as.data.table(f1); f1b <- data.table::as.data.table(f1b)
  ck <- f1[, .(n_cages = .N, n_cage_epochs = data.table::uniqueN(CageEpisodeID), cage_sizes = paste(n_animals, collapse = ";"),
               n_in_cage = paste(unique(n_in_cage), collapse = ";"), batches = paste(sort(Batch), collapse = ";"),
               n_total = sum(n_animals), mean_of_cage_means = mean(cage_mean)), by = .(measure, Sex, CC)]
  ck <- merge(ck, f1b[, .(measure, Sex, CC, b2_n = n, b2_mean = mean)], by = c("measure", "Sex", "CC"), all = TRUE)
  ck[, abs_diff := abs(mean_of_cage_means - b2_mean)]
  ck[, expected_batches := vapply(Sex, function(s) paste(sort(sex_batches[[s]]), collapse = ";"), "")]
  ncx <- as.integer(cages_expected); npx <- as.integer(per_cage_expected)
  ck[, ok_cages := !is.na(n_cages) & n_cages == ncx & n_cage_epochs == ncx]
  ck[, ok_sizes := !is.na(cage_sizes) & vapply(strsplit(ifelse(is.na(cage_sizes), "", cage_sizes), ";"), function(z) length(z) > 0L && all(as.integer(z) == npx), TRUE) &
                   !is.na(n_in_cage) & n_in_cage == as.character(npx)]
  ck[, ok_batches := !is.na(batches) & batches == expected_batches]
  ck[, ok_n := !is.na(b2_n) & n_total == b2_n]
  ck[, ok_mean := !is.na(abs_diff) & abs_diff <= FSB_TOL]
  ck[]
}

#' Composition checks on B1: every CON cage epoch holds only CON animals; each CON group keeps the same animals at every CC.
fsb_con_composition <- function(b1) {
  b1 <- data.table::as.data.table(b1); cages <- unique(b1[Group == "CON", CageEpisodeID])
  mixed <- b1[CageEpisodeID %in% cages & Group != "CON", unique(CageEpisodeID)]
  sets <- b1[Group == "CON", .(animals = paste(sort(AnimalNum), collapse = ";")), by = .(Sex, Batch, CC)]
  stable <- sets[, .(n_sets = data.table::uniqueN(animals), n_cc = .N), by = .(Sex, Batch)]
  list(mixed_cages = mixed, stable = stable, ok_pure = !length(mixed), ok_stable = all(stable$n_sets == 1L))
}

# ---------------------------------------------------------------- F2 / F2b: CombZ components as they enter CombZ
#' F2: 117 animals x 6 components from the canonical CombZ table. signed_z = the stored component (as it enters CombZ);
#' direction = -1 where the frozen definition inverts the sign after standardisation; z = direction x signed_z;
#' raw_value / reference_mean / reference_sd NA (the upstream standardisation is not re-derivable in the repository).
#' Stops unless CombZ recomposed from signed_z by the frozen rule equals the stored CombZ within `tol` for every animal
#' and n_components_present matches. `thresholds` = canonical combz_classification_thresholds (Sex, susceptibility_threshold).
fsb_combz_components <- function(cz, defn, thresholds, n_expected = FSB_N$combz_animals, tol = FSB_TOL) {
  cz <- data.table::as.data.table(cz); cst <- defn$constants; comps <- cst$COMBZ_COMPONENTS; meta <- cst$COMBZ_COMPONENT_META
  need <- c("AnimalNum", "Sex", "Batch", "outcome_group", comps, "n_components_present", "CombZ", "combz_definition_id")
  if (length(setdiff(need, names(cz)))) stop("CombZ table lacks column(s): ", paste(setdiff(need, names(cz)), collapse = ","), call. = FALSE)
  if (nrow(cz) != n_expected || anyDuplicated(cz$AnimalNum)) stop("CombZ table: expected ", n_expected, " unique animals (found ", nrow(cz), ").", call. = FALSE)
  if (!all(cz$combz_definition_id == cst$COMBZ_DEFINITION_ID)) stop("CombZ table combz_definition_id differs from the frozen definition.", call. = FALSE)
  S <- as.data.frame(lapply(cz[, comps, with = FALSE], as.numeric)); names(S) <- comps
  re <- fsb_combz_recompose(defn, S)
  stored <- as.numeric(cz$CombZ)
  if (!identical(is.na(re$CombZ), is.na(stored))) stop("Recomposed CombZ NA pattern differs from the stored CombZ.", call. = FALSE)
  d <- abs(re$CombZ - stored); dmax <- if (any(!is.na(d))) max(d, na.rm = TRUE) else 0
  if (dmax > tol) stop(sprintf("CombZ recomposed from signed_z by the frozen rule differs from the stored CombZ (max |diff| %.3g > %g): %s",
                               dmax, tol, paste(cz$AnimalNum[!is.na(d) & d > tol], collapse = ",")), call. = FALSE)
  np_mis <- sum(re$n_components_present != as.integer(cz$n_components_present))
  if (np_mis) stop("n_components_present differs from the frozen rule for ", np_mis, " animal(s).", call. = FALSE)
  thr <- data.table::as.data.table(thresholds)
  if (!setequal(thr$Sex, unique(cz$Sex)) || anyDuplicated(thr$Sex)) stop("Thresholds: need exactly one row per Sex.", call. = FALSE)
  th <- as.numeric(thr$susceptibility_threshold[match(cz$Sex, thr$Sex)])
  base <- data.table::data.table(AnimalNum = cz$AnimalNum, Sex = cz$Sex, Batch = cz$Batch, Group = cz$outcome_group,
                                 n_components_present = as.integer(cz$n_components_present), CombZ = stored, susceptibility_threshold = th,
                                 below_threshold = stored < th)
  base[, heatmap_order_within_sex := data.table::frank(data.table::data.table(CombZ, AnimalNum), ties.method = "first"), by = Sex]
  dirs <- ifelse(meta$sign_inverted, -1L, 1L)
  out <- data.table::rbindlist(lapply(seq_along(comps), function(j) {
    sz <- S[[comps[j]]]
    cbind(base[, .(AnimalNum, Sex, Batch, Group)],
          data.table::data.table(component = comps[j], component_order = j, raw_value = NA_real_, reference_mean = NA_real_, reference_sd = NA_real_,
                                 z = dirs[j] * sz, direction = dirs[j], signed_z = sz, present = is.finite(sz)),
          base[, .(n_components_present, CombZ, susceptibility_threshold, below_threshold, heatmap_order_within_sex)])
  }))
  data.table::setorderv(out, c("Sex", "heatmap_order_within_sex", "component_order"))
  data.table::setattr(out, "recompose_max_abs_diff", dmax)
  data.table::setattr(out, "n_present_mismatch", np_mis)
  out[]
}

#' F2b: one row per component: sources, standardisation as implemented, direction and why, relationship to CombZ.
fsb_combz_definition_table <- function(defn, compdef, f2, combz_table_path, combz_table_sha256) {
  cst <- defn$constants; meta <- cst$COMBZ_COMPONENT_META; cd <- data.table::as.data.table(compdef); f2 <- data.table::as.data.table(f2)
  k <- length(cst$COMBZ_COMPONENTS); n_all <- data.table::uniqueN(f2$AnimalNum)
  np <- f2[, .(n_present = sum(present), n_missing = sum(!present), missing_animals = paste(sort(AnimalNum[!present]), collapse = ";")), by = component]
  np <- np[match(meta$component, component)]
  n5 <- f2[, .(n = n_components_present[1]), by = AnimalNum][, table(n)]
  # the corrections are applied in the corrected workbook, not by the producer (version 2)
  src <- paste0(" (in the corrected workbook; upstream correction ", cst$COMBZ_CORRECTION_ID, "; ", FSB_COMBZ_CODE$doc, " sections 4b-4c)")
  organ <- "damaged, missing or blank organ weights carry no score and leave the CON reference (2026-10-16)"
  corr <- c(NOR = paste0("batch 1 values are the analysed SLEAP D2 (2026-10-16); 00317's value is missing until its novel-object video is re-tracked", src),  # RETRACK-PIN
            sucrose_pref = paste0("rebuilt keyed by animal, not by row position, with negative bottle readings counted as zero (2026-09-20)", src),
            delta_cort = paste0("OR620's value from the linear map every other batch-5 male follows (2026-09-20); the female sex-pooled reference rebuilt (2026-10-16)", src),
            adrenal_weight = paste0(organ, src), spleen_weight = paste0(organ, src))
  out <- data.table::data.table(
    component = meta$component, component_order = seq_len(k), display_label = meta$raw_measure, domain = meta$domain,
    source_table = combz_table_path, source_table_sha256 = combz_table_sha256, source_column = meta$component,
    upstream_source = paste0("E9_Behavior_Data.xlsx (pinned corrected workbook) :: sheet combz_canonical column ", meta$xlsx_column),
    raw_measure = meta$raw_measure, raw_derivation = meta$raw_derivation,
    standardisation = "z = (x - mean(x | reference)) / populationSD(x | reference), computed upstream in the corrected endpoint workbook",
    reference_population = paste0("the within-sex CON animals (12 per sex) with a valid value for the component, as computed in the corrected ",
                                  "workbook (", FSB_COMBZ_CODE$doc, " sections 4a and 4c); reference_population_id ", cst$COMBZ_REFERENCE_POP_ID),
    sd_convention = as.character(cd$sd_convention[match(meta$component, cd$component)]),
    standardisation_reproducible_in_repository = as.logical(cd$reproducible_in_repository[match(meta$component, cd$component)]),
    raw_value_reference_columns = "NA in F2: the producer carries the stored z-scores verbatim; re-deriving raw -> z would change the endpoint",
    reproducibility_note = as.character(cd$reproducibility_note[match(meta$component, cd$component)]),
    sign_inverted = meta$sign_inverted, direction = ifelse(meta$sign_inverted, -1L, 1L),
    direction_reason = ifelse(meta$sign_inverted,
                              paste0("multiplied by -1 after standardisation in the frozen definition, so that a higher signed_z means ", meta$higher_means,
                                     " (every component points the same way: higher = more resilient-like)"),
                              paste0("not inverted in the frozen definition: a higher ", meta$raw_measure, " already means ", meta$higher_means,
                                     " (higher = more resilient-like)")),
    higher_signed_z_means = meta$higher_means,
    weight_in_composite = as.numeric(cd$weight_in_composite[match(meta$component, cd$component)]),
    relationship_to_combz = paste0("signed_z (= direction x z) enters CombZ unchanged: CombZ = unweighted mean of the present components' signed_z ",
                                   "(Excel AVERAGE semantics: a missing component is ignored, not zero); ",
                                   paste(sprintf("%d animals with %s of %d present", as.integer(n5), names(n5), k), collapse = ", "),
                                   "; higher CombZ = more resilient-like = lower later stress burden"),
    upstream_correction = ifelse(meta$component %in% names(corr), unname(corr[meta$component]), "none"),
    n_animals = n_all, n_present = np$n_present, n_missing = np$n_missing, missing_animals = np$missing_animals,
    combz_definition_id = cst$COMBZ_DEFINITION_ID, reference_population_id = cst$COMBZ_REFERENCE_POP_ID,
    definition_code = paste0(FSB_COMBZ_CODE$path, " (git blob ", FSB_COMBZ_CODE$git_blob, "): COMBZ_COMPONENT_META and the composite rule"))
  fsb_check_display(out, c("display_label", "domain", "raw_measure", "raw_derivation", "direction_reason", "higher_signed_z_means", "relationship_to_combz",
                           "upstream_correction", "reference_population"), "F2b_combz_definition")
  out[]
}

# ---------------------------------------------------------------- P1 / P1b / P1c: the post hoc run 29b (copied verbatim)
#' Frozen post hoc run: folder name, audit/output_manifest.csv hash, its files (bytes, SHA-256; CR stripped), no extra file,
#' every file read-only, README commit / registry lines, run_manifest (REAL, commit, registry, status counts). Never stops.
#' `tables` = the run files the bundle copies; `stage` = the gate-stage label (29b: 1b-posthoc; v4 29c: 1e-posthoc29c).
fsb_verify_posthoc_run <- function(run_dir, expect = FSB_POSTHOC, tables = FSB_POSTHOC_TABLES, stage = "1b-posthoc") {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row(stage, name, passed, detail)
  mf <- file.path(run_dir, "audit", "output_manifest.csv")
  add(sprintf("post hoc run folder = pinned run (%s/%s)", expect$stage_dir, expect$run_name),
      identical(basename(run_dir), expect$run_name) && identical(basename(dirname(run_dir)), expect$stage_dir), run_dir)
  if (!file.exists(mf)) { add("audit/output_manifest.csv exists", FALSE, mf); return(list(gates = data.table::rbindlist(g))) }
  msha <- s30fb_sha(mf)
  add("audit/output_manifest.csv SHA-256 = pinned", identical(msha, expect$output_manifest_sha256), msha)
  man <- data.table::fread(mf, colClasses = "character")
  for (k in names(man)) data.table::set(man, j = k, value = s30fb_strip_cr(man[[k]]))
  chk <- s30fb_manifest_check(run_dir, man)
  add(sprintf("output manifest lists %d files, among them the %d tables copied", expect$output_manifest_n, length(tables)),
      nrow(man) == expect$output_manifest_n && all(tables %in% man$file), nrow(man))
  add("every manifest file: bytes and SHA-256 match", nrow(chk) > 0L && all(chk$ok), paste(chk[ok == FALSE, file], collapse = ","))
  extra <- setdiff(s30fb_files_on_disk(run_dir), c(chk$file, "audit/output_manifest.csv"))
  add("no file in the run folder outside the manifest", !length(extra), paste(extra, collapse = ","))
  fl <- list.files(run_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  add("every run file is read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), run_dir)
  rd <- if (file.exists(file.path(run_dir, "README.txt"))) s30fb_strip_cr(readLines(file.path(run_dir, "README.txt"), warn = FALSE, encoding = "UTF-8")) else character()
  add("README commit = post hoc run commit", any(rd == paste("commit", expect$run_commit)), grep("^commit ", rd, value = TRUE))
  add("README registry sha256 = frozen post hoc registry", any(rd == paste("registry sha256", expect$registry_sha256)), grep("^registry sha256 ", rd, value = TRUE))
  rp <- file.path(run_dir, "audit", "run_manifest.csv")
  run <- if (file.exists(rp)) data.table::fread(rp, colClasses = "character") else data.table::data.table()
  if (nrow(run)) for (k in names(run)) data.table::set(run, j = k, value = s30fb_strip_cr(run[[k]]))
  add("run_manifest: REAL at the run commit with the frozen registry, fitted once",
      nrow(run) == 1L && identical(run$mode, "REAL") && identical(run$git_commit, expect$run_commit) && identical(run$registry_sha256, expect$registry_sha256) &&
        identical(run$registry_version, expect$registry_version), paste(run$mode, run$git_commit, run$registry_sha256))
  add("run_manifest: status counts = pinned (no FAILED fit)", nrow(run) == 1L && identical(run$status_counts, expect$status_counts) &&
        (!("n_failed" %in% names(run)) || identical(run$n_failed, "0")), paste(c(run$status_counts, run$n_failed), collapse = " "))
  list(gates = data.table::rbindlist(g), check = chk, manifest = man, manifest_sha256 = msha, run = run)
}

#' One post hoc table copied verbatim: every cell read as text (blank = NA, "" kept), then source_run / source_file /
#' source_sha256 appended. Stops unless the file's SHA-256 equals `sha256` (its output-manifest entry) at the time of the read.
fsb_posthoc_copy <- function(run_dir, rel, sha256, run_label) {
  p <- file.path(run_dir, rel)
  if (!identical(s30fb_sha(p), sha256)) stop("Post hoc table ", rel, " differs from its output-manifest SHA-256.", call. = FALSE)
  x <- data.table::fread(p, colClasses = "character", na.strings = "", encoding = "UTF-8")
  if (length(intersect(FSB_POSTHOC_ADDED, names(x)))) stop("Post hoc table ", rel, " already has a provenance column.", call. = FALSE)
  x[, (FSB_POSTHOC_ADDED) := list(run_label, rel, sha256)]
  x[]
}

#' Gate rows on the three copied tables: row counts, the registry identity and tier on every row, six estimands per
#' measure x sex, Holm m = 3 on the tested rows, the display text free of banned wording, and the copy = an independent
#' re-read of the source (every cell, as text).
fsb_posthoc_gates <- function(P, run_dir, expect = FSB_POSTHOC) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("6-P", name, passed, detail)
  add("P1 / P1b / P1c present with the declared row counts (24, 24, 8)", setequal(names(P), names(FSB_POSTHOC_TABLES)) &&
        all(vapply(names(FSB_POSTHOC_ROWS), function(k) !is.null(P[[k]]) && nrow(P[[k]]) == FSB_POSTHOC_ROWS[[k]], TRUE)),
      paste(vapply(P, nrow, 1L), collapse = ","))
  for (k in names(P)) {
    src <- data.table::fread(file.path(run_dir, FSB_POSTHOC_TABLES[[k]]), colClasses = "character", na.strings = "", encoding = "UTF-8")
    cp <- P[[k]][, setdiff(names(P[[k]]), FSB_POSTHOC_ADDED), with = FALSE]
    add(paste0(k, ": every source cell carried verbatim (text) in source column order"), identical(names(cp), names(src)) && identical(as.list(cp), as.list(src)))
  }
  p1 <- P$P1_posthoc_con_contrasts; p1b <- P$P1b_posthoc_sensitivity
  add("P1 / P1b: registry sha256, version and tier on every row = the frozen post hoc registry",
      all(c(p1$registry_sha256, p1b$registry_sha256) == expect$registry_sha256) && all(c(p1$registry_version, p1b$registry_version) == expect$registry_version) &&
        all(c(p1$tier, p1b$tier) == expect$tier))
  add("P1: six estimands (3 means, 3 contrasts) per measure x sex; Holm m = 3 on the tested rows",
      nrow(unique(p1[, .(measure, Sex)])) == 4L && all(p1[, .N, by = .(measure, Sex)]$N == 6L) &&
        all(p1[tested == "TRUE", .N, by = .(measure, Sex)]$N == 3L) && all(p1[tested == "TRUE", holm_m] == "3") && all(is.na(p1[tested == "FALSE", p_holm])))
  add("P1: primary model rows only; P1b: sensitivity rows only",
      all(p1$model == "primary_separate_cage_variances") && all(p1b$model == "sensitivity_common_cage_variance"))
  add("P1 / P1b display text free of banned wording", {
    fsb_check_display(p1, FSB_POSTHOC_DISPLAY_COLS, "P1"); fsb_check_display(p1b, FSB_POSTHOC_DISPLAY_COLS, "P1b"); TRUE })
  data.table::rbindlist(g)
}

#' Post-write gate: every line of a written verbatim copy (P1-P1c, D1) = the source line + "," + the appended provenance
#' cells (byte-level verbatim copy of every source cell). `verbatim` = named character (bundle table -> source file path).
fsb_verbatim_gates <- function(bd, verbatim) {
  if (!length(verbatim)) return(data.table::data.table(stage = character(), gate = character(), passed = logical(), hard = logical(), detail = character()))
  data.table::rbindlist(lapply(names(verbatim), function(k) {
    a <- readLines(file.path(bd, paste0(k, ".csv")), warn = FALSE, encoding = "UTF-8")
    b <- s30fb_strip_cr(readLines(verbatim[[k]], warn = FALSE, encoding = "UTF-8"))
    ok <- length(a) == length(b) && length(a) > 1L && all(startsWith(a, paste0(b, ",")))
    s30fb_gate_row("7-write", paste0(k, ".csv: every line = the source line + the appended provenance cells"), ok,
                   paste(length(a), "lines vs", length(b)))
  }))
}

# ---------------------------------------------------------------- D1 / M1-M4 (v3): the Stage 14 display tables
#' Gate rows on the Stage 14 inputs: each file's on-disk SHA-256 = the pinned value, and the Stage 14 promotion record
#' lists the file with that SHA-256 (so the copy is the promoted state). `record` = the record table. Never stops.
fsb_verify_stage14_inputs <- function(root14, record, pin = FSB_STAGE14) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("1c-stage14", name, passed, detail)
  rec <- data.table::as.data.table(record)
  for (k in names(pin$files)) {
    p <- file.path(root14, pin$files[[k]]); now <- if (file.exists(p)) s30fb_sha(p) else NA_character_
    add(paste0(k, ": on-disk SHA-256 = pinned"), identical(now, pin$sha256[[k]]), paste(p, now))
    r <- rec[rec$target_root_rel == pin$root_rel & rec$target_file == pin$files[[k]]]
    add(paste0(k, ": the Stage 14 promotion record lists this file with this SHA-256"), nrow(r) == 1L && identical(r$sha256, pin$sha256[[k]]),
        paste(nrow(r), "row(s);", paste(r$change, collapse = ",")))
  }
  data.table::rbindlist(g)
}

#' The display label of each HMM state at one resolution, keyed by the semantic StateLabel Stage 14 writes into its
#' transition tables. Stops unless each state is present once with a unique semantic and display label.
fsb_hmm_state_labels <- function(labels, res = FSB_HMM_RESOLUTION) {
  x <- data.table::as.data.table(labels)
  need <- c("resolution", "State", "StateLabel", "SemanticState", "DisplayState")
  if (length(setdiff(need, names(x)))) stop("State label table lacks column(s): ", paste(setdiff(need, names(x)), collapse = ","), call. = FALSE)
  x <- x[x$resolution == res]; x[, State := as.character(State)]
  if (nrow(x) != length(FSB_HMM_STATES) || !setequal(x$State, FSB_HMM_STATES) || anyDuplicated(x$StateLabel) || anyDuplicated(x$DisplayState))
    stop("State label table: expected states ", paste(FSB_HMM_STATES, collapse = ","), " once each with unique labels at ", res, call. = FALSE)
  fsb_check_display(x, "DisplayState", "HMM state display labels")
  x[order(as.integer(State)), .(State, StateLabel, SemanticState, DisplayState)]
}
fsb_hmm_index <- function(label, lab) {
  i <- match(label, lab$StateLabel)
  if (anyNA(i)) stop("HMM label(s) without a state: ", paste(unique(label[is.na(i)]), collapse = " | "), call. = FALSE)
  i
}

#' M1: the stored Stage 14 transition-probability differences with display labels and the figure's own colour limit.
#' Stops unless every row is finite, the contrasts are the stored three, every Sex x From x To x Contrast occurs once, and
#' DeltaProbability = comparison mean - reference mean within `tol` (attribute max_abs_recon_diff).
fsb_hmm_transition_difference <- function(diffs, lab, tol = FSB_TOL) {
  d <- data.table::as.data.table(diffs)
  need <- c("Sex", "FromLabel", "ToLabel", FSB_HMM_GROUPS, "Contrast", "DeltaProbability")
  if (length(setdiff(need, names(d)))) stop("Transition differences lack column(s): ", paste(setdiff(need, names(d)), collapse = ","), call. = FALSE)
  if (!setequal(unique(d$Contrast), FSB_HMM_CONTRASTS)) stop("Transition differences: contrasts are not ", paste(FSB_HMM_CONTRASTS, collapse = ", "), call. = FALSE)
  fi <- fsb_hmm_index(d$FromLabel, lab); ti <- fsb_hmm_index(d$ToLabel, lab)
  M <- sapply(FSB_HMM_GROUPS, function(g) as.numeric(d[[g]])); delta <- as.numeric(d$DeltaProbability)
  if (!all(is.finite(M)) || !all(is.finite(delta))) stop("Transition differences: a non-finite mean or difference.", call. = FALSE)
  rows <- seq_len(nrow(d)); comp <- match(sub(" - .*$", "", d$Contrast), FSB_HMM_GROUPS); ref <- match(sub("^.* - ", "", d$Contrast), FSB_HMM_GROUPS)
  recon <- abs(M[cbind(rows, comp)] - M[cbind(rows, ref)] - delta)
  if (max(recon) > tol) stop(sprintf("Transition differences: DeltaProbability differs from the stored means (max %.3g).", max(recon)), call. = FALSE)
  out <- data.table::data.table(Sex = d$Sex, from_state = lab$State[fi], to_state = lab$State[ti], from_label = lab$DisplayState[fi],
                                to_label = lab$DisplayState[ti], CON = M[, "CON"], RES = M[, "RES"], SUS = M[, "SUS"], contrast = d$Contrast,
                                delta_probability = delta, colour_limit = max(abs(delta)), resolution = FSB_HMM_RESOLUTION,
                                source_from_label = d$FromLabel, source_to_label = d$ToLabel)
  if (anyDuplicated(out[, .(Sex, from_state, to_state, contrast)])) stop("Transition differences: a Sex x From x To x Contrast row occurs twice.", call. = FALSE)
  out[, contrast := factor(contrast, levels = FSB_HMM_CONTRASTS)]
  data.table::setorderv(out, c("Sex", "contrast", "from_state", "to_state")); out[, contrast := as.character(contrast)]
  data.table::setattr(out, "max_abs_recon_diff", max(recon))
  out[]
}

#' M2: the stored Stage 14 mean transition probabilities with display labels, their rank within Sex x Group (descending,
#' ties in stored row order) and shown = rank <= n_top, the transitions the Stage 14 state-flow figure draws.
fsb_hmm_transition_flows <- function(means, lab, n_top = FSB_HMM_FLOW_TOP) {
  m <- data.table::as.data.table(means)
  need <- c("Sex", "Group", "FromLabel", "ToLabel", "mean_probability")
  if (length(setdiff(need, names(m)))) stop("Transition means lack column(s): ", paste(setdiff(need, names(m)), collapse = ","), call. = FALSE)
  p <- as.numeric(m$mean_probability); if (!all(is.finite(p))) stop("Transition means: a non-finite mean.", call. = FALSE)
  fi <- fsb_hmm_index(m$FromLabel, lab); ti <- fsb_hmm_index(m$ToLabel, lab)
  out <- data.table::data.table(Sex = m$Sex, Group = m$Group, from_state = lab$State[fi], to_state = lab$State[ti], from_label = lab$DisplayState[fi],
                                to_label = lab$DisplayState[ti], mean_probability = p, stored_row = seq_len(nrow(m)), resolution = FSB_HMM_RESOLUTION,
                                source_from_label = m$FromLabel, source_to_label = m$ToLabel)
  if (anyDuplicated(out[, .(Sex, Group, from_state, to_state)])) stop("Transition means: a Sex x Group x From x To row occurs twice.", call. = FALSE)
  data.table::setorderv(out, c("Sex", "Group", "stored_row"))
  out[, rank_within_sex_group := order(order(-mean_probability, stored_row)), by = .(Sex, Group)]
  out[, shown := rank_within_sex_group <= n_top]
  data.table::setorderv(out, c("Sex", "Group", "rank_within_sex_group"))
  out[, .(Sex, Group, from_state, to_state, from_label, to_label, mean_probability, rank_within_sex_group, shown, resolution,
          source_from_label, source_to_label, stored_row)]
}

#' M3: records of the stored mean dwell time per bin of mean_dwell_bins doubling from one HMM bin, for every
#' Sex x Phase x Group x State (a complete grid, zero counts kept); fraction = count / records of that group.
#' Stops on a record outside the bins, a missing value, another HMM resolution or bin size, or an unknown state.
fsb_hmm_dwell_distribution <- function(dwell, lab, edges = FSB_DWELL_EDGES_BINS, pin = FSB_HMM_DWELL, res = FSB_HMM_RESOLUTION) {
  x <- data.table::copy(data.table::as.data.table(dwell))
  need <- c("BinLevel", "Group", "Sex", "Phase", "CageChange", "AnimalNum", "State", "mean_dwell_bins", "bin_size_sec")
  if (length(setdiff(need, names(x)))) stop("Dwell-time table lacks column(s): ", paste(setdiff(need, names(x)), collapse = ","), call. = FALSE)
  if (!all(x$BinLevel == res) || !all(as.numeric(x$bin_size_sec) == pin$bin_size_sec)) stop("Dwell-time table: not the ", res, " HMM (bin size ", pin$bin_size_sec, " s).", call. = FALSE)
  v <- as.numeric(x$mean_dwell_bins)
  if (!all(is.finite(v)) || any(v < min(edges) | v > max(edges))) stop("Dwell-time table: a mean dwell outside [", min(edges), ", ", max(edges), "] bins.", call. = FALSE)
  if (!all(as.character(x$State) %in% lab$State) || !all(x$Group %in% FSB_HMM_GROUPS) || !all(x$Phase %in% names(FSB_HMM_PHASE_LABELS)))
    stop("Dwell-time table: an unknown state, group or phase.", call. = FALSE)
  nb <- length(edges) - 1L
  x[, `:=`(State = as.character(State), bin = findInterval(v, edges, rightmost.closed = TRUE))]
  grid <- data.table::CJ(Sex = sort(unique(x$Sex)), Phase = names(FSB_HMM_PHASE_LABELS), Group = FSB_HMM_GROUPS, State = lab$State, bin = seq_len(nb))
  cnt <- x[, .(n_records = .N), by = .(Sex, Phase, Group, State, bin)]
  tot <- x[, .(n_records_group = .N, n_animals = data.table::uniqueN(AnimalNum)), by = .(Sex, Phase, Group, State)]
  out <- merge(merge(grid, cnt, by = c("Sex", "Phase", "Group", "State", "bin"), all.x = TRUE), tot, by = c("Sex", "Phase", "Group", "State"), all.x = TRUE)
  out[is.na(n_records), n_records := 0L]
  if (anyNA(out$n_records_group)) stop("Dwell-time table: a Sex x Phase x Group x State without records.", call. = FALSE)
  out[, `:=`(fraction = n_records / n_records_group, bin_lower_bins = edges[bin], bin_upper_bins = edges[bin + 1L],
             bin_lower_min = edges[bin] * pin$bin_size_sec / 60, bin_upper_min = edges[bin + 1L] * pin$bin_size_sec / 60,
             upper_closed = bin == nb, phase_label = unname(FSB_HMM_PHASE_LABELS[Phase]),
             state_label = lab$DisplayState[match(State, lab$State)], resolution = res)]
  out[, bin_label := sprintf("%g-%g min", bin_lower_min, bin_upper_min)]
  data.table::setorderv(out, c("Sex", "Phase", "State", "Group", "bin"))
  res <- out[, .(Sex, Phase, phase_label, Group, State, state_label, bin, bin_label, bin_lower_min, bin_upper_min, upper_closed, bin_lower_bins,
                 bin_upper_bins, n_records, n_records_group, fraction, n_animals, resolution)]
  data.table::setattr(res, "n_records_total", nrow(x))
  res[]
}

#' M4: the stored Stage 14 HMM-resolution sensitivity rows with the Wald interval Stage 14 draws
#' (mixed_model_estimate -/+ 1.96 x mixed_model_SE). Stops on a non-finite estimate or SE.
fsb_hmm_resolution_sensitivity <- function(rs) {
  x <- data.table::copy(data.table::as.data.table(rs))
  need <- c("resolution", "Domain", "Sex", "Phase", "contrast", "animal_level_hedges_g", "mixed_model_estimate", "mixed_model_SE")
  if (length(setdiff(need, names(x)))) stop("Resolution sensitivity lacks column(s): ", paste(setdiff(need, names(x)), collapse = ","), call. = FALSE)
  est <- as.numeric(x$mixed_model_estimate); se <- as.numeric(x$mixed_model_SE)
  if (!all(is.finite(est)) || !all(is.finite(se)) || any(se < 0)) stop("Resolution sensitivity: a non-finite estimate or SE.", call. = FALSE)
  x[, `:=`(wald_ci_low = est - 1.96 * se, wald_ci_high = est + 1.96 * se, ci_rule = "mixed_model_estimate -/+ 1.96 x mixed_model_SE (as Stage 14 draws it)",
           phase_label = unname(FSB_HMM_PHASE_LABELS[Phase]))]
  if (anyNA(x$phase_label)) stop("Resolution sensitivity: an unknown phase.", call. = FALSE)
  x[]
}

# ---------------------------------------------------------------- E1 (v4): Stage 09 sex-stratified correlations (copied verbatim)
#' Gate rows on E1: 6 rows = 3 features x 2 sexes; the Movement_mean rows describe exactly the ebb_v101 A2 animals (n per
#' sex = A2; the same Stage 09 run's per-animal input `mli` (model_ladder_input.csv) = A2 Movement_mean and observed_CombZ
#' for every animal within tol, so the copied rho / q were computed on the plotted values; no statistic is computed here);
#' every source cell carried verbatim; display text free of banned wording. `src_path` = the Stage 09 file.
fsb_stage09_gates <- function(E1, src_path, a2, mli, pin = FSB_STAGE09, tol = FSB_TOL) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("6-E1", name, passed, detail)
  src <- data.table::fread(src_path, colClasses = "character", na.strings = "", encoding = "UTF-8")
  cp <- E1[, setdiff(names(E1), FSB_POSTHOC_ADDED), with = FALSE]
  add("E1: every source cell carried verbatim (text) in source column order", identical(names(cp), names(src)) && identical(as.list(cp), as.list(src)))
  add(sprintf("E1: %d rows = %d features x 2 sexes, once each", pin$n_rows, length(pin$features)),
      nrow(E1) == pin$n_rows && setequal(E1$feature, pin$features) && setequal(E1$Sex, FSB_SEXES) && !anyDuplicated(E1[, .(Sex, feature)]),
      paste(nrow(E1), paste(unique(E1$feature), collapse = ",")))
  a <- data.table::as.data.table(a2)
  for (s in FSB_SEXES) {
    r <- E1[Sex == s & feature == pin$feature]
    add(sprintf("E1 %s %s: n = the ebb_v101 A2 animals of that sex", s, pin$feature), nrow(r) == 1L && identical(as.integer(r$n), nrow(a[Sex == s])),
        paste(r$n, nrow(a[Sex == s])))
  }
  m <- merge(data.table::as.data.table(mli)[, .(AnimalNum = canonical_animal_id(AnimalNum), Sex_09 = Sex, x09 = Movement_mean, y09 = outcome)],
             a[, .(AnimalNum, Sex, Movement_mean, observed_CombZ)], by = "AnimalNum")
  add(sprintf("Stage 09 input (model_ladder_input) = ebb_v101 A2 for all animals: Sex, Movement_mean, outcome = observed_CombZ (within %g)", tol),
      nrow(m) == nrow(a) && nrow(m) == nrow(mli) && all(m$Sex_09 == m$Sex) && max(abs(m$x09 - m$Movement_mean)) <= tol &&
        max(abs(m$y09 - m$observed_CombZ)) <= tol, paste(nrow(m), nrow(a), nrow(mli), s30fb_num_chr(max(abs(m$x09 - m$Movement_mean))),
                                                        s30fb_num_chr(max(abs(m$y09 - m$observed_CombZ)))))
  add("E1 display text free of banned wording (feature_label, CorrelationUse)", { fsb_check_display(E1, c("feature_label", "CorrelationUse"), "E1"); TRUE })
  data.table::rbindlist(g)
}

# ---------------------------------------------------------------- E2 / E2b / E2c (v4): the post hoc run 29c (copied verbatim)
#' Gate rows on the three copied 29c tables: row counts (202, 4, 2), every source cell carried verbatim, the registry
#' identity and tier on every row, both fits OK, each line's n = the A2 animals of that sex and its x range = their
#' Movement_mean range, and no test statistic or P column. Never stops.
fsb_posthoc29c_gates <- function(P, run_dir, a2, expect = FSB_POSTHOC29C, tol = FSB_TOL) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("6-E2", name, passed, detail)
  add("E2 / E2b / E2c present with the declared row counts (202, 4, 2)", setequal(names(P), names(FSB_POSTHOC29C_TABLES)) &&
        all(vapply(names(FSB_POSTHOC29C_ROWS), function(k) !is.null(P[[k]]) && nrow(P[[k]]) == FSB_POSTHOC29C_ROWS[[k]], TRUE)),
      paste(vapply(P, nrow, 1L), collapse = ","))
  for (k in names(P)) {
    src <- data.table::fread(file.path(run_dir, FSB_POSTHOC29C_TABLES[[k]]), colClasses = "character", na.strings = "", encoding = "UTF-8")
    cp <- P[[k]][, setdiff(names(P[[k]]), FSB_POSTHOC_ADDED), with = FALSE]
    add(paste0(k, ": every source cell carried verbatim (text) in source column order"), identical(names(cp), names(src)) && identical(as.list(cp), as.list(src)))
  }
  id <- data.table::rbindlist(lapply(P, function(x) x[, .(registry_sha256, registry_version, tier)]))
  add("E2 / E2b / E2c: registry sha256, version and tier on every row = the frozen registry of run 29c",
      nrow(id) > 0L && all(id$registry_sha256 == expect$registry_sha256) && all(id$registry_version == expect$registry_version) && all(id$tier == expect$tier))
  dg <- P$E2c_movement_combz_fit_diagnostics; fl <- P$E2_movement_combz_fit_lines; cf <- P$E2b_movement_combz_fit_coefficients
  add("E2c: both sex fits OK", setequal(dg$Sex, FSB_SEXES) && all(dg$status == "OK"), paste(dg$Sex, dg$status, collapse = "; "))
  a <- data.table::as.data.table(a2)
  for (s in FSB_SEXES) {
    x <- a[Sex == s]; l <- fl[Sex == s]; xs <- as.numeric(l$x)
    add(sprintf("E2 %s: n = the ebb_v101 A2 animals; the line spans their Movement_mean range (within %g)", s, tol),
        nrow(l) > 0L && all(as.integer(l$n) == nrow(x)) && abs(min(xs) - min(x$Movement_mean)) <= tol && abs(max(xs) - max(x$Movement_mean)) <= tol,
        paste(nrow(x), s30fb_num_chr(min(xs)), s30fb_num_chr(max(xs))))
  }
  bad <- grep("(^p_|_p$|^p$|statistic|t_value|p_value)", unique(c(names(fl), names(cf), names(dg))), value = TRUE)
  add("E2 / E2b / E2c: no test statistic or P column (registry 29c section 3)", !length(bad), paste(bad, collapse = ","))
  data.table::rbindlist(g)
}

# ---------------------------------------------------------------- T1 / T1b (v4): design ages from the planning workbook
FSB_MONTHS <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
fsb_xl_date <- function(x) as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")
#' A workbook day written as "mm/dd/yyyy" or "dd-Mon-yy" (English month abbreviation, any case) -> Date; NA otherwise.
#' Locale independent (the month is matched against FSB_MONTHS, never through %b).
fsb_parse_day <- function(x) {
  x <- trimws(as.character(x)); out <- as.Date(rep(NA_character_, length(x)))
  a <- !is.na(x) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
  out[a] <- as.Date(x[a], format = "%m/%d/%Y")
  b <- !is.na(x) & grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}$", x)
  if (any(b)) {
    p <- do.call(rbind, strsplit(x[b], "-", fixed = TRUE)); mo <- match(tolower(p[, 2]), FSB_MONTHS)
    out[b] <- as.Date(sprintf("20%s-%02d-%02d", p[, 3], mo, as.integer(p[, 1])), format = "%Y-%m-%d")
  }
  out
}
#' A workbook window "start - end" -> list(start, end): Dates (fsb_parse_day) or, with `as_int`, integers; NA when unparsable.
fsb_parse_window <- function(x, as_int = FALSE) {
  parts <- strsplit(trimws(as.character(x)), "\\s+-\\s+")
  side <- function(i) vapply(parts, function(p) if (length(p) == 2L) p[i] else NA_character_, "")
  if (as_int) return(list(start = suppressWarnings(as.integer(side(1))), end = suppressWarnings(as.integer(side(2)))))
  list(start = fsb_parse_day(side(1)), end = fsb_parse_day(side(2)))
}
#' Name of the column right after `name` (the workbook records each test's age in the unnamed column beside its date).
fsb_col_after <- function(x, name) {
  i <- match(name, names(x))
  if (is.na(i) || i >= ncol(x)) stop("Planning workbook sheet lacks the column after ", name, ".", call. = FALSE)
  names(x)[i + 1L]
}

#' Ages of the CombZ animals at each design step. `tl` = the workbook Animals sheet read as text; `a4` = ebb_v101 A4
#' (AnimalNum, Sex, Batch, Group); `v` = ebb_v101 V_runtime_gates (Batch, CC, t0, local time). Workbook ids map to
#' AnimalNum as Stage 33 maps them (prefix "XXXN-" removed, canonical id). Returns list(T1, T1b, frame).
fsb_design_ages <- function(tl, a4, v, events = FSB_DESIGN_EVENTS) {
  tl <- data.table::as.data.table(tl)
  need <- c("ID", "Batch", "DoB", "SucPref", "SocPref", "OFT", "NOR", "EPM", "End date", "Age")
  miss <- setdiff(need, names(tl)); if (length(miss)) stop("Planning workbook Animals sheet lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  tl <- tl[!is.na(ID) & nzchar(trimws(ID))]
  tl[, AnimalNum := canonical_animal_id(sub("^[A-Z]{3}[0-9]-", "", trimws(ID)))]
  d <- merge(data.table::as.data.table(a4)[, .(AnimalNum, Sex, Batch, Group)], tl, by = "AnimalNum", suffixes = c("", "_workbook"))
  dob <- fsb_xl_date(d$DoB)
  vv <- data.table::as.data.table(v)[!is.na(Batch) & nzchar(Batch) & CC %in% c("CC1", "CC4")]
  day <- function(cc) as.Date(substr(as.character(vv[CC == cc][match(d$Batch, Batch), t0]), 1, 10), format = "%Y-%m-%d")
  cc1 <- day("CC1"); cc4 <- day("CC4"); sw <- fsb_parse_window(d$SucPref)
  one <- function(x) list(x, x)
  dates <- list(shuffle_start = one(cc1), shuffle_stop = one(cc4 + 1L), sucrose_preference = list(sw$start, sw$end),
                social_preference = one(fsb_xl_date(d$SocPref)), open_field = one(fsb_xl_date(d$OFT)), novel_object = one(fsb_xl_date(d$NOR)),
                elevated_plus_maze = one(fsb_xl_date(d$EPM)), samples = one(fsb_xl_date(d$`End date`)))
  if (!setequal(names(dates), events$event_id)) stop("Design events differ from the declared events.", call. = FALSE)
  chr <- function(x) ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d"))
  long <- data.table::rbindlist(lapply(seq_len(nrow(events)), function(i) {
    e <- events$event_id[i]; ds <- dates[[e]]
    data.table::data.table(AnimalNum = d$AnimalNum, Sex = d$Sex, Batch = d$Batch, Group = d$Group, event_order = events$event_order[i],
      event_id = e, date_of_birth = chr(dob), date_start = chr(ds[[1]]), date_end = chr(ds[[2]]),
      age_start = as.integer(ds[[1]] - dob), age_end = as.integer(ds[[2]] - dob))
  }))
  data.table::setorderv(long, c("event_order", "Sex", "Batch", "AnimalNum"))
  s <- long[, .(n_animals = sum(!is.na(age_start) & !is.na(age_end)), age_min = min(age_start), age_max = max(age_end)), by = .(event_order, event_id)]
  t1 <- merge(events[, .(event_order, event_id, event_label, source, definition)], s, by = c("event_order", "event_id"))
  t1[, unit := FSB_DESIGN_UNIT]
  data.table::setorderv(t1, "event_order")
  list(T1 = t1[], T1b = long[], frame = d[])
}

#' Gate rows on the design ages: the 117 CombZ animals matched once each; workbook Batch = ebb_v101 Batch; every date and
#' age present; the design order holds for every animal; CC1 ages 22-29 (Stage 33 GC-6l); every age the workbook records
#' itself (the column beside SucPref, SocPref, OFT, NOR, EPM, and Age at the end date) = the computed age. Never stops.
fsb_design_age_gates <- function(D, tl, n_expected = FSB_N$combz_animals, cc1_age = FSB_DESIGN_CC1_AGE) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("6-T1", name, passed, detail)
  f <- D$frame; L <- D$T1b; T1 <- D$T1
  add(sprintf("planning workbook Animals sheet: the %d CombZ animals matched once each", n_expected),
      nrow(f) == n_expected && !anyDuplicated(f$AnimalNum), paste(nrow(f), data.table::uniqueN(f$AnimalNum)))
  add("workbook Batch = ebb_v101 Batch for every animal", all(paste0("B", trimws(f$Batch_workbook)) == f$Batch),
      paste(f[paste0("B", trimws(Batch_workbook)) != Batch, AnimalNum], collapse = ","))
  add("every animal has a date of birth and a date and age at every design step", !anyNA(L$date_start) && !anyNA(L$date_end) && !anyNA(L$age_start) &&
        !anyNA(L$age_end) && nrow(L) == n_expected * nrow(FSB_DESIGN_EVENTS), paste(L[is.na(age_start) | is.na(age_end), unique(paste(event_id, AnimalNum))], collapse = ","))
  w <- data.table::dcast(L, AnimalNum ~ event_id, value.var = "age_start"); we <- data.table::dcast(L, AnimalNum ~ event_id, value.var = "age_end")
  tests <- c("sucrose_preference", "social_preference", "open_field", "novel_object", "elevated_plus_maze")
  ord <- w$shuffle_start < w$shuffle_stop & apply(as.matrix(w[, tests, with = FALSE]) >= w$shuffle_stop, 1L, all) &
    w$samples >= apply(as.matrix(we[, tests, with = FALSE]), 1L, max)
  add("design order for every animal: shuffle start < shuffle stop <= every test day <= blood and tissue collection", isTRUE(all(ord)),
      paste(w$AnimalNum[!ord %in% TRUE], collapse = ","))
  add(sprintf("CC1 ages %d-%d for every animal (Stage 33 gate GC-6l)", cc1_age[1], cc1_age[2]),
      isTRUE(all(w$shuffle_start >= cc1_age[1] & w$shuffle_start <= cc1_age[2])), paste(range(w$shuffle_start), collapse = "-"))
  sis <- if ("SIS start" %in% names(f)) fsb_xl_date(f$`SIS start`) else as.Date(rep(NA_character_, nrow(f)))
  cc1d <- as.Date(L[event_id == "shuffle_start"][match(f$AnimalNum, AnimalNum), date_start], format = "%Y-%m-%d")
  add("workbook 'SIS start' = the CC1 date of the ebb_v101 cage-change time, for every animal", !anyNA(sis) && all(sis == cc1d),
      paste(f$AnimalNum[is.na(sis) | sis != cc1d], collapse = ","))
  # the ages the workbook records itself
  key <- function(e, which = "age_start") L[event_id == e][match(f$AnimalNum, AnimalNum)][[which]]
  cmp <- function(label, recorded, computed) {
    has <- !is.na(recorded)
    add(sprintf("workbook-recorded %s age = computed age (%d animals with a recorded age)", label, sum(has)),
        sum(has) > 0L && all(recorded[has] == computed[has]), paste(f$AnimalNum[has & recorded != computed], collapse = ","))
  }
  day_tests <- c(SocPref = "social_preference", OFT = "open_field", NOR = "novel_object", EPM = "elevated_plus_maze")
  for (nm in names(day_tests)) cmp(nm, suppressWarnings(as.integer(f[[fsb_col_after(tl, nm)]])), key(day_tests[[nm]]))
  sr <- fsb_parse_window(f[[fsb_col_after(tl, "SucPref")]], as_int = TRUE)
  cmp("sucrose window start", sr$start, key("sucrose_preference")); cmp("sucrose window end", sr$end, key("sucrose_preference", "age_end"))
  cmp("end-date (Age)", suppressWarnings(as.integer(f$Age)), key("samples"))
  add(sprintf("T1: %d design steps, %d animals each", nrow(FSB_DESIGN_EVENTS), n_expected),
      nrow(T1) == nrow(FSB_DESIGN_EVENTS) && all(T1$n_animals == n_expected) && identical(T1$event_id, FSB_DESIGN_EVENTS$event_id),
      paste(T1$event_id, T1$n_animals, collapse = "; "))
  data.table::rbindlist(g)
}

# ---------------------------------------------------------------- provenance and writer
fsb_provenance <- function(fields) {
  v <- vapply(fields, function(z) paste(as.character(z), collapse = " || "), "")
  h <- data.table::data.table(field = names(fields), value = unname(v)); data.table::setnames(h, "field", "key")
  miss <- setdiff(FSB_PROVENANCE_KEYS, h$key[!is.na(h$value) & nzchar(h$value)])
  if (length(miss)) stop("H_provenance lacks key(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyDuplicated(h$key)) stop("H_provenance has duplicated keys.", call. = FALSE)
  h
}
fsb_bundle_files <- function() c(paste0(FSB_TABLES, ".csv"), "H_provenance.csv", "H2_inputs.csv", FSB_GATE_FILE)

#' Post-write gates on the renamed folder, run BEFORE the registry row is appended. Never stops.
fsb_post_write_gates <- function(bd, manifest, expected_files = fsb_bundle_files(), verbatim = NULL) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("7-write", name, passed, detail)
  m <- data.table::as.data.table(manifest); mf <- file.path(bd, "00_manifest.csv")
  back <- if (file.exists(mf)) data.table::fread(mf, colClasses = "character") else data.table::data.table()
  add("00_manifest.csv read back = the manifest written (file, bytes, sha256, schema_version)",
      nrow(back) == nrow(m) && identical(names(back), names(m)) && identical(back$file, m$file) && identical(back$sha256, m$sha256) &&
        identical(as.numeric(back$bytes), as.numeric(m$bytes)) && identical(back$schema_version, as.character(m$schema_version)), mf)
  chk <- s30fb_manifest_check(bd, m); on_disk <- s30fb_files_on_disk(bd); extra <- setdiff(on_disk, c(m$file, "00_manifest.csv"))
  add("written bundle: every file = 00_manifest (bytes and SHA-256); nothing else in the folder",
      nrow(chk) > 0L && all(chk$ok) && !length(extra) && "00_manifest.csv" %in% on_disk,
      paste(c(bd, if (!all(chk$ok)) paste("mismatch:", paste(chk$file[!chk$ok], collapse = ",")), if (length(extra)) paste("extra:", paste(extra, collapse = ","))), collapse = "; "))
  add(sprintf("00_manifest lists exactly the declared files (%d tables, H_provenance, H2_inputs, H3_gate_results)", length(FSB_TABLES)),
      setequal(m$file, expected_files) && !anyDuplicated(m$file), paste(c(setdiff(expected_files, m$file), setdiff(m$file, expected_files)), collapse = ","))
  fl <- list.files(bd, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  add("written files are read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), bd)
  rbind(data.table::rbindlist(g), fsb_verbatim_gates(bd, verbatim))
}

#' Write the bundle once (the Stage 30b write protocol). Refuses if the version folder, its staging folder, its registry
#' row or its gate log exists, if a hard pre-write gate failed, or if H_provenance pre_write_gates_passed does not count
#' the gate table. Tables, H_provenance, H2_inputs, H3_gate_results and 00_manifest go to a staging folder, which is
#' renamed and made read-only; the post-write gates run before the BUNDLE_REGISTRY.csv row is appended; the complete gate
#' table goes to logs/<bundle_id>_gate_results.csv beside the registry (0444). `verbatim` (bundle table -> source path) adds the
#' line-level verbatim post-write gate for the copied post hoc tables. `.before_verify` is a test hook only.
fsb_write_bundle <- function(tables, out_root, bundle_id, provenance, inputs, gates, status, stage29_bundle_id, combz_table_sha256,
                             mmm_git_commit, created_at, verbatim = NULL, .before_verify = NULL) {
  bd <- file.path(out_root, bundle_id); stg <- file.path(out_root, paste0(".tmp_", bundle_id)); reg <- file.path(out_root, "BUNDLE_REGISTRY.csv")
  log <- s30fb_log_path(out_root, bundle_id)
  registered <- function() file.exists(reg) && bundle_id %in% data.table::fread(reg, colClasses = "character")$bundle_id
  if (dir.exists(bd)) stop("Bundle version exists (immutable): ", bd, call. = FALSE)
  if (dir.exists(stg)) stop("A staging folder for this bundle exists (earlier aborted write): ", stg, call. = FALSE)
  if (registered()) stop("Bundle id already registered: ", bundle_id, call. = FALSE)
  if (file.exists(log)) stop("A gate log for this bundle exists (immutable): ", log, call. = FALSE)
  bad <- setdiff(names(tables), FSB_TABLES); miss <- setdiff(FSB_TABLES, names(tables))
  if (length(bad) || length(miss)) stop("Table set differs from the declared set: extra ", paste(bad, collapse = ","), "; missing ", paste(miss, collapse = ","), call. = FALSE)
  gates <- data.table::as.data.table(gates)
  if (!identical(names(gates), FSB_GATE_COLS) || !is.logical(gates$passed) || !is.logical(gates$hard))
    stop("The gate table must have columns ", paste(FSB_GATE_COLS, collapse = ", "), " (passed / hard logical).", call. = FALSE)
  if (!nrow(gates) || anyNA(gates$passed) || any(gates$hard & !gates$passed)) stop("Refusing to write: a hard pre-write gate did not pass.", call. = FALSE)
  pv <- provenance$value[provenance$key == "pre_write_gates_passed"]
  if (!identical(pv, fsb_gate_count(gates)))
    stop("H_provenance pre_write_gates_passed (", paste(pv, collapse = ","), ") does not count the gate table (", fsb_gate_count(gates), ").", call. = FALSE)
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  dir.create(stg)
  for (n in FSB_TABLES) s30fb_write_csv(tables[[n]], file.path(stg, paste0(n, ".csv")))
  s30fb_write_csv(provenance, file.path(stg, "H_provenance.csv"))
  s30fb_write_csv(inputs, file.path(stg, "H2_inputs.csv"))
  s30fb_write_csv(gates, file.path(stg, FSB_GATE_FILE))
  fl <- sort(list.files(stg, full.names = TRUE))
  man <- data.table::data.table(file = basename(fl), bytes = as.numeric(file.size(fl)), sha256 = vapply(fl, s30fb_sha, "", USE.NAMES = FALSE), schema_version = FSB_SCHEMA_VERSION)
  s30fb_write_csv(man, file.path(stg, "00_manifest.csv"))
  if (dir.exists(bd)) stop("Bundle version appeared during the write (immutable): ", bd, call. = FALSE)
  if (!file.rename(stg, bd)) stop("Could not rename ", stg, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, full.names = TRUE), mode = "0444")
  if (is.function(.before_verify)) .before_verify(bd)
  post <- fsb_post_write_gates(bd, man, verbatim = verbatim)
  if (!all(post$passed)) {
    s30fb_write_gate_log(rbind(gates, post), log)
    stop("Post-write gate failed; the bundle is NOT registered (folder ", bd, " left for inspection; gate log ", log, "): ",
         paste(post$gate[!post$passed], collapse = "; "), call. = FALSE)
  }
  msha <- s30fb_sha(file.path(bd, "00_manifest.csv"))
  if (registered()) stop("A registry row for this bundle appeared during the write: ", bundle_id, call. = FALSE)
  if (file.exists(log)) stop("A gate log for this bundle appeared during the write: ", log, call. = FALSE)
  row <- data.table::data.table(bundle_id = bundle_id, status = status, manifest_sha256 = msha, stage29_bundle_id = stage29_bundle_id,
                                combz_table_sha256 = combz_table_sha256, mmm_git_commit = mmm_git_commit, created_at = created_at)
  data.table::fwrite(row, reg, append = file.exists(reg))
  rg <- s30fb_registration_gate(reg, bundle_id, status, msha)
  all_gates <- rbind(gates, post, rg)
  s30fb_write_gate_log(all_gates, log)
  if (!isTRUE(rg$passed)) stop("Registration gate failed: ", rg$detail, call. = FALSE)
  list(dir = bd, manifest_sha256 = msha, manifest = man, registry = reg, log = log, log_sha256 = s30fb_sha(log),
       post_gates = rbind(post, rg), gates = all_gates)
}
