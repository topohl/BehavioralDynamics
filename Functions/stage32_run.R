# ================================================================
# Stage 32: identity constants, gates, QC plots and the run-once read-only writer (Exp9 SIS RFID)
# MMMSociability -- Functions/stage32_run.R
# ================================================================
# Pure constants and functions for Analysis/32_behavior_exposure_adaptation.R (registry sections 8 and 10 of the FROZEN
# docs/STAGE32_REGISTRY_v1.0.md, and the FROZEN addendum docs/STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md). Sourcing this file
# reads and writes nothing; it holds no S: path (the runner builds paths from mmm_project_root()).
# Addendum A1 (writer correction): the first REAL execution (commit 4f9c016) aborted inside the writer and left the staging
# folder .tmp_v1.0_4f9c016 (kept, read-only, as evidence; it keeps every v1.0 run name blocked). The corrected execution
# writes v1.1_<commit7> once: every file is written and round-trip-checked in a LOCAL staging folder, the 12 tables must be
# byte-identical to the aborted staging tables (determinism gate), and only then is the folder copied to S:.
# Requires: data.table, digest; Functions/stage30_figure_bundle.R (s30fb_sha, s30fb_write_csv, s30fb_gate_row,
# s30fb_manifest_check, s30fb_files_on_disk) sourced first; ggplot2 for the QC plots.
# ================================================================

S32_VERSION <- "1.0"          # registry version (analysis registry v1.0, unchanged)
S32_RUN_VERSION <- "1.1"      # run-folder version (addendum A1: the corrected execution)
S32_REGISTRY <- list(
  version = "1.0", file = "STAGE32_REGISTRY_v1.0.md", csv = "STAGE32_HYPOTHESES_v1.0.csv",
  repo_path = "docs/STAGE32_REGISTRY_v1.0.md", repo_csv = "docs/STAGE32_HYPOTHESES_v1.0.csv",
  sha256 = "cfbd954cde6056c275ba28a9eddcae31b38eff07cec061b8f6a74a703791ee5a",
  csv_sha256 = "bcd002161ad3585c71d0a96a41cffc741f39d18770343188967715a0b63f7696",
  canonical_rel = file.path("canonical", "stage32_registry", "v1.0"), sha_file = "REGISTRY_SHA256.txt",
  freeze_commit = "0970d320e4f594f7836cd1ca3598e948992cc630", frozen_at = "2026-09-30T22:41:10+0200")
S32_STAGE_DIR <- "32_behavior_exposure_adaptation"
S32_OPTIN_ENV <- "MMM_STAGE32_REAL_RUN"
S32_LOCAL_STAGING_ENV <- "MMM_STAGE32_LOCAL_STAGING"   # optional local staging root (default: the R session tempdir())
S32_REAL_OUT_PATTERN <- "^(\\.tmp_)?v1\\.1_"
# registry addendum A1 (FROZEN before the v1.1 execution; committed alone, S: copy read-only next to the registry)
S32_ADDENDUM <- list(
  id = "A1", file = "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md", repo_path = "docs/STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md",
  sha_file = "ADDENDUM_A1_SHA256.txt",
  sha256 = "0893b5de811ecd57fca25127174fb88e63308588a05e442c6c17d4532cd98acc",
  freeze_commit = "3ae8198c2779c115bb0e56e236f4859171aefe93", frozen_at = "2026-10-01T00:12:25+0200")
# the aborted first execution (addendum A1 section A): its staging folder and the 14 files it holds (sha256 as left on S:)
S32_ABORTED <- list(
  name = ".tmp_v1.0_4f9c016", commit = "4f9c0169b975d9d1efce49791ff9815b2c6d93a7",
  table_sha256 = c(
    coverage_manifest = "e5352ac5add6a7dc02521952e77c4ce9ed921dbc537026c592bbd363d955e493",
    window_metrics_long = "7f2bfdcc25a9331dc68bc93eddfc03599908208723b94b16c8e753d03c5b2f93",
    descriptives = "cdb27f2ddc40e987ecac9d8bdb42044275c0146bfaf85e6a65d46a1fa92c1dbc",
    models = "b2b1f608cf5bc7bd2dace7e020e7c23c3732d9474fda76f57f2958e5a2adee67",
    estimates = "76f262ffad6b8f7a184fccc0cbe4852f3bfe59549cf2ad8745a0691ea6dcfc40",
    joint_tests = "5fd5d9460b66e945189d1e3dc008398bc463be72c5b4499774941056f3fa2411",
    multiplicity = "3544bcd052e66cef7535eff3ed21d3f7e37e35fe94fd3168bda7c322e0e9b81b",
    sensitivities = "c013bf899336666a615ba0a7e7fba7f024d21440eae0ebbeea4ec920fdfcd636",
    diagnostics = "d39ef652675a86d4094ee2593635e059fa0acf8ae1ecd533abff7b98166e8fe4",
    prediction_performance = "c8d627a0bd1fb5688f1ee865f91bfbde6290d4eb871aa7111248fd8d8ffc2846",
    prediction_permutations = "5c4e36d66b942f812f90adc353b38ada9d0ef79f6cd9e6c66176e04fb9fec8be",
    prediction_heldout = "c384ca1188f0412bd2780b09dce8e85df5f32155170c42f026066aad0e5fb664"),
  audit_sha256 = c(
    input_hashes = "46ae956610216aa17e28730790c7df85a2ce0781d59bdc643969e285ee58301d",
    gate_results = "86dd53bf3cdfb44b42a0057fb91ebec2e98eb723f745b42471bd2e1bbc5419dd"))
# numeric Stage 32 code: unchanged since the aborted execution (git blobs compared by the runner)
S32_NUMERIC_CODE <- c("Functions/stage32_windows.R", "Functions/stage32_inference.R", "Functions/stage32_prediction.R")
S32_TIER <- "POST HOC relative to the original experiment and the earlier analysis plans; registered before its own fitting"
S32_DECISION_BASIS <- "POST_HOC_CONTEXT"
S32_RUN_MODE <- "REAL_POST_HOC_CONTEXT"
# frozen inputs (registry sections 1, 5, 8); relative to the RFID project root unless absolute
S32_INPUT_SHA256 <- c(
  v2_manifest = "bb33a111980913f4ddf97c40872ccdd96dd73accdbdc5c5867954b51aad2c571",
  ebb_v101_manifest = "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c",
  combz_table = "1f6a2a69c6b0781b8de8da50ecdadf309b62bbc9c82106c0fa91d18119a7de54",
  sus_list = "dea3804b71b479f84900f946e5494b204ed0e8801498da459a6fc781beb9f391",
  con_list = "eddd2ee9c3a182f98211b4cbba689e2bb5f72985aeb178f72b25b7d25db1b75d",
  stage09_input = "da78f80e4b7867cbabe7847a7da2667459a690a1b25f8ff97ef919cc8d2426dd",
  stage29_exposure_estimates = "33d91929453ae9eec694b173208a6588021893604aa98fa379227adfc7611a70",
  stage29_stage09_cv_sensitivities = "6cc3027e0a6f3af6797b1436e26e49b320212bf267657e29e91cdcb5de0e3028",
  stage29_continuous_estimates = "cc549c248778a735f1e79ba2b33e59443778ef200e480ad7479b4150f2b15200")   # addendum A1: H13 comparator only
S32_EBB <- list(bundle_id = "ebb_v101_20260929_b2ce507", manifest_sha256 = S32_INPUT_SHA256[["ebb_v101_manifest"]],
                b1_file = "B1_animal_longitudinal.csv", b1_sha256 = "2a710a8ad90f98a4bf2f9f1460bf47c8d59e1bbfdc707533a4e68e864b679860")
S32_S29_RELEASE <- "v101_dv2_b2ce507"
S32_V2_DIR <- "v2_cage_label_correction_2026-09-28"
S32_V2_N_FILES <- 24L
# raw_data AnimalPos files: the canonical seed source and the raw records of the board observation interval
# (hashes = Stage 29 v1.0.1 release audit/run_inputs.csv, role raw_seed; also frozen in Functions/stage30_screen.R)
S32_RAW_SHA256 <- c(
  "B1/E9_SIS_B1_CC1_AnimalPos.csv" = "03b50f8f39e63026b354e611eb10617809c10edfd7cc623d09b8f86f7cdc9df1",
  "B1/E9_SIS_B1_CC2_AnimalPos.csv" = "09265bd14c8b9e627188fecff04e92519ec5b484df4673760ee5054617cc1778",
  "B1/E9_SIS_B1_CC3_AnimalPos.csv" = "5855d1bbf97a74bf1fa3500f2a64c425e114bc98d7b7d1169f76b9e39001597b",
  "B1/E9_SIS_B1_CC4_AnimalPos.csv" = "b7b9cffc1000065205e4ba09b1b74122c5ae07d0b5949482385bbfe8293f7663",
  "B2/E9_SIS_B2_CC1_AnimalPos.csv" = "8069d8cef4e277f6f9106d5ca8b568a6db38e15d3bbfe4d26ff72c8b2ac7ed16",
  "B2/E9_SIS_B2_CC2_AnimalPos.csv" = "813f9113143da736b0dbba3e4bda78654161a7072bcb8a10e4eb60be62957bb8",
  "B2/E9_SIS_B2_CC3_AnimalPos.csv" = "14d031e5f4a41c8c655248a8200e9bb0cea32f9b694a1b0b641022c022d1a61f",
  "B2/E9_SIS_B2_CC4_AnimalPos.csv" = "ef5c68588fd359d42f9373735afdff33673c49438fde5bf2db4c68f06fee7489",
  "B3/E9_SIS_B3_CC1_AnimalPos.csv" = "70f83fd9c6c8a1a8628e54b19f9792e425f5d6306741f5b8e253fbfb51a89449",
  "B3/E9_SIS_B3_CC2_AnimalPos.csv" = "9b8c29f8d20cf0591e2fccf7d38b4408f06c1822bf14f4f3723a0616e8b9bf1c",
  "B3/E9_SIS_B3_CC3_AnimalPos.csv" = "c73f2cf0bd3b9b0b6119b989fbd8ff3ccbf427161173448f8418c000a57680be",
  "B3/E9_SIS_B3_CC4_AnimalPos.csv" = "f9f767c7b13f67a958105f031b8a6ecbee078f89bf676eb8b1f9fa8c6440ebff",
  "B4/E9_SIS_B4_CC1_AnimalPos.csv" = "5a81febaad777abdab7c1b3571f44abe7f49c8169d831fbcf39b69b166ae96d4",
  "B4/E9_SIS_B4_CC2_AnimalPos.csv" = "1f61a21103042daad06962c5800914e54b67c872ba415facccf40ea34dd87cd3",
  "B4/E9_SIS_B4_CC3_AnimalPos.csv" = "dcf5de83aff6b912a08eab2c970925a675727d49ecff27d079ac1e9dee25919a",
  "B4/E9_SIS_B4_CC4_AnimalPos.csv" = "7eaed2de19063de21e9e67301c14f8427158d85b516147990978cef96f778f3f",
  "B5/E9_SIS_B5_CC1_AnimalPos.csv" = "c9d0e75480f1cf426e3e634058e2b383427b6e547d7e124a2483087e2b0a00dc",
  "B5/E9_SIS_B5_CC2_AnimalPos.csv" = "a2df44eaf28d952f7fec14fe0604c05f7cc489afa0a74bec2abcae8b77b1c053",
  "B5/E9_SIS_B5_CC3_AnimalPos.csv" = "79139239ea4f0b0e5c9584b6f7a20259d0073df97b89f2c920a05e0eac1559dd",
  "B5/E9_SIS_B5_CC4_AnimalPos.csv" = "bb1a7cf44657b0e8aca2718051da8b163d376b5a6080666125207c9f5902c3a7",
  "B6/E9_SIS_B6_CC1_AnimalPos.csv" = "98dd98a2a83436a71f6ac4be209aff25e6ed9f26727bd3468a1ad3415cccd0cb",
  "B6/E9_SIS_B6_CC2_AnimalPos.csv" = "c36de3dc8f004880075443381f4445a26aa1ce3f46e2b4820378c8c90e5ddded",
  "B6/E9_SIS_B6_CC3_AnimalPos.csv" = "fe570ca5381cf6b5ff5b18506ef195985499170d50d59c209a6e996a60881727",
  "B6/E9_SIS_B6_CC4_AnimalPos.csv" = "af827417db646c6dc2756aaff3bea01f992813975e92dd5b742c238b016e5549")
# frozen code sourced unchanged (Stage 30 FREEZE_RECORD code identities; stage30_figure_bundle.R at 5394f2f)
S32_FROZEN_CODE_SHA256 <- c(
  "Functions/stage30_movement.R" = "9a368491ff373194a22e6baacd4874bfa260c4e1296e81d9be5863ed6bfde659",
  "Functions/rfid_canonical_inference.R" = "886d8bc06807b5958482a31449d0b8e5886db106cc7d02f63863d535dc78b92c",
  "Functions/rfid_event_stream.R" = "3fda3d40f154d2423c1a2e16b687a14369ac9f8c070d883534a713d9a5dceb68",
  "Functions/rfid_binfree_metrics.R" = "ffc1235dbef6b05e63ee6dee7b7b22f3e7107157be2c0a3ff491706079e0d8d0",
  "Functions/stage30_figure_bundle.R" = "db396fcd456e11e33908ecbb04233d9c4acaec960373d515cdcfba2a8fd0c05c")
S32_BOUT_CRITERION_S <- 39.3970988275363
S32_PACKAGES <- c(lme4 = "2.0.1", lmerTest = "3.2.1", pbkrtest = "0.5.5", clubSandwich = "0.7.0", Matrix = "1.7.5", reformulas = "0.4.4")
S32_R_VERSION <- "R version 4.5.1 (2025-06-13 ucrt)"
S32_POP <- list(n_animals = 111L, n_sis = 87L, by_sex_group = list(Female = c(CON = 12L, RES = 28L, SUS = 18L), Male = c(CON = 12L, RES = 25L, SUS = 16L)),
                sex_batches = list(Female = c("B3", "B4", "B6"), Male = c("B1", "B2", "B5")), overlap_nonfinite_A1_CC1 = c("OQ770", "OQ771"),
                n_clean_phase_cc = 24L, n_c_rows = 1554L, n_d_rows = 1332L, n_e_rows = 1110L)
S32_FORBIDDEN_WORDS <- c("acute", "immediate response", "sleep", "settled state", "confirmatory", "preregistered", "significant", "stars", "\\brest\\b")
S32_TABLES <- c("coverage_manifest", "window_metrics_long", "descriptives", "models", "estimates", "joint_tests", "multiplicity", "sensitivities",
                "diagnostics", "prediction_performance", "prediction_permutations", "prediction_heldout")
S32_AUDIT <- c("input_hashes", "gate_results", "run_manifest", "reproduction_gate", "reference_a1_gate", "coverage_counts",
               "aborted_run_evidence", "determinism_gate", "reporting_flags")        # the last three: addendum A1
S32_MANIFEST_FILE <- "audit/output_manifest.csv"
S32_README <- "README.txt"
S32_CONVENTIONS <- c(
  "C1 board observation interval: raw records are attributed to a board through the v2 System of their AnimalNum in that SourceFile; records of animals absent from the v2 file (raw_data/excluded_animals.csv) are not attributed",
  "C2 (a) block kept = the block label (A<k> / I<k+1>) occurs in the SourceFile's v2 preprocessed rows",
  "C3 non-convergence = the engine's converged flag FALSE (any lme4 convergence-class message); the optimizer-check verdict is recorded, not used to rescue the fit (29b precedent)",
  "C4 KR numeric failure = lmerTest's KR->Satterthwaite warning, a KR error, or a non-finite KR SE / df; the row is recomputed with Satterthwaite df (ddf_fallback = TRUE)",
  "C5 CC enters as the c2, c3, c4 treatment dummies (CC1 reference), the same design as a CC factor",
  "C6 A-dec: group3 treatment-coded against CON as explicit RES / SUS indicator columns (R's default coding of group3:sex_c without a sex_c main effect adds a CON:sex_c column collinear with Batch)",
  "C7 categorical-phase models (D and the phase_f sensitivity): phase_f replaces phase in the fixed part; the random part keeps the registered numeric-phase terms",
  "C8 C secondary: g_SIS:phase:sex_c (CombZ_wb:phase:sex_c) is added alone, as registered, without further lower-order sex terms; estimation only",
  "C9 C secondary is fitted only if its primary C model has status OK",
  "C10 D and phase profile: batch-balanced (1/6 per batch dummy) and CC1-CC3-averaged L-vectors; plateau contrast = A4 - A2; linear trend of the categorical profile = weights (-0.3, -0.1, 0.1, 0.3) over A1..A4",
  "C11 Module F repeated grouped 5-fold: CC1 cages shuffled (seed 521) and assigned round-robin to 5 folds, the same folds for A, B and C; any test batch absent from its training fold gets the mean of the training batch effects (the LOBO rule)",
  "C12 permutations: one set of 1000 within-Batch index vectors (seed 20260811; Mersenne-Twister / Inversion / Rejection) shared by both behaviour sets; the unrestricted null re-seeded with 20260811; a null draw within 1e-12 of the observed value counts as >=",
  paste("C13 CombZ_wb = CombZ minus its batch mean over the 87 SIS animals (animal level); every C-cz, D-cz and E-cz model uses all 87 animals, the",
        "centring set (A-cz overlap uses 85). A batch-specific shift of CombZ_wb is absorbed by Batch when the only CombZ_wb terms are CombZ_wb and",
        "CombZ_wb:sex_c, but it would create Batch x phase or Batch x CC terms that the C-cz, D-cz and E-cz models do not contain, so CombZ_wb:phase",
        "and CombZ_wb:CC depend on the centring set (wording corrected by addendum A1; the v1.0 wording said no slope changes)"),
  "C14 estimation-only rows carry no statistic and no p (registry section 6)",
  "C15 sensitivities are compared with the primary-model row of the same estimand (shift in primary SEs; Stage 29 robustness label); categorical-phase rows through the linear-trend contrast only",
  paste("C16 nothing estimated is printed before the outputs are written; every output file is written and round-trip-checked in a local staging",
        "folder before anything is copied to S: (addendum A1), so a failure before the S: copy leaves nothing on S:; a failure during the copy leaves",
        ".tmp_v1.1_<commit7>, which blocks any further run"),
  paste("C17 estimand shared_phase_slope (C-exp phase coefficient) is the CON/SIS-average phase slope: under +-1/2 g_SIS coding it is the unweighted",
        "average of the CON and SIS phase slopes, not a slope shared by CON and SIS (addendum A1; the table label is kept)"))

# ---------------------------------------------------------------- identity utilities
s32r_run_name <- function(commit) {
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("Not a full git commit hash: ", commit, call. = FALSE)
  paste0("v", S32_RUN_VERSION, "_", substr(commit, 1, 7))
}
#' The only mode is --real (registry section 8: one REAL run; the synthetic tests are the only rehearsal).
s32r_parse_mode <- function(args) {
  if (!identical(as.character(args), "--real"))
    stop("Refusing to run: the only mode is --real (registry v1.0 section 8: one REAL run). Synthetic checks: Testing/tests/test_stage32_*.R", call. = FALSE)
  "REAL"
}
s32r_sha_lf <- function(path) { b <- readBin(path, what = "raw", n = file.size(path)); digest::digest(b[b != as.raw(13L)], algo = "sha256", serialize = FALSE) }
s32r_sha_list_lookup <- function(lines, name) {
  x <- trimws(gsub("\r", "", lines, fixed = TRUE)); x <- x[grepl("^[0-9a-f]{64}\\s", x)]
  sha <- sub("^([0-9a-f]{64})\\s.*$", "\\1", x); nm <- trimws(sub("^[0-9a-f]{64}\\s+[*]?", "", x))
  hit <- basename(nm) == name; if (sum(hit) != 1L) NA_character_ else sha[hit]
}
s32r_gate <- function(stage, gate, passed, detail = "") s30fb_gate_row(stage, gate, passed, detail, hard = TRUE)

#' Registry gates: S: copies (bytes) and repository copies (LF form) = the frozen sha256; REGISTRY_SHA256.txt lists both;
#' S: files read-only; status line FROZEN.
s32r_registry_gates <- function(canon_dir, repo_md, repo_csv, expect = S32_REGISTRY) {
  s_md <- file.path(canon_dir, expect$file); s_csv <- file.path(canon_dir, expect$csv); s_sha <- file.path(canon_dir, expect$sha_file)
  sh <- function(p) if (file.exists(p)) s30fb_sha(p) else NA_character_
  lf <- function(p) if (file.exists(p)) s32r_sha_lf(p) else NA_character_
  lines <- if (file.exists(s_sha)) readLines(s_sha, warn = FALSE) else character()
  st <- if (file.exists(s_md)) grep("^\\*\\*Status\\.\\*\\*", readLines(s_md, warn = FALSE, encoding = "UTF-8"), value = TRUE) else character()
  ro <- c(s_md, s_csv, s_sha); ro <- ro[file.exists(ro)]
  rbind(s32r_gate("0-registry", "registry S: copy SHA-256 = frozen", identical(sh(s_md), expect$sha256), paste(s_md, sh(s_md))),
        s32r_gate("0-registry", "hypothesis table S: copy SHA-256 = frozen", identical(sh(s_csv), expect$csv_sha256), paste(s_csv, sh(s_csv))),
        s32r_gate("0-registry", "REGISTRY_SHA256.txt lists both frozen SHA-256", identical(s32r_sha_list_lookup(lines, expect$file), expect$sha256) &&
                    identical(s32r_sha_list_lookup(lines, expect$csv), expect$csv_sha256), s_sha),
        s32r_gate("0-registry", "registry repository copy (LF form) SHA-256 = frozen", identical(lf(repo_md), expect$sha256), paste(repo_md, lf(repo_md))),
        s32r_gate("0-registry", "hypothesis table repository copy (LF form) SHA-256 = frozen", identical(lf(repo_csv), expect$csv_sha256), paste(repo_csv, lf(repo_csv))),
        s32r_gate("0-registry", "registry status line: FROZEN", length(st) == 1L && grepl("FROZEN", st, fixed = TRUE), st),
        s32r_gate("0-registry", "registry S: files read-only", length(ro) == 3L && all(file.access(ro, 2L) != 0L), paste(ro, collapse = "; ")))
}
s32r_package_gates <- function(expected = S32_PACKAGES, r_version = S32_R_VERSION) {
  obs <- vapply(names(expected), function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), "")
  rbind(data.table::rbindlist(lapply(names(expected), function(p)
          s32r_gate("0-packages", paste0(p, " version = Stage 29 v1.0.1 release version ", expected[[p]]), identical(obs[[p]], expected[[p]]), obs[[p]]))),
        s32r_gate("0-packages", paste("R version =", r_version), identical(R.version.string, r_version), R.version.string))
}
s32r_blocking_entries <- function(out_root) {
  if (!dir.exists(out_root)) return(character())
  e <- list.files(out_root, all.files = TRUE, no.. = TRUE); e[grepl(S32_REAL_OUT_PATTERN, e)]
}

#' Addendum A1 gates: S: copy (bytes) and repository copy (LF form) = the frozen sha256; the addendum sha list names it;
#' S: files read-only; status line FROZEN.
s32r_addendum_gates <- function(canon_dir, repo_md, expect = S32_ADDENDUM) {
  s_md <- file.path(canon_dir, expect$file); s_sha <- file.path(canon_dir, expect$sha_file)
  sh <- if (file.exists(s_md)) s30fb_sha(s_md) else NA_character_
  lf <- if (file.exists(repo_md)) s32r_sha_lf(repo_md) else NA_character_
  lines <- if (file.exists(s_sha)) readLines(s_sha, warn = FALSE) else character()
  st <- if (file.exists(s_md)) grep("^\\*\\*Status\\.\\*\\*", readLines(s_md, warn = FALSE, encoding = "UTF-8"), value = TRUE) else character()
  ro <- c(s_md, s_sha); ro <- ro[file.exists(ro)]
  rbind(s32r_gate("0-addendum", "addendum A1 S: copy SHA-256 = frozen", identical(sh, expect$sha256), paste(s_md, sh)),
        s32r_gate("0-addendum", "addendum A1 repository copy (LF form) SHA-256 = frozen", identical(lf, expect$sha256), paste(repo_md, lf)),
        s32r_gate("0-addendum", "ADDENDUM_A1_SHA256.txt lists the frozen SHA-256", identical(s32r_sha_list_lookup(lines, expect$file), expect$sha256), s_sha),
        s32r_gate("0-addendum", "addendum A1 status line: FROZEN", length(st) == 1L && grepl("FROZEN", st, fixed = TRUE), st),
        s32r_gate("0-addendum", "addendum A1 S: files read-only", length(ro) == 2L && all(file.access(ro, 2L) != 0L), paste(ro, collapse = "; ")))
}

#' Addendum A1 section B2: the Stage 32 folder holds only the aborted v1.0 staging folder, whose files are exactly the 14 pinned
#' files (sha256), all read-only. Returns list(gates, evidence) (evidence = audit/aborted_run_evidence.csv).
s32r_aborted_check <- function(out_root, expect = S32_ABORTED) {
  entries <- if (dir.exists(out_root)) list.files(out_root, all.files = TRUE, no.. = TRUE) else character()
  ad <- file.path(out_root, expect$name)
  pinned <- c(stats::setNames(expect$table_sha256, paste0("tables/", names(expect$table_sha256), ".csv")),
              stats::setNames(expect$audit_sha256, paste0("audit/", names(expect$audit_sha256), ".csv")))
  on_disk <- if (dir.exists(ad)) s30fb_files_on_disk(ad) else character()
  on_disk_files <- on_disk[!dir.exists(file.path(ad, on_disk))]
  rel <- sort(union(names(pinned), on_disk_files))
  p <- file.path(ad, rel); ex <- file.exists(p)
  obs <- rep(NA_character_, length(p)); obs[ex] <- vapply(p[ex], s30fb_sha, "", USE.NAMES = FALSE)
  ev <- data.table::data.table(folder = expect$name, file = rel, bytes = ifelse(ex, as.numeric(file.size(p)), NA_real_), sha256 = obs,
                               sha256_pinned = unname(pinned[rel]), read_only = ex & file.access(p, 2L) != 0L)
  ev[, status := ifelse(is.na(sha256_pinned), "UNEXPECTED FILE", ifelse(is.na(sha256), "MISSING", ifelse(sha256 == sha256_pinned, "as left by the aborted v1.0 execution", "CHANGED")))]
  ev[, role := ifelse(grepl("^tables/", file), "determinism reference (addendum A1 B3)", "evidence only")]
  gates <- rbind(
    s32r_gate("1-output", paste0("the Stage 32 folder holds only the aborted v1.0 staging folder ", expect$name, " (no v1.0_* run folder, no other entry)"),
              identical(entries, expect$name), paste(entries, collapse = ",")),
    s32r_gate("1-output", "aborted staging folder = the 14 pinned files (sha256), nothing else", nrow(ev) == length(pinned) && all(ev$status == "as left by the aborted v1.0 execution"),
              paste(ev[status != "as left by the aborted v1.0 execution", paste(file, status)], collapse = "; ")),
    s32r_gate("1-output", "aborted staging folder: every file read-only", nrow(ev) > 0L && all(ev$read_only), paste(ev[read_only == FALSE, file], collapse = ",")))
  list(gates = gates, evidence = ev[])
}

#' The frozen hypothesis table (registry section 6) as read from its S: copy; checked against the implemented map.
s32r_hypotheses <- function(path) {
  h <- data.table::fread(path, encoding = "UTF-8", colClasses = "character")
  need <- c("HypothesisID", "FamilyID", "Module", "Metric", "Population", "Window", "Model", "Estimand_test", "Status", "Multiplicity")
  if (!identical(names(h), need) || nrow(h) != 13L || !identical(h$HypothesisID, sprintf("H%02d", 1:13))) stop("Unexpected hypothesis table.", call. = FALSE)
  h[]
}
S32_HYP_MAP <- data.table::data.table(
  HypothesisID = sprintf("H%02d", 1:13),
  FamilyID = c(rep(c("F32-A1-EXP", "F32-A1-CZ", "F32-CC-EXP", "F32-AD-EXP", "F32-AD-CZ", "F32-PRED"), each = 2L), "none"),
  Model = c("A-exp", "A-exp", "A-cz", "A-cz", "B-exp", "B-exp", "C-exp", "C-exp", "C-cz", "C-cz", "F (A vs C)", "F (A vs C)", "H13"),
  model_id = c("A_EXP|crossing_rate", "A_EXP|shared_zone_use", "A_CZ|crossing_rate", "A_CZ|shared_zone_use", "B_EXP|crossing_rate",
               "B_EXP|shared_zone_use", "C_EXP|crossing_rate", "C_EXP|shared_zone_use", "C_CZ|crossing_rate", "C_CZ|shared_zone_use",
               "F|SIS|movement_mean", "F|SIS|primary_behavior_family", "H13|Movement_mean"),
  estimand = c("SIS_minus_CON", "SIS_minus_CON", "CombZ_wb_slope", "CombZ_wb_slope", "Exposure_x_CC_joint", "Exposure_x_CC_joint",
               "SIS_minus_CON_phase_slope", "SIS_minus_CON_phase_slope", "CombZ_wb_phase_slope", "CombZ_wb_phase_slope",
               "Delta R2_LOCO", "Delta R2_LOCO", "Movement_mean:sex_c"),
  kind = c(rep("KR t", 4), rep("KR F(3)", 2), rep("KR t", 4), rep("permutation", 2), "CR2 t"))

#' Forbidden-word check (registry section 9) over every character cell and column name of the tables and the README.
s32r_forbidden <- function(tables, readme) {
  hits <- character()
  scan <- function(v, where) { v <- as.character(v); v <- v[!is.na(v)]
    for (w in S32_FORBIDDEN_WORDS) if (any(grepl(w, v, ignore.case = TRUE))) hits <<- c(hits, paste0(where, ": '", w, "'")) }
  for (nm in names(tables)) { x <- tables[[nm]]; scan(names(x), paste(nm, "(column names)"))
    for (k in names(x)) if (is.character(x[[k]]) || is.factor(x[[k]])) scan(unique(x[[k]]), paste(nm, k)) }
  scan(readme, "README")
  unique(hits)
}

# ---------------------------------------------------------------- writer (once, read-only)
#' CSV-ready copy: POSIXct -> ISO-8601 UTC text (ms), factors -> text, double quotes -> single quotes, and (addendum A1)
#' leading / trailing whitespace trimmed from character cells: data.table::fread strips it on read, so the frozen round-trip
#' check of s30fb_write_csv would otherwise refuse the file (the v1.0 abort).
s32r_prepare <- function(x) {
  x <- data.table::copy(data.table::as.data.table(x))
  for (k in names(x)) {
    v <- x[[k]]
    if (inherits(v, "POSIXct")) data.table::set(x, j = k, value = format(v, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
    else if (is.factor(v)) data.table::set(x, j = k, value = as.character(v))
    if (is.character(x[[k]])) data.table::set(x, j = k, value = trimws(gsub("\"", "'", x[[k]], fixed = TRUE)))
  }
  x[]
}

#' Local staging root (addendum A1): MMM_STAGE32_LOCAL_STAGING if set, else the R session tempdir().
s32r_local_root <- function() { r <- Sys.getenv(S32_LOCAL_STAGING_ENV, ""); normalizePath(if (nzchar(r)) r else tempdir(), winslash = "/", mustWork = FALSE) }

#' Step 1 of the write (local disk only): the 12 tables, each written and round-trip-checked by the frozen s30fb_write_csv,
#' into <local_root>/<run_name>/tables/. Returns the local run folder.
s32r_stage_tables <- function(local_root, run_name, tables) {
  if (!setequal(names(tables), S32_TABLES)) stop("Table set differs from the declared set.", call. = FALSE)
  sd <- file.path(local_root, run_name)
  if (file.exists(sd)) stop("Local staging folder already exists: ", sd, call. = FALSE)
  dir.create(file.path(sd, "tables"), recursive = TRUE)
  for (k in S32_TABLES) s30fb_write_csv(s32r_prepare(tables[[k]]), file.path(sd, "tables", paste0(k, ".csv")))
  sd
}

#' Addendum A1 section B3: sha256 of each locally staged table against the aborted v1.0 staging table.
s32r_determinism <- function(stage_dir, expect = S32_ABORTED$table_sha256) {
  f <- paste0("tables/", names(expect), ".csv"); p <- file.path(stage_dir, f)
  obs <- vapply(p, function(z) if (file.exists(z)) s30fb_sha(z) else NA_character_, "", USE.NAMES = FALSE)
  data.table::data.table(file = f, bytes_this_run = ifelse(file.exists(p), as.numeric(file.size(p)), NA_real_), sha256_this_run = obs,
                         sha256_aborted_v1_0 = unname(expect), identical = !is.na(obs) & obs == unname(expect))
}

#' Step 2 of the write. Completes the local staging folder (audit CSVs, README, QC PNGs rendered beforehand into `png_dir`,
#' audit/output_manifest.csv listing every other file) and verifies it; only then refuses if the S: stage folder holds any
#' v1.1_* / .tmp_v1.1_* entry, copies the folder to .tmp_<run> on S:, verifies every byte there, renames it to <run>, sets
#' every file 0444 and re-verifies. A failure before the S: copy leaves nothing on S:.
s32r_write_run <- function(out_root, run_name, stage_dir, audit, readme, png_dir) {
  if (!setequal(names(audit), S32_AUDIT)) stop("Audit set differs from the declared set.", call. = FALSE)
  if (!identical(basename(stage_dir), run_name)) stop("Local staging folder ", stage_dir, " is not for ", run_name, call. = FALSE)
  tabs <- paste0("tables/", S32_TABLES, ".csv")
  if (!all(file.exists(file.path(stage_dir, tabs))) || file.exists(file.path(stage_dir, "audit"))) stop("Local staging folder is not a fresh table stage: ", stage_dir, call. = FALSE)
  pngs <- sort(list.files(png_dir, pattern = "[.]png$", full.names = TRUE))
  if (!length(pngs)) stop("No QC plot was rendered.", call. = FALSE)
  # local: complete and verify
  dir.create(file.path(stage_dir, "audit")); dir.create(file.path(stage_dir, "qc_plots"))
  for (k in S32_AUDIT) s30fb_write_csv(s32r_prepare(audit[[k]]), file.path(stage_dir, "audit", paste0(k, ".csv")))
  for (p in pngs) if (!file.copy(p, file.path(stage_dir, "qc_plots", basename(p)))) stop("Could not copy ", p, call. = FALSE)
  con <- file(file.path(stage_dir, S32_README), open = "wb"); writeLines(enc2utf8(readme), con, useBytes = TRUE); close(con)
  rel <- sort(c(tabs, paste0("audit/", S32_AUDIT, ".csv"), paste0("qc_plots/", basename(pngs)), S32_README))
  man <- data.table::data.table(file = rel, bytes = as.numeric(file.size(file.path(stage_dir, rel))), sha256 = vapply(file.path(stage_dir, rel), s30fb_sha, "", USE.NAMES = FALSE))
  s30fb_write_csv(man, file.path(stage_dir, S32_MANIFEST_FILE))
  loc <- s32r_verify_written(stage_dir, man, require_read_only = FALSE)
  if (!all(loc$passed)) stop("Local staging verification failed (nothing copied to S:): ", paste(loc$gate[!loc$passed], collapse = "; "), call. = FALSE)
  # S:: copy, verify, rename, protect, re-verify
  blk <- s32r_blocking_entries(out_root)
  if (length(blk)) stop("Refusing to write: an earlier REAL run or staging folder exists: ", paste(file.path(out_root, blk), collapse = ", "), call. = FALSE)
  bd <- file.path(out_root, run_name); stg <- file.path(out_root, paste0(".tmp_", run_name))
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  all_rel <- c(rel, S32_MANIFEST_FILE)
  for (d in unique(dirname(all_rel))) dir.create(if (d == ".") stg else file.path(stg, d), recursive = TRUE, showWarnings = FALSE)
  okc <- file.copy(file.path(stage_dir, all_rel), file.path(stg, all_rel), overwrite = FALSE)
  if (!all(okc)) stop("Could not copy to S: (staging folder left, it blocks further runs): ", paste(all_rel[!okc], collapse = ","), call. = FALSE)
  cp <- s32r_verify_written(stg, man, require_read_only = FALSE)
  if (!all(cp$passed)) stop("S: copy differs from the local staging folder (staging folder left for inspection): ", paste(cp$gate[!cp$passed], collapse = "; "), call. = FALSE)
  if (dir.exists(bd)) stop("Run folder appeared during the write: ", bd, call. = FALSE)
  if (!file.rename(stg, bd)) stop("Could not rename ", stg, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, recursive = TRUE, full.names = TRUE), mode = "0444")
  post <- s32r_verify_written(bd, man)
  if (!all(post$passed)) stop("Post-write verification failed (folder left for inspection): ", paste(post$gate[!post$passed], collapse = "; "), call. = FALSE)
  list(dir = bd, local_dir = stage_dir, manifest = man, manifest_sha256 = s30fb_sha(file.path(bd, S32_MANIFEST_FILE)), post_gates = post)
}
s32r_verify_written <- function(bd, manifest, require_read_only = TRUE) {
  m <- data.table::as.data.table(manifest); mf <- file.path(bd, S32_MANIFEST_FILE)
  back <- if (file.exists(mf)) data.table::fread(mf, colClasses = "character") else data.table::data.table(file = character(), sha256 = character())
  chk <- s30fb_manifest_check(bd, m); extra <- setdiff(s30fb_files_on_disk(bd), c(m$file, S32_MANIFEST_FILE))
  fl <- list.files(bd, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  g <- rbind(s32r_gate("7-write", "output_manifest read back = the manifest written", identical(back$file, m$file) && identical(back$sha256, m$sha256), mf),
             s32r_gate("7-write", "every file = output_manifest (bytes, SHA-256); nothing else in the folder", nrow(chk) > 0L && all(chk$ok) && !length(extra),
                       paste(c(chk$file[!chk$ok], extra), collapse = ",")))
  if (isTRUE(require_read_only)) g <- rbind(g, s32r_gate("7-write", "every written file is read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), bd))
  g
}

# ---------------------------------------------------------------- QC plots (lightweight; descriptive)
S32_COL <- c(CON = "#2a78d6", RES = "#eb6834", SUS = "#1baf7a")   # reference palette slots 1-3 (validated all-pairs)
S32_INK <- c(primary = "#0b0b0b", secondary = "#52514e", grid = "#e6e5e1", surface = "#fcfcfb")
s32r_theme <- function() ggplot2::theme_minimal(base_size = 10) + ggplot2::theme(
  plot.background = ggplot2::element_rect(fill = S32_INK[["surface"]], colour = NA), panel.grid.minor = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_line(colour = S32_INK[["grid"]], linewidth = 0.3), text = ggplot2::element_text(colour = S32_INK[["primary"]]),
  axis.text = ggplot2::element_text(colour = S32_INK[["secondary"]]), legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))

#' Group x phase means (CON / RES / SUS) with thin CON cage-mean lines, one panel per CC.
s32r_plot_phase <- function(desc, which_metric, which_type, ylab, title) {
  g <- desc[metric == which_metric & phase_type == which_type & level == "group" & Group %in% names(S32_COL) & n > 0]
  g <- g[, .(mean = sum(n * mean) / sum(n)), by = .(Group, CC, phase)]
  cm <- desc[metric == which_metric & phase_type == which_type & level == "CON_cage_mean" & n > 0]
  # lines only where a series has more than one phase (e.g. the CC4 light panel holds L1 only: points, no line)
  cm_l <- cm[cm[, .I[data.table::uniqueN(phase) > 1L], by = .(CageEpisodeID, CC)]$V1]
  g_l <- g[g[, .I[data.table::uniqueN(phase) > 1L], by = .(Group, CC)]$V1]
  ggplot2::ggplot() +
    ggplot2::geom_line(data = cm_l, ggplot2::aes(phase, mean, group = CageEpisodeID), colour = S32_COL[["CON"]], alpha = 0.35, linewidth = 0.4) +
    ggplot2::geom_point(data = cm, ggplot2::aes(phase, mean), colour = S32_COL[["CON"]], alpha = 0.35, size = 0.8) +
    ggplot2::geom_line(data = g_l, ggplot2::aes(phase, mean, colour = Group, group = Group), linewidth = 0.9) +
    ggplot2::geom_point(data = g, ggplot2::aes(phase, mean, colour = Group), size = 2.2) +
    ggplot2::facet_wrap(~ CC, nrow = 1, scales = "free_x") + ggplot2::scale_colour_manual(values = S32_COL, name = NULL) +
    ggplot2::labs(x = NULL, y = ylab, title = title,
                  subtitle = "Descriptive means of used animal-phases; thin lines = the CON cage means (one per batch)") + s32r_theme()
}
s32r_plot_prediction <- function(held, perf, nulls) {
  h <- held[population == "SIS" & scheme == "LOCO" & model %in% c("A", "C")]
  h[, panel := ifelse(model == "A", "A: CombZ ~ Batch", paste("C: CombZ ~ Batch +", behaviour_set))]
  p1 <- ggplot2::ggplot(h, ggplot2::aes(prediction, CombZ)) + ggplot2::geom_abline(slope = 1, intercept = 0, colour = S32_INK[["secondary"]], linewidth = 0.3) +
    ggplot2::geom_point(colour = S32_COL[["CON"]], size = 1.6, alpha = 0.8) + ggplot2::facet_wrap(~ panel, nrow = 1) +
    ggplot2::labs(x = "held-out prediction (LOCO by CC1 cage)", y = "observed CombZ", title = "Module F, SIS: held-out predictions, Batch-only vs Batch + behaviour") + s32r_theme()
  p1
}
s32r_plot_null <- function(nulls, tests) {
  n <- nulls[null == "within_batch"]
  ggplot2::ggplot(n, ggplot2::aes(delta_r2)) + ggplot2::geom_histogram(bins = 40, fill = S32_COL[["CON"]], colour = S32_INK[["surface"]], linewidth = 0.2) +
    ggplot2::geom_vline(data = tests, ggplot2::aes(xintercept = delta_r2_loco), colour = S32_COL[["RES"]], linewidth = 0.8) +
    ggplot2::facet_wrap(~ behaviour_set, nrow = 1) +
    ggplot2::labs(x = "Delta R2_LOCO (C - A) under within-Batch permutation of CombZ", y = "draws", title = "Module F, SIS: permutation null (1000 draws) and observed value (line)") + s32r_theme()
}
#' Render the QC plots to `dir` (local, before the S: write). Returns the file paths.
s32r_render_plots <- function(desc, held, perf, nulls, tests, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  P <- list(qc_rate_by_phase = s32r_plot_phase(desc, "crossing_rate", "active", "position changes per hour", "RFID position-change rate, active phases after each cage change"),
            qc_overlap_by_phase = s32r_plot_phase(desc, "shared_zone_use", "active", "shared RFID-position occupancy", "Social-spatial overlap, active phases after each cage change"),
            qc_light_rate_by_phase = s32r_plot_phase(desc, "light_crossing_rate", "light", "position changes per hour", "Light-phase RFID position-change rate"),
            qc_prediction_A_vs_C = s32r_plot_prediction(held, perf, nulls),
            qc_prediction_null = s32r_plot_null(nulls, tests))
  for (nm in names(P)) ggplot2::ggsave(file.path(dir, paste0(nm, ".png")), P[[nm]], width = if (grepl("phase", nm)) 9 else 8, height = 4, dpi = 110, bg = S32_INK[["surface"]])
  file.path(dir, paste0(names(P), ".png"))
}
