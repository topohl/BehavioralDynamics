# ================================================================
# Stage 33 - run machinery: plan pin, inputs, seeds, tables, run id, gates, checkpoints, lint, README, writer
# MMMSociability
# ================================================================
# For Analysis/33_posthoc_cohort_followups.R, implementing sections 4-6 and 14 of the frozen plan
# docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md. Definitions only: sourcing this file reads and writes nothing.
# Requires data.table and digest; the writer needs Functions/stage30_figure_bundle.R (s30fb_write_csv, s30fb_sha,
# s30fb_manifest_check, s30fb_files_on_disk) and Functions/stage32_run.R (s32r_prepare).
# ================================================================

S33_PLAN <- list(
  path = "docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md",
  version = "1.0",
  freeze_commit = "d914f10022821ce4ede0710c4cf61f742c79e3ee",
  sha256_lf = "dd070a950d9084b0db3219ec4013a71f5329728d833273c79ec6311831b30746",
  blob = "8632be0da3a9b4bd25b85f15db824309cf7f1f97")

S33_STAGE_FOLDER <- "posthoc_cohort_followups"
S33_MODULES <- c(A = "A_components", B = "B_presis_arena", C = "C_cc1_cage", D = "D_hourly", E = "E_cagemates")
S33_RUN_ID_PATTERN <- "^v1[.]0_[A-E]{1,5}_[0-9a-f]{7}$"
S33_MAX_PATH_CHARS <- 240L
S33_OPTIN_ENV <- "MMM_S33_REAL_RUN"

# ---------------------------------------------------------------- declared tables (plan sections 8-12 and 14)
S33_TABLES <- list(
  A = c("a01_cohort_component_means", "a02_cohort_offset_split", "a03_cohort_covariance_shares", "a04_con_repairing_reference",
        "a05_component_slopes", "a06_component_slopes_wide", "a07_cort_raw", "a08_weight_episode_gains",
        "a09_weight_gain_models", "a10_s32_multiplicity_rows", "a11_s32_estimate_rows", "a12_cohort_descriptors",
        "a13_bootstrap_scheme_comparison"),
  B = c("b01_covariates_animal", "b02_analysis_dataset", "b03_balance_cohort", "b04_variance_shares", "b05_associations",
        "b06_line_J_animals", "b07_weight_component_coupling", "b08_cc1_board_table", "b09_con_board_rotation",
        "b10_arena_cohort_means", "b11_arena_con_animal_values", "b12_arena_cohort_contrasts",
        "b13_arena_profile_agreement", "b14_arena_within_cohort_association", "b15_appendix_board_offsets"),
  C = c("c01_population", "c02_animal_terms", "c03_cage_means", "c04_information", "c05_reallocation", "c06_models",
        "c07_estimates", "c08_parent_bridge", "c09_influence_summary", "c10_variance", "c11_correlations",
        "c12_board_offsets", "c13_design_sensitivity"),
  D = c("d01_window_metrics_animal", "d02_cohort_window_means", "d03_separation_by_window", "d04_separation_argmax",
        "d05_cohort_cc_table", "d06_rank_correlations", "d07_individual_stability", "d08_recording_start_lags"),
  E = c("e01_exposures_animal", "e02_dyads", "e03_models", "e04_contrasts", "e05_simulation", "e06_sensitivities",
        "e07_age_descriptive", "e08_jackknife_cc1cage"))
S33_AUDIT_TABLES <- list(A = c("a_data_checks", "a_bootstrap_summaries"), B = c("b_join_audit", "b_bootstrap_summaries"),
                         C = c("c_expected_value_checks", "c_placebo_structure_checks"),
                         D = c("d_hourly_additivity", "d_light_alignment"), E = character())
S33_RUN_AUDIT <- c("input_hashes", "code_hashes", "gate_results", "run_manifest", "seeds_and_streams",
                   "outcome_free_products", "lint_results", "checkpoints_used")

# ---------------------------------------------------------------- seeds and replicate counts (plan section 7)
S33_SEEDS <- c(
  A_animal = 33010101L, A_cc1 = 33010102L, A_cc4 = 33010103L, A_s_animal = 33010201L, A_s_cc1 = 33010202L, A_s_cc4 = 33010203L,
  B_e1_rate = 33020101L, B_e2_cz = 33020102L,
  stats::setNames(33020200L + 1:12, paste0("B_sens_", 1:12)),
  B_e9_arena = 33020301L, B_s16 = 33020302L, B_s17 = 33020303L, B_s19 = 33020304L,
  C_realloc = 33030101L, C_r2 = 33030102L, C_r3 = 33030103L, C_r4 = 33030104L,
  C_icc_resid1 = 33030201L, C_icc_resid2 = 33030202L, C_icc_empty = 33030203L,
  C_placebo = 33030301L, C_placebo_1 = 33030311L, C_placebo_2 = 33030312L, C_placebo_3 = 33030313L,
  D_resample = 33040101L,
  E_wild = 33050101L,
  stats::setNames(33050200L + 1:24, paste0("E_scenario_", 1:24)),
  stats::setNames(33050400L + c(1L, 10L, 21L, 22L, 23L), paste0("E_coverage_", c(1L, 10L, 21L, 22L, 23L))))
S33_B <- c(nonparametric = 4000L, parametric = 1999L, parametric_sensitivity = 999L, wild = 9999L, sensitivity = 999L,
           placebo = 19L, coverage_reps = 1000L, coverage_wild = 999L)

# ---------------------------------------------------------------- code pins (plan section 5, GC-1)
S33_CODE_PINS <- list(
  sha256 = c("Functions/stage30_movement.R" = "9a368491", "Functions/rfid_canonical_inference.R" = "886d8bc0",
             "Functions/rfid_event_stream.R" = "3fda3d40", "Functions/rfid_binfree_metrics.R" = "ffc1235d",
             "Functions/stage30_figure_bundle.R" = "db396fcd",
             "Functions/behavior_analysis_config.R" = "f34c762488279f38322f649d2ac6398ebb1f820675436b610cd48417f98fcb2d"),
  blob_commit = "f4c7a31",
  blob_globs = c("Functions/stage32_*.R", "Functions/stage30_*.R", "Functions/rfid_*.R", "Functions/behavior_analysis_config.R"),
  canonical_id_srcref_sha256 = "f7f22a336c219eb8483afcc3ab3ec3318888c9f3911131a0908e2209ba26b622",
  bout_criterion_s = 39.3970988275363)

# ---------------------------------------------------------------- inputs (plan section 4)
#' Every declared input with its expected sha256. Roots come from the project-root accessors, never from a literal.
s33_input_spec <- function(root, ep, pl, sl, a4 = NA_character_, repo) {
  ar <- file.path(root, "analysis_ready")
  bun <- file.path(ar, "canonical/behavior_bundle/ebb_v101_20260929_b2ce507")
  s32 <- file.path(ar, "pipeline/32_behavior_exposure_adaptation/v1.1_f4c7a31")
  s29 <- file.path(ar, "pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507")
  czt <- file.path(ar, "canonical/later_outcome_combz/tables")
  v2 <- file.path(root, "MMMSociability/data_versions/v2_cage_label_correction_2026-09-28")
  row <- function(role, path, sha, module = "core") data.table::data.table(role = role, path = path, sha256 = sha, module = module)
  x <- rbind(
    row("bundle_manifest", file.path(bun, "00_manifest.csv"), "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c"),
    row("bundle_a1", file.path(bun, "A1_animal_cc1.csv"), "c782b38796ad35cc45d246520869ab6f7fd327afd2e197fb4d00407e326b50a4"),
    row("bundle_b1", file.path(bun, "B1_animal_longitudinal.csv"), "2a710a8ad90f98a4bf2f9f1460bf47c8d59e1bbfdc707533a4e68e864b679860"),
    row("bundle_a2", file.path(bun, "A2_prediction_animals.csv"), "5b8ee3fbb92edf06aab1e18b1cee8cc31361f165cbe748fdfaeb1fd9f39d2c22"),
    row("bundle_a4", file.path(bun, "A4_combz_animals.csv"), "50681052284c6ccc3fadf3b5394400062920e15641870f68e1f8069e8625bb38"),
    row("bundle_g", file.path(bun, "G_sample_sizes.csv"), "77a3789b7a4ed3fa57ef19cf83c77e4ac8135dd2445686152b2282e0d4395278"),
    row("bundle_h", file.path(bun, "H_provenance.csv"), "5f77f37e605baad5a427bb59918b24a1a2275ef9f9b9c70e9537972517c62c73"),
    row("combz_table", file.path(czt, "later_outcome_combz_animal_level.csv"), "1f6a2a69c6b0781b8de8da50ecdadf309b62bbc9c82106c0fa91d18119a7de54"),
    row("combz_components", file.path(czt, "combz_component_definition.csv"), "af3c273434c7457b7b28ba39bbc231afc5f161435cf04b588c9de820fab6f70f"),
    row("combz_thresholds", file.path(czt, "combz_classification_thresholds.csv"), "e1536f0c6e45a708770b0b78139232ebf9077eed4b884fd3e03524aad861afc9"),
    row("s32_window_metrics", file.path(s32, "tables/window_metrics_long.csv"), "7f2bfdcc25a9331dc68bc93eddfc03599908208723b94b16c8e753d03c5b2f93"),
    row("s32_coverage", file.path(s32, "tables/coverage_manifest.csv"), "e5352ac5add6a7dc02521952e77c4ce9ed921dbc537026c592bbd363d955e493"),
    row("s32_diagnostics", file.path(s32, "tables/diagnostics.csv"), "d39ef652675a86d4094ee2593635e059fa0acf8ae1ecd533abff7b98166e8fe4"),
    row("s32_estimates", file.path(s32, "tables/estimates.csv"), "76f262ffad6b8f7a184fccc0cbe4852f3bfe59549cf2ad8745a0691ea6dcfc40"),
    row("s32_multiplicity", file.path(s32, "tables/multiplicity.csv"), "3544bcd052e66cef7535eff3ed21d3f7e37e35fe94fd3168bda7c322e0e9b81b"),
    row("s32_output_manifest", file.path(s32, "audit/output_manifest.csv"), "454a794460cdcac34fedf3adace59961c9da17f5247606dcb65a8e7fe78dfc68"),
    row("s29_continuous_estimates", file.path(s29, "tables/continuous_estimates.csv"), "cc549c248778a735f1e79ba2b33e59443778ef200e480ad7479b4150f2b15200"),
    row("s29_output_manifest", file.path(s29, "audit/output_manifest.csv"), "6928bc63b16b3632cf86b5bca85e3b42c929a540898218fea23753c517f87331"),
    row("sus_list", file.path(ep, "sus_animals.csv"), "dea3804b71b479f84900f946e5494b204ed0e8801498da459a6fc781beb9f391"),
    row("con_list", file.path(ep, "con_animals.csv"), "eddd2ee9c3a182f98211b4cbba689e2bb5f72985aeb178f72b25b7d25db1b75d"),
    row("workbook", file.path(ep, "SIS_Analysis/E9_Behavior_Data.xlsx"), "38d200a2062cb01db4817d9bddfd4079ad0d812657506c98c5c1e15d0a6e1127"),
    row("timeline", file.path(pl, "E9_Experimental Timeline.xlsx"), "2b785435bc1083800d3cbef0d13bcc1542499a037aff5554a98c731b738973bb"),
    row("groupcomp_b2", file.path(pl, "GroupCompBatch2.xlsx"), "9e46ba4d4ebbd331c32f59d7ebbcad72c7e6b381f01bb24a850dfdf181ccc865"),
    row("groupcomp_b456", file.path(pl, "GroupComposition.xlsx"), "214f07173199624456fdc47f65c7176ac451c19e55889c18676707c067c73cf4"),
    row("groupcomp_b3", file.path(pl, "GroupCompBatch3.xlsx"), "599f993da245c1be7b35aab8d6eb2040908b383980c89bc55d532c04903e5d63"),
    row("excluded_list", file.path(root, "MMMSociability/raw_data/excluded_animals.csv"), "2e1fb67abd010edcd57fe2a6cebe3788f3bbaa20d77002a7a78371c8396f141a"),
    row("v2_manifest", file.path(v2, "MANIFEST_SHA256.csv"), "bb33a111980913f4ddf97c40872ccdd96dd73accdbdc5c5867954b51aad2c571"),
    row("sleap_geometry", file.path(sl, "qc/source_batch_audit_geometry_rect.csv"), "f95b8fddec1bf46c017f22318e907e948329b2f0700d7bb570c59882b54d7d71", "B"),
    # PLAN ISSUE: the plan names <SL>/configs/; the four configs are under <SL>/provenance/configs/ (same pinned bytes).
    row("sleap_socp", file.path(sl, "provenance/configs/socp_all.yaml"), "30c09ab8a801a0c139f2ab749d03538adec31d9b63158fd3b6883828e9e4b5c2", "B"),
    row("sleap_nor", file.path(sl, "provenance/configs/nor_all.yaml"), "d3ab18755ee3779367569f365106b512b6a1a9355c7e80b3cf1745bffc9c1564", "B"),
    row("sleap_epm", file.path(sl, "provenance/configs/epm_all.yaml"), "8ab6afbf0243ce3f8e5323c4f73e161308acdc0067db0e5f8a4c9021e0be6e07", "B"),
    row("sleap_oft", file.path(sl, "provenance/configs/oft_all.yaml"), "9695813a640ac8848b4c66f010531e82c4fc110d03f8c65ea20beac6a7d10a83", "B"))
  # the 24 raw files (pins of the frozen Stage 32) and the 24 v2 preprocessed files (pins from the v2 manifest; read at run time)
  raw <- data.table::data.table(role = paste0("raw_", sub("^(B[0-9])/E9_SIS_B[0-9]_(CC[0-9])_AnimalPos[.]csv$", "\\1_\\2", names(S32_RAW_SHA256))),
                                path = file.path(root, "MMMSociability/raw_data", names(S32_RAW_SHA256)), sha256 = unname(S32_RAW_SHA256), module = "core")
  exp <- data.table::data.table(
    role = paste0("expected_", c("bw_cc1_primary", "cage_terms", "information_primary", "board_offsets", "design_sensitivity",
                                 "s2_shared_zone_no692", "reallocation")),
    path = file.path(repo, "docs/stage33/expected", c("bw_cc1_primary.csv", "expected_cage_terms.csv", "expected_information_primary.csv",
                                                       "expected_board_offsets.csv", "expected_design_sensitivity.csv",
                                                       "expected_s2_shared_zone_no692.csv", "expected_reallocation.csv")),
    sha256 = c("909e16d1f9024c3915b6ec9d0ca38a166d5ac2e6a8aabcca79a9a05a3bf67c95", "cdc933f318eb6955082a4709b4958943539e54da42a450c8a446a43deee72f24",
               "481e211075bc8cf4ba6f11a71a9c54cd8cbd1d19f44c3e60f8510a03bb139965", "da74215125034a9afc955410baa4aa79f9af24c8e38f7866c0f56f7cecd9c044",
               "7061dfac25d2736c49833cd5262db8dcdefbcfeceb63aa1f7e6fb00d536928b5", "0bd2458f4269b15203551612dbe34985e75ac658329a5f16eb5aa82fd25c7dc5",
               "7a520498b5ad0ce2e80d086bef33e0565ce67828ba5f5137bee99ddc6bbe2c9f"), module = "C")
  a4r <- if (!is.na(a4) && nzchar(a4)) data.table::data.table(
    role = c("a4_report", "a4_components_wide", "a4_cage_noise", "a4_batch_technical", "a4_elisa_dates"),
    path = c(file.path(a4, "BATCH_DECOMPOSITION_REPORT_2026-10-01.md"), file.path(a4, "a4_cohort", c("a4_batch_components_wide.csv",
             "a4_cage_noise_yardstick.csv", "a4_batch_table_technical.csv", "a4_elisa_files_run_dates.csv"))),
    sha256 = c("7f652e27ea800459403bafde7bddaf4c1b83d94219b4924a515d7240e8279831", "26b21b82829e635ac8caffa370e9955bcdbe08ed9a3f250dce886b63cd1053ed",
               "c719f9680aa9937f67b8faa063d1810291695659199b7593cd41b6d5d4f30862", "130a70c431b12fd5683b1352ab61505e2d55d2f56990d10fc292a082586b2a7f",
               "064c2e91e91b93dac5f3f465b3c9c4377b3ce2d262107d5e528b71c767b924ec"), module = "A") else NULL
  out <- rbind(x, raw, exp, a4r)
  if (anyDuplicated(out$role)) stop("Duplicate input role.", call. = FALSE)
  out[]
}

#' The v2 preprocessed files with their manifest pins (sha256_v2); `v2_manifest` must already have passed its own gate.
s33_v2_files <- function(root) {
  v2 <- file.path(root, "MMMSociability/data_versions/v2_cage_label_correction_2026-09-28")
  m <- data.table::fread(file.path(v2, "MANIFEST_SHA256.csv"), showProgress = FALSE)
  data.table::data.table(role = paste0("v2_", sub("^E9_SIS_(B[0-9])_(CC[0-9])_AnimalPos_preprocessed[.]csv$", "\\1_\\2", m$file)),
                         path = file.path(v2, "preprocessed_data", m$file), sha256 = m$sha256_v2, module = "core")
}

#' Named list of input paths for the design and the modules.
s33_paths <- function(spec, root) {
  p <- stats::setNames(as.list(spec$path), spec$role)
  p$bundle_dir <- dirname(p$bundle_a1)
  p$raw_dir <- file.path(root, "MMMSociability/raw_data")
  p$v2_dir <- file.path(root, "MMMSociability/data_versions/v2_cage_label_correction_2026-09-28/preprocessed_data")
  p
}

# ---------------------------------------------------------------- mode, run id and output root (plan section 6)
#' Exactly --real and --modules=<letters A-E, unique, alphabetical>.
s33_parse_args <- function(args) {
  mods <- sub("^--modules=", "", grep("^--modules=", args, value = TRUE))
  ok <- length(args) == 2L && "--real" %in% args && length(mods) == 1L && grepl("^[A-E]{1,5}$", mods) &&
    !anyDuplicated(strsplit(mods, "")[[1]]) && identical(paste(sort(strsplit(mods, "")[[1]]), collapse = ""), mods)
  if (!ok) stop("Usage: Rscript Analysis/33_posthoc_cohort_followups.R --real --modules=<letters A-E, unique, alphabetical>", call. = FALSE)
  strsplit(mods, "")[[1]]
}

s33_run_id <- function(modules, commit) {
  id <- paste0("v1.0_", paste(modules, collapse = ""), "_", substr(commit, 1L, 7L))
  if (!grepl(S33_RUN_ID_PATTERN, id)) stop("Invalid run id: ", id, call. = FALSE)
  id
}

s33_out_root <- function(root) file.path(root, "analysis_ready", "analyses", S33_STAGE_FOLDER)

#' Refusals before anything is computed: the run folder or any .tmp_* entry exists; an earlier run holds a selected
#' module and no rerun reason is given. Returns the earlier run folders (recorded).
s33_output_refusals <- function(out_root, run_id, modules, rerun_reason = "") {
  entries <- if (dir.exists(out_root)) list.files(out_root, all.files = TRUE, no.. = TRUE) else character()
  if (run_id %in% entries) stop("Run folder exists: ", file.path(out_root, run_id), call. = FALSE)
  tmp <- grep("^[.]tmp_", entries, value = TRUE)
  if (length(tmp)) stop("A .tmp_ folder exists (never removed automatically): ", paste(tmp, collapse = ", "), call. = FALSE)
  earlier <- grep("^v1[.]0_[A-E]+_[0-9a-f]{7}$", entries, value = TRUE)
  clash <- earlier[vapply(earlier, function(e) any(strsplit(sub("^v1[.]0_([A-E]+)_.*$", "\\1", e), "")[[1]] %in% modules), TRUE)]
  if (length(clash) && !nzchar(rerun_reason))
    stop("Earlier run(s) hold a selected module (set MMM_S33_RERUN_REASON): ", paste(clash, collapse = ", "), call. = FALSE)
  earlier
}

#' The longest output path must stay within the budget (drive-letter or UNC root form).
s33_path_budget <- function(out_root, run_id, rel_paths, limit = S33_MAX_PATH_CHARS) {
  full <- c(file.path(out_root, paste0(".tmp_", run_id), rel_paths), file.path(out_root, run_id, rel_paths))
  list(ok = all(nchar(full) <= limit), longest = max(nchar(full)), path = full[which.max(nchar(full))])
}

#' Relative output paths of a run with the given modules (used for the budget before computing).
s33_planned_outputs <- function(modules) {
  mod <- unlist(lapply(modules, function(m) c(file.path(S33_MODULES[[m]], "tables", paste0(S33_TABLES[[m]], ".csv")),
                                              if (length(S33_AUDIT_TABLES[[m]])) file.path(S33_MODULES[[m]], "audit", paste0(S33_AUDIT_TABLES[[m]], ".csv")))))
  c("README.txt", file.path("plan", basename(S33_PLAN$path)), file.path("audit", paste0(c(S33_RUN_AUDIT, "output_manifest"), ".csv")),
    "audit/session_info.txt", mod)
}

# ---------------------------------------------------------------- checkpoints (plan section 6)
#' Checkpoint key: sha256 of the plan sha, HEAD, module, step, run mode, the input sha256s and caller extras (frame
#' sha256, seed, B, window set, ...).
s33_checkpoint_key <- function(ctx, module, step, key_extra = list()) {
  digest::digest(list(plan = S33_PLAN$sha256_lf, head = ctx$head %s33or% "", module = module, step = step,
                      mode = ctx$run_mode %s33or% "", inputs = ctx$input_sha256 %s33or% "", extra = key_extra), algo = "sha256")
}

#' Compute `fun()` once per key: a stored value is reused only if its sha256 sidecar matches (otherwise recomputed);
#' writes go to '.part' and are renamed. ctx$checkpoint_log (an environment) records every use. Without a checkpoint
#' folder (or with ctx$no_checkpoints, as in the module C placebo) the value is simply computed.
s33_checkpoint <- function(ctx, module, step, fun, key_extra = list()) {
  if (is.null(ctx$checkpoint_dir) || isTRUE(ctx$no_checkpoints)) return(fun())
  key <- s33_checkpoint_key(ctx, module, step, key_extra)
  dir.create(ctx$checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(ctx$checkpoint_dir, paste0(module, "_", step, "_", substr(key, 1L, 16L), ".rds"))
  side <- paste0(f, ".sha256")
  reused <- file.exists(f) && file.exists(side) &&
    identical(readLines(side, warn = FALSE)[1], digest::digest(file = f, algo = "sha256"))
  if (reused) v <- readRDS(f) else {
    v <- fun(); tmp <- paste0(f, ".part"); saveRDS(v, tmp)
    if (file.exists(f)) unlink(f)
    if (!file.rename(tmp, f)) stop("Could not store checkpoint ", f, call. = FALSE)
    writeLines(digest::digest(file = f, algo = "sha256"), side)
  }
  if (is.environment(ctx$checkpoint_log)) {
    n <- length(ls(ctx$checkpoint_log)) + 1L
    row <- data.table::data.table(module = module, step = step, checkpoint_key = key, file = basename(f), reused = reused)
    assign(sprintf("%06d", n), row, envir = ctx$checkpoint_log)
  }
  v
}

#' Plan-mandated fixed sentences (verbatim from the frozen plan; test_stage33_run.R checks them against the plan text).
#' A cell equal to one of them is not linted: the plan prescribes the wording (B's sentence names 'distance' and
#' 'shared-cause' only to say that no such statement is made).
S33_FIXED_SENTENCES <- c(
  b_selection = paste("B2 and B6 were selected because their SIS A1 RFID position-change rates were the lowest of their sex.",
                      "In the planning dry run (SIS including the untracked B1 animals) B2 and B6 SIS were the highest of their sex in OFT,",
                      "EPM and NOR distance, opposite to their A1 rates. With three cohorts per sex and different constructs, no shared-cause",
                      "statement is made."))
#' Columns the lint skips (plan section 7: engine messages and declared path columns): the core set and each module's
#' declared S33<M>_LINT_EXEMPT_COLUMNS.
S33_LINT_EXEMPT_CORE <- c("messages", "kr_messages", "failure_reason", "path", "raw_file", "SourceFile", "file", "error",
                          "optimizer_check_messages", "formula")
s33_lint_exempt_columns <- function(modules) {
  unique(c(S33_LINT_EXEMPT_CORE, unlist(lapply(modules, function(m) get0(paste0("S33", m, "_LINT_EXEMPT_COLUMNS"), envir = globalenv(),
                                                                        ifnotfound = character())))))
}

# ---------------------------------------------------------------- lint (plan section 7, GC-9)
#' Barred patterns, built from fragments so no barred literal sits in the source. Returns a list of named groups.
s33_lint_patterns <- function(config = NULL) {
  f <- function(...) paste0(...)
  stage <- c(f("acu", "te"), f("immed", "iate"), f("signi", "ficant"), f("confirm", "atory"), f("pre", "registered"), f("pre-", "registered"),
             f("sle", "ep"), f("settled", " state"), f("\\b", "sta", "rs\\b"), f("\\b", "re", "st\\b"), f("expl", "ain"), f("nuis", "ance"),
             f("just ", "batch"), f("batch ", "effect"), f("\\bcaus", "(e|es|ed|ing|al)\\b"), f("\\bdriv", "(e|es|en|ing)\\b"), f("\\blead", "s? to\\b"),
             f("\\bdue ", "to\\b"), f("pred", "ict"), f("bio", "marker"), f("SIS-", "specific"), f("stress-", "specific"), f("group-", "specific"),
             f("cohort-", "wide"), f("adaptation to social ", "instability"), f("loco", "mot"), f("\\bstrang", "er"), f("unfam", "iliar"),
             f("\\bunrel", "ated\\b"), f("no-contact ", "control"), f("stable ", "trait"), f("consistent cohort ", "phenotype"), f("\\bLO", "CO\\b"))
  journals <- c(f("\\bNat", "ure\\b"), f("\\bSci", "ence\\b"), f("\\bCe", "ll\\b"), f("\\bNeu", "ron\\b"), f("\\beLi", "fe\\b"), f("\\bPN", "AS\\b"))
  rfid_labels <- c(f("\\bdist", "ance\\b"), f("explor", "ation"), f("socia", "bility"), f("social ", "coordination"), f("hud", "dling"),
                   f("social ", "preference"), f("nest ", "fidelity"), f("impul", "sivity"), f("\\bactiv", "ity\\b"), f("\\bmove", "ment\\b"))
  if (!is.null(config)) {
    extra <- unique(unlist(lapply(config$metrics, function(m) m$prohibited_labels)))
    rfid_labels <- unique(c(rfid_labels, paste0("\\b", extra, "\\b")))
  }
  pcols <- c("^(p|pval|p_value|statistic|t|t_value|F|F_value|Pr)$", "^p_", "_p$", "Pr[(]")
  list(stage = unique(c(stage, if (exists("S32_FORBIDDEN_WORDS")) S32_FORBIDDEN_WORDS)), journals = journals, rfid_labels = rfid_labels,
       p_columns = pcols, d_estimate_columns = "(?i)minus|diff|delta|contrast|g_sis")
}

#' Lint every table (character cells and column names) and the README. Returns one row per hit.
#' Not scanned: a10/a11 (verbatim registered rows), engine message and path columns, cells equal to a plan-mandated fixed
#' sentence (S33_FIXED_SENTENCES). RFID labels are checked on rows of
#' an RFID metric, on column names (except declared SLEAP columns) and on README lines that are not arena lines.
s33_lint <- function(tables, readme, pats, exempt_columns = S33_LINT_EXEMPT_CORE, fixed_text = S33_FIXED_SENTENCES) {
  hits <- list(); hit <- function(where, pat, text) hits[[length(hits) + 1L]] <<- data.table::data.table(where = where, pattern = pat, text = substr(text, 1, 160))
  scan_txt <- function(where, v, pats_vec, ignore_case = TRUE) for (p in pats_vec) { m <- grepl(p, v, ignore.case = ignore_case, perl = TRUE)
    if (any(m, na.rm = TRUE)) hit(where, p, v[which(m)[1]]) }
  for (nm in names(tables)) {
    if (nm %in% c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows")) next
    x <- tables[[nm]]; cn <- names(x)
    for (p in pats$p_columns) { m <- grepl(p, cn, perl = TRUE); if (any(m)) hit(paste0(nm, " column"), p, cn[which(m)[1]]) }
    if (grepl("^d0[2-7]_", nm)) { m <- grepl(pats$d_estimate_columns, cn, perl = TRUE) & !grepl("^dev_from_", cn)
      if (any(m)) hit(paste0(nm, " column"), pats$d_estimate_columns, cn[which(m)[1]]) }
    sleap_cols <- grepl("(?i)sleap|arena|oft|epm|nor_dist|hab|^s1|^s2", cn, perl = TRUE)
    scan_txt(paste0(nm, " column"), cn[!sleap_cols], pats$rfid_labels)
    scan_txt(paste0(nm, " column"), cn, c(pats$stage))
    chr <- setdiff(cn[vapply(x, is.character, TRUE)], exempt_columns)
    for (k in chr) {
      v <- unique(x[[k]]); v <- v[!is.na(v) & nzchar(v) & !trimws(v) %in% fixed_text]; if (!length(v)) next
      scan_txt(paste(nm, k), v, pats$stage); scan_txt(paste(nm, k), v, pats$journals, ignore_case = FALSE)
      if ("metric_id" %in% cn) {
        rf <- unique(x[metric_id %in% c("crossing_rate", "shared_zone_use", "Movement_mean")][[k]])
        rf <- rf[!is.na(rf) & nzchar(rf) & !trimws(rf) %in% fixed_text]; if (length(rf)) scan_txt(paste(nm, k, "(RFID rows)"), rf, pats$rfid_labels)
      }
    }
  }
  rl <- readme[!grepl("(?i)arena|sleap", readme, perl = TRUE)]
  scan_txt("README", readme, pats$stage); scan_txt("README", readme, pats$journals, ignore_case = FALSE); scan_txt("README", rl, pats$rfid_labels)
  if (length(hits)) data.table::rbindlist(hits) else data.table::data.table(where = character(), pattern = character(), text = character())
}

# ---------------------------------------------------------------- code, plan, package and input gates (GC-1 to GC-5)
s33_git <- function(repo, ...) suppressWarnings(system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = TRUE))

#' GC-1: Stage 33 files committed and clean; working-tree sha256 of the frozen helpers; their git blob equals the blob at
#' f4c7a31; canonical_animal_id srcref sha256 and probe; bout criterion.
s33_code_gates <- function(repo, stage33_files, canon_info, config) {
  G <- list(); gate <- function(id, name, ok, detail = "") G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, detail = detail)
  st <- s33_git(repo, "status", "--porcelain", "--untracked-files=all", "--", stage33_files)
  tracked <- s33_git(repo, "ls-files", "--", stage33_files)
  gate("GC-1a", "Stage 33 files committed and clean", c(length(st) == 0L, setequal(tracked, stage33_files)), paste(c(st, setdiff(stage33_files, tracked)), collapse = ";"))
  pins <- S33_CODE_PINS$sha256
  obs <- vapply(names(pins), function(p) digest::digest(file = file.path(repo, p), algo = "sha256"), "")
  gate("GC-1b", "frozen helpers: working-tree sha256 = the pins", startsWith(obs, pins), paste(names(pins)[!startsWith(obs, pins)], collapse = ","))
  frozen <- s33_git(repo, "ls-files", "--", S33_CODE_PINS$blob_globs)
  now <- vapply(frozen, function(p) s33_git(repo, "hash-object", "--", p)[1], "")
  then <- vapply(frozen, function(p) s33_git(repo, "rev-parse", paste0(S33_CODE_PINS$blob_commit, ":", p))[1], "")
  head_blob <- vapply(frozen, function(p) s33_git(repo, "rev-parse", paste0("HEAD:", p))[1], "")
  gate("GC-1c", "frozen code: working tree and HEAD blob = blob at f4c7a31", c(length(frozen) > 0L, now == then, head_blob == then),
       paste(frozen[now != then | head_blob != then], collapse = ","))
  gate("GC-1d", "canonical_animal_id: one assignment, srcref sha256 pinned, probe answer",
       c(identical(canon_info$srcref_sha256, S33_CODE_PINS$canonical_id_srcref_sha256), isTRUE(canon_info$probe_ok)))
  gate("GC-1e", "bout criterion of the frozen configuration", identical(config$metrics$fragmentation$bout_criterion_s, S33_CODE_PINS$bout_criterion_s))
  data.table::rbindlist(G)
}

#' The plan's text with LF line endings and its sha256.
s33_plan_sha_lf <- function(path) {
  lf <- gsub("\r\n", "\n", rawToChar(readBin(path, "raw", file.size(path))), fixed = TRUE)
  list(text = lf, sha256 = digest::digest(lf, algo = "sha256", serialize = FALSE))
}

#' GC-2: the plan (LF sha256, git blob unchanged since the freeze commit, which is an ancestor of HEAD; status FROZEN).
s33_plan_gates <- function(repo) {
  ps <- s33_plan_sha_lf(file.path(repo, S33_PLAN$path)); lf <- ps$text; sha <- ps$sha256
  anc <- identical(attr(suppressWarnings(system2("git", c("-C", shQuote(repo), "merge-base", "--is-ancestor", S33_PLAN$freeze_commit, "HEAD"),
                                                 stdout = FALSE, stderr = FALSE)), "status") %s33or% 0L, 0L)
  blob_head <- s33_git(repo, "rev-parse", paste0("HEAD:", S33_PLAN$path))[1]
  log <- s33_git(repo, "log", "--format=%H", paste0(S33_PLAN$freeze_commit, "..HEAD"), "--", S33_PLAN$path)
  s33_gate_rows("GC-2", "plan: LF sha256 and blob pinned; unchanged since the freeze commit (an ancestor of HEAD); status FROZEN",
                c(identical(sha, S33_PLAN$sha256_lf), identical(blob_head, S33_PLAN$blob), anc, length(log) == 0L,
                  grepl("**Status.** FROZEN", lf, fixed = TRUE)))
}

#' GC-3: R version and the frozen package versions.
s33_package_gates <- function(expected = S32_PACKAGES, r_version = S32_R_VERSION) {
  vers <- vapply(names(expected), function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), "")
  s33_gate_rows("GC-3", "R 4.5.1 ucrt and the frozen package versions", c(identical(R.version.string, r_version), vers == expected),
                paste(names(vers)[is.na(vers) | vers != expected], collapse = ","))
}

#' GC-5: every declared input exists and matches its sha256.
s33_input_gates <- function(spec) {
  obs <- vapply(spec$path, function(p) if (file.exists(p)) digest::digest(file = p, algo = "sha256") else NA_character_, "", USE.NAMES = FALSE)
  out <- data.table::copy(spec)[, `:=`(bytes = as.numeric(file.size(path)), observed_sha256 = obs, passed = !is.na(obs) & obs == sha256)]
  list(table = out, gate = s33_gate_rows("GC-5", "declared inputs present and matching their sha256", out$passed,
                                         detail = paste(out[passed == FALSE, role], collapse = ",")))
}

# ---------------------------------------------------------------- outcome-free products (plan section 6)
#' sha256 of the s30fb_write_csv serialization of each outcome-free product (re-extractable from the written tables).
s33_product_hashes <- function(products, module) {
  data.table::rbindlist(lapply(names(products), function(nm) {
    f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
    s30fb_write_csv(s32r_prepare(products[[nm]]), f)
    data.table::data.table(module = module, product = nm, rows = nrow(products[[nm]]), columns = ncol(products[[nm]]),
                           sha256 = digest::digest(file = f, algo = "sha256"))
  }))
}

# ---------------------------------------------------------------- recorded deviations (README; plan section 6)
#' Readings of the plan that differ from its literal text, each marked 'PLAN ISSUE' or explained at its code site.
S33_DEVIATIONS <- data.table::data.table(module = c("core", "core", "core"), text = c(
  "The four SLEAP configuration files are read from <SLEAP release>/provenance/configs/ (the plan names <SLEAP release>/configs/); the pinned bytes are the same.",
  "GC-6q compares board first records with Stage 32 board_first except on the boards whose labels v2 corrected (B1 CC2, B6 CC4).",
  "The lint (GC-9) skips cells equal to the plan's fixed B2/B6 selection sentence (section 9, E10), which the plan prescribes verbatim."))
s33_deviations <- function(modules) S33_DEVIATIONS[module %in% c("core", modules), paste0(module, ": ", text)]

# ---------------------------------------------------------------- README (plan section 6)
#' Interval counts per module for the README (plan 17.1 item 6): pairs of finite limits in any column pair named alike
#' but for 'low' / 'high' (ci_low / ci_high, resamp_low / resamp_high, ci_low_S13 / ci_high_S13, ...), over the module
#' tables except the verbatim copies a10/a11.
s33_interval_counts <- function(results, modules) {
  stats::setNames(vapply(modules, function(m) {
    tabs <- results[[m]]$tables[setdiff(names(results[[m]]$tables), c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows"))]
    as.integer(sum(vapply(tabs, function(x) {
      lo <- grep("(^|_)low(_|$)", names(x), value = TRUE); hi <- sub("(^|_)low(_|$)", "\\1high\\2", lo); keep <- hi %in% names(x)
      sum(vapply(which(keep), function(i) sum(is.finite(suppressWarnings(as.numeric(x[[lo[i]]]))) &
                                                is.finite(suppressWarnings(as.numeric(x[[hi[i]]])))), 0)) }, 0)))
  }, 0L), modules)
}

s33_readme <- function(run_id, modules, commit, tables_by_module, gate_counts, deviations = character(), earlier = character(),
                       rerun_reason = "", partial = FALSE, interval_counts = integer()) {
  c(paste0("Stage 33 post hoc cohort follow-ups, run ", run_id),
    "POST HOC, descriptive estimation only; not registered; not a re-test of any registered hypothesis.",
    paste0("Modules: ", paste(modules, collapse = ", "), if (partial) " (partial run: cross-module gates without a partner are recorded as not evaluated)" else ""),
    paste0("Code commit: ", commit, "; plan ", S33_PLAN$path, " (LF sha256 ", S33_PLAN$sha256_lf, ", frozen at ", substr(S33_PLAN$freeze_commit, 1, 7), ")."),
    paste0("Written: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    "Stage 33 reports no p-value; p-values produced internally by the frozen engine are discarded before any table is built; registered Stage 32 p-values appear only as verbatim copies in A_components/tables/a10 and a11 (runs that include module A).",
    "Inputs: the ebb_v101 bundle, the canonical later-outcome tables, Stage 29 run v101_dv2_b2ce507, Stage 32 run v1.1_f4c7a31, the endpoint lists and workbook, the planning files, the raw and v2 RFID files; sha256 of every input in audit/input_hashes.csv.",
    "Intervals: nominal 95% (method and basis in each row: interval_method, interval_basis). Percentile intervals from about 4 cages per cohort undercover (about 80% coverage) and are labelled cage_resampling_range. Intervals that use a CON cohort mean depend on the single CON cage of the cohort and are anti-conservative.",
    "Rates are RFID position-change rates: position changes per observed hour, counting vendor position updates of at least 200 grid units.",
    "With no association anywhere, about 1 in 20 nominal 95% intervals, and more of the cage-resampling ranges, would exclude 0.",
    if (length(interval_counts)) paste0("Intervals reported (pairs of finite limits, every method and basis): ",
                                        paste(paste(names(interval_counts), interval_counts), collapse = ", "), "; ", sum(interval_counts), " in total.")
    else character(),
    "Cohort-level and individual-level results are reported separately; six cohorts cannot separate cohort biology from cohort-level procedure, assay runs or CC1 body size.",
    "Tables:", unlist(lapply(names(tables_by_module), function(m) paste0("  ", S33_MODULES[[m]], "/tables/", tables_by_module[[m]], ".csv"))),
    paste0("Gates: ", gate_counts),
    if (length(deviations)) c("Deviations from the plan:", paste0("  - ", deviations)) else "Deviations from the plan: none recorded.",
    if (length(earlier)) paste0("Earlier runs in this folder: ", paste(earlier, collapse = ", ")) else "Earlier runs in this folder: none.",
    if (nzchar(rerun_reason)) paste0("Rerun reason: ", rerun_reason) else character())
}

# ---------------------------------------------------------------- the run folder's file set (plan section 14)
#' Phase 5 file set: the module tables and audit tables under their module folders, the run audit (section 5), the plan
#' copy, session_info and the README. `run` holds repo, run_id, commit, root, modules, stage33_files, earlier, rerun_reason,
#' timings, input_table, prod_hashes, gates_all, lint, checkpoints and readme. Stops unless the set (plus the writer's
#' output_manifest) equals s33_planned_outputs(modules).
s33_assemble_files <- function(results, run) {
  files <- list()
  for (m in run$modules) {
    for (nm in names(results[[m]]$tables)) files[[file.path(S33_MODULES[[m]], "tables", paste0(nm, ".csv"))]] <- results[[m]]$tables[[nm]]
    for (nm in names(results[[m]]$audit)) files[[file.path(S33_MODULES[[m]], "audit", paste0(nm, ".csv"))]] <- results[[m]]$audit[[nm]]
  }
  files[["audit/input_hashes.csv"]] <- run$input_table
  files[["audit/code_hashes.csv"]] <- data.table::data.table(file = run$stage33_files,
    sha256 = vapply(run$stage33_files, function(p) digest::digest(file = file.path(run$repo, p), algo = "sha256"), "", USE.NAMES = FALSE),
    blob = vapply(run$stage33_files, function(p) s33_git(run$repo, "rev-parse", paste0("HEAD:", p))[1], "", USE.NAMES = FALSE))
  files[["audit/gate_results.csv"]] <- run$gates_all
  ck <- run$checkpoints
  # "key" cannot be a data.table() argument name (it sets the key): build as field, then rename (as Stage 30)
  files[["audit/run_manifest.csv"]] <- data.table::setnames(data.table::data.table(
    field = c("run_id", "modules", "commit", "branch", "plan_sha256", "R", "root_form", "earlier_runs", "rerun_reason",
            "checkpoints_reused", paste0("seconds_", names(run$timings))),
    value = c(run$run_id, paste(run$modules, collapse = ""), run$commit, s33_git(run$repo, "branch", "--show-current")[1] %s33or% "",
              S33_PLAN$sha256_lf, R.version.string, if (grepl("^//", run$root)) "UNC" else "drive", paste(run$earlier, collapse = ";"),
              run$rerun_reason %s33or% "", if (nrow(ck)) sum(ck$reused) else 0L, vapply(run$timings, function(t) sprintf("%.1f", t), ""))),
    "field", "key")
  files[["audit/seeds_and_streams.csv"]] <- data.table::data.table(stream = names(S33_SEEDS), seed = unname(S33_SEEDS),
    rng = paste(RNGkind(), collapse = "/"))
  files[["audit/outcome_free_products.csv"]] <- data.table::rbindlist(run$prod_hashes)
  files[["audit/lint_results.csv"]] <- run$lint
  files[["audit/checkpoints_used.csv"]] <- if (nrow(ck)) ck else data.table::data.table(module = character(), step = character(),
                                                                                       checkpoint_key = character(), file = character(), reused = logical())
  files[["audit/session_info.txt"]] <- utils::capture.output(utils::sessionInfo())
  files[[file.path("plan", basename(S33_PLAN$path))]] <- readLines(file.path(run$repo, S33_PLAN$path), warn = FALSE)
  files[["README.txt"]] <- run$readme
  planned <- s33_planned_outputs(run$modules)
  got <- c(names(files), "audit/output_manifest.csv")
  if (!setequal(got, planned) || anyDuplicated(got))
    stop("Run file set differs from the plan: missing ", paste(setdiff(planned, got), collapse = ","), "; extra ",
         paste(setdiff(got, planned), collapse = ","), call. = FALSE)
  files
}

# ---------------------------------------------------------------- the writer (plan section 6, Phase 5)
#' Stage the run locally, verify it, re-hash the inputs (GC-10), copy to <out_root>/.tmp_<run_id>, verify, rename,
#' make read-only and verify again. `files` = named list: relative path -> data.table (written with s30fb_write_csv after
#' s32r_prepare) or character (written as text lines). Returns the manifest and the checks.
s33_write_run <- function(local_root, out_root, run_id, files, spec, pre_copy_check = function() TRUE) {
  sd <- file.path(local_root, "staging", run_id)
  if (file.exists(sd)) unlink(sd, recursive = TRUE)
  dir.create(sd, recursive = TRUE)
  for (rel in names(files)) {
    p <- file.path(sd, rel); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); x <- files[[rel]]
    if (is.character(x)) { con <- file(p, open = "wb"); writeLines(enc2utf8(x), con, useBytes = TRUE); close(con) }
    else s30fb_write_csv(s32r_prepare(data.table::as.data.table(x)), p)
  }
  rel <- sort(names(files))
  man <- data.table::data.table(file = rel, bytes = as.numeric(file.size(file.path(sd, rel))),
                                sha256 = vapply(file.path(sd, rel), s30fb_sha, "", USE.NAMES = FALSE))
  dir.create(file.path(sd, "audit"), showWarnings = FALSE)
  s30fb_write_csv(man, file.path(sd, "audit", "output_manifest.csv"))
  verify <- function(dir, ro = FALSE) {
    chk <- s30fb_manifest_check(dir, man)
    extra <- setdiff(s30fb_files_on_disk(dir), c(man$file, "audit/output_manifest.csv"))
    back <- data.table::fread(file.path(dir, "audit", "output_manifest.csv"), colClasses = "character")
    fl <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    c(manifest_read_back = identical(back$file, man$file) && identical(back$sha256, man$sha256),
      files_match = nrow(chk) > 0L && all(chk$ok) && !length(extra), read_only = !ro || all(file.access(fl, 2L) != 0L))
  }
  loc <- verify(sd)
  if (!all(loc)) stop("Local staging verification failed; nothing copied: ", paste(names(loc)[!loc], collapse = ","), call. = FALSE)
  ig <- s33_input_gates(spec)
  if (!isTRUE(ig$gate$passed)) stop("An input changed during the run: ", ig$gate$detail, call. = FALSE)
  if (!isTRUE(pre_copy_check())) stop("GC-10 pre-copy check failed.", call. = FALSE)
  bd <- file.path(out_root, run_id); tmp <- file.path(out_root, paste0(".tmp_", run_id))
  if (file.exists(bd) || file.exists(tmp)) stop("Run folder or .tmp_ folder appeared: ", bd, call. = FALSE)
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  all_rel <- c(rel, "audit/output_manifest.csv")
  for (d in unique(dirname(all_rel))) dir.create(if (d == ".") tmp else file.path(tmp, d), recursive = TRUE, showWarnings = FALSE)
  okc <- file.copy(file.path(sd, all_rel), file.path(tmp, all_rel), overwrite = FALSE)
  if (!all(okc)) stop("Copy failed (", tmp, " left; it blocks further runs): ", paste(all_rel[!okc], collapse = ","), call. = FALSE)
  cp <- verify(tmp)
  if (!all(cp)) stop("Copy differs from the local staging folder (", tmp, " left for inspection)", call. = FALSE)
  if (!file.rename(tmp, bd)) stop("Could not rename ", tmp, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, recursive = TRUE, full.names = TRUE), mode = "0444")
  post <- verify(bd, ro = TRUE)
  if (!all(post)) stop("Post-write verification failed (", bd, " left for inspection)", call. = FALSE)
  list(dir = bd, staging = sd, manifest = man, manifest_sha256 = s30fb_sha(file.path(bd, "audit", "output_manifest.csv")),
       checks = data.table::data.table(stage = c("local", "copy", "final"), passed = c(all(loc), all(cp), all(post))))
}

# ---------------------------------------------------------------- gate registry (plan section 5)
#' The core and cross-module gates: id, phase, kind ('hard' stops the run; 'recorded' never stops) and the module a gate
#' belongs to (NA = every run). Module gates (GA-*, GB-*, GC-C*, GD-*, GE-*) are kept by their modules. GC-10 is the
#' writer's pre-copy check (it stops inside s33_write_run() and leaves no row).
S33_GATES <- data.table::data.table(
  gate_id = c(paste0("GC-1", letters[1:5]), "GC-2", "GC-3", "GC-4", "GC-5", paste0("GC-6", letters[1:23]), paste0("GC-7", letters[1:4]),
              paste0("GC-8", LETTERS[1:5]), "GC-8x", "GC-9", paste0("X", 1:8), "GC-10", "GC-11"),
  phase = c(rep(1L, 9L), rep(2L, 23L), rep(3L, 4L), rep(4L, 6L), 4L, rep(4L, 8L), 5L, 5L),
  kind = c(rep("hard", 9L + 23L + 4L + 6L + 1L + 7L), "recorded", "hard", "recorded"),
  module = c(rep(NA_character_, 9L + 23L + 4L), LETTERS[1:5], rep(NA_character_, 1L + 1L + 8L + 2L)))

#' GC-8x: every registered gate of Phases 1-4 that applies to the run was recorded (cross-module gates as held or 'not
#' evaluated') and no unregistered core or cross-module id appears.
s33_registry_gate <- function(gates_all, modules) {
  want <- S33_GATES[phase <= 4L & gate_id != "GC-8x" & (is.na(module) | module %in% modules), gate_id]
  got <- unique(gates_all$gate_id)
  unknown <- setdiff(grep("^(GC-[0-9]|X[0-9])", got, value = TRUE), S33_GATES$gate_id)
  s33_gate_rows("GC-8x", "every registered gate of Phases 1-4 recorded (section 13 held or not evaluated); no unregistered core id",
                c(want %in% got, length(unknown) == 0L), n_expected = length(want) + 1L,
                detail = paste(c(if (length(setdiff(want, got))) paste("missing", paste(setdiff(want, got), collapse = ",")),
                                 if (length(unknown)) paste("unregistered", paste(unknown, collapse = ","))), collapse = "; "))
}

# ---------------------------------------------------------------- post-fit gates (plan sections 5 and 13)
#' GC-8: every declared table and audit table present; a 'tier' column on all tables but a10/a11; estimate tables carry
#' level, units, lead, interval_basis and metric_label, with levels from the declared vocabulary.
s33_table_gates <- function(results, modules) {
  rows <- list()
  for (m in modules) {
    r <- results[[m]]
    present <- c(identical(sort(names(r$tables)), sort(S33_TABLES[[m]])), identical(sort(names(r$audit) %s33or% character()), sort(S33_AUDIT_TABLES[[m]])))
    tabs <- c(r$tables, r$audit)
    tier_ok <- vapply(names(tabs), function(nm) nm %in% c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows") || "tier" %in% names(tabs[[nm]]), TRUE)
    est <- names(r$tables)[vapply(r$tables, function(x) "estimate" %in% names(x) || "level" %in% names(x), TRUE)]
    est <- setdiff(est, c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows"))
    cols_ok <- vapply(est, function(nm) all(c("level", "units", "lead", "interval_basis", "metric_label") %in% names(r$tables[[nm]])), TRUE)
    lev_ok <- vapply(est, function(nm) { lv <- r$tables[[nm]]$level; all(is.na(lv) | lv %in% S33_LEVELS) }, TRUE)
    rows[[m]] <- s33_gate_rows(paste0("GC-8", m), paste0("module ", m, ": declared tables, tier column, section-7 columns and levels on estimate rows"),
                               c(present, tier_ok, cols_ok, lev_ok),
                               detail = paste(c(names(tier_ok)[!tier_ok], names(cols_ok)[!cols_ok], names(lev_ok)[!lev_ok]), collapse = ","))
  }
  data.table::rbindlist(rows)
}

#' Phase 4, before the gates: C's S3 delta_peer per frozen SD is placed beside E's S15 row (plan section 12, S15) when C
#' and E are both in the run (e06 column c_s3_delta_peer_per_frozen_sd; it stays NA otherwise).
s33_cross_fill <- function(results, modules) {
  if (!all(c("C", "E") %in% modules)) return(results)
  v <- results$C$cross$s3_delta_peer_per_sd
  if (length(v) != 1L) stop("C's S3 delta_peer per frozen SD must be one value (", length(v), " found).", call. = FALSE)
  e06 <- data.table::copy(results$E$tables$e06_sensitivities)
  if (!"c_s3_delta_peer_per_frozen_sd" %in% names(e06) || e06[sensitivity_id == "S15", .N] != 1L)
    stop("e06 needs one S15 row and the column c_s3_delta_peer_per_frozen_sd.", call. = FALSE)
  e06[sensitivity_id == "S15", c_s3_delta_peer_per_frozen_sd := v]
  results$E$tables$e06_sensitivities <- e06
  results
}

#' Cross-module gates X1-X8 (plan section 13), from the module tables and the modules' `cross` exports
#' (results[[m]]$cross). 'Not evaluated' when a partner module is absent (X1 and X4 need two of their modules); X8 is
#' recorded. A missing column or export fails its gate with the error as detail.
s33_cross_gates <- function(results, p2, modules) {
  G <- list(); add <- function(g) G[[length(G) + 1L]] <<- g
  tb <- function(m, nm) { x <- results[[m]]$tables[[nm]]; if (is.null(x)) stop("table ", nm, " missing", call. = FALSE); data.table::as.data.table(x) }
  cx <- function(m, nm) { x <- results[[m]]$cross[[nm]]; if (is.null(x)) stop("module ", m, " exports no cross$", nm, call. = FALSE); x }
  num_eq <- function(a, b, tol) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= tol)
  chr_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  run <- function(id, text, need, f, hard = TRUE, min_present = length(need)) {
    present <- intersect(need, modules)
    if (length(present) < min_present)
      return(add(s33_gate_not_evaluated(id, text, paste0("module(s) ", paste(setdiff(need, modules), collapse = ","), " not in this run"))))
    r <- tryCatch(f(present), error = function(e) list(ok = NA, detail = paste("error:", conditionMessage(e))))
    add(s33_gate_rows(id, text, r$ok, hard = hard, detail = paste0(r$detail, if (length(present) < length(need))
      paste0(" (evaluated over ", paste(present, collapse = ","), ")") else "")))
  }
  # X1 recording-start lags (CC1, by cohort)
  run("X1", "CC1 recording-start lags identical in a12, b03 and b01, d08", c("A", "B", "D"), min_present = 2L, function(ms) {
    lags <- list()
    if ("A" %in% ms) { x <- cx("A", "lags"); lags$A <- stats::setNames(x$lag_cc1_h, x$Batch) }
    if ("B" %in% ms) { x3 <- unique(tb("B", "b03_balance_cohort")[, .(Batch, lag_h_cc1)]); x1 <- unique(tb("B", "b01_covariates_animal")[, .(Batch, lag_h_cc1)])
      lags$B_b03 <- stats::setNames(x3$lag_h_cc1, x3$Batch); lags$B_b01 <- stats::setNames(x1$lag_h_cc1, x1$Batch) }
    if ("D" %in% ms) { x <- tb("D", "d08_recording_start_lags")[CC == "CC1"]; lags$D <- stats::setNames(x$lag_h, x$Batch) }
    shape <- vapply(lags, function(v) length(v) == 6L && setequal(names(v), S33_COHORTS) && !anyNA(v), TRUE)
    ref <- lags[[1]]
    same <- vapply(lags[-1], function(v) isTRUE(all(shape)) && all(num_eq(v[S33_COHORTS], ref[S33_COHORTS], 1e-9)), TRUE)
    list(ok = c(shape, same), detail = paste0("sources ", paste(names(lags), collapse = ","), "; max |difference| ",
      if (all(shape)) sprintf("%.3g h", max(vapply(lags, function(v) max(abs(v[S33_COHORTS] - ref[S33_COHORTS])), 0))) else "n/a"))
  })
  # X2 A's x_RU means and CR2 rows x 6 = D's d05 SIS CC1 rows
  run("X2", "A's SIS x_RU cohort means and CR2 rows x 6 = D's d05 SIS CC1 rate rows (1e-9)", c("A", "D"), function(ms) {
    a <- data.table::as.data.table(cx("A", "x_RU_sis_cc1_rows"))
    d <- tb("D", "d05_cohort_cc_table")[CC == "CC1" & exposure_set == "SIS" & metric == "crossing_rate"]
    m <- merge(a, d, by = "Batch")
    ok <- c(nrow(a) == 6L, nrow(d) == 6L, nrow(m) == 6L,
            all(num_eq(6 * m$estimate, m$mean, 1e-9)), all(num_eq(6 * m$cr2_cc1_se, m$se_cr2, 1e-9)), all(num_eq(m$cr2_cc1_df, m$df_satt, 1e-9)),
            all(num_eq(6 * m$cr2_cc1_ci_low, m$ci_low, 1e-9)), all(num_eq(6 * m$cr2_cc1_ci_high, m$ci_high, 1e-9)))
    list(ok = ok, detail = sprintf("%d cohorts; max |6 x A - D| mean %.3g, interval %.3g", nrow(m), max(abs(6 * m$estimate - m$mean)),
                                   max(abs(c(6 * m$cr2_cc1_ci_low - m$ci_low, 6 * m$cr2_cc1_ci_high - m$ci_high)), na.rm = TRUE, 0)))
  })
  # X3 C cages = A's cage_sd_reference_cc1 cages; C focal = E focal
  run("X3", "C's CC1 cages = A's cage_sd_reference_cc1 cages; C's focal animals = E's focal animals", c("A", "C", "E"), min_present = 2L, function(ms) {
    if (!"C" %in% ms) stop("module C not in this run")
    cc <- cx("C", "cages"); cf <- cx("C", "focal"); ok <- c(!anyDuplicated(cc), !anyDuplicated(cf)); det <- character()
    if ("A" %in% ms) { ac <- cx("A", "cage_sd_reference_cc1_cages"); ok <- c(ok, !anyDuplicated(ac), setequal(ac, cc), length(ac) == length(cc))
      det <- c(det, sprintf("cages C %d, A %d", length(cc), length(ac))) }
    if ("E" %in% ms) { ef <- tb("E", "e01_exposures_animal")[focal %in% TRUE, AnimalNum]; ok <- c(ok, setequal(ef, cf), length(ef) == length(cf), length(cf) == 85L)
      det <- c(det, sprintf("focal C %d, E %d", length(cf), length(ef))) }
    list(ok = ok, detail = paste(det, collapse = "; "))
  })
  # X4 tp2 and source cages identical in A, B, C, E (on the animals each pair shares)
  run("X4", "tp2 and source cages identical in A, B, C and E (animals shared by each pair)", c("A", "B", "C", "E"), min_present = 2L, function(ms) {
    src <- list()
    if ("A" %in% ms) src$A <- data.table::as.data.table(cx("A", "tp2"))[, .(AnimalNum, tp2, src_cage)]
    if ("B" %in% ms) src$B <- tb("B", "b01_covariates_animal")[, .(AnimalNum, tp2, src_cage)]
    if ("C" %in% ms) src$C <- merge(data.table::as.data.table(cx("C", "tp2")), data.table::as.data.table(cx("C", "src_cage")), by = "AnimalNum", all = TRUE)
    if ("E" %in% ms) src$E <- tb("E", "e01_exposures_animal")[, .(AnimalNum, tp2, src_cage)]
    pr <- utils::combn(names(src), 2L); ok <- logical(); det <- character()
    for (k in seq_len(ncol(pr))) {
      a <- src[[pr[1, k]]]; b <- src[[pr[2, k]]]; m <- merge(a, b, by = "AnimalNum", suffixes = c(".a", ".b"))
      ok <- c(ok, !anyDuplicated(a$AnimalNum), !anyDuplicated(b$AnimalNum), nrow(m) > 0L,
              all(num_eq(m$tp2.a, m$tp2.b, 1e-12)), all(chr_eq(as.character(m$src_cage.a), as.character(m$src_cage.b))))
      det <- c(det, sprintf("%s-%s %d animals", pr[1, k], pr[2, k], nrow(m)))
    }
    list(ok = ok, detail = paste(det, collapse = "; "))
  })
  # X5 cc4_cage identical in A and B
  run("X5", "cc4_cage identical in A and B (shared animals)", c("A", "B"), function(ms) {
    a <- data.table::as.data.table(cx("A", "cc4_cage"))[, .(AnimalNum, cc4_cage)]; b <- tb("B", "b01_covariates_animal")[, .(AnimalNum, cc4_cage)]
    m <- merge(a, b, by = "AnimalNum", suffixes = c(".a", ".b"))
    list(ok = c(!anyDuplicated(a$AnimalNum), nrow(m) == nrow(a), all(chr_eq(as.character(m$cc4_cage.a), as.character(m$cc4_cage.b)))),
         detail = sprintf("%d of A's %d animals in b01", nrow(m), nrow(a)))
  })
  # X6 identical s1 in C and E
  run("X6", "identical s1 (frozen SD of the CC1 A1 rate) in C and E", c("C", "E"), function(ms) {
    cs <- cx("C", "s1"); es <- unique(tb("E", "e01_exposures_animal")$s1)
    list(ok = c(length(cs) == 1L, length(es) == 1L, isTRUE(cs == es)), detail = sprintf("C %s, E %s", paste(cs, collapse = ","), paste(es, collapse = ",")))
  })
  # X7 |xbar_loo (C) - s1 m1 (E)| < 1e-10 on the 85 focal animals
  run("X7", "|xbar_loo (C) - s1 m1 (E)| < 1e-10 on the 85 focal animals", c("C", "E"), function(ms) {
    cl <- data.table::as.data.table(cx("C", "xbar_loo"))[, .(AnimalNum, xbar_loo)]
    e <- tb("E", "e01_exposures_animal")[focal %in% TRUE, .(AnimalNum, m1, s1)]
    m <- merge(cl, e, by = "AnimalNum"); dd <- abs(m$xbar_loo - m$s1 * m$m1)
    list(ok = c(nrow(cl) == 85L, nrow(e) == 85L, nrow(m) == 85L, all(is.finite(dd) & dd < 1e-10)),
         detail = sprintf("%d animals; max |difference| %.3g", nrow(m), if (nrow(m)) max(dd) else NA_real_))
  })
  # X8 (recorded) E's S15 = C's S3 per frozen SD within 1e-6 when S3 is singular, else the difference
  run("X8", "E's S15 = C's S3 delta_peer per frozen SD (1e-6) when S3 is singular; otherwise the difference is recorded", c("C", "E"),
      hard = FALSE, function(ms) {
    c3 <- cx("C", "s3_delta_peer_per_sd"); sg <- cx("C", "s3_singular")
    e15 <- tb("E", "e06_sensitivities")[sensitivity_id == "S15", estimate]
    if (length(c3) != 1L || length(e15) != 1L) stop(sprintf("expected one C S3 and one E S15 value (%d, %d)", length(c3), length(e15)))
    d <- e15 - c3
    if (isTRUE(sg)) list(ok = is.finite(d) && abs(d) < 1e-6, detail = sprintf("S3 singular; E S15 - C S3 = %.3g per frozen SD", d))
    else list(ok = is.finite(d), detail = sprintf("S3 not singular; E S15 - C S3 = %.6g per frozen SD (recorded; equality not expected)", d))
  })
  data.table::rbindlist(G, fill = TRUE)
}
