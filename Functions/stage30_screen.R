# ================================================================
# Stage 30 exploratory outcome screen: helpers (Exp9 SIS RFID)
# MMMSociability -- Functions/stage30_screen.R
# ================================================================
# Pure constants and functions for Analysis/30_exploratory_screen.R, implementing the FROZEN Stage 30 registry v1.0
# (docs/stage30/stage30_registry_v1.0.json, sha256 5c252bf6...; the JSON is authoritative). Sourcing this file defines
# constants and functions only; it reads and writes nothing.
#
# Blindness contract: no function here reads a Group, RES/SUS or CombZ column from disk. The driver attaches the
# outcome columns (CombZ, CombZ_wb, g_RS) to the analysis data only after the design gate:
#   REAL: the canonical sus/con lists and later-outcome CombZ table; DRY: permuted labels / synthetic CombZ.
#
# Inference (registry inference): the frozen Stage 29 engine Functions/rfid_canonical_inference.R.
#   CR2  mmm_ci_fit(engine = "lm", expected_rank) -> clubSandwich CR2 by CageEpisodeID, Satterthwaite (Stage 29
#        section-6b route; mmm_ci_cr2 is NOT used); OLS t alongside (descriptive).
#   KR   mmm_ci_fit(engine = "lmer", expected_rank) -> mmm_ci_contrast (KR t) / mmm_ci_joint (KR F(3)).
#   failure rule: mmm_ci_fit stop() (rank < columns, or columns != expected_rank) -> FAILED_RANK; an erroring fit or a
#   non-converged fit whose optimizers disagree -> FAILED; both enter multiplicity with p = 1; nothing stops the run.
#   KR fidelity: every mmm_ci_contrast / mmm_ci_joint / mmm_ci_vcov_kr call runs under a warning handler; lmerTest's silent
#   fallback ("Unable to compute Kenward-Roger ...: using Satterthwaite instead") makes the fit FAILED (never reported as KR).
# Requires: data.table, clubSandwich, lme4, lmerTest, pbkrtest, reformulas, digest, jsonlite; mmm_ci_* (engine).
# ================================================================

# ---------------------------------------------------------------- frozen identities (registry v1.0 / FREEZE_RECORD.csv)
S30SC_REGISTRY_VERSION <- "1.0"
S30SC_REGISTRY_SHA256 <- "5c252bf699bc0f517c5508854d710bbaa014d4b6fe98d128eb0071bde89dd05e"
S30SC_REGISTRY_MD_SHA256 <- "e12b390b3c8bfadd32e5d2aae319f6053c0875893a41b3cf3bd60553d5bdf531"
S30SC_V2_MANIFEST_SHA256 <- "bb33a111980913f4ddf97c40872ccdd96dd73accdbdc5c5867954b51aad2c571"
S30SC_V2_N_FILES <- 24L
S30SC_REF_TABLE_SHA256 <- "fc053468f2a1d1a07e981b6a76c71caa6cfd89b19da7318ee2c2f82f49050dd1"
S30SC_X401_SHA256 <- "a533b239852fbe46c8a4ff776754984f4fe1f29c0c4277082487c1546f7acf8a"
S30SC_COOKIE_MANIFEST_SHA256 <- "0abe20ee9bcf34b422c81202bf58409d133e26834d3d395d8aa608c3b2c19666"
S30SC_COOKIE_FILE_SHA256 <- c(cookie_response.csv = "329e2455a243a8e393f60bfe600f2f8a267fb03714571432890f7beb488e16d8",
                              animals.csv = "ce66d07475cee8ea384ebbd7b6708c24a83e0aaba04a6a5fa6467914c9fb0163",
                              cookie_windows.csv = "e7e7e27edc6288405b84001aaff524355fd2ae1d5777be34105d656e0c289e1d")
# Stage 29 v1.0.1 / data version v2 tables read for the carry-over rows (identical in cage_fix_rerun/out/tables and the
# release folder v101_dv2_<commit7>/tables)
S30SC_S29_TABLE_SHA256 <- c(canonical_window_metrics.csv = "fc053468f2a1d1a07e981b6a76c71caa6cfd89b19da7318ee2c2f82f49050dd1",
                            estimates.csv = "a2fc84dbed97803e37ec916e77c5c8e76d20c62cfebb2ce82b6a17adc4f56042",
                            continuous_estimates.csv = "cc549c248778a735f1e79ba2b33e59443778ef200e480ad7479b4150f2b15200",
                            exposure_estimates.csv = "33d91929453ae9eec694b173208a6588021893604aa98fa379227adfc7611a70",
                            multiplicity.csv = "efe2d1f85d3349a32d6897f794780cfe4a825789af8c6cfb00d13b451c845a92")
# frozen code identities (FREEZE_RECORD.csv; the config is gated on its analytic constants, not on its file hash)
S30SC_CODE_SHA256 <- c("Functions/stage30_movement.R" = "9a368491ff373194a22e6baacd4874bfa260c4e1296e81d9be5863ed6bfde659",
                       "Functions/rfid_canonical_inference.R" = "886d8bc06807b5958482a31449d0b8e5886db106cc7d02f63863d535dc78b92c",
                       "Functions/rfid_event_stream.R" = "3fda3d40f154d2423c1a2e16b687a14369ac9f8c070d883534a713d9a5dceb68",
                       "Functions/rfid_binfree_metrics.R" = "ffc1235dbef6b05e63ee6dee7b7b22f3e7107157be2c0a3ff491706079e0d8d0")
S30SC_ANALYTIC_CONSTANTS <- list(bout_criterion_s = 39.3970988275363, alpha = 0.05, ci_level = 0.95)
# raw_data seed sources (Stage 29 v1.0.1 release audit/run_inputs.csv, role raw_seed; unchanged since Stage 29)
S30SC_RAW_SEED_SHA256 <- c(
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

# Stage 29 v1.0.1 release folder (registry stage29_release.stage29_run): analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_<commit7>
S30SC_S29_RELEASE_PATTERN <- "/analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_([0-9a-f]{7})$"
# numerics-relevant package versions gated against the Stage 29 v1.0.1 release run_manifest (and R.version.string)
S30SC_GATED_PACKAGES <- c("lme4", "lmerTest", "pbkrtest", "clubSandwich", "Matrix", "reformulas")
# REAL output root entries that block a REAL run (any earlier REAL output or staging folder, at any commit)
S30SC_REAL_OUT_PATTERN <- "^(\\.tmp_)?v1\\.0_"
# lmerTest's silent Kenward-Roger -> Satterthwaite fallback (contest1D / contestMD warning text)
S30SC_KR_FALLBACK_PATTERN <- "Unable to compute Kenward-Roger"
# CombZ table columns: the label pre-flight (before outcome contact) never selects CombZ; the outcome read selects only these
S30SC_COMBZ_LABEL_COLS <- c("AnimalNum", "Sex", "Batch", "outcome_group")
S30SC_COMBZ_OUTCOME_COLS <- c("AnimalNum", "CombZ")
# Driver conventions where the registry is silent (declared in README / run_manifest; conservative by construction)
S30SC_DRIVER_CONVENTIONS <- c(
  "C1 D-MOVEMENT on a FAILED Movement-adjusted fit: an inactivity hypothesis with raw p < 0.05 whose Movement-adjusted model FAILED is D-MOVEMENT (D(i) not evaluable; treated conservatively)",
  "C2 LOBO: a FAILED LOBO refit counts as 'focal sign not stable' (class A impossible)",
  "C3 sensitivities: a FAILED declared sensitivity is not SIGN_CHANGE (registry literal) but is flagged (any_sensitivity_failed); an erroring sensitivity block sets any_sign_change = NA (class A impossible)",
  "C4 POOL entries: the declared sensitivities, LOBO and influence are also run (estimation only; no p)",
  "C5 optimizer check: always run for the primary (1 + cc1 || AnimalID) SCREEN-L-PCR fits (incl. POOL); refits get it only on a convergence warning (mmm_ci_fit); influence refits use lme4::lmer without it",
  "C6 nested LOBO prediction (cookie hypotheses classified A; estimation only, never a classification input): per training fold, OLS of the registered fixed part on the other batches (Batch dropped if one training batch remains); the held-out batch is predicted from the focal terms only, centred within the batch; per-fold r, pooled out-of-batch R2 (centred); CAT additionally reports the held-out within-batch AUC P(y_RES > y_SUS)",
  "C7 sex-specific wording: 'female-/male-specific' requires (registry) INT local BH q < 0.05 AND (driver) this sex's row classified A or B AND its direction consistent with the INT estimate (F: sign(INT) = sign(F); M: sign(INT) = -sign(M); L: the row's max-|KR t| component vs the matching INT component)",
  "C8 KR fidelity: a Kenward-Roger computation that lmerTest replaces by Satterthwaite (warning) is FAILED, never reported as KR",
  "C9 fits use the in-memory recomputed group-blind features (full double precision); features_*.csv carry 15 significant digits, features_*_full_precision.rds the exact values (refits on CSV values may differ in KR p by ~1e-8)")

S30SC_ALPHA <- 0.05
S30SC_TOL <- 1e-9          # recomputation gates (reference table, inactivity table, cookie table)
S30SC_DF_TOL <- 1e-6       # design gate: |df_Satt - expected_df_cr2|
S30SC_LOW_DF <- 4          # registry low_df_rule
S30SC_INACTIVITY_X <- c(40, 60)
S30SC_RUN_MODE_DRY <- "DRY_RUN_NOT_RESULTS"
S30SC_RUN_MODE_REAL <- "REAL_EXPLORATORY_POST_HOC_CONTEXT"
# columns a group-blind read must never select
S30SC_FORBIDDEN <- c("Group", "g_RS", "g_SIS", "CombZ", "CombZ_wb", "isCON", "conCage", "sisCage", "Session", "outcome_group",
                     "zScore", "classification_source")
# canonical_window_metrics.csv: the group-blind columns read for the reference gate (Sex is metadata, not recomputed)
S30SC_REF_COLS <- c("AnimalNum", "CC", "obs_s", "n_positions_occupied", "occupancy_dispersion", "dominant_share", "n_bouts", "n_events",
                    "fragmentation", "events_per_bout", "crossing_rate", "bout_rate_h", "shared_zone_use", "n_tracked_mates", "dyadic_obs_s",
                    "hardware_flag", "SourceFile", "System", "Sex", "Batch", "CageEpisodeID", "n_in_cage", "light_phase_crossing_rate",
                    "light_complete")
S30SC_REF_COMPARE_COLS <- setdiff(S30SC_REF_COLS, c("AnimalNum", "CC", "Sex"))
S30SC_COOKIE_ANIMAL_COLS <- c("AnimalID", "AnimalNum", "Batch", "Sex", "CageEpisodeID", "System", "cage_size", "board_override_applied",
                              "or646_protocol_deviation", "v2_CC4_System", "v2_CC4_CageEpisodeID", "board_System_equals_v2_CC4",
                              "pop_cookie_all_eligible", "pop_COOKIE_SIS", "pop_COOKIE_SIS_F", "pop_COOKIE_SIS_M")

# registry measure strings (exact) -> analysis columns; sens = the declared measurement sensitivity column; movadj = M
S30SC_MEASURES <- data.frame(
  measure = c("occupancy_dispersion (CC1 active)", "fragmentation (CC1 active)", "light_phase_crossing_rate (CC1)",
              "crossing_rate (active, CC1-CC4)", "shared_zone_use (data version v2; active, CC1-CC4)", "occupancy_dispersion (active, CC1-CC4)",
              "fragmentation (active, CC1-CC4)", "light_phase_crossing_rate (CC1-CC4)", "posinact40_frac (CC1 active)", "posinact40_frac (CC1 light)",
              "posinact40_frac (active, CC1-CC4)", "posinact40_frac (light, CC1-CC4)", "dcookie60"),
  measure_col = c("occupancy_dispersion", "fragmentation", "light_phase_crossing_rate", "crossing_rate", "shared_zone_use", "occupancy_dispersion",
                  "fragmentation", "light_phase_crossing_rate", "posinact40_active", "posinact40_light", "posinact40_active", "posinact40_light",
                  "dcookie60"),
  sens_col = c(NA, NA, NA, NA, NA, NA, NA, NA, "posinact60_active", "posinact60_light", "posinact60_active", "posinact60_light", "dcookie45"),
  movadj_col = c(NA, NA, NA, NA, NA, NA, NA, NA, "crossing_rate", "light_phase_crossing_rate", "crossing_rate", "light_phase_crossing_rate",
                 "cc1_crossing_rate"),
  phase = c("active", "active", "light", "active", "active", "active", "active", "light", "active", "light", "active", "light", "cookie"),
  stringsAsFactors = FALSE)

S30SC_ENGINE_BY_QUESTION <- c(CONT = "CR2", CAT = "KR t", L = "KR F(3)")

# ---------------------------------------------------------------- small utilities
s30sc_nz <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
s30sc_sha <- function(f) digest::digest(file = f, algo = "sha256")
s30sc_assert_blind <- function(cols, what) {
  b <- intersect(cols, S30SC_FORBIDDEN)
  if (length(b)) stop("Group/outcome blindness violated in ", what, ": ", paste(b, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
s30sc_norm_path <- function(p) tolower(normalizePath(p, winslash = "/", mustWork = FALSE))
s30sc_inside <- function(path, root) startsWith(paste0(s30sc_norm_path(path), "/"), paste0(s30sc_norm_path(root), "/"))
s30sc_gate <- function(gate, passed, detail = "", stage = "") data.table::data.table(stage = stage, gate = gate, passed = isTRUE(passed), detail = as.character(detail))
s30sc_sign_chr <- function(v) ifelse(is.na(v), "NA", ifelse(v > 0, "+", ifelse(v < 0, "-", "0")))

# ---------------------------------------------------------------- run-once / release / environment gates (pure)
#' Entries of the REAL output root that block a REAL run: any v1.0_* output or .tmp_v1.0_* staging folder, at any commit.
s30sc_real_out_conflicts <- function(out_root) {
  if (!dir.exists(out_root)) return(character())
  grep(S30SC_REAL_OUT_PATTERN, list.files(out_root, all.files = TRUE, no.. = TRUE), value = TRUE)
}
#' commit7 of a Stage 29 v1.0.1 release folder path (NA if the path is not .../29_canonical_behavior_releases/v101_dv2_<commit7>).
s30sc_release_commit <- function(dir) {
  p <- sub("/+$", "", gsub("\\\\", "/", as.character(dir)))
  if (!grepl(S30SC_S29_RELEASE_PATTERN, p)) return(NA_character_)
  sub(paste0("^.*", S30SC_S29_RELEASE_PATTERN), "\\1", p)
}
#' Release audit/output_manifest.csv (file, bytes, sha256) lists every expected table with the expected SHA-256.
s30sc_release_manifest_check <- function(manifest, table_sha = S30SC_S29_TABLE_SHA256) {
  m <- unique(data.table::as.data.table(manifest)[, .(file = basename(file), sha256)])
  ok <- vapply(names(table_sha), function(f) any(m$file == f & m$sha256 == table_sha[[f]]) && sum(m$file == f) == 1L, TRUE)
  list(passed = all(ok), detail = if (all(ok)) paste(length(ok), "tables listed with the gated SHA-256") else paste("missing / different:", paste(names(ok)[!ok], collapse = ",")))
}
#' "lme4 2.0.1; lmerTest 3.2.1; ..." -> named character vector of versions.
s30sc_parse_packages <- function(s) {
  p <- trimws(strsplit(as.character(s), ";", fixed = TRUE)[[1]]); p <- p[nzchar(p)]
  stats::setNames(sub("^\\S+\\s+", "", p), sub("\\s.*$", "", p))
}
#' Package / R version gate rows: the numerics-relevant versions must equal the Stage 29 v1.0.1 release run_manifest.
s30sc_package_gate <- function(release_packages, release_r, pkgs = S30SC_GATED_PACKAGES, r_now = R.version.string,
                               version_of = function(p) as.character(utils::packageVersion(p))) {
  exp <- if (length(release_packages) != 1L || is.na(release_packages)) character() else s30sc_parse_packages(release_packages)
  if (length(release_r) != 1L) release_r <- NA_character_
  obs <- vapply(pkgs, function(p) tryCatch(version_of(p), error = function(e) NA_character_), "")
  data.table::data.table(item = c("R", pkgs), expected = c(as.character(release_r), unname(exp[pkgs])), observed = c(r_now, unname(obs)))[
    , passed := !is.na(expected) & !is.na(observed) & expected == observed][]
}

# ---------------------------------------------------------------- registry
#' Read the frozen registry; the caller compares $sha with S30SC_REGISTRY_SHA256.
s30sc_read_registry <- function(path) list(reg = jsonlite::fromJSON(path, simplifyVector = FALSE), sha = s30sc_sha(path), path = path)

#' Focal coefficient names implied by question x sex analysis (checked against the registry focal text in s30sc_spec).
s30sc_focal_terms <- function(question, sex_analysis) {
  int <- identical(sex_analysis, "INT")
  switch(question,
         CONT = if (int) "x:sex_c" else "x",
         CAT = if (int) "g_RS:sex_c" else "g_RS",
         L = if (int) paste0("CombZ_wb:c", 2:4, ":sex_c") else paste0("CombZ_wb:c", 2:4),
         stop("unknown question ", question, call. = FALSE))
}

#' Analysis specification: 48 DISCOVERY hypotheses + 16 ESTIMATION_ONLY (POOL) entries, validated against the registry.
s30sc_spec <- function(reg) {
  one <- function(h, role) {
    cal <- s30sc_nz(h$calibration, list())
    num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
    data.table::data.table(
      id = h$id, role = role, family = h$family, block = h$block, question = h$question, sex_analysis = h$sex_analysis,
      population = h$population, measure = h$measure, measure_role = h$measure_role, formula = h$formula, focal_text = h$focal,
      engine = h$engine, expected_rank = as.integer(h$expected_rank), expected_df_cr2 = num(h$expected_df_cr2),
      calibration_verdict = s30sc_nz(cal$verdict, NA_character_), calibration_size05 = num(cal$size05), calibration_size10 = num(cal$size10),
      calibration_mcse05 = num(cal$mcse05), calibration_df_median = num(cal$df_median), calibration_singular_share = num(cal$singular_share),
      calibration_size05_range = if (is.null(cal$size05_range_scenarios)) NA_character_ else paste(unlist(cal$size05_range_scenarios), collapse = "-"),
      calibration_note = s30sc_nz(h$calibration_note, NA_character_), robustness_text = s30sc_nz(h$robustness, NA_character_))
  }
  S <- data.table::rbindlist(c(lapply(reg$hypotheses, one, role = "DISCOVERY"), lapply(reg$estimation_only$entries, one, role = "ESTIMATION_ONLY")))
  mm <- data.table::as.data.table(S30SC_MEASURES)
  unknown <- setdiff(S$measure, mm$measure)
  if (length(unknown)) stop("Registry measure(s) without a column mapping: ", paste(unknown, collapse = "; "), call. = FALSE)
  S <- merge(S, mm, by = "measure", all.x = TRUE, sort = FALSE)
  S[, focal_terms := lapply(seq_len(.N), function(i) s30sc_focal_terms(question[i], sex_analysis[i]))]
  S[, cell := sub("-(F|M|INT|POOL)$", "", id)]
  S[, order := match(id, c(vapply(reg$hypotheses, `[[`, "", "id"), vapply(reg$estimation_only$entries, `[[`, "", "id")))]
  data.table::setorder(S, order)
  # ---- validation (stops: the registry is frozen, so any mismatch is a code or registry-reading defect)
  D <- S[role == "DISCOVERY"]; P <- S[role == "ESTIMATION_ONLY"]
  fam <- reg$multiplicity$local_families
  chk <- c(
    n_discovery_48 = nrow(D) == 48L, n_pool_16 = nrow(P) == 16L, ids_unique = !anyDuplicated(S$id),
    total_m = identical(as.integer(reg$multiplicity$m$total), nrow(D)),
    families_equal = all(vapply(names(fam), function(f) setequal(unlist(fam[[f]]), D[family == f, id]), TRUE)) &&
      setequal(names(fam), unique(D$family)),
    family_m_declared = all(vapply(names(fam), function(f) as.integer(reg$multiplicity$m$by_family[[f]]) == length(fam[[f]]), TRUE)),
    discovery_sex = all(D$sex_analysis %in% c("F", "M", "INT")), pool_sex = all(P$sex_analysis == "POOL"),
    pool_role = all(vapply(reg$estimation_only$entries, function(x) identical(x$role, "ESTIMATION_ONLY"), TRUE)),
    populations_declared = all(S$population %in% names(reg$populations)),
    engine_by_question = all(S$engine == S30SC_ENGINE_BY_QUESTION[S$question]),
    cr2_df_declared = all(is.finite(S[engine == "CR2", expected_df_cr2])),
    focal_text = all(mapply(function(q, s, f) {
      if (q == "L") grepl("joint", f) && ((s == "INT") == grepl("sex_c", f)) else
        grepl(if (q == "CONT") "^x" else "^g_RS", f) && ((s == "INT") == grepl(":sex_c", f)) }, S$question, S$sex_analysis, S$focal_text)),
    every_cell_has_F_M_INT_POOL = all(S[, .(ok = setequal(sex_analysis, c("F", "M", "INT", "POOL"))), by = cell]$ok))
  if (!all(chk)) stop("Registry specification check failed: ", paste(names(chk)[!chk], collapse = ", "), call. = FALSE)
  attr(S, "checks") <- chk
  S[]
}

# ---------------------------------------------------------------- measurement helpers
#' Positional inactivity on canonical occupancy runs (mmm_evs_window_stream / s30mv_window_streams $runs, window-clipped):
#' per animal x window, the share of observed seconds in runs whose clipped duration is >= X s (a qualifying run counts
#' in full). T = observed seconds = obs_s.
s30sc_inactivity <- function(runs, X = S30SC_INACTIVITY_X) {
  r <- data.table::copy(data.table::as.data.table(runs)); r[, dur := e - s]
  out <- r[, .(T_obs = sum(dur), n_runs = .N), by = .(AnimalNum, CC)]
  for (x in X) {
    f <- r[, .(v = sum(dur[dur >= x]) / sum(dur)), by = .(AnimalNum, CC)]
    data.table::setnames(f, "v", paste0("frac", x))
    out <- merge(out, f, by = c("AnimalNum", "CC"))
  }
  out[]
}

#' Column-wise comparison of a recomputed table with a reference (numeric: max |diff| <= tol and identical NA pattern;
#' other types: identical). Returns one row per column.
s30sc_compare <- function(rec, ref, key, cols, tol = S30SC_TOL) {
  z <- merge(data.table::as.data.table(rec)[, c(key, cols), with = FALSE], data.table::as.data.table(ref)[, c(key, cols), with = FALSE],
             by = key, suffixes = c(".rec", ".ref"), all = TRUE)
  data.table::rbindlist(lapply(cols, function(k) {
    a <- z[[paste0(k, ".rec")]]; b <- z[[paste0(k, ".ref")]]
    na_same <- identical(is.na(a), is.na(b))
    if (is.numeric(a) || is.numeric(b) || is.logical(a) && is.logical(b)) {
      aa <- as.numeric(a); bb <- as.numeric(b); d <- abs(aa - bb); d <- d[is.finite(d)]
      mx <- if (length(d)) max(d) else 0
      data.table::data.table(column = k, n = nrow(z), na_pattern_identical = na_same, max_abs_diff = mx, identical_text = NA,
                             passed = na_same && mx <= tol)
    } else {
      same <- na_same && all(as.character(a) == as.character(b), na.rm = TRUE)
      data.table::data.table(column = k, n = nrow(z), na_pattern_identical = na_same, max_abs_diff = NA_real_, identical_text = same, passed = same)
    }
  }))
}

#' Parse registry cage-size strings ("19 x 4, 3 x 3, 2 x 1") -> named counts by cage size.
s30sc_parse_cage_sizes <- function(s) {
  p <- trimws(strsplit(sub(" [(].*$", "", s), ",")[[1]])
  n <- as.integer(sub("^([0-9]+) x.*$", "\\1", p)); sz <- sub("^[0-9]+ x ([0-9]+).*$", "\\1", p)
  stats::setNames(n, sz)[order(as.integer(sz), decreasing = TRUE)]
}
s30sc_cage_size_table <- function(cage) { tb <- table(table(cage)); v <- as.integer(tb); stats::setNames(v, names(tb))[order(as.integer(names(tb)), decreasing = TRUE)] }
s30sc_parse_batches <- function(s) {
  s <- as.character(s); n <- as.integer(sub("^([0-9]+).*$", "\\1", s))
  inner <- if (grepl("^[0-9]+ [(]", s)) sub("^[0-9]+ [(]([^)]*)[)].*$", "\\1", s) else ""   # only the parenthesised list after the count
  lst <- if (nzchar(inner)) regmatches(inner, gregexpr("B[0-9]", inner))[[1]] else character()
  list(n = n, batches = lst)
}
#' "RES 28, SUS 18 (B3 7/9, B4 9/6, B6 12/3)" or res_sus_counts_by_batch entries -> data.table(Batch, RES, SUS).
s30sc_parse_group_counts <- function(s) {
  m <- regmatches(s, gregexpr("B[0-9] [0-9]+/[0-9]+", s))[[1]]
  data.table::data.table(Batch = sub(" .*$", "", m), RES = as.integer(sub("^B[0-9] ([0-9]+)/.*$", "\\1", m)),
                         SUS = as.integer(sub("^.*/([0-9]+)$", "\\1", m)))
}

# ---------------------------------------------------------------- analysis data
#' Population rows. D$W: one row per animal x CC window (all RFID animals, SIS flag); D$K: one row per cookie animal.
s30sc_pop_data <- function(D, population) {
  base <- sub("_(F|M)$", "", population)
  sx <- if (grepl("_F$", population)) "Female" else if (grepl("_M$", population)) "Male" else NA_character_
  d <- switch(base, SIS_CC1 = D$W[SIS == TRUE & CC == "CC1"], SIS_CC1_CC4 = D$W[SIS == TRUE], COOKIE_SIS = D$K[SIS == TRUE],
              stop("unknown population ", population, call. = FALSE))
  if (!is.na(sx)) d <- d[Sex == sx]
  d <- data.table::copy(d); d[, Batch := factor(as.character(Batch))]
  data.table::setorderv(d, intersect(c("Batch", "AnimalNum", "CC"), names(d)))
  d[]
}

#' Hypothesis data: the measure as x (CONT) or y (CAT, L), optional M (Movement covariate); rows with a non-finite measure
#' dropped (shared_zone_use: OQ770 / OQ771 at CC1). CONT requires a finite CombZ for every row (outcome completeness).
s30sc_hyp_data <- function(D, e, measure_col = e$measure_col, with_M = FALSE) {
  d <- s30sc_pop_data(D, e$population)
  if (!measure_col %in% names(d)) stop("measure column ", measure_col, " not in the data of ", e$id, call. = FALSE)
  v <- if (e$question == "CONT") "x" else "y"
  d[, (v) := as.numeric(get(measure_col))]
  d <- d[is.finite(get(v))]
  if (isTRUE(with_M)) {
    if (is.na(e$movadj_col) || !e$movadj_col %in% names(d)) stop("no Movement covariate for ", e$id, call. = FALSE)
    d[, M := as.numeric(get(e$movadj_col))]
    if (!all(is.finite(d$M))) stop("non-finite Movement covariate in ", e$id, call. = FALSE)
  }
  need <- switch(e$question, CONT = "CombZ", CAT = "g_RS", L = "CombZ_wb")
  if (!need %in% names(d) || !all(is.finite(d[[need]]))) stop("outcome/label column ", need, " missing or non-finite in ", e$id, call. = FALSE)
  droplevels(d)
}

# ---------------------------------------------------------------- formulas
#' Movement-adjusted formula (sensitivities_estimation_only.inactivity[2] / cookie[3]): + M; INT: + M:sex_c; L POOL: + M:sex_c.
s30sc_movadj_formula <- function(formula, question, sex_analysis) {
  add <- if (sex_analysis == "INT" || (question == "L" && sex_analysis == "POOL")) "M + M:sex_c" else "M"
  out <- sub("^(CombZ ~ Batch|y ~ Batch)", paste("\\1 +", add), formula)
  if (identical(out, formula)) stop("cannot insert the Movement covariate into: ", formula, call. = FALSE)
  out
}
#' Batch term dropped (LOBO refit with a single remaining batch).
s30sc_drop_batch <- function(formula) {
  out <- sub("~ Batch + ", "~ ", formula, fixed = TRUE)
  if (identical(out, formula)) stop("no 'Batch +' term in: ", formula, call. = FALSE)
  out
}
s30sc_fixed_formula <- function(formula) paste(deparse(reformulas::nobars(stats::as.formula(formula)), width.cutoff = 500L), collapse = " ")

#' Columns and rank of the fixed-effects design (as mmm_ci_fit computes them).
s30sc_design_info <- function(d, formula, engine_lm) {
  f <- stats::as.formula(formula)
  ff <- if (engine_lm) f else stats::as.formula(paste(deparse(f[[2]]), "~", paste(deparse(reformulas::nobars(f)[[3]]), collapse = " ")))
  X <- stats::model.matrix(ff, droplevels(as.data.frame(d)))
  list(ncol = ncol(X), rank = qr(X)$rank, n_obs = nrow(X))
}

# ---------------------------------------------------------------- fitting (registry inference; frozen engine)
S30SC_FIT_TEMPLATE <- function() data.table::data.table(
  status = NA_character_, error = NA_character_, engine_route = NA_character_, formula_fitted = NA_character_,
  estimate = NA_real_, se = NA_real_, df = NA_real_, statistic = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_raw = NA_real_,
  F = NA_real_, df1 = NA_real_, df2 = NA_real_, ols_se = NA_real_, ols_df = NA_real_, ols_p = NA_real_,
  rank = NA_integer_, n_fixed_cols = NA_integer_, expected_rank = NA_integer_, n_obs = NA_integer_, n_animals = NA_integer_,
  n_cages = NA_integer_, n_batches = NA_integer_, batches = NA_character_, singular = NA, converged = NA, failed = NA,
  optimizer_check_run = "no", optimizer_check_agree = NA, optimizer_check_messages = NA_character_, fit_messages = NA_character_)
s30sc_set <- function(r, ...) {
  a <- list(...)
  for (k in names(a)) { v <- a[[k]]; if (is.null(v) || length(v) == 0L) next
    cls <- class(r[[k]])[1]
    v <- switch(cls, integer = as.integer(v), numeric = as.numeric(v), character = as.character(v), logical = as.logical(v), v)
    data.table::set(r, j = k, value = v[1]) }
  r
}
s30sc_failure_status <- function(msg) if (grepl("Rank-deficient design|frozen expected rank|lme4 dropped columns", msg)) "FAILED_RANK" else "FAILED"
s30sc_fill_data_n <- function(r, d, formula, engine_lm) {
  di <- tryCatch(s30sc_design_info(d, formula, engine_lm), error = function(e) NULL)
  s30sc_set(r, n_obs = nrow(d), n_animals = data.table::uniqueN(d$AnimalNum), n_cages = data.table::uniqueN(d$CageEpisodeID),
            n_batches = data.table::uniqueN(d$Batch), batches = paste(sort(unique(as.character(d$Batch))), collapse = ","),
            rank = if (is.null(di)) NA else di$rank, n_fixed_cols = if (is.null(di)) NA else di$ncol)
}
s30sc_fill_info <- function(r, info) {
  oc_run <- if (!is.na(info$optimizer_check_messages)) "on convergence warning (mmm_ci_fit)" else "no"
  s30sc_set(r, rank = info$rank, n_fixed_cols = info$n_fixed_cols, expected_rank = info$expected_rank, n_obs = info$n_obs,
            n_animals = info$n_animals, n_cages = info$n_cage_episodes, n_batches = info$n_batches, singular = info$singular,
            converged = info$converged, failed = info$failed, optimizer_check_agree = info$optimizer_check_agree,
            optimizer_check_messages = info$optimizer_check_messages, fit_messages = info$messages, optimizer_check_run = oc_run)
}

#' One registered fit. engine: "CR2" (lm + CR2 Satterthwaite), "KR t" (lmer + KR contrast), "KR F(3)" (lmer + KR joint F
#' and the three components). Returns list(row, components, m, focal). Never stops on a model problem.
#'   joint = FALSE: no joint F (POOL entries: estimation only). kr_se_only = TRUE: L components from the KR-adjusted
#'   covariance (mmm_ci_vcov_kr; LOBO refits: estimate and KR SE only). always_optimizer_check: registry KR rule for the
#'   (1 + cc1 || AnimalID) crossing-rate models.
s30sc_fit <- function(d, formula, engine, label, focal, expected_rank = NULL, joint = TRUE, kr_se_only = FALSE,
                      always_optimizer_check = FALSE) {
  lm_route <- engine == "CR2"
  r <- S30SC_FIT_TEMPLATE()
  r <- s30sc_set(r, formula_fitted = formula, expected_rank = if (is.null(expected_rank)) NA else expected_rank,
                 engine_route = switch(engine, CR2 = "lm; CR2 by CageEpisodeID; Satterthwaite t (clubSandwich)",
                                       "KR t" = "lmer REML bobyqa; Kenward-Roger t (mmm_ci_contrast)",
                                       "KR F(3)" = if (kr_se_only) "lmer REML bobyqa; KR-adjusted SEs (mmm_ci_vcov_kr)" else "lmer REML bobyqa; Kenward-Roger joint F (mmm_ci_joint) + KR t components",
                                       stop("unknown engine ", engine, call. = FALSE)))
  r <- s30sc_fill_data_n(r, d, formula, lm_route)
  out <- list(row = r, components = NULL, m = NULL, focal = focal)
  m <- tryCatch(mmm_ci_fit(formula, d, label, engine = if (lm_route) "lm" else "lmer", expected_rank = expected_rank), error = function(e) e)
  if (inherits(m, "error")) { msg <- conditionMessage(m); out$row <- s30sc_set(r, status = s30sc_failure_status(msg), error = msg); return(out) }
  r <- s30sc_fill_info(r, m$info); out$m <- m
  if (always_optimizer_check && !is.null(m$fit) && !lm_route && is.na(m$info$optimizer_check_agree)) {
    oc <- tryCatch(mmm_ci_optimizer_check(m), error = function(e) data.table::data.table(agree = NA, messages = conditionMessage(e)))
    r <- s30sc_set(r, optimizer_check_run = "always (registry: (1 + cc1 || AnimalID) crossing-rate model)", optimizer_check_agree = oc$agree,
                   optimizer_check_messages = oc$messages)
  }
  if (mmm_ci_fit_failed(m)) { out$row <- s30sc_set(r, status = "FAILED", error = s30sc_nz(m$info$error, "failure rule: non-converged, optimizers disagree")); return(out) }
  # KR fidelity (G2-04): every KR call runs under a warning handler; warnings are kept in fit_messages, and lmerTest's
  # fallback from Kenward-Roger to Satterthwaite (a warning only) fails the fit instead of being reported as KR.
  kr_msgs <- character()
  kr <- function(expr) {
    val <- withCallingHandlers(expr, warning = function(w) { kr_msgs <<- c(kr_msgs, paste("warning (KR step):", conditionMessage(w))); invokeRestart("muffleWarning") })
    if (any(grepl(S30SC_KR_FALLBACK_PATTERN, kr_msgs, fixed = TRUE)))
      stop("KR unavailable: lmerTest fell back to Satterthwaite (", paste(unique(kr_msgs[grepl(S30SC_KR_FALLBACK_PATTERN, kr_msgs, fixed = TRUE)]), collapse = "; "), ")", call. = FALSE)
    val
  }
  add_msgs <- function(r) if (length(kr_msgs)) s30sc_set(r, fit_messages = paste(c(if (!is.na(r$fit_messages) && nzchar(r$fit_messages)) r$fit_messages, unique(kr_msgs)), collapse = " | ")) else r
  res <- tryCatch({
    if (lm_route) {
      fc <- mmm_ci_match(focal, names(stats::coef(m$fit)))
      V <- clubSandwich::vcovCR(m$fit, cluster = m$data$CageEpisodeID, type = "CR2")
      ct <- clubSandwich::coef_test(m$fit, vcov = V, test = "Satterthwaite", coefs = fc)
      cs <- summary(m$fit)$coefficients[fc, ]
      q <- stats::qt(0.975, ct$df_Satt)
      out$focal <- fc
      s30sc_set(r, estimate = ct$beta, se = ct$SE, df = ct$df_Satt, statistic = ct$tstat, p_raw = ct$p_Satt,
                ci_low = ct$beta - q * ct$SE, ci_high = ct$beta + q * ct$SE, ols_se = cs[[2]], ols_df = m$fit$df.residual, ols_p = cs[[4]])
    } else if (engine == "KR t") {
      k <- kr(mmm_ci_contrast(m, stats::setNames(list(1), focal), focal))
      if (!identical(k$status, "OK")) stop("KR contrast FAILED")
      s30sc_set(r, estimate = k$estimate, se = k$se, df = k$df, statistic = k$statistic, p_raw = k$p_raw, ci_low = k$ci_low, ci_high = k$ci_high)
    } else {
      if (kr_se_only) {
        V <- kr(mmm_ci_vcov_kr(m, focal)); b <- lme4::fixef(m$fit)[mmm_ci_match(focal, names(lme4::fixef(m$fit)))]
        out$components <- data.table::data.table(component = focal, estimate = unname(b), se = sqrt(diag(V)), df = NA_real_,
                                                 statistic = unname(b) / sqrt(diag(V)), ci_low = NA_real_, ci_high = NA_real_, status = "OK")
      } else {
        out$components <- data.table::rbindlist(lapply(focal, function(cf) {
          k <- kr(mmm_ci_contrast(m, stats::setNames(list(1), cf), cf))
          data.table::data.table(component = cf, estimate = k$estimate, se = k$se, df = k$df, statistic = k$statistic,
                                 ci_low = k$ci_low, ci_high = k$ci_high, status = k$status) }))
      }
      if (joint && !kr_se_only) {
        jt <- kr(mmm_ci_joint(m, focal, label))
        if (!identical(jt$status, "OK")) stop("KR joint F FAILED")
        s30sc_set(r, F = jt$F, df1 = jt$df1, df2 = jt$df2, df = jt$df2, statistic = jt$F, p_raw = jt$p_raw)
      } else r
    }
  }, error = function(e) e)
  if (inherits(res, "error")) { out$components <- NULL   # a FAILED fit carries no (partial) component estimates
    out$row <- s30sc_set(add_msgs(r), status = "FAILED", error = paste("inference step:", conditionMessage(res))); return(out) }
  r <- add_msgs(res)   # (res is r, updated by reference)
  if (!is.null(out$components) && any(out$components$status != "OK")) { out$row <- s30sc_set(r, status = "FAILED", error = "a KR component FAILED"); return(out) }
  needs_p <- (lm_route || engine == "KR t" || joint) && !kr_se_only
  bad <- if (engine == "KR F(3)") (needs_p && !is.finite(r$p_raw)) || (!is.null(out$components) && !all(is.finite(out$components$estimate)))
         else !is.finite(r$estimate) || !is.finite(r$se) || (needs_p && !is.finite(r$p_raw))
  out$row <- if (bad) s30sc_set(r, status = "FAILED", error = "non-finite estimate / SE / p") else s30sc_set(r, status = "OK")
  out
}

#' Standardizer (registry inference.standardized_estimate): sd of residuals of lm(v ~ Batch) in the analysis population.
s30sc_standardizer <- function(d, v) {
  f <- if (data.table::uniqueN(d$Batch) > 1) stats::as.formula(paste(v, "~ Batch")) else stats::as.formula(paste(v, "~ 1"))
  stats::sd(stats::resid(stats::lm(f, data = d)))
}

# ---------------------------------------------------------------- sensitivities, LOBO, influence
s30sc_primary_ref <- function(e, pr) {
  if (e$question == "L") {
    cp <- pr$components
    if (!identical(pr$row$status, "OK") || is.null(cp) || !nrow(cp) || !all(is.finite(cp$statistic))) return(NULL)
    k <- which.max(abs(cp$statistic))
    list(component = cp$component, estimate = cp$estimate, se = cp$se, key = cp$component[k])
  } else if (identical(pr$row$status, "OK")) list(component = e$focal_terms[[1]], estimate = pr$row$estimate, se = pr$row$se, key = e$focal_terms[[1]]) else NULL
}

#' Declared estimation-only sensitivities of one entry (registry sensitivities_estimation_only), with robustness labels
#' (mmm_ci_robust_label) against the primary. L rows: one per focal component (+ the Movement-adjusted joint KR F, the
#' decision quantity of rule D(i) only). No p-value here is a decision quantity except that joint F p (rule D(i)).
s30sc_sensitivities <- function(D, e, pr) {
  ref <- s30sc_primary_ref(e, pr); rows <- list(); fits <- list()
  focal <- e$focal_terms[[1]]
  add <- function(sid, desc, fr, counts_for_A, used_for_D, formula) {
    info <- fr$row
    comp <- if (e$question == "L" && !is.null(fr$components)) fr$components else
      data.table::data.table(component = focal, estimate = info$estimate, se = info$se, df = info$df, statistic = info$statistic,
                             ci_low = info$ci_low, ci_high = info$ci_high, status = info$status)
    if (!is.null(fr$m)) fits[[length(fits) + 1L]] <<- cbind(data.table::data.table(context = paste("sensitivity", sid), id = e$id), fr$m$info)
    for (j in seq_len(nrow(comp))) {
      cj <- comp[j]; pe <- if (is.null(ref)) NA_real_ else ref$estimate[match(cj$component, ref$component)]
      ps <- if (is.null(ref)) NA_real_ else ref$se[match(cj$component, ref$component)]
      ok <- identical(info$status, "OK") && identical(cj$status, "OK") && is.finite(cj$estimate)
      lab <- if (!ok) "FAILED" else if (!is.finite(pe) || !is.finite(ps)) "NO_PRIMARY" else mmm_ci_robust_label(pe, ps, cj$estimate)
      is_key <- is.null(ref) || e$question != "L" || identical(cj$component, ref$key)
      rows[[length(rows) + 1L]] <<- data.table::data.table(
        id = e$id, role = e$role, family = e$family, block = e$block, question = e$question, sex_analysis = e$sex_analysis, measure = e$measure,
        sensitivity_id = sid, description = desc, component = cj$component, is_classification_component = is_key, formula = formula,
        engine_route = info$engine_route, status = if (ok) "OK" else s30sc_nz(info$status, "FAILED"), error = info$error,
        estimate = cj$estimate, se = cj$se, df = cj$df, ci_low = cj$ci_low, ci_high = cj$ci_high,
        primary_estimate = pe, primary_se = ps, shift_se = (cj$estimate - pe) / ps, robustness_label = lab,
        counts_for_class_A = counts_for_A && is_key, used_for_rule_D = used_for_D && is_key,
        low_df_label = if (!grepl("CR2", s30sc_nz(info$engine_route, ""))) NA_character_ else if (is.finite(cj$df) && cj$df < S30SC_LOW_DF) "LOW_DF" else "NOT_LOW_DF",
        joint_F = if (used_for_D && e$question == "L") info$F else NA_real_, joint_df1 = if (used_for_D && e$question == "L") info$df1 else NA_real_,
        joint_df2 = if (used_for_D && e$question == "L") info$df2 else NA_real_,
        joint_p_rule_D_only = if (used_for_D && e$question == "L") info$p_raw else NA_real_,
        n_obs = info$n_obs, n_animals = info$n_animals, n_cages = info$n_cages, n_batches = info$n_batches, rank = info$rank,
        singular = info$singular, converged = info$converged, fit_messages = info$fit_messages)
    }
  }
  run <- function(d, formula, engine, joint = FALSE, expected_rank = NULL)
    s30sc_fit(d, formula, engine, paste(e$id, "sens", sep = "|"), focal, expected_rank = expected_rank, joint = joint)
  guard <- function(sid, desc, counts_for_A, used_for_D, formula, expr) {
    fr <- tryCatch(expr, error = function(err) list(row = s30sc_set(S30SC_FIT_TEMPLATE(), status = "FAILED", error = conditionMessage(err)), components = NULL, m = NULL))
    add(sid, desc, fr, counts_for_A, used_for_D, formula)
  }
  if (e$question == "CONT" && identical(pr$row$status, "OK")) {
    r <- pr$row; q <- stats::qt(0.975, r$ols_df)
    fr <- list(row = s30sc_set(data.table::copy(r), engine_route = "lm; classical OLS t (n - p df); descriptive only", se = r$ols_se, df = r$ols_df,
                               ci_low = r$estimate - q * r$ols_se, ci_high = r$estimate + q * r$ols_se), components = NULL, m = NULL)
    add("OLS_T", "OLS t alongside CR2 (descriptive only; E4 null size up to 0.31)", fr, TRUE, FALSE, e$formula)
  }
  pool <- e$role == "ESTIMATION_ONLY"
  if (e$block == "inactivity") {
    guard("POSINACT60", "posinact60_frac in place of posinact40_frac (same model; measurement sensitivity)", TRUE, FALSE, e$formula,
          run(s30sc_hyp_data(D, e, measure_col = e$sens_col), e$formula, e$engine, expected_rank = e$expected_rank))
    fm <- tryCatch(s30sc_movadj_formula(e$formula, e$question, e$sex_analysis), error = function(err) NA_character_)   # NA -> a FAILED row
    guard("MOVADJ", paste("Movement-adjusted incremental information: M =", e$movadj_col, "(same window)"), TRUE, !pool, fm,
          run(s30sc_hyp_data(D, e, with_M = TRUE), fm, e$engine, joint = (e$question == "L" && !pool)))
  }
  if (e$block == "cookie") {
    guard("DCOOKIE45", "dcookie45 in place of dcookie60 (same model)", TRUE, FALSE, e$formula,
          run(s30sc_hyp_data(D, e, measure_col = e$sens_col), e$formula, e$engine, expected_rank = e$expected_rank))
    if (e$question == "CONT") {
      fa <- paste(e$formula, "+ (1 | CageEpisodeID)")
      guard("ALT_KR", "alternative inference: lmer (1 | CageEpisodeID), KR t (estimate and KR CI only; never a decision quantity)", TRUE, FALSE, fa,
            run(s30sc_hyp_data(D, e), fa, "KR t"))
    } else {
      fa <- tryCatch(s30sc_fixed_formula(e$formula), error = function(err) NA_character_)
      guard("ALT_CR2", "robustness: lm fixed part with CR2 by CageEpisodeID, Satterthwaite df (diagnostic; LOW_DF if df < 4)", TRUE, FALSE, fa,
            run(s30sc_hyp_data(D, e), fa, "CR2"))
    }
    fm <- tryCatch(s30sc_movadj_formula(e$formula, e$question, e$sex_analysis), error = function(err) NA_character_)   # NA -> a FAILED row
    guard("MOVADJ", "Movement-adjusted incremental information: M = Stage 29 CC1 active crossing_rate", TRUE, FALSE, fm,
          run(s30sc_hyp_data(D, e, with_M = TRUE), fm, e$engine))
  }
  list(rows = data.table::rbindlist(rows, fill = TRUE), fits = data.table::rbindlist(fits, fill = TRUE))
}

#' Leave-one-batch-out refits (registry sensitivities_estimation_only.all[1]): every batch of the entry's own population
#' (within-sex: same-sex batches only); Batch dropped when one batch remains; gated on full rank only; L: the three focal
#' components with KR SEs.
s30sc_lobo <- function(d, e, pr) {
  ref <- s30sc_primary_ref(e, pr); focal <- e$focal_terms[[1]]
  bs <- levels(droplevels(d$Batch)); fits <- list()
  rows <- data.table::rbindlist(lapply(bs, function(b) {
    db <- droplevels(d[as.character(Batch) != b]); single <- nlevels(db$Batch) == 1L
    f <- if (single) s30sc_drop_batch(e$formula) else e$formula
    fr <- tryCatch(s30sc_fit(db, f, e$engine, paste(e$id, "LOBO", b, sep = "|"), focal, expected_rank = NULL, joint = FALSE, kr_se_only = TRUE),
                   error = function(err) list(row = s30sc_set(S30SC_FIT_TEMPLATE(), status = "FAILED", error = conditionMessage(err)), components = NULL, m = NULL))
    if (!is.null(fr$m)) fits[[length(fits) + 1L]] <<- cbind(data.table::data.table(context = paste("LOBO", b), id = e$id), fr$m$info)
    comp <- if (e$question == "L") { if (is.null(fr$components)) data.table::data.table(component = focal, estimate = NA_real_, se = NA_real_, df = NA_real_) else fr$components[, .(component, estimate, se, df)] } else
      data.table::data.table(component = focal, estimate = fr$row$estimate, se = fr$row$se, df = fr$row$df)
    ok <- identical(fr$row$status, "OK")
    pe <- if (is.null(ref)) rep(NA_real_, nrow(comp)) else ref$estimate[match(comp$component, ref$component)]
    dfv <- if (ok) comp$df else rep(NA_real_, nrow(comp))
    data.table::data.table(id = e$id, role = e$role, family = e$family, sex_analysis = e$sex_analysis, left_out_batch = b,
                           remaining_batches = nlevels(db$Batch), batch_term_dropped = single, formula = f, component = comp$component,
                           is_classification_component = if (is.null(ref)) TRUE else comp$component == ref$key,
                           status = if (ok) "OK" else s30sc_nz(fr$row$status, "FAILED"), error = fr$row$error,
                           estimate = if (ok) comp$estimate else NA_real_, se = if (ok) comp$se else NA_real_,
                           df = dfv, df_method = switch(e$engine, CR2 = "Satterthwaite (CR2)", "KR t" = "Kenward-Roger", "KR F(3)" = "none (KR-adjusted SE only)"),
                           low_df_label = if (e$engine != "CR2") NA_character_ else ifelse(is.finite(dfv), ifelse(dfv < S30SC_LOW_DF, "LOW_DF", "NOT_LOW_DF"), NA_character_),
                           primary_estimate = pe, same_sign_as_primary = if (ok) sign(comp$estimate) == sign(pe) else NA,
                           n_obs = fr$row$n_obs, n_animals = fr$row$n_animals, n_cages = fr$row$n_cages, rank = fr$row$rank,
                           singular = fr$row$singular, converged = fr$row$converged, fit_messages = fr$row$fit_messages)
  }), fill = TRUE)
  key <- rows[is_classification_component == TRUE]
  sign_str <- function(x) paste(sprintf("%s:%s", x$left_out_batch, ifelse(x$status == "OK", s30sc_sign_chr(x$estimate), "FAILED")), collapse = "|")
  lev_str <- function(x) paste(x[status != "OK" | same_sign_as_primary %in% FALSE, left_out_batch], collapse = "|")
  bc <- rows[, .(signs = sign_str(.SD), lev = lev_str(.SD)), by = component, .SDcols = c("left_out_batch", "status", "estimate", "same_sign_as_primary")]
  summ <- list(lobo_component = if (is.null(ref)) NA_character_ else ref$key, lobo_n_refits = length(bs),
               lobo_n_ok = sum(key$status == "OK"),
               lobo_signs = sign_str(key), lobo_leverage_batches = lev_str(key),
               lobo_signs_by_component = paste(sprintf("%s %s", bc$component, bc$signs), collapse = "; "),
               lobo_leverage_batches_by_component = paste(sprintf("%s %s", bc$component, ifelse(nzchar(bc$lev), bc$lev, "none")), collapse = "; "),
               lobo_sign_stable = !is.null(ref) && nrow(key) == length(bs) && all(key$status == "OK") && all(key$same_sign_as_primary %in% TRUE))
  list(rows = rows, summary = summ, fits = data.table::rbindlist(fits, fill = TRUE))
}

#' Animal influence (registry sensitivities_estimation_only.all[2]): lm: stats::dfbeta; lmer: case-deletion refits by animal
#' (lme4::lmer, same REML / bobyqa control as the engine). Reported as max |delta| / SE of the primary fit with the animal
#' ID. Estimation only; not a classification input.
s30sc_influence <- function(e, pr) {
  ref <- s30sc_primary_ref(e, pr); m <- pr$m
  base <- data.table::data.table(id = e$id, role = e$role, family = e$family, sex_analysis = e$sex_analysis)
  if (is.null(ref) || is.null(m) || is.null(m$fit))
    return(cbind(base, data.table::data.table(component = NA_character_, method = NA_character_, status = "NOT_RUN (primary not OK)")))
  if (e$engine == "CR2") {
    fc <- pr$focal; db <- stats::dfbeta(m$fit)[, fc]; ratio <- abs(db) / pr$row$se; k <- which.max(ratio)
    return(cbind(base, data.table::data.table(component = fc, method = "stats::dfbeta (lm), scaled by the primary CR2 SE", status = "OK",
      n_units = length(ratio), n_refits_failed = 0L, n_refits_warned = 0L, se_reference = pr$row$se, se_type = "CR2",
      max_abs_dfbeta_over_se = ratio[k], animal_at_max = as.character(m$data$AnimalNum[k]), median_abs_dfbeta_over_se = stats::median(ratio))))
  }
  f <- stats::as.formula(m$info$formula); dat <- m$data; b0 <- lme4::fixef(m$fit)
  nm <- mmm_ci_match(ref$component, names(b0)); an <- sort(unique(as.character(dat$AnimalNum)))
  ctl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5), check.rankX = "stop.deficient")
  warned <- 0L
  D <- do.call(rbind, lapply(an, function(a) {
    w <- FALSE
    fit <- tryCatch(withCallingHandlers(lme4::lmer(f, data = droplevels(dat[as.character(AnimalNum) != a]), REML = TRUE, control = ctl),
                                        warning = function(x) { w <<- TRUE; invokeRestart("muffleWarning") },
                                        message = function(x) invokeRestart("muffleMessage")), error = function(err) NULL)
    if (w) warned <<- warned + 1L
    if (is.null(fit)) return(rep(NA_real_, length(nm)))
    b <- lme4::fixef(fit); mm <- tryCatch(mmm_ci_match(ref$component, names(b)), error = function(err) NULL)
    if (is.null(mm)) rep(NA_real_, length(nm)) else unname(b0[nm] - b[mm])
  }))
  D <- matrix(D, ncol = length(nm))
  data.table::rbindlist(lapply(seq_along(nm), function(j) {
    ratio <- abs(D[, j]) / ref$se[j]; k <- if (all(is.na(ratio))) NA_integer_ else which.max(ratio)
    cbind(base, data.table::data.table(component = ref$component[j], method = "case-deletion refits by animal (lme4::lmer REML bobyqa), scaled by the primary KR SE",
      status = "OK", n_units = length(an), n_refits_failed = sum(is.na(D[, j])), n_refits_warned = warned, se_reference = ref$se[j], se_type = "KR",
      max_abs_dfbeta_over_se = if (is.na(k)) NA_real_ else ratio[k], animal_at_max = if (is.na(k)) NA_character_ else an[k],
      median_abs_dfbeta_over_se = stats::median(ratio, na.rm = TRUE)))
  }))
}

#' Nested leave-one-batch-out prediction (cookie hypotheses classified A; estimation only, no effect on classification):
#' in each training fold (all other batches of the population) the fixed part of the registered model is refitted by OLS
#' (Batch dropped with one training batch); the held-out batch is predicted from the focal terms only, within-batch
#' centred (the batch intercept is not transportable). Reports per fold and pooled out-of-batch R^2 of centred values;
#' CAT also the held-out within-batch AUC P(y_RES > y_SUS) (+ half the ties) and its orientation by the training sign
#' (driver convention C6; the registry gives no algorithm).
s30sc_auc <- function(y, g) {
  a <- y[g > 0]; b <- y[g < 0]
  if (!length(a) || !length(b)) return(NA_real_)
  mean(outer(a, b, ">") + 0.5 * outer(a, b, "=="))
}
s30sc_nested_lobo_prediction <- function(d, e, triggered) {
  yv <- if (e$question == "CONT") "CombZ" else "y"
  terms <- if (e$question == "CONT") c("x", if (e$sex_analysis == "INT") "x:sex_c") else c("g_RS", if (e$sex_analysis == "INT") "g_RS:sex_c")
  val <- function(dd, t) if (grepl(":sex_c$", t)) dd[[sub(":sex_c$", "", t)]] * dd$sex_c else dd[[t]]
  bs <- levels(droplevels(d$Batch))
  folds <- data.table::rbindlist(lapply(bs, function(b) {
    tr <- droplevels(d[as.character(Batch) != b]); te <- d[as.character(Batch) == b]
    f <- s30sc_fixed_formula(e$formula); if (nlevels(tr$Batch) == 1L) f <- s30sc_drop_batch(f)
    fit <- stats::lm(stats::as.formula(f), data = tr); cf <- stats::coef(fit)
    nm <- mmm_ci_match(terms, names(cf))
    pred <- Reduce(`+`, lapply(seq_along(terms), function(j) cf[[nm[j]]] * val(te, terms[j])))
    pc <- pred - mean(pred); yc <- te[[yv]] - mean(te[[yv]])
    auc <- if (e$question == "CAT") s30sc_auc(te$y, te$g_RS) else NA_real_
    data.table::data.table(id = e$id, triggered_by_class_A = triggered, held_out_batch = b, n_train = nrow(tr), n_test = nrow(te),
                           train_focal_estimate = cf[[nm[1]]], r_pred_obs_within_batch = if (isTRUE(stats::sd(pc) > 0) && isTRUE(stats::sd(yc) > 0)) stats::cor(pc, yc) else NA_real_,
                           sse = sum((yc - pc)^2), sst = sum(yc^2),
                           auc_res_vs_sus_heldout = auc,
                           auc_oriented_by_training_sign = if (is.finite(auc)) (if (cf[[nm[1]]] >= 0) auc else 1 - auc) else NA_real_)
  }))
  folds[, oob_R2_centred_pooled := 1 - sum(sse) / sum(sst)]
  folds[, method := "driver convention C6 (estimation only; never a classification input)"]
  folds[]
}

# ---------------------------------------------------------------- one entry, the whole screen
s30sc_run_entry <- function(D, e, log = function(...) invisible(NULL)) {
  d <- s30sc_hyp_data(D, e)   # an error here fails the entry (its primary cannot be fitted)
  pr <- s30sc_fit(d, e$formula, e$engine, e$id, e$focal_terms[[1]], expected_rank = e$expected_rank,
                  joint = (e$question == "L" && e$role == "DISCOVERY"), always_optimizer_check = grepl("||", e$formula, fixed = TRUE))
  blk <- character()   # block-level errors after the primary fit never change the primary result
  keep <- function(what, expr, empty) tryCatch(expr, error = function(err) { blk <<- c(blk, paste0(what, ": ", conditionMessage(err))); empty })
  std <- NA_real_; std_sd <- NA_real_; std_rule <- "none (joint F)"
  if (identical(pr$row$status, "OK") && e$question == "CONT") { std_sd <- keep("standardizer", s30sc_standardizer(d, "x"), NA_real_); std <- pr$row$estimate * std_sd; std_rule <- "estimate x SD_x (sd of resid(lm(x ~ Batch)))" }
  if (identical(pr$row$status, "OK") && e$question == "CAT") { std_sd <- keep("standardizer", s30sc_standardizer(d, "y"), NA_real_); std <- pr$row$estimate / std_sd; std_rule <- "estimate / SD_y (sd of resid(lm(y ~ Batch)))" }
  if (e$question == "L") std_rule <- "none (L: joint F; components reported with KR SEs)"
  sens <- keep("sensitivities", s30sc_sensitivities(D, e, pr), list(rows = data.table::data.table(), fits = data.table::data.table(), block_failed = TRUE))
  lobo <- keep("LOBO", s30sc_lobo(d, e, pr), list(rows = data.table::data.table(), fits = data.table::data.table(),
               summary = list(lobo_component = NA_character_, lobo_n_refits = NA_integer_, lobo_n_ok = 0L, lobo_signs = "LOBO block FAILED",
                              lobo_leverage_batches = NA_character_, lobo_signs_by_component = "LOBO block FAILED",
                              lobo_leverage_batches_by_component = NA_character_, lobo_sign_stable = FALSE)))
  infl <- keep("influence", s30sc_influence(e, pr), data.table::data.table(id = e$id, role = e$role, family = e$family, sex_analysis = e$sex_analysis, status = "FAILED (block error)"))
  fits <- data.table::rbindlist(list(if (!is.null(pr$m)) cbind(data.table::data.table(context = "primary", id = e$id), pr$m$info), sens$fits, lobo$fits), fill = TRUE)
  list(id = e$id, d = d, primary = pr, std = std, std_sd = std_sd, std_rule = std_rule, sens = sens$rows, sens_block_failed = isTRUE(sens$block_failed),
       lobo = lobo, influence = infl, fits = fits, block_errors = blk)
}

#' Result classification (registry result_classification_rules; order D, A, B, C; first match wins).
s30sc_classify <- function(status, p_raw, q_local, block, question, movadj_status, movadj_ci_includes_0, movadj_joint_p,
                           lobo_sign_stable, any_sign_change, alpha = S30SC_ALPHA) {
  n <- length(status); cls <- character(n); code <- character(n); detail <- character(n)
  for (i in seq_len(n)) {
    if (status[i] %in% c("FAILED", "FAILED_RANK")) { cls[i] <- "D"; code[i] <- "D-FIT"; detail[i] <- paste("primary fit", status[i], "(enters multiplicity with p = 1)"); next }
    sig <- is.finite(p_raw[i]) && p_raw[i] < alpha
    if (block[i] == "inactivity" && sig) {
      mv_fail <- !identical(movadj_status[i], "OK")
      mv_null <- if (question[i] == "L") !is.finite(movadj_joint_p[i]) || movadj_joint_p[i] >= alpha else !isFALSE(movadj_ci_includes_0[i])
      if (mv_fail || mv_null) { cls[i] <- "D"; code[i] <- "D-MOVEMENT"
        detail[i] <- if (mv_fail) "D-MOVEMENT (Movement-adjusted model FAILED; D(i) not evaluable, treated conservatively: driver convention C1)" else
          if (question[i] == "L") "raw p < 0.05; Movement-adjusted joint KR F p >= 0.05" else "raw p < 0.05; Movement-adjusted focal 95% CI includes 0"
        next }
    }
    if (is.finite(q_local[i]) && q_local[i] < alpha && isTRUE(lobo_sign_stable[i]) && isFALSE(any_sign_change[i])) {
      cls[i] <- "A"; code[i] <- "A"; detail[i] <- "local BH q < 0.05; focal sign stable in every LOBO refit; no declared sensitivity SIGN_CHANGE"; next }
    if (sig) { cls[i] <- "B"
      if (!is.finite(q_local[i]) || q_local[i] >= alpha) { code[i] <- "B-MULT"; detail[i] <- "raw p < 0.05, local BH q >= 0.05 (multiplicity-negative)" } else {
        code[i] <- "B-ROBUST"; detail[i] <- paste("local BH q < 0.05 but", paste(c(if (!isTRUE(lobo_sign_stable[i])) "LOBO sign not stable in every refit (a FAILED refit counts as not stable: convention C2)",
                                                                              if (isTRUE(any_sign_change[i])) "a declared sensitivity is SIGN_CHANGE", if (is.na(any_sign_change[i])) "the sensitivity block could not be evaluated (convention C3)"), collapse = " and ")) }
      next }
    cls[i] <- "C"; code[i] <- "C"; detail[i] <- "raw p >= 0.05: no evidence of association at the registered test (report the CI; never 'no effect')"
  }
  data.table::data.table(classification = cls, reason_code = code, reason_detail = detail)
}

#' Sex-specific wording permission for F / M rows (registry: INT local BH q < 0.05 is necessary; driver convention C7 adds:
#' this row classified A or B and its direction consistent with the INT estimate, INT = (F - M) with sex_c = +1/2 F, -1/2 M:
#' F: sign(INT) = sign(F); M: sign(INT) = -sign(M)). L: estimate_row / estimate_int are the row's max-|KR t| component and
#' the matching INT component. Returns NA for INT / POOL rows.
s30sc_sex_specific_allowed <- function(sex_analysis, classification, estimate_row, estimate_int, int_ok, int_q, alpha = S30SC_ALPHA) {
  fin <- is.finite(estimate_row) & is.finite(estimate_int) & estimate_row != 0 & estimate_int != 0
  dir_ok <- fin & ifelse(sex_analysis == "F", sign(estimate_int) == sign(estimate_row), sign(estimate_int) == -sign(estimate_row))
  out <- (int_ok %in% TRUE) & is.finite(int_q) & int_q < alpha & classification %in% c("A", "B") & (dir_ok %in% TRUE)
  ifelse(sex_analysis %in% c("F", "M"), out, NA)
}

#' Wording flags (registry result_classification_rules.wording, sex_analyses.rule, terminology).
#' sex_specific_allowed: s30sc_sex_specific_allowed() for F / M rows.
s30sc_wording <- function(e, classification, low_df, int_q_local, sex_specific_allowed = NA) {
  f <- c("EXPLORATORY_POST_HOC_CONTEXT: never primary / confirmatory / preregistered")
  f <- c(f, switch(e$sex_analysis, F = "describe as 'in females'", M = "describe as 'in males'",
                   INT = "formal sex-moderation test (F - M); sex specificity is inferred only from this test",
                   POOL = "ESTIMATION_ONLY: 'pooled across sexes (estimation only)', never 'in both sexes'"))
  if (e$sex_analysis %in% c("F", "M")) {
    sx <- if (e$sex_analysis == "F") "female" else "male"
    int_sig <- is.finite(int_q_local) && int_q_local < S30SC_ALPHA
    f <- c(f, if (int_sig && isTRUE(sex_specific_allowed)) paste0("'", sx, "-specific' allowed, in the direction of this row only (INT local q < 0.05; this row A/B and direction-consistent with INT: convention C7)")
           else if (int_sig) paste0("INT q < 0.05 but this sex's association is not supported (not A/B, or direction inconsistent with INT): no '", sx, "-specific' wording")
           else "no 'female-specific' / 'male-specific' wording (INT local q >= 0.05 or INT not OK)")
  }
  if (e$block == "inactivity") f <- c(f, "RFID-defined sustained positional inactivity (>= 40 s): an RFID-derived sleep/inactivity proxy; never physiological sleep duration")
  if (e$block == "inactivity" && e$phase == "light") f <- c(f, "LIGHT_PHASE_SATURATED: the metric is saturated (87% of light windows >= 0.99); inference rests on its lower tail")
  if (identical(classification, "D-MOVEMENT")) f <- c(f, "D-MOVEMENT: never describe as independent sleep biology")
  if (identical(classification, "C")) f <- c(f, "'no evidence of association at the registered test' with its CI; never 'no effect'")
  if (e$block == "cookie") f <- c(f, "home-cage cookie response (second presentation, EPM+1): a locomotor challenge-response phenotype; not approach, proximity, investigation or consumption")
  if (grepl("shared_zone_use", e$measure)) f <- c(f, "shared RFID-position occupancy: not sociability, coordination or huddling")
  if (grepl("crossing_rate", e$measure_col) || grepl("crossing_rate", s30sc_nz(e$movadj_col, ""))) f <- c(f, "Movement = 'RFID position-change rate' (position changes/h); never 'antenna crossing rate'")
  if (isTRUE(low_df)) f <- c(f, "LOW_DF: CR2 Satterthwaite df < 4; registered p kept (conservative by design)")
  paste(f, collapse = " | ")
}

#' Run every entry (48 discovery + 16 estimation-only) once, then multiplicity, classification and wording.
#' Never stops on a single entry: an entry-level error becomes a FAILED row (recorded in run_failures).
s30sc_run_screen <- function(D, spec, design_df = NULL, force_nested = FALSE, log = function(...) message(...)) {
  R <- list(); failures <- list()
  for (i in seq_len(nrow(spec))) {
    e <- spec[i]; t0 <- Sys.time()
    R[[e$id]] <- tryCatch(s30sc_run_entry(D, e), error = function(err) {
      failures[[length(failures) + 1L]] <<- data.table::data.table(block = paste("entry", e$id), error = conditionMessage(err))
      list(id = e$id, d = NULL, primary = list(row = s30sc_set(S30SC_FIT_TEMPLATE(), status = "FAILED", error = conditionMessage(err)), components = NULL, m = NULL),
           std = NA_real_, std_sd = NA_real_, std_rule = NA_character_, sens = data.table::data.table(), lobo = list(rows = data.table::data.table(),
           summary = list(lobo_component = NA_character_, lobo_n_refits = NA_integer_, lobo_n_ok = 0L, lobo_signs = NA_character_,
                          lobo_leverage_batches = NA_character_, lobo_signs_by_component = NA_character_, lobo_leverage_batches_by_component = NA_character_,
                          lobo_sign_stable = FALSE)), influence = data.table::data.table(), fits = data.table::data.table(),
           sens_block_failed = TRUE, block_errors = character())
    })
    R[[e$id]]$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    log(sprintf("[%2d/%d] %-24s %-10s %6.1f s", i, nrow(spec), e$id, R[[e$id]]$primary$row$status, R[[e$id]]$seconds))
  }
  for (x in R) if (length(x$block_errors)) failures[[length(failures) + 1L]] <- data.table::data.table(block = paste("entry", x$id, "(post-primary block)"), error = paste(x$block_errors, collapse = " | "))
  # ---- per-entry rows
  rows <- data.table::rbindlist(lapply(seq_len(nrow(spec)), function(i) {
    e <- spec[i]; x <- R[[e$id]]; r <- x$primary$row; s <- x$sens; lb <- x$lobo$summary
    comp <- x$primary$components
    mv <- if (nrow(s)) s[sensitivity_id == "MOVADJ" & is_classification_component == TRUE] else data.table::data.table()
    labs <- if (nrow(s)) s[counts_for_class_A == TRUE] else data.table::data.table()
    ifl <- x$influence
    ifl_key <- if (nrow(ifl) && "max_abs_dfbeta_over_se" %in% names(ifl) && any(is.finite(ifl$max_abs_dfbeta_over_se))) ifl[which.max(max_abs_dfbeta_over_se)] else NULL
    ddf <- if (!is.null(design_df) && e$id %in% design_df$id) design_df[id == e$id, design_df_cr2][1] else NA_real_
    data.table::data.table(
      id = e$id, role = e$role, family = e$family, block = e$block, question = e$question, sex_analysis = e$sex_analysis,
      sex_scope = switch(e$sex_analysis, F = "within females", M = "within males", INT = "sex moderation (F - M)", POOL = "pooled across sexes (estimation only)"),
      cell = e$cell, population = e$population, measure = e$measure, measure_col = e$measure_col, measure_role = e$measure_role, phase = e$phase,
      formula = e$formula, focal = e$focal_text, focal_terms = paste(e$focal_terms[[1]], collapse = "; "), engine = e$engine,
      engine_route = r$engine_route, status = r$status, error = r$error,
      estimate = if (e$question == "L") NA_real_ else r$estimate, standardized_estimate = x$std, standardizer_sd = x$std_sd, standardization_rule = x$std_rule,
      se = if (e$question == "L") NA_real_ else r$se, se_type = switch(e$engine, CR2 = "CR2 (clubSandwich)", "KR t" = "Kenward-Roger", "KR F(3)" = "Kenward-Roger (components)"),
      df = r$df, df_method = switch(e$engine, CR2 = "Satterthwaite (CR2)", "Kenward-Roger"),
      ci_low = if (e$question == "L") NA_real_ else r$ci_low, ci_high = if (e$question == "L") NA_real_ else r$ci_high,
      statistic = r$statistic, statistic_type = switch(e$engine, CR2 = "t", "KR t" = "t", "KR F(3)" = if (e$role == "DISCOVERY") "F(3, df_KR)" else "none (estimation only)"),
      df1 = r$df1, df2 = r$df2, p_raw = if (e$role == "DISCOVERY") r$p_raw else NA_real_,
      n_animals = r$n_animals, n_windows = r$n_obs, n_cages = r$n_cages, n_batches = r$n_batches, batches = r$batches,
      rank = r$rank, n_fixed_cols = r$n_fixed_cols, expected_rank = e$expected_rank,
      rank_matches_expected = isTRUE(r$n_fixed_cols == e$expected_rank && r$rank == e$expected_rank),
      design_df_cr2 = ddf, expected_df_cr2 = e$expected_df_cr2,
      df_cr2_matches_expected = if (e$engine == "CR2") isTRUE(abs(r$df - e$expected_df_cr2) <= S30SC_DF_TOL) else NA,
      low_df_label = if (e$engine != "CR2") NA_character_ else if (is.finite(r$df) && r$df < S30SC_LOW_DF) "LOW_DF" else "NOT_LOW_DF",
      singular = r$singular, converged = r$converged, failed = r$failed, optimizer_check_run = r$optimizer_check_run, optimizer_check_agree = r$optimizer_check_agree,
      ols_se = r$ols_se, ols_df = r$ols_df, ols_p_descriptive = if (e$role == "DISCOVERY") r$ols_p else NA_real_,
      cr2_diagnostic_estimate = if (nrow(s) && any(s$sensitivity_id == "ALT_CR2")) s[sensitivity_id == "ALT_CR2", estimate][1] else NA_real_,
      cr2_diagnostic_se = if (nrow(s) && any(s$sensitivity_id == "ALT_CR2")) s[sensitivity_id == "ALT_CR2", se][1] else NA_real_,
      cr2_diagnostic_df = if (nrow(s) && any(s$sensitivity_id == "ALT_CR2")) s[sensitivity_id == "ALT_CR2", df][1] else NA_real_,
      cr2_diagnostic_ci_low = if (nrow(s) && any(s$sensitivity_id == "ALT_CR2")) s[sensitivity_id == "ALT_CR2", ci_low][1] else NA_real_,
      cr2_diagnostic_ci_high = if (nrow(s) && any(s$sensitivity_id == "ALT_CR2")) s[sensitivity_id == "ALT_CR2", ci_high][1] else NA_real_,
      cr2_diagnostic_low_df = if (nrow(s) && any(s$sensitivity_id == "ALT_CR2")) s[sensitivity_id == "ALT_CR2", low_df_label][1] else NA_character_,
      l_components = if (!is.null(comp) && nrow(comp)) paste(sprintf("%s=%.6g [%.6g, %.6g] (KR SE %.6g)", comp$component, comp$estimate, comp$ci_low, comp$ci_high, comp$se), collapse = "; ") else NA_character_,
      l_max_t_component = if (!is.null(comp) && nrow(comp) && all(is.finite(comp$statistic))) comp$component[which.max(abs(comp$statistic))] else NA_character_,
      lobo_component = lb$lobo_component, lobo_n_refits = lb$lobo_n_refits, lobo_n_ok = lb$lobo_n_ok, lobo_signs = lb$lobo_signs,
      lobo_leverage_batches = lb$lobo_leverage_batches, lobo_sign_stable = lb$lobo_sign_stable,
      lobo_signs_by_component = s30sc_nz(lb$lobo_signs_by_component, NA_character_),
      lobo_leverage_batches_by_component = s30sc_nz(lb$lobo_leverage_batches_by_component, NA_character_),
      robustness_labels = if (nrow(labs)) paste(sprintf("%s=%s", labs$sensitivity_id, labs$robustness_label), collapse = "; ") else "",
      any_sign_change = if (isTRUE(x$sens_block_failed)) NA else if (nrow(labs)) any(labs$robustness_label == "SIGN_CHANGE") else FALSE,
      any_sensitivity_failed = if (nrow(labs)) any(labs$robustness_label == "FAILED") else FALSE,
      block_errors = paste(x$block_errors, collapse = " | "),
      movadj_status = if (nrow(mv)) mv$status[1] else NA_character_, movadj_estimate = if (nrow(mv)) mv$estimate[1] else NA_real_,
      movadj_se = if (nrow(mv)) mv$se[1] else NA_real_, movadj_df = if (nrow(mv)) mv$df[1] else NA_real_,
      movadj_ci_low = if (nrow(mv)) mv$ci_low[1] else NA_real_, movadj_ci_high = if (nrow(mv)) mv$ci_high[1] else NA_real_,
      movadj_ci_includes_0 = if (nrow(mv) && is.finite(mv$ci_low[1])) mv$ci_low[1] <= 0 & mv$ci_high[1] >= 0 else NA,
      movadj_joint_F = if (nrow(mv)) mv$joint_F[1] else NA_real_, movadj_joint_df1 = if (nrow(mv)) mv$joint_df1[1] else NA_real_,
      movadj_joint_df2 = if (nrow(mv)) mv$joint_df2[1] else NA_real_, movadj_joint_p = if (nrow(mv)) mv$joint_p_rule_D_only[1] else NA_real_,
      influence_max_abs_dfbeta_over_se = if (is.null(ifl_key)) NA_real_ else ifl_key$max_abs_dfbeta_over_se,
      influence_animal = if (is.null(ifl_key)) NA_character_ else ifl_key$animal_at_max,
      calibration_verdict = e$calibration_verdict, calibration_size05 = e$calibration_size05, calibration_size10 = e$calibration_size10,
      calibration_mcse05 = e$calibration_mcse05, calibration_size05_range_scenarios = e$calibration_size05_range, calibration_df_median = e$calibration_df_median,
      calibration_singular_share = e$calibration_singular_share, calibration_note = e$calibration_note, robustness_text = e$robustness_text,
      fit_messages = r$fit_messages, seconds = x$seconds)
  }), fill = TRUE)
  # ---- multiplicity: local BH per family (the decision unit) and a global BH over the 48 (descriptive column)
  M <- rows[role == "DISCOVERY"]
  M[, p_for_multiplicity := ifelse(status == "OK" & is.finite(p_raw), p_raw, 1)]
  M[, `:=`(family_m = .N, q_local_bh = stats::p.adjust(p_for_multiplicity, method = "BH")), by = family]
  M[, `:=`(global_m = .N, q_global_bh_descriptive = stats::p.adjust(p_for_multiplicity, method = "BH"))]
  cl <- s30sc_classify(M$status, M$p_raw, M$q_local_bh, M$block, M$question, M$movadj_status, M$movadj_ci_includes_0, M$movadj_joint_p,
                       M$lobo_sign_stable, M$any_sign_change)
  M <- cbind(M, cl)
  # ---- sex-specific wording (registry: INT local q < 0.05 necessary; convention C7: row A/B and direction-consistent)
  okc <- function(h) { cp <- R[[h]]$primary$components
    if (!identical(R[[h]]$primary$row$status, "OK") || is.null(cp) || !nrow(cp) || !all(is.finite(cp$statistic))) NULL else cp }
  SS <- data.table::rbindlist(lapply(seq_len(nrow(M)), function(i) {
    h <- M$id[i]; na <- data.table::data.table(int_ok = NA, int_q = NA_real_, est_row = NA_real_, est_int = NA_real_, basis = NA_character_)
    if (!M$sex_analysis[i] %in% c("F", "M")) return(na)
    ih <- M[cell == M$cell[i] & sex_analysis == "INT"]
    int_ok <- nrow(ih) == 1L && identical(ih$status, "OK"); int_q <- if (int_ok) ih$q_local_bh else NA_real_
    if (M$question[i] == "L") {
      cp <- okc(h); ci <- if (int_ok) okc(ih$id) else NULL
      if (is.null(cp) || is.null(ci)) return(data.table::data.table(int_ok = int_ok, int_q = int_q, est_row = NA_real_, est_int = NA_real_, basis = "L components unavailable"))
      k <- cp$component[which.max(abs(cp$statistic))]; j <- match(mmm_ci_canon(paste0(k, ":sex_c")), mmm_ci_canon(ci$component))
      return(data.table::data.table(int_ok = int_ok, int_q = int_q, est_row = cp$estimate[cp$component == k][1], est_int = if (is.na(j)) NA_real_ else ci$estimate[j],
                                    basis = paste0(k, " vs ", if (is.na(j)) "no INT component" else ci$component[j])))
    }
    data.table::data.table(int_ok = int_ok, int_q = int_q, est_row = if (identical(M$status[i], "OK")) M$estimate[i] else NA_real_,
                           est_int = if (int_ok) ih$estimate else NA_real_, basis = paste(M$focal_terms[i], "vs", ih$focal_terms[1]))
  }))
  M[, `:=`(int_q_local_same_cell = SS$int_q, sex_specific_basis = SS$basis,
           sex_specific_direction_consistent = ifelse(sex_analysis %in% c("F", "M") & is.finite(SS$est_row) & is.finite(SS$est_int) & SS$est_row != 0 & SS$est_int != 0,
                                                      ifelse(sex_analysis == "F", sign(SS$est_int) == sign(SS$est_row), sign(SS$est_int) == -sign(SS$est_row)), NA),
           sex_specific_wording_allowed = s30sc_sex_specific_allowed(sex_analysis, classification, SS$est_row, SS$est_int, SS$int_ok, SS$int_q))]
  M[, wording_flags := vapply(seq_len(.N), function(i) s30sc_wording(spec[id == M$id[i]], M$reason_code[i], identical(M$low_df_label[i], "LOW_DF"),
                                                                     M$int_q_local_same_cell[i], M$sex_specific_wording_allowed[i]), "")]
  # ---- nested LOBO prediction: cookie hypotheses classified A (force_nested: DRY / test code exercise on all cookie hypotheses)
  nest_ids <- M[block == "cookie" & (classification == "A" | force_nested), id]
  NEST <- data.table::rbindlist(lapply(nest_ids, function(h) tryCatch(
    s30sc_nested_lobo_prediction(R[[h]]$d, spec[id == h], triggered = M[id == h, classification] == "A"),
    error = function(err) data.table::data.table(id = h, status = paste("FAILED:", conditionMessage(err))))), fill = TRUE)
  M[, nested_lobo_prediction := vapply(id, function(h) if (nrow(NEST) && h %in% NEST$id && "oob_R2_centred_pooled" %in% names(NEST))
    sprintf("%s: pooled out-of-batch R2 (centred) = %.4g over %d folds", if (M[id == h, classification] == "A") "run (class A)" else "code exercise (not class A)",
            NEST[id == h, oob_R2_centred_pooled][1], NEST[id == h, .N]) else if (M[id == h, block] == "cookie") "not run (not class A)" else "not applicable (not a cookie hypothesis)", "")]
  data.table::setorderv(M, "id"); M <- M[match(spec[role == "DISCOVERY", id], M$id)]
  P <- rows[role == "ESTIMATION_ONLY"]
  P[, `:=`(classification = "none (ESTIMATION_ONLY)", flag = "ESTIMATION_ONLY")]
  P[, c("p_raw", "ols_p_descriptive", "statistic", "statistic_type", "df1", "df2") := NULL]   # estimation only: no p and no test statistic
  P[, wording_flags := vapply(seq_len(.N), function(i) s30sc_wording(spec[id == P$id[i]], NA_character_, identical(P$low_df_label[i], "LOW_DF"), NA_real_), "")]
  COMP <- data.table::rbindlist(lapply(spec[question == "L", id], function(h) { x <- R[[h]]; cp <- x$primary$components; e <- spec[id == h]
    if (is.null(cp) || !nrow(cp)) return(data.table::data.table(id = h, role = e$role, family = e$family, sex_analysis = e$sex_analysis, measure = e$measure,
                                                              component = e$focal_terms[[1]], status = x$primary$row$status))
    cbind(data.table::data.table(id = h, role = e$role, family = e$family, sex_analysis = e$sex_analysis, measure = e$measure), cp[, .(component, estimate, se, df, statistic, ci_low, ci_high, status)],
          data.table::data.table(is_max_abs_t = seq_len(nrow(cp)) == which.max(abs(cp$statistic)), engine = "KR t (component; no p reported)")) }), fill = TRUE)
  if (nrow(COMP) && "statistic" %in% names(COMP)) COMP[role == "ESTIMATION_ONLY", statistic := NA_real_]   # POOL: estimation only
  list(master = M, pool = P, components = COMP,
       sensitivities = data.table::rbindlist(lapply(R, `[[`, "sens"), fill = TRUE),
       lobo = data.table::rbindlist(lapply(R, function(x) x$lobo$rows), fill = TRUE),
       influence = data.table::rbindlist(lapply(R, `[[`, "influence"), fill = TRUE),
       nested = NEST, fits = data.table::rbindlist(lapply(R, `[[`, "fits"), fill = TRUE),
       failures = if (length(failures)) data.table::rbindlist(failures) else data.table::data.table(block = character(), error = character()),
       results = R)
}

# ---------------------------------------------------------------- design gate (before any outcome column is joined)
#' Placeholder outcomes on the real design (never the real ones): CombZ ~ N(0,1) per animal, CombZ_wb its batch-centred
#' version, g_RS alternating +-1/2 within batch. CR2 df_Satt depends only on X and the clusters (registry design_gate),
#' so the placeholder CombZ does not affect the gated df.
s30sc_placeholder_outcomes <- function(D, seed) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  an <- unique(rbind(D$W[SIS == TRUE, .(AnimalNum, Batch)], D$K[SIS == TRUE, .(AnimalNum, Batch)]))
  data.table::setorder(an, Batch, AnimalNum)
  an[, `:=`(CombZ = stats::rnorm(.N), g_RS = rep(c(0.5, -0.5), length.out = .N)), by = Batch]
  an[, CombZ_wb := CombZ - mean(CombZ), by = Batch]
  an[]
}
s30sc_attach_outcomes <- function(D, an) {
  for (k in c("W", "K")) {
    x <- data.table::copy(D[[k]]); for (v in intersect(c("CombZ", "CombZ_wb", "g_RS"), names(x))) x[, (v) := NULL]
    D[[k]] <- merge(x, an[, .(AnimalNum, CombZ, CombZ_wb, g_RS)], by = "AnimalNum", all.x = TRUE, sort = FALSE)
  }
  D
}
#' Design gate: every entry's fixed design has its declared columns and full rank; every CR2 entry's Satterthwaite df
#' equals expected_df_cr2 within S30SC_DF_TOL. D must carry placeholder outcomes only.
s30sc_design_gate <- function(D, spec) {
  data.table::rbindlist(lapply(seq_len(nrow(spec)), function(i) {
    e <- spec[i]
    d <- tryCatch(s30sc_hyp_data(D, e), error = function(err) NULL)
    if (is.null(d)) return(data.table::data.table(id = e$id, role = e$role, engine = e$engine, passed = FALSE, detail = "data build failed"))
    di <- s30sc_design_info(d, e$formula, e$engine == "CR2")
    df <- NA_real_
    if (e$engine == "CR2") { fr <- s30sc_fit(d, e$formula, "CR2", paste(e$id, "design_gate", sep = "|"), e$focal_terms[[1]], expected_rank = e$expected_rank)
      df <- fr$row$df }
    rank_ok <- di$ncol == e$expected_rank && di$rank == di$ncol
    df_ok <- e$engine != "CR2" || isTRUE(abs(df - e$expected_df_cr2) <= S30SC_DF_TOL)
    data.table::data.table(id = e$id, role = e$role, engine = e$engine, n_obs = di$n_obs, n_animals = data.table::uniqueN(d$AnimalNum),
                           n_cages = data.table::uniqueN(d$CageEpisodeID), n_batches = data.table::uniqueN(d$Batch), n_fixed_cols = di$ncol, rank = di$rank,
                           expected_rank = e$expected_rank, rank_ok = rank_ok, design_df_cr2 = df, expected_df_cr2 = e$expected_df_cr2,
                           df_abs_diff = if (e$engine == "CR2") abs(df - e$expected_df_cr2) else NA_real_, df_ok = df_ok, passed = rank_ok && df_ok,
                           detail = if (e$engine == "CR2") "rank + CR2 Satterthwaite df (placeholder CombZ)" else "rank (placeholder label / CombZ_wb)")
  }), fill = TRUE)
}

# ---------------------------------------------------------------- DRY mode outcomes (never real)
#' Real RES/SUS labels of SIS animals permuted within Batch x Sex (fixed seed). Returns AnimalNum, g_RS (+1/2 RES, -1/2 SUS).
s30sc_permute_labels <- function(an, seed) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  x <- data.table::copy(data.table::as.data.table(an)); data.table::setorder(x, Batch, Sex, AnimalNum)
  x[, Group_perm := Group[sample.int(.N)], by = .(Batch, Sex)]
  x[, .(AnimalNum, Batch, Sex, g_RS = ifelse(Group_perm == "RES", 0.5, -0.5))]
}
#' Synthetic animal-level CombZ: batch shift + CC1-cage component (ICC 0.30) + noise, N(0,1)-scaled (fixed seed).
s30sc_synthetic_combz <- function(an, seed, batch_fx = c(B1 = -0.6, B2 = 0.4, B3 = -0.2, B4 = 0.7, B5 = 0.1, B6 = -0.4), icc = 0.30) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  x <- data.table::copy(data.table::as.data.table(an)); data.table::setorder(x, Batch, AnimalNum)
  uc <- sort(unique(x$cage_CC1)); u <- stats::rnorm(length(uc))
  x[, CombZ := unname(batch_fx[as.character(Batch)]) + sqrt(icc) * u[match(cage_CC1, uc)] + sqrt(1 - icc) * stats::rnorm(.N)]
  x[, .(AnimalNum, CombZ)]
}
# ---------------------------------------------------------------- labels and the REAL outcome join
#' Group from the canonical lists (rule of Analysis/build_later_outcome_combz.R:335-339): CON if on the con list, SUS if on the
#' sus list, otherwise RES.
s30sc_labels_from_lists <- function(an, sus, con) {
  x <- data.table::copy(data.table::as.data.table(an))
  x[, Group := ifelse(AnimalNum %in% con, "CON", ifelse(AnimalNum %in% sus, "SUS", "RES"))]
  x[]
}
#' Registered label counts: SIS_CC1 res_sus_counts_by_batch, COOKIE_SIS group_counts, COOKIE_SIS_F / _M by batch.
#' an: SIS animals with Batch, Group; ck: cookie SIS animals (AnimalNum, Batch, Sex). Returns gate rows.
s30sc_label_count_checks <- function(an, ck, pops) {
  cnt <- an[, .(RES = sum(Group == "RES"), SUS = sum(Group == "SUS")), by = Batch][order(Batch)]
  rb <- pops$SIS_CC1$res_sus_counts_by_batch
  exp1 <- data.table::rbindlist(lapply(setdiff(names(rb), c("total", "note")), function(b)
    data.table::data.table(Batch = b, RES = as.integer(sub("/.*$", "", rb[[b]])), SUS = as.integer(sub("^.*/", "", rb[[b]])))))[order(Batch)]
  g <- list(s30sc_gate("RES/SUS counts by batch = registry SIS_CC1 res_sus_counts_by_batch", isTRUE(all.equal(cnt, exp1, check.attributes = FALSE)),
                       paste(cnt[, paste0(Batch, " ", RES, "/", SUS)], collapse = "; ")),
            s30sc_gate("RES/SUS total = registry SIS_CC1 total", identical(sprintf("%d/%d", sum(cnt$RES), sum(cnt$SUS)), rb$total)))
  ckg <- merge(data.table::as.data.table(ck), an[, .(AnimalNum, Group)], by = "AnimalNum")
  for (p in c("COOKIE_SIS_F", "COOKIE_SIS_M")) {
    e <- s30sc_parse_group_counts(pops[[p]]$group_counts)[order(Batch)]
    o <- ckg[Sex == if (p == "COOKIE_SIS_F") "Female" else "Male", .(RES = sum(Group == "RES"), SUS = sum(Group == "SUS")), by = Batch][order(Batch)]
    g[[length(g) + 1L]] <- s30sc_gate(paste("RES/SUS counts by batch = registry", p, "group_counts"), isTRUE(all.equal(o, e, check.attributes = FALSE)))
  }
  g[[length(g) + 1L]] <- s30sc_gate("cookie SIS RES/SUS = registry COOKIE_SIS group_counts",
                                    identical(sprintf("RES %d, SUS %d", ckg[Group == "RES", .N], ckg[Group == "SUS", .N]), pops$COOKIE_SIS$group_counts) &&
                                      nrow(ckg) == nrow(ck))
  data.table::rbindlist(g)
}
#' Label pre-flight (section 5, BEFORE outcome contact; G2-03): the CombZ table's label columns (S30SC_COMBZ_LABEL_COLS:
#' AnimalNum [canonical], Sex, Batch, outcome_group -- never CombZ) against the canonical lists for every RFID animal.
#' an_all: AnimalNum, Batch, Sex, Group (CON / SUS / RES from the lists) for all RFID animals; lab: the label columns
#' (REAL: read from the CombZ table; DRY / tests: a synthetic table). Returns gate rows.
s30sc_label_table_checks <- function(an_all, lab) {
  lab <- data.table::as.data.table(lab)
  if (!setequal(names(lab), S30SC_COMBZ_LABEL_COLS)) stop("the label pre-flight selects exactly ", paste(S30SC_COMBZ_LABEL_COLS, collapse = ", "), " (never CombZ)", call. = FALSE)
  x <- merge(data.table::as.data.table(an_all), lab, by = "AnimalNum", all.x = TRUE, suffixes = c("", ".cz"))
  bcz <- ifelse(is.na(x$Batch.cz), NA_character_, ifelse(grepl("^B", as.character(x$Batch.cz)), as.character(x$Batch.cz), paste0("B", x$Batch.cz)))
  data.table::rbindlist(list(
    s30sc_gate("CombZ table (label columns): AnimalNum unique", !anyDuplicated(lab$AnimalNum), paste(nrow(lab), "rows")),
    s30sc_gate("CombZ table (label columns): every RFID animal present", nrow(x) == nrow(an_all) && !anyNA(x$outcome_group), paste(sum(!is.na(x$outcome_group)), "of", nrow(an_all))),
    s30sc_gate("CombZ table Sex and Batch agree with the RFID metadata", isTRUE(all(x$Sex == x$Sex.cz)) && isTRUE(all(x$Batch == bcz))),
    s30sc_gate("CombZ table outcome_group = canonical sus/con list label (every RFID animal)", isTRUE(all(x$outcome_group == x$Group)),
               paste(sum(x$outcome_group != x$Group, na.rm = TRUE), "disagreeing")),
    s30sc_gate("con-list animals are CON in the CombZ table", x[Group == "CON", .N] > 0L && isTRUE(all(x[Group == "CON", outcome_group] == "CON")))))
}
#' REAL outcome read (section 6): exactly AnimalNum + CombZ (S30SC_COMBZ_OUTCOME_COLS); complete and finite for every SIS animal.
#' Returns list(an = animals with CombZ, checks = gate rows).
s30sc_real_outcome <- function(an, cz) {
  cz <- data.table::as.data.table(cz)
  if (!setequal(names(cz), S30SC_COMBZ_OUTCOME_COLS)) stop("the outcome read selects exactly AnimalNum and CombZ", call. = FALSE)
  x <- merge(data.table::as.data.table(an)[, .(AnimalNum)], cz, by = "AnimalNum", all.x = TRUE)
  checks <- data.table::rbindlist(list(
    s30sc_gate("CombZ table: AnimalNum unique (outcome read)", !anyDuplicated(cz$AnimalNum)),
    s30sc_gate("CombZ present and finite for every SIS animal", nrow(x) == nrow(an) && isTRUE(all(is.finite(x$CombZ))), paste(sum(is.finite(x$CombZ)), "of", nrow(an), "animals"))))
  list(an = merge(data.table::as.data.table(an), cz, by = "AnimalNum"), checks = checks)
}

#' CombZ_wb (registry data.outcome.CombZ_wb): animal-level CombZ minus the mean over the analysis population's animals in
#' the same batch (one value per animal; identical for POOL, F and M because Sex is nested in Batch).
s30sc_combz_wb <- function(an) { x <- data.table::copy(data.table::as.data.table(an)); x[, CombZ_wb := CombZ - mean(CombZ), by = Batch]; x[] }

# ---------------------------------------------------------------- Stage 29 carry-over rows (never re-tested)
#' The Stage 29-determined metric x question combinations (multiplicity.never_in_a_family): CONT crossing_rate and
#' shared_zone_use (continuous_estimates.csv; both populations as exported), CAT all 5 metrics (estimates.csv Q1 /
#' RS_sexavg_CC1 / RS_by_sex_CC1; light phase from exposure_estimates.csv), with Stage 29 estimates, CIs and p as exported
#' (plus the Stage 29 Holm-adjusted p of the tested members). mask_values = TRUE (DRY): only the key columns of the Stage 29
#' tables are read (G2-14); every value column is NA (never read from disk).
S30SC_CARRY_VALUE_COLS <- c("estimate", "se", "df", "ci_low", "ci_high", "p_raw", "se_cr2", "df_cr2", "p_adjusted")
S30SC_CARRY_KEY_COLS <- list(estimates.csv = c("model_id", "estimand", "construct", "sex", "test"),
                             exposure_estimates.csv = c("model_id", "estimand", "construct", "sex", "test"),
                             continuous_estimates.csv = c("population", "predictor", "estimand", "n"),
                             multiplicity.csv = c("family_id", "member"))
s30sc_carry_over <- function(tables_dir, mask_values) {
  rd <- function(f) {
    p <- file.path(tables_dir, f)
    if (!isTRUE(mask_values)) return(data.table::fread(p))
    x <- data.table::fread(p, select = S30SC_CARRY_KEY_COLS[[f]])
    if (length(intersect(names(x), S30SC_CARRY_VALUE_COLS))) stop("DRY carry-over read a value column", call. = FALSE)
    for (k in S30SC_CARRY_VALUE_COLS) data.table::set(x, j = k, value = NA_real_)
    x
  }
  est <- rd("estimates.csv"); cont <- rd("continuous_estimates.csv"); expo <- rd("exposure_estimates.csv"); mult <- rd("multiplicity.csv")
  k4 <- c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation")
  map_sex <- function(estimand, sex) data.table::fcase(grepl("^Q1", estimand), "INT (Q1: (RES - SUS)_F - (RES - SUS)_M)",
                                                        grepl("^RS_sexavg", estimand), "sex-averaged (RES - SUS)",
                                                        grepl("^RS_by_sex", estimand) & sex == "Female", "F (RES - SUS in females)",
                                                        grepl("^RS_by_sex", estimand) & sex == "Male", "M (RES - SUS in males)", default = NA_character_)
  a <- est[construct %in% k4 & estimand %in% c("Q1", "RS_sexavg_CC1", "RS_by_sex_CC1")]
  a <- a[(estimand == "RS_by_sex_CC1" & model_id == paste0("CC1_BY_SEX|", construct, "|", sex)) | (estimand != "RS_by_sex_CC1" & model_id == paste0("CC1_POOLED|", construct))]
  b <- expo[construct == "light_phase_crossing_rate" & estimand %in% c("Q1_light", "RS_sexavg_CC1_light", "RS_by_sex_CC1_light")]
  cat_rows <- data.table::rbindlist(list(a[, .(source_table = "estimates.csv", model_id, estimand, construct, sex, estimate, se, df, ci_low, ci_high, p_raw, test)],
                                         b[, .(source_table = "exposure_estimates.csv", model_id, estimand, construct, sex, estimate, se, df, ci_low, ci_high, p_raw, test)]), fill = TRUE)
  cat_rows[, member := data.table::fcase(estimand == "Q1", paste0("Q1|", construct), estimand == "RS_by_sex_CC1", paste0("RS_by_sex_CC1|", construct, "|", sex), default = NA_character_)]
  cat_rows <- merge(cat_rows, mult[, .(member, stage29_family = family_id, stage29_p_adjusted_holm = p_adjusted)], by = "member", all.x = TRUE, sort = FALSE)
  cat_rows[, `:=`(question = "CAT", stage30_sex_analysis_equivalent = map_sex(estimand, sex))]
  cont_rows <- cont[predictor %in% c("crossing_rate", "shared_zone_use")]
  cont_rows <- cont_rows[, .(source_table = "continuous_estimates.csv", model_id = paste("CONTINUOUS", population, predictor, sep = "|"), estimand,
                             construct = predictor, population, estimate, se, df, ci_low, ci_high, se_cr2, df_cr2, n, p_raw, question = "CONT",
                             stage30_sex_analysis_equivalent = data.table::fcase(estimand == "slope_sexavg", "sex-averaged slope", estimand == "slope_DiD_F_minus_M", "INT (slope F - M)",
                                                                                 estimand == "slope_Female", "F (slope in females)", estimand == "slope_Male", "M (slope in males)", default = NA_character_))]
  out <- data.table::rbindlist(list(cont_rows, cat_rows), fill = TRUE)
  out[, `:=`(status = "CARRY_OVER", carry_over_rule = "Stage 29-determined combination: shown with its Stage 29 row as exported (release v1.0.1 / data version v2); never re-tested; no Stage 30 p; not in any Stage 30 family",
             values_masked = isTRUE(mask_values))]
  if (isTRUE(mask_values)) for (k in intersect(c("estimate", "se", "df", "ci_low", "ci_high", "p_raw", "se_cr2", "df_cr2", "stage29_p_adjusted_holm"), names(out)))
    data.table::set(out, j = k, value = NA_real_)
  data.table::setcolorder(out, c("question", "construct", "estimand", "stage30_sex_analysis_equivalent", "status"))
  out[]
}

#' Write one output table with the run-mode stamp as its first column.
s30sc_write <- function(x, dir, name, run_mode) {
  x <- data.table::copy(data.table::as.data.table(x))
  if ("run_mode" %in% names(x)) x[, run_mode := NULL]
  x[, run_mode := rep(run_mode, .N)]
  data.table::setcolorder(x, c("run_mode", setdiff(names(x), "run_mode")))
  for (k in names(x)) if (is.list(x[[k]])) data.table::set(x, j = k, value = vapply(x[[k]], function(v) paste(v, collapse = "; "), ""))
  data.table::fwrite(x, file.path(dir, name))
  invisible(file.path(dir, name))
}
#' Full-precision copy (G2-12: fwrite keeps 15 significant digits; KR refits are sensitive to the last digits).
s30sc_write_rds <- function(x, dir, name, run_mode) {
  x <- data.table::copy(data.table::as.data.table(x))
  if ("run_mode" %in% names(x)) x[, run_mode := NULL]
  x[, run_mode := rep(run_mode, .N)]
  data.table::setcolorder(x, c("run_mode", setdiff(names(x), "run_mode")))
  saveRDS(x, file.path(dir, name), version = 3L)
  invisible(file.path(dir, name))
}
