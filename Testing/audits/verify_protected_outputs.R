# Round-level verification that the protected outputs are unchanged (read-only apart from its own run folder).
# Every protected location of Functions/protected_outputs.R is checked against the records that exist for it; run once
# at the start and once at the end of a round, not per run (each isolated run has its own guard, mmm_protection_guard_*).
#
# Run from the repository root:
#   Rscript Testing/audits/verify_protected_outputs.R [--tier=1|2] [--run-id=<new id>] [--output-root=<absolute>]
#           (--baseline=<run folder of an earlier check> | --first-run)
# A check compares every protected file only against a baseline: run it with --first-run at the start of a round and
# with --baseline=<that run folder> at the end. Without either, baseline_diff is 'not_run' and the check FAILS, because
# the files without a self-manifest (tables/unchecked_files.csv) would go unchecked.
# Writes runs/<run_id>/ only (built in runs/.tmp_<run_id>/) under the output root, default <Behavior>/RANM/
# protected_output_checks; refused where mmm_isolated_output_root_check() or mmm_protection_assert_base() refuse it.
# The default run id is vpo_t<tier>_<time>_<commit>; it needs committed code. Ids starting with "dev" may run on
# uncommitted code.
# Tier 1 (default): existence and sizes against the self-manifests, plus the sha256 of the small manifests that the
# chain checks compare and of every repository pin; seconds to minutes. Tier 2: also the sha256 of every listed file and
# of every protected file except the raw antenna logs (about 5 GB over the network; ask before running it on this host).
# Checks (VPO_CHECKS names the record each is compared with):
#   frozen_pins            the 118 files the registered runs pinned (mmm_frozen_guard_pins())
#   stage29_v101_release   the registered Stage 29 v1.0.1 release against its audit/output_manifest.csv
#   registered_runs        Stage 29 v1.0.0, 29b, 29c, 30, 32 v1.1 and the Stage 33 post hoc runs, the same way
#   stage19_record         the 38 live Stage 19 outputs and their 19 publication_ready copies (VPO_STAGE19_COPY_FILES; a
#                          missing copy fails) against the Stage 19 record
#   stage31_runs           the seven Stage 31/31b/31c/31d run folders against their manifests; no other folder
#   stage31_censored       the censored Stage 31 run(s) in RANM/cc4_grid_exposure_censored
#   ranm_runs              every published RANM run against manifest/output_files_sha256.csv (no unlisted file); every run
#                          of VPO_RANM_RUNS must exist
#   stage00b_ledger_chain  each Stage 00b ledger: sha256 of runs/<id>/manifest/output_files_sha256.csv equals the
#                          ledger's output_manifest_sha256, then the run's files; ledger rows = run folders; with
#                          --baseline, the ledger still starts with the bytes it had then
#   canonical_bundles      sha256 of each bundle's 00_manifest.csv equals its BUNDLE_REGISTRY.csv row, then each file
#   manuscript_imports     the bundle copies in Exp9_manuscript/source_data against provenance/source_manifests/
#   recorded_inputs        the protected inputs (raw logs, tag maps, data versions) recorded by the registered pin lists
#                          and by the srs release's manifest/input_hashes.csv
#   repo_pins              SXM_RELEASED, the S13 digest and the behavior_analysis_config.R byte pin (read from the tests
#                          that hold them), the calibration criteria and schedule pins
#   baseline_diff          (--baseline) this run's snapshot against the earlier one: only new RANM run folders may appear;
#                          (--first-run) recorded as the round's first snapshot; (neither) not_run, a failure
# Outputs: tables/summary.csv (PASS/FAIL per check and overall), tables/check_<name>.csv (one row per file),
# tables/ledger_states.csv, tables/registry.csv, snapshot/baseline_snapshot.csv (size, time stamp and, where taken,
# sha256 of every protected file: the baseline of a later --baseline run) and manifest/. Exit status 1 when a check fails.

vpo_args <- function(args) {
  known <- "^--(tier|run-id|output-root|baseline)=|^--first-run$"
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  tier <- val("tier")
  if (is.na(tier)) tier <- "1"
  if (!tier %in% c("1", "2")) stop("--tier must be 1 or 2.", call. = FALSE)
  first_run <- "--first-run" %in% args
  if (first_run && !is.na(val("baseline"))) stop("--first-run and --baseline exclude each other.", call. = FALSE)
  list(tier = as.integer(tier), run_id = val("run-id"), output_root = val("output-root"), baseline = val("baseline"), first_run = first_run)
}

VPO_CODE <- c("Testing/audits/verify_protected_outputs.R", "Functions/protected_outputs.R", "Functions/qc_run_protection.R",
              "Functions/frozen_input_guard.R", "Functions/project_paths.R", "Functions/spatial_exclusion_manifest.R",
              "Functions/behavioral_dynamics_helpers.R", "Analysis/_pipeline_setup.R")
# The record each check compares with (the summary's baseline_source).
VPO_CHECKS <- c(
  frozen_pins = "pin lists of the registered runs (audit/run_inputs.csv, audit/input_hashes.csv) and docs/frozen_output_pins/",
  stage29_v101_release = "the release's audit/output_manifest.csv (tables/ listed by base name; audit/ not listed)",
  registered_runs = "each run's audit/output_manifest.csv",
  stage19_record = "docs/behavior_output_producer_reruns/stage19_20261012.csv overlaid by stage19_20261012b.csv (sha256 only)",
  stage31_runs = "manifest.csv (31b-31d) or audit/output_manifest.csv (31) of each run folder (sha256 only)",
  stage31_censored = "audit/output_manifest.csv of each censored run (sha256 only)",
  ranm_runs = "each run's manifest/output_files_sha256.csv",
  stage00b_ledger_chain = "runs_ledger.csv (output_manifest_sha256), then each run's manifest/output_files_sha256.csv",
  canonical_bundles = "BUNDLE_REGISTRY.csv (manifest_sha256), then each bundle's 00_manifest.csv",
  manuscript_imports = "Exp9_manuscript/provenance/source_manifests/{behaviour,figure_support,stage30,cc4_candidate}_bundle(s)_manifest.csv",
  recorded_inputs = "input rows of the registered pin lists and of srs_v1.1_9b18624/manifest/input_hashes.csv",
  repo_pins = "SXM_RELEASED; S13 digest and config byte pin in Testing/tests; ACP_*_SHA256 in Functions/antenna_calibration_pilot.R",
  baseline_diff = "snapshot/baseline_snapshot.csv of the --baseline run")
# Row statuses that pass; every other status fails its check.
VPO_OK <- c("ok", "partial_run", "baseline_only", "recorded", "prefix_ok", "expected_addition", "expected_removal", "identical_bytes",
            "first_run")
VPO_EXPECTED_FROZEN_PINS <- 118L
VPO_STAGE29_V101 <- "analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507"
# Present in a Stage 29 release folder without being in its output manifest: the audit files, and the archive note and
# receipt added when v1.0.0 was superseded (2026-09-29). They are covered by the baseline snapshot.
VPO_RELEASE_UNLISTED <- c("^audit/", "^ARCHIVE_README[.]txt$", "^ARCHIVE_RECEIPT[.]csv$")
VPO_REGISTERED_RUNS <- c("analysis_ready/pipeline/29_canonical_behavior_releases/v100_dv1_0555c90",
                         "analysis_ready/pipeline/29b_posthoc_con_contrasts/v1.0_7f1da1f",
                         "analysis_ready/pipeline/29c_posthoc_movement_combz_fit/v1.0_fd61cd9",
                         "analysis_ready/pipeline/30_exploratory_screen/v1.0_be71e2f",
                         "analysis_ready/pipeline/32_behavior_exposure_adaptation/v1.1_f4c7a31",
                         "analysis_ready/analyses/posthoc_cohort_followups/v1.0_ABCDE_6b50c1f",
                         "analysis_ready/analyses/posthoc_cohort_followups/v1.0_D_fb52ad9")
VPO_STAGE19_RECORDS <- c("docs/behavior_output_producer_reruns/stage19_20261012.csv",
                         "docs/behavior_output_producer_reruns/stage19_20261012b.csv")
VPO_STAGE19_ROOT <- "analyses/spatial_occupancy"   # relative to analysis_ready, as the record's target_root_rel
VPO_STAGE19_N <- 38L
VPO_STAGE19_COPIES <- c("publication_ready/tables/spatial_occupancy", "publication_ready/figures/single_panels/spatial_occupancy")
# The publication copies present on 2026-10-14 (8 tables, 11 figures); an absent copy is 'missing'.
VPO_STAGE19_COPY_FILES <- list(
  "publication_ready/tables/spatial_occupancy" = c(
    "all_position_occupancy_by_bin.csv", "all_position_occupancy_by_phase.csv", "column_occupancy_group_contrasts_effect_sizes.csv",
    "group_level_column_occupancy_summary.csv", "group_level_reader_occupancy_summary.csv",
    "longitudinal_full_phase_reader_occupancy_emmeans_group_contrasts_by_position.csv",
    "primary_cc1_first_active_reader_occupancy_emmeans_group_contrasts_by_position.csv", "reader_occupancy_group_contrasts_effect_sizes.csv"),
  "publication_ready/figures/single_panels/spatial_occupancy" = c(
    "longitudinal_reader_occupancy_cage_maps_active.svg", "longitudinal_reader_occupancy_cage_maps_inactive.svg",
    "longitudinal_reader_occupancy_effect_size_heatmap.svg", "longitudinal_reader_occupancy_position_by_cagechange.svg",
    "primary_cc1_first_active_reader_occupancy_by_animal_female.svg", "primary_cc1_first_active_reader_occupancy_by_animal_male.svg",
    "primary_cc1_first_active_reader_occupancy_by_sex_group.svg", "primary_cc1_first_active_reader_occupancy_difference_maps.svg",
    "primary_cc1_first_active_reader_occupancy_effect_size_maps.svg", "qc_occupancy_source_comparison_cc1_first_active.svg",
    "qc_reader_fraction_by_batch_cagechange_system.svg"))
VPO_STAGE31_ROOT <- "analysis_ready/analyses/cc4_grid_exposure"
VPO_STAGE31_RUNS <- c(whole_inactive_20260929_v1 = "audit/output_manifest.csv", timing_review_20260929_v1 = "audit/output_manifest.csv",
                      phase_groups_20260929_v1 = "manifest.csv", phase_groups_20260929_v2 = "manifest.csv",
                      phase_statistics_20260929_v1 = "manifest.csv", phase_phenotypes_20260929_v1 = "manifest.csv",
                      phase_phenotypes_validation_20260929_v1 = "manifest.csv")
VPO_STAGE31_CENSORED <- c(tool = "cc4_grid_exposure_censored", run = "whole_inactive_sx04v11_20261014")
VPO_RUN_MANIFEST <- file.path("manifest", "output_files_sha256.csv")
# Stage 00b lists no rec/ file in its manifest, and its publish step writes two files after the manifest.
VPO_RAQC_UNLISTED <- c("^rec/", "^manifest/protection_check[.]csv$", "^manifest/protection_snapshot_after[.]csv$")
VPO_QC_ROOT <- "analysis_ready/quality_control/raw_acquisition"
VPO_BUNDLE_ROOTS <- c("analysis_ready/canonical/behavior_bundle", "analysis_ready/canonical/figure_support_bundle",
                      "analysis_ready/canonical/stage30_figure_bundle")
VPO_BUNDLE_ROOT_LOGS <- "logs"   # the bundle builders' gate results beside the bundles: baseline snapshot only
# Manuscript import manifests (provenance/source_manifests/) and the folder their bundle_id/file rows lie in.
VPO_MANUSCRIPT_IMPORTS <- c(behaviour_bundle_manifest.csv = "source_data/MMMSociability",
                            figure_support_bundle_manifest.csv = "source_data/MMMSociability",
                            stage30_bundle_manifest.csv = "source_data/MMMSociability",
                            cc4_candidate_bundles_manifest.csv = "source_data/MMMSociability_candidates")
VPO_SRS_RELEASE <- "single_read_spatial/runs/srs_v1.1_9b18624"   # relative to RANM
# Published RANM runs (<tool>/<run id>) present on 2026-10-14; each must still exist. Later runs are covered by --baseline.
VPO_RANM_RUNS <- c("board_adjustment/badj_v1_79ec773", "board_adjustment/badj_v1_9b18624", "board_identifiability/idf_v1_6165adf",
                   "board_identifiability/idf_v1_622f5cb", "cage_episode_presence/cep_v1_25804cb", "cage_episode_presence/cep_v1_9b18624",
                   "raw_acquisition_qc/raqc_20261013_9aa84d9", "raw_acquisition_qc/raqc_20261013_b1783bf",
                   "single_read_spatial/srs_v1.0_6165adf", "single_read_spatial/srs_v1.1_25804cb", "single_read_spatial/srs_v1.1_9b18624")
# Pins held by the portable tests: the file and a regular expression whose group 1 is the pinned sha256.
VPO_TEST_PINS <- list(
  s13 = c(file = "Testing/tests/test_spatial_exclusion_manifest.R", regex = "digest::digest\\(s13.*\"([0-9a-f]{64})\""),
  config = c(file = "Testing/tests/test_frozen_code_identity.R",
             regex = "\"Functions/behavior_analysis_config[.]R\"\\s*=\\s*\"([0-9a-f]{64})\""))
VPO_ACP_FILE <- "Functions/antenna_calibration_pilot.R"
VPO_TIER2_SIZE_ONLY <- "input_raw_logs"   # registry classes compared by size and time stamp in tier 2 too
VPO_LONGEST_RELATIVE <- c(file.path("tables", "check_stage00b_ledger_chain.csv"), file.path("snapshot", "baseline_snapshot.csv"),
                          file.path("manifest", "output_files_sha256.csv"))

vpo_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

# sha256 of files, each read once per run (NA for a missing file).
vpo_hasher <- function() {
  cache <- new.env(parent = emptyenv())
  function(paths) vapply(paths, function(f) {
    k <- tolower(gsub("\\\\", "/", f))
    if (!is.null(cache[[k]])) return(cache[[k]])
    h <- if (!is.na(f) && file.exists(f) && !dir.exists(f)) digest::digest(file = f, algo = "sha256") else NA_character_
    assign(k, h, envir = cache)
    h
  }, "", USE.NAMES = FALSE)
}

vpo_rows <- function(check, unit, file, path, status, expected_bytes = NA_real_, bytes = NA_real_, expected_sha256 = NA_character_,
                     sha256 = NA_character_, note = "") {
  if (!length(path)) return(NULL)
  data.table::data.table(check = check, unit = unit, file = file, path = path, status = status,
                         expected_bytes = suppressWarnings(as.numeric(expected_bytes)), bytes = as.numeric(bytes),
                         expected_sha256 = tolower(as.character(expected_sha256)), sha256 = as.character(sha256), note = note)
}

# Files against their recorded bytes (when recorded) and, in tier 2, their recorded sha256.
vpo_compare <- function(check, unit, file, path, expected_bytes, expected_sha256, tier, hasher, note = "") {
  if (!length(path)) return(NULL)
  n <- length(path)
  ex <- file.exists(path) & !dir.exists(path)
  bytes <- ifelse(ex, file.size(path), NA_real_)
  eb <- rep_len(suppressWarnings(as.numeric(expected_bytes)), n)
  es <- rep_len(tolower(as.character(expected_sha256)), n)
  sha <- if (tier >= 2L) hasher(path) else rep(NA_character_, n)
  status <- ifelse(!ex, "missing", ifelse(!is.na(eb) & bytes != eb, "size_mismatch",
                   ifelse(tier >= 2L & !is.na(es) & nzchar(es) & (is.na(sha) | sha != es), "sha_mismatch", "ok")))
  vpo_rows(check, unit, file, path, status, eb, bytes, es, sha, note)
}

# One run folder against its own manifest: every listed file (bytes when recorded; sha256 in tier 2), and every present
# file that is not listed. subdir: the listed names are relative to it (the Stage 29 releases list tables/ by base name);
# allow: patterns of relative paths that may be present without being listed (files the producer writes later).
vpo_manifest_check <- function(check, run_dir, manifest_rel, tier, hasher, subdir = "", allow = character(0), unit = basename(run_dir)) {
  mf <- file.path(run_dir, manifest_rel)
  if (!file.exists(mf)) return(vpo_rows(check, unit, manifest_rel, mf, "no_manifest", note = "self-manifest missing"))
  m <- data.table::fread(mf, colClasses = "character", showProgress = FALSE)
  col <- function(x) { hit <- names(m)[tolower(names(m)) == x]; if (length(hit)) m[[hit[1]]] else rep(NA_character_, nrow(m)) }
  listed <- gsub("\\\\", "/", col("file"))
  if (!nrow(m) || anyNA(listed)) return(vpo_rows(check, unit, manifest_rel, mf, "no_manifest", note = "manifest without file rows"))
  rel <- if (nzchar(subdir)) file.path(subdir, listed) else listed
  out <- vpo_compare(check, unit, rel, file.path(run_dir, rel), col("bytes"), col("sha256"), tier, hasher)
  present <- list.files(run_dir, recursive = TRUE, all.files = TRUE)
  extra <- present[!tolower(present) %in% tolower(c(rel, manifest_rel)) & !basename(present) %in% MMM_PROTECTION_IGNORED_NAMES]
  for (a in allow) extra <- extra[!grepl(a, extra)]
  data.table::rbindlist(list(out, vpo_rows(check, unit, extra, file.path(run_dir, extra), "unlisted", note = "present, not in the manifest")),
                        fill = TRUE)
}

vpo_check_frozen_pins <- function(project_root, repo_root, tier, hasher) {
  pins <- tryCatch(mmm_frozen_guard_pins(project_root), error = function(e) e)
  if (inherits(pins, "error")) return(vpo_rows("frozen_pins", "all", "", project_root, "pin_lists_unreadable", note = conditionMessage(pins)))
  if (is.null(pins)) return(vpo_rows("frozen_pins", "all", "", project_root, "no_manifest", note = "no pin applies: not the recorded project root"))
  lists <- c(file.path(project_root, MMM_FROZEN_PIN_LISTS), file.path(repo_root, MMM_REPO_PIN_LISTS))
  b <- data.table::rbindlist(lapply(lists[file.exists(lists)], function(l) {
    x <- data.table::fread(l, colClasses = "character", showProgress = FALSE)
    if (!all(c("input", "bytes") %in% names(x))) return(NULL)
    p <- gsub("\\\\", "/", x$input)
    at <- regexpr("/analysis_ready/", tolower(p), fixed = TRUE)
    data.table::data.table(rel_key = tolower(substring(p, at + 1L)), bytes = x$bytes)[at > 0L]
  }))
  out <- vpo_compare("frozen_pins", pins$pin_lists, pins$rel, pins$path, b$bytes[match(tolower(pins$rel), b$rel_key)], pins$sha256, tier, hasher)
  if (nrow(pins) != VPO_EXPECTED_FROZEN_PINS) {
    out <- data.table::rbindlist(list(out, vpo_rows("frozen_pins", "all", "", project_root, "count_mismatch",
                                                    note = sprintf("%d pins, expected %d", nrow(pins), VPO_EXPECTED_FROZEN_PINS))), fill = TRUE)
  }
  out
}

vpo_check_registered_runs <- function(project_root, tier, hasher) {
  one <- function(ck, d) vpo_manifest_check(ck, file.path(project_root, d), file.path("audit", "output_manifest.csv"), tier, hasher,
                                            subdir = if (grepl("/29_canonical_behavior_releases/", d)) "tables" else "",
                                            allow = if (grepl("/29_canonical_behavior_releases/", d)) VPO_RELEASE_UNLISTED else character(0),
                                            unit = sub("^analysis_ready/(pipeline|analyses)/", "", d))
  list(stage29_v101_release = one("stage29_v101_release", VPO_STAGE29_V101),
       registered_runs = data.table::rbindlist(lapply(VPO_REGISTERED_RUNS, function(d) one("registered_runs", d)), fill = TRUE))
}

# The record: the first file's target files and sha256, each later file replacing the rows it lists (its prior_sha256
# must equal the sha256 it replaces). Rows with an empty sha256 are files the record expects to be absent.
vpo_stage19_record <- function(record_files) {
  rd <- function(f) { x <- data.table::fread(f, colClasses = "character", showProgress = FALSE); x[, rel := file.path(target_root_rel, target_file)]; x[] }
  rec <- rd(record_files[1])[, .(rel, target_file, sha256)]
  chain <- NULL
  for (f in record_files[-1]) {
    o <- rd(f)
    prev <- rec$sha256[match(o$rel, rec$rel)]
    nz <- function(x) ifelse(is.na(x), "", x)
    chain <- data.table::rbindlist(list(chain, data.table::data.table(record = basename(f), rel = o$rel, prior_sha256 = o$prior_sha256, replaced_sha256 = prev,
                                                                      ok = nz(o$prior_sha256) == nz(prev))))
    rec <- rbind(rec[!rel %in% o$rel], o[, .(rel, target_file, sha256)])
  }
  list(record = rec[!is.na(sha256) & nzchar(sha256)], chain = chain)
}

vpo_check_stage19 <- function(project_root, repo_root, tier, hasher) {
  ck <- "stage19_record"
  recf <- file.path(repo_root, VPO_STAGE19_RECORDS)
  if (!all(file.exists(recf))) return(vpo_rows(ck, "record", basename(recf), recf, ifelse(file.exists(recf), "ok", "no_manifest")))
  r <- vpo_stage19_record(recf)
  rec <- r$record
  ar <- file.path(project_root, "analysis_ready")
  out <- list(vpo_rows(ck, r$chain$record, r$chain$rel, file.path(ar, r$chain$rel), ifelse(r$chain$ok, "ok", "chain_mismatch"),
                       expected_sha256 = r$chain$replaced_sha256, sha256 = r$chain$prior_sha256, note = "overlay prior_sha256 = replaced sha256"),
              vpo_compare(ck, "analysis_ready", rec$rel, file.path(ar, rec$rel), NA_real_, rec$sha256, tier, hasher))
  root <- file.path(ar, VPO_STAGE19_ROOT)
  present <- if (dir.exists(root)) file.path(VPO_STAGE19_ROOT, list.files(root, recursive = TRUE, all.files = TRUE)) else character(0)
  extra <- present[!tolower(present) %in% tolower(rec$rel) & !basename(present) %in% MMM_PROTECTION_IGNORED_NAMES]
  out <- c(out, list(vpo_rows(ck, "analysis_ready", extra, file.path(ar, extra), "unlisted", note = "not in the Stage 19 record")))
  if (nrow(rec) != VPO_STAGE19_N) out <- c(out, list(vpo_rows(ck, "record", "", recf[1], "count_mismatch",
                                                              note = sprintf("%d recorded files, expected %d", nrow(rec), VPO_STAGE19_N))))
  # publication copies: same name as a recorded file, same size as the live file (tier 1), the recorded sha256 (tier 2)
  for (d in VPO_STAGE19_COPIES) {
    fs <- list.files(file.path(project_root, d), all.files = TRUE, no.. = TRUE)
    fs <- union(fs[!fs %in% MMM_PROTECTION_IGNORED_NAMES], VPO_STAGE19_COPY_FILES[[d]])   # an absent pinned copy -> missing
    idx <- match(fs, rec$target_file)
    cmp <- vpo_compare(ck, d, fs, file.path(project_root, d, fs), file.size(file.path(ar, rec$rel[idx])), rec$sha256[idx], tier, hasher,
                       note = "copy of the recorded live file")
    if (!is.null(cmp)) cmp[is.na(idx), `:=`(status = "unrecorded", note = "no recorded Stage 19 file of this name")]
    out <- c(out, list(cmp))
  }
  data.table::rbindlist(out, fill = TRUE)
}

vpo_check_stage31 <- function(project_root, tier, hasher) {
  root <- file.path(project_root, VPO_STAGE31_ROOT)
  out <- lapply(names(VPO_STAGE31_RUNS), function(r) vpo_manifest_check("stage31_runs", file.path(root, r), VPO_STAGE31_RUNS[[r]], tier, hasher))
  other <- setdiff(list.files(root, all.files = TRUE, no.. = TRUE), c(names(VPO_STAGE31_RUNS), MMM_PROTECTION_IGNORED_NAMES))
  data.table::rbindlist(c(out, list(vpo_rows("stage31_runs", other, other, file.path(root, other), "unexpected_folder",
                                             note = "not one of the seven Stage 31-31d run folders"))), fill = TRUE)
}

vpo_check_censored <- function(ranm, tier, hasher) {
  base <- file.path(ranm, VPO_STAGE31_CENSORED[["tool"]])
  dirs <- list.dirs(base, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "runs"]   # runs/<id> folders are checked as RANM runs
  out <- lapply(dirs, function(d) vpo_manifest_check("stage31_censored", d, file.path("audit", "output_manifest.csv"), tier, hasher))
  if (!VPO_STAGE31_CENSORED[["run"]] %in% basename(dirs)) {
    out <- c(out, list(vpo_rows("stage31_censored", VPO_STAGE31_CENSORED[["run"]], "", file.path(base, VPO_STAGE31_CENSORED[["run"]]), "missing")))
  }
  data.table::rbindlist(out, fill = TRUE)
}

# Every RANM run folder except the current run's. Folders without a self-manifest are covered by the snapshot only.
vpo_check_ranm_runs <- function(ranm, tier, hasher, own = character(0)) {
  out <- list()
  top <- list.files(ranm, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  for (t in top) {
    runs <- file.path(t, "runs")
    if (!dir.exists(t) || !dir.exists(runs)) {
      if (basename(t) != VPO_STAGE31_CENSORED[["tool"]])
        out <- c(out, list(vpo_rows("ranm_runs", basename(t), "", t, "baseline_only", note = "no runs/ folder: covered by the baseline snapshot only")))
      next
    }
    for (r in list.dirs(runs, recursive = FALSE)) {
      if (.mmm_po_key(r) %in% .mmm_po_key(own)) next
      unit <- paste(basename(t), basename(r), sep = "/")
      if (startsWith(basename(r), ".tmp_")) {
        out <- c(out, list(vpo_rows("ranm_runs", unit, "", r, "partial_run", note = "unpublished run folder (in progress or failed)")))
        next
      }
      raqc <- basename(t) == "raw_acquisition_qc" || startsWith(basename(r), "raqc_")
      out <- c(out, list(vpo_manifest_check("ranm_runs", r, VPO_RUN_MANIFEST, tier, hasher, allow = if (raqc) VPO_RAQC_UNLISTED else character(0),
                                            unit = unit)))
    }
    loose <- setdiff(list.files(t, all.files = TRUE, no.. = TRUE), c("runs", MMM_PROTECTION_IGNORED_NAMES))
    out <- c(out, list(vpo_rows("ranm_runs", basename(t), loose, file.path(t, loose), "baseline_only", note = "outside runs/: baseline snapshot only")))
  }
  gone <- VPO_RANM_RUNS[!dir.exists(file.path(ranm, dirname(VPO_RANM_RUNS), "runs", basename(VPO_RANM_RUNS)))]
  out <- c(out, list(vpo_rows("ranm_runs", gone, "", file.path(ranm, dirname(gone), "runs", basename(gone)), "missing",
                              note = "a published run of VPO_RANM_RUNS is gone")))
  data.table::rbindlist(out, fill = TRUE)
}

# Each ledger root: the chain from the ledger to every run's manifest, then the run's files; ledger rows = run folders.
# Returns list(rows, states); states (ledger path, bytes, sha256) are the reference of a later prefix check.
vpo_check_ledgers <- function(roots, tier, hasher) {
  ck <- "stage00b_ledger_chain"
  out <- list(); states <- list(data.table::data.table(ledger = character(0), bytes = numeric(0), sha256 = character(0), rows = integer(0)))
  for (b in roots) {
    L <- tryCatch(mmm_qc_ledger_read(b), error = function(e) e)
    if (inherits(L, "error")) {
      out <- c(out, list(vpo_rows(ck, basename(b), "runs_ledger.csv", mmm_qc_ledger_path(b), "ledger_unreadable", note = conditionMessage(L))))
      next
    }
    st <- mmm_qc_ledger_state(b)
    states <- c(states, list(data.table::data.table(ledger = mmm_qc_ledger_path(b), bytes = st$bytes, sha256 = st$sha256, rows = nrow(L))))
    dup <- L$run_id[duplicated(L$run_id)]
    out <- c(out, list(vpo_rows(ck, dup, rep("runs_ledger.csv", length(dup)), rep(mmm_qc_ledger_path(b), length(dup)), "duplicate_ledger_id")))
    for (i in seq_len(nrow(L))) {
      d <- file.path(b, "runs", L$run_id[i]); mf <- file.path(d, VPO_RUN_MANIFEST); h <- hasher(mf)
      status <- if (is.na(h)) "missing" else if (identical(h, tolower(L$output_manifest_sha256[i]))) "ok" else "chain_mismatch"
      out <- c(out, list(vpo_rows(ck, L$run_id[i], VPO_RUN_MANIFEST, mf, status, expected_sha256 = L$output_manifest_sha256[i], sha256 = h,
                                  note = "ledger output_manifest_sha256")))
      if (dir.exists(d)) out <- c(out, list(vpo_manifest_check(ck, d, VPO_RUN_MANIFEST, tier, hasher, allow = VPO_RAQC_UNLISTED, unit = L$run_id[i])))
    }
    dirs <- basename(list.dirs(file.path(b, "runs"), recursive = FALSE))
    tmp <- dirs[startsWith(dirs, ".tmp_")]
    un <- setdiff(dirs[!startsWith(dirs, ".tmp_")], L$run_id)
    out <- c(out, list(vpo_rows(ck, un, "", file.path(b, "runs", un), "unledgered", note = "run folder without a ledger line"),
                       vpo_rows(ck, tmp, "", file.path(b, "runs", tmp), "partial_run", note = "unpublished run folder")))
  }
  list(rows = data.table::rbindlist(out, fill = TRUE), states = data.table::rbindlist(states))
}

# With an earlier run's ledger states: each ledger must still start with the bytes it had (lines appended since pass).
vpo_ledger_prefix <- function(states) {
  if (is.null(states) || !nrow(states)) return(NULL)
  ok <- vapply(seq_len(nrow(states)), function(i) mmm_qc_ledger_prefix_ok(dirname(states$ledger[i]),
                                                                          list(bytes = as.numeric(states$bytes[i]), sha256 = states$sha256[i])), logical(1))
  vpo_rows("stage00b_ledger_chain", "prefix", basename(states$ledger), states$ledger, ifelse(ok, "prefix_ok", "prefix_changed"),
           expected_bytes = states$bytes, bytes = file.size(states$ledger), expected_sha256 = states$sha256,
           note = "ledger starts with the bytes of the baseline run")
}

vpo_check_bundles <- function(project_root, tier, hasher) {
  ck <- "canonical_bundles"
  out <- list()
  for (r in file.path(project_root, VPO_BUNDLE_ROOTS)) {
    regf <- file.path(r, "BUNDLE_REGISTRY.csv")
    if (!file.exists(regf)) { out <- c(out, list(vpo_rows(ck, basename(r), "BUNDLE_REGISTRY.csv", regf, "no_manifest"))); next }
    reg <- data.table::fread(regf, colClasses = "character", showProgress = FALSE)
    for (i in seq_len(nrow(reg))) {
      d <- file.path(r, reg$bundle_id[i]); mf <- file.path(d, "00_manifest.csv"); h <- hasher(mf)
      status <- if (is.na(h)) "missing" else if (identical(h, tolower(reg$manifest_sha256[i]))) "ok" else "chain_mismatch"
      out <- c(out, list(vpo_rows(ck, reg$bundle_id[i], "00_manifest.csv", mf, status, expected_sha256 = reg$manifest_sha256[i], sha256 = h,
                                  note = "BUNDLE_REGISTRY manifest_sha256")))
      if (!is.na(h)) out <- c(out, list(vpo_manifest_check(ck, d, "00_manifest.csv", tier, hasher, unit = reg$bundle_id[i])))
    }
    other <- setdiff(list.files(r, all.files = TRUE, no.. = TRUE), c("BUNDLE_REGISTRY.csv", reg$bundle_id, MMM_PROTECTION_IGNORED_NAMES))
    out <- c(out, list(vpo_rows(ck, basename(r), other, file.path(r, other), ifelse(other %in% VPO_BUNDLE_ROOT_LOGS, "baseline_only", "unregistered"),
                                note = ifelse(other %in% VPO_BUNDLE_ROOT_LOGS, "build logs: baseline snapshot only", "not a registered bundle"))))
  }
  data.table::rbindlist(out, fill = TRUE)
}

vpo_check_manuscript <- function(manuscript, tier, hasher) {
  out <- lapply(names(VPO_MANUSCRIPT_IMPORTS), function(f) {
    mf <- file.path(manuscript, "provenance", "source_manifests", f)
    if (!file.exists(mf)) return(vpo_rows("manuscript_imports", f, f, mf, "no_manifest"))
    m <- data.table::fread(mf, colClasses = "character", showProgress = FALSE)
    rel <- file.path(VPO_MANUSCRIPT_IMPORTS[[f]], m$bundle_id, m$file)
    vpo_compare("manuscript_imports", f, rel, file.path(manuscript, rel), m$bytes, m$sha256, tier, hasher)
  })
  data.table::rbindlist(out, fill = TRUE)
}

# Inputs recorded by the registered pin lists and the srs release that lie in a protected input location.
vpo_check_recorded_inputs <- function(project_root, registry, tier, hasher) {
  ck <- "recorded_inputs"
  lists <- c(file.path(project_root, MMM_FROZEN_PIN_LISTS), file.path(dirname(project_root), "RANM", VPO_SRS_RELEASE, "manifest", "input_hashes.csv"))
  rows <- data.table::rbindlist(lapply(lists[file.exists(lists)], function(l) {
    x <- data.table::fread(l, colClasses = "character", showProgress = FALSE)
    data.table::data.table(source = l, input = gsub("\\\\", "/", x$input), bytes = x$bytes, sha256 = tolower(x$sha256))
  }))
  out <- list(vpo_rows(ck, "pin list", basename(lists[!file.exists(lists)]), lists[!file.exists(lists)], "no_manifest"))
  if (nrow(rows)) {
    inputs <- .mmm_po_key(registry$location[registry$class %in% c("input", "input_raw_logs")])
    rows <- rows[.mmm_po_under(.mmm_po_key(rows$input), inputs)]
    rows[, input_key := tolower(input)]
    agg <- rows[, .(input = input[1], bytes = bytes[1], sha256 = sha256[1], n_sha = data.table::uniqueN(sha256),
                    sources = paste(unique(basename(dirname(dirname(source)))), collapse = " ")), by = input_key]
    cmp <- vpo_compare(ck, agg$sources, basename(agg$input), agg$input, agg$bytes, agg$sha256, tier, hasher)
    if (!is.null(cmp)) cmp[agg$n_sha > 1L, `:=`(status = "pin_conflict", note = "the pin lists record different sha256")]
    out <- c(out, list(cmp))
  }
  data.table::rbindlist(out, fill = TRUE)
}

# Evaluates only the named top-level assignments of an R file, in file order; nothing else in it runs.
vpo_constants <- function(file, names) {
  env <- new.env(parent = baseenv())
  for (e in as.list(parse(file, keep.source = FALSE))) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% names)
      assign(as.character(e[[2]]), eval(e[[3]], env), envir = env)
  }
  mget(names, envir = env, ifnotfound = list(NULL))
}

# The pins held by the tests (VPO_TEST_PINS): one sha256 per pin, NA when the test no longer holds exactly one.
vpo_test_pins <- function(repo_root) {
  vapply(VPO_TEST_PINS, function(p) {
    f <- file.path(repo_root, p[["file"]])
    if (!file.exists(f)) return(NA_character_)
    m <- regmatches(readLines(f, warn = FALSE), regexec(p[["regex"]], readLines(f, warn = FALSE)))
    hit <- unique(unlist(lapply(m, function(z) if (length(z) > 1L) z[2])))
    if (length(hit) == 1L) hit else NA_character_
  }, "")
}

vpo_check_repo_pins <- function(repo_root) {
  ck <- "repo_pins"
  pin <- function(unit, path, want, got, note) {
    status <- if (is.na(want)) "pin_not_found" else if (is.na(got)) "missing" else if (identical(want, got)) "ok" else "sha_mismatch"
    vpo_rows(ck, unit, substring(path, nchar(repo_root) + 2L), path, status, expected_sha256 = want, sha256 = got, note = note)
  }
  out <- list()
  files <- list.files(file.path(repo_root, SXM_DIR), pattern = "^spatial_analysis_exclusions_v[0-9.]+[.]csv$")
  versions <- union(sub("^spatial_analysis_exclusions_(v[0-9.]+)[.]csv$", "\\1", files), names(SXM_RELEASED))
  for (v in versions) {
    p <- sxm_path(v, repo_root)
    out <- c(out, list(pin(paste("SXM_RELEASED", v), p, if (v %in% names(SXM_RELEASED)) unname(SXM_RELEASED[v]) else NA_character_,
                           if (file.exists(p)) sxm_content_sha256(p) else NA_character_, "content sha256 (LF)")))
  }
  tp <- vpo_test_pins(repo_root)
  cfg <- file.path(repo_root, "Functions", "behavior_analysis_config.R")
  s13 <- tryCatch({ e <- new.env(); sys.source(cfg, envir = e)
                    digest::digest(e$MMM_BEHAVIOR_CONFIG$sensitivities$OTHER$S13_hardware, algo = "sha256", serialize = FALSE) },
                  error = function(e) NA_character_)
  out <- c(out, list(pin("S13 digest", cfg, tp[["s13"]], s13, paste("pin from", VPO_TEST_PINS$s13[["file"]])),
                     pin("config byte pin", cfg, tp[["config"]], if (file.exists(cfg)) digest::digest(file = cfg, algo = "sha256") else NA_character_,
                         paste("pin from", VPO_TEST_PINS$config[["file"]]))))
  acp <- vpo_constants(file.path(repo_root, VPO_ACP_FILE), c("ACP_DIR", "ACP_CRITERIA_FILE", "ACP_SCHEDULE_FILE", "ACP_CRITERIA_SHA256", "ACP_SCHEDULE_SHA256"))
  for (k in c("CRITERIA", "SCHEDULE")) {
    rel <- acp[[paste0("ACP_", k, "_FILE")]]; want <- acp[[paste0("ACP_", k, "_SHA256")]]
    p <- if (is.null(rel)) file.path(repo_root, "docs", "rfid_calibration") else file.path(repo_root, rel)
    out <- c(out, list(pin(paste0("ACP_", k, "_SHA256"), p, if (is.null(want)) NA_character_ else want,
                           if (file.exists(p) && !dir.exists(p)) sxm_content_sha256(p) else NA_character_, "content sha256 (LF)")))
  }
  data.table::rbindlist(out, fill = TRUE)
}

# The snapshot of every protected location (the run's own folders left out); time stamps rounded to milliseconds so a
# snapshot read back from CSV compares exactly. Tier 2 adds the sha256 of every file outside VPO_TIER2_SIZE_ONLY.
vpo_snapshot <- function(registry, own, tier, hasher) {
  s <- mmm_protection_snapshot(registry, exclude = own)
  if (tier >= 2L) {
    todo <- which(s$exists & is.na(s$sha256) & !s$class %in% VPO_TIER2_SIZE_ONLY)
    if (length(todo)) data.table::set(s, todo, "sha256", hasher(s$path[todo]))
  }
  s[, mtime := round(mtime, 3)]
  s[]
}

# An earlier run of this check: its snapshot and ledger states, after verifying them against its own manifest.
vpo_read_baseline <- function(dir) {
  need <- c(file.path("snapshot", "baseline_snapshot.csv"), file.path("tables", "ledger_states.csv"))
  mf <- file.path(dir, VPO_RUN_MANIFEST)
  if (!file.exists(mf)) stop("Not a run folder of this check (no ", VPO_RUN_MANIFEST, "): ", dir, call. = FALSE)
  m <- data.table::fread(mf, colClasses = "character", showProgress = FALSE)
  for (f in need) {
    h <- if (file.exists(file.path(dir, f))) digest::digest(file = file.path(dir, f), algo = "sha256") else NA_character_
    if (!identical(h, m$sha256[match(f, m$file)])) stop("The baseline file ", f, " differs from its run's manifest: ", dir, call. = FALSE)
  }
  snap <- data.table::fread(file.path(dir, need[1]), colClasses = list(character = c("path", "location", "class", "sha256"),
                                                                       numeric = c("size", "mtime"), logical = "exists"),
                            na.strings = "NA", showProgress = FALSE)
  ledgers <- data.table::fread(file.path(dir, need[2]), colClasses = list(character = c("ledger", "sha256"), numeric = c("bytes", "rows")),
                               na.strings = "NA", showProgress = FALSE)
  list(dir = dir, snapshot = snap, ledgers = ledgers)
}

# This run's snapshot against the baseline's. Expected: files added in a RANM run folder (or tool folder) that held no
# file at the baseline, and files gone from a RANM runs/.tmp_ folder (published or cleaned since). A file whose size or
# time stamp changed passes only when both snapshots hold the same sha256 for it.
vpo_baseline_diff <- function(old, new, ranm) {
  cols <- c("path", "class", "exists", "size", "mtime", "sha256")
  m <- merge(old[, ..cols], new[, ..cols], by = "path", all = TRUE, suffixes = c("_baseline", "_now"))
  was <- m$exists_baseline %in% TRUE; is <- m$exists_now %in% TRUE
  same <- function(a, b, tol) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= tol)
  meta <- same(m$size_baseline, m$size_now, 0) & same(m$mtime_baseline, m$mtime_now, 5e-4)
  both <- !is.na(m$sha256_baseline) & !is.na(m$sha256_now)
  kind <- ifelse(!was & is, "added", ifelse(was & !is, "removed",
          ifelse(was & is & both & m$sha256_baseline != m$sha256_now, "modified",
          ifelse(was & is & !meta & both, "identical_bytes", ifelse(was & is & !meta, "modified",
          ifelse(startsWith(m$path, "<missing>") & xor(is.na(m$exists_baseline), is.na(m$exists_now)), "location_changed", NA_character_))))))
  d <- m[!is.na(kind)]
  if (!nrow(d)) return(vpo_rows("baseline_diff", "all", "", ranm, "ok", note = "no difference"))
  d[, status := kind[!is.na(kind)]]
  rk <- .mmm_po_key(ranm)
  key <- .mmm_po_key(d$path)
  inr <- startsWith(key, paste0(rk, "/"))
  unit <- rep(NA_character_, nrow(d))
  comps <- strsplit(substring(key[inr], nchar(rk) + 2L), "/", fixed = TRUE)
  unit[inr] <- vapply(comps, function(c) if (length(c) >= 3L && c[2] == "runs") paste(rk, c[1], "runs", c[3], sep = "/") else paste(rk, c[1], sep = "/"), "")
  old_keys <- .mmm_po_key(old$path[old$exists %in% TRUE])
  units <- unique(unit[!is.na(unit)])
  fresh <- units[!vapply(units, function(u) any(old_keys == u | startsWith(old_keys, paste0(u, "/"))), logical(1))]
  d[status == "added" & unit %in% fresh, status := "expected_addition"]
  d[status == "removed" & !is.na(unit) & grepl("/runs/[.]tmp_[^/]*$", unit), status := "expected_removal"]
  shown <- ifelse(is.na(unit), ifelse(is.na(d$class_now), d$class_baseline, d$class_now), paste0("RANM/", substring(unit, nchar(rk) + 2L)))
  vpo_rows("baseline_diff", shown, basename(d$path), d$path, d$status, expected_bytes = d$size_baseline, bytes = d$size_now,
           expected_sha256 = d$sha256_baseline, sha256 = d$sha256_now)
}

vpo_summary <- function(res) {
  data.table::rbindlist(lapply(names(res), function(k) {
    d <- res[[k]]
    n <- if (is.null(d)) 0L else nrow(d)
    bad <- if (n) !d$status %in% VPO_OK else logical(0)
    data.table::data.table(check = k, baseline_source = unname(VPO_CHECKS[k]), rows = n, ok = n - sum(bad), failed = sum(bad),
                           status = if (n && !any(bad)) "PASS" else "FAIL",
                           failures = if (!n) "nothing was checked" else paste(utils::head(unique(paste(d$status[bad], d$unit[bad])), 6), collapse = "; "))
  }))
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  data.table::setDTthreads(1L)
  options(width = 200)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R", "spatial_exclusion_manifest.R")) source_mmm_helper(h)
  a <- vpo_args(commandArgs(trailingOnly = TRUE))
  t0 <- Sys.time()
  project_root <- mmm_project_root()
  head_commit <- vpo_git(c("rev-parse", "HEAD"))[1]
  run_id <- if (is.na(a$run_id)) sprintf("vpo_t%d_%s_%s", a$tier, format(t0, "%Y%m%d_%H%M"), substr(head_commit, 1, 7)) else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  dev <- startsWith(run_id, "dev")
  dirty <- vpo_git(c("status", "--porcelain", "--untracked-files=all", "--", VPO_CODE))
  if (!dev && (!is.null(attr(dirty, "status")) || length(dirty))) stop("A release run needs committed code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  ranm <- file.path(dirname(project_root), "RANM")
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(ranm, "protected_output_checks") else a$output_root, project_root)
  registry <- mmm_protected_output_locations(project_root, MMM_REPO_ROOT)
  own <- mmm_protection_assert_base(base, run_id, registry)
  tmp <- own[1]; out_dir <- own[2]
  if (file.exists(tmp)) stop("A partial run of this id exists; choose a new --run-id: ", tmp, call. = FALSE)
  mmm_assert_output_path_budget(c(file.path(tmp, VPO_LONGEST_RELATIVE), file.path(out_dir, VPO_LONGEST_RELATIVE)), source_label = "protected-output check")
  baseline <- if (is.na(a$baseline)) NULL else vpo_read_baseline(normalizePath(a$baseline, winslash = "/", mustWork = TRUE))
  hasher <- vpo_hasher()

  # all reads first; nothing is written before the checks are done
  res <- list(frozen_pins = vpo_check_frozen_pins(project_root, MMM_REPO_ROOT, a$tier, hasher))
  res <- c(res, vpo_check_registered_runs(project_root, a$tier, hasher))
  res$stage19_record <- vpo_check_stage19(project_root, MMM_REPO_ROOT, a$tier, hasher)
  res$stage31_runs <- vpo_check_stage31(project_root, a$tier, hasher)
  res$stage31_censored <- vpo_check_censored(ranm, a$tier, hasher)
  res$ranm_runs <- vpo_check_ranm_runs(ranm, a$tier, hasher, own)
  ledger_roots <- unique(c(file.path(project_root, VPO_QC_ROOT), dirname(Sys.glob(file.path(ranm, "*", "runs_ledger.csv")))))
  led <- vpo_check_ledgers(ledger_roots, a$tier, hasher)
  res$stage00b_ledger_chain <- data.table::rbindlist(list(led$rows, if (!is.null(baseline)) vpo_ledger_prefix(baseline$ledgers)), fill = TRUE)
  res$canonical_bundles <- vpo_check_bundles(project_root, a$tier, hasher)
  res$manuscript_imports <- vpo_check_manuscript(file.path(dirname(dirname(project_root)), "Exp9_manuscript"), a$tier, hasher)
  res$recorded_inputs <- vpo_check_recorded_inputs(project_root, registry, a$tier, hasher)
  res$repo_pins <- vpo_check_repo_pins(MMM_REPO_ROOT)
  snap <- vpo_snapshot(registry, own, a$tier, hasher)
  res$baseline_diff <- if (!is.null(baseline)) vpo_baseline_diff(baseline$snapshot, snap, ranm) else
    vpo_rows("baseline_diff", "all", "", base, if (a$first_run) "first_run" else "not_run",
             note = if (a$first_run) "first snapshot of the round: the baseline of the end-of-round check" else
               "no --baseline: the files without a self-manifest (tables/unchecked_files.csv) were not compared")
  checked <- unique(.mmm_po_key(unlist(lapply(res[names(res) != "baseline_diff"], function(d) if (!is.null(d) && nrow(d)) d$path))))
  unchecked <- snap[exists %in% TRUE & !.mmm_po_key(path) %in% checked, .(path, location, class, size)]
  summary <- vpo_summary(res)
  overall <- if (all(summary$status == "PASS")) "PASS" else "FAIL"
  summary <- rbind(summary, data.table::data.table(check = "OVERALL", baseline_source = sprintf("tier %d", a$tier), rows = sum(summary$rows),
                                                   ok = sum(summary$ok), failed = sum(summary$failed), status = overall,
                                                   failures = paste(summary$check[summary$status != "PASS"], collapse = " ")))
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  for (dd in c("tables", "snapshot", "manifest")) dir.create(file.path(tmp, dd), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(summary, file.path(tmp, "tables", "summary.csv"))
  for (k in names(res)) if (!is.null(res[[k]])) data.table::fwrite(res[[k]], file.path(tmp, "tables", paste0("check_", k, ".csv")))
  data.table::fwrite(led$states, file.path(tmp, "tables", "ledger_states.csv"), na = "NA")
  data.table::fwrite(registry[, exists := file.exists(location)], file.path(tmp, "tables", "registry.csv"))
  data.table::fwrite(unchecked, file.path(tmp, "tables", "unchecked_files.csv"))
  data.table::fwrite(snap[, .(path, location, class, exists, size, mtime, sha256)], file.path(tmp, "snapshot", "baseline_snapshot.csv"), na = "NA")
  inputs <- unique(rbind(snap[!is.na(sha256), .(input = path, role = "protected file (hashed)", sha256, bytes = size)],
                         data.table::data.table(input = file.path(MMM_REPO_ROOT, c(VPO_STAGE19_RECORDS, vapply(VPO_TEST_PINS, `[[`, "", "file"))),
                                                role = "record or pin source")[, `:=`(sha256 = hasher(input), bytes = file.size(input))]))
  data.table::fwrite(inputs, file.path(tmp, "manifest", "input_hashes.csv"))
  code <- data.table::data.table(file = VPO_CODE)[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  data.table::fwrite(code, file.path(tmp, "manifest", "code_hashes.csv"))
  run <- data.table::data.table(item = c("run_id", "tier", "overall", "git_commit", "code_clean", "dev_run", "project_root", "raw_root", "output_root",
                                         "baseline", "first_run", "protected_files", "hashed_files", "unchecked_files", "started", "finished",
                                         "runtime_s", "R"),
                                value = c(run_id, a$tier, overall, head_commit, as.character(!length(dirty)), as.character(dev), project_root,
                                          mmm_raw_antenna_root(project_root), base, if (is.null(baseline)) "" else baseline$dir,
                                          as.character(a$first_run), sum(snap$exists), sum(!is.na(snap$sha256)), nrow(unchecked),
                                          format(t0, "%Y-%m-%d %H:%M:%S"), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sprintf("%.1f", runtime),
                                          R.version.string))
  data.table::fwrite(run, file.path(tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(tmp, "manifest", "session_info.txt"))
  writeLines(c(paste("PROTECTED-OUTPUT CHECK (read-only), tier", a$tier, "|", overall), "",
               paste("run:", run_id, "| commit:", head_commit, if (!is.null(baseline)) paste("| baseline:", baseline$dir)), "",
               capture.output(print(summary[, .(check, rows, failed, status)])), "",
               "tables/summary.csv            PASS/FAIL per check, with the record each check compares with",
               "tables/check_<name>.csv       one row per file: status, recorded and found bytes and sha256",
               "tables/unchecked_files.csv    protected files with no self-manifest or record row; only a --baseline",
               sprintf("                              comparison checks them (%d files here)", nrow(unchecked)),
               "snapshot/baseline_snapshot.csv size, time stamp and sha256 (where taken) of every protected file; pass this",
               "                              run folder as --baseline to a later run to list every difference",
               "Registry and guard: Functions/protected_outputs.R. Nothing outside this folder was written."), file.path(tmp, "README.txt"))
  outs <- list.files(tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
                     file.path(tmp, VPO_RUN_MANIFEST))
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  print(summary[, .(check, rows, ok, failed, status, failures = substr(failures, 1, 110))])
  message(sprintf("protected-output check tier %d: %s in %.1f s -> %s", a$tier, overall, runtime, out_dir))
  if (overall != "PASS") quit(save = "no", status = 1L)
}
