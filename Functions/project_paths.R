# ================================================================
# Portable roots and a SEMANTIC path registry
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# Manuscript-assembly code must be able to ask for an input by what it MEANS
# ("the canonical first-night animal-level movement table") instead of by where
# it currently happens to sit ("analysis_ready/pipeline/09_early_prediction/
# 10min/tables/..."). The output tree is expected to be reorganised later; when
# that happens only THIS file should need editing, not every figure script.
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO
#
#   * It does not replace the stage-directory helpers in
#     Analysis/_pipeline_setup.R. behavior_stage_dir(), behavior_stage_tables(),
#     behavior_manuscript_dir(), behavior_analysis_ready_dir() and
#     resolve_stage09_early_prediction_artifact() are REUSED here, never
#     reimplemented. This is a semantic key layer ON TOP OF the existing path
#     layer, not a second path layer.
#   * It does not move, copy or rewrite any existing output.
#   * It does not invent a new configuration convention. It UNIFIES the three
#     that already exist, all of which carry the identical S: default and
#     currently have no cross-fallback at all:
#       1. getOption("mmm.project_root", <default>)
#          -- Analysis/08_hmm_behavioral_states_optional.R:46-49, and the
#             documented convention for portable tests
#             (Testing/tests/test_output_path_length.R:21,
#              test_first_night_window_parity.R:153, docs/REPRODUCIBILITY.md:145)
#       2. Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", unset = <default>)
#          -- Analysis/09_early_prediction_model_ladder.R:53,
#             Analysis/build_publication_release.R:59-64 (also --project-root=),
#             manuscript/Fig1_behavior_candidates/build_fig1_candidates.R:39
#       3. a bare hard-coded literal -- 9 active stages including 14, 15 and the
#          whole GAMM family 20-26
#     Today, setting the option does NOT redirect Stage 09 or the release
#     builder, and setting the environment variable does NOT redirect Stage 08 or
#     Stages 20-26. mmm_project_root() honours BOTH, so new code follows whichever
#     one the caller already uses. Migrating the existing stages onto it is a
#     separate, deliberate change (see docs/FUTURE_REPO_RESTRUCTURE_PLAN.md);
#     nothing here modifies them.
#
# PRECEDENCE for every root: getOption() > Sys.getenv() > documented default.
#
# The maintainer-specific default below is the ONE place it may appear in new
# code. New analysis or manuscript scripts must call the accessors instead of
# repeating the literal; Testing/tests/test_behavior_main_figure_contracts.R
# asserts that.
# ================================================================

# ------------------------------------------------------------------ roots

#' Documented default analysis-data root.
#'
#' Historically every stage script repeated this literal. It is centralised here
#' so a different machine only needs options(mmm.project_root = ...) or the
#' MMM_PROJECT_ROOT environment variable.
MMM_PROJECT_ROOT_DEFAULT <-
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"

.mmm_configured <- function(option_name, env_names, default) {
  from_option <- getOption(option_name, NULL)
  if (!is.null(from_option) && length(from_option) == 1L &&
      !is.na(from_option) && nzchar(from_option)) {
    return(as.character(from_option))
  }
  for (env_name in env_names) {
    from_env <- Sys.getenv(env_name, unset = "")
    if (nzchar(from_env)) return(from_env)
  }
  default
}

#' Environment variables consulted for the analysis-data root, in order.
#'
#' MMM_BEHAVIOR_PROJECT_ROOT comes first because it is the name Stage 09 and the
#' release builder already read; MMM_PROJECT_ROOT is accepted as a shorter alias
#' so a caller who guesses the obvious name is not silently ignored.
MMM_PROJECT_ROOT_ENV_VARS <- c("MMM_BEHAVIOR_PROJECT_ROOT", "MMM_PROJECT_ROOT")

#' Configured analysis-data root, where the statistical stages read and write.
mmm_project_root <- function() {
  root <- .mmm_configured("mmm.project_root", MMM_PROJECT_ROOT_ENV_VARS,
                          MMM_PROJECT_ROOT_DEFAULT)
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

# Semantic output groups for bounded, receipt-activated migrations.
# Direct callers can request a layout explicitly. Active callers use the
# validated per-group migration receipt below; directory existence never
# chooses a scientific input. Without a receipt, the current layout is used.
mmm_behavior_output_group_root <- function(group,
                                           layout = c("current", "semantic"),
                                           project_root = mmm_project_root()) {
  layout <- match.arg(layout)
  paths <- list(
    behavior_metrics_foundation = c(
      current = "03_derived_metrics",
      semantic = "foundations/behavior_metrics"),
    systems_dashboard_5min = c(
      current = "12_systems_neuroscience_summary/5min_based",
      semantic = "analyses/systems_dashboard/5min"),
    first_night_10min = c(
      current = "12_systems_neuroscience_summary/5min_based/first_night/10min_based",
      semantic = "analyses/first_night_five_domain_characterization/10min"),
    first_night_5min = c(
      current = "12_systems_neuroscience_summary/5min_based/first_night/5min_based",
      semantic = "analyses/first_night_five_domain_characterization/5min"),
    spatial_tables = c(current = "03_derived_metrics/spatial_occupancy",
                       semantic = "analyses/spatial_occupancy/tables"),
    spatial_audit = c(current = "03_derived_metrics/spatial_occupancy",
                      semantic = "analyses/spatial_occupancy/audit"),
    spatial_models = c(current = "04_model_outputs/spatial_occupancy",
                       semantic = "analyses/spatial_occupancy/models"),
    spatial_figures = c(current = "05_figures/spatial_occupancy",
                        semantic = "analyses/spatial_occupancy/figures"),
    dyadic_contacts = c(current = "06_behavioral_dynamics/dyadic_contacts",
                        semantic = "analyses/dyadic_contacts"),
    social_networks_5min = c(
      current = "06_behavioral_dynamics/social_networks/5min_based",
      semantic = "analyses/dynamic_social_networks/5min"),
    gamm_features_10min = c(
      current = "06_behavioral_dynamics/gamm_features/10min_based",
      semantic = "analyses/gamm_trajectory_features/10min"),
    state_space_5min = c(
      current = "06_behavioral_dynamics/state_space/5min_based",
      semantic = "analyses/behavioral_state_space/5min"),
    hmm_states_10min = c(
      current = "06_behavioral_dynamics/hmm_states/10min_based",
      semantic = "analyses/hmm_states/10min"),
    hmm_states_5min = c(
      current = "06_behavioral_dynamics/hmm_states/5min_based",
      semantic = "analyses/hmm_states/5min"),
    temporal_instability_10sec = c(
      current = "06_behavioral_dynamics/temporal_instability/10sec_based",
      semantic = "analyses/temporal_instability/10sec"),
    history_social_networks_10sec = c(
      current = "06_behavioral_dynamics/social_networks/10sec_based",
      semantic = "history/social_networks/10sec"),
    history_social_networks_1min = c(
      current = "06_behavioral_dynamics/social_networks/1min_based",
      semantic = "history/social_networks/1min"),
    history_social_networks_10min = c(
      current = "06_behavioral_dynamics/social_networks/10min_based",
      semantic = "history/social_networks/10min"),
    history_social_networks_30min = c(
      current = "06_behavioral_dynamics/social_networks/30min_based",
      semantic = "history/social_networks/30min"),
    history_state_space_1min = c(
      current = "06_behavioral_dynamics/state_space/1min_based",
      semantic = "history/state_space/1min"),
    history_state_space_10min = c(
      current = "06_behavioral_dynamics/state_space/10min_based",
      semantic = "history/state_space/10min"),
    history_temporal_instability_1min = c(
      current = "06_behavioral_dynamics/temporal_instability/1min_based",
      semantic = "history/temporal_instability/1min"),
    history_temporal_instability_5min = c(
      current = "06_behavioral_dynamics/temporal_instability/5min_based",
      semantic = "history/temporal_instability/5min"),
    history_gamm_features_30min = c(
      current = "06_behavioral_dynamics/gamm_features/30min_based",
      semantic = "history/gamm_features/30min"),
    proteomics_mnn_primary = c(
      current = "06_behavioral_dynamics/proteomics_mnn_primary",
      semantic = "analyses/behavior_proteomics/proteomics_mnn_primary"),
    proteomics_mnn_sensitivity = c(
      current = "06_behavioral_dynamics/proteomics_mnn_sensitivity",
      semantic = "analyses/behavior_proteomics/proteomics_mnn_sensitivity"),
    adaptation_kinetics_10min = c(
      current = "15_behavioral_adaptation_kinetics/10min_based",
      semantic = "analyses/adaptation_kinetics/10min"),
    sleep_like_inactivity_10min = c(
      current = "16_sleep_like_inactivity_metrics/10min_based",
      semantic = "analyses/sleep_like_inactivity/10min"),
    phase_organization_10min = c(
      current = "17_ethological_phase_organization/10min_based",
      semantic = "analyses/phase_organization/10min"),
    nonlinear_dynamics_5min = c(
      current = "13_nonlinear_systems_dynamics/5min_based",
      semantic = "analyses/nonlinear_dynamics/5min"),
    systems_phenotyping_5min = c(
      current = "14_nextgen_behavioral_phenotyping/5min_based",
      semantic = "analyses/systems_phenotyping/5min"),
    inactive_phase_qc_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_inactive_phase_qc",
      semantic = "analyses/inactive_phase_qc_audit"),
    rfid_domain_comparison_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_rfid_legacy_vs_new",
      semantic = "analyses/rfid_domain_comparison_audit"),
    rfid_leading_bin_seed_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_rfid_leading_bin_seed",
      semantic = "analyses/rfid_leading_bin_seed_audit"),
    rfid_construct_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_rfid_domain_construct_blind",
      semantic = "analyses/rfid_construct_audit"),
    rfid_conservatism_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_rfid_conservatism",
      semantic = "analyses/rfid_conservatism_audit"),
    rfid_alternative_inference_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_rfid_alternative_inference",
      semantic = "analyses/rfid_alternative_inference_audit"),
    rfid_reliability_audit = c(
      current = "12_systems_neuroscience_summary/5min_based/audit_rfid_reliability_improvement",
      semantic = "analyses/rfid_reliability_audit"),
    history_tracking_integrity_10sec = c(
      current = "00_qc_tracking_integrity",
      semantic = "history/tracking_integrity/10sec"),
    proteomics_module_scores = c(
      current = "proteomics",
      semantic = "foundations/proteomics_module_scores")
  )
  if (length(group) != 1L || is.na(group) || !group %in% names(paths)) {
    stop("Unknown behavioral output group: ", paste(group, collapse = ", "),
         call. = FALSE)
  }
  file.path(project_root, "analysis_ready", paths[[group]][[layout]])
}

# Each top-level tree of the original layout may be retained unchanged under
# history/original_layout/<root>/. Its archive receipt is separate from each
# semantic-copy activation receipt; without that receipt the original location
# is still mandatory. Maintenance/BehaviorNumberedRootLocation.ps1 and the
# archive tools' RootName lists must name the same roots
# (Testing/tests/test_numbered_root_lists_agree.R).
MMM_NUMBERED_BEHAVIOR_ROOTS <- c(
  "03_derived_metrics", "06_behavioral_dynamics",
  "12_systems_neuroscience_summary",
  "00_qc_tracking_integrity", "03_primary_raw_movement_phase_stats",
  "04_model_outputs", "05_figures", "13_nonlinear_systems_dynamics",
  "14_nextgen_behavioral_phenotyping", "15_behavioral_adaptation_kinetics",
  "16_manuscript_behavior_report", "16_sleep_like_inactivity_metrics",
  "17_ethological_phase_organization", "18_raw_movement_publication_trajectory",
  "18b_raw_movement_broad_phase_stats",
  "18c_raw_movement_broad_phase_stats_corrected", "proteomics",
  "_archive_stale_stage10_outputs", "_archive_stale_stage27_candidates",
  "_quarantine_legacy_s09")

# Paths on the Windows share are case-insensitive, and normalizePath() keeps
# the caller's spelling for a path that does not exist yet.
.mmm_path_key <- function(path) {
  key <- sub("/+$", "", normalizePath(path, winslash = "/", mustWork = FALSE))
  if (identical(.Platform$OS.type, "windows")) tolower(key) else key
}
.mmm_path_within <- function(path, root) {
  path <- .mmm_path_key(path)
  root <- .mmm_path_key(root)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}
.mmm_is_unc_path <- function(path) {
  grepl("^(//|\\\\\\\\)", normalizePath(path, winslash = "/", mustWork = FALSE))
}

# Read a numbered-root archive receipt with the same schema the transaction
# tool writes. Exact field names only: `$` would partially match state_note.
.mmm_read_numbered_archive_receipt <- function(receipt_path, numbered) {
  receipt <- tryCatch(jsonlite::fromJSON(receipt_path, simplifyVector = FALSE),
                      error = function(e) stop("Invalid numbered source archive receipt: ",
                                               receipt_path, ": ", conditionMessage(e),
                                               call. = FALSE))
  text <- function(name, pattern = NULL) {
    value <- receipt[[name]]
    is.character(value) && length(value) == 1L && !is.na(value) &&
      (is.null(pattern) || grepl(pattern, value))
  }
  count <- function(name, minimum) {
    value <- receipt[[name]]
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      value >= minimum && value == floor(value)
  }
  if (!is.list(receipt) || is.null(names(receipt)) ||
      anyDuplicated(names(receipt)) ||
      !identical(receipt[["root"]], numbered) ||
      !identical(receipt[["source_root_rel"]], numbered) ||
      !identical(receipt[["archive_root_rel"]],
                 paste0("history/original_layout/", numbered)) ||
      !text("manifest_sha256", "^[0-9a-f]{64}$") ||
      !text("reader_gate_sha256", "^[0-9a-f]{64}$") ||
      !text("reader_queue_sha256", "^[0-9a-f]{64}$") ||
      !text("reader_gate_kind", "^(ScientificReplay|ArchivePath)$") ||
      !count("files", 1) || !count("bytes", 0) ||
      !text("state", "^(prepared|transferring|activated)$")) {
    stop("Numbered source archive receipt does not match ", numbered, ": ",
         receipt_path, call. = FALSE)
  }
  receipt
}

mmm_behavior_retained_source_root <- function(group,
                                              project_root = mmm_project_root()) {
  current <- mmm_behavior_output_group_root(group, "current", project_root)
  ready <- file.path(project_root, "analysis_ready")
  # Split the static relative mapping, not a normalized path whose case can
  # differ from the caller's spelling once the numbered source has moved.
  parts <- strsplit(substring(current, nchar(ready) + 2L), "/",
                    fixed = TRUE)[[1L]]
  if (!startsWith(current, paste0(ready, "/")) || length(parts) == 0L ||
      any(parts %in% c("", ".", ".."))) {
    stop("Behavior output source escaped analysis_ready: ", current,
         call. = FALSE)
  }
  numbered <- parts[[1L]]
  if (!numbered %in% MMM_NUMBERED_BEHAVIOR_ROOTS) return(current)
  retained <- .mmm_behavior_retained_root(numbered, project_root)
  if (length(parts) == 1L) retained else
    file.path(retained, paste(parts[-1L], collapse = "/"))
}

# Where a top-level tree of the original layout is now: analysis_ready/<root>
# without a receipt or while prepared, history/original_layout/<root> once
# activated. The receipt selects the location, never directory existence
# alone, and every intermediate or inconsistent state stops the caller.
.mmm_behavior_retained_root <- function(numbered, project_root) {
  old_root <- file.path(project_root, "analysis_ready", numbered)
  archive_root <- file.path(project_root, "analysis_ready", "history",
                            "original_layout", numbered)
  receipt_path <- file.path(project_root, "analysis_ready", "_migration_control",
                            "numbered_root_archive", paste0(numbered, ".json"))
  if (!file.exists(receipt_path)) {
    if (dir.exists(archive_root)) {
      stop("Numbered source archive exists without a receipt for ", numbered,
           call. = FALSE)
    }
    return(old_root)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to validate a numbered source archive receipt.",
         call. = FALSE)
  }
  state <- .mmm_read_numbered_archive_receipt(receipt_path, numbered)[["state"]]
  if (identical(state, "prepared")) {
    if (!dir.exists(old_root) || dir.exists(archive_root)) {
      stop("Prepared numbered source archive has unexpected locations for ",
           numbered, call. = FALSE)
    }
    return(old_root)
  }
  if (identical(state, "transferring")) {
    stop("Numbered source archive is transferring for ", numbered,
         call. = FALSE)
  }
  if (dir.exists(old_root) || !dir.exists(archive_root)) {
    stop("Activated numbered source archive has unexpected locations for ",
         numbered, call. = FALSE)
  }
  archive_root
}

# Historical audit replays read the retained original lineage, regardless of
# whether its top-level numbered root has later been archived.
mmm_behavior_numbered_source_root <- function(root_name,
                                              project_root = mmm_project_root()) {
  if (length(root_name) != 1L || is.na(root_name) ||
      !root_name %in% MMM_NUMBERED_BEHAVIOR_ROOTS) {
    stop("Unknown numbered behavioral source root: ", root_name,
         call. = FALSE)
  }
  .mmm_behavior_retained_root(root_name, project_root)
}

# A historical or optional producer must not recreate a numbered top-level
# root once its archive transaction has begun. Readers use the retained source
# resolver above; writes need a separate, stricter guard.
mmm_behavior_numbered_writer_root <- function(root_name,
                                              project_root = mmm_project_root()) {
  source <- mmm_behavior_numbered_source_root(root_name, project_root)
  old <- file.path(project_root, "analysis_ready", root_name)
  receipt <- file.path(project_root, "analysis_ready", "_migration_control",
                       "numbered_root_archive", paste0(root_name, ".json"))
  if (file.exists(receipt) ||
      !identical(.mmm_path_key(source), .mmm_path_key(old))) {
    stop("Numbered behavioral output root is under archive control; ",
         "refusing to write: ", old, call. = FALSE)
  }
  old
}

# Resolution helpers also serve readers, so guard the producer's resolved path
# separately. An optional run may otherwise write into the retained archive.
mmm_behavior_guard_numbered_output_path <- function(path,
                                                    project_root = mmm_project_root()) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Behavior output path must be one nonempty path.", call. = FALSE)
  }
  # A UNC spelling of a drive-letter project root (or the reverse) cannot be
  # compared as a string, so refuse it wherever it names a guarded root.
  guarded <- paste0("/analysis_ready/(",
                    paste(c(MMM_NUMBERED_BEHAVIOR_ROOTS, "history/original_layout"),
                          collapse = "|"), ")(/|$)")
  if (!identical(.mmm_is_unc_path(path), .mmm_is_unc_path(project_root)) &&
      grepl(guarded, tolower(gsub("\\\\", "/", path)))) {
    stop("Refusing a numbered-root output path spelled differently from the ",
         "project root: ", path, call. = FALSE)
  }
  for (root_name in MMM_NUMBERED_BEHAVIOR_ROOTS) {
    old <- file.path(project_root, "analysis_ready", root_name)
    archived <- file.path(project_root, "analysis_ready", "history",
                          "original_layout", root_name)
    if (.mmm_path_within(path, archived)) {
      stop("Refusing to write into retained numbered archive: ", path,
           call. = FALSE)
    }
    if (.mmm_path_within(path, old)) {
      mmm_behavior_numbered_writer_root(root_name, project_root)
    }
  }
  path
}

# A replay requires an explicit run identifier and a previously unused output
# directory. No historical original or prior replay output may be overwritten.
mmm_behavior_audit_replay_path <- function(script_id,
                                           project_root = mmm_project_root(),
                                           run_id = Sys.getenv(
                                             "MMM_BEHAVIOR_AUDIT_REPLAY_ID",
                                             unset = "")) {
  # The shared writers mirror any path containing /figures/ and canonicalize
  # /pipeline/ paths (Functions/behavioral_dynamics_helpers.R), so these ids
  # would place copies outside the replay folder.
  reserved <- c("figures", "pipeline")
  valid <- function(x, longest) is.character(x) && length(x) == 1L && !is.na(x) &&
    nchar(x) <= longest && grepl("^[a-z0-9][a-z0-9_-]{2,}$", x) && !x %in% reserved
  # The longest queued replay output is 218 characters plus the run id under
  # the default root; 20 characters keeps it within the 240-character budget.
  if (!valid(script_id, 64L) || !valid(run_id, 20L)) {
    stop("Historical audit replay needs a safe script id and an explicit run id of ",
         "at most 20 characters (MMM_BEHAVIOR_AUDIT_REPLAY_ID); 'figures' and ",
         "'pipeline' are reserved.", call. = FALSE)
  }
  path <- file.path(project_root, "analysis_ready", "analyses",
                    "historical_audit_replays", run_id, script_id)
  path
}

mmm_behavior_audit_replay_output_root <- function(script_id,
                                                  project_root = mmm_project_root(),
                                                  run_id = Sys.getenv(
                                                    "MMM_BEHAVIOR_AUDIT_REPLAY_ID",
                                                    unset = "")) {
  path <- mmm_behavior_audit_replay_path(script_id, project_root, run_id)
  if (file.exists(path) || dir.exists(path)) {
    stop("Historical audit replay output already exists: ", path,
         call. = FALSE)
  }
  path
}

mmm_behavior_audit_replay_input_root <- function(script_id,
                                                 project_root = mmm_project_root(),
                                                 run_id = Sys.getenv(
                                                   "MMM_BEHAVIOR_AUDIT_REPLAY_ID",
                                                   unset = "")) {
  path <- mmm_behavior_audit_replay_path(script_id, project_root, run_id)
  if (!dir.exists(path)) {
    stop("Missing prerequisite historical audit replay: ", path,
         call. = FALSE)
  }
  path
}

# The migration receipt is the explicit per-group layout switch. The semantic
# directory alone never selects a scientific input. Intermediate and corrupted
# migration states fail closed rather than silently returning to the old tree.
mmm_behavior_output_layout_state <- function(group, project_root = mmm_project_root()) {
  current <- mmm_behavior_retained_source_root(group, project_root)
  semantic <- mmm_behavior_output_group_root(group, "semantic", project_root)
  receipt_path <- file.path(project_root, "analysis_ready", "_migration_control",
                            paste0(group, ".json"))
  if (!file.exists(receipt_path)) {
    if (file.exists(semantic) || dir.exists(semantic)) {
      stop("Semantic output exists without an activation receipt for ", group,
           ": ", semantic, call. = FALSE)
    }
    return("current")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to validate a behavioral migration receipt.",
         call. = FALSE)
  }
  receipt <- tryCatch(jsonlite::fromJSON(receipt_path),
                      error = function(e) stop("Invalid migration receipt: ",
                                               receipt_path, ": ", conditionMessage(e),
                                               call. = FALSE))
  # Compare normalized absolute roots instead of relying on machine-specific
  # slash spelling in the receipt.
  recorded_target <- if (is.character(receipt$target_root_rel) &&
                         length(receipt$target_root_rel) == 1L) {
    file.path(project_root, "analysis_ready", receipt$target_root_rel)
  } else NA_character_
  if (!identical(receipt$group, group) ||
      !isTRUE(receipt$source_retained) ||
      !is.character(receipt$group_plan_sha256) ||
      length(receipt$group_plan_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", receipt$group_plan_sha256) ||
      !is.character(receipt$contract_sha256) ||
      length(receipt$contract_sha256) != 1L ||
      !grepl("^[0-9a-f]{64}$", receipt$contract_sha256) ||
      !is.numeric(receipt$files) || length(receipt$files) != 1L ||
      is.na(receipt$files) || receipt$files < 1L ||
      is.na(recorded_target) ||
      !identical(normalizePath(recorded_target, winslash = "/", mustWork = FALSE),
                 normalizePath(semantic, winslash = "/", mustWork = FALSE))) {
    stop("Migration receipt does not match output group ", group, ": ",
         receipt_path, call. = FALSE)
  }
  if (identical(receipt$state, "prepared")) {
    if (file.exists(semantic) || dir.exists(semantic)) {
      stop("Prepared migration has an activated destination for ", group,
           "; finish or repair activation before running analyses.", call. = FALSE)
    }
    return("current")
  }
  if (identical(receipt$state, "activated")) {
    if (!dir.exists(semantic) || !dir.exists(current)) {
      stop("Activated migration is missing a destination or retained source for ",
           group, call. = FALSE)
    }
    return("semantic")
  }
  stop("Unknown migration receipt state for ", group, ": ", receipt$state,
       call. = FALSE)
}

mmm_behavior_output_active_root <- function(group, project_root = mmm_project_root()) {
  layout <- mmm_behavior_output_layout_state(group, project_root)
  if (identical(layout, "current")) {
    return(mmm_behavior_retained_source_root(group, project_root))
  }
  mmm_behavior_output_group_root(group, "semantic", project_root)
}

# Stage 01's explicit output override is used by the separate cookie-habituation
# runner. Only the default main-experiment root follows an activation receipt.
mmm_derived_metrics_output_root <- function(project_root = mmm_project_root(),
                                            configured_root = getOption("mmm.derived_metrics_dir", NULL)) {
  if (!is.null(configured_root)) {
    if (!is.character(configured_root) || length(configured_root) != 1L ||
        is.na(configured_root) || !nzchar(configured_root)) {
      stop("mmm.derived_metrics_dir must be one nonempty path.", call. = FALSE)
    }
    return(configured_root)
  }
  mmm_behavior_output_active_root("behavior_metrics_foundation", project_root)
}

# The eight May 2026 QC products have no proved lineage to the current Stage 01
# files. A new Stage 00 run must not overwrite them or silently replace the
# optional manuscript/release sources before its results are reviewed. Their
# receipt-selected copy is history/tracking_integrity/10sec/.
mmm_tracking_qc_historical_root <- function(project_root = mmm_project_root()) {
  mmm_behavior_output_active_root("history_tracking_integrity_10sec", project_root)
}

mmm_tracking_qc_new_run_root <- function(project_root = mmm_project_root()) {
  file.path(project_root, "analysis_ready", "quality_control",
            "tracking_integrity")
}

mmm_source_relative_path <- function(path, project_root = mmm_project_root()) {
  root <- normalizePath(file.path(project_root, "analysis_ready"),
                        winslash = "/", mustWork = FALSE)
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Expected one analysis_ready source path.", call. = FALSE)
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (!startsWith(normalized, prefix)) {
    stop("Source path escaped analysis_ready: ", path, call. = FALSE)
  }
  substring(normalized, nchar(prefix) + 1L)
}

# Older resolution trees remain scientific inputs for optional readers. Keep
# their nine independently reviewed history cutovers behind receipts; an
# unmapped resolution retains its original path.
mmm_behavior_historical_resolution_groups <- c(
  "social_networks/10sec_based" = "history_social_networks_10sec",
  "social_networks/1min_based" = "history_social_networks_1min",
  "social_networks/10min_based" = "history_social_networks_10min",
  "social_networks/30min_based" = "history_social_networks_30min",
  "state_space/1min_based" = "history_state_space_1min",
  "state_space/10min_based" = "history_state_space_10min",
  "temporal_instability/1min_based" = "history_temporal_instability_1min",
  "temporal_instability/5min_based" = "history_temporal_instability_5min",
  "gamm_features/30min_based" = "history_gamm_features_30min"
)

mmm_behavior_historical_resolution_root <- function(family, resolution,
                                                    project_root = mmm_project_root()) {
  if (length(family) != 1L || is.na(family) ||
      length(resolution) != 1L || is.na(resolution)) {
    stop("Historical behavior lookup needs one family and resolution.", call. = FALSE)
  }
  key <- paste(family, resolution, sep = "/")
  if (!key %in% names(mmm_behavior_historical_resolution_groups)) {
    return(file.path(project_root, "analysis_ready", "06_behavioral_dynamics",
                     family, resolution))
  }
  mmm_behavior_output_active_root(mmm_behavior_historical_resolution_groups[[key]],
                                  project_root)
}

# Stage 10 discovers files in the retained numbered root. Rewrite each
# receipt-activated historical file in place so discovery order and basename
# stay fixed. The numbered source still has to exist until discovery itself
# is moved to a reviewed semantic manifest.
mmm_behavior_route_historical_feature_sources <- function(paths,
                                                          project_root = mmm_project_root()) {
  if (length(paths) == 0L) return(paths)
  routed <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  for (group in unname(mmm_behavior_historical_resolution_groups)) {
    old <- normalizePath(mmm_behavior_output_group_root(group, "current", project_root),
                         winslash = "/", mustWork = FALSE)
    active <- mmm_behavior_output_active_root(group, project_root)
    if (identical(normalizePath(active, winslash = "/", mustWork = FALSE), old)) next
    matched <- which(startsWith(routed, paste0(old, "/")))
    for (i in matched) {
      suffix <- substring(routed[[i]], nchar(old) + 2L)
      target <- file.path(active, suffix)
      if (!file.exists(target) ||
          !identical(file.info(routed[[i]])$size, file.info(target)$size)) {
        stop("Activated historical feature is missing or size-mismatched: ",
             routed[[i]], " -> ", target, call. = FALSE)
      }
      routed[[i]] <- normalizePath(target, winslash = "/", mustWork = TRUE)
    }
  }
  if (anyDuplicated(routed)) stop("Historical feature routing produced duplicate paths.")
  routed
}

# Only the current five-minute social producer participates in this migration.
# Other resolution runs retain their historical paths and provenance.
mmm_social_network_resolution_root <- function(resolution,
                                               project_root = mmm_project_root()) {
  if (length(resolution) == 0L || anyNA(resolution) ||
      any(!resolution %in% c("10sec_based", "1min_based", "5min_based",
                             "10min_based", "30min_based"))) {
    stop("Unknown social-network resolution: ", paste(resolution, collapse = ", "),
         call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "5min_based")) {
      mmm_behavior_output_active_root("social_networks_5min", project_root)
    } else {
      mmm_behavior_historical_resolution_root("social_networks", one, project_root)
    }
  }, character(1), USE.NAMES = FALSE)
}

# Stage 07 currently writes 10-minute features. The separate 30-minute tree
# remains at its recorded location for Stage 15's historical optional input.
mmm_gamm_features_resolution_root <- function(resolution,
                                              project_root = mmm_project_root()) {
  if (length(resolution) == 0L || anyNA(resolution) ||
      any(!resolution %in% c("10sec_based", "1min_based", "5min_based",
                             "10min_based", "30min_based"))) {
    stop("Unknown GAMM-feature resolution: ", paste(resolution, collapse = ", "),
         call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "10min_based")) {
      mmm_behavior_output_active_root("gamm_features_10min", project_root)
    } else {
      mmm_behavior_historical_resolution_root("gamm_features", one, project_root)
    }
  }, character(1), USE.NAMES = FALSE)
}

# Stage 05's declared five-minute output moves independently of older
# one- and ten-minute state-space trees.
mmm_state_space_resolution_root <- function(resolution,
                                            project_root = mmm_project_root()) {
  if (length(resolution) == 0L || anyNA(resolution) ||
      any(!resolution %in% c("10sec_based", "1min_based", "5min_based",
                             "10min_based", "30min_based"))) {
    stop("Unknown state-space resolution: ", paste(resolution, collapse = ", "),
         call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "5min_based")) {
      mmm_behavior_output_active_root("state_space_5min", project_root)
    } else {
      mmm_behavior_historical_resolution_root("state_space", one, project_root)
    }
  }, character(1), USE.NAMES = FALSE)
}

# The declared HMM primary and sensitivity resolutions activate as a pair.
# Other opt-in resolutions remain at their historical paths.
mmm_hmm_resolution_root <- function(resolution,
                                    project_root = mmm_project_root()) {
  if (length(resolution) == 0L || anyNA(resolution) ||
      any(!resolution %in% c("10sec_based", "1min_based", "5min_based",
                             "10min_based", "30min_based"))) {
    stop("Unknown HMM resolution: ", paste(resolution, collapse = ", "),
         call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "10min_based")) {
      mmm_behavior_output_active_root("hmm_states_10min", project_root)
    } else if (identical(one, "5min_based")) {
      mmm_behavior_output_active_root("hmm_states_5min", project_root)
    } else {
      file.path(project_root, "analysis_ready", "06_behavioral_dynamics",
                "hmm_states", one)
    }
  }, character(1), USE.NAMES = FALSE)
}

# Stage 04 currently produces 10-second outputs; older one- and five-minute
# trees remain at their recorded locations.
mmm_temporal_instability_resolution_root <- function(resolution,
                                                     project_root = mmm_project_root()) {
  if (length(resolution) == 0L || anyNA(resolution) ||
      any(!resolution %in% c("10sec_based", "1min_based", "5min_based",
                             "10min_based", "30min_based"))) {
    stop("Unknown temporal-instability resolution: ",
         paste(resolution, collapse = ", "), call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "10sec_based")) {
      mmm_behavior_output_active_root("temporal_instability_10sec", project_root)
    } else {
      mmm_behavior_historical_resolution_root("temporal_instability", one, project_root)
    }
  }, character(1), USE.NAMES = FALSE)
}

# Stages 11-13 currently produce ten-minute results. Their older five-minute
# branches are read from the retained original root, which follows its
# archive receipt; a writer there is refused by the numbered-root guard.
mmm_phase_analysis_resolution_root <- function(analysis, resolution,
                                               project_root = mmm_project_root()) {
  old_roots <- c(adaptation_kinetics = "15_behavioral_adaptation_kinetics",
                 sleep_like_inactivity = "16_sleep_like_inactivity_metrics",
                 phase_organization = "17_ethological_phase_organization")
  if (length(analysis) != 1L || is.na(analysis) ||
      !analysis %in% names(old_roots) || length(resolution) == 0L ||
      anyNA(resolution) || any(!resolution %in% c("5min_based", "10min_based"))) {
    stop("Unknown phase-analysis group or resolution.", call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "10min_based")) {
      mmm_behavior_output_active_root(paste0(analysis, "_10min"), project_root)
    } else {
      file.path(mmm_behavior_numbered_source_root(old_roots[[analysis]], project_root), one)
    }
  }, character(1), USE.NAMES = FALSE)
}

# The two manual supporting analyses declare five-minute outputs. Selecting a
# single receipt-controlled root prevents Stage 10 from scanning both copies.
mmm_supporting_resolution_root <- function(analysis, resolution,
                                           project_root = mmm_project_root()) {
  old_roots <- c(nonlinear_dynamics = "13_nonlinear_systems_dynamics",
                 systems_phenotyping = "14_nextgen_behavioral_phenotyping")
  group_names <- c(nonlinear_dynamics = "nonlinear_dynamics_5min",
                   systems_phenotyping = "systems_phenotyping_5min")
  allowed <- c("10sec_based", "1min_based", "5min_based",
               "10min_based", "30min_based")
  if (length(analysis) != 1L || is.na(analysis) ||
      !analysis %in% names(old_roots) || length(resolution) == 0L ||
      anyNA(resolution) || any(!resolution %in% allowed)) {
    stop("Unknown supporting analysis or resolution.", call. = FALSE)
  }
  vapply(resolution, function(one) {
    if (identical(one, "5min_based")) {
      mmm_behavior_output_active_root(group_names[[analysis]], project_root)
    } else {
      # No other resolution exists; the retained root keeps a reader on the
      # original lineage and lets the guard refuse a writer there.
      file.path(mmm_behavior_numbered_source_root(old_roots[[analysis]], project_root), one)
    }
  }, character(1), USE.NAMES = FALSE)
}

mmm_behavior_proteomics_base_dir <- function(project_root = mmm_project_root()) {
  groups <- c("proteomics_mnn_primary", "proteomics_mnn_sensitivity")
  mmm_behavior_output_assert_uniform_layout(groups, project_root,
                                            "Stage 15 behavior-proteomics")
  layout <- mmm_behavior_output_layout_state(groups[[1]], project_root)
  if (identical(layout, "semantic")) {
    return(file.path(project_root, "analysis_ready", "analyses/behavior_proteomics"))
  }
  dirname(mmm_behavior_retained_source_root(groups[[1]], project_root))
}

mmm_behavior_output_assert_uniform_layout <- function(groups,
                                                       project_root = mmm_project_root(),
                                                       producer = "Producer") {
  if (length(groups) == 0L || anyDuplicated(groups)) {
    stop("Output layout guard needs distinct groups.", call. = FALSE)
  }
  states <- vapply(groups, mmm_behavior_output_layout_state, character(1),
                   project_root = project_root)
  if (length(unique(states)) != 1L) {
    stop(producer, " output migration is partly activated; complete its group set ",
         "before rerunning the producer: ",
         paste(paste(names(states), states, sep = "="), collapse = ", "),
         call. = FALSE)
  }
  invisible(states[[1]])
}

mmm_behavior_output_index_entry <- function(group, project_root = mmm_project_root()) {
  state <- mmm_behavior_output_layout_state(group, project_root)
  ready_root <- normalizePath(file.path(project_root, "analysis_ready"),
                              winslash = "/", mustWork = FALSE)
  relative <- function(layout) {
    path <- normalizePath(mmm_behavior_output_group_root(group, layout, project_root),
                          winslash = "/", mustWork = FALSE)
    prefix <- paste0(ready_root, "/")
    if (!startsWith(path, prefix)) {
      stop("Output group escaped analysis_ready: ", group, call. = FALSE)
    }
    paste0("analysis_ready/", substring(path, nchar(prefix) + 1L), "/")
  }
  list(canonical_path = if (identical(state, "semantic")) relative("semantic")
       else NA_character_,
       legacy_path = relative("current"),
       status = if (identical(state, "semantic")) "migrated_source_retained"
                else "legacy_pending_migration")
}

.mmm_require_pipeline_setup <- function() {
  needed <- c("behavior_stage_dir", "behavior_stage_tables",
              "behavior_manuscript_dir", "behavior_analysis_ready_dir")
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function",
                            inherits = TRUE)]
  if (length(missing) > 0L) {
    stop("Functions/project_paths.R requires Analysis/_pipeline_setup.R to be ",
         "sourced first (missing: ", paste(missing, collapse = ", "), ").",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Configured publication-output root for the behavior main figure.
#'
#' Defaults to the current pipeline layout but is overridable, so the whole
#' publication tree can be relocated without touching any assembler source.
#'
#' Deliberately NOT resolution-scoped: a manuscript figure is not "a 10-min
#' analysis". The resolution of each contributing source travels in the
#' manifests instead (see mmm_path_describe()).
mmm_publication_root <- function(project_root = mmm_project_root(),
                                 stage_id = "27",
                                 stage_name = "behavior_main_figure") {
  configured <- .mmm_configured("mmm.publication_root", "MMM_PUBLICATION_ROOT", "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  .mmm_require_pipeline_setup()
  normalizePath(behavior_stage_dir(project_root, stage_id, stage_name),
                winslash = "/", mustWork = FALSE)
}


#' Semantic publication subdirectories.
#'
#' Manuscript figures, panel candidates, source data, legends, audit records and
#' manifests are conceptually distinct products. Keeping the mapping in one
#' named vector means a future restructure edits this vector, not the assembler.
# Note on brevity: these slugs are deliberately short. Stage 27 also renders
# into candidate variant roots such as
#   .../27_behavior_main_figure/candidates/<variant_slug>/
# and the combined path has to stay inside the 240-character budget enforced by
# mmm_assert_publication_path_budget(). On 2026-09-21 "figures/panel_candidates"
# pushed six outputs to 243 characters and Stage 27 hard-stopped; shortening the
# two slugs below reclaimed 10 and 11 characters respectively. Lengthen them
# again only after checking the longest candidate-variant path still fits.
MMM_PUBLICATION_SUBDIRS <- c(
  figures_main      = "figures/main",
  figures_panels    = "figures/panels",
  figures_ed        = "figures/extended_data",
  figures_previews  = "figures/previews",
  tables_manuscript = "tables/manuscript",
  source_data       = "source_data",
  legends           = "legends",
  audit             = "audit",
  manifests         = "manifests"
)

#' Resolve one semantic publication subdirectory, optionally creating it.
mmm_publication_dir <- function(kind, create = FALSE,
                                publication_root = mmm_publication_root()) {
  if (length(kind) != 1L || !kind %in% names(MMM_PUBLICATION_SUBDIRS)) {
    stop("Unknown publication subdirectory '", paste(kind, collapse = ", "),
         "'. Known kinds: ",
         paste(names(MMM_PUBLICATION_SUBDIRS), collapse = ", "), call. = FALSE)
  }
  path <- file.path(publication_root, MMM_PUBLICATION_SUBDIRS[[kind]])
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

# ------------------------------------------------------------------ registry
#
# Each entry declares the SEMANTIC meaning of a canonical analysis product plus
# enough provenance to fill a manifest row without the consumer knowing any
# directory name. `dir` is a function of the analysis-data root, so relocating a
# producer means editing exactly one closure here.
#
# `files` is a named vector: the name is the semantic role, the value the
# canonical filename. A key with one file resolves to a bare path; a key with
# several resolves to a named vector, or to one path when `file =` is supplied.

#' Root of the upstream SIS endpoint sources (outside the RFID analysis tree).
#'
#' The composite outcome and the phenotype classification lists live one level up
#' from the RFID analysis root, in the SIS analysis folder. Configurable for the
#' same reason as every other root.
mmm_endpoint_source_root <- function(project_root = mmm_project_root()) {
  configured <- .mmm_configured("mmm.endpoint_source_root",
                                "MMM_ENDPOINT_SOURCE_ROOT", "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  # .../Analysis/Behavior/RFID -> .../Analysis
  normalizePath(dirname(dirname(project_root)), winslash = "/", mustWork = FALSE)
}

.mmm_path_specs <- function() {
  list(
    # ------------------------------------------------- upstream endpoint source
    "behavior.combz_upstream_workbook" = list(
      description = paste(
        "UPSTREAM, NOT PRODUCED BY THIS REPOSITORY. The hand-maintained SIS",
        "endpoint workbook that supplies the six standardized outcome",
        "components, plus the two phenotype-classification identifier lists.",
        "Sheet 'zScore' is the ONLY canonical composite; sheets 'combZScore'",
        "and 'CombZScore_noBatch' are noncanonical alternatives and must never",
        "be read. See docs/COMBZ_CANONICAL_DEFINITION.md."),
      producer_stage = "external",
      producer_script = "none - hand-maintained Excel workbook",
      resolution = "per animal; the composite has no time resolution",
      analysis_role = "upstream endpoint source (external dependency)",
      dir = function(root) file.path(mmm_endpoint_source_root(root)),
      files = c(workbook = "SIS_Analysis/E9_Behavior_Data.xlsx",
                susceptible_ids = "sus_animals.csv",
                control_ids = "con_animals.csv")
    ),

    "behavior.later_outcome_combz" = list(
      description = paste(
        "CANONICAL in-repository later composite stress-burden score (CombZ):",
        "animal-level components, the composite, and the derived later outcome",
        "group, together with the component definition, the classification",
        "thresholds and the workbook parity audit. Produced by",
        "Analysis/build_later_outcome_combz.R, which reproduces the workbook",
        "endpoint exactly and refuses to run if parity fails."),
      producer_stage = "canonical-endpoint",
      producer_script = "Analysis/build_later_outcome_combz.R",
      resolution = "per animal; the composite has no time resolution",
      analysis_role = "CANONICAL later outcome definition",
      dir = function(root) file.path(behavior_analysis_ready_dir(root),
                                     "canonical", "later_outcome_combz", "tables"),
      files = c(animal_level = "later_outcome_combz_animal_level.csv",
                component_definition = "combz_component_definition.csv",
                classification_thresholds = "combz_classification_thresholds.csv",
                parity_audit = "combz_workbook_parity_audit.csv",
                alternative_composite_audit = "combz_alternative_composite_audit.csv")
    ),

    # ---------------------------------------------------------------- panel A
    "behavior.combz_definition" = list(
      description = paste(
        "Canonical manuscript-facing animal-level outcome package: later CombZ",
        "per animal, its endpoint-derived CON/RES/SUS label, and the frozen",
        "primary result rows. Assembly-only stage; nothing is refitted there."),
      producer_stage = "16",
      producer_script = "Analysis/16_manuscript_behavior_report.R",
      resolution = "10min",
      analysis_role = "manuscript source-data package (INFRASTRUCTURE)",
      dir = function(root) behavior_manuscript_dir(root, "behavior"),
      files = c(animal_level = "animal_level_source_data.csv",
                primary_results = "primary_results.csv",
                provenance = "provenance.csv",
                validation = "validation.csv",
                manifest = "manifest.csv")
    ),

    # ---------------------------------------------------------------- panel B
    "behavior.rfid_domain_summary" = list(
      description = paste(
        "Canonical first-night multiscale DOMAIN-level group contrasts: five",
        "displayed behavioural domains x three contrasts x two sexes, with",
        "Hedges g and a declared BH family per sex. Descriptive",
        "characterisation; Stage 09 owns the predictive question."),
      producer_stage = "14",
      producer_script = paste(
        "Functions/first_night_domain_driver.R via",
        "Analysis/14_systems_neuroscience_summary_dashboard.R"),
      resolution = "10min",
      analysis_role = "secondary descriptive characterisation",
      dir = function(root) mmm_behavior_output_active_root(
        "first_night_10min", project_root = root),
      files = c(group_contrasts = "first_night_group_contrasts.csv")
    ),

    "behavior.rfid_domain_summary_broad" = list(
      description = paste(
        "ALTERNATIVE broad domain map: seven domains x Active/Inactive x two",
        "sexes x three contrasts, from a mixed model with an animal random",
        "intercept. The only artifact that can fill a phase-resolved",
        "one-row-per-domain heatmap, but it is in NO manuscript registry row",
        "and NO release-plan entry, it still displays an HMM-derived domain,",
        "all of its FDR-supported cells are Inactive (which",
        "docs/KNOWN_LIMITATIONS.md item 3 forbids interpreting as biology),",
        "its domain scores are built with na.rm and coalesce-to-zero, and it",
        "ships no confidence intervals. Extended-data candidate only; see",
        "docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md."),
      producer_stage = "14",
      producer_script = paste(
        "Functions/hmm_stage14_helpers.R via",
        "Analysis/14_systems_neuroscience_summary_dashboard.R"),
      resolution = "MISLABELLED UPSTREAM: the file's resolution column reads 10min_based for all rows but describes only the HMM contributor; six of seven domains come from the 5min backbone",
      analysis_role = "exploratory/secondary; not registry-declared",
      dir = function(root) file.path(
        mmm_behavior_output_active_root("systems_dashboard_5min", root),
        "stats_tables"),
      files = c(domain_effects = "systems_sis_domain_effect_summary.csv")
    ),

    # -------------------------------------------------------- panels C and D
    "behavior.first_night_movement" = list(
      description = paste(
        "Canonical Stage 09 animal-level first-active-window feature table,",
        "one row per animal, plus the frozen window contract that defines the",
        "window identity and the descriptive CON/RES/SUS group contrasts."),
      producer_stage = "09",
      producer_script = "Analysis/09_early_prediction_model_ladder.R",
      resolution = "10min",
      analysis_role = "primary prospective feature source",
      dir = function(root) behavior_stage_tables(root, "09", "early_prediction",
                                                 "10min"),
      files = c(model_input = "model_ladder_input.csv",
                features_wide = "early_behavior_features_wide.csv",
                window_contract = "early_window_contract_summary.csv",
                window_design = "early_window_design_by_animal.csv",
                group_summary = "primary_feature_group_summary.csv",
                group_contrasts_descriptive =
                  "primary_feature_group_contrasts_descriptive.csv"),
      stage09_resolver = TRUE
    ),

    "behavior.combz_components" = list(
      description = paste(
        "Canonical table that names the CombZ component measures alongside the",
        "composite, so a schematic can list the components without any script",
        "hard-coding them. CombZ itself is constructed in an upstream endpoint",
        "workbook, not in this repository."),
      producer_stage = "14",
      producer_script = "Analysis/14_systems_neuroscience_summary_dashboard.R",
      resolution = "5min backbone; CombZ itself has no resolution",
      analysis_role = "endpoint-association source data",
      dir = function(root) file.path(
        mmm_behavior_output_active_root("systems_dashboard_5min", root),
        "tables"),
      files = c(component_scores = "systems_sis_first_active_12h_domain_scores.csv")
    ),

    "behavior.first_night_combz_association" = list(
      description = paste(
        "Canonical Stage 09 continuous prospective association between each of",
        "the three fixed a priori first-night features and later CombZ:",
        "Spearman rank correlation with bootstrap CI, BH-adjusted within the",
        "three-association family."),
      producer_stage = "09",
      producer_script = "Analysis/09_early_prediction_model_ladder.R",
      resolution = "10min",
      analysis_role = "PRIMARY prospective association",
      dir = function(root) behavior_stage_tables(root, "09", "early_prediction",
                                                 "10min"),
      files = c(associations = "primary_movement_entropyacf1_associations.csv",
                sex_interactions = "primary_feature_sex_interactions.csv"),
      stage09_resolver = TRUE
    ),

    # ---------------------------------------------------------------- panel E
    "behavior.early_prediction" = list(
      description = paste(
        "Canonical Stage 09 out-of-sample prediction of later CombZ from",
        "first-night behaviour: leave-one-animal-out performance, the",
        "full-refit outcome permutation reference, and the fixed model",
        "registry."),
      producer_stage = "09",
      producer_script = "Analysis/09_early_prediction_model_ladder.R",
      resolution = "10min",
      analysis_role = "PRIMARY prospective prediction",
      dir = function(root) behavior_stage_tables(root, "09", "early_prediction",
                                                 "10min"),
      files = c(performance = "primary_prediction_performance.csv",
                permutation = "primary_prediction_permutation_test.csv",
                # The per-permutation statistics themselves, so a figure can
                # show the REAL null distribution instead of reconstructing a
                # density from published quantiles. One observed row plus
                # n_permutations null rows per model.
                permutation_draws = "early_prediction_permutation_draws.csv",
                predictions = "primary_prediction_predictions.csv",
                model_registry = "primary_prediction_model_registry.csv"),
      stage09_resolver = TRUE
    ),

    "behavior.early_prediction_heldout" = list(
      description = paste(
        "Canonical manuscript-facing held-out predictions: one row per animal",
        "per fixed model, observed versus leave-one-animal-out predicted",
        "CombZ. Every predicted value was generated with that animal absent",
        "from the corresponding training fold."),
      producer_stage = "16",
      producer_script = "Analysis/16_manuscript_behavior_report.R",
      resolution = "10min",
      analysis_role = "manuscript source-data package (INFRASTRUCTURE)",
      dir = function(root) behavior_manuscript_dir(root, "behavior"),
      files = c(predictions = "prediction_source_data.csv")
    ),

    # ---------------------------------------- temporal panels for Stage 27 candidates
    "behavior.first_active_trajectory" = list(
      description = paste(
        "Canonical Stage 20 first-active 12 h GAMM outputs used by the",
        "assembly-only five-panel behavior-figure candidate. The primary",
        "prediction grid is already fitted upstream; group-aware candidates",
        "must plot those stored Sex x Group trajectories directly."),
      producer_stage = "20",
      producer_script = "Analysis/20_first_night_gamm.R",
      resolution = "10min",
      analysis_role = "PRIMARY first-active temporal characterisation",
      dir = function(root) behavior_stage_tables(root, "20",
                                                 "first_night_gamm", "10min"),
      files = c(
        trajectory_predictions = "first_active_trajectory_predictions.csv",
        primary_contrasts = "first_active_primary_contrasts.csv",
        model_specification = "first_active_model_specification.csv")
    ),

    "behavior.repeated_acute_movement" = list(
      description = paste(
        "Canonical Stage 22 repeated acute Active-window outputs used by the",
        "assembly-only five-panel behavior-figure candidate: animal-level",
        "empirical points and the precomputed overall-population CC4-CC1",
        "estimand, with the phenotype-dependent null retained as context."),
      producer_stage = "22",
      producer_script = "Analysis/22_repeated_cagechange_acute_gamm.R",
      resolution = "10min",
      analysis_role = "SECONDARY repeated acute-response characterisation",
      dir = function(root) behavior_stage_tables(
        root, "22", "repeated_cagechange_acute_gamm", "10min"),
      files = c(
        empirical_auc = "allcc_active_animal_empirical_auc.csv",
        adaptation_registry = "allcc_active_adaptation_multiplicity_registry.csv",
        sex_moderation = "allcc_active_sex_moderation_contrasts.csv",
        model_specification = "allcc_active_model_specification.csv")
    ),

    # -------------------------------------------------- context / cross-links
    "behavior.longitudinal_movement" = list(
      description = paste(
        "Canonical Stage 03 animal-level raw movement endpoints across cage",
        "change x light/dark phase cells. Secondary descriptive longitudinal",
        "characterisation, offered as an alternative breadth panel."),
      producer_stage = "03",
      producer_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      resolution = "10min",
      analysis_role = "secondary descriptive characterisation",
      dir = function(root) behavior_stage_tables(root, "03",
                                                 "movement_phase_stats", "10min"),
      files = c(animal_endpoints = "raw_movement_animal_level_endpoints.csv")
    ),

    "gamm.manuscript_outputs" = list(
      description = paste(
        "Stage 26 GAMM manuscript-assembly claim trace. Read only to",
        "cross-reference the temporal-characterisation claims and to inherit",
        "the declared ownership of the prospective claim; never re-assembled",
        "here."),
      producer_stage = "26",
      producer_script = "Analysis/26_build_gamm_manuscript_outputs.R",
      resolution = "10min",
      analysis_role = "sibling manuscript assembler",
      dir = function(root) file.path(
        behavior_stage_dir(root, "26", "gamm_manuscript_outputs", "10min"),
        "audit"),
      files = c(claim_trace = "gamm_claim_to_analysis_trace.csv")
    )
  )
}

#' Every semantic key known to the registry.
mmm_path_keys <- function() names(.mmm_path_specs())

.mmm_spec <- function(key) {
  specs <- .mmm_path_specs()
  if (length(key) != 1L || !key %in% names(specs)) {
    stop("Unknown semantic path key '", paste(key, collapse = ", "),
         "'. Known keys:\n- ", paste(names(specs), collapse = "\n- "),
         call. = FALSE)
  }
  specs[[key]]
}

#' Resolve a canonical analysis input by SEMANTIC KEY.
#'
#' @param key one of mmm_path_keys()
#' @param file optional semantic file role within the key
#' @param required error if the resolved file does not exist
#' @param root configured analysis-data root
#' @return one absolute path, or a named character vector when the key holds
#'   several files and `file` is not supplied
mmm_path_get <- function(key, file = NULL, required = TRUE,
                         root = mmm_project_root()) {
  .mmm_require_pipeline_setup()
  spec <- .mmm_spec(key)
  roles <- names(spec$files)

  if (!is.null(file)) {
    unknown <- setdiff(file, roles)
    if (length(unknown) > 0L) {
      stop("Key '", key, "' has no file role '", paste(unknown, collapse = ", "),
           "'. Known roles: ", paste(roles, collapse = ", "), call. = FALSE)
    }
    roles <- file
  }

  resolved <- vapply(roles, function(role) {
    filename <- spec$files[[role]]
    if (isTRUE(spec$stage09_resolver) &&
        exists("resolve_stage09_early_prediction_artifact", mode = "function",
               inherits = TRUE)) {
      # Reuse the existing canonical-then-legacy Stage 09 resolver rather than
      # duplicating its precedence rules here.
      hit <- resolve_stage09_early_prediction_artifact(
        root, filename, resolutions = "10min", required = required)
      return(normalizePath(hit$path, winslash = "/", mustWork = FALSE))
    }
    path <- file.path(spec$dir(root), filename)
    if (isTRUE(required) && !file.exists(path)) {
      stop("Missing canonical input for key '", key, "' (role '", role, "'):\n  ",
           path, "\nThis is a broken producer contract, not something a ",
           "manuscript assembler may recompute. Re-run or fix stage ",
           spec$producer_stage, " (", spec$producer_script, ").", call. = FALSE)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1))

  if (length(resolved) == 1L) unname(resolved) else resolved
}

#' SHA-256 of a file, matching Analysis/build_publication_release.R.
mmm_file_sha256 <- function(path) {
  vapply(path, function(p) {
    if (is.na(p) || !file.exists(p)) return(NA_character_)
    if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
    digest::digest(p, algo = "sha256", file = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

#' Provenance table for the registry: one row per semantic key x file role.
#'
#' Manifests should be built from this, so recorded provenance can never
#' disagree with what was actually read.
mmm_path_describe <- function(keys = mmm_path_keys(), root = mmm_project_root(),
                              hash = FALSE) {
  .mmm_require_pipeline_setup()
  rows <- lapply(keys, function(key) {
    spec <- .mmm_spec(key)
    paths <- mmm_path_get(key, required = FALSE, root = root)
    if (is.null(names(paths))) names(paths) <- names(spec$files)
    data.frame(
      path_key = key,
      file_role = names(paths),
      canonical_file = unname(spec$files[names(paths)]),
      resolved_path = unname(paths),
      exists = file.exists(unname(paths)),
      producer_stage = spec$producer_stage,
      producer_script = spec$producer_script,
      source_resolution = spec$resolution,
      analysis_role = spec$analysis_role,
      description = spec$description,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (isTRUE(hash)) out$sha256 <- mmm_file_sha256(out$resolved_path)
  rownames(out) <- NULL
  out
}

#' Assert an intended publication tree fits the repository path-length budget.
#'
#' Wraps the existing mmm_assert_output_path_budget() so the manuscript tree is
#' held to the same MAX_PATH discipline as every analysis output tree.
mmm_assert_publication_path_budget <- function(paths,
                                               source_label = "publication tree") {
  if (exists("mmm_assert_output_path_budget", mode = "function", inherits = TRUE)) {
    return(mmm_assert_output_path_budget(paths, source_label = source_label))
  }
  if (any(nchar(paths) > 240L)) {
    stop(source_label, ": path exceeds the 240-character budget.", call. = FALSE)
  }
  invisible(TRUE)
}
