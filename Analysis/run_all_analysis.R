# Minimal runner for the staged MMMSociability analysis pipeline.
#
# Nothing runs by default. Choose a named profile or an explicit stage list:
#   options(mmm.pipeline_profile = "legacy_systems")    # 02-07, 10, 11, 13, 15
#   options(mmm.pipeline_profile = "heatmap_inputs")    # 01, 08, 12, 14: the inputs of the Stage 14 heatmaps, then 14
#   options(mmm.pipeline_profile = "early_prediction")  # 09
#   options(mmm.pipeline_stages = c("04", "05"))        # explicit stages instead of a profile
# Stages 01 and 09 rewrite files that the frozen runs pinned by sha256 (Stage 29 v1.0.1 release run_inputs.csv,
# Stage 32 input_hashes.csv). Functions/frozen_input_guard.R refuses to start them on the live project root unless
# options(mmm.allow_pinned_overwrite = TRUE) is set, and after every stage it re-checks the pinned files.
# Stage 14 is rebuilt in a guarded sandbox and promoted, not run on the live root (docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md).

RUN_SYSTEMS_EXTENSION <- isTRUE(getOption("mmm.run_systems_extension", TRUE))
RUN_BEHAVIOR_PROTEOMICS <- isTRUE(getOption("mmm.run_behavior_proteomics", FALSE))
CONTINUE_ON_ERROR <- isTRUE(getOption("mmm.continue_on_error", FALSE))

PIPELINE_PROFILES <- list(
  legacy_systems = c("02", "03", "04", "05", "06", "07", "10", "11", "13", "15"),
  heatmap_inputs = c("01", "08", "12", "14"),
  early_prediction = "09"
)
pipeline_profile <- getOption("mmm.pipeline_profile", NULL)
pipeline_stages <- getOption("mmm.pipeline_stages", NULL)
if (is.null(pipeline_profile) && is.null(pipeline_stages)) {
  stop("run_all_analysis.R runs nothing by default. Set options(mmm.pipeline_profile = ...) (",
       paste(names(PIPELINE_PROFILES), collapse = ", "), ") or options(mmm.pipeline_stages = ...).", call. = FALSE)
}
if (!is.null(pipeline_profile) && !is.null(pipeline_stages)) {
  stop("Set mmm.pipeline_profile or mmm.pipeline_stages, not both.", call. = FALSE)
}
if (!is.null(pipeline_profile) && !pipeline_profile %in% names(PIPELINE_PROFILES)) {
  stop("Unknown pipeline profile '", pipeline_profile, "'. Profiles: ", paste(names(PIPELINE_PROFILES), collapse = ", "),
       call. = FALSE)
}
selected_stages <- if (!is.null(pipeline_profile)) PIPELINE_PROFILES[[pipeline_profile]] else
  formatC(as.integer(pipeline_stages), width = 2, flag = "0")

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) file.path(getwd(), "Analysis", "run_all_analysis.R"))
if (is.null(script_file) || is.na(script_file)) script_file <- file.path(getwd(), "Analysis", "run_all_analysis.R")
analysis_dir <- normalizePath(dirname(script_file), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(analysis_dir, "_pipeline_setup.R"))) {
  analysis_dir <- normalizePath(file.path(getwd(), "Analysis"), winslash = "/", mustWork = FALSE)
}
source(file.path(analysis_dir, "_pipeline_setup.R"))
source_mmm_helper("project_paths.R")
source_mmm_helper("frozen_input_guard.R")

pipeline <- tibble::tribble(
  ~script, ~stage, ~optional_flag, ~role,
  "01_build_multiscale_behavior_metrics.R", "01", NA_character_, "canonical multiscale behavior metrics",
  "02_build_dyadic_rfid_contacts.R", "02", NA_character_, "dyadic RFID contacts for network analyses",
  "03_primary_raw_movement_phase_stats.R", "03", NA_character_, "primary raw movement broad phase statistics",
  "04_temporal_instability.R", "04", NA_character_, "temporal instability and burstiness",
  "05_behavioral_state_space.R", "05", NA_character_, "behavioral state-space features",
  "06_dynamic_social_networks.R", "06", NA_character_, "dynamic social networks",
  "07_gamm_trajectory_features.R", "07", NA_character_, "GAMM trajectory feature extraction",
  "08_hmm_behavioral_states_optional.R", "08", NA_character_, "HMM behavioral states (required by Stage 14; the file name is historical)",
  "09_early_prediction_model_ladder.R", "09", NA_character_, "primary early prediction model ladder",
  "10_systems_feature_prediction_ladder.R", "10", "RUN_SYSTEMS_EXTENSION", "secondary systems-extension prediction ladder",
  "11_behavioral_adaptation_kinetics.R", "11", NA_character_, "adaptation and recovery kinetics",
  "12_sleep_like_quiescence_metrics.R", "12", NA_character_, "sleep-like quiescence metrics",
  "13_ethological_phase_organization.R", "13", NA_character_, "ethological phase organization",
  "14_systems_neuroscience_summary_dashboard.R", "14", NA_character_, "integrated systems neuroscience dashboard",
  "15_behavior_proteomics_integration.R", "15", "RUN_BEHAVIOR_PROTEOMICS", "optional behavior-proteomics integration"
)
unknown <- setdiff(selected_stages, pipeline$stage)
if (length(unknown)) stop("Unknown stage(s): ", paste(unknown, collapse = ", "), call. = FALSE)

flag_enabled <- function(flag) {
  if (is.na(flag)) return(TRUE)
  isTRUE(get(flag, envir = .GlobalEnv, inherits = TRUE))
}

run_script <- function(script) {
  path <- file.path(analysis_dir, script)
  if (!file.exists(path)) stop("Missing pipeline script: ", path, call. = FALSE)
  message("\n=== Running ", script, " ===")
  source(path, local = new.env(parent = .GlobalEnv))
  invisible(path)
}

message("Pipeline ", if (!is.null(pipeline_profile)) paste0("profile '", pipeline_profile, "'") else "stages",
        ": ", paste(selected_stages, collapse = ", "))
# The run is wrapped in local() because a top-level on.exit() in a source()d file fires immediately.
local({
  old_wd <- setwd(MMM_REPO_ROOT)
  on.exit(setwd(old_wd), add = TRUE)
  for (i in seq_len(nrow(pipeline))) {
    row <- pipeline[i, ]
    if (!row$stage %in% selected_stages) next
    if (!flag_enabled(row$optional_flag)) {
      message("Skipping ", row$script, " (", row$optional_flag, " = FALSE)")
      next
    }
    # Stages 01 and 28 guard themselves; every other stage is checked here (Stage 09 is refused on the live root).
    snapshot <- if (row$stage %in% MMM_SELF_GUARDED_STAGES) NULL else mmm_frozen_guard_before(row$stage)
    stage_error <- tryCatch({ run_script(row$script); NULL }, error = function(e) e)
    mmm_frozen_guard_after(snapshot)
    if (!is.null(stage_error)) {
      message("Pipeline stage failed: ", row$script)
      message(conditionMessage(stage_error))
      if (!CONTINUE_ON_ERROR) stop(stage_error)
    }
  }
})

message("\nPipeline complete.")
