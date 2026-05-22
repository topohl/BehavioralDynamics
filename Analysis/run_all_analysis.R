# Minimal runner for the staged MMMSociability analysis pipeline.

RUN_OPTIONAL_HMM <- isTRUE(getOption("mmm.run_optional_hmm", TRUE))
RUN_SYSTEMS_EXTENSION <- isTRUE(getOption("mmm.run_systems_extension", TRUE))
RUN_BEHAVIOR_PROTEOMICS <- isTRUE(getOption("mmm.run_behavior_proteomics", FALSE))
CONTINUE_ON_ERROR <- isTRUE(getOption("mmm.continue_on_error", FALSE))

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) file.path(getwd(), "Analysis", "run_all_analysis.R"))
if (is.null(script_file) || is.na(script_file)) script_file <- file.path(getwd(), "Analysis", "run_all_analysis.R")
analysis_dir <- normalizePath(dirname(script_file), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(analysis_dir, "_pipeline_setup.R"))) {
  analysis_dir <- normalizePath(file.path(getwd(), "Analysis"), winslash = "/", mustWork = FALSE)
}
source(file.path(analysis_dir, "_pipeline_setup.R"))
old_wd <- getwd()
setwd(MMM_REPO_ROOT)
on.exit(setwd(old_wd), add = TRUE)

pipeline <- tibble::tribble(
  ~script, ~stage, ~optional_flag, ~role,
  "00_qc_tracking_integrity.R", "00", NA_character_, "tracking integrity QC",
  "01_build_multiscale_behavior_metrics.R", "01", NA_character_, "canonical multiscale behavior metrics",
  "02_build_dyadic_rfid_contacts.R", "02", NA_character_, "dyadic RFID contacts for network analyses",
  "03_primary_raw_movement_phase_stats.R", "03", NA_character_, "primary raw movement broad phase statistics",
  "04_temporal_instability.R", "04", NA_character_, "temporal instability and burstiness",
  "05_behavioral_state_space.R", "05", NA_character_, "behavioral state-space features",
  "06_dynamic_social_networks.R", "06", NA_character_, "dynamic social networks",
  "07_gamm_trajectory_features.R", "07", NA_character_, "GAMM trajectory feature extraction",
  "08_hmm_behavioral_states_optional.R", "08", "RUN_OPTIONAL_HMM", "optional HMM behavioral states",
  "09_early_prediction_model_ladder.R", "09", NA_character_, "primary early prediction model ladder",
  "10_systems_feature_prediction_ladder.R", "10", "RUN_SYSTEMS_EXTENSION", "secondary systems-extension prediction ladder",
  "11_behavioral_adaptation_kinetics.R", "11", NA_character_, "adaptation and recovery kinetics",
  "12_sleep_like_quiescence_metrics.R", "12", NA_character_, "sleep-like quiescence metrics",
  "13_ethological_phase_organization.R", "13", NA_character_, "ethological phase organization",
  "14_systems_neuroscience_summary_dashboard.R", "14", NA_character_, "integrated systems neuroscience dashboard",
  "15_behavior_proteomics_integration.R", "15", "RUN_BEHAVIOR_PROTEOMICS", "optional behavior-proteomics integration"
)

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

for (i in seq_len(nrow(pipeline))) {
  row <- pipeline[i, ]
  if (!flag_enabled(row$optional_flag)) {
    message("Skipping ", row$script, " (", row$optional_flag, " = FALSE)")
    next
  }
  tryCatch(
    run_script(row$script),
    error = function(e) {
      message("Pipeline stage failed: ", row$script)
      message(conditionMessage(e))
      if (!CONTINUE_ON_ERROR) stop(e)
    }
  )
}

message("\nPipeline complete.")
