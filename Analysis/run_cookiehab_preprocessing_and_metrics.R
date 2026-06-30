# Lightweight runner for cookie habituation preprocessing and metrics.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) file.path(getwd(), "Analysis", "run_cookiehab_preprocessing_and_metrics.R"))
if (is.null(script_file) || is.na(script_file)) script_file <- file.path(getwd(), "Analysis", "run_cookiehab_preprocessing_and_metrics.R")
analysis_dir <- normalizePath(dirname(script_file), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(analysis_dir, "_pipeline_setup.R"))) {
  analysis_dir <- normalizePath(file.path(getwd(), "Analysis"), winslash = "/", mustWork = FALSE)
}

source(file.path(analysis_dir, "_pipeline_setup.R"))

old_wd <- getwd()
setwd(MMM_REPO_ROOT)
on.exit(setwd(old_wd), add = TRUE)

options(
  mmm.dataset_id = "cookiehab",
  mmm.preprocessed_dir = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/cookiehab/preprocessed_data",
  mmm.cookiehab_preprocessed_dir = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/cookiehab/preprocessed_data",
  mmm.derived_metrics_dir = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/cookiehab/analysis_ready/03_derived_metrics",
  mmm.dyadic_contacts_dir = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/cookiehab/analysis_ready/06_behavioral_dynamics/dyadic_contacts"
)

source(file.path(MMM_REPO_ROOT, "Formatting", "01_preprocess_cookiehab_animalpos.R"), local = new.env(parent = .GlobalEnv))
source(file.path(analysis_dir, "01_build_multiscale_behavior_metrics.R"), local = new.env(parent = .GlobalEnv))
source(file.path(analysis_dir, "02_build_dyadic_rfid_contacts.R"), local = new.env(parent = .GlobalEnv))

message("Cookiehab preprocessing and metric runner complete.")
