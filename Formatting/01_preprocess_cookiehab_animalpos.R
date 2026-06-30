# ================================================================
# Preprocess cookie habituation AnimalPos files
# MMMSociability
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(lubridate)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(dirname(getwd()), "Analysis", "_pipeline_setup.R"),
  file.path(dirname(tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) getwd())), "..", "Analysis", "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("cookiehab_preprocessing_helpers.R")

raw_root <- getOption(
  "mmm.cookiehab_raw_dir",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/MMMdata"
)
output_dir <- getOption(
  "mmm.cookiehab_preprocessed_dir",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/cookiehab/preprocessed_data"
)
qc_dir <- getOption(
  "mmm.cookiehab_qc_dir",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/cookiehab/qc"
)

sus_animals_file <- getOption(
  "mmm.sus_animals_file",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv"
)
con_animals_file <- getOption(
  "mmm.con_animals_file",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv"
)

if (!dir.exists(raw_root)) {
  stop("Cookiehab raw root does not exist: ", raw_root, call. = FALSE)
}

ensure_dir(output_dir)
ensure_dir(qc_dir)

raw_files <- list.files(
  raw_root,
  pattern = "^E9_SIS_B[1-6]_EPMaftercagechange_AnimalPos\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(raw_files) == 0) {
  stop(
    "No cookiehab AnimalPos raw files found under ", raw_root,
    " matching E9_SIS_B[1-6]_EPMaftercagechange_AnimalPos.csv",
    call. = FALSE
  )
}

sus_ids <- cookiehab_read_id_list(sus_animals_file)
con_ids <- cookiehab_read_id_list(con_animals_file)

manifest <- tibble(
  RawFile = normalizePath(raw_files, winslash = "/", mustWork = FALSE),
  SourceFile = basename(raw_files),
  Batch = stringr::str_extract(SourceFile, "B[1-6]"),
  Dataset = "cookiehab",
  RawParadigm = "EPMaftercagechange",
  OutputFile = file.path(
    output_dir,
    stringr::str_replace(SourceFile, "_AnimalPos\\.csv$", "_AnimalPos_cookiehab_preprocessed.csv")
  )
)

message("Found ", nrow(manifest), " cookiehab raw AnimalPos files.")

processed <- purrr::map2(manifest$RawFile, manifest$OutputFile, function(raw_file, output_file) {
  message("Preprocessing ", basename(raw_file), "...")
  dat <- cookiehab_preprocess_one_file(raw_file, sus_ids = sus_ids, con_ids = con_ids)
  readr::write_csv(dat, output_file)
  dat
})

all_preprocessed <- dplyr::bind_rows(processed)

manifest <- manifest |>
  dplyr::left_join(
    all_preprocessed |>
      dplyr::group_by(SourceFile) |>
      dplyr::summarise(
        n_rows = dplyr::n(),
        n_animals = dplyr::n_distinct(AnimalID),
        n_systems = dplyr::n_distinct(System),
        start_time = min(DateTime, na.rm = TRUE),
        end_time = max(DateTime, na.rm = TRUE),
        n_primary_window_rows = sum(CookieWindowPrimary, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "SourceFile"
  )

phase_window_qc <- all_preprocessed |>
  dplyr::group_by(SourceFile, Batch, System, AnimalID, Phase, ConsecActive, ConsecInactive, CookieHabEpoch) |>
  dplyr::summarise(
    start_time = min(DateTime, na.rm = TRUE),
    end_time = max(DateTime, na.rm = TRUE),
    n_rows = dplyr::n(),
    n_primary_window_rows = sum(CookieWindowPrimary, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(SourceFile, System, AnimalID, start_time)

animal_system_qc <- all_preprocessed |>
  dplyr::group_by(SourceFile, Batch, System, AnimalID, AnimalNum, Sex, Group) |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    start_time = min(DateTime, na.rm = TRUE),
    end_time = max(DateTime, na.rm = TRUE),
    n_positions = dplyr::n_distinct(PositionID),
    missing_position_rows = sum(is.na(PositionID)),
    primary_window_rows = sum(CookieWindowPrimary, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(SourceFile, System, AnimalID)

primary_window_coverage_qc <- all_preprocessed |>
  dplyr::filter(CookieWindowPrimary) |>
  dplyr::group_by(SourceFile, Batch, System, AnimalID, Sex, Group) |>
  dplyr::summarise(
    first_primary_time = min(DateTime, na.rm = TRUE),
    last_primary_time = max(DateTime, na.rm = TRUE),
    n_primary_rows = dplyr::n(),
    n_subwindows = dplyr::n_distinct(CookieSubWindow, na.rm = TRUE),
    has_17_anchor = any(MinutesOfDay == 17L * 60L),
    has_1745_anchor = any(MinutesOfDay == 17L * 60L + 45L),
    coverage_minutes = as.numeric(difftime(max(DateTime, na.rm = TRUE), min(DateTime, na.rm = TRUE), units = "mins")),
    .groups = "drop"
  ) |>
  dplyr::arrange(SourceFile, System, AnimalID)

readr::write_csv(manifest, file.path(qc_dir, "cookiehab_preprocessing_file_manifest.csv"))
readr::write_csv(phase_window_qc, file.path(qc_dir, "cookiehab_phase_window_qc.csv"))
readr::write_csv(animal_system_qc, file.path(qc_dir, "cookiehab_animal_system_qc.csv"))
readr::write_csv(primary_window_coverage_qc, file.path(qc_dir, "cookiehab_primary_window_coverage_qc.csv"))

message("Cookiehab preprocessing complete.")
message("Preprocessed files: ", output_dir)
message("QC files: ", qc_dir)
