# Focused, non-refitting checks for the behavioral reporting/output contract.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(openxlsx)
})

source("Analysis/_pipeline_setup.R")

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
analysis_ready_dir <- behavior_analysis_ready_dir(base_dir)
manuscript_dir <- behavior_manuscript_dir(base_dir, "behavior")
workbook_path <- file.path(manuscript_dir, "Behavioral_Source_Data.xlsx")

required_artifacts <- file.path(
  manuscript_dir,
  c(
    "Behavioral_Source_Data.xlsx", "primary_results.csv",
    "supplementary_results.csv", "animal_level_source_data.csv",
    "prediction_source_data.csv", "movement_phase_source_data.csv",
    "provenance.csv", "validation.csv", "manifest.csv"
  )
)
stopifnot(all(file.exists(required_artifacts)))

# Canonical path grammar, resolution normalization, and fallback precedence.
stopifnot(
  endsWith(
    gsub("\\\\", "/", behavior_stage_dir(base_dir, "09", "early_prediction", "10min_based")),
    "/analysis_ready/pipeline/09_early_prediction/10min"
  )
)
temp_root <- file.path(tempdir(), paste0("mmm_reporting_contract_", Sys.getpid()))
dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)
canonical_test <- file.path(temp_root, "canonical.csv")
legacy_test <- file.path(temp_root, "legacy.csv")
writeLines("x\n1", legacy_test)
resolved_legacy <- resolve_behavior_artifact(canonical_test, legacy_test, source_id = "test")
stopifnot(identical(resolved_legacy$resolution, "legacy_fallback"))
writeLines("x\n2", canonical_test)
resolved_canonical <- resolve_behavior_artifact(canonical_test, legacy_test, source_id = "test")
stopifnot(identical(resolved_canonical$path, canonical_test))

temp_stage <- file.path(temp_root, "analysis_ready", "pipeline", "09_early_prediction", "10min")
temp_dirs <- analysis_output_dirs(temp_stage)
stopifnot(
  setequal(list.files(temp_stage), c("audit", "figures", "tables")),
  identical(
    gsub("\\\\", "/", canonicalize_behavior_output_path(file.path(temp_stage, "tables", "models", "x.csv"))),
    gsub("\\\\", "/", file.path(temp_stage, "tables", "x.csv"))
  )
)
demo <- tibble(x = 1)
demo_path <- file.path(temp_stage, "tables", "models", "demo.csv")
write_table(demo, demo_path)
write_table(demo, file.path(temp_stage, "tables", "demo.csv"))
stopifnot(file.exists(file.path(temp_dirs$tables, "demo.csv")))

# Navigation, manifest, and compact typed result contracts.
output_index <- read_csv(file.path(analysis_ready_dir, "output_index.csv"), show_col_types = FALSE)
stopifnot(!anyDuplicated(na.omit(output_index$canonical_path)))
provenance <- read_csv(file.path(manuscript_dir, "provenance.csv"), show_col_types = FALSE)
stopifnot(!anyDuplicated(provenance$source_id), all(nzchar(provenance$sha256[provenance$required])))
validation <- read_csv(file.path(manuscript_dir, "validation.csv"), show_col_types = FALSE)
stopifnot(all(validation$status == "PASS"), !anyDuplicated(validation$check_id))
manifest <- read_csv(file.path(manuscript_dir, "manifest.csv"), show_col_types = FALSE)
stopifnot(
  all(manifest$hash_algorithm == "SHA-256"),
  !anyDuplicated(manifest$path),
  setequal(
    manifest$artifact,
    c("workbook", "primary", "supplementary", "animal_source", "prediction_source", "movement_source", "provenance", "validation")
  )
)

expected_result_columns <- c(
  "claim_id", "analysis_domain", "endpoint", "time_window", "biological_unit",
  "n_animals", "sex", "contrast_or_model", "estimate", "effect_size_type",
  "effect_size", "ci_low", "ci_high", "p_raw", "p_adjusted",
  "adjustment_method", "multiplicity_family", "robustness_status",
  "source_id", "source_row_key", "notes"
)
primary <- read_csv(file.path(manuscript_dir, "primary_results.csv"), show_col_types = FALSE)
supplementary <- read_csv(file.path(manuscript_dir, "supplementary_results.csv"), show_col_types = FALSE)
stopifnot(
  identical(names(primary), expected_result_columns),
  identical(names(supplementary), expected_result_columns),
  nrow(primary) == 10L,
  identical(
    primary$source_row_key[primary$analysis_domain == "Prospective feature association"],
    c("Movement_mean", "Movement_rmssd", "Entropy_acf1")
  ),
  !any(grepl("Group", primary$contrast_or_model[grepl("Prospective prediction", primary$analysis_domain)]))
)

# Exact source-data lineage; only selection/renaming is allowed here.
source_path <- function(source_id) {
  rel <- provenance$path[provenance$source_id == source_id]
  stopifnot(length(rel) == 1L)
  file.path(base_dir, sub("^analysis_ready/", "analysis_ready/", rel))
}
model_input <- read_csv(source_path("s09_model_input"), show_col_types = FALSE)
animal_expected <- model_input %>%
  transmute(
    AnimalID = as.character(AnimalNum), Sex = as.character(Sex), Group = as.character(Group),
    CombZ = outcome, Movement_mean, Movement_rmssd, Entropy_acf1,
    early_observation_hours, contains_short_duration_epoch,
    source_id = "s09_model_input"
  ) %>% arrange(AnimalID)
animal_actual <- read_csv(file.path(manuscript_dir, "animal_level_source_data.csv"), show_col_types = FALSE)
stopifnot(isTRUE(all.equal(animal_actual, animal_expected, tolerance = 1e-12, check.attributes = FALSE)))

prediction_raw <- read_csv(source_path("s09_prediction_source"), show_col_types = FALSE)
prediction_expected <- if ("AdjustmentSet" %in% names(prediction_raw)) {
  prediction_raw %>%
    filter(AdjustmentSet == "Behavior only", ModelFamily %in% c("Movement mean", "Primary behavior family")) %>%
    transmute(
      AnimalID = as.character(AnimalNum), observed_CombZ = observed, predicted_CombZ = predicted,
      model_id = recode(ModelFamily, "Movement mean" = "movement_mean", "Primary behavior family" = "primary_behavior_family"),
      validation_scheme = "Leave-one-animal-out", Sex = as.character(Sex), Group = as.character(Group),
      source_id = "s09_prediction_source"
    )
} else {
  prediction_raw %>%
    filter(Model %in% c("movement_mean", "primary_behavior_family")) %>%
    transmute(
      AnimalID = as.character(AnimalNum), observed_CombZ = observed, predicted_CombZ = predicted,
      model_id = Model, validation_scheme = "Leave-one-animal-out",
      Sex = as.character(Sex), Group = as.character(Group), source_id = "s09_prediction_source"
    )
}
prediction_expected <- prediction_expected %>% arrange(model_id, AnimalID)
prediction_actual <- read_csv(file.path(manuscript_dir, "prediction_source_data.csv"), show_col_types = FALSE)
stopifnot(isTRUE(all.equal(prediction_actual, prediction_expected, tolerance = 1e-12, check.attributes = FALSE)))

movement_raw <- read_csv(source_path("s03_animal_endpoints"), show_col_types = FALSE)
movement_expected <- movement_raw %>%
  transmute(
    AnimalID = as.character(AnimalNum), Sex = as.character(Sex), Group = as.character(Group),
    cage_change = as.character(CageChange), phase = as.character(PhaseClass),
    movement_measure = "mean_movement", value = mean_movement, n_bins,
    scope_type = ScopeType, endpoint = Endpoint, source_id = "s03_animal_endpoints"
  ) %>% arrange(scope_type, endpoint, Sex, Group, AnimalID)
movement_actual <- read_csv(file.path(manuscript_dir, "movement_phase_source_data.csv"), show_col_types = FALSE)
stopifnot(isTRUE(all.equal(movement_actual, movement_expected, tolerance = 1e-12, check.attributes = FALSE)))

# Workbook sheet registry and OOXML safety.
expected_sheets <- c(
  "README", "Primary_results", "Primary_source_data", "Prediction_validation",
  "Prediction_source_data", "Supplementary_results", "Movement_phase_source_data",
  "QC", "Feature_dictionary", "Provenance", "Validation"
)
stopifnot(identical(getSheetNames(workbook_path), expected_sheets))
zip_parts <- unzip(workbook_path, list = TRUE)$Name
stopifnot(
  !any(grepl("^xl/drawings/|^xl/externalLinks/|vbaProject|macrosheet", zip_parts, ignore.case = TRUE))
)
worksheet_parts <- grep("^xl/worksheets/sheet[0-9]+[.]xml$", zip_parts, value = TRUE)
for (part in worksheet_parts) {
  con <- unz(workbook_path, part, open = "rb")
  raw <- readBin(con, what = "raw", n = 100000000L)
  close(con)
  xml <- rawToChar(raw)
  stopifnot(!grepl("<f\\b", xml, perl = TRUE), !grepl("<c\\b[^>]*\\bt=\"e\"", xml, perl = TRUE))
}

cat("Reporting architecture checks: PASS\n")
