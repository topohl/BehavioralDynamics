# Stage 16 must not silently substitute divergent historical Stage 03 tables.
source("Analysis/_pipeline_setup.R")

lines <- readLines("Analysis/16_manuscript_behavior_report.R", warn = FALSE)
stage03 <- grep('^  "s03_', lines, value = TRUE)
stopifnot(length(stage03) == 8L,
          !any(grepl("legacy_stage03_dir", lines, fixed = TRUE)),
          all(grepl('\\.csv"\\), NA_character_,', stage03)))

root <- file.path(tempdir(), "stage16_stage03_canonical_only")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
canonical <- file.path(root, "canonical.csv")
historical <- file.path(root, "historical.csv")
writeLines("old", historical)

required_error <- tryCatch({
  resolve_behavior_artifact(canonical, NA_character_, required = TRUE,
                            source_id = "s03_animal_endpoints")
  NULL
}, error = conditionMessage)
stopifnot(is.character(required_error),
          grepl("s03_animal_endpoints", required_error, fixed = TRUE),
          grepl(canonical, required_error, fixed = TRUE),
          !grepl(historical, required_error, fixed = TRUE))

optional <- resolve_behavior_artifact(canonical, NA_character_, required = FALSE,
                                      source_id = "s03_pairwise")
stopifnot(identical(optional$resolution, "missing_optional"),
          !optional$exists)

writeLines("current", canonical)
selected <- resolve_behavior_artifact(canonical, NA_character_, required = TRUE,
                                      source_id = "s03_animal_endpoints")
stopifnot(identical(selected$path, canonical),
          identical(selected$resolution, "canonical"))

cat("Stage 16 Stage 03 canonical-only source contract: PASS\n")
