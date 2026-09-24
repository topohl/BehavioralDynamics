# Stage 16's canonical Stage 09 manuscript sources have no live legacy tree.
source("Analysis/_pipeline_setup.R")

lines <- readLines("Analysis/16_manuscript_behavior_report.R", warn = FALSE)
stage09 <- grep('^  "s09_', lines, value = TRUE)
stage09 <- stage09[!grepl('^  "s09_sens5_', stage09)]
stopifnot(length(stage09) == 9L,
          !any(grepl("legacy_stage09_dir", lines, fixed = TRUE)),
          all(grepl('\\.csv"\\), NA_character_,', stage09)),
          any(grepl('"canonical_only_legacy_path_absent"', lines, fixed = TRUE)))

root <- file.path(tempdir(), "stage16_stage09_canonical_only")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
canonical <- file.path(root, "canonical.csv")
historical <- file.path(root, "historical.csv")
writeLines("old", historical)

error <- tryCatch({
  resolve_behavior_artifact(canonical, NA_character_, required = TRUE,
                            source_id = "s09_associations")
  NULL
}, error = conditionMessage)
stopifnot(is.character(error),
          grepl(canonical, error, fixed = TRUE),
          !grepl(historical, error, fixed = TRUE))

writeLines("current", canonical)
selected <- resolve_behavior_artifact(canonical, NA_character_, required = TRUE,
                                      source_id = "s09_associations")
stopifnot(identical(selected$path, canonical),
          identical(selected$resolution, "canonical"))

cat("Stage 16 Stage 09 canonical-only source contract: PASS\n")
