# ANALYSIS OF ANIMAL POSITIONS - PREPROCESSING
#
# Converts each raw PhenoSys AnimalPos export into a preprocessed session table.
#
# ARCHITECTURE NOTE (2026-08-27)
# -----------------------------
# This entrypoint used to call preprocess_file(), which inserted synthetic rows
# into the position record: one row at every half-hour mark, one at each
# phase boundary minus one second (06:29:59 / 18:29:59) and one at each day
# boundary. Those rows were never observations -- they existed only so that the
# row-sequence-based helpers compute_phase_transitions(), count_phases() and
# compute_day_transitions() would see a row at every aggregation boundary. The
# side effect was that 80,190 fabricated position states (11.9% of all rows)
# entered the analysis record carrying a real AnimalID, System and PositionID,
# indistinguishable downstream from genuine RFID reads.
#
# preprocess_animalpos_file() replaces it. Measurement events and aggregation
# boundaries are now separate concerns: phase, day and half-hour metadata are
# computed from a deterministic clock grid (see animalpos_phase_block_index and
# animalpos_half_hours_elapsed), so no row has to exist at a boundary for the
# boundary to be known. Aggregation windows are intersected with state intervals
# downstream via animalpos_split_intervals().
#
# Verified against the pre-refactor outputs archived under
# RFID_snapshot_pre_stage01_refactor_20260827/pre_refactor_preprocessed:
#   - 0 genuine reads lost, 0 rows invented
#   - Phase / ConsecActive / ConsecInactive / HalfHoursElapsed identical on all
#     592,609 genuine rows
#   - all 80,190 removed rows lie exactly on a boundary instant
# Sub-second timestamp precision is preserved (previously truncated to seconds).
#
# Note:
# - The "raw_data" folder must contain the data files and excluded_animals.csv.

required_packages <- c("readr", "stringr", "dplyr", "tidyr", "lubridate", "tibble",
                       "writexl", "foreach", "doParallel", "parallel",
                       "tidyverse", "jsonlite", "rlang")

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", dependencies = TRUE)
}
pacman::p_load(readr, stringr, dplyr, tidyr, lubridate, tibble,
               writexl, foreach, doParallel, parallel,
               tidyverse, jsonlite, rlang)

repo_dir <- Sys.getenv("MMM_REPO_DIR",
                       unset = "C:/Users/topohl/Documents/GitHub/MMMSociability")
data_dir <- Sys.getenv("MMM_DATA_DIR",
                       unset = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/MMMSociability")
setwd(data_dir)

function_files <- file.path(repo_dir, "Functions",
                            c("E9_SIS_AnimalPos-functions.R",
                              "animalpos_preprocessing_helpers.R"))
stopifnot(all(file.exists(function_files)))
invisible(lapply(function_files, source))

raw_dir    <- file.path(getwd(), "raw_data")
output_dir <- file.path(getwd(), "preprocessed_data")
audit_dir  <- file.path(getwd(), "preprocessed_data", "audit")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(audit_dir,  showWarnings = FALSE, recursive = TRUE)

batches      <- c("B1", "B2", "B3", "B4", "B5", "B6")
cage_changes <- c("CC1", "CC2", "CC3", "CC4")

excluded_animals_path <- file.path(raw_dir, "excluded_animals.csv")
if (!file.exists(excluded_animals_path)) {
  stop("The excluded animals file does not exist at ", excluded_animals_path)
}
excluded_animals <- trimws(readLines(excluded_animals_path))
excluded_animals <- excluded_animals[nzchar(excluded_animals)]
message("Excluding ", length(excluded_animals), " animals.")

jobs <- expand.grid(batch = batches, cage_change = cage_changes,
                    stringsAsFactors = FALSE)

num_cores <- max(1L, parallel::detectCores() - 1L)
cl <- makeCluster(num_cores)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
registerDoParallel(cl)

# Each worker sources the function files itself: preprocess_animalpos_file()
# depends on several helpers that foreach's automatic export cannot discover
# transitively.
summaries <- foreach(i = seq_len(nrow(jobs)),
                     .packages = c("readr", "stringr", "dplyr", "tidyr", "tibble"),
                     .errorhandling = "pass") %dopar% {
  invisible(lapply(function_files, source))
  res <- preprocess_animalpos_file(
    batch        = jobs$batch[i],
    change       = jobs$cage_change[i],
    excl_animals = excluded_animals,
    raw_dir      = raw_dir,
    output_dir   = output_dir
  )
  if (is.null(res)) return(NULL)
  res$data <- NULL
  res
}

stopCluster(cl)

failed <- vapply(summaries, function(x) inherits(x, "error"), logical(1))
if (any(failed)) {
  for (e in summaries[failed]) message("ERROR: ", conditionMessage(e))
  stop(sum(failed), " of ", nrow(jobs), " sessions failed to preprocess.")
}

audit <- dplyr::bind_rows(lapply(summaries[!vapply(summaries, is.null, logical(1))],
                                tibble::as_tibble))
if (nrow(audit) > 0) {
  audit <- audit %>%
    dplyr::rename(cage_change = cage_change) %>%
    dplyr::arrange(batch, cage_change)
  readr::write_csv(audit, file.path(audit_dir, "preprocessing_row_accounting.csv"))
  message("\n=== preprocessing row accounting ===")
  print(as.data.frame(audit))
  message("total genuine rows written: ", sum(audit$n_after_remove_phases))
  message("synthetic rows inserted:    ", sum(audit$n_synthetic_rows),
          "  (must be 0)")
  stopifnot(sum(audit$n_synthetic_rows) == 0L)
}
message("Preprocessed ", nrow(audit), " of ", nrow(jobs), " sessions.")
