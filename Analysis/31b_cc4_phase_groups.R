# Explicit Stage 31 extension; reads a verified prior run, never raw/preprocessed writes.
source("Analysis/_pipeline_setup.R")
for (helper in c("project_paths.R", "cc4_grid_exposure_helpers.R", "cc4_whole_inactive_helpers.R",
                 "cc4_phase_group_helpers.R")) source_mmm_helper(helper)

run_cc4_phase_groups <- function(source_dir, outcome_file, output_dir) {
  if (file.exists(output_dir)) stop("Output already exists; choose a new run ID.", call. = FALSE)
  manifest <- grid31_verify_manifest(source_dir)
  bin_name <- "tables/animal_clock_profiles_5min.csv"
  if (!bin_name %in% gsub("\\\\", "/", manifest$File)) stop("Clock bins are not manifested.", call. = FALSE)
  paths <- c(file.path(source_dir, bin_name), file.path(source_dir, "audit/output_manifest.csv"), outcome_file,
    file.path(MMM_REPO_ROOT, c("Analysis/31b_cc4_phase_groups.R", "Functions/cc4_phase_group_helpers.R",
      "Functions/cc4_whole_inactive_helpers.R", "Functions/cc4_grid_exposure_helpers.R",
      "Functions/behavioral_dynamics_helpers.R", "Analysis/_pipeline_setup.R")))
  if (!all(file.exists(paths))) stop("Missing phase-group input.", call. = FALSE)
  before <- mmm_file_sha256(paths)
  bins <- readr::read_csv(paths[1], col_types = readr::cols(.default = readr::col_guess(),
    AnimalID = readr::col_character(), AnimalNum = readr::col_character()), show_col_types = FALSE)
  if (nrow(readr::problems(bins))) stop("Clock-bin parse failure.", call. = FALSE)
  outcomes <- readr::read_csv(outcome_file, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  if (nrow(readr::problems(outcomes))) stop("Outcome parse failure.", call. = FALSE)
  result <- grid31_group_phases(bins, outcomes)
  if (!identical(before, mmm_file_sha256(paths))) stop("Input changed during phase-group calculation.", call. = FALSE)
  dir.create(output_dir, recursive = TRUE)
  tables <- list(animal_phase_activity = result$animals, cage_group_phase_activity = result$cages,
    batch_group_phase_activity = result$batch, population = result$population,
    batch_group_contrasts = result$batch_contrasts, group_phase_summary = result$summary,
    group_phase_contrasts = result$contrasts, canonical_outcomes = outcomes,
    input_manifest = tibble::tibble(Path = paths, SHA256 = before))
  for (nm in names(tables)) readr::write_csv(tables[[nm]], file.path(output_dir, paste0(nm, ".csv")), na = "")
  writeLines(c("CC4 whole-phase group comparison: exploratory descriptive candidate.",
    "Common complete cage roster across I2-I5 and A1-A5. I2 inactive and A2 active references; A1 is earlier context.",
    "CON/RES/SUS come from canonical later_outcome_combz; no classification is recomputed.",
    "Within cage/group: equal animals; within batch/group: equal cages; within sex/group: equal batches.",
    "Intervals: pointwise 95% Student t across independent batch summaries, n=3 and df=2 per sex in the real data.",
    "These intervals assume approximately normal batch estimates; only three batches provide limited uncertainty information.",
    "Group differences and differences in phase changes are paired within batch before summary. No p-values or multiplicity claims.",
    "Control has one cage per batch; no within-batch control cage bootstrap is used.",
    "Unknown two-hour grid timings: whole-phase association only, not a timed or causal grid effect.",
    "RFID crossings measure recorded position changes, not distance, sleep or sociability.",
    "RES/SUS are outcome-defined groups; sucrose preference contributes to the outcome classification.",
    "Renderer must consume frozen tables and must not compute or refit scientific estimates."), file.path(output_dir, "README.txt"))
  files <- list.files(output_dir, full.names = TRUE)
  readr::write_csv(tibble::tibble(File = basename(files), SHA256 = mmm_file_sha256(files)), file.path(output_dir, "manifest.csv"))
  invisible(result)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) stop("Usage: Rscript Analysis/31b_cc4_phase_groups.R <Stage31_run> <canonical_outcomes.csv> <new_output_dir>", call. = FALSE)
  run_cc4_phase_groups(args[1], args[2], args[3])
}
