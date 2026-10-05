# Stage 00 path boundary: a future run cannot overwrite the May QC snapshot,
# and the output index and analysis_ready README keep that snapshot explicitly
# historical. (The Stage 16 package and the release builder, which read two of its
# tables, were retired on 2026-10-05.)
env <- new.env(parent = baseenv())
source("Functions/project_paths.R", local = env)
fixture <- file.path(tempdir(), "tracking_qc_path_fixture")
historical <- file.path(fixture, "analysis_ready", "00_qc_tracking_integrity")
new_run <- file.path(fixture, "analysis_ready", "quality_control",
                     "tracking_integrity")
stopifnot(identical(env$mmm_tracking_qc_historical_root(fixture), historical),
          identical(env$mmm_tracking_qc_new_run_root(fixture), new_run),
          !identical(historical, new_run))

source_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
producer <- source_text("Analysis/00_qc_tracking_integrity.R")
index_source <- source_text("Functions/behavior_output_index.R")
refresher <- source_text("Maintenance/Refresh-BehaviorOutputIndex.R")
readme <- source_text("docs/BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md")
runner <- source_text("Analysis/run_all_analysis.R")
stopifnot(grepl("OUT_DIR <- run_config$output_dir",
               producer, fixed = TRUE),
          grepl('source_mmm_helper("tracking_qc_run_config.R")',
                producer, fixed = TRUE),
          grepl("These derived metrics alone cannot establish chip loss.",
                producer, fixed = TRUE),
          grepl("Any chip-loss or censoring decision requires independent raw-read evidence",
                producer, fixed = TRUE),
          !grepl('OUT_DIR <- file.path(PROJECT_BASE_DIR, "analysis_ready", "00_qc_tracking_integrity")',
                 producer, fixed = TRUE),
          grepl('output_group_index("history_tracking_integrity_10sec")',
                index_source, fixed = TRUE),
          grepl("Current Stage 01 lineage unverified", index_source, fixed = TRUE),
          grepl("BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md", refresher,
                fixed = TRUE),
          grepl("history/tracking_integrity/10sec/", readme, fixed = TRUE),
          grepl("REVIEW_STATUS.md", readme, fixed = TRUE),
          !grepl('"00_qc_tracking_integrity.R", "00"', runner, fixed = TRUE),
          file.exists("docs/BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md"))

source("Functions/tracking_qc_run_config.R", local = env)
metric_root <- file.path(fixture, "analysis_ready", "foundations", "behavior_metrics")
dir.create(file.path(metric_root, "10sec_based"), recursive = TRUE, showWarnings = FALSE)
writeLines("AnimalID,Movement", file.path(metric_root, "10sec_based",
                                    "all_behavior_metrics.csv"))
args <- c("--input-scale=10sec_based", "--run-id=review_01")
cfg <- env$mmm_tracking_qc_run_config(fixture, metric_root, args)
stopifnot(identical(cfg$input_files,
                    file.path(metric_root, "10sec_based", "all_behavior_metrics.csv")),
          identical(cfg$output_dir, file.path(new_run, "runs", "review_01")),
          !dir.exists(cfg$output_dir))
must_error <- function(x) inherits(tryCatch(x, error = identity), "error")
stopifnot(must_error(env$mmm_tracking_qc_run_config(fixture, metric_root, character())),
          must_error(env$mmm_tracking_qc_run_config(
            fixture, metric_root, c("--input-scale=5min_based", "--run-id=review_02"))),
          must_error(env$mmm_tracking_qc_run_config(
            fixture, metric_root, c("--input-scale=10sec_based", "--run-id=../escape"))))
dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(must_error(env$mmm_tracking_qc_run_config(fixture, metric_root, args)))
cat("Tracking QC writer and historical-reader path boundary: PASS\n")
