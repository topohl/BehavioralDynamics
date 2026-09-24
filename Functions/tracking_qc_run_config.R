# Stage 00 diagnostic runs must use one resolution and a fresh isolated folder.
mmm_tracking_qc_run_config <- function(project_root, metric_root, args) {
  if (length(args) != 2L ||
      sum(grepl("^--input-scale=", args)) != 1L ||
      sum(grepl("^--run-id=", args)) != 1L) {
    stop("Use --input-scale=10sec_based --run-id=<new_name>.", call. = FALSE)
  }
  scale <- sub("^--input-scale=", "", args[grepl("^--input-scale=", args)])
  run_id <- sub("^--run-id=", "", args[grepl("^--run-id=", args)])
  if (!identical(scale, "10sec_based")) {
    stop("Stage 00 row-count thresholds currently have only a 10-second interpretation; use --input-scale=10sec_based.",
         call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", run_id)) {
    stop("--run-id must contain only letters, digits, underscores, and hyphens.",
         call. = FALSE)
  }
  input_file <- file.path(metric_root, scale, "all_behavior_metrics.csv")
  if (!file.exists(input_file)) {
    stop("Required Stage 01 QC input is missing: ", input_file, call. = FALSE)
  }
  output_dir <- file.path(mmm_tracking_qc_new_run_root(project_root), "runs", run_id)
  if (file.exists(output_dir) || dir.exists(output_dir)) {
    stop("QC run directory already exists; choose a fresh --run-id: ", output_dir,
         call. = FALSE)
  }
  list(input_files = input_file, output_dir = output_dir,
       input_scale = scale, run_id = run_id)
}
