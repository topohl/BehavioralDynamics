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

# Stage 00b raw-acquisition QC runs: --run-id=<id> (required; at most 40
# characters), --output-root=<absolute isolated folder> (optional; checked by
# mmm_isolated_output_root_check()), --recordings=<regex on "B3_CC1">
# (only with --output-root) and --resume (continue an existing .tmp_<id>).
# Output goes to <root>/runs/<id>, built in <root>/runs/.tmp_<id>; an id listed
# in <root>/runs_ledger.csv is refused even if its folder is gone.
MMM_QC_RUN_ID_PATTERN <- "^[A-Za-z0-9][A-Za-z0-9_-]{0,39}$"

mmm_raw_acquisition_qc_run_config <- function(project_root, args) {
  usage <- paste("Use --run-id=<new_name> [--output-root=<absolute isolated folder> [--recordings=<regex>]] [--resume].")
  known <- "^--(run-id|output-root|recordings)=|^--resume$"
  if (!length(args) || any(!grepl(known, args))) stop(usage, call. = FALSE)
  keys <- sub("=.*$", "", args)
  if (anyDuplicated(keys)) stop("Each argument may be given once. ", usage, call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  run_id <- val("run-id")
  if (is.na(run_id) || !grepl(MMM_QC_RUN_ID_PATTERN, run_id)) {
    stop("--run-id is required: 1-40 letters, digits, underscores or hyphens. ", usage, call. = FALSE)
  }
  out_root <- val("output-root")
  recordings <- val("recordings")
  isolated <- !is.na(out_root)
  if (!is.na(recordings) && !isolated) stop("--recordings needs --output-root (subset runs never go to the live root).", call. = FALSE)
  if (!is.na(recordings) && (!nzchar(recordings) || inherits(try(grepl(recordings, "B1_CC1"), silent = TRUE), "try-error"))) {
    stop("--recordings must be a valid regular expression.", call. = FALSE)
  }
  base <- if (isolated) mmm_isolated_output_root_check(out_root, project_root) else mmm_raw_acquisition_qc_root(project_root)
  output_dir <- file.path(base, "runs", run_id)
  tmp_dir <- file.path(base, "runs", paste0(".tmp_", run_id))
  resume <- "--resume" %in% args
  if (file.exists(output_dir)) stop("QC run directory already exists; choose a fresh --run-id: ", output_dir, call. = FALSE)
  # a run id is used once, even if its folder was moved away (runs_ledger.csv, Functions/qc_run_protection.R)
  ledger <- file.path(base, "runs_ledger.csv")
  if (file.exists(ledger)) {
    used <- tryCatch(utils::read.csv(ledger, colClasses = "character"), error = function(e) data.frame())
    if (!"run_id" %in% names(used)) stop("Unreadable run ledger (no run_id column): ", ledger, call. = FALSE)
    if (run_id %in% used$run_id) stop("Run id already used in ", ledger, "; choose a fresh --run-id.", call. = FALSE)
  }
  if (file.exists(tmp_dir) && !resume) stop("A partial run exists; inspect it, or continue with --resume: ", tmp_dir, call. = FALSE)
  list(run_id = run_id, output_dir = output_dir, tmp_dir = tmp_dir, isolated = isolated, base = base,
       recordings = recordings, resume = resume, require_clean_code = !isolated, raw_root = mmm_raw_antenna_root(project_root))
}
