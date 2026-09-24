# Constrain scientific HMM revalidation writes to a fresh, named run folder.
# Historical September audits remain immutable and are never a default target.
mmm_hmm_revalidation_output_dir <- function(project_root, requested_dir,
                                            expected_files, allowed_existing = character()) {
  if (!is.character(requested_dir) || length(requested_dir) != 1L ||
      is.na(requested_dir) || !nzchar(requested_dir) ||
      !grepl("^[A-Za-z]:[/\\\\]", requested_dir)) {
    stop("Provide one absolute --output-dir for the HMM revalidation run.",
         call. = FALSE)
  }
  if (!is.character(expected_files) || !length(expected_files) ||
      anyNA(expected_files) || any(!nzchar(expected_files)) ||
      any(grepl("[/\\\\]", expected_files)) || anyDuplicated(expected_files) ||
      any(grepl("[/\\\\]", allowed_existing))) {
    stop("HMM revalidation output filenames must be distinct basenames.",
         call. = FALSE)
  }
  run_name <- basename(requested_dir)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", run_name) ||
      run_name %in% c(".", "..")) {
    stop("HMM revalidation run folder needs a simple name.", call. = FALSE)
  }
  root <- normalizePath(file.path(project_root, "analysis_ready", "analyses",
                                  "hmm_revalidation_runs"),
                        winslash = "/", mustWork = FALSE)
  requested <- normalizePath(requested_dir, winslash = "/", mustWork = FALSE)
  if (!identical(tolower(dirname(requested)), tolower(root))) {
    stop("HMM revalidation output must be one run folder under: ", root,
         call. = FALSE)
  }
  if (dir.exists(requested)) {
    existing <- list.files(requested, recursive = TRUE, all.files = TRUE,
                           no.. = TRUE)
    if (any(existing %in% expected_files) ||
        any(!existing %in% allowed_existing)) {
      stop("HMM revalidation output folder contains existing or unexpected files: ",
           requested, call. = FALSE)
    }
  } else if (file.exists(requested)) {
    stop("HMM revalidation output path is a file: ", requested, call. = FALSE)
  }
  requested
}
