# Shared setup for MMMSociability analysis scripts.
# It should only locate the repo and source helpers.

find_mmm_repo_root <- function(start = getwd()) {
  candidates <- unique(c(
    normalizePath(start, winslash = "/", mustWork = FALSE),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  ))

  script_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
  if (!is.na(script_file)) {
    candidates <- unique(c(dirname(dirname(script_file)), dirname(script_file), candidates))
  }

  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", file_arg[grepl("^--file=", file_arg)])
  if (length(file_arg) > 0) {
    candidates <- unique(c(dirname(dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))), candidates))
  }

  for (candidate in candidates) {
    current <- candidate
    repeat {
      if (file.exists(file.path(current, "Functions", "behavioral_dynamics_helpers.R")) &&
          dir.exists(file.path(current, "Analysis"))) {
        return(normalizePath(current, winslash = "/", mustWork = FALSE))
      }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
  }

  stop("Could not locate MMMSociability repo root from: ", paste(candidates, collapse = "; "), call. = FALSE)
}

MMM_REPO_ROOT <- find_mmm_repo_root()
MMM_ANALYSIS_DIR <- file.path(MMM_REPO_ROOT, "Analysis")

source_mmm_helper <- function(helper_file, required = TRUE) {
  helper_path <- file.path(MMM_REPO_ROOT, "Functions", helper_file)
  if (file.exists(helper_path)) {
    source(helper_path)
    return(invisible(helper_path))
  }
  if (isTRUE(required)) {
    stop("Missing MMMSociability helper: ", helper_path, call. = FALSE)
  }
  invisible(NA_character_)
}

source_mmm_helper("behavioral_dynamics_helpers.R")
