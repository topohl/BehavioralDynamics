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

# Canonical reporting/output paths for the bounded manuscript-critical migration.
# Other stages retain their historical locations until they are migrated explicitly.
behavior_analysis_ready_dir <- function(base_dir) {
  file.path(base_dir, "analysis_ready")
}

behavior_normalize_resolution <- function(resolution) {
  if (is.null(resolution) || length(resolution) == 0L || is.na(resolution) || !nzchar(resolution)) {
    return(NULL)
  }
  sub("_based$", "", as.character(resolution))
}

behavior_stage_dir <- function(base_dir, stage_id, stage_name, resolution = NULL) {
  stage_root <- file.path(
    behavior_analysis_ready_dir(base_dir),
    "pipeline",
    paste0(stage_id, "_", stage_name)
  )
  resolution <- behavior_normalize_resolution(resolution)
  if (is.null(resolution)) stage_root else file.path(stage_root, resolution)
}

behavior_stage_tables <- function(base_dir, stage_id, stage_name, resolution = NULL) {
  file.path(behavior_stage_dir(base_dir, stage_id, stage_name, resolution), "tables")
}

behavior_stage_figures <- function(base_dir, stage_id, stage_name, resolution = NULL) {
  file.path(behavior_stage_dir(base_dir, stage_id, stage_name, resolution), "figures")
}

behavior_stage_audit <- function(base_dir, stage_id, stage_name, resolution = NULL) {
  file.path(behavior_stage_dir(base_dir, stage_id, stage_name, resolution), "audit")
}

behavior_manuscript_dir <- function(base_dir, domain = "behavior") {
  file.path(behavior_analysis_ready_dir(base_dir), "manuscript", domain)
}

resolve_behavior_artifact <- function(canonical_path,
                                      legacy_paths = character(),
                                      required = TRUE,
                                      source_id = basename(canonical_path)) {
  candidates <- unique(c(canonical_path, legacy_paths))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hits <- candidates[file.exists(candidates)]
  if (length(hits) == 0L) {
    if (isTRUE(required)) {
      stop(
        "Missing required behavioral artifact ", source_id, ". Tried:\n- ",
        paste(candidates, collapse = "\n- "),
        call. = FALSE
      )
    }
    return(list(
      path = canonical_path,
      resolution = "missing_optional",
      exists = FALSE,
      canonical_path = canonical_path,
      legacy_path = paste(legacy_paths, collapse = "; ")
    ))
  }
  selected <- hits[[1]]
  list(
    path = selected,
    resolution = if (identical(selected, canonical_path)) "canonical" else "legacy_fallback",
    exists = TRUE,
    canonical_path = canonical_path,
    legacy_path = paste(legacy_paths, collapse = "; ")
  )
}
