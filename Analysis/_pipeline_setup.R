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

# ------------------------------------------------------------------
# Stage 14 upstream artifact registry (Stage 04 / Stage 09 inputs)
# ------------------------------------------------------------------
# Single source of truth for resolving the manuscript-relevant Stage 04
# (temporal instability) and Stage 09 (early prediction) artifacts that
# Stage 14 imports. The same resolved object these functions return must be
# used both to load the data and to report status in any dependency/
# integration audit, so the audit can never disagree with what was actually
# loaded. Deliberately free of any Stage-14-specific globals (project_root,
# domain_bin_preference(), etc.) so they can be sourced and unit-tested with
# tempdir() fixtures without executing the full dashboard script.
#
# `resolution` is a bin-level preference order such as
# c("5min_based", "10min_based", ...) (Stage 14's domain_bin_preference()
# convention, with the "_based" suffix). behavior_stage_dir()/
# behavior_stage_tables() are only ever called with a single scalar
# resolution per attempt, never a vector.
#
# Source-class precedence is global: ANY canonical path at ANY acceptable
# resolution beats ANY legacy path at ANY resolution. Resolution preference
# order only breaks ties *within* a class. Concretely:
#   1. build canonical candidates for every resolution, in preference order;
#   2. select the first one that exists;
#   3. only if none of them exist, build legacy candidates for every
#      resolution, in preference order, and select the first one that exists;
#   4. otherwise fail/mark missing per `required`.
# A resolution-by-resolution loop that returns as soon as ANY match (canonical
# or legacy) is found at that resolution is WRONG: it lets a legacy hit at a
# higher-preference resolution beat a canonical hit at a lower-preference one.

.resolve_stage_artifact_across_resolutions <- function(filename,
                                                        resolutions,
                                                        candidate_fn,
                                                        required,
                                                        stage_label) {
  resolutions <- resolutions[!is.na(resolutions) & nzchar(resolutions)]

  candidate_rows <- lapply(resolutions, function(resolution) {
    candidates <- candidate_fn(resolution)
    list(
      resolution = resolution,
      canonical = candidates$canonical,
      legacy = candidates$legacy[!is.na(candidates$legacy) & nzchar(candidates$legacy)]
    )
  })

  canonical_paths <- vapply(candidate_rows, `[[`, character(1), "canonical")
  canonical_resolutions <- vapply(candidate_rows, `[[`, character(1), "resolution")
  all_legacy_paths <- unique(unlist(lapply(candidate_rows, `[[`, "legacy")))
  all_tried <- unique(c(canonical_paths, all_legacy_paths))

  # Pass 1: the whole canonical class, in resolution-preference order.
  canonical_hit_idx <- which(file.exists(canonical_paths))
  if (length(canonical_hit_idx) > 0) {
    idx <- canonical_hit_idx[[1]]
    return(list(
      path = canonical_paths[[idx]],
      resolution = "canonical",
      exists = TRUE,
      canonical_path = canonical_paths[[idx]],
      legacy_path = paste(all_legacy_paths, collapse = "; "),
      resolution_bin_level = canonical_resolutions[[idx]],
      tried = all_tried
    ))
  }

  # Pass 2: no canonical candidate existed anywhere; now search the legacy
  # class, in resolution-preference order.
  for (row in candidate_rows) {
    legacy_hits <- row$legacy[file.exists(row$legacy)]
    if (length(legacy_hits) > 0) {
      selected <- legacy_hits[[1]]
      warning(
        stage_label, ": using documented legacy fallback path for '", filename,
        "' at resolution ", row$resolution, ": ", selected,
        ". No canonical path existed at any acceptable resolution.",
        call. = FALSE
      )
      return(list(
        path = selected,
        resolution = "legacy_fallback",
        exists = TRUE,
        canonical_path = if (length(canonical_paths) > 0) canonical_paths[[1]] else NA_character_,
        legacy_path = selected,
        resolution_bin_level = row$resolution,
        tried = all_tried
      ))
    }
  }

  # Nothing found anywhere, in either class.
  if (isTRUE(required)) {
    stop(
      "Missing required ", stage_label, " artifact '", filename,
      "'. Tried resolutions in preference order (", paste(resolutions, collapse = ", "),
      "), canonical class first, then legacy:\n- ",
      paste(all_tried, collapse = "\n- "),
      call. = FALSE
    )
  }
  list(
    path = if (length(all_tried) > 0) all_tried[[1]] else NA_character_,
    resolution = "missing",
    exists = FALSE,
    canonical_path = if (length(canonical_paths) > 0) canonical_paths[[1]] else NA_character_,
    legacy_path = NA_character_,
    resolution_bin_level = NA_character_,
    tried = all_tried
  )
}

# Stage 04 (temporal instability) has not been migrated to the canonical
# analysis_ready/pipeline/ layout in this pass; its only real output location
# is analysis_ready/06_behavioral_dynamics/temporal_instability/<resolution>/
# tables/. There is no legacy fallback because that is already its one true
# location (the historical ".../burstiness/..." path some consumers guessed
# was never actually written by any version of Stage 04).
resolve_stage04_temporal_instability_artifact <- function(base_dir,
                                                           filename,
                                                           resolutions,
                                                           required = FALSE) {
  .resolve_stage_artifact_across_resolutions(
    filename = filename,
    resolutions = resolutions,
    required = required,
    stage_label = "Stage 04 (temporal instability)",
    candidate_fn = function(resolution) {
      list(
        canonical = file.path(
          behavior_analysis_ready_dir(base_dir), "06_behavioral_dynamics", "temporal_instability",
          resolution, "tables", filename
        ),
        legacy = character()
      )
    }
  )
}

# Stage 09 (early prediction) canonical location is
# analysis_ready/pipeline/09_early_prediction/<resolution>/tables/; the
# documented pre-migration location was
# analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/<resolution>/tables/
# (bin resolution folder name used verbatim, e.g. "10min_based"). A stale
# legacy file must never win over a fresh canonical one at the same
# resolution, which resolve_behavior_artifact() already guarantees by trying
# the canonical path first.
resolve_stage09_early_prediction_artifact <- function(base_dir,
                                                       filename,
                                                       resolutions,
                                                       required = FALSE) {
  .resolve_stage_artifact_across_resolutions(
    filename = filename,
    resolutions = resolutions,
    required = required,
    stage_label = "Stage 09 (early prediction)",
    candidate_fn = function(resolution) {
      list(
        canonical = file.path(behavior_stage_tables(base_dir, "09", "early_prediction", resolution), filename),
        legacy = file.path(
          behavior_analysis_ready_dir(base_dir), "06_behavioral_dynamics", "early_prediction_model_ladder",
          resolution, "tables", filename
        )
      )
    }
  )
}
