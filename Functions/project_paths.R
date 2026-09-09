# ================================================================
# Portable roots and a SEMANTIC path registry
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# Manuscript-assembly code must be able to ask for an input by what it MEANS
# ("the canonical first-night animal-level movement table") instead of by where
# it currently happens to sit ("analysis_ready/pipeline/09_early_prediction/
# 10min/tables/..."). The output tree is expected to be reorganised later; when
# that happens only THIS file should need editing, not every figure script.
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO
#
#   * It does not replace the stage-directory helpers in
#     Analysis/_pipeline_setup.R. behavior_stage_dir(), behavior_stage_tables(),
#     behavior_manuscript_dir(), behavior_analysis_ready_dir() and
#     resolve_stage09_early_prediction_artifact() are REUSED here, never
#     reimplemented. This is a semantic key layer ON TOP OF the existing path
#     layer, not a second path layer.
#   * It does not move, copy or rewrite any existing output.
#   * It does not invent a new configuration convention. It UNIFIES the three
#     that already exist, all of which carry the identical S: default and
#     currently have no cross-fallback at all:
#       1. getOption("mmm.project_root", <default>)
#          -- Analysis/08_hmm_behavioral_states_optional.R:46-49, and the
#             documented convention for portable tests
#             (Testing/tests/test_output_path_length.R:21,
#              test_first_night_window_parity.R:153, docs/REPRODUCIBILITY.md:145)
#       2. Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", unset = <default>)
#          -- Analysis/09_early_prediction_model_ladder.R:53,
#             Analysis/build_publication_release.R:59-64 (also --project-root=),
#             manuscript/Fig1_behavior_candidates/build_fig1_candidates.R:39
#       3. a bare hard-coded literal -- 9 active stages including 14, 15 and the
#          whole GAMM family 20-26
#     Today, setting the option does NOT redirect Stage 09 or the release
#     builder, and setting the environment variable does NOT redirect Stage 08 or
#     Stages 20-26. mmm_project_root() honours BOTH, so new code follows whichever
#     one the caller already uses. Migrating the existing stages onto it is a
#     separate, deliberate change (see docs/FUTURE_REPO_RESTRUCTURE_PLAN.md);
#     nothing here modifies them.
#
# PRECEDENCE for every root: getOption() > Sys.getenv() > documented default.
#
# The maintainer-specific default below is the ONE place it may appear in new
# code. New analysis or manuscript scripts must call the accessors instead of
# repeating the literal; Testing/tests/test_behavior_main_figure_contracts.R
# asserts that.
# ================================================================

# ------------------------------------------------------------------ roots

#' Documented default analysis-data root.
#'
#' Historically every stage script repeated this literal. It is centralised here
#' so a different machine only needs options(mmm.project_root = ...) or the
#' MMM_PROJECT_ROOT environment variable.
MMM_PROJECT_ROOT_DEFAULT <-
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"

.mmm_configured <- function(option_name, env_names, default) {
  from_option <- getOption(option_name, NULL)
  if (!is.null(from_option) && length(from_option) == 1L &&
      !is.na(from_option) && nzchar(from_option)) {
    return(as.character(from_option))
  }
  for (env_name in env_names) {
    from_env <- Sys.getenv(env_name, unset = "")
    if (nzchar(from_env)) return(from_env)
  }
  default
}

#' Environment variables consulted for the analysis-data root, in order.
#'
#' MMM_BEHAVIOR_PROJECT_ROOT comes first because it is the name Stage 09 and the
#' release builder already read; MMM_PROJECT_ROOT is accepted as a shorter alias
#' so a caller who guesses the obvious name is not silently ignored.
MMM_PROJECT_ROOT_ENV_VARS <- c("MMM_BEHAVIOR_PROJECT_ROOT", "MMM_PROJECT_ROOT")

#' Configured analysis-data root, where the statistical stages read and write.
mmm_project_root <- function() {
  root <- .mmm_configured("mmm.project_root", MMM_PROJECT_ROOT_ENV_VARS,
                          MMM_PROJECT_ROOT_DEFAULT)
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

.mmm_require_pipeline_setup <- function() {
  needed <- c("behavior_stage_dir", "behavior_stage_tables",
              "behavior_manuscript_dir", "behavior_analysis_ready_dir")
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function",
                            inherits = TRUE)]
  if (length(missing) > 0L) {
    stop("Functions/project_paths.R requires Analysis/_pipeline_setup.R to be ",
         "sourced first (missing: ", paste(missing, collapse = ", "), ").",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Configured publication-output root for the behavior main figure.
#'
#' Defaults to the current pipeline layout but is overridable, so the whole
#' publication tree can be relocated without touching any assembler source.
#'
#' Deliberately NOT resolution-scoped: a manuscript figure is not "a 10-min
#' analysis". The resolution of each contributing source travels in the
#' manifests instead (see mmm_path_describe()).
mmm_publication_root <- function(project_root = mmm_project_root(),
                                 stage_id = "27",
                                 stage_name = "behavior_main_figure") {
  configured <- .mmm_configured("mmm.publication_root", "MMM_PUBLICATION_ROOT", "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  .mmm_require_pipeline_setup()
  normalizePath(behavior_stage_dir(project_root, stage_id, stage_name),
                winslash = "/", mustWork = FALSE)
}


#' Semantic publication subdirectories.
#'
#' Manuscript figures, panel candidates, source data, legends, audit records and
#' manifests are conceptually distinct products. Keeping the mapping in one
#' named vector means a future restructure edits this vector, not the assembler.
MMM_PUBLICATION_SUBDIRS <- c(
  figures_main      = "figures/main",
  figures_panels    = "figures/panel_candidates",
  figures_ed        = "figures/extended_data_candidates",
  figures_previews  = "figures/previews",
  tables_manuscript = "tables/manuscript",
  source_data       = "source_data",
  legends           = "legends",
  audit             = "audit",
  manifests         = "manifests"
)

#' Resolve one semantic publication subdirectory, optionally creating it.
mmm_publication_dir <- function(kind, create = FALSE,
                                publication_root = mmm_publication_root()) {
  if (length(kind) != 1L || !kind %in% names(MMM_PUBLICATION_SUBDIRS)) {
    stop("Unknown publication subdirectory '", paste(kind, collapse = ", "),
         "'. Known kinds: ",
         paste(names(MMM_PUBLICATION_SUBDIRS), collapse = ", "), call. = FALSE)
  }
  path <- file.path(publication_root, MMM_PUBLICATION_SUBDIRS[[kind]])
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

# ------------------------------------------------------------------ registry
#
# Each entry declares the SEMANTIC meaning of a canonical analysis product plus
# enough provenance to fill a manifest row without the consumer knowing any
# directory name. `dir` is a function of the analysis-data root, so relocating a
# producer means editing exactly one closure here.
#
# `files` is a named vector: the name is the semantic role, the value the
# canonical filename. A key with one file resolves to a bare path; a key with
# several resolves to a named vector, or to one path when `file =` is supplied.

#' Root of the upstream SIS endpoint sources (outside the RFID analysis tree).
#'
#' The composite outcome and the phenotype classification lists live one level up
#' from the RFID analysis root, in the SIS analysis folder. Configurable for the
#' same reason as every other root.
mmm_endpoint_source_root <- function(project_root = mmm_project_root()) {
  configured <- .mmm_configured("mmm.endpoint_source_root",
                                "MMM_ENDPOINT_SOURCE_ROOT", "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = FALSE))
  }
  # .../Analysis/Behavior/RFID -> .../Analysis
  normalizePath(dirname(dirname(project_root)), winslash = "/", mustWork = FALSE)
}

.mmm_path_specs <- function() {
  list(
    # ------------------------------------------------- upstream endpoint source
    "behavior.combz_upstream_workbook" = list(
      description = paste(
        "UPSTREAM, NOT PRODUCED BY THIS REPOSITORY. The hand-maintained SIS",
        "endpoint workbook that supplies the six standardized outcome",
        "components, plus the two phenotype-classification identifier lists.",
        "Sheet 'zScore' is the ONLY canonical composite; sheets 'combZScore'",
        "and 'CombZScore_noBatch' are noncanonical alternatives and must never",
        "be read. See docs/COMBZ_CANONICAL_DEFINITION.md."),
      producer_stage = "external",
      producer_script = "none - hand-maintained Excel workbook",
      resolution = "per animal; the composite has no time resolution",
      analysis_role = "upstream endpoint source (external dependency)",
      dir = function(root) file.path(mmm_endpoint_source_root(root)),
      files = c(workbook = "SIS_Analysis/E9_Behavior_Data.xlsx",
                susceptible_ids = "sus_animals.csv",
                control_ids = "con_animals.csv")
    ),

    "behavior.later_outcome_combz" = list(
      description = paste(
        "CANONICAL in-repository later composite stress-burden score (CombZ):",
        "animal-level components, the composite, and the derived later outcome",
        "group, together with the component definition, the classification",
        "thresholds and the workbook parity audit. Produced by",
        "Analysis/build_later_outcome_combz.R, which reproduces the workbook",
        "endpoint exactly and refuses to run if parity fails."),
      producer_stage = "canonical-endpoint",
      producer_script = "Analysis/build_later_outcome_combz.R",
      resolution = "per animal; the composite has no time resolution",
      analysis_role = "CANONICAL later outcome definition",
      dir = function(root) file.path(behavior_analysis_ready_dir(root),
                                     "canonical", "later_outcome_combz", "tables"),
      files = c(animal_level = "later_outcome_combz_animal_level.csv",
                component_definition = "combz_component_definition.csv",
                classification_thresholds = "combz_classification_thresholds.csv",
                parity_audit = "combz_workbook_parity_audit.csv",
                alternative_composite_audit = "combz_alternative_composite_audit.csv")
    ),

    # ---------------------------------------------------------------- panel A
    "behavior.combz_definition" = list(
      description = paste(
        "Canonical manuscript-facing animal-level outcome package: later CombZ",
        "per animal, its endpoint-derived CON/RES/SUS label, and the frozen",
        "primary result rows. Assembly-only stage; nothing is refitted there."),
      producer_stage = "16",
      producer_script = "Analysis/16_manuscript_behavior_report.R",
      resolution = "10min",
      analysis_role = "manuscript source-data package (INFRASTRUCTURE)",
      dir = function(root) behavior_manuscript_dir(root, "behavior"),
      files = c(animal_level = "animal_level_source_data.csv",
                primary_results = "primary_results.csv",
                provenance = "provenance.csv",
                validation = "validation.csv",
                manifest = "manifest.csv")
    ),

    # ---------------------------------------------------------------- panel B
    "behavior.rfid_domain_summary" = list(
      description = paste(
        "Canonical first-night multiscale DOMAIN-level group contrasts: five",
        "displayed behavioural domains x three contrasts x two sexes, with",
        "Hedges g and a declared BH family per sex. Descriptive",
        "characterisation; Stage 09 owns the predictive question."),
      producer_stage = "14",
      producer_script = paste(
        "Functions/first_night_domain_driver.R via",
        "Analysis/14_systems_neuroscience_summary_dashboard.R"),
      resolution = "10min",
      analysis_role = "secondary descriptive characterisation",
      dir = function(root) file.path(
        behavior_analysis_ready_dir(root), "12_systems_neuroscience_summary",
        "5min_based", "first_night", "10min_based"),
      files = c(group_contrasts = "first_night_group_contrasts.csv")
    ),

    "behavior.rfid_domain_summary_broad" = list(
      description = paste(
        "ALTERNATIVE broad domain map: seven domains x Active/Inactive x two",
        "sexes x three contrasts, from a mixed model with an animal random",
        "intercept. The only artifact that can fill a phase-resolved",
        "one-row-per-domain heatmap, but it is in NO manuscript registry row",
        "and NO release-plan entry, it still displays an HMM-derived domain,",
        "all of its FDR-supported cells are Inactive (which",
        "docs/KNOWN_LIMITATIONS.md item 3 forbids interpreting as biology),",
        "its domain scores are built with na.rm and coalesce-to-zero, and it",
        "ships no confidence intervals. Extended-data candidate only; see",
        "docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md."),
      producer_stage = "14",
      producer_script = paste(
        "Functions/hmm_stage14_helpers.R via",
        "Analysis/14_systems_neuroscience_summary_dashboard.R"),
      resolution = "MISLABELLED UPSTREAM: the file's resolution column reads 10min_based for all rows but describes only the HMM contributor; six of seven domains come from the 5min backbone",
      analysis_role = "exploratory/secondary; not registry-declared",
      dir = function(root) file.path(
        behavior_analysis_ready_dir(root), "12_systems_neuroscience_summary",
        "5min_based", "stats_tables"),
      files = c(domain_effects = "systems_sis_domain_effect_summary.csv")
    ),

    # -------------------------------------------------------- panels C and D
    "behavior.first_night_movement" = list(
      description = paste(
        "Canonical Stage 09 animal-level first-active-window feature table,",
        "one row per animal, plus the frozen window contract that defines the",
        "window identity and the descriptive CON/RES/SUS group contrasts."),
      producer_stage = "09",
      producer_script = "Analysis/09_early_prediction_model_ladder.R",
      resolution = "10min",
      analysis_role = "primary prospective feature source",
      dir = function(root) behavior_stage_tables(root, "09", "early_prediction",
                                                 "10min"),
      files = c(model_input = "model_ladder_input.csv",
                features_wide = "early_behavior_features_wide.csv",
                window_contract = "early_window_contract_summary.csv",
                window_design = "early_window_design_by_animal.csv",
                group_summary = "primary_feature_group_summary.csv",
                group_contrasts_descriptive =
                  "primary_feature_group_contrasts_descriptive.csv"),
      stage09_resolver = TRUE
    ),

    "behavior.combz_components" = list(
      description = paste(
        "Canonical table that names the CombZ component measures alongside the",
        "composite, so a schematic can list the components without any script",
        "hard-coding them. CombZ itself is constructed in an upstream endpoint",
        "workbook, not in this repository."),
      producer_stage = "14",
      producer_script = "Analysis/14_systems_neuroscience_summary_dashboard.R",
      resolution = "5min backbone; CombZ itself has no resolution",
      analysis_role = "endpoint-association source data",
      dir = function(root) file.path(
        behavior_analysis_ready_dir(root), "12_systems_neuroscience_summary",
        "5min_based", "tables"),
      files = c(component_scores = "systems_sis_first_active_12h_domain_scores.csv")
    ),

    "behavior.first_night_combz_association" = list(
      description = paste(
        "Canonical Stage 09 continuous prospective association between each of",
        "the three fixed a priori first-night features and later CombZ:",
        "Spearman rank correlation with bootstrap CI, BH-adjusted within the",
        "three-association family."),
      producer_stage = "09",
      producer_script = "Analysis/09_early_prediction_model_ladder.R",
      resolution = "10min",
      analysis_role = "PRIMARY prospective association",
      dir = function(root) behavior_stage_tables(root, "09", "early_prediction",
                                                 "10min"),
      files = c(associations = "primary_movement_entropyacf1_associations.csv",
                sex_interactions = "primary_feature_sex_interactions.csv"),
      stage09_resolver = TRUE
    ),

    # ---------------------------------------------------------------- panel E
    "behavior.early_prediction" = list(
      description = paste(
        "Canonical Stage 09 out-of-sample prediction of later CombZ from",
        "first-night behaviour: leave-one-animal-out performance, the",
        "full-refit outcome permutation reference, and the fixed model",
        "registry."),
      producer_stage = "09",
      producer_script = "Analysis/09_early_prediction_model_ladder.R",
      resolution = "10min",
      analysis_role = "PRIMARY prospective prediction",
      dir = function(root) behavior_stage_tables(root, "09", "early_prediction",
                                                 "10min"),
      files = c(performance = "primary_prediction_performance.csv",
                permutation = "primary_prediction_permutation_test.csv",
                # The per-permutation statistics themselves, so a figure can
                # show the REAL null distribution instead of reconstructing a
                # density from published quantiles. One observed row plus
                # n_permutations null rows per model.
                permutation_draws = "early_prediction_permutation_draws.csv",
                predictions = "primary_prediction_predictions.csv",
                model_registry = "primary_prediction_model_registry.csv"),
      stage09_resolver = TRUE
    ),

    "behavior.early_prediction_heldout" = list(
      description = paste(
        "Canonical manuscript-facing held-out predictions: one row per animal",
        "per fixed model, observed versus leave-one-animal-out predicted",
        "CombZ. Every predicted value was generated with that animal absent",
        "from the corresponding training fold."),
      producer_stage = "16",
      producer_script = "Analysis/16_manuscript_behavior_report.R",
      resolution = "10min",
      analysis_role = "manuscript source-data package (INFRASTRUCTURE)",
      dir = function(root) behavior_manuscript_dir(root, "behavior"),
      files = c(predictions = "prediction_source_data.csv")
    ),

    # -------------------------------------------------- context / cross-links
    "behavior.longitudinal_movement" = list(
      description = paste(
        "Canonical Stage 03 animal-level raw movement endpoints across cage",
        "change x light/dark phase cells. Secondary descriptive longitudinal",
        "characterisation, offered as an alternative breadth panel."),
      producer_stage = "03",
      producer_script = "Analysis/03_primary_raw_movement_phase_stats.R",
      resolution = "10min",
      analysis_role = "secondary descriptive characterisation",
      dir = function(root) behavior_stage_tables(root, "03",
                                                 "movement_phase_stats", "10min"),
      files = c(animal_endpoints = "raw_movement_animal_level_endpoints.csv")
    ),

    "gamm.manuscript_outputs" = list(
      description = paste(
        "Stage 26 GAMM manuscript-assembly claim trace. Read only to",
        "cross-reference the temporal-characterisation claims and to inherit",
        "the declared ownership of the prospective claim; never re-assembled",
        "here."),
      producer_stage = "26",
      producer_script = "Analysis/26_build_gamm_manuscript_outputs.R",
      resolution = "10min",
      analysis_role = "sibling manuscript assembler",
      dir = function(root) file.path(
        behavior_stage_dir(root, "26", "gamm_manuscript_outputs", "10min"),
        "audit"),
      files = c(claim_trace = "gamm_claim_to_analysis_trace.csv")
    )
  )
}

#' Every semantic key known to the registry.
mmm_path_keys <- function() names(.mmm_path_specs())

.mmm_spec <- function(key) {
  specs <- .mmm_path_specs()
  if (length(key) != 1L || !key %in% names(specs)) {
    stop("Unknown semantic path key '", paste(key, collapse = ", "),
         "'. Known keys:\n- ", paste(names(specs), collapse = "\n- "),
         call. = FALSE)
  }
  specs[[key]]
}

#' Resolve a canonical analysis input by SEMANTIC KEY.
#'
#' @param key one of mmm_path_keys()
#' @param file optional semantic file role within the key
#' @param required error if the resolved file does not exist
#' @param root configured analysis-data root
#' @return one absolute path, or a named character vector when the key holds
#'   several files and `file` is not supplied
mmm_path_get <- function(key, file = NULL, required = TRUE,
                         root = mmm_project_root()) {
  .mmm_require_pipeline_setup()
  spec <- .mmm_spec(key)
  roles <- names(spec$files)

  if (!is.null(file)) {
    unknown <- setdiff(file, roles)
    if (length(unknown) > 0L) {
      stop("Key '", key, "' has no file role '", paste(unknown, collapse = ", "),
           "'. Known roles: ", paste(roles, collapse = ", "), call. = FALSE)
    }
    roles <- file
  }

  resolved <- vapply(roles, function(role) {
    filename <- spec$files[[role]]
    if (isTRUE(spec$stage09_resolver) &&
        exists("resolve_stage09_early_prediction_artifact", mode = "function",
               inherits = TRUE)) {
      # Reuse the existing canonical-then-legacy Stage 09 resolver rather than
      # duplicating its precedence rules here.
      hit <- resolve_stage09_early_prediction_artifact(
        root, filename, resolutions = "10min", required = required)
      return(normalizePath(hit$path, winslash = "/", mustWork = FALSE))
    }
    path <- file.path(spec$dir(root), filename)
    if (isTRUE(required) && !file.exists(path)) {
      stop("Missing canonical input for key '", key, "' (role '", role, "'):\n  ",
           path, "\nThis is a broken producer contract, not something a ",
           "manuscript assembler may recompute. Re-run or fix stage ",
           spec$producer_stage, " (", spec$producer_script, ").", call. = FALSE)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1))

  if (length(resolved) == 1L) unname(resolved) else resolved
}

#' SHA-256 of a file, matching Analysis/build_publication_release.R.
mmm_file_sha256 <- function(path) {
  vapply(path, function(p) {
    if (is.na(p) || !file.exists(p)) return(NA_character_)
    if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
    digest::digest(p, algo = "sha256", file = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

#' Provenance table for the registry: one row per semantic key x file role.
#'
#' Manifests should be built from this, so recorded provenance can never
#' disagree with what was actually read.
mmm_path_describe <- function(keys = mmm_path_keys(), root = mmm_project_root(),
                              hash = FALSE) {
  .mmm_require_pipeline_setup()
  rows <- lapply(keys, function(key) {
    spec <- .mmm_spec(key)
    paths <- mmm_path_get(key, required = FALSE, root = root)
    if (is.null(names(paths))) names(paths) <- names(spec$files)
    data.frame(
      path_key = key,
      file_role = names(paths),
      canonical_file = unname(spec$files[names(paths)]),
      resolved_path = unname(paths),
      exists = file.exists(unname(paths)),
      producer_stage = spec$producer_stage,
      producer_script = spec$producer_script,
      source_resolution = spec$resolution,
      analysis_role = spec$analysis_role,
      description = spec$description,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (isTRUE(hash)) out$sha256 <- mmm_file_sha256(out$resolved_path)
  rownames(out) <- NULL
  out
}

#' Assert an intended publication tree fits the repository path-length budget.
#'
#' Wraps the existing mmm_assert_output_path_budget() so the manuscript tree is
#' held to the same MAX_PATH discipline as every analysis output tree.
mmm_assert_publication_path_budget <- function(paths,
                                               source_label = "publication tree") {
  if (exists("mmm_assert_output_path_budget", mode = "function", inherits = TRUE)) {
    return(mmm_assert_output_path_budget(paths, source_label = source_label))
  }
  if (any(nchar(paths) > 240L)) {
    stop(source_label, ": path exceeds the 240-character budget.", call. = FALSE)
  }
  invisible(TRUE)
}
