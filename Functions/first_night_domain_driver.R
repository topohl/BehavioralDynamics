# ================================================================
# Canonical first-night analysis driver
# MMMSociability
# ================================================================
# Single entry point used by Stage 14 for BOTH first-night resolutions.
# Formula, window, scaling, completeness and inference contracts all live in
# Functions/first_night_domain_helpers.R; this file only orchestrates them and
# writes the resolution-scoped artifacts.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr); library(tibble)
})

if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
  source_mmm_helper("first_night_domain_helpers.R")
}

#' Build the complete first-night domain analysis for ONE bin level.
#'
#' Loads its own Stage 01 input for the requested resolution: the 10-min result
#' is never derived by rebinning a 5-min object.
build_first_night_domain_analysis <- function(bin_level,
                                              project_root,
                                              output_dir,
                                              canonical_roster,
                                              resolution_role = "primary",
                                              displayed_domains = MMM_FIRST_NIGHT_DISPLAYED_DOMAINS,
                                              audit_only_domains = MMM_FIRST_NIGHT_AUDIT_ONLY_DOMAINS) {
  bin_size_sec <- switch(bin_level,
    "1min_based" = 60, "5min_based" = 300, "10min_based" = 600, "30min_based" = 1800,
    stop("Unsupported first-night bin level: ", bin_level, call. = FALSE))
  ensure_dir(output_dir)

  # (1) Stage 01 input for THIS resolution.
  input_file <- file.path(project_root, "analysis_ready/03_derived_metrics", bin_level,
                          "all_behavior_metrics.csv")
  if (!file.exists(input_file)) {
    stop("First-night Stage 01 input is missing for ", bin_level, ": ", input_file, call. = FALSE)
  }
  raw <- read_csv(input_file,
    col_types = cols(AnimalNum = col_character(), BinStart = col_datetime(), .default = col_guess()),
    progress = FALSE)

  # (2) Canonical identity, fail closed.
  raw <- raw %>% mutate(AnimalNum = canonical_animal_id(.data$AnimalNum))
  identity_audit <- audit_hmm_identity(raw, canonical_roster,
                                       paste0("First-night Stage 01 input ", bin_level))
  assert_hmm_identity_audit(identity_audit)
  dat <- identity_audit$data
  proximity_col <- if ("ProximityFraction" %in% names(dat)) "ProximityFraction" else "Proximity"

  # (3-5) First cage change + exact Active phase + clock window: one source of truth.
  selected <- mmm_select_first_night_window(dat, bin_size_sec = bin_size_sec)
  window_qc <- mmm_first_night_window_qc(selected)

  window_contract <- tibble(
    bin_level = bin_level, resolution_role = resolution_role, bin_size_sec = bin_size_sec,
    window_definition = selected$window_definition[1],
    window_hours = MMM_FIRST_NIGHT_WINDOW_HOURS,
    first_cage_change = selected$first_cage_change[1],
    phase_rule = paste0("exact membership in c(",
                        paste(MMM_ACTIVE_PHASE_VALUES, collapse = "/"), ")"),
    anchor_rule = "per-session min(animalpos_phase_block_index(BinStart)); start = block*43200 + 23400",
    anchor_clock_start = paste(sort(unique(format(selected$target_window_start, "%H:%M"))), collapse = "|"),
    anchor_clock_end = paste(sort(unique(format(selected$target_window_end, "%H:%M"))), collapse = "|"),
    n_sessions = n_distinct(format(selected$target_window_start)),
    expected_slots = selected$expected_slots[1],
    n_selected_rows = nrow(selected),
    n_animals = n_distinct(selected$AnimalNum),
    n_inactive_rows_selected = sum(mmm_is_inactive_phase(selected$Phase)),
    median_observed_slots = stats::median(window_qc$observed_slots),
    min_coverage_fraction = min(window_qc$coverage_fraction),
    mean_coverage_fraction = mean(window_qc$coverage_fraction),
    n_animals_complete = sum(window_qc$window_complete),
    total_missing_leading_slots = sum(window_qc$missing_leading_slots),
    total_missing_interior_slots = sum(window_qc$missing_interior_slots),
    total_missing_trailing_slots = sum(window_qc$missing_trailing_slots),
    max_internal_gap_slots = max(window_qc$max_internal_gap_slots),
    proximity_input = proximity_col,
    source_table = input_file,
    source_script = "Analysis/14_systems_neuroscience_summary_dashboard.R",
    selector = "Functions/first_night_window_helpers.R :: mmm_select_first_night_window",
    selector_parity = "Testing/tests/test_first_night_window_parity.R asserts equality with Stage 09",
    resolution_rationale = paste0(
      "10 min is primary for THIS panel so its temporal resolution and clock window match ",
      "canonical Stage 09, which owns the first-12-h question. Not a claim that these Stage 14 ",
      "domain endpoints were historically prespecified at 10 min; Stage 14's global 5-min ",
      "backbone is unchanged.")
  )
  if (window_contract$n_inactive_rows_selected != 0L) {
    stop("First-night window selected Inactive rows for ", bin_level, ".", call. = FALSE)
  }
  if (any(selected$elapsed_sec_in_window >= MMM_FIRST_NIGHT_WINDOW_HOURS * 3600) ||
      any(selected$elapsed_sec_in_window < 0)) {
    stop("First-night window contains rows outside [0, 12 h) for ", bin_level, ".", call. = FALSE)
  }

  # (6) Animal-level raw features, adjacency-aware.
  feat <- selected %>%
    mutate(.prox = .data[[proximity_col]]) %>%
    group_by(.data$AnimalNum, .data$Group, .data$Sex) %>%
    arrange(.data$target_slot, .by_group = TRUE) %>%
    summarise(
      expected_slots = dplyr::first(.data$expected_slots),
      observed_slots = n_distinct(.data$target_slot),
      max_internal_gap_slots = { s <- sort(unique(.data$target_slot))
        if (length(s) < 2L) 0L else as.integer(max(diff(s)) - 1L) },
      Movement_mean = mean(.data$Movement, na.rm = TRUE),
      Movement_rmssd = mmm_rmssd_adjacent(.data$Movement, .data$target_slot),
      Movement_acf1 = mmm_acf1_adjacent(.data$Movement, .data$target_slot),
      Entropy_mean = mean(.data$Entropy, na.rm = TRUE),
      Entropy_rmssd = mmm_rmssd_adjacent(.data$Entropy, .data$target_slot),
      Entropy_acf1 = mmm_acf1_adjacent(.data$Entropy, .data$target_slot),
      Proximity_mean = mean(.data$.prox, na.rm = TRUE),
      Proximity_rmssd = mmm_rmssd_adjacent(.data$.prox, .data$target_slot),
      Proximity_acf1 = mmm_acf1_adjacent(.data$.prox, .data$target_slot),
      n_adjacent_pairs_Movement = mmm_n_adjacent_pairs(.data$Movement, .data$target_slot),
      n_adjacent_pairs_Entropy = mmm_n_adjacent_pairs(.data$Entropy, .data$target_slot),
      n_adjacent_pairs_Proximity = mmm_n_adjacent_pairs(.data$.prox, .data$target_slot),
      .groups = "drop"
    ) %>%
    mutate(across(all_of(MMM_FIRST_NIGHT_RAW_FEATURES), ~ifelse(is.finite(.x), .x, NA_real_)),
           coverage_fraction = .data$observed_slots / .data$expected_slots,
           bin_level = bin_level, resolution_role = resolution_role, bin_size_sec = bin_size_sec,
           min_pairs_rmssd = MMM_FIRST_NIGHT_MIN_PAIRS_RMSSD,
           min_pairs_acf1 = MMM_FIRST_NIGHT_MIN_PAIRS_ACF1,
           adjacency_rule = paste0("RMSSD/ACF1 use only pairs with diff(target_slot) == 1 and both ",
                                   "values finite; missing bins are never bridged or interpolated"))

  # (7) Within-Sex standardization.
  std <- mmm_first_night_standardize_within_sex(feat, MMM_FIRST_NIGHT_RAW_FEATURES)
  z <- std$scaled

  # (8-9) Exact formulas, strict contributor completeness.
  all_domains <- c(displayed_domains, audit_only_domains)
  scored <- map_dfr(all_domains, ~mmm_first_night_score_domain(z, .x)) %>%
    mutate(bin_level = bin_level, resolution_role = resolution_role,
           displayed = .data$Domain %in% displayed_domains,
           candidate_status = if_else(.data$Domain %in% displayed_domains, "displayed_primary",
                                      "audit_only_pruned_for_redundancy"),
           cage_change = window_contract$first_cage_change,
           phase_window = window_contract$window_definition,
           standardization = "z within Sex, computed inside this resolution's first-night dataset",
           feature_origin = "raw_RFID", aggregation_level = "one value per animal",
           source_table = input_file,
           source_script = "Analysis/14_systems_neuroscience_summary_dashboard.R")
  contributor_qc <- scored %>%
    select(any_of(c("bin_level", "AnimalNum", "Group", "Sex", "Domain",
                    "required_contributor_count", "available_contributor_count",
                    "complete_contributors", "missing_contributors", "DomainScore",
                    "score_formula", "contributor_policy")))

  # (10) Inference.
  inf <- map(displayed_domains, ~mmm_first_night_domain_inference(scored, .x))
  contrasts <- map_dfr(inf, "contrasts")
  interactions <- map_dfr(inf, "interaction")
  mmm_assert_effect_sign_agreement(contrasts, label = paste0("first-night ", bin_level))

  # (11) FDR with explicit declared family sizes.
  n_disp <- length(displayed_domains)
  contrasts <- contrasts %>%
    group_by(.data$Sex) %>%
    mutate(family_id = paste0("FIRST_NIGHT__", .data$Sex, "__DISPLAYED_", n_disp, "x3"),
           n_tests_in_family = n_disp * 3L,
           q = mmm_first_night_bh(.data$raw_p, n_disp * 3L, dplyr::first(.data$family_id))) %>%
    ungroup() %>%
    mutate(bin_level = bin_level, resolution_role = resolution_role,
           inferential_unit = "one animal = one value; lm, no random effect",
           interpretation_guard = paste0(
             "Descriptive association with LATER CombZ-derived phenotype labels; not prospective ",
             "validation. Stage 09 owns the predictive question."))
  interactions <- interactions %>%
    mutate(family_id = paste0("FIRST_NIGHT__GROUP_X_SEX__DISPLAYED_", n_disp),
           n_tests_in_family = n_disp,
           q = mmm_first_night_bh(.data$raw_p, n_disp,
                                  paste0("FIRST_NIGHT__GROUP_X_SEX__DISPLAYED_", n_disp)),
           bin_level = bin_level, resolution_role = resolution_role,
           multiplicity_treatment = paste0("BH across ", n_disp,
                                           " Group:Sex tests (one per displayed domain)"),
           standardization_caveat = paste0(
             "Contributors are z-scored WITHIN SEX, so interaction effects are expressed in each ",
             "sex's own standardized units."),
           sex_differential_language_supported = is.finite(.data$q) & .data$q < 0.05)

  # Transparency sensitivity over all eight raw-RFID candidates considered before pruning.
  n_all <- length(all_domains)
  full_family <- map_dfr(map(all_domains, ~mmm_first_night_domain_inference(scored, .x)), "contrasts") %>%
    group_by(.data$Sex) %>%
    mutate(family_id = paste0("FIRST_NIGHT_TRANSPARENCY__", .data$Sex, "__ALL_RAW_", n_all, "x3"),
           n_tests_in_family = n_all * 3L,
           q = mmm_first_night_bh(.data$raw_p, n_all * 3L, dplyr::first(.data$family_id))) %>%
    ungroup() %>%
    mutate(bin_level = bin_level, resolution_role = resolution_role,
           family_role = "transparency_sensitivity_not_primary",
           note = paste0("All ", n_all, " raw-RFID candidate domains considered before the documented ",
                         "redundancy/construct audit. Does NOT determine the displayed rows. No HMM ",
                         "construct is reintroduced to enlarge this family."))

  w <- function(x, nm) { write_csv(x, file.path(output_dir, nm)); nm }
  written <- c(
    w(window_contract, "first_night_window_contract.csv"),
    w(window_qc %>% mutate(bin_level = bin_level), "first_night_animal_window_qc.csv"),
    w(feat, "first_night_raw_features.csv"),
    w(std$parameters %>% mutate(bin_level = bin_level,
        standardization = "z within Sex; a zero-variance contributor yields NA, never 0"),
      "first_night_feature_standardization_parameters.csv"),
    w(scored, "first_night_domain_scores.csv"),
    w(contributor_qc, "first_night_domain_contributor_qc.csv"),
    w(contrasts, "first_night_group_contrasts.csv"),
    w(interactions, "first_night_group_sex_interactions.csv"),
    w(bind_rows(
        contrasts %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
          mutate(family_role = "PRIMARY", scope = "displayed domains x 3 contrasts, within Sex"),
        interactions %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
          mutate(family_role = "INTERACTION", scope = "one Group:Sex test per displayed domain"),
        full_family %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
          mutate(family_role = "TRANSPARENCY_SENSITIVITY",
                 scope = "all raw candidates x 3 contrasts, within Sex")) %>%
        mutate(bin_level = bin_level, method = "Benjamini-Hochberg with explicit declared n"),
      "first_night_multiplicity_contract.csv"),
    w(full_family, "first_night_full_raw_candidate_family_sensitivity.csv")
  )

  message("First-night analysis complete: ", bin_level, " (", resolution_role, "); ",
          nrow(selected), " rows, ", n_distinct(selected$AnimalNum), " animals, ",
          n_disp, " displayed domains")
  list(bin_level = bin_level, resolution_role = resolution_role,
       window_contract = window_contract, window_qc = window_qc, raw_features = feat,
       standardization = std$parameters, scores = scored, contributor_qc = contributor_qc,
       contrasts = contrasts, interactions = interactions, full_family = full_family,
       identity_audit = identity_audit, outputs_written = written)
}
