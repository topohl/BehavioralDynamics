# ================================================================
# Raw acute-window features and the TWO declared scaling references
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# The first-night and longitudinal analyses answer different questions and
# therefore need different, EXPLICITLY DECLARED scaling references. Putting both
# here keeps the two declarations side by side so neither can be applied by
# accident.
#
#   FIRST NIGHT      z within Sex, computed across ALL animals in the CC1 window.
#                    Never within Group. Parameters are exported.
#
#   LONGITUDINAL     FIXED parameters estimated from the CC1 acute window within
#                    Sex, then applied UNCHANGED to CC1, CC2, CC3 and CC4. Later
#                    values are therefore expressed in units of the INITIAL
#                    perturbation-response distribution, which is what makes a
#                    trajectory interpretable.
#
# WHY NOT THE EXISTING BROAD STAGE 14 NORMALISATION
# Functions/hmm_stage14_helpers.R:18 standardizes within
# Sex x PhaseClass x CageChangeIndex. That removes each cage change's OWN mean and
# SD, which removes exactly the longitudinal signal an adaptation analysis is
# trying to measure: every cage change is forced to mean 0 by construction, so a
# real population-level change across CC1-CC4 becomes invisible. It also zero-fills
# a zero-variance cell (helpers:268 returns rep(0, n)), silently entering a
# constant contributor into the composite at its own mean. Neither behaviour is
# used here. Standardizing within CageChange is never done, and standardizing
# within Group is never done.
#
# TEMPORAL-SCALE CAVEAT
# RMSSD and ACF1 are LAG-IN-BIN quantities. At 5 min they describe a 5-min lag and
# at 10 min a 10-min lag, so the 5-min analysis is a DIFFERENT TEMPORAL CONSTRUCT
# for those six features, not an exact replication of the 10-min one. Movement_mean,
# Entropy_mean and Proximity_mean are lag-free and directly comparable. This string
# ships with every scaling-parameter table.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
  source_mmm_helper("rfid_domain_core.R")
  source_mmm_helper("first_night_domain_helpers.R")
}

MMM_RFID_RESOLUTION_CAVEAT <- paste(
  "RMSSD and ACF1 are lag-in-bin quantities: at 5 min they measure a 5-min lag and",
  "at 10 min a 10-min lag. The 5-min analysis is therefore a different temporal",
  "construct for the six rmssd/acf1 features, NOT an exact replication of the",
  "10-min construct. The three *_mean features are lag-free and directly comparable.")

# ------------------------------------------------------------- raw features

#' Animal x window raw features from selected acute-window bins.
#'
#' RMSSD and ACF1 reuse the audited adjacency-aware estimators from
#' Functions/first_night_domain_helpers.R (mmm_rmssd_adjacent / mmm_acf1_adjacent),
#' which use ONLY pairs with diff(target_slot) == 1 and both values finite. Missing
#' bins are never bridged and never interpolated, and the minimum valid
#' adjacent-pair requirements (2 for RMSSD, 3 for ACF1) are preserved.
#'
#' These estimators are deliberately NOT the ones Stage 09 uses. Stage 09's
#' calc_rmssd/calc_acf1 delete non-finite values and therefore BRIDGE across
#' missing bins, and its ACF1 uses stats::acf() (1/n normalisation against the
#' global series mean) rather than stats::cor(). Unifying them would move published
#' Stage 09 values, so Stage 09 keeps its own implementation untouched and this
#' analysis uses the adjacency-aware one.
#'
#' @param selected acute-window bins with target_slot and expected_slots.
#' @param proximity_col which proximity column to use; ProximityFraction preferred.
#' @param group_keys identity/provenance columns to aggregate within.
mmm_rfid_build_window_features <- function(selected,
                                           proximity_col = NULL,
                                           group_keys = c("AnimalNum", "Group", "Sex",
                                                          "Batch", "System", "SourceFile",
                                                          "CageChangeLabel", "CageChangeIndex")) {
  if (is.null(proximity_col)) {
    proximity_col <- if ("ProximityFraction" %in% names(selected)) "ProximityFraction" else "Proximity"
  }
  for (needed in c("Movement", "Entropy", proximity_col, "target_slot", "expected_slots")) {
    if (!needed %in% names(selected)) {
      stop("mmm_rfid_build_window_features() requires column: ", needed, call. = FALSE)
    }
  }
  keys <- intersect(group_keys, names(selected))

  selected %>%
    mutate(.prox = .data[[proximity_col]]) %>%
    group_by(across(all_of(keys))) %>%
    arrange(.data$target_slot, .by_group = TRUE) %>%
    summarise(
      expected_slots = dplyr::first(.data$expected_slots),
      observed_slots = n_distinct(.data$target_slot),
      max_internal_gap_slots = {
        s <- sort(unique(.data$target_slot))
        if (length(s) < 2L) 0L else as.integer(max(diff(s)) - 1L)
      },
      Movement_mean  = mean(.data$Movement, na.rm = TRUE),
      Movement_rmssd = mmm_rmssd_adjacent(.data$Movement, .data$target_slot),
      Movement_acf1  = mmm_acf1_adjacent(.data$Movement, .data$target_slot),
      Entropy_mean   = mean(.data$Entropy, na.rm = TRUE),
      Entropy_rmssd  = mmm_rmssd_adjacent(.data$Entropy, .data$target_slot),
      Entropy_acf1   = mmm_acf1_adjacent(.data$Entropy, .data$target_slot),
      Proximity_mean  = mean(.data$.prox, na.rm = TRUE),
      Proximity_rmssd = mmm_rmssd_adjacent(.data$.prox, .data$target_slot),
      Proximity_acf1  = mmm_acf1_adjacent(.data$.prox, .data$target_slot),
      n_adjacent_pairs_Movement  = mmm_n_adjacent_pairs(.data$Movement, .data$target_slot),
      n_adjacent_pairs_Entropy   = mmm_n_adjacent_pairs(.data$Entropy, .data$target_slot),
      n_adjacent_pairs_Proximity = mmm_n_adjacent_pairs(.data$.prox, .data$target_slot),
      .groups = "drop"
    ) %>%
    mutate(
      across(all_of(MMM_RFID_RAW_FEATURES), ~ifelse(is.finite(.x), .x, NA_real_)),
      coverage_fraction = .data$observed_slots / .data$expected_slots,
      proximity_input = proximity_col,
      min_pairs_rmssd = MMM_FIRST_NIGHT_MIN_PAIRS_RMSSD,
      min_pairs_acf1 = MMM_FIRST_NIGHT_MIN_PAIRS_ACF1,
      adjacency_rule = paste0(
        "RMSSD/ACF1 use only pairs with diff(target_slot) == 1 and both values ",
        "finite; missing bins are never bridged or interpolated")
    )
}

# ----------------------------------------------------------------- scaling

#' Estimate scaling parameters within Sex from a REFERENCE subset.
#'
#' @param reference rows to estimate mean/sd from. For the longitudinal primary
#'   scaling this is the CC1 acute window ONLY; for the pooled sensitivity it is
#'   all of CC1-CC4.
#' @return one row per Sex x feature, with the parameters and their provenance.
#'
#' A zero-variance or non-estimable reference yields NA, never 0, so strict
#' contributor completeness catches it downstream instead of a constant silently
#' entering the composite.
mmm_rfid_scaling_parameters <- function(reference, feature_cols = MMM_RFID_RAW_FEATURES,
                                        reference_label = "unspecified") {
  if (!"Sex" %in% names(reference)) {
    stop("mmm_rfid_scaling_parameters() requires a Sex column.", call. = FALSE)
  }
  purrr::map_dfr(feature_cols, function(f) {
    reference %>%
      group_by(Sex = as.character(.data$Sex)) %>%
      summarise(
        feature = f,
        n_finite = sum(is.finite(.data[[f]])),
        center = { x <- .data[[f]][is.finite(.data[[f]])]
                   if (length(x) > 0L) mean(x) else NA_real_ },
        scale = { x <- .data[[f]][is.finite(.data[[f]])]
                  s <- if (length(x) > 1L) stats::sd(x) else NA_real_
                  if (!is.finite(s) || s == 0) NA_real_ else s },
        .groups = "drop"
      )
  }) %>%
    mutate(
      scaling_reference = reference_label,
      scaling_unit = "within Sex; never within Group; never within CageChange",
      zero_variance_policy = "sd == 0 or non-estimable yields NA, never 0",
      resolution_caveat = MMM_RFID_RESOLUTION_CAVEAT
    )
}

#' Apply previously estimated scaling parameters.
#'
#' Applying FIXED parameters is what makes CC2-CC4 interpretable in units of the
#' CC1 perturbation-response distribution. The parameters are never re-estimated
#' per cage change.
mmm_rfid_apply_scaling <- function(features, parameters,
                                   feature_cols = MMM_RFID_RAW_FEATURES) {
  if (!"Sex" %in% names(features)) {
    stop("mmm_rfid_apply_scaling() requires a Sex column.", call. = FALSE)
  }
  out <- features
  out$.sex_chr <- as.character(out$Sex)
  for (f in feature_cols) {
    p <- parameters[parameters$feature == f, c("Sex", "center", "scale"), drop = FALSE]
    idx <- match(out$.sex_chr, p$Sex)
    ctr <- p$center[idx]
    scl <- p$scale[idx]
    val <- suppressWarnings(as.numeric(out[[f]]))
    z <- ifelse(is.finite(val) & is.finite(ctr) & is.finite(scl), (val - ctr) / scl, NA_real_)
    out[[paste0(f, "_z")]] <- z
  }
  out$.sex_chr <- NULL
  out
}

#' Convenience: estimate within-Sex parameters on `features` and apply them.
#'
#' This is the FIRST-NIGHT scaling: the reference IS the analysed set.
mmm_rfid_standardize_within_sex <- function(features, feature_cols = MMM_RFID_RAW_FEATURES,
                                            reference_label = "first_night_CC1_within_Sex") {
  params <- mmm_rfid_scaling_parameters(features, feature_cols, reference_label)
  list(scaled = mmm_rfid_apply_scaling(features, params, feature_cols),
       parameters = params)
}
