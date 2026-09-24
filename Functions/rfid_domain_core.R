# ================================================================
# Four CORE raw-RFID behavioural domains: one source of truth
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# The legacy first-night implementation (Functions/first_night_domain_helpers.R)
# keeps the SAME information in three hand-maintained places: a contributor list,
# a formula STRING that is written verbatim into shipped CSVs, and a switch()
# that does the arithmetic. Nothing ties them together, so a silent drift between
# the published formula string and the evaluated arithmetic is possible. That
# legacy file is deliberately left untouched (it is locked by
# Testing/tests/test_first_night_domain_contract.R and is preserved as the
# LEGACY/SENSITIVITY artifact); this file is the replacement contract for the new
# four-domain characterisation.
#
# Here a domain is ONE coefficient vector over standardized raw contributors.
# Contributors, the printable formula and the arithmetic are all DERIVED from it,
# so two of the three drift channels cannot exist. The third - agreement between
# the printable string and the coefficients - is closed by a test that evaluates
# the string and compares it to the coefficient score
# (Testing/tests/test_rfid_domain_core_contract.R).
#
# WHAT THIS FILE DOES NOT DO
#   * It does not select domains using Group, CombZ or any phenotype label.
#   * It does not standardize. Scaling is the caller's responsibility, because
#     the first-night and longitudinal analyses use DIFFERENT, explicitly
#     declared scaling references (within-Sex CC1 vs CC1-referenced fixed
#     parameters). See Functions/rfid_domain_scaling.R.
#   * It does not tolerate missing contributors. A domain is NA unless every
#     contributor its coefficient vector names is finite. No na.rm, no
#     coalesce(missing, 0), no re-weighted partial score.
#
# NAMING (deliberate, and narrower than the legacy labels)
#   "Movement output" not "psychomotor activation": RFID position transitions do
#   not establish a psychomotor construct.
#   "Spatial entropy dynamics" not "behavioral/cognitive flexibility": this is
#   the dispersion and temporal persistence of spatial-occupancy entropy.
#   "Social-spatial organization" not "sociability": RFID proximity is a
#   CO-LOCATION time fraction, not social motivation, affiliation or preference.
#   "Cross-channel behavioral volatility" not "fragmentation": the score contains
#   three RMSSD terms and no explicit fragmentation variable.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ------------------------------------------------------------------- features

#' The nine raw per-animal-per-window RFID features every domain is built from.
#'
#' Movement  = count of RFID position transitions in the bin.
#' Entropy   = occupancy entropy over grid positions within the bin.
#' Proximity = ProximityFraction = ProximitySeconds / dyadic_observation_seconds,
#'             i.e. a CO-LOCATION time fraction with cage-mates. Undefined (NA)
#'             for a singly-housed animal, which is a real structural absence and
#'             must never be imputed as 0.
MMM_RFID_RAW_FEATURES <- c(
  "Movement_mean", "Movement_rmssd", "Movement_acf1",
  "Entropy_mean", "Entropy_rmssd", "Entropy_acf1",
  "Proximity_mean", "Proximity_rmssd", "Proximity_acf1"
)

#' Standardized contributor names, i.e. the columns the coefficient vectors index.
MMM_RFID_CONTRIBUTORS <- paste0(MMM_RFID_RAW_FEATURES, "_z")

# --------------------------------------------------------------- core domains

#' The four CORE domains, in presentation order.
#'
#' Movement output is last so it reads as the locomotion reference for the rows
#' above it, matching how the legacy panel ordered its locomotion row.
MMM_RFID_CORE_DOMAINS <- c(
  "Spatial entropy dynamics",
  "Social-spatial organization",
  "Cross-channel behavioral volatility",
  "Movement output"
)

#' Coefficient vectors. THIS IS THE SOURCE OF TRUTH.
#'
#' Domains 1, 2 and 3 retain the CURRENT AUDITED first-night coefficients
#' verbatim; only the labels change. Domain "Movement output" is the audited
#' Movement_mean_z row. The fifth legacy row
#' ("Active-phase adaptation / exploration") is deliberately ABSENT - see
#' MMM_RFID_EXCLUDED_LEGACY_DOMAIN below.
MMM_RFID_CORE_COEFFICIENTS <- list(
  "Spatial entropy dynamics" = c(
    Entropy_mean_z = 0.5, Entropy_rmssd_z = 0.5, Entropy_acf1_z = -1
  ),
  "Social-spatial organization" = c(
    Proximity_mean_z = 0.5, Proximity_acf1_z = 0.5, Proximity_rmssd_z = -1
  ),
  "Cross-channel behavioral volatility" = c(
    Movement_rmssd_z = 1 / 3, Entropy_rmssd_z = 1 / 3, Proximity_rmssd_z = 1 / 3
  ),
  "Movement output" = c(Movement_mean_z = 1)
)

#' Printable formulas, exactly as they are to appear in shipped provenance and in
#' the manuscript. Verified against the coefficients by the contract test.
MMM_RFID_CORE_FORMULAS <- c(
  "Spatial entropy dynamics" =
    "0.5 * (Entropy_mean_z + Entropy_rmssd_z) - Entropy_acf1_z",
  "Social-spatial organization" =
    "0.5 * (Proximity_mean_z + Proximity_acf1_z) - Proximity_rmssd_z",
  "Cross-channel behavioral volatility" =
    "(Movement_rmssd_z + Entropy_rmssd_z + Proximity_rmssd_z) / 3",
  "Movement output" = "Movement_mean_z"
)

#' Interpretation strings, shipped alongside every score so a reader cannot pick
#' up the number without the construct caveat.
MMM_RFID_CORE_INTERPRETATION <- c(
  "Spatial entropy dynamics" = paste(
    "Higher = broader/more variable spatial-occupancy entropy with LOWER temporal",
    "persistence. Descriptive RFID construct. NOT behavioural or cognitive",
    "flexibility."),
  "Social-spatial organization" = paste(
    "Higher = greater, more persistent and less volatile spatial CO-LOCATION with",
    "cage-mates. RFID proximity is a co-location measure; it is NOT sociability,",
    "social motivation, affiliation or social preference."),
  "Cross-channel behavioral volatility" = paste(
    "Higher = larger bin-to-bin fluctuation shared across movement, entropy and",
    "proximity channels. Contains RMSSD terms only and NO explicit fragmentation",
    "variable; do not label it fragmentation."),
  "Movement output" = paste(
    "Higher = more RFID position transitions per bin. Locomotor activity measured",
    "as RFID position transitions; NOT a general psychomotor construct.")
)

# ------------------------------------------------- weighting-sensitivity forms

#' Equal-term sensitivity coefficients for the two domains whose primary form
#' gives the single subtracted term the same TOTAL weight as the average of the
#' two positive terms. That choice is defensible but not uniquely determined, so
#' the equal-term alternative is scored alongside and compared PHENOTYPE-BLIND.
#'
#' The primary form remains primary unless the construct audit shows a serious
#' instability that is independent of phenotype. Formula choice is never made on
#' the basis of group p-values.
MMM_RFID_SENSITIVITY_COEFFICIENTS <- list(
  "Spatial entropy dynamics" = c(
    Entropy_mean_z = 1 / 3, Entropy_rmssd_z = 1 / 3, Entropy_acf1_z = -1 / 3
  ),
  "Social-spatial organization" = c(
    Proximity_mean_z = 1 / 3, Proximity_acf1_z = 1 / 3, Proximity_rmssd_z = -1 / 3
  )
)

MMM_RFID_SENSITIVITY_FORMULAS <- c(
  "Spatial entropy dynamics" =
    "(Entropy_mean_z + Entropy_rmssd_z - Entropy_acf1_z) / 3",
  "Social-spatial organization" =
    "(Proximity_mean_z + Proximity_acf1_z - Proximity_rmssd_z) / 3"
)

# --------------------------------------------------------- excluded legacy row

#' The legacy fifth domain, retained here ONLY as a documented exclusion.
#'
#' It is excluded for a CONSTRUCT-VALIDITY reason, not a statistical one, and the
#' reason is recorded so nobody re-derives it from p-values later.
MMM_RFID_EXCLUDED_LEGACY_DOMAIN <- list(
  name = "Active-phase adaptation / exploration",
  formula = paste(
    "(Movement_mean_z + Entropy_mean_z + Proximity_mean_z)/3 -",
    "(Movement_acf1_z + Entropy_acf1_z)/2"),
  exclusion_reason = paste(
    "A single 12-h level/persistence composite does not operationally measure",
    "adaptation. Adaptation is a CHANGE across repeated perturbations and must be",
    "inferred from the repeated responses to CC1-CC4, which the longitudinal",
    "analysis does. The formula is NOT claimed to be mathematically invalid, and",
    "the domain was NOT removed on the basis of any p-value. The five-domain",
    "first-night analysis is preserved unchanged as a legacy/sensitivity artifact."),
  preserved_in = paste(
    "Functions/first_night_domain_helpers.R +",
    "Functions/first_night_domain_driver.R (untouched)")
)

# ------------------------------------------------------------------- accessors

#' Contributors a domain requires, DERIVED from its coefficient vector.
mmm_rfid_domain_contributors <- function(domain, coefficients = MMM_RFID_CORE_COEFFICIENTS) {
  if (length(domain) != 1L || !domain %in% names(coefficients)) {
    stop("Unknown RFID domain: ", paste(domain, collapse = ", "),
         ". Known: ", paste(names(coefficients), collapse = ", "), call. = FALSE)
  }
  names(coefficients[[domain]])
}

#' Dense domain x contributor coefficient matrix, for algebraic overlap audits.
#'
#' Rows are domains, columns are ALL nine standardized contributors, zeros where a
#' domain does not use a contributor. This is what the phenotype-blind audit uses
#' to measure coefficient-vector overlap without touching any score.
mmm_rfid_coefficient_matrix <- function(coefficients = MMM_RFID_CORE_COEFFICIENTS,
                                        contributors = MMM_RFID_CONTRIBUTORS) {
  m <- matrix(0, nrow = length(coefficients), ncol = length(contributors),
              dimnames = list(names(coefficients), contributors))
  for (d in names(coefficients)) {
    cf <- coefficients[[d]]
    unknown <- setdiff(names(cf), contributors)
    if (length(unknown) > 0L) {
      stop("Domain '", d, "' names contributor(s) outside the declared raw feature ",
           "set: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    m[d, names(cf)] <- cf
  }
  m
}

# --------------------------------------------------------------------- scoring

#' Score ONE domain with strict contributor completeness.
#'
#' @param z data frame carrying the standardized contributor columns.
#' @param domain domain name present in `coefficients`.
#' @param coefficients coefficient list; swap in MMM_RFID_SENSITIVITY_COEFFICIENTS
#'   to score the equal-term weighting sensitivity with identical machinery.
#' @param formulas printable formula lookup matching `coefficients`.
#' @param id_cols identifier columns carried through onto the result.
#' @return one row per row of `z`, with the score plus full completeness QC.
#'
#' A row whose required contributors are not ALL finite yields NA. There is no
#' na.rm and no zero-fill: every animal is scored with exactly the same formula or
#' it is not scored at all.
mmm_rfid_score_domain <- function(z, domain,
                                  coefficients = MMM_RFID_CORE_COEFFICIENTS,
                                  formulas = MMM_RFID_CORE_FORMULAS,
                                  id_cols = c("AnimalNum", "Group", "Sex")) {
  need <- mmm_rfid_domain_contributors(domain, coefficients)
  missing_cols <- setdiff(need, names(z))
  if (length(missing_cols) > 0L) {
    stop("RFID domain '", domain, "' is missing contributor column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  mat <- as.matrix(z[, need, drop = FALSE])
  storage.mode(mat) <- "double"
  finite_mask <- is.finite(mat)
  complete <- rowSums(!finite_mask) == 0L

  cf <- coefficients[[domain]][need]
  # Zero out non-finite cells ONLY so the matrix product is defined; the
  # `complete` mask below is what actually decides whether a row is scored, so no
  # incomplete row can ever receive a re-weighted partial value.
  safe <- mat
  safe[!finite_mask] <- 0
  raw <- as.numeric(safe %*% cf)

  keep <- intersect(id_cols, names(z))
  out <- tibble::as_tibble(z[, keep, drop = FALSE])
  out$Domain <- domain
  out$DomainScore <- ifelse(complete, raw, NA_real_)
  out$required_contributor_count <- length(need)
  out$available_contributor_count <- as.integer(rowSums(finite_mask))
  out$complete_contributors <- complete
  out$missing_contributors <- apply(finite_mask, 1, function(r) {
    paste(need[!r], collapse = "|")
  })
  out$score_formula <- unname(formulas[[domain]])
  out$score_coefficients <- paste(sprintf("%s=%+.6f", need, unname(cf)), collapse = "; ")
  out$contributor_policy <- "strict_complete_contributors_required"
  out
}

#' Score every domain in a coefficient set.
mmm_rfid_score_all_domains <- function(z,
                                       coefficients = MMM_RFID_CORE_COEFFICIENTS,
                                       formulas = MMM_RFID_CORE_FORMULAS,
                                       id_cols = c("AnimalNum", "Group", "Sex")) {
  dplyr::bind_rows(lapply(names(coefficients), function(d) {
    mmm_rfid_score_domain(z, d, coefficients = coefficients, formulas = formulas,
                          id_cols = id_cols)
  }))
}

#' Machine-readable definition table, shipped with every analysis run.
mmm_rfid_domain_definition_table <- function() {
  dplyr::bind_rows(lapply(MMM_RFID_CORE_DOMAINS, function(d) {
    cf <- MMM_RFID_CORE_COEFFICIENTS[[d]]
    tibble::tibble(
      Domain = d,
      domain_role = "CORE",
      formula = unname(MMM_RFID_CORE_FORMULAS[[d]]),
      coefficients = paste(sprintf("%s=%+.6f", names(cf), unname(cf)), collapse = "; "),
      n_contributors = length(cf),
      contributors = paste(names(cf), collapse = "; "),
      interpretation = unname(MMM_RFID_CORE_INTERPRETATION[[d]]),
      feature_origin = "raw_RFID",
      hmm_derived = FALSE,
      inactive_phase_derived = FALSE,
      sensitivity_formula = if (d %in% names(MMM_RFID_SENSITIVITY_FORMULAS)) {
        unname(MMM_RFID_SENSITIVITY_FORMULAS[[d]])
      } else {
        NA_character_
      }
    )
  }))
}
