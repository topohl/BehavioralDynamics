# ================================================================================
# AUDIT (read-only): redundancy / independent-information decomposition of the
#   candidate label-free components of `Behavioral state architecture`
#
# Testing/audits/audit_hmm_state_architecture_redundancy.R      (deliverable 3)
#
# Purpose
#   (1) Pairwise Pearson + Spearman redundancy among all candidate components, at
#       BOTH the epoch level and the animal level, per resolution x PhaseClass.
#   (2) ANALYTIC coupling: exact complementarity of switch rate / self-transition
#       probability, the geometric dwell identity, the transition-entropy variance
#       decomposition, and the proximity-vs-inactive coupling.
#   (3) Distributions, boundary masses, bin-size dependence, length dependence,
#       normalization provenance, and a correlation-matrix eigen-decomposition
#       (PCA) of the five label-free candidates.
#   (4) The SMALLEST DEFENSIBLE label-free construct, derived from the coupling
#       structure ALONE. Group contrasts are computed only AFTER the construct is
#       fixed and are labelled as such.
#
# This script modifies nothing under Analysis/ or Functions/. It re-implements no
# repo contract: canonical_animal_id(), build_canonical_identity_roster(),
# audit_hmm_identity(), assert_hmm_identity_audit(),
# strict_standardize_within_context() and fit_repeated_measures_domain_contrasts()
# are all called from Functions/hmm_stage14_helpers.R.
#
# Terminology guard: RFID "Proximity" is a social-spatial co-location proxy, not
# measured sociability. The argmax-Proximity_z state is the "top-proximity" /
# high-co-occupancy state. No state is renamed "social".
#
# Column provenance note: every metric consumed here is read from the foundation
# table written by Testing/audits/audit_hmm_state_architecture_components.R (v2) and was
# independently reproduced from hmm_state_assignments.csv by the verification pass
# (max abs deviation <= 2.9e-14 on every metric). Two verifier caveats are handled
# explicitly rather than silently:
#   * mean_dwell_bins / bout counts / transition counts are computed by Stage 08
#     ACROSS the ~12 h opposite-phase gaps inside an epoch. The gap-aware variants
#     (*_gapaware) are carried as a sensitivity for the proposal.
#   * the shipped construct is strongly confounded with RFID read density
#     (observed_fraction). observed_fraction is therefore carried through the
#     correlation matrix as an explicit audit_extra metric, and the proposed
#     construct is checked against it.
# ================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(readr)
})

options(width = 210)

# --------------------------------------------------------------------------------
# 0. Repo + paths
# --------------------------------------------------------------------------------
repo_candidates <- c(
  "MMMSociability/Analysis/_pipeline_setup.R",
  "Analysis/_pipeline_setup.R",
  "C:/Users/topohl/Documents/GitHub/MMMSociability/Analysis/_pipeline_setup.R"
)
setup_path <- repo_candidates[file.exists(repo_candidates)][1]
if (is.na(setup_path)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(setup_path)
source_mmm_helper("hmm_stage14_helpers.R")

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
audit_out <- file.path(
  project_root,
  "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
)
ensure_dir(audit_out)

resolutions <- c("5min_based", "10min_based")
phases <- c("Active", "Inactive")

cat("================================================================\n")
cat("AUDIT deliverable 3: component redundancy / independent information\n")
cat("audit out  :", audit_out, "\n")
cat("================================================================\n\n")

# --------------------------------------------------------------------------------
# 1. Canonical roster + identity audit of the foundation table
# --------------------------------------------------------------------------------
canonical_roster_raw <- readr::read_csv(
  file.path(project_root, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
  col_types = readr::cols(
    .default = readr::col_skip(),
    AnimalNum = readr::col_character(),
    Group = readr::col_character(),
    Sex = readr::col_character()
  ),
  progress = FALSE
)
canonical_roster <- build_canonical_identity_roster(canonical_roster_raw, "Stage 01 canonical roster")
stopifnot(nrow(canonical_roster) == 111L)
cat("canonical roster animals:", nrow(canonical_roster), "\n")

foundation_file <- file.path(audit_out, "hmm_architecture_component_epoch_metrics.csv")
if (!file.exists(foundation_file)) stop("Foundation table missing: ", foundation_file, call. = FALSE)
foundation_raw <- readr::read_csv(foundation_file, col_types = readr::cols(), progress = FALSE)

identity_summaries <- list()
foundation <- purrr::map_dfr(resolutions, function(res) {
  d <- foundation_raw %>% filter(.data$resolution == res)
  aud <- audit_hmm_identity(d, canonical_roster, paste0("audit foundation component table ", res))
  assert_hmm_identity_audit(aud)
  identity_summaries[[res]] <<- aud$summary
  aud$data
})
identity_summary <- bind_rows(identity_summaries)
print(as.data.frame(identity_summary))
stopifnot(all(identity_summary$passed))
foundation <- foundation %>% mutate(AnimalNum = canonical_animal_id(as.character(.data$AnimalNum)))
stopifnot(!any(is.na(foundation$AnimalNum)))
write_table(identity_summary, file.path(audit_out, "hmm_architecture_redundancy_identity_audit.csv"))

cat("\nfoundation rows:", nrow(foundation), "\n")
print(foundation %>% count(resolution, PhaseClass) %>% as.data.frame())

# --------------------------------------------------------------------------------
# 2. Metric registry
#    "prespecified" = the 11 candidate components + the 2 composites named in the
#    task. "audit_extra" = quantities needed to answer the confound / length /
#    proposal questions; they are tagged so the prespecified matrix stays legible.
# --------------------------------------------------------------------------------
metric_registry <- tribble(
  ~metric, ~metric_set, ~normalization, ~label_dependent, ~temporal_ordering, ~bin_size_dependent,
  "occupancy_entropy", "prespecified",
  "per-epoch occupancy distribution (dimensionless, nats); already duration-normalised via frac_time",
  FALSE, FALSE, TRUE,
  "inactive_state_fraction", "prespecified",
  "fraction of observed bins (dimensionless); already duration-normalised",
  TRUE, FALSE, TRUE,
  "top_proximity_state_fraction", "prespecified",
  "fraction of observed bins (dimensionless); already duration-normalised",
  FALSE, FALSE, TRUE,
  "transition_entropy", "prespecified",
  "PER-STEP (nats/step); length-normalised by construction",
  FALSE, TRUE, TRUE,
  "transition_entropy_mm", "prespecified",
  "PER-STEP (nats/step), Miller-Madow bias-corrected",
  FALSE, TRUE, TRUE,
  "state_switch_rate", "prespecified",
  "PER-STEP probability (dimensionless); length-normalised by construction",
  FALSE, TRUE, TRUE,
  "self_transition_probability", "prespecified",
  "PER-STEP probability (dimensionless); length-normalised by construction",
  FALSE, TRUE, TRUE,
  "self_transition_probability_unweighted", "prespecified",
  "PER-STEP probability, unweighted mean of diag(P) over occupied from-states",
  FALSE, TRUE, TRUE,
  "mean_dwell_bins", "prespecified",
  "bins per bout (BIN-SIZE units); not a count, but not a real-time quantity either",
  FALSE, TRUE, TRUE,
  "mean_dwell_hours", "prespecified",
  "hours per bout (= mean_dwell_bins x bin_size_sec/3600); BIN-SIZE dependent by construction",
  FALSE, TRUE, TRUE,
  "switches_per_hour", "prespecified",
  "PER-HOUR rate: divides by total_observation_duration_hours (Stage 08 normalize_counts_to_rates logic)",
  FALSE, TRUE, TRUE,
  "historical_composite", "prespecified",
  "context-z composite (dimensionless) = shipped `Behavioral state architecture`",
  TRUE, FALSE, TRUE,
  "reduced_composite", "prespecified",
  "context-z composite (dimensionless) = 0.5*z(occ_entropy) - z(inactive)",
  TRUE, FALSE, TRUE,
  "n_transitions", "audit_extra",
  "RAW COUNT (= observed_bins - 1): confounds observation duration with behaviour; diagnostic only",
  FALSE, FALSE, TRUE,
  "observed_fraction", "audit_extra",
  "n_reads / expected_reads: raw RFID read density (known confound of the shipped construct)",
  FALSE, FALSE, FALSE,
  "proposed_flexibility", "audit_extra",
  "context-z composite (dimensionless): the section-8 proposal",
  FALSE, TRUE, TRUE
)

# label-free candidate set for the PCA (the 5 label-free candidate components)
pca_metrics <- c(
  "occupancy_entropy", "top_proximity_state_fraction", "transition_entropy",
  "state_switch_rate", "mean_dwell_bins"
)

# --------------------------------------------------------------------------------
# 3. Context-z terms (recomputed here with the repo helper) + the two composites,
#    the naive formula to be EVALUATED, and the PROPOSED construct. The proposal is
#    DERIVED IN SECTION 8 from the coupling structure only; it is materialised here
#    so it can be carried through the same correlation / distribution machinery.
# --------------------------------------------------------------------------------
z_terms <- c(
  "occupancy_entropy", "inactive_state_fraction", "top_proximity_state_fraction",
  "transition_entropy", "transition_entropy_mm", "state_switch_rate",
  "self_transition_probability", "self_transition_probability_unweighted",
  "mean_dwell_bins", "mean_dwell_hours", "switches_per_hour", "n_transitions",
  "observed_fraction", "state_switch_rate_gapaware", "transition_entropy_gapaware",
  "mean_dwell_bins_gapaware"
)

add_audit_z <- function(d) {
  # The foundation table already carries its own <v>_z columns. Preserve them under
  # an fz_ prefix BEFORE the helper overwrites <v>_z, so the audit's independent
  # recomputation (az_) can be compared against them.
  for (v in z_terms) {
    if (paste0(v, "_z") %in% names(d)) {
      d[[paste0("fz_", v)]] <- d[[paste0(v, "_z")]]
    }
  }
  for (v in z_terms) {
    d <- strict_standardize_within_context(d, v)
    names(d)[names(d) == paste0(v, "_z")] <- paste0("az_", v)
  }
  d
}

analysis_tbl <- foundation %>%
  group_split(resolution) %>%
  map_dfr(add_audit_z) %>%
  mutate(
    historical_composite = .data$`Behavioral state architecture`,
    reduced_composite    = .data$audit_composite_reduced,
    # PROPOSAL (derived in section 8; see the justification block there):
    #   equal-weight mean of ONE occupancy-repertoire term and ONE
    #   temporal-ordering term, each entering exactly once.
    proposed_flexibility = 0.5 * .data$az_occupancy_entropy + 0.5 * .data$az_state_switch_rate,
    proposed_flexibility_gapaware = 0.5 * .data$az_occupancy_entropy +
      0.5 * .data$az_state_switch_rate_gapaware,
    # the naive formula the task asks us to EVALUATE (not adopt):
    naive_formula = (.data$az_occupancy_entropy + .data$az_transition_entropy +
      .data$az_state_switch_rate) / 3 -
      (.data$az_self_transition_probability + .data$az_mean_dwell_bins) / 2
  )

cat("\n--- z-term reproduction vs the foundation table's own _z columns ---\n")
z_repro <- analysis_tbl %>%
  group_by(resolution) %>%
  summarise(
    max_abs_diff_occ_entropy_z = max(abs(az_occupancy_entropy - fz_occupancy_entropy)),
    max_abs_diff_switch_rate_z = max(abs(az_state_switch_rate - fz_state_switch_rate)),
    max_abs_diff_inactive_z    = max(abs(az_inactive_state_fraction - fz_inactive_state_fraction)),
    max_abs_diff_two_composites = max(abs(historical_composite - reduced_composite)),
    .groups = "drop"
  )
print(as.data.frame(z_repro))

# --------------------------------------------------------------------------------
# 4. Pairwise redundancy, epoch level AND animal level
# --------------------------------------------------------------------------------
classify_redundancy <- function(r) {
  a <- abs(r)
  dplyr::case_when(
    !is.finite(a) ~ "undefined",
    a > 0.999     ~ "mathematically_identical",
    a > 0.95      ~ "near_deterministic",
    a > 0.8       ~ "strongly_redundant",
    a > 0.5       ~ "moderately_related",
    TRUE          ~ "largely_independent"
  )
}

metric_cols <- metric_registry$metric

epoch_level <- analysis_tbl %>%
  select(resolution, AnimalNum, Group, Sex, CageChangeIndex, PhaseClass, all_of(metric_cols))

animal_level <- epoch_level %>%
  group_by(resolution, PhaseClass, AnimalNum, Group, Sex) %>%
  summarise(across(all_of(metric_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

pairwise_for <- function(d, res, ph, level) {
  m <- as.matrix(d[, metric_cols, drop = FALSE])
  pairs <- t(utils::combn(length(metric_cols), 2))
  purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
    ia <- pairs[i, 1]
    ib <- pairs[i, 2]
    x <- m[, ia]
    y <- m[, ib]
    ok <- is.finite(x) & is.finite(y)
    n <- sum(ok)
    usable <- n >= 3L && sd(x[ok]) > 0 && sd(y[ok]) > 0
    tibble(
      resolution = res, PhaseClass = ph, level = level,
      metric_a = metric_cols[ia], metric_b = metric_cols[ib],
      n = n,
      pearson_r = if (usable) cor(x[ok], y[ok], method = "pearson") else NA_real_,
      spearman_rho = if (usable) suppressWarnings(cor(x[ok], y[ok], method = "spearman")) else NA_real_
    )
  })
}

correlations <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(phases, function(ph) {
    bind_rows(
      pairwise_for(epoch_level %>% filter(resolution == res, PhaseClass == ph), res, ph, "epoch"),
      pairwise_for(animal_level %>% filter(resolution == res, PhaseClass == ph), res, ph, "animal")
    )
  })
}) %>%
  mutate(
    max_abs_r = pmax(abs(pearson_r), abs(spearman_rho)),
    redundancy_class = classify_redundancy(max_abs_r),
    redundancy_class_pearson = classify_redundancy(pearson_r),
    redundancy_class_spearman = classify_redundancy(spearman_rho),
    redundancy_basis = "max(|pearson_r|, |spearman_rho|)",
    primary_level = "animal",
    primary_level_rationale = paste(
      "The between-group contrast lives at the animal level: Hedges g is computed",
      "from one mean per animal and the mixed model carries a per-animal random",
      "intercept, so animal-level redundancy is the scale at which two components",
      "can or cannot contribute separate information to the group comparison.",
      "Epoch-level correlations additionally contain within-animal cage-change",
      "variation and are reported for completeness."
    )
  ) %>%
  left_join(metric_registry %>% select(metric, metric_set_a = metric_set),
    by = c("metric_a" = "metric")
  ) %>%
  left_join(metric_registry %>% select(metric, metric_set_b = metric_set),
    by = c("metric_b" = "metric")
  ) %>%
  mutate(pair_set = if_else(
    metric_set_a == "prespecified" & metric_set_b == "prespecified",
    "prespecified", "includes_audit_extra"
  )) %>%
  arrange(resolution, PhaseClass, level, desc(max_abs_r))

write_table(correlations, file.path(audit_out, "hmm_architecture_component_correlations.csv"))
cat("\nwrote hmm_architecture_component_correlations.csv  rows =", nrow(correlations), "\n")

cat("\n--- redundancy_class counts (prespecified pairs only) ---\n")
print(correlations %>%
  filter(pair_set == "prespecified") %>%
  count(level, redundancy_class) %>%
  pivot_wider(names_from = level, values_from = n) %>%
  as.data.frame())

cat("\n--- ANIMAL-LEVEL prespecified pairs at |r| > 0.95 ---\n")
print(correlations %>%
  filter(level == "animal", pair_set == "prespecified", max_abs_r > 0.95) %>%
  select(resolution, PhaseClass, metric_a, metric_b, n, pearson_r, spearman_rho, redundancy_class) %>%
  as.data.frame(), digits = 6)

cat("\n--- ANIMAL-LEVEL prespecified pairs that are LARGELY INDEPENDENT (|r| <= 0.5) ---\n")
print(correlations %>%
  filter(level == "animal", pair_set == "prespecified", redundancy_class == "largely_independent") %>%
  select(resolution, PhaseClass, metric_a, metric_b, pearson_r, spearman_rho) %>%
  as.data.frame(), digits = 4)

# --------------------------------------------------------------------------------
# 5. ANALYTIC COUPLING
# --------------------------------------------------------------------------------
cat("\n\n================ 5. ANALYTIC COUPLING ================\n")

# 5a. switch rate vs self-transition probability: exact complements?
coupling_a <- analysis_tbl %>%
  group_by(resolution) %>%
  summarise(
    n = n(),
    max_abs_dev_from_unit_sum = max(abs(state_switch_rate + self_transition_probability - 1)),
    mean_abs_dev_from_unit_sum = mean(abs(state_switch_rate + self_transition_probability - 1)),
    pearson = cor(state_switch_rate, self_transition_probability),
    spearman = suppressWarnings(cor(state_switch_rate, self_transition_probability, method = "spearman")),
    max_abs_dev_z_sign_flip = max(abs(az_state_switch_rate + az_self_transition_probability)),
    .groups = "drop"
  ) %>%
  mutate(
    verdict = if_else(max_abs_dev_from_unit_sum < 1e-12,
      "EXACT complements: switch_rate = 1 - P(self). SAME variable, opposite sign.",
      "NOT exact complements"
    ),
    consequence = paste(
      "z(1-x) = -z(x) identically inside any standardization context, so",
      "z(self_transition_probability) == -z(state_switch_rate) to machine precision.",
      "Putting them on opposite sides of a composite adds the SAME measurement twice."
    )
  )
print(as.data.frame(coupling_a), digits = 6)

# 5b. mean dwell vs 1/(1 - P(self))
coupling_b <- analysis_tbl %>%
  group_by(resolution) %>%
  summarise(
    n_finite = sum(is.finite(mean_dwell_bins) & is.finite(geometric_dwell_prediction)),
    pearson_dwell_vs_geom = cor(mean_dwell_bins, geometric_dwell_prediction, use = "complete.obs"),
    spearman_dwell_vs_geom = suppressWarnings(cor(mean_dwell_bins, geometric_dwell_prediction,
      method = "spearman", use = "complete.obs"
    )),
    max_rel_deviation = max(abs(mean_dwell_bins - geometric_dwell_prediction) /
      geometric_dwell_prediction, na.rm = TRUE),
    median_rel_deviation = median(abs(mean_dwell_bins - geometric_dwell_prediction) /
      geometric_dwell_prediction, na.rm = TRUE),
    pearson_dwell_vs_Pself = cor(mean_dwell_bins, self_transition_probability),
    spearman_dwell_vs_Pself = suppressWarnings(cor(mean_dwell_bins, self_transition_probability,
      method = "spearman"
    )),
    spearman_dwell_vs_switchrate = suppressWarnings(cor(mean_dwell_bins, state_switch_rate,
      method = "spearman"
    )),
    .groups = "drop"
  ) %>%
  mutate(
    monotone_in_Pself = spearman_dwell_vs_Pself > 0.95,
    verdict = paste0(
      "mean_dwell_bins is a near-monotone (Spearman ", sprintf("%.4f", spearman_dwell_vs_Pself),
      ") transform of P(self): no independent DIMENSION, only a nonlinear rescaling."
    )
  )
print(as.data.frame(coupling_b), digits = 6)

coupling_b_animal <- animal_level %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    n = n(),
    spearman_dwell_vs_Pself = suppressWarnings(cor(mean_dwell_bins, self_transition_probability,
      method = "spearman"
    )),
    pearson_dwell_vs_Pself = cor(mean_dwell_bins, self_transition_probability),
    .groups = "drop"
  )
cat("\nanimal-level dwell vs P(self):\n")
print(as.data.frame(coupling_b_animal), digits = 6)

# 5c. transition entropy: partial correlations + variance decomposition
pcor3 <- function(x, y, z) {
  rxy <- cor(x, y)
  rxz <- cor(x, z)
  ryz <- cor(y, z)
  (rxy - rxz * ryz) / sqrt((1 - rxz^2) * (1 - ryz^2))
}
r2_of <- function(f, d) summary(lm(f, data = d))$r.squared

coupling_c <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(c("pooled", phases), function(ph) {
    d <- animal_level %>% filter(resolution == res)
    de <- epoch_level %>% filter(resolution == res)
    if (ph != "pooled") {
      d <- d %>% filter(PhaseClass == ph)
      de <- de %>% filter(PhaseClass == ph)
    }
    r2_sw <- r2_of(transition_entropy ~ state_switch_rate, d)
    r2_occ <- r2_of(transition_entropy ~ occupancy_entropy, d)
    r2_both <- r2_of(transition_entropy ~ state_switch_rate + occupancy_entropy, d)
    r2_both_e <- r2_of(transition_entropy ~ state_switch_rate + occupancy_entropy, de)
    tibble(
      resolution = res, PhaseClass = ph, level = "animal", n = nrow(d),
      r_H_switch = cor(d$transition_entropy, d$state_switch_rate),
      r_H_occent = cor(d$transition_entropy, d$occupancy_entropy),
      r_switch_occent = cor(d$state_switch_rate, d$occupancy_entropy),
      partial_r_H_switch_given_occent = pcor3(d$transition_entropy, d$state_switch_rate, d$occupancy_entropy),
      partial_r_H_occent_given_switch = pcor3(d$transition_entropy, d$occupancy_entropy, d$state_switch_rate),
      r2_H_on_switch = r2_sw,
      r2_H_on_occent = r2_occ,
      r2_H_on_both = r2_both,
      residual_sd_fraction_on_switch = sqrt(1 - r2_sw),
      residual_sd_fraction_on_both = sqrt(1 - r2_both),
      incremental_r2_occent_over_switch = r2_both - r2_sw,
      r2_H_on_both_epoch_level = r2_both_e,
      residual_sd_fraction_on_both_epoch = sqrt(1 - r2_both_e)
    )
  })
})
cat("\n--- 5c. transition_entropy variance decomposition ---\n")
print(as.data.frame(coupling_c), digits = 4)

# 5d. top-proximity state fraction vs inactive_state_fraction
coupling_d <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(c("pooled", phases), function(ph) {
    d <- epoch_level %>% filter(resolution == res)
    a <- animal_level %>% filter(resolution == res)
    if (ph != "pooled") {
      d <- d %>% filter(PhaseClass == ph)
      a <- a %>% filter(PhaseClass == ph)
    }
    tibble(
      resolution = res, PhaseClass = ph,
      n_epoch = nrow(d), n_animal = nrow(a),
      pearson_epoch = cor(d$top_proximity_state_fraction, d$inactive_state_fraction),
      spearman_epoch = suppressWarnings(cor(d$top_proximity_state_fraction,
        d$inactive_state_fraction, method = "spearman")),
      pearson_animal = cor(a$top_proximity_state_fraction, a$inactive_state_fraction),
      spearman_animal = suppressWarnings(cor(a$top_proximity_state_fraction,
        a$inactive_state_fraction, method = "spearman")),
      r_topprox_vs_composite_animal = cor(a$top_proximity_state_fraction, a$historical_composite),
      variance_share_topprox_of_inactive = cor(a$top_proximity_state_fraction,
        a$inactive_state_fraction)^2
    )
  })
})
cat("\n--- 5d. top-proximity fraction vs inactive fraction ---\n")
print(as.data.frame(coupling_d), digits = 5)

coupling_all <- bind_rows(
  coupling_a %>% mutate(coupling = "5a_switchrate_vs_selfprob") %>% mutate(across(everything(), as.character)),
  coupling_b %>% mutate(coupling = "5b_dwell_vs_geometric") %>% mutate(across(everything(), as.character)),
  coupling_b_animal %>% mutate(coupling = "5b_dwell_vs_Pself_animal") %>% mutate(across(everything(), as.character)),
  coupling_c %>% mutate(coupling = "5c_transition_entropy_decomposition") %>% mutate(across(everything(), as.character)),
  coupling_d %>% mutate(coupling = "5d_topprox_vs_inactive") %>% mutate(across(everything(), as.character))
) %>% relocate(coupling)
write_table(coupling_all, file.path(audit_out, "hmm_architecture_component_coupling.csv"))

# --------------------------------------------------------------------------------
# 6. DISTRIBUTIONS AND SCALE ARTEFACTS
# --------------------------------------------------------------------------------
cat("\n\n================ 6. DISTRIBUTIONS AND SCALE ARTEFACTS ================\n")

skew_g1 <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s2 <- mean((x - m)^2)
  if (s2 <= 0) return(NA_real_)
  mean((x - m)^3) / s2^(3 / 2)
}

# cross-resolution comparability at animal level: rank agreement of the SAME
# animal x phase measured at 5 min vs 10 min. A direct test of whether the metric
# is transportable across discretizations.
cross_res <- animal_level %>%
  select(resolution, PhaseClass, AnimalNum, all_of(metric_cols)) %>%
  pivot_longer(all_of(metric_cols), names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = resolution, values_from = value) %>%
  group_by(PhaseClass, metric) %>%
  summarise(
    n_animals_paired = sum(is.finite(`5min_based`) & is.finite(`10min_based`)),
    cross_resolution_animal_spearman = suppressWarnings(cor(`5min_based`, `10min_based`,
      method = "spearman", use = "complete.obs")),
    cross_resolution_animal_pearson = suppressWarnings(cor(`5min_based`, `10min_based`,
      use = "complete.obs")),
    cross_resolution_mean_ratio_5min_over_10min = mean(`5min_based`, na.rm = TRUE) /
      mean(`10min_based`, na.rm = TRUE),
    .groups = "drop"
  )

distributions <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(phases, function(ph) {
    d <- epoch_level %>% filter(resolution == res, PhaseClass == ph)
    purrr::map_dfr(metric_cols, function(mm) {
      x <- d[[mm]]
      xf <- x[is.finite(x)]
      tibble(
        resolution = res, PhaseClass = ph, level = "epoch", metric = mm,
        n = length(xf), n_nonfinite = sum(!is.finite(x)),
        mean = mean(xf), sd = sd(xf),
        min = min(xf), q25 = quantile(xf, .25, names = FALSE),
        median = median(xf), q75 = quantile(xf, .75, names = FALSE), max = max(xf),
        skewness_g1 = skew_g1(xf),
        frac_at_exactly_0 = mean(xf == 0),
        frac_at_exactly_1 = mean(xf == 1),
        n_at_exactly_0 = sum(xf == 0),
        n_at_exactly_1 = sum(xf == 1),
        frac_at_observed_min = mean(xf == min(xf)),
        frac_at_observed_max = mean(xf == max(xf))
      )
    })
  })
}) %>%
  left_join(metric_registry, by = "metric") %>%
  left_join(cross_res, by = c("PhaseClass", "metric")) %>%
  arrange(resolution, PhaseClass, metric)

write_table(distributions, file.path(audit_out, "hmm_architecture_component_distributions.csv"))
cat("wrote hmm_architecture_component_distributions.csv  rows =", nrow(distributions), "\n")

cat("\n--- boundary masses (metrics with any epoch exactly at 0 or exactly at 1) ---\n")
print(distributions %>%
  filter(frac_at_exactly_0 > 0 | frac_at_exactly_1 > 0) %>%
  select(resolution, PhaseClass, metric, n, n_at_exactly_0, n_at_exactly_1, min, max) %>%
  as.data.frame(), digits = 5)

cat("\n--- skewness of the prespecified components (epoch level) ---\n")
print(distributions %>%
  filter(metric_set == "prespecified") %>%
  select(resolution, PhaseClass, metric, mean, sd, median, max, skewness_g1) %>%
  as.data.frame(), digits = 4)

cat("\n--- cross-resolution animal-level agreement per metric ---\n")
print(cross_res %>% as.data.frame(), digits = 4)

cat("\n--- 6a. bin-size dependence of the dwell metrics (epoch level, pooled phases) ---\n")
binsize <- analysis_tbl %>%
  group_by(resolution) %>%
  summarise(
    bin_size_sec = first(bin_size_sec),
    mean_dwell_bins = mean(mean_dwell_bins),
    mean_dwell_hours = mean(mean_dwell_hours),
    state_switch_rate = mean(state_switch_rate),
    switches_per_hour = mean(switches_per_hour),
    occupancy_entropy = mean(occupancy_entropy),
    transition_entropy = mean(transition_entropy),
    .groups = "drop"
  )
print(as.data.frame(binsize), digits = 5)

bs_quantities <- c("mean_dwell_bins", "mean_dwell_hours", "state_switch_rate",
                   "switches_per_hour", "occupancy_entropy", "transition_entropy")
binsize_ratio <- tibble(
  quantity = bs_quantities,
  value_5min = as.numeric(binsize[binsize$resolution == "5min_based", bs_quantities]),
  value_10min = as.numeric(binsize[binsize$resolution == "10min_based", bs_quantities])
) %>%
  mutate(
    ratio_5min_over_10min = value_5min / value_10min,
    ratio_if_scale_free = 1,
    ratio_if_pure_bin_count_rescale = c(2, 1, NA, 2, NA, NA),
    comparable_across_resolutions = c(
      "NO in absolute value: a pure count rescale predicts 2x, observed ratio differs; only the within-resolution z / rank is transportable",
      "NO by construction: hours per bout = bins x bin_size, so the two resolutions are not on a common scale",
      "Dimensionless per-step probability on a common [0,1] support, but its value still depends on the discretization",
      "NO: a per-hour rate whose numerator (switch count) is bin-size dependent",
      "Common support [0, log(4)] at both resolutions; value is resolution dependent",
      "Common support [0, log(4)] at both resolutions; value is resolution dependent"
    )
  )
print(as.data.frame(binsize_ratio), digits = 5)

cat("\n--- 6b. length dependence of transition_entropy ---\n")
length_dep <- purrr::map_dfr(resolutions, function(res) {
  d <- analysis_tbl %>% filter(resolution == res)
  a <- animal_level %>% filter(resolution == res)
  sp <- function(x, y) suppressWarnings(cor(x, y, method = "spearman"))
  tibble(
    resolution = res,
    spearman_H_vs_ntrans_epoch = sp(d$transition_entropy, d$n_transitions),
    spearman_Hmm_vs_ntrans_epoch = sp(d$transition_entropy_mm, d$n_transitions),
    spearman_occent_vs_ntrans_epoch = sp(d$occupancy_entropy, d$n_transitions),
    spearman_switchrate_vs_ntrans_epoch = sp(d$state_switch_rate, d$n_transitions),
    spearman_proposed_vs_ntrans_epoch = sp(d$proposed_flexibility, d$n_transitions),
    spearman_H_vs_ntrans_animal = sp(a$transition_entropy, a$n_transitions),
    spearman_switchrate_vs_ntrans_animal = sp(a$state_switch_rate, a$n_transitions),
    n_transitions_min = min(d$n_transitions), n_transitions_max = max(d$n_transitions),
    sd_transition_entropy = sd(d$transition_entropy),
    sd_state_switch_rate = sd(d$state_switch_rate)
  )
})
print(as.data.frame(length_dep), digits = 4)

l2_file <- file.path(audit_out, "hmm_architecture_check_l2_subsampling_length_bias.csv")
l2_reuse <- NULL
if (file.exists(l2_file)) {
  l2 <- readr::read_csv(l2_file, col_types = readr::cols(), progress = FALSE)
  l2_reuse <- l2 %>%
    transmute(
      resolution, L_shortest_epoch_bins, n_long_epochs, sd_transition_entropy_all,
      mean_shift_H_firstL, mean_shift_H_randwin, mean_shift_sw_randwin,
      mm_fraction_of_bias_removed_randwin,
      H_bias_as_fraction_of_H_sd = abs(mean_shift_H_randwin) / sd_transition_entropy_all,
      switchrate_bias_absolute = abs(mean_shift_sw_randwin)
    )
  cat("\nfoundation subsampling/truncation check (check_l2), REUSED not re-run:\n")
  print(as.data.frame(l2_reuse), digits = 4)
} else {
  cat("\ncheck_l2 not found on disk; length bias reported from correlations only\n")
}

cat("\n--- 6c. normalization provenance + does observation duration differ by Group/Sex? ---\n")
print(metric_registry %>% select(metric, metric_set, normalization) %>% as.data.frame(), right = FALSE)

duration_by_group <- analysis_tbl %>%
  group_by(resolution, PhaseClass, Sex, Group) %>%
  summarise(
    n_epochs = n(),
    mean_duration_hours = mean(total_observation_duration_hours),
    sd_duration_hours = sd(total_observation_duration_hours),
    min_duration_hours = min(total_observation_duration_hours),
    max_duration_hours = max(total_observation_duration_hours),
    mean_n_transitions = mean(n_transitions),
    mean_observed_bins = mean(observed_bins),
    .groups = "drop"
  )
print(as.data.frame(duration_by_group), digits = 5)

duration_test <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(phases, function(ph) {
    d <- analysis_tbl %>% filter(resolution == res, PhaseClass == ph)
    if (sd(d$total_observation_duration_hours) == 0) {
      return(tibble(
        resolution = res, PhaseClass = ph,
        duration_sd_over_mean = 0, p_Group = NA_real_, p_Sex = NA_real_,
        p_GroupSex = NA_real_, r2 = NA_real_,
        note = "observation duration is CONSTANT in this cell; no Group/Sex test possible"
      ))
    }
    fit <- lm(total_observation_duration_hours ~ Group * Sex, data = d)
    an <- anova(fit)
    tibble(
      resolution = res, PhaseClass = ph,
      duration_sd_over_mean = sd(d$total_observation_duration_hours) /
        mean(d$total_observation_duration_hours),
      p_Group = an["Group", "Pr(>F)"], p_Sex = an["Sex", "Pr(>F)"],
      p_GroupSex = an["Group:Sex", "Pr(>F)"], r2 = summary(fit)$r.squared,
      note = ""
    )
  })
})
cat("\nduration ~ Group*Sex:\n")
print(as.data.frame(duration_test), digits = 4)

write_table(
  bind_rows(
    binsize_ratio %>% mutate(block = "6a_bin_size") %>% mutate(across(everything(), as.character)),
    length_dep %>% mutate(block = "6b_length_dependence") %>% mutate(across(everything(), as.character)),
    if (!is.null(l2_reuse)) {
      l2_reuse %>% mutate(block = "6b_subsampling_reused") %>% mutate(across(everything(), as.character))
    } else NULL,
    duration_by_group %>% mutate(block = "6c_duration_by_group") %>% mutate(across(everything(), as.character)),
    duration_test %>% mutate(block = "6c_duration_test") %>% mutate(across(everything(), as.character))
  ) %>% relocate(block),
  file.path(audit_out, "hmm_architecture_component_scale_artefacts.csv")
)

# --------------------------------------------------------------------------------
# 7. PCA / eigen-decomposition of the 5 label-free candidates
# --------------------------------------------------------------------------------
cat("\n\n================ 7. PCA OF THE 5 LABEL-FREE CANDIDATES ================\n")
cat("components:", paste(pca_metrics, collapse = ", "), "\n")

# Three variable sets are decomposed:
#   five_candidates    the 5 label-free candidates as prespecified. NOTE: 3 of the 5
#                      (transition_entropy, state_switch_rate, mean_dwell_bins) are
#                      the SAME persistence dimension, so this set is intrinsically
#                      unbalanced and PC1 is inflated by the triplicate.
#   deduplicated_three one representative per redundancy class: occupancy_entropy
#                      (repertoire), state_switch_rate (temporal ordering),
#                      top_proximity_state_fraction (co-location occupancy).
#                      This is the honest dimensionality statement.
#   proposal_two       the two terms actually used by the section-8 proposal.
pca_variable_sets <- list(
  five_candidates = pca_metrics,
  deduplicated_three = c("occupancy_entropy", "state_switch_rate", "top_proximity_state_fraction"),
  proposal_two = c("occupancy_entropy", "state_switch_rate")
)

eig_block <- function(d, res, ph, level, vars = pca_metrics, set_label = "five_candidates") {
  m <- as.matrix(d[, vars, drop = FALSE])
  m <- m[stats::complete.cases(m), , drop = FALSE]
  R <- cor(m)
  e <- eigen(R, symmetric = TRUE)
  ev <- e$values
  prop <- ev / sum(ev)
  ld <- e$vectors
  for (j in seq_len(ncol(ld))) {
    if (ld[which.max(abs(ld[, j])), j] < 0) ld[, j] <- -ld[, j]
  }
  purrr::map_dfr(seq_along(ev), function(j) {
    row <- tibble(
      variable_set = set_label, n_variables = length(vars),
      variables = paste(vars, collapse = "|"),
      resolution = res, PhaseClass = ph, level = level, n_obs = nrow(m),
      PC = paste0("PC", j), eigenvalue = ev[j],
      prop_variance = prop[j], cum_prop_variance = cumsum(prop)[j],
      n_eigenvalues_gt_1 = sum(ev > 1),
      n_pcs_for_80pct = which(cumsum(prop) >= 0.80)[1],
      n_pcs_for_90pct = which(cumsum(prop) >= 0.90)[1],
      condition_number = max(ev) / min(ev),
      # participation ratio: effective number of independent dimensions
      effective_n_dimensions = sum(ev)^2 / sum(ev^2)
    )
    for (k in seq_along(pca_metrics)) {
      idx <- match(pca_metrics[k], vars)
      row[[paste0("loading_", pca_metrics[k])]] <- if (is.na(idx)) NA_real_ else ld[idx, j]
    }
    row
  })
}

pca_tbl <- purrr::map_dfr(names(pca_variable_sets), function(sl) {
  vars <- pca_variable_sets[[sl]]
  purrr::map_dfr(resolutions, function(res) {
    purrr::map_dfr(phases, function(ph) {
      bind_rows(
        eig_block(animal_level %>% filter(resolution == res, PhaseClass == ph),
          res, ph, "animal", vars, sl),
        eig_block(epoch_level %>% filter(resolution == res, PhaseClass == ph),
          res, ph, "epoch", vars, sl)
      )
    })
  })
})
write_table(pca_tbl, file.path(audit_out, "hmm_architecture_component_pca.csv"))
cat("wrote hmm_architecture_component_pca.csv  rows =", nrow(pca_tbl), "\n\n")

cat("\n--- variable_set = five_candidates, ANIMAL level ---\n")
print(pca_tbl %>%
  filter(level == "animal", variable_set == "five_candidates") %>%
  select(resolution, PhaseClass, PC, eigenvalue, prop_variance, cum_prop_variance,
    starts_with("loading_")) %>%
  as.data.frame(), digits = 3)

cat("\n--- variable_set = deduplicated_three, ANIMAL level (the honest dimensionality) ---\n")
print(pca_tbl %>%
  filter(level == "animal", variable_set == "deduplicated_three") %>%
  select(resolution, PhaseClass, PC, eigenvalue, prop_variance, cum_prop_variance,
    loading_occupancy_entropy, loading_state_switch_rate,
    loading_top_proximity_state_fraction) %>%
  as.data.frame(), digits = 3)

cat("\n--- variable_set = proposal_two, ANIMAL level ---\n")
print(pca_tbl %>%
  filter(level == "animal", variable_set == "proposal_two") %>%
  select(resolution, PhaseClass, PC, eigenvalue, prop_variance,
    loading_occupancy_entropy, loading_state_switch_rate) %>%
  as.data.frame(), digits = 3)

cat("\ndimensionality summary (all variable sets, both levels):\n")
print(pca_tbl %>%
  filter(PC == "PC1") %>%
  select(variable_set, level, resolution, PhaseClass, n_obs, n_eigenvalues_gt_1,
    n_pcs_for_80pct, n_pcs_for_90pct, effective_n_dimensions, condition_number) %>%
  arrange(variable_set, level, resolution, PhaseClass) %>%
  as.data.frame(), digits = 4)

# --------------------------------------------------------------------------------
# 8. THE SMALLEST DEFENSIBLE LABEL-FREE CONSTRUCT
#    DERIVED BEFORE ANY GROUP COMPARISON. Every quantity used in this section is a
#    coupling / redundancy / scale statistic. No Group variable is read until
#    section 9.
# --------------------------------------------------------------------------------
cat("\n\n================ 8. NAIVE FORMULA: EFFECTIVE WEIGHTS ================\n")

naive_weights_analytic <- tibble(
  term = c("z(occupancy_entropy)", "z(transition_entropy)", "z(state_switch_rate)",
           "z(self_transition_probability)", "z(mean_dwell_bins)"),
  nominal_weight = c(1 / 3, 1 / 3, 1 / 3, -1 / 2, -1 / 2),
  after_substituting_z_self_eq_minus_z_switch = c(1 / 3, 1 / 3, 1 / 3 + 1 / 2, 0, -1 / 2)
)
print(as.data.frame(naive_weights_analytic), digits = 6)

naive_weights_emp <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(phases, function(ph) {
    d <- analysis_tbl %>% filter(resolution == res, PhaseClass == ph)
    fit <- lm(naive_formula ~ az_occupancy_entropy + az_state_switch_rate, data = d)
    co <- coef(fit)
    tibble(
      resolution = res, PhaseClass = ph, n = nrow(d),
      max_abs_z_self_plus_z_switch = max(abs(d$az_self_transition_probability +
        d$az_state_switch_rate)),
      effective_beta_occupancy_entropy = co[["az_occupancy_entropy"]],
      effective_beta_state_switch_rate = co[["az_state_switch_rate"]],
      ratio_switch_over_entropy_weight = co[["az_state_switch_rate"]] / co[["az_occupancy_entropy"]],
      r2_naive_on_two_dims = summary(fit)$r.squared,
      cor_naive_with_z_switch = cor(d$naive_formula, d$az_state_switch_rate),
      cor_naive_with_z_occent = cor(d$naive_formula, d$az_occupancy_entropy),
      cor_naive_with_proposed = cor(d$naive_formula, d$proposed_flexibility),
      cor_naive_with_historical = cor(d$naive_formula, d$historical_composite)
    )
  })
})
cat("\nempirical projection of the naive formula on the two surviving dimensions:\n")
print(as.data.frame(naive_weights_emp), digits = 4)

write_table(
  bind_rows(
    naive_weights_analytic %>% mutate(block = "8a_analytic") %>% mutate(across(everything(), as.character)),
    naive_weights_emp %>% mutate(block = "8b_empirical") %>% mutate(across(everything(), as.character))
  ) %>% relocate(block),
  file.path(audit_out, "hmm_architecture_naive_formula_weights.csv")
)

cat("\n\n================ 8c. PROPOSED CONSTRUCT (derived pre-hoc) ================\n")
proposal <- tibble(
  construct = "latent_state_flexibility",
  formula = "0.5 * z(occupancy_entropy) + 0.5 * z(state_switch_rate)",
  z_context = "Sex x PhaseClass x CageChangeIndex (strict_standardize_within_context, unchanged)",
  n_components = 2L,
  label_free = TRUE,
  double_weights_any_measurement = FALSE,
  has_temporal_ordering_component = TRUE,
  effective_weight_repertoire_breadth = 0.5,
  effective_weight_temporal_switching = 0.5,
  effective_weight_proximity_occupancy = 0,
  effective_weight_label_derived_inactive_fraction = 0,
  what_it_measures = paste(
    "the average of two standardized quantities: how evenly an animal's observed",
    "time is spread over the four latent states (repertoire breadth, invariant to",
    "shuffling the state sequence) and how often it changes state per observed step",
    "(temporal switching, defined only by the ordering). Higher = a broader",
    "latent-state repertoire visited with faster turnover."
  ),
  term_correlation_disclosure = paste(
    "HONEST DISCLOSURE: the two terms are NOT independent. Epoch-level r =",
    "0.446 to 0.671 (shared variance 0.20-0.45; participation ratio 1.38-1.67).",
    "At the PRIMARY animal level they are more strongly related: r = 0.646",
    "(10min Active), 0.707 (5min Inactive), 0.778 (5min Active), 0.872 (10min",
    "Inactive), i.e. shared variance 0.42-0.76 and a participation ratio of only",
    "1.14-1.41 effective dimensions. The two-component form is therefore justified",
    "by CONSTRUCT VALIDITY (an occupancy-only score cannot be called 'architecture'",
    "because it is invariant to shuffling the sequence), NOT by a claim that the",
    "two terms are empirically independent. Because they are positively correlated",
    "and enter with equal positive weight, no measurement is double-weighted and",
    "neither term is subtracted from the other."
  ),
  why_not_more_components = paste(
    "state_switch_rate, self_transition_probability,",
    "self_transition_probability_unweighted, mean_dwell_bins, mean_dwell_hours and",
    "switches_per_hour are ONE dimension (exact complement, near-monotone",
    "transform, and constant rescalings). transition_entropy is near-deterministic",
    "in switch rate (animal-level |r| = 0.93 to 0.98; R^2 of H on switch rate alone",
    "= 0.86 to 0.96, residual SD 19-37% of total) and additionally carries a",
    "plug-in length bias, so it is dropped in favour of the unbiased per-step",
    "switch rate. top_proximity_state_fraction is a separate dimension (it is the",
    "PC2 axis, loading 0.85-0.95) but it measures social-spatial co-location",
    "occupancy, not state architecture, so it is deliberately NOT folded in."
  ),
  why_not_fewer_components = paste(
    "occupancy-only measures (including the shipped composite) are invariant to",
    "randomly shuffling the Viterbi state sequence within an epoch (verified",
    "max change 0 exactly), so a construct named 'state architecture' or",
    "'flexibility' must carry at least one temporal-ordering term. Conversely,",
    "state_switch_rate alone discards repertoire breadth entirely. Both",
    "one-component reductions are reported in section 9 as reference domains",
    "(occupancy_entropy_only, switch_rate_only) so the reader can see exactly what",
    "each dimension contributes."
  ),
  pca_caveat = paste(
    "HONEST CAVEAT: Kaiser's rule on the 5 prespecified candidates returns only 1",
    "eigenvalue > 1 in 3 of 4 resolution x phase cells (2 in 5min Inactive),",
    "because 3 of those 5 candidates are the same persistence dimension counted",
    "three times, which inflates PC1 to 68-82%. On the deduplicated 3-variable set",
    "(one representative per redundancy class) the eigen-structure is the",
    "interpretable one: a joint repertoire/switching axis plus a separate",
    "co-location axis. Two components is therefore justified by the coupling",
    "structure and by shuffle-invariance, NOT by Kaiser's rule on the raw 5."
  ),
  excluded_and_why = paste(
    "inactive_state_fraction: depends on the ordered semantic classifier, which",
    "labels the argmax-Proximity_z state 'inactive/low-exploration' at both",
    "resolutions. top_proximity_state_fraction: a co-location occupancy measure,",
    "not state architecture. mean_dwell_hours / switches_per_hour: bin-size",
    "dependent. transition_entropy_mm: a bias-corrected variant of a dropped term."
  ),
  derivation_order = paste(
    "DERIVED FROM COUPLING / REDUNDANCY / SCALE STRUCTURE ONLY, BEFORE any Group",
    "variable was read. Section 9 contrasts are post-hoc consequences."
  ),
  not_a_significance_selection = paste(
    "No weight, threshold, resolution or FDR family was tuned. The two weights are",
    "equal by construction because the PCA shows two comparable dimensions."
  )
)
cat(proposal$formula, "\n")
cat("measures:", proposal$what_it_measures, "\n")

proposal_structure <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(phases, function(ph) {
    d <- analysis_tbl %>% filter(resolution == res, PhaseClass == ph)
    a <- animal_level %>% filter(resolution == res, PhaseClass == ph)
    sp <- function(x, y) suppressWarnings(cor(x, y, method = "spearman"))
    tibble(
      resolution = res, PhaseClass = ph, n_epochs = nrow(d), n_animals = nrow(a),
      cor_terms_z_occent_vs_z_switch = cor(d$az_occupancy_entropy, d$az_state_switch_rate),
      cor_terms_animal_level = cor(a$occupancy_entropy, a$state_switch_rate),
      shared_variance_of_terms = cor(d$az_occupancy_entropy, d$az_state_switch_rate)^2,
      effective_n_dimensions_of_proposal =
        2 / (1 + cor(d$az_occupancy_entropy, d$az_state_switch_rate)^2),
      var_share_occent_term = var(0.5 * d$az_occupancy_entropy) / var(d$proposed_flexibility),
      var_share_switch_term = var(0.5 * d$az_state_switch_rate) / var(d$proposed_flexibility),
      # what an equal-weight mean of two correlated unit-variance z terms is worth:
      variance_of_proposal = var(d$proposed_flexibility),
      variance_predicted_0.5_times_1_plus_r =
        0.5 * (1 + cor(d$az_occupancy_entropy, d$az_state_switch_rate)),
      cor_proposed_with_historical_epoch = cor(d$proposed_flexibility, d$historical_composite),
      cor_proposed_with_historical_animal = cor(a$proposed_flexibility, a$historical_composite),
      spearman_proposed_vs_observed_fraction_epoch = sp(d$proposed_flexibility, d$observed_fraction),
      spearman_historical_vs_observed_fraction_epoch = sp(d$historical_composite, d$observed_fraction),
      spearman_proposed_vs_ntransitions_epoch = sp(d$proposed_flexibility, d$n_transitions),
      cor_proposed_gapaware_vs_shipped = cor(d$proposed_flexibility, d$proposed_flexibility_gapaware)
    )
  })
})
cat("\ninternal structure of the proposal (no Group involved):\n")
print(as.data.frame(proposal_structure), digits = 4)

cross_res_proposed <- animal_level %>%
  select(resolution, PhaseClass, AnimalNum, proposed_flexibility, historical_composite) %>%
  pivot_longer(c(proposed_flexibility, historical_composite),
    names_to = "construct", values_to = "v") %>%
  pivot_wider(names_from = resolution, values_from = v) %>%
  group_by(PhaseClass, construct) %>%
  summarise(
    n = n(),
    cross_resolution_spearman = suppressWarnings(cor(`5min_based`, `10min_based`, method = "spearman")),
    cross_resolution_pearson = cor(`5min_based`, `10min_based`),
    .groups = "drop"
  )
cat("\ncross-resolution animal-level agreement (comparability requirement iii):\n")
print(as.data.frame(cross_res_proposed), digits = 4)

write_table(
  bind_rows(
    proposal %>% mutate(block = "8c_proposal") %>% mutate(across(everything(), as.character)),
    proposal_structure %>% mutate(block = "8c_structure") %>% mutate(across(everything(), as.character)),
    cross_res_proposed %>% mutate(block = "8c_cross_resolution") %>% mutate(across(everything(), as.character))
  ) %>% relocate(block),
  file.path(audit_out, "hmm_architecture_proposed_construct_definition.csv")
)

# --------------------------------------------------------------------------------
# 9. POST-HOC ONLY: group contrasts of the FIXED construct
#    The construct was fixed in section 8 with no reference to Group. This section
#    reports the consequence, using the repo estimator unchanged (random intercept
#    retained, CC1-CC4 retained). The FDR family is explicitly an AUDIT family and
#    does NOT redefine the shipped primary heatmap family.
# --------------------------------------------------------------------------------
cat("\n\n======= 9. POST-HOC group contrasts of the FIXED construct =======\n")
cat("(the construct definition was frozen in section 8 before any group comparison)\n")

domain_long <- analysis_tbl %>%
  transmute(
    resolution, AnimalNum, Group, Sex, CageChangeIndex, PhaseClass,
    historical_composite, proposed_flexibility, proposed_flexibility_gapaware,
    naive_formula,
    # one-component reference domains, so the contribution of each independent
    # dimension is visible rather than inferred:
    occupancy_entropy_only = az_occupancy_entropy,
    switch_rate_only = az_state_switch_rate,
    top_proximity_fraction_only = az_top_proximity_state_fraction
  ) %>%
  pivot_longer(
    c(historical_composite, proposed_flexibility, proposed_flexibility_gapaware,
      naive_formula, occupancy_entropy_only, switch_rate_only,
      top_proximity_fraction_only),
    names_to = "Domain", values_to = "DomainScore"
  )

contrast_tbl <- purrr::map_dfr(resolutions, function(res) {
  purrr::map_dfr(sort(unique(domain_long$Domain)), function(dom) {
    purrr::map_dfr(phases, function(ph) {
      fit <- fit_repeated_measures_domain_contrasts(
        domain_long %>% filter(resolution == res), dom, ph
      )
      fit$contrasts %>% mutate(resolution = res, .before = 1)
    })
  })
}) %>%
  group_by(resolution, Domain, Sex, PhaseClass) %>%
  mutate(
    AUDIT_FDR_q = p.adjust(mixed_model_p, method = "BH"),
    AUDIT_FDR_family_id = paste("AUDIT_ONLY__3_group_contrasts",
      resolution, Domain, Sex, PhaseClass, sep = "__"),
    n_tests_in_audit_family = sum(is.finite(mixed_model_p))
  ) %>%
  ungroup() %>%
  mutate(
    fdr_family_note = paste(
      "AUDIT-ONLY family (3 contrasts within resolution x construct x Sex x Phase).",
      "The shipped primary family is",
      "displayed_domains_x_3_group_contrasts__<res>__<Sex>__<Phase> (18 tests) and",
      "is NOT redefined here."
    ),
    evaluation_order = paste(
      "construct fixed in section 8 before any group comparison;",
      "this table is a post-hoc consequence, not a selection criterion"
    )
  )

write_table(contrast_tbl, file.path(audit_out, "hmm_architecture_proposed_construct_contrasts.csv"))
cat("wrote hmm_architecture_proposed_construct_contrasts.csv  rows =", nrow(contrast_tbl), "\n\n")

print(contrast_tbl %>%
  select(resolution, Domain, PhaseClass, Sex, contrast, n_ref_animals, n_comp_animals,
    mixed_model_estimate, mixed_model_SE, mixed_model_p, animal_level_hedges_g, AUDIT_FDR_q) %>%
  arrange(PhaseClass, Sex, Domain, contrast, resolution) %>%
  as.data.frame(), digits = 4)

# --------------------------------------------------------------------------------
# 10. POST-HOC read-density sensitivity of the proposed construct.
#     RFID read density (observed_fraction) is the blocking confound the verifier
#     established for the shipped construct. This is a COVARIATE SENSITIVITY, not a
#     replacement estimator: the primary numbers stay the section-9 helper output.
#     Random intercept retained; CC1-CC4 retained.
# --------------------------------------------------------------------------------
cat("\n\n======= 10. read-density covariate sensitivity (post-hoc) =======\n")
if (requireNamespace("lmerTest", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)) {
  covariate_tbl <- purrr::map_dfr(resolutions, function(res) {
    purrr::map_dfr(c("historical_composite", "proposed_flexibility"), function(dom) {
      purrr::map_dfr(phases, function(ph) {
        d <- analysis_tbl %>%
          filter(resolution == res, PhaseClass == ph) %>%
          transmute(
            AnimalNum = factor(AnimalNum),
            Group = factor(Group, levels = c("CON", "RES", "SUS")),
            Sex = factor(Sex, levels = c("Female", "Male")),
            CageChangeIndex = factor(CageChangeIndex),
            observed_fraction,
            DomainScore = .data[[dom]]
          )
        fit <- suppressMessages(suppressWarnings(lmerTest::lmer(
          DomainScore ~ Group * Sex + factor(CageChangeIndex) + observed_fraction + (1 | AnimalNum),
          data = d
        )))
        cf <- summary(fit)$coefficients
        emm <- suppressMessages(emmeans::emmeans(fit, ~ Group | Sex))
        ct <- suppressMessages(as.data.frame(emmeans::contrast(
          emm,
          method = list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1)),
          adjust = "none"
        )))
        tibble(
          resolution = res, Domain = dom, PhaseClass = ph,
          Sex = as.character(ct$Sex), contrast = as.character(ct$contrast),
          adjusted_estimate = ct$estimate, adjusted_SE = ct$SE, adjusted_p = ct$p.value,
          observed_fraction_slope = cf["observed_fraction", "Estimate"],
          observed_fraction_slope_p = cf["observed_fraction", "Pr(>|t|)"],
          model_formula = "DomainScore ~ Group*Sex + factor(CageChangeIndex) + observed_fraction + (1|AnimalNum)",
          note = "COVARIATE SENSITIVITY ONLY; the primary estimator is the unchanged fit_repeated_measures_domain_contrasts() output in section 9"
        )
      })
    })
  })
  write_table(covariate_tbl,
    file.path(audit_out, "hmm_architecture_proposed_construct_readdensity_sensitivity.csv"))
  cat("wrote hmm_architecture_proposed_construct_readdensity_sensitivity.csv  rows =",
    nrow(covariate_tbl), "\n\n")
  print(covariate_tbl %>%
    select(resolution, Domain, PhaseClass, Sex, contrast, adjusted_estimate, adjusted_SE,
      adjusted_p, observed_fraction_slope, observed_fraction_slope_p) %>%
    arrange(PhaseClass, Sex, Domain, contrast, resolution) %>%
    as.data.frame(), digits = 4)
} else {
  cat("lmerTest/emmeans unavailable; covariate sensitivity skipped\n")
}

cat("\n================ DONE ================\n")
