# ================================================================================
# AUDIT (read-only): DECOMPOSITION of the Stage 14 construct
#   `Behavioral state architecture`
# into its individual HMM latent-state components, each fitted with the repo's
# own corrected repeated-measures estimator.
#
# Testing/audit_hmm_state_architecture_component_models.R   (deliverable 2)
#
# WHAT THIS DOES
#   Instead of chasing one composite, this script runs the SAME corrected
#   repeated-measures model
#       value ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)
#   with emmeans ~ Group | Sex and contrasts RES-CON / SUS-CON / SUS-RES,
#   separately on each of the seven prespecified component metrics, at both
#   resolutions, in both phases, in BOTH scalings (context-z and raw units), so
#   that we can see WHICH latent-state property (if any) carries the female
#   Active RES < SUS pattern.
#
#   The estimator is NOT re-implemented: fit_repeated_measures_domain_contrasts()
#   from Functions/hmm_stage14_helpers.R is called unchanged (HARD CONSTRAINT 4).
#   The random intercept on AnimalNum and all four cage changes are retained.
#
# WHAT THIS IS NOT
#   This is an EXPLORATORY / HYPOTHESIS-GENERATING decomposition. Its FDR
#   families are declared separately and prefixed AUDIT_COMPONENT_SCAN_* /
#   AUDIT_REFERENCE_SCORE_*. It does NOT touch, redefine or re-share the primary
#   Stage 14 heatmap FDR family
#   (displayed_domains_x_3_group_contrasts__<res>__<Sex>__<Phase>), which remains
#   the only confirmatory family. q values produced here do NOT confer
#   confirmatory status on anything.
#
#   No construct here is selected on the basis of its p-value. The
#   unit_weighted_variant is included ONLY as a documented post-hoc reweighting
#   sensitivity and is labelled as such in construct_status.
#
# TERMINOLOGY GUARD
#   RFID "Proximity" is a social-spatial co-location proxy, NOT measured
#   sociability. top_proximity_state_fraction is the occupancy of the label-free
#   argmax-Proximity_z state (a low-activity high-co-occupancy state at both
#   resolutions). No state is renamed "social".
#
# Reads : AUDIT_OUT/hmm_architecture_component_epoch_metrics.csv
#         (built by Testing/audit_hmm_state_architecture_components.R v2)
#         Stage 08 hmm_state_summary.csv (label-free top-proximity check only)
# Writes: AUDIT_OUT/hmm_architecture_component_results.csv
#         AUDIT_OUT/hmm_architecture_component_interactions.csv
#         AUDIT_OUT/hmm_architecture_component_rankings.csv
#         AUDIT_OUT/hmm_architecture_component_redundancy_mirror_check.csv
# Modifies nothing under Analysis/ or Functions/.
# ================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(readr)
})

options(width = 220)

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
roster_bin_level <- "5min_based"

cat("================================================================\n")
cat("AUDIT deliverable 2: per-component repeated-measures decomposition\n")
cat("repo root :", MMM_REPO_ROOT, "\n")
cat("audit out :", audit_out, "\n")
cat("================================================================\n\n")

# --------------------------------------------------------------------------------
# 1. Canonical 111-animal roster, derived exactly as Stage 08 derives it
# --------------------------------------------------------------------------------
canonical_roster_file <- file.path(
  project_root, "analysis_ready/03_derived_metrics", roster_bin_level, "all_behavior_metrics.csv"
)
if (!file.exists(canonical_roster_file)) {
  stop("Canonical Stage 01 roster input is missing: ", canonical_roster_file, call. = FALSE)
}
canonical_roster <- build_canonical_identity_roster(
  readr::read_csv(
    canonical_roster_file,
    col_types = readr::cols(
      .default = readr::col_skip(),
      AnimalNum = readr::col_character(),
      Group = readr::col_character(),
      Sex = readr::col_character()
    ),
    progress = FALSE
  ),
  paste0("Stage 01 ", roster_bin_level, " roster")
)
stopifnot(nrow(canonical_roster) == 111L)
cat("[1] canonical roster animals:", nrow(canonical_roster), "\n\n")

# --------------------------------------------------------------------------------
# 2. Foundation component table (identity-audited on load)
# --------------------------------------------------------------------------------
comp_path <- file.path(audit_out, "hmm_architecture_component_epoch_metrics.csv")
if (!file.exists(comp_path)) {
  stop("Foundation component table is missing. Run ",
    "Testing/audit_hmm_state_architecture_components.R first: ", comp_path, call. = FALSE)
}
epochs_raw <- readr::read_csv(
  comp_path,
  col_types = readr::cols(
    AnimalNum = readr::col_character(),
    CageChange = readr::col_character(),
    .default = readr::col_guess()
  ),
  progress = FALSE, show_col_types = FALSE
)
epoch_audit <- audit_hmm_identity(
  epochs_raw, canonical_roster, "audit foundation component epoch metrics"
)
assert_hmm_identity_audit(epoch_audit)
epochs <- epoch_audit$data %>%
  mutate(
    AnimalNum = canonical_animal_id(AnimalNum),
    CageChangeIndex = as.integer(str_extract(CageChange, "\\d+"))
  )
cat("[2] foundation table:", nrow(epochs), "rows,", ncol(epochs), "cols\n")
print(as.data.frame(
  epochs %>% count(resolution, PhaseClass, name = "n_epochs") %>%
    left_join(epochs %>% group_by(resolution) %>%
      summarise(n_animals = n_distinct(AnimalNum), .groups = "drop"), by = "resolution")
))
cat("    identity audit passed:", epoch_audit$summary$passed, "\n\n")

# --------------------------------------------------------------------------------
# 3. CONFIRM (do not assume) the composite reductions
# --------------------------------------------------------------------------------
reduction_check <- epochs %>%
  group_by(resolution) %>%
  summarise(
    n_epochs = n(),
    max_abs_social_z = max(abs(social_state_fraction_z), na.rm = TRUE),
    var_social_raw = var(social_state_fraction, na.rm = TRUE),
    max_abs_diff_reduced_vs_shipped = max(abs(
      `Behavioral state architecture` -
        (0.5 * occupancy_entropy_z - inactive_state_fraction_z)
    ), na.rm = TRUE),
    max_abs_diff_shipped_z_vs_mine = max(
      c(abs(occupancy_entropy_z - shipped_state_occupancy_entropy_z),
        abs(inactive_state_fraction_z - shipped_inactive_state_fraction_z)),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
cat("[3] composite-reduction confirmation (NOT assumed):\n")
print(as.data.frame(reduction_check))
if (any(reduction_check$max_abs_diff_reduced_vs_shipped > 1e-10)) {
  stop("reduced_composite does NOT equal the shipped composite. Investigate before proceeding.")
}
cat("    -> reduced_composite == shipped `Behavioral state architecture` (confirmed)\n\n")

# --------------------------------------------------------------------------------
# 4. Construct registry
# --------------------------------------------------------------------------------
component_registry <- tribble(
  ~construct, ~raw_col, ~z_col, ~raw_unit, ~construct_note,
  "occupancy_entropy", "occupancy_entropy", "occupancy_entropy_z",
  "nats (Shannon, max log4 = 1.3863)",
  "Shannon entropy of the 4 state occupancies (frac_time); pure bin-count quantity, unaffected by within-epoch time gaps.",
  "inactive_state_fraction", "inactive_state_fraction", "inactive_state_fraction_z",
  "fraction of epoch bins",
  "Pooled occupancy of states whose CURRENT semantic label is inactive/low-exploration (S1+S3+S4 at 5min = 1-frac(S2); S1+S4 at 10min). Pure bin-count quantity.",
  "top_proximity_state_fraction", "top_proximity_state_fraction", "top_proximity_state_fraction_z",
  "fraction of epoch bins",
  "LABEL-FREE occupancy of the single argmax-Proximity_z state (S1 at 5min, S4 at 10min); a low-activity HIGH-CO-OCCUPANCY state, not a sociability measure.",
  "transition_entropy", "transition_entropy", "transition_entropy_z",
  "nats per step (max log4)",
  "Empirical entropy RATE of the epoch Markov chain, length-normalized by construction. Plug-in estimator; 0.46-0.93 pct of transition pairs bridge a ~12 h phase gap.",
  "state_switch_rate", "state_switch_rate", "state_switch_rate_z",
  "per-step switch probability",
  "n_switches / n_transitions. EXACT complement of self_transition_probability (residual 0), so it is the SAME measurement with the sign flipped -- not independent evidence.",
  "self_transition_probability", "self_transition_probability", "self_transition_probability_z",
  "probability",
  "Occupancy(pi)-weighted mean diagonal of P. Exact complement of state_switch_rate; near-deterministic monotone transform of mean_dwell_bins.",
  "mean_dwell_bins", "mean_dwell_bins", "mean_dwell_bins_z",
  "bins (5 min or 10 min bins)",
  "frac_time-weighted mean state dwell in BINS (comparable across resolutions; mean_dwell_hours is not). Merges bouts across within-epoch phase gaps (mean 10.54 -> 10.27 bins gap-aware at 5min)."
)

reference_registry <- tribble(
  ~construct, ~z_col, ~construct_status, ~construct_note,
  "historical_composite", "Behavioral state architecture", "prespecified",
  "The SHIPPED manuscript quantity: rowMeans(z(occ_entropy), z(social)) - z(inactive). Reproduced bit-identically from the Stage 14 artifact.",
  "reduced_composite", "reduced_composite", "reduced_form_of_prespecified",
  "0.5*z(occupancy_entropy) - z(inactive_state_fraction). Numerically IDENTICAL to historical_composite because social_state_fraction has zero variance (confirmed in section 3).",
  "unit_weighted_variant", "unit_weighted_variant", "post_hoc_reweighting_shown_for_transparency_only",
  "z(occupancy_entropy) - z(inactive_state_fraction). POST-HOC REWEIGHTING, shown only so the reader can see what the 0.5 weight (an artifact of rowMeans over one live and one dead term) does. NOT recommended, and NOT to be adopted on the basis of its p-value."
)

epochs <- epochs %>%
  mutate(
    reduced_composite = 0.5 * occupancy_entropy_z - inactive_state_fraction_z,
    unit_weighted_variant = occupancy_entropy_z - inactive_state_fraction_z
  )

missing_cols <- setdiff(
  c(component_registry$raw_col, component_registry$z_col, reference_registry$z_col),
  names(epochs)
)
if (length(missing_cols) > 0L) {
  stop("Foundation table lacks required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

top_prox_states <- map_dfr(resolutions, function(res) {
  ss <- readr::read_csv(
    resolve_configured_hmm_artifact(project_root, res, "hmm_state_summary.csv", required = TRUE)$path,
    col_types = readr::cols(State = readr::col_character()), progress = FALSE, show_col_types = FALSE
  )
  lab <- annotate_hmm_semantic_states(ss, res)
  tibble(
    resolution = res,
    top_proximity_state = lab$State[which.max(lab$Proximity_z)],
    top_proximity_state_label = lab$SemanticState[which.max(lab$Proximity_z)],
    n_states_labelled_social = sum(lab$SemanticState == "social")
  )
})
cat("[4] label-free argmax(Proximity_z) state per resolution:\n")
print(as.data.frame(top_prox_states))
cat("\n")

# --------------------------------------------------------------------------------
# 5. Long analysis frame (one row per epoch x construct x scaling)
# --------------------------------------------------------------------------------
long_components <- pmap_dfr(
  component_registry %>% select(construct, raw_col, z_col),
  function(construct, raw_col, z_col) {
    bind_rows(
      epochs %>% transmute(
        resolution, AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
        Domain = construct, scaling = "raw", DomainScore = as.numeric(.data[[raw_col]])
      ),
      epochs %>% transmute(
        resolution, AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
        Domain = construct, scaling = "context_z", DomainScore = as.numeric(.data[[z_col]])
      )
    )
  }
)

long_reference <- pmap_dfr(
  reference_registry %>% select(construct, z_col),
  function(construct, z_col) {
    epochs %>% transmute(
      resolution, AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
      Domain = construct, scaling = "context_z", DomainScore = as.numeric(.data[[z_col]])
    )
  }
)

long_all <- bind_rows(
  long_components %>% mutate(construct_family = "component_scan"),
  long_reference %>% mutate(construct_family = "reference_score")
)

cat("[5] long analysis frame:", nrow(long_all), "rows;",
  n_distinct(long_all$Domain), "constructs; non-finite DomainScore rows:",
  sum(!is.finite(long_all$DomainScore)), "\n\n")

# --------------------------------------------------------------------------------
# 6. Fit the corrected repeated-measures model for every cell
# --------------------------------------------------------------------------------
fit_grid <- long_all %>%
  distinct(resolution, scaling, Domain, construct_family) %>%
  crossing(PhaseClass = c("Active", "Inactive")) %>%
  arrange(resolution, scaling, construct_family, Domain, PhaseClass)

cat("[6] fitting", nrow(fit_grid), "models (resolution x scaling x construct x PhaseClass) ...\n")

fit_one <- function(resolution, scaling, Domain, construct_family, PhaseClass) {
  dat <- long_all %>%
    filter(.data$resolution == !!resolution, .data$scaling == !!scaling, .data$Domain == !!Domain)
  res <- fit_repeated_measures_domain_contrasts(dat, Domain, PhaseClass)
  list(
    contrasts = res$contrasts %>%
      mutate(resolution = resolution, scaling = scaling,
        construct_family = construct_family, .before = 1),
    interaction = res$interaction %>%
      mutate(resolution = resolution, scaling = scaling,
        construct_family = construct_family, .before = 1)
  )
}

all_fits <- pmap(fit_grid, fit_one)
contrasts_raw <- map_dfr(all_fits, "contrasts")
interactions_raw <- map_dfr(all_fits, "interaction")
cat("    fitted contrast rows:", nrow(contrasts_raw),
  " interaction rows:", nrow(interactions_raw), "\n")
print(as.data.frame(contrasts_raw %>% count(model_status, name = "n_rows")))
cat("\n")

# --------------------------------------------------------------------------------
# 7. Descriptive raw-unit context (SD so raw effects are physically meaningful)
# --------------------------------------------------------------------------------
value_moments <- long_all %>%
  filter(is.finite(DomainScore)) %>%
  group_by(resolution, scaling, Domain, PhaseClass) %>%
  summarise(value_sd_all_epochs = sd(DomainScore), value_mean_all_epochs = mean(DomainScore),
    .groups = "drop")
value_moments_sex <- long_all %>%
  filter(is.finite(DomainScore)) %>%
  group_by(resolution, scaling, Domain, PhaseClass, Sex) %>%
  summarise(value_sd_within_sex = sd(DomainScore), value_mean_within_sex = mean(DomainScore),
    n_epochs_in_model = n(), .groups = "drop")

# --------------------------------------------------------------------------------
# 8. FDR families (declared explicitly; primary Stage 14 family NOT touched)
# --------------------------------------------------------------------------------
family_component_scan_label <-
  "7 components x 3 contrasts, within resolution x Sex x PhaseClass, per scaling"
family_reference_label <-
  "3 reference scores x 3 contrasts, within resolution x Sex x PhaseClass, context-z scaling"

results <- contrasts_raw %>%
  rename(
    estimate = mixed_model_estimate, SE = mixed_model_SE, df = mixed_model_df,
    t = mixed_model_t, p_raw = mixed_model_p
  ) %>%
  mutate(
    ci_method = "estimate +/- qt(0.975, Satterthwaite df) * SE (df as returned by emmeans/lmerTest)",
    ci_lower = estimate - qt(0.975, df) * SE,
    ci_upper = estimate + qt(0.975, df) * SE,
    mean_difference = mean_comp - mean_ref
  ) %>%
  group_by(construct_family, resolution, scaling, Sex, PhaseClass) %>%
  mutate(
    FDR_q = p.adjust(p_raw, method = "BH"),
    n_tests_in_family = sum(is.finite(p_raw)),
    FDR_family_id = if_else(
      construct_family == "component_scan",
      paste("AUDIT_COMPONENT_SCAN", resolution, scaling, Sex, PhaseClass, sep = "__"),
      paste("AUDIT_REFERENCE_SCORE", resolution, scaling, Sex, PhaseClass, sep = "__")
    ),
    FDR_family_label = if_else(
      construct_family == "component_scan", family_component_scan_label, family_reference_label
    )
  ) %>%
  ungroup() %>%
  left_join(
    component_registry %>% transmute(Domain = construct, raw_unit, construct_note,
      construct_status = "prespecified"),
    by = "Domain"
  ) %>%
  left_join(
    reference_registry %>% transmute(Domain = construct,
      ref_status = construct_status, ref_note = construct_note),
    by = "Domain"
  ) %>%
  mutate(
    construct_status = coalesce(construct_status, ref_status),
    construct_note = coalesce(construct_note, ref_note),
    raw_unit = if_else(scaling == "context_z",
      "context-z units (SD within Sex x PhaseClass x CageChangeIndex)",
      raw_unit)
  ) %>%
  select(-ref_status, -ref_note) %>%
  left_join(value_moments, by = c("resolution", "scaling", "Domain", "PhaseClass")) %>%
  left_join(value_moments_sex, by = c("resolution", "scaling", "Domain", "PhaseClass", "Sex")) %>%
  left_join(top_prox_states %>% select(resolution, top_proximity_state), by = "resolution")

# --------------------------------------------------------------------------------
# 9. Resolution sensitivity summary (sign / CI overlap / |ratio|)
# --------------------------------------------------------------------------------
res_sens <- results %>%
  select(construct_family, scaling, Domain, PhaseClass, Sex, contrast,
    resolution, estimate, ci_lower, ci_upper, p_raw) %>%
  pivot_wider(names_from = resolution, values_from = c(estimate, ci_lower, ci_upper, p_raw)) %>%
  mutate(
    same_sign = sign(estimate_5min_based) == sign(estimate_10min_based),
    ci_overlap = (ci_lower_5min_based <= ci_upper_10min_based) &
      (ci_lower_10min_based <= ci_upper_5min_based),
    abs_estimate_ratio_5min_over_10min = abs(estimate_5min_based) / abs(estimate_10min_based),
    resolution_sensitivity = sprintf(
      "5min est %.4f [%.4f, %.4f] p=%.4f | 10min est %.4f [%.4f, %.4f] p=%.4f | same_sign=%s; CI_overlap=%s; |est5|/|est10|=%.3f",
      estimate_5min_based, ci_lower_5min_based, ci_upper_5min_based, p_raw_5min_based,
      estimate_10min_based, ci_lower_10min_based, ci_upper_10min_based, p_raw_10min_based,
      same_sign, ci_overlap, abs_estimate_ratio_5min_over_10min
    )
  ) %>%
  select(construct_family, scaling, Domain, PhaseClass, Sex, contrast,
    resolution_sensitivity, resolution_same_sign = same_sign,
    resolution_ci_overlap = ci_overlap, abs_estimate_ratio_5min_over_10min)

results <- results %>%
  left_join(res_sens,
    by = c("construct_family", "scaling", "Domain", "PhaseClass", "Sex", "contrast")) %>%
  mutate(
    inference_status = if_else(
      construct_family == "component_scan",
      "EXPLORATORY / HYPOTHESIS-GENERATING component decomposition. FDR q within the AUDIT_COMPONENT_SCAN family does NOT confer confirmatory status and does NOT replace or modify the primary Stage 14 heatmap family displayed_domains_x_3_group_contrasts__<res>__<Sex>__<Phase>.",
      "Reference score, reported in its OWN 9-test family. The shipped composite's confirmatory status is unchanged and is governed solely by the primary Stage 14 heatmap FDR family."
    ),
    scaling_note = if_else(
      scaling == "context_z",
      "PRIMARY scaling: score is z-scored within Sex x PhaseClass x CageChangeIndex, so the Sex main effect and the factor(CageChangeIndex) fixed effect are ABSORBED BY CONSTRUCTION (each context cell is centred at mean 0). Group contrasts WITHIN Sex remain estimable; n-weighted group means inside a cell are constrained to sum to zero.",
      "SECONDARY scaling: raw physical units (see raw_unit). Here Sex and factor(CageChangeIndex) are genuine, non-absorbed fixed effects. value_sd_all_epochs / value_sd_within_sex are provided so a raw estimate can be read as a fraction of an SD."
    ),
    primary_heatmap_family_untouched = TRUE
  ) %>%
  arrange(construct_family, scaling, Domain, PhaseClass, Sex, contrast, resolution) %>%
  select(
    construct_family, Domain, construct_status, scaling, resolution, PhaseClass, Sex, contrast,
    estimate, SE, df, t, ci_lower, ci_upper, ci_method,
    p_raw, FDR_q, FDR_family_id, FDR_family_label, n_tests_in_family,
    n_ref_animals, n_comp_animals, mean_ref, mean_comp, mean_difference, animal_level_hedges_g,
    raw_unit, value_mean_all_epochs, value_sd_all_epochs,
    value_mean_within_sex, value_sd_within_sex, n_epochs_in_model,
    model_engine, model_formula, model_status, model_warnings,
    significance_method, effect_size_method,
    resolution_sensitivity, resolution_same_sign, resolution_ci_overlap,
    abs_estimate_ratio_5min_over_10min,
    top_proximity_state, construct_note, scaling_note, inference_status,
    primary_heatmap_family_untouched
  )

results_path <- file.path(audit_out, "hmm_architecture_component_results.csv")
write_table(results, results_path)
cat("[9] wrote", results_path, "-", nrow(results), "rows x", ncol(results), "cols\n\n")

# --------------------------------------------------------------------------------
# 10. Group x Sex interaction F tests
# --------------------------------------------------------------------------------
interactions <- interactions_raw %>%
  rename(F_value = statistic, p_raw = p.value) %>%
  left_join(
    component_registry %>% transmute(Domain = construct,
      construct_status = "prespecified", construct_note),
    by = "Domain"
  ) %>%
  left_join(
    reference_registry %>% transmute(Domain = construct,
      ref_status = construct_status, ref_note = construct_note),
    by = "Domain"
  ) %>%
  mutate(
    construct_status = coalesce(construct_status, ref_status),
    construct_note = coalesce(construct_note, ref_note),
    model_formula = "DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)",
    model_engine = "lmerTest::lmer, type-III anova (Satterthwaite)",
    inference_status = "EXPLORATORY. No FDR correction applied to these interaction F tests; they are descriptive omnibus checks of whether the Group effect differs by Sex. Note that under context-z scaling Sex is centred by construction, so the interaction tests whether the WITHIN-SEX group pattern differs between sexes."
  ) %>%
  select(-ref_status, -ref_note) %>%
  arrange(construct_family, scaling, Domain, PhaseClass, resolution)

interactions_path <- file.path(audit_out, "hmm_architecture_component_interactions.csv")
write_table(interactions, interactions_path)
cat("[10] wrote", interactions_path, "-", nrow(interactions), "rows\n\n")

# --------------------------------------------------------------------------------
# 11. Redundancy / mirror check
# --------------------------------------------------------------------------------
mirror <- results %>%
  filter(Domain %in% c("state_switch_rate", "self_transition_probability")) %>%
  select(resolution, scaling, PhaseClass, Sex, contrast, Domain,
    estimate, SE, p_raw, animal_level_hedges_g) %>%
  pivot_wider(names_from = Domain,
    values_from = c(estimate, SE, p_raw, animal_level_hedges_g)) %>%
  mutate(
    sum_of_estimates = estimate_state_switch_rate + estimate_self_transition_probability,
    abs_diff_SE = abs(SE_state_switch_rate - SE_self_transition_probability),
    abs_diff_p = abs(p_raw_state_switch_rate - p_raw_self_transition_probability),
    sum_of_hedges_g = animal_level_hedges_g_state_switch_rate +
      animal_level_hedges_g_self_transition_probability
  )
mirror_summary <- mirror %>%
  group_by(resolution, scaling) %>%
  summarise(
    n_cells = n(),
    max_abs_sum_of_estimates = max(abs(sum_of_estimates)),
    max_abs_diff_SE = max(abs_diff_SE),
    max_abs_diff_p = max(abs_diff_p),
    max_abs_sum_of_hedges_g = max(abs(sum_of_hedges_g)),
    interpretation = "state_switch_rate == 1 - self_transition_probability EXACTLY, so the two models are the same fit with the sign flipped: estimates are exact negatives, SE / |t| / p / |g| identical. They are NOT independent evidence and must never be double-counted in a composite.",
    .groups = "drop"
  )
epoch_complement <- epochs %>%
  group_by(resolution) %>%
  summarise(
    max_abs_complementarity_residual = max(abs(state_switch_rate + self_transition_probability - 1)),
    pearson_switch_vs_self = cor(state_switch_rate, self_transition_probability),
    spearman_dwell_vs_self = cor(mean_dwell_bins, self_transition_probability, method = "spearman"),
    spearman_dwell_vs_switch = cor(mean_dwell_bins, state_switch_rate, method = "spearman"),
    spearman_transentropy_vs_occentropy = cor(transition_entropy, occupancy_entropy, method = "spearman"),
    spearman_topprox_vs_inactive = cor(top_proximity_state_fraction, inactive_state_fraction, method = "spearman"),
    .groups = "drop"
  )
mirror_out <- mirror_summary %>% left_join(epoch_complement, by = "resolution")
mirror_path <- file.path(audit_out, "hmm_architecture_component_redundancy_mirror_check.csv")
write_table(mirror_out, mirror_path)
cat("[11] redundancy / mirror check:\n")
print(as.data.frame(mirror_out %>% select(-interpretation)))
cat("\n")

# --------------------------------------------------------------------------------
# 12. Rankings: which component drives female Active RES<SUS?
# --------------------------------------------------------------------------------
rank_cells <- results %>%
  filter(construct_family == "component_scan", scaling == "context_z",
    Sex == "Female", PhaseClass == "Active",
    contrast %in% c("SUS-RES", "RES-CON")) %>%
  mutate(abs_g = abs(animal_level_hedges_g), abs_t = abs(t)) %>%
  group_by(resolution, contrast) %>%
  mutate(
    rank_by_abs_hedges_g = rank(-abs_g, ties.method = "min"),
    rank_by_abs_t = rank(-abs_t, ties.method = "min"),
    rank_by_p = rank(p_raw, ties.method = "min")
  ) %>%
  ungroup() %>%
  select(resolution, PhaseClass, Sex, contrast, Domain, estimate, SE, t, p_raw, FDR_q,
    animal_level_hedges_g, abs_g, rank_by_abs_hedges_g, rank_by_abs_t, rank_by_p,
    ci_lower, ci_upper, construct_status)

rank_wide <- rank_cells %>%
  select(contrast, Domain, resolution, rank_by_abs_hedges_g, rank_by_abs_t,
    abs_g, estimate, p_raw) %>%
  pivot_wider(names_from = resolution,
    values_from = c(rank_by_abs_hedges_g, rank_by_abs_t, abs_g, estimate, p_raw)) %>%
  mutate(
    rank_g_consistent = rank_by_abs_hedges_g_5min_based == rank_by_abs_hedges_g_10min_based,
    rank_t_consistent = rank_by_abs_t_5min_based == rank_by_abs_t_10min_based,
    same_sign = sign(estimate_5min_based) == sign(estimate_10min_based)
  )

rank_path <- file.path(audit_out, "hmm_architecture_component_rankings.csv")
write_table(rank_cells %>% arrange(contrast, resolution, rank_by_abs_hedges_g), rank_path)
cat("[12] wrote", rank_path, "\n\n")

cat("--- FEMALE ACTIVE, context-z, ranked by |Hedges g| ---\n")
for (ct in c("SUS-RES", "RES-CON")) {
  for (res in resolutions) {
    cat("\n>>", ct, "|", res, "\n")
    print(as.data.frame(
      rank_cells %>% filter(contrast == ct, resolution == res) %>%
        arrange(rank_by_abs_hedges_g) %>%
        transmute(Domain, estimate = round(estimate, 4), SE = round(SE, 4),
          t = round(t, 3), p_raw = signif(p_raw, 3), FDR_q = signif(FDR_q, 3),
          g = round(animal_level_hedges_g, 3),
          rank_g = rank_by_abs_hedges_g, rank_t = rank_by_abs_t, rank_p = rank_by_p)
    ))
  }
}
cat("\n--- rank consistency across resolutions (female Active, context-z) ---\n")
print(as.data.frame(rank_wide %>% arrange(contrast, rank_by_abs_hedges_g_5min_based)))

cat("\n--- Spearman of the two resolutions' |g| values (female Active) ---\n")
print(as.data.frame(
  rank_cells %>% select(contrast, Domain, resolution, abs_g) %>%
    pivot_wider(names_from = resolution, values_from = abs_g) %>%
    group_by(contrast) %>%
    summarise(spearman_absg_5_vs_10 = cor(`5min_based`, `10min_based`, method = "spearman"),
      pearson_absg_5_vs_10 = cor(`5min_based`, `10min_based`), .groups = "drop")
))

# --------------------------------------------------------------------------------
# 12b. EXACT ADDITIVE DECOMPOSITION of the shipped composite's contrast estimate
# --------------------------------------------------------------------------------
# The composite is a LINEAR combination of two context-z terms fitted on the same
# rows with the same design, so the emmeans contrast of the composite MUST equal
#   0.5 * contrast(occupancy_entropy_z)  -  contrast(inactive_state_fraction_z)
# exactly. Verifying this turns "which component drives the effect" from a
# ranking exercise into an arithmetic identity, and lets us attribute an exact
# share of the composite estimate to each component.
decomp <- results %>%
  filter(scaling == "context_z",
    Domain %in% c("occupancy_entropy", "inactive_state_fraction", "historical_composite")) %>%
  select(resolution, PhaseClass, Sex, contrast, Domain, estimate) %>%
  pivot_wider(names_from = Domain, values_from = estimate) %>%
  mutate(
    term_entropy_contribution = 0.5 * occupancy_entropy,
    term_inactive_contribution = -inactive_state_fraction,
    predicted_composite = term_entropy_contribution + term_inactive_contribution,
    abs_reconstruction_error = abs(predicted_composite - historical_composite),
    share_entropy = term_entropy_contribution / historical_composite,
    share_inactive = term_inactive_contribution / historical_composite,
    dominant_term = if_else(abs(term_inactive_contribution) >= abs(term_entropy_contribution),
      "inactive_state_fraction", "occupancy_entropy"),
    terms_same_direction = sign(term_entropy_contribution) == sign(term_inactive_contribution)
  ) %>%
  arrange(PhaseClass, Sex, contrast, resolution)

decomp_path <- file.path(audit_out, "hmm_architecture_composite_additive_decomposition.csv")
write_table(decomp, decomp_path)
cat("[12b] wrote", decomp_path, "\n")
cat("     max |reconstruction error| =", max(decomp$abs_reconstruction_error), "\n")
print(as.data.frame(
  decomp %>% transmute(PhaseClass, Sex, contrast, resolution,
    composite = round(historical_composite, 4),
    from_half_z_entropy = round(term_entropy_contribution, 4),
    from_minus_z_inactive = round(term_inactive_contribution, 4),
    pct_entropy = round(100 * share_entropy, 1),
    pct_inactive = round(100 * share_inactive, 1),
    dominant_term, terms_same_direction,
    recon_err = signif(abs_reconstruction_error, 3))
))
cat("\n")

# --------------------------------------------------------------------------------
# 12c. LEAVE-OUT SENSITIVITY of the component-level phenotypes
# --------------------------------------------------------------------------------
# Two known fragilities were established upstream (verifier + repair pass) and
# must be carried through to the component level, because the dynamics components
# (mean_dwell_bins / self_transition_probability / state_switch_rate /
# transition_entropy) are far MORE exposed to them than the pure bin-count
# occupancy components:
#   (i)  OR539 and OR540 (Female CON) each contribute a DEGENERATE CC4 Inactive
#        epoch that is a single state in a single bout, so mean_dwell_bins = 144
#        (5min) / 72 (10min) bins against a typical ~10 bins, P(self) = 1,
#        switch rate = 0, transition entropy = 0. With only 12 CON females this is
#        extreme leverage on every Inactive-phase dynamics contrast.
#   (ii) 8 Female Active epochs are Stage 14 "exclude_after_dropout"
#        (qc_active_dropout_leaveout_flag), all in RES/SUS, none in CON.
# The estimator is again fit_repeated_measures_domain_contrasts() UNCHANGED; only
# the row set differs. Scores are NOT re-standardized (the deletion is a
# sensitivity on the model, not a redefinition of the construct).
leaveout_specs <- list(
  list(id = "full_data", phase = c("Active", "Inactive"),
    keep = function(d) d),
  list(id = "drop_animals_OR539_OR540", phase = "Inactive",
    keep = function(d) filter(d, !AnimalNum %in% canonical_animal_id(c("OR539", "OR540")))),
  list(id = "drop_8_active_qc_dropout_epochs", phase = "Active",
    keep = function(d) filter(d, !epoch_key %in% dropout_keys))
)

dropout_keys <- epochs %>%
  filter(qc_active_dropout_leaveout_flag) %>%
  transmute(k = paste(AnimalNum, CageChange, PhaseClass, sep = "|")) %>%
  pull(k) %>% unique()
cat("[12c] qc_active_dropout_leaveout_flag epochs (distinct keys):",
  length(dropout_keys), "\n")

long_lo <- long_all %>%
  filter(construct_family == "component_scan", scaling == "context_z") %>%
  mutate(epoch_key = paste(AnimalNum, CageChange, PhaseClass, sep = "|"))

leaveout <- map_dfr(leaveout_specs, function(spec) {
  dat_all <- spec$keep(long_lo)
  crossing(resolution = resolutions, Domain = component_registry$construct,
    PhaseClass = spec$phase) %>%
    pmap_dfr(function(resolution, Domain, PhaseClass) {
      d <- dat_all %>% filter(.data$resolution == !!resolution, .data$Domain == !!Domain)
      fit_repeated_measures_domain_contrasts(d, Domain, PhaseClass)$contrasts %>%
        mutate(row_set = spec$id, resolution = resolution, .before = 1)
    })
}) %>%
  transmute(row_set, resolution, Domain, PhaseClass, Sex, contrast,
    estimate = mixed_model_estimate, SE = mixed_model_SE, df = mixed_model_df,
    p_raw = mixed_model_p, animal_level_hedges_g,
    n_ref_animals, n_comp_animals, model_status,
    inference_status = "EXPLORATORY leave-out sensitivity of the exploratory component scan. No FDR applied; compare row_set == 'full_data' with the deletion rows.")

lo_wide <- leaveout %>%
  select(row_set, resolution, Domain, PhaseClass, Sex, contrast, estimate, SE, p_raw, animal_level_hedges_g) %>%
  pivot_wider(names_from = row_set, values_from = c(estimate, SE, p_raw, animal_level_hedges_g))

leaveout_path <- file.path(audit_out, "hmm_architecture_component_leaveout_sensitivity.csv")
write_table(leaveout, leaveout_path)
cat("     wrote", leaveout_path, "-", nrow(leaveout), "rows\n")

cat("\n     >> Inactive phase, dropping OR539+OR540 (Female CON degenerate epochs)\n")
print(as.data.frame(
  lo_wide %>% filter(PhaseClass == "Inactive", contrast %in% c("RES-CON", "SUS-CON")) %>%
    transmute(Domain, Sex, contrast, resolution,
      est_full = round(estimate_full_data, 4), p_full = signif(p_raw_full_data, 3),
      est_drop = round(estimate_drop_animals_OR539_OR540, 4),
      p_drop = signif(p_raw_drop_animals_OR539_OR540, 3),
      pct_of_full = round(100 * estimate_drop_animals_OR539_OR540 / estimate_full_data, 1)) %>%
    arrange(Sex, contrast, Domain, resolution)
))
cat("\n     >> Female Active, dropping the 8 QC dropout epochs\n")
print(as.data.frame(
  lo_wide %>% filter(PhaseClass == "Active", Sex == "Female") %>%
    transmute(Domain, contrast, resolution,
      est_full = round(estimate_full_data, 4), p_full = signif(p_raw_full_data, 3),
      est_drop = round(estimate_drop_8_active_qc_dropout_epochs, 4),
      p_drop = signif(p_raw_drop_8_active_qc_dropout_epochs, 3),
      pct_of_full = round(100 * estimate_drop_8_active_qc_dropout_epochs / estimate_full_data, 1)) %>%
    arrange(contrast, Domain, resolution)
))
cat("\n")

# --------------------------------------------------------------------------------
# 13. Console report
# --------------------------------------------------------------------------------
cat("\n\n=========== COMPONENT SCAN, context-z, ALL cells ===========\n")
for (ph in c("Active", "Inactive")) {
  for (sx in c("Female", "Male")) {
    cat("\n### PhaseClass =", ph, "| Sex =", sx, "\n")
    print(as.data.frame(
      results %>%
        filter(construct_family == "component_scan", scaling == "context_z",
          PhaseClass == ph, Sex == sx) %>%
        transmute(Domain, contrast, resolution,
          est = round(estimate, 4), SE = round(SE, 4),
          CI = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
          p = signif(p_raw, 3), q = signif(FDR_q, 3),
          g = round(animal_level_hedges_g, 3),
          n_ref = n_ref_animals, n_comp = n_comp_animals) %>%
        arrange(Domain, contrast, resolution)
    ))
  }
}

cat("\n\n=========== REFERENCE SCORES, context-z ===========\n")
print(as.data.frame(
  results %>% filter(construct_family == "reference_score") %>%
    transmute(Domain, construct_status, PhaseClass, Sex, contrast, resolution,
      est = round(estimate, 4), SE = round(SE, 4), p = signif(p_raw, 4),
      q_own_family = signif(FDR_q, 3), g = round(animal_level_hedges_g, 3)) %>%
    arrange(PhaseClass, Sex, contrast, Domain, resolution)
))

cat("\n\n=========== RAW-UNIT (secondary), Female ===========\n")
print(as.data.frame(
  results %>% filter(construct_family == "component_scan", scaling == "raw", Sex == "Female") %>%
    transmute(Domain, PhaseClass, contrast, resolution,
      est_raw = signif(estimate, 4), SE = signif(SE, 4),
      CI = sprintf("[%.4g, %.4g]", ci_lower, ci_upper),
      p = signif(p_raw, 3), g = round(animal_level_hedges_g, 3),
      sd_all = signif(value_sd_all_epochs, 4),
      est_over_sd = round(estimate / value_sd_all_epochs, 3)) %>%
    arrange(Domain, PhaseClass, contrast, resolution)
))

cat("\n\n=========== RAW-UNIT (secondary), Male ===========\n")
print(as.data.frame(
  results %>% filter(construct_family == "component_scan", scaling == "raw", Sex == "Male") %>%
    transmute(Domain, PhaseClass, contrast, resolution,
      est_raw = signif(estimate, 4), SE = signif(SE, 4),
      p = signif(p_raw, 3), g = round(animal_level_hedges_g, 3),
      sd_all = signif(value_sd_all_epochs, 4),
      est_over_sd = round(estimate / value_sd_all_epochs, 3)) %>%
    arrange(Domain, PhaseClass, contrast, resolution)
))

cat("\n\n=========== Group x Sex interaction F tests (context-z) ===========\n")
print(as.data.frame(
  interactions %>% filter(scaling == "context_z") %>%
    transmute(construct_family, Domain, PhaseClass, resolution,
      F_value = round(F_value, 3), df_num, df_den = round(df_den, 1),
      p = signif(p_raw, 3), model_status) %>%
    arrange(construct_family, Domain, PhaseClass, resolution)
))

cat("\n\n=========== Group x Sex interaction F tests (raw) ===========\n")
print(as.data.frame(
  interactions %>% filter(scaling == "raw") %>%
    transmute(Domain, PhaseClass, resolution,
      F_value = round(F_value, 3), df_num, df_den = round(df_den, 1),
      p = signif(p_raw, 3), model_status) %>%
    arrange(Domain, PhaseClass, resolution)
))

cat("\n\n=========== MODEL HEALTH ===========\n")
print(as.data.frame(results %>% count(model_status, name = "n_contrast_rows")))
cat("\nDistinct model warnings:\n")
wtab <- results %>% filter(nzchar(coalesce(model_warnings, ""))) %>%
  count(scaling, Domain, PhaseClass, model_warnings, name = "n_rows")
if (nrow(wtab) == 0) cat("  (none)\n") else print(as.data.frame(wtab))
cat("\nSingular fits:\n")
stab <- results %>% filter(model_status == "singular_fit") %>%
  distinct(resolution, scaling, Domain, PhaseClass)
if (nrow(stab) == 0) cat("  (none)\n") else print(as.data.frame(stab))
cat("\nNon-estimable / errored fits:\n")
etab <- results %>% filter(!model_status %in% c("fitted", "singular_fit")) %>%
  distinct(resolution, scaling, Domain, PhaseClass, model_status)
if (nrow(etab) == 0) cat("  (none)\n") else print(as.data.frame(etab))

cat("\nDONE.\n")
