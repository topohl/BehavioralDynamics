# ================================================================================
# AUDIT (read-only): three-way construct comparison for `Behavioral state
# architecture`
#
# Testing/audits/audit_hmm_state_architecture_construct_comparison.R
#
# Compares, side by side:
#   A  HISTORICAL construct exactly as written in build_hmm_epoch_scores():
#        rowMeans(cbind(z(state_occupancy_entropy), z(social_state_fraction)))
#          - z(inactive_state_fraction)
#   B  its ACTUAL REDUCED FORM given social variance = 0:
#        0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction)
#   C  the LABEL-FREE candidate PROPOSED BY THE REDUNDANCY DELIVERABLE
#      (Testing/audits/audit_hmm_state_architecture_redundancy.R, section 8c,
#      "latent_state_flexibility"), adopted here unchanged:
#        0.5 * z(occupancy_entropy) + 0.5 * z(state_switch_rate)
#   C_alt  an alternative label-free construct derived here, reported alongside
#      because the proposal's two terms are empirically correlated (r 0.45-0.87)
#      while these two are orthogonal by an exact identity:
#        mean( z(occupancy_entropy), z(sequential_mutual_information) )
#      with sequential_mutual_information = I(X_t ; X_{t+1}) in nats/step.
#   C_temporal_only = z(state_switch_rate) is the pure-dynamics reference.
#
# This script is an AUDIT. It reads repo helpers and Stage 08 / Stage 14
# artifacts and writes only into the Stage 14 audit folder. Nothing under
# Analysis/ or Functions/ is modified. The estimator is NOT re-implemented:
# fit_repeated_measures_domain_contrasts() is called unchanged.
#
# Terminology guard: RFID "Proximity" is a social-spatial co-location proxy, not
# measured sociability. No HMM state is renamed "social" here.
#
# Selection guard: C was chosen for what it measures (label-free, temporally
# informative, non-redundant), NOT for its p-values. Its group separation is
# reported as observed, including where it is weaker than A/B.
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
stage14_dir <- file.path(project_root, "analysis_ready/12_systems_neuroscience_summary/5min_based")
audit_out <- file.path(stage14_dir, "audit_hmm_state_architecture")
ensure_dir(audit_out)

resolutions <- c("5min_based", "10min_based")
roster_bin_level <- "5min_based"
SHUFFLE_SEED <- 20260902L

cat("================================================================\n")
cat("AUDIT: HMM state-architecture THREE-WAY CONSTRUCT COMPARISON\n")
cat("repo root   :", MMM_REPO_ROOT, "\n")
cat("project     :", project_root, "\n")
cat("audit out   :", audit_out, "\n")
cat("shuffle seed:", SHUFFLE_SEED, "\n")
cat("================================================================\n\n")

# --------------------------------------------------------------------------------
# 1. Canonical 111-animal roster, exactly as Stage 08 derives it
# --------------------------------------------------------------------------------
canonical_roster_file <- file.path(
  project_root, "analysis_ready/03_derived_metrics", roster_bin_level, "all_behavior_metrics.csv"
)
if (!file.exists(canonical_roster_file)) stop("Missing roster input: ", canonical_roster_file, call. = FALSE)
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
cat("[1] canonical roster animals:", nrow(canonical_roster), "\n")
stopifnot(nrow(canonical_roster) == 111L)

# --------------------------------------------------------------------------------
# 2. Load + identity-audit the HMM tables actually needed here
# --------------------------------------------------------------------------------
load_and_audit <- function(resolution, filename) {
  path <- resolve_configured_hmm_artifact(project_root, resolution, filename, required = TRUE)$path
  dat <- readr::read_csv(
    path,
    col_types = readr::cols(AnimalNum = readr::col_character()),
    progress = FALSE, show_col_types = FALSE
  ) %>%
    mutate(across(any_of(c("State", "NextState")), as.character))
  audit <- audit_hmm_identity(dat, canonical_roster, paste0(filename, " @ ", resolution))
  assert_hmm_identity_audit(audit)
  list(data = audit$data, summary = audit$summary)
}

identity_summaries <- list()
bundles <- list()
for (res in resolutions) {
  occ <- load_and_audit(res, "hmm_state_occupancy.csv")
  trp <- load_and_audit(res, "hmm_transition_probabilities.csv")
  asg <- load_and_audit(res, "hmm_state_assignments.csv")
  state_summary <- readr::read_csv(
    resolve_configured_hmm_artifact(project_root, res, "hmm_state_summary.csv", required = TRUE)$path,
    col_types = readr::cols(State = readr::col_character()),
    progress = FALSE, show_col_types = FALSE
  )
  bundles[[res]] <- list(
    occupancy = occ$data, transitions = trp$data, assignments = asg$data,
    state_summary = state_summary
  )
  identity_summaries <- c(identity_summaries, list(occ$summary, trp$summary, asg$summary))
}
identity_summary_tbl <- bind_rows(identity_summaries)
cat("[2] identity audits (all must be passed = TRUE):\n")
print(as.data.frame(identity_summary_tbl %>% select(
  source, input_rows, raw_animal_spellings, canonical_animals, aliases_merged,
  identity_conflicts, unknown_animals, metadata_disagreements, passed
)))
stopifnot(all(identity_summary_tbl$passed))
cat("    -> all identity audits PASSED\n\n")

# --------------------------------------------------------------------------------
# 3. Semantic labels (repo classifier, unmodified) + label-free argmax-proximity
# --------------------------------------------------------------------------------
state_labels <- imap(bundles, ~ annotate_hmm_semantic_states(.x$state_summary, .y))
top_prox_state <- setNames(rep(NA_character_, length(resolutions)), resolutions)
for (res in resolutions) {
  sl <- state_labels[[res]]
  top_prox_state[[res]] <- sl$State[which.max(sl$Proximity_z)] # label-free argmax
  cat("[3]", res, "state labels:\n")
  print(as.data.frame(sl %>% select(State, Movement_z, Entropy_z, Proximity_z, SemanticState)))
  cat("    label-free argmax(Proximity_z) = S", top_prox_state[[res]],
    " (current label: '", sl$SemanticState[sl$State == top_prox_state[[res]]], "')\n\n",
    sep = ""
  )
}

EPOCH_KEY <- c("AnimalNum", "Group", "Sex", "CageChange", "CageChangeIndex", "PhaseClass")

# PhaseClass / CageChangeIndex mapping copied verbatim from build_hmm_epoch_scores()
add_epoch_keys <- function(dat) {
  dat %>%
    mutate(
      AnimalNum = as.character(.data$AnimalNum),
      Group = as.character(.data$Group),
      Sex = as.character(.data$Sex),
      PhaseClass = case_when(
        str_detect(str_to_lower(as.character(.data$Phase)), "\\binactive\\b|\\blight\\b|\\bday\\b") ~ "Inactive",
        str_detect(str_to_lower(as.character(.data$Phase)), "\\bactive\\b|\\bdark\\b|\\bnight\\b") ~ "Active",
        TRUE ~ as.character(.data$Phase)
      ),
      CageChange = as.character(.data$CageChange),
      CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChange, "\\d+"))),
      State = as.character(.data$State)
    )
}

# ================================================================================
# SECTION 1 -- MATHEMATICAL IDENTITY OF A AND B
# ================================================================================
cat("\n================================================================\n")
cat("SECTION 1: mathematical identity of A (as written) and B (reduced)\n")
cat("================================================================\n")

# (1a) ALGEBRAIC PROOF, stated and then demonstrated numerically.
#
#   strict_standardize_within_context(dat, v) computes, WITHIN each
#   Sex x PhaseClass x CageChangeIndex cell (helpers:264-269):
#        s <- sd(x); m <- mean(x)
#        if (!is.finite(s) || s == 0) rep(0, length(x)) else (x - m)/s
#
#   social_state_fraction is identically 0 in every epoch (no fitted state
#   carries the "social" label at either resolution), hence sd = 0 in every
#   cell, hence z(social) = 0 EXACTLY for every row -- a real 0, not NA.
#
#   Therefore, for every row i:
#        rowMeans(cbind(a_i, 0)) = (a_i + 0)/2 = 0.5 * a_i
#   and
#        A_i = rowMeans(cbind(z_ent_i, z_soc_i)) - z_inact_i
#            = 0.5 * z_ent_i - z_inact_i = B_i        (identically, all i)
#
#   CONSEQUENCE (the failure mode): because the constant column is mapped to 0
#   rather than NA, rowMeans(..., na.rm = FALSE) still returns a FINITE number.
#   The composite silently ABSORBS a dead component -- it halves the weight of
#   the surviving entropy term instead of erroring or returning NA. The same
#   holds for a fully MISSING component (sd = NA is non-finite -> also 0),
#   demonstrated below.

cat("\n[1a] behaviour of strict_standardize_within_context() on degenerate columns\n")
degenerate_probe <- tibble(
  Sex = rep(c("Female", "Male"), each = 4),
  PhaseClass = rep(c("Active", "Inactive"), times = 4),
  CageChangeIndex = rep(1:2, times = 4),
  constant_col = 0,
  constant_nonzero_col = 7,
  all_na_col = NA_real_,
  varying_col = c(1, 2, 3, 4, 5, 6, 7, 8)
) %>%
  strict_standardize_within_context("constant_col") %>%
  strict_standardize_within_context("constant_nonzero_col") %>%
  strict_standardize_within_context("all_na_col") %>%
  strict_standardize_within_context("varying_col")

probe_tbl <- tibble(
  probe = c("constant_col (all 0)", "constant_nonzero_col (all 7)", "all_na_col (all NA)", "varying_col"),
  n_NA_in_z = c(
    sum(is.na(degenerate_probe$constant_col_z)),
    sum(is.na(degenerate_probe$constant_nonzero_col_z)),
    sum(is.na(degenerate_probe$all_na_col_z)),
    sum(is.na(degenerate_probe$varying_col_z))
  ),
  all_exactly_zero = c(
    all(degenerate_probe$constant_col_z == 0),
    all(degenerate_probe$constant_nonzero_col_z == 0),
    all(degenerate_probe$all_na_col_z == 0),
    all(degenerate_probe$varying_col_z == 0)
  ),
  max_abs_z = c(
    max(abs(degenerate_probe$constant_col_z)),
    max(abs(degenerate_probe$constant_nonzero_col_z)),
    max(abs(degenerate_probe$all_na_col_z)),
    max(abs(degenerate_probe$varying_col_z))
  )
)
print(as.data.frame(probe_tbl))
stopifnot(
  all(degenerate_probe$constant_col_z == 0),
  all(degenerate_probe$constant_nonzero_col_z == 0),
  all(degenerate_probe$all_na_col_z == 0),
  sum(is.na(degenerate_probe$all_na_col_z)) == 0L
)
cat("    -> a constant column AND a fully-NA column both map to EXACTLY 0 (no NA).\n")

rowmeans_demo <- rowMeans(cbind(c(-1.5, 0.25, 3), c(0, 0, 0)), na.rm = FALSE)
cat("    -> rowMeans(cbind(a, 0), na.rm = FALSE) = ",
  paste(sprintf("%.6f", rowmeans_demo), collapse = ", "),
  "  == 0.5 * a = ", paste(sprintf("%.6f", 0.5 * c(-1.5, 0.25, 3)), collapse = ", "), "\n",
  sep = ""
)
cat("    -> rowMeans returns a finite number: the composite absorbs the dead\n")
cat("       component silently instead of failing closed.\n")

# (1b) build the shipped composite with the repo builder, at both resolutions
shipped <- list()
for (res in resolutions) {
  shipped[[res]] <- build_hmm_epoch_scores(
    bundles[[res]]$occupancy, state_labels[[res]], canonical_roster, res
  )
}

identity_AB <- map_dfr(resolutions, function(res) {
  s <- shipped[[res]]$scores
  A <- s[["Behavioral state architecture"]]
  B <- 0.5 * s$state_occupancy_entropy_z - s$inactive_state_fraction_z
  A_manual <- rowMeans(cbind(s$state_occupancy_entropy_z, s$social_state_fraction_z), na.rm = FALSE) -
    s$inactive_state_fraction_z
  tibble(
    resolution = res,
    n_epochs = nrow(s),
    max_abs_diff_A_minus_B = max(abs(A - B)),
    max_abs_diff_A_minus_A_manual = max(abs(A - A_manual)),
    n_rows_A_not_identical_B = sum(A != B),
    max_abs_social_z = max(abs(s$social_state_fraction_z)),
    social_raw_min = min(s$social_state_fraction),
    social_raw_max = max(s$social_state_fraction),
    social_raw_var = var(s$social_state_fraction),
    n_states_labelled_social = sum(state_labels[[res]]$SemanticState == "social"),
    n_NA_in_A = sum(is.na(A)),
    pearson_A_B = suppressWarnings(cor(A, B))
  )
})
cat("\n[1b] numerical identity of A and B over all epochs:\n")
print(as.data.frame(identity_AB))
stopifnot(all(identity_AB$max_abs_diff_A_minus_B == 0), all(identity_AB$n_rows_A_not_identical_B == 0))
cat("    -> A == B EXACTLY (bitwise) at both resolutions.\n")

# (1c) does the SHIPPED artifact already document the reduction?
comp_audit_path <- file.path(stage14_dir, "tables", "systems_hmm_composite_component_audit.csv")
if (!file.exists(comp_audit_path)) stop("Missing shipped component audit: ", comp_audit_path, call. = FALSE)
shipped_comp_audit <- readr::read_csv(comp_audit_path, progress = FALSE, show_col_types = FALSE)
cat("\n[1c] shipped tables/systems_hmm_composite_component_audit.csv:\n")
print(as.data.frame(shipped_comp_audit %>%
  select(resolution, component, is_constant, is_all_zero, variance, mathematical_reduction)))
reduction_strings <- shipped_comp_audit %>%
  filter(!is.na(mathematical_reduction)) %>%
  distinct(resolution, component, mathematical_reduction)
cat("    documented reduction rows:", nrow(reduction_strings), "\n")
for (i in seq_len(nrow(reduction_strings))) {
  cat("    ", reduction_strings$resolution[i], " / ", reduction_strings$component[i], ": \"",
    reduction_strings$mathematical_reduction[i], "\"\n",
    sep = ""
  )
}
reduction_documented <- nrow(reduction_strings) == 2L &&
  all(reduction_strings$component == "social_state_fraction")

# ================================================================================
# SECTION 3 (run early: it defines what B's negative term IS)
#   inactive_state_fraction as the COMPLEMENT of high-activity-state occupancy
# ================================================================================
cat("\n================================================================\n")
cat("SECTION 3: what the negative term of B literally is\n")
cat("================================================================\n")

complement_tbl <- map_dfr(resolutions, function(res) {
  sl <- state_labels[[res]]
  inactive_states <- sort(sl$State[sl$SemanticState == "inactive/low-exploration"])
  other_states <- sort(setdiff(sl$State, inactive_states))
  other_labels <- vapply(other_states, function(s) sl$SemanticState[sl$State == s], character(1))

  frac <- bundles[[res]]$occupancy %>%
    add_epoch_keys() %>%
    mutate(frac_time = suppressWarnings(as.numeric(.data$frac_time))) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      inactive_frac = sum(frac_time[State %in% inactive_states], na.rm = TRUE),
      other_frac = sum(frac_time[State %in% other_states], na.rm = TRUE),
      frac_sum = sum(frac_time, na.rm = TRUE),
      .groups = "drop"
    )
  tibble(
    resolution = res,
    n_epochs = nrow(frac),
    inactive_states_pooled = paste0("S", inactive_states, collapse = "+"),
    n_inactive_states_pooled = length(inactive_states),
    complement_states = paste0("S", other_states, collapse = "+"),
    complement_state_labels = paste(other_labels, collapse = " | "),
    n_complement_states = length(other_states),
    max_abs_dev_inactive_plus_complement_minus_1 = max(abs(frac$inactive_frac + frac$other_frac - 1)),
    max_abs_dev_frac_time_sum_minus_1 = max(abs(frac$frac_sum - 1)),
    negative_term_of_B_is =
      if (length(other_states) == 1L) {
        paste0(
          "-z(1 - frac(S", other_states, ")) == +z(frac(S", other_states,
          "))  i.e. the exact complement of ", other_labels, " occupancy"
        )
      } else {
        paste0(
          "-z(1 - [frac(S", paste(other_states, collapse = ")+frac(S"),
          ")])  i.e. the complement of POOLED ", paste(other_labels, collapse = "+"), " occupancy"
        )
      }
  )
})
cat("\n[3a] inactive_state_fraction is the exact complement of the non-inactive states:\n")
print(as.data.frame(complement_tbl %>% select(-negative_term_of_B_is)))
for (i in seq_len(nrow(complement_tbl))) {
  cat("    ", complement_tbl$resolution[i], ": inactive = ", complement_tbl$inactive_states_pooled[i],
    " ; complement = ", complement_tbl$complement_states[i],
    " (", complement_tbl$complement_state_labels[i], ")\n",
    sep = ""
  )
  cat("        negative term of B  ->  ", complement_tbl$negative_term_of_B_is[i], "\n", sep = "")
}

# Exact z-level statement: z is affine, so an exact complement flips sign exactly.
zsign_tbl <- map_dfr(resolutions, function(res) {
  sl <- state_labels[[res]]
  inactive_states <- sl$State[sl$SemanticState == "inactive/low-exploration"]
  other_states <- setdiff(sl$State, inactive_states)
  dat <- bundles[[res]]$occupancy %>%
    add_epoch_keys() %>%
    mutate(frac_time = suppressWarnings(as.numeric(.data$frac_time))) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      inactive_frac = sum(frac_time[State %in% inactive_states], na.rm = TRUE),
      complement_frac = sum(frac_time[State %in% other_states], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    strict_standardize_within_context("inactive_frac") %>%
    strict_standardize_within_context("complement_frac")
  tibble(
    resolution = res,
    max_abs_z_inactive_plus_z_complement = max(abs(dat$inactive_frac_z + dat$complement_frac_z)),
    pearson_z_inactive_vs_z_complement = cor(dat$inactive_frac_z, dat$complement_frac_z)
  )
})
cat("\n[3b] z-level restatement (z is affine, so an exact complement flips sign exactly):\n")
print(as.data.frame(zsign_tbl))

# ================================================================================
# SECTION C0 -- DERIVATION OF THE LABEL-FREE CANDIDATE C
# ================================================================================
cat("\n================================================================\n")
cat("SECTION C0: derivation of the label-free candidate C\n")
cat("================================================================\n")
cat("
C IS INHERITED, NOT INVENTED. The redundancy deliverable
(Testing/audits/audit_hmm_state_architecture_redundancy.R, section 8c) proposes the
label-free construct 'latent_state_flexibility':

    C := 0.5 * z(occupancy_entropy) + 0.5 * z(state_switch_rate)

with z = strict_standardize_within_context over Sex x PhaseClass x
CageChangeIndex. It is adopted here VERBATIM (label-free; equal positive weights;
one occupancy term + one temporal-ordering term; both bin-size free). Its own
honest disclosure is that the two terms are NOT independent: epoch-level
r = 0.446-0.671, animal-level r = 0.646-0.872, i.e. 1.14-1.41 effective
dimensions. Equal POSITIVE weights mean nothing is double-subtracted, but the
shared variance is real.

C_alt IS DERIVED HERE, and is reported next to C precisely because of that
coupling -- not to replace it, and not because of any p-value:

  (i)   The redundancy analysis established that the temporal metrics collapse
        onto ONE axis: state_switch_rate = 1 - self_transition_probability
        EXACTLY (Spearman -1.000), mean_dwell_bins ~ 1/(1-P(self)) (Spearman
        0.978 / 0.985), switches_per_hour ~ switch_rate (Spearman ~1.000, and
        bin-size dependent), transition_entropy ~ switch_rate (Spearman
        0.985 / 0.976). A construct may therefore carry AT MOST ONE of these.
  (ii)  transition_entropy (the entropy RATE H(X_{t+1}|X_t)) is NOT independent
        of occupancy_entropy (Spearman 0.332 at 5min but 0.913 at 10min), so
        including both would double-weight the time budget at 10min.
  (iii) The exact information-theoretic decomposition removes that overlap with
        no fitted residualisation:
              H(X_{t+1}) = H(X_{t+1}|X_t) + I(X_t ; X_{t+1})
        One-step mutual information I is, BY CONSTRUCTION, the part of the
        sequence structure the marginal state distribution cannot explain. I = 0
        for any sequence whose order is exchangeable (i.i.d. draws from the same
        occupancy vector) and I > 0 exactly to the extent the next state is
        predictable from the current one.
  (iv)  I uses unlabeled state indices only (label-free), is a per-step quantity
        in nats (no bin count or duration enters it), and is bounded by log(4).

  C_alt := mean( z(occupancy_entropy), z(sequential_mutual_information) )
         = 0.5*z(time-budget dispersion) + 0.5*z(sequential predictability)

  Two dimensions -- one shuffle-invariant, one purely temporal -- orthogonal by
  an exact identity rather than by regression. C_temporal_only_switch_rate :=
  z(state_switch_rate) is the pure-dynamics reference for C.

  DELIBERATELY EXCLUDED FROM BOTH C AND C_alt: (a) inactive_state_fraction and every other
  semantic-label-derived fraction (label-dependent; and at both resolutions the
  rank-1 highest-co-occupancy state is pooled into it); (b)
  top_proximity_state_fraction -- label-free but it measures spatial
  co-occupancy, not architecture, and carries the read-density confound
  (Spearman(inactive_state_fraction, observed_fraction) = -0.63 to -0.70);
  (c) mean_dwell_bins / mean_dwell_hours / switches_per_hour -- bin-size
  dependent, hence not resolution-comparable; (d) any second temporal metric,
  which would double-weight axis (i).
")

# ================================================================================
# SECTION 2 -- WHAT EACH CONSTRUCT MEASURES + THE SHUFFLE-INVARIANCE TEST
# ================================================================================
cat("\n================================================================\n")
cat("SECTION 2: construct semantics and the within-epoch shuffle test\n")
cat("================================================================\n")

# --- chain metrics from a state SEQUENCE (label-free) --------------------------
# Transitions are consecutive rows of the epoch ordered by TimeIndex, exactly as
# Stage 08 does (Analysis/08:419-456). DOCUMENTED LIMITATION inherited from
# Stage 08: an epoch is a CONCATENATION of same-phase blocks, so 0.46% (5min) /
# 0.93% (10min) of transition pairs bridge a ~12 h gap. The effect on
# entropy-type quantities is <= 0.03 / 0.05 nats (component audit check 6m).
chain_metrics_from_sequence <- function(asg) {
  pairs <- asg %>%
    arrange(across(all_of(c(EPOCH_KEY, "TimeIndex")))) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    mutate(NextState = dplyr::lead(State)) %>%
    ungroup() %>%
    filter(!is.na(NextState)) %>%
    count(across(all_of(c(EPOCH_KEY, "State", "NextState"))), name = "n")

  H_joint <- pairs %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(n_transitions = sum(n), H_joint = hmm_feature_entropy(n / sum(n)), .groups = "drop")
  H_from <- pairs %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    summarise(n_from = sum(n), .groups = "drop") %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      H_from = hmm_feature_entropy(n_from / sum(n_from)),
      n_from_states = n(),
      .groups = "drop"
    )
  H_to <- pairs %>%
    group_by(across(all_of(c(EPOCH_KEY, "NextState")))) %>%
    summarise(n_to = sum(n), .groups = "drop") %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(H_to = hmm_feature_entropy(n_to / sum(n_to)), .groups = "drop")
  n_switch <- pairs %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(n_switches = sum(n[State != NextState]), .groups = "drop")

  H_joint %>%
    left_join(H_from, by = EPOCH_KEY) %>%
    left_join(H_to, by = EPOCH_KEY) %>%
    left_join(n_switch, by = EPOCH_KEY) %>%
    mutate(
      H_joint = coalesce(H_joint, 0),
      H_from = coalesce(H_from, 0),
      H_to = coalesce(H_to, 0),
      transition_entropy = H_joint - H_from,                   # = H(X_{t+1} | X_t)
      sequential_mutual_information = H_from + H_to - H_joint, # = I(X_t ; X_{t+1}) >= 0
      state_switch_rate = n_switches / n_transitions
    )
}

occupancy_metrics_from_sequence <- function(asg, res) {
  sl <- state_labels[[res]] %>% select(State, SemanticState)
  asg %>%
    count(across(all_of(c(EPOCH_KEY, "State"))), name = "n_bins") %>%
    left_join(sl, by = "State") %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      occupancy_entropy = hmm_feature_entropy(n_bins / sum(n_bins)),
      inactive_state_fraction = sum(n_bins[SemanticState == "inactive/low-exploration"]) / sum(n_bins),
      social_state_fraction = sum(n_bins[SemanticState == "social"]) / sum(n_bins),
      n_bins = sum(n_bins),
      n_states_occupied = n(),
      .groups = "drop"
    )
}

build_all_constructs <- function(asg, res, tag) {
  occ <- occupancy_metrics_from_sequence(asg, res)
  chn <- chain_metrics_from_sequence(asg)
  occ %>%
    left_join(chn, by = EPOCH_KEY) %>%
    strict_standardize_within_context("occupancy_entropy") %>%
    strict_standardize_within_context("inactive_state_fraction") %>%
    strict_standardize_within_context("social_state_fraction") %>%
    strict_standardize_within_context("sequential_mutual_information") %>%
    strict_standardize_within_context("transition_entropy") %>%
    strict_standardize_within_context("state_switch_rate") %>%
    mutate(
      A_historical_as_written =
        rowMeans(cbind(occupancy_entropy_z, social_state_fraction_z), na.rm = FALSE) -
          inactive_state_fraction_z,
      B_reduced_actual = 0.5 * occupancy_entropy_z - inactive_state_fraction_z,
      # C: the redundancy deliverable's proposal, adopted verbatim
      C_redundancy_proposal_flexibility = 0.5 * occupancy_entropy_z + 0.5 * state_switch_rate_z,
      # C_alt: derived here; terms orthogonal by the exact entropy decomposition
      C_alt_mutual_information =
        rowMeans(cbind(occupancy_entropy_z, sequential_mutual_information_z), na.rm = FALSE),
      C_temporal_only_switch_rate = state_switch_rate_z,
      resolution = res,
      variant = tag
    )
}

original <- imap(bundles, ~ build_all_constructs(.x$assignments %>% add_epoch_keys(), .y, "original"))

# --- cross-check: sequence-derived quantities vs the shipped artifacts ---------
crosscheck <- map_dfr(resolutions, function(res) {
  mine <- original[[res]]
  ship <- shipped[[res]]$scores
  j <- mine %>%
    select(all_of(EPOCH_KEY), occupancy_entropy, inactive_state_fraction, A_historical_as_written) %>%
    inner_join(
      ship %>% select(
        all_of(EPOCH_KEY), state_occupancy_entropy, inactive_state_fraction,
        `Behavioral state architecture`
      ),
      by = EPOCH_KEY, suffix = c("_mine", "_ship")
    )
  # independent check of transition_entropy against the transition TABLE
  tr <- bundles[[res]]$transitions %>%
    add_epoch_keys() %>%
    mutate(Transitions = suppressWarnings(as.numeric(.data$Transitions))) %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    summarise(
      n_from = sum(Transitions, na.rm = TRUE),
      H_cond = coalesce(hmm_feature_entropy(Transitions[Transitions > 0] / sum(Transitions, na.rm = TRUE)), 0),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      transition_entropy_table = sum((n_from / sum(n_from)) * H_cond),
      n_transitions_table = sum(n_from),
      .groups = "drop"
    )
  jt <- mine %>%
    select(all_of(EPOCH_KEY), transition_entropy, n_transitions, sequential_mutual_information) %>%
    inner_join(tr, by = EPOCH_KEY)
  tibble(
    resolution = res,
    n_joined_vs_shipped = nrow(j),
    max_abs_diff_occupancy_entropy = max(abs(j$occupancy_entropy - j$state_occupancy_entropy)),
    max_abs_diff_inactive_fraction = max(abs(j$inactive_state_fraction_mine - j$inactive_state_fraction_ship)),
    max_abs_diff_composite = max(abs(j$A_historical_as_written - j$`Behavioral state architecture`)),
    n_joined_vs_transition_table = nrow(jt),
    max_abs_diff_transition_entropy = max(abs(jt$transition_entropy - jt$transition_entropy_table)),
    max_abs_diff_n_transitions = max(abs(jt$n_transitions - jt$n_transitions_table)),
    min_sequential_mutual_information = min(jt$sequential_mutual_information),
    max_sequential_mutual_information = max(jt$sequential_mutual_information),
    n_negative_MI = sum(jt$sequential_mutual_information < -1e-12)
  )
})
cat("\n[2a] cross-check of sequence-derived quantities vs shipped artifacts:\n")
print(as.data.frame(crosscheck))
stopifnot(all(crosscheck$max_abs_diff_composite < 1e-9))
cat("    -> the sequence-derived reconstruction IS the manuscript quantity.\n")

# --- the shuffle test ----------------------------------------------------------
set.seed(SHUFFLE_SEED)
shuffled <- imap(bundles, function(b, res) {
  asg_shuf <- b$assignments %>%
    add_epoch_keys() %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    mutate(State = sample(State)) %>%
    ungroup()
  build_all_constructs(asg_shuf, res, "shuffled")
})

construct_names <- c(
  "A_historical_as_written", "B_reduced_actual",
  "C_redundancy_proposal_flexibility", "C_alt_mutual_information",
  "C_temporal_only_switch_rate"
)
raw_names <- c(
  "occupancy_entropy", "inactive_state_fraction",
  "sequential_mutual_information", "transition_entropy", "state_switch_rate"
)

shuffle_tbl <- map_dfr(resolutions, function(res) {
  o <- original[[res]]
  s <- shuffled[[res]] %>% select(all_of(EPOCH_KEY), all_of(c(construct_names, raw_names)))
  j <- o %>%
    select(all_of(EPOCH_KEY), all_of(c(construct_names, raw_names))) %>%
    inner_join(s, by = EPOCH_KEY, suffix = c("_orig", "_shuf"))
  map_dfr(c(construct_names, raw_names), function(v) {
    a <- j[[paste0(v, "_orig")]]
    b <- j[[paste0(v, "_shuf")]]
    ok <- is.finite(a) & is.finite(b)
    tibble(
      resolution = res,
      quantity = v,
      quantity_type = if (v %in% construct_names) "construct" else "raw component",
      n = sum(ok),
      pearson_orig_vs_shuffled = if (sd(a[ok]) > 0 && sd(b[ok]) > 0) cor(a[ok], b[ok]) else NA_real_,
      spearman_orig_vs_shuffled = if (sd(a[ok]) > 0 && sd(b[ok]) > 0) {
        cor(a[ok], b[ok], method = "spearman")
      } else {
        NA_real_
      },
      max_abs_diff = max(abs(a[ok] - b[ok])),
      mean_orig = mean(a[ok]),
      mean_shuffled = mean(b[ok]),
      shuffle_invariant_exactly = max(abs(a[ok] - b[ok])) == 0
    )
  })
})
cat("\n[2b] WITHIN-EPOCH SHUFFLE TEST (seed ", SHUFFLE_SEED,
  "): does the construct know the ORDER?\n",
  sep = ""
)
print(as.data.frame(shuffle_tbl))
cat("\n    Reading: an occupancy-only construct is EXACTLY invariant (r = 1, max diff 0)\n")
cat("    -- it cannot be called 'architecture'. A construct carrying transition\n")
cat("    information must drop substantially.\n")

shuffle_lookup <- shuffle_tbl %>%
  filter(quantity_type == "construct") %>%
  select(resolution,
    construct = quantity,
    shuffle_pearson = pearson_orig_vs_shuffled,
    shuffle_spearman = spearman_orig_vs_shuffled,
    shuffle_max_abs_diff = max_abs_diff,
    shuffle_invariant_exactly
  )

# ================================================================================
# SECTION 4a -- EMPIRICAL AGREEMENT BETWEEN CONSTRUCTS
# ================================================================================
cat("\n================================================================\n")
cat("SECTION 4a: empirical agreement between constructs\n")
cat("================================================================\n")

pair_cor <- function(dat, level_label, res, phase) {
  combos <- list(
    c("A_historical_as_written", "B_reduced_actual"),
    c("B_reduced_actual", "C_redundancy_proposal_flexibility"),
    c("A_historical_as_written", "C_redundancy_proposal_flexibility"),
    c("B_reduced_actual", "C_alt_mutual_information"),
    c("C_redundancy_proposal_flexibility", "C_alt_mutual_information"),
    c("C_redundancy_proposal_flexibility", "C_temporal_only_switch_rate"),
    c("occupancy_entropy_z", "state_switch_rate_z"),
    c("occupancy_entropy_z", "sequential_mutual_information_z"),
    c("occupancy_entropy_z", "inactive_state_fraction_z")
  )
  map_dfr(combos, function(p) {
    a <- dat[[p[1]]]
    b <- dat[[p[2]]]
    ok <- is.finite(a) & is.finite(b)
    tibble(
      level = level_label, resolution = res, PhaseClass = phase,
      x = p[1], y = p[2], n = sum(ok),
      pearson = if (sd(a[ok]) > 0 && sd(b[ok]) > 0) cor(a[ok], b[ok]) else NA_real_,
      spearman = if (sd(a[ok]) > 0 && sd(b[ok]) > 0) cor(a[ok], b[ok], method = "spearman") else NA_real_
    )
  })
}

epoch_all <- bind_rows(original)
agreement_epoch <- map_dfr(resolutions, function(res) {
  map_dfr(c("Active", "Inactive"), function(ph) {
    pair_cor(epoch_all %>% filter(resolution == res, PhaseClass == ph), "epoch", res, ph)
  })
})
animal_level <- epoch_all %>%
  group_by(resolution, PhaseClass, AnimalNum, Group, Sex) %>%
  summarise(
    across(
      all_of(c(
        construct_names, "occupancy_entropy_z",
        "inactive_state_fraction_z", "sequential_mutual_information_z", "state_switch_rate_z"
      )),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
agreement_animal <- map_dfr(resolutions, function(res) {
  map_dfr(c("Active", "Inactive"), function(ph) {
    pair_cor(animal_level %>% filter(resolution == res, PhaseClass == ph), "animal", res, ph)
  })
})
agreement <- bind_rows(agreement_epoch, agreement_animal)
cat("\n[4a] construct agreement, epoch level and animal level:\n")
print(as.data.frame(agreement))
write_table(agreement, file.path(audit_out, "hmm_architecture_construct_agreement.csv"))

# --- 4a2. read-density confound check applied to EVERY construct ---------------
# The component audit found the shipped composite strongly confounded with RFID
# read density (observed_fraction = n_reads / expected_reads). It would be
# irresponsible to propose C without subjecting it to the same test.
foundation_path <- file.path(audit_out, "hmm_architecture_component_epoch_metrics.csv")
readdensity <- NULL
if (file.exists(foundation_path)) {
  obs_frac <- readr::read_csv(
    foundation_path,
    col_types = readr::cols(AnimalNum = readr::col_character(), .default = readr::col_guess()),
    progress = FALSE, show_col_types = FALSE
  ) %>%
    transmute(
      resolution = as.character(resolution), AnimalNum = as.character(AnimalNum),
      CageChange = as.character(CageChange), PhaseClass = as.character(PhaseClass),
      observed_fraction = suppressWarnings(as.numeric(observed_fraction))
    )
  rd <- epoch_all %>%
    select(resolution, AnimalNum, CageChange, PhaseClass, all_of(construct_names),
      occupancy_entropy_z, inactive_state_fraction_z, sequential_mutual_information_z,
      state_switch_rate_z) %>%
    inner_join(obs_frac, by = c("resolution", "AnimalNum", "CageChange", "PhaseClass"))
  readdensity <- map_dfr(
    c(construct_names, "occupancy_entropy_z", "inactive_state_fraction_z",
      "sequential_mutual_information_z", "state_switch_rate_z"),
    function(v) {
      map_dfr(resolutions, function(res) {
        map_dfr(c("Active", "Inactive"), function(ph) {
          d <- rd %>% filter(resolution == res, PhaseClass == ph)
          ok <- is.finite(d[[v]]) & is.finite(d$observed_fraction)
          tibble(
            quantity = v, resolution = res, PhaseClass = ph, n = sum(ok),
            spearman_vs_observed_fraction = cor(d[[v]][ok], d$observed_fraction[ok], method = "spearman"),
            pearson_vs_observed_fraction = cor(d[[v]][ok], d$observed_fraction[ok])
          )
        })
      })
    }
  )
  cat("\n[4a2] read-density confound: Spearman(quantity, observed_fraction)\n")
  print(as.data.frame(readdensity))
} else {
  cat("\n[4a2] SKIPPED: foundation table not found at", foundation_path, "\n")
}

# ================================================================================
# SECTION 4b -- GROUP CONTRASTS, SIDE BY SIDE, VIA THE REPO ESTIMATOR
# ================================================================================
cat("\n================================================================\n")
cat("SECTION 4b: group contrasts via fit_repeated_measures_domain_contrasts()\n")
cat("================================================================\n")

domain_long <- epoch_all %>%
  select(resolution, AnimalNum, Group, Sex, CageChangeIndex, PhaseClass, all_of(construct_names)) %>%
  pivot_longer(all_of(construct_names), names_to = "Domain", values_to = "DomainScore")

contrast_rows <- map_dfr(resolutions, function(res) {
  d <- domain_long %>% filter(resolution == res)
  map_dfr(construct_names, function(cn) {
    map_dfr(c("Active", "Inactive"), function(ph) {
      fit_repeated_measures_domain_contrasts(d, cn, ph)$contrasts %>%
        mutate(resolution = res, .before = 1)
    })
  })
})

contrast_rows <- contrast_rows %>%
  mutate(
    ci_low = mixed_model_estimate - qt(0.975, mixed_model_df) * mixed_model_SE,
    ci_high = mixed_model_estimate + qt(0.975, mixed_model_df) * mixed_model_SE
  ) %>%
  group_by(resolution, Domain, Sex, PhaseClass) %>%
  mutate(
    AUDIT_ONLY_FDR_q = p.adjust(mixed_model_p, method = "BH"),
    AUDIT_ONLY_FDR_family_id = paste("AUDIT_ONLY_3_group_contrasts", resolution, Domain, Sex, PhaseClass,
      sep = "__"
    ),
    AUDIT_ONLY_n_tests_in_family = sum(is.finite(mixed_model_p))
  ) %>%
  ungroup()

# resolution agreement, pairwise within construct x Sex x PhaseClass x contrast
res_agree <- contrast_rows %>%
  select(resolution, Domain, Sex, PhaseClass, contrast, mixed_model_estimate,
    mixed_model_p, animal_level_hedges_g) %>%
  pivot_wider(
    names_from = resolution,
    values_from = c(mixed_model_estimate, mixed_model_p, animal_level_hedges_g)
  ) %>%
  mutate(
    resolution_same_sign = sign(mixed_model_estimate_5min_based) == sign(mixed_model_estimate_10min_based),
    resolution_abs_diff_estimate = abs(mixed_model_estimate_5min_based - mixed_model_estimate_10min_based),
    resolution_estimate_ratio_5_over_10 = mixed_model_estimate_5min_based / mixed_model_estimate_10min_based,
    resolution_abs_diff_hedges_g = abs(animal_level_hedges_g_5min_based - animal_level_hedges_g_10min_based)
  ) %>%
  select(Domain, Sex, PhaseClass, contrast, resolution_same_sign, resolution_abs_diff_estimate,
    resolution_estimate_ratio_5_over_10, resolution_abs_diff_hedges_g,
    estimate_5min_based = mixed_model_estimate_5min_based,
    estimate_10min_based = mixed_model_estimate_10min_based)

# --- 4b1. cross-check C against the redundancy deliverable's OWN contrast table -
# My C is computed from the raw Viterbi assignments; theirs from the foundation
# component table. Agreement is therefore an independent implementation check.
proposal_path <- file.path(audit_out, "hmm_architecture_proposed_construct_contrasts.csv")
proposal_crosscheck <- NULL
if (file.exists(proposal_path)) {
  theirs <- readr::read_csv(proposal_path, progress = FALSE, show_col_types = FALSE) %>%
    filter(Domain == "proposed_flexibility") %>%
    transmute(resolution, PhaseClass, Sex, contrast,
      their_estimate = mixed_model_estimate, their_SE = mixed_model_SE,
      their_p = mixed_model_p, their_g = animal_level_hedges_g)
  mine <- contrast_rows %>%
    filter(Domain == "C_redundancy_proposal_flexibility") %>%
    select(resolution, PhaseClass, Sex, contrast, mixed_model_estimate, mixed_model_SE,
      mixed_model_p, animal_level_hedges_g)
  j <- inner_join(mine, theirs, by = c("resolution", "PhaseClass", "Sex", "contrast"))
  proposal_crosscheck <- tibble(
    n_joined = nrow(j),
    n_expected = nrow(mine),
    max_abs_diff_estimate = max(abs(j$mixed_model_estimate - j$their_estimate)),
    max_abs_diff_SE = max(abs(j$mixed_model_SE - j$their_SE)),
    max_abs_diff_p = max(abs(j$mixed_model_p - j$their_p)),
    max_abs_diff_hedges_g = max(abs(j$animal_level_hedges_g - j$their_g))
  )
  cat("\n[4b1] C vs the redundancy deliverable's own 'proposed_flexibility' contrasts:\n")
  print(as.data.frame(proposal_crosscheck))
  cat("    (my C from raw hmm_state_assignments.csv; theirs from the foundation component table)\n")
} else {
  cat("\n[4b1] SKIPPED: redundancy deliverable contrast table not present at", proposal_path, "\n")
}

# --- 4b2. RESOLUTION STABILITY, per construct (direction / magnitude / model) ---
res_stability <- res_agree %>%
  group_by(construct = Domain) %>%
  summarise(
    n_contrast_cells = n(),
    n_same_sign = sum(resolution_same_sign),
    frac_same_sign = mean(resolution_same_sign),
    median_abs_diff_estimate = median(resolution_abs_diff_estimate),
    max_abs_diff_estimate = max(resolution_abs_diff_estimate),
    median_abs_diff_hedges_g = median(resolution_abs_diff_hedges_g),
    max_abs_diff_hedges_g = max(resolution_abs_diff_hedges_g),
    pearson_estimates_5_vs_10 = cor(estimate_5min_based, estimate_10min_based),
    spearman_estimates_5_vs_10 = cor(estimate_5min_based, estimate_10min_based, method = "spearman"),
    .groups = "drop"
  )
cat("\n[4b2] RESOLUTION STABILITY of each construct across all 12 contrast cells:\n")
print(as.data.frame(res_stability))
cat("    (frac_same_sign < 1 means the 5min and 10min fits disagree on the DIRECTION\n")
cat("     of at least one contrast -- a model-stability failure, reported regardless\n")
cat("     of which resolution gives smaller p-values.)\n")

cat("\n[4b] contrasts (estimate / SE / 95% CI / raw p in one block, Hedges g SEPARATE):\n")
print(as.data.frame(contrast_rows %>%
  transmute(resolution,
    construct = Domain, PhaseClass, Sex, contrast,
    est = round(mixed_model_estimate, 4), SE = round(mixed_model_SE, 4),
    ci = paste0("[", round(ci_low, 3), ", ", round(ci_high, 3), "]"),
    raw_p = signif(mixed_model_p, 3),
    hedges_g = round(animal_level_hedges_g, 3),
    n_ref = n_ref_animals, n_comp = n_comp_animals,
    audit_q = signif(AUDIT_ONLY_FDR_q, 3),
    model_status
  )))

# ================================================================================
# SECTION 5 -- the deliverable CSV
# ================================================================================
descriptors <- tibble(
  construct = construct_names,
  construct_role = c(
    "A (historical, as written)", "B (actual reduced form)",
    "C (label-free candidate, INHERITED from the redundancy deliverable section 8c 'latent_state_flexibility')",
    "C_alt (label-free alternative derived here; terms orthogonal by an exact identity)",
    "C reference (pure dynamics)"
  ),
  formula_as_written = c(
    "rowMeans(cbind(z(state_occupancy_entropy), z(social_state_fraction))) - z(inactive_state_fraction)",
    "0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction)",
    "0.5 * z(occupancy_entropy) + 0.5 * z(state_switch_rate)",
    "rowMeans(cbind(z(occupancy_entropy), z(sequential_mutual_information)))",
    "z(state_switch_rate)"
  ),
  formula_reduced = c(
    "0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction)   [social term is identically 0]",
    "0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction)   [already reduced]",
    "0.5 * z(H(occupancy)) + 0.5 * z(n_switches / n_transitions)   [= 0.5*z(H) + 0.5*z(1 - P(self))]",
    "0.5 * z(H(occupancy)) + 0.5 * z(H(pi_from) + H(pi_to) - H(joint))",
    "z(n_switches / n_transitions)   [= z(1 - self_transition_probability), exact complement]"
  ),
  inputs = c(
    "hmm_state_occupancy.csv frac_time (per-state bin counts) + hmm_state_summary.csv semantic labels",
    "hmm_state_occupancy.csv frac_time (per-state bin counts) + hmm_state_summary.csv semantic labels",
    "Viterbi state sequence only: per-state bin counts AND consecutive-bin transition counts; no state labels",
    "Viterbi state sequence only: per-state bin counts AND consecutive-bin transition counts; no state labels",
    "Viterbi state sequence only: consecutive-bin transition counts; no state labels"
  ),
  effective_weights = c(
    paste0(
      "+0.5 on z(occupancy entropy); 0.0 on z(social) (dead component); -1.0 on z(inactive fraction), ",
      "and inactive fraction is the exact complement of high-activity-state occupancy -> the inactive term ",
      "carries ~91% (5min) / ~73% (10min) of composite variance"
    ),
    "identical to A: +0.5 z(occupancy entropy), -1.0 z(inactive fraction); 2:1 weighting in favour of the label-derived term",
    paste0(
      "+0.5 on z(occupancy entropy) = time-budget dispersion; +0.5 on z(per-step switch probability) = ",
      "temporal switching. Terms are positively correlated (epoch r 0.45-0.67, animal r 0.65-0.87 per the ",
      "redundancy deliverable), i.e. 1.14-1.41 effective dimensions -- equal POSITIVE weights so nothing is ",
      "double-subtracted, but the shared variance is disclosed, not assumed away"
    ),
    paste0(
      "+0.5 on z(occupancy entropy) = time-budget dispersion; +0.5 on z(one-step mutual information) = ",
      "sequential predictability; the two are orthogonal by the exact decomposition ",
      "H(X_t+1) = H(X_t+1|X_t) + I, not by regression"
    ),
    "+1.0 on z(per-step switch probability); exact complement of self-transition probability, so it is that dimension too"
  ),
  units = c(
    "dimensionless: within-context z of nats minus within-context z of a proportion; scale is context-relative (cell SD of the composite is 0.62-1.42, not 1)",
    "same as A",
    "dimensionless: mean of a within-context z of nats and a within-context z of a per-step probability in [0,1]",
    "dimensionless: mean of two within-context z scores of nats / nats-per-step quantities; both raw terms bounded by log(4) = 1.3863",
    "dimensionless within-context z of a per-step probability in [0,1]"
  ),
  contains_temporal_information = c(FALSE, FALSE, TRUE, TRUE, TRUE),
  label_dependent = c(TRUE, TRUE, FALSE, FALSE, FALSE),
  bin_size_comparable = c(
    "occupancy fractions are bin-size free, but the STATE DEFINITIONS differ between the two fits, so 5 and 10 min are different constructs",
    "same as A",
    "both terms are per-bin / per-step and free of any duration multiplier; but coarse-graining still changes the per-step switch probability, so 5 vs 10 min must be compared by direction and magnitude, never pooled",
    "same as C; I is additionally coarse-graining dependent",
    "same as C"
  ),
  construct_status = c(
    paste0(
      "MISLEADING AS NAMED: exactly equal to B; contains no transition, dwell or ordering information ",
      "(shuffle-invariant); the word 'architecture' is not earned. The social term is dead and is silently ",
      "absorbed rather than failing closed."
    ),
    paste0(
      "HONEST RESTATEMENT of the shipped value: 0.5 x (how evenly time is spread over the 4 states) minus ",
      "1.0 x (fraction of time in the pooled low-movement/low-entropy states), i.e. dominated by the ",
      "complement of high-activity-state occupancy."
    ),
    paste0(
      "DEFENSIBLE ON CONSTRUCT GROUNDS and better behaved than A/B on label-free criteria: not ",
      "shuffle-invariant, 11/12 contrast cells agree in sign across resolutions (vs 8/12 for A/B), and its ",
      "read-density coupling flips sign between phases rather than being a uniform gradient. But it does ",
      "NOT reproduce the primary Female Active RES-CON claim, it disagrees with A/B in DIRECTION on Female ",
      "Active SUS-RES, its two terms share 20-76% of their variance, and |Spearman| vs observed_fraction ",
      "still reaches 0.56. Reported as evaluated, not endorsed; adoption needs its own coverage audit."
    ),
    paste0(
      "ALTERNATIVE derived here, reported because C's two terms share 20-76% of their variance while these ",
      "two are orthogonal by an exact identity. Not a competing proposal; a robustness reference for C."
    ),
    "DIAGNOSTIC reference, not a proposed manuscript construct: isolates the purely temporal axis of C."
  )
)

comparison_csv <- contrast_rows %>%
  rename(construct = Domain) %>%
  left_join(descriptors, by = "construct") %>%
  left_join(shuffle_lookup, by = c("resolution", "construct")) %>%
  left_join(res_agree %>% rename(construct = Domain),
    by = c("construct", "Sex", "PhaseClass", "contrast")
  ) %>%
  left_join(identity_AB %>% select(resolution, max_abs_diff_A_minus_B, max_abs_social_z),
    by = "resolution"
  ) %>%
  left_join(
    complement_tbl %>% select(resolution, inactive_states_pooled, complement_states,
      complement_state_labels, max_abs_dev_inactive_plus_complement_minus_1),
    by = "resolution"
  ) %>%
  mutate(
    shuffle_invariant = shuffle_invariant_exactly,
    shuffle_correlation = shuffle_pearson,
    A_equals_B_exactly = max_abs_diff_A_minus_B == 0,
    shipped_reduction_documented = reduction_documented,
    shipped_reduction_text = "0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction); social component contributes zero",
    standardization_context = paste(hmm_standardization_context, collapse = " x "),
    candidate_C_provenance = "C inherited verbatim from Testing/audits/audit_hmm_state_architecture_redundancy.R section 8c (latent_state_flexibility); C_alt derived here as an orthogonality robustness reference",
    selection_guard = "constructs were not selected on p-value; C is reported even where it separates groups less than A/B"
  ) %>%
  select(
    construct, construct_role, resolution, PhaseClass, Sex, contrast,
    mixed_model_estimate, mixed_model_SE, ci_low, ci_high, mixed_model_df,
    mixed_model_t, mixed_model_p,
    animal_level_hedges_g, mean_ref, mean_comp, n_ref_animals, n_comp_animals,
    AUDIT_ONLY_FDR_q, AUDIT_ONLY_FDR_family_id, AUDIT_ONLY_n_tests_in_family,
    resolution_same_sign, resolution_abs_diff_estimate, resolution_estimate_ratio_5_over_10,
    resolution_abs_diff_hedges_g, estimate_5min_based, estimate_10min_based,
    formula_as_written, formula_reduced, inputs, effective_weights, units,
    shuffle_invariant, shuffle_correlation, shuffle_spearman, shuffle_max_abs_diff,
    contains_temporal_information, label_dependent, bin_size_comparable, construct_status,
    A_equals_B_exactly, max_abs_diff_A_minus_B, max_abs_social_z,
    inactive_states_pooled, complement_states, complement_state_labels,
    max_abs_dev_inactive_plus_complement_minus_1,
    shipped_reduction_documented, shipped_reduction_text,
    standardization_context, candidate_C_provenance, selection_guard,
    model_engine, model_formula, model_status, model_warnings
  ) %>%
  arrange(construct, resolution, PhaseClass, Sex, contrast)

write_table(comparison_csv, file.path(audit_out, "hmm_architecture_construct_comparison.csv"))
cat("\n[5] wrote hmm_architecture_construct_comparison.csv  rows:", nrow(comparison_csv),
  " cols:", ncol(comparison_csv), "\n")

structural <- bind_rows(
  identity_AB %>% mutate(check = "A_vs_B_identity") %>% relocate(check),
  complement_tbl %>% mutate(check = "inactive_fraction_complement") %>% relocate(check),
  zsign_tbl %>% mutate(check = "z_level_complement_sign") %>% relocate(check),
  crosscheck %>% mutate(check = "sequence_reconstruction_vs_shipped") %>% relocate(check),
  shuffle_tbl %>% mutate(check = "shuffle_invariance") %>% relocate(check),
  probe_tbl %>%
    mutate(check = "standardizer_on_degenerate_columns", resolution = NA_character_) %>%
    relocate(check),
  res_stability %>% mutate(check = "resolution_stability", resolution = NA_character_) %>% relocate(check),
  if (!is.null(proposal_crosscheck)) {
    proposal_crosscheck %>%
      mutate(check = "C_vs_redundancy_deliverable_contrasts", resolution = NA_character_) %>%
      relocate(check)
  } else {
    NULL
  },
  if (!is.null(readdensity)) {
    readdensity %>% mutate(check = "read_density_confound") %>% relocate(check)
  } else {
    NULL
  }
)
write_table(structural, file.path(audit_out, "hmm_architecture_construct_comparison_structural.csv"))
cat("[5] wrote hmm_architecture_construct_comparison_structural.csv  rows:", nrow(structural), "\n")

# ================================================================================
# SECTION 6 -- human-readable summary
# ================================================================================
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
gv <- function(cn, res, ph, sx, ct, col) {
  v <- comparison_csv[[col]][comparison_csv$construct == cn & comparison_csv$resolution == res &
    comparison_csv$PhaseClass == ph & comparison_csv$Sex == sx & comparison_csv$contrast == ct]
  if (length(v) == 0) NA else v[1]
}
sh <- function(cn, res) {
  shuffle_lookup$shuffle_pearson[shuffle_lookup$construct == cn & shuffle_lookup$resolution == res]
}
shd <- function(cn, res) {
  shuffle_lookup$shuffle_max_abs_diff[shuffle_lookup$construct == cn & shuffle_lookup$resolution == res]
}
cv <- function(tbl, col, res) tbl[[col]][tbl$resolution == res]

md <- c(
  "# `Behavioral state architecture`: three-way construct comparison",
  "",
  paste0(
    "`Testing/audits/audit_hmm_state_architecture_construct_comparison.R` (read-only audit). Contrasts from the ",
    "repo's `fit_repeated_measures_domain_contrasts()` unchanged: 882 epochs / 111 animals per resolution, ",
    "`AnimalNum` random intercept, CC1-CC4 retained."
  ),
  "",
  "## Structure",
  "",
  paste0(
    "| | A (as written) | B (actual reduced form) | C (label-free, inherited from the redundancy ",
    "deliverable) | C_alt (label-free, derived here) |"
  ),
  "|---|---|---|---|---|",
  paste0(
    "| Formula | `mean(z(occ_entropy), z(social_frac)) - z(inactive_frac)` | ",
    "`0.5*z(occ_entropy) - z(inactive_frac)` | `0.5*z(occ_entropy) + 0.5*z(switch_rate)` | ",
    "`mean(z(occ_entropy), z(I(X_t;X_t+1)))` |"
  ),
  paste0(
    "| Inputs | occupancy `frac_time` + semantic labels | occupancy `frac_time` + semantic labels | ",
    "Viterbi sequence only | Viterbi sequence only |"
  ),
  "| Label-dependent | yes | yes | no | no |",
  "| Contains temporal-ordering information | **no** | **no** | **yes** | **yes** |",
  paste0(
    "| Shuffle-invariant (within-epoch permutation) | **yes, exactly** (r = 1.000, max abs diff ",
    fmt(shd("A_historical_as_written", "5min_based"), 0), ") | **yes, exactly** (r = 1.000, max abs diff ",
    fmt(shd("B_reduced_actual", "5min_based"), 0), ") | no: r = ",
    fmt(sh("C_redundancy_proposal_flexibility", "5min_based")), " / ",
    fmt(sh("C_redundancy_proposal_flexibility", "10min_based")), " | no: r = ",
    fmt(sh("C_alt_mutual_information", "5min_based")), " / ",
    fmt(sh("C_alt_mutual_information", "10min_based")), " |"
  ),
  paste0(
    "| Effective weights | +0.5 entropy / 0.0 social / -1.0 inactive | identical to A | ",
    "+0.5 dispersion / +0.5 switching (terms correlated, 1.14-1.41 effective dims) | ",
    "+0.5 dispersion / +0.5 predictability (orthogonal by identity) |"
  ),
  paste0(
    "| Units | dimensionless (context z of nats minus context z of a proportion) | same | ",
    "dimensionless (z of nats + z of a per-step probability) | dimensionless (z of nats + z of nats/step) |"
  ),
  paste0(
    "| Status | misleading as named | honest restatement of the shipped value | earns the name; more ",
    "resolution-stable than A/B; does **not** reproduce the primary Female Active claim | robustness ",
    "reference for C, not a competing proposal |"
  ),
  "",
  "## 1. A and B are the same number",
  "",
  paste0(
    "`max |A - B| = ", fmt(cv(identity_AB, "max_abs_diff_A_minus_B", "5min_based"), 0),
    "` at 5min_based and `", fmt(cv(identity_AB, "max_abs_diff_A_minus_B", "10min_based"), 0),
    "` at 10min_based over ", cv(identity_AB, "n_epochs", "5min_based"),
    " epochs each -- bitwise, 0 rows differ. `social_state_fraction` is identically 0 (variance 0; 0 of 4 ",
    "states carry the `social` label), so `sd = 0` in every standardization cell and ",
    "`strict_standardize_within_context()` returns `rep(0, n)` (`hmm_stage14_helpers.R:268`) -- an exact 0, ",
    "**not** `NA`. Since `rowMeans(cbind(a, 0), na.rm = FALSE) = a/2`, A collapses to ",
    "`0.5*z(occ_entropy) - z(inactive_frac)`."
  ),
  "",
  paste0(
    "The failure is silent: the standardizer maps a constant column *and* a fully-`NA` column to exactly 0 ",
    "(0 `NA`s in the z of an all-`NA` probe), so `rowMeans(..., na.rm = FALSE)` still returns a finite ",
    "number. The composite absorbs a dead component by halving the surviving term instead of failing closed. ",
    "The pipeline **does** already document this: ",
    "`tables/systems_hmm_composite_component_audit.csv` records, for `social_state_fraction` at both ",
    "resolutions, `is_constant = TRUE`, `is_all_zero = TRUE`, `variance = 0`, ",
    "`mathematical_reduction = \"0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction); ",
    "social component contributes zero\"`. Disclosed in a QC table, not in the displayed name."
  ),
  "",
  "## 2. What the shipped value literally is",
  "",
  paste0(
    "At **5min_based** the three `inactive/low-exploration` states pool to ",
    cv(complement_tbl, "inactive_states_pooled", "5min_based"),
    " and the complement is the single state ",
    cv(complement_tbl, "complement_states", "5min_based"),
    " (", cv(complement_tbl, "complement_state_labels", "5min_based"), "): ",
    "`max |inactive_frac + frac(S2) - 1| = ",
    signif(cv(complement_tbl, "max_abs_dev_inactive_plus_complement_minus_1", "5min_based"), 3),
    "`. Because z is affine, `z(inactive_frac) = -z(frac(S2))` exactly (`max |sum| = ",
    signif(cv(zsign_tbl, "max_abs_z_inactive_plus_z_complement", "5min_based"), 3), "`, r = ",
    fmt(cv(zsign_tbl, "pearson_z_inactive_vs_z_complement", "5min_based"), 6), "). So B is exactly"
  ),
  "",
  "> `0.5 * z(occupancy entropy) + z(fraction of time in the single high-movement / high-entropy state)`",
  "",
  paste0(
    "At **10min_based** the pooling is ", cv(complement_tbl, "inactive_states_pooled", "10min_based"),
    " and the complement is ", cv(complement_tbl, "complement_states", "10min_based"),
    " (", cv(complement_tbl, "complement_state_labels", "10min_based"),
    "). The complement identity and the z-level sign flip still hold (`",
    signif(cv(complement_tbl, "max_abs_dev_inactive_plus_complement_minus_1", "10min_based"), 3), "` and `",
    signif(cv(zsign_tbl, "max_abs_z_inactive_plus_z_complement", "10min_based"), 3), "`, r = ",
    fmt(cv(zsign_tbl, "pearson_z_inactive_vs_z_complement", "10min_based"), 4),
    "), but the negative term is now the complement of a **two-state pool** (burst + mixed), so the 5 and ",
    "10 min values are not the same quantity. The pooling is also not proximity-coherent: at 5 min it ",
    "contains the rank-1 highest-co-occupancy state, at 10 min both the highest and the lowest."
  ),
  "",
  "## 3. The shuffle test settles the name",
  "",
  paste0(
    "Permuting the state sequence *within each epoch* (seed ", SHUFFLE_SEED,
    ") destroys ordering but preserves every bin count. A and B are unchanged to the last bit ",
    "(`max abs diff = 0`, r = 1.000, both resolutions). C drops to r = ",
    fmt(sh("C_redundancy_proposal_flexibility", "5min_based")), " / ",
    fmt(sh("C_redundancy_proposal_flexibility", "10min_based")),
    " and its purely temporal term to r = ", fmt(sh("C_temporal_only_switch_rate", "5min_based")), " / ",
    fmt(sh("C_temporal_only_switch_rate", "10min_based")),
    ". The shipped construct therefore measures a **time budget**, not an architecture: two animals with ",
    "the same occupancy vector receive the same score however differently their states are strung together. ",
    "Two honest riders. (i) The raw temporal components move a long way under shuffling -- mean ",
    "`state_switch_rate` ",
    fmt(shuffle_tbl$mean_orig[shuffle_tbl$quantity == "state_switch_rate" &
      shuffle_tbl$resolution == "5min_based"]), " -> ",
    fmt(shuffle_tbl$mean_shuffled[shuffle_tbl$quantity == "state_switch_rate" &
      shuffle_tbl$resolution == "5min_based"]),
    " and mean `I(X_t;X_t+1)` ",
    fmt(shuffle_tbl$mean_orig[shuffle_tbl$quantity == "sequential_mutual_information" &
      shuffle_tbl$resolution == "5min_based"]), " -> ",
    fmt(shuffle_tbl$mean_shuffled[shuffle_tbl$quantity == "sequential_mutual_information" &
      shuffle_tbl$resolution == "5min_based"]),
    " at 5 min -- so the test has real power. (ii) C is still ",
    fmt(100 * sh("C_redundancy_proposal_flexibility", "5min_based")^2, 0),
    "% shuffle-explained by variance (r^2) at 5 min, because half of it is the invariant occupancy term; ",
    "the temporal half is what makes the construct non-invariant, not most of its variance."
  ),
  "",
  "## 4. Group contrasts, Female Active RES-CON (the primary cell)",
  "",
  "| construct | res | est | SE | 95% CI | raw p | Hedges g |",
  "|---|---|---|---|---|---|---|"
)
for (cn in construct_names) {
  for (res in resolutions) {
    md <- c(md, paste0(
      "| `", cn, "` | ", sub("_based", "", res), " | ",
      fmt(gv(cn, res, "Active", "Female", "RES-CON", "mixed_model_estimate"), 4), " | ",
      fmt(gv(cn, res, "Active", "Female", "RES-CON", "mixed_model_SE"), 4), " | [",
      fmt(gv(cn, res, "Active", "Female", "RES-CON", "ci_low")), ", ",
      fmt(gv(cn, res, "Active", "Female", "RES-CON", "ci_high")), "] | ",
      signif(gv(cn, res, "Active", "Female", "RES-CON", "mixed_model_p"), 3), " | ",
      fmt(gv(cn, res, "Active", "Female", "RES-CON", "animal_level_hedges_g")), " |"
    ))
  }
}
rdv <- function(v, res, ph) {
  if (is.null(readdensity)) {
    return(NA_real_)
  }
  readdensity$spearman_vs_observed_fraction[readdensity$quantity == v &
    readdensity$resolution == res & readdensity$PhaseClass == ph]
}
stv <- function(cn, col) res_stability[[col]][res_stability$construct == cn]

md <- c(
  md, "",
  paste0(
    "C separates the groups **less** than A/B here (RES-CON ",
    fmt(gv("C_redundancy_proposal_flexibility", "5min_based", "Active", "Female", "RES-CON", "mixed_model_estimate"), 4),
    ", p ", signif(gv("C_redundancy_proposal_flexibility", "5min_based", "Active", "Female", "RES-CON", "mixed_model_p"), 3),
    ", g ", fmt(gv("C_redundancy_proposal_flexibility", "5min_based", "Active", "Female", "RES-CON", "animal_level_hedges_g")),
    " vs A/B ",
    fmt(gv("B_reduced_actual", "5min_based", "Active", "Female", "RES-CON", "mixed_model_estimate"), 4),
    ", p ", signif(gv("B_reduced_actual", "5min_based", "Active", "Female", "RES-CON", "mixed_model_p"), 3),
    ", g ", fmt(gv("B_reduced_actual", "5min_based", "Active", "Female", "RES-CON", "animal_level_hedges_g")),
    "). Reported as observed: C does **not** reproduce the primary claim. In the same cell the two ",
    "constructs also **disagree in direction** on SUS-RES -- A/B ",
    fmt(gv("B_reduced_actual", "5min_based", "Active", "Female", "SUS-RES", "mixed_model_estimate"), 4),
    " (p ", signif(gv("B_reduced_actual", "5min_based", "Active", "Female", "SUS-RES", "mixed_model_p"), 3),
    ") vs C ",
    fmt(gv("C_redundancy_proposal_flexibility", "5min_based", "Active", "Female", "SUS-RES", "mixed_model_estimate"), 4),
    " (p ", signif(gv("C_redundancy_proposal_flexibility", "5min_based", "Active", "Female", "SUS-RES", "mixed_model_p"), 3),
    ") -- consistent with their being *negatively* correlated in the Active phase (epoch r ",
    fmt(agreement$pearson[agreement$level == "epoch" & agreement$resolution == "5min_based" &
      agreement$PhaseClass == "Active" & agreement$x == "B_reduced_actual" &
      agreement$y == "C_redundancy_proposal_flexibility"]),
    ", animal r ",
    fmt(agreement$pearson[agreement$level == "animal" & agreement$resolution == "5min_based" &
      agreement$PhaseClass == "Active" & agreement$x == "B_reduced_actual" &
      agreement$y == "C_redundancy_proposal_flexibility"]),
    ") while agreeing strongly in the Inactive phase (epoch r ",
    fmt(agreement$pearson[agreement$level == "epoch" & agreement$resolution == "5min_based" &
      agreement$PhaseClass == "Inactive" & agreement$x == "B_reduced_actual" &
      agreement$y == "C_redundancy_proposal_flexibility"]),
    "). Where C is strongest is the **Inactive** phase, and there it is stronger and more consistent than ",
    "A/B: Female RES-CON ",
    fmt(gv("C_redundancy_proposal_flexibility", "5min_based", "Inactive", "Female", "RES-CON", "mixed_model_estimate"), 4),
    " (p ", signif(gv("C_redundancy_proposal_flexibility", "5min_based", "Inactive", "Female", "RES-CON", "mixed_model_p"), 3),
    ", g ", fmt(gv("C_redundancy_proposal_flexibility", "5min_based", "Inactive", "Female", "RES-CON", "animal_level_hedges_g")),
    ") and Male RES-CON ",
    fmt(gv("C_redundancy_proposal_flexibility", "10min_based", "Inactive", "Male", "RES-CON", "mixed_model_estimate"), 4),
    " (p ", signif(gv("C_redundancy_proposal_flexibility", "10min_based", "Inactive", "Male", "RES-CON", "mixed_model_p"), 3),
    ", g ", fmt(gv("C_redundancy_proposal_flexibility", "10min_based", "Inactive", "Male", "RES-CON", "animal_level_hedges_g")),
    ")."
  ),
  "",
  "### Resolution stability and read-density confound (all four constructs)",
  "",
  "| construct | contrast cells agreeing in sign, 5 vs 10 min | r(est 5 min, est 10 min) | Spearman vs `observed_fraction`, Active | Inactive |",
  "|---|---|---|---|---|",
  paste0("| A / B | ", stv("B_reduced_actual", "n_same_sign"), " / ",
    stv("B_reduced_actual", "n_contrast_cells"), " | ",
    fmt(stv("B_reduced_actual", "pearson_estimates_5_vs_10")), " | ",
    fmt(rdv("B_reduced_actual", "5min_based", "Active")), " (5 min) / ",
    fmt(rdv("B_reduced_actual", "10min_based", "Active")), " (10 min) | ",
    fmt(rdv("B_reduced_actual", "5min_based", "Inactive")), " / ",
    fmt(rdv("B_reduced_actual", "10min_based", "Inactive")), " |"),
  paste0("| C | ", stv("C_redundancy_proposal_flexibility", "n_same_sign"), " / ",
    stv("C_redundancy_proposal_flexibility", "n_contrast_cells"), " | ",
    fmt(stv("C_redundancy_proposal_flexibility", "pearson_estimates_5_vs_10")), " | ",
    fmt(rdv("C_redundancy_proposal_flexibility", "5min_based", "Active")), " / ",
    fmt(rdv("C_redundancy_proposal_flexibility", "10min_based", "Active")), " | ",
    fmt(rdv("C_redundancy_proposal_flexibility", "5min_based", "Inactive")), " / ",
    fmt(rdv("C_redundancy_proposal_flexibility", "10min_based", "Inactive")), " |"),
  paste0("| C_alt | ", stv("C_alt_mutual_information", "n_same_sign"), " / ",
    stv("C_alt_mutual_information", "n_contrast_cells"), " | ",
    fmt(stv("C_alt_mutual_information", "pearson_estimates_5_vs_10")), " | ",
    fmt(rdv("C_alt_mutual_information", "5min_based", "Active")), " / ",
    fmt(rdv("C_alt_mutual_information", "10min_based", "Active")), " | ",
    fmt(rdv("C_alt_mutual_information", "5min_based", "Inactive")), " / ",
    fmt(rdv("C_alt_mutual_information", "10min_based", "Inactive")), " |"),
  paste0("| C_temporal_only (switch rate) | ", stv("C_temporal_only_switch_rate", "n_same_sign"), " / ",
    stv("C_temporal_only_switch_rate", "n_contrast_cells"), " | ",
    fmt(stv("C_temporal_only_switch_rate", "pearson_estimates_5_vs_10")), " | ",
    fmt(rdv("C_temporal_only_switch_rate", "5min_based", "Active")), " / ",
    fmt(rdv("C_temporal_only_switch_rate", "10min_based", "Active")), " | ",
    fmt(rdv("C_temporal_only_switch_rate", "5min_based", "Inactive")), " / ",
    fmt(rdv("C_temporal_only_switch_rate", "10min_based", "Inactive")), " |"),
  "",
  paste0(
    "All ", nrow(comparison_csv), " rows (5 constructs x 2 resolutions x 2 phases x 2 sexes x 3 contrasts) ",
    "are in `hmm_architecture_construct_comparison.csv`, effect size and inference in separate columns, ",
    "with an AUDIT-ONLY BH family (`AUDIT_ONLY_3_group_contrasts__<res>__<construct>__<Sex>__<Phase>`, ",
    "3 tests) that does **not** redefine the shipped 18-test primary heatmap family."
  ),
  "",
  "## 5. Verdict",
  "",
  paste0(
    "A and B are one construct with two names. B is the honest form: an occupancy-only *state time-budget* ",
    "index dominated by the complement of high-activity-state occupancy -- not 'architecture'. C ",
    "(`0.5*z(occ_entropy) + 0.5*z(switch_rate)`, inherited from the redundancy deliverable) does contain ",
    "ordering information; C_alt substitutes the mutual-information term so the two halves are orthogonal ",
    "by identity rather than merely equally signed. ",
    "C does not reproduce the primary Female Active RES-CON claim; that is not automatically a mark against ",
    "C, because A/B's effect sits on the label-derived occupancy term the read-density audit found ",
    "confounded (Spearman(A/B, observed_fraction) = +0.59 to +0.61 in **all four** resolution x phase ",
    "cells) and its nominal significance rests on 8 post-chip-dropout Female epochs."
  ),
  "",
  paste0(
    "On the criteria that do not depend on group labels, C is the better-behaved construct: it is ",
    "resolution-stable (",
    stv("C_redundancy_proposal_flexibility", "n_same_sign"), "/",
    stv("C_redundancy_proposal_flexibility", "n_contrast_cells"),
    " contrast cells agree in sign, r(estimates) = ",
    fmt(stv("C_redundancy_proposal_flexibility", "pearson_estimates_5_vs_10")), ", versus ",
    stv("B_reduced_actual", "n_same_sign"), "/", stv("B_reduced_actual", "n_contrast_cells"),
    " and r = ", fmt(stv("B_reduced_actual", "pearson_estimates_5_vs_10")),
    " for A/B), and its read-density coupling changes sign between phases rather than being a uniform ",
    "gradient. Its own weaknesses are real and must travel with it: its two terms share 20-76% of their ",
    "variance, per-step switching is coarse-graining dependent, and its |Spearman| against read density ",
    "still reaches 0.56. C_alt (mutual information in place of switch rate) buys term orthogonality at the ",
    "cost of resolution stability (",
    stv("C_alt_mutual_information", "n_same_sign"), "/",
    stv("C_alt_mutual_information", "n_contrast_cells"), " same sign, r = ",
    fmt(stv("C_alt_mutual_information", "pearson_estimates_5_vs_10")),
    "), so it is a diagnostic, not a candidate. The defensible immediate action is to stop calling B ",
    "'architecture' and rename it for the time budget it measures; adopting C is a separate decision that ",
    "needs its own length / bin-size / coverage audit."
  )
)
writeLines(md, file.path(audit_out, "hmm_architecture_construct_comparison_summary.md"))
cat("[6] wrote hmm_architecture_construct_comparison_summary.md  lines:", length(md), "\n")

cat("\n================================================================\n")
cat("DONE\n")
cat("================================================================\n")
