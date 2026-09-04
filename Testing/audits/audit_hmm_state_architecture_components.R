# ================================================================================
# AUDIT (read-only): foundation component-metric table for the Stage 14 construct
#   `Behavioral state architecture`
#
# Testing/audits/audit_hmm_state_architecture_components.R
#
# Purpose
#   Build ONE canonical per-epoch (animal x CageChange x PhaseClass x resolution)
#   table holding all seven prespecified HMM "state architecture" component
#   metrics plus their named variants, in raw and context-z form, together with
#   the shipped composite recomputed via build_hmm_epoch_scores() so the table can
#   be PROVEN to reproduce the manuscript quantity exactly.
#
# This script is an AUDIT. It reads repo helpers and Stage 08 artifacts and writes
# only into the Stage 14 audit folder. It modifies nothing under Analysis/ or
# Functions/, and it re-implements no repo contract: canonical_animal_id(),
# build_canonical_identity_roster(), audit_hmm_identity(),
# assert_hmm_identity_audit(), annotate_hmm_semantic_states(),
# hmm_feature_entropy(), strict_standardize_within_context() and
# build_hmm_epoch_scores() are all called from Functions/hmm_stage14_helpers.R.
#
# Terminology guard: RFID "Proximity" is a social-spatial co-location proxy, not
# measured sociability. The highest-proximity HMM state is referred to here as the
# "top-proximity" / high-co-occupancy state and is derived label-free as the argmax
# of model-level Proximity_z. No state is renamed "social".
#
# ------------------------------------------------------------------------------
# REVISION (v2) -- repairs of blocking defects found by independent verification
# ------------------------------------------------------------------------------
#  (R1) BLOCKING. The foundation table carried NO RFID data-completeness column.
#       Stage 14's tables/qc_chip_loss_flags.csv is now identity-audited and
#       joined 1:1 on AnimalNum x CageChange x PhaseClass, and the foundation
#       table carries observed_fraction (= n_reads / expected_reads, i.e. raw
#       RFID read density), qc_epoch_class, recommended_action, longest_gap_hours,
#       hard_dropout_signature and derived exclusion flags, plus a context-z of
#       observed_fraction. Section 6j quantifies the read-density confound
#       (pooled, within-standardization-context and animal-level), and section 6k
#       adds observed_fraction as a model covariate sensitivity.
#  (R2) BLOCKING. The 8 Active-phase epochs Stage 14 classes
#       "exclude_after_dropout" are now flagged
#       (qc_active_dropout_leaveout_flag) and section 6k reports the leave-out
#       refit of the PRIMARY quantity next to the primary estimate, using
#       fit_repeated_measures_domain_contrasts() unchanged (random intercept and
#       CC1-CC4 retained).
#  (R3) IMPORTANT. Section 6k also reports the OR539/OR540 leave-out for the
#       Inactive-phase phenotype (both animals are QC-flagged Female CON).
#  (R4) IMPORTANT. Epochs are CONCATENATIONS of same-phase blocks separated by the
#       opposite (unobserved) phase. Stage 08 counts transitions and run-length
#       encodes bouts ACROSS those ~12 h discontinuities. The table now carries
#       n_time_blocks / n_time_gaps / n_gap_bridged_transitions / max_time_gap_bins
#       and gap-aware sensitivity variants (*_gapaware) computed from
#       hmm_state_assignments.csv. The shipped Stage 08 artifacts are NOT modified.
#  (R5) IMPORTANT. Section 6l adds the decisive length-dependence evidence
#       (R^2 of n_transitions on CageChangeIndex x PhaseClass, within-cell SD,
#       mean within-context Spearman) and a subsampling truncation test that
#       measures the actual plug-in bias and the fraction of it that
#       Miller-Madow removes.
#  (R6) MINOR. hmm_epoch_data_quality_exclusions.csv is now identity-audited;
#       two documentation errors corrected (short-epoch bin ratio; "0 exactly"
#       -> "0 to machine precision").
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
roster_bin_level <- "5min_based" # matches Stage 08 hmm_roster_bin_level default

cat("================================================================\n")
cat("AUDIT: HMM state-architecture component metrics\n")
cat("repo root  :", MMM_REPO_ROOT, "\n")
cat("project    :", project_root, "\n")
cat("audit out  :", audit_out, "\n")
cat("resolutions:", paste(resolutions, collapse = ", "), "\n")
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
canonical_roster_raw <- readr::read_csv(
  canonical_roster_file,
  col_types = readr::cols(
    .default = readr::col_skip(),
    AnimalNum = readr::col_character(),
    Group = readr::col_character(),
    Sex = readr::col_character()
  ),
  progress = FALSE
)
canonical_roster <- build_canonical_identity_roster(
  canonical_roster_raw, paste0("Stage 01 ", roster_bin_level, " roster")
)
cat("[1] canonical roster animals:", nrow(canonical_roster), "\n")
print(as.data.frame(canonical_roster %>% count(Sex, Group, name = "n_animals")))
cat("\n")

# --------------------------------------------------------------------------------
# 2. Load + identity-audit every HMM table at both resolutions
# --------------------------------------------------------------------------------
hmm_artifact <- function(resolution, filename) {
  resolve_configured_hmm_artifact(project_root, resolution, filename, required = TRUE)$path
}

load_and_audit <- function(resolution, filename) {
  path <- hmm_artifact(resolution, filename)
  dat <- readr::read_csv(
    path,
    col_types = readr::cols(AnimalNum = readr::col_character()),
    progress = FALSE, show_col_types = FALSE
  ) %>%
    mutate(across(any_of(c("State", "NextState")), as.character))
  audit <- audit_hmm_identity(dat, canonical_roster, paste0(filename, " @ ", resolution))
  assert_hmm_identity_audit(audit)
  list(data = audit$data, audit = audit, path = path)
}

identity_summaries <- list()
bundles <- list()
for (res in resolutions) {
  occ <- load_and_audit(res, "hmm_state_occupancy.csv")
  trp <- load_and_audit(res, "hmm_transition_probabilities.csv")
  dwl <- load_and_audit(res, "hmm_state_dwell_times.csv")
  # (R4) raw Viterbi sequences: needed for the gap-aware sensitivity variants and
  # for the subsampling length-bias test. (R6) also identity-audited.
  asg <- load_and_audit(res, "hmm_state_assignments.csv")
  # (R6) hmm_epoch_data_quality_exclusions.csv carries AnimalNum and was
  # previously read with a bare read_csv(); it is now audited like every other
  # HMM table so the script's stated contract actually holds.
  exc <- load_and_audit(res, "hmm_epoch_data_quality_exclusions.csv")
  state_summary <- readr::read_csv(
    hmm_artifact(res, "hmm_state_summary.csv"),
    col_types = readr::cols(State = readr::col_character()),
    progress = FALSE, show_col_types = FALSE
  )
  bundles[[res]] <- list(
    occupancy = occ$data, transitions = trp$data, dwell = dwl$data,
    assignments = asg$data, exclusions = exc$data, state_summary = state_summary
  )
  identity_summaries <- c(
    identity_summaries,
    list(occ$audit$summary, trp$audit$summary, dwl$audit$summary,
      asg$audit$summary, exc$audit$summary)
  )
}

# --- (R1) RFID data-completeness / chip-loss QC, identity-audited and joined ----
# observed_fraction in tables/qc_chip_loss_flags.csv is n_reads / expected_reads
# (Analysis/14_systems_neuroscience_summary_dashboard.R, raw_observed_fraction),
# i.e. the RAW RFID read density from which Movement / Entropy / Proximity -- and
# hence every HMM state assignment -- are derived. It is therefore an upstream
# measurement-coverage variable, not a downstream behavioural one.
chip_loss_path <- file.path(
  project_root,
  "analysis_ready/12_systems_neuroscience_summary/5min_based/tables/qc_chip_loss_flags.csv"
)
if (!file.exists(chip_loss_path)) {
  stop("Stage 14 chip-loss QC table is missing: ", chip_loss_path, call. = FALSE)
}
chip_loss_raw <- readr::read_csv(
  chip_loss_path,
  col_types = readr::cols(AnimalNum = readr::col_character(), .default = readr::col_guess()),
  progress = FALSE, show_col_types = FALSE
)
chip_loss_audit <- audit_hmm_identity(chip_loss_raw, canonical_roster, "qc_chip_loss_flags.csv")
assert_hmm_identity_audit(chip_loss_audit)
identity_summaries <- c(identity_summaries, list(chip_loss_audit$summary))

chip_loss <- chip_loss_audit$data %>%
  mutate(
    CageChange = as.character(.data$CageChange),
    PhaseClass = case_when(
      str_detect(str_to_lower(as.character(.data$Phase)), "\\binactive\\b|\\blight\\b|\\bday\\b") ~ "Inactive",
      str_detect(str_to_lower(as.character(.data$Phase)), "\\bactive\\b|\\bdark\\b|\\bnight\\b") ~ "Active",
      TRUE ~ as.character(.data$Phase)
    )
  ) %>%
  transmute(
    AnimalNum = as.character(.data$AnimalNum),
    CageChange, PhaseClass,
    observed_fraction = suppressWarnings(as.numeric(.data$observed_fraction)),
    qc_longest_gap_hours = suppressWarnings(as.numeric(.data$longest_gap_hours)),
    qc_n_positions = suppressWarnings(as.numeric(.data$n_positions)),
    qc_epoch_class = as.character(.data$qc_epoch_class),
    recommended_action = as.character(.data$recommended_action),
    qc_hard_dropout_signature = as.logical(.data$hard_dropout_signature),
    qc_active_behavioral_collapse = as.logical(.data$active_behavioral_collapse),
    qc_inactive_low_motion_review = as.logical(.data$inactive_low_motion_review)
  ) %>%
  mutate(
    qc_exclude_after_dropout = qc_epoch_class == "exclude_after_dropout",
    qc_insufficient_data = qc_epoch_class == "insufficient_data",
    qc_recommends_exclusion = str_detect(str_to_lower(recommended_action), "exclude")
  )
dup_qc_keys <- sum(duplicated(chip_loss %>% select(AnimalNum, CageChange, PhaseClass)))
cat("[2b] chip-loss QC rows:", nrow(chip_loss),
  " duplicate join keys:", dup_qc_keys, "\n")
stopifnot(dup_qc_keys == 0L)
print(as.data.frame(chip_loss %>% count(PhaseClass, qc_epoch_class, name = "n_epochs")))
cat("\n")
identity_summary_tbl <- bind_rows(identity_summaries)
cat("[2] identity audits (all must have passed = TRUE):\n")
print(as.data.frame(identity_summary_tbl %>% select(
  source, input_rows, raw_animal_spellings, canonical_animals, aliases_merged,
  identity_conflicts, unknown_animals, metadata_disagreements, missing_canonical_ids, passed
)))
stopifnot(all(identity_summary_tbl$passed))
cat("    -> all identity audits PASSED\n\n")
write_table(identity_summary_tbl, file.path(audit_out, "hmm_architecture_identity_audit_summary.csv"))

# --------------------------------------------------------------------------------
# 3. Semantic labels (repo classifier) + label-free top-proximity state
# --------------------------------------------------------------------------------
state_labels <- imap(bundles, ~ annotate_hmm_semantic_states(.x$state_summary, .y))
cat("[3] semantic state labels (repo annotate_hmm_semantic_states()):\n")
top_prox_state <- setNames(rep(NA_character_, length(resolutions)), resolutions)
state_label_report <- list()
for (res in resolutions) {
  sl <- state_labels[[res]]
  top_prox_state[[res]] <- sl$State[which.max(sl$Proximity_z)] # label-free argmax
  rep_tbl <- sl %>%
    mutate(
      proximity_rank = rank(-Proximity_z, ties.method = "min"),
      is_top_proximity_state = State == top_prox_state[[res]],
      median_movement_z = median(Movement_z, na.rm = TRUE),
      median_entropy_z = median(Entropy_z, na.rm = TRUE),
      q67_proximity_z = quantile(Proximity_z, 0.67, na.rm = TRUE)
    )
  state_label_report[[res]] <- rep_tbl
  print(as.data.frame(rep_tbl %>% select(
    resolution, State, Movement_z, Entropy_z, Proximity_z, SemanticState,
    proximity_rank, is_top_proximity_state
  )))
  cat("    ", res, ": median(Mov)=", sprintf("%.6f", unique(rep_tbl$median_movement_z)),
    "  median(Ent)=", sprintf("%.6f", unique(rep_tbl$median_entropy_z)),
    "  q67(Prox)=", sprintf("%.6f", unique(rep_tbl$q67_proximity_z)), "\n", sep = ""
  )
  cat("    ", res, ": label-free top-proximity state = S", top_prox_state[[res]],
    "  (current semantic label: '", sl$SemanticState[sl$State == top_prox_state[[res]]], "')\n\n", sep = ""
  )
}
write_table(bind_rows(state_label_report), file.path(audit_out, "hmm_architecture_state_label_report.csv"))

# --------------------------------------------------------------------------------
# 4. Per-epoch component metrics
# --------------------------------------------------------------------------------
# PhaseClass / CageChangeIndex mapping copied verbatim from build_hmm_epoch_scores()
# so the epoch key is bit-identical to the shipped composite's epoch key.
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

EPOCH_KEY <- c("AnimalNum", "Group", "Sex", "CageChange", "CageChangeIndex", "PhaseClass")

# --- 4a. occupancy-derived metrics ---------------------------------------------
#  (1) occupancy_entropy            Shannon entropy of frac_time, natural log
#  (2) inactive_state_fraction      current semantic label "inactive/low-exploration"
#  (3) top_proximity_state_fraction label-free argmax Proximity_z state
occupancy_metrics <- function(res) {
  sl <- state_labels[[res]] %>% select(State, SemanticState)
  top_state <- top_prox_state[[res]]
  bundles[[res]]$occupancy %>%
    add_epoch_keys() %>%
    mutate(frac_time = suppressWarnings(as.numeric(.data$frac_time))) %>%
    left_join(sl, by = "State") %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      occupancy_entropy = hmm_feature_entropy(frac_time / sum(frac_time, na.rm = TRUE)),
      inactive_state_fraction = sum(frac_time[SemanticState == "inactive/low-exploration"], na.rm = TRUE),
      social_state_fraction = sum(frac_time[SemanticState == "social"], na.rm = TRUE),
      top_proximity_state_fraction = sum(frac_time[State == top_state], na.rm = TRUE),
      burst_state_fraction = sum(frac_time[SemanticState == "burst/high-movement"], na.rm = TRUE),
      mixed_state_fraction = sum(frac_time[SemanticState == "mixed"], na.rm = TRUE),
      n_states_occupied = sum(is.finite(frac_time) & frac_time > 0),
      frac_time_sum = sum(frac_time, na.rm = TRUE),
      observed_bins = suppressWarnings(as.numeric(first(observed_bins))),
      total_observation_duration_hours = suppressWarnings(as.numeric(first(total_observation_duration_hours))),
      short_epoch = as.logical(first(short_epoch)),
      cage_change_duration_class = as.character(first(cage_change_duration_class)),
      .groups = "drop"
    )
}

# --- 4b. transition-derived metrics --------------------------------------------
#  (4) transition_entropy  = entropy RATE of the empirical first-order chain
#        H = -sum_s pi_s sum_t P(t|s) log P(t|s),  pi_s = n_s / N,  P(t|s) = n_st / n_s
#      This is a PER-STEP quantity: no bin count or duration enters it, so it is
#      length-normalised by construction. Bounded above by log(4).
#      transition_entropy_mm adds the Miller-Madow correction (m_s - 1)/(2 n_s) to
#      each conditional entropy, m_s = number of DISTINCT observed successors of s,
#      to offset the downward small-sample bias of the plug-in estimator.
#  (5) state_switch_rate = sum_{s != t} n_st / N  (per-step switch probability)
#      switches_per_hour = switches / total_observation_duration_hours (bin-size dep.)
#  (6) self_transition_probability = sum_s pi_s P(s|s) = sum_s n_ss / N
#      self_transition_probability_unweighted = unweighted mean of P(s|s) over
#      from-states with at least one outgoing transition.
transition_metrics <- function(res) {
  tr <- bundles[[res]]$transitions %>%
    add_epoch_keys() %>%
    mutate(
      NextState = as.character(.data$NextState),
      Transitions = suppressWarnings(as.numeric(.data$Transitions)),
      TransitionProbability = suppressWarnings(as.numeric(.data$TransitionProbability))
    ) %>%
    filter(is.finite(Transitions))

  # from-state (row) level: recompute P(t|s) from raw counts and check the file
  from_rows <- tr %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    mutate(
      n_from_state = sum(Transitions, na.rm = TRUE),
      P_recomputed = if_else(n_from_state > 0, Transitions / n_from_state, NA_real_)
    ) %>%
    ungroup()

  prob_check <- from_rows %>%
    summarise(
      resolution = res,
      n_rows = n(),
      n_zero_count_rows = sum(Transitions == 0),
      max_abs_diff_P = max(abs(P_recomputed - TransitionProbability), na.rm = TRUE),
      .groups = "drop"
    )

  from_level <- from_rows %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    summarise(
      n_from_state = sum(Transitions, na.rm = TRUE),
      n_distinct_successors = sum(Transitions > 0),
      self_count = sum(Transitions[State == NextState], na.rm = TRUE),
      switch_count = sum(Transitions[State != NextState], na.rm = TRUE),
      # plug-in conditional entropy of P(.|s), using recomputed probabilities
      H_cond = hmm_feature_entropy(Transitions[Transitions > 0] / sum(Transitions, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      H_cond = if_else(is.na(H_cond), 0, H_cond),
      P_self = if_else(n_from_state > 0, self_count / n_from_state, NA_real_),
      H_cond_mm = if_else(
        n_from_state > 0,
        H_cond + (n_distinct_successors - 1) / (2 * n_from_state),
        NA_real_
      )
    )

  epoch_level <- from_level %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      n_transitions = sum(n_from_state, na.rm = TRUE),
      n_from_states_used = sum(n_from_state > 0),
      pi_sum = if (sum(n_from_state, na.rm = TRUE) > 0) {
        sum(n_from_state / sum(n_from_state, na.rm = TRUE))
      } else {
        NA_real_
      },
      transition_entropy = if (sum(n_from_state, na.rm = TRUE) > 0) {
        sum((n_from_state / sum(n_from_state, na.rm = TRUE)) * H_cond, na.rm = TRUE)
      } else {
        NA_real_
      },
      transition_entropy_mm = if (sum(n_from_state, na.rm = TRUE) > 0) {
        sum((n_from_state / sum(n_from_state, na.rm = TRUE)) * H_cond_mm, na.rm = TRUE)
      } else {
        NA_real_
      },
      n_switches = sum(switch_count, na.rm = TRUE),
      state_switch_rate = if (sum(n_from_state, na.rm = TRUE) > 0) {
        sum(switch_count, na.rm = TRUE) / sum(n_from_state, na.rm = TRUE)
      } else {
        NA_real_
      },
      self_transition_probability = if (sum(n_from_state, na.rm = TRUE) > 0) {
        sum(self_count, na.rm = TRUE) / sum(n_from_state, na.rm = TRUE)
      } else {
        NA_real_
      },
      self_transition_probability_unweighted = mean(P_self[n_from_state > 0], na.rm = TRUE),
      .groups = "drop"
    )

  list(epoch = epoch_level, prob_check = prob_check)
}

# --- 4c. dwell-derived metrics -------------------------------------------------
#  (7) mean_dwell_bins  = occupancy(frac_time)-weighted mean of per-state
#      mean_dwell_bins. mean_dwell_hours = mean_dwell_bins * bin_size_sec / 3600 and
#      is therefore BIN-SIZE DEPENDENT (not comparable across resolutions);
#      mean_dwell_bins is the scale-comparable version.
dwell_metrics <- function(res) {
  occ_weights <- bundles[[res]]$occupancy %>%
    add_epoch_keys() %>%
    transmute(
      across(all_of(EPOCH_KEY)), State,
      frac_time = suppressWarnings(as.numeric(.data$frac_time))
    )

  bundles[[res]]$dwell %>%
    add_epoch_keys() %>%
    mutate(
      mean_dwell_bins_state = suppressWarnings(as.numeric(.data$mean_dwell_bins)),
      bin_size_sec = suppressWarnings(as.numeric(.data$bin_size_sec)),
      n_bouts = suppressWarnings(as.numeric(.data$n_bouts))
    ) %>%
    left_join(occ_weights, by = c(EPOCH_KEY, "State")) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      bin_size_sec = first(bin_size_sec),
      mean_dwell_bins = {
        w <- frac_time
        x <- mean_dwell_bins_state
        ok <- is.finite(w) & is.finite(x) & w > 0
        if (any(ok)) sum(w[ok] * x[ok]) / sum(w[ok]) else NA_real_
      },
      n_bouts_total = sum(n_bouts, na.rm = TRUE),
      n_dwell_states = n(),
      .groups = "drop"
    ) %>%
    mutate(mean_dwell_hours = mean_dwell_bins * bin_size_sec / 3600)
}

# --- 4c2. (R4) temporal contiguity + gap-aware sensitivity variants ------------
# An "epoch" (animal x CageChange x PhaseClass) is a CONCATENATION of same-phase
# blocks separated by the opposite, unobserved phase: an Active epoch stitches
# together 2-4 dark blocks separated by full light phases. Stage 08 groups by the
# epoch key and then uses lead()/rle() over ROW ORDER, so it counts one transition
# pair and merges one bout across each ~12 h discontinuity. That is faithful to the
# shipped definition, but it must be DOCUMENTED, because mean_dwell_bins can then
# report a single "bout" that spans an unobserved light phase.
#
# We do not modify the Stage 08 artifacts. We add, per epoch:
#   n_time_blocks, n_time_gaps (= n_gap_bridged_transitions), max_time_gap_bins
# and gap-aware recomputations that break the chain at each discontinuity:
#   transition_entropy_gapaware, transition_entropy_mm_gapaware,
#   state_switch_rate_gapaware, self_transition_probability_gapaware,
#   mean_dwell_bins_gapaware, n_bouts_total_gapaware, n_transitions_gapaware
contiguity_metrics <- function(res) {
  asg <- bundles[[res]]$assignments %>%
    add_epoch_keys() %>%
    mutate(TimeIndex = suppressWarnings(as.numeric(.data$TimeIndex))) %>%
    arrange(!!!rlang::syms(EPOCH_KEY), TimeIndex) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    mutate(
      .step = TimeIndex - lag(TimeIndex),
      .block = cumsum(is.na(.step) | .step != 1)
    ) %>%
    ungroup()

  gap_summary <- asg %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      n_bins_assignments = n(),
      n_time_blocks = max(.block),
      n_time_gaps = sum(!is.na(.step) & .step != 1),
      max_time_gap_bins = {
        g <- .step[!is.na(.step) & .step != 1]
        if (length(g) == 0) 0 else max(g)
      },
      .groups = "drop"
    ) %>%
    mutate(n_gap_bridged_transitions = n_time_gaps)

  # gap-aware transition counts: pairs only WITHIN a contiguous block
  pairs <- asg %>%
    group_by(across(all_of(c(EPOCH_KEY, ".block")))) %>%
    mutate(NextState = lead(State)) %>%
    ungroup() %>%
    filter(!is.na(NextState))

  from_level <- pairs %>%
    count(across(all_of(c(EPOCH_KEY, "State", "NextState"))), name = "Transitions") %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    summarise(
      n_from_state = sum(Transitions),
      n_distinct_successors = sum(Transitions > 0),
      self_count = sum(Transitions[State == NextState]),
      switch_count = sum(Transitions[State != NextState]),
      H_cond = hmm_feature_entropy(Transitions[Transitions > 0] / sum(Transitions)),
      .groups = "drop"
    ) %>%
    mutate(
      H_cond = if_else(is.na(H_cond), 0, H_cond),
      H_cond_mm = H_cond + (n_distinct_successors - 1) / (2 * n_from_state)
    )

  trans_gap <- from_level %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      n_transitions_gapaware = sum(n_from_state),
      transition_entropy_gapaware = sum((n_from_state / sum(n_from_state)) * H_cond),
      transition_entropy_mm_gapaware = sum((n_from_state / sum(n_from_state)) * H_cond_mm),
      state_switch_rate_gapaware = sum(switch_count) / sum(n_from_state),
      self_transition_probability_gapaware = sum(self_count) / sum(n_from_state),
      .groups = "drop"
    )

  # gap-aware bouts: run-length encode WITHIN a contiguous block only
  bouts <- asg %>%
    group_by(across(all_of(c(EPOCH_KEY, ".block")))) %>%
    mutate(.run = cumsum(is.na(lag(State)) | State != lag(State))) %>%
    ungroup() %>%
    count(across(all_of(c(EPOCH_KEY, ".block", ".run", "State"))), name = "run_len") %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    summarise(
      mean_dwell_bins_state_gapaware = mean(run_len),
      n_bouts_state_gapaware = n(),
      .groups = "drop"
    )

  occ_weights <- bundles[[res]]$occupancy %>%
    add_epoch_keys() %>%
    transmute(across(all_of(EPOCH_KEY)), State, frac_time = suppressWarnings(as.numeric(.data$frac_time)))

  dwell_gap <- bouts %>%
    left_join(occ_weights, by = c(EPOCH_KEY, "State")) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      mean_dwell_bins_gapaware = {
        ok <- is.finite(frac_time) & frac_time > 0
        if (any(ok)) {
          sum(frac_time[ok] * mean_dwell_bins_state_gapaware[ok]) / sum(frac_time[ok])
        } else {
          NA_real_
        }
      },
      n_bouts_total_gapaware = sum(n_bouts_state_gapaware),
      .groups = "drop"
    )

  gap_summary %>%
    left_join(trans_gap, by = EPOCH_KEY) %>%
    left_join(dwell_gap, by = EPOCH_KEY)
}

# --- 4d. shipped composite, recomputed with the repo builder -------------------
shipped_composite <- function(res) {
  built <- build_hmm_epoch_scores(
    bundles[[res]]$occupancy, state_labels[[res]], canonical_roster, res
  )
  built$scores %>%
    select(
      all_of(EPOCH_KEY),
      shipped_state_occupancy_entropy = state_occupancy_entropy,
      shipped_inactive_state_fraction = inactive_state_fraction,
      shipped_social_state_fraction = social_state_fraction,
      shipped_state_occupancy_entropy_z = state_occupancy_entropy_z,
      shipped_inactive_state_fraction_z = inactive_state_fraction_z,
      shipped_social_state_fraction_z = social_state_fraction_z,
      `Behavioral state architecture`
    )
}

# --- 4e. assemble one resolution ----------------------------------------------
component_cols <- c(
  "occupancy_entropy", "inactive_state_fraction", "social_state_fraction",
  "top_proximity_state_fraction", "transition_entropy", "transition_entropy_mm",
  "state_switch_rate", "switches_per_hour", "self_transition_probability",
  "self_transition_probability_unweighted", "mean_dwell_bins", "mean_dwell_hours",
  "n_transitions"
)

build_resolution <- function(res) {
  occ <- occupancy_metrics(res)
  trn <- transition_metrics(res)
  dwl <- dwell_metrics(res)
  ctg <- contiguity_metrics(res)
  cmp <- shipped_composite(res)

  out <- occ %>%
    full_join(trn$epoch, by = EPOCH_KEY) %>%
    full_join(dwl, by = EPOCH_KEY) %>%
    full_join(ctg, by = EPOCH_KEY) %>%
    full_join(cmp, by = EPOCH_KEY) %>%
    # (R1) RFID data-completeness / chip-loss QC, 1:1 on animal x CC x PhaseClass
    left_join(chip_loss, by = c("AnimalNum", "CageChange", "PhaseClass")) %>%
    mutate(
      resolution = res,
      switches_per_hour = if_else(
        is.finite(total_observation_duration_hours) & total_observation_duration_hours > 0,
        n_switches / total_observation_duration_hours, NA_real_
      ),
      geometric_dwell_prediction = if_else(
        is.finite(self_transition_probability) & self_transition_probability < 1,
        1 / (1 - self_transition_probability), NA_real_
      ),
      complementarity_residual = state_switch_rate + self_transition_probability - 1,
      standardization_context = paste(hmm_standardization_context, collapse = " x "),
      # (R2) the 8 Active-phase epochs Stage 14's own QC classes
      # "exclude_after_dropout". These are the leave-out set for the PRIMARY
      # Female Active sensitivity in section 6k. They are RETAINED in the table.
      qc_active_dropout_leaveout_flag = PhaseClass == "Active" &
        !is.na(qc_epoch_class) & qc_epoch_class == "exclude_after_dropout",
      # (R4) gap-bridging deltas, for transparency
      delta_transition_entropy_gapaware = transition_entropy_gapaware - transition_entropy,
      delta_state_switch_rate_gapaware = state_switch_rate_gapaware - state_switch_rate,
      delta_mean_dwell_bins_gapaware = mean_dwell_bins_gapaware - mean_dwell_bins,
      n_bouts_merged_across_gaps = n_bouts_total_gapaware - n_bouts_total
    )

  # context-z for every component and named variant, via the repo standardizer
  for (cc in c(
    component_cols, "burst_state_fraction", "mixed_state_fraction",
    "observed_fraction", "transition_entropy_gapaware", "state_switch_rate_gapaware",
    "mean_dwell_bins_gapaware"
  )) {
    out <- strict_standardize_within_context(out, cc)
  }

  out %>%
    mutate(
      audit_composite_reduced = 0.5 * occupancy_entropy_z - inactive_state_fraction_z,
      composite_reproduction_diff = `Behavioral state architecture` - audit_composite_reduced
    ) %>%
    relocate(resolution, all_of(EPOCH_KEY))
}

cat("[4] building per-epoch component metrics ...\n")
per_resolution <- map(setNames(resolutions, resolutions), build_resolution)
prob_checks <- map_dfr(setNames(resolutions, resolutions), ~ transition_metrics(.x)$prob_check)
components <- bind_rows(per_resolution)
cat("    rows:", nrow(components), " cols:", ncol(components), "\n\n")

# --------------------------------------------------------------------------------
# 5. WRITE the foundation table
# --------------------------------------------------------------------------------
out_path <- file.path(audit_out, "hmm_architecture_component_epoch_metrics.csv")
write_table(components, out_path)
cat("[5] WROTE", out_path, "\n")
cat("    ", nrow(components), "rows x", ncol(components), "cols\n\n")

# ================================================================================
# 6. VERIFICATION CHECKS
# ================================================================================
num <- function(x) sprintf("%.10g", x)
sep <- function(t) cat("\n---- ", t, " ", strrep("-", max(0, 60 - nchar(t))), "\n", sep = "")

# --- (a) row / animal counts ---------------------------------------------------
sep("6a. row counts and animal counts per resolution")
count_a <- components %>%
  group_by(resolution) %>%
  summarise(
    n_epoch_rows = n(),
    n_animals = n_distinct(AnimalNum),
    n_active = sum(PhaseClass == "Active"),
    n_inactive = sum(PhaseClass == "Inactive"),
    n_cage_changes = n_distinct(CageChangeIndex),
    n_rows_any_na_component = sum(!complete.cases(pick(all_of(component_cols)))),
    .groups = "drop"
  )
print(as.data.frame(count_a))
cat("expected animal-CC-phase cells = n_animals x 4 CC x 2 phases =",
  111 * 4 * 2, "\n")
excl <- map_dfr(resolutions, function(res) {
  p <- hmm_artifact(res, "hmm_epoch_data_quality_exclusions.csv")
  readr::read_csv(p, col_types = readr::cols(AnimalNum = readr::col_character()),
    progress = FALSE, show_col_types = FALSE) %>% mutate(resolution = res)
})
cat("hmm_epoch_data_quality_exclusions.csv rows per resolution:\n")
print(as.data.frame(excl %>% count(resolution, retained_for_hmm, exclusion_reason, name = "n")))
cat("excluded epochs (animal x CC x phase):\n")
print(as.data.frame(excl %>% select(resolution, AnimalNum, Group, Sex, CageChange, Phase, input_bins, complete_hmm_bins)))
write_table(count_a, file.path(audit_out, "hmm_architecture_check_a_counts.csv"))

# --- (b) composite reproduction proof -----------------------------------------
sep("6b. reproduction of shipped `Behavioral state architecture`")
check_b <- components %>%
  group_by(resolution) %>%
  summarise(
    n = sum(is.finite(composite_reproduction_diff)),
    max_abs_diff_vs_0.5z_entropy_minus_z_inactive = max(abs(composite_reproduction_diff), na.rm = TRUE),
    max_abs_diff_my_entropy_vs_shipped = max(abs(occupancy_entropy - shipped_state_occupancy_entropy), na.rm = TRUE),
    max_abs_diff_my_inactive_vs_shipped = max(abs(inactive_state_fraction - shipped_inactive_state_fraction), na.rm = TRUE),
    max_abs_diff_my_entropy_z_vs_shipped_z = max(abs(occupancy_entropy_z - shipped_state_occupancy_entropy_z), na.rm = TRUE),
    max_abs_diff_my_inactive_z_vs_shipped_z = max(abs(inactive_state_fraction_z - shipped_inactive_state_fraction_z), na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_b))
write_table(check_b, file.path(audit_out, "hmm_architecture_check_b_composite_reproduction.csv"))

# (b2) stronger provenance claim: compare against the SHIPPED Stage 14 artifact on
# disk, not only against a re-run of build_hmm_epoch_scores(). This is what makes
# the foundation table THE manuscript quantity rather than a look-alike.
shipped_scores_path <- file.path(
  project_root,
  "analysis_ready/12_systems_neuroscience_summary/5min_based/tables",
  "systems_hmm_epoch_scores_by_resolution.csv"
)
if (file.exists(shipped_scores_path)) {
  shipped_scores <- readr::read_csv(
    shipped_scores_path,
    col_types = readr::cols(AnimalNum = readr::col_character(), .default = readr::col_guess()),
    progress = FALSE, show_col_types = FALSE
  ) %>%
    transmute(
      resolution = as.character(.data$resolution),
      AnimalNum = as.character(.data$AnimalNum),
      CageChange = as.character(.data$CageChange),
      PhaseClass = as.character(.data$PhaseClass),
      artifact_composite = .data[["Behavioral state architecture"]],
      artifact_occ_entropy = .data$state_occupancy_entropy,
      artifact_inactive = .data$inactive_state_fraction
    )
  check_b2 <- components %>%
    select(resolution, AnimalNum, CageChange, PhaseClass, occupancy_entropy,
      inactive_state_fraction, `Behavioral state architecture`) %>%
    inner_join(shipped_scores, by = c("resolution", "AnimalNum", "CageChange", "PhaseClass")) %>%
    group_by(resolution) %>%
    summarise(
      n_joined = n(),
      max_abs_diff_composite_vs_shipped_artifact = max(abs(`Behavioral state architecture` - artifact_composite)),
      max_abs_diff_occ_entropy_vs_artifact = max(abs(occupancy_entropy - artifact_occ_entropy)),
      max_abs_diff_inactive_vs_artifact = max(abs(inactive_state_fraction - artifact_inactive)),
      .groups = "drop"
    )
  cat("\n(b2) vs the shipped Stage 14 artifact systems_hmm_epoch_scores_by_resolution.csv:\n")
  print(as.data.frame(check_b2))
  write_table(check_b2, file.path(audit_out, "hmm_architecture_check_b2_vs_shipped_artifact.csv"))
} else {
  cat("\n(b2) SKIPPED: shipped artifact not found at", shipped_scores_path, "\n")
}

# --- (c) social_state_fraction identically zero -------------------------------
sep("6c. social_state_fraction (expect identically 0)")
check_c <- components %>%
  group_by(resolution) %>%
  summarise(
    n = n(),
    min = min(social_state_fraction, na.rm = TRUE),
    max = max(social_state_fraction, na.rm = TRUE),
    variance = var(social_state_fraction, na.rm = TRUE),
    n_nonzero = sum(social_state_fraction != 0, na.rm = TRUE),
    social_z_min = min(social_state_fraction_z, na.rm = TRUE),
    social_z_max = max(social_state_fraction_z, na.rm = TRUE),
    n_states_labelled_social = sum(state_labels[[first(resolution)]]$SemanticState == "social"),
    .groups = "drop"
  )
print(as.data.frame(check_c))
write_table(check_c, file.path(audit_out, "hmm_architecture_check_c_social_zero.csv"))

# --- (d) switch-rate / self-transition complementarity ------------------------
# (R6) Phrasing correction: the residual is 0 TO MACHINE PRECISION, not "0
# exactly". It is 0 in double arithmetic here because both quantities are counts
# over the identical denominator, but the CSV round-trip re-introduces ~1 ulp
# (2.22e-16), so the exact-zero claim does not reproduce from the written file.
sep("6d. complementarity: |switch_rate + self_transition_probability - 1|")
check_d <- components %>%
  group_by(resolution) %>%
  summarise(
    n = sum(is.finite(complementarity_residual)),
    max_abs_residual = max(abs(complementarity_residual), na.rm = TRUE),
    mean_abs_residual = mean(abs(complementarity_residual), na.rm = TRUE),
    pearson_switch_vs_self = suppressWarnings(cor(state_switch_rate, self_transition_probability,
      use = "complete.obs")),
    .groups = "drop"
  )
print(as.data.frame(check_d))
write_table(check_d, file.path(audit_out, "hmm_architecture_check_d_complementarity.csv"))

# --- (e) geometric-dwell check -------------------------------------------------
sep("6e. geometric dwell: mean_dwell_bins vs 1/(1 - P(self))")
check_e <- components %>%
  group_by(resolution) %>%
  summarise(
    n = sum(is.finite(mean_dwell_bins) & is.finite(geometric_dwell_prediction)),
    pearson = suppressWarnings(cor(mean_dwell_bins, geometric_dwell_prediction,
      use = "complete.obs", method = "pearson")),
    spearman = suppressWarnings(cor(mean_dwell_bins, geometric_dwell_prediction,
      use = "complete.obs", method = "spearman")),
    max_rel_deviation = max(abs(mean_dwell_bins - geometric_dwell_prediction) /
      geometric_dwell_prediction, na.rm = TRUE),
    median_rel_deviation = median(abs(mean_dwell_bins - geometric_dwell_prediction) /
      geometric_dwell_prediction, na.rm = TRUE),
    mean_ratio_obs_over_pred = mean(mean_dwell_bins / geometric_dwell_prediction, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_e))
write_table(check_e, file.path(audit_out, "hmm_architecture_check_e_geometric_dwell.csv"))

# (e2) Why the epoch-level geometric check is biased upward: the epoch-level
# mean_dwell_bins is a frac_time-weighted mean of PER-STATE bout lengths, whereas
# 1/(1 - sum_s pi_s P(s|s)) inverts the POOLED diagonal. 1/(1-p) is convex, so by
# Jensen the weighted mean of per-state 1/(1 - P(s|s)) must be >= the pooled
# inversion. The per-state comparison is the fair one.
cat("\n(e2) per-state geometric prediction, frac_time-weighted, vs mean_dwell_bins:\n")
per_state_geom <- map_dfr(resolutions, function(res) {
  tr <- bundles[[res]]$transitions %>%
    add_epoch_keys() %>%
    mutate(
      NextState = as.character(.data$NextState),
      Transitions = suppressWarnings(as.numeric(.data$Transitions))
    ) %>%
    group_by(across(all_of(c(EPOCH_KEY, "State")))) %>%
    summarise(
      n_from_state = sum(Transitions, na.rm = TRUE),
      P_self_state = sum(Transitions[State == NextState], na.rm = TRUE) / sum(Transitions, na.rm = TRUE),
      .groups = "drop"
    )
  occ_w <- bundles[[res]]$occupancy %>%
    add_epoch_keys() %>%
    transmute(across(all_of(EPOCH_KEY)), State, frac_time = suppressWarnings(as.numeric(.data$frac_time)))
  dw <- bundles[[res]]$dwell %>%
    add_epoch_keys() %>%
    transmute(across(all_of(EPOCH_KEY)), State,
      mean_dwell_bins_state = suppressWarnings(as.numeric(.data$mean_dwell_bins))
    )
  tr %>%
    inner_join(occ_w, by = c(EPOCH_KEY, "State")) %>%
    inner_join(dw, by = c(EPOCH_KEY, "State")) %>%
    mutate(geom_state = if_else(P_self_state < 1, 1 / (1 - P_self_state), NA_real_)) %>%
    group_by(across(all_of(EPOCH_KEY))) %>%
    summarise(
      mean_dwell_bins = {
        ok <- is.finite(frac_time) & is.finite(mean_dwell_bins_state) & frac_time > 0
        if (any(ok)) sum(frac_time[ok] * mean_dwell_bins_state[ok]) / sum(frac_time[ok]) else NA_real_
      },
      geom_weighted_per_state = {
        ok <- is.finite(frac_time) & is.finite(geom_state) & frac_time > 0
        if (any(ok)) sum(frac_time[ok] * geom_state[ok]) / sum(frac_time[ok]) else NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(resolution = res)
})
check_e2 <- per_state_geom %>%
  group_by(resolution) %>%
  summarise(
    n = sum(is.finite(mean_dwell_bins) & is.finite(geom_weighted_per_state)),
    pearson = suppressWarnings(cor(mean_dwell_bins, geom_weighted_per_state,
      use = "complete.obs", method = "pearson")),
    spearman = suppressWarnings(cor(mean_dwell_bins, geom_weighted_per_state,
      use = "complete.obs", method = "spearman")),
    max_rel_deviation = max(abs(mean_dwell_bins - geom_weighted_per_state) /
      geom_weighted_per_state, na.rm = TRUE),
    median_rel_deviation = median(abs(mean_dwell_bins - geom_weighted_per_state) /
      geom_weighted_per_state, na.rm = TRUE),
    mean_ratio_obs_over_pred = mean(mean_dwell_bins / geom_weighted_per_state, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_e2))
write_table(check_e2, file.path(audit_out, "hmm_architecture_check_e2_geometric_dwell_per_state.csv"))

# (e3) degenerate epochs: no state switch at all -> switch_rate 0, P(self) 1,
# transition entropy 0, and 1/(1-P(self)) undefined. These are RETAINED.
cat("\n(e3) degenerate epochs (state_switch_rate == 0 and/or single occupied state):\n")
check_e3 <- components %>%
  filter(state_switch_rate == 0 | n_states_occupied == 1 | !is.finite(geometric_dwell_prediction)) %>%
  select(
    resolution, AnimalNum, Group, Sex, CageChange, PhaseClass, n_states_occupied,
    occupancy_entropy, transition_entropy, state_switch_rate, self_transition_probability,
    mean_dwell_bins, geometric_dwell_prediction, n_transitions, short_epoch
  )
print(as.data.frame(check_e3))
write_table(check_e3, file.path(audit_out, "hmm_architecture_check_e3_degenerate_epochs.csv"))

# --- (f) range / sanity of the two entropies ----------------------------------
sep("6f. entropy ranges (bound = log 4 = 1.3862944)")
check_f <- components %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    n = n(),
    occ_ent_min = min(occupancy_entropy, na.rm = TRUE),
    occ_ent_median = median(occupancy_entropy, na.rm = TRUE),
    occ_ent_max = max(occupancy_entropy, na.rm = TRUE),
    occ_ent_out_of_bounds = sum(occupancy_entropy < 0 | occupancy_entropy > log(4), na.rm = TRUE),
    trans_ent_min = min(transition_entropy, na.rm = TRUE),
    trans_ent_median = median(transition_entropy, na.rm = TRUE),
    trans_ent_max = max(transition_entropy, na.rm = TRUE),
    trans_ent_out_of_bounds = sum(transition_entropy < 0 | transition_entropy > log(4), na.rm = TRUE),
    trans_ent_mm_max = max(transition_entropy_mm, na.rm = TRUE),
    trans_ent_mm_over_log4 = sum(transition_entropy_mm > log(4), na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_f))
cat("\nfrac_time sums (must be 1) and pi_s sums (must be 1):\n")
check_f2 <- components %>%
  group_by(resolution) %>%
  summarise(
    max_abs_frac_time_sum_minus_1 = max(abs(frac_time_sum - 1), na.rm = TRUE),
    max_abs_pi_sum_minus_1 = max(abs(pi_sum - 1), na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_f2))
cat("\nTransitionProbability recomputation check (P(t|s) = Transitions / row sum):\n")
print(as.data.frame(prob_checks))
write_table(check_f, file.path(audit_out, "hmm_architecture_check_f_entropy_ranges.csv"))
write_table(
  bind_cols(prob_checks, check_f2 %>% select(-resolution)),
  file.path(audit_out, "hmm_architecture_check_f_estimator_identities.csv")
)

# --- (g) length-dependence of transition entropy ------------------------------
sep("6g. length dependence: entropy vs n_transitions (Spearman)")
check_g <- components %>%
  group_by(resolution) %>%
  summarise(
    n = sum(is.finite(transition_entropy) & is.finite(n_transitions)),
    spearman_H_vs_n = suppressWarnings(cor(transition_entropy, n_transitions,
      use = "complete.obs", method = "spearman")),
    pearson_H_vs_n = suppressWarnings(cor(transition_entropy, n_transitions,
      use = "complete.obs", method = "pearson")),
    spearman_Hmm_vs_n = suppressWarnings(cor(transition_entropy_mm, n_transitions,
      use = "complete.obs", method = "spearman")),
    pearson_Hmm_vs_n = suppressWarnings(cor(transition_entropy_mm, n_transitions,
      use = "complete.obs", method = "pearson")),
    spearman_occEnt_vs_n = suppressWarnings(cor(occupancy_entropy, n_transitions,
      use = "complete.obs", method = "spearman")),
    mm_minus_plugin_mean = mean(transition_entropy_mm - transition_entropy, na.rm = TRUE),
    mm_minus_plugin_max = max(transition_entropy_mm - transition_entropy, na.rm = TRUE),
    n_transitions_min = min(n_transitions, na.rm = TRUE),
    n_transitions_median = median(n_transitions, na.rm = TRUE),
    n_transitions_max = max(n_transitions, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_g))
cat("\nsame Spearman split by PhaseClass (Active vs Inactive have different lengths):\n")
check_g2 <- components %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    spearman_H_vs_n = suppressWarnings(cor(transition_entropy, n_transitions,
      use = "complete.obs", method = "spearman")),
    spearman_Hmm_vs_n = suppressWarnings(cor(transition_entropy_mm, n_transitions,
      use = "complete.obs", method = "spearman")),
    n_transitions_median = median(n_transitions, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_g2))
write_table(check_g, file.path(audit_out, "hmm_architecture_check_g_length_dependence.csv"))
write_table(check_g2, file.path(audit_out, "hmm_architecture_check_g2_length_dependence_by_phase.csv"))

# (g3) epoch length here is essentially binary, so the length dependence is a
# short-vs-standard contrast rather than a smooth gradient. Report it that way.
# (R6) CORRECTION: the ratio is NOT "~half" in both phases. At 5min_based the
# short (CC4) epochs have 288 vs 576 bins in the Active phase (one half) but
# 144 vs 432 bins in the Inactive phase (one THIRD). The observed medians are
# printed by check_g3 / check_g4c below.
cat("\n(g3) transition entropy in short vs standard epochs (length is bimodal):\n")
check_g3 <- components %>%
  group_by(resolution, PhaseClass, short_epoch) %>%
  summarise(
    n = n(),
    n_transitions_median = median(n_transitions, na.rm = TRUE),
    transition_entropy_mean = mean(transition_entropy, na.rm = TRUE),
    transition_entropy_mm_mean = mean(transition_entropy_mm, na.rm = TRUE),
    occupancy_entropy_mean = mean(occupancy_entropy, na.rm = TRUE),
    state_switch_rate_mean = mean(state_switch_rate, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_g3))
cat("\nhedges g (short vs standard) for the two entropies, within resolution x phase:\n")
check_g4 <- components %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    g_transition_entropy = hmm_hedges_g(
      transition_entropy[!short_epoch], transition_entropy[short_epoch]
    ),
    g_transition_entropy_mm = hmm_hedges_g(
      transition_entropy_mm[!short_epoch], transition_entropy_mm[short_epoch]
    ),
    g_occupancy_entropy = hmm_hedges_g(
      occupancy_entropy[!short_epoch], occupancy_entropy[short_epoch]
    ),
    .groups = "drop"
  )
print(as.data.frame(check_g4))
write_table(check_g3, file.path(audit_out, "hmm_architecture_check_g3_short_vs_standard_entropy.csv"))
write_table(check_g4, file.path(audit_out, "hmm_architecture_check_g4_short_vs_standard_hedges_g.csv"))

# (g4b) Where does short_epoch live? If it is confined to whole cage changes then
# it is fully collinear with CageChangeIndex, which is BOTH a standardization
# context variable and a fixed effect in fit_repeated_measures_domain_contrasts().
# In that case the epoch-length confound is absorbed by design and cannot bias the
# group contrasts, as long as groups are balanced within the affected cage change.
cat("\n(g4b) short_epoch structure vs CageChangeIndex and Group:\n")
print(as.data.frame(components %>% count(resolution, CageChangeIndex, short_epoch, name = "n_epochs")))
print(as.data.frame(components %>% count(resolution, short_epoch, Group, name = "n_epochs")))
cat("\n(g4c) length dependence AFTER removing the short cage change (standard epochs only):\n")
check_g4c <- components %>%
  filter(!short_epoch) %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    n = n(),
    n_transitions_min = min(n_transitions), n_transitions_max = max(n_transitions),
    spearman_H_vs_n = suppressWarnings(cor(transition_entropy, n_transitions,
      use = "complete.obs", method = "spearman")),
    spearman_Hmm_vs_n = suppressWarnings(cor(transition_entropy_mm, n_transitions,
      use = "complete.obs", method = "spearman")),
    spearman_occEnt_vs_n = suppressWarnings(cor(occupancy_entropy, n_transitions,
      use = "complete.obs", method = "spearman")),
    .groups = "drop"
  )
print(as.data.frame(check_g4c))
write_table(check_g4c, file.path(audit_out, "hmm_architecture_check_g4c_length_dependence_standard_epochs.csv"))

# (g5) exact pairwise redundancy for the pairs the task flagged as suspect.
cat("\n(g5) exact redundancy correlations for the flagged pairs:\n")
check_g5 <- components %>%
  group_by(resolution) %>%
  summarise(
    sp_switchrate_vs_switchesperhour = suppressWarnings(cor(state_switch_rate, switches_per_hour,
      use = "complete.obs", method = "spearman")),
    sp_switchrate_vs_selfprob = suppressWarnings(cor(state_switch_rate, self_transition_probability,
      use = "complete.obs", method = "spearman")),
    sp_transent_vs_switchrate = suppressWarnings(cor(transition_entropy, state_switch_rate,
      use = "complete.obs", method = "spearman")),
    sp_transent_vs_transentmm = suppressWarnings(cor(transition_entropy, transition_entropy_mm,
      use = "complete.obs", method = "spearman")),
    pe_transent_vs_transentmm = suppressWarnings(cor(transition_entropy, transition_entropy_mm,
      use = "complete.obs", method = "pearson")),
    sp_dwellbins_vs_selfprob = suppressWarnings(cor(mean_dwell_bins, self_transition_probability,
      use = "complete.obs", method = "spearman")),
    sp_dwellbins_vs_dwellhours = suppressWarnings(cor(mean_dwell_bins, mean_dwell_hours,
      use = "complete.obs", method = "spearman")),
    sp_occent_vs_transent = suppressWarnings(cor(occupancy_entropy, transition_entropy,
      use = "complete.obs", method = "spearman")),
    sp_occentz_vs_transentz = suppressWarnings(cor(occupancy_entropy_z, transition_entropy_z,
      use = "complete.obs", method = "spearman")),
    sp_topproxfrac_vs_inactivefrac = suppressWarnings(cor(top_proximity_state_fraction,
      inactive_state_fraction, use = "complete.obs", method = "spearman")),
    sp_topproxfracz_vs_inactivefracz = suppressWarnings(cor(top_proximity_state_fraction_z,
      inactive_state_fraction_z, use = "complete.obs", method = "spearman")),
    .groups = "drop"
  )
print(as.data.frame(t(check_g5 %>% column_to_rownames("resolution"))), digits = 8)
write_table(check_g5, file.path(audit_out, "hmm_architecture_check_g5_exact_redundancy.csv"))

# --- (h) bin-size dependence of dwell -----------------------------------------
sep("6h. bin-size dependence of dwell metrics")
check_h <- components %>%
  group_by(resolution) %>%
  summarise(
    bin_size_sec = paste(sort(unique(bin_size_sec)), collapse = "|"),
    mean_dwell_bins_mean = mean(mean_dwell_bins, na.rm = TRUE),
    mean_dwell_bins_median = median(mean_dwell_bins, na.rm = TRUE),
    mean_dwell_bins_min = min(mean_dwell_bins, na.rm = TRUE),
    mean_dwell_bins_max = max(mean_dwell_bins, na.rm = TRUE),
    mean_dwell_hours_mean = mean(mean_dwell_hours, na.rm = TRUE),
    mean_dwell_hours_median = median(mean_dwell_hours, na.rm = TRUE),
    switches_per_hour_median = median(switches_per_hour, na.rm = TRUE),
    state_switch_rate_median = median(state_switch_rate, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_h))
write_table(check_h, file.path(audit_out, "hmm_architecture_check_h_bin_size_dependence.csv"))

# --- (i) epoch DURATION *and* DATA COMPLETENESS flags (documented, NOT dropped) -
# (R1) CORRECTION OF SCOPE. The previous version of this check examined only
# epoch DURATION (short_epoch, cage_change_duration_class) while its heading
# implied that missingness had been checked. Duration and completeness are
# different things: every CC4 epoch is short but complete, whereas the chip-loss
# QC classes flag read-SPARSE epochs at every cage change. Both are reported here.
sep("6i. duration AND data-completeness flags (retained, documented)")
check_i <- components %>%
  group_by(resolution) %>%
  summarise(
    n = n(),
    n_short_epoch_TRUE = sum(short_epoch, na.rm = TRUE),
    n_short_epoch_NA = sum(is.na(short_epoch)),
    n_duration_class_short = sum(cage_change_duration_class == "short", na.rm = TRUE),
    duration_classes = paste(sort(unique(cage_change_duration_class)), collapse = "|"),
    n_either_duration_flag = sum(short_epoch | cage_change_duration_class == "short", na.rm = TRUE),
    # completeness side (new)
    n_missing_observed_fraction = sum(is.na(observed_fraction)),
    observed_fraction_min = min(observed_fraction, na.rm = TRUE),
    observed_fraction_median = median(observed_fraction, na.rm = TRUE),
    n_observed_fraction_lt_0.6 = sum(observed_fraction < 0.6, na.rm = TRUE),
    n_qc_recommends_exclusion = sum(qc_recommends_exclusion, na.rm = TRUE),
    n_qc_exclude_after_dropout = sum(qc_exclude_after_dropout, na.rm = TRUE),
    n_qc_insufficient_data = sum(qc_insufficient_data, na.rm = TRUE),
    n_active_dropout_leaveout = sum(qc_active_dropout_leaveout_flag, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_i))
cat("\nbreakdown of duration classes:\n")
print(as.data.frame(components %>% count(resolution, cage_change_duration_class, short_epoch, name = "n")))
cat("\nbreakdown of chip-loss QC classes by phase (identical at both resolutions):\n")
print(as.data.frame(components %>% count(resolution, PhaseClass, qc_epoch_class, name = "n")))
cat("\nshort_epoch (duration) vs qc_recommends_exclusion (completeness) cross-tab:\n")
print(as.data.frame(components %>% count(resolution, short_epoch, qc_recommends_exclusion, name = "n")))
write_table(check_i, file.path(audit_out, "hmm_architecture_check_i_duration_and_completeness.csv"))

# ================================================================================
# (R1) 6j. READ-DENSITY CONFOUND
# observed_fraction = n_reads / expected_reads is the raw RFID read density that
# Movement / Entropy / Proximity are computed from. Sparse reads depress computed
# movement, which pushes bins into the low-movement HMM states, which RAISES
# inactive_state_fraction -- and z(inactive) is the dominant variance term of the
# audited composite. So a coverage gradient and the reported phenotype are not
# separable in this table without explicit adjustment. Quantify it three ways:
# pooled, WITHIN each z-standardization context cell, and at the animal level.
# ================================================================================
sep("6j. read-density confound: components vs observed_fraction")

sp <- function(x, y) suppressWarnings(cor(x, y, use = "complete.obs", method = "spearman"))

check_j_pooled <- components %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    n = sum(is.finite(observed_fraction)),
    sp_composite_vs_obsfrac = sp(`Behavioral state architecture`, observed_fraction),
    sp_inactive_frac_vs_obsfrac = sp(inactive_state_fraction, observed_fraction),
    sp_occ_entropy_vs_obsfrac = sp(occupancy_entropy, observed_fraction),
    sp_top_prox_frac_vs_obsfrac = sp(top_proximity_state_fraction, observed_fraction),
    sp_trans_entropy_vs_obsfrac = sp(transition_entropy, observed_fraction),
    sp_switch_rate_vs_obsfrac = sp(state_switch_rate, observed_fraction),
    sp_dwell_bins_vs_obsfrac = sp(mean_dwell_bins, observed_fraction),
    .groups = "drop"
  )
print(as.data.frame(check_j_pooled))
write_table(check_j_pooled, file.path(audit_out, "hmm_architecture_check_j_read_density_pooled.csv"))

cat("\n(6j2) WITHIN each Sex x PhaseClass x CageChangeIndex standardization cell:\n")
check_j_cell <- components %>%
  group_by(resolution, Sex, PhaseClass, CageChangeIndex) %>%
  summarise(
    n = n(),
    sp_composite_vs_obsfrac = sp(`Behavioral state architecture`, observed_fraction),
    sp_inactive_frac_vs_obsfrac = sp(inactive_state_fraction, observed_fraction),
    .groups = "drop"
  )
check_j_cell_summary <- check_j_cell %>%
  group_by(resolution, PhaseClass) %>%
  summarise(
    n_cells = n(),
    median_sp_composite = median(sp_composite_vs_obsfrac, na.rm = TRUE),
    min_sp_composite = min(sp_composite_vs_obsfrac, na.rm = TRUE),
    max_sp_composite = max(sp_composite_vs_obsfrac, na.rm = TRUE),
    median_sp_inactive = median(sp_inactive_frac_vs_obsfrac, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_j_cell_summary))
write_table(check_j_cell, file.path(audit_out, "hmm_architecture_check_j2_read_density_within_context.csv"))

cat("\n(6j3) ANIMAL level (one mean per animal across cage changes):\n")
check_j_animal <- components %>%
  group_by(resolution, Sex, PhaseClass, AnimalNum, Group) %>%
  summarise(
    composite = mean(`Behavioral state architecture`, na.rm = TRUE),
    inactive_frac = mean(inactive_state_fraction, na.rm = TRUE),
    obs_frac = mean(observed_fraction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(resolution, Sex, PhaseClass) %>%
  summarise(
    n_animals = n(),
    sp_composite_vs_obsfrac = sp(composite, obs_frac),
    sp_inactive_frac_vs_obsfrac = sp(inactive_frac, obs_frac),
    .groups = "drop"
  )
print(as.data.frame(check_j_animal))
write_table(check_j_animal, file.path(audit_out, "hmm_architecture_check_j3_read_density_animal_level.csv"))

cat("\n(6j4) observed_fraction by Group (the confound and the phenotype run the same way?):\n")
check_j_group <- components %>%
  group_by(resolution, Sex, PhaseClass, Group) %>%
  summarise(
    n_epochs = n(),
    obs_frac_mean = mean(observed_fraction, na.rm = TRUE),
    obs_frac_median = median(observed_fraction, na.rm = TRUE),
    obs_frac_min = min(observed_fraction, na.rm = TRUE),
    n_epochs_obsfrac_lt_0.6 = sum(observed_fraction < 0.6, na.rm = TRUE),
    composite_mean = mean(`Behavioral state architecture`, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_j_group))
write_table(check_j_group, file.path(audit_out, "hmm_architecture_check_j4_read_density_by_group.csv"))

cat("\n(6j5) the 8 Active-phase 'exclude_after_dropout' epochs, with their z-context ranks:\n")
check_j5 <- components %>%
  group_by(resolution, Sex, PhaseClass, CageChangeIndex) %>%
  mutate(
    obsfrac_rank_in_context = rank(observed_fraction, ties.method = "min"),
    n_in_context = n()
  ) %>%
  ungroup() %>%
  filter(qc_active_dropout_leaveout_flag) %>%
  select(
    resolution, AnimalNum, Group, Sex, CageChange, PhaseClass, qc_epoch_class,
    observed_fraction, obsfrac_rank_in_context, n_in_context,
    occupancy_entropy, inactive_state_fraction, `Behavioral state architecture`
  ) %>%
  arrange(resolution, AnimalNum, CageChange)
print(as.data.frame(check_j5), digits = 5)
write_table(check_j5, file.path(audit_out, "hmm_architecture_check_j5_active_dropout_epochs.csv"))

# ================================================================================
# (R2/R3) 6k. MODEL SENSITIVITY OF THE PRIMARY QUANTITY
# The estimator is fit_repeated_measures_domain_contrasts() from
# Functions/hmm_stage14_helpers.R, used UNCHANGED: random intercept for
# AnimalNum, factor(CageChangeIndex) fixed effect, CC1-CC4 all retained. Only the
# ROW SET changes between scenarios. A covariate-augmented model is reported
# separately and is explicitly NOT a replacement for the primary estimator.
# ================================================================================
sep("6k. leave-out and covariate sensitivity of `Behavioral state architecture`")

DOMAIN_NAME <- "Behavioral state architecture"

to_domain_tbl <- function(dat) {
  dat %>%
    transmute(
      AnimalNum, Group, Sex, CageChangeIndex, PhaseClass,
      Domain = DOMAIN_NAME,
      DomainScore = .data[[DOMAIN_NAME]],
      observed_fraction
    )
}

run_scenario <- function(dat, scenario, res, restandardize = FALSE) {
  d <- dat
  if (restandardize) {
    d <- strict_standardize_within_context(d, "occupancy_entropy")
    d <- strict_standardize_within_context(d, "inactive_state_fraction")
    d[[DOMAIN_NAME]] <- 0.5 * d$occupancy_entropy_z - d$inactive_state_fraction_z
  }
  dd <- to_domain_tbl(d)
  map_dfr(c("Active", "Inactive"), function(ph) {
    fit_repeated_measures_domain_contrasts(dd, DOMAIN_NAME, ph)$contrasts %>%
      mutate(
        resolution = res, scenario = scenario,
        rescored_after_deletion = restandardize,
        n_epoch_rows_used = sum(dd$PhaseClass == ph & is.finite(dd$DomainScore)),
        .before = 1
      )
  })
}

scenario_defs <- list(
  primary_full = function(d) d,
  drop_8_active_qc_dropout_epochs = function(d) d %>% filter(!qc_active_dropout_leaveout_flag),
  drop_animals_OR539_OR540 = function(d) d %>% filter(!AnimalNum %in% c("OR539", "OR540")),
  drop_all_qc_recommends_exclusion = function(d) d %>% filter(!coalesce(qc_recommends_exclusion, FALSE))
)

sens_rows <- list()
for (res in resolutions) {
  d0 <- components %>% filter(resolution == res)
  for (sc in names(scenario_defs)) {
    d1 <- scenario_defs[[sc]](d0)
    sens_rows <- c(sens_rows, list(run_scenario(d1, sc, res, restandardize = FALSE)))
  }
  # the leave-out with the score RE-standardized inside the reduced row set, so
  # both readings of "remove those epochs" are on the record
  d2 <- scenario_defs$drop_8_active_qc_dropout_epochs(d0)
  sens_rows <- c(sens_rows, list(run_scenario(
    d2, "drop_8_active_qc_dropout_epochs_rescored", res, restandardize = TRUE
  )))
}
sensitivity <- bind_rows(sens_rows) %>%
  group_by(resolution, scenario, Sex, PhaseClass) %>%
  mutate(
    audit_sensitivity_FDR_q = p.adjust(mixed_model_p, method = "BH"),
    audit_sensitivity_FDR_family_id = paste(
      "AUDIT_ONLY_3_group_contrasts", scenario, resolution, Sex, PhaseClass, sep = "__"
    ),
    n_tests_in_audit_family = sum(is.finite(mixed_model_p))
  ) %>%
  ungroup()

# the SHIPPED primary FDR family (18 tests: displayed domains x 3 contrasts within
# resolution x Sex x Phase) is NOT redefined here. It is joined for reference.
shipped_sens_path <- file.path(
  project_root,
  "analysis_ready/12_systems_neuroscience_summary/5min_based/stats_tables",
  "systems_sis_hmm_resolution_sensitivity.csv"
)
if (file.exists(shipped_sens_path)) {
  shipped_primary <- readr::read_csv(shipped_sens_path, progress = FALSE, show_col_types = FALSE) %>%
    filter(Domain == DOMAIN_NAME) %>%
    transmute(
      resolution, Sex, PhaseClass = Phase, contrast,
      shipped_estimate = mixed_model_estimate,
      shipped_SE = mixed_model_SE,
      shipped_p = mixed_model_p,
      shipped_FDR_q = FDR_q,
      shipped_FDR_family_id = FDR_family_id,
      shipped_n_tests_in_family = n_tests_in_family
    )
  sensitivity <- sensitivity %>%
    left_join(shipped_primary, by = c("resolution", "Sex", "PhaseClass", "contrast"))
}

cat("\nFEMALE ACTIVE (the primary quantity) across scenarios:\n")
print(as.data.frame(sensitivity %>%
  filter(Sex == "Female", PhaseClass == "Active") %>%
  select(resolution, scenario, contrast, n_epoch_rows_used, mixed_model_estimate,
    mixed_model_SE, mixed_model_p, audit_sensitivity_FDR_q, animal_level_hedges_g,
    n_ref_animals, n_comp_animals, model_status) %>%
  arrange(resolution, contrast, scenario)), digits = 5)

cat("\nINACTIVE phase, both sexes (the separate phenotype), RES-CON and SUS-CON:\n")
print(as.data.frame(sensitivity %>%
  filter(PhaseClass == "Inactive", contrast %in% c("RES-CON", "SUS-CON")) %>%
  select(resolution, scenario, Sex, contrast, mixed_model_estimate, mixed_model_SE,
    mixed_model_p, animal_level_hedges_g, n_ref_animals) %>%
  arrange(resolution, Sex, contrast, scenario)), digits = 5)

write_table(sensitivity, file.path(audit_out, "hmm_architecture_check_k_model_sensitivity.csv"))

# (6k2) observed_fraction as a COVARIATE. fit_repeated_measures_domain_contrasts()
# takes no covariate argument, so a covariate sensitivity necessarily requires an
# augmented model. It is a strict superset of the primary formula: the random
# intercept and factor(CageChangeIndex) are retained and only
# "+ observed_fraction" is added. emmeans averages over observed_fraction at its
# mean. This is reported ALONGSIDE the primary estimate, never instead of it.
cat("\n(6k2) covariate-adjusted sensitivity: + observed_fraction\n")
fit_covariate_sensitivity <- function(dat, phase, res, label) {
  model_dat <- dat %>%
    filter(PhaseClass == phase, is.finite(DomainScore), is.finite(observed_fraction)) %>%
    transmute(
      AnimalNum = factor(as.character(AnimalNum)),
      Group = factor(as.character(Group), levels = c("CON", "RES", "SUS")),
      Sex = factor(as.character(Sex), levels = c("Female", "Male")),
      CageChangeIndex = factor(CageChangeIndex),
      observed_fraction = as.numeric(observed_fraction),
      DomainScore = as.numeric(DomainScore)
    ) %>%
    filter(!is.na(Group), !is.na(Sex), !is.na(CageChangeIndex))
  f <- "DomainScore ~ Group * Sex + factor(CageChangeIndex) + observed_fraction + (1 | AnimalNum)"
  warn <- character()
  fit <- tryCatch(withCallingHandlers(
    lmerTest::lmer(as.formula(f), data = model_dat),
    warning = function(w) {
      warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning")
    }
  ), error = function(e) e)
  if (inherits(fit, "error")) {
    return(tibble(resolution = res, model = label, PhaseClass = phase,
      Sex = NA_character_, contrast = NA_character_, estimate = NA_real_,
      SE = NA_real_, p = NA_real_, covariate_beta = NA_real_, covariate_p = NA_real_,
      n_rows = nrow(model_dat), model_formula = f, model_status = conditionMessage(fit)))
  }
  cf <- summary(fit)$coefficients
  emm <- emmeans::emmeans(fit, ~ Group | Sex)
  emmeans::contrast(emm, method = list(
    "RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1)
  ), adjust = "none") %>%
    as.data.frame() %>%
    as_tibble() %>%
    transmute(
      resolution = res, model = label, PhaseClass = phase,
      Sex = as.character(Sex), contrast = as.character(contrast),
      estimate, SE, p = p.value,
      covariate_beta = cf["observed_fraction", "Estimate"],
      covariate_p = cf["observed_fraction", ncol(cf)],
      n_rows = nrow(model_dat), model_formula = f,
      model_status = if (lme4::isSingular(fit, tol = 1e-4)) "singular_fit" else "fitted",
      model_warnings = paste(unique(warn), collapse = " | ")
    )
}
# CAVEAT to record with the covariate model: observed_fraction is an upstream
# MEASUREMENT variable, but it is not guaranteed to be behaviour-free (a mouse that
# moves less triggers fewer antenna reads), so the adjusted estimate is a LOWER
# bound on the behavioural effect and the unadjusted estimate is an UPPER bound.
# Neither is "the" answer; the gap between them is the size of the ambiguity.
cov_sens <- map_dfr(resolutions, function(res) {
  dd <- to_domain_tbl(components %>% filter(resolution == res))
  map_dfr(c("Active", "Inactive"), ~ fit_covariate_sensitivity(dd, .x, res, "plus_observed_fraction"))
})
print(as.data.frame(cov_sens %>% select(resolution, PhaseClass, Sex, contrast, estimate,
  SE, p, covariate_beta, covariate_p, n_rows, model_status)), digits = 5)
write_table(cov_sens, file.path(audit_out, "hmm_architecture_check_k2_covariate_sensitivity.csv"))

# ================================================================================
# (R5) 6l. LENGTH DEPENDENCE -- the decisive evidence
# Pooled Spearman(transition_entropy, n_transitions) ~ 0.61 looks alarming. The
# question that matters is whether ANY length variation survives inside the
# z-standardization context (Sex x PhaseClass x CageChangeIndex), which is also
# the fixed-effect structure of the estimator. Report both, plus a subsampling
# test that measures the ACTUAL plug-in bias in entropy units.
# ================================================================================
sep("6l. length dependence: design collinearity + subsampling bias")

check_l1 <- map_dfr(resolutions, function(res) {
  d <- components %>% filter(resolution == res)
  m <- lm(n_transitions ~ factor(CageChangeIndex) * PhaseClass, data = d)
  cell_sd <- d %>%
    group_by(Sex, PhaseClass, CageChangeIndex) %>%
    summarise(sd_n = sd(n_transitions), rng = diff(range(n_transitions)), .groups = "drop")
  within_sp <- d %>%
    group_by(Sex, PhaseClass, CageChangeIndex) %>%
    summarise(sp_H = sp(transition_entropy, n_transitions), .groups = "drop")
  tibble(
    resolution = res,
    r2_n_transitions_on_CC_x_Phase = summary(m)$r.squared,
    n_context_cells = nrow(cell_sd),
    n_cells_with_zero_length_variance = sum(cell_sd$sd_n == 0),
    max_within_cell_range_bins = max(cell_sd$rng),
    n_within_cell_spearman_defined = sum(is.finite(within_sp$sp_H)),
    n_within_cell_spearman_undefined = sum(!is.finite(within_sp$sp_H)),
    mean_within_context_spearman_H_vs_n = mean(within_sp$sp_H, na.rm = TRUE),
    max_abs_within_context_spearman_H_vs_n = max(abs(within_sp$sp_H), na.rm = TRUE)
  )
})
print(as.data.frame(check_l1))
cat("\nNOTE: the NA within-cell correlations in check_g4c are NA because the\n")
cat("within-cell SD of n_transitions is EXACTLY 0 there (every Inactive epoch in a\n")
cat("given cage change has the same length), not because the estimate is unstable.\n")
write_table(check_l1, file.path(audit_out, "hmm_architecture_check_l1_length_design_collinearity.csv"))

# subsampling: truncate every epoch to the shortest observed length L and measure
# the shift in the plug-in entropy rate. First-L window and 10 random contiguous
# windows. This is the honest magnitude of the small-sample bias.
chain_stats <- function(s) {
  n <- length(s)
  if (n < 2L) return(c(H = NA_real_, H_mm = NA_real_, sw = NA_real_))
  f <- factor(s)
  tab <- table(f[-n], f[-1])
  ns <- rowSums(tab)
  keep <- ns > 0
  tab <- tab[keep, , drop = FALSE]
  ns <- ns[keep]
  if (length(ns) == 0L) return(c(H = NA_real_, H_mm = NA_real_, sw = NA_real_))
  N <- sum(ns)
  pi_s <- ns / N
  Hs <- apply(tab, 1, function(r) {
    p <- r[r > 0] / sum(r)
    -sum(p * log(p))
  })
  ms <- apply(tab, 1, function(r) sum(r > 0))
  diag_idx <- cbind(seq_len(nrow(tab)), match(rownames(tab), colnames(tab)))
  self_n <- sum(tab[diag_idx], na.rm = TRUE)
  c(
    H = sum(pi_s * Hs),
    H_mm = sum(pi_s * (Hs + (ms - 1) / (2 * ns))),
    sw = (N - self_n) / N
  )
}

check_l2 <- map_dfr(resolutions, function(res) {
  asg <- bundles[[res]]$assignments %>%
    add_epoch_keys() %>%
    mutate(
      TimeIndex = suppressWarnings(as.numeric(.data$TimeIndex)),
      .eid = paste(AnimalNum, CageChange, PhaseClass, sep = "|")
    ) %>%
    arrange(.eid, TimeIndex)
  seqs <- split(as.character(asg$State), asg$.eid)
  L <- min(lengths(seqs))
  set.seed(20260902)
  res_tbl <- map_dfr(names(seqs), function(id) {
    s <- seqs[[id]]
    full <- chain_stats(s)
    if (length(s) <= L) {
      return(tibble(
        .eid = id, n_bins = length(s), is_long = FALSE,
        H_full = full[["H"]], H_mm_full = full[["H_mm"]], sw_full = full[["sw"]],
        H_firstL = full[["H"]], H_mm_firstL = full[["H_mm"]], sw_firstL = full[["sw"]],
        H_randwin = full[["H"]], H_mm_randwin = full[["H_mm"]], sw_randwin = full[["sw"]]
      ))
    }
    first_stats <- chain_stats(s[seq_len(L)])
    starts <- sample.int(length(s) - L + 1L, size = 10, replace = TRUE)
    win <- vapply(starts, function(st) chain_stats(s[st:(st + L - 1L)]), numeric(3))
    tibble(
      .eid = id, n_bins = length(s), is_long = TRUE,
      H_full = full[["H"]], H_mm_full = full[["H_mm"]], sw_full = full[["sw"]],
      H_firstL = first_stats[["H"]], H_mm_firstL = first_stats[["H_mm"]],
      sw_firstL = first_stats[["sw"]],
      H_randwin = mean(win["H", ]), H_mm_randwin = mean(win["H_mm", ]),
      sw_randwin = mean(win["sw", ])
    )
  })
  long <- res_tbl %>% filter(is_long)
  tibble(
    resolution = res,
    L_shortest_epoch_bins = L,
    n_long_epochs = nrow(long),
    sd_transition_entropy_all = sd(res_tbl$H_full, na.rm = TRUE),
    mean_shift_H_firstL = mean(long$H_firstL - long$H_full),
    median_shift_H_firstL = median(long$H_firstL - long$H_full),
    max_abs_shift_H_firstL = max(abs(long$H_firstL - long$H_full)),
    mean_shift_H_randwin = mean(long$H_randwin - long$H_full),
    mean_shift_sw_randwin = mean(long$sw_randwin - long$sw_full),
    spearman_Hfull_vs_Hrandwin = sp(long$H_full, long$H_randwin),
    mean_mm_correction_full = mean(long$H_mm_full - long$H_full),
    mean_mm_correction_randwin = mean(long$H_mm_randwin - long$H_randwin),
    residual_shift_after_mm_firstL = mean(long$H_mm_firstL - long$H_mm_full),
    residual_shift_after_mm_randwin = mean(long$H_mm_randwin - long$H_mm_full),
    mm_fraction_of_bias_removed_firstL =
      1 - mean(long$H_mm_firstL - long$H_mm_full) / mean(long$H_firstL - long$H_full),
    mm_fraction_of_bias_removed_randwin =
      1 - mean(long$H_mm_randwin - long$H_mm_full) / mean(long$H_randwin - long$H_full)
  )
})
print(as.data.frame(check_l2), digits = 6)
write_table(check_l2, file.path(audit_out, "hmm_architecture_check_l2_subsampling_length_bias.csv"))

cat("\n(6l3) for scale: largest raw between-group difference in transition_entropy\n")
check_l3 <- components %>%
  group_by(resolution, Sex, PhaseClass, AnimalNum, Group) %>%
  summarise(H = mean(transition_entropy), .groups = "drop") %>%
  group_by(resolution, Sex, PhaseClass) %>%
  summarise(
    diff_RES_minus_CON = mean(H[Group == "RES"]) - mean(H[Group == "CON"]),
    diff_SUS_minus_CON = mean(H[Group == "SUS"]) - mean(H[Group == "CON"]),
    .groups = "drop"
  )
print(as.data.frame(check_l3), digits = 5)
write_table(check_l3, file.path(audit_out, "hmm_architecture_check_l3_group_differences_transition_entropy.csv"))

# ================================================================================
# (R4) 6m. TEMPORAL CONTIGUITY and the gap-aware sensitivity variants
# ================================================================================
sep("6m. temporal contiguity of epochs and gap-aware sensitivity")
check_m <- components %>%
  group_by(resolution) %>%
  summarise(
    n_epochs = n(),
    n_epochs_non_contiguous = sum(n_time_gaps > 0),
    median_n_time_gaps = median(n_time_gaps),
    max_n_time_blocks = max(n_time_blocks),
    modal_max_gap_bins = as.numeric(names(sort(table(max_time_gap_bins[max_time_gap_bins > 0]),
      decreasing = TRUE))[1]),
    total_gap_bridged_transitions = sum(n_gap_bridged_transitions),
    total_transitions = sum(n_transitions),
    pct_transitions_bridging_a_gap = 100 * sum(n_gap_bridged_transitions) / sum(n_transitions),
    n_bouts_merged_across_gaps_total = sum(n_bouts_merged_across_gaps),
    mean_delta_transition_entropy = mean(delta_transition_entropy_gapaware),
    max_abs_delta_transition_entropy = max(abs(delta_transition_entropy_gapaware)),
    mean_delta_switch_rate = mean(delta_state_switch_rate_gapaware),
    mean_dwell_bins_shipped_avg = mean(mean_dwell_bins),
    mean_dwell_bins_gapaware_avg = mean(mean_dwell_bins_gapaware),
    mean_delta_dwell_bins = mean(delta_mean_dwell_bins_gapaware),
    max_abs_delta_dwell_bins = max(abs(delta_mean_dwell_bins_gapaware)),
    # two denominators, both reported so the count is unambiguous
    n_epochs_dwell_inflated_gt_5pct_den_shipped = sum(
      abs(delta_mean_dwell_bins_gapaware) / mean_dwell_bins > 0.05, na.rm = TRUE
    ),
    n_epochs_dwell_inflated_gt_5pct_den_gapaware = sum(
      (mean_dwell_bins - mean_dwell_bins_gapaware) / mean_dwell_bins_gapaware > 0.05, na.rm = TRUE
    ),
    spearman_H_shipped_vs_gapaware = sp(transition_entropy, transition_entropy_gapaware),
    spearman_dwell_shipped_vs_gapaware = sp(mean_dwell_bins, mean_dwell_bins_gapaware),
    .groups = "drop"
  )
print(as.data.frame(check_m), digits = 6)
cat("\nn_time_blocks distribution by phase and cage change:\n")
print(as.data.frame(components %>% count(resolution, PhaseClass, CageChangeIndex, n_time_blocks, name = "n")))
cat("\nSANITY: bins in assignments == observed_bins, and n_transitions == observed_bins - 1?\n")
print(as.data.frame(components %>% group_by(resolution) %>% summarise(
  max_abs_bins_diff = max(abs(n_bins_assignments - observed_bins)),
  max_abs_ntrans_minus_bins_plus_1 = max(abs(n_transitions - (observed_bins - 1))),
  .groups = "drop"
)))
write_table(check_m, file.path(audit_out, "hmm_architecture_check_m_temporal_contiguity.csv"))

# --- extra: component redundancy matrix (for downstream agents) ---------------
sep("EXTRA. component correlation matrix (Spearman, within resolution)")
corr_cols <- c(
  "occupancy_entropy", "inactive_state_fraction", "top_proximity_state_fraction",
  "transition_entropy", "transition_entropy_mm", "state_switch_rate",
  "self_transition_probability", "self_transition_probability_unweighted",
  "mean_dwell_bins", "switches_per_hour", "n_transitions"
)
corr_tbl <- map_dfr(resolutions, function(res) {
  m <- components %>% filter(resolution == res) %>% select(all_of(corr_cols)) %>% as.matrix()
  cm <- suppressWarnings(cor(m, use = "pairwise.complete.obs", method = "spearman"))
  as_tibble(as.data.frame(cm), rownames = "metric") %>% mutate(resolution = res, .before = 1)
})
for (res in resolutions) {
  cat("\n", res, ":\n", sep = "")
  m <- components %>% filter(resolution == res) %>% select(all_of(corr_cols)) %>% as.matrix()
  print(round(suppressWarnings(cor(m, use = "pairwise.complete.obs", method = "spearman")), 3))
}
write_table(corr_tbl, file.path(audit_out, "hmm_architecture_component_spearman_correlations.csv"))

# --- extra: raw descriptive table for every component -------------------------
sep("EXTRA. component descriptives by resolution x PhaseClass")
desc_tbl <- components %>%
  select(resolution, PhaseClass, all_of(component_cols)) %>%
  pivot_longer(all_of(component_cols), names_to = "metric", values_to = "value") %>%
  group_by(resolution, PhaseClass, metric) %>%
  summarise(
    n = sum(is.finite(value)),
    min = min(value, na.rm = TRUE), q25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE), mean = mean(value, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE), max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(desc_tbl), digits = 6)
write_table(desc_tbl, file.path(audit_out, "hmm_architecture_component_descriptives.csv"))

# --- extra: occupied-state distribution + composite loading -------------------
sep("EXTRA. occupied-state distribution")
check_occ <- components %>% count(resolution, n_states_occupied, name = "n_epochs")
print(as.data.frame(check_occ))
cat("\noccupancy table rows implied (sum of n_states_occupied):\n")
print(as.data.frame(components %>% group_by(resolution) %>%
  summarise(occupancy_rows_implied = sum(n_states_occupied), .groups = "drop")))
write_table(check_occ, file.path(audit_out, "hmm_architecture_occupied_state_distribution.csv"))

sep("EXTRA. how the shipped composite loads on its two live terms")
check_load <- components %>%
  group_by(resolution) %>%
  summarise(
    n = n(),
    r_entropy_z_vs_inactive_z_pearson = suppressWarnings(cor(occupancy_entropy_z, inactive_state_fraction_z,
      use = "complete.obs")),
    r_entropy_z_vs_inactive_z_spearman = suppressWarnings(cor(occupancy_entropy_z, inactive_state_fraction_z,
      use = "complete.obs", method = "spearman")),
    r_composite_vs_entropy_z = suppressWarnings(cor(`Behavioral state architecture`, occupancy_entropy_z,
      use = "complete.obs")),
    r_composite_vs_inactive_z = suppressWarnings(cor(`Behavioral state architecture`, inactive_state_fraction_z,
      use = "complete.obs")),
    var_composite = var(`Behavioral state architecture`, na.rm = TRUE),
    var_share_from_inactive_term = var(inactive_state_fraction_z, na.rm = TRUE) /
      var(`Behavioral state architecture`, na.rm = TRUE),
    var_share_from_half_entropy_term = var(0.5 * occupancy_entropy_z, na.rm = TRUE) /
      var(`Behavioral state architecture`, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(check_load))
write_table(check_load, file.path(audit_out, "hmm_architecture_composite_loading.csv"))

sep("EXTRA. is inactive_state_fraction the exact complement of the burst state?")
# At 5min_based three of four states carry the "inactive/low-exploration" label and
# the fourth is the burst/high-movement state, so inactive_state_fraction is
# arithmetically 1 - frac(burst) and -z(inactive) == +z(burst) exactly. At
# 10min_based a "mixed" state also exists, so the identity should NOT hold.
check_comp <- components %>%
  group_by(resolution) %>%
  summarise(
    n_states_inactive_labelled = sum(state_labels[[first(resolution)]]$SemanticState == "inactive/low-exploration"),
    max_abs_inactive_plus_burst_minus_1 = max(abs(inactive_state_fraction + burst_state_fraction - 1), na.rm = TRUE),
    max_abs_z_inactive_plus_z_burst = max(abs(inactive_state_fraction_z + burst_state_fraction_z), na.rm = TRUE),
    r_z_inactive_vs_z_burst = suppressWarnings(cor(inactive_state_fraction_z, burst_state_fraction_z,
      use = "complete.obs")),
    .groups = "drop"
  )
print(as.data.frame(check_comp))
write_table(check_comp, file.path(audit_out, "hmm_architecture_inactive_burst_complement.csv"))

cat("\n================================================================\n")
cat("AUDIT COMPLETE\n")
cat("primary output:", out_path, "\n")
cat("================================================================\n")
