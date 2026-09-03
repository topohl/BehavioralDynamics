# ================================================================================
# audit_hmm_state_architecture_profile.R
# Deliverable 1 of the "Behavioral state architecture" audit.
#
# PURPOSE
#   Audit the SHIPPED ordered semantic classifier annotate_hmm_semantic_states()
#   (Functions/hmm_stage14_helpers.R) to determine whether high-proximity HMM
#   states are being absorbed by the "inactive/low-exploration" label because the
#   rule is ORDERED and MUTUALLY EXCLUSIVE (one label per state), and to quantify
#   the DEGENERACY of the fitted emission structure.
#
#   This script is READ-ONLY with respect to Analysis/ and Functions/. It calls the
#   repo's own classifier rather than re-implementing it; the only re-derivation is
#   of WHICH ordered branch fired, which is then cross-checked against the shipped
#   label (rule_label_consistent).
#
# TERMINOLOGY (deliberately non-anthropomorphic)
#   RFID Proximity is a social-SPATIAL CO-LOCATION / CO-OCCUPANCY proxy, not a
#   direct measure of sociability. States are therefore described as e.g.
#   "low-movement / low-entropy / highest-co-occupancy", never as "social".
#
# OUTPUTS (all under AUDIT_OUT)
#   hmm_state_multidimensional_profile_audit.csv   (primary deliverable)
#   hmm_state_emission_degeneracy_audit.csv
#   hmm_state_binlevel_profile_audit.csv
# ================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(stringr)
  library(purrr)
})

# ---------------------------------------------------------------- setup / paths
script_path <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) normalizePath(f[[1]], winslash = "/", mustWork = FALSE) else NA_character_
}, error = function(e) NA_character_)

repo_root <- if (!is.na(script_path)) {
  dirname(dirname(script_path))
} else {
  "C:/Users/topohl/Documents/GitHub/MMMSociability"
}

source(file.path(repo_root, "Analysis", "_pipeline_setup.R"))
source_mmm_helper("hmm_stage14_helpers.R")

PROJECT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
AUDIT_OUT <- file.path(
  PROJECT,
  "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
)
dir.create(AUDIT_OUT, recursive = TRUE, showWarnings = FALSE)

RESOLUTIONS <- c("5min_based", "10min_based")
BIN_SIZE_SEC <- c("5min_based" = 300, "10min_based" = 600)

# ------------------------------------------------------- canonical 111 roster
roster_file <- file.path(PROJECT, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv")
if (!file.exists(roster_file)) stop("Canonical Stage 01 roster input missing: ", roster_file, call. = FALSE)
roster_raw <- read_csv(
  roster_file,
  col_types = cols(
    .default = col_skip(),
    AnimalNum = col_character(),
    Group = col_character(),
    Sex = col_character()
  ),
  progress = FALSE
)
canonical_roster <- build_canonical_identity_roster(roster_raw, "Stage 01 5min_based roster")
message("[roster] canonical animals = ", nrow(canonical_roster))
stopifnot(nrow(canonical_roster) == 111L)

# --------------------------------------------------------------------- helpers
# Same PhaseClass regex as build_hmm_epoch_scores().
phase_class <- function(x) {
  x <- str_to_lower(as.character(x))
  case_when(
    str_detect(x, "\\binactive\\b|\\blight\\b|\\bday\\b") ~ "Inactive",
    str_detect(x, "\\bactive\\b|\\bdark\\b|\\bnight\\b") ~ "Active",
    TRUE ~ as.character(x)
  )
}

# 1 = highest. ties.method = "min" so exactly-tied state means share the best rank.
rank_desc <- function(x) rank(-x, ties.method = "min", na.last = "keep")

# Empirical-CDF percentile over the 4 state means at this resolution:
#   pct = 100 * (# state means <= this state's mean) / 4
ecdf_pct <- function(x) vapply(x, function(v) 100 * mean(x <= v, na.rm = TRUE), numeric(1))

n_distinct_tol <- function(x, tol) {
  x <- sort(x[is.finite(x)])
  if (length(x) == 0L) return(0L)
  k <- 1L
  anchor <- x[1]
  for (v in x[-1]) {
    if (abs(v - anchor) > tol) {
      k <- k + 1L
      anchor <- v
    }
  }
  as.integer(k)
}

fmt17 <- function(x) vapply(x, function(v) sprintf("%.17g", v), character(1))

# Neutral relative descriptor for one emission dimension.
dim_descriptor <- function(value, values, median_thr, noun) {
  is_max <- abs(value - max(values, na.rm = TRUE)) < 1e-12
  is_min <- abs(value - min(values, na.rm = TRUE)) < 1e-12
  n_tied_max <- sum(abs(values - max(values, na.rm = TRUE)) < 1e-12)
  n_tied_min <- sum(abs(values - min(values, na.rm = TRUE)) < 1e-12)
  qual <- if (is_max && n_tied_max == 1L) {
    "highest"
  } else if (is_min && n_tied_min == 1L) {
    "lowest"
  } else if (value <= median_thr) {
    "below-median"
  } else {
    "above-median"
  }
  paste0(qual, "-", noun)
}

# ============================================================================
# PART A: state-level multidimensional profile (audits the shipped classifier)
# ============================================================================
profile_rows <- list()
degeneracy_rows <- list()
resolution_meta <- list()

for (res in RESOLUTIONS) {
  summary_path <- resolve_configured_hmm_artifact(PROJECT, res, "hmm_state_summary.csv")$path
  state_summary <- read_csv(summary_path, col_types = cols(), progress = FALSE)

  # hmm_state_summary.csv is a MODEL-LEVEL table: it has no AnimalNum column, so
  # audit_hmm_identity() is structurally inapplicable to it (it fails closed on a
  # missing AnimalNum). Identity is audited below on the two per-animal tables
  # (hmm_state_occupancy.csv, hmm_state_assignments.csv) that describe the same fit.
  stopifnot(!"AnimalNum" %in% names(state_summary))

  # ---- the SHIPPED classifier (not a copy)
  labels <- annotate_hmm_semantic_states(state_summary, resolution = res)

  mv <- labels$Movement_z
  en <- labels$Entropy_z
  px <- labels$Proximity_z

  med_mv <- median(mv, na.rm = TRUE)
  med_en <- median(en, na.rm = TRUE)
  med_px <- median(px, na.rm = TRUE)
  q67_mv <- unname(quantile(mv, 0.67, na.rm = TRUE))
  q67_en <- unname(quantile(en, 0.67, na.rm = TRUE))
  q67_px <- unname(quantile(px, 0.67, na.rm = TRUE))

  # ---- independent (UNORDERED) evaluation of each branch predicate
  passes_inactive_rule <- mv <= med_mv & en <= med_en
  passes_social_rule <- px >= q67_px          # rule-2 predicate, evaluated on its own
  passes_burst_rule <- mv >= q67_mv
  passes_exploratory_rule <- en >= q67_en

  # ---- which ORDERED branch actually fired (re-derived, then cross-checked)
  rule_that_fired <- ifelse(
    passes_inactive_rule, 1L,
    ifelse(passes_social_rule, 2L,
      ifelse(passes_burst_rule, 3L,
        ifelse(passes_exploratory_rule, 4L, 5L)
      )
    )
  )
  rule_label_expected <- hmm_semantic_categories[rule_that_fired]

  occ_total <- sum(state_summary$n_bins, na.rm = TRUE)
  idx <- match(labels$State, as.character(state_summary$State))
  n_bins_state <- state_summary$n_bins[idx]
  occ_share <- n_bins_state / occ_total

  top_prox_state <- labels$State[which.max(px)]
  bottom_prox_state <- labels$State[which.min(px)]
  inactive_states <- labels$State[labels$SemanticState == "inactive/low-exploration"]
  inactive_px <- px[labels$SemanticState == "inactive/low-exploration"]
  inactive_occ <- sum(occ_share[labels$SemanticState == "inactive/low-exploration"])

  descriptive_profile <- vapply(seq_along(mv), function(i) {
    paste(
      dim_descriptor(mv[i], mv, med_mv, "movement"),
      dim_descriptor(en[i], en, med_en, "entropy"),
      dim_descriptor(px[i], px, med_px, "co-occupancy"),
      sep = " / "
    )
  }, character(1))

  profile_rows[[res]] <- tibble(
    resolution = res,
    bin_size_sec = BIN_SIZE_SEC[[res]],
    State = labels$State,
    n_bins = n_bins_state,
    total_bins_resolution = occ_total,
    occupancy_share = occ_share,
    Movement_z = mv,
    Entropy_z = en,
    Proximity_z = px,
    Movement_z_full_precision = fmt17(mv),
    Entropy_z_full_precision = fmt17(en),
    Proximity_z_full_precision = fmt17(px),
    movement_rank = as.integer(rank_desc(mv)),
    entropy_rank = as.integer(rank_desc(en)),
    proximity_rank = as.integer(rank_desc(px)),
    movement_pct = ecdf_pct(mv),
    entropy_pct = ecdf_pct(en),
    proximity_pct = ecdf_pct(px),
    percentile_definition = "100 * (count of the 4 state means <= this state mean) / 4",
    rank_definition = "1 = highest; ties.method = min, so exact ties share the best rank",
    SemanticState = labels$SemanticState,
    StateLabel = labels$StateLabel,
    semantic_rule_order = labels$semantic_rule_order,
    median_Movement_z = med_mv,
    median_Entropy_z = med_en,
    median_Proximity_z = med_px,
    q67_Proximity_z = q67_px,
    q67_Movement_z = q67_mv,
    q67_Entropy_z = q67_en,
    movement_gap_to_median = mv - med_mv,
    entropy_gap_to_median = en - med_en,
    proximity_gap_to_q67 = px - q67_px,
    rule_that_fired = rule_that_fired,
    rule_that_fired_label = rule_label_expected,
    rule_label_consistent = rule_label_expected == labels$SemanticState,
    passes_inactive_rule = passes_inactive_rule,
    passes_social_rule = passes_social_rule,
    passes_burst_rule = passes_burst_rule,
    passes_exploratory_rule = passes_exploratory_rule,
    n_rules_passed_unordered = as.integer(
      passes_inactive_rule + passes_social_rule + passes_burst_rule + passes_exploratory_rule
    ),
    flag_low_activity_high_proximity =
      passes_inactive_rule & (passes_social_rule | rank_desc(px) == 1),
    would_be_social_if_proximity_checked_first =
      passes_social_rule & labels$SemanticState != "social",
    is_top_proximity_state = labels$State == top_prox_state,
    label_free_top_proximity_state = top_prox_state,
    label_free_bottom_proximity_state = bottom_prox_state,
    inactive_label_pooled_states = paste(sort(inactive_states), collapse = "|"),
    inactive_label_n_pooled_states = length(inactive_states),
    inactive_label_pooled_occupancy = inactive_occ,
    inactive_label_proximity_min = if (length(inactive_px)) min(inactive_px) else NA_real_,
    inactive_label_proximity_max = if (length(inactive_px)) max(inactive_px) else NA_real_,
    inactive_label_proximity_spread = if (length(inactive_px)) max(inactive_px) - min(inactive_px) else NA_real_,
    inactive_label_includes_top_proximity_state = top_prox_state %in% inactive_states,
    inactive_label_includes_bottom_proximity_state = bottom_prox_state %in% inactive_states,
    descriptive_profile = descriptive_profile,
    proximity_interpretation = "RFID Proximity is a social-spatial co-location/co-occupancy proxy, NOT direct sociability",
    emission_z_scope = "Movement_z/Entropy_z/Proximity_z were z-scored GLOBALLY in Stage 08 via z_within_metric(); Active and Inactive phases POOLED",
    state_summary_source = summary_path
  )

  # ---------------------- degeneracy: resolution-level summary
  sep_sd <- c(Movement_z = sd(mv), Entropy_z = sd(en), Proximity_z = sd(px))
  degeneracy_rows[[paste0(res, "__summary")]] <- tibble(
    resolution = res,
    row_type = "resolution_summary",
    state_a = NA_character_,
    state_b = NA_character_,
    n_states = length(mv),
    n_distinct_Movement_z_exact = n_distinct(mv),
    n_distinct_Entropy_z_exact = n_distinct(en),
    n_distinct_Proximity_z_exact = n_distinct(px),
    n_distinct_Movement_z_tol_1e12 = n_distinct_tol(mv, 1e-12),
    n_distinct_Entropy_z_tol_1e12 = n_distinct_tol(en, 1e-12),
    n_distinct_Proximity_z_tol_1e12 = n_distinct_tol(px, 1e-12),
    n_distinct_Movement_z_tol_1e6 = n_distinct_tol(mv, 1e-6),
    n_distinct_Entropy_z_tol_1e6 = n_distinct_tol(en, 1e-6),
    n_distinct_Proximity_z_tol_1e6 = n_distinct_tol(px, 1e-6),
    n_distinct_Movement_z_tol_1e3 = n_distinct_tol(mv, 1e-3),
    n_distinct_Entropy_z_tol_1e3 = n_distinct_tol(en, 1e-3),
    n_distinct_Proximity_z_tol_1e3 = n_distinct_tol(px, 1e-3),
    n_distinct_Movement_z_tol_1e2 = n_distinct_tol(mv, 1e-2),
    n_distinct_Entropy_z_tol_1e2 = n_distinct_tol(en, 1e-2),
    n_distinct_Proximity_z_tol_1e2 = n_distinct_tol(px, 1e-2),
    sd_state_mean_Movement_z = sep_sd[["Movement_z"]],
    sd_state_mean_Entropy_z = sep_sd[["Entropy_z"]],
    sd_state_mean_Proximity_z = sep_sd[["Proximity_z"]],
    range_state_mean_Movement_z = diff(range(mv)),
    range_state_mean_Entropy_z = diff(range(en)),
    range_state_mean_Proximity_z = diff(range(px)),
    dimension_with_largest_sd = names(sep_sd)[which.max(sep_sd)],
    dimension_with_smallest_sd = names(sep_sd)[which.min(sep_sd)],
    Movement_z_values_full_precision = paste(fmt17(mv), collapse = " | "),
    Entropy_z_values_full_precision = paste(fmt17(en), collapse = " | "),
    Proximity_z_values_full_precision = paste(fmt17(px), collapse = " | "),
    abs_diff_Movement_z = NA_real_,
    abs_diff_Entropy_z = NA_real_,
    abs_diff_Proximity_z = NA_real_,
    euclidean_distance_all_dims = NA_real_,
    differs_only_in_proximity_exact = NA,
    differs_only_in_proximity_tol_1e6 = NA,
    differs_only_in_proximity_tol_1e3 = NA,
    same_semantic_label = NA,
    state_summary_source = summary_path
  )

  # ---------------------- degeneracy: pairwise
  pairs <- t(combn(seq_along(mv), 2))
  degeneracy_rows[[paste0(res, "__pairs")]] <- map_dfr(seq_len(nrow(pairs)), function(k) {
    i <- pairs[k, 1]
    j <- pairs[k, 2]
    dmv <- abs(mv[i] - mv[j])
    den <- abs(en[i] - en[j])
    dpx <- abs(px[i] - px[j])
    tibble(
      resolution = res,
      row_type = "state_pair",
      state_a = labels$State[i],
      state_b = labels$State[j],
      n_states = length(mv),
      n_distinct_Movement_z_exact = NA_integer_,
      n_distinct_Entropy_z_exact = NA_integer_,
      n_distinct_Proximity_z_exact = NA_integer_,
      n_distinct_Movement_z_tol_1e12 = NA_integer_,
      n_distinct_Entropy_z_tol_1e12 = NA_integer_,
      n_distinct_Proximity_z_tol_1e12 = NA_integer_,
      n_distinct_Movement_z_tol_1e6 = NA_integer_,
      n_distinct_Entropy_z_tol_1e6 = NA_integer_,
      n_distinct_Proximity_z_tol_1e6 = NA_integer_,
      n_distinct_Movement_z_tol_1e3 = NA_integer_,
      n_distinct_Entropy_z_tol_1e3 = NA_integer_,
      n_distinct_Proximity_z_tol_1e3 = NA_integer_,
      n_distinct_Movement_z_tol_1e2 = NA_integer_,
      n_distinct_Entropy_z_tol_1e2 = NA_integer_,
      n_distinct_Proximity_z_tol_1e2 = NA_integer_,
      sd_state_mean_Movement_z = NA_real_,
      sd_state_mean_Entropy_z = NA_real_,
      sd_state_mean_Proximity_z = NA_real_,
      range_state_mean_Movement_z = NA_real_,
      range_state_mean_Entropy_z = NA_real_,
      range_state_mean_Proximity_z = NA_real_,
      dimension_with_largest_sd = NA_character_,
      dimension_with_smallest_sd = NA_character_,
      Movement_z_values_full_precision = NA_character_,
      Entropy_z_values_full_precision = NA_character_,
      Proximity_z_values_full_precision = NA_character_,
      abs_diff_Movement_z = dmv,
      abs_diff_Entropy_z = den,
      abs_diff_Proximity_z = dpx,
      euclidean_distance_all_dims = sqrt(dmv^2 + den^2 + dpx^2),
      differs_only_in_proximity_exact = (dmv == 0) & (den == 0) & (dpx > 0),
      differs_only_in_proximity_tol_1e6 = (dmv < 1e-6) & (den < 1e-6) & (dpx >= 1e-6),
      differs_only_in_proximity_tol_1e3 = (dmv < 1e-3) & (den < 1e-3) & (dpx >= 1e-3),
      same_semantic_label = labels$SemanticState[i] == labels$SemanticState[j],
      state_summary_source = summary_path
    )
  })

  resolution_meta[[res]] <- list(labels = labels, med_mv = med_mv, med_en = med_en, q67_px = q67_px)
}

profile <- bind_rows(profile_rows)
degeneracy <- bind_rows(degeneracy_rows) %>% arrange(resolution, row_type, state_a, state_b)

# ============================================================================
# PART B: identity audit on the per-animal HMM tables + occupancy cross-check
# ============================================================================
identity_summaries <- list()
occupancy_crosscheck <- list()

for (res in RESOLUTIONS) {
  occ_path <- resolve_configured_hmm_artifact(PROJECT, res, "hmm_state_occupancy.csv")$path
  occ <- read_csv(
    occ_path,
    col_types = cols(.default = col_guess(), AnimalNum = col_character()),
    progress = FALSE
  )
  aud <- audit_hmm_identity(occ, roster_raw, paste0("audit occupancy ", res))
  assert_hmm_identity_audit(aud)
  identity_summaries[[paste0("occupancy_", res)]] <- aud$summary

  occupancy_crosscheck[[res]] <- aud$data %>%
    mutate(State = as.character(State), n = as.numeric(n)) %>%
    group_by(State) %>%
    summarise(bins_from_occupancy = sum(n, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      resolution = res,
      occupancy_share_occupancy_table = bins_from_occupancy / sum(bins_from_occupancy)
    )
}

occupancy_crosscheck <- bind_rows(occupancy_crosscheck)
profile <- profile %>%
  left_join(occupancy_crosscheck, by = c("resolution", "State")) %>%
  mutate(
    occupancy_share_abs_diff_vs_occupancy_table = abs(occupancy_share - occupancy_share_occupancy_table)
  )

# ============================================================================
# PART C: bin-level profile (is the state-mean picture an averaging artifact?)
#         + circadian composition of each state's bins
# ============================================================================
binlevel_rows <- list()

for (res in RESOLUTIONS) {
  asg_path <- resolve_configured_hmm_artifact(PROJECT, res, "hmm_state_assignments.csv")$path
  asg <- read_csv(
    asg_path,
    col_types = cols(
      .default = col_skip(),
      AnimalNum = col_character(),
      Group = col_character(),
      Sex = col_character(),
      Phase = col_character(),
      CageChange = col_character(),
      Movement_z = col_double(),
      Entropy_z = col_double(),
      Proximity_z = col_double(),
      State = col_character()
    ),
    progress = FALSE
  )
  aud <- audit_hmm_identity(asg, roster_raw, paste0("audit assignments ", res))
  assert_hmm_identity_audit(aud)
  identity_summaries[[paste0("assignments_", res)]] <- aud$summary

  bins <- aud$data %>% mutate(PhaseClass = phase_class(Phase))

  # bin-level thresholds over the WHOLE dataset at this resolution
  bin_med_mv <- median(bins$Movement_z, na.rm = TRUE)
  bin_med_en <- median(bins$Entropy_z, na.rm = TRUE)
  bin_med_px <- median(bins$Proximity_z, na.rm = TRUE)
  bin_q67_px <- unname(quantile(bins$Proximity_z, 0.67, na.rm = TRUE))
  bin_q67_mv <- unname(quantile(bins$Movement_z, 0.67, na.rm = TRUE))
  bin_q67_en <- unname(quantile(bins$Entropy_z, 0.67, na.rm = TRUE))

  bins <- bins %>%
    mutate(
      below_med_movement = Movement_z <= bin_med_mv,
      below_med_entropy = Entropy_z <= bin_med_en,
      above_q67_proximity = Proximity_z >= bin_q67_px,
      lowact_highprox = below_med_movement & above_q67_proximity,
      lowact_lowent_highprox = below_med_movement & below_med_entropy & above_q67_proximity
    )

  n_all <- nrow(bins)
  n_active_all <- sum(bins$PhaseClass == "Active")
  n_inactive_all <- sum(bins$PhaseClass == "Inactive")

  # --- point-mass / discreteness of the bin-level emission inputs.
  # Movement and Entropy are counts/derived-from-counts with a hard floor at 0,
  # so a global z-score puts a large share of bins on a single exact value.
  # NB: table()/names() round-trips doubles through as.character() and loses the
  # last ~2 significant digits, which breaks exact == tests. match()/tabulate()
  # keeps the bit-exact double.
  mode_of <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux), nbins = length(ux))
    i <- which.max(tab)
    list(value = ux[[i]], share = tab[[i]] / length(x), n_distinct = length(ux))
  }
  m_mv <- mode_of(bins$Movement_z)
  m_en <- mode_of(bins$Entropy_z)
  m_px <- mode_of(bins$Proximity_z)
  bins <- bins %>%
    mutate(
      at_modal_movement = Movement_z == m_mv$value,
      at_modal_entropy = Entropy_z == m_en$value,
      at_modal_movement_and_entropy = at_modal_movement & at_modal_entropy
    )

  lab <- resolution_meta[[res]]$labels

  add_common <- function(df) {
    df %>%
      mutate(
        resolution = res,
        bin_size_sec = BIN_SIZE_SEC[[res]],
        bin_median_Movement_z = bin_med_mv,
        bin_median_Entropy_z = bin_med_en,
        bin_median_Proximity_z = bin_med_px,
        bin_q67_Movement_z = bin_q67_mv,
        bin_q67_Entropy_z = bin_q67_en,
        bin_q67_Proximity_z = bin_q67_px,
        n_bins_dataset = n_all,
        n_bins_dataset_active = n_active_all,
        n_bins_dataset_inactive = n_inactive_all,
        dataset_modal_Movement_z = m_mv$value,
        dataset_modal_Movement_z_share = m_mv$share,
        dataset_n_distinct_binlevel_Movement_z = m_mv$n_distinct,
        dataset_modal_Entropy_z = m_en$value,
        dataset_modal_Entropy_z_share = m_en$share,
        dataset_n_distinct_binlevel_Entropy_z = m_en$n_distinct,
        dataset_modal_Proximity_z = m_px$value,
        dataset_modal_Proximity_z_share = m_px$share,
        dataset_n_distinct_binlevel_Proximity_z = m_px$n_distinct,
        assignments_source = asg_path
      )
  }

  summarise_block <- function(df) {
    df %>%
      summarise(
        n_bins = n(),
        mean_Movement_z = mean(Movement_z), sd_Movement_z = sd(Movement_z),
        mean_Entropy_z = mean(Entropy_z), sd_Entropy_z = sd(Entropy_z),
        mean_Proximity_z = mean(Proximity_z), sd_Proximity_z = sd(Proximity_z),
        median_bin_Proximity_z = median(Proximity_z),
        frac_below_med_movement = mean(below_med_movement),
        frac_below_med_entropy = mean(below_med_entropy),
        frac_above_q67_proximity = mean(above_q67_proximity),
        frac_lowact_highprox = mean(lowact_highprox),
        frac_lowact_lowent_highprox = mean(lowact_lowent_highprox),
        frac_bins_at_modal_Movement_z = mean(at_modal_movement),
        frac_bins_at_modal_Entropy_z = mean(at_modal_entropy),
        frac_bins_at_modal_Movement_and_Entropy_z = mean(at_modal_movement_and_entropy),
        n_animals = n_distinct(AnimalNum),
        .groups = "drop"
      )
  }

  per_state <- bins %>%
    group_by(State) %>%
    summarise_block() %>%
    left_join(
      bins %>%
        group_by(State) %>%
        summarise(
          n_bins_active = sum(PhaseClass == "Active"),
          n_bins_inactive = sum(PhaseClass == "Inactive"),
          .groups = "drop"
        ),
      by = "State"
    ) %>%
    mutate(
      row_type = "state_overall",
      PhaseClass = NA_character_,
      frac_bins_active = n_bins_active / n_bins,
      frac_bins_inactive = n_bins_inactive / n_bins,
      share_of_all_active_bins = n_bins_active / n_active_all,
      share_of_all_inactive_bins = n_bins_inactive / n_inactive_all,
      occupancy_share_binlevel = n_bins / n_all
    ) %>%
    left_join(
      lab %>% transmute(
        State, SemanticState,
        state_mean_Movement_z = Movement_z,
        state_mean_Entropy_z = Entropy_z,
        state_mean_Proximity_z = Proximity_z
      ),
      by = "State"
    ) %>%
    mutate(
      binlevel_vs_statemean_abs_diff_Movement_z = abs(mean_Movement_z - state_mean_Movement_z),
      binlevel_vs_statemean_abs_diff_Entropy_z = abs(mean_Entropy_z - state_mean_Entropy_z),
      binlevel_vs_statemean_abs_diff_Proximity_z = abs(mean_Proximity_z - state_mean_Proximity_z)
    ) %>%
    add_common()

  per_state_phase <- bins %>%
    group_by(State, PhaseClass) %>%
    summarise_block() %>%
    mutate(row_type = "state_x_phase", occupancy_share_binlevel = n_bins / n_all) %>%
    left_join(lab %>% transmute(State, SemanticState), by = "State") %>%
    add_common()

  phase_marginal <- bins %>%
    group_by(PhaseClass) %>%
    summarise_block() %>%
    mutate(
      row_type = "phase_marginal", State = NA_character_, SemanticState = NA_character_,
      occupancy_share_binlevel = n_bins / n_all
    ) %>%
    add_common()

  dataset_marginal <- bins %>%
    summarise_block() %>%
    mutate(
      row_type = "dataset_marginal", State = NA_character_, PhaseClass = NA_character_,
      SemanticState = NA_character_, occupancy_share_binlevel = 1
    ) %>%
    add_common()

  binlevel_rows[[res]] <- bind_rows(per_state, per_state_phase, phase_marginal, dataset_marginal)
}

binlevel <- bind_rows(binlevel_rows) %>%
  relocate(resolution, row_type, State, PhaseClass, SemanticState)

# the bin-level point-mass structure is a degeneracy fact, so attach it to the
# resolution_summary rows of the degeneracy audit as well
degeneracy <- degeneracy %>%
  left_join(
    binlevel %>%
      filter(row_type == "dataset_marginal") %>%
      transmute(
        resolution,
        binlevel_modal_Movement_z = dataset_modal_Movement_z,
        binlevel_modal_Movement_z_share = dataset_modal_Movement_z_share,
        binlevel_n_distinct_Movement_z = dataset_n_distinct_binlevel_Movement_z,
        binlevel_modal_Entropy_z = dataset_modal_Entropy_z,
        binlevel_modal_Entropy_z_share = dataset_modal_Entropy_z_share,
        binlevel_n_distinct_Entropy_z = dataset_n_distinct_binlevel_Entropy_z,
        binlevel_modal_Proximity_z = dataset_modal_Proximity_z,
        binlevel_modal_Proximity_z_share = dataset_modal_Proximity_z_share,
        binlevel_n_distinct_Proximity_z = dataset_n_distinct_binlevel_Proximity_z,
        binlevel_frac_at_modal_Movement_and_Entropy = frac_bins_at_modal_Movement_and_Entropy_z
      ),
    by = "resolution"
  ) %>%
  mutate(across(starts_with("binlevel_"), ~ if_else(row_type == "resolution_summary", .x, .x[NA_integer_])))

# add the circadian composition of each state to the primary profile too
profile <- profile %>%
  left_join(
    binlevel %>%
      filter(row_type == "state_overall") %>%
      transmute(
        resolution, State,
        binlevel_frac_bins_active = frac_bins_active,
        binlevel_frac_bins_inactive = frac_bins_inactive,
        binlevel_frac_lowact_highprox = frac_lowact_highprox,
        binlevel_mean_Movement_z = mean_Movement_z,
        binlevel_sd_Movement_z = sd_Movement_z,
        binlevel_mean_Entropy_z = mean_Entropy_z,
        binlevel_sd_Entropy_z = sd_Entropy_z,
        binlevel_mean_Proximity_z = mean_Proximity_z,
        binlevel_sd_Proximity_z = sd_Proximity_z
      ),
    by = c("resolution", "State")
  )

# ============================================================================
# WRITE
# ============================================================================
p1 <- file.path(AUDIT_OUT, "hmm_state_multidimensional_profile_audit.csv")
p2 <- file.path(AUDIT_OUT, "hmm_state_emission_degeneracy_audit.csv")
p3 <- file.path(AUDIT_OUT, "hmm_state_binlevel_profile_audit.csv")
write_csv(profile, p1, na = "NA")
write_csv(degeneracy, p2, na = "NA")
write_csv(binlevel, p3, na = "NA")

# ============================================================================
# CONSOLE REPORT
# ============================================================================
cat("\n================ IDENTITY AUDITS (all must show passed = TRUE) ================\n")
print(as.data.frame(bind_rows(identity_summaries)))

cat("\n================ CLASSIFIER RE-DERIVATION CONSISTENCY ================\n")
cat("all rule_that_fired labels match shipped SemanticState: ",
    all(profile$rule_label_consistent), "\n")

cat("\n================ STATE-LEVEL PROFILE ================\n")
print(as.data.frame(profile %>% select(
  resolution, State, n_bins, occupancy_share, Movement_z, Entropy_z, Proximity_z,
  movement_rank, entropy_rank, proximity_rank, movement_pct, entropy_pct, proximity_pct,
  SemanticState, rule_that_fired,
  passes_inactive_rule, passes_social_rule, flag_low_activity_high_proximity,
  would_be_social_if_proximity_checked_first, is_top_proximity_state, descriptive_profile
)), digits = 6)

cat("\n================ THRESHOLDS ================\n")
print(as.data.frame(profile %>% distinct(
  resolution, median_Movement_z, median_Entropy_z, median_Proximity_z,
  q67_Proximity_z, q67_Movement_z, q67_Entropy_z
)), digits = 10)

cat("\n================ INACTIVE-LABEL POOLING ================\n")
print(as.data.frame(profile %>% distinct(
  resolution, inactive_label_pooled_states, inactive_label_n_pooled_states,
  inactive_label_pooled_occupancy, inactive_label_proximity_min,
  inactive_label_proximity_max, inactive_label_proximity_spread,
  inactive_label_includes_top_proximity_state, inactive_label_includes_bottom_proximity_state,
  label_free_top_proximity_state
)), digits = 6)

cat("\n================ OCCUPANCY CROSS-CHECK (state_summary vs occupancy table) ================\n")
print(as.data.frame(profile %>% select(
  resolution, State, occupancy_share, occupancy_share_occupancy_table,
  occupancy_share_abs_diff_vs_occupancy_table
)), digits = 10)
cat("max abs diff = ", max(profile$occupancy_share_abs_diff_vs_occupancy_table, na.rm = TRUE), "\n")

cat("\n================ EMISSION DEGENERACY (resolution summary) ================\n")
print(as.data.frame(degeneracy %>% filter(row_type == "resolution_summary") %>%
  select(resolution, n_distinct_Movement_z_exact, n_distinct_Entropy_z_exact,
         n_distinct_Proximity_z_exact, n_distinct_Movement_z_tol_1e6,
         n_distinct_Entropy_z_tol_1e6, n_distinct_Movement_z_tol_1e3,
         n_distinct_Entropy_z_tol_1e3, n_distinct_Proximity_z_tol_1e3,
         sd_state_mean_Movement_z, sd_state_mean_Entropy_z, sd_state_mean_Proximity_z,
         range_state_mean_Movement_z, range_state_mean_Entropy_z, range_state_mean_Proximity_z,
         dimension_with_largest_sd, dimension_with_smallest_sd)), digits = 6)

cat("\n---- bin-level point-mass structure of the emission inputs ----\n")
print(as.data.frame(degeneracy %>% filter(row_type == "resolution_summary") %>%
  select(resolution, binlevel_modal_Movement_z, binlevel_modal_Movement_z_share,
         binlevel_n_distinct_Movement_z, binlevel_modal_Entropy_z,
         binlevel_modal_Entropy_z_share, binlevel_n_distinct_Entropy_z,
         binlevel_modal_Proximity_z, binlevel_modal_Proximity_z_share,
         binlevel_n_distinct_Proximity_z, binlevel_frac_at_modal_Movement_and_Entropy)),
  digits = 8)

cat("\n---- full-precision state means ----\n")
print(as.data.frame(degeneracy %>% filter(row_type == "resolution_summary") %>%
  select(resolution, Movement_z_values_full_precision, Entropy_z_values_full_precision,
         Proximity_z_values_full_precision)))

cat("\n================ EMISSION DEGENERACY (pairwise) ================\n")
print(as.data.frame(degeneracy %>% filter(row_type == "state_pair") %>%
  select(resolution, state_a, state_b, abs_diff_Movement_z, abs_diff_Entropy_z,
         abs_diff_Proximity_z, euclidean_distance_all_dims,
         differs_only_in_proximity_exact, differs_only_in_proximity_tol_1e6,
         differs_only_in_proximity_tol_1e3, same_semantic_label)), digits = 6)

cat("\n================ BIN-LEVEL: state_overall ================\n")
print(as.data.frame(binlevel %>% filter(row_type == "state_overall") %>%
  select(resolution, State, SemanticState, n_bins, occupancy_share_binlevel,
         mean_Movement_z, sd_Movement_z, mean_Entropy_z, sd_Entropy_z,
         mean_Proximity_z, sd_Proximity_z, frac_above_q67_proximity, frac_lowact_highprox,
         frac_lowact_lowent_highprox, frac_bins_active, frac_bins_inactive,
         share_of_all_active_bins, share_of_all_inactive_bins,
         binlevel_vs_statemean_abs_diff_Movement_z,
         binlevel_vs_statemean_abs_diff_Entropy_z,
         binlevel_vs_statemean_abs_diff_Proximity_z,
         frac_bins_at_modal_Movement_z, frac_bins_at_modal_Entropy_z,
         frac_bins_at_modal_Movement_and_Entropy_z)), digits = 6)

cat("\n================ BIN-LEVEL: thresholds, phase and dataset marginals ================\n")
print(as.data.frame(binlevel %>% filter(row_type %in% c("phase_marginal", "dataset_marginal")) %>%
  select(resolution, row_type, PhaseClass, n_bins, mean_Movement_z, mean_Entropy_z,
         mean_Proximity_z, frac_below_med_movement, frac_above_q67_proximity,
         frac_lowact_highprox, bin_median_Movement_z, bin_median_Entropy_z,
         bin_q67_Proximity_z)), digits = 6)

cat("\n================ BIN-LEVEL: state x phase ================\n")
print(as.data.frame(binlevel %>% filter(row_type == "state_x_phase") %>%
  select(resolution, State, SemanticState, PhaseClass, n_bins, mean_Movement_z,
         mean_Entropy_z, mean_Proximity_z, frac_lowact_highprox)), digits = 6)

cat("\n================ OUTPUTS ================\n")
cat(p1, " rows=", nrow(profile), " cols=", ncol(profile), "\n", sep = "")
cat(p2, " rows=", nrow(degeneracy), " cols=", ncol(degeneracy), "\n", sep = "")
cat(p3, " rows=", nrow(binlevel), " cols=", ncol(binlevel), "\n", sep = "")
