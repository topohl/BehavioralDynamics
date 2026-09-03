# ==============================================================================
# AUDIT (read-only): documentation/code provenance of the Stage 14
# "Behavioral state architecture" heatmap value.
#
# This script does NOT modify any pipeline code. It only re-derives the numbers
# needed to check the documented description against the actual mathematics.
#
# Verifies:
#   V1  state-level Movement_z / Entropy_z / Proximity_z + semantic labels at
#       both resolutions (independent re-derivation of the orchestrator baseline)
#   V2  social_state_fraction is identically zero -> composite reduces to
#       0.5 * z(occupancy_entropy) - z(inactive_state_fraction)
#   V3  the composite is EXACTLY permutation-invariant to the within-epoch
#       ordering of the Viterbi state sequence
#   V4  what the Sex main effect and factor(CageChangeIndex) fixed effects mean
#       once the score is z-scored inside Sex x PhaseClass x CageChangeIndex
#   V5  group means within each Sex x Phase x CC cell sum (n-weighted) to zero
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})

repo <- "C:/Users/topohl/Documents/GitHub/MMMSociability"
setwd(repo)
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

project <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
audit_out <- file.path(project, "analysis_ready/12_systems_neuroscience_summary/5min_based",
                       "audit_hmm_state_architecture")
dir.create(audit_out, recursive = TRUE, showWarnings = FALSE)

resolutions <- c("5min_based", "10min_based")
hmm_tab <- function(res, f) {
  file.path(project, "analysis_ready/06_behavioral_dynamics/hmm_states", res, "tables", f)
}

# ---- canonical roster exactly as Stage 08 does -------------------------------
roster_raw <- read_csv(
  file.path(project, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
  col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                   Group = col_character(), Sex = col_character()),
  progress = FALSE
)
roster <- build_canonical_identity_roster(roster_raw, "Stage 01 5min_based roster")
cat("roster animals:", nrow(roster), "\n")

# ---- V1: state summary + semantic labels ------------------------------------
state_summaries <- map(set_names(resolutions),
                       ~ read_csv(hmm_tab(.x, "hmm_state_summary.csv"), show_col_types = FALSE))
labels <- imap(state_summaries, ~ annotate_hmm_semantic_states(.x, .y))

v1 <- imap_dfr(labels, function(lab, res) {
  ss <- state_summaries[[res]]
  lab %>%
    left_join(ss %>% transmute(State = as.character(State), n_bins), by = "State") %>%
    mutate(
      occupancy = n_bins / sum(n_bins),
      median_movement_z = median(Movement_z), median_entropy_z = median(Entropy_z),
      q67_proximity_z   = quantile(Proximity_z, 0.67),
      proximity_rank    = rank(-Proximity_z),
      is_top_proximity_state = Proximity_z == max(Proximity_z)
    )
})
write_csv(v1, file.path(audit_out, "audit_doc_v1_state_semantics.csv"))
print(v1 %>% select(resolution, State, Movement_z, Entropy_z, Proximity_z, occupancy,
                    SemanticState, proximity_rank, is_top_proximity_state), n = 20)

# ---- V2/V3: epoch scores, reduction, permutation invariance ------------------
occupancy_raw <- map(set_names(resolutions),
                     ~ read_csv(hmm_tab(.x, "hmm_state_occupancy.csv"),
                                col_types = cols(AnimalNum = col_character())))
assignments <- map(set_names(resolutions),
                   ~ read_csv(hmm_tab(.x, "hmm_state_assignments.csv"),
                              col_types = cols(AnimalNum = col_character())))

# fail-closed identity audit on every table we touch
walk2(occupancy_raw, names(occupancy_raw), function(d, r) {
  assert_hmm_identity_audit(audit_hmm_identity(d, roster, paste("occupancy", r)))
})
walk2(assignments, names(assignments), function(d, r) {
  assert_hmm_identity_audit(audit_hmm_identity(d, roster, paste("assignments", r)))
})

epoch <- imap(occupancy_raw, ~ build_hmm_epoch_scores(.x, labels[[.y]], roster, .y))

v2 <- map_dfr(epoch, "component_audit")
write_csv(v2, file.path(audit_out, "audit_doc_v2_component_reduction.csv"))
print(v2)

# permutation invariance: shuffle State within each animal x CC x Phase sequence,
# rebuild occupancy from the shuffled labels, re-score, compare.
set.seed(20260902)
rebuild_occ <- function(d) {
  d %>%
    count(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum, State) %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum) %>%
    mutate(frac_time = n / sum(n)) %>%
    ungroup()
}
perm_check <- imap_dfr(assignments, function(asg, res) {
  asg <- audit_hmm_identity(asg, roster, paste("assignments", res))$data
  shuffled <- asg %>%
    group_by(AnimalNum, CageChange, Phase) %>%
    mutate(State = sample(State)) %>%
    ungroup()
  orig  <- build_hmm_epoch_scores(rebuild_occ(asg), labels[[res]], roster, res)$scores
  shuff <- build_hmm_epoch_scores(rebuild_occ(shuffled), labels[[res]], roster, res)$scores
  cmp <- orig %>%
    select(AnimalNum, CageChange, PhaseClass, orig = `Behavioral state architecture`) %>%
    inner_join(shuff %>% select(AnimalNum, CageChange, PhaseClass,
                                shuf = `Behavioral state architecture`),
               by = c("AnimalNum", "CageChange", "PhaseClass"))
  tibble(
    resolution = res,
    n_epochs_compared = nrow(cmp),
    max_abs_difference = max(abs(cmp$orig - cmp$shuf)),
    identical_to_1e15 = max(abs(cmp$orig - cmp$shuf)) < 1e-15,
    pearson_r = cor(cmp$orig, cmp$shuf)
  )
})
write_csv(perm_check, file.path(audit_out, "audit_doc_v3_permutation_invariance.csv"))
print(perm_check)

# also confirm the reduced closed form reproduces the shipped composite exactly
reduction <- imap_dfr(epoch, function(e, res) {
  s <- e$scores
  reduced <- 0.5 * s$state_occupancy_entropy_z - s$inactive_state_fraction_z
  tibble(resolution = res,
         n = nrow(s),
         max_abs_diff_composite_vs_reduced = max(abs(s[["Behavioral state architecture"]] - reduced)),
         max_abs_social_z = max(abs(s$social_state_fraction_z)))
})
write_csv(reduction, file.path(audit_out, "audit_doc_v3b_closed_form_reduction.csv"))
print(reduction)

# ---- V4/V5: standardization side effects on the inference model --------------
group_levels <- c("CON", "RES", "SUS")
sex_levels <- c("Female", "Male")
domain_dat <- imap_dfr(epoch, function(e, res) {
  e$scores %>%
    transmute(resolution = res, AnimalNum, Group, Sex, CageChange, CageChangeIndex,
              PhaseClass, Domain = "Behavioral state architecture",
              DomainScore = .data[["Behavioral state architecture"]]) %>%
    filter(is.finite(DomainScore))
})

# V5: cell means must be ~0 by construction
cellmeans <- domain_dat %>%
  group_by(resolution, Sex, PhaseClass, CageChangeIndex) %>%
  summarise(n_epochs = n(), cell_mean = mean(DomainScore), cell_sd = sd(DomainScore),
            .groups = "drop")
write_csv(cellmeans, file.path(audit_out, "audit_doc_v5_context_cell_moments.csv"))
cat("max |cell mean| =", max(abs(cellmeans$cell_mean)), "\n")
cat("cell sd range   =", paste(range(cellmeans$cell_sd), collapse = " .. "), "\n")

group_cell <- domain_dat %>%
  group_by(resolution, Sex, PhaseClass, CageChangeIndex, Group) %>%
  summarise(n = n(), m = mean(DomainScore), .groups = "drop") %>%
  group_by(resolution, Sex, PhaseClass, CageChangeIndex) %>%
  summarise(n_weighted_sum_of_group_means = sum(n * m) / sum(n), .groups = "drop")
write_csv(group_cell, file.path(audit_out, "audit_doc_v5b_group_mean_zero_sum.csv"))
cat("max |n-weighted sum of group means| =",
    max(abs(group_cell$n_weighted_sum_of_group_means)), "\n")

# V4: fit the EXACT heatmap model formula and inspect the Sex / CC coefficients
fixef_tbl <- map_dfr(resolutions, function(res) {
  map_dfr(c("Active", "Inactive"), function(ph) {
    md <- domain_dat %>%
      filter(resolution == res, PhaseClass == ph) %>%
      transmute(AnimalNum = factor(AnimalNum),
                Group = factor(Group, levels = group_levels),
                Sex = factor(Sex, levels = sex_levels),
                CageChangeIndex = factor(CageChangeIndex),
                DomainScore = as.numeric(DomainScore))
    fit <- lmerTest::lmer(DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum),
                          data = md)
    as.data.frame(summary(fit)$coefficients) %>%
      rownames_to_column("term") %>%
      as_tibble() %>%
      mutate(resolution = res, PhaseClass = ph, .before = 1)
  })
})
write_csv(fixef_tbl, file.path(audit_out, "audit_doc_v4_model_fixed_effects.csv"))
print(fixef_tbl %>%
        filter(str_detect(term, "Sex|CageChange")) %>%
        select(resolution, PhaseClass, term, Estimate, `Std. Error`, `Pr(>|t|)`), n = 40)

# cross-check the group contrasts against the shipped helper (no re-implementation)
helper_contrasts <- map_dfr(resolutions, function(res) {
  map_dfr(c("Active", "Inactive"), function(ph) {
    fit_repeated_measures_domain_contrasts(
      domain_dat %>% filter(resolution == res),
      "Behavioral state architecture", ph
    )$contrasts %>% mutate(resolution = res, .before = 1)
  })
})
write_csv(helper_contrasts, file.path(audit_out, "audit_doc_v4b_helper_contrasts.csv"))
print(helper_contrasts %>%
        filter(Sex == "Female", PhaseClass == "Active") %>%
        select(resolution, contrast, n_ref_animals, n_comp_animals,
               animal_level_hedges_g, mixed_model_estimate, mixed_model_SE, mixed_model_p))

cat("\nAUDIT (documentation provenance) COMPLETE\n")
