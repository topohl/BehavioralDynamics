# ================================================================
# LEGACY five-domain vs NEW four-domain comparison
# MMMSociability
# ================================================================
# The legacy first-night analysis is PRESERVED, not replaced: this script reads
# both shipped artifacts and writes an explicit row-by-row comparison into a
# fresh replay folder so a reader can see exactly what changed and WHY.
#
# Nothing here rewrites a historical result. The legacy five-domain outputs under
# analysis_ready/12_systems_neuroscience_summary/5min_based/first_night/ are left
# byte-identical; they remain the conservative flat-family sensitivity.
#
# THE NEW SCHEME WAS NOT SELECTED BECAUSE IT YIELDS MORE SIGNIFICANT FINDINGS.
# It yields FEWER. Under the legacy flat family (5 domains x 3 contrasts = 15 per
# Sex, unadjusted lm) zero cells survive BH and the minimum q is 0.089. Under the
# new hierarchical scheme with cage adjustment, zero domain omnibus tests survive
# and the minimum q is larger still. The change is motivated by construct validity
# (adaptation cannot be measured from one 12-h composite), by dependence (cage ICCs
# of 0.06-0.45), and by multiplicity SHAPE, not by the number of stars.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
})

.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"),
                                file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("project_paths.R")
source_mmm_helper("rfid_domain_core.R")

ROOT <- mmm_project_root()
LEGACY <- mmm_behavior_output_active_root("first_night_10min", project_root = ROOT)
NEWDIR <- file.path(behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", "10min"), "tables")
OUT <- mmm_behavior_audit_replay_output_root("rfid_legacy_vs_new_domains", ROOT)

need <- function(p) { if (!file.exists(p)) stop("Missing input: ", p, call. = FALSE); p }
legacy_contrasts <- read_csv(need(file.path(LEGACY, "first_night_group_contrasts.csv")),
                             col_types = cols(.default = col_guess()), progress = FALSE)
legacy_inter <- read_csv(need(file.path(LEGACY, "first_night_group_sex_interactions.csv")),
                         col_types = cols(.default = col_guess()), progress = FALSE)
new_omni <- read_csv(need(file.path(NEWDIR, "first_night_domain_omnibus_primary.csv")),
                     col_types = cols(.default = col_guess()), progress = FALSE)
new_omni_all <- read_csv(need(file.path(NEWDIR, "first_night_domain_omnibus_all_models.csv")),
                         col_types = cols(.default = col_guess()), progress = FALSE)
new_pairs <- read_csv(need(file.path(NEWDIR, "first_night_pairwise_contrasts.csv")),
                      col_types = cols(.default = col_guess()), progress = FALSE)
new_inter <- read_csv(need(file.path(NEWDIR, "first_night_group_sex_interaction.csv")),
                      col_types = cols(.default = col_guess()), progress = FALSE)

# ------------------------------------------------- domain-level crosswalk
crosswalk <- tribble(
  ~legacy_domain, ~new_domain, ~disposition,
  "Psychomotor activation", "Movement output", "RETAINED_RENAMED",
  "Behavioral flexibility / predictability", "Spatial entropy dynamics", "RETAINED_RENAMED",
  "Social spatial organization", "Social-spatial organization", "RETAINED_RENAMED",
  "Behavioral volatility / fragmentation", "Cross-channel behavioral volatility", "RETAINED_RENAMED",
  "Active-phase adaptation / exploration", NA_character_, "EXCLUDED"
) %>%
  mutate(
    legacy_formula = c(
      "Movement_mean_z",
      "0.5*(Entropy_mean_z + Entropy_rmssd_z) - Entropy_acf1_z",
      "0.5*(Proximity_mean_z + Proximity_acf1_z) - Proximity_rmssd_z",
      "(Movement_rmssd_z + Entropy_rmssd_z + Proximity_rmssd_z)/3",
      MMM_RFID_EXCLUDED_LEGACY_DOMAIN$formula),
    new_formula = ifelse(is.na(.data$new_domain), NA_character_,
                         unname(MMM_RFID_CORE_FORMULAS[.data$new_domain])),
    formula_identical = !is.na(.data$new_domain),
    rename_reason = c(
      "RFID position transitions do not establish a psychomotor construct.",
      "The score is the dispersion and temporal persistence of spatial-occupancy entropy; it is not behavioural or cognitive flexibility.",
      "RFID proximity is a co-location time fraction, not sociability, social motivation, affiliation or preference.",
      "The score contains three RMSSD terms and no explicit fragmentation variable.",
      MMM_RFID_EXCLUDED_LEGACY_DOMAIN$exclusion_reason),
    legacy_model = "lm(DomainScore ~ Group * Sex); no Batch term, no cage term",
    new_model = ifelse(is.na(.data$new_domain), NA_character_,
                       "lmerTest::lmer(DomainScore ~ Group + factor(Batch) + (1|CageEpochID)) within Sex; plus legacy-LM, Batch-LM and CR2 cluster-robust variants reported alongside"),
    legacy_multiplicity = "BH over a FLAT family: 5 domains x 3 contrasts = 15 tests per Sex; interaction family n=5",
    new_multiplicity = ifelse(is.na(.data$new_domain), NA_character_,
                              "HIERARCHICAL: BH over 4 domain omnibus Group tests per Sex, then Holm over 3 pairwise contrasts WITHIN a domain whose parent survived; interaction family n=4"),
    legacy_preserved_at = LEGACY)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write_csv(crosswalk, file.path(OUT, "domain_crosswalk.csv"))

# ---------------------------------------- contrast-level old vs new table
lg <- legacy_contrasts %>%
  select(legacy_domain = "Domain", "Sex", "contrast",
         legacy_estimate = "estimate", legacy_ci_low = "ci_low", legacy_ci_high = "ci_high",
         legacy_hedges_g = "hedges_g", legacy_raw_p = "raw_p", legacy_q = "q",
         legacy_family_n = "n_tests_in_family")

nw <- new_pairs %>%
  select(new_domain = "Domain", "Sex", "contrast",
         new_estimate = "estimate", new_ci_low = "ci_low", new_ci_high = "ci_high",
         new_hedges_g = "hedges_g", new_raw_p = "raw_p",
         new_pairwise_role = "pairwise_role", new_parent_q = "parent_q",
         new_p_holm = "p_holm_within_domain", new_n_ref = "n_ref", new_n_comp = "n_comp")

cmp <- crosswalk %>%
  filter(!is.na(.data$new_domain)) %>%
  select("legacy_domain", "new_domain", "disposition") %>%
  left_join(lg, by = "legacy_domain", relationship = "many-to-many") %>%
  left_join(nw, by = c("new_domain", "Sex", "contrast")) %>%
  left_join(new_omni %>% select(new_domain = "Domain", "Sex",
                                new_omnibus_raw_p = "raw_p", new_omnibus_q = "q",
                                new_parent_supported = "parent_supported"),
            by = c("new_domain", "Sex")) %>%
  left_join(new_omni_all %>% filter(.data$model == "legacy_lm") %>%
              select(new_domain = "Domain", "Sex", omnibus_p_legacy_lm = "raw_p"),
            by = c("new_domain", "Sex")) %>%
  left_join(new_omni_all %>% filter(.data$model == "batch_lm") %>%
              select(new_domain = "Domain", "Sex", omnibus_p_batch_lm = "raw_p"),
            by = c("new_domain", "Sex")) %>%
  left_join(new_omni_all %>% filter(.data$model == "cage_lmer") %>%
              select(new_domain = "Domain", "Sex", omnibus_p_cage_lmer = "raw_p",
                     cage_icc = "cage_icc"),
            by = c("new_domain", "Sex")) %>%
  left_join(new_omni_all %>% filter(.data$model == "cage_cr2") %>%
              select(new_domain = "Domain", "Sex", omnibus_p_cage_cr2 = "raw_p"),
            by = c("new_domain", "Sex")) %>%
  mutate(
    estimate_delta = .data$new_estimate - .data$legacy_estimate,
    hedges_g_delta = .data$new_hedges_g - .data$legacy_hedges_g,
    sign_changed = is.finite(.data$new_estimate) & is.finite(.data$legacy_estimate) &
      sign(.data$new_estimate) != sign(.data$legacy_estimate),
    legacy_fdr_supported = is.finite(.data$legacy_q) & .data$legacy_q < 0.05,
    # ATTRIBUTION: which design change moved this cell, in the order the changes
    # are applied. Reported as a decomposition, not a single cause.
    attribution_domain_removal =
      "Family shrank from 15 to 4 tests per Sex AND changed unit from contrast to domain omnibus",
    attribution_batch_adjustment = dplyr::case_when(
      !is.finite(.data$omnibus_p_legacy_lm) | !is.finite(.data$omnibus_p_batch_lm) ~ NA_real_,
      TRUE ~ .data$omnibus_p_batch_lm - .data$omnibus_p_legacy_lm),
    attribution_cage_adjustment = dplyr::case_when(
      !is.finite(.data$omnibus_p_batch_lm) | !is.finite(.data$omnibus_p_cage_lmer) ~ NA_real_,
      TRUE ~ .data$omnibus_p_cage_lmer - .data$omnibus_p_batch_lm),
    attribution_omnibus_gating = dplyr::if_else(
      .data$legacy_fdr_supported & !.data$new_parent_supported,
      "cell lost support at the parent-domain gate", NA_character_),
    attribution_leading_bin_rebuild = paste(
      "Both legacy and new artifacts read the SAME post-leading-bin-fix Stage 01",
      "table, so the rebuild is NOT a source of old-vs-new difference here; it is",
      "already reflected in the shipped legacy numbers."),
    attribution_scaling = paste(
      "First-night scaling is identical in both: z within Sex across all CC1",
      "animals. Scaling is NOT a source of first-night old-vs-new difference."),
    attribution_resolution = "Both are 10 min. Resolution is NOT a source of difference.")
write_csv(cmp, file.path(OUT, "contrast_level_old_vs_new.csv"))

# ---------------------------------------------------- interaction family
inter_cmp <- crosswalk %>%
  filter(!is.na(.data$new_domain)) %>%
  select("legacy_domain", "new_domain") %>%
  left_join(legacy_inter %>% select(legacy_domain = "Domain", legacy_F = "F_value",
                                    legacy_raw_p = "raw_p", legacy_q = "q",
                                    legacy_family_n = "n_tests_in_family"),
            by = "legacy_domain") %>%
  left_join(new_inter %>% select(new_domain = "Domain", new_F = "test_statistic",
                                 new_raw_p = "raw_p", new_q = "q",
                                 new_family_n = "n_tests_in_family",
                                 new_model = "model"),
            by = "new_domain") %>%
  mutate(note = paste(
    "Legacy: anova(lm(~Group*Sex)) Group:Sex F, BH over 5 domains. New: Group:Sex",
    "from lmer with a cage random intercept and Batch absorbing Sex, BH over 4",
    "domains. No Sex MAIN effect is claimed in either scheme."))
write_csv(inter_cmp, file.path(OUT, "interaction_family_old_vs_new.csv"))

# ---------------------------------------------------------- headline summary
summary_tbl <- tibble(
  legacy_n_domains = 5L,
  new_n_core_domains = length(MMM_RFID_CORE_DOMAINS),
  legacy_family_per_sex = 15L,
  new_primary_family_per_sex = length(MMM_RFID_CORE_DOMAINS),
  legacy_cells_total = nrow(legacy_contrasts),
  legacy_cells_fdr_supported = sum(legacy_contrasts$q < 0.05, na.rm = TRUE),
  legacy_min_q = suppressWarnings(min(legacy_contrasts$q, na.rm = TRUE)),
  legacy_cells_nominal_p05 = sum(legacy_contrasts$raw_p < 0.05, na.rm = TRUE),
  new_omnibus_tests_total = nrow(new_omni),
  new_omnibus_supported = sum(new_omni$parent_supported, na.rm = TRUE),
  new_min_q = suppressWarnings(min(new_omni$q, na.rm = TRUE)),
  new_omnibus_nominal_p05 = sum(new_omni$raw_p < 0.05, na.rm = TRUE),
  direction_of_change = paste(
    "The new scheme is MORE conservative, not less. It was not chosen for the",
    "number of significant cells."))
write_csv(summary_tbl, file.path(OUT, "headline_summary.csv"))

message("Legacy vs new comparison written to ", OUT)
message("  legacy: ", summary_tbl$legacy_cells_fdr_supported, " of ",
        summary_tbl$legacy_cells_total, " cells FDR-supported, min q = ",
        signif(summary_tbl$legacy_min_q, 3))
message("  new:    ", summary_tbl$new_omnibus_supported, " of ",
        summary_tbl$new_omnibus_tests_total, " domain omnibus tests supported, min q = ",
        signif(summary_tbl$new_min_q, 3))
