# ================================================================
# Leading-bin SEED sensitivity for the four core raw-RFID domains
# MMMSociability
# ================================================================
# WHY THIS AUDIT EXISTS
#
# The 2026-09-22 leading-bin fix (commit 0685366) recovers each animal's last
# pre-window position from raw_data and injects it as a synthetic row at the
# session's first timestamp, so the carry-forward state exists and the leading
# bins of the first night produce metric rows instead of being absent. That is why
# window coverage is now 111/111 animals x 72/72 slots with zero gaps.
#
# The seed is an ASSUMPTION, not an observation, and it is not neutral for every
# channel:
#   * A seeded bin in which the animal never triggers a read contains exactly ONE
#     position, so Entropy = 0 EXACTLY - the floor of the scale - and Movement = 0.
#   * Seeded leading bins therefore push affected animals toward minimum entropy at
#     the start of the window, which can depress Entropy_mean and inflate
#     Entropy_rmssd at the seed/first-read boundary.
#   * Analysis/01 computes SeedReadTime but DROPS it (line ~630), so how STALE a
#     seed is - how long before the window it was recorded - is not recoverable
#     from the shipped table, and seed_rows_for_file() puts no lower bound on it.
#
# This audit measures how much that matters for the four core domains, by
# recomputing every domain after TRIMMING the leading run of zero-entropy bins and
# comparing. It is a robustness check, not a correction: the primary analysis
# continues to use the full 12 h window.
#
# It is PHENOTYPE-BLIND for the construct part and reports the group-level
# consequence separately.
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
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")
source_mmm_helper("rfid_domain_core.R")
source_mmm_helper("rfid_domain_scaling.R")
source_mmm_helper("rfid_acute_window_helpers.R")
source_mmm_helper("rfid_domain_inference.R")

ROOT <- mmm_project_root()
BIN <- "10min_based"; BS <- 600
OUT <- mmm_behavior_output_active_root("rfid_leading_bin_seed_audit", project_root = ROOT)
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
w <- function(x, nm) { write_csv(x, file.path(OUT, nm)); invisible(nm) }

f <- file.path(mmm_derived_metrics_output_root(ROOT), BIN, "all_behavior_metrics.csv")
if (!file.exists(f)) stop("Missing Stage 01 input: ", f, call. = FALSE)
dat <- read_csv(f, col_types = cols(AnimalNum = col_character(),
                                    BinStart = col_datetime(), .default = col_guess()),
                progress = FALSE)
dat$AnimalNum <- canonical_animal_id(dat$AnimalNum)

# ALL FOUR cage changes. The leading-bin burden is NOT equal across cage changes
# (CC1 carries roughly twice the burden of CC2), so auditing CC1 alone cannot tell
# you whether a CC-to-CC comparison is contaminated. That is exactly the comparison
# the longitudinal change scores make.
sel_all <- mmm_rfid_select_all_acute_windows(dat, bin_size_sec = BS)
sel <- sel_all[sel_all$CageChangeLabel == "CC1", ]

# ------------------------------------------- 1. how many leading bins look seeded
lead <- sel %>%
  arrange(.data$AnimalNum, .data$target_slot) %>%
  group_by(.data$AnimalNum, .data$Group, .data$Sex, .data$Batch, .data$System) %>%
  summarise(
    n_slots = dplyr::n(),
    first_nonzero_entropy_slot = { i <- which(.data$Entropy > 0); if (length(i)) min(i) else NA_integer_ },
    n_leading_zero_entropy = { i <- which(.data$Entropy > 0)
      if (length(i)) min(i) - 1L else dplyr::n() },
    n_zero_entropy_total = sum(.data$Entropy == 0, na.rm = TRUE),
    slot1_single_position = dplyr::first(.data$n_positions_visited) == 1L,
    slot1_observation_seconds = dplyr::first(.data$observation_seconds),
    .groups = "drop") %>%
  mutate(leading_seed_suspected = .data$n_leading_zero_entropy >= 1L)
w(lead, "leading_zero_entropy_by_animal.csv")

by_group <- lead %>%
  group_by(.data$Group) %>%
  summarise(n = dplyr::n(), n_with_leading_zero = sum(.data$leading_seed_suspected),
            mean_leading_zero_bins = mean(.data$n_leading_zero_entropy),
            max_leading_zero_bins = max(.data$n_leading_zero_entropy), .groups = "drop")
by_sex <- lead %>%
  group_by(.data$Sex) %>%
  summarise(n = dplyr::n(), n_with_leading_zero = sum(.data$leading_seed_suspected),
            mean_leading_zero_bins = mean(.data$n_leading_zero_entropy), .groups = "drop")
w(by_group, "leading_zero_burden_by_group.csv")
w(by_sex, "leading_zero_burden_by_sex.csv")

# Is the burden group-confounded? A one-way test on the COUNT, reported descriptively.
burden_test <- tryCatch({
  a <- stats::kruskal.test(n_leading_zero_entropy ~ factor(Group), data = lead)
  tibble(test = "Kruskal-Wallis on leading zero-entropy bin count by Group",
         statistic = unname(a$statistic), df = unname(a$parameter), p_value = a$p.value,
         interpretation = paste(
           "A non-significant result means the seeding burden is NOT differential by",
           "Group, so it cannot by itself generate a Group contrast. It remains a",
           "measurement caveat for the entropy channel."))
}, error = function(e) tibble(test = "kruskal", statistic = NA_real_, df = NA_real_,
                              p_value = NA_real_, interpretation = conditionMessage(e)))
w(burden_test, "leading_zero_burden_group_test.csv")

# ---------------------------- 1b. burden ACROSS cage changes (the binding check)
lead_all <- sel_all %>%
  arrange(.data$AnimalNum, .data$CageChangeLabel, .data$target_slot) %>%
  group_by(.data$AnimalNum, .data$Group, .data$Sex, .data$Batch, .data$CageChangeLabel) %>%
  summarise(n_leading_zero_entropy = { i <- which(.data$Entropy > 0)
              if (length(i)) min(i) - 1L else dplyr::n() },
            n_zero_entropy_total = sum(.data$Entropy == 0, na.rm = TRUE),
            .groups = "drop")
w(lead_all, "leading_zero_by_animal_and_cagechange.csv")

burden_by_cc <- lead_all %>%
  group_by(.data$CageChangeLabel) %>%
  summarise(n = dplyr::n(), mean_leading_zero = mean(.data$n_leading_zero_entropy),
            median_leading_zero = stats::median(.data$n_leading_zero_entropy),
            max_leading_zero = max(.data$n_leading_zero_entropy),
            pct_with_any = 100 * mean(.data$n_leading_zero_entropy >= 1),
            mean_zero_total = mean(.data$n_zero_entropy_total), .groups = "drop") %>%
  mutate(warning_note = paste(
    "The leading-bin burden is NOT constant across cage changes. Any CC-to-CC",
    "change score on an RMSSD-based domain must be checked against it; see",
    "burden_change_by_group.csv and the adjusted-vs-unadjusted comparison."))
w(burden_by_cc, "leading_zero_burden_by_cagechange.csv")

burden_change <- lead_all %>%
  select("AnimalNum", "Group", "Sex", "CageChangeLabel", "n_leading_zero_entropy") %>%
  tidyr::pivot_wider(names_from = "CageChangeLabel", values_from = "n_leading_zero_entropy")
if (all(c("CC1", "CC2") %in% names(burden_change))) {
  burden_change$delta_CC2_CC1 <- burden_change$CC2 - burden_change$CC1
  bt <- tryCatch(stats::kruskal.test(delta_CC2_CC1 ~ factor(Group), data = burden_change),
                 error = function(e) NULL)
  w(burden_change %>% group_by(.data$Group) %>%
      summarise(n = dplyr::n(), mean_CC1 = mean(.data$CC1), mean_CC2 = mean(.data$CC2),
                mean_delta = mean(.data$delta_CC2_CC1), .groups = "drop") %>%
      mutate(kruskal_p_delta_by_group = if (is.null(bt)) NA_real_ else bt$p.value,
             interpretation = paste(
               "If the burden CHANGE from CC1 to CC2 does not differ by Group, the",
               "leading-bin artifact cannot generate a Group effect in a CC2-CC1",
               "change score, however unequal the burden is between cage changes.")),
    "burden_change_by_group.csv")
}

# ------------------------------------- 2. recompute domains with leading bins trimmed
trim_first_k <- function(selected, k) {
  if (k <= 0) return(selected)
  selected %>% filter(.data$target_slot > k)
}
score_from <- function(selected, label) {
  ft <- mmm_rfid_build_window_features(selected)
  ft$CageEpochID <- mmm_rfid_cage_epoch_id(ft$Batch, ft$System, ft$CageChangeLabel)
  z <- mmm_rfid_standardize_within_sex(ft, reference_label = label)$scaled
  mmm_rfid_score_all_domains(z, id_cols = c("AnimalNum", "Group", "Sex", "Batch",
                                            "CageEpochID")) %>%
    mutate(variant = label)
}
base <- score_from(sel, "full_12h_primary")
variants <- purrr::map_dfr(c(1L, 2L, 4L, 6L), function(k) {
  score_from(trim_first_k(sel, k), paste0("trim_first_", k, "_slots"))
})
all_scores <- bind_rows(base, variants)
w(all_scores, "domain_scores_by_trim_variant.csv")

agree <- purrr::map_dfr(setdiff(unique(all_scores$variant), "full_12h_primary"), function(v) {
  purrr::map_dfr(MMM_RFID_CORE_DOMAINS, function(dm) {
    j <- inner_join(
      base %>% filter(.data$Domain == dm) %>% select("AnimalNum", "Sex", a = "DomainScore"),
      all_scores %>% filter(.data$variant == v, .data$Domain == dm) %>%
        select("AnimalNum", b = "DomainScore"), by = "AnimalNum")
    ok <- is.finite(j$a) & is.finite(j$b)
    if (sum(ok) < 3) return(tibble())
    tibble(variant = v, Domain = dm, n = sum(ok),
           pearson_r = stats::cor(j$a[ok], j$b[ok]),
           spearman_rho = stats::cor(j$a[ok], j$b[ok], method = "spearman"),
           max_abs_rank_shift = max(abs(rank(j$a[ok]) - rank(j$b[ok]))))
  })
})
w(agree, "score_agreement_vs_trim.csv")

# ---------------------------- 3. does the Group omnibus conclusion change?
omni <- purrr::map_dfr(unique(all_scores$variant), function(v) {
  purrr::map_dfr(c("Female", "Male"), function(sx) {
    purrr::map_dfr(MMM_RFID_CORE_DOMAINS, function(dm) {
      d <- all_scores %>% filter(.data$variant == v, .data$Sex == sx, .data$Domain == dm)
      mmm_rfid_omnibus_group(d, dm, sx) %>%
        filter(.data$model %in% c("legacy_lm", "cage_lmer")) %>%
        mutate(variant = v)
    })
  })
})
w(omni, "group_omnibus_by_trim_variant.csv")

omni_wide <- omni %>%
  select("variant", "Domain", "Sex", "model", "raw_p") %>%
  pivot_wider(names_from = "variant", values_from = "raw_p")
w(omni_wide, "group_omnibus_by_trim_variant_wide.csv")

verdict <- tibble(
  n_animals = nrow(lead),
  n_with_any_leading_zero_entropy = sum(lead$leading_seed_suspected),
  max_leading_zero_bins = max(lead$n_leading_zero_entropy),
  max_leading_zero_fraction_of_window = max(lead$n_leading_zero_entropy) / 72,
  burden_group_p = burden_test$p_value[1],
  min_spearman_vs_full = suppressWarnings(min(agree$spearman_rho, na.rm = TRUE)),
  note = paste(
    "The seed is an assumption, not an observation. This audit quantifies its reach",
    "by trimming leading bins and recomputing. The PRIMARY analysis keeps the full",
    "12 h window; these variants are robustness evidence only. Analysis/01 drops",
    "SeedReadTime, so seed STALENESS cannot be audited from the shipped table - that",
    "remains an open provenance gap and is recorded in docs/KNOWN_LIMITATIONS.md."))
w(verdict, "verdict.csv")

message("Leading-bin seed sensitivity written to ", OUT)
message("  animals with >=1 leading zero-entropy bin: ", verdict$n_with_any_leading_zero_entropy,
        " of ", verdict$n_animals, "; max ", verdict$max_leading_zero_bins, " bins of 72")
message("  burden-by-Group p = ", signif(verdict$burden_group_p, 3),
        "; min Spearman vs full window = ", signif(verdict$min_spearman_vs_full, 3))
