## Standalone execution of the PRODUCTION first-night builder, plus numerical
## parity against the audit outputs (item J). Writes to a scratch dir so it does
## not pre-empt the Stage 14 run.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("duration_normalization_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")
source_mmm_helper("first_night_domain_driver.R")

PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
OUT <- "C:/Users/topohl/AppData/Local/Temp/claude/c--Users-topohl-Documents-GitHub/2603874b-c21a-494f-aefd-10f961b8053d/scratchpad/orch/first_night_prod"
AUDIT <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap")

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                     Group = col_character(), Sex = col_character()), progress = FALSE),
  "Stage 01 roster")
cat("roster:", nrow(roster), "animals\n")

res <- map2(c("10min_based", "5min_based"), c("primary", "sensitivity"), function(bl, role)
  build_first_night_domain_analysis(bl, PROJ, file.path(OUT, bl), roster, resolution_role = role))
names(res) <- c("10min_based", "5min_based")

for (bl in names(res)) {
  wc <- res[[bl]]$window_contract
  cat("\n########", bl, "\n")
  cat("  window:", wc$anchor_clock_start, "->", wc$anchor_clock_end, " hours:", wc$window_hours,
      " sessions:", wc$n_sessions, "\n")
  cat("  rows:", wc$n_selected_rows, " animals:", wc$n_animals,
      " expected slots:", wc$expected_slots, " median observed:", wc$median_observed_slots, "\n")
  cat("  coverage: min", round(wc$min_coverage_fraction,4), " mean", round(wc$mean_coverage_fraction,4),
      " complete animals:", wc$n_animals_complete, "\n")
  cat("  inactive rows selected:", wc$n_inactive_rows_selected,
      " interior missing:", wc$total_missing_interior_slots,
      " trailing missing:", wc$total_missing_trailing_slots,
      " max internal gap:", wc$max_internal_gap_slots, "\n")
  q <- res[[bl]]$window_qc
  cat("  internal-gap distribution:", paste(names(table(q$max_internal_gap_slots)),
      table(q$max_internal_gap_slots), sep="x", collapse=" "), "\n")
}

cat("\n===== per-domain complete-case n by Sex x Group (10min primary) =====\n")
print(as.data.frame(res$`10min_based`$scores %>% filter(displayed) %>%
  group_by(Domain, Sex, Group) %>%
  summarise(n_scored = sum(is.finite(DomainScore)), n_incomplete = sum(!complete_contributors), .groups="drop") %>%
  pivot_wider(names_from=c(Sex,Group), values_from=c(n_scored,n_incomplete))), row.names=FALSE)

cat("\n===== animals with any incomplete displayed domain =====\n")
print(as.data.frame(res$`10min_based`$scores %>% filter(displayed, !complete_contributors) %>%
  select(AnimalNum, Group, Sex, Domain, missing_contributors)), row.names=FALSE)

cat("\n===== FIVE displayed domains, 10min PRIMARY =====\n")
print(as.data.frame(res$`10min_based`$contrasts %>%
  transmute(Domain=substr(Domain,1,38), Sex, contrast, n_ref, n_comp,
            g=round(hedges_g,3), est=round(estimate,3), SE=round(SE,3),
            CI=paste0("[",round(ci_low,2),",",round(ci_high,2),"]"),
            p=signif(raw_p,4), q=signif(q,4), n_fam=n_tests_in_family) %>%
  arrange(Sex, contrast, Domain)), row.names=FALSE)

cat("\n===== FIXED five rows, 5min SENSITIVITY =====\n")
print(as.data.frame(res$`5min_based`$contrasts %>%
  transmute(Domain=substr(Domain,1,38), Sex, contrast, g=round(hedges_g,3),
            est=round(estimate,3), p=signif(raw_p,4), q=signif(q,4)) %>%
  arrange(Sex, contrast, Domain)), row.names=FALSE)

cat("\n===== Group x Sex interactions (5 tests) =====\n")
print(as.data.frame(res$`10min_based`$interactions %>%
  transmute(Domain=substr(Domain,1,38), df_num, df_den, F=round(F_value,4),
            p=signif(raw_p,4), q=signif(q,4), n_fam=n_tests_in_family,
            sex_diff_supported=sex_differential_language_supported)), row.names=FALSE)

cat("\n===== 24-test transparency family (10min) =====\n")
print(as.data.frame(res$`10min_based`$full_family %>% filter(Sex=="Female") %>%
  transmute(Domain=substr(Domain,1,38), contrast, g=round(hedges_g,3),
            p=signif(raw_p,4), q24=signif(q,4), n_fam=n_tests_in_family) %>%
  arrange(contrast, Domain)), row.names=FALSE)

## ---- item J: numerical parity against the audit ----
cat("\n===== J. PARITY vs AUDIT (edc49e0 / 2075739) =====\n")
af <- file.path(AUDIT, "first_night_10domain_scores.csv")
if (file.exists(af)) {
  aud <- read_csv(af, col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
    filter(grepl("10min", bin_resolution))
  name_map <- c("Active-phase adaptation / exploration" = "Active-phase adaptation/exploration")
  prod <- res$`10min_based`$scores %>%
    mutate(Domain_audit = ifelse(Domain %in% names(name_map), name_map[Domain], Domain))
  j <- prod %>%
    inner_join(aud %>% select(AnimalNum, Domain, audit_score = DomainScore),
               by = c("AnimalNum", "Domain_audit" = "Domain"))
  cat("  joined rows:", nrow(j), " domains:", n_distinct(j$Domain), "\n")
  cmp <- j %>% group_by(Domain) %>%
    summarise(n = n(), n_both_finite = sum(is.finite(DomainScore) & is.finite(audit_score)),
              max_abs_diff = suppressWarnings(max(abs(DomainScore - audit_score), na.rm = TRUE)),
              r = suppressWarnings(cor(DomainScore, audit_score, use = "complete.obs")),
              n_prod_na_audit_finite = sum(is.na(DomainScore) & is.finite(audit_score)),
              .groups = "drop")
  print(as.data.frame(cmp %>% mutate(Domain = substr(Domain, 1, 38),
    max_abs_diff = signif(max_abs_diff, 4), r = round(r, 6))), row.names = FALSE)
  cat("\n  Animals where production is NA but the audit had a value (strict-completeness effect):\n")
  print(as.data.frame(j %>% filter(is.na(DomainScore), is.finite(audit_score)) %>%
    select(AnimalNum, Group, Sex, Domain, missing_contributors, audit_score) %>%
    mutate(audit_score = round(audit_score, 4))), row.names = FALSE)
} else cat("  audit score file not found:", af, "\n")
cat("\nwrote production first-night outputs under", OUT, "\n")
