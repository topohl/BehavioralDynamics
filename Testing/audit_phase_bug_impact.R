## Quantifies the impact of the permissive-Active phase bug on Stages 11/12 and
## on the shared duration helper, by COUNTERFACTUAL: the old classifier is
## re-applied to the same Stage 01 input and compared against the exact
## classifier. This is the honest way to report the affected row count without
## needing the pre-fix artifacts, which were overwritten by the corrected rerun.
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tibble)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")

PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
OUT <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")

# The old, order-dependent classifier as it stood in Stage 11 (86-90) and
# Stage 12 (60-64): the Active branch was tested FIRST with a permissive
# substring, and "inactive" contains "active".
buggy_phase_class <- function(phase) {
  case_when(
    str_detect(str_to_lower(as.character(phase)), "active|dark|night") ~ "Active",
    str_detect(str_to_lower(as.character(phase)), "inactive|light|day") ~ "Inactive",
    TRUE ~ as.character(phase)
  )
}

rows <- list()
for (bl in c("10min_based", "5min_based")) {
  p <- file.path(PROJ, "analysis_ready/03_derived_metrics", bl, "all_behavior_metrics.csv")
  if (!file.exists(p)) next
  d <- read_csv(p, col_types = cols(AnimalNum = col_character(), Phase = col_character(),
                                    .default = col_guess()), progress = FALSE)
  buggy <- buggy_phase_class(d$Phase)
  exact <- mmm_phase_class(d$Phase, unmatched = "keep")
  rows[[length(rows) + 1]] <- tibble(
    bin_level = bl,
    stage_inputs_affected = "Analysis/11_behavioral_adaptation_kinetics.R; Analysis/12_sleep_like_quiescence_metrics.R",
    n_rows = nrow(d),
    raw_phase_labels = paste(names(table(d$Phase)), table(d$Phase), sep = "=", collapse = "; "),
    n_misclassified = sum(buggy != exact),
    pct_misclassified = 100 * sum(buggy != exact) / nrow(d),
    n_inactive_called_active = sum(exact == "Inactive" & buggy == "Active"),
    n_active_called_inactive = sum(exact == "Active" & buggy == "Inactive"),
    inactive_stratum_survived_old_code = sum(buggy == "Inactive"),
    n_inactive_double_counted_in_active_duration = sum(mmm_is_inactive_phase(d$Phase)),
    consequence = paste0(
      "Under the old code the Inactive stratum did not exist in Stage 11/12: every Inactive row was ",
      "relabelled Active, so any phase-stratified result from those stages was computed on a ",
      "collapsed phase variable. The shared duration helper separately credited the same rows to ",
      "active_duration_hours as well as inactive_duration_hours."),
    corrected_output_check = "post-fix Stage 12 sleep_like_inactivity_features.csv is 444 Active / 444 Inactive; Stage 11 adaptation_kinetics_features.csv is 1332 / 1332"
  )
}
impact <- bind_rows(rows)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write_csv(impact, file.path(OUT, "phase_bug_impact_counterfactual.csv"))

cat("=== PHASE BUG IMPACT (counterfactual) ===\n")
print(as.data.frame(impact %>% select(bin_level, n_rows, n_misclassified, pct_misclassified,
  n_inactive_called_active, inactive_stratum_survived_old_code,
  n_inactive_double_counted_in_active_duration)), row.names = FALSE)
cat("\nwrote phase_bug_impact_counterfactual.csv\n")

stopifnot(all(impact$n_active_called_inactive == 0))
stopifnot(all(impact$inactive_stratum_survived_old_code == 0))
cat("Asserted: the old code produced NO Inactive rows at all in these stages.\n")
