## Point (1): does pooling low-activity states into ONE "inactive" label ERASE distinct
## low-activity/high-proximity vs low-activity/low-proximity signals?
## Method: run the SAME corrected repeated-measures estimator on EACH individual state's
## occupancy fraction, then compare with the pooled inactive_state_fraction. If the pooled
## states move in OPPOSITE directions between groups, pooling cancels real variance.
## No new construct is proposed here; this is a decomposition of the existing negative term.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
HMM <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")
PH_INACT <- "\\binactive\\b|\\blight\\b|\\bday\\b"
PH_ACT <- "\\bactive\\b|\\bdark\\b|\\bnight\\b"

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE),
  "Stage 01 5min_based roster")
stopifnot(nrow(roster) == 111)

out <- list()
for (res in c("10min_based", "5min_based")) {
  occ <- read_csv(file.path(HMM, res, "tables/hmm_state_occupancy.csv"),
                  col_types = cols(AnimalNum = col_character(), State = col_character(), .default = col_guess()))
  aud <- audit_hmm_identity(occ, roster, paste("occupancy", res)); assert_hmm_identity_audit(aud)
  ss <- read_csv(file.path(HMM, res, "tables/hmm_state_summary.csv"), show_col_types = FALSE)
  lab <- annotate_hmm_semantic_states(ss, res)

  ep <- aud$data %>%
    mutate(State = as.character(State),
           PhaseClass = case_when(str_detect(str_to_lower(Phase), PH_INACT) ~ "Inactive",
                                  str_detect(str_to_lower(Phase), PH_ACT) ~ "Active", TRUE ~ Phase),
           CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+")),
           frac_time = as.numeric(frac_time)) %>%
    select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, State, frac_time) %>%
    # zero-fill states an animal never occupied in that epoch (absent rows == 0 occupancy)
    complete(nesting(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass),
             State = as.character(sort(as.integer(lab$State))), fill = list(frac_time = 0))

  pooled <- ep %>%
    left_join(lab %>% select(State, SemanticState), by = "State") %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    summarise(inactive_state_fraction = sum(frac_time[SemanticState == "inactive/low-exploration"]),
              .groups = "drop") %>%
    mutate(State = "POOLED_inactive_label") %>%
    rename(frac_time = inactive_state_fraction)

  long <- bind_rows(ep %>% select(names(pooled)), pooled)

  # z within the SAME context the shipped composite uses, so estimates are on a comparable scale
  scored <- long %>%
    rename(value = frac_time) %>%
    group_by(State) %>%
    group_modify(~ strict_standardize_within_context(.x, "value")) %>%
    ungroup() %>%
    transmute(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
              Domain = State, DomainScore = value_z)

  for (ph in c("Active", "Inactive")) {
    for (st in unique(scored$Domain)) {
      r <- fit_repeated_measures_domain_contrasts(scored, st, ph)
      out[[length(out) + 1]] <- r$contrasts %>%
        mutate(resolution = res,
               Proximity_z = if (st == "POOLED_inactive_label") NA_real_ else lab$Proximity_z[lab$State == st],
               Movement_z = if (st == "POOLED_inactive_label") NA_real_ else lab$Movement_z[lab$State == st],
               SemanticState = if (st == "POOLED_inactive_label") "POOLED" else lab$SemanticState[lab$State == st])
    }
  }
}
res_tbl <- bind_rows(out)

cat("\n=============== PER-STATE OCCUPANCY vs POOLED 'inactive' LABEL ===============\n")
cat("Estimates are context-z occupancy fractions; model = value ~ Group*Sex + factor(CC) + (1|Animal)\n")
for (rs in c("10min_based", "5min_based")) {
  for (ph in c("Active", "Inactive")) {
    cat("\n#### ", rs, " / ", ph, " / FEMALE\n", sep = "")
    print(as.data.frame(res_tbl %>%
      filter(resolution == rs, PhaseClass == ph, Sex == "Female") %>%
      arrange(desc(is.na(Proximity_z)), desc(Proximity_z), contrast) %>%
      transmute(state = Domain, SemanticState, Prox_z = round(Proximity_z, 3), Mov_z = round(Movement_z, 3),
                contrast, est = round(mixed_model_estimate, 3), SE = round(mixed_model_SE, 3),
                p = signif(mixed_model_p, 3), g = round(animal_level_hedges_g, 3))), row.names = FALSE)
  }
}
write_csv(res_tbl, file.path(getOption("mmm.audit_out", "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"), "hmm_architecture_per_state_occupancy_contrasts.csv"))
cat("\nwrote per_state_occupancy_contrasts.csv rows =", nrow(res_tbl), "\n")
