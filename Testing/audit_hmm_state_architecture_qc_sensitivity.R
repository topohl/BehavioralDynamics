## Blocking-finding verification: is the phenotype driven by epochs the project's OWN QC
## classifies as "exclude epoch from primary analyses"?
## Stage 14 sets chip_loss_qc_mode <- "annotate_only" (line 139), so no exclusion is applied;
## and build_hmm_epoch_scores() reads Stage 08 occupancy directly, never joining qc_epoch_class.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
B <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based"
A <- file.path(B, "audit_hmm_state_architecture")

qc <- read_csv(file.path(B, "tables/qc_chip_loss_flags.csv"),
               col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum),
         PhaseClass = case_when(str_detect(str_to_lower(Phase), "inactive|light|day") ~ "Inactive",
                                str_detect(str_to_lower(Phase), "active|dark|night") ~ "Active", TRUE ~ Phase),
         CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+")))
cat("=== qc_epoch_class distribution (all animals/epochs) ===\n"); print(table(qc$qc_epoch_class, qc$PhaseClass))
drop <- qc %>% filter(qc_epoch_class %in% c("exclude_after_dropout", "insufficient_data"))
cat("\n=== epochs QC says to exclude:", nrow(drop), "===\n")
print(as.data.frame(drop %>% count(PhaseClass, qc_epoch_class, Group, Sex)), row.names = FALSE)
cat("\nper-animal detail:\n")
print(as.data.frame(drop %>% transmute(AnimalNum, Group, Sex, CageChange, PhaseClass, qc_epoch_class,
      observed_fraction = round(observed_fraction, 4)) %>% arrange(PhaseClass, Group, AnimalNum)), row.names = FALSE)

met <- read_csv(file.path(A, "hmm_architecture_temporal_epoch_metrics.csv"),
                col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
  left_join(qc %>% select(AnimalNum, CageChangeIndex, PhaseClass, qc_epoch_class, observed_fraction),
            by = c("AnimalNum", "CageChangeIndex", "PhaseClass")) %>%
  mutate(qc_epoch_class = coalesce(qc_epoch_class, "usable"),
         qc_drop = qc_epoch_class %in% c("exclude_after_dropout", "insufficient_data"))
cat("\n=== HMM epochs by qc class (per resolution) ===\n")
print(table(met$resolution, met$qc_epoch_class))

METRICS <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability", "transition_entropy", "mean_dwell_bins")
cat("\n=== read-density association (Spearman with observed_fraction, animal level) ===\n")
for (rs in unique(met$resolution)) for (ph in c("Active", "Inactive")) {
  al <- met %>% filter(resolution == rs, PhaseClass == ph) %>% group_by(AnimalNum) %>%
    summarise(across(all_of(c(METRICS, "observed_fraction")), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  cat(sprintf(" %s / %-8s : %s\n", rs, ph,
      paste(sprintf("%s=%+.3f", c("occEnt","switch","selfP","transEnt","dwell"),
        sapply(METRICS, function(v) suppressWarnings(cor(al[[v]], al$observed_fraction, method="spearman", use="complete.obs")))), collapse="  ")))
}

run <- function(dat, tag) {
  sc <- reduce(METRICS, function(a, v) strict_standardize_within_context(a, v), .init = dat)
  long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, all_of(paste0(METRICS, "_z"))) %>%
    pivot_longer(all_of(paste0(METRICS, "_z")), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(Domain = sub("_z$", "", Domain)) %>% filter(is.finite(DomainScore))
  map_dfr(c("Active", "Inactive"), function(ph) map_dfr(METRICS, function(v)
    fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts)) %>% mutate(analysis_set = tag)
}
cmp <- list()
for (rs in c("10min_based", "5min_based")) {
  d <- met %>% filter(resolution == rs)
  cmp[[length(cmp)+1]] <- run(d, "full") %>% mutate(resolution = rs)
  cmp[[length(cmp)+1]] <- run(d %>% filter(!qc_drop), "qc_excluded") %>% mutate(resolution = rs)
}
ct <- bind_rows(cmp)
write_csv(ct, file.path(A, "hmm_architecture_qc_leaveout_sensitivity.csv"))

cat("\n=== FEMALE: full vs QC-excluded (10min primary, context-z estimates) ===\n")
for (ph in c("Active", "Inactive")) {
  cat("\n### ", ph, "\n", sep = "")
  print(as.data.frame(ct %>% filter(resolution == "10min_based", Sex == "Female", PhaseClass == ph) %>%
    select(Domain, contrast, analysis_set, est = mixed_model_estimate, p = mixed_model_p, n_ref = n_ref_animals) %>%
    pivot_wider(names_from = analysis_set, values_from = c(est, p, n_ref)) %>%
    transmute(Domain = substr(Domain, 1, 28), contrast,
              est_full = round(est_full, 3), est_qcx = round(est_qc_excluded, 3),
              p_full = signif(p_full, 3), p_qcx = signif(p_qc_excluded, 3),
              nCON_full = n_ref_full, nCON_qcx = n_ref_qc_excluded,
              pct_change = ifelse(abs(est_full) > 0.15, round(100*(1 - abs(est_qc_excluded)/abs(est_full))), NA)) %>%
    arrange(contrast, Domain)), row.names = FALSE)
}
