## PHASE B prerequisite: what exactly is "the first Active phase after CC1" in each source?
## Exposes (does not harmonize) the discrepancy between the raw 12-h first-night window and the
## HMM CC1 Active epoch, which spans several dark blocks.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
HMM <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")

for (res in c("10min_based", "5min_based")) {
  bs <- if (res == "10min_based") 600 else 300
  a <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                col_types = cols(AnimalNum = col_character(), State = col_character(), .default = col_guess())) %>%
    filter(CageChange == "CC1", Phase == "Active") %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum))
  step <- median(diff(sort(unique(a$TimeIndex))), na.rm = TRUE)
  blk <- a %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(gap = c(0, diff(TimeIndex)),
           block = cumsum(gap > 1.5 * step) + 1L) %>% ungroup()
  per_animal <- blk %>% group_by(AnimalNum) %>%
    summarise(n_bins = n(), span_hours = n() * bs / 3600,
              n_blocks = n_distinct(block),
              first_block_bins = sum(block == 1L),
              first_block_hours = sum(block == 1L) * bs / 3600,
              max_gap_bins = max(gap), max_gap_hours = max(gap) * bs / 3600, .groups = "drop")
  cat("\n######## ", res, " -- CC1 Active, HMM assignments (common state space)\n", sep = "")
  cat("  animals:", nrow(per_animal), "\n")
  cat("  TimeIndex median step:", step, " (bin size", bs, "sec)\n")
  cat("  bins per animal      : median", median(per_animal$n_bins), " range [", min(per_animal$n_bins), ",", max(per_animal$n_bins), "]\n")
  cat("  TOTAL span (hours)   : median", round(median(per_animal$span_hours), 2),
      " range [", round(min(per_animal$span_hours), 2), ",", round(max(per_animal$span_hours), 2), "]\n")
  cat("  contiguous blocks    :", paste(names(table(per_animal$n_blocks)), table(per_animal$n_blocks), sep = "x", collapse = " "), "\n")
  cat("  FIRST block (hours)  : median", round(median(per_animal$first_block_hours), 2),
      " range [", round(min(per_animal$first_block_hours), 2), ",", round(max(per_animal$first_block_hours), 2), "]\n")
  cat("  max within-epoch gap : median", round(median(per_animal$max_gap_hours), 2), "h\n")
  cat("  --> 12-h window would be", 12 * 3600 / bs, "bins;",
      "first block has median", median(per_animal$first_block_bins), "bins\n")
  cat("  --> animals whose first block is >= 12h:", sum(per_animal$first_block_hours >= 11.9),
      "of", nrow(per_animal), "\n")
}

cat("\n######## RAW first-night window as Stage 14 defines it\n")
cat("  primary_bin_level for Stage 14 = 5min_based  -> early_window_bins = 12*60/5 = 144 bins = 12.0 h\n")
cat("  first_active <- base %>% filter(CageChange == first_cage_change, Active) %>%\n")
cat("                  group_by(AnimalNum, Phase) %>% arrange(TimeIndex) %>%\n")
cat("                  mutate(local_bin = row_number()) %>% filter(local_bin <= 144)\n")
cat("  -> raw domains use the FIRST 12 h of the CC1 Active epoch (i.e. the first dark block).\n")
cat("  -> HMM domains, as currently produced by Stage 08, use the ENTIRE CC1 Active epoch.\n")

## Are 'Early adaptation / prediction' and 'Active-phase adaptation/exploration' identical at CC1?
ds <- read_csv(file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based/tables/systems_sis_domain_scores.csv"),
               col_types = cols(AnimalNum = col_character(), .default = col_guess()))
w <- ds %>% filter(PhaseClass == "Active", CageChangeIndex == 1,
                   Domain %in% c("Early adaptation / prediction", "Active-phase adaptation/exploration")) %>%
  select(AnimalNum, Domain, DomainScore) %>%
  pivot_wider(names_from = Domain, values_from = DomainScore)
cat("\n######## Duplicate-row check at CC1 Active\n")
cat("  n animals with both:", sum(complete.cases(w)), "\n")
cat("  max |Early adaptation/prediction - Active-phase adaptation/exploration| =",
    max(abs(w$`Early adaptation / prediction` - w$`Active-phase adaptation/exploration`), na.rm = TRUE), "\n")
cat("  correlation =", round(cor(w$`Early adaptation / prediction`, w$`Active-phase adaptation/exploration`,
                                 use = "complete.obs"), 10), "\n")
cat("  -> MATHEMATICALLY IDENTICAL at CC1 Active: include only ONE row.\n")
