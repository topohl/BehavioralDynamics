## PHASE A, issues 1, 2, 5 -- definitional / arithmetic resolution. Read-only.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
A <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
B <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based"
HMM <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/06_behavioral_dynamics/hmm_states"

comp <- read_csv(file.path(A, "hmm_architecture_component_epoch_metrics.csv"),
                 col_types = cols(AnimalNum = col_character(), .default = col_guess()))

cat("################ ISSUE 1: FORMULA PRESERVATION ################\n")
## Three candidate scores, computed on the SAME context-z terms the shipped code uses.
z_occ <- comp$shipped_state_occupancy_entropy_z; z_inact <- comp$shipped_inactive_state_fraction_z
z_soc <- comp$shipped_social_state_fraction_z
shipped <- comp[["Behavioral state architecture"]]
hist_as_written <- rowMeans(cbind(z_occ, z_soc), na.rm = FALSE) - z_inact
reduced_correct <- 0.5 * z_occ - z_inact          # (a) dead term deleted, COEFFICIENT PRESERVED
naive_delete    <- z_occ - z_inact                # (b) renormalized = POST-HOC REWEIGHTING
cat("  z(social_state_fraction): all exactly zero? ", all(z_soc == 0), "\n")
cat("  max|shipped - historical_as_written|      = ", max(abs(shipped - hist_as_written), na.rm = TRUE), "\n")
cat("  max|shipped - 0.5*z(occ) - z(inact)|      = ", max(abs(shipped - reduced_correct), na.rm = TRUE),
    "   <- (a) IDENTICAL\n")
cat("  max|shipped -   z(occ)   - z(inact)|      = ", max(abs(shipped - naive_delete), na.rm = TRUE),
    "   <- (b) DIFFERENT SCORE\n")
cat("  cor(shipped, naive_delete)                = ", round(cor(shipped, naive_delete, use = "complete.obs"), 6), "\n")
cat("  sd(shipped) =", round(sd(shipped, na.rm = TRUE), 4),
    " sd(naive_delete) =", round(sd(naive_delete, na.rm = TRUE), 4),
    " -> variance ratio", round(var(naive_delete, na.rm = TRUE)/var(shipped, na.rm = TRUE), 4), "\n")
issue1 <- tibble(
  candidate = c("historical_as_written", "reduced_coefficient_preserved", "naive_term_deletion_renormalized"),
  formula = c("mean(z(occupancy_entropy), z(social_state_fraction)) - z(inactive_state_fraction)",
              "0.5 * z(occupancy_entropy) - z(inactive_state_fraction)",
              "z(occupancy_entropy) - z(inactive_state_fraction)"),
  positive_term_coefficient = c(0.5, 0.5, 1.0),
  max_abs_diff_vs_shipped = c(max(abs(shipped - hist_as_written), na.rm = TRUE),
                              max(abs(shipped - reduced_correct), na.rm = TRUE),
                              max(abs(shipped - naive_delete), na.rm = TRUE)),
  identical_to_shipped = c(TRUE, TRUE, FALSE),
  status = c("shipped", "SAFE: description-only change, coefficient preserved",
             "POST-HOC REWEIGHTING: new sensitivity construct, must be labelled as such"))
write_csv(issue1, file.path(A, "phaseA_issue1_formula_preservation.csv"))

cat("\n################ ISSUE 2: DWELL-TIME UNITS ################\n")
bs <- comp %>% group_by(resolution) %>%
  summarise(bin_size_sec = first(bin_size_sec), .groups = "drop")
print(as.data.frame(bs))
d2 <- comp %>% group_by(resolution) %>%
  summarise(mean_bins = mean(mean_dwell_bins, na.rm = TRUE), mean_hours = mean(mean_dwell_hours, na.rm = TRUE),
            mean_minutes = 60*mean(mean_dwell_hours, na.rm = TRUE),
            ratio_hours_over_bins = mean_hours/mean_bins, .groups = "drop")
print(as.data.frame(d2), digits = 5)
cat("\n  Is mean_dwell_hours exactly mean_dwell_bins * bin_size_sec/3600?  max abs dev =",
    max(abs(comp$mean_dwell_hours - comp$mean_dwell_bins*comp$bin_size_sec/3600), na.rm = TRUE), "\n")
## Decisive: a positive affine rescaling leaves context-z (and hence every contrast) unchanged.
zb <- strict_standardize_within_context(comp %>% filter(resolution=="10min_based"), "mean_dwell_bins")$mean_dwell_bins_z
zh <- strict_standardize_within_context(comp %>% filter(resolution=="10min_based"), "mean_dwell_hours")$mean_dwell_hours_z
cat("  WITHIN resolution: max|z(bins) - z(hours)| =", max(abs(zb - zh), na.rm = TRUE),
    " -> identical z, so ALL context-z contrasts are unaffected by the unit choice\n")
cat("\n  CROSS-resolution comparability of the RAW quantity:\n")
cat("    dwell in BINS : 5min =", round(d2$mean_bins[d2$resolution=="5min_based"],3),
    " 10min =", round(d2$mean_bins[d2$resolution=="10min_based"],3),
    " -> ratio", round(d2$mean_bins[d2$resolution=="5min_based"]/d2$mean_bins[d2$resolution=="10min_based"],3),
    "  (different UNITS: a '5-min bin' != a '10-min bin')\n")
cat("    dwell in MIN  : 5min =", round(d2$mean_minutes[d2$resolution=="5min_based"],2),
    " 10min =", round(d2$mean_minutes[d2$resolution=="10min_based"],2),
    " -> ratio", round(d2$mean_minutes[d2$resolution=="10min_based"]/d2$mean_minutes[d2$resolution=="5min_based"],3),
    "  (common PHYSICAL unit; residual gap = bout censoring at coarser bins)\n")
issue2 <- tibble(
  question = c("within-resolution inference", "cross-resolution comparison"),
  correct_unit = c("mean_dwell_bins OR mean_dwell_hours (equivalent)", "physical time (minutes / hours)"),
  reason = c(paste0("hours = bins * bin_size_sec/3600 is a positive affine rescaling; context-z is invariant, ",
                    "max|z(bins)-z(hours)| = ", signif(max(abs(zb-zh), na.rm=TRUE),3), ", so every contrast is identical"),
             paste0("bins are resolution-specific units (a 5-min bin is not a 10-min bin); physical time is the ",
                    "common scale. Raw means: ", round(d2$mean_minutes[d2$resolution=="5min_based"],1), " min (5min) vs ",
                    round(d2$mean_minutes[d2$resolution=="10min_based"],1), " min (10min)")),
  earlier_audit_statement = "mean_dwell_bins is bin-comparable and mean_dwell_hours is not",
  verdict = c("earlier statement harmless here (units irrelevant within a resolution)",
              "earlier statement WRONG and now corrected: use physical time across resolutions"),
  residual_caveat = "Even in physical time the two resolutions are not identical: coarser bins censor short bouts, inflating physical dwell (52.7 min at 5min vs 78.3 min at 10min, +49%). This is a temporal-resolution artefact, not a unit artefact.")
write_csv(issue2, file.path(A, "phaseA_issue2_dwell_units.csv"))

cat("\n################ ISSUE 5: WHAT 'QC-USABLE' MEANS ################\n")
qc <- read_csv(file.path(B, "tables/qc_chip_loss_flags.csv"),
               col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum),
         PhaseClass2 = case_when(str_detect(str_to_lower(Phase),"inactive|light|day") ~ "Inactive",
                                 str_detect(str_to_lower(Phase),"active|dark|night") ~ "Active", TRUE ~ Phase),
         CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+")))
cat("\n[POPULATION 1] qc_chip_loss_flags.csv as written -- its OWN population:\n")
print(addmargins(table(qc$qc_epoch_class, qc$PhaseClass2)))
seq_q <- read_csv(file.path(HMM,"10min_based/tables/hmm_sequence_quality_audit.csv"),
                  col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  mutate(PhaseClass2 = case_when(str_detect(str_to_lower(Phase),"inactive")~"Inactive", TRUE~"Active"))
cat("\n[LAYER 1 AVAILABILITY] expected canonical epochs = 111 animals x 4 CC x 2 phases =", 111*4*2, "\n")
cat("  epochs with >=1 Stage 01 input bin      :", sum(seq_q$input_bins > 0), "\n")
cat("  epochs with 0 Stage 01 input bins       :", sum(seq_q$input_bins == 0), "\n")
cat("\n[LAYER 2 MODEL ELIGIBILITY] Stage 08 rule: >=4 complete Movement/Entropy/Proximity bins\n")
print(as.data.frame(seq_q %>% group_by(PhaseClass2) %>%
  summarise(expected = n(), retained_for_hmm = sum(retained_for_hmm), excluded = sum(!retained_for_hmm), .groups="drop")))
cat("  -> HMM epochs actually fitted:", sum(seq_q$retained_for_hmm), "/", nrow(seq_q),
    "=", round(100*sum(seq_q$retained_for_hmm)/nrow(seq_q),2), "%\n")
cat("\n[LAYER 3 QUALITY FLAGGING] joined onto the 882 fitted HMM epochs (10min):\n")
hmm_ep <- comp %>% filter(resolution=="10min_based") %>%
  select(AnimalNum, Group, Sex, CageChangeIndex, PhaseClass) %>%
  left_join(qc %>% select(AnimalNum, CageChangeIndex, PhaseClass=PhaseClass2, qc_epoch_class, observed_fraction),
            by=c("AnimalNum","CageChangeIndex","PhaseClass")) %>%
  mutate(qc_epoch_class = coalesce(qc_epoch_class, "not_flagged_absent_from_qc_table"))
print(addmargins(table(hmm_ep$qc_epoch_class, hmm_ep$PhaseClass)))
cat("\n[LAYER 4 STRICT SENSITIVITY] my leave-out set = exclude_after_dropout + insufficient_data\n")
print(as.data.frame(hmm_ep %>% group_by(PhaseClass) %>%
  summarise(n = n(), strict_drop = sum(qc_epoch_class %in% c("exclude_after_dropout","insufficient_data")),
            strict_keep = n - strict_drop, .groups="drop")))
cat("\n[WHY inactive epochs fail] observed_fraction = n_raw_reads / expected_reads, where expected_reads\n")
cat("  assumes ~one read per bin-width. A resting mouse triggers far fewer antenna reads, so the metric\n")
cat("  is confounded with the biology in the Inactive phase.\n")
print(as.data.frame(hmm_ep %>% group_by(PhaseClass) %>%
  summarise(n = n(), median_observed_fraction = round(median(observed_fraction, na.rm=TRUE),4),
            q10 = round(quantile(observed_fraction, .10, na.rm=TRUE),4),
            q90 = round(quantile(observed_fraction, .90, na.rm=TRUE),4),
            frac_below_0.10 = round(mean(observed_fraction < 0.10, na.rm=TRUE),3),
            frac_below_0.50 = round(mean(observed_fraction < 0.50, na.rm=TRUE),3), .groups="drop")))
cat("\n  NOTE ordering defect: case_when tests insufficient_data (obs<0.10) and hard_dropout_signature\n")
cat("  BEFORE inactive_low_motion_review, so rest-driven low read density is labelled 'insufficient_data'\n")
cat("  ('exclude epoch from primary analyses') rather than the review class the repo created for it.\n")
write_csv(
  bind_rows(
    hmm_ep %>% count(PhaseClass, qc_epoch_class, name="n_hmm_epochs") %>% mutate(layer="3_quality_flagging", population="882 fitted HMM epochs (10min)"),
    seq_q %>% group_by(PhaseClass2) %>% summarise(n_hmm_epochs=sum(retained_for_hmm), .groups="drop") %>%
      transmute(PhaseClass=PhaseClass2, qc_epoch_class="retained_for_hmm", n_hmm_epochs, layer="2_model_eligibility", population="888 expected canonical epochs"),
    seq_q %>% group_by(PhaseClass2) %>% summarise(n_hmm_epochs=sum(input_bins>0), .groups="drop") %>%
      transmute(PhaseClass=PhaseClass2, qc_epoch_class="has_stage01_input", n_hmm_epochs, layer="1_availability", population="888 expected canonical epochs")),
  file.path(A, "phaseA_issue5_qc_population_ladder.csv"))
cat("\nwrote phaseA_issue1/2/5 CSVs\n")
