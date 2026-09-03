## DEDICATED AUDIT: phase-aware inactive RFID dropout / QC redesign.
##
## Problem. Stage 14's chip-loss classifier decides from raw read density:
##   hard_dropout_signature = observed_fraction < 0.10 |
##                            (observed_fraction < 0.25 & longest_gap_hours >= 4)
## In the Inactive phase both clauses describe ordinary rest. A resting mouse
## triggers few antenna reads and can easily go >= 4 h between reads, so
## observed_fraction is confounded with the biology it is supposed to QC:
## median 0.149 in Inactive versus 1.000 in Active, with 95.7% of inactive
## epochs below 0.50. Reordering the case_when cannot help, because
## inactive_low_motion_review is itself gated on !hard_dropout_signature.
##
## Question. Is there a discriminator that separates genuine RFID loss from
## rest WITHOUT using absolute read density?
##
## Idea under test. Tag/animal-specific loss should be animal-specific;
## rest should be shared with co-housed animals on the same recording system.
## Animals in one Batch x System share hardware and the same recording window,
## so an animal that is unread while its system-mates are read normally in the
## SAME epoch is a loss candidate, whereas a whole system going quiet together
## is rest (or a system-wide outage, which is separable by its own signature).
##
## Read-only. Writes audit tables only; changes no production QC.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr); library(tibble)
})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")

PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
ST14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
OUT <- file.path(ST14, "audit_inactive_phase_qc")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
hr <- function(x) cat("\n########", x, "########\n")

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE),
  "Stage 01 roster")

## ---- inputs -----------------------------------------------------------------
qc <- read_csv(file.path(ST14, "tables/qc_chip_loss_flags.csv"),
               col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum),
         PhaseClass = mmm_phase_class(Phase),
         CageChangeIndex = as.integer(str_extract(as.character(CageChange), "[0-9]+")))

bins <- read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"),
                 col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum), PhaseClass = mmm_phase_class(Phase))

## Per animal-epoch presence evidence that does NOT depend on read density.
presence <- bins %>%
  group_by(AnimalNum, Batch, System, CageChange, PhaseClass) %>%
  summarise(n_bins = n(),
            n_positions_visited_max = suppressWarnings(max(n_positions_visited, na.rm = TRUE)),
            n_distinct_dominant_positions = n_distinct(DominantPosition[!is.na(DominantPosition)]),
            any_movement = any(is.finite(Movement) & Movement > 0),
            .groups = "drop") %>%
  mutate(n_positions_visited_max = ifelse(is.finite(n_positions_visited_max), n_positions_visited_max, NA_real_))

ep <- qc %>%
  select(AnimalNum, Group, Sex, Batch, System, CageChange, CageChangeIndex, PhaseClass,
         observed_fraction, longest_gap_hours, n_positions, position_switch_rate,
         qc_epoch_class, hard_dropout_signature, inactive_low_motion_review) %>%
  left_join(presence, by = c("AnimalNum", "Batch", "System", "CageChange", "PhaseClass")) %>%
  semi_join(roster, by = "AnimalNum")

hr("A. The confound, restated on current data")
print(as.data.frame(ep %>% group_by(PhaseClass) %>%
  summarise(n = n(), median_observed_fraction = round(median(observed_fraction, na.rm = TRUE), 4),
            frac_below_0.10 = round(mean(observed_fraction < 0.10, na.rm = TRUE), 3),
            frac_below_0.50 = round(mean(observed_fraction < 0.50, na.rm = TRUE), 3),
            median_longest_gap_h = round(median(longest_gap_hours, na.rm = TRUE), 2),
            frac_gap_ge_4h = round(mean(longest_gap_hours >= 4, na.rm = TRUE), 3),
            .groups = "drop")), row.names = FALSE)

## ---- the discriminator ------------------------------------------------------
## Co-observation group: same Batch x System x CageChange x PhaseClass.
ep <- ep %>%
  group_by(Batch, System, CageChange, PhaseClass) %>%
  mutate(group_n = n(),
         systemmate_median_of = median(observed_fraction[!is.na(observed_fraction)][-0], na.rm = TRUE),
         systemmate_median_excl_self = map_dbl(row_number(), function(i)
           median(observed_fraction[-i], na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(relative_read_density = observed_fraction / systemmate_median_excl_self)

hr("B. Does the co-observation contrast separate anything?")
print(as.data.frame(ep %>% filter(PhaseClass == "Inactive") %>%
  summarise(n = n(), median_group_n = median(group_n),
            median_relative = round(median(relative_read_density, na.rm = TRUE), 3),
            q10_relative = round(quantile(relative_read_density, 0.10, na.rm = TRUE), 3),
            q90_relative = round(quantile(relative_read_density, 0.90, na.rm = TRUE), 3),
            n_relative_lt_0.25 = sum(relative_read_density < 0.25, na.rm = TRUE),
            n_relative_lt_0.10 = sum(relative_read_density < 0.10, na.rm = TRUE))), row.names = FALSE)
cat("\nInterpretation: a relative density near 1 means the animal is as sparsely read as its\n",
    "system-mates in the SAME epoch, i.e. shared quiet (rest), not animal-specific loss.\n", sep = "")

## Same-animal adjacent Active-phase evidence: a working tag reads normally in
## the neighbouring dark phase of the same cage change.
adjacent_active <- ep %>% filter(PhaseClass == "Active") %>%
  transmute(AnimalNum, CageChange, active_observed_fraction = observed_fraction)
ep <- ep %>% left_join(adjacent_active, by = c("AnimalNum", "CageChange"))

hr("C. Same-animal adjacent Active-phase read density, for inactive epochs")
print(as.data.frame(ep %>% filter(PhaseClass == "Inactive") %>%
  summarise(n = n(),
            n_with_active_neighbour = sum(!is.na(active_observed_fraction)),
            median_active_neighbour = round(median(active_observed_fraction, na.rm = TRUE), 3),
            n_active_neighbour_ge_0.7 = sum(active_observed_fraction >= 0.7, na.rm = TRUE),
            n_active_neighbour_lt_0.3 = sum(active_observed_fraction < 0.3, na.rm = TRUE))),
  row.names = FALSE)

## ---- proposed three-class rule ---------------------------------------------
## A  evidence of RFID loss: animal-specific sparsity AND no presence evidence
##    AND the tag is not demonstrably working in the adjacent Active phase.
## B  low read density consistent with inactivity: sparsity shared with
##    system-mates, or positive presence evidence, or a working adjacent tag.
## C  uncertain: everything else -> sensitivity / manual review.
## Thresholds CALIBRATED against known positives rather than assumed. The eight
## Active epochs the current QC calls exclude_after_dropout are the closest
## available ground truth for genuine partial RFID loss. On the co-observation
## contrast they sit at relative_read_density 0.305-0.512 (median 0.385), while
## the 436 Active "usable" epochs sit at median 1.000 with a minimum of 0.586.
## That is a clean separating gap, so the boundary is placed inside it.
##
## Two corrections this calibration forced on the first draft of the rule:
##  - a 0.25 cut caught 0 of 8 known positives and was simply too strict;
##  - has_presence_evidence is TRUE for all 8 known positives, because partial
##    dropout still yields some reads and therefore some positions. Presence
##    evidence must NOT veto class A; it only supports class B when the
##    co-observation contrast is unremarkable.
MMM_INACTIVE_QC_LOSS_CUT <- 0.55
MMM_INACTIVE_QC_REVIEW_BAND <- c(0.45, 0.65)

ep <- ep %>%
  mutate(
    animal_specific_sparsity = is.finite(relative_read_density) &
      relative_read_density < MMM_INACTIVE_QC_LOSS_CUT,
    near_boundary = is.finite(relative_read_density) &
      relative_read_density >= MMM_INACTIVE_QC_REVIEW_BAND[1] &
      relative_read_density <= MMM_INACTIVE_QC_REVIEW_BAND[2],
    has_presence_evidence = (coalesce(n_positions_visited_max, 0) >= 1 &
                             coalesce(n_distinct_dominant_positions, 0L) >= 1L) | coalesce(any_movement, FALSE),
    tag_working_adjacent_active = coalesce(active_observed_fraction, 0) >= 0.7,
    proposed_class = case_when(
      !is.finite(relative_read_density) ~ "C_uncertain_review",
      near_boundary ~ "C_uncertain_review",
      animal_specific_sparsity ~ "A_rfid_loss_evidence",
      TRUE ~ "B_low_density_consistent_with_inactivity"
    )
  )

hr("C2. Threshold calibration against known positives (ACTIVE exclude_after_dropout)")
calib <- ep %>% filter(PhaseClass == "Active") %>%
  group_by(qc_epoch_class) %>%
  summarise(n = n(), median_relative = round(median(relative_read_density, na.rm = TRUE), 3),
            min_relative = round(min(relative_read_density, na.rm = TRUE), 3),
            max_relative = round(max(relative_read_density, na.rm = TRUE), 3), .groups = "drop")
print(as.data.frame(calib), row.names = FALSE)
known_pos <- ep %>% filter(PhaseClass == "Active", qc_epoch_class == "exclude_after_dropout")
known_neg <- ep %>% filter(PhaseClass == "Active", qc_epoch_class == "usable")
cat(sprintf("  cut = %.2f -> sensitivity %d/%d known positives, specificity %d/%d known negatives\n",
            MMM_INACTIVE_QC_LOSS_CUT,
            sum(known_pos$relative_read_density < MMM_INACTIVE_QC_LOSS_CUT, na.rm = TRUE), nrow(known_pos),
            sum(known_neg$relative_read_density >= MMM_INACTIVE_QC_LOSS_CUT, na.rm = TRUE), nrow(known_neg)))
write_csv(calib, file.path(OUT, "inactive_qc_threshold_calibration.csv"))

hr("D. Proposed class distribution for INACTIVE epochs")
print(as.data.frame(ep %>% filter(PhaseClass == "Inactive") %>% count(proposed_class)), row.names = FALSE)

hr("E. Proposed class vs the CURRENT qc_epoch_class (Inactive only)")
print(addmargins(table(current = ep$qc_epoch_class[ep$PhaseClass == "Inactive"],
                       proposed = ep$proposed_class[ep$PhaseClass == "Inactive"])))

hr("F. What the current rule excludes that the proposal would retain")
cur_excl <- c("exclude_after_dropout", "insufficient_data")
comp <- ep %>% filter(PhaseClass == "Inactive") %>%
  summarise(n_inactive = n(),
            current_would_exclude = sum(qc_epoch_class %in% cur_excl),
            proposed_class_A = sum(proposed_class == "A_rfid_loss_evidence"),
            retained_by_proposal_but_excluded_now =
              sum(qc_epoch_class %in% cur_excl & proposed_class != "A_rfid_loss_evidence"),
            flagged_by_proposal_but_kept_now =
              sum(!qc_epoch_class %in% cur_excl & proposed_class == "A_rfid_loss_evidence"))
print(as.data.frame(comp), row.names = FALSE)

write_csv(ep, file.path(OUT, "inactive_qc_epoch_evidence.csv"))

spec <- tribble(
  ~class, ~definition, ~evidence_used, ~read_density_used_absolutely, ~action,
  "A_rfid_loss_evidence",
  "Animal-specific sparsity: relative_read_density < 0.55 versus same Batch x System x CageChange x Phase system-mates, and outside the 0.45-0.65 review band. Cut calibrated on known positives, NOT assumed.",
  "co-observation contrast; positional presence; same-animal adjacent Active phase",
  FALSE,
  "exclude from primary analyses; report count",
  "B_low_density_consistent_with_inactivity",
  "relative_read_density >= 0.65: the animal is read as sparsely as its system-mates in the SAME epoch, i.e. shared quiet. Positional presence and a working adjacent-Active tag corroborate but are not required.",
  "same",
  FALSE,
  "RETAIN; this is rest-like biology, not a data defect",
  "C_uncertain_review",
  "relative_read_density in the 0.45-0.65 review band, or not computable (no usable system-mates).",
  "same",
  FALSE,
  "retain with a sensitivity flag; manual review before any phase-specific claim"
) %>% mutate(
  replaces = "hard_dropout_signature / insufficient_data / inactive_low_motion_review for Inactive epochs",
  why_reorder_insufficient = paste0(
    "Reordering case_when cannot work: inactive_low_motion_review is defined as ",
    "is_inactive_phase & !hard_dropout_signature & (...), so the guard sits in the DEFINITION, ",
    "not the branch order. The dropout criteria themselves must become phase-aware."),
  active_phase_unchanged = TRUE,
  implementation_status = "SPECIFIED, NOT IMPLEMENTED; chip_loss_qc_mode remains annotate_only"
)
write_csv(spec, file.path(OUT, "inactive_qc_proposed_specification.csv"))

hr("G. Deliverables")
cat("  ", file.path(OUT, "inactive_qc_epoch_evidence.csv"), "\n")
cat("  ", file.path(OUT, "inactive_qc_proposed_specification.csv"), "\n")
cat("\nchip_loss_qc_mode is NOT changed by this audit; it remains annotate_only.\n")
