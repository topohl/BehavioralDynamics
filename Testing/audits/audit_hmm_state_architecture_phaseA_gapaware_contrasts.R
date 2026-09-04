## ISSUE 4 (fixed): gap-aware vs original contrasts.
## Bug in prior run: sub("_gapaware_z$","", sub("_z$","",x)) -- the inner sub strips "_z" first, so the
## outer pattern can never match. Correct order: strip "_z" then "_gapaware".
suppressMessages({library(dplyr); library(tidyr); library(readr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
OUT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
comp <- read_csv(file.path(OUT,"hmm_architecture_component_epoch_metrics.csv"),
                 col_types = cols(AnimalNum=col_character(), .default=col_guess()))
G <- c("transition_entropy","state_switch_rate","mean_dwell_bins","self_transition_probability")
cmp <- list()
for (rs in c("10min_based","5min_based")) {
  d <- comp %>% filter(resolution == rs)
  for (variant in c("original","gapaware")) {
    cols <- if (variant=="original") G else paste0(G,"_gapaware")
    sc <- reduce(cols, function(a,v) strict_standardize_within_context(a, v), .init = d)
    long <- sc %>%
      select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, all_of(paste0(cols,"_z"))) %>%
      pivot_longer(all_of(paste0(cols,"_z")), names_to="Domain", values_to="DomainScore") %>%
      mutate(Domain = sub("_gapaware$", "", sub("_z$", "", Domain))) %>%   # <- corrected order
      filter(is.finite(DomainScore))
    stopifnot(all(G %in% unique(long$Domain)))
    for (ph in c("Active","Inactive")) for (v in G)
      cmp[[length(cmp)+1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
        mutate(resolution = rs, variant = variant)
  }
}
gt <- bind_rows(cmp)
gcmp <- gt %>% select(resolution, Domain, PhaseClass, Sex, contrast, variant,
                      est=mixed_model_estimate, SE=mixed_model_SE, p=mixed_model_p, df=mixed_model_df) %>%
  pivot_wider(names_from=variant, values_from=c(est,SE,p,df)) %>%
  mutate(abs_diff = est_gapaware - est_original,
         pct_diff = ifelse(abs(est_original) > 1e-8, 100*(est_gapaware/est_original - 1), NA_real_),
         direction_changed = sign(est_original) != sign(est_gapaware),
         ci_low_original  = est_original - 1.96*SE_original,  ci_high_original  = est_original + 1.96*SE_original,
         ci_low_gapaware  = est_gapaware - 1.96*SE_gapaware,  ci_high_gapaware  = est_gapaware + 1.96*SE_gapaware,
         ci_overlap = pmin(ci_high_original, ci_high_gapaware) >= pmax(ci_low_original, ci_low_gapaware),
         gap_detection_rule = "new run/sequence boundary where diff(TimeIndex) > 1.5 * median(step)")
write_csv(gcmp, file.path(OUT,"phaseA_issue4_gapaware_contrast_comparison.csv"))

show <- function(rs, ph, sx) {
  cat("\n===== ", rs, " / ", ph, " / ", sx, " =====\n", sep="")
  print(as.data.frame(gcmp %>% filter(resolution==rs, PhaseClass==ph, Sex==sx) %>%
    transmute(Domain=substr(Domain,1,27), contrast,
              orig=round(est_original,4), gap=round(est_gapaware,4),
              abs_diff=round(abs_diff,4), pct=round(pct_diff,2),
              CI_orig=paste0("[",round(ci_low_original,2),",",round(ci_high_original,2),"]"),
              CI_gap =paste0("[",round(ci_low_gapaware,2),",",round(ci_high_gapaware,2),"]"),
              p_orig=signif(p_original,3), p_gap=signif(p_gapaware,3),
              dir_chg=direction_changed, CI_ovl=ci_overlap) %>%
    arrange(contrast, Domain)), row.names=FALSE)
}
show("10min_based","Active","Female"); show("10min_based","Inactive","Female")
cat("\n########## SUMMARY ##########\n")
cat("Any direction change anywhere?", any(gcmp$direction_changed, na.rm=TRUE), "\n")
cat("All original/gap-aware 95% CIs overlap?", all(gcmp$ci_overlap, na.rm=TRUE), "\n")
big <- gcmp %>% filter(abs(est_original) > 0.15)
cat("Cells with |est_original| > 0.15:", nrow(big),
    " max |pct_diff| =", round(max(abs(big$pct_diff), na.rm=TRUE),2), "%",
    " median |pct_diff| =", round(median(abs(big$pct_diff), na.rm=TRUE),2), "%\n")
cat("Largest single shift:\n")
print(as.data.frame(big %>% slice_max(abs(pct_diff), n=3) %>%
  transmute(resolution, Domain, PhaseClass, Sex, contrast,
            orig=round(est_original,4), gap=round(est_gapaware,4), pct=round(pct_diff,2))), row.names=FALSE)
cat("\nProvenance of the '<=3.8%' figure: it is the epoch-level MEAN shift of mean_dwell_bins at 10min\n")
cat("  (7.8341 -> 7.5336 bins = -3.84%); the other three metrics shift -1.25% to +0.40%.\n")
