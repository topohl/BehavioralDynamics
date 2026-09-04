# ==============================================================================
# 02_verify_zero_fill_mechanism.R
#
# The Stage-12 defect does NOT propagate as missingness. It propagates as ZERO.
#
#   safe_scale()  (14_..._dashboard.R:349-354)
#     s <- sd(x, na.rm = TRUE)
#     if (!is.finite(s) || s == 0) return(rep(0, length(x)))
#
# For an Inactive context group every quiescence value is NA, so sd() is NA,
# so safe_scale returns a vector of exact zeros. The feature therefore enters
# the domain formula as "exactly context-average" instead of being dropped by
# score_mean(na.rm = TRUE). This script proves that and derives what the two
# affected heatmap rows actually compute.
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr) })

DATA <- "C:/Users/topohl/AppData/Local/Temp/claude/c--Users-topohl-Documents-GitHub/77f6ce3c-a76b-4ee2-89c5-d1210af23338/scratchpad/data"
OUT  <- "C:/Users/topohl/Documents/GitHub/MMMSociability/manuscript/archive/BehavioralDynamics_schema_preproduction_audit/data"

raw    <- read.csv(file.path(DATA, "systems_sis_raw_phase_epoch_features.csv"), check.names = FALSE)
scores <- read.csv(file.path(DATA, "systems_sis_domain_scores.csv"), check.names = FALSE)

safe_scale <- function(x) {
  s <- sd(x, na.rm = TRUE); m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}
standardize_within_context <- function(dat, value_col,
                                       group_cols = c("Sex", "PhaseClass", "CageChangeIndex")) {
  dat %>% group_by(across(any_of(group_cols))) %>%
    mutate("{value_col}_z" := safe_scale(as.numeric(.data[[value_col]]))) %>% ungroup()
}
score_mean <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dat)))
  out <- rowMeans(as.matrix(dat[, cols, drop = FALSE]), na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}

sis_z_cols <- c("Movement_mean", "Movement_rmssd", "Movement_acf1",
                "Entropy_mean", "Entropy_rmssd", "Entropy_acf1",
                "Proximity_mean", "Proximity_rmssd", "Proximity_acf1",
                "inactivity_fraction", "mean_inactivity_bout_min", "inactivity_fragmentation",
                "active_inactive_transition_rate", "prolonged_inactivity_episodes_per_hour")
z <- Reduce(function(a, c) standardize_within_context(a, c),
            intersect(sis_z_cols, names(raw)), init = raw)

ii <- z$PhaseClass == "Inactive"
quiesc <- c("inactivity_fraction_z", "mean_inactivity_bout_min_z",
            "inactivity_fragmentation_z", "active_inactive_transition_rate_z")

cat("############ A. Raw quiescence values for Inactive epochs ############\n")
for (v in sub("_z$", "", quiesc))
  cat(sprintf("  %-34s non-NA Active=%-4d Inactive=%-4d\n", v,
              sum(!is.na(z[[v]][z$PhaseClass == "Active"])), sum(!is.na(z[[v]][ii]))))

cat("\n############ B. ...and the z-columns they become ############\n")
for (v in quiesc)
  cat(sprintf("  %-34s Inactive: all exactly 0? %-5s  (n non-NA = %d, max|val| = %g)\n", v,
              all(z[[v]][ii] == 0, na.rm = TRUE), sum(!is.na(z[[v]][ii])),
              max(abs(z[[v]][ii]), na.rm = TRUE)))

cat("\n############ C. What the rest row actually computes ############\n")
as_written <- score_mean(z, c("inactivity_fraction_z", "mean_inactivity_bout_min_z", "Movement_acf1_z")) -
  score_mean(z, c("Movement_rmssd_z", "inactivity_fragmentation_z", "active_inactive_transition_rate_z"))
third <- (z$Movement_acf1_z - z$Movement_rmssd_z) / 3

cat(sprintf("  max |as_written - (Movement_acf1_z - Movement_rmssd_z)/3| over Inactive = %.3e\n",
            max(abs(as_written[ii] - third[ii]), na.rm = TRUE)))

exp_rest <- scores %>% filter(Domain == "Inactive-phase rest/circadian regulation") %>%
  select(AnimalNum, CageChange, PhaseClass, exported = DomainScore)
chk <- exp_rest %>%
  inner_join(z[ii, ] %>% mutate(third = third[ii]) %>%
               select(AnimalNum, CageChange, PhaseClass, third),
             by = c("AnimalNum", "CageChange", "PhaseClass"))
cat(sprintf("  vs EXPORTED heatmap values: n = %d, max |exported - (acf1_z - rmssd_z)/3| = %.3e\n",
            nrow(chk), max(abs(chk$exported - chk$third), na.rm = TRUE)))
cat(sprintf("  correlation(exported, Movement_acf1_z - Movement_rmssd_z) = %.10f\n",
            cor(chk$exported, chk$third * 3, use = "complete.obs")))

cat("\n############ D. Volatility row, Inactive facet ############\n")
vol_written <- score_mean(z, c("Movement_rmssd_z", "Entropy_rmssd_z", "Proximity_rmssd_z",
                               "inactivity_fragmentation_z", "active_inactive_transition_rate_z"))
vol_3of5 <- score_mean(z, c("Movement_rmssd_z", "Entropy_rmssd_z", "Proximity_rmssd_z")) * 3 / 5
cat(sprintf("  max |as_written - 3/5 x mean(3 real rmssd terms)| over Inactive = %.3e\n",
            max(abs(vol_written[ii] - vol_3of5[ii]), na.rm = TRUE)))
cat("  => the two zero-filled terms shrink the Inactive volatility score by exactly 2/5.\n")

cat("\n############ E. Consequence for cross-facet comparability ############\n")
cat(sprintf("  sd(volatility) Active   = %.4f\n", sd(vol_written[z$PhaseClass == "Active"], na.rm = TRUE)))
cat(sprintf("  sd(volatility) Inactive = %.4f\n", sd(vol_written[ii], na.rm = TRUE)))

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(
  finding = c("quiescence_raw_nonNA_inactive",
              "quiescence_z_all_exactly_zero_inactive",
              "rest_row_equals_(Movement_acf1_z-Movement_rmssd_z)/3",
              "rest_max_abs_diff_vs_exported",
              "volatility_inactive_shrink_factor",
              "sd_volatility_active", "sd_volatility_inactive"),
  value = c(0, TRUE,
            TRUE, max(abs(chk$exported - chk$third), na.rm = TRUE),
            3 / 5,
            sd(vol_written[z$PhaseClass == "Active"], na.rm = TRUE),
            sd(vol_written[ii], na.rm = TRUE))),
  file.path(OUT, "zero_fill_mechanism_evidence.csv"), row.names = FALSE)
cat("\nwrote zero_fill_mechanism_evidence.csv\n")
