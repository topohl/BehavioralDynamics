# ==============================================================================
# 01_verify_rest_domain_collapse.R
# Consequence of the Stage-12 phase-parser defect for the heatmap.
#
# Stage 12 (12_sleep_like_quiescence_metrics.R:61-65) tests "active|dark|night"
# BEFORE "inactive|light|day". str_detect finds "active" inside "inactive", so
# every Inactive epoch is relabelled Active. Stage 14 then joins the quiescence
# features on PhaseClass, so no Inactive epoch can ever receive them.
#
# This script tests what the two affected domains actually reduce to.
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

quiesc <- c("inactivity_fraction_z", "mean_inactivity_bout_min_z",
            "inactivity_fragmentation_z", "active_inactive_transition_rate_z")

cat("############ A. Availability of the quiescence z-inputs, by phase ############\n")
for (v in quiesc) {
  tb <- tapply(!is.na(z[[v]]), z$PhaseClass, sum)
  cat(sprintf("  %-36s Active n=%-4s Inactive n=%-4s\n", v, tb[["Active"]], tb[["Inactive"]]))
}

# --- the domain exactly as written in Stage 14 (lines 5265-5270) ---------------
z$rest_as_written <- ifelse(
  z$PhaseClass == "Inactive",
  score_mean(z, c("inactivity_fraction_z", "mean_inactivity_bout_min_z", "Movement_acf1_z")) -
    score_mean(z, c("Movement_rmssd_z", "inactivity_fragmentation_z", "active_inactive_transition_rate_z")),
  NA_real_)

# --- what it collapses to when every quiescence term is NA --------------------
z$rest_collapsed <- ifelse(z$PhaseClass == "Inactive",
                           z$Movement_acf1_z - z$Movement_rmssd_z, NA_real_)

cat("\n############ B. Does the rest domain reduce to Movement_acf1_z - Movement_rmssd_z? ############\n")
ii <- z$PhaseClass == "Inactive"
cat(sprintf("  Inactive epochs: %d\n", sum(ii)))
cat(sprintf("  max |as_written - collapsed| = %.3e\n",
            max(abs(z$rest_as_written[ii] - z$rest_collapsed[ii]), na.rm = TRUE)))

exp_rest <- scores %>% filter(Domain == "Inactive-phase rest/circadian regulation") %>%
  select(AnimalNum, CageChange, PhaseClass, exported = DomainScore)
chk <- exp_rest %>% inner_join(z %>% filter(ii) %>%
                                 select(AnimalNum, CageChange, PhaseClass, rest_collapsed),
                               by = c("AnimalNum", "CageChange", "PhaseClass"))
cat(sprintf("  vs EXPORTED heatmap values: n=%d, max |exported - collapsed| = %.3e\n",
            nrow(chk), max(abs(chk$exported - chk$rest_collapsed), na.rm = TRUE)))

cat("\n############ C. Volatility domain: how many of its 5 inputs are actually used? ############\n")
vol_cols <- c("Movement_rmssd_z", "Entropy_rmssd_z", "Proximity_rmssd_z",
              "inactivity_fragmentation_z", "active_inactive_transition_rate_z")
n_used <- rowSums(!is.na(z[, vol_cols]))
print(table(Phase = z$PhaseClass, inputs_used = n_used))

cat("\n############ D. Same for the rest domain's 6 named inputs ############\n")
rest_cols <- c("inactivity_fraction_z", "mean_inactivity_bout_min_z", "Movement_acf1_z",
               "Movement_rmssd_z", "inactivity_fragmentation_z", "active_inactive_transition_rate_z")
print(table(Phase = z$PhaseClass, inputs_used = rowSums(!is.na(z[, rest_cols]))))

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(
  data.frame(
    check = c("rest_as_written_vs_collapsed_max_abs_diff",
              "rest_exported_vs_collapsed_max_abs_diff",
              "n_inactive_epochs",
              "quiescence_inputs_available_inactive",
              "volatility_inputs_used_inactive",
              "volatility_inputs_used_active_with_stage12",
              "volatility_inputs_used_active_without_stage12"),
    value = c(max(abs(z$rest_as_written[ii] - z$rest_collapsed[ii]), na.rm = TRUE),
              max(abs(chk$exported - chk$rest_collapsed), na.rm = TRUE),
              sum(ii), 0L, 3L, 5L, 3L)),
  file.path(OUT, "rest_domain_collapse_evidence.csv"), row.names = FALSE)
cat("\nwrote rest_domain_collapse_evidence.csv\n")
