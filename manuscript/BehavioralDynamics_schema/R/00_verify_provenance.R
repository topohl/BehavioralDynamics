# ==============================================================================
# 00_verify_provenance.R
# Reproduce the Stage 14 transformation chain on the REAL exported tables, so
# every number printed in the schema figure can be traced to a file + a formula.
#
# Two things this script establishes:
#   (1) the SIS epoch backbone is 5-min, and the BinLevel column in
#       systems_sis_raw_phase_epoch_features.csv is a Stage-12 provenance tag
#       carried in by the left_join, not the backbone resolution;
#   (2) re-applying standardize_within_context() + the documented domain formula
#       to the exported raw features reproduces the exported DomainScore exactly,
#       which proves the worked example in the figure is genuine.
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr) })

DATA <- "C:/Users/topohl/AppData/Local/Temp/claude/c--Users-topohl-Documents-GitHub/77f6ce3c-a76b-4ee2-89c5-d1210af23338/scratchpad/data"
OUT  <- "C:/Users/topohl/Documents/GitHub/MMMSociability/manuscript/BehavioralDynamics_schema/data"

raw    <- read.csv(file.path(DATA, "systems_sis_raw_phase_epoch_features.csv"), check.names = FALSE)
scores <- read.csv(file.path(DATA, "systems_sis_domain_scores.csv"), check.names = FALSE)
eff    <- read.csv(file.path(DATA, "systems_sis_domain_effect_summary.csv"), check.names = FALSE)

cat("################ (1) BIN LEVEL ################\n")
amax <- max(raw$n_bins[raw$PhaseClass == "Active"])
imax <- max(raw$n_bins[raw$PhaseClass == "Inactive"])
cat(sprintf("Active  max n_bins = %d  = %g x 144 bins  (144 x 5 min = 12 h)\n", amax, amax / 144))
cat(sprintf("Inactive max n_bins = %d  = %g x 144 bins\n", imax, imax / 144))
cat("=> backbone bin = 5 min; a CC epoch pools 4 dark / 3 light 12-h phases.\n")
cat("BinLevel column:\n"); print(table(raw$BinLevel, useNA = "ifany"))

cat("\n################ (2) STAGE-12 COVERAGE ################\n")
has12 <- !is.na(raw$inactivity_fragmentation)
cat(sprintf("rows with Stage-12 features: %d / %d (%.1f%%)\n", sum(has12), nrow(raw), 100 * mean(has12)))
print(table(raw$PhaseClass, ifelse(has12, "stage12_present", "stage12_absent")))

# ---- Stage 14 helpers, copied verbatim from 14_..._dashboard.R -----------------
safe_scale <- function(x) {                                   # line 349
  s <- sd(x, na.rm = TRUE); m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}
standardize_within_context <- function(dat, value_col,        # line 5039
                                       group_cols = c("Sex", "PhaseClass", "CageChangeIndex")) {
  dat %>% group_by(across(any_of(group_cols))) %>%
    mutate("{value_col}_z" := safe_scale(as.numeric(.data[[value_col]]))) %>% ungroup()
}
score_mean <- function(dat, cols) {                           # line 5046
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

z <- Reduce(function(acc, cc) standardize_within_context(acc, cc),
            intersect(sis_z_cols, names(raw)), init = raw)

z$`Social spatial organization` <-
  score_mean(z, c("Proximity_mean_z", "Proximity_acf1_z")) - dplyr::coalesce(z$Proximity_rmssd_z, 0)
z$`Psychomotor activation` <- z$Movement_mean_z
z$`Behavioral flexibility / predictability` <-
  score_mean(z, c("Entropy_mean_z", "Entropy_rmssd_z")) - dplyr::coalesce(z$Entropy_acf1_z, 0)
z$`Behavioral volatility / fragmentation` <-
  score_mean(z, c("Movement_rmssd_z", "Entropy_rmssd_z", "Proximity_rmssd_z",
                  "inactivity_fragmentation_z", "active_inactive_transition_rate_z"))

cat("\n################ (3) CHAIN VALIDATION ################\n")
chk <- scores %>% filter(Domain == "Social spatial organization") %>%
  select(AnimalNum, CageChange, PhaseClass, exported = DomainScore) %>%
  inner_join(z %>% select(AnimalNum, CageChange, PhaseClass,
                          recomputed = `Social spatial organization`),
             by = c("AnimalNum", "CageChange", "PhaseClass"))
cat(sprintf("matched rows: %d ; max |exported - recomputed| = %.3e\n",
            nrow(chk), max(abs(chk$exported - chk$recomputed), na.rm = TRUE)))

cat("\n################ (4) WORKED EXAMPLE: animal 1545, CC1, Active ################\n")
ex <- z %>% filter(AnimalNum == 1545, CageChange == "CC1", PhaseClass == "Active")
show <- c("AnimalNum", "Group", "Sex", "CageChange", "PhaseClass", "n_bins",
          "Proximity_mean", "Proximity_rmssd", "Proximity_acf1",
          "Proximity_mean_z", "Proximity_rmssd_z", "Proximity_acf1_z",
          "Social spatial organization")
for (nm in show) cat(sprintf("  %-32s %s\n", nm, format(ex[[nm]][1], digits = 17)))
cat(sprintf("  %-32s %s\n", "exported DomainScore",
            format(scores$DomainScore[scores$AnimalNum == 1545 & scores$CageChange == "CC1" &
                                      scores$PhaseClass == "Active" &
                                      scores$Domain == "Social spatial organization"], digits = 17)))

ctx <- z %>% filter(Sex == ex$Sex[1], PhaseClass == "Active", CageChangeIndex == ex$CageChangeIndex[1])
cat(sprintf("\n  standardization context = Sex=%s x PhaseClass=Active x CageChangeIndex=%s : n = %d epochs\n",
            ex$Sex[1], ex$CageChangeIndex[1], nrow(ctx)))
for (v in c("Proximity_mean", "Proximity_rmssd", "Proximity_acf1")) {
  cat(sprintf("  context %-16s mean = %-20.15f sd = %.15f\n", v,
              mean(ctx[[v]], na.rm = TRUE), sd(ctx[[v]], na.rm = TRUE)))
}

cat("\n################ (5) TARGET TILE ################\n")
tile <- eff %>% filter(Domain == "Social spatial organization")
print(tile, digits = 17)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(ex[, show], file.path(OUT, "worked_example_animal1545_CC1_active.csv"), row.names = FALSE)
write.csv(tile, file.path(OUT, "tile_values_social_spatial_organization.csv"), row.names = FALSE)
write.csv(chk, file.path(OUT, "chain_validation_social_spatial_organization.csv"), row.names = FALSE)
cat("\nwrote provenance CSVs to", OUT, "\n")
