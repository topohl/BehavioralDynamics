## audit_first_night_domain_scores_v2.R
## ===========================================================================
## FIRST-NIGHT (CC1, canonical CLOCK window) domain matrix -- v2, SUPERSEDES
## Testing/audits/audit_first_night_domain_scores.R.
##
## WHY v2 EXISTS
##   v1 built the five RAW domains on Stage 14's production rule
##   `local_bin <= 12h/bin` (a fixed COUNT of Active bins, Analysis/14 lines 967-976).
##   That rule matches the canonical experimental-clock window for only 50/111 animals at
##   10-min and 33/111 at 5-min resolution: whenever an animal has missing bins in night 1
##   the bin COUNT over-reaches into the SECOND dark block of CC1. v2 rebuilds every raw
##   domain on the canonical clock window instead.
##
## THE CANONICAL FIRST-NIGHT WINDOW (reconstructed FROM CODE:
##   Analysis/09_early_prediction_model_ladder.R :: select_primary_active_window)
##   - restrict to the first cage change (CC1)
##   - keep rows whose Phase is EXACTLY one of c("active","dark","night") after
##     lower-case + trim (NEVER a substring regex: "inactive" contains "active")
##   - per SESSION (SourceFile): target_phase_block = min(animalpos_phase_block_index(BinStart))
##     target_window_start = target_phase_block * 43200 + 23400   (i.e. 18:30)
##   - keep difftime(BinStart, target_window_start, "secs") in [0, 12*3600)
##   The anchor is a property of the experimental CLOCK, not of any animal, and is never
##   shifted later to an animal's first read.
##
## INTERPRETATION GUARDS enforced throughout
##   - RFID proximity is a social-spatial CO-LOCATION proxy, NEVER "sociability".
##   - Occupancy composition carries NO temporal-order information; domain 6 is
##     "Latent-state occupancy organization", NEVER "temporal flexibility".
##   - RES/SUS are LATER phenotype labels derived from subsequent CombZ. Every first-night
##     contrast is a DESCRIPTIVE association with later phenotype -- never prospective,
##     never causal.
##   - No row is added because it is significant or dropped because it is null; no
##     resolution is chosen on p-values; the shipped composite coefficient 0.5 is KEPT.
##
## READ-ONLY with respect to Analysis/ and Functions/. Writes only into
##   <STAGE14>/audit_hmm_state_architecture/first_night_domain_heatmap/
## ===========================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble)
})

setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")

PROJ    <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
STAGE14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
OUT     <- file.path(STAGE14, "audit_hmm_state_architecture/first_night_domain_heatmap")
HMM     <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")
DERIV   <- file.path(PROJ, "analysis_ready/03_derived_metrics")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

THIS_SCRIPT  <- "Testing/audits/audit_first_night_domain_scores_v2.R"
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
WINDOW_HOURS <- 12
RESOLUTIONS  <- c("10min_based", "5min_based")   # 10min = PRIMARY, 5min = SENSITIVITY
BIN_SEC      <- c("10min_based" = 600, "5min_based" = 300)
RES_ROLE     <- c("10min_based" = "primary", "5min_based" = "sensitivity")

hr  <- function(x) cat("\n", strrep("=", 84), "\n", x, "\n", strrep("=", 84), "\n", sep = "")
sec <- function(x) cat("\n--- ", x, " ---\n", sep = "")
pf  <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
r4  <- function(x) round(x, 4)

## ---- local copies of small Stage 14 / Stage 09 helpers (production is read-only) ------
safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}
safe_cor_p <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor.test(x[ok], y[ok], method = method)$p.value)
}
## Verbatim Stage 09 get_first_cage_change (Analysis/09_early_prediction_model_ladder.R:288)
get_first_cage_change <- function(x) {
  ux <- unique(as.character(x))
  cc_num <- suppressWarnings(as.numeric(str_extract(ux, "[0-9]+")))
  if (any(is.finite(cc_num))) ux[which.min(ifelse(is.finite(cc_num), cc_num, Inf))] else sort(ux)[1]
}
## Stage 09's EXACT phase rule: explicit membership, never a substring regex.
ACTIVE_PHASE_VALUES <- c("active", "dark", "night")
is_active_phase <- function(x) str_to_lower(str_trim(as.character(x))) %in% ACTIVE_PHASE_VALUES
## Verbatim Stage 14 score_mean (Analysis/14 lines 5126-5132)
score_mean <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dat)))
  out <- rowMeans(as.matrix(dat[, cols, drop = FALSE]), na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }
## Contiguous-block id: boundary where diff(TimeIndex) > 1.5 * median step
add_block_id <- function(ti) {
  o <- order(ti); ti_s <- ti[o]; ut <- sort(unique(ti_s))
  step <- if (length(ut) > 1) stats::median(diff(ut)) else 1
  blk_s <- cumsum(c(1L, as.integer(diff(ti_s) > 1.5 * step)))
  out <- integer(length(ti)); out[o] <- blk_s; out
}
## Stage 14's animal-level raw summary estimators, verbatim semantics (Analysis/14:5519-5533)
f_mean  <- function(x) mean(x, na.rm = TRUE)
f_rmssd <- function(x) { xf <- x[is.finite(x)]; if (length(xf) >= 3) sqrt(mean(diff(xf)^2, na.rm = TRUE)) else NA_real_ }
f_acf1  <- function(x) { xf <- x[is.finite(x)]; n <- length(xf); if (n >= 4) safe_cor(xf[-n], xf[-1], "pearson") else NA_real_ }
## Viterbi-sequence metrics (same estimator as Testing/audits/audit_hmm_state_architecture_temporal_components.R)
seq_metrics <- function(s, K) {
  s <- as.integer(s); n <- length(s)
  occ <- tabulate(s, nbins = K) / n
  H <- ent(occ)
  if (n < 2) return(tibble(occupancy_entropy = H, state_switch_rate = NA_real_,
                           self_transition_probability = NA_real_, transition_entropy = NA_real_,
                           mean_dwell_bins = NA_real_, n_transitions = 0L, n_bins = n, occ = list(occ)))
  from <- s[-n]; to <- s[-1]; nt <- n - 1L
  TC <- matrix(0L, K, K)
  for (i in seq_len(nt)) TC[from[i], to[i]] <- TC[from[i], to[i]] + 1L
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  self_p <- sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0))
  runs <- rle(s)
  dw <- vapply(seq_len(K), function(k) { l <- runs$lengths[runs$values == k]; if (!length(l)) NA_real_ else mean(l) }, numeric(1))
  mean_dwell <- sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)])
  tibble(occupancy_entropy = H, state_switch_rate = mean(from != to),
         self_transition_probability = self_p, transition_entropy = sum(pi_s * rowH),
         mean_dwell_bins = mean_dwell, n_transitions = nt, n_bins = n, occ = list(occ))
}
zsex <- function(dat, col) strict_standardize_within_context(dat, col, group_cols = "Sex")

ASSERT <- list()
add_assert <- function(assertion, method, ok, evidence) {
  ASSERT[[length(ASSERT) + 1L]] <<- tibble(assertion = assertion, method = method,
                                           result = pf(ok), evidence = evidence)
  cat("  [", pf(ok), "] ", assertion, "  ::  ", evidence, "\n", sep = "")
  invisible(ok)
}

## ==========================================================================
hr("STEP 0. Canonical 111-animal roster + constants")
## ==========================================================================
cat("ANIMALPOS_INACTIVE_START_SEC =", ANIMALPOS_INACTIVE_START_SEC,
    "(", ANIMALPOS_INACTIVE_START_SEC / 3600, "h = 06:30 )\n")
cat("ANIMALPOS_PHASE_LENGTH_SEC   =", ANIMALPOS_PHASE_LENGTH_SEC,
    "(", ANIMALPOS_PHASE_LENGTH_SEC / 3600, "h )\n")

roster_src <- file.path(DERIV, "5min_based/all_behavior_metrics.csv")
roster <- build_canonical_identity_roster(
  read_csv(roster_src, col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                                        Group = col_character(), Sex = col_character()),
           progress = FALSE),
  "Stage 01 canonical roster (5min_based all_behavior_metrics.csv)"
)
cat("canonical roster animals:", nrow(roster), "\n")
print(as.data.frame(roster %>% count(Group, Sex) %>% arrange(Group, Sex)), row.names = FALSE)
stopifnot(nrow(roster) == 111L, nrow(roster) == n_distinct(roster$AnimalNum))

## ==========================================================================
hr("STEP 1. Domain source classification (A / B / C)")
## ==========================================================================
## Empirical evidence that the Stage 08 per-epoch tables are aggregated over the
## WHOLE ~48 h CC1 Active epoch (class C), not over the first night.
occ10 <- read_csv(file.path(HMM, "10min_based/tables/hmm_state_occupancy.csv"),
                  col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE)
occ_cc1a <- occ10 %>% filter(CageChange == "CC1", Phase == "Active")
occ_epoch_hours <- occ_cc1a %>% distinct(AnimalNum, active_duration_hours, observed_bins)
cat("Stage 08 hmm_state_occupancy.csv, CC1/Active epoch coverage per animal:\n")
cat("  active_duration_hours: median", r4(median(occ_epoch_hours$active_duration_hours, na.rm = TRUE)),
    " range [", r4(min(occ_epoch_hours$active_duration_hours, na.rm = TRUE)), ",",
    r4(max(occ_epoch_hours$active_duration_hours, na.rm = TRUE)), "]\n")
cat("  observed_bins        : median", median(occ_epoch_hours$observed_bins, na.rm = TRUE),
    " (12 h at 10 min would be 72)\n")
CLASS_C_EVIDENCE <- sprintf("CC1/Active epoch active_duration_hours median %.2f h, observed_bins median %s (>> 12 h / 72 bins)",
                            median(occ_epoch_hours$active_duration_hours, na.rm = TRUE),
                            median(occ_epoch_hours$observed_bins, na.rm = TRUE))

src_class <- tribble(
  ~Domain, ~upstream_table, ~upstream_script, ~class, ~evidence, ~action_taken,
  "Psychomotor activation",
  file.path(DERIV, "<res>/all_behavior_metrics.csv"), "Analysis/01_build_multiscale_behavior_metrics.R", "B",
  "Analysis/14_systems_neuroscience_summary_dashboard.R:693-695 (bin-level Movement/Entropy/Proximity); BinStart present in Stage 01 table",
  "REBUILT: bin-level rows restricted by BinStart to the canonical 12 h clock window, then Stage 14 formula (14:5553)",

  "Behavioral flexibility / predictability",
  file.path(DERIV, "<res>/all_behavior_metrics.csv"), "Analysis/01_build_multiscale_behavior_metrics.R", "B",
  "Analysis/14:5554 formula on bin-level Entropy summaries; bin-level input carries BinStart",
  "REBUILT on the canonical clock window; formula preserved verbatim",

  "Social spatial organization",
  file.path(DERIV, "<res>/all_behavior_metrics.csv"), "Analysis/01_build_multiscale_behavior_metrics.R", "B",
  "Analysis/14:5555 formula on bin-level ProximityFraction summaries (first_existing_col at 14:695)",
  "REBUILT on the canonical clock window; formula preserved verbatim; proximity = CO-LOCATION proxy",

  "Behavioral volatility / fragmentation",
  file.path(DERIV, "<res>/all_behavior_metrics.csv"), "Analysis/01_build_multiscale_behavior_metrics.R", "B",
  "Analysis/14:5556 first-active variant uses only the three RMSSD terms; the epoch variant at 14:5344 additionally uses inactivity_fragmentation_z and active_inactive_transition_rate_z from Stage 12 sleep-like features",
  "REBUILT on the canonical clock window using the THREE RMSSD terms only. The two Stage 12 sub-features are NOT computable inside a single Active window (they require Active<->Inactive alternation) and Stage 14's own first-active variant already omits them. Deviation recorded, not silently substituted.",

  "Active-phase adaptation/exploration",
  file.path(DERIV, "<res>/all_behavior_metrics.csv"), "Analysis/01_build_multiscale_behavior_metrics.R", "B",
  "Analysis/14:5557-5558 formula on bin-level mean/ACF1 summaries",
  "REBUILT on the canonical clock window; formula preserved verbatim",

  "Early adaptation / prediction",
  file.path(DERIV, "<res>/all_behavior_metrics.csv"), "Analysis/01_build_multiscale_behavior_metrics.R", "B",
  "Analysis/14:5364-5368 defines it as `Active-phase adaptation/exploration` restricted to min(CageChangeIndex); at CC1 the restriction is vacuous",
  "COMPUTED then EXCLUDED as mathematically identical to `Active-phase adaptation/exploration` (displayed = FALSE)",

  "Latent-state occupancy organization",
  file.path(HMM, "<res>/tables/hmm_state_assignments.csv"), "Analysis/08_hmm_behavioral_states_optional.R", "B",
  "Analysis/08:402-405 writes per-bin Viterbi State with AnimalNum/CageChange/Phase/TimeIndex; TimeIndex joins 1:1 to Stage 01 BinStart",
  "REBUILT from per-bin Viterbi states restricted to the canonical clock window; COMMON group-blind state space reused, nothing refitted",

  "Latent-state persistence",
  file.path(HMM, "<res>/tables/hmm_state_assignments.csv"), "Analysis/08_hmm_behavioral_states_optional.R", "B",
  "Analysis/08:402-405 per-bin Viterbi State sequence",
  "REBUILT from per-bin Viterbi states restricted to the canonical clock window; reported as mean_dwell_minutes (physical time)",

  "Top-proximity state occupancy (excluded)",
  file.path(HMM, "<res>/tables/hmm_state_assignments.csv"), "Analysis/08_hmm_behavioral_states_optional.R", "B",
  "Analysis/08:402-405 per-bin Viterbi State; argmax Proximity_z taken programmatically from hmm_state_summary.csv",
  "COMPUTED then EXCLUDED (displayed = FALSE, status excluded_failed_partition_robustness): across 5 distinct 10-min HMM optima the argmax-proximity state's Proximity_z spans 0.190-2.880 and its occupancy 0.029-0.564 (19-fold); animal-level r between optima as low as 0.117 (Active) / -0.157 (Inactive)",

  "Behavioral state architecture (Stage 14 shipped HMM domain)",
  file.path(STAGE14, "tables/systems_sis_domain_scores.csv"), "Analysis/14_systems_neuroscience_summary_dashboard.R", "C",
  sprintf("Functions/hmm_stage14_helpers.R:280-317 build_hmm_epoch_scores() aggregates hmm_state_occupancy.csv over AnimalNum x CageChange x PhaseClass. %s", CLASS_C_EVIDENCE),
  "NOT REUSED. Recomputed from per-bin Viterbi states inside the clock window as `Latent-state occupancy organization`.",

  "Stage 08 hmm_state_occupancy.csv",
  file.path(HMM, "<res>/tables/hmm_state_occupancy.csv"), "Analysis/08_hmm_behavioral_states_optional.R", "C",
  sprintf("Analysis/08:458-464 count(... CageChange, Phase, AnimalNum, State) -> frac_time over the whole epoch. %s", CLASS_C_EVIDENCE),
  "NOT REUSED as a first-night value; used only as class-C evidence and for identity auditing",

  "Stage 08 hmm_state_dwell_times.csv",
  file.path(HMM, "<res>/tables/hmm_state_dwell_times.csv"), "Analysis/08_hmm_behavioral_states_optional.R", "C",
  "Analysis/08:435-456 dwell runs computed over the whole AnimalNum x CageChange x Phase epoch (~48 h of CC1 Active)",
  "NOT REUSED. Dwell recomputed inside the clock window.",

  "Stage 08 hmm_transition_probabilities.csv",
  file.path(HMM, "<res>/tables/hmm_transition_probabilities.csv"), "Analysis/08_hmm_behavioral_states_optional.R", "C",
  "Analysis/08:419-433 transition counts over the whole AnimalNum x CageChange x Phase epoch",
  "NOT REUSED. Self-transition / transition entropy / switch rate recomputed inside the clock window.",

  "Stage 14 systems_sis_first_active_12h_domain_scores.csv",
  file.path(STAGE14, "tables/systems_sis_first_active_12h_domain_scores.csv"), "Analysis/14_systems_neuroscience_summary_dashboard.R", "C",
  "Analysis/14:967-976 builds `first_active` with group_by(AnimalNum, Phase) + local_bin <= early_window_bins (a fixed COUNT of bins), which over-reaches into the second dark block of CC1 whenever night-1 bins are missing",
  "NOT REUSED as a first-night value. Superseded: the count rule matches the clock window for only 50/111 (10 min) and 33/111 (5 min) animals.",

  "Stage 09 early_window_summary_by_animal.csv (on disk)",
  file.path(PROJ, "analysis_ready/06_behavioral_dynamics/early_prediction/10min_based/tables/early_window_summary_by_animal.csv"),
  "Analysis/_archive/08_early_prediction_models.R", "C",
  "Manifest dated May 18, written by the ARCHIVED predecessor: 113 animals, zero-padded IDs, BOTH Active and Inactive phases, all four cage changes, EarlyPhasePattern 'active|dark|night' (documented substring bug), n_early_bins = 4",
  "NOT REUSED -- STALE. The current Stage 09 window contract was reconstructed from CODE (select_primary_active_window). Reported as a finding; Stage 09 is NOT modified.",

  "Stage 12 sleep_like_inactivity_features.csv",
  file.path(PROJ, "analysis_ready/16_sleep_like_inactivity_metrics/<res>/tables/sleep_like_inactivity_features.csv"),
  "Analysis/12_sleep_like_quiescence_metrics.R", "C",
  "Joined into Stage 14 at 14:5320-5322 keyed on AnimalNum x CageChange x CageChangeIndex x PhaseClass -- one value per ~48 h epoch, and inactivity_fragmentation / active_inactive_transition_rate are undefined inside a single Active window",
  "NOT REUSED. Volatility rebuilt from the three RMSSD terms only (see that row)."
)
write_csv(src_class, file.path(OUT, "first_night_domain_source_classification.csv"))
cat("\nwrote first_night_domain_source_classification.csv  rows =", nrow(src_class), "\n")
print(as.data.frame(src_class %>% count(class)), row.names = FALSE)
print(as.data.frame(src_class %>% transmute(Domain = str_trunc(Domain, 46), class)), row.names = FALSE)

## ==========================================================================
hr("STEP 2a. Canonical window: reuse + re-derive the per-animal anchor audit")
## ==========================================================================
anchor_long_path <- file.path(OUT, "first_night_time_anchor_audit_long.csv")
stopifnot(file.exists(anchor_long_path))
anchor_long <- read_csv(anchor_long_path, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                        progress = FALSE)
cat("reused", basename(anchor_long_path), " rows =", nrow(anchor_long), "\n")
sec("Invariants of the existing anchor audit (asserted, not assumed)")
for (res in RESOLUTIONS) {
  a <- anchor_long %>% filter(resolution == res)
  cat(sprintf("  %-12s n=%3d  animals=%3d  all 12.0 h=%s  anchor start=%s -> %s  expected_bins=%s\n",
              res, nrow(a), n_distinct(a$AnimalNum), all(a$is_exactly_12h),
              paste(unique(format(a$target_window_start, "%H:%M")), collapse = "|"),
              paste(unique(format(a$target_window_end, "%H:%M")), collapse = "|"),
              paste(unique(a$expected_bins), collapse = "|")))
  cat(sprintf("               n_bins median=%s range=[%s,%s]  coverage median=%.4f min=%.4f\n",
              median(a$n_bins_window), min(a$n_bins_window), max(a$n_bins_window),
              median(a$coverage_fraction), min(a$coverage_fraction)))
  cat(sprintf("               block1_equals_window=%d/%d   localbin_equals_window=%d/%d\n",
              sum(a$block1_equals_window), nrow(a), sum(a$localbin_equals_window), nrow(a)))
  stopifnot(nrow(a) == 111L, all(a$is_exactly_12h), all(a$block1_equals_window))
}

## ==========================================================================
hr("STEP 2b. Re-derive the window from the bin-level tables (class B) and rebuild raw domains")
## ==========================================================================
win_store <- list(); anchor_store <- list(); geom_store <- list(); raw_feat <- list(); raw_dom <- list()
purity_store <- list()

for (res in RESOLUTIONS) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  sec(sprintf("%s  (bin %ds, role %s, expected slots %d)", res, bs, RES_ROLE[[res]], expected_slots))

  d <- read_csv(file.path(DERIV, res, "all_behavior_metrics.csv"),
                col_types = cols(AnimalNum = col_character(), BinStart = col_datetime(), .default = col_guess()),
                progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
    semi_join(roster, by = "AnimalNum") %>%
    mutate(Phase = as.character(Phase), CageChange = as.character(CageChange),
           TimeIndex = safe_numeric(TimeIndex),
           Movement = safe_numeric(Movement), Entropy = safe_numeric(Entropy),
           Proximity = safe_numeric(ProximityFraction))   # Stage 14 first_existing_col -> ProximityFraction
  cat("  bin-level rows on roster:", nrow(d), " animals:", n_distinct(d$AnimalNum), "\n")

  first_cc <- get_first_cage_change(d$CageChange)
  cc1 <- d %>% filter(CageChange == first_cc)
  act <- cc1 %>% filter(is_active_phase(Phase))
  cat("  first cage change =", first_cc, " | CC1 rows:", nrow(cc1),
      " | CC1 Active(exact-membership) rows:", nrow(act), " animals:", n_distinct(act$AnimalNum), "\n")
  cat("  CC1 phase values present:", paste(sort(unique(cc1$Phase)), collapse = ", "), "\n")

  anchors <- act %>%
    mutate(.blk = animalpos_phase_block_index(BinStart)) %>%
    group_by(.session = as.character(SourceFile)) %>%
    summarise(target_phase_block = min(.blk, na.rm = TRUE), .groups = "drop") %>%
    mutate(target_window_start = as.POSIXct(target_phase_block * ANIMALPOS_PHASE_LENGTH_SEC +
                                              ANIMALPOS_INACTIVE_START_SEC, origin = "1970-01-01", tz = "UTC"),
           target_window_end = target_window_start + WINDOW_HOURS * 3600,
           resolution = res)
  cat("  sessions:", nrow(anchors), "\n")
  print(as.data.frame(anchors %>% transmute(session = str_trunc(.session, 34), target_phase_block,
                                            start = format(target_window_start), end = format(target_window_end))),
        row.names = FALSE)
  anchor_store[[res]] <- anchors

  win <- act %>%
    mutate(.session = as.character(SourceFile)) %>%
    left_join(anchors %>% select(.session, target_window_start, target_window_end), by = ".session") %>%
    mutate(elapsed_sec_in_window = as.numeric(difftime(BinStart, target_window_start, units = "secs"))) %>%
    filter(elapsed_sec_in_window >= 0, elapsed_sec_in_window < WINDOW_HOURS * 3600) %>%
    arrange(AnimalNum, TimeIndex)
  cat("  window rows:", nrow(win), " animals:", n_distinct(win$AnimalNum), "\n")
  win_store[[res]] <- win

  ## ---- cross-check against the reused anchor audit -----------------------
  chk <- win %>% count(AnimalNum, name = "n_bins_rederived") %>%
    full_join(anchor_long %>% filter(resolution == res) %>% select(AnimalNum, n_bins_window), by = "AnimalNum")
  n_disagree <- sum(chk$n_bins_rederived != chk$n_bins_window, na.rm = TRUE) +
    sum(is.na(chk$n_bins_rederived)) + sum(is.na(chk$n_bins_window))
  add_assert(sprintf("[%s] re-derived window == reused first_night_time_anchor_audit_long.csv", res),
             "per-animal n_bins compared between re-derivation and the stored audit",
             n_disagree == 0L,
             sprintf("%d/%d animals disagree on n_bins_window", n_disagree, nrow(chk)))

  ## ---- window purity / block geometry -----------------------------------
  actg <- act %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(block = add_block_id(TimeIndex), local_bin = row_number()) %>% ungroup()
  geom <- actg %>% group_by(AnimalNum) %>%
    summarise(n_cc1_active_bins = n(), n_active_blocks = n_distinct(block),
              n_block1 = sum(block == 1L),
              cc1_active_hours = n() * bs / 3600, .groups = "drop") %>%
    mutate(resolution = res)
  cat("  CC1 Active blocks per animal:\n"); print(table(geom$n_active_blocks))
  cat("  CC1 Active total hours: median", r4(median(geom$cc1_active_hours)),
      " range [", r4(min(geom$cc1_active_hours)), ",", r4(max(geom$cc1_active_hours)), "]\n")
  geom_store[[res]] <- geom

  wend <- win %>% group_by(AnimalNum) %>%
    summarise(min_BinStart = min(BinStart), max_BinStart = max(BinStart),
              win_start = first(target_window_start), win_end = first(target_window_end),
              n_bins = n(), max_step = if (n() > 1) max(diff(sort(TimeIndex))) else NA_real_,
              all_cc1 = all(CageChange == first_cc),
              all_active = all(is_active_phase(Phase)), .groups = "drop") %>%
    mutate(resolution = res,
           inside_window = max_BinStart < win_end & min_BinStart >= win_start,
           last_bin_end_le_window_end = (max_BinStart + bs) <= win_end)
  purity_store[[res]] <- wend

  add_assert(sprintf("[%s] every contributing bin has BinStart in [window_start, window_start+12h)", res),
             "per-animal min/max BinStart vs the session anchor bounds",
             all(wend$inside_window),
             sprintf("%d/%d animals inside; global BinStart span %s -> %s",
                     sum(wend$inside_window), nrow(wend), format(min(win$BinStart)), format(max(win$BinStart))))
  add_assert(sprintf("[%s] no bin from a LATER Active block within CC1", res),
             "max(BinStart) + bin_size <= window_end per animal",
             all(wend$last_bin_end_le_window_end),
             sprintf("%d/%d animals; CC1 has median %s Active blocks totalling median %.2f h, of which we keep median %d bins (%.2f h)",
                     sum(wend$last_bin_end_le_window_end), nrow(wend),
                     median(geom$n_active_blocks), median(geom$cc1_active_hours),
                     median(wend$n_bins), median(wend$n_bins) * bs / 3600))
  add_assert(sprintf("[%s] no bin from CC2/CC3/CC4", res),
             "all(CageChange == first_cage_change) over every contributing row",
             all(wend$all_cc1) && identical(sort(unique(win$CageChange)), first_cc),
             sprintf("unique CageChange in window = {%s}; first_cage_change = %s",
                     paste(unique(win$CageChange), collapse = ","), first_cc))
  add_assert(sprintf("[%s] no Inactive/light/day bin (exact membership, not substring)", res),
             "all(Phase in c('active','dark','night')) after lower+trim",
             all(wend$all_active),
             sprintf("unique Phase in window = {%s}", paste(unique(win$Phase), collapse = ",")))
  add_assert(sprintf("[%s] window is a single contiguous run of bins (no internal gap)", res),
             "max(diff(sort(TimeIndex))) per animal inside the window",
             all(wend$max_step == 1, na.rm = TRUE),
             sprintf("max within-window TimeIndex step across animals = %s (1 = contiguous); n animals with step>1 = %d",
                     max(wend$max_step, na.rm = TRUE), sum(wend$max_step > 1, na.rm = TRUE)))

  ## ---- animal-level raw features INSIDE the window ----------------------
  ## Built from a window frame stripped of EVERY outcome/phenotype column first, so that
  ## Group/Sex/CombZ provably cannot enter feature construction (verified below).
  win_blind <- win %>% select(AnimalNum, TimeIndex, Movement, Entropy, Proximity)
  add_assert(sprintf("[%s] feature-construction frame contains NO outcome/phenotype column", res),
             "names() of the frame actually passed to the summarise() that builds the features",
             !any(str_detect(str_to_lower(names(win_blind)), "combz|group|sex|phenotype|outcome|resil|suscep")),
             sprintf("columns used for features = {%s}; no CombZ/Group/Sex/phenotype column present in ANY input table read here (checked: %s)",
                     paste(names(win_blind), collapse = ", "),
                     if (any(str_detect(str_to_lower(names(win)), "combz"))) "CombZ FOUND" else "no CombZ column exists in all_behavior_metrics.csv"))
  feat_blind <- win_blind %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    summarise(Movement_mean = f_mean(Movement), Movement_rmssd = f_rmssd(Movement), Movement_acf1 = f_acf1(Movement),
              Entropy_mean  = f_mean(Entropy),  Entropy_rmssd  = f_rmssd(Entropy),  Entropy_acf1  = f_acf1(Entropy),
              Proximity_mean = f_mean(Proximity), Proximity_rmssd = f_rmssd(Proximity), Proximity_acf1 = f_acf1(Proximity),
              n_bins = n(), .groups = "drop")
  feat <- feat_blind %>%
    left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum") %>%   # Group/Sex joined AFTER features
    mutate(resolution = res, expected_bins = expected_slots, coverage_fraction = n_bins / expected_slots)
  cat("  animal-level raw feature rows:", nrow(feat), "\n")
  miss <- feat %>% summarise(across(c(Movement_mean, Movement_rmssd, Movement_acf1, Entropy_mean, Entropy_rmssd,
                                      Entropy_acf1, Proximity_mean, Proximity_rmssd, Proximity_acf1),
                                    ~ sum(!is.finite(.x))))
  cat("  non-finite counts per feature:\n"); print(as.data.frame(miss), row.names = FALSE)
  nofin <- feat %>% filter(!is.finite(Proximity_mean)) %>% select(AnimalNum, Group, Sex, n_bins)
  if (nrow(nofin) > 0) {
    cat("  animals with NO finite ProximityFraction in the window (co-location proxy unavailable):\n")
    print(as.data.frame(nofin), row.names = FALSE)
  }
  raw_feat[[res]] <- feat

  ## ---- Stage 14 formulas, z within SEX ONLY -----------------------------
  zcols <- c("Movement_mean","Movement_rmssd","Movement_acf1","Entropy_mean","Entropy_rmssd",
             "Entropy_acf1","Proximity_mean","Proximity_rmssd","Proximity_acf1")
  fz <- reduce(zcols, zsex, .init = feat)
  scored <- fz %>% mutate(
    `Psychomotor activation` = Movement_mean_z,
    `Behavioral flexibility / predictability` =
      score_mean(pick(everything()), c("Entropy_mean_z","Entropy_rmssd_z")) - coalesce(Entropy_acf1_z, 0),
    `Social spatial organization` =
      score_mean(pick(everything()), c("Proximity_mean_z","Proximity_acf1_z")) - coalesce(Proximity_rmssd_z, 0),
    `Behavioral volatility / fragmentation` =
      score_mean(pick(everything()), c("Movement_rmssd_z","Entropy_rmssd_z","Proximity_rmssd_z")),
    `Active-phase adaptation/exploration` =
      score_mean(pick(everything()), c("Movement_mean_z","Entropy_mean_z","Proximity_mean_z")) -
      score_mean(pick(everything()), c("Movement_acf1_z","Entropy_acf1_z")),
    `Early adaptation / prediction` = `Active-phase adaptation/exploration`
  )
  raw_dom[[res]] <- scored
  cat("  domain score summaries (z within Sex only):\n")
  print(as.data.frame(scored %>% select(`Psychomotor activation`:`Early adaptation / prediction`) %>%
    pivot_longer(everything(), names_to = "Domain", values_to = "v") %>%
    group_by(Domain) %>% summarise(n = sum(is.finite(v)), mean = r4(mean(v, na.rm = TRUE)),
                                   sd = r4(sd(v, na.rm = TRUE)), min = r4(min(v, na.rm = TRUE)),
                                   max = r4(max(v, na.rm = TRUE)), .groups = "drop")), row.names = FALSE)
}

sec("Duplicate-domain check: `Early adaptation / prediction` vs `Active-phase adaptation/exploration`")
dup_chk <- map_dfr(RESOLUTIONS, function(res) {
  s <- raw_dom[[res]]
  a <- s$`Active-phase adaptation/exploration`; b <- s$`Early adaptation / prediction`
  tibble(resolution = res, n = sum(is.finite(a) & is.finite(b)),
         max_abs_diff = max(abs(a - b), na.rm = TRUE), pearson_r = safe_cor(a, b, "pearson"),
         verdict = if (max(abs(a - b), na.rm = TRUE) < 1e-12) "mathematically_identical" else "differs",
         action = "kept only `Active-phase adaptation/exploration`; Early adaptation / prediction displayed = FALSE",
         reason = "Analysis/14:5364-5368 defines Early adaptation / prediction as Active-phase adaptation/exploration restricted to min(CageChangeIndex); the CC1-only window makes that restriction vacuous")
})
print(as.data.frame(dup_chk %>% select(resolution, n, max_abs_diff, pearson_r, verdict)), row.names = FALSE)
write_csv(dup_chk, file.path(OUT, "first_night_duplicate_domain_check_v2.csv"))

## ==========================================================================
hr("STEP 3. HMM domains on the SAME window (COMMON group-blind state space)")
## ==========================================================================
hmm_feat <- list(); hmm_dom <- list(); eq_assert <- list(); state_sem <- list(); hmm_cov <- list()

for (res in RESOLUTIONS) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  sec(sprintf("%s HMM", res))

  ss <- read_csv(file.path(HMM, res, "tables/hmm_state_summary.csv"), col_types = cols(), progress = FALSE)
  lab <- annotate_hmm_semantic_states(ss, res)
  K <- nrow(lab)
  cat("  common state space, K =", K, "\n")
  print(as.data.frame(lab %>% transmute(State, Movement_z = r4(Movement_z), Entropy_z = r4(Entropy_z),
                                        Proximity_z = r4(Proximity_z), SemanticState)), row.names = FALSE)
  inactive_states <- lab$State[lab$SemanticState == "inactive/low-exploration"]
  social_states   <- lab$State[lab$SemanticState == "social"]
  top_prox_state  <- lab$State[which.max(lab$Proximity_z)]     # derived programmatically
  cat("  inactive/low-exploration states: {", paste(inactive_states, collapse = ","),
      "} | social states: {", paste(social_states, collapse = ","),
      "} | argmax-Proximity_z state: S", top_prox_state,
      " (Proximity_z = ", r4(max(lab$Proximity_z)), ")\n", sep = "")
  state_sem[[res]] <- lab %>% mutate(is_inactive_semantic = State %in% inactive_states,
                                     is_argmax_proximity = State == top_prox_state)

  asg <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                  col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE)
  aud <- audit_hmm_identity(asg, roster, paste("hmm_state_assignments", res))
  assert_hmm_identity_audit(aud)
  cat("  identity audit ASSERTED for hmm_state_assignments (", res, ")\n", sep = "")
  asg <- aud$data %>% mutate(AnimalNum = as.character(AnimalNum), Phase = as.character(Phase),
                             CageChange = as.character(CageChange), TimeIndex = safe_numeric(TimeIndex),
                             State = as.integer(State))
  cat("  assignment rows:", nrow(asg), " animals:", n_distinct(asg$AnimalNum),
      " CCs:", paste(sort(unique(asg$CageChange)), collapse = ","),
      " phases:", paste(sort(unique(asg$Phase)), collapse = ","), "\n")

  first_cc <- get_first_cage_change(asg$CageChange)
  hcc1 <- asg %>% filter(CageChange == first_cc, is_active_phase(Phase))
  cat("  CC1 Active assignment rows:", nrow(hcc1), " animals:", n_distinct(hcc1$AnimalNum), "\n")

  ## --- HMM first-night coverage vs the 111-animal roster -----------------
  ## The HMM CC1 Active epoch is NOT guaranteed to cover all 111 animals: Stage 08 applies a
  ## fail-closed epoch-level data-quality exclusion. Report which animals and why.
  hmm_missing <- setdiff(roster$AnimalNum, unique(hcc1$AnimalNum))
  epoch_ex <- read_csv(file.path(HMM, res, "tables/hmm_epoch_data_quality_exclusions.csv"),
                       col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum))
  ex_cc1 <- epoch_ex %>% filter(AnimalNum %in% hmm_missing, CageChange == first_cc, is_active_phase(Phase))
  cov_tbl <- tibble(resolution = res, n_roster = nrow(roster),
                    n_animals_hmm_cc1_active = n_distinct(hcc1$AnimalNum),
                    n_missing = length(hmm_missing),
                    missing_animals = paste(sort(hmm_missing), collapse = "|"),
                    missing_groups = paste(sort(roster$Group[roster$AnimalNum %in% hmm_missing]), collapse = "|"),
                    missing_sexes = paste(sort(roster$Sex[roster$AnimalNum %in% hmm_missing]), collapse = "|"),
                    exclusion_documented_in_stage08 = nrow(ex_cc1) == length(hmm_missing),
                    exclusion_reason = paste(unique(ex_cc1$exclusion_reason), collapse = " | "),
                    is_identity_loss = FALSE,
                    note = "Stage 08 fail-closed epoch-level data-quality exclusion, NOT identity/ID-format loss. These animals have zero complete Movement/Entropy/Proximity bins in CC1 (dyadic_observation_seconds == 0), so their latent-state sequence does not exist for the first night.")
  hmm_cov[[res]] <- cov_tbl
  if (length(hmm_missing) > 0) {
    cat("  *** HMM first-night coverage is", n_distinct(hcc1$AnimalNum), "/", nrow(roster),
        "-- missing:", paste(sort(hmm_missing), collapse = ", "), "\n")
    print(as.data.frame(ex_cc1 %>% select(AnimalNum, Group, Sex, CageChange, Phase, input_bins,
                                          complete_hmm_bins, retained_for_hmm, exclusion_reason)), row.names = FALSE)
  }
  add_assert(sprintf("[%s] every HMM animal missing from the first night is a DOCUMENTED Stage 08 data-quality exclusion (not identity loss)", res),
             "setdiff(roster, HMM CC1-Active animals) matched against hmm_epoch_data_quality_exclusions.csv",
             cov_tbl$exclusion_documented_in_stage08,
             sprintf("HMM CC1-Active covers %d/%d roster animals; missing = {%s} (%s); Stage 08 reason = '%s'",
                     cov_tbl$n_animals_hmm_cc1_active, nrow(roster), cov_tbl$missing_animals,
                     cov_tbl$missing_groups, cov_tbl$exclusion_reason))

  ## --- attach BinStart from the class-B bin-level table (TimeIndex is the shared key) ---
  keys <- win_store[[res]] %>% select(AnimalNum, CageChange, Phase, TimeIndex, BinStart,
                                      target_window_start, target_window_end, elapsed_sec_in_window)
  hwin <- hcc1 %>% inner_join(keys, by = c("AnimalNum", "CageChange", "Phase", "TimeIndex"))
  cat("  HMM rows inside the CLOCK window:", nrow(hwin), " animals:", n_distinct(hwin$AnimalNum), "\n")

  ## --- (i) clock window vs first-contiguous-block, on ROW SETS -----------
  hblk <- hcc1 %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(block = add_block_id(TimeIndex)) %>% ungroup() %>% filter(block == 1L)
  set_clock <- hwin %>% transmute(AnimalNum, TimeIndex) %>% arrange(AnimalNum, TimeIndex)
  set_block <- hblk %>% transmute(AnimalNum, TimeIndex) %>% arrange(AnimalNum, TimeIndex)
  only_clock <- anti_join(set_clock, set_block, by = c("AnimalNum", "TimeIndex"))
  only_block <- anti_join(set_block, set_clock, by = c("AnimalNum", "TimeIndex"))
  n_animals_differ <- n_distinct(c(only_clock$AnimalNum, only_block$AnimalNum))
  cat("  (i) row-set comparison: clock-only rows =", nrow(only_clock),
      " block1-only rows =", nrow(only_block), " animals differing =", n_animals_differ, "\n")

  ## --- Stage 14 production count rule, for the record --------------------
  hlb <- hcc1 %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(local_bin = row_number()) %>% filter(local_bin <= expected_slots) %>% ungroup()
  set_lb <- hlb %>% transmute(AnimalNum, TimeIndex) %>% arrange(AnimalNum, TimeIndex)
  lb_only <- anti_join(set_lb, set_clock, by = c("AnimalNum", "TimeIndex"))
  lb_animals_differ <- hcc1 %>% distinct(AnimalNum) %>%
    left_join(set_lb %>% count(AnimalNum, name = "n_lb"), by = "AnimalNum") %>%
    left_join(set_clock %>% count(AnimalNum, name = "n_cl"), by = "AnimalNum") %>%
    mutate(same = coalesce(n_lb, 0L) == coalesce(n_cl, 0L))
  cat("  local_bin<=", expected_slots, " rule: rows outside the clock window =", nrow(lb_only),
      "; animals matching the clock window =", sum(lb_animals_differ$same), "/", nrow(lb_animals_differ), "\n", sep = "")

  ## --- (ii) internal gaps ------------------------------------------------
  gaps <- hwin %>% group_by(AnimalNum) %>% summarise(max_step = if (n() > 1) max(diff(sort(TimeIndex))) else NA_real_,
                                                     n = n(), .groups = "drop")
  cat("  (ii) max diff(TimeIndex) inside the window: max over animals =", max(gaps$max_step, na.rm = TRUE),
      "; animals with step > 1 =", sum(gaps$max_step > 1, na.rm = TRUE), "\n")

  eq_assert[[res]] <- tibble(
    resolution = res, bin_size_sec = bs, expected_bins = expected_slots,
    n_animals_clock = n_distinct(set_clock$AnimalNum), n_rows_clock = nrow(set_clock),
    n_animals_block1 = n_distinct(set_block$AnimalNum), n_rows_block1 = nrow(set_block),
    n_rows_clock_only = nrow(only_clock), n_rows_block1_only = nrow(only_block),
    n_animals_row_sets_differ = n_animals_differ,
    assertion_i_clock_equals_first_contiguous_block = pf(nrow(only_clock) == 0 && nrow(only_block) == 0),
    max_TimeIndex_step_in_window = max(gaps$max_step, na.rm = TRUE),
    n_animals_with_internal_gap = sum(gaps$max_step > 1, na.rm = TRUE),
    assertion_ii_no_internal_gap = pf(all(gaps$max_step == 1, na.rm = TRUE)),
    n_rows_localbin_rule_outside_clock_window = nrow(lb_only),
    n_animals_localbin_rule_matches_clock = sum(lb_animals_differ$same),
    n_bins_median = median(gaps$n), n_bins_min = min(gaps$n), n_bins_max = max(gaps$n),
    coverage_median = median(gaps$n) / expected_slots, coverage_min = min(gaps$n) / expected_slots
  )

  ## --- per-animal Viterbi-sequence features INSIDE the window -----------
  ## Strip every outcome/phenotype column BEFORE feature construction (verified).
  hwin_blind <- hwin %>% select(AnimalNum, TimeIndex, State)
  add_assert(sprintf("[%s HMM] feature-construction frame contains NO outcome/phenotype column", res),
             "names() of the frame actually passed to the Viterbi-sequence feature builder",
             !any(str_detect(str_to_lower(names(hwin_blind)), "combz|group|sex|phenotype|outcome")),
             sprintf("columns used for HMM features = {%s}; Group/Sex are dropped before scoring and re-joined from the roster afterwards",
                     paste(names(hwin_blind), collapse = ", ")))
  hf <- hwin_blind %>% arrange(AnimalNum, TimeIndex) %>% group_by(AnimalNum) %>%
    group_modify(~ {
      m <- seq_metrics(.x$State, K)
      occv <- m$occ[[1]]
      m %>% select(-occ) %>%
        mutate(inactive_state_fraction = sum(occv[as.integer(inactive_states)]),
               social_state_fraction = if (length(social_states)) sum(occv[as.integer(social_states)]) else 0,
               top_proximity_state_fraction = occv[as.integer(top_prox_state)])
    }) %>% ungroup() %>%
    mutate(bin_size_sec = bs, mean_dwell_minutes = mean_dwell_bins * bs / 60,
           resolution = res, expected_bins = expected_slots, coverage_fraction = n_bins / expected_slots) %>%
    left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum")   # Group/Sex joined AFTER features
  cat("  per-animal HMM feature rows:", nrow(hf), "\n")
  print(as.data.frame(hf %>% select(occupancy_entropy, inactive_state_fraction, social_state_fraction,
                                    top_proximity_state_fraction, self_transition_probability,
                                    transition_entropy, state_switch_rate, mean_dwell_minutes,
                                    n_bins, n_transitions) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "v") %>% group_by(feature) %>%
    summarise(n = sum(is.finite(v)), mean = r4(mean(v, na.rm = TRUE)), sd = r4(sd(v, na.rm = TRUE)),
              min = r4(min(v, na.rm = TRUE)), max = r4(max(v, na.rm = TRUE)), .groups = "drop")), row.names = FALSE)

  ## redundancy identities (report, do not display three rows)
  cat("  identity check  max|state_switch_rate - (1 - self_transition_probability)| =",
      format(max(abs(hf$state_switch_rate - (1 - hf$self_transition_probability)), na.rm = TRUE), scientific = TRUE), "\n")
  cat("  cor(mean_dwell_bins, 1/(1-P_self)) =", r4(safe_cor(hf$mean_dwell_bins, 1 / (1 - hf$self_transition_probability))), "\n")
  cat("  social_state_fraction: all exactly 0? ", all(hf$social_state_fraction == 0), "\n")
  hmm_feat[[res]] <- hf

  ## --- domains 6 and 7, z within SEX ONLY -------------------------------
  hz <- reduce(c("occupancy_entropy", "inactive_state_fraction", "social_state_fraction",
                 "mean_dwell_minutes", "top_proximity_state_fraction"), zsex, .init = hf)
  hz <- hz %>% mutate(
    `Latent-state occupancy organization` = 0.5 * occupancy_entropy_z - inactive_state_fraction_z,
    shipped_3component_form = rowMeans(cbind(occupancy_entropy_z, social_state_fraction_z), na.rm = FALSE) -
      inactive_state_fraction_z,
    `Latent-state persistence` = mean_dwell_minutes_z,
    `Top-proximity state occupancy (excluded)` = top_proximity_state_fraction_z
  )
  cat("  FORMULA PRESERVATION: max|0.5*z(H) - z(inact)  minus  mean(z(H), z(social)) - z(inact)| =",
      format(max(abs(hz$`Latent-state occupancy organization` - hz$shipped_3component_form), na.rm = TRUE),
             scientific = TRUE), "\n")
  sens <- hz$occupancy_entropy_z - hz$inactive_state_fraction_z
  cat("  (sensitivity construct, NOT displayed) max|1.0*z(H)-z(inact) - shipped| =",
      r4(max(abs(sens - hz$`Latent-state occupancy organization`), na.rm = TRUE)),
      " r =", r4(safe_cor(sens, hz$`Latent-state occupancy organization`)),
      " var ratio =", r4(var(sens, na.rm = TRUE) / var(hz$`Latent-state occupancy organization`, na.rm = TRUE)), "\n")
  hmm_dom[[res]] <- hz
}

eq_tbl <- bind_rows(eq_assert)
write_csv(eq_tbl, file.path(OUT, "first_night_hmm_window_equivalence_assertions.csv"))
sec("first_night_hmm_window_equivalence_assertions.csv")
print(as.data.frame(eq_tbl %>% select(resolution, n_animals_clock, n_rows_clock, n_rows_clock_only,
                                      n_rows_block1_only, n_animals_row_sets_differ,
                                      assertion_i_clock_equals_first_contiguous_block,
                                      max_TimeIndex_step_in_window, assertion_ii_no_internal_gap,
                                      n_animals_localbin_rule_matches_clock)), row.names = FALSE)
write_csv(bind_rows(state_sem), file.path(OUT, "first_night_hmm_state_semantics_v2.csv"))
write_csv(bind_rows(hmm_feat) %>% relocate(AnimalNum, Group, Sex, resolution),
          file.path(OUT, "first_night_hmm_component_features_v2.csv"))

## (iii) same animal set across resolutions
a10 <- sort(unique(hmm_feat[["10min_based"]]$AnimalNum)); a05 <- sort(unique(hmm_feat[["5min_based"]]$AnimalNum))
sec("Cross-resolution animal-set assertion (iii)")
cov_all <- bind_rows(hmm_cov)
write_csv(cov_all, file.path(OUT, "first_night_hmm_coverage_v2.csv"))
print(as.data.frame(cov_all %>% select(resolution, n_roster, n_animals_hmm_cc1_active, n_missing,
                                       missing_animals, exclusion_reason)), row.names = FALSE)
add_assert("(iii) 10-min and 5-min HMM windows yield the SAME animal set",
           "setdiff both ways on AnimalNum",
           identical(a10, a05),
           sprintf("n(10min)=%d n(5min)=%d identical=%s; setdiff sizes %d / %d. NOTE the set has %d animals, NOT 111: OQ770 and OQ771 (SUS Male, batch B1) have zero complete Movement/Entropy/Proximity bins in CC1 and were excluded by Stage 08's fail-closed epoch data-quality rule. RAW domains still cover 111/111.",
                   length(a10), length(a05), identical(a10, a05),
                   length(setdiff(a10, a05)), length(setdiff(a05, a10)), length(a10)))
add_assert("RAW-domain first-night coverage is the full 111-animal roster",
           "n_distinct(AnimalNum) in the re-derived clock window at both resolutions",
           all(map_int(win_store, ~ n_distinct(.x$AnimalNum)) == 111L),
           sprintf("window animals: 10min = %d, 5min = %d, of roster 111",
                   n_distinct(win_store[["10min_based"]]$AnimalNum),
                   n_distinct(win_store[["5min_based"]]$AnimalNum)))
add_assert("(i) clock window row set == first-contiguous-block row set, all animals, both resolutions",
           "anti_join on (AnimalNum, TimeIndex) in both directions",
           all(eq_tbl$n_animals_row_sets_differ == 0),
           sprintf("animals differing: %s; rows clock-only %s; rows block1-only %s",
                   paste(eq_tbl$n_animals_row_sets_differ, collapse = "/"),
                   paste(eq_tbl$n_rows_clock_only, collapse = "/"),
                   paste(eq_tbl$n_rows_block1_only, collapse = "/")))
add_assert("(ii) no internal gap inside any animal's window (max diff(TimeIndex) == 1)",
           "max(diff(sort(TimeIndex))) per animal, both resolutions",
           all(eq_tbl$assertion_ii_no_internal_gap == "PASS"),
           sprintf("max step = %s; animals with gap = %s",
                   paste(eq_tbl$max_TimeIndex_step_in_window, collapse = "/"),
                   paste(eq_tbl$n_animals_with_internal_gap, collapse = "/")))
add_assert("HMM state space is the COMMON group-blind longitudinal fit; nothing was refitted",
           "depmixS4 namespace is never loaded in this session (a refit is impossible without it); states are read from hmm_state_assignments.csv",
           !("depmixS4" %in% loadedNamespaces()),
           sprintf("depmixS4 loaded = %s. States read from disk only. Analysis/08:305-311 fits the emission model on list(Movement_z ~ 1, Entropy_z ~ 1, Proximity_z ~ 1) with NO Group/Sex term; Analysis/08:382 records GroupBlind = TRUE; Group/Sex first appear at Analysis/08:419-464, i.e. post-inference aggregation only.",
                   "depmixS4" %in% loadedNamespaces()))
gs_free <- map_lgl(RESOLUTIONS, function(res) {
  ## Re-derive the raw features from a Group/Sex-free frame and compare to the shipped features.
  fb <- win_store[[res]] %>% select(AnimalNum, TimeIndex, Movement, Entropy, Proximity) %>%
    group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    summarise(Movement_mean = f_mean(Movement), Entropy_mean = f_mean(Entropy),
              Proximity_mean = f_mean(Proximity), Movement_acf1 = f_acf1(Movement), .groups = "drop")
  cmp <- raw_feat[[res]] %>% select(AnimalNum, Movement_mean, Entropy_mean, Proximity_mean, Movement_acf1) %>%
    inner_join(fb, by = "AnimalNum", suffix = c("_ship", "_blind"))
  all(abs(cmp$Movement_mean_ship - cmp$Movement_mean_blind) < 1e-12, na.rm = TRUE) &&
    all(abs(cmp$Entropy_mean_ship - cmp$Entropy_mean_blind) < 1e-12, na.rm = TRUE) &&
    all(abs(cmp$Movement_acf1_ship - cmp$Movement_acf1_blind) < 1e-12, na.rm = TRUE)
})
add_assert("no outcome/phenotype variable enters FEATURE construction",
           "features re-derived from a frame containing ONLY AnimalNum/TimeIndex/Movement/Entropy/Proximity and compared value-by-value to the shipped features",
           all(gs_free),
           "Group/Sex-blind re-derivation reproduces the shipped features to < 1e-12 at both resolutions. Group and Sex are left_join()ed from the canonical roster strictly AFTER the animal-level summarise(). Sex is then used SOLELY as the standardization stratum -- justified because inside a single CC1 Active epoch no Sex x PhaseClass x CageChangeIndex context remains, and Sex is a design variable fixed before the experiment rather than an outcome. Group enters only the downstream lm(DomainScore ~ Group*Sex). CombZ is never read: all_behavior_metrics.csv and hmm_state_assignments.csv contain no CombZ column.")
add_assert("canonical roster is exactly 111 animals",
           "build_canonical_identity_roster() on Stage 01 5min_based all_behavior_metrics.csv",
           nrow(roster) == 111L, sprintf("nrow(roster) = %d", nrow(roster)))
add_assert("identity audits asserted on every HMM table used",
           "audit_hmm_identity() + assert_hmm_identity_audit() on hmm_state_assignments.csv at both resolutions",
           TRUE, "assert_hmm_identity_audit() called for 10min_based and 5min_based; no error raised")
add_assert("canonical_animal_id() applied to every AnimalNum",
           "canonical_animal_id() on the bin-level tables; audit_hmm_identity() canonicalizes the HMM tables",
           TRUE, "applied at read time for both resolutions of all_behavior_metrics.csv and via audit_hmm_identity()$data for hmm_state_assignments.csv")

## ==========================================================================
hr("STEP 4. Leakage / provenance assertion table")
## ==========================================================================
leak <- bind_rows(ASSERT)
write_csv(leak, file.path(OUT, "first_night_leakage_assertions.csv"))
cat("\nwrote first_night_leakage_assertions.csv  rows =", nrow(leak),
    " PASS =", sum(leak$result == "PASS"), " FAIL =", sum(leak$result == "FAIL"), "\n")
if (any(leak$result == "FAIL")) print(as.data.frame(leak %>% filter(result == "FAIL")), row.names = FALSE)

## ==========================================================================
hr("STEP 5. Assemble first_night_domain_scores.csv")
## ==========================================================================
DOM_META <- tribble(
  ~Domain, ~feature_origin, ~displayed, ~status, ~display_order,
  "Psychomotor activation", "raw_RFID", TRUE, "displayed", 1L,
  "Behavioral flexibility / predictability", "raw_RFID", TRUE, "displayed", 2L,
  "Social spatial organization", "raw_RFID", TRUE, "displayed", 3L,
  "Behavioral volatility / fragmentation", "raw_RFID", TRUE, "displayed_with_documented_formula_deviation", 4L,
  "Active-phase adaptation/exploration", "raw_RFID", TRUE, "displayed", 5L,
  "Latent-state occupancy organization", "HMM_derived", TRUE, "displayed", 6L,
  "Latent-state persistence", "HMM_derived", TRUE, "displayed", 7L,
  "Early adaptation / prediction", "raw_RFID", FALSE, "excluded_mathematically_identical_to_active_phase_adaptation", 8L,
  "Top-proximity state occupancy (excluded)", "HMM_derived", FALSE, "excluded_failed_partition_robustness", 9L
)
RAW_DOMS <- DOM_META$Domain[DOM_META$feature_origin == "raw_RFID"]
HMM_DOMS <- DOM_META$Domain[DOM_META$feature_origin == "HMM_derived"]

scores <- map_dfr(RESOLUTIONS, function(res) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  anc <- anchor_store[[res]]
  win_desc <- sprintf("CC1 first dark/Active block, experimental-clock anchored: [target_window_start, +12h); target_window_start = target_phase_block*43200 + 23400 = %s clock, end %s; session-specific dates %s",
                      paste(unique(format(anc$target_window_start, "%H:%M")), collapse = "|"),
                      paste(unique(format(anc$target_window_end, "%H:%M")), collapse = "|"),
                      paste(sort(unique(format(anc$target_window_start, "%Y-%m-%d"))), collapse = ","))
  rawl <- raw_dom[[res]] %>% select(AnimalNum, Group, Sex, n_bins, coverage_fraction, all_of(RAW_DOMS)) %>%
    pivot_longer(all_of(RAW_DOMS), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(source_table = file.path(DERIV, res, "all_behavior_metrics.csv"),
           source_script = "Analysis/01_build_multiscale_behavior_metrics.R (bin-level input); formulas from Analysis/14_systems_neuroscience_summary_dashboard.R:5553-5558",
           aggregation_level = "bin-level rows -> one animal-level value per domain")
  hmml <- hmm_dom[[res]] %>% select(AnimalNum, Group, Sex, n_bins, coverage_fraction, all_of(HMM_DOMS)) %>%
    pivot_longer(all_of(HMM_DOMS), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(source_table = file.path(HMM, res, "tables/hmm_state_assignments.csv"),
           source_script = "Analysis/08_hmm_behavioral_states_optional.R (per-bin Viterbi states, COMMON group-blind fit); composite formula from Functions/hmm_stage14_helpers.R:280-317",
           aggregation_level = "per-bin Viterbi state sequence -> one animal-level value per domain")
  bind_rows(rawl, hmml) %>%
    left_join(DOM_META, by = "Domain") %>%
    mutate(bin_resolution = res, resolution_role = RES_ROLE[[res]], bin_size_sec = bs,
           cage_change = "CC1", phase_window = win_desc,
           expected_bins = expected_slots,
           standardization = "z within SEX ONLY (no PhaseClass x CageChangeIndex context remains inside a single CC1 Active epoch); strict_standardize_within_context(group_cols='Sex')",
           window_start_rule = "Stage 09 select_primary_active_window: per-session min(animalpos_phase_block_index(BinStart)) over CC1 rows whose Phase is EXACTLY in c('active','dark','night'); start = block*43200 + 23400; keep elapsed in [0, 43200)",
           window_hours = WINDOW_HOURS,
           script = THIS_SCRIPT,
           supersedes = "Testing/audits/audit_first_night_domain_scores.R (raw domains built on Stage 14 local_bin<=12h/bin count rule)",
           interpretation_guard = case_when(
             Domain == "Social spatial organization" ~ "RFID proximity = social-spatial CO-LOCATION proxy, never sociability",
             Domain == "Latent-state occupancy organization" ~ "occupancy composition carries NO temporal-order information; never 'temporal flexibility'",
             Domain == "Latent-state persistence" ~ "sign convention: HIGHER = longer mean dwell = MORE persistent / less switching",
             Domain == "Top-proximity state occupancy (excluded)" ~ "never call this 'social'; fails partition robustness across HMM optima",
             TRUE ~ "RES/SUS are LATER phenotype labels from subsequent CombZ; all contrasts are DESCRIPTIVE associations with later phenotype"
           ))
}) %>%
  left_join(src_class %>% select(Domain, source_class = class), by = "Domain") %>%
  arrange(bin_resolution, display_order, AnimalNum) %>%
  select(AnimalNum, Group, Sex, Domain, DomainScore, bin_resolution, resolution_role, bin_size_sec,
         cage_change, phase_window, window_start_rule, window_hours, expected_bins, n_bins,
         coverage_fraction, aggregation_level, feature_origin, source_class, source_table, source_script,
         standardization, displayed, status, display_order, interpretation_guard, script, supersedes)

write_csv(scores, file.path(OUT, "first_night_domain_scores.csv"))
cat("wrote first_night_domain_scores.csv  rows =", nrow(scores), "\n")
print(as.data.frame(scores %>% count(bin_resolution, Domain, displayed) %>% arrange(bin_resolution, desc(displayed), Domain)), row.names = FALSE)

sec("Domain score distributions (both resolutions)")
print(as.data.frame(scores %>% group_by(bin_resolution, Domain, displayed) %>%
  summarise(n = sum(is.finite(DomainScore)), mean = r4(mean(DomainScore, na.rm = TRUE)),
            sd = r4(sd(DomainScore, na.rm = TRUE)), .groups = "drop") %>%
  arrange(bin_resolution, Domain)), row.names = FALSE)

## ==========================================================================
hr("STEP 5b. first_night_group_sex_n.csv")
## ==========================================================================
gsn <- scores %>% filter(is.finite(DomainScore)) %>%
  group_by(bin_resolution, Domain, Group, Sex) %>% summarise(n_animals = n_distinct(AnimalNum), .groups = "drop")
gsn_overall <- scores %>% filter(is.finite(DomainScore), displayed) %>%
  distinct(bin_resolution, AnimalNum, Group, Sex) %>% count(bin_resolution, Group, Sex, name = "n_animals") %>%
  mutate(Domain = "ALL_DISPLAYED_DOMAINS")
gsn_out <- bind_rows(gsn_overall %>% select(bin_resolution, Domain, Group, Sex, n_animals), gsn) %>%
  mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS)) %>%
  arrange(bin_resolution, Domain, Sex, Group)
write_csv(gsn_out, file.path(OUT, "first_night_group_sex_n.csv"))
sec("n per Group x Sex on this window (all displayed domains)")
print(as.data.frame(gsn_overall %>% arrange(bin_resolution, Sex, Group)), row.names = FALSE)
print(as.data.frame(gsn_overall %>% filter(bin_resolution == "10min_based") %>%
  select(-Domain) %>% pivot_wider(names_from = Group, values_from = n_animals)), row.names = FALSE)

## ==========================================================================
hr("STEP 6. first_night_domain_provenance.csv")
## ==========================================================================
prov <- map_dfr(RESOLUTIONS, function(res) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  s <- scores %>% filter(bin_resolution == res)
  s %>% group_by(Domain) %>%
    summarise(n_animals = sum(is.finite(DomainScore)),
              median_observed_bins = median(n_bins, na.rm = TRUE),
              min_observed_bins = min(n_bins, na.rm = TRUE),
              max_observed_bins = max(n_bins, na.rm = TRUE),
              median_coverage_fraction = median(coverage_fraction, na.rm = TRUE),
              min_coverage_fraction = min(coverage_fraction, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(s %>% distinct(Domain, feature_origin, source_class, source_table, source_script,
                             aggregation_level, standardization, phase_window, window_start_rule,
                             window_hours, cage_change, displayed, status, display_order,
                             interpretation_guard), by = "Domain") %>%
    mutate(bin_resolution = res, resolution_role = RES_ROLE[[res]], bin_size_sec = bs,
           expected_bins = expected_slots)
}) %>%
  left_join(src_class %>% select(Domain, upstream_table, upstream_script, class_evidence = evidence,
                                 action_taken), by = "Domain") %>%
  mutate(
    hmm_vs_raw = if_else(feature_origin == "HMM_derived", "HMM_derived (latent Viterbi states)", "raw RFID bin-level"),
    score_formula = case_when(
      Domain == "Psychomotor activation" ~ "Movement_mean_z",
      Domain == "Behavioral flexibility / predictability" ~ "mean(Entropy_mean_z, Entropy_rmssd_z) - coalesce(Entropy_acf1_z, 0)",
      Domain == "Social spatial organization" ~ "mean(Proximity_mean_z, Proximity_acf1_z) - coalesce(Proximity_rmssd_z, 0)   [Proximity = ProximityFraction]",
      Domain == "Behavioral volatility / fragmentation" ~ "mean(Movement_rmssd_z, Entropy_rmssd_z, Proximity_rmssd_z)",
      Domain == "Active-phase adaptation/exploration" ~ "mean(Movement_mean_z, Entropy_mean_z, Proximity_mean_z) - mean(Movement_acf1_z, Entropy_acf1_z)",
      Domain == "Latent-state occupancy organization" ~ "0.5 * z(occupancy_entropy) - z(inactive_state_fraction)   [== shipped mean(z(H), z(social)) - z(inactive) because z(social) is exactly 0]",
      Domain == "Latent-state persistence" ~ "z(mean_dwell_minutes)   [higher = more persistent]",
      Domain == "Early adaptation / prediction" ~ "identical to `Active-phase adaptation/exploration` at CC1",
      Domain == "Top-proximity state occupancy (excluded)" ~ "z(occupancy of the argmax-Proximity_z state)",
      TRUE ~ NA_character_),
    formula_deviation_from_stage14 = case_when(
      Domain == "Behavioral volatility / fragmentation" ~
        "Stage 14's EPOCH score (14:5344) also averages inactivity_fragmentation_z and active_inactive_transition_rate_z (Stage 12 sleep-like features). Both are undefined inside a single Active window (they need Active<->Inactive alternation), so this score contains ONLY the three RMSSD terms - exactly as Stage 14's own first-active variant at 14:5556. Documented, not silently substituted.",
      Domain == "Latent-state occupancy organization" ~
        "Coefficient 0.5 on z(occupancy_entropy) is KEPT. The alternative 1.0*z(H) - z(inactive) is a DIFFERENT construct (r ~ 0.95, variance ~1.6x) and is not displayed.",
      Domain == "Latent-state persistence" ~
        "Reported as mean_dwell_minutes (physical time) so 10-min and 5-min are comparable. state_switch_rate and self_transition_probability are NOT displayed as separate domains: state_switch_rate == 1 - self_transition_probability exactly, and dwell is near-deterministically related to 1/(1-P_self).",
      TRUE ~ NA_character_),
    coverage_caveat = case_when(
      feature_origin == "HMM_derived" ~
        "109/111 animals. OQ770 and OQ771 (SUS Male, batch B1) have zero complete Movement/Entropy/Proximity bins in CC1 (dyadic_observation_seconds == 0) and were removed by Stage 08's fail-closed epoch data-quality rule ('fewer than 4 complete Movement/Entropy/Proximity bins'). This is a data-quality exclusion, NOT identity loss.",
      Domain == "Social spatial organization" ~
        "111 rows but 109 finite values: OQ770 and OQ771 have no finite ProximityFraction in the window, so all three Proximity sub-features are NA and the score is NA.",
      Domain %in% c("Behavioral volatility / fragmentation", "Active-phase adaptation/exploration",
                    "Early adaptation / prediction") ~
        "111/111 finite, but for OQ770 and OQ771 the value rests on fewer sub-features because Stage 14's score_mean() uses na.rm = TRUE and their Proximity terms are NA. Stage 14 behaviour preserved verbatim; flagged rather than changed.",
      TRUE ~ "111/111 animals, all values finite.")
  ) %>%
  arrange(bin_resolution, display_order) %>%
  select(Domain, bin_resolution, resolution_role, bin_size_sec, cage_change, phase_window,
         window_start_rule, window_hours, expected_bins, median_observed_bins, min_observed_bins,
         max_observed_bins, median_coverage_fraction, min_coverage_fraction, n_animals,
         aggregation_level, hmm_vs_raw, feature_origin, source_class, source_table, source_script,
         upstream_table, upstream_script, class_evidence, action_taken, score_formula,
         formula_deviation_from_stage14, coverage_caveat, standardization, displayed, status,
         interpretation_guard)
write_csv(prov, file.path(OUT, "first_night_domain_provenance.csv"))
cat("wrote first_night_domain_provenance.csv  rows =", nrow(prov), "\n")
print(as.data.frame(prov %>% filter(bin_resolution == "10min_based") %>%
  transmute(Domain = str_trunc(Domain, 42), cls = source_class, exp = expected_bins,
            med = median_observed_bins, cov = r4(median_coverage_fraction), n = n_animals,
            disp = displayed)), row.names = FALSE)

## ==========================================================================
hr("STEP 7. Redundancy audit")
## ==========================================================================
ALL_DOMS <- DOM_META$Domain
wide_by_res <- map(set_names(RESOLUTIONS), function(res) {
  scores %>% filter(bin_resolution == res) %>%
    select(AnimalNum, Group, Sex, Domain, DomainScore) %>%
    pivot_wider(names_from = Domain, values_from = DomainScore)
})
class_of <- function(r) {
  a <- abs(r)
  if (!is.finite(a)) return(NA_character_)
  if (a > 0.999) "mathematically_identical" else if (a > 0.95) "near_deterministic" else
    if (a > 0.8) "strongly_redundant" else if (a > 0.5) "moderately_related" else "largely_independent"
}
red <- map_dfr(RESOLUTIONS, function(res) {
  w <- wide_by_res[[res]]
  strata <- c(list(pooled = w), split(w, w$Sex))
  map_dfr(names(strata), function(st) {
    dd <- strata[[st]]
    cmb <- t(combn(ALL_DOMS, 2))
    map_dfr(seq_len(nrow(cmb)), function(i) {
      x <- dd[[cmb[i, 1]]]; y <- dd[[cmb[i, 2]]]
      ok <- is.finite(x) & is.finite(y)
      pr <- safe_cor(x, y, "pearson"); sp <- safe_cor(x, y, "spearman")
      tibble(bin_resolution = res, stratum = st, domain_a = cmb[i, 1], domain_b = cmb[i, 2],
             n = sum(ok), pearson_r = pr, pearson_p = safe_cor_p(x, y, "pearson"),
             spearman_rho = sp, spearman_p = safe_cor_p(x, y, "spearman"),
             redundancy_class_pearson = class_of(pr), redundancy_class_spearman = class_of(sp))
    })
  })
}) %>%
  mutate(
    must_not_both_be_displayed = redundancy_class_pearson == "mathematically_identical",
    dropped_member = case_when(
      domain_a == "Early adaptation / prediction" | domain_b == "Early adaptation / prediction" ~ "Early adaptation / prediction",
      TRUE ~ NA_character_),
    drop_reason = case_when(
      !is.na(dropped_member) ~ "duplicate of `Active-phase adaptation/exploration` by construction (Analysis/14:5364-5368)",
      TRUE ~ NA_character_)
  )
write_csv(red, file.path(OUT, "first_night_domain_redundancy_audit.csv"))
cat("wrote first_night_domain_redundancy_audit.csv  rows =", nrow(red), "\n")

sec("Pooled Pearson, 10min_based (PRIMARY) -- all candidate domain pairs")
print(as.data.frame(red %>% filter(bin_resolution == "10min_based", stratum == "pooled") %>%
  transmute(a = str_trunc(domain_a, 34), b = str_trunc(domain_b, 34), n,
            r = r4(pearson_r), rho = r4(spearman_rho), class = redundancy_class_pearson) %>%
  arrange(desc(abs(r)))), row.names = FALSE)

sec("Pairs that MUST NOT both be displayed")
mm <- red %>% filter(must_not_both_be_displayed)
if (nrow(mm) == 0) cat("  none\n") else
  print(as.data.frame(mm %>% transmute(bin_resolution, stratum, a = str_trunc(domain_a, 36),
                                       b = str_trunc(domain_b, 36), r = r4(pearson_r),
                                       dropped = dropped_member)), row.names = FALSE)
cat("\nACTION: `Early adaptation / prediction` dropped (displayed = FALSE). ",
    "`Top-proximity state occupancy` dropped for partition-robustness, not redundancy.\n", sep = "")

## locomotion dominance vs Psychomotor activation, repo threshold |rho| >= 0.70
loco <- red %>% filter(domain_a == "Psychomotor activation" | domain_b == "Psychomotor activation") %>%
  transmute(bin_resolution, stratum,
            Domain = if_else(domain_a == "Psychomotor activation", domain_b, domain_a),
            n, pearson_r, spearman_rho, spearman_p,
            locomotion_dominance_flag = abs(spearman_rho) >= 0.70,
            threshold = "repo standard |rho| >= 0.70") %>%
  group_by(bin_resolution, stratum) %>% mutate(spearman_fdr = p.adjust(spearman_p, "BH")) %>% ungroup()
write_csv(loco, file.path(OUT, "first_night_locomotion_dominance_v2.csv"))
sec("Locomotion dominance vs Psychomotor activation (|rho| >= 0.70)")
print(as.data.frame(loco %>% filter(stratum == "pooled") %>%
  transmute(res = bin_resolution, Domain = str_trunc(Domain, 40), n, rho = r4(spearman_rho),
            fdr = signif(spearman_fdr, 3), flag = locomotion_dominance_flag) %>%
  arrange(res, desc(abs(rho)))), row.names = FALSE)

## ==========================================================================
hr("STEP 8. CC1-only contrast model  DomainScore ~ Group * Sex  (descriptive)")
## ==========================================================================
stopifnot(requireNamespace("emmeans", quietly = TRUE))
CONTRASTS <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))
fit_one <- function(dd, dom, res) {
  m <- dd %>% filter(Domain == dom, is.finite(DomainScore)) %>%
    mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS))
  if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
  fit <- stats::lm(DomainScore ~ Group * Sex, data = m)
  em <- emmeans::emmeans(fit, ~ Group | Sex)
  ct <- as.data.frame(summary(emmeans::contrast(em, method = CONTRASTS, adjust = "none"),
                              infer = c(TRUE, TRUE)))
  out <- ct %>% as_tibble() %>%
    transmute(bin_resolution = res, Domain = dom, Sex = as.character(Sex),
              contrast = as.character(contrast), estimate, SE, df,
              ci_low = lower.CL, ci_high = upper.CL, t_ratio = t.ratio, p_value = p.value)
  ref_of <- c("RES-CON" = "CON", "SUS-CON" = "CON", "SUS-RES" = "RES")
  cmp_of <- c("RES-CON" = "RES", "SUS-CON" = "SUS", "SUS-RES" = "SUS")
  out %>% rowwise() %>%
    mutate(group_ref = ref_of[[contrast]], group_comp = cmp_of[[contrast]],
           n_ref = sum(as.character(m$Group) == group_ref & as.character(m$Sex) == Sex),
           n_comp = sum(as.character(m$Group) == group_comp & as.character(m$Sex) == Sex),
           hedges_g = hmm_hedges_g(m$DomainScore[as.character(m$Group) == group_ref & as.character(m$Sex) == Sex],
                                   m$DomainScore[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex])) %>%
    ungroup()
}
contr <- map_dfr(RESOLUTIONS, function(res) {
  dd <- scores %>% filter(bin_resolution == res)
  map_dfr(ALL_DOMS, ~ fit_one(dd, .x, res))
}) %>%
  left_join(DOM_META %>% select(Domain, displayed, status, feature_origin), by = "Domain") %>%
  group_by(bin_resolution, Sex) %>%
  mutate(family_id = sprintf("FIRST_NIGHT__%s__displayed_domains_x_3_contrasts", Sex),
         p_fdr = p.adjust(if_else(displayed, p_value, NA_real_), "BH")) %>%
  ungroup() %>%
  mutate(ci_formula = "emmeans 95% CI: estimate +/- qt(0.975, df) * SE from lm(DomainScore ~ Group*Sex)",
         model = "lm(DomainScore ~ Group * Sex); ONE value per animal per domain; no random effect, no repeated measures",
         fdr_note = "BH within Sex over DISPLAYED domains x 3 contrasts; excluded domains carry NA p_fdr and are exploratory only",
         interpretation = "DESCRIPTIVE association of a first-night measure with LATER phenotype label (RES/SUS derived from subsequent CombZ). Not prospective, not causal.")
write_csv(contr, file.path(OUT, "first_night_domain_contrasts_v2.csv"))
cat("wrote first_night_domain_contrasts_v2.csv  rows =", nrow(contr), "\n")

sec("PRIMARY (10min_based), displayed domains, Group contrasts by Sex")
print(as.data.frame(contr %>% filter(bin_resolution == "10min_based", displayed) %>%
  transmute(Domain = str_trunc(Domain, 34), Sex, contrast, est = r4(estimate), SE = r4(SE),
            ci = sprintf("[%.2f,%.2f]", ci_low, ci_high), g = r4(hedges_g),
            p = signif(p_value, 3), fdr = signif(p_fdr, 3), n = sprintf("%d/%d", n_ref, n_comp)) %>%
  arrange(Sex, Domain, contrast)), row.names = FALSE)

## Group:Sex interaction, SEPARATE table, uncorrected
inter <- map_dfr(RESOLUTIONS, function(res) {
  dd <- scores %>% filter(bin_resolution == res)
  map_dfr(ALL_DOMS, function(dom) {
    m <- dd %>% filter(Domain == dom, is.finite(DomainScore)) %>%
      mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS))
    if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
    av <- stats::anova(stats::lm(DomainScore ~ Group * Sex, data = m))
    tibble(bin_resolution = res, Domain = dom, term = "Group:Sex",
           df = av["Group:Sex", "Df"], df_resid = av["Residuals", "Df"],
           F_value = av["Group:Sex", "F value"], p_value = av["Group:Sex", "Pr(>F)"],
           n_animals = nrow(m))
  })
}) %>% left_join(DOM_META %>% select(Domain, displayed), by = "Domain") %>%
  mutate(multiplicity_treatment = "UNCORRECTED; exactly one Group:Sex test per domain per resolution; reported separately from the contrast FDR family and NOT part of it")
write_csv(inter, file.path(OUT, "first_night_group_sex_interaction_v2.csv"))
sec("Group:Sex interaction (uncorrected, one test per domain)")
print(as.data.frame(inter %>% filter(bin_resolution == "10min_based") %>%
  transmute(Domain = str_trunc(Domain, 42), F = r4(F_value), p = signif(p_value, 3),
            n = n_animals, disp = displayed)), row.names = FALSE)

## ==========================================================================
hr("STEP 9. Cross-resolution + first-night vs longitudinal persistence sanity")
## ==========================================================================
dw <- bind_rows(hmm_feat) %>% select(AnimalNum, resolution, mean_dwell_minutes) %>%
  pivot_wider(names_from = resolution, values_from = mean_dwell_minutes)
cat("mean_dwell_minutes inside the first-night window:\n")
cat("  10min: mean", r4(mean(dw$`10min_based`, na.rm = TRUE)), " median", r4(median(dw$`10min_based`, na.rm = TRUE)), "\n")
cat("  5min : mean", r4(mean(dw$`5min_based`, na.rm = TRUE)), " median", r4(median(dw$`5min_based`, na.rm = TRUE)), "\n")
cat("  pct difference 10min vs 5min:", r4(100 * (mean(dw$`10min_based`, na.rm = TRUE) / mean(dw$`5min_based`, na.rm = TRUE) - 1)), "%\n")
cat("  cor(animal-level) r =", r4(safe_cor(dw$`10min_based`, dw$`5min_based`)),
    " rho =", r4(safe_cor(dw$`10min_based`, dw$`5min_based`, "spearman")), "\n")

xres <- scores %>% filter(displayed) %>% select(AnimalNum, Domain, bin_resolution, DomainScore) %>%
  pivot_wider(names_from = bin_resolution, values_from = DomainScore) %>%
  group_by(Domain) %>% summarise(n = sum(is.finite(`10min_based`) & is.finite(`5min_based`)),
                                 pearson_r = r4(safe_cor(`10min_based`, `5min_based`)),
                                 spearman_rho = r4(safe_cor(`10min_based`, `5min_based`, "spearman")),
                                 .groups = "drop")
sec("Cross-resolution stability of the displayed domain scores (10min PRIMARY vs 5min SENSITIVITY)")
print(as.data.frame(xres %>% mutate(Domain = str_trunc(Domain, 44))), row.names = FALSE)
write_csv(xres, file.path(OUT, "first_night_cross_resolution_stability_v2.csv"))

sec("Female Active persistence: first night vs the longitudinal CC1-CC4 result")
fem <- contr %>% filter(bin_resolution == "10min_based", Sex == "Female",
                        Domain == "Latent-state persistence") %>%
  transmute(contrast, est = r4(estimate), SE = r4(SE), g = r4(hedges_g), p = signif(p_value, 3),
            fdr = signif(p_fdr, 3), n = sprintf("%d/%d", n_ref, n_comp))
print(as.data.frame(fem), row.names = FALSE)
cat("Longitudinal reference (10min, Female, Active, context-z, CC1-CC4 repeated measures):\n")
cat("  mean_dwell SUS-CON +0.639 (p 0.015); occupancy_entropy SUS-CON -0.138 (p 0.562, null).\n")
cat("  The first-night value above is the CC1-only, clock-window, Sex-standardized analogue.\n")

## ==========================================================================
hr("STEP 10. How much did superseding the v1 count rule actually change the raw domains?")
## ==========================================================================
## Rebuild the five raw domains on Stage 14's `local_bin <= 12h/bin` COUNT rule (what v1 used)
## and compare, animal by animal, against the clock-window scores. This quantifies the defect;
## the count-rule scores are NOT written as a domain and are NOT displayed anywhere.
v1_cmp <- map_dfr(RESOLUTIONS, function(res) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  d <- read_csv(file.path(DERIV, res, "all_behavior_metrics.csv"),
                col_types = cols(AnimalNum = col_character(), BinStart = col_datetime(), .default = col_guess()),
                progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>% semi_join(roster, by = "AnimalNum") %>%
    mutate(Movement = safe_numeric(Movement), Entropy = safe_numeric(Entropy),
           Proximity = safe_numeric(ProximityFraction), TimeIndex = safe_numeric(TimeIndex))
  first_cc <- get_first_cage_change(d$CageChange)
  lb <- d %>% filter(as.character(CageChange) == first_cc, is_active_phase(Phase)) %>%
    group_by(AnimalNum, Phase) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(local_bin = row_number()) %>% filter(local_bin <= expected_slots) %>% ungroup()
  ## how far past the window does the count rule reach?
  reach <- lb %>% left_join(win_store[[res]] %>% distinct(AnimalNum, target_window_end), by = "AnimalNum") %>%
    group_by(AnimalNum) %>% summarise(max_BinStart = max(BinStart), win_end = first(target_window_end),
                                      overshoot_hours = as.numeric(difftime(max(BinStart) + bs, first(target_window_end), units = "hours")),
                                      .groups = "drop")
  f <- lb %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    summarise(Movement_mean = f_mean(Movement), Movement_rmssd = f_rmssd(Movement), Movement_acf1 = f_acf1(Movement),
              Entropy_mean = f_mean(Entropy), Entropy_rmssd = f_rmssd(Entropy), Entropy_acf1 = f_acf1(Entropy),
              Proximity_mean = f_mean(Proximity), Proximity_rmssd = f_rmssd(Proximity), Proximity_acf1 = f_acf1(Proximity),
              .groups = "drop") %>% left_join(roster %>% select(AnimalNum, Sex), by = "AnimalNum")
  fz <- reduce(c("Movement_mean","Movement_rmssd","Movement_acf1","Entropy_mean","Entropy_rmssd",
                 "Entropy_acf1","Proximity_mean","Proximity_rmssd","Proximity_acf1"), zsex, .init = f) %>%
    mutate(`Psychomotor activation` = Movement_mean_z,
           `Behavioral flexibility / predictability` = score_mean(pick(everything()), c("Entropy_mean_z","Entropy_rmssd_z")) - coalesce(Entropy_acf1_z, 0),
           `Social spatial organization` = score_mean(pick(everything()), c("Proximity_mean_z","Proximity_acf1_z")) - coalesce(Proximity_rmssd_z, 0),
           `Behavioral volatility / fragmentation` = score_mean(pick(everything()), c("Movement_rmssd_z","Entropy_rmssd_z","Proximity_rmssd_z")),
           `Active-phase adaptation/exploration` = score_mean(pick(everything()), c("Movement_mean_z","Entropy_mean_z","Proximity_mean_z")) -
             score_mean(pick(everything()), c("Movement_acf1_z","Entropy_acf1_z"))) %>%
    select(AnimalNum, all_of(setdiff(RAW_DOMS, "Early adaptation / prediction"))) %>%
    pivot_longer(-AnimalNum, names_to = "Domain", values_to = "score_countrule_v1")
  cat(sprintf("  %s: count rule reaches past the window end for %d/111 animals; max overshoot %.2f h (median overshoot among those %.2f h)\n",
              res, sum(reach$overshoot_hours > 0), max(reach$overshoot_hours),
              median(reach$overshoot_hours[reach$overshoot_hours > 0])))
  scores %>% filter(bin_resolution == res, feature_origin == "raw_RFID",
                    Domain != "Early adaptation / prediction") %>%
    select(AnimalNum, Domain, score_clock_v2 = DomainScore) %>%
    inner_join(fz, by = c("AnimalNum", "Domain")) %>%
    group_by(Domain) %>%
    summarise(bin_resolution = res, n = sum(is.finite(score_clock_v2) & is.finite(score_countrule_v1)),
              n_animals_score_changed = sum(abs(score_clock_v2 - score_countrule_v1) > 1e-9, na.rm = TRUE),
              max_abs_diff = max(abs(score_clock_v2 - score_countrule_v1), na.rm = TRUE),
              median_abs_diff = median(abs(score_clock_v2 - score_countrule_v1), na.rm = TRUE),
              pearson_r = safe_cor(score_clock_v2, score_countrule_v1),
              spearman_rho = safe_cor(score_clock_v2, score_countrule_v1, "spearman"),
              .groups = "drop")
}) %>% mutate(note = "score_countrule_v1 reproduces what Testing/audits/audit_first_night_domain_scores.R used (Stage 14 local_bin <= 12h/bin). It is NOT a displayed domain; shown only to quantify the defect that v2 corrects.")
write_csv(v1_cmp, file.path(OUT, "first_night_v1_countrule_vs_v2_clockwindow.csv"))
sec("Clock window (v2, shipped) vs count rule (v1, superseded) -- raw domains")
print(as.data.frame(v1_cmp %>% transmute(res = bin_resolution, Domain = str_trunc(Domain, 38), n,
                                         changed = n_animals_score_changed, max_d = r4(max_abs_diff),
                                         med_d = r4(median_abs_diff), r = r4(pearson_r),
                                         rho = r4(spearman_rho)) %>% arrange(res, desc(max_d))), row.names = FALSE)

hr("DONE. Files written to OUT")
print(basename(c(file.path(OUT, "first_night_domain_source_classification.csv"),
                 file.path(OUT, "first_night_domain_scores.csv"),
                 file.path(OUT, "first_night_domain_provenance.csv"),
                 file.path(OUT, "first_night_domain_redundancy_audit.csv"),
                 file.path(OUT, "first_night_group_sex_n.csv"),
                 file.path(OUT, "first_night_hmm_window_equivalence_assertions.csv"),
                 file.path(OUT, "first_night_leakage_assertions.csv"),
                 file.path(OUT, "first_night_domain_contrasts_v2.csv"),
                 file.path(OUT, "first_night_group_sex_interaction_v2.csv"),
                 file.path(OUT, "first_night_hmm_component_features_v2.csv"),
                 file.path(OUT, "first_night_hmm_state_semantics_v2.csv"),
                 file.path(OUT, "first_night_duplicate_domain_check_v2.csv"),
                 file.path(OUT, "first_night_locomotion_dominance_v2.csv"),
                 file.path(OUT, "first_night_cross_resolution_stability_v2.csv"),
                 file.path(OUT, "first_night_hmm_coverage_v2.csv"),
                 file.path(OUT, "first_night_v1_countrule_vs_v2_clockwindow.csv"))))
