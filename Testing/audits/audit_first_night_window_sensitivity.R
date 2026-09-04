## audit_first_night_window_sensitivity.R
## ===========================================================================
## WINDOW SENSITIVITY AUDIT:  (a) canonical first post-CC1 Active 12 h
##                       vs   (b) the ENTIRE CC1 Active epoch (~48 h, 4 dark blocks)
##
## PURPOSE. This is an AUDIT / SENSITIVITY comparison ONLY. The manuscript panel uses the
## canonical clock-anchored 12 h window (a) REGARDLESS of which version produces larger or
## smaller contrasts. Nothing here is a recommendation to switch, and no window is chosen on
## p-values.
##
## WHY (b) IS NOT AN ALTERNATIVE FIRST-NIGHT MEASURE
##   (b) pools four separate dark/Active blocks separated by ~12.17 h Inactive gaps. It is an
##   AVERAGE OVER FOUR POST-REGROUPING NIGHTS, i.e. it mixes the first-encounter response with
##   three progressively habituated nights. (b) must NEVER be labelled a "first night",
##   "first encounter", "acute response" or "novelty response" measure.
##
## HOW (b) IS COMPUTED FOR TEMPORAL METRICS -- TWO VARIANTS, BOTH REPORTED
##   b_variant = "bridging_stage08_current":  runs, lag-1 differences, ACF1 and transitions are
##     taken over the animal's whole epoch ordered by TimeIndex, so a run/transition may BRIDGE
##     the ~12.17 h Inactive gap between dark blocks. This is what Stage 08
##     (Analysis/08_hmm_behavioral_states_optional.R:419-456) and Stage 14 currently do, so it is
##     the (b) REFERENCE for this audit: the question asked is "what would the SHIPPED ~48 h
##     epoch value give?", and the shipped estimator bridges.
##   b_variant = "gap_aware":  adjacency is restricted to pairs of bins in the SAME 12 h clock
##     phase block whose TimeIndex differs by exactly 1; runs are cut at every segment boundary.
##     No quantity ever crosses a gap. Reported alongside so the bridging artefact is visible.
##   For window (a) the sequence is a SINGLE contiguous segment, so the two variants are
##   numerically identical there -- asserted, not assumed.
##
## INTERPRETATION GUARDS (carried over unchanged)
##   - RFID proximity is a social-spatial CO-LOCATION proxy, NEVER "sociability".
##   - Occupancy composition carries NO temporal-order information; that domain is
##     "Latent-state occupancy organization", NEVER "temporal flexibility".
##   - RES/SUS are LATER phenotype labels derived from subsequent CombZ. Every contrast here is a
##     DESCRIPTIVE association with a later phenotype label -- never prospective, never causal.
##   - The shipped composite coefficient 0.5 on z(occupancy_entropy) is KEPT.
##   - top_proximity_state_fraction stays excluded (failed partition robustness) and is not
##     part of the displayed set audited here.
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

THIS_SCRIPT  <- "Testing/audits/audit_first_night_window_sensitivity.R"
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
WINDOW_HOURS <- 12
RESOLUTIONS  <- c("10min_based", "5min_based")
BIN_SEC      <- c("10min_based" = 600, "5min_based" = 300)
RES_ROLE     <- c("10min_based" = "primary", "5min_based" = "sensitivity")

hr  <- function(x) cat("\n", strrep("=", 90), "\n", x, "\n", strrep("=", 90), "\n", sep = "")
sec <- function(x) cat("\n--- ", x, " ---\n", sep = "")
pf  <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
r4  <- function(x) round(x, 4)

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}
get_first_cage_change <- function(x) {
  ux <- unique(as.character(x))
  cc_num <- suppressWarnings(as.numeric(str_extract(ux, "[0-9]+")))
  if (any(is.finite(cc_num))) ux[which.min(ifelse(is.finite(cc_num), cc_num, Inf))] else sort(ux)[1]
}
ACTIVE_PHASE_VALUES <- c("active", "dark", "night")
is_active_phase <- function(x) str_to_lower(str_trim(as.character(x))) %in% ACTIVE_PHASE_VALUES
score_mean <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dat)))
  out <- rowMeans(as.matrix(dat[, cols, drop = FALSE]), na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }
f_mean  <- function(x) mean(x, na.rm = TRUE)
f_rmssd <- function(x) { xf <- x[is.finite(x)]; if (length(xf) >= 3) sqrt(mean(diff(xf)^2, na.rm = TRUE)) else NA_real_ }
f_acf1  <- function(x) { xf <- x[is.finite(x)]; n <- length(xf); if (n >= 4) safe_cor(xf[-n], xf[-1], "pearson") else NA_real_ }
zsex <- function(dat, col) strict_standardize_within_context(dat, col, group_cols = "Sex")

## ---- segmentation: contiguous TimeIndex runs WITHIN one 12 h clock phase block -------------
seg_id <- function(ti, pb) {
  o <- order(ti); ti_s <- ti[o]; pb_s <- pb[o]
  if (length(ti_s) == 1) { out <- integer(1); out[o] <- 1L; return(out) }
  brk <- c(TRUE, (diff(ti_s) != 1) | (pb_s[-1] != pb_s[-length(pb_s)]))
  s <- cumsum(brk); out <- integer(length(ti)); out[o] <- s; out
}

## ---- Viterbi-sequence metrics over a LIST of segments -------------------------------------
## bridging  -> pass list(whole ordered sequence)   (== Stage 08 current behaviour)
## gap_aware -> pass the per-segment split
seq_metrics_multi <- function(seqs, K) {
  s_all <- unlist(seqs, use.names = FALSE)
  n <- length(s_all)
  occ <- tabulate(s_all, nbins = K) / n
  H <- ent(occ)
  from <- unlist(lapply(seqs, function(s) if (length(s) > 1) s[-length(s)] else integer(0)), use.names = FALSE)
  to   <- unlist(lapply(seqs, function(s) if (length(s) > 1) s[-1]            else integer(0)), use.names = FALSE)
  nt <- length(from)
  if (nt < 1) {
    return(tibble(occupancy_entropy = H, state_switch_rate = NA_real_,
                  self_transition_probability = NA_real_, transition_entropy = NA_real_,
                  mean_dwell_bins = NA_real_, n_transitions = 0L, n_bins = n,
                  n_segments = length(seqs), occ = list(occ)))
  }
  TC <- matrix(0L, K, K)
  for (i in seq_len(nt)) TC[from[i], to[i]] <- TC[from[i], to[i]] + 1L
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  self_p <- sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0))
  runs_l <- unlist(lapply(seqs, function(s) rle(s)$lengths), use.names = FALSE)
  runs_v <- unlist(lapply(seqs, function(s) rle(s)$values),  use.names = FALSE)
  dw <- vapply(seq_len(K), function(k) { l <- runs_l[runs_v == k]; if (!length(l)) NA_real_ else mean(l) }, numeric(1))
  mean_dwell <- sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)])
  tibble(occupancy_entropy = H, state_switch_rate = mean(from != to),
         self_transition_probability = self_p, transition_entropy = sum(pi_s * rowH),
         mean_dwell_bins = mean_dwell, n_transitions = nt, n_bins = n,
         n_segments = length(seqs), occ = list(occ))
}

## ---- raw bin-level features, bridging vs gap-aware -----------------------------------------
raw_metrics_one <- function(x, seg, variant) {
  if (identical(variant, "bridging_stage08_current")) {
    return(c(mean = f_mean(x), rmssd = f_rmssd(x), acf1 = f_acf1(x)))
  }
  sq <- numeric(0); p1 <- numeric(0); p2 <- numeric(0)
  for (g in unique(seg)) {
    xs <- x[seg == g]; xf <- xs[is.finite(xs)]; ns <- length(xf)
    if (ns >= 2) { sq <- c(sq, diff(xf)^2); p1 <- c(p1, xf[-ns]); p2 <- c(p2, xf[-1]) }
  }
  c(mean  = f_mean(x),
    rmssd = if (length(sq) >= 2) sqrt(mean(sq)) else NA_real_,
    acf1  = if (length(p1) >= 3) safe_cor(p1, p2, "pearson") else NA_real_)
}

ASSERT <- list()
add_assert <- function(assertion, method, ok, evidence) {
  ASSERT[[length(ASSERT) + 1L]] <<- tibble(assertion = assertion, method = method,
                                           result = pf(ok), evidence = evidence)
  cat("  [", pf(ok), "] ", assertion, "  ::  ", evidence, "\n", sep = "")
  invisible(ok)
}

## ==========================================================================
hr("STEP 0. Canonical roster + constants")
## ==========================================================================
roster <- build_canonical_identity_roster(
  read_csv(file.path(DERIV, "5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE),
  "Stage 01 canonical roster (5min_based all_behavior_metrics.csv)")
cat("canonical roster animals:", nrow(roster), "\n")
stopifnot(nrow(roster) == 111L)
cat("ANIMALPOS_INACTIVE_START_SEC =", ANIMALPOS_INACTIVE_START_SEC,
    " ANIMALPOS_PHASE_LENGTH_SEC =", ANIMALPOS_PHASE_LENGTH_SEC, "\n")

DISPLAYED_DOMAINS <- c("Psychomotor activation",
                       "Behavioral flexibility / predictability",
                       "Social spatial organization",
                       "Behavioral volatility / fragmentation",
                       "Active-phase adaptation/exploration",
                       "Latent-state occupancy organization",
                       "Latent-state persistence")
HMM_COMPONENTS <- c("occupancy_entropy", "inactive_state_fraction", "self_transition_probability",
                    "transition_entropy", "state_switch_rate", "mean_dwell_minutes")
B_VARIANTS <- c("bridging_stage08_current", "gap_aware")
B_REFERENCE <- "bridging_stage08_current"

## ==========================================================================
hr("STEP 1. Build both windows, both variants, at both resolutions")
## ==========================================================================
raw_store <- list(); hmm_store <- list(); geom_store <- list(); anchor_store <- list()

for (res in RESOLUTIONS) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  sec(sprintf("%s  (bin %d s, role %s)", res, bs, RES_ROLE[[res]]))

  d <- read_csv(file.path(DERIV, res, "all_behavior_metrics.csv"),
                col_types = cols(AnimalNum = col_character(), BinStart = col_datetime(),
                                 .default = col_guess()), progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
    semi_join(roster, by = "AnimalNum") %>%
    mutate(Phase = as.character(Phase), CageChange = as.character(CageChange),
           TimeIndex = safe_numeric(TimeIndex), Movement = safe_numeric(Movement),
           Entropy = safe_numeric(Entropy), Proximity = safe_numeric(ProximityFraction))

  first_cc <- get_first_cage_change(d$CageChange)
  act <- d %>% filter(CageChange == first_cc, is_active_phase(Phase)) %>%
    mutate(.session = as.character(SourceFile),
           phase_block = animalpos_phase_block_index(BinStart))
  cat("  CC1 =", first_cc, "| CC1 Active rows:", nrow(act), "| animals:", n_distinct(act$AnimalNum),
      "| distinct clock phase blocks:", n_distinct(act$phase_block), "\n")

  anchors <- act %>% group_by(.session) %>%
    summarise(target_phase_block = min(phase_block, na.rm = TRUE), .groups = "drop") %>%
    mutate(target_window_start = as.POSIXct(target_phase_block * ANIMALPOS_PHASE_LENGTH_SEC +
                                              ANIMALPOS_INACTIVE_START_SEC, origin = "1970-01-01", tz = "UTC"),
           target_window_end = target_window_start + WINDOW_HOURS * 3600, resolution = res)
  anchor_store[[res]] <- anchors
  cat("  sessions:", nrow(anchors), " anchors:",
      paste(unique(format(anchors$target_window_start, "%H:%M")), collapse = "|"), "->",
      paste(unique(format(anchors$target_window_end, "%H:%M")), collapse = "|"), "\n")

  actw <- act %>% left_join(anchors %>% select(.session, target_window_start, target_window_end),
                            by = ".session") %>%
    mutate(elapsed_sec_in_window = as.numeric(difftime(BinStart, target_window_start, units = "secs")),
           in_window_a = elapsed_sec_in_window >= 0 & elapsed_sec_in_window < WINDOW_HOURS * 3600)
  cat("  window (a) rows:", sum(actw$in_window_a), " animals:",
      n_distinct(actw$AnimalNum[actw$in_window_a]), "\n")
  cat("  epoch  (b) rows:", nrow(actw), " animals:", n_distinct(actw$AnimalNum), "\n")

  ## ---- descriptive geometry of (b) --------------------------------------
  geom <- actw %>% group_by(AnimalNum) %>%
    summarise(n_bins_b = n(),
              n_dark_blocks_b = n_distinct(phase_block),
              n_segments_b = n_distinct(seg_id(TimeIndex, phase_block)),
              span_hours_b = as.numeric(difftime(max(BinStart) + bs, min(BinStart), units = "hours")),
              covered_hours_b = n() * bs / 3600,
              n_bins_a = sum(in_window_a),
              covered_hours_a = sum(in_window_a) * bs / 3600,
              first_night_bin_fraction = sum(in_window_a) / n(),
              max_gap_hours = { bsx <- sort(BinStart); if (length(bsx) > 1)
                as.numeric(max(difftime(bsx[-1], bsx[-length(bsx)], units = "hours"))) else NA_real_ },
              .groups = "drop") %>%
    mutate(resolution = res, bin_size_sec = bs)
  geom_store[[res]] <- geom
  cat("  (b) dark blocks per animal:\n"); print(table(geom$n_dark_blocks_b))
  cat("  (b) contiguous segments per animal (gap-aware):\n"); print(table(geom$n_segments_b))
  cat("  (b) span_hours: median", r4(median(geom$span_hours_b)),
      " range [", r4(min(geom$span_hours_b)), ",", r4(max(geom$span_hours_b)), "]\n")
  cat("  (b) covered_hours: median", r4(median(geom$covered_hours_b)),
      " | (a) covered_hours: median", r4(median(geom$covered_hours_a)), "\n")
  cat("  fraction of (b) bins coming from the FIRST night: median",
      r4(median(geom$first_night_bin_fraction)), " range [",
      r4(min(geom$first_night_bin_fraction)), ",", r4(max(geom$first_night_bin_fraction)), "]\n")
  cat("  (b) max within-epoch gap between consecutive bins: median",
      r4(median(geom$max_gap_hours, na.rm = TRUE)), " h  (inter-dark-block Inactive gap)\n")

  ## ---- raw features: window x variant ----------------------------------
  for (win in c("a_canonical_12h", "b_all_cc1_active")) {
    src <- if (win == "a_canonical_12h") actw %>% filter(in_window_a) else actw
    blind <- src %>% select(AnimalNum, TimeIndex, phase_block, Movement, Entropy, Proximity)
    for (v in B_VARIANTS) {
      ff <- blind %>% arrange(AnimalNum, TimeIndex) %>% group_by(AnimalNum) %>%
        group_modify(~ {
          sg <- seg_id(.x$TimeIndex, .x$phase_block)
          mv <- raw_metrics_one(.x$Movement,  sg, v)
          en <- raw_metrics_one(.x$Entropy,   sg, v)
          px <- raw_metrics_one(.x$Proximity, sg, v)
          tibble(Movement_mean = mv[["mean"]], Movement_rmssd = mv[["rmssd"]], Movement_acf1 = mv[["acf1"]],
                 Entropy_mean = en[["mean"]],  Entropy_rmssd = en[["rmssd"]],  Entropy_acf1 = en[["acf1"]],
                 Proximity_mean = px[["mean"]], Proximity_rmssd = px[["rmssd"]], Proximity_acf1 = px[["acf1"]],
                 n_bins = nrow(.x), n_segments = length(unique(sg)))
        }) %>% ungroup() %>%
        left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum") %>%
        mutate(resolution = res, window = win, b_variant = v)
      zc <- c("Movement_mean","Movement_rmssd","Movement_acf1","Entropy_mean","Entropy_rmssd",
              "Entropy_acf1","Proximity_mean","Proximity_rmssd","Proximity_acf1")
      fz <- reduce(zc, zsex, .init = ff)
      raw_store[[paste(res, win, v)]] <- fz %>% mutate(
        `Psychomotor activation` = Movement_mean_z,
        `Behavioral flexibility / predictability` =
          score_mean(pick(everything()), c("Entropy_mean_z","Entropy_rmssd_z")) - coalesce(Entropy_acf1_z, 0),
        `Social spatial organization` =
          score_mean(pick(everything()), c("Proximity_mean_z","Proximity_acf1_z")) - coalesce(Proximity_rmssd_z, 0),
        `Behavioral volatility / fragmentation` =
          score_mean(pick(everything()), c("Movement_rmssd_z","Entropy_rmssd_z","Proximity_rmssd_z")),
        `Active-phase adaptation/exploration` =
          score_mean(pick(everything()), c("Movement_mean_z","Entropy_mean_z","Proximity_mean_z")) -
          score_mean(pick(everything()), c("Movement_acf1_z","Entropy_acf1_z")))
    }
  }

  ## ---- HMM ---------------------------------------------------------------
  ss  <- read_csv(file.path(HMM, res, "tables/hmm_state_summary.csv"), col_types = cols(), progress = FALSE)
  lab <- annotate_hmm_semantic_states(ss, res); K <- nrow(lab)
  inactive_states <- lab$State[lab$SemanticState == "inactive/low-exploration"]
  social_states   <- lab$State[lab$SemanticState == "social"]
  cat("  common state space K =", K, " inactive = {", paste(inactive_states, collapse = ","), "}\n", sep = "")

  asg <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                  col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE)
  aud <- audit_hmm_identity(asg, roster, paste("hmm_state_assignments", res))
  assert_hmm_identity_audit(aud)
  asg <- aud$data %>% mutate(AnimalNum = as.character(AnimalNum), Phase = as.character(Phase),
                             CageChange = as.character(CageChange),
                             TimeIndex = safe_numeric(TimeIndex), State = as.integer(State))
  hcc1 <- asg %>% filter(CageChange == get_first_cage_change(asg$CageChange), is_active_phase(Phase))
  keys <- actw %>% select(AnimalNum, CageChange, Phase, TimeIndex, BinStart, phase_block, in_window_a)
  hb <- hcc1 %>% inner_join(keys, by = c("AnimalNum", "CageChange", "Phase", "TimeIndex"))
  cat("  HMM CC1-Active rows:", nrow(hcc1), "-> joined to BinStart:", nrow(hb),
      " animals:", n_distinct(hb$AnimalNum), " | in (a):", sum(hb$in_window_a), "\n")
  add_assert(sprintf("[%s] every HMM CC1-Active row joins 1:1 to a bin-level BinStart", res),
             "inner_join on (AnimalNum, CageChange, Phase, TimeIndex)",
             nrow(hb) == nrow(hcc1),
             sprintf("%d of %d HMM rows joined (%d animals)", nrow(hb), nrow(hcc1), n_distinct(hb$AnimalNum)))

  for (win in c("a_canonical_12h", "b_all_cc1_active")) {
    src <- if (win == "a_canonical_12h") hb %>% filter(in_window_a) else hb
    for (v in B_VARIANTS) {
      hf <- src %>% select(AnimalNum, TimeIndex, phase_block, State) %>%
        arrange(AnimalNum, TimeIndex) %>% group_by(AnimalNum) %>%
        group_modify(~ {
          sg <- seg_id(.x$TimeIndex, .x$phase_block)
          o  <- order(.x$TimeIndex)
          seqs <- if (identical(v, "bridging_stage08_current")) list(.x$State[o]) else
            unname(split(.x$State[o], sg[o]))
          m <- seq_metrics_multi(seqs, K); occv <- m$occ[[1]]
          m %>% select(-occ) %>%
            mutate(inactive_state_fraction = sum(occv[as.integer(inactive_states)]),
                   social_state_fraction = if (length(social_states)) sum(occv[as.integer(social_states)]) else 0,
                   n_clock_blocks = n_distinct(.x$phase_block))
        }) %>% ungroup() %>%
        mutate(bin_size_sec = bs, mean_dwell_minutes = mean_dwell_bins * bs / 60,
               resolution = res, window = win, b_variant = v) %>%
        left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum")
      hz <- reduce(c("occupancy_entropy", "inactive_state_fraction", "social_state_fraction",
                     "mean_dwell_minutes", "self_transition_probability", "transition_entropy",
                     "state_switch_rate"), zsex, .init = hf)
      hmm_store[[paste(res, win, v)]] <- hz %>% mutate(
        `Latent-state occupancy organization` = 0.5 * occupancy_entropy_z - inactive_state_fraction_z,
        shipped_3component_form = rowMeans(cbind(occupancy_entropy_z, social_state_fraction_z), na.rm = FALSE) -
          inactive_state_fraction_z,
        `Latent-state persistence` = mean_dwell_minutes_z)
    }
  }
}

geom_all <- bind_rows(geom_store)
write_csv(geom_all, file.path(OUT, "first_night_vs_all_cc1_active_epoch_geometry.csv"))

## ==========================================================================
hr("STEP 2. Assertions")
## ==========================================================================
for (res in RESOLUTIONS) {
  ra <- raw_store[[paste(res, "a_canonical_12h", "bridging_stage08_current")]]
  rg <- raw_store[[paste(res, "a_canonical_12h", "gap_aware")]]
  cmpc <- c("Movement_rmssd","Movement_acf1","Entropy_rmssd","Entropy_acf1","Proximity_rmssd","Proximity_acf1")
  md <- max(unlist(map(cmpc, ~ max(abs(ra[[.x]] - rg[[.x]]), na.rm = TRUE))), na.rm = TRUE)
  add_assert(sprintf("[%s raw] inside window (a) the bridging and gap-aware estimators are identical", res),
             "max abs diff over RMSSD/ACF1 features, animal by animal",
             md < 1e-12, sprintf("max abs diff = %s; max n_segments in (a) = %d",
                                 format(md, scientific = TRUE), max(ra$n_segments)))
  ha <- hmm_store[[paste(res, "a_canonical_12h", "bridging_stage08_current")]]
  hg <- hmm_store[[paste(res, "a_canonical_12h", "gap_aware")]]
  cmph <- c("self_transition_probability","transition_entropy","state_switch_rate","mean_dwell_bins","occupancy_entropy")
  mdh <- max(unlist(map(cmph, ~ max(abs(ha[[.x]] - hg[[.x]]), na.rm = TRUE))), na.rm = TRUE)
  add_assert(sprintf("[%s HMM] inside window (a) the bridging and gap-aware estimators are identical", res),
             "max abs diff over the sequence metrics, animal by animal",
             mdh < 1e-12, sprintf("max abs diff = %s; max n_segments in (a) = %d",
                                  format(mdh, scientific = TRUE), max(ha$n_segments)))
  hb1 <- hmm_store[[paste(res, "b_all_cc1_active", "bridging_stage08_current")]]
  hb2 <- hmm_store[[paste(res, "b_all_cc1_active", "gap_aware")]]
  add_assert(sprintf("[%s] occupancy_entropy and inactive_state_fraction are variant-invariant in (b)", res),
             "order-free occupancy quantities compared between bridging and gap-aware over the ~48 h epoch",
             max(abs(hb1$occupancy_entropy - hb2$occupancy_entropy), na.rm = TRUE) < 1e-12 &&
               max(abs(hb1$inactive_state_fraction - hb2$inactive_state_fraction), na.rm = TRUE) < 1e-12,
             sprintf("max abs diff H = %s, inactive = %s (occupancy carries NO temporal-order information)",
                     format(max(abs(hb1$occupancy_entropy - hb2$occupancy_entropy), na.rm = TRUE), scientific = TRUE),
                     format(max(abs(hb1$inactive_state_fraction - hb2$inactive_state_fraction), na.rm = TRUE), scientific = TRUE)))
  add_assert(sprintf("[%s] switch-rate identity holds in (b) under both variants", res),
             "max |state_switch_rate - (1 - self_transition_probability)|",
             max(abs(hb1$state_switch_rate - (1 - hb1$self_transition_probability)), na.rm = TRUE) < 1e-10 &&
               max(abs(hb2$state_switch_rate - (1 - hb2$self_transition_probability)), na.rm = TRUE) < 1e-10,
             sprintf("bridging %s, gap-aware %s",
                     format(max(abs(hb1$state_switch_rate - (1 - hb1$self_transition_probability)), na.rm = TRUE), scientific = TRUE),
                     format(max(abs(hb2$state_switch_rate - (1 - hb2$self_transition_probability)), na.rm = TRUE), scientific = TRUE)))
  add_assert(sprintf("[%s] formula preservation: 0.5*z(H) - z(inactive) == shipped mean(z(H), z(social)) - z(inactive) in BOTH windows", res),
             "both forms computed side by side on the same z-scores",
             max(abs(hb1$`Latent-state occupancy organization` - hb1$shipped_3component_form), na.rm = TRUE) < 1e-12 &&
               max(abs(ha$`Latent-state occupancy organization` - ha$shipped_3component_form), na.rm = TRUE) < 1e-12,
             sprintf("(b) max abs diff = %s ; (a) max abs diff = %s ; social_state_fraction identically zero = %s",
                     format(max(abs(hb1$`Latent-state occupancy organization` - hb1$shipped_3component_form), na.rm = TRUE), scientific = TRUE),
                     format(max(abs(ha$`Latent-state occupancy organization` - ha$shipped_3component_form), na.rm = TRUE), scientific = TRUE),
                     all(hb1$social_state_fraction == 0)))
}
up_dom <- read_csv(file.path(OUT, "first_night_domain_scores.csv"),
                   col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE)
up_cmp <- read_csv(file.path(OUT, "first_night_hmm_component_features_v2.csv"),
                   col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE)
for (res in RESOLUTIONS) {
  mine <- bind_rows(
    raw_store[[paste(res, "a_canonical_12h", "bridging_stage08_current")]] %>%
      select(AnimalNum, any_of(DISPLAYED_DOMAINS)) %>% pivot_longer(-AnimalNum, names_to = "Domain", values_to = "mine"),
    hmm_store[[paste(res, "a_canonical_12h", "bridging_stage08_current")]] %>%
      select(AnimalNum, any_of(DISPLAYED_DOMAINS)) %>% pivot_longer(-AnimalNum, names_to = "Domain", values_to = "mine"))
  cmp0 <- up_dom %>% filter(bin_resolution == res, Domain %in% DISPLAYED_DOMAINS) %>%
    select(AnimalNum, Domain, DomainScore) %>% inner_join(mine, by = c("AnimalNum", "Domain"))
  md <- max(abs(cmp0$DomainScore - cmp0$mine), na.rm = TRUE)
  add_assert(sprintf("[%s] window (a) reproduces the upstream v2 domain scores", res),
             "value-by-value comparison against first_night_domain_scores.csv",
             md < 1e-10, sprintf("n compared = %d, max abs diff = %s", nrow(cmp0), format(md, scientific = TRUE)))
  mc <- hmm_store[[paste(res, "a_canonical_12h", "bridging_stage08_current")]] %>%
    select(AnimalNum, all_of(HMM_COMPONENTS)) %>%
    inner_join(up_cmp %>% filter(resolution == res) %>% select(AnimalNum, all_of(HMM_COMPONENTS)),
               by = "AnimalNum", suffix = c("_mine", "_up"))
  mdc <- max(unlist(map(HMM_COMPONENTS, ~ max(abs(mc[[paste0(.x, "_mine")]] - mc[[paste0(.x, "_up")]]), na.rm = TRUE))))
  add_assert(sprintf("[%s] window (a) reproduces the upstream v2 HMM components", res),
             "value-by-value comparison against first_night_hmm_component_features_v2.csv",
             mdc < 1e-10, sprintf("n animals = %d, max abs diff = %s", nrow(mc), format(mdc, scientific = TRUE)))
}
add_assert("no HMM refit anywhere in this script",
           "depmixS4 namespace never loaded; Viterbi states read from hmm_state_assignments.csv only",
           !("depmixS4" %in% loadedNamespaces()),
           sprintf("depmixS4 loaded = %s; the COMMON group-blind longitudinal state space is reused unchanged",
                   "depmixS4" %in% loadedNamespaces()))

## ==========================================================================
hr("STEP 3. Long metric table for both windows")
## ==========================================================================
long_vals <- map_dfr(RESOLUTIONS, function(res) {
  map_dfr(c("a_canonical_12h", "b_all_cc1_active"), function(win) {
    map_dfr(B_VARIANTS, function(v) {
      rr <- raw_store[[paste(res, win, v)]]
      hh <- hmm_store[[paste(res, win, v)]]
      raw_d <- intersect(DISPLAYED_DOMAINS, names(rr))
      hmm_d <- intersect(DISPLAYED_DOMAINS, names(hh))
      bind_rows(
        rr %>% select(AnimalNum, Group, Sex, n_bins, all_of(raw_d)) %>%
          pivot_longer(all_of(raw_d), names_to = "metric", values_to = "value") %>%
          mutate(metric_family = "displayed_domain", feature_origin = "raw_RFID"),
        hh %>% select(AnimalNum, Group, Sex, n_bins, all_of(hmm_d)) %>%
          pivot_longer(all_of(hmm_d), names_to = "metric", values_to = "value") %>%
          mutate(metric_family = "displayed_domain", feature_origin = "HMM_derived"),
        hh %>% select(AnimalNum, Group, Sex, n_bins, all_of(paste0(HMM_COMPONENTS, "_z"))) %>%
          pivot_longer(all_of(paste0(HMM_COMPONENTS, "_z")), names_to = "metric", values_to = "value") %>%
          mutate(metric = str_remove(metric, "_z$"),
                 metric_family = "hmm_component", feature_origin = "HMM_derived")
      ) %>% mutate(bin_resolution = res, window = win, b_variant = v)
    })
  })
})
long_a <- long_vals %>% filter(window == "a_canonical_12h", b_variant == B_REFERENCE) %>%
  select(-window, -b_variant) %>% rename(value_a = value, n_bins_a = n_bins)
long_b <- long_vals %>% filter(window == "b_all_cc1_active") %>%
  select(-window) %>% rename(value_b = value, n_bins_b = n_bins)
paired <- long_b %>% inner_join(long_a, by = c("AnimalNum","Group","Sex","metric","metric_family",
                                               "feature_origin","bin_resolution"))
cat("paired animal-level rows:", nrow(paired), " metrics:", n_distinct(paired$metric), "\n")
sec("metric inventory")
print(as.data.frame(paired %>% distinct(metric_family, feature_origin, metric) %>% arrange(metric_family, metric)),
      row.names = FALSE)

## ==========================================================================
hr("STEP 4. Animal-level (a) vs (b) correlations")
## ==========================================================================
cors <- paired %>% group_by(bin_resolution, b_variant, metric, metric_family, feature_origin) %>%
  summarise(n_pooled = sum(is.finite(value_a) & is.finite(value_b)),
            pearson_pooled  = safe_cor(value_a, value_b, "pearson"),
            spearman_pooled = safe_cor(value_a, value_b, "spearman"), .groups = "drop")
cors_sex <- paired %>% group_by(bin_resolution, b_variant, metric, Sex) %>%
  summarise(n_sex = sum(is.finite(value_a) & is.finite(value_b)),
            pearson_within_sex  = safe_cor(value_a, value_b, "pearson"),
            spearman_within_sex = safe_cor(value_a, value_b, "spearman"), .groups = "drop")
write_csv(cors %>% left_join(cors_sex, by = c("bin_resolution","b_variant","metric")),
          file.path(OUT, "first_night_vs_all_cc1_active_correlations.csv"))
sec("PRIMARY 10min, (b) reference variant: animal-level a-vs-b correlation")
print(as.data.frame(cors %>% filter(bin_resolution == "10min_based", b_variant == B_REFERENCE) %>%
  transmute(metric_family, metric = str_trunc(metric, 40), n = n_pooled,
            r = r4(pearson_pooled), rho = r4(spearman_pooled)) %>% arrange(metric_family, desc(r))),
  row.names = FALSE)
sec("5min sensitivity, (b) reference variant: animal-level a-vs-b correlation")
print(as.data.frame(cors %>% filter(bin_resolution == "5min_based", b_variant == B_REFERENCE) %>%
  transmute(metric_family, metric = str_trunc(metric, 40), n = n_pooled,
            r = r4(pearson_pooled), rho = r4(spearman_pooled)) %>% arrange(metric_family, desc(r))),
  row.names = FALSE)

## ==========================================================================
hr("STEP 5. Same lm(value ~ Group*Sex) + emmeans model under (a) and under (b)")
## ==========================================================================
stopifnot(requireNamespace("emmeans", quietly = TRUE))
CONTRASTS <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))
REF_OF <- c("RES-CON" = "CON", "SUS-CON" = "CON", "SUS-RES" = "RES")
CMP_OF <- c("RES-CON" = "RES", "SUS-CON" = "SUS", "SUS-RES" = "SUS")

fit_contrasts <- function(dd) {
  m <- dd %>% filter(is.finite(value)) %>%
    mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS))
  if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
  fit <- stats::lm(value ~ Group * Sex, data = m)
  ct <- as.data.frame(summary(emmeans::contrast(emmeans::emmeans(fit, ~ Group | Sex),
                                                method = CONTRASTS, adjust = "none"),
                              infer = c(TRUE, TRUE)))
  as_tibble(ct) %>%
    transmute(Sex = as.character(Sex), contrast = as.character(contrast),
              estimate, SE, df, ci_low = lower.CL, ci_high = upper.CL,
              t_ratio = t.ratio, p_value = p.value) %>%
    rowwise() %>%
    mutate(group_ref = REF_OF[[contrast]], group_comp = CMP_OF[[contrast]],
           n_ref  = sum(as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex),
           n_comp = sum(as.character(m$Group) == group_comp & as.character(m$Sex) == Sex),
           hedges_g = hmm_hedges_g(m$value[as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex],
                                   m$value[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex])) %>%
    ungroup()
}
grid <- paired %>% distinct(bin_resolution, b_variant, metric, metric_family, feature_origin)
res_a <- pmap_dfr(list(grid$bin_resolution, grid$b_variant, grid$metric), function(rr, vv, mm) {
  dd <- paired %>% filter(bin_resolution == rr, b_variant == vv, metric == mm) %>%
    transmute(AnimalNum, Group, Sex, value = value_a)
  out <- fit_contrasts(dd); if (is.null(out)) return(NULL)
  out %>% mutate(bin_resolution = rr, b_variant = vv, metric = mm)
})
res_b <- pmap_dfr(list(grid$bin_resolution, grid$b_variant, grid$metric), function(rr, vv, mm) {
  dd <- paired %>% filter(bin_resolution == rr, b_variant == vv, metric == mm) %>%
    transmute(AnimalNum, Group, Sex, value = value_b)
  out <- fit_contrasts(dd); if (is.null(out)) return(NULL)
  out %>% mutate(bin_resolution = rr, b_variant = vv, metric = mm)
})
cat("contrast rows: (a)", nrow(res_a), " (b)", nrow(res_b), "\n")

KEYS <- c("bin_resolution", "b_variant", "metric", "Sex", "contrast")
cmp <- res_a %>% rename_with(~ paste0(.x, "_a"), -all_of(KEYS)) %>%
  inner_join(res_b %>% rename_with(~ paste0(.x, "_b"), -all_of(KEYS)), by = KEYS) %>%
  left_join(grid %>% distinct(bin_resolution, b_variant, metric, metric_family, feature_origin),
            by = c("bin_resolution", "b_variant", "metric")) %>%
  left_join(cors, by = c("bin_resolution","b_variant","metric","metric_family","feature_origin")) %>%
  left_join(cors_sex, by = c("bin_resolution","b_variant","metric","Sex"))

geom_sum <- geom_all %>% group_by(resolution) %>%
  summarise(b_n_dark_blocks_median = median(n_dark_blocks_b),
            b_n_dark_blocks_range = sprintf("%d-%d", min(n_dark_blocks_b), max(n_dark_blocks_b)),
            b_span_hours_median = median(span_hours_b),
            b_covered_hours_median = median(covered_hours_b),
            a_covered_hours_median = median(covered_hours_a),
            first_night_bin_fraction_median = median(first_night_bin_fraction),
            first_night_bin_fraction_min = min(first_night_bin_fraction),
            first_night_bin_fraction_max = max(first_night_bin_fraction),
            b_max_inter_bin_gap_hours_median = median(max_gap_hours, na.rm = TRUE),
            .groups = "drop") %>% rename(bin_resolution = resolution)

TEMPORAL_METRICS <- c("Behavioral flexibility / predictability", "Social spatial organization",
                      "Behavioral volatility / fragmentation", "Active-phase adaptation/exploration",
                      "Latent-state persistence", "self_transition_probability",
                      "transition_entropy", "state_switch_rate", "mean_dwell_minutes")

sens <- cmp %>%
  left_join(geom_sum, by = "bin_resolution") %>%
  mutate(
    resolution_role = RES_ROLE[bin_resolution],
    b_variant_role = if_else(b_variant == B_REFERENCE, "REFERENCE for (b)", "secondary (gap-aware) variant of (b)"),
    b_variant_reason = if_else(b_variant == B_REFERENCE,
      "Stage 08 (Analysis/08:419-456) and Stage 14 currently compute runs and transitions over the whole AnimalNum x CageChange x Phase epoch ordered by TimeIndex, so they BRIDGE the ~12.17 h inter-dark-block Inactive gaps. This audit asks what the SHIPPED ~48 h epoch value would give, so the bridging estimator is the (b) reference.",
      "Adjacency restricted to the same 12 h clock phase block AND consecutive TimeIndex; runs cut at every segment boundary. Nothing crosses a gap. Shown so the size of the bridging artefact is visible."),
    metric_is_temporal_order_dependent = metric %in% TEMPORAL_METRICS,
    abs_diff_estimate = abs(estimate_b - estimate_a),
    signed_diff_estimate = estimate_b - estimate_a,
    pct_diff_estimate = 100 * (estimate_b - estimate_a) / abs(estimate_a),
    abs_diff_hedges_g = abs(hedges_g_b - hedges_g_a),
    direction_changes = is.finite(estimate_a) & is.finite(estimate_b) & (sign(estimate_a) != sign(estimate_b)),
    ci_overlap = (ci_low_a <= ci_high_b) & (ci_low_b <= ci_high_a),
    crosses_p05 = (p_value_a < 0.05) != (p_value_b < 0.05),
    statistical_conclusion_changes = direction_changes | !ci_overlap | crosses_p05,
    biological_interpretation_changes = TRUE,
    biological_interpretation_statement = sprintf(
      "(a) = FIRST-ENCOUNTER RESPONSE: the single 12 h dark block immediately following the CC1 regrouping (%.1f h of Active time, one contiguous block). (b) = AVERAGE OF FOUR POST-REGROUPING NIGHTS: %s dark blocks totalling %.1f h of Active time across a %.1f h wall-clock span (the difference is three ~12.1 h Inactive gaps), of which only %.0f%% of bins are the first night, so (b) mixes the acute first-encounter night with three progressively habituated nights and is NOT a first-encounter measure. The biological construct therefore ALWAYS differs, independently of whether the numeric estimate moves%s.",
      a_covered_hours_median, b_n_dark_blocks_range, b_covered_hours_median, b_span_hours_median,
      100 * first_night_bin_fraction_median,
      if_else(metric_is_temporal_order_dependent,
              "; and because this metric is temporal-order dependent, (b) additionally absorbs (bridging variant) or censors (gap-aware variant) the ~12.17 h inter-night Inactive gaps",
              "; this metric is order-free, so the only change is the three extra nights of data")),
    model = "lm(value ~ Group * Sex) then emmeans(~ Group | Sex), contrasts RES-CON/SUS-CON/SUS-RES, adjust='none'. ONE value per animal per metric per window; no random effect, no repeated measures.",
    standardization = "each metric z-scored within SEX ONLY, computed SEPARATELY inside each window, so (a) and (b) estimates are both in within-window SD units (a positive affine rescaling of the raw feature).",
    ci_formula = "emmeans 95% CI: estimate +/- qt(0.975, df) * SE from lm(value ~ Group*Sex)",
    audit_status = "SENSITIVITY AUDIT ONLY. The manuscript panel uses window (a) regardless of which window yields larger contrasts. No window, resolution or variant was selected on p-values.",
    interpretation_guard = case_when(
      metric == "Social spatial organization" ~ "RFID proximity = social-spatial CO-LOCATION proxy, never sociability",
      metric %in% c("Latent-state occupancy organization", "occupancy_entropy", "inactive_state_fraction") ~
        "occupancy composition carries NO temporal-order information; never 'temporal flexibility'",
      metric %in% c("Latent-state persistence", "mean_dwell_minutes", "self_transition_probability", "state_switch_rate") ~
        "ONE persistence construct: state_switch_rate == 1 - self_transition_probability exactly; only mean_dwell_minutes is displayed as a domain",
      TRUE ~ "RES/SUS are LATER phenotype labels from subsequent CombZ; every contrast is a DESCRIPTIVE association with later phenotype, never prospective or causal"),
    window_a_definition = "CC1, Phase exactly in c('active','dark','night'); per-session target_window_start = min(animalpos_phase_block_index(BinStart))*43200 + 23400 (= 18:30 clock); keep elapsed_sec in [0, 43200)",
    window_b_definition = "CC1, Phase exactly in c('active','dark','night'), ENTIRE epoch: all four dark/Active blocks, no time filter",
    script = THIS_SCRIPT) %>%
  group_by(bin_resolution, b_variant, Sex, metric_family) %>%
  mutate(fdr_family_id = sprintf("WINDOW_SENSITIVITY__%s__%s__%s__%s", bin_resolution, b_variant, Sex, metric_family),
         p_fdr_a = p.adjust(p_value_a, "BH"), p_fdr_b = p.adjust(p_value_b, "BH")) %>%
  ungroup() %>%
  mutate(fdr_note = "BH within bin_resolution x b_variant x Sex x metric_family. The displayed_domain family mirrors the primary FIRST_NIGHT family (7 domains x 3 contrasts); the hmm_component family is an EXPLORATORY scan (6 components x 3 contrasts). This audit table is itself exploratory and does not replace or reweight the primary family.") %>%
  arrange(bin_resolution, b_variant, metric_family, metric, Sex, contrast) %>%
  select(bin_resolution, resolution_role, metric, metric_family, feature_origin,
         metric_is_temporal_order_dependent, b_variant, b_variant_role, b_variant_reason,
         Sex, contrast, group_ref_a, group_comp_a, n_ref_a, n_comp_a, n_ref_b, n_comp_b,
         n_pooled, pearson_pooled, spearman_pooled, n_sex, pearson_within_sex, spearman_within_sex,
         estimate_a, SE_a, ci_low_a, ci_high_a, t_ratio_a, df_a, p_value_a, p_fdr_a, hedges_g_a,
         estimate_b, SE_b, ci_low_b, ci_high_b, t_ratio_b, df_b, p_value_b, p_fdr_b, hedges_g_b,
         signed_diff_estimate, abs_diff_estimate, pct_diff_estimate, abs_diff_hedges_g,
         direction_changes, ci_overlap, crosses_p05, statistical_conclusion_changes,
         biological_interpretation_changes, biological_interpretation_statement,
         b_n_dark_blocks_median, b_n_dark_blocks_range, b_span_hours_median, b_covered_hours_median,
         a_covered_hours_median, first_night_bin_fraction_median, first_night_bin_fraction_min,
         first_night_bin_fraction_max, b_max_inter_bin_gap_hours_median,
         window_a_definition, window_b_definition, standardization, model, ci_formula,
         fdr_family_id, fdr_note, interpretation_guard, audit_status, script)

write_csv(sens, file.path(OUT, "first_night_vs_all_cc1_active_sensitivity.csv"))
cat("\nwrote first_night_vs_all_cc1_active_sensitivity.csv  rows =", nrow(sens), "\n")

## ==========================================================================
hr("STEP 6. Reported results")
## ==========================================================================
sec("Descriptive geometry of window (b)")
print(as.data.frame(geom_sum), row.names = FALSE)
cat("\nPLAIN STATEMENT: window (b) pools", geom_sum$b_n_dark_blocks_range[1],
    "dark blocks totalling ~", r4(geom_sum$b_covered_hours_median[1]),
    "h of Active time across a ~", r4(geom_sum$b_span_hours_median[1]),
    "h wall-clock span (three ~", r4(geom_sum$b_max_inter_bin_gap_hours_median[1]),
    "h Inactive gaps), of which only ~", r4(100 * geom_sum$first_night_bin_fraction_median[1]),
    "% of the bins are the first night.\n")
cat("(b) IS NOT A FIRST-ENCOUNTER MEASURE and must never be labelled 'first night', 'first encounter',\n")
cat("'acute' or 'novelty' response: three of its four nights follow 12-60 h of habituation.\n")

sec("PRIMARY 10min, (b) = bridging reference, DISPLAYED DOMAINS")
print(as.data.frame(sens %>% filter(bin_resolution == "10min_based", b_variant == B_REFERENCE,
                                    metric_family == "displayed_domain") %>%
  transmute(metric = str_trunc(metric, 32), Sx = str_sub(Sex, 1, 1), contrast,
            r = r4(pearson_pooled),
            a = sprintf("%+.3f[%+.2f,%+.2f]p%.3f", estimate_a, ci_low_a, ci_high_a, p_value_a),
            b = sprintf("%+.3f[%+.2f,%+.2f]p%.3f", estimate_b, ci_low_b, ci_high_b, p_value_b),
            d = r4(signed_diff_estimate), pct = round(pct_diff_estimate),
            flip = direction_changes, ovl = ci_overlap, chg = statistical_conclusion_changes)),
  row.names = FALSE)

sec("PRIMARY 10min, (b) = bridging reference, HMM COMPONENTS (exploratory)")
print(as.data.frame(sens %>% filter(bin_resolution == "10min_based", b_variant == B_REFERENCE,
                                    metric_family == "hmm_component") %>%
  transmute(metric = str_trunc(metric, 28), Sx = str_sub(Sex, 1, 1), contrast,
            r = r4(pearson_pooled),
            a = sprintf("%+.3f p%.3f", estimate_a, p_value_a),
            b = sprintf("%+.3f p%.3f", estimate_b, p_value_b),
            d = r4(signed_diff_estimate), pct = round(pct_diff_estimate),
            flip = direction_changes, ovl = ci_overlap, chg = statistical_conclusion_changes)),
  row.names = FALSE)

sec("Bridging vs gap-aware inside window (b): how big is the bridging artefact? (10min)")
ba <- sens %>% filter(metric_is_temporal_order_dependent) %>%
  select(bin_resolution, metric, Sex, contrast, b_variant, estimate_b, pearson_pooled) %>%
  pivot_wider(names_from = b_variant, values_from = c(estimate_b, pearson_pooled))
print(as.data.frame(ba %>% filter(bin_resolution == "10min_based") %>%
  transmute(metric = str_trunc(metric, 30), Sx = str_sub(Sex, 1, 1), contrast,
            est_bridge = r4(estimate_b_bridging_stage08_current), est_gap = r4(estimate_b_gap_aware),
            diff = r4(estimate_b_gap_aware - estimate_b_bridging_stage08_current),
            r_bridge = r4(pearson_pooled_bridging_stage08_current),
            r_gap = r4(pearson_pooled_gap_aware))), row.names = FALSE)

sec("Headline stability counts (per resolution x b_variant x metric_family)")
print(as.data.frame(sens %>% group_by(bin_resolution, b_variant, metric_family) %>%
  summarise(n_cells = n(), n_direction_flips = sum(direction_changes),
            n_ci_nonoverlap = sum(!ci_overlap), n_cross_p05 = sum(crosses_p05),
            n_conclusion_changes = sum(statistical_conclusion_changes),
            median_abs_diff = r4(median(abs_diff_estimate)),
            max_abs_diff = r4(max(abs_diff_estimate)),
            median_abs_pct = round(median(abs(pct_diff_estimate))), .groups = "drop")),
  row.names = FALSE)

sec("KEY CELL: Female Latent-state persistence / mean_dwell_minutes (the longitudinal SUS phenotype), 10min")
print(as.data.frame(sens %>% filter(bin_resolution == "10min_based", Sex == "Female",
                                    metric %in% c("Latent-state persistence", "mean_dwell_minutes")) %>%
  transmute(metric, b_variant = str_trunc(b_variant, 24), contrast,
            a = sprintf("%+.3f [%+.2f,%+.2f] p=%.3f", estimate_a, ci_low_a, ci_high_a, p_value_a),
            b = sprintf("%+.3f [%+.2f,%+.2f] p=%.3f", estimate_b, ci_low_b, ci_high_b, p_value_b),
            pct = round(pct_diff_estimate), flip = direction_changes, ovl = ci_overlap)),
  row.names = FALSE)

sec("KEY CELL: Female Behavioral volatility / fragmentation (the only FDR-surviving (a) cell), 10min")
print(as.data.frame(sens %>% filter(bin_resolution == "10min_based", Sex == "Female",
                                    metric == "Behavioral volatility / fragmentation") %>%
  transmute(b_variant = str_trunc(b_variant, 24), contrast,
            a = sprintf("%+.3f [%+.2f,%+.2f] p=%.4f", estimate_a, ci_low_a, ci_high_a, p_value_a),
            b = sprintf("%+.3f [%+.2f,%+.2f] p=%.4f", estimate_b, ci_low_b, ci_high_b, p_value_b),
            pct = round(pct_diff_estimate), flip = direction_changes, ovl = ci_overlap,
            g_a = r4(hedges_g_a), g_b = r4(hedges_g_b))), row.names = FALSE)

sec("Assertions")
leak <- bind_rows(ASSERT)
write_csv(leak, file.path(OUT, "first_night_vs_all_cc1_active_assertions.csv"))
cat("assertions:", nrow(leak), " PASS =", sum(leak$result == "PASS"), " FAIL =", sum(leak$result == "FAIL"), "\n")
if (any(leak$result == "FAIL")) print(as.data.frame(leak %>% filter(result == "FAIL")), row.names = FALSE)

hr("DONE")
cat("OUT =", OUT, "\n")
for (f in c("first_night_vs_all_cc1_active_sensitivity.csv",
            "first_night_vs_all_cc1_active_correlations.csv",
            "first_night_vs_all_cc1_active_epoch_geometry.csv",
            "first_night_vs_all_cc1_active_assertions.csv"))
  cat("  ", f, " exists =", file.exists(file.path(OUT, f)), "\n")
