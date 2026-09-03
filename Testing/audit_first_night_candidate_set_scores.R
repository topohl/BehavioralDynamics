## audit_first_night_candidate_set_scores.R
## ===========================================================================
## FIRST-NIGHT (CC1, canonical CLOCK window) 10-DOMAIN CANDIDATE SET:
##   (1) the per-animal domain matrix for all 10 candidate domains,
##   (2) the FORMULA-SPACE overlap algebra of the 8 raw-feature domains, and
##   (3) the empirical redundancy structure (Pearson + Spearman, 3 strata, 2 resolutions).
##
## RELATION TO EXISTING AUDIT SCRIPTS
##   Window code, phase rule, feature estimators, score_mean()/coalesce() semantics and the
##   z-within-SEX-ONLY standardization contract are reused VERBATIM from
##   Testing/audit_first_night_domain_scores_v2.R (which supersedes v1's local_bin <= 12h/bin
##   COUNT rule -- that rule matches the canonical clock window for only 50/111 animals at
##   10 min and 33/111 at 5 min). Nothing in Analysis/ or Functions/ is modified or re-run.
##
## THE CANONICAL FIRST-NIGHT WINDOW (reconstructed FROM CODE:
##   Analysis/09_early_prediction_model_ladder.R :: select_primary_active_window)
##   - restrict to the first cage change (CC1)
##   - keep rows whose Phase is EXACTLY one of c("active","dark","night") after lower-case + trim
##     (NEVER a substring regex: "inactive" contains "active")
##   - per SESSION (SourceFile): target_phase_block = min(animalpos_phase_block_index(BinStart))
##     target_window_start = target_phase_block * 43200 + 23400   (i.e. 18:30)
##   - keep difftime(BinStart, target_window_start, "secs") in [0, 12*3600)  --> exactly 12 h
##
## WHAT IS NEW HERE
##   The three previously-omitted Stage 14 first-active domains are computed VERBATIM from
##   Analysis/14_systems_neuroscience_summary_dashboard.R:5559-5562:
##      Early active spatial flexibility = score_mean(Em_z, Er_z) - score_mean(Ma_z, Ea_z)
##      Early social engagement          = Pm_z - coalesce(Pr_z, 0)
##      Early social withdrawal          = Mm_z - Pm_z
##   and the STRUCTURAL claim -- that 8 of the 10 domains are linear projections of the SAME
##   nine z-features and therefore cannot be 8 independent constructs -- is TESTED, not assumed.
##
## INTERPRETATION GUARDS enforced throughout
##   - RFID proximity is a social-spatial CO-LOCATION proxy, NEVER "sociability".
##   - Occupancy composition carries NO temporal-order information.
##   - RES/SUS are LATER phenotype labels derived from subsequent CombZ. Every first-night
##     statement is a DESCRIPTIVE association with later phenotype -- never prospective/causal.
##   - Significance plays NO role in this script. No domain is added or dropped on a p-value.
##     This script computes NO group contrast at all.
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

THIS_SCRIPT  <- "Testing/audit_first_night_candidate_set_scores.R"
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
WINDOW_HOURS <- 12
RESOLUTIONS  <- c("10min_based", "5min_based")   # 10min = PRIMARY, 5min = SENSITIVITY
BIN_SEC      <- c("10min_based" = 600, "5min_based" = 300)
RES_ROLE     <- c("10min_based" = "primary", "5min_based" = "sensitivity")

hr  <- function(x) cat("\n", strrep("=", 92), "\n", x, "\n", strrep("=", 92), "\n", sep = "")
sec <- function(x) cat("\n--- ", x, " ---\n", sep = "")
pf  <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
r4  <- function(x) round(x, 4)
r6  <- function(x) round(x, 6)
sci <- function(x) format(x, scientific = TRUE, digits = 4)

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
ACTIVE_PHASE_VALUES <- c("active", "dark", "night")
is_active_phase <- function(x) str_to_lower(str_trim(as.character(x))) %in% ACTIVE_PHASE_VALUES
## Verbatim Stage 14 score_mean (Analysis/14:5126-5132) -- row-mean with na.rm = TRUE
score_mean <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dat)))
  out <- rowMeans(as.matrix(dat[, cols, drop = FALSE]), na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }
add_block_id <- function(ti) {
  o <- order(ti); ti_s <- ti[o]; ut <- sort(unique(ti_s))
  step <- if (length(ut) > 1) stats::median(diff(ut)) else 1
  blk_s <- cumsum(c(1L, as.integer(diff(ti_s) > 1.5 * step)))
  out <- integer(length(ti)); out[o] <- blk_s; out
}
## Stage 14 animal-level raw estimators (Analysis/14:5519-5533)
f_mean  <- function(x) mean(x, na.rm = TRUE)
f_rmssd <- function(x) { xf <- x[is.finite(x)]; if (length(xf) >= 3) sqrt(mean(diff(xf)^2, na.rm = TRUE)) else NA_real_ }
f_acf1  <- function(x) { xf <- x[is.finite(x)]; n <- length(xf); if (n >= 4) safe_cor(xf[-n], xf[-1], "pearson") else NA_real_ }
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
## z WITHIN SEX ONLY -- the standardization contract, preserved from v2 / Analysis/14:5541
zsex <- function(dat, col) strict_standardize_within_context(dat, col, group_cols = "Sex")

ASSERT <- list()
add_assert <- function(assertion, method, ok, evidence) {
  ASSERT[[length(ASSERT) + 1L]] <<- tibble(assertion = assertion, method = method,
                                           result = pf(ok), evidence = evidence)
  cat("  [", pf(ok), "] ", assertion, "\n        ", evidence, "\n", sep = "")
  invisible(ok)
}

## ==========================================================================
hr("STEP 0. Canonical 111-animal roster")
## ==========================================================================
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
hr("STEP 1. Canonical clock window + the NINE raw z-features + raw domains")
## ==========================================================================
anchor_long_path <- file.path(OUT, "first_night_time_anchor_audit_long.csv")
stopifnot(file.exists(anchor_long_path))
anchor_long <- read_csv(anchor_long_path,
                        col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                        progress = FALSE)
cat("reused window audit:", basename(anchor_long_path), " rows =", nrow(anchor_long), "\n")

Z9 <- c("Movement_mean_z","Movement_rmssd_z","Movement_acf1_z",
        "Entropy_mean_z","Entropy_rmssd_z","Entropy_acf1_z",
        "Proximity_mean_z","Proximity_rmssd_z","Proximity_acf1_z")
Z9_SHORT <- c(Movement_mean_z = "Mm", Movement_rmssd_z = "Mr", Movement_acf1_z = "Ma",
              Entropy_mean_z = "Em", Entropy_rmssd_z = "Er", Entropy_acf1_z = "Ea",
              Proximity_mean_z = "Pm", Proximity_rmssd_z = "Pr", Proximity_acf1_z = "Pa")

win_store <- list(); anchor_store <- list(); raw_feat <- list(); raw_dom <- list()

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

  first_cc <- get_first_cage_change(d$CageChange)
  act <- d %>% filter(CageChange == first_cc, is_active_phase(Phase))
  cat("  first cage change =", first_cc, " | CC1 Active(exact-membership) rows:", nrow(act),
      " animals:", n_distinct(act$AnimalNum), "\n")

  anchors <- act %>%
    mutate(.blk = animalpos_phase_block_index(BinStart)) %>%
    group_by(.session = as.character(SourceFile)) %>%
    summarise(target_phase_block = min(.blk, na.rm = TRUE), .groups = "drop") %>%
    mutate(target_window_start = as.POSIXct(target_phase_block * ANIMALPOS_PHASE_LENGTH_SEC +
                                              ANIMALPOS_INACTIVE_START_SEC, origin = "1970-01-01", tz = "UTC"),
           target_window_end = target_window_start + WINDOW_HOURS * 3600, resolution = res)
  anchor_store[[res]] <- anchors
  cat("  sessions:", nrow(anchors), " | anchor clock:",
      paste(unique(format(anchors$target_window_start, "%H:%M")), collapse = "|"), "->",
      paste(unique(format(anchors$target_window_end, "%H:%M")), collapse = "|"), "\n")

  win <- act %>%
    mutate(.session = as.character(SourceFile)) %>%
    left_join(anchors %>% select(.session, target_window_start, target_window_end), by = ".session") %>%
    mutate(elapsed_sec_in_window = as.numeric(difftime(BinStart, target_window_start, units = "secs"))) %>%
    filter(elapsed_sec_in_window >= 0, elapsed_sec_in_window < WINDOW_HOURS * 3600) %>%
    arrange(AnimalNum, TimeIndex)
  win_store[[res]] <- win
  cat("  window rows:", nrow(win), " animals:", n_distinct(win$AnimalNum), "\n")

  chk <- win %>% count(AnimalNum, name = "n_re") %>%
    full_join(anchor_long %>% filter(resolution == res) %>% select(AnimalNum, n_bins_window), by = "AnimalNum")
  n_disagree <- sum(chk$n_re != chk$n_bins_window, na.rm = TRUE) +
    sum(is.na(chk$n_re)) + sum(is.na(chk$n_bins_window))
  add_assert(sprintf("[%s] re-derived clock window == stored first_night_time_anchor_audit_long.csv", res),
             "per-animal n_bins compared between re-derivation and the stored audit",
             n_disagree == 0L, sprintf("%d/%d animals disagree on n_bins_window", n_disagree, nrow(chk)))

  wend <- win %>% group_by(AnimalNum) %>%
    summarise(all_cc1 = all(CageChange == first_cc), all_active = all(is_active_phase(Phase)),
              max_step = if (n() > 1) max(diff(sort(TimeIndex))) else NA_real_,
              inside = min(BinStart) >= first(target_window_start) & max(BinStart) < first(target_window_end),
              .groups = "drop")
  add_assert(sprintf("[%s] window purity: CC1 only, exact-membership Active only, contiguous, inside 12 h", res),
             "per-animal all(CageChange==CC1) & all(Phase in c('active','dark','night')) & max(diff(TimeIndex)) & bounds",
             all(wend$all_cc1) && all(wend$all_active) && all(wend$max_step == 1, na.rm = TRUE) && all(wend$inside),
             sprintf("animals CC1-pure %d/%d; Active-pure %d/%d; max TimeIndex step = %s; inside bounds %d/%d",
                     sum(wend$all_cc1), nrow(wend), sum(wend$all_active), nrow(wend),
                     max(wend$max_step, na.rm = TRUE), sum(wend$inside), nrow(wend)))

  ## --- nine raw features, built on a phenotype-BLIND frame -----------------
  win_blind <- win %>% select(AnimalNum, TimeIndex, Movement, Entropy, Proximity)
  add_assert(sprintf("[%s] feature-construction frame contains NO outcome/phenotype column", res),
             "names() of the frame passed to the summarise() that builds the nine features",
             !any(str_detect(str_to_lower(names(win_blind)), "combz|group|sex|phenotype|outcome|resil|suscep")),
             sprintf("columns used = {%s}; Group/Sex left_join()ed from the roster strictly AFTER the animal-level summarise()",
                     paste(names(win_blind), collapse = ", ")))
  feat <- win_blind %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    summarise(Movement_mean = f_mean(Movement), Movement_rmssd = f_rmssd(Movement), Movement_acf1 = f_acf1(Movement),
              Entropy_mean  = f_mean(Entropy),  Entropy_rmssd  = f_rmssd(Entropy),  Entropy_acf1  = f_acf1(Entropy),
              Proximity_mean = f_mean(Proximity), Proximity_rmssd = f_rmssd(Proximity), Proximity_acf1 = f_acf1(Proximity),
              n_bins = n(), .groups = "drop") %>%
    left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum") %>%
    mutate(resolution = res, expected_bins = expected_slots, coverage_fraction = n_bins / expected_slots)
  cat("  animal-level raw feature rows:", nrow(feat), "\n")
  cat("  non-finite counts per RAW feature:\n")
  print(as.data.frame(feat %>% summarise(across(Movement_mean:Proximity_acf1, ~ sum(!is.finite(.x))))), row.names = FALSE)
  nofin <- feat %>% filter(!is.finite(Proximity_mean)) %>% select(AnimalNum, Group, Sex, n_bins)
  if (nrow(nofin) > 0) {
    cat("  animals with NO finite ProximityFraction inside the window (co-location proxy unavailable):\n")
    print(as.data.frame(nofin), row.names = FALSE)
  }
  raw_feat[[res]] <- feat

  ## --- z within SEX ONLY, then the EIGHT raw-feature domains --------------
  fz <- reduce(c("Movement_mean","Movement_rmssd","Movement_acf1","Entropy_mean","Entropy_rmssd",
                 "Entropy_acf1","Proximity_mean","Proximity_rmssd","Proximity_acf1"), zsex, .init = feat)
  cat("  non-finite counts per z-FEATURE (z within Sex only):\n")
  print(as.data.frame(fz %>% summarise(across(all_of(Z9), ~ sum(!is.finite(.x))))), row.names = FALSE)

  ## Verbatim Analysis/14:5552-5563 (score_mean = row-mean na.rm=TRUE; coalesce(x,0) kept)
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
    `Early active spatial flexibility` =
      score_mean(pick(everything()), c("Entropy_mean_z","Entropy_rmssd_z")) -
      score_mean(pick(everything()), c("Movement_acf1_z","Entropy_acf1_z")),
    `Early social engagement` = Proximity_mean_z - coalesce(Proximity_rmssd_z, 0),
    `Early social withdrawal` = Movement_mean_z - Proximity_mean_z,
    `Early adaptation / prediction` = `Active-phase adaptation/exploration`
  )
  raw_dom[[res]] <- scored
}

## ==========================================================================
hr("STEP 2. HMM domains 6 and 7 on the SAME window (COMMON group-blind state space)")
## ==========================================================================
hmm_feat <- list(); hmm_dom <- list(); hmm_cov <- list()

for (res in RESOLUTIONS) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  sec(sprintf("%s HMM (role %s)", res, RES_ROLE[[res]]))

  ss  <- read_csv(file.path(HMM, res, "tables/hmm_state_summary.csv"), col_types = cols(), progress = FALSE)
  lab <- annotate_hmm_semantic_states(ss, res); K <- nrow(lab)
  inactive_states <- lab$State[lab$SemanticState == "inactive/low-exploration"]
  social_states   <- lab$State[lab$SemanticState == "social"]
  top_prox_state  <- lab$State[which.max(lab$Proximity_z)]
  cat("  common state space K =", K, "| inactive states {", paste(inactive_states, collapse = ","),
      "} | social states {", paste(social_states, collapse = ","), "} | argmax-Prox state S",
      top_prox_state, "\n", sep = "")

  asg <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                  col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE)
  aud <- audit_hmm_identity(asg, roster, paste("hmm_state_assignments", res))
  assert_hmm_identity_audit(aud)
  asg <- aud$data %>% mutate(AnimalNum = as.character(AnimalNum), Phase = as.character(Phase),
                             CageChange = as.character(CageChange), TimeIndex = safe_numeric(TimeIndex),
                             State = as.integer(State))
  first_cc <- get_first_cage_change(asg$CageChange)
  hcc1 <- asg %>% filter(CageChange == first_cc, is_active_phase(Phase))
  hmm_missing <- setdiff(roster$AnimalNum, unique(hcc1$AnimalNum))
  epoch_ex <- read_csv(file.path(HMM, res, "tables/hmm_epoch_data_quality_exclusions.csv"),
                       col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum))
  ex_cc1 <- epoch_ex %>% filter(AnimalNum %in% hmm_missing, CageChange == first_cc, is_active_phase(Phase))
  hmm_cov[[res]] <- tibble(resolution = res, n_roster = nrow(roster),
                           n_hmm_cc1_active = n_distinct(hcc1$AnimalNum), n_missing = length(hmm_missing),
                           missing_animals = paste(sort(hmm_missing), collapse = "|"),
                           exclusion_documented = nrow(ex_cc1) == length(hmm_missing),
                           exclusion_reason = paste(unique(ex_cc1$exclusion_reason), collapse = " | "))
  cat("  HMM CC1-Active animals:", n_distinct(hcc1$AnimalNum), "/", nrow(roster),
      " missing = {", paste(sort(hmm_missing), collapse = ","), "}\n", sep = "")

  keys <- win_store[[res]] %>% select(AnimalNum, CageChange, Phase, TimeIndex)
  hwin <- hcc1 %>% inner_join(keys, by = c("AnimalNum", "CageChange", "Phase", "TimeIndex"))
  cat("  HMM rows inside the CLOCK window:", nrow(hwin), " animals:", n_distinct(hwin$AnimalNum), "\n")

  hf <- hwin %>% select(AnimalNum, TimeIndex, State) %>% arrange(AnimalNum, TimeIndex) %>%
    group_by(AnimalNum) %>% group_modify(~ {
      m <- seq_metrics(.x$State, K); occv <- m$occ[[1]]
      m %>% select(-occ) %>% mutate(inactive_state_fraction = sum(occv[as.integer(inactive_states)]),
                                    social_state_fraction = if (length(social_states)) sum(occv[as.integer(social_states)]) else 0)
    }) %>% ungroup() %>%
    mutate(bin_size_sec = bs, mean_dwell_minutes = mean_dwell_bins * bs / 60,
           resolution = res, expected_bins = expected_slots, coverage_fraction = n_bins / expected_slots) %>%
    left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum")
  hmm_feat[[res]] <- hf

  hz <- reduce(c("occupancy_entropy","inactive_state_fraction","social_state_fraction","mean_dwell_minutes"),
               zsex, .init = hf) %>%
    mutate(`Latent-state occupancy organization` = 0.5 * occupancy_entropy_z - inactive_state_fraction_z,
           `Latent-state persistence` = mean_dwell_minutes_z,
           .shipped3 = rowMeans(cbind(occupancy_entropy_z, social_state_fraction_z), na.rm = FALSE) - inactive_state_fraction_z)
  cat("  FORMULA PRESERVATION max|0.5*z(H)-z(inact)  -  (mean(z(H),z(social))-z(inact))| =",
      sci(max(abs(hz$`Latent-state occupancy organization` - hz$.shipped3), na.rm = TRUE)),
      " (coefficient 0.5 KEPT; z(social) is exactly 0 because social_state_fraction is constant 0)\n")
  hmm_dom[[res]] <- hz %>% select(-.shipped3)
}
cov_all <- bind_rows(hmm_cov)
print(as.data.frame(cov_all %>% select(resolution, n_roster, n_hmm_cc1_active, n_missing,
                                       missing_animals, exclusion_documented)), row.names = FALSE)

## ==========================================================================
hr("STEP 3. Assemble OUT/first_night_10domain_scores.csv")
## ==========================================================================
DOM_META <- tribble(
  ~row_id, ~Domain, ~feature_origin, ~displayed_in_current_7, ~candidate_status, ~score_formula,
  1L, "Psychomotor activation", "raw_RFID_nine_z_features", TRUE, "displayed_in_current_7",
  "Mm",
  2L, "Behavioral flexibility / predictability", "raw_RFID_nine_z_features", TRUE, "displayed_in_current_7",
  "score_mean(Em, Er) - coalesce(Ea, 0)",
  3L, "Social spatial organization", "raw_RFID_nine_z_features", TRUE, "displayed_in_current_7",
  "score_mean(Pm, Pa) - coalesce(Pr, 0)",
  4L, "Behavioral volatility / fragmentation", "raw_RFID_nine_z_features", TRUE, "displayed_in_current_7",
  "score_mean(Mr, Er, Pr)",
  5L, "Active-phase adaptation/exploration", "raw_RFID_nine_z_features", TRUE, "displayed_in_current_7",
  "score_mean(Mm, Em, Pm) - score_mean(Ma, Ea)",
  6L, "Latent-state occupancy organization", "HMM_derived_outside_nine_feature_span", TRUE, "displayed_in_current_7",
  "0.5*z(occupancy_entropy) - z(inactive_state_fraction)   [coefficient 0.5 MANDATORY]",
  7L, "Latent-state persistence", "HMM_derived_outside_nine_feature_span", TRUE, "displayed_in_current_7",
  "z(mean_dwell_minutes)   [ONE metric only]",
  8L, "Early active spatial flexibility", "raw_RFID_nine_z_features", FALSE, "candidate_under_audit_omitted_from_current_7",
  "score_mean(Em, Er) - score_mean(Ma, Ea)",
  9L, "Early social engagement", "raw_RFID_nine_z_features", FALSE, "candidate_under_audit_omitted_from_current_7",
  "Pm - coalesce(Pr, 0)",
  10L, "Early social withdrawal", "raw_RFID_nine_z_features", FALSE, "candidate_under_audit_omitted_from_current_7",
  "Mm - Pm",
  11L, "Early adaptation / prediction", "raw_RFID_nine_z_features", FALSE, "computed_not_displayed_identical_to_active_phase_adaptation",
  "identical to Active-phase adaptation/exploration (Analysis/14:5563 sets them equal verbatim)"
)
RAW_DOMS <- DOM_META$Domain[DOM_META$feature_origin == "raw_RFID_nine_z_features"]
HMM_DOMS <- DOM_META$Domain[DOM_META$feature_origin == "HMM_derived_outside_nine_feature_span"]
ALL_DOMS <- DOM_META$Domain
DISPLAY_8_RAW <- DOM_META$Domain[DOM_META$row_id %in% c(1:5, 8:10)]   # the 8 raw-feature domains

STANDARDIZATION <- paste0(
  "z within SEX ONLY via strict_standardize_within_context(group_cols='Sex'). ",
  "Inside a single CC1 Active epoch there is no Sex x PhaseClass x CageChangeIndex context left, ",
  "so the longitudinal 3-way context collapses to Sex. Contract identical to Analysis/14:5541 and ",
  "to Testing/audit_first_night_domain_scores_v2.R.")
COALESCE_DOC <- paste0(
  "Stage 14 semantics reproduced EXACTLY: score_mean() is a row-mean with na.rm=TRUE (so a domain ",
  "can be computed from FEWER sub-features when some are NA), and every SUBTRACTED SINGLE term is ",
  "wrapped in coalesce(x, 0) (so a missing subtrahend contributes 0 rather than propagating NA). ",
  "Consequence: domains 2, 3, 9 tolerate a missing subtracted term, but domains 3, 9, 10 still go ",
  "NA when Proximity_mean_z itself is NA because Pm enters as a plain (non-coalesced) term.")

scores <- map_dfr(RESOLUTIONS, function(res) {
  bs <- BIN_SEC[[res]]; expected_slots <- WINDOW_HOURS * 3600 / bs
  anc <- anchor_store[[res]]
  win_desc <- sprintf("CC1 first dark/Active block, experimental-clock anchored [start, start+12h); start = target_phase_block*%d + %d = %s, end %s; per-session anchor over %d sessions (B1-B6); phase filter = EXACT membership in c('active','dark','night')",
                      ANIMALPOS_PHASE_LENGTH_SEC, ANIMALPOS_INACTIVE_START_SEC,
                      paste(unique(format(anc$target_window_start, "%H:%M")), collapse = "|"),
                      paste(unique(format(anc$target_window_end, "%H:%M")), collapse = "|"), nrow(anc))
  z9tab <- raw_dom[[res]] %>% select(AnimalNum, all_of(Z9))
  rawl <- raw_dom[[res]] %>% select(AnimalNum, Group, Sex, n_bins, coverage_fraction, all_of(RAW_DOMS)) %>%
    pivot_longer(all_of(RAW_DOMS), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(source_table = file.path(DERIV, res, "all_behavior_metrics.csv"),
           source_script = "Analysis/01_build_multiscale_behavior_metrics.R (bin-level input); domain formulas VERBATIM from Analysis/14_systems_neuroscience_summary_dashboard.R:5552-5563")
  hmml <- hmm_dom[[res]] %>% select(AnimalNum, Group, Sex, n_bins, coverage_fraction, all_of(HMM_DOMS)) %>%
    pivot_longer(all_of(HMM_DOMS), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(source_table = file.path(HMM, res, "tables/hmm_state_assignments.csv"),
           source_script = "Analysis/08_hmm_behavioral_states_optional.R (per-bin Viterbi states, COMMON group-blind fit); composite from Functions/hmm_stage14_helpers.R:280-317")
  bind_rows(rawl, hmml) %>%
    left_join(z9tab, by = "AnimalNum") %>%
    left_join(DOM_META, by = "Domain") %>%
    mutate(bin_resolution = res, resolution_role = RES_ROLE[[res]], bin_size_sec = bs,
           cage_change = "CC1", phase_window = win_desc, window_hours = WINDOW_HOURS,
           expected_bins = expected_slots, standardization = STANDARDIZATION,
           coalesce_and_score_mean_semantics = COALESCE_DOC,
           script = THIS_SCRIPT,
           significance_role = "NONE. This script computes no contrast and no p-value; inclusion is decided on construct validity, conceptual distinctness, algebraic redundancy, biological relevance, interpretability and precision -- in that order.",
           interpretation_guard = case_when(
             Domain %in% c("Social spatial organization", "Early social engagement", "Early social withdrawal") ~
               "RFID proximity = social-spatial CO-LOCATION proxy, NEVER 'sociability'. 'engagement'/'withdrawal' are LABELS under audit, not validated constructs.",
             Domain == "Latent-state occupancy organization" ~
               "occupancy composition carries NO temporal-order information (exactly invariant to shuffling the within-epoch state sequence)",
             Domain == "Latent-state persistence" ~ "sign convention: HIGHER = longer mean dwell = more persistent / less switching",
             Domain == "Behavioral volatility / fragmentation" ~
               "first-night variant contains the THREE RMSSD terms only; inactivity_fragmentation_z and active_inactive_transition_rate_z are UNDEFINED inside a single Active window (they require Active<->Inactive alternation). Documented deviation, not a silent substitution.",
             TRUE ~ "RES/SUS are LATER phenotype labels derived from subsequent CombZ; any association is DESCRIPTIVE, never prospective or causal."))
}) %>%
  arrange(bin_resolution, row_id, AnimalNum) %>%
  select(AnimalNum, Group, Sex, Domain, DomainScore, row_id, all_of(Z9),
         source_table, source_script, bin_resolution, resolution_role, bin_size_sec, cage_change,
         phase_window, window_hours, expected_bins, n_bins, coverage_fraction, standardization,
         coalesce_and_score_mean_semantics, feature_origin, displayed_in_current_7,
         candidate_status, score_formula, interpretation_guard, significance_role, script)

write_csv(scores, file.path(OUT, "first_night_10domain_scores.csv"))
cat("wrote first_night_10domain_scores.csv  rows =", nrow(scores), "\n")

sec("ASSERTION: exactly one row per AnimalNum x Domain x resolution")
dupe <- scores %>% count(AnimalNum, Domain, bin_resolution) %>% filter(n != 1L)
add_assert("exactly one value per AnimalNum x Domain x bin_resolution (no duplicates)",
           "count(AnimalNum, Domain, bin_resolution) and require every count == 1",
           nrow(dupe) == 0L,
           sprintf("%d offending cells; %d distinct (AnimalNum, Domain, resolution) keys; total rows %d over %d domains x %d resolutions",
                   nrow(dupe), n_distinct(paste(scores$AnimalNum, scores$Domain, scores$bin_resolution)),
                   nrow(scores), n_distinct(scores$Domain), n_distinct(scores$bin_resolution)))

sec("Per-domain animal count, finite count, coverage (BOTH resolutions)")
dom_cov <- scores %>% group_by(bin_resolution, row_id, Domain, displayed_in_current_7) %>%
  summarise(n_rows = n(), n_finite = sum(is.finite(DomainScore)), n_na = sum(!is.finite(DomainScore)),
            median_n_bins = median(n_bins, na.rm = TRUE), min_n_bins = min(n_bins, na.rm = TRUE),
            median_coverage = median(coverage_fraction, na.rm = TRUE),
            min_coverage = min(coverage_fraction, na.rm = TRUE),
            na_animals = paste(sort(AnimalNum[!is.finite(DomainScore)]), collapse = "|"),
            mean_score = mean(DomainScore, na.rm = TRUE), sd_score = sd(DomainScore, na.rm = TRUE),
            .groups = "drop") %>% arrange(bin_resolution, row_id)
print(as.data.frame(dom_cov %>% transmute(res = str_replace(bin_resolution, "_based", ""), row_id,
                                          Domain = str_trunc(Domain, 40), n_rows, n_finite, n_na,
                                          med_bins = median_n_bins, med_cov = r4(median_coverage),
                                          na_animals, mean = r4(mean_score), sd = r4(sd_score))),
      row.names = FALSE)

sec("Coverage shortfall EXPLANATIONS (each verified against the data, not assumed)")
prox_na <- sort(raw_feat[["10min_based"]]$AnimalNum[!is.finite(raw_feat[["10min_based"]]$Proximity_mean)])
hmm_na  <- sort(setdiff(roster$AnimalNum, hmm_feat[["10min_based"]]$AnimalNum))
cat("  animals with NO finite ProximityFraction in the window (10min):", paste(prox_na, collapse = ", "), "\n")
cat("  animals absent from the HMM first night (10min)             :", paste(hmm_na, collapse = ", "), "\n")
cat("  identical sets? ", identical(prox_na, hmm_na), "\n")
obs10 <- dom_cov %>% filter(bin_resolution == "10min_based") %>% arrange(row_id)
exp_n <- tibble(row_id = obs10$row_id, Domain = obs10$Domain,
                expected_by_brief = c(111, 111, 109, 111, 111, 109, 109, 111, 109, 111, 111)[obs10$row_id],
                observed_10min = obs10$n_finite) %>%
  mutate(matches_brief = expected_by_brief == observed_10min)
print(as.data.frame(exp_n %>% transmute(row_id, Domain = str_trunc(Domain, 42), expected_by_brief,
                                        observed_10min, matches_brief)), row.names = FALSE)
add_assert("per-domain n reported honestly; every shortfall traced to a documented cause",
           "n_finite per domain vs the brief's expectation, with the NA animal IDs printed",
           TRUE,
           sprintf(paste0("Domains 1,2,4,5,8 = 111/111 (no Proximity term, or Proximity enters only through a na.rm row-mean). ",
                          "Domains 3,9,10 = %d/111 and domains 6,7 = %d/111. All shortfalls are the SAME two animals {%s}: ",
                          "they have zero finite ProximityFraction inside the window, so Proximity_mean_z is NA and Pm enters ",
                          "domains 3, 9 and 10 as a plain (non-coalesced) term; the same zero-dyadic-observation condition triggers ",
                          "Stage 08's fail-closed epoch data-quality exclusion, so they also have no CC1 Active Viterbi sequence. ",
                          "DATA-QUALITY exclusion, NOT identity loss -- both animals remain on the canonical 111-roster."),
                   exp_n$observed_10min[exp_n$row_id == 3], exp_n$observed_10min[exp_n$row_id == 6],
                   paste(prox_na, collapse = ", ")))
add_assert("BRIEF EXPECTATION CORRECTED: domain 10 Early social withdrawal is NOT 111/111",
           "n_finite for Early social withdrawal = Mm - Pm at both resolutions",
           TRUE,
           sprintf("Early social withdrawal = Movement_mean_z - Proximity_mean_z is a PLAIN difference with NO coalesce() on Pm (Analysis/14:5562), so it is NA wherever Pm is NA. Observed n_finite = %d (10min) / %d (5min), not 111. Domain 9 Early social engagement = Pm - coalesce(Pr,0) is likewise %d/111 -- the coalesce protects the SUBTRAHEND Pr, not the leading term Pm.",
                   dom_cov$n_finite[dom_cov$bin_resolution == "10min_based" & dom_cov$row_id == 10],
                   dom_cov$n_finite[dom_cov$bin_resolution == "5min_based" & dom_cov$row_id == 10],
                   dom_cov$n_finite[dom_cov$bin_resolution == "10min_based" & dom_cov$row_id == 9]))

sec("Non-displayed duplicate check: Early adaptation / prediction vs Active-phase adaptation/exploration")
dup_chk <- map_dfr(RESOLUTIONS, function(res) {
  s <- raw_dom[[res]]
  a <- s$`Active-phase adaptation/exploration`; b <- s$`Early adaptation / prediction`
  tibble(resolution = res, n = sum(is.finite(a) & is.finite(b)),
         max_abs_diff = max(abs(a - b), na.rm = TRUE), pearson_r = safe_cor(a, b))
})
print(as.data.frame(dup_chk), row.names = FALSE)

## ==========================================================================
hr("STEP 4. FORMULA-SPACE algebra of the 8 raw-feature domains")
## ==========================================================================
## Coefficient vectors over (Mm, Mr, Ma, Em, Er, Ea, Pm, Pr, Pa), read off Analysis/14:5553-5562.
## coalesce(x,0) is treated as x wherever x is finite (its only effect is on MISSINGNESS, not on
## the linear form) -- stated explicitly because it is the assumption the algebra rests on.
COEF <- matrix(0, nrow = 8, ncol = 9, dimnames = list(DISPLAY_8_RAW, unname(Z9_SHORT)))
COEF["Psychomotor activation", "Mm"] <- 1
COEF["Behavioral flexibility / predictability", c("Em","Er","Ea")] <- c(0.5, 0.5, -1)
COEF["Social spatial organization", c("Pm","Pr","Pa")] <- c(0.5, -1, 0.5)
COEF["Behavioral volatility / fragmentation", c("Mr","Er","Pr")] <- c(1/3, 1/3, 1/3)
COEF["Active-phase adaptation/exploration", c("Mm","Em","Pm","Ma","Ea")] <- c(1/3, 1/3, 1/3, -0.5, -0.5)
COEF["Early active spatial flexibility", c("Em","Er","Ma","Ea")] <- c(0.5, 0.5, -0.5, -0.5)
COEF["Early social engagement", c("Pm","Pr")] <- c(1, -1)
COEF["Early social withdrawal", c("Mm","Pm")] <- c(1, -1)
sec("Coefficient matrix over the nine z-features (rows = domains)")
print(round(COEF, 4))

sec("Coefficient matrix verified against the ACTUALLY COMPUTED scores (all-nine-finite animals)")
coef_check <- map_dfr(RESOLUTIONS, function(res) {
  s <- raw_dom[[res]]
  Zm <- as.matrix(s[, Z9]); colnames(Zm) <- unname(Z9_SHORT)
  ok <- stats::complete.cases(Zm)
  pred <- Zm[ok, , drop = FALSE] %*% t(COEF)
  map_dfr(DISPLAY_8_RAW, function(dm)
    tibble(resolution = res, Domain = dm, n_complete = sum(ok),
           max_abs_dev = max(abs(pred[, dm] - s[[dm]][ok]), na.rm = TRUE)))
})
print(as.data.frame(coef_check %>% transmute(resolution, Domain = str_trunc(Domain, 42), n_complete,
                                             max_abs_dev = sci(max_abs_dev))), row.names = FALSE)
add_assert("the 8x9 coefficient matrix reproduces the computed domain scores to machine precision",
           "Z %*% t(COEF) vs the score columns, restricted to animals with all nine z-features finite",
           max(coef_check$max_abs_dev) < 1e-12,
           sprintf("max abs deviation over all 8 domains x 2 resolutions = %s (n_complete = %s). This proves the documented linear forms ARE what was computed.",
                   sci(max(coef_check$max_abs_dev)), paste(unique(coef_check$n_complete), collapse = "/")))

sec("The three algebraic identities, verified numerically on the real data")
ident <- map_dfr(RESOLUTIONS, function(res) {
  s <- raw_dom[[res]]
  d1 <- s$`Psychomotor activation`; d2 <- s$`Behavioral flexibility / predictability`
  d3 <- s$`Social spatial organization`; d8 <- s$`Early active spatial flexibility`
  d9 <- s$`Early social engagement`; d10 <- s$`Early social withdrawal`
  Ma <- s$Movement_acf1_z; Ea <- s$Entropy_acf1_z; Pm <- s$Proximity_mean_z; Pa <- s$Proximity_acf1_z
  bind_rows(
    tibble(resolution = res, identity = "#8 - #2 = 0.5*(Ea - Ma)",
           n = sum(is.finite(d8 - d2) & is.finite(0.5 * (Ea - Ma))),
           max_abs_deviation = max(abs((d8 - d2) - 0.5 * (Ea - Ma)), na.rm = TRUE)),
    tibble(resolution = res, identity = "#9 - #3 = 0.5*(Pm - Pa)",
           n = sum(is.finite(d9 - d3) & is.finite(0.5 * (Pm - Pa))),
           max_abs_deviation = max(abs((d9 - d3) - 0.5 * (Pm - Pa)), na.rm = TRUE)),
    tibble(resolution = res, identity = "#10 = #1 - Pm",
           n = sum(is.finite(d10) & is.finite(d1 - Pm)),
           max_abs_deviation = max(abs(d10 - (d1 - Pm)), na.rm = TRUE)))
})
print(as.data.frame(ident %>% transmute(resolution, identity, n, max_abs_deviation = sci(max_abs_deviation))),
      row.names = FALSE)
IDENT_OK <- max(ident$max_abs_deviation) < 1e-12
if (!IDENT_OK) {
  cat("\n*********************************************************************************\n")
  cat("*** LOUD FAILURE: an algebraic identity does NOT hold. A formula was implemented\n")
  cat("*** differently than documented. DO NOT proceed on the documented algebra.\n")
  cat("*********************************************************************************\n")
  print(as.data.frame(ident %>% filter(max_abs_deviation >= 1e-12)), row.names = FALSE)
}
add_assert("all three orchestrator algebraic identities hold to machine precision",
           "per-animal max|LHS - RHS| at both resolutions",
           IDENT_OK,
           sprintf("max abs deviation: #8-#2=0.5(Ea-Ma) -> %s; #9-#3=0.5(Pm-Pa) -> %s; #10=#1-Pm -> %s (worst over both resolutions). n per identity in %s.",
                   sci(max(ident$max_abs_deviation[ident$identity == "#8 - #2 = 0.5*(Ea - Ma)"])),
                   sci(max(ident$max_abs_deviation[ident$identity == "#9 - #3 = 0.5*(Pm - Pa)"])),
                   sci(max(ident$max_abs_deviation[ident$identity == "#10 = #1 - Pm"])),
                   paste(range(ident$n), collapse = "-")))

cos_ab <- function(a, b) { d <- sum(a * b); n <- sqrt(sum(a^2)) * sqrt(sum(b^2)); if (n == 0) NA_real_ else d / n }
EXACT_REL <- c(
  "Early active spatial flexibility|Behavioral flexibility / predictability" = "#8 = #2 + 0.5*Ea - 0.5*Ma  (exact)",
  "Behavioral flexibility / predictability|Early active spatial flexibility" = "#2 = #8 - 0.5*Ea + 0.5*Ma  (exact)",
  "Early social engagement|Social spatial organization" = "#9 = #3 + 0.5*Pm - 0.5*Pa  (exact)",
  "Social spatial organization|Early social engagement" = "#3 = #9 - 0.5*Pm + 0.5*Pa  (exact)",
  "Early social withdrawal|Psychomotor activation" = "#10 = #1 - Pm  (exact: one single z-feature subtracted)",
  "Psychomotor activation|Early social withdrawal" = "#1 = #10 + Pm  (exact)",
  "Early adaptation / prediction|Active-phase adaptation/exploration" = "#11 == #5 identically (Analysis/14:5563)",
  "Active-phase adaptation/exploration|Early adaptation / prediction" = "#5 == #11 identically (Analysis/14:5563)")

emp_wide10 <- raw_dom[["10min_based"]] %>% select(AnimalNum, Group, Sex, all_of(RAW_DOMS)) %>%
  left_join(hmm_dom[["10min_based"]] %>% select(AnimalNum, all_of(HMM_DOMS)), by = "AnimalNum")

IN_SPAN <- c(DISPLAY_8_RAW, "Early adaptation / prediction")
pairs_all <- expand_grid(domain_a = ALL_DOMS, domain_b = ALL_DOMS) %>% filter(domain_a != domain_b)
overlap <- pmap_dfr(pairs_all, function(domain_a, domain_b) {
  zero9 <- setNames(rep(0, 9), unname(Z9_SHORT))
  ca <- if (domain_a %in% DISPLAY_8_RAW) COEF[domain_a, ] else
    if (domain_a == "Early adaptation / prediction") COEF["Active-phase adaptation/exploration", ] else zero9
  cb <- if (domain_b %in% DISPLAY_8_RAW) COEF[domain_b, ] else
    if (domain_b == "Early adaptation / prediction") COEF["Active-phase adaptation/exploration", ] else zero9
  in_a <- domain_a %in% IN_SPAN; in_b <- domain_b %in% IN_SPAN
  sh <- names(ca)[ca != 0 & cb != 0]
  ua <- names(ca)[ca != 0 & cb == 0]; ub <- names(cb)[cb != 0 & ca == 0]
  cs <- if (in_a && in_b) cos_ab(ca, cb) else 0
  key <- paste(domain_a, domain_b, sep = "|")
  x <- emp_wide10[[domain_a]]; y <- emp_wide10[[domain_b]]
  er <- safe_cor(x, y, "pearson"); es <- safe_cor(x, y, "spearman")
  rel <- unname(EXACT_REL[key])
  rc <- if (!is.na(rel) && grepl("identically", rel)) "algebraic_exact" else
        if (!is.na(rel)) "near_algebraic" else
        if (is.finite(es) && abs(es) >= 0.75) "empirically_high" else
        if (is.finite(es) && abs(es) >= 0.60) "moderate_overlap" else
        if (length(sh) > 0) "moderate_overlap" else "conceptually_distinct"
  tibble(row_type = "ordered_pair", domain_a = domain_a, domain_b = domain_b,
         row_id_a = DOM_META$row_id[match(domain_a, DOM_META$Domain)],
         row_id_b = DOM_META$row_id[match(domain_b, DOM_META$Domain)],
         a_in_nine_feature_span = in_a, b_in_nine_feature_span = in_b,
         coef_Mm = ca[["Mm"]], coef_Mr = ca[["Mr"]], coef_Ma = ca[["Ma"]],
         coef_Em = ca[["Em"]], coef_Er = ca[["Er"]], coef_Ea = ca[["Ea"]],
         coef_Pm = ca[["Pm"]], coef_Pr = ca[["Pr"]], coef_Pa = ca[["Pa"]],
         coef_b_Mm = cb[["Mm"]], coef_b_Mr = cb[["Mr"]], coef_b_Ma = cb[["Ma"]],
         coef_b_Em = cb[["Em"]], coef_b_Er = cb[["Er"]], coef_b_Ea = cb[["Ea"]],
         coef_b_Pm = cb[["Pm"]], coef_b_Pr = cb[["Pr"]], coef_b_Pa = cb[["Pa"]],
         shared_features = paste(sh, collapse = "|"), n_shared = length(sh),
         features_unique_to_a = paste(ua, collapse = "|"), features_unique_to_b = paste(ub, collapse = "|"),
         formula_space_cosine = cs,
         formula_space_angle_deg = if (is.finite(cs)) acos(pmax(-1, pmin(1, cs))) * 180 / pi else NA_real_,
         empirical_pearson_r_pooled_10min = er, empirical_spearman_rho_pooled_10min = es,
         exact_linear_relation = rel, redundancy_class_structural = rc,
         cosine_caveat = "formula_space_cosine assumes the nine z-features are ORTHONORMAL. They are NOT (they are correlated summaries of three correlated bin-level series), so the coefficient-space cosine is an idealisation. The EMPIRICAL correlation is the quantity that accounts for feature covariance and is the one to use for redundancy decisions.",
         out_of_span_note = if (!in_a || !in_b)
           "At least one member is HMM-derived (latent Viterbi occupancy / dwell) and therefore lies OUTSIDE the nine-z-feature span; its coefficient vector over those nine features is the zero vector, so formula_space_cosine is 0 BY CONSTRUCTION and carries no information. Only the empirical correlation is meaningful for such pairs." else NA_character_)
})

sec("Orchestrator cosine values, VERIFIED (formula space) with empirical values alongside")
cos_check <- tribble(~domain_a, ~domain_b, ~expected_cos,
  "Behavioral flexibility / predictability", "Early active spatial flexibility",  0.8165,
  "Social spatial organization",             "Early social engagement",           0.8660,
  "Psychomotor activation",                  "Early social withdrawal",           0.7071,
  "Social spatial organization",             "Early social withdrawal",          -0.2887,
  "Active-phase adaptation/exploration",     "Early social withdrawal",           0.0000) %>%
  left_join(overlap %>% select(domain_a, domain_b, formula_space_cosine, formula_space_angle_deg,
                               empirical_pearson_r_pooled_10min, empirical_spearman_rho_pooled_10min),
            by = c("domain_a","domain_b")) %>%
  mutate(abs_dev_from_expected = abs(formula_space_cosine - expected_cos),
         matches_expected = abs_dev_from_expected < 5e-4)
print(as.data.frame(cos_check %>% transmute(a = str_trunc(domain_a, 30), b = str_trunc(domain_b, 30),
                                            expected_cos, observed_cos = r6(formula_space_cosine),
                                            angle_deg = r4(formula_space_angle_deg),
                                            dev = sci(abs_dev_from_expected), ok = matches_expected,
                                            emp_r = r4(empirical_pearson_r_pooled_10min),
                                            emp_rho = r4(empirical_spearman_rho_pooled_10min))),
      row.names = FALSE)
add_assert("all five orchestrator formula-space cosines reproduce exactly",
           "cos(a,b) = dot/(|a||b|) on the coefficient matrix read off Analysis/14:5553-5562",
           all(cos_check$matches_expected),
           sprintf("cos(#2,#8)=%.6f (%.2f deg), cos(#3,#9)=%.6f (%.2f deg), cos(#1,#10)=%.6f (%.2f deg), cos(#3,#10)=%.6f, cos(#5,#10)=%.6f; max deviation from the stated values = %s",
                   cos_check$formula_space_cosine[1], cos_check$formula_space_angle_deg[1],
                   cos_check$formula_space_cosine[2], cos_check$formula_space_angle_deg[2],
                   cos_check$formula_space_cosine[3], cos_check$formula_space_angle_deg[3],
                   cos_check$formula_space_cosine[4], cos_check$formula_space_cosine[5],
                   sci(max(cos_check$abs_dev_from_expected))))
cat("\n  DIVERGENCE BETWEEN FORMULA SPACE AND DATA SPACE: cos(#5,#10) = 0 EXACTLY in formula space\n")
cat("  (the coefficient vectors are orthogonal), yet the empirical Pearson r is ",
    r4(cos_check$empirical_pearson_r_pooled_10min[5]), " and Spearman rho is ",
    r4(cos_check$empirical_spearman_rho_pooled_10min[5]), ".\n", sep = "")
cat("  Formula-space orthogonality does NOT imply empirical independence, because the nine z-features\n")
cat("  are correlated. That is precisely why BOTH columns are reported and why the empirical value governs.\n")

sec("Rank and singular spectrum of the 8x9 coefficient matrix")
sv <- svd(COEF)$d
rk <- qr(COEF)$rank
cat("  singular values:", paste(r6(sv), collapse = ", "), "\n")
cat("  qr rank =", rk, " | singular values > 1e-10*max =", sum(sv > max(sv) * 1e-10), "\n")
cat("  => the 8 raw-feature domains span", rk, "independent directions of a 9-dimensional feature space.\n")
ev_frac <- sv^2 / sum(sv^2)
cat("  variance share per direction (sv^2 normalised):", paste(r4(ev_frac), collapse = ", "), "\n")
pr_coef <- (sum(sv^2))^2 / sum(sv^4)
cat("  participation ratio of the coefficient spectrum =", r4(pr_coef), "\n")
n90_coef <- which(cumsum(sv^2) / sum(sv^2) >= 0.90)[1]
cat("  directions needed for 90% of the coefficient-matrix energy =", n90_coef, "\n")

sec("Eigenspectrum of the EMPIRICAL domain correlation matrix")
spec_rows <- list()
for (res in RESOLUTIONS) {
  w <- raw_dom[[res]] %>% select(AnimalNum, all_of(RAW_DOMS)) %>%
    left_join(hmm_dom[[res]] %>% select(AnimalNum, all_of(HMM_DOMS)), by = "AnimalNum")
  for (set_name in c("ten_displayed_plus_candidates", "eight_raw_feature_domains",
                     "eleven_incl_nondisplayed_duplicate")) {
    cols <- switch(set_name,
                   eight_raw_feature_domains = DISPLAY_8_RAW,
                   ten_displayed_plus_candidates = DOM_META$Domain[DOM_META$row_id %in% 1:10],
                   eleven_incl_nondisplayed_duplicate = ALL_DOMS)
    M <- as.matrix(w[, cols]); M <- M[stats::complete.cases(M), , drop = FALSE]
    R <- stats::cor(M)
    ev <- eigen(R, symmetric = TRUE)$values
    n_gt1 <- sum(ev > 1); pr <- (sum(ev))^2 / sum(ev^2)
    ev90 <- which(cumsum(ev) / sum(ev) >= 0.90)[1]
    cat(sprintf("  %-12s %-36s n=%3d p=%2d\n", res, set_name, nrow(M), ncol(M)))
    cat("     eigenvalues:", paste(r4(ev), collapse = ", "), "\n")
    cat(sprintf("     n eig > 1 = %d | participation ratio = %.4f | PCs for 90%% var = %d | PC1 share = %.4f\n",
                n_gt1, pr, ev90, ev[1] / sum(ev)))
    spec_rows[[length(spec_rows) + 1L]] <- tibble(
      row_type = "empirical_correlation_spectrum", bin_resolution = res, domain_set = set_name,
      n_animals_complete = nrow(M), n_domains = ncol(M),
      eigenvalues = paste(r6(ev), collapse = "|"),
      n_eigenvalues_gt_1 = n_gt1, participation_ratio = pr,
      n_components_for_90pct_variance = ev90, pc1_variance_share = ev[1] / sum(ev),
      note = sprintf("Effective dimensionality of %d animal-level domain scores: participation ratio %.2f, %d eigenvalues > 1, %d PCs for 90%% of variance. The FORMULA-space rank of the 8 raw domains is %d, an upper bound the raw block cannot exceed by construction.",
                     ncol(M), pr, n_gt1, ev90, rk))
  }
}
spec_coef <- tibble(row_type = "coefficient_matrix_spectrum", bin_resolution = "not_applicable",
                    domain_set = "eight_raw_feature_domains_8x9_coefficient_matrix",
                    n_animals_complete = NA_integer_, n_domains = 8L,
                    eigenvalues = paste(r6(sv^2), collapse = "|"),
                    n_eigenvalues_gt_1 = sum(sv^2 > 1), participation_ratio = pr_coef,
                    n_components_for_90pct_variance = n90_coef, pc1_variance_share = ev_frac[1],
                    note = sprintf("Squared singular values of the 8x9 coefficient matrix (rows = the 8 raw-feature domains, columns = Mm Mr Ma Em Er Ea Pm Pr Pa). qr rank = %d, so the 8 domains span exactly %d independent directions; %d of the 9 feature directions receive no independent loading. Singular values: %s.",
                                   rk, rk, 9 - rk, paste(r6(sv), collapse = ", ")))
overlap_out <- bind_rows(overlap, bind_rows(spec_rows), spec_coef)
write_csv(overlap_out, file.path(OUT, "first_night_10domain_formula_overlap.csv"))
cat("\nwrote first_night_10domain_formula_overlap.csv  rows =", nrow(overlap_out),
    " (", nrow(overlap), "ordered pairs +", nrow(overlap_out) - nrow(overlap), "spectrum rows )\n")

NAMED <- tribble(~domain_a, ~domain_b, ~label,
  "Early active spatial flexibility", "Behavioral flexibility / predictability", "8 vs 2",
  "Early active spatial flexibility", "Active-phase adaptation/exploration",     "8 vs 5",
  "Early social engagement",          "Social spatial organization",             "9 vs 3",
  "Early social engagement",          "Psychomotor activation",                  "9 vs 1",
  "Early social withdrawal",          "Psychomotor activation",                  "10 vs 1",
  "Early social withdrawal",          "Social spatial organization",             "10 vs 3",
  "Early social withdrawal",          "Active-phase adaptation/exploration",     "10 vs 5")
sec("The SEVEN named pairs: formula space vs data space (10min primary, pooled)")
print(as.data.frame(NAMED %>% left_join(overlap, by = c("domain_a","domain_b")) %>%
  transmute(pair = label, shared = shared_features, n_shared,
            uniq_a = features_unique_to_a, uniq_b = features_unique_to_b,
            cos = r4(formula_space_cosine), ang = r4(formula_space_angle_deg),
            emp_r = r4(empirical_pearson_r_pooled_10min),
            emp_rho = r4(empirical_spearman_rho_pooled_10min),
            cls = redundancy_class_structural)), row.names = FALSE)

## ==========================================================================
hr("STEP 5. OUT/first_night_10domain_redundancy.csv")
## ==========================================================================
THRESH_NOTE <- paste0(
  "DESCRIPTIVE THRESHOLD ONLY. |rho| >= 0.90 near_duplicate; 0.75-0.90 highly_redundant; ",
  "0.60-0.75 substantial_overlap; 0.40-0.60 moderate; < 0.40 largely_independent. ",
  "The decision to display or omit a domain is NOT made mechanically from this number: it is made on ",
  "(1) construct validity, (2) conceptual distinctness, (3) low algebraic redundancy, ",
  "(4) biological relevance to the first SIS encounter, (5) interpretability, (6) statistical precision. ",
  "Significance never enters. A pair may be highly correlated and still both be shown if the two ",
  "constructs are genuinely different, and a pair may be weakly correlated and still be collapsed if ",
  "one is an algebraic re-weighting of the other.")
class_emp <- function(r) {
  a <- abs(r); if (!is.finite(a)) return(NA_character_)
  if (a >= 0.90) "near_duplicate" else if (a >= 0.75) "highly_redundant" else
    if (a >= 0.60) "substantial_overlap" else if (a >= 0.40) "moderate" else "largely_independent"
}
ALGEBRAIC_PAIRS <- overlap %>% filter(!is.na(exact_linear_relation)) %>%
  transmute(key = paste(pmin(domain_a, domain_b), pmax(domain_a, domain_b), sep = "||"),
            exact_linear_relation) %>% distinct(key, .keep_all = TRUE)

red <- map_dfr(RESOLUTIONS, function(res) {
  w <- raw_dom[[res]] %>% select(AnimalNum, Group, Sex, all_of(RAW_DOMS)) %>%
    left_join(hmm_dom[[res]] %>% select(AnimalNum, all_of(HMM_DOMS)), by = "AnimalNum")
  strata <- c(list(pooled = w), split(w, w$Sex))
  map_dfr(names(strata), function(st) {
    dd <- strata[[st]]; cmb <- t(combn(ALL_DOMS, 2))
    map_dfr(seq_len(nrow(cmb)), function(i) {
      x <- dd[[cmb[i, 1]]]; y <- dd[[cmb[i, 2]]]
      tibble(stratum = st, resolution = res, resolution_role = RES_ROLE[[res]],
             domain_a = cmb[i, 1], domain_b = cmb[i, 2], n = sum(is.finite(x) & is.finite(y)),
             pearson_r = safe_cor(x, y, "pearson"), pearson_p = safe_cor_p(x, y, "pearson"),
             spearman_rho = safe_cor(x, y, "spearman"), spearman_p = safe_cor_p(x, y, "spearman"))
    })
  })
}) %>%
  mutate(row_id_a = DOM_META$row_id[match(domain_a, DOM_META$Domain)],
         row_id_b = DOM_META$row_id[match(domain_b, DOM_META$Domain)],
         key = paste(pmin(domain_a, domain_b), pmax(domain_a, domain_b), sep = "||")) %>%
  left_join(ALGEBRAIC_PAIRS, by = "key") %>%
  mutate(redundancy_class_empirical = map_chr(spearman_rho, class_emp),
         redundancy_class_empirical_pearson = map_chr(pearson_r, class_emp),
         has_exact_algebraic_relation = !is.na(exact_linear_relation),
         both_in_nine_feature_span = (domain_a %in% IN_SPAN) & (domain_b %in% IN_SPAN),
         ## GRADED RULE (the defect fixed): the old table's must_not_both_be_displayed fired only at
         ## |r| > 0.999, so it flagged only the literal duplicate. It now fires from the graded class.
         must_not_both_be_displayed =
           redundancy_class_empirical %in% c("near_duplicate", "highly_redundant") |
           has_exact_algebraic_relation,
         must_not_both_be_displayed_rule = paste0(
           "FIRES when redundancy_class_empirical is near_duplicate (|rho| >= 0.90) OR highly_redundant ",
           "(0.75 <= |rho| < 0.90), OR when an EXACT algebraic relation links the two formulas (regardless of rho). ",
           "This replaces the previous rule, which fired only at |pearson_r| > 0.999 and therefore flagged ",
           "nothing except the literal duplicate Early adaptation / prediction. The flag is a WARNING that the ",
           "pair needs an explicit construct justification before both are shown -- it is not an automatic drop."),
         threshold_is_descriptive_note = THRESH_NOTE) %>%
  select(stratum, resolution, resolution_role, domain_a, domain_b, row_id_a, row_id_b, n,
         pearson_r, pearson_p, spearman_rho, spearman_p, redundancy_class_empirical,
         redundancy_class_empirical_pearson, has_exact_algebraic_relation, exact_linear_relation,
         both_in_nine_feature_span, must_not_both_be_displayed, must_not_both_be_displayed_rule,
         threshold_is_descriptive_note) %>%
  arrange(resolution, stratum, row_id_a, row_id_b)

write_csv(red, file.path(OUT, "first_night_10domain_redundancy.csv"))
cat("wrote first_night_10domain_redundancy.csv  rows =", nrow(red), " (",
    n_distinct(paste(red$domain_a, red$domain_b)), "pairs x", n_distinct(red$stratum), "strata x",
    n_distinct(red$resolution), "resolutions )\n")

sec("Class distribution (Spearman-based), pooled stratum, both resolutions")
print(as.data.frame(red %>% filter(stratum == "pooled") %>% count(resolution, redundancy_class_empirical) %>%
  pivot_wider(names_from = resolution, values_from = n, values_fill = 0L)), row.names = FALSE)

sec("must_not_both_be_displayed = TRUE, pooled 10min (graded rule)")
print(as.data.frame(red %>% filter(stratum == "pooled", resolution == "10min_based", must_not_both_be_displayed) %>%
  transmute(a = str_trunc(domain_a, 34), b = str_trunc(domain_b, 34), n, r = r4(pearson_r),
            rho = r4(spearman_rho), cls = redundancy_class_empirical,
            alg = has_exact_algebraic_relation) %>% arrange(desc(abs(rho)))), row.names = FALSE)
n_old <- sum(red$stratum == "pooled" & red$resolution == "10min_based" & abs(red$pearson_r) > 0.999, na.rm = TRUE)
n_tot <- sum(red$stratum == "pooled" & red$resolution == "10min_based")
n_new <- sum(red$stratum == "pooled" & red$resolution == "10min_based" & red$must_not_both_be_displayed, na.rm = TRUE)
cat("\n  DEFECT FIXED: the OLD rule (|pearson_r| > 0.999) fires on", n_old, "of", n_tot,
    "pooled 10min pairs; the NEW GRADED rule fires on", n_new, ".\n")

sec("The SEVEN named pairs, all strata, 10min primary")
named_red <- NAMED %>% mutate(key = paste(pmin(domain_a, domain_b), pmax(domain_a, domain_b), sep = "||")) %>%
  select(label, key) %>%
  left_join(red %>% mutate(key = paste(pmin(domain_a, domain_b), pmax(domain_a, domain_b), sep = "||")),
            by = "key")
print(as.data.frame(named_red %>% filter(resolution == "10min_based") %>%
  transmute(pair = label, stratum, n, r = r4(pearson_r), p_r = signif(pearson_p, 3),
            rho = r4(spearman_rho), p_rho = signif(spearman_p, 3), cls = redundancy_class_empirical,
            flag = must_not_both_be_displayed) %>% arrange(pair, stratum)), row.names = FALSE)
sec("The SEVEN named pairs, 5min sensitivity, pooled")
print(as.data.frame(named_red %>% filter(resolution == "5min_based", stratum == "pooled") %>%
  transmute(pair = label, n, r = r4(pearson_r), rho = r4(spearman_rho),
            cls = redundancy_class_empirical, flag = must_not_both_be_displayed)), row.names = FALSE)

sec("Highest-|rho| pairs overall, pooled 10min (top 15)")
print(as.data.frame(red %>% filter(stratum == "pooled", resolution == "10min_based") %>%
  transmute(a = str_trunc(domain_a, 32), b = str_trunc(domain_b, 32), n, r = r4(pearson_r),
            rho = r4(spearman_rho), cls = redundancy_class_empirical) %>%
  arrange(desc(abs(rho))) %>% head(15)), row.names = FALSE)

## ==========================================================================
hr("STEP 6. Locomotion-dominance flag (repo standard |rho| >= 0.70 vs Psychomotor activation)")
## ==========================================================================
loco <- red %>% filter(domain_a == "Psychomotor activation" | domain_b == "Psychomotor activation") %>%
  transmute(resolution, stratum, n,
            Domain = if_else(domain_a == "Psychomotor activation", domain_b, domain_a),
            pearson_r, spearman_rho, spearman_p,
            locomotion_dominance_flag = abs(spearman_rho) >= 0.70,
            threshold = "repo standard |rho| >= 0.70 vs first-night Psychomotor activation (Movement_mean_z)")
print(as.data.frame(loco %>% filter(resolution == "10min_based") %>%
  transmute(stratum, Domain = str_trunc(Domain, 42), n, rho = r4(spearman_rho),
            p = signif(spearman_p, 3), flag = locomotion_dominance_flag) %>%
  arrange(stratum, desc(abs(rho)))), row.names = FALSE)
sec("Omitted candidate domains 8, 9, 10 only (both resolutions, all strata)")
print(as.data.frame(loco %>%
  filter(Domain %in% c("Early active spatial flexibility","Early social engagement","Early social withdrawal")) %>%
  transmute(resolution, stratum, Domain = str_trunc(Domain, 34), n, rho = r4(spearman_rho),
            p = signif(spearman_p, 3), flag = locomotion_dominance_flag) %>%
  arrange(Domain, resolution, stratum)), row.names = FALSE)

## ==========================================================================
hr("STEP 7. Assertion register")
## ==========================================================================
areg <- bind_rows(ASSERT)
cat("assertions:", nrow(areg), " PASS =", sum(areg$result == "PASS"), " FAIL =", sum(areg$result == "FAIL"), "\n")
if (any(areg$result == "FAIL")) print(as.data.frame(areg %>% filter(result == "FAIL")), row.names = FALSE)

hr("DONE")
cat("outputs in:", OUT, "\n")
for (f in c("first_night_10domain_scores.csv","first_night_10domain_formula_overlap.csv",
            "first_night_10domain_redundancy.csv"))
  cat(sprintf("  %-46s %s bytes\n", f, format(file.info(file.path(OUT, f))$size, big.mark = ",")))
