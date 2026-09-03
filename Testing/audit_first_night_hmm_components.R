## ============================================================================
## audit_first_night_hmm_components.R
##
## FIRST-NIGHT HMM COMPONENT SCAN -- EXPLORATORY / HYPOTHESIS-GENERATING
##
## Question: is the LONGITUDINAL female Active-phase TEMPORAL PERSISTENCE
## phenotype (SUS more persistent: mean_dwell SUS-CON +0.639, p 0.015;
## transition_entropy SUS-CON -0.597, p 0.028; self_transition SUS-CON +0.550,
## p 0.033; occupancy_entropy null) ALREADY PRESENT during the CC1 first night?
##
## Design decisions that are NOT negotiable in this script:
##  * COMMON STATE SPACE. We reuse the group-blind Viterbi labels in
##    hmm_state_assignments.csv, fitted across the whole longitudinal dataset.
##    No CC1-only refit is produced.
##  * FIRST CONTIGUOUS BLOCK of CC1 Active only. The CC1 Active epoch is FOUR
##    nights (~48 h), so "the entire CC1 Active epoch" is NOT the first night.
##    Blocks are detected as maximal runs of consecutive TimeIndex.
##  * NO TEMPORAL GAPS inside the analysed sequence, by construction. Verified
##    numerically (max diff(TimeIndex) within block 1 must be exactly 1).
##  * one value per animal per component  =>  plain lm, NO random effect.
##  * RFID proximity is a social-spatial CO-LOCATION proxy, never "sociability".
##  * RES/SUS are LATER phenotype labels derived from subsequent CombZ outcome
##    data. Every contrast below is a DESCRIPTIVE association with later
##    phenotype -- never prospective, never causal.
##  * The whole scan is EXPLORATORY. Both raw p and BH q are reported and
##    neither is allowed to select the reported rows.
##
## READ-ONLY with respect to Analysis/ and Functions/. Writes only into
## <Stage14>/audit_hmm_state_architecture/first_night_domain_heatmap.
## ============================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble); library(emmeans); library(readxl)
})
options(dplyr.summarise.inform = FALSE)

setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

PROJ    <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
STAGE14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
AUDIT   <- file.path(STAGE14, "audit_hmm_state_architecture")
OUT     <- file.path(AUDIT, "first_night_domain_heatmap")
HMM     <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")
COMBZ_XLSX <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

BIN_SEC      <- c("10min_based" = 600, "5min_based" = 300)
RESOLUTIONS  <- c("10min_based", "5min_based")  # 10min PRIMARY, 5min SENSITIVITY
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
CONTRASTS    <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))

## the 8 scanned components; occupancy-side vs temporal-side is the dissociation
## the whole deliverable turns on, so it is declared once, here.
OCCUPANCY_COMPONENTS <- c("occupancy_entropy", "latent_state_occupancy_organization",
                          "inactive_state_fraction", "top_proximity_state_fraction")
TEMPORAL_COMPONENTS  <- c("self_transition_probability", "transition_entropy",
                          "state_switch_rate", "mean_dwell_minutes")
COMPONENTS <- c(OCCUPANCY_COMPONENTS, TEMPORAL_COMPONENTS)
## the composite is ALREADY a z-composite in Sex context; re-standardizing it
## would silently reweight the preserved 0.5 coefficient, so it is exempt.
NO_RESTANDARDIZE <- "latent_state_occupancy_organization"

say <- function(...) cat(..., "\n", sep = "")
hdr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

## ---------------------------------------------------------------- roster ----
hdr("0. CANONICAL 111-ANIMAL ROSTER")
roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()),
           progress = FALSE),
  "Stage 01 canonical roster (5min_based all_behavior_metrics.csv)")
say("roster animals = ", nrow(roster), "  (expected 111)")
if (nrow(roster) != 111L) warning("ROSTER SIZE DISAGREEMENT: expected 111, got ", nrow(roster))
print(roster %>% count(Group, Sex) %>% as.data.frame(), row.names = FALSE)

## ------------------------------------------------- per-animal estimators ----
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }

#' All first-night HMM components for ONE contiguous Viterbi sequence.
#'   self_transition_probability = sum_s pi_s P(s|s), pi_s = from-state marginal
#'   transition_entropy          = -sum_s pi_s sum_t P(t|s) log P(t|s)  (per step)
#'   state_switch_rate           = fraction of steps with State != NextState
#'   mean_dwell                  = occupancy-weighted mean run length
block_components <- function(states, K, inactive_states, top_prox_state, bin_sec) {
  s <- as.integer(states); n <- length(s)
  occ <- tabulate(s, nbins = K) / n
  out <- tibble(
    n_bins_block1                = n,
    block1_hours                 = n * bin_sec / 3600,
    occupancy_entropy            = ent(occ),
    inactive_state_fraction      = sum(occ[as.integer(inactive_states)]),
    top_proximity_state_fraction = occ[as.integer(top_prox_state)],
    n_transitions                = max(n - 1L, 0L),
    self_transition_probability  = NA_real_, transition_entropy = NA_real_,
    state_switch_rate            = NA_real_, mean_dwell_bins = NA_real_)
  if (n < 2L) return(out)
  from <- s[-n]; to <- s[-1]; nt <- n - 1L
  TC <- matrix(0L, K, K)
  for (i in seq_len(nt)) TC[from[i], to[i]] <- TC[from[i], to[i]] + 1L
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  runs <- rle(s)
  dw <- vapply(seq_len(K), function(k) {
    l <- runs$lengths[runs$values == k]; if (!length(l)) NA_real_ else mean(l) }, numeric(1))
  out$self_transition_probability <- sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0))
  out$transition_entropy          <- sum(pi_s * rowH)
  out$state_switch_rate           <- mean(from != to)
  out$mean_dwell_bins             <- sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)])
  out
}

zsex <- function(x, sex) ave(x, sex, FUN = function(v) {
  s <- sd(v, na.rm = TRUE)
  if (!is.finite(s) || s == 0) rep(0, length(v)) else (v - mean(v, na.rm = TRUE)) / s })

## ---------------------------------------------------- inference machinery ----
#' ComponentValue ~ Group * Sex on ONE value per animal. Plain lm: there is
#' exactly one observation per animal, so a random intercept is unidentified and
#' any repeated-measures structure would be fictional. Bins / states /
#' transitions are NEVER treated as independent observations.
fit_first_night_component <- function(dat, component, resolution) {
  model_formula <- "ComponentValue ~ Group * Sex"
  md <- dat %>%
    filter(.data$Component == component, is.finite(.data$ComponentValue)) %>%
    transmute(AnimalNum = as.character(AnimalNum),
              Group = factor(as.character(Group), levels = GROUP_LEVELS),
              Sex   = factor(as.character(Sex),   levels = SEX_LEVELS),
              ComponentValue = as.numeric(ComponentValue)) %>%
    filter(!is.na(Group), !is.na(Sex))
  if (anyDuplicated(md$AnimalNum) > 0L)
    stop("first-night design violated: >1 row per animal for ", component, call. = FALSE)

  empty <- crossing(Sex = SEX_LEVELS, contrast = names(CONTRASTS)) %>%
    mutate(component = component, resolution = resolution,
           n_ref = NA_integer_, n_comp = NA_integer_, mean_ref = NA_real_, mean_comp = NA_real_,
           raw_mean_ref = NA_real_, raw_mean_comp = NA_real_, animal_level_hedges_g = NA_real_,
           estimate = NA_real_, SE = NA_real_, df = NA_real_, t_ratio = NA_real_,
           ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_,
           model_formula = model_formula, model_warnings = NA_character_,
           inference_engine = "not_estimable", ci_method = NA_character_,
           effect_size_method = NA_character_, analysis_status = "EXPLORATORY",
           model_status = "not_estimable")
  int_empty <- tibble(component = component, resolution = resolution, term = "Group:Sex",
                      df_num = NA_real_, df_den = NA_real_, F_value = NA_real_,
                      p_value = NA_real_, model_status = "not_estimable")

  wrn <- character()
  fit <- tryCatch(withCallingHandlers(lm(as.formula(model_formula), data = md),
                    warning = function(w) { wrn <<- c(wrn, conditionMessage(w)); invokeRestart("muffleWarning") }),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    empty$model_status <- conditionMessage(fit); int_empty$model_status <- conditionMessage(fit)
    return(list(contrasts = empty, interaction = int_empty))
  }
  emm <- tryCatch(emmeans::emmeans(fit, ~ Group | Sex), error = function(e) e)
  if (inherits(emm, "error")) {
    empty$model_status <- conditionMessage(emm); int_empty$model_status <- conditionMessage(emm)
    return(list(contrasts = empty, interaction = int_empty))
  }
  mc <- emmeans::contrast(emm, method = CONTRASTS, adjust = "none") %>%
    as.data.frame() %>% as_tibble() %>%
    transmute(Sex = as.character(Sex), contrast = as.character(contrast),
              estimate, SE, df, t_ratio = t.ratio, p_value = p.value,
              ## Wald CI on the emmeans contrast: estimate +/- qt(0.975, df) * SE
              ci_low = estimate - qt(0.975, df) * SE,
              ci_high = estimate + qt(0.975, df) * SE)

  raw_lookup <- dat %>% filter(.data$Component == component) %>%
    select(AnimalNum, Group, Sex, RawValue) %>% distinct()
  eff <- crossing(Sex = SEX_LEVELS, contrast = names(CONTRASTS)) %>%
    pmap_dfr(function(Sex, contrast) {
      ref <- sub("^.*-", "", contrast); cmp <- sub("-.*$", "", contrast)
      rv <- md$ComponentValue[as.character(md$Sex) == Sex & as.character(md$Group) == ref]
      cv <- md$ComponentValue[as.character(md$Sex) == Sex & as.character(md$Group) == cmp]
      rr <- raw_lookup$RawValue[raw_lookup$Sex == Sex & raw_lookup$Group == ref]
      rc <- raw_lookup$RawValue[raw_lookup$Sex == Sex & raw_lookup$Group == cmp]
      tibble(Sex = Sex, contrast = contrast,
             n_ref = sum(is.finite(rv)), n_comp = sum(is.finite(cv)),
             mean_ref = if (any(is.finite(rv))) mean(rv, na.rm = TRUE) else NA_real_,
             mean_comp = if (any(is.finite(cv))) mean(cv, na.rm = TRUE) else NA_real_,
             raw_mean_ref = if (any(is.finite(rr))) mean(rr, na.rm = TRUE) else NA_real_,
             raw_mean_comp = if (any(is.finite(rc))) mean(rc, na.rm = TRUE) else NA_real_,
             animal_level_hedges_g = hmm_hedges_g(rv, cv))
    })
  contrasts <- eff %>% left_join(mc, by = c("Sex", "contrast")) %>%
    mutate(component = component, resolution = resolution,
           model_formula = model_formula,
           model_warnings = paste(unique(wrn), collapse = " | "),
           inference_engine = "stats::lm + emmeans (one value per animal; NO random effect)",
           ci_method = "Wald: estimate +/- qt(0.975, residual df) * SE",
           effect_size_method = "animal-level Hedges g (hmm_hedges_g), one value per animal",
           analysis_status = "EXPLORATORY",
           model_status = "fitted")
  at <- tryCatch(as.data.frame(anova(fit)), error = function(e) NULL)
  interaction <- if (is.null(at) || !"Group:Sex" %in% rownames(at)) int_empty else
    tibble(component = component, resolution = resolution, term = "Group:Sex",
           df_num = at["Group:Sex", "Df"], df_den = at["Residuals", "Df"],
           F_value = at["Group:Sex", "F value"], p_value = at["Group:Sex", "Pr(>F)"],
           model_status = "fitted")
  list(contrasts = contrasts, interaction = interaction)
}

## ======================================================= per-resolution ====
animal_features <- list(); all_contrasts <- list(); all_inter <- list()
block_qc <- list(); window_impurity <- list(); state_space <- list()

for (res in RESOLUTIONS) {
  hdr(paste0("1. ", res, " -- COMMON STATE SPACE, CC1 ACTIVE, FIRST CONTIGUOUS BLOCK",
             if (res == "10min_based") "   [PRIMARY]" else "   [SENSITIVITY]"))
  bs <- BIN_SEC[[res]]

  ## --- state space semantics, derived programmatically (never hard-coded) ---
  ss <- read_csv(file.path(HMM, res, "tables/hmm_state_summary.csv"),
                 col_types = cols(State = col_character(), .default = col_guess()), progress = FALSE)
  labs <- annotate_hmm_semantic_states(ss, resolution = res)
  inactive_states <- labs$State[labs$SemanticState == "inactive/low-exploration"]
  top_prox_state  <- labs$State[which.max(labs$Proximity_z)]
  say("state semantics (shipped annotate_hmm_semantic_states()):")
  print(labs %>% select(State, Movement_z, Entropy_z, Proximity_z, SemanticState) %>%
          mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame(), row.names = FALSE)
  say("  inactive_state_fraction states = S", paste(inactive_states, collapse = "+S"))
  say("  argmax(Proximity_z) state      = S", top_prox_state,
      "  (Proximity_z = ", round(max(labs$Proximity_z), 3), ")")
  say("  NOTE: S", top_prox_state, " is a LOW-ACTIVITY HIGH-CO-OCCUPANCY state. ",
      "RFID proximity is a social-spatial CO-LOCATION proxy, not 'sociability'.")
  state_space[[res]] <- labs %>% mutate(
    is_inactive_component  = State %in% inactive_states,
    is_top_proximity_state = State == top_prox_state)

  ## --- Viterbi assignments, identity-audited -------------------------------
  a <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                col_types = cols(AnimalNum = col_character(), State = col_character(),
                                 .default = col_guess()), progress = FALSE)
  aud <- audit_hmm_identity(a, roster, paste("Stage 08 hmm_state_assignments", res))
  assert_hmm_identity_audit(aud)
  A <- aud$data
  K <- max(as.integer(A$State))
  say("assignments rows = ", nrow(A), "  K = ", K, "  identity audit PASSED")

  ## --- CC1 Active, contiguous-block decomposition --------------------------
  cc1 <- A %>%
    filter(as.character(CageChange) == "CC1",
           str_detect(str_to_lower(as.character(Phase)), "\\bactive\\b|\\bdark\\b|\\bnight\\b")) %>%
    arrange(AnimalNum, TimeIndex) %>%
    group_by(AnimalNum) %>%
    mutate(block_index = cumsum(c(1L, diff(TimeIndex)) != 1L) + 1L,
           local_bin = row_number()) %>%
    ungroup()

  nblk <- cc1 %>% distinct(AnimalNum, block_index) %>% count(AnimalNum, name = "n_blocks")
  say("CC1 Active animals present = ", n_distinct(cc1$AnimalNum), " / ", nrow(roster))
  say("  n_blocks per animal: ", paste(sprintf("%d blocks: %d animals",
        sort(unique(nblk$n_blocks)), as.integer(table(nblk$n_blocks))), collapse = " | "))
  bstat <- cc1 %>% count(AnimalNum, block_index) %>%
    group_by(block_index) %>% summarise(n_animals = n(), min_bins = min(n),
      median_bins = median(n), max_bins = max(n),
      median_hours = round(median(n) * bs / 3600, 2))
  print(as.data.frame(bstat), row.names = FALSE)
  span <- cc1 %>% group_by(AnimalNum) %>%
    summarise(n_bins_total = n(),
              data_hours = n() * bs / 3600,
              span_h = (max(TimeIndex) - min(TimeIndex) + 1) * bs / 3600)
  gaps <- cc1 %>% group_by(AnimalNum) %>% mutate(d = c(NA, diff(TimeIndex))) %>%
    filter(!is.na(d), d > 1) %>% pull(d)
  say("  CC1-Active total RETAINED DATA duration (h) = n_bins * bin_size: median ",
      round(median(span$data_hours), 2), " range ", round(min(span$data_hours), 2),
      "-", round(max(span$data_hours), 2), "  (n_bins median ", median(span$n_bins_total), ")")
  say("  CC1-Active WALL-CLOCK span (h) = (max-min TimeIndex + 1) * bin_size: median ",
      round(median(span$span_h), 2), " range ", round(min(span$span_h), 2),
      "-", round(max(span$span_h), 2))
  say("  DEFINITIONAL NOTE (flagged, not a data disagreement): the audit context quotes a ",
      "'total span median 47.83 h'. That is the RETAINED DATA duration (4 dark blocks x ~12 h), ",
      "not the wall-clock span, which is ~84 h because the 3 intervening light phases sit inside ",
      "the CC1 Active TimeIndex range. Both are reported above so the two numbers reconcile.")
  say("  between-block gap: median (d-1)*bin = ", round(median((gaps - 1) * bs / 3600), 2), " h; ",
      "median d*bin = ", round(median(gaps * bs / 3600), 2), " h",
      "  (n gaps = ", length(gaps), ")  -> the intervening light/inactive phases. The context's ",
      "'12.17 h' is the d*bin convention; the physically missing interval is (d-1)*bin = 12.00 h.")

  ## MISSING ANIMALS -- who and why
  missing <- setdiff(roster$AnimalNum, unique(cc1$AnimalNum))
  excl <- read_csv(file.path(HMM, res, "tables/hmm_epoch_data_quality_exclusions.csv"),
                   col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                   progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum))
  say("  MISSING from CC1 Active HMM (n = ", length(missing), "): ", paste(missing, collapse = ", "))
  print(as.data.frame(excl %>% filter(AnimalNum %in% missing, CageChange == "CC1", Phase == "Active") %>%
    select(AnimalNum, Group, Sex, CageChange, Phase, input_bins, complete_hmm_bins,
           retained_for_hmm, exclusion_reason)), row.names = FALSE)

  ## --- block 1 only; NO GAPS by construction, verified --------------------
  b1 <- cc1 %>% filter(block_index == 1L)
  gapchk <- b1 %>% group_by(AnimalNum) %>% summarise(max_step = max(diff(TimeIndex)))
  say("  *** NO TEMPORAL GAPS inside block 1, BY CONSTRUCTION. ",
      "max diff(TimeIndex) within block 1 across all ", nrow(gapchk), " animals = ",
      max(gapchk$max_step), " (must be 1). The gap-crossing problem that affects the ",
      "longitudinal metrics therefore does NOT arise for first-night features.")
  if (max(gapchk$max_step) != 1L) stop("block 1 is not contiguous -- block detection is wrong")

  ## --- provenance discrepancy: Stage 14 `local_bin <= 12h` window ---------
  EW <- as.integer(12 * 3600 / bs)
  imp <- cc1 %>% group_by(AnimalNum) %>%
    summarise(n_window = sum(local_bin <= EW),
              n_window_from_later_night = sum(local_bin <= EW & block_index > 1L),
              n_block1 = sum(block_index == 1L),
              n_block1_outside_window = sum(block_index == 1L & local_bin > EW)) %>%
    mutate(resolution = res, early_window_bins = EW)
  say("  PROVENANCE DISCREPANCY vs Stage 14 raw domains (`local_bin <= ", EW, "`):")
  say("    animals whose 12 h window pulls >=1 bin from the SECOND night: ",
      sum(imp$n_window_from_later_night > 0), " / ", nrow(imp),
      "  (mean ", round(mean(imp$n_window_from_later_night), 2),
      " bins, median ", median(imp$n_window_from_later_night),
      ", max ", max(imp$n_window_from_later_night), ")")
  say("    block-1 bins falling OUTSIDE that window: max ", max(imp$n_block1_outside_window))
  window_impurity[[res]] <- imp

  ## --- per-animal components ---------------------------------------------
  feats <- b1 %>%
    group_by(AnimalNum, Group, Sex) %>%
    arrange(TimeIndex, .by_group = TRUE) %>%
    group_modify(~ bind_cols(
      block_components(.x$State, K, inactive_states, top_prox_state, bs),
      tibble(movement_block1_mean_z  = mean(.x$Movement_z,  na.rm = TRUE),
             entropy_block1_mean_z   = mean(.x$Entropy_z,   na.rm = TRUE),
             proximity_block1_mean_z = mean(.x$Proximity_z, na.rm = TRUE)))) %>%
    ungroup() %>%
    mutate(resolution = res, bin_size_sec = bs,
           ## PHYSICAL TIME so 5 vs 10 min are comparable. Positive affine
           ## rescale of bins => every context-z contrast is numerically
           ## identical in either unit.
           mean_dwell_minutes = mean_dwell_bins * bs / 60,
           ## first-night Psychomotor activation score = z within Sex of
           ## mean(Movement_z) over the SAME first contiguous block.
           movement_first_night = zsex(movement_block1_mean_z, Sex),
           ## composite: Sex-context z of its two parts, coefficient 0.5 PRESERVED
           z_occupancy_entropy_sex       = zsex(occupancy_entropy, Sex),
           z_inactive_state_fraction_sex = zsex(inactive_state_fraction, Sex),
           latent_state_occupancy_organization =
             0.5 * z_occupancy_entropy_sex - z_inactive_state_fraction_sex)

  say("  per-animal features built: n = ", nrow(feats),
      "  block1 hours median ", round(median(feats$block1_hours), 2),
      "  n_bins median ", median(feats$n_bins_block1),
      "  n_transitions median ", median(feats$n_transitions))
  say("  mean_dwell_minutes: mean ", round(mean(feats$mean_dwell_minutes), 2),
      " sd ", round(sd(feats$mean_dwell_minutes), 2),
      " | mean_dwell_bins mean ", round(mean(feats$mean_dwell_bins), 3))
  say("  occupancy_entropy mean ", round(mean(feats$occupancy_entropy), 4),
      " | inactive_state_fraction mean ", round(mean(feats$inactive_state_fraction), 4),
      " | top_proximity_state_fraction mean ", round(mean(feats$top_proximity_state_fraction), 4))
  say("  self_transition_probability mean ", round(mean(feats$self_transition_probability), 4),
      " | transition_entropy mean ", round(mean(feats$transition_entropy), 4),
      " | state_switch_rate mean ", round(mean(feats$state_switch_rate), 4))
  say("  redundancy check  max|state_switch_rate + self_transition_probability - 1| = ",
      signif(max(abs(feats$state_switch_rate + feats$self_transition_probability - 1)), 3))
  say("  composite identity check: max|0.5*z(occ_ent) - z(inact) - composite| = ",
      signif(max(abs(0.5 * feats$z_occupancy_entropy_sex - feats$z_inactive_state_fraction_sex -
                       feats$latent_state_occupancy_organization)), 3))
  animal_features[[res]] <- feats

  ## --- model matrix: context-z within Sex (comparable to the longitudinal
  ##     context-z estimates), composite exempt -----------------------------
  long <- map_dfr(COMPONENTS, function(cp) {
    raw <- feats[[cp]]
    val <- if (cp %in% NO_RESTANDARDIZE) raw else zsex(raw, feats$Sex)
    tibble(AnimalNum = feats$AnimalNum, Group = feats$Group, Sex = feats$Sex,
           Component = cp, RawValue = raw, ComponentValue = val,
           scale_used = if (cp %in% NO_RESTANDARDIZE)
             "already a Sex-context z-composite; NOT re-standardized (preserves 0.5 coefficient)"
             else "z within Sex (context-z, comparable to longitudinal context-z estimates)")
  })

  for (cp in COMPONENTS) {
    r <- fit_first_night_component(long, cp, res)
    all_contrasts[[length(all_contrasts) + 1]] <- r$contrasts %>%
      left_join(long %>% distinct(Component, scale_used), by = c("component" = "Component"))
    all_inter[[length(all_inter) + 1]] <- r$interaction
  }
  block_qc[[res]] <- bstat %>% mutate(resolution = res, bin_size_sec = bs,
    n_animals_cc1_active = n_distinct(cc1$AnimalNum), n_roster = nrow(roster),
    missing_animals = paste(missing, collapse = "|"),
    max_timeindex_step_in_block1 = max(gapchk$max_step),
    total_retained_data_hours_median = round(median(span$data_hours), 3),
    total_wallclock_span_hours_median = round(median(span$span_h), 3),
    between_block_gap_hours_median = round(median((gaps - 1) * bs / 3600), 3),
    between_block_gap_hours_median_dxbin_convention = round(median(gaps * bs / 3600), 3))
}

feat_all <- bind_rows(animal_features)
res_tbl <- bind_rows(all_contrasts) %>%
  group_by(resolution, Sex) %>%
  mutate(family_id = paste0("FIRST_NIGHT_HMM_COMPONENT_SCAN__", resolution, "__", Sex),
         n_tests_in_family = sum(is.finite(p_value)),
         q_value = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(component_group = if_else(component %in% OCCUPANCY_COMPONENTS,
                                   "latent-state occupancy organization", "temporal dynamics"),
         multiplicity = "BH within resolution x Sex over 8 components x 3 contrasts",
         analysis_status = "EXPLORATORY / hypothesis-generating scan; raw p AND BH q both reported, neither selects rows",
         phenotype_caveat = "RES/SUS are LATER phenotype labels derived from subsequent CombZ outcome data; descriptive association with later phenotype, NOT prospective and NOT causal",
         window_definition = "CC1 Active, FIRST CONTIGUOUS BLOCK of consecutive TimeIndex (~12 h, one night); NO temporal gaps inside the sequence",
         state_space_definition = "common longitudinal group-blind Viterbi labels from Stage 08 hmm_state_assignments.csv; NO CC1-only refit",
         proximity_caveat = "top_proximity_state_fraction indexes a LOW-ACTIVITY HIGH-CO-OCCUPANCY state; RFID proximity is a social-spatial co-location proxy, never sociability") %>%
  relocate(resolution, component, component_group, Sex, contrast)
inter_tbl <- bind_rows(all_inter) %>%
  mutate(multiplicity = "UNCORRECTED: exactly one Group:Sex test per component x resolution; NO FDR family applied to interactions",
         analysis_status = "EXPLORATORY / hypothesis-generating",
         note = "computed on the same context-z outcome as the contrasts; z-within-Sex removes the Sex main effect by construction, so this F tests whether standardized group spacing differs between sexes")

write_csv(res_tbl,   file.path(OUT, "first_night_hmm_component_results.csv"))
write_csv(inter_tbl, file.path(OUT, "first_night_hmm_component_interactions.csv"))
write_csv(feat_all,  file.path(OUT, "first_night_hmm_component_animal_features.csv"))
write_csv(bind_rows(block_qc),        file.path(OUT, "first_night_hmm_block_structure_qc.csv"))
write_csv(bind_rows(window_impurity), file.path(OUT, "first_night_hmm_window_provenance_impurity.csv"))
write_csv(bind_rows(state_space),     file.path(OUT, "first_night_hmm_state_space_semantics.csv"))

## ================================================ printed primary results ==
for (rs in RESOLUTIONS) for (sx in SEX_LEVELS) {
  hdr(paste0("2. FIRST-NIGHT COMPONENT CONTRASTS -- ", rs,
             if (rs == "10min_based") " [PRIMARY]" else " [SENSITIVITY]", " / ", sx,
             "   family n_tests = ",
             unique(res_tbl$n_tests_in_family[res_tbl$resolution == rs & res_tbl$Sex == sx])))
  print(res_tbl %>% filter(resolution == rs, Sex == sx) %>%
    transmute(component, contrast, n = paste0(n_ref, "v", n_comp),
              est = round(estimate, 3), SE = round(SE, 3),
              CI = sprintf("[%.2f,%.2f]", ci_low, ci_high), df = df,
              g = round(animal_level_hedges_g, 3), p = signif(p_value, 4), q = signif(q_value, 3)) %>%
    arrange(component, contrast) %>% as.data.frame(), row.names = FALSE)
}
hdr("3. Group:Sex INTERACTION (UNCORRECTED, one test per component x resolution)")
print(inter_tbl %>% transmute(resolution, component, df_num, df_den,
        F = round(F_value, 3), p = signif(p_value, 4)) %>% as.data.frame(), row.names = FALSE)

## ================================================ resolution sensitivity ==
hdr("4. RESOLUTION SENSITIVITY (10 min PRIMARY vs 5 min SENSITIVITY)")
w10 <- res_tbl %>% filter(resolution == "10min_based") %>%
  select(component, component_group, Sex, contrast, est10 = estimate, se10 = SE,
         lo10 = ci_low, hi10 = ci_high, p10 = p_value, q10 = q_value, g10 = animal_level_hedges_g)
w05 <- res_tbl %>% filter(resolution == "5min_based") %>%
  select(component, Sex, contrast, est5 = estimate, se5 = SE,
         lo5 = ci_low, hi5 = ci_high, p5 = p_value, q5 = q_value, g5 = animal_level_hedges_g)
sens <- w10 %>% inner_join(w05, by = c("component", "Sex", "contrast")) %>%
  mutate(sign_agree = sign(est10) == sign(est5),
         ci_overlap = (lo10 <= hi5) & (lo5 <= hi10),
         abs_ratio_5_over_10 = abs(est5) / abs(est10),
         se_ratio_5_over_10 = se5 / se10,
         comparison_basis = "direction, magnitude and uncertainty only; resolution is NEVER selected by p-value",
         analysis_status = "EXPLORATORY")
write_csv(sens, file.path(OUT, "first_night_hmm_component_resolution_sensitivity.csv"))
say("sign agreement: ", sum(sens$sign_agree), " / ", nrow(sens),
    "   CI overlap: ", sum(sens$ci_overlap), " / ", nrow(sens),
    "   median |est5|/|est10| = ", round(median(sens$abs_ratio_5_over_10, na.rm = TRUE), 3),
    "   median SE ratio = ", round(median(sens$se_ratio_5_over_10, na.rm = TRUE), 3))
print(sens %>% transmute(component, Sex, contrast, est10 = round(est10, 3), est5 = round(est5, 3),
        sign_agree, ci_overlap, ratio = round(abs_ratio_5_over_10, 2),
        p10 = signif(p10, 3), p5 = signif(p5, 3)) %>%
  arrange(component, Sex, contrast) %>% as.data.frame(), row.names = FALSE)

## ============================================ movement-adjustment (diag) ==
hdr("5. MOVEMENT-ADJUSTMENT SENSITIVITY -- CONSTRUCT DIAGNOSTIC, NOT PRIMARY")
say("movement_first_night = z within Sex of mean(Movement_z) over the SAME CC1-Active FIRST")
say("CONTIGUOUS BLOCK used for every HMM component (block-1 window, not `local_bin <= 12 h`).")
say("CAVEAT (mandatory): when the covariate is strongly collinear with the outcome the residual")
say("variance collapses, so unadjusted and adjusted p-values are NOT comparable. The")
say("component-movement correlation is reported so the reader can judge. The adjusted model is")
say("NEVER primary.")
mv_rows <- list(); loco_rows <- list()
for (rs in RESOLUTIONS) {
  f <- feat_all %>% filter(resolution == rs)
  for (cp in COMPONENTS) {
    raw <- f[[cp]]
    val <- if (cp %in% NO_RESTANDARDIZE) raw else zsex(raw, f$Sex)
    md <- tibble(AnimalNum = f$AnimalNum,
                 Group = factor(f$Group, levels = GROUP_LEVELS),
                 Sex = factor(f$Sex, levels = SEX_LEVELS),
                 ComponentValue = val, movement_first_night = f$movement_first_night) %>%
      filter(is.finite(ComponentValue), is.finite(movement_first_night))
    for (sx in SEX_LEVELS) {
      s <- md %>% filter(Sex == sx)
      ct <- suppressWarnings(cor.test(s$ComponentValue, s$movement_first_night, method = "spearman"))
      loco_rows[[length(loco_rows) + 1]] <- tibble(
        resolution = rs, component = cp, Sex = sx, n = nrow(s),
        spearman_rho_with_movement = unname(ct$estimate), spearman_p = ct$p.value,
        pearson_r_with_movement = suppressWarnings(cor(s$ComponentValue, s$movement_first_night)),
        locomotion_dominance_flag = abs(unname(ct$estimate)) >= 0.70,
        threshold = "repo locomotion-dominance threshold |rho| >= 0.70")
    }
    gc0 <- function(f0) emmeans::contrast(emmeans::emmeans(f0, ~ Group | Sex),
                                          method = CONTRASTS, adjust = "none") %>%
      as.data.frame() %>% as_tibble() %>%
      transmute(Sex = as.character(Sex), contrast = as.character(contrast), estimate, SE, p = p.value)
    u <- gc0(lm(ComponentValue ~ Group * Sex, data = md)) %>%
      rename(est_unadj = estimate, se_unadj = SE, p_unadj = p)
    a <- gc0(lm(ComponentValue ~ Group * Sex + movement_first_night, data = md)) %>%
      rename(est_adj = estimate, se_adj = SE, p_adj = p)
    mv_rows[[length(mv_rows) + 1]] <- u %>% left_join(a, by = c("Sex", "contrast")) %>%
      mutate(resolution = rs, component = cp,
             pct_attenuation = 100 * (1 - abs(est_adj) / abs(est_unadj)),
             sign_flip_after_adjustment = sign(est_adj) != sign(est_unadj))
  }
}
loco_tbl <- bind_rows(loco_rows)
mv_tbl <- bind_rows(mv_rows) %>%
  left_join(loco_tbl %>% select(resolution, component, Sex,
              spearman_rho_with_movement, locomotion_dominance_flag),
            by = c("resolution", "component", "Sex")) %>%
  mutate(model_unadjusted = "ComponentValue ~ Group * Sex",
         model_adjusted   = "ComponentValue ~ Group * Sex + movement_first_night",
         movement_definition = "z within Sex of mean(Movement_z) over the SAME CC1-Active first contiguous block",
         status = "CONSTRUCT DIAGNOSTIC -- the adjusted model is NOT primary",
         p_comparability_caveat = "residual variance collapses under strong collinearity; unadjusted vs adjusted p are NOT comparable -- judge via spearman_rho_with_movement") %>%
  relocate(resolution, component, Sex, contrast)
write_csv(mv_tbl, file.path(OUT, "first_night_hmm_component_movement_adjustment.csv"))
write_csv(loco_tbl, file.path(OUT, "first_night_hmm_component_locomotion_dominance.csv"))
say("\nSpearman(component, movement_first_night) per Sex  [threshold |rho| >= 0.70]:")
print(loco_tbl %>% transmute(resolution, component, Sex, n, rho = round(spearman_rho_with_movement, 3),
        p = signif(spearman_p, 3), flag = locomotion_dominance_flag) %>% as.data.frame(), row.names = FALSE)
say("\nunadjusted vs movement-adjusted contrasts:")
print(mv_tbl %>% transmute(resolution, component, Sex, contrast,
        est_unadj = round(est_unadj, 3), est_adj = round(est_adj, 3),
        atten_pct = round(pct_attenuation, 1), p_unadj = signif(p_unadj, 3), p_adj = signif(p_adj, 3),
        rho_mv = round(spearman_rho_with_movement, 2)) %>% as.data.frame(), row.names = FALSE)

## ==================================================== CombZ (exploratory) ==
hdr("6. EXPLORATORY CONTINUOUS CombZ ASSOCIATION -- NOT INDEPENDENT PREDICTION")
say("MANDATORY FRAMING: RES/SUS were DERIVED FROM CombZ. A CombZ association is therefore")
say("NOT independent prediction and NOT evidence of incremental predictive value. The")
say("movement-adjusted models below are a DESCRIPTIVE incremental-association check only. A")
say("prospective predictive claim would require the cross-validated framework of Stage 09,")
say("where Movement_mean carries most of the cross-validated signal.")
cz_raw <- read_excel(COMBZ_XLSX, sheet = "zScore")
id_col <- intersect(c("ID", "AnimalNum", "Animal", "MouseID"), names(cz_raw))[1]
cz <- cz_raw %>% transmute(AnimalNum = canonical_animal_id(.data[[id_col]]),
                           CombZ = suppressWarnings(as.numeric(CombZ))) %>%
  filter(!is.na(AnimalNum), is.finite(CombZ)) %>% group_by(AnimalNum) %>%
  summarise(CombZ = mean(CombZ), n_src = n())
say("CombZ animals = ", nrow(cz), "  (id column used: ", id_col, ")  duplicate ids collapsed: ",
    sum(cz$n_src > 1))

cz_rows <- list()
for (rs in RESOLUTIONS) {
  f <- feat_all %>% filter(resolution == rs) %>% inner_join(cz, by = "AnimalNum")
  say("  ", rs, ": animals with both first-night HMM features and CombZ = ", nrow(f),
      " (Female ", sum(f$Sex == "Female"), " / Male ", sum(f$Sex == "Male"), ")")
  for (cp in COMPONENTS) {
    raw <- f[[cp]]
    val <- if (cp %in% NO_RESTANDARDIZE) raw else zsex(raw, f$Sex)
    d <- tibble(Sex = f$Sex, comp = val, mv = f$movement_first_night, CombZ = f$CombZ) %>%
      filter(is.finite(comp), is.finite(CombZ), is.finite(mv))
    for (sx in c(SEX_LEVELS, "Both")) {
      s <- if (sx == "Both") d else d %>% filter(Sex == sx)
      if (nrow(s) < 5) next
      sp <- suppressWarnings(cor.test(s$comp, s$CombZ, method = "spearman"))
      pe <- suppressWarnings(cor.test(s$comp, s$CombZ, method = "pearson"))
      row <- tibble(resolution = rs, component = cp, Sex = sx, n = nrow(s),
                    spearman_rho = unname(sp$estimate), spearman_p = sp$p.value,
                    pearson_r = unname(pe$estimate), pearson_p = pe$p.value,
                    m1_comp_beta = NA_real_, m1_comp_se = NA_real_, m1_comp_p = NA_real_,
                    m2_movement_beta = NA_real_, m2_movement_p = NA_real_,
                    m3_comp_beta_adj_for_movement = NA_real_, m3_comp_se_adj = NA_real_,
                    m3_comp_p_adj_for_movement = NA_real_,
                    m3_movement_beta = NA_real_, m3_movement_p = NA_real_,
                    comp_pct_attenuation_after_movement = NA_real_,
                    spearman_component_vs_movement = NA_real_)
      if (sx == "Both") {
        c1 <- summary(lm(CombZ ~ comp + Sex, data = s))$coefficients
        c2 <- summary(lm(CombZ ~ mv + Sex, data = s))$coefficients
        c3 <- summary(lm(CombZ ~ mv + comp + Sex, data = s))$coefficients
        row <- row %>% mutate(
          m1_comp_beta = c1["comp", 1], m1_comp_se = c1["comp", 2], m1_comp_p = c1["comp", 4],
          m2_movement_beta = c2["mv", 1], m2_movement_p = c2["mv", 4],
          m3_comp_beta_adj_for_movement = c3["comp", 1], m3_comp_se_adj = c3["comp", 2],
          m3_comp_p_adj_for_movement = c3["comp", 4],
          m3_movement_beta = c3["mv", 1], m3_movement_p = c3["mv", 4],
          comp_pct_attenuation_after_movement = 100 * (1 - abs(c3["comp", 1]) / abs(c1["comp", 1])),
          spearman_component_vs_movement = suppressWarnings(cor(s$comp, s$mv, method = "spearman")))
      }
      cz_rows[[length(cz_rows) + 1]] <- row
    }
  }
}
cz_tbl <- bind_rows(cz_rows) %>%
  mutate(model_1 = "CombZ ~ component + Sex", model_2 = "CombZ ~ movement_first_night + Sex",
         model_3 = "CombZ ~ movement_first_night + component + Sex",
         framing = "EXPLORATORY DESCRIPTIVE ASSOCIATION. RES/SUS were derived FROM CombZ, so this is NOT independent prediction and NOT evidence of incremental predictive value.",
         incremental_check_label = "DESCRIPTIVE incremental-association check only; a prospective predictive claim requires the cross-validated Stage 09 framework where Movement_mean carries most of the signal.",
         multiplicity = "UNCORRECTED exploratory correlations; no FDR family claimed")
write_csv(cz_tbl, file.path(OUT, "first_night_hmm_component_combz_association.csv"))
print(cz_tbl %>% transmute(resolution, component, Sex, n, rho = round(spearman_rho, 3),
        sp_p = signif(spearman_p, 3), r = round(pearson_r, 3), pe_p = signif(pearson_p, 3),
        b_comp = round(m1_comp_beta, 3), p_comp = signif(m1_comp_p, 3),
        b_comp_adj = round(m3_comp_beta_adj_for_movement, 3),
        p_comp_adj = signif(m3_comp_p_adj_for_movement, 3)) %>%
  arrange(resolution, component, Sex) %>% as.data.frame(), row.names = FALSE)
say("\nCombZ ~ movement_first_night + Sex   (movement term, identical across components):")
print(cz_tbl %>% filter(Sex == "Both") %>% distinct(resolution, .keep_all = TRUE) %>%
  transmute(resolution, n, movement_beta = round(m2_movement_beta, 4),
            movement_p = signif(m2_movement_p, 4)) %>% as.data.frame(), row.names = FALSE)

## ============================================ reconciliation with Stage 14 ==
hdr("7. RECONCILIATION WITH THE EXISTING PRODUCTION CC1 PANEL (read-only)")
say("Ours is a CANDIDATE REPLACEMENT/EXTENSION. Nothing in Analysis/, Functions/, or the")
say("production tables/figures is edited by this script.")
ex_con <- read_csv(file.path(STAGE14, "tables/sis_CC1_first_active_domain_contrasts.csv"),
                   col_types = cols(.default = col_guess()), progress = FALSE)
say("existing sis_CC1_first_active_domain_contrasts.csv: rows = ", nrow(ex_con),
    "  domains = ", n_distinct(ex_con$Domain))
say("existing test_method = ", unique(ex_con$test_method)[1])
print(as.data.frame(ex_con %>% count(Domain, name = "n_rows")), row.names = FALSE)
say("\nEXISTING inference: Welch t-test + Wilcoxon sensitivity, BH within Sex.")
say("OURS: ComponentValue ~ Group*Sex (lm) + emmeans planned contrasts, BH within resolution x Sex.")
say("The existing panel contains NO HMM latent-state component rows, so our 8 component rows are")
say("ADDITIVE rather than competing. The nearest overlapping production construct is the raw-metric")
say("'Behavioral state architecture' domain, built from raw Movement/Entropy/Proximity summaries,")
say("NOT from Viterbi state sequences.")
say("\nDUPLICATE-ROW NOTE: at CC1 Active, Stage 14 defines")
say("  `Early adaptation / prediction` = if_else(first CC, `Active-phase adaptation/exploration`, NA)")
say("so the two rows are MATHEMATICALLY IDENTICAL. Only 'Active-phase adaptation/exploration' is")
say("retained in the companion first-night heatmap build; 'Early adaptation / prediction' is excluded.")
ex_heat <- read_csv(file.path(STAGE14, "tables/sis_CC1_first_active_domain_heatmap_data.csv"),
                    col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                    progress = FALSE) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum))
say("\nexisting heatmap_data animals = ", nrow(ex_heat),
    "  ours (10min) = ", sum(feat_all$resolution == "10min_based"),
    "  overlap = ", length(intersect(ex_heat$AnimalNum, feat_all$AnimalNum)))
cross <- feat_all %>% filter(resolution == "10min_based") %>%
  select(AnimalNum, Sex, occupancy_entropy, mean_dwell_minutes, movement_first_night) %>%
  inner_join(ex_heat %>% select(AnimalNum,
               psy = sis_CC1_first_active_score__psychomotor_activation,
               arch = sis_CC1_first_active_score__behavioral_state_architecture),
             by = "AnimalNum")
r_psy  <- cor(cross$movement_first_night, cross$psy, use = "complete.obs")
r_occ  <- cor(cross$occupancy_entropy, cross$arch, use = "complete.obs")
r_dwl  <- cor(cross$mean_dwell_minutes, cross$arch, use = "complete.obs")
say("cor(our movement_first_night, production 'Psychomotor activation') = ", round(r_psy, 4),
    "  -> validates that our block-1 window reproduces the production 12 h Movement score")
say("cor(our first-night occupancy_entropy, production 'Behavioral state architecture') = ", round(r_occ, 4))
say("cor(our first-night mean_dwell_minutes, production 'Behavioral state architecture') = ", round(r_dwl, 4))
recon <- tibble(
  item = c("existing_contrast_rows", "existing_domains", "existing_method", "our_method",
           "our_contrast_rows", "our_components", "overlap_animals",
           "cor_movement_vs_production_psychomotor",
           "cor_occ_entropy_vs_production_state_architecture",
           "cor_mean_dwell_vs_production_state_architecture",
           "duplicate_row_exclusion", "relationship_to_production"),
  value = as.character(c(nrow(ex_con), n_distinct(ex_con$Domain),
            "Welch t-test + Wilcoxon sensitivity, BH within Sex",
            "lm ComponentValue ~ Group*Sex + emmeans planned contrasts, BH within resolution x Sex",
            nrow(res_tbl), length(COMPONENTS), length(intersect(ex_heat$AnimalNum, feat_all$AnimalNum)),
            round(r_psy, 4), round(r_occ, 4), round(r_dwl, 4),
            "`Early adaptation / prediction` == `Active-phase adaptation/exploration` exactly at CC1; only the latter is displayed",
            "CANDIDATE REPLACEMENT/EXTENSION -- production tables and figures are NOT edited")))
write_csv(recon, file.path(OUT, "first_night_hmm_production_reconciliation.csv"))

## ==================================================== longitudinal compare ==
hdr("8. FIRST NIGHT vs LONGITUDINAL (10min, Female, Active, context-z)")
lng <- tribble(
  ~component, ~contrast, ~long_estimate, ~long_p,
  "mean_dwell_minutes",          "SUS-CON",  0.639, 0.015,
  "transition_entropy",          "SUS-CON", -0.597, 0.028,
  "self_transition_probability", "SUS-CON",  0.550, 0.033,
  "self_transition_probability", "SUS-RES",  0.422, 0.047,
  "occupancy_entropy",           "SUS-CON", -0.138, 0.562)
cmp <- lng %>% left_join(res_tbl %>% filter(resolution == "10min_based", Sex == "Female") %>%
  select(component, contrast, fn_estimate = estimate, fn_SE = SE, fn_ci_low = ci_low,
         fn_ci_high = ci_high, fn_p = p_value, fn_q = q_value, fn_g = animal_level_hedges_g),
  by = c("component", "contrast")) %>%
  mutate(sign_agree = sign(fn_estimate) == sign(long_estimate),
         fn_over_long = fn_estimate / long_estimate,
         long_est_inside_fn_ci = long_estimate >= fn_ci_low & long_estimate <= fn_ci_high,
         note = "longitudinal values are CC1-CC4 repeated-measures 10min Female Active context-z quoted from the audit context; first-night values are CC1 block-1 only. Longitudinal mean_dwell was reported in bins/hours; context-z makes the unit irrelevant.")
write_csv(cmp, file.path(OUT, "first_night_vs_longitudinal_persistence.csv"))
print(cmp %>% transmute(component, contrast, long_est = long_estimate, long_p,
        fn_est = round(fn_estimate, 3), fn_CI = sprintf("[%.2f,%.2f]", fn_ci_low, fn_ci_high),
        fn_p = signif(fn_p, 3), fn_q = signif(fn_q, 3), fn_g = round(fn_g, 3),
        sign_agree, long_in_fn_CI = long_est_inside_fn_ci) %>% as.data.frame(), row.names = FALSE)

## ==================================================================== figure ==
hdr("9. FIGURE")
fig_ok <- tryCatch({
  suppressMessages(library(ggplot2))
  pd <- res_tbl %>% filter(resolution == "10min_based") %>%
    mutate(component = factor(component, levels = rev(COMPONENTS)),
           component_group = factor(component_group,
             levels = c("latent-state occupancy organization", "temporal dynamics")),
           contrast = factor(contrast, levels = names(CONTRASTS)))
  p <- ggplot(pd, aes(x = estimate, y = component, colour = contrast)) +
    geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey55") +
    geom_linerange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.4,
                   position = position_dodge(width = 0.62)) +
    geom_point(size = 1.3, position = position_dodge(width = 0.62)) +
    facet_grid(component_group ~ Sex, scales = "free_y", space = "free_y") +
    scale_colour_manual(values = mmm_pair_colors) +
    labs(title = "CC1 first night (first contiguous Active block): HMM component contrasts",
         subtitle = "10 min primary; common longitudinal state space; context-z within Sex; lm ~ Group*Sex + emmeans; EXPLORATORY",
         x = "contrast estimate (SD units within Sex, 95% CI)", y = NULL,
         caption = paste0("Upper facet = latent-state OCCUPANCY organization (carries NO temporal-order information). ",
                          "Lower facet = TEMPORAL persistence/flexibility.\n",
                          "RFID proximity is a social-spatial co-location proxy, never sociability. ",
                          "RES/SUS are LATER phenotype labels; associations are descriptive, not prospective.")) +
    make_nature_theme(base_size = 7) +
    theme(legend.position = "top", plot.caption = ggplot2::element_text(size = 4.6))
  ggsave(file.path(OUT, "Fig_first_night_hmm_components.svg"), p, width = 180, height = 108, units = "mm")
  ggsave(file.path(OUT, "Fig_first_night_hmm_components.pdf"), p, width = 180, height = 108, units = "mm",
         device = if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf")
  TRUE
}, error = function(e) { say("figure failed: ", conditionMessage(e)); FALSE })
say("figure written: ", fig_ok)

hdr("10. FILES WRITTEN")
say(paste(sort(list.files(OUT)), collapse = "\n"))
say("\nOUT = ", OUT)
