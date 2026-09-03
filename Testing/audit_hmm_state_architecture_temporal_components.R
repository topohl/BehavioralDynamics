## Point (6): does the female RES-SUS pattern live in OCCUPANCY COMPOSITION only, or also in
## genuine TEMPORAL HMM quantities (switch rate, transition entropy, self-transition, dwell)?
## Built directly from the raw Viterbi sequences so temporal metrics are unambiguous.
## Same corrected estimator, same canonical roster, same repeated-measures structure.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

OUT <- getOption("mmm.audit_out", "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
HMM <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")
PH_INACT <- "\\binactive\\b|\\blight\\b|\\bday\\b"; PH_ACT <- "\\bactive\\b|\\bdark\\b|\\bnight\\b"

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE), "roster")

ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }

epoch_metrics <- function(d, K) {
  s <- as.integer(d$State); n <- length(s)
  occ <- tabulate(s, nbins = K) / n
  H <- ent(occ)
  if (n < 2) return(tibble(occupancy_entropy = H, state_switch_rate = NA_real_,
                           self_transition_probability = NA_real_, transition_entropy = NA_real_,
                           transition_entropy_mm = NA_real_, mean_dwell_bins = NA_real_,
                           n_transitions = 0L, n_bins = n))
  from <- s[-n]; to <- s[-1]; nt <- n - 1L
  TC <- matrix(0L, K, K); for (i in seq_len(nt)) TC[from[i], to[i]] <- TC[from[i], to[i]] + 1L
  rs <- rowSums(TC); pi_s <- rs / nt
  # entropy rate: occupancy-weighted (by from-state marginal) mean row entropy
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  Hrate <- sum(pi_s * rowH)
  # Miller-Madow: add (m_k - 1)/(2 * n_k) per conditioning state, weighted the same way
  mk <- vapply(seq_len(K), function(k) sum(TC[k, ] > 0), integer(1))
  mm <- sum(pi_s * ifelse(rs > 0, (mk - 1) / (2 * rs), 0))
  self_p <- sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0))
  runs <- rle(s)
  dw <- vapply(seq_len(K), function(k) { l <- runs$lengths[runs$values == k]
    if (!length(l)) NA_real_ else mean(l) }, numeric(1))
  mean_dwell <- sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)])
  tibble(occupancy_entropy = H, state_switch_rate = mean(from != to),
         self_transition_probability = self_p, transition_entropy = Hrate,
         transition_entropy_mm = Hrate + mm, mean_dwell_bins = mean_dwell,
         n_transitions = nt, n_bins = n)
}

METRICS <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability",
             "transition_entropy", "transition_entropy_mm", "mean_dwell_bins")
allres <- list(); allmet <- list()
for (res in c("10min_based", "5min_based")) {
  a <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                col_types = cols(AnimalNum = col_character(), State = col_character(), .default = col_guess()))
  aud <- audit_hmm_identity(a, roster, paste("assignments", res)); assert_hmm_identity_audit(aud)
  K <- n_distinct(as.integer(aud$data$State))
  m <- aud$data %>%
    mutate(PhaseClass = case_when(str_detect(str_to_lower(Phase), PH_INACT) ~ "Inactive",
                                  str_detect(str_to_lower(Phase), PH_ACT) ~ "Active", TRUE ~ Phase),
           CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+"))) %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    arrange(TimeIndex, .by_group = TRUE) %>%
    group_modify(~ epoch_metrics(.x, K)) %>% ungroup() %>% mutate(resolution = res)
  cat("\n####", res, ": epochs =", nrow(m), " animals =", n_distinct(m$AnimalNum), " K =", K, "\n")
  cat("  max|switch_rate + self_transition_probability - 1| =",
      max(abs(m$state_switch_rate + m$self_transition_probability - 1), na.rm = TRUE), "\n")
  ok <- is.finite(m$mean_dwell_bins) & is.finite(m$self_transition_probability) & m$self_transition_probability < 1
  cat("  cor(mean_dwell_bins, 1/(1-P_self)) =",
      round(cor(m$mean_dwell_bins[ok], 1 / (1 - m$self_transition_probability[ok])), 4),
      " Spearman =", round(cor(m$mean_dwell_bins[ok], 1 / (1 - m$self_transition_probability[ok]), method = "spearman"), 4), "\n")
  cat("  Spearman(transition_entropy, n_transitions) =",
      round(cor(m$transition_entropy, m$n_transitions, method = "spearman", use = "complete.obs"), 4),
      " | mm-corrected =", round(cor(m$transition_entropy_mm, m$n_transitions, method = "spearman", use = "complete.obs"), 4), "\n")
  cat("  cor(occupancy_entropy, transition_entropy) =",
      round(cor(m$occupancy_entropy, m$transition_entropy, use = "complete.obs"), 4),
      " | cor(occ_ent, switch_rate) =", round(cor(m$occupancy_entropy, m$state_switch_rate, use = "complete.obs"), 4), "\n")
  cat("  mean_dwell_bins mean =", round(mean(m$mean_dwell_bins, na.rm = TRUE), 3),
      " -> hours =", round(mean(m$mean_dwell_bins, na.rm = TRUE) * ifelse(res == "5min_based", 300, 600) / 3600, 3), "\n")
  allmet[[res]] <- m

  scored <- reduce(METRICS, function(acc, v) strict_standardize_within_context(acc, v), .init = m)
  long <- scored %>%
    select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, all_of(paste0(METRICS, "_z"))) %>%
    pivot_longer(all_of(paste0(METRICS, "_z")), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(Domain = sub("_z$", "", Domain)) %>% filter(is.finite(DomainScore))
  for (ph in c("Active", "Inactive")) for (v in METRICS) {
    allres[[length(allres) + 1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
      mutate(resolution = res)
  }
}
rt <- bind_rows(allres)
write_csv(rt, file.path(OUT, "hmm_architecture_temporal_component_contrasts.csv"))
write_csv(bind_rows(allmet), file.path(OUT, "hmm_architecture_temporal_epoch_metrics.csv"))

cat("\n=============== FEMALE: occupancy vs TEMPORAL components (context-z) ===============\n")
for (rs in c("10min_based", "5min_based")) for (ph in c("Active", "Inactive")) {
  cat("\n#### ", rs, " / ", ph, " / FEMALE\n", sep = "")
  print(as.data.frame(rt %>% filter(resolution == rs, PhaseClass == ph, Sex == "Female") %>%
    transmute(component = Domain, contrast, est = round(mixed_model_estimate, 3),
              SE = round(mixed_model_SE, 3), p = signif(mixed_model_p, 3),
              g = round(animal_level_hedges_g, 3), status = model_status) %>%
    arrange(component, contrast)), row.names = FALSE)
}
cat("\nwrote temporal_component_contrasts.csv rows =", nrow(rt), "\n")
