## STEP 6: reassess the longitudinal HMM phenotype under the NEW gap-aware fit.
## Uses the shipped gap-aware Viterbi labels and computes temporal metrics WITHIN
## SequenceBlockID (no bridging), then re-runs the corrected repeated-measures model.
## Compares against the pre-gap audit values.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R"); source_mmm_helper("hmm_stage14_helpers.R")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
H <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")
OUT <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")
K <- 4L
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                     Group = col_character(), Sex = col_character()), progress = FALSE), "roster")

# Gap-aware epoch metrics: every temporal quantity computed within blocks only.
epoch_metrics <- function(d, bs) {
  s <- as.integer(d$State); n <- length(s)
  occ <- tabulate(s, nbins = K) / n
  blocks <- split(seq_len(n), d$SequenceBlockID)
  TC <- matrix(0L, K, K); sw <- 0L; nt <- 0L; runs <- vector("list", K)
  for (ix in blocks) {
    sb <- s[ix]; nb <- length(sb)
    if (nb >= 2L) { f <- sb[-nb]; t2 <- sb[-1]
      for (i in seq_along(f)) TC[f[i], t2[i]] <- TC[f[i], t2[i]] + 1L
      sw <- sw + sum(f != t2); nt <- nt + (nb - 1L) }
    r <- rle(sb); for (k in seq_len(K)) runs[[k]] <- c(runs[[k]], r$lengths[r$values == k])
  }
  if (nt == 0L) return(tibble(occupancy_entropy = ent(occ), state_switch_rate = NA_real_,
    self_transition_probability = NA_real_, transition_entropy = NA_real_,
    mean_dwell_minutes = NA_real_, n_transitions = 0L, n_blocks = length(blocks)))
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  dw <- vapply(seq_len(K), function(k) if (!length(runs[[k]])) NA_real_ else mean(runs[[k]]), numeric(1))
  tibble(occupancy_entropy = ent(occ), state_switch_rate = sw / nt,
         self_transition_probability = sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0)),
         transition_entropy = sum(pi_s * rowH),
         mean_dwell_minutes = sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)]) * bs / 60,
         n_transitions = nt, n_blocks = length(blocks))
}

METRICS <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability",
             "transition_entropy", "mean_dwell_minutes")
out <- list()
for (res in c("10min_based", "5min_based")) {
  bs <- if (res == "10min_based") 600 else 300
  a <- read_csv(file.path(H, res, "tables/hmm_state_assignments.csv"),
                col_types = cols(AnimalNum = col_character(), State = col_character(),
                                 SequenceBlockID = col_character(), .default = col_guess()))
  stopifnot("SequenceBlockID" %in% names(a))
  aud <- audit_hmm_identity(a, roster, paste("gap-aware", res)); assert_hmm_identity_audit(aud)
  m <- aud$data %>%
    mutate(PhaseClass = mmm_phase_class(Phase),
           CageChangeIndex = as.integer(str_extract(as.character(CageChange), "[0-9]+"))) %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    arrange(TimeIndex, .by_group = TRUE) %>%
    group_modify(~epoch_metrics(.x, bs)) %>% ungroup()
  cat("\n####", res, ": epochs", nrow(m), " animals", n_distinct(m$AnimalNum),
      " blocks/epoch median", median(m$n_blocks), "\n")
  cat("  mean dwell (min):", round(mean(m$mean_dwell_minutes, na.rm = TRUE), 2),
      " switch rate:", round(mean(m$state_switch_rate, na.rm = TRUE), 4),
      " transition entropy:", round(mean(m$transition_entropy, na.rm = TRUE), 4), "\n")
  sc <- reduce(METRICS, function(x, v) strict_standardize_within_context(x, v), .init = m)
  long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
                        all_of(paste0(METRICS, "_z"))) %>%
    pivot_longer(all_of(paste0(METRICS, "_z")), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(Domain = sub("_z$", "", Domain)) %>% filter(is.finite(DomainScore))
  for (ph in c("Active", "Inactive")) for (v in METRICS)
    out[[length(out) + 1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
      mutate(resolution = res, sequence_contract = "gap_aware")
}
gt <- bind_rows(out)
write_csv(gt, file.path(OUT, "step6_longitudinal_gapaware_contrasts.csv"))

pre <- tribble(
  ~PhaseClass, ~Domain, ~contrast, ~pre_gap_est, ~pre_gap_p,
  "Active","mean_dwell_minutes","SUS-CON", 0.639, 0.0151,
  "Active","mean_dwell_minutes","SUS-RES", 0.391, 0.0689,
  "Active","transition_entropy","SUS-CON",-0.597, 0.0276,
  "Active","transition_entropy","SUS-RES",-0.483, 0.0302,
  "Active","self_transition_probability","SUS-CON", 0.550, 0.0330,
  "Active","self_transition_probability","SUS-RES", 0.422, 0.0465,
  "Active","occupancy_entropy","SUS-CON",-0.138, 0.5620,
  "Inactive","mean_dwell_minutes","RES-CON",-1.092, 5.87e-05,
  "Inactive","mean_dwell_minutes","SUS-CON",-1.133, 4.14e-05,
  "Inactive","transition_entropy","RES-CON", 0.872, 0.00145,
  "Inactive","transition_entropy","SUS-CON", 0.898, 0.00125,
  "Inactive","occupancy_entropy","RES-CON", 0.745, 0.00284,
  "Inactive","self_transition_probability","RES-CON",-0.762, 0.00565
)
cmp <- gt %>% filter(resolution == "10min_based", Sex == "Female") %>%
  transmute(PhaseClass, Domain, contrast, gap_est = mixed_model_estimate,
            gap_p = mixed_model_p, gap_g = animal_level_hedges_g) %>%
  inner_join(pre, by = c("PhaseClass", "Domain", "contrast")) %>%
  mutate(abs_diff = gap_est - pre_gap_est,
         pct = 100 * (gap_est / pre_gap_est - 1),
         direction_kept = sign(gap_est) == sign(pre_gap_est))
cat("\n===== STEP 6: FEMALE 10min, pre-gap vs gap-aware (longitudinal RM model) =====\n")
print(as.data.frame(cmp %>% transmute(PhaseClass, Domain = substr(Domain, 1, 28), contrast,
  pre = round(pre_gap_est, 3), gap = round(gap_est, 3), diff = round(abs_diff, 3),
  pct = round(pct, 1), p_pre = signif(pre_gap_p, 3), p_gap = signif(gap_p, 3),
  dir_kept = direction_kept)), row.names = FALSE)
cat("\n  direction preserved in", sum(cmp$direction_kept), "of", nrow(cmp), "compared cells\n")
cat("  max |pct change|:", round(max(abs(cmp$pct), na.rm = TRUE), 1), "%\n")
cat("\n===== Full gap-aware Female 10min contrasts =====\n")
print(as.data.frame(gt %>% filter(resolution == "10min_based", Sex == "Female") %>%
  transmute(PhaseClass, Domain = substr(Domain, 1, 28), contrast,
            est = round(mixed_model_estimate, 3), SE = round(mixed_model_SE, 3),
            p = signif(mixed_model_p, 3), g = round(animal_level_hedges_g, 3)) %>%
  arrange(PhaseClass, contrast, Domain)), row.names = FALSE)
cat("\nwrote step6_longitudinal_gapaware_contrasts.csv\n")
