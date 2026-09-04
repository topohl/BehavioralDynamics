## Blocking-finding verification: epochs are concatenations of 1-4 same-phase blocks separated by
## the opposite (unobserved) phase. Naive per-epoch metrics therefore create spurious transitions
## and merge dwell bouts across ~12 h gaps. Recompute GAP-AWARE (metrics within contiguous blocks,
## then occupancy-weighted aggregation) and compare the Female contrasts.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
A <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
HMM <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/06_behavioral_dynamics/hmm_states"
PH_I <- "\\binactive\\b|\\blight\\b|\\bday\\b"; PH_A <- "\\bactive\\b|\\bdark\\b|\\bnight\\b"
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }
METRICS <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability", "transition_entropy", "mean_dwell_bins")

gap_metrics <- function(d, K) {
  s <- as.integer(d$State); ti <- as.numeric(d$TimeIndex); n <- length(s)
  occ <- tabulate(s, nbins = K) / n                       # occupancy is gap-insensitive
  step <- stats::median(diff(sort(unique(ti))), na.rm = TRUE); if (!is.finite(step) || step <= 0) step <- 1
  blk <- cumsum(c(1, diff(ti) > 1.5 * step))              # contiguous-block id
  TC <- matrix(0L, K, K); sw <- 0L; nt <- 0L; runlens <- vector("list", K)
  for (b in unique(blk)) {
    sb <- s[blk == b]; nb <- length(sb)
    if (nb >= 2) { f <- sb[-nb]; t2 <- sb[-1]
      for (i in seq_along(f)) TC[f[i], t2[i]] <- TC[f[i], t2[i]] + 1L
      sw <- sw + sum(f != t2); nt <- nt + (nb - 1L) }
    r <- rle(sb); for (k in seq_len(K)) runlens[[k]] <- c(runlens[[k]], r$lengths[r$values == k])
  }
  if (nt == 0L) return(tibble(occupancy_entropy = ent(occ), state_switch_rate = NA_real_,
    self_transition_probability = NA_real_, transition_entropy = NA_real_, mean_dwell_bins = NA_real_,
    n_blocks = length(unique(blk)), n_transitions = 0L))
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  dw <- vapply(seq_len(K), function(k) if (!length(runlens[[k]])) NA_real_ else mean(runlens[[k]]), numeric(1))
  tibble(occupancy_entropy = ent(occ), state_switch_rate = sw / nt,
         self_transition_probability = sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0)),
         transition_entropy = sum(pi_s * rowH),
         mean_dwell_bins = sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)]),
         n_blocks = length(unique(blk)), n_transitions = nt)
}

roster <- build_canonical_identity_roster(
  read_csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv",
    col_types = cols(.default = col_skip(), AnimalNum = col_character(), Group = col_character(), Sex = col_character()),
    progress = FALSE), "roster")

out <- list()
for (res in c("10min_based", "5min_based")) {
  a <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                col_types = cols(AnimalNum = col_character(), State = col_character(), .default = col_guess()))
  aud <- audit_hmm_identity(a, roster, res); assert_hmm_identity_audit(aud)
  K <- n_distinct(as.integer(aud$data$State))
  m <- aud$data %>%
    mutate(PhaseClass = case_when(str_detect(str_to_lower(Phase), PH_I) ~ "Inactive",
                                  str_detect(str_to_lower(Phase), PH_A) ~ "Active", TRUE ~ Phase),
           CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+"))) %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    arrange(TimeIndex, .by_group = TRUE) %>% group_modify(~ gap_metrics(.x, K)) %>% ungroup()
  cat("\n####", res, " epochs:", nrow(m), "\n")
  cat("  blocks per epoch:", paste(names(table(m$n_blocks)), table(m$n_blocks), sep = "x", collapse = " "), "\n")
  cat("  non-contiguous epochs (n_blocks>1):", sum(m$n_blocks > 1), "of", nrow(m), "\n")
  naive <- read_csv(file.path(A, "hmm_architecture_temporal_epoch_metrics.csv"),
                    col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
    filter(resolution == res) %>% mutate(AnimalNum = canonical_animal_id(AnimalNum))
  j <- m %>% inner_join(naive, by = c("AnimalNum","CageChangeIndex","PhaseClass"), suffix = c("_gap","_naive"))
  for (v in METRICS) cat(sprintf("  %-28s r(gap,naive)=%.4f  mean gap=%.4f naive=%.4f  (%.1f%% shift)\n", v,
      suppressWarnings(cor(j[[paste0(v,"_gap")]], j[[paste0(v,"_naive")]], use="complete.obs")),
      mean(j[[paste0(v,"_gap")]], na.rm=TRUE), mean(j[[paste0(v,"_naive")]], na.rm=TRUE),
      100*(mean(j[[paste0(v,"_gap")]],na.rm=TRUE)/mean(j[[paste0(v,"_naive")]],na.rm=TRUE)-1)))
  sc <- reduce(METRICS, function(x, v) strict_standardize_within_context(x, v), .init = m)
  long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, all_of(paste0(METRICS,"_z"))) %>%
    pivot_longer(all_of(paste0(METRICS,"_z")), names_to="Domain", values_to="DomainScore") %>%
    mutate(Domain = sub("_z$","",Domain)) %>% filter(is.finite(DomainScore))
  for (ph in c("Active","Inactive")) for (v in METRICS)
    out[[length(out)+1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
      mutate(resolution = res, metric_variant = "gap_aware")
}
gt <- bind_rows(out); write_csv(gt, file.path(A, "hmm_architecture_gap_aware_contrasts.csv"))
cat("\n=== FEMALE 10min: GAP-AWARE contrasts (compare with naive reported earlier) ===\n")
print(as.data.frame(gt %>% filter(resolution=="10min_based", Sex=="Female") %>%
  transmute(PhaseClass, Domain=substr(Domain,1,28), contrast, est=round(mixed_model_estimate,3),
            SE=round(mixed_model_SE,3), p=signif(mixed_model_p,3), g=round(animal_level_hedges_g,3)) %>%
  arrange(PhaseClass, contrast, Domain)), row.names=FALSE)
