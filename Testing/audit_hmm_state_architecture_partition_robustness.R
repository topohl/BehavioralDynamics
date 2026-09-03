## The state PARTITION is not identified at either resolution (5 distinct 10-min optima over 8 seeds).
## Decisive follow-up: is the GROUP-LEVEL PHENOTYPE identified even though the partition is not?
## Refit 10min at several seeds, extract Viterbi states from EACH local optimum, recompute the
## temporal + occupancy components, and run the SAME corrected estimator on each solution.
## If the female contrasts agree across materially different partitions, the phenotype is robust
## to the identifiability problem. WRITES ONLY TO SCRATCHPAD.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

OUT <- getOption("mmm.audit_out", "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
n_states <- 4L; sd_floor <- 0.05
SEEDS <- c(7L, 1L, 101L, 23L, 313L)   # spans all five distinct 10-min optima found

depmix_loglik <- methods::getMethod("logLik", "depmix", where = asNamespace("depmixS4"))
fit <- depmixS4::fit
if (!methods::isClass("HMMRegularizedNORMresponse")) methods::setClass("HMMRegularizedNORMresponse", contains = "NORMresponse")
methods::setMethod("fit", signature(object = "HMMRegularizedNORMresponse"), function(object, w) {
  if (missing(w)) w <- NULL
  nas <- is.na(rowSums(object@y)); pars <- object@parameters
  if (!is.null(w)) {
    fr <- stats::lm.wfit(as.matrix(object@x[!nas, , drop = FALSE]), as.matrix(object@y[!nas, , drop = FALSE]), w[!nas])
    fsd <- sqrt(sum(w[!nas] * fr$residuals^2 / sum(w[!nas])))
  } else {
    fr <- stats::lm.fit(as.matrix(object@x[!nas, , drop = FALSE]), as.matrix(object@y[!nas, , drop = FALSE]))
    fsd <- sqrt(sum(fr$residuals^2) / length(fr$residuals))
  }
  pars$coefficients <- fr$coefficients; pars$sd <- max(fsd, sd_floor)
  depmixS4::setpars(object, unlist(pars))
})
init_km <- function(mod, hd, K, seed) {
  em <- as.matrix(hd[, c("Movement_z", "Entropy_z", "Proximity_z")]); set.seed(seed)
  km <- stats::kmeans(em, centers = K, iter.max = 100, nstart = 1)
  for (si in seq_len(K)) { rows <- km$cluster == si
    for (ri in seq_len(ncol(em))) {
      mod@response[[si]][[ri]] <- methods::new("HMMRegularizedNORMresponse", mod@response[[si]][[ri]])
      v <- em[rows, ri]
      mod@response[[si]][[ri]]@parameters$coefficients[] <- mean(v)
      mod@response[[si]][[ri]]@parameters$sd <- max(stats::sd(v), sd_floor)
      mod@dens[, ri, si] <- stats::dnorm(em[, ri], mean = mod@response[[si]][[ri]]@parameters$coefficients,
                                         sd = mod@response[[si]][[ri]]@parameters$sd) } }
  sid <- as.character(hd$SequenceID); st <- !duplicated(sid)
  ic <- tabulate(km$cluster[st], nbins = K) + 1; mod@init[1, ] <- ic / sum(ic)
  same <- sid[-length(sid)] == sid[-1]
  for (f in seq_len(K)) { fr <- which(km$cluster[-length(km$cluster)] == f & same)
    tc <- tabulate(km$cluster[fr + 1L], nbins = K) + 1; mod@trDens[1, , f] <- tc / sum(tc) }
  mod
}
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }
epoch_metrics <- function(d, K) {
  s <- as.integer(d$State); n <- length(s); occ <- tabulate(s, nbins = K) / n
  if (n < 2) return(tibble(occupancy_entropy = ent(occ), state_switch_rate = NA_real_,
                           self_transition_probability = NA_real_, transition_entropy = NA_real_, mean_dwell_bins = NA_real_))
  from <- s[-n]; to <- s[-1]; nt <- n - 1L
  TC <- matrix(0L, K, K); for (i in seq_len(nt)) TC[from[i], to[i]] <- TC[from[i], to[i]] + 1L
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  runs <- rle(s)
  dw <- vapply(seq_len(K), function(k) { l <- runs$lengths[runs$values == k]; if (!length(l)) NA_real_ else mean(l) }, numeric(1))
  tibble(occupancy_entropy = ent(occ), state_switch_rate = mean(from != to),
         self_transition_probability = sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0)),
         transition_entropy = sum(pi_s * rowH),
         mean_dwell_bins = sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)]))
}
METRICS <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability", "transition_entropy", "mean_dwell_bins")
PH_INACT <- "\\binactive\\b|\\blight\\b|\\bday\\b"; PH_ACT <- "\\bactive\\b|\\bdark\\b|\\bnight\\b"

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE), "roster")
raw <- read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"),
                col_types = cols(AnimalNum = col_character()), progress = FALSE, show_col_types = FALSE)
idn <- audit_hmm_identity(raw, roster, "10min probe"); assert_hmm_identity_audit(idn)
hd <- standardize_behavior_columns(idn$data, proximity_col = "ProximityFraction") %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum), Movement_z = z_within_metric(Movement),
         Entropy_z = z_within_metric(Entropy), Proximity_z = z_within_metric(Proximity)) %>%
  filter(is.finite(Movement_z), is.finite(Entropy_z), is.finite(Proximity_z)) %>%
  arrange(AnimalNum, CageChange, Phase, TimeIndex) %>%
  mutate(SequenceID = interaction(AnimalNum, CageChange, Phase, drop = TRUE, sep = "__"))
stbl <- hd %>% count(SequenceID, name = "n_bins") %>% filter(n_bins >= 4)
hd <- hd %>% semi_join(stbl %>% select(SequenceID), by = "SequenceID") %>%
  mutate(SequenceID = factor(SequenceID, levels = stbl$SequenceID)) %>% arrange(SequenceID, TimeIndex)
mod <- depmixS4::depmix(list(Movement_z ~ 1, Entropy_z ~ 1, Proximity_z ~ 1), data = hd,
                        ntimes = stbl$n_bins, nstates = n_states, family = list(gaussian(), gaussian(), gaussian()))

allc <- list()
for (sd_seed in SEEDS) {
  f <- suppressWarnings(depmixS4::fit(init_km(mod, hd, n_states, sd_seed), verbose = FALSE,
        emcontrol = depmixS4::em.control(maxit = 500L, tol = 1e-6, random.start = FALSE)))
  ll <- as.numeric(depmix_loglik(f))
  d <- hd; d$State <- as.character(depmixS4::posterior(f, type = "viterbi")$state)
  prof <- d %>% group_by(State) %>% summarise(P = mean(Proximity_z), M = mean(Movement_z), .groups = "drop") %>% arrange(P)
  cat("\n#### seed", sd_seed, " logLik =", round(ll, 2), " Prox profile =",
      paste(round(prof$P, 3), collapse = "|"), "\n")
  m <- d %>% mutate(PhaseClass = case_when(str_detect(str_to_lower(Phase), PH_INACT) ~ "Inactive",
                                           str_detect(str_to_lower(Phase), PH_ACT) ~ "Active", TRUE ~ Phase),
                    CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+"))) %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    arrange(TimeIndex, .by_group = TRUE) %>% group_modify(~ epoch_metrics(.x, n_states)) %>% ungroup()
  sc <- reduce(METRICS, function(a, v) strict_standardize_within_context(a, v), .init = m)
  long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, all_of(paste0(METRICS, "_z"))) %>%
    pivot_longer(all_of(paste0(METRICS, "_z")), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(Domain = sub("_z$", "", Domain)) %>% filter(is.finite(DomainScore))
  for (ph in c("Active", "Inactive")) for (v in METRICS)
    allc[[length(allc) + 1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
      mutate(seed = sd_seed, logLik = ll, prox_profile = paste(round(prof$P, 3), collapse = "|"))
}
rt <- bind_rows(allc)
write_csv(rt, file.path(OUT, "hmm_architecture_partition_robustness_contrasts.csv"))

cat("\n===== FEMALE contrasts ACROSS FIVE DIFFERENT 10-min LOCAL OPTIMA (context-z estimates) =====\n")
for (ph in c("Active", "Inactive")) {
  cat("\n### ", ph, "\n", sep = "")
  print(as.data.frame(rt %>% filter(Sex == "Female", PhaseClass == ph) %>%
    select(Domain, contrast, seed, logLik, est = mixed_model_estimate) %>%
    mutate(est = round(est, 3), logLik = round(logLik)) %>%
    pivot_wider(names_from = seed, values_from = est, names_prefix = "seed") %>%
    select(-logLik) %>% distinct() %>% arrange(Domain, contrast)), row.names = FALSE)
}
cat("\n===== sign concordance across optima (Female) =====\n")
print(as.data.frame(rt %>% filter(Sex == "Female") %>% group_by(PhaseClass, Domain, contrast) %>%
  summarise(n_optima = n(), all_same_sign = n_distinct(sign(mixed_model_estimate)) == 1,
            est_min = round(min(mixed_model_estimate), 3), est_max = round(max(mixed_model_estimate), 3),
            .groups = "drop") %>% arrange(PhaseClass, Domain, contrast)), row.names = FALSE)
