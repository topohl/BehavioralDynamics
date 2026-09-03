## DEDICATED AUDIT: HMM cross-optimum robustness UNDER THE GAP-AWARE CONTRACT.
##
## Why this is needed. The earlier multi-optimum audit was run under the old
## contiguous sequence contract. Gap-aware segmentation changes the sequence
## factorization handed to depmixS4, so those results cannot simply be carried
## forward as evidence -- even though the promoted state space turned out
## numerically identical up to relabeling.
##
## What the gap-aware 8-seed production run already established at 10 min:
##   4 distinct optima; logLik 43578.93 (seeds 1, 7, 199), 43578.92 (11, 101, 23),
##   -22942.88 (313), -50960.90 (57). Six of eight seeds land in the top basin.
## So the multi-optimum problem is intrinsic to the emission structure -- 66% of
## bins sit at a Movement/Entropy point mass -- and is NOT resolved by
## gap-awareness. Manuscript HMM claims therefore need cross-optimum evidence.
##
## This audit refits at seeds spanning ALL FOUR distinct gap-aware optima,
## recomputes the temporal and occupancy metrics WITHIN SequenceBlockID, runs the
## established repeated-measures estimator on each, and reports sign stability.
## Read-only; writes audit tables only.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr); library(tibble)
})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")

PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
OUT <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
K <- 4L; SD_FLOOR <- 0.05; BS <- 600
SEEDS <- c(1L, 11L, 101L, 313L, 57L)   # spans all four distinct gap-aware optima
hr <- function(x) cat("\n########", x, "########\n")
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }

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
  pars$coefficients <- fr$coefficients; pars$sd <- max(fsd, SD_FLOOR); depmixS4::setpars(object, unlist(pars))
})
init_km <- function(mod, hd, K, seed) {
  em <- as.matrix(hd[, c("Movement_z", "Entropy_z", "Proximity_z")]); set.seed(seed)
  km <- stats::kmeans(em, centers = K, iter.max = 100, nstart = 1)
  for (si in seq_len(K)) { rows <- km$cluster == si
    for (ri in seq_len(ncol(em))) {
      mod@response[[si]][[ri]] <- methods::new("HMMRegularizedNORMresponse", mod@response[[si]][[ri]])
      v <- em[rows, ri]
      mod@response[[si]][[ri]]@parameters$coefficients[] <- mean(v)
      mod@response[[si]][[ri]]@parameters$sd <- max(stats::sd(v), SD_FLOOR)
      mod@dens[, ri, si] <- stats::dnorm(em[, ri], mean = mod@response[[si]][[ri]]@parameters$coefficients,
                                          sd = mod@response[[si]][[ri]]@parameters$sd) } }
  # Gap-aware: priors and empirical transition starts within contiguous BLOCKS.
  sid <- as.character(hd$SequenceBlockID); st <- !duplicated(sid)
  ic <- tabulate(km$cluster[st], nbins = K) + 1; mod@init[1, ] <- ic / sum(ic)
  same <- sid[-length(sid)] == sid[-1]
  for (f in seq_len(K)) { fr <- which(km$cluster[-length(km$cluster)] == f & same)
    tc <- tabulate(km$cluster[fr + 1L], nbins = K) + 1; mod@trDens[1, , f] <- tc / sum(tc) }
  mod
}

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE), "roster")
raw <- read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"),
                col_types = cols(AnimalNum = col_character(), BinStart = col_datetime(),
                                 .default = col_guess()), progress = FALSE)
idn <- audit_hmm_identity(raw, roster, "gap-aware cross-optimum 10min"); assert_hmm_identity_audit(idn)

hd <- standardize_behavior_columns(idn$data, proximity_col = "ProximityFraction") %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum),
         Movement_z = z_within_metric(Movement), Entropy_z = z_within_metric(Entropy),
         Proximity_z = z_within_metric(Proximity)) %>%
  filter(is.finite(Movement_z), is.finite(Entropy_z), is.finite(Proximity_z)) %>%
  arrange(AnimalNum, CageChange, Phase, TimeIndex) %>%
  mutate(SequenceID = interaction(AnimalNum, CageChange, Phase, drop = TRUE, sep = "__"))
epoch_tbl <- hd %>% count(SequenceID, name = "n_bins") %>% filter(n_bins >= 4)
hd <- hd %>% semi_join(epoch_tbl %>% select(SequenceID), by = "SequenceID")

## Gap-aware blocks, exactly as production Stage 08 defines them.
declared_bin_size_sec <- unique(suppressWarnings(as.numeric(hd$BinSizeSec)))
declared_bin_size_sec <- declared_bin_size_sec[is.finite(declared_bin_size_sec) & declared_bin_size_sec > 0]
stopifnot(length(declared_bin_size_sec) == 1L)
hd <- hd %>% group_by(SequenceID) %>% arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(delta_sec = as.numeric(difftime(BinStart, lag(BinStart), units = "secs")),
         SequenceBlock = cumsum(coalesce(!is.na(delta_sec) & delta_sec > 1.5 * declared_bin_size_sec, FALSE)) + 1L) %>%
  ungroup() %>%
  mutate(SequenceBlockID = paste(as.character(SequenceID), SequenceBlock, sep = "__blk"))
blk_tbl <- hd %>% count(SequenceBlockID, name = "n_bins")
hd <- hd %>% mutate(SequenceBlockID = factor(SequenceBlockID, levels = blk_tbl$SequenceBlockID)) %>%
  arrange(SequenceBlockID, TimeIndex)
ntimes <- blk_tbl$n_bins
stopifnot(sum(ntimes) == nrow(hd))
cat("gap-aware contract: ", nrow(hd), " bins, ", n_distinct(hd$SequenceID), " epochs, ",
    length(ntimes), " blocks\n", sep = "")

mod <- depmixS4::depmix(list(Movement_z ~ 1, Entropy_z ~ 1, Proximity_z ~ 1), data = hd,
                        ntimes = ntimes, nstates = K, family = list(gaussian(), gaussian(), gaussian()))

epoch_metrics <- function(d) {
  s <- as.integer(d$State); n <- length(s); occ <- tabulate(s, nbins = K) / n
  blocks <- split(seq_len(n), droplevels(factor(d$SequenceBlockID)))
  TC <- matrix(0L, K, K); sw <- 0L; nt <- 0L; runs <- vector("list", K)
  for (ix in blocks) {
    sb <- s[ix]; nb <- length(sb)
    if (nb >= 2L) { f <- sb[-nb]; t2 <- sb[-1]
      for (i in seq_along(f)) TC[f[i], t2[i]] <- TC[f[i], t2[i]] + 1L
      sw <- sw + sum(f != t2); nt <- nt + (nb - 1L) }
    r <- rle(sb); for (k in seq_len(K)) runs[[k]] <- c(runs[[k]], r$lengths[r$values == k])
  }
  if (nt == 0L) return(tibble(occupancy_entropy = ent(occ), state_switch_rate = NA_real_,
    self_transition_probability = NA_real_, transition_entropy = NA_real_, mean_dwell_minutes = NA_real_))
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  dw <- vapply(seq_len(K), function(k) if (!length(runs[[k]])) NA_real_ else mean(runs[[k]]), numeric(1))
  tibble(occupancy_entropy = ent(occ), state_switch_rate = sw / nt,
         self_transition_probability = sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0)),
         transition_entropy = sum(pi_s * rowH),
         mean_dwell_minutes = sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)]) * BS / 60)
}
METRICS <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability",
             "transition_entropy", "mean_dwell_minutes")

profiles <- list(); contrasts_l <- list()
for (sd_seed in SEEDS) {
  f <- suppressWarnings(depmixS4::fit(init_km(mod, hd, K, sd_seed), verbose = FALSE,
        emcontrol = depmixS4::em.control(maxit = 500L, tol = 1e-6, random.start = FALSE)))
  ll <- as.numeric(depmix_loglik(f))
  d <- hd; d$State <- as.integer(depmixS4::posterior(f, type = "viterbi")$state)
  prof <- d %>% group_by(State) %>%
    summarise(Mov = mean(Movement_z), Ent = mean(Entropy_z), Prox = mean(Proximity_z),
              occ = n() / nrow(d), .groups = "drop") %>% arrange(Prox)
  profiles[[length(profiles) + 1]] <- prof %>% mutate(seed = sd_seed, logLik = ll)
  cat(sprintf("seed %4d logLik %11.2f  prox profile %s\n", sd_seed, ll,
              paste(round(prof$Prox, 3), collapse = "|"))); flush.console()
  m <- d %>% mutate(PhaseClass = mmm_phase_class(Phase),
                    CageChangeIndex = as.integer(str_extract(as.character(CageChange), "[0-9]+"))) %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    arrange(TimeIndex, .by_group = TRUE) %>% group_modify(~epoch_metrics(.x)) %>% ungroup()
  sc <- reduce(METRICS, function(a, v) strict_standardize_within_context(a, v), .init = m)
  long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
                        all_of(paste0(METRICS, "_z"))) %>%
    pivot_longer(all_of(paste0(METRICS, "_z")), names_to = "Domain", values_to = "DomainScore") %>%
    mutate(Domain = sub("_z$", "", Domain)) %>% filter(is.finite(DomainScore))
  for (ph in c("Active", "Inactive")) for (v in METRICS)
    contrasts_l[[length(contrasts_l) + 1]] <-
      fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
      mutate(seed = sd_seed, logLik = ll, sequence_contract = "gap_aware")
}
pf <- bind_rows(profiles); ct <- bind_rows(contrasts_l)
write_csv(pf, file.path(OUT, "hmm_cross_optimum_gapaware_state_profiles.csv"))
write_csv(ct, file.path(OUT, "hmm_cross_optimum_gapaware_contrasts.csv"))

hr("A. Distinct gap-aware optima reached")
print(as.data.frame(pf %>% distinct(seed, logLik) %>% arrange(desc(logLik)) %>%
  mutate(logLik = round(logLik, 2))), row.names = FALSE)

hr("B. Sign stability of FEMALE contrasts across gap-aware optima")
stab <- ct %>% filter(Sex == "Female") %>% group_by(PhaseClass, Domain, contrast) %>%
  summarise(n_optima = n(), all_same_sign = n_distinct(sign(mixed_model_estimate)) == 1,
            est_min = round(min(mixed_model_estimate), 3),
            est_max = round(max(mixed_model_estimate), 3),
            n_nominally_sig = sum(mixed_model_p < 0.05), .groups = "drop") %>%
  arrange(PhaseClass, Domain, contrast)
print(as.data.frame(stab), row.names = FALSE)
write_csv(stab, file.path(OUT, "hmm_cross_optimum_gapaware_sign_stability.csv"))

hr("C. Verdict per manuscript-relevant claim")
verdict <- stab %>%
  filter((PhaseClass == "Active" & contrast %in% c("SUS-CON", "SUS-RES")) |
         (PhaseClass == "Inactive" & contrast %in% c("RES-CON", "SUS-CON"))) %>%
  mutate(cross_optimum_robust = all_same_sign,
         claim_status = if_else(all_same_sign,
                               "robust across all distinct gap-aware optima",
                               "NOT robust: sign flips across optima; do not report as a finding"))
print(as.data.frame(verdict %>% select(PhaseClass, Domain, contrast, est_min, est_max,
                                       cross_optimum_robust, claim_status) %>%
  mutate(Domain = substr(Domain, 1, 28), claim_status = substr(claim_status, 1, 44))), row.names = FALSE)
write_csv(verdict, file.path(OUT, "hmm_cross_optimum_gapaware_claim_verdicts.csv"))
cat("\nrobust cells:", sum(verdict$cross_optimum_robust), "of", nrow(verdict), "\n")
