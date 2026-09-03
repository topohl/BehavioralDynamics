## PHASE A, issue 3: partition robustness of top_proximity_state_fraction across 10-min optima.
## PHASE A, issue 4: explicit gap-aware sensitivity table with abs/pct differences and CIs.
## Read-only w.r.t. production code. Refits 10-min HMM at 5 seeds spanning all distinct optima.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
OUT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
K <- 4L; sd_floor <- 0.05; SEEDS <- c(7L, 1L, 101L, 23L, 313L)
PH_I <- "\\binactive\\b|\\blight\\b|\\bday\\b"; PH_A <- "\\bactive\\b|\\bdark\\b|\\bnight\\b"

depmix_loglik <- methods::getMethod("logLik", "depmix", where = asNamespace("depmixS4"))
fit <- depmixS4::fit
if (!methods::isClass("HMMRegularizedNORMresponse")) methods::setClass("HMMRegularizedNORMresponse", contains = "NORMresponse")
methods::setMethod("fit", signature(object = "HMMRegularizedNORMresponse"), function(object, w) {
  if (missing(w)) w <- NULL
  nas <- is.na(rowSums(object@y)); pars <- object@parameters
  if (!is.null(w)) { fr <- stats::lm.wfit(as.matrix(object@x[!nas,,drop=FALSE]), as.matrix(object@y[!nas,,drop=FALSE]), w[!nas])
    fsd <- sqrt(sum(w[!nas]*fr$residuals^2/sum(w[!nas]))) } else {
    fr <- stats::lm.fit(as.matrix(object@x[!nas,,drop=FALSE]), as.matrix(object@y[!nas,,drop=FALSE]))
    fsd <- sqrt(sum(fr$residuals^2)/length(fr$residuals)) }
  pars$coefficients <- fr$coefficients; pars$sd <- max(fsd, sd_floor); depmixS4::setpars(object, unlist(pars)) })
init_km <- function(mod, hd, K, seed) {
  em <- as.matrix(hd[, c("Movement_z","Entropy_z","Proximity_z")]); set.seed(seed)
  km <- stats::kmeans(em, centers = K, iter.max = 100, nstart = 1)
  for (si in seq_len(K)) { rows <- km$cluster == si
    for (ri in seq_len(ncol(em))) {
      mod@response[[si]][[ri]] <- methods::new("HMMRegularizedNORMresponse", mod@response[[si]][[ri]])
      v <- em[rows, ri]; mod@response[[si]][[ri]]@parameters$coefficients[] <- mean(v)
      mod@response[[si]][[ri]]@parameters$sd <- max(stats::sd(v), sd_floor)
      mod@dens[, ri, si] <- stats::dnorm(em[, ri], mean = mod@response[[si]][[ri]]@parameters$coefficients,
                                          sd = mod@response[[si]][[ri]]@parameters$sd) } }
  sid <- as.character(hd$SequenceID); st <- !duplicated(sid)
  ic <- tabulate(km$cluster[st], nbins = K) + 1; mod@init[1,] <- ic/sum(ic)
  same <- sid[-length(sid)] == sid[-1]
  for (f in seq_len(K)) { fr <- which(km$cluster[-length(km$cluster)] == f & same)
    tc <- tabulate(km$cluster[fr+1L], nbins = K) + 1; mod@trDens[1,,f] <- tc/sum(tc) }
  mod }
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p*log(p)) }

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types = cols(.default=col_skip(), AnimalNum=col_character(), Group=col_character(), Sex=col_character()),
    progress = FALSE), "roster")
raw <- read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"),
                col_types = cols(AnimalNum = col_character()), progress = FALSE, show_col_types = FALSE)
idn <- audit_hmm_identity(raw, roster, "10min"); assert_hmm_identity_audit(idn)
hd <- standardize_behavior_columns(idn$data, proximity_col = "ProximityFraction") %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum), Movement_z = z_within_metric(Movement),
         Entropy_z = z_within_metric(Entropy), Proximity_z = z_within_metric(Proximity)) %>%
  filter(is.finite(Movement_z), is.finite(Entropy_z), is.finite(Proximity_z)) %>%
  arrange(AnimalNum, CageChange, Phase, TimeIndex) %>%
  mutate(SequenceID = interaction(AnimalNum, CageChange, Phase, drop = TRUE, sep = "__"))
stbl <- hd %>% count(SequenceID, name="n_bins") %>% filter(n_bins >= 4)
hd <- hd %>% semi_join(stbl %>% select(SequenceID), by="SequenceID") %>%
  mutate(SequenceID = factor(SequenceID, levels = stbl$SequenceID)) %>% arrange(SequenceID, TimeIndex)
mod <- depmixS4::depmix(list(Movement_z~1, Entropy_z~1, Proximity_z~1), data = hd,
                        ntimes = stbl$n_bins, nstates = K, family = list(gaussian(),gaussian(),gaussian()))

profiles <- list(); fracs <- list(); contrasts_l <- list()
for (sd_seed in SEEDS) {
  f <- suppressWarnings(depmixS4::fit(init_km(mod, hd, K, sd_seed), verbose = FALSE,
        emcontrol = depmixS4::em.control(maxit = 500L, tol = 1e-6, random.start = FALSE)))
  ll <- as.numeric(depmix_loglik(f))
  d <- hd; d$State <- as.character(depmixS4::posterior(f, type="viterbi")$state)
  prof <- d %>% group_by(State) %>%
    summarise(n_bins = n(), Movement_z = mean(Movement_z), Entropy_z = mean(Entropy_z),
              Proximity_z = mean(Proximity_z), .groups="drop") %>%
    mutate(occupancy_share = n_bins/sum(n_bins),
           proximity_rank = rank(-Proximity_z), movement_rank = rank(-Movement_z),
           is_top_proximity = Proximity_z == max(Proximity_z), seed = sd_seed, logLik = ll)
  top <- prof$State[which.max(prof$Proximity_z)]
  profiles[[length(profiles)+1]] <- prof
  ep <- d %>% mutate(PhaseClass = case_when(str_detect(str_to_lower(Phase), PH_I) ~ "Inactive",
                                            str_detect(str_to_lower(Phase), PH_A) ~ "Active", TRUE ~ Phase),
                     CageChangeIndex = as.integer(str_extract(as.character(CageChange), "\\d+"))) %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    summarise(top_proximity_state_fraction = mean(State == top),
              occupancy_entropy = ent(tabulate(as.integer(State), nbins=K)/n()), .groups="drop") %>%
    mutate(seed = sd_seed)
  fracs[[length(fracs)+1]] <- ep
  sc <- reduce(c("top_proximity_state_fraction","occupancy_entropy"),
               function(a,v) strict_standardize_within_context(a, v), .init = ep)
  long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass,
                        top_proximity_state_fraction_z, occupancy_entropy_z) %>%
    pivot_longer(ends_with("_z"), names_to="Domain", values_to="DomainScore") %>%
    mutate(Domain = sub("_z$","",Domain)) %>% filter(is.finite(DomainScore))
  for (ph in c("Active","Inactive")) for (v in c("top_proximity_state_fraction","occupancy_entropy"))
    contrasts_l[[length(contrasts_l)+1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
      mutate(seed = sd_seed, logLik = ll, top_state = top,
             top_state_Proximity_z = prof$Proximity_z[prof$State==top],
             top_state_Movement_z = prof$Movement_z[prof$State==top],
             top_state_Entropy_z = prof$Entropy_z[prof$State==top],
             top_state_occupancy_share = prof$occupancy_share[prof$State==top])
  cat("seed", sd_seed, "logLik", round(ll,1), "top state S", top,
      " Prox_z", round(prof$Proximity_z[prof$State==top],3),
      " Mov_z", round(prof$Movement_z[prof$State==top],3),
      " occ", round(prof$occupancy_share[prof$State==top],3), "\n"); flush.console()
}
pf <- bind_rows(profiles); fr <- bind_rows(fracs); ct <- bind_rows(contrasts_l)
write_csv(pf, file.path(OUT,"phaseA_issue3_topproximity_state_profiles.csv"))
write_csv(ct, file.path(OUT,"phaseA_issue3_topproximity_partition_contrasts.csv"))

cat("\n===== ISSUE 3a: identity + emissions of the argmax-proximity state per optimum =====\n")
print(as.data.frame(ct %>% distinct(seed, logLik, top_state, top_state_Movement_z, top_state_Entropy_z,
  top_state_Proximity_z, top_state_occupancy_share) %>%
  mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)

cat("\n===== ISSUE 3b: animal-level correlations of top_proximity_state_fraction BETWEEN optima =====\n")
for (ph in c("Active","Inactive")) {
  w <- fr %>% filter(PhaseClass==ph) %>% group_by(AnimalNum, seed) %>%
    summarise(v = mean(top_proximity_state_fraction), .groups="drop") %>%
    pivot_wider(names_from=seed, values_from=v, names_prefix="s")
  m <- round(cor(w %>% select(-AnimalNum), use="complete.obs"), 3)
  cat("\n--", ph, "(animal level, n =", nrow(w), ")\n"); print(m)
  cat("   min off-diagonal r =", round(min(m[upper.tri(m)]),3), "\n")
}

cat("\n===== ISSUE 3c: FEMALE contrasts for top_proximity_state_fraction across optima =====\n")
for (ph in c("Active","Inactive")) {
  cat("\n--", ph, "\n")
  print(as.data.frame(ct %>% filter(Sex=="Female", PhaseClass==ph, Domain=="top_proximity_state_fraction") %>%
    transmute(contrast, seed, top_state, Prox_z=round(top_state_Proximity_z,2),
              est=round(mixed_model_estimate,3), SE=round(mixed_model_SE,3),
              p=signif(mixed_model_p,3), g=round(animal_level_hedges_g,3)) %>%
    arrange(contrast, seed)), row.names=FALSE)
}
cat("\n===== ISSUE 3d: sign stability =====\n")
print(as.data.frame(ct %>% filter(Sex=="Female") %>% group_by(Domain, PhaseClass, contrast) %>%
  summarise(n_optima=n(), all_same_sign = n_distinct(sign(mixed_model_estimate))==1,
            est_min=round(min(mixed_model_estimate),3), est_max=round(max(mixed_model_estimate),3),
            .groups="drop") %>% arrange(Domain, PhaseClass, contrast)), row.names=FALSE)

## ---------------- ISSUE 4: explicit gap-aware sensitivity table ----------------
cat("\n\n################ ISSUE 4: GAP-AWARE SENSITIVITY ################\n")
comp <- read_csv(file.path(OUT,"hmm_architecture_component_epoch_metrics.csv"),
                 col_types = cols(AnimalNum=col_character(), .default=col_guess()))
cat("Gap detection: a new sequence/run boundary is inserted where diff(TimeIndex) > 1.5 * median(step).\n")
print(as.data.frame(comp %>% group_by(resolution) %>%
  summarise(epochs=n(), non_contiguous=sum(n_time_blocks>1),
            median_blocks=median(n_time_blocks), max_blocks=max(n_time_blocks),
            median_max_gap_bins=median(max_time_gap_bins, na.rm=TRUE),
            gap_bridged_transitions=sum(n_gap_bridged_transitions, na.rm=TRUE),
            bouts_merged=sum(n_bouts_merged_across_gaps, na.rm=TRUE), .groups="drop")), row.names=FALSE)
PAIRS <- list(c("transition_entropy","transition_entropy_gapaware"),
              c("state_switch_rate","state_switch_rate_gapaware"),
              c("mean_dwell_bins","mean_dwell_bins_gapaware"),
              c("self_transition_probability","self_transition_probability_gapaware"))
cat("\nEpoch-level metric shift (all 882 epochs per resolution):\n")
print(as.data.frame(map_dfr(PAIRS, function(p) comp %>% group_by(resolution) %>%
  summarise(metric=p[1], r=cor(.data[[p[1]]], .data[[p[2]]], use="complete.obs"),
            mean_orig=mean(.data[[p[1]]], na.rm=TRUE), mean_gap=mean(.data[[p[2]]], na.rm=TRUE),
            abs_diff=mean_gap-mean_orig, pct_diff=100*(mean_gap/mean_orig-1), .groups="drop")) %>%
  mutate(across(where(is.numeric), ~round(.x,4)))), row.names=FALSE)

G <- c("transition_entropy","state_switch_rate","mean_dwell_bins","self_transition_probability")
cmp <- list()
for (rs in c("10min_based","5min_based")) {
  d <- comp %>% filter(resolution == rs)
  for (variant in c("original","gapaware")) {
    cols <- if (variant=="original") G else paste0(G,"_gapaware")
    sc <- reduce(cols, function(a,v) strict_standardize_within_context(a, v), .init = d)
    long <- sc %>% select(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass, all_of(paste0(cols,"_z"))) %>%
      pivot_longer(all_of(paste0(cols,"_z")), names_to="Domain", values_to="DomainScore") %>%
      mutate(Domain = sub("_gapaware$", "", sub("_z$", "", Domain))) %>% filter(is.finite(DomainScore))
    for (ph in c("Active","Inactive")) for (v in G)
      cmp[[length(cmp)+1]] <- fit_repeated_measures_domain_contrasts(long, v, ph)$contrasts %>%
        mutate(resolution = rs, variant = variant)
  }
}
gt <- bind_rows(cmp)
gcmp <- gt %>% select(resolution, Domain, PhaseClass, Sex, contrast, variant,
                      est=mixed_model_estimate, SE=mixed_model_SE, p=mixed_model_p) %>%
  pivot_wider(names_from=variant, values_from=c(est,SE,p)) %>%
  mutate(abs_diff = est_gapaware - est_original,
         pct_diff = ifelse(abs(est_original) > 1e-8, 100*(est_gapaware/est_original - 1), NA_real_),
         direction_changed = sign(est_original) != sign(est_gapaware),
         ci_low_original = est_original - 1.96*SE_original, ci_high_original = est_original + 1.96*SE_original,
         ci_low_gapaware = est_gapaware - 1.96*SE_gapaware, ci_high_gapaware = est_gapaware + 1.96*SE_gapaware)
write_csv(gcmp, file.path(OUT,"phaseA_issue4_gapaware_contrast_comparison.csv"))
cat("\n===== FEMALE ACTIVE, 10min: original vs gap-aware contrasts =====\n")
print(as.data.frame(gcmp %>% filter(resolution=="10min_based", PhaseClass=="Active", Sex=="Female") %>%
  transmute(Domain=substr(Domain,1,28), contrast, orig=round(est_original,3), gap=round(est_gapaware,3),
            abs_diff=round(abs_diff,4), pct=round(pct_diff,1),
            p_orig=signif(p_original,3), p_gap=signif(p_gapaware,3), dir_change=direction_changed) %>%
  arrange(contrast, Domain)), row.names=FALSE)
cat("\nAny direction change anywhere?", any(gcmp$direction_changed, na.rm=TRUE), "\n")
cat("Max |pct_diff| over all cells with |est|>0.15:",
    round(max(abs(gcmp$pct_diff[abs(gcmp$est_original)>0.15]), na.rm=TRUE),2), "%\n")
cat("\nwrote phaseA_issue3 + issue4 CSVs\n")
