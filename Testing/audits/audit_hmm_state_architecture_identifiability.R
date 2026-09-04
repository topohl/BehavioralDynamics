## Point (4): is the 5-min HMM fit actually IDENTIFIED, or is the multi-start logLik spread a
## symptom of a degenerate (spiked) likelihood?
## Faithful replication of Stage 08's model setup and initialization, with MORE seeds.
## WRITES ONLY TO SCRATCHPAD. Does not touch any pipeline artifact.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("duration_normalization_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")

OUT <- getOption("mmm.audit_out", "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
n_states <- 4L
sd_floor <- 0.05
em_tol <- 1e-6
em_maxit <- 500L
SEEDS <- c(1L, 11L, 101L, 7L, 23L, 57L, 199L, 313L)

depmix_loglik <- methods::getMethod("logLik", "depmix", where = asNamespace("depmixS4"))
fit <- depmixS4::fit
if (!methods::isClass("HMMRegularizedNORMresponse")) {
  methods::setClass("HMMRegularizedNORMresponse", contains = "NORMresponse")
}
methods::setMethod("fit", signature(object = "HMMRegularizedNORMresponse"), function(object, w) {
  if (missing(w)) w <- NULL
  nas <- is.na(rowSums(object@y)); pars <- object@parameters
  if (!is.null(w)) {
    fr <- stats::lm.wfit(x = as.matrix(object@x[!nas, , drop = FALSE]),
                         y = as.matrix(object@y[!nas, , drop = FALSE]), w = w[!nas])
    fsd <- sqrt(sum(w[!nas] * fr$residuals^2 / sum(w[!nas])))
  } else {
    fr <- stats::lm.fit(x = as.matrix(object@x[!nas, , drop = FALSE]),
                        y = as.matrix(object@y[!nas, , drop = FALSE]))
    fsd <- sqrt(sum(fr$residuals^2) / length(fr$residuals))
  }
  pars$coefficients <- fr$coefficients; pars$sd <- max(fsd, sd_floor)
  depmixS4::setpars(object, unlist(pars))
})

init_from_kmeans <- function(mod, hmm_dat, n_states, seed) {
  em <- as.matrix(hmm_dat[, c("Movement_z", "Entropy_z", "Proximity_z")])
  set.seed(seed)
  km <- stats::kmeans(em, centers = n_states, iter.max = 100, nstart = 1)
  for (si in seq_len(n_states)) {
    rows <- km$cluster == si
    for (ri in seq_len(ncol(em))) {
      mod@response[[si]][[ri]] <- methods::new("HMMRegularizedNORMresponse", mod@response[[si]][[ri]])
      v <- em[rows, ri]
      mod@response[[si]][[ri]]@parameters$coefficients[] <- mean(v)
      mod@response[[si]][[ri]]@parameters$sd <- max(stats::sd(v), sd_floor)
      mod@dens[, ri, si] <- stats::dnorm(em[, ri],
        mean = mod@response[[si]][[ri]]@parameters$coefficients,
        sd = mod@response[[si]][[ri]]@parameters$sd)
    }
  }
  sid <- as.character(hmm_dat$SequenceID)
  starts <- !duplicated(sid)
  ic <- tabulate(km$cluster[starts], nbins = n_states) + 1
  mod@init[1, ] <- ic / sum(ic)
  same <- sid[-length(sid)] == sid[-1]
  for (f in seq_len(n_states)) {
    fr <- which(km$cluster[-length(km$cluster)] == f & same)
    tc <- tabulate(km$cluster[fr + 1L], nbins = n_states) + 1
    mod@trDens[1, , f] <- tc / sum(tc)
  }
  mod
}

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE),
  "roster")

probe <- list()
for (bin_level in c("5min_based", "10min_based")) {
  raw <- read_csv(file.path(PROJ, "analysis_ready/03_derived_metrics", bin_level, "all_behavior_metrics.csv"),
                  col_types = cols(AnimalNum = col_character()), progress = FALSE, show_col_types = FALSE)
  idn <- audit_hmm_identity(raw, roster, paste("probe", bin_level)); assert_hmm_identity_audit(idn)
  behav <- standardize_behavior_columns(idn$data, proximity_col = "ProximityFraction")
  hd <- behav %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum),
           Movement_z = z_within_metric(Movement), Entropy_z = z_within_metric(Entropy),
           Proximity_z = z_within_metric(Proximity)) %>%
    filter(is.finite(Movement_z), is.finite(Entropy_z), is.finite(Proximity_z)) %>%
    arrange(AnimalNum, CageChange, Phase, TimeIndex) %>%
    mutate(SequenceID = interaction(AnimalNum, CageChange, Phase, drop = TRUE, sep = "__"))
  stbl <- hd %>% count(SequenceID, name = "n_bins") %>% filter(n_bins >= 4)
  hd <- hd %>% semi_join(stbl %>% select(SequenceID), by = "SequenceID") %>%
    mutate(SequenceID = factor(SequenceID, levels = stbl$SequenceID)) %>% arrange(SequenceID, TimeIndex)
  cat("\n#### ", bin_level, ": rows =", nrow(hd), " sequences =", nrow(stbl), "\n")

  mod <- depmixS4::depmix(list(Movement_z ~ 1, Entropy_z ~ 1, Proximity_z ~ 1),
                          data = hd, ntimes = stbl$n_bins, nstates = n_states,
                          family = list(gaussian(), gaussian(), gaussian()))
  for (sd_seed in SEEDS) {
    t0 <- proc.time()[["elapsed"]]
    f <- tryCatch(suppressWarnings(depmixS4::fit(init_from_kmeans(mod, hd, n_states, sd_seed),
      verbose = FALSE,
      emcontrol = depmixS4::em.control(maxit = em_maxit, tol = em_tol, random.start = FALSE))),
      error = function(e) e)
    if (inherits(f, "error")) {
      cat(sprintf("  seed %4d: ERROR %s\n", sd_seed, conditionMessage(f)))
      next
    }
    ll <- as.numeric(depmix_loglik(f))
    pv <- depmixS4::getpars(f)
    # emission sd parameters are every 3rd element of each state's response block (mean, sd) pairs
    pn <- names(pv)
    sds <- unname(pv[pn == "sd"])
    st <- as.integer(depmixS4::posterior(f, type = "viterbi")$state)
    prof <- tapply(seq_along(st), st, function(i) c(mean(hd$Movement_z[i]), mean(hd$Entropy_z[i]), mean(hd$Proximity_z[i])))
    ord <- order(sapply(prof, function(p) p[3]))
    prox_sorted <- round(sapply(prof, function(p) p[3])[ord], 4)
    mov_sorted <- round(sapply(prof, function(p) p[1])[ord], 4)
    probe[[length(probe) + 1]] <- tibble(
      bin_level, seed = sd_seed, logLik = ll, n_states_occupied = length(unique(st)),
      n_sd_at_floor = sum(abs(sds - sd_floor) < 1e-8), n_sd_params = length(sds),
      min_sd = min(sds), median_sd = median(sds),
      prox_profile = paste(prox_sorted, collapse = "|"),
      mov_profile = paste(mov_sorted, collapse = "|"),
      elapsed_sec = round(proc.time()[["elapsed"]] - t0, 1))
    cat(sprintf("  seed %4d: logLik = %14.3f  states = %d  sd@floor = %d/%d  minSD = %.4f  Prox = %s  (%.0fs)\n",
                sd_seed, ll, length(unique(st)), sum(abs(sds - sd_floor) < 1e-8), length(sds),
                min(sds), paste(prox_sorted, collapse = "|"), proc.time()[["elapsed"]] - t0))
    flush.console()
  }
}
pt <- bind_rows(probe)
write_csv(pt, file.path(OUT, "hmm_architecture_identifiability_probe.csv"))
cat("\n=============== IDENTIFIABILITY SUMMARY ===============\n")
print(as.data.frame(pt %>% group_by(bin_level) %>%
  summarise(n_seeds = n(), logLik_min = min(logLik), logLik_max = max(logLik),
            logLik_spread = max(logLik) - min(logLik),
            logLik_rel_spread = (max(logLik) - min(logLik)) / abs(median(logLik)),
            n_distinct_solutions = n_distinct(round(logLik, 2)),
            max_sd_at_floor = max(n_sd_at_floor), sd_params = first(n_sd_params),
            n_distinct_prox_profiles = n_distinct(prox_profile))), digits = 6)
cat("\nDistinct proximity profiles per resolution:\n")
print(as.data.frame(pt %>% count(bin_level, prox_profile, name = "n_seeds") %>% arrange(bin_level, desc(n_seeds))))
cat("\nwrote hmm_identifiability_probe.csv\n")
