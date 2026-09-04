## PRODUCTION PREREQUISITE (B): is FIRST-NIGHT mean_dwell_minutes stable across the distinct
## defensible 10-min HMM optima? Construct-stability requirement for a row we intend to display.
## Refits the 10-min HMM at 5 seeds spanning all 5 distinct optima, restricts each optimum's Viterbi
## labels to the canonical clock window (CC1 Active, 18:30->06:30, 12 h), computes mean_dwell_minutes
## per animal, and compares across optima. Read-only; writes only to the audit dir.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("animalpos_preprocessing_helpers.R"); source_mmm_helper("hmm_stage14_helpers.R")
OUT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap"
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
K <- 4L; sd_floor <- 0.05; SEEDS <- c(7L, 1L, 101L, 23L, 313L); BS <- 600
active_vals <- c("active","dark","night"); is_act <- function(x) str_to_lower(str_trim(as.character(x))) %in% active_vals

depmix_loglik <- methods::getMethod("logLik","depmix", where=asNamespace("depmixS4"))
fit <- depmixS4::fit
if (!methods::isClass("HMMRegularizedNORMresponse")) methods::setClass("HMMRegularizedNORMresponse", contains="NORMresponse")
methods::setMethod("fit", signature(object="HMMRegularizedNORMresponse"), function(object, w) {
  if (missing(w)) w <- NULL
  nas <- is.na(rowSums(object@y)); pars <- object@parameters
  if (!is.null(w)) { fr <- stats::lm.wfit(as.matrix(object@x[!nas,,drop=FALSE]), as.matrix(object@y[!nas,,drop=FALSE]), w[!nas])
    fsd <- sqrt(sum(w[!nas]*fr$residuals^2/sum(w[!nas]))) } else {
    fr <- stats::lm.fit(as.matrix(object@x[!nas,,drop=FALSE]), as.matrix(object@y[!nas,,drop=FALSE]))
    fsd <- sqrt(sum(fr$residuals^2)/length(fr$residuals)) }
  pars$coefficients <- fr$coefficients; pars$sd <- max(fsd, sd_floor); depmixS4::setpars(object, unlist(pars)) })
init_km <- function(mod, hd, K, seed) {
  em <- as.matrix(hd[, c("Movement_z","Entropy_z","Proximity_z")]); set.seed(seed)
  km <- stats::kmeans(em, centers=K, iter.max=100, nstart=1)
  for (si in seq_len(K)) { rows <- km$cluster==si
    for (ri in seq_len(ncol(em))) {
      mod@response[[si]][[ri]] <- methods::new("HMMRegularizedNORMresponse", mod@response[[si]][[ri]])
      v <- em[rows,ri]; mod@response[[si]][[ri]]@parameters$coefficients[] <- mean(v)
      mod@response[[si]][[ri]]@parameters$sd <- max(stats::sd(v), sd_floor)
      mod@dens[,ri,si] <- stats::dnorm(em[,ri], mean=mod@response[[si]][[ri]]@parameters$coefficients,
                                       sd=mod@response[[si]][[ri]]@parameters$sd) } }
  sid <- as.character(hd$SequenceID); st <- !duplicated(sid)
  ic <- tabulate(km$cluster[st], nbins=K)+1; mod@init[1,] <- ic/sum(ic)
  same <- sid[-length(sid)]==sid[-1]
  for (f in seq_len(K)) { fr <- which(km$cluster[-length(km$cluster)]==f & same)
    tc <- tabulate(km$cluster[fr+1L], nbins=K)+1; mod@trDens[1,,f] <- tc/sum(tc) }
  mod }

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types=cols(.default=col_skip(), AnimalNum=col_character(), Group=col_character(), Sex=col_character()),
    progress=FALSE), "roster")
raw <- read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"),
                col_types=cols(AnimalNum=col_character(), BinStart=col_datetime(), .default=col_guess()), progress=FALSE)
idn <- audit_hmm_identity(raw, roster, "10min"); assert_hmm_identity_audit(idn)
hd <- standardize_behavior_columns(idn$data, proximity_col="ProximityFraction") %>%
  mutate(AnimalNum=canonical_animal_id(AnimalNum), Movement_z=z_within_metric(Movement),
         Entropy_z=z_within_metric(Entropy), Proximity_z=z_within_metric(Proximity)) %>%
  filter(is.finite(Movement_z), is.finite(Entropy_z), is.finite(Proximity_z)) %>%
  arrange(AnimalNum, CageChange, Phase, TimeIndex) %>%
  mutate(SequenceID=interaction(AnimalNum, CageChange, Phase, drop=TRUE, sep="__"))
stbl <- hd %>% count(SequenceID, name="n_bins") %>% filter(n_bins>=4)
hd <- hd %>% semi_join(stbl %>% select(SequenceID), by="SequenceID") %>%
  mutate(SequenceID=factor(SequenceID, levels=stbl$SequenceID)) %>% arrange(SequenceID, TimeIndex)

## canonical clock window keys (CC1 Active, 18:30 -> 06:30), built directly from the identity-audited
## Stage 01 table so BinStart/SourceFile are unambiguous.
cc1a <- idn$data %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum), .sess = as.character(SourceFile)) %>%
  filter(as.character(CageChange) == "CC1", is_act(Phase))
anch <- cc1a %>% mutate(.b = animalpos_phase_block_index(BinStart)) %>%
  group_by(.sess) %>% summarise(tb = min(.b, na.rm = TRUE), .groups = "drop") %>%
  mutate(ws = as.POSIXct(tb * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
                         origin = "1970-01-01", tz = "UTC"))
winkeys <- cc1a %>% left_join(anch, by = ".sess") %>%
  mutate(el = as.numeric(difftime(BinStart, ws, units = "secs"))) %>%
  filter(el >= 0, el < 12 * 3600) %>%
  transmute(AnimalNum, TimeIndex) %>% distinct()
cat("clock-window rows:", nrow(winkeys), " animals:", n_distinct(winkeys$AnimalNum), "\n")

mod <- depmixS4::depmix(list(Movement_z~1, Entropy_z~1, Proximity_z~1), data=hd,
                        ntimes=stbl$n_bins, nstates=K, family=list(gaussian(),gaussian(),gaussian()))
dwell_min <- function(st) { r <- rle(st); occ <- tabulate(st, nbins=K)/length(st)
  dw <- vapply(seq_len(K), function(k){ l<-r$lengths[r$values==k]; if(!length(l)) NA_real_ else mean(l)}, numeric(1))
  sum(occ*dw, na.rm=TRUE)/sum(occ[!is.na(dw)]) * BS/60 }

per <- list(); ctr <- list()
for (sd_seed in SEEDS) {
  f <- suppressWarnings(depmixS4::fit(init_km(mod, hd, K, sd_seed), verbose=FALSE,
        emcontrol=depmixS4::em.control(maxit=500L, tol=1e-6, random.start=FALSE)))
  ll <- as.numeric(depmix_loglik(f))
  d <- hd; d$State <- as.integer(depmixS4::posterior(f, type="viterbi")$state)
  fn <- d %>% inner_join(winkeys, by=c("AnimalNum","TimeIndex")) %>%
    arrange(AnimalNum, TimeIndex) %>% group_by(AnimalNum, Group, Sex) %>%
    summarise(mean_dwell_minutes=dwell_min(State), n_bins=n(), .groups="drop") %>%
    mutate(seed=sd_seed, logLik=ll)
  per[[length(per)+1]] <- fn
  z <- fn %>% group_by(Sex) %>% mutate(v=as.numeric(scale(mean_dwell_minutes))) %>% ungroup() %>%
    mutate(Group=factor(Group, levels=c("CON","RES","SUS")), Sex=factor(Sex, levels=c("Female","Male")))
  m <- lm(v ~ Group*Sex, data=z)
  cv <- list("RES-CON"=c(-1,1,0), "SUS-CON"=c(-1,0,1), "SUS-RES"=c(0,-1,1))
  ctr[[length(ctr)+1]] <- as.data.frame(emmeans::contrast(emmeans::emmeans(m, ~Group|Sex), cv, adjust="none")) %>%
    transmute(Sex=as.character(Sex), contrast=as.character(contrast), estimate, SE, p.value) %>%
    mutate(seed=sd_seed, logLik=ll)
  cat(sprintf("seed %4d logLik %10.1f  mean dwell %.2f min  n=%d\n", sd_seed, ll,
      mean(fn$mean_dwell_minutes, na.rm=TRUE), nrow(fn))); flush.console()
}
P <- bind_rows(per); C <- bind_rows(ctr)
write_csv(P, file.path(OUT,"first_night_dwell_partition_stability_values.csv"))
write_csv(C, file.path(OUT,"first_night_dwell_partition_stability_contrasts.csv"))

cat("\n===== (a) ANIMAL-LEVEL CORRELATIONS OF first-night mean_dwell_minutes ACROSS OPTIMA =====\n")
W <- P %>% select(AnimalNum, seed, mean_dwell_minutes) %>% pivot_wider(names_from=seed, values_from=mean_dwell_minutes, names_prefix="s")
M <- cor(W %>% select(-AnimalNum), use="complete.obs")
print(round(M,3)); cat("  min off-diagonal Pearson r =", round(min(M[upper.tri(M)]),3), "\n")
Ms <- cor(W %>% select(-AnimalNum), use="complete.obs", method="spearman")
cat("  min off-diagonal Spearman rho =", round(min(Ms[upper.tri(Ms)]),3), "\n")
cat("\n  raw magnitude range across optima (minutes): mean per optimum =",
    paste(round(tapply(P$mean_dwell_minutes, P$seed, mean, na.rm=TRUE),2), collapse=", "), "\n")

cat("\n===== (b) CONTRAST DIRECTION AND MAGNITUDE ACROSS OPTIMA =====\n")
print(as.data.frame(C %>% transmute(Sex, contrast, seed, logLik=round(logLik), est=round(estimate,3),
  SE=round(SE,3), p=signif(p.value,3)) %>% arrange(Sex, contrast, seed)), row.names=FALSE)
cat("\n===== (c) SIGN STABILITY / RANGE =====\n")
print(as.data.frame(C %>% group_by(Sex, contrast) %>%
  summarise(n_optima=n(), all_same_sign=n_distinct(sign(estimate))==1,
            est_min=round(min(estimate),3), est_max=round(max(estimate),3),
            range=round(max(estimate)-min(estimate),3),
            any_nominally_sig=any(p.value<0.05), .groups="drop")), row.names=FALSE)
cat("\nANY substantive sign reversal (Female or Male, any contrast)? ",
    any(C %>% group_by(Sex,contrast) %>% summarise(f=n_distinct(sign(estimate))>1, .groups="drop") %>% pull(f)), "\n")
