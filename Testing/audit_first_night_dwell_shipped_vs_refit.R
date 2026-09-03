## Reconcile: first-night mean_dwell_minutes from the SHIPPED Stage 08 Viterbi labels
## vs my seed-7 refit (same logLik 42864). Isolates refit-vs-shipped as the cause of the
## Female RES-CON sign discrepancy (+0.059 reported earlier vs -0.815 in the refit).
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("animalpos_preprocessing_helpers.R"); source_mmm_helper("hmm_stage14_helpers.R")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
OUT <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap")
K <- 4L; BS <- 600
is_act <- function(x) str_to_lower(str_trim(as.character(x))) %in% c("active","dark","night")

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types=cols(.default=col_skip(), AnimalNum=col_character(), Group=col_character(), Sex=col_character()),
    progress=FALSE), "roster")

## clock window from Stage 01 10-min
raw <- read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"),
                col_types=cols(AnimalNum=col_character(), BinStart=col_datetime(), .default=col_guess()), progress=FALSE) %>%
  mutate(AnimalNum=canonical_animal_id(AnimalNum)) %>% semi_join(roster, by="AnimalNum")
cc1a <- raw %>% mutate(.sess=as.character(SourceFile)) %>% filter(as.character(CageChange)=="CC1", is_act(Phase))
anch <- cc1a %>% mutate(.b=animalpos_phase_block_index(BinStart)) %>% group_by(.sess) %>%
  summarise(tb=min(.b, na.rm=TRUE), .groups="drop") %>%
  mutate(ws=as.POSIXct(tb*ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC, origin="1970-01-01", tz="UTC"))
winkeys <- cc1a %>% left_join(anch, by=".sess") %>%
  mutate(el=as.numeric(difftime(BinStart, ws, units="secs"))) %>%
  filter(el>=0, el < 12*3600) %>% transmute(AnimalNum, TimeIndex) %>% distinct()
cat("clock-window keys:", nrow(winkeys), " animals:", n_distinct(winkeys$AnimalNum), "\n")

## SHIPPED Viterbi labels
asg <- read_csv(file.path(PROJ,"analysis_ready/06_behavioral_dynamics/hmm_states/10min_based/tables/hmm_state_assignments.csv"),
                col_types=cols(AnimalNum=col_character(), State=col_character(), .default=col_guess())) %>%
  mutate(AnimalNum=canonical_animal_id(AnimalNum))
aud <- audit_hmm_identity(asg, roster, "shipped 10min"); assert_hmm_identity_audit(aud)
ship <- aud$data %>% filter(as.character(CageChange)=="CC1", is_act(Phase)) %>%
  inner_join(winkeys, by=c("AnimalNum","TimeIndex"))
cat("shipped rows in window:", nrow(ship), " animals:", n_distinct(ship$AnimalNum), "\n")
cat("animals in window keys but NOT in shipped assignments:",
    paste(setdiff(unique(winkeys$AnimalNum), unique(ship$AnimalNum)), collapse=", "), "\n")

dwell_min <- function(st){ st<-as.integer(st); r<-rle(st); occ<-tabulate(st,nbins=K)/length(st)
  dw <- vapply(seq_len(K), function(k){l<-r$lengths[r$values==k]; if(!length(l)) NA_real_ else mean(l)}, numeric(1))
  sum(occ*dw, na.rm=TRUE)/sum(occ[!is.na(dw)]) * BS/60 }

fn_ship <- ship %>% arrange(AnimalNum, TimeIndex) %>% group_by(AnimalNum, Group, Sex) %>%
  summarise(dwell_shipped=dwell_min(State), n_bins=n(), .groups="drop")
cat("\nshipped first-night dwell: n =", nrow(fn_ship), " mean =", round(mean(fn_ship$dwell_shipped),2), "min\n")

## compare with the refit values from the stability run (seed 7 and seed 1 = promoted optimum)
P <- read_csv(file.path(OUT,"first_night_dwell_partition_stability_values.csv"),
              col_types=cols(AnimalNum=col_character(), .default=col_guess()))
for (s in c(7,1)) {
  r7 <- P %>% filter(seed==s) %>% transmute(AnimalNum, dwell_refit=mean_dwell_minutes)
  j <- fn_ship %>% inner_join(r7, by="AnimalNum")
  cat(sprintf("  seed %d refit vs shipped: n=%d  r=%.4f  rho=%.4f  mean refit=%.2f  mean shipped=%.2f  max|diff|=%.3f\n",
      s, nrow(j), cor(j$dwell_refit, j$dwell_shipped), cor(j$dwell_refit, j$dwell_shipped, method="spearman"),
      mean(j$dwell_refit), mean(j$dwell_shipped), max(abs(j$dwell_refit-j$dwell_shipped))))
}

## contrasts on the SHIPPED labels (the production-relevant version)
z <- fn_ship %>% group_by(Sex) %>% mutate(v=as.numeric(scale(dwell_shipped))) %>% ungroup() %>%
  mutate(Group=factor(Group, levels=c("CON","RES","SUS")), Sex=factor(Sex, levels=c("Female","Male")))
m <- lm(v ~ Group*Sex, data=z)
cv <- list("RES-CON"=c(-1,1,0), "SUS-CON"=c(-1,0,1), "SUS-RES"=c(0,-1,1))
cat("\n===== contrasts from SHIPPED Viterbi labels (clock window, Sex-only z) =====\n")
print(as.data.frame(emmeans::contrast(emmeans::emmeans(m, ~Group|Sex), cv, adjust="none")) %>%
  transmute(Sex, contrast, est=round(estimate,3), SE=round(SE,3), p=signif(p.value,3)), row.names=FALSE)
cat("\n===== state-label composition check (shipped vs seed-7 refit) =====\n")
r7full <- P %>% filter(seed==7)
cat("  shipped n animals:", nrow(fn_ship), " refit n animals:", nrow(r7full), "\n")
write_csv(fn_ship %>% left_join(P %>% filter(seed==7) %>% transmute(AnimalNum, dwell_refit_seed7=mean_dwell_minutes), by="AnimalNum"),
          file.path(OUT,"first_night_dwell_shipped_vs_refit.csv"))
cat("wrote first_night_dwell_shipped_vs_refit.csv\n")
