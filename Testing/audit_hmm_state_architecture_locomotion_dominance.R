## Are the TEMPORAL HMM metrics themselves locomotion-dominated, or do they carry
## information the movement mean does not? Uses the repo's own 0.70 |rho| dominance threshold.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(lmerTest); library(emmeans)})
OUT <- getOption("mmm.audit_out", "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture")
B <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based"

met <- read_csv(file.path(OUT, "hmm_architecture_temporal_epoch_metrics.csv"),
                col_types = cols(AnimalNum = col_character(), .default = col_guess()))
mov <- read_csv(file.path(B, "tables/systems_sis_domain_scores.csv"),
                col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  filter(Domain == "Psychomotor activation") %>%
  select(AnimalNum, CageChangeIndex, PhaseClass, mov = DomainScore) %>% distinct()
arch <- read_csv(file.path(B, "tables/systems_sis_domain_scores.csv"),
                 col_types = cols(AnimalNum = col_character(), .default = col_guess())) %>%
  filter(Domain == "Behavioral state architecture") %>%
  select(AnimalNum, CageChangeIndex, PhaseClass, arch = DomainScore) %>% distinct()

d <- met %>% left_join(mov, by = c("AnimalNum", "CageChangeIndex", "PhaseClass")) %>%
  left_join(arch, by = c("AnimalNum", "CageChangeIndex", "PhaseClass"))

M <- c("occupancy_entropy", "state_switch_rate", "self_transition_probability",
       "transition_entropy", "mean_dwell_bins")
cat("Spearman rho with Movement_mean_z (repo dominance threshold |rho| >= 0.70)\n")
cat("and with the shipped composite, per resolution x phase. Epoch level (n) and animal level.\n")
res <- list()
for (rs in unique(d$resolution)) for (ph in c("Active", "Inactive")) {
  s <- d %>% filter(resolution == rs, PhaseClass == ph)
  al <- s %>% group_by(AnimalNum) %>% summarise(across(all_of(c(M, "mov", "arch")), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  cat("\n#### ", rs, " / ", ph, "  (epochs n=", nrow(s), ", animals n=", nrow(al), ")\n", sep = "")
  for (v in M) {
    r_ep <- suppressWarnings(cor(s[[v]], s$mov, method = "spearman", use = "complete.obs"))
    r_al <- suppressWarnings(cor(al[[v]], al$mov, method = "spearman", use = "complete.obs"))
    r_arch <- suppressWarnings(cor(s[[v]], s$arch, method = "spearman", use = "complete.obs"))
    cat(sprintf("  %-28s rho_mov epoch=%+.3f  animal=%+.3f %s | rho_composite=%+.3f\n",
                v, r_ep, r_al, ifelse(abs(r_al) >= 0.70, "<-DOMINATED", "           "), r_arch))
    res[[length(res) + 1]] <- tibble(resolution = rs, PhaseClass = ph, metric = v,
      rho_movement_epoch = r_ep, rho_movement_animal = r_al, rho_composite_epoch = r_arch,
      locomotion_dominance_flag = abs(r_al) >= 0.70)
  }
}
write_csv(bind_rows(res), file.path(OUT, "hmm_architecture_locomotion_dominance_by_metric.csv"))

## Movement-adjusted contrasts for the two most informative temporal metrics
cat("\n=============== MOVEMENT-ADJUSTED TEMPORAL CONTRASTS (10min primary, Female) ===============\n")
cat("Sensitivity only; SEs are not comparable pre/post when the covariate is highly collinear.\n")
for (ph in c("Active", "Inactive")) for (v in c("self_transition_probability", "transition_entropy", "mean_dwell_bins")) {
  s <- d %>% filter(resolution == "10min_based", PhaseClass == ph, is.finite(.data[[v]]), is.finite(mov)) %>%
    mutate(y = as.numeric(scale(.data[[v]])), AnimalNum = factor(AnimalNum),
           Group = factor(Group, levels = c("CON", "RES", "SUS")),
           Sex = factor(Sex, levels = c("Female", "Male")), CCf = factor(CageChangeIndex))
  cv <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))
  g0 <- as.data.frame(contrast(emmeans(lmer(y ~ Group * Sex + CCf + (1 | AnimalNum), data = s), ~ Group | Sex), cv, adjust = "none"))
  g1 <- as.data.frame(contrast(emmeans(lmer(y ~ Group * Sex + mov + CCf + (1 | AnimalNum), data = s), ~ Group | Sex), cv, adjust = "none"))
  j <- g0 %>% filter(Sex == "Female") %>% transmute(contrast, unadj = round(estimate, 3), p_unadj = signif(p.value, 3)) %>%
    left_join(g1 %>% filter(Sex == "Female") %>% transmute(contrast, movadj = round(estimate, 3), p_movadj = signif(p.value, 3)), by = "contrast") %>%
    mutate(pct_atten = ifelse(abs(unadj) > 0.15, round(100 * (1 - abs(movadj) / abs(unadj)), 0), NA))
  cat("\n ", ph, "/", v, "\n"); print(as.data.frame(j), row.names = FALSE)
}
