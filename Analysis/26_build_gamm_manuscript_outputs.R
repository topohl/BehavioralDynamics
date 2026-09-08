# ================================================================
# GAMM manuscript assembly - NO MODEL FITTING
# MMMSociability
# ================================================================
# Reads the frozen canonical outputs of Stages 20-25 and produces the
# publication tree: main-candidate figures, Extended Data figures, human
# readable tables, per-panel Source Data, legends, provenance manifests and a
# claim-to-analysis trace.
#
# HARD CONTRACT, asserted in this script:
#   * no statistical model is fitted here
#   * no p-value or q-value is recomputed; every one is carried through
#   * no multiplicity family is redefined
#   * no scientific number is changed
#   * no invalidated Gaussian Inactive table is ever read
# Filenames are semantic; manuscript figure numbers are assigned later.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
  library(ggplot2); library(stringr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("mmm_publication_theme.R")
source_mmm_helper("gamm_publication_style_helpers.R")
source_mmm_helper("gamm_manuscript_docs.R")

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
bin_level <- "10min_based"
P <- file.path(project_root, "analysis_ready", "pipeline")

out <- behavior_stage_dir(project_root, "26", "gamm_manuscript_outputs", bin_level)
dirs <- list(
  root = out,
  fig_main = file.path(out, "figures", "main_candidates"),
  fig_ed = file.path(out, "figures", "extended_data"),
  fig_diag = file.path(out, "figures", "diagnostics"),
  tab_ms = file.path(out, "tables", "manuscript"),
  tab_ed = file.path(out, "tables", "extended_data"),
  src = file.path(out, "source_data"),
  leg = file.path(out, "legends"),
  audit = file.path(out, "audit"),
  man = file.path(out, "manifests"))
for (d in dirs) ensure_dir(d)

cat("Stage 26 | GAMM manuscript assembly (no fitting) |", bin_level, "\n")

# ------------------------------------------------------------------ read frozen
stage_dir <- function(id, nm) file.path(P, paste0(id, "_", nm), "10min")
rd <- function(id, nm, sub, f) {
  p <- file.path(stage_dir(id, nm), sub, f)
  if (!file.exists(p)) stop("Missing frozen input: ", p, call. = FALSE)
  if (grepl("gaussian_log1p_invalid", p, fixed = TRUE))
    stop("Refusing to read an invalidated table: ", p, call. = FALSE)
  x <- read_csv(p, show_col_types = FALSE, progress = FALSE)
  if ("inference_status" %in% names(x) &&
      any(x$inference_status == "MODEL_INADEQUATE__DO_NOT_INTERPRET", na.rm = TRUE))
    stop("Refusing to use a MODEL_INADEQUATE table: ", p, call. = FALSE)
  x
}
N20 <- "first_night_gamm"; N21 <- "cc1_active_longitudinal_gamm"
N22 <- "repeated_cagechange_acute_gamm"; N23 <- "first_inactive_gamm"
N24 <- "cc1_inactive_longitudinal_gamm"; N25 <- "repeated_cagechange_inactive_gamm"

a20_traj <- rd("20", N20, "tables", "first_active_trajectory_predictions.csv")
a20_pc   <- rd("20", N20, "tables", "first_active_primary_contrasts.csv")
a20_ciw  <- rd("20", N20, "tables", "first_active_ci_width_diagnostics.csv")
a21_traj <- rd("21", N21, "tables", "cc1_active_trajectory_predictions.csv")
a22_traj <- rd("22", N22, "tables", "allcc_active_trajectory_predictions.csv")
a22_gauc <- rd("22", N22, "tables", "allcc_active_gamm_group_auc.csv")
a22_adap <- rd("22", N22, "tables", "allcc_active_adaptation_multiplicity_registry.csv")
a22_loc  <- rd("22", N22, "tables", "allcc_active_multiplicity_localization.csv")
a22_pw   <- rd("22", N22, "tables", "allcc_active_pairwise_trajectory_differences.csv")
a22_aud  <- rd("22", N22, "audit",  "cc1_cross_model_prediction_audit.csv")
i23_traj <- rd("23", N23, "tables", "first_inactive_markov_probability_trajectory.csv")
i23_ct   <- rd("23", N23, "tables", "first_inactive_markov_auc_contrasts.csv")
i23_gate <- rd("23", N23, "audit",  "first_inactive_markov_adequacy_gate.csv")
i23_cal  <- rd("23", N23, "audit",  "first_inactive_markov_decile_calibration.csv")
i24_traj <- rd("24", N24, "tables", "cc1_inactive_markov_probability_trajectory.csv")
i24_adap <- rd("24", N24, "tables", "cc1_inactive_markov_adaptation_multiplicity_registry.csv")
i25_traj <- rd("25", N25, "tables", "allcc_inactive_markov_probability_trajectory.csv")
i25_adap <- rd("25", N25, "tables", "allcc_inactive_markov_adaptation_multiplicity_registry.csv")

gate_ok <- all(i23_gate$passed)
N_ANIMALS <- tibble(Sex = c("Female", "Male"), n_animals = c(58L, 53L))

figman <- list(); srcman <- list()
reg <- function(fm, panel, src_file, src_table, model_id, script) {
  figman[[length(figman) + 1L]] <<- fm %>% mutate(panel_id = panel, model_id = model_id,
                                                  script = script)
  srcman[[length(srcman) + 1L]] <<- tibble(
    figure_file = paste0(fm$stem, ".pdf"), panel = panel, source_data_file = src_file,
    source_analysis_table = src_table, model_id = model_id, script = script)
}
wsrc <- function(x, f) { write_csv(x, file.path(dirs$src, f)); f }

# ============================================================ MAIN CANDIDATES
cat("  main candidates ...\n")

trA <- a20_traj %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
         resp = expm1(.data$fit), resp_lo = expm1(.data$lower), resp_hi = expm1(.data$upper))
pA <- mmm_traj_panel(trA, "TimeAxis", "resp", "resp_lo", "resp_hi",
                     MMM_X_LAB_ACTIVE, "Movement (RFID transitions per 10 min)")
fm <- mmm_export_figure(pA, dirs$fig_main, "first_active_trajectory_main", MMM_WIDTH_DOUBLE_MM, 70)
reg(fm, "A", wsrc(mmm_source_data(trA %>% left_join(N_ANIMALS, by = "Sex"), "A", "stage20",
      "stage20_first_active_primary",
      c("Sex","Group","TimeAxis","resp","resp_lo","resp_hi","fit","lower","upper","n_animals")),
      "source_first_active_trajectory.csv"),
    "20/tables/first_active_trajectory_predictions.csv", "stage20_first_active_primary",
    "20_first_night_gamm.R")

ctB <- a20_pc %>% mutate(contrast = factor(.data$contrast,
                                           levels = c("RES-CON", "SUS-CON", "SUS-RES")))
pB <- mmm_forest_panel(ctB, "AUC_diff_log1p", "contrast", "AUC_diff_CI_low", "AUC_diff_CI_high",
                       "Difference in AUC (log1p Movement x h)")
fm <- mmm_export_figure(pB, dirs$fig_main, "first_active_auc_contrasts_main",
                        MMM_WIDTH_MEDIUM_MM, 48)
reg(fm, "B", wsrc(mmm_source_data(ctB %>% left_join(N_ANIMALS, by = "Sex"), "B", "stage20",
      "stage20_first_active_primary",
      c("Sex","contrast","contrast_role","AUC_diff_log1p","AUC_diff_CI_low","AUC_diff_CI_high",
        "p_raw","p_bh","family_id","n_animals")), "source_first_active_auc_contrasts.csv"),
    "20/tables/first_active_primary_contrasts.csv", "stage20_first_active_primary",
    "20_first_night_gamm.R")

gaC <- a22_gauc %>% mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
                           cc = as.integer(str_extract(.data$stratum, "[0-9]+")))
pC <- ggplot(gaC, aes(cc, .data$AUC_log1p, colour = .data$Group, fill = .data$Group,
                      linetype = .data$Group, shape = .data$Group)) +
  geom_errorbar(aes(ymin = .data$AUC_CI_low, ymax = .data$AUC_CI_high),
                width = 0.10, linewidth = 0.35) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.7, stroke = 0.35, colour = "grey20") +
  facet_wrap(~ Sex) +
  mmm_scale_colour_group() + mmm_scale_fill_group() +
  mmm_scale_linetype_group() + mmm_scale_shape_group() +
  scale_x_continuous(breaks = sort(unique(gaC$cc)), labels = sort(unique(gaC$stratum))) +
  labs(x = "Cage change", y = "Acute-response AUC (log1p Movement x h)") +
  theme_mmm_pub()
fm <- mmm_export_figure(pC, dirs$fig_main, "active_auc_by_cagechange_main",
                        MMM_WIDTH_DOUBLE_MM, 70)
reg(fm, "C", wsrc(mmm_source_data(gaC %>% left_join(N_ANIMALS, by = "Sex"), "C", "stage22",
      "stage22_allcc_active_primary",
      c("Sex","Group","stratum","AUC_log1p","AUC_CI_low","AUC_CI_high","AUC_per_hour","n_animals")),
      "source_active_auc_by_cagechange.csv"),
    "22/tables/allcc_active_gamm_group_auc.csv", "stage22_allcc_active_primary",
    "22_repeated_cagechange_acute_gamm.R")

adD <- a22_adap %>% filter(.data$family_key %in% c("A_OVERALL", "B_GROUP_SPECIFIC")) %>%
  mutate(row_lab = factor(ifelse(.data$scope == "overall_population", "Overall", .data$scope),
                          levels = rev(c("Overall", "CON", "RES", "SUS"))))
pD <- mmm_forest_panel(adD, "estimate", "row_lab", "ci_low", "ci_high",
                       "CC4 - CC1 difference in AUC (log1p Movement x h)")
fm <- mmm_export_figure(pD, dirs$fig_main, "active_cc4_minus_cc1_main", MMM_WIDTH_MEDIUM_MM, 52)
reg(fm, "D", wsrc(mmm_source_data(adD %>% left_join(N_ANIMALS, by = "Sex"), "D", "stage22",
      "stage22_allcc_active_primary",
      c("Sex","family_key","scope","contrast_name","estimate","ci_low","ci_high","p_raw",
        "q_BH_family","q_BH_all14_sensitivity","n_animals")), "source_active_cc4_minus_cc1.csv"),
    "22/tables/allcc_active_adaptation_multiplicity_registry.csv",
    "stage22_allcc_active_primary", "22_repeated_cagechange_acute_gamm.R")

if (gate_ok) {
  trE <- i23_traj %>% mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
    pct = 100 * .data$p, pct_lo = 100 * .data$p_low, pct_hi = 100 * .data$p_high)
  pE <- mmm_traj_panel(trE, "TimeHours", "pct", "pct_lo", "pct_hi",
                       MMM_X_LAB_INACTIVE, "Active 10-min bins (%)")
  fm <- mmm_export_figure(pE, dirs$fig_main, "first_inactive_probability_main",
                          MMM_WIDTH_DOUBLE_MM, 70)
  reg(fm, "E", wsrc(mmm_source_data(trE %>% left_join(N_ANIMALS, by = "Sex"), "E", "stage23",
        "stage23_first_inactive_markov",
        c("Sex","Group","TimeHours","pct","pct_lo","pct_hi","p","p_low","p_high","n_animals")),
        "source_first_inactive_probability.csv"),
      "23/tables/first_inactive_markov_probability_trajectory.csv",
      "stage23_first_inactive_markov", "23_first_inactive_gamm.R")

  ctF <- i23_ct %>% mutate(contrast = factor(.data$contrast,
                                             levels = c("RES-CON", "SUS-CON", "SUS-RES")))
  pF <- mmm_forest_panel(ctF, "diff_active_bin_percent_points", "contrast",
                         "diff_pct_pts_ci_low", "diff_pct_pts_ci_high",
                         "Difference in active 10-min bins (percentage points)")
  fm <- mmm_export_figure(pF, dirs$fig_main, "first_inactive_auc_contrasts_main",
                          MMM_WIDTH_MEDIUM_MM, 48)
  reg(fm, "F", wsrc(mmm_source_data(ctF %>% left_join(N_ANIMALS, by = "Sex"), "F", "stage23",
        "stage23_first_inactive_markov",
        c("Sex","contrast","contrast_role","diff_active_bin_percent_points",
          "diff_pct_pts_ci_low","diff_pct_pts_ci_high","AUC_diff_p","p_bh","n_animals")),
        "source_inactive_auc_contrasts.csv"),
      "23/tables/first_inactive_markov_auc_contrasts.csv",
      "stage23_first_inactive_markov", "23_first_inactive_gamm.R")
} else {
  cat("  NOTE: Inactive adequacy gate not passed; main candidates E and F skipped.\n")
}

# ============================================================ EXTENDED DATA
cat("  extended data ...\n")
ed <- function(p, stem, w, h, panel, src, srctab, model, script) {
  fm <- mmm_export_figure(p, dirs$fig_ed, stem, w, h)
  reg(fm, panel, src, srctab, model, script)
}

ed1 <- a21_traj %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
         panel = paste("Night", .data$stratum), resp = expm1(.data$fit),
         resp_lo = expm1(.data$lower), resp_hi = expm1(.data$upper))
ed(mmm_traj_panel(ed1, "TimeAxis", "resp", "resp_lo", "resp_hi", MMM_X_LAB_ACTIVE,
                  "Movement (RFID transitions per 10 min)", facet = NULL) +
     facet_grid(Sex ~ panel),
   "ed1_cc1_active_trajectories_by_night", MMM_WIDTH_DOUBLE_MM, 90, "ED1",
   wsrc(mmm_source_data(ed1, "ED1", "stage21", "stage21_cc1_active_primary",
     c("Sex","Group","stratum","TimeAxis","resp","resp_lo","resp_hi")),
     "source_ed1_cc1_active_by_night.csv"),
   "21/tables/cc1_active_trajectory_predictions.csv", "stage21_cc1_active_primary",
   "21_cc1_active_longitudinal_gamm.R")

ed2 <- a22_traj %>% filter(.data$variant == "primary") %>%
  mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS), resp = expm1(.data$fit),
         resp_lo = expm1(.data$lower), resp_hi = expm1(.data$upper))
ed(mmm_traj_panel(ed2, "TimeAxis", "resp", "resp_lo", "resp_hi", MMM_X_LAB_ACTIVE,
                  "Movement (RFID transitions per 10 min)", facet = NULL) +
     facet_grid(Sex ~ stratum),
   "ed2_allcc_active_trajectories", MMM_WIDTH_DOUBLE_MM, 90, "ED2",
   wsrc(mmm_source_data(ed2, "ED2", "stage22", "stage22_allcc_active_primary",
     c("Sex","Group","stratum","TimeAxis","resp","resp_lo","resp_hi")),
     "source_ed2_allcc_active.csv"),
   "22/tables/allcc_active_trajectory_predictions.csv", "stage22_allcc_active_primary",
   "22_repeated_cagechange_acute_gamm.R")

ed3 <- a22_pw %>% filter(.data$variant == "primary")
ed(ggplot(ed3, aes(.data$TimeAxis, .data$diff)) +
     geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
     geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.10,
                 fill = MMM_CONTRAST_COLOUR) +
     geom_line(linewidth = 0.6, colour = MMM_CONTRAST_COLOUR) +
     facet_grid(contrast + Sex ~ stratum) +
     labs(x = MMM_X_LAB_ACTIVE, y = "Predicted difference (log1p Movement)") + theme_mmm_pub(),
   "ed3_active_pairwise_difference_curves", MMM_WIDTH_DOUBLE_MM, 150, "ED3",
   wsrc(mmm_source_data(ed3, "ED3", "stage22", "stage22_allcc_active_primary",
     c("Sex","stratum","contrast","TimeAxis","diff","lower","upper")),
     "source_ed3_active_pairwise_differences.csv"),
   "22/tables/allcc_active_pairwise_trajectory_differences.csv",
   "stage22_allcc_active_primary", "22_repeated_cagechange_acute_gamm.R")

ed4 <- a22_aud %>% dplyr::select("Sex","Group","rmse_old","rmse_new") %>%
  pivot_longer(c("rmse_old","rmse_new"), names_to = "model", values_to = "rmse") %>%
  mutate(model = recode(.data$model, rmse_old = "Full shape-interaction",
                        rmse_new = "Parsimonious (primary)"),
         Group = factor(.data$Group, levels = MMM_GROUP_LEVELS))
ed(ggplot(ed4, aes(.data$Group, .data$rmse, shape = .data$Group, fill = .data$Group)) +
     geom_point(size = 1.9, stroke = 0.35, colour = "grey20") +
     facet_grid(Sex ~ model) + mmm_scale_shape_group() + mmm_scale_fill_group() +
     labs(x = NULL, y = "RMSE vs Stage 20 CC1 prediction") + theme_mmm_pub(),
   "ed4_stage20_vs_stage22_cc1_audit", MMM_WIDTH_MEDIUM_MM, 70, "ED4",
   wsrc(mmm_source_data(ed4, "ED4", "stage22", "stage20_vs_stage22",
     c("Sex","Group","model","rmse")), "source_ed4_cc1_cross_model_audit.csv"),
   "22/audit/cc1_cross_model_prediction_audit.csv", "stage20_vs_stage22",
   "22_repeated_cagechange_acute_gamm.R")

ed5 <- a20_ciw %>% mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS))
ed(ggplot(ed5, aes(.data$ratio_vs_primary, .data$variant, shape = .data$Group,
                   fill = .data$Group)) +
     geom_vline(xintercept = 1, linewidth = 0.3, colour = "grey60") +
     geom_point(size = 1.7, stroke = 0.35, colour = "grey20") +
     facet_wrap(~ Sex) + mmm_scale_shape_group() + mmm_scale_fill_group() +
     labs(x = "Mean CI width relative to the primary model", y = NULL) + theme_mmm_pub(),
   "ed5_active_ci_width_sensitivity", MMM_WIDTH_DOUBLE_MM, 65, "ED5",
   wsrc(mmm_source_data(ed5, "ED5", "stage20", "stage20_first_active_primary",
     c("Sex","Group","variant","mean_CI_width","ratio_vs_primary","note")),
     "source_ed5_ci_width.csv"),
   "20/tables/first_active_ci_width_diagnostics.csv", "stage20_first_active_primary",
   "20_first_night_gamm.R")

if (gate_ok) {
  ed6 <- i24_traj %>% mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
    panel = paste("Block", .data$stratum), pct = 100 * .data$p,
    pct_lo = 100 * .data$p_low, pct_hi = 100 * .data$p_high)
  ed(mmm_traj_panel(ed6, "TimeHours", "pct", "pct_lo", "pct_hi", MMM_X_LAB_INACTIVE,
                    "Active 10-min bins (%)", facet = NULL) + facet_grid(Sex ~ panel),
     "ed6_cc1_inactive_probability_by_block", MMM_WIDTH_DOUBLE_MM, 90, "ED6",
     wsrc(mmm_source_data(ed6, "ED6", "stage24", "stage24_cc1_inactive_markov",
       c("Sex","Group","stratum","TimeHours","pct","pct_lo","pct_hi")),
       "source_ed6_cc1_inactive_by_block.csv"),
     "24/tables/cc1_inactive_markov_probability_trajectory.csv",
     "stage24_cc1_inactive_markov", "24_cc1_inactive_longitudinal_gamm.R")

  ed7 <- i25_traj %>% mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
    pct = 100 * .data$p, pct_lo = 100 * .data$p_low, pct_hi = 100 * .data$p_high)
  ed(mmm_traj_panel(ed7, "TimeHours", "pct", "pct_lo", "pct_hi", MMM_X_LAB_INACTIVE,
                    "Active 10-min bins (%)", facet = NULL) + facet_grid(Sex ~ stratum),
     "ed7_allcc_inactive_probability", MMM_WIDTH_DOUBLE_MM, 90, "ED7",
     wsrc(mmm_source_data(ed7, "ED7", "stage25", "stage25_allcc_inactive_markov",
       c("Sex","Group","stratum","TimeHours","pct","pct_lo","pct_hi")),
       "source_ed7_allcc_inactive.csv"),
     "25/tables/allcc_inactive_markov_probability_trajectory.csv",
     "stage25_allcc_inactive_markov", "25_repeated_cagechange_inactive_gamm.R")

  ed8 <- bind_rows(
    i23_traj %>% transmute(Sex = .data$Sex, Group = .data$Group, TimeHours = .data$TimeHours,
      quantity = "Activation  P(active | previous bin inactive)",
      est = 100 * .data$p01, lo = 100 * .data$p01_low, hi = 100 * .data$p01_high),
    i23_traj %>% transmute(Sex = .data$Sex, Group = .data$Group, TimeHours = .data$TimeHours,
      quantity = "Persistence  P(active | previous bin active)",
      est = 100 * .data$p11, lo = 100 * .data$p11_low, hi = 100 * .data$p11_high)) %>%
    mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS))
  ed(mmm_traj_panel(ed8, "TimeHours", "est", "lo", "hi", MMM_X_LAB_INACTIVE,
                    "Probability (%)", facet = NULL) +
       facet_grid(quantity ~ Sex, scales = "free_y"),
     "ed8_inactive_transition_probabilities", MMM_WIDTH_DOUBLE_MM, 90, "ED8",
     wsrc(mmm_source_data(ed8, "ED8", "stage23", "stage23_first_inactive_markov",
       c("Sex","Group","TimeHours","quantity","est","lo","hi")),
       "source_ed8_inactive_transitions.csv"),
     "23/tables/first_inactive_markov_probability_trajectory.csv",
     "stage23_first_inactive_markov", "23_first_inactive_gamm.R")

  ed(ggplot(i23_cal, aes(.data$mean_predicted, .data$observed)) +
       geom_abline(slope = 1, intercept = 0, linewidth = 0.3, colour = "grey60") +
       geom_point(size = 1.4, colour = MMM_CONTRAST_COLOUR) + facet_wrap(~ Sex) +
       labs(x = "Mean predicted activity probability",
            y = "Observed activity probability") + theme_mmm_pub(),
     "ed9_inactive_markov_calibration", MMM_WIDTH_MEDIUM_MM, 60, "ED9",
     wsrc(mmm_source_data(i23_cal, "ED9", "stage23", "stage23_first_inactive_markov",
       c("Sex","bin","n","mean_predicted","observed","abs_gap")),
       "source_ed9_inactive_calibration.csv"),
     "23/audit/first_inactive_markov_decile_calibration.csv",
     "stage23_first_inactive_markov", "23_first_inactive_gamm.R")
}

# ============================================================ TABLES
cat("  tables ...\n")
act_key <- bind_rows(
  a20_pc %>% transmute(Analysis = "First Active response (CC1 night 1)", Sex = .data$Sex,
    comparison = .data$contrast, Estimate = .data$AUC_diff_log1p,
    Unit = "AUC, log1p(Movement) x h", lo = .data$AUC_diff_CI_low, hi = .data$AUC_diff_CI_high,
    raw_p = .data$p_raw, q = .data$p_bh, family = .data$family_id,
    role = .data$contrast_role),
  a22_adap %>% filter(.data$family_key == "A_OVERALL") %>%
    transmute(Analysis = "Repeated Active adaptation", Sex = .data$Sex,
      comparison = paste0("Overall ", .data$contrast_name), Estimate = .data$estimate,
      Unit = "AUC, log1p(Movement) x h", lo = .data$ci_low, hi = .data$ci_high,
      raw_p = .data$p_raw, q = .data$q_BH_family, family = .data$family_id,
      role = "OVERALL_ADAPTATION"),
  a22_adap %>% filter(.data$family_key == "B_GROUP_SPECIFIC") %>%
    transmute(Analysis = "Repeated Active adaptation", Sex = .data$Sex,
      comparison = paste0(.data$scope, " ", .data$contrast_name), Estimate = .data$estimate,
      Unit = "AUC, log1p(Movement) x h", lo = .data$ci_low, hi = .data$ci_high,
      raw_p = .data$p_raw, q = .data$q_BH_family, family = .data$family_id,
      role = "GROUP_SPECIFIC_LOCALIZATION"),
  a22_adap %>% filter(.data$family_key == "C_PHENOTYPE_DEPENDENT") %>%
    transmute(Analysis = "Repeated Active adaptation", Sex = .data$Sex,
      comparison = .data$contrast_name, Estimate = .data$estimate,
      Unit = "AUC, log1p(Movement) x h", lo = .data$ci_low, hi = .data$ci_high,
      raw_p = .data$p_raw, q = .data$q_BH_family, family = .data$family_id,
      role = "PHENOTYPE_DEPENDENT_ADAPTATION")) %>%
  left_join(N_ANIMALS, by = "Sex") %>%
  transmute(Analysis, Sex, `Biological comparison` = .data$comparison,
            Estimate = round(.data$Estimate, 2), Unit,
            `95% CI` = mmm_fmt_ci(.data$lo, .data$hi),
            `raw p` = mmm_fmt_p(.data$raw_p), `BH q` = mmm_fmt_p(.data$q),
            `Multiplicity family` = .data$family, `n animals` = .data$n_animals,
            `Interpretation role` = .data$role)
write_csv(act_key, file.path(dirs$tab_ms, "manuscript_active_key_results.csv"))

if (gate_ok) {
  inact_key <- i23_ct %>% left_join(N_ANIMALS, by = "Sex") %>%
    transmute(Analysis = "First Inactive activation (post-CC1)", Sex = .data$Sex,
      `Biological comparison` = .data$contrast,
      Estimate = round(.data$diff_active_bin_percent_points, 2),
      Unit = "percentage points of active 10-min bins",
      `95% CI` = mmm_fmt_ci(.data$diff_pct_pts_ci_low, .data$diff_pct_pts_ci_high),
      `raw p` = mmm_fmt_p(.data$AUC_diff_p), `BH q` = mmm_fmt_p(.data$p_bh),
      `Multiplicity family` = .data$family_id, `n animals` = .data$n_animals,
      `Interpretation role` = .data$contrast_role)
  write_csv(inact_key, file.path(dirs$tab_ms, "manuscript_inactive_key_results.csv"))
}

ed_full <- bind_rows(
  a20_pc %>% transmute(stage = "20", phase = "Active", Sex = .data$Sex, stratum = "CC1_night1",
    contrast = .data$contrast, effect = .data$AUC_diff_log1p, SE = .data$AUC_diff_SE,
    ci_low = .data$AUC_diff_CI_low, ci_high = .data$AUC_diff_CI_high, raw_p = .data$p_raw,
    family_q = .data$p_bh, conservative_q = NA_real_, unit = "AUC log1p x h",
    model_role = "PRIMARY", adequacy = "READY"),
  a22_loc %>% transmute(stage = "22", phase = "Active", Sex = .data$Sex,
    stratum = .data$stratum, contrast = .data$contrast, effect = .data$AUC_diff_log1p,
    SE = .data$AUC_diff_SE, ci_low = .data$AUC_diff_CI_low, ci_high = .data$AUC_diff_CI_high,
    raw_p = .data$p_raw, family_q = .data$q_BH_global24, conservative_q = NA_real_,
    unit = "AUC log1p x h", model_role = "PRIMARY", adequacy = "READY"),
  a22_adap %>% transmute(stage = "22", phase = "Active", Sex = .data$Sex,
    stratum = .data$contrast_name, contrast = .data$scope, effect = .data$estimate,
    SE = .data$se, ci_low = .data$ci_low, ci_high = .data$ci_high, raw_p = .data$p_raw,
    family_q = .data$q_BH_family, conservative_q = .data$q_BH_all14_sensitivity,
    unit = "AUC log1p x h", model_role = "PRIMARY", adequacy = "READY"),
  if (gate_ok) i23_ct %>% transmute(stage = "23", phase = "Inactive", Sex = .data$Sex,
    stratum = "CC1_inactive_block1", contrast = .data$contrast,
    effect = .data$diff_active_bin_percent_points, SE = NA_real_,
    ci_low = .data$diff_pct_pts_ci_low, ci_high = .data$diff_pct_pts_ci_high,
    raw_p = .data$AUC_diff_p, family_q = .data$p_bh, conservative_q = NA_real_,
    unit = "percentage points", model_role = "PRIMARY", adequacy = "READY") else NULL,
  if (gate_ok) i24_adap %>% transmute(stage = "24", phase = "Inactive", Sex = .data$Sex,
    stratum = .data$contrast_name, contrast = .data$scope, effect = .data$estimate,
    SE = .data$se, ci_low = .data$ci_low, ci_high = .data$ci_high, raw_p = .data$p_raw,
    family_q = .data$q_BH_family, conservative_q = .data$q_BH_all14_sensitivity,
    unit = "probability-hours", model_role = "PRIMARY", adequacy = "READY") else NULL,
  if (gate_ok) i25_adap %>% transmute(stage = "25", phase = "Inactive", Sex = .data$Sex,
    stratum = .data$contrast_name, contrast = .data$scope, effect = .data$estimate,
    SE = .data$se, ci_low = .data$ci_low, ci_high = .data$ci_high, raw_p = .data$p_raw,
    family_q = .data$q_BH_family, conservative_q = .data$q_BH_all14_sensitivity,
    unit = "probability-hours", model_role = "PRIMARY", adequacy = "READY") else NULL) %>%
  left_join(N_ANIMALS, by = "Sex")
write_csv(ed_full, file.path(dirs$tab_ed, "extended_data_gamm_full_results.csv"))

# ============================================================ MANIFESTS + TRACE
figure_manifest <- bind_rows(figman)
write_csv(figure_manifest, file.path(dirs$man, "figure_manifest.csv"))
write_csv(bind_rows(srcman), file.path(dirs$src, "source_data_manifest.csv"))

claims <- tribble(
  ~claim_id, ~manuscript_claim, ~analysis_stage, ~phase, ~estimand, ~model_id,
  ~statistical_test, ~multiplicity_family, ~primary_table, ~primary_figure,
  ~source_data_file, ~status,
  "CLAIM_GAMM_01", "The first acute locomotor response has a structured temporal trajectory.",
  "20", "Active", "population trajectory over the first 12 h Active block",
  "stage20_first_active_primary", "GAMM smooth terms (shared + group difference smooths)",
  "none (descriptive trajectory)", "first_active_smooth_terms.csv",
  "first_active_trajectory_main.pdf", "source_first_active_trajectory.csv", "SUPPORTED",
  "CLAIM_GAMM_02",
  "Later phenotype groups are not cleanly separated categorically during the first Active response.",
  "20", "Active", "pairwise AUC contrasts over the first Active block",
  "stage20_first_active_primary", "Wald on window-integrated AUC contrast",
  "PRIMARY_FIRST_ACTIVE__2Sex_x_3GroupContrasts", "first_active_primary_contrasts.csv",
  "first_active_auc_contrasts_main.pdf", "source_first_active_auc_contrasts.csv",
  "SUPPORTED - all six contrasts null",
  "CLAIM_GAMM_03",
  "Continuous first-night locomotor magnitude prospectively relates to later CombZ.",
  "09", "Active", "continuous prospective association", "stage09_early_prediction",
  "Stage 09 model ladder", "Stage 09 families", "Stage 09 tables",
  "not a Stage 20-26 figure", "not applicable",
  "OWNED BY STAGE 09 - explicitly NOT claimed by Stage 20",
  "CLAIM_GAMM_04", "Acute locomotor response increases from CC1 to CC4 in both sexes.",
  "22", "Active", "CC4 - CC1 difference in acute-response AUC",
  "stage22_allcc_active_primary", "Wald on the difference of integrated AUCs",
  "stage22_allcc__ACTIVE__ADAPTATION__A_OVERALL",
  "allcc_active_adaptation_multiplicity_registry.csv", "active_cc4_minus_cc1_main.pdf",
  "source_active_cc4_minus_cc1.csv", "SUPPORTED in both sexes",
  "CLAIM_GAMM_05",
  "Adaptation magnitude does not differ detectably among later phenotype groups.",
  "22", "Active", "difference-of-differences in AUC across cage changes",
  "stage22_allcc_active_primary", "Wald on difference-of-differences",
  "stage22_allcc__ACTIVE__ADAPTATION__C_PHENOTYPE_DEPENDENT",
  "allcc_active_adaptation_multiplicity_registry.csv", "active_cc4_minus_cc1_main.pdf",
  "source_active_cc4_minus_cc1.csv", "SUPPORTED - all six null",
  "CLAIM_GAMM_06",
  "Inactive-phase locomotor activation probability differs among phenotype groups in males.",
  "23", "Inactive", "activity-probability AUC contrasts over the first Inactive block",
  "stage23_first_inactive_markov", "delta-method Wald on the marginal-probability AUC contrast",
  "SECONDARY_FIRST_INACTIVE_MARKOV__2Sex_x_3GroupContrasts",
  "first_inactive_markov_auc_contrasts.csv", "first_inactive_auc_contrasts_main.pdf",
  "source_inactive_auc_contrasts.csv",
  if (gate_ok) "SUPPORTED in males for RES-CON and SUS-CON; primary phenotype contrast SUS-RES null"
  else "PENDING - Markov adequacy gate not passed")
write_csv(claims, file.path(dirs$audit, "gamm_claim_to_analysis_trace.csv"))

# ============================================================ CONTRACT CHECK
flat <- unlist(lapply(list(act_key, ed_full, claims), function(x) as.character(unlist(x))))
stopifnot(!any(grepl("MODEL_INADEQUATE", flat, fixed = TRUE)))
cat("  contract: no invalidated Gaussian value entered any manuscript artefact\n")

saveRDS(list(n_figures = nrow(figure_manifest), gate_ok = gate_ok,
             palette = MMM_GROUP_COLOURS),
        file.path(dirs$audit, "stage26_build_record.rds"))

cat("\nStage 26 complete ->", out, "\n")
cat("  figures:", nrow(figure_manifest), "| source data files:", length(srcman),
    "| Inactive gate:", gate_ok, "\n")

# ============================================================ LEGENDS + README
cat("  legends and README ...\n")
mmm_write_gamm_legends(file.path(dirs$leg, "gamm_figure_legends_draft.md"))
mmm_write_gamm_readme(file.path(out, "README.md"), MMM_GROUP_COLOURS, MMM_BASE_PT,
                      MMM_PANEL_LABEL_PT, MMM_WIDTH_SINGLE_MM, MMM_WIDTH_MEDIUM_MM,
                      MMM_WIDTH_DOUBLE_MM, MMM_MAX_HEIGHT_MM)
cat("  wrote legends and README\n")
