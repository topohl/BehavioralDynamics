# ================================================================
# Shared runner for the stratified stages (21, 22 Active; 24, 25 Inactive)
# MMMSociability
# ================================================================
# Wraps mmm_run_stratified_gamm() and adds everything the revision pass needs
# on top of it: exact AUC per stratum, response-scale simulated AUC, planned
# adaptation contrasts, the global-24 family plus its two labelled localization
# corrections, cross-sex moderation, the parsimonious-vs-full shape comparison,
# and the manuscript figure set.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(ggplot2); library(mgcv)
})

for (.h in c("stratified_gamm_driver.R", "gamm_auc_helpers.R",
             "gamm_diagnostics_helpers.R", "mmm_publication_theme.R")) {
  .probe <- switch(.h,
    "stratified_gamm_driver.R" = "mmm_run_stratified_gamm",
    "gamm_auc_helpers.R" = "mmm_auc_cvec",
    "gamm_diagnostics_helpers.R" = "mmm_traj_ci_width",
    "mmm_publication_theme.R" = "theme_mmm_pub")
  if (!exists(.probe, inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) source_mmm_helper(.h)
    else stop("stratified_stage_runner.R requires ", .h, call. = FALSE)
  }
}
rm(.h, .probe)

#' Everything downstream of the stratified model fits.
#'
#' @param res Output of mmm_run_stratified_gamm().
#' @param stratum_col "ActiveNight" or "CageChange" (labels only).
#' @param global_family_id Identifier for the conservative 24-test family.
mmm_stratified_postprocess <- function(res, stratum_col, global_family_id, window_hours = 12,
                                       stage_id = "stage", phase_label = "Active") {
  prim_fits <- res$fits[vapply(res$fits, function(f) f$variant == "primary", logical(1))]

  # ---- exact AUC per group x stratum, and response-scale AUC
  group_auc <- purrr::map_dfr(prim_fits, function(f) {
    m <- f$fit$model
    purrr::map_dfr(f$strata, function(s) {
      purrr::map_dfr(f$groups, function(g) {
        mmm_gamm_group_auc(m, f$grid_fun(g, s), f$batch_weights, f$animal_ref,
                           "TimeAxis", window_hours) %>%
          mutate(Sex = f$Sex, stratum = s, Group = g, .before = 1)
      })
    })
  })

  resp_auc <- purrr::map_dfr(prim_fits, function(f) {
    m <- f$fit$model
    purrr::map_dfr(f$strata, function(s) {
      grids <- setNames(lapply(f$groups, function(g) f$grid_fun(g, s)), f$groups)
      sim <- mmm_gamm_response_auc_sim(m, grids, f$batch_weights, f$animal_ref, "TimeAxis",
                                       window_hours = window_hours)
      sim %>% rename(Group = "level_name") %>% mutate(Sex = f$Sex, stratum = s, .before = 1)
    })
  })

  auc_audit <- purrr::map_dfr(prim_fits, function(f) {
    m <- f$fit$model
    purrr::map_dfr(f$strata, function(s) {
      purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
        mmm_auc_vs_window_average(m, f$grid_fun(pp[1], s), f$grid_fun(pp[2], s),
                                  f$batch_weights, f$animal_ref, "TimeAxis", window_hours) %>%
          mutate(Sex = f$Sex, stratum = s, contrast = paste0(pp[1], "-", pp[2]), .before = 1)
      })
    })
  })

  # ---- planned adaptation contrasts: last stratum vs first
  adaptation_raw <- purrr::map_dfr(prim_fits, function(f) {
    first_s <- f$strata[1]; last_s <- f$strata[length(f$strata)]
    mmm_adaptation_auc_contrasts(
      f$fit$model, grid_fun = function(g, s) f$grid_fun(g, s), groups = f$groups,
      first_stratum = first_s, last_stratum = last_s,
      batch_weights = f$batch_weights, animal_ref = f$animal_ref,
      time_col = "TimeAxis", window_hours = window_hours) %>%
      mutate(Sex = f$Sex, stratum_variable = stratum_col,
             first_stratum = first_s, last_stratum = last_s, .before = 1)
  })
  # Three prespecified biologically distinct BH families (+ all-14 sensitivity).
  adaptation <- mmm_adaptation_families(adaptation_raw, stage_id = stage_id,
                                        phase_label = phase_label)

  # ---- batch-marginalization sensitivity: equal (primary) vs count
  batch_weighting <- purrr::map_dfr(prim_fits, function(f) {
    m <- f$fit$model
    purrr::map_dfr(f$strata, function(s) {
      purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
        eq <- mmm_gamm_auc_contrast(m, f$grid_fun(pp[1], s), f$grid_fun(pp[2], s),
                                    f$batch_weights, f$animal_ref, "TimeAxis", window_hours)
        ct <- mmm_gamm_auc_contrast(m, f$grid_fun(pp[1], s), f$grid_fun(pp[2], s),
                                    f$batch_weights_count, f$animal_ref, "TimeAxis", window_hours)
        tibble(Sex = f$Sex, stratum = s, contrast = paste0(pp[1], "-", pp[2]),
               AUC_diff_equal = eq$AUC_diff_log1p, AUC_diff_count = ct$AUC_diff_log1p,
               abs_gap = abs(eq$AUC_diff_log1p - ct$AUC_diff_log1p),
               SE_equal = eq$AUC_diff_SE, SE_count = ct$AUC_diff_SE,
               p_equal = eq$AUC_diff_p, p_count = ct$AUC_diff_p,
               primary_weighting = f$batch_weighting)
      })
    })
  }) %>% mutate(contrast_invariant_to_weighting = .data$abs_gap < 1e-8)

  # ---- multiplicity: conservative global family + labelled localizations
  localization <- res$contrasts %>%
    filter(.data$variant == "primary") %>%
    mmm_multiplicity_localization(global_family_id)

  # ---- omnibus adaptation tests (separate family)
  omnibus <- mmm_interaction_tests(res$parametric %>% filter(.data$variant == "primary"),
                                   stratum_col) %>%
    group_by(.data$term) %>%
    group_modify(~ mmm_bh_family(.x, paste0(global_family_id, "__omnibus__", .y$term))) %>%
    ungroup()

  # ---- cross-sex moderation on the adaptation contrasts and per-stratum AUC
  sex_mod_adapt <- mmm_sex_moderation(
    adaptation %>% dplyr::select("Sex", "scope", "contrast_name", "estimate", "se"),
    by_cols = c("scope", "contrast_name")) %>%
    mutate(quantity = paste0("adaptation_AUC_log1p_", stratum_col), .before = 1)

  sex_mod_local <- mmm_sex_moderation(
    res$contrasts %>% filter(.data$variant == "primary") %>%
      dplyr::select("Sex", "stratum", "contrast",
                    estimate = "AUC_diff_log1p", se = "AUC_diff_SE"),
    by_cols = c("stratum", "contrast")) %>%
    mutate(quantity = "per_stratum_AUC_diff_log1p", .before = 1)

  list(group_auc = group_auc, resp_auc = resp_auc, auc_audit = auc_audit,
       adaptation = adaptation, localization = localization, omnibus = omnibus,
       batch_weighting = batch_weighting,
       sex_moderation = bind_rows(sex_mod_adapt, sex_mod_local),
       shape_comparison = mmm_shape_model_comparison(res$spec) %>%
         mutate(primary_model = "parsimonious additive shape decomposition",
                secondary_model = "SECONDARY_SHAPE_INTERACTION_SENSITIVITY",
                selection_basis = "estimand, parsimony, identifiability and cross-model stability; NOT significance"))
}

#' Manuscript figure set for a stratified stage.
mmm_stratified_figures <- function(res, post, dirs, stratum_col, x_lab, stratum_prefix,
                                   file_prefix, window_hours = 12,
                                   traj_file = paste0(file_prefix, "_trajectories.svg")) {
  traj <- res$trajectory %>% filter(.data$variant == "primary") %>%
    mutate(Group = factor(.data$Group, levels = MMM_GROUP_LEVELS),
           panel = paste(stratum_prefix, .data$stratum))
  n_str <- dplyr::n_distinct(traj$stratum)

  p_traj <- ggplot(traj, aes(TimeAxis, fit, colour = Group, fill = Group)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA) +
    geom_line(linewidth = 0.7) +
    facet_grid(Sex ~ panel) +
    mmm_scale_colour_group() + mmm_scale_fill_group() +
    scale_x_continuous(breaks = seq(0, window_hours, 4), limits = c(0, window_hours),
                       expand = c(0.01, 0)) +
    mmm_scale_y_log1p() +
    labs(x = x_lab) +
    theme_mmm_pub()
  f1 <- mmm_save_pub(p_traj, file.path(dirs$figure_root, traj_file),
                     45 * n_str + 20, 80)

  adapt_pheno <- post$adaptation %>% filter(.data$scope == "phenotype_dependent")
  adapt_group <- post$adaptation %>% filter(.data$scope %in% c("overall_population", MMM_GROUP_LEVELS))

  p_adapt <- adapt_group %>%
    mutate(scope = factor(.data$scope, levels = c("overall_population", MMM_GROUP_LEVELS))) %>%
    ggplot(aes(estimate, scope)) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey55") +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0, linewidth = 0.4) +
    geom_point(size = 1.5) +
    facet_wrap(~ Sex) +
    labs(x = paste0(unique(adapt_group$contrast_name)[1],
                    " difference in AUC, log1p(Movement) x h"), y = NULL) +
    theme_mmm_pub()
  f2 <- mmm_save_pub(p_adapt, file.path(dirs$figure_root, paste0(file_prefix, "_adaptation_auc.svg")),
                     130, 55)

  p_contr <- post$localization %>%
    mutate(contrast = factor(.data$contrast, levels = names(MMM_CONTRAST_COLOURS)),
           panel = paste(stratum_prefix, .data$stratum)) %>%
    ggplot(aes(AUC_diff_log1p, contrast, colour = contrast)) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey55") +
    geom_errorbarh(aes(xmin = AUC_diff_CI_low, xmax = AUC_diff_CI_high),
                   height = 0, linewidth = 0.4) +
    geom_point(size = 1.4) +
    facet_grid(Sex ~ panel) +
    mmm_scale_colour_contrast(guide = "none") +
    labs(x = "AUC difference, log1p(Movement) x h", y = NULL) +
    theme_mmm_pub()
  f3 <- mmm_save_pub(p_contr, file.path(dirs$figure_root, paste0(file_prefix, "_auc_contrasts.svg")),
                     45 * n_str + 20, 70)

  bind_rows(f1, f2, f3)
}

#' Write the standard stratified table set.
mmm_stratified_write <- function(res, post, dirs, prefix) {
  w <- function(x, f, d = dirs$tables) { readr::write_csv(x, file.path(d, f)); invisible(NULL) }
  w(post$group_auc, paste0(prefix, "_gamm_group_auc.csv"))
  w(post$localization, paste0(prefix, "_gamm_auc_contrasts.csv"))
  w(post$localization, paste0(prefix, "_multiplicity_localization.csv"))
  w(post$resp_auc, paste0(prefix, "_response_scale_group_auc.csv"))
  w(post$auc_audit, paste0(prefix, "_auc_vs_window_average_audit.csv"))
  w(post$adaptation, paste0(prefix, "_adaptation_auc_contrasts.csv"))
  w(post$batch_weighting, paste0(prefix, "_batch_weighting_diagnostic.csv"))
  # Compact multiplicity registry: one row per planned adaptation test.
  w(post$adaptation %>%
      dplyr::select("family_id", "family_key", "biological_question", "phase", "Sex",
                    "scope", "contrast_name", "tests_in_family", "estimate", "se",
                    "ci_low", "ci_high", "p_raw", "q_BH_family",
                    "q_BH_all14_sensitivity", "all14_role", "family_provenance"),
    paste0(prefix, "_adaptation_multiplicity_registry.csv"))
  w(post$sex_moderation, paste0(prefix, "_sex_moderation_contrasts.csv"))
  w(post$omnibus, paste0(prefix, "_omnibus_tests.csv"))
  w(post$shape_comparison, paste0(prefix, "_shape_model_comparison.csv"))
  w(res$trajectory, paste0(prefix, "_trajectory_predictions.csv"))
  w(res$pointwise, paste0(prefix, "_pairwise_trajectory_differences.csv"))
  w(res$spec, paste0(prefix, "_model_specification.csv"))
  w(res$parametric, paste0(prefix, "_parametric_terms.csv"))
  w(res$smooth, paste0(prefix, "_smooth_terms.csv"))
  w(res$stratum_avg, paste0(prefix, "_period_averaged_contrasts.csv"))
  w(res$diagnostics, paste0(prefix, "_model_diagnostics.csv"), dirs$audit)
  w(res$k_check, paste0(prefix, "_k_check.csv"), dirs$audit)
  w(res$acf, paste0(prefix, "_residual_acf.csv"), dirs$audit)
  w(res$ar1, paste0(prefix, "_ar1_sequence_proof.csv"), dirs$audit)

  for (nm in c("no_batch", "raw_scale", "k8", "full_shape")) {
    cmp <- mmm_sensitivity_compare(
      res$contrasts %>% filter(.data$variant == "primary"),
      res$contrasts %>% filter(.data$variant == nm),
      by_cols = c("Sex", "stratum", "contrast"), label = paste0("primary_vs_", nm))
    w(cmp, paste0(prefix, "_sensitivity_", nm, ".csv"))
  }
}
