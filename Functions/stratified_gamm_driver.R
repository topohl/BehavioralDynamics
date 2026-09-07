# ================================================================
# Shared driver for the stratified Active/Inactive GAMM stages
# MMMSociability
# ================================================================
# Stages 21/22 (Active) and 24/25 (Inactive) all have the same shape: one
# phase-window dataset stratified by a repeated factor, fitted within Sex, with
# a formal Group x Stratum interaction.
#
# TWO SHAPE STRUCTURES ARE FITTED, both prespecified.
#
#   PRIMARY - additive shape decomposition (parsimonious)
#     resp ~ [Batch +] Group * Stratum
#            + s(Time, k)                      overall acute-response shape
#            + s(Time, by = GroupOrd, k)       stable phenotype shape difference
#            + s(Time, by = StratumOrd, k)     shape change across repeats,
#                                              shared across phenotypes
#            + s(AnimalNum, bs = "re")
#
#   SECONDARY - full shape interaction (the previous primary, retained)
#     ... + s(Time, by = ordered(Group x Stratum), k) ...
#     which allows a separate curve shape for all 3 x 4 = 12 cells per Sex.
#
# The primary answers "does the overall acute response change across repeats,
# and does that change differ by phenotype". The secondary answers the stronger
# and much more flexible "does exact within-window curve SHAPE depend jointly
# on phenotype and repeat". The secondary is kept as a declared sensitivity, not
# deleted, and the two are compared on AIC/edf and on CC1 predictions.
#
# All factor smooths use ORDERED factors, which give centred difference smooths
# and therefore stay identifiable beside the parametric interaction. An
# unordered `bs="sz"` interaction carries per-level constants and makes the
# parametric table rank-deficient (measured: Group 1 df instead of 2).
#
# Multiplicity is NOT applied here. Each stage declares its own families.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(mgcv)
})

for (.h in c("gamm_group_inference_helpers.R", "gamm_auc_helpers.R", "gamm_diagnostics_helpers.R")) {
  .probe <- switch(.h,
    "gamm_group_inference_helpers.R" = "mmm_fit_gamm_ar1",
    "gamm_auc_helpers.R" = "mmm_auc_cvec",
    "gamm_diagnostics_helpers.R" = "mmm_traj_ci_width")
  if (!exists(.probe, mode = "function", inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) source_mmm_helper(.h)
    else stop("stratified_gamm_driver.R requires ", .h, call. = FALSE)
  }
}
rm(.h, .probe)

# MMM_GROUP_PAIRS is defined in gamm_group_inference_helpers.R.

#' Prepare a stratified window dataset for modelling.
mmm_prepare_stratified <- function(dat, stratum_col, time_col) {
  stopifnot(all(c("AnimalNum", "Batch", "Group", "Sex", "Movement",
                  stratum_col, time_col) %in% names(dat)))
  dat %>%
    filter(!is.na(.data$Movement)) %>%
    mutate(
      y_log = log1p(.data$Movement),
      y_raw = .data$Movement,
      Group = factor(.data$Group, levels = c("CON", "RES", "SUS")),
      Batch = factor(.data$Batch),
      AnimalNum = factor(.data$AnimalNum),
      Stratum = factor(as.character(.data[[stratum_col]])),
      TimeAxis = as.numeric(.data[[time_col]])
    ) %>%
    mutate(
      GroupOrd = factor(.data$Group, levels = levels(.data$Group), ordered = TRUE),
      StratumOrd = factor(.data$Stratum, levels = levels(.data$Stratum), ordered = TRUE),
      Cross = mmm_ordered_cross(.data$Group, .data$Stratum)
    )
}

#' Model formula for either shape structure.
mmm_stratified_formula <- function(response, with_batch, shape = c("parsimonious", "full_interaction"),
                                   k = MMM_GAMM_K) {
  shape <- match.arg(shape)
  shape_terms <- if (shape == "parsimonious") {
    paste0(" + s(TimeAxis, by = GroupOrd, k = ", k, ", bs = 'tp')",
           " + s(TimeAxis, by = StratumOrd, k = ", k, ", bs = 'tp')")
  } else {
    paste0(" + s(TimeAxis, by = Cross, k = ", k, ", bs = 'tp')")
  }
  stats::as.formula(paste0(
    response, " ~ ", if (with_batch) "Batch + " else "", "Group * Stratum",
    " + s(TimeAxis, k = ", k, ", bs = 'tp')", shape_terms,
    " + s(AnimalNum, bs = 're')"
  ))
}

#' Prediction grid carrying every factor either shape structure needs.
mmm_stratified_grid <- function(d, group, stratum, window_hours = 12, grid_n = 100L) {
  tibble(TimeAxis = seq(0, window_hours, length.out = grid_n)) %>%
    mutate(
      Group = factor(group, levels = levels(d$Group)),
      Stratum = factor(stratum, levels = levels(d$Stratum)),
      GroupOrd = factor(group, levels = levels(d$GroupOrd), ordered = TRUE),
      StratumOrd = factor(stratum, levels = levels(d$StratumOrd), ordered = TRUE),
      Cross = factor(paste(group, stratum, sep = "."), levels = levels(d$Cross), ordered = TRUE)
    )
}

#' Run one stratified analysis across Sex, shape structure and model variant.
#'
#' @return list of tidy tibbles plus `fits`, a named list of
#'   list(fit, data, formula, batch_weights, animal_ref, strata, groups).
mmm_run_stratified_gamm <- function(dat, stratum_col, time_col, ar_block_cols,
                                    window_hours = 12, grid_n = 100L,
                                    k_sensitivity = 8L) {
  base <- mmm_prepare_stratified(dat, stratum_col, time_col)

  variants <- tibble::tribble(
    ~key,          ~response, ~with_batch, ~shape,             ~k,           ~role,
    "primary",     "y_log",   TRUE,        "parsimonious",     MMM_GAMM_K,   "PRIMARY",
    "no_batch",    "y_log",   FALSE,       "parsimonious",     MMM_GAMM_K,   "SENSITIVITY_batch",
    "raw_scale",   "y_raw",   TRUE,        "parsimonious",     MMM_GAMM_K,   "SENSITIVITY_response_scale",
    "k8",          "y_log",   TRUE,        "parsimonious",     k_sensitivity, "SENSITIVITY_k",
    "full_shape",  "y_log",   TRUE,        "full_interaction", MMM_GAMM_K,   "SECONDARY_SHAPE_INTERACTION_SENSITIVITY"
  )

  out <- list(contrasts = list(), stratum_avg = list(), trajectory = list(),
              pointwise = list(), spec = list(), parametric = list(),
              smooth = list(), ar1 = list(), diagnostics = list(),
              k_check = list(), acf = list())
  fits <- list()

  for (sx in sort(unique(as.character(base$Sex)))) {
    dsex <- base %>% filter(.data$Sex == sx)

    for (i in seq_len(nrow(variants))) {
      v <- variants[i, ]
      label <- paste0(stratum_col, "|", sx, "|", v$key)
      cat("  fitting ", label, " ...", sep = "")

      d <- mmm_order_for_ar1(dsex, block_cols = ar_block_cols) %>% droplevels()
      d$.ar_start <- mmm_build_ar_start(d, block_cols = ar_block_cols)
      form <- mmm_stratified_formula(v$response, v$with_batch, v$shape, v$k)
      fit <- mmm_fit_gamm_ar1(form, d)
      m <- fit$model
      cat(" rho=", round(fit$rho, 3), " ar1=", fit$ar1_applied,
          " edf=", round(sum(m$edf), 1), "\n", sep = "")

      # Manuscript-facing predictions marginalize EQUALLY over batches; the
      # count-weighted version is retained as a sensitivity (see the stage
      # runner's batch_weighting_diagnostic output).
      batch_w <- mmm_batch_weights(d, MMM_BATCH_WEIGHTING)
      batch_w_count <- mmm_batch_weights(d, "count")
      animal_ref <- levels(d$AnimalNum)[1]
      strata <- levels(d$Stratum)
      groups <- levels(d$Group)
      gr <- function(g, s) mmm_stratified_grid(d, g, s, window_hours, grid_n)
      tag <- function(x) x %>% mutate(Sex = sx, variant = v$key, shape = v$shape,
                                      model_label = label, .before = 1)

      fits[[label]] <- list(fit = fit, data = d, formula = form, batch_weights = batch_w,
                            batch_weights_count = batch_w_count,
                            batch_weighting = MMM_BATCH_WEIGHTING,
                            animal_ref = animal_ref, strata = strata, groups = groups,
                            grid_fun = gr, Sex = sx, variant = v$key, shape = v$shape,
                            window_hours = window_hours)

      # per-stratum planned contrasts, exact AUC-based
      out$contrasts[[label]] <- purrr::map_dfr(strata, function(s) {
        purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
          avg <- mmm_gamm_avg_contrast_grids(m, gr(pp[1], s), gr(pp[2], s), batch_w, animal_ref)
          auc <- mmm_gamm_auc_contrast(m, gr(pp[1], s), gr(pp[2], s), batch_w, animal_ref,
                                       "TimeAxis", window_hours)
          bind_cols(
            tibble(stratum = s, contrast = paste0(pp[1], "-", pp[2]),
                   group_comp = pp[1], group_ref = pp[2]),
            avg %>% dplyr::select("estimate", "se", "ci_low", "ci_high", "statistic", "p_raw"),
            auc %>% dplyr::select("AUC_diff_log1p", "AUC_diff_SE", "AUC_diff_CI_low",
                                  "AUC_diff_CI_high", "AUC_diff_p", "AUC_diff_per_hour")
          )
        })
      }) %>% tag()

      out$stratum_avg[[label]] <- purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
        g1 <- purrr::map_dfr(strata, ~ gr(pp[1], .x))
        g0 <- purrr::map_dfr(strata, ~ gr(pp[2], .x))
        mmm_gamm_avg_contrast_grids(m, g1, g0, batch_w, animal_ref) %>%
          mutate(contrast = paste0(pp[1], "-", pp[2]),
                 group_comp = pp[1], group_ref = pp[2], .before = 1)
      }) %>% tag()

      out$trajectory[[label]] <- purrr::map_dfr(strata, function(s) {
        purrr::map_dfr(groups, function(g) {
          mmm_gamm_trajectory(m, gr(g, s), batch_w, animal_ref) %>%
            mutate(stratum = s, Group = g, .before = 1)
        })
      }) %>% tag()

      out$pointwise[[label]] <- purrr::map_dfr(strata, function(s) {
        purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
          X1 <- mmm_design_rows(m, gr(pp[1], s), batch_w, animal_ref)
          X0 <- mmm_design_rows(m, gr(pp[2], s), batch_w, animal_ref)
          Xd <- X1 - X0
          V <- mgcv::vcov.gam(m, unconditional = TRUE); b <- stats::coef(m)
          dv <- as.numeric(Xd %*% b)
          se <- sqrt(pmax(0, rowSums((Xd %*% V) * Xd)))
          tibble(TimeAxis = gr(pp[1], s)$TimeAxis, stratum = s,
                 contrast = paste0(pp[1], "-", pp[2]),
                 diff = dv, se = se, lower = dv - 1.96 * se, upper = dv + 1.96 * se)
        })
      }) %>% tag()

      out$spec[[label]] <- mmm_gamm_spec_row(fit, label, form, d,
                                             response_scale = v$response,
                                             batch_adjusted = v$with_batch,
                                             role = v$role) %>%
        mutate(shape_structure = v$shape, k_time = v$k) %>% tag()
      out$parametric[[label]] <- mmm_gamm_parametric_table(m, label) %>% tag()
      out$smooth[[label]] <- mmm_gamm_smooth_table(m, label) %>% tag()
      out$ar1[[label]] <- mmm_ar1_sequence_proof(d, d$.ar_start, block_cols = ar_block_cols) %>% tag()

      if (v$key %in% c("primary", "full_shape")) {
        dg <- mmm_model_diagnostics(fit, d, label, response_col = "Movement")
        out$diagnostics[[label]] <- dg$summary %>% tag()
        out$k_check[[label]] <- dg$k_check %>% tag()
        out$acf[[label]] <- dg$acf %>% tag()
      }
    }
  }

  res <- lapply(out, function(x) bind_rows(x))
  res$fits <- fits
  res
}

#' Formal Group x Stratum omnibus rows.
mmm_interaction_tests <- function(parametric_tbl, stratum_col) {
  parametric_tbl %>%
    filter(.data$term %in% c("Group", "Stratum", "Group:Stratum")) %>%
    mutate(
      term_meaning = dplyr::case_when(
        .data$term == "Group" ~ "average Group difference across strata",
        .data$term == "Stratum" ~ paste0("average change across ", stratum_col),
        TRUE ~ paste0("Group x ", stratum_col, " (formal adaptation test)")
      ),
      stratum_variable = stratum_col
    )
}

#' Global + localization BH columns (section 10).
#'
#' q_BH_global24 is the conservative prespecified family and is never replaced.
#' The two localization columns are labelled SECONDARY and reported alongside.
mmm_multiplicity_localization <- function(contrasts_tbl, global_family_id) {
  contrasts_tbl %>%
    mutate(family_id_global = global_family_id,
           n_tests_global = sum(!is.na(.data$p_raw)),
           q_BH_global24 = stats::p.adjust(.data$p_raw, method = "BH")) %>%
    group_by(.data$Sex) %>%
    mutate(n_tests_within_sex = sum(!is.na(.data$p_raw)),
           q_BH_within_sex = stats::p.adjust(.data$p_raw, method = "BH")) %>%
    ungroup() %>%
    group_by(.data$stratum) %>%
    mutate(n_tests_within_stratum = sum(!is.na(.data$p_raw)),
           q_BH_within_stratum = stats::p.adjust(.data$p_raw, method = "BH")) %>%
    ungroup() %>%
    mutate(localization_note =
             "q_BH_global24 is primary; within_sex and within_stratum are SECONDARY localization values")
}

#' Compare the parsimonious primary against the full shape-interaction model.
mmm_shape_model_comparison <- function(spec_tbl) {
  spec_tbl %>%
    filter(.data$variant %in% c("primary", "full_shape")) %>%
    dplyr::select("Sex", "variant", "shape_structure", "edf_total", "aic",
                  "dev_expl", "rho_estimated", "n_obs") %>%
    tidyr::pivot_wider(names_from = "variant",
                       values_from = c("shape_structure", "edf_total", "aic", "dev_expl", "rho_estimated")) %>%
    mutate(
      delta_aic_full_minus_primary = .data$aic_full_shape - .data$aic_primary,
      delta_edf_full_minus_primary = .data$edf_total_full_shape - .data$edf_total_primary,
      delta_dev_expl = .data$dev_expl_full_shape - .data$dev_expl_primary,
      note = "reported for parsimony/robustness, not for significance-based selection"
    )
}
