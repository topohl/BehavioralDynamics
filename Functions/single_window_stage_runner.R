# ================================================================
# Shared runner for the single-window acute stages (20 Active, 23 Inactive)
# MMMSociability
# ================================================================
# One acute 12 h window, fitted within Sex:
#
#   resp ~ [Batch +] Group + s(Time, k) + s(Time, by = GroupOrd, k)
#          + s(AnimalNum, bs = "re")
#
# There is only one temporal stratum, so no stratum term is warranted.
#
# Produces everything the revision pass requires: exact model-scale AUC,
# response-scale simulated AUC, the AUC-vs-window-average audit, the six-variant
# CI-width decomposition, the batch-weighting diagnostic, descriptive
# animal-level observed AUC, full model diagnostics, and the stage's own
# six-test BH family with an explicit contrast_role hierarchy.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(ggplot2); library(mgcv)
})

for (.h in c("gamm_group_inference_helpers.R", "gamm_auc_helpers.R",
             "gamm_diagnostics_helpers.R", "mmm_publication_theme.R")) {
  .probe <- switch(.h,
    "gamm_group_inference_helpers.R" = "mmm_fit_gamm_ar1",
    "gamm_auc_helpers.R" = "mmm_auc_cvec",
    "gamm_diagnostics_helpers.R" = "mmm_traj_ci_width",
    "mmm_publication_theme.R" = "theme_mmm_pub")
  if (!exists(.probe, inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) source_mmm_helper(.h)
    else stop("single_window_stage_runner.R requires ", .h, call. = FALSE)
  }
}
rm(.h, .probe)

MMM_CONTRAST_ROLE <- c(
  "SUS-RES" = "PRIMARY_PHENOTYPE",
  "SUS-CON" = "CONTEXTUAL_CONTROL",
  "RES-CON" = "CONTEXTUAL_CONTROL"
)

#' Run one single-window acute stage end to end.
#'
#' @param selected Window rows from a phase selector (must carry TimeHours).
#' @param family_id BH family identifier for the six planned contrasts.
#' @param phase_label "Active" or "Inactive"; controls axis wording only.
mmm_run_single_window_stage <- function(selected, dirs, family_id, phase_label,
                                        window_hours = 12, grid_n = 100L,
                                        k_sensitivity = 8L, stage_tag = "first") {
  groups <- c("CON", "RES", "SUS")
  base <- selected %>%
    filter(!is.na(.data$Movement)) %>%
    mutate(
      y_log = log1p(.data$Movement), y_raw = .data$Movement,
      Group = factor(.data$Group, levels = groups),
      Batch = factor(.data$Batch), AnimalNum = factor(.data$AnimalNum),
      TimeAxis = as.numeric(.data$TimeHours)
    ) %>%
    mutate(GroupOrd = factor(.data$Group, levels = groups, ordered = TRUE))

  build_formula <- function(response, with_batch, k) stats::as.formula(paste0(
    response, " ~ ", if (with_batch) "Batch + " else "", "Group",
    " + s(TimeAxis, k = ", k, ", bs = 'tp')",
    " + s(TimeAxis, by = GroupOrd, k = ", k, ", bs = 'tp')",
    " + s(AnimalNum, bs = 're')"))

  variants <- tibble::tribble(
    ~key,        ~response, ~with_batch, ~k,            ~role,
    "primary",   "y_log",   TRUE,        MMM_GAMM_K,    "PRIMARY",
    "no_batch",  "y_log",   FALSE,       MMM_GAMM_K,    "SENSITIVITY_batch",
    "raw_scale", "y_raw",   TRUE,        MMM_GAMM_K,    "SENSITIVITY_response_scale",
    "k8",        "y_log",   TRUE,        k_sensitivity, "SENSITIVITY_k"
  )

  acc <- list(contrasts = list(), trajectory = list(), pointwise = list(), spec = list(),
              parametric = list(), smooth = list(), ar1 = list(), group_auc = list(),
              auc_contrasts = list(), resp_auc = list(), resp_auc_diff = list(),
              auc_audit = list(), ci_width = list(), batch_diag_auc = list(),
              batch_diag_contr = list(), diagnostics = list(), k_check = list(),
              acf = list(), residuals = list(), empirical = list())

  for (sx in sort(unique(as.character(base$Sex)))) {
    dsex <- base %>% filter(.data$Sex == sx)
    fits <- list()

    for (i in seq_len(nrow(variants))) {
      v <- variants[i, ]
      label <- paste0(stage_tag, "|", sx, "|", v$key)
      d <- mmm_order_for_ar1(dsex, block_cols = character(0)) %>% droplevels()
      d$.ar_start <- mmm_build_ar_start(d, block_cols = character(0))
      form <- build_formula(v$response, v$with_batch, v$k)
      cat("  fitting ", label, " ...", sep = "")
      fit <- mmm_fit_gamm_ar1(form, d)
      cat(" rho=", round(fit$rho, 3), " ar1=", fit$ar1_applied, "\n", sep = "")
      fits[[v$key]] <- list(fit = fit, data = d, formula = form, role = v$role, k = v$k,
                            response = v$response, with_batch = v$with_batch, label = label)
    }

    pr <- fits[["primary"]]
    d <- pr$data; m <- pr$fit$model
    # Manuscript-facing predictions marginalize EQUALLY over batches (Batch is
    # a blocking factor); count weighting is kept as a sensitivity.
    batch_counts <- mmm_batch_weights(d, "count")
    batch_w_primary <- mmm_batch_weights(d, MMM_BATCH_WEIGHTING)
    animal_ref <- levels(d$AnimalNum)[1]
    gfun <- function(g) tibble(TimeAxis = seq(0, window_hours, length.out = grid_n)) %>%
      mutate(Group = factor(g, levels = groups),
             GroupOrd = factor(g, levels = groups, ordered = TRUE))
    tag <- function(x, key = "primary") x %>% mutate(Sex = sx, variant = key, .before = 1)

    # ---- all variants: contrasts, trajectories, spec, terms
    for (key in names(fits)) {
      fo <- fits[[key]]; mm <- fo$fit$model; dd <- fo$data
      bw <- mmm_batch_weights(dd, MMM_BATCH_WEIGHTING)
      aref <- levels(dd$AnimalNum)[1]
      acc$contrasts[[fo$label]] <- purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
        avg <- mmm_gamm_avg_contrast_grids(mm, gfun(pp[1]), gfun(pp[2]), bw, aref)
        auc <- mmm_gamm_auc_contrast(mm, gfun(pp[1]), gfun(pp[2]), bw, aref, "TimeAxis", window_hours)
        bind_cols(tibble(contrast = paste0(pp[1], "-", pp[2]),
                         group_comp = pp[1], group_ref = pp[2]),
                  avg %>% dplyr::select("estimate", "se", "ci_low", "ci_high", "statistic", "p_raw"),
                  auc %>% dplyr::select("AUC_diff_log1p", "AUC_diff_SE", "AUC_diff_CI_low",
                                        "AUC_diff_CI_high", "AUC_diff_p", "AUC_diff_per_hour"))
      }) %>% tag(key)
      acc$trajectory[[fo$label]] <- purrr::map_dfr(groups, function(g)
        mmm_gamm_trajectory(mm, gfun(g), bw, aref) %>% mutate(Group = g, .before = 1)) %>% tag(key)
      acc$spec[[fo$label]] <- mmm_gamm_spec_row(fo$fit, fo$label, fo$formula, dd,
        response_scale = fo$response, batch_adjusted = fo$with_batch, role = fo$role) %>%
        mutate(k_time = fo$k) %>% tag(key)
      acc$parametric[[fo$label]] <- mmm_gamm_parametric_table(mm, fo$label) %>% tag(key)
      acc$smooth[[fo$label]] <- mmm_gamm_smooth_table(mm, fo$label) %>% tag(key)
      acc$ar1[[fo$label]] <- mmm_ar1_sequence_proof(dd, dd$.ar_start, block_cols = character(0)) %>% tag(key)
    }

    # ---- primary only: pointwise, AUC, response AUC, audits, diagnostics
    acc$pointwise[[sx]] <- purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
      X1 <- mmm_design_rows(m, gfun(pp[1]), batch_w_primary, animal_ref)
      X0 <- mmm_design_rows(m, gfun(pp[2]), batch_w_primary, animal_ref)
      Xd <- X1 - X0; V <- mgcv::vcov.gam(m, unconditional = TRUE); b <- stats::coef(m)
      dv <- as.numeric(Xd %*% b); se <- sqrt(pmax(0, rowSums((Xd %*% V) * Xd)))
      tibble(TimeAxis = gfun(pp[1])$TimeAxis, contrast = paste0(pp[1], "-", pp[2]),
             diff = dv, se = se, lower = dv - 1.96 * se, upper = dv + 1.96 * se)
    }) %>% tag()

    acc$group_auc[[sx]] <- purrr::map_dfr(groups, function(g)
      mmm_gamm_group_auc(m, gfun(g), batch_w_primary, animal_ref, "TimeAxis", window_hours) %>%
        mutate(Group = g, .before = 1)) %>% tag()

    acc$auc_contrasts[[sx]] <- purrr::map_dfr(MMM_GROUP_PAIRS, function(pp)
      mmm_gamm_auc_contrast(m, gfun(pp[1]), gfun(pp[2]), batch_w_primary, animal_ref,
                            "TimeAxis", window_hours) %>%
        mutate(contrast = paste0(pp[1], "-", pp[2]), .before = 1)) %>% tag()

    grids <- setNames(lapply(groups, gfun), groups)
    sim <- mmm_gamm_response_auc_sim(m, grids, batch_w_primary, animal_ref, "TimeAxis",
                                     window_hours = window_hours)
    acc$resp_auc[[sx]] <- sim %>% rename(Group = "level_name") %>% tag()
    acc$resp_auc_diff[[sx]] <- mmm_response_auc_contrasts(sim, MMM_GROUP_PAIRS) %>% tag()

    acc$auc_audit[[sx]] <- purrr::map_dfr(MMM_GROUP_PAIRS, function(pp)
      mmm_auc_vs_window_average(m, gfun(pp[1]), gfun(pp[2]), batch_w_primary, animal_ref,
                                "TimeAxis", window_hours) %>%
        mutate(contrast = paste0(pp[1], "-", pp[2]), .before = 1)) %>% tag()

    equal_w <- setNames(rep(1, length(batch_counts)), names(batch_counts))
    ci_variants <- list(
      A_primary = list(model = m, batch_weights = batch_w_primary, unconditional = TRUE,
                       note = "PRIMARY: batch-adjusted, AR1, unconditional covariance, equal batch weighting"),
      B_conditional_vcov = list(model = m, batch_weights = batch_w_primary, unconditional = FALSE,
                       note = "DIAGNOSTIC ONLY: conditional on smoothing parameters"),
      C_no_batch = list(model = fits[["no_batch"]]$fit$model, batch_weights = batch_w_primary,
                       unconditional = TRUE, note = "AR1 retained, Batch dropped"),
      D_no_ar1 = list(model = pr$fit$model_stage1, batch_weights = batch_w_primary,
                       unconditional = TRUE, note = "DIAGNOSTIC ONLY: stage-1 fit without AR1"),
      E_equal_batch = list(model = m, batch_weights = equal_w, unconditional = TRUE,
                       note = "equal weight per batch (coincides with A: equal is now primary)"),
      F_count_batch = list(model = m, batch_weights = batch_counts, unconditional = TRUE,
                       note = "animal-count weight per batch (SENSITIVITY; primary is now equal weighting)")
    )
    acc$ci_width[[sx]] <- mmm_ci_width_diagnostics(ci_variants, gfun, groups, animal_ref, sx)

    bd <- mmm_batch_weighting_diagnostic(m, gfun, groups, animal_ref, "TimeAxis",
                                         batch_counts, sx, window_hours)
    acc$batch_diag_auc[[sx]] <- bd$group_auc
    acc$batch_diag_contr[[sx]] <- bd$contrasts

    dg <- mmm_model_diagnostics(pr$fit, d, pr$label, response_col = "Movement")
    acc$diagnostics[[sx]] <- dg$summary %>% tag()
    acc$k_check[[sx]] <- dg$k_check %>% tag()
    acc$acf[[sx]] <- dg$acf %>% tag()
    acc$residuals[[sx]] <- dg$residuals %>% tag()

    acc$empirical[[sx]] <- mmm_animal_empirical_auc(
      selected %>% filter(.data$Sex == sx), time_col = "TimeHours",
      window_hours = window_hours)
  }

  res <- lapply(acc, function(x) bind_rows(x))

  # ---- planned six-test family with the biological role hierarchy
  res$primary_contrasts <- res$contrasts %>%
    filter(.data$variant == "primary") %>%
    mutate(contrast_role = unname(MMM_CONTRAST_ROLE[.data$contrast])) %>%
    mmm_bh_family(family_id)
  res
}
