# ================================================================
# Shared runner for the Inactive Markov-binomial stages (23, 24, 25)
# MMMSociability
# ================================================================
# Stage 23 is a single acute window; stages 24 and 25 add a repeated stratum
# (InactiveBlock / CageChange). One runner covers both: `stratum_col = NULL`
# selects the single-window form.
#
# Primary models (binomial logit, mgcv::bam, fREML, discrete):
#
#   single window (23)
#     ActiveBin ~ Batch + Group*PrevState + s(TimeHours,k) +
#                 s(TimeHours,by=GroupOrd,k) + s(AnimalNum,bs="re")
#
#   stratified (24, 25) - parsimonious, NO three-way interaction
#     ActiveBin ~ Batch + Group*Stratum + PrevState + Group:PrevState +
#                 Stratum:PrevState + s(TimeHours,k) +
#                 s(TimeHours,by=GroupOrd,k) + s(TimeHours,by=StratumOrd,k) +
#                 s(AnimalNum,bs="re")
#
# Declared secondaries: the full Group x Stratum x PrevState transition
# interaction, and the full Group x Stratum trajectory-shape interaction.
# Neither is promoted unless the parsimonious model fails the adequacy gate.
#
# No rho / AR.start anywhere: PrevState IS the dependence model.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble); library(mgcv)
})

for (.h in c("gamm_group_inference_helpers.R", "gamm_auc_helpers.R",
             "inactive_markov_gamm_helpers.R")) {
  .probe <- switch(.h, "gamm_group_inference_helpers.R" = "mmm_design_rows",
                       "gamm_auc_helpers.R" = "mmm_trapz_weights",
                       "inactive_markov_gamm_helpers.R" = "mmm_build_prev_state")
  if (!exists(.probe, mode = "function", inherits = TRUE)) {
    if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) source_mmm_helper(.h)
    else stop("inactive_markov_stage_runner.R requires ", .h, call. = FALSE)
  }
}
rm(.h, .probe)

#' Formula builder for either Inactive form.
mmm_inactive_formula <- function(with_batch, stratified, k = MMM_GAMM_K,
                                 shape = c("parsimonious", "full_shape"),
                                 transition = c("parsimonious", "full_transition")) {
  shape <- match.arg(shape); transition <- match.arg(transition)
  b <- if (with_batch) "Batch + " else ""
  if (!stratified) {
    return(stats::as.formula(paste0(
      "ActiveBin ~ ", b, "Group*PrevState",
      " + s(TimeHours, k = ", k, ", bs = 'tp')",
      " + s(TimeHours, by = GroupOrd, k = ", k, ", bs = 'tp')",
      " + s(AnimalNum, bs = 're')")))
  }
  fixed <- if (transition == "full_transition") "Group*Stratum*PrevState" else
    "Group*Stratum + PrevState + Group:PrevState + Stratum:PrevState"
  shape_terms <- if (shape == "full_shape") {
    paste0(" + s(TimeHours, by = Cross, k = ", k, ", bs = 'tp')")
  } else {
    paste0(" + s(TimeHours, by = GroupOrd, k = ", k, ", bs = 'tp')",
           " + s(TimeHours, by = StratumOrd, k = ", k, ", bs = 'tp')")
  }
  stats::as.formula(paste0("ActiveBin ~ ", b, fixed,
    " + s(TimeHours, k = ", k, ", bs = 'tp')", shape_terms, " + s(AnimalNum, bs = 're')"))
}

#' Prepare an Inactive window dataset: ActiveBin, PrevState, factors.
mmm_prepare_inactive <- function(dat, block_cols, stratum_col = NULL, time_col = "TimeHours") {
  d <- mmm_build_prev_state(dat, block_cols = block_cols)
  d <- d %>% mutate(
    Group = factor(.data$Group, levels = c("CON", "RES", "SUS")),
    Batch = factor(.data$Batch), AnimalNum = factor(.data$AnimalNum),
    TimeHours = as.numeric(.data[[time_col]]),
    GroupOrd = factor(.data$Group, levels = c("CON", "RES", "SUS"), ordered = TRUE))
  if (!is.null(stratum_col)) {
    d <- d %>% mutate(Stratum = factor(as.character(.data[[stratum_col]]))) %>%
      mutate(StratumOrd = factor(.data$Stratum, levels = levels(.data$Stratum), ordered = TRUE),
             Cross = mmm_ordered_cross(.data$Group, .data$Stratum))
  }
  d
}

#' Slot grid for the recursion: one row per SCHEDULED slot.
mmm_inactive_slot_grid <- function(d, group, stratum = NULL) {
  slots <- sort(unique(d$target_slot))
  g <- tibble(target_slot = slots,
              TimeHours = (slots - 1) * (d$bin_size_sec[1] / 3600),
              Group = factor(group, levels = levels(d$Group)),
              GroupOrd = factor(group, levels = levels(d$GroupOrd), ordered = TRUE))
  if (!is.null(stratum)) {
    g <- g %>% mutate(
      Stratum = factor(stratum, levels = levels(d$Stratum)),
      StratumOrd = factor(stratum, levels = levels(d$StratumOrd), ordered = TRUE),
      Cross = factor(paste(group, stratum, sep = "."), levels = levels(d$Cross), ordered = TRUE))
  }
  g
}

#' Run one Inactive Markov stage end to end.
mmm_run_inactive_markov_stage <- function(selected, block_cols, stage_id, phase_label = "Inactive",
                                          stratum_col = NULL, time_col = "TimeHours",
                                          n_draws = MMM_MARKOV_SIM_DRAWS,
                                          seed = MMM_MARKOV_SIM_SEED,
                                          k_sensitivity = 8L) {
  stratified <- !is.null(stratum_col)
  base <- mmm_prepare_inactive(selected, block_cols, stratum_col, time_col)

  variants <- if (!stratified) tibble::tribble(
    ~key, ~with_batch, ~k, ~shape, ~transition, ~role,
    "primary",       TRUE,  MMM_GAMM_K,    "parsimonious", "parsimonious", "PRIMARY",
    "no_batch",      FALSE, MMM_GAMM_K,    "parsimonious", "parsimonious", "SENSITIVITY_batch",
    "k8",            TRUE,  k_sensitivity, "parsimonious", "parsimonious", "SENSITIVITY_k"
  ) else tibble::tribble(
    ~key, ~with_batch, ~k, ~shape, ~transition, ~role,
    "primary",          TRUE,  MMM_GAMM_K,    "parsimonious", "parsimonious",     "PRIMARY",
    "no_batch",         FALSE, MMM_GAMM_K,    "parsimonious", "parsimonious",     "SENSITIVITY_batch",
    "k8",               TRUE,  k_sensitivity, "parsimonious", "parsimonious",     "SENSITIVITY_k",
    "full_shape",       TRUE,  MMM_GAMM_K,    "full_shape",   "parsimonious",     "SECONDARY_SHAPE_INTERACTION_SENSITIVITY",
    "full_transition",  TRUE,  MMM_GAMM_K,    "parsimonious", "full_transition",  "SECONDARY_TRANSITION_INTERACTION_SENSITIVITY"
  )

  acc <- list(spec = list(), parametric = list(), trajectory = list(), group_auc = list(),
              contrasts = list(), calibration = list(), decile = list(), trans_calib = list(),
              resid_dep = list(), trans_matrix = list(), state_proof = list(),
              observed_trans = list(), adequacy = list(), adaptation = list())

  for (sx in sort(unique(as.character(base$Sex)))) {
    dsex <- base %>% filter(.data$Sex == sx) %>% droplevels()
    strata <- if (stratified) levels(dsex$Stratum) else NA_character_
    bl <- levels(dsex$Batch); aref <- levels(dsex$AnimalNum)[1]
    groups <- levels(dsex$Group)
    tagx <- function(x, key) if (is.null(x) || nrow(x) == 0) x else
      x %>% mutate(Sex = sx, variant = key, stage_id = stage_id, phase = phase_label, .before = 1)

    acc$state_proof[[sx]] <- mmm_prev_state_proof(dsex, block_cols) %>% tagx("primary")
    acc$observed_trans[[sx]] <- mmm_observed_transitions(
      dsex, by = if (stratified) c("Sex", "Group", "Stratum") else c("Sex", "Group")) %>%
      mutate(stage_id = stage_id, variant = "observed", .before = 1)

    fits <- list()
    for (i in seq_len(nrow(variants))) {
      v <- variants[i, ]
      label <- paste0(stage_id, "|", sx, "|", v$key)
      form <- mmm_inactive_formula(v$with_batch, stratified, v$k, v$shape, v$transition)
      cat("  fitting ", label, " ...", sep = "")
      fit <- tryCatch(mgcv::bam(form, data = dsex, family = stats::binomial(),
                                method = MMM_GAMM_METHOD, discrete = TRUE),
                      error = function(e) { cat(" FAILED: ", conditionMessage(e), "\n"); NULL })
      if (is.null(fit)) next
      cat(" dev.expl=", round(summary(fit)$dev.expl, 4), "\n", sep = "")
      fits[[v$key]] <- list(model = fit, formula = form, role = v$role, k = v$k)

      acc$spec[[label]] <- tibble(
        model_label = label, role = v$role, family = "binomial(logit)",
        engine = "mgcv::bam", method = MMM_GAMM_METHOD, discrete = TRUE, k_time = v$k,
        shape_structure = v$shape, transition_structure = v$transition,
        batch_adjusted = v$with_batch,
        formula = paste(deparse(form), collapse = " "),
        n_obs = nrow(dsex), n_animals = dplyr::n_distinct(dsex$AnimalNum),
        dev_expl = unname(summary(fit)$dev.expl), edf_total = sum(fit$edf),
        aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
        serial_dependence_model = "explicit first-order PrevState (no AR1)") %>% tagx(v$key)
      acc$parametric[[label]] <- as.data.frame(summary(fit)$p.table) %>%
        tibble::rownames_to_column("term") %>% as_tibble() %>%
        rename(estimate = "Estimate", se = "Std. Error", statistic = "z value", p_raw = "Pr(>|z|)") %>%
        mutate(model_label = label) %>% tagx(v$key)
    }

    # ---- diagnostics: primary vs an explicit no-PrevState comparator
    pm <- fits[["primary"]]$model
    f_nops <- if (!stratified) {
      stats::as.formula(paste0("ActiveBin ~ Batch + Group + s(TimeHours, k = ", MMM_GAMM_K,
        ", bs = 'tp') + s(TimeHours, by = GroupOrd, k = ", MMM_GAMM_K,
        ", bs = 'tp') + s(AnimalNum, bs = 're')"))
    } else {
      stats::as.formula(paste0("ActiveBin ~ Batch + Group*Stratum + s(TimeHours, k = ", MMM_GAMM_K,
        ", bs = 'tp') + s(TimeHours, by = GroupOrd, k = ", MMM_GAMM_K,
        ", bs = 'tp') + s(TimeHours, by = StratumOrd, k = ", MMM_GAMM_K,
        ", bs = 'tp') + s(AnimalNum, bs = 're')"))
    }
    m_nops <- mgcv::bam(f_nops, data = dsex, family = stats::binomial(),
                        method = MMM_GAMM_METHOD, discrete = TRUE)

    acc$calibration[[sx]] <- bind_rows(
      mmm_markov_calibration(pm, dsex, "with_PrevState"),
      mmm_markov_calibration(m_nops, dsex, "without_PrevState")) %>% tagx("primary")
    acc$decile[[sx]] <- mmm_markov_decile_calibration(pm, dsex, "with_PrevState") %>% tagx("primary")
    acc$trans_calib[[sx]] <- mmm_markov_transition_calibration(pm, dsex, "with_PrevState") %>% tagx("primary")
    acc$resid_dep[[sx]] <- bind_rows(
      mmm_markov_residual_dependence(pm, dsex, block_cols, "with_PrevState"),
      mmm_markov_residual_dependence(m_nops, dsex, block_cols, "without_PrevState")) %>% tagx("primary")
    acc$trans_matrix[[sx]] <- mmm_markov_transition_matrices(
      pm, dsex, by = if (stratified) c("Sex", "Group", "Stratum") else c("Sex", "Group")) %>%
      mutate(stage_id = stage_id, .before = 1)

    # ---- marginal trajectories, AUC and contrasts (primary only)
    B <- mmm_markov_draws(pm, n_draws = n_draws, seed = seed)
    strata_iter <- if (stratified) strata else NA_character_
    ds_all <- list()
    for (s in strata_iter) {
      for (g in groups) {
        grid <- mmm_inactive_slot_grid(dsex, g, if (stratified) s else NULL)
        ds_all[[paste(g, s, sep = "|")]] <- list(
          ds = mmm_markov_design_sets(pm, grid, bl, aref), th = grid$TimeHours, g = g, s = s)
      }
    }
    for (nm in names(ds_all)) {
      e <- ds_all[[nm]]
      sim <- mmm_markov_sim_group(e$ds, B, e$th)
      acc$trajectory[[paste0(sx, nm)]] <- sim$trajectory %>%
        mutate(Group = e$g, stratum = e$s, .before = 1) %>% tagx("primary")
      acc$group_auc[[paste0(sx, nm)]] <- sim$auc %>%
        mutate(Group = e$g, stratum = e$s, .before = 1) %>% tagx("primary")
    }
    for (s in strata_iter) {
      for (pp in MMM_GROUP_PAIRS) {
        e1 <- ds_all[[paste(pp[1], s, sep = "|")]]; e0 <- ds_all[[paste(pp[2], s, sep = "|")]]
        acc$contrasts[[paste0(sx, s, pp[1], pp[2])]] <-
          mmm_markov_auc_contrast(pm, e1$ds, e0$ds, e1$th, B, pp[1], pp[2]) %>%
          mutate(stratum = s, contrast_role = unname(MMM_CONTRAST_ROLE[paste0(pp[1], "-", pp[2])]),
                 .before = 1) %>% tagx("primary")
      }
    }

    # ---- adaptation contrasts (stratified only), all nonlinear -> delta method
    if (stratified && length(strata) > 1L) {
      s1 <- strata[1]; sL <- strata[length(strata)]
      aucf <- function(g, s) function(beta)
        mmm_markov_auc(mmm_markov_marginal(ds_all[[paste(g, s, sep = "|")]]$ds, beta)$p,
                       ds_all[[paste(g, s, sep = "|")]]$th)
      rows <- list()
      overall <- function(beta) mean(vapply(groups, function(g) aucf(g, sL)(beta), numeric(1))) -
                                mean(vapply(groups, function(g) aucf(g, s1)(beta), numeric(1)))
      rows[[1]] <- mmm_numeric_delta(pm, overall) %>%
        mutate(scope = "overall_population", contrast_name = paste0(sL, " - ", s1), .before = 1)
      for (g in groups) {
        fn <- local({ gg <- g; function(beta) aucf(gg, sL)(beta) - aucf(gg, s1)(beta) })
        rows[[length(rows) + 1]] <- mmm_numeric_delta(pm, fn) %>%
          mutate(scope = g, contrast_name = paste0(sL, " - ", s1), .before = 1)
      }
      for (pp in MMM_GROUP_PAIRS) {
        fn <- local({ p1 <- pp[1]; p0 <- pp[2]; function(beta)
          (aucf(p1, sL)(beta) - aucf(p0, sL)(beta)) - (aucf(p1, s1)(beta) - aucf(p0, s1)(beta)) })
        rows[[length(rows) + 1]] <- mmm_numeric_delta(pm, fn) %>%
          mutate(scope = "phenotype_dependent",
                 contrast_name = paste0("[", pp[1], "-", pp[2], "]_", sL,
                                        " - [", pp[1], "-", pp[2], "]_", s1), .before = 1)
      }
      acc$adaptation[[sx]] <- bind_rows(rows) %>%
        mutate(Sex = sx, AUC_scale = "probability_hours",
               first_stratum = s1, last_stratum = sL, stratum_variable = stratum_col,
               stage_id = stage_id, phase = phase_label, .before = 1)
    }
  }

  res <- lapply(acc, function(x) bind_rows(x))

  # ---- adequacy gate
  res$adequacy <- mmm_markov_adequacy_gate(
    calib = res$calibration %>% filter(.data$model_label == "with_PrevState"),
    resid_with = res$resid_dep %>% filter(.data$model_label == "with_PrevState"),
    resid_without = res$resid_dep %>% filter(.data$model_label == "without_PrevState"),
    trans_calib = res$trans_calib,
    se_ratios = res$contrasts$se_ratio_sim_over_delta) %>%
    mutate(stage_id = stage_id, phase = phase_label, .before = 1)

  # ---- planned six-test family (single window) or the three adaptation families
  if (!stratified) {
    res$primary_contrasts <- res$contrasts %>%
      mmm_bh_family(paste0("SECONDARY_FIRST_INACTIVE_MARKOV__2Sex_x_3GroupContrasts"),
                    p_col = "AUC_diff_p")
  } else {
    res$adaptation <- mmm_adaptation_families(res$adaptation, stage_id, phase_label)
    res$localization <- res$contrasts %>%
      mutate(p_raw = .data$AUC_diff_p) %>%
      mmm_multiplicity_localization(paste0(stage_id, "__INACTIVE_MARKOV__local_contrasts"))
  }
  res
}
