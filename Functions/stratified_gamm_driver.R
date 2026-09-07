# ================================================================
# Shared driver for the two SECONDARY stratified Active-window GAMMs
# MMMSociability
# ================================================================
# Both secondary analyses have the same shape: one Active-window dataset that
# is stratified by a repeated factor, fitted separately within Sex, with a
# formal Group x Stratum interaction.
#
#   Analysis/21  stratum = ActiveNight  (nights 1..N inside CC1)
#   Analysis/22  stratum = CageChange   (acute window after CC1..CC4)
#
# Model (identifiable form, see gamm_group_inference_helpers.R for why the
# obvious bs="sz" spelling is not used):
#
#   response ~ [Batch +] Group * Stratum
#              + s(Time, k = 6, bs = "tp")
#              + s(Time, by = ordered(Group x Stratum), k = 6, bs = "tp")
#              + s(AnimalNum, bs = "re")
#   fREML, discrete, two-stage AR1 with block-correct AR.start.
#
# The driver returns tidy tables only. Multiplicity is NOT applied here: each
# calling stage declares its own family explicitly, so the two secondary
# families can never be silently merged with each other or with the PRIMARY
# six-test first-night family.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(mgcv)
})

if (!exists("mmm_fit_gamm_ar1", mode = "function", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("gamm_group_inference_helpers.R")
  } else {
    stop("stratified_gamm_driver.R requires gamm_group_inference_helpers.R", call. = FALSE)
  }
}

MMM_GROUP_PAIRS <- list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES"))

#' Run one stratified Active-window GAMM analysis across Sex and model variants.
#'
#' @param dat Selected window rows. Must carry AnimalNum, Batch, Group, Sex,
#'   Movement, the time column and the stratum column.
#' @param stratum_col Name of the repeated factor ("ActiveNight" / "CageChange").
#' @param time_col Name of the within-window clock axis, in hours.
#' @param ar_block_cols Block keys for AR1 restarts, in addition to AnimalNum.
#' @param window_hours Axis length for the prediction grid.
#' @param grid_n Prediction grid resolution.
#' @return Named list of tidy tibbles.
mmm_run_stratified_gamm <- function(dat,
                                    stratum_col,
                                    time_col,
                                    ar_block_cols,
                                    window_hours = 12,
                                    grid_n = 100L) {
  stopifnot(all(c("AnimalNum", "Batch", "Group", "Sex", "Movement",
                  stratum_col, time_col) %in% names(dat)))

  base <- dat %>%
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
    mutate(Cross = mmm_ordered_cross(.data$Group, .data$Stratum))

  variants <- tibble::tribble(
    ~key,        ~response, ~with_batch, ~role,
    "primary",   "y_log",   TRUE,        "PRIMARY",
    "no_batch",  "y_log",   FALSE,       "SENSITIVITY_batch",
    "raw_scale", "y_raw",   TRUE,        "SENSITIVITY_response_scale"
  )

  build_formula <- function(response, with_batch) {
    stats::as.formula(paste0(
      response, " ~ ", if (with_batch) "Batch + " else "",
      "Group * Stratum",
      " + s(TimeAxis, k = ", MMM_GAMM_K, ", bs = 'tp')",
      " + s(TimeAxis, by = Cross, k = ", MMM_GAMM_K, ", bs = 'tp')",
      " + s(AnimalNum, bs = 're')"
    ))
  }

  out <- list(contrasts = list(), stratum_avg = list(), trajectory = list(),
              pointwise = list(), spec = list(), parametric = list(),
              smooth = list(), ar1 = list())

  for (sx in sort(unique(as.character(base$Sex)))) {
    dsex <- base %>% filter(.data$Sex == sx)

    for (i in seq_len(nrow(variants))) {
      v <- variants[i, ]
      label <- paste0(stratum_col, "|", sx, "|", v$key)
      cat("  fitting ", label, " ...", sep = "")

      d <- mmm_order_for_ar1(dsex, block_cols = ar_block_cols) %>% droplevels()
      d$.ar_start <- mmm_build_ar_start(d, block_cols = ar_block_cols)
      form <- build_formula(v$response, v$with_batch)
      fit <- mmm_fit_gamm_ar1(form, d)
      m <- fit$model
      cat(" rho=", round(fit$rho, 3), " ar1=", fit$ar1_applied, "\n", sep = "")

      batch_w <- table(distinct(d, AnimalNum, Batch)$Batch)
      batch_w <- batch_w[batch_w > 0]
      animal_ref <- levels(d$AnimalNum)[1]
      strata <- levels(d$Stratum)
      tgrid <- seq(0, window_hours, length.out = grid_n)

      # Grid rows carry Group, Stratum and the ordered cross level explicitly,
      # so a contrast can be averaged across strata as well as across time.
      make_grid <- function(g, s) {
        tibble(TimeAxis = tgrid) %>%
          mutate(
            Group = factor(g, levels = levels(d$Group)),
            Stratum = factor(s, levels = levels(d$Stratum)),
            Cross = factor(paste(g, s, sep = "."), levels = levels(d$Cross), ordered = TRUE)
          )
      }
      stack_grid <- function(g) purrr::map_dfr(strata, ~ make_grid(g, .x))

      tag <- function(x) x %>% mutate(Sex = sx, variant = v$key, model_label = label, .before = 1)

      # --- per-stratum planned contrasts
      out$contrasts[[label]] <- purrr::map_dfr(strata, function(s) {
        purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
          mmm_gamm_avg_contrast_grids(m, make_grid(pp[1], s), make_grid(pp[2], s),
                                      batch_w, animal_ref) %>%
            mutate(stratum = s, contrast = paste0(pp[1], "-", pp[2]),
                   group_comp = pp[1], group_ref = pp[2], .before = 1)
        })
      }) %>% tag()

      # --- contrasts averaged over the whole stratified period
      out$stratum_avg[[label]] <- purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
        mmm_gamm_avg_contrast_grids(m, stack_grid(pp[1]), stack_grid(pp[2]),
                                    batch_w, animal_ref) %>%
          mutate(contrast = paste0(pp[1], "-", pp[2]),
                 group_comp = pp[1], group_ref = pp[2], .before = 1)
      }) %>% tag()

      # --- trajectories
      out$trajectory[[label]] <- purrr::map_dfr(strata, function(s) {
        purrr::map_dfr(levels(d$Group), function(g) {
          mmm_gamm_trajectory(m, make_grid(g, s), batch_w, animal_ref) %>%
            mutate(stratum = s, Group = g, .before = 1)
        })
      }) %>% tag()

      # --- pointwise differences
      out$pointwise[[label]] <- purrr::map_dfr(strata, function(s) {
        purrr::map_dfr(MMM_GROUP_PAIRS, function(pp) {
          g1 <- make_grid(pp[1], s); g0 <- make_grid(pp[2], s)
          X1 <- mmm_design_rows(m, g1, batch_w, animal_ref)
          X0 <- mmm_design_rows(m, g0, batch_w, animal_ref)
          Xd <- X1 - X0
          V <- mgcv::vcov.gam(m, unconditional = TRUE)
          b <- stats::coef(m)
          dvec <- as.numeric(Xd %*% b)
          se <- sqrt(pmax(0, rowSums((Xd %*% V) * Xd)))
          tibble(TimeAxis = tgrid, stratum = s,
                 contrast = paste0(pp[1], "-", pp[2]),
                 diff = dvec, se = se,
                 lower = dvec - 1.96 * se, upper = dvec + 1.96 * se)
        })
      }) %>% tag()

      out$spec[[label]] <- mmm_gamm_spec_row(fit, label, form, d,
                                             response_scale = v$response,
                                             batch_adjusted = v$with_batch,
                                             role = v$role) %>% tag()
      out$parametric[[label]] <- mmm_gamm_parametric_table(m, label) %>% tag()
      out$smooth[[label]] <- mmm_gamm_smooth_table(m, label) %>% tag()
      out$ar1[[label]] <- mmm_ar1_sequence_proof(d, d$.ar_start, block_cols = ar_block_cols) %>% tag()
    }
  }

  lapply(out, function(x) bind_rows(x))
}

#' Pull the formal Group x Stratum interaction rows out of the parametric table.
#'
#' This is the adaptation test. It is a SEPARATE inferential family from the
#' pairwise contrasts and is never pooled with them.
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
