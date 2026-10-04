# ================================================================
# Stage 28 - Four core raw-RFID behavioural domains
# MMMSociability
# ================================================================
# Goal:
#   1) A first-night (CC1, first Active 12 h) characterisation on FOUR core
#      raw-RFID domains, with cage- and batch-aware inference and HIERARCHICAL
#      multiplicity (BH across four domain omnibus tests per Sex, then gated Holm
#      within a supported domain).
#   2) A separate LONGITUDINAL analysis in which adaptation is actually measured
#      as change in the same acute response across CC1-CC4.
#   3) A phenotype-blind construct audit of the four-domain set.
#   4) An explicit OLD (five-domain) vs NEW (four-domain) comparison.
#
# Manuscript role:
#   SECONDARY, DESCRIPTIVE characterisation. RES/SUS are defined by a LATER CombZ
#   composite, so every group contrast here describes animals grouped by their
#   later outcome. Stage 09 owns the prospective question and is NOT touched.
#
# What this stage does NOT do:
#   * It does not modify or re-run Stage 01 or Stage 09, and it does not edit
#     Functions/first_night_window_helpers.R (locked by the Stage 09 parity test)
#     or Functions/first_night_domain_helpers.R (the preserved legacy contract).
#   * It does not put any HMM state-architecture quantity into the core domain set.
#   * It does not analyse Inactive-phase behaviour, because
#     docs/KNOWN_LIMITATIONS.md item 3 (inactive read density cannot be separated
#     from genuine rest) is unresolved. Active phase only.
#   * It does not choose domains, formulas, models or scalings by p-value.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("project_paths.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")
source_mmm_helper("rfid_domain_core.R")
source_mmm_helper("rfid_domain_scaling.R")
source_mmm_helper("rfid_acute_window_helpers.R")
source_mmm_helper("rfid_domain_inference.R")
source_mmm_helper("rfid_longitudinal_inference.R")

if (!exists("ensure_dir")) {
  ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
  }
}

PROJECT_ROOT <- mmm_project_root()
# The acute_window_* tables are inputs the frozen Stage 29 release pinned by sha256: on the live root the guard
# refuses to run without options(mmm.allow_pinned_overwrite = TRUE) (Functions/frozen_input_guard.R).
source_mmm_helper("frozen_input_guard.R")
frozen_guard_snapshot <- mmm_frozen_guard_before("28", PROJECT_ROOT)
PRIMARY_BIN <- "10min_based"
SENSITIVITY_BIN <- "5min_based"
STAGE_ID <- "28"
STAGE_NAME <- "rfid_behavioral_domains"

stage_out <- function(bin_level) {
  ensure_dir(file.path(behavior_stage_dir(PROJECT_ROOT, STAGE_ID, STAGE_NAME,
                                          sub("_based$", "", bin_level)), "tables"))
}

# ----------------------------------------------------------------- load

load_stage01 <- function(bin_level) {
  f <- file.path(mmm_derived_metrics_output_root(PROJECT_ROOT), bin_level,
                 "all_behavior_metrics.csv")
  if (!file.exists(f)) stop("Missing Stage 01 input for ", bin_level, ": ", f, call. = FALSE)
  d <- readr::read_csv(f, col_types = readr::cols(
    AnimalNum = readr::col_character(), BinStart = readr::col_datetime(),
    .default = readr::col_guess()), progress = FALSE)
  d$AnimalNum <- canonical_animal_id(d$AnimalNum)
  attr(d, "source_table") <- f
  d
}

bin_seconds <- function(bin_level) {
  switch(bin_level, "1min_based" = 60, "5min_based" = 300, "10min_based" = 600,
         "30min_based" = 1800,
         stop("Unsupported bin level: ", bin_level, call. = FALSE))
}

# ------------------------------------------------ acute windows + features

build_acute_features <- function(dat, bin_level) {
  bs <- bin_seconds(bin_level)
  validity <- mmm_rfid_assert_acute_window_validity(dat, bin_size_sec = bs)
  sel <- mmm_rfid_select_all_acute_windows(dat, bin_size_sec = bs)
  qc <- mmm_rfid_acute_window_qc(sel)
  mmm_rfid_assert_one_cage_per_animal_window(qc)
  feat <- mmm_rfid_build_window_features(sel)
  feat$CageEpochID <- mmm_rfid_cage_epoch_id(feat$Batch, feat$System, feat$CageChangeLabel)
  feat$bin_level <- bin_level
  list(validity = validity, selected = sel, qc = qc, features = feat)
}

# --------------------------------------------------------- first night

run_first_night <- function(feat, bin_level, resolution_role) {
  fn <- feat %>% filter(.data$CageChangeLabel == "CC1")
  if (nrow(fn) == 0L) stop("No CC1 rows for ", bin_level, call. = FALSE)

  std <- mmm_rfid_standardize_within_sex(
    fn, reference_label = paste0("first_night_CC1_within_Sex__", bin_level))
  z <- std$scaled

  scored <- mmm_rfid_score_all_domains(
    z, id_cols = c("AnimalNum", "Group", "Sex", "Batch", "System", "SourceFile",
                   "CageEpochID", "CageChangeLabel")) %>%
    mutate(bin_level = bin_level, resolution_role = resolution_role,
           domain_role = "CORE",
           scaling_reference = "z within Sex across all CC1 animals; never within Group",
           aggregation_level = "one animal = one value",
           interpretation_guard = MMM_RFID_DESCRIPTIVE_GUARD)

  sens <- mmm_rfid_score_all_domains(
    z, coefficients = MMM_RFID_SENSITIVITY_COEFFICIENTS,
    formulas = MMM_RFID_SENSITIVITY_FORMULAS,
    id_cols = c("AnimalNum", "Group", "Sex", "Batch", "System", "CageEpochID")) %>%
    mutate(bin_level = bin_level, domain_role = "EQUAL_WEIGHT_SENSITIVITY")

  domains <- MMM_RFID_CORE_DOMAINS
  sexes <- intersect(MMM_RFID_SEX_LEVELS, unique(as.character(scored$Sex)))

  omnibus <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_omnibus_group(scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  })
  pairwise <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_pairwise(scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  })
  hier <- mmm_rfid_apply_hierarchy(
    omnibus, pairwise,
    family_prefix = paste0("RFID_FIRSTNIGHT_", sub("_based$", "", bin_level)))

  inter <- purrr::map(domains, function(dm) {
    mmm_rfid_group_sex_interaction(scored %>% filter(.data$Domain == dm), dm)
  })
  interaction_tests <- purrr::map_dfr(inter, "test")
  interaction_alias <- purrr::map_dfr(inter, "aliasing")
  interaction_primary <- interaction_tests %>%
    filter(.data$model == "batch_absorbed") %>%
    mutate(family_id = paste0("RFID_FIRSTNIGHT_", sub("_based$", "", bin_level),
                              "__GROUP_X_SEX_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(),
                           paste0("RFID_FIRSTNIGHT__GROUP_X_SEX_n", dplyr::n())),
           sex_differential_supported = is.finite(.data$q) & .data$q < 0.05,
           family_role = "INTERACTION")

  lobo <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_leave_one_batch_out(
        scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  })

  # Contrast identification + minimum detectable effect. Without these a null is
  # uninterpretable: the two CON contrasts are BETWEEN-cage on 6 control cages and
  # can only detect very large effects, while SUS-RES is within-cage.
  sd_by <- scored %>%
    filter(is.finite(.data$DomainScore)) %>%
    group_by(.data$Domain, .data$Sex) %>%
    summarise(sd_domain = stats::sd(.data$DomainScore), .groups = "drop")
  pairwise_annotated <- hier$pairwise %>%
    left_join(sd_by, by = c("Domain", "Sex")) %>%
    mutate(contrast_identification =
             unname(MMM_RFID_CONTRAST_IDENTIFICATION[.data$contrast]),
           mde_hedges_g = mmm_rfid_mde(.data$SE, .data$df, .data$sd_domain),
           power_note = paste(
             "mde_hedges_g is the smallest effect detectable at 80% power, alpha .05,",
             "under THIS contrast's actual standard error and df. A non-significant",
             "contrast whose MDE is large is uninformative, not evidence of absence."))

  # Within-sex ordered trend, alongside the pooled version.
  trend_within <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_ordered_trend(scored %>% filter(.data$Sex == sx, .data$Domain == dm),
                             dm, sx)
    })
  }) %>%
    group_by(.data$stratum) %>%
    mutate(family_id = paste0("RFID_FIRSTNIGHT__", .data$stratum, "__ORDERED_TREND_n",
                              dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(), dplyr::first(.data$family_id))) %>%
    ungroup()

  # Predeclared pooling gate: pool sexes only if no interaction survives.
  pooled <- mmm_rfid_pooled_sex_analysis(scored, interaction_primary)

  # ORTHOGONAL DECOMPOSITION of the 2 df of Group into the two questions that are
  # actually asked: the manipulation (between-cage) and the phenotype (within-cage).
  orth_pooled <- mmm_rfid_orthogonal_family(
    scored, "pooled", paste0("RFID_FIRSTNIGHT_", sub("_based$", "", bin_level)))
  orth_by_sex <- purrr::map_dfr(sexes, function(sx) {
    mmm_rfid_orthogonal_family(scored %>% filter(.data$Sex == sx), sx,
                               paste0("RFID_FIRSTNIGHT_", sub("_based$", "", bin_level)))
  })

  list(features = fn, standardization = std$parameters, scores = scored,
       pairwise_annotated = pairwise_annotated, trend_within = trend_within,
       pooled = pooled, orthogonal_pooled = orth_pooled, orthogonal_by_sex = orth_by_sex,
       sensitivity_scores = sens, omnibus = hier$omnibus, omnibus_all_models = omnibus,
       pairwise = hier$pairwise, interaction = interaction_primary,
       interaction_all_models = interaction_tests, interaction_aliasing = interaction_alias,
       leave_one_batch_out = lobo)
}

# ------------------------------------------------------- longitudinal

run_longitudinal <- function(feat, bin_level) {
  cc1 <- feat %>% filter(.data$CageChangeLabel == "CC1")
  params_cc1 <- mmm_rfid_scaling_parameters(
    cc1, reference_label = paste0("CC1_acute_within_Sex_FIXED__", bin_level))
  params_pooled <- mmm_rfid_scaling_parameters(
    feat, reference_label = paste0("pooled_CC1toCC4_within_Sex__", bin_level))

  score_with <- function(params, label) {
    z <- mmm_rfid_apply_scaling(feat, params)
    mmm_rfid_score_all_domains(
      z, id_cols = c("AnimalNum", "Group", "Sex", "Batch", "System", "SourceFile",
                     "CageEpochID", "CageChangeLabel", "CageChangeIndex")) %>%
      mutate(bin_level = bin_level, scaling_variant = label,
             interpretation_guard = MMM_RFID_DESCRIPTIVE_GUARD,
             structural_confound = MMM_RFID_LONGITUDINAL_CONFOUND_GUARD)
  }
  scored <- score_with(params_cc1, "PRIMARY_CC1_reference")
  scored_pooled <- score_with(params_pooled, "SENSITIVITY_pooled_CC1toCC4")

  domains <- MMM_RFID_CORE_DOMAINS
  sexes <- intersect(MMM_RFID_SEX_LEVELS, unique(as.character(scored$Sex)))

  traj <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_trajectory_omnibus(
        scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  })
  traj_primary <- traj %>%
    filter(.data$model == "primary") %>%
    group_by(.data$Sex) %>%
    mutate(family_id = paste0("RFID_LONGITUDINAL_", sub("_based$", "", bin_level),
                              "__", .data$Sex, "__GROUPxCC_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(), dplyr::first(.data$family_id))) %>%
    ungroup() %>%
    mutate(trajectory_supported = is.finite(.data$q) & .data$q < 0.05,
           family_role = "PRIMARY_TRAJECTORY")

  emm <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_trajectory_emmeans(
        scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  })

  follow <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      f <- mmm_rfid_trajectory_followups(
        scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
      if (nrow(f) == 0L) return(f)
      sup <- traj_primary %>%
        filter(.data$Sex == sx, .data$Domain == dm) %>%
        pull("trajectory_supported")
      supported <- length(sup) == 1L && isTRUE(sup)
      f %>% mutate(
        parent_supported = supported,
        followup_role = if (supported) "gated_followup_inference" else "descriptive_non_gated",
        p_holm_within_domain = if (supported) {
          mmm_rfid_holm(.data$p.value, length(MMM_RFID_TRAJECTORY_FOLLOWUPS))
        } else rep(NA_real_, dplyr::n()))
    })
  })

  lin <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_trajectory_linear_trend(
        scored %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  })

  # Predeclared within-animal change scores: a 2-df focused test of the same
  # adaptation question the 6-df interaction asks, but far better powered.
  change <- purrr::map_dfr(c("pooled", sexes), function(st) {
    dplyr::bind_rows(
      mmm_rfid_change_score_analysis(scored, c("CC3", "CC4"), "CC1", st,
                                     "mean(CC3,CC4) - CC1"),
      mmm_rfid_change_score_analysis(scored, "CC4", "CC1", st, "CC4 - CC1"),
      mmm_rfid_change_score_analysis(scored, "CC2", "CC1", st, "CC2 - CC1"))
  }) %>%
    group_by(.data$stratum, .data$change_measure) %>%
    mutate(family_id = paste0("RFID_CHANGE__", .data$stratum, "__",
                              gsub("[^A-Za-z0-9]+", "", .data$change_measure),
                              "_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q_cage_adjusted = mmm_rfid_bh(.data$p_cage_adjusted, dplyr::n(),
                                         dplyr::first(.data$family_id)),
           q_naive_no_cage = mmm_rfid_bh(.data$p_naive_no_cage, dplyr::n(),
                                         dplyr::first(.data$family_id))) %>%
    ungroup() %>%
    # ALSO correct across EVERY change test actually run within a stratum. The
    # per-measure family (4 domains) is only defensible if a single change measure
    # was declared; three were run, so the wider family is the honest default and
    # both are shipped rather than the smaller one being quoted alone.
    group_by(.data$stratum) %>%
    mutate(wide_family_id = paste0("RFID_CHANGE_ALLMEASURES__", .data$stratum,
                                   "_n", dplyr::n()),
           n_tests_in_wide_family = dplyr::n(),
           q_cage_adjusted_wide_family =
             mmm_rfid_bh(.data$p_cage_adjusted, dplyr::n(),
                         dplyr::first(.data$wide_family_id)),
           family_choice_note = paste(
             "TWO declarations are shipped. q_cage_adjusted corrects across the 4",
             "domains WITHIN one predeclared change measure; ",
             "q_cage_adjusted_wide_family corrects across every change test run in",
             "this stratum (3 measures x 4 domains). The wide family is the more",
             "defensible default because all three measures were run; quote the",
             "narrow one only if a single measure is declared in advance.")) %>%
    ungroup()

  # Per-cage-change and early/late decomposition of the two orthogonal contrasts.
  winc <- dplyr::bind_rows(
    mmm_rfid_window_contrasts(scored, "pooled"),
    purrr::map_dfr(sexes, function(sx) mmm_rfid_window_contrasts(
      scored %>% filter(.data$Sex == sx), sx))) %>%
    mmm_rfid_exploratory_path_accounting()
  orth_long <- dplyr::bind_rows(
    mmm_rfid_orthogonal_family(scored, "pooled",
      paste0("RFID_LONGITUDINAL_", sub("_based$", "", bin_level)),
      include_animal_re = TRUE, cagechange_term = TRUE),
    purrr::map_dfr(sexes, function(sx) mmm_rfid_orthogonal_family(
      scored %>% filter(.data$Sex == sx), sx,
      paste0("RFID_LONGITUDINAL_", sub("_based$", "", bin_level)),
      include_animal_re = TRUE, cagechange_term = TRUE)))

  three <- purrr::map(domains, function(dm) {
    mmm_rfid_three_way_interaction(scored %>% filter(.data$Domain == dm), dm)
  })
  three_tests <- purrr::map_dfr(three, "test")
  three_alias <- purrr::map_dfr(three, "aliasing")
  three_primary <- three_tests %>%
    filter(.data$model == "batch_absorbed") %>%
    mutate(family_id = paste0("RFID_LONGITUDINAL_", sub("_based$", "", bin_level),
                              "__GROUPxSEXxCC_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(),
                           paste0("RFID_LONGITUDINAL__GROUPxSEXxCC_n", dplyr::n())),
           three_way_supported = is.finite(.data$q) & .data$q < 0.05,
           family_role = "THREE_WAY_INTERACTION")

  # Scaling sensitivity: does the trajectory conclusion survive pooled scaling?
  traj_pooled <- purrr::map_dfr(sexes, function(sx) {
    purrr::map_dfr(domains, function(dm) {
      mmm_rfid_trajectory_omnibus(
        scored_pooled %>% filter(.data$Sex == sx, .data$Domain == dm), dm, sx)
    })
  }) %>% filter(.data$model == "primary") %>% mutate(scaling_variant = "SENSITIVITY_pooled")

  list(scaling_cc1 = params_cc1, scaling_pooled = params_pooled,
       scores = scored, scores_pooled = scored_pooled,
       trajectory = traj_primary, trajectory_all_models = traj, change_scores = change,
       window_contrasts = winc, orthogonal = orth_long,
       trajectory_pooled = traj_pooled, emmeans = emm, followups = follow,
       linear_trend = lin, three_way = three_primary,
       three_way_all_models = three_tests, three_way_aliasing = three_alias)
}

# --------------------------------------------------------------- run

message("Stage 28: four core raw-RFID behavioural domains")
message("  project root: ", PROJECT_ROOT)

results <- list()
for (bl in c(PRIMARY_BIN, SENSITIVITY_BIN)) {
  role <- if (identical(bl, PRIMARY_BIN)) "primary" else "sensitivity"
  message("  -- ", bl, " (", role, ")")
  dat <- load_stage01(bl)
  acute <- build_acute_features(dat, bl)
  fnres <- run_first_night(acute$features, bl, role)
  longres <- run_longitudinal(acute$features, bl)

  out <- stage_out(bl)
  w <- function(x, nm) {
    if (is.null(x) || nrow(x) == 0L) return(invisible(NULL))
    readr::write_csv(x, file.path(out, nm)); invisible(nm)
  }
  w(mmm_rfid_domain_definition_table(), "rfid_domain_definitions.csv")
  w(acute$validity, "acute_window_session_validity.csv")
  w(acute$qc, "acute_window_provenance_by_animal_cagechange.csv")
  w(acute$features, "acute_window_raw_features.csv")
  cs <- mmm_rfid_cage_structure_summary(acute$qc)
  w(cs$by_cage_change, "cage_structure_by_cagechange.csv")
  w(cs$by_cage, "cage_structure_by_cage.csv")

  w(fnres$standardization, "first_night_scaling_parameters.csv")
  w(fnres$scores, "first_night_domain_scores.csv")
  w(fnres$sensitivity_scores, "first_night_equal_weight_sensitivity_scores.csv")
  w(fnres$omnibus, "first_night_domain_omnibus_primary.csv")
  w(fnres$omnibus_all_models, "first_night_domain_omnibus_all_models.csv")
  w(fnres$pairwise, "first_night_pairwise_contrasts.csv")
  w(fnres$pairwise_annotated, "first_night_pairwise_contrasts_with_power.csv")
  w(fnres$trend_within, "first_night_ordered_trend_within_sex.csv")
  w(fnres$pooled$gate, "first_night_sex_pooling_gate.csv")
  w(fnres$pooled$omnibus, "first_night_pooled_sex_omnibus.csv")
  w(fnres$pooled$omnibus_all_models, "first_night_pooled_sex_omnibus_all_models.csv")
  w(fnres$pooled$ordered_trend, "first_night_pooled_sex_ordered_trend.csv")
  w(fnres$pooled$pairwise, "first_night_pooled_sex_pairwise.csv")
  w(fnres$orthogonal_pooled, "first_night_orthogonal_contrasts_pooled.csv")
  w(fnres$orthogonal_by_sex, "first_night_orthogonal_contrasts_by_sex.csv")
  w(fnres$interaction, "first_night_group_sex_interaction.csv")
  w(fnres$interaction_all_models, "first_night_group_sex_interaction_all_models.csv")
  w(fnres$interaction_aliasing, "first_night_model_aliasing_audit.csv")
  w(fnres$leave_one_batch_out, "first_night_leave_one_batch_out.csv")

  w(longres$scaling_cc1, "longitudinal_scaling_parameters_cc1_reference.csv")
  w(longres$scaling_pooled, "longitudinal_scaling_parameters_pooled_sensitivity.csv")
  w(longres$scores, "longitudinal_domain_scores.csv")
  w(longres$trajectory, "longitudinal_trajectory_omnibus_primary.csv")
  w(longres$trajectory_all_models, "longitudinal_trajectory_omnibus_all_models.csv")
  w(longres$trajectory_pooled, "longitudinal_trajectory_pooled_scaling_sensitivity.csv")
  w(longres$emmeans, "longitudinal_group_cagechange_emmeans.csv")
  w(longres$followups, "longitudinal_predeclared_followups.csv")
  w(longres$linear_trend, "longitudinal_linear_trend_sensitivity.csv")
  w(longres$change_scores, "longitudinal_change_score_predeclared.csv")
  w(longres$window_contrasts, "longitudinal_window_contrasts_by_cagechange.csv")
  w(longres$orthogonal, "longitudinal_orthogonal_contrasts.csv")
  w(longres$three_way, "longitudinal_three_way_interaction.csv")
  w(longres$three_way_all_models, "longitudinal_three_way_all_models.csv")
  w(longres$three_way_aliasing, "longitudinal_model_aliasing_audit.csv")

  # Multiplicity contract, explicit and machine-readable.
  fam <- dplyr::bind_rows(
    fnres$omnibus %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
      mutate(family_role = "FIRST_NIGHT_PRIMARY",
             scope = "one Group omnibus test per CORE domain, within Sex", method = "BH"),
    fnres$interaction %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
      mutate(family_role = "FIRST_NIGHT_INTERACTION",
             scope = "one Group:Sex test per CORE domain", method = "BH"),
    longres$trajectory %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
      mutate(family_role = "LONGITUDINAL_TRAJECTORY",
             scope = "one Group:CageChange test per CORE domain, within Sex", method = "BH"),
    longres$three_way %>% distinct(.data$family_id, .data$n_tests_in_family) %>%
      mutate(family_role = "LONGITUDINAL_THREE_WAY",
             scope = "one Group:Sex:CageChange test per CORE domain", method = "BH")
  ) %>% mutate(
    bin_level = bl,
    gated_followup = paste(
      "Pairwise / trajectory follow-ups are corrected with Holm WITHIN a domain",
      "whose parent omnibus survives its BH family; follow-ups under an unsupported",
      "parent are exported as descriptive effect sizes only."))
  w(fam, "multiplicity_contract.csv")

  results[[bl]] <- list(acute = acute, first_night = fnres, longitudinal = longres,
                        output_dir = out)
  message("     wrote ", length(list.files(out, pattern = "csv$")), " tables to ", out)
}

mmm_frozen_guard_after(frozen_guard_snapshot)
message("Stage 28 complete.")
