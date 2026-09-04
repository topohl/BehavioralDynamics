# ================================================================================
# AUDIT (read-only) -- deliverable 4: LONGITUDINAL follow-up for the Stage 14
# construct `Behavioral state architecture` and its implicated components.
#
# Testing/audits/audit_hmm_state_architecture_longitudinal.R
#
# The PRIMARY average-effect model
#     DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)
# is valid and STAYS. Nothing here replaces it. This script only asks WHEN across
# CC1-CC4 any group separation lives: present from CC1, persistent, increasing,
# habituating, or concentrated in a single regrouping.
#
# Four model families per outcome x resolution x PhaseClass:
#   interaction_mixed    value ~ Group * Sex * factor(CageChangeIndex) + (1|AnimalNum)
#   stratified_lm        value ~ Group * Sex        (ONE fit per CageChangeIndex;
#                        within a single CC each animal contributes exactly ONE
#                        observation per phase, so an animal random intercept is
#                        NOT identifiable -- these are ordinary least squares fits
#                        and are labelled as such. They are NOT mixed models.)
#   linear_trend_mixed   value ~ Group * Sex * CageChangeIndexNum + (1|AnimalNum)
#   average_effect_mixed the SHIPPED estimator, via
#                        fit_repeated_measures_domain_contrasts() unchanged
#
# INTERPRETIVE CAVEAT (handled explicitly, see `scaling` column):
#   The shipped composite and every *_z component are z-scored WITHIN
#   Sex x PhaseClass x CageChangeIndex. Each cage change is therefore centred to
#   mean 0 BY CONSTRUCTION, so a longitudinal analysis of context-z values can
#   only test whether the between-group SPREAD changes across cage changes; it is
#   mathematically incapable of showing absolute level drift. Therefore the
#   RAW-UNIT components are the PRIMARY longitudinal evidence for level changes
#   (scaling = "raw_units"), and the context-z version is reported as the
#   composite-consistent spread version (scaling = "context_z_sex_phase_cc").
#   The historical composite has NO raw-unit equivalent (it is defined in z
#   units). As a clearly-labelled sensitivity we additionally recompute it with a
#   SINGLE standardization per resolution x Sex x PhaseClass (pooling cage
#   changes) so that absolute CC drift becomes visible
#   (scaling = "sensitivity_pooled_z_sex_phase"). That is a re-standardization
#   sensitivity, NOT the manuscript composite.
#
# Terminology guard: RFID "Proximity" is a social-spatial co-location proxy, not
# measured sociability. top_proximity_state_fraction is the label-free argmax
# Proximity_z state's occupancy. No state is called "social".
#
# FDR guard: the primary heatmap family (18 tests within resolution x Sex x
# Phase) is NOT redefined. Every family created here is prefixed
# LONGITUDINAL_AUDIT_ONLY__ and is reported next to the raw p.
#
# Writes ONLY into the Stage 14 audit folder. Modifies nothing under Analysis/
# or Functions/.
# ================================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(readr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
})

options(width = 220)
emm_options(lmer.df = "satterthwaite", msg.interaction = FALSE, msg.nesting = FALSE)

# --------------------------------------------------------------------------------
# 0. Repo + paths
# --------------------------------------------------------------------------------
repo_candidates <- c(
  "MMMSociability/Analysis/_pipeline_setup.R",
  "Analysis/_pipeline_setup.R",
  "C:/Users/topohl/Documents/GitHub/MMMSociability/Analysis/_pipeline_setup.R"
)
setup_path <- repo_candidates[file.exists(repo_candidates)][1]
if (is.na(setup_path)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(setup_path)
source_mmm_helper("hmm_stage14_helpers.R")

project_root <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
audit_out <- file.path(
  project_root,
  "analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture"
)
ensure_dir(audit_out)

resolutions <- c("5min_based", "10min_based")
roster_bin_level <- "5min_based"

cat("================================================================\n")
cat("AUDIT deliverable 4: LONGITUDINAL (cage-change) follow-up\n")
cat("audit out  :", audit_out, "\n")
cat("================================================================\n\n")

# --------------------------------------------------------------------------------
# 1. Canonical 111-animal roster, exactly as Stage 08 derives it
# --------------------------------------------------------------------------------
canonical_roster_file <- file.path(
  project_root, "analysis_ready/03_derived_metrics", roster_bin_level, "all_behavior_metrics.csv"
)
if (!file.exists(canonical_roster_file)) stop("Missing roster input: ", canonical_roster_file)
canonical_roster <- build_canonical_identity_roster(
  readr::read_csv(
    canonical_roster_file,
    col_types = readr::cols(
      .default = readr::col_skip(),
      AnimalNum = readr::col_character(),
      Group = readr::col_character(),
      Sex = readr::col_character()
    ),
    progress = FALSE
  ),
  paste0("Stage 01 ", roster_bin_level, " roster")
)
stopifnot(nrow(canonical_roster) == 111L)
cat("[1] canonical roster animals:", nrow(canonical_roster), "\n")
print(as.data.frame(canonical_roster %>% count(Sex, Group, name = "n_animals")))
cat("\n")

# --------------------------------------------------------------------------------
# 2. Foundation table (deliverable 1 of this audit), identity-audited
# --------------------------------------------------------------------------------
comp_path <- file.path(audit_out, "hmm_architecture_component_epoch_metrics.csv")
if (!file.exists(comp_path)) stop("Foundation table missing: ", comp_path, call. = FALSE)
components_raw <- readr::read_csv(
  comp_path,
  col_types = readr::cols(AnimalNum = readr::col_character(), .default = readr::col_guess()),
  progress = FALSE, show_col_types = FALSE
)
foundation_audit <- audit_hmm_identity(
  components_raw, canonical_roster, "audit foundation component table"
)
assert_hmm_identity_audit(foundation_audit)
cat("[2] foundation identity audit:\n")
print(as.data.frame(foundation_audit$summary %>% select(
  source, input_rows, raw_animal_spellings, canonical_animals, aliases_merged,
  identity_conflicts, unknown_animals, metadata_disagreements, passed
)))
components <- foundation_audit$data %>%
  mutate(
    AnimalNum = canonical_animal_id(.data$AnimalNum),
    CageChangeIndex = as.integer(.data$CageChangeIndex),
    PhaseClass = as.character(.data$PhaseClass),
    Sex = as.character(.data$Sex),
    Group = as.character(.data$Group),
    resolution = as.character(.data$resolution)
  )
cat("    rows:", nrow(components), " cols:", ncol(components),
  " animals:", n_distinct(components$AnimalNum), "\n")
print(as.data.frame(components %>% count(resolution, PhaseClass, name = "n_epochs")))
cat("\n")

# --- sanity: the foundation composite still equals its closed form --------------
repro <- components %>%
  group_by(resolution) %>%
  summarise(
    max_abs_diff_composite_vs_closed_form =
      max(abs(`Behavioral state architecture` - (0.5 * occupancy_entropy_z - inactive_state_fraction_z))),
    .groups = "drop"
  )
cat("[2b] composite closed-form re-check (CSV round-trip, expect ~1e-16):\n")
print(as.data.frame(repro))
cat("\n")

# --------------------------------------------------------------------------------
# 3. SENSITIVITY re-standardization: pool cage changes
#    z within resolution x Sex x PhaseClass ONLY (CageChangeIndex dropped from the
#    context), so absolute across-CC level drift is no longer removed by
#    construction. Labelled sensitivity_pooled_z_sex_phase everywhere.
# --------------------------------------------------------------------------------
pool_ctx <- c("resolution", "Sex", "PhaseClass")
components <- components %>%
  mutate(
    pool_occ = occupancy_entropy,
    pool_inact = inactive_state_fraction,
    pool_soc = social_state_fraction
  ) %>%
  strict_standardize_within_context("pool_occ", group_cols = pool_ctx) %>%
  strict_standardize_within_context("pool_inact", group_cols = pool_ctx) %>%
  strict_standardize_within_context("pool_soc", group_cols = pool_ctx) %>%
  mutate(
    composite_pooled_sex_phase_z =
      rowMeans(cbind(pool_occ_z, pool_soc_z), na.rm = FALSE) - pool_inact_z
  )

cat("[3] sensitivity re-standardization built. Context comparison:\n")
print(as.data.frame(
  components %>%
    group_by(resolution, Sex, PhaseClass, CageChangeIndex) %>%
    summarise(
      mean_context_z_composite = mean(`Behavioral state architecture`),
      mean_pooled_z_composite = mean(composite_pooled_sex_phase_z),
      .groups = "drop"
    )
))
cat("  -> mean of the context-z composite is 0 in EVERY CC cell by construction;\n")
cat("     the pooled version is free to drift across CC.\n\n")

# --------------------------------------------------------------------------------
# 4. Outcome specification
# --------------------------------------------------------------------------------
outcome_spec <- tribble(
  ~outcome, ~scaling, ~value_col,
  "composite_behavioral_state_architecture", "context_z_sex_phase_cc", "Behavioral state architecture",
  "composite_behavioral_state_architecture", "sensitivity_pooled_z_sex_phase", "composite_pooled_sex_phase_z",
  "occupancy_entropy", "raw_units", "occupancy_entropy",
  "occupancy_entropy", "context_z_sex_phase_cc", "occupancy_entropy_z",
  "inactive_state_fraction", "raw_units", "inactive_state_fraction",
  "inactive_state_fraction", "context_z_sex_phase_cc", "inactive_state_fraction_z",
  "top_proximity_state_fraction", "raw_units", "top_proximity_state_fraction",
  "top_proximity_state_fraction", "context_z_sex_phase_cc", "top_proximity_state_fraction_z",
  "state_switch_rate", "raw_units", "state_switch_rate",
  "state_switch_rate", "context_z_sex_phase_cc", "state_switch_rate_z"
)

scaling_note <- c(
  raw_units = paste(
    "PRIMARY longitudinal evidence for LEVEL change. Raw component units, no",
    "standardization, so absolute across-cage-change drift is visible."
  ),
  context_z_sex_phase_cc = paste(
    "Composite-consistent SPREAD version. z within Sex x PhaseClass x",
    "CageChangeIndex: each cage change is centred to mean 0 BY CONSTRUCTION, so",
    "this tests whether the between-group SPREAD changes across cage changes,",
    "NOT whether absolute levels drift."
  ),
  sensitivity_pooled_z_sex_phase = paste(
    "SENSITIVITY re-standardization only, NOT the manuscript composite. Single z",
    "per resolution x Sex x PhaseClass, pooling CC1-CC4, so absolute CC drift of",
    "the composite becomes visible. The historical composite has no raw-unit",
    "equivalent because it is defined in z units."
  )
)

group_levels <- c("CON", "RES", "SUS")
sex_levels <- c("Female", "Male")
contrast_vectors <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))

# --------------------------------------------------------------------------------
# 5. helpers (local to this audit; the shipped estimator is NOT re-implemented)
# --------------------------------------------------------------------------------
# Captures BOTH warnings and messages. lme4 emits "boundary (singular) fit" as a
# message(), not a warning(), so a warning-only handler would silently drop the
# single most important diagnostic. Messages are prefixed so the two are
# distinguishable in the CSV.
with_warnings <- function(expr) {
  w <- character()
  val <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    warning = function(x) {
      w <<- c(w, paste0("warning: ", conditionMessage(x)))
      invokeRestart("muffleWarning")
    },
    message = function(x) {
      w <<- c(w, paste0("message: ", trimws(conditionMessage(x))))
      invokeRestart("muffleMessage")
    }
  )
  list(value = val, warnings = unique(w))
}

# Hedges g between two groups of ANIMAL-level values, via the repo helper.
g_between <- function(dat, sex, ref, comp) {
  r <- dat$value[dat$Sex == sex & dat$Group == ref]
  c_ <- dat$value[dat$Sex == sex & dat$Group == comp]
  list(
    n_ref = sum(is.finite(r)), n_comp = sum(is.finite(c_)),
    mean_ref = if (any(is.finite(r))) mean(r[is.finite(r)]) else NA_real_,
    mean_comp = if (any(is.finite(c_))) mean(c_[is.finite(c_)]) else NA_real_,
    g = hmm_hedges_g(r, c_)
  )
}

cell_counts <- function(dat) {
  dat %>%
    filter(is.finite(value)) %>%
    distinct(Sex, CageChangeIndex, Group, AnimalNum) %>%
    count(Sex, CageChangeIndex, Group, name = "n") %>%
    mutate(Sex = as.character(Sex), Group = as.character(Group)) %>%
    pivot_wider(
      names_from = Group, values_from = n, names_prefix = "n_cell_",
      values_fill = 0L
    ) %>%
    mutate(across(starts_with("n_cell_"), as.integer))
}

empty_cols <- tibble(
  estimate = NA_real_, SE = NA_real_, df = NA_real_,
  t_ratio = NA_real_, p_value = NA_real_
)

# --------------------------------------------------------------------------------
# 6. Fit everything
# --------------------------------------------------------------------------------
all_rows <- list()
omnibus_rows <- list()
diag_rows <- list()
cellcount_rows <- list()

grid <- tidyr::crossing(
  resolution = resolutions,
  PhaseClass = c("Active", "Inactive"),
  spec_row = seq_len(nrow(outcome_spec))
)

for (i in seq_len(nrow(grid))) {
  res <- grid$resolution[i]
  phase <- grid$PhaseClass[i]
  sp <- outcome_spec[grid$spec_row[i], ]
  outcome <- sp$outcome
  scaling <- sp$scaling
  vcol <- sp$value_col
  tag <- paste(outcome, scaling, res, phase, sep = " | ")

  dat <- components %>%
    filter(resolution == res, PhaseClass == phase) %>%
    transmute(
      AnimalNum = factor(as.character(AnimalNum)),
      Group = factor(as.character(Group), levels = group_levels),
      Sex = factor(as.character(Sex), levels = sex_levels),
      CageChangeIndex = as.integer(CageChangeIndex),
      CCf = factor(CageChangeIndex),
      CageChangeIndexNum = as.numeric(CageChangeIndex),
      PhaseClass = phase,
      value = suppressWarnings(as.numeric(.data[[vcol]]))
    ) %>%
    filter(is.finite(value), !is.na(Group), !is.na(Sex), !is.na(CageChangeIndex))

  cc_levels <- sort(unique(dat$CageChangeIndex))
  cells <- cell_counts(dat)
  cellcount_rows[[length(cellcount_rows) + 1L]] <- cells %>%
    mutate(
      resolution = res, PhaseClass = phase, outcome = outcome, scaling = scaling,
      .before = 1
    )

  n_obs <- nrow(dat)
  n_animals <- n_distinct(dat$AnimalNum)

  # ---------------- (A) interaction_mixed --------------------------------------
  f_int <- "value ~ Group * Sex * factor(CageChangeIndex) + (1 | AnimalNum)  [CCf = factor(CageChangeIndex)]"
  fit_int_w <- with_warnings(lmerTest::lmer(value ~ Group * Sex * CCf + (1 | AnimalNum), data = dat))
  fit_int <- fit_int_w$value
  int_warn <- paste(fit_int_w$warnings, collapse = " | ")
  if (inherits(fit_int, "error")) {
    int_status <- paste0("error: ", conditionMessage(fit_int))
    int_singular <- NA
    n_fixed_int <- NA_integer_
  } else {
    int_singular <- lme4::isSingular(fit_int, tol = 1e-4)
    int_status <- if (isTRUE(int_singular)) "singular_fit" else "fitted"
    n_fixed_int <- length(lme4::fixef(fit_int))
  }

  int_rows <- NULL
  if (!inherits(fit_int, "error")) {
    emm_w <- with_warnings(emmeans::emmeans(fit_int, ~ Group | Sex * CCf))
    if (!inherits(emm_w$value, "error")) {
      ct <- with_warnings(
        as.data.frame(emmeans::contrast(emm_w$value, method = contrast_vectors, adjust = "none"))
      )
      if (!inherits(ct$value, "error")) {
        int_rows <- ct$value %>%
          as_tibble() %>%
          transmute(
            Sex = as.character(Sex),
            CageChangeIndex_label = as.character(CCf),
            contrast = as.character(contrast),
            estimate, SE, df, t_ratio = t.ratio, p_value = p.value
          )
      }
      int_warn <- paste(unique(c(fit_int_w$warnings, emm_w$warnings, ct$warnings)), collapse = " | ")
    }
    # omnibus Satterthwaite F tests
    an <- with_warnings(as.data.frame(anova(fit_int)))
    if (!inherits(an$value, "error")) {
      av <- an$value
      keep <- intersect(
        c("Group", "Sex", "CCf", "Group:Sex", "Group:CCf", "Sex:CCf", "Group:Sex:CCf"),
        rownames(av)
      )
      omnibus_rows[[length(omnibus_rows) + 1L]] <- tibble(
        resolution = res, PhaseClass = phase, outcome = outcome, scaling = scaling,
        model_type = "interaction_mixed",
        term_as_fitted = keep,
        term = recode(keep,
          "CCf" = "factor(CageChangeIndex)",
          "Group:CCf" = "Group:factor(CageChangeIndex)",
          "Sex:CCf" = "Sex:factor(CageChangeIndex)",
          "Group:Sex:CCf" = "Group:Sex:factor(CageChangeIndex)"
        ),
        NumDF = av[keep, "NumDF"], DenDF = av[keep, "DenDF"],
        F_value = av[keep, "F value"], p_value = av[keep, "Pr(>F)"],
        n_obs = n_obs, n_animals = n_animals, n_fixed_params = n_fixed_int,
        model_status = int_status, model_warnings = int_warn
      )
    }
  }
  if (is.null(int_rows)) {
    int_rows <- tidyr::crossing(
      Sex = sex_levels, CageChangeIndex_label = as.character(cc_levels),
      contrast = names(contrast_vectors)
    )
    int_rows <- bind_cols(int_rows, empty_cols[rep(1, nrow(int_rows)), ])
  }
  int_rows <- int_rows %>%
    mutate(
      model_type = "interaction_mixed",
      model_engine = "lmerTest::lmer + emmeans (~Group | Sex * CageChangeIndex)",
      model_formula = f_int, model_status = int_status, model_warnings = int_warn,
      n_obs_model = n_obs, n_animals_model = n_animals, n_fixed_params = n_fixed_int,
      is_singular = int_singular
    )

  # ---------------- (B) stratified_lm (per CageChangeIndex) --------------------
  strat_rows <- map_dfr(cc_levels, function(cc) {
    d_cc <- dat %>% filter(CageChangeIndex == cc)
    obs_per_animal <- d_cc %>% count(AnimalNum) %>% pull(n)
    f_lm <- "value ~ Group * Sex   (OLS, one observation per animal within this CageChangeIndex)"
    fw <- with_warnings(lm(value ~ Group * Sex, data = d_cc))
    if (inherits(fw$value, "error")) {
      out <- tidyr::crossing(Sex = sex_levels, contrast = names(contrast_vectors))
      out <- bind_cols(out, empty_cols[rep(1, nrow(out)), ]) %>%
        mutate(model_status = paste0("error: ", conditionMessage(fw$value)))
    } else {
      emm_w <- with_warnings(emmeans::emmeans(fw$value, ~ Group | Sex))
      ct <- with_warnings(as.data.frame(emmeans::contrast(emm_w$value,
        method = contrast_vectors, adjust = "none"
      )))
      out <- ct$value %>%
        as_tibble() %>%
        transmute(
          Sex = as.character(Sex), contrast = as.character(contrast),
          estimate, SE, df, t_ratio = t.ratio, p_value = p.value
        ) %>%
        mutate(model_status = "fitted")
      fw$warnings <- unique(c(fw$warnings, emm_w$warnings, ct$warnings))
    }
    out %>%
      mutate(
        CageChangeIndex_label = as.character(cc),
        model_type = "stratified_lm",
        model_engine = "stats::lm + emmeans (OLS; animal random intercept NOT identifiable within one cage change)",
        model_formula = f_lm,
        model_warnings = paste(fw$warnings, collapse = " | "),
        n_obs_model = nrow(d_cc), n_animals_model = n_distinct(d_cc$AnimalNum),
        n_fixed_params = if (inherits(fw$value, "error")) NA_integer_ else length(coef(fw$value)),
        is_singular = NA,
        max_obs_per_animal_in_stratum = max(obs_per_animal)
      )
  })
  stopifnot(all(strat_rows$max_obs_per_animal_in_stratum == 1L))
  strat_rows <- strat_rows %>% select(-max_obs_per_animal_in_stratum)

  # ---------------- (C) linear_trend_mixed ------------------------------------
  f_tr <- "value ~ Group * Sex * CageChangeIndexNum + (1 | AnimalNum)"
  fit_tr_w <- with_warnings(lmerTest::lmer(
    value ~ Group * Sex * CageChangeIndexNum + (1 | AnimalNum),
    data = dat
  ))
  fit_tr <- fit_tr_w$value
  tr_warn <- paste(fit_tr_w$warnings, collapse = " | ")
  if (inherits(fit_tr, "error")) {
    tr_status <- paste0("error: ", conditionMessage(fit_tr))
    tr_sing <- NA
    n_fixed_tr <- NA_integer_
    trend_rows <- tidyr::crossing(Sex = sex_levels, contrast = names(contrast_vectors))
    trend_rows <- bind_cols(trend_rows, empty_cols[rep(1, nrow(trend_rows)), ])
  } else {
    tr_sing <- lme4::isSingular(fit_tr, tol = 1e-4)
    tr_status <- if (isTRUE(tr_sing)) "singular_fit" else "fitted"
    n_fixed_tr <- length(lme4::fixef(fit_tr))
    et <- with_warnings(emmeans::emtrends(fit_tr, ~ Group | Sex, var = "CageChangeIndexNum"))
    ct <- with_warnings(as.data.frame(emmeans::contrast(et$value,
      method = contrast_vectors,
      adjust = "none"
    )))
    if (inherits(ct$value, "error")) {
      trend_rows <- tidyr::crossing(Sex = sex_levels, contrast = names(contrast_vectors))
      trend_rows <- bind_cols(trend_rows, empty_cols[rep(1, nrow(trend_rows)), ])
    } else {
      trend_rows <- ct$value %>%
        as_tibble() %>%
        transmute(
          Sex = as.character(Sex), contrast = as.character(contrast),
          estimate, SE, df, t_ratio = t.ratio, p_value = p.value
        )
    }
    tr_warn <- paste(unique(c(fit_tr_w$warnings, et$warnings, ct$warnings)), collapse = " | ")
    an_tr <- with_warnings(as.data.frame(anova(fit_tr)))
    if (!inherits(an_tr$value, "error")) {
      av <- an_tr$value
      keep <- intersect(
        c(
          "Group", "Sex", "CageChangeIndexNum", "Group:Sex", "Group:CageChangeIndexNum",
          "Sex:CageChangeIndexNum", "Group:Sex:CageChangeIndexNum"
        ),
        rownames(av)
      )
      omnibus_rows[[length(omnibus_rows) + 1L]] <- tibble(
        resolution = res, PhaseClass = phase, outcome = outcome, scaling = scaling,
        model_type = "linear_trend_mixed", term_as_fitted = keep, term = keep,
        NumDF = av[keep, "NumDF"], DenDF = av[keep, "DenDF"],
        F_value = av[keep, "F value"], p_value = av[keep, "Pr(>F)"],
        n_obs = n_obs, n_animals = n_animals, n_fixed_params = n_fixed_tr,
        model_status = tr_status, model_warnings = tr_warn
      )
    }
  }
  trend_rows <- trend_rows %>%
    mutate(
      CageChangeIndex_label = "linear_slope_difference_per_CC",
      model_type = "linear_trend_mixed",
      model_engine = "lmerTest::lmer + emmeans::emtrends (~Group | Sex, var = CageChangeIndexNum)",
      model_formula = f_tr, model_status = tr_status, model_warnings = tr_warn,
      n_obs_model = n_obs, n_animals_model = n_animals, n_fixed_params = n_fixed_tr,
      is_singular = tr_sing
    )

  # ---------------- (D) average_effect_mixed: SHIPPED estimator, unchanged -----
  shipped_input <- dat %>%
    transmute(
      AnimalNum = as.character(AnimalNum), Group = as.character(Group),
      Sex = as.character(Sex), CageChangeIndex = CageChangeIndex,
      PhaseClass = PhaseClass, Domain = outcome, DomainScore = value
    )
  avg_w <- with_warnings(fit_repeated_measures_domain_contrasts(shipped_input, outcome, phase))
  if (inherits(avg_w$value, "error")) {
    avg_rows <- tidyr::crossing(Sex = sex_levels, contrast = names(contrast_vectors))
    avg_rows <- bind_cols(avg_rows, empty_cols[rep(1, nrow(avg_rows)), ]) %>%
      mutate(
        model_status = paste0("error: ", conditionMessage(avg_w$value)),
        model_formula = "DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)",
        model_warnings = paste(avg_w$warnings, collapse = " | ")
      )
  } else {
    avg_rows <- avg_w$value$contrasts %>%
      transmute(
        Sex = as.character(Sex), contrast = as.character(contrast),
        estimate = mixed_model_estimate, SE = mixed_model_SE, df = mixed_model_df,
        t_ratio = mixed_model_t, p_value = mixed_model_p,
        model_status = model_status, model_formula = model_formula,
        model_warnings = model_warnings
      )
  }
  avg_rows <- avg_rows %>%
    mutate(
      CageChangeIndex_label = "average_over_CC1_CC4",
      model_type = "average_effect_mixed",
      model_engine = "Functions/hmm_stage14_helpers.R::fit_repeated_measures_domain_contrasts() UNCHANGED",
      n_obs_model = n_obs, n_animals_model = n_animals,
      n_fixed_params = NA_integer_,
      is_singular = model_status == "singular_fit"
    )

  # ---------------- assemble ---------------------------------------------------
  bind_all <- bind_rows(int_rows, strat_rows, trend_rows, avg_rows) %>%
    mutate(
      resolution = res, PhaseClass = phase, outcome = outcome, scaling = scaling,
      value_column_used = vcol
    )

  # Descriptives / Hedges g. For a specific CC use that CC's single observation per
  # animal; for slope rows use the per-animal OLS slope of value on CC; for the
  # average model use the per-animal mean across CC (matches the shipped helper).
  animal_mean_tbl <- dat %>%
    group_by(AnimalNum, Group, Sex) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(Sex = as.character(Sex), Group = as.character(Group))
  animal_slope_tbl <- dat %>%
    group_by(AnimalNum, Group, Sex) %>%
    summarise(
      value = if (n_distinct(CageChangeIndexNum) > 1) {
        stats::cov(value, CageChangeIndexNum) / stats::var(CageChangeIndexNum)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(Sex = as.character(Sex), Group = as.character(Group))
  animal_cc_tbl <- dat %>%
    mutate(Sex = as.character(Sex), Group = as.character(Group)) %>%
    select(AnimalNum, Group, Sex, CageChangeIndex, value)

  desc <- pmap_dfr(
    list(bind_all$Sex, bind_all$contrast, bind_all$CageChangeIndex_label, bind_all$model_type),
    function(sx, ct, cclab, mt) {
      ref <- sub("^.*-", "", ct)
      comp <- sub("-.*$", "", ct)
      base <- if (mt == "linear_trend_mixed") {
        animal_slope_tbl
      } else if (mt == "average_effect_mixed") {
        animal_mean_tbl
      } else {
        animal_cc_tbl %>% filter(CageChangeIndex == as.integer(cclab))
      }
      d <- g_between(base, sx, ref, comp)
      tibble(
        n_ref_animals = d$n_ref, n_comp_animals = d$n_comp,
        mean_ref = d$mean_ref, mean_comp = d$mean_comp,
        mean_difference = d$mean_comp - d$mean_ref, hedges_g = d$g
      )
    }
  )
  bind_all <- bind_cols(bind_all, desc)

  # per-cell n (Group x Sex x CC); NA on slope/average rows, which pool CC1-CC4
  cells_long <- cells %>% mutate(CageChangeIndex_label = as.character(CageChangeIndex))
  bind_all <- bind_all %>%
    left_join(
      cells_long %>% select(
        Sex, CageChangeIndex_label,
        n_cell_CON, n_cell_RES, n_cell_SUS
      ),
      by = c("Sex", "CageChangeIndex_label")
    )

  all_rows[[length(all_rows) + 1L]] <- bind_all

  diag_rows[[length(diag_rows) + 1L]] <- tibble(
    resolution = res, PhaseClass = phase, outcome = outcome, scaling = scaling,
    n_obs = n_obs, n_animals = n_animals,
    n_fixed_params_interaction = n_fixed_int,
    n_fixed_params_trend = n_fixed_tr,
    obs_per_fixed_param_interaction = n_obs / n_fixed_int,
    interaction_status = int_status, interaction_singular = int_singular,
    interaction_warnings = int_warn,
    trend_status = tr_status, trend_singular = tr_sing, trend_warnings = tr_warn
  )
  cat(sprintf(
    "  [fit] %-78s obs=%3d animals=%3d int=%s trend=%s\n", tag, n_obs, n_animals,
    int_status, tr_status
  ))
}

longitudinal <- bind_rows(all_rows)
omnibus <- bind_rows(omnibus_rows)
diagnostics <- bind_rows(diag_rows)
cellcounts <- bind_rows(cellcount_rows)

# --------------------------------------------------------------------------------
# 7. CIs + named audit-only FDR families
# --------------------------------------------------------------------------------
longitudinal <- longitudinal %>%
  mutate(
    ci_low = if_else(is.finite(estimate) & is.finite(SE) & is.finite(df),
      estimate - stats::qt(0.975, df) * SE, NA_real_
    ),
    ci_high = if_else(is.finite(estimate) & is.finite(SE) & is.finite(df),
      estimate + stats::qt(0.975, df) * SE, NA_real_
    )
  ) %>%
  group_by(resolution, PhaseClass, outcome, scaling, model_type, Sex) %>%
  mutate(
    FDR_q = p.adjust(p_value, method = "BH"),
    n_tests_in_family = sum(is.finite(p_value)),
    FDR_family_id = paste("LONGITUDINAL_AUDIT_ONLY", outcome, scaling, resolution,
      PhaseClass, Sex, model_type,
      sep = "__"
    )
  ) %>%
  ungroup() %>%
  mutate(
    # keep model_warnings a non-empty character column so downstream readers do
    # not type-guess it as logical from the leading all-empty rows
    model_warnings = if_else(is.na(model_warnings) | !nzchar(model_warnings),
      "none", model_warnings
    ),
    scaling_note = scaling_note[scaling],
    fdr_note = paste(
      "BH within resolution x PhaseClass x outcome x scaling x model_type x Sex",
      "(all cage changes x all 3 contrasts of that model family). This is an",
      "AUDIT-ONLY family and does NOT redefine the shipped primary heatmap family",
      "displayed_domains_x_3_group_contrasts__<res>__<Sex>__<Phase> (18 tests)."
    ),
    longitudinal_note = paste(
      "stratified_lm rows are ORDINARY LEAST SQUARES, not mixed models: within one",
      "CageChangeIndex each animal contributes exactly one observation per phase so",
      "an animal random intercept is not identifiable. interaction_mixed,",
      "linear_trend_mixed and average_effect_mixed all retain (1 | AnimalNum) and",
      "all four cage changes. Per-CC point estimates must NOT be read as a trend",
      "unless the Group:factor(CageChangeIndex) omnibus F is non-null; see",
      "hmm_architecture_longitudinal_omnibus_tests.csv."
    )
  ) %>%
  rename(CageChangeIndex = CageChangeIndex_label) %>%
  select(
    outcome, scaling, value_column_used, resolution, PhaseClass, Sex, CageChangeIndex,
    contrast, model_type, model_engine, model_formula,
    estimate, SE, df, ci_low, ci_high, t_ratio, p_value, FDR_q, FDR_family_id,
    n_tests_in_family, n_ref_animals, n_comp_animals, mean_ref, mean_comp,
    mean_difference, hedges_g, n_cell_CON, n_cell_RES, n_cell_SUS,
    n_obs_model, n_animals_model, n_fixed_params, is_singular,
    model_status, model_warnings, scaling_note, fdr_note, longitudinal_note
  ) %>%
  arrange(outcome, scaling, resolution, PhaseClass, Sex, model_type, CageChangeIndex, contrast)

out_main <- file.path(audit_out, "hmm_architecture_longitudinal_results.csv")
write_table(longitudinal, out_main)
cat("\n[7] WROTE", out_main, "\n    ", nrow(longitudinal), "rows x", ncol(longitudinal), "cols\n\n")

omnibus <- omnibus %>%
  mutate(omnibus_note = paste(
    "Satterthwaite type-III F tests from lmerTest::anova(). Group:factor(CageChangeIndex)",
    "and Group:Sex:factor(CageChangeIndex) are THE tests of whether the group effect",
    "changes across cage changes. Group:CageChangeIndexNum is the parsimonious",
    "linear (increasing vs habituating) version. Raw p only; no FDR applied to",
    "omnibus tests."
  ))
write_table(omnibus, file.path(audit_out, "hmm_architecture_longitudinal_omnibus_tests.csv"))
write_table(diagnostics, file.path(audit_out, "hmm_architecture_longitudinal_model_diagnostics.csv"))
write_table(cellcounts, file.path(audit_out, "hmm_architecture_longitudinal_cell_counts.csv"))

# --------------------------------------------------------------------------------
# 8. Cross-check: interaction-model simple effects vs stratified OLS
# --------------------------------------------------------------------------------
xcheck <- longitudinal %>%
  filter(
    model_type %in% c("interaction_mixed", "stratified_lm"),
    CageChangeIndex %in% as.character(1:4)
  ) %>%
  select(
    outcome, scaling, resolution, PhaseClass, Sex, CageChangeIndex, contrast,
    model_type, estimate, SE, p_value
  ) %>%
  pivot_wider(names_from = model_type, values_from = c(estimate, SE, p_value)) %>%
  mutate(
    abs_diff_estimate = abs(estimate_interaction_mixed - estimate_stratified_lm),
    ratio_SE = SE_interaction_mixed / SE_stratified_lm
  )
xcheck_summary <- xcheck %>%
  group_by(outcome, scaling, resolution, PhaseClass) %>%
  summarise(
    n = n(),
    max_abs_diff_estimate = max(abs_diff_estimate, na.rm = TRUE),
    median_abs_diff_estimate = median(abs_diff_estimate, na.rm = TRUE),
    median_SE_ratio_interaction_over_lm = median(ratio_SE, na.rm = TRUE),
    min_SE_ratio = min(ratio_SE, na.rm = TRUE), max_SE_ratio = max(ratio_SE, na.rm = TRUE),
    .groups = "drop"
  )
write_table(xcheck, file.path(audit_out, "hmm_architecture_longitudinal_crosscheck_interaction_vs_lm.csv"))
write_table(xcheck_summary, file.path(audit_out, "hmm_architecture_longitudinal_crosscheck_summary.csv"))
cat("[8] interaction-model simple effects vs stratified OLS:\n")
print(as.data.frame(xcheck_summary))

# --------------------------------------------------------------------------------
# 9. CONSOLE READOUT
# --------------------------------------------------------------------------------
sep <- function(t) cat("\n==== ", t, " ", strrep("=", max(0, 90 - nchar(t))), "\n", sep = "")

sep("9a. per-cell n (Group x Sex x CageChangeIndex), 5min composite context-z")
print(as.data.frame(cellcounts %>%
  filter(
    resolution == "5min_based", outcome == "composite_behavioral_state_architecture",
    scaling == "context_z_sex_phase_cc"
  ) %>%
  select(PhaseClass, Sex, CageChangeIndex, n_cell_CON, n_cell_RES, n_cell_SUS)))

sep("9a2. per-cell n, 10min")
print(as.data.frame(cellcounts %>%
  filter(
    resolution == "10min_based", outcome == "composite_behavioral_state_architecture",
    scaling == "context_z_sex_phase_cc"
  ) %>%
  select(PhaseClass, Sex, CageChangeIndex, n_cell_CON, n_cell_RES, n_cell_SUS)))

sep("9b. model diagnostics (all outcomes)")
print(as.data.frame(diagnostics %>%
  select(
    resolution, PhaseClass, outcome, scaling, n_obs, n_animals,
    n_fixed_params_interaction, obs_per_fixed_param_interaction,
    interaction_status, trend_status
  )))
cat("\nany interaction-model warnings:\n")
print(as.data.frame(diagnostics %>% filter(nzchar(interaction_warnings)) %>%
  select(resolution, PhaseClass, outcome, scaling, interaction_warnings)))
cat("\nany trend-model warnings:\n")
print(as.data.frame(diagnostics %>% filter(nzchar(trend_warnings)) %>%
  select(resolution, PhaseClass, outcome, scaling, trend_warnings)))

sep("9c. OMNIBUS Group x CC and Group x Sex x CC tests (interaction model)")
print(as.data.frame(omnibus %>%
  filter(
    model_type == "interaction_mixed",
    term %in% c("Group:factor(CageChangeIndex)", "Group:Sex:factor(CageChangeIndex)")
  ) %>%
  select(outcome, scaling, resolution, PhaseClass, term, NumDF, DenDF, F_value, p_value) %>%
  arrange(outcome, scaling, resolution, PhaseClass, term)))

sep("9d. OMNIBUS linear Group x CC_numeric tests (trend model)")
print(as.data.frame(omnibus %>%
  filter(
    model_type == "linear_trend_mixed",
    term %in% c("Group:CageChangeIndexNum", "Group:Sex:CageChangeIndexNum")
  ) %>%
  select(outcome, scaling, resolution, PhaseClass, term, NumDF, DenDF, F_value, p_value) %>%
  arrange(outcome, scaling, resolution, PhaseClass, term)))

show_block <- function(oc, sc, ph, sx) {
  sep(paste0("9e. ", oc, " [", sc, "] -- ", sx, " ", ph))
  print(as.data.frame(longitudinal %>%
    filter(outcome == oc, scaling == sc, PhaseClass == ph, Sex == sx) %>%
    select(
      resolution, model_type, CageChangeIndex, contrast, estimate, SE, ci_low, ci_high,
      p_value, FDR_q, hedges_g, n_ref_animals, n_comp_animals
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    arrange(resolution, model_type, CageChangeIndex, contrast)))
}

for (sc in c("context_z_sex_phase_cc", "sensitivity_pooled_z_sex_phase")) {
  for (ph in c("Active", "Inactive")) {
    for (sx in c("Female", "Male")) {
      show_block("composite_behavioral_state_architecture", sc, ph, sx)
    }
  }
}
for (oc in c(
  "occupancy_entropy", "inactive_state_fraction",
  "top_proximity_state_fraction", "state_switch_rate"
)) {
  for (ph in c("Active", "Inactive")) {
    show_block(oc, "raw_units", ph, "Female")
    show_block(oc, "raw_units", ph, "Male")
  }
}

sep("9f. raw-unit group means per CC (level drift), Female")
print(as.data.frame(components %>%
  filter(Sex == "Female") %>%
  group_by(resolution, PhaseClass, CageChangeIndex, Group) %>%
  summarise(
    n = n_distinct(AnimalNum),
    occ_entropy = round(mean(occupancy_entropy), 4),
    inactive_frac = round(mean(inactive_state_fraction), 4),
    top_prox_frac = round(mean(top_proximity_state_fraction), 4),
    switch_rate = round(mean(state_switch_rate), 4),
    .groups = "drop"
  ) %>% arrange(resolution, PhaseClass, CageChangeIndex, Group)))

sep("9g. raw-unit group means per CC (level drift), Male")
print(as.data.frame(components %>%
  filter(Sex == "Male") %>%
  group_by(resolution, PhaseClass, CageChangeIndex, Group) %>%
  summarise(
    n = n_distinct(AnimalNum),
    occ_entropy = round(mean(occupancy_entropy), 4),
    inactive_frac = round(mean(inactive_state_fraction), 4),
    top_prox_frac = round(mean(top_proximity_state_fraction), 4),
    switch_rate = round(mean(state_switch_rate), 4),
    .groups = "drop"
  ) %>% arrange(resolution, PhaseClass, CageChangeIndex, Group)))

cat("\n[done]\n")
