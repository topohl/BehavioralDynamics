# ================================================================
# HMM / Stage 14 identity, semantic-state and inference helpers
# MMMSociability
# ================================================================
# These helpers deliberately depend on canonical_animal_id() from
# behavioral_dynamics_helpers.R. They do not define a second animal-ID
# normalization contract.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
})

hmm_required_identity_columns <- c("AnimalNum", "Group", "Sex")
hmm_standardization_context <- c("Sex", "PhaseClass", "CageChangeIndex")
hmm_semantic_categories <- c(
  "inactive/low-exploration",
  "social",
  "burst/high-movement",
  "exploratory",
  "mixed"
)

assert_canonical_animal_id_available <- function() {
  if (!exists("canonical_animal_id", mode = "function", inherits = TRUE)) {
    stop(
      "canonical_animal_id() is required. Source Analysis/_pipeline_setup.R before HMM helpers.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

build_canonical_identity_roster <- function(dat, source_label = "Stage 01 canonical roster") {
  assert_canonical_animal_id_available()
  missing_cols <- setdiff(hmm_required_identity_columns, names(dat))
  if (length(missing_cols) > 0L) {
    stop(source_label, " is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  roster_long <- dat %>%
    transmute(
      AnimalNum = canonical_animal_id(.data$AnimalNum),
      Group = str_trim(as.character(.data$Group)),
      Sex = str_trim(as.character(.data$Sex))
    ) %>%
    filter(!is.na(AnimalNum), nzchar(AnimalNum)) %>%
    distinct()

  conflicts <- roster_long %>%
    group_by(AnimalNum) %>%
    summarise(
      n_groups = n_distinct(Group[!is.na(Group) & nzchar(Group)]),
      n_sexes = n_distinct(Sex[!is.na(Sex) & nzchar(Sex)]),
      groups = paste(sort(unique(Group[!is.na(Group) & nzchar(Group)])), collapse = "|"),
      sexes = paste(sort(unique(Sex[!is.na(Sex) & nzchar(Sex)])), collapse = "|"),
      .groups = "drop"
    ) %>%
    filter(n_groups != 1L | n_sexes != 1L)

  if (nrow(conflicts) > 0L) {
    stop(
      source_label, " has missing or conflicting canonical Group/Sex metadata:\n",
      paste(utils::capture.output(print(conflicts, n = Inf)), collapse = "\n"),
      call. = FALSE
    )
  }

  roster_long %>%
    group_by(AnimalNum) %>%
    summarise(Group = first(Group), Sex = first(Sex), .groups = "drop") %>%
    arrange(AnimalNum)
}

audit_hmm_identity <- function(dat, canonical_roster, source_label = "HMM table") {
  assert_canonical_animal_id_available()
  if (!"AnimalNum" %in% names(dat)) {
    stop(source_label, " is missing AnimalNum.", call. = FALSE)
  }
  canonical_roster <- build_canonical_identity_roster(canonical_roster, "canonical Stage 01 roster")

  raw_id <- as.character(dat$AnimalNum)
  canonical_id <- canonical_animal_id(raw_id)
  source_group <- if ("Group" %in% names(dat)) str_trim(as.character(dat$Group)) else rep(NA_character_, nrow(dat))
  source_sex <- if ("Sex" %in% names(dat)) str_trim(as.character(dat$Sex)) else rep(NA_character_, nrow(dat))

  identity_rows <- tibble(
    source = source_label,
    raw_animal_id = raw_id,
    AnimalNum = canonical_id,
    source_group = source_group,
    source_sex = source_sex
  )

  alias_audit <- identity_rows %>%
    filter(!is.na(AnimalNum)) %>%
    distinct(source, AnimalNum, raw_animal_id) %>%
    group_by(source, AnimalNum) %>%
    summarise(
      raw_alias_count = n_distinct(raw_animal_id),
      raw_aliases = paste(sort(unique(raw_animal_id)), collapse = "|"),
      alias_merge_required = raw_alias_count > 1L || any(raw_animal_id != AnimalNum),
      .groups = "drop"
    ) %>%
    arrange(AnimalNum)

  identity_conflicts <- identity_rows %>%
    filter(!is.na(AnimalNum)) %>%
    distinct(AnimalNum, source_group, source_sex) %>%
    group_by(AnimalNum) %>%
    summarise(
      n_groups = n_distinct(source_group[!is.na(source_group) & nzchar(source_group)]),
      n_sexes = n_distinct(source_sex[!is.na(source_sex) & nzchar(source_sex)]),
      groups = paste(sort(unique(source_group[!is.na(source_group) & nzchar(source_group)])), collapse = "|"),
      sexes = paste(sort(unique(source_sex[!is.na(source_sex) & nzchar(source_sex)])), collapse = "|"),
      .groups = "drop"
    ) %>%
    filter(n_groups > 1L | n_sexes > 1L) %>%
    mutate(source = source_label, conflict_type = "alias_metadata_conflict", .before = 1)

  concordance <- identity_rows %>%
    distinct(AnimalNum, source_group, source_sex) %>%
    left_join(
      canonical_roster %>% rename(roster_group = Group, roster_sex = Sex),
      by = "AnimalNum"
    ) %>%
    mutate(
      source = source_label,
      group_concordant = is.na(source_group) | !nzchar(source_group) | source_group == roster_group,
      sex_concordant = is.na(source_sex) | !nzchar(source_sex) | source_sex == roster_sex,
      status = case_when(
        is.na(AnimalNum) ~ "missing_canonical_id",
        is.na(roster_group) | is.na(roster_sex) ~ "not_in_canonical_roster",
        !group_concordant | !sex_concordant ~ "metadata_disagreement",
        TRUE ~ "concordant"
      )
    ) %>%
    relocate(source)

  failing_concordance <- concordance %>% filter(status != "concordant")
  passed <- nrow(identity_conflicts) == 0L && nrow(failing_concordance) == 0L

  roster_idx <- match(canonical_id, canonical_roster$AnimalNum)
  reconciled <- dat
  if (!"AnimalID_raw" %in% names(reconciled)) reconciled$AnimalID_raw <- raw_id
  reconciled$AnimalNum <- canonical_id
  reconciled$Group <- canonical_roster$Group[roster_idx]
  reconciled$Sex <- canonical_roster$Sex[roster_idx]

  summary <- tibble(
    source = source_label,
    input_rows = nrow(dat),
    raw_animal_spellings = n_distinct(raw_id, na.rm = TRUE),
    canonical_animals = n_distinct(canonical_id, na.rm = TRUE),
    canonical_roster_animals = n_distinct(canonical_roster$AnimalNum),
    aliases_merged = sum(alias_audit$alias_merge_required),
    identity_conflicts = nrow(identity_conflicts),
    unknown_animals = sum(concordance$status == "not_in_canonical_roster"),
    metadata_disagreements = sum(concordance$status == "metadata_disagreement"),
    missing_canonical_ids = sum(concordance$status == "missing_canonical_id"),
    passed = passed
  )

  list(
    data = reconciled,
    alias_audit = alias_audit,
    identity_conflicts = identity_conflicts,
    concordance = concordance,
    summary = summary,
    passed = passed
  )
}

assert_hmm_identity_audit <- function(audit) {
  if (isTRUE(audit$passed)) return(invisible(audit))
  failures <- audit$concordance %>% filter(status != "concordant")
  stop(
    "HMM identity reconciliation failed closed. Alias metadata conflicts or Stage 01 roster disagreements were found.\n",
    paste(utils::capture.output(print(audit$identity_conflicts, n = Inf)), collapse = "\n"),
    if (nrow(failures) > 0L) paste0("\n", paste(utils::capture.output(print(failures, n = Inf)), collapse = "\n")) else "",
    call. = FALSE
  )
}

resolve_configured_hmm_artifact <- function(project_root, resolution, filename = "hmm_state_occupancy.csv", required = TRUE) {
  path <- file.path(
    project_root,
    "analysis_ready/06_behavioral_dynamics/hmm_states",
    resolution,
    "tables",
    filename
  )
  exists <- file.exists(path)
  if (required && !exists) {
    stop("Configured HMM artifact is missing for ", resolution, ": ", path, call. = FALSE)
  }
  list(path = path, resolution = resolution, exists = exists)
}

annotate_hmm_semantic_states <- function(state_summary, resolution = NA_character_) {
  required <- c("State", "Movement_z", "Entropy_z", "Proximity_z")
  missing_cols <- setdiff(required, names(state_summary))
  if (length(missing_cols) > 0L) {
    stop("HMM state summary is missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  state_summary %>%
    transmute(
      resolution = resolution,
      State = as.character(.data$State),
      Movement_z = suppressWarnings(as.numeric(.data$Movement_z)),
      Entropy_z = suppressWarnings(as.numeric(.data$Entropy_z)),
      Proximity_z = suppressWarnings(as.numeric(.data$Proximity_z))
    ) %>%
    mutate(
      SemanticState = case_when(
        Movement_z <= median(Movement_z, na.rm = TRUE) & Entropy_z <= median(Entropy_z, na.rm = TRUE) ~ "inactive/low-exploration",
        Proximity_z >= quantile(Proximity_z, 0.67, na.rm = TRUE) ~ "social",
        Movement_z >= quantile(Movement_z, 0.67, na.rm = TRUE) ~ "burst/high-movement",
        Entropy_z >= quantile(Entropy_z, 0.67, na.rm = TRUE) ~ "exploratory",
        TRUE ~ "mixed"
      ),
      StateLabel = paste0("S", State, "\n", SemanticState),
      semantic_rule_order = "inactive_then_social_then_burst_then_exploratory_then_mixed"
    ) %>%
    distinct()
}

audit_hmm_semantic_categories <- function(state_labels, resolution = unique(state_labels$resolution)[1]) {
  tibble(SemanticState = hmm_semantic_categories) %>%
    left_join(
      state_labels %>% count(SemanticState, name = "n_fitted_states"),
      by = "SemanticState"
    ) %>%
    mutate(
      resolution = resolution,
      n_fitted_states = replace_na(n_fitted_states, 0L),
      fitted_states = map_chr(
        SemanticState,
        ~ paste(sort(state_labels$State[state_labels$SemanticState == .x]), collapse = "|")
      ),
      category_missing = n_fitted_states == 0L,
      semantic_labels_are_operational = TRUE
    ) %>%
    relocate(resolution)
}

strict_standardize_within_context <- function(dat, value_col, group_cols = hmm_standardization_context) {
  missing_cols <- setdiff(c(value_col, group_cols), names(dat))
  if (length(missing_cols) > 0L) {
    stop(
      "Cannot standardize ", value_col, "; required columns are missing: ",
      paste(missing_cols, collapse = ", "),
      ". Intended context: ", paste(group_cols, collapse = " x "),
      call. = FALSE
    )
  }
  dat %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      "{value_col}_z" := {
        x <- suppressWarnings(as.numeric(.data[[value_col]]))
        s <- sd(x, na.rm = TRUE)
        m <- mean(x, na.rm = TRUE)
        if (!is.finite(s) || s == 0) rep(0, length(x)) else (x - m) / s
      }
    ) %>%
    ungroup()
}

hmm_feature_entropy <- function(p) {
  p <- p[is.finite(p) & p > 0]
  if (length(p) == 0L) return(NA_real_)
  -sum(p * log(p))
}

build_hmm_epoch_scores <- function(occupancy, state_labels, canonical_roster, resolution) {
  identity <- audit_hmm_identity(occupancy, canonical_roster, paste0("Stage 14 HMM occupancy ", resolution))
  assert_hmm_identity_audit(identity)

  components <- identity$data %>%
    mutate(
      AnimalNum = as.character(.data$AnimalNum),
      Group = as.character(.data$Group),
      Sex = as.character(.data$Sex),
      PhaseClass = case_when(
        str_detect(str_to_lower(as.character(.data$Phase)), "\\binactive\\b|\\blight\\b|\\bday\\b") ~ "Inactive",
        str_detect(str_to_lower(as.character(.data$Phase)), "\\bactive\\b|\\bdark\\b|\\bnight\\b") ~ "Active",
        TRUE ~ as.character(.data$Phase)
      ),
      CageChange = as.character(.data$CageChange),
      CageChangeIndex = suppressWarnings(as.integer(str_extract(CageChange, "\\d+"))),
      State = as.character(.data$State),
      frac_time = suppressWarnings(as.numeric(.data$frac_time))
    ) %>%
    left_join(state_labels %>% select(State, SemanticState), by = "State") %>%
    group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) %>%
    summarise(
      state_occupancy_entropy = hmm_feature_entropy(frac_time / sum(frac_time, na.rm = TRUE)),
      inactive_state_fraction = sum(frac_time[SemanticState == "inactive/low-exploration"], na.rm = TRUE),
      social_state_fraction = sum(frac_time[SemanticState == "social"], na.rm = TRUE),
      .groups = "drop"
    )

  scored <- components %>%
    strict_standardize_within_context("state_occupancy_entropy") %>%
    strict_standardize_within_context("inactive_state_fraction") %>%
    strict_standardize_within_context("social_state_fraction") %>%
    mutate(
      `Behavioral state architecture` =
        rowMeans(cbind(state_occupancy_entropy_z, social_state_fraction_z), na.rm = FALSE) -
        inactive_state_fraction_z,
      resolution = resolution,
      standardization_context = paste(hmm_standardization_context, collapse = " x ")
    )

  component_cols <- c("state_occupancy_entropy", "inactive_state_fraction", "social_state_fraction")
  component_audit <- map_dfr(component_cols, function(component) {
    x <- suppressWarnings(as.numeric(components[[component]]))
    tibble(
      resolution = resolution,
      component = component,
      n_finite = sum(is.finite(x)),
      mean = if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_,
      variance = if (sum(is.finite(x)) > 1L) var(x, na.rm = TRUE) else NA_real_,
      is_constant = sum(is.finite(x)) > 0L && n_distinct(x[is.finite(x)]) == 1L,
      is_all_zero = sum(is.finite(x)) > 0L && all(x[is.finite(x)] == 0),
      available = any(is.finite(x))
    )
  }) %>%
    mutate(
      composite_formula = "mean(z(state_occupancy_entropy), z(social_state_fraction)) - z(inactive_state_fraction)",
      mathematical_reduction = if_else(
        component == "social_state_fraction" & is_all_zero,
        "0.5 * z(state_occupancy_entropy) - z(inactive_state_fraction); social component contributes zero",
        NA_character_
      )
    )

  context_audit <- components %>%
    group_by(Sex, PhaseClass, CageChangeIndex) %>%
    summarise(
      n_animals = n_distinct(AnimalNum),
      entropy_variance = var(state_occupancy_entropy, na.rm = TRUE),
      inactive_variance = var(inactive_state_fraction, na.rm = TRUE),
      social_variance = var(social_state_fraction, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      resolution = resolution,
      standardization_grouping_variables = paste(hmm_standardization_context, collapse = " x "),
      .before = 1
    )

  list(
    scores = scored,
    components = components,
    component_audit = component_audit,
    context_audit = context_audit,
    identity_audit = identity
  )
}

audit_hmm_coverage <- function(base_epochs, hmm_scores, resolution) {
  expected <- base_epochs %>%
    transmute(
      AnimalNum = as.character(.data$AnimalNum),
      Group = as.character(.data$Group),
      Sex = as.character(.data$Sex),
      Phase = as.character(.data$PhaseClass),
      CageChangeIndex = suppressWarnings(as.numeric(.data$CageChangeIndex))
    ) %>%
    filter(Phase %in% c("Active", "Inactive"), is.finite(CageChangeIndex)) %>%
    distinct()
  observed <- hmm_scores %>%
    transmute(
      AnimalNum = as.character(.data$AnimalNum),
      Group = as.character(.data$Group),
      Sex = as.character(.data$Sex),
      Phase = as.character(.data$PhaseClass),
      CageChangeIndex = suppressWarnings(as.numeric(.data$CageChangeIndex))
    ) %>%
    distinct()

  expected %>%
    group_by(Sex, Group, Phase) %>%
    group_modify(~{
      e <- .x
      o <- observed %>%
        filter(Sex == .y$Sex, Group == .y$Group, Phase == .y$Phase)
      matched_epochs <- e %>% inner_join(o, by = c("AnimalNum", "Group", "Sex", "Phase", "CageChangeIndex"))
      missing_ids <- sort(setdiff(unique(e$AnimalNum), unique(o$AnimalNum)))
      tibble(
        animals_expected = n_distinct(e$AnimalNum),
        animals_with_hmm = n_distinct(intersect(e$AnimalNum, o$AnimalNum)),
        animals_missing = length(missing_ids),
        missing_animal_ids = paste(missing_ids, collapse = "|"),
        epochs_expected = nrow(e),
        epochs_with_hmm = nrow(matched_epochs)
      )
    }, .keep = TRUE) %>%
    ungroup() %>%
    mutate(
      resolution = resolution,
      unexpected_identity_loss = animals_missing > 0L,
      .before = 1
    )
}

audit_hmm_coverage_detail <- function(base_epochs, hmm_scores, resolution) {
  expected <- base_epochs %>%
    transmute(
      AnimalNum = as.character(.data$AnimalNum),
      Group = as.character(.data$Group),
      Sex = as.character(.data$Sex),
      Phase = as.character(.data$PhaseClass),
      CageChangeIndex = suppressWarnings(as.integer(.data$CageChangeIndex))
    ) %>%
    filter(Phase %in% c("Active", "Inactive"), is.finite(CageChangeIndex)) %>%
    distinct()
  observed <- hmm_scores %>%
    transmute(
      AnimalNum = as.character(.data$AnimalNum),
      Group = as.character(.data$Group),
      Sex = as.character(.data$Sex),
      Phase = as.character(.data$PhaseClass),
      CageChangeIndex = suppressWarnings(as.integer(.data$CageChangeIndex))
    ) %>%
    distinct() %>%
    mutate(hmm_epoch_present = TRUE)

  expected %>%
    left_join(
      observed,
      by = c("AnimalNum", "Group", "Sex", "Phase", "CageChangeIndex")
    ) %>%
    mutate(
      resolution = resolution,
      hmm_epoch_present = coalesce(hmm_epoch_present, FALSE),
      coverage_status = if_else(
        hmm_epoch_present,
        "present",
        "missing usable HMM sequence; inspect Stage 08 hmm_epoch_data_quality_exclusions.csv"
      ),
      .before = 1
    )
}

hmm_hedges_g <- function(ref, comp) {
  ref <- ref[is.finite(ref)]
  comp <- comp[is.finite(comp)]
  if (length(ref) < 2L || length(comp) < 2L) return(NA_real_)
  pooled_sd <- sqrt(((length(ref) - 1) * var(ref) + (length(comp) - 1) * var(comp)) /
    (length(ref) + length(comp) - 2))
  if (!is.finite(pooled_sd) || pooled_sd == 0) return(NA_real_)
  d <- (mean(comp) - mean(ref)) / pooled_sd
  df <- length(ref) + length(comp) - 2
  d * (1 - 3 / (4 * df - 1))
}

fit_repeated_measures_domain_contrasts <- function(dat, domain, phase, group_levels = c("CON", "RES", "SUS"), sex_levels = c("Female", "Male")) {
  if (!requireNamespace("lmerTest", quietly = TRUE) || !requireNamespace("emmeans", quietly = TRUE)) {
    stop("lmerTest and emmeans are required for Stage 14 heatmap inference; no Welch fallback is permitted.", call. = FALSE)
  }

  model_dat <- dat %>%
    filter(.data$Domain == domain, .data$PhaseClass == phase, is.finite(.data$DomainScore)) %>%
    transmute(
      AnimalNum = factor(as.character(.data$AnimalNum)),
      Group = factor(as.character(.data$Group), levels = group_levels),
      Sex = factor(as.character(.data$Sex), levels = sex_levels),
      CageChangeIndex = factor(.data$CageChangeIndex),
      DomainScore = as.numeric(.data$DomainScore)
    ) %>%
    filter(!is.na(Group), !is.na(Sex), !is.na(CageChangeIndex))

  model_formula <- "DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)"
  model_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      lmerTest::lmer(as.formula(model_formula), data = model_dat),
      warning = function(w) {
        model_warnings <<- c(model_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  empty_contrasts <- tidyr::crossing(
    Sex = sex_levels,
    contrast = c("RES-CON", "SUS-CON", "SUS-RES")
  ) %>%
    mutate(
      Domain = domain,
      PhaseClass = phase,
      n_ref_animals = NA_integer_,
      n_comp_animals = NA_integer_,
      mean_ref = NA_real_,
      mean_comp = NA_real_,
      animal_level_hedges_g = NA_real_,
      mixed_model_estimate = NA_real_,
      mixed_model_SE = NA_real_,
      mixed_model_df = NA_real_,
      mixed_model_t = NA_real_,
      mixed_model_p = NA_real_,
      model_engine = "not_estimable",
      model_formula = model_formula,
      model_status = if (inherits(fit, "error")) conditionMessage(fit) else "not_estimable",
      model_warnings = paste(unique(model_warnings), collapse = " | ")
    ) %>%
    relocate(Domain, PhaseClass)

  if (inherits(fit, "error")) {
    return(list(contrasts = empty_contrasts, interaction = tibble(
      Domain = domain, PhaseClass = phase, term = "Group:Sex", statistic = NA_real_,
      df_num = NA_real_, df_den = NA_real_, p.value = NA_real_, model_status = conditionMessage(fit)
    )))
  }

  emm <- tryCatch(emmeans::emmeans(fit, ~ Group | Sex), error = function(e) e)
  if (inherits(emm, "error")) {
    empty_contrasts$model_status <- conditionMessage(emm)
    return(list(contrasts = empty_contrasts, interaction = tibble(
      Domain = domain, PhaseClass = phase, term = "Group:Sex", statistic = NA_real_,
      df_num = NA_real_, df_den = NA_real_, p.value = NA_real_, model_status = conditionMessage(emm)
    )))
  }

  contrast_vectors <- list(
    "RES-CON" = c(-1, 1, 0),
    "SUS-CON" = c(-1, 0, 1),
    "SUS-RES" = c(0, -1, 1)
  )
  model_contrasts <- emmeans::contrast(emm, method = contrast_vectors, adjust = "none") %>%
    as.data.frame() %>%
    as_tibble() %>%
    transmute(
      Sex = as.character(.data$Sex),
      contrast = as.character(.data$contrast),
      mixed_model_estimate = .data$estimate,
      mixed_model_SE = .data$SE,
      mixed_model_df = .data$df,
      mixed_model_t = .data$t.ratio,
      mixed_model_p = .data$p.value
    )

  animal_means <- model_dat %>%
    group_by(AnimalNum, Group, Sex) %>%
    summarise(DomainScore = mean(DomainScore), .groups = "drop")
  effect_sizes <- tidyr::crossing(
    Sex = sex_levels,
    contrast = names(contrast_vectors)
  ) %>%
    pmap_dfr(function(Sex, contrast) {
      ref <- sub("^.*-", "", contrast)
      comp <- sub("-.*$", "", contrast)
      ref_values <- animal_means$DomainScore[
        as.character(animal_means$Sex) == Sex & as.character(animal_means$Group) == ref
      ]
      comp_values <- animal_means$DomainScore[
        as.character(animal_means$Sex) == Sex & as.character(animal_means$Group) == comp
      ]
      tibble(
        Sex = Sex,
        contrast = contrast,
        n_ref_animals = sum(is.finite(ref_values)),
        n_comp_animals = sum(is.finite(comp_values)),
        mean_ref = if (any(is.finite(ref_values))) mean(ref_values, na.rm = TRUE) else NA_real_,
        mean_comp = if (any(is.finite(comp_values))) mean(comp_values, na.rm = TRUE) else NA_real_,
        animal_level_hedges_g = hmm_hedges_g(ref_values, comp_values)
      )
    })

  contrasts <- effect_sizes %>%
    left_join(model_contrasts, by = c("Sex", "contrast")) %>%
    mutate(
      Domain = domain,
      PhaseClass = phase,
      model_engine = "lmerTest::lmer + emmeans",
      model_formula = model_formula,
      model_status = if (lme4::isSingular(fit, tol = 1e-4)) "singular_fit" else "fitted",
      model_warnings = paste(unique(model_warnings), collapse = " | "),
      significance_method = "repeated-measures mixed-model emmeans contrast",
      effect_size_method = "Hedges g from one mean per animal across included cage changes"
    ) %>%
    relocate(Domain, PhaseClass)

  anova_tbl <- tryCatch(as.data.frame(anova(fit)), error = function(e) NULL)
  interaction <- if (is.null(anova_tbl) || !"Group:Sex" %in% rownames(anova_tbl)) {
    tibble(
      Domain = domain, PhaseClass = phase, term = "Group:Sex", statistic = NA_real_,
      df_num = NA_real_, df_den = NA_real_, p.value = NA_real_, model_status = "interaction_not_estimable"
    )
  } else {
    row <- anova_tbl["Group:Sex", , drop = FALSE]
    tibble(
      Domain = domain,
      PhaseClass = phase,
      term = "Group:Sex",
      statistic = row[["F value"]],
      df_num = row[["NumDF"]],
      df_den = row[["DenDF"]],
      p.value = row[["Pr(>F)"]],
      model_status = if (lme4::isSingular(fit, tol = 1e-4)) "singular_fit" else "fitted"
    )
  }

  list(contrasts = contrasts, interaction = interaction)
}

analyze_repeated_measures_heatmap <- function(dat, displayed_domains, resolution) {
  fits <- tidyr::crossing(
    Domain = displayed_domains,
    PhaseClass = c("Active", "Inactive")
  ) %>%
    pmap(~ fit_repeated_measures_domain_contrasts(dat, ..1, ..2))

  contrasts <- map_dfr(fits, "contrasts") %>%
    mutate(resolution = resolution, .before = 1) %>%
    group_by(resolution, Sex, PhaseClass) %>%
    mutate(
      FDR_q = p.adjust(mixed_model_p, method = "BH"),
      n_tests_in_family = sum(is.finite(mixed_model_p)),
      FDR_family_id = paste("displayed_domains_x_3_group_contrasts", resolution, Sex, PhaseClass, sep = "__")
    ) %>%
    ungroup() %>%
    mutate(
      n_ref = n_ref_animals,
      n_comp = n_comp_animals,
      hedges_g = animal_level_hedges_g,
      mean_difference = mean_comp - mean_ref,
      p.value = mixed_model_p,
      p_fdr = FDR_q
    )

  interactions <- map_dfr(fits, "interaction") %>%
    mutate(resolution = resolution, .before = 1)

  list(contrasts = contrasts, interactions = interactions)
}
