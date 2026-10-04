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
  if (!exists("mmm_hmm_resolution_root", mode = "function", inherits = TRUE)) {
    stop("HMM artifact resolver requires Functions/project_paths.R", call. = FALSE)
  }
  path <- file.path(mmm_hmm_resolution_root(resolution, project_root),
                    "tables", filename)
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

# ----------------------------------------------------------------
# Domain-heatmap display layer
# ----------------------------------------------------------------
# Appended so that every line above keeps its number: audit reports cite
# this file by line (docs/REPOSITORY_FILE_CLASSIFICATION.csv).
#
# The two helpers below reproduce the model-data slice and the effect-size
# block of fit_repeated_measures_domain_contrasts() for rows that are shown
# without a test. They are kept separate from the engine on purpose, and
# Testing/tests/test_domain_heatmap_display.R asserts that they return the
# engine's own effect sizes.

# One domain x phase slice in the shape the heatmap model and effect sizes use.
domain_phase_model_data <- function(dat, domain, phase, group_levels = c("CON", "RES", "SUS"), sex_levels = c("Female", "Male")) {
  dat %>%
    filter(.data$Domain == domain, .data$PhaseClass == phase, is.finite(.data$DomainScore)) %>%
    transmute(
      AnimalNum = factor(as.character(.data$AnimalNum)),
      Group = factor(as.character(.data$Group), levels = group_levels),
      Sex = factor(as.character(.data$Sex), levels = sex_levels),
      CageChangeIndex = factor(.data$CageChangeIndex),
      DomainScore = as.numeric(.data$DomainScore)
    ) %>%
    filter(!is.na(Group), !is.na(Sex), !is.na(CageChangeIndex))
}

# Animal-level Hedges g within Sex, from one mean per animal across the
# included cage changes. Contrast names are "<comp>-<ref>"; g is comp - ref.
animal_level_contrast_effects <- function(model_dat, contrasts, sex_levels = c("Female", "Male")) {
  animal_means <- model_dat %>%
    group_by(AnimalNum, Group, Sex) %>%
    summarise(DomainScore = mean(DomainScore), .groups = "drop")
  tidyr::crossing(
    Sex = sex_levels,
    contrast = contrasts
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
}

MMM_DOMAIN_HEATMAP_CONTRASTS <- c("RES-CON", "SUS-CON", "RES-SUS")

# ----------------------------------------------------------------
# Domain heatmaps, version 2: windows, resolution variants and tier tests
# ----------------------------------------------------------------
# Three heatmaps (CC1 first dark phase A1, CC1 first light phase L1, and all
# clean phase blocks of CC1-CC4) in three resolution variants. The functions
# below only call frozen helpers (Functions/stage30_movement.R,
# rfid_event_stream.R, rfid_binfree_metrics.R, stage32_windows.R,
# stage30_screen.R, rfid_canonical_inference.R, posthoc_con_contrasts.R) and
# check their outputs; nothing of the canonical measurement or of the
# registered model engine is re-implemented here.

MMM_DHM_VARIANTS <- c("picked", "all5min", "all10min")
MMM_DHM_ALPHA <- 0.05
# CON contrasts treat the animals as the units (decision of 2026-10-03): the
# 12 CON animals per sex live in one stable cage per batch (3 per sex), and
# that cage clustering is not modelled, as is common in behavioural analyses.
# SIS animals keep their cage-episode random effect. The registered post hoc
# CON tests of Figure 1c (Stage 29b) do model the CON cages and are the more
# conservative reference for the CC1 contrasts.
MMM_DHM_MODELS <- list(
  single = list(
    rs = list(formula = "y ~ Batch + g_RS + (1 | CageEpisodeID)", rank = 4L),
    con = list(formula = "y ~ Batch + group + (0 + sisCage | CageEpisodeID)", rank = 5L)
  ),
  pooled = list(
    rs = list(formula = "y ~ Batch + g_RS + c2 + c3 + c4 + (1 | AnimalNum) + (1 | CageEpisodeID)", rank = 7L),
    con = list(formula = "y ~ Batch + group + c2 + c3 + c4 + (1 | AnimalNum) + (0 + sisCage | CageEpisodeID)", rank = 8L)
  )
)
MMM_DHM_CON_UNIT_NOTE <- "animals as units (12 CON animals per sex in 3 stable cages; CON cage clustering not modelled)"
MMM_DHM_CONTRAST_WEIGHTS <- list(`RES-SUS` = list(g_RS = 1), `RES-CON` = list(groupRES = 1), `SUS-CON` = list(groupSUS = 1))

# Which bin width each bin-based component uses in each variant. Declared
# before any of these heatmap results were computed, for continuity with the
# frozen config and Stage 09 and on measurement grounds where a width is
# adequate; group results at 5 and 10 min had been seen before (config v1.0.1,
# lag_block disclosure), so the declaration is a post hoc judgement, not a
# prespecification. Bin-free rows use the canonical per-block estimators in
# every variant.
MMM_DHM_RESOLUTION_MANIFEST <- tibble::tribble(
  ~variant, ~component, ~bin_level, ~basis,
  "picked", "movement_entropy_terms", "10min_based",
  paste("continuity with the frozen config lag block (Movement RMSSD/ACF1 primary at 10 min) and Stage 09 (Entropy_acf1 registered at 10 min);",
        "for Movement, 10 min nearly halves the zero/non-zero switching share (dark phase 0.24 vs 0.44 at 5 min, group-blind);",
        "no width is adequate for the Entropy terms (switching share >= 0.41 at 1-30 min; 5- vs 10-min flexibility rho 0.39), so they follow Movement"),
  "picked", "proximity_terms", "5min_based",
  "frozen config lag block: Proximity RMSSD/ACF1 primary at 5 min; dark-phase proximity autocorrelation half-decays in about 5 min (group-blind)",
  "picked", "switching_term", "10min_based",
  "Stage 12 writes the active/inactive switching rate (H3 epochs) at 10 min only; H1/H2 compute the same rule on their window at 10 min for comparability",
  "picked", "hmm", "10min_based",
  "Stage 08 prespecified the 10-min HMM as primary; the 5-min fit is a near-deterministic any-change / no-change partition",
  "all5min", "movement_entropy_terms", "5min_based", "all bin-based terms at 5 min",
  "all5min", "proximity_terms", "5min_based", "all bin-based terms at 5 min",
  "all5min", "switching_term", "10min_based",
  "kept at 10 min in every variant for comparability: Stage 12 writes the H3 epoch rate at 10 min only (the 5-min H1/H2 window value is exported but not used)",
  "all5min", "hmm", "5min_based", "5-min Stage 08 fit (different construct: any change vs no change, split by proximity)",
  "all10min", "movement_entropy_terms", "10min_based", "all bin-based terms at 10 min",
  "all10min", "proximity_terms", "10min_based", "all bin-based terms at 10 min",
  "all10min", "switching_term", "10min_based", "all bin-based terms at 10 min",
  "all10min", "hmm", "10min_based", "all bin-based terms at 10 min"
) %>%
  mutate(declaration = paste("POST_HOC_JUDGEMENT: continuity with the frozen config and Stage 09, and measurement grounds for Movement and Proximity;",
                             "the bin widths were declared before the first (v2) heatmap results, after earlier 5- and 10-min group results had",
                             "been seen; the rationale text was revised on 2026-10-03 after review, with no bin width changed"))

dhm_resolution <- function(variant, component) {
  x <- MMM_DHM_RESOLUTION_MANIFEST$bin_level[
    MMM_DHM_RESOLUTION_MANIFEST$variant == variant & MMM_DHM_RESOLUTION_MANIFEST$component == component]
  if (length(x) != 1L) stop("No unique resolution for ", variant, " / ", component, call. = FALSE)
  x
}

# ---------------------------------------------------------------- canonical bin-free metrics
# Per animal x phase block on data version 2: Stage 32's block windows and
# raw-record coverage rule, the canonical window metrics (full = TRUE,
# na_if_file_ends_early = FALSE, i.e. exactly s32w_window_metrics), and Stage
# 30's >= 40-s / >= 60-s positional inactivity on the window-clipped runs,
# once per phase label (s30sc_inactivity groups by animal x CC only). Every
# input is hash-gated and the A1 / L1 values are gated against the frozen
# registered tables (1e-9). Returns list(blocks, gates, inputs); stops if any
# gate fails.
MMM_DHM_INPUT_SHA256 <- c(
  sus_animals = "dea3804b71b479f84900f946e5494b204ed0e8801498da459a6fc781beb9f391",
  con_animals = "eddd2ee9c3a182f98211b4cbba689e2bb5f72985aeb178f72b25b7d25db1b75d",
  later_outcome_combz = "1f6a2a69c6b0781b8de8da50ecdadf309b62bbc9c82106c0fa91d18119a7de54",
  ebb_v101_manifest = "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c",
  stage29_canonical_window_metrics = "fc053468f2a1d1a07e981b6a76c71caa6cfd89b19da7318ee2c2f82f49050dd1",
  stage30_features_full_precision = "5d196b218ea6e441bc346a2f3d494fa5419742199e2d367b664ec8e7e599bb83"
)

dhm_canonical_paths <- function(project_root) {
  ar <- file.path(project_root, "analysis_ready")
  dv2 <- file.path(project_root, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28")
  list(
    dv2_manifest = file.path(dv2, "MANIFEST_SHA256.csv"),
    dv2_dir = file.path(dv2, "preprocessed_data"),
    raw_dir = file.path(project_root, "MMMSociability", "raw_data"),
    sus_animals = file.path(dirname(dirname(project_root)), "sus_animals.csv"),
    con_animals = file.path(dirname(dirname(project_root)), "con_animals.csv"),
    later_outcome_combz = file.path(ar, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv"),
    ebb_v101_dir = file.path(ar, "canonical", "behavior_bundle", "ebb_v101_20260929_b2ce507"),
    stage29_canonical_window_metrics = file.path(ar, "pipeline", "29_canonical_behavior_releases", "v101_dv2_b2ce507",
                                                 "tables", "canonical_window_metrics.csv"),
    stage30_features_full_precision = file.path(ar, "pipeline", "30_exploratory_screen", "v1.0_be71e2f", "tables",
                                                "features_cc1_cc4_windows_full_precision.rds")
  )
}

# File of a frozen bundle, checked against the bundle's own manifest (whose
# hash is pinned).
dhm_bundle_file <- function(bundle_dir, file, manifest_sha256) {
  man_path <- file.path(bundle_dir, "00_manifest.csv")
  if (!file.exists(man_path) || !identical(s30sc_sha(man_path), manifest_sha256)) {
    stop("Frozen bundle manifest missing or changed: ", man_path, call. = FALSE)
  }
  man <- data.table::fread(man_path, colClasses = "character")
  want <- man$sha256[man$file == file]
  path <- file.path(bundle_dir, file)
  if (length(want) != 1L || !file.exists(path) || !identical(s30sc_sha(path), want)) {
    stop("Frozen bundle file missing or not equal to its manifest: ", path, call. = FALSE)
  }
  path
}

dhm_canonical_block_metrics <- function(project_root, stage14_roster, s30b_dir, s30b_manifest_sha256) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table is required.", call. = FALSE)
  pth <- dhm_canonical_paths(project_root)
  crit <- MMM_BEHAVIOR_CONFIG$metrics$fragmentation$bout_criterion_s
  gates <- list(); inputs <- list()
  gate <- function(name, passed, detail = "") {
    gates[[length(gates) + 1L]] <<- data.frame(gate = name, passed = isTRUE(passed),
                                                detail = paste(as.character(detail), collapse = "; "))
    invisible(isTRUE(passed))
  }
  inp <- function(path, role) {
    inputs[[length(inputs) + 1L]] <<- data.frame(role = role, path = normalizePath(path, winslash = "/", mustWork = FALSE),
                                                  sha256 = s30sc_sha(path))
  }
  stop_if_failed <- function(stage) {
    g <- do.call(rbind, gates)
    if (!all(g$passed)) {
      stop("Canonical window metrics: ", stage, " gate(s) failed: ",
           paste(g$gate[!g$passed], collapse = "; "), call. = FALSE)
    }
  }

  gate("bout criterion = frozen config value", identical(crit, S30SC_ANALYTIC_CONSTANTS$bout_criterion_s), crit)
  gate("data version 2 manifest sha = pinned", identical(s30sc_sha(pth$dv2_manifest), S30SC_V2_MANIFEST_SHA256))
  inp(pth$dv2_manifest, "data_version_v2_manifest")
  mv2 <- data.table::fread(pth$dv2_manifest, colClasses = "character")
  on_disk <- list.files(pth$dv2_dir, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$")
  gate("24 data-version-2 files = manifest", nrow(mv2) == S30SC_V2_N_FILES && setequal(on_disk, mv2$file), length(on_disk))
  pre_files <- file.path(pth$dv2_dir, mv2$file)
  gate("every data-version-2 file sha = manifest", all(vapply(pre_files, s30sc_sha, "") == mv2$sha256_v2))
  raw_files <- file.path(pth$raw_dir, names(S30SC_RAW_SEED_SHA256))
  raw_sha <- vapply(raw_files, function(p) if (file.exists(p)) s30sc_sha(p) else NA_character_, "")
  gate("24 raw seed files sha = pinned", !anyNA(raw_sha) && all(raw_sha == S30SC_RAW_SEED_SHA256))
  for (k in c("sus_animals", "con_animals", "later_outcome_combz", "stage29_canonical_window_metrics",
              "stage30_features_full_precision")) {
    gate(paste(k, "sha = pinned"), file.exists(pth[[k]]) && identical(s30sc_sha(pth[[k]]), MMM_DHM_INPUT_SHA256[[k]]))
    inp(pth[[k]], k)
  }
  stop_if_failed("input")

  pre <- s30mv_read_preprocessed(pre_files)
  st <- s30mv_build_stream(pre, pth$raw_dir)
  gate("seeds read only from hash-gated raw files",
       all(s30sc_norm_path(unique(st$seeds$raw_file)) %in% s30sc_norm_path(raw_files)))
  fw <- mmm_evs_windows(pre)
  pw <- s32w_phase_windows(fw)
  gate("24 files x 8 phase labels", nrow(pw) == 192L, nrow(pw))
  kept <- s32w_read_blocks(pre_files)
  an_sys <- unique(merge(st$pos[st$pos$is_seed == FALSE, list(SourceFile, AnimalNum, System)],
                         st$file_span[, list(SourceFile, Batch, CC)], by = "SourceFile"))
  gate("111 animals, one System per file, 4 files each",
       data.table::uniqueN(an_sys$AnimalNum) == 111L && !anyDuplicated(an_sys[, list(SourceFile, AnimalNum)]) &&
         all(an_sys[, .N, by = AnimalNum]$N == 4L))
  src_raw <- fw[, list(SourceFile, raw = file.path(pth$raw_dir, Batch, sub("_preprocessed[.]csv$", ".csv", SourceFile)))]
  gate("board records read only from hash-gated raw files", all(s30sc_norm_path(src_raw$raw) %in% s30sc_norm_path(raw_files)))
  rr <- data.table::rbindlist(lapply(seq_len(nrow(src_raw)), function(i) s32w_read_raw_records(src_raw$raw[i], src_raw$SourceFile[i])))
  boards <- s32w_board_intervals(rr, an_sys)
  rm(rr)
  gate("120 boards", nrow(boards) == 120L, nrow(boards))
  cov <- s32w_coverage(an_sys, pw, kept, boards)
  clean_counts <- s32w_coverage_counts(cov)[in_clean_set == TRUE]
  gate("all 111 animals complete in the 24 clean CC x phase blocks",
       nrow(clean_counts) == 24L && all(clean_counts$n_complete_primary == 111L))
  stop_if_failed("coverage")

  long <- s32w_long(s32w_window_metrics(st, pw, crit), cov)
  gate("3552 animal-blocks; 2664 complete with a finite rate",
       nrow(long) == 3552L && long[complete_primary == TRUE, .N] == 2664L &&
         long[complete_primary == TRUE, all(is.finite(crossing_rate))])
  ia <- data.table::rbindlist(lapply(unique(pw$phase), function(p) {
    w <- pw[phase == p, list(SourceFile, window_id = phase, start, end)]
    ws <- s30mv_window_streams(st, w)[[p]]
    out <- s30sc_inactivity(ws$runs)
    out[, phase := p][]
  }))
  data.table::setnames(ia, c("frac40", "frac60"), c("posinact40", "posinact60"))
  blocks <- merge(long, ia, by = c("AnimalNum", "CC", "phase"), all.x = TRUE)
  blocks[complete_primary == FALSE, `:=`(posinact40 = NA_real_, posinact60 = NA_real_, T_obs = NA_real_)]
  ok <- blocks[complete_primary == TRUE]
  gate("inactivity defined for all 2664 complete blocks", sum(is.finite(ok$posinact40)) == 2664L)
  gate("inactivity denominator = observed seconds (1e-9)", max(abs(ok$T_obs - ok$obs_s)) <= S30SC_TOL, max(abs(ok$T_obs - ok$obs_s)))
  gate("0 <= posinact60 <= posinact40 <= 1; zero-event blocks = 1",
       all(ok$posinact40 >= 0 & ok$posinact40 <= 1 & ok$posinact60 <= ok$posinact40) && all(ok[n_events == 0L, posinact40] == 1))
  stop_if_failed("measure")

  con <- canonical_animal_id(readLines(pth$con_animals, warn = FALSE)); con <- unique(con[!is.na(con) & nzchar(con)])
  sus <- canonical_animal_id(readLines(pth$sus_animals, warn = FALSE)); sus <- unique(sus[!is.na(sus) & nzchar(sus)])
  blocks <- s30sc_labels_from_lists(blocks, sus, con)
  cz <- data.table::fread(pth$later_outcome_combz, select = c("AnimalNum", "Sex", "Batch", "outcome_group"),
                          colClasses = c(AnimalNum = "character"))
  cz[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), Batch_cz = paste0("B", Batch))]
  blocks <- merge(blocks, cz[, list(AnimalNum, Sex, Batch_cz, outcome_group)], by = "AnimalNum", all.x = TRUE)
  gate("Group from the canonical lists = CombZ outcome_group; Batch = CombZ Batch",
       !anyNA(blocks$Sex) && all(blocks$Group == blocks$outcome_group) && all(blocks$Batch == blocks$Batch_cz))
  r14 <- data.table::as.data.table(stage14_roster)[, list(AnimalNum = as.character(AnimalNum),
                                                           Group14 = as.character(Group), Sex14 = as.character(Sex))]
  chk <- merge(unique(blocks[, list(AnimalNum, Group, Sex)]), unique(r14), by = "AnimalNum", all = TRUE)
  gate("Group and Sex = Stage 14 roster for all 111 animals",
       nrow(chk) == 111L && !anyNA(chk) && all(chk$Group == chk$Group14) && all(chk$Sex == chk$Sex14))
  con_cage <- unique(blocks[Group == "CON" & phase == "A1", list(Batch, CC, CageEpisodeID, AnimalNum)])
  gate("CON: one intact 4-animal cage per batch at every CC, same animals across CCs",
       all(con_cage[, .N, by = list(Batch, CC)]$N == 4L) &&
         all(con_cage[, data.table::uniqueN(CageEpisodeID), by = list(Batch, CC)]$V1 == 1L) &&
         all(con_cage[, list(s = paste(sort(AnimalNum), collapse = ";")), by = list(Batch, CC)][, data.table::uniqueN(s), by = Batch]$V1 == 1L))
  gate("no cage episode mixes CON and SIS animals", all(blocks[, data.table::uniqueN(Group == "CON"), by = CageEpisodeID]$V1 == 1L))
  stop_if_failed("design")

  # Reference gates on the A1 / L1 blocks (1e-9).
  cmp <- function(rec, ref, key, cols, label) {
    ref <- data.table::as.data.table(ref)
    sub <- merge(unique(ref[, key, with = FALSE]), data.table::as.data.table(rec), by = key)
    r <- s32w_compare(sub, ref, key, cols, tol = S30SC_TOL)
    gate(paste0(label, " (", nrow(ref), " reference rows)"), all(r$passed) && nrow(sub) == nrow(ref) && !anyDuplicated(ref[, key, with = FALSE]),
         paste(r[, sprintf("%s %.2g", column, max_abs_diff)], collapse = ", "))
  }
  rec_a1 <- merge(blocks[phase == "A1"], blocks[phase == "L1", list(AnimalNum, CC, light_phase_crossing_rate = crossing_rate)],
                  by = c("AnimalNum", "CC"))
  b1_path <- dhm_bundle_file(pth$ebb_v101_dir, "B1_animal_longitudinal.csv", MMM_DHM_INPUT_SHA256[["ebb_v101_manifest"]])
  inp(b1_path, "ebb_v101_B1_animal_longitudinal")
  b1 <- data.table::fread(b1_path, colClasses = list(character = c("AnimalNum", "CageEpisodeID")))
  b1[, AnimalNum := canonical_animal_id(AnimalNum)]
  cmp(rec_a1, b1, c("AnimalNum", "CC"),
      c("CageEpisodeID", "n_in_cage", "n_tracked_mates", "obs_s", "n_events", "n_bouts", "crossing_rate", "shared_zone_use",
        "occupancy_dispersion", "fragmentation", "light_phase_crossing_rate", "Sex", "Batch", "Group"),
      "ebb_v101 B1 = recomputed first dark phase of CC1-CC4")
  s29 <- data.table::fread(pth$stage29_canonical_window_metrics, colClasses = list(character = "AnimalNum"))
  s29[, AnimalNum := canonical_animal_id(AnimalNum)]
  cmp(rec_a1, s29, c("AnimalNum", "CC"),
      c("obs_s", "occupancy_dispersion", "n_events", "fragmentation", "crossing_rate", "shared_zone_use", "dyadic_obs_s",
        "System", "Batch", "CageEpisodeID", "light_phase_crossing_rate", "Sex", "Group"),
      "Stage 29 v1.0.1 canonical_window_metrics = recomputed first dark phase")
  s2_path <- dhm_bundle_file(s30b_dir, "S2_rate_inactivity_windows.csv", s30b_manifest_sha256)
  inp(s2_path, "s30b_S2_rate_inactivity_windows")
  s2 <- data.table::fread(s2_path, colClasses = list(character = "AnimalNum"))
  s2[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), phase = "L1")]
  data.table::setnames(s2, "frac", "posinact40")
  cmp(blocks, s2, c("AnimalNum", "CC", "phase"), c("crossing_rate", "posinact40", "Batch"),
      "s30b S2 = recomputed first light phase, all animals")
  fr <- data.table::as.data.table(readRDS(pth$stage30_features_full_precision))
  fr[, AnimalNum := canonical_animal_id(AnimalNum)]
  cmp(blocks, fr[, list(AnimalNum, CC, phase = "A1", posinact40 = posinact40_active, posinact60 = posinact60_active, crossing_rate,
                        occupancy_dispersion, fragmentation, shared_zone_use, CageEpisodeID)],
      c("AnimalNum", "CC", "phase"), c("posinact40", "posinact60", "crossing_rate", "occupancy_dispersion", "fragmentation",
                                       "shared_zone_use", "CageEpisodeID"),
      "Stage 30 features = recomputed first dark phase, SIS animals")
  cmp(blocks, fr[, list(AnimalNum, CC, phase = "L1", posinact40 = posinact40_light, posinact60 = posinact60_light,
                        crossing_rate = light_phase_crossing_rate)],
      c("AnimalNum", "CC", "phase"), c("posinact40", "posinact60", "crossing_rate"),
      "Stage 30 features = recomputed first light phase, SIS animals")
  stop_if_failed("reference")

  blocks[, `:=`(Batch_cz = NULL, outcome_group = NULL)]
  list(blocks = blocks[], gates = do.call(rbind, gates), inputs = do.call(rbind, inputs))
}

# Per-animal values for one heatmap window from the canonical blocks:
# cc1_A1 / cc1_L1 = that single block; clean_all = equal-weight mean of each
# epoch's clean blocks (CC1-CC3: 4 dark / 3 light; CC4: 2 dark / 1 light).
dhm_canonical_window_values <- function(blocks, window_set = c("cc1_A1", "cc1_L1", "clean_all")) {
  window_set <- match.arg(window_set)
  b <- data.table::as.data.table(blocks)[complete_primary == TRUE & in_clean_set == TRUE]
  b[, PhaseClass := ifelse(substr(phase, 1, 1) == "A", "Active", ifelse(substr(phase, 1, 1) == "L", "Inactive", NA_character_))]
  if (anyNA(b$PhaseClass)) stop("Unexpected phase labels in the canonical blocks.", call. = FALSE)
  if (window_set == "cc1_A1") b <- b[CC == "CC1" & phase == "A1"]
  if (window_set == "cc1_L1") b <- b[CC == "CC1" & phase == "L1"]
  fmean <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  out <- b[, list(
    n_blocks = .N,
    crossing_rate = mean(crossing_rate),
    shared_zone_use = fmean(shared_zone_use),
    occupancy_dispersion = mean(occupancy_dispersion),
    posinact40 = mean(posinact40),
    posinact60 = mean(posinact60),
    n_tracked_mates = n_tracked_mates[1],
    obs_s = sum(obs_s)
  ), by = list(AnimalNum, Group, Sex, Batch, CC, PhaseClass, CageEpisodeID)]
  if (anyDuplicated(out[, list(AnimalNum, CC, PhaseClass)])) {
    stop("A canonical epoch has more than one cage episode.", call. = FALSE)
  }
  expected_blocks <- if (window_set == "clean_all") {
    ifelse(out$CC == "CC4", ifelse(out$PhaseClass == "Active", 2L, 1L), ifelse(out$PhaseClass == "Active", 4L, 3L))
  } else 1L
  if (!all(out$n_blocks == expected_blocks)) stop("Canonical epochs do not hold the expected clean blocks.", call. = FALSE)
  tibble::as_tibble(out) %>%
    mutate(window_set = window_set, CageChange = CC, CageChangeIndex = as.integer(sub("^CC", "", CC)))
}

# ---------------------------------------------------------------- tier tests (post hoc)
# One fit with the registered engine. A stop() (rank, expected rank, dropped
# columns) becomes an explicit FAILED stub, as in phc_fit(); unlike phc_fit()
# the engine's own failure rule applies (failed = error, or non-converged
# with disagreeing optimizers).
dhm_fit <- function(d, formula, model_id, expected_rank) {
  m <- tryCatch(mmm_ci_fit(formula, d, model_id, expected_rank = expected_rank), error = function(e) e)
  if (inherits(m, "error")) {
    info <- data.table::data.table(model_id = model_id, engine = "lmer", formula = formula, dispformula = NA_character_,
      n_obs = nrow(d), n_animals = data.table::uniqueN(d$AnimalNum), n_cage_episodes = data.table::uniqueN(d$CageEpisodeID),
      n_batches = data.table::uniqueN(d$Batch), n_fixed_cols = NA_integer_, rank = NA_integer_, expected_rank = expected_rank,
      singular = NA, converged = FALSE, messages = "", optimizer_check_agree = NA, failed = TRUE,
      optimizer_check_messages = NA_character_, error = conditionMessage(m))
    return(list(fit = NULL, info = info, data = d))
  }
  m
}

# The two models for one row x phase x sex slice. d must be coded by
# mmm_ci_code_design() on ALL animals of the window before it is sliced, and
# carry y. Every contrast is a Kenward-Roger t of its model (one rule, no
# switching). Returns list(contrasts = 3 rows, fits = 2 rows).
dhm_tier_tests <- function(d, kind = c("single", "pooled"), label) {
  kind <- match.arg(kind)
  spec <- MMM_DHM_MODELS[[kind]]
  d <- data.table::as.data.table(d)[is.finite(y)]
  d[, group := factor(Group, levels = c("CON", "RES", "SUS"))]
  rs_dat <- d[Group != "CON"]
  m_rs <- dhm_fit(rs_dat, spec$rs$formula, paste(label, "RES-SUS (SIS animals)", sep = " | "), spec$rs$rank)
  m_con <- dhm_fit(d, spec$con$formula, paste(label, "CON contrasts (all animals)", sep = " | "), spec$con$rank)
  one <- function(m, contrast) {
    r <- phc_kr_guard(mmm_ci_contrast(m, MMM_DHM_CONTRAST_WEIGHTS[[contrast]], contrast))
    r[, contrast := contrast][]
  }
  rows <- data.table::rbindlist(list(one(m_rs, "RES-SUS"), one(m_con, "RES-CON"), one(m_con, "SUS-CON")), fill = TRUE)
  rows[, `:=`(kr_df = df, test_used = "KR t",
              unit_of_analysis = ifelse(contrast == "RES-SUS", "SIS animals; cage episode as random effect",
                                        paste("SIS cage episode as random effect; CON", MMM_DHM_CON_UNIT_NOTE)))]
  fits <- data.table::rbindlist(list(m_rs$info[, model := "RES-SUS (SIS animals)"], m_con$info[, model := "CON contrasts (all animals)"]),
                                fill = TRUE)
  list(contrasts = rows[], fits = fits[])
}

# ---------------------------------------------------------------- families and markers
# BH within heatmap x variant x tier family x phase over the post hoc cells
# (a FAILED or not-estimable test enters with p = 1, the Stage 30 convention);
# consumed registered cells, registered estimation-only cells and undefined
# cells are not members. Sensitivity columns (never used for markers): a family
# pooling both phases, and RES-SUS-only families (the CON contrasts, whose
# animals are the units, share the main families with RES-SUS and lower its q).
# A cell with reportable = FALSE (e.g. the HMM row, KNOWN_LIMITATIONS 1) keeps
# its test but never gets a marker. sign_agrees is NA where no estimate exists.
# No global correction.
dhm_add_families <- function(cells, alpha = MMM_DHM_ALPHA) {
  if (!"reportable" %in% names(cells)) cells$reportable <- TRUE
  cells %>%
    mutate(
      posthoc_member = .data$test_source == "posthoc",
      res_sus_member = .data$posthoc_member & .data$contrast == "RES-SUS",
      p_for_bh = if_else(.data$posthoc_member & .data$test_status %in% "OK" & is.finite(.data$p_raw), .data$p_raw, 1),
      family_id = if_else(.data$posthoc_member,
                          paste(.data$heatmap_id, .data$variant, .data$stat_family, .data$PhaseClass, sep = "|"), NA_character_),
      family_id_phase_pooled = if_else(.data$posthoc_member,
                                       paste(.data$heatmap_id, .data$variant, .data$stat_family, "both_phases", sep = "|"), NA_character_)
    ) %>%
    group_by(.data$family_id) %>%
    mutate(
      family_m = if_else(.data$posthoc_member, sum(.data$posthoc_member), NA_integer_),
      q_bh = if_else(.data$posthoc_member, stats::p.adjust(.data$p_for_bh, method = "BH"), NA_real_)
    ) %>%
    group_by(.data$family_id_phase_pooled) %>%
    mutate(
      family_m_phase_pooled = if_else(.data$posthoc_member, sum(.data$posthoc_member), NA_integer_),
      q_bh_phase_pooled = if_else(.data$posthoc_member, stats::p.adjust(.data$p_for_bh, method = "BH"), NA_real_)
    ) %>%
    group_by(.data$family_id, .data$res_sus_member) %>%
    mutate(q_bh_res_sus_only = if_else(.data$res_sus_member, stats::p.adjust(.data$p_for_bh, method = "BH"), NA_real_)) %>%
    group_by(.data$family_id_phase_pooled, .data$res_sus_member) %>%
    mutate(q_bh_res_sus_only_phase_pooled = if_else(.data$res_sus_member, stats::p.adjust(.data$p_for_bh, method = "BH"), NA_real_)) %>%
    ungroup() %>%
    mutate(
      adjusted_p = case_when(
        .data$test_source == "posthoc" ~ .data$q_bh,
        .data$test_source == "registered" ~ .data$registered_p_adjusted,
        TRUE ~ NA_real_
      ),
      evidence = is.finite(.data$adjusted_p) & .data$adjusted_p < alpha,
      sign_agrees = if_else(is.finite(.data$estimate) & is.finite(.data$hedges_g) & .data$estimate != 0 & .data$hedges_g != 0,
                            sign(.data$estimate) == sign(.data$hedges_g), NA),
      marker_class = case_when(
        !.data$reportable ~ NA_character_,
        .data$evidence & .data$sign_agrees %in% TRUE & .data$test_source == "posthoc" ~ "posthoc",
        .data$evidence & .data$sign_agrees %in% TRUE & .data$test_source == "registered" ~ "registered",
        .data$evidence & .data$sign_agrees %in% FALSE ~ "sign_conflict",
        TRUE ~ NA_character_
      ),
      marker = .data$marker_class %in% c("posthoc", "registered"),
      global_correction_used = FALSE
    ) %>%
    select(-"res_sus_member")
}

# Display classes of the marker layer (fixed limits so legends merge across
# heatmaps even when a heatmap has no marker).
MMM_DHM_MARKER_LEVELS <- c(posthoc = "post hoc q < 0.05",
                           registered = "registered adj. p < 0.05",
                           sign_conflict = "adj. p < 0.05, opposite sign")

# Leave-one-CON-cage-out Hedges g of one CON contrast in one slice. The CON
# animals of a sex live in one stable cage per batch, so leaving out one
# batch's CON animals leaves out one CON cage. animal_means: one row per animal
# with Group, Batch and m (the animal's mean row score). Descriptive: the
# scores are not re-standardised.
dhm_con_cage_loo <- function(animal_means, comp) {
  am <- animal_means[is.finite(animal_means$m), , drop = FALSE]
  con <- am[am$Group == "CON", , drop = FALSE]
  comp_values <- am$m[am$Group == comp]
  cages <- sort(unique(as.character(con$Batch)))
  tibble::tibble(con_cage = cages,
                 g_without = vapply(cages, function(cb) hmm_hedges_g(con$m[con$Batch != cb], comp_values), numeric(1), USE.NAMES = FALSE))
}

# Robustness of one post hoc marker. A check counts only where it can fail:
# the phase-pooled family (not for a single-phase window), the other
# resolution variants (not for a bin-free row, identical in every variant),
# for a CON contrast every leave-one-CON-cage-out g (same sign and at least
# half the size), and for RES-SUS the RES-SUS-only families. Returns
# "robust: <checks>" or "NOT robust (fails: <failed checks>): <checks>".
dhm_marker_robustness <- function(contrast, g, single_phase, bin_free, q_pooled, other_variants, g_without_cage = NULL,
                                  q_rs_only = NA_real_, q_rs_only_pooled = NA_real_, alpha = MMM_DHM_ALPHA) {
  fmt_g <- function(x) dhm_minus(x)
  checks <- character(); failed <- character()
  if (single_phase) {
    checks <- c(checks, "phase-pooled family not assessable (single-phase window)")
  } else {
    checks <- c(checks, sprintf("phase-pooled family q = %.3f", q_pooled))
    if (!(is.finite(q_pooled) && q_pooled < alpha)) failed <- c(failed, "phase-pooled family")
  }
  if (bin_free) {
    checks <- c(checks, "resolution variants not assessable (bin-free row, identical in every variant)")
  } else {
    checks <- c(checks, sprintf("%s: g = %s, q = %.3f%s", other_variants$variant, fmt_g(other_variants$hedges_g), other_variants$q_bh,
                                ifelse(other_variants$marker %in% TRUE, "", " (no marker)")))
    if (!all(other_variants$marker %in% TRUE)) {
      failed <- c(failed, paste0("bin width (", paste(other_variants$variant[!other_variants$marker %in% TRUE], collapse = ", "), ")"))
    }
  }
  if (contrast == "RES-SUS") {
    checks <- c(checks, if (single_phase) sprintf("RES-SUS-only family q = %.3f", q_rs_only) else
      sprintf("RES-SUS-only family q = %.3f (phase-pooled %.3f)", q_rs_only, q_rs_only_pooled))
    if (!(is.finite(q_rs_only) && q_rs_only < alpha && (single_phase || (is.finite(q_rs_only_pooled) && q_rs_only_pooled < alpha)))) {
      failed <- c(failed, "RES-SUS-only family")
    }
  } else {
    gw <- g_without_cage
    pass_c <- length(gw) > 0 && all(is.finite(gw)) && all(sign(gw) == sign(g)) && all(abs(gw) >= 0.5 * abs(g))
    checks <- c(checks, paste0("without each CON cage: ", paste(sprintf("%s g = %s", names(gw), fmt_g(gw)), collapse = ", ")))
    if (!pass_c) {
      worst <- names(gw)[which.min(ifelse(is.finite(gw), sign(g) * gw, -Inf))]
      failed <- c(failed, paste0("one CON cage (without ", worst, ")"))
    }
  }
  if (length(failed) == 0L) paste0("robust: ", paste(checks, collapse = "; ")) else
    paste0("NOT robust (fails: ", paste(failed, collapse = ", "), "): ", paste(checks, collapse = "; "))
}

# ---------------------------------------------------------------- single-window bin features
# Per-animal bin features of one 12-h window (rows selected by
# mmm_select_first_night_window or mmm_select_acute_phase_window, which add
# target_slot). RMSSD and ACF1 use adjacent slots only (the first-night
# estimators); a complete window has no gap, so these equal Stage 14's epoch
# estimators. The switching rate follows Stage 12 (Analysis/12): a bin is
# inactive-like when Movement <= the animal's 20th percentile over all its bins
# at that resolution, and the rate is the share of adjacent bin pairs that
# switch.
dhm_window_bin_features <- function(sel, movement_q20) {
  need <- c("AnimalNum", "Group", "Sex", "target_slot", "Movement", "Entropy", "ProximityFraction")
  miss <- setdiff(need, names(sel))
  if (length(miss)) stop("Window rows lack: ", paste(miss, collapse = ", "), call. = FALSE)
  sel %>%
    mutate(AnimalNum = as.character(.data$AnimalNum)) %>%
    left_join(movement_q20, by = "AnimalNum") %>%
    group_by(AnimalNum, Group, Sex) %>%
    arrange(.data$target_slot, .by_group = TRUE) %>%
    summarise(
      n_slots = n(),
      Movement_mean = mean(Movement, na.rm = TRUE),
      Movement_rmssd = mmm_rmssd_adjacent(Movement, target_slot),
      Movement_acf1 = mmm_acf1_adjacent(Movement, target_slot),
      Entropy_mean = mean(Entropy, na.rm = TRUE),
      Entropy_rmssd = mmm_rmssd_adjacent(Entropy, target_slot),
      Entropy_acf1 = mmm_acf1_adjacent(Entropy, target_slot),
      Proximity_mean = if (any(is.finite(ProximityFraction))) mean(ProximityFraction, na.rm = TRUE) else NA_real_,
      Proximity_rmssd = mmm_rmssd_adjacent(ProximityFraction, target_slot),
      Proximity_acf1 = mmm_acf1_adjacent(ProximityFraction, target_slot),
      switch_rate = {
        p <- mmm_adjacent_pairs(as.numeric(Movement <= movement_q20[1]), target_slot)
        if (p$n_pairs >= 2L) mean(p$x != p$y) else NA_real_
      },
      .groups = "drop"
    )
}

# ---------------------------------------------------------------- HMM display (panel F)
# "No position change" = the vendor reported no RFID relocation (>= 200 grid
# units) for the animal in the bin; it is not immobility and not sleep.
MMM_HMM_DISPLAY_STATE_LEVELS <- c(
  "Many position changes", "Few position changes", "Position changes",
  "No position change, co-located", "No position change, apart", "No position change"
)

# Display names derived from the state means only, never from state numbers:
# no-change states are the composite's inactive states; apart / co-located is
# the sign of the state's mean Proximity_z; changing states are ranked by mean
# Movement_z. Fails closed for fits with "social" or "exploratory" states.
hmm_state_display_labels <- function(state_labels, split_no_change_by_proximity = TRUE) {
  need <- c("State", "Movement_z", "Entropy_z", "Proximity_z", "SemanticState")
  missing_cols <- setdiff(need, names(state_labels))
  if (length(missing_cols) > 0L) stop("HMM state labels are missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  unsupported <- setdiff(unique(state_labels$SemanticState), c("inactive/low-exploration", "burst/high-movement", "mixed"))
  if (length(unsupported) > 0L) {
    stop("HMM display labels are defined for inactive, burst and mixed states only; this fit also has: ",
         paste(unsupported, collapse = ", "), ". Re-derive the display rule before plotting.", call. = FALSE)
  }
  no_change <- state_labels$SemanticState == "inactive/low-exploration"
  if (!any(no_change) || all(no_change)) stop("HMM display labels need at least one no-change and one changing state.", call. = FALSE)
  if (max(state_labels$Movement_z[no_change]) >= min(state_labels$Movement_z[!no_change])) {
    stop("HMM no-change states must have the lowest mean Movement_z.", call. = FALSE)
  }
  n_changing <- sum(!no_change)
  changing_rank <- rank(-replace(state_labels$Movement_z, no_change, -Inf), ties.method = "first")
  state_labels %>%
    mutate(
      State = as.character(.data$State),
      DisplayState = case_when(
        no_change & !split_no_change_by_proximity ~ "No position change",
        no_change & .data$Proximity_z < 0 ~ "No position change, apart",
        no_change ~ "No position change, co-located",
        n_changing == 1L ~ "Position changes",
        changing_rank == 1L ~ "Many position changes",
        TRUE ~ "Few position changes"
      ),
      display_rule = paste(
        "no position change = SemanticState inactive/low-exploration;",
        if (split_no_change_by_proximity) "apart/co-located = sign of mean Proximity_z;" else "no-change states merged;",
        "changing states ranked by mean Movement_z"
      )
    )
}

# Per-animal time budget by display state: frac_time summed within the display
# state per Animal x CC x Phase epoch (absent states count 0; Stage 08 omits
# zero-count states), then averaged over the animal's cage changes. Every
# Animal x PhaseClass budget sums to 1 (asserted).
hmm_display_time_budget <- function(occupancy, display_labels) {
  need <- c("AnimalNum", "Group", "Sex", "Phase", "CageChange", "State", "frac_time")
  missing_cols <- setdiff(need, names(occupancy))
  if (length(missing_cols) > 0L) stop("HMM occupancy is missing: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  lab <- display_labels %>% transmute(State = as.character(.data$State), DisplayState = as.character(.data$DisplayState))
  dat <- occupancy %>%
    mutate(
      AnimalNum = as.character(.data$AnimalNum), Group = as.character(.data$Group), Sex = as.character(.data$Sex),
      PhaseClass = mmm_phase_class(.data$Phase), CageChange = as.character(.data$CageChange),
      State = as.character(.data$State), frac_time = suppressWarnings(as.numeric(.data$frac_time))
    ) %>%
    left_join(lab, by = "State")
  if (anyNA(dat$DisplayState)) {
    stop("HMM occupancy has states without a display label: ", paste(unique(dat$State[is.na(dat$DisplayState)]), collapse = ", "),
         call. = FALSE)
  }
  out <- dat %>%
    group_by(AnimalNum, Group, Sex, PhaseClass, CageChange, DisplayState) %>%
    summarise(share = sum(frac_time), .groups = "drop") %>%
    tidyr::complete(tidyr::nesting(AnimalNum, Group, Sex, PhaseClass, CageChange), DisplayState = unique(lab$DisplayState),
                    fill = list(share = 0)) %>%
    group_by(AnimalNum, Group, Sex, PhaseClass, DisplayState) %>%
    summarise(share = mean(share), n_cage_changes = n_distinct(CageChange), .groups = "drop")
  totals <- out %>% group_by(AnimalNum, PhaseClass) %>% summarise(total = sum(share), .groups = "drop")
  if (any(abs(totals$total - 1) > 1e-9)) stop("HMM time budget does not sum to 1 for every animal and phase.", call. = FALSE)
  out
}

# Wrap text to a printed width: about width_mm / (em x size_pt) characters per
# line (em = 0.5 is a safe upper bound of Arial's average glyph width).
dhm_wrap_text <- function(x, width_mm, size_pt, em = 0.5) {
  n <- max(20L, floor(width_mm / (em * size_pt * 25.4 / 72)))
  vapply(x, function(s) paste(unlist(lapply(strsplit(s, "\n", fixed = TRUE)[[1]], strwrap, width = n)), collapse = "\n"), "",
         USE.NAMES = FALSE)
}

# Fixed-decimal labels with a true minus sign; values that round to 0 print as
# 0 (never "-0.00").
dhm_minus <- function(x, digits = 2) {
  r <- round(x, digits)
  r[!is.na(r) & r == 0] <- 0
  sub("^-", "−", formatC(r, format = "f", digits = digits))
}

# ---------------------------------------------------------------- manuscript figure format (palette v2)
# The manuscript's figure format (Exp9_manuscript config/manuscript_palette.yml), so the Stage 14 figures match the
# manuscript's own figure renderers without reading another repository at run time. Group and diverging colours and
# the source pin come from Functions/manuscript_palette.R, the single colour source; the remaining roles, typography,
# line widths and canvas are copied from the same file (they are identical in v1 and v2).
# Text and line sizes are final-size points on a canvas of at most 183 x 170
# mm; line widths use the manuscript renderers' pt -> linewidth convention
# (x 0.75); explanatory text belongs in the figure legend, not in the artwork.
if (!exists("MMM_PALETTE_GROUP", inherits = TRUE)) {
  .mmm_palette_file <- file.path(c(if (exists("MMM_REPO_ROOT", inherits = TRUE)) get("MMM_REPO_ROOT", inherits = TRUE), getwd()),
                                 "Functions", "manuscript_palette.R")
  .mmm_palette_file <- .mmm_palette_file[file.exists(.mmm_palette_file)][1]
  if (is.na(.mmm_palette_file)) stop("Functions/manuscript_palette.R not found; run from the repository root", call. = FALSE)
  source(.mmm_palette_file, local = TRUE)
  rm(.mmm_palette_file)
}
MMM_DHM_PALETTE <- list(
  version = MMM_PALETTE_VERSION,
  source = paste0(MMM_PALETTE_SOURCE$repository, " ", MMM_PALETTE_SOURCE$file, " @ master ", substr(MMM_PALETTE_SOURCE$commit, 1, 7)),
  # source_sha256: bytes of the CRLF working copy on S:; source_git_blob: the line-ending independent identity
  source_sha256 = MMM_PALETTE_SOURCE$sha256,
  source_git_blob = MMM_PALETTE_SOURCE$git_blob,
  group = MMM_PALETTE_GROUP,
  diverging = MMM_PALETTE_DIVERGING,
  evidence = c(supported = "#1F3D52", descriptive = "#B9B9B4", not_evaluable = "#D8D6D0", not_audited = "#E6E4DF",
               qc_context = "#B08968"),
  claimability = c(claimable = "#1F3D52", claimable_with_caveat = "#5B7C93", not_claimable = "#D1543A"),
  typography = list(family = "Arial", panel_label_pt = 8, axis_text_pt = 5.2, axis_title_pt = 6, legend_text_pt = 5.2,
                    legend_title_pt = 5.6, annotation_pt = 5, minimum_pt = 5),
  line = list(axis_pt = 0.3, tile_border_pt = 0.15, reference_pt = 0.25, data_pt = 0.5),
  canvas = list(width_mm = 183, max_height_mm = 170)
)
# Batch identity (not a measured value), used only in the early-movement
# panel: the palette defines no batch colours, so three of its dark neutral
# tones (from the evidence/claimability sets) are borrowed for identity only;
# they differ from the group colours. The two sexes are separate panels, so
# female and male batches reuse the tones (B1 = B3, B5 = B4, B2 = B6).
MMM_DHM_BATCH_COLOURS <- c(B1 = "#1F3D52", B2 = "#B08968", B5 = "#5B7C93", B3 = "#1F3D52", B4 = "#5B7C93", B6 = "#B08968")
dhm_pt <- function(key) MMM_DHM_PALETTE$typography[[key]]
dhm_size <- function(pt) pt * 0.3527777          # text size: pt -> ggplot mm
dhm_lw <- function(pt) pt * 0.75                 # line width: pt -> ggplot linewidth (manuscript convention)

dhm_theme <- function(base = dhm_pt("axis_text_pt")) {
  ggplot2::theme_bw(base_size = base, base_family = MMM_DHM_PALETTE$typography$family) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank(), plot.caption = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      axis.line = ggplot2::element_line(linewidth = dhm_lw(MMM_DHM_PALETTE$line$axis_pt), colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = dhm_lw(MMM_DHM_PALETTE$line$axis_pt), colour = "black"),
      axis.ticks.length = ggplot2::unit(0.6, "mm"),
      axis.text = ggplot2::element_text(size = dhm_pt("axis_text_pt"), colour = "black"),
      axis.title = ggplot2::element_text(size = dhm_pt("axis_title_pt"), colour = "black"),
      legend.title = ggplot2::element_text(size = dhm_pt("legend_title_pt")),
      legend.text = ggplot2::element_text(size = dhm_pt("legend_text_pt")),
      legend.key.size = ggplot2::unit(2.4, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(1, "mm"),
      legend.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = dhm_pt("axis_text_pt"), colour = "black", margin = ggplot2::margin(0.4, 0, 0.4, 0, "mm")),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.8, "mm")
    )
}
dhm_theme_tile <- function() dhm_theme() + ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())

# Fill of a g value on the manuscript diverging scale (as scale_fill_gradient2
# draws it: Lab interpolation, symmetric limits, squished), and the text colour
# with the higher WCAG contrast on that fill.
dhm_fill_hex <- function(g, limit) {
  pal <- scales::div_gradient_pal(MMM_DHM_PALETTE$diverging[["low"]], MMM_DHM_PALETTE$diverging[["mid"]],
                                  MMM_DHM_PALETTE$diverging[["high"]], "Lab")
  out <- rep(NA_character_, length(g))
  ok <- is.finite(g)
  out[ok] <- pal(pmin(1, pmax(0, (g[ok] + limit) / (2 * limit))))
  out
}
dhm_text_on <- function(fill_hex) {
  lum <- function(hex) {
    v <- grDevices::col2rgb(hex) / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    as.numeric(0.2126 * v[1, ] + 0.7152 * v[2, ] + 0.0722 * v[3, ])
  }
  out <- rep("black", length(fill_hex))
  ok <- !is.na(fill_hex)
  if (any(ok)) {
    l <- lum(fill_hex[ok])
    out[ok] <- ifelse((1.05) / (l + 0.05) > (l + 0.05) / (0.0 + 0.05), "white", "black")
  }
  out
}

# Light-phase blocks of a heatmap window without any position update (n_events
# = 0). Each may be consolidated rest in one position (alone or with
# cage-mates) or an undetected tag; the vendor position stream cannot tell the
# two apart (KNOWN_LIMITATIONS 3). Same block selection as
# dhm_canonical_window_values().
dhm_zero_event_blocks <- function(blocks, window_set = c("cc1_A1", "cc1_L1", "clean_all")) {
  window_set <- match.arg(window_set)
  b <- data.table::as.data.table(blocks)[complete_primary == TRUE & in_clean_set == TRUE & substr(phase, 1, 1) == "L"]
  if (!"n_events" %in% names(b)) stop("The canonical blocks lack n_events.", call. = FALSE)
  if (window_set == "cc1_A1") b <- b[0]
  if (window_set == "cc1_L1") b <- b[CC == "CC1" & phase == "L1"]
  b[n_events == 0, list(AnimalNum = as.character(AnimalNum), Group, Sex, Batch, CC, phase, CageEpisodeID)]
}
