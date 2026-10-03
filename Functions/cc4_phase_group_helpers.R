# Stage 31b: fixed-clock group summaries. All uncertainty is calculated here,
# upstream of the manuscript renderer. No inferred grid windows enter this path.

grid31_group_phases <- function(bins, outcomes) {
  grid31_required(outcomes, c("AnimalNum", "Sex", "Batch", "outcome_group", "experimental_condition"), "Canonical outcomes")
  if (anyNA(outcomes$AnimalNum) || anyDuplicated(outcomes$AnimalNum)) stop("Duplicate/missing outcome identity.", call. = FALSE)
  outcomes <- outcomes %>% dplyr::transmute(AnimalNum = as.character(AnimalNum), OutcomeSex = Sex,
    OutcomeBatch = paste0("B", Batch), Group = outcome_group, Condition = experimental_condition)
  if (anyNA(outcomes) || any(!outcomes$Group %in% c("CON", "RES", "SUS")) ||
      any((outcomes$Group == "CON") != (outcomes$Condition == "CON"))) stop("Invalid canonical groups/conditions.", call. = FALSE)
  ai <- grid31_whole_phase(bins, "Inactive", 2:5, 2L)$animals %>% dplyr::mutate(Phase = "Inactive", Reference = "I2")
  aa <- grid31_whole_phase(bins, "Active", 1:5, 2L)$animals %>% dplyr::mutate(Phase = "Active", Reference = "A2")
  a <- dplyr::bind_rows(ai, aa) %>% dplyr::left_join(outcomes, by = "AnimalNum")
  if (anyNA(a$Group) || any(a$Sex != a$OutcomeSex | a$Batch != a$OutcomeBatch)) stop("Missing or inconsistent canonical outcome match.", call. = FALSE)
  identity <- a %>% dplyr::group_by(AnimalKey) %>% dplyr::summarise(NinePhases = n() == 9L,
    OneCage = n_distinct(CageID) == 1L, .groups = "drop")
  if (any(!identity$NinePhases | !identity$OneCage)) stop("Animal roster/cage differs between phase families.", call. = FALSE)
  # Common roster across all nine displayed phases, including A1 context.
  roster <- a %>% dplyr::group_by(CageID) %>%
    dplyr::summarise(CommonCage = all(IncludedInPrimary), .groups = "drop")
  a <- a %>% dplyr::rename(IncludedInFamily = IncludedInPrimary) %>% dplyr::left_join(roster, by = "CageID") %>%
    dplyr::mutate(IncludedInPrimary = CommonCage)
  base <- a %>% dplyr::filter(PhaseLabel == Reference) %>%
    dplyr::select(AnimalKey, Phase, BaselineRate = CrossingsPerHour)
  a <- a %>% dplyr::left_join(base, by = c("AnimalKey", "Phase")) %>%
    dplyr::mutate(ChangeFromBaseline = CrossingsPerHour - BaselineRate) %>%
    dplyr::arrange(Sex, Batch, CageID, AnimalKey, Phase, PhaseLabel)
  cages <- a %>% dplyr::filter(IncludedInPrimary) %>%
    dplyr::group_by(Sex, Batch, CageID, Group, Phase, PhaseLabel, Reference) %>%
    dplyr::summarise(Animals = n(), Rate = mean(CrossingsPerHour), Change = mean(ChangeFromBaseline), .groups = "drop")
  batch <- cages %>% dplyr::group_by(Sex, Batch, Group, Phase, PhaseLabel, Reference) %>%
    dplyr::summarise(Animals = sum(Animals), Cages = n(), Rate = mean(Rate), Change = mean(Change), .groups = "drop")
  expected <- a %>% dplyr::distinct(Sex, Batch) %>% tidyr::crossing(Group = c("CON", "RES", "SUS"),
    PhaseLabel = c(paste0("I", 2:5), paste0("A", 1:5)))
  if (nrow(dplyr::anti_join(expected, batch, by = c("Sex", "Batch", "Group", "PhaseLabel"))))
    stop("Missing complete group in a batch/phase; cannot silently change batch weights.", call. = FALSE)
  pop <- a %>% dplyr::group_by(Sex, Batch, Group, Phase, PhaseLabel) %>%
    dplyr::summarise(RecordedAnimals = n(), EligibleAnimals = sum(EligiblePhase), PrimaryAnimals = sum(IncludedInPrimary),
      PrimaryCages = n_distinct(CageID[IncludedInPrimary]), .groups = "drop")
  # Paired group differences are formed WITHIN the same batch before combining.
  contrasts <- lapply(list(c("RES", "CON"), c("SUS", "CON"), c("SUS", "RES")), function(pair) {
    x <- batch %>% dplyr::filter(Group == pair[1])
    y <- batch %>% dplyr::filter(Group == pair[2]) %>%
      dplyr::select(Sex, Batch, Phase, PhaseLabel, Reference, Rate0 = Rate, Change0 = Change)
    dplyr::left_join(x, y, by = c("Sex", "Batch", "Phase", "PhaseLabel", "Reference")) %>%
      dplyr::transmute(Sex, Batch, Phase, PhaseLabel, Reference, Contrast = paste(pair, collapse = "-"),
        RateDifference = Rate - Rate0, ChangeDifference = Change - Change0)
  }) %>% dplyr::bind_rows()
  group_long <- batch %>% tidyr::pivot_longer(c("Rate", "Change"), names_to = "Measure", values_to = "Value")
  contrast_long <- contrasts %>% tidyr::pivot_longer(c("RateDifference", "ChangeDifference"), names_to = "Measure", values_to = "Value")
  list(animals = a, cages = cages, batch = batch, population = pop, batch_contrasts = contrasts,
       summary = grid31_batch_intervals(group_long, "Group"),
       contrasts = grid31_batch_intervals(contrast_long, "Contrast"))
}

grid31_batch_intervals <- function(x, identity_col) {
  keys <- c("Sex", identity_col, "Phase", "PhaseLabel", "Reference", "Measure")
  if (anyDuplicated(x[c(keys, "Batch")]) || any(!is.finite(x$Value))) stop("Invalid paired batch estimates.", call. = FALSE)
  x %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(Batches = n(), Estimate = mean(Value), BatchSD = stats::sd(Value), .groups = "drop") %>%
    dplyr::mutate(SE = BatchSD / sqrt(Batches), DF = Batches - 1L,
      Margin = ifelse(Batches >= 3, stats::qt(0.975, pmax(DF, 1L)) * SE, NA_real_),
      Lower95 = Estimate - Margin, Upper95 = Estimate + Margin,
      IntervalStatus = ifelse(Batches >= 3, "DESCRIPTIVE_T_INTERVAL_ACROSS_BATCHES", "NOT_ESTIMABLE_FEWER_THAN_THREE_BATCHES"),
      Estimand = "equal_batch_mean_of_equal_cage_group_means; position_changes_per_animal_hour",
      Interval = "pointwise_95_t_interval; independent_approximately_normal_batch_estimates; not_multiplicity_adjusted") %>%
    dplyr::select(-Margin)
}

grid31_verify_manifest <- function(root, manifest = "audit/output_manifest.csv") {
  path <- file.path(root, manifest)
  if (!file.exists(path)) stop("Source manifest missing.", call. = FALSE)
  m <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  grid31_required(m, c("File", "SHA256"), "Source manifest")
  if (!nrow(m) || anyNA(m) || anyDuplicated(m$File) || any(grepl("(^[/\\\\]|:|(^|[/\\\\])[.][.]([/\\\\]|$))", m$File)))
    stop("Unsafe or duplicate manifest paths.", call. = FALSE)
  paths <- file.path(root, m$File)
  if (!all(file.exists(paths)) || !identical(unname(mmm_file_sha256(paths)), m$SHA256)) stop("Source manifest hash mismatch.", call. = FALSE)
  m
}
