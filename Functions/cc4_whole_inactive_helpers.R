# Stage 31 fixed-clock I2-I5 activity. No inferred exposure times are inputs.
# Primary population: cages whose complete animal roster brackets all four days.
# Cage means are equally weighted within batch, then batches equally within sex.

grid31_whole_inactive <- function(bins) {
  result <- grid31_whole_phase(bins, "Inactive", 2:5, 2L)
  names(result$animal_pairs)[names(result$animal_pairs) == "ChangeFromBaseline"] <- "ChangeFromI2"
  names(result$cage_pairs)[names(result$cage_pairs) == "ChangeFromBaseline"] <- "ChangeFromI2"
  result
}

# Shared fixed-clock aggregation; the legacy wrapper above retains its schema.
grid31_whole_phase <- function(bins, phase, numbers, reference) {
  if (!phase %in% c("Inactive", "Active") || !identical(numbers, if (phase == "Inactive") 2:5 else 1:5) ||
      !identical(as.integer(reference), 2L)) stop("Unsupported whole-phase specification.", call. = FALSE)
  labels <- paste0(if (phase == "Inactive") "I" else "A", numbers)
  reference_label <- paste0(substr(phase, 1, 1), reference)
  start_clock <- if (phase == "Inactive") "06:30:00" else "18:30:00"
  keys <- c("Batch", "CageChange", "System", "AnimalID", "AnimalNum", "Sex", "PhaseLabel")
  grid31_required(bins, c(keys, "Phase", "PhaseNumber", "BlockStart", "BlockEnd", "BinIndex", "Crossings",
                          "SessionSpansWholeBlock", "AnimalSpansWholeBlock", "SpanOverlapSec"), "Animal clock bins")
  d <- bins %>% dplyr::filter(Phase == phase, PhaseNumber %in% numbers)
  if (!nrow(d)) stop("No requested whole-phase animal bins.", call. = FALSE)
  if (anyNA(d[c(keys, "Crossings", "BinIndex", "SpanOverlapSec", "SessionSpansWholeBlock", "AnimalSpansWholeBlock")]) ||
      any(!d$Sex %in% c("Male", "Female")) || any(!is.finite(d$Crossings)) ||
      any(d$Crossings < 0 | d$Crossings != floor(d$Crossings))) stop("Invalid whole-phase metadata/counts.", call. = FALSE)
  if (anyDuplicated(d[c(keys, "BinIndex")])) stop("Duplicate animal-phase bin.", call. = FALSE)
  valid <- d %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(ValidBins = identical(sort(as.integer(BinIndex)), 0:143),
      ValidWindow = dplyr::n_distinct(BlockStart) == 1L && dplyr::n_distinct(BlockEnd) == 1L &&
        all(as.numeric(BlockEnd) - as.numeric(BlockStart) == 43200) &&
        all(format(BlockStart, "%H:%M:%S", tz = "UTC") == start_clock), .groups = "drop")
  if (any(!valid$ValidBins | !valid$ValidWindow)) stop("Whole phases require 144 unique five-minute bins at the declared clock boundary.", call. = FALSE)
  if (any(d$PhaseLabel != paste0(substr(phase, 1, 1), d$PhaseNumber))) stop("Phase label/number mismatch.", call. = FALSE)
  animals <- d %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(BlockStart = first(BlockStart), BlockEnd = first(BlockEnd),
      RecordedCrossings = sum(Crossings), NominalHours = 12,
      EligiblePhase = all(SessionSpansWholeBlock & AnimalSpansWholeBlock & SpanOverlapSec >= 300), .groups = "drop") %>%
    dplyr::mutate(CageID = paste(Batch, System, CageChange, sep = "|"),
      AnimalKey = paste(Batch, AnimalNum, sep = "|"),
      CrossingsPerHour = ifelse(EligiblePhase, RecordedCrossings / NominalHours, NA_real_),
      PhaseStatus = ifelse(EligiblePhase, "complete_clock_and_animal_span", "incomplete_clock_or_animal_span"))
  identity <- animals %>% dplyr::group_by(AnimalKey) %>%
    dplyr::summarise(nc = n_distinct(CageID), ns = n_distinct(Sex), .groups = "drop")
  if (any(identity$nc != 1L | identity$ns != 1L)) stop("Unstable cage/sex identity across whole phases.", call. = FALSE)
  balance <- animals %>% dplyr::group_by(AnimalKey) %>%
    dplyr::summarise(FourPhases = identical(sort(PhaseLabel), labels),
                     BalancedAnimal = all(EligiblePhase), .groups = "drop")
  if (any(!balance$FourPhases)) stop("Missing ", paste(range(labels), collapse = "-"), " phase rows for an animal.", call. = FALSE)
  animals <- animals %>% dplyr::left_join(balance %>% dplyr::select(-FourPhases), by = "AnimalKey")
  cage_balance <- animals %>% dplyr::group_by(CageID) %>%
    dplyr::summarise(BalancedCage = all(BalancedAnimal), .groups = "drop")
  animals <- animals %>% dplyr::left_join(cage_balance, by = "CageID") %>%
    dplyr::mutate(IncludedInPrimary = BalancedAnimal & BalancedCage)
  cages <- animals %>% dplyr::group_by(Sex, Batch, CageChange, System, CageID, PhaseLabel) %>%
    dplyr::summarise(ExpectedAnimals = n(), EligibleAnimals = sum(EligiblePhase),
      RecordedCrossings = sum(RecordedCrossings), CompleteRosterPhase = all(EligiblePhase),
      IncludedInPrimary = all(IncludedInPrimary), .groups = "drop") %>%
    dplyr::mutate(CageMeanCrossingsPerHour = ifelse(CompleteRosterPhase, RecordedCrossings / (ExpectedAnimals * 12), NA_real_))
  baseline <- animals %>% dplyr::filter(PhaseLabel == reference_label) %>%
    dplyr::select(AnimalKey, BaselineRate = CrossingsPerHour, BaselineEligible = EligiblePhase)
  pairs <- animals %>% dplyr::filter(PhaseLabel != reference_label) %>% dplyr::left_join(baseline, by = "AnimalKey") %>%
    dplyr::mutate(PairedEligible = EligiblePhase & BaselineEligible,
      ChangeFromBaseline = ifelse(PairedEligible, CrossingsPerHour - BaselineRate, NA_real_))
  cage_base <- cages %>% dplyr::filter(PhaseLabel == reference_label) %>%
    dplyr::select(CageID, BaselineCageRate = CageMeanCrossingsPerHour)
  cage_pairs <- cages %>% dplyr::filter(PhaseLabel != reference_label) %>% dplyr::left_join(cage_base, by = "CageID") %>%
    dplyr::mutate(ChangeFromBaseline = CageMeanCrossingsPerHour - BaselineCageRate)
  # Full all-available table is retained as an audit; primary comparisons use
  # the same complete cages at I2/I3/I4/I5, never a changing denominator.
  population <- animals %>% dplyr::group_by(Sex, Batch, PhaseLabel) %>%
    dplyr::summarise(RecordedAnimals = n(), EligibleAnimals = sum(EligiblePhase), PrimaryAnimals = sum(IncludedInPrimary),
      RecordedCages = n_distinct(CageID), PrimaryCages = n_distinct(CageID[IncludedInPrimary]), .groups = "drop")
  list(animals = animals, cages = cages, animal_pairs = pairs, cage_pairs = cage_pairs, population = population)
}

# Conditional-on-batch descriptive uncertainty, with whole cage trajectories
# resampled together. No animal-level independent bootstrap and no p-values.
grid31_whole_inactive_uncertainty <- function(cages, draws = 2000L, seed = 20260929L) {
  if (length(draws) != 1L || !is.finite(draws) || draws < 100 || draws != floor(draws) ||
      length(seed) != 1L || !is.finite(seed)) stop("Invalid bootstrap draws/seed.", call. = FALSE)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  old_kind <- RNGkind()
  on.exit({ do.call(RNGkind, as.list(old_kind)); if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
            else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  summaries <- status <- draw_tables <- list()
  labels <- c("I2", "I3", "I4", "I5", "I3-I2", "I4-I2", "I5-I2", "mean_I3_I5-I2")
  for (sex in sort(unique(cages$Sex))) {
    all_sex <- cages %>% dplyr::filter(Sex == sex)
    cc <- all_sex %>% dplyr::filter(IncludedInPrimary) %>%
      dplyr::select(Batch, CageID, ExpectedAnimals, PhaseLabel, CageMeanCrossingsPerHour) %>%
      tidyr::pivot_wider(names_from = PhaseLabel, values_from = CageMeanCrossingsPerHour) %>% dplyr::arrange(Batch, CageID)
    batch_levels <- sort(unique(all_sex$Batch))
    counts <- table(factor(cc$Batch, levels = batch_levels))
    ok <- length(batch_levels) >= 2 && all(counts >= 2)
    status[[sex]] <- tibble::tibble(Sex = sex, Batches = length(batch_levels), Cages = nrow(cc),
      Animals = sum(cc$ExpectedAnimals), BootstrapStatus = if (ok) "PASS" else "NOT_ESTIMABLE_NEED_TWO_CAGES_PER_BATCH_AND_TWO_BATCHES",
      Draws = if (ok) as.integer(draws) else 0L, Seed = as.integer(seed))
    if (!nrow(cc) || any(counts == 0)) next
    if (!all(paste0("I", 2:5) %in% names(cc)) || anyNA(cc[paste0("I", 2:5)])) stop("Incomplete primary cage trajectory.", call. = FALSE)
    mat <- as.matrix(cc[paste0("I", 2:5)])
    mat <- cbind(mat, mat[, 2:4, drop = FALSE] - mat[, 1], rowMeans(mat[, 2:4, drop = FALSE]) - mat[, 1])
    colnames(mat) <- labels
    ix <- lapply(batch_levels, function(b) which(cc$Batch == b))
    point <- colMeans(do.call(rbind, lapply(ix, function(i) colMeans(mat[i, , drop = FALSE]))))
    boot <- if (ok) t(replicate(draws, {
      colMeans(do.call(rbind, lapply(ix, function(i) colMeans(mat[i[sample.int(length(i), length(i), replace = TRUE)], , drop = FALSE]))))
    })) else matrix(NA_real_, nrow = 0L, ncol = length(labels))
    lower <- upper <- rep(NA_real_, length(labels))
    if (ok) {
      lower <- apply(boot, 2, stats::quantile, probs = 0.025, names = FALSE)
      upper <- apply(boot, 2, stats::quantile, probs = 0.975, names = FALSE)
      draw_tables[[sex]] <- tibble::as_tibble(boot, .name_repair = ~ labels) %>% dplyr::mutate(Sex = sex, Draw = seq_len(draws), .before = 1)
    }
    summaries[[sex]] <- tibble::tibble(Sex = sex, Measure = labels,
      Role = c(rep("phase_mean",4), rep("main_phase_contrast",3), "secondary_three_day_average"),
      Estimate = as.numeric(point), Lower95 = lower, Upper95 = upper, Batches = length(batch_levels),
      Cages = nrow(cc), Animals = sum(cc$ExpectedAnimals),
      Estimand = "equal_batch_mean_of_equal_cage_means; position_changes_per_animal_hour",
      Interval = "pointwise_percentile_95; cages_resampled_within_fixed_batches; not_multiplicity_adjusted")
  }
  list(summary = dplyr::bind_rows(summaries), status = dplyr::bind_rows(status), draws = dplyr::bind_rows(draw_tables))
}
