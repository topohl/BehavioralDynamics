# ================================================================
# Hidden Markov Behavioral States
# MMMSociability
# ================================================================
# Goal:
#   Infer latent behavioral states and transitions using HMMs.
#
# Compared with k-means:
#   - explicitly models temporal persistence
#   - estimates transition probabilities
#   - estimates dwell times
#   - group-blind: Group/Sex are used only after state inference for summaries
#
# Input expectation:
#   Run Analysis/01_build_multiscale_behavior_metrics.R first.
#
# Recommended scale:
#   5–10 min bins. Phase-level data are too coarse for HMMs.
#
# Requires:
#   depmixS4
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"),
  file.path(dirname(tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) getwd())), "_pipeline_setup.R")
)
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("duration_normalization_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")

# ------------------------------------------------
# USER INPUT
# ------------------------------------------------

project_root <- getOption(
  "mmm.project_root",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
)
hmm_primary_bin_level <- getOption("mmm.hmm.primary_bin_level", "10min_based")
hmm_sensitivity_bin_levels <- getOption("mmm.hmm.sensitivity_bin_levels", "5min_based")
hmm_roster_bin_level <- getOption("mmm.hmm.roster_bin_level", "5min_based")
hmm_run_bin_levels <- unique(c(hmm_primary_bin_level, hmm_sensitivity_bin_levels))
hmm_fit_seeds <- as.integer(getOption("mmm.hmm.fit_seeds", c(1L, 11L, 101L)))
hmm_em_tolerance <- as.numeric(getOption("mmm.hmm.em_tolerance", 1e-6))
hmm_em_max_iterations <- as.integer(getOption("mmm.hmm.em_max_iterations", 500L))
hmm_emission_sd_floor <- as.numeric(getOption("mmm.hmm.emission_sd_floor", 0.05))
n_states <- as.integer(getOption("mmm.hmm.n_states", 4L))

# Use normalized proximity because raw contact seconds scale with bin size.
proximity_col_config <- "ProximityFraction"

if (!requireNamespace("depmixS4", quietly = TRUE)) {
  stop("Please install depmixS4 to run HMM behavioral states.")
}
depmix_loglik <- methods::getMethod("logLik", "depmix", where = asNamespace("depmixS4"))
fit <- depmixS4::fit

if (!methods::isClass("HMMRegularizedNORMresponse")) {
  methods::setClass("HMMRegularizedNORMresponse", contains = "NORMresponse")
}
methods::setMethod(
  "fit",
  signature(object = "HMMRegularizedNORMresponse"),
  function(object, w) {
    if (missing(w)) w <- NULL
    nas <- is.na(rowSums(object@y))
    pars <- object@parameters
    if (!is.null(w)) {
      fitted_response <- stats::lm.wfit(
        x = as.matrix(object@x[!nas, , drop = FALSE]),
        y = as.matrix(object@y[!nas, , drop = FALSE]),
        w = w[!nas]
      )
      fitted_sd <- sqrt(sum(w[!nas] * fitted_response$residuals^2 / sum(w[!nas])))
    } else {
      fitted_response <- stats::lm.fit(
        x = as.matrix(object@x[!nas, , drop = FALSE]),
        y = as.matrix(object@y[!nas, , drop = FALSE])
      )
      fitted_sd <- sqrt(sum(fitted_response$residuals^2) / length(fitted_response$residuals))
    }
    pars$coefficients <- fitted_response$coefficients
    pars$sd <- max(fitted_sd, hmm_emission_sd_floor)
    depmixS4::setpars(object, unlist(pars))
  }
)

initialize_hmm_from_kmeans <- function(mod, hmm_dat, n_states, seed, sd_floor = 0.05) {
  emission_matrix <- as.matrix(hmm_dat[, c("Movement_z", "Entropy_z", "Proximity_z")])
  set.seed(seed)
  km <- stats::kmeans(emission_matrix, centers = n_states, iter.max = 100, nstart = 1)
  for (state_index in seq_len(n_states)) {
    state_rows <- km$cluster == state_index
    for (response_index in seq_len(ncol(emission_matrix))) {
      mod@response[[state_index]][[response_index]] <- methods::new(
        "HMMRegularizedNORMresponse",
        mod@response[[state_index]][[response_index]]
      )
      response_values <- emission_matrix[state_rows, response_index]
      response_sd <- stats::sd(response_values)
      mod@response[[state_index]][[response_index]]@parameters$coefficients[] <-
        mean(response_values)
      mod@response[[state_index]][[response_index]]@parameters$sd <-
        max(response_sd, hmm_emission_sd_floor)
      mod@dens[, response_index, state_index] <- stats::dnorm(
        emission_matrix[, response_index],
        mean = mod@response[[state_index]][[response_index]]@parameters$coefficients,
        sd = mod@response[[state_index]][[response_index]]@parameters$sd
      )
    }
  }
  sequence_id <- as.character(hmm_dat$SequenceID)
  sequence_starts <- !duplicated(sequence_id)
  initial_counts <- tabulate(km$cluster[sequence_starts], nbins = n_states) + 1
  mod@init[1, ] <- initial_counts / sum(initial_counts)
  same_sequence_next <- sequence_id[-length(sequence_id)] == sequence_id[-1]
  for (from_state in seq_len(n_states)) {
    from_rows <- which(km$cluster[-length(km$cluster)] == from_state & same_sequence_next)
    transition_counts <- tabulate(km$cluster[from_rows + 1L], nbins = n_states) + 1
    mod@trDens[1, , from_state] <- transition_counts / sum(transition_counts)
  }
  mod
}

canonical_roster_file <- file.path(
  project_root,
  "analysis_ready/03_derived_metrics",
  hmm_roster_bin_level,
  "all_behavior_metrics.csv"
)
if (!file.exists(canonical_roster_file)) {
  stop("Canonical Stage 01 roster input is missing: ", canonical_roster_file, call. = FALSE)
}

canonical_roster_raw <- readr::read_csv(
  canonical_roster_file,
  col_types = readr::cols(
    .default = readr::col_skip(),
    AnimalNum = readr::col_character(),
    Group = readr::col_character(),
    Sex = readr::col_character()
  ),
  progress = FALSE
)
canonical_roster <- build_canonical_identity_roster(
  canonical_roster_raw,
  paste0("Stage 01 ", hmm_roster_bin_level, " roster")
)

git_sha <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[1], error = function(e) NA_character_)
hmm_script_path <- file.path(MMM_ANALYSIS_DIR, "08_hmm_behavioral_states_optional.R")

run_hmm_resolution <- function(bin_level) {
  input_file <- file.path(
    project_root,
    "analysis_ready/03_derived_metrics",
    bin_level,
    "all_behavior_metrics.csv"
  )
  output_dir <- file.path(
    project_root,
    "analysis_ready/06_behavioral_dynamics/hmm_states",
    bin_level
  )
  ensure_dir(output_dir)
  ensure_dir(file.path(output_dir, "tables"))
  ensure_dir(file.path(output_dir, "figures"))
  analysis_output_dirs(output_dir)

  write_output_manifest(
    output_dir,
    script_name = "08_hmm_behavioral_states_optional.R",
    analysis_name = paste("hidden Markov behavioral states", bin_level),
    primary_tables = c(
      "tables/hmm_state_assignments.csv",
      "tables/hmm_state_summary.csv",
      "tables/hmm_transition_probabilities.csv",
      "tables/hmm_state_dwell_times.csv",
      "tables/hmm_state_occupancy.csv",
      "tables/epoch_duration_qc.csv",
      "tables/hmm_fit_attempt_audit.csv",
      "tables/hmm_identity_summary.csv",
      "tables/hmm_sequence_quality_audit.csv",
      "tables/hmm_epoch_data_quality_exclusions.csv",
      "tables/hmm_input_provenance.csv"
    ),
    primary_figures = "figures/hmm_state_occupancy.svg",
    notes = c(
      paste0("Explicit HMM resolution role: ", ifelse(bin_level == hmm_primary_bin_level, "primary", "sensitivity"), "."),
      "Group and Sex inherit from the canonical current Stage 01 roster after fail-closed identity validation."
    )
  )

  if (!file.exists(input_file)) stop("Configured HMM input is missing: ", input_file, call. = FALSE)
  raw_dat <- readr::read_csv(
    input_file,
    col_types = readr::cols(AnimalNum = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE
  )

  identity <- audit_hmm_identity(raw_dat, canonical_roster, paste0("HMM input ", bin_level))
  write_table(canonical_roster, file.path(output_dir, "tables", "hmm_canonical_animal_roster.csv"))
  write_table(identity$alias_audit, file.path(output_dir, "tables", "hmm_identity_alias_audit.csv"))
  write_table(identity$identity_conflicts, file.path(output_dir, "tables", "hmm_identity_conflicts.csv"))
  write_table(identity$concordance, file.path(output_dir, "tables", "hmm_identity_concordance.csv"))
  write_table(identity$summary, file.path(output_dir, "tables", "hmm_identity_summary.csv"))
  assert_hmm_identity_audit(identity)

  proximity_col <- if (proximity_col_config %in% names(identity$data)) proximity_col_config else "Proximity"
  behav <- standardize_behavior_columns(identity$data, proximity_col = proximity_col)

  expected_sequences <- tidyr::crossing(
    canonical_roster,
    CageChange = paste0("CC", 1:4),
    Phase = c("Active", "Inactive")
  )
  observed_sequence_quality <- behav %>%
    mutate(
      AnimalNum = canonical_animal_id(.data$AnimalNum),
      complete_hmm_features = is.finite(.data$Movement) &
        is.finite(.data$Entropy) &
        is.finite(.data$Proximity)
    ) %>%
    group_by(AnimalNum, CageChange, Phase) %>%
    summarise(
      input_bins = n(),
      complete_hmm_bins = sum(complete_hmm_features),
      .groups = "drop"
    )
  sequence_quality_audit <- expected_sequences %>%
    left_join(observed_sequence_quality, by = c("AnimalNum", "CageChange", "Phase")) %>%
    mutate(
      BinLevel = bin_level,
      input_bins = coalesce(input_bins, 0L),
      complete_hmm_bins = coalesce(complete_hmm_bins, 0L),
      retained_for_hmm = complete_hmm_bins >= 4L,
      exclusion_reason = case_when(
        input_bins == 0L ~ "no Stage 01 input bins for canonical animal/epoch",
        complete_hmm_bins < 4L ~ "fewer than 4 complete Movement/Entropy/Proximity bins",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(BinLevel)
  write_table(sequence_quality_audit, file.path(output_dir, "tables", "hmm_sequence_quality_audit.csv"))
  write_table(
    sequence_quality_audit %>% filter(!retained_for_hmm),
    file.path(output_dir, "tables", "hmm_epoch_data_quality_exclusions.csv")
  )

  hmm_dat <- behav %>%
    mutate(
      AnimalNum = canonical_animal_id(AnimalNum),
      Movement_z = z_within_metric(Movement),
      Entropy_z = z_within_metric(Entropy),
      Proximity_z = z_within_metric(Proximity),
      BinLevel = bin_level,
      ProximityInput = proximity_col
    ) %>%
    filter(is.finite(Movement_z), is.finite(Entropy_z), is.finite(Proximity_z)) %>%
    arrange(AnimalNum, CageChange, Phase, TimeIndex) %>%
    mutate(SequenceID = interaction(AnimalNum, CageChange, Phase, drop = TRUE, sep = "__"))

  sequence_tbl <- hmm_dat %>%
    count(SequenceID, AnimalNum, Group, Sex, CageChange, Phase, name = "n_bins") %>%
    arrange(AnimalNum, CageChange, Phase) %>%
    filter(n_bins >= 4)

  hmm_dat <- hmm_dat %>%
    semi_join(sequence_tbl %>% select(SequenceID), by = "SequenceID") %>%
    mutate(SequenceID = factor(SequenceID, levels = sequence_tbl$SequenceID)) %>%
    arrange(SequenceID, TimeIndex)

  data_quality_exclusions <- canonical_roster %>%
    anti_join(sequence_tbl %>% distinct(AnimalNum), by = "AnimalNum") %>%
    mutate(
      BinLevel = bin_level,
      exclusion_reason = "no sequence with at least 4 complete Movement/Entropy/Proximity bins"
    ) %>%
    relocate(BinLevel)
  write_table(data_quality_exclusions, file.path(output_dir, "tables", "hmm_data_quality_exclusions.csv"))

  epoch_duration_qc <- write_epoch_duration_qc(
    hmm_dat,
    output_dir,
    metric_source = "08_hmm_behavioral_states_optional",
    bin_size_sec = infer_bin_size_sec(hmm_dat)
  )
  ntimes <- sequence_tbl$n_bins
  if (nrow(hmm_dat) < n_states * 10) {
    stop("Too few usable rows for HMM. Use a finer bin level or fewer states.", call. = FALSE)
  }

  mod <- depmixS4::depmix(
    list(Movement_z ~ 1, Entropy_z ~ 1, Proximity_z ~ 1),
    data = hmm_dat,
    ntimes = ntimes,
    nstates = n_states,
    family = list(gaussian(), gaussian(), gaussian())
  )

  fit_attempts <- map(hmm_fit_seeds, function(fit_seed) {
    initialized_mod <- initialize_hmm_from_kmeans(mod, hmm_dat, n_states, fit_seed)
    attempt_warnings <- character()
    fitted <- tryCatch(
      withCallingHandlers(
        depmixS4::fit(
          initialized_mod,
          verbose = FALSE,
          emcontrol = depmixS4::em.control(
            maxit = hmm_em_max_iterations,
            tol = hmm_em_tolerance,
            random.start = FALSE
          )
        ),
        warning = function(w) {
          attempt_warnings <<- c(attempt_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    message_text <- if (inherits(fitted, "error")) conditionMessage(fitted) else paste(fitted@message, collapse = " | ")
    list(
      fit = fitted,
      audit = tibble(
        BinLevel = bin_level,
        fit_seed = fit_seed,
        initialization = "kmeans emission means/SDs plus empirical sequence-aware transition/prior start",
        em_tolerance = hmm_em_tolerance,
        em_max_iterations = hmm_em_max_iterations,
        emission_sd_floor_z = hmm_emission_sd_floor,
        converged = !inherits(fitted, "error") && str_detect(str_to_lower(message_text), "converged"),
        logLik = if (inherits(fitted, "error")) NA_real_ else as.numeric(depmix_loglik(fitted)),
        fitter_message = message_text,
        fit_warning = ifelse(length(attempt_warnings) == 0, NA_character_, paste(unique(attempt_warnings), collapse = " | "))
      )
    )
  })
  fit_attempt_audit <- map_dfr(fit_attempts, "audit")
  write_table(fit_attempt_audit, file.path(output_dir, "tables", "hmm_fit_attempt_audit.csv"))
  converged_attempts <- which(fit_attempt_audit$converged & is.finite(fit_attempt_audit$logLik))
  if (length(converged_attempts) == 0L) {
    stop(
      "No deterministic HMM start converged for ", bin_level,
      ". See tables/hmm_fit_attempt_audit.csv; unconverged fits are not promoted.",
      call. = FALSE
    )
  }
  selected_attempt <- converged_attempts[which.max(fit_attempt_audit$logLik[converged_attempts])]
  fit_mod <- fit_attempts[[selected_attempt]]$fit
  fit_warnings <- fit_attempt_audit$fit_warning[selected_attempt]
  post <- depmixS4::posterior(fit_mod, type = "viterbi")
  hmm_dat$State <- factor(post$state)
  fit_loglik <- as.numeric(depmix_loglik(fit_mod))
  occupied_states <- sort(unique(as.integer(post$state)))
  if (length(occupied_states) != n_states) {
    stop(
      "Selected HMM fit for ", bin_level, " occupies ", length(occupied_states),
      " of ", n_states, " states (missing: ",
      paste(setdiff(seq_len(n_states), occupied_states), collapse = ", "),
      "). The fit is not promoted.",
      call. = FALSE
    )
  }

  hmm_qc <- tibble(
    BinLevel = bin_level,
    ResolutionRole = ifelse(bin_level == hmm_primary_bin_level, "primary", "sensitivity"),
    ProximityInput = proximity_col,
    GroupBlind = TRUE,
    n_states = n_states,
    n_occupied_states = length(occupied_states),
    unoccupied_states = paste(setdiff(seq_len(n_states), occupied_states), collapse = "|"),
    n_rows = nrow(hmm_dat),
    n_animals = n_distinct(hmm_dat$AnimalNum),
    n_expected_animals = n_distinct(canonical_roster$AnimalNum),
    n_data_quality_exclusions = nrow(data_quality_exclusions),
    n_sequences = length(ntimes),
    min_sequence_bins = min(ntimes),
    median_sequence_bins = median(ntimes),
    max_sequence_bins = max(ntimes),
    logLik = fit_loglik,
    selected_fit_seed = fit_attempt_audit$fit_seed[selected_attempt],
    fitter_message = fit_attempt_audit$fitter_message[selected_attempt],
    fit_warning = fit_warnings
  )

  write_table(sequence_tbl, file.path(output_dir, "tables", "hmm_sequence_design.csv"))
  write_table(hmm_qc, file.path(output_dir, "tables", "hmm_model_qc.csv"))
  write_table(
    hmm_dat %>%
      select(BinLevel, ProximityInput, SequenceID, AnimalNum, Group, Sex, Phase, CageChange, TimeIndex, Movement_z, Entropy_z, Proximity_z, State),
    file.path(output_dir, "tables", "hmm_state_assignments.csv")
  )

  state_summary <- hmm_dat %>%
    group_by(BinLevel, ProximityInput, State) %>%
    summarise(
      Movement_z = mean(Movement_z, na.rm = TRUE),
      Entropy_z = mean(Entropy_z, na.rm = TRUE),
      Proximity_z = mean(Proximity_z, na.rm = TRUE),
      n_bins = n(),
      .groups = "drop"
    )
  write_table(state_summary, file.path(output_dir, "tables", "hmm_state_summary.csv"))

  transition_tbl <- hmm_dat %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum) %>%
    arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(NextState = lead(State)) %>%
    filter(!is.na(NextState)) %>%
    count(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum, State, NextState, name = "Transitions") %>%
    join_duration_qc(epoch_duration_qc) %>%
    normalize_counts_to_rates("Transitions")
  write_table(transition_tbl, file.path(output_dir, "tables", "hmm_transition_counts.csv"))

  transition_prob_tbl <- transition_tbl %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum, State) %>%
    mutate(TransitionProbability = Transitions / sum(Transitions)) %>%
    ungroup()
  write_table(transition_prob_tbl, file.path(output_dir, "tables", "hmm_transition_probabilities.csv"))

  dwell_tbl <- hmm_dat %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum) %>%
    arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(StateRun = cumsum(State != lag(State, default = first(State))) + 1L) %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum, State, StateRun) %>%
    summarise(dwell_bins = n(), .groups = "drop") %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum, State) %>%
    summarise(
      mean_dwell_bins = mean(dwell_bins, na.rm = TRUE),
      median_dwell_bins = median(dwell_bins, na.rm = TRUE),
      max_dwell_bins = max(dwell_bins, na.rm = TRUE),
      n_bouts = n(),
      .groups = "drop"
    ) %>%
    mutate(
      bin_size_sec = infer_bin_size_sec(hmm_dat),
      mean_dwell_hours = mean_dwell_bins * bin_size_sec / 3600,
      median_dwell_hours = median_dwell_bins * bin_size_sec / 3600,
      max_dwell_hours = max_dwell_bins * bin_size_sec / 3600
    ) %>%
    join_duration_qc(epoch_duration_qc)
  write_table(dwell_tbl, file.path(output_dir, "tables", "hmm_state_dwell_times.csv"))

  occupancy_tbl <- hmm_dat %>%
    count(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum, State) %>%
    group_by(BinLevel, ProximityInput, Group, Sex, Phase, CageChange, AnimalNum) %>%
    mutate(frac_time = n / sum(n)) %>%
    ungroup() %>%
    join_duration_qc(epoch_duration_qc)
  write_table(occupancy_tbl, file.path(output_dir, "tables", "hmm_state_occupancy.csv"))

  provenance <- tibble(
    artifact_type = c("HMM input", "canonical roster", "HMM code"),
    path = c(input_file, canonical_roster_file, hmm_script_path),
    resolution = c(bin_level, hmm_roster_bin_level, bin_level),
    resolution_role = c(
      ifelse(bin_level == hmm_primary_bin_level, "primary", "sensitivity"),
      "identity_reference",
      ifelse(bin_level == hmm_primary_bin_level, "primary", "sensitivity")
    ),
    git_sha = git_sha,
    code_md5 = c(NA_character_, NA_character_, unname(tools::md5sum(hmm_script_path))),
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    model_specification = paste0(
      n_states, "-state regularized Gaussian HMM: Movement_z + Entropy_z + Proximity_z; sequences = AnimalNum x CageChange x Phase; ",
      "deterministic starts=", paste(hmm_fit_seeds, collapse = "|"),
      "; selected highest converged logLik; EM tolerance=", hmm_em_tolerance,
      "; max iterations=", hmm_em_max_iterations,
      "; Gaussian emission SD floor (z units)=", hmm_emission_sd_floor
    )
  )
  write_table(provenance, file.path(output_dir, "tables", "hmm_input_provenance.csv"))

  p_occ <- occupancy_tbl %>%
    ggplot(aes(State, frac_time, fill = Group)) +
    geom_violin(alpha = 0.5, linewidth = 0.2, trim = FALSE) +
    geom_jitter(width = 0.1, size = 0.8, alpha = 0.7) +
    facet_grid(Phase ~ State, scales = "free_y") +
    labs(
      title = "Hidden Markov behavioral state occupancy",
      subtitle = paste0("Bin level: ", bin_level, "; proximity input: ", proximity_col),
      y = "Fraction of time",
      x = NULL
    ) +
    make_nature_theme()
  save_plot_svg_pdf(p_occ, file.path(output_dir, "figures", "hmm_state_occupancy"), width = 180, height = 120)

  if (exists("harmonize_analysis_outputs")) harmonize_analysis_outputs(output_dir)
  message("HMM behavioral-state analysis complete: ", bin_level)
  invisible(hmm_qc)
}

hmm_run_qc <- map_dfr(hmm_run_bin_levels, run_hmm_resolution)
message(
  "Completed explicit HMM resolution contract: primary=", hmm_primary_bin_level,
  "; sensitivity=", paste(hmm_sensitivity_bin_levels, collapse = ", "),
  "; fitted animals=", paste(paste0(hmm_run_qc$BinLevel, ":", hmm_run_qc$n_animals), collapse = "; ")
)
