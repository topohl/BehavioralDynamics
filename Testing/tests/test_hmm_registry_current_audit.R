# The manuscript-source registry must distinguish the current-input HMM rerun
# from the retained September audit and preserve nonpublication boundaries.
source("Functions/project_paths.R")
registry <- utils::read.csv("docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv",
                            check.names = FALSE, na.strings = "NA")
ids <- c("HMM_ACTIVE_LONGITUDINAL_PERSISTENCE", "HMM_INACTIVE_PHENOTYPE",
         "HMM_OCCUPANCY_ENTROPY")
rows <- registry[match(ids, registry$analysis_id), , drop = FALSE]
stopifnot(nrow(rows) == 3L, !anyNA(rows$analysis_id),
          all(grepl("analysis_ready/analyses/hmm_revalidation_runs/current_stage08_review_20260924/",
                    rows$source_artifact, fixed = TRUE)),
          !any(grepl("12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/",
                     rows$source_artifact, fixed = TRUE)),
          grepl("five seeded current-input fits", rows$multiplicity_family[[1L]],
                fixed = TRUE),
          grepl("three printed likelihood levels", rows$multiplicity_family[[1L]],
                fixed = TRUE),
          grepl("not been incorporated into manuscript or release products",
                rows$publication_ready[[1L]], fixed = TRUE),
          identical(rows$publication_ready[[2L]], "no"),
          identical(rows$publication_ready[[3L]], "no"))

verdict_path <- file.path(mmm_project_root(), "analysis_ready", "analyses",
                          "hmm_revalidation_runs", "current_stage08_review_20260924",
                          "hmm_cross_optimum_gapaware_claim_verdicts.csv")
if (file.exists(verdict_path)) {
  verdict <- utils::read.csv(verdict_path)
  stopifnot(nrow(verdict) == 20L, all(verdict$n_optima == 5L),
            all(verdict$all_same_sign))
  active <- verdict[verdict$PhaseClass == "Active" &
                      verdict$contrast == "SUS-CON", , drop = FALSE]
  expected <- list(mean_dwell_minutes = c(0.519, 0.741, 4),
                   self_transition_probability = c(0.458, 0.642, 4),
                   state_switch_rate = c(-0.642, -0.458, 4),
                   transition_entropy = c(-0.694, -0.472, 4))
  for (domain in names(expected)) {
    actual <- active[active$Domain == domain, , drop = FALSE]
    stopifnot(nrow(actual) == 1L,
              isTRUE(all.equal(unname(as.numeric(unlist(actual[1L,
                c("est_min", "est_max", "n_nominally_sig")]))),
                expected[[domain]])))
  }
  occupancy <- verdict[verdict$PhaseClass == "Active" &
                         verdict$Domain == "occupancy_entropy" &
                         verdict$contrast == "SUS-RES", , drop = FALSE]
  stopifnot(nrow(occupancy) == 1L, occupancy$all_same_sign,
            occupancy$n_nominally_sig == 0L,
            grepl("0 of 5 nominally significant", rows$effect_size[[3L]],
                  fixed = TRUE))
  cat("HMM registry source and current-input numeric boundary: PASS\n")
} else {
  cat("HMM registry source boundary: PASS; live current-input audit absent\n")
}
