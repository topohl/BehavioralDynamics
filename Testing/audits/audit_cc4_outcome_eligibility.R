# CC4 outcome x cage-episode eligibility (rule cc4e_v1.0): UNREGISTERED, isolated, group-blind; no model is fitted and no
# stage is rerun. Applies the released spatial exclusion manifest v1.1 to the Stage 31-31d outcomes per animal x cage
# episode x phase family x phase (Functions/cc4_outcome_eligibility.R), and reports what the rule changes: the status of
# every cell, the affected outputs and model contexts, and the exact sample sizes of the frozen rosters, the rule-applied
# primary rosters and the inactive sensitivity. The rosters are computed by the functions the stages use
# (grid31_group_roster/grid31_group_summaries of 31b, grid31_stats_data of 31c/31d, 31d's phenotype gate) on the frozen
# 31b table, so they are the ones a rerun would use; frozen_parity.csv shows that the same functions without the rule
# reproduce the frozen tables. Documentation: docs/CC4_OUTCOME_ELIGIBILITY.md.
#
# Run from the repository root (one light R process; most of the time is the protection snapshot):
#   Rscript Testing/audits/audit_cc4_outcome_eligibility.R [--phase-groups=<31b run>] [--phase-statistics=<31c run>]
#           [--phase-phenotypes=<31d run>] [--knockout-run=<cc4 knockout run>] [--run-id=<new id>] [--output-root=<absolute>]
# Defaults: the frozen runs phase_groups_20260929_v2, phase_statistics_20260929_v1 and phase_phenotypes_20260929_v1
# under <project>/analysis_ready/analyses/cc4_grid_exposure; output root <Behavior>/RANM/cc4_outcome_eligibility, refused
# where mmm_isolated_output_root_check() or the protection guard refuse it. Writes runs/<run_id>/ only (built in
# runs/.tmp_<run_id>/). The default run id is cc4e_v1.0_<commit>; it needs committed code. Ids starting with "dev" may run
# on uncommitted code.
# Reads, read-only and verified against their manifests: the three runs (31c and 31d must have read this 31b table), the
# Stage 31 run the 31b run read (output manifest, input manifest, whole-inactive population), the cage-label correction
# registry it recorded, tables/animals.csv of the srs_v1.1 release, the released manifest v1.1 and, with --knockout-run,
# tables/materiality_cc4.csv of a CC4 knockout run. Group labels come from the frozen 31b table and are used for counting
# only; the rule never reads them.
# Outputs: tables/rules_used.csv, outcome_episode_eligibility.csv (the file Stages 31b-31d read through
# MMM_CC4_OUTCOME_ELIGIBILITY), affected_analyses.csv, sample_sizes.csv, cross_cc_episodes.csv, missing_vs_excluded.csv,
# frozen_parity.csv, roster_checks.csv, knockout_used.csv (with --knockout-run); README.txt; manifest/ (code_hashes,
# input_hashes, run.csv, session_info.txt, spatial_exclusions_used.csv, the protection guard's two files and
# output_files_sha256.csv).

cc4ea_args <- function(args) {
  known <- "^--(phase-groups|phase-statistics|phase-phenotypes|knockout-run|run-id|output-root|variant)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  variant <- val("variant")
  if (is.na(variant)) variant <- "primary"
  if (!variant %in% CC4E_VARIANTS) stop("--variant must be one of: ", paste(CC4E_VARIANTS, collapse = ", "), call. = FALSE)
  list(phase_groups = val("phase-groups"), phase_statistics = val("phase-statistics"), phase_phenotypes = val("phase-phenotypes"),
       knockout_run = val("knockout-run"), run_id = val("run-id"), output_root = val("output-root"), variant = variant)
}

CC4EA_CODE <- c("Testing/audits/audit_cc4_outcome_eligibility.R", "Functions/cc4_outcome_eligibility.R", "Functions/spatial_exclusion_manifest.R",
                "Functions/cc4_phase_group_helpers.R", "Functions/cc4_phase_statistics_helpers.R", "Functions/cc4_whole_inactive_helpers.R",
                "Functions/cc4_grid_exposure_helpers.R", "Analysis/31b_cc4_phase_groups.R", "Analysis/31c_cc4_phase_statistics.R",
                "Analysis/31d_cc4_phenotype_statistics.R", "Functions/project_paths.R", "Functions/protected_outputs.R",
                "Functions/qc_run_protection.R", "Functions/frozen_input_guard.R", "Functions/behavioral_dynamics_helpers.R",
                "Functions/behavior_analysis_config.R", "Analysis/_pipeline_setup.R")
CC4EA_FROZEN <- c(phase_groups = "phase_groups_20260929_v2", phase_statistics = "phase_statistics_20260929_v1",
                  phase_phenotypes = "phase_phenotypes_20260929_v1")
CC4EA_SRS_RUN <- "srs_v1.1_9b18624"
CC4EA_LONGEST_RELATIVE <- c(file.path("manifest", "protection_snapshot_before.csv"), file.path("tables", "outcome_episode_eligibility.csv"),
                            file.path("manifest", "output_files_sha256.csv"))
CC4EA_ROSTERS <- c("frozen", "primary", "sensitivity")

# Outputs and model contexts of Stages 31-31d that read the crossing counts of a phase family, and what they depend on.
CC4EA_OUTPUTS <- data.frame(
  Stage = c("31", "31", "31", "31", "31", "31", "31b", "31b", "31c", "31c", "31d", "31d"),
  output = c("tables/animal_clock_profiles_5min.csv and cage_clock_profiles_5min.csv, active blocks",
             "figures/<batch>_active_clock_profiles.png",
             "tables/*_clock_profiles_5min.csv inactive blocks; figures/<batch>_inactive_clock_profiles.png",
             "audit/all_two_hour_candidate_scores.csv; tables/activity_inferred_candidates_UNVERIFIED.csv",
             "whole-inactive tables (animal, cage, changes from I2, sex summary), bootstrap and population",
             "audit/phase_animal_coverage.csv (bracketing, ReadCount)",
             "Active family: animal_phase_activity, cage/batch/group summaries, group contrasts",
             "Inactive family: animal_phase_activity, cage/batch/group summaries, group contrasts",
             "<sex>_Active: primary NB2 ratio of rate ratios, model rates, secondary (withheld), leave-one-batch-out, dispersion, mixed cages, predictive checks",
             "<sex>_Inactive: primary NB2 ratio of rate ratios, model rates, secondary (withheld), leave-one-batch-out, dispersion, mixed cages, predictive checks",
             "<sex>_Active: omnibus, RES-CON, SUS-CON, SUS-RES, intervals, leave-one-batch-out, dispersion, mixed cages, predictive checks",
             "<sex>_Inactive: omnibus, RES-CON, SUS-CON, SUS-RES, intervals, leave-one-batch-out, dispersion, mixed cages, predictive checks"),
  family = c("Active", "Active", "Inactive", "Inactive", "Inactive", "QC", "Active", "Inactive", "Active", "Inactive", "Active", "Inactive"),
  phases = c("A1-A5", "A1-A5", "I2-I5", "I2-I5", "I2-I5", "all", "A1-A5", "I2-I5", "A2-A5", "I2-I5", "A2-A5", "I2-I5"),
  roster_stage = c(NA, NA, NA, NA, "31", NA, "31b", "31b", "31c", "31c", "31d", "31d"),
  dependency = c(
    "vendor crossings per 5-min bin: with a silent reader the vendor cannot report it, so passes through it become neighbour transitions or flicker (active knockout material)",
    "descriptive cage clock profile of the active crossing rate",
    "inactive crossings: the light-window knockout is not material (0.12/0.35 SD) but was not measured on the grid days I3-I5, whose rates are 2-4x higher",
    "same-clock difference from I2: a constant offset cancels, an activity-dependent bias does not",
    "whole-phase inactive crossing rate and its change from I2",
    "bracketing needs a read by any reader; a dead reader is invisible to it (QC)",
    "active crossing rate and its change from A2 (A1 earlier context)",
    "inactive crossing rate and its change from I2",
    "summed active crossings, A2 vs pooled A3-A5 (A1 not used)",
    "summed inactive crossings, I2 vs pooled I3-I5",
    "animal active crossings, A2 vs pooled A3-A5 (A1 not used)",
    "animal inactive crossings, I2 vs pooled I3-I5"),
  stringsAsFactors = FALSE)

cc4ea_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, "--no-optional-locks", args), stdout = TRUE, stderr = TRUE))
cc4ea_sha <- function(f) digest::digest(file = f, algo = "sha256")

# The 31b animal table as Stages 31c and 31d read it.
cc4ea_read_phases <- function(path) {
  a <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_guess(), AnimalNum = readr::col_character(),
                                                      AnimalID = readr::col_character()), show_col_types = FALSE)
  if (nrow(readr::problems(a))) stop("Phase parse failure: ", path, call. = FALSE)
  a
}

# The joined animal-phase table before the 31b roster (the frozen table without its roster and change columns, the
# family flag of grid31_whole_phase back under its name IncludedInPrimary), the input of grid31_group_summaries().
cc4ea_pre_roster <- function(a) {
  if (!all(c("IncludedInFamily", "CommonCage", "IncludedInPrimary", "BaselineRate", "ChangeFromBaseline") %in% names(a)))
    stop("Not a Stage 31b animal_phase_activity table.", call. = FALSE)
  pre <- a[, setdiff(names(a), c("CommonCage", "IncludedInPrimary", "BaselineRate", "ChangeFromBaseline"))]
  names(pre)[names(pre) == "IncludedInFamily"] <- "IncludedInPrimary"
  pre
}

cc4ea_try <- function(expr) tryCatch(list(value = force(expr), status = "pass"), error = function(e) list(value = NULL, status = conditionMessage(e)))

# Stage 31d's gate after grid31_stats_data() (Analysis/31d_cc4_phenotype_statistics.R): every Sex x Phase x Batch of the
# model input holds CON, RES and SUS animals.
cc4ea_31d_check <- function(animals) {
  expected <- tidyr::crossing(animals %>% dplyr::distinct(Sex, Phase, Batch), Group = c("CON", "RES", "SUS"))
  if (nrow(dplyr::anti_join(expected, animals, by = c("Sex", "Phase", "Batch", "Group")))) stop("Missing phenotype within batch.", call. = FALSE)
  invisible(TRUE)
}
# Stage 31d's population table (the same expression).
cc4ea_31d_population <- function(animals) animals %>% dplyr::group_by(Sex, Phase, Group) %>%
  dplyr::summarise(Animals = dplyr::n_distinct(AnimalKey), Cages = dplyr::n_distinct(CageID), Batches = dplyr::n_distinct(Batch), .groups = "drop")
# Stage 31's whole-inactive population (grid31_whole_phase's expression) from the inactive rows of the pre-roster table.
cc4ea_stage31_population <- function(x) x %>% dplyr::group_by(Sex, Batch, PhaseLabel) %>%
  dplyr::summarise(RecordedAnimals = dplyr::n(), EligibleAnimals = sum(EligiblePhase), PrimaryAnimals = sum(IncludedInPrimary),
    RecordedCages = dplyr::n_distinct(CageID), PrimaryCages = dplyr::n_distinct(CageID[IncludedInPrimary]), .groups = "drop")

#' Rosters of Stages 31 (whole-inactive cohort), 31b, 31c and 31d on a frozen 31b table `a`: frozen (no rule), primary
#' (eligibility `elig`) and sensitivity (`sens`, cc4e_inactive_sensitivity(elig)). Returns included (one row per Stage x
#' roster x family x included animal), checks (whether a rerun would pass the stage's gates) and the stage results.
cc4ea_rosters <- function(a, elig, sens) {
  pre <- cc4ea_pre_roster(a)
  variants <- list(frozen = NULL, primary = elig, sensitivity = sens)
  inc <- list(); chk <- list(); runs <- list()
  add <- function(stage, roster, rows, status) {
    rows <- as.data.frame(rows)
    inc[[length(inc) + 1L]] <<- unique(data.table::data.table(Stage = rep(stage, nrow(rows)), roster = roster, Sex = rows$Sex,
      family = rows$Phase, Group = rows$Group, AnimalKey = rows$AnimalKey, CageID = rows$CageID, Batch = rows$Batch))
    chk[[length(chk) + 1L]] <<- data.table::data.table(Stage = stage, roster = roster, check = status)
  }
  for (v in names(variants)) {
    e <- variants[[v]]
    # Stage 31: the whole-inactive primary cohort (grid31_whole_phase's family flag); with the rule, a cage with an
    # excluded or censored inactive cell leaves it
    x <- as.data.frame(pre[pre$Phase == "Inactive", ])
    ok <- x$IncludedInPrimary
    if (!is.null(e)) {
      j <- cc4e_join(x, e)
      out <- tapply(j$FamilyExcluded, j$CageID, any)
      ok <- ok & !as.vector(out[j$CageID])
    }
    add("31", v, x[ok, ], "pass")
    # Stage 31b: the roster is computed before its batch-completeness gate, which a rerun applies
    r <- grid31_group_roster(pre, e)
    b <- cc4ea_try(grid31_group_summaries(pre, e))
    add("31b", v, r[r$IncludedInPrimary, ], b$status)
    # Stages 31c and 31d: grid31_stats_data() (31d adds its phenotype gate)
    s <- cc4ea_try(grid31_stats_data(a, e))
    if (is.null(s$value)) {
      chk[[length(chk) + 1L]] <- data.table::data.table(Stage = c("31c", "31d"), roster = v, check = s$status)
    } else {
      ref <- s$value$animals[s$value$animals$Period == "Reference", ]
      add("31c", v, ref, s$status)
      add("31d", v, ref, cc4ea_try(cc4ea_31d_check(s$value$animals))$status)
    }
    runs[[v]] <- list(b = b$value, c = s$value, s31 = if (is.null(e)) cc4ea_stage31_population(x) else NULL)
  }
  list(included = data.table::rbindlist(inc), checks = data.table::rbindlist(chk), runs = runs)
}

# Same column names and values (numbers equal to 1e-12 relative), read as text.
cc4ea_same_values <- function(f0, f1) {
  rd <- function(f) utils::read.csv(f, colClasses = "character", na.strings = c("", "NA"), check.names = FALSE)
  x0 <- rd(f0); x1 <- rd(f1)
  if (!identical(names(x0), names(x1)) || nrow(x0) != nrow(x1)) return(FALSE)
  all(vapply(names(x0), function(k) {
    p <- x0[[k]]; q <- x1[[k]]
    if (identical(p, q)) return(TRUE)
    np <- suppressWarnings(as.numeric(p)); nq <- suppressWarnings(as.numeric(q))
    identical(is.na(p), is.na(q)) && !anyNA(np[!is.na(p)]) && !anyNA(nq[!is.na(q)]) && isTRUE(all.equal(np, nq, tolerance = 1e-12))
  }, logical(1)))
}

#' The frozen tables against the same tables recomputed without the rule from the frozen 31b table (written as the stage
#' writes them): bytes and values.
cc4ea_parity <- function(runs, pg, ps, pp, s31) {
  tmp <- tempfile("cc4ea_parity_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  b <- runs$frozen$b; cc <- runs$frozen$c
  if (is.null(b) || is.null(cc)) stop("The legacy 31b or 31c computation failed on the frozen table.", call. = FALSE)
  items <- list(list("31b", pg, "animal_phase_activity.csv", b$animals, ""), list("31b", pg, "cage_group_phase_activity.csv", b$cages, ""),
                list("31b", pg, "batch_group_phase_activity.csv", b$batch, ""), list("31b", pg, "population.csv", b$population, ""),
                list("31b", pg, "batch_group_contrasts.csv", b$batch_contrasts, ""), list("31b", pg, "group_phase_summary.csv", b$summary, ""),
                list("31b", pg, "group_phase_contrasts.csv", b$contrasts, ""),
                list("31c", ps, "roster_model_input.csv", cc$roster, ""), list("31c", ps, "animals_model_input.csv", cc$animals, ""),
                list("31c", ps, "cages_model_input.csv", cc$cages, ""),
                list("31d", pp, "animal_period_input.csv", cc$animals, "NA"), list("31d", pp, "cage_roster.csv", cc$roster, "NA"),
                list("31d", pp, "population.csv", cc4ea_31d_population(cc$animals), "NA"),
                list("31", s31, "audit/whole_inactive_population.csv", runs$frozen$s31, ""))
  data.table::rbindlist(lapply(seq_along(items), function(i) {
    it <- items[[i]]
    f0 <- file.path(it[[2]], it[[3]]); f1 <- file.path(tmp, sprintf("%02d_%s", i, basename(it[[3]])))
    readr::write_csv(it[[4]], f1, na = it[[5]])
    data.table::data.table(Stage = it[[1]], run = basename(it[[2]]), file = it[[3]], frozen_sha256 = cc4ea_sha(f0), recomputed_sha256 = cc4ea_sha(f1),
                           bytes_identical = cc4ea_sha(f0) == cc4ea_sha(f1), values_identical = cc4ea_same_values(f0, f1))
  }))
}

#' Sample sizes per Stage x Sex x family x Group (CON, RES, SUS, all) for the frozen, primary and sensitivity rosters,
#' with the primary-minus-frozen differences and whether a rerun would pass each stage's gates.
cc4ea_sample_sizes <- function(ro) {
  pop <- data.table::rbindlist(lapply(split(ro$included, by = c("Stage", "roster")), function(x)
    cbind(data.table::data.table(Stage = x$Stage[1], roster = x$roster[1]), cc4e_population(x))))
  meas <- c("Animals", "Cages", "CONCages", "Batches", "MixedCages")
  w <- data.table::dcast(pop, Stage + Sex + family + Group ~ roster, value.var = meas, fill = 0L)
  for (m in meas) for (r in CC4EA_ROSTERS) if (!paste0(m, "_", r) %in% names(w)) w[, (paste0(m, "_", r)) := 0L]
  w[, `:=`(Animals_primary_minus_frozen = Animals_primary - Animals_frozen, Cages_primary_minus_frozen = Cages_primary - Cages_frozen)]
  ck <- data.table::dcast(ro$checks, Stage ~ roster, value.var = "check")
  data.table::setnames(ck, setdiff(names(ck), "Stage"), paste0("check_", setdiff(names(ck), "Stage")))
  w <- merge(w, ck, by = "Stage", all.x = TRUE)
  data.table::setcolorder(w, c("Stage", "Sex", "family", "Group", as.vector(t(outer(meas, CC4EA_ROSTERS, paste, sep = "_")))))
  w[order(Stage, Sex, family, match(Group, c("CON", "RES", "SUS", "all")))]
}

# "status: phases" of a set of cells, e.g. "excluded_hardware: A1,A2; censored: A3,A4,A5".
cc4ea_status_summary <- function(cells) {
  if (!nrow(cells)) return("")
  s <- unique(cells[, .(status, PhaseLabel)])[order(match(status, CC4E_STATUSES), PhaseLabel)]
  s <- s[, .(p = paste(sort(unique(PhaseLabel)), collapse = ",")), by = status]
  paste(paste0(s$status, ": ", s$p), collapse = "; ")
}
cc4ea_consequence <- function(st) {
  if (!length(st)) return("unaffected")
  if (all(st == "included")) return("unaffected: included")
  if (all(st %in% CC4E_RETAINED) && "retained_material" %in% st) {
    return(paste("retained in the primary analysis, flagged (manifest v1.1 keeps the light-phase rate); CC4 knockout MATERIAL: decision for Tobias,",
                 "an exclusion needs an approved new rule version; left out of the inactive sensitivity"))
  }
  if (all(st %in% CC4E_RETAINED)) return("retained in the primary analysis, flagged; left out of the inactive sensitivity")
  if (all(st == "excluded_hardware")) return("excluded from the primary analysis; kept in the all-episode sensitivity (the frozen run)")
  if ("censored" %in% st && "excluded_hardware" %in% st) return("excluded (hardware) and censored (missing); outside the roster")
  if (all(st %in% c("censored", "incomplete_span", "cage_roster_incomplete"))) return("censored or incomplete (missing); outside the complete-family roster")
  paste(st, collapse = "; ")
}

#' One row per Stage 31-31d output or model context x affected CC4 cage episode, plus the records that do not act.
cc4ea_affected <- function(elig, ro, man) {
  m <- data.table::as.data.table(man)
  eps <- unique(m[CC == "CC4"]$CageEpisodeID)
  phase_set <- function(p) switch(p, "A1-A5" = paste0("A", 1:5), "A2-A5" = paste0("A", 2:5), "I2-I5" = paste0("I", 2:5), character(0))
  inr <- function(stage, which, ep, fam) {
    if (is.na(stage)) return(NA_character_)
    x <- ro$included[Stage == stage & roster == which & CageID == ep & family == fam]
    if (nrow(x)) sprintf("in (%d animals)", data.table::uniqueN(x$AnimalKey)) else "out"
  }
  rows <- list()
  for (ep in eps) for (i in seq_len(nrow(CC4EA_OUTPUTS))) {
    o <- CC4EA_OUTPUTS[i, ]
    cells <- elig[CageID == ep & family == o$family & PhaseLabel %in% phase_set(o$phases)]
    sex <- unique(elig[CageID == ep]$Sex)
    applied <- if (o$Stage == "31") "not wired: Stage 31 is descriptive and unchanged; join tables/outcome_episode_eligibility.csv by AnimalKey x PhaseLabel" else
      paste("rerun with MMM_CC4_OUTCOME_ELIGIBILITY=<this run>/tables/outcome_episode_eligibility.csv into an isolated root;",
            "the inactive sensitivity uses the eligibility file of a --variant=inactive_sensitivity run")
    rows[[length(rows) + 1L]] <- data.table::data.table(
      Stage = o$Stage, output_or_context = sub("<sex>", sex, o$output, fixed = TRUE), family = o$family, phases = o$phases,
      CageEpisodeID = ep, records = if (o$family == "QC") cc4e_collapse(m[CageEpisodeID == ep]$record_id) else cc4e_collapse(unlist(strsplit(cells$records, ";", fixed = TRUE))),
      cells = if (o$family == "QC") "all phases: kept" else cc4ea_status_summary(cells),
      consequence = if (o$family == "QC") "unaffected: QC table, kept (bracketing does not establish sensor health)" else cc4ea_consequence(unique(cells$status)),
      animals = data.table::uniqueN(elig[CageID == ep]$AnimalKey),
      frozen_roster = inr(o$roster_stage, "frozen", ep, o$family), primary_roster = inr(o$roster_stage, "primary", ep, o$family),
      inactive_sensitivity_roster = inr(o$roster_stage, "sensitivity", ep, o$family),
      outcome_dependency = o$dependency, applied_by = applied)
  }
  fem <- data.table::data.table(Stage = c("31c", "31c", "31d", "31d"), output_or_context = c("Female_Active", "Female_Inactive", "Female_Active", "Female_Inactive"),
    family = c("Active", "Inactive", "Active", "Inactive"), phases = c("A2-A5", "I2-I5", "A2-A5", "I2-I5"), CageEpisodeID = "none",
    records = "", cells = "", consequence = "unaffected: B3, B4 and B6 have no manifest record", animals = NA_integer_,
    outcome_dependency = "", applied_by = "")
  holm <- data.table::data.table(Stage = "31c/31d", output_or_context = "Holm adjustment across contexts (31c n=4; 31d omnibus n=4, contrasts n=12)",
    family = "both", phases = "", CageEpisodeID = paste(eps, collapse = "; "), records = "", cells = "",
    consequence = "a changed Male Active P changes the adjusted P of every context, including the unaffected Female ones", animals = NA_integer_,
    outcome_dependency = "multiplicity adjustment spans all contexts", applied_by = "rerun of 31c/31d with the rule")
  cc3 <- m[CC != "CC4"]
  other <- data.table::data.table(Stage = "31-31d", output_or_context = c("all outputs", "all outputs"), family = "both", phases = "",
    CageEpisodeID = c(cc4e_collapse(cc3$CageEpisodeID), cc4e_collapse(m[action == "exclude_spatial"]$CageEpisodeID)),
    records = c(cc4e_collapse(cc3$record_id), cc4e_collapse(m[action == "exclude_spatial"]$record_id)), cells = "",
    consequence = c("unaffected: Stage 31-31d read CC4 only; these records apply to analyses of their own cage change",
                    "not applied as such: exclude_spatial covers occupancy, co-location and cage-use outcomes, which Stage 31-31d do not have; the dead reader acts through the movement rule (E05-E08)"),
    animals = NA_integer_, outcome_dependency = "", applied_by = "")
  data.table::rbindlist(list(data.table::rbindlist(rows), fem, holm, other), fill = TRUE)
}

#' CC1-CC4 cage episodes (srs_v1.1 animals table) of every animal named in a manifest record, with the records of each
#' episode and what the CC4 rule does to it; the CC4 episode must equal the animal's Stage 31b CageID.
cc4ea_cross_cc <- function(srs, man, elig) {
  m <- data.table::as.data.table(man)
  animals <- unique(unlist(strsplit(m$animals, ";", fixed = TRUE)))
  x <- data.table::as.data.table(srs)[AnimalNum %in% animals, .(AnimalNum, Batch, CC, board, System, CageEpisodeID,
    srs_spatial_excluded = spatial_excluded, srs_movement_excluded = movement_excluded, srs_censor_from = censor_from)]
  rec <- m[, .(manifest_records = cc4e_collapse(record_id), manifest_actions = cc4e_collapse(action)), by = CageEpisodeID]
  x <- merge(x, rec, by = "CageEpisodeID", all.x = TRUE)
  x[is.na(manifest_records), `:=`(manifest_records = "", manifest_actions = "")]
  b <- unique(elig[, .(AnimalNum, Batch, CageID)])
  cc4 <- x[CC == "CC4"]
  k <- match(paste(cc4$Batch, cc4$AnimalNum), paste(b$Batch, b$AnimalNum))
  if (anyNA(k) || any(b$CageID[k] != cc4$CageEpisodeID)) stop("The srs CC4 cage episode of an animal differs from its Stage 31b CageID.", call. = FALSE)
  eff <- elig[, .(e = paste0(family[1], " ", cc4ea_status_summary(.SD))), by = .(AnimalNum, Batch, family)][, .(effect = paste(e, collapse = " | ")), by = .(AnimalNum, Batch)]
  x[, read_by_stage31_31d := CC == "CC4"]
  x[, cc4e_effect := ifelse(CC == "CC4", eff$effect[match(paste(Batch, AnimalNum), paste(eff$Batch, eff$AnimalNum))],
                            ifelse(nzchar(manifest_records), paste0("not read by Stage 31-31d; its own records (", manifest_records, ") apply to ", CC, " analyses only"),
                                   "not read by Stage 31-31d; unaffected by any CC4 record"))]
  x[order(AnimalNum, CC)]
}

#' Missing versus excluded: cells by status, and per Sex x family how many animals contribute to the primary 31c/31d
#' model and why the others do not; no animal leaves the roster.
cc4ea_missing_vs_excluded <- function(elig, ro, n_table_animals) {
  cat_of <- c(included = "observed: primary", retained_flagged = "observed: primary, flagged", retained_unverified = "observed: primary, flagged (unverified)",
              retained_material = "observed: primary, flagged (CC4 knockout material; decision for Tobias)",
              excluded_hardware = "excluded: hardware fault, kept in the all-episode sensitivity", censored = "missing: unobserved after the censoring time",
              incomplete_span = "missing: incomplete span", cage_roster_incomplete = "observed: outside the complete-family roster")
  s <- elig[, .(cells = .N, animals = data.table::uniqueN(AnimalKey), cages = data.table::uniqueN(CageID),
                records = cc4e_collapse(unlist(strsplit(records, ";", fixed = TRUE)))), by = .(Sex, family, status)]
  s[, `:=`(row_type = "cells_by_status", category = unname(cat_of[status]))]
  model <- ro$included[Stage == "31c" & roster == "primary"]
  fam <- elig[PhaseLabel != "A1", .(excluded = any(status == "excluded_hardware"), missing = any(status %in% c("censored", "incomplete_span"))),
              by = .(Sex, family, AnimalKey, CageID)]
  fam[, in_model := paste(AnimalKey, family) %in% paste(model$AnimalKey, model$family)]
  fam[, category := ifelse(in_model, "in the primary 31c/31d model",
                    ifelse(excluded & missing, "not in the model: excluded (hardware) and censored or incomplete cells",
                    ifelse(excluded, "not in the model: family excluded (hardware); in the all-episode sensitivity",
                    ifelse(missing, "not in the model: censored or incomplete cells (missing)", "not in the model: cage roster incomplete"))))]
  a <- fam[, .(animals = data.table::uniqueN(AnimalKey), cages = data.table::uniqueN(CageID)), by = .(Sex, family, category)]
  a[, row_type := "animals_in_31c_31d_families"]
  roster <- elig[, .(animals = data.table::uniqueN(AnimalKey), cages = data.table::uniqueN(CageID)), by = Sex]
  roster[, `:=`(row_type = "animal_roster", family = "both", category = "animals in the roster (never removed by an episode outcome)")]
  none <- fam[, .(any_model = any(in_model)), by = .(Sex, AnimalKey, CageID)][any_model == FALSE]
  none <- none[, .(animals = data.table::uniqueN(AnimalKey), cages = data.table::uniqueN(CageID)), by = Sex]
  if (nrow(none)) none[, `:=`(row_type = "animal_roster", family = "both",
                              category = "animals contributing to no CC4 model family (still in the roster; their CC1-CC3 data unaffected)")]
  removed <- data.table::data.table(row_type = "animal_roster", Sex = "all", family = "both", category = "animals removed from the roster",
                                    animals = n_table_animals - data.table::uniqueN(elig$AnimalKey), cages = NA_integer_)
  out <- data.table::rbindlist(list(s, a, roster, none, removed), fill = TRUE)
  data.table::setcolorder(out, c("row_type", "Sex", "family", "status", "category", "cells", "animals", "cages", "records"))
  out[order(row_type, Sex, family, match(status, CC4E_STATUSES))]
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({ library(data.table); library(dplyr) })
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "behavior_analysis_config.R", "cc4_grid_exposure_helpers.R", "cc4_whole_inactive_helpers.R",
              "cc4_phase_group_helpers.R", "cc4_phase_statistics_helpers.R", "spatial_exclusion_manifest.R", "cc4_outcome_eligibility.R",
              "qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R")) source_mmm_helper(h)
  a <- cc4ea_args(commandArgs(trailingOnly = TRUE))
  project_root <- mmm_project_root()
  frozen_root <- file.path(project_root, "analysis_ready", "analyses", "cc4_grid_exposure")
  pg <- if (is.na(a$phase_groups)) file.path(frozen_root, CC4EA_FROZEN[["phase_groups"]]) else a$phase_groups
  ps <- if (is.na(a$phase_statistics)) file.path(frozen_root, CC4EA_FROZEN[["phase_statistics"]]) else a$phase_statistics
  pp <- if (is.na(a$phase_phenotypes)) file.path(frozen_root, CC4EA_FROZEN[["phase_phenotypes"]]) else a$phase_phenotypes
  head_commit <- cc4ea_git(c("rev-parse", "HEAD"))[1]
  run_id <- if (is.na(a$run_id)) paste0(CC4E_VERSION, "_", substr(head_commit, 1, 7), if (a$variant == "inactive_sensitivity") "_isens" else "") else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  dev <- startsWith(run_id, "dev")
  dirty <- cc4ea_git(c("status", "--porcelain", "--untracked-files=all", "--", CC4EA_CODE, SXM_DIR))
  if (!dev && length(dirty)) stop("A release run needs committed code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "cc4_outcome_eligibility") else a$output_root,
                                         project_root)
  out_dir <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out_dir)) stop("Run folder exists; choose a new --run-id: ", out_dir, call. = FALSE)
  if (file.exists(tmp)) stop("A partial run exists; inspect and remove it first: ", tmp, call. = FALSE)
  mmm_assert_output_path_budget(c(file.path(tmp, CC4EA_LONGEST_RELATIVE), file.path(out_dir, CC4EA_LONGEST_RELATIVE)), source_label = "cc4 eligibility audit")
  guard <- mmm_protection_guard_begin(base, run_id, "cc4_outcome_eligibility", project_root)
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # inputs, each verified against its manifest
  for (r in c(pg, ps, pp)) invisible(grid31_verify_manifest(r, "manifest.csv"))
  apa <- file.path(pg, "animal_phase_activity.csv"); apa_sha <- cc4ea_sha(apa)
  for (r in c(ps, pp)) {
    im <- utils::read.csv(file.path(r, "input_manifest.csv"), colClasses = "character")
    if (!any(basename(im$Path) == "animal_phase_activity.csv" & im$SHA256 == apa_sha)) stop(r, " was not computed from ", apa, call. = FALSE)
  }
  im_pg <- utils::read.csv(file.path(pg, "input_manifest.csv"), colClasses = "character")
  s31_om <- im_pg$Path[grepl("/audit/output_manifest[.]csv$", im_pg$Path)]
  if (length(s31_om) != 1L || !file.exists(s31_om) || cc4ea_sha(s31_om) != im_pg$SHA256[im_pg$Path == s31_om])
    stop("The Stage 31 output manifest recorded by ", pg, " is missing or changed.", call. = FALSE)
  s31 <- dirname(dirname(s31_om))
  om <- utils::read.csv(s31_om, colClasses = "character")
  for (f in c("audit/input_manifest.csv", "audit/whole_inactive_population.csv")) {
    if (cc4ea_sha(file.path(s31, f)) != om$SHA256[om$File == f]) stop("Stage 31 file changed: ", file.path(s31, f), call. = FALSE)
  }
  if (im_pg$SHA256[basename(im_pg$Path) == "animal_clock_profiles_5min.csv"] != om$SHA256[om$File == "tables/animal_clock_profiles_5min.csv"])
    stop("The 31b run read other clock bins than its Stage 31 run lists.", call. = FALSE)
  im31 <- utils::read.csv(file.path(s31, "audit", "input_manifest.csv"), colClasses = "character")
  reg <- im31$Path[basename(im31$Path) == "cage_label_corrections.csv"]
  spec <- MMM_BEHAVIOR_CONFIG$data_versions[[MMM_BEHAVIOR_CONFIG$data_versions$release]]$supporting_files$cage_label_corrections
  if (length(reg) != 1L || !file.exists(reg) || cc4ea_sha(reg) != im31$SHA256[im31$Path == reg] || cc4ea_sha(reg) != spec$sha256)
    stop("The cage-label correction registry is missing or differs from the one Stage 31 read and the configured one.", call. = FALSE)
  corrections <- utils::read.csv(reg, colClasses = "character", check.names = FALSE)
  srs <- file.path(dirname(project_root), "RANM", "single_read_spatial", "runs", CC4EA_SRS_RUN)
  srs_sha <- cc4e_verify_listed(srs, "tables/animals.csv")
  srs_animals <- data.table::fread(file.path(srs, "tables", "animals.csv"), colClasses = c(AnimalNum = "character"),
                                   select = c("AnimalNum", "Batch", "CC", "board", "System", "CageEpisodeID", "spatial_excluded", "movement_excluded", "censor_from"))
  ko <- if (is.na(a$knockout_run)) NULL else cc4e_read_knockout(a$knockout_run, release = !dev)   # a release needs a release knockout

  # the rule
  ap <- cc4ea_read_phases(apa)
  MAN <- cc4e_manifest()
  cc4e_check_corrections(MAN, corrections)
  elig <- cc4e_status(ap, MAN, ko)
  sens <- cc4e_inactive_sensitivity(elig)
  ro <- cc4ea_rosters(ap, elig, sens)
  parity <- cc4ea_parity(ro$runs, pg, ps, pp, s31)
  if (any(!parity$values_identical)) stop("The stage functions without the rule do not reproduce the frozen tables:\n",
                                          paste(parity[values_identical == FALSE]$file, collapse = "\n"), call. = FALSE)
  if (any(ro$checks[roster == "frozen"]$check != "pass")) stop("A frozen roster fails its own stage gate.", call. = FALSE)
  sizes <- cc4ea_sample_sizes(ro)
  affected <- cc4ea_affected(elig, ro, MAN)
  cross <- cc4ea_cross_cc(srs_animals, MAN, elig)
  mve <- cc4ea_missing_vs_excluded(elig, ro, data.table::uniqueN(ap$AnimalKey))

  # outputs
  for (dd in c("tables", "manifest")) dir.create(file.path(tmp, dd), recursive = TRUE, showWarnings = FALSE)
  written <- if (a$variant == "inactive_sensitivity") sens else elig   # the eligibility file Stages 31b-31d read
  tabs <- list(rules_used = data.table::as.data.table(CC4E_RULES), outcome_episode_eligibility = written, affected_analyses = affected,
               sample_sizes = sizes, cross_cc_episodes = cross, missing_vs_excluded = mve, frozen_parity = parity,
               roster_checks = ro$checks)
  if (!is.null(ko)) tabs$knockout_used <- data.table::as.data.table(ko)
  for (nm in names(tabs)) data.table::fwrite(tabs[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  code <- data.table::data.table(file = c(CC4EA_CODE, file.path(SXM_DIR, basename(attr(MAN, "path")))))
  code[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  data.table::fwrite(code, file.path(tmp, "manifest", "code_hashes.csv"))
  inputs <- data.table::data.table(
    role = c("31b manifest", "31b animal table", "31b input manifest", "31c manifest", "31c input manifest", "31d manifest", "31d input manifest",
             "Stage 31 output manifest", "Stage 31 input manifest", "Stage 31 whole-inactive population", "cage-label correction registry",
             "srs animals table", "srs run manifest"),
    input = c(file.path(pg, "manifest.csv"), apa, file.path(pg, "input_manifest.csv"), file.path(ps, "manifest.csv"), file.path(ps, "input_manifest.csv"),
              file.path(pp, "manifest.csv"), file.path(pp, "input_manifest.csv"), s31_om, file.path(s31, "audit", "input_manifest.csv"),
              file.path(s31, "audit", "whole_inactive_population.csv"), reg, file.path(srs, "tables", "animals.csv"), file.path(srs, CC4E_RUN_MANIFEST)))
  if (!is.null(ko)) inputs <- rbind(inputs, data.table::data.table(role = c("knockout materiality", "knockout run manifest"),
                                                                   input = c(attr(ko, "path"), file.path(a$knockout_run, CC4E_RUN_MANIFEST))))
  inputs <- rbind(inputs, data.table::data.table(role = "frozen table compared (frozen_parity.csv)",
                                                 input = setdiff(unique(file.path(c(pg, ps, pp, s31)[match(parity$run, basename(c(pg, ps, pp, s31)))], parity$file)), inputs$input)))
  inputs[, `:=`(sha256 = mmm_file_sha256(input), bytes = file.size(input))]
  data.table::fwrite(inputs, file.path(tmp, "manifest", "input_hashes.csv"))
  data.table::fwrite(MAN, file.path(tmp, "manifest", "spatial_exclusions_used.csv"))
  kor <- function(k) if (is.null(ko) || is.na(attr(ko, "record")[k])) "" else unname(attr(ko, "record")[k])
  male_active <- sizes[Stage == "31c" & Sex == "Male" & family == "Active" & Group == "all"]
  male_inactive <- sizes[Stage == "31c" & Sex == "Male" & family == "Inactive" & Group == "all"]
  run <- data.table::data.table(
    item = c("run_id", "rule_version", "variant", "git_commit", "code_clean", "dev_run", "manifest_version", "manifest_sha256", "phase_groups",
             "phase_statistics", "phase_phenotypes", "stage31_run", "srs_run", "srs_animals_sha256", "knockout_run", "knockout_sha256",
             "knockout_run_id", "knockout_git_commit", "knockout_release", "knockout_dev_run", "knockout_parity_gate_pass", "knockout_recordings",
             "retained_material_cells", "project_root", "started", "finished", "R"),
    value = c(run_id, CC4E_VERSION, a$variant, head_commit, as.character(!length(dirty)), as.character(dev), CC4E_MANIFEST_VERSION, attr(MAN, "sha256"),
              pg, ps, pp, s31, srs, srs_sha, if (is.null(ko)) "" else a$knockout_run, if (is.null(ko)) "" else attr(ko, "sha256"),
              kor("run_id"), kor("git_commit"), if (is.null(ko)) "" else as.character(attr(ko, "release")), kor("dev_run"), kor("parity_gate_pass"),
              kor("recordings"), sum(elig$status == "retained_material"), project_root, started, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string))
  data.table::fwrite(run, file.path(tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(tmp, "manifest", "session_info.txt"))
  st <- elig[, .N, by = .(status)][order(match(status, CC4E_STATUSES))]
  writeLines(c(paste("CC4 OUTCOME x CAGE-EPISODE ELIGIBILITY", CC4E_VERSION, "(UNREGISTERED; isolated; no model fitted; no stage rerun)"), "",
               paste("run:", run_id, "| variant:", a$variant, "| commit:", head_commit, "| manifest:", CC4E_MANIFEST_VERSION, "| knockout:",
                     if (is.null(ko)) "none (inactive cells of dead-reader episodes: retained_unverified)" else
                       paste0(a$knockout_run, if (isTRUE(attr(ko, "release"))) " (release)" else " (DEV knockout: not a verdict)")), "",
               if (a$variant == "inactive_sensitivity") "VARIANT inactive_sensitivity: the eligibility file leaves the retained inactive cells of dead-reader episodes out (for the sensitivity rerun)." else
                 "VARIANT primary: the eligibility file of the primary analyses.",
               "Rule (tables/rules_used.csv): every Stage 31-31d outcome is a vendor crossing count; exclude_spatial is not applied as such.",
               "A whole-session dead reader with its movement record excludes the episode's active family (A1-A5). Its inactive family is kept",
               "(manifest v1.1 keeps the light-phase crossing rate): retained_unverified without a CC4 knockout, retained_flagged if the knockout",
               "is not material, retained_material if it is (kept and flagged; an exclusion would need an approved new rule version).",
               if (any(elig$status == "retained_material")) paste0("DECISION FOR TOBIAS: the CC4 knockout is material for the inactive family of ",
                 cc4e_collapse(elig[status == "retained_material"]$CageID), " (", cc4e_collapse(elig[status == "retained_material"]$note), ").") else
                 "No inactive cell is retained_material.",
               "Censored phases are missing, not excluded. No animal leaves the roster; a family's roster drops only that family.",
               paste0("Cells by status: ", paste(paste0(st$status, " ", st$N), collapse = ", "), "."), "",
               sprintf("31c/31d Male Active: %d -> %d cages, %d -> %d animals (frozen -> rule-applied primary).",
                       male_active$Cages_frozen, male_active$Cages_primary, male_active$Animals_frozen, male_active$Animals_primary),
               sprintf("31c/31d Male Inactive: %d -> %d cages, %d -> %d animals; inactive sensitivity %d cages, %d animals.",
                       male_inactive$Cages_frozen, male_inactive$Cages_primary, male_inactive$Animals_frozen, male_inactive$Animals_primary,
                       male_inactive$Cages_sensitivity, male_inactive$Animals_sensitivity),
               sprintf("Frozen parity: %d of %d frozen tables recomputed byte-identical, %d value-identical (tables/frozen_parity.csv).",
                       sum(parity$bytes_identical), nrow(parity), sum(parity$values_identical)), "",
               "tables/outcome_episode_eligibility.csv  one row per animal x CC4 cage episode x family x phase: status, records, primary use",
               "tables/sample_sizes.csv                 per Stage x Sex x family x Group: frozen, primary and inactive-sensitivity rosters",
               "tables/affected_analyses.csv            each Stage 31-31d output or model context x affected episode: consequence",
               "tables/missing_vs_excluded.csv          missing (censored, incomplete) vs excluded cells; animals per model family",
               "tables/cross_cc_episodes.csv            CC1-CC4 episodes of the animals named in the manifest; earlier CCs unaffected",
               "tables/frozen_parity.csv                the stage functions without the rule reproduce the frozen tables",
               "Apply to Stages 31b-31d: MMM_CC4_OUTCOME_ELIGIBILITY=<this run>/tables/outcome_episode_eligibility.csv, output into an isolated root.",
               "The frozen runs remain the all-episode sensitivity. Documentation: docs/CC4_OUTCOME_ELIGIBILITY.md."),
             file.path(tmp, "README.txt"))
  mmm_protection_guard_end(guard, file.path(tmp, "manifest"))
  outs <- list.files(tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
                     file.path(tmp, CC4E_RUN_MANIFEST))
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  message("cc4 outcome eligibility audit done: ", out_dir)
}
