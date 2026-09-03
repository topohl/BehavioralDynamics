## STAGE 09 STALE-ARTIFACT FORENSIC AUDIT
## Characterizes every Stage-09-related artifact family against the CURRENT code contract in
## Analysis/09_early_prediction_model_ladder.R (select_primary_active_window):
##   first Active phase block after the first cage change, 18:30 inclusive -> 06:30 exclusive,
##   12 h, 72 slots at 10-min, Active phase ONLY (exact membership), CC1 only, canonical 111 animals.
## Read-only. Does not modify or rerun Stage 09.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("hmm_stage14_helpers.R")
OUT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap"
RFID <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
AR <- file.path(RFID, "analysis_ready")
SNAP <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID_snapshot_stage09_erroneous_active_plus_inactive_24h_20260827"

roster <- build_canonical_identity_roster(
  read_csv(file.path(AR,"03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types = cols(.default=col_skip(), AnimalNum=col_character(), Group=col_character(), Sex=col_character()),
    progress=FALSE), "roster")
CANON_N <- nrow(roster)

## Artifact families to characterize: label, dir, producer (if known from manifest/provenance)
families <- tribble(
  ~family, ~dir, ~producer_declared,
  "CURRENT canonical Stage 09 (10min)", file.path(AR,"pipeline/09_early_prediction/10min/tables"),
    "Analysis/09_early_prediction_model_ladder.R",
  "LEGACY early_prediction 10min_based", file.path(AR,"06_behavioral_dynamics/early_prediction/10min_based/tables"),
    "Analysis/_archive/08_early_prediction_models.R (per output_manifest.csv)",
  "LEGACY early_prediction 5min_based", file.path(AR,"06_behavioral_dynamics/early_prediction/5min_based/tables"),
    "Analysis/_archive/08_early_prediction_models.R (inferred, same generation)",
  "LEGACY early_prediction 1min_based", file.path(AR,"06_behavioral_dynamics/early_prediction/1min_based/tables"),
    "Analysis/_archive/08_early_prediction_models.R (inferred, same generation)",
  "LEGACY mirror early_prediction_model_ladder 10min_based", file.path(AR,"06_behavioral_dynamics/early_prediction_model_ladder/10min_based/tables"),
    "Analysis/09_early_prediction_model_ladder.R (pre-migration location)",
  "LEGACY mirror early_prediction_model_ladder 5min_based", file.path(AR,"06_behavioral_dynamics/early_prediction_model_ladder/5min_based/tables"),
    "Analysis/09_early_prediction_model_ladder.R (pre-migration location)",
  "QUARANTINED snapshot (erroneous 24 h)", file.path(SNAP,"09_early_prediction_10min/tables"),
    "Analysis/09_early_prediction_model_ladder.R at git 3c12151 (snapshot manifest)"
)

## Diagnostic files that carry the window design, in preference order per family
CANDIDATES <- c("early_window_design_by_animal.csv", "early_window_summary_by_animal.csv",
                "early_window_contract_summary.csv", "early_window_design_summary.csv",
                "early_behavior_features.csv", "early_behavior_features_wide.csv",
                "early_window_rows_used.csv")

characterize <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- suppressWarnings(try(read_csv(path, col_types = cols(.default = col_character()),
                                     progress = FALSE, n_max = 200000), silent = TRUE))
  if (inherits(d, "try-error")) return(NULL)
  nm <- names(d)
  ac <- intersect(c("AnimalNum","AnimalID","Animal"), nm)[1]
  animals <- if (!is.na(ac)) unique(d[[ac]]) else character(0)
  canon <- if (length(animals)) canonical_animal_id(animals) else character(0)
  ph <- if ("Phase" %in% nm) sort(unique(d$Phase)) else
        if ("selected_phase_labels" %in% nm) sort(unique(d$selected_phase_labels)) else NA_character_
  cc <- if ("CageChange" %in% nm) sort(unique(d$CageChange)) else
        if ("first_cage_change" %in% nm) sort(unique(d$first_cage_change)) else NA_character_
  binv <- intersect(c("n_selected_bins","n_early_bins","n_bins","expected_target_slots_per_animal",
                      "expected_bins_per_animal","observed_target_slots"), nm)
  win <- if (length(binv)) paste0(binv[1], " in {",
            paste(head(sort(unique(suppressWarnings(as.numeric(d[[binv[1]]])))), 6), collapse=","), "}") else NA_character_
  wdef <- if ("PrimaryWindowDefinition" %in% nm) unique(d$PrimaryWindowDefinition)[1] else
          if ("primary_window_definition" %in% nm) unique(d$primary_window_definition)[1] else
          if ("EarlyPhasePattern" %in% nm) paste0("EarlyPhasePattern=", unique(d$EarlyPhasePattern)[1]) else NA_character_
  tibble(
    n_rows = nrow(d),
    animal_count = if (length(animals)) n_distinct(animals) else NA_integer_,
    canonical_id_status = if (!length(animals)) "no animal column" else if (
        any(grepl("^0[0-9]", animals))) "NON-canonical: zero-padded IDs present" else if (
        !setequal(canon, roster$AnimalNum)) paste0("canonical format but roster mismatch (", n_distinct(canon), " vs ", CANON_N, ")")
        else "canonical, matches 111-animal roster",
    phases_present = paste(ph, collapse="; "),
    cage_changes_present = paste(cc, collapse="; "),
    effective_window = paste(na.omit(c(wdef, win)), collapse=" | ")
  )
}

rows <- list()
for (i in seq_len(nrow(families))) {
  fam <- families$family[i]; dir <- families$dir[i]
  if (!dir.exists(dir)) {
    rows[[length(rows)+1]] <- tibble(family=fam, file=NA_character_, dir=dir,
      timestamp=NA_character_, generating_script=families$producer_declared[i],
      n_rows=NA_integer_, animal_count=NA_integer_, canonical_id_status="DIRECTORY ABSENT",
      phases_present=NA_character_, cage_changes_present=NA_character_, effective_window=NA_character_,
      compatible_with_current_contract=NA, reason_incompatible="directory does not exist")
    next
  }
  present <- CANDIDATES[file.exists(file.path(dir, CANDIDATES))]
  for (f in present) {
    p <- file.path(dir, f)
    ch <- characterize(p)
    if (is.null(ch)) next
    ts <- format(file.info(p)$mtime, "%Y-%m-%d %H:%M")
    reasons <- character()
    if (!is.na(ch$animal_count) && ch$animal_count != CANON_N)
      reasons <- c(reasons, paste0("animal count ", ch$animal_count, " != canonical ", CANON_N))
    if (grepl("zero-padded", ch$canonical_id_status)) reasons <- c(reasons, "non-canonical zero-padded AnimalNum")
    if (grepl("[Ii]nactive", ch$phases_present)) reasons <- c(reasons, "Inactive-phase rows present (substring-regex phase bug)")
    if (grepl("CC2|CC3|CC4", ch$cage_changes_present)) reasons <- c(reasons, "cage changes beyond CC1 present despite first_cage_change_only")
    if (grepl("n_early_bins in \\{4", ch$effective_window)) reasons <- c(reasons, "n_early_bins = 4, not the declared 12 h design")
    if (grepl("EarlyPhasePattern=active\\|dark\\|night", ch$effective_window))
      reasons <- c(reasons, "permissive substring phase pattern (matches 'Inactive')")
    if (grepl("QUARANTINED", fam)) reasons <- c(reasons, "snapshot manifest declares methodological_status = invalid_for_declared_primary_12h_active_window")
    compat <- length(reasons) == 0
    rows[[length(rows)+1]] <- bind_cols(
      tibble(family=fam, file=f, dir=dir, timestamp=ts, generating_script=families$producer_declared[i]), ch,
      tibble(compatible_with_current_contract=compat,
             reason_incompatible=if (compat) "" else paste(reasons, collapse="; ")))
  }
}
audit <- bind_rows(rows) %>%
  mutate(current_contract = "first Active block after first cage change; 18:30 inclusive -> 06:30 exclusive; 12 h; 72 slots at 10-min; Active only (exact membership); CC1 only; canonical 111 animals",
         use_as_evidence_in_phaseB = "NO - Phase B derives the window from CODE only") %>%
  relocate(family, file, timestamp, generating_script, animal_count, canonical_id_status,
           phases_present, cage_changes_present, effective_window,
           compatible_with_current_contract, reason_incompatible)
write_csv(audit, file.path(OUT, "stage09_stale_artifact_audit.csv"))

cat("################ STAGE 09 ARTIFACT FAMILIES ################\n")
print(as.data.frame(audit %>% transmute(family=str_trunc(family,40), file=str_trunc(file,38),
  timestamp, animals=animal_count, compat=compatible_with_current_contract)), row.names=FALSE)
cat("\n################ INCOMPATIBILITY DETAIL ################\n")
for (i in seq_len(nrow(audit))) {
  a <- audit[i,]
  cat("\n[", ifelse(isTRUE(a$compatible_with_current_contract), "OK   ", "STALE"), "] ",
      a$family, " / ", a$file, "  (", a$timestamp, ")\n", sep="")
  cat("   animals: ", a$animal_count, " | ", a$canonical_id_status, "\n", sep="")
  cat("   phases: ", a$phases_present, " | CCs: ", a$cage_changes_present, "\n", sep="")
  cat("   window: ", a$effective_window, "\n", sep="")
  if (!isTRUE(a$compatible_with_current_contract)) cat("   REASON: ", a$reason_incompatible, "\n", sep="")
}

## Downstream consumers -------------------------------------------------------
cons <- tribble(
  ~consumer, ~code_ref, ~path_used, ~resolution_mode, ~resolves_to, ~risk,
  "Analysis/10_systems_feature_prediction_ladder.R", "10:75-86",
    "behavior_stage_tables(...,'09','early_prediction',bin) then legacy early_prediction_model_ladder",
    "resolve_behavior_artifact(): CANONICAL FIRST, legacy fallback",
    "current canonical pipeline/09_early_prediction/<res> when present", "LOW",
  "Analysis/14_systems_neuroscience_summary_dashboard.R", "14:595,2079-2082",
    "resolve_stage09_early_prediction_artifact()",
    "canonical-first by construction; _pipeline_setup.R:263-269 states a stale legacy file must never win",
    "current canonical pipeline/09_early_prediction/10min", "LOW",
  "Analysis/15_behavior_proteomics_integration.R", "15:28",
    "HARD-CODED S:/.../06_behavioral_dynamics/early_prediction/5min_based/tables/early_behavior_features.csv",
    "NONE - literal path, no resolver, no fallback",
    "LEGACY May-18 artifact of the archived producer", "HIGH",
  "Analysis/16_manuscript_behavior_report.R", "16:1797",
    "declares pipeline/09_early_prediction/10min/ as primary manuscript evidence, legacy retained",
    "registry entry: producer_migrated_legacy_artifacts_retained",
    "current canonical", "LOW"
)
write_csv(cons, file.path(OUT, "stage09_downstream_consumer_audit.csv"))
cat("\n################ DOWNSTREAM CONSUMERS ################\n")
print(as.data.frame(cons %>% transmute(consumer=str_trunc(consumer,48), code_ref, risk,
  resolution_mode=str_trunc(resolution_mode,44))), row.names=FALSE)

## Does the canonical tree exist at every resolution downstream asks for?
cat("\n################ CANONICAL COVERAGE BY RESOLUTION ################\n")
for (r in c("1min","5min","10min","30min")) {
  p <- file.path(AR, "pipeline/09_early_prediction", r, "tables")
  cat(sprintf("  pipeline/09_early_prediction/%-6s : %s\n", r, if (dir.exists(p)) "PRESENT" else "ABSENT"))
}
cat("  -> Stage 15 asks for 5min; canonical Stage 09 produces 10min only, so Stage 15 CANNOT be\n")
cat("     satisfied canonically and silently falls back to the stale legacy 5-min tree.\n")
cat("\nwrote stage09_stale_artifact_audit.csv and stage09_downstream_consumer_audit.csv\n")
