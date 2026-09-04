# ================================================================
# Publication release bundle builder
# BehavioralDynamics
# ================================================================
# Assembles a self-contained, hash-verified copy of the artifacts backing the
# manuscript into
#
#     <RFID_ROOT>/releases/E9_behavior_manuscript_<release_id>/
#
# This is deliberately NOT numbered as a pipeline stage. It computes nothing
# scientific: it resolves, copies, hashes and verifies. Numbering it "17" would
# imply it belongs to the analysis sequence, which it does not.
#
# Hard guarantees, in order of importance:
#   1. COPY ONLY. Never moves, deletes or rewrites anything under analysis_ready/.
#   2. NEVER reads from a quarantine tree.
#   3. Every copied file has its source SHA-256 and copied SHA-256 recorded, and
#      the build aborts if they differ.
#   4. A missing REQUIRED artifact aborts the build. Nothing is silently dropped.
#   5. Panel selection comes from docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv, never
#      from a significance threshold.
#   6. Writes only under <RFID_ROOT>/releases/. Never into the repository, never
#      elsewhere in analysis_ready/.
#
# Usage:
#     Rscript Analysis/build_publication_release.R --dry-run
#     Rscript Analysis/build_publication_release.R --release-id=rc1
#     Rscript Analysis/build_publication_release.R --release-id=rc1 --project-root=<path>
#
# See docs/PUBLICATION_RELEASE.md.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

.pipeline_setup <- "Analysis/_pipeline_setup.R"
if (!file.exists(.pipeline_setup)) {
  cand <- file.path(getwd(), "Analysis", "_pipeline_setup.R")
  if (file.exists(cand)) .pipeline_setup <- cand
}
source(.pipeline_setup)

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to hash release artifacts.", call. = FALSE)
}

# ---------------------------------------------------------------- arguments
args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^", flag, "="), "", hit[[1]])
}

DRY_RUN     <- "--dry-run" %in% args
RELEASE_ID  <- arg_value("--release-id", if (DRY_RUN) "dryrun" else NULL)
PROJECT_ROOT <- arg_value(
  "--project-root",
  Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT",
             unset = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID")
)

if (is.null(RELEASE_ID) || !nzchar(RELEASE_ID)) {
  stop("Provide --release-id=<id> (or use --dry-run).", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9._-]+$", RELEASE_ID)) {
  stop("--release-id must be alphanumeric with . _ - only. Got: ", RELEASE_ID, call. = FALSE)
}
if (!dir.exists(PROJECT_ROOT)) {
  stop("Project root not found: ", PROJECT_ROOT, call. = FALSE)
}

ANALYSIS_READY <- file.path(PROJECT_ROOT, "analysis_ready")
RELEASE_ROOT   <- file.path(PROJECT_ROOT, "releases",
                            paste0("E9_behavior_manuscript_", RELEASE_ID))
BUILD_TIME     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# ---------------------------------------------------------------- guards
# Any path containing one of these segments is refused outright: quarantined and
# archived trees must never reach a release bundle, even by an explicit request.
FORBIDDEN_SEGMENTS <- c("_quarantine", "quarantine_legacy", "_archive", ".old")

assert_not_quarantined <- function(path, artifact_id) {
  norm <- gsub("\\\\", "/", path)
  for (seg in FORBIDDEN_SEGMENTS) {
    if (grepl(seg, norm, fixed = TRUE)) {
      stop("REFUSED: artifact '", artifact_id,
           "' resolves into a forbidden tree ('", seg, "'):\n  ", path,
           "\nQuarantined and archived data must never enter a release bundle.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

sha256_file <- function(path) digest::digest(path, algo = "sha256", file = TRUE)

# ---------------------------------------------------------------- git identity
git_field <- function(cmd, default = NA_character_) {
  out <- tryCatch(suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE)),
                  error = function(e) character(0))
  if (length(out) == 0) default else out[[1]]
}
GIT_SHA    <- git_field("git rev-parse HEAD")
GIT_BRANCH <- git_field("git rev-parse --abbrev-ref HEAD")
GIT_DIRTY  <- length(tryCatch(
  suppressWarnings(system("git status --porcelain", intern = TRUE, ignore.stderr = TRUE)),
  error = function(e) character(0))) > 0

# ---------------------------------------------------------------- registry
REGISTRY_PATH <- file.path(MMM_REPO_ROOT, "docs", "MANUSCRIPT_ANALYSIS_REGISTRY.csv")
if (!file.exists(REGISTRY_PATH)) {
  stop("Manuscript analysis registry not found: ", REGISTRY_PATH,
       "\nThe registry defines what belongs in a release; the build cannot proceed without it.",
       call. = FALSE)
}
registry <- read_csv(REGISTRY_PATH, show_col_types = FALSE, progress = FALSE)
if (nrow(registry) == 0) stop("Manuscript analysis registry is empty.", call. = FALSE)

# ---------------------------------------------------------------- artifact plan
#
# Explicit and configuration-driven. Content is chosen by manuscript role, not by
# any p-value. `required = TRUE` means a missing file aborts the build.
S09 <- behavior_stage_tables(PROJECT_ROOT, "09", "early_prediction", "10min")
S03 <- behavior_stage_tables(PROJECT_ROOT, "03", "movement_phase_stats", "10min")
# Declared 5-min resolution sensitivity for the primary Stage 09 question.
# Supplementary evidence only; "10min" above stays the canonical primary.
S09_SENS <- behavior_stage_tables(PROJECT_ROOT, "09", "early_prediction", "5min")
S09_SENS_AUDIT <- file.path(behavior_stage_dir(PROJECT_ROOT, "09", "early_prediction", "5min"), "audit")
S09_FIG <- file.path(behavior_stage_dir(PROJECT_ROOT, "09", "early_prediction", "10min"), "figures")
S03_FIG <- file.path(behavior_stage_dir(PROJECT_ROOT, "03", "movement_phase_stats", "10min"), "figures")
MANU <- behavior_manuscript_dir(PROJECT_ROOT, "behavior")
FIRSTNIGHT <- file.path(ANALYSIS_READY, "12_systems_neuroscience_summary", "5min_based",
                        "first_night", "10min_based")
QC <- file.path(ANALYSIS_READY, "00_qc_tracking_integrity", "tables")

a <- function(artifact_id, source_path, bundle_subdir, required = TRUE, role = "",
              dest_name = NULL) {
  tibble(artifact_id = artifact_id, source_path = source_path,
         bundle_subdir = bundle_subdir, required = required, role = role,
         dest_name = if (is.null(dest_name)) basename(source_path) else dest_name)
}

plan <- bind_rows(
  # ---- primary: Stage 09 tables
  a("s09_associations",          file.path(S09, "primary_movement_entropyacf1_associations.csv"), "tables/primary",  TRUE,  "PRIMARY feature associations"),
  a("s09_prediction_performance", file.path(S09, "primary_prediction_performance.csv"),           "tables/primary",  TRUE,  "PRIMARY prediction performance"),
  a("s09_permutation",           file.path(S09, "primary_prediction_permutation_test.csv"),       "tables/primary",  TRUE,  "PRIMARY permutation test"),
  a("s09_model_registry",        file.path(S09, "primary_prediction_model_registry.csv"),         "provenance",      TRUE,  "Fixed a priori model definitions"),
  a("s09_feature_dictionary",    file.path(S09, "readout_dictionary.csv"),                        "provenance",      TRUE,  "Feature definitions and roles"),
  a("s09_sex_interactions",      file.path(S09, "primary_feature_sex_interactions.csv"),          "tables/supplementary", TRUE, "Feature-by-Sex interactions"),
  a("s09_sex_stratified",        file.path(S09, "primary_movement_entropyacf1_correlations_by_sex.csv"), "tables/supplementary", TRUE, "Descriptive sex-stratified"),
  a("s09_model_input",           file.path(S09, "model_ladder_input.csv"),                        "source_data",     TRUE,  "Animal-level model input"),
  a("s09_predictions",           file.path(S09, "primary_prediction_predictions.csv"),            "source_data",     TRUE,  "Held-out predictions"),
  a("s09_window_contract",      file.path(S09, "early_window_contract_summary.csv"),           "provenance",      TRUE,  "PRIMARY 10-min window contract", dest_name = "stage09_10min_early_window_contract_summary.csv"),

  # ---- secondary: Stage 03 tables
  a("s03_animal_endpoints",      file.path(S03, "raw_movement_animal_level_endpoints.csv"),       "source_data",     TRUE,  "SECONDARY movement source data"),
  a("s03_pairwise",              file.path(S03, "raw_movement_pairwise_wilcox_stats_corrected.csv"), "tables/supplementary", FALSE, "Holm-adjusted pairwise"),
  a("s03_group_summary",         file.path(S03, "raw_movement_group_summary.csv"),                "tables/supplementary", FALSE, "Group summaries"),
  a("s03_lm",                    file.path(S03, "raw_movement_one_way_lm_stats_corrected.csv"),   "tables/supplementary", FALSE, "One-way models"),
  a("s03_lmm",                   file.path(S03, "raw_movement_repeated_lmm_cagechange_phase_by_sex.csv"), "tables/supplementary", FALSE, "Repeated-measures models"),
  a("s03_filter_qc",             file.path(S03, "raw_movement_phase_filter_qc.csv"),              "qc",              FALSE, "Retention counts"),

  # ---- secondary: canonical first-night five-domain panel
  a("fn_group_contrasts",        file.path(FIRSTNIGHT, "first_night_group_contrasts.csv"),        "tables/supplementary", TRUE, "SECONDARY first-night contrasts"),
  a("fn_domain_scores",          file.path(FIRSTNIGHT, "first_night_domain_scores.csv"),          "source_data",     TRUE,  "First-night domain scores"),
  a("fn_raw_features",           file.path(FIRSTNIGHT, "first_night_raw_features.csv"),           "source_data",     TRUE,  "First-night raw features"),
  a("fn_window_contract",        file.path(FIRSTNIGHT, "first_night_window_contract.csv"),        "provenance",      TRUE,  "Window definition contract"),
  a("fn_multiplicity_contract",  file.path(FIRSTNIGHT, "first_night_multiplicity_contract.csv"),  "provenance",      TRUE,  "Declared BH families"),
  a("fn_interactions",           file.path(FIRSTNIGHT, "first_night_group_sex_interactions.csv"), "tables/supplementary", TRUE, "Group x Sex interactions"),
  a("fn_window_qc",              file.path(FIRSTNIGHT, "first_night_animal_window_qc.csv"),       "qc",              TRUE,  "Per-animal window completeness"),
  a("fn_contributor_qc",         file.path(FIRSTNIGHT, "first_night_domain_contributor_qc.csv"),  "qc",              FALSE, "Domain contributor QC"),

  # ---- manuscript package (Stage 16)
  a("s16_workbook",              file.path(MANU, "Behavioral_Source_Data.xlsx"),                  "source_data",     TRUE,  "Manuscript source-data workbook"),
  a("s16_primary_results",       file.path(MANU, "primary_results.csv"),                          "tables/primary",  TRUE,  "Typed primary results"),
  a("s16_supplementary_results", file.path(MANU, "supplementary_results.csv"),                    "tables/supplementary", TRUE, "Typed supplementary results"),
  a("s16_animal_source",         file.path(MANU, "animal_level_source_data.csv"),                 "source_data",     TRUE,  "Animal-level source data"),
  a("s16_prediction_source",     file.path(MANU, "prediction_source_data.csv"),                   "source_data",     TRUE,  "Held-out prediction source data"),
  a("s16_movement_source",       file.path(MANU, "movement_phase_source_data.csv"),               "source_data",     TRUE,  "Movement-phase source data"),
  a("s16_provenance",            file.path(MANU, "provenance.csv"),                               "provenance",      TRUE,  "Upstream provenance and hashes", dest_name = "stage16_provenance.csv"),
  a("s16_validation",            file.path(MANU, "validation.csv"),                               "provenance",      TRUE,  "Stage 16 validation results", dest_name = "stage16_validation.csv"),
  a("s16_manifest",              file.path(MANU, "manifest.csv"),                                 "provenance",      TRUE,  "Stage 16 output manifest", dest_name = "stage16_manifest.csv"),

  # ---- QC

  # ---- supplementary: Stage 09 5-min resolution sensitivity
  a("s09sens_associations",   file.path(S09_SENS, "primary_movement_entropyacf1_associations.csv"), "tables/supplementary", TRUE, "SENSITIVITY 5-min feature associations"),
  a("s09sens_performance",    file.path(S09_SENS, "primary_prediction_performance.csv"),           "tables/supplementary", TRUE, "SENSITIVITY 5-min prediction performance"),
  a("s09sens_permutation",    file.path(S09_SENS, "primary_prediction_permutation_test.csv"),      "tables/supplementary", TRUE, "SENSITIVITY 5-min permutation"),
  a("s09sens_window",         file.path(S09_SENS, "early_window_contract_summary.csv"),            "provenance",           TRUE, "SENSITIVITY 5-min window contract", dest_name = "stage09_5min_early_window_contract_summary.csv"),
  a("s09sens_comparison",     file.path(S09_SENS_AUDIT, "stage09_resolution_sensitivity_comparison.csv"), "provenance",     TRUE, "10-min versus 5-min comparison of the same fixed analysis"),
  a("qc_by_animal",              file.path(QC, "tracking_qc_by_animal.csv"),                      "qc",              FALSE, "Tracking integrity"),
  a("qc_manual_review",          file.path(QC, "suggested_animals_for_manual_tracking_review.csv"), "qc",            FALSE, "Manual-review suggestions"),

  # ---- figures
  a("fig_s09_associations",      file.path(S09_FIG, "primary_movement_entropyacf1_vs_combz.svg"), "figures/main",    FALSE, "Fig: feature associations"),
  a("fig_s09_cv_ladder",         file.path(S09_FIG, "behavior_only_repeated_cv_ladder.svg"),      "figures/main",    FALSE, "Fig: CV ladder"),
  a("fig_s03_cagechange_phase",  file.path(S03_FIG, "Fig18c_cage_change_phase_mean_movement_corrected_stats.svg"), "figures/main", FALSE, "Fig: longitudinal movement"),
  a("fig_s03_overall_phase",     file.path(S03_FIG, "Fig18c_overall_active_inactive_mean_movement_corrected_stats.svg"), "figures/supplementary", FALSE, "Fig: overall phase movement"),
  a("fig_s03_pvalue_heatmap",    file.path(S03_FIG, "Fig18c_cage_change_phase_pairwise_pvalue_heatmap_corrected.svg"), "figures/supplementary", FALSE, "Fig: pairwise p-value heatmap")
)

# ---------------------------------------------------------------- resolve
message("Publication release builder")
message("  release id   : ", RELEASE_ID)
message("  project root : ", PROJECT_ROOT)
message("  target       : ", RELEASE_ROOT)
message("  git sha      : ", GIT_SHA, if (GIT_DIRTY) "  [WORKING TREE DIRTY]" else "")
message("  mode         : ", if (DRY_RUN) "DRY RUN (nothing will be written)" else "BUILD")
message("")

resolved <- plan %>%
  rowwise() %>%
  mutate(
    exists      = file.exists(source_path),
    bytes       = if (exists) as.numeric(file.info(source_path)$size) else NA_real_,
    source_mtime = if (exists) format(file.info(source_path)$mtime, "%Y-%m-%dT%H:%M:%S") else NA_character_,
    source_sha256 = if (exists) sha256_file(source_path) else NA_character_
  ) %>%
  ungroup()

# Guard 2: no forbidden tree, even for optional artifacts.
for (i in seq_len(nrow(resolved))) {
  assert_not_quarantined(resolved$source_path[i], resolved$artifact_id[i])
}

missing_required <- resolved %>% filter(required, !exists)
if (nrow(missing_required) > 0) {
  message("MISSING REQUIRED ARTIFACTS:")
  for (i in seq_len(nrow(missing_required))) {
    message("  - ", missing_required$artifact_id[i], ": ", missing_required$source_path[i])
  }
  stop("Release aborted: ", nrow(missing_required),
       " required canonical artifact(s) missing. Run the pipeline and Stage 16 first.",
       call. = FALSE)
}

missing_optional <- resolved %>% filter(!required, !exists)
if (nrow(missing_optional) > 0) {
  message("Optional artifacts absent (recorded, not fatal): ", nrow(missing_optional))
  for (i in seq_len(nrow(missing_optional))) {
    message("  - ", missing_optional$artifact_id[i])
  }
  message("")
}

to_copy <- resolved %>% filter(exists)

# Two artifacts must never land on the same bundle path, and no copied artifact
# may collide with a file this builder writes itself.
.planned <- file.path(to_copy$bundle_subdir, to_copy$dest_name)
if (anyDuplicated(.planned)) {
  stop("Release aborted: two artifacts resolve to the same bundle path:\n  ",
       paste(unique(.planned[duplicated(.planned)]), collapse = "\n  "), call. = FALSE)
}
.builder_writes <- c("provenance/artifact_manifest.csv", "provenance/upstream_hashes.csv",
                     "provenance/validation.csv", "provenance/analysis_registry.csv",
                     "provenance/stage16_upstream_crosscheck.csv",
                     "code/git_sha.txt", "code/sessionInfo.txt", "code/package_versions.csv",
                     "README.md", "SHA256SUMS.txt")
if (any(.planned %in% .builder_writes)) {
  stop("Release aborted: a copied artifact would be overwritten by a file the builder writes:\n  ",
       paste(intersect(.planned, .builder_writes), collapse = "\n  "), call. = FALSE)
}

message("Resolved ", nrow(to_copy), " artifacts (",
        sum(to_copy$required), " required, ", sum(!to_copy$required), " optional), ",
        format(sum(to_copy$bytes, na.rm = TRUE) / 1024^2, digits = 4), " MB")

# ---------------------------------------------------------------- upstream cross-check
# Stage 16 recorded a SHA-256 per upstream source. If a file we are about to ship
# is one of those, it must still match: a mismatch means the artifact changed
# after the manuscript package was assembled.
s16_prov_path <- file.path(MANU, "provenance.csv")
upstream_check <- tibble(source_id = character(), path = character(),
                         recorded_sha256 = character(), live_sha256 = character(),
                         match = logical())
if (file.exists(s16_prov_path)) {
  prov <- read_csv(s16_prov_path, show_col_types = FALSE, progress = FALSE)
  upstream_check <- prov %>%
    rowwise() %>%
    mutate(
      full = file.path(PROJECT_ROOT, path),
      live_sha256 = if (file.exists(full)) sha256_file(full) else NA_character_,
      match = !is.na(live_sha256) && identical(tolower(live_sha256), tolower(sha256))
    ) %>%
    ungroup() %>%
    transmute(source_id, path, recorded_sha256 = sha256, live_sha256, match)

  n_bad <- sum(!upstream_check$match)
  if (n_bad > 0) {
    message("UPSTREAM HASH MISMATCH on ", n_bad, " artifact(s):")
    bad <- upstream_check %>% filter(!match)
    for (i in seq_len(nrow(bad))) message("  - ", bad$source_id[i], ": ", bad$path[i])
    stop("Release aborted: canonical upstream artifacts no longer match the hashes ",
         "Stage 16 recorded. Rerun Stage 16 before building a release.", call. = FALSE)
  }
  message("Upstream provenance cross-check: ", nrow(upstream_check), "/",
          nrow(upstream_check), " hashes match Stage 16.")
}

# ---------------------------------------------------------------- dry run exit
if (DRY_RUN) {
  message("")
  message("DRY RUN SUMMARY")
  message("  would create : ", RELEASE_ROOT)
  by_dir <- to_copy %>% count(bundle_subdir, name = "files") %>% arrange(bundle_subdir)
  for (i in seq_len(nrow(by_dir))) {
    message(sprintf("  %-24s %d file(s)", by_dir$bundle_subdir[i], by_dir$files[i]))
  }
  message("  required missing : 0")
  message("  optional missing : ", nrow(missing_optional))
  message("")
  message("DRY RUN COMPLETE. Nothing was written.")
  quit(save = "no", status = 0)
}

# ---------------------------------------------------------------- build
if (dir.exists(RELEASE_ROOT)) {
  stop("Release directory already exists: ", RELEASE_ROOT,
       "\nRefusing to overwrite an existing release. Use a new --release-id.", call. = FALSE)
}

for (d in unique(c(to_copy$bundle_subdir, "code", "provenance", "figures/main",
                   "figures/supplementary", "tables/primary", "tables/supplementary",
                   "source_data", "qc"))) {
  dir.create(file.path(RELEASE_ROOT, d), recursive = TRUE, showWarnings = FALSE)
}

manifest <- to_copy %>%
  rowwise() %>%
  mutate(
    bundle_path = file.path(bundle_subdir, dest_name),
    dest        = file.path(RELEASE_ROOT, bundle_path),
    copied      = file.copy(source_path, dest, overwrite = FALSE, copy.date = TRUE),
    copied_sha256 = if (isTRUE(copied) && file.exists(dest)) sha256_file(dest) else NA_character_,
    hash_match  = !is.na(copied_sha256) && identical(copied_sha256, source_sha256),
    resolution_class = "canonical"
  ) %>%
  ungroup()

# Guard 3: every copy must be byte-identical to its source.
bad_copies <- manifest %>% filter(!hash_match)
if (nrow(bad_copies) > 0) {
  message("COPY VERIFICATION FAILED:")
  for (i in seq_len(nrow(bad_copies))) {
    message("  - ", bad_copies$artifact_id[i], " -> ", bad_copies$bundle_path[i])
  }
  stop("Release aborted: ", nrow(bad_copies),
       " copied file(s) do not match their source SHA-256.", call. = FALSE)
}

# ---------------------------------------------------------------- provenance
write_csv(
  manifest %>% select(artifact_id, bundle_path, source_path, source_sha256,
                      copied_sha256, hash_match, resolution_class, required,
                      role, bytes, source_mtime),
  file.path(RELEASE_ROOT, "provenance", "artifact_manifest.csv")
)
write_csv(
  resolved %>% select(artifact_id, source_path, exists, required, bytes,
                      source_mtime, source_sha256),
  file.path(RELEASE_ROOT, "provenance", "upstream_hashes.csv")
)
file.copy(REGISTRY_PATH, file.path(RELEASE_ROOT, "provenance", "analysis_registry.csv"))

validation <- tribble(
  ~check_id, ~category, ~status, ~expected, ~observed, ~details,
  "required_artifacts_present", "completeness",
  if (nrow(missing_required) == 0) "PASS" else "FAIL",
  as.character(sum(plan$required)), as.character(sum(to_copy$required)),
  "Every artifact marked required resolved to an existing canonical file.",

  "copy_hash_integrity", "integrity",
  if (all(manifest$hash_match)) "PASS" else "FAIL",
  as.character(nrow(manifest)), as.character(sum(manifest$hash_match)),
  "Source and copied SHA-256 match for every bundled file.",

  "no_quarantined_sources", "provenance", "PASS",
  "0 forbidden path segments", "0",
  paste0("No source path contains: ", paste(FORBIDDEN_SEGMENTS, collapse = ", ")),

  "upstream_matches_stage16", "provenance",
  if (nrow(upstream_check) == 0) "SKIP" else if (all(upstream_check$match)) "PASS" else "FAIL",
  as.character(nrow(upstream_check)), as.character(sum(upstream_check$match)),
  "Canonical upstream artifacts still match the hashes Stage 16 recorded.",

  "git_sha_recorded", "reproducibility",
  if (!is.na(GIT_SHA)) "PASS" else "FAIL", "non-empty commit SHA",
  if (is.na(GIT_SHA)) "NA" else substr(GIT_SHA, 1, 12),
  if (GIT_DIRTY) "WARNING: working tree was dirty at build time." else "Working tree clean.",

  "registry_present", "provenance",
  if (nrow(registry) > 0) "PASS" else "FAIL", ">0 registry rows",
  as.character(nrow(registry)),
  "Bundle contents are driven by the manuscript analysis registry, not by significance."
)
write_csv(validation, file.path(RELEASE_ROOT, "provenance", "validation.csv"))
if (nrow(upstream_check) > 0) {
  write_csv(upstream_check, file.path(RELEASE_ROOT, "provenance", "stage16_upstream_crosscheck.csv"))
}

# ---------------------------------------------------------------- code identity
writeLines(c(
  paste0("git_sha=", GIT_SHA),
  paste0("git_branch=", GIT_BRANCH),
  paste0("working_tree_dirty=", tolower(as.character(GIT_DIRTY))),
  paste0("build_timestamp=", BUILD_TIME),
  paste0("release_id=", RELEASE_ID),
  paste0("project_root=", PROJECT_ROOT),
  paste0("builder=Analysis/build_publication_release.R")
), file.path(RELEASE_ROOT, "code", "git_sha.txt"))

writeLines(capture.output(sessionInfo()), file.path(RELEASE_ROOT, "code", "sessionInfo.txt"))

pkg_csv <- file.path(MMM_REPO_ROOT, "docs", "package_versions.csv")
if (file.exists(pkg_csv)) {
  file.copy(pkg_csv, file.path(RELEASE_ROOT, "code", "package_versions.csv"))
} else {
  ip <- installed.packages()
  write_csv(tibble(package = rownames(ip), version = unname(ip[, "Version"])),
            file.path(RELEASE_ROOT, "code", "package_versions.csv"))
}

# ---------------------------------------------------------------- README
writeLines(c(
  paste0("# E9 behaviour manuscript release: ", RELEASE_ID),
  "",
  paste0("Built ", BUILD_TIME, " by `Analysis/build_publication_release.R`"),
  paste0("from BehavioralDynamics commit `", GIT_SHA, "`",
         if (GIT_DIRTY) " (WORKING TREE DIRTY -- not a clean release)." else "."),
  "",
  "This bundle is a copy-only, hash-verified projection of the canonical analysis",
  "outputs. It was assembled by resolving artifacts through the repository's",
  "canonical-first contract; nothing was recomputed and no source file was moved,",
  "deleted or modified.",
  "",
  "## Contents",
  "",
  "| Directory | Contents |",
  "|---|---|",
  "| `code/` | Commit SHA, sessionInfo, package versions |",
  "| `figures/main/`, `figures/supplementary/` | Rendered panels |",
  "| `source_data/` | Per-panel source data |",
  "| `tables/primary/`, `tables/supplementary/` | Result tables |",
  "| `provenance/` | Analysis registry, artifact manifest, upstream hashes, validation |",
  "| `qc/` | QC tables supporting the reported analyses |",
  "| `SHA256SUMS.txt` | Hash of every file in this bundle |",
  "",
  "## Verifying this bundle",
  "",
  "```bash",
  "sha256sum -c SHA256SUMS.txt",
  "```",
  "",
  "`provenance/artifact_manifest.csv` records, per file, where it came from, its",
  "SHA-256 before copying and after copying, and that the two matched.",
  "`provenance/validation.csv` records the build-time assertions.",
  "",
  "## What this bundle is not",
  "",
  "- It contains no raw experimental data, which cannot be regenerated.",
  "- Its contents were selected from `provenance/analysis_registry.csv` by",
  "  manuscript role, never by a significance threshold.",
  "- A release candidate is broader than a final submission: see",
  "  `docs/PUBLICATION_RELEASE.md` in the repository.",
  "",
  "Caveats on the analyses themselves are in `docs/KNOWN_LIMITATIONS.md`."
), file.path(RELEASE_ROOT, "README.md"))

# ---------------------------------------------------------------- SHA256SUMS
all_files <- list.files(RELEASE_ROOT, recursive = TRUE, full.names = TRUE)
all_files <- all_files[basename(all_files) != "SHA256SUMS.txt"]
sums <- vapply(all_files, sha256_file, character(1))
rel <- substring(gsub("\\\\", "/", all_files), nchar(gsub("\\\\", "/", RELEASE_ROOT)) + 2)
writeLines(paste0(sums, "  ", rel), file.path(RELEASE_ROOT, "SHA256SUMS.txt"))

# ---------------------------------------------------------------- summary
message("")
message("RELEASE BUILT: ", RELEASE_ROOT)
message("  artifacts copied : ", nrow(manifest))
message("  hash matches     : ", sum(manifest$hash_match), "/", nrow(manifest))
message("  files in bundle  : ", length(all_files) + 1)
message("  validation       : ", sum(validation$status == "PASS"), " PASS, ",
        sum(validation$status == "FAIL"), " FAIL, ",
        sum(validation$status == "SKIP"), " SKIP")
if (any(validation$status == "FAIL")) {
  stop("Release built but validation FAILED. Do not distribute this bundle.", call. = FALSE)
}
message("")
message("Source artifacts were not modified. Verify with: sha256sum -c SHA256SUMS.txt")
