# ================================================================
# Stage 29b - post hoc CON / RES / SUS contrasts, first active phase after CC1 (registry v1.0)
# MMMSociability -- Analysis/29b_posthoc_con_contrasts.R
# ================================================================
# POST HOC exploratory (decision basis USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION, 2026-09-30). Implements exactly
# docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md (FROZEN before any fit; sha256 a6435d8d...; read-only copy on S: at
# analysis_ready/canonical/posthoc_con_contrasts_registry/v1.0/). It changes none of the frozen Stage 29 families or results.
#   4 fits (crossing_rate, shared_zone_use x Female, Male) of y ~ Batch + group + (0 + conCage | CageEpisodeID) +
#   (0 + sisCage | CageEpisodeID) on ebb_v101 A1_animal_cc1; batch-balanced CON / RES / SUS means (KR CI) and RES - CON,
#   SUS - CON, SUS - RES (KR t, Holm m = 3 per measure x sex); sensitivity with one common cage variance; diagnostics.
#   All model code is in Functions/posthoc_con_contrasts.R on the frozen engine Functions/rfid_canonical_inference.R.
#
# Gates before any fit: code committed and clean and this is the committed runner; registry freeze commit is an ancestor
# and the registry file is unchanged since it; registry sha256 (S: copy, S: REGISTRY_SHA256.txt, repository copy);
# engine package versions; ebb_v101 (00_manifest ec59aa33..., every file, FROZEN registry row); A1 sha256 (pinned = the
# manifest entry); population / cage structure; no earlier REAL output or staging folder (fitted ONCE).
#
# Output: analysis_ready/pipeline/29b_posthoc_con_contrasts/v1.0_<commit7>/ with tables/{estimates,
#   sensitivity_common_cage_variance,diagnostics}.csv, audit/{output_manifest,input_hashes,run_manifest,gate_results}.csv and
#   README.txt; files read-only (0444).
# Mode: exactly `--real`, and the environment variable MMM_POSTHOC29B_REAL_RUN must equal the HEAD commit7. There is no
#   dry-run mode on project data (registry section 7); the synthetic tests are Testing/tests/test_posthoc_con_contrasts.R.
# Run from the MMMSociability repo root:  MMM_POSTHOC29B_REAL_RUN=<commit7> Rscript Analysis/29b_posthoc_con_contrasts.R --real
# ================================================================

MODE_ARGS <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
suppressPackageStartupMessages(source(.pipeline_setup))
source_mmm_helper("stage30_figure_bundle.R"); source_mmm_helper("rfid_canonical_inference.R"); source_mmm_helper("posthoc_con_contrasts.R")
MODE <- phc_parse_mode(MODE_ARGS)                          # refuses here unless exactly --real
source_mmm_helper("project_paths.R")
STARTED <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE))
git_ok <- function(...) identical(suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = FALSE, stderr = FALSE)), 0L)
commit <- git("rev-parse", "HEAD")[1]; branch <- git("branch", "--show-current")[1]
DRIVER <- { fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(fa)) normalizePath(fa[1], winslash = "/", mustWork = FALSE) else NA_character_ }
GATES <- list(); INPUTS <- list()
gate <- function(stage, name, passed, detail = "") {
  GATES[[length(GATES) + 1L]] <<- s30fb_gate_row(stage, name, passed, detail, hard = TRUE)
  message(sprintf("  gate %-13s %-96s %s", stage, substr(name, 1, 96), if (isTRUE(passed)) "PASS" else "FAIL"))
  if (!isTRUE(passed)) stop("Stage 29b gate failed [", stage, "] ", name, ": ", paste(detail, collapse = "; "), call. = FALSE)
  invisible(passed)
}
gate_rows <- function(rows) for (i in seq_len(nrow(rows))) gate(rows$stage[i], rows$gate[i], rows$passed[i], rows$detail[i])
inp <- function(path, role, sha = s30fb_sha(path)) {
  INPUTS[[length(INPUTS) + 1L]] <<- data.table(input = normalizePath(path, winslash = "/", mustWork = FALSE), bytes = as.numeric(file.size(path)), sha256 = sha, role = role)
  invisible(sha) }
message("Stage 29b ", MODE, " (post hoc registry v", PHC_REGISTRY$version, ") at commit ", commit)

# ---------------------------------------------------------------- 0. identity: opt-in, code, registry, packages
OPTIN <- Sys.getenv(PHC_OPTIN_ENV, "")
gate("0-identity", paste0("explicit REAL opt-in: ", PHC_OPTIN_ENV, " = HEAD commit7"), grepl("^[0-9a-f]{40}$", commit) && identical(OPTIN, substr(commit, 1, 7)), OPTIN)
CODE_FILES <- c("Analysis/29b_posthoc_con_contrasts.R", "Functions/posthoc_con_contrasts.R", "Functions/rfid_canonical_inference.R",
                "Functions/stage30_figure_bundle.R", "Analysis/_pipeline_setup.R", "Functions/behavioral_dynamics_helpers.R",
                "Functions/project_paths.R", PHC_REGISTRY$repo_path)
cln <- s30fb_code_clean(git("status", "--porcelain", "--untracked-files=all", "--", CODE_FILES), git("ls-files", "--", CODE_FILES), CODE_FILES)
gate("0-identity", "MMM files used are committed and clean (git status --untracked-files=all / ls-files)", cln$ok, if (nzchar(cln$detail)) cln$detail else commit)
gate("0-identity", "this is the committed Analysis/29b_posthoc_con_contrasts.R",
     identical(s30fb_norm_path(DRIVER), s30fb_norm_path(file.path(MMM_REPO_ROOT, "Analysis", "29b_posthoc_con_contrasts.R"))), DRIVER)
CODE_SHA <- vapply(CODE_FILES, function(f) s30fb_sha(file.path(MMM_REPO_ROOT, f)), "")
gate("0-identity", "registry freeze commit is an ancestor of HEAD", git_ok("merge-base", "--is-ancestor", PHC_REGISTRY$freeze_commit, "HEAD"), PHC_REGISTRY$freeze_commit)
b_head <- git("rev-parse", paste0("HEAD:", PHC_REGISTRY$repo_path))[1]; b_frz <- git("rev-parse", paste0(PHC_REGISTRY$freeze_commit, ":", PHC_REGISTRY$repo_path))[1]
gate("0-identity", "registry file at HEAD = the file committed at the freeze (git blob)", grepl("^[0-9a-f]{40}$", b_head) && identical(b_head, b_frz), paste(b_head, b_frz))
ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
REG_DIR <- file.path(AR, PHC_REGISTRY$canonical_rel); REG_REPO <- file.path(MMM_REPO_ROOT, PHC_REGISTRY$repo_path)
gate_rows(phc_registry_gates(REG_DIR, REG_REPO))
inp(file.path(REG_DIR, PHC_REGISTRY$file), "registry (S:, read-only)"); inp(file.path(REG_DIR, PHC_REGISTRY$sha_file), "registry_sha256_list (S:)")
inp(REG_REPO, "registry repository copy (working tree bytes; LF-form sha256 gated)")
gate_rows(phc_package_gates())

# ---------------------------------------------------------------- 1. output location (fitted ONCE)
OUT_ROOT <- file.path(AR, "pipeline", PHC_STAGE_DIR); RUN <- phc_run_name(commit); RUN_DIR <- file.path(OUT_ROOT, RUN)
blk <- phc_blocking_entries(OUT_ROOT)
gate("1-output", "no earlier REAL run or staging folder in the 29b stage folder (registry: fitted once)", !length(blk), paste(blk, collapse = ","))
longest <- max(nchar(file.path(OUT_ROOT, paste0(".tmp_", RUN), c(PHC_TABLE_FILES, PHC_AUDIT_FILES, PHC_MANIFEST_FILE, PHC_README))))
gate("1-output", "every output path shorter than 250 characters (Windows R path limit)", longest < 250, longest)

# ---------------------------------------------------------------- 2. inputs: ebb_v101 and A1
B29_ROOT <- file.path(AR, "canonical", "behavior_bundle"); B29 <- file.path(B29_ROOT, PHC_STAGE29$bundle_id)
V29 <- s30fb_verify_stage29_bundle(B29, file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), PHC_STAGE29[c("bundle_id", "manifest_sha256")]); gate_rows(V29$gates)
a1r <- V29$check[file == PHC_STAGE29$a1_file]
gate("2-inputs", "A1_animal_cc1.csv: pinned SHA-256 = ebb_v101 manifest entry = on disk",
     nrow(a1r) == 1L && isTRUE(a1r$ok) && identical(a1r$expected_sha256, PHC_STAGE29$a1_sha256) && identical(a1r$observed_sha256, PHC_STAGE29$a1_sha256),
     paste(a1r$observed_sha256))
cfr <- V29$check[file == PHC_STAGE29$config_file]
gate("2-inputs", "I_analysis_config.json listed in the ebb_v101 manifest and verified (display labels only)", nrow(cfr) == 1L && isTRUE(cfr$ok), cfr$observed_sha256)
inp(file.path(B29, "00_manifest.csv"), "stage29_bundle_manifest", V29$manifest_sha256)
inp(file.path(B29, PHC_STAGE29$a1_file), "stage29_bundle_file A1 (data)", a1r$observed_sha256)
inp(file.path(B29, PHC_STAGE29$config_file), "stage29_bundle_file config (measure label / unit)", cfr$observed_sha256)
inp(file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), "stage29_bundle_registry (read for the FROZEN row)")

A1 <- fread(file.path(B29, PHC_STAGE29$a1_file), colClasses = list(character = c("AnimalNum", "CageEpisodeID")))
DES <- phc_prepare(A1)
gate_rows(phc_population_gates(DES))
CFG29 <- jsonlite::fromJSON(file.path(B29, PHC_STAGE29$config_file), simplifyVector = FALSE)
LAB <- phc_metric_labels(CFG29$metrics)
gate("2-inputs", "measure labels copied from the ebb_v101 config (label, unit)", nrow(LAB) == length(PHC_MEASURES) && !anyNA(LAB$measure_label), paste(LAB$measure_label, collapse = "; "))

# ---------------------------------------------------------------- 3. fit (once; nothing is printed until written)
FIT_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
R <- phc_run_all(DES, LAB)
gate("3-fit", "4 primary + 4 sensitivity fits; 24 + 24 estimand rows", nrow(R$diagnostics) == 8L && nrow(R$estimates) == 24L && nrow(R$sensitivity) == 24L,
     paste(nrow(R$diagnostics), nrow(R$estimates), nrow(R$sensitivity)))

# ---------------------------------------------------------------- 4. audit and write
GT <- rbindlist(GATES); IN <- unique(rbindlist(INPUTS), by = c("input", "role"))
pk <- c("lme4", "lmerTest", "pbkrtest", "Matrix", "reformulas", "data.table", "digest", "jsonlite")
st <- R$diagnostics[, .N, by = .(model, status)]
agree <- R$sensitivity[tested == TRUE, .(n = .N, same_sign = sum(same_sign %in% TRUE), ci_zero_agreement = sum(ci_zero_agreement %in% TRUE))]
RUNM <- data.table(
  mode = MODE, mode_optin = PHC_OPTIN_ENV, real_run_optin_value = OPTIN, tier = PHC_TIER, decision_basis = PHC_DECISION_BASIS,
  registry_version = PHC_REGISTRY$version, registry_sha256 = PHC_REGISTRY$sha256, registry_freeze_commit = PHC_REGISTRY$freeze_commit,
  registry_frozen_at = PHC_REGISTRY$frozen_at, registry_dir = normalizePath(REG_DIR, winslash = "/", mustWork = FALSE),
  git_commit = commit, git_branch = branch, driver = DRIVER, code_sha256 = paste(names(CODE_SHA), CODE_SHA, sep = "=", collapse = "; "),
  stage29_bundle_id = PHC_STAGE29$bundle_id, stage29_bundle_manifest_sha256 = V29$manifest_sha256, a1_sha256 = a1r$observed_sha256,
  out_dir = normalizePath(RUN_DIR, winslash = "/", mustWork = FALSE), n_fits = nrow(R$diagnostics),
  status_counts = paste(st[, paste0(model, " ", status, "=", N)], collapse = "; "),
  n_singular = sum(R$diagnostics$singular %in% TRUE), n_failed = sum(R$diagnostics$status == "FAILED"),
  sensitivity_agreement = sprintf("contrasts: same sign %d of %d; CI-excludes-zero agreement %d of %d", agree$same_sign, agree$n, agree$ci_zero_agreement, agree$n),
  holm = "within each measure x sex over RES - CON, SUS - CON, SUS - RES (m = 3); sensitivity Holm computed the same way; no global correction",
  failure_rule = paste("error / engine FAILED / non-converged -> FAILED (no estimate); KR -> Satterthwaite fallback -> FAILED row; singular kept and flagged"),
  n_gates = nrow(GT), output_protection = "files read-only (Sys.chmod 0444 after the rename); directories not ACL-protected (Stage 29 release practice)",
  started_at = STARTED, fitted_at = FIT_AT, finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), r_version = R.version.string,
  packages = paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; "))
README <- c(
  "Stage 29b - post hoc CON / RES / SUS contrasts, first active phase after CC1 (registry v1.0)",
  paste("tier", PHC_TIER, "; decision basis", PHC_DECISION_BASIS, "; not part of the frozen Stage 29 families; changes none of its results"),
  paste("commit", commit),
  paste("registry sha256", PHC_REGISTRY$sha256),
  paste("registry", normalizePath(file.path(REG_DIR, PHC_REGISTRY$file), winslash = "/", mustWork = FALSE)),
  paste("Stage 29 bundle", PHC_STAGE29$bundle_id, "manifest sha256", V29$manifest_sha256),
  paste("A1 sha256", a1r$observed_sha256),
  "run mode REAL (fitted once; a later run is refused while any v1.0_* or .tmp_v1.0_* folder exists here)",
  paste("generated_at", RUNM$finished_at),
  paste("models:", PHC_MODELS$primary$formula, "(primary);", PHC_MODELS$sensitivity$formula, "(sensitivity); REML, bobyqa, KR via lmerTest::contest"),
  "tables/estimates.csv: batch-balanced CON / RES / SUS means (KR CI, estimation only) and RES - CON, SUS - CON, SUS - RES (KR t, Holm m = 3 per measure x sex)",
  "tables/sensitivity_common_cage_variance.csv: the same estimands with (1 | CageEpisodeID), with agreement columns (reported, not used for display)",
  "tables/diagnostics.csv: one row per fit (n per group, CON / SIS cage counts, variance components, KR df, singularity, convergence)",
  paste("caveats:", PHC_CAVEATS),
  paste("note:", PHC_REGISTERED_NOTE),
  "audit/output_manifest.csv lists every other file (bytes, sha256); every file is read-only (0444)")
W <- phc_write_run(OUT_ROOT, RUN, list(estimates = R$estimates, sensitivity = R$sensitivity, diagnostics = R$diagnostics),
                   list(gates = GT, inputs = IN, run = RUNM), README)
for (i in seq_len(nrow(W$post_gates))) message(sprintf("  gate %-13s %-96s %s", W$post_gates$stage[i], W$post_gates$gate[i], if (W$post_gates$passed[i]) "PASS" else "FAIL"))
message("Stage 29b REAL complete: ", W$dir, " (output_manifest sha256 ", W$manifest_sha256, "); ", RUNM$status_counts, "; singular ", RUNM$n_singular)
