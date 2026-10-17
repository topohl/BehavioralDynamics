# ================================================================
# Stage 29c - post hoc per-sex least-squares display line, CombZ on early movement (registry v1.1)
# MMMSociability -- Analysis/29c_posthoc_movement_combz_fit.R
# ================================================================
# POST HOC descriptive display (decision basis USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION, 2026-10-12). Implements exactly
# docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.1.md (registry v1.0 on the corrected outcome; FROZEN before any fit; read-only
# copy on S: at analysis_ready/canonical/posthoc_movement_combz_fit_registry/v1.1/). It adds no test and changes no registered
# result. v1.0 ran once as v1.0_fd61cd9 (ebb_v101).
#   2 fits (Female, Male) of observed_CombZ ~ Movement_mean on A2_prediction_animals of the pinned Stage 29 bundle; the fitted line on a
#   101-point grid over each sex's observed range with its pointwise 95% confidence band; coefficients without t or P.
#   All code is in Functions/posthoc_movement_combz_fit.R.
#
# Gates before any fit: code committed and clean and this is the committed runner; registry freeze commit is an ancestor
# and the registry file is unchanged since it; registry sha256 (S: copy, S: REGISTRY_SHA256.txt, repository copy);
# the pinned Stage 29 bundle (00_manifest, every file, FROZEN registry row); A2 sha256 (pinned = the manifest entry);
# population; no earlier REAL output or staging folder (fitted ONCE).
#
# Output: analysis_ready/pipeline/29c_posthoc_movement_combz_fit/v<PMF_VERSION>_<commit7>/ with tables/{fit_lines,coefficients,
#   diagnostics}.csv, audit/{output_manifest,input_hashes,run_manifest,gate_results}.csv and README.txt; files read-only.
# Mode: exactly `--real`, and the environment variable MMM_POSTHOC29C_REAL_RUN must equal the HEAD commit7. There is no
#   dry-run mode on project data (registry section 7); the synthetic tests are Testing/tests/test_posthoc_movement_combz_fit.R.
# Run from the MMMSociability repo root:  MMM_POSTHOC29C_REAL_RUN=<commit7> Rscript Analysis/29c_posthoc_movement_combz_fit.R --real
# ================================================================

MODE_ARGS <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({ library(data.table); library(digest) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
suppressPackageStartupMessages(source(.pipeline_setup))
source_mmm_helper("stage30_figure_bundle.R"); source_mmm_helper("posthoc_con_contrasts.R"); source_mmm_helper("posthoc_movement_combz_fit.R")
MODE <- pmf_parse_mode(MODE_ARGS)                          # refuses here unless exactly --real
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
  if (!isTRUE(passed)) stop("Stage 29c gate failed [", stage, "] ", name, ": ", paste(detail, collapse = "; "), call. = FALSE)
  invisible(passed)
}
gate_rows <- function(rows) for (i in seq_len(nrow(rows))) gate(rows$stage[i], rows$gate[i], rows$passed[i], rows$detail[i])
inp <- function(path, role, sha = s30fb_sha(path)) {
  INPUTS[[length(INPUTS) + 1L]] <<- data.table(input = normalizePath(path, winslash = "/", mustWork = FALSE), bytes = as.numeric(file.size(path)), sha256 = sha, role = role)
  invisible(sha) }
message("Stage 29c ", MODE, " (post hoc registry v", PMF_REGISTRY$version, ") at commit ", commit)

# ---------------------------------------------------------------- 0. identity: opt-in, code, registry
OPTIN <- Sys.getenv(PMF_OPTIN_ENV, "")
gate("0-identity", paste0("explicit REAL opt-in: ", PMF_OPTIN_ENV, " = HEAD commit7"), grepl("^[0-9a-f]{40}$", commit) && identical(OPTIN, substr(commit, 1, 7)), OPTIN)
CODE_FILES <- c("Analysis/29c_posthoc_movement_combz_fit.R", "Functions/posthoc_movement_combz_fit.R", "Functions/posthoc_con_contrasts.R",
                "Functions/stage30_figure_bundle.R", "Analysis/_pipeline_setup.R", "Functions/behavioral_dynamics_helpers.R",
                "Functions/project_paths.R", PMF_REGISTRY$repo_path)
cln <- s30fb_code_clean(git("status", "--porcelain", "--untracked-files=all", "--", CODE_FILES), git("ls-files", "--", CODE_FILES), CODE_FILES)
gate("0-identity", "MMM files used are committed and clean (git status --untracked-files=all / ls-files)", cln$ok, if (nzchar(cln$detail)) cln$detail else commit)
gate("0-identity", "this is the committed Analysis/29c_posthoc_movement_combz_fit.R",
     identical(s30fb_norm_path(DRIVER), s30fb_norm_path(file.path(MMM_REPO_ROOT, "Analysis", "29c_posthoc_movement_combz_fit.R"))), DRIVER)
CODE_SHA <- vapply(CODE_FILES, function(f) s30fb_sha(file.path(MMM_REPO_ROOT, f)), "")
gate("0-identity", "registry freeze commit is an ancestor of HEAD", git_ok("merge-base", "--is-ancestor", PMF_REGISTRY$freeze_commit, "HEAD"), PMF_REGISTRY$freeze_commit)
b_head <- git("rev-parse", paste0("HEAD:", PMF_REGISTRY$repo_path))[1]; b_frz <- git("rev-parse", paste0(PMF_REGISTRY$freeze_commit, ":", PMF_REGISTRY$repo_path))[1]
gate("0-identity", "registry file at HEAD = the file committed at the freeze (git blob)", grepl("^[0-9a-f]{40}$", b_head) && identical(b_head, b_frz), paste(b_head, b_frz))
ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
REG_DIR <- file.path(AR, PMF_REGISTRY$canonical_rel); REG_REPO <- file.path(MMM_REPO_ROOT, PMF_REGISTRY$repo_path)
gate_rows(phc_registry_gates(REG_DIR, REG_REPO, expect = PMF_REGISTRY))
inp(file.path(REG_DIR, PMF_REGISTRY$file), "registry (S:, read-only)"); inp(file.path(REG_DIR, PMF_REGISTRY$sha_file), "registry_sha256_list (S:)")
inp(REG_REPO, "registry repository copy (working tree bytes; LF-form sha256 gated)")

# ---------------------------------------------------------------- 1. output location (fitted ONCE)
OUT_ROOT <- file.path(AR, "pipeline", PMF_STAGE_DIR); RUN <- pmf_run_name(commit); RUN_DIR <- file.path(OUT_ROOT, RUN)
blk <- pmf_blocking_entries(OUT_ROOT)
gate("1-output", "no earlier REAL run or staging folder in the 29c stage folder (registry: fitted once)", !length(blk), paste(blk, collapse = ","))
longest <- max(nchar(file.path(OUT_ROOT, paste0(".tmp_", RUN), c(PMF_TABLE_FILES, PMF_AUDIT_FILES, PMF_MANIFEST_FILE, PMF_README))))
gate("1-output", "every output path shorter than 250 characters (Windows R path limit)", longest < 250, longest)

# ---------------------------------------------------------------- 2. inputs: the Stage 29 bundle and A2
B29_ROOT <- file.path(AR, "canonical", "behavior_bundle"); B29 <- file.path(B29_ROOT, PMF_STAGE29$bundle_id)
V29 <- s30fb_verify_stage29_bundle(B29, file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), PMF_STAGE29[c("bundle_id", "manifest_sha256")]); gate_rows(V29$gates)
a2r <- V29$check[file == PMF_STAGE29$a2_file]
gate("2-inputs", "A2_prediction_animals.csv: pinned SHA-256 = Stage 29 bundle manifest entry = on disk",
     nrow(a2r) == 1L && isTRUE(a2r$ok) && identical(a2r$expected_sha256, PMF_STAGE29$a2_sha256) && identical(a2r$observed_sha256, PMF_STAGE29$a2_sha256),
     paste(a2r$observed_sha256))
inp(file.path(B29, "00_manifest.csv"), "stage29_bundle_manifest", V29$manifest_sha256)
inp(file.path(B29, PMF_STAGE29$a2_file), "stage29_bundle_file A2 (data)", a2r$observed_sha256)
inp(file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), "stage29_bundle_registry (read for the FROZEN row)")
A2 <- fread(file.path(B29, PMF_STAGE29$a2_file), colClasses = list(character = "AnimalNum"))
gate_rows(pmf_population_gates(A2))

# ---------------------------------------------------------------- 3. fit (once; nothing is printed until written)
FIT_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
R <- pmf_run_all(A2)
gate_rows(pmf_output_gates(R, A2))

# ---------------------------------------------------------------- 4. audit and write
GT <- rbindlist(GATES); IN <- unique(rbindlist(INPUTS), by = c("input", "role"))
pk <- c("data.table", "digest")
RUNM <- data.table(
  mode = MODE, mode_optin = PMF_OPTIN_ENV, real_run_optin_value = OPTIN, tier = PMF_TIER, decision_basis = PMF_DECISION_BASIS,
  registry_version = PMF_REGISTRY$version, registry_sha256 = PMF_REGISTRY$sha256, registry_freeze_commit = PMF_REGISTRY$freeze_commit,
  registry_frozen_at = PMF_REGISTRY$frozen_at, registry_dir = normalizePath(REG_DIR, winslash = "/", mustWork = FALSE),
  git_commit = commit, git_branch = branch, driver = DRIVER, code_sha256 = paste(names(CODE_SHA), CODE_SHA, sep = "=", collapse = "; "),
  stage29_bundle_id = PMF_STAGE29$bundle_id, stage29_bundle_manifest_sha256 = V29$manifest_sha256, a2_sha256 = a2r$observed_sha256,
  out_dir = normalizePath(RUN_DIR, winslash = "/", mustWork = FALSE), n_fits = nrow(R$diagnostics),
  status_counts = paste(R$diagnostics[, paste0(Sex, " ", status)], collapse = "; "),
  model = "stats::lm(observed_CombZ ~ Movement_mean), one fit per sex", display_label = PMF_DISPLAY_LABEL,
  band = sprintf("pointwise %g%% confidence interval of the mean: fit +- qt(%g, n - 2) x se.fit; %d-point grid over the observed range",
                 100 * PMF_LEVEL, 1 - (1 - PMF_LEVEL) / 2, PMF_GRID_N),
  tests = "none (registry section 3): no t statistic and no P", note = PMF_NOTE,
  n_gates = nrow(GT), output_protection = "files read-only (Sys.chmod 0444 after the rename); directories not ACL-protected (Stage 29 release practice)",
  started_at = STARTED, fitted_at = FIT_AT, finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), r_version = R.version.string,
  packages = paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; "))
README <- c(
  paste0("Stage 29c - post hoc per-sex least-squares display line, CombZ on early movement (registry v", PMF_REGISTRY$version, ")"),
  paste("tier", PMF_TIER, "; decision basis", PMF_DECISION_BASIS, ";", PMF_NOTE),
  paste("commit", commit),
  paste("registry sha256", PMF_REGISTRY$sha256),
  paste("registry", normalizePath(file.path(REG_DIR, PMF_REGISTRY$file), winslash = "/", mustWork = FALSE)),
  paste("Stage 29 bundle", PMF_STAGE29$bundle_id, "manifest sha256", V29$manifest_sha256),
  paste("A2 sha256", a2r$observed_sha256),
  paste0("run mode REAL (fitted once; a later run is refused while any v", PMF_VERSION, "_* or .tmp_v", PMF_VERSION, "_* folder exists here)"),
  paste("generated_at", RUNM$finished_at),
  paste("model:", RUNM$model, "; band:", RUNM$band),
  "tables/fit_lines.csv: per sex, the fitted line and its 95% confidence band on the grid (display only)",
  "tables/coefficients.csv: intercept and slope with SE, residual df, residual SD and R squared; no t statistic and no P",
  "tables/diagnostics.csv: one row per fit (n, excluded, predictor range, rank, status)",
  paste("label for figures and legends:", PMF_DISPLAY_LABEL, "; not preregistered; adds no test"),
  "audit/output_manifest.csv lists every other file (bytes, sha256); every file is read-only (0444)")
W <- pmf_write_run(OUT_ROOT, RUN, list(fit_lines = R$fit_lines, coefficients = R$coefficients, diagnostics = R$diagnostics),
                   list(gates = GT, inputs = IN, run = RUNM), README)
for (i in seq_len(nrow(W$post_gates))) message(sprintf("  gate %-13s %-96s %s", W$post_gates$stage[i], W$post_gates$gate[i], if (W$post_gates$passed[i]) "PASS" else "FAIL"))
message("Stage 29c REAL complete: ", W$dir, " (output_manifest sha256 ", W$manifest_sha256, "); ", RUNM$status_counts)
