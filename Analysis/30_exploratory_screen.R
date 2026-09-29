# ================================================================
# Stage 30 - Exploratory outcome screen (Exp9 SIS RFID)
# MMMSociability -- Analysis/30_exploratory_screen.R
# ================================================================
# Runs the FROZEN Stage 30 registry v1.0 (analysis_ready/canonical/stage30_registry/v1.0/stage30_registry_v1.0.json,
# sha256 5c252bf6...; identical copy docs/stage30/) exactly once: 48 DISCOVERY hypotheses (F, M, INT; local BH per family,
# global BH over the 48 as a descriptive column), 16 POOL ESTIMATION_ONLY entries (no p), every declared sensitivity,
# leave-one-batch-out refits, animal influence, classification (D, A, B, C) and wording flags, plus the Stage 29
# carry-over rows. Tier: exploratory, decision basis POST_HOC_CONTEXT; never primary, confirmatory or preregistered.
#
#   0. mode opt-in and identity gates: registry SHA-256 (S: and repo copy), every FREEZE_RECORD.csv input hash (config: its
#      analytic constants only, so config v1.0.1 documentation edits do not block), frozen code hashes, clean committed
#      code (REAL), no earlier REAL output or staging folder at any commit (REAL)
#   1. data gates: data version v2 manifest + 24 files, raw_data seed files, cookie_postEPM_v1 manifest + files, Stage 29
#      tables, the X4 inactivity reference table; the Stage 29 v1.0.1 release folder (path, ancestry, manifest, read-only,
#      run_manifest) and the R / package versions of that release; the CombZ table header (first line only); the Stage 29
#      carry-over rows and their count gate (all before any label or outcome contact)
#   2. measures through Functions/stage30_movement.R (s30mv_*): CC1-CC4 canonical window metrics must equal the Stage 29 v2
#      reference table (tolerance 1e-9); >= 40 s / >= 60 s positional-inactivity fractions from the canonical occupancy
#      runs must equal x4_01 (888 windows); the cookie windows are recomputed from the cookie data version
#   3. populations (con list only) against the registry counts
#   4. DESIGN GATE before any outcome column is joined: every entry's declared rank; every CR2 entry's design-fixed
#      Satterthwaite df = expected_df_cr2 within 1e-6 (placeholder outcomes; df_Satt depends only on X and the clusters)
#      4b. staging folder + audit/RUN_STARTED.txt written BEFORE any label / outcome contact; never deleted by the driver
#   5. labels (sus/con lists), their registered counts and the label pre-flight (CombZ table label columns, never CombZ)
#   6. outcome (CombZ only; CombZ_wb)   7. the screen (Functions/stage30_screen.R)   9. outputs, audit, output manifest
#
# RUN MODES (exactly one must be selected explicitly; with neither variable set the driver refuses to run)
#   REAL (the single registered run): MMM_STAGE30_REAL_RUN = the HEAD commit7 (explicit opt-in) AND
#     MMM_STAGE30_S29_RELEASE_DIR = the Stage 29 v1.0.1 release folder. Refuses unless the registry and every FREEZE_RECORD
#     hash match, the driver and its code are committed and clean, analysis_ready/pipeline/30_exploratory_screen/ holds no
#     v1.0_* output or .tmp_v1.0_* staging folder (any commit), and every gate passes; writes only to
#     analysis_ready/pipeline/30_exploratory_screen/v1.0_<commit7>/ (staged in .tmp_v1.0_<commit7>/, created before the
#     label lists are read and never deleted by the driver: an aborted REAL run leaves a documented blocking folder that
#     needs a user decision; renamed at the end, then made read-only).
#   DRY (code testing only): MMM_STAGE30_DRY_RUN_DIR = a new folder on C: under <evidence root>/evidence and outside the
#     repo. The real CombZ table is never parsed (hashed; its first line is read for the column names); CombZ is a seeded
#     synthetic N(0,1)-scale variable with batch and CC1-cage structure; the real RES/SUS labels are read only to check the
#     registered counts and to be permuted within Batch x Sex (fixed seed); only the key columns of the Stage 29 tables are
#     read (carry-over values NA); every table is stamped DRY_RUN_NOT_RESULTS. Every gate except the clean-code gate is
#     enforced (release-folder gates: whenever MMM_STAGE30_S29_RELEASE_DIR is given).
# Environment:
#   MMM_STAGE30_REAL_RUN               REAL opt-in: must equal substr(HEAD, 1, 7)
#   MMM_STAGE30_DRY_RUN_DIR            DRY mode + output folder (must not exist)
#   MMM_STAGE30_S29_RELEASE_DIR        Stage 29 v1.0.1 release folder (REAL: required); DRY default: evidence/cage_fix_rerun/out
#   MMM_STAGE30_SCREEN_HELPER          DRY only: explicit path to a (proposed) Functions/stage30_screen.R
#   MMM_STAGE30_EVIDENCE_ROOT          root of the relative evidence/ paths in FREEZE_RECORD.csv
#   MMM_STAGE30_ACK_PACKAGE_MISMATCH   "1" = the user explicitly accepts R / package versions differing from the release
#   MMM_STAGE30_DRY_SEED               DRY seed (default 20260929); MMM_STAGE30_DRY_SUBSET DRY only, comma-separated ids
# Run from the MMMSociability repo root: Rscript Analysis/30_exploratory_screen.R
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
T_START <- Sys.time()
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
suppressPackageStartupMessages(source(.pipeline_setup))
git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE))
git_ok <- function(...) identical(suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = FALSE, stderr = FALSE)), 0L)
commit <- git("rev-parse", "HEAD")[1]

# ---------------------------------------------------------------- run mode: explicit opt-in (G2-01; nothing is read before this)
DRY_DIR <- Sys.getenv("MMM_STAGE30_DRY_RUN_DIR"); REAL_OPTIN <- Sys.getenv("MMM_STAGE30_REAL_RUN")
if (nzchar(DRY_DIR) && nzchar(REAL_OPTIN)) stop("Both MMM_STAGE30_DRY_RUN_DIR and MMM_STAGE30_REAL_RUN are set: set exactly one.", call. = FALSE)
if (!nzchar(DRY_DIR) && !nzchar(REAL_OPTIN))
  stop("Refusing to run: no mode selected. DRY (code testing): MMM_STAGE30_DRY_RUN_DIR=<new C: folder under the evidence root>. ",
       "REAL (the single registered run): MMM_STAGE30_REAL_RUN=<HEAD commit7> and MMM_STAGE30_S29_RELEASE_DIR=<Stage 29 v1.0.1 release folder>.", call. = FALSE)
DRY <- nzchar(DRY_DIR); MODE <- if (DRY) "DRY" else "REAL"
if (!DRY && !(grepl("^[0-9a-f]{40}$", commit) && identical(REAL_OPTIN, substr(commit, 1, 7))))
  stop("REAL opt-in refused: MMM_STAGE30_REAL_RUN must equal the 7-character HEAD commit of this repo.", call. = FALSE)
S29_ENV <- Sys.getenv("MMM_STAGE30_S29_RELEASE_DIR")
if (!DRY && !nzchar(S29_ENV))
  stop("REAL requires MMM_STAGE30_S29_RELEASE_DIR = the Stage 29 v1.0.1 release folder (analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_<commit7>).", call. = FALSE)

for (h in c("project_paths.R", "behavior_analysis_config.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "rfid_canonical_inference.R",
            "stage30_movement.R")) source_mmm_helper(h)
for (p in c("lme4", "lmerTest", "pbkrtest", "clubSandwich", "Matrix", "reformulas", "R.utils"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Missing package ", p, call. = FALSE)
helper_override <- Sys.getenv("MMM_STAGE30_SCREEN_HELPER")
if (nzchar(helper_override)) {
  if (!DRY) stop("MMM_STAGE30_SCREEN_HELPER is for DRY runs only; a REAL run uses the committed Functions/stage30_screen.R.", call. = FALSE)
  HELPER <- normalizePath(helper_override, winslash = "/", mustWork = TRUE); source(HELPER)
} else HELPER <- source_mmm_helper("stage30_screen.R")
RUN_MODE <- if (DRY) S30SC_RUN_MODE_DRY else S30SC_RUN_MODE_REAL
DRY_SEED <- as.integer(Sys.getenv("MMM_STAGE30_DRY_SEED", "20260929"))
SUBSET <- Sys.getenv("MMM_STAGE30_DRY_SUBSET")
if (nzchar(SUBSET) && !DRY) stop("MMM_STAGE30_DRY_SUBSET is for DRY development runs only.", call. = FALSE)
ACK_PKG <- identical(Sys.getenv("MMM_STAGE30_ACK_PACKAGE_MISMATCH"), "1")
DRIVER <- { fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(fa)) normalizePath(fa[1], winslash = "/", mustWork = FALSE) else NA_character_ }
sha_or_na <- function(p) if (!is.na(p) && file.exists(p)) s30sc_sha(p) else NA_character_

# ---------------------------------------------------------------- paths
RFID_ROOT <- mmm_project_root(); AR <- file.path(RFID_ROOT, "analysis_ready")
REG_DIR <- file.path(AR, "canonical", "stage30_registry", "v1.0")
REG_JSON <- file.path(REG_DIR, "stage30_registry_v1.0.json"); REG_MD <- file.path(REG_DIR, "STAGE30_REGISTRY_v1.0.md")
REG_REPO <- file.path(MMM_REPO_ROOT, "docs", "stage30", "stage30_registry_v1.0.json")
FREEZE_FILE <- file.path(REG_DIR, "FREEZE_RECORD.csv")
EVIDENCE_ROOT <- Sys.getenv("MMM_STAGE30_EVIDENCE_ROOT", "C:/Users/topohl/Documents/e9_behavior_rationalization_audit_2026-09-27/stage30_sleep_cookie")
S29_IS_ARG <- nzchar(S29_ENV)
S29_DIR <- sub("/+$", "", normalizePath(if (S29_IS_ARG) S29_ENV else file.path(EVIDENCE_ROOT, "evidence", "cage_fix_rerun", "out"), winslash = "/", mustWork = FALSE))
S29_TABLES <- file.path(S29_DIR, "tables")
DV2_DIR <- file.path(RFID_ROOT, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28")
PRE_DIR <- file.path(DV2_DIR, "preprocessed_data"); RAW <- file.path(RFID_ROOT, "MMMSociability", "raw_data")
CK_DIR <- file.path(RFID_ROOT, "MMMSociability", "data_versions", "cookie_postEPM_v1")
X401 <- file.path(EVIDENCE_ROOT, "evidence", "wfD", "X4_critic", "x4_01_inactivity_animal_window.csv")
COMBZ_EXPECTED <- file.path(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv")
OUT_ROOT <- file.path(AR, "pipeline", "30_exploratory_screen")

GATES <- list(); TIMES <- list(); INPUTS <- list()
tick <- function(block) TIMES[[length(TIMES) + 1L]] <<- data.table(block = block, at = Sys.time())
gate <- function(stage, name, passed, detail = "", hard = TRUE) {
  GATES[[length(GATES) + 1L]] <<- s30sc_gate(name, passed, detail, stage)
  message(sprintf("  gate %-12s %-72s %s", stage, substr(name, 1, 72), if (isTRUE(passed)) "PASS" else if (hard) "FAIL" else "NOT MET (recorded)"))
  if (hard && !isTRUE(passed)) { message("FAILED gate detail: ", detail)
    stop("Stage 30 gate failed [", stage, "] ", name, ": ", detail, call. = FALSE) }
  invisible(passed)
}
inp <- function(path, role, sha = s30sc_sha(path)) { INPUTS[[length(INPUTS) + 1L]] <<- data.table(input = path, bytes = file.size(path), sha256 = sha, role = role); invisible(sha) }
message("Stage 30 ", MODE, " run at commit ", commit, if (DRY) " -- DRY_RUN_NOT_RESULTS (synthetic CombZ, permuted labels)" else "")
tick("start")

# ---------------------------------------------------------------- 0. identity gates
gate("0-identity", "explicit run-mode opt-in", TRUE, if (DRY) "MMM_STAGE30_DRY_RUN_DIR set" else paste("MMM_STAGE30_REAL_RUN =", REAL_OPTIN, "= HEAD commit7"))
RG <- s30sc_read_registry(REG_JSON); reg <- RG$reg
gate("0-identity", "registry JSON (S:) SHA-256 = frozen v1.0", RG$sha == S30SC_REGISTRY_SHA256, RG$sha); inp(REG_JSON, "registry", RG$sha)
gate("0-identity", "registry JSON repo copy (docs/stage30) identical", file.exists(REG_REPO) && s30sc_sha(REG_REPO) == S30SC_REGISTRY_SHA256, REG_REPO)
gate("0-identity", "registry companion .md SHA-256", s30sc_sha(REG_MD) == S30SC_REGISTRY_MD_SHA256, REG_MD); inp(REG_MD, "registry_md")
gate("0-identity", "registry status FROZEN, version 1.0", identical(reg$meta$status, "FROZEN") && identical(reg$meta$version, S30SC_REGISTRY_VERSION))
fr <- fread(FREEZE_FILE, colClasses = "character"); inp(FREEZE_FILE, "freeze_record")
resolve_fr <- function(p) if (grepl("^[A-Za-z]:/", p)) p else if (startsWith(p, "evidence/")) file.path(EVIDENCE_ROOT, p) else file.path(REG_DIR, p)
for (i in seq_len(nrow(fr))) {
  pth <- resolve_fr(fr$path[i]); ok_file <- file.exists(pth); sha <- if (ok_file) s30sc_sha(pth) else NA_character_
  if (ok_file) inp(pth, paste0("freeze_record:", fr$item[i]), sha)
  if (fr$item[i] == "config_at_freeze") {
    gate("0-identity", "FREEZE_RECORD config_at_freeze: file hash recorded (gated on analytic constants instead)", ok_file,
         paste("sha now", sha, if (identical(sha, fr$sha256[i])) "(= freeze)" else "(differs from freeze: expected after config v1.0.1)"), hard = FALSE)
  } else gate("0-identity", paste("FREEZE_RECORD", fr$item[i], "SHA-256"), ok_file && identical(sha, fr$sha256[i]), pth)
}
gate("0-identity", "FREEZE_RECORD lists every bound input", all(c("sus_list", "con_list", "combz_table", "v2_manifest", "cookie_manifest_staged",
     "cc1_cc4_reference_table", "movement_wrapper", "inference_engine", "event_stream", "binfree_metrics", "config_at_freeze") %in% fr$item))
COMBZ <- resolve_fr(fr[item == "combz_table", path])
gate("0-identity", "project root resolves to the frozen CombZ path", identical(s30sc_norm_path(COMBZ), s30sc_norm_path(COMBZ_EXPECTED)), RFID_ROOT)
for (f in names(S30SC_CODE_SHA256)) gate("0-identity", paste("frozen code", f), s30sc_sha(file.path(MMM_REPO_ROOT, f)) == S30SC_CODE_SHA256[[f]])
CFG <- MMM_BEHAVIOR_CONFIG
gate("0-identity", "config analytic constant bout_criterion_s = 39.3970988275363", identical(CFG$metrics$fragmentation$bout_criterion_s, S30SC_ANALYTIC_CONSTANTS$bout_criterion_s))
gate("0-identity", "config analytic constants alpha = 0.05, ci_level = 0.95", identical(CFG$meta$alpha, S30SC_ANALYTIC_CONSTANTS$alpha) && identical(CFG$meta$ci_level, S30SC_ANALYTIC_CONSTANTS$ci_level))
CRIT <- S30SC_ANALYTIC_CONSTANTS$bout_criterion_s
SPEC <- s30sc_spec(reg)
gate("0-identity", "registry specification: 48 discovery + 16 POOL, families, focal terms, engines, measures", TRUE, paste(names(attr(SPEC, "checks")), collapse = ","))
CODE_FILES <- c("Analysis/30_exploratory_screen.R", "Functions/stage30_screen.R", "Functions/stage30_movement.R", "Functions/rfid_canonical_inference.R",
                "Functions/rfid_event_stream.R", "Functions/rfid_binfree_metrics.R", "Functions/behavioral_dynamics_helpers.R",
                "Functions/behavior_analysis_config.R", "Functions/project_paths.R", "Analysis/_pipeline_setup.R", "docs/stage30/stage30_registry_v1.0.json")
dirty <- git("status", "--porcelain", "--untracked-files=all", "--", CODE_FILES)
tracked <- git("ls-files", "--", CODE_FILES); untracked <- setdiff(CODE_FILES, tracked)
gate("0-identity", "driver code committed and clean (git status / ls-files)", !length(dirty) && !length(untracked) && grepl("^[0-9a-f]{40}$", commit),
     paste(c(dirty, paste("untracked:", untracked)), collapse = "; "), hard = !DRY)
if (!DRY) {
  gate("0-identity", "REAL run executes the committed Analysis/30_exploratory_screen.R",
       identical(s30sc_norm_path(DRIVER), s30sc_norm_path(file.path(MMM_REPO_ROOT, "Analysis", "30_exploratory_screen.R"))), DRIVER)
  gate("0-identity", "REAL run uses the committed Functions/stage30_screen.R",
       identical(s30sc_norm_path(HELPER), s30sc_norm_path(file.path(MMM_REPO_ROOT, "Functions", "stage30_screen.R"))), HELPER)
  NAME <- paste0("v1.0_", substr(commit, 1, 7))
  OUT <- file.path(OUT_ROOT, NAME); WORK <- file.path(OUT_ROOT, paste0(".tmp_", NAME))
  conf <- s30sc_real_out_conflicts(OUT_ROOT)
  gate("0-identity", "REAL output root holds no v1.0_* output or .tmp_v1.0_* staging folder (any commit; run once)", !length(conf), paste(c(OUT_ROOT, conf), collapse = ": "))
  gate("0-identity", "REAL output folder does not exist (immutable)", !dir.exists(OUT), OUT)
  gate("0-identity", "no partial REAL staging folder", !dir.exists(WORK), WORK)
} else {
  OUT <- normalizePath(DRY_DIR, winslash = "/", mustWork = FALSE)
  WORK <- file.path(dirname(OUT), paste0(".tmp_", basename(OUT)))
  gate("0-identity", "DRY output on C:, outside analysis_ready and S:", grepl("^c:/", tolower(OUT)) && !s30sc_inside(OUT, AR), OUT)
  gate("0-identity", "DRY output under <evidence root>/evidence and outside the git worktree", s30sc_inside(OUT, file.path(EVIDENCE_ROOT, "evidence")) &&
         !s30sc_inside(OUT, MMM_REPO_ROOT), paste(OUT, "| evidence:", file.path(EVIDENCE_ROOT, "evidence"), "| repo:", MMM_REPO_ROOT))
  gate("0-identity", "DRY output folder and staging folder do not exist", !dir.exists(OUT) && !dir.exists(WORK), OUT)
}
tick("0 identity gates")

# ---------------------------------------------------------------- 1. data gates (before any row is read)
man2 <- file.path(DV2_DIR, "MANIFEST_SHA256.csv")
gate("1-data", "data version v2 manifest SHA-256", s30sc_sha(man2) == S30SC_V2_MANIFEST_SHA256, man2); inp(man2, "data_version_manifest")
mv2 <- fread(man2, colClasses = "character")
pre_found <- list.files(PRE_DIR, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$")
gate("1-data", "data version v2: 24 files on disk = manifest", nrow(mv2) == S30SC_V2_N_FILES && setequal(pre_found, mv2$file))
PRE_FILES <- file.path(PRE_DIR, mv2$file)
pre_sha <- vapply(PRE_FILES, s30sc_sha, "", USE.NAMES = FALSE)
gate("1-data", "data version v2: every file hash = manifest sha256_v2", all(pre_sha == mv2$sha256_v2), paste(mv2$file[pre_sha != mv2$sha256_v2], collapse = ","))
for (i in seq_along(PRE_FILES)) inp(PRE_FILES[i], "preprocessed_v2", pre_sha[i])
raw_paths <- file.path(RAW, names(S30SC_RAW_SEED_SHA256)); raw_sha <- vapply(raw_paths, function(p) if (file.exists(p)) s30sc_sha(p) else NA_character_, "", USE.NAMES = FALSE)
gate("1-data", "raw_data seed sources: 24 files = Stage 29 run_inputs hashes", all(raw_sha == S30SC_RAW_SEED_SHA256, na.rm = FALSE) && !anyNA(raw_sha))
for (i in seq_along(raw_paths)) inp(raw_paths[i], "raw_seed_source", raw_sha[i])
ckman <- file.path(CK_DIR, "MANIFEST_SHA256.csv")
gate("1-data", "cookie_postEPM_v1 MANIFEST SHA-256", s30sc_sha(ckman) == S30SC_COOKIE_MANIFEST_SHA256, ckman); inp(ckman, "cookie_manifest")
ckm <- fread(ckman, colClasses = c(bytes = "numeric"))
ck_fl <- setdiff(list.files(CK_DIR, recursive = TRUE, all.files = TRUE), "MANIFEST_SHA256.csv")
gate("1-data", "cookie_postEPM_v1: file set = manifest (21 files)", setequal(ck_fl, ckm$file) && nrow(ckm) == 21L)
ck_sha <- vapply(file.path(CK_DIR, ckm$file), s30sc_sha, "", USE.NAMES = FALSE)
gate("1-data", "cookie_postEPM_v1: every file hash and size = manifest", all(ck_sha == ckm$sha256) && all(file.size(file.path(CK_DIR, ckm$file)) == ckm$bytes))
for (f in names(S30SC_COOKIE_FILE_SHA256)) gate("1-data", paste("cookie", f, "SHA-256 = registry"), s30sc_sha(file.path(CK_DIR, f)) == S30SC_COOKIE_FILE_SHA256[[f]])
for (i in seq_len(nrow(ckm))) inp(file.path(CK_DIR, ckm$file[i]), "cookie_data_version", ck_sha[i])
for (f in names(S30SC_S29_TABLE_SHA256)) { p <- file.path(S29_TABLES, f)
  gate("1-data", paste("Stage 29 v1.0.1 table", f, "SHA-256"), file.exists(p) && s30sc_sha(p) == S30SC_S29_TABLE_SHA256[[f]], p); inp(p, "stage29_release_table") }
gate("1-data", "X4 inactivity reference table SHA-256", s30sc_sha(X401) == S30SC_X401_SHA256, X401); inp(X401, "inactivity_reference")
# CombZ table header only (first line; no value is read): the label pre-flight and the outcome read will find their columns
cz_head <- gsub('^"|"$', "", strsplit(readLines(COMBZ, n = 1L, warn = FALSE), ",", fixed = TRUE)[[1]])
gate("1-data", "CombZ table header (first line only) has the label and outcome columns", all(c(S30SC_COMBZ_LABEL_COLS, S30SC_COMBZ_OUTCOME_COLS) %in% cz_head),
     paste(length(cz_head), "columns"))
# Stage 29 v1.0.1 release folder (G2-05): required in REAL; in DRY enforced whenever the folder is passed explicitly
REL_HARD <- !DRY || S29_IS_ARG
REL_C7 <- s30sc_release_commit(S29_DIR)
gate("1-release", "Stage 29 folder = .../29_canonical_behavior_releases/v101_dv2_<commit7> (registry stage29_release)", !is.na(REL_C7), S29_DIR, hard = REL_HARD)
gate("1-release", "Stage 29 release commit is an ancestor of HEAD (git merge-base --is-ancestor)", !is.na(REL_C7) && git_ok("merge-base", "--is-ancestor", REL_C7, "HEAD"),
     paste(REL_C7, "->", commit), hard = REL_HARD)
REL_MAN <- file.path(S29_DIR, "audit", "output_manifest.csv"); REL_RUN <- file.path(S29_DIR, "audit", "run_manifest.csv")
rm_chk <- if (file.exists(REL_MAN)) s30sc_release_manifest_check(fread(REL_MAN, colClasses = "character")) else list(passed = FALSE, detail = paste("missing", REL_MAN))
gate("1-release", "release audit/output_manifest.csv lists the 5 carried tables with the gated SHA-256", rm_chk$passed, rm_chk$detail, hard = REL_HARD)
ro <- vapply(file.path(S29_TABLES, names(S30SC_S29_TABLE_SHA256)), function(p) file.exists(p) && file.access(p, 2L) != 0L, TRUE)
gate("1-release", "the 5 carried release tables are read-only", all(ro), paste(names(S30SC_S29_TABLE_SHA256)[!ro], collapse = ","), hard = REL_HARD)
RM <- if (file.exists(REL_RUN)) fread(REL_RUN, colClasses = "character") else NULL
gate("1-release", "release run_manifest: RELEASE mode, git_commit = folder commit7, data manifest = v2 (bb33a111)",
     !is.null(RM) && identical(RM$run_mode[1], "RELEASE") && !is.na(REL_C7) && startsWith(RM$git_commit[1], REL_C7) &&
       identical(RM$data_manifest_sha256[1], S30SC_V2_MANIFEST_SHA256), REL_RUN, hard = REL_HARD)
if (file.exists(REL_MAN)) inp(REL_MAN, "stage29_release_output_manifest")
if (file.exists(REL_RUN)) inp(REL_RUN, "stage29_release_run_manifest")
# R / package versions (G2-10): the KR / CR2 numerics must run on the versions of the Stage 29 v1.0.1 release (and the E4 calibration)
PKG <- s30sc_package_gate(if (is.null(RM)) NA_character_ else RM$packages[1], if (is.null(RM)) NA_character_ else RM$r_version[1])
gate("1-release", "R and numerics package versions = Stage 29 release run_manifest (R, lme4, lmerTest, pbkrtest, clubSandwich, Matrix, reformulas)",
     all(PKG$passed), paste(PKG[, sprintf("%s %s|%s", item, expected, observed)], collapse = "; "), hard = !ACK_PKG)
if (ACK_PKG && !all(PKG$passed)) message("NOTE: package / R version mismatch explicitly acknowledged (MMM_STAGE30_ACK_PACKAGE_MISMATCH=1); recorded in run_manifest.")
# Stage 29 carry-over rows (never re-tested) and their count gate, before any label / outcome contact (G2-03)
CARRY <- s30sc_carry_over(S29_TABLES, mask_values = DRY)
CARRY[, source_dir := S29_TABLES]
gate("1-carry", "carry-over rows: CONT 2 metrics x 2 populations x 4 estimands; CAT 5 metrics x 4 estimands", CARRY[question == "CONT", .N] == 16L && CARRY[question == "CAT", .N] == 20L &&
       CARRY[question == "CAT", uniqueN(construct)] == 5L && all(CARRY$status == "CARRY_OVER"))
tick("1 data, release and carry-over gates")

# ---------------------------------------------------------------- 2. measures (canonical Stage 29 code path via s30mv_*)
pre <- s30mv_read_preprocessed(PRE_FILES)
gate("2-measures", "no Group column in the preprocessed data", !"Group" %in% names(pre))
st <- s30mv_build_stream(pre, RAW)
seed_files <- unique(st$seeds$raw_file)
gate("2-measures", "seed step read only hash-gated raw files", all(s30sc_norm_path(seed_files) %in% s30sc_norm_path(raw_paths)), paste(length(seed_files), "raw files"))
win <- mmm_evs_windows(pre)
WIN <- rbind(win[, .(SourceFile, window_id = "active", start = active_start, end = active_end)], win[, .(SourceFile, window_id = "light", start = light_start, end = light_end)])
mt <- s30mv_window_metrics(st, WIN, bout_criterion_s = CRIT, full = TRUE)
wstreams <- s30mv_window_streams(st, WIN)
act <- mt[window_id == "active"]; lgt <- mt[window_id == "light"]
gate("2-measures", "444 active and 444 light animal-windows, all status ok", nrow(act) == 444L && nrow(lgt) == 444L && all(act$status == "ok") && all(lgt$status == "ok"),
     paste(nrow(act), nrow(lgt), paste(unique(c(act$status, lgt$status)), collapse = ",")))
rec <- act[, .(AnimalNum, CC, SourceFile, System, obs_s, n_positions_occupied, occupancy_dispersion, dominant_share, n_bouts, n_events, fragmentation,
               events_per_bout, crossing_rate, bout_rate_h, shared_zone_use, n_tracked_mates, dyadic_obs_s, hardware_flag)]
rec <- merge(rec, lgt[, .(AnimalNum, CC, light_phase_crossing_rate = crossing_rate, light_obs_s = obs_s, light_complete = file_covers_end)], by = c("AnimalNum", "CC"))
rec[light_complete == FALSE, light_phase_crossing_rate := NA_real_]
rec[, Batch := sub("^.*(B[0-9]).*$", "\\1", SourceFile)]
rec[, CageEpisodeID := paste(Batch, System, CC, sep = "|")]
rec[, n_in_cage := .N, by = CageEpisodeID]
s30sc_assert_blind(S30SC_REF_COLS, "reference-table select list")
ref <- fread(file.path(S29_TABLES, "canonical_window_metrics.csv"), select = S30SC_REF_COLS, colClasses = list(character = "AnimalNum"))
s30sc_assert_blind(names(ref), "reference table")
ref[, AnimalNum := canonical_animal_id(AnimalNum)]
cmp_ref <- s30sc_compare(rec, ref, c("AnimalNum", "CC"), S30SC_REF_COMPARE_COLS)
gate("2-measures", "recomputed CC1-CC4 window metrics = Stage 29 v2 reference table (1e-9; 444 rows; all measure columns)",
     all(cmp_ref$passed) && nrow(ref) == 444L && all(cmp_ref$n == 444L), paste(cmp_ref[passed == FALSE, column], collapse = ","))
IA <- rbindlist(lapply(c("active", "light"), function(w) s30sc_inactivity(wstreams[[w]]$runs)[, window := w]))
chkT <- merge(IA, rbind(rec[, .(window = "active", AnimalNum, CC, obs = obs_s)], rec[, .(window = "light", AnimalNum, CC, obs = light_obs_s)]), by = c("window", "AnimalNum", "CC"))
gate("2-measures", "inactivity denominator = obs_s for all 888 windows", nrow(chkT) == 888L && max(abs(chkT$T_obs - chkT$obs)) <= S30SC_TOL)
x4 <- fread(X401, select = c("window", "AnimalNum", "CC", "X", "frac"), colClasses = list(character = "AnimalNum")); x4[, AnimalNum := canonical_animal_id(AnimalNum)]
IAl <- melt(IA, id.vars = c("window", "AnimalNum", "CC"), measure.vars = paste0("frac", S30SC_INACTIVITY_X), variable.name = "X", value.name = "frac_rec")
IAl[, X := as.numeric(sub("frac", "", X))]
cx <- merge(IAl, x4, by = c("window", "AnimalNum", "CC", "X"), all = TRUE)
gate("2-measures", ">= 40 s / >= 60 s inactivity fractions = x4_01 for the 888 windows (1e-9)", nrow(cx) == 1776L && !anyNA(cx$frac_rec) && !anyNA(cx$frac) &&
       max(abs(cx$frac_rec - cx$frac)) <= S30SC_TOL, sprintf("rows %d max|diff| %.3g", nrow(cx), max(abs(cx$frac_rec - cx$frac), na.rm = TRUE)))
IAw <- dcast(IA, AnimalNum + CC ~ window, value.var = c("frac40", "frac60"))
setnames(IAw, c("frac40_active", "frac40_light", "frac60_active", "frac60_light"), c("posinact40_active", "posinact40_light", "posinact60_active", "posinact60_light"))
W <- merge(rec, IAw, by = c("AnimalNum", "CC"))
sex <- unique(ref[, .(AnimalNum, Sex)])
gate("2-measures", "one Sex per animal (reference table metadata)", !anyDuplicated(sex$AnimalNum) && all(sex$Sex %in% c("Female", "Male")))
W <- merge(W, sex, by = "AnimalNum")
W[, `:=`(AnimalID = AnimalNum, sex_c = ifelse(Sex == "Female", 0.5, -0.5), cc = as.integer(sub("^CC", "", CC)))]
W[, `:=`(c2 = as.numeric(cc == 2), c3 = as.numeric(cc == 3), c4 = as.numeric(cc == 4), cc1 = cc - 1)]
# cookie: recomputation from the data version's own preprocessed files (unseeded, strict), then its tables
pm <- fread(file.path(CK_DIR, "preprocessed", "preprocessed_files_sha256.csv"))
st_ck <- s30mv_build_stream(s30mv_read_preprocessed(file.path(CK_DIR, pm$file)), raw_dir = NULL)
cw <- fread(file.path(CK_DIR, "cookie_windows.csv"), colClasses = c(AnimalNum = "character"))
s30sc_assert_blind(names(cw), "cookie_windows.csv")
Wck <- unique(cw[, .(SourceFile, window_id, start = as.POSIXct(window_start_utc, tz = "UTC"), end = as.POSIXct(window_end_utc, tz = "UTC"))])
m_ck <- s30mv_window_metrics(st_ck, Wck, strict_seed = TRUE)
cm <- merge(cw[, .(Batch, AnimalNum, window_id, n_events, obs_s, crossing_rate)], m_ck[, .(Batch, AnimalNum, window_id, n_events, obs_s, crossing_rate)],
            by = c("Batch", "AnimalNum", "window_id"), suffixes = c(".v", ".r"))
gate("2-measures", "cookie windows recomputed from cookie_postEPM_v1 = cookie_windows.csv (388; 1e-9)", nrow(cm) == 388L && nrow(cw) == 388L &&
       all(cm$n_events.v == cm$n_events.r) && max(abs(cm$obs_s.v - cm$obs_s.r)) <= S30SC_TOL && max(abs(cm$crossing_rate.v - cm$crossing_rate.r)) <= S30SC_TOL)
cr <- fread(file.path(CK_DIR, "cookie_response.csv"), colClasses = c(AnimalNum = "character", AnimalID = "character"))
s30sc_assert_blind(names(cr), "cookie_response.csv")
wd <- dcast(m_ck, Batch + AnimalNum ~ window_id, value.var = "crossing_rate")
wd[, `:=`(dcookie60 = POST60 - PRE60, dcookie45 = POST45 - PRE45)]
cc_ <- s30sc_compare(wd, cr, c("Batch", "AnimalNum"), c("PRE60", "POST60", "dcookie60", "PRE45", "POST45", "dcookie45"))
gate("2-measures", "cookie_response.csv = recomputed PRE/POST/delta (97 animals; 1e-9)", all(cc_$passed) && nrow(cr) == 97L && all(cr$all_windows_valid))
s30sc_assert_blind(S30SC_COOKIE_ANIMAL_COLS, "cookie animals select list")
an_ck <- fread(file.path(CK_DIR, "animals.csv"), select = S30SC_COOKIE_ANIMAL_COLS, colClasses = list(character = c("AnimalID", "AnimalNum")))
s30sc_assert_blind(names(an_ck), "cookie animals.csv")
K <- merge(cr, an_ck[, .(AnimalNum, Batch, Sex, cage_ck = CageEpisodeID, cage_size, or646_protocol_deviation, board_override_applied,
                         board_System_equals_v2_CC4, pop_COOKIE_SIS, pop_COOKIE_SIS_F, pop_COOKIE_SIS_M)], by = c("AnimalNum", "Batch"))
gate("2-measures", "cookie response and animals tables join 97/97 with identical cages", nrow(K) == 97L && all(K$CageEpisodeID == K$cage_ck))
K[, `:=`(cage_ck = NULL, AnimalNum = canonical_animal_id(AnimalNum))]
K[, `:=`(AnimalID = AnimalNum, sex_c = ifelse(Sex == "Female", 0.5, -0.5))]
K <- merge(K, W[CC == "CC1", .(AnimalNum, cc1_crossing_rate = crossing_rate, Sex_w = Sex)], by = "AnimalNum", all.x = TRUE)
tick("2 measures and recomputation gates")

# ---------------------------------------------------------------- 3. populations (con list only)
CON_FILE <- resolve_fr(fr[fr$item == "con_list", path]); SUS_FILE <- resolve_fr(fr[fr$item == "sus_list", path])
gate("3-population", "label list paths = registry data.labels", identical(s30sc_norm_path(CON_FILE), s30sc_norm_path(reg$data$labels$con)) &&
       identical(s30sc_norm_path(SUS_FILE), s30sc_norm_path(reg$data$labels$sus)))
con <- canonical_animal_id(readLines(CON_FILE, warn = FALSE)); con <- unique(con[!is.na(con) & nzchar(con)])
W[, SIS := !(AnimalNum %in% con)]; K[, SIS := !(AnimalNum %in% con)]
gate("3-population", "cookie SIS (con list) = cookie_postEPM_v1 pop_COOKIE_SIS flags (97/97)", all(K$SIS == K$pop_COOKIE_SIS) &&
       all((K$SIS & K$Sex == "Female") == K$pop_COOKIE_SIS_F) && all((K$SIS & K$Sex == "Male") == K$pop_COOKIE_SIS_M))
gate("3-population", "cookie SIS animals have a finite Stage 29 CC1 active rate and the same Sex (77/77)",
     K[SIS == TRUE, all(is.finite(cc1_crossing_rate)) && all(Sex == Sex_w)] && K[SIS == TRUE, .N] == 77L)
K[, Sex_w := NULL]
gate("3-population", "OR646 included in B6|sys.5 with the protocol-deviation flag", K[AnimalNum == "OR646", .N == 1L && CageEpisodeID == "B6|sys.5" && isTRUE(or646_protocol_deviation) && SIS])
gate("3-population", "cookie cages do not mix SIS and CON", K[, uniqueN(SIS), by = CageEpisodeID][, all(V1 == 1L)])
D <- list(W = W, K = K)
pops <- reg$populations
POPCHK <- rbindlist(lapply(names(pops), function(p) { x <- pops[[p]]; if (!is.list(x) || p %in% c("cookie_all_eligible")) return(NULL)
  base <- sub("_(F|M)$", "", p); if (!base %in% c("SIS_CC1", "SIS_CC1_CC4", "COOKIE_SIS")) return(NULL)
  d <- s30sc_pop_data(D, p); o <- list()
  add <- function(k, exp, obs) o[[length(o) + 1L]] <<- data.table(population = p, field = k, expected = as.character(exp), observed = as.character(obs), passed = identical(as.character(exp), as.character(obs)))
  if (!is.null(x$n_animals)) add("n_animals", x$n_animals, uniqueN(d$AnimalNum))
  if (!is.null(x$n_windows)) add("n_windows", x$n_windows, nrow(d))
  if (!is.null(x$shared_position_windows_v2)) add("shared_position_windows", x$shared_position_windows_v2, d[is.finite(shared_zone_use), .N])
  if (!is.null(x$n_windows_shared_position)) add("shared_position_windows", x$n_windows_shared_position, d[is.finite(shared_zone_use), .N])
  if (!is.null(x$cage_episodes)) add("cage_episodes", x$cage_episodes, uniqueN(d$CageEpisodeID))
  if (!is.null(x$cage_episodes_shared_position)) add("cage_episodes_shared_position", x$cage_episodes_shared_position, d[is.finite(shared_zone_use), uniqueN(CageEpisodeID)])
  if (!is.null(x$cc1_cage_clusters)) add("cc1_cage_clusters", x$cc1_cage_clusters, uniqueN(d$CageEpisodeID))
  if (!is.null(x$cage_clusters)) add("cage_clusters", x$cage_clusters, uniqueN(d$CageEpisodeID))
  if (!is.null(x$batches)) { b <- s30sc_parse_batches(x$batches); add("n_batches", b$n, uniqueN(d$Batch))
    if (length(b$batches)) add("batches", paste(sort(b$batches), collapse = ","), paste(sort(unique(as.character(d$Batch))), collapse = ",")) }
  if (!is.null(x$cage_sizes)) { e <- s30sc_parse_cage_sizes(x$cage_sizes); ob <- s30sc_cage_size_table(d$CageEpisodeID)
    add("cage_sizes", paste(names(e), e, sep = ":", collapse = ","), paste(names(ob), ob, sep = ":", collapse = ",")) }
  if (!is.null(x$females)) add("females", sub(" .*$", "", x$females), d[Sex == "Female", uniqueN(AnimalNum)])
  if (!is.null(x$males)) add("males", sub(" .*$", "", x$males), d[Sex == "Male", uniqueN(AnimalNum)])
  rbindlist(o) }), fill = TRUE)
ca <- pops$cookie_all_eligible
POPCHK <- rbind(POPCHK, data.table(population = "cookie_all_eligible", field = c("n_animals", "cages"), expected = as.character(c(ca$n_animals, ca$cages)),
                                   observed = as.character(c(nrow(K), uniqueN(K$CageEpisodeID))), passed = c(ca$n_animals == nrow(K), ca$cages == uniqueN(K$CageEpisodeID))))
gate("3-population", "every registry population count / cage / batch / window field reproduced", all(POPCHK$passed), paste(POPCHK[passed == FALSE, paste(population, field)], collapse = "; "))
tick("3 populations")

# ---------------------------------------------------------------- 4. design gate (placeholder outcomes; before any outcome column is joined)
ph <- s30sc_placeholder_outcomes(D, seed = 20260929L)
DG <- s30sc_design_gate(s30sc_attach_outcomes(D, ph), SPEC)
gate("4-design", "design gate: all 64 entries have their declared rank", all(DG$rank_ok), paste(DG[rank_ok == FALSE, id], collapse = ","))
gate("4-design", "design gate: every CR2 entry (18 discovery + 6 POOL) has df_Satt = expected_df_cr2 (1e-6)", DG[engine == "CR2", .N == SPEC[engine == "CR2", .N] && .N == 24L && all(df_ok)],
     sprintf("max |diff| %.3g; %s", DG[engine == "CR2", max(df_abs_diff)], paste(DG[df_ok == FALSE, id], collapse = ",")))
rm(ph)
tick("4 design gate")

# ---------------------------------------------------------------- 4b. staging folder + RUN_STARTED marker, BEFORE any label / outcome contact (G2-02)
if (!DRY) { conf <- s30sc_real_out_conflicts(OUT_ROOT)
  gate("4-staging", "REAL output root still holds no v1.0_* / .tmp_v1.0_* entry", !length(conf), paste(c(OUT_ROOT, conf), collapse = ": ")) }
gate("4-staging", "output and staging folders still do not exist", !dir.exists(OUT) && !dir.exists(WORK), WORK)
dir.create(file.path(WORK, "tables"), recursive = TRUE, showWarnings = FALSE); dir.create(file.path(WORK, "audit"), recursive = TRUE, showWarnings = FALSE)
TW <- file.path(WORK, "tables"); AW <- file.path(WORK, "audit")
writeLines(c(paste0(MODE, "_RUN_STARTED"), paste("run_mode", RUN_MODE), paste("started_at", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
             paste("git_commit", commit), paste("registry_sha256", RG$sha), paste("driver", DRIVER, sha_or_na(DRIVER)), paste("helper", HELPER, sha_or_na(HELPER)),
             paste("stage29_dir", S29_DIR), paste("mode_optin", if (DRY) "MMM_STAGE30_DRY_RUN_DIR" else paste("MMM_STAGE30_REAL_RUN", REAL_OPTIN)),
             "Gates 0-4 (identity, data, release, carry-over, measures, populations, design) passed.",
             if (DRY) "DRY: permuted labels and a synthetic CombZ follow; no real label-measure relation is formed or written." else
               c("Outcome contact follows (sus/con lists, CombZ table). The driver never deletes this staging folder: if the run aborts,",
                 "this folder stays and blocks every later REAL run (any commit) until the user decides.")),
           file.path(AW, "RUN_STARTED.txt"))
gate("4-staging", "staging folder and audit/RUN_STARTED.txt written before any label / outcome contact", file.exists(file.path(AW, "RUN_STARTED.txt")), WORK)
tick("4b staging folder")

# ---------------------------------------------------------------- 5. labels (sus/con lists) and the label pre-flight
sus <- canonical_animal_id(readLines(SUS_FILE, warn = FALSE)); sus <- unique(sus[!is.na(sus) & nzchar(sus)])
AN <- unique(W[SIS == TRUE, .(AnimalNum, Batch, Sex)])
cage1 <- W[SIS == TRUE & CC == "CC1", .(AnimalNum, cage_CC1 = CageEpisodeID)]
AN <- merge(AN, cage1, by = "AnimalNum")
gate("5-labels", "87 SIS animals, one row each, CC1 cage known", nrow(AN) == 87L && !anyDuplicated(AN$AnimalNum))
AN <- s30sc_labels_from_lists(AN, sus, con)
gate("5-labels", "no SIS animal is on the con list", !any(AN$Group == "CON"))
LC <- s30sc_label_count_checks(AN, K[SIS == TRUE, .(AnimalNum, Batch, Sex)], pops)
for (i in seq_len(nrow(LC))) gate("5-labels", LC$gate[i], LC$passed[i], LC$detail[i])
AN_ALL <- s30sc_labels_from_lists(unique(W[, .(AnimalNum, Batch, Sex)]), sus, con)
gate("5-labels", "111 RFID animals (SIS + CON), one Batch / Sex each", nrow(AN_ALL) == 111L && !anyDuplicated(AN_ALL$AnimalNum), paste(nrow(AN_ALL), "animals"))
if (DRY) {
  LAB <- AN_ALL[, .(AnimalNum, Sex, Batch = as.integer(sub("^B", "", Batch)), outcome_group = Group)]
  LAB_SRC <- "DRY: synthetic label table from the lists (code-path exercise; the CombZ table is never parsed in DRY)"
} else {
  LAB <- fread(COMBZ, select = S30SC_COMBZ_LABEL_COLS, colClasses = c(AnimalNum = "character"))   # never CombZ
  LAB[, AnimalNum := canonical_animal_id(AnimalNum)]
  LAB_SRC <- "REAL: CombZ table label columns AnimalNum, Sex, Batch, outcome_group (CombZ not selected)"
}
LT <- s30sc_label_table_checks(AN_ALL[, .(AnimalNum, Batch, Sex, Group)], LAB)
for (i in seq_len(nrow(LT))) gate("5-labels", paste0(LT$gate[i], if (DRY) " [DRY synthetic table]" else ""), LT$passed[i], paste(LT$detail[i], "|", LAB_SRC))
rm(LAB, AN_ALL)
if (DRY) {
  gl <- s30sc_permute_labels(AN[, .(AnimalNum, Batch, Sex, Group)], seed = DRY_SEED)
  AN <- merge(AN[, !"Group"], gl[, .(AnimalNum, g_RS)], by = "AnimalNum")
  message("DRY: RES/SUS labels permuted within Batch x Sex (seed ", DRY_SEED, "); the real labels are not used further.")
} else { AN[, g_RS := ifelse(Group == "RES", 0.5, -0.5)]; AN[, Group := NULL] }
rm(sus, LC)
tick("5 labels")

# ---------------------------------------------------------------- 6. outcome (CombZ, CombZ_wb)
if (DRY) {
  AN <- merge(AN, s30sc_synthetic_combz(AN, seed = DRY_SEED + 1L), by = "AnimalNum")
  message("DRY: CombZ replaced by a seeded synthetic variable (batch shift + CC1-cage ICC 0.30); the real CombZ table is hashed only.")
} else {
  cz <- fread(COMBZ, select = S30SC_COMBZ_OUTCOME_COLS, colClasses = c(AnimalNum = "character"))
  cz[, AnimalNum := canonical_animal_id(AnimalNum)]
  RO <- s30sc_real_outcome(AN, cz)
  for (i in seq_len(nrow(RO$checks))) gate("6-outcome", RO$checks$gate[i], RO$checks$passed[i], RO$checks$detail[i])
  AN <- RO$an; rm(cz, RO)
}
AN <- s30sc_combz_wb(AN)
gate("6-outcome", "CombZ_wb: batch means 0 over the 87 animals (one value per animal)", AN[, max(abs(mean(CombZ_wb))), by = Batch][, max(V1)] < 1e-12 && nrow(AN) == 87L)
D <- s30sc_attach_outcomes(D, AN[, .(AnimalNum, CombZ, CombZ_wb, g_RS)])
tick("6 outcome")

# ---------------------------------------------------------------- 7. the screen
RUN_SPEC <- if (nzchar(SUBSET)) SPEC[id %in% trimws(strsplit(SUBSET, ",")[[1]])] else SPEC
if (nzchar(SUBSET)) message("DRY development subset: ", nrow(RUN_SPEC), " entries")
SCR <- s30sc_run_screen(D, RUN_SPEC, design_df = DG[, .(id, design_df_cr2)], force_nested = DRY)
tick("7 screen")

# ---------------------------------------------------------------- 9. outputs (into the staging folder created in 4b)
wr <- function(x, name) s30sc_write(x, TW, name, RUN_MODE)
wr(SCR$master, "master_hypothesis_table.csv"); wr(SCR$pool, "pool_estimates.csv"); wr(SCR$components, "l_components.csv")
wr(SCR$sensitivities, "sensitivities.csv"); wr(SCR$lobo, "lobo.csv"); wr(SCR$influence, "influence.csv"); wr(SCR$nested, "nested_lobo_prediction.csv")
wr(CARRY, "carry_over_stage29.csv")
FW <- D$W[SIS == TRUE, .(AnimalNum, Batch, Sex, CC, SourceFile, System, CageEpisodeID, obs_s, crossing_rate, light_phase_crossing_rate, occupancy_dispersion,
                          fragmentation, shared_zone_use, n_tracked_mates, posinact40_active, posinact40_light, posinact60_active, posinact60_light)][order(Batch, AnimalNum, CC)]
FK <- D$K[SIS == TRUE, .(AnimalNum, Batch, Sex, CageEpisodeID, PRE60, POST60, dcookie60, PRE45, POST45, dcookie45, cc1_crossing_rate, or646_protocol_deviation)][order(Batch, AnimalNum)]
wr(FW, "features_cc1_cc4_windows.csv"); wr(FK, "features_cookie.csv")
s30sc_write_rds(FW, TW, "features_cc1_cc4_windows_full_precision.rds", RUN_MODE); s30sc_write_rds(FK, TW, "features_cookie_full_precision.rds", RUN_MODE)
s30sc_write(rbindlist(GATES), AW, "gate_results.csv", RUN_MODE)
s30sc_write(DG, AW, "design_gate.csv", RUN_MODE)
s30sc_write(POPCHK, AW, "population_gate.csv", RUN_MODE)
s30sc_write(cmp_ref, AW, "reference_table_gate.csv", RUN_MODE)
s30sc_write(PKG, AW, "package_version_gate.csv", RUN_MODE)
s30sc_write(rbindlist(INPUTS), AW, "input_hashes.csv", RUN_MODE)
s30sc_write(SCR$fits, AW, "model_registry.csv", RUN_MODE)
s30sc_write(SCR$failures, AW, "run_failures.csv", RUN_MODE)
s30sc_write(data.table(convention = S30SC_DRIVER_CONVENTIONS), AW, "driver_conventions.csv", RUN_MODE)
TT <- rbindlist(TIMES); TT[, seconds_since_previous := c(0, diff(as.numeric(at)))]
s30sc_write(TT[, .(block, at = format(at, "%Y-%m-%dT%H:%M:%S%z"), seconds_since_previous)], AW, "timings.csv", RUN_MODE)
s30sc_write(data.table(id = SCR$master$id, seconds = SCR$master$seconds), AW, "timings_by_hypothesis.csv", RUN_MODE)
writeLines(capture.output(sessionInfo()), file.path(AW, "session_info.txt"))
pk <- c("lme4", "lmerTest", "pbkrtest", "clubSandwich", "data.table", "Matrix", "reformulas", "digest", "jsonlite")
PROTECTION <- "files read-only (Sys.chmod 0444 after the rename); directories not ACL-protected (Stage 29 release practice)"
RUN <- data.table(run_mode = RUN_MODE, mode = MODE, mode_optin = if (DRY) "MMM_STAGE30_DRY_RUN_DIR" else "MMM_STAGE30_REAL_RUN",
                  real_run_optin_value = if (DRY) NA_character_ else REAL_OPTIN, registry_version = paste0("v", S30SC_REGISTRY_VERSION),
                  registry_sha256 = RG$sha, git_commit = commit,
                  driver = DRIVER, driver_sha256 = sha_or_na(DRIVER), helper = HELPER, helper_sha256 = sha_or_na(HELPER),
                  stage29_dir = S29_DIR, stage29_tables_dir = S29_TABLES, stage29_dir_from_argument = S29_IS_ARG, stage29_release_commit7 = REL_C7,
                  stage29_release_gates_enforced = REL_HARD, package_gate_passed = all(PKG$passed), package_mismatch_acknowledged = ACK_PKG,
                  evidence_root = EVIDENCE_ROOT, out_dir = OUT, dry_seed = if (DRY) DRY_SEED else NA_integer_, dry_subset = SUBSET,
                  n_discovery_run = SCR$master[, .N], n_pool_run = SCR$pool[, .N],
                  status_counts = paste(SCR$master[, .N, by = status][, paste0(status, "=", N)], collapse = "; "),
                  classification_counts = if (DRY) "not summarised (DRY)" else paste(SCR$master[, .N, by = classification][, paste0(classification, "=", N)], collapse = "; "),
                  n_block_failures = nrow(SCR$failures), n_gates = length(GATES), gates_not_met = paste(rbindlist(GATES)[passed == FALSE, gate], collapse = "; "),
                  features_used_in_fits = "in-memory recomputed group-blind features (full double precision); exact copies: tables/features_*_full_precision.rds",
                  driver_conventions = paste(S30SC_DRIVER_CONVENTIONS, collapse = " || "), output_protection = PROTECTION,
                  started_at = format(T_START, "%Y-%m-%dT%H:%M:%S%z"), finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                  elapsed_min = as.numeric(difftime(Sys.time(), T_START, units = "mins")), r_version = R.version.string,
                  packages = paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; "))
s30sc_write(RUN, AW, "run_manifest.csv", RUN_MODE)
DOC <- c("", paste("commit", commit), paste("registry sha256", RG$sha), paste("Stage 29 folder", S29_DIR),
         "", "Fits use the in-memory recomputed group-blind features at full double precision; features_*.csv carry 15 significant digits",
         "(KR p of refits on the CSV values can differ by ~1e-8); features_*_full_precision.rds hold the exact values.",
         "", "Driver conventions where the registry is silent (conservative; also in audit/driver_conventions.csv):", paste(" ", S30SC_DRIVER_CONVENTIONS),
         "", paste("Output protection:", PROTECTION))
writeLines(c(if (DRY) c("DRY_RUN_NOT_RESULTS", "", "This folder is a DRY run of Analysis/30_exploratory_screen.R: CombZ is synthetic, RES/SUS labels are permuted within",
                        "Batch x Sex, Stage 29 carry-over values are not read (NA). No number here is a Stage 30 result.") else
               c("Stage 30 exploratory outcome screen (registry v1.0, FROZEN). Tier: exploratory; decision basis POST_HOC_CONTEXT.",
                 "Never primary, confirmatory or preregistered. Classification uses the local BH families only; the global BH is descriptive."),
             DOC), file.path(WORK, if (DRY) "DRY_RUN_NOT_RESULTS.txt" else "README.txt"))
outs <- setdiff(list.files(WORK, recursive = TRUE, full.names = TRUE), file.path(AW, "output_manifest.csv"))
fwrite(data.table(run_mode = RUN_MODE, file = substring(outs, nchar(WORK) + 2L), bytes = file.size(outs), sha256 = vapply(outs, s30sc_sha, "", USE.NAMES = FALSE)),
       file.path(AW, "output_manifest.csv"))
if (dir.exists(OUT)) stop("Output folder appeared during the run (immutable): ", OUT, call. = FALSE)
if (!file.rename(WORK, OUT)) stop("Could not rename ", WORK, " to ", OUT, call. = FALSE)
Sys.chmod(list.files(OUT, recursive = TRUE, full.names = TRUE), mode = "0444")
if (nrow(SCR$failures)) warning(nrow(SCR$failures), " entry-level failure(s) recorded in audit/run_failures.csv; their rows are FAILED.")
message("Stage 30 ", MODE, " complete (", sprintf("%.1f", as.numeric(difftime(Sys.time(), T_START, units = "mins"))), " min): ", OUT)
