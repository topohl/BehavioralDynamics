# ================================================================
# Stage 30b - Stage 30 figure bundle (canonical source-data export for manuscript figures)
# MMMSociability -- Analysis/30b_stage30_figure_bundle.R
# ================================================================
# The ONLY writer of the Stage 30 figure bundle that Exp9_manuscript imports (record: docs/STAGE30_FIGURE_BUNDLE_v1.md).
# It fits, tests and refits nothing and produces no p or q value. It copies frozen values from the Stage 30 REAL run
# pipeline/30_exploratory_screen/v1.0_be71e2f and adds only descriptive summaries (counts, medians, quartiles, centroid
# means) and the unit rescaling of stored CIs (Functions/stage30_figure_bundle.R). Every input is hash-gated first:
#   * the frozen Stage 30 run: audit/output_manifest.csv (pinned SHA-256) and its 27 files, README commit / registry lines;
#   * the Stage 30 registry v1.0 JSON (5c252bf6...);
#   * the Stage 29 bundle ebb_v101_20260929_b2ce507 (00_manifest ec59aa33..., every file, FROZEN registry row);
#   * sus / con lists and the CombZ table (hash = Stage 30 audit/input_hashes.csv = pinned);
#   * the x4_01 inactivity reference (a533b239..., = input_hashes) and w1_04_gate_values.csv (ae80b7e1..., = w1_99 list).
# Group rule (the Stage 30 driver's): CON if on the con list, else SUS if on the sus list, else RES; it must equal the
# v101 A1 / A4 Group of every SIS animal.
#
# Output (REAL): analysis_ready/canonical/stage30_figure_bundle/<bundle_id>/ with BUNDLE_REGISTRY.csv beside it;
#   bundle_id = s30b_v10_<YYYYMMDD>_<commit7>; written once (refused if it exists), files 0444.
# Modes (exactly one argument; with none the runner refuses before reading anything):
#   --dry-run  code testing: writes <dry root>/<YYYYMMDD_HHMMSS>/<bundle_id>/ + BUNDLE_REGISTRY.csv + gate_results.csv
#              (status DRY_RUN_NOT_FOR_USE); never writes to S:. Dry root: MMM_S30B_DRY_ROOT or the default below
#              (must be on C:, outside analysis_ready and outside this repo).
#   --real     writes the S: bundle once; refused unless the MMM files it uses are committed and clean and this is the
#              committed runner.
# Run from the MMMSociability repo root:  Rscript Analysis/30b_stage30_figure_bundle.R --dry-run
# ================================================================

MODE_ARGS <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
suppressPackageStartupMessages(source(.pipeline_setup))
source_mmm_helper("stage30_figure_bundle.R")
MODE <- s30fb_parse_mode(MODE_ARGS)                       # refuses here unless exactly one of --dry-run / --real
DRY <- identical(MODE, "DRY")
source_mmm_helper("project_paths.R"); source_mmm_helper("stage30_screen.R")   # constants, group rule and label-count checks only

git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE))
commit <- git("rev-parse", "HEAD")[1]; branch <- git("branch", "--show-current")[1]
DRIVER <- { fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(fa)) normalizePath(fa[1], winslash = "/", mustWork = FALSE) else NA_character_ }
GATES <- list(); INPUTS <- list()
gate <- function(stage, name, passed, detail = "", hard = TRUE) {
  GATES[[length(GATES) + 1L]] <<- s30fb_gate_row(stage, name, passed, detail, hard)
  message(sprintf("  gate %-15s %-86s %s", stage, substr(name, 1, 86), if (isTRUE(passed)) "PASS" else if (hard) "FAIL" else "NOT MET (recorded)"))
  if (hard && !isTRUE(passed)) stop("Stage 30b gate failed [", stage, "] ", name, ": ", paste(detail, collapse = "; "), call. = FALSE)
  invisible(passed)
}
gate_rows <- function(rows) for (i in seq_len(nrow(rows))) gate(rows$stage[i], rows$gate[i], rows$passed[i], rows$detail[i], rows$hard[i])
inp <- function(path, role, sha = s30fb_sha(path), bytes = file.size(path)) {
  INPUTS[[length(INPUTS) + 1L]] <<- data.table(input = normalizePath(path, winslash = "/", mustWork = FALSE), bytes = as.numeric(bytes), sha256 = sha, role = role)
  invisible(sha) }
message("Stage 30b ", MODE, " at commit ", commit, if (DRY) " -- DRY_RUN_NOT_FOR_USE (scratch output only)" else "")

# ---------------------------------------------------------------- 0. code identity and output location
CODE_FILES <- c("Analysis/30b_stage30_figure_bundle.R", "Functions/stage30_figure_bundle.R", "Functions/stage30_screen.R",
                "Analysis/_pipeline_setup.R", "Functions/behavioral_dynamics_helpers.R", "Functions/project_paths.R")
cln <- s30fb_code_clean(git("status", "--porcelain", "--untracked-files=all", "--", CODE_FILES), git("ls-files", "--", CODE_FILES), CODE_FILES)
gate("0-code", "MMM files used are committed and clean (git status --untracked-files=all / ls-files)", cln$ok && grepl("^[0-9a-f]{40}$", commit),
     if (nzchar(cln$detail)) cln$detail else commit, hard = !DRY)
CODE_SHA <- vapply(CODE_FILES, function(f) s30fb_sha(file.path(MMM_REPO_ROOT, f)), "")
if (!DRY) gate("0-code", "REAL run executes the committed Analysis/30b_stage30_figure_bundle.R",
               identical(s30fb_norm_path(DRIVER), s30fb_norm_path(file.path(MMM_REPO_ROOT, "Analysis", "30b_stage30_figure_bundle.R"))), DRIVER)
gate("0-code", "pinned registry / x4_01 hashes = the Stage 30 driver constants (Functions/stage30_screen.R)",
     identical(S30FB_STAGE30$registry_sha256, S30SC_REGISTRY_SHA256) && identical(S30FB_INPUT_SHA256[["registry"]], S30SC_REGISTRY_SHA256) &&
       identical(S30FB_INPUT_SHA256[["inactivity_reference"]], S30SC_X401_SHA256))

ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
S30 <- file.path(AR, "pipeline", "30_exploratory_screen", S30FB_STAGE30$run_name)
REG_JSON <- file.path(AR, "canonical", "stage30_registry", "v1.0", "stage30_registry_v1.0.json")
B29_ROOT <- file.path(AR, "canonical", "behavior_bundle"); B29 <- file.path(B29_ROOT, S30FB_STAGE29$bundle_id)
bundle_id <- s30fb_bundle_id(Sys.Date(), commit)
DRY_DEFAULT <- "C:/Users/topohl/Documents/e9_behavior_rationalization_audit_2026-09-27/manuscript_integration_2026-09-29/evidence/s30b_dry"
if (DRY) {
  OUT_ROOT <- file.path(normalizePath(Sys.getenv("MMM_S30B_DRY_ROOT", DRY_DEFAULT), winslash = "/", mustWork = FALSE), format(Sys.time(), "%Y%m%d_%H%M%S"))
  gate("0-output", "DRY output on C:, outside analysis_ready, S: and this repo",
       grepl("^c:/", tolower(OUT_ROOT)) && !s30fb_inside(OUT_ROOT, AR) && !s30fb_inside(OUT_ROOT, ROOT) && !s30fb_inside(OUT_ROOT, MMM_REPO_ROOT), OUT_ROOT)
  gate("0-output", "DRY timestamp folder does not exist", !dir.exists(OUT_ROOT), OUT_ROOT)
} else {
  OUT_ROOT <- file.path(AR, "canonical", "stage30_figure_bundle")
  gate("0-output", "REAL bundle version folder does not exist (immutable)", !dir.exists(file.path(OUT_ROOT, bundle_id)), file.path(OUT_ROOT, bundle_id))
}
longest <- max(nchar(file.path(OUT_ROOT, paste0(".tmp_", bundle_id), paste0(S30FB_TABLES, ".csv"))))
gate("0-output", "every output path shorter than 250 characters (Windows R path limit)", longest < 250, longest)

# ---------------------------------------------------------------- 1. frozen Stage 30 run and registry
V30 <- s30fb_verify_stage30_run(S30); gate_rows(V30$gates)
for (i in seq_len(nrow(V30$check))) inp(file.path(S30, V30$check$file[i]), "stage30_run_output", V30$check$observed_sha256[i], V30$check$observed_bytes[i])
inp(file.path(S30, "audit", "output_manifest.csv"), "stage30_output_manifest", V30$manifest_sha256)
reg_sha <- s30fb_sha(REG_JSON); inp(REG_JSON, "stage30_registry", reg_sha)
gate("1-registry", "registry JSON SHA-256 = frozen v1.0", identical(reg_sha, S30FB_STAGE30$registry_sha256), reg_sha)
REG <- jsonlite::fromJSON(REG_JSON, simplifyVector = FALSE)
gate("1-registry", "registry status FROZEN, version 1.0", identical(REG$meta$status, "FROZEN") && identical(REG$meta$version, S30FB_STAGE30$registry_version))
IH <- fread(file.path(S30, "audit", "input_hashes.csv"), colClasses = "character")    # hash-verified through the output manifest
rec_reg <- s30fb_recorded_input(IH, "registry")
gate("1-registry", "registry path and hash = Stage 30 input_hashes (role registry)",
     identical(s30fb_norm_path(rec_reg$input), s30fb_norm_path(REG_JSON)) && identical(rec_reg$sha256, reg_sha), rec_reg$input)

# ---------------------------------------------------------------- 2. labels, outcome and inactivity references (= input_hashes = pinned)
REC <- list()
for (role in c("freeze_record:sus_list", "freeze_record:con_list", "freeze_record:combz_table", "inactivity_reference")) {
  r <- s30fb_recorded_input(IH, role); now <- if (file.exists(r$input)) s30fb_sha(r$input) else NA_character_
  gate("2-inputs", paste(role, ": on-disk SHA-256 = Stage 30 input_hashes = pinned"),
       identical(now, r$sha256) && identical(now, S30FB_INPUT_SHA256[[role]]), paste(r$input, now))
  inp(r$input, role, now); REC[[role]] <- r$input
}
W1_DIR <- file.path(dirname(dirname(REC[["inactivity_reference"]])), "W1_canonical_inactivity_gate")
W104 <- file.path(W1_DIR, S30FB_W104$file); W199 <- file.path(W1_DIR, S30FB_W104$sha_list)
w104_sha <- s30fb_sha(W104); w199_rec <- s30fb_sha_list_lookup(readLines(W199, warn = FALSE), S30FB_W104$file)
gate("2-inputs", "w1_04_gate_values.csv SHA-256 = pinned = w1_99_output_sha256.txt entry", identical(w104_sha, S30FB_W104$sha256) && identical(w199_rec, w104_sha),
     paste(w104_sha, w199_rec))
inp(W104, "inactivity_gate_values_full_precision", w104_sha); inp(W199, "inactivity_gate_sha_list")

# ---------------------------------------------------------------- 3. Stage 29 bundle ebb_v101
V29 <- s30fb_verify_stage29_bundle(B29, file.path(B29_ROOT, "BUNDLE_REGISTRY.csv")); gate_rows(V29$gates)
for (i in seq_len(nrow(V29$check))) inp(file.path(B29, V29$check$file[i]), "stage29_bundle_file", V29$check$observed_sha256[i], V29$check$observed_bytes[i])
inp(file.path(B29, "00_manifest.csv"), "stage29_bundle_manifest", V29$manifest_sha256)
inp(file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), "stage29_bundle_registry (read for the FROZEN row; appended by later bundles)")
H29 <- fread(file.path(B29, "H_provenance.csv"), colClasses = "character")
gate("3-stage29", "Stage 30 README Stage 29 folder = ebb_v101 H_provenance stage29_output_dir",
     identical(s30fb_norm_path(V30$stage29_folder), s30fb_norm_path(H29[key == "stage29_output_dir", value])), V30$stage29_folder)

# ---------------------------------------------------------------- 4. read the frozen values (copies; full precision where an rds exists)
T30 <- file.path(S30, "tables")
MASTER <- fread(file.path(T30, "master_hypothesis_table.csv")); POOL <- fread(file.path(T30, "pool_estimates.csv"))
LCOMP <- fread(file.path(T30, "l_components.csv")); SENS <- fread(file.path(T30, "sensitivities.csv")); LOBO <- fread(file.path(T30, "lobo.csv"))
FW <- as.data.table(as.data.frame(readRDS(file.path(T30, "features_cc1_cc4_windows_full_precision.rds"))))
FK <- as.data.table(as.data.frame(readRDS(file.path(T30, "features_cookie_full_precision.rds"))))
X4 <- fread(REC[["inactivity_reference"]], colClasses = list(character = "AnimalNum"))
GV <- fread(W104)
A1 <- fread(file.path(B29, "A1_animal_cc1.csv"), colClasses = list(character = "AnimalNum"))
A4 <- fread(file.path(B29, "A4_combz_animals.csv"), colClasses = list(character = "AnimalNum"))
CFG29 <- jsonlite::fromJSON(file.path(B29, "I_analysis_config.json"), simplifyVector = FALSE)
CZ <- fread(REC[["freeze_record:combz_table"]], select = c("AnimalNum", "Sex", "Batch", "outcome_group", "CombZ"), colClasses = c(AnimalNum = "character"))
CZ[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), Batch = paste0("B", Batch))]
read_list <- function(p) { x <- canonical_animal_id(readLines(p, warn = FALSE)); unique(x[!is.na(x) & nzchar(x)]) }
SUS <- read_list(REC[["freeze_record:sus_list"]]); CON <- read_list(REC[["freeze_record:con_list"]])
FW[, AnimalNum := canonical_animal_id(AnimalNum)]; FK[, AnimalNum := canonical_animal_id(AnimalNum)]; X4[, AnimalNum := canonical_animal_id(AnimalNum)]
A1[, AnimalNum := canonical_animal_id(AnimalNum)]; A4[, AnimalNum := canonical_animal_id(AnimalNum)]
gate("4-read", "features: 348 SIS windows (87 x CC1-CC4), 77 cookie SIS animals; x4_01 1776 rows",
     nrow(FW) == 348L && uniqueN(FW$AnimalNum) == S30FB_N$sis_cc1 && nrow(FK) == S30FB_N$cookie_sis && nrow(X4) == 1776L, paste(nrow(FW), nrow(FK), nrow(X4)))
gate("4-read", "full-precision rds = features CSV (15 significant digits) for every numeric column",
     { fc <- fread(file.path(T30, "features_cc1_cc4_windows.csv")); kc <- fread(file.path(T30, "features_cookie.csv"))
       nm <- function(a, b) all(vapply(names(a)[vapply(a, is.numeric, TRUE)], function(k) s30fb_near_rel(as.numeric(a[[k]]), as.numeric(b[[k]]), 1e-13), TRUE))
       nrow(fc) == nrow(FW) && nrow(kc) == nrow(FK) && nm(FW, fc) && nm(FK, kc) })

# ---------------------------------------------------------------- 5. Group (driver rule) and CombZ, checked against v101
AN <- unique(FW[, .(AnimalNum, Batch, Sex)]); CK <- FK[, .(AnimalNum, Batch, Sex)]
GRP <- data.table(AnimalNum = union(AN$AnimalNum, CK$AnimalNum)); GRP[, Group := s30fb_group_from_lists(AnimalNum, SUS, CON)]
gate("5-labels", "group rule = Stage 30 driver s30sc_labels_from_lists (every SIS animal)",
     identical(GRP$Group, s30sc_labels_from_lists(GRP, SUS, CON)$Group) && !any(GRP$Group == "CON"), paste(GRP[, .N, by = Group][, paste(Group, N)], collapse = ", "))
gate("5-labels", "Group = ebb_v101 A1 Group for the 87 SIS CC1 animals", {
  m <- merge(GRP, A1[, .(AnimalNum, G1 = Group)], by = "AnimalNum"); nrow(m) == nrow(AN) && all(m[AnimalNum %in% AN$AnimalNum, Group == G1]) })
gate("5-labels", "Group = ebb_v101 A4 Group for every SIS animal (87 CC1 + 77 cookie)", {
  m <- merge(GRP, A4[, .(AnimalNum, G4 = Group)], by = "AnimalNum"); nrow(m) == nrow(GRP) && all(m$Group == m$G4) })
gate("5-labels", "Group = CombZ-table outcome_group for every SIS animal", {
  m <- merge(GRP, CZ[, .(AnimalNum, og = outcome_group)], by = "AnimalNum"); nrow(m) == nrow(GRP) && all(m$Group == m$og) })
LC <- s30sc_label_count_checks(merge(AN, GRP, by = "AnimalNum"), CK, REG$populations)
for (i in seq_len(nrow(LC))) gate("5-labels", paste(LC$gate[i], "(Stage 30 helper)"), LC$passed[i], LC$detail[i])
CZS <- CZ[AnimalNum %in% GRP$AnimalNum, .(AnimalNum, CombZ)]
gate("5-labels", "CombZ (canonical table) = ebb_v101 A1 CombZ (87) and A4 CombZ (all SIS) within 1e-12", {
  m1 <- merge(CZS, A1[, .(AnimalNum, c1 = CombZ)], by = "AnimalNum"); m4 <- merge(CZS, A4[, .(AnimalNum, c4 = CombZ)], by = "AnimalNum")
  nrow(m1) == S30FB_N$sis_cc1 && nrow(m4) == nrow(GRP) && max(abs(m1$CombZ - m1$c1)) <= 1e-12 && max(abs(m4$CombZ - m4$c4)) <= 1e-12 })

# ---------------------------------------------------------------- 6. tables
S5 <- s30fb_display_labels(REG$measures, CFG29$metrics)
S0 <- s30fb_master_table(MASTER, S5)
gate("6-tables", "S0: standardized estimate reproduced from the stored rule within 1e-12 (48 rows)", attr(S0, "std_max_abs_diff") <= S30FB_STD_TOL, attr(S0, "std_max_abs_diff"))
S0b <- s30fb_pool_table(POOL)
gate("6-tables", "S0b: standardized estimate reproduced within 1e-12 (16 POOL rows)", attr(S0b, "std_max_abs_diff") <= S30FB_STD_TOL, attr(S0b, "std_max_abs_diff"))
S0c <- s30fb_drop_run_mode(LCOMP); S0d <- s30fb_drop_run_mode(SENS); S0e <- s30fb_drop_run_mode(LOBO)
S1 <- s30fb_light_animals(FW, GRP, CZS)
gate("6-tables", "S1 = ebb_v101 A1 (Sex, Batch, CageEpisodeID identical; light_phase_crossing_rate within 1e-9)", {
  m <- merge(S1, A1[, .(AnimalNum, Sex1 = Sex, Batch1 = Batch, Cage1 = CageEpisodeID, lp1 = light_phase_crossing_rate)], by = "AnimalNum")
  nrow(m) == S30FB_N$sis_cc1 && all(m$Sex == m$Sex1) && all(m$Batch == m$Batch1) && all(m$CageEpisodeID == m$Cage1) &&
    max(abs(m$light_phase_crossing_rate - m$lp1)) <= S30FB_ID_TOL })
S1b <- s30fb_estimate_rows(MASTER, POOL, SENS, paste0("SLEEP-CAT-IA40L-", c("F", "M", "INT", "POOL")), "POSINACT60", "posinact60")
S1c <- s30fb_light_descriptives(S1)
S2 <- s30fb_rate_windows(X4)
gate("6-tables", "S2 light windows = Stage 30 SIS features (rate and >= 40 s fraction within 1e-9; 348 windows)", {
  m <- merge(S2, FW[, .(AnimalNum, CC, lp = light_phase_crossing_rate, pl = posinact40_light)], by = c("AnimalNum", "CC"))
  nrow(m) == 348L && max(abs(m$crossing_rate - m$lp)) <= S30FB_ID_TOL && max(abs(m$frac - m$pl)) <= S30FB_ID_TOL })
QC40 <- REG$measures$positional_inactivity_40$group_blind_qc
S2b <- s30fb_rate_relationship(QC40, GV, X4, normalizePath(W104, winslash = "/", mustWork = FALSE), w104_sha)
gate("6-tables", "S2b: round(full, 2) = registry rho / ICC; 444 windows and 111 animals per phase",
     nrow(S2b) == 2L && all(S2b$n_windows == S30FB_N$windows_per_phase) && all(S2b$n_animals == S30FB_N$rfid_animals), paste(S2b$phase, S2b$rho_full, S2b$icc_full))
QR <- s30fb_qc_rounding_checks(QC40, GV)
gate("6-tables", "registry group_blind_qc medians / share >= 0.99 = round(w1_04, 3)", all(QR$ok), paste(QR[, paste(item, registry, rounded)], collapse = "; "))
S3 <- s30fb_cookie_animals(FK, GRP, CZS, REG$measures$cookie_response_60$epm1_dates)
S3b <- s30fb_cookie_descriptives(S3, REG$measures$cookie_response_60$group_blind_qc)
S3c <- s30fb_estimate_rows(MASTER, POOL, SENS, c(paste0("COOKIE-CAT-DC60-", c("F", "M", "INT", "POOL")), paste0("COOKIE-CONT-DC60-", c("F", "M", "INT", "POOL"))),
                           "DCOOKIE45", "dcookie45")
S3d <- s30fb_cookie_lines(S3, MASTER)
gate("6-tables", "S3d: line n = frozen n_animals (F 46, M 31) and batches = frozen n_batches", identical(S3d$n, c(46L, 31L)), paste(S3d$Sex, S3d$n))
S4 <- s30fb_screen_matrix(S0)
gate("6-tables", "S4: 48 cells = 16 metric rows (CONT 6, CAT 3, L 7) x 3 columns",
     nrow(S4) == 48L && max(S4$row_order) == 16L && identical(S4[, uniqueN(row_order), by = question][match(S30FB_QUESTION_ORDER, question), V1], c(6L, 3L, 7L)))
s30fb_check_display(S0, c("display_metric", "display_block", "display_question"), "S0_master_hypotheses")
gate("6-tables", "display labels contain no banned wording (S0, S4, S5)", TRUE)
TABLES <- list(S0_master_hypotheses = S0, S0b_pool_estimates = S0b, S0c_l_components = S0c, S0d_sensitivities = S0d, S0e_lobo = S0e,
               S1_light_animals_cc1 = S1, S1b_light_estimates = S1b, S1c_light_descriptives = S1c, S2_rate_inactivity_windows = S2,
               S2b_rate_inactivity_relationship = S2b, S3_cookie_animals = S3, S3b_cookie_descriptives = S3b, S3c_cookie_estimates = S3c,
               S3d_cookie_cont_lines = S3d, S4_screen_matrix = S4, S5_display_labels = S5)
gate("6-tables", "no table carries run_mode", !any(vapply(TABLES, function(t) "run_mode" %in% names(t), TRUE)))

# ---------------------------------------------------------------- 7. provenance and write (immutable)
STATUS <- if (DRY) S30FB_STATUS_DRY else S30FB_STATUS_REAL
GEN_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
GT <- rbindlist(GATES)
pk <- c("data.table", "jsonlite", "digest")
H <- s30fb_provenance(list(
  bundle_id = bundle_id, status = STATUS, generated_at = GEN_AT, generator = "Analysis/30b_stage30_figure_bundle.R",
  mmm_git_commit = commit, mmm_branch = branch, mmm_code_clean = cln$ok, run_mode = MODE,
  mmm_code_sha256 = paste(names(CODE_SHA), CODE_SHA, sep = "=", collapse = "; "),
  stage30_run_dir = normalizePath(S30, winslash = "/", mustWork = FALSE), stage30_run_commit = S30FB_STAGE30$run_commit,
  stage30_run_mode = V30$run$run_mode, stage30_status_counts = V30$run$status_counts, stage30_classification_counts = V30$run$classification_counts,
  stage30_registry_sha256 = reg_sha, stage30_output_manifest_sha256 = V30$manifest_sha256,
  stage29_bundle_id = S30FB_STAGE29$bundle_id, stage29_bundle_manifest_sha256 = V29$manifest_sha256,
  stage29_bundle_dir = normalizePath(B29, winslash = "/", mustWork = FALSE), stage29_output_dir = H29[key == "stage29_output_dir", value],
  labels_sus_sha256 = S30FB_INPUT_SHA256[["freeze_record:sus_list"]], labels_con_sha256 = S30FB_INPUT_SHA256[["freeze_record:con_list"]],
  combz_table_sha256 = S30FB_INPUT_SHA256[["freeze_record:combz_table"]], inactivity_reference_sha256 = S30FB_INPUT_SHA256[["inactivity_reference"]],
  gate_values_sha256 = w104_sha, group_rule = S30FB_GROUP_RULE,
  scientific_recomputation = S30FB_SCIENTIFIC_RECOMPUTATION, descriptive_computations = S30FB_DESCRIPTIVE_COMPUTATIONS,
  quantile_type = "R stats::quantile type 7", standardized_ci_rule = "CONT: ci x standardizer_sd (SD_x); CAT: ci / standardizer_sd (SD_y); L: none",
  cookie_line_derivation = S30FB_LINE_DERIVATION, value_precision = "doubles written as the shortest decimal (15-17 significant digits) that reads back to the identical double; every CSV re-read and compared",
  label_sources = "S5_display_labels.csv label_source / unit_source; block / question / sex column labels: DESIGN 2026-09-29 section 2 (S4)",
  tables = paste(sprintf("%s (%d rows)", names(TABLES), vapply(TABLES, nrow, 1L)), collapse = "; "),
  gates_passed = sprintf("%d of %d", sum(GT$passed), nrow(GT)), r_version = R.version.string,
  packages = paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; ")))
IN <- unique(rbindlist(INPUTS), by = c("input", "role"))
W <- s30fb_write_bundle(TABLES, OUT_ROOT, bundle_id, H, IN, STATUS, S30FB_STAGE30$run_commit, commit, GEN_AT)
chk <- s30fb_manifest_check(W$dir, W$manifest)
gate("7-write", "written bundle: every file = 00_manifest; nothing else in the folder",
     all(chk$ok) && setequal(s30fb_files_on_disk(W$dir), c(W$manifest$file, "00_manifest.csv")), W$dir)
gate("7-write", "written files are read-only", all(file.access(list.files(W$dir, full.names = TRUE), 2L) != 0L), W$dir)
if (DRY) fwrite(rbindlist(GATES), file.path(OUT_ROOT, "gate_results.csv"))
message("Stage 30b ", MODE, " complete: bundle ", bundle_id, " ", STATUS, " at ", W$dir, " (manifest sha256 ", W$manifest_sha256, ")")
for (n in names(TABLES)) message(sprintf("  %-34s %5d rows", n, nrow(TABLES[[n]])))
