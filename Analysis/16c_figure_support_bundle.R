# ================================================================
# Stage 16c - figure-support bundle v4 (source-data export for manuscript Figure 1, its options 3 and 3b, and the Stage 14
# domain-heatmap and HMM figure candidates)
# MMMSociability -- Analysis/16c_figure_support_bundle.R
# ================================================================
# The ONLY writer of the figure-support bundle that Exp9_manuscript imports (records: docs/FIGURE_SUPPORT_BUNDLE_v1.md to _v4.md).
# It fits, tests and refits nothing and computes no p or q value. It exports:
#   F1  descriptive CON cage means (ebb_v101 B1; 3 CON cages per sex per CC, 4 animals each),
#   F1b the CON descriptive mean copied from ebb_v101 B2,
#   F2  the six stored CombZ component z-scores exactly as they enter CombZ, with the frozen direction,
#   F2b the component definition (sources, standardisation as implemented, direction, relationship to CombZ).
#   P1  / P1b / P1c (v2) verbatim copies of the post hoc run 29b (registry POSTHOC_CON_CONTRASTS v1.0, fitted there once):
#       estimates (batch-balanced means, contrasts, Holm, labels), the common-cage-variance sensitivity, the diagnostics;
#       hash-gated on the run's audit/output_manifest.csv (4c079ecf...), every file, README and run_manifest.
#   D1  (v3) a verbatim copy of the Stage 14 domain-heatmap source table of the run promoted 2026-10-06c;
#   M1-M4 (v3) the stored Stage 14 HMM figure tables of that run with display labels (transition differences, transition
#       means with the rank the state-flow figure draws by, the HMM-resolution sensitivity with its Wald interval) and the
#       Stage 08 10-minute mean dwell times counted in doubling bins. Stage 14 is not re-run: every Stage 14 file must
#       match its pinned SHA-256 and the promotion record docs/behavior_output_producer_reruns/stage14_20261006c.csv.
#   E1  (v4) a verbatim copy of Stage 09's sex-stratified correlations (pipeline/09_early_prediction/10min; pinned SHA-256),
#   E3  (v5) a verbatim copy of Stage 09's early-window coverage summary (same folder; pinned SHA-256; group-blind),
#   E2-E2c (v4) verbatim copies of the post hoc run 29c (registry POSTHOC_MOVEMENT_COMBZ_FIT v1.0, fitted there once): the
#       Figure 1d least-squares display line with its 95% band, its coefficients (no t, no P) and diagnostics;
#   T1 / T1b (v4) the postnatal age at each design step (shuffle start and stop, the five tests, sampling) by date
#       arithmetic from the planning workbook E9_Experimental Timeline.xlsx (pinned SHA-256 = the Stage 33 pin) and the
#       ebb_v101 cage-change times, checked against every age the workbook records itself.
# Every input is hash-gated first: ebb_v101 (00_manifest ec59aa33..., every file, FROZEN registry row), the canonical
# CombZ table (1f6a2a69...), its component definition and thresholds, the sus / con lists, and the frozen CombZ
# definition code Analysis/build_later_outcome_combz.R (git blob pinned). That producer is PARSED, never run: its
# definition constants and composite-rule lines are evaluated in an empty environment (Functions/figure_support_bundle.R).
#
# Output (REAL): analysis_ready/canonical/figure_support_bundle/<bundle_id>/ with BUNDLE_REGISTRY.csv beside it;
#   bundle_id = <FSB_BUNDLE_PREFIX>_<YYYYMMDD>_<commit7> (fsb_v5 on the corrected outcome; fsb_v1 to fsb_v4 stay FROZEN and untouched); written once (refused if it, its staging folder, its registry row or its gate
#   log exists), files 0444; post-write gates before the registry row; complete gate log in logs/ beside the registry.
# Modes (exactly one argument; with none the runner refuses before reading anything):
#   --dry-run  writes <dry root>/<YYYYMMDD_HHMMSS>/<bundle_id>/ (status DRY_RUN_NOT_FOR_USE); never writes to S:.
#              Dry root: MMM_FSB_DRY_ROOT or the default below (must be on C:, outside analysis_ready and this repo).
#   --real     writes the S: bundle once; refused unless the MMM files it uses are committed and clean and this is the
#              committed runner.
# Run from the MMMSociability repo root:  Rscript Analysis/16c_figure_support_bundle.R --dry-run
# ================================================================

MODE_ARGS <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest); library(readxl) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
suppressPackageStartupMessages(source(.pipeline_setup))
source_mmm_helper("stage30_figure_bundle.R"); source_mmm_helper("figure_support_bundle.R")
MODE <- fsb_parse_mode(MODE_ARGS)                         # refuses here unless exactly one of --dry-run / --real
DRY <- identical(MODE, "DRY")
source_mmm_helper("project_paths.R")

git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE))
commit <- git("rev-parse", "HEAD")[1]; branch <- git("branch", "--show-current")[1]
DRIVER <- { fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(fa)) normalizePath(fa[1], winslash = "/", mustWork = FALSE) else NA_character_ }
GATES <- list(); INPUTS <- list()
gate <- function(stage, name, passed, detail = "", hard = TRUE) {
  GATES[[length(GATES) + 1L]] <<- s30fb_gate_row(stage, name, passed, detail, hard)
  message(sprintf("  gate %-13s %-92s %s", stage, substr(name, 1, 92), if (isTRUE(passed)) "PASS" else if (hard) "FAIL" else "NOT MET (recorded)"))
  if (hard && !isTRUE(passed)) stop("Stage 16c gate failed [", stage, "] ", name, ": ", paste(detail, collapse = "; "), call. = FALSE)
  invisible(passed)
}
gate_rows <- function(rows) for (i in seq_len(nrow(rows))) gate(rows$stage[i], rows$gate[i], rows$passed[i], rows$detail[i], rows$hard[i])
inp <- function(path, role, sha = s30fb_sha(path), bytes = file.size(path)) {
  INPUTS[[length(INPUTS) + 1L]] <<- data.table(input = normalizePath(path, winslash = "/", mustWork = FALSE), bytes = as.numeric(bytes), sha256 = sha, role = role)
  invisible(sha) }
message("Stage 16c ", MODE, " at commit ", commit, if (DRY) " -- DRY_RUN_NOT_FOR_USE (scratch output only)" else "")

# ---------------------------------------------------------------- 0. code identity and output location
CODE_FILES <- c("Analysis/16c_figure_support_bundle.R", "Functions/figure_support_bundle.R", "Functions/stage30_figure_bundle.R",
                "Analysis/_pipeline_setup.R", "Functions/behavioral_dynamics_helpers.R", "Functions/project_paths.R", FSB_COMBZ_CODE$path,
                FSB_STAGE14$record)
cln <- s30fb_code_clean(git("status", "--porcelain", "--untracked-files=all", "--", CODE_FILES), git("ls-files", "--", CODE_FILES), CODE_FILES)
gate("0-code", "MMM files used are committed and clean (git status --untracked-files=all / ls-files)", cln$ok && grepl("^[0-9a-f]{40}$", commit),
     if (nzchar(cln$detail)) cln$detail else commit, hard = !DRY)
CODE_SHA <- vapply(CODE_FILES, function(f) s30fb_sha(file.path(MMM_REPO_ROOT, f)), "")
if (!DRY) gate("0-code", "REAL run executes the committed Analysis/16c_figure_support_bundle.R",
               identical(s30fb_norm_path(DRIVER), s30fb_norm_path(file.path(MMM_REPO_ROOT, "Analysis", "16c_figure_support_bundle.R"))), DRIVER)
PRODUCER <- file.path(MMM_REPO_ROOT, FSB_COMBZ_CODE$path)
blob <- git("hash-object", "--", FSB_COMBZ_CODE$path)[1]
gate("0-code", "frozen CombZ definition code: git hash-object of Analysis/build_later_outcome_combz.R = pinned blob", identical(blob, FSB_COMBZ_CODE$git_blob), blob)
gate("0-code", "frozen CombZ definition code: HEAD blob = pinned blob", identical(git("rev-parse", paste0("HEAD:", FSB_COMBZ_CODE$path))[1], FSB_COMBZ_CODE$git_blob))
inp(PRODUCER, paste0("combz_definition_code (git blob ", blob, "; parsed, never executed)"))

ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
B29_ROOT <- file.path(AR, "canonical", "behavior_bundle"); B29 <- file.path(B29_ROOT, FSB_STAGE29$bundle_id)
CZ_DIR <- file.path(AR, "canonical", "later_outcome_combz", "tables")
bundle_id <- fsb_bundle_id(Sys.Date(), commit)
DRY_DEFAULT <- "C:/Users/topohl/Documents/e9_behavior_rationalization_audit_2026-09-27/manuscript_integration_2026-09-29/evidence/fsb_dry"
if (DRY) {
  OUT_ROOT <- file.path(normalizePath(Sys.getenv("MMM_FSB_DRY_ROOT", DRY_DEFAULT), winslash = "/", mustWork = FALSE), format(Sys.time(), "%Y%m%d_%H%M%S"))
  gate("0-output", "DRY output on C:, outside analysis_ready, S: and this repo",
       grepl("^c:/", tolower(OUT_ROOT)) && !s30fb_inside(OUT_ROOT, AR) && !s30fb_inside(OUT_ROOT, ROOT) && !s30fb_inside(OUT_ROOT, MMM_REPO_ROOT), OUT_ROOT)
  gate("0-output", "DRY timestamp folder does not exist", !dir.exists(OUT_ROOT), OUT_ROOT)
} else {
  OUT_ROOT <- file.path(AR, "canonical", "figure_support_bundle")
  gate("0-output", "REAL bundle version folder does not exist (immutable)", !dir.exists(file.path(OUT_ROOT, bundle_id)), file.path(OUT_ROOT, bundle_id))
  gate("0-output", "REAL staging folder does not exist (no earlier aborted write)", !dir.exists(file.path(OUT_ROOT, paste0(".tmp_", bundle_id))),
       file.path(OUT_ROOT, paste0(".tmp_", bundle_id)))
  REG_OUT <- file.path(OUT_ROOT, "BUNDLE_REGISTRY.csv")
  gate("0-output", "REAL BUNDLE_REGISTRY.csv has no row for this bundle id",
       !file.exists(REG_OUT) || !(bundle_id %in% fread(REG_OUT, colClasses = "character")$bundle_id), REG_OUT)
  gate("0-output", "REAL gate log for this bundle does not exist (immutable)", !file.exists(s30fb_log_path(OUT_ROOT, bundle_id)), s30fb_log_path(OUT_ROOT, bundle_id))
}
OUT_PATHS <- c(file.path(OUT_ROOT, paste0(".tmp_", bundle_id), c(fsb_bundle_files(), "00_manifest.csv")), s30fb_log_path(OUT_ROOT, bundle_id))
longest <- max(nchar(OUT_PATHS))
gate("0-output", "every output path shorter than 250 characters (Windows R path limit)", longest < 250, longest)

# ---------------------------------------------------------------- 1. Stage 29 bundle ebb_v101
V29 <- s30fb_verify_stage29_bundle(B29, file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), FSB_STAGE29); gate_rows(V29$gates)
USED29 <- c("B1_animal_longitudinal.csv", "B2_descriptive_summaries.csv", "A4_combz_animals.csv", "A2b_combz_thresholds.csv", "I_analysis_config.json",
            "A2_prediction_animals.csv", "V_runtime_gates.csv")
for (f in USED29) { r <- V29$check[file == f]; gate("1-stage29", paste0("ebb_v101 ", f, " listed in 00_manifest and verified"), nrow(r) == 1L && isTRUE(r$ok), f)
  inp(file.path(B29, f), "stage29_bundle_file", r$observed_sha256, r$observed_bytes) }
inp(file.path(B29, "00_manifest.csv"), "stage29_bundle_manifest", V29$manifest_sha256)
inp(file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), "stage29_bundle_registry (read for the FROZEN row; appended by later bundles)")

# ---------------------------------------------------------------- 1b. post hoc run 29b (v2; copied verbatim)
PH_DIR <- file.path(AR, "pipeline", FSB_POSTHOC$stage_dir, FSB_POSTHOC$run_name)
VPH <- fsb_verify_posthoc_run(PH_DIR); gate_rows(VPH$gates)
for (f in VPH$manifest$file) inp(file.path(PH_DIR, f), paste0("posthoc_run_file ", f), VPH$check[file == f, observed_sha256])
inp(file.path(PH_DIR, "audit", "output_manifest.csv"), "posthoc_run_output_manifest", VPH$manifest_sha256)

# ---------------------------------------------------------------- 1c. Stage 14 display tables and Stage 08 dwell times (v3)
S14_DIR <- file.path(AR, FSB_STAGE14$root_rel)
S14_RECORD <- file.path(MMM_REPO_ROOT, FSB_STAGE14$record)
gate_rows(fsb_verify_stage14_inputs(S14_DIR, fread(S14_RECORD, colClasses = "character")))
for (k in names(FSB_STAGE14$files)) inp(file.path(S14_DIR, FSB_STAGE14$files[[k]]), paste0("stage14_", k), FSB_STAGE14$sha256[[k]])
inp(S14_RECORD, "stage14_promotion_record (lists every Stage 14 file of the promoted run with its SHA-256)")
DWELL <- file.path(AR, FSB_HMM_DWELL$root_rel, FSB_HMM_DWELL$file)
dw_sha <- if (file.exists(DWELL)) s30fb_sha(DWELL) else NA_character_
gate("1c-stage14", "Stage 08 10-minute dwell-time table: on-disk SHA-256 = pinned", identical(dw_sha, FSB_HMM_DWELL$sha256), paste(DWELL, dw_sha))
inp(DWELL, "hmm_dwell_times (Stage 08, 10-minute HMM; the table Stage 14 draws its dwell-time figure from)", dw_sha)

# ---------------------------------------------------------------- 1d. Stage 09 sex-stratified correlations (v4; copied verbatim)
S09_DIR <- file.path(AR, FSB_STAGE09$root_rel); S09 <- file.path(S09_DIR, FSB_STAGE09$file)
s09_sha <- if (file.exists(S09)) s30fb_sha(S09) else NA_character_
gate("1d-stage09", "Stage 09 sex-stratified correlation table: on-disk SHA-256 = pinned", identical(s09_sha, FSB_STAGE09$sha256), paste(S09, s09_sha))
inp(S09, "stage09_correlations_by_sex (copied verbatim as E1)", s09_sha)
S09_IN <- file.path(S09_DIR, FSB_STAGE09$input_file)
s09_in_sha <- if (file.exists(S09_IN)) s30fb_sha(S09_IN) else NA_character_
gate("1d-stage09", "Stage 09 model_ladder_input.csv: on-disk SHA-256 = pinned (= the Stage 29 v1.0.1 release input pin)",
     identical(s09_in_sha, FSB_STAGE09$input_sha256), paste(S09_IN, s09_in_sha))
inp(S09_IN, "stage09_model_ladder_input (identity gate for E1 only)", s09_in_sha)
S09_COV <- file.path(S09_DIR, FSB_STAGE09_COVERAGE$file)
s09_cov_sha <- if (file.exists(S09_COV)) s30fb_sha(S09_COV) else NA_character_
gate("1d-stage09", "Stage 09 early-window coverage summary: on-disk SHA-256 = pinned", identical(s09_cov_sha, FSB_STAGE09_COVERAGE$sha256), paste(S09_COV, s09_cov_sha))
inp(S09_COV, "stage09_early_window_contract_summary (copied verbatim as E3)", s09_cov_sha)

# ---------------------------------------------------------------- 1e. post hoc run 29c (v4; copied verbatim)
P29C_DIR <- file.path(AR, "pipeline", FSB_POSTHOC29C$stage_dir, FSB_POSTHOC29C$run_name)
VP29C <- fsb_verify_posthoc_run(P29C_DIR, FSB_POSTHOC29C, FSB_POSTHOC29C_TABLES, "1e-posthoc29c"); gate_rows(VP29C$gates)
for (f in VP29C$manifest$file) inp(file.path(P29C_DIR, f), paste0("posthoc29c_run_file ", f), VP29C$check[file == f, observed_sha256])
inp(file.path(P29C_DIR, "audit", "output_manifest.csv"), "posthoc29c_run_output_manifest", VP29C$manifest_sha256)

# ---------------------------------------------------------------- 1f. planning workbook (v4; the bytes Stage 33 pins)
TIMELINE <- file.path(dirname(mmm_endpoint_source_root()), "Planning", FSB_TIMELINE$file)
tl_sha <- if (file.exists(TIMELINE)) s30fb_sha(TIMELINE) else NA_character_
gate("1f-timeline", "planning workbook E9_Experimental Timeline.xlsx: on-disk SHA-256 = pinned (2026-10-12, batch-6 SIS start corrected)",
     identical(tl_sha, FSB_TIMELINE$sha256), paste(TIMELINE, tl_sha))
inp(TIMELINE, "planning_workbook (Animals sheet: dates of birth, test days and the ages it records)", tl_sha)

# ---------------------------------------------------------------- 2. canonical CombZ tables and label lists (= pinned)
SRC <- c(combz_table = mmm_path_get("behavior.later_outcome_combz", "animal_level", root = ROOT),
         combz_component_definition = mmm_path_get("behavior.later_outcome_combz", "component_definition", root = ROOT),
         combz_classification_thresholds = mmm_path_get("behavior.later_outcome_combz", "classification_thresholds", root = ROOT),
         sus_list = mmm_path_get("behavior.combz_upstream_workbook", "susceptible_ids", root = ROOT),
         con_list = mmm_path_get("behavior.combz_upstream_workbook", "control_ids", root = ROOT))
gate("2-inputs", "CombZ tables resolve to analysis_ready/canonical/later_outcome_combz/tables",
     all(vapply(SRC[1:3], function(p) identical(s30fb_norm_path(dirname(p)), s30fb_norm_path(CZ_DIR)), TRUE)), paste(SRC[1:3], collapse = "; "))
for (role in names(SRC)) {
  now <- if (file.exists(SRC[[role]])) s30fb_sha(SRC[[role]]) else NA_character_
  gate("2-inputs", paste(role, ": on-disk SHA-256 = pinned"), identical(now, FSB_INPUT_SHA256[[role]]), paste(SRC[[role]], now))
  inp(SRC[[role]], role, now)
}

# ---------------------------------------------------------------- 3. frozen CombZ definition (parsed from the producer)
DEF <- fsb_read_combz_definition(PRODUCER); CST <- DEF$constants
gate("3-definition", "producer parsed: 7 definition constants (one assignment each) and the 4 consecutive composite-rule lines",
     length(CST) == length(FSB_COMBZ_CONSTANTS) && length(DEF$rule) == length(FSB_COMBZ_RULE), paste(DEF$n_expressions, "top-level expressions"))
gate("3-definition", "frozen components: NOR, sucrose_pref, weight_dev, delta_cort, adrenal_weight, spleen_weight; inverted: delta_cort, adrenal_weight, spleen_weight",
     identical(CST$COMBZ_COMPONENTS, c("NOR", "sucrose_pref", "weight_dev", "delta_cort", "adrenal_weight", "spleen_weight")) &&
       identical(CST$COMBZ_COMPONENT_META$sign_inverted, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)))
COMPDEF <- fread(SRC[["combz_component_definition"]])
gate_rows(fsb_component_definition_gates(DEF, COMPDEF))

# ---------------------------------------------------------------- 4. read
B1 <- fread(file.path(B29, "B1_animal_longitudinal.csv"), colClasses = list(character = "AnimalNum"))
B2 <- fread(file.path(B29, "B2_descriptive_summaries.csv"))
A4 <- fread(file.path(B29, "A4_combz_animals.csv"), colClasses = list(character = "AnimalNum"))
A2B <- fread(file.path(B29, "A2b_combz_thresholds.csv"))
CFG29 <- jsonlite::fromJSON(file.path(B29, "I_analysis_config.json"), simplifyVector = FALSE)
A2 <- fread(file.path(B29, "A2_prediction_animals.csv"), colClasses = list(character = "AnimalNum"))
V29T <- fread(file.path(B29, "V_runtime_gates.csv"), colClasses = "character")
TL <- data.table::as.data.table(suppressMessages(readxl::read_excel(TIMELINE, sheet = FSB_TIMELINE$sheet, col_types = "text")))
gate("4-read", "planning workbook re-hashed after the read = pinned (the bytes read are the pinned bytes)", identical(s30fb_sha(TIMELINE), FSB_TIMELINE$sha256))
CZ <- fread(SRC[["combz_table"]], colClasses = list(character = c("AnimalNum", "upstream_id", "Batch")))
THR <- fread(SRC[["combz_classification_thresholds"]])
SUS <- fsb_read_id_list(readLines(SRC[["sus_list"]], warn = FALSE)); CON <- fsb_read_id_list(readLines(SRC[["con_list"]], warn = FALSE))
CZ[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), Batch = paste0("B", Batch))]
B1[, AnimalNum := canonical_animal_id(AnimalNum)]; A4[, AnimalNum := canonical_animal_id(AnimalNum)]; A2[, AnimalNum := canonical_animal_id(AnimalNum)]
gate("4-read", "B1: 111 animals x CC1-CC4 (444 rows); CON 24 animals (96 rows)",
     nrow(B1) == 4L * FSB_N$rfid_animals && uniqueN(B1$AnimalNum) == FSB_N$rfid_animals && setequal(B1$CC, FSB_CCS) &&
       B1[Group == "CON", uniqueN(AnimalNum)] == FSB_N$con_rfid_animals && nrow(B1[Group == "CON"]) == 4L * FSB_N$con_rfid_animals,
     paste(nrow(B1), uniqueN(B1$AnimalNum), nrow(B1[Group == "CON"])))
gate("4-read", "CombZ table: 117 unique animals; A4: 117 animals", nrow(CZ) == FSB_N$combz_animals && !anyDuplicated(CZ$AnimalNum) && nrow(A4) == FSB_N$combz_animals,
     paste(nrow(CZ), nrow(A4)))
gate("4-read", sprintf("sus / con lists: %d and %d ids, disjoint", FSB_N$sus_list, FSB_N$con_list),
     length(SUS) == FSB_N$sus_list && length(CON) == FSB_N$con_list && !length(intersect(SUS, CON)), paste(length(SUS), length(CON)))

# ---------------------------------------------------------------- 5. labels and CombZ cross-checks (no new label)
CZ[, Group_lists := fsb_group_from_lists(AnimalNum, SUS, CON)]
gate("5-labels", "CombZ-table outcome_group = list rule (CON if con list, else SUS if sus list, else RES) for all 117",
     all(CZ$outcome_group == CZ$Group_lists), paste(CZ[, .N, by = outcome_group][, paste(outcome_group, N)], collapse = ", "))
gate("5-labels", "CON on the con list <=> experimental_condition CON", all((CZ$experimental_condition == "CON") == (CZ$AnimalNum %in% CON)))
m4 <- merge(CZ[, .(AnimalNum, Sex, Batch, outcome_group, CombZ)], A4[, .(AnimalNum, Sex4 = Sex, Batch4 = Batch, G4 = Group, C4 = CombZ)], by = "AnimalNum")
gate("5-labels", "CombZ table = ebb_v101 A4 for all 117 (Sex, Batch, Group identical; CombZ within 1e-12)",
     nrow(m4) == FSB_N$combz_animals && all(m4$Sex == m4$Sex4) && all(m4$Batch == m4$Batch4) && all(m4$outcome_group == m4$G4) && max(abs(m4$CombZ - m4$C4)) <= FSB_TOL,
     max(abs(m4$CombZ - m4$C4)))
m1 <- merge(unique(B1[, .(AnimalNum, Sex, Batch, Group, CombZ)]), CZ[, .(AnimalNum, SexZ = Sex, BatchZ = Batch, GZ = outcome_group, CZ = CombZ)], by = "AnimalNum")
gate("5-labels", "B1 animals: one Sex / Batch / Group / CombZ each, = CombZ table (CombZ within 1e-12)",
     nrow(m1) == FSB_N$rfid_animals && all(m1$Sex == m1$SexZ) && all(m1$Batch == m1$BatchZ) && all(m1$Group == m1$GZ) && max(abs(m1$CombZ - m1$CZ)) <= FSB_TOL,
     paste(nrow(m1), max(abs(m1$CombZ - m1$CZ))))
mt <- merge(THR[, .(Sex, t = susceptibility_threshold)], A2B[, .(Sex, t2 = susceptibility_threshold)], by = "Sex")
gate("5-labels", "canonical thresholds = ebb_v101 A2b within 1e-12 (Female, Male)", nrow(mt) == 2L && max(abs(mt$t - mt$t2)) <= FSB_TOL, paste(mt$Sex, mt$t))

# ---------------------------------------------------------------- 6. tables
LAB <- fsb_metric_labels(CFG29$metrics)
F1 <- fsb_con_cage_means(B1, LAB); F1b <- fsb_con_reference_means(B2, LAB)
CK <- fsb_con_cage_checks(F1, F1b); CP <- fsb_con_composition(B1)
gate("6-F1", "F1 / F1b: 4 measures x 2 sexes x 4 CC; 96 cage rows, 32 reference rows",
     nrow(F1) == 96L && nrow(F1b) == 32L && nrow(CK) == 32L && setequal(F1$measure, FSB_MEASURES), paste(nrow(F1), nrow(F1b), nrow(CK)))
gate("6-F1", "exactly 3 CON cage epochs per sex per CC (every measure)", all(CK$ok_cages), paste(CK[ok_cages == FALSE, paste(measure, Sex, CC)], collapse = ","))
gate("6-F1", "every CON cage: 4 CON animals with a value, n_in_cage 4", all(CK$ok_sizes), paste(CK[ok_sizes == FALSE, paste(measure, Sex, CC, cage_sizes)], collapse = ","))
gate("6-F1", "one CON cage per batch: Female B3 / B4 / B6, Male B1 / B2 / B5", all(CK$ok_batches), paste(CK[ok_batches == FALSE, paste(measure, Sex, CC, batches)], collapse = ","))
gate("6-F1", "sum of cage n = B2 CON n (12 per sex x CC)", all(CK$ok_n) && all(CK$b2_n == FSB_N$con_per_sex), paste(unique(CK$b2_n), collapse = ","))
gate("6-F1", "mean of the 3 cage means = B2 CON mean within 1e-12 (equal cage sizes)", all(CK$ok_mean), max(CK$abs_diff))
gate("6-F1", "CON cage epochs hold only CON animals", CP$ok_pure, paste(CP$mixed_cages, collapse = ","))
gate("6-F1", "each CON group keeps the same 4 animals at CC1-CC4", CP$ok_stable, paste(CP$stable[n_sets != 1L, paste(Sex, Batch)], collapse = ","))
F2 <- fsb_combz_components(CZ, DEF, THR)
gate("6-F2", "F2: 117 animals x 6 components = 702 rows", nrow(F2) == FSB_N$combz_animals * FSB_N$components && uniqueN(F2$AnimalNum) == FSB_N$combz_animals, nrow(F2))
gate("6-F2", "CombZ recomposed from signed_z by the frozen producer rule = stored CombZ within 1e-12 (all 117)",
     attr(F2, "recompose_max_abs_diff") <= FSB_TOL, attr(F2, "recompose_max_abs_diff"))
npres <- F2[, .(n = n_components_present[1]), by = AnimalNum][, table(factor(n, levels = names(FSB_N$n_components_present)))]
gate("6-F2", paste0("n_components_present from the frozen rule = stored (all 117); animals with ",
                    paste(names(FSB_N$n_components_present), "of 6:", FSB_N$n_components_present, collapse = ", ")),
     attr(F2, "n_present_mismatch") == 0L && identical(as.integer(npres), unname(FSB_N$n_components_present)),
     paste(F2[present == FALSE, paste(AnimalNum, component)], collapse = ","))
gate("6-F2", "signed_z = stored component value for every animal x component (identical doubles)", {
  ok <- TRUE; for (k in CST$COMBZ_COMPONENTS) { a <- F2[component == k][match(CZ$AnimalNum, AnimalNum), signed_z]; b <- as.numeric(CZ[[k]])
    ok <- ok && identical(is.na(a), is.na(b)) && all(a[!is.na(a)] == b[!is.na(b)]) }; ok })
gate("6-F2", "z = direction x signed_z; direction -1 exactly for the inverted components", F2[present == TRUE, all(z == direction * signed_z)] &&
       identical(F2[, unique(direction), by = component][match(CST$COMBZ_COMPONENTS, component), V1], ifelse(CST$COMBZ_COMPONENT_META$sign_inverted, -1L, 1L)))
gate("6-F2", "classification reproduced: for SIS animals below_threshold <=> Group SUS (0 mismatches)",
     F2[Group != "CON", all(below_threshold == (Group == "SUS"))], F2[Group != "CON" & below_threshold != (Group == "SUS"), paste(unique(AnimalNum), collapse = ",")])
F2b <- fsb_combz_definition_table(DEF, COMPDEF, F2, normalizePath(SRC[["combz_table"]], winslash = "/", mustWork = FALSE), FSB_INPUT_SHA256[["combz_table"]])
gate("6-F2", "F2b: 6 component rows; n_present = 117 - n_missing", nrow(F2b) == 6L && all(F2b$n_present + F2b$n_missing == FSB_N$combz_animals))
PH_LABEL <- paste(FSB_POSTHOC$stage_dir, FSB_POSTHOC$run_name, sep = "/")
PT <- lapply(FSB_POSTHOC_TABLES, function(rel) fsb_posthoc_copy(PH_DIR, rel, VPH$check[file == rel, observed_sha256], PH_LABEL))
gate_rows(fsb_posthoc_gates(PT, PH_DIR))
D1 <- fsb_posthoc_copy(S14_DIR, FSB_STAGE14$files[["domain_heatmap"]], FSB_STAGE14$sha256[["domain_heatmap"]],
                       paste0(FSB_STAGE14$root_rel, " (Stage 14 run promoted 2026-10-06c)"))
d1n <- D1[, .N, by = .(heatmap_id, variant)]
gate("6-D1", "D1: 504 rows = 3 heatmaps x 3 variants (all_blocks 84, cc1_a1 42, cc1_l1 42 rows per variant); 468 estimated, 36 not_shown",
     nrow(D1) == 504L && nrow(d1n) == 9L && setequal(d1n$variant, FSB_D1_VARIANTS) && all(d1n$N == FSB_D1_ROWS[d1n$heatmap_id]) &&
       sum(D1$cell_status == "estimated") == 468L && sum(D1$cell_status == "not_shown") == 36L, paste(d1n[, paste(heatmap_id, variant, N)], collapse = "; "))
gate("6-D1", "D1 display text free of banned wording (window, row label and description, tier)", { fsb_check_display(D1, FSB_D1_DISPLAY_COLS, "D1"); TRUE })
LABS <- fsb_hmm_state_labels(fread(file.path(S14_DIR, FSB_STAGE14$files[["state_display_labels"]])))
gate("6-M", "HMM display labels: the 4 states of the 10-minute HMM with unique semantic and display labels", nrow(LABS) == 4L, paste(LABS$DisplayState, collapse = "; "))
M1 <- fsb_hmm_transition_difference(fread(file.path(S14_DIR, FSB_STAGE14$files[["transition_differences"]])), LABS)
gate("6-M", "M1: 96 rows = 2 sexes x 4 x 4 states x 3 contrasts; DeltaProbability = comparison - reference mean within 1e-12",
     nrow(M1) == 96L && attr(M1, "max_abs_recon_diff") <= FSB_TOL, paste(nrow(M1), attr(M1, "max_abs_recon_diff")))
M2 <- fsb_hmm_transition_flows(fread(file.path(S14_DIR, FSB_STAGE14$files[["transition_means"]])), LABS)
gate("6-M", "M2: 96 rows = 2 sexes x 3 groups x 16 transitions; the 8 largest shown per sex x group",
     nrow(M2) == 96L && all(M2[, .N, by = .(Sex, Group)]$N == 16L) && all(M2[, sum(shown), by = .(Sex, Group)]$V1 == FSB_HMM_FLOW_TOP), nrow(M2))
M3 <- fsb_hmm_dwell_distribution(fread(DWELL), LABS)
gate("6-M", "M3: 2 sexes x 2 phases x 3 groups x 4 states x 7 bins; counts sum to the 3,346 records; fractions sum to 1 per group",
     nrow(M3) == 2L * 2L * 3L * 4L * 7L && sum(M3$n_records) == FSB_HMM_DWELL$n_records && attr(M3, "n_records_total") == FSB_HMM_DWELL$n_records &&
       max(abs(M3[, sum(fraction), by = .(Sex, Phase, Group, State)]$V1 - 1)) <= FSB_TOL, paste(nrow(M3), sum(M3$n_records)))
M4 <- fsb_hmm_resolution_sensitivity(fread(file.path(S14_DIR, FSB_STAGE14$files[["resolution_sensitivity"]])))
gate("6-M", "M4: 24 rows = 2 HMM resolutions x 2 sexes x 2 phases x 3 contrasts, behavioural state architecture only",
     nrow(M4) == 24L && uniqueN(M4$resolution) == 2L && all(M4$Domain == "Behavioral state architecture") &&
       nrow(unique(M4[, .(resolution, Sex, Phase, contrast)])) == 24L, paste(nrow(M4), paste(unique(M4$Domain), collapse = ",")))
E1 <- fsb_posthoc_copy(S09_DIR, FSB_STAGE09$file, FSB_STAGE09$sha256, FSB_STAGE09$root_rel)
gate_rows(fsb_stage09_gates(E1, S09, A2, fread(S09_IN, colClasses = list(character = "AnimalNum"))))
E3 <- fsb_posthoc_copy(S09_DIR, FSB_STAGE09_COVERAGE$file, FSB_STAGE09_COVERAGE$sha256, FSB_STAGE09_COVERAGE$root_rel)
gate_rows(fsb_coverage_gates(E3, S09_COV))
P29C_LABEL <- paste("pipeline", FSB_POSTHOC29C$stage_dir, FSB_POSTHOC29C$run_name, sep = "/")
P29C <- lapply(FSB_POSTHOC29C_TABLES, function(rel) fsb_posthoc_copy(P29C_DIR, rel, VP29C$check[file == rel, observed_sha256], P29C_LABEL))
gate_rows(fsb_posthoc29c_gates(P29C, P29C_DIR, A2))
DA <- fsb_design_ages(TL, A4, V29T)
gate_rows(fsb_design_age_gates(DA, TL))
TABLES <- c(list(F1_con_cage_means = F1, F1b_con_reference_means = F1b, F2_combz_components = F2, F2b_combz_definition = F2b), PT,
            list(D1_domain_heatmap_cells = D1, M1_hmm_transition_difference = M1, M2_hmm_transition_flows = M2,
                 M3_hmm_dwell_distribution = M3, M4_hmm_resolution_sensitivity = M4, E1_movement_combz_spearman_by_sex = E1),
            P29C, list(E3_early_window_coverage = E3, T1_design_ages = DA$T1, T1b_design_ages_by_animal = DA$T1b))
gate("6-tables", "display text free of banned wording (labels, F2b text)", { fsb_check_display(F1, c("measure_label", "unit"), "F1")
  fsb_check_display(F1b, c("measure_label", "unit"), "F1b"); TRUE })

# ---------------------------------------------------------------- 7. provenance and write (immutable)
STATUS <- if (DRY) FSB_STATUS_DRY else FSB_STATUS_REAL
GEN_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
GT <- rbindlist(GATES)
pk <- c("data.table", "jsonlite", "digest", "tibble", "readxl")
H <- fsb_provenance(list(
  bundle_id = bundle_id, status = STATUS, generated_at = GEN_AT, generator = "Analysis/16c_figure_support_bundle.R",
  mmm_git_commit = commit, mmm_branch = branch, mmm_code_clean = cln$ok, run_mode = MODE,
  mmm_code_sha256 = paste(names(CODE_SHA), CODE_SHA, sep = "=", collapse = "; "),
  stage29_bundle_id = FSB_STAGE29$bundle_id, stage29_bundle_manifest_sha256 = V29$manifest_sha256,
  stage29_bundle_dir = normalizePath(B29, winslash = "/", mustWork = FALSE),
  combz_table = normalizePath(SRC[["combz_table"]], winslash = "/", mustWork = FALSE), combz_table_sha256 = FSB_INPUT_SHA256[["combz_table"]],
  combz_component_definition_sha256 = FSB_INPUT_SHA256[["combz_component_definition"]],
  combz_classification_thresholds_sha256 = FSB_INPUT_SHA256[["combz_classification_thresholds"]],
  labels_sus_sha256 = FSB_INPUT_SHA256[["sus_list"]], labels_con_sha256 = FSB_INPUT_SHA256[["con_list"]],
  combz_definition_code = paste0(FSB_COMBZ_CODE$path, " (parsed; only the ", length(FSB_COMBZ_CONSTANTS), " definition constants and the ",
                                 length(FSB_COMBZ_RULE), " composite-rule lines are evaluated; never executed)"),
  combz_definition_code_git_blob = blob, combz_definition_id = CST$COMBZ_DEFINITION_ID, combz_reference_population_id = CST$COMBZ_REFERENCE_POP_ID,
  combz_upstream_correction_id = CST$COMBZ_CORRECTION_ID, combz_definition_doc = FSB_COMBZ_CODE$doc,
  posthoc_run_dir = normalizePath(PH_DIR, winslash = "/", mustWork = FALSE), posthoc_run_commit = FSB_POSTHOC$run_commit,
  posthoc_output_manifest_sha256 = VPH$manifest_sha256, posthoc_registry_sha256 = FSB_POSTHOC$registry_sha256,
  posthoc_registry_version = FSB_POSTHOC$registry_version, posthoc_tier = paste(FSB_POSTHOC$tier, "(decision basis USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION, 2026-09-30)"),
  posthoc_tables = paste(sprintf("%s = %s/%s", names(FSB_POSTHOC_TABLES), PH_LABEL, FSB_POSTHOC_TABLES), collapse = "; "),
  stage14_source_dir = normalizePath(S14_DIR, winslash = "/", mustWork = FALSE),
  stage14_promotion_record = paste0(FSB_STAGE14$record, " (sha256 ", s30fb_sha(S14_RECORD), "; Stage 14 not re-run for this bundle)"),
  stage14_tables = paste(sprintf("%s = %s (sha256 %s)", names(FSB_STAGE14$files), FSB_STAGE14$files, FSB_STAGE14$sha256), collapse = "; "),
  hmm_dwell_source = paste0(normalizePath(DWELL, winslash = "/", mustWork = FALSE), " (sha256 ", FSB_HMM_DWELL$sha256, ")"),
  hmm_resolution = paste0(FSB_HMM_RESOLUTION, " for M1-M3; M4 carries both resolutions"),
  stage09_table = paste0(normalizePath(S09, winslash = "/", mustWork = FALSE), " (sha256 ", s09_sha, "; copied verbatim as E1)"),
  stage09_coverage_table = paste0(normalizePath(S09_COV, winslash = "/", mustWork = FALSE), " (sha256 ", s09_cov_sha, "; copied verbatim as E3)"),
  posthoc29c_run_dir = normalizePath(P29C_DIR, winslash = "/", mustWork = FALSE), posthoc29c_run_commit = FSB_POSTHOC29C$run_commit,
  posthoc29c_output_manifest_sha256 = VP29C$manifest_sha256, posthoc29c_registry_sha256 = FSB_POSTHOC29C$registry_sha256,
  posthoc29c_tier = paste(FSB_POSTHOC29C$tier, "(decision basis USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION, 2026-10-12); adds no test"),
  timeline_workbook = paste0(normalizePath(TIMELINE, winslash = "/", mustWork = FALSE), " (sheet ", FSB_TIMELINE$sheet, ")"),
  timeline_workbook_sha256 = tl_sha, design_age_unit = FSB_DESIGN_UNIT,
  design_steps = paste(sprintf("%s: %s", FSB_DESIGN_EVENTS$event_id, FSB_DESIGN_EVENTS$definition), collapse = "; "),
  combz_recompose_max_abs_diff = s30fb_num_chr(attr(F2, "recompose_max_abs_diff")),
  con_mean_of_cage_means_max_abs_diff = s30fb_num_chr(max(CK$abs_diff)),
  group_rule = "Group = canonical CombZ-table outcome_group, gated = list rule (CON if con list, else SUS if sus list, else RES) and = ebb_v101 A4 / B1 Group",
  scientific_recomputation = FSB_SCIENTIFIC_RECOMPUTATION, descriptive_computations = FSB_DESCRIPTIVE_COMPUTATIONS,
  raw_standardisation_columns = "F2 raw_value, reference_mean, reference_sd are NA: the canonical producer carries the upstream component z-scores verbatim (docs/COMBZ_CANONICAL_DEFINITION.md section 4a)",
  value_precision = "doubles written as the shortest decimal (15-17 significant digits) that reads back to the identical double; every CSV re-read and compared",
  tables = paste(sprintf("%s (%d rows)", names(TABLES), vapply(TABLES, nrow, 1L)), collapse = "; "),
  pre_write_gates_passed = fsb_gate_count(GT),
  gate_results = paste0(FSB_GATE_FILE, " (this bundle, listed in 00_manifest): the ", nrow(GT), " pre-write gates; the complete gate table ",
                        "(pre-write, post-write and registration gates) is logs/", bundle_id, "_gate_results.csv beside BUNDLE_REGISTRY.csv, outside the bundle"),
  r_version = R.version.string,
  packages = paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; ")))
IN <- unique(rbindlist(INPUTS), by = c("input", "role"))
W <- fsb_write_bundle(TABLES, OUT_ROOT, bundle_id, H, IN, GT, STATUS, FSB_STAGE29$bundle_id, FSB_INPUT_SHA256[["combz_table"]], commit, GEN_AT,
                      verbatim = c(stats::setNames(file.path(PH_DIR, FSB_POSTHOC_TABLES), names(FSB_POSTHOC_TABLES)),
                                   D1_domain_heatmap_cells = file.path(S14_DIR, FSB_STAGE14$files[["domain_heatmap"]]),
                                   E1_movement_combz_spearman_by_sex = S09, E3_early_window_coverage = S09_COV,
                                   stats::setNames(file.path(P29C_DIR, FSB_POSTHOC29C_TABLES), names(FSB_POSTHOC29C_TABLES))))
gate_rows(W$post_gates)
message("Stage 16c ", MODE, " complete: bundle ", bundle_id, " ", STATUS, " at ", W$dir, " (manifest sha256 ", W$manifest_sha256, ")")
message("  gate log ", W$log, " (", fsb_gate_count(W$gates), " gates passed; sha256 ", W$log_sha256, ")")
for (n in names(TABLES)) message(sprintf("  %-26s %5d rows", n, nrow(TABLES[[n]])))
