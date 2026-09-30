# ================================================================
# Stage 32 - SIS exposure, within-episode adaptation and early prediction beyond Batch (registry v1.0)
# MMMSociability -- Analysis/32_behavior_exposure_adaptation.R
# ================================================================
# Implements exactly docs/STAGE32_REGISTRY_v1.0.md (FROZEN 2026-09-30T22:41:10+0200, commit 0970d32, sha256 cfbd954c...;
# read-only copy with docs/STAGE32_HYPOTHESES_v1.0.csv and REGISTRY_SHA256.txt at analysis_ready/canonical/stage32_registry/v1.0/).
# Tier: POST HOC relative to the original experiment and the earlier plans (decision basis POST_HOC_CONTEXT); registered before
# its own fitting. The frozen Stage 29 / 30 results and families are unchanged.
#
#   0. identity: explicit opt-in, committed clean code, frozen code hashes, registry (S: bytes, repository LF form, sha list,
#      status, read-only, blob unchanged since the freeze), EXPOSURE formulas = the frozen config, package and R versions
#   1. output: run once (no v1.0_* / .tmp_v1.0_* entry in the stage folder), path lengths
#   2. inputs: data version v2 manifest + 24 files, 24 raw_data files, ebb_v101 bundle (manifest, files, FROZEN row),
#      CombZ table, sus / con lists, Stage 09 input, the Stage 29 v1.0.1 release tables used by the gates
#   3. measures (group-blind): canonical stream (Functions/rfid_event_stream.R via Functions/stage30_movement.R), phase windows,
#      board observation intervals from raw records, coverage manifest and the coverage-count gate, window metrics one window
#      at a time (s30mv_window_metrics(full = TRUE, na_if_file_ends_early = FALSE)), A1 of CC1-CC4 = ebb_v101 B1 within 1e-9
#   4. labels and outcome: Group from the lists (checked against ebb_v101 and the CombZ table), CombZ, CombZ_wb; design and
#      population gates; the Stage 09 input
#   5. fits: modules A-E and sensitivities (Functions/stage32_inference.R), Module F and H13 (Functions/stage32_prediction.R)
#   6. post-fit gates: A-exp = frozen EXPOSURE_CC1 and B-exp = frozen EXPOSURE_TR within 1e-6; Module F continuity with the
#      stored Stage 29 section-7 values (1e-9); every registered hypothesis row present; forbidden-word check
#   7. write once: pipeline/32_behavior_exposure_adaptation/v1.0_<commit7>/ {tables, audit, qc_plots, README.txt}, read-only
# Nothing estimated is printed before the write; any failure before the write leaves nothing on S:.
# Mode: exactly `--real`, and MMM_STAGE32_REAL_RUN must equal the HEAD commit7. Synthetic tests: Testing/tests/test_stage32_*.R.
# Run from the MMMSociability repo root:  MMM_STAGE32_REAL_RUN=<commit7> Rscript Analysis/32_behavior_exposure_adaptation.R --real
# ================================================================

MODE_ARGS <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages({ library(data.table); library(digest) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
suppressPackageStartupMessages(source(.pipeline_setup))
for (h in c("project_paths.R", "behavior_analysis_config.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "rfid_canonical_inference.R",
            "stage30_movement.R", "stage30_figure_bundle.R", "stage32_windows.R", "stage32_inference.R", "stage32_prediction.R", "stage32_run.R"))
  source_mmm_helper(h)
MODE <- s32r_parse_mode(MODE_ARGS)                                  # refuses here unless exactly --real
for (p in c("lme4", "lmerTest", "pbkrtest", "clubSandwich", "Matrix", "reformulas", "ggplot2"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Missing package ", p, call. = FALSE)
T_START <- Sys.time(); STARTED <- format(T_START, "%Y-%m-%dT%H:%M:%S%z")
git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE))
git_ok <- function(...) identical(suppressWarnings(system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = FALSE, stderr = FALSE)), 0L)
commit <- git("rev-parse", "HEAD")[1]; branch <- git("branch", "--show-current")[1]
DRIVER <- { fa <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(fa)) normalizePath(fa[1], winslash = "/", mustWork = FALSE) else NA_character_ }
GATES <- list(); INPUTS <- list()
gate <- function(stage, name, passed, detail = "") {
  GATES[[length(GATES) + 1L]] <<- s32r_gate(stage, name, passed, detail)
  message(sprintf("  gate %-12s %-100s %s", stage, substr(name, 1, 100), if (isTRUE(passed)) "PASS" else "FAIL"))
  if (!isTRUE(passed)) stop("Stage 32 gate failed [", stage, "] ", name, ": ", paste(detail, collapse = "; "), call. = FALSE)
  invisible(passed)
}
gate_rows <- function(rows) for (i in seq_len(nrow(rows))) gate(rows$stage[i], rows$gate[i], rows$passed[i], rows$detail[i])
inp <- function(path, role, sha = s30fb_sha(path)) {
  INPUTS[[length(INPUTS) + 1L]] <<- data.table(input = normalizePath(path, winslash = "/", mustWork = FALSE), bytes = as.numeric(file.size(path)), sha256 = sha, role = role)
  invisible(sha) }
tick <- function(what) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), what))
message("Stage 32 ", MODE, " (registry v", S32_REGISTRY$version, ") at commit ", commit)

# ---------------------------------------------------------------- 0. identity
OPTIN <- Sys.getenv(S32_OPTIN_ENV, "")
gate("0-identity", paste0("explicit REAL opt-in: ", S32_OPTIN_ENV, " = HEAD commit7"), grepl("^[0-9a-f]{40}$", commit) && identical(OPTIN, substr(commit, 1, 7)), OPTIN)
CODE_FILES <- c("Analysis/32_behavior_exposure_adaptation.R", "Functions/stage32_windows.R", "Functions/stage32_inference.R", "Functions/stage32_prediction.R",
                "Functions/stage32_run.R", names(S32_FROZEN_CODE_SHA256), "Functions/behavior_analysis_config.R", "Functions/behavioral_dynamics_helpers.R",
                "Functions/project_paths.R", "Analysis/_pipeline_setup.R", S32_REGISTRY$repo_path, S32_REGISTRY$repo_csv)
cln <- s30fb_code_clean(git("status", "--porcelain", "--untracked-files=all", "--", CODE_FILES), git("ls-files", "--", CODE_FILES), CODE_FILES)
gate("0-identity", "MMM files used are committed and clean (git status --untracked-files=all / ls-files)", cln$ok, if (nzchar(cln$detail)) cln$detail else commit)
gate("0-identity", "this is the committed Analysis/32_behavior_exposure_adaptation.R",
     identical(s30fb_norm_path(DRIVER), s30fb_norm_path(file.path(MMM_REPO_ROOT, "Analysis", "32_behavior_exposure_adaptation.R"))), DRIVER)
CODE_SHA <- vapply(CODE_FILES, function(f) s30fb_sha(file.path(MMM_REPO_ROOT, f)), "")
for (f in names(S32_FROZEN_CODE_SHA256)) gate("0-identity", paste("frozen code unchanged:", f), identical(CODE_SHA[[f]], S32_FROZEN_CODE_SHA256[[f]]), CODE_SHA[[f]])
CFG <- MMM_BEHAVIOR_CONFIG
gate("0-identity", "config analytic constant bout_criterion_s = 39.3970988275363", identical(CFG$metrics$fragmentation$bout_criterion_s, S32_BOUT_CRITERION_S))
fsub <- function(f) gsub("AnimalID", "AnimalNum", f)
gate("0-identity", "A-exp formula = frozen EXPOSURE_CC1 (config), rank 8", identical(S32I_FORMULAS$A_EXP, fsub(CFG$models$EXPOSURE_CC1$formula)) &&
       identical(S32I_RANK[["A_EXP"]], as.integer(CFG$models$EXPOSURE_CC1$expected_rank)), S32I_FORMULAS$A_EXP)
gate("0-identity", "B-exp formulas = frozen EXPOSURE_TR (config; rate: crossing_rate_animal_term), rank 20",
     identical(S32I_FORMULAS$B_EXP, fsub(CFG$models$EXPOSURE_TR$formula)) &&
       identical(S32I_FORMULAS$B_EXP_RATE, fsub(sub("(1 | AnimalID)", CFG$models$EXPOSURE_TR$crossing_rate_animal_term, CFG$models$EXPOSURE_TR$formula, fixed = TRUE))) &&
       identical(S32I_RANK[["B_EXP"]], as.integer(CFG$models$EXPOSURE_TR$expected_rank)), S32I_FORMULAS$B_EXP_RATE)
gate("0-identity", "registry freeze commit is an ancestor of HEAD", git_ok("merge-base", "--is-ancestor", S32_REGISTRY$freeze_commit, "HEAD"), S32_REGISTRY$freeze_commit)
for (rp in c(S32_REGISTRY$repo_path, S32_REGISTRY$repo_csv)) {
  b_head <- git("rev-parse", paste0("HEAD:", rp))[1]; b_frz <- git("rev-parse", paste0(S32_REGISTRY$freeze_commit, ":", rp))[1]
  gate("0-identity", paste("file at HEAD = the file committed at the freeze (git blob):", rp), grepl("^[0-9a-f]{40}$", b_head) && identical(b_head, b_frz), paste(b_head, b_frz))
}
ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
REG_DIR <- file.path(AR, S32_REGISTRY$canonical_rel)
gate_rows(s32r_registry_gates(REG_DIR, file.path(MMM_REPO_ROOT, S32_REGISTRY$repo_path), file.path(MMM_REPO_ROOT, S32_REGISTRY$repo_csv)))
inp(file.path(REG_DIR, S32_REGISTRY$file), "registry (S:, read-only)"); inp(file.path(REG_DIR, S32_REGISTRY$csv), "hypothesis table (S:, read-only)")
inp(file.path(REG_DIR, S32_REGISTRY$sha_file), "registry sha256 list (S:)")
HYP <- s32r_hypotheses(file.path(REG_DIR, S32_REGISTRY$csv))
gate("0-identity", "frozen hypothesis table: 13 rows; families, models = the implemented map", identical(HYP$FamilyID, S32_HYP_MAP$FamilyID) && identical(HYP$Model, S32_HYP_MAP$Model),
     paste(HYP$HypothesisID, HYP$FamilyID, HYP$Model, collapse = "; "))
gate_rows(s32r_package_gates())

# ---------------------------------------------------------------- 1. output location (run once)
OUT_ROOT <- file.path(AR, "pipeline", S32_STAGE_DIR); RUN <- s32r_run_name(commit); RUN_DIR <- file.path(OUT_ROOT, RUN)
blk <- s32r_blocking_entries(OUT_ROOT)
gate("1-output", "no earlier v1.0_* output or .tmp_v1.0_* staging folder in the Stage 32 folder (run once)", !length(blk), paste(blk, collapse = ","))
rel_out <- c(paste0("tables/", S32_TABLES, ".csv"), paste0("audit/", c(S32_AUDIT, "output_manifest"), ".csv"), "qc_plots/qc_light_rate_by_phase.png", S32_README)
longest <- max(nchar(file.path(OUT_ROOT, paste0(".tmp_", RUN), rel_out)))
gate("1-output", "every output path shorter than 250 characters (Windows R path limit)", longest < 250, longest)

# ---------------------------------------------------------------- 2. inputs
DV2 <- file.path(ROOT, "MMMSociability", "data_versions", S32_V2_DIR); PRE_DIR <- file.path(DV2, "preprocessed_data")
RAW <- file.path(ROOT, "MMMSociability", "raw_data")
man2 <- file.path(DV2, "MANIFEST_SHA256.csv")
gate("2-inputs", "data version v2 manifest SHA-256 (bb33a111)", identical(s30fb_sha(man2), S32_INPUT_SHA256[["v2_manifest"]]), man2); inp(man2, "data_version_v2_manifest")
mv2 <- fread(man2, colClasses = "character")
pre_found <- list.files(PRE_DIR, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$")
gate("2-inputs", "data version v2: 24 files on disk = manifest", nrow(mv2) == S32_V2_N_FILES && setequal(pre_found, mv2$file), paste(length(pre_found), "files"))
PRE_FILES <- file.path(PRE_DIR, mv2$file); pre_sha <- vapply(PRE_FILES, s30fb_sha, "", USE.NAMES = FALSE)
gate("2-inputs", "data version v2: every file hash = manifest sha256_v2", all(pre_sha == mv2$sha256_v2), paste(mv2$file[pre_sha != mv2$sha256_v2], collapse = ","))
for (i in seq_along(PRE_FILES)) inp(PRE_FILES[i], "preprocessed_v2", pre_sha[i])
RAW_FILES <- file.path(RAW, names(S32_RAW_SHA256))
raw_sha <- vapply(RAW_FILES, function(p) if (file.exists(p)) s30fb_sha(p) else NA_character_, "", USE.NAMES = FALSE)
gate("2-inputs", "raw_data AnimalPos files (seed source and board records): 24 hashes = Stage 29 run_inputs", !anyNA(raw_sha) && all(raw_sha == S32_RAW_SHA256))
for (i in seq_along(RAW_FILES)) inp(RAW_FILES[i], "raw_data_animalpos", raw_sha[i])
B29_ROOT <- file.path(AR, "canonical", "behavior_bundle"); B29 <- file.path(B29_ROOT, S32_EBB$bundle_id)
V29 <- s30fb_verify_stage29_bundle(B29, file.path(B29_ROOT, "BUNDLE_REGISTRY.csv"), S32_EBB[c("bundle_id", "manifest_sha256")]); gate_rows(V29$gates)
B1_FILE <- file.path(B29, S32_EBB$b1_file)
gate("2-inputs", "ebb_v101 B1_animal_longitudinal.csv SHA-256 = pinned = manifest entry", identical(s30fb_sha(B1_FILE), S32_EBB$b1_sha256) &&
       isTRUE(V29$check[file == S32_EBB$b1_file, ok]), B1_FILE)
inp(file.path(B29, "00_manifest.csv"), "ebb_v101 manifest", V29$manifest_sha256); inp(B1_FILE, "ebb_v101 B1 (A1 gate, label check)")
COMBZ <- file.path(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv")
LIST_DIR <- dirname(dirname(ROOT)); SUS_FILE <- file.path(LIST_DIR, "sus_animals.csv"); CON_FILE <- file.path(LIST_DIR, "con_animals.csv")
S09_FILE <- file.path(AR, "pipeline", "09_early_prediction", "10min", "tables", "model_ladder_input.csv")
REL <- file.path(AR, "pipeline", "29_canonical_behavior_releases", S32_S29_RELEASE, "tables")
S29_EXPO <- file.path(REL, "exposure_estimates.csv"); S29_S09 <- file.path(REL, "stage09_cv_sensitivities.csv")
for (it in list(list(COMBZ, "combz_table"), list(SUS_FILE, "sus_list"), list(CON_FILE, "con_list"), list(S09_FILE, "stage09_input"),
                list(S29_EXPO, "stage29_exposure_estimates"), list(S29_S09, "stage29_stage09_cv_sensitivities"))) {
  sha <- if (file.exists(it[[1]])) s30fb_sha(it[[1]]) else NA_character_
  gate("2-inputs", paste0(it[[2]], " SHA-256 = frozen (", substr(S32_INPUT_SHA256[[it[[2]]]], 1, 8), ")"), identical(sha, S32_INPUT_SHA256[[it[[2]]]]), it[[1]])
  inp(it[[1]], it[[2]], sha)
}
gate("2-inputs", "Stage 29 v1.0.1 release tables used by the gates are read-only", all(file.access(c(S29_EXPO, S29_S09), 2L) != 0L))

# ---------------------------------------------------------------- 3. measures (group-blind)
tick("3 measures")
pre <- s30mv_read_preprocessed(PRE_FILES)
gate("3-measures", "no Group column in the preprocessed data", !"Group" %in% names(pre))
st <- s30mv_build_stream(pre, RAW)
gate("3-measures", "seed step read only the hash-gated raw files", all(s30fb_norm_path(unique(st$seeds$raw_file)) %in% s30fb_norm_path(RAW_FILES)),
     paste(length(unique(st$seeds$raw_file)), "raw files"))
FW <- mmm_evs_windows(pre)                                           # stops unless 0 <= t0 - 18:30 < 3600 s in every file
gate("3-measures", "24 SourceFiles with an 18:30 anchor", nrow(FW) == 24L && all(format(FW$active_start, "%H:%M:%S", tz = "UTC") == "18:30:00"))
PW <- s32w_phase_windows(FW)
KEPT <- s32w_read_blocks(PRE_FILES)
AN_SYS <- unique(merge(st$pos[is_seed == FALSE, .(SourceFile, AnimalNum, System)], st$file_span[, .(SourceFile, Batch, CC)], by = "SourceFile"))
gate("3-measures", "111 animals, each in one System per SourceFile and in 4 SourceFiles (CC1-CC4)", uniqueN(AN_SYS$AnimalNum) == S32_POP$n_animals &&
       !anyDuplicated(AN_SYS[, .(SourceFile, AnimalNum)]) && all(AN_SYS[, .N, by = AnimalNum]$N == 4L), paste(uniqueN(AN_SYS$AnimalNum), "animals"))
src_raw <- FW[, .(SourceFile, raw = file.path(RAW, Batch, sub("_preprocessed[.]csv$", ".csv", SourceFile)))]
gate("3-measures", "board records come from the 24 hash-gated raw files", all(s30fb_norm_path(src_raw$raw) %in% s30fb_norm_path(RAW_FILES)))
RR <- rbindlist(lapply(seq_len(nrow(src_raw)), function(i) s32w_read_raw_records(src_raw$raw[i], src_raw$SourceFile[i])))
BOARDS <- s32w_board_intervals(RR, AN_SYS); UNATTR <- attr(BOARDS, "unattributed"); rm(RR)
gate("3-measures", "board observation intervals: every SourceFile x System of the stream has raw records (120 boards)",
     nrow(BOARDS) == nrow(unique(AN_SYS[, .(SourceFile, System)])) && nrow(BOARDS) == 120L,
     paste(nrow(BOARDS), "boards; unattributed raw records:", sum(UNATTR$n_records), paste(UNATTR$SourceFile, UNATTR$labels, collapse = " | ")))
COV <- s32w_coverage(AN_SYS, PW, KEPT, BOARDS)
CNT <- s32w_coverage_counts(COV)
cl <- CNT[in_clean_set == TRUE]
gate("3-coverage", "coverage (10-min tolerance): all 111 animals complete in every one of the 24 clean CC x phase combinations",
     nrow(cl) == S32_POP$n_clean_phase_cc && all(cl$n_animals == 111L) && all(cl$n_complete_primary == 111L),
     paste(cl[n_complete_primary != 111L, paste(CC, phase, n_complete_primary)], collapse = "; "))
ex <- COV[explicit_exclusion == TRUE]
gate("3-coverage", "explicit exclusion B5|sys.1|CC4 from L2: 4 animals x 5 phases, all outside the clean set, board ends before phase end - 10 min",
     nrow(ex) == 20L && setequal(ex$AnimalNum, S32W_EXCLUSION$animals) && !any(ex$in_clean_set) && !any(ex$board_ends_ok_primary),
     paste(nrow(ex), "rows;", paste(unique(ex$phase), collapse = ",")))
nc <- CNT[in_clean_set == FALSE]
gate("3-coverage", "non-clean phases (recorded): complete-if-clean counts", TRUE,
     paste(nc[, sprintf("%s %s kept=%d ends_ok=%d", CC, phase, n_block_kept, n_board_ends_ok_primary)], collapse = "; "))
gate("3-coverage", "strict variant (tolerance 0; recorded): clean animal-phases removed", TRUE,
     paste(sum(COV$complete_primary & !COV$complete_strict), "removed;", paste(COV[complete_primary & !complete_strict, unique(paste(CC, phase))], collapse = ",")))
tick("3 window metrics (one window at a time)")
MET <- s32w_window_metrics(st, PW, S32_BOUT_CRITERION_S)
gate("3-measures", "one canonical window row per coverage row (111 animals x 4 CC x 8 phases)", nrow(MET) == nrow(COV) && nrow(COV) == 3552L, paste(nrow(MET), nrow(COV)))
LONG <- s32w_long(MET, COV)
gate("3-measures", "every used animal-phase has status ok / file_ends_before_window_end and a finite rate",
     LONG[complete_primary == TRUE, all(status %in% c("ok", "file_ends_before_window_end")) && all(is.finite(crossing_rate))])
rec <- LONG[phase == "A1", .(AnimalNum, CC, CageEpisodeID, n_in_cage, n_tracked_mates, hardware_flag, obs_s, n_events, n_bouts, crossing_rate, shared_zone_use,
                             occupancy_dispersion, fragmentation, events_per_bout)]
rec <- merge(rec, LONG[phase == "L1", .(AnimalNum, CC, light_phase_crossing_rate = crossing_rate)], by = c("AnimalNum", "CC"))
REF <- fread(B1_FILE, select = S32W_REF_COLS, colClasses = list(character = c("AnimalNum", "CageEpisodeID")))
REF[, AnimalNum := canonical_animal_id(AnimalNum)]
CMP_A1 <- s32w_compare(rec, REF, c("AnimalNum", "CC"), setdiff(S32W_REF_COLS, c("AnimalNum", "CC")), tol = 1e-9)
gate("3-measures", "recomputed A1 of CC1-CC4 (and L1 rate) = ebb_v101 B1 within 1e-9 (444 rows, every column, identical NA pattern)",
     nrow(rec) == 444L && nrow(REF) == 444L && all(CMP_A1$passed) && all(CMP_A1$n == 444L),
     paste(CMP_A1[, sprintf("%s:%.3g", column, max_abs_diff)], collapse = "; "))

# ---------------------------------------------------------------- 4. labels, outcome, design
tick("4 labels and outcome")
con <- canonical_animal_id(readLines(CON_FILE, warn = FALSE)); con <- unique(con[!is.na(con) & nzchar(con)])
sus <- canonical_animal_id(readLines(SUS_FILE, warn = FALSE)); sus <- unique(sus[!is.na(sus) & nzchar(sus)])
AN <- unique(AN_SYS[, .(AnimalNum, Batch)])
AN[, Group := s30fb_group_from_lists(AnimalNum, sus, con)]
CZ <- fread(COMBZ, select = c("AnimalNum", "Sex", "Batch", "outcome_group", "CombZ"), colClasses = c(AnimalNum = "character"))
CZ[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), Batch = paste0("B", Batch))]
AN <- merge(AN, CZ[, .(AnimalNum, Sex, Batch_cz = Batch, outcome_group, CombZ)], by = "AnimalNum", all.x = TRUE)
gate("4-labels", "111 animals found in the CombZ table with the stream Batch", nrow(AN) == 111L && !anyNA(AN$Sex) && all(AN$Batch == AN$Batch_cz))
gate("4-labels", "Group from the lists (con -> CON, else sus -> SUS, else RES) = CombZ table outcome_group", all(AN$Group == AN$outcome_group))
EBBL <- fread(B1_FILE, select = c("AnimalNum", "CC", "Sex", "Batch", "Group", "CombZ"), colClasses = c(AnimalNum = "character"))
EBBL[, AnimalNum := canonical_animal_id(AnimalNum)]
chkL <- merge(EBBL, AN[, .(AnimalNum, G2 = Group, S2 = Sex, B2 = Batch, C2 = CombZ)], by = "AnimalNum")
gate("4-labels", "Group, Sex, Batch = ebb_v101 B1 (444 rows); CombZ = ebb_v101 within 1e-9",
     nrow(chkL) == 444L && all(chkL$Group == chkL$G2) && all(chkL$Sex == chkL$S2) && all(chkL$Batch == chkL$B2) && max(abs(chkL$CombZ - chkL$C2)) <= 1e-9)
cnt <- AN[, .N, by = .(Sex, Group)]
ok_cnt <- all(vapply(names(S32_POP$by_sex_group), function(s) all(vapply(names(S32_POP$by_sex_group[[s]]), function(g)
  isTRUE(cnt[Sex == s & Group == g, N] == S32_POP$by_sex_group[[s]][[g]]), TRUE)), TRUE))
gate("4-labels", "animals per Sex x Group: CON 12/12, RES 28/25, SUS 18/16 (F/M)", ok_cnt, paste(cnt[order(Sex, Group), paste(Sex, Group, N)], collapse = ", "))
bs <- AN[, .(b = paste(sort(unique(Batch)), collapse = ",")), by = Sex]
gate("4-labels", "Sex nested in Batch (F B3/B4/B6, M B1/B2/B5)", identical(bs[Sex == "Female", b], "B3,B4,B6") && identical(bs[Sex == "Male", b], "B1,B2,B5"))
gate("4-labels", "CombZ finite for all 111 animals", all(is.finite(AN$CombZ)))
AN[, CombZ_wb := NA_real_]; AN[Group != "CON", CombZ_wb := CombZ - mean(CombZ), by = Batch]
gate("4-labels", "CombZ_wb: batch means 0 over the 87 SIS animals", AN[Group != "CON", .N] == 87L && AN[Group != "CON", max(abs(mean(CombZ_wb))), by = Batch][, max(V1)] < 1e-12)
D <- merge(LONG, AN[, .(AnimalNum, Sex, Group, CombZ, CombZ_wb)], by = "AnimalNum")
D[, phase := as.numeric(phase_index)]
D <- s32i_code(D)
data.table::setorder(D, AnimalNum, CC, phase_type, k)
cc_con <- unique(D[phase == 0 & phase_type == "active", .(CC, CageEpisodeID, conCage, Group, Sex, Batch, AnimalNum)])
cons <- cc_con[conCage == 1, .(n = .N, n_con = sum(Group == "CON"), Batch = Batch[1], Sex = Sex[1]), by = .(CC, CageEpisodeID)]
gate("4-design", "CON-only cage episodes: 6 per CC (3 per sex, one per batch), 4 CON animals each; no cage mixes CON and SIS",
     nrow(cons) == 24L && all(cons$n == 4L & cons$n_con == 4L) && all(cons[, .N, by = .(CC, Sex)]$N == 3L) && all(cc_con[Group == "CON", conCage] == 1) &&
       all(cc_con[Group != "CON", conCage] == 0))
a1 <- D[phase_type == "active" & k == 1L & CC == "CC1"]
gate("4-design", "A1 of CC1: 111 rows; non-finite overlap = OQ770, OQ771 (H02 n = 109, H04 n = 85)", nrow(a1) == 111L &&
       setequal(a1[!is.finite(shared_zone_use), AnimalNum], S32_POP$overlap_nonfinite_A1_CC1), paste(a1[!is.finite(shared_zone_use), AnimalNum], collapse = ","))
gate("4-design", "C rows (A1-A4 CC1-CC3, A1-A2 CC4) = 1554; D rows = 1332; E rows (L1-L3 CC1-CC3, L1 CC4) = 1110",
     nrow(s32i_sel$C_ROWS(D)[complete_primary == TRUE]) == S32_POP$n_c_rows && nrow(s32i_sel$D_ROWS(D)[complete_primary == TRUE]) == S32_POP$n_d_rows &&
       nrow(s32i_sel$E_ROWS(D)[complete_primary == TRUE]) == S32_POP$n_e_rows)
S09 <- fread(S09_FILE, select = c("AnimalNum", "Group", "Sex", "Movement_mean", "Movement_rmssd", "Entropy_acf1", "outcome"), colClasses = c(AnimalNum = "character"))
S09[, AnimalNum := canonical_animal_id(AnimalNum)]
F9 <- merge(S09, a1[, .(AnimalNum, Batch = as.character(Batch), CageEpisodeID, sex_c, G_lists = Group, Sex_lists = Sex, CombZ)], by = "AnimalNum")
gate("4-design", "Stage 09 input: 111 animals; Group = lists; Sex = CombZ table; outcome = CombZ within 1e-9; features finite",
     nrow(F9) == 111L && nrow(S09) == 111L && all(F9$Group == F9$G_lists) && all(F9$Sex == F9$Sex_lists) && max(abs(F9$outcome - F9$CombZ)) <= 1e-9 &&
       all(is.finite(as.matrix(F9[, .(Movement_mean, Movement_rmssd, Entropy_acf1)]))))
F9[, CombZ := outcome]; data.table::setorder(F9, AnimalNum)
F9_SIS <- F9[Group != "CON"]
gate("4-design", "Module F SIS population: 87 animals (F 28 RES / 18 SUS, M 25 / 16), 24 CC1 cages, 6 batches",
     nrow(F9_SIS) == 87L && uniqueN(F9_SIS$CageEpisodeID) == 24L && uniqueN(F9_SIS$Batch) == 6L &&
       identical(F9_SIS[, .N, by = .(Sex, Group)][order(Sex, Group), N], c(28L, 18L, 25L, 16L)))

# ---------------------------------------------------------------- 5. fits (nothing estimated is printed)
FIT_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"); tick("5 fits: modules A-E and sensitivities")
R <- s32i_run_all(D, s32i_jobs(), progress = function(id, status, secs) message(sprintf("  fit %-44s %-8s %7.1f s", id, status, secs)))
tick("5 Module F (SIS: LOCO / LOAO / LOBO / grouped 5-fold, 2 x 1000 permutations; all animals: estimation only)")
FS <- s32p_module(F9_SIS, "SIS", perms = TRUE)
FA <- s32p_module(F9, "ALL", perms = FALSE)
FT <- s32p_tests(FS)
H13 <- s32p_h13(F9_SIS)
tick("5 fits done")

# ---------------------------------------------------------------- 6. post-fit gates
FROZEN <- fread(S29_EXPO, colClasses = list(character = c("sex")))
RP <- s32i_repro(R$estimates, FROZEN, s32i_repro_pairs())
RP <- merge(RP, s32i_repro_pairs()[, .(model_id, estimand, gate)], by = c("model_id", "estimand"), all.x = TRUE)
gate("6-reproduction", "A-exp = frozen EXPOSURE_CC1 within 1e-6 (4 measures x SIS - CON, x sex; estimate, SE, KR df, CI)",
     RP[gate == "A-exp = EXPOSURE_CC1", .N == 40L && all(passed)], RP[gate == "A-exp = EXPOSURE_CC1", sprintf("max |diff| %.3g", max(abs_diff, na.rm = TRUE))])
gate("6-reproduction", "B-exp = frozen EXPOSURE_TR within 1e-6 (4 measures x CC1-CC4 SIS - CON and x sex; estimate, SE, KR df, CI)",
     RP[gate == "B-exp = EXPOSURE_TR", .N == 160L && all(passed)], RP[gate == "B-exp = EXPOSURE_TR", sprintf("max |diff| %.3g", max(abs_diff, na.rm = TRUE))])
S29CV <- fread(S29_S09)
cont <- merge(S29CV[, .(behaviour_set = model, scheme = ifelse(scheme == "LOCO_CC1_cage", "LOCO", scheme), r2_stage29 = r2)],
              FA$performance[model == "B", .(behaviour_set, scheme, r2)], by = c("behaviour_set", "scheme"))
gate("6-continuity", "Module F all animals, behaviour-only LOAO / LOCO / LOBO R2 = stored Stage 29 section-7 values within 1e-9",
     nrow(cont) == 6L && max(abs(cont$r2 - cont$r2_stage29)) <= 1e-9, sprintf("max |diff| %.3g", max(abs(cont$r2 - cont$r2_stage29))))
gate("6-results", "Module F: 1000 within-Batch and 1000 unrestricted draws per behaviour set", nrow(FS$nulls) == 4000L &&
       all(FS$nulls[, .N, by = .(null, behaviour_set)]$N == 1000L))

# ---------------------------------------------------------------- assemble outputs
EST <- R$estimates; JT <- R$joints
pick <- function(mid, e) EST[model_id == mid & estimand == e]
pickj <- function(mid, e) JT[model_id == mid & estimand == e]
mm <- R$models
MULT <- rbindlist(lapply(seq_len(nrow(S32_HYP_MAP)), function(i) { h <- S32_HYP_MAP[i]; hid <- h$HypothesisID
  if (h$kind == "KR t") { x <- pick(h$model_id, h$estimand); mi <- mm[model_id == h$model_id]
    data.table(HypothesisID = hid, model_id = h$model_id, estimand = h$estimand, estimate = x$estimate, se = x$se, df = x$df, df1 = NA_real_, df2 = NA_real_,
               ci_low = x$ci_low, ci_high = x$ci_high, statistic = x$statistic, statistic_type = if (isTRUE(x$ddf_fallback)) "Satterthwaite t (KR fallback)" else "KR t",
               p_raw = x$p_raw, n = x$n_animals, n_obs = x$n_obs, n_cage_episodes = x$n_cage_episodes, status = x$status, singular = mi$singular,
               ddf_fallback = x$ddf_fallback, failure_reason = x$failure_reason)
  } else if (h$kind == "KR F(3)") { x <- pickj(h$model_id, h$estimand); mi <- mm[model_id == h$model_id]
    data.table(HypothesisID = hid, model_id = h$model_id, estimand = h$estimand, estimate = NA_real_, se = NA_real_, df = NA_real_, df1 = x$df1, df2 = x$df2,
               ci_low = NA_real_, ci_high = NA_real_, statistic = x$F, statistic_type = if (isTRUE(x$ddf_fallback)) "Satterthwaite F (KR fallback)" else "KR F",
               p_raw = x$p_raw, n = x$n_animals, n_obs = x$n_obs, n_cage_episodes = x$n_cage_episodes, status = x$status, singular = mi$singular,
               ddf_fallback = x$ddf_fallback, failure_reason = x$failure_reason)
  } else if (h$kind == "permutation") { x <- FT[hypothesis_id == hid]
    data.table(HypothesisID = hid, model_id = h$model_id, estimand = h$estimand, estimate = x$delta_r2_loco, se = NA_real_, df = NA_real_, df1 = NA_real_, df2 = NA_real_,
               ci_low = NA_real_, ci_high = NA_real_, statistic = NA_real_, statistic_type = "within-Batch permutation (1000 draws)", p_raw = x$p_raw, n = x$n,
               n_obs = x$n, n_cage_episodes = 24L, status = "OK", singular = NA, ddf_fallback = NA, failure_reason = NA_character_)
  } else { x <- H13
    data.table(HypothesisID = hid, model_id = h$model_id, estimand = h$estimand, estimate = x$estimate, se = x$se, df = x$df, df1 = NA_real_, df2 = NA_real_,
               ci_low = x$ci_low, ci_high = x$ci_high, statistic = x$statistic, statistic_type = "CR2 t (Satterthwaite df)", p_raw = x$p_raw, n = x$n,
               n_obs = x$n, n_cage_episodes = x$n_clusters, status = x$status, singular = NA, ddf_fallback = NA, failure_reason = x$failure_reason) } }), fill = TRUE)
gate("6-results", "every registered hypothesis H01-H13 has exactly one result row (OK or FAILED)", nrow(MULT) == 13L && identical(MULT$HypothesisID, sprintf("H%02d", 1:13)) &&
       all(MULT$status %in% c("OK", "FAILED")), paste(MULT[, paste(HypothesisID, status)], collapse = ", "))
HOLM <- s32i_holm(HYP, setNames(MULT$p_raw, MULT$HypothesisID))
MULT <- merge(HOLM[, !"p_raw"], MULT, by = "HypothesisID", sort = FALSE)
MULT[, `:=`(method = ifelse(FamilyID == "none", "none (single secondary test, unadjusted; not a discovery claim)", "Holm"), realised_m = ifelse(FamilyID == "none", NA_integer_, 2L),
            tier = S32_TIER, decision_basis = S32_DECISION_BASIS, registry_sha256 = S32_REGISTRY$sha256)]
MULT[, caveat := fifelse(grepl("^(A_EXP|B_EXP|C_EXP)", model_id), S32I_CAVEAT_CON, fifelse(grepl("_CZ", model_id), S32I_CAVEAT_CZ,
                  fifelse(grepl("^F", model_id), "no individual-level prediction claim unless held-out information beyond Batch (registry section 9)", "")))]
ph <- MULT[, .(hypothesis_id = HypothesisID, family_id = FamilyID, p_holm)]
H13_ROW <- H13[, .(module = "F", metric = "Movement_mean", population = "SIS (87)", window = "Stage 09 early window (A1 of CC1)", variant = "primary", fkey = "H13",
                   direction = "CombZ ~ Batch + Movement_mean + Movement_mean:sex_c", model_id, estimand, L = "Movement_mean:sex_c=1", estimate, se, df, statistic,
                   ci_low, ci_high, p_raw, test, df_method = "Satterthwaite (CR2)", status, failure_reason, hypothesis_id, tier = "secondary", tested = TRUE,
                   ddf_fallback = FALSE, n_obs = n, n_animals = n, n_cage_episodes = n_clusters)]
ESTIMATES <- rbindlist(list(EST[tier != "sensitivity"], H13_ROW), fill = TRUE)
ESTIMATES <- merge(ESTIMATES, ph, by = "hypothesis_id", all.x = TRUE, sort = FALSE)
ESTIMATES[, caveat := fifelse(direction == "SIS - CON" | grepl("^A_DEC", model_id), S32I_CAVEAT_CON, fifelse(grepl("CombZ", direction), S32I_CAVEAT_CZ, ""))]
ESTIMATES[, metric_label := S32I_METRIC_LABEL[metric]]
JOINTS <- merge(JT[tier != "sensitivity"], ph, by = "hypothesis_id", all.x = TRUE, sort = FALSE)
SENS <- s32i_sensitivities(EST, JT)
SENSITIVITIES <- rbindlist(list(SENS$contrasts[, row_type := "contrast"], SENS$joints[, row_type := "joint (no p)"][, p_raw := NA_real_]), fill = TRUE)
SENSITIVITIES[, strict_rows_removed := sum(COV$complete_primary & !COV$complete_strict)]
FMOD <- rbindlist(lapply(c("SIS", "ALL"), function(pop) rbindlist(lapply(c("A", paste0("B|", names(S32P_SETS)), paste0("C|", names(S32P_SETS))), function(k)
  data.table(model_id = paste("F", pop, k, sep = "|"), module = "F", metric = "CombZ", population = if (pop == "SIS") "SIS (87)" else "all RFID (111; estimation only)",
             window = "Stage 09 early window (A1 of CC1)", variant = "OLS inside each training fold",
             formula = switch(substr(k, 1, 1), A = "CombZ ~ Batch", B = paste("CombZ ~", paste(S32P_SETS[[sub("^B[|]", "", k)]], collapse = " + ")),
                              C = paste("CombZ ~ Batch +", paste(S32P_SETS[[sub("^C[|]", "", k)]], collapse = " + "))),
             n_obs = if (pop == "SIS") 87L else 111L, status = "OK")))))
MODELS <- rbindlist(list(mm, FMOD, data.table(model_id = "H13|Movement_mean", module = "F", metric = "CombZ", population = "SIS (87)", window = "Stage 09 early window (A1 of CC1)",
                                               variant = "primary", formula = S32P_H13_FORMULA, n_obs = H13$n, n_cage_episodes = H13$n_clusters, rank = S32P_H13_RANK,
                                               expected_rank = S32P_H13_RANK, status = H13$status, failure_reason = H13$failure_reason)), fill = TRUE)
PERF <- rbindlist(list(FS$performance[, row_type := "performance"], FA$performance[, row_type := "performance (estimation only)"],
                       FT[, row_type := "test (H11 / H12)"]), fill = TRUE)
WL <- merge(LONG, AN[, .(AnimalNum, Sex, Group, Exposure = ifelse(Group == "CON", "CON", "SIS"), CombZ, CombZ_wb)], by = "AnimalNum")
WL[, metric_role := ifelse(phase_type == "light", "light phase: crossing_rate = light-phase position-change rate", "active phase")]
data.table::setorder(WL, Batch, AnimalNum, CC, phase_type, k)
DESC <- s32i_descriptives(WL)
n_sing <- sum(mm$singular %in% TRUE); n_fail <- sum(mm$status == "FAILED"); n_notrun <- sum(mm$status == "NOT_RUN")
n_fb <- sum(c(EST$ddf_fallback, JT$ddf_fallback) %in% TRUE)
pk <- c("lme4", "lmerTest", "pbkrtest", "clubSandwich", "Matrix", "reformulas", "data.table", "digest", "ggplot2")
RUNM <- data.table(
  mode = MODE, run_mode = S32_RUN_MODE, mode_optin = S32_OPTIN_ENV, real_run_optin_value = OPTIN, tier = S32_TIER, decision_basis = S32_DECISION_BASIS,
  registry_version = S32_REGISTRY$version, registry_sha256 = S32_REGISTRY$sha256, hypothesis_table_sha256 = S32_REGISTRY$csv_sha256,
  registry_freeze_commit = S32_REGISTRY$freeze_commit, registry_frozen_at = S32_REGISTRY$frozen_at, registry_dir = normalizePath(REG_DIR, winslash = "/", mustWork = FALSE),
  git_commit = commit, git_branch = branch, driver = DRIVER, code_sha256 = paste(names(CODE_SHA), CODE_SHA, sep = "=", collapse = "; "),
  data_version = S32_V2_DIR, data_manifest_sha256 = S32_INPUT_SHA256[["v2_manifest"]], ebb_bundle_id = S32_EBB$bundle_id, ebb_manifest_sha256 = V29$manifest_sha256,
  out_dir = normalizePath(RUN_DIR, winslash = "/", mustWork = FALSE), n_models = nrow(mm), n_singular = n_sing, n_failed = n_fail, n_not_run = n_notrun,
  n_ddf_fallback_rows = n_fb, status_counts = paste(mm[, .N, by = status][, paste0(status, "=", N)], collapse = "; "),
  n_hypotheses = nrow(MULT), n_gates_before_write = length(GATES) + 1L, strict_rows_removed = sum(COV$complete_primary & !COV$complete_strict),
  seeds = "permutations 20260811 (within-Batch; unrestricted re-seeded); grouped 5-fold 521; RNG Mersenne-Twister / Inversion / Rejection",
  conventions = paste(S32_CONVENTIONS, collapse = " || "),
  output_protection = "files read-only (Sys.chmod 0444 after the rename); directories not ACL-protected (Stage 29 release practice)",
  started_at = STARTED, fitted_at = FIT_AT, finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  elapsed_min = as.numeric(difftime(Sys.time(), T_START, units = "mins")), r_version = R.version.string,
  packages = paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; "))
README <- c(
  "Stage 32 - SIS exposure, within-episode adaptation and early prediction beyond Batch (registry v1.0)",
  paste("tier:", S32_TIER, "; decision basis", S32_DECISION_BASIS), "The frozen Stage 29 and Stage 30 results and families are unchanged.",
  paste("commit", commit, "; branch", branch), paste("registry sha256", S32_REGISTRY$sha256, "(frozen", S32_REGISTRY$frozen_at, "at", S32_REGISTRY$freeze_commit, ")"),
  paste("registry", normalizePath(file.path(REG_DIR, S32_REGISTRY$file), winslash = "/", mustWork = FALSE)),
  paste("data version", S32_V2_DIR, "(manifest", S32_INPUT_SHA256[["v2_manifest"]], "); raw_data seeds and board records; ebb", S32_EBB$bundle_id),
  "run mode REAL (fitted once; a later run is refused while any v1.0_* or .tmp_v1.0_* folder exists here)", paste("generated_at", RUNM$finished_at), "",
  "Tables (tables/):",
  "  coverage_manifest       per animal x CC x phase: the registered coverage rule (a)-(d), explicit exclusion, primary (10 min) and strict completeness",
  "  window_metrics_long     per animal x CC x phase: canonical window metrics (NA unless complete), labels, CombZ, CombZ_wb",
  "  descriptives            Group x Sex x CC x phase n / mean / sd (CON, RES, SUS, pooled SIS) and CON cage means",
  "  models / diagnostics    one row per model: formula, rank, singular, convergence, status, variance components, KR / Satterthwaite fallback",
  "  estimates               L-vector estimates, SE, df, 95% CI; p only for the registered tests (H01-H04, H07-H10, H13); Holm p for the families",
  "  joint_tests             H05 / H06 joint KR F(3) of g_SIS:(c2 + c3 + c4)",
  "  multiplicity            H01-H13: family, estimate, CI, df, statistic, p, Holm p (6 families of m = 2; H13 single, unadjusted)",
  "  sensitivities           registry section 7 (estimates and CIs only; shift in primary SEs and robustness label)",
  "  prediction_performance  Module F R2 by model (A Batch, B behaviour, C Batch + behaviour), scheme, population; Delta R2 = C - A; H11 / H12",
  "  prediction_permutations 1000 within-Batch and 1000 unrestricted permutation draws (SIS; Delta R2_LOCO)",
  "  prediction_heldout      held-out predictions per animal, model, scheme and population", "",
  "Interpretation rules (registry section 9, binding):",
  "  * a Movement x CC or Movement x phase pattern shared by CON and SIS is not described as adaptation to social instability without a supporting",
  "    Exposure interaction (H05-H08); relocation, novelty or repeated handling, and development over about P25-P37 are the alternatives.",
  "  * social-spatial overlap differences may reflect familiarity (CON stays with its partners) against repeated regrouping (SIS); not asserted from plots.",
  paste("  * replication:", S32I_CAVEAT_CON),
  "  * no individual-level prediction claim unless H11 / H12 show held-out information beyond Batch.",
  "  * the direction of every CombZ model is stated: A-cz, C-cz, D (cz) and E (cz) are behaviour ~ CombZ_wb; Module F and H13 are CombZ ~ behaviour.", "",
  "Driver conventions where the registry is silent (also in audit/run_manifest.csv):", paste(" ", S32_CONVENTIONS), "",
  "audit/: input_hashes, gate_results (every pre-write gate), run_manifest, reproduction_gate (A-exp / B-exp vs the frozen Stage 29 rows),",
  "reference_a1_gate (A1 metrics vs ebb_v101), coverage_counts; audit/output_manifest.csv lists every other file (bytes, sha256); every file is read-only (0444).",
  "qc_plots/: lightweight descriptive QC figures (group x phase means with CON cage lines; Module F held-out predictions and permutation null).")
TABLES <- list(coverage_manifest = COV, window_metrics_long = WL, descriptives = DESC, models = MODELS, estimates = ESTIMATES, joint_tests = JOINTS,
               multiplicity = MULT, sensitivities = SENSITIVITIES, diagnostics = R$diagnostics, prediction_performance = PERF,
               prediction_permutations = FS$nulls, prediction_heldout = rbindlist(list(FS$heldout, FA$heldout)))
AUDIT <- list(input_hashes = unique(rbindlist(INPUTS), by = c("input", "role")), gate_results = rbindlist(GATES), run_manifest = RUNM,
              reproduction_gate = RP, reference_a1_gate = CMP_A1, coverage_counts = CNT)
bad <- s32r_forbidden(c(TABLES, AUDIT), README)
gate("6-results", "no forbidden word (registry section 9) in any output table, column name or the README", !length(bad), paste(bad, collapse = "; "))
AUDIT$gate_results <- rbindlist(GATES)
tick("7 QC plots (local) and write")
PNG_DIR <- file.path(normalizePath(tempdir(), winslash = "/"), paste0("s32_qc_", substr(commit, 1, 7)))
unlink(PNG_DIR, recursive = TRUE, force = TRUE)
s32r_render_plots(DESC, rbindlist(list(FS$heldout, FA$heldout)), PERF, FS$nulls, FT, PNG_DIR)

# ---------------------------------------------------------------- 7. write once, read-only
W <- s32r_write_run(OUT_ROOT, RUN, TABLES, AUDIT, README, PNG_DIR)
for (i in seq_len(nrow(W$post_gates))) message(sprintf("  gate %-12s %-100s %s", W$post_gates$stage[i], W$post_gates$gate[i], if (W$post_gates$passed[i]) "PASS" else "FAIL"))
message("Stage 32 REAL complete: ", W$dir, " (output_manifest sha256 ", W$manifest_sha256, "); models ", RUNM$status_counts, "; singular ", n_sing,
        "; ddf fallback rows ", n_fb, "; ", sprintf("%.1f", as.numeric(difftime(Sys.time(), T_START, units = "mins"))), " min")
