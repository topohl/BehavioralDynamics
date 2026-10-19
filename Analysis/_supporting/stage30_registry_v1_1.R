# ================================================================
# Stage 30 registry v1.1: build from the frozen v1.0 and freeze (re-registration on the corrected outcome)
# MMMSociability -- Analysis/_supporting/stage30_registry_v1_1.R
# ================================================================
# Registry v1.1 is the frozen v1.0 (docs/stage30/stage30_registry_v1.0.json, sha256 5c252bf6...; run v1.0_be71e2f) with
# exactly these changes (user decision 2026-10-17: rerun every registered analysis on the corrected workbook; erratum E13):
#   * meta: version 1.1, the re-registration record, the outcome-contact disclosure, companion and freeze times;
#   * populations: the RES / SUS counts (SIS_CC1 by batch; COOKIE_SIS, _F, _M), recomputed from the bound CombZ table;
#   * stage29_release, data$cc1_cc4$gate and the never_in_a_family carry-over note: the config v1.0.2 release (ebb_v102).
# Hypotheses, families, inference, measures, sensitivities and classification rules are not touched (checked).
# It fits nothing and computes no outcome association; the counts are design facts, as in v1.0.
#
# Modes (run from the repository root):
#   --selfcheck  counts from MMM_STAGE30_S29_RELEASE_DIR (e.g. v101_dv2_b2ce507) and the CombZ table in
#                MMM_STAGE30_OUTCOME_TABLE (default: the canonical table) must equal the v1.0 strings exactly; writes nothing.
#   --build      MMM_STAGE30_S29_RELEASE_DIR = the config v1.0.2 release (v102_dv2_<commit7>); the canonical CombZ table
#                must be the release outcome version of the config; writes docs/stage30/stage30_registry_v1.1.json and
#                STAGE30_REGISTRY_v1.1.md (status FROZEN, frozen_at now) into the repository only. Commit both alone next.
#   --freeze     after that commit: copies both to analysis_ready/canonical/stage30_registry/v1.1/ with REGISTRY_SHA256.txt
#                and FREEZE_RECORD.csv (read-only); refuses if the folder exists or the files are not committed and clean.
#                MMM_STAGE30_DECISIONS_FILE = the user-decision record to bind (copied hash only).
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
MODE <- commandArgs(trailingOnly = TRUE)
if (length(MODE) != 1L || !MODE %in% c("--selfcheck", "--build", "--freeze")) stop("Usage: --selfcheck | --build | --freeze", call. = FALSE)
source(file.path(getwd(), "Analysis", "_pipeline_setup.R"))
for (h in c("project_paths.R", "behavior_analysis_config.R", "behavior_config_identity.R")) source_mmm_helper(h)
sha <- function(f) digest::digest(file = f, algo = "sha256")
git <- function(...) system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE)
ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready"); EP <- mmm_endpoint_source_root(ROOT)
V10 <- file.path(MMM_REPO_ROOT, "docs", "stage30", "stage30_registry_v1.0.json")
V11 <- file.path(MMM_REPO_ROOT, "docs", "stage30", "stage30_registry_v1.1.json")
MD11 <- file.path(MMM_REPO_ROOT, "docs", "stage30", "STAGE30_REGISTRY_v1.1.md")
V10_SHA <- "5c252bf699bc0f517c5508854d710bbaa014d4b6fe98d128eb0071bde89dd05e"
CANON <- file.path(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv")
COOKIE <- file.path(ROOT, "MMMSociability", "data_versions", "cookie_postEPM_v1", "animals.csv")
REG_OUT <- file.path(AR, "canonical", "stage30_registry", "v1.1")
if (!identical(sha(V10), V10_SHA)) stop("docs/stage30/stage30_registry_v1.0.json is not the frozen v1.0.", call. = FALSE)
reg <- jsonlite::fromJSON(V10, simplifyVector = FALSE)

# ---------------------------------------------------------------- counts (design facts) from the bound inputs
counts <- function(release_dir, combz_file) {
  w <- fread(file.path(release_dir, "tables", "canonical_window_metrics.csv"), select = c("AnimalNum", "CC", "Batch"),
             colClasses = list(character = "AnimalNum"))
  cz <- fread(combz_file, select = c("AnimalNum", "experimental_condition", "outcome_group"), colClasses = list(character = "AnimalNum"))
  w[, AnimalNum := canonical_animal_id(AnimalNum)]; cz[, AnimalNum := canonical_animal_id(AnimalNum)]
  s <- merge(unique(w[CC == "CC1", .(AnimalNum, Batch)]), cz, by = "AnimalNum")[experimental_condition == "SIS"]
  if (nrow(s) != reg$populations$SIS_CC1$n_animals) stop("SIS CC1 animals: ", nrow(s), ", registry ", reg$populations$SIS_CC1$n_animals, call. = FALSE)
  rs <- function(d, b) sprintf("%d/%d", d[Batch == b & outcome_group == "RES", .N], d[Batch == b & outcome_group == "SUS", .N])
  by_batch <- c(stats::setNames(lapply(paste0("B", 1:6), function(b) rs(s, b)), paste0("B", 1:6)),
                list(total = sprintf("%d/%d", s[outcome_group == "RES", .N], s[outcome_group == "SUS", .N]), note = "RES/SUS counts only"))
  ck <- fread(COOKIE, select = c("AnimalNum", "pop_COOKIE_SIS", "pop_COOKIE_SIS_F", "pop_COOKIE_SIS_M"), colClasses = "character")
  ck[, AnimalNum := canonical_animal_id(AnimalNum)]
  ck <- merge(ck, merge(cz, unique(w[, .(AnimalNum, Batch)]), by = "AnimalNum", all.x = TRUE), by = "AnimalNum", all.x = TRUE)
  pop <- function(flag) { d <- ck[get(flag) == "TRUE"]; if (anyNA(d$outcome_group) || any(d$outcome_group == "CON")) stop(flag, ": CON or unlabelled animal", call. = FALSE); d }
  tot <- function(d) sprintf("RES %d, SUS %d", d[outcome_group == "RES", .N], d[outcome_group == "SUS", .N])
  bat <- function(d, bs) paste(vapply(bs, function(b) paste(b, rs(d, b)), ""), collapse = ", ")
  m <- pop("pop_COOKIE_SIS_M"); b2_sus <- m[Batch == "B2" & outcome_group == "SUS", .N]
  if (b2_sus > 3L) stop("B2 now has ", b2_sus, " SUS males; the v1.0 note on male within-batch information no longer applies: edit by hand.", call. = FALSE)
  list(SIS_CC1 = by_batch,
       COOKIE_SIS = tot(pop("pop_COOKIE_SIS")),
       COOKIE_SIS_F = paste0(tot(pop("pop_COOKIE_SIS_F")), " (", bat(pop("pop_COOKIE_SIS_F"), c("B3", "B4", "B6")), ")"),
       COOKIE_SIS_M = paste0(tot(m), " (", bat(m, c("B2", "B5")), "); within-batch RES-SUS information in males comes mainly from B5 because B2 has only ",
                             b2_sus, " SUS"))
}
current <- function(r) list(SIS_CC1 = r$populations$SIS_CC1$res_sus_counts_by_batch, COOKIE_SIS = r$populations$COOKIE_SIS$group_counts,
                            COOKIE_SIS_F = r$populations$COOKIE_SIS_F$group_counts, COOKIE_SIS_M = r$populations$COOKIE_SIS_M$group_counts)
rel <- Sys.getenv("MMM_STAGE30_S29_RELEASE_DIR")
if (MODE != "--freeze" && !dir.exists(rel)) stop("MMM_STAGE30_S29_RELEASE_DIR is not a Stage 29 release folder: ", rel, call. = FALSE)

if (MODE == "--selfcheck") {
  tab <- Sys.getenv("MMM_STAGE30_OUTCOME_TABLE", CANON)
  got <- counts(rel, tab); want <- current(reg)
  ok <- identical(got$SIS_CC1, want$SIS_CC1) && identical(got[-1], want[-1])
  for (k in names(want)) message(k, ": v1.0 ", paste(unlist(want[[k]]), collapse = " | "), "\n   now ", paste(unlist(got[[k]]), collapse = " | "))
  if (!ok) stop("SELFCHECK FAILED: the counts from these inputs differ from the frozen v1.0 strings.", call. = FALSE)
  message("SELFCHECK PASS: the v1.0 population counts reproduce exactly from ", basename(rel), " and ", tab, " (", sha(tab), ")")
}

if (MODE == "--build") {
  OV <- mmm_ov_resolve(MMM_BEHAVIOR_CONFIG)
  invisible(mmm_ov_verify_table(OV, CANON))
  rm <- fread(file.path(rel, "audit", "run_manifest.csv"), colClasses = "character")
  if (!identical(rm$run_mode, "RELEASE") || !identical(rm$outcome_version, OV$id) || !identical(rm$config_version, MMM_BEHAVIOR_CONFIG$meta$config_version))
    stop("The Stage 29 folder is not the RELEASE run of config v", MMM_BEHAVIOR_CONFIG$meta$config_version, " on outcome version ", OV$id, call. = FALSE)
  if (file.exists(V11) || file.exists(MD11)) stop("docs/stage30 already holds a v1.1 registry; never rebuilt.", call. = FALSE)
  got <- counts(rel, CANON); old <- current(reg)
  ref <- file.path(rel, "tables", "canonical_window_metrics.csv"); ref_sha <- sha(ref)
  bundle <- Sys.getenv("MMM_STAGE30_EBB_ID"); if (!grepl("^ebb_v102_[0-9]{8}_[0-9a-f]{7}$", bundle)) stop("MMM_STAGE30_EBB_ID = the ebb_v102 bundle id", call. = FALSE)
  now <- Sys.time(); n <- reg
  n$meta$version <- "1.1"; n$meta$status <- "FROZEN"; n$meta$prepared <- format(now, "%Y-%m-%d")
  n$meta$supersedes <- paste("stage30_registry_v1.0.json (FROZEN 2026-09-29T11:21:10+0200, sha256", V10_SHA, "; REAL run v1.0_be71e2f on outcome version ov1)")
  n$meta$companion <- "STAGE30_REGISTRY_v1.1.md"
  n$meta$reregistration <- list(
    reason = "erratum E13 (docs/BEHAVIOR_CONFIG_v1.0.0_ERRATA.md): the endpoint workbook was corrected upstream; CombZ changes for every animal and RES/SUS for OR419 and OR434 among the RFID animals",
    decision = "user decision 2026-10-17: rerun every registered analysis on the corrected workbook (scope 'Everything')",
    unchanged = "hypotheses, families, inference, measures, sensitivities, classification rules, populations other than their RES/SUS counts, run rules",
    changed = "meta; populations RES/SUS counts; stage29_release; data$cc1_cc4$gate; multiplicity$never_in_a_family carry-over note",
    outcome_version = OV$id, combz_table_sha256 = OV$combz_table_sha256)
  n$meta$outcome_contact_before_freeze <- paste("the v1.0 REAL screen on the pre-correction outcome (run v1.0_be71e2f, outcome version ov1) was computed",
                                                "and seen; no Stage 30 RES/SUS, Group or CombZ association on the corrected outcome was computed or inspected")
  n$meta$disclosure <- c(n$meta$disclosure, list("re-registered on the corrected outcome after the v1.0 results had been seen (user decision 2026-10-17)"))
  n$meta$frozen_on <- format(now, "%Y-%m-%d"); n$meta$frozen_at <- format(now, "%Y-%m-%dT%H:%M:%S%z")
  n$populations$SIS_CC1$res_sus_counts_by_batch <- got$SIS_CC1
  n$populations$COOKIE_SIS$group_counts <- got$COOKIE_SIS
  n$populations$COOKIE_SIS_F$group_counts <- got$COOKIE_SIS_F
  n$populations$COOKIE_SIS_M$group_counts <- got$COOKIE_SIS_M
  rel_name <- basename(rel)
  n$stage29_release <- list(
    status = "executed before this freeze (corrected-outcome release, user decision 2026-10-17)",
    config = paste("v1.0.2 outcome-version binding (release = ov2_corrected_workbook, CombZ table sha256", OV$combz_table_sha256,
                   "); analytic parent v1.0.0 33d22430...; data version v2_cage_label_correction_2026-09-28 (manifest sha bb33a111...)"),
    stage29_run = paste0("analysis_ready/pipeline/29_canonical_behavior_releases/", rel_name, "/ (RELEASE mode; immutable, read-only)"),
    bundle = bundle,
    preserved = c(reg$stage29_release$preserved, list("config v1.0.1 and the v101_dv2_b2ce507 run behind ebb_v101_20260929_b2ce507 (outcome version ov1)")),
    release_record = "docs/STAGE29_RELEASE_v1.0.2_ov2.md",
    manuscript_facing = "the corrected-outcome release")
  n$data$cc1_cc4$gate <- paste0("Stage 30 recomputation of the canonical window metrics on v2 must equal, on the measure columns (tolerance 1e-9), the Stage 29 ",
                                "config v1.0.2 release table with sha256 ", ref_sha, " (analysis_ready/pipeline/29_canonical_behavior_releases/", rel_name,
                                "/tables/canonical_window_metrics.csv; the measure columns are group-blind and equal those of the v1.0 reference table)")
  nf <- vapply(n$multiplicity$never_in_a_family, function(x) if (is.character(x)) x else "", "")
  k <- grep("manuscript-facing v1.0.1 / data version v2 release (ebb_v101)", nf, fixed = TRUE)
  if (length(k) != 1L) stop("multiplicity$never_in_a_family carry-over note not found once.", call. = FALSE)
  n$multiplicity$never_in_a_family[[k]] <- sub("manuscript-facing v1.0.1 / data version v2 release (ebb_v101)",
                                               paste0("manuscript-facing v1.0.2 / data version v2 / outcome version ov2 release (", bundle, ")"), nf[k], fixed = TRUE)
  # nothing outside the declared nodes changed
  touched <- c("meta", "populations", "stage29_release", "data", "multiplicity")
  for (s in setdiff(names(reg), touched)) if (!identical(reg[[s]], n[[s]])) stop("Section ", s, " changed.", call. = FALSE)
  pp <- setdiff(names(reg$populations), c("SIS_CC1", "COOKIE_SIS", "COOKIE_SIS_F", "COOKIE_SIS_M"))
  if (!identical(reg$populations[pp], n$populations[pp])) stop("A population other than the RES/SUS counts changed.", call. = FALSE)
  if (!identical(reg$data[setdiff(names(reg$data), "cc1_cc4")], n$data[setdiff(names(n$data), "cc1_cc4")]) ||
      !identical(reg$data$cc1_cc4[setdiff(names(reg$data$cc1_cc4), "gate")], n$data$cc1_cc4[setdiff(names(n$data$cc1_cc4), "gate")]))
    stop("A data entry other than cc1_cc4$gate changed.", call. = FALSE)
  if (!identical(reg$multiplicity[setdiff(names(reg$multiplicity), "never_in_a_family")], n$multiplicity[setdiff(names(n$multiplicity), "never_in_a_family")]))
    stop("A multiplicity entry other than the carry-over note changed.", call. = FALSE)
  writeLines(jsonlite::toJSON(n, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"), V11, useBytes = TRUE)
  back <- jsonlite::fromJSON(V11, simplifyVector = FALSE)
  if (!identical(back$hypotheses, reg$hypotheses) || !identical(back$inference, reg$inference)) stop("JSON round trip changed hypotheses or inference.", call. = FALSE)
  line <- function(k) sprintf("| %s | %s | %s |", k, paste(unlist(old[[k]]), collapse = "; "), paste(unlist(got[[k]]), collapse = "; "))
  writeLines(c(
    "# Stage 30 registry v1.1 (companion to stage30_registry_v1.1.json; the JSON is authoritative)", "",
    paste0("**Status.** FROZEN ", n$meta$frozen_at, ". Re-registration of v1.0 (", n$meta$supersedes, ") on the corrected outcome."), "",
    paste("**Reason.**", n$meta$reregistration$reason, "."), paste("**Decision.**", n$meta$reregistration$decision, "."), "",
    paste("**Unchanged.**", n$meta$reregistration$unchanged, "(the v1.0 companion STAGE30_REGISTRY_v1.0.md describes them)."), "",
    "**Changed.**", "",
    paste0("- Outcome: version ", OV$id, ", CombZ table sha256 ", OV$combz_table_sha256, "."),
    paste0("- Stage 29 release: ", rel_name, ", bundle ", bundle, "; reference table sha256 ", ref_sha, "."),
    "- RES/SUS counts:", "", "| population | v1.0 | v1.1 |", "|---|---|---|",
    line("SIS_CC1"), line("COOKIE_SIS"), line("COOKIE_SIS_F"), line("COOKIE_SIS_M"), "",
    paste("**Outcome contact before this freeze.**", n$meta$outcome_contact_before_freeze, ".")), MD11, useBytes = TRUE)
  message("Built ", V11, " (", sha(V11), ") and ", MD11, ". Commit both ALONE, then run --freeze.")
}

if (MODE == "--freeze") {
  for (f in c(V11, MD11)) if (!file.exists(f)) stop("Missing ", f, "; run --build first.", call. = FALSE)
  dirty <- git("status", "--porcelain", "--untracked-files=all", "--", "docs/stage30")
  if (length(dirty)) stop("docs/stage30 is not committed and clean:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  if (dir.exists(REG_OUT)) stop("Frozen v1.1 registry folder exists (immutable): ", REG_OUT, call. = FALSE)
  rel <- Sys.getenv("MMM_STAGE30_S29_RELEASE_DIR"); dec <- Sys.getenv("MMM_STAGE30_DECISIONS_FILE")
  if (!dir.exists(rel) || !file.exists(dec)) stop("Set MMM_STAGE30_S29_RELEASE_DIR and MMM_STAGE30_DECISIONS_FILE.", call. = FALSE)
  v11 <- jsonlite::fromJSON(V11, simplifyVector = FALSE)
  if (!identical(v11$meta$status, "FROZEN") || !identical(v11$meta$version, "1.1")) stop("The v1.1 JSON is not status FROZEN, version 1.1.", call. = FALSE)
  commit <- git("rev-parse", "HEAD")[1]
  reg_commit <- git("log", "-1", "--format=%H", "--", "docs/stage30/stage30_registry_v1.1.json")[1]
  dir.create(REG_OUT, recursive = TRUE)
  file.copy(V11, file.path(REG_OUT, basename(V11))); file.copy(MD11, file.path(REG_OUT, basename(MD11)))
  if (!identical(sha(V11), sha(file.path(REG_OUT, basename(V11)))) || !identical(sha(MD11), sha(file.path(REG_OUT, basename(MD11)))))
    stop("Registry copy is not byte-identical.", call. = FALSE)
  ev <- "C:/Users/topohl/Documents/e9_behavior_rationalization_audit_2026-09-27/stage30_sleep_cookie"
  fn <- function(p) file.path(MMM_REPO_ROOT, p)
  items <- c(registry_json = file.path(REG_OUT, basename(V11)), registry_md = file.path(REG_OUT, basename(MD11)),
             sus_list = file.path(EP, "sus_animals.csv"), con_list = file.path(EP, "con_animals.csv"), combz_table = CANON,
             v2_manifest = file.path(ROOT, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "MANIFEST_SHA256.csv"),
             cookie_manifest_staged = file.path(ev, "evidence", "wfE", "E2_cookie_dataset", "staged", "cookie_postEPM_v1", "MANIFEST_SHA256.csv"),
             cc1_cc4_reference_table = file.path(rel, "tables", "canonical_window_metrics.csv"),
             movement_wrapper = fn("Functions/stage30_movement.R"), inference_engine = fn("Functions/rfid_canonical_inference.R"),
             event_stream = fn("Functions/rfid_event_stream.R"), binfree_metrics = fn("Functions/rfid_binfree_metrics.R"),
             config_at_freeze = fn("Functions/behavior_analysis_config.R"),
             calibration_manifest = file.path(ev, "evidence", "wfE", "E4_registry_audit", "e4_99_manifest_sha256.csv"),
             decisions_round5 = file.path(ev, "evidence", "wfE", "USER_DECISIONS_2026-09-29_round5.txt"),
             decisions_round6 = file.path(ev, "evidence", "wfE", "USER_DECISIONS_2026-09-29_round6_POOL_estimation_only.txt"),
             decisions_v1_1 = dec)
  miss <- items[!file.exists(items)]; if (length(miss)) stop("Missing freeze inputs: ", paste(miss, collapse = ", "), call. = FALSE)
  at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rec <- data.table(item = names(items), path = unname(items), sha256 = vapply(items, sha, ""), bytes = file.size(items),
                    repo_head_at_freeze = commit, frozen_at = at, registry_commit = reg_commit)
  rec[item %in% c("registry_json", "registry_md"), path := basename(path)]
  fwrite(rec, file.path(REG_OUT, "FREEZE_RECORD.csv"))
  writeLines(c(paste("# Stage 30 registry v1.1 - FROZEN", v11$meta$frozen_at),
               paste0(rec[item == "registry_json", sha256], "  ", basename(V11)), paste0(rec[item == "registry_md", sha256], "  ", basename(MD11))),
             file.path(REG_OUT, "REGISTRY_SHA256.txt"))
  Sys.chmod(list.files(REG_OUT, full.names = TRUE), mode = "0444")
  message("Frozen ", REG_OUT, "\n  S30SC_REGISTRY_SHA256 <- \"", rec[item == "registry_json", sha256], "\"\n  S30SC_REGISTRY_MD_SHA256 <- \"",
          rec[item == "registry_md", sha256], "\"\n  S30SC_REF_TABLE_SHA256 <- \"", rec[item == "cc1_cc4_reference_table", sha256], "\"")
}
