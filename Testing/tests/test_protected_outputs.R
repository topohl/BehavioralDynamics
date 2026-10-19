# ===========================================================================
# Protected outputs (Functions/protected_outputs.R) and the round-level verifier (Testing/audits/
# verify_protected_outputs.R). Portable: fixtures under tempdir(); nothing on the lab drive is read or written, and the
# verifier is sourced for its definitions only.
#
#  1. registry: every required live tree, the publication copies, the inputs, the whole RANM tree, the manuscript's
#     source data and provenance, the repository pin files and the write-refused roots; the snapshot protects earlier
#     RANM runs of every tool (the same tool included) and leaves out only the current run's two folders
#  2. output base: refused inside an earlier RANM run (also as an existing run id or inside a non-run RANM folder), as
#     the RANM tree itself or around it, inside an Exp9_manuscript-like folder, inside the protected live trees, the
#     project root, the raw logs and the repository, inside a "runs" folder, and (on a sandbox root) as or around the live
#     RANM tree or inside the live Exp9_manuscript; an existing .tmp_ folder only when resuming; a new RANM/<tool>, a new
#     run of an existing tool and an outside scratch folder are accepted; verifier arguments --first-run / --baseline
#  3. guard begin/end: a clean run passes and writes its two protection files; an added file in an earlier run folder,
#     a byte change of a hashed file with its time stamp restored, a touched unhashed file, a removed file and a changed
#     repository pin stop the run; a byte-identical rewrite of a hashed file and an Explorer thumbnail pass; the
#     protection files must go into the run's own folder; a first run under a missing RANM tree passes; the frozen-input
#     guard is silent on a sandbox root
#  4. verifier self-manifest checks: a one-byte change (size in tier 1, sha256 in tier 2), a missing and an extra file
#     are caught; the manifest's own file is not reported; Stage 00b's unlisted files are allowed for its runs only;
#     sha256-only manifests and manifests that list tables/ by base name
#  5. Stage 00b ledger chain: ledger -> run manifest -> files; a changed run manifest and a run folder without a ledger
#     line are caught; the ledger prefix against an earlier state
#  6. bundle chain: BUNDLE_REGISTRY manifest_sha256 -> 00_manifest.csv -> files; a wrong registry hash and an
#     unregistered folder are caught
#  7. Stage 19 record overlay and its prior-hash chain
#  8. baseline snapshot: written and read back without spurious differences; a new RANM run folder is expected, a
#     modified protected file is not; an edited baseline file is refused
#  9. repository pins read from the tests that hold them; sourcing the verifier runs nothing
# 10. frozen pins (bytes in tier 1, the pin count) and recorded inputs (conflicting records, paths outside the protected
#     inputs ignored), on a fixture root named as an alias of the recorded root
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "protected_outputs.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
refused <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
MMM_REPO_ROOT <- repo
source("Functions/project_paths.R")
source("Functions/qc_run_protection.R")
source("Functions/frozen_input_guard.R")
source("Functions/protected_outputs.R")
source("Functions/spatial_exclusion_manifest.R")
before_vpo <- ls(all.names = TRUE)
source("Testing/audits/verify_protected_outputs.R")
new_objects <- setdiff(ls(all.names = TRUE), c(before_vpo, "before_vpo"))
sha <- function(f) digest::digest(file = f, algo = "sha256")
put <- function(path, text) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(text, path); invisible(path) }
quiet <- function(expr) suppressMessages(expr)
# A self-manifest as the builders write it: file, sha256, bytes (or only the first columns, renamed, e.g. File,SHA256).
write_manifest <- function(run, rel, cols = c("file", "sha256", "bytes"), path = VPO_RUN_MANIFEST) {
  d <- data.table(file = rel, sha256 = vapply(file.path(run, rel), sha, "", USE.NAMES = FALSE), bytes = file.size(file.path(run, rel)))
  d <- d[, seq_along(cols), with = FALSE]
  setnames(d, cols)
  dir.create(dirname(file.path(run, path)), recursive = TRUE, showWarnings = FALSE)
  fwrite(d, file.path(run, path))
  invisible(file.path(run, path))
}

fx <- gsub("\\\\", "/", tempfile("protout_")); dir.create(fx); fx <- normalizePath(fx, winslash = "/")
analysis <- file.path(fx, "Exp", "Analysis")
B <- file.path(analysis, "Behavior"); root <- file.path(B, "RFID"); ranm <- file.path(B, "RANM")
manuscript <- file.path(analysis, "Exp9_manuscript")
raw <- file.path(fx, "Exp", "Raw Data", "Behavior", "RFID", "MMMdata")
options(mmm.raw_antenna_dir = raw)
frepo <- file.path(fx, "repo")
for (rel in names(MMM_PROTECTED_PROJECT_LOCATIONS)) put(file.path(root, rel, "f.csv"), rel)
put(file.path(root, "MMMSociability", "raw_data", "B1", "tags.csv"), "tag")
put(file.path(root, "MMMSociability", "data_versions", "v2", "p.csv"), "pre")
put(file.path(raw, "B1", "raw", "log.csv"), "raw")
srs <- file.path(ranm, "single_read_spatial")
old <- file.path(srs, "runs", "srs_old"); old2 <- file.path(srs, "runs", "srs_older")
for (r in c(old, old2)) { put(file.path(r, "tables", "t.csv"), c("a,b", "1,2")); write_manifest(r, "tables/t.csv") }
put(file.path(ranm, "board_adjustment", "runs", "badj_v1", "tables", "x.csv"), "x")
put(file.path(ranm, "raw_acquisition_qc", "REVIEW_STATUS.md"), "status")
put(file.path(ranm, "cc4_grid_exposure_censored", "whole_inactive_x", "audit", "output_manifest.csv"), "File,SHA256")
put(file.path(ranm, "measurement_validation", "tables", "m.csv"), "m")
put(file.path(manuscript, "source_data", "MMMSociability", "s.csv"), "s")
put(file.path(manuscript, "provenance", "source_manifests", "behaviour_bundle_manifest.csv"), "bundle_id,file,bytes,sha256")
put(file.path(manuscript, "R", "code.R"), "x <- 1")
put(file.path(root, "analysis_ready", "manuscript", "unprotected.csv"), "u")
put(file.path(frepo, "docs", "spatial_exclusions", "spatial_analysis_exclusions_v1.0.csv"), "a")
put(file.path(frepo, "Functions", "behavior_analysis_config.R"), "cfg <- 1")
put(file.path(frepo, "docs", "frozen_output_pins", "pins.csv"), "input,bytes,sha256,role")
put(file.path(frepo, "docs", "behavior_output_producer_reruns", "stage19_20261012.csv"), "group")
put(file.path(frepo, "docs", "behavior_output_producer_reruns", "stage14_20261006.csv"), "group")
put(file.path(frepo, "docs", "rfid_calibration", "acp_criteria_v1.0.csv"), "rule_id")

# 1. registry and snapshot
reg <- mmm_protected_output_locations(root, frepo)
check(identical(names(reg), c("location", "class", "note")), "1: the registry has the columns location, class and note")
required <- c(file.path("analysis_ready/analyses", c("spatial_occupancy", "cc4_grid_exposure", "hmm_states", "posthoc_cohort_followups")),
              file.path("analysis_ready/pipeline", c("29_canonical_behavior_releases", "29b_posthoc_con_contrasts", "29c_posthoc_movement_combz_fit",
                                                     "30_exploratory_screen", "32_behavior_exposure_adaptation", "09_early_prediction",
                                                     "28_rfid_behavioral_domains")),
              "analysis_ready/canonical", "analysis_ready/quality_control/raw_acquisition", "analysis_ready/quality_control/tracking_integrity",
              "analysis_ready/history/tracking_integrity/10sec", "analysis_ready/foundations")
check(all(file.path(root, required) %in% reg[class == "live_output"]$location), "1: every required live tree is protected")
check(setequal(reg[class == "publication_copy"]$location, file.path(root, "publication_ready", c("tables", "figures/single_panels"), "spatial_occupancy")),
      "1: the Stage 19 publication copies are protected")
check(identical(reg[class == "input_raw_logs"]$location, raw) &&
        setequal(reg[class == "input"]$location, file.path(root, "MMMSociability", c("raw_data", "data_versions"))), "1: the inputs are protected")
check(identical(reg[class == "isolated_runs"]$location, ranm), "1: the whole RANM tree is one protected location")
check(setequal(reg[class == "manuscript"]$location, file.path(manuscript, c("source_data", "provenance"))), "1: manuscript source data and provenance")
pins <- reg[class == "repo_pin"]$location
check(all(file.path(frepo, c("docs/spatial_exclusions", "Functions/behavior_analysis_config.R", "docs/frozen_output_pins", "docs/rfid_calibration",
                             "docs/behavior_output_producer_reruns/stage19_20261012.csv")) %in% pins) &&
        !any(grepl("stage14_", pins)), "1: the repository pin files (stage19_* expanded, other records not)")
live_ranm <- file.path(dirname(MMM_PROJECT_ROOT_DEFAULT), "RANM")
live_ms <- file.path(dirname(dirname(MMM_PROJECT_ROOT_DEFAULT)), "Exp9_manuscript")
check(setequal(reg[class == "write_refused"]$location, c(root, manuscript, frepo, live_ms)),
      "1: project, manuscript and repository roots (and, on a sandbox root, the live manuscript) are write-refused")
check(identical(reg[class == "isolated_runs_live"]$location, live_ranm), "1: on a sandbox root the live RANM tree is registered (not snapshotted)")
check(!nrow(mmm_protected_output_locations(MMM_PROJECT_ROOT_DEFAULT, frepo)[class == "isolated_runs_live"]), "1: no live-tree entry on the live root")
own <- file.path(srs, "runs", c(".tmp_new", "new"))
put(file.path(own[1], "tables", "o.csv"), "own"); put(file.path(own[1], "manifest", "run.csv"), "own")
put(file.path(root, "analysis_ready", "canonical", "Thumbs.db"), "thumb")
s0 <- mmm_protection_snapshot(reg, exclude = own)
check(all(file.path(c(old, old2), "tables", "t.csv") %in% s0$path) && file.path(ranm, "board_adjustment/runs/badj_v1/tables/x.csv") %in% s0$path,
      "1: earlier runs of the same and of other tools are snapshotted")
check(!any(startsWith(tolower(s0$path), tolower(own[1]))), "1: the current run's .tmp_ folder is left out")
check(!any(basename(s0$path) == "Thumbs.db"), "1: Explorer thumbnails are not listed")
check(!is.na(s0$sha256[s0$path == file.path(old, VPO_RUN_MANIFEST)]) && is.na(s0$sha256[s0$path == file.path(old, "tables", "t.csv")]) &&
        !is.na(s0$sha256[s0$path == file.path(frepo, "Functions", "behavior_analysis_config.R")]),
      "1: manifests and repository pins are hashed, other files by size and time stamp")
check(!any(startsWith(s0$path, file.path(manuscript, "R"))) && !any(startsWith(s0$path, file.path(root, "analysis_ready", "manuscript"))),
      "1: write-refused roots are not snapshotted beyond their protected folders")
check(identical(s0$class[s0$path == file.path(old, "tables", "t.csv")], "isolated_runs"), "1: rows carry their location class")
unlink(file.path(root, "analysis_ready", "canonical", "Thumbs.db")); unlink(own[1], recursive = TRUE)

# 2. output base
ab <- function(base, id = "new") mmm_protection_assert_base(base, id, reg)
check(refused(ab(old)) && refused(ab(file.path(old, "sub"))), "2: a base equal to or inside an earlier RANM run is refused")
check(refused(ab(srs, "srs_old")), "2: an existing run id of the same tool is refused")
check(refused(ab(file.path(ranm, "cc4_grid_exposure_censored", "whole_inactive_x"))), "2: a base inside a non-run RANM folder is refused")
check(refused(ab(ranm)) && refused(ab(B)) && refused(ab(analysis)), "2: the RANM tree itself and folders around it are refused")
check(refused(ab(file.path(manuscript, "scratch"))) && refused(ab(file.path(manuscript, "source_data", "x"))), "2: inside an Exp9_manuscript-like folder")
check(refused(ab(file.path(root, "analysis_ready", "analyses", "spatial_occupancy", "x"))) && refused(ab(file.path(root, "publication_ready", "x"))) &&
        refused(ab(file.path(root, "analysis_ready", "pipeline", "29_canonical_behavior_releases"))), "2: inside or equal to the protected live trees")
check(refused(ab(file.path(raw, "x"))) && refused(ab(file.path(frepo, "out"))) && refused(ab(file.path(root, "MMMSociability", "out"))),
      "2: inside the raw logs, the repository or the inputs")
check(refused(ab(srs, "")) && refused(ab(srs, "../x")) && refused(ab(srs, ".tmp_x")), "2: malformed run ids")
check(identical(ab(file.path(ranm, "new_tool")), file.path(ranm, "new_tool", "runs", c(".tmp_new", "new"))), "2: a new RANM/<tool> is accepted")
check(isTRUE(!refused(ab(srs, "srs_v2"))), "2: a new run of an existing tool is accepted")
put(file.path(srs, "runs", ".tmp_srs_v2", "x.csv"), "partial")
check(refused(ab(srs, "srs_v2")) && isTRUE(!refused(mmm_protection_assert_base(srs, "srs_v2", reg, resume = TRUE))),
      "2: an existing .tmp_ folder of the same run is refused unless resuming")
unlink(file.path(srs, "runs", ".tmp_srs_v2"), recursive = TRUE)
check(refused(ab(file.path(srs, "runs"))) && refused(ab(file.path(srs, "runs", "x"))) && refused(ab(file.path(fx, "scratch", "runs", "dev"))),
      "2: a base inside a runs folder is refused")
check(refused(ab(dirname(live_ranm))) && refused(ab(live_ranm)) && refused(ab(file.path(live_ms, "source_data", "x"))),
      "2: on a sandbox root the live RANM tree (as or around it) and the live manuscript are refused")
check(isTRUE(!refused(ab(file.path(fx, "scratch", "dev")))), "2: an outside scratch folder is accepted")
check(.mmm_po_within("//server/share/lab/e/behavior/ranm/x", "s:/lab/e/behavior/ranm") &&
        .mmm_po_within("s:/lab/e/behavior/ranm/x", "//server/share/lab/e/behavior/ranm") &&
        !.mmm_po_within("s:/lab/e/behavior/ranm", "//server/share/lab/e/behavior/ranm/x"), "2: drive-letter and UNC spellings")

# 3. guard begin/end
tool <- function() file.path(srs, "runs", ".tmp_new")
begin <- function() quiet(mmm_protection_guard_begin(srs, "new", "test", project_root = root, repo_root = frepo))
finish <- function(st) quiet(mmm_protection_guard_end(st, file.path(tool(), "manifest")))
outputs <- function() { put(file.path(tool(), "tables", "o.csv"), "out"); put(file.path(tool(), "manifest", "run.csv"), "run") }
pc <- function() fread(file.path(tool(), "manifest", "protection_check.csv"))
reset <- function() unlink(tool(), recursive = TRUE)
#   clean run
st <- begin()
check(is.null(st$frozen), "3: the frozen-input guard is silent on a sandbox root")
check(identical(st$own, file.path(srs, "runs", c(".tmp_new", "new"))) && nrow(st$before) == nrow(s0), "3: begin() snapshots the registry without the run's folders")
outputs()
ch <- finish(st)
check(nrow(ch) == 0L && nrow(pc()) == 0L && nrow(fread(file.path(tool(), "manifest", "protection_snapshot_before.csv"))) == nrow(st$before),
      "3: a clean run passes and writes protection_check.csv and protection_snapshot_before.csv")
reset()
#   an added file in an earlier run folder
st <- begin(); outputs(); put(file.path(old, "tables", "late.csv"), "late")
check(refused(finish(st)) && identical(pc()$change, "added"), "3: an added file in an earlier run folder stops the run")
unlink(file.path(old, "tables", "late.csv")); reset()
#   a byte change of a hashed small file, its time stamp restored
mf <- file.path(old, VPO_RUN_MANIFEST); txt <- readLines(mf); mt <- file.mtime(mf)
st <- begin(); outputs()
writeLines(sub("tables/t.csv", "tables/T.csv", txt, fixed = TRUE), mf); invisible(Sys.setFileTime(mf, mt))
check(file.size(mf) == st$before$size[st$before$path == mf], "3: (fixture) the edit keeps the size")
check(refused(finish(st)) && identical(pc()$change, "modified") && identical(pc()$verdict, "changed"), "3: a same-size byte change of a hashed file is caught")
writeLines(txt, mf); reset()
#   a byte-identical rewrite of a hashed file (new time stamp)
st <- begin(); outputs()
writeLines(txt, mf); invisible(Sys.setFileTime(mf, Sys.time() + 30))
ch <- finish(st)
check(nrow(ch) == 1L && identical(ch$verdict, "identical bytes"), "3: a byte-identical rewrite of a hashed file passes")
reset()
#   a touched file that is not hashed (bytes cannot be confirmed)
tf <- file.path(old, "tables", "t.csv")
st <- begin(); outputs(); invisible(Sys.setFileTime(tf, Sys.time() + 60))
check(refused(finish(st)) && identical(pc()$verdict, "changed"), "3: a changed time stamp of an unhashed file stops the run")
reset()
#   a removed file, a changed repository pin, a thumbnail
st <- begin(); outputs(); keep <- readLines(tf); invisible(file.remove(tf))
check(refused(finish(st)) && identical(pc()$change, "removed"), "3: a removed protected file stops the run")
writeLines(keep, tf); reset()
cfg <- file.path(frepo, "Functions", "behavior_analysis_config.R"); mt <- file.mtime(cfg)
st <- begin(); outputs(); writeLines("cfg <- 2", cfg); invisible(Sys.setFileTime(cfg, mt))
check(refused(finish(st)) && identical(basename(pc()$path), "behavior_analysis_config.R"), "3: a changed repository pin is caught (same size, time stamp restored)")
writeLines("cfg <- 1", cfg); reset()
st <- begin(); outputs(); put(file.path(root, "analysis_ready", "analyses", "hmm_states", "Thumbs.db"), "t")
check(nrow(finish(st)) == 0L, "3: an Explorer thumbnail in a protected folder is ignored")
unlink(file.path(root, "analysis_ready", "analyses", "hmm_states", "Thumbs.db")); reset()
#   the protection files go into the run's own folder only
st <- begin(); outputs()
check(refused(mmm_protection_guard_end(st, file.path(fx, "elsewhere"))) && !dir.exists(file.path(fx, "elsewhere")),
      "3: protection files outside the run's own folder are refused before anything is written")
reset()
#   the guard refuses an output base inside an earlier run before snapshotting
check(refused(quiet(mmm_protection_guard_begin(old, "x", "test", project_root = root, repo_root = frepo))), "3: begin() refuses a base inside an earlier run")
#   a first run under a missing RANM tree
root2 <- file.path(fx, "Exp2", "Analysis", "Behavior", "RFID"); put(file.path(root2, "analysis_ready", "canonical", "c.csv"), "c")
base2 <- file.path(dirname(root2), "RANM", "first_tool")
st2 <- quiet(mmm_protection_guard_begin(base2, "r1", "test", project_root = root2, repo_root = frepo))
put(file.path(base2, "runs", ".tmp_r1", "tables", "o.csv"), "o")
ch2 <- quiet(mmm_protection_guard_end(st2, file.path(base2, "runs", ".tmp_r1", "manifest")))
check(all(ch2$verdict == "parent of the run's own folder") && nrow(ch2) >= 1L, "3: a first run under a missing RANM tree passes")

# 4. verifier self-manifest checks
check(all(startsWith(new_objects, "vpo_") | startsWith(new_objects, "VPO_")), "9: sourcing the verifier only defines vpo_*/VPO_* objects (the main block did not run)")
vr <- file.path(fx, "vr", "runs", "r1")
put(file.path(vr, "tables", "a.csv"), "alpha"); put(file.path(vr, "tables", "b.csv"), "bravo")
write_manifest(vr, c("tables/a.csv", "tables/b.csv"))
mc <- function(tier, allow = character(0), run = vr, man = VPO_RUN_MANIFEST, subdir = "") vpo_manifest_check("t", run, man, tier, vpo_hasher(), subdir, allow)
r <- mc(1L)
check(nrow(r) == 2L && all(r$status == "ok") && !any(grepl("output_files_sha256", r$file)), "4: a clean run folder passes; the manifest is not reported")
put(file.path(vr, "tables", "a.csv"), "alphA")                                           # one byte, same size
check(all(mc(1L)$status == "ok") && identical(mc(2L)[file == "tables/a.csv"]$status, "sha_mismatch"), "4: a same-size one-byte change is caught in tier 2")
put(file.path(vr, "tables", "a.csv"), "alpha!")                                          # one byte more
check(identical(mc(1L)[file == "tables/a.csv"]$status, "size_mismatch"), "4: a one-byte size change is caught in tier 1")
put(file.path(vr, "tables", "a.csv"), "alpha")
invisible(file.remove(file.path(vr, "tables", "b.csv")))
check(identical(mc(1L)[file == "tables/b.csv"]$status, "missing"), "4: a missing file is caught")
put(file.path(vr, "tables", "b.csv"), "bravo"); put(file.path(vr, "tables", "c.csv"), "extra"); put(file.path(vr, "tables", "Thumbs.db"), "t")
check(identical(mc(1L)[status != "ok"]$file, "tables/c.csv") && identical(mc(1L)[file == "tables/c.csv"]$status, "unlisted"),
      "4: an extra file is caught; a thumbnail is not")
invisible(file.remove(file.path(vr, "tables", "c.csv")))
put(file.path(vr, "rec", "B1_CC1", "x.csv"), "rec"); put(file.path(vr, "manifest", "protection_check.csv"), "p")
check(sum(mc(1L)$status == "unlisted") == 2L && all(mc(1L, VPO_RAQC_UNLISTED)$status == "ok"), "4: Stage 00b's unlisted files pass for its runs only")
unlink(file.path(vr, "rec"), recursive = TRUE); invisible(file.remove(file.path(vr, "manifest", "protection_check.csv")))
s31 <- file.path(fx, "s31", "phase_groups_x"); put(file.path(s31, "a.csv"), "a")
write_manifest(s31, "a.csv", cols = c("File", "SHA256"), path = "manifest.csv")
check(all(mc(1L, run = s31, man = "manifest.csv")$status == "ok") && all(mc(2L, run = s31, man = "manifest.csv")$status == "ok"), "4: a sha256-only manifest")
put(file.path(s31, "a.csv"), "b")
check(identical(mc(2L, run = s31, man = "manifest.csv")$status, "sha_mismatch"), "4: a sha256-only manifest catches a change in tier 2")
rel29 <- file.path(fx, "s29", "v101"); put(file.path(rel29, "tables", "w.csv"), "w"); put(file.path(rel29, "audit", "run_inputs.csv"), "i")
fwrite(data.table(file = "w.csv", bytes = file.size(file.path(rel29, "tables", "w.csv")), sha256 = sha(file.path(rel29, "tables", "w.csv"))),
       file.path(rel29, "audit", "output_manifest.csv"))
put(file.path(rel29, "ARCHIVE_README.txt"), "archived")
check(all(mc(2L, VPO_RELEASE_UNLISTED, run = rel29, man = "audit/output_manifest.csv", subdir = "tables")$status == "ok") &&
        identical(mc(2L, "^audit/", run = rel29, man = "audit/output_manifest.csv", subdir = "tables")[status != "ok"]$file, "ARCHIVE_README.txt"),
      "4: a release manifest listing tables/ by base name; audit files and the archive note are known unlisted files")

# 5. Stage 00b ledger chain
qc <- file.path(fx, "qc"); r1 <- file.path(qc, "runs", "raqc_1")
put(file.path(r1, "tables", "t.csv"), "t"); write_manifest(r1, "tables/t.csv")
put(file.path(r1, "rec", "B1_CC1", "r.csv"), "r"); put(file.path(r1, "manifest", "protection_check.csv"), "p")
lrow <- list(run_id = "raqc_1", finished = "x", git_commit = "c", rules_version = "v", qc_status = "PASS", isolated = TRUE, recordings = "B1_CC1",
             output_manifest_sha256 = sha(file.path(r1, VPO_RUN_MANIFEST)), protected_files = 1, protection_changes = 0)
mmm_qc_ledger_append(qc, lrow, mmm_qc_ledger_state(qc))
lc <- vpo_check_ledgers(qc, 1L, vpo_hasher())
check(nrow(lc$rows) >= 2L && all(lc$rows$status %in% VPO_OK) && any(lc$rows$note == "ledger output_manifest_sha256" & lc$rows$status == "ok"),
      "5: ledger -> run manifest -> files")
check(nrow(lc$states) == 1L && lc$states$bytes == file.size(mmm_qc_ledger_path(qc)), "5: the ledger state is recorded")
mtxt <- readLines(file.path(r1, VPO_RUN_MANIFEST)); writeLines(c(mtxt, "tables/z.csv,0,0"), file.path(r1, VPO_RUN_MANIFEST))
check(any(vpo_check_ledgers(qc, 1L, vpo_hasher())$rows$status == "chain_mismatch"), "5: a changed run manifest breaks the chain")
writeLines(mtxt, file.path(r1, VPO_RUN_MANIFEST))
dir.create(file.path(qc, "runs", "raqc_9"))
check(identical(vpo_check_ledgers(qc, 1L, vpo_hasher())$rows[status == "unledgered"]$unit, "raqc_9"), "5: a run folder without a ledger line is caught")
unlink(file.path(qc, "runs", "raqc_9"), recursive = TRUE)
lrow$run_id <- "raqc_2"; mmm_qc_ledger_append(qc, lrow, mmm_qc_ledger_state(qc))
check(identical(vpo_ledger_prefix(lc$states)$status, "prefix_ok"), "5: an appended ledger line keeps the prefix")
ltxt <- readLines(mmm_qc_ledger_path(qc)); writeLines(sub("raqc_1", "raqc_X", ltxt, fixed = TRUE), mmm_qc_ledger_path(qc))
check(identical(vpo_ledger_prefix(lc$states)$status, "prefix_changed"), "5: a rewritten ledger line is caught")
writeLines(ltxt, mmm_qc_ledger_path(qc))

# 6. bundle chain
broot <- file.path(fx, "bundles", "RFID"); bb <- file.path(broot, VPO_BUNDLE_ROOTS[1]); b1 <- file.path(bb, "ebb_1")
put(file.path(b1, "A.csv"), "a")
fwrite(data.table(file = "A.csv", bytes = file.size(file.path(b1, "A.csv")), sha256 = sha(file.path(b1, "A.csv")), schema_version = 1),
       file.path(b1, "00_manifest.csv"))
regw <- function(h) fwrite(data.table(bundle_id = "ebb_1", status = "FROZEN", manifest_sha256 = h), file.path(bb, "BUNDLE_REGISTRY.csv"))
regw(sha(file.path(b1, "00_manifest.csv")))
bc <- function() { x <- vpo_check_bundles(broot, 2L, vpo_hasher()); x[startsWith(path, bb)] }
check(nrow(bc()) == 2L && all(bc()$status == "ok"), "6: registry -> 00_manifest.csv -> files")
regw(strrep("0", 64))
check(identical(bc()[file == "00_manifest.csv"]$status, "chain_mismatch"), "6: a registry hash that differs from 00_manifest.csv is caught")
regw(sha(file.path(b1, "00_manifest.csv"))); dir.create(file.path(bb, "ebb_2")); dir.create(file.path(bb, "logs"))
check(identical(bc()[status == "unregistered"]$file, "ebb_2") && identical(bc()[file == "logs"]$status, "baseline_only"),
      "6: an unregistered bundle folder is caught; the build-log folder is left to the snapshot")

# 7. Stage 19 record overlay
ra <- file.path(fx, "rec_a.csv"); rb <- file.path(fx, "rec_b.csv")
fwrite(data.table(group = "g", target_root_rel = "analyses/spatial_occupancy/tables", target_file = c("x.csv", "y.csv"), prior_sha256 = "",
                  sha256 = c("a1", "b1"), change = "rewritten"), ra)
fwrite(data.table(group = "g", target_root_rel = "analyses/spatial_occupancy/tables", target_file = "x.csv", prior_sha256 = "a1", sha256 = "a2",
                  change = "rewritten"), rb)
rr <- vpo_stage19_record(c(ra, rb))
check(identical(rr$record[order(rel)]$sha256, c("a2", "b1")) && all(rr$chain$ok), "7: the overlay replaces the rows it lists; its prior hashes match")
fwrite(data.table(group = "g", target_root_rel = "analyses/spatial_occupancy/tables", target_file = "x.csv", prior_sha256 = "zz", sha256 = "a2",
                  change = "rewritten"), rb)
check(!any(vpo_stage19_record(c(ra, rb))$chain$ok), "7: an overlay whose prior hash differs is flagged")
real_rec <- vpo_stage19_record(file.path(repo, VPO_STAGE19_RECORDS))$record
check(identical(unname(lengths(VPO_STAGE19_COPY_FILES)), c(8L, 11L)) && all(unlist(VPO_STAGE19_COPY_FILES) %in% real_rec$target_file),
      "7: the 19 pinned publication copies are named in the Stage 19 record")
rk <- vpo_check_ranm_runs(ranm, 1L, vpo_hasher())
check(setequal(rk[status == "missing"]$unit, VPO_RANM_RUNS), "7: a registered RANM run that is gone is reported missing")
s31r <- file.path(ranm, "cc4_qc_reruns", "runs", "p31c_x"); dir.create(s31r, recursive = TRUE); writeLines("a", file.path(s31r, "primary_effects.csv"))
fwrite(data.table(File = "primary_effects.csv", SHA256 = digest::digest(file = file.path(s31r, "primary_effects.csv"), algo = "sha256")), file.path(s31r, "manifest.csv"))
r31 <- vpo_check_ranm_runs(ranm, 2L, vpo_hasher())[unit == "cc4_qc_reruns/p31c_x"]
check(nrow(r31) >= 1L && all(r31$status == "ok"), "7: a Stage 31b-31d run with a root manifest.csv (File, SHA256) is checked against it")
writeLines("b", file.path(s31r, "primary_effects.csv"))
check(any(vpo_check_ranm_runs(ranm, 2L, vpo_hasher())[unit == "cc4_qc_reruns/p31c_x"]$status != "ok"), "7: ... and a changed file in it is caught")
unlink(file.path(ranm, "cc4_qc_reruns"), recursive = TRUE)

# 8. baseline snapshot round trip and diff
h <- vpo_hasher()
snapA <- vpo_snapshot(reg, own, 1L, h)
prev <- file.path(fx, "checks", "runs", "dev_prev")
dir.create(file.path(prev, "snapshot"), recursive = TRUE); dir.create(file.path(prev, "tables"))
fwrite(snapA[, .(path, location, class, exists, size, mtime, sha256)], file.path(prev, "snapshot", "baseline_snapshot.csv"), na = "NA")
fwrite(lc$states, file.path(prev, "tables", "ledger_states.csv"), na = "NA")
write_manifest(prev, c("snapshot/baseline_snapshot.csv", "tables/ledger_states.csv"))
bl <- vpo_read_baseline(prev)
d0 <- vpo_baseline_diff(bl$snapshot, vpo_snapshot(reg, own, 1L, vpo_hasher()), ranm)
check(nrow(d0) == 1L && identical(d0$status, "ok"), "8: a snapshot read back from CSV shows no spurious difference")
put(file.path(ranm, "new_tool", "runs", "nt_1", "tables", "n.csv"), "n")
put(file.path(srs, "runs", "srs_v3", "tables", "n.csv"), "n")
put(file.path(root, "analysis_ready", "canonical", "f.csv"), "changed!")
d1 <- vpo_baseline_diff(bl$snapshot, vpo_snapshot(reg, own, 1L, vpo_hasher()), ranm)
check(setequal(d1[status == "expected_addition"]$file, "n.csv") && sum(d1$status == "expected_addition") == 2L, "8: new RANM run folders are expected additions")
check(identical(d1[!status %in% VPO_OK]$path, file.path(root, "analysis_ready", "canonical", "f.csv")) &&
        identical(d1[!status %in% VPO_OK]$status, "modified"), "8: a modified protected file is not")
put(file.path(old, "tables", "late.csv"), "late")
d2 <- vpo_baseline_diff(bl$snapshot, vpo_snapshot(reg, own, 1L, vpo_hasher()), ranm)
check(identical(d2[path == file.path(old, "tables", "late.csv")]$status, "added"), "8: a file added to an earlier run folder is not expected")
s <- vpo_summary(list(baseline_diff = d2, repo_pins = vpo_rows("repo_pins", "u", "f", "p", "ok")))
check(identical(s$status, c("FAIL", "PASS")), "8: the summary fails a check with any failing row")
nb <- function(st) vpo_summary(list(baseline_diff = vpo_rows("baseline_diff", "all", "", fx, st)))$status
check(identical(nb("not_run"), "FAIL") && identical(nb("first_run"), "PASS"), "8: a check without --baseline fails unless it is the round's --first-run")
write(c("x"), file.path(prev, "snapshot", "baseline_snapshot.csv"), append = TRUE)
check(refused(vpo_read_baseline(prev)), "8: an edited baseline file is refused")

# 9. repository pins and the sourced verifier
tp <- vpo_test_pins(repo)
check(all(grepl("^[0-9a-f]{64}$", tp)) && identical(names(tp), c("s13", "config")), "9: the S13 and configuration pins are read from the tests that hold them")
rp <- vpo_check_repo_pins(repo)
check(nrow(rp) >= 5L && all(rp$status == "ok"), paste("9: the repository pins match:", paste(rp$unit[rp$status != "ok"], collapse = ", ")))
check(identical(vpo_args(character(0))$tier, 1L) && refused(vpo_args("--tier=3")) && refused(vpo_args("--bogus=1")), "9: verifier arguments")
check(isTRUE(vpo_args("--first-run")$first_run) && isFALSE(vpo_args(character(0))$first_run) && refused(vpo_args(c("--first-run", "--baseline=x"))),
      "9: --first-run, and never together with --baseline")

# 10. frozen pins and recorded inputs, mapped onto a fixture root named as an alias of the recorded root (no drive access)
froot <- file.path(fx, "Exp3", "Analysis", "Behavior", "RFID")
op <- options(mmm.live_root_aliases = c(MMM_PROJECT_ROOT_DEFAULT, froot))
s08 <- fread(file.path(repo, MMM_REPO_PIN_LISTS[[1]]), colClasses = "character")
first <- sub("^.*?/analysis_ready/", "analysis_ready/", s08$input[1], perl = TRUE)
put(file.path(froot, first), strrep("x", as.numeric(s08$bytes[1]) + 1))
fp <- vpo_check_frozen_pins(froot, repo, 1L, vpo_hasher())
check(nrow(fp) == nrow(s08) + 1L && identical(fp[status == "count_mismatch"]$unit, "all"), "10: every repository pin is mapped onto the root; the pin count is checked")
check(identical(fp[file == first]$status, "size_mismatch") && all(fp[file != first & status != "count_mismatch"]$status == "missing"),
      "10: recorded bytes are compared in tier 1; absent pinned files are missing")
pl <- file.path(froot, MMM_FROZEN_PIN_LISTS[["stage29_v102"]])   # any registered list; v101 was retired 2026-10-19
tags <- put(file.path(froot, "MMMSociability", "raw_data", "B1", "tags.csv"), "tag")
dv <- put(file.path(froot, "MMMSociability", "data_versions", "v2", "p.csv"), "pre")
dir.create(dirname(pl), recursive = TRUE)
fwrite(data.table(input = c(tags, dv, dv, file.path(fx, "elsewhere.csv")), bytes = c(file.size(tags), file.size(dv), file.size(dv), 1),
                  sha256 = c(sha(tags), sha(dv), strrep("0", 64), strrep("1", 64)), role = "input"), pl)
ri <- vpo_check_recorded_inputs(froot, mmm_protected_output_locations(froot, frepo), 2L, vpo_hasher())
check(identical(ri[path == tags]$status, "ok") && identical(ri[path == dv]$status, "pin_conflict") && !any(ri$path == file.path(fx, "elsewhere.csv")),
      "10: recorded inputs in protected input folders are checked; conflicting records are flagged; other paths are ignored")
check(any(ri$status == "no_manifest"), "10: a missing pin list (the srs release on a fixture root) is reported")
options(op)

unlink(fx, recursive = TRUE)
cat("test_protected_outputs: all checks passed\n")
