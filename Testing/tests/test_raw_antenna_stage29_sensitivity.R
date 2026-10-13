# Contract test for the unregistered raw-antenna Stage 29 sensitivity wrapper (Analysis/raw_antenna_stage29_sensitivity.R).
#
#  1. sourcing without mmm.rant_version does nothing
#  2. each replaced statement occurs exactly once in the frozen Stage 29, and so does the seed-guard anchor
#  3. the declared raw data versions resolve through the frozen mmm_dv_resolve(): not the release, their own files, hashes and
#     manifest column, v2's count facts except reseeded_windows = 0; the frozen configuration and its hash are unchanged
#  4. seed guard: a frozen result passes through unchanged; an empty one gets the typed seed columns and binds like the frozen
#     stream; a late animal still makes the frozen function read the missing vendor folder and stop
#  5. outputs are labelled UNREGISTERED, outside analysis_ready; the vendor raw_data folder is named only in the replaced statement
#
# Portable: parses the frozen Stage 29 and the repository config; synthetic tables in tempdir() only; no stage runs.
# Portable-suite idiom: plain Rscript from the repo root, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
source("Analysis/_pipeline_setup.R")
for (h in c("project_paths.R", "behavior_analysis_config.R", "behavior_config_identity.R", "behavioral_dynamics_helpers.R",
            "rfid_event_stream.R")) source_mmm_helper(h)

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
err   <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
WRAPPER <- "Analysis/raw_antenna_stage29_sensitivity.R"

# ---------------------------------------------------------------- 1. no-op without the option
options(mmm.rant_version = NULL)
W <- new.env(parent = globalenv())
msg <- character()
withCallingHandlers(sys.source(WRAPPER, envir = W), message = function(m) { msg <<- c(msg, conditionMessage(m)); invokeRestart("muffleMessage") })
check(length(msg) == 1 && grepl("nothing to do", msg), "1: without mmm.rant_version the wrapper only reports that it has nothing to do")
check(is.function(W$raw29_run) && is.function(W$raw29_spec) && is.function(W$raw29_guard_seeds), "1: the wrapper defines its functions")
ok("sourcing without mmm.rant_version runs nothing")

# ---------------------------------------------------------------- 2. substitution targets
exprs <- parse(W$RAW29_STAGE29, keep.source = TRUE)
txt <- vapply(attr(exprs, "srcref"), function(s) paste(as.character(s), collapse = "\n"), character(1))
for (t in names(W$RAW29_SUBSTITUTIONS)) check(sum(txt == t) == 1L, paste("2: frozen Stage 29 must contain exactly once:", t))
check(sum(grepl(W$RAW29_GUARD_AFTER, txt, fixed = TRUE) & startsWith(txt, "for (h in")) == 1L, "2: one helper-sourcing loop (guard anchor)")
check(which(txt == names(W$RAW29_SUBSTITUTIONS)[1]) < which(grepl("^dv_inputs <- mmm_dv_verify\\(DV\\)$", txt)),
      "2: the data version is resolved before the frozen mmm_dv_verify()")
check(all(vapply(W$RAW29_SUBSTITUTIONS, function(s) is.call(str2lang(s)), TRUE)), "2: replacements parse to single calls")
ok(sprintf("%d replaced statements and the guard anchor occur once each in the frozen Stage 29 (%d statements)",
           length(W$RAW29_SUBSTITUTIONS), length(txt)))

# ---------------------------------------------------------------- 3. declared raw data versions
CFG <- MMM_BEHAVIOR_CONFIG; sha0 <- mmm_behavior_config_sha256(CFG)
v2 <- CFG$data_versions[[W$RAW29_V2]]
root <- file.path(tempdir(), "raw29_root")
for (ver in names(W$RAW29_VERSIONS)) {
  v <- W$RAW29_VERSIONS[[ver]]
  cfg <- CFG; cfg$data_versions[[v$id]] <- W$raw29_spec(CFG, v)
  dv <- mmm_dv_resolve(cfg, root, v$id)
  check(!dv$is_release && identical(dv$id, v$id) && grepl("^raw", dv$tag), paste("3:", ver, "is a non-release raw version"))
  check(identical(dv$dir, file.path(root, "MMMSociability", "data_versions", v$id, "preprocessed_data")) &&
          identical(dv$sha_column, "sha256") && dv$n_files == 24 && grepl("^[0-9a-f]{64}$", dv$manifest_sha256),
        paste("3:", ver, "resolves to its own folder, manifest column and pinned hash"))
  check(length(dv$supporting) == 3 && all(grepl(v$id, vapply(dv$supporting, `[[`, "", "file"), fixed = TRUE)),
        paste("3:", ver, "supporting files are its own README, seed table and build audit"))
  s <- dv$spec
  check(s$runtime_gates$reseeded_windows == 0, paste("3:", ver, "declares no vendor-seeded window"))
  declared <- function(key) if (is.null(v$facts[[key]])) v2[[key]] else if (is.list(v2[[key]]) && is.list(v$facts[[key]]))
    utils::modifyList(v2[[key]], v$facts[[key]]) else v$facts[[key]]
  check(all(vapply(c("runtime_gates", "expected_counts", "complete_case_missing", "d2"), function(k) identical(s[[k]], declared(k)), TRUE)),
        paste("3:", ver, "keeps v2's count facts (same roster and cages) apart from its declared departures"))
  departures <- setdiff(names(unlist(v$facts)), "runtime_gates.reseeded_windows")
  check(!length(departures) || (is.character(v$facts_reason) && nzchar(v$facts_reason)), paste("3:", ver, "gives a reason for each departure from v2"))
  check(grepl("UNREGISTERED", s$status), paste("3:", ver, "status says UNREGISTERED"))
}
check(identical(mmm_behavior_config_sha256(CFG), sha0) && identical(sha0, mmm_behavior_config_sha256(MMM_BEHAVIOR_CONFIG)),
      "3: the frozen configuration is not modified")
vv <- W$RAW29_VERSIONS
check(!anyDuplicated(vapply(vv, `[[`, "", "manifest_sha256")) && !anyDuplicated(vapply(vv, `[[`, "", "tag")), "3: manifests and tags differ between versions")
ok("every raw version resolves as a declared, non-release data version; the frozen config hash is unchanged")

# ---------------------------------------------------------------- 3b. one version table across wrapper, runner and builder
rr <- W$RAW29_ROOT_NAMES
check(identical(names(rr), names(vv)) && !anyDuplicated(rr) && all(nchar(rr) <= 4) && !any(toupper(rr) == "RFID"),
      "3b: every version has its own root of at most 4 characters, not the live RFID root")
run_exprs <- parse("Analysis/run_raw_antenna_parallel.R")
run_const <- new.env()
for (e in run_exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("RANT_ROOT_NAMES", "RANT_DV_DATES"))
  eval(e, run_const)   # constants only; the runner itself is never sourced here
check(identical(run_const$RANT_ROOT_NAMES, rr), "3b: runner and wrapper use the same roots")
check(identical(unname(vapply(names(vv), function(x) vv[[x]]$id, "")), unname(paste0("raw_antenna_", names(vv), "_", run_const$RANT_DV_DATES[names(vv)]))),
      "3b: the wrapper's data-version ids match the runner's version dates")
suppressPackageStartupMessages(library(dplyr))
source("Functions/antenna_log_helpers.R"); source("Functions/antenna_data_version.R")
check(identical(names(ANTENNA_DV_VERSIONS), names(vv)) && ANTENNA_DV_VERSIONS$ge200$root == rr[["ge200"]] && ANTENNA_DV_VERSIONS$flip2s$root == rr[["flip2s"]],
      "3b: the builder's registry has the same versions and builds flip2s and ge200 into their run roots (noflip is copied to RANU)")
ok("wrapper, runner and builder agree on versions, roots and dates")

# ---------------------------------------------------------------- 4. seed guard
t0 <- as.POSIXct("2023-04-24 18:30:00", tz = "UTC")
pre <- data.table(DateTime = t0 + c(0, 0, 60, 120), AnimalID = c("OR1", "0002", "OR1", "0002"), System = "sys.1",
                  PositionID = c(1L, 5L, 2L, 6L), Batch = "B3", CageChange = "CC1", SourceFile = "E9_SIS_B3_CC1_AnimalPos_preprocessed.csv")
pre[, AnimalNum := canonical_animal_id(AnimalID)][, is_seed := FALSE]
missing_raw <- file.path(tempdir(), "no_vendor_raw_data")
check(!dir.exists(missing_raw), "4: the vendor folder used here must not exist")
frozen0 <- mmm_evs_seeds(pre, missing_raw)
check(nrow(frozen0) == 0 && ncol(frozen0) == 0, "4: with every animal present at t0 the frozen function returns a 0 x 0 table")
guarded <- W$raw29_guard_seeds(mmm_evs_seeds)
g <- guarded(pre, missing_raw)
check(nrow(g) == 0 && identical(names(g), c("DateTime", "AnimalID", "System", "PositionID", "Batch", "CageChange", "SourceFile",
                                             "AnimalNum", "is_seed", "SeedReadTime", "raw_file")), "4: the empty result gets the typed seed columns")
pos <- rbindlist(list(pre, g[, !c("SeedReadTime", "raw_file")]), fill = TRUE)
check(identical(pos, rbindlist(list(pre), fill = TRUE)), "4: binding the empty seeds leaves the stream unchanged")
check(nrow(unique(g[, .(AnimalNum, CC = paste0("CC", sub("^CC", "", as.character(CageChange))))])) == 0, "4: the frozen key_cc() works on it")
late <- rbind(pre, data.table(DateTime = t0 + 300, AnimalID = "OR9", System = "sys.1", PositionID = 3L, Batch = "B3", CageChange = "CC1",
                              SourceFile = pre$SourceFile[1], AnimalNum = "OR9", is_seed = FALSE))
check(!is.na(err(guarded(late, missing_raw))), "4: a late animal still makes the frozen function read the missing vendor folder and stop")
seeded <- data.table(DateTime = t0, AnimalID = "OR9", System = "sys.1", PositionID = 3L, Batch = "B3", CageChange = "CC1",
                     SourceFile = pre$SourceFile[1], AnimalNum = "OR9", is_seed = TRUE, SeedReadTime = t0 - 5, raw_file = "x.csv")
check(identical(W$raw29_guard_seeds(function(pre, raw_dir, parser) seeded)(pre, missing_raw), seeded), "4: a frozen result passes through unchanged")
# installed as in a run: the guard replaces the binding it wraps (a lazily bound wrapper would recurse forever)
G <- new.env(); G$mmm_evs_seeds <- mmm_evs_seeds
W$raw29_install_seed_guard(G)
check(!identical(G$mmm_evs_seeds, mmm_evs_seeds), "4: the install replaces the binding")
gi <- err(G$mmm_evs_seeds(pre, missing_raw))
check(is.na(gi), paste("4: the installed guard calls the frozen function, not itself:", gi))
check(identical(names(G$mmm_evs_seeds(pre, missing_raw)), names(g)), "4: the installed guard types the empty result")
check(grepl("Fehler|Error|exist|not|nicht", err(G$mmm_evs_seeds(late, missing_raw))) && !grepl("rekursion|recursion", err(G$mmm_evs_seeds(late, missing_raw)), ignore.case = TRUE),
      "4: the installed guard still stops on a late animal by reading the missing folder")
ok("the seed guard only types an empty result")

# ---------------------------------------------------------------- 5. labels and paths
code <- readLines(WRAPPER, warn = FALSE); code <- code[!grepl("^\\s*#", code)]
check(sum(grepl("RFID_SRC", code, fixed = TRUE)) == 1 && any(grepl("RAW29_SUBSTITUTIONS <- c(", code, fixed = TRUE)),
      "5: the vendor folder (RFID_SRC/raw_data) is named only in the replaced statement")
check(any(grepl("\"S29_raw_sensitivity_UNREGISTERED\"", code, fixed = TRUE)) && any(grepl("UNREGISTERED.txt", code, fixed = TRUE)),
      "5: output folder and marker file are labelled UNREGISTERED")
ar_lines <- code[grepl("analysis_ready", code, fixed = TRUE)]
check(length(ar_lines) == 1 && grepl("behavior_config", ar_lines), "5: analysis_ready is touched only to copy the frozen configuration")
check(any(grepl("Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root, MMM_STAGE29_CONTROL_DIR = work)", code, fixed = TRUE)) &&
        any(grepl("Sys.unsetenv(c(\"MMM_PROJECT_ROOT\", \"MMM_STAGE29_DRY_RUN_DIR\", \"MMM_STAGE29_DATA_VERSION\"))", code, fixed = TRUE)),
      "5: CONTROL mode into the staging folder; no DRY run, no declared data-version override")
ok("outputs labelled UNREGISTERED and kept outside analysis_ready")
cat("Raw-antenna Stage 29 sensitivity wrapper checks: PASS\n")
