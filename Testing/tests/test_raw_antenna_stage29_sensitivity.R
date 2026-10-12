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
  check(identical(s$runtime_gates[c("active_windows", "anchor_sourcefiles")], v2$runtime_gates[c("active_windows", "anchor_sourcefiles")]) &&
          identical(s$expected_counts, v2$expected_counts) && identical(s$complete_case_missing, v2$complete_case_missing) &&
          identical(s$d2, v2$d2), paste("3:", ver, "keeps v2's count facts (same roster and cages)"))
  check(grepl("UNREGISTERED", s$status), paste("3:", ver, "status says UNREGISTERED"))
}
check(identical(mmm_behavior_config_sha256(CFG), sha0) && identical(sha0, mmm_behavior_config_sha256(MMM_BEHAVIOR_CONFIG)),
      "3: the frozen configuration is not modified")
check(!identical(W$RAW29_VERSIONS$flip2s$manifest_sha256, W$RAW29_VERSIONS$noflip$manifest_sha256), "3: the two versions have different manifests")
ok("both raw versions resolve as declared, non-release data versions; the frozen config hash is unchanged")

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
