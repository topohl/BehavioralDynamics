# ================================================================
# Build the raw-antenna data versions (filtered and unfiltered) in the parallel root RANT
# MMMSociability -- Formatting/build_raw_antenna_data_versions.R
# ================================================================
# Reads (never writes): the raw MultiMouseMonitor logs, the vendor AnimalPos exports (tag map) and the vendor
# preprocessed files of data version v2 (roster, AnimalID spelling, phase calendar) under the live data root.
# Writes: <parent of the data root>/RANT/MMMSociability/data_versions/<version>/ -- one folder per version, refused
# if it exists (versions are immutable), and refused if it would lie inside the live data root.
#
# Versions (ANTENNA_DV_VERSIONS in Functions/antenna_data_version.R; decided 2026-10-12/13):
#   raw_antenna_flip2s_<date>  Stage 19 rules incl. the 2-s flip filter: the primary raw-derived sensitivity input (RANT);
#   raw_antenna_noflip_<date>  the same without the flip filter: the audit of filter dependence (built in RANT, copied
#                              hash-checked into RANU);
#   raw_antenna_ge200_<date>   only relocations of >= 200 grid units, the vendor emission threshold (RANV);
#   raw_antenna_avg500_<date>  the reconstructed PhenoSys 500-ms window mean of the reads without its 200-unit gate (RANA).
# options(mmm.antenna_dv_versions) selects the versions (default flip2s and noflip); every selected version must name
# the target root, unless the root is not a sibling of the live root (test builds).
# Each folder: preprocessed_data/ (24 files, vendor names and 12-column schema), MANIFEST_SHA256.csv,
# seed_provenance.csv (age of the detection behind every 18:30 seed), build_audit.csv, README.txt.
# The cage-to-antenna-board audit (all 4 cage changes x 6 batches) is written with every build, to
# <root>/MMMSociability/data_versions/board_audit_<date>/. Folders are built as .tmp_<name> and renamed at the end.
#
# Run from the repository root:  Rscript Formatting/build_raw_antenna_data_versions.R
#   ge200:  Rscript -e 'options(mmm.rant_root = "<parent of the live root>/RANV", mmm.antenna_dv_versions = "ge200");
#                       source("Formatting/build_raw_antenna_data_versions.R")'
# ================================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(data.table) })
source("Functions/project_paths.R")
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/behavioral_dynamics_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/animalpos_reconstruction.R")
source("Functions/antenna_data_version.R")

BUILD_DATE <- getOption("mmm.antenna_dv_date", format(Sys.Date(), "%Y%m%d"))
LIVE_ROOT <- mmm_project_root()
RANT_ROOT <- getOption("mmm.rant_root", file.path(dirname(LIVE_ROOT), "RANT"))
VENDOR_DIR <- file.path(LIVE_ROOT, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "preprocessed_data")
VENDOR_V1_DIR <- file.path(LIVE_ROOT, "MMMSociability", "preprocessed_data")
TAG_DIR <- file.path(LIVE_ROOT, "MMMSociability", "raw_data")
RAW_DIR <- getOption("mmm.raw_antenna_dir", file.path(dirname(dirname(dirname(LIVE_ROOT))), "Raw Data", "Behavior", "RFID", "MMMdata"))
version_names <- getOption("mmm.antenna_dv_versions", c("flip2s", "noflip"))
if (!length(version_names) || !all(version_names %in% names(ANTENNA_DV_VERSIONS)) || anyDuplicated(version_names))
  stop("mmm.antenna_dv_versions must name distinct versions out of ", paste(names(ANTENNA_DV_VERSIONS), collapse = ", "), call. = FALSE)
VERSIONS <- ANTENNA_DV_VERSIONS[version_names]

norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
root_parts <- toupper(strsplit(norm(RANT_ROOT), "/", fixed = TRUE)[[1]])
if (startsWith(tolower(norm(RANT_ROOT)), tolower(norm(LIVE_ROOT))) || toupper(basename(norm(LIVE_ROOT))) %in% root_parts)
  stop("RANT_ROOT lies inside the live data root.", call. = FALSE)   # by path or by name (a UNC spelling of S: differs)
version_roots <- toupper(c(vapply(ANTENNA_DV_VERSIONS, function(s) s$root, ""), "RANU"))
sibling <- identical(tolower(dirname(norm(RANT_ROOT))), tolower(dirname(norm(LIVE_ROOT))))
if (sibling || any(root_parts %in% version_roots)) {   # a version root, however its parent path is spelled
  wrong <- names(VERSIONS)[vapply(VERSIONS, function(s) !identical(toupper(s$root), utils::tail(root_parts, 1)), logical(1))]
  if (length(wrong)) stop("Version(s) ", paste(wrong, collapse = ", "), " belong in another root than ", RANT_ROOT, call. = FALSE)
}
ONLY <- getOption("mmm.antenna_dv_only", NULL)   # test builds only: files matching a pattern
if (!is.null(ONLY) && (!is.character(ONLY) || length(ONLY) != 1 || !nzchar(ONLY) || sibling || any(root_parts %in% version_roots)))
  stop("mmm.antenna_dv_only must be a non-empty pattern, and test builds must not write into a version root.", call. = FALSE)
for (d in c(VENDOR_DIR, TAG_DIR, RAW_DIR)) if (!dir.exists(d)) stop("Missing input folder: ", d, call. = FALSE)
GIT_COMMIT <- NULL
if (is.null(ONLY)) {   # a real build: committed code only, and git must answer (fails closed)
  code <- c("Formatting/build_raw_antenna_data_versions.R", "Functions/antenna_data_version.R", "Functions/antenna_log_helpers.R",
            "Functions/animalpos_reconstruction.R", "Functions/project_paths.R", "Functions/animalpos_preprocessing_helpers.R",
            "Functions/behavioral_dynamics_helpers.R")
  dirty <- suppressWarnings(system2("git", c("-C", getwd(), "status", "--porcelain", "--untracked-files=all", "--", code), stdout = TRUE))
  if (!is.null(attr(dirty, "status"))) stop("git status failed: a real build needs a git work tree with committed code.", call. = FALSE)
  if (length(dirty)) stop("Commit the build code first:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  GIT_COMMIT <- suppressWarnings(system2("git", c("-C", getwd(), "rev-parse", "HEAD"), stdout = TRUE))
  if (!is.null(attr(GIT_COMMIT, "status")) || length(GIT_COMMIT) != 1 || !grepl("^[0-9a-f]{40}$", GIT_COMMIT))
    stop("Cannot resolve the git commit of the build code.", call. = FALSE)
}
dv_root <- file.path(RANT_ROOT, "MMMSociability", "data_versions")
out_dirs <- setNames(file.path(dv_root, paste0("raw_antenna_", names(VERSIONS), "_", BUILD_DATE)), names(VERSIONS))
board_dir <- file.path(dv_root, paste0("board_audit_", BUILD_DATE))
final_dirs <- c(out_dirs, board = board_dir)
tmp_dirs <- setNames(file.path(dirname(final_dirs), paste0(".tmp_", basename(final_dirs))), names(final_dirs))
for (d in final_dirs) if (dir.exists(d)) stop("Version folder exists (immutable): ", d, call. = FALSE)
for (d in tmp_dirs) if (dir.exists(d)) stop("A partial build exists; inspect and remove it first: ", d, call. = FALSE)
out_dirs <- tmp_dirs[names(VERSIONS)]; board_dir <- tmp_dirs[["board"]]   # built here, renamed at the end
for (d in out_dirs) dir.create(file.path(d, "preprocessed_data"), recursive = TRUE)
dir.create(board_dir, recursive = TRUE)

vendor_files <- sort(list.files(VENDOR_DIR, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$", full.names = TRUE))
if (length(vendor_files) != 24) stop("Expected 24 vendor files, found ", length(vendor_files), call. = FALSE)
# Test runs only: restrict to files matching a pattern (a real version must hold all 24).
if (!is.null(ONLY)) vendor_files <- vendor_files[grepl(ONLY, basename(vendor_files))]
if (!length(vendor_files)) stop("mmm.antenna_dv_only matches no recording.", call. = FALSE)
sha <- function(f) digest::digest(file = f, algo = "sha256")

seeds_all <- list(); audit_all <- list(); manifest_all <- list(); board_animal <- list(); board_blocks <- list()
for (vf in vendor_files) {
  batch <- sub("^E9_SIS_(B[0-9]+)_.*$", "\\1", basename(vf)); cc <- sub("^.*_(CC[0-9]+)_.*$", "\\1", basename(vf))
  vendor <- fread(vf, colClasses = "character")
  v1 <- fread(file.path(VENDOR_V1_DIR, basename(vf)), colClasses = "character", select = c("AnimalID", "System"))
  with_reads <- any(vapply(VERSIONS, function(s) !is.null(s$estimator), logical(1)))   # read-level times only when needed
  rec_reads <- rfid_read_recording(RAW_DIR, batch, cc, keep_read_offsets = with_reads)
  rec <- rec_reads; rec$events <- rec$events[, setdiff(names(rec$events), c("read_offsets_ms", "file_order"))]   # = default read
  tags <- rfid_animalpos_tag_map(file.path(TAG_DIR, batch, sprintf("E9_SIS_%s_%s_AnimalPos.csv", batch, cc)))
  for (v in names(VERSIONS)) {
    b <- if (is.null(VERSIONS[[v]]$estimator)) {
      antenna_dv_build_recording(vendor, rec, tags, VERSIONS[[v]]$min_flip_sec, move_rule = VERSIONS[[v]]$move_rule)
    } else antenna_dv_build_recording_window(vendor, rec_reads, tags, window_ms = VERSIONS[[v]]$estimator$window_ms)
    target <- file.path(out_dirs[[v]], "preprocessed_data", basename(vf))
    fwrite(b$rows, target)
    manifest_all[[paste(v, basename(vf))]] <- data.frame(version = v, file = basename(vf), sha256 = sha(target),
      bytes = file.size(target), rows = nrow(b$rows), seed_rows = sum(b$rows$is_synthetic_preprocessing_row),
      vendor_v2_sha256 = sha(vf), raw_log = basename(rec$path))
    seeds_all[[paste(v, basename(vf))]] <- data.frame(version = v, SourceFile = basename(vf), Batch = batch, CageChange = cc, b$seeds)
    audit_all[[paste(v, basename(vf))]] <- data.frame(version = v, SourceFile = basename(vf), Batch = batch, CageChange = cc, b$audit)
    if (v == names(VERSIONS)[1]) {   # the board audit does not depend on the version
      cal <- antenna_dv_vendor_calendar(vendor)
      init_files <- list.files(file.path(RAW_DIR, batch, "raw"),
                               pattern = sprintf("Batch%d_change_%d_.*initScan[.]csv$", as.integer(sub("B", "", batch)), as.integer(sub("CC", "", cc))),
                               full.names = TRUE)
      init <- bind_rows(lapply(init_files, antenna_dv_initscan_boards))
      init <- if (nrow(init)) init %>% group_by(tag) %>% slice_max(initscan_reads, n = 1, with_ties = FALSE) %>% ungroup() else
        data.frame(tag = character(), initscan_board = integer(), initscan_reads = integer(), initscan_board_share = numeric())   # no placement scan (Batch 1)
      blk <- antenna_dv_board_by_block(rec$events, tags, b$seeds$AnimalNum, cal$span_start, b$audit$span_end[1])
      board_blocks[[basename(vf)]] <- data.frame(SourceFile = basename(vf), Batch = batch, CageChange = cc, blk)
      ids <- vendor %>% distinct(AnimalID, System_v2 = System) %>%
        left_join(v1 %>% distinct(AnimalID, System_v1 = System), by = "AnimalID") %>%
        mutate(AnimalNum = canonical_animal_id(AnimalID))
      board_animal[[basename(vf)]] <- b$audit %>%
        select(AnimalNum, tag, Board, events_in_span, events_off_board, detected_seconds, span_seconds) %>%
        left_join(ids %>% select(AnimalNum, AnimalID, System_v1, System_v2), by = "AnimalNum") %>%
        left_join(init, by = "tag") %>%
        left_join(blk %>% left_join(b$audit %>% select(AnimalNum, Board), by = "AnimalNum") %>%
                    group_by(AnimalNum) %>%
                    summarise(n_blocks = n(), n_blocks_other_board = sum(block_board != Board),
                              min_block_board_share = min(block_board_share), .groups = "drop"), by = "AnimalNum") %>%
        mutate(SourceFile = basename(vf), Batch = batch, CageChange = cc,
               share_on_board = 1 - events_off_board / events_in_span,
               board_eq_v2_label = paste0("sys.", Board) == System_v2, board_eq_v1_label = paste0("sys.", Board) == System_v1,
               board_eq_initscan = !is.na(initscan_board) & initscan_board == Board,
               initscan_files = paste(basename(init_files), collapse = "|"), .before = 1) %>%
        as.data.frame()
    }
  }
  message(sprintf("%s %s: %s -> %s", batch, cc, basename(rec$path),
                  paste(sprintf("%s rows (%s)", vapply(names(VERSIONS), function(v) manifest_all[[paste(v, basename(vf))]]$rows, numeric(1)),
                                names(VERSIONS)), collapse = ", ")))
  rm(rec)
}

of_version <- function(lst, v) bind_rows(lst[startsWith(names(lst), paste0(v, " "))])   # per version: no foreign columns
for (v in names(VERSIONS)) {
  d <- out_dirs[[v]]
  fwrite(of_version(manifest_all, v), file.path(d, "MANIFEST_SHA256.csv"))
  fwrite(of_version(seeds_all, v), file.path(d, "seed_provenance.csv"))
  fwrite(of_version(audit_all, v), file.path(d, "build_audit.csv"))
  writeLines(antenna_dv_readme_lines(basename(final_dirs[[v]]), BUILD_DATE, VERSIONS[[v]],
                                     git_commit = if (is.null(VERSIONS[[v]]$move_rule) && is.null(VERSIONS[[v]]$estimator)) NULL else GIT_COMMIT),
             file.path(d, "README.txt"))
}
fwrite(bind_rows(board_animal), file.path(board_dir, "board_by_animal_recording.csv"))
fwrite(bind_rows(board_blocks), file.path(board_dir, "board_by_animal_phase_block.csv"))
ba <- bind_rows(board_animal)
summary_tbl <- data.frame(
  animal_recordings = nrow(ba),
  recordings = n_distinct(ba$SourceFile),
  board_eq_v2_label = sum(ba$board_eq_v2_label), board_eq_v1_label = sum(ba$board_eq_v1_label),
  with_initscan = sum(!is.na(ba$initscan_board)), board_eq_initscan = sum(ba$board_eq_initscan),
  off_board_events = sum(ba$events_off_board), min_share_on_board = min(ba$share_on_board),
  animals_with_block_on_other_board = sum(ba$n_blocks_other_board > 0, na.rm = TRUE),
  min_block_board_share = min(ba$min_block_board_share, na.rm = TRUE))
fwrite(summary_tbl, file.path(board_dir, "board_audit_summary.csv"))
print(summary_tbl)
print(ba %>% filter(!board_eq_v2_label | !board_eq_v1_label | (!is.na(initscan_board) & !board_eq_initscan) | n_blocks_other_board > 0) %>%
        select(Batch, CageChange, AnimalID, Board, System_v1, System_v2, initscan_board, initscan_board_share, n_blocks_other_board, min_block_board_share))
for (k in names(final_dirs)) if (!file.rename(tmp_dirs[[k]], final_dirs[[k]])) stop("Could not rename ", tmp_dirs[[k]], call. = FALSE)
message("Done: ", paste(final_dirs, collapse = ", "))
