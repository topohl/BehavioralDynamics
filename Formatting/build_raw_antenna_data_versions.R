# ================================================================
# Build the raw-antenna data versions (filtered and unfiltered) in the parallel root RANT
# MMMSociability -- Formatting/build_raw_antenna_data_versions.R
# ================================================================
# Reads (never writes): the raw MultiMouseMonitor logs, the vendor AnimalPos exports (tag map) and the vendor
# preprocessed files of data version v2 (roster, AnimalID spelling, phase calendar) under the live data root.
# Writes: <parent of the data root>/RANT/MMMSociability/data_versions/<version>/ -- one folder per version, refused
# if it exists (versions are immutable), and refused if it would lie inside the live data root.
#
# Versions (decided 2026-10-12):
#   raw_antenna_flip2s_<date>  Stage 19 rules incl. the 2-s flip filter: the primary raw-derived sensitivity input;
#   raw_antenna_noflip_<date>  the same without the flip filter: the audit of filter dependence.
# Each folder: preprocessed_data/ (24 files, vendor names and 12-column schema), MANIFEST_SHA256.csv,
# seed_provenance.csv (age of the detection behind every 18:30 seed), build_audit.csv, README.txt.
# The cage-to-antenna-board audit (all 4 cage changes x 6 batches) is written once, to
# RANT/MMMSociability/data_versions/board_audit_<date>/.
#
# Run from the repository root:  Rscript Formatting/build_raw_antenna_data_versions.R
# ================================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(data.table) })
source("Functions/project_paths.R")
source("Functions/animalpos_preprocessing_helpers.R")
source("Functions/behavioral_dynamics_helpers.R")
source("Functions/antenna_log_helpers.R")
source("Functions/antenna_data_version.R")

BUILD_DATE <- getOption("mmm.antenna_dv_date", format(Sys.Date(), "%Y%m%d"))
LIVE_ROOT <- mmm_project_root()
RANT_ROOT <- getOption("mmm.rant_root", file.path(dirname(LIVE_ROOT), "RANT"))
VENDOR_DIR <- file.path(LIVE_ROOT, "MMMSociability", "data_versions", "v2_cage_label_correction_2026-09-28", "preprocessed_data")
VENDOR_V1_DIR <- file.path(LIVE_ROOT, "MMMSociability", "preprocessed_data")
TAG_DIR <- file.path(LIVE_ROOT, "MMMSociability", "raw_data")
RAW_DIR <- getOption("mmm.raw_antenna_dir", file.path(dirname(dirname(dirname(LIVE_ROOT))), "Raw Data", "Behavior", "RFID", "MMMdata"))
VERSIONS <- c(flip2s = 2, noflip = 0)

norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
if (startsWith(tolower(norm(RANT_ROOT)), tolower(norm(LIVE_ROOT)))) stop("RANT_ROOT lies inside the live data root.", call. = FALSE)
for (d in c(VENDOR_DIR, TAG_DIR, RAW_DIR)) if (!dir.exists(d)) stop("Missing input folder: ", d, call. = FALSE)
dv_root <- file.path(RANT_ROOT, "MMMSociability", "data_versions")
out_dirs <- setNames(file.path(dv_root, paste0("raw_antenna_", names(VERSIONS), "_", BUILD_DATE)), names(VERSIONS))
board_dir <- file.path(dv_root, paste0("board_audit_", BUILD_DATE))
for (d in c(out_dirs, board_dir)) if (dir.exists(d)) stop("Version folder exists (immutable): ", d, call. = FALSE)
for (d in out_dirs) dir.create(file.path(d, "preprocessed_data"), recursive = TRUE)
dir.create(board_dir, recursive = TRUE)

vendor_files <- sort(list.files(VENDOR_DIR, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$", full.names = TRUE))
if (length(vendor_files) != 24) stop("Expected 24 vendor files, found ", length(vendor_files), call. = FALSE)
# Test runs only: restrict to files matching a pattern (a real version must hold all 24).
ONLY <- getOption("mmm.antenna_dv_only", NULL)
if (!is.null(ONLY)) vendor_files <- vendor_files[grepl(ONLY, basename(vendor_files))]
sha <- function(f) digest::digest(file = f, algo = "sha256")

seeds_all <- list(); audit_all <- list(); manifest_all <- list(); board_animal <- list(); board_blocks <- list()
for (vf in vendor_files) {
  batch <- sub("^E9_SIS_(B[0-9]+)_.*$", "\\1", basename(vf)); cc <- sub("^.*_(CC[0-9]+)_.*$", "\\1", basename(vf))
  vendor <- fread(vf, colClasses = "character")
  v1 <- fread(file.path(VENDOR_V1_DIR, basename(vf)), colClasses = "character", select = c("AnimalID", "System"))
  rec <- rfid_read_recording(RAW_DIR, batch, cc)
  tags <- rfid_animalpos_tag_map(file.path(TAG_DIR, batch, sprintf("E9_SIS_%s_%s_AnimalPos.csv", batch, cc)))
  for (v in names(VERSIONS)) {
    b <- antenna_dv_build_recording(vendor, rec, tags, VERSIONS[[v]])
    target <- file.path(out_dirs[[v]], "preprocessed_data", basename(vf))
    fwrite(b$rows, target)
    manifest_all[[paste(v, basename(vf))]] <- data.frame(version = v, file = basename(vf), sha256 = sha(target),
      bytes = file.size(target), rows = nrow(b$rows), seed_rows = sum(b$rows$is_synthetic_preprocessing_row),
      vendor_v2_sha256 = sha(vf), raw_log = basename(rec$path))
    seeds_all[[paste(v, basename(vf))]] <- data.frame(version = v, SourceFile = basename(vf), Batch = batch, CageChange = cc, b$seeds)
    audit_all[[paste(v, basename(vf))]] <- data.frame(version = v, SourceFile = basename(vf), Batch = batch, CageChange = cc, b$audit)
    if (v == "flip2s") {
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
  message(sprintf("%s %s: %s -> %s rows (flip2s), %s rows (noflip)", batch, cc, basename(rec$path),
                  manifest_all[[paste("flip2s", basename(vf))]]$rows, manifest_all[[paste("noflip", basename(vf))]]$rows))
  rm(rec)
}

manifest <- bind_rows(manifest_all); seeds <- bind_rows(seeds_all); audit <- bind_rows(audit_all)
for (v in names(VERSIONS)) {
  d <- out_dirs[[v]]
  fwrite(manifest[manifest$version == v, ], file.path(d, "MANIFEST_SHA256.csv"))
  fwrite(seeds[seeds$version == v, ], file.path(d, "seed_provenance.csv"))
  fwrite(audit[audit$version == v, ], file.path(d, "build_audit.csv"))
  writeLines(c(
    sprintf("Exp9 RFID raw-antenna data version '%s' (built %s)", basename(d), BUILD_DATE),
    "",
    "The raw MultiMouseMonitor antenna logs in the schema of the vendor preprocessed AnimalPos files (same 24 file names,",
    "same 12 columns), so Stages 01/02 and the Stage 29 event stream read them unchanged. Built by",
    "Formatting/build_raw_antenna_data_versions.R with Functions/antenna_data_version.R and antenna_log_helpers.R.",
    sprintf("Flip filter: %s.", if (VERSIONS[[v]] > 0) sprintf("a run at one antenna shorter than %g s that returns to the antenna it came from is reassigned to it", VERSIONS[[v]]) else "none (every antenna change counts)"),
    "Rows: one row per change of antenna reader per animal (its first detection at the new reader); System = sys.<antenna board>.",
    "Seeds: every animal has a row at the span start (18:30 of the cage-change day) with the reader of its last detection",
    "before it, marked is_synthetic_preprocessing_row = TRUE, synthetic_row_source = raw_seed_last_detection_before_span;",
    "seed_provenance.csv gives the age of that detection (seed_detection_age_s). No vendor position is ever used as a seed.",
    "Calendar: the vendor v2 file's phase blocks; Phase/ConsecActive/ConsecInactive copied per block; HalfHoursElapsed from",
    "the shared helper. Inputs: vendor v2 preprocessed files (roster, spelling, calendar), raw_data AnimalPos exports (tag map),",
    "raw logs. The repeated daylight-saving hour of B1 CC1 is dropped; empty software sessions are ignored (build_audit.csv).",
    "Immutable: a change needs a new version folder."), file.path(d, "README.txt"))
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
message("Done: ", paste(out_dirs, collapse = ", "), " and ", board_dir)
