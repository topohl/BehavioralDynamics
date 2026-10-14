# ================================================================
# Audit: animals present in every Exp9 RFID cage episode (design records, antenna reads, tracked roster)
# MMMSociability -- Testing/audits/audit_cage_episode_presence.R
# ================================================================
# UNREGISTERED design audit (2026-10-14). No RES/SUS label, CombZ, outcome or body-weight column is read. The planning
# sheets are read as whole sheets and also contain CON/SIS condition labels; those cells are not used. The only group
# information used is the planned fixed cage (the four animals the plan never regroups), which is the CON cage; it is
# derived without reading a group column: the fixed animals are the batch's planned animals without a regrouping letter.
#
# Unit: the cage episode = Batch x CC x antenna board (120 = 24 recordings x 5 boards). For every cage episode:
#   n_animals_present  animals housed according to detection-independent records: the planned group of the CC (letter
#                      plan R1-R4 + letter-to-animal key, regrouping lists with planned systems, the B1 CC1 PhenoSys
#                      configuration), minus planned animals whose Animals-sheet end date is on or before the CC's
#                      start day, corrected for executed deviations from the plan that an independent record confirms
#                      (the post-CC4 sucrose-sheet cage equals the detection board)
#   n_tags_present     tags with >= CEP_MIN_EVENTS in-span detection events whose majority board is this board (raw
#                      MultiMouseMonitor log of the recording; roster and non-roster tags)
#   n_tracked          v2 roster animals on the board (Stage 00b animal_qc: majority board of the in-span events)
#   n_untagged         housed animals (counted in n_animals_present) with no RFID tag in any record
#   fixed_cage         TRUE for the cage episode that holds the planned fixed group
#   classification     design-known (records agree), conflict (an executed deviation, or a post-CC4 record that
#                      disagrees with the plan), detection-derived (no design record; none expected)
#   presence_source    codes of the records behind n_animals_present (CEP_SOURCE_CODES)
# Board of a planned group: its planned system list; where the CC has none (B1 CC2-CC4, B2 CC1/CC4, B3 CC2-CC4), the
# post-CC4 sucrose-sheet cage (CC4) or else the board of the group's tracked animals. The count always comes from the
# design records; only the board label of such a group comes from detection (column `placement`).
# Planned systems: a list labelled <animal>_sys.<k> is assigned to the CC whose planned partition it follows (exactly
# one CC must fit). Letter plan: the social-golfer rounds CEP_ROUNDS in the order CEP_ROUND_ORDER; every complete
# 4 x 4 letter block of a planning sheet is checked against that order (the run stops on a mismatch).
# Tags: the roster tag (vendor export behind the v2 files) where the animal is tracked, else the planning workbooks'
# tag (B1: the PhenoSys configuration); disagreements are listed in tables/checks.csv. A tag read on several boards
# counts on its majority board. Raw logs are read with rfid_read_raw_log() (clock-fold rows dropped, as in Stage 00b).
#
# Inputs (read-only; <Exp> = the experiment folder that holds Analysis/Behavior/RFID):
#   <Exp>/Planning/E9_Experimental Timeline.xlsx  Group_Composition (B1 letter key); Animals: only ID, Cage, Batch,
#                                                 Mark date, End date, Method, Notes (located by header, read singly)
#   <Exp>/Planning/GroupCompBatch2.xlsx, GroupCompBatch3.xlsx (Sheet1), GroupComposition.xlsx (Batch 4, Batch5,
#     Batch 6): letter key, letter blocks, planned system lists; GroupComposition.xls (same sheets): comparison only
#   <Exp>/Planning/E9_Behavior_Templates.xls      Kill_List, columns A:C only (Run, ID, Cage)
#   <Exp>/Raw Data/Behavior/RFID/sis_phenosys/Batch1_SIS_5cage.xlsx  Parameter (Individual label and rfid rows)
#   <Exp>/Analysis/SIS_Analysis/E9_Behavior_Data_before_restructure.xlsx  sucrosePreference, columns A and C:D only
#     (ID, Cage, Batch; column B is never read)
#   <project root>/analysis_ready/quality_control/raw_acquisition/runs/<qc run>/tables/  (Stage 00b run, default
#     CEP_QC_RUN): animal_qc.csv (roster, board, tag), board_qc.csv, recording_qc.csv (raw log, span), inputs.csv
#   <project root>/MMMSociability/raw_data/excluded_animals.csv
#   <raw root>/B<n>/raw/  the 24 recordings named in recording_qc.csv and every *initScan.csv placement scan
#   <repo>/docs/spatial_exclusions/spatial_analysis_exclusions_v1.1.csv  (record ids of censored cage episodes)
# Outputs: <OUT>/runs/<run_id>/, built in runs/.tmp_<run_id>/ and renamed when complete (an existing folder is refused):
#   tables/*.csv (CEP_FILE_DOCS), manifest/run.csv, manifest/inputs.csv (every input with SHA-256),
#   manifest/output_files_sha256.csv and README.txt. OUT = --output-root=<absolute>, default
#   <Behavior>/RANM/cage_episode_presence (<Behavior> = dirname(mmm_project_root())), checked by
#   mmm_isolated_output_root_check(); run id = --run-id=<id>, default cep_v1_<short commit>; --qc-run=<Stage 00b run id>.
#
# Run from the repository root (one R process; reads 24 raw logs and the placement scans, a few minutes):
#   Rscript Testing/audits/audit_cage_episode_presence.R [--run-id=<new id>] [--output-root=<absolute>] [--qc-run=<id>]
# ================================================================

CEP_VERSION <- "cep_v1"
CEP_SCRIPT <- "Testing/audits/audit_cage_episode_presence.R"
CEP_QC_RUN <- "raqc_20261014_6165adf"
CEP_MIN_EVENTS <- 100L
CEP_BOARDS <- 1:5
CEP_TAG_PATTERN <- "^041[0-9A-F]{7}$"
CEP_ID_PATTERN <- "^([A-Z]{3}[0-9]-)?([A-Z]{2})?[0-9]+$"
CEP_SYS_PATTERN <- "[-_]sys[.][0-9]*$"
CEP_LETTER_PATTERN <- "^[A-P]$"
CEP_EXCLUSION_MANIFEST <- "docs/spatial_exclusions/spatial_analysis_exclusions_v1.1.csv"
CEP_ANIMAL_COLUMNS <- c(ID = "ID", home_cage_pre = "Cage", BatchNo = "Batch", mark_date = "Mark date", end_date = "End date",
                        method = "Method", notes = "Notes")

# Social-golfer design (E9_Experimental Timeline.xlsx::Group_Composition, "4x4 individuals, each meets one once"):
# the four groups of letters of each regrouping round.
CEP_ROUNDS <- list(R1 = list(c("A", "E", "I", "M"), c("B", "F", "J", "N"), c("C", "G", "K", "O"), c("D", "H", "L", "P")),
                   R2 = list(c("A", "F", "K", "P"), c("B", "E", "L", "O"), c("C", "H", "I", "N"), c("D", "G", "J", "M")),
                   R3 = list(c("A", "G", "L", "N"), c("B", "H", "K", "M"), c("C", "E", "J", "P"), c("D", "F", "I", "O")),
                   R4 = list(c("A", "H", "J", "O"), c("B", "G", "I", "P"), c("C", "F", "L", "M"), c("D", "E", "K", "N")))
# Round of CC1-CC4 per batch: the order of the regrouping blocks in the planning workbooks (B4 and B5: RG2 holds R4,
# RG3 R2 and RG4 R3). Checked against every complete letter block found in the sheets.
CEP_ROUND_ORDER <- list(B1 = c("R1", "R2", "R3", "R4"), B2 = c("R1", "R2", "R3", "R4"), B3 = c("R1", "R2", "R3", "R4"),
                        B4 = c("R1", "R4", "R2", "R3"), B5 = c("R1", "R4", "R2", "R3"), B6 = c("R1", "R2", "R3", "R4"))

# Planning sheets (relative to <Exp>/Planning). use = plan: letter key, letter blocks, planned system lists;
# use = comparison: planned system lists only, compared with the plan (GroupComposition.xls, a later edit).
# plan_code / list_code: the presence_source code of the batch's plan and of a planned system list from the sheet.
CEP_PLAN_SHEETS <- data.frame(
  Batch = c("B1", "B2", "B3", "B4", "B5", "B6", "B4", "B5", "B6"),
  file = c("E9_Experimental Timeline.xlsx", "GroupCompBatch2.xlsx", "GroupCompBatch3.xlsx", rep("GroupComposition.xlsx", 3),
           rep("GroupComposition.xls", 3)),
  sheet = c("Group_Composition", "Sheet1", "Sheet1", "Batch 4", "Batch5", "Batch 6", "Batch 4", "Batch5", "Batch 6"),
  use = c(rep("plan", 6), rep("comparison", 3)),
  plan_code = c("PLAN", "PLAN", "PLAN", "RGSYS", "RGSYS", "RGSYS", NA, NA, NA),
  list_code = c(NA, "SYSLIST", "SYSLIST", NA, NA, NA, NA, NA, NA), stringsAsFactors = FALSE)
CEP_B1_CONFIG_SOURCE <- "sis_phenosys/Batch1_SIS_5cage.xlsx"
CEP_B1_CONFIG_CODE <- "CFG1"

CEP_SOURCE_CODES <- c(
  PLAN = "letter plan R1-R4 with the letter-to-animal key (E9_Experimental Timeline.xlsx::Group_Composition for B1, GroupCompBatch2/3.xlsx for B2/B3)",
  RGSYS = "GroupComposition.xlsx regrouping lists RG1-RG4 with planned systems (B4-B6)",
  CFG1 = "PhenoSys configuration Batch1_SIS_5cage.xlsx::Parameter, Individual list with systems incl. untagged animals (B1 CC1)",
  SYSLIST = "planned system list (<animal>_sys.<k>) of GroupCompBatch2/3.xlsx for this CC",
  ANIM = "E9_Experimental Timeline.xlsx::Animals end dates (an animal of the batch left during the recordings)",
  SUC = "post-CC4 home cage: sucrosePreference sheet of E9_Behavior_Data_before_restructure.xlsx (ID, Cage, Batch)",
  KILL = "end-of-experiment cage: E9_Behavior_Templates.xls::Kill_List (Run, ID, Cage)")

CEP_FILE_DOCS <- c(
  "tables/cage_episode_presence.csv" = "one row per cage episode (120): Batch, CC, board, System, CageEpisodeID, fixed_cage, n_animals_present, n_tags_present, n_tracked, n_untagged, presence_source, classification, note",
  "tables/cage_episode_presence_full.csv" = "the same 120 rows with every intermediate count and list: roster, raw-log and initScan tags, planned, removed, excluded and untracked animals, deviations, post-CC4 records, Stage 00b board QC",
  "tables/planned_animal_episodes.csv" = "per batch x CC x planned slot: the animal, letter and planned group, tag, Animals record, alive / untagged / excluded, planned system (xlsx and xls), post-CC4 sucrose and kill-list cage, initScan board, roster board, group board and its source, deviation",
  "tables/design_key.csv" = "per planned animal: letter (none = planned fixed cage), tag and its source, the planning sheets naming it",
  "tables/planned_system_lists.csv" = "every planned system list read (<animal>_sys.<k> rows and the B1 configuration) and the CC whose planned partition it follows",
  "tables/rawlog_tag_board_events.csv" = "every tag x board x span part (before / in / after span) of the 24 recordings: events, first and last event, labels, animal",
  "tables/initscan_tag_board_events.csv" = "every tag x board of every initScan placement scan: events, first and last event, labels, animal, whether the scan is the recording's",
  "tables/checks.csv" = "structure, coverage and consistency checks (planning blocks, lists, keys, records, detection vs Stage 00b)",
  "manifest/run.csv" = "run id, version, git commit, script SHA-256 and git status, QC run, roots, minimum events, R and package versions, times",
  "manifest/inputs.csv" = "every input file: role, path, SHA-256, bytes, modification time",
  "manifest/output_files_sha256.csv" = "SHA-256 and size of every other file of the run",
  "README.txt" = "this file")

# ------------------------------------------------------------------ small helpers

cep_args <- function(args) {
  known <- "^--(run-id|output-root|qc-run)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  list(run_id = val("run-id"), output_root = val("output-root"), qc_run = val("qc-run"))
}

cep_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

# Animal identity from planning IDs and system labels: drop the line prefix (HKH0-, LUM0-) and a _sys.<k> suffix,
# then canonical_animal_id() (numeric IDs lose leading zeros).
cep_animal <- function(x) {
  x <- toupper(trimws(as.character(x)))
  canonical_animal_id(sub("[-_]SYS.*$", "", sub("^[A-Z]{3}[0-9]-", "", x)))
}

# Planning IDs written without the OR prefix (B3-B5 sheets, sucrose sheet, kill list): the batch's planned animal of
# that ID, else the same ID with OR, else NA.
cep_resolve <- function(ids, universe) {
  data.table::fifelse(ids %in% universe, ids, data.table::fifelse(paste0("OR", ids) %in% universe, paste0("OR", ids), NA_character_))
}

cep_utc <- function(t, fmt = "%Y-%m-%d %H:%M:%S") format(as.POSIXct(t, origin = "1970-01-01", tz = "UTC"), fmt)

cep_check <- function(check, value, ok = NA) data.table::data.table(check = check, value = as.character(value), ok = ok)

cep_join <- function(x) if (length(x)) paste(x, collapse = ";") else ""

# A whole sheet as a character matrix (empty cells NA). Leading empty rows are skipped by readxl; rows are grid rows.
cep_read_grid <- function(path, sheet) {
  g <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  m <- as.matrix(as.data.frame(g, stringsAsFactors = FALSE))
  dimnames(m) <- NULL
  m[] <- trimws(m)
  m[!is.na(m) & !nzchar(m)] <- NA_character_
  m
}

# ------------------------------------------------------------------ planning records

# Rows holding >= 4 system labels (<animal>_sys.<k>): one planned system list each.
cep_system_lists <- function(m, source, sheet) {
  out <- list()
  for (i in seq_len(nrow(m))) {
    v <- m[i, ]; v <- v[!is.na(v)]
    hit <- v[grepl(CEP_SYS_PATTERN, v)]
    if (length(hit) >= 4L) {
      out[[length(out) + 1L]] <- data.table::data.table(source = source, sheet = sheet, list_no = length(out) + 1L, grid_row = i, label = hit,
                                                        AnimalNum = cep_animal(hit), sys = suppressWarnings(as.integer(sub("^.*sys[.]", "", hit))))
    }
  }
  data.table::rbindlist(out)
}

# Cells holding a letter A-P whose right neighbour is an animal ID; tag = first tag-like cell 2-4 columns further on
# (before the next letter cell).
cep_letter_cells <- function(m) {
  idx <- which(!is.na(m) & grepl(CEP_LETTER_PATTERN, m), arr.ind = TRUE)
  out <- data.table::data.table(grid_row = as.integer(idx[, 1]), grid_col = as.integer(idx[, 2]))[grid_col < ncol(m)]
  out[, `:=`(letter = m[cbind(grid_row, grid_col)], raw_id = m[cbind(grid_row, grid_col + 1L)])]
  out <- out[!is.na(raw_id) & grepl(CEP_ID_PATTERN, toupper(raw_id))]
  out[, tag := vapply(seq_len(.N), function(k) {
    j <- grid_col[k] + 2:4; j <- j[j <= ncol(m)]
    v <- m[grid_row[k], j]
    stop_at <- which(!is.na(v) & grepl(CEP_LETTER_PATTERN, v))
    if (length(stop_at)) v <- v[seq_len(stop_at[1] - 1L)]
    hit <- v[!is.na(v) & grepl(CEP_TAG_PATTERN, toupper(v))]
    if (length(hit)) toupper(hit[1]) else NA_character_
  }, character(1))]
  out[order(grid_row, grid_col)]
}

# Complete 4 x 4 letter blocks (four consecutive grid rows with four letter cells A-P each, with or without an animal
# next to the letter); group j = the j-th letter of every row. Returns one row per block with the round it matches.
cep_letter_blocks <- function(m) {
  idx <- which(!is.na(m) & grepl(CEP_LETTER_PATTERN, m), arr.ind = TRUE)
  cells <- data.table::data.table(grid_row = as.integer(idx[, 1]), grid_col = as.integer(idx[, 2]), letter = m[idx])
  rows <- cells[, .N, by = grid_row][N == 4L][order(grid_row)]
  if (!nrow(rows)) return(data.table::data.table(block = integer(), first_row = integer(), groups = character(), round = character()))
  rows[, run := cumsum(c(1L, diff(grid_row) != 1L))]
  rows[, pos := seq_len(.N), by = run]
  rows[, chunk := paste(run, (pos - 1L) %/% 4L)]
  rows <- rows[, if (.N == 4L) .SD, by = chunk]
  blocks <- unique(rows$chunk)
  data.table::rbindlist(lapply(seq_along(blocks), function(k) {
    r <- rows[chunk == blocks[k]]$grid_row
    x <- cells[grid_row %in% r][order(grid_row, grid_col)]
    x[, pos_in_row := seq_len(.N), by = grid_row]
    groups <- lapply(1:4, function(jj) sort(x[pos_in_row == jj]$letter))
    fits <- names(CEP_ROUNDS)[vapply(CEP_ROUNDS, function(R) all(vapply(groups, function(g) any(vapply(R, function(h) setequal(g, h), logical(1))), logical(1))), logical(1))]
    data.table::data.table(block = k, first_row = min(r), groups = paste(vapply(groups, paste, "", collapse = ""), collapse = " "),
                           round = if (length(fits) == 1L) fits else NA_character_)
  }))
}

# B1 CC1 PhenoSys configuration: the Individual block of the Parameter sheet (label <animal>_sys.1k = system k, rfid).
cep_b1_config <- function(path) {
  m <- cep_read_grid(path, "Parameter")
  li <- which(m[, 1] %in% "label"); ri <- which(m[, 1] %in% "rfid")
  if (length(li) != 1L || length(ri) != 1L) stop("B1 configuration: expected one label and one rfid row.", call. = FALSE)
  lab <- m[li, -(1:2)]; rf <- m[ri, -(1:2)]; keep <- !is.na(lab)
  data.table::data.table(source = CEP_B1_CONFIG_SOURCE, sheet = "Parameter", list_no = 1L, grid_row = li, label = lab[keep],
                         AnimalNum = cep_animal(lab[keep]), sys = as.integer(substring(lab[keep], nchar(lab[keep]))),
                         rfid = toupper(rf[keep]), Batch = "B1")
}

# Animals sheet: the seven columns of CEP_ANIMAL_COLUMNS, each read on its own over the same row range.
cep_animal_records <- function(path) {
  hdr <- names(suppressMessages(readxl::read_excel(path, sheet = "Animals", n_max = 0)))
  pos <- match(CEP_ANIMAL_COLUMNS, hdr)
  if (anyNA(pos)) stop("Animals sheet: missing column(s) ", paste(CEP_ANIMAL_COLUMNS[is.na(pos)], collapse = ", "), call. = FALSE)
  rd <- function(j, n = NULL) {
    rng <- if (is.null(n)) readxl::cell_cols(j) else readxl::cell_limits(c(1L, j), c(n + 1L, j))
    suppressMessages(readxl::read_excel(path, sheet = "Animals", range = rng, col_types = "text"))
  }
  n <- nrow(rd(pos[1]))
  cols <- lapply(seq_along(pos), function(k) {
    x <- rd(pos[k], n)
    if (!identical(names(x), unname(CEP_ANIMAL_COLUMNS[k]))) stop("Animals sheet: column ", pos[k], " is not ", CEP_ANIMAL_COLUMNS[k], call. = FALSE)
    v <- unlist(x, use.names = FALSE); length(v) <- n; v
  })
  rec <- data.table::as.data.table(stats::setNames(cols, names(CEP_ANIMAL_COLUMNS)))
  xd <- function(v) as.Date(suppressWarnings(as.numeric(v)), origin = "1899-12-30")
  rec <- rec[!is.na(ID)]
  rec[, `:=`(AnimalNum = cep_animal(ID), Batch = paste0("B", trimws(BatchNo)), mark_date = xd(mark_date), end_date = xd(end_date))]
  rec[]
}

# Post-CC4 home cage: sucrosePreference columns A (ID) and C:D (Cage, Batch).
cep_sucrose <- function(path) {
  a <- suppressMessages(readxl::read_excel(path, sheet = "sucrosePreference", range = readxl::cell_cols("A:A"), col_types = "text"))
  cd <- suppressMessages(readxl::read_excel(path, sheet = "sucrosePreference", range = readxl::cell_cols("C:D"), col_types = "text"))
  if (!identical(names(a), "ID") || !identical(names(cd), c("Cage", "Batch")) || nrow(a) != nrow(cd))
    stop("sucrosePreference: expected columns A = ID, C:D = Cage, Batch of equal length.", call. = FALSE)
  s <- data.table::data.table(ID = a$ID, Cage = cd$Cage, BatchNo = cd$Batch)[!is.na(ID)]
  s[, `:=`(Batch = paste0("B", trimws(BatchNo)), id_canon = cep_animal(ID), sucpref_cage = suppressWarnings(as.integer(Cage)))]
  s[]
}

# Kill list: columns A:C (Run, ID, Cage); batch from the "E9 SIS Batch <n> Killing List" title rows, cage from the first
# row of each run.
cep_kill_list <- function(path) {
  k <- data.table::as.data.table(suppressMessages(readxl::read_excel(path, sheet = "Kill_List", range = readxl::cell_cols("A:C"),
                                                                     col_names = FALSE, col_types = "text", .name_repair = "minimal")))
  data.table::setnames(k, c("run", "ID", "cage"))
  k[, Batch := data.table::fifelse(grepl("Batch [0-9] Killing", run), paste0("B", sub("^.*Batch ([0-9]) Killing.*$", "\\1", run)), NA_character_)]
  k[, Batch := Batch[1], by = cumsum(!is.na(Batch))]
  k <- k[!is.na(Batch) & !is.na(ID) & nzchar(ID) & ID != "ID"]
  k[, cage := suppressWarnings(as.integer(cage))][, cage := cage[1], by = .(Batch, grp = cumsum(!is.na(run) & nzchar(run)))]
  k[, id_canon := cep_animal(ID)]
  k[]
}

cep_input_paths <- function(experiment, project_root, qc_run) {
  P <- file.path(experiment, "Planning")
  qc <- file.path(mmm_raw_acquisition_qc_root(project_root), "runs", qc_run, "tables")
  plan <- unique(CEP_PLAN_SHEETS$file)
  c(stats::setNames(file.path(P, plan), paste0("planning:", plan)),
    "planning:E9_Behavior_Templates.xls" = file.path(P, "E9_Behavior_Templates.xls"),
    "config:B1" = file.path(experiment, "Raw Data", "Behavior", "RFID", "sis_phenosys", "Batch1_SIS_5cage.xlsx"),
    "sucrose" = file.path(experiment, "Analysis", "SIS_Analysis", "E9_Behavior_Data_before_restructure.xlsx"),
    "qc:animal_qc" = file.path(qc, "animal_qc.csv"), "qc:board_qc" = file.path(qc, "board_qc.csv"),
    "qc:recording_qc" = file.path(qc, "recording_qc.csv"), "qc:inputs" = file.path(qc, "inputs.csv"),
    "excluded_animals" = file.path(project_root, "MMMSociability", "raw_data", "excluded_animals.csv"),
    "exclusion_manifest" = file.path(MMM_REPO_ROOT, CEP_EXCLUSION_MANIFEST))
}

cep_read_inputs <- function(paths) {
  P <- function(role) paths[[role]]
  grids <- lapply(seq_len(nrow(CEP_PLAN_SHEETS)), function(i) cep_read_grid(P(paste0("planning:", CEP_PLAN_SHEETS$file[i])), CEP_PLAN_SHEETS$sheet[i]))
  aq <- data.table::fread(P("qc:animal_qc"), select = c("Batch", "CC", "AnimalNum", "tag", "board"), colClasses = list(character = c("AnimalNum", "tag")))
  aq[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), tag = toupper(trimws(tag)))]
  aq[!is.na(tag) & !nzchar(tag), tag := NA_character_]
  ex <- data.table::fread(P("excluded_animals"), header = FALSE, colClasses = "character")
  list(grids = grids,
       b1_config = cep_b1_config(P("config:B1")),
       records = cep_animal_records(P("planning:E9_Experimental Timeline.xlsx")),
       sucrose = cep_sucrose(P("sucrose")),
       kill = cep_kill_list(P("planning:E9_Behavior_Templates.xls")),
       animal_qc = aq,
       board_qc = data.table::fread(P("qc:board_qc"), select = c("Batch", "CC", "board", "tags_present", "nonroster_events", "nonroster_tags_ge100",
                                                              "last_read", "tail_silence_s", "removal_signature")),
       recording_qc = data.table::fread(P("qc:recording_qc"), select = c("Batch", "CC", "raw_log", "span_start", "span_end", "first_event", "events",
                                                                      "clock_fold_rows_dropped")),
       qc_inputs = data.table::fread(P("qc:inputs"), colClasses = "character"),
       excluded = stats::na.omit(unique(canonical_animal_id(ex[[1]]))),
       manifest = data.table::fread(P("exclusion_manifest"), select = c("record_id", "action", "CageEpisodeID"), colClasses = "character"))
}

# ------------------------------------------------------------------ raw logs and placement scans

cep_scan_logs <- function(rq, raw_root) {
  rec_out <- vector("list", nrow(rq))
  files <- data.table::data.table(Batch = rq$Batch, CC = rq$CC, kind = "recording", file = rq$raw_log,
                                  path = file.path(raw_root, rq$Batch, "raw", rq$raw_log), events = NA_integer_,
                                  clock_fold_rows_dropped = NA_integer_, status = "")
  for (i in seq_len(nrow(rq))) {
    if (!file.exists(files$path[i])) stop("Missing raw log: ", files$path[i], call. = FALSE)
    lg <- rfid_read_raw_log(files$path[i])
    ev <- data.table::as.data.table(lg$events)[, .(t, tag = toupper(tag), label, board)]
    data.table::set(files, i, c("events", "clock_fold_rows_dropped"), list(nrow(ev), as.integer(lg$clock_fold_rows_dropped)))
    rm(lg)
    s0 <- rq$span_start[i]; s1 <- rq$span_end[i]
    ev[, where := data.table::fifelse(t < s0, "before_span", data.table::fifelse(t >= s1, "after_span", "in_span"))]
    agg <- ev[, .(events = .N, first_t = min(t), last_t = max(t)), by = .(tag, board, where)]
    agg <- merge(agg, ev[, .(labels = paste(sort(unique(label)), collapse = "|")), by = tag], by = "tag", all.x = TRUE)
    agg[, `:=`(Batch = rq$Batch[i], CC = rq$CC[i], raw_log = rq$raw_log[i])]
    rec_out[[i]] <- agg
    message(sprintf("%s %s: %d events, %d tags", rq$Batch[i], rq$CC[i], nrow(ev), data.table::uniqueN(ev$tag)))
    rm(ev); invisible(gc(verbose = FALSE))
  }
  isf <- sort(unlist(lapply(sort(unique(rq$Batch)), function(b) list.files(file.path(raw_root, b, "raw"), pattern = "initScan[.]csv$", full.names = TRUE))))
  is_out <- list()
  for (p in isf) {
    b <- basename(dirname(dirname(p)))
    cc <- paste0("CC", sub("^.*change_([0-9]).*$", "\\1", basename(p)))
    lg <- tryCatch(rfid_read_raw_log(p), error = function(e) e)
    st <- if (inherits(lg, "error")) paste("not read:", conditionMessage(lg)) else if (!nrow(lg$events)) "no detection events" else "read"
    n <- if (st == "read") nrow(lg$events) else 0L
    files <- rbind(files, data.table::data.table(Batch = b, CC = cc, kind = "initScan", file = basename(p), path = p, events = n,
                                                 clock_fold_rows_dropped = if (st == "read") as.integer(lg$clock_fold_rows_dropped) else NA_integer_,
                                                 status = st))
    if (st != "read") next
    ev <- data.table::as.data.table(lg$events)[, .(t, tag = toupper(tag), label, board)]
    agg <- ev[, .(events = .N, first_t = min(t), last_t = max(t), labels = paste(sort(unique(label)), collapse = "|")), by = .(tag, board)]
    agg[, `:=`(Batch = b, file_batch = regmatches(basename(p), regexpr("Batch[0-9]", basename(p))), CC = cc, file = basename(p))]
    is_out[[length(is_out) + 1L]] <- agg
    message(sprintf("initScan %s: %d events, %d tags", basename(p), nrow(ev), data.table::uniqueN(ev$tag)))
  }
  list(raw = data.table::rbindlist(rec_out, use.names = TRUE), initscan = data.table::rbindlist(is_out, use.names = TRUE), files = files)
}

# ------------------------------------------------------------------ design key, plan and cage-episode table

cep_design <- function(inp) {
  sh <- data.table::as.data.table(CEP_PLAN_SHEETS)
  lists <- data.table::rbindlist(lapply(seq_len(nrow(sh)), function(i) {
    x <- cep_system_lists(inp$grids[[i]], sh$file[i], sh$sheet[i])
    if (nrow(x)) x[, `:=`(Batch = sh$Batch[i], use = sh$use[i])]
    x
  }), fill = TRUE)
  lists <- rbind(lists, data.table::copy(inp$b1_config)[, use := "plan"], fill = TRUE)
  universe <- lists[use == "plan", .(AnimalNum = sort(unique(AnimalNum))), by = Batch]
  uni <- split(universe$AnimalNum, universe$Batch)

  cells <- data.table::rbindlist(lapply(which(sh$use == "plan"), function(i) {
    x <- cep_letter_cells(inp$grids[[i]])
    x[, `:=`(Batch = sh$Batch[i], file = sh$file[i], sheet = sh$sheet[i])]
  }))
  cells[, AnimalNum := cep_resolve(cep_animal(raw_id), uni[[.BY$Batch]]), by = Batch]
  if (anyNA(cells$AnimalNum)) stop("Letter-key IDs outside the planned lists: ", paste(unique(cells[is.na(AnimalNum), paste(Batch, raw_id)]), collapse = ", "), call. = FALSE)
  lkey <- unique(cells[, .(Batch, letter, AnimalNum)])
  if (anyDuplicated(lkey[, .(Batch, letter)]) || anyDuplicated(lkey[, .(Batch, AnimalNum)]))
    stop("The planning letter key is ambiguous (a letter with two animals or an animal with two letters).", call. = FALSE)
  blocks <- data.table::rbindlist(lapply(which(sh$use == "plan"), function(i) {
    b <- cep_letter_blocks(inp$grids[[i]])
    if (nrow(b)) b[, `:=`(Batch = sh$Batch[i], file = sh$file[i], sheet = sh$sheet[i], expected = CEP_ROUND_ORDER[[sh$Batch[i]]][block])]
    b
  }), fill = TRUE)
  if (!nrow(blocks) || anyNA(blocks$round) || any(blocks$round != blocks$expected))
    stop("Planning letter blocks disagree with CEP_ROUND_ORDER:\n", paste(utils::capture.output(print(blocks)), collapse = "\n"), call. = FALSE)
  fixed <- universe[!lkey, on = .(Batch, AnimalNum)]
  if (any(fixed[, .N, by = Batch]$N != 4L) || data.table::uniqueN(fixed$Batch) != data.table::uniqueN(universe$Batch))
    stop("Expected four planned animals without a letter (the fixed cage) per batch.", call. = FALSE)

  # tags: roster first, else the planning workbooks (B1: configuration)
  aq <- inp$animal_qc[!is.na(tag)]
  cand <- rbind(unique(aq[, .(Batch, AnimalNum, tag, source = "roster (Stage 00b animal_qc)")]),
                inp$b1_config[!is.na(rfid), .(Batch, AnimalNum, tag = rfid, source = CEP_B1_CONFIG_SOURCE)],
                unique(cells[!is.na(tag), .(Batch, AnimalNum, tag, source = paste0(file, "::", sheet))]))
  if (anyDuplicated(unique(aq[, .(Batch, AnimalNum, tag)])[, .(Batch, AnimalNum)])) stop("A roster animal has two tags.", call. = FALSE)
  roster_tag <- unique(aq[, .(Batch, AnimalNum, tag)])
  plan_tag <- unique(cand[source != "roster (Stage 00b animal_qc)", .(Batch, AnimalNum, tag)])
  plan_tag <- plan_tag[!roster_tag, on = .(Batch, AnimalNum)]
  if (anyDuplicated(plan_tag[, .(Batch, AnimalNum)])) stop("A non-roster animal has two planning tags.", call. = FALSE)
  tagkey <- rbind(roster_tag[, tag_source := "roster (Stage 00b animal_qc)"],
                  merge(plan_tag, cand[, .(tag_source = paste(sort(unique(source)), collapse = " | ")), by = .(Batch, AnimalNum, tag)],
                        by = c("Batch", "AnimalNum", "tag")))
  if (anyDuplicated(tagkey[, .(Batch, tag)])) stop("A tag maps to two animals of a batch.", call. = FALSE)
  disagree <- merge(cand, tagkey[, .(Batch, AnimalNum, chosen = tag)], by = c("Batch", "AnimalNum"))[tag != chosen]

  key <- rbind(lkey[, .(Batch, AnimalNum, letter, planned_fixed = FALSE)],
               fixed[, .(Batch, AnimalNum, letter = NA_character_, planned_fixed = TRUE)])
  key <- merge(key, tagkey, by = c("Batch", "AnimalNum"), all.x = TRUE)
  key <- merge(key, cells[, .(letter_sheets = paste(sort(unique(paste0(file, "::", sheet))), collapse = " | ")), by = .(Batch, AnimalNum)],
               by = c("Batch", "AnimalNum"), all.x = TRUE)
  key <- merge(key, lists[use == "plan", .(list_sources = paste(sort(unique(paste0(source, "::", sheet))), collapse = " | ")), by = .(Batch, AnimalNum)],
               by = c("Batch", "AnimalNum"), all.x = TRUE)
  empty <- data.table::rbindlist(lapply(names(uni), function(b) data.table::data.table(Batch = b, letter = setdiff(LETTERS[1:16], lkey[Batch == b]$letter))))
  list(lists = lists, universe = uni, letters = lkey, fixed = fixed, key = key[order(Batch, planned_fixed, letter, AnimalNum)],
       tagkey = tagkey, disagree = disagree, blocks = blocks, cells = cells, empty_letters = empty)
}

cep_match_lists <- function(L, plan) {
  L[, {
    ids <- AnimalNum; sysv <- sys; best <- character()
    for (cc in sort(unique(plan[Batch == .BY$Batch, CC]))) {
      p <- plan[Batch == .BY$Batch & CC == cc & slot_filled == TRUE, .(AnimalNum, pgroup)]
      m <- merge(data.table::data.table(AnimalNum = ids, sys = sysv), p, by = "AnimalNum")[!is.na(sys)]
      if (nrow(m) >= 4L && m[, data.table::uniqueN(pgroup), by = sys][, all(V1 == 1L)] && m[, data.table::uniqueN(sys), by = pgroup][, all(V1 == 1L)])
        best <- c(best, cc)
    }
    list(CC_matched = if (length(best) == 1L) best else NA_character_, candidates = paste(best, collapse = "|"), n = .N,
         n_sys_na = sum(is.na(sys)), sys_sizes = paste(tabulate(sys[!is.na(sys)], nbins = 5L), collapse = "/"))
  }, by = .(Batch, use, source, sheet, list_no)]
}

cep_mode_int <- function(v) if (all(is.na(v))) NA_integer_ else as.integer(names(which.max(table(v))))

cep_build <- function(inp, scan) {
  checks <- list()
  ck <- function(...) checks[[length(checks) + 1L]] <<- cep_check(...)
  aq <- inp$animal_qc; bq <- inp$board_qc; rq <- inp$recording_qc; excluded <- inp$excluded
  if (anyDuplicated(aq[, .(Batch, CC, AnimalNum)])) stop("animal_qc repeats an animal within a recording.", call. = FALSE)
  if (nrow(rq) != 24L) stop("Expected 24 recordings in recording_qc.", call. = FALSE)
  dz <- cep_design(inp)
  uni <- dz$universe; tagmap <- dz$tagkey[, .(Batch, tag, AnimalNum)]
  ck("planning letter blocks (sheet block k = CC k) agree with CEP_ROUND_ORDER",
     paste(dz$blocks[, sprintf("%s %s::%s block %d = %s", Batch, file, sheet, block, round)], collapse = "; "), TRUE)
  ck("planned animals per batch (lists) / lettered / fixed / empty letters",
     paste(vapply(names(uni), function(b) sprintf("%s %d/%d/%d/%s", b, length(uni[[b]]), nrow(dz$letters[Batch == b]), nrow(dz$fixed[Batch == b]),
                                                  cep_join(dz$empty_letters[Batch == b]$letter)), ""), collapse = "; "))
  ck("planning tags that disagree with the chosen tag (roster tag preferred)",
     if (nrow(dz$disagree)) paste(dz$disagree[, sprintf("%s %s: %s says %s, chosen %s", Batch, AnimalNum, source, tag, chosen)], collapse = "; ") else "none",
     NA)
  ck("planned animals without any tag (untagged)", cep_join(dz$key[is.na(tag), paste(Batch, AnimalNum)]))

  # animal records
  recs <- data.table::copy(inp$records)
  dup <- recs[duplicated(recs[, .(Batch, AnimalNum)]) | duplicated(recs[, .(Batch, AnimalNum)], fromLast = TRUE)]
  ck("duplicated Animals-sheet records (Batch x animal)", nrow(dup), nrow(dup) == 0L)
  recs <- unique(recs[order(Batch, AnimalNum, is.na(end_date))], by = c("Batch", "AnimalNum"))
  ck("Animals-sheet records per batch", paste(recs[, .N, by = Batch][order(Batch), paste(Batch, N)], collapse = "; "))

  # post-CC4 records, resolved to the batch's planned animals
  uni_of <- function(b) if (b %in% names(uni)) uni[[b]] else character()
  suc <- data.table::copy(inp$sucrose)
  suc[, AnimalNum := cep_resolve(id_canon, uni_of(.BY$Batch)), by = Batch]
  ck("sucrose-sheet animals not resolved to a planned animal", nrow(suc[is.na(AnimalNum)]), nrow(suc[is.na(AnimalNum)]) == 0L)
  ck("sucrose-sheet animals per batch (cage sizes 1-5)",
     paste(suc[, sprintf("%s %d (%s)", .BY$Batch, .N, paste(tabulate(sucpref_cage[!is.na(sucpref_cage)], 5L), collapse = "/")), by = Batch]$V1, collapse = "; "))
  suc <- suc[!is.na(AnimalNum)]
  kl <- data.table::copy(inp$kill)
  kl[, AnimalNum := cep_resolve(id_canon, uni_of(.BY$Batch)), by = Batch]
  ck("kill-list animals not resolved to a planned animal", nrow(kl[is.na(AnimalNum)]), nrow(kl[is.na(AnimalNum)]) == 0L)
  ck("kill-list animals per batch (cage sizes 1-5)", paste(kl[, sprintf("%s %d (%s)", .BY$Batch, .N, paste(tabulate(cage[!is.na(cage)], 5L), collapse = "/")), by = Batch]$V1, collapse = "; "))
  kl <- kl[!is.na(AnimalNum)]
  if (anyDuplicated(suc[, .(Batch, AnimalNum)]) || anyDuplicated(kl[, .(Batch, AnimalNum)]))
    stop("An animal appears twice in the sucrose sheet or in the kill list.", call. = FALSE)

  # planned composition per recording
  spans <- rq[, .(Batch, CC, span_start, span_end, start_date = as.Date(as.POSIXct(span_start, origin = "1970-01-01", tz = "UTC")))]
  plan <- data.table::rbindlist(lapply(seq_len(nrow(spans)), function(i) {
    b <- spans$Batch[i]; cc <- spans$CC[i]; r <- CEP_ROUND_ORDER[[b]][as.integer(sub("CC", "", cc))]
    let <- data.table::rbindlist(lapply(1:4, function(j) data.table::data.table(pgroup = paste0(r, "g", j), letter = CEP_ROUNDS[[r]][[j]])))
    let <- merge(let, dz$letters[Batch == b, .(letter, AnimalNum)], by = "letter", all.x = TRUE)
    fx <- data.table::data.table(letter = NA_character_, pgroup = "fixed", AnimalNum = dz$fixed[Batch == b]$AnimalNum)
    g <- rbind(let, fx, use.names = TRUE)
    g[, `:=`(Batch = b, CC = cc, round = r)]
  }))
  plan <- merge(plan, dz$tagkey[, .(Batch, AnimalNum, tag)], by = c("Batch", "AnimalNum"), all.x = TRUE)
  plan <- merge(plan, recs[, .(Batch, AnimalNum, end_date, method, notes)], by = c("Batch", "AnimalNum"), all.x = TRUE)
  norec <- plan[!is.na(AnimalNum) & !paste(Batch, AnimalNum) %in% paste(recs$Batch, recs$AnimalNum), unique(paste(Batch, AnimalNum))]
  ck("planned animals without an Animals-sheet record", cep_join(norec), !length(norec))
  plan <- merge(plan, spans[, .(Batch, CC, start_date)], by = c("Batch", "CC"))
  plan[, `:=`(slot_filled = !is.na(AnimalNum), fixed = pgroup == "fixed")]
  plan[, alive := slot_filled & (is.na(end_date) | end_date > start_date)]
  plan[, `:=`(untagged = slot_filled & is.na(tag), excluded = !is.na(AnimalNum) & AnimalNum %in% excluded)]

  # planned systems: the CC each list follows
  ml <- cep_match_lists(dz$lists, plan)
  matched <- ml[!is.na(CC_matched)]
  if (anyDuplicated(matched[, .(Batch, use, CC_matched)])) stop("Two planned system lists of one kind follow the same CC.", call. = FALSE)
  lists <- merge(dz$lists, ml[, .(Batch, use, source, sheet, list_no, CC_matched, candidates)], by = c("Batch", "use", "source", "sheet", "list_no"))
  sysmap <- function(u) lists[use == u & !is.na(CC_matched), .(Batch, CC = CC_matched, AnimalNum, sys, plan_list = paste0(source, "::", sheet, "#", list_no))]
  plan <- merge(plan, sysmap("plan")[, .(Batch, CC, AnimalNum, plan_sys = sys, plan_list)], by = c("Batch", "CC", "AnimalNum"), all.x = TRUE)
  plan <- merge(plan, sysmap("comparison")[, .(Batch, CC, AnimalNum, xls_sys = sys)], by = c("Batch", "CC", "AnimalNum"), all.x = TRUE)
  ck("planned system lists without exactly one fitting CC", paste(ml[is.na(CC_matched), sprintf("%s %s::%s #%d (n=%d, fits %s)", Batch, source, sheet, list_no, n,
                                                                                               data.table::fifelse(nzchar(candidates), candidates, "none"))], collapse = "; "))
  # post-CC4 cages (CC4 rows only)
  plan <- merge(plan, suc[, .(Batch, AnimalNum, sucpref_cage)], by = c("Batch", "AnimalNum"), all.x = TRUE)
  plan <- merge(plan, kl[, .(Batch, AnimalNum, kill_cage = cage)], by = c("Batch", "AnimalNum"), all.x = TRUE)
  plan[CC != "CC4", `:=`(sucpref_cage = NA_integer_, kill_cage = NA_integer_)]
  # tracked roster
  plan <- merge(plan, aq[, .(Batch, CC, AnimalNum, roster_board = board, in_roster = TRUE)], by = c("Batch", "CC", "AnimalNum"), all.x = TRUE)
  plan[is.na(in_roster), in_roster := FALSE]
  nr <- aq[!plan, on = .(Batch, CC, AnimalNum)]
  ck("roster animals not in the plan", nrow(nr), nrow(nr) == 0L)

  gp <- plan[slot_filled == TRUE, .(plan_sys_grp = cep_mode_int(plan_sys), xls_sys_grp = cep_mode_int(xls_sys), suc_cage_grp = cep_mode_int(sucpref_cage),
                                    det_board_grp = cep_mode_int(roster_board), n_slots = .N), by = .(Batch, CC, pgroup)]
  gp[, grp_board := data.table::fcoalesce(plan_sys_grp, suc_cage_grp, det_board_grp)]
  gp[, grp_board_source := data.table::fifelse(!is.na(plan_sys_grp), "planned system list",
                                               data.table::fifelse(!is.na(suc_cage_grp), "post-CC4 sucrose-sheet cage", "board of the group's tracked animals"))]
  if (anyNA(gp$grp_board) || gp[, anyDuplicated(grp_board) > 0L, by = .(Batch, CC)][, any(V1)])
    stop("A planned group has no board, or two groups share a board.", call. = FALSE)
  plan <- merge(plan, gp[, .(Batch, CC, pgroup, grp_board, grp_board_source, plan_sys_grp, xls_sys_grp, suc_cage_grp, det_board_grp)],
                by = c("Batch", "CC", "pgroup"))
  plan[, deviation := in_roster & roster_board != grp_board]
  plan[, deviation_confirmed := deviation & !is.na(sucpref_cage) & sucpref_cage == roster_board]
  ck("group placement by source", paste(gp[, .N, by = grp_board_source][, paste(grp_board_source, N)], collapse = "; "))
  ck("groups whose planned system (xlsx / configuration) differs from the board of their tracked animals",
     nrow(gp[!is.na(plan_sys_grp) & !is.na(det_board_grp) & plan_sys_grp != det_board_grp]))
  ck("groups whose GroupComposition.xls system differs from the board of their tracked animals",
     nrow(gp[!is.na(xls_sys_grp) & !is.na(det_board_grp) & xls_sys_grp != det_board_grp]))
  ck("CC4 groups whose sucrose-sheet cage differs from the board of their tracked animals",
     nrow(gp[!is.na(suc_cage_grp) & !is.na(det_board_grp) & suc_cage_grp != det_board_grp]))
  ck("roster animals off their planned group's board",
     paste(plan[deviation == TRUE, sprintf("%s %s %s (group %s planned board %d, tracked on %d, sucrose cage %s)", Batch, CC, AnimalNum, pgroup, grp_board,
                                          roster_board, sucpref_cage)], collapse = "; "))

  # detection: raw logs (in span) and the recording's own placement scan
  rawt <- merge(data.table::copy(scan$raw), tagmap, by = c("Batch", "tag"), all.x = TRUE)
  rs <- rawt[where == "in_span", .(events = sum(events)), by = .(Batch, CC, board, tag, AnimalNum)]
  rs <- merge(rs, aq[, .(Batch, CC, AnimalNum, roster_board = board)], by = c("Batch", "CC", "AnimalNum"), all.x = TRUE)
  rs[, roster := !is.na(roster_board)]
  rs[, main := events == max(events), by = .(Batch, CC, tag)]
  nmis <- rs[main == TRUE & roster == TRUE & board != roster_board, .N]
  ck("roster tags whose raw-log majority board differs from the Stage 00b board", nmis, nmis == 0L)
  lab <- function(id, tag) data.table::fifelse(is.na(id), tag, id)
  rd <- rs[main == TRUE & events >= CEP_MIN_EVENTS,
           .(n_tags_read_ge100 = .N, n_roster_read_ge100 = sum(roster), n_nonroster_tags_read = sum(!roster),
             nonroster_read = paste(sort(paste0(lab(AnimalNum, tag), "(", events, " ev)")[!roster]), collapse = ";")), by = .(Batch, CC, board)]
  small <- rs[roster == FALSE & events < CEP_MIN_EVENTS, .(nonroster_lt100 = paste(paste0(lab(AnimalNum, tag), "(", events, " ev)"), collapse = ";")),
              by = .(Batch, CC, board)]
  nre <- merge(rs[, .(n_tags_any_in_span = data.table::uniqueN(tag)), by = .(Batch, CC, board)],
               rs[roster == FALSE, .(nonroster_events_in_span = sum(events)), by = .(Batch, CC, board)], by = c("Batch", "CC", "board"), all = TRUE)
  rq2 <- data.table::copy(rq)[, day := sub("^MultiMouseMonitor-([0-9.]+)-.*$", "\\1", raw_log)]
  ini <- merge(data.table::copy(scan$initscan), tagmap, by = c("Batch", "tag"), all.x = TRUE)
  ini[, day := sub("^MultiMouseMonitor-([0-9.]+)-.*$", "\\1", file)]
  ini[, recording_scan := paste(Batch, CC, day) %in% rq2[, paste(Batch, CC, day)]]
  ini2 <- ini[recording_scan == TRUE, .(events = sum(events), first_t = min(first_t), last_t = max(last_t)), by = .(Batch, CC, board, tag, AnimalNum)]
  ini2[, main := events == max(events), by = .(Batch, CC, tag)]
  scan_span <- ini[recording_scan == TRUE, .(scan_events = sum(events), scan_start = min(first_t), scan_end = max(last_t),
                                             scan_files = paste(sort(unique(file)), collapse = "|")), by = .(Batch, CC)]
  im <- merge(ini2[main == TRUE], aq[, .(Batch, CC, AnimalNum, roster_board = board)], by = c("Batch", "CC", "AnimalNum"), all.x = TRUE)
  isc <- im[, .(n_initscan_tags = .N, n_initscan_roster = sum(!is.na(roster_board)),
                initscan_nonroster = paste(sort(lab(AnimalNum, tag)[is.na(roster_board)]), collapse = ";"),
                initscan_roster_off_board = paste(sort(AnimalNum[!is.na(roster_board) & roster_board != board]), collapse = ";")), by = .(Batch, CC, board)]
  plan <- merge(plan, unique(im[!is.na(AnimalNum), .(Batch, CC, AnimalNum, initscan_board = board)], by = c("Batch", "CC", "AnimalNum")),
                by = c("Batch", "CC", "AnimalNum"), all.x = TRUE)

  # the cage-episode table
  ce <- data.table::CJ(Batch = sort(unique(rq$Batch)), CC = sort(unique(rq$CC)), board = CEP_BOARDS)
  ce <- merge(ce, aq[, .(n_roster = .N, roster = paste(sort(AnimalNum), collapse = ";")), by = .(Batch, CC, board)], by = c("Batch", "CC", "board"), all.x = TRUE)
  ce[is.na(n_roster), `:=`(n_roster = 0L, roster = "")]
  ce <- merge(ce, rd, by = c("Batch", "CC", "board"), all.x = TRUE)
  ce <- merge(ce, small, by = c("Batch", "CC", "board"), all.x = TRUE)
  ce <- merge(ce, nre, by = c("Batch", "CC", "board"), all.x = TRUE)
  for (v in c("n_tags_read_ge100", "n_roster_read_ge100", "n_nonroster_tags_read", "n_tags_any_in_span", "nonroster_events_in_span")) ce[is.na(get(v)), (v) := 0L]
  ce[is.na(nonroster_read), nonroster_read := ""]
  ce <- merge(ce, isc, by = c("Batch", "CC", "board"), all.x = TRUE)
  ce[paste(Batch, CC) %in% scan_span[, paste(Batch, CC)] & is.na(n_initscan_tags), `:=`(n_initscan_tags = 0L, n_initscan_roster = 0L)]
  pb <- plan[slot_filled == TRUE, .(n_plan_alive = sum(alive), n_excluded_known = sum(alive & !in_roster & excluded),
                                    n_untagged = sum(alive & untagged), n_present_untracked = sum(alive & !in_roster),
                                    planned_untracked = paste(sort(AnimalNum[alive & !in_roster]), collapse = ";"),
                                    removed_before_cc = paste(sort(AnimalNum[!alive]), collapse = ";"),
                                    placement = paste(sort(unique(grp_board_source)), collapse = "|"),
                                    planned_fixed = any(fixed)), by = .(Batch, CC, board = grp_board)]
  pb <- merge(pb, plan[slot_filled == FALSE, .(n_empty_slots = .N, empty_letters = paste(sort(letter), collapse = ";")), by = .(Batch, CC, board = grp_board)],
              by = c("Batch", "CC", "board"), all.x = TRUE)
  pb[is.na(n_empty_slots), `:=`(n_empty_slots = 0L, empty_letters = "")]
  ce <- merge(ce, pb, by = c("Batch", "CC", "board"), all.x = TRUE)
  # executed deviations that an independent record confirms (post-CC4 sucrose-sheet cage = detection board)
  dev <- plan[deviation_confirmed == TRUE]
  ce[, `:=`(dev_out = 0L, dev_in = 0L)]
  for (i in seq_len(nrow(dev))) {
    ce[Batch == dev$Batch[i] & CC == dev$CC[i] & board == dev$grp_board[i], dev_out := dev_out + 1L]
    ce[Batch == dev$Batch[i] & CC == dev$CC[i] & board == dev$roster_board[i], dev_in := dev_in + 1L]
  }
  ce[, n_design_present := n_plan_alive - dev_out + dev_in]
  ce <- merge(ce, suc[!is.na(sucpref_cage), .(n_sucpref_cage = .N), by = .(Batch, board = sucpref_cage)][, CC := "CC4"], by = c("Batch", "CC", "board"), all.x = TRUE)
  ce <- merge(ce, kl[!is.na(cage), .(n_kill_cage = .N), by = .(Batch, board = cage)][, CC := "CC4"], by = c("Batch", "CC", "board"), all.x = TRUE)
  ce[, n_post_cc4_record := data.table::fifelse(!is.na(n_kill_cage), n_kill_cage, n_sucpref_cage)]
  kc <- plan[CC == "CC4" & in_roster == TRUE & !is.na(kill_cage)]
  ck("CC4 roster animals whose kill-list cage differs from the detection board", sprintf("%d of %d", kc[kill_cage != roster_board, .N], nrow(kc)),
     kc[kill_cage != roster_board, .N] == 0L)
  sc <- plan[CC == "CC4" & in_roster == TRUE & !is.na(sucpref_cage)]
  ck("CC4 roster animals whose sucrose-sheet cage differs from the detection board", sprintf("%d of %d", sc[sucpref_cage != roster_board, .N], nrow(sc)),
     sc[sucpref_cage != roster_board, .N] == 0L)
  ce <- merge(ce, bq, by = c("Batch", "CC", "board"), all.x = TRUE)
  ce[, `:=`(System = paste0("sys.", board), fixed_cage = planned_fixed %in% TRUE)]
  ce[, CageEpisodeID := paste(Batch, System, CC, sep = "|")]

  # sources and classification: the batch's plan, the planned system list used for the CC (configuration or a
  # GroupCompBatch list; GroupComposition.xlsx lists are part of RGSYS), Animals end dates that change the batch's
  # composition during the recordings, the post-CC4 records of the batch
  sh <- data.table::as.data.table(CEP_PLAN_SHEETS)
  plan_code <- sh[use == "plan", .(Batch, plan_code)]
  list_code <- unique(rbind(sh[!is.na(list_code), .(source = file, sheet, list_code)],
                            data.table::data.table(source = CEP_B1_CONFIG_SOURCE, sheet = "Parameter", list_code = CEP_B1_CONFIG_CODE)))
  cc_code <- merge(unique(lists[use == "plan" & !is.na(CC_matched), .(Batch, CC = CC_matched, source, sheet)]), list_code, by = c("source", "sheet"))
  anim_b <- unique(plan[slot_filled == TRUE & !alive, Batch])
  suc_b <- unique(suc[!is.na(sucpref_cage), Batch]); kill_b <- unique(kl[!is.na(cage), Batch])
  ce[, presence_source := mapply(function(b, cc) {
    s <- c(plan_code[Batch == b]$plan_code, cc_code[Batch == b & CC == cc]$list_code)
    if (b %in% anim_b) s <- c(s, "ANIM")
    if (cc == "CC4" && b %in% suc_b) s <- c(s, "SUC")
    if (cc == "CC4" && b %in% kill_b) s <- c(s, "KILL")
    paste(s, collapse = "+")
  }, Batch, CC)]
  ce[, presence_source_detail := vapply(strsplit(presence_source, "+", fixed = TRUE), function(s) paste(CEP_SOURCE_CODES[s], collapse = " + "), "")]
  ce[, classification := data.table::fifelse(is.na(n_design_present), "detection-derived",
                                             data.table::fifelse(dev_out + dev_in > 0L | (!is.na(n_sucpref_cage) & n_sucpref_cage != n_design_present) |
                                                                   (!is.na(n_kill_cage) & n_kill_cage != n_design_present), "conflict", "design-known"))]
  ce[, accounted := n_roster + n_present_untracked == n_design_present]
  notes <- cep_notes(ce, plan, rawt, ini2, scan_span, rq, recs, inp$manifest)
  ce[, note := notes]
  data.table::setorderv(ce, c("Batch", "CC", "board"))

  # detection checks against Stage 00b
  ck("cage episodes / recordings / fixed cages (one per recording)",
     sprintf("%d / %d / %d (recordings with exactly one: %d)", nrow(ce), data.table::uniqueN(ce[, paste(Batch, CC)]), sum(ce$fixed_cage),
             ce[, sum(fixed_cage) == 1L, by = .(Batch, CC)][, sum(V1)]), nrow(ce) == 120L && ce[, sum(fixed_cage) == 1L, by = .(Batch, CC)][, all(V1)])
  ck("roster + present-untracked != design count", nrow(ce[accounted == FALSE]), !any(ce$accounted == FALSE, na.rm = TRUE))
  ck("Stage 00b nonroster_tags_ge100 differs from this scan's non-roster tags >= 100 events",
     nrow(ce[nonroster_tags_ge100 != n_nonroster_tags_read]), nrow(ce[nonroster_tags_ge100 != n_nonroster_tags_read]) == 0L)
  ck("Stage 00b nonroster_events differs from this scan's non-roster in-span events",
     nrow(ce[nonroster_events != nonroster_events_in_span]), nrow(ce[nonroster_events != nonroster_events_in_span]) == 0L)
  ck("Stage 00b tags_present differs from this scan's distinct tags read in span on the board (any event count)",
     nrow(ce[tags_present != n_tags_any_in_span]), nrow(ce[tags_present != n_tags_any_in_span]) == 0L)
  fz <- merge(scan$files[kind == "recording"], rq[, .(Batch, CC, qc_events = events, qc_fold = clock_fold_rows_dropped)], by = c("Batch", "CC"))
  ck("recordings whose event count or dropped clock-fold rows differ from Stage 00b recording_qc",
     nrow(fz[events != qc_events | clock_fold_rows_dropped != qc_fold]), nrow(fz[events != qc_events | clock_fold_rows_dropped != qc_fold]) == 0L)
  ck("non-roster tags with < 100 in-span events", paste(ce[!is.na(nonroster_lt100), sprintf("%s %s board %d: %s", Batch, CC, board, nonroster_lt100)], collapse = "; "))
  unk <- rawt[is.na(AnimalNum), .(events = sum(events)), by = .(Batch, CC, tag, board, where)]
  ck("tags in no tag map (raw logs)", paste(unk[, sprintf("%s %s %s board %d %s: %d", Batch, CC, tag, board, where, events)], collapse = "; "))
  xt <- rawt[AnimalNum %in% excluded, .(events = sum(events), first = cep_utc(min(first_t)), last = cep_utc(max(last_t))), by = .(Batch, CC, AnimalNum, board, where)]
  ck("excluded animals' tags in the raw logs", paste(xt[order(Batch, AnimalNum, CC, board, where), sprintf("%s %s %s board %d %s: %d (%s to %s)", Batch, CC, AnimalNum, board,
                                                                                                          where, events, first, last)], collapse = "; "))
  ck("initScan files not read or without detection events", paste(scan$files[kind == "initScan" & status != "read", sprintf("%s (%s)", file, status)], collapse = "; "))
  ck("recordings without a placement scan of their start day", paste(rq[!scan_span, on = .(Batch, CC), paste(Batch, CC)], collapse = "; "))

  raw_out <- rawt[, .(Batch, CC, raw_log, tag, AnimalNum, board, where, events, first_utc = cep_utc(first_t, "%Y-%m-%d %H:%M:%OS3"),
                      last_utc = cep_utc(last_t, "%Y-%m-%d %H:%M:%OS3"), labels)][order(Batch, CC, tag, board, where)]
  ini_out <- ini[, .(Batch, CC, file, recording_scan, tag, AnimalNum, board, events, first_utc = cep_utc(first_t, "%Y-%m-%d %H:%M:%OS3"),
                     last_utc = cep_utc(last_t, "%Y-%m-%d %H:%M:%OS3"), labels)][order(Batch, CC, file, tag, board)]
  key_cols <- c("Batch", "CC", "board", "System", "CageEpisodeID", "fixed_cage", "n_animals_present", "n_tags_present", "n_tracked", "n_untagged",
                "presence_source", "classification", "note")
  data.table::setnames(ce, c("n_design_present", "n_tags_read_ge100", "n_roster"), c("n_animals_present", "n_tags_present", "n_tracked"))
  full_cols <- c(key_cols, "roster", "n_roster_read_ge100", "n_nonroster_tags_read", "nonroster_read", "nonroster_lt100", "n_tags_any_in_span", "nonroster_events_in_span",
                 "n_initscan_tags", "n_initscan_roster", "initscan_nonroster", "initscan_roster_off_board", "n_plan_alive", "n_excluded_known",
                 "n_present_untracked", "planned_untracked", "removed_before_cc", "n_empty_slots", "empty_letters", "placement", "dev_out", "dev_in",
                 "n_sucpref_cage", "n_kill_cage", "n_post_cc4_record", "accounted", "presence_source_detail", "tags_present", "nonroster_events",
                 "nonroster_tags_ge100", "last_read", "tail_silence_s", "removal_signature")
  plan_out <- plan[order(Batch, CC, fixed, pgroup, letter, AnimalNum),
                   .(Batch, CC, round, pgroup, fixed, letter, AnimalNum, tag, slot_filled, start_date, end_date, method, notes, alive, untagged, excluded,
                     plan_sys, plan_list, xls_sys, sucpref_cage, kill_cage, initscan_board, in_roster, roster_board, grp_board, grp_board_source,
                     plan_sys_grp, xls_sys_grp, suc_cage_grp, det_board_grp, deviation, deviation_confirmed)]
  list(cage_episode_presence = ce[, ..key_cols], cage_episode_presence_full = ce[, ..full_cols], planned_animal_episodes = plan_out,
       design_key = dz$key, planned_system_lists = lists[order(Batch, use, source, sheet, list_no, sys, AnimalNum)],
       rawlog_tag_board_events = raw_out, initscan_tag_board_events = ini_out, checks = data.table::rbindlist(checks))
}

# Notes per cage episode, assembled from the records (no hand-written facts).
cep_notes <- function(ce, plan, rawt, ini2, scan_span, rq, recs, manifest) {
  hms <- function(t) cep_utc(t, "%H:%M:%S")
  vapply(seq_len(nrow(ce)), function(i) {
    x <- ce[i]; b <- x$Batch; cc <- x$CC; bd <- x$board
    out <- character()
    if (isTRUE(x$removal_signature) || isTRUE(x$tail_silence_s > 3600)) {
      sx <- manifest[CageEpisodeID == x$CageEpisodeID & action == "censor", record_id]
      out <- c(out, sprintf("all tags stop at %s (logger wall clock), %.1f h before the span end (probable removal, not recorded%s)",
                            cep_utc(x$last_read, "%Y-%m-%d %H:%M:%OS3"), x$tail_silence_s / 3600,
                            if (length(sx)) paste0("; censored: spatial exclusion manifest ", paste(sx, collapse = ", ")) else ""))
    }
    p <- plan[Batch == b & CC == cc]
    for (k in which(p$deviation & p$grp_board == bd)) {
      a <- p[k]
      ev <- c("detection", if (isTRUE(a$sucpref_cage == a$roster_board)) "post-CC4 sucrose-sheet cage", if (isTRUE(a$kill_cage == a$roster_board)) "kill list",
              if (isTRUE(a$initscan_board == a$roster_board)) "initScan")
      out <- c(out, sprintf("%s, planned here, was housed on board %d (protocol deviation; %s agree)%s", a$AnimalNum, a$roster_board, paste(ev, collapse = ", "),
                            if (a$deviation_confirmed) "" else "; not confirmed by an independent record, not counted"))
    }
    for (k in which(p$deviation & p$roster_board == bd)) {
      a <- p[k]
      out <- c(out, sprintf("%s housed here against the plan (planned board %d; protocol deviation)%s", a$AnimalNum, a$grp_board,
                            if (a$deviation_confirmed) "" else "; not confirmed by an independent record, not counted"))
    }
    if (nzchar(x$planned_untracked)) out <- c(out, paste0("present not in roster: ", x$planned_untracked,
                                                          if (nzchar(x$nonroster_read)) paste0("; tag read: ", x$nonroster_read) else " (no tag read)"))
    if (nzchar(x$removed_before_cc)) {
      r <- p[grp_board == bd & slot_filled & !alive]
      out <- c(out, paste0("planned mate removed before this CC: ", paste(sprintf("%s (Animals end date %s, %s)", r$AnimalNum, r$end_date, r$method), collapse = ", ")))
    }
    if (x$n_empty_slots > 0L) out <- c(out, sprintf("planned group has an empty slot (letter %s has no animal in the plan)", x$empty_letters))
    present <- c(p[grp_board == bd & alive == TRUE & !(deviation_confirmed & roster_board != bd), AnimalNum], p[deviation_confirmed & roster_board == bd, AnimalNum])
    s <- ini2[Batch == b & CC == cc & board == bd & main == TRUE]
    for (k in which(is.na(s$AnimalNum) | !s$AnimalNum %in% c(present, p[in_roster == TRUE, AnimalNum]))) {
      a <- s[k]; who <- if (is.na(a$AnimalNum)) a$tag else a$AnimalNum
      n_rec <- rawt[Batch == b & CC == cc & tag == a$tag, sum(events)]
      r <- recs[Batch == b & AnimalNum %in% a$AnimalNum]
      out <- c(out, sprintf("initScan %s %s-%s read %s here (%d events, last read %s); the recording %s%s",
                            cep_utc(a$first_t, "%Y-%m-%d"), cep_utc(a$first_t, "%H:%M"), cep_utc(a$last_t, "%H:%M"), who, a$events, hms(a$last_t),
                            if (n_rec > 0) sprintf("reads the tag %d times", n_rec) else sprintf("never reads the tag (first event %s)", hms(rq[Batch == b & CC == cc, first_event])),
                            if (nrow(r)) sprintf("; Animals record: end date %s, %s%s", r$end_date, r$method, if (!is.na(r$notes) && nzchar(r$notes)) paste0(", ", r$notes) else "") else ""))
    }
    off <- s[!is.na(AnimalNum) & AnimalNum %in% p[in_roster == TRUE & roster_board != bd, AnimalNum]]
    for (k in seq_len(nrow(off))) {
      a <- off[k]; rb <- p[AnimalNum == a$AnimalNum, roster_board][1]
      there <- ini2[Batch == b & CC == cc & tag == a$tag & board == rb]
      out <- c(out, sprintf("initScan majority board differs for %s: read here %s-%s (%d events), on its tracked board %d %s; before the span",
                            a$AnimalNum, hms(a$first_t), hms(a$last_t), a$events, rb,
                            if (nrow(there)) sprintf("%s-%s (%d events)", hms(there$first_t), hms(there$last_t), there$events) else "not at all"))
    }
    if (!is.na(x$n_initscan_tags) && x$n_initscan_tags < x$n_roster) {
      sp <- scan_span[Batch == b & CC == cc]
      out <- c(out, sprintf("initScan incomplete (%d events within %.0f s, %s-%s; the recording's first event follows at %s)", sp$scan_events,
                            sp$scan_end - sp$scan_start, hms(sp$scan_start), hms(sp$scan_end), hms(rq[Batch == b & CC == cc, first_event])))
    }
    paste(out, collapse = "; ")
  }, character(1))
}

# ------------------------------------------------------------------ main

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (helper in c("project_paths.R", "animalpos_preprocessing_helpers.R", "antenna_log_helpers.R")) source_mmm_helper(helper)
  for (pkg in c("readxl", "digest")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Package ", pkg, " is needed.", call. = FALSE)
  a <- cep_args(commandArgs(trailingOnly = TRUE))
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  project_root <- mmm_project_root()
  behavior <- dirname(project_root)
  experiment <- dirname(dirname(dirname(project_root)))
  raw_root <- mmm_raw_antenna_root(project_root)
  head_commit <- cep_git(c("rev-parse", "HEAD"))[1]
  if (!grepl("^[0-9a-f]{40}$", head_commit)) stop("Could not read the git commit: ", head_commit, call. = FALSE)
  run_id <- if (is.na(a$run_id)) paste0(CEP_VERSION, "_", substr(head_commit, 1, 7)) else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  qc_run <- if (is.na(a$qc_run)) CEP_QC_RUN else a$qc_run
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(behavior, "RANM", "cage_episode_presence") else a$output_root, project_root)
  out_dir <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out_dir)) stop("Run folder exists; choose a new --run-id: ", out_dir, call. = FALSE)
  if (file.exists(tmp)) stop("A partial run folder exists; inspect and remove it first: ", tmp, call. = FALSE)
  mmm_assert_output_path_budget(c(file.path(tmp, names(CEP_FILE_DOCS)), file.path(out_dir, names(CEP_FILE_DOCS))), source_label = "cep audit")
  script_status <- cep_git(c("status", "--porcelain", "--untracked-files=all", "--", CEP_SCRIPT))
  script_status <- if (!length(script_status)) "committed, clean" else if (startsWith(script_status[1], "??")) "untracked" else "modified"

  paths <- cep_input_paths(experiment, project_root, qc_run)
  miss <- paths[!file.exists(paths)]
  if (length(miss)) stop("Missing input(s):\n  ", paste(miss, collapse = "\n  "), call. = FALSE)
  inp <- cep_read_inputs(paths)
  scan <- cep_scan_logs(inp$recording_qc, raw_root)
  tabs <- cep_build(inp, scan)

  inputs <- rbind(data.table(role = names(paths), path = unname(paths)),
                  scan$files[, .(role = paste0(kind, ":", Batch, "_", CC), path)])
  inputs[, `:=`(sha256 = mmm_file_sha256(path), bytes = file.size(path), mtime = format(file.mtime(path), "%Y-%m-%d %H:%M:%S"))]
  qi <- inp$qc_inputs[, .(path_base = basename(input), qc_sha256 = sha256)]
  rawin <- merge(inputs[startsWith(role, "recording:") | startsWith(role, "initScan:"), .(path_base = basename(path), sha256)], unique(qi), by = "path_base")
  tabs$checks <- rbind(tabs$checks, cep_check("raw logs and placement scans whose SHA-256 differs from the Stage 00b run's inputs.csv",
                                               sprintf("%d of %d compared", rawin[sha256 != qc_sha256, .N], nrow(rawin)), rawin[sha256 != qc_sha256, .N] == 0L))
  documented <- sub("^tables/(.*)[.]csv$", "\\1", grep("^tables/", names(CEP_FILE_DOCS), value = TRUE))
  if (!setequal(names(tabs), documented))
    stop("Tables and README entries differ: ", paste(c(setdiff(names(tabs), documented), setdiff(documented, names(tabs))), collapse = " "), call. = FALSE)

  dir.create(file.path(tmp, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "manifest"), showWarnings = FALSE)
  for (nm in documented) fwrite(tabs[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  fwrite(inputs, file.path(tmp, "manifest", "inputs.csv"))
  run <- data.table(
    item = c("run_id", "version", "status", "git_commit", "script", "script_sha256", "script_git_status", "qc_run", "min_events", "project_root",
             "experiment_root", "raw_root", "output_root", "inputs", "columns_read_from_workbooks", "started", "finished", "R", "data.table", "readxl"),
    value = c(run_id, CEP_VERSION, "UNREGISTERED design audit; no RES/SUS label, CombZ, outcome or body-weight column read; fixed_cage = the planned CON cage", head_commit,
              CEP_SCRIPT, mmm_file_sha256(file.path(MMM_REPO_ROOT, CEP_SCRIPT)), script_status, qc_run, CEP_MIN_EVENTS, project_root, experiment,
              raw_root, base, sprintf("manifest/inputs.csv (%d files with SHA-256)", nrow(inputs)),
              "Animals: ID, Cage, Batch, Mark date, End date, Method, Notes; sucrosePreference: A (ID), C:D (Cage, Batch); Kill_List: A:C (Run, ID, Cage); planning sheets and B1 configuration: whole sheets (no outcome column; their CON/SIS condition cells are not used)",
              started, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string, as.character(utils::packageVersion("data.table")),
              as.character(utils::packageVersion("readxl"))))
  fwrite(run, file.path(tmp, "manifest", "run.csv"))
  w <- max(nchar(names(CEP_FILE_DOCS)))
  ce <- tabs$cage_episode_presence
  writeLines(c("ANIMALS PRESENT IN EVERY EXP9 RFID CAGE EPISODE (UNREGISTERED design audit)", "",
               paste("run:", run_id, "| commit:", head_commit, "| script:", CEP_SCRIPT, paste0("(", script_status, ")")),
               paste("roster and spans: Stage 00b QC run", qc_run, "| raw logs:", raw_root), "",
               "No RES/SUS label, CombZ, outcome or body-weight column was read; the planning sheets' CON/SIS cells are not used.",
               "The planned fixed cage (= the CON cage) is the batch's planned animals without a regrouping letter.",
               "Unit: the cage episode (Batch x CC x board, 120). Definitions: the",
               "header of the script. Inputs with SHA-256: manifest/inputs.csv. Checks: tables/checks.csv.", "",
               sprintf("n_animals_present: %s", paste(ce[, .N, by = n_animals_present][order(n_animals_present), sprintf("%d in %d", n_animals_present, N)], collapse = ", ")),
               sprintf("n_tracked < n_animals_present: %d; n_tags_present != n_tracked: %d; classification: %s", ce[n_tracked < n_animals_present, .N],
                       ce[n_tags_present != n_tracked, .N], paste(ce[, .N, by = classification][, sprintf("%s %d", classification, N)], collapse = ", ")), "",
               "presence_source codes:", paste0("  ", names(CEP_SOURCE_CODES), "  ", CEP_SOURCE_CODES), "",
               "Files:", paste(formatC(names(CEP_FILE_DOCS), width = -w), CEP_FILE_DOCS, sep = "  ")),
             file.path(tmp, "README.txt"))
  outs <- setdiff(list.files(tmp, recursive = TRUE), "manifest/output_files_sha256.csv")
  fwrite(data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
         file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (!setequal(list.files(tmp, recursive = TRUE), names(CEP_FILE_DOCS))) stop("Unexpected file set in ", tmp, call. = FALSE)
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  print(tabs$checks)
  message("cage-episode presence audit done: ", out_dir)
}
