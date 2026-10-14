# ================================================================
# CC4 outcome x cage-episode eligibility for Stages 31-31d (rule cc4e_v1.0)
# MMMSociability -- Functions/cc4_outcome_eligibility.R
# ================================================================
# Applies the released spatial exclusion manifest (v1.1) to the Stage 31-31d outcomes at the level of one animal x cage
# episode x phase family x phase, instead of removing a cage episode from every roster. Every Stage 31-31d outcome is a
# vendor displacement-event (crossing) count; none is an occupancy, co-location or cage-use measure, so the
# exclude_spatial records are not applied as such: a whole-session dead reader enters through the active family (always
# compromised: out of the primary analyses) and the inactive family, which manifest v1.1 keeps (SX06/SX07: the
# light-phase crossing rate is kept and evaluated separately). The CC4 reader knockout only grades the kept inactive
# cells (retained_unverified, retained_flagged, retained_material); under cc4e_v1.0 it never excludes them, and an
# exclusion would need Tobias's approval of a new rule version. The rule table CC4E_RULES was fixed before any
# rule-applied result was seen and reads no group label (Group is carried for counting only).
#   cc4e_manifest()          the released manifest through sxm_read() (content-hash pinned)
#   cc4e_status()            the status of every animal-phase cell of a Stage 31b animal_phase_activity table
#   cc4e_read_knockout()     a CC4 reader-knockout run's materiality table and run record (release or dev)
#   cc4e_read_eligibility()  an audit run's outcome_episode_eligibility.csv, verified against its run manifest; a dev
#                            audit run is refused unless MMM_CC4_ALLOW_DEV_ELIGIBILITY=TRUE
#   cc4e_inactive_sensitivity()  the inactive sensitivity (the retained inactive cells leave the family); the audit
#                            writes it as its own run (--variant=inactive_sensitivity) for a sensitivity rerun
#   cc4e_join()              appends FamilyExcluded, EligibilityStatus, EligibilityRecord, EligibilityVersion (last)
#   cc4e_population()        animals, cages, CON cages, batches and mixed RES/SUS cages of a roster
# Stages 31b-31d use the rule only when MMM_CC4_OUTCOME_ELIGIBILITY names the eligibility file of an audit run
# (Testing/audits/audit_cc4_outcome_eligibility.R), and then only into an isolated output root; without it their code
# paths and outputs are unchanged. The frozen runs are the all-episode sensitivity. Documentation:
# docs/CC4_OUTCOME_ELIGIBILITY.md.
# Pure definitions: sourcing this file reads no data. Needs data.table and digest.
# ================================================================

CC4E_VERSION <- "cc4e_v1.0"
CC4E_MANIFEST_VERSION <- "v1.1"
CC4E_FAMILIES <- list(Active = paste0("A", 1:5), Inactive = paste0("I", 2:5))   # the Stage 31b families (31c/31d: A2-A5)
CC4E_STATUSES <- c("included", "retained_flagged", "retained_unverified", "retained_material", "excluded_hardware", "censored",
                   "incomplete_span", "cage_roster_incomplete")
CC4E_PRIMARY_USE <- c(included = "primary", retained_flagged = "primary_flagged", retained_unverified = "primary_flagged",
                      retained_material = "primary_flagged", excluded_hardware = "sensitivity_only", censored = "missing",
                      incomplete_span = "missing", cage_roster_incomplete = "outside_roster")
# A cell with one of these statuses removes its cage from the family's complete-cage roster (FamilyExcluded); an
# incomplete span already does so through EligiblePhase.
CC4E_FAMILY_EXCLUDING <- c("excluded_hardware", "censored")
CC4E_RETAINED <- c("retained_flagged", "retained_unverified", "retained_material")
CC4E_VARIANTS <- c("primary", "inactive_sensitivity")
CC4E_KNOCKOUT_VERSION <- "ckr_v1.0"
CC4E_KNOCKOUT_RECORDINGS <- paste0("B", 1:6, "_CC4", collapse = " ")
CC4E_COLUMNS <- c("FamilyExcluded", "EligibilityStatus", "EligibilityRecord", "EligibilityVersion")
CC4E_ELIGIBILITY_COLUMNS <- c("AnimalNum", "AnimalKey", "CageID", "Batch", "Sex", "Group", "family", "PhaseLabel", "EligiblePhase",
                              "status", "records", "primary_use", "dead_reader", "knockout_check", "note", "rule_version")
CC4E_KNOCKOUT_COLUMNS <- c("reader", "family", "outcome", "n_animals", "n_board_recordings", "scale_sd", "median_shift",
                           "median_abs_shift_sd", "p90_abs_shift_sd", "material", "rule")
CC4E_KNOCKOUT_OUTCOMES <- c("reference_rate", "post_rate", "log_rate_ratio")
CC4E_ELIGIBILITY_FILE <- "tables/outcome_episode_eligibility.csv"
CC4E_RUN_MANIFEST <- "manifest/output_files_sha256.csv"

# The rule, one row per clause. Precedence of the cell statuses: E04 > E05 > E09 > E06/E07/E08 > E11; E10 then
# replaces E06/E07/E08/E11 where the cage's family is incomplete.
CC4E_RULES <- data.frame(
  rule_id = sprintf("E%02d", 1:12),
  scope = c("all Stage 31-31d outcomes", "exclude_spatial records (SX01-SX03)", "records of CC1-CC3 (SX01, SX05)",
            "censor record (SX04)", "whole-session dead reader with its exclude_movement record (SX02+SX06, SX03+SX07)",
            "same episode", "same episode", "same episode", "any cell", "any observed cell", "any cell", "rosters"),
  condition = c("classification of the outcomes",
                "occupancy, co-location or cage-use outcomes that need complete antenna coverage",
                "cage episodes outside CC4",
                "the phase block ends after the censoring time",
                "the episode's reader was silent from span_start to span_end",
                "the CC4 knockout of the dead reader is material for any inactive outcome (reference_rate, post_rate, log_rate_ratio)",
                "a CC4 knockout is supplied and is not material for all three inactive outcomes",
                "no CC4 knockout is supplied",
                "no complete clock and animal span and no record (frozen complete-span rule)",
                "another cell of the same cage x family is censored, excluded_hardware or incomplete_span",
                "none of the above",
                "statuses never remove an animal"),
  family = c("both", "both", "both", "both", "Active", "Inactive", "Inactive", "Inactive", "both", "both", "both", "both"),
  phases = c("all", "all", "all", "blocks ending after the censoring time", "A1-A5", "I2-I5", "I2-I5", "I2-I5", "the cell",
             "the family's cells (31b: I2-I5, A1-A5)", "the cell", "all"),
  status = c("none", "not applied", "unaffected", "censored", "excluded_hardware", "retained_material", "retained_flagged",
             "retained_unverified", "incomplete_span", "cage_roster_incomplete", "included", "none"),
  primary_use = c("", "", "", "missing", "sensitivity_only", "primary_flagged", "primary_flagged", "primary_flagged", "missing",
                  "outside_roster", "primary", ""),
  rationale = c(
    "every Stage 31-31d outcome is a vendor displacement-event (crossing) count of the AnimalPos stream; none is an occupancy, co-location or cage-use measure",
    "exclude_spatial covers measures that need complete antenna coverage; Stage 31-31d have none, so the records act only through E05-E08",
    "Stage 31-31d read CC4 only; CC1-CC3 measurements of the same animals stay, and a CC3 record does not enter Stage 31-31d",
    "unobserved after the board's last read: missing, not excluded; takes precedence over every other clause",
    "SX06/SX07 were written for the Stage 29 window (CC4 A1); the reader was silent the whole session (SX02/SX03), so every active phase carries the same fault (knockout: R3 +1.7/h, 0.33/0.90 SD; R5 -3.0/h, 0.58/1.21 SD); out of the primary analyses, kept in the all-episode sensitivity",
    "manifest v1.1 keeps the light-phase crossing rate (SX06/SX07); the light-window knockout (0.12/0.35 and 0.12/0.41 SD) does not cover the grid days I3-I5. A material CC4 knockout keeps the cells in the primary analyses, flagged, makes the inactive sensitivity mandatory and is reported as a decision for Tobias: an exclusion would need an approved new rule version",
    "manifest v1.1 keeps the light-phase crossing rate; kept in the primary analyses with a flag; the inactive sensitivity leaves the episode out",
    "manifest v1.1 keeps the light-phase crossing rate; kept in the primary analyses with a flag (materiality not verified on CC4); the inactive sensitivity leaves the episode out",
    "the frozen rule: an incomplete phase is missing (NA), never a low rate",
    "the complete-family roster needs every cell of every animal of the cage; the cell's value stays in the tables",
    "complete span, no record",
    "a family's cage roster drops only that family (31b per-family roster; 31c/31d CompleteCage per family); the other family keeps the cage; models drop only the affected family cells"),
  rule_version = CC4E_VERSION, stringsAsFactors = FALSE)

#' The released manifest the rule is defined on (sxm_read(): content-hash pinned and validated).
cc4e_manifest <- function(version = CC4E_MANIFEST_VERSION, repo_root = MMM_REPO_ROOT) {
  if (!identical(version, CC4E_MANIFEST_VERSION)) {
    stop("Rule ", CC4E_VERSION, " is defined on manifest ", CC4E_MANIFEST_VERSION, "; another version needs a new rule version.", call. = FALSE)
  }
  if (!exists("sxm_read", mode = "function")) source(file.path(repo_root, "Functions", "spatial_exclusion_manifest.R"))
  sxm_read(version, repo_root = repo_root)
}

# Sorted unique nonempty values, ";"-joined.
cc4e_collapse <- function(x) paste(sort(unique(x[!is.na(x) & nzchar(x)])), collapse = ";")

# Row keys of a table (columns `by`).
.cc4e_key <- function(x, by) do.call(paste, c(lapply(by, function(k) as.character(x[[k]])), sep = "\r"))

#' sha256 of <run>/<rel> checked against <run>/manifest/output_files_sha256.csv; returns the hash, stops on a mismatch.
cc4e_verify_listed <- function(run, rel) {
  mf <- file.path(run, CC4E_RUN_MANIFEST)
  if (!file.exists(mf)) stop("No run manifest ", mf, ": cannot verify ", rel, call. = FALSE)
  m <- utils::read.csv(mf, colClasses = "character", check.names = FALSE)
  names(m) <- tolower(names(m))
  if (!all(c("file", "sha256") %in% names(m))) stop("Run manifest without file and sha256 columns: ", mf, call. = FALSE)
  want <- m$sha256[gsub("\\\\", "/", m$file) == rel]
  f <- file.path(run, rel)
  if (length(want) != 1L || !file.exists(f)) stop(rel, " is not listed once in ", mf, " or is missing", call. = FALSE)
  got <- digest::digest(file = f, algo = "sha256")
  if (!identical(got, want)) stop("sha256 of ", f, " does not match its run manifest (hash mismatch): the file changed after the run.", call. = FALSE)
  got
}

#' Validated CC4 knockout materiality table (interface of the cc4_knockout audit: tables/materiality_cc4.csv). Only the
#' outcomes CC4E_KNOCKOUT_OUTCOMES decide; other rows (e.g. the single-phase rates rate_A1, rate_I2) are kept, unused.
cc4e_validate_knockout <- function(k) {
  k <- as.data.frame(k, stringsAsFactors = FALSE)
  miss <- setdiff(CC4E_KNOCKOUT_COLUMNS, names(k))
  if (length(miss)) stop("Knockout table lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  k$reader <- suppressWarnings(as.integer(as.character(k$reader)))
  if (is.character(k$material)) k$material <- as.logical(toupper(k$material))
  decide <- k$outcome %in% CC4E_KNOCKOUT_OUTCOMES
  if (anyNA(k$reader) || any(!k$reader %in% c(3L, 5L)) || any(!k$family %in% names(CC4E_FAMILIES)) || !any(decide) ||
      !is.logical(k$material) || anyNA(k$material[decide]) || anyDuplicated(k[c("reader", "family", "outcome")])) {
    stop("Invalid knockout table: reader 3 or 5, family Active or Inactive, material TRUE or FALSE for reference_rate, post_rate ",
         "and log_rate_ratio, one row per reader x family x outcome.", call. = FALSE)
  }
  k
}

#' A run record (manifest/run.csv, item/value) verified against the run's manifest; a named character vector.
cc4e_run_record <- function(run) {
  cc4e_verify_listed(run, "manifest/run.csv")
  r <- utils::read.csv(file.path(run, "manifest", "run.csv"), colClasses = "character", check.names = FALSE)
  if (!all(c("item", "value") %in% names(r)) || anyDuplicated(r$item)) stop("Invalid run record: ", file.path(run, "manifest", "run.csv"), call. = FALSE)
  stats::setNames(r$value, r$item)
}

#' The knockout table of a cc4_knockout run, verified against the run's manifest, with its run record. Attributes:
#' path, sha256, run_id, git_commit, and release (TRUE for a release run: not a dev run, clean code, parity gate passed,
#' all six CC4 recordings). `release = TRUE` refuses a run that is not a release.
cc4e_read_knockout <- function(run, release = FALSE) {
  run <- normalizePath(run, winslash = "/", mustWork = TRUE)
  rel <- "tables/materiality_cc4.csv"
  sha <- cc4e_verify_listed(run, rel)
  rr <- cc4e_run_record(run)
  if (!identical(unname(rr["version"]), CC4E_KNOCKOUT_VERSION) || !identical(unname(rr["run_id"]), basename(run))) {
    stop("Not a ", CC4E_KNOCKOUT_VERSION, " run, or its run id differs from its folder: ", run, call. = FALSE)
  }
  is_release <- identical(unname(rr["dev_run"]), "FALSE") && identical(unname(rr["code_clean"]), "TRUE") &&
    identical(unname(rr["parity_gate_pass"]), "TRUE") && identical(unname(rr["recordings"]), CC4E_KNOCKOUT_RECORDINGS)
  if (isTRUE(release) && !is_release) {
    stop("A release eligibility run needs a release CC4 knockout run (dev_run FALSE, code clean, parity gate passed, all six ",
         "recordings): ", run, "; use a dev run id for a dev knockout.", call. = FALSE)
  }
  k <- cc4e_validate_knockout(utils::read.csv(file.path(run, rel), colClasses = "character", check.names = FALSE))
  attr(k, "path") <- normalizePath(file.path(run, rel), winslash = "/")
  attr(k, "sha256") <- sha
  attr(k, "run_id") <- unname(rr["run_id"]); attr(k, "git_commit") <- unname(rr["git_commit"]); attr(k, "release") <- is_release
  attr(k, "record") <- rr[intersect(c("run_id", "git_commit", "dev_run", "code_clean", "parity_gate_pass", "recordings"), names(rr))]
  k
}

#' Status of every animal-phase cell of a Stage 31b animal_phase_activity table (columns AnimalNum, AnimalKey, CageID,
#' Batch, Sex, Phase, PhaseLabel, EligiblePhase, BlockEnd; Group is carried for counting only and never read by the
#' rule). man: the released manifest (cc4e_manifest()); knockout: NULL or a materiality table. Stops when a CC4
#' manifest episode is not a CageID of the table, when its animals differ from the record, or when a censored phase is
#' eligible. Returns one row per cell with CC4E_ELIGIBILITY_COLUMNS.
cc4e_status <- function(animal_phase, man, knockout = NULL) {
  need <- c("AnimalNum", "AnimalKey", "CageID", "Batch", "Sex", "Phase", "PhaseLabel", "EligiblePhase", "BlockEnd")
  miss <- setdiff(need, names(animal_phase))
  if (length(miss)) stop("Animal-phase table lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  a <- data.table::as.data.table(as.data.frame(animal_phase)[c(need, intersect("Group", names(animal_phase)))])
  if (!"Group" %in% names(a)) a[, Group := NA_character_]
  a[, AnimalNum := as.character(AnimalNum)]
  end <- if (inherits(a$BlockEnd, "POSIXct")) as.numeric(a$BlockEnd) else
    as.numeric(as.POSIXct(sub("Z$", "", sub("T", " ", as.character(a$BlockEnd))), tz = "UTC"))
  if (anyNA(a[, setdiff(need, "BlockEnd"), with = FALSE]) || anyNA(end) || !is.logical(a$EligiblePhase) ||
      any(!a$Phase %in% names(CC4E_FAMILIES)) ||
      any(!mapply(function(p, l) l %in% CC4E_FAMILIES[[p]], a$Phase, a$PhaseLabel)) ||
      anyDuplicated(a[, .(AnimalKey, PhaseLabel)]) || any(!grepl("^B[1-6][|]sys[.][1-5][|]CC4$", a$CageID)) ||
      any(sub("[|].*$", "", a$CageID) != a$Batch)) {
    stop("Invalid CC4 animal-phase table (phases, keys or cage episodes).", call. = FALSE)
  }
  per <- a[, .(cells = .N, cages = data.table::uniqueN(CageID)), by = AnimalKey]
  if (any(per$cells != sum(lengths(CC4E_FAMILIES)) | per$cages != 1L)) stop("Every animal needs its nine phases in one cage episode.", call. = FALSE)
  m <- data.table::as.data.table(man)[CC == "CC4"]
  # the manifest's cage episodes are CageIDs of the table (CageID = Batch|System|CageChange = CageEpisodeID), same animals
  for (ep in unique(m$CageEpisodeID)) {
    have <- unique(a[CageID == ep]$AnimalNum)
    if (!length(have)) stop("Manifest episode ", ep, " is not a CageID of the phase table (CageID must equal CageEpisodeID).", call. = FALSE)
    listed <- unique(unlist(strsplit(m[CageEpisodeID == ep]$animals, ";", fixed = TRUE)))
    if (!setequal(have, listed)) stop("Animals of ", ep, " differ from the manifest record: ", paste(sort(have), collapse = ";"), call. = FALSE)
  }
  dead <- m[action == "exclude_spatial" & from == "span_start" & to == "span_end", .(CageEpisodeID, dead_reader = reader, spatial = record_id)]
  mov <- m[action == "exclude_movement", .(CageEpisodeID, movement = record_id)]
  if (!setequal(dead$CageEpisodeID, mov$CageEpisodeID) || anyDuplicated(dead$CageEpisodeID) || any(!grepl("^R[1-8]$", dead$dead_reader))) {
    stop("Rule ", CC4E_VERSION, " covers a whole-session dead reader together with its exclude_movement record; this manifest pairs them differently.", call. = FALSE)
  }
  dead <- merge(dead, mov, by = "CageEpisodeID")
  cen <- m[action == "censor", .(CageEpisodeID, censor = record_id, from_s = as.numeric(as.POSIXct(from, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")))]
  if (anyNA(cen$from_s) || anyDuplicated(cen$CageEpisodeID)) stop("Invalid censor record.", call. = FALSE)
  # inactive materiality of each dead reader in the CC4 knockout (NULL: not verified)
  ko <- NULL
  if (!is.null(knockout)) {
    k <- cc4e_validate_knockout(knockout)
    readers <- unique(as.integer(sub("^R", "", dead$dead_reader)))
    ko <- data.table::rbindlist(lapply(readers, function(r) {
      x <- k[k$reader == r & k$family == "Inactive" & k$outcome %in% CC4E_KNOCKOUT_OUTCOMES, ]
      if (!setequal(x$outcome, CC4E_KNOCKOUT_OUTCOMES)) stop("The knockout table lacks inactive outcomes of R", r, ".", call. = FALSE)
      data.table::data.table(dead_reader = paste0("R", r), material = any(x$material), which = cc4e_collapse(x$outcome[x$material]))
    }))
  }

  i_dead <- match(a$CageID, dead$CageEpisodeID); i_cen <- match(a$CageID, cen$CageEpisodeID)
  a[, `:=`(dead_reader = ifelse(is.na(i_dead), "", dead$dead_reader[i_dead]),
           dead_records = ifelse(is.na(i_dead), "", paste(dead$spatial[i_dead], dead$movement[i_dead], sep = ";")),
           censored = !is.na(i_cen) & end > cen$from_s[i_cen], censor_record = ifelse(is.na(i_cen), "", cen$censor[i_cen]))]
  if (any(a$censored & a$EligiblePhase)) stop("A censored phase is eligible: reads after the censoring time.", call. = FALSE)
  a[, knockout_check := "not_applicable"]
  a[nzchar(dead_reader) & Phase == "Active", knockout_check := "not_used_active"]
  inact <- nzchar(a$dead_reader) & a$Phase == "Inactive"
  ki <- if (is.null(ko)) rep(NA_integer_, nrow(a)) else match(a$dead_reader, ko$dead_reader)
  a[, ko_which := ifelse(is.na(ki), "", ko$which[ki])]
  a[inact, knockout_check := if (is.null(ko)) "unverified" else ifelse(ko$material[ki[inact]], "material", "not_material")]
  # cell statuses by precedence (E04 > E05/E06 > E09 > E07/E08 > E11)
  a[, status := ifelse(EligiblePhase, "included", "incomplete_span")]
  a[EligiblePhase & knockout_check == "not_material", status := "retained_flagged"]
  a[EligiblePhase & knockout_check == "unverified", status := "retained_unverified"]
  a[EligiblePhase & knockout_check == "material", status := "retained_material"]
  a[knockout_check == "not_used_active", status := "excluded_hardware"]
  a[censored == TRUE, status := "censored"]
  ko_ref <- if (is.null(knockout) || is.null(attr(knockout, "run_id"))) "ckr" else paste0("ckr:", attr(knockout, "run_id"))
  kept <- "; manifest v1.1 keeps the light-phase crossing rate"
  a[, `:=`(records = "", note = "")]
  a[status == "excluded_hardware" & Phase == "Active", `:=`(records = dead_records,
    note = paste0(dead_reader, " silent span_start-span_end: every active phase compromised (the movement record was written for the Stage 29 window, CC4 A1)"))]
  a[status == "retained_material", `:=`(records = paste(dead_records, ko_ref, sep = ";"),
    note = paste0(dead_reader, " silent the whole session; cc4 knockout material (", ko_which, ")", kept,
                  ": kept in the primary analyses, flagged; the inactive sensitivity is mandatory; an exclusion needs an approved new rule version"))]
  a[status == "retained_flagged", `:=`(records = paste(dead_records, ko_ref, sep = ";"),
    note = paste0(dead_reader, " silent the whole session; cc4 knockout not material", kept))]
  a[status == "retained_unverified", `:=`(records = dead_records,
    note = paste0(dead_reader, " silent the whole session; no cc4 knockout supplied (the light-window knockout does not cover the grid days I3-I5)", kept))]
  a[status == "censored", `:=`(records = censor_record, note = "after the board's last read: unobserved")]
  a[status == "incomplete_span", note := "incomplete clock or animal span (frozen complete-span rule)"]
  # E10: observed cells of a cage x family that holds a censored, excluded or incomplete cell
  cause <- a[status %in% c("censored", "excluded_hardware", "incomplete_span"),
             .(cause_status = cc4e_collapse(status), cause_records = cc4e_collapse(unlist(strsplit(records, ";", fixed = TRUE)))),
             by = .(CageID, Phase)]
  k <- match(paste(a$CageID, a$Phase), paste(cause$CageID, cause$Phase))
  hit <- a$status %in% c("included", CC4E_RETAINED) & !is.na(k)
  a[hit, `:=`(status = "cage_roster_incomplete", records = cause$cause_records[k[hit]],
              note = paste0("observed; the cage's ", Phase, " roster is incomplete (", cause$cause_status[k[hit]], ")"))]
  a[, `:=`(family = Phase, primary_use = unname(CC4E_PRIMARY_USE[status]), rule_version = CC4E_VERSION)]
  out <- a[, CC4E_ELIGIBILITY_COLUMNS, with = FALSE]
  out[order(Sex, Batch, CageID, AnimalKey, family, PhaseLabel)]
}

#' The inactive sensitivity: the retained inactive cells of a dead-reader episode (retained_unverified, retained_flagged,
#' retained_material) leave the family as excluded cells do. The audit writes it as its own run
#' (--variant=inactive_sensitivity), whose eligibility file Stages 31b-31d read like the primary one.
cc4e_inactive_sensitivity <- function(eligibility) {
  e <- data.table::copy(data.table::as.data.table(eligibility))
  e[family == "Inactive" & status %in% CC4E_RETAINED, `:=`(status = "excluded_hardware", primary_use = "sensitivity_only",
                                                            note = paste(note, "[inactive sensitivity]"))]
  e
}

#' Checks that no cage-label correction moves an animal into or out of a CC4 manifest episode (corrections: the
#' registry of the data version, columns Batch, CageChange, AnimalID, old_System, new_System).
cc4e_check_corrections <- function(man, corrections) {
  m <- data.table::as.data.table(man)[CC == "CC4"]
  miss <- setdiff(c("Batch", "CageChange", "AnimalID", "old_System", "new_System"), names(corrections))
  if (length(miss)) stop("Correction registry lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  canon <- function(x) sub("^0+(?=[0-9])", "", as.character(x), perl = TRUE)
  for (ep in unique(m$CageEpisodeID)) {
    r <- m[CageEpisodeID == ep][1]
    animals <- canon(unlist(strsplit(m[CageEpisodeID == ep]$animals, ";", fixed = TRUE)))
    hit <- corrections$Batch == r$Batch & corrections$CageChange == r$CC &
      (corrections$old_System == r$System | corrections$new_System == r$System | canon(corrections$AnimalID) %in% animals)
    if (any(hit)) stop("A cage-label correction touches manifest episode ", ep, ": ", paste(corrections$AnimalID[hit], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' The eligibility table of an audit run (<run>/tables/outcome_episode_eligibility.csv), verified against the run's
#' manifest/output_files_sha256.csv and validated (columns, rule version, statuses). Attributes: path, sha256, run.
cc4e_read_eligibility <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("MMM_CC4_OUTCOME_ELIGIBILITY must name an existing outcome_episode_eligibility.csv: ", paste(path, collapse = " "), call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/")
  if (basename(path) != basename(CC4E_ELIGIBILITY_FILE) || basename(dirname(path)) != "tables") {
    stop("The eligibility file must be <run>/", CC4E_ELIGIBILITY_FILE, " of an audit run: ", path, call. = FALSE)
  }
  run <- dirname(dirname(path))
  sha <- cc4e_verify_listed(run, CC4E_ELIGIBILITY_FILE)
  rr <- cc4e_run_record(run)
  variant <- if (is.na(rr["variant"])) "primary" else unname(rr["variant"])
  if (!variant %in% CC4E_VARIANTS) stop("Unknown eligibility variant ", variant, ": ", run, call. = FALSE)
  dev_ok <- identical(toupper(Sys.getenv("MMM_CC4_ALLOW_DEV_ELIGIBILITY")), "TRUE")
  is_release <- identical(unname(rr["dev_run"]), "FALSE") && identical(unname(rr["code_clean"]), "TRUE")
  if (!is_release && !dev_ok) {
    stop("The eligibility file comes from a dev audit run or uncommitted code (", run, "); use a release audit run, or set ",
         "MMM_CC4_ALLOW_DEV_ELIGIBILITY=TRUE for a dev rerun.", call. = FALSE)
  }
  e <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!identical(names(e), CC4E_ELIGIBILITY_COLUMNS)) stop("Eligibility file columns differ from CC4E_ELIGIBILITY_COLUMNS.", call. = FALSE)
  if (!nrow(e) || any(e$rule_version != CC4E_VERSION)) stop("Eligibility file of another rule version than ", CC4E_VERSION, ".", call. = FALSE)
  if (any(!e$status %in% CC4E_STATUSES) || any(e$primary_use != CC4E_PRIMARY_USE[e$status]) || any(!e$EligiblePhase %in% c("TRUE", "FALSE")) ||
      any(!mapply(function(f, l) f %in% names(CC4E_FAMILIES) && l %in% CC4E_FAMILIES[[f]], e$family, e$PhaseLabel)) ||
      anyDuplicated(e[, .(AnimalKey, PhaseLabel)])) {
    stop("Invalid eligibility file (statuses, phases or duplicate cells).", call. = FALSE)
  }
  e[, EligiblePhase := EligiblePhase == "TRUE"]
  attr(e, "path") <- path; attr(e, "sha256") <- sha; attr(e, "run") <- run
  attr(e, "variant") <- variant; attr(e, "release") <- is_release
  e
}

#' Opt-in of Stages 31b-31d: the output directory must be an isolated root (mmm_isolated_output_root_check()); returns
#' the verified eligibility table.
cc4e_opt_in <- function(path, output_dir) {
  if (!exists("mmm_isolated_output_root_check", mode = "function")) stop("cc4e_opt_in() needs Functions/project_paths.R.", call. = FALSE)
  mmm_isolated_output_root_check(output_dir)
  cc4e_read_eligibility(path)
}

#' Files a stage hashes as inputs when the rule is applied (character(0) without it).
cc4e_input_files <- function(eligibility, repo_root = MMM_REPO_ROOT) {
  if (is.null(eligibility)) return(character(0))
  c(attr(eligibility, "path"), file.path(attr(eligibility, "run"), CC4E_RUN_MANIFEST), file.path(repo_root, "Functions", "cc4_outcome_eligibility.R"))
}

#' Lines a stage adds to its protocol or README when the rule is applied (character(0) without it).
cc4e_protocol_lines <- function(eligibility) {
  if (is.null(eligibility)) return(character(0))
  variant <- if (is.null(attr(eligibility, "variant"))) "primary" else attr(eligibility, "variant")
  c(paste0("Outcome eligibility ", CC4E_VERSION, " (manifest ", CC4E_MANIFEST_VERSION, "), variant ", variant,
           if (variant == "inactive_sensitivity") " (INACTIVE SENSITIVITY: the retained inactive cells of dead-reader episodes are left out)" else "",
           if (isFALSE(attr(eligibility, "release"))) " [DEV eligibility file, MMM_CC4_ALLOW_DEV_ELIGIBILITY=TRUE: not a release]" else "",
           ": ", attr(eligibility, "path"), " sha256 ", attr(eligibility, "sha256"), "."),
    "A cage leaves a phase family's roster when a cell of that family is excluded_hardware or censored; the other family keeps it. No animal is removed; values are unchanged.",
    "The all-episode analysis (every cage episode kept) is the frozen run of the same stage.")
}

#' Appends the eligibility of each row (matched by AnimalKey x PhaseLabel) as the last columns FamilyExcluded,
#' EligibilityStatus, EligibilityRecord and EligibilityVersion. Every row needs exactly one eligibility row, the animals
#' must be the same, and the statuses must agree with EligiblePhase (else the file was built from another table).
cc4e_join <- function(a, eligibility) {
  if (any(CC4E_COLUMNS %in% names(a))) stop("The phase table already carries eligibility columns.", call. = FALSE)
  e <- as.data.frame(eligibility, stringsAsFactors = FALSE)
  ka <- .cc4e_key(a, c("AnimalKey", "PhaseLabel")); i <- match(ka, .cc4e_key(e, c("AnimalKey", "PhaseLabel")))
  if (anyNA(i) || anyDuplicated(ka) || !setequal(unique(a$AnimalKey), unique(e$AnimalKey))) {
    stop("The eligibility table does not match the phase table (animals or phases differ).", call. = FALSE)
  }
  e <- e[i, , drop = FALSE]
  if (any(e$CageID != a$CageID | e$AnimalNum != as.character(a$AnimalNum) | e$Batch != a$Batch | e$Sex != a$Sex | e$family != a$Phase)) {
    stop("The eligibility table's cage episode, animal, batch, sex or family differs from the phase table.", call. = FALSE)
  }
  ok <- a$EligiblePhase; st <- e$status
  if (any(st == "incomplete_span" & ok) || any(st %in% c("included", CC4E_RETAINED, "cage_roster_incomplete") & !ok)) {
    stop("The eligibility statuses do not match the phase eligibility of this table; the file was built from another phase table.", call. = FALSE)
  }
  a$FamilyExcluded <- st %in% CC4E_FAMILY_EXCLUDING
  a$EligibilityStatus <- st
  a$EligibilityRecord <- e$records
  a$EligibilityVersion <- e$rule_version
  a
}

#' Appends the collapsed eligibility of the matching cells (keys `by`) to the rows of an aggregated table.
cc4e_append_status <- function(tab, cells, by) {
  kc <- .cc4e_key(cells, by); kt <- .cc4e_key(tab, by)
  for (col in c("EligibilityStatus", "EligibilityRecord", "EligibilityVersion")) {
    v <- tapply(as.character(cells[[col]]), kc, cc4e_collapse)
    tab[[col]] <- as.vector(v[kt])
  }
  tab
}

#' ExclusionReason of each `by` group: for the cells outside the roster (`included` FALSE), "<status>:<CageID>
#' (<records>)" of the causing cells of each cage (censored, excluded_hardware, incomplete_span), else
#' "cage_roster_incomplete:<CageID>"; sorted, "; "-joined; "" when no cell of the group is outside the roster.
cc4e_append_reason <- function(tab, cells, by, included) {
  x <- as.data.frame(cells, stringsAsFactors = FALSE)
  x <- x[!x[[included]], , drop = FALSE]
  if (!nrow(x)) { tab$ExclusionReason <- ""; return(tab) }
  causal <- x$EligibilityStatus %in% c("censored", "excluded_hardware", "incomplete_span")
  lab <- paste0(x$EligibilityStatus, ":", x$CageID, ifelse(nzchar(x$EligibilityRecord), paste0(" (", x$EligibilityRecord, ")"), ""))
  gk <- .cc4e_key(x, by)
  cage_causal <- stats::ave(causal, paste(gk, x$CageID), FUN = any)
  lab[!cage_causal] <- paste0("cage_roster_incomplete:", x$CageID[!cage_causal])
  keep <- causal | !cage_causal
  reason <- tapply(lab[keep], gk[keep], function(l) paste(sort(unique(l)), collapse = "; "))
  r <- as.vector(reason[.cc4e_key(tab, by)])
  tab$ExclusionReason <- ifelse(is.na(r), "", r)
  tab
}

#' Sample sizes of a roster: data.frame(Sex, family, Group, AnimalKey, CageID, Batch) of the included animals (one row
#' per animal and family). Returns per Sex x family x Group (CON, RES, SUS, all): Animals, Cages, CONCages, Batches,
#' MixedCages (cages holding RES and SUS animals).
cc4e_population <- function(x) {
  x <- unique(data.table::as.data.table(as.data.frame(x)[c("Sex", "family", "Group", "AnimalKey", "CageID", "Batch")]))
  if (!nrow(x)) return(data.table::data.table(Sex = character(), family = character(), Group = character(), Animals = integer(),
                                              Cages = integer(), CONCages = integer(), Batches = integer(), MixedCages = integer()))
  mixed <- x[, .(mixed = all(c("RES", "SUS") %in% Group)), by = .(Sex, family, CageID)]
  x <- merge(x, mixed, by = c("Sex", "family", "CageID"))
  x[, con := Group == "CON"]
  count <- function(d) d[, .(Animals = data.table::uniqueN(AnimalKey), Cages = data.table::uniqueN(CageID),
                             CONCages = data.table::uniqueN(CageID[con]), Batches = data.table::uniqueN(Batch),
                             MixedCages = data.table::uniqueN(CageID[mixed])), by = .(Sex, family, Group)]
  out <- rbind(count(x), count(data.table::copy(x)[, Group := "all"]))
  out[order(Sex, family, match(Group, c("CON", "RES", "SUS", "all")))]
}
