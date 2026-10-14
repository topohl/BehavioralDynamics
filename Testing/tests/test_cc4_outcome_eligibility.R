# ===========================================================================
# CC4 outcome x cage-episode eligibility (Functions/cc4_outcome_eligibility.R, rule cc4e_v1.0), its opt-in wiring in
# Stages 31b-31d (MMM_CC4_OUTCOME_ELIGIBILITY) and the audit's roster functions (Testing/audits/
# audit_cc4_outcome_eligibility.R, sourced for its definitions only). Portable: fixtures under tempdir() and the released
# manifest v1.1 of the repository; no model is fitted and nothing on the lab drive is read or written.
#
#  1. the rule table: versions, one clause per status, exclude_spatial not applied, censoring first, the active family
#     excluded A1-A5, the inactive family by the CC4 knockout, no clause reads a group label
#  2. statuses: B2|sys.5|CC4 active A1-A5 excluded_hardware (SX02;SX06); inactive kept (manifest v1.1 keeps the
#     light-phase rate): retained_unverified without a knockout, retained_material (primary, flagged; a decision for
#     Tobias) when material for any inactive outcome, retained_flagged when not, the knockout run cited; B5|sys.1|CC4 A1-A2
#     excluded_hardware (SX03;SX07), A3-A5 and I3-I5 censored (SX04), I2 observed but outside the roster; every other
#     cage included; statuses are group-blind
#  3. fail closed: the manifest episode must be a CageID with the record's animals; a cage-label correction touching an
#     episode; a censored eligible phase; an incomplete or invalid knockout table; another manifest version
#  4. legacy byte parity: without the env var, the 31b driver's outputs and grid31_stats_data()'s model inputs are byte
#     for byte those of the code before the rule (digests pinned on this fixture with that code, commit 8ba26ab)
#  5. per-family roster: 31b and 31c/31d drop B2|sys.5|CC4 from the active family only; no animal row is dropped, values
#     are unchanged, the new columns come last, rosters and populations gain ExclusionReason; the inactive sensitivity
#     drops the flagged inactive episode only
#  6. the eligibility file: read back verified with its variant; a changed byte, another rule version, another location,
#     a table it was not built from and a dev audit run (unless MMM_CC4_ALLOW_DEV_ELIGIBILITY=TRUE) are refused; the
#     knockout run: its run record verified, a dev or failed-gate knockout refused for a release eligibility run
# 10. the audit main block: the protection guard starts before the first write and ends before the output hashes
#  7. the drivers: 31b with the env var writes its per-family tables (same files, columns last, inputs hashed) and
#     refuses a non-isolated output root; 31c and 31d refuse a non-isolated root and a changed eligibility file before
#     any fit and write nothing
#  8. sample sizes by hand on the fixture (frozen, primary, inactive sensitivity), missing vs excluded, affected outputs,
#     cross-CC episodes
#  9. separation: Stages 31b-31d read the eligibility file only, never the manifest; the helper is an allowed reader
# ===========================================================================
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "cc4_outcome_eligibility.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
fails <- function(expr, pattern) {
  e <- tryCatch({ force(expr); NULL }, error = identity)
  inherits(e, "error") && grepl(pattern, conditionMessage(e))
}
Sys.unsetenv("MMM_CC4_OUTCOME_ELIGIBILITY")
suppressPackageStartupMessages({
  library(data.table)
  source("Analysis/31b_cc4_phase_groups.R")
  source("Analysis/31d_cc4_phenotype_statistics.R")   # sources Analysis/31c_cc4_phase_statistics.R
  source("Testing/audits/audit_cc4_outcome_eligibility.R")
})
sha <- function(f) digest::digest(file = f, algo = "sha256")
tmp <- tempfile("cc4e_"); dir.create(tmp)

# Fixture: Male batches B1, B2 and B5 with the two manifest v1.1 CC4 episodes (same cage keys and animals as the
# records): B2|sys.5|CC4 (dead R3, SX02/SX06) and B5|sys.1|CC4 (dead R5, SX03/SX07; censored from 2023-12-04
# 14:00:42.962, SX04, so its A3-A5 and I3-I5 have no complete span). Every other cage is complete. Rates are
# deterministic.
fx_cages <- list(
  B1 = list(sys.1 = c(F101 = "CON", F102 = "CON", F103 = "CON"), sys.2 = c(F111 = "RES", F112 = "SUS", F113 = "RES")),
  B2 = list(sys.1 = c(F201 = "CON", F202 = "CON", F203 = "CON", F204 = "CON"), sys.3 = c(F211 = "RES", F212 = "SUS", F213 = "RES"),
            sys.5 = c(OR106 = "RES", OR113 = "SUS", OR135 = "RES", OR138 = "RES")),
  B5 = list(sys.1 = c("314" = "RES", "318" = "RES", OR620 = "SUS", OR630 = "RES"), sys.2 = c(F501 = "CON", F502 = "CON", F503 = "CON"),
            sys.3 = c(F511 = "RES", F512 = "SUS")))
fx_start <- c(B1 = "2022-11-09 06:30:00", B2 = "2023-02-10 06:30:00", B5 = "2023-12-02 06:30:00")
fx_build <- function() {
  rows <- list(); labels <- list()
  for (b in names(fx_cages)) for (s in names(fx_cages[[b]])) {
    g <- fx_cages[[b]][[s]]; ids <- names(g)
    for (k in seq_along(ids)) {
      id <- ids[k]; num <- grepl("^[0-9]+$", id)
      labels[[length(labels) + 1L]] <- tibble::tibble(AnimalNum = id, Sex = "Male", Batch = sub("^B", "", b), outcome_group = g[[k]],
                                                      experimental_condition = ifelse(g[[k]] == "CON", "CON", "SIS"))
      for (family in c("Inactive", "Active")) for (ph in if (family == "Inactive") 2:5 else 1:5) {
        st <- as.POSIXct(fx_start[[b]], tz = "UTC") + (ph - 1) * 86400 + ifelse(family == "Active", 43200, 0)
        lost <- b == "B5" && s == "sys.1" && ph >= 3
        rate <- if (lost && !(family == "Inactive" && ph == 3)) 0 else
          20 + 2 * match(s, paste0("sys.", 1:5)) + k + (ph - 2) * (match(g[[k]], c("CON", "RES", "SUS")) + ifelse(family == "Active", 3, 1))
        rows[[length(rows) + 1L]] <- tibble::tibble(Batch = b, CageChange = "CC4", System = s,
          AnimalID = if (num) sprintf("%05d", as.integer(id)) else id, AnimalNum = id, Sex = "Male", Phase = family,
          PhaseLabel = paste0(substr(family, 1, 1), ph), PhaseNumber = ph, BlockStart = st, BlockEnd = st + 43200, BinIndex = 0:143,
          Crossings = c(12 * rate, rep(0, 143)), SessionSpansWholeBlock = TRUE, AnimalSpansWholeBlock = !lost,
          SpanOverlapSec = if (lost) 0 else 300)
      }
    }
  }
  list(bins = dplyr::bind_rows(rows), outcomes = dplyr::bind_rows(labels))
}
# A Stage 31 run folder holding the bins and their output manifest, as 31b reads it.
fx_stage31 <- function(bins, dir) {
  dir.create(file.path(dir, "tables"), recursive = TRUE); dir.create(file.path(dir, "audit"))
  f <- file.path(dir, "tables", "animal_clock_profiles_5min.csv")
  readr::write_csv(bins, f)
  readr::write_csv(tibble::tibble(File = "tables/animal_clock_profiles_5min.csv", SHA256 = mmm_file_sha256(f)),
                   file.path(dir, "audit", "output_manifest.csv"))
  dir
}
# The 31b animal table as 31c and 31d read it.
fx_read_phases <- function(path) readr::read_csv(path, col_types = readr::cols(.default = readr::col_guess(),
  AnimalNum = readr::col_character(), AnimalID = readr::col_character()), show_col_types = FALSE)
# A minimal audit run holding an eligibility table and its run manifest.
fx_run <- function(e, dir, dev = FALSE, variant = "primary") {
  dir.create(file.path(dir, "tables"), recursive = TRUE); dir.create(file.path(dir, "manifest"))
  f <- file.path(dir, CC4E_ELIGIBILITY_FILE)
  data.table::fwrite(e, f)
  r <- file.path(dir, "manifest", "run.csv")
  data.table::fwrite(data.table::data.table(item = c("run_id", "rule_version", "variant", "code_clean", "dev_run"),
                                            value = c(basename(dir), CC4E_VERSION, variant, as.character(!dev), as.character(dev))), r)
  rel <- c(CC4E_ELIGIBILITY_FILE, "manifest/run.csv")
  data.table::fwrite(data.table::data.table(file = rel, sha256 = vapply(file.path(dir, rel), sha, "", USE.NAMES = FALSE), bytes = file.size(file.path(dir, rel))),
                     file.path(dir, CC4E_RUN_MANIFEST))
  normalizePath(f, winslash = "/")
}
# A CC4 knockout run folder: materiality table, run record and run manifest.
fx_knockout_run <- function(k, dir, dev = FALSE, gate = TRUE, recordings = CC4E_KNOCKOUT_RECORDINGS, version = CC4E_KNOCKOUT_VERSION) {
  dir.create(file.path(dir, "tables"), recursive = TRUE); dir.create(file.path(dir, "manifest"))
  utils::write.csv(k, file.path(dir, "tables", "materiality_cc4.csv"), row.names = FALSE)
  data.table::fwrite(data.table::data.table(item = c("run_id", "version", "git_commit", "code_clean", "dev_run", "recordings", "parity_gate_pass"),
                                            value = c(basename(dir), version, "abc1234", as.character(!dev), as.character(dev), recordings,
                                                      as.character(gate))), file.path(dir, "manifest", "run.csv"))
  rel <- c("tables/materiality_cc4.csv", "manifest/run.csv")
  data.table::fwrite(data.table::data.table(file = rel, sha256 = vapply(file.path(dir, rel), sha, "", USE.NAMES = FALSE), bytes = file.size(file.path(dir, rel))),
                     file.path(dir, CC4E_RUN_MANIFEST))
  dir
}
# A CC4 knockout materiality table: R3 and R5 material for every active outcome; inactive post_rate as given.
fx_knockout <- function(r3_post = FALSE, r5_post = FALSE) data.frame(
  reader = rep(c(3L, 5L), each = 6), family = rep(rep(c("Active", "Inactive"), each = 3), 2), outcome = rep(CC4E_KNOCKOUT_OUTCOMES, 4),
  n_animals = 400L, n_board_recordings = 117L, scale_sd = 1, median_shift = 0, median_abs_shift_sd = 0.1, p90_abs_shift_sd = 0.3,
  material = c(TRUE, TRUE, TRUE, FALSE, r3_post, FALSE, TRUE, TRUE, TRUE, FALSE, r5_post, FALSE),
  rule = "median |shift| >= 0.2 SD or 90th percentile >= 0.5 SD", stringsAsFactors = FALSE)
B2E <- "B2|sys.5|CC4"; B5E <- "B5|sys.1|CC4"

# ------------------------------------------------------------------ 1. the rule table
rl <- function(id) CC4E_RULES[match(id, CC4E_RULES$rule_id), ]
check(identical(CC4E_VERSION, "cc4e_v1.0") && identical(CC4E_MANIFEST_VERSION, "v1.1") && all(CC4E_RULES$rule_version == CC4E_VERSION),
      "1: rule cc4e_v1.0 on manifest v1.1")
check(nrow(CC4E_RULES) == 12L && !anyDuplicated(CC4E_RULES$rule_id) && all(CC4E_STATUSES %in% CC4E_RULES$status) &&
        all(setdiff(CC4E_RULES$status, c("none", "not applied", "unaffected")) %in% CC4E_STATUSES), "1: one clause per status")
check(rl("E02")$status == "not applied" && grepl("exclude_spatial", rl("E02")$scope) && rl("E03")$status == "unaffected",
      "1: exclude_spatial not applied as such; CC1-CC3 records do not act")
check(rl("E04")$status == "censored" && rl("E04")$primary_use == "missing" && grepl("precedence", rl("E04")$rationale), "1: censoring first, missing")
check(rl("E05")$family == "Active" && rl("E05")$phases == "A1-A5" && rl("E05")$status == "excluded_hardware" && rl("E05")$primary_use == "sensitivity_only",
      "1: the active family of a dead-reader episode is excluded in A1-A5, kept in the sensitivity")
check(identical(rl(c("E06", "E07", "E08"))$status, c("retained_material", "retained_flagged", "retained_unverified")) &&
        all(rl(c("E06", "E07", "E08"))$primary_use == "primary_flagged") && all(rl(c("E06", "E07", "E08"))$family == "Inactive") &&
        all(grepl("keeps the light-phase crossing rate", rl(c("E06", "E07", "E08"))$rationale)) && grepl("approved new rule version", rl("E06")$rationale),
      "1: the inactive family is kept and graded by the CC4 knockout; a material knockout never excludes it")
check(grepl("never remove", rl("E12")$condition) && !any(grepl("\\b(Group|CON|RES|SUS)\\b", CC4E_RULES$condition)), "1: no animal removed; no group label read")
check(identical(unname(CC4E_PRIMARY_USE[CC4E_STATUSES]), c("primary", "primary_flagged", "primary_flagged", "primary_flagged", "sensitivity_only", "missing",
                                                           "missing", "outside_roster")), "1: primary use per status")

# ------------------------------------------------------------------ 2. statuses
fx <- fx_build()
legacy <- grid31_group_phases(fx$bins, fx$outcomes)
man <- cc4e_manifest()
e0 <- cc4e_status(legacy$animals, man)
check(nrow(e0) == nrow(legacy$animals) && identical(names(e0), CC4E_ELIGIBILITY_COLUMNS) && all(e0$rule_version == CC4E_VERSION),
      "2: one row per animal-phase cell")
b2 <- e0[CageID == B2E]; b5 <- e0[CageID == B5E]
check(all(b2[family == "Active"]$status == "excluded_hardware") && setequal(b2[family == "Active"]$PhaseLabel, paste0("A", 1:5)) &&
        all(b2[family == "Active"]$records == "SX02;SX06") && all(b2[family == "Active"]$primary_use == "sensitivity_only"),
      "2: B2|sys.5 active A1-A5 excluded_hardware (SX02;SX06)")
check(all(b2[family == "Inactive"]$status == "retained_unverified") && all(b2[family == "Inactive"]$primary_use == "primary_flagged") &&
        all(b2[family == "Inactive"]$knockout_check == "unverified"), "2: B2|sys.5 inactive retained_unverified without a knockout")
e_mat <- cc4e_status(legacy$animals, man, fx_knockout(r3_post = TRUE))
e_not <- cc4e_status(legacy$animals, man, fx_knockout())
check(all(e_mat[CageID == B2E & family == "Inactive"]$status == "retained_material") &&
        all(e_mat[CageID == B2E & family == "Inactive"]$primary_use == "primary_flagged") &&
        all(grepl("knockout material (post_rate)", e_mat[CageID == B2E & family == "Inactive"]$note, fixed = TRUE)) &&
        all(grepl("approved new rule version", e_mat[CageID == B2E & family == "Inactive"]$note, fixed = TRUE)),
      "2: retained_material (kept, flagged; a decision for Tobias) when the CC4 knockout is material for any inactive outcome")
k_run <- fx_knockout(r3_post = TRUE); attr(k_run, "run_id") <- "ckr_v1.0_abc1234"
check(all(cc4e_status(legacy$animals, man, k_run)[CageID == B2E & family == "Inactive"]$records == "SX02;SX06;ckr:ckr_v1.0_abc1234"),
      "2: knockout-graded cells cite the knockout run")
check(all(e_not[CageID == B2E & family == "Inactive"]$status == "retained_flagged") && all(e_not[CageID == B2E & family == "Inactive"]$knockout_check == "not_material"),
      "2: retained_flagged when the CC4 knockout is not material")
k_extra <- rbind(fx_knockout(), transform(fx_knockout()[fx_knockout()$outcome == "reference_rate", ], outcome = ifelse(family == "Active", "rate_A1", "rate_I2"),
                                           material = TRUE))
check(identical(cc4e_status(legacy$animals, man, k_extra)$status, e_not$status), "2: only reference_rate, post_rate and log_rate_ratio decide (single-phase rates ignored)")
check(identical(e0[family == "Active", .(status, records)], e_mat[family == "Active", .(status, records)]) &&
        identical(e0[family == "Active", .(status, records)], e_not[family == "Active", .(status, records)]), "2: the active family does not depend on the knockout")
check(identical(b5[family == "Active"]$status, rep(c("excluded_hardware", "excluded_hardware", "censored", "censored", "censored"), 4)) &&
        all(b5[family == "Active" & status == "excluded_hardware"]$records == "SX03;SX07") && all(b5[status == "censored"]$records == "SX04") &&
        all(b5[status == "censored"]$primary_use == "missing"), "2: B5|sys.1 A1-A2 excluded_hardware (SX03;SX07), A3-A5 censored (SX04)")
check(all(b5[PhaseLabel == "I2"]$status == "cage_roster_incomplete") && all(b5[PhaseLabel == "I2"]$EligiblePhase) &&
        all(b5[PhaseLabel == "I2"]$records == "SX04") && all(b5[family == "Inactive" & PhaseLabel != "I2"]$status == "censored"),
      "2: B5|sys.1 I2 observed but outside the complete-family roster; I3-I5 censored")
check(all(e_mat[CageID == B5E & PhaseLabel != "I2" & family == "Inactive"]$status == "censored") &&
        all(cc4e_status(legacy$animals, man, fx_knockout(r5_post = TRUE))[CageID == B5E & PhaseLabel == "I2"]$status == "cage_roster_incomplete"),
      "2: censoring takes precedence; a material R5 knockout leaves B5's observed I2 outside the roster through the censoring, not excluded")
check(all(e0[!CageID %in% c(B2E, B5E)]$status == "included") && all(e0[!CageID %in% c(B2E, B5E)]$records == ""), "2: every other cage included")
shuffled <- legacy$animals; set.seed(7); shuffled$Group <- sample(shuffled$Group)
check(identical(cc4e_status(shuffled, man)[, !"Group"], e0[, !"Group"]), "2: statuses are group-blind")

# ------------------------------------------------------------------ 3. fail closed
bad <- legacy$animals; bad$CageID[bad$CageID == B2E] <- "B2|sys.4|CC4"
check(fails(cc4e_status(bad, man), "not a CageID"), "3: a manifest episode must be a CageID of the table")
bad <- legacy$animals; bad$AnimalNum[bad$AnimalNum == "OR138"] <- "OR139"
check(fails(cc4e_status(bad, man), "differ from the manifest"), "3: the episode's animals must equal the record's")
bad <- legacy$animals; bad$EligiblePhase[bad$CageID == B5E & bad$PhaseLabel == "I4"] <- TRUE
check(fails(cc4e_status(bad, man), "censored phase is eligible"), "3: a censored eligible phase is refused")
corr <- data.frame(Batch = c("B1", "B6"), CageChange = c("CC2", "CC4"), AnimalID = c("OQ764", "OR646"), old_System = c("sys.5", "sys.2"),
                   new_System = c("sys.2", "sys.5"), stringsAsFactors = FALSE)
check(isTRUE(cc4e_check_corrections(man, corr)), "3: the registry's corrections do not touch a CC4 manifest episode")
check(fails(cc4e_check_corrections(man, rbind(corr, data.frame(Batch = "B2", CageChange = "CC4", AnimalID = "OR135", old_System = "sys.2",
                                                               new_System = "sys.3"))), "touches") &&
        fails(cc4e_check_corrections(man, rbind(corr, data.frame(Batch = "B5", CageChange = "CC4", AnimalID = "OR700", old_System = "sys.1",
                                                                 new_System = "sys.2"))), "touches"),
      "3: a correction moving an episode's animal or a label into or out of the episode is refused")
k <- fx_knockout(); k <- k[!(k$reader == 3L & k$family == "Inactive" & k$outcome == "log_rate_ratio"), ]
check(fails(cc4e_status(legacy$animals, man, k), "lacks inactive outcomes"), "3: an incomplete knockout table is refused")
k <- fx_knockout(); k$material[2] <- NA
check(fails(cc4e_status(legacy$animals, man, k), "Invalid knockout"), "3: an invalid knockout table is refused")
check(fails(cc4e_manifest("v1.0"), "defined on manifest v1.1"), "3: the rule is tied to manifest v1.1")

# ------------------------------------------------------------------ 4. legacy byte parity
# sha256 of the outputs of the code before the rule (commit 8ba26ab) on this fixture; input_manifest.csv (code hashes,
# temp paths) and manifest.csv are not compared. A readr release that formats CSV differently needs new pins; the
# audit's frozen_parity.csv checks the real frozen tables independently.
PINS_31B <- c(
  "animal_phase_activity.csv" = "a124e18222b8c480945026261b7a7cbd275a3697bb7eb760f60e061b1a99ad63",
  "batch_group_contrasts.csv" = "85b684af3689fa47eb2ce1b8ee7d016e89a0162853d9fafc7baf7c0488f538fc",
  "batch_group_phase_activity.csv" = "28f717404ed82bbfe6baaae7b893b00a409185056a7077fb4370154704cdd99d",
  "cage_group_phase_activity.csv" = "000e2d471c9eeda0a7ae8006e3afa8cdc0a99c738f746083ca2fb84f2a42b6b7",
  "canonical_outcomes.csv" = "1eb910e7d59ee92ea5b33cfe83d70f044c1c35fba554a0e792830bbe7c110502",
  "group_phase_contrasts.csv" = "82f846c0a2571873f7dc4f97b0685491153a49cef5f4c8de9a009ed4b75ea744",
  "group_phase_summary.csv" = "a0dafcf169dc53ec1722389f4d556e9d16660e9f5910d2f8ff33d882f4bdccd5",
  "population.csv" = "fbb4682e7c321070ae679ff05dce1ca6e2ac9e86e05d514d57f106faf23e646e",
  "README.txt" = "a02dfbd851a950115b40c262804b840ab974daa4610fdc7b17a9489432516f33")
PINS_31C <- c(
  "animals_model_input.csv" = "6bd461cf7a229ebdc89cf9a70c3a1160659f280bebbcea83c3dbde5b29b5d007",
  "cages_model_input.csv" = "7f15c5491870bfb2f1eba481fdb6b796e8e4d00a7194ccfb65ca110c4a826e23",
  "roster_model_input.csv" = "ecd983d10cbaf0d26117b24ca36bc87d00c98d3d6946f58bc40390050ef10bbd")
src <- fx_stage31(fx$bins, file.path(tmp, "s31"))
readr::write_csv(fx$outcomes, file.path(tmp, "outcomes.csv"))
invisible(run_cc4_phase_groups(src, file.path(tmp, "outcomes.csv"), file.path(tmp, "legacy")))
got <- vapply(names(PINS_31B), function(f) sha(file.path(tmp, "legacy", f)), "")
check(identical(unname(got), unname(PINS_31B)), paste("4: 31b outputs without the env var are byte-identical to the code before the rule:",
                                                      paste(names(PINS_31B)[got != PINS_31B], collapse = ", ")))
check(setequal(list.files(file.path(tmp, "legacy")), c(names(PINS_31B), "input_manifest.csv", "manifest.csv")), "4: no extra 31b output file")
im <- utils::read.csv(file.path(tmp, "legacy", "input_manifest.csv"), colClasses = "character")
check(nrow(im) == 9L && !any(grepl("eligibility", im$Path)), "4: the legacy run hashes the same nine inputs")
ap <- fx_read_phases(file.path(tmp, "legacy", "animal_phase_activity.csv"))
s <- grid31_stats_data(ap)
check(identical(names(s), c("animals", "cages", "roster")), "4: grid31_stats_data() returns the legacy elements")
for (nm in names(s)) {
  f <- file.path(tmp, paste0(nm, "_model_input.csv")); readr::write_csv(s[[nm]], f, na = "")
  check(identical(sha(f), unname(PINS_31C[paste0(nm, "_model_input.csv")])), paste("4: 31c model input byte-identical without the rule:", nm))
}

# ------------------------------------------------------------------ 5. per-family roster
r <- grid31_group_phases(fx$bins, fx$outcomes, e0)
inf <- function(x, cage, fam) unique(x$IncludedInPrimary[x$CageID == cage & x$Phase == fam])
check(nrow(r$animals) == nrow(legacy$animals) && identical(names(r$animals), c(names(legacy$animals), CC4E_COLUMNS)),
      "5: 31b keeps every animal row; the eligibility columns come last")
check(isTRUE(inf(legacy$animals, B2E, "Active")) && isFALSE(inf(r$animals, B2E, "Active")) && isTRUE(inf(r$animals, B2E, "Inactive")),
      "5: 31b drops B2|sys.5 from the active family only")
check(isFALSE(inf(r$animals, B5E, "Active")) && isFALSE(inf(r$animals, B5E, "Inactive")) && all(r$animals$IncludedInPrimary[!r$animals$CageID %in% c(B2E, B5E)]),
      "5: B5|sys.1 stays out of both families; every other cage stays in")
check(identical(r$animals$CrossingsPerHour, legacy$animals$CrossingsPerHour) && identical(r$animals$ChangeFromBaseline, legacy$animals$ChangeFromBaseline) &&
        identical(r$animals$IncludedInFamily, legacy$animals$IncludedInFamily), "5: values and the family flag unchanged")
p <- r$population
pr <- function(b, g, l) p[p$Batch == b & p$Group == g & p$PhaseLabel == l, ]
check(identical(names(p), c(names(legacy$population), "ExclusionReason")), "5: the 31b population gains ExclusionReason last")
check(pr("B2", "SUS", "A3")$ExclusionReason == "excluded_hardware:B2|sys.5|CC4 (SX02;SX06)" && pr("B2", "SUS", "A3")$PrimaryAnimals == 1L &&
        pr("B2", "SUS", "I3")$ExclusionReason == "" && pr("B2", "SUS", "I3")$PrimaryAnimals == 2L, "5: B2 SUS: active on the B2|sys.3 animal only")
check(pr("B5", "RES", "I2")$ExclusionReason == "cage_roster_incomplete:B5|sys.1|CC4" &&
        pr("B5", "RES", "A3")$ExclusionReason == "censored:B5|sys.1|CC4 (SX04)" &&
        pr("B5", "RES", "A1")$ExclusionReason == "excluded_hardware:B5|sys.1|CC4 (SX03;SX07)" && all(p$ExclusionReason[p$Batch == "B1"] == ""),
      "5: ExclusionReason names the cause and the record")
s1 <- grid31_stats_data(ap, e0)
ro1 <- s1$roster
rr <- function(x, cage, fam) x[x$CageID == cage & x$Phase == fam, ]
check(!rr(ro1, B2E, "Active")$Included && rr(ro1, B2E, "Inactive")$Included && rr(ro1, B2E, "Inactive")$EligibilityStatus == "retained_unverified" &&
        rr(ro1, B2E, "Active")$ExclusionReason == "excluded_hardware:B2|sys.5|CC4 (SX02;SX06)", "5: 31c/31d roster per family with the reason")
check(rr(ro1, B5E, "Active")$ExclusionReason == "censored:B5|sys.1|CC4 (SX04); excluded_hardware:B5|sys.1|CC4 (SX03;SX07)" &&
        rr(ro1, B5E, "Inactive")$ExclusionReason == "censored:B5|sys.1|CC4 (SX04)", "5: B5|sys.1: censored (missing) and excluded, per family")
check(identical(names(ro1), c(names(s$roster), "EligibilityStatus", "ExclusionReason")) &&
        identical(names(s1$animals), c(names(s$animals), "EligibilityStatus", "EligibilityRecord", "EligibilityVersion")) &&
        identical(names(s1$cages), c(names(s$cages), "EligibilityStatus", "EligibilityRecord", "EligibilityVersion")) && "eligibility" %in% names(s1),
      "5: model inputs gain their eligibility last; the cells used are returned")
kept <- merge(as.data.frame(s1$animals)[c("AnimalKey", "Phase", "Period", "Count", "Hours")], as.data.frame(s$animals)[c("AnimalKey", "Phase", "Period", "Count", "Hours")],
              by = c("AnimalKey", "Phase", "Period"))
check(nrow(s1$animals) == nrow(s$animals) - 8L && nrow(kept) == nrow(s1$animals) && identical(kept$Count.x, kept$Count.y) &&
        !any(s1$animals$CageID == B2E & s1$animals$Phase == "Active") && sum(s1$animals$CageID == B2E & s1$animals$Phase == "Inactive") == 8L,
      "5: only the active cells of B2|sys.5 leave the 31c/31d input; the counts are unchanged")
check(fails(grid31_stats_data(r$animals), "carries outcome eligibility") && isTRUE(all.equal(grid31_stats_data(r$animals, e0)$roster, ro1)),
      "5: a table with eligibility columns needs the eligibility file; with it the columns are rejoined")
sens <- cc4e_inactive_sensitivity(e0)
s2 <- grid31_stats_data(ap, sens)
check(!rr(s2$roster, B2E, "Inactive")$Included && identical(s2$roster$Included[s2$roster$Phase == "Active"], ro1$Included[ro1$Phase == "Active"]) &&
        identical(e0$status, cc4e_status(legacy$animals, man)$status), "5: the inactive sensitivity drops the flagged inactive episode only")
check(!rr(grid31_stats_data(ap, cc4e_inactive_sensitivity(e_mat))$roster, B2E, "Inactive")$Included &&
        rr(grid31_stats_data(ap, e_mat)$roster, B2E, "Inactive")$Included, "5: retained_material is in the primary roster and out of the inactive sensitivity")

# ------------------------------------------------------------------ 6. the eligibility file
f0 <- fx_run(e0, file.path(tmp, "elig_run"))
rb <- cc4e_read_eligibility(f0)
check(isTRUE(all.equal(as.data.frame(rb), as.data.frame(e0), check.attributes = FALSE)) && identical(attr(rb, "sha256"), sha(f0)),
      "6: the eligibility file reads back verified")
f1 <- fx_run(e0, file.path(tmp, "elig_changed")); cat("\n", file = f1, append = TRUE)
check(fails(cc4e_read_eligibility(f1), "hash mismatch"), "6: a changed eligibility file is refused (hash)")
check(fails(cc4e_read_eligibility(fx_run(data.table::copy(e0)[, rule_version := "cc4e_v0.9"], file.path(tmp, "elig_v09"))), "rule version"),
      "6: another rule version is refused")
f2 <- file.path(tmp, "elsewhere.csv"); invisible(file.copy(f0, f2))
check(fails(cc4e_read_eligibility(f2), "must be <run>/tables"), "6: a file outside an audit run is refused")
check(fails(cc4e_read_eligibility(file.path(tmp, "missing.csv")), "existing"), "6: a missing file is refused")
check(identical(attr(rb, "variant"), "primary") && isTRUE(attr(rb, "release")), "6: the variant and release status are read from the run record")
fdev <- fx_run(e0, file.path(tmp, "elig_dev"), dev = TRUE)
check(fails(cc4e_read_eligibility(fdev), "dev audit run"), "6: a dev audit run's eligibility file is refused")
Sys.setenv(MMM_CC4_ALLOW_DEV_ELIGIBILITY = "TRUE")
rd <- cc4e_read_eligibility(fdev)
check(isFALSE(attr(rd, "release")) && any(grepl("DEV eligibility file", cc4e_protocol_lines(rd), fixed = TRUE)), "6: ... unless explicitly allowed, and then labelled")
Sys.unsetenv("MMM_CC4_ALLOW_DEV_ELIGIBILITY")
fsen <- fx_run(cc4e_inactive_sensitivity(e0), file.path(tmp, "elig_isens"), variant = "inactive_sensitivity")
check(identical(attr(cc4e_read_eligibility(fsen), "variant"), "inactive_sensitivity") &&
        any(grepl("INACTIVE SENSITIVITY", cc4e_protocol_lines(cc4e_read_eligibility(fsen)), fixed = TRUE)),
      "6: the inactive-sensitivity variant is readable by the stages and labelled in their protocol")
kr <- fx_knockout_run(fx_knockout(), file.path(tmp, "ckr_v1.0_rel"))
kd <- fx_knockout_run(fx_knockout(), file.path(tmp, "dev_ckr"), dev = TRUE)
kg <- fx_knockout_run(fx_knockout(), file.path(tmp, "ckr_v1.0_gate"), gate = FALSE)
ks <- fx_knockout_run(fx_knockout(), file.path(tmp, "ckr_v1.0_sub"), recordings = "B1_CC4")
check(isTRUE(attr(cc4e_read_knockout(kr, release = TRUE), "release")) && identical(attr(cc4e_read_knockout(kr), "run_id"), "ckr_v1.0_rel"),
      "6: a release knockout run is accepted, with its run id")
check(fails(cc4e_read_knockout(kd, release = TRUE), "release CC4 knockout") && fails(cc4e_read_knockout(kg, release = TRUE), "release CC4 knockout") &&
        fails(cc4e_read_knockout(ks, release = TRUE), "release CC4 knockout") && isFALSE(attr(cc4e_read_knockout(kd), "release")),
      "6: a dev, failed-gate or subset knockout is refused for a release eligibility run and accepted (as not a release) for a dev one")
kv <- fx_knockout_run(fx_knockout(), file.path(tmp, "ckr_v0.9"), version = "ckr_v0.9")
check(fails(cc4e_read_knockout(kv), "Not a ckr_v1.0 run"), "6: another knockout version is refused")
e_bad <- data.table::copy(e0)[CageID == "B1|sys.2|CC4" & PhaseLabel == "I3", `:=`(status = "incomplete_span", primary_use = "missing")]
check(fails(cc4e_join(ap, e_bad), "another phase table") && fails(cc4e_join(ap, e0[AnimalNum != "F101"]), "does not match"),
      "6: an eligibility table built from another phase table is refused")

# ------------------------------------------------------------------ 7. the drivers
froot <- file.path(tmp, "RFID"); dir.create(froot)
op <- options(mmm.project_root = froot)   # a sandbox project root: the isolation check never consults the lab drive's root
Sys.setenv(MMM_CC4_OUTCOME_ELIGIBILITY = f0)
out_e <- file.path(tmp, "with_rule")
invisible(run_cc4_phase_groups(src, file.path(tmp, "outcomes.csv"), out_e))
w <- fx_read_phases(file.path(out_e, "animal_phase_activity.csv"))
check(identical(utils::tail(names(w), 4), CC4E_COLUMNS) && identical(utils::head(names(w), -4), names(ap)) && nrow(w) == nrow(ap),
      "7: 31b with the env var: same rows, eligibility columns last")
check(setequal(list.files(out_e), list.files(file.path(tmp, "legacy"))) && identical(sha(file.path(out_e, "canonical_outcomes.csv")), PINS_31B[["canonical_outcomes.csv"]]),
      "7: 31b writes the same files")
ime <- utils::read.csv(file.path(out_e, "input_manifest.csv"), colClasses = "character")
rme <- readLines(file.path(out_e, "README.txt"))
check(any(ime$Path == f0) && any(grepl("cc4_outcome_eligibility.R$", ime$Path)) && any(grepl("output_files_sha256.csv$", ime$Path)) &&
        any(grepl("Outcome eligibility cc4e_v1.0", rme, fixed = TRUE)) && any(grepl("roster per phase family", rme, fixed = TRUE)),
      "7: the eligibility file, its run manifest and the rule code are hashed inputs; the README states the rule")
check(fails(run_cc4_phase_groups(src, file.path(tmp, "outcomes.csv"), file.path(froot, "b")), "Refusing") && !dir.exists(file.path(froot, "b")),
      "7: 31b with the rule refuses an output inside the project root")
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  check(fails(run_cc4_statistics("missing", file.path(froot, "c")), "Refusing") &&
          fails(run_cc4_phenotype_statistics("missing", file.path(froot, "d")), "Refusing"), "7: 31c and 31d with the rule refuse a non-isolated root")
  Sys.setenv(MMM_CC4_OUTCOME_ELIGIBILITY = f1)
  check(fails(run_cc4_statistics("missing", file.path(tmp, "c")), "hash mismatch") && !dir.exists(file.path(tmp, "c")) &&
          fails(run_cc4_phenotype_statistics("missing", file.path(tmp, "d")), "hash mismatch") && !dir.exists(file.path(tmp, "d")),
        "7: 31c and 31d refuse a changed eligibility file before any fit and write nothing")
} else cat("  (glmmTMB not installed: the 31c/31d driver refusals are not exercised)\n")
options(op)
Sys.unsetenv("MMM_CC4_OUTCOME_ELIGIBILITY")
check(fails(run_cc4_statistics("missing", "new", draws = 100), "4999"), "7: without the env var 31c's own argument checks come first")

# ------------------------------------------------------------------ 8. sample sizes, missing vs excluded, affected, cross-CC
pp <- cc4e_population(data.frame(Sex = "Male", family = "Active", Group = c("CON", "RES", "SUS", "RES"), AnimalKey = c("a", "b", "c", "d"),
                                 CageID = c("c1", "c2", "c2", "c3"), Batch = c("B1", "B1", "B1", "B2")))
check(identical(as.integer(unlist(pp[Group == "all", .(Animals, Cages, CONCages, Batches, MixedCages)])), c(4L, 3L, 1L, 2L, 1L)) &&
        identical(as.integer(unlist(pp[Group == "RES", .(Animals, Cages, CONCages, Batches, MixedCages)])), c(2L, 2L, 0L, 2L, 1L)) &&
        pp[Group == "CON"]$MixedCages == 0L, "8: cc4e_population by hand")
ro <- cc4ea_rosters(ap, e0, sens)
sz <- cc4ea_sample_sizes(ro)
g <- function(st, fam, grp) sz[Stage == st & family == fam & Group == grp]
# 26 animals in 8 cages; frozen: B5|sys.1 out (22 animals, 7 cages: 3 CON, 4 mixed RES/SUS); primary active: also
# B2|sys.5 out (18, 6, 3 mixed); inactive sensitivity: B2|sys.5 out of the inactive family too.
x <- g("31c", "Active", "all")
check(x$Animals_frozen == 22L && x$Animals_primary == 18L && x$Animals_sensitivity == 18L && x$Cages_frozen == 7L && x$Cages_primary == 6L &&
        x$CONCages_primary == 3L && x$Batches_primary == 3L && x$MixedCages_frozen == 4L && x$MixedCages_primary == 3L && x$Animals_primary_minus_frozen == -4L,
      "8: 31c Male Active: 22 -> 18 animals, 7 -> 6 cages, 4 -> 3 mixed cages")
x <- g("31c", "Inactive", "all")
check(x$Animals_frozen == 22L && x$Animals_primary == 22L && x$Animals_sensitivity == 18L && x$Cages_sensitivity == 6L, "8: 31c Male Inactive unchanged; sensitivity 18/6")
check(g("31c", "Active", "RES")$Animals_frozen == 8L && g("31c", "Active", "RES")$Animals_primary == 5L && g("31c", "Active", "RES")$Cages_primary == 3L &&
        g("31c", "Active", "SUS")$Animals_frozen == 4L && g("31c", "Active", "SUS")$Animals_primary == 3L && g("31c", "Active", "CON")$Animals_primary == 10L,
      "8: 31c Male Active per group")
check(g("31d", "Active", "all")$Animals_primary == 18L && g("31b", "Active", "all")$Animals_frozen == 22L && g("31b", "Active", "all")$Animals_primary == 18L &&
        g("31b", "Inactive", "all")$Animals_primary == 22L, "8: 31b and 31d rosters")
check(g("31", "Inactive", "all")$Animals_frozen == 22L && g("31", "Inactive", "all")$Animals_primary == 22L && g("31", "Inactive", "all")$Animals_sensitivity == 18L &&
        !nrow(sz[Stage == "31" & family == "Active"]), "8: Stage 31 whole-inactive cohort; no active cohort")
check(all(sz$check_frozen == "pass" & sz$check_primary == "pass" & sz$check_sensitivity == "pass"), "8: every stage gate passes on every roster")
check(isTRUE(all.equal(as.data.frame(ro$runs$frozen$b$population), as.data.frame(legacy$population))) &&
        isTRUE(all.equal(ro$runs$frozen$c$roster, s$roster)), "8: the audit's frozen rosters are the stages' own")
mve <- cc4ea_missing_vs_excluded(e0, ro, length(unique(ap$AnimalKey)))
cat_n <- function(fam, cat) mve[row_type == "animals_in_31c_31d_families" & family == fam & category == cat]$animals
check(mve[category == "animals removed from the roster"]$animals == 0L && mve[grepl("no CC4 model family", category)]$animals == 4L &&
        mve[category == "animals in the roster (never removed by an episode outcome)"]$animals == 26L,
      "8: no animal leaves the roster; the four B5|sys.1 animals contribute to no family")
check(cat_n("Active", "not in the model: family excluded (hardware); in the all-episode sensitivity") == 4L &&
        cat_n("Active", "not in the model: excluded (hardware) and censored or incomplete cells") == 4L &&
        cat_n("Inactive", "not in the model: censored or incomplete cells (missing)") == 4L && cat_n("Inactive", "in the primary 31c/31d model") == 22L,
      "8: missing (censored) is distinguished from excluded (hardware)")
af <- cc4ea_affected(e0, ro, man)
ac <- function(st, fam, ep) af[Stage == st & family == fam & CageEpisodeID == ep]
check(grepl("^excluded from the primary", ac("31c", "Active", B2E)$consequence) && ac("31c", "Active", B2E)$frozen_roster == "in (4 animals)" &&
        ac("31c", "Active", B2E)$primary_roster == "out" && grepl("^retained", ac("31c", "Inactive", B2E)$consequence) &&
        ac("31c", "Inactive", B2E)$inactive_sensitivity_roster == "out" && grepl("^unaffected", ac("31", "QC", B2E)$consequence) &&
        grepl("excluded \\(hardware\\) and censored", ac("31b", "Active", B5E)$consequence) && any(grepl("^unaffected: Stage 31-31d read CC4 only", af$consequence)),
      "8: affected analyses per output and episode")
srs <- data.table::data.table(AnimalNum = c("OR106", "OR106", "314"), Batch = c("B2", "B2", "B5"), CC = c("CC3", "CC4", "CC4"), board = c(2L, 5L, 1L),
                              System = c("sys.2", "sys.5", "sys.1"), CageEpisodeID = c("B2|sys.2|CC3", B2E, B5E), spatial_excluded = TRUE,
                              movement_excluded = TRUE, censor_from = c("Inf", "Inf", "1701698442.962"))
cx <- cc4ea_cross_cc(srs, man, e0)
check(grepl("^not read by Stage 31-31d; its own records \\(SX01;SX05\\)", cx[CC == "CC3"]$cc4e_effect) &&
        grepl("Active excluded_hardware: A1,A2,A3,A4,A5", cx[AnimalNum == "OR106" & CC == "CC4"]$cc4e_effect, fixed = TRUE) &&
        cx[AnimalNum == "OR106" & CC == "CC4"]$manifest_records == "SX02;SX06", "8: cross-CC episodes: the CC3 episode is not read; CC4 carries the rule")
srs_bad <- data.table::copy(srs)[CC == "CC4" & AnimalNum == "OR106", CageEpisodeID := "B2|sys.4|CC4"]
check(fails(cc4ea_cross_cc(srs_bad, man, e0), "differs from its Stage 31b CageID"), "8: the srs CC4 episode must equal the 31b CageID")

# ------------------------------------------------------------------ 9. separation
for (f in c("Analysis/31b_cc4_phase_groups.R", "Analysis/31c_cc4_phase_statistics.R", "Analysis/31d_cc4_phenotype_statistics.R")) {
  txt <- readLines(f, warn = FALSE)
  check(!any(grepl("spatial_exclusion_manifest|spatial_analysis_exclusions|sxm_read", txt)) && any(grepl("MMM_CC4_OUTCOME_ELIGIBILITY", txt, fixed = TRUE)),
        paste("9: reads the eligibility file only, never the manifest:", f))
}
check(any(grepl("Functions/cc4_outcome_eligibility.R", readLines("Testing/tests/test_spatial_exclusion_manifest.R", warn = FALSE), fixed = TRUE)),
      "9: the helper is on the manifest test's reader allowlist (which still bans the registered stages)")

# ------------------------------------------------------------------ 10. the audit's protection guard
main <- paste(deparse(Filter(function(e) is.call(e) && identical(e[[1]], as.name("if")),
                             as.list(parse("Testing/audits/audit_cc4_outcome_eligibility.R", keep.source = FALSE)))[[1]]), collapse = "\n")
pos <- function(p) regexpr(p, main, fixed = TRUE)[1]
check(pos("mmm_isolated_output_root_check") > 0 && pos("mmm_isolated_output_root_check") < pos("mmm_protection_guard_begin") &&
        pos("mmm_protection_guard_begin") < pos("dir.create") && pos("mmm_protection_guard_begin") < pos("fwrite") &&
        pos("mmm_protection_guard_end") > pos("README.txt") && pos("mmm_protection_guard_end") < pos("outs <- list.files(tmp") &&
        pos("mmm_protection_guard_end") < pos("file.rename"),
      "10: the protection guard starts before the first write and ends before the output hashes and the rename")
unlink(tmp, recursive = TRUE)

cat("test_cc4_outcome_eligibility: all checks passed\n")
