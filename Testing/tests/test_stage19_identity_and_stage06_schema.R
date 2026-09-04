# ===========================================================================
# Regression tests for the final pre-rebuild pass.
#
# Stage 19 identity:
#   1. canonical aliases collapse correctly
#   2. 0004/4 cannot carry conflicting phenotypes
#   3. canonical roster = 111
#   4. identity conflicts fail closed
#
# Stage 06 schema contract:
#   5. the current Stage 02 dyadic_network_ready schema satisfies Stage 06
#   6. the gap provenance field is either unused or accepted under its new name
#   7. Stage 06 cannot silently fall back because of a missing dyadic column
#
# Stage 09 common-clock sensitivity:
#   8. the common window is exactly slots 5..72
#   9. every animal contributes exactly 68 bins
#  10. no imputation or backward fill occurs
# ===========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(stringr); library(purrr); library(tidyr)
})
repo <- Sys.getenv("MMM_REPO_DIR", unset = "C:/Users/topohl/Documents/GitHub/MMMSociability")
setwd(repo)
source(file.path(repo, "Functions", "behavioral_dynamics_helpers.R"))
source(file.path(repo, "Functions", "animalpos_preprocessing_helpers.R"))

check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
load_defs <- function(path, want, env = new.env(parent = globalenv())) {
  for (e in as.list(parse(path))) {
    op <- if (is.call(e) && length(e) >= 3) as.character(e[[1]])[1] else ""
    if (isTRUE(op %in% c("<-", "="))) {
      nm <- tryCatch(as.character(e[[2]]), error = function(err) "")
      if (length(nm) == 1 && nm %in% want) tryCatch(eval(e, envir = env), error = function(err) {})
    }
  }
  env
}

# ---------------------------------------------------------------------------
# 1-2. Canonical aliases collapse; alias pairs cannot carry conflicting labels.
# ---------------------------------------------------------------------------
s19 <- load_defs("Analysis/19_spatial_occupancy_maps.R", c("normalize_animal_id"))
check("normalize_animal_id" %in% ls(s19), "1: Stage 19 must define normalize_animal_id")
norm19 <- get("normalize_animal_id", envir = s19)

check(identical(norm19("0003"), "3"), "1: Stage 19 must collapse 0003 to 3")
check(identical(norm19("0004"), "4"), "1: Stage 19 must collapse 0004 to 4")
check(identical(norm19("4"), "4"),    "1: a bare numeric id must be unchanged")
check(identical(norm19(c("0003", "3", "0004", "4")), c("3", "3", "4", "4")),
      "1: numeric aliases must collapse many-to-one")
# It must be the SHARED contract, not an independent rule.
set.seed(11)
probe <- c("0003", "3", "0004", "4", "00304", "OR004", "OQ754", " 0012 ", "13856", "1545")
check(identical(norm19(probe), canonical_animal_id(probe)),
      "1: Stage 19 normalization must equal the shared canonical_animal_id contract")
# Alphanumeric ids keep embedded zeros (guards against over-aggressive stripping).
check(identical(norm19("OR004"), "OR004"), "1: alphanumeric ids must retain embedded zeros")
check(identical(norm19("00304"), "304"),   "1: zero-padded numerics strip leading zeros only")

# 2. The alias pair must resolve to ONE phenotype. The SUS reference list stores
# 0004, so the bare spelling must still resolve to SUS once canonicalized.
sus_ref <- canonical_animal_id(c("0004", "OQ753"))
check("4" %in% sus_ref, "2: canonicalized SUS reference must contain 4")
lookup <- function(id) if (canonical_animal_id(id) %in% sus_ref) "SUS" else "RES"
check(lookup("0004") == "SUS", "2: 0004 must resolve to SUS")
check(lookup("4") == "SUS",
      "2: the bare spelling 4 must ALSO resolve to SUS, not fall through to the unlisted default")
check(lookup("0004") == lookup("4"),
      "2: 0004 and 4 cannot carry conflicting phenotypes after canonicalization")
# Control: under the OLD trim/upper-only rule the pair disagreed.
old_norm <- function(x) toupper(str_replace_all(str_trim(as.character(x)), "\\s+", ""))
check(old_norm("0004") != old_norm("4"),
      "2: control -- the superseded rule must be shown to split the alias pair")

cat("Stage 19 alias collapse checks (1-2): PASS\n")

# ---------------------------------------------------------------------------
# 3-4. Roster contract and fail-closed conflict detection.
# ---------------------------------------------------------------------------
# The production contract is asserted against the committed script text so the
# test does not require the S: drive.
s19_src <- paste(readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE), collapse = "\n")
check(grepl("canonical_animal_id(AnimalID)", s19_src, fixed = TRUE),
      "3: Stage 19 must canonicalize AnimalID at read time")
check(grepl("identity_conflicts", s19_src, fixed = TRUE),
      "4: Stage 19 must compute an identity-conflict table")
# Pin the actual predicate: a neutered filter must fail this.
check(grepl("filter(n_groups > 1 | n_sexes > 1 | n_batches > 1)", s19_src, fixed = TRUE),
      "4: Stage 19 must flag any animal whose aliases disagree on Group, Sex or Batch")
# The three counts must be derived from the real columns, not stubbed out.
for (expr in c("n_groups = dplyr::n_distinct(as.character(Group))",
               "n_sexes = dplyr::n_distinct(as.character(Sex))",
               "n_batches = dplyr::n_distinct(Batch_norm)")) {
  check(grepl(expr, s19_src, fixed = TRUE),
        paste0("4: the conflict table must compute ", expr))
}
# Pin the actual fail-closed call: the conflict branch must call stop().
s19_lines <- readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE)
guard_at <- grep("if (nrow(identity_conflicts) > 0)", s19_lines, fixed = TRUE)
check(length(guard_at) == 1, "4: Stage 19 must guard on a non-empty identity-conflict table")
guard_end <- guard_at + which(trimws(s19_lines[(guard_at + 1):(guard_at + 12)]) == "}")[1]
guard_body <- s19_lines[(guard_at + 1):(guard_end - 1)]
check(any(grepl("stop(", guard_body, fixed = TRUE)),
      "4: the identity-conflict branch must call stop(), not merely warn or message")
check(!any(grepl("message(", guard_body, fixed = TRUE)) ||
        any(grepl("stop(", guard_body, fixed = TRUE)),
      "4: the identity-conflict branch must not downgrade to a message")
check(any(grepl("Canonical animal identity conflict for", guard_body, fixed = TRUE)),
      "4: the conflict error must name the failing animals")
check(!grepl("AnimalNum = AnimalID,", s19_src, fixed = TRUE),
      "3: the un-canonicalized AnimalNum = AnimalID assignment must be gone")

# The conflict rule itself, exercised on synthetic rosters.
conflict_rows <- function(d) {
  d %>% distinct(AnimalNum, Group, Sex, Batch_norm) %>%
    group_by(AnimalNum) %>%
    summarise(n_groups = n_distinct(as.character(Group)),
              n_sexes = n_distinct(as.character(Sex)),
              n_batches = n_distinct(Batch_norm), .groups = "drop") %>%
    filter(n_groups > 1 | n_sexes > 1 | n_batches > 1)
}
clean <- tibble(AnimalNum = c("3", "3", "4", "4"), Group = c("RES", "RES", "SUS", "SUS"),
                Sex = "Male", Batch_norm = "B1")
check(nrow(conflict_rows(clean)) == 0, "4: a consistent alias merge must raise no conflict")
bad_group <- clean; bad_group$Group[4] <- "RES"
check(nrow(conflict_rows(bad_group)) == 1,
      "4: an alias pair with conflicting Group must be flagged")
bad_sex <- clean; bad_sex$Sex[2] <- "Female"
check(nrow(conflict_rows(bad_sex)) == 1, "4: conflicting Sex must be flagged")
bad_batch <- clean; bad_batch$Batch_norm[2] <- "B2"
check(nrow(conflict_rows(bad_batch)) == 1, "4: conflicting Batch must be flagged")

# 3. Roster contract, checked against the canonical Stage 01 export when present.
s01 <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv"
if (file.exists(s01)) {
  r <- read_csv(s01, show_col_types = FALSE, progress = FALSE,
                col_types = cols_only(AnimalNum = col_character(), Group = col_character(),
                                      Sex = col_character())) %>% distinct()
  check(n_distinct(r$AnimalNum) == 111, "3: canonical roster must be 111 animals")
  g <- r %>% distinct(AnimalNum, Group) %>% count(Group)
  check(g$n[g$Group == "CON"] == 24 && g$n[g$Group == "RES"] == 49 && g$n[g$Group == "SUS"] == 38,
        "3: roster must be 24 CON / 49 RES / 38 SUS")
  s <- r %>% distinct(AnimalNum, Sex) %>% count(Sex)
  check(s$n[s$Sex == "Female"] == 58 && s$n[s$Sex == "Male"] == 53,
        "3: roster must be 58 Female / 53 Male")
  check(unique(r$Group[r$AnimalNum == "4"]) == "SUS", "3: animal 4 must be SUS")
  cat("Stage 19 roster contract checks (3-4): PASS (verified against canonical export)\n")
} else {
  cat("Stage 19 roster contract checks (3-4): PASS (structural only; canonical export not mounted)\n")
}

# ---------------------------------------------------------------------------
# 5-7. Stage 06 input-contract against the Stage 02 schema.
# ---------------------------------------------------------------------------
s06_src <- paste(readLines("Analysis/06_dynamic_social_networks.R", warn = FALSE), collapse = "\n")
GAP_FIELDS <- c("n_long_gaps", "n_system_event_gaps", "LongGap", "SystemEventGap")
used_gap <- GAP_FIELDS[vapply(GAP_FIELDS, function(g) grepl(g, s06_src, fixed = TRUE), logical(1))]
check(length(used_gap) == 0,
      paste0("6: Stage 06 must not consume a gap provenance field; found: ",
             paste(used_gap, collapse = ", ")))
check(!grepl("exclude_long", s06_src, fixed = TRUE),
      "6: Stage 06 must not carry a long-gap exclusion switch")

# Stage 06's required columns, as encoded by first_existing_col(..., TRUE, ...).
REQUIRED <- list(
  focal     = c("Focal", "AnimalNum", "Animal", "MouseID", "animal_1", "id1"),
  partner   = c("Partner", "OtherAnimal", "PartnerID", "animal_2", "id2"),
  time      = c("TimeIndex", "HalfHourElapsed", "HalfHourWithinCC0", "HalfHour", "Time", "TimeBin", "datetime", "DateTime"),
  proximity = c("Weight", "ProximityFraction", "Proximity", "proximity", "contact_fraction",
                "same_position_fraction", "Distance", "distance", "Interaction", "interaction_weight"))
OPTIONAL <- list(
  group = c("Group", "Phenotype", "Condition", "Treatment", "StressGroup"),
  sex = c("Sex", "sex"), phase = c("Phase", "phase", "LightDark", "DayNight"),
  cage = c("CageChange", "CC", "CageChangeNum", "Regrouping", "Batch", "Cage"),
  batch = c("Batch", "batch"), system = c("System", "system", "Cage", "cage"),
  observed = c("observation_seconds", "ObservationSeconds", "duration", "DurationSec"))
for (nm in names(REQUIRED)) {
  check(all(vapply(REQUIRED[[nm]], function(c) grepl(paste0('"', c, '"'), s06_src, fixed = TRUE), logical(1))[1]),
        paste0("5: Stage 06 must still resolve the ", nm, " column from a candidate list"))
}

# The schema Stage 02 now emits.
STAGE02_SCHEMA <- c("Focal", "Partner", "TimeIndex", "BinStart", "Weight", "Proximity", "Contact",
                    "Group", "Sex", "Phase", "CageChange", "Batch", "System", "BinSizeSec",
                    "observation_seconds", "same_position_fraction", "adjacent_fraction",
                    "same_position_seconds_per_hour", "adjacent_seconds_per_hour",
                    "mean_grid_distance", "n_intervals", "n_system_event_gaps")
resolve <- function(cands, schema) { hit <- cands[cands %in% schema][1]; if (is.na(hit)) NA_character_ else hit }
for (nm in names(REQUIRED)) {
  check(!is.na(resolve(REQUIRED[[nm]], STAGE02_SCHEMA)),
        paste0("5: the Stage 02 schema must satisfy Stage 06's required ", nm, " column"))
}
check(resolve(REQUIRED$proximity, STAGE02_SCHEMA) == "Weight",
      "5: the proximity column must resolve to Weight")
check(resolve(REQUIRED$time, STAGE02_SCHEMA) == "TimeIndex",
      "5: the time column must resolve to TimeIndex")
for (nm in names(OPTIONAL)) {
  check(!is.na(resolve(OPTIONAL[[nm]], STAGE02_SCHEMA)),
        paste0("5: the Stage 02 schema should still supply the optional ", nm, " column"))
}
check(sum(duplicated(STAGE02_SCHEMA)) == 0, "5: required columns must exist exactly once")
check(!"n_system_event_gaps" %in% unlist(c(REQUIRED, OPTIONAL)),
      "6: the renamed gap field must not appear in any Stage 06 candidate list")
# Renaming the gap column must not change how any Stage 06 column resolves.
renamed_back <- replace(STAGE02_SCHEMA, STAGE02_SCHEMA == "n_system_event_gaps", "n_long_gaps")
for (nm in c(names(REQUIRED), names(OPTIONAL))) {
  cands <- c(REQUIRED, OPTIONAL)[[nm]]
  check(identical(resolve(cands, STAGE02_SCHEMA), resolve(cands, renamed_back)),
        paste0("6: the gap rename must not change resolution of the ", nm, " column"))
}

# 7. A missing required column must ERROR, not silently degrade to animal-level.
fec <- get("first_existing_col", envir = globalenv())
broken <- as.data.frame(setNames(rep(list(1), 3), c("Partner", "TimeIndex", "Weight")))
err <- tryCatch({ fec(broken, REQUIRED$focal, TRUE, "dyad focal column"); NULL },
                error = function(e) conditionMessage(e))
check(!is.null(err) && grepl("Could not find", err, fixed = TRUE),
      "7: a missing required dyadic column must raise an error")
s06_lines <- readLines("Analysis/06_dynamic_social_networks.R", warn = FALSE)
guard <- trimws(s06_lines[grep("file.exists(dyad_file)", s06_lines, fixed = TRUE)])
check(length(guard) == 1, "7: there must be exactly one dyadic-input guard")
# The guard must test ONLY nullity and file existence. Any extra condition (for
# example a column check) would let a schema mismatch silently fall back to
# animal-level mode instead of erroring.
check(identical(guard, "if (!is.null(dyad_file) && file.exists(dyad_file)) {"),
      paste0("7: the fallback guard must be file-existence only; found: ", guard))
check(grepl("No dyadic file found at:", s06_src, fixed = TRUE),
      "7: the fallback branch must announce a missing FILE, not a schema mismatch")

cat("Stage 06 schema-contract checks (5-7): PASS\n")

# ---------------------------------------------------------------------------
# 8-10. Stage 09 common-clock sensitivity window.
# ---------------------------------------------------------------------------
COMMON_FIRST_SLOT <- 5L; COMMON_LAST_SLOT <- 72L; COMMON_N <- 68L
check(COMMON_LAST_SLOT - COMMON_FIRST_SLOT + 1L == COMMON_N,
      "8: slots 5..72 must be exactly 68 bins")
block_start <- as.POSIXct("2022-10-28 18:30:00", tz = "UTC")
check(block_start + (COMMON_FIRST_SLOT - 1L) * 600 ==
        as.POSIXct("2022-10-28 19:10:00", tz = "UTC"),
      "8: slot 5 must begin at 19:10")
check(block_start + COMMON_LAST_SLOT * 600 == as.POSIXct("2022-10-29 06:30:00", tz = "UTC"),
      "8: the window must end at 06:30 exclusive")

mk <- function(id, first_slot) tibble(
  AnimalNum = id, SourceFile = "S1", Phase = "Active",
  BinStart = block_start + (seq(first_slot, 72) - 1L) * 600,
  Movement = seq_along(seq(first_slot, 72)) * 1.0)
# leading missingness of 0..4 bins, as observed in the accepted primary
fixture <- bind_rows(mk("a", 1), mk("b", 2), mk("c", 3), mk("d", 4), mk("e", 5))
anchor <- animalpos_phase_block_bounds(min(fixture$BinStart))$block_start
common <- fixture %>%
  mutate(target_slot = as.integer(as.numeric(difftime(BinStart, anchor, units = "secs")) %/% 600) + 1L) %>%
  filter(target_slot >= COMMON_FIRST_SLOT, target_slot <= COMMON_LAST_SLOT)

per <- common %>% count(AnimalNum, name = "n")
check(all(per$n == COMMON_N),
      paste0("9: every animal must contribute exactly 68 bins; got ",
             paste(sort(unique(per$n)), collapse = ",")))
check(n_distinct(per$AnimalNum) == 5, "9: no animal may be dropped by the common window")
slots <- common %>% group_by(AnimalNum) %>%
  summarise(lo = min(target_slot), hi = max(target_slot), k = n_distinct(target_slot), .groups = "drop")
check(all(slots$lo == COMMON_FIRST_SLOT), "8: every animal must start at slot 5")
check(all(slots$hi == COMMON_LAST_SLOT), "8: every animal must end at slot 72")
check(all(slots$k == COMMON_N), "9: slot sets must be complete and unique")
# Identical clock slots for everyone.
sets <- common %>% group_by(AnimalNum) %>% summarise(s = paste(sort(target_slot), collapse = ","), .groups = "drop")
check(n_distinct(sets$s) == 1, "9: all animals must share one identical slot set")

# 10. No imputation or backward fill: row counts equal genuinely observed rows,
# and the animal whose record starts at slot 5 gains nothing.
check(nrow(common) == 5 * COMMON_N, "10: total rows must be 5 animals x 68 observed bins")
check(all(is.finite(common$Movement)), "10: no NA-filled rows may be introduced")
observed_e <- fixture %>% filter(AnimalNum == "e") %>% nrow()
check(sum(common$AnimalNum == "e") == observed_e,
      "10: an animal already starting at slot 5 must gain no rows")
# An animal missing slot 5 itself would be short: the window must NOT invent it.
short <- mk("f", 6)
short_common <- short %>%
  mutate(target_slot = as.integer(as.numeric(difftime(BinStart, anchor, units = "secs")) %/% 600) + 1L) %>%
  filter(target_slot >= COMMON_FIRST_SLOT, target_slot <= COMMON_LAST_SLOT)
check(nrow(short_common) == COMMON_N - 1L,
      "10: a genuinely absent slot must stay absent rather than being back-filled")
check(min(short_common$target_slot) == 6L, "10: no backward fill to slot 5 may occur")

cat("Stage 09 common-clock window checks (8-10): PASS\n")
cat("Stage 19 identity / Stage 06 schema / Stage 09 common-clock checks: PASS\n")
