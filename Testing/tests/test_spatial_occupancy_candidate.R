# ===========================================================================
# Stage 19 candidate s19c (Functions/spatial_occupancy_candidate.R, Testing/audits/build_spatial_occupancy_candidate.R).
# Portable: a synthetic srs release (two recordings, five animals) built in memory, fixtures under tempdir(); nothing on
# the lab drive is read or written, and the builder is sourced for its definitions only. Expected values are worked
# out by hand.
#
#  1. column mapping (C1 = R1 + R5 ... C4 = R4 + R8) and the resource labels (R1 food, R5 house, R8 water); sex from batch
#  2. zero-completion to 4 columns / 8 readers plus unknown for every animal-block x basis x bound; the shares sum to 1
#  3. hand-worked shares: unknown = observed - supported; LOCF unknown = 0 when seeded; unseeded time is unknown under LOCF
#  4. censoring: observed time stops at the censoring time; no observed time gives NA shares, not 0; no support gives NA
#     shares of supported time
#  5. excluded cage episodes are flagged in the animal tables and absent from every summary and figure input
#  6. windows pool seconds (not shares); stage19_window keeps CC4 A1, I2, A2; Light = vendor Inactive
#  7. parity with a Stage-19-format fixture; a changed Stage 19 cell, a changed srs pooled share or a changed exclusion
#     flag fails its gate
#  8. a changed input sha256 is refused; an existing run or build folder is refused; the srs run record gate
#  9. static checks: no model, test statistic or group file in the builder or helpers; no Functions/rfid_*.R name; the
#     helpers never name the exclusion manifest; no Analysis/ file references the candidate; the protection guard
#     brackets every write; Stage 19 and presence inputs are read without group columns
# 10. output roots: the live Stage 19 and publication folders are refused, an outside scratch root is accepted
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "spatial_occupancy_candidate.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
data.table::setDTthreads(1L)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1, check.attributes = FALSE))
refused <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
MMM_REPO_ROOT <- repo
for (f in c("project_paths.R", "antenna_log_helpers.R", "antenna_measurement.R", "antenna_spatial_reconstruction.R", "spatial_occupancy_candidate.R",
            "qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R")) source(file.path("Functions", f))
source("Testing/audits/build_spatial_occupancy_candidate.R")   # definitions only (main-guarded)

# ---- a synthetic srs release ------------------------------------------------------------------------------------------
# R1: B3 CC1 (female), blocks 100 A1, 101 I2; board 1 = OR1, OR2 (OR2 unseeded in block 100: 3,200 s before its first
#     read); board 2 = OR3 (spatially excluded cage episode).
# R2: B5 CC4 (male), blocks 200 A1, 201 I2, 202 A2, 203 I3, 204 A3 (203, 204 outside Stage 19's CC4 crop); board 1 =
#     OR4, OR5, censored from 10,000 s into block 203 (block 204 fully censored).
F1 <- "E9_SIS_B3_CC1_AnimalPos_preprocessed.csv"; F2 <- "E9_SIS_B5_CC4_AnimalPos_preprocessed.csv"
make_fixture <- function() {
  H <- 43200
  blocks <- rbind(
    data.table(SourceFile = F1, Batch = "B3", CC = "CC1", block = 100:101, bs = 1e6 + c(0, H), Phase = c("Active", "Inactive"), PhaseNumber = c(1L, 2L), block_no = 1:2),
    data.table(SourceFile = F2, Batch = "B5", CC = "CC4", block = 200:204, bs = 2e6 + H * (0:4), Phase = c("Active", "Inactive", "Active", "Inactive", "Active"),
               PhaseNumber = c(1L, 2L, 2L, 3L, 3L), block_no = 1:5))
  blocks[, `:=`(be = bs + H, stage19_window = !(CC == "CC4" & PhaseNumber > 2L))]
  setcolorder(blocks, c("SourceFile", "Batch", "CC", "block", "bs", "be", "Phase", "PhaseNumber", "block_no", "stage19_window"))
  cfrom <- 2e6 + 3 * H + 10000
  animals <- data.table(AnimalNum = c("OR1", "OR2", "OR3", "OR4", "OR5"), SourceFile = c(F1, F1, F1, F2, F2), Batch = c("B3", "B3", "B3", "B5", "B5"),
                        CC = c("CC1", "CC1", "CC1", "CC4", "CC4"), board = c(1L, 1L, 2L, 1L, 1L))
  animals[, `:=`(System = paste0("sys.", board), CageEpisodeID = paste(Batch, paste0("sys.", board), CC, sep = "|"), tag = paste0("T", 1:5),
                 spatial_excluded = AnimalNum == "OR3", movement_excluded = AnimalNum == "OR3", censor_from = ifelse(SourceFile == F2, cfrom, Inf),
                 read_seeded = AnimalNum != "OR2", read_seed_age_s = 12.5, event_seeded = AnimalNum != "OR2", event_seed_age_s = 13,
                 tie_observations_in_span = 2L, observations = 5000L)]
  # occupancy: block, basis, PositionID, seconds, sup_60, sup_300 (sup_2, sup_10 = half of sup_60)
  o <- function(id, blk, basis, pid, s, s60, s300) data.table(AnimalNum = id, block = blk, basis = basis, PositionID = pid, seconds = s, sup_60 = s60, sup_300 = s300)
  both <- function(id, blk, pid, s, s60, s300) rbind(o(id, blk, "event", pid, s, s60, s300), o(id, blk, "read", pid, s, s60, s300))
  occ <- rbind(
    o("OR1", 100, "event", c(1L, 5L, 2L), c(20000, 13200, 10000), c(15000, 8000, 9000), c(19000, 12000, 9500)),
    o("OR1", 100, "read", c(1L, 5L, 2L), c(21000, 14200, 8000), c(15500, 8500, 8000), c(19500, 12500, 8500)),
    both("OR1", 101, 1L, H, H, H),
    both("OR2", 100, 5L, 40000, 30000, 38000), both("OR2", 101, 5L, H, 0, 0),
    both("OR3", 100, 3L, H, H, H), both("OR3", 101, 3L, H, H, H),
    both("OR4", 200, 8L, H, H, H), both("OR4", 201, 8L, H, H, H), both("OR4", 202, 8L, H, H, H), both("OR4", 203, 1L, 10000, 10000, 10000),
    o("OR5", 200, "event", 6L, H, H, H), o("OR5", 200, "read", 2L, H, H, H),
    both("OR5", 201, 6L, H, H / 2, H), both("OR5", 202, 6L, H, H, H), both("OR5", 203, 6L, 10000, 10000, 10000))
  occ[, `:=`(sup_2 = sup_60 / 2, sup_10 = sup_60 / 2)]
  key <- merge(animals[, .(AnimalNum, SourceFile, Batch, CC, board, System, CageEpisodeID)],
               blocks[, .(SourceFile, block, block_no, Phase, PhaseNumber, stage19_window, bs, be)], by = "SourceFile", allow.cartesian = TRUE)
  key[, observed_s := pmax(0, pmin(be, ifelse(SourceFile == F2, cfrom, Inf)) - bs)]
  occ <- merge(occ, key[, !c("bs", "be", "observed_s")], by = c("AnimalNum", "block"))
  ab <- key[, .(block, SourceFile, Batch, CC, observed_s, changes_event_reader = 10L, changes_event_column = 8L, changes_read_reader = 9L,
                changes_read_column = 7L, observations = ifelse(observed_s > 0, 1000L, 0L), AnimalNum, board, System, CageEpisodeID, block_no, Phase,
                PhaseNumber, stage19_window)]
  cens <- key[observed_s < be - bs, .(SourceFile, Batch, CC, block, seconds = (be - bs) - observed_s, AnimalNum, board, System, CageEpisodeID)]
  ra <- ab[observed_s > 0, .(block, SourceFile, Batch, CC, observations = 1000L, event_holds_read_reader = 800L, read_holds_read_reader = 950L,
                             event_holds_read_column = 900L, read_holds_read_column = 970L, AnimalNum, board, System, CageEpisodeID, block_no, Phase,
                             PhaseNumber, stage19_window)]
  # gaps: the exact unknown time per animal-block and bound; OR1 block 100 at 60 s split over R5 and R1
  sp <- occ[basis == "read", .(s60 = sum(sup_60), s300 = sum(sup_300), pos = PositionID[which.max(seconds)]), by = .(AnimalNum, block)]
  g <- merge(key[observed_s > 0], sp, by = c("AnimalNum", "block"))
  gaps <- rbind(g[, .(AnimalNum, block, pos, gap_class = "300-1800s", unknown_s = observed_s - s60, bound_s = 60L)],
                g[, .(AnimalNum, block, pos, gap_class = "300-1800s", unknown_s = observed_s - s300, bound_s = 300L)])[unknown_s > 0]
  gaps <- rbind(gaps[!(AnimalNum == "OR1" & block == 100 & bound_s == 60)],
                data.table(AnimalNum = "OR1", block = 100, pos = c(5L, 1L), gap_class = c("300-1800s", "60-300s"), unknown_s = c(8000, 3200), bound_s = 60L))
  gaps <- merge(gaps, key[, !c("bs", "be", "observed_s")], by = c("AnimalNum", "block"))
  colo <- rbind(g[, .(AnimalNum, block, pos, state = "supported", seconds = s60, bound_s = 60L)], g[, .(AnimalNum, block, pos, state = "unknown", seconds = observed_s - s60, bound_s = 60L)],
                g[, .(AnimalNum, block, pos, state = "supported", seconds = s300, bound_s = 300L)], g[, .(AnimalNum, block, pos, state = "unknown", seconds = observed_s - s300, bound_s = 300L)])[seconds > 0]
  colo[, `:=`(mate_same_reader = floor(seconds / 2), mate_same_reader_supported = floor(seconds / 4), mate_supported_anywhere = floor(seconds / 3))]
  colo <- merge(colo, key[, !c("bs", "be", "observed_s")], by = c("AnimalNum", "block"))
  colo[, mates := ifelse(CageEpisodeID == "B3|sys.2|CC1", 0L, 1L)]
  tab <- list(occupancy = occ, animal_block = ab, animals = animals, blocks = blocks, censored = cens, read_agreement = ra, gaps = gaps, colocation = colo)
  # Stage 19 phase table: event-basis LOCF of every reader (zeros included) per animal-phase in its CC4 crop
  e19 <- occ[basis == "event" & stage19_window == TRUE, .(PositionSeconds = sum(seconds)), by = .(SourceFile, Phase, PhaseNumber, AnimalNum, PositionID)]
  aph <- unique(ab[stage19_window == TRUE, .(SourceFile, Phase, PhaseNumber, AnimalNum, observation_seconds = observed_s)])
  s19 <- merge(aph[, .(PositionID = 1:8), by = .(SourceFile, Phase, PhaseNumber, AnimalNum, observation_seconds)], e19,
               by = c("SourceFile", "Phase", "PhaseNumber", "AnimalNum", "PositionID"), all.x = TRUE)
  s19[is.na(PositionSeconds), PositionSeconds := 0]
  # srs pooled reference tables, as the srs builder makes them (excluded episode left out, whole span)
  occ_ok <- occ[CageEpisodeID != "B3|sys.2|CC1"][, column := amv_column(PositionID)]
  ab_ok <- ab[CageEpisodeID != "B3|sys.2|CC1"]
  obs <- ab_ok[, .(observed_s = sum(observed_s)), by = Phase]
  pooled <- function(u) rbindlist(lapply(c("event", "read"), function(b) asr_shares_with_unknown(occ_ok[basis == b], obs, by = "Phase", unit = u)[, basis := b]))
  spp <- merge(ab_ok[, .(SourceFile, AnimalNum, block, Phase, observed_s)],
               occ_ok[basis == "read", .(s60 = sum(sup_60), s300 = sum(sup_300)), by = .(SourceFile, AnimalNum, block)], by = c("SourceFile", "AnimalNum", "block"), all.x = TRUE)
  spp[is.na(s60), `:=`(s60 = 0, s300 = 0)]
  ref <- list(presence = data.table(CageEpisodeID = c("B3|sys.1|CC1", "B3|sys.2|CC1", "B5|sys.1|CC4"), fixed_cage = c(TRUE, FALSE, FALSE),
                                    n_animals_present = c(4L, 4L, 3L), n_tags_present = c(2L, 1L, 2L), n_tracked = c(2L, 1L, 2L)),
              raw_logs = data.table(SourceFile = c(F1, F2), raw_log = c("MultiMouseMonitor-b3cc1.csv", "MultiMouseMonitor-b5cc4.csv"),
                                    raw_logs_ignored = "", clock_fold_events_dropped = c(0L, 5L)),
              stage19 = s19, shares_unknown_column_pooled = pooled("column"), shares_unknown_reader_pooled = pooled("PositionID"),
              support_by_phase = spp[, .(observed_h = sum(observed_s) / 3600, supported_le60_share = sum(s60) / sum(observed_s),
                                         supported_le300_share = sum(s300) / sum(observed_s)), by = Phase],
              parity_stage19 = data.table(rows = nrow(s19), animal_phases = nrow(aph)))
  expected <- list(animal_flags = animals[, .(SourceFile, AnimalNum, spatial_excluded, movement_excluded, censor_from_s = censor_from)],
                   records = data.table(record_id = c("SX01", "SX04", "SX05"), action = c("exclude_spatial", "censor", "exclude_movement"),
                                        CageEpisodeID = c("B3|sys.2|CC1", "B5|sys.1|CC4", "B3|sys.2|CC1"), reader = c("R3", "all", "R3"),
                                        resource = c("none", "all", "none"), from = c("span_start", "censor time", "span_start")),
                   counts = list(animal_recordings = 5L, kept_animal_recordings = 4L, animal_blocks = 16L, observed_animal_blocks = 14L, stage19_cells = nrow(s19)))
  list(tab = tab, ref = ref, expected = expected, meta = list(run_id = "dev_test", srs_run_id = "srs_fixture", reconstruction = "srs_v1.1", tau_s = 2))
}
fx <- make_fixture()
res <- soc_build(fx$tab, fx$ref, fx$meta, fx$expected)
T <- res$tables
g <- res$gates
check(all(g[type == "gate"]$pass), paste("every gate passes on the fixture; failed:", paste(g[type == "gate" & pass == FALSE]$gate_id, collapse = ", ")))
check(all(c("G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G10b", "G11", "G12", "G12b", "G13", "G14", "G15", "G16", "G17", "G18", "G19", "G20", "G21") %in% g$gate_id),
      "the gate table lists every check")

# 1. layout and labels
L <- soc_reader_layout()
check(identical(L$column, amv_column(1:8)) && identical(L[column == 1]$reader, c("R1", "R5")) && identical(L[column == 4]$reader, c("R4", "R8")),
      "1: C1 = R1 + R5, C4 = R4 + R8 (amv_column)")
check(identical(L[nzchar(resource), paste(reader, resource)], c("R1 food", "R5 house", "R8 water")), "1: resources R1 food, R5 house, R8 water")
check(identical(soc_column_layout()$resource_note, c("C1 = R1 food + R5 house", "C2 = R2 + R6", "C3 = R3 + R7", "C4 = R4 + R8 water")), "1: column notes")
check(identical(soc_sex(c("B1", "B2", "B5", "B3", "B4", "B6")), c("Male", "Male", "Male", "Female", "Female", "Female")) && refused(soc_sex("B7")),
      "1: sex from batch; an unknown batch stops")
t1 <- T$occupancy_animal_phase_column
check(all(t1[Batch == "B3"]$Sex == "Female") && all(t1[Batch == "B5"]$Sex == "Male"), "1: Sex column from batch")

# 2. zero-completion and sums
check(nrow(t1) == 16L * 2L * 3L * 5L && nrow(T$occupancy_animal_phase_reader) == 16L * 2L * 3L * 9L, "2: every animal-block x basis x bound x unit, zeros included")
check(all(t1[, .N, by = .(SourceFile, AnimalNum, block, basis, bound)]$N == 5L), "2: four columns plus unknown in every group")
s <- t1[observed_s > 0, .(a = sum(share_all), b = sum(share_supported, na.rm = TRUE), su = supported_s[1]), by = .(SourceFile, AnimalNum, block, basis, bound)]
check(all(abs(s$a - 1) < 1e-12) && all(abs(s[su > 0]$b - 1) < 1e-12), "2: share_all and share_supported sum to 1")
x <- t1[AnimalNum == "OR1" & block == 100 & basis == "event" & bound == "LOCF" & unit == "C3"]
check(nrow(x) == 1L && x$seconds == 0 && x$share_all == 0 && x$share_supported == 0, "2: a column without time is a zero row")
check(all(c("run_id", "srs_run_id", "reconstruction", "basis", "basis_label", "tau_s", "bound", "support_criterion", "SourceFile", "raw_log", "Batch",
            "Sex", "CC", "board", "System", "CageEpisodeID", "AnimalNum", "tag", "n_animals_present", "n_tags_present", "n_tracked", "spatial_excluded",
            "censor_from", "block_set", "Phase", "Phase_vendor") %in% names(t1)), "2: metadata columns on the primary table")
check(identical(sort(unique(t1$basis_label)), c("event-start (historical reference)", "single-read (provisional candidate)")) &&
        identical(sort(unique(t1$support_criterion)), c("none (unrestricted LOCF)", "observation age <= 300 s", "observation age <= 60 s")), "2: labels")

# 3. hand-worked shares
v <- function(id, blk, b, bd, u, col = "share_all", d = t1) d[AnimalNum == id & block == blk & basis == b & bound == bd & unit == u][[col]]
check(near(v("OR1", 100, "event", "LOCF", "C1"), 33200 / 43200) && near(v("OR1", 100, "read", "LOCF", "C1"), 35200 / 43200) &&
        near(v("OR1", 100, "event", "LOCF", "C2"), 10000 / 43200), "3: LOCF column shares (C1 = R1 + R5)")
check(near(v("OR1", 100, "event", "max60s", "C1"), 23000 / 43200) && near(v("OR1", 100, "event", "max60s", "C1", "share_supported"), 23000 / 32000) &&
        near(v("OR1", 100, "event", "max60s", "unknown"), 11200 / 43200) && near(v("OR1", 100, "read", "max60s", "unknown"), 11200 / 43200) &&
        near(v("OR1", 100, "read", "max300s", "unknown", "seconds"), 2700), "3: unknown = observed - supported, the same in both bases")
check(near(v("OR1", 100, "event", "LOCF", "unknown"), 0), "3: LOCF unknown is 0 for a seeded animal")
check(near(v("OR2", 100, "read", "LOCF", "unknown", "seconds"), 3200) && near(v("OR2", 100, "read", "LOCF", "C1", "share_supported"), 1),
      "3: an unseeded animal's time before its first read is unknown even under LOCF")
tr <- T$occupancy_animal_phase_reader
check(near(v("OR1", 100, "read", "LOCF", "R5", d = tr), 14200 / 43200) && identical(tr[unit == "R5"]$resource[1], "house") &&
        near(v("OR5", 200, "event", "LOCF", "R6", d = tr), 1) && near(v("OR5", 200, "read", "LOCF", "R2", d = tr), 1), "3: reader shares and resource labels")

# 4. censoring and NA shares
a4 <- t1[AnimalNum == "OR4" & basis == "read" & bound == "LOCF" & unit == "C1"]
check(near(a4[block == 203]$observed_s, 10000) && near(a4[block == 203]$censored_s, 33200) && near(a4[block == 203]$share_all, 1), "4: observed time stops at the censoring time")
check(near(a4[block == 204]$observed_s, 0) && all(is.na(t1[AnimalNum == "OR4" & block == 204]$share_all)) && all(is.na(t1[AnimalNum == "OR4" & block == 204]$share_supported)),
      "4: no observed time gives NA shares, not 0")
check(all(is.na(t1[AnimalNum == "OR2" & block == 101 & bound == "max60s"]$share_supported)) && near(v("OR2", 101, "read", "max60s", "unknown"), 1),
      "4: no supported time gives NA shares of supported time; all observed time unknown")
t4 <- T$support_animal_phase
check(near(t4[AnimalNum == "OR1" & block == 100]$unknown_le60_share_of_observed_time, 11200 / 43200) && is.na(t4[AnimalNum == "OR4" & block == 204]$reads_per_observed_h),
      "4: support table shares and rates (NA without observed time)")
check(T$exclusions_censoring[record_id == "SX04"]$censored_s == 2 * (33200 + 43200) && T$exclusions_censoring[record_id == "SX04"]$censored_s_stage19_window == 0,
      "4: censored seconds per record; none inside stage19_window")

# 5. exclusions: flagged in animal tables, absent from summaries and figure inputs
ex <- "B3|sys.2|CC1"
check(all(t1[CageEpisodeID == ex]$spatial_excluded) && nrow(t1[CageEpisodeID == ex]) == 2L * 2L * 3L * 5L && !any(t1[CageEpisodeID != ex]$spatial_excluded),
      "5: the excluded cage episode is kept and flagged in the animal table")
animal_tabs <- c("occupancy_animal_phase_column", "occupancy_animal_phase_reader", "occupancy_animal_window_column", "occupancy_animal_window_reader",
                 "support_animal_phase", "unknown_by_last_reader_gap", "organisation_animal_block", "organisation_animal_window",
                 "stage19_comparison_animal_window", "exclusions_censoring")
for (nm in setdiff(names(T), animal_tabs)) {
  check(!("CageEpisodeID" %in% names(T[[nm]])) || !any(T[[nm]]$CageEpisodeID == ex), paste("5: excluded episode absent from", nm))
  check(!("spatial_excluded" %in% names(T[[nm]])) || !any(T[[nm]]$spatial_excluded %in% TRUE), paste("5: no excluded row in", nm))
}
check(!any(res$figures$f2$CageEpisodeID == ex) && !any(res$figures$f4$CageEpisodeID == ex) && identical(res$figures$excluded_episodes, ex), "5: figure inputs")
pa <- T$column_shares_by_bound[level == "pooled" & block_set == "stage19_window" & Phase == "Active" & basis == "event" & bound == "LOCF" & unit == "C1"]
check(near(pa$observed_time_s, 6 * 43200) && near(pa$share_of_observed_time, 73200 / (6 * 43200)), "5: pooled time without the excluded episode (C1 active, LOCF)")

# 6. windows, block sets and phase labels
w <- T$occupancy_animal_window_column[AnimalNum == "OR4" & window == "phase_class" & Phase == "Light" & basis == "read" & bound == "LOCF"]
check(near(w[block_set == "cc4_grid_period" & unit == "C1"]$share_all, 1) && near(w[block_set == "cc4_grid_period" & unit == "C4"]$share_all, 0) &&
        w[block_set == "cc4_grid_period" & unit == "C1"]$n_blocks == 1L, "6: the CC4 grid period is its own set: OR4's light I3 alone, not pooled with I2")
check(!any(unlist(lapply(T, function(x) if ("block_set" %in% names(x)) x$block_set))  %in% "whole_span") &&
        identical(sort(unique(T$occupancy_animal_window_column$block_set)), sort(SOC_BLOCK_SETS)) && identical(SOC_VERSION, "s19c_v0.2"),
      "6: no output row pools the grid period with the primary window (no whole_span set)")
check(near(w[block_set == "stage19_window" & unit == "C4"]$share_all, 1) && w[block_set == "stage19_window" & unit == "C4"]$n_blocks == 1L,
      "6: stage19_window leaves out CC4 I3")
check(identical(unique(t1[SourceFile == F2, .(block_no, block_set)])[order(block_no)]$block_set,
                c("stage19_window", "stage19_window", "stage19_window", "cc4_grid_period", "cc4_grid_period")), "6: CC4 A1, I2, A2 in stage19_window; I3, A3 in the grid period")
pl <- unique(t1[, .(SourceFile, block_no, Phase, Phase_vendor, PhaseNumber, phase_index_in_class)])
check(all(pl[Phase_vendor == "Inactive"]$Phase == "Light") && all(pl[Phase == "Light", phase_index_in_class == PhaseNumber - 1L]) &&
        all(pl[Phase == "Active", phase_index_in_class == PhaseNumber]), "6: Light = vendor Inactive; k-th light phase = vendor PhaseNumber k + 1")
c1 <- T$occupancy_animal_window_column[window == "cc1_active1"]
check(identical(sort(unique(c1$SourceFile)), F1) && all(c1$Phase == "Active") && all(c1$n_blocks == 1L), "6: the Stage 19 primary window is CC1 active phase 1")
ep <- T$cage_episode_window_column[CageEpisodeID == "B3|sys.1|CC1" & Phase == "Active" & block_set == "stage19_window" & basis == "read" & bound == "max60s" & unit == "C1"]
check(near(ep$share_all_pooled, (24000 + 30000) / 86400) && near(ep$share_all_animal_mean, mean(c(24000 / 43200, 30000 / 43200))) && ep$n_animals_observed == 2L,
      "6: cage-episode share pooled by seconds, with the equal-weight animal mean beside it")
bc <- T$basis_comparison_column[level == "pooled over animals (seconds)" & block_set == "stage19_window" & Phase == "Active" & bound == "LOCF" &
                                  denominator == "observed time (share_all)" & unit == "C1"]
check(near(bc$read_minus_event, 2000 / (6 * 43200)) && near(bc$event_share, 73200 / (6 * 43200)), "6: read - event pooled by seconds")
ut <- T$unsupported_time_summary[level == "pooled" & block_set == "stage19_window" & Phase == "Active" & bound == "max60s"]
check(near(ut$unknown_s, 11200 + 13200) && near(ut$unknown_share_of_observed_time, 24400 / (6 * 43200)), "6: unknown time pooled (Active, 60 s)")
og <- T$observation_gap_summary[level == "last reader" & block_set == "stage19_window" & Phase == "Active" & bound == "max60s"]
check(near(og[last_reader == "R5"]$unknown_s_grid, 8000 + 13200) && near(og[last_reader == "R5"]$reader_locf_s, 14200 + 40000) &&
        near(sum(og$share_of_phase_unknown_time), 1), "6: unknown time by the reader last assigned (house association)")
o8 <- T$organisation_animal_block[AnimalNum == "OR1" & block == 100 & basis == "event" & bound == "max60s"]
p8 <- c(23000, 9000) / 32000
check(near(o8$column_entropy_bits, -sum(p8 * log2(p8))) && o8$dominant_unit == "C1" && near(o8$unknown_share_all, 11200 / 43200), "6: organisation (S4) on supported shares")

# 7. parity with Stage 19, and gates that must fail
check(T$stage19_comparison_animal_window[, max(abs(event_LOCF_minus_stage19))] < 1e-12, "7: event LOCF equals the Stage-19-format fixture")
# mutation: an excluded episode leaking into a table without CageEpisodeID (F3/F5 inputs, column shares by CC) fails G14b
mut <- function(from, to) {
  src <- deparse(soc_build, width.cutoff = 500L)
  if (sum(grepl(from, src, fixed = TRUE)) != 1L) stop("mutation anchor not found once: ", from)
  f <- eval(parse(text = gsub(from, to, src, fixed = TRUE))); environment(f) <- environment(soc_build); f
}
g14b <- function(b) { g <- suppressWarnings(b(fx$tab, fx$ref, fx$meta, fx$expected))$gates; g[g[[1]] == "G14b"] }
m1 <- g14b(mut('fm <- pool_by(c("Phase", "CC"), "reader")', 'fm <- soc_pool(occ, ab, c("Phase", "CC"), "reader")'))
m2 <- g14b(mut('P_col_cc <- pool_by(c("Phase", "CC"), "column")', 'P_col_cc <- soc_pool(occ, ab, c("Phase", "CC"), "column")'))
check(!m1$pass && grepl("F3/F5", m1$value) && !m2$pass && grepl("column_shares_by_bound (by CC)", m2$value, fixed = TRUE) &&
        isTRUE(res$gates[res$gates[[1]] == "G14b"]$pass), "7: G14b fails when an excluded episode leaks into F3/F5 or the column shares by CC")
gate_fails <- function(id, mutate) { f <- mutate(make_fixture()); r <- soc_build(f$tab, f$ref, f$meta, f$expected); isFALSE(r$gates[gate_id == id]$pass) }
check(gate_fails("G10", function(f) { f$ref$stage19[AnimalNum == "OR1" & PositionID == 1L & Phase == "Active", PositionSeconds := PositionSeconds + 1]; f }),
      "7: a changed Stage 19 cell fails parity")
check(gate_fails("G12", function(f) { f$ref$shares_unknown_column_pooled[1, share_all := share_all + 1e-6]; f }), "7: a changed srs pooled share fails")
check(gate_fails("G03", function(f) { f$expected$animal_flags[AnimalNum == "OR1", spatial_excluded := TRUE]; f }), "7: a changed exclusion flag fails")
check(gate_fails("G04", function(f) { f$expected$counts$animal_blocks <- 17L; f }), "7: a changed row count fails")
check(gate_fails("G06", function(f) { f$tab$occupancy[AnimalNum == "OR1" & block == 100 & basis == "read" & PositionID == 1L, sup_60 := sup_60 + 1]; f }),
      "7: unequal support across bases fails")
check(gate_fails("G15", function(f) { f$tab$animals[, Group := "X"]; f }) == FALSE, "7: an extra input column is not carried into the outputs")

# 8. pinned inputs, run folders and the srs run record
td <- tempfile("s19c_"); dir.create(td)
f_in <- file.path(td, "occupancy.csv"); writeLines(c("a,b", "1,2"), f_in)
pins <- data.table(role = "srs table", item = "occupancy", path = f_in, expected_sha256 = digest::digest(file = f_in, algo = "sha256"),
                   listed_sha256 = digest::digest(file = f_in, algo = "sha256"), listed_in = "fixture")
check(isTRUE(s19c_verify_pins(pins)$verified), "8: a matching pinned input passes")
check(refused(s19c_verify_pins(copy(pins)[, expected_sha256 := strrep("0", 64)])), "8: a changed input sha256 is refused")
check(refused(s19c_verify_pins(copy(pins)[, listed_sha256 := strrep("1", 64)])), "8: an input its producer lists with another sha256 is refused")
check(refused(s19c_verify_pins(copy(pins)[, listed_sha256 := NA_character_])), "8: an input its producer does not list is refused")
check(refused(s19c_verify_pins(copy(pins)[, path := file.path(td, "missing.csv")])), "8: a missing input is refused")
proj <- file.path(td, "Exp", "Analysis", "Behavior", "RFID"); dir.create(proj, recursive = TRUE)
old_raw <- Sys.getenv("MMM_RAW_ANTENNA_DIR", unset = NA_character_); Sys.unsetenv("MMM_RAW_ANTENNA_DIR")
scratch <- file.path(td, "scratch", "s19c")
d <- s19c_output_dirs(scratch, "dev_t1", proj)
check(identical(basename(d$out), "dev_t1") && identical(basename(d$tmp), ".tmp_dev_t1"), "8: run and build folders under runs/")
dir.create(d$out, recursive = TRUE)
check(refused(s19c_output_dirs(scratch, "dev_t1", proj)), "8: an existing run folder is refused")
dir.create(file.path(scratch, "runs", ".tmp_dev_t2"), recursive = TRUE)
check(refused(s19c_output_dirs(scratch, "dev_t2", proj)), "8: an existing build folder is refused")
check(refused(s19c_output_dirs(scratch, "../x", proj)) && refused(s19c_output_dirs("relative/path", "dev_t3", proj)), "8: bad run id or relative root refused")
run <- c(run_id = "srs_v1.1_9b18624", version = "srs_v1.1", code_clean = "TRUE", dev_run = "FALSE", tau_s = "2", bounds_s = "2 10 60 300",
         manifest_version = "v1.1", manifest_sha256 = "abc")
check(isTRUE(s19c_srs_run_gates(run, "abc")$pass) && isFALSE(s19c_srs_run_gates(replace(run, "dev_run", "TRUE"), "abc")$pass) &&
        isFALSE(s19c_srs_run_gates(run, "def")$pass), "8: the srs run record gate (release, exclusion version hash)")
gd <- s19c_table_guide(c(T, list(gates = g)))
check(setequal(gd$table, c(names(T), "gates")) && !anyNA(gd$rows) && all(nzchar(gd$grain)) && all(nzchar(gd$contents)) &&
        all(endsWith(gd[table %in% S19C_GZIP_TABLES]$file, ".csv.gz")) && identical(gd[table == "gates"]$file, "gates.csv"),
      "8: the table guide lists every output table with its grain, contents, rows and file name")

# 9. static checks
src <- c(builder = "Testing/audits/build_spatial_occupancy_candidate.R", helpers = "Functions/spatial_occupancy_candidate.R")
txt <- lapply(src, function(f) readLines(f, warn = FALSE))
code_only <- lapply(txt, function(x) sub("#.*$", "", x))
for (nm in names(src)) {
  for (pat in c("\\blmer\\(", "\\blme4::", "emmeans", "p\\.adjust", "cohens_d", "\\bt\\.test\\(", "wilcox", "\\blm\\(", "\\bglm\\(", "anova\\(",
                "sus_animals", "con_animals", "CombZ\\b.*fread", "Group\\s*="))
    check(!any(grepl(pat, code_only[[nm]], perl = TRUE)), paste0("9: ", src[[nm]], " has no ", pat))
}
check(!any(grepl("sxm_read|spatial_exclusion_manifest|spatial_analysis_exclusions", txt$helpers)), "9: the helpers never name the exclusion manifest")
check(!any(grepl("^rfid_", basename(src))), "9: no new Functions/rfid_*.R")
an_files <- list.files("Analysis", pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
check(!any(vapply(an_files, function(f) any(grepl("spatial_occupancy_candidate|s19c_|soc_build", readLines(f, warn = FALSE))), logical(1))),
      "9: no Analysis/ file references the candidate")
main <- paste(deparse(Filter(function(e) is.call(e) && identical(e[[1]], as.name("if")), as.list(parse(src[["builder"]], keep.source = FALSE)))[[1]]), collapse = "\n")
pos <- function(p) regexpr(p, main, fixed = TRUE)[1]
check(pos("mmm_protection_guard_begin") > 0 && pos("mmm_protection_guard_begin") < pos("dir.create") && pos("mmm_protection_guard_begin") < pos("fwrite") &&
        pos("mmm_protection_guard_end") > pos("s19c_figures") && pos("mmm_protection_guard_end") < pos("output_files_sha256") &&
        pos("output_files_sha256") < pos("file.rename") && pos("mmm_isolated_output_root_check") < 0 && pos("s19c_output_dirs") < pos("mmm_protection_guard_begin"),
      "9: the protection guard starts before the first write and ends before the output hashes and the rename")
rd <- paste(deparse(s19c_read_inputs), collapse = "\n")
sel <- regmatches(rd, gregexpr("select = c\\([^)]*\\)", rd))[[1]]
check(length(sel) >= 3L && !any(grepl("Group|Sex|fixed_cage|CombZ", sel)) && any(grepl("PositionSeconds", sel)) && any(grepl("n_tracked", sel)),
      "9: Stage 19 and presence inputs are read by explicit select without group columns")

# 10. output roots
reg <- mmm_protected_output_locations(project_root = proj, repo_root = file.path(td, "repo"))
for (bad in c(file.path(proj, "analysis_ready", "analyses", "spatial_occupancy"), file.path(proj, "publication_ready", "tables", "spatial_occupancy"),
              file.path(proj, "publication_ready", "figures", "single_panels", "spatial_occupancy"))) {
  check(refused(s19c_output_dirs(bad, "dev_x", proj)), paste("10: refused as an isolated root:", bad))
  check(refused(mmm_protection_assert_base(bad, "dev_x", reg)), paste("10: refused by the protection guard:", bad))
}
check(!refused(mmm_protection_assert_base(file.path(td, "scratch2"), "dev_x", reg)), "10: an outside scratch root is accepted")
if (!is.na(old_raw)) Sys.setenv(MMM_RAW_ANTENNA_DIR = old_raw)
unlink(td, recursive = TRUE)

cat("test_spatial_occupancy_candidate: all checks passed\n")
