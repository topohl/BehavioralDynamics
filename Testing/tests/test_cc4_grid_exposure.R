# Portable fixtures through the actual Stage 31 entry function. No private data.
source("Analysis/31_cc4_grid_exposure.R")
check <- function(x, msg) if (!isTRUE(x)) stop("FAIL: ", msg, call. = FALSE)
fails <- function(expr, pattern) {
  e <- tryCatch({force(expr); NULL}, error = identity)
  check(inherits(e, "error") && grepl(pattern, conditionMessage(e)), paste("expected error", pattern))
}
tt <- function(x) as.POSIXct(x, tz = "UTC")
fixture <- tempfile("grid31_"); dir.create(fixture)
raw_dir <- file.path(fixture, "raw_data"); dir.create(raw_dir); dir.create(file.path(raw_dir, "B1"))
writeLines("EXCLUDED", file.path(raw_dir, "excluded_animals.csv"))
# Mid-I1 through mid-I6. Two animals; second stops during I3.
t <- seq(tt("2022-11-09 12:00:00"), tt("2022-11-14 12:00:00"), by = 300)
raw <- data.frame(DateTime = format(t, "%d.%m.%Y %H:%M:%OS3", tz = "UTC"), Animal = "001_sys.1",
                  RFID = "tag1", AM = "SAM", xPos = rep(c(0, 100), length.out = length(t)), yPos = 0, zPos = 0)
second <- raw[t <= tt("2022-11-11 14:00:00"), ]; second$Animal <- "002_sys.2"; second$RFID <- "tag2"
raw <- rbind(raw, second)
raw_path <- file.path(raw_dir, "B1", "E9_SIS_B1_CC4_AnimalPos.csv")
readr::write_delim(raw, raw_path, delim = ";")
raw_hash <- mmm_file_sha256(raw_path)
full <- preprocess_animalpos_file("B1", "CC4", "EXCLUDED", raw_dir, "unused", write_output = FALSE, phase_policy = "full_recording")
legacy <- preprocess_animalpos_file("B1", "CC4", "EXCLUDED", raw_dir, "unused", write_output = FALSE)
check(nrow(full$data) == nrow(raw), "full recording retains every eligible read")
check(max(full$data$ConsecActive) == 5 && max(full$data$ConsecInactive) == 6, "full counters include A5/I6")
check(max(legacy$data$ConsecActive) == 4 && max(legacy$data$ConsecInactive) == 5, "legacy maxima unchanged")
expected <- remove_phases(full$data)
check(identical(legacy$data$DateTime, expected$DateTime) && identical(legacy$data$AnimalID, expected$AnimalID), "legacy row selection parity")
fails(preprocess_animalpos_file("B1", "CC4", character(), raw_dir, "unused", phase_policy = "full_recording"), "write_output = FALSE")
cov <- grid31_phase_coverage(full$data)
check(all(!cov$SessionSpansWholeBlock[cov$PhaseLabel %in% c("I1", "I6")]), "edge phases are partial")
check(all(cov$SessionSpansWholeBlock[cov$PhaseLabel == "A5"]), "A5 is clock-complete")
check(all(cov$ReadCount[cov$AnimalID == "002" & cov$PhaseLabel == "A5"] == 0), "zero-read animal phases remain visible")
prof <- grid31_clock_profiles(full$data, cov)
check(sum(prof$cages$Crossings) == nrow(raw) - 2L, "all genuine crossings counted once; no first-read crossing")
check(all(!prof$cages$CompleteAnimalBracketing[prof$cages$System == "sys.2" & prof$cages$PhaseLabel == "A5"]), "lost animal never converted into a fully observed zero")
dup <- dplyr::bind_rows(full$data, dplyr::mutate(full$data[1, ], PositionID = 8L))
fails(grid31_clock_profiles(dup, cov), "Conflicting simultaneous")

# Opt-in censoring: animal 002 (sys.2) is unobserved from T, after its last read
# (14:00:00). The straddling bin is censored from T, later bins wholly; no row is
# dropped, counts stay finite and only censored cage-bins lose their rate.
cz <- tibble::tibble(Batch = "B1", CageChange = "CC4", System = "sys.2", CensorFrom = tt("2022-11-11 14:02:30"), CensorRecord = "SX99")
cov_c <- grid31_phase_coverage(full$data, cz)
prof_c <- grid31_clock_profiles(full$data, cov_c, cz)
check(!any(GRID31_CENSOR_COLUMNS %in% c(names(cov), names(prof$animals), names(prof$cages))), "no censor columns without a censor table")
check(identical(names(cov_c), c(names(cov), "CensorFrom", "CensorRecord", "CensoredSec")) &&
      identical(names(prof_c$animals), c(names(prof$animals), "CensorFrom", "CensorRecord", "CensoredSec")) &&
      identical(names(prof_c$cages), c(names(prof$cages), "CensorFrom", "CensorRecord", "CensoredAnimals")), "censoring only appends columns")
same <- function(x, y, cols = names(y)) identical(as.data.frame(x[cols]), as.data.frame(y[cols]))
check(same(cov_c, cov) && same(prof_c$animals, prof$animals), "censoring drops no row and changes no legacy coverage/animal-bin value")
a2 <- prof_c$animals[prof_c$animals$AnimalID == "002", ]
check(a2$CensoredSec[a2$BinStart == tt("2022-11-11 14:00:00")] == 150, "straddling bin censored from T to its end")
check(all(a2$CensoredSec[a2$BinStart >= tt("2022-11-11 14:05:00")] == 300) && all(a2$CensoredSec[a2$BinEnd <= tt("2022-11-11 14:00:00")] == 0),
      "later bins wholly censored, earlier bins not")
check(all(prof_c$animals$CensoredSec[prof_c$animals$AnimalID == "001"] == 0) && all(is.na(prof_c$animals$CensorFrom[prof_c$animals$AnimalID == "001"])),
      "other cage episodes uncensored")
check(cov_c$CensoredSec[cov_c$AnimalID == "002" & cov_c$PhaseLabel == "I3"] == 16050 &&
      all(cov_c$CensoredSec[cov_c$AnimalID == "002" & cov_c$BlockStart > tt("2022-11-11 18:00:00")] == 43200), "phase coverage carries censored seconds")
cg <- prof_c$cages
check(identical(is.na(cg$CrossingsPerExpectedAnimalHour), cg$CensoredAnimals > 0) && sum(cg$CensoredAnimals > 0) > 0 &&
      all(cg$CensoredAnimals[cg$System == "sys.2"] == (cg$BinEnd[cg$System == "sys.2"] > cz$CensorFrom)), "cage rate NA exactly in censored cage-bins")
check(same(cg, prof$cages, setdiff(names(prof$cages), "CrossingsPerExpectedAnimalHour")) &&
      identical(cg$CrossingsPerExpectedAnimalHour[cg$CensoredAnimals == 0], prof$cages$CrossingsPerExpectedAnimalHour[cg$CensoredAnimals == 0]) &&
      all(is.finite(cg$Crossings)) && all(is.finite(prof_c$animals$Crossings)), "uncensored rates and all counts unchanged and finite")
el <- grid31_candidates(cg)$eligibility; el0 <- grid31_candidates(prof$cages)$eligibility
check(all(el$Reason[el$System == "sys.2"] == "censored_SX99") && !any(el$EligibleForCandidateScan[el$System == "sys.2"]) &&
      identical(el[el$System == "sys.1", ], el0[el0$System == "sys.1", ]), "censored cage-days labelled and never scanned; others unchanged")
fails(grid31_clock_profiles(full$data, cov_c), "built with a censor table")
fails(grid31_phase_coverage(full$data, rbind(cz, cz)), "Invalid censor table")

# A known two-hour increase on I3, shifted by two hours on I4. Per-cage starts
# are recovered independently; identical baseline has no positive candidate.
cages <- prof$cages %>% dplyr::filter(System == "sys.1", Phase == "Inactive", PhaseNumber %in% 2:5)
cages$Crossings <- 1L
cages$Crossings[cages$PhaseNumber == 3 & cages$BinIndex %in% 42:65] <- 11L
cages$Crossings[cages$PhaseNumber == 4 & cages$BinIndex %in% 66:89] <- 11L
scan <- grid31_candidates(cages)
best <- scan$candidates[scan$candidates$CandidateRank == 1, ]
check(identical(best$BinIndex, c(42L,66L)), "variable event times not forced to one clock time")
check(all(best$ExcessCrossingsPerAnimalHour == 120), "same-clock I2 contrast has correct denominator")
check(all(best$TimingStatus == "activity_inferred_unverified"), "candidates remain unverified")
check(!any(scan$candidates$PhaseNumber == 5), "flat I5 does not force an exposure time")
broken <- cages; broken$CompleteAnimalBracketing <- FALSE
none <- grid31_candidates(broken)
check(nrow(none$candidates) == 0 && "CandidateStart" %in% names(none$candidates), "ineligible scan has stable empty schema")

# Correction registry is explicit, hashed and retains original labels.
correction <- data.frame(Batch = "B1", CageChange = "CC4", AnimalID = "001", old_System = "sys.1", new_System = "sys.3", evidence = "fixture")
correction_file <- file.path(fixture, "corrections.csv"); readr::write_csv(correction, correction_file)
sha <- unname(mmm_file_sha256(correction_file))
out <- file.path(fixture, "result")
fails(run_cc4_grid_exposure(fixture, out, correction_file, paste(rep("0",64),collapse=""), "B1", FALSE), "hash mismatch")
check(!dir.exists(out), "failed validation must not create output")
result <- run_cc4_grid_exposure(fixture, out, correction_file, sha, "B1", make_plots = TRUE)
check(file.exists(file.path(out, "figures", "B1_inactive_clock_profiles.png")), "actual driver renders diagnostic plot")
saved <- readr::read_csv(file.path(out,"full_recording","E9_SIS_B1_CC4_AnimalPos_full_recording.csv"),
                        col_types = readr::cols(.default = readr::col_character()))
check(nrow(saved) == nrow(raw) && any(saved$ConsecActive == "5"), "driver saves recovered A5")
check(all(saved$System[saved$AnimalID == "001"] == "sys.3") && all(saved$System_raw[saved$AnimalID == "001"] == "sys.1"), "correction applied with original identity preserved")
timing <- readr::read_csv(file.path(out,"tables","grid_timing_review.csv"), show_col_types = FALSE)
check(all(is.na(timing$GridInsertedAt)) && all(!timing$ExposureAnalysisAllowed), "unknown timing never populated from candidates")
whole <- result$whole_inactive
check(nrow(whole$animals) == 8 && sum(whole$animals$IncludedInPrimary) == 4, "whole-phase driver keeps a fixed complete cohort")
check(all(is.na(whole$animals$CrossingsPerHour[whole$animals$AnimalID == "002" & whole$animals$PhaseLabel != "I2"])),
      "incomplete phase activity is missing, never a zero or partial count divided by 12")
check(all(whole$animals$CrossingsPerHour[whole$animals$AnimalID == "001"] == 12), "whole-phase rate sums all 144 five-minute bins and divides by 12")
check(file.exists(file.path(out, "tables", "whole_inactive_sex_summary.csv")), "actual driver writes whole-phase summaries")
check(file.exists(file.path(out, "figures", "whole_inactive_animal_cage_trajectories.png")), "actual driver plots animal and cage trajectories")
check(all(result$whole_inactive_uncertainty$status$Draws == 0), "one-batch fixture cannot produce uncertainty intervals")
check(identical(raw_hash, mmm_file_sha256(raw_path)), "raw input unchanged")
fails(run_cc4_grid_exposure(fixture, out, correction_file, sha, "B1", FALSE), "already exists")

# Driver censoring with the released manifest v1.1: SX04 censors B5|sys.1|CC4 from
# 2023-12-04 14:00:42.962. B5 fixture: the four sys.1 animals stop at that read;
# one sys.2 animal reads to the end. Legacy run vs censored run of the same data.
make_b5 <- function(sys1 = "sys.1", ids = c("00314", "00318", "OR620", "OR630"), last = "04.12.2023 14:00:42.962",
                    end = "2023-12-07 12:00:00") {
  dir <- tempfile("grid31_b5_"); dir.create(file.path(dir, "raw_data", "B5"), recursive = TRUE)
  writeLines("EXCLUDED", file.path(dir, "raw_data", "excluded_animals.csv"))
  t5 <- seq(tt("2023-12-02 12:00:00"), tt(end), by = 300); t1 <- t5[t5 <= tt("2023-12-04 14:00:00")]
  rows <- function(id, sys, when) data.frame(DateTime = when, Animal = paste0(id, "_", sys), RFID = id, AM = "SAM",
                                             xPos = rep(c(0, 100), length.out = length(when)), yPos = 0, zPos = 0)
  fmt <- function(x) format(x, "%d.%m.%Y %H:%M:%OS3", tz = "UTC")
  r <- do.call(rbind, c(lapply(ids, rows, sys = sys1, when = fmt(t1)), list(rows(ids[3], sys1, last), rows("OR700", "sys.2", fmt(t5)))))
  readr::write_delim(r, file.path(dir, "raw_data", "B5", "E9_SIS_B5_CC4_AnimalPos.csv"), delim = ";")
  dir
}
fx5 <- make_b5()
out_l <- file.path(fixture, "b5_legacy"); out_c <- file.path(fixture, "b5_censored")
run_cc4_grid_exposure(fx5, out_l, correction_file, sha, "B5", make_plots = FALSE)
run_cc4_grid_exposure(fx5, out_c, correction_file, sha, "B5", make_plots = FALSE, censor_manifest = "v1.1")
rd <- function(root, f) utils::read.csv(file.path(root, f), colClasses = "character", na.strings = "", check.names = FALSE)
hdr <- function(root, f) strsplit(readLines(file.path(root, f), n = 1L), ",", fixed = TRUE)[[1]]
legacy_headers <- list(
  "audit/phase_animal_coverage.csv" = c("Batch","CageChange","System","AnimalID","AnimalFirstRead","AnimalLastRead","PhaseBlockIndex","Phase",
    "ConsecActive","ConsecInactive","BlockStart","BlockEnd","PhaseNumber","PhaseLabel","SessionSpansWholeBlock","ReadCount","FirstRead",
    "LastRead","MaxWithinBlockReadGapSec","AnimalSpanOverlapSec","AnimalSpansWholeBlock","AnimalNum","Sex"),
  "tables/animal_clock_profiles_5min.csv" = c("Batch","CageChange","System","AnimalID","AnimalFirstRead","AnimalLastRead","PhaseBlockIndex",
    "Phase","ConsecActive","ConsecInactive","BlockStart","BlockEnd","PhaseNumber","PhaseLabel","SessionSpansWholeBlock","AnimalSpanOverlapSec",
    "AnimalSpansWholeBlock","BinIndex","BinStart","BinEnd","ReadCount","Crossings","SpanOverlapSec","MinutesFromPhaseStart","AnimalNum","Sex"),
  "tables/cage_clock_profiles_5min.csv" = c("Batch","CageChange","System","Phase","PhaseLabel","PhaseNumber","PhaseBlockIndex","BinIndex",
    "BinStart","BinEnd","MinutesFromPhaseStart","SessionSpansWholeBlock","ExpectedAnimals","AnimalsWithReads","AnimalsSpanningBin","ReadCount",
    "Crossings","CompleteAnimalBracketing","CrossingsPerExpectedAnimalHour","Sex"),
  "tables/whole_inactive_animal_activity.csv" = c("Batch","CageChange","System","AnimalID","AnimalNum","Sex","PhaseLabel","BlockStart","BlockEnd",
    "RecordedCrossings","NominalHours","EligiblePhase","CageID","AnimalKey","CrossingsPerHour","PhaseStatus","BalancedAnimal","BalancedCage","IncludedInPrimary"),
  "tables/whole_inactive_cage_activity.csv" = c("Sex","Batch","CageChange","System","CageID","PhaseLabel","ExpectedAnimals","EligibleAnimals",
    "RecordedCrossings","CompleteRosterPhase","IncludedInPrimary","CageMeanCrossingsPerHour"),
  "tables/whole_inactive_cage_changes_from_I2.csv" = c("Sex","Batch","CageChange","System","CageID","PhaseLabel","ExpectedAnimals","EligibleAnimals",
    "RecordedCrossings","CompleteRosterPhase","IncludedInPrimary","CageMeanCrossingsPerHour","BaselineCageRate","ChangeFromI2"),
  "tables/grid_timing_review.csv" = c("Batch","CageChange","System","PhaseLabel","BlockStart","BlockEnd","GridInsertedAt","GridRemovedAt",
    "TimingStatus","TimingEvidence","ExposureAnalysisAllowed"),
  "audit/candidate_scan_eligibility.csv" = c("Batch","CageChange","System","PhaseLabel","PhaseNumber","EligibleForCandidateScan","Reason"))
added <- list("audit/phase_animal_coverage.csv" = c("CensorFrom","CensorRecord","CensoredSec"),
  "tables/animal_clock_profiles_5min.csv" = c("CensorFrom","CensorRecord","CensoredSec"),
  "tables/cage_clock_profiles_5min.csv" = c("CensorFrom","CensorRecord","CensoredAnimals"),
  "tables/whole_inactive_cage_activity.csv" = "CensoredAnimals", "tables/whole_inactive_cage_changes_from_I2.csv" = "CensoredAnimals",
  "tables/grid_timing_review.csv" = "CensoredFrom")
for (f in names(legacy_headers)) check(identical(hdr(out_l, f), legacy_headers[[f]]) && identical(hdr(out_c, f), c(legacy_headers[[f]], added[[f]])),
                                       paste("legacy schema without censoring; new columns appended last:", f))
files_l <- list.files(out_l, recursive = TRUE); files_c <- list.files(out_c, recursive = TRUE)
check(identical(setdiff(files_c, files_l), "audit/censoring_applied.csv") && !length(setdiff(files_l, files_c)), "censoring adds only its audit table")
unchanged <- c(grep("^full_recording/", files_l, value = TRUE), "audit/row_accounting.csv", "audit/whole_inactive_population.csv",
  "audit/whole_inactive_bootstrap_status.csv", "audit/whole_inactive_bootstrap_draws.csv", "audit/all_two_hour_candidate_scores.csv",
  "tables/activity_inferred_candidates_UNVERIFIED.csv", "tables/whole_inactive_sex_summary.csv")
check(identical(unname(tools::md5sum(file.path(out_l, unchanged))), unname(tools::md5sum(file.path(out_c, unchanged)))), "outputs without censored content are byte-identical")
ac <- rd(out_c, "tables/animal_clock_profiles_5min.csv"); al <- rd(out_l, "tables/animal_clock_profiles_5min.csv")
cut <- as.numeric(tt("2023-12-04 14:00:42.962")); bs <- as.numeric(tt(sub("T", " ", sub("Z$", "", ac$BinStart))))
check(nrow(ac) == nrow(al) && same(ac, al), "censored run keeps every animal-bin row and value")
cs <- as.numeric(ac$CensoredSec); s1 <- ac$System == "sys.1"; straddle <- s1 & bs < cut & bs + 300 > cut
check(sum(straddle) == 4 && all(abs(cs[straddle] - (300 - 42.962)) < 1e-3), "SX04 straddling bin censored from 14:00:42.962")
check(all(cs[s1 & bs > cut] == 300) && all(cs[s1 & bs + 300 <= cut] == 0) && all(cs[!s1] == 0) && all(ac$CensorRecord[s1] == "SX04"),
      "later bins 300 s, earlier bins and other cages 0")
cc <- rd(out_c, "tables/cage_clock_profiles_5min.csv"); cl <- rd(out_l, "tables/cage_clock_profiles_5min.csv")
check(identical(is.na(cc$CrossingsPerExpectedAnimalHour), as.integer(cc$CensoredAnimals) > 0) &&
      identical(cc[as.integer(cc$CensoredAnimals) == 0, names(cl)], cl[as.integer(cc$CensoredAnimals) == 0, ]) &&
      identical(cc[setdiff(names(cl), "CrossingsPerExpectedAnimalHour")], cl[setdiff(names(cl), "CrossingsPerExpectedAnimalHour")]),
      "cage rate NA only in censored cage-bins; all other values unchanged")
wa <- rd(out_c, "tables/whole_inactive_animal_activity.csv"); wl <- rd(out_l, "tables/whole_inactive_animal_activity.csv")
cens <- wa$System == "sys.1" & wa$PhaseLabel %in% c("I3", "I4", "I5")
check(all(wa$PhaseStatus[cens] == "censored_after_last_read_SX04") && identical(wa$PhaseStatus[!cens], wl$PhaseStatus[!cens]) &&
      identical(wa[setdiff(names(wl), "PhaseStatus")], wl[setdiff(names(wl), "PhaseStatus")]), "whole phases labelled; eligibility and rates unchanged")
el <- rd(out_c, "audit/candidate_scan_eligibility.csv")
check(all(el$Reason[el$System == "sys.1"] == "censored_SX04") && all(el$EligibleForCandidateScan[el$System == "sys.1"] == "FALSE"), "censored cage-days labelled")
tm <- rd(out_c, "tables/grid_timing_review.csv")
check(all(tm$CensoredFrom[tm$System == "sys.1"] == format(tt("2023-12-04 14:00:42.962"), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")) &&
      all(is.na(tm$CensoredFrom[tm$System != "sys.1"])), "timing review carries the censor time")
ap <- rd(out_c, "audit/censoring_applied.csv"); span <- (as.numeric(tt("2023-12-07 12:00:00")) - cut) / 3600
check(nrow(ap) == 1 && ap$RecordId == "SX04" && ap$CageEpisodeID == "B5|sys.1|CC4" && ap$VendorRowsAfterFrom == "0" && ap$Animals == "4" &&
      abs(as.numeric(ap$CensoredHoursPerAnimal) - span) < 1e-6 && abs(as.numeric(ap$CensoredAnimalHours) - 4 * span) < 1e-6 &&
      as.integer(ap$CensoredAnimalBinsInRecording) == 4L * as.integer(ap$CensoredCageBinsInRecording), "censoring audit")
readme <- readLines(file.path(out_c, "README.txt"))
check(any(grepl("span_end is the end of the full CC4 recording, 2023-12-07 12:00:00", readme, fixed = TRUE)) &&
      any(grepl("not Stage 00b's 2023-12-06 18:30:00", readme, fixed = TRUE)) && !any(grepl("Censoring", readLines(file.path(out_l, "README.txt")))),
      "README states the Stage 31 span end")
check(any(grepl("spatial_analysis_exclusions_v1.1.csv", rd(out_c, "audit/input_manifest.csv")$Path, fixed = TRUE)) &&
      !any(grepl("spatial", rd(out_l, "audit/input_manifest.csv")$Path)), "manifest and loader hashed only when censoring")
# Fail closed, before any output: a vendor row after T, an unknown cage episode,
# other animals, a time outside the session, an unreleased manifest version.
bad <- list(list(make_b5(last = "04.12.2023 14:00:43.000"), "vendor rows after the censor time"),
            list(make_b5(sys1 = "sys.3"), "cage episode not in the data"),
            list(make_b5(ids = c("00314", "00318", "OR620", "OR631")), "animals differ"),
            list(make_b5(end = "2023-12-04 12:00:00", last = "04.12.2023 11:00:01.000"), "outside the session"),
            list(fx5, "not a released version", "v9.9"))
for (b in bad) {
  o <- file.path(fixture, basename(tempfile("b5_fail_")))
  fails(run_cc4_grid_exposure(b[[1]], o, correction_file, sha, "B5", FALSE, censor_manifest = if (length(b) > 2) b[[3]] else "v1.1"), b[[2]])
  check(!dir.exists(o), paste("failed censoring check writes nothing:", b[[2]]))
}
# Malformed raw timestamps fail closed in the full-recording path.
raw$DateTime[1] <- "not a timestamp"; readr::write_delim(raw, raw_path, delim = ";")
fails(preprocess_animalpos_file("B1", "CC4", character(), raw_dir, "unused", write_output = FALSE, phase_policy = "full_recording"), "Invalid raw DateTime")
cat("PASS: Stage 31 full retention, legacy parity, coverage, variable candidate timing, no circular effect test, correction provenance, driver outputs, overwrite guards and opt-in censoring\n")
