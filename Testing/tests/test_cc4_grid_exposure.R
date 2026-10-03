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
# Malformed raw timestamps fail closed in the full-recording path.
raw$DateTime[1] <- "not a timestamp"; readr::write_delim(raw, raw_path, delim = ";")
fails(preprocess_animalpos_file("B1", "CC4", character(), raw_dir, "unused", write_output = FALSE, phase_policy = "full_recording"), "Invalid raw DateTime")
cat("PASS: Stage 31 full retention, legacy parity, coverage, variable candidate timing, no circular effect test, correction provenance, driver outputs and overwrite guards\n")
