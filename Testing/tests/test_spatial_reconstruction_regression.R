# ===========================================================================
# Regression tests of the single-read spatial reconstruction (srs_v1.1): exclusions, missing-time denominators and
# timestamp handling, end to end on a synthetic UTF-16LE raw log through asr_process_recording() and the builder's
# summaries. Portable: everything under tempdir(); nothing on the lab drive. Expected values are worked out by hand.
#
#  1. timestamps: reads from the MsgValue1 gaps of a multi-read event; an overlapping short event at another antenna is
#     a separate observation; the event basis holds it until the next event start (99.5 s), the read basis returns to the
#     long event's antenna (a 0.3-s flip removed by the 2-s filter)
#  2. a same-millisecond tie at two readers resolves to the lowest PositionID when the held reader is not among them
#  3. denominators: observed time per animal-block = block time before the censoring time; LOCF assigns all of it; the
#     unsupported time at 60 s is exact (hand count) and, with the column shares, sums to the observed time
#  4. censoring: no occupancy and no observation after the censoring time; the censored seconds are counted
#  5. exclusions: an excluded cage episode is computed and flagged but left out of every summary; movement exclusions
#  6. the 1-s grid's unknown time agrees with the exact computation; co-location counts on the grid
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble); library(ggplot2) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_spatial_reconstruction.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
# times carry float noise of a few microseconds (Excel day fractions), so time comparisons use a 1-ms tolerance
near <- function(a, b, tol = 1e-3) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1, check.attributes = FALSE))
canonical_animal_id <- function(x) {   # as Functions/behavioral_dynamics_helpers.R
  x <- toupper(gsub("\\s+", "", trimws(as.character(x))))
  num <- grepl("^[0-9]+$", x); x[num] <- sub("^0+(?=.)", "", x[num], perl = TRUE)
  ifelse(is.na(x) | !nzchar(x), NA_character_, x)
}
MMM_REPO_ROOT <- repo
for (f in c("animalpos_preprocessing_helpers.R", "antenna_log_helpers.R", "animalpos_reconstruction.R", "antenna_data_version.R",
            "antenna_measurement.R", "antenna_acquisition_qc.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "manuscript_palette.R",
            "spatial_exclusion_manifest.R", "antenna_spatial_reconstruction.R")) source(file.path("Functions", f))
source("Testing/audits/build_single_read_spatial_reconstruction.R")   # definitions only (main-guarded)

# ---- a synthetic recording: B3 CC1, boards 1 (OR1 = T1, OR2 = T2) and 2 (OR3 = T3, OR4 = T4) -------------------------
S0 <- as.numeric(as.POSIXct("2023-04-24 18:30:00", tz = "UTC"))
e <- aqc_expected_devices()
ant_of <- function(pid) e$antenna[e$PositionID == pid]
excel <- function(s) sprintf("%.10f", s / 86400 + RFID_EXCEL_EPOCH_DAYS)
evt <- function(t, tag, board, pid, gaps = 0) data.frame(t = t, end = t + if (length(gaps) > 1) sum(gaps) / 1000 else 0.075,
  row = sprintf("%s;%s;unknown;AM%d-%d;%d;;;%d;;;;%s;;", excel(t), tag, board, ant_of(pid), if (length(gaps) > 1) sum(gaps) else 75L,
                length(gaps), paste(gaps, collapse = ",")))
E <- list()
add <- function(...) E[[length(E) + 1L]] <<- evt(...)
# OR1: seed at R1, R1 single reads every 30 s for 100 reads, then the timestamp case at t1, a read at t1 + 100, one read
#      after the censoring time (S0 + 60000) and a last read
add(S0 - 100, "T1", 1, 1)
for (k in 0:99) add(S0 + 10 + 30 * k, "T1", 1, 1)
t1 <- S0 + 5000
add(t1, "T1", 1, 1, c(0, 400, 400, 400, 400))   # reads at t1, +0.4, +0.8, +1.2, +1.6 (one event)
add(t1 + 0.5, "T1", 1, 5)                        # a single read at R5 inside the long R1 event
add(t1 + 100, "T1", 1, 1)
add(S0 + 60000, "T1", 1, 2)                      # after the censoring time: never counted
add(S0 + 86000, "T1", 1, 1)
# OR2: seed at R5, R5 reads every 600 s in block 1 (600 ... 42600), a tie at S0 + 7000 (R2 and R3 in one ms), a last read
add(S0 - 100, "T2", 1, 5)
for (k in 1:71) add(S0 + 600 * k, "T2", 1, 5)
add(S0 + 7000, "T2", 1, 2); add(S0 + 7000, "T2", 1, 3)
add(S0 + 86000, "T2", 1, 5)
# board 2 (excluded from spatial summaries in the manifest below)
for (tg in c("T3", "T4")) { add(S0 - 50, tg, 2, 1); for (k in 1:20) add(S0 + 2000 * k, tg, 2, if (k %% 2) 1 else 8); add(S0 + 86000, tg, 2, 8) }
E <- do.call(rbind, E); E <- E[order(E$end, E$t), ]
tmp <- tempfile("srs_"); dir.create(file.path(tmp, "B3", "raw"), recursive = TRUE)
devrows <- unlist(lapply(1:5, function(b) sprintf("#ID-Device;AM%d-%d;%g;%g;0;SAM", b, e$antenna, e$gx, e$gy)))
sysrows <- c(sprintf("%s;;;;;;;;;;version;22:04:2022;1.0.8147.28429;", excel(S0 - 3600)), sprintf("%s;;;Control;;;;;;;start;x.xlsx;;", excel(S0 - 3600)))
hdr <- "DateTime;IdRFID;IdLabel;unitLabel;eventDuration;sense1duration;sense1Events;senseRFIDrecords;outFuncLabel;outLabel;SystemMsg;MsgValue1;MsgValue2;MsgValue3"
path <- file.path(tmp, "B3", "raw", "MultiMouseMonitor-23.04.24-Batch3_change_1_24Apr23.csv")
writeBin(c(as.raw(c(0xFF, 0xFE)), iconv(paste(c(hdr, devrows, sysrows, E$row, sprintf("%s;;;Control;;;;;;;end;;;", excel(S0 + 86001))), collapse = "\r\n"),
                                        "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), path)
rec <- rfid_read_recording(tmp, "B3", "CC1", keep_read_offsets = TRUE)
tags <- data.table(AnimalNum = c("OR1", "OR2", "OR3", "OR4"), tag = c("T1", "T2", "T3", "T4"))
vendor <- data.frame(DateTime = c("2023-04-24T18:31:00.000Z", "2023-04-25T07:00:00.000Z"), AnimalID = "OR1", System = "sys.1", PositionID = "1",
                     CageChange = "CC1", Batch = "B3", Phase = c("Active", "Inactive"), ConsecActive = c("1", "0"), ConsecInactive = c("0", "1"),
                     HalfHoursElapsed = "0", is_synthetic_preprocessing_row = "FALSE", synthetic_row_source = "")
vendor <- rbind(vendor, transform(vendor, AnimalID = "OR2"), transform(vendor, AnimalID = "OR3", System = "sys.2"), transform(vendor, AnimalID = "OR4", System = "sys.2"))
cfrom <- S0 + 50000
man <- data.table(manifest_version = "vt", record_id = c("T1", "T2", "T3"), action = c("exclude_spatial", "censor", "exclude_movement"),
                  Batch = "B3", CC = "CC1", board = c("2", "1", "2"), System = c("sys.2", "sys.1", "sys.2"),
                  CageEpisodeID = c("B3|sys.2|CC1", "B3|sys.1|CC1", "B3|sys.2|CC1"), reader = "R1", resource = "", animals = "",
                  from = c("span_start", format(as.POSIXct(cfrom, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%OS3"), "span_start"),
                  to = "span_end", applies_to = "test", measures_affected = "test", movement_outcomes = "test", evidence = "test",
                  registered_relation = "unchanged", decision = "test", decision_date = "2026-10-14")
check(isTRUE(sxm_validate(man)), "setup: the test manifest validates")
res <- asr_process_recording(vendor, rec, tags, man, "B3", "CC1", "E9_SIS_B3_CC1_AnimalPos_preprocessed.csv", knockout = FALSE)
S1 <- res$animals$span_end[1]
check(near(S1, S0 + 86000.075), "setup: the span ends at the recording end")

# 1. timestamps
an <- res$animals[order(AnimalNum)]
check(an[AnimalNum == "OR1"]$observations == 1 + 100 + 5 + 1 + 1 + 1 + 1, "1: the five reads of a multi-read event and the overlapping read are separate observations")
x1 <- res$crosstab[AnimalNum == "OR1"]
check(near(sum(x1[pos_e == 5 & pos_r == 1]$seconds), 99.5) && near(sum(x1[pos_e != pos_r]$seconds), 99.5),
      "1: the event basis holds the overlapping R5 event for 99.5 s; the read basis stays at R1")
ra <- res$read_agreement[AnimalNum == "OR1"]
check(sum(ra$observations) - sum(ra$read_holds_read_reader) == 1 && sum(ra$observations) - sum(ra$event_holds_read_reader) == 3,
      "1: at the reads, the read basis differs only at the filtered R5 flip; the event basis at the three later R1 reads of the long event")

# 2. tie
check(an[AnimalNum == "OR2"]$tie_observations == 1, "2: one same-millisecond tie")
r2 <- res$runs[AnimalNum == "OR2" & abs(start - (S0 + 7000)) < 1e-3]
check(nrow(r2) == 1 && r2$PositionID == 2L && near(r2$end, S0 + 7200), "2: the tie resolves to R2 (lowest; held R5 not among them) until the next read")

# 3. denominators
ab <- res$animal_block[order(AnimalNum, block)]
b1 <- ab[AnimalNum == "OR1"]
check(near(b1$observed_s, c(43200, cfrom - S0 - 43200)) && all(near(ab[AnimalNum == "OR3"]$observed_s, c(43200, S1 - S0 - 43200))),
      "3: observed time = block time before the censoring time")
oc <- res$occupancy[basis == "read", .(locf = sum(seconds), sup60 = sum(sup_60)), by = .(AnimalNum, block)]
m <- merge(ab, oc, by = c("AnimalNum", "block"), all.x = TRUE)
check(all(near(m$locf, m$observed_s)), "3: LOCF assigns every observed second (all animals seeded)")
# OR2 block 1: supported (age <= 60 s) = 71 reads x 60 s + the tie read's 60 s (S0 + 7000, not overlapping S0 + 7200) = 4320
check(near(m[AnimalNum == "OR2" & block == min(block)]$sup60, 4320), "3: supported seconds at 60 s, by hand")
sh <- asr_shares_with_unknown(res$occupancy[basis == "read"][, column := amv_column(PositionID)], ab[, .(AnimalNum, block, observed_s)],
                              by = c("AnimalNum", "block"), unit = "column")
s2 <- sh[AnimalNum == "OR2" & block == min(ab$block) & bound == "max60s"]
check(near(s2[unit == "unknown"]$share_all, (43200 - 4320) / 43200) && near(sum(s2$share_all), 1) && near(sum(s2$share_supported, na.rm = TRUE), 1),
      "3: unknown share and both denominators")
check(all(sh[, near(sum(share_all), 1), by = .(AnimalNum, block, bound)]$V1[sh[, sum(observed_s > 0), by = .(AnimalNum, block, bound)]$V1 > 0]),
      "3: the units and unknown sum to 1 wherever time was observed")

# 4. censoring
check(all(abs(res$censored[board == 1, sum(seconds), by = AnimalNum]$V1 - (S1 - cfrom)) < 1e-3) && !nrow(res$censored[board == 2]), "4: censored seconds")
check(nrow(res$runs[board == 1 & start >= cfrom]) == 0 && sum(ra$observations) == an[AnimalNum == "OR1"]$observations - 2 - 1,
      "4: no run and no counted observation after the censoring time (seed, post-censoring and last read left out)")

# 5. exclusions
check(identical(res$animals[order(AnimalNum)]$spatial_excluded, c(FALSE, FALSE, TRUE, TRUE)) &&
        identical(res$animals[order(AnimalNum)]$movement_excluded, c(FALSE, FALSE, TRUE, TRUE)), "5: excluded cage episode flagged")
sm <- srs_summaries_v11(res, man)
check(!any(grepl("sys.2", sm$shares_unknown_column_by_episode$CageEpisodeID)) && any(grepl("sys.1", sm$shares_unknown_column_by_episode$CageEpisodeID)),
      "5: the excluded cage episode is left out of the summaries")
obs_sum <- sum(res$animal_block[board == 1]$observed_s)
check(near(sum(unique(sm$shares_unknown_column_pooled[basis == "read" & bound == "LOCF", .(Phase, observed_s)])$observed_s), obs_sum),
      "5: the pooled denominators are the non-excluded, uncensored observed time")
check(all(sm$shares_unknown_column_pooled[, near(sum(share_all), 1), by = .(Phase, basis, bound)]$V1), "5: pooled shares sum to 1")

# 6. grid against exact
gu <- res$colocation[bound_s == 60 & state == "unknown", .(grid = sum(seconds)), by = .(AnimalNum, block)]
m <- merge(m, gu, by = c("AnimalNum", "block"), all.x = TRUE)[is.na(grid), grid := 0]
check(all(abs(m$grid - (m$observed_s - m$sup60)) <= 3), "6: the 1-s grid's unknown time is within 3 s of the exact value per animal-block")
co <- res$colocation[bound_s == 60 & AnimalNum == "OR1"]
check(all(co$mates == 1) && all(co$mate_same_reader <= co$seconds), "6: co-location counts on the grid")

unlink(tmp, recursive = TRUE)
cat("test_spatial_reconstruction_regression: all checks passed\n")
