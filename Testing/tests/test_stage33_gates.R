# Stage 33 core gates: a passing and a failing fixture for each, including NA and empty inputs.
#
#   GC-1d  canonical_animal_id taken by parse(): srcref sha256 and probe (the repository helper and a tampered copy)
#   GC-2   the plan's LF sha256 (LF and CRLF copies agree; an edited copy differs)
#   GC-3   package versions (the installed set and a wrong expectation)
#   GC-5   input sha256 (a matching, a changed and a missing file)
#   GC-6   the planning parsers on in-memory sheets (Social Golfer plan, GroupCompBatch2, GroupComposition) and the
#          ID-list reader; the recording start of a synthetic raw file
#   GC-8   declared tables, tier column, section-7 columns and levels
#   gate rows: NA fails, empty (vacuous) fails, not-evaluated never stops
# Portable: temporary files and in-memory matrices only; no readxl, no S: drive.

suppressPackageStartupMessages({ library(data.table); library(digest) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
for (f in c("Functions/stage32_run.R", "Functions/stage33_common.R", "Functions/stage33_run.R")) source(f)
fx <- file.path(tempdir(), paste0("s33gates_", Sys.getpid())); dir.create(fx)

cat("GC-1d canonical_animal_id\n")
ci <- s33_load_canonical_id("Functions/behavioral_dynamics_helpers.R")
check(identical(ci$srcref_sha256, S33_CODE_PINS$canonical_id_srcref_sha256) && isTRUE(ci$probe_ok), "the repository helper passes")
src <- readLines("Functions/behavioral_dynamics_helpers.R", warn = FALSE)
k <- grep("^canonical_animal_id <- function", src)
tam <- src; tam[k + 2L] <- sub("toupper", "tolower", tam[k + 2L], fixed = TRUE)
writeLines(tam, file.path(fx, "bdh.R"))
ct <- s33_load_canonical_id(file.path(fx, "bdh.R"))
check(!identical(ct$srcref_sha256, S33_CODE_PINS$canonical_id_srcref_sha256) && !isTRUE(ct$probe_ok), "a tampered copy fails hash and probe")
writeLines(c(src, "canonical_animal_id <- function(x) x"), file.path(fx, "bdh2.R"))
check(inherits(tryCatch(s33_load_canonical_id(file.path(fx, "bdh2.R")), error = function(e) e), "error"), "two assignments are refused")
ok("pass and fail")

cat("\nGC-2 plan LF sha256\n")
lf <- "# plan\n**Status.** FROZEN before the first run.\nline\n"
writeBin(charToRaw(lf), file.path(fx, "plan_lf.md")); writeBin(charToRaw(gsub("\n", "\r\n", lf)), file.path(fx, "plan_crlf.md"))
writeBin(charToRaw(sub("line", "edited", lf)), file.path(fx, "plan_edit.md"))
a <- s33_plan_sha_lf(file.path(fx, "plan_lf.md")); b <- s33_plan_sha_lf(file.path(fx, "plan_crlf.md")); e <- s33_plan_sha_lf(file.path(fx, "plan_edit.md"))
check(identical(a$sha256, b$sha256) && !identical(a$sha256, e$sha256), "LF and CRLF agree; an edit differs")
check(identical(s33_plan_sha_lf(S33_PLAN$path)$sha256, S33_PLAN$sha256_lf), "the repository plan matches S33_PLAN")
ok("pass and fail")

cat("\nGC-3 packages\n")
check(isTRUE(s33_package_gates(c(data.table = as.character(packageVersion("data.table"))), R.version.string)$passed), "installed versions pass")
check(!isTRUE(s33_package_gates(c(data.table = "0.0.1"), R.version.string)$passed), "a wrong version fails")
check(!isTRUE(s33_package_gates(c(nonexistentpkg = "1.0"), R.version.string)$passed), "a missing package fails (NA)")
check(!isTRUE(s33_package_gates(c(data.table = as.character(packageVersion("data.table"))), "R version 0.0")$passed), "a wrong R version fails")
ok("pass and fail")

cat("\nGC-5 inputs\n")
writeLines("a", file.path(fx, "i1.csv")); writeLines("b", file.path(fx, "i2.csv"))
spec <- data.table(role = c("i1", "i2", "i3"), path = file.path(fx, c("i1.csv", "i2.csv", "i3.csv")), module = "core",
                   sha256 = c(digest::digest(file = file.path(fx, "i1.csv"), algo = "sha256"), strrep("0", 64), strrep("1", 64)))
g <- s33_input_gates(spec)
check(isTRUE(s33_input_gates(spec[1])$gate$passed), "a matching input passes")
check(!isTRUE(g$gate$passed) && identical(g$table$passed, c(TRUE, FALSE, FALSE)) && is.na(g$table$observed_sha256[3]), "a changed and a missing input fail")
ok("pass and fail")

cat("\nGC-6 planning parsers and readers\n")
canon <- ci$fun
gm <- matrix(NA_character_, 40, 45)
ids <- matrix(sprintf("HKH0-OQ%03d", 701:716), 4, 4)
for (rd in 1:4) for (gi in 1:4) { rows <- c(13, 19, 25, 31)[rd] + 0:3; col <- c(2, 11, 20, 29)[gi]
  perm <- ((seq_len(4) + rd + gi) %% 4) + 1L; gm[rows, col] <- ids[, gi][perm]; gm[rows, col + 4] <- "SHKH0-00001" }
gm[13:16, 40] <- sprintf("HKH0-OQ%03d", 760:763)
gp <- s33_parse_golfer_plan(gm, canon)
check(nrow(gp) == 68L && sum(gp$condition == "CON") == 4L && all(gp[condition == "SIS", data.table::uniqueN(AnimalNum), by = CC]$V1 == 16L),
      "64 SIS rows in 4 rounds of 16 and 4 CON rows")
check(identical(sort(unique(gp$AnimalNum[gp$condition == "SIS"])), sort(canon(sprintf("OQ%03d", 701:716)))), "planning prefixes stripped")
g2 <- matrix(NA_character_, 30, 20); g2[10, 13:16] <- c("ID", "Cage", "Cage#", "Stress")
g2[11:30, 13] <- c(sprintf("HKH0-OR%03d", 101:116), sprintf("HKH0-OR%03d", 126:129)); g2[11:30, 14] <- "SHKH0-01414"; g2[11:30, 16] <- "SIS"
check(nrow(s33_parse_groupcomp_b2(g2, canon)) == 20L, "GroupCompBatch2: 20 rows under the header in row 10")
g2b <- g2; g2b[10, 16] <- "Group"
check(inherits(tryCatch(s33_parse_groupcomp_b2(g2b, canon), error = function(e) e), "error"), "a moved header stops")
g3 <- matrix(NA_character_, 12, 10); g3[1, 7:8] <- c("ID", "CAGE"); g3[2:5, 7] <- c("537", "HKH0-OR538", "0655", "OQ900"); g3[2:5, 8] <- "SHKH0-01615"
g3[6, 7] <- "537"; g3[6, 8] <- "SHKH0-01615"; g3[7, 7] <- "541"; g3[7, 8] <- "not a cage"
pc <- s33_parse_groupcomp_cols78(g3, canon)
check(identical(sort(pc$AnimalNum), sort(c("OR537", "OR538", "655", "OQ900"))), "bare 3-digit IDs become OR...; duplicates collapsed; non-cage rows dropped")
writeBin(charToRaw("HKH0-OR001\r\n\r\n 0655 \r\nOQ750\r\n"), file.path(fx, "list.csv"))
check(identical(s33_read_id_list(file.path(fx, "list.csv"), canon), c("OR001", "655", "OQ750")), "headerless CRLF ID list")
raw <- c("DateTime;Animal;RFID;AM;xPos;yPos;zPos", "garbage;x;1;1;1;1;1", "28.10.2022 15:44:45;OQ770-sys.2;1;1;1;1;1",
         "28.10.2022 15:45:00;OQ771-sys.5;1;1;1;1;1", "28.10.2022 16:00:00;OQ764-sys.3;1;1;1;1;1")
writeLines(raw, file.path(fx, "raw.csv"))
rs <- s33_recording_start(file.path(fx, "raw.csv"))
check(isTRUE(all.equal(rs$lag_h, as.numeric(difftime(as.POSIXct("2022-10-28 18:30:00", tz = "UTC"), as.POSIXct("2022-10-28 15:44:45", tz = "UTC"), units = "hours")))),
      "lag of the 18:30 anchor after the first parsable record")
rs2 <- s33_recording_start(file.path(fx, "raw.csv"), exclude_ids = "OQ764", canon = canon)
check(nrow(rs$boards) == 3L && nrow(rs2$boards) == 2L && isTRUE(all.equal(rs$board_first_delay_h, 15 / 60 + 15 / 3600)), "board first records; corrected labels left out")
ok("parsers, readers and recording start")

cat("\nGC-8 tables\n")
est <- s33_row("within_cc1_cage", "animal_within_cohort", estimate = 1, metric_id = "crossing_rate", units = "x", interval_basis = "conditional_on_cohorts")
res_ok <- list(D = list(tables = setNames(lapply(S33_TABLES$D, function(n) est), S33_TABLES$D),
                        audit = setNames(lapply(S33_AUDIT_TABLES$D, function(n) data.table(a = 1, tier = S33_TIER)), S33_AUDIT_TABLES$D)))
check(isTRUE(s33_table_gates(res_ok, "D")$passed), "declared tables with tier and section-7 columns pass")
res_bad <- res_ok; res_bad$D$tables[[1]] <- data.table(estimate = 1, tier = S33_TIER)
check(!isTRUE(s33_table_gates(res_bad, "D")$passed), "an estimate table without level/units/lead/basis fails")
res_bad2 <- res_ok; res_bad2$D$tables[[2]] <- NULL
check(!isTRUE(s33_table_gates(res_bad2, "D")$passed), "a missing declared table fails")
res_bad3 <- res_ok; res_bad3$D$audit[[1]][, tier := NULL]
check(!isTRUE(s33_table_gates(res_bad3, "D")$passed), "a table without tier fails")
ok("pass and fail")

cat("\ngate rows\n")
check(!isTRUE(s33_gate_rows("X", "x", NA)$passed) && !isTRUE(s33_gate_rows("X", "x", logical(), n_expected = 0L)$passed), "NA and vacuous gates fail")
check(isTRUE(s33_stop_on_gates(s33_gate_not_evaluated("X1", "x", "partner absent"))), "not-evaluated rows never stop")
ok("NA, empty, not evaluated")

unlink(fx, recursive = TRUE)
cat("\nPASS: stage 33 core gates\n")
