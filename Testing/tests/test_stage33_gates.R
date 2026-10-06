# Stage 33 core gates: a passing and a failing fixture for each, including NA and empty inputs.
#
#   GC-1d  canonical_animal_id taken by parse(): srcref sha256 and probe (the repository helper and a tampered copy)
#   GC-2   the plan's LF sha256 (LF and CRLF copies agree; an edited copy differs)
#   GC-3   package versions (the installed set and a wrong expectation)
#   GC-5   input sha256 (a matching, a changed and a missing file)
#   GC-6   the planning parsers on in-memory sheets (Social Golfer plan, GroupCompBatch2, GroupComposition) and the
#          ID-list reader; the recording start of a synthetic raw file
#   GC-8   declared tables, tier column, section-7 columns and levels
#   registry  every S33_GATES id passes on a passing fixture and fails on a failing, an NA and an empty one; GC-8x
#          (every registered Phase 1-4 gate recorded; partial runs; unregistered ids)
#   X1-X8  cross-module gates on consistent synthetic modules, a targeted failure for each, partial runs (not evaluated),
#          a missing export, X8 recorded; s33_cross_fill() places C's S3 beside E's S15
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

cat("\nregistry (S33_GATES) and GC-8x\n")
check(!anyDuplicated(S33_GATES$gate_id) && all(S33_GATES$kind %in% c("hard", "recorded")) && all(S33_GATES$phase %in% 1:5) &&
        identical(S33_GATES[kind == "recorded", gate_id], c("X8", "GC-11")), "registry ids unique; X8 and GC-11 recorded")
for (id in S33_GATES$gate_id) {
  check(isTRUE(s33_gate_rows(id, "fixture", c(TRUE, TRUE))$passed), paste(id, "passes on a passing fixture"))
  check(!isTRUE(s33_gate_rows(id, "fixture", c(TRUE, FALSE))$passed) && !isTRUE(s33_gate_rows(id, "fixture", c(TRUE, NA))$passed) &&
          !isTRUE(s33_gate_rows(id, "fixture", logical(), n_expected = 0L)$passed), paste(id, "fails on a failing, an NA and an empty fixture"))
}
rec <- function(ids) rbindlist(lapply(ids, function(id) s33_gate_rows(id, "fixture", TRUE)))
full <- S33_GATES[phase <= 4L & gate_id != "GC-8x", gate_id]
check(isTRUE(s33_registry_gate(rec(full), LETTERS[1:5])$passed), "GC-8x passes when every Phase 1-4 gate was recorded")
check(!isTRUE(s33_registry_gate(rec(setdiff(full, "X3")), LETTERS[1:5])$passed), "GC-8x fails when a cross-module gate is missing")
check(!isTRUE(s33_registry_gate(rec(c(full, "GC-6z")), LETTERS[1:5])$passed), "GC-8x fails on an unregistered core id")
check(isTRUE(s33_registry_gate(rec(setdiff(full, paste0("GC-8", c("A", "B", "C", "E")))), "D")$passed), "a partial run needs only its own GC-8 rows")
check(isTRUE(s33_registry_gate(rbind(rec(full), rec("GD-9")), LETTERS[1:5])$passed), "module gate ids are not core ids")
ok("every registered id: pass, fail, NA, empty; GC-8x completeness")

cat("\ncross-module gates X1-X8 and the S15 fill\n")
co <- S33_COHORTS; lagv <- c(B1 = 2.5, B2 = 3, B3 = 4, B4 = 5, B5 = 6, B6 = 2.75)
ani <- data.table(AnimalNum = sprintf("A%03d", 1:85), Batch = rep(co, length.out = 85), tp2 = seq(20, 28, length.out = 85),
                  src_cage = rep(paste0("S", 1:17), each = 5), cc4_cage = rep(paste0("K", 1:17), 5))
xr <- data.table(Batch = co, estimate = 1:6, cr2_cc1_se = 0.1 * (1:6), cr2_cc1_df = 2 + (1:6) / 10, cr2_cc1_ci_low = (1:6) - 1, cr2_cc1_ci_high = (1:6) + 1)
cages <- paste0("C", 1:22); xl <- data.table(AnimalNum = ani$AnimalNum, xbar_loo = seq(5, 15, length.out = 85))
# built fresh for every fixture: data.table's := changes a table in place
mkX <- function() list(
  A = list(tables = list(), cross = list(lags = data.table(Batch = co, lag_cc1_h = unname(lagv[co])), x_RU_sis_cc1_rows = xr,
                                         cage_sd_reference_cc1_cages = cages, cc4_cage = ani[, .(AnimalNum, cc4_cage)], tp2 = ani[, .(AnimalNum, tp2, src_cage)])),
  B = list(tables = list(b03_balance_cohort = data.table(Batch = rep(co, each = 2), condition = c("CON", "SIS"), lag_h_cc1 = rep(unname(lagv[co]), each = 2)),
                         b01_covariates_animal = ani[, .(AnimalNum, Batch, tp2, src_cage, cc4_cage, lag_h_cc1 = unname(lagv[Batch]))])),
  C = list(tables = list(), cross = list(focal = ani$AnimalNum, cages = cages, tp2 = ani[, .(AnimalNum, tp2)], src_cage = ani[, .(AnimalNum, src_cage)],
                                         s1 = S33_SD_RATE, xbar_loo = xl, s3_delta_peer_per_sd = 0.25, s3_singular = TRUE)),
  D = list(tables = list(d08_recording_start_lags = data.table(Batch = rep(co, 4), CC = rep(S33_CC, each = 6), lag_h = rep(unname(lagv[co]), 4)),
                         d05_cohort_cc_table = data.table(Batch = co, CC = "CC1", exposure_set = "SIS", metric = "crossing_rate", mean = 6 * xr$estimate,
                                                          se_cr2 = 6 * xr$cr2_cc1_se, df_satt = xr$cr2_cc1_df, ci_low = 6 * xr$cr2_cc1_ci_low,
                                                          ci_high = 6 * xr$cr2_cc1_ci_high))),
  E = list(tables = list(e01_exposures_animal = ani[, .(AnimalNum, tp2, src_cage, focal = TRUE, s1 = S33_SD_RATE, m1 = xl$xbar_loo / S33_SD_RATE)],
                         e06_sensitivities = data.table(sensitivity_id = c("S1", "S15"), estimate = c(0.1, 0.25), c_s3_delta_peer_per_frozen_sd = NA_real_))))
resX <- mkX()
xg <- s33_cross_gates(resX, list(), LETTERS[1:5])
check(identical(xg$gate_id, paste0("X", 1:8)) && all(xg$passed) && all(xg$evaluated) && identical(xg[gate_id == "X8", hard], FALSE) &&
        all(xg[gate_id != "X8", hard]), "all eight pass on consistent modules; X8 recorded, X1-X7 hard")
bad <- function(f) s33_cross_gates(f(mkX()), list(), LETTERS[1:5])
passed_of <- function(g, id) isTRUE(g[gate_id == id, passed])
check(!passed_of(bad(function(r) { r$D$tables$d08_recording_start_lags[CC == "CC1" & Batch == "B3", lag_h := 4.01]; r }), "X1"), "X1 fails on a changed lag")
check(!passed_of(bad(function(r) { r$B$tables$b01_covariates_animal[1, lag_h_cc1 := 9]; r }), "X1"), "X1 fails when b01 and b03 disagree")
check(!passed_of(bad(function(r) { r$D$tables$d05_cohort_cc_table[Batch == "B2", ci_high := ci_high + 1e-6]; r }), "X2"), "X2 fails on a changed CR2 limit")
check(!passed_of(bad(function(r) { r$A$cross$cage_sd_reference_cc1_cages <- cages[-1]; r }), "X3"), "X3 fails on a different cage set")
check(!passed_of(bad(function(r) { r$E$tables$e01_exposures_animal[1, focal := FALSE]; r }), "X3"), "X3 fails on a different focal set")
check(!passed_of(bad(function(r) { r$B$tables$b01_covariates_animal[5, tp2 := tp2 + 0.5]; r }), "X4"), "X4 fails on a changed tp2")
check(!passed_of(bad(function(r) { r$E$tables$e01_exposures_animal[7, src_cage := "S99"]; r }), "X4"), "X4 fails on a changed source cage")
check(!passed_of(bad(function(r) { r$A$cross$cc4_cage <- data.table::copy(r$A$cross$cc4_cage)[3, cc4_cage := "K99"]; r }), "X5"), "X5 fails on a changed cc4_cage")
check(!passed_of(bad(function(r) { r$E$tables$e01_exposures_animal[, s1 := 5.1699]; r }), "X6"), "X6 fails on a different s1")
check(!passed_of(bad(function(r) { r$E$tables$e01_exposures_animal[2, m1 := m1 + 1e-9]; r }), "X7"), "X7 fails beyond 1e-10")
g8 <- bad(function(r) { r$E$tables$e06_sensitivities[sensitivity_id == "S15", estimate := 0.26]; r })
check(!passed_of(g8, "X8") && all(g8[gate_id != "X8", passed]), "X8 differs when S3 is singular (recorded)")
check(isTRUE(s33_stop_on_gates(g8)), "a failing X8 does not stop the run")
g8b <- bad(function(r) { r$C$cross$s3_singular <- FALSE; r$E$tables$e06_sensitivities[sensitivity_id == "S15", estimate := 0.26]; r })
check(passed_of(g8b, "X8") && grepl("not singular", g8b[gate_id == "X8", detail]), "X8 records the difference when S3 is not singular")
gm <- bad(function(r) { r$A$cross$lags <- NULL; r })
check(!passed_of(gm, "X1") && grepl("^error:", gm[gate_id == "X1", detail]), "a missing export fails its gate with the error as detail")
gD <- s33_cross_gates(resX["D"], list(), "D")
check(nrow(gD) == 8L && all(!gD$evaluated) && isTRUE(s33_stop_on_gates(gD)), "a D-only run: every cross gate not evaluated")
gAD <- s33_cross_gates(resX[c("A", "D")], list(), c("A", "D"))
check(all(gAD[gate_id %in% c("X1", "X2"), passed]) && all(!gAD[!gate_id %in% c("X1", "X2"), evaluated]) &&
        grepl("evaluated over A,D", gAD[gate_id == "X1", detail]), "an A+D run evaluates X1 (over A, D) and X2 only")
filled <- s33_cross_fill(resX, LETTERS[1:5])$E$tables$e06_sensitivities
check(filled[sensitivity_id == "S15", c_s3_delta_peer_per_frozen_sd] == 0.25 && is.na(filled[sensitivity_id == "S1", c_s3_delta_peer_per_frozen_sd]) &&
        is.na(resX$E$tables$e06_sensitivities[sensitivity_id == "S15", c_s3_delta_peer_per_frozen_sd]), "S15 fill: C's S3 beside E's S15 only; input unchanged")
check(identical(s33_cross_fill(resX[c("D", "E")], c("D", "E")), resX[c("D", "E")]) && all(xg$passed), "no fill without C; the fixture is unchanged")
ok("X1-X8 pass and fail fixtures, partial runs, missing exports, S15 fill")

cat("\ngate row types\n")
must_err <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
check(must_err(s33_gate_rows("X", "x", TRUE, "a detail passed by position")) && must_err(s33_gate_rows("X", "x", TRUE, hard = NA)),
      "a detail passed by position (landing in hard) or an NA hard flag stops")
g3 <- s33_package_gates(expected = c(data.table = "0.0.0"), r_version = "none")
check(isTRUE(g3$hard) && !isTRUE(g3$passed) && nzchar(g3$detail) && isTRUE(tryCatch(s33_stop_on_gates(g3), error = function(e) FALSE) == FALSE),
      "GC-3 is a hard gate whose failure stops the run (detail named)")
gg <- rbindlist(list(s33_gate_rows("A", "a", TRUE), s33_gate_not_evaluated("B", "b", "x"), s33_package_gates()), fill = TRUE)
check(is.logical(gg$hard) && is.logical(gg$passed) && is.logical(gg$evaluated), "combined gate rows keep logical flags")
ok("hard and evaluated are logical; GC-3 stops")

cat("\ngate rows\n")
check(!isTRUE(s33_gate_rows("X", "x", NA)$passed) && !isTRUE(s33_gate_rows("X", "x", logical(), n_expected = 0L)$passed), "NA and vacuous gates fail")
check(isTRUE(s33_stop_on_gates(s33_gate_not_evaluated("X1", "x", "partner absent"))), "not-evaluated rows never stop")
ok("NA, empty, not evaluated")

unlink(fx, recursive = TRUE)
cat("\nPASS: stage 33 core gates\n")
