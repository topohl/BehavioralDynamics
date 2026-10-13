# ===========================================================================
# Append-only QC runs (Functions/qc_run_protection.R) and their use by Stage 00b. Portable: fixtures under tempdir();
# nothing on the lab drive is read or written.
#
#  1. run ledger: created with a header; a used run id is refused; a peer's appended line is accepted, a rewritten
#     earlier line is refused; earlier lines keep their bytes
#  2. protection snapshot: an added, removed or modified file (size, time stamp, or sha256 of a hashed file with the
#     same size and time stamp) is reported; an untouched tree reports nothing
#  3. output-root assertion: refused inside, equal to or around a protected location, also across drive-letter and UNC
#     spellings; a sibling is accepted
#  4. publish: refused (nothing renamed, ledger unchanged, the .tmp_ folder kept) when a protected file changed, when
#     the run folder exists (its bytes untouched), when the id is in the ledger, when another run holds the lock or when
#     earlier ledger lines were rewritten; a failed ledger append moves the run back; otherwise renamed with a ledger
#     line and an empty protection check
#  5. run configuration: an id in the ledger is refused even when its folder is gone; an unreadable ledger is refused
#  6. Stage 00b uses the publish step, the protection snapshot and the frozen-input guard, never renames by itself, and
#     stays out of the automatic runner
#  7. cage-episode keys on QC tables (System, CageEpisodeID); a vendor System label is kept
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "qc_run_protection.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
refused <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
source("Functions/qc_run_protection.R")
source("Functions/project_paths.R")
source("Functions/tracking_qc_run_config.R")
source("Functions/antenna_acquisition_qc.R")
sha <- function(f) digest::digest(file = f, algo = "sha256")
row <- function(id) list(run_id = id, finished = "2026-10-14 12:00:00", git_commit = "abc", rules_version = "v", qc_status = "PASS",
                         isolated = TRUE, recordings = "B1_CC1", output_manifest_sha256 = "x", protected_files = 1, protection_changes = 0)

fx <- gsub("\\\\", "/", tempfile("qcprot_")); dir.create(fx)

# 1. ledger
base <- file.path(fx, "root"); dir.create(file.path(base, "runs"), recursive = TRUE)
check(nrow(mmm_qc_ledger_read(base)) == 0L && mmm_qc_ledger_state(base)$bytes == 0, "1: no ledger yet")
mmm_qc_ledger_append(base, row("r1"), mmm_qc_ledger_state(base))
L <- mmm_qc_ledger_read(base)
check(identical(L$run_id, "r1") && identical(names(L), MMM_QC_LEDGER_COLUMNS), "1: first line with a header")
st1 <- mmm_qc_ledger_state(base); bytes1 <- readBin(mmm_qc_ledger_path(base), "raw", 1e6)
check(refused(mmm_qc_ledger_append(base, row("r1"), st1)), "1: a used run id is refused")
mmm_qc_ledger_append(base, row("r2"), st1)                              # a peer run appended since st1: still accepted
mmm_qc_ledger_append(base, row("r2b"), st1)
bytes2 <- readBin(mmm_qc_ledger_path(base), "raw", 1e6)
check(identical(bytes2[seq_along(bytes1)], bytes1) && identical(mmm_qc_ledger_read(base)$run_id, c("r1", "r2", "r2b")), "1: earlier lines keep their bytes")
st2 <- mmm_qc_ledger_state(base)
txt <- readLines(mmm_qc_ledger_path(base)); writeLines(sub('"r1"', '"rX"', txt, fixed = TRUE), mmm_qc_ledger_path(base))
check(refused(mmm_qc_ledger_append(base, row("r2c"), st2)), "1: a rewritten earlier line is refused")
writeLines(txt, mmm_qc_ledger_path(base))

# 2. snapshot and comparison
pt <- file.path(fx, "protected"); dir.create(file.path(pt, "sub"), recursive = TRUE)
writeLines("a", file.path(pt, "a.txt")); writeLines("bbb", file.path(pt, "sub", "b.txt")); writeLines("keep", file.path(pt, "c.txt"))
hashed <- file.path(pt, "sub", "b.txt")
s0 <- mmm_qc_protection_snapshot(c(pt, file.path(fx, "absent")), hash = hashed)
check(nrow(mmm_qc_protection_compare(s0, mmm_qc_protection_snapshot(c(pt, file.path(fx, "absent")), hash = hashed))) == 0L, "2: untouched tree")
check(any(startsWith(s0$path, "<missing>")) && !is.na(s0$sha256[s0$path == hashed]), "2: a missing location is recorded; hashed files carry sha256")
s1 <- s0; s1$sha256[s1$path == hashed] <- "0000"                       # only the bytes of a hashed file differ
check(identical(mmm_qc_protection_compare(s0, s1)$change, "modified"), "2: a sha256-only change of a hashed file is reported")
mt <- file.mtime(hashed)
writeLines("bbc", hashed); invisible(Sys.setFileTime(hashed, mt))      # same size, other bytes
writeLines("new", file.path(pt, "d.txt")); invisible(file.remove(file.path(pt, "c.txt")))
writeLines("aa", file.path(pt, "a.txt"))
ch <- mmm_qc_protection_compare(s0, mmm_qc_protection_snapshot(c(pt, file.path(fx, "absent")), hash = hashed))
got <- setNames(ch$change, basename(ch$path))
check(identical(unname(got[c("a.txt", "b.txt", "c.txt", "d.txt")]), c("modified", "modified", "removed", "added")), "2: modified, sha-only, removed, added")

# 3. output-root assertion
check(refused(mmm_qc_assert_target(file.path(pt, "x"), pt)), "3: inside a protected location")
check(refused(mmm_qc_assert_target(pt, pt)), "3: equal to a protected location")
check(refused(mmm_qc_assert_target(fx, pt)), "3: around a protected location")
check(isTRUE(mmm_qc_assert_target(file.path(fx, "root"), pt)), "3: a sibling is accepted")
unc <- "//mdc-berlin.net/fs/AG_Hoernberg/Lab_Member/Tobi/E/RFID/analysis_ready/analyses/hmm_states"
drv <- "S:/Lab_Member/Tobi/E/RFID/analysis_ready/analyses/hmm_states"
check(refused(mmm_qc_assert_target(paste0(unc, "/x"), drv)), "3: a UNC base inside a drive-letter protected location")
check(refused(mmm_qc_assert_target(paste0(drv, "/x"), unc)), "3: a drive-letter base inside a UNC protected location")
check(refused(mmm_qc_assert_target("//mdc-berlin.net/fs/AG_Hoernberg/Lab_Member/Tobi/E/RFID", drv)), "3: a UNC base around a drive-letter protected location")
check(isTRUE(mmm_qc_assert_target("S:/Lab_Member/Tobi/E/RANM/qc", "//mdc-berlin.net/fs/AG_Hoernberg/Lab_Member/Tobi/E/RFID/analysis_ready")),
      "3: a drive-letter sibling of a UNC protected location is accepted")

# 4. publish
prot <- file.path(fx, "inputs"); dir.create(prot); writeLines("x", file.path(prot, "in.csv"))
mk_tmp <- function(id) { d <- file.path(base, "runs", paste0(".tmp_", id)); dir.create(file.path(d, "manifest"), recursive = TRUE); writeLines("out", file.path(d, "o.csv")); d }
before <- mmm_qc_protection_snapshot(prot)
hL <- mmm_qc_ledger_state(base); hL_sha <- sha(mmm_qc_ledger_path(base))
#   a protected file changed during the run
t3 <- mk_tmp("r3"); writeLines("changed", file.path(prot, "in.csv"))
check(refused(mmm_qc_publish(t3, file.path(base, "runs", "r3"), base, row("r3"), before, prot, character(), hL)), "4: refused after a protected change")
check(dir.exists(t3) && !dir.exists(file.path(base, "runs", "r3")) && identical(sha(mmm_qc_ledger_path(base)), hL_sha), "4: nothing published")
check(nrow(read.csv(file.path(t3, "manifest", "protection_check.csv"))) == 1L, "4: the change is written to the run's protection check")
unlink(t3, recursive = TRUE)
before <- mmm_qc_protection_snapshot(prot)
#   the run folder exists
dir.create(file.path(base, "runs", "r4")); writeLines("earlier", file.path(base, "runs", "r4", "e.csv")); he <- sha(file.path(base, "runs", "r4", "e.csv"))
t4 <- mk_tmp("r4")
check(refused(mmm_qc_publish(t4, file.path(base, "runs", "r4"), base, row("r4"), before, prot, character(), hL)), "4: refused onto an existing folder")
check(identical(sha(file.path(base, "runs", "r4", "e.csv")), he) && !file.exists(file.path(base, "runs", "r4", "o.csv")) && dir.exists(t4),
      "4: the existing folder is untouched")
unlink(t4, recursive = TRUE)
#   the id is in the ledger
t1 <- mk_tmp("r1")
check(refused(mmm_qc_publish(t1, file.path(base, "runs", "r1"), base, row("r1"), before, prot, character(), hL)), "4: refused for a used id")
check(!dir.exists(file.path(base, "runs", "r1")), "4: nothing renamed")
unlink(t1, recursive = TRUE)
#   another run holds the lock
t6 <- mk_tmp("r6"); dir.create(file.path(base, "runs_ledger.lock"))
check(refused(mmm_qc_publish(t6, file.path(base, "runs", "r6"), base, row("r6"), before, prot, character(), hL)) && dir.exists(t6) &&
        !dir.exists(file.path(base, "runs", "r6")), "4: refused while another run holds the lock")
unlink(file.path(base, "runs_ledger.lock"), recursive = TRUE)
#   earlier ledger lines rewritten during the run
txt <- readLines(mmm_qc_ledger_path(base)); writeLines(sub('"r1"', '"rX"', txt, fixed = TRUE), mmm_qc_ledger_path(base))
check(refused(mmm_qc_publish(t6, file.path(base, "runs", "r6"), base, row("r6"), before, prot, character(), hL)) && dir.exists(t6),
      "4: refused when earlier ledger lines changed")
writeLines(txt, mmm_qc_ledger_path(base)); unlink(t6, recursive = TRUE)
#   a ledger append that fails after the rename moves the run back
t7 <- mk_tmp("r7"); bad <- row("r7"); bad$run_id <- NA_character_
check(refused(mmm_qc_publish(t7, file.path(base, "runs", "r7"), base, bad, before, prot, character(), hL)), "4: a failed append stops")
check(dir.exists(t7) && !dir.exists(file.path(base, "runs", "r7")) && !dir.exists(file.path(base, "runs_ledger.lock")),
      "4: the run is moved back and the lock released")
unlink(t7, recursive = TRUE)
#   success
t5 <- mk_tmp("r5")
out <- mmm_qc_publish(t5, file.path(base, "runs", "r5"), base, row("r5"), before, prot, character(), hL)
check(dir.exists(out) && !dir.exists(t5) && file.exists(file.path(out, "o.csv")), "4: published by renaming")
check(identical(tail(mmm_qc_ledger_read(base)$run_id, 1), "r5") && nrow(read.csv(file.path(out, "manifest", "protection_check.csv"))) == 0L,
      "4: ledger line and an empty protection check")
check(identical(tail(mmm_qc_ledger_read(base)$protection_changes, 1), "0"), "4: the ledger records zero protection changes")

# 5. run configuration refuses an id from the ledger
root <- file.path(fx, "Exp", "Analysis", "Behavior", "RFID"); dir.create(root, recursive = TRUE)
iso <- file.path(fx, "iso"); dir.create(file.path(iso, "runs"), recursive = TRUE)
mmm_qc_ledger_append(iso, row("gone"), mmm_qc_ledger_state(iso))
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=gone", paste0("--output-root=", iso)))), "5: a ledger id is refused without its folder")
check(identical(mmm_raw_acquisition_qc_run_config(root, c("--run-id=fresh", paste0("--output-root=", iso)))$run_id, "fresh"), "5: a fresh id passes")
iso2 <- file.path(fx, "iso2"); dir.create(file.path(iso2, "runs"), recursive = TRUE); writeLines("garbage", file.path(iso2, "runs_ledger.csv"))
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=fresh", paste0("--output-root=", iso2)))), "5: an unreadable ledger is refused")

# 6. Stage 00b
code <- readLines("Analysis/00b_qc_raw_acquisition.R", warn = FALSE)
code <- code[!grepl("^\\s*#", code)]
for (call in c("mmm_frozen_guard_before(\"00b\"", "mmm_frozen_guard_after(", "mmm_qc_publish(", "mmm_qc_protection_snapshot(", "mmm_qc_assert_target("))
  check(any(grepl(call, code, fixed = TRUE)), paste("6: Stage 00b calls", call))
check(!any(grepl("file.rename(", code, fixed = TRUE)), "6: Stage 00b publishes only through mmm_qc_publish()")
check(any(grepl("Functions/qc_run_protection.R", code, fixed = TRUE)) && any(grepl("Functions/frozen_input_guard.R", code, fixed = TRUE)),
      "6: the protection code is part of the run's code hashes")
check(!any(grepl("00b_qc_raw_acquisition", readLines("Analysis/run_all_analysis.R", warn = FALSE))), "6: not in the automatic runner")

# 7. cage-episode keys
d <- data.table(SourceFile = "f", Batch = c("B2", "B2"), CC = "CC3", board = c(2L, NA))
k <- aqc_episode_keys(d)
check(identical(k$System, c("sys.2", NA)) && identical(k$CageEpisodeID, c("B2|sys.2|CC3", NA)), "7: keys from the board")
k2 <- aqc_episode_keys(data.table(Batch = "B1", CC = "CC2", board = 3L, System = "sys.3"))
check(identical(k2$System, "sys.3") && identical(k2$CageEpisodeID, "B1|sys.3|CC2"), "7: an existing System label is kept")
check(identical(names(aqc_episode_keys(data.table(Phase = "Active", x = 1))), c("Phase", "x")), "7: tables without board keys are unchanged")

unlink(fx, recursive = TRUE)
cat("test_qc_run_protection: all checks passed\n")
