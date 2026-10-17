# Freeze a registry or registry addendum the way 29b v1.0, 29c v1.0 and Stage 32 v1.0 / A1 were frozen: the file, with its
# status line set to FROZEN <ISO time>, committed ALONE at HEAD; then a read-only S: copy of the committed (LF) bytes and a
# read-only sha file "# <title> - FROZEN <time> - MMMSociability commit <40>" + "<sha256>  <file>". Prints the constants the
# runner pins (sha256, freeze_commit, frozen_at). Refuses a dirty file, a HEAD commit that touches anything else, or a copy
# that already exists. Writes nothing without --write.
#
# Run from the repository root, e.g.
#   Rscript Analysis/_supporting/freeze_registry_copy.R docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.1.md canonical/posthoc_con_contrasts_registry/v1.1 REGISTRY_SHA256.txt "Post hoc CON contrasts registry v1.1" --write
#   Rscript Analysis/_supporting/freeze_registry_copy.R docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.1.md canonical/posthoc_movement_combz_fit_registry/v1.1 REGISTRY_SHA256.txt "Post hoc movement-CombZ display line registry v1.1" --write
#   Rscript Analysis/_supporting/freeze_registry_copy.R docs/STAGE32_REGISTRY_v1.0_ADDENDUM_A2.md canonical/stage32_registry/v1.0 ADDENDUM_A2_SHA256.txt "Stage 32 registry v1.0 addendum A2" --write
# (analysis_ready root: MMM_BEHAVIOR_PROJECT_ROOT/analysis_ready, default the S: RFID project.)

args <- commandArgs(TRUE); WRITE <- "--write" %in% args; args <- setdiff(args, "--write")
if (length(args) != 4L) stop("usage: freeze_registry_copy.R <repo_file> <canonical_rel_dir> <sha_file> \"<title>\" [--write]", call. = FALSE)
FILE <- args[1]; REL <- args[2]; SHAF <- args[3]; TITLE <- args[4]
git <- function(...) { out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE)); st <- attr(out, "status"); if (!is.null(st) && st != 0) stop("git ", paste(c(...), collapse = " "), ": ", paste(out, collapse = " "), call. = FALSE); out }
ROOT <- Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID")
DEST <- file.path(ROOT, "analysis_ready", REL)
head <- git("rev-parse", "HEAD")
stopifnot("run from the repository root" = file.exists(FILE), "the file is tracked" = length(git("ls-files", "--", FILE)) == 1L,
          "the file is committed unchanged" = !length(git("status", "--porcelain", "--untracked-files=no", "--", FILE)),
          "HEAD commits this file alone" = identical(git("show", "--name-only", "--format=", "HEAD"), FILE))
lf_file <- tempfile(); system2("git", c("cat-file", "blob", paste0("HEAD:", FILE)), stdout = lf_file)   # the committed bytes, unfiltered
stopifnot("the copied bytes are the committed blob" = identical(git("hash-object", "--no-filters", lf_file), git("rev-parse", paste0("HEAD:", FILE))))
bytes <- readBin(lf_file, "raw", file.size(lf_file)); stopifnot("LF only" = !any(bytes == as.raw(13L)))
sha <- unname(tools::sha256sum(lf_file))
st <- grep("^\\*\\*Status\\.\\*\\*", readLines(lf_file, warn = FALSE, encoding = "UTF-8"), value = TRUE)
at <- regmatches(st, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[+-][0-9]{4}", st))
if (length(st) != 1L || !grepl("^\\*\\*Status\\.\\*\\* FROZEN ", st) || length(at) != 1L)
  stop("the status line must read '**Status.** FROZEN <YYYY-MM-DDTHH:MM:SS+zzzz>, ...' in the committed file", call. = FALSE)
targets <- file.path(DEST, c(basename(FILE), SHAF))
if (any(file.exists(targets))) stop("already frozen (exists): ", paste(targets[file.exists(targets)], collapse = ", "), call. = FALSE)
cat(sprintf("%s\n  file %s at %s\n  sha256 (LF) %s\n  status %s\n  copy to %s\n", TITLE, FILE, substr(head, 1, 7), sha, at, DEST))
if (WRITE) {
  dir.create(DEST, recursive = TRUE, showWarnings = FALSE)
  writeBin(bytes, targets[1])
  writeBin(charToRaw(paste0(sprintf("# %s - FROZEN %s - MMMSociability commit %s\n%s  %s\n", TITLE, at, head, sha, basename(FILE)))), targets[2])   # LF, as the v1.0 copies
  stopifnot(identical(unname(tools::sha256sum(targets[1])), sha))
  Sys.chmod(targets, "0444"); stopifnot("read-only" = all(file.access(targets, 2L) != 0L))
  cat("written read-only:", paste(targets, collapse = "; "), "\n")
} else cat("dry run (add --write)\n")
cat(sprintf('constants:  sha256 = "%s", freeze_commit = "%s", frozen_at = "%s"\n', sha, head, at))
