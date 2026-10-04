# Frozen code identity: the code and registry files that the registered runs pinned are still identical to their pins,
# and .gitattributes keeps the line endings those pins depend on.
#
#   1. byte pins: S32_FROZEN_CODE_SHA256 (Functions/stage32_run.R), S30SC_CODE_SHA256 and the Stage 30 registry copies
#      (Functions/stage30_screen.R), and the frozen configuration R file (docs/STAGE29_RELEASE_v1.0.1_dv2.md);
#   2. .gitattributes gives every byte-pinned file the line endings of its pinned bytes;
#   3. the code set of each registered run is unchanged (git blob) since the commit that ran it;
#   4. the registry copies are unchanged since their freeze commits and match their registered LF-form sha256;
#   5. the CombZ producer still has the git blob that the figure support bundle pins.
#
# Portable: repository files and git only, plus the digest package. No S: access. Nothing is sourced: the pinned
# constants are read from the Functions/ files by evaluating only their assignments. Sections 3-5 need the full git
# history (CI: fetch-depth 0) and are skipped, with a message, outside a git work tree or in a shallow clone.
# Run it before any sweep that touches Functions/ or Analysis/.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

if (!requireNamespace("digest", quietly = TRUE)) fail("the digest package is required")
sha_bytes <- function(f) digest::digest(file = f, algo = "sha256")
sha_lf <- function(f) { b <- readBin(f, what = "raw", n = file.size(f)); digest::digest(b[b != as.raw(13L)], algo = "sha256", serialize = FALSE) }

# Evaluate only the named top-level assignments of a file; no other code in it runs.
pinned_constants <- function(file, names) {
  env <- new.env(parent = baseenv())
  for (e in as.list(parse(file, keep.source = FALSE))) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% names)
      assign(as.character(e[[2]]), eval(e[[3]], env), envir = env)
  }
  missing <- setdiff(names, ls(env, all.names = TRUE))
  if (length(missing)) fail(paste0(file, ": pinned constant(s) not found: ", paste(missing, collapse = ", ")))
  mget(names, envir = env)
}

s32 <- pinned_constants("Functions/stage32_run.R",
                        c("S32_FROZEN_CODE_SHA256", "S32_NUMERIC_CODE", "S32_REGISTRY", "S32_ADDENDUM", "S32_ABORTED"))
s30 <- pinned_constants("Functions/stage30_screen.R", c("S30SC_CODE_SHA256", "S30SC_REGISTRY_SHA256", "S30SC_REGISTRY_MD_SHA256"))
phc <- pinned_constants("Functions/posthoc_con_contrasts.R", "PHC_REGISTRY")$PHC_REGISTRY
fsb <- pinned_constants("Functions/figure_support_bundle.R", "FSB_COMBZ_CODE")$FSB_COMBZ_CODE

# ------------------------------------------------------------------ 1. byte pins
cat("1. pinned bytes\n")
shared <- intersect(names(s32$S32_FROZEN_CODE_SHA256), names(s30$S30SC_CODE_SHA256))
check(length(shared) == 4L && identical(s32$S32_FROZEN_CODE_SHA256[shared], s30$S30SC_CODE_SHA256[shared]),
      "S32_FROZEN_CODE_SHA256 and S30SC_CODE_SHA256 must pin the four shared files to the same sha256")
byte_pins <- c(s32$S32_FROZEN_CODE_SHA256, s30$S30SC_CODE_SHA256[setdiff(names(s30$S30SC_CODE_SHA256), shared)],
               "docs/stage30/stage30_registry_v1.0.json" = s30$S30SC_REGISTRY_SHA256,
               "docs/stage30/STAGE30_REGISTRY_v1.0.md" = s30$S30SC_REGISTRY_MD_SHA256,
               # frozen configuration v1.0.1 (docs/STAGE29_RELEASE_v1.0.1_dv2.md: R file f34c7624...)
               "Functions/behavior_analysis_config.R" = "f34c762488279f38322f649d2ac6398ebb1f820675436b610cd48417f98fcb2d")
for (f in names(byte_pins)) {
  check(file.exists(f), paste0("pinned file missing: ", f))
  check(identical(sha_bytes(f), unname(byte_pins[[f]])),
        paste0(f, ": sha256 ", substr(sha_bytes(f), 1, 8), " differs from its pin ", substr(byte_pins[[f]], 1, 8),
               " (an edit, or a re-checkout with other line endings)"))
}
ok(sprintf("%d byte-pinned files match their sha256", length(byte_pins)))

# ------------------------------------------------------------------ git helpers
git_ok <- nzchar(Sys.which("git"))
git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  if (!is.null(st) && st != 0L) stop(paste(c(paste("git", paste(c(...), collapse = " ")), out), collapse = "\n"), call. = FALSE)
  out
}
in_work_tree <- git_ok && identical(tryCatch(git("rev-parse", "--is-inside-work-tree")[1], error = function(e) ""), "true")

# ------------------------------------------------------------------ 2. line endings
cat("\n2. .gitattributes keeps the pinned line endings\n")
if (!in_work_tree) {
  message("SKIP section 2: not a git work tree")
} else {
  for (f in names(byte_pins)) {
    b <- readBin(f, what = "raw", n = file.size(f))
    want <- if (any(b == as.raw(13L))) "crlf" else "lf"
    got <- sub("^.*: eol: ", "", git("check-attr", "eol", "--", f)[1])
    check(identical(got, want), paste0(f, ": .gitattributes must set eol=", want, " (the pinned bytes); found ", got))
  }
  ok(sprintf("eol attribute matches the pinned bytes for all %d files", length(byte_pins)))
}

# ------------------------------------------------------------------ 3-5. git blobs
shallow <- in_work_tree && identical(tryCatch(git("rev-parse", "--is-shallow-repository")[1], error = function(e) "true"), "true")
if (!in_work_tree || shallow) {
  message("SKIP sections 3-5: ", if (!in_work_tree) "not a git work tree" else "shallow clone (fetch the full history)")
} else {
  blob_at <- function(commit, f) git("rev-parse", paste0(commit, ":", f))[1]
  blob_now <- function(f) git("hash-object", "--", f)[1]   # the working-tree file as git would store it
  unchanged_since <- function(commit, files, what) {
    for (f in files) {
      check(file.exists(f), paste0(what, ": file missing: ", f))
      check(identical(blob_now(f), blob_at(commit, f)),
            paste0(what, ": ", f, " differs from its state at ", substr(commit, 1, 7), " (git diff ", substr(commit, 1, 7), " -- ", f, ")"))
    }
    ok(sprintf("%s: %d files unchanged since %s", what, length(files), substr(commit, 1, 7)))
  }

  cat("\n3. code sets of the registered runs\n")
  rfid3 <- c("Functions/rfid_canonical_inference.R", "Functions/rfid_event_stream.R", "Functions/rfid_binfree_metrics.R")
  code_sets <- list(
    list(what = "Stage 29 v1.0.1 release (run v101_dv2_b2ce507)", commit = "b2ce5072ccbfd94e203fa7efa614dcc11e623337",
         files = c("Analysis/29_canonical_behavior_characterization.R", "Functions/behavior_analysis_config.R",
                   "Functions/behavior_config_identity.R", rfid3)),
    list(what = "Stage 30 (run v1.0_be71e2f)", commit = "be71e2f07bfefeeb13c1e1f82cbc62cff2e052dc",
         files = c("Analysis/30_exploratory_screen.R", "Functions/stage30_screen.R", "Functions/stage30_movement.R",
                   "Functions/behavior_analysis_config.R", rfid3, "docs/stage30/stage30_registry_v1.0.json")),
    list(what = "Stage 30 figure bundle (s30b_v10_20260929_5394f2f)", commit = "5394f2f3fb6c58d0e1f337bb22c33fdac2203365",
         files = c("Analysis/30b_stage30_figure_bundle.R", "Functions/stage30_figure_bundle.R")),
    list(what = "Stage 29b (run v1.0_7f1da1f)", commit = "7f1da1f6cea098a97cd0b06e3a37140826dda2f1",
         files = c("Analysis/29b_posthoc_con_contrasts.R", "Functions/posthoc_con_contrasts.R",
                   "Functions/rfid_canonical_inference.R", "Functions/stage30_figure_bundle.R")),
    list(what = "Stage 32 (run v1.1_f4c7a31)", commit = "f4c7a31dfa76486b5af2fdc5024b0c023f0e1921",
         files = c("Analysis/32_behavior_exposure_adaptation.R",
                   sprintf("Functions/stage32_%s.R", c("windows", "inference", "prediction", "reporting", "run")),
                   "Functions/stage30_movement.R", "Functions/stage30_figure_bundle.R", "Functions/behavior_analysis_config.R", rfid3)),
    list(what = "Stage 32 numeric code since the aborted execution", commit = s32$S32_ABORTED$commit, files = s32$S32_NUMERIC_CODE))
  for (cs in code_sets) unchanged_since(cs$commit, cs$files, cs$what)

  cat("\n4. registry copies\n")
  regs <- list(
    list(file = s32$S32_REGISTRY$repo_path, commit = s32$S32_REGISTRY$freeze_commit, sha = s32$S32_REGISTRY$sha256),
    list(file = s32$S32_REGISTRY$repo_csv, commit = s32$S32_REGISTRY$freeze_commit, sha = s32$S32_REGISTRY$csv_sha256),
    list(file = s32$S32_ADDENDUM$repo_path, commit = s32$S32_ADDENDUM$freeze_commit, sha = s32$S32_ADDENDUM$sha256),
    list(file = phc$repo_path, commit = phc$freeze_commit, sha = phc$sha256))
  for (r in regs) {
    unchanged_since(r$commit, r$file, paste("registry", basename(r$file)))
    check(identical(sha_lf(r$file), r$sha), paste0(r$file, ": LF-form sha256 differs from the registered ", substr(r$sha, 1, 8)))
  }
  ok(sprintf("%d registry copies match their registered LF-form sha256", length(regs)))

  cat("\n5. CombZ producer\n")
  check(identical(blob_now(fsb$path), fsb$git_blob),
        paste0(fsb$path, ": git hash-object differs from the pinned blob ", substr(fsb$git_blob, 1, 8)))
  check(identical(git("rev-parse", paste0("HEAD:", fsb$path))[1], fsb$git_blob),
        paste0(fsb$path, ": HEAD blob differs from the pinned blob ", substr(fsb$git_blob, 1, 8)))
  ok(paste0(fsb$path, " = pinned blob ", substr(fsb$git_blob, 1, 8), " (working tree and HEAD)"))
}

cat("\nPASS: frozen code identity\n")
