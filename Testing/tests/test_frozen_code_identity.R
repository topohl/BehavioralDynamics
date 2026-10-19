# Frozen code identity: the code and registry files that the registered runs pinned are still identical to their pins,
# and .gitattributes keeps the line endings those pins depend on.
#
#   1. byte pins: S32_FROZEN_CODE_SHA256 (Functions/stage32_run.R), S30SC_CODE_SHA256 and the Stage 30 registry copies
#      (Functions/stage30_screen.R), and the frozen configuration R file of the current release;
#   2. .gitattributes gives every byte-pinned file the line endings of its pinned bytes;
#   3. the code set of each registered run is unchanged (git blob) since the commit that ran it, unless the run is
#      superseded by a new registered version: then its code must still be recoverable at its run commit;
#   4. the registry copies are unchanged since their freeze commits and match their registered LF-form sha256;
#   5. the CombZ producer still has the git blob that the figure support bundle pins.
#
# Corrected-outcome rerun (2026-10-17): the successors (config v1.0.2, Stage 29 v102, 29b/29c v1.1, Stage 30 v1.1 and
# s30b_v11, Stage 32 v1.2) are prepared before they run. Their pins read "PHASE-B" until then; such a pin is listed as
# pending, not failed, and becomes a check as soon as its value is filled in. After Phase B nothing may be pending.
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
pmf <- pinned_constants("Functions/posthoc_movement_combz_fit.R", "PMF_REGISTRY")$PMF_REGISTRY
fsb <- pinned_constants("Functions/figure_support_bundle.R", "FSB_COMBZ_CODE")$FSB_COMBZ_CODE

PENDING <- "PHASE-B"
pending <- character()
note_pending <- function(what) { pending <<- c(pending, what); cat("  pending (Phase B):", what, "\n") }
# The frozen configuration R file (LF bytes; .gitattributes eol=lf). v1.0.1: f34c7624... (docs/STAGE29_RELEASE_v1.0.1_dv2.md),
# checked at its release commit in section 3. v1.0.2: written by the freeze (docs/STAGE29_RELEASE_v1.0.2_ov2.md).
CONFIG_R <- list(version = "1.0.2", sha256 = "491d41464a0098538a3ebb68899ec869e0b4c345e54473de90de29e250686bb7")  # RETRACK-PIN
CONFIG_R_V101 <- list(commit = "b2ce5072ccbfd94e203fa7efa614dcc11e623337", sha256 = "f34c762488279f38322f649d2ac6398ebb1f820675436b610cd48417f98fcb2d")

# ------------------------------------------------------------------ 1. byte pins
cat("1. pinned bytes\n")
shared <- intersect(names(s32$S32_FROZEN_CODE_SHA256), names(s30$S30SC_CODE_SHA256))
check(length(shared) == 4L && identical(s32$S32_FROZEN_CODE_SHA256[shared], s30$S30SC_CODE_SHA256[shared]),
      "S32_FROZEN_CODE_SHA256 and S30SC_CODE_SHA256 must pin the four shared files to the same sha256")
# The Stage32 pin on this exporter belongs to its retained historical run.
# Current exporter bytes are frozen by the new Stage30b/29b/29c run at5482c43.
byte_pins <- c(s32$S32_FROZEN_CODE_SHA256[setdiff(names(s32$S32_FROZEN_CODE_SHA256), "Functions/stage30_figure_bundle.R")],
               "Functions/stage30_figure_bundle.R" = "15509cb1477b32885747637cff5f756f2bafb786a00d776d880b9f5daf08ce47",
               s30$S30SC_CODE_SHA256[setdiff(names(s30$S30SC_CODE_SHA256), shared)],
               # Stage 30 registry v1.0 (frozen; superseded by v1.1, never edited)
               "docs/stage30/stage30_registry_v1.0.json" = "5c252bf699bc0f517c5508854d710bbaa014d4b6fe98d128eb0071bde89dd05e",
               "docs/stage30/STAGE30_REGISTRY_v1.0.md" = "e12b390b3c8bfadd32e5d2aae319f6053c0875893a41b3cf3bd60553d5bdf531",
               "docs/stage30/stage30_registry_v1.1.json" = s30$S30SC_REGISTRY_SHA256,
               "docs/stage30/STAGE30_REGISTRY_v1.1.md" = s30$S30SC_REGISTRY_MD_SHA256,
               "Functions/behavior_analysis_config.R" = "491d41464a0098538a3ebb68899ec869e0b4c345e54473de90de29e250686bb7")
for (f in names(byte_pins)[byte_pins == PENDING]) note_pending(paste("byte pin of", f))
byte_pins <- byte_pins[byte_pins != PENDING]
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
  set29 <- c("Analysis/29_canonical_behavior_characterization.R", "Functions/behavior_analysis_config.R", "Functions/behavior_config_identity.R", rfid3)
  set30 <- c("Analysis/30_exploratory_screen.R", "Functions/stage30_screen.R", "Functions/stage30_movement.R", "Functions/behavior_analysis_config.R", rfid3)
  set30b <- c("Analysis/30b_stage30_figure_bundle.R", "Functions/stage30_figure_bundle.R")
  set29b <- c("Analysis/29b_posthoc_con_contrasts.R", "Functions/posthoc_con_contrasts.R", "Functions/rfid_canonical_inference.R", "Functions/stage30_figure_bundle.R")
  set29c <- c("Analysis/29c_posthoc_movement_combz_fit.R", "Functions/posthoc_movement_combz_fit.R", "Functions/posthoc_con_contrasts.R", "Functions/stage30_figure_bundle.R")
  set32 <- c("Analysis/32_behavior_exposure_adaptation.R", sprintf("Functions/stage32_%s.R", c("windows", "inference", "prediction", "reporting", "run")),
             "Functions/stage30_movement.R", "Functions/stage30_figure_bundle.R", "Functions/behavior_analysis_config.R", rfid3)
  # Historical run source remains recoverable; Stage32 is retained outside this figure rerun.
  superseded <- list(
    list(what = "Stage 29 v1.0.1 release (run v101_dv2_b2ce507)", by = "config v1.0.2 release v102", commit = "b2ce5072ccbfd94e203fa7efa614dcc11e623337", files = set29),
    list(what = "Stage 30 (run v1.0_be71e2f)", by = "registry v1.1", commit = "be71e2f07bfefeeb13c1e1f82cbc62cff2e052dc", files = c(set30, "docs/stage30/stage30_registry_v1.0.json")),
    list(what = "Stage 30 figure bundle (s30b_v10_20260929_5394f2f)", by = "s30b_v11", commit = "5394f2f3fb6c58d0e1f337bb22c33fdac2203365", files = set30b),
    list(what = "Stage 29b (run v1.0_7f1da1f)", by = "registry v1.1", commit = "7f1da1f6cea098a97cd0b06e3a37140826dda2f1", files = set29b),
    list(what = "Stage 29c (run v1.0_fd61cd9)", by = "registry v1.1", commit = "fd61cd99787e9ce207a650b8c6a6f1ef3764ac30", files = set29c),
    list(what = "Stage 32 (run v1.1_f4c7a31)", by = "retained historical run; outside this Figure 1 rerun", commit = "f4c7a31dfa76486b5af2fdc5024b0c023f0e1921", files = set32))
  for (cs in superseded) {
    check(identical(attr(suppressWarnings(system2("git", c("merge-base", "--is-ancestor", cs$commit, "HEAD"), stdout = FALSE, stderr = FALSE)), "status"), NULL),
          paste0(cs$what, ": run commit ", substr(cs$commit, 1, 7), " is not in the history of HEAD"))
    for (f in cs$files) blob_at(cs$commit, f)   # stops if the file did not exist at the run commit
    ok(sprintf("%s: historical (%s); its %d files are recoverable at %s", cs$what, cs$by, length(cs$files), substr(cs$commit, 1, 7)))
  }
  historical_exporter <- paste0(paste(git("show", "f4c7a31dfa76486b5af2fdc5024b0c023f0e1921:Functions/stage30_figure_bundle.R"), collapse = "\n"), "\n")
  check(identical(digest::digest(historical_exporter, algo = "sha256", serialize = FALSE),
                  unname(s32$S32_FROZEN_CODE_SHA256[["Functions/stage30_figure_bundle.R"]])),
        "retained Stage32 run: historical exporter bytes differ from its frozen pin")
  check(identical(digest::digest(paste0(paste(git("show", paste0(CONFIG_R_V101$commit, ":Functions/behavior_analysis_config.R")), collapse = "\n"), "\n"),
                                 algo = "sha256", serialize = FALSE), CONFIG_R_V101$sha256),
        "config v1.0.1: Functions/behavior_analysis_config.R at the release commit differs from the frozen f34c7624")
  ok("config v1.0.1: the frozen R file (f34c7624) is the file at the v101 release commit")
  # registered and current: unchanged since the run commit (pending until the run exists)
  current <- list(
    list(what = "Stage 32 numeric code since the aborted execution", commit = s32$S32_ABORTED$commit, files = s32$S32_NUMERIC_CODE),
    list(what = "Stage 29 config v1.0.2 release (run v102_dv2_<commit7>)", commit = "a46be6a288032d97392a5f16f8eb81d7d1bd2890", files = set29),  # RETRACK-PIN
    list(what = "Stage 30 (run v1.1_<commit7>)", commit = "8f47527d3815d1305ff049a2f8189f8c4aaccd3c", files = c(set30, "docs/stage30/stage30_registry_v1.1.json")),  # RETRACK-PIN
    list(what = "Stage 30 figure bundle (s30b_v11)", commit = "5482c439f6408ac415276d16f1ac1d1b59e8fd0e", files = set30b),  # RETRACK-PIN
    list(what = "Stage 29b (run v1.1_<commit7>)", commit = "5482c439f6408ac415276d16f1ac1d1b59e8fd0e", files = set29b),  # RETRACK-PIN
    list(what = "Stage 29c (run v1.1_<commit7>)", commit = "5482c439f6408ac415276d16f1ac1d1b59e8fd0e", files = set29c))  # RETRACK-PIN
  for (cs in current) if (identical(cs$commit, PENDING)) note_pending(cs$what) else unchanged_since(cs$commit, cs$files, cs$what)

  cat("\n4. registry copies\n")
  regs <- list(
    list(file = s32$S32_REGISTRY$repo_path, commit = s32$S32_REGISTRY$freeze_commit, sha = s32$S32_REGISTRY$sha256),
    list(file = s32$S32_REGISTRY$repo_csv, commit = s32$S32_REGISTRY$freeze_commit, sha = s32$S32_REGISTRY$csv_sha256),
    list(file = s32$S32_ADDENDUM$repo_path, commit = s32$S32_ADDENDUM$freeze_commit, sha = s32$S32_ADDENDUM$sha256),
    # the v1.0 registries of the superseded post hoc runs (frozen, never edited)
    list(file = "docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md", commit = "322be31af04003b1a573f5ec3ee09b013046175c",
         sha = "a6435d8d84b164671b59486f7a3eb9af2bff29bd251c58866cd4b777f9fa0604"),
    list(file = "docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.0.md", commit = "2be501d8b849346bdde47e246fb738a9df05ad98",
         sha = "a617bb5be709b558ed628a61fab0095b598345912c86e9c25070d07b02cb8098"),
    list(file = phc$repo_path, commit = phc$freeze_commit, sha = phc$sha256),
    list(file = pmf$repo_path, commit = pmf$freeze_commit, sha = pmf$sha256))
  for (r in regs[vapply(regs, function(r) identical(r$sha, PENDING), TRUE)]) note_pending(paste("registry", basename(r$file)))
  regs <- regs[!vapply(regs, function(r) identical(r$sha, PENDING), TRUE)]
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

cat("\n6. Stage 33 plan and expected values\n")
s33_env <- new.env(); sys.source("Functions/stage33_run.R", envir = s33_env)
plan <- s33_env$S33_PLAN
plan_lf <- gsub("\r\n", "\n", rawToChar(readBin(plan$path, "raw", file.size(plan$path))), fixed = TRUE)
check(identical(digest::digest(plan_lf, algo = "sha256", serialize = FALSE), plan$sha256_lf), "Stage 33 plan: LF sha256 differs from S33_PLAN")
check(identical(readBin(plan$path, "raw", file.size(plan$path)), charToRaw(plan_lf)), "Stage 33 plan: the checkout must have LF line endings")
exp_dir <- "docs/stage33/expected"
exp_pins <- c(bw_cc1_primary.csv = "909e16d1f9024c3915b6ec9d0ca38a166d5ac2e6a8aabcca79a9a05a3bf67c95",
              expected_cage_terms.csv = "cdc933f318eb6955082a4709b4958943539e54da42a450c8a446a43deee72f24",
              expected_information_primary.csv = "481e211075bc8cf4ba6f11a71a9c54cd8cbd1d19f44c3e60f8510a03bb139965",
              expected_board_offsets.csv = "da74215125034a9afc955410baa4aa79f9af24c8e38f7866c0f56f7cecd9c044",
              expected_design_sensitivity.csv = "7061dfac25d2736c49833cd5262db8dcdefbcfeceb63aa1f7e6fb00d536928b5",
              expected_s2_shared_zone_no692.csv = "0bd2458f4269b15203551612dbe34985e75ac658329a5f16eb5aa82fd25c7dc5",
              expected_reallocation.csv = "7a520498b5ad0ce2e80d086bef33e0565ce67828ba5f5137bee99ddc6bbe2c9f")
for (f in names(exp_pins)) check(identical(digest::digest(file = file.path(exp_dir, f), algo = "sha256"), exp_pins[[f]]),
                                 paste0(exp_dir, "/", f, ": sha256 differs from its pin (CRLF bytes)"))
ok(sprintf("plan LF sha256 %s and %d expected-value files match their pins", substr(plan$sha256_lf, 1, 8), length(exp_pins)))

cat(if (length(pending)) sprintf("\nPASS: frozen code identity (%d pins pending until Phase B)\n", length(pending)) else "\nPASS: frozen code identity\n")
