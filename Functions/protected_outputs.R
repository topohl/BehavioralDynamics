# ================================================================
# Protected outputs: the location registry and the before/after guard of isolated runs
# MMMSociability -- Functions/protected_outputs.R
# ================================================================
# An isolated run of the revised spatial analysis writes <base>/runs/<run_id>/ only (built in <base>/runs/.tmp_<run_id>/,
# usually <base> = <Behavior>/RANM/<tool>). Everything else that is registered, live, imported into the manuscript or
# written by an earlier isolated run must be byte for byte as it was when the run started.
#   mmm_protected_output_locations()  the registry (MMM_PROTECTED_PROJECT_LOCATIONS, the inputs, the whole RANM tree, the
#                                     manuscript's source data and provenance, the repository pin files, and the roots
#                                     no isolated run may write into)
#   mmm_protection_assert_base()      refuses an output base that lies in, equals or contains a protected location; inside
#                                     RANM the run's two folders must not lie in, equal or contain an earlier run's folder
#                                     or any other RANM entry (so a base inside an earlier run, or an existing run id, is
#                                     refused, while a new RANM/<tool> and new runs of an existing tool are accepted); a
#                                     base inside a "runs" folder and an existing runs/.tmp_<run_id> (unless resuming) are
#                                     refused too
#   mmm_protection_snapshot()         size and time stamp of every protected file; sha256 of the files whose name matches
#                                     MMM_PROTECTION_HASHED_NAME_PATTERN and of every repository pin file
#   mmm_protection_guard_begin()      assert_base, the before snapshot and mmm_frozen_guard_before()
#   mmm_protection_guard_end()        the after snapshot and the comparison, written to protection_snapshot_before.csv and
#                                     protection_check.csv in the run's manifest folder, then mmm_frozen_guard_after(); stops
#                                     when a protected file was added, removed or changed
# Only the current run's runs/.tmp_<run_id> and runs/<run_id> are left out of the snapshots, so earlier runs of every
# tool (the same tool included) are protected. A changed size or time stamp counts as a change unless the file is hashed
# and its sha256 is unchanged (a byte-identical rewrite). A file under a protected location that another session writes
# while the run is going (a concurrent RANM run, a manuscript import, a new calibration document) also stops the run:
# the guard cannot tell it from a change made by the run. Operating-system thumbnails (MMM_PROTECTION_IGNORED_NAMES) are
# not listed.
# On a sandbox project root (MMM_BEHAVIOR_PROJECT_ROOT) the registry still refuses the live <Behavior>/RANM runs and the
# live Exp9_manuscript as output bases (classes isolated_runs_live and write_refused); they are not snapshotted then.
# Builders call begin() in their main block after mmm_isolated_output_root_check() and before any write, and end() after
# all outputs are written and before output_files_sha256.csv and the rename. Reuses Functions/qc_run_protection.R
# (snapshot, comparison, target check) and Functions/frozen_input_guard.R, sourced below through source_mmm_helper()
# when it exists. The round-level check of the self-manifests is Testing/audits/verify_protected_outputs.R.
# Pure definitions: sourcing this file reads no data. Needs data.table and digest.
# ================================================================

for (.po_dep in c("project_paths.R", "qc_run_protection.R", "frozen_input_guard.R")) {
  .po_probe <- switch(.po_dep, "project_paths.R" = "mmm_raw_antenna_root", "qc_run_protection.R" = "mmm_qc_protection_snapshot",
                      "frozen_input_guard.R" = "mmm_frozen_guard_before")
  if (!exists(.po_probe, mode = "function", inherits = TRUE) && exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper(.po_dep)
  }
}
rm(.po_dep, .po_probe)

# Protected trees under the project root (relative path = note). Stage 19 has no pin list: its record is
# docs/behavior_output_producer_reruns/stage19_20261012.csv overlaid by stage19_20261012b.csv.
MMM_PROTECTED_PROJECT_LOCATIONS <- c(
  "analysis_ready/analyses/spatial_occupancy" = "Stage 19 live outputs (record: docs/behavior_output_producer_reruns/stage19_*.csv)",
  "analysis_ready/analyses/cc4_grid_exposure" = "Stage 31, 31b, 31c and 31d run folders (each with its own manifest)",
  "analysis_ready/analyses/hmm_states" = "Stage 08 outputs (72 files pinned in docs/frozen_output_pins/)",
  "analysis_ready/analyses/posthoc_cohort_followups" = "Stage 33 post hoc runs",
  "analysis_ready/pipeline/29_canonical_behavior_releases" = "Stage 29 releases v1.0.0 and v1.0.1 (registered)",
  "analysis_ready/pipeline/29b_posthoc_con_contrasts" = "Stage 29b run (registered)",
  "analysis_ready/pipeline/29c_posthoc_movement_combz_fit" = "Stage 29c run (registered)",
  "analysis_ready/pipeline/30_exploratory_screen" = "Stage 30 run (registered)",
  "analysis_ready/pipeline/32_behavior_exposure_adaptation" = "Stage 32 run v1.1 and the .tmp_v1.0 evidence (registered)",
  "analysis_ready/pipeline/09_early_prediction" = "Stage 09 outputs (frozen-guard pins)",
  "analysis_ready/pipeline/28_rfid_behavioral_domains" = "Stage 28 outputs (frozen-guard pins)",
  "analysis_ready/canonical" = "frozen bundles (BUNDLE_REGISTRY.csv), registries, CombZ, configuration",
  "analysis_ready/quality_control/raw_acquisition" = "Stage 00b live runs and their ledger (append-only)",
  "analysis_ready/quality_control/tracking_integrity" = "Stage 00 runs",
  "analysis_ready/history/tracking_integrity/10sec" = "May 2026 Stage 00 products",
  "analysis_ready/foundations" = "Stage 01 metrics (frozen-guard pins)",
  "publication_ready/tables/spatial_occupancy" = "Stage 19 publication copies (tables)",
  "publication_ready/figures/single_panels/spatial_occupancy" = "Stage 19 publication copies (figures)")
# Inputs under the project root; the raw antenna logs (mmm_raw_antenna_root()) are added as class input_raw_logs.
MMM_PROTECTED_INPUT_LOCATIONS <- c(
  "MMMSociability/raw_data" = "vendor AnimalPos exports (tag maps) and excluded_animals.csv",
  "MMMSociability/data_versions" = "preprocessed data versions (v2 cage-label correction, cookie)")
# Folders of the Exp9_manuscript tree (<Analysis>/Exp9_manuscript) that hold imported copies and their manifests.
MMM_PROTECTED_MANUSCRIPT_FOLDERS <- c(
  "source_data" = "imported source data (bundle copies)",
  "provenance" = "import manifests, claims and registries")
# Repository files (relative to the repository root); a trailing "*" is expanded to the matching files of that folder.
MMM_PROTECTED_REPO_FILES <- c(
  "docs/spatial_exclusions" = "released spatial exclusion versions (content-pinned)",
  "Functions/behavior_analysis_config.R" = "frozen configuration, S13 (byte-pinned)",
  "docs/frozen_output_pins" = "repository pin lists of the frozen-input guard",
  "docs/behavior_output_producer_reruns/stage19_*" = "Stage 19 output record",
  "docs/rfid_calibration" = "calibration pilot criteria and schedule (content-pinned)")
# File names whose sha256 is taken in every snapshot (manifests, ledgers, registries, run records); other files are
# compared by size and time stamp. Matched case-insensitively against the base name.
MMM_PROTECTION_HASHED_NAME_PATTERN <- "manifest|ledger|registry|output_files_sha256|input_hashes|run[.]csv|BUNDLE_REGISTRY|00_manifest"
# Thumbnail and folder-view files that Windows Explorer writes when a folder is browsed; never outputs.
MMM_PROTECTION_IGNORED_NAMES <- c("Thumbs.db", "desktop.ini", ".DS_Store")

.mmm_po_repo_root <- function() {
  if (exists("MMM_REPO_ROOT", envir = globalenv(), inherits = FALSE)) get("MMM_REPO_ROOT", envir = globalenv()) else getwd()
}

# Lower case, "/" separators, no trailing "/"; `full` also resolves the path (slow on many files, so only for roots).
.mmm_po_key <- function(p, full = FALSE) {
  if (full) p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  tolower(sub("/+$", "", gsub("\\\\", "/", p)))
}

# TRUE for each path (key) that equals or lies in one of the roots (keys); same spelling only.
.mmm_po_under <- function(keys, roots) {
  hit <- logical(length(keys))
  for (r in roots) hit <- hit | keys == r | startsWith(keys, paste0(r, "/"))
  hit
}

# TRUE when normalised path a lies in or equals normalised path b. A drive-letter path and a UNC path are compared as
# in .mmm_qc_overlap(): the drive path's components after the drive must align with whole UNC components (two or more).
.mmm_po_within <- function(a, b) {
  if (identical(a, b) || startsWith(a, paste0(b, "/"))) return(TRUE)
  drive <- function(x) grepl("^[a-z]:/", x)
  if (drive(a) == drive(b)) return(FALSE)
  if (drive(b)) {
    rest <- sub("^[a-z]:", "", b)
    if (length(strsplit(sub("^/", "", rest), "/")[[1]]) < 2) return(FALSE)
    hit <- gregexpr(rest, a, fixed = TRUE)[[1]]
    after <- substr(a, hit + nchar(rest), hit + nchar(rest))
    return(any(hit > 0 & after %in% c("", "/")))
  }
  rest <- paste0(sub("^[a-z]:", "", a), "/")
  comps <- strsplit(sub("^//", "", b), "/")[[1]]
  for (k in seq.int(2L, length.out = max(length(comps) - 1L, 0L))) {
    if (startsWith(rest, paste0("/", paste(utils::tail(comps, k), collapse = "/"), "/"))) return(TRUE)
  }
  FALSE
}

#' The protected locations: data.table(location, class, note). Classes: live_output and publication_copy (under the
#' project root), input and input_raw_logs, isolated_runs (the whole <Behavior>/RANM tree), manuscript, repo_pin, and
#' write_refused (the project root, the Exp9_manuscript root and the repository root: refused as output roots, not
#' snapshotted). On a project root other than MMM_PROJECT_ROOT_DEFAULT the live <Behavior>/RANM (isolated_runs_live: the
#' earlier-run rules apply, no snapshot) and the live Exp9_manuscript root (write_refused) are added. Missing locations are
#' kept; the snapshot records them as missing.
mmm_protected_output_locations <- function(project_root = mmm_project_root(), repo_root = .mmm_po_repo_root()) {
  rel <- names(MMM_PROTECTED_PROJECT_LOCATIONS)
  manuscript <- file.path(dirname(dirname(project_root)), "Exp9_manuscript")
  repo <- unlist(lapply(names(MMM_PROTECTED_REPO_FILES), function(r) {
    if (!endsWith(r, "*")) return(stats::setNames(file.path(repo_root, r), r))
    d <- file.path(repo_root, dirname(r))
    hit <- if (dir.exists(d)) list.files(d, pattern = utils::glob2rx(basename(r)), full.names = TRUE) else character(0)
    stats::setNames(hit, rep(r, length(hit)))
  }))
  data.table::rbindlist(list(
    data.table::data.table(location = file.path(project_root, rel), class = ifelse(startsWith(rel, "publication_ready/"), "publication_copy", "live_output"),
                           note = unname(MMM_PROTECTED_PROJECT_LOCATIONS)),
    data.table::data.table(location = mmm_raw_antenna_root(project_root), class = "input_raw_logs", note = "raw MultiMouseMonitor antenna logs"),
    data.table::data.table(location = file.path(project_root, names(MMM_PROTECTED_INPUT_LOCATIONS)), class = "input",
                           note = unname(MMM_PROTECTED_INPUT_LOCATIONS)),
    data.table::data.table(location = file.path(dirname(project_root), "RANM"), class = "isolated_runs",
                           note = "every earlier isolated run of every tool; the current run's runs/.tmp_<id> and runs/<id> are left out"),
    data.table::data.table(location = file.path(manuscript, names(MMM_PROTECTED_MANUSCRIPT_FOLDERS)), class = "manuscript",
                           note = unname(MMM_PROTECTED_MANUSCRIPT_FOLDERS)),
    data.table::data.table(location = unname(repo), class = "repo_pin", note = unname(MMM_PROTECTED_REPO_FILES[names(repo)])),
    data.table::data.table(location = c(project_root, manuscript, repo_root), class = "write_refused",
                           note = c("live project root", "Exp9_manuscript checkout", "MMMSociability repository")),
    .mmm_po_live_trees(project_root)))
}

# The live RANM tree and Exp9_manuscript root when project_root is not the live root (a sandbox); NULL otherwise.
.mmm_po_live_trees <- function(project_root) {
  if (!exists("MMM_PROJECT_ROOT_DEFAULT", inherits = TRUE)) return(NULL)
  live <- get("MMM_PROJECT_ROOT_DEFAULT", inherits = TRUE)
  if (identical(.mmm_po_key(project_root, full = TRUE), .mmm_po_key(live, full = TRUE))) return(NULL)
  data.table::data.table(location = c(file.path(dirname(live), "RANM"), file.path(dirname(dirname(live)), "Exp9_manuscript")),
                         class = c("isolated_runs_live", "write_refused"),
                         note = c("live isolated-run tree (refused as a base inside an earlier run; not snapshotted on a sandbox root)",
                                  "live Exp9_manuscript checkout"))
}

# Classes whose earlier runs a base inside them must not touch; classes left out of the snapshots.
MMM_PROTECTION_RANM_CLASSES <- c("isolated_runs", "isolated_runs_live")
MMM_PROTECTION_UNSNAPSHOTTED_CLASSES <- c("write_refused", "isolated_runs_live")

# Entries of the RANM tree that a run may neither lie in, equal nor contain: each <tool>/runs/<run> (published runs and
# other runs' .tmp_ folders) and every other file or folder of the tree down to the tool level. `keep`: the current
# run's .tmp_ folder (a resumed run may find it).
.mmm_po_ranm_entries <- function(ranm, keep = character(0)) {
  if (!dir.exists(ranm)) return(character(0))
  ls1 <- function(d) list.files(d, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  out <- character(0)
  for (t in ls1(ranm)) {
    if (!dir.exists(t)) { out <- c(out, t); next }
    runs <- file.path(t, "runs")
    kids <- ls1(t)
    out <- c(out, kids[.mmm_po_key(kids) != .mmm_po_key(runs)], if (dir.exists(runs)) ls1(runs))
  }
  out[!.mmm_po_key(out) %in% .mmm_po_key(keep)]
}

#' Refuses an output base (and the run folders <base>/runs/.tmp_<run_id> and <base>/runs/<run_id>) that would write into
#' a protected location, a base inside a "runs" folder (its runs would nest inside another run tree), and an existing
#' runs/.tmp_<run_id> unless `resume` is TRUE. Returns the two run folders.
mmm_protection_assert_base <- function(base, run_id, registry, resume = FALSE) {
  if (!is.character(run_id) || length(run_id) != 1L || is.na(run_id) || !nzchar(run_id) || grepl("[/\\\\]|^[.]", run_id)) {
    stop("A run id must be one nonempty folder name: ", paste(run_id, collapse = " "), call. = FALSE)
  }
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  own <- file.path(base, "runs", c(paste0(".tmp_", run_id), run_id))
  if ("runs" %in% tolower(strsplit(.mmm_po_key(base), "/", fixed = TRUE)[[1]])) {
    stop("Refusing output root ", base, ": it lies in a \"runs\" folder; use the tool folder (<tool>, not <tool>/runs/...)", call. = FALSE)
  }
  if (file.exists(own[2])) stop("Refusing run ", run_id, ": ", own[2], " exists; choose a new run id", call. = FALSE)
  if (file.exists(own[1]) && !isTRUE(resume)) {
    stop("Refusing run ", run_id, ": a partial run folder exists (", own[1], "); inspect it, or resume explicitly", call. = FALSE)
  }
  mmm_qc_assert_target(base, registry[!class %in% MMM_PROTECTION_RANM_CLASSES]$location)
  b <- .mmm_po_key(base, full = TRUE)
  for (r in registry[class %in% MMM_PROTECTION_RANM_CLASSES]$location) {
    rk <- .mmm_po_key(r, full = TRUE)
    if (.mmm_po_within(rk, b)) stop("Refusing output root ", base, ": it equals or contains the isolated-run tree ", r, call. = FALSE)
    if (.mmm_po_within(b, rk)) {
      entries <- .mmm_po_ranm_entries(r, keep = own[1])
      for (o in own) mmm_qc_assert_target(o, entries)
    }
  }
  invisible(own)
}

#' Snapshot of the registry (write_refused locations excepted) without the folders in `exclude`: path, exists, size,
#' mtime, sha256 (as mmm_qc_protection_snapshot()), plus class and location. sha256 is taken for every repository pin
#' file and for the files whose base name matches MMM_PROTECTION_HASHED_NAME_PATTERN.
mmm_protection_snapshot <- function(registry, exclude = character(0)) {
  loc <- registry[!class %in% MMM_PROTECTION_UNSNAPSHOTTED_CLASSES]
  pins <- unlist(lapply(loc[class == "repo_pin"]$location, function(l) {
    if (dir.exists(l)) list.files(l, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE) else if (file.exists(l)) l
  }), use.names = FALSE)
  snap <- data.table::as.data.table(mmm_qc_protection_snapshot(loc$location, hash = as.character(pins)))
  key <- .mmm_po_key(sub("^<missing>", "", snap$path))
  drop <- basename(snap$path) %in% MMM_PROTECTION_IGNORED_NAMES | .mmm_po_under(key, .mmm_po_key(exclude))
  snap <- snap[!drop]; key <- key[!drop]
  snap[, `:=`(location = NA_character_, class = NA_character_)]
  lk <- .mmm_po_key(loc$location)
  for (i in order(nchar(lk))) {
    hit <- key == lk[i] | startsWith(key, paste0(lk[i], "/"))
    data.table::set(snap, which(hit), c("location", "class"), list(loc$location[i], loc$class[i]))
  }
  todo <- which(snap$exists & is.na(snap$sha256) & grepl(MMM_PROTECTION_HASHED_NAME_PATTERN, basename(snap$path), ignore.case = TRUE))
  if (length(todo)) data.table::set(snap, todo, "sha256", vapply(snap$path[todo], function(f) digest::digest(file = f, algo = "sha256"), "",
                                                                 USE.NAMES = FALSE))
  snap[]
}

#' Starts the protection of one isolated run. Refuses an output base that lies in, equals or contains a protected
#' location (mmm_protection_assert_base()), takes the before snapshot of every protected location without the run's
#' own two folders, and calls mmm_frozen_guard_before(label, project_root, output_roots = base). Call it after
#' mmm_isolated_output_root_check() and before any write; `resume = TRUE` accepts an existing runs/.tmp_<run_id>.
#' Returns the state for mmm_protection_guard_end().
mmm_protection_guard_begin <- function(base, run_id, label, project_root = mmm_project_root(), repo_root = .mmm_po_repo_root(),
                                       resume = FALSE) {
  for (fn in c("mmm_qc_protection_snapshot", "mmm_qc_assert_target", "mmm_frozen_guard_before", "mmm_raw_antenna_root")) {
    if (!exists(fn, mode = "function")) stop("protected_outputs.R needs ", fn, "(): source qc_run_protection.R, frozen_input_guard.R and project_paths.R",
                                              call. = FALSE)
  }
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  registry <- mmm_protected_output_locations(project_root, repo_root)
  own <- mmm_protection_assert_base(base, run_id, registry, resume = resume)
  started <- Sys.time()
  before <- mmm_protection_snapshot(registry, exclude = own)
  frozen <- mmm_frozen_guard_before(label, project_root, output_roots = base)
  message(sprintf("protection guard (%s): %d protected files, %d hashed, %d locations missing", label, sum(before$exists),
                  sum(!is.na(before$sha256)), sum(!before$exists)))
  list(base = base, run_id = run_id, label = label, project_root = project_root, repo_root = repo_root, registry = registry,
       own = own, before = before, frozen = frozen, started = started)
}

#' Ends the protection of a run: the after snapshot and the comparison with the before snapshot. Writes
#' <manifest_dir>/protection_snapshot_before.csv and <manifest_dir>/protection_check.csv (one row per difference, with
#' its verdict), then calls mmm_frozen_guard_after(). Stops when a protected file was added or removed, when its size or
#' time stamp changed and its bytes cannot be confirmed (not hashed, or another sha256), or when a frozen pin changed; a
#' hashed file rewritten with identical bytes passes. manifest_dir must lie in the run's own folder. Returns the
#' differences invisibly (none: an empty table).
mmm_protection_guard_end <- function(state, manifest_dir) {
  md <- .mmm_po_key(normalizePath(manifest_dir, winslash = "/", mustWork = FALSE))
  if (!any(.mmm_po_under(md, .mmm_po_key(state$own)))) {
    stop("The protection files must go into the run's own folder (", state$own[1], "), not ", manifest_dir, call. = FALSE)
  }
  after <- mmm_protection_snapshot(state$registry, exclude = state$own)
  cols <- c("path", "exists", "size", "mtime", "sha256")
  ch <- data.table::as.data.table(mmm_qc_protection_compare(as.data.frame(state$before[, ..cols]), as.data.frame(after[, ..cols])))
  ch[, verdict := "changed"]
  ch[change == "modified" & !is.na(sha256_before) & !is.na(sha256_after) & sha256_before == sha256_after, verdict := "identical bytes"]
  # a location that was missing and now exists only because of the run's own folders (e.g. a first RANM run)
  own_parent <- vapply(sub("^<missing>", "", ch$path), function(p) any(.mmm_po_under(.mmm_po_key(state$own), .mmm_po_key(p))), logical(1))
  ch[change == "placeholder" & own_parent, verdict := "parent of the run's own folder"]
  dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(state$before, file.path(manifest_dir, "protection_snapshot_before.csv"))
  data.table::fwrite(ch, file.path(manifest_dir, "protection_check.csv"))
  fg <- tryCatch({ mmm_frozen_guard_after(state$frozen); NULL }, error = function(e) conditionMessage(e))
  bad <- ch[verdict == "changed"]
  if (nrow(bad) || !is.null(fg)) {
    stop(if (nrow(bad)) sprintf("%d protected file(s) changed during run %s (%s). ", nrow(bad), state$run_id,
                                paste(utils::head(paste(bad$change, bad$path), 5), collapse = "; ")),
         if (!is.null(fg)) paste0("Frozen-input guard: ", fg, " "),
         "See ", file.path(manifest_dir, "protection_check.csv"), "; nothing may be published from this run.", call. = FALSE)
  }
  message(sprintf("protection guard (%s): %d protected files unchanged (%d rewritten with identical bytes)", state$label,
                  sum(after$exists), sum(ch$verdict == "identical bytes")))
  invisible(ch)
}
