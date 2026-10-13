# ================================================================
# Append-only QC runs: run ledger, protected-location snapshots and the publish step
# MMMSociability -- Functions/qc_run_protection.R
# ================================================================
# Used by Analysis/00b_qc_raw_acquisition.R. A QC run is built in <base>/runs/.tmp_<run_id>/ and published by
# mmm_qc_publish():
#   1. the protected locations (inputs, Stage 00 outputs, Stage 19 outputs, earlier QC runs, the registered exclusion
#      definitions) must be byte-for-byte as they were when the run started: size and time stamp of every file, and
#      sha256 of the listed definition files; otherwise the run stops and the .tmp_ folder stays for inspection;
#   2. the run folder must not exist, its id must not be in the ledger, and the ledger must still start with the bytes
#      it had when the run started (a peer run may have appended a line; nothing may have been rewritten);
#   3. the .tmp_ folder is renamed and one line is appended to <base>/runs_ledger.csv; if the append fails, the rename
#      is undone.
# Steps 1-3 hold a lock folder (<base>/runs_ledger.lock), so two runs cannot publish at once. The protection check is
# relative to the run's own start; it does not compare the definitions with a registered baseline.
# Pinned files of the frozen runs (Stage 08 outputs, Stage 29-32 inputs) are checked separately by
# Functions/frozen_input_guard.R. Needs base R and digest.
# ================================================================

MMM_QC_LEDGER_COLUMNS <- c("run_id", "finished", "git_commit", "rules_version", "qc_status", "isolated", "recordings",
                           "output_manifest_sha256", "protected_files", "protection_changes")

mmm_qc_ledger_path <- function(base) file.path(base, "runs_ledger.csv")

#' The ledger of a QC root (empty data.frame with the ledger columns if none).
mmm_qc_ledger_read <- function(base) {
  p <- mmm_qc_ledger_path(base)
  if (!file.exists(p)) return(stats::setNames(data.frame(matrix(character(), 0, length(MMM_QC_LEDGER_COLUMNS))), MMM_QC_LEDGER_COLUMNS))
  d <- utils::read.csv(p, colClasses = "character", check.names = FALSE)
  if (!identical(names(d), MMM_QC_LEDGER_COLUMNS)) stop("Unexpected columns in ", p, call. = FALSE)
  d
}

#' The ledger's state at a run's start: its size in bytes and the sha256 of those bytes (0 and NA without a ledger).
mmm_qc_ledger_state <- function(base) {
  p <- mmm_qc_ledger_path(base)
  if (!file.exists(p)) return(list(bytes = 0, sha256 = NA_character_))
  list(bytes = file.size(p), sha256 = digest::digest(file = p, algo = "sha256"))
}

#' TRUE if the ledger still starts with the bytes recorded in `state` (lines appended since are allowed).
mmm_qc_ledger_prefix_ok <- function(base, state) {
  if (!state$bytes) return(TRUE)
  p <- mmm_qc_ledger_path(base)
  if (!file.exists(p) || file.size(p) < state$bytes) return(FALSE)
  con <- file(p, "rb"); on.exit(close(con))
  identical(digest::digest(readBin(con, "raw", state$bytes), algo = "sha256", serialize = FALSE), state$sha256)
}

#' Appends one run to the ledger. Refuses an id already listed, and refuses to write if earlier bytes changed since
#' `state` (mmm_qc_ledger_state() at the run's start).
mmm_qc_ledger_append <- function(base, row, state) {
  p <- mmm_qc_ledger_path(base)
  row <- as.data.frame(lapply(row[MMM_QC_LEDGER_COLUMNS], as.character), stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(row) != 1L || anyNA(row$run_id)) stop("One ledger row with a run_id is needed.", call. = FALSE)
  if (row$run_id %in% mmm_qc_ledger_read(base)$run_id) stop("Run id already in the ledger: ", row$run_id, call. = FALSE)
  if (!mmm_qc_ledger_prefix_ok(base, state)) stop("Earlier lines of the ledger changed since the run started: ", p, call. = FALSE)
  quote <- function(x) paste0("\"", gsub("\"", "\"\"", x), "\"")
  line <- paste(vapply(row, quote, ""), collapse = ",")
  if (!file.exists(p)) cat(paste(vapply(MMM_QC_LEDGER_COLUMNS, quote, ""), collapse = ","), "\n", file = p, sep = "")
  cat(line, "\n", file = p, sep = "", append = TRUE)
  invisible(p)
}

#' Snapshot of protected locations: every file under each folder (recursively) or the file itself, with size and
#' modification time; sha256 for the files in `hash`. Missing locations are recorded as such.
mmm_qc_protection_snapshot <- function(locations, hash = character()) {
  locations <- unique(locations[nzchar(locations)])
  files <- unlist(lapply(locations, function(l) {
    if (dir.exists(l)) list.files(l, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    else if (file.exists(l)) l else paste0("<missing>", l)
  }), use.names = FALSE)
  files <- unique(c(files, hash[!hash %in% files]))
  real <- !startsWith(files, "<missing>") & file.exists(files)
  info <- file.info(files[real], extra_cols = FALSE)
  out <- data.frame(path = files, exists = real, size = NA_real_, mtime = NA_real_, sha256 = NA_character_, stringsAsFactors = FALSE)
  out$size[real] <- info$size
  out$mtime[real] <- as.numeric(info$mtime)
  h <- out$path %in% hash & real
  out$sha256[h] <- vapply(out$path[h], function(f) digest::digest(file = f, algo = "sha256"), "", USE.NAMES = FALSE)
  out
}

#' Differences between two snapshots: one row per added, removed or modified file (size, time stamp or sha256).
mmm_qc_protection_compare <- function(before, after) {
  m <- merge(before, after, by = "path", all = TRUE, suffixes = c("_before", "_after"))
  was <- m$exists_before %in% TRUE; is <- m$exists_after %in% TRUE
  same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  modified <- was & is & !(same(m$size_before, m$size_after) & same(m$mtime_before, m$mtime_after) & same(m$sha256_before, m$sha256_after))
  # a location recorded as missing ("<missing>path") that appears is listed as its files (added) and its placeholder
  kind <- ifelse(!was & is, "added", ifelse(was & !is, "removed", ifelse(modified, "modified",
                 ifelse(xor(is.na(m$exists_before), is.na(m$exists_after)), "placeholder", NA_character_))))
  ch <- m[!is.na(kind), c("path", "size_before", "size_after", "mtime_before", "mtime_after", "sha256_before", "sha256_after"), drop = FALSE]
  ch$change <- kind[!is.na(kind)]
  ch
}

#' TRUE if two normalised paths (lower case, "/" separators, no trailing "/") are equal or one lies in the other, also
#' when one is spelled with a drive letter (s:/a/b) and the other as a UNC path (//server/share/.../a/b): the drive path's
#' components after the drive must then appear in the UNC path, aligned to whole components (at least two of them).
.mmm_qc_overlap <- function(a, b) {
  within <- function(x, y) identical(x, y) || startsWith(x, paste0(y, "/"))
  if (within(a, b) || within(b, a)) return(TRUE)
  drive <- function(x) grepl("^[a-z]:/", x)
  if (drive(a) == drive(b)) return(FALSE)
  d <- if (drive(a)) a else b; u <- if (drive(a)) b else a
  rest <- sub("^[a-z]:", "", d)                                   # "/lab_member/.../hmm_states"
  if (length(strsplit(sub("^/", "", rest), "/")[[1]]) < 2) return(FALSE)
  hit <- gregexpr(rest, u, fixed = TRUE)[[1]]
  after <- substr(u, hit + nchar(rest), hit + nchar(rest))
  if (any(hit > 0 & after %in% c("", "/"))) return(TRUE)         # the UNC path lies in (or is) the drive path
  comps <- strsplit(sub("^//", "", u), "/")[[1]]
  for (k in seq.int(2L, length.out = max(length(comps) - 1L, 0L))) {   # the drive path lies in the UNC path
    suf <- paste0("/", paste(utils::tail(comps, k), collapse = "/"))
    if (startsWith(paste0(rest, "/"), paste0(suf, "/"))) return(TRUE)
  }
  FALSE
}

#' Refuses a QC output base that lies in, equals or contains a protected location (drive or UNC spelling).
mmm_qc_assert_target <- function(base, protected) {
  n <- function(p) tolower(sub("/+$", "", gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE))))
  b <- n(base)
  for (p in protected[nzchar(protected)]) {
    if (.mmm_qc_overlap(b, n(p))) stop("Refusing QC output root ", base, ": it overlaps the protected location ", p, call. = FALSE)
  }
  invisible(TRUE)
}

#' Publishes a built run: protection check, collision checks, rename, ledger line, under the ledger lock. `before`: the
#' snapshot taken at the start; locations/hash: as passed to it; ledger_state: mmm_qc_ledger_state() at the start.
#' Writes the comparison to <tmp>/manifest/protection_check.csv before deciding. Returns the output folder.
mmm_qc_publish <- function(tmp_dir, output_dir, base, ledger_row, before, locations, hash, ledger_state) {
  lock <- file.path(base, "runs_ledger.lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another run is publishing (", lock, "); nothing was published.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  after <- mmm_qc_protection_snapshot(locations, hash)
  # the run's own .tmp_ folder is the only thing allowed to change under a protected root (it is never one)
  ch <- mmm_qc_protection_compare(before, after)
  dir.create(file.path(tmp_dir, "manifest"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(ch, file.path(tmp_dir, "manifest", "protection_check.csv"), row.names = FALSE)
  utils::write.csv(after, file.path(tmp_dir, "manifest", "protection_snapshot_after.csv"), row.names = FALSE)
  if (nrow(ch)) {
    stop(nrow(ch), " protected file(s) changed during the run (", paste(utils::head(paste(ch$change, ch$path), 5), collapse = "; "),
         "). Nothing was published; the run stays in ", tmp_dir, call. = FALSE)
  }
  if (file.exists(output_dir)) stop("Run folder exists; nothing was published: ", output_dir, call. = FALSE)
  if (ledger_row$run_id %in% mmm_qc_ledger_read(base)$run_id) stop("Run id already in the ledger: ", ledger_row$run_id, call. = FALSE)
  if (!mmm_qc_ledger_prefix_ok(base, ledger_state)) stop("Earlier lines of the ledger changed during the run; nothing was published.", call. = FALSE)
  ledger_row$protected_files <- nrow(after)
  ledger_row$protection_changes <- 0L
  if (!file.rename(tmp_dir, output_dir)) stop("Could not rename ", tmp_dir, " to ", output_dir, call. = FALSE)
  ok <- tryCatch({ mmm_qc_ledger_append(base, ledger_row, ledger_state); TRUE }, error = function(e) e)
  if (!isTRUE(ok)) {
    back <- file.rename(output_dir, tmp_dir)
    stop("The ledger line could not be written (", conditionMessage(ok), "); ",
         if (back) paste("the run was moved back to", tmp_dir) else paste("the run stays in", output_dir, "WITHOUT a ledger line"), call. = FALSE)
  }
  invisible(output_dir)
}
