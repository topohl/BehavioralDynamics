# ================================================================
# Versioned spatial-analysis exclusion manifest
# MMMSociability -- Functions/spatial_exclusion_manifest.R
# ================================================================
# docs/spatial_exclusions/spatial_analysis_exclusions_<version>.csv: cage-recording records for NEW spatial analyses.
#   exclude_spatial  the cage episode (Batch x System x CC; one antenna board in one recording) is left out of spatial
#                    measures that need complete antenna coverage, for the whole recording span.
#   censor           the cage episode is unobserved from `from` to the span end: every time-based measure ends there
#                    (recording-duration censoring, not an exclusion of the episode).
#   exclude_movement the cage episode's active-phase vendor movement outcomes (crossing rate, fragmentation) are left out
#                    of new primary analyses and kept in a sensitivity analysis (from manifest v1.1).
# Separate from the registered exclusions: the frozen S13 sensitivity of Stage 29 and every registered output stay as
# they are, and no registered stage reads this file (Testing/tests/test_spatial_exclusion_manifest.R). A released
# version is never edited: SXM_RELEASED pins its content, and a change is a new version file.
# Pure definitions: sourcing this file reads no data. Needs data.table and digest.
# ================================================================

SXM_DIR <- file.path("docs", "spatial_exclusions")
SXM_COLUMNS <- c("manifest_version", "record_id", "action", "Batch", "CC", "board", "System", "CageEpisodeID", "reader",
                 "resource", "animals", "from", "to", "applies_to", "measures_affected", "movement_outcomes", "evidence",
                 "registered_relation", "decision", "decision_date")
SXM_ACTIONS <- c("exclude_spatial", "censor", "exclude_movement")
SXM_TIME_FORMAT <- "%Y-%m-%d %H:%M:%OS"   # logger clock, labelled UTC as everywhere in the pipeline
# sha256 of each released version's content with line endings normalised to LF (sxm_content_sha256()).
SXM_RELEASED <- c(v1.0 = "9203a199cca0e4d1c7b2d57cd964a032e35acd3ec3032a009600cd0f723ddaa7",
                  v1.1 = "3ecd65bcf1a90e19c0ad99e99b3f26ac31a1877d834e770661799bd62024d940")

#' File of a manifest version.
sxm_path <- function(version, repo_root = MMM_REPO_ROOT) {
  file.path(repo_root, SXM_DIR, sprintf("spatial_analysis_exclusions_%s.csv", version))
}

#' sha256 of a text file's content with CRLF/CR normalised to LF (stable across checkouts).
sxm_content_sha256 <- function(path) {
  txt <- paste(sub("\r$", "", readLines(path, warn = FALSE, encoding = "UTF-8")), collapse = "\n")
  digest::digest(txt, algo = "sha256", serialize = FALSE)
}

#' Logger-clock time of a manifest field as POSIX seconds (NA if it is not a time).
sxm_time <- function(x) as.numeric(as.POSIXct(x, format = SXM_TIME_FORMAT, tz = "UTC"))

#' Checks of a manifest table (all columns character). Stops on the first problem; returns TRUE.
sxm_validate <- function(m, version = NULL) {
  if (!identical(names(m), SXM_COLUMNS)) stop("Manifest columns differ from SXM_COLUMNS.", call. = FALSE)
  if (!nrow(m)) stop("Empty manifest.", call. = FALSE)
  if (!is.null(version) && any(m$manifest_version != version)) stop("manifest_version differs from the file version.", call. = FALSE)
  if (anyDuplicated(m$record_id)) stop("Duplicate record_id.", call. = FALSE)
  if (any(!m$action %in% SXM_ACTIONS)) stop("Unknown action: ", paste(setdiff(m$action, SXM_ACTIONS), collapse = ", "), call. = FALSE)
  if (any(!grepl("^B[1-6]$", m$Batch)) || any(!grepl("^CC[1-4]$", m$CC)) || any(!m$board %in% as.character(1:5)))
    stop("Batch, CC or board out of range.", call. = FALSE)
  if (any(m$System != paste0("sys.", m$board))) stop("System must be sys.<board>.", call. = FALSE)
  if (any(m$CageEpisodeID != paste(m$Batch, m$System, m$CC, sep = "|"))) stop("CageEpisodeID must be Batch|System|CC.", call. = FALSE)
  ex <- m$action %in% c("exclude_spatial", "exclude_movement")
  if (any(m$from[ex] != "span_start" | m$to[ex] != "span_end")) stop("Exclusion records cover the whole span.", call. = FALSE)
  ce <- m$action == "censor"
  if (any(is.na(sxm_time(m$from[ce]))) || any(m$to[ce] != "span_end")) stop("censor records need a time `from` and to = span_end.", call. = FALSE)
  if (anyDuplicated(paste(m$action, m$CageEpisodeID))) stop("Two records of one action for one cage episode.", call. = FALSE)
  if (any(!nzchar(m$evidence)) || any(!nzchar(m$registered_relation)) || any(!nzchar(m$decision))) {
    stop("Every record needs its evidence, its relation to the registered exclusions and its decision.", call. = FALSE)
  }
  invisible(TRUE)
}

#' A manifest version, validated; a released version must match its pinned content hash.
sxm_read <- function(version = "v1.0", repo_root = MMM_REPO_ROOT, allow_unreleased = FALSE) {
  path <- sxm_path(version, repo_root)
  if (!file.exists(path)) stop("No spatial exclusion manifest ", version, ": ", path, call. = FALSE)
  pinned <- SXM_RELEASED[version]
  if (is.na(pinned) || identical(unname(pinned), "PENDING")) {
    if (!allow_unreleased) stop("Manifest ", version, " is not released (no pinned hash in SXM_RELEASED).", call. = FALSE)
  } else if (!identical(sxm_content_sha256(path), unname(pinned))) {
    stop("Manifest ", version, " differs from its released content; a change needs a new version file.", call. = FALSE)
  }
  m <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  sxm_validate(m, version)
  attr(m, "path") <- path
  attr(m, "sha256") <- sxm_content_sha256(path)
  m
}

#' Whether cage episodes (vectors batch, cc, board) are excluded from spatial measures that need complete coverage.
sxm_spatial_excluded <- function(m, batch, cc, board) {
  ex <- m[m$action == "exclude_spatial", , drop = FALSE]
  paste(batch, cc, board) %in% paste(ex$Batch, ex$CC, ex$board)
}

#' Whether cage episodes are left out of new primary analyses of active-phase vendor movement outcomes (exclude_movement).
sxm_movement_excluded <- function(m, batch, cc, board) {
  ex <- m[m$action == "exclude_movement", , drop = FALSE]
  paste(batch, cc, board) %in% paste(ex$Batch, ex$CC, ex$board)
}

#' Censoring time (POSIX seconds) of each cage episode: the earliest censor `from`, Inf without one.
sxm_censor_from <- function(m, batch, cc, board) {
  ce <- m[m$action == "censor", , drop = FALSE]
  key <- paste(ce$Batch, ce$CC, ce$board)
  t <- sxm_time(ce$from)
  vapply(paste(batch, cc, board), function(k) if (k %in% key) min(t[key == k]) else Inf, numeric(1), USE.NAMES = FALSE)
}
