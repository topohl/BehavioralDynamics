# ===========================================================================
# Versioned spatial-analysis exclusion manifest (Functions/spatial_exclusion_manifest.R, docs/spatial_exclusions/).
# Portable: the repository files only.
#
#  1. every version file is released (pinned content hash) and validates; v1.0 holds the three dead-channel cage
#     episodes and the B5 CC4 board 1 censoring
#  2. lookups: excluded cage episodes, censoring time
#  3. validation refuses malformed records
#  4. an edited released version is refused (a change needs a new version file)
#  5. separation from the registered exclusions: the frozen S13 definition is unchanged; no registered stage reads the
#     spatial manifest or the Stage 00b proposed exclusions (the unregistered Stage 31 and the CC4 outcome-eligibility
#     rule may read it, opt-in)
#  6. v1.1: the movement decision as exclude_movement records for the same three cage episodes; spatial exclusions and the
#     censoring time unchanged from v1.0; SX04 states what span_end means per analysis
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "spatial_exclusion_manifest.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
MMM_REPO_ROOT <- repo
source("Functions/spatial_exclusion_manifest.R")

# 1. released versions
files <- list.files(SXM_DIR, pattern = "^spatial_analysis_exclusions_v[0-9.]+[.]csv$")
versions <- sub("^spatial_analysis_exclusions_(v[0-9.]+)[.]csv$", "\\1", files)
check(length(files) >= 1L && setequal(versions, names(SXM_RELEASED)), "1: every version file has a pinned hash and every pin a file")
check(all(SXM_RELEASED != "PENDING"), "1: no unreleased version")
for (v in versions) check(identical(sxm_content_sha256(sxm_path(v)), unname(SXM_RELEASED[v])), paste("1: content of", v, "matches its pin"))
m <- sxm_read("v1.0")
check(identical(sort(m[action == "exclude_spatial"]$CageEpisodeID), c("B2|sys.2|CC3", "B2|sys.5|CC4", "B5|sys.1|CC4")),
      "1: v1.0 excludes exactly the three confirmed channel-dropout cage episodes")
check(identical(m[action == "exclude_spatial"][order(CageEpisodeID)]$reader, c("R3", "R3", "R5")), "1: their dead readers")
check(identical(m[action == "censor"]$CageEpisodeID, "B5|sys.1|CC4") && identical(m[action == "censor"]$from, "2023-12-04 14:00:42.962"),
      "1: v1.0 censors B5 CC4 board 1 from its last read")
check(all(!grepl("PENDING", unlist(m))), "1: no placeholder text left in v1.0")
ex <- m[action == "exclude_spatial"]
check(all(grepl("^Open", ex$movement_outcomes) == grepl("movement outcomes: open", ex$decision, fixed = TRUE)) &&
        all(grepl("^spatial measures: exclude", ex$decision)), "1: the decision states what is decided (spatial measures) and what is open (movement outcomes)")
check(all(grepl("unchanged", m$registered_relation) | grepl("no registered output is changed", m$registered_relation)),
      "1: every record states that the registered exclusions and outputs are unchanged")

# 2. lookups
check(identical(sxm_spatial_excluded(m, c("B2", "B2", "B2", "B5"), c("CC3", "CC3", "CC4", "CC4"), c(2, 1, 5, 1)), c(TRUE, FALSE, TRUE, TRUE)),
      "2: excluded cage episodes")
cf <- sxm_censor_from(m, c("B5", "B5", "B2"), c("CC4", "CC4", "CC4"), c(1, 2, 5))
check(isTRUE(all.equal(cf[1], as.numeric(as.POSIXct("2023-12-04 14:00:42.962", tz = "UTC")))) && all(is.infinite(cf[2:3])),
      "2: censoring time of B5 CC4 board 1; none elsewhere")

# 3. validation
bad <- function(f) inherits(tryCatch({ x <- data.table::copy(m); f(x); sxm_validate(x) }, error = function(e) e), "error")
check(bad(function(x) x[1, CageEpisodeID := "B2|sys.3|CC3"]), "3: CageEpisodeID must match Batch, System and CC")
check(bad(function(x) x[1, System := "sys.3"]), "3: System must be sys.<board>")
check(bad(function(x) x[1, from := "2023-01-01 00:00:00"]), "3: exclude_spatial covers the whole span")
check(bad(function(x) x[action == "censor", from := "span_start"]), "3: censor needs a time")
check(bad(function(x) x[2, record_id := x$record_id[1]]), "3: unique record ids")
check(bad(function(x) x[1, action := "exclude_everything"]), "3: known actions only")
check(bad(function(x) x[1, registered_relation := ""]), "3: the relation to the registered exclusions is required")
check(bad(function(x) data.table::setnames(x, "decision", "approved")), "3: fixed columns")
check(!bad(function(x) x), "3: v1.0 itself validates")

# 4. an edited released version is refused
tmp <- tempfile("sxm_"); dir.create(file.path(tmp, SXM_DIR), recursive = TRUE)
txt <- readLines(sxm_path("v1.0"), warn = FALSE)
writeLines(sub("2023-12-04 14:00:42.962", "2023-12-04 14:00:43.000", txt), file.path(tmp, SXM_DIR, "spatial_analysis_exclusions_v1.0.csv"))
check(inherits(tryCatch(sxm_read("v1.0", repo_root = tmp), error = function(e) e), "error"), "4: edited released content is refused")
writeLines(txt, file.path(tmp, SXM_DIR, "spatial_analysis_exclusions_v1.0.csv"), sep = "\r\n")
check(nrow(sxm_read("v1.0", repo_root = tmp)) == nrow(m), "4: the same content with CRLF line endings is accepted")
unlink(tmp, recursive = TRUE)

# 5. separation from the registered exclusions
source("Functions/behavior_analysis_config.R")
s13 <- MMM_BEHAVIOR_CONFIG$sensitivities$OTHER$S13_hardware
check(identical(digest::digest(s13, algo = "sha256", serialize = FALSE), "e40410ea852aae07fc9810da059d529e2668658b167b0a00c12aa426c5a18721"),
      "5: the registered S13 definition is unchanged")
code <- c(list.files("Analysis", pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE),
          list.files("Functions", pattern = "[.][Rr]$", full.names = TRUE))
code <- code[!grepl("_archive", code)]
reads <- function(pattern) code[vapply(code, function(f) any(grepl(pattern, readLines(f, warn = FALSE))), logical(1))]
readers <- reads("spatial_exclusion_manifest|spatial_analysis_exclusions|sxm_read")
check(all(readers %in% c("Functions/spatial_exclusion_manifest.R", "Functions/antenna_spatial_reconstruction.R", "Analysis/31_cc4_grid_exposure.R",
                         "Functions/cc4_outcome_eligibility.R")),
      "5: only the loader, the isolated reconstruction module, the unregistered, opt-in Stage 31 and the CC4 outcome-eligibility rule read the spatial manifest")
registered <- code[grepl("^Analysis/(0[0-9]|1[0-9]|2[0-9]|30|32|33)[a-z]?_", code)]
check(!any(registered %in% readers), "5: no registered stage (00-29, 30, 32, 33) reads the spatial manifest")
check(!any(vapply(registered, function(f) any(grepl("antenna_spatial_reconstruction", readLines(f, warn = FALSE))), logical(1))),
      "5: no registered stage sources the reconstruction module")
check(setequal(reads("qc_proposed_exclusions"), c("Analysis/00b_qc_raw_acquisition.R")),
      "5: only Stage 00b itself writes or reads its proposed exclusions")
check(!any(grepl("00b_qc_raw_acquisition", readLines("Analysis/run_all_analysis.R", warn = FALSE))), "5: Stage 00b is not in the automatic runner")

# 6. v1.1
m11 <- sxm_read("v1.1")
check(identical(sort(m11[action == "exclude_movement"]$CageEpisodeID), sort(m[action == "exclude_spatial"]$CageEpisodeID)),
      "6: exclude_movement for exactly the three dead-channel cage episodes")
check(identical(m11[action %in% c("exclude_spatial", "censor"), .(action, CageEpisodeID, from, to)], m[, .(action, CageEpisodeID, from, to)]),
      "6: spatial exclusions and the censoring time unchanged from v1.0")
check(!any(grepl("^Open", m11$movement_outcomes)) && all(grepl("see exclude_movement", m11[action == "exclude_spatial"]$decision, fixed = TRUE)) &&
        all(grepl("keep as sensitivity", m11[action == "exclude_movement"]$decision, fixed = TRUE)), "6: the movement decision is recorded, not open")
check(all(grepl("light-phase crossing rate", m11[action == "exclude_movement"]$applies_to, fixed = TRUE)) &&
        all(grepl("active-phase crossing_rate and active-phase fragmentation", m11[action == "exclude_movement"]$measures_affected, fixed = TRUE)),
      "6: only the active-phase crossing rate and fragmentation; the light-phase rate is kept")
check(grepl("Stage 31", m11[action == "censor"]$applies_to) && grepl("2023-12-07 13:12:01", m11[action == "censor"]$applies_to),
      "6: SX04 states the span end per analysis and its Stage 31 application")
check(identical(sxm_movement_excluded(m11, c("B2", "B2", "B5", "B5"), c("CC3", "CC4", "CC4", "CC3"), c(2, 5, 1, 1)), c(TRUE, TRUE, TRUE, FALSE)) &&
        !any(sxm_movement_excluded(m, c("B2", "B5"), c("CC3", "CC4"), c(2, 1))), "6: movement lookups (v1.1 only)")

cat("test_spatial_exclusion_manifest: all checks passed\n")
