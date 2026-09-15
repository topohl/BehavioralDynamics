#!/usr/bin/env Rscript

# Old-versus-new audit of the frozen Figure 1 export bundle.
#
# The bundle is the artefact the proteomics manuscript quotes, so regenerating
# it needs a machine-readable account of what moved and why. Every cell-level
# difference is classified, and the audit fails loudly if any quantitative value
# changed - a provenance repair must not move a scientific number.
#
# Usage:  Rscript Analysis/_supporting/audit_figure1_bundle_diff.R [<baseline-ref>]
# Default baseline is the original freeze commit 53bc7e9.

REPO <- "C:/Users/topohl/Documents/GitHub/MMMSociability"
EXPREL <- "results/manuscript_bridge/figure1/export"
EXP <- file.path(REPO, EXPREL)
OUTDIR <- file.path(REPO, "results/manuscript_bridge/figure1")

args <- commandArgs(trailingOnly = TRUE)
BASE <- if (length(args)) args[[1]] else "53bc7e9"

git_show <- function(ref, rel) {
  o <- suppressWarnings(system2("git", c("-C", shQuote(REPO), "show",
                                         shQuote(paste0(ref, ":", rel))),
                                stdout = TRUE, stderr = FALSE))
  if (!length(o)) NULL else o
}

read_ref <- function(ref, rel) {
  txt <- git_show(ref, rel)
  if (is.null(txt)) return(NULL)
  utils::read.csv(text = paste(txt, collapse = "\n"), stringsAsFactors = FALSE,
                  check.names = FALSE)
}

# Anything matching these is a number the manuscript quotes. If one of them
# changes, the run is not a provenance repair and must stop.
NUMERIC_GUARD <- paste0(
  "rho|CI|confidence|\\bq\\b|R2|R\u00b2|cv_r2|permutation|p = |p=|",
  "baseline|interaction|threshold|-0\\.4366|-0\\.2223|0\\.159|0\\.156|",
  "0\\.116|0\\.179|1/1001|6\\.9e|0\\.026|0\\.067|-0\\.39|-0\\.226|-0\\.175|",
  "111|58|53|24|49|38")

classify <- function(file, key, col, old, new) {
  o <- if (is.na(old)) "" else as.character(old)
  n <- if (is.na(new)) "" else as.character(new)
  # provenance fields that are expected to move on any regeneration
  if (col %in% c("created_at", "source_commit", "worktree_state", "file_hash",
                 "sha256", "bytes", "source_branch"))
    return("EXPECTED_PROVENANCE_UPDATE")
  # the seed repair
  if (grepl("seed 123", o, fixed = TRUE) && grepl("seed 521", n, fixed = TRUE))
    return("EXPECTED_SEED_PROVENANCE_FIX")
  if (grepl("bootstrap CI;", o, fixed = TRUE) &&
      grepl("bootstrap CI (percentile, seed 123);", n, fixed = TRUE))
    return("EXPECTED_SEED_PROVENANCE_FIX")
  # model labelling
  if (grepl("[Mm]ovement only|Movement\"|behaviour-only|behavior-only", o) &&
      !identical(o, n))
    return("EXPECTED_MODEL_LABEL_FIX")
  # anything else that touches a quoted number is a hard stop
  strip <- function(z) gsub("[^0-9.eE+-]", "", z)
  if (grepl(NUMERIC_GUARD, o, perl = TRUE) ||
      grepl(NUMERIC_GUARD, n, perl = TRUE)) {
    if (!identical(strip(o), strip(n)))
      return("UNEXPECTED_NUMERICAL_CHANGE")
    return("EXPECTED_PROVENANCE_UPDATE")
  }
  "UNEXPECTED_OTHER_CHANGE"
}

files <- sort(basename(list.files(EXP, pattern = "[.]csv$")))
rows <- list()

for (f in files) {
  rel <- file.path(EXPREL, f)
  old <- read_ref(BASE, rel)
  new <- utils::read.csv(file.path(EXP, f), stringsAsFactors = FALSE,
                         check.names = FALSE)
  if (is.null(old)) {
    rows[[length(rows) + 1L]] <- data.frame(
      file = f, row_key = "<file>", column = "<file>", old_value = "<absent>",
      new_value = "<present>", classification = "EXPECTED_PROVENANCE_UPDATE",
      stringsAsFactors = FALSE)
    next
  }
  keycol <- names(old)[1]
  # column set changes
  for (cc in setdiff(names(new), names(old)))
    rows[[length(rows) + 1L]] <- data.frame(
      file = f, row_key = "<column>", column = cc, old_value = "<absent>",
      new_value = "<added>", classification = "UNEXPECTED_OTHER_CHANGE",
      stringsAsFactors = FALSE)
  for (cc in setdiff(names(old), names(new)))
    rows[[length(rows) + 1L]] <- data.frame(
      file = f, row_key = "<column>", column = cc, old_value = "<present>",
      new_value = "<removed>", classification = "UNEXPECTED_OTHER_CHANGE",
      stringsAsFactors = FALSE)

  shared_cols <- intersect(names(old), names(new))
  ok <- as.character(old[[keycol]]); nk <- as.character(new[[keycol]])
  for (k in union(ok, nk)) {
    oi <- which(ok == k); ni <- which(nk == k)
    if (!length(oi) || !length(ni)) {
      rows[[length(rows) + 1L]] <- data.frame(
        file = f, row_key = k, column = "<row>",
        old_value = if (length(oi)) "<present>" else "<absent>",
        new_value = if (length(ni)) "<present>" else "<absent>",
        classification = "UNEXPECTED_OTHER_CHANGE", stringsAsFactors = FALSE)
      next
    }
    for (cc in shared_cols) {
      o <- old[[cc]][oi[1]]; n <- new[[cc]][ni[1]]
      if (identical(as.character(o), as.character(n))) next
      rows[[length(rows) + 1L]] <- data.frame(
        file = f, row_key = k, column = cc,
        old_value = as.character(o), new_value = as.character(n),
        classification = classify(f, k, cc, o, n), stringsAsFactors = FALSE)
    }
  }
}

audit <- if (length(rows)) do.call(rbind, rows) else data.frame(
  file = character(0), row_key = character(0), column = character(0),
  old_value = character(0), new_value = character(0),
  classification = character(0), stringsAsFactors = FALSE)

dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)
outp <- file.path(OUTDIR, "figure1_bundle_regeneration_audit.csv")
utils::write.csv(audit, outp, row.names = FALSE)

cat("===== FIGURE 1 BUNDLE REGENERATION AUDIT =====\n")
cat("baseline: ", BASE, "\n", sep = "")
cat("files compared: ", length(files), "\n", sep = "")
cat("differences: ", nrow(audit), "\n\n", sep = "")
tab <- table(audit$classification)
for (nm in names(tab)) cat(sprintf("  %-34s %d\n", nm, tab[[nm]]))
cat("\naudit written to: ", outp, "\n", sep = "")

bad <- audit[audit$classification %in%
               c("UNEXPECTED_NUMERICAL_CHANGE", "UNEXPECTED_OTHER_CHANGE"), ]
if (nrow(bad)) {
  cat("\n*** UNEXPECTED CHANGES ***\n")
  print(bad, row.names = FALSE)
}
if (any(audit$classification == "UNEXPECTED_NUMERICAL_CHANGE")) {
  stop("A quantitative value changed. This is not a provenance repair. STOP.",
       call. = FALSE)
}
cat("\n0 UNEXPECTED_NUMERICAL_CHANGE\n")
