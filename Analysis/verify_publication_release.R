# ================================================================
# Publication release bundle self-check
# BehavioralDynamics
# ================================================================
# Verifies a built release bundle using ONLY what the bundle itself contains
# plus the source paths it records. Run it FROM anywhere:
#
#     Rscript Analysis/verify_publication_release.R <RELEASE_DIR>
#
# It re-hashes the finished bundle rather than trusting the manifest, which is
# how the RC1 build caught a copied artifact being silently overwritten by a
# file the builder wrote afterwards.
# Exits non-zero if any check fails.

## Independent self-check, run FROM the release directory, using only what the
## bundle itself contains plus the recorded source paths.
REL <- commandArgs(TRUE)[1]
stopifnot(dir.exists(REL))
setwd(REL)
h <- function(p) digest::digest(p, algo = "sha256", file = TRUE)
fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) cat("  PASS  ", msg, "\n", sep = "") else { cat("  FAIL  ", msg, "\n", sep = ""); fail <<- fail + 1L }
}

cat("=== 1. required structure present ===\n")
for (d in c("code", "figures/main", "figures/supplementary", "source_data",
            "tables/primary", "tables/supplementary", "provenance", "qc")) {
  ok(dir.exists(d), paste0("directory ", d))
}
for (f in c("README.md", "SHA256SUMS.txt", "code/git_sha.txt", "code/sessionInfo.txt",
            "code/package_versions.csv", "provenance/artifact_manifest.csv",
            "provenance/upstream_hashes.csv", "provenance/validation.csv",
            "provenance/analysis_registry.csv")) {
  ok(file.exists(f), paste0("file ", f))
}

cat("\n=== 2. SHA256SUMS covers every file and every hash verifies ===\n")
sums <- readLines("SHA256SUMS.txt", warn = FALSE)
sums <- sums[nzchar(sums)]
rec <- sub("^([0-9a-f]{64})  (.*)$", "\\1", sums)
rel <- sub("^([0-9a-f]{64})  (.*)$", "\\2", sums)
on_disk <- list.files(".", recursive = TRUE)
on_disk <- on_disk[on_disk != "SHA256SUMS.txt"]
ok(setequal(rel, on_disk),
   paste0("SHA256SUMS lists exactly the bundle contents (", length(rel), " listed, ",
          length(on_disk), " on disk)"))
if (!setequal(rel, on_disk)) {
  cat("    only in SHA256SUMS: ", paste(setdiff(rel, on_disk), collapse = ", "), "\n")
  cat("    only on disk      : ", paste(setdiff(on_disk, rel), collapse = ", "), "\n")
}
live <- vapply(rel[file.exists(rel)], h, character(1))
ok(all(tolower(live) == tolower(rec[file.exists(rel)])),
   paste0("every listed file hashes correctly (", length(live), " files)"))

cat("\n=== 3. artifact manifest integrity ===\n")
man <- read.csv("provenance/artifact_manifest.csv", stringsAsFactors = FALSE)
ok(nrow(man) > 0, paste0("manifest is non-empty (", nrow(man), " artifacts)"))
ok(all(man$hash_match %in% TRUE), "every artifact recorded hash_match = TRUE")
ok(all(file.exists(man$bundle_path)), "every manifest bundle_path exists in the bundle")
copied_now <- vapply(man$bundle_path[file.exists(man$bundle_path)], h, character(1))
ok(all(tolower(copied_now) == tolower(man$copied_sha256[file.exists(man$bundle_path)])),
   "bundled file hashes still equal the recorded copied_sha256")
ok(all(tolower(man$copied_sha256) == tolower(man$source_sha256)),
   "copied_sha256 equals source_sha256 for every artifact")

cat("\n=== 4. no source resolves into a quarantine or archive tree ===\n")
bad <- c("_quarantine", "quarantine_legacy", "_archive", ".old")
hits <- unlist(lapply(bad, function(b) man$source_path[grepl(b, man$source_path, fixed = TRUE)]))
ok(length(hits) == 0, "no source_path contains a forbidden segment")
if (length(hits)) print(hits)
ok(all(man$resolution_class == "canonical"), "every artifact resolved as canonical")

cat("\n=== 5. no artifact references a missing source ===\n")
src_exists <- file.exists(man$source_path)
ok(all(src_exists), paste0("every recorded source_path still exists (",
                           sum(src_exists), "/", nrow(man), ")"))
if (!all(src_exists)) print(man$source_path[!src_exists])
live_src <- vapply(man$source_path[src_exists], h, character(1))
ok(all(tolower(live_src) == tolower(man$source_sha256[src_exists])),
   "every source file still matches its recorded source_sha256 (sources unmodified)")

cat("\n=== 6. registry rows marked ready point to present artifacts ===\n")
reg <- read.csv("provenance/analysis_registry.csv", stringsAsFactors = FALSE)
ready <- reg[grepl("^yes", tolower(trimws(reg$publication_ready))), ]
cat("    registry rows:", nrow(reg), " marked publication-ready:", nrow(ready), "\n")
bundle_files <- basename(on_disk)
missing_rows <- character(0)
for (i in seq_len(nrow(ready))) {
  arts <- unlist(strsplit(ready$source_artifact[i], "[;,]"))
  arts <- trimws(arts)
  arts <- arts[grepl("[.](csv|xlsx|svg|pdf|png)$", arts)]
  arts <- basename(arts)
  if (!length(arts)) next
  if (!any(arts %in% bundle_files)) missing_rows <- c(missing_rows, ready$analysis_id[i])
}
ok(length(missing_rows) == 0,
   "every publication-ready registry row has at least one artifact in the bundle")
if (length(missing_rows)) cat("    rows with no bundled artifact: ",
                              paste(missing_rows, collapse = ", "), "\n")

cat("\n=== 7. build validation and identity ===\n")
val <- read.csv("provenance/validation.csv", stringsAsFactors = FALSE)
ok(all(val$status %in% c("PASS", "SKIP")), paste0("build validation: ",
   sum(val$status == "PASS"), " PASS, ", sum(val$status == "FAIL"), " FAIL, ",
   sum(val$status == "SKIP"), " SKIP"))
gs <- readLines("code/git_sha.txt", warn = FALSE)
sha <- sub("^git_sha=", "", grep("^git_sha=", gs, value = TRUE))
dirty <- sub("^working_tree_dirty=", "", grep("^working_tree_dirty=", gs, value = TRUE))
ok(grepl("^[0-9a-f]{40}$", sha), paste0("git SHA recorded: ", sha))
ok(identical(dirty, "false"), paste0("working tree was clean at build time (dirty=", dirty, ")"))

cat("\n=== 8. the 5-min sensitivity is present and separated from primary ===\n")
ok(any(grepl("s09sens", man$artifact_id)), "5-min sensitivity artifacts are bundled")
sens <- man[grepl("s09sens", man$artifact_id), ]
ok(all(grepl("supplementary|provenance", sens$bundle_path)),
   "every 5-min sensitivity artifact is filed as supplementary or provenance, never primary")
prim <- man[grepl("^tables/primary/", man$bundle_path), ]
ok(!any(grepl("/5min/", prim$source_path)),
   "no file in tables/primary/ came from the 5-min tree")

cat("\n==================================================\n")
cat(if (fail == 0L) "BUNDLE SELF-CHECK: ALL CHECKS PASSED\n" else
    paste0("BUNDLE SELF-CHECK: ", fail, " CHECK(S) FAILED\n"))
quit(save = "no", status = if (fail == 0L) 0L else 1L)
