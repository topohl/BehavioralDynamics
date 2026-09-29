# Contract test for the data-version binding gate (Functions/behavior_config_identity.R: mmm_dv_resolve,
# mmm_dv_verify, mmm_dv_verify_run_inputs) used by Stage 29 (before any row is read) and Stage 16b.
#
# Part A uses synthetic files in tempdir() only. Part B (skipped when the project root is not reachable) hashes the
# two declared data versions of the repository config read-only: v1_original must equal the frozen Stage 29 inputs
# and v2 must differ from it in exactly the two corrected files. Nothing is written outside tempdir(); no row of
# behavioural data is parsed; no Analysis/ stage runs.
#
# Portable-suite idiom: plain Rscript from the repo root, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
source("Analysis/_pipeline_setup.R")
for (h in c("project_paths.R", "behavior_analysis_config.R", "behavior_config_identity.R")) source_mmm_helper(h)

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
err   <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
sha   <- function(f) digest::digest(file = f, algo = "sha256")

# ---------------------------------------------------------------- A. synthetic data version
root <- file.path(tempdir(), "dv_gate"); unlink(root, recursive = TRUE)
d1 <- file.path(root, "pre_v1"); d2 <- file.path(root, "dv", "v2", "pre"); dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)
files <- sprintf("E9_SIS_B%d_CC1_AnimalPos_preprocessed.csv", 1:3)
for (i in 1:3) { writeLines(c("DateTime,AnimalID,System", sprintf("2022-10-28T18:30:0%dZ,A%d,sys.1", i, i)), file.path(d1, files[i]))
                 file.copy(file.path(d1, files[i]), file.path(d2, files[i])) }
writeLines(c("DateTime,AnimalID,System", "2022-10-28T18:30:01Z,A1,sys.2"), file.path(d2, files[1]))   # the corrected file
writeLines("corrections", file.path(root, "dv", "v2", "corrections.csv"))
man <- data.frame(file = files, sha256_original = vapply(file.path(d1, files), sha, ""), sha256_v2 = vapply(file.path(d2, files), sha, ""))
write.csv(man, file.path(root, "dv", "v2", "MANIFEST.csv"), row.names = FALSE)
msha <- sha(file.path(root, "dv", "v2", "MANIFEST.csv"))
mk <- function(dir, col, sup = list()) list(tag = "t", preprocessed_dir = dir, manifest = "dv/v2/MANIFEST.csv", manifest_sha256 = msha,
                                          manifest_sha_column = col, n_files = 3, supporting_files = sup)
cfg <- list(meta = list(config_version = "x"), data_versions = list(release = "v2", rule = "r",
  v1 = mk("pre_v1", "sha256_original"),
  v2 = mk("dv/v2/pre", "sha256_v2", list(corr = list(file = "dv/v2/corrections.csv", sha256 = sha(file.path(root, "dv", "v2", "corrections.csv")))))))

dv2 <- mmm_dv_resolve(cfg, root); check(identical(dv2$id, "v2") && dv2$is_release, "release resolves by default")
v <- mmm_dv_verify(dv2)
check(sum(v$role == "preprocessed") == 3 && sum(v$role == "data_version_manifest") == 1 && sum(v$role == "data_version_supporting") == 1, "inventory")
check(nrow(mmm_dv_verify(mmm_dv_resolve(cfg, root, "v1"))) == 4, "v1 verifies against sha256_original")
ok("both synthetic data versions verify (3 files + manifest [+ supporting])")

check(grepl("Unknown data version", err(mmm_dv_resolve(cfg, root, "v9"))), "unknown version")
ok("undeclared data version -> error")
c2 <- cfg; c2$data_versions$v2$manifest_sha256 <- strrep("0", 64)
check(grepl("manifest hash mismatch", err(mmm_dv_verify(mmm_dv_resolve(c2, root)))), "manifest hash")
ok("manifest hash mismatch -> error")
c3 <- cfg; c3$data_versions$v2$manifest_sha_column <- "sha256_v3"
check(grepl("lacks", err(mmm_dv_verify(mmm_dv_resolve(c3, root)))), "sha column")
ok("missing manifest sha column -> error")
c4 <- cfg; c4$data_versions$v2$preprocessed_dir <- "pre_v1"
check(grepl("file hash differs", err(mmm_dv_verify(mmm_dv_resolve(c4, root)))), "v1 files under the v2 binding")
ok("original files under the corrected binding -> error (hash differs)")

keep <- readLines(file.path(d2, files[2])); writeLines(c(keep, "2022-10-28T18:31:00Z,A2,sys.1"), file.path(d2, files[2]))
check(grepl("file hash differs.*B2", err(mmm_dv_verify(dv2))), "tampered file")
writeLines(keep, file.path(d2, files[2])); ok("tampered file -> error")
writeLines("x", file.path(d2, "E9_SIS_B4_CC1_AnimalPos_preprocessed.csv"))
check(grepl("differ from the manifest \\(extra: E9_SIS_B4", err(mmm_dv_verify(dv2))), "extra file")
unlink(file.path(d2, "E9_SIS_B4_CC1_AnimalPos_preprocessed.csv")); ok("extra preprocessed file -> error")
invisible(file.rename(file.path(d2, files[3]), file.path(root, "moved.csv")))
check(grepl("missing: E9_SIS_B3", err(mmm_dv_verify(dv2))), "missing file")
invisible(file.rename(file.path(root, "moved.csv"), file.path(d2, files[3]))); ok("missing preprocessed file -> error")
writeLines("changed", file.path(root, "dv", "v2", "corrections.csv"))
check(grepl("supporting file missing or changed", err(mmm_dv_verify(dv2))), "supporting file")
writeLines("corrections", file.path(root, "dv", "v2", "corrections.csv")); ok("changed supporting file -> error")
check(nrow(mmm_dv_verify(dv2)) == 5, "restored data version verifies again")

# run_inputs check (Stage 16b)
ri <- data.frame(input = c(file.path(d2, files), file.path(root, "raw.csv")), bytes = 1, sha256 = c(man$sha256_v2, "x"))
check(mmm_dv_verify_run_inputs(dv2, ri)$ok, "matching run inputs")
ri_v1 <- ri; ri_v1$input[1:3] <- file.path(d1, files); ri_v1$sha256[1:3] <- man$sha256_original
r <- mmm_dv_verify_run_inputs(dv2, ri_v1); check(!r$ok && any(grepl("outside the data-version directory", r$problems)), "run on v1 files")
ri_bad <- ri; ri_bad$sha256[2] <- "0"
check(!mmm_dv_verify_run_inputs(dv2, ri_bad)$ok, "run input hash differs")
check(!mmm_dv_verify_run_inputs(dv2, ri[-1, ])$ok, "run used fewer files")
ok("run_inputs gate: accepts the bound files; rejects other directory, other hash, missing file")
unlink(root, recursive = TRUE)

# ---------------------------------------------------------------- B. the declared data versions of the repository config
PR <- mmm_project_root()
f3a <- file.path(PR, "analysis_ready", "pipeline", "29_canonical_behavior", "audit", "run_inputs.csv")
if (!dir.exists(file.path(PR, "MMMSociability")) || !file.exists(f3a)) {
  cat("  skip  B: project root not reachable\n")
} else {
  C <- MMM_BEHAVIOR_CONFIG
  inv <- lapply(c("v1_original", C$data_versions$release), function(id) mmm_dv_verify(mmm_dv_resolve(C, PR, id)))
  names(inv) <- c("v1", "rel")
  frozen <- data.table::fread(f3a)[grepl(MMM_DV_PRE_PATTERN, basename(input))]
  a <- inv$v1[inv$v1$role == "preprocessed", ]
  check(setequal(a$sha256, frozen$sha256) && nrow(a) == 24, "v1_original equals the frozen Stage 29 inputs (24/24)")
  b <- inv$rel[inv$rel$role == "preprocessed", ]
  diff_files <- basename(b$input)[b$sha256 != a$sha256[match(basename(b$input), basename(a$input))]]
  check(setequal(diff_files, c("E9_SIS_B1_CC2_AnimalPos_preprocessed.csv", "E9_SIS_B6_CC4_AnimalPos_preprocessed.csv")), "v2 differs in 2 files")
  ok("repository bindings verify: v1_original = frozen inputs 24/24; release differs only in B1 CC2 and B6 CC4")
}
cat("test_behavior_data_version_gate: all checks passed\n")
