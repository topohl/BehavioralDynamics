# ================================================================
# Stage 29 on a raw-antenna data version: UNREGISTERED sensitivity analysis, not a release
# MMMSociability -- Analysis/raw_antenna_stage29_sensitivity.R
# ================================================================
# Runs the frozen, registered Stage 29 (Analysis/29_canonical_behavior_characterization.R, unchanged) in its CONTROL mode on a
# raw-antenna data version (Formatting/build_raw_antenna_data_versions.R), inside that version's parallel root (RANT or RANU),
# after Analysis/run_raw_antenna_parallel.R has built the root's Stage 01, 09 and 28 outputs. Decided 2026-10-12: an explicitly
# unregistered, non-release raw-input sensitivity variant; the registered Stage 29 and its configuration stay frozen.
#
# How: the frozen script's own statements are evaluated in order. Two statements are replaced, each matched by its full text (the
# run stops unless each is found exactly once), and one guard is installed after the helpers are sourced:
#   DV <- mmm_dv_resolve(...)              -> the raw data version declared below (RAW29_VERSIONS), resolved by the frozen
#                                             mmm_dv_resolve() and verified by the frozen mmm_dv_verify() (manifest, files, hashes)
#   RAW <- file.path(RFID_SRC, "raw_data") -> a folder that must not exist: the vendor AnimalPos export is never read
#   seed guard                             -> every animal of a raw version is seeded from its own last antenna read (in the data
#                                             version), so mmm_evs_seeds() finds no late animal; the guard returns its empty result
#                                             with typed columns. A late animal would make it read the missing folder and stop.
# Everything else is the frozen code: freeze gate (config hash), committed-code gate, analytic identity, every data gate, models,
# multiplicity, robustness, diagnostics. The data gates read the declared facts of the raw version; they start from v2 (same
# roster, same cages; board audit 444/444) with reseeded_windows = 0, and any departure is declared in RAW29_VERSIONS with its reason.
#
# Added after the frozen statements (tables prefixed raw_, in the same folder):
#   raw_standardized_estimates.csv  every standardized frozen estimate also divided by an SD re-derived on the raw data with the
#                                   frozen rule (SD of CC1 SIS residuals from lm(y ~ Batch), n - 1 divisor); estimates.csv keeps
#                                   the frozen constants
#   raw_seed_age_sensitivity.csv    Q1 and the Q2b components refitted without the animal-windows whose 18:30 seed is a read more than
#                                   RAW29_STALE_SEED_S seconds old: a position carried forward from long before the window must not
#                                   weigh like a recent detection
#   raw_seed_provenance.csv         the data version's seed table (seed age, first change after 18:30) with the window keys
# Output: <root>/S29_raw_sensitivity_UNREGISTERED/<data version>_<commit7>/{tables,audit}/ plus UNREGISTERED.txt, written in a
# .tmp_ folder and renamed after a complete run; outside analysis_ready, never bundled, never a release. The frozen
# behavior_config folder and nothing else is copied from the live root (hash-checked), for the freeze gate.
#
# Run from the repository root in a fresh R session, after the descriptive tier of the same version:
#   options(mmm.rant_version = "flip2s"); source("Analysis/raw_antenna_stage29_sensitivity.R")
# ================================================================

RAW29_STAGE29 <- "Analysis/29_canonical_behavior_characterization.R"
RAW29_ROOT_NAMES <- c(flip2s = "RANT", noflip = "RANU")
RAW29_STALE_SEED_S <- 600
RAW29_V2 <- "v2_cage_label_correction_2026-09-28"
# Declared facts of each raw data version. Hashes pinned when the versions were built (2026-10-12).
RAW29_VERSIONS <- list(
  flip2s = list(id = "raw_antenna_flip2s_20261012", tag = "rawf2",
                manifest_sha256 = "4dd9798533d624880408f157b962d762b14e1121cac35ce3c597b07236908d52",
                supporting = c(README.txt = "49f34194070952e4b987edee5c8567a5af46eba334a835678678e65d7c8d5b7f",
                               seed_provenance.csv = "8978a29dc1ea5ae62a4d9cab856e1e309ee85674ec555c698358d6bbab1c812a",
                               build_audit.csv = "974c952047d28c5ee09291ef08fd015c3b3d03b51405bd864fe3d53a849030fe"),
                facts = list(runtime_gates = list(reseeded_windows = 0))),
  noflip = list(id = "raw_antenna_noflip_20261012", tag = "rawnf",
                manifest_sha256 = "dc31cfc058e9321c63822d0a5ca0dd57ef9f43444492202ec9e9add3b477b429",
                supporting = c(README.txt = "8924580a83cc81814b05f06dc553c34616e098db8462a56e7fa7002c0bddb5a8",
                               seed_provenance.csv = "bdc7b4f44ad9364f6634bc0a90bef1c2bc1d0a86416b0ae10886741b1e917e50",
                               build_audit.csv = "927ddc2c0be8d3c22c8783328e27ea5ac52c69f8ec08200a69e59c5391a1151d"),
                facts = list(runtime_gates = list(reseeded_windows = 0))))
RAW29_SUBSTITUTIONS <- c(
  "DV <- mmm_dv_resolve(CFG, ROOT, if (nzchar(dv_req)) dv_req else CFG$data_versions$release)" = "DV <- raw29_resolve(CFG, ROOT)",
  "RAW <- file.path(RFID_SRC, \"raw_data\")" = "RAW <- raw29_no_vendor_raw")
RAW29_GUARD_AFTER <- "source_mmm_helper(h)"   # the helper-sourcing loop ends with this call

#' The declared data-version spec of a raw version: v2's spec with the raw version's files, hashes and facts.
raw29_spec <- function(cfg, v) {
  s <- cfg$data_versions[[RAW29_V2]]
  rel <- file.path("MMMSociability", "data_versions", v$id)
  s$tag <- v$tag
  s$status <- "UNREGISTERED raw-antenna sensitivity input (Analysis/raw_antenna_stage29_sensitivity.R); not in the frozen configuration"
  s$preprocessed_dir <- file.path(rel, "preprocessed_data")
  s$manifest <- file.path(rel, "MANIFEST_SHA256.csv")
  s$manifest_sha256 <- v$manifest_sha256
  s$manifest_sha_column <- "sha256"
  s$supporting_files <- stats::setNames(lapply(names(v$supporting), function(f) list(file = file.path(rel, f), sha256 = v$supporting[[f]])),
                                        sub("[.].*$", "", names(v$supporting)))
  s$corrections <- NULL
  s$label_vs_board <- "System = the reading antenna board; board audit: board equals the v2 cage label in 444 of 444 animal-files"
  utils::modifyList(s, v$facts)
}

#' The frozen mmm_evs_seeds() with its empty result typed (it returns a 0 x 0 table when no file has a late animal).
raw29_guard_seeds <- function(seeds_fn) function(pre, raw_dir, parser = MMM_EVS_SEED_PARSER) {
  s <- seeds_fn(pre, raw_dir, parser = parser)
  if (nrow(s) || ncol(s)) return(s)
  data.table::data.table(DateTime = as.POSIXct(character(), tz = "UTC"), AnimalID = character(), System = character(),
                         PositionID = integer(), Batch = character(), CageChange = character(), SourceFile = character(),
                         AnimalNum = character(), is_seed = logical(), SeedReadTime = as.POSIXct(character(), tz = "UTC"),
                         raw_file = character())
}

raw29_run <- function(version) {
  if (!version %in% names(RAW29_ROOT_NAMES)) stop("mmm.rant_version must be one of ", paste(names(RAW29_ROOT_NAMES), collapse = ", "), call. = FALSE)
  v <- RAW29_VERSIONS[[version]]
  repo <- normalizePath(getwd(), winslash = "/")
  if (!file.exists(file.path(repo, RAW29_STAGE29))) stop("Run from the repository root.", call. = FALSE)
  pp <- new.env(); sys.source(file.path(repo, "Functions", "project_paths.R"), envir = pp)
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  live_root <- norm(pp$MMM_PROJECT_ROOT_DEFAULT)
  root <- norm(file.path(dirname(live_root), RAW29_ROOT_NAMES[[version]]))
  if (startsWith(tolower(paste0(root, "/")), tolower(paste0(live_root, "/")))) stop("The target root lies inside the live root.", call. = FALSE)
  if (!dir.exists(file.path(root, "MMMSociability", "data_versions", v$id))) stop("Data version missing in ", root, call. = FALSE)

  # The root's descriptive tier must have been built from this data version.
  run_log <- file.path(root, "raw_antenna_run_log.csv")
  if (!file.exists(run_log)) stop("No descriptive-tier run in ", root, " (Analysis/run_raw_antenna_parallel.R).", call. = FALSE)
  lg <- utils::read.csv(run_log, stringsAsFactors = FALSE)
  for (s in c("01_build_multiscale_behavior_metrics.R", "09_early_prediction_model_ladder.R", "28_rfid_behavioral_domains.R")) {
    last <- utils::tail(lg[lg$stage == s, ], 1)
    if (!nrow(last) || last$status != "ok" || last$data_version != v$id) stop("Stage ", s, " has no successful run on ", v$id, " in ", root, call. = FALSE)
  }

  git <- function(...) system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = TRUE)
  commit <- git("rev-parse", "HEAD")[1]
  dirty <- git("status", "--porcelain", "--", "Analysis/raw_antenna_stage29_sensitivity.R", RAW29_STAGE29)
  if (length(dirty)) stop("Commit the wrapper before a run:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  name <- paste0(v$id, "_", substr(commit, 1, 7))
  base <- file.path(root, "S29_raw_sensitivity_UNREGISTERED")
  out <- file.path(base, name); work <- file.path(base, paste0(".tmp_", name))
  if (dir.exists(out)) stop("Run exists (immutable): ", out, call. = FALSE)
  if (dir.exists(work)) stop("A partial run exists; inspect and remove it first: ", work, call. = FALSE)
  no_vendor_raw <- file.path(root, "no_vendor_raw_data")
  if (dir.exists(no_vendor_raw)) stop("Must not exist: ", no_vendor_raw, call. = FALSE)

  # Frozen configuration versions into the root (freeze gate), hash-checked.
  rel <- file.path("analysis_ready", "canonical", "behavior_config")
  sha <- function(f) digest::digest(file = f, algo = "sha256")
  for (f in list.files(file.path(live_root, rel), recursive = TRUE)) {
    src <- file.path(live_root, rel, f); dst <- file.path(root, rel, f)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(dst)) file.copy(src, dst)
    if (!identical(sha(src), sha(dst))) stop("Frozen configuration copy differs from the live file: ", f, call. = FALSE)
  }

  # Root and mode switch; everything restored on exit.
  envs <- c("MMM_BEHAVIOR_PROJECT_ROOT", "MMM_PROJECT_ROOT", "MMM_STAGE29_CONTROL_DIR", "MMM_STAGE29_DRY_RUN_DIR", "MMM_STAGE29_DATA_VERSION")
  old_env <- Sys.getenv(envs, unset = NA)
  mmm_opts <- function() grep("^mmm[.]", names(options()), value = TRUE)
  old_opts <- options()[mmm_opts()]
  frozen_seeds <- get0("mmm_evs_seeds", envir = globalenv(), inherits = FALSE)
  on.exit({
    for (e in names(old_env)) if (is.na(old_env[[e]])) Sys.unsetenv(e) else do.call(Sys.setenv, stats::setNames(list(old_env[[e]]), e))
    options(stats::setNames(rep(list(NULL), length(mmm_opts())), mmm_opts())); options(old_opts)
    if (is.null(frozen_seeds)) suppressWarnings(rm("mmm_evs_seeds", envir = globalenv())) else assign("mmm_evs_seeds", frozen_seeds, envir = globalenv())
  }, add = TRUE)
  Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root, MMM_STAGE29_CONTROL_DIR = work)
  Sys.unsetenv(c("MMM_PROJECT_ROOT", "MMM_STAGE29_DRY_RUN_DIR", "MMM_STAGE29_DATA_VERSION"))
  options(stats::setNames(rep(list(NULL), length(mmm_opts())), mmm_opts()))
  options(mmm.project_root = root)

  # The frozen statements, with the declared substitutions.
  exprs <- parse(file.path(repo, RAW29_STAGE29), keep.source = TRUE)
  txt <- vapply(attr(exprs, "srcref"), function(s) paste(as.character(s), collapse = "\n"), character(1))
  hits <- vapply(names(RAW29_SUBSTITUTIONS), function(t) sum(txt == t), integer(1))
  guard_at <- which(grepl(RAW29_GUARD_AFTER, txt, fixed = TRUE) & startsWith(txt, "for (h in"))
  if (any(hits != 1L) || length(guard_at) != 1L) stop("The frozen Stage 29 no longer has the statements this wrapper replaces.", call. = FALSE)
  E <- new.env(parent = globalenv())
  E$raw29_no_vendor_raw <- no_vendor_raw
  E$raw29_resolve <- function(CFG, ROOT) {
    cfg <- CFG; cfg$data_versions[[v$id]] <- raw29_spec(CFG, v)
    mmm_dv_resolve(cfg, ROOT, v$id)
  }
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  message("Raw-antenna Stage 29 sensitivity (UNREGISTERED): ", v$id, " -> ", work)
  for (i in seq_along(exprs)) {
    j <- match(txt[i], names(RAW29_SUBSTITUTIONS))
    eval(if (is.na(j)) exprs[[i]] else str2lang(RAW29_SUBSTITUTIONS[[j]]), E)
    if (i == guard_at) assign("mmm_evs_seeds", raw29_guard_seeds(get("mmm_evs_seeds", envir = globalenv())), envir = globalenv())
  }

  # Additions (labelled raw_), evaluated with the frozen run's objects.
  E$raw29_stale_s <- RAW29_STALE_SEED_S
  E$raw29_seed_file <- file.path(dirname(E$DV$dir), "seed_provenance.csv")
  eval(quote({
    raw_sd <- vapply(CON_SET, function(k) { x <- dat(k); stats::sd(stats::residuals(stats::lm(y ~ Batch, data = x))) }, numeric(1))
    std <- ALL_EST[, .(model_id, estimand, construct, sex, estimate, se, ci_low, ci_high, estimate_standardized_frozen_sd = estimate_standardized)]
    std[, `:=`(sd_frozen = unname(sdz[construct]), sd_raw = unname(raw_sd[construct]))]
    std[, estimate_standardized_raw_sd := estimate_standardized_frozen_sd * sd_frozen / sd_raw]
    wr(std, "raw_standardized_estimates.csv")

    sp <- data.table::fread(raw29_seed_file, colClasses = c(AnimalNum = "character", AnimalID = "character"))
    sp[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), CC = paste0("CC", sub("^CC", "", CageChange)))]
    sp <- merge(sp, unique(des[, .(AnimalNum, CC, Group, Sex)]), by = c("AnimalNum", "CC"), all.x = TRUE)
    sp[, stale_seed := seeded & is.finite(seed_detection_age_s) & seed_detection_age_s > raw29_stale_s]
    wr(sp, "raw_seed_provenance.csv")
    stale <- sp[stale_seed == TRUE, paste(AnimalNum, CC)]
    fresh <- sisd[!(paste(AnimalNum, CC) %in% stale)]
    n_sens0 <- length(sens)
    for (k in CON_SET) sens_all("RAW_stale_seed_windows_excluded", k,
                                refit(k, dat(k, TRUE, fresh), dat(k, FALSE, fresh), "RAW_seed",
                                      r1 = M$CC1_POOLED$expected_rank, rL = M$TR_POOLED$expected_rank))
    seed_sens <- rbindlist(sens[seq_len(length(sens) - n_sens0) + n_sens0], fill = TRUE)
    seed_sens[, `:=`(stale_seed_threshold_s = raw29_stale_s, sis_windows_excluded = nrow(sisd) - nrow(fresh),
                     sis_cc1_windows_excluded = nrow(sisd[CC == "CC1"]) - nrow(fresh[CC == "CC1"]))]
    wr(seed_sens, "raw_seed_age_sensitivity.csv")
    data.table::fwrite(rbindlist(reg, fill = TRUE), file.path(AUD, "raw_model_registry_with_additions.csv"))
  }), E)

  subs <- data.frame(statement = c(match(names(RAW29_SUBSTITUTIONS), txt), guard_at),
                     frozen_text = c(names(RAW29_SUBSTITUTIONS), txt[guard_at]),
                     evaluated = c(unname(RAW29_SUBSTITUTIONS), paste(txt[guard_at], "; then the typed-empty seed guard")))
  utils::write.csv(subs, file.path(work, "audit", "raw_substitutions.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    item = c("status", "data_version", "data_version_tag", "root", "git_commit", "wrapper_sha256", "stage29_sha256",
             "stale_seed_threshold_s", "started_at", "finished_at"),
    value = c("UNREGISTERED raw-antenna sensitivity; not a release; never bundled", v$id, v$tag, root, commit,
              sha(file.path(repo, "Analysis", "raw_antenna_stage29_sensitivity.R")), sha(file.path(repo, RAW29_STAGE29)),
              RAW29_STALE_SEED_S, started, format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))),
    file.path(work, "audit", "raw_run_manifest.csv"), row.names = FALSE)
  writeLines(c("UNREGISTERED SENSITIVITY ANALYSIS - NOT A STAGE 29 RELEASE",
               paste0("The frozen Stage 29 in CONTROL mode on the raw-antenna data version ", v$id, " (", RAW29_ROOT_NAMES[[version]], ")."),
               "Tables without the raw_ prefix are the frozen code's output on this input; raw_ tables are the wrapper's additions.",
               "Standardized estimates in estimates.csv use the frozen (vendor-data) SDs; raw_standardized_estimates.csv adds raw-data SDs.",
               "Produced by Analysis/raw_antenna_stage29_sensitivity.R; see audit/raw_run_manifest.csv and audit/raw_substitutions.csv."),
             file.path(work, "UNREGISTERED.txt"))
  if (!file.rename(work, out)) stop("Could not rename ", work, " to ", out, call. = FALSE)
  message("Raw-antenna Stage 29 sensitivity complete: ", out)
  invisible(out)
}

if (is.null(getOption("mmm.rant_version", NULL))) {
  message("raw_antenna_stage29_sensitivity.R: nothing to do; set options(mmm.rant_version = \"flip2s\" or \"noflip\").")
} else {
  raw29_run(getOption("mmm.rant_version"))
}
