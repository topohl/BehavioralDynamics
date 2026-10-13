# ================================================================
# Audit: the Stage 09 declared CV evaluation (frozen Stage 29 section 7) for every RFID input, without running Stage 29
# MMMSociability -- Testing/audits/audit_stage09_section7_by_input.R
# ================================================================
# UNREGISTERED methodological sensitivity analysis (decided 2026-10-13). Evaluates the frozen top-level statement
# `guarded("7 Stage 09 sensitivities", {...})` of Analysis/29_canonical_behavior_characterization.R verbatim, with
# only the objects it reads: AR (the input root's analysis_ready, for its Stage 09 model_ladder_input.csv), des (the
# design columns Batch, CageEpisodeID, sex_c), DRY = FALSE, CFG (the frozen v1.0.1 configuration),
# canonical_animal_id, S09S/S09X (empty, as Stage 29 initialises them) and guarded() re-defined to evaluate and
# re-throw. LOAO, LOCO-cage and LOBO R2 for movement_mean and primary_behavior_family; within-Batch restricted
# permutation p for LOAO (1000 draws, the frozen RNG settings); the batch-adjusted sex interaction.
#
# Identity proof: the four inputs that have a Stage 29 run (vendor release v101_dv2_b2ce507 and the raw sensitivity
# runs in RANT, RANU, RANV) must reproduce their published stage09_cv_sensitivities.csv and
# stage09_batch_adjusted_sex_interaction.csv byte for byte before any other input is reported. The design columns
# come from the vendor release (identical across those runs, checked); for an input without a Stage 29 run (avg500,
# root RANA) the cage of every animal-window is checked against them through its data version's build_audit.csv.
#
# Reads only; writes <Behavior>/RANT/comparison_vendor_raw/stage09_section7/ (MMM_S09_AUDIT_OUT overrides).
# Refuses to run unless Stage 29 and the configuration hash to their frozen values.
# Run from the repository root:  Rscript Testing/audits/audit_stage09_section7_by_input.R
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(digest); library(jsonlite) })

S09A_STAGE29_SHA <- "5967349b40cc8e816781a234d99ff22ba1ba262a1c6ff13d7107e3c1794265d2"   # = stage29_sha256 of every raw Stage 29 run
S09A_INPUTS <- list(   # root under <Behavior>; Stage 29 run folder (relative to <Behavior>) or NA; data version for the cage check
  vendor = list(root = "RFID", s29 = "RFID/analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507", dv = NA),
  noflip = list(root = "RANU", s29 = "RANU/S29_raw_sensitivity_UNREGISTERED/raw_antenna_noflip_20261012_b23cb51", dv = NA),
  flip2s = list(root = "RANT", s29 = "RANT/S29_raw_sensitivity_UNREGISTERED/raw_antenna_flip2s_20261012_b23cb51", dv = NA),
  ge200 = list(root = "RANV", s29 = "RANV/S29_raw_sensitivity_UNREGISTERED/raw_antenna_ge200_20261013_9c712aa", dv = NA),
  avg500 = list(root = "RANA", s29 = NA, dv = "RANA/MMMSociability/data_versions/raw_antenna_avg500_20261013"))

if (sys.nframe() == 0L) {
  data.table::setDTthreads(1L)
  source("Functions/project_paths.R")
  BEH <- dirname(MMM_PROJECT_ROOT_DEFAULT)
  OUT <- Sys.getenv("MMM_S09_AUDIT_OUT", unset = file.path(BEH, "RANT", "comparison_vendor_raw", "stage09_section7"))
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  sha <- function(f) digest::digest(file = f, algo = "sha256")
  logf <- file.path(OUT, "run_log.txt"); if (file.exists(logf)) file.remove(logf)
  say <- function(...) { m <- paste0(...); message(m); cat(m, "\n", file = logf, append = TRUE) }

  stopifnot(identical(sha("Analysis/29_canonical_behavior_characterization.R"), S09A_STAGE29_SHA))
  ce <- new.env(); sys.source("Functions/behavior_analysis_config.R", envir = ce)
  CFG <- ce$MMM_BEHAVIOR_CONFIG
  frozen_cfg <- readLines(file.path(MMM_PROJECT_ROOT_DEFAULT, "analysis_ready/canonical/behavior_config/v1.0.1/config_sha256.txt"))[1]
  stopifnot(identical(ce$mmm_behavior_config_sha256(CFG), frozen_cfg))
  hx <- parse("Functions/behavioral_dynamics_helpers.R", keep.source = TRUE)
  htxt <- vapply(attr(hx, "srcref"), function(s) paste(as.character(s), collapse = "\n"), "")
  canon <- eval(hx[[which(startsWith(htxt, "canonical_animal_id <- function(x)"))]][[3]], globalenv())
  ex <- parse("Analysis/29_canonical_behavior_characterization.R", keep.source = TRUE)
  txt <- vapply(attr(ex, "srcref"), function(s) paste(as.character(s), collapse = "\n"), "")
  i7 <- which(startsWith(txt, "guarded(\"7 Stage 09 sensitivities\", {")); stopifnot(length(i7) == 1L)
  SEC7 <- ex[[i7]]
  say("frozen Stage 29 ", S09A_STAGE29_SHA, "; config ", frozen_cfg, "; section 7 = statement ", i7, " (text sha256 ",
      digest::digest(txt[i7], algo = "sha256", serialize = FALSE), ")")

  read_des <- function(s29) {
    f <- file.path(BEH, s29, "tables", "canonical_window_metrics.csv")
    om <- fread(file.path(BEH, s29, "audit", "output_manifest.csv"))
    stopifnot(identical(sha(f), om[file == "canonical_window_metrics.csv", sha256]))
    d <- fread(f, colClasses = c(AnimalNum = "character"), select = c("AnimalNum", "CC", "Batch", "System", "CageEpisodeID", "Sex", "sex_c", "Group"))
    d[, AnimalNum := canon(AnimalNum)][, Batch := factor(Batch)][]
  }
  des <- read_des(S09A_INPUTS$vendor$s29)
  keyall <- function(d) setorder(d[, .(AnimalNum, CC, Batch = as.character(Batch), System, CageEpisodeID, Sex, sex_c, Group)], AnimalNum, CC)
  for (v in names(S09A_INPUTS)) {
    x <- S09A_INPUTS[[v]]
    if (!is.na(x$s29) && v != "vendor") stopifnot(identical(keyall(read_des(x$s29)), keyall(des)))
    if (!is.na(x$dv)) {   # no Stage 29 run: check every animal-window's cage against the design
      a <- fread(file.path(BEH, x$dv, "build_audit.csv"), colClasses = c(AnimalNum = "character"))
      a[, `:=`(AnimalNum = canon(AnimalNum), CC = CageChange, System = paste0("sys.", Board))]
      m <- merge(a[, .(AnimalNum, CC, Batch, System)], des[, .(AnimalNum, CC, Batch = as.character(Batch), System)], by = c("AnimalNum", "CC"))
      stopifnot(nrow(a) == 444, nrow(m) == 444, all(m$Batch.x == m$Batch.y), all(m$System.x == m$System.y), all(a$board_matches_label))
      say(v, ": cages of all 444 animal-windows equal the design (build_audit.csv)")
    }
  }
  oc0 <- NULL; res <- list()
  for (v in names(S09A_INPUTS)) {
    x <- S09A_INPUTS[[v]]
    s09 <- file.path(BEH, x$root, "analysis_ready/pipeline/09_early_prediction/10min/tables/model_ladder_input.csv")
    if (!is.na(x$s29)) {
      ri <- fread(file.path(BEH, x$s29, "audit", "run_inputs.csv"))
      stopifnot(identical(sha(s09), ri[grepl("model_ladder_input.csv$", input), sha256]))
    }
    oc <- fread(s09, select = c("AnimalNum", "outcome"), colClasses = c(AnimalNum = "character"))[, AnimalNum := canon(AnimalNum)]
    setorder(oc, AnimalNum)
    if (is.null(oc0)) oc0 <- oc else stopifnot(identical(oc, oc0))
    E <- new.env(parent = globalenv())
    E$AR <- file.path(BEH, x$root, "analysis_ready"); E$des <- data.table::copy(des); E$DRY <- FALSE; E$CFG <- CFG
    E$canonical_animal_id <- canon; E$S09S <- data.table(); E$S09X <- data.table()
    E$guarded <- function(block, expr) { expr; invisible(NULL) }
    eval(SEC7, E)
    fs <- file.path(OUT, sprintf("%s_stage09_cv_sensitivities.csv", v)); fx <- file.path(OUT, sprintf("%s_stage09_batch_adjusted_sex_interaction.csv", v))
    data.table::fwrite(E$S09S, fs); data.table::fwrite(E$S09X, fx)   # as Stage 29's wr()
    ident <- if (is.na(x$s29)) NA else identical(sha(fs), sha(file.path(BEH, x$s29, "tables", "stage09_cv_sensitivities.csv"))) &&
      identical(sha(fx), sha(file.path(BEH, x$s29, "tables", "stage09_batch_adjusted_sex_interaction.csv")))
    if (isFALSE(ident)) stop("Section 7 does not reproduce the published tables for ", v, ".")
    say(v, ": input ", sha(s09), if (is.na(ident)) "; no Stage 29 run to compare" else "; published tables reproduced byte for byte")
    res[[v]] <- cbind(input = v, E$S09S, reproduces_published = ident, model_ladder_input_sha256 = sha(s09))
  }
  out <- rbindlist(res, fill = TRUE)
  fwrite(out, file.path(OUT, "stage09_section7_by_input.csv"))
  fwrite(data.frame(item = c("status", "git_commit", "script_sha256", "stage29_sha256", "config_sha256", "r_version", "data_table", "finished"),
                    value = c("UNREGISTERED methodological sensitivity (Stage 09 declared CV evaluation by input); not a Stage 29 run",
                              system2("git", c("-C", getwd(), "rev-parse", "HEAD"), stdout = TRUE)[1], sha("Testing/audits/audit_stage09_section7_by_input.R"),
                              S09A_STAGE29_SHA, frozen_cfg, R.version.string, as.character(utils::packageVersion("data.table")),
                              format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), file.path(OUT, "provenance.csv"))
  print(out[, .(input, model, scheme, r2 = round(r2, 4), restricted_perm_p = signif(restricted_perm_p, 3), reproduces_published)])
}
