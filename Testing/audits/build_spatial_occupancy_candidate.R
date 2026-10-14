# Stage 19 candidate s19c_v0.1: UNREGISTERED, isolated, group-blind; Stage 19, its outputs and the manuscript figures are
# not touched. Four-column primary occupancy (eight readers as diagnostics) of both reconstructions of the pinned
# single-read spatial release srs_v1.1_9b18624 (event-start = historical reference, single-read = provisional candidate),
# with unrestricted LOCF as the historical reference and the 300-s and 60-s observation-age bounds as sensitivity, the
# unknown time as its own category, both denominators (all observed time; supported time), Active and Light phases
# separately, the Stage 19 block set (primary) and the whole span (secondary), the revised exclusions and the censoring.
# No raw log is read and nothing is reconstructed again: every number is an aggregation of the srs tables. Definitions:
# Functions/spatial_occupancy_candidate.R; tests: Testing/tests/test_spatial_occupancy_candidate.R; specification:
# docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.1.md (S3, S4, section 8).
#
# Run from the repository root (one light R process, a few minutes; data.table threads 1):
#   Rscript Testing/audits/build_spatial_occupancy_candidate.R [--run-id=<new id>] [--output-root=<absolute>]
# Default output root <Behavior>/RANM/spatial_occupancy_candidate (checked with mmm_isolated_output_root_check()); writes
# runs/<run_id>/ only, built in runs/.tmp_<run_id>/ and renamed at the end; an existing run or build folder is refused.
# The default run id is s19c_v0.1_<commit>; it needs committed code. Ids starting with "dev" may run on uncommitted code.
# Inputs are pinned by sha256 (constants below, cross-checked against the srs and cep output manifests, the srs input
# hashes and the Stage 19 rerun record) and the run stops on any mismatch. Protected locations are snapshotted before the
# first write and compared before the rename (Functions/protected_outputs.R). The run fails unless every gate passes
# (tables/gates.csv); a failed run leaves its .tmp_ folder for inspection and publishes nothing.
# Read-only from the live root: Stage 19's phase table (keys and seconds only, never Group or Sex) and its raw-antenna
# audit (raw log names). No group label, CombZ, outcome, model or test statistic.

S19C_SRS_RUN_ID <- "srs_v1.1_9b18624"
S19C_CEP_RUN_ID <- "cep_v1_9b18624"
S19C_MANIFEST_VERSION <- "v1.1"
S19C_RECONSTRUCTION <- "srs_v1.1"
S19C_TAU_S <- 2
# srs tables read (sha256 as listed in the release's manifest/output_files_sha256.csv)
S19C_SRS_TABLES <- c(
  occupancy = "6d968df08eb9d127a17a271139dadb8f9c5b57796fff1b557a7c383b51d740ea",
  animal_block = "11bd8412bf4716245a530a98832e72a497fd21028dc477599ee05a5da24e71bb",
  animals = "19a674cfe7bf60f086527f82fd2b20f2341006c0fd6d9dad84d352dbfb97241f",
  blocks = "9ad57de9b8ec1d604fe6dcd3f55e8c71cce25c05e8ad349a48231cacc15a6927",
  censored = "9d6211ea031350461148c0fbaf85518361275bc11f244ca6d9150adf892d0266",
  read_agreement = "4b5a4cedf587df8a34a0c4670317060fff2c6a94d5ccef52dcad55c9475e7552",
  gaps = "c99e45e4fe38ac17594eed369ee1d01d36ea69019b14c0d0d70d9f0d39d25f52",
  colocation = "cc2c3f4f1efa09ea9c2efa78c5d273140bc7e756562e5a7fc6c98d404a2a1d3f",
  shares_unknown_column_pooled = "bc4f7636ea3ecac1ea838fb0802837110c1d86db5fe3762cfe8672a7108f6d42",
  shares_unknown_reader_pooled = "aa1dd798feefdc97f3e6999b1c3760b3d4d48e7872513a3b683ccefae0bc3245",
  support_by_phase = "653ffdbc77e622175306bd4465290350779ee85ef38dfddf8276efcf7c649a7e",
  parity_stage19 = "cd573c6a87074faa51321ba650418c7433a4173f21d2330178c8ad5136df3248")
S19C_SRS_RUN_CSV_SHA <- "2ad222a04451d06c8852d578ef2bf0dc5f2e1e532682d9e2086b3f8907b324bb"
S19C_SRS_INPUT_HASHES_SHA <- "8e8c6148b22b7c9f20c331b7afc406b9cdea88b77f4eb83d8b21c076c96bf7d3"
S19C_SRS_OUTPUT_MANIFEST_SHA <- "81739ec848df8cc0d15b9e65c0a2706aa6614c6cd4bd0605575aa62e9e2973b3"
S19C_CEP_PRESENCE_SHA <- "3613f86e60bd264d83b3593a1965a24250b55656ea3a8c1e29f1189e99ffe00e"
S19C_CEP_OUTPUT_MANIFEST_SHA <- "0a61fe9d649a1e892fe37e476e43e9a4973aaa216ae2f82fbc9b49d2057565a3"
# Stage 19 live outputs (read only; record: docs/behavior_output_producer_reruns/stage19_20261012*.csv)
S19C_STAGE19_PHASE_SHA <- "094bce9b29805d14c6fc1fcb3671730ba03f0470b96c56645b609e0d0a3ddf55"
S19C_STAGE19_AUDIT_SHA <- "be29e97bd91cb4d66e572116f23e22cd14ab40c1dc320fb88b6a79c7acb41366"
S19C_STAGE19_RECORDS <- c("docs/behavior_output_producer_reruns/stage19_20261012.csv", "docs/behavior_output_producer_reruns/stage19_20261012b.csv")
# Row counts of the pinned release (animal-recordings, kept after the spatial exclusions, animal-blocks, observed ones,
# Stage 19 reader cells)
S19C_COUNTS <- list(animal_recordings = 444L, kept_animal_recordings = 432L, animal_blocks = 3219L, observed_animal_blocks = 3203L,
                    stage19_cells = 21312L)
S19C_CODE <- c("Testing/audits/build_spatial_occupancy_candidate.R", "Functions/spatial_occupancy_candidate.R",
               "Functions/antenna_spatial_reconstruction.R", "Functions/spatial_exclusion_manifest.R", "Functions/antenna_measurement.R",
               "Functions/antenna_log_helpers.R", "Functions/cage_map_helpers.R", "Functions/project_paths.R",
               "Functions/behavioral_dynamics_helpers.R", "Functions/manuscript_palette.R", "Functions/protected_outputs.R",
               "Functions/qc_run_protection.R", "Functions/frozen_input_guard.R", "Analysis/_pipeline_setup.R")
# The animal-level data products and the per-episode table are written gzip-compressed (fread reads them directly); the
# review tables stay plain CSV.
S19C_GZIP_TABLES <- c("occupancy_animal_phase_column", "occupancy_animal_phase_reader", "occupancy_animal_window_column",
                      "occupancy_animal_window_reader", "unknown_by_last_reader_gap", "organisation_animal_block", "organisation_animal_window",
                      "stage19_comparison_animal_window", "basis_bound_sensitivity_by_episode", "cage_episode_block_column", "cage_episode_window_column",
                      "unknown_cagemate_context")
S19C_LONGEST_RELATIVE <- c(file.path("tables", "basis_bound_sensitivity_by_episode.csv.gz"), file.path("manifest", "protection_snapshot_before.csv"),
                           file.path("figures", "s19c_F2_event_vs_read_by_cage_episode.png"), file.path("manifest", "output_files_sha256.csv"))
# Figure colours: categorical slots of the validated reference palette in fixed order (C1-C4; three series), a neutral
# grey for unknown time, a one-hue ordinal ramp for the gap classes; light surface and recessive ink.
S19C_SURFACE <- "#fcfcfb"
S19C_INK <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781", grid = "#e1e0d9", axis = "#c3c2b7")
S19C_UNIT_COLOURS <- c(C1 = "#2a78d6", C2 = "#eb6834", C3 = "#1baf7a", C4 = "#eda100", unknown = "#b4b2aa")
S19C_SERIES_COLOURS <- c("#2a78d6", "#eb6834", "#1baf7a")
S19C_GAP_COLOURS <- c(`60-300s` = "#86b6ef", `300-1800s` = "#5598e7", `1800-3600s` = "#256abf", `3600-10800s` = "#184f95", `>10800s` = "#0d366b",
                      `open (no later read)` = "#c3c2b7", `before the first read` = "#898781")

s19c_args <- function(args) {
  known <- "^--(run-id|output-root)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  list(run_id = val("run-id"), output_root = val("output-root"))
}

s19c_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

#' Output folders of a run: the base (an isolated root, mmm_isolated_output_root_check()), runs/.tmp_<run_id> and
#' runs/<run_id>. Refuses a bad run id and an existing run or build folder.
s19c_output_dirs <- function(output_root, run_id, project_root) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  base <- mmm_isolated_output_root_check(output_root, project_root)
  out <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out)) stop("Run folder exists; choose a new --run-id: ", out, call. = FALSE)
  if (file.exists(tmp)) stop("A partial run exists (inspect it, then choose a new --run-id): ", tmp, call. = FALSE)
  list(base = base, tmp = tmp, out = out)
}

#' The pinned inputs: role, path, expected sha256 (constant), and the sha256 that a manifest or record of the producer
#' lists for the same file (listed_sha256, listed_in). Reads the srs and cep output manifests, the srs input hashes and the
#' Stage 19 rerun records; those manifests are themselves pinned.
s19c_pins <- function(project_root, repo_root = MMM_REPO_ROOT) {
  ranm <- file.path(dirname(project_root), "RANM")
  srs <- file.path(ranm, "single_read_spatial", "runs", S19C_SRS_RUN_ID)
  cep <- file.path(ranm, "cage_episode_presence", "runs", S19C_CEP_RUN_ID)
  s19 <- file.path(project_root, "analysis_ready", "analyses", "spatial_occupancy")
  p <- data.table::rbindlist(list(
    data.table::data.table(role = "srs output manifest", item = "srs_output_manifest", path = file.path(srs, "manifest", "output_files_sha256.csv"),
                           expected_sha256 = S19C_SRS_OUTPUT_MANIFEST_SHA),
    data.table::data.table(role = "srs run record", item = "srs_run", path = file.path(srs, "manifest", "run.csv"), expected_sha256 = S19C_SRS_RUN_CSV_SHA),
    data.table::data.table(role = "srs input hashes", item = "srs_input_hashes", path = file.path(srs, "manifest", "input_hashes.csv"),
                           expected_sha256 = S19C_SRS_INPUT_HASHES_SHA),
    data.table::data.table(role = "srs table", item = names(S19C_SRS_TABLES), path = file.path(srs, "tables", paste0(names(S19C_SRS_TABLES), ".csv")),
                           expected_sha256 = unname(S19C_SRS_TABLES)),
    data.table::data.table(role = "cep output manifest", item = "cep_output_manifest", path = file.path(cep, "manifest", "output_files_sha256.csv"),
                           expected_sha256 = S19C_CEP_OUTPUT_MANIFEST_SHA),
    data.table::data.table(role = "cage-episode presence", item = "presence", path = file.path(cep, "tables", "cage_episode_presence.csv"),
                           expected_sha256 = S19C_CEP_PRESENCE_SHA),
    data.table::data.table(role = "Stage 19 phase table (read only; keys and seconds)", item = "stage19_phase",
                           path = file.path(s19, "tables", "all_position_occupancy_by_phase.csv"), expected_sha256 = S19C_STAGE19_PHASE_SHA),
    data.table::data.table(role = "Stage 19 raw-antenna audit (read only; raw log names)", item = "stage19_audit",
                           path = file.path(s19, "audit", "raw_antenna_occupancy_audit.csv"), expected_sha256 = S19C_STAGE19_AUDIT_SHA)))
  p[, `:=`(listed_sha256 = NA_character_, listed_in = NA_character_)]
  lst <- function(manifest, rel_of) {
    if (!file.exists(manifest)) return(invisible(NULL))
    m <- data.table::fread(manifest, colClasses = "character")
    for (i in which(!is.na(rel_of))) {
      hit <- m[file == rel_of[i]]
      if (nrow(hit) == 1L) data.table::set(p, i, c("listed_sha256", "listed_in"), list(hit$sha256, basename(dirname(dirname(manifest)))))
    }
  }
  rel_srs <- ifelse(p$role == "srs table", file.path("tables", basename(p$path)), ifelse(p$item == "srs_run", "manifest/run.csv",
                    ifelse(p$item == "srs_input_hashes", "manifest/input_hashes.csv", NA_character_)))
  lst(file.path(srs, "manifest", "output_files_sha256.csv"), rel_srs)
  lst(file.path(cep, "manifest", "output_files_sha256.csv"), ifelse(p$item == "presence", "tables/cage_episode_presence.csv", NA_character_))
  ih <- file.path(srs, "manifest", "input_hashes.csv")
  if (file.exists(ih)) {
    h <- data.table::fread(ih, colClasses = "character")[role == "Stage 19 phase table"]
    if (nrow(h) == 1L) data.table::set(p, which(p$item == "stage19_phase"), c("listed_sha256", "listed_in"), list(h$sha256, paste(S19C_SRS_RUN_ID, "input_hashes.csv")))
  }
  rec <- data.table::rbindlist(lapply(file.path(repo_root, S19C_STAGE19_RECORDS), function(f) if (file.exists(f)) data.table::fread(f, colClasses = "character")))
  if (nrow(rec)) {
    rec <- rec[, .SD[.N], by = target_file]   # the later record overrides the earlier one
    for (k in c("stage19_audit", "stage19_phase")) {
      tf <- basename(p[item == k]$path); r <- rec[target_file == tf]
      if (nrow(r) && is.na(p[item == k]$listed_sha256)) data.table::set(p, which(p$item == k), c("listed_sha256", "listed_in"), list(r$sha256, "Stage 19 rerun record"))
      if (nrow(r) && !is.na(p[item == k]$listed_sha256) && !identical(r$sha256, p[item == k]$listed_sha256))
        data.table::set(p, which(p$item == k), "listed_sha256", "records disagree")
    }
  }
  p[]
}

#' sha256 of every pinned input; stops unless each file exists, matches its constant and matches what its producer lists.
s19c_verify_pins <- function(pins) {
  pins <- data.table::copy(pins)
  pins[, `:=`(sha256 = mmm_file_sha256(path), bytes = file.size(path))]
  pins[, verified := !is.na(sha256) & sha256 == expected_sha256 & (is.na(listed_sha256) | listed_sha256 == expected_sha256)]
  pins[, listed := !is.na(listed_sha256)]
  bad <- pins[verified == FALSE | (listed == FALSE & role != "srs output manifest" & role != "cep output manifest")]
  if (nrow(bad)) stop("Pinned input(s) missing, changed or not listed by their producer:\n  ",
                      paste0(bad$role, ": ", bad$path, " (sha ", substr(bad$sha256, 1, 12), ", pinned ", substr(bad$expected_sha256, 1, 12),
                             ", listed ", substr(bad$listed_sha256, 1, 12), ")", collapse = "\n  "), call. = FALSE)
  pins[]
}

#' The srs tables, the presence counts, the Stage 19 references (keys and seconds only) and the srs run record.
s19c_read_inputs <- function(pins) {
  path <- function(k) pins[item == k]$path
  rd <- function(k) {
    f <- path(k); h <- names(data.table::fread(f, nrows = 0))
    data.table::fread(f, colClasses = if ("AnimalNum" %in% h) c(AnimalNum = "character"))
  }
  tab <- stats::setNames(lapply(c("occupancy", "animal_block", "animals", "blocks", "censored", "read_agreement", "gaps", "colocation"), rd),
                         c("occupancy", "animal_block", "animals", "blocks", "censored", "read_agreement", "gaps", "colocation"))
  ref <- stats::setNames(lapply(c("shares_unknown_column_pooled", "shares_unknown_reader_pooled", "support_by_phase", "parity_stage19"), function(k)
    data.table::fread(path(k), colClasses = if (grepl("^shares_unknown", k)) c(unit = "character"))),
    c("shares_unknown_column_pooled", "shares_unknown_reader_pooled", "support_by_phase", "parity_stage19"))
  # presence: the three counts only (fixed_cage, which encodes the CON cages, is never read)
  ref$presence <- data.table::fread(path("presence"), select = c("CageEpisodeID", "n_animals_present", "n_tags_present", "n_tracked"))
  f19 <- path("stage19_phase")
  s19 <- data.table::fread(f19, select = c("SourceFile", "Phase", "PhaseNumber", "AnimalNum", "PositionID", "PositionSeconds", "observation_seconds"),
                           colClasses = c(AnimalNum = "character"), dec = if (grepl(";", readLines(f19, n = 1))) "," else ".")
  s19[, AnimalNum := canonical_animal_id(AnimalNum)]
  ref$stage19 <- s19[, .(PositionSeconds = sum(PositionSeconds), observation_seconds = max(observation_seconds)),
                     by = .(SourceFile, Phase, PhaseNumber, AnimalNum, PositionID)]
  au <- data.table::fread(path("stage19_audit"), select = c("SourceFile", "raw_log", "raw_logs_ignored", "clock_fold_events_dropped"),
                          colClasses = c(raw_logs_ignored = "character"))
  au <- unique(au)
  au[is.na(raw_logs_ignored), raw_logs_ignored := ""]
  if (anyDuplicated(au$SourceFile)) stop("The Stage 19 audit names more than one raw log for a recording.", call. = FALSE)
  ref$raw_logs <- au
  run <- data.table::fread(path("srs_run"), colClasses = "character")
  list(tab = tab, ref = ref, srs_run = stats::setNames(run$value, run$item))
}

#' Gates on the srs run record: version, clean release code, tau, bounds and the exclusion version and its content hash.
s19c_srs_run_gates <- function(run, manifest_sha) {
  ok <- identical(run[["run_id"]], S19C_SRS_RUN_ID) && identical(run[["version"]], S19C_RECONSTRUCTION) && identical(run[["code_clean"]], "TRUE") &&
    identical(run[["dev_run"]], "FALSE") && identical(as.numeric(run[["tau_s"]]), S19C_TAU_S) && identical(run[["bounds_s"]], "2 10 60 300") &&
    identical(run[["manifest_version"]], S19C_MANIFEST_VERSION) && identical(run[["manifest_sha256"]], manifest_sha)
  soc_gate_row("G02", "the srs release: run id, version, clean code, tau 2 s, bounds, exclusion version and its content hash", ok,
               sprintf("%s, %s, code_clean %s, dev_run %s, tau %s s, bounds %s, %s (%s)", run[["run_id"]], run[["version"]], run[["code_clean"]],
                       run[["dev_run"]], run[["tau_s"]], run[["bounds_s"]], run[["manifest_version"]], substr(run[["manifest_sha256"]], 1, 12)),
               paste(S19C_SRS_RUN_ID, S19C_RECONSTRUCTION, "TRUE FALSE 2 '2 10 60 300'", S19C_MANIFEST_VERSION, substr(manifest_sha, 1, 12)))
}

# ---------------------------------------------------------------- figures (group-blind QC)

s19c_theme <- function(base = 8.5) {
  ggplot2::theme_minimal(base_size = base) + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = S19C_SURFACE, colour = NA), panel.background = ggplot2::element_rect(fill = S19C_SURFACE, colour = NA),
    panel.grid.major = ggplot2::element_line(colour = S19C_INK[["grid"]], linewidth = 0.25), panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = S19C_INK[["axis"]], linewidth = 0.3), axis.ticks = ggplot2::element_blank(),
    text = ggplot2::element_text(colour = S19C_INK[["primary"]]), axis.text = ggplot2::element_text(colour = S19C_INK[["secondary"]]),
    strip.text = ggplot2::element_text(face = "bold", colour = S19C_INK[["primary"]]),
    plot.title = ggplot2::element_text(face = "bold", size = base + 1.5), plot.subtitle = ggplot2::element_text(colour = S19C_INK[["secondary"]], size = base - 0.5),
    plot.caption = ggplot2::element_text(colour = S19C_INK[["secondary"]], size = base - 1.5, hjust = 0), legend.position = "bottom",
    plot.title.position = "plot", plot.caption.position = "plot", panel.spacing = grid::unit(3, "mm"))
}

s19c_bound_label <- function(b) c(LOCF = "LOCF", max300s = "\u2264 300 s", max60s = "\u2264 60 s")[b]
s19c_reader_axis <- function() { L <- soc_reader_layout(); stats::setNames(sub(" ", "\n", L$reader_label), L$reader) }

#' The six QC figures (PNG) from soc_build()$figures into `dir`; returns the file names.
s19c_figures <- function(fig, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  sv <- function(p, n, w, h) { ggplot2::ggsave(file.path(dir, n), p, width = w, height = h, dpi = 200, bg = S19C_SURFACE); n }
  cap1 <- "Group-blind QC of the candidate s19c_v0.1 (srs_v1.1_9b18624); the three spatially excluded cage episodes are left out."
  cap2 <- "Column or reader occupancy is not resource engagement, proximity or a validated location; supported = a read at most d s old."
  cap <- paste(cap1, cap2, sep = "\n")
  files <- character(0)
  ul <- c(C1 = "C1 (R1 food + R5 house)", C2 = "C2 (R2 + R6)", C3 = "C3 (R3 + R7)", C4 = "C4 (R4 + R8 water)", unknown = "unknown")

  # F1: column shares with unknown, share of observed time, read basis, LOCF / 300 s / 60 s, by phase and CC
  d <- data.table::copy(fig$f1)
  d[, CCf := factor(ifelse(CC == "all", "all CC", CC), levels = c("CC1", "CC2", "CC3", "CC4", "all CC"))]
  d[, `:=`(boundf = factor(s19c_bound_label(bound), levels = s19c_bound_label(SOC_BOUNDS)), unitf = factor(unit, levels = rev(names(ul))))]
  lab <- d[unit == "unknown"][, txt := ifelse(share_of_observed_time > 0.0005, sprintf("%.0f%%", 100 * share_of_observed_time), "0%")]
  p <- ggplot2::ggplot(d, ggplot2::aes(boundf, share_of_observed_time, fill = unitf)) +
    ggplot2::geom_col(width = 0.72, colour = S19C_SURFACE, linewidth = 0.35) +
    ggplot2::geom_text(data = lab, ggplot2::aes(boundf, 1.035, label = txt), inherit.aes = FALSE, size = 2.3, colour = S19C_INK[["secondary"]]) +
    ggplot2::scale_fill_manual(values = S19C_UNIT_COLOURS, labels = ul, name = NULL, guide = ggplot2::guide_legend(reverse = TRUE, nrow = 1)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"), breaks = seq(0, 1, 0.25), limits = c(0, 1.07), expand = c(0, 0)) +
    ggplot2::facet_grid(Phase ~ CCf) + s19c_theme() +
    ggplot2::labs(x = "carry-forward: unrestricted (LOCF, historical reference) or bounded by the observation age (sensitivity)", y = "share of observed time",
                  title = "F1  Column occupancy with unknown time: share of ALL observed time, Active and Light phases (single-read basis)",
                  subtitle = "Stage 19 block set (CC4 A1, I2, A2); pooled by seconds over animals; numbers above the bars: unknown share (no read within the bound; not redistributed)",
                  caption = cap)
  files <- c(files, sv(p, "s19c_F1_column_shares_with_unknown.png", 10, 5.6))

  # F2: event vs read per cage episode (LOCF share of observed time; <= 60 s share of supported time)
  d <- data.table::copy(fig$f2)
  meas <- c("LOCF, share of observed time", "\u2264 60 s, share of supported time")
  d[, measure := ifelse(bound == "LOCF", meas[1], meas[2])]
  d[, panel := factor(paste0(Phase, ", ", measure, "\n", ul[unit]),
                      levels = paste0(rep(c("Active", "Light"), each = 8), ", ", rep(rep(meas, each = 4), 2), "\n", rep(ul[1:4], 4)))]
  sym <- d[, .(value_a = value_b, value_b = value_a, panel)]
  p <- ggplot2::ggplot(d, ggplot2::aes(value_b, value_a)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = S19C_INK[["muted"]], linewidth = 0.3) +
    ggplot2::geom_blank(data = sym) +
    ggplot2::geom_point(size = 1.1, alpha = 0.65, colour = S19C_SERIES_COLOURS[1], stroke = 0) +
    ggplot2::facet_wrap(~panel, ncol = 4, scales = "free") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) + ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    s19c_theme(7.5) + ggplot2::theme(strip.text = ggplot2::element_text(size = 6.5)) +
    ggplot2::labs(x = "event-start basis (historical reference)", y = "single-read basis (provisional candidate)",
                  title = "F2  Event-start vs single-read column shares per cage episode: LOCF (share of observed time) and \u2264 60 s (share of supported time)",
                  subtitle = "one point per cage episode and phase class (Stage 19 block set, pooled by seconds); grey line: equal shares; panels have their own scales",
                  caption = cap)
  files <- c(files, sv(p, "s19c_F2_event_vs_read_by_cage_episode.png", 10.5, 10.5))

  # F3: reader cage maps per CC x phase (read basis, <= 60 s, share of supported time) with R1 food, R5 house, R8 water
  d <- data.table::copy(fig$f3)
  d[, `:=`(ReaderLabel = reader_label, share = share_supported, Phase = factor(Phase, levels = c("Active", "Light")))]
  d[, `:=`(label = rfid_format_percent(share), ink = rfid_ink_for_fill(rfid_occupancy_fill(share)))]
  nt <- data.table::copy(fig$f3_unknown)[, note := sprintf("unknown (no read \u2264 60 s): %.0f%% of observed time", 100 * unknown_share_all)]
  nt[, Phase := factor(Phase, levels = c("Active", "Light"))]
  lay <- rfid_cage_map_layout(paste(cap1, cap2, "Cells: share of SUPPORTED time (readers sum to 100% per panel); R5-R8 row on top, R1 bottom left."),
                              n_col = 4, n_row = 2, panel_w = 1.9, extra_w = 1.3, head_h = 0.62)
  p <- rfid_cage_map(as.data.frame(d), "share", "reader", label_col = "label", ink_col = "ink", reader_ids = TRUE, notes = as.data.frame(nt), note_side = "bottom") +
    ggplot2::facet_grid(Phase ~ CC) + rfid_scale_fill_occupancy(name = "share of\nsupported time") +
    ggplot2::labs(title = "F3  Reader cage maps (single-read, \u2264 60 s): share of SUPPORTED time per CC, Active and Light phases",
                  subtitle = "Stage 19 block set; pooled by seconds over animals; below each cage: the unknown share of observed time", caption = lay$caption) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = S19C_SURFACE, colour = NA),
                   legend.title = ggplot2::element_text(size = 6, margin = ggplot2::margin(b = 9)))
  files <- c(files, sv(p, "s19c_F3_reader_cage_maps.png", lay$size[["width"]], lay$size[["height"]]))

  # F4: support <= 60 s per animal-phase across the blocks, by board and CC (whole span)
  d <- data.table::copy(fig$f4)
  d[, blab := paste0(ifelse(Phase == "Active", "A", "I"), PhaseNumber)]
  lv <- unique(d[order(block_no)]$blab)
  d[, `:=`(blab = factor(blab, levels = lv), boardf = factor(paste("board", board)), inwin = ifelse(stage19_window, "Stage 19 block set", "whole span only (CC4 grid days)"))]
  p <- ggplot2::ggplot(d, ggplot2::aes(blab, supported_le60_share_of_observed_time)) +
    ggplot2::geom_jitter(ggplot2::aes(colour = Phase, shape = inwin), width = 0.18, height = 0, size = 0.7, alpha = 0.6, stroke = 0.3) +
    ggplot2::stat_summary(fun = stats::median, geom = "point", shape = 95, size = 4, colour = S19C_INK[["primary"]]) +
    ggplot2::scale_colour_manual(values = c(Active = S19C_SERIES_COLOURS[1], Light = S19C_SERIES_COLOURS[2]), name = NULL,
                                 guide = ggplot2::guide_legend(override.aes = list(size = 2.2, alpha = 1))) +
    ggplot2::scale_shape_manual(values = c(`Stage 19 block set` = 16, `whole span only (CC4 grid days)` = 1), name = NULL,
                                guide = ggplot2::guide_legend(override.aes = list(size = 2.2, alpha = 1))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"), limits = c(0, 1)) +
    ggplot2::facet_grid(CC ~ boardf) + s19c_theme(7.5) +
    ggplot2::labs(x = "phase block (A = active, I = light/inactive; vendor numbering)", y = "share of observed time with a read \u2264 60 s old",
                  title = "F4  Support: share of observed time with a read at most 60 s old, per animal and phase block, by board and cage change",
                  subtitle = "points: animal-blocks (whole span); bars: medians; supported = recently detected, not a validated location", caption = cap)
  files <- c(files, sv(p, "s19c_F4_support_by_board.png", 11, 7.5))

  # F5: original Stage 19 reader profile (= event LOCF) against the candidate, no group panels
  d <- data.table::copy(fig$f5)
  ax <- s19c_reader_axis()
  d[, `:=`(rl = factor(ax[unit], levels = ax), series = factor(series, levels = unique(series)))]
  p <- ggplot2::ggplot(d, ggplot2::aes(rl, share, fill = series)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75, colour = S19C_SURFACE, linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = S19C_SERIES_COLOURS, name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::facet_grid(Phase ~ CC) + s19c_theme(7.5) +
    ggplot2::labs(x = NULL, y = "share of time at the reader (denominator in the legend)",
                  title = "F5  Reader profiles: original Stage 19 (= event-start LOCF) against the candidate, Active and Light phases",
                  subtitle = "Stage 19 block set; the same animals in every series (excluded cage episodes left out of all three); pooled by seconds",
                  caption = cap)
  files <- c(files, sv(p, "s19c_F5_stage19_vs_candidate_readers.png", 11, 6))

  # F6: unknown time by the reader last assigned and the gap length
  d <- data.table::copy(fig$f6)
  ax <- c(s19c_reader_axis(), none = "none")
  d[, `:=`(rl = factor(ax[last_reader], levels = ax), gap = factor(gap_class, levels = rev(names(S19C_GAP_COLOURS))),
           boundf = factor(paste("unknown at", s19c_bound_label(bound)), levels = paste("unknown at", s19c_bound_label(c("max60s", "max300s")))))]
  p <- ggplot2::ggplot(d, ggplot2::aes(rl, share_of_phase_unknown_time, fill = gap)) +
    ggplot2::geom_col(width = 0.72, colour = S19C_SURFACE, linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = S19C_GAP_COLOURS, name = "length of the read gap", drop = TRUE, guide = ggplot2::guide_legend(reverse = TRUE, nrow = 1)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::facet_grid(Phase ~ boundf) + s19c_theme(7.5) +
    ggplot2::labs(x = "reader last assigned (where the animal was last read, not where it was while unread)", y = "share of the phase's unknown time",
                  title = "F6  Unknown time by the reader last assigned and the length of its read gap (single-read basis), Active and Light phases",
                  subtitle = "Stage 19 block set; 1-s grid; denominator: all unknown time of the phase at that bound", caption = cap)
  files <- c(files, sv(p, "s19c_F6_unknown_by_last_reader_gap.png", 10, 6.2))
  files
}

#' File name of an output table (gzip for S19C_GZIP_TABLES).
s19c_table_file <- function(nm) paste0(nm, ifelse(nm %in% S19C_GZIP_TABLES, ".csv.gz", ".csv"))

#' One row per output table: file, rows, grain and contents (tables/table_guide.csv).
s19c_table_guide <- function(tables) {
  g <- data.table::data.table(table = c(
    "occupancy_animal_phase_column", "occupancy_animal_phase_reader", "occupancy_animal_window_column", "occupancy_animal_window_reader",
    "support_animal_phase", "unknown_by_last_reader_gap", "unknown_cagemate_context", "cage_episode_block_column", "cage_episode_window_column",
    "organisation_animal_block", "organisation_animal_window",
    "basis_bound_sensitivity", "basis_bound_sensitivity_by_episode", "basis_comparison_column", "unsupported_time_summary", "column_shares_by_bound",
    "observation_gap_summary", "stage19_comparison_animal_window", "exclusions_censoring", "gates"),
    grain = c(
      "animal-recording x phase block x basis x bound x unit (C1-C4, unknown); zero-completed",
      "animal-recording x phase block x basis x bound x unit (R1-R8, unknown); zero-completed",
      "animal-recording x window (phase_class | cc1_active1) x block_set x Phase x basis x bound x unit (C1-C4, unknown)",
      "animal-recording x window x block_set x Phase x basis x bound x unit (R1-R8, unknown)",
      "animal-recording x phase block",
      "animal-recording x phase block x bound (max60s, max300s) x reader last assigned x gap class",
      "level (pooled | cage episode) x block_set x Phase x bound x reader x state (supported | unknown)",
      "cage episode x phase block x basis x bound x unit (C1-C4, unknown); block_set = the narrowest set holding the block",
      "cage episode x phase class x block_set (the set pooled over) x basis x bound x unit (C1-C4, unknown)",
      "animal-recording x phase block x basis x bound; block_set = the narrowest set holding the block",
      "animal-recording x phase class x block_set (the set pooled over) x basis x bound",
      "block_set x Phase x comparison x resolution x unit x denominator",
      "cage episode x block_set x Phase x comparison x resolution x unit x denominator",
      "level (pooled | cage-episode spread | by board | by CC | cage episode) x block_set x Phase x bound x denominator x unit",
      "level (pooled | by CC | by board | by CC x board) x block_set x Phase x bound",
      "level (pooled | by CC) x block_set x Phase x basis x bound x unit (C1-C4, unknown)",
      "level (total | gap class | last reader | last reader x gap class) x block_set x Phase x bound",
      "animal-recording x window x Phase x resolution x unit (Stage 19 block set; wide over basis and bound)",
      "exclusion record x cage episode", "gate or diagnostic"),
    contents = c(
      "PRIMARY. seconds, share_all (of observed time; units + unknown = 1), share_supported (of supported time; units = 1; NA without support); observed_s, censored_s, supported_s",
      "reader diagnostics with the lab-reported resource (R1 food, R5 house, R8 water); same columns as the column table",
      "shares pooled by seconds over the phases of the window (never means of shares); n_blocks, observed_s, censored_s",
      "as the column window table at reader resolution",
      "observed, supported (<= 60 s, <= 300 s) and unknown time and shares of observed time; reads and reconstruction changes per observed hour (not locomotion); agreement at reads (timestamp consistency, not accuracy); seeds, ties; raw-log QC",
      "unknown seconds on the 1-s grid of the single-read basis by the reader last assigned (where the animal was last read, not where it was) and the read-gap length",
      "cage-mate context of unknown and supported time on the 1-s grid: missingness context, not proximity or affiliation",
      "per phase block: pooled by seconds (seconds_pooled, share_all_pooled, share_supported_pooled) and the equal-weight animal mean beside it; presence counts",
      "per phase class (pooled over the block set's phases): as the block table",
      "S4 per phase block: entropy (bits) and dominant column of the shares of supported time, with the unknown share beside it",
      "S4 per phase class (pooled over the block set's phases): as the block table",
      "read - event and bound - LOCF: pooled values (a, b, diff) and the spread of the per-cage-episode differences",
      "per cage episode (phase class, pooled by seconds): value_a, value_b, diff for every comparison",
      "read minus event at column resolution (primary rows) and for R5 house at reader level (diagnostic rows): event_share, read_share, read_minus_event, episode spread",
      "unknown and supported time as shares of observed time; spread over cage episodes and over animal-recordings; n_animals = distinct animals, n_animal_recordings = animal x CC recordings",
      "LOCF vs <= 300 s vs <= 60 s column shares under both denominators (share_of_observed_time, share_of_supported_time) and their change against LOCF",
      "unknown time (1-s grid) by gap class and reader last assigned; house association (unknown share of the reader's LOCF time); grid against exact unknown time",
      "Stage 19's share (PositionSeconds / observation_seconds) against the candidate's event LOCF (identical), read LOCF and the bounded shares, with differences",
      "per exclusion record: animal-blocks, observed and censored seconds, what the record does in this candidate", "every gate with pass, value and criterion"))
  g[, `:=`(file = s19c_table_file(table), rows = vapply(table, function(t) if (t %in% names(tables)) nrow(tables[[t]]) else NA_integer_, integer(1)))]
  data.table::setcolorder(g, c("file", "table", "rows"))
  g[]
}

s19c_readme <- function(run_id, commit, gates, files) {
  c(paste("STAGE 19 CANDIDATE", SOC_VERSION, "(UNREGISTERED; isolated; group-blind; Stage 19, its outputs and the manuscript are unchanged)"), "",
    paste("run:", run_id, "| commit:", commit, "| reconstruction:", S19C_RECONSTRUCTION, "release", S19C_SRS_RUN_ID, "| exclusions:", S19C_MANIFEST_VERSION,
          "| presence:", S19C_CEP_RUN_ID),
    paste("gates:", sum(gates[type == "gate"]$pass), "of", nrow(gates[type == "gate"]), "passed (tables/gates.csv)"), "",
    "Representation: four columns primary (C1 = R1 food + R5 house, C2 = R2 + R6, C3 = R3 + R7, C4 = R4 + R8 water); eight readers as diagnostics.",
    "Bases: event = event-start (historical reference); read = single-read (provisional candidate). Bounds: LOCF (historical reference), max300s, max60s.",
    "Denominators: share_all = seconds / observed time (units + unknown = 1); share_supported = seconds / supported time (units = 1; NA without support).",
    "Phase = Active | Light (Phase_vendor Active | Inactive). block_set: stage19_window (primary; CC4 A1, I2, A2) | whole_span (secondary).",
    "Per-block rows: block_set names the narrowest set holding the block; aggregated rows: the set they were pooled over.",
    "Excluded cage episodes (spatial_excluded = TRUE) stay in the animal tables and are left out of every summary and figure.",
    "Supported = recently detected (a read <= d s old), not validated. Column or reader occupancy is not resource engagement or proximity.",
    "Estimates of the two bases are sensitivity analyses, not bounds on true physical occupancy.", "",
    "tables/ (grain and contents: tables/table_guide.csv; *.csv.gz are read directly by data.table::fread)", paste0("  ", files),
    "figures/  s19c_F1 ... s19c_F6 (group-blind QC; PNG)", "manifest/ inputs_sha256, code_hashes, run, spatial_exclusions_used, protection check, output hashes", "",
    "No group label, CombZ, outcome, model or test statistic was read or computed.")
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "antenna_log_helpers.R", "antenna_measurement.R", "antenna_spatial_reconstruction.R", "spatial_exclusion_manifest.R",
              "spatial_occupancy_candidate.R", "cage_map_helpers.R", "qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R")) source_mmm_helper(h)
  a <- s19c_args(commandArgs(trailingOnly = TRUE))
  project_root <- mmm_project_root()
  head_commit <- s19c_git(c("rev-parse", "HEAD"))[1]
  run_id <- if (is.na(a$run_id)) paste0(SOC_VERSION, "_", substr(head_commit, 1, 7)) else a$run_id
  dev <- startsWith(run_id, "dev")
  dirty <- s19c_git(c("status", "--porcelain", "--untracked-files=all", "--", S19C_CODE, SXM_DIR))
  if (!dev && length(dirty)) stop("A release run needs committed code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  dirs <- s19c_output_dirs(if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "spatial_occupancy_candidate") else a$output_root,
                           run_id, project_root)
  mmm_assert_output_path_budget(c(file.path(dirs$tmp, S19C_LONGEST_RELATIVE), file.path(dirs$out, S19C_LONGEST_RELATIVE)), source_label = "s19c build")
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pins <- s19c_verify_pins(s19c_pins(project_root))
  MAN <- sxm_read(S19C_MANIFEST_VERSION)
  guard <- mmm_protection_guard_begin(dirs$base, run_id, "s19c", project_root)

  x <- s19c_read_inputs(pins)
  an <- x$tab$animals
  expected <- list(
    animal_flags = an[, .(SourceFile, AnimalNum, spatial_excluded = sxm_spatial_excluded(MAN, Batch, CC, board),
                          movement_excluded = sxm_movement_excluded(MAN, Batch, CC, board), censor_from_s = sxm_censor_from(MAN, Batch, CC, board))],
    records = MAN[, .(record_id, action, CageEpisodeID, reader, resource, from)], counts = S19C_COUNTS)
  meta <- list(run_id = run_id, srs_run_id = S19C_SRS_RUN_ID, reconstruction = S19C_RECONSTRUCTION, tau_s = S19C_TAU_S)
  res <- soc_build(x$tab, x$ref, meta, expected)
  gates <- rbind(soc_gate_row("G01", "every pinned input exists, matches its sha256 constant and the sha256 its producer lists", all(pins$verified),
                              paste(sum(pins$verified), "of", nrow(pins), "inputs verified"), "sha256 identical; listed in the producer's manifest or record"),
                 s19c_srs_run_gates(x$srs_run, attr(MAN, "sha256")),
                 res$gates)
  gates[, `:=`(run_id = run_id, srs_run_id = S19C_SRS_RUN_ID)]
  data.table::setcolorder(gates, c("run_id", "srs_run_id"))

  for (dd in c("tables", "figures", "manifest")) dir.create(file.path(dirs$tmp, dd), recursive = TRUE, showWarnings = FALSE)
  for (nm in names(res$tables)) data.table::fwrite(res$tables[[nm]], file.path(dirs$tmp, "tables", s19c_table_file(nm)))
  data.table::fwrite(gates, file.path(dirs$tmp, "tables", "gates.csv"))
  guide <- s19c_table_guide(c(res$tables, list(gates = gates)))
  data.table::fwrite(guide, file.path(dirs$tmp, "tables", "table_guide.csv"))
  figs <- s19c_figures(res$figures, file.path(dirs$tmp, "figures"))
  code <- data.table::data.table(file = c(S19C_CODE, file.path(SXM_DIR, basename(attr(MAN, "path")))))
  code[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  data.table::fwrite(code, file.path(dirs$tmp, "manifest", "code_hashes.csv"))
  data.table::fwrite(pins[, .(role, item, path, expected_sha256, listed_sha256, listed_in, sha256, bytes, verified)], file.path(dirs$tmp, "manifest", "inputs_sha256.csv"))
  data.table::fwrite(MAN, file.path(dirs$tmp, "manifest", "spatial_exclusions_used.csv"))
  n_gate <- nrow(gates[type == "gate"]); n_pass <- sum(gates[type == "gate"]$pass)
  run <- data.table::data.table(
    item = c("run_id", "version", "git_commit", "code_clean", "dev_run", "srs_run_id", "reconstruction", "tau_s", "bounds", "block_sets", "primary_block_set",
             "manifest_version", "manifest_sha256", "cep_run_id", "gates_passed", "project_root", "output_dir", "started", "finished", "R"),
    value = c(run_id, SOC_VERSION, head_commit, as.character(!length(dirty)), as.character(dev), S19C_SRS_RUN_ID, S19C_RECONSTRUCTION, S19C_TAU_S,
              paste(SOC_BOUNDS, collapse = " "), paste(SOC_BLOCK_SETS, collapse = " "), SOC_BLOCK_SETS[1], S19C_MANIFEST_VERSION, attr(MAN, "sha256"),
              S19C_CEP_RUN_ID, paste(n_pass, "of", n_gate), project_root, dirs$out, started, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string))
  data.table::fwrite(run, file.path(dirs$tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(dirs$tmp, "manifest", "session_info.txt"))
  writeLines(s19c_readme(run_id, head_commit, gates, c(guide$file, "table_guide.csv")), file.path(dirs$tmp, "README.txt"))
  mmm_protection_guard_end(guard, file.path(dirs$tmp, "manifest"))
  if (n_pass < n_gate) {
    stop(n_gate - n_pass, " gate(s) failed; nothing is published. See ", file.path(dirs$tmp, "tables", "gates.csv"), ":\n  ",
         paste(gates[type == "gate" & pass == FALSE, paste(gate_id, check, "|", value)], collapse = "\n  "), call. = FALSE)
  }
  outs <- list.files(dirs$tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(dirs$tmp, outs)), bytes = file.size(file.path(dirs$tmp, outs))),
                     file.path(dirs$tmp, "manifest", "output_files_sha256.csv"))
  if (file.exists(dirs$out)) stop("Run folder appeared during the run: ", dirs$out, call. = FALSE)
  if (!file.rename(dirs$tmp, dirs$out)) stop("Could not rename ", dirs$tmp, call. = FALSE)
  message("s19c build done: ", dirs$out, " (", n_pass, " of ", n_gate, " gates passed)")
}
