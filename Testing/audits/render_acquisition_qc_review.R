# New, isolated, unregistered review bundle: publication-style QC figures + eight-reader overlap.
# Saved inputs only; no raw reconstruction, phenotype labels, model refit or import.
# Rscript Testing/audits/render_acquisition_qc_review.R --output-root=<absolute> --run-id=<new dev id>
# The guard brackets all output writes. Existing run IDs are refused.

AQR_DYAD_SHA256 <- "6ffae52b9232d5d8d682892dc7e35ed9069fde6c411ebab8abce5a27506a189d"

aqr_read_manifested <- function(run, rel, records) {
  path <- file.path(run, rel)
  mf <- file.path(run, "manifest/output_files_sha256.csv")
  if (!file.exists(path) || !file.exists(mf)) stop("Missing saved input or producer manifest: ", path, call. = FALSE)
  m <- data.table::fread(mf, colClasses = "character")
  names(m) <- tolower(names(m))
  want <- m$sha256[m$file == rel]
  got <- digest::digest(file = path, algo = "sha256")
  if (length(want) != 1 || !identical(got, want)) stop("Input differs from its producer manifest: ", path, call. = FALSE)
  records[[length(records) + 1L]] <- data.table::data.table(input = path, sha256 = got, producer_manifest = mf,
    producer_manifest_sha256 = digest::digest(file = mf, algo = "sha256"), verification = "sha256 matches producer manifest")
  has_animal <- "AnimalNum" %in% names(data.table::fread(path, nrows = 0))
  list(data = data.table::fread(path, colClasses = if (has_animal) c(AnimalNum = "character") else NULL), records = records)
}

aqr_legends <- function() c(
  "# RFID QC review bundle aqr_v0.1", "",
  "UNREGISTERED REVIEW OUTPUT. No registered result, live Stage 19 output or manuscript input is replaced. No calibration is scheduled or required.", "",
  "## QC01 | Acquisition quality and detection support", "",
  "a, Relative reader co-detection: observed / expected co-reads (log2), the expectation coming from the same reader pair on the other boards of the recording; 24 recordings on five boards. It depends on behaviour and board, not only hardware, so low values are diagnostic, not failures. Grey with x: no read in the session (the three corroborated dropouts). The log2 colour scale is capped at -6/+6. Reader order follows R5-R8 (upper row), then R1-R4 (lower row). b, PASS/WARN/FAIL cage-episode counts. FAIL is confined to the three known episodes; W14/W15 review ranks do not set status. c, Mean within-event read gaps per board-recording; points are recordings, diamonds and bars the median and interquartile range (IQR), not confidence intervals. Cadence does not identify hardware causation. d, Median and IQR across healthy board-recordings of time assigned to a reader with a detection <=60 s old. This is recent detection, not validated location. QC01 covers the original acquisition-QC spans and is not restricted to the first 12 h.", "",
  "## QC02 | Reconstruction and observation uncertainty", "",
  "The inactive s19c_v0.2 candidate's primary stage19_window only; the CC4 grid period is excluded. Failed-reader episodes are omitted by released manifest v1.1. a, Single-read column shares of all observed time under unrestricted LOCF and 300/60-s support conventions. Unknown is explicit and never redistributed. b, Single-read minus event-start column shares, unrestricted LOCF; points and bars are the median and IQR across cage episodes, not inferential intervals. c, Pooled unknown share by board at 60 s, with observed-time weighting. d, Share of unknown time in read-gap duration categories, on the saved 1-s grid. Gap context is approximate, unlike the exact interval-based total unknown shares. House/food/water positions locate resources; assignment does not establish engagement. Neither basis, the 2-s flip filter nor either age cap has validated localization accuracy.", "",
  "## QC03 | Eight-reader spatial overlap, without column pooling", "",
  "New descriptive downstream aggregation of the saved measurement-validation dyads. Spatial overlap means two tracked cage-mates assigned the same one of R1-R8, not physical proximity, contact or affiliation. It is not the fraction sharing a column. First active 12 h and the following light 12 h of each cage change (Stage-29 windows), not QC02's wider Stage-19 block set. All three exclude_spatial episodes in manifest v1.1 are removed; no roster animal is globally removed. The released SX04 censor falls outside the first windows, and the builder refuses any retained window overlapping a censor because the saved dyads cannot be clipped.", "",
  "a, Cage-episode observed same-reader fraction against the saved short-shift reference, by phase and support convention, under event-start and single-read reconstruction (both tau 2 s). A cage fraction is sum same-reader dyad-seconds / sum jointly assigned dyad-seconds; at 60 s both animals must have a read <=60 s old. Unassigned/unsupported time is excluded from that conditional denominator, not called non-overlap. b, Observed minus short-shift reference under the read/60-s definition, by cage change. Faint points are cage episodes; summaries are median/IQR, not confidence intervals. Repeated episodes from the same batch/cage are not independent replicates and no p-values or group effects are computed. c, Sensitivity to marginal-occupancy, short-shift and long-shift reference construction under read/60-s. d, An explicitly illustrative four-animal CC1 episode: lexicographically first retained episode with all six dyads, selected without outcomes. Matrix cells show observed minus short-shift reference (percentage points). Blank diagonal is undefined, not zero.", "",
  "The inherited references preserve each tag's trajectory/support while shifting one member: short shifts +/-5 to 60 min in 5-min steps; long shifts 30 min to 11.5 h in 30-min steps. Circular wrap-around and changing jointly supported coverage limit the reference. They are timing-disrupted descriptive references, not a physical chance model or a calibrated interaction baseline. Saved mean reference fractions are aggregated using observed joint seconds, following the original audit; they are not a new permutation distribution and furnish no new p-value. The independence reference is the product of reader occupancy marginals within jointly supported cells. No null quantile is repurposed as an uncertainty interval.", "",
  "Outputs include dyad, partner-weighted animal, cage-episode and descriptive summary tables, paired basis differences, support-coverage and exclusion audits. Only tracked cage-mates contribute. Tag read competition can itself induce apparent deficits in same-reader overlap, so negative excess does not establish avoidance; positive excess does not establish affiliation. No CON/RES/SUS labels or CombZ values are read.", "",
  "## Provenance and scope", "",
  "Existing manifested inputs are re-hashed against producer manifests. The legacy measurement-validation dyad table has no self-manifest: its bytes are explicitly pinned in this renderer and its saved provenance file is included. This verifies file identity, not a fresh raw-data replication. All inputs are re-hashed before publication. The protection guard checks existing protected locations; outputs are created only under this review run. The code is uncommitted and snapshotted in source/; dev identifiers make that status explicit. No historical document is updated by this run.", "",
  "Figure specifications: 183 mm width; 160/125/155 mm height for QC01/QC02/QC03; Arial 5.5-7 pt for chart text (example-matrix numbers approximately 5.1 pt), 8 pt panel letters; editable vector PDF/SVG and 300-dpi PNG previews. (a common journal figure specification). Design compliance does not imply acceptance by a journal."
)

aqr_main <- function(args) {
  if (any(!grepl("^--(output-root|run-id)=", args))) stop("Unknown argument.", call. = FALSE)
  val <- function(k) { z <- sub(paste0("^--", k, "="), "", grep(paste0("^--", k, "="), args, value = TRUE));
    if (length(z) != 1 || !nzchar(z)) stop("Supply exactly one --", k, call. = FALSE); z }
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R", "spatial_exclusion_manifest.R", "manuscript_palette.R", "acquisition_qc_review.R")) source_mmm_helper(h)
  for (pkg in c("data.table", "digest", "ggplot2", "patchwork", "svglite"))
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing existing dependency: ", pkg, call. = FALSE)
  data.table::setDTthreads(1L)
  root <- mmm_project_root(); base <- mmm_isolated_output_root_check(val("output-root"), root); run_id <- val("run-id")
  if (!grepl("^dev[A-Za-z0-9_-]{1,30}$", run_id)) stop("This uncommitted review needs a short dev run id.", call. = FALSE)
  own <- file.path(base, "runs", c(paste0(".tmp_", run_id), run_id))
  if (any(file.exists(own))) stop("Run id already exists; choose a new id.", call. = FALSE)
  guard <- mmm_protection_guard_begin(base, run_id, "aqr", root, MMM_REPO_ROOT)
  tmp <- own[1]; dest <- own[2]
  for (dd in c("figures", "tables", "source_data", "manifest", "source")) dir.create(file.path(tmp, dd), recursive = TRUE, showWarnings = FALSE)
  r <- file.path(root, "analysis_ready/quality_control/raw_acquisition/runs/raqc_20261014_25804cb")
  ranm <- file.path(dirname(root), "RANM")
  c <- file.path(ranm, "spatial_occupancy_candidate/runs/s19c_v0.2_defd263")
  s <- file.path(ranm, "single_read_spatial/runs/srs_v1.1_9b18624")
  records <- list(); x <- list()
  specs <- list(reader = c(r, "tables/reader_qc.csv"), status = c(r, "manifest/qc_status_by_unit.csv"), cadence = c(r, "tables/board_history.csv"),
    reader_spread = c(r, "tables/reader_spread.csv"), shares = c(c, "tables/column_shares_by_bound.csv"), basis = c(c, "tables/basis_comparison_column.csv"),
    unknown = c(c, "tables/unsupported_time_summary.csv"), gaps = c(c, "tables/observation_gap_summary.csv"), gates = c(c, "tables/gates.csv"),
    animals = c(s, "tables/animals.csv"), blocks = c(s, "tables/blocks.csv"))
  for (nm in names(specs)) {
    z <- aqr_read_manifested(specs[[nm]][1], specs[[nm]][2], records); records <- z$records; x[[nm]] <- z$data
  }
  if (any(x$gates[type == "gate"]$pass != TRUE) || nrow(x$gates[type == "gate"]) != 25L) stop("Candidate gates are not 25/25.", call. = FALSE)
  f <- file.path(ranm, "measurement_validation/tables/window_dyads.csv")
  got <- digest::digest(file = f, algo = "sha256")
  if (!identical(got, AQR_DYAD_SHA256)) stop("Saved dyads differ from the review pin.", call. = FALSE)
  dyads <- data.table::fread(f, colClasses = c(A = "character", B = "character"))
  records[[length(records) + 1L]] <- data.table::data.table(input = f, sha256 = got, producer_manifest = "legacy audit: no self-manifest", verification = "explicit byte pin")
  prov <- file.path(ranm, "measurement_validation/tables/provenance.csv")
  records[[length(records) + 1L]] <- data.table::data.table(input = prov, sha256 = digest::digest(file = prov, algo = "sha256"), verification = "legacy provenance recorded")
  manifest <- sxm_read("v1.1")
  records[[length(records) + 1L]] <- data.table::data.table(input = attr(manifest, "path"), sha256 = digest::digest(file = attr(manifest, "path"), algo = "sha256"), verification = "released content pin via sxm_read")
  overlap <- aqr_overlap(dyads, x$animals, x$blocks, manifest)
  for (nm in names(overlap)) data.table::fwrite(overlap[[nm]], file.path(tmp, "tables", paste0("reader_overlap_", nm, ".csv")))
  plots <- aqr_figures(x, overlap)
  for (nm in names(plots$plot_data)) data.table::fwrite(plots$plot_data[[nm]], file.path(tmp, "source_data", paste0(nm, ".csv")))
  height <- c(160, 125, 155)
  for (i in seq_along(plots$figures)) {
    nm <- names(plots$figures)[i]; p <- plots$figures[[i]]
    ggplot2::ggsave(file.path(tmp, "figures", paste0(nm, ".pdf")), p, width = 183, height = height[i], units = "mm", device = grDevices::cairo_pdf, bg = "white")
    ggplot2::ggsave(file.path(tmp, "figures", paste0(nm, ".svg")), p, width = 183, height = height[i], units = "mm", device = svglite::svglite, bg = "white")
    ggplot2::ggsave(file.path(tmp, "figures", paste0(nm, ".png")), p, width = 183, height = height[i], units = "mm", dpi = 300, bg = "white")
  }
  # Inventory exact originals, not all old runs of each tool.
  locations <- list(acquisition = file.path(r, "figures/qc"), measurement = file.path(ranm, "measurement_validation/figures"),
    reconstruction = file.path(s, "figures"), spatial_candidate = file.path(c, "figures"),
    board_adjustment = file.path(ranm, "board_adjustment/runs/badj_v1_9b18624/figures"))
  inventory <- data.table::rbindlist(lapply(names(locations), function(k) data.table::data.table(family = k,
    path = list.files(locations[[k]], pattern = "[.]png$", full.names = TRUE, recursive = TRUE))), fill = TRUE)
  data.table::fwrite(inventory, file.path(tmp, "tables/existing_figure_inventory.csv"))
  writeLines(aqr_legends(), file.path(tmp, "LEGENDS_AND_METHODS.md"), useBytes = TRUE)
  src <- c("Functions/acquisition_qc_review.R", "Testing/audits/render_acquisition_qc_review.R", "Functions/manuscript_palette.R",
           "Functions/spatial_exclusion_manifest.R", "Functions/antenna_measurement.R")
  for (p in src) file.copy(file.path(MMM_REPO_ROOT, p), file.path(tmp, "source", basename(p)))
  inputs <- data.table::rbindlist(records, fill = TRUE)
  now <- vapply(inputs$input, function(p) digest::digest(file = p, algo = "sha256"), "")
  if (!all(now == inputs$sha256)) stop("A source changed during the render.", call. = FALSE)
  data.table::fwrite(inputs, file.path(tmp, "manifest/input_hashes.csv"))
  data.table::fwrite(data.table::data.table(file = src, sha256 = vapply(file.path(MMM_REPO_ROOT, src), function(p) digest::digest(file = p, algo = "sha256"), "")), file.path(tmp, "manifest/code_hashes.csv"))
  head <- system2("git", c("-C", shQuote(MMM_REPO_ROOT), "rev-parse", "HEAD"), stdout = TRUE)[1]
  data.table::fwrite(data.table::data.table(item = c("run_id", "version", "git_head", "code_status", "analysis_status", "dyad_rows_retained", "cage_episodes_retained", "new_group_models", "output"),
    value = c(run_id, AQR_VERSION, head, "uncommitted review code; snapshots included", "UNREGISTERED descriptive eight-reader spatial overlap", nrow(overlap$dyads),
              data.table::uniqueN(overlap$episodes$CageEpisodeID), 0, dest)), file.path(tmp, "manifest/run.csv"))
  capture.output(sessionInfo(), file = file.path(tmp, "manifest/session_info.txt"))
  mmm_protection_guard_end(guard, file.path(tmp, "manifest"))
  outs <- list.files(tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, bytes = file.size(file.path(tmp, outs)),
    sha256 = vapply(file.path(tmp, outs), function(p) digest::digest(file = p, algo = "sha256"), "")), file.path(tmp, "manifest/output_files_sha256.csv"))
  if (!file.rename(tmp, dest)) stop("Could not publish isolated review run.", call. = FALSE)
  print(overlap$summary)
  message("Review bundle: ", dest)
  invisible(dest)
}

if (sys.nframe() == 0L) aqr_main(commandArgs(trailingOnly = TRUE))
