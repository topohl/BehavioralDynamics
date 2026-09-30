# ================================================================
# Figure-support bundle: helpers (descriptive source-data export for manuscript Figure 1, option 3)
# MMMSociability -- Functions/figure_support_bundle.R
# ================================================================
# Pure constants and functions for Analysis/16c_figure_support_bundle.R (record: docs/FIGURE_SUPPORT_BUNDLE_v1.md).
# Sourcing this file defines constants and functions only; it reads and writes nothing. It needs the generic hash-gate
# and writer utilities of Functions/stage30_figure_bundle.R (s30fb_*), which the caller sources first.
#
# NO INFERENCE. Nothing here fits, tests, refits, adjusts, smooths or predicts, and no p or q value is produced.
#   * F1 / F1b: the descriptive mean of each CON cage's 4 animals (ebb_v101 B1), and a copy of the ebb_v101 B2 CON mean.
#   * F2 / F2b: the six stored CombZ component z-scores exactly as they enter CombZ (canonical CombZ table), their
#     direction from the frozen CombZ definition, and CombZ recomposed from them by the frozen rule. The frozen
#     definition constants and the composite rule are read from the producer Analysis/build_later_outcome_combz.R by
#     parsing it: only the named constant assignments and the four composite lines are evaluated, never the file.
#     The upstream raw -> z standardisation is NOT re-derived (docs/COMBZ_CANONICAL_DEFINITION.md section 4a):
#     raw_value, reference_mean and reference_sd are therefore NA, and F2b says why.
# Requires: data.table, digest, tibble (the producer's component table is a tibble::tribble).
# ================================================================

FSB_SCHEMA_VERSION <- "1"
FSB_BUNDLE_PREFIX <- "fsb_v1"
FSB_STATUS_REAL <- "FROZEN"
FSB_STATUS_DRY <- "DRY_RUN_NOT_FOR_USE"
FSB_TOL <- 1e-12

FSB_STAGE29 <- list(bundle_id = "ebb_v101_20260929_b2ce507",
                    manifest_sha256 = "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c")
# canonical CombZ tables and label lists: the on-disk SHA-256 must equal these (the first three = Stage 30 input_hashes)
FSB_INPUT_SHA256 <- c(
  combz_table = "1f6a2a69c6b0781b8de8da50ecdadf309b62bbc9c82106c0fa91d18119a7de54",
  sus_list = "dea3804b71b479f84900f946e5494b204ed0e8801498da459a6fc781beb9f391",
  con_list = "eddd2ee9c3a182f98211b4cbba689e2bb5f72985aeb178f72b25b7d25db1b75d",
  combz_component_definition = "af3c273434c7457b7b28ba39bbc231afc5f161435cf04b588c9de820fab6f70f",
  combz_classification_thresholds = "e1536f0c6e45a708770b0b78139232ebf9077eed4b884fd3e03524aad861afc9")
# the frozen CombZ definition code: the producer, pinned by its git blob (git hash-object; line-ending independent)
FSB_COMBZ_CODE <- list(path = "Analysis/build_later_outcome_combz.R", git_blob = "d850859e088de991946f0e9b1f5230b24cccfab7",
                       last_commit = "bac46bf", doc = "docs/COMBZ_CANONICAL_DEFINITION.md")
FSB_COMBZ_CONSTANTS <- c("COMBZ_DEFINITION_ID", "COMBZ_REFERENCE_POP_ID", "COMBZ_CLASSIFICATION_ID", "COMBZ_CORRECTION_ID",
                         "COMBZ_COMPONENTS", "COMBZ_COMPONENT_META", "PARITY_TOL_COMBZ")
# the producer's composite rule, evaluated verbatim (build_later_outcome_combz.R, "recompute the composite")
FSB_COMBZ_RULE <- c("comp_mat <- as.matrix(dat[, COMBZ_COMPONENTS])",
                    "dat$n_components_present <- rowSums(is.finite(comp_mat))",
                    "dat$CombZ <- rowMeans(comp_mat, na.rm = TRUE)",
                    "dat$CombZ[dat$n_components_present == 0L] <- NA_real_")
# call heads a frozen constant's right-hand side may contain (literals, vectors, the tribble); anything else is refused
FSB_CONSTANT_CALLS <- c("c", "::", "tibble", "tribble", "~", "-", "+", "*", "/", "(", "list", "paste", "paste0")

FSB_MEASURES <- c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation")
FSB_SEXES <- c("Female", "Male")
FSB_CCS <- paste0("CC", 1:4)
FSB_SEX_BATCHES <- list(Female = c("B3", "B4", "B6"), Male = c("B1", "B2", "B5"))
FSB_N <- list(con_cages = 3L, con_per_cage = 4L, con_per_sex = 12L, combz_animals = 117L, components = 6L,
              rfid_animals = 111L, con_rfid_animals = 24L)
FSB_TABLES <- c("F1_con_cage_means", "F1b_con_reference_means", "F2_combz_components", "F2b_combz_definition")
FSB_GATE_FILE <- "H3_gate_results.csv"
FSB_GATE_COLS <- c("stage", "gate", "passed", "hard", "detail")
FSB_REGISTRY_COLS <- c("bundle_id", "status", "manifest_sha256", "stage29_bundle_id", "combz_table_sha256", "mmm_git_commit", "created_at")
FSB_PROVENANCE_KEYS <- c("bundle_id", "status", "generated_at", "generator", "mmm_git_commit", "mmm_branch", "stage29_bundle_id",
                         "stage29_bundle_manifest_sha256", "combz_table_sha256", "combz_definition_code", "combz_definition_code_git_blob",
                         "scientific_recomputation", "descriptive_computations", "pre_write_gates_passed", "gate_results")
FSB_SCIENTIFIC_RECOMPUTATION <- paste(
  "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model.",
  "The standardised values are the stored canonical component z-scores exactly as they enter CombZ (the upstream",
  "within-sex CON standardisation is carried through verbatim, not re-derived from raw measures); CombZ is recomposed",
  "from them by the frozen rule of Analysis/build_later_outcome_combz.R and equals the stored CombZ within 1e-12")
FSB_DESCRIPTIVE_COMPUTATIONS <- c(
  "F1 cage_mean: arithmetic mean of the measure over the CON animals of one cage epoch (Batch|System|CC) in ebb_v101 B1 (values as stored); n_animals = count of those animals",
  "F1b: n and mean copied from ebb_v101 B2_descriptive_summaries (Group CON); nothing computed",
  "gate only (not exported): mean of the 3 cage means per measure x Sex x CC = the F1b / B2 CON mean within 1e-12 (equal cage sizes)",
  "F2 z = direction x signed_z (sign flip, direction -1 where the frozen definition inverts the sign after standardisation); signed_z = the stored component",
  "F2 heatmap_order_within_sex: rank of CombZ within Sex (ascending; ties by AnimalNum), a layout key; below_threshold = CombZ < the stored within-sex susceptibility_threshold",
  "gate only (not exported): CombZ recomposed from signed_z by the frozen producer rule (mean of the present components) = the stored CombZ within 1e-12")
# words never allowed in display text (DESIGN section 0; OPTION3 section 0); identifiers such as crossing_rate are not labels
FSB_FORBIDDEN_DISPLAY <- paste0("antenna|crossing|sleep|confirmatory|preregistered|approach|investigation|consumption|habituation|time near|",
                                "female-specific|sex-specific|acute|sociability|social behaviou?r|social preference|huddling")

# ---------------------------------------------------------------- small utilities
fsb_parse_mode <- function(args) s30fb_parse_mode(args)
fsb_bundle_id <- function(date, commit) {
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("Not a full git commit hash: ", commit, call. = FALSE)
  sprintf("%s_%s_%s", FSB_BUNDLE_PREFIX, format(as.Date(date), "%Y%m%d"), substr(commit, 1, 7))
}
fsb_gate_count <- function(gates) sprintf("%d of %d", sum(gates$passed), nrow(gates))
fsb_check_display <- function(x, cols, what) {
  for (k in cols) { v <- as.character(x[[k]]); bad <- !is.na(v) & grepl(FSB_FORBIDDEN_DISPLAY, v, ignore.case = TRUE)
    if (any(bad)) stop(what, ": banned display wording in ", k, ": ", paste(unique(v[bad]), collapse = " | "), call. = FALSE) }
  invisible(TRUE)
}
#' Canonical ids from a one-id-per-line list (the producer's read_ids: trim, drop blanks and a header line).
fsb_read_id_list <- function(lines) {
  x <- trimws(lines); x <- x[nzchar(x)]; x <- x[!grepl("^(id|animal|animalnum|animal_id)$", tolower(x))]
  ids <- canonical_animal_id(x)
  if (!length(ids) || anyNA(ids)) stop("Identifier list is empty or has an unreadable id.", call. = FALSE)
  unique(ids)
}
fsb_group_from_lists <- function(animal, sus, con) s30fb_group_from_lists(animal, sus, con)

# ---------------------------------------------------------------- frozen CombZ definition (parsed, never executed)
fsb_call_heads <- function(e) {
  if (!is.call(e)) return(character())
  h <- e[[1]]; head <- if (is.name(h)) as.character(h) else if (is.call(h)) as.character(h[[1]]) else ""
  c(head, if (is.call(h)) unlist(lapply(as.list(h)[-1], fsb_call_heads)), unlist(lapply(as.list(e)[-1], fsb_call_heads)))
}
fsb_expr_text <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")

#' Read the frozen CombZ definition from the producer source WITHOUT running it: every top-level assignment to one of
#' FSB_COMBZ_CONSTANTS (exactly one each; right-hand side limited to FSB_CONSTANT_CALLS) is evaluated in an empty
#' environment, and the four composite-rule expressions (FSB_COMBZ_RULE, exactly once each, in order) are returned
#' unevaluated. Returns list(constants = <list>, rule = <expressions>, n_expressions).
fsb_read_combz_definition <- function(path) {
  ex <- parse(file = path, keep.source = FALSE)
  is_assign <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]), TRUE)
  lhs <- rep(NA_character_, length(ex)); lhs[is_assign] <- vapply(ex[is_assign], function(e) as.character(e[[2]]), "")
  env <- new.env(parent = baseenv())
  for (k in FSB_COMBZ_CONSTANTS) {
    i <- which(lhs == k)
    if (length(i) != 1L) stop("Frozen CombZ definition: expected exactly one top-level assignment to ", k, " (found ", length(i), ").", call. = FALSE)
    bad <- setdiff(fsb_call_heads(ex[[i]][[3]]), FSB_CONSTANT_CALLS)
    if (length(bad)) stop("Frozen CombZ definition: ", k, " calls ", paste(bad, collapse = ","), "; refusing to evaluate it.", call. = FALSE)
    assign(k, eval(ex[[i]][[3]], envir = env), envir = env)
  }
  txt <- vapply(ex, fsb_expr_text, "")
  want <- vapply(FSB_COMBZ_RULE, function(s) fsb_expr_text(parse(text = s, keep.source = FALSE)[[1]]), "", USE.NAMES = FALSE)
  pos <- vapply(want, function(w) { i <- which(txt == w); if (length(i) != 1L) NA_integer_ else i }, 1L)
  if (anyNA(pos)) stop("Frozen CombZ definition: composite rule line(s) not found exactly once: ", paste(FSB_COMBZ_RULE[is.na(pos)], collapse = " | "), call. = FALSE)
  if (is.unsorted(pos, strictly = TRUE) || any(diff(pos) != 1L)) stop("Frozen CombZ definition: the composite rule lines are not consecutive and in order.", call. = FALSE)
  cst <- mget(FSB_COMBZ_CONSTANTS, envir = env)
  meta <- as.data.frame(cst$COMBZ_COMPONENT_META, stringsAsFactors = FALSE)
  if (!identical(as.character(meta$component), as.character(cst$COMBZ_COMPONENTS)) || !is.logical(meta$sign_inverted) || anyNA(meta$sign_inverted))
    stop("Frozen CombZ definition: COMBZ_COMPONENT_META does not list COMBZ_COMPONENTS in order with a logical sign_inverted.", call. = FALSE)
  cst$COMBZ_COMPONENT_META <- meta
  list(constants = cst, rule = ex[pos], n_expressions = length(ex))
}

#' CombZ and n_components_present from a signed-z matrix by the frozen producer rule (its own expressions, evaluated in an
#' empty environment holding only `dat` and COMBZ_COMPONENTS).
fsb_combz_recompose <- function(defn, signed) {
  comps <- defn$constants$COMBZ_COMPONENTS
  env <- new.env(parent = baseenv())
  env$dat <- as.data.frame(signed[, comps, drop = FALSE]); env$COMBZ_COMPONENTS <- comps
  for (e in defn$rule) eval(e, envir = env)
  data.frame(CombZ = env$dat$CombZ, n_components_present = as.integer(env$dat$n_components_present))
}

#' Gate rows: the canonical combz_component_definition.csv agrees with the constants parsed from the producer.
fsb_component_definition_gates <- function(defn, compdef) {
  cd <- data.table::as.data.table(compdef); meta <- defn$constants$COMBZ_COMPONENT_META; k <- length(defn$constants$COMBZ_COMPONENTS)
  g <- function(name, ok, detail = "") s30fb_gate_row("3-definition", name, ok, detail)
  rbind(
    g("combz_component_definition.csv components = producer COMBZ_COMPONENTS (same order)", identical(as.character(cd$component), as.character(meta$component)),
      paste(cd$component, collapse = ",")),
    g("combz_component_definition.csv sign_inverted = producer COMBZ_COMPONENT_META", identical(as.logical(cd$sign_inverted), meta$sign_inverted),
      paste(cd$sign_inverted, collapse = ",")),
    g("combz_component_definition.csv xlsx_column / raw_measure / higher_means = producer COMBZ_COMPONENT_META",
      identical(as.character(cd$xlsx_column), as.character(meta$xlsx_column)) && identical(as.character(cd$raw_measure), as.character(meta$raw_measure)) &&
        identical(as.character(cd$higher_means), as.character(meta$higher_means))),
    g("combz_component_definition.csv weight_in_composite = 1 / number of components", isTRUE(all(abs(as.numeric(cd$weight_in_composite) - 1 / k) <= FSB_TOL))),
    g("combz_component_definition.csv combz_definition_id / reference_population_id = producer constants",
      all(cd$combz_definition_id == defn$constants$COMBZ_DEFINITION_ID) && all(cd$reference_population_id == defn$constants$COMBZ_REFERENCE_POP_ID)),
    g("combz_component_definition.csv: component standardisation not reproducible in the repository (recorded, carried verbatim)",
      all(!as.logical(cd$reproducible_in_repository))))
}

# ---------------------------------------------------------------- F1 / F1b: CON cage means and the CON reference mean
#' Metric display label / unit / tier copied from the ebb_v101 configuration `metrics`.
fsb_metric_labels <- function(cfg_metrics, measures = FSB_MEASURES) {
  out <- data.table::rbindlist(lapply(measures, function(m) { x <- cfg_metrics[[m]]
    if (is.null(x$label) || is.null(x$unit) || is.null(x$tier)) stop("ebb_v101 config has no label / unit / tier for ", m, call. = FALSE)
    data.table::data.table(measure = m, measure_label = x$label, unit = x$unit, tier = x$tier) }))
  fsb_check_display(out, c("measure_label", "unit"), "metric labels")
  out[]
}

#' F1: one row per measure x Sex x CC x CON cage epoch; cage_mean = mean over the cage's CON animals (B1 values as stored).
#' Stops on an NA value, a missing measure column or a CON animal without a cage epoch.
fsb_con_cage_means <- function(b1, labels, measures = FSB_MEASURES) {
  b1 <- data.table::as.data.table(b1); miss <- setdiff(c("AnimalNum", "Sex", "Batch", "Group", "CC", "CageEpisodeID", "n_in_cage", measures), names(b1))
  if (length(miss)) stop("B1 lacks column(s): ", paste(miss, collapse = ","), call. = FALSE)
  con <- b1[Group == "CON"]
  if (!nrow(con) || anyNA(con$CageEpisodeID) || any(!nzchar(con$CageEpisodeID))) stop("B1: CON rows missing or without a cage epoch.", call. = FALSE)
  out <- data.table::rbindlist(lapply(measures, function(m) {
    v <- con[[m]]; if (anyNA(v) || !all(is.finite(v))) stop("B1: non-finite CON value in ", m, call. = FALSE)
    con[, .(Group = "CON", n_animals = .N, n_in_cage = paste(sort(unique(n_in_cage)), collapse = ";"), animals = paste(sort(AnimalNum), collapse = ";"),
            cage_mean = mean(get(m))), by = .(Sex, CC, Batch, CageEpisodeID)][, measure := m]
  }))
  out <- merge(out, labels, by = "measure", all.x = TRUE, sort = FALSE)
  if (anyNA(out$measure_label)) stop("F1: no display label for a measure.", call. = FALSE)
  out[, measure := factor(measure, levels = measures)]
  data.table::setorderv(out, c("measure", "Sex", "CC", "Batch")); out[, measure := as.character(measure)]
  out[, .(measure, measure_label, unit, tier, Sex, CC, Batch, CageEpisodeID, Group, n_animals, n_in_cage, animals, cage_mean)]
}

#' F1b: the CON descriptive mean (n, mean) per measure x Sex x CC, copied from ebb_v101 B2.
fsb_con_reference_means <- function(b2, labels, measures = FSB_MEASURES) {
  b2 <- data.table::as.data.table(b2)
  x <- b2[Group == "CON" & construct %in% measures, .(measure = construct, Sex, CC, Group, n = as.integer(n), mean = as.numeric(mean))]
  if (anyDuplicated(x[, .(measure, Sex, CC)])) stop("B2: duplicated CON row.", call. = FALSE)
  x <- merge(x, labels, by = "measure", all.x = TRUE, sort = FALSE)
  x[, measure := factor(measure, levels = measures)]; data.table::setorderv(x, c("measure", "Sex", "CC")); x[, measure := as.character(measure)]
  x[, source := "copied: ebb_v101 B2_descriptive_summaries.csv (Group CON; construct, Sex, CC, n, mean)"]
  x[, .(measure, measure_label, unit, tier, Sex, CC, Group, n, mean, source)]
}

#' One check row per measure x Sex x CC: cage count, cage sizes, batches, and mean of cage means vs the B2 CON mean.
fsb_con_cage_checks <- function(f1, f1b, sex_batches = FSB_SEX_BATCHES, cages_expected = FSB_N$con_cages, per_cage_expected = FSB_N$con_per_cage) {
  f1 <- data.table::as.data.table(f1); f1b <- data.table::as.data.table(f1b)
  ck <- f1[, .(n_cages = .N, n_cage_epochs = data.table::uniqueN(CageEpisodeID), cage_sizes = paste(n_animals, collapse = ";"),
               n_in_cage = paste(unique(n_in_cage), collapse = ";"), batches = paste(sort(Batch), collapse = ";"),
               n_total = sum(n_animals), mean_of_cage_means = mean(cage_mean)), by = .(measure, Sex, CC)]
  ck <- merge(ck, f1b[, .(measure, Sex, CC, b2_n = n, b2_mean = mean)], by = c("measure", "Sex", "CC"), all = TRUE)
  ck[, abs_diff := abs(mean_of_cage_means - b2_mean)]
  ck[, expected_batches := vapply(Sex, function(s) paste(sort(sex_batches[[s]]), collapse = ";"), "")]
  ncx <- as.integer(cages_expected); npx <- as.integer(per_cage_expected)
  ck[, ok_cages := !is.na(n_cages) & n_cages == ncx & n_cage_epochs == ncx]
  ck[, ok_sizes := !is.na(cage_sizes) & vapply(strsplit(ifelse(is.na(cage_sizes), "", cage_sizes), ";"), function(z) length(z) > 0L && all(as.integer(z) == npx), TRUE) &
                   !is.na(n_in_cage) & n_in_cage == as.character(npx)]
  ck[, ok_batches := !is.na(batches) & batches == expected_batches]
  ck[, ok_n := !is.na(b2_n) & n_total == b2_n]
  ck[, ok_mean := !is.na(abs_diff) & abs_diff <= FSB_TOL]
  ck[]
}

#' Composition checks on B1: every CON cage epoch holds only CON animals; each CON group keeps the same animals at every CC.
fsb_con_composition <- function(b1) {
  b1 <- data.table::as.data.table(b1); cages <- unique(b1[Group == "CON", CageEpisodeID])
  mixed <- b1[CageEpisodeID %in% cages & Group != "CON", unique(CageEpisodeID)]
  sets <- b1[Group == "CON", .(animals = paste(sort(AnimalNum), collapse = ";")), by = .(Sex, Batch, CC)]
  stable <- sets[, .(n_sets = data.table::uniqueN(animals), n_cc = .N), by = .(Sex, Batch)]
  list(mixed_cages = mixed, stable = stable, ok_pure = !length(mixed), ok_stable = all(stable$n_sets == 1L))
}

# ---------------------------------------------------------------- F2 / F2b: CombZ components as they enter CombZ
#' F2: 117 animals x 6 components from the canonical CombZ table. signed_z = the stored component (as it enters CombZ);
#' direction = -1 where the frozen definition inverts the sign after standardisation; z = direction x signed_z;
#' raw_value / reference_mean / reference_sd NA (the upstream standardisation is not re-derivable in the repository).
#' Stops unless CombZ recomposed from signed_z by the frozen rule equals the stored CombZ within `tol` for every animal
#' and n_components_present matches. `thresholds` = canonical combz_classification_thresholds (Sex, susceptibility_threshold).
fsb_combz_components <- function(cz, defn, thresholds, n_expected = FSB_N$combz_animals, tol = FSB_TOL) {
  cz <- data.table::as.data.table(cz); cst <- defn$constants; comps <- cst$COMBZ_COMPONENTS; meta <- cst$COMBZ_COMPONENT_META
  need <- c("AnimalNum", "Sex", "Batch", "outcome_group", comps, "n_components_present", "CombZ", "combz_definition_id")
  if (length(setdiff(need, names(cz)))) stop("CombZ table lacks column(s): ", paste(setdiff(need, names(cz)), collapse = ","), call. = FALSE)
  if (nrow(cz) != n_expected || anyDuplicated(cz$AnimalNum)) stop("CombZ table: expected ", n_expected, " unique animals (found ", nrow(cz), ").", call. = FALSE)
  if (!all(cz$combz_definition_id == cst$COMBZ_DEFINITION_ID)) stop("CombZ table combz_definition_id differs from the frozen definition.", call. = FALSE)
  S <- as.data.frame(lapply(cz[, comps, with = FALSE], as.numeric)); names(S) <- comps
  re <- fsb_combz_recompose(defn, S)
  stored <- as.numeric(cz$CombZ)
  if (!identical(is.na(re$CombZ), is.na(stored))) stop("Recomposed CombZ NA pattern differs from the stored CombZ.", call. = FALSE)
  d <- abs(re$CombZ - stored); dmax <- if (any(!is.na(d))) max(d, na.rm = TRUE) else 0
  if (dmax > tol) stop(sprintf("CombZ recomposed from signed_z by the frozen rule differs from the stored CombZ (max |diff| %.3g > %g): %s",
                               dmax, tol, paste(cz$AnimalNum[!is.na(d) & d > tol], collapse = ",")), call. = FALSE)
  np_mis <- sum(re$n_components_present != as.integer(cz$n_components_present))
  if (np_mis) stop("n_components_present differs from the frozen rule for ", np_mis, " animal(s).", call. = FALSE)
  thr <- data.table::as.data.table(thresholds)
  if (!setequal(thr$Sex, unique(cz$Sex)) || anyDuplicated(thr$Sex)) stop("Thresholds: need exactly one row per Sex.", call. = FALSE)
  th <- as.numeric(thr$susceptibility_threshold[match(cz$Sex, thr$Sex)])
  base <- data.table::data.table(AnimalNum = cz$AnimalNum, Sex = cz$Sex, Batch = cz$Batch, Group = cz$outcome_group,
                                 n_components_present = as.integer(cz$n_components_present), CombZ = stored, susceptibility_threshold = th,
                                 below_threshold = stored < th)
  base[, heatmap_order_within_sex := data.table::frank(data.table::data.table(CombZ, AnimalNum), ties.method = "first"), by = Sex]
  dirs <- ifelse(meta$sign_inverted, -1L, 1L)
  out <- data.table::rbindlist(lapply(seq_along(comps), function(j) {
    sz <- S[[comps[j]]]
    cbind(base[, .(AnimalNum, Sex, Batch, Group)],
          data.table::data.table(component = comps[j], component_order = j, raw_value = NA_real_, reference_mean = NA_real_, reference_sd = NA_real_,
                                 z = dirs[j] * sz, direction = dirs[j], signed_z = sz, present = is.finite(sz)),
          base[, .(n_components_present, CombZ, susceptibility_threshold, below_threshold, heatmap_order_within_sex)])
  }))
  data.table::setorderv(out, c("Sex", "heatmap_order_within_sex", "component_order"))
  data.table::setattr(out, "recompose_max_abs_diff", dmax)
  data.table::setattr(out, "n_present_mismatch", np_mis)
  out[]
}

#' F2b: one row per component: sources, standardisation as implemented, direction and why, relationship to CombZ.
fsb_combz_definition_table <- function(defn, compdef, f2, combz_table_path, combz_table_sha256) {
  cst <- defn$constants; meta <- cst$COMBZ_COMPONENT_META; cd <- data.table::as.data.table(compdef); f2 <- data.table::as.data.table(f2)
  k <- length(cst$COMBZ_COMPONENTS); n_all <- data.table::uniqueN(f2$AnimalNum)
  np <- f2[, .(n_present = sum(present), n_missing = sum(!present), missing_animals = paste(sort(AnimalNum[!present]), collapse = ";")), by = component]
  np <- np[match(meta$component, component)]
  n5 <- f2[, .(n = n_components_present[1]), by = AnimalNum][, table(n)]
  corr <- c(sucrose_pref = paste("rebuilt by the producer from the sucrosePreference sheet keyed by animal (not by row position), negative bottle",
                                 "readings counted as zero; standardised with the reference rows the workbook's own formulas name (upstream correction",
                                 paste0(cst$COMBZ_CORRECTION_ID, "; 18 animals, ", FSB_COMBZ_CODE$doc, " section 4b)")),
            delta_cort = paste("OR620's value replaced by the exact linear map every other batch-5 male follows (upstream correction",
                               paste0(cst$COMBZ_CORRECTION_ID, "; 1 animal, ", FSB_COMBZ_CODE$doc, " section 4b)")))
  out <- data.table::data.table(
    component = meta$component, component_order = seq_len(k), display_label = meta$raw_measure, domain = meta$domain,
    source_table = combz_table_path, source_table_sha256 = combz_table_sha256, source_column = meta$component,
    upstream_source = paste0("E9_Behavior_Data_before_restructure.xlsx (pinned) :: sheet zScore column ", meta$xlsx_column),
    raw_measure = meta$raw_measure, raw_derivation = meta$raw_derivation,
    standardisation = "z = (x - mean(x | reference)) / populationSD(x | reference), computed upstream in the endpoint workbook",
    reference_population = paste0("as implemented upstream: the within-sex CON animals (12 per sex), encoded as per-sex positional formula blocks whose ",
                                  "row ranges only approximately coincide with sex (", FSB_COMBZ_CODE$doc, " section 4a); reference_population_id ",
                                  cst$COMBZ_REFERENCE_POP_ID),
    sd_convention = as.character(cd$sd_convention[match(meta$component, cd$component)]),
    standardisation_reproducible_in_repository = as.logical(cd$reproducible_in_repository[match(meta$component, cd$component)]),
    raw_value_reference_columns = "NA in F2: the producer carries the stored z-scores verbatim; re-deriving raw -> z would change the endpoint",
    reproducibility_note = as.character(cd$reproducibility_note[match(meta$component, cd$component)]),
    sign_inverted = meta$sign_inverted, direction = ifelse(meta$sign_inverted, -1L, 1L),
    direction_reason = ifelse(meta$sign_inverted,
                              paste0("multiplied by -1 after standardisation in the frozen definition, so that a higher signed_z means ", meta$higher_means,
                                     " (every component points the same way: higher = more resilient-like)"),
                              paste0("not inverted in the frozen definition: a higher ", meta$raw_measure, " already means ", meta$higher_means,
                                     " (higher = more resilient-like)")),
    higher_signed_z_means = meta$higher_means,
    weight_in_composite = as.numeric(cd$weight_in_composite[match(meta$component, cd$component)]),
    relationship_to_combz = paste0("signed_z (= direction x z) enters CombZ unchanged: CombZ = unweighted mean of the present components' signed_z ",
                                   "(Excel AVERAGE semantics: a missing component is ignored, not zero); ",
                                   paste(sprintf("%d animals with %s of %d present", as.integer(n5), names(n5), k), collapse = ", "),
                                   "; higher CombZ = more resilient-like = lower later stress burden"),
    upstream_correction = ifelse(meta$component %in% names(corr), unname(corr[meta$component]), "none"),
    n_animals = n_all, n_present = np$n_present, n_missing = np$n_missing, missing_animals = np$missing_animals,
    combz_definition_id = cst$COMBZ_DEFINITION_ID, reference_population_id = cst$COMBZ_REFERENCE_POP_ID,
    definition_code = paste0(FSB_COMBZ_CODE$path, " (git blob ", FSB_COMBZ_CODE$git_blob, "): COMBZ_COMPONENT_META and the composite rule"))
  fsb_check_display(out, c("display_label", "domain", "raw_measure", "raw_derivation", "direction_reason", "higher_signed_z_means", "relationship_to_combz",
                           "upstream_correction", "reference_population"), "F2b_combz_definition")
  out[]
}

# ---------------------------------------------------------------- provenance and writer
fsb_provenance <- function(fields) {
  v <- vapply(fields, function(z) paste(as.character(z), collapse = " || "), "")
  h <- data.table::data.table(field = names(fields), value = unname(v)); data.table::setnames(h, "field", "key")
  miss <- setdiff(FSB_PROVENANCE_KEYS, h$key[!is.na(h$value) & nzchar(h$value)])
  if (length(miss)) stop("H_provenance lacks key(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyDuplicated(h$key)) stop("H_provenance has duplicated keys.", call. = FALSE)
  h
}
fsb_bundle_files <- function() c(paste0(FSB_TABLES, ".csv"), "H_provenance.csv", "H2_inputs.csv", FSB_GATE_FILE)

#' Post-write gates on the renamed folder, run BEFORE the registry row is appended. Never stops.
fsb_post_write_gates <- function(bd, manifest, expected_files = fsb_bundle_files()) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("7-write", name, passed, detail)
  m <- data.table::as.data.table(manifest); mf <- file.path(bd, "00_manifest.csv")
  back <- if (file.exists(mf)) data.table::fread(mf, colClasses = "character") else data.table::data.table()
  add("00_manifest.csv read back = the manifest written (file, bytes, sha256, schema_version)",
      nrow(back) == nrow(m) && identical(names(back), names(m)) && identical(back$file, m$file) && identical(back$sha256, m$sha256) &&
        identical(as.numeric(back$bytes), as.numeric(m$bytes)) && identical(back$schema_version, as.character(m$schema_version)), mf)
  chk <- s30fb_manifest_check(bd, m); on_disk <- s30fb_files_on_disk(bd); extra <- setdiff(on_disk, c(m$file, "00_manifest.csv"))
  add("written bundle: every file = 00_manifest (bytes and SHA-256); nothing else in the folder",
      nrow(chk) > 0L && all(chk$ok) && !length(extra) && "00_manifest.csv" %in% on_disk,
      paste(c(bd, if (!all(chk$ok)) paste("mismatch:", paste(chk$file[!chk$ok], collapse = ",")), if (length(extra)) paste("extra:", paste(extra, collapse = ","))), collapse = "; "))
  add(sprintf("00_manifest lists exactly the declared files (%d tables, H_provenance, H2_inputs, H3_gate_results)", length(FSB_TABLES)),
      setequal(m$file, expected_files) && !anyDuplicated(m$file), paste(c(setdiff(expected_files, m$file), setdiff(m$file, expected_files)), collapse = ","))
  fl <- list.files(bd, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  add("written files are read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), bd)
  data.table::rbindlist(g)
}

#' Write the bundle once (the Stage 30b write protocol). Refuses if the version folder, its staging folder, its registry
#' row or its gate log exists, if a hard pre-write gate failed, or if H_provenance pre_write_gates_passed does not count
#' the gate table. Tables, H_provenance, H2_inputs, H3_gate_results and 00_manifest go to a staging folder, which is
#' renamed and made read-only; the post-write gates run before the BUNDLE_REGISTRY.csv row is appended; the complete gate
#' table goes to logs/<bundle_id>_gate_results.csv beside the registry (0444). `.before_verify` is a test hook only.
fsb_write_bundle <- function(tables, out_root, bundle_id, provenance, inputs, gates, status, stage29_bundle_id, combz_table_sha256,
                             mmm_git_commit, created_at, .before_verify = NULL) {
  bd <- file.path(out_root, bundle_id); stg <- file.path(out_root, paste0(".tmp_", bundle_id)); reg <- file.path(out_root, "BUNDLE_REGISTRY.csv")
  log <- s30fb_log_path(out_root, bundle_id)
  registered <- function() file.exists(reg) && bundle_id %in% data.table::fread(reg, colClasses = "character")$bundle_id
  if (dir.exists(bd)) stop("Bundle version exists (immutable): ", bd, call. = FALSE)
  if (dir.exists(stg)) stop("A staging folder for this bundle exists (earlier aborted write): ", stg, call. = FALSE)
  if (registered()) stop("Bundle id already registered: ", bundle_id, call. = FALSE)
  if (file.exists(log)) stop("A gate log for this bundle exists (immutable): ", log, call. = FALSE)
  bad <- setdiff(names(tables), FSB_TABLES); miss <- setdiff(FSB_TABLES, names(tables))
  if (length(bad) || length(miss)) stop("Table set differs from the declared set: extra ", paste(bad, collapse = ","), "; missing ", paste(miss, collapse = ","), call. = FALSE)
  gates <- data.table::as.data.table(gates)
  if (!identical(names(gates), FSB_GATE_COLS) || !is.logical(gates$passed) || !is.logical(gates$hard))
    stop("The gate table must have columns ", paste(FSB_GATE_COLS, collapse = ", "), " (passed / hard logical).", call. = FALSE)
  if (!nrow(gates) || anyNA(gates$passed) || any(gates$hard & !gates$passed)) stop("Refusing to write: a hard pre-write gate did not pass.", call. = FALSE)
  pv <- provenance$value[provenance$key == "pre_write_gates_passed"]
  if (!identical(pv, fsb_gate_count(gates)))
    stop("H_provenance pre_write_gates_passed (", paste(pv, collapse = ","), ") does not count the gate table (", fsb_gate_count(gates), ").", call. = FALSE)
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  dir.create(stg)
  for (n in FSB_TABLES) s30fb_write_csv(tables[[n]], file.path(stg, paste0(n, ".csv")))
  s30fb_write_csv(provenance, file.path(stg, "H_provenance.csv"))
  s30fb_write_csv(inputs, file.path(stg, "H2_inputs.csv"))
  s30fb_write_csv(gates, file.path(stg, FSB_GATE_FILE))
  fl <- sort(list.files(stg, full.names = TRUE))
  man <- data.table::data.table(file = basename(fl), bytes = as.numeric(file.size(fl)), sha256 = vapply(fl, s30fb_sha, "", USE.NAMES = FALSE), schema_version = FSB_SCHEMA_VERSION)
  s30fb_write_csv(man, file.path(stg, "00_manifest.csv"))
  if (dir.exists(bd)) stop("Bundle version appeared during the write (immutable): ", bd, call. = FALSE)
  if (!file.rename(stg, bd)) stop("Could not rename ", stg, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, full.names = TRUE), mode = "0444")
  if (is.function(.before_verify)) .before_verify(bd)
  post <- fsb_post_write_gates(bd, man)
  if (!all(post$passed)) {
    s30fb_write_gate_log(rbind(gates, post), log)
    stop("Post-write gate failed; the bundle is NOT registered (folder ", bd, " left for inspection; gate log ", log, "): ",
         paste(post$gate[!post$passed], collapse = "; "), call. = FALSE)
  }
  msha <- s30fb_sha(file.path(bd, "00_manifest.csv"))
  if (registered()) stop("A registry row for this bundle appeared during the write: ", bundle_id, call. = FALSE)
  if (file.exists(log)) stop("A gate log for this bundle appeared during the write: ", log, call. = FALSE)
  row <- data.table::data.table(bundle_id = bundle_id, status = status, manifest_sha256 = msha, stage29_bundle_id = stage29_bundle_id,
                                combz_table_sha256 = combz_table_sha256, mmm_git_commit = mmm_git_commit, created_at = created_at)
  data.table::fwrite(row, reg, append = file.exists(reg))
  rg <- s30fb_registration_gate(reg, bundle_id, status, msha)
  all_gates <- rbind(gates, post, rg)
  s30fb_write_gate_log(all_gates, log)
  if (!isTRUE(rg$passed)) stop("Registration gate failed: ", rg$detail, call. = FALSE)
  list(dir = bd, manifest_sha256 = msha, manifest = man, registry = reg, log = log, log_sha256 = s30fb_sha(log),
       post_gates = rbind(post, rg), gates = all_gates)
}
