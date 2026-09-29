# ================================================================
# Analytic-identity and data-version gates for a documentation revision of the frozen behaviour config
# MMMSociability -- Functions/behavior_config_identity.R
# ================================================================
# Pure functions (they read files only where a path is passed in; they never write).
#
# 1. mmm_cfg_analytic_identity(parent, new): is `new` (e.g. config v1.0.1) analytically identical to the
#    frozen `parent` (v1.0.0)? Both are compared as parsed canonical JSON trees, leaf by leaf:
#      * every parent leaf must still exist (the one exception is an empty list that becomes populated,
#        e.g. meta$change_log);
#      * no numeric, logical or null leaf may change, except meta$frozen_before_res_sus_outcome_models;
#      * a string leaf may change only if its path is whitelisted:
#          META  - meta$config_version and meta$frozen_on;
#          FREE  - pure display text (label, unit, display_name, interpretation, caveat, figure units);
#          TOKEN - spec-bearing documentation text (event-stream rules, definitions, estimator descriptions):
#                  the new text must contain every protected token of the parent text (every token with a
#                  digit, an upper-case letter, an underscore, a quote or '::'), i.e. no number, identifier,
#                  argument value or code fragment may be dropped or replaced; prose may be reworded and
#                  annotations appended;
#      * a new leaf may be added only under data_versions, meta$(change_log|analytic_parent|release|
#        errata_applied), metrics$<k>$(display_name|first_use_definition|legacy_identifier_note) or
#        population$expected_counts_note;
#      * semantic checks: the pinned parent version and sha, the change_log entry with the change_rule label,
#        frozen_before_res_sus_outcome_models = FALSE, a complete data_versions binding, and a parent data
#        version whose counts equal the parent's frozen expected_counts.
#    Anything else (formulas, expected ranks, contrasts, families, estimators, thresholds, constants, windows,
#    populations, RNG, figure data sources, arrays of terms) must be byte-identical.
#    Limit (by construction): a whitelisted text leaf can still gain misleading prose; every accepted change
#    is therefore listed in the freeze record (analytic_identity.csv) for review, and the analytic identity of
#    the code path is proven separately by the byte-identical control run.
# 2. mmm_cfg_check_parent(cfg, cfg_root): locate the frozen parent under cfg_root, verify its hashes, run 1.
# 3. mmm_dv_resolve / mmm_dv_verify / mmm_dv_verify_run_inputs: the data-version binding gate
#    (cfg$data_versions): manifest hash, exact file set, per-file SHA-256, supporting files, and the same
#    check on a Stage 29 run's audit/run_inputs.csv.
# ================================================================

# ---------------------------------------------------------------- 1. analytic identity

MMM_CFG_V101_RULES <- list(
  meta_change = c("^/meta/(config_version|frozen_on|frozen_before_res_sus_outcome_models)$"),
  free = c("^/metrics/[^/]+/(label|display_name|unit|interpretation|caveat|first_use_definition|legacy_identifier_note)$",
           "^/figure1/units/(crossing_rate|shared_zone_use)$"),
  token = c("^/event_stream/(source|seed|carry_forward|event|validation/pre_implementation)$",
            "^/metrics/[^/]+/(definition|standardizer_definition)$",
            "^/metrics/descriptive/(events_per_bout|bout_decomposition)$",
            "^/windows/light_phase/role$",
            "^/models/TR_POOLED/random_slope/evidence$",
            "^/fitting/(failure_rule|optimizer_check)$",
            "^/diagnostics/zone_variance/trigger$",
            "^/sensitivities/SHARED_ZONE/(D1/longitudinal|D2/model_tr|complete_case/definition)$",
            "^/figure1/(panels/[cde]|panel_sources/e/x|extended_data/\\[2\\])$",
            "^/population/rfid_cohort$"),
  added = c("^/data_versions(/|$)",
            "^/meta/(change_log|analytic_parent|release|errata_applied)(/|$)",
            "^/metrics/[^/]+/(display_name|first_use_definition|legacy_identifier_note)$",
            "^/population/expected_counts_note$"),
  populated_empty = c("^/meta/change_log$"),
  logical_change = c("^/meta/frozen_before_res_sus_outcome_models$"))

#' Parsed canonical JSON tree of a config list (the representation that is hashed and frozen).
mmm_cfg_tree <- function(cfg) jsonlite::fromJSON(mmm_behavior_config_json(cfg), simplifyVector = FALSE)

#' Flatten a parsed JSON tree to leaves: a list of list(path, type, value). Array elements get [i] paths.
mmm_cfg_leaves <- function(x, path = "") {
  if (is.list(x)) {
    if (!length(x)) return(list(list(path = path, type = "empty", value = "<empty>")))
    nm <- names(x); if (is.null(nm)) nm <- rep("", length(x))
    nm[!nzchar(nm)] <- paste0("[", which(!nzchar(nm)), "]")
    return(do.call(c, unname(Map(mmm_cfg_leaves, x, paste0(path, "/", nm)))))
  }
  if (is.null(x)) return(list(list(path = path, type = "null", value = NULL)))
  type <- if (is.character(x)) "string" else if (is.logical(x)) "logical" else if (is.numeric(x)) "number" else class(x)[1]
  list(list(path = path, type = type, value = x))
}

#' Protected tokens of a documentation string: whitespace/punctuation-delimited words that carry a digit,
#' an upper-case letter, an underscore, a quote or '::' (numbers, identifiers, argument values, code).
mmm_cfg_tokens <- function(s) {
  w <- unlist(strsplit(s, "[[:space:],;()\\[\\]{}\"]+", perl = TRUE))
  w <- sub("[.:!?]+$", "", w)
  w <- w[nzchar(w)]
  unique(w[grepl("[0-9A-Z_']|::", w)])
}

.mmm_cfg_match <- function(p, patterns) any(vapply(patterns, grepl, TRUE, x = p, perl = TRUE))
.mmm_cfg_fmt <- function(v) if (is.null(v)) "<null>" else paste(format(v, digits = 17, scientific = FALSE, trim = TRUE), collapse = " ")

#' Compare a frozen parent tree with a revised tree. Returns list(ok, differences, checks, disallowed, ...).
#' parent, new: parsed JSON trees (mmm_cfg_tree / jsonlite::fromJSON(simplifyVector = FALSE)).
#' parent_sha256: SHA-256 of the parent's canonical JSON text (checked against new$meta$analytic_parent).
mmm_cfg_analytic_identity <- function(parent, new, parent_sha256 = NA_character_, rules = MMM_CFG_V101_RULES) {
  la <- mmm_cfg_leaves(parent); lb <- mmm_cfg_leaves(new)
  pa <- vapply(la, `[[`, "", "path"); pb <- vapply(lb, `[[`, "", "path")
  if (anyDuplicated(pa) || anyDuplicated(pb)) stop("Duplicated leaf paths in a configuration tree.", call. = FALSE)
  names(la) <- pa; names(lb) <- pb
  rows <- list()
  add_row <- function(path, status, klass, allowed, reason, old, new)
    rows[[length(rows) + 1]] <<- data.frame(path = path, status = status, class = klass, allowed = allowed, reason = reason,
                                            old = old, new = new, stringsAsFactors = FALSE)
  for (p in union(pa, pb)) {
    a <- la[[p]]; b <- lb[[p]]
    if (!is.null(a) && !is.null(b) && identical(a$type, b$type) && identical(a$value, b$value)) next
    if (is.null(b)) {   # removed
      ok <- a$type == "empty" && .mmm_cfg_match(p, rules$populated_empty) && any(startsWith(pb, paste0(p, "/")))
      add_row(p, "removed", if (ok) "populated_empty" else "removed", ok,
              if (ok) "empty list populated" else "a leaf of the frozen parent was removed", .mmm_cfg_fmt(a$value), NA_character_)
      next
    }
    if (is.null(a)) {   # added
      ok <- .mmm_cfg_match(p, rules$added)
      add_row(p, "added", "added", ok, if (ok) "whitelisted new documentation/data-version leaf" else "new leaf outside the whitelist",
              NA_character_, .mmm_cfg_fmt(b$value))
      next
    }
    if (!identical(a$type, b$type)) { add_row(p, "changed", "type", FALSE, paste("type changed:", a$type, "->", b$type),
                                              .mmm_cfg_fmt(a$value), .mmm_cfg_fmt(b$value)); next }
    if (a$type != "string") {
      ok <- a$type == "logical" && .mmm_cfg_match(p, rules$logical_change)
      add_row(p, "changed", if (ok) "meta" else a$type, ok, if (ok) "whitelisted meta flag" else paste("a", a$type, "leaf changed"),
              .mmm_cfg_fmt(a$value), .mmm_cfg_fmt(b$value)); next
    }
    if (.mmm_cfg_match(p, rules$meta_change)) { add_row(p, "changed", "meta", TRUE, "whitelisted meta field", a$value, b$value); next }
    if (.mmm_cfg_match(p, rules$free)) { add_row(p, "changed", "free_text", TRUE, "display text", a$value, b$value); next }
    if (.mmm_cfg_match(p, rules$token)) {
      lost <- setdiff(mmm_cfg_tokens(a$value), mmm_cfg_tokens(b$value))
      add_row(p, "changed", "token_text", !length(lost),
              if (length(lost)) paste("protected tokens dropped:", paste(lost, collapse = " ")) else "documentation text; every protected token kept",
              a$value, b$value); next
    }
    add_row(p, "changed", "string", FALSE, "string leaf outside the documentation whitelist", a$value, b$value)
  }
  d <- if (length(rows)) do.call(rbind, rows) else
    data.frame(path = character(), status = character(), class = character(), allowed = logical(), reason = character(),
               old = character(), new = character(), stringsAsFactors = FALSE)

  # semantic checks
  ck <- list()
  chk <- function(id, ok, detail) ck[[length(ck) + 1]] <<- data.frame(check = id, ok = isTRUE(ok), detail = detail, stringsAsFactors = FALSE)
  ap <- new$meta$analytic_parent
  chk("parent_version_pinned", identical(ap$config_version, parent$meta$config_version),
      paste("meta$analytic_parent$config_version =", ap$config_version %||% "<missing>", "; parent =", parent$meta$config_version))
  chk("parent_sha_pinned", !is.na(parent_sha256) && identical(ap$config_sha256, parent_sha256),
      paste("pinned", ap$config_sha256 %||% "<missing>", "; parent JSON", parent_sha256))
  chk("new_version_differs", !identical(new$meta$config_version, parent$meta$config_version), paste("new", new$meta$config_version))
  lab <- parent$meta$change_rule$required_label
  cl <- Filter(function(e) identical(e$version, new$meta$config_version), new$meta$change_log %||% list())
  chk("change_log_entry", length(cl) == 1 && identical(cl[[1]]$label, lab),
      paste0("one change_log entry for ", new$meta$config_version, " labelled '", lab, "'"))
  chk("not_claimed_pre_outcome", identical(new$meta$frozen_before_res_sus_outcome_models, FALSE),
      "a revision frozen after outcome inspection must set frozen_before_res_sus_outcome_models = FALSE")
  dvs <- new$data_versions
  ids <- setdiff(names(dvs %||% list()), c("release", "rule"))
  need <- c("tag", "preprocessed_dir", "manifest", "manifest_sha256", "manifest_sha_column", "n_files", "expected_counts",
            "runtime_gates", "complete_case_missing", "d2")
  complete <- length(ids) > 0 && all(vapply(ids, function(i) all(need %in% names(dvs[[i]])) &&
    grepl("^[0-9a-f]{64}$", dvs[[i]]$manifest_sha256 %||% ""), TRUE))
  tags <- vapply(ids, function(i) dvs[[i]]$tag %||% NA_character_, "")
  chk("data_versions_complete", complete && !anyDuplicated(tags) && !anyNA(tags) && isTRUE(dvs$release %in% ids),
      paste("versions:", paste(ids, collapse = ", "), "; release:", dvs$release %||% "<missing>"))
  pdv <- ap$data_version %||% NA_character_
  same_counts <- isTRUE(pdv %in% ids) && identical(dvs[[pdv]]$expected_counts, parent$population$expected_counts)
  chk("parent_data_version_counts", same_counts,
      paste("data_versions$", pdv, "$expected_counts must equal the parent's population$expected_counts", sep = ""))
  cc_txt <- parent$sensitivities$SHARED_ZONE$complete_case$definition %||% ""
  cc_ids <- regmatches(cc_txt, gregexpr("OQ[0-9]+|OR[0-9]+", cc_txt))[[1]]
  chk("parent_data_version_complete_case", isTRUE(pdv %in% ids) && setequal(unlist(dvs[[pdv]]$complete_case_missing), cc_ids),
      paste("parent complete-case set:", paste(cc_ids, collapse = ", ")))
  struct_ok <- all(vapply(ids, function(i) identical(names(unlist(dvs[[i]]$expected_counts)), names(unlist(parent$population$expected_counts))), TRUE))
  chk("data_version_count_structure", struct_ok, "every data version declares the same count structure as the parent")
  ck <- do.call(rbind, ck)

  list(ok = all(d$allowed) && all(ck$ok), n_leaves_parent = length(la), n_leaves_new = length(lb),
       differences = d, disallowed = d[!d$allowed, , drop = FALSE], checks = ck)
}

#' SHA-256 of a frozen config JSON file as recorded in config_sha256.txt (the canonical text: LF line ends,
#' no trailing newline; the freeze script's writeLines() writes CRLF on Windows).
mmm_cfg_json_file_sha256 <- function(json_file) {
  txt <- rawToChar(readBin(json_file, "raw", file.size(json_file)))
  txt <- gsub("\r\n", "\n", sub("\r?\n$", "", txt), fixed = TRUE)
  digest::digest(txt, algo = "sha256", serialize = FALSE)
}

#' Locate the frozen analytic parent under cfg_root (analysis_ready/canonical/behavior_config), verify its hashes
#' (config_sha256.txt, the JSON text and cfg$meta$analytic_parent$config_sha256) and run the identity gate.
#' Returns the identity result plus the parent fields; stops unless the gate passes when stop_on_fail = TRUE.
mmm_cfg_check_parent <- function(cfg, cfg_root, stop_on_fail = TRUE) {
  ap <- cfg$meta$analytic_parent
  if (is.null(ap)) stop("Config v", cfg$meta$config_version, " declares no meta$analytic_parent.", call. = FALSE)
  pdir <- file.path(cfg_root, paste0("v", ap$config_version))
  pjson <- file.path(pdir, "behavior_analysis_config.json"); psha_file <- file.path(pdir, "config_sha256.txt")
  if (!file.exists(pjson) || !file.exists(psha_file)) stop("Frozen analytic parent not found: ", pdir, call. = FALSE)
  recorded <- readLines(psha_file, warn = FALSE)[1]; actual <- mmm_cfg_json_file_sha256(pjson)
  if (!identical(recorded, actual) || !identical(actual, ap$config_sha256))
    stop("Frozen analytic parent hash mismatch: file ", actual, ", config_sha256.txt ", recorded, ", pinned ", ap$config_sha256, call. = FALSE)
  res <- mmm_cfg_analytic_identity(jsonlite::fromJSON(pjson, simplifyVector = FALSE), mmm_cfg_tree(cfg), parent_sha256 = actual)
  res$parent_dir <- pdir; res$parent_version <- ap$config_version; res$parent_sha256 <- actual
  if (stop_on_fail && !isTRUE(res$ok)) {
    bad <- c(if (nrow(res$disallowed)) paste(res$disallowed$path, res$disallowed$reason, sep = ": "),
             if (any(!res$checks$ok)) paste(res$checks$check[!res$checks$ok], res$checks$detail[!res$checks$ok], sep = ": "))
    stop("Analytic-identity gate failed against frozen v", ap$config_version, ":\n", paste(bad, collapse = "\n"), call. = FALSE)
  }
  res
}

# ---------------------------------------------------------------- 2. data-version binding

MMM_DV_PRE_PATTERN <- "_CC[0-9]_AnimalPos_preprocessed[.]csv$"
.mmm_dv_norm <- function(p) tolower(normalizePath(p, winslash = "/", mustWork = FALSE))

#' Resolve a declared data version (default: cfg$data_versions$release) against the project root.
mmm_dv_resolve <- function(cfg, root, data_version = cfg$data_versions$release) {
  dvs <- cfg$data_versions
  if (is.null(dvs)) stop("Config v", cfg$meta$config_version, " has no data_versions binding.", call. = FALSE)
  ids <- setdiff(names(dvs), c("release", "rule"))
  if (length(data_version) != 1 || !data_version %in% ids)
    stop("Unknown data version '", data_version, "'; declared: ", paste(ids, collapse = ", "), call. = FALSE)
  s <- dvs[[data_version]]
  list(id = data_version, tag = s$tag, is_release = identical(data_version, dvs$release),
       dir = file.path(root, s$preprocessed_dir), manifest = file.path(root, s$manifest), manifest_sha256 = s$manifest_sha256,
       sha_column = s$manifest_sha_column, n_files = s$n_files,
       supporting = lapply(s$supporting_files, function(x) list(file = file.path(root, x$file), sha256 = x$sha256)),
       spec = s)
}

#' Verify a resolved data version on disk before any row is read. Stops on the first failure; returns the
#' verified inventory (data.frame input, bytes, sha256, role) for audit/run_inputs.csv.
mmm_dv_verify <- function(dv, pattern = MMM_DV_PRE_PATTERN) {
  sha <- function(f) digest::digest(file = f, algo = "sha256")
  if (!file.exists(dv$manifest)) stop("Data-version manifest missing: ", dv$manifest, call. = FALSE)
  if (!identical(sha(dv$manifest), dv$manifest_sha256))
    stop("Data-version manifest hash mismatch (", dv$id, "): ", sha(dv$manifest), " != ", dv$manifest_sha256, call. = FALSE)
  man <- utils::read.csv(dv$manifest, stringsAsFactors = FALSE, colClasses = "character")
  if (!all(c("file", dv$sha_column) %in% names(man))) stop("Manifest lacks file/", dv$sha_column, " columns.", call. = FALSE)
  if (nrow(man) != dv$n_files || anyDuplicated(man$file)) stop("Manifest must list ", dv$n_files, " distinct files.", call. = FALSE)
  if (!dir.exists(dv$dir)) stop("Data-version directory missing: ", dv$dir, call. = FALSE)
  found <- list.files(dv$dir, pattern = pattern)
  if (!setequal(found, man$file))
    stop("Data version ", dv$id, ": files on disk differ from the manifest (extra: ", paste(setdiff(found, man$file), collapse = ", "),
         "; missing: ", paste(setdiff(man$file, found), collapse = ", "), ")", call. = FALSE)
  f <- file.path(dv$dir, man$file); now <- vapply(f, sha, "", USE.NAMES = FALSE)
  bad <- man$file[now != man[[dv$sha_column]]]
  if (length(bad)) stop("Data version ", dv$id, ": file hash differs from the manifest: ", paste(bad, collapse = ", "), call. = FALSE)
  empty <- data.frame(input = character(), bytes = numeric(), sha256 = character(), role = character(), stringsAsFactors = FALSE)
  sup <- do.call(rbind, c(list(empty), lapply(dv$supporting, function(x) {
    if (!file.exists(x$file) || !identical(sha(x$file), x$sha256)) stop("Data-version supporting file missing or changed: ", x$file, call. = FALSE)
    data.frame(input = x$file, bytes = file.size(x$file), sha256 = x$sha256, role = "data_version_supporting", stringsAsFactors = FALSE) })))
  rbind(data.frame(input = f, bytes = file.size(f), sha256 = now, role = "preprocessed", stringsAsFactors = FALSE),
        data.frame(input = dv$manifest, bytes = file.size(dv$manifest), sha256 = dv$manifest_sha256, role = "data_version_manifest", stringsAsFactors = FALSE),
        sup)
}

#' Check a Stage 29 run's audit/run_inputs.csv against a resolved data version without reading the data:
#' the run's preprocessed inputs must be exactly the manifest files, inside dv$dir, with the manifest hashes.
mmm_dv_verify_run_inputs <- function(dv, run_inputs, pattern = MMM_DV_PRE_PATTERN) {
  man <- utils::read.csv(dv$manifest, stringsAsFactors = FALSE, colClasses = "character")
  pre <- run_inputs[grepl(pattern, basename(run_inputs$input)), , drop = FALSE]
  expect <- man[[dv$sha_column]][match(basename(pre$input), man$file)]
  problems <- c(
    if (nrow(pre) != dv$n_files) paste("run used", nrow(pre), "preprocessed files; data version declares", dv$n_files),
    if (any(.mmm_dv_norm(dirname(pre$input)) != .mmm_dv_norm(dv$dir))) "run read preprocessed files outside the data-version directory",
    if (!setequal(basename(pre$input), man$file)) "the run's preprocessed file set differs from the manifest",
    if (anyNA(expect) || any(pre$sha256 != expect)) "a run input hash differs from the manifest")
  list(ok = !length(problems), problems = problems, n = nrow(pre))
}
