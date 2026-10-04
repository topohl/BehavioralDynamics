# ================================================================
# Frozen-input guard
# MMMSociability
# ================================================================
# The registered runs recorded the sha256 of every input they read (paths relative to the project root):
#   Stage 29 v1.0.1 release  analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507/audit/run_inputs.csv
#   Stage 29b                analysis_ready/pipeline/29b_posthoc_con_contrasts/v1.0_7f1da1f/audit/input_hashes.csv
#   Stage 30                 analysis_ready/pipeline/30_exploratory_screen/v1.0_be71e2f/audit/input_hashes.csv
#   Stage 32 v1.1            analysis_ready/pipeline/32_behavior_exposure_adaptation/v1.1_f4c7a31/audit/input_hashes.csv
# Some of those inputs are ordinary outputs that a producer stage rewrites in place:
#   Stage 01  analysis_ready/foundations/behavior_metrics/10min_based/all_behavior_metrics.csv
#   Stage 09  analysis_ready/pipeline/09_early_prediction/10min/tables/model_ladder_input.csv
#   Stage 28  analysis_ready/pipeline/28_rfid_behavioral_domains/{5min,10min}/tables/acute_window_*.csv
# A routine rerun would replace the bytes these runs were verified against. On the live project root the guard
#   1. refuses to start a stage that writes a pinned file, unless options(mmm.allow_pinned_overwrite = TRUE) or the
#      environment variable MMM_ALLOW_PINNED_OVERWRITE=1 is set; with that permission it first copies the pinned
#      files the stage writes to analysis_ready/_migration_control/pinned_inputs_before_stage<id>_<time>/;
#   2. after the stage, re-hashes every pinned file whose size or time stamp changed, and stops if its bytes no
#      longer match the pin without that permission (a byte-identical rewrite passes).
# A sandbox root holds copies, so the guard does nothing there: it applies only when the project root is the root the
# pin lists record, that root under its UNC name (\\server\share\...\same\path), or a name listed in
# options(mmm.live_root_aliases).
#
# Kept outside the frozen code sets and outside behavioral_dynamics_helpers.R on purpose. Needs base R and digest.
# The CombZ producer (Analysis/build_later_outcome_combz.R) is pinned by its git blob and therefore cannot call it.
# ================================================================

MMM_FROZEN_PIN_LISTS <- c(
  stage29_v101 = "analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507/audit/run_inputs.csv",
  stage29b_v10 = "analysis_ready/pipeline/29b_posthoc_con_contrasts/v1.0_7f1da1f/audit/input_hashes.csv",
  stage30_v10 = "analysis_ready/pipeline/30_exploratory_screen/v1.0_be71e2f/audit/input_hashes.csv",
  stage32_v11 = "analysis_ready/pipeline/32_behavior_exposure_adaptation/v1.1_f4c7a31/audit/input_hashes.csv")

# Output folders (relative to the project root) of the stages that write pinned files. A stage that knows its actual
# output folder passes it instead (Stage 01 honours options(mmm.derived_metrics_dir), e.g. for the cookie data).
MMM_PINNED_WRITER_ROOTS <- list(
  "01" = "analysis_ready/foundations/behavior_metrics",
  "09" = "analysis_ready/pipeline/09_early_prediction",
  "28" = "analysis_ready/pipeline/28_rfid_behavioral_domains")
# Stages whose scripts call the guard themselves (before their first write and at their end); the runner leaves them
# to it. Stage 09 is treated as registered and is not edited: the runner guards it.
MMM_SELF_GUARDED_STAGES <- c("01", "28")

.mmm_fg_norm <- function(p) tolower(sub("/+$", "", gsub("\\\\", "/", p)))

# TRUE when two normalised roots name the same folder: identical, both listed as aliases, or a drive path and its UNC
# name (the UNC root ends with the drive path minus its drive letter).
.mmm_fg_same_place <- function(a, b, aliases = character(0)) {
  if (identical(a, b) || (a %in% aliases && b %in% aliases)) return(TRUE)
  unc_of <- function(u, d) startsWith(u, "//") && grepl("^[a-z]:/.", d) && endsWith(u, substring(d, 3L))
  unc_of(a, b) || unc_of(b, a)
}

#' TRUE when the caller allowed pinned files to be replaced.
mmm_pinned_overwrite_allowed <- function() {
  isTRUE(getOption("mmm.allow_pinned_overwrite", FALSE)) ||
    identical(Sys.getenv("MMM_ALLOW_PINNED_OVERWRITE", unset = ""), "1")
}

#' Pinned derived files (under analysis_ready/) of the frozen runs, mapped onto project_root.
#'
#' NULL when project_root is not the root that the pin lists record (a sandbox or test root holds copies) or when no
#' pin list exists under it. One row per file: rel, path, sha256 and the pin lists naming it.
mmm_frozen_guard_pins <- function(project_root, pin_lists = MMM_FROZEN_PIN_LISTS,
                                  aliases = getOption("mmm.live_root_aliases", character(0))) {
  lists <- file.path(project_root, pin_lists)
  have <- file.exists(lists)
  if (!any(have)) return(NULL)
  rows <- lapply(which(have), function(i) {
    x <- utils::read.csv(lists[i], stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
    if (!all(c("input", "sha256") %in% names(x))) stop("Pin list without input/sha256 columns: ", lists[i], call. = FALSE)
    data.frame(pin_list = names(pin_lists)[i], input = x$input, sha256 = tolower(x$sha256), stringsAsFactors = FALSE)
  })
  pins <- do.call(rbind, rows)
  root <- .mmm_fg_norm(project_root)
  aliases <- .mmm_fg_norm(aliases)
  # each recorded path splits into its root and analysis_ready/...; keep the files whose root is this project root
  input <- gsub("\\\\", "/", pins$input)
  at <- regexpr("/analysis_ready/", tolower(input), fixed = TRUE)
  rec_root <- ifelse(at > 0L, .mmm_fg_norm(substr(input, 1L, at - 1L)), NA_character_)
  same <- !is.na(rec_root) & vapply(rec_root, function(r) !is.na(r) && .mmm_fg_same_place(r, root, aliases), logical(1),
                                    USE.NAMES = FALSE)
  if (!any(same)) return(NULL)
  pins <- pins[same, , drop = FALSE]
  pins$rel <- substring(input[same], at[same] + 1L)
  key <- tolower(pins$rel)
  conflict <- tapply(pins$sha256, key, function(s) length(unique(s)) > 1L)
  if (any(conflict)) stop("Pin lists disagree on the sha256 of: ", paste(names(conflict)[conflict], collapse = "; "), call. = FALSE)
  out <- data.frame(rel = pins$rel[!duplicated(key)], sha256 = pins$sha256[!duplicated(key)], stringsAsFactors = FALSE)
  out$pin_lists <- vapply(tolower(out$rel), function(k) paste(sort(unique(pins$pin_list[key == k])), collapse = ","), "",
                          USE.NAMES = FALSE)
  out$path <- file.path(project_root, out$rel)
  out
}

.mmm_fg_sha <- function(p) if (file.exists(p)) digest::digest(file = p, algo = "sha256") else NA_character_

# TRUE for each path that lies inside one of the folders in roots (none when roots is empty).
.mmm_fg_under <- function(paths, roots) {
  hit <- logical(length(paths))
  for (r in .mmm_fg_norm(roots)) hit <- hit | startsWith(.mmm_fg_norm(paths), paste0(r, "/"))
  hit
}

#' Check before a stage runs. Returns a snapshot for mmm_frozen_guard_after(), or NULL when the guard does not apply.
#'
#' Stops when the stage writes pinned files on the live root without permission. With permission it copies those
#' files to analysis_ready/_migration_control/ first. output_roots: the folders the stage writes into (absolute);
#' default MMM_PINNED_WRITER_ROOTS[[stage]] under project_root, none for other stages.
mmm_frozen_guard_before <- function(stage, project_root = mmm_project_root(), output_roots = NULL) {
  pins <- mmm_frozen_guard_pins(project_root)
  if (is.null(pins)) return(invisible(NULL))
  if (is.null(output_roots) && !is.null(MMM_PINNED_WRITER_ROOTS[[stage]]))
    output_roots <- file.path(project_root, MMM_PINNED_WRITER_ROOTS[[stage]])
  hit <- pins[.mmm_fg_under(pins$path, output_roots), , drop = FALSE]
  allowed <- mmm_pinned_overwrite_allowed()
  if (nrow(hit) && !allowed) {
    stop("Stage ", stage, " rewrites ", nrow(hit), " file(s) that frozen runs pinned by sha256:\n  ",
         paste0(hit$rel, " [", hit$pin_lists, "]", collapse = "\n  "),
         "\nRun it in a sandbox root (MMM_BEHAVIOR_PROJECT_ROOT), or set options(mmm.allow_pinned_overwrite = TRUE) ",
         "after deciding to replace the pinned bytes (the guard then backs them up first).", call. = FALSE)
  }
  if (nrow(hit)) {
    backup <- file.path(project_root, "analysis_ready", "_migration_control",
                        sprintf("pinned_inputs_before_stage%s_%s", stage, format(Sys.time(), "%Y%m%d_%H%M%S")))
    present <- hit[file.exists(hit$path), , drop = FALSE]
    for (i in seq_len(nrow(present))) {
      dest <- file.path(backup, present$rel[i])
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(present$path[i], dest, copy.date = TRUE)) stop("Backup failed: ", present$path[i], call. = FALSE)
    }
    present$sha256_backup <- vapply(file.path(backup, present$rel), .mmm_fg_sha, "", USE.NAMES = FALSE)
    if (nrow(present)) utils::write.csv(present[, c("rel", "sha256", "sha256_backup", "pin_lists")],
                                        file.path(backup, "backup_manifest.csv"), row.names = FALSE)
    message("frozen-input guard: stage ", stage, " may replace ", nrow(hit), " pinned file(s) (mmm.allow_pinned_overwrite); ",
            nrow(present), " backed up to ", backup)
  }
  info <- file.info(pins$path, extra_cols = FALSE)
  pins$size <- info$size
  pins$mtime <- as.numeric(info$mtime)
  attr(pins, "stage") <- stage
  attr(pins, "allowed") <- allowed && nrow(hit) > 0L
  invisible(pins)
}

#' Check after a stage ran: every pinned file whose size or time stamp changed must still match its pin.
mmm_frozen_guard_after <- function(snapshot) {
  if (is.null(snapshot)) return(invisible(TRUE))
  info <- file.info(snapshot$path, extra_cols = FALSE)
  touched <- !mapply(identical, info$size, snapshot$size) | !mapply(identical, as.numeric(info$mtime), snapshot$mtime)
  if (!any(touched)) return(invisible(TRUE))
  t <- snapshot[touched, , drop = FALSE]
  t$sha_now <- vapply(t$path, .mmm_fg_sha, "", USE.NAMES = FALSE)
  same <- !is.na(t$sha_now) & t$sha_now == t$sha256
  stage <- attr(snapshot, "stage")
  if (any(same)) message("frozen-input guard: stage ", stage, " rewrote ", sum(same), " pinned file(s) with identical bytes")
  if (any(!same)) {
    msg <- paste0("Stage ", stage, " changed ", sum(!same), " pinned file(s); the frozen runs no longer match them:\n  ",
                  paste0(t$rel[!same], " [", t$pin_lists[!same], "] pinned ", substr(t$sha256[!same], 1, 8), ", now ",
                         ifelse(is.na(t$sha_now[!same]), "missing", substr(t$sha_now[!same], 1, 8)), collapse = "\n  "))
    if (isTRUE(attr(snapshot, "allowed"))) {
      warning(msg, "\nRecord the replacement in docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md.", call. = FALSE)
    } else {
      stop(msg, "\nRestore the pinned bytes before any frozen run is verified again.", call. = FALSE)
    }
  }
  invisible(!any(!same))
}
