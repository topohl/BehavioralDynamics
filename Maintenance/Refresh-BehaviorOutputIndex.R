# Refresh analysis_ready/output_index.csv and analysis_ready/README.md from the repository. Run from the repository root.
#
#   Rscript Maintenance/Refresh-BehaviorOutputIndex.R
#       dry run: prints every changed index cell and whether the README differs; writes nothing
#   Rscript Maintenance/Refresh-BehaviorOutputIndex.R --write \
#       --backup=output_index_before_<label>.csv
#       copies the current index (and README) to analysis_ready/_migration_control/output_index_before_<label>.csv
#       (and README_before_<label>.md), refusing to overwrite a backup, then writes the new index and README
#
# The index comes from mmm_behavior_output_index() (Functions/behavior_output_index.R); the README is a copy of
# docs/BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md. The project root follows mmm_project_root(). A numbered root that
# is transferring makes the index definition stop, so nothing is written then.
args <- commandArgs(trailingOnly = TRUE)
write_index <- "--write" %in% args
backup_name <- sub("^--backup=", "", grep("^--backup=", args, value = TRUE))
unknown <- setdiff(args, c("--write", grep("^--backup=", args, value = TRUE)))
if (length(unknown)) stop("Unknown argument(s): ", paste(unknown, collapse = " "), call. = FALSE)

env <- new.env(parent = baseenv())
source("Functions/project_paths.R", local = env)
source("Functions/behavior_output_index.R", local = env)
env$base_dir <- env$mmm_project_root()
new_index <- env$mmm_behavior_output_index(env$base_dir)

ready <- file.path(env$base_dir, "analysis_ready")
index_path <- file.path(ready, "output_index.csv")
readme_path <- file.path(ready, "README.md")
readme_source <- file.path("docs", "BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md")
if (!file.exists(index_path)) stop("Missing live index: ", index_path, call. = FALSE)
if (!file.exists(readme_source)) stop("Missing README source: ", readme_source, call. = FALSE)
old_index <- utils::read.csv(index_path, stringsAsFactors = FALSE, check.names = FALSE,
                             na.strings = "NA", colClasses = "character")
new_chr <- as.data.frame(lapply(new_index, as.character), stringsAsFactors = FALSE,
                         check.names = FALSE)
if (!identical(names(old_index), names(new_chr)) ||
    !identical(old_index$stage, new_chr$stage)) {
  cat("Index rows or columns change:\n  old stages:", nrow(old_index),
      "\n  new stages:", nrow(new_chr), "\n")
  cat("  rows added:", paste(setdiff(new_chr$stage, old_index$stage), collapse = ", "), "\n")
  cat("  rows removed:", paste(setdiff(old_index$stage, new_chr$stage), collapse = ", "), "\n")
} else {
  changed <- 0L
  for (column in names(new_chr)) {
    a <- old_index[[column]]
    b <- new_chr[[column]]
    same <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    for (i in which(!same)) {
      changed <- changed + 1L
      cat(sprintf("[%s] %s\n  old: %s\n  new: %s\n", new_chr$stage[[i]], column,
                  a[[i]], b[[i]]))
    }
  }
  cat(changed, "changed cell(s) in", nrow(new_chr), "rows\n")
}
readme_same <- file.exists(readme_path) &&
  identical(readLines(readme_path, warn = FALSE), readLines(readme_source, warn = FALSE))
cat("README:", if (readme_same) "unchanged" else "differs from docs/BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md", "\n")
if (!write_index) {
  cat("Dry run: nothing written.\n")
  quit(save = "no", status = 0L)
}
if (length(backup_name) != 1L ||
    !grepl("^output_index_before_[a-z0-9_]+\\.csv$", backup_name)) {
  stop("--write needs --backup=output_index_before_<label>.csv", call. = FALSE)
}
backup <- file.path(ready, "_migration_control", backup_name)
readme_backup <- file.path(ready, "_migration_control",
                           sub("^output_index_before_(.*)\\.csv$", "README_before_\\1.md", backup_name))
if (file.exists(backup)) stop("Backup already exists: ", backup, call. = FALSE)
if (file.exists(readme_path) && file.exists(readme_backup)) stop("Backup already exists: ", readme_backup, call. = FALSE)
if (!file.copy(index_path, backup, overwrite = FALSE)) {
  stop("Could not back up the current index to: ", backup, call. = FALSE)
}
if (file.exists(readme_path) && !file.copy(readme_path, readme_backup, overwrite = FALSE)) {
  stop("Could not back up the current README to: ", readme_backup, call. = FALSE)
}
readr::write_csv(new_index, index_path, na = "NA")
if (!file.copy(readme_source, readme_path, overwrite = TRUE)) {
  stop("Could not write the README: ", readme_path, call. = FALSE)
}
cat("Backed up:", backup, if (file.exists(readme_backup)) paste("and", readme_backup) else "",
    "\nWrote:", index_path, "and", readme_path, "\nSHA-256:", env$mmm_file_sha256(index_path), "\n")
