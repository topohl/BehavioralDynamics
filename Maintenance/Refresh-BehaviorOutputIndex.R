# Refresh analysis_ready/output_index.csv from the Stage 16 index definition
# without running the Stage 16 exporter. Run from the repository root.
#
#   Rscript Maintenance/Refresh-BehaviorOutputIndex.R
#       dry run: prints every changed cell and writes nothing
#   Rscript Maintenance/Refresh-BehaviorOutputIndex.R --write \
#       --backup=output_index_before_<label>.csv
#       copies the current index to analysis_ready/_migration_control/<backup>
#       (refusing to overwrite a backup), then writes the new index
#
# The project root follows mmm_project_root(). A numbered root that is
# transferring makes the index definition stop, so nothing is written then.
args <- commandArgs(trailingOnly = TRUE)
write_index <- "--write" %in% args
backup_name <- sub("^--backup=", "", grep("^--backup=", args, value = TRUE))
unknown <- setdiff(args, c("--write", grep("^--backup=", args, value = TRUE)))
if (length(unknown)) stop("Unknown argument(s): ", paste(unknown, collapse = " "), call. = FALSE)

source_lines <- readLines("Analysis/16_manuscript_behavior_report.R", warn = FALSE)
start <- grep("^output_group_index <- function\\(group\\) \\{$", source_lines)
end <- grep("^if \\(anyDuplicated\\(na.omit\\(output_index\\$canonical_path\\)\\)\\) \\{$",
            source_lines)
if (length(start) != 1L || length(end) != 1L || end <= start) {
  stop("Cannot locate the Stage 16 index definition.", call. = FALSE)
}
env <- new.env(parent = baseenv())
env$tribble <- tibble::tribble
source("Functions/project_paths.R", local = env)
env$base_dir <- env$mmm_project_root()
eval(parse(text = paste(source_lines[start:(end - 1L)], collapse = "\n")), envir = env)
new_index <- env$output_index
if (anyDuplicated(na.omit(new_index$canonical_path))) {
  stop("output_index.csv contains duplicate canonical paths.", call. = FALSE)
}

ready <- file.path(env$base_dir, "analysis_ready")
index_path <- file.path(ready, "output_index.csv")
if (!file.exists(index_path)) stop("Missing live index: ", index_path, call. = FALSE)
old_index <- utils::read.csv(index_path, stringsAsFactors = FALSE, check.names = FALSE,
                             na.strings = "NA", colClasses = "character")
new_chr <- as.data.frame(lapply(new_index, as.character), stringsAsFactors = FALSE,
                         check.names = FALSE)
if (!identical(names(old_index), names(new_chr)) ||
    !identical(old_index$stage, new_chr$stage)) {
  cat("Index rows or columns change:\n  old stages:", nrow(old_index),
      "\n  new stages:", nrow(new_chr), "\n")
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
if (!write_index) {
  cat("Dry run: nothing written.\n")
  quit(save = "no", status = 0L)
}
if (length(backup_name) != 1L ||
    !grepl("^output_index_before_[a-z0-9_]+\\.csv$", backup_name)) {
  stop("--write needs --backup=output_index_before_<label>.csv", call. = FALSE)
}
backup <- file.path(ready, "_migration_control", backup_name)
if (file.exists(backup)) stop("Backup already exists: ", backup, call. = FALSE)
if (!file.copy(index_path, backup, overwrite = FALSE)) {
  stop("Could not back up the current index to: ", backup, call. = FALSE)
}
readr::write_csv(new_index, index_path, na = "NA")
cat("Backed up:", backup, "\nWrote:", index_path, "\nSHA-256:",
    env$mmm_file_sha256(index_path), "\n")
