# Rebuild only the dashboard's derived figure index and folder guides after a
# verified copy. This does not run Stage 14 or alter authored plots/tables.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: Rscript Refresh-SystemsDashboardMetadata.R <output-dir> <expected-figures> <repo-root>",
       call. = FALSE)
}
output_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
expected_n <- suppressWarnings(as.integer(args[[2]]))
repo_root <- normalizePath(args[[3]], winslash = "/", mustWork = TRUE)
if (is.na(expected_n) || expected_n < 1L ||
    !file.exists(file.path(repo_root, "Analysis", "_pipeline_setup.R"))) {
  stop("Invalid dashboard metadata contract.", call. = FALSE)
}

setwd(repo_root)
suppressPackageStartupMessages(library(purrr))
source("Analysis/_pipeline_setup.R")
figure_files <- function() {
  sort(gsub("\\\\", "/", list.files(
    file.path(output_dir, "figures"),
    pattern = "\\.(svg|pdf|png|jpg|jpeg|tif|tiff|html)$",
    recursive = TRUE, full.names = FALSE, ignore.case = TRUE)))
}
before <- figure_files()
if (length(before) != expected_n) {
  stop("Copied dashboard figure count differs from the reviewed plan.",
       call. = FALSE)
}
inventory <- harmonize_analysis_outputs(output_dir, copy_figures = FALSE)
if (!identical(before, figure_files()) || nrow(inventory) != expected_n ||
    !setequal(gsub("\\\\", "/", inventory$relative_path),
              paste0("figures/", before))) {
  stop("Metadata refresh changed the authored figure set or indexed it incorrectly.",
       call. = FALSE)
}
for (relative in inventory$harmonized_path) {
  if (grepl("(^|/)\\.\\.(/|$)", relative) ||
      !file.exists(file.path(output_dir, relative))) {
    stop("Figure index points to a missing or unsafe path: ", relative,
         call. = FALSE)
  }
}
generated <- c("tables/output_figure_inventory.csv",
               "tables/output_folder_summary.csv",
               paste0("figures/",
                      c("publication_panels", "qc", "supplementary",
                        "exploratory", "interactive"), "/README.txt"))
if (!all(file.exists(file.path(output_dir, generated)))) {
  stop("Dashboard figure metadata is incomplete.", call. = FALSE)
}
cat("PASS: regenerated seven dashboard metadata files for ", expected_n,
    " authored figures without adding mirrors\n", sep = "")
