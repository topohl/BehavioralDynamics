# The R resolver and the PowerShell archive tools must know the same set of
# original-layout roots. Static: both files are parsed, nothing is run.
code <- as.list(parse("Functions/project_paths.R", keep.source = FALSE))
definition <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
                       identical(e[[2L]], as.name("MMM_NUMBERED_BEHAVIOR_ROOTS")), code)
stopifnot(length(definition) == 1L)
r_roots <- eval(definition[[1L]][[3L]], baseenv())

ps <- paste(readLines("Maintenance/BehaviorNumberedRootLocation.ps1", warn = FALSE),
            collapse = "\n")
block <- regmatches(ps, regexpr("\\$BehaviorNumberedRoots = @\\([^)]*\\)", ps))
stopifnot(length(block) == 1L)
ps_roots <- regmatches(block, gregexpr("'[^']+'", block))[[1L]]
ps_roots <- gsub("'", "", ps_roots, fixed = TRUE)

stopifnot(!anyDuplicated(r_roots), !anyDuplicated(ps_roots),
          setequal(r_roots, ps_roots),
          all(grepl("^[A-Za-z0-9_]+$", r_roots)))
# The archive tools take their RootName list from the shared PowerShell file.
for (tool in c("Maintenance/Invoke-BehaviorNumberedRootArchive.ps1",
               "Maintenance/Invoke-BehaviorNumberedRootArchiveManifest.ps1",
               "Maintenance/New-BehaviorArchiveReaderGateTemplate.ps1")) {
  text <- paste(readLines(tool, warn = FALSE), collapse = "\n")
  stopifnot(grepl("Test-BehaviorNumberedRootName", text, fixed = TRUE),
            !grepl("ValidateSet\\([^)]*'03_derived_metrics'", text))
}
cat("Numbered root lists agree across R and PowerShell: PASS\n")
