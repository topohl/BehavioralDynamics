# Stage 15's default primary behavior matrix must exclude HMM-derived features,
# including the direct five-minute curated tables and the HMM summary features.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

source_file <- "Analysis/15_behavior_proteomics_integration.R"
lines <- readLines(source_file, warn = FALSE)
stopifnot(any(grepl('include_hmm_in_primary_axes <- getOption("mmm.stage15.include_hmm_in_primary_axes", FALSE)',
                  lines, fixed = TRUE)),
          any(grepl("classify_behavior_feature_roles(include_hmm_in_primary_axes)",
                    lines, fixed = TRUE)),
          any(grepl("behavior_matrix <- build_primary_behavior_matrix(feature_long)",
                    lines, fixed = TRUE)))

exprs <- as.list(parse(file = source_file))
env <- new.env(parent = globalenv())
for (name in c("collapse_behavior_feature_rows", "classify_behavior_feature_roles",
               "build_primary_behavior_matrix")) {
  found <- Filter(function(expr) is.call(expr) &&
                    identical(expr[[1]], as.name("<-")) &&
                    identical(expr[[2]], as.name(name)), exprs)
  stopifnot(length(found) == 1L)
  eval(found[[1]], envir = env)
}

features <- tibble(
  AnimalNum = rep(c("1", "2"), each = 3),
  Group = "CON",
  Sex = "male",
  feature = rep(c("raw_multiscale__movement", "hmm_states__5min__dwell",
                  "hmm__10min__occupancy"), times = 2),
  FeatureValue = c(1, 2, 3, 4, 5, 6)
)

default <- env$classify_behavior_feature_roles(features)
stopifnot(identical(default$in_primary_axes,
                    rep(c(TRUE, FALSE, FALSE), times = 2)),
          identical(default$feature_stability_class[c(2, 3)],
                    rep("hmm_multi_optimum_unstable", 2)))
primary <- env$build_primary_behavior_matrix(default)
stopifnot(nrow(primary) == 2L,
          "raw_multiscale__movement" %in% names(primary),
          !any(grepl("^(hmm_states|hmm)__", names(primary))),
          identical(primary$raw_multiscale__movement, c(1, 4)))

opted_in <- env$classify_behavior_feature_roles(features, include_hmm = TRUE)
sensitivity <- env$build_primary_behavior_matrix(opted_in)
stopifnot(all(opted_in$in_primary_axes),
          all(c("hmm_states__5min__dwell", "hmm__10min__occupancy") %in%
                names(sensitivity)))

cat("Stage 15 HMM primary-axis exclusion: PASS\n")
