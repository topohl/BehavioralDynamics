# One colour source: Functions/manuscript_palette.R (manuscript palette v3) and its aliases.
#
#   1. the source: version, group and diverging roles, pin formats, diverging ends distinct from each other and from the
#      white midpoint;
#   2. the aliases (mmm_group_colors, mmm_pair_colors, mmm_diverging_colors, MMM_GROUP_COLOURS, MMM_DIVERGING_COLOURS,
#      MMM_DHM_PALETTE) equal the source;
#   3. no stage or helper repeats a group or diverging colour as a literal, apart from the listed exceptions.
#
# Portable: repository files only.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

cat("1. the palette source\n")
pal <- new.env(parent = baseenv())
sys.source("Functions/manuscript_palette.R", envir = pal)
check(identical(pal$MMM_PALETTE_VERSION, "manuscript_palette_v3"), "1: palette version v3")
check(identical(names(pal$MMM_PALETTE_GROUP), c("CON", "RES", "SUS")) && all(grepl("^#[0-9A-F]{6}$", pal$MMM_PALETTE_GROUP)),
      "1: group colours CON, RES, SUS as upper-case hex")
check(identical(names(pal$MMM_PALETTE_DIVERGING), c("low", "mid", "high")) && all(grepl("^#[0-9A-F]{6}$", pal$MMM_PALETTE_DIVERGING)),
      "1: diverging low, mid, high as upper-case hex")
src <- pal$MMM_PALETTE_SOURCE
check(grepl("^[0-9a-f]{40}$", src$commit) && grepl("^[0-9a-f]{40}$", src$git_blob) && grepl("^[0-9a-f]{64}$", src$sha256) &&
        identical(src$file, "config/manuscript_palette.yml"), "1: the source pin names the manuscript file, commit, blob and sha256")
if (requireNamespace("farver", quietly = TRUE)) {
  de <- function(a, b) as.numeric(farver::compare_colour(farver::decode_colour(a), farver::decode_colour(b), from_space = "rgb",
                                                         method = "cie2000"))
  d <- pal$MMM_PALETTE_DIVERGING
  # v3 (2026-10-06): a white midpoint, chosen over a balanced grey; the yellow arm is the weaker one (documented in the
  # palette source), so the check is distinctness, not balance
  check(identical(d[["mid"]], "#FFFFFF") && de(d[["low"]], d[["mid"]]) > 25 && de(d[["high"]], d[["mid"]]) > 25 && de(d[["low"]], d[["high"]]) > 50,
        sprintf("1: diverging ends distinct from the white midpoint (dE2000 %.1f, %.1f) and from each other (%.1f)",
                de(d[["low"]], d[["mid"]]), de(d[["high"]], d[["mid"]]), de(d[["low"]], d[["high"]])))
} else message("1: farver not installed; balance check skipped")
ok("version, roles, pin and distinct diverging ends")

cat("\n2. aliases\n")
suppressPackageStartupMessages(source("Analysis/_pipeline_setup.R"))
source_mmm_helper("hmm_stage14_helpers.R")
pub <- new.env(); sys.source("Functions/mmm_publication_theme.R", envir = pub)
G <- pal$MMM_PALETTE_GROUP; D <- pal$MMM_PALETTE_DIVERGING
check(identical(mmm_group_colors[c("CON", "RES", "SUS")], G), "2: mmm_group_colors")
check(identical(unname(mmm_pair_colors[c("RES-CON", "SUS-CON")]), unname(G[c("CON", "SUS")])), "2: mmm_pair_colors")
check(identical(mmm_diverging_colors, D), "2: mmm_diverging_colors")
check(identical(pub$MMM_GROUP_COLOURS, G) && identical(pub$MMM_DIVERGING_COLOURS, D), "2: MMM_GROUP_COLOURS and MMM_DIVERGING_COLOURS")
check(identical(MMM_DHM_PALETTE$group, G) && identical(MMM_DHM_PALETTE$diverging, D) &&
        identical(MMM_DHM_PALETTE$version, pal$MMM_PALETTE_VERSION) && identical(MMM_DHM_PALETTE$source_git_blob, src$git_blob),
      "2: MMM_DHM_PALETTE")
ok("every alias equals the source")

cat("\n3. no repeated literals\n")
# current values (white excepted: a generic background and text colour) and the replaced variants (v1, v2)
hexes <- toupper(c(G, D[D != "#FFFFFF"], "#3d3b6e", "#e63947", "#d45b58", "#D98B3A",
                   "#3E3C6F", "#C6C3BB", "#E63A48", "#4C566A", "#D8D2C7", "#96460A"))
EXCEPTIONS <- c("Functions/manuscript_palette.R",
                "Analysis/09_early_prediction_model_ladder.R")          # treated as registered; stays unedited
FROZEN <- "^Functions/(stage30_|stage32_|rfid_)"                         # frozen code keeps its own colours
files <- c(list.files("Analysis", pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE),
           list.files("Functions", pattern = "[.][Rr]$", full.names = TRUE))
files <- files[!grepl("_archive", files) & !files %in% EXCEPTIONS & !grepl(FROZEN, files)]
for (f in files) {
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  hit <- vapply(hexes, function(h) any(grepl(h, toupper(code), fixed = TRUE)), logical(1))
  check(!any(hit), paste0("3: ", f, " repeats palette colour(s) ", paste(hexes[hit], collapse = ", "),
                          "; use MMM_PALETTE_GROUP / MMM_PALETTE_DIVERGING or their aliases"))
}
ok(sprintf("%d scripts and helpers use the palette names (%d listed exceptions)", length(files), length(EXCEPTIONS)))

cat("\nPASS: manuscript palette\n")
