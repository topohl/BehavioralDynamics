# Contract tests for systems_module_scorecards.csv.
#
# WHY THIS EXISTS
#
# The canonical file used to be written twice to the same path: a provisional
# base-only version, then a prediction-enriched version that superseded it -
# but only inside `if (length(available_outcomes) > 0)`. Endpoint columns reach
# systems_features through read_any_table() on an .xlsx workbook on a network
# share, and read_any_table() returns NULL silently when file.exists() is
# false. One unreachable moment left the canonical file with 13 columns instead
# of 15, with no error and no warning: the SCHEMA encoded whether the share had
# been up.
#
# The contract is now: one schema, one write, always. Absence of prediction is
# a VALUE ("not_available"), never a missing column.
#
# Portable: source-level checks plus in-memory fixtures. No project root needed.

suppressPackageStartupMessages({ library(dplyr) })

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

STAGE14 <- "Analysis/14_systems_neuroscience_summary_dashboard.R"
check(file.exists(STAGE14), "Stage 14 source is missing")
s14 <- readLines(STAGE14, warn = FALSE)
code <- s14[!grepl("^\\s*#", s14)]

CANON <- "tables/systems_module_scorecards.csv"
ENRICH <- c("PredictionReadout", "PredictionInterpretation")

# ---------------------------------------------------------------------------
cat("\n[E] exactly one canonical write site\n")
# ---------------------------------------------------------------------------

writes <- grep(paste0('write_table\\(.*"', CANON, '"'), code)
check(length(writes) == 1L,
      paste0("systems_module_scorecards.csv must have exactly ONE write site; found ",
             length(writes), ". Two writes to one canonical path is how its schema ",
             "became dependent on whether the enrichment branch ran."))
ok("single canonical write site")

check(!any(grepl("supersede = TRUE", code, fixed = TRUE)),
      paste0("supersede = TRUE is back in Stage 14. Provisional-then-supersede is ",
             "unsafe when the two versions differ in SHAPE rather than only values."))
ok("no supersede-based intra-run overwrite remains")

# The write must NOT sit inside the availability conditional.
# Anchor on the statement itself: prose elsewhere in the file quotes this
# condition, so a plain fixed-string search matches comments too.
gate <- grep("^if \\(length\\(available_outcomes\\) > 0\\) \\{", s14)
check(length(gate) == 1L, "cannot locate the available_outcomes conditional")
gate_end <- gate + which(s14[(gate + 1):length(s14)] == "}")[1]
write_line <- grep(paste0('write_table\\(.*"', CANON, '"'), s14)
check(length(write_line) == 1L, "cannot locate the canonical write in raw source")
check(write_line > gate_end,
      paste0("the canonical write (line ", write_line, ") is inside the ",
             "available_outcomes conditional (lines ", gate, "-", gate_end,
             "); its schema would again depend on endpoint availability"))
ok(paste0("canonical write at line ", write_line, " is outside the conditional (ends ", gate_end, ")"))

# ---------------------------------------------------------------------------
cat("\n[A] the unenriched path still carries the full schema\n")
# ---------------------------------------------------------------------------

init <- grep("^module_scorecards <- module_scorecards_base", code)
check(length(init) >= 1L,
      "module_scorecards is not initialised from module_scorecards_base outside the conditional")
init_blk <- paste(code[init[1]:min(init[1] + 6, length(code))], collapse = " ")
for (cn in ENRICH) {
  check(grepl(cn, init_blk, fixed = TRUE),
        paste0("the unenriched initialisation does not create '", cn,
               "'; the column would vanish when the endpoint workbook is unreachable"))
}
check(grepl("not_available", init_blk, fixed = TRUE),
      "the unenriched path must mark PredictionInterpretation as \"not_available\"")
ok("unenriched initialisation creates both enrichment columns with an explicit missing value")

# ---------------------------------------------------------------------------
cat("\n[C/E] schema is normalised and asserted before writing\n")
# ---------------------------------------------------------------------------

check(any(grepl("MMM_MODULE_SCORECARD_COLUMNS", code, fixed = TRUE)),
      "no canonical column vector is defined")
norm_i <- grep("MMM_MODULE_SCORECARD_COLUMNS", code)
norm_blk <- paste(code[min(norm_i):min(max(norm_i) + 12, length(code))], collapse = " ")
check(grepl("select(all_of(MMM_MODULE_SCORECARD_COLUMNS))", norm_blk, fixed = TRUE),
      "column ORDER is not pinned before the write")
check(grepl("arrange(", norm_blk, fixed = TRUE),
      "row ORDER is not pinned before the write")
check(grepl("as.numeric(PredictionReadout)", norm_blk, fixed = TRUE) &&
      grepl("as.character(PredictionInterpretation)", norm_blk, fixed = TRUE),
      "enrichment column TYPES are not pinned before the write")
ok("column set, column order, row order and types are all pinned")

# ---------------------------------------------------------------------------
cat("\n[F] join safety around the enrichment\n")
# ---------------------------------------------------------------------------

join_i <- grep("left_join(module_prediction_map", code, fixed = TRUE)
check(length(join_i) == 1L, "cannot locate the enrichment join")
join_blk <- paste(code[max(1, join_i - 6):min(join_i + 12, length(code))], collapse = " ")
check(grepl('relationship = "one-to-one"', join_blk, fixed = TRUE),
      "the enrichment join does not declare a one-to-one relationship; a duplicated Module key would silently fan out rows")
check(grepl("anyDuplicated(module_prediction_map$Module)", join_blk, fixed = TRUE),
      "enrichment key uniqueness is not asserted")
check(grepl("nrow(module_scorecards) == nrow(module_scorecards_base)", join_blk, fixed = TRUE),
      "row-universe preservation is not asserted after the enrichment join")
ok("key uniqueness, one-to-one join and row preservation are all asserted")

# ---------------------------------------------------------------------------
cat("\n[A/B/D] behavioural: both paths yield an identical schema\n")
# ---------------------------------------------------------------------------
#
# Reproduces the two code paths in miniature and asserts that what differs is
# values, never shape. This is the property the old code violated.

base_tbl <- tibble(
  Module = c("Magnitude", "Social topology", "Other interpretable feature"),
  n_features = c(201L, 201L, 176L),
  median_abs_hedges_g = c(0.31, 0.34, 0.34)
)
CANON_COLS <- c(names(base_tbl), ENRICH)

# Path A: no enrichment available.
unenriched <- base_tbl %>%
  mutate(PredictionReadout = NA_real_, PredictionInterpretation = "not_available") %>%
  select(all_of(CANON_COLS)) %>%
  arrange(Module)

# Path B: enrichment available for a SUBSET of modules (the realistic case -
# "Other interpretable feature" has no prediction entry).
pred_map <- tibble(Module = c("Magnitude", "Social topology"),
                   PredictionReadout = c(0.133, -0.0099))
stopifnot(!anyDuplicated(pred_map$Module))
enriched <- base_tbl %>%
  left_join(pred_map, by = "Module", relationship = "one-to-one") %>%
  mutate(PredictionInterpretation = case_when(
    !is.finite(PredictionReadout) ~ "not_available",
    PredictionReadout > 0.05 ~ "improves_prediction",
    PredictionReadout > 0 ~ "small_positive_increment",
    TRUE ~ "no_increment_or_overfit")) %>%
  select(all_of(CANON_COLS)) %>%
  arrange(Module)

check(identical(names(unenriched), names(enriched)),
      "the two paths produce different column sets")
check(identical(dim(unenriched), dim(enriched)),
      "the two paths produce different dimensions")
check(identical(unenriched$Module, enriched$Module),
      "the two paths produce different row ordering")
check(identical(vapply(unenriched, class, character(1)),
                vapply(enriched, class, character(1))),
      "the two paths produce different column types")
ok("schema, dimensions, row order and types identical across both paths")

# A module absent from the prediction map keeps its row and is marked, not dropped.
check(nrow(enriched) == nrow(base_tbl),
      "the enrichment join changed the module row universe")
check(enriched$PredictionInterpretation[enriched$Module == "Other interpretable feature"] ==
        "not_available",
      "a module with no prediction entry must be marked not_available, not dropped")
check(!anyNA(enriched$PredictionInterpretation),
      "PredictionInterpretation must never be NA; absence is an explicit token")
ok("modules without a prediction entry are retained and marked")

# Values do differ where enrichment exists - otherwise the test proves nothing.
check(!identical(unenriched$PredictionReadout, enriched$PredictionReadout),
      "enrichment produced no value change; the fixture is not exercising path B")
ok("values differ between paths while shape does not")

# ---------------------------------------------------------------------------
cat("\n[C] repeated identical execution is stable\n")
# ---------------------------------------------------------------------------

again <- base_tbl %>%
  left_join(pred_map, by = "Module", relationship = "one-to-one") %>%
  mutate(PredictionInterpretation = case_when(
    !is.finite(PredictionReadout) ~ "not_available",
    PredictionReadout > 0.05 ~ "improves_prediction",
    PredictionReadout > 0 ~ "small_positive_increment",
    TRUE ~ "no_increment_or_overfit")) %>%
  select(all_of(CANON_COLS)) %>%
  arrange(Module)
check(identical(enriched, again), "repeating the enrichment produced a different table")

f1 <- tempfile(fileext = ".csv"); f2 <- tempfile(fileext = ".csv")
readr::write_csv(enriched, f1); readr::write_csv(again, f2)
check(identical(unname(tools::md5sum(f1)), unname(tools::md5sum(f2))),
      "serialising the same table twice produced different bytes")
unlink(c(f1, f2))
ok("repeated execution is byte-identical")

cat("\nStage 14 module-scorecard contract checks: PASS\n")
