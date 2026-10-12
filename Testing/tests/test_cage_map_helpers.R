# ===========================================================================
# Cage-map helpers (Functions/cage_map_helpers.R) and how Stage 19 uses them.
# Portable: synthetic data only; nothing is read from or written to the lab drive.
#
#  1. the reader map is the preprocessing grid (animalpos_position_id) and its column map
#  2. animal occupancy is time-weighted over bins/phases, not a mean of per-unit fractions
#  3. group summaries and contrasts refuse more than one row per animal (no phase pseudo-replication)
#  4. contrasts count animals and reproduce Cohen's d by hand
#  5. column pooling sums a column's two readers and refuses inconsistent inputs
#  6. the cage map draws R1-R4 above R5-R8, columns span both rows, and builds at both resolutions
#  7. label helpers: typographic minus, ink on dark fills, caption wrapping
#  8. Stage 19: every phase is in Full_phase, summaries use the helpers, raw source, per-sex log-ratio models
# ===========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(ggplot2)
})
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "cage_map_helpers.R")) &&
           dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

source("Functions/cage_map_helpers.R")
source("Functions/animalpos_preprocessing_helpers.R")

# 1 -------------------------------------------------------------------------
check(identical(animalpos_position_id(RFID_READER_MAP$ReaderX, RFID_READER_MAP$ReaderY), RFID_READER_MAP$PositionID),
      "1: RFID_READER_MAP must be the grid animalpos_position_id() assigns")
check(identical(RFID_READER_MAP$ColumnID, c(1:4, 1:4)), "1: R1/R5 ... R4/R8 share a column")
check(all(RFID_COLUMN_MAP$ColumnX == RFID_READER_MAP$ReaderX[1:4]), "1: column x must be the reader x")
cat("1 reader and column maps: PASS\n")

# 2 -------------------------------------------------------------------------
# One animal, two bins: 1800 s observed with 900 s at R1, then 60 s observed with 60 s at R1.
bins <- tidyr::expand_grid(AnimalNum = "a1", Bin = 1:2, PositionID = 1:8) %>%
  mutate(observation_seconds = ifelse(Bin == 1, 1800, 60),
         PositionSeconds = case_when(PositionID == 1 & Bin == 1 ~ 900, PositionID == 2 & Bin == 1 ~ 900,
                                     PositionID == 1 & Bin == 2 ~ 60, TRUE ~ 0))
a <- rfid_animal_reader_occupancy(bins, "AnimalNum")
r1 <- a$occupancy_fraction[a$PositionID == 1]
check(nrow(a) == 8, "2: one row per animal x reader")
check(abs(r1 - 960 / 1860) < 1e-12, "2: R1 must be 960 / 1860 s (time-weighted)")
check(abs(r1 - mean(c(0.5, 1))) > 0.1, "2: R1 must not be the unweighted mean of the bin fractions (0.75)")
check(abs(sum(a$occupancy_fraction) - 1) < 1e-12, "2: shares must sum to 1 per animal")
check(fails(rfid_animal_reader_occupancy(select(bins, -observation_seconds), "AnimalNum")),
      "2: missing observation_seconds must fail")
cat("2 time weighting: PASS\n")

# 3-4 -----------------------------------------------------------------------
set.seed(19)
animals <- tibble(AnimalNum = sprintf("m%02d", 1:10), Group = rep(c("CON", "SUS"), each = 5))
per_phase <- tidyr::expand_grid(animals, PhaseNumber = 1:3, PositionID = 1:8) %>%
  mutate(occupancy_fraction = runif(n()), Sex = "Male")
check(fails(rfid_group_reader_occupancy(per_phase, c("Sex", "Group"))),
      "3: a group summary over per-phase rows must fail closed")
specs <- tribble(~contrast, ~group_a, ~group_b, "SUS_minus_CON", "SUS", "CON")
check(fails(rfid_reader_occupancy_contrasts(per_phase, "Sex", specs)),
      "3: contrasts over per-phase rows must fail closed")
pooled <- per_phase %>% group_by(AnimalNum, Group, Sex, PositionID) %>%
  summarise(occupancy_fraction = mean(occupancy_fraction), .groups = "drop")
g <- rfid_group_reader_occupancy(pooled, c("Sex", "Group"))
check(all(g$n_animals == 5), "3: n_animals must count animals")
check(all(abs(g$se_occupancy - g$sd_occupancy / sqrt(5)) < 1e-12), "3: SE must use the animal n")
ct <- rfid_reader_occupancy_contrasts(pooled, "Sex", specs)
check(all(ct$n_a == 5 & ct$n_b == 5), "4: contrast n must count animals")
xa <- pooled$occupancy_fraction[pooled$Group == "SUS" & pooled$PositionID == 3]
xb <- pooled$occupancy_fraction[pooled$Group == "CON" & pooled$PositionID == 3]
d_hand <- (mean(xa) - mean(xb)) / sqrt((4 * var(xa) + 4 * var(xb)) / 8)
check(abs(ct$cohens_d[ct$PositionID == 3] - d_hand) < 1e-12, "4: Cohen's d must match the pooled-SD formula")
check(is.na(rfid_cohens_d(1, c(1, 2, 3))), "4: d needs two values per group")
check(is.na(rfid_cohens_d(c(1, 1), c(1, 1))), "4: d is undefined without spread")
cat("3-4 one row per animal, contrasts: PASS\n")

# 5 -------------------------------------------------------------------------
reader_rows <- a %>% left_join(select(RFID_READER_MAP, PositionID, ColumnID), by = "PositionID")
col <- rfid_animal_column_occupancy(reader_rows, "AnimalNum")
check(nrow(col) == 4 && identical(col$ColumnLabel, RFID_COLUMN_MAP$ColumnLabel), "5: four columns per animal")
check(abs(col$occupancy_fraction[col$ColumnID == 1] - sum(a$occupancy_fraction[a$PositionID %in% c(1, 5)])) < 1e-12,
      "5: column 1 must pool R1 and R5")
bad <- reader_rows; bad$observation_seconds[bad$PositionID == 5] <- 10
check(fails(rfid_animal_column_occupancy(bad, "AnimalNum")), "5: inconsistent observed time must fail")
check(fails(rfid_animal_column_occupancy(filter(reader_rows, PositionID != 8), "AnimalNum")),
      "5: a column with one reader must fail")
cat("5 column pooling: PASS\n")

# 6 -------------------------------------------------------------------------
map_dat <- RFID_READER_MAP %>% mutate(value = PositionID / 10, lab = rfid_format_percent(value),
                                      ink = rfid_ink_for_fill(rfid_occupancy_fill(value)))
built <- ggplot_build(rfid_cage_map(map_dat, "value", label_col = "lab", ink_col = "ink", reader_ids = TRUE) +
                        rfid_scale_fill_occupancy())
tiles <- built$data[[2]]
check(nrow(tiles) == 8, "6: eight reader tiles")
y_of <- function(id) tiles$y[which(map_dat$PositionID == id)]
x_of <- function(id) tiles$x[which(map_dat$PositionID == id)]
check(y_of(5) > y_of(1) && y_of(8) > y_of(4), "6: R5-R8 must be drawn above R1-R4 (R5 top left, R1 bottom left)")
check(y_of(5) == y_of(8) && y_of(1) == y_of(4) && x_of(1) == x_of(5) && x_of(1) < x_of(4),
      "6: rows are horizontal, R1/R5 on the left")
check(all(abs((tiles$xmax - tiles$xmin) - (tiles$ymax - tiles$ymin)) < 1e-9), "6: every cell is square")
check(isTRUE(all.equal(abs(y_of(5) - y_of(1)), abs(x_of(2) - x_of(1)))), "6: equal cell pitch in both directions")
check(identical(RFID_READER_DISPLAY_ORDER, paste0("R", c(5:8, 1:4))), "6: reading order R5-R8, then R1-R4")
col_dat <- RFID_COLUMN_MAP %>% mutate(value = c(0.6, 0.2, 0.1, 0.1))
col_built <- ggplot_build(rfid_cage_map(col_dat, "value", resolution = "column") + rfid_scale_fill_occupancy())
ct_tiles <- col_built$data[[2]]
check(nrow(ct_tiles) == 4, "6: four column tiles")
check(all(ct_tiles$ymax >= max(tiles$ymax) - 1e-9 & ct_tiles$ymin <= min(tiles$ymin) + 1e-9),
      "6: a column tile spans both reader rows")
size <- rfid_cage_map_size(3, 2)
check(all(is.finite(size)) && size[["width"]] > 0 && size[["height"]] > 0, "6: positive device size")
check(rfid_cage_map_size(3, 2, caption_lines = 2)[["height"]] > size[["height"]], "6: caption lines add height")
cat("6 cage map geometry: PASS\n")

# 7 -------------------------------------------------------------------------
check(identical(rfid_format_signed(c(-1.234, 0, 2.5, NA)),
                c(paste0(intToUtf8(0x2212), "1.2"), "0.0", "+2.5", "")), "7: signed labels with a typographic minus")
check(identical(rfid_format_percent(c(0.125, NA)), c("12%", "")) ||
        identical(rfid_format_percent(c(0.125, NA)), c("13%", "")), "7: percent labels")
check(identical(rfid_ink_for_fill(c("#0D366B", "#F7FAFE", "#F2CA4E")), c("white", "black", "black")),
      "7: white ink on the dark end only")
check(identical(levels(rfid_contrast_label(c("RES_minus_CON", "SUS_minus_RES"))),
                paste(c("RES", "SUS"), intToUtf8(0x2212), c("CON", "RES"))), "7: contrast labels")
wrapped <- rfid_wrap_caption(strrep("word ", 80), width_in = 4)
check(all(nchar(strsplit(wrapped, "\n")[[1]]) <= floor((4 - 0.15) * 23)), "7: captions wrap to the figure width")
helper_bytes <- readBin("Functions/cage_map_helpers.R", "raw", file.info("Functions/cage_map_helpers.R")$size)
check(all(helper_bytes < as.raw(0x80)), "7: the helper file stays ASCII (code points via intToUtf8)")
cat("7 labels: PASS\n")

# 8 -------------------------------------------------------------------------
src <- paste(readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE), collapse = "\n")
check(grepl('source(.mmm_cage_maps)', src, fixed = TRUE), "8: Stage 19 must source the cage-map helpers")
check(grepl('Window = "Full_phase",', src, fixed = TRUE) &&
        !grepl('PhaseNumber == PRIMARY_PHASE_NUMBER ~ "CC1_active1_fullphase"', src, fixed = TRUE),
      "8: every phase, CC1's first active phase included, must be in the Full_phase window")
check(!grepl("occupancy_fraction = mean(occupancy_fraction", src, fixed = TRUE),
      "8: animal occupancy must not average per-unit fractions")
check(grepl("rfid_animal_reader_occupancy(animal_reader_keys)", src, fixed = TRUE) &&
        grepl("rfid_group_reader_occupancy(", src, fixed = TRUE) &&
        grepl("rfid_reader_occupancy_contrasts(", src, fixed = TRUE),
      "8: summaries must go through the helpers")
check(grepl('OCCUPANCY_SOURCE <- "raw_antenna"', src, fixed = TRUE) && grepl('source(.mmm_raw_antenna)', src, fixed = TRUE),
      "8: occupancy must come from the raw antenna logs")
check(grepl('CAGE_MAP_RESOLUTION <- "reader"', src, fixed = TRUE) &&
        grepl('OCCUPANCY_SOURCE == "animalpos" && CAGE_MAP_RESOLUTION == "reader"', src, fixed = TRUE),
      "8: reader-level (4 x 2) maps, refused for the row-blind AnimalPos source")
check(grepl("clr ~ Group + Batch + (1 | Cage)", src, fixed = TRUE) &&
        grepl("clr ~ Group * CageChange + Batch + (1 | AnimalNum) + (1 | CageEpoch)", src, fixed = TRUE),
      "8: models per sex on log-ratios with Batch fixed and a cage random effect")
check(!grepl("Group * PositionID", src, fixed = TRUE) && !grepl("run_lmer_occupancy", src, fixed = TRUE),
      "8: the rank-deficient pooled model must be gone")
check(grepl('ddf = "Kenward-Roger"', src, fixed = TRUE) && grepl('lmer.df = "kenward-roger"', src, fixed = TRUE) &&
        grepl('p.adjust(p_value, method = "BH")', src, fixed = TRUE),
      "8: Kenward-Roger df and BH-adjusted p-values")
check(grepl("MMM_TILE_BORDER", src, fixed = TRUE), "8: diverging tiles take the shared grey outline")
check(!grepl("scale_fill_viridis", src, fixed = TRUE), "8: occupancy maps use the one-hue occupancy scale")

# Behaviour, not text: run Stage 19's own window and animal-summary statements on a synthetic, production-typed
# phase table (factor Group/Sex/PhaseClass, integer PositionID) and check what reaches the group summary.
exprs <- parse("Analysis/19_spatial_occupancy_maps.R", keep.source = TRUE)
txt <- vapply(attr(exprs, "srcref"), function(s) paste(as.character(s), collapse = "\n"), character(1))
stmt <- function(prefix) {
  i <- which(startsWith(txt, prefix))
  check(length(i) == 1, paste0("8: Stage 19 must have exactly one statement starting '", prefix, "'"))
  exprs[[i]]
}
s19 <- new.env()
for (p in c("GROUP_LEVELS <- ", "SEX_LEVELS <- ", "EPS_LOGIT <- ", "MIN_OBSERVATION_SEC_PER_ANIMAL_WINDOW <- ",
            "PRIMARY_CAGE_CHANGE <- ", "PRIMARY_PHASE <- ", "PRIMARY_PHASE_NUMBER <- ", "POSITION_MAP <- ",
            "safe_logit <- ")) eval(stmt(p), s19)
by_phase <- tidyr::expand_grid(AnimalNum = c("c1", "c2", "s1", "s2"), PhaseNumber = 1:2, PositionID = 1:8) %>%
  mutate(
    Group = factor(ifelse(startsWith(AnimalNum, "c"), "CON", "SUS"), levels = c("CON", "RES", "SUS")),
    Sex = factor("Female", levels = c("Female", "Male")), AnimalID = AnimalNum, Batch = "B3", System = "sys.1",
    CageChangeIndex = 1L, PhaseClass = factor("Active", levels = c("Active", "Inactive")),
    observation_seconds = 43200,
    # phase 1: all time at R1; phase 2: all time at R4 -> pooled 50/50 per animal
    PositionSeconds = ifelse((PhaseNumber == 1 & PositionID == 1) | (PhaseNumber == 2 & PositionID == 4), 43200, 0),
    occupancy_fraction = PositionSeconds / observation_seconds
  ) %>%
  left_join(select(RFID_READER_MAP, PositionID, ReaderX, ReaderY, ReaderLabel, ColumnID), by = "PositionID")
s19$position_occupancy_by_phase <- by_phase
for (p in c("occ_animal_phase_all <- ", "occ_animal_phase <- ")) eval(stmt(p), s19)
s19$occ_animal <- s19$occ_animal_phase
for (p in c("animal_reader_keys <- ", "occ_animal_used <- ", "animal_reader_occ <- ")) eval(stmt(p), s19)
check(!"PhaseNumber" %in% s19$animal_reader_keys, "8: the animal-level keys must not include PhaseNumber")
full <- s19$animal_reader_occ %>% filter(Window == "Full_phase")
check(nrow(full) == 4 * 8, "8: Full_phase must hold one row per animal x reader, phases pooled")
check(all(abs(full$occupancy_fraction[full$ReaderLabel %in% c("R1", "R4")] - 0.5) < 1e-12),
      "8: Full_phase must pool CC1's first active phase with the later ones (R1 and R4 at 50%)")
check(sum(s19$animal_reader_occ$Window == "CC1_active1_fullphase") == 4 * 8,
      "8: CC1's first active phase must also remain available as the fallback primary window")
eval(stmt("group_occ <- rfid_group_reader_occupancy("), s19)
g_full <- s19$group_occ %>% filter(Window == "Full_phase")
check(all(g_full$n_animals == 2), "8: group n must count animals, not phases")
cat("8 Stage 19 wiring: PASS\n")

cat("Cage-map helper checks: PASS\n")
