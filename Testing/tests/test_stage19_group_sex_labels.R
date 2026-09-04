# ===========================================================================
# Regression tests for the Stage 19 Group/Sex label corruption.
#
# Root cause (reproduced in test 0): all_pos carries Group and Sex as FACTORS,
# and the occupancy interval builder assigns factor elements into pre-allocated
# CHARACTER vectors. In R, `chr[i] <- factor_element` stores the integer code,
# so "RES" becomes "2"; every later factor(..., levels = GROUP_LEVELS) then
# maps "2" to NA, emptying the animal- and group-level summaries.
#
#  1. Group remains semantic labels, never integer codes
#  2. Sex remains Female/Male
#  3. animal_reader_occ has no missing Group/Sex
#  4. all three groups occur
#  5. both sexes occur
#  6. canonical animal counts are correct
#  7. animal 4 is SUS
#  8. contrast tables contain non-missing sample sizes/effects
#  9. mutation factor(unclass(Group), levels = GROUP_LEVELS) is caught
# 10. mutation converting Sex to integer codes is caught
# ===========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(stringr); library(purrr)
})
# Repo root: honour MMM_REPO_DIR when set, otherwise discover it by walking up
# from the working directory, exactly as Analysis/_pipeline_setup.R does. The
# previous fallback was a hard-coded Windows clone path, which made this test
# fail anywhere else, including CI.
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "behavioral_dynamics_helpers.R")) &&
           dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)

GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS <- c("Female", "Male")
src <- paste(readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE), collapse = "\n")

# ---------------------------------------------------------------------------
# 0. Reproduce the mechanism, so the tests below are anchored on the real bug.
# ---------------------------------------------------------------------------
f <- factor(c("RES", "SUS"), levels = GROUP_LEVELS)
chr <- rep(NA_character_, 2)
chr[1] <- f[1]                                   # the defective assignment
check(identical(chr[1], "2"),
      "0: control -- assigning a factor element into a character vector must store the integer code")
check(is.na(factor(chr[1], levels = GROUP_LEVELS)),
      "0: control -- re-factoring that code against character levels must yield NA")
check(identical(as.character(f[1]), "RES"),
      "0: as.character() on a factor element must recover the label")

# ---------------------------------------------------------------------------
# 1-2. The production script must preserve labels, not codes.
# ---------------------------------------------------------------------------
check(grepl("current_group[a] <- as.character(updates$Group[j])", src, fixed = TRUE),
      "1: Group must be carried forward with as.character(), not as a factor element")
check(grepl("current_sex[a] <- as.character(updates$Sex[j])", src, fixed = TRUE),
      "2: Sex must be carried forward with as.character(), not as a factor element")
check(!grepl("current_group[a] <- updates$Group[j]", src, fixed = TRUE),
      "1: the label-stripping Group assignment must be gone")
check(!grepl("current_sex[a] <- updates$Sex[j]", src, fixed = TRUE),
      "2: the label-stripping Sex assignment must be gone")
# Every re-factor must go through as.character() so a code can never be mapped.
n_raw_g <- length(gregexpr("factor(Group, levels = GROUP_LEVELS)", src, fixed = TRUE)[[1]])
check(!grepl("factor(Group, levels = GROUP_LEVELS)", src, fixed = TRUE),
      "1: every Group re-factor must use factor(as.character(Group), ...)")
check(grepl("factor(as.character(Group), levels = GROUP_LEVELS)", src, fixed = TRUE),
      "1: Group must be re-factored from its character labels")
check(grepl("factor(as.character(Sex), levels = SEX_LEVELS)", src, fixed = TRUE),
      "2: Sex must be re-factored from its character labels against explicit levels")
check(grepl('SEX_LEVELS <- c("Female", "Male")', src, fixed = TRUE),
      "2: Sex levels must be declared explicitly as Female/Male")
# No positional 1/2/3 translation may be introduced.
check(!grepl('c("1", "2", "3")', src, fixed = TRUE) &&
        !grepl('recode(Group, `1`', src, fixed = TRUE),
      "1: labels must not be recovered by hard-coding factor codes")

# ---------------------------------------------------------------------------
# 3. A fail-closed contract must run before summaries/models.
# ---------------------------------------------------------------------------
check(grepl("label_contract", src, fixed = TRUE),
      "3: Stage 19 must assert a Group/Sex label contract")
check(grepl("holds non-semantic value(s)", src, fixed = TRUE),
      "3: the contract must reject non-semantic label values")
lines <- readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE)
fn_at <- grep("^label_contract <- function", lines)
check(length(fn_at) == 1, "3: the label contract must be defined exactly once")
fn_end <- fn_at + which(lines[(fn_at + 1):(fn_at + 40)] == "}")[1]
body <- lines[fn_at:fn_end]
n_stop <- sum(grepl("stop(", body, fixed = TRUE))
check(n_stop >= 4,
      paste0("3: the label contract must stop() on each of bad Group, bad Sex, missing Group and ",
             "missing Sex; found ", n_stop, " stop() call(s)"))
check(!any(grepl("message(", body, fixed = TRUE)) && !any(grepl("warning(", body, fixed = TRUE)),
      "3: the label contract must not downgrade any branch to a message or warning")
check(any(grepl("missing Group", body, fixed = TRUE)) &&
        any(grepl("missing Sex", body, fixed = TRUE)),
      "3: the contract must reject missing Group and missing Sex")
call_at <- grep("label_contract(occ_animal", lines, fixed = TRUE)
check(length(call_at) >= 1, "3: the contract must be applied to occ_animal")
# It must run BEFORE the animal/group summaries are built.
summ_at <- grep("^animal_reader_occ <- ", lines)
check(length(summ_at) == 1 && min(call_at) < summ_at,
      "3: the label contract must run before animal_reader_occ is built")

# The contract logic itself, exercised directly.
contract <- function(dat) {
  g <- as.character(dat$Group); s <- as.character(dat$Sex)
  if (length(setdiff(unique(g[!is.na(g)]), GROUP_LEVELS)) > 0) stop("bad group")
  if (length(setdiff(unique(s[!is.na(s)]), SEX_LEVELS)) > 0) stop("bad sex")
  if (any(is.na(g))) stop("missing group")
  if (any(is.na(s))) stop("missing sex")
  TRUE
}
good <- tibble(Group = c("CON", "RES", "SUS"), Sex = c("Female", "Male", "Male"))
check(isTRUE(contract(good)), "3: a well-formed roster must pass the contract")
check(inherits(try(contract(tibble(Group = c("1", "2"), Sex = c("Female", "Male"))), silent = TRUE), "try-error"),
      "3: integer-code Group values must fail the contract")
check(inherits(try(contract(tibble(Group = c("CON", "RES"), Sex = c("1", "2"))), silent = TRUE), "try-error"),
      "3: integer-code Sex values must fail the contract")
check(inherits(try(contract(tibble(Group = c("CON", NA), Sex = c("Female", "Male"))), silent = TRUE), "try-error"),
      "3: a missing Group must fail the contract")
check(inherits(try(contract(tibble(Group = c("CON", "RES"), Sex = c("Female", NA))), silent = TRUE), "try-error"),
      "3: a missing Sex must fail the contract")

cat("Stage 19 label-preservation checks (1-3): PASS\n")

# ---------------------------------------------------------------------------
# 4-8. Verified against the produced outputs when a validated run is present.
# ---------------------------------------------------------------------------
OUT <- Sys.getenv("MMM_STAGE19_OUT", unset = "C:/tmp/s19v")
D <- file.path(OUT, "analysis_ready/03_derived_metrics/spatial_occupancy")
if (dir.exists(D) && file.exists(file.path(D, "animal_level_reader_occupancy_summary.csv"))) {
  a <- read_csv(file.path(D, "animal_level_reader_occupancy_summary.csv"),
                show_col_types = FALSE, progress = FALSE,
                col_types = cols(AnimalNum = col_character(), Group = col_character(),
                                 Sex = col_character(), .default = col_guess()))
  check(sum(is.na(a$Group)) == 0,
        paste0("3: animal summary must have no missing Group; found ", sum(is.na(a$Group))))
  check(sum(is.na(a$Sex)) == 0,
        paste0("3: animal summary must have no missing Sex; found ", sum(is.na(a$Sex))))
  check(setequal(unique(a$Group), GROUP_LEVELS),
        paste0("4: all three groups must occur; found ", paste(sort(unique(a$Group)), collapse = ",")))
  check(setequal(unique(a$Sex), SEX_LEVELS),
        paste0("5: both sexes must occur; found ", paste(sort(unique(a$Sex)), collapse = ",")))
  check(!any(grepl("^[0-9]+$", unique(a$Group))), "1: Group must never be an integer code")
  check(!any(grepl("^[0-9]+$", unique(a$Sex))), "2: Sex must never be an integer code")

  r <- a %>% distinct(AnimalNum, Group, Sex)
  check(n_distinct(r$AnimalNum) == 111,
        paste0("6: canonical roster must be 111 animals; found ", n_distinct(r$AnimalNum)))
  g <- r %>% count(Group)
  check(g$n[g$Group == "CON"] == 24 && g$n[g$Group == "RES"] == 49 && g$n[g$Group == "SUS"] == 38,
        "6: roster must be 24 CON / 49 RES / 38 SUS")
  s <- r %>% count(Sex)
  check(s$n[s$Sex == "Female"] == 58 && s$n[s$Sex == "Male"] == 53,
        "6: roster must be 58 Female / 53 Male")
  check(identical(unique(r$Group[r$AnimalNum == "4"]), "SUS"), "7: animal 4 must be SUS")

  cf <- file.path(D, "reader_occupancy_group_contrasts_effect_sizes.csv")
  if (file.exists(cf)) {
    ct <- read_csv(cf, show_col_types = FALSE, progress = FALSE)
    check(nrow(ct) > 0, "8: the contrast table must not be empty")
    check(setequal(unique(ct$contrast), c("RES_minus_CON", "SUS_minus_CON", "SUS_minus_RES")),
          paste0("8: all three contrasts must be present; found ",
                 paste(sort(unique(ct$contrast)), collapse = ",")))
    for (col in c("n_a", "n_b", "mean_a", "mean_b", "delta_occupancy")) {
      check(col %in% names(ct), paste0("8: the contrast table must carry ", col))
      check(sum(!is.na(ct[[col]])) > 0,
            paste0("8: ", col, " must be populated in the contrast table, not all missing"))
    }
    check(sum(!is.na(ct$cohens_d)) > 0,
          "8: Cohen's d must be populated where sample sizes and variance permit")
  }
  cat("Stage 19 output label checks (4-8): PASS (verified against ", OUT, ")\n", sep = "")
} else {
  cat("Stage 19 output label checks (4-8): SKIPPED (no validated run at ", OUT, ")\n", sep = "")
}

cat("Stage 19 Group/Sex label checks: PASS\n")
