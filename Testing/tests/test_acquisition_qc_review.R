# Hand-calculated eight-reader overlap, masks, roster and method pairing contracts.
# Pure fixtures only; no laboratory file is read or written and no model is fit.
suppressPackageStartupMessages(library(data.table))
source("Functions/acquisition_qc_review.R")
check <- function(x, m) if (!isTRUE(x)) stop(m, call. = FALSE)
refuses <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); "NO_ERROR" }, error = conditionMessage)
  check(grepl(pattern, msg), paste("Expected refusal", pattern, "but got", msg))
}
pair <- data.table(A = c("1", "1", "2"), B = c("2", "3", "3"), joint_s = c(100, 200, 300), same_s = c(10, 40, 90))
d <- rbindlist(lapply(c("event_start", "read"), function(b) rbindlist(lapply(c(Inf, 60), function(delta) {
  x <- copy(pair); if (is.finite(delta)) x[, `:=`(joint_s = joint_s / 2, same_s = same_s / 2)]
  x[, `:=`(SourceFile = "fixture", Batch = "B1", CC = "CC1", window = "active", System = "sys.1", basis = b, tau = 2, delta = delta,
    indep_same = .25, short_null_same_mean = .3, long_null_same_mean = .35)]; x
}))))
a <- data.table(SourceFile = "fixture", AnimalNum = as.character(1:3), Batch = "B1", CC = "CC1", System = "sys.1", CageEpisodeID = "B1|sys.1|CC1")
bl <- data.table(SourceFile = "fixture", Phase = c("Active", "Inactive"), PhaseNumber = c(1L, 2L), be = c(1000, 44200))
m <- data.table(action = character(), CageEpisodeID = character(), from = character())
r <- aqr_overlap(d, a, bl, m)
check(nrow(r$dyads) == 12 && nrow(r$episodes) == 4, "Wrong fixture grain")
check(all(abs(r$episodes$observed - 140 / 600) < 1e-12), "Must use ratio of seconds, not mean dyad fractions")
check(all(abs(r$episodes$excess_short - (140 / 600 - .3)) < 1e-12), "Wrong reference centring")
check(all(r$paired$read_minus_event_pp == 0), "Identical bases must have zero paired difference")
check(all(r$coverage$supported_pair_share == .5), "Wrong joint support coverage")
check(abs(r$animals[AnimalNum == "1" & basis == "read" & is.infinite(delta)]$observed - 50 / 300) < 1e-12,
      "Animal overlap must be partner-time weighted")
check(!any(c("same_col_s", "adj_col_s", "Group", "CombZ") %in% names(r$dyads)), "Column/outcome fields must not enter the new analysis")
x <- copy(d); x[, same_col_s := 999999]
check(identical(aqr_overlap(x, a, bl, m)$episodes, r$episodes), "Column metrics must be ignored")
refuses(aqr_overlap(rbind(d, d[1]), a, bl, m), "Duplicate unordered")
x <- copy(d); x[1, `:=`(A = "2", B = "1")]
refuses(aqr_overlap(rbind(d, x[1]), a, bl, m), "Duplicate unordered")
refuses(aqr_overlap(d[-1], a, bl, m), "Missing or extra method")
x <- copy(d); x[1, same_s := joint_s + 1]
refuses(aqr_overlap(x, a, bl, m), "Invalid dyad")
x <- copy(d); x[1, short_null_same_mean := NA_real_]
refuses(aqr_overlap(x, a, bl, m), "Invalid dyad")
x <- copy(a); x[1, System := "sys.2"]
refuses(aqr_overlap(d, x, bl, m), "cage roster")
refuses(aqr_overlap(d, rbind(a, a[1]), bl, m), "Ambiguous srs roster")
cen <- data.table(action = "censor", CageEpisodeID = "B1|sys.1|CC1", from = "1970-01-01 00:08:20")
refuses(aqr_overlap(d, a, bl, cen), "Censor overlaps")
# Endpoint equality is safe: half-open windows end at the censor instant.
cen$from <- "1970-01-01 00:16:40"
check(nrow(aqr_overlap(d, a, bl, cen)$episodes) == 4, "Censor at window end must pass")
# Exclude a second full episode before pairing/summaries, preserving original roster.
d2 <- copy(d)[, `:=`(SourceFile = "excluded", Batch = "B2", CC = "CC4", System = "sys.5")]
a2 <- copy(a)[, `:=`(SourceFile = "excluded", Batch = "B2", CC = "CC4", System = "sys.5", CageEpisodeID = "B2|sys.5|CC4")]
bl2 <- copy(bl)[, SourceFile := "excluded"]
ex <- data.table(action = "exclude_spatial", CageEpisodeID = "B2|sys.5|CC4", from = "span_start")
z <- aqr_overlap(rbind(d, d2), rbind(a, a2), rbind(bl, bl2), ex)
check(identical(z$episodes, r$episodes) && any(z$exclusion_audit$excluded), "Excluded episode leaked into a summary")
check(nrow(a2) == 3, "Do not mutate the roster input")
ex_all <- data.table(action = "exclude_spatial", CageEpisodeID = "B1|sys.1|CC1", from = "span_start")
refuses(aqr_overlap(d, a, bl, ex_all), "No retained dyad")
cat("PASS: acquisition QC review eight-reader overlap arithmetic, scope, exclusions, censoring and paired-method contracts\n")
