# Discriminating fixtures for fixed phase windows, equal-batch cage summaries,
# paired bootstrap and missing-recording safeguards. No real biological input.
source("Analysis/31_cc4_grid_exposure.R")
check <- function(x, msg) if (!isTRUE(x)) stop("FAIL: ", msg, call. = FALSE)
fails <- function(expr, pattern) {
  err <- tryCatch({force(expr); NULL}, error = identity)
  check(inherits(err, "error") && grepl(pattern, conditionMessage(err)), paste("expected",pattern))
}
rows <- list()
# Unequal cages/batch and animals/cage distinguish equal batch/cage weighting
# from a pooled animal average. Batch B1: two cages, B2: three cages.
for (b in 1:2) for (c in seq_len(b+1)) for (a in seq_len(c)) for (ph in 2:5) {
  st <- as.POSIXct("2022-11-09 06:30:00", tz="UTC") + (ph-1)*86400
  baseline <- 10*b+c+a
  change <- if (ph == 2) 0 else (ph-2)*(10*b+c)
  rows[[length(rows)+1L]] <- tibble::tibble(Batch=paste0("B",b), CageChange="CC4", System=paste0("sys.",c),
    AnimalID=paste(b,c,a,sep="_"), AnimalNum=paste(b,c,a,sep="_"), Sex="Male", Phase="Inactive",
    PhaseLabel=paste0("I",ph), PhaseNumber=ph, BlockStart=st, BlockEnd=st+43200,
    BinIndex=0:143, Crossings=c(12*(baseline+change),rep(0,143)),
    SessionSpansWholeBlock=TRUE, AnimalSpansWholeBlock=TRUE, SpanOverlapSec=300)
}
bins <- dplyr::bind_rows(rows)
w <- grid31_whole_inactive(bins)
check(nrow(w$animals)==36 && all(w$animals$IncludedInPrimary),"four phases per animal retained")
u <- grid31_whole_inactive_uncertainty(w$cages,draws=200,seed=17)
# B1 cage changes 11,12 -> 11.5; B2 21,22,23 -> 22. Equal batch mean = 16.75.
check(abs(u$summary$Estimate[u$summary$Measure=="I3-I2"]-16.75)<1e-12,"equal batch weighting, not cage/animal pooling")
check(abs(u$summary$Estimate[u$summary$Measure=="I5-I2"]-50.25)<1e-12,"paired I5 difference")
check(abs(u$summary$Estimate[u$summary$Measure=="mean_I3_I5-I2"]-33.5)<1e-12,"secondary average retains paired baseline")
check(all(abs(u$draws$`I5-I2`-3*u$draws$`I3-I2`)<1e-12),"every bootstrap draw resamples whole trajectories, not independent days")
set.seed(222); state <- .Random.seed
u2 <- grid31_whole_inactive_uncertainty(w$cages,draws=200,seed=17)
check(identical(u$draws,u2$draws) && identical(state,.Random.seed),"fixed seed reproducibility and caller RNG state preserved")
set.seed(1); shuffled <- bins[sample.int(nrow(bins)), ]
u3 <- grid31_whole_inactive_uncertainty(grid31_whole_inactive(shuffled)$cages,draws=200,seed=17)
check(identical(u$draws,u3$draws),"row-order invariance")
# One animal loses one phase. Its cage must leave the main cohort on ALL days;
# the other animal remains eligible descriptively, without replacing the roster.
lost <- bins
lost$AnimalSpansWholeBlock[lost$AnimalID=="2_2_1" & lost$PhaseLabel=="I4"] <- FALSE
lost$SpanOverlapSec[lost$AnimalID=="2_2_1" & lost$PhaseLabel=="I4"] <- 0
wl <- grid31_whole_inactive(lost)
check(all(!wl$animals$IncludedInPrimary[wl$animals$CageID=="B2|sys.2|CC4"]),"partial cage excluded consistently including baseline")
check(is.na(wl$animals$CrossingsPerHour[wl$animals$AnimalID=="2_2_1" & wl$animals$PhaseLabel=="I4"]),"missing phase cannot become low activity")
check(is.finite(wl$animals$CrossingsPerHour[wl$animals$AnimalID=="2_2_2" & wl$animals$PhaseLabel=="I4"]),"valid animal retained in audit")
# Recorded zero crossings over a bracketed complete phase stays zero, not missing.
zero <- bins;zero$Crossings[zero$AnimalID=="1_1_1" & zero$PhaseLabel=="I3"] <- 0
wz <- grid31_whole_inactive(zero)
check(wz$animals$CrossingsPerHour[wz$animals$AnimalID=="1_1_1" & wz$animals$PhaseLabel=="I3"]==0,"complete observed zero retained")
# Opt-in Stage 31 censoring: cage B2|sys.2 is unobserved from I4 on. Against the
# same lost phases without censor columns, only the labels and the appended cage
# counts differ; eligibility, rates, cohorts and estimates are unchanged.
check(identical(names(w$cages),c("Sex","Batch","CageChange","System","CageID","PhaseLabel","ExpectedAnimals","EligibleAnimals",
  "RecordedCrossings","CompleteRosterPhase","IncludedInPrimary","CageMeanCrossingsPerHour")) && !"CensoredAnimals" %in% names(w$cage_pairs),
  "legacy cage schema without censor columns")
off <- bins$Batch=="B2" & bins$System=="sys.2" & bins$PhaseLabel %in% c("I4","I5")
uncens <- bins; uncens$AnimalSpansWholeBlock[off] <- FALSE; uncens$SpanOverlapSec[off] <- 0; uncens$Crossings[off] <- 0
cens <- uncens; cens$CensoredSec <- ifelse(off,300,0); cens$CensorRecord <- ifelse(bins$Batch=="B2" & bins$System=="sys.2","SX04",NA_character_)
wu <- grid31_whole_inactive(uncens); wc <- grid31_whole_inactive(cens)
lab <- wc$animals$CageID=="B2|sys.2|CC4" & wc$animals$PhaseLabel %in% c("I4","I5")
same <- function(x,y,cols=names(y)) identical(as.data.frame(x[cols]),as.data.frame(y[cols]))
check(sum(lab)==4 && all(wc$animals$PhaseStatus[lab]=="censored_after_last_read_SX04") && identical(wc$animals$PhaseStatus[!lab],wu$animals$PhaseStatus[!lab]) &&
      identical(names(wc$animals),names(wu$animals)) && same(wc$animals,wu$animals,setdiff(names(wu$animals),"PhaseStatus")),"censored phases labelled; eligibility and rates unchanged")
check(same(wc$animal_pairs,wu$animal_pairs,setdiff(names(wu$animal_pairs),"PhaseStatus")) && same(wc$population,wu$population),"pairs and population unchanged")
check(identical(names(wc$cages),c(names(wu$cages),"CensoredAnimals")) && same(wc$cages,wu$cages) && same(wc$cage_pairs,wu$cage_pairs) &&
      identical(wc$cages$CensoredAnimals,ifelse(wc$cages$CageID=="B2|sys.2|CC4" & wc$cages$PhaseLabel %in% c("I4","I5"),2L,0L)),"cage tables count censored animals, values unchanged")
check(identical(grid31_whole_inactive_uncertainty(wc$cages,draws=200,seed=17),grid31_whole_inactive_uncertainty(wu$cages,draws=200,seed=17)),"estimates unchanged")
eligible <- bins; eligible$CensoredSec <- ifelse(off,300,0); eligible$CensorRecord <- "SX04"
fails(grid31_whole_inactive(eligible),"censored phase is eligible")
fails(grid31_whole_inactive(dplyr::select(cens,-CensorRecord)),"CensorRecord")
fails(grid31_whole_inactive(dplyr::mutate(cens,CensoredSec=ifelse(off,301,0))),"Invalid censored seconds")
fails(grid31_whole_inactive(dplyr::bind_rows(bins,bins[1,])),"Duplicate")
fails(grid31_whole_inactive(bins[-1,]),"144 unique")
fails(grid31_whole_inactive(bins[bins$PhaseLabel!="I2",]),"Missing I2-I5")
fails(grid31_whole_inactive_uncertainty(w$cages,draws=2),"Invalid bootstrap")
check(!any(grepl("p_value|p.value",names(u$summary))),"descriptive uncertainty never labelled as a hypothesis test")
cat("PASS: whole-inactive fixed windows, complete cohorts, equal batch/cage weights, paired reproducible bootstrap, zero vs missing, censoring labels, malformed-bin guards\n")
