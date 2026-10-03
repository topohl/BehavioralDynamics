source("Analysis/31b_cc4_phase_groups.R")
check <- function(x, msg) if (!isTRUE(x)) stop("FAIL: ", msg, call. = FALSE)
fails <- function(expr, pattern) {
  err <- tryCatch({force(expr); NULL}, error = identity)
  check(inherits(err, "error") && grepl(pattern, conditionMessage(err)), paste("expected", pattern))
}
rows <- list(); labels <- list()
for (b in c(1L, 2L, 5L)) for (c in 1:3) {
  groups <- if (c == 1) rep("CON", 4) else if (c == 2) c("RES", "SUS") else c("RES", "RES", "SUS")
  for (a in seq_along(groups)) {
    id <- sprintf("%05d", 100*b+10*c+a); g <- groups[a]
    labels[[length(labels)+1L]] <- tibble::tibble(AnimalNum=id, Sex="Male", Batch=as.character(b),
      outcome_group=g, experimental_condition=ifelse(g=="CON","CON","SIS"))
    for (family in c("Inactive", "Active")) for (ph in if (family=="Inactive") 2:5 else 1:5) {
      st <- as.POSIXct("2023-01-01 06:30:00", tz="UTC")+(ph-1)*86400+ifelse(family=="Active",43200,0)
      rate <- 100+10*b+c+a+(ph-2)*(match(g,c("CON","RES","SUS"))*b+c)*ifelse(family=="Active",2,1)
      rows[[length(rows)+1L]] <- tibble::tibble(Batch=paste0("B",b), CageChange="CC4", System=paste0("sys.",c),
        AnimalID=id, AnimalNum=id, Sex="Male", Phase=family, PhaseLabel=paste0(substr(family,1,1),ph), PhaseNumber=ph,
        BlockStart=st, BlockEnd=st+43200, BinIndex=0:143, Crossings=c(12*rate,rep(0,143)),
        SessionSpansWholeBlock=TRUE, AnimalSpansWholeBlock=TRUE, SpanOverlapSec=300)
    }
  }
}
bins <- dplyr::bind_rows(rows); outcomes <- dplyr::bind_rows(labels)
r <- grid31_group_phases(bins,outcomes)
check(nrow(r$animals)==27*9 && all(r$animals$IncludedInPrimary),"common nine-phase roster")
z <- subset(r$contrasts, PhaseLabel=="I3" & Contrast=="RES-CON" & Measure=="ChangeDifference")
expected <- mean(c(1,2,5))+1.5
check(abs(z$Estimate-expected)<1e-12,"equal cage-group means and equal batches, paired change contrast")
margin <- qt(.975,2)*sd(c(1,2,5))/sqrt(3)
check(abs(z$Lower95-(expected-margin))<1e-12 && z$DF==2,"batch t interval has three independent units")
check(all(r$batch_contrasts$ChangeDifference[r$batch_contrasts$PhaseLabel=="I2"]==0),"paired reference is zero")
ix <- subset(r$contrasts,PhaseLabel=="A3" & Contrast=="RES-CON" & Measure=="ChangeDifference")
check(abs(ix$Estimate-2*z$Estimate)<1e-12 && abs(ix$Lower95-2*z$Lower95)<1e-12,"active reference A2 and paired interval")
bad <- outcomes;bad$outcome_group[1]<-NA_character_;fails(grid31_group_phases(bins,bad),"Invalid canonical")
fails(grid31_group_phases(bins,rbind(outcomes,outcomes[1,])),"Duplicate")
fails(grid31_group_phases(bins,outcomes[-1,]),"Missing or inconsistent")
bad <- outcomes;bad$Sex[1]<-"Female";fails(grid31_group_phases(bins,bad),"inconsistent")
bad <- bins;bad$System[bad$Phase=="Active" & bad$AnimalNum=="00121"]<-"sys.9"
fails(grid31_group_phases(bad,outcomes),"roster/cage differs")
bad <- bins;bad$AnimalSpansWholeBlock[bad$AnimalNum=="00121" & bad$PhaseLabel=="A5"]<-FALSE
lost <- grid31_group_phases(bad,outcomes)
check(all(!lost$animals$IncludedInPrimary[lost$animals$CageID=="B1|sys.2|CC4"]),"lost A5 excludes full cage on all nine phases")
check(all(lost$animals$IncludedInFamily[lost$animals$CageID=="B1|sys.2|CC4" & lost$animals$Phase=="Inactive"]),"family coverage retained separately")
bad <- bins;bad$AnimalSpansWholeBlock[bad$Batch=="B1" & bad$System=="sys.1"]<-FALSE
fails(grid31_group_phases(bad,outcomes),"Missing complete group")
bad <- bins;bad$PhaseLabel[1]<-"I5";fails(grid31_group_phases(bad,outcomes),"Duplicate|144 unique|mismatch")
two <- r$batch[r$batch$Batch!="B5",] %>% tidyr::pivot_longer(c("Rate","Change"),names_to="Measure",values_to="Value")
check(all(is.na(grid31_batch_intervals(two,"Group")$Lower95)),"two batches are explicitly not estimable")
set.seed(13); shuffled <- grid31_group_phases(bins[sample(nrow(bins)),],outcomes[nrow(outcomes):1,])
check(isTRUE(all.equal(r$summary,shuffled$summary)),"input row-order invariant")
# Exercise the actual driver, input verification, output manifest and overwrite guards.
tmp <- tempfile("cc4_groups_");dir.create(tmp);dir.create(file.path(tmp,"source"));dir.create(file.path(tmp,"source","tables"));dir.create(file.path(tmp,"source","audit"))
readr::write_csv(bins,file.path(tmp,"source","tables","animal_clock_profiles_5min.csv"))
readr::write_csv(tibble::tibble(File="tables/animal_clock_profiles_5min.csv",SHA256=mmm_file_sha256(file.path(tmp,"source","tables","animal_clock_profiles_5min.csv"))),file.path(tmp,"source","audit","output_manifest.csv"))
readr::write_csv(outcomes,file.path(tmp,"outcomes.csv"))
actual <- run_cc4_phase_groups(file.path(tmp,"source"),file.path(tmp,"outcomes.csv"),file.path(tmp,"out"))
check(isTRUE(all.equal(r$summary,actual$summary)),"actual driver preserves leading-zero identities and results")
grid31_verify_manifest(file.path(tmp,"out"),"manifest.csv")
fails(run_cc4_phase_groups(file.path(tmp,"source"),file.path(tmp,"outcomes.csv"),file.path(tmp,"out")),"already exists")
cat("\n",file=file.path(tmp,"source","tables","animal_clock_profiles_5min.csv"),append=TRUE)
fails(run_cc4_phase_groups(file.path(tmp,"source"),file.path(tmp,"outcomes.csv"),file.path(tmp,"bad")),"hash mismatch")
cat("PASS: CC4 group identities, active/inactive windows, cage and batch weighting, paired contrasts, t intervals, missingness, actual driver and manifests\n")
