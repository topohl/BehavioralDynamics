source("Analysis/31c_cc4_phase_statistics.R")
check<-function(x,msg) if(!isTRUE(x)) stop("FAIL: ",msg,call.=FALSE)
fails<-function(expr,pattern){e<-tryCatch({force(expr);NULL},error=identity);check(inherits(e,"error") && grepl(pattern,conditionMessage(e)),paste("expected",pattern))}
set.seed(821)
rows<-list()
for(sex in c("Female","Male")) for(batch in if(sex=="Female")c("B3","B4","B6") else c("B1","B2","B5")) for(cage in 1:5) {
  u<-rnorm(1,0,.4)
  for(animal in 1:4) {
    v<-rnorm(1,0,.25);g<-if(cage==1)"CON" else if(animal<=2)"RES" else "SUS"
    for(phase in c("Inactive","Active")) for(num in if(phase=="Active")1:5 else 2:5) {
      rows[[length(rows)+1L]]<-data.frame(Sex=sex,Phase=phase,Batch=batch,CageID=paste(batch,cage,sep="|"),
        AnimalKey=paste(batch,cage,animal,sep="|"),AnimalNum=paste0("00",substr(batch,2,2),cage,animal),Group=g,
        Condition=if(g=="CON")"CON" else "SIS",PhaseLabel=paste0(substr(phase,1,1),num),
        RecordedCrossings=rnbinom(1,mu=12*exp(2+u+v+.2*(g!="CON")+.4*(num>2)+.15*(num>2 & g!="CON")),size=25),NominalHours=12,EligiblePhase=TRUE)
    }
  }
}
a<-dplyr::bind_rows(rows);a$AnimalID<-a$AnimalNum;r<-grid31_stats_data(a)
check(nrow(r$animals)==120*4 && nrow(r$cages)==30*4,"two periods per animal/cage and phase")
check(sum(r$animals$Count)==sum(a$RecordedCrossings[a$PhaseLabel!="A1"]),"counts exactly conserved")
check(all(r$cages$Hours==ifelse(r$cages$Period=="Reference",48,144)),"animal-hour offset 12 vs36 hours")
bad<-a;bad$EligiblePhase[bad$PhaseLabel=="A1"]<-FALSE
check(identical(grid31_stats_data(bad),r),"A1 is not a primary eligibility criterion")
bad<-a;bad$EligiblePhase[bad$AnimalKey=="B3|2|1" & bad$PhaseLabel=="I5"]<-FALSE
lost<-grid31_stats_data(bad)
check(nrow(lost$animals)==nrow(r$animals)-8 && all(lost$roster$Included[lost$roster$Phase=="Active"]),"exclude whole cage only in affected family")
fails(grid31_stats_data(a[-1,]),"Missing")
fails(grid31_stats_data(rbind(a,a[1,])),"Duplicate")
bad<-a;bad$RecordedCrossings[1]<-.5;fails(grid31_stats_data(bad),"Invalid")
bad<-a;bad$Condition[1]<-"SIS";fails(grid31_stats_data(bad),"Invalid")
check(isTRUE(all.equal(r,grid31_stats_data(a[sample(nrow(a)),]))),"row-order invariance")
fails(run_cc4_statistics("missing","new",draws=100),"4999")
fails(run_cc4_statistics("missing","new",draws=5000,workers=0),"workers")
cat("PASS: CC4 statistical aggregation, identity, eligibility, offsets and fail-closed contracts\n")

# Explicit opt-in exercises fitted model, parallel bootstrap and actual driver;
# portable CI remains independent of the scientific fitting dependencies.
if(identical(Sys.getenv("MMM_TEST_CC4_MODELS"),"1")) {
  check(requireNamespace("glmmTMB",quietly=TRUE),"glmmTMB available for requested model integration test")
  d<-grid31_stats_factors(subset(r$cages,Sex=="Female" & Phase=="Inactive"))
  fit<-grid31_stats_fit(d);check(fit$diagnostic$Valid,"fixture full model fits")
  e<-grid31_stats_effect(fit$model,d)
  check(abs(exp(e['LogRRR'])-(e['SISPost']/e['SISReference'])/(e['CONPost']/e['CONReference']))<1e-8,"model RRR equals rate ratio contrast")
  before<-d
  sim<-simulate(fit$model,nsim=2,seed=998)
  one<-grid31_boot_one(1,sim,d,"full")
  check(identical(d,before) && nrow(one)==1 && 'LogRRR' %in% names(one),"refit does not mutate observed counts")
  check(isTRUE(all.equal(one,grid31_boot_one(1,simulate(fit$model,nsim=2,seed=998),d,"full"))),"seeded simulation and refitting reproduce")
  invalid<-fit$model;invalid$sdr$pdHess<-FALSE
  check(!grid31_fit_diagnostic(invalid)$Valid,"non-positive Hessian cannot pass the numerical gate")
  tmp<-tempfile("cc4stats_");dir.create(tmp);src<-file.path(tmp,"src");dir.create(src)
  readr::write_csv(a,file.path(src,"animal_phase_activity.csv"))
  readr::write_csv(data.frame(File="animal_phase_activity.csv",SHA256=mmm_file_sha256(file.path(src,"animal_phase_activity.csv"))),file.path(src,"manifest.csv"))
  actual<-run_cc4_statistics(src,file.path(tmp,"out"),draws=2,workers=2,seed=998,validation_only=TRUE)
  check(nrow(actual$primary_ratios)==4 && all(is.na(actual$primary_ratios$P)) && all(actual$primary_ratios$Status=="VALIDATION_ONLY_NO_INFERENCE"),"actual driver never publishes low-draw inference")
  invisible(grid31_verify_manifest(file.path(tmp,"out"),"manifest.csv"))
  fails(run_cc4_statistics(src,file.path(tmp,"out")),"already exists")
  cat("\n",file=file.path(src,"animal_phase_activity.csv"),append=TRUE)
  fails(run_cc4_statistics(src,file.path(tmp,"bad")),"hash mismatch")
  cat("PASS: fitted-model identity, parallel refits, actual statistical driver, manifest and overwrite guards\n")
}
