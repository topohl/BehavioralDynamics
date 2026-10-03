source("Analysis/31d_cc4_phenotype_statistics.R")
check<-function(x,msg)if(!isTRUE(x))stop("FAIL: ",msg,call.=FALSE)
fails<-function(expr,pattern){e<-tryCatch({force(expr);NULL},error=identity);check(inherits(e,"error")&&grepl(pattern,conditionMessage(e)),paste("expected",pattern))}
# Balanced, non-biological fixture; randomness affects counts only.
set.seed(672);rows<-list()
for(sex in c("Female","Male"))for(batch in if(sex=="Female")c("B3","B4","B6")else c("B1","B2","B5"))for(cage in 1:5){
  u<-rnorm(1,0,.35);post<-rnorm(1,0,.18)
  for(animal in 1:4){
    g<-if(cage==1)"CON" else if(animal<=2)"RES" else "SUS";v<-rnorm(1,0,.25)
    for(phase in c("Inactive","Active"))for(n in if(phase=="Active")1:5 else 2:5){
      id<-paste0("00",substr(batch,2,2),cage,animal)
      rows[[length(rows)+1L]]<-data.frame(Sex=sex,Phase=phase,Batch=batch,CageID=paste(batch,cage,sep="|"),
        AnimalKey=paste(batch,cage,animal,sep="|"),AnimalNum=id,AnimalID=id,Group=g,Condition=if(g=="CON")"CON"else"SIS",
        PhaseLabel=paste0(substr(phase,1,1),n),NominalHours=12,EligiblePhase=TRUE,
        RecordedCrossings=rnbinom(1,mu=12*exp(2+u+v+(n>2)*(.3+post+.1*(g=="SUS"))),size=30))
    }
  }
}
a<-dplyr::bind_rows(rows);input<-grid31_stats_data(a)
d<-grid31g_data(subset(input$animals,Sex=="Female"&Phase=="Inactive"))
fixed<-function(h)stats::as.formula(paste("~",switch(h,full="Group*Period+Batch*Period",omnibus="Group+Batch*Period",
  `RES-CON`="Group+Batch*Period+PostSUS",`SUS-CON`="Group+Batch*Period+PostRES",`SUS-RES`="Group+Batch*Period+PostSIS")))
X<-model.matrix(fixed("full"),d)
for(h in c("omnibus","RES-CON","SUS-CON","SUS-RES")) {
  N<-model.matrix(fixed(h),d)
  check(qr(X)$rank-qr(N)$rank==if(h=="omnibus")2L else 1L,paste("correct nested restriction",h))
  check(max(abs(qr.resid(qr(X),N)))<1e-10,paste("null is nested",h))
}
# Nulls keep group baseline differences and let the third group change freely.
check(all(d$PostSUS==d$Post*(d$Group=="SUS"))&&all(d$PostSIS==d$Post*(d$Group!="CON")),"pairwise null indicators")
fails(grid31g_formula("bad"),"Unknown")
fails(run_cc4_phenotype_statistics("missing","new",draws=100),"4999")
context<-subset(input$animals,Sex=="Female"&Phase=="Inactive")
check(isTRUE(all.equal(grid31g_data(context),grid31g_data(context[sample(nrow(context)),]))),"stable row ordering within a model context")
cat("PASS: phenotype fixed design, nested omnibus/pairwise nulls and contracts\n")

if(identical(Sys.getenv("MMM_TEST_CC4_MODELS"),"1")) {
  f<-grid31g_fit(d);check(f$diagnostic$Valid,"fixture full model fits")
  e<-grid31g_contrasts(f$model);check(abs(e['SUS-RES']-(e['SUS-CON']-e['RES-CON']))<1e-12,"contrast identity")
  null<-grid31g_fit(d,"SUS-RES");check(null$diagnostic$Valid,"constrained fit valid")
  check(f$diagnostic$LogLik>=null$diagnostic$LogLik-5e-6,"nested likelihood ordering")
  sim<-simulate(f$model,nsim=2,seed=51);one<-grid31g_boot_one(1,sim,d,"full")
  check(isTRUE(all.equal(one,grid31g_boot_one(1,simulate(f$model,nsim=2,seed=51),d,"full"))),"deterministic refitting")
  tmp<-tempfile("cc4phenotype_");dir.create(tmp);src<-file.path(tmp,"source");dir.create(src)
  readr::write_csv(a,file.path(src,"animal_phase_activity.csv"))
  readr::write_csv(data.frame(File="animal_phase_activity.csv",SHA256=mmm_file_sha256(file.path(src,"animal_phase_activity.csv"))),file.path(src,"manifest.csv"))
  z<-run_cc4_phenotype_statistics(src,file.path(tmp,"out"),draws=2,workers=2,seed=51,validation_only=TRUE)
  check(nrow(z$omnibus)==4&&nrow(z$contrasts)==12,"four omnibus and twelve planned contrasts")
  check(all(is.na(z$omnibus$P))&&all(is.na(z$contrasts$P))&&all(is.na(z$contrasts$Lower95)),"validation cannot publish inference")
  invisible(grid31_verify_manifest(file.path(tmp,"out"),"manifest.csv"))
  fails(run_cc4_phenotype_statistics(src,file.path(tmp,"out")),"already exists")
  cat("\n",file=file.path(src,"animal_phase_activity.csv"),append=TRUE)
  fails(run_cc4_phenotype_statistics(src,file.path(tmp,"bad")),"hash mismatch")
  cat("PASS: phenotype models, deterministic parallel refitting, driver, hashes and overwrite guard\n")
}
