# Explicit CC4 phenotype module. Historical Stage31c results are immutable.
source("Analysis/31c_cc4_phase_statistics.R")
source_mmm_helper("cc4_phenotype_statistics_helpers.R")

run_cc4_phenotype_statistics <- function(source_dir,output_dir,draws=4999L,workers=24L,seed=20260930L,validation_only=FALSE) {
  if(file.exists(output_dir))stop("Output already exists; choose a new run ID.",call.=FALSE)
  if(length(draws)!=1L || !is.finite(draws) || draws!=floor(draws) || draws<1 || (!validation_only && draws<4999))stop("Final inference requires at least 4999 successful draws.",call.=FALSE)
  if(length(workers)!=1L || !is.finite(workers) || workers<1 || workers!=floor(workers) ||
    length(seed)!=1L || !is.finite(seed) || seed!=floor(seed) || seed<1 || seed>2e9)stop("Invalid workers/seed.",call.=FALSE)
  if(!requireNamespace("glmmTMB",quietly=TRUE))stop("glmmTMB required.",call.=FALSE)
  manifest<-grid31_verify_manifest(source_dir,"manifest.csv")
  if(!"animal_phase_activity.csv" %in% manifest$File)stop("Animal-phase table not manifested.",call.=FALSE)
  paths<-c(file.path(source_dir,c("manifest.csv",manifest$File)),file.path(MMM_REPO_ROOT,c(
    "Analysis/31d_cc4_phenotype_statistics.R","Functions/cc4_phenotype_statistics_helpers.R",
    "Analysis/31c_cc4_phase_statistics.R","Functions/cc4_phase_statistics_helpers.R",
    "Functions/cc4_phase_group_helpers.R","Functions/cc4_grid_exposure_helpers.R",
    "Functions/project_paths.R","Functions/behavioral_dynamics_helpers.R","Analysis/_pipeline_setup.R")))
  before<-mmm_file_sha256(paths)
  a<-readr::read_csv(file.path(source_dir,"animal_phase_activity.csv"),col_types=readr::cols(.default=readr::col_guess(),
    AnimalNum=readr::col_character(),AnimalID=readr::col_character()),show_col_types=FALSE)
  if(nrow(readr::problems(a)))stop("Phase parse failure.",call.=FALSE)
  inputs<-grid31_stats_data(a)
  expected<-tidyr::crossing(inputs$animals %>% dplyr::distinct(Sex,Phase,Batch),Group=c("CON","RES","SUS"))
  if(nrow(dplyr::anti_join(expected,inputs$animals,by=c("Sex","Phase","Batch","Group"))))stop("Missing phenotype within batch.",call.=FALSE)
  dir.create(output_dir,recursive=TRUE);dir.create(file.path(output_dir,"bootstrap"));dir.create(file.path(output_dir,"models"))
  readr::write_csv(inputs$animals,file.path(output_dir,"animal_period_input.csv"))
  readr::write_csv(inputs$roster,file.path(output_dir,"cage_roster.csv"))
  population<-inputs$animals %>% dplyr::group_by(Sex,Phase,Group) %>%
    dplyr::summarise(Animals=dplyr::n_distinct(AnimalKey),Cages=dplyr::n_distinct(CageID),Batches=dplyr::n_distinct(Batch),.groups="drop")
  readr::write_csv(population,file.path(output_dir,"population.csv"))
  writeLines(c("Stage31d: CON/RES/SUS phenotype-associated CC4 whole-phase response; exploratory plan revised after inspecting descriptive and condition results.",
    "I2 vs pooled I3-I5; A2 vs pooled A3-A5; fixed complete cage roster per family. A1 does not determine eligibility.",
    "Full animal NB2: Group*Period+Batch*Period+offset(log(Hours))+(1|AnimalKey)+(1|CageID)+(0+Post|CageID).",
    "Four omnibus nulls remove both Group:Period coefficients. Four tests adjusted by Holm.",
    "Twelve planned pairwise nulls each constrain exactly one difference of Group:Period effects; retain all animals and the third group's free response. Holm across12.",
    "All12 contrasts are tested regardless of omnibus significance. No significance-driven contrast selection.",
    "Nested null bootstrap LR: simulate fresh animal/cage/post random effects and NB counts under each null; refit full/null with identical random structure.",
    "One full-model bootstrap per context supplies shared draws for percentile95% contrast intervals (pointwise, not simultaneous).",
    "Numerical gate: convergence0, positive Hessian, finite LL/beta, max absolute gradient<=0.01; BFGS retry only on failed initial fit.",
    "Random SD<1e-4 is an audited boundary flag, NOT an automatic inference exclusion. No random terms are removed/fixed to zero.",
    "More than5% failed refits at a240-attempt checkpoint withholds that procedure; max attempts ceiling(draws/0.94). First requested successes used; all attempts saved.",
    "P=(1+LR exceedances)/(1+successful draws). Monte Carlo SE exported. Null tests and full intervals have separate procedure status.",
    "Sensitivity: leave one batch out; dispersion~Group+Period; mixed-cage SUS/RES. Sensitivity estimates only, no extra P values.",
    "Unknown grid times; CON also received grid. Not a causal grid effect, prospective prediction, sleep assay, or test of adaptation across sessions.",
    "RES/SUS labels are fixed canonical outcomes; sucrose preference contributes to classification. No reclassification or independent validation claim.",
    paste("Draws",draws,"Workers",workers,"Seed",seed,"ValidationOnly",validation_only)),file.path(output_dir,"protocol.txt"))
  cl<-if(workers>1)parallel::makePSOCKcluster(workers) else NULL
  if(!is.null(cl)) {
    on.exit(parallel::stopCluster(cl),add=TRUE)
    parallel::clusterExport(cl,c("grid31g_formula","grid31_fit_diagnostic","grid31g_fit","grid31g_contrasts","grid31g_boot_one"),envir=environment(grid31g_fit))
  }
  contexts<-expand.grid(Sex=c("Female","Male"),Phase=c("Inactive","Active"),stringsAsFactors=FALSE)
  omnibus<-contrasts<-diagnostics<-boots<-predictive<-sensitivity<-list()
  hypotheses<-c("omnibus","RES-CON","SUS-CON","SUS-RES","full")
  for(i in seq_len(nrow(contexts))) {
    sex<-contexts$Sex[i];phase<-contexts$Phase[i];id<-paste(sex,phase,sep="_")
    d<-grid31g_data(subset(inputs$animals,Sex==sex & Phase==phase))
    if(nlevels(d$Batch)!=3L)stop("Expected three batches per sex.",call.=FALSE)
    models<-lapply(c("full",hypotheses[1:4]),function(h)grid31g_fit(d,h));names(models)<-c("full",hypotheses[1:4])
    saveRDS(models,file.path(output_dir,"models",paste0(id,".rds")))
    for(h in names(models))diagnostics[[paste(id,h)]]<-data.frame(Sex=sex,Phase=phase,Hypothesis=h,models[[h]]$diagnostic)
    full<-models$full
    if(full$diagnostic$Valid)predictive[[id]]<-data.frame(Sex=sex,Phase=phase,grid31g_predictive(full$model,d,seed+i*1000L,if(validation_only)19L else 999L),row.names=NULL)
    effects<-if(full$diagnostic$Valid)grid31g_contrasts(full$model) else setNames(rep(NA_real_,3),hypotheses[2:4])
    lo<-hi<-setNames(rep(NA_real_,3),names(effects));p<-mcse<-lr<-setNames(rep(NA_real_,4),hypotheses[1:4])
    status<-setNames(rep("WITHHELD_OBSERVED_NUMERICAL_FAILURE",5),hypotheses)
    for(j in seq_along(hypotheses)) {
      h<-hypotheses[j];m<-models[[h]]
      acceptable<-full$diagnostic$Valid && m$diagnostic$Valid && full$diagnostic$LogLik>=m$diagnostic$LogLik-5e-6
      if(!acceptable)next
      b<-grid31g_bootstrap(m$model,d,h,draws,seed+i*1000L+j,cl,file.path(output_dir,"bootstrap",paste0(id,"_",h,".csv")))
      boots[[paste(id,h)]]<-data.frame(Sex=sex,Phase=phase,b$status)
      status[h]<-b$status$Status
      if(status[h]=="PASS" && !validation_only) {
        used<-b$draws[b$draws$Used,,drop=FALSE]
        if(h=="full") {lo<-vapply(used[names(effects)],quantile,numeric(1),probs=.025);hi<-vapply(used[names(effects)],quantile,numeric(1),probs=.975)}
        else {lr[h]<-max(0,2*(full$diagnostic$LogLik-m$diagnostic$LogLik));p[h]<-(1+sum(used$LR>=lr[h]))/(nrow(used)+1);mcse[h]<-sqrt(p[h]*(1-p[h])/nrow(used))}
      }
    }
    if(validation_only)status[]<-"VALIDATION_ONLY_NO_INFERENCE"
    omnibus[[id]]<-data.frame(Sex=sex,Phase=phase,LR=unname(lr["omnibus"]),DF=2L,P=unname(p["omnibus"]),MCSE=unname(mcse["omnibus"]),Status=unname(status["omnibus"]))
    contrasts[[id]]<-data.frame(Sex=sex,Phase=phase,Contrast=names(effects),LogRRR=unname(effects),Estimate=exp(unname(effects)),
      Lower95=exp(unname(lo)),Upper95=exp(unname(hi)),P=unname(p[names(effects)]),MCSE=unname(mcse[names(effects)]),
      LR=unname(lr[names(effects)]),DF=1L,TestStatus=unname(status[names(effects)]),IntervalStatus=unname(status["full"]))
    for(batch in levels(d$Batch)) {
      z<-grid31g_data(d[d$Batch!=batch,]);fit<-grid31g_fit(z)
      v<-if(fit$diagnostic$Valid)grid31g_contrasts(fit$model) else setNames(rep(NA_real_,3),names(effects))
      sensitivity[[paste(id,batch)]]<-data.frame(Sex=sex,Phase=phase,Analysis=paste0("leave_out_",batch),Contrast=names(v),Estimate=exp(unname(v)),Valid=fit$diagnostic$Valid,Boundary=fit$diagnostic$Boundary)
    }
    fit<-grid31g_fit(d,dispersion=~Group+Period)
    v<-if(fit$diagnostic$Valid)grid31g_contrasts(fit$model) else setNames(rep(NA_real_,3),names(effects))
    sensitivity[[paste(id,"dispersion")]]<-data.frame(Sex=sex,Phase=phase,Analysis="dispersion_Group_Period",Contrast=names(v),Estimate=exp(unname(v)),Valid=fit$diagnostic$Valid,Boundary=fit$diagnostic$Boundary)
    mixed<-d %>% dplyr::group_by(CageID) %>% dplyr::filter(all(c("RES","SUS") %in% as.character(Group))) %>% dplyr::ungroup()
    if(nrow(mixed)) {
      mixed$Group<-droplevels(mixed$Group);fit<-grid31g_fit(mixed)
      sensitivity[[paste(id,"mixed")]]<-data.frame(Sex=sex,Phase=phase,Analysis="mixed_cages_only",Contrast="SUS-RES",
        Estimate=if(fit$diagnostic$Valid)exp(unname(glmmTMB::fixef(fit$model)$cond["GroupSUS:PeriodPost"])) else NA_real_,Valid=fit$diagnostic$Valid,Boundary=fit$diagnostic$Boundary)
    }
    cat("Finished",id,"\n");flush.console()
  }
  om<-dplyr::bind_rows(omnibus);co<-dplyr::bind_rows(contrasts)
  om$HolmP<-p.adjust(om$P,"holm",n=4L);co$HolmP<-p.adjust(co$P,"holm",n=12L)
  tables<-list(omnibus=om,contrasts=co,fit_diagnostics=dplyr::bind_rows(diagnostics),bootstrap_status=dplyr::bind_rows(boots),
    predictive_checks=dplyr::bind_rows(predictive),sensitivity_estimates=dplyr::bind_rows(sensitivity),input_manifest=data.frame(Path=paths,SHA256=before))
  for(n in names(tables))readr::write_csv(tables[[n]],file.path(output_dir,paste0(n,".csv")),na="")
  if(!identical(before,mmm_file_sha256(paths)))stop("Statistical inputs changed during execution.",call.=FALSE)
  capture.output(sessionInfo(),file=file.path(output_dir,"session_info.txt"))
  files<-list.files(output_dir,recursive=TRUE,full.names=TRUE)
  readr::write_csv(data.frame(File=substring(files,nchar(output_dir)+2L),SHA256=mmm_file_sha256(files)),file.path(output_dir,"manifest.csv"))
  invisible(tables)
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  if(!length(args)%in%c(2L,4L))stop("Usage: Rscript Analysis/31d_cc4_phenotype_statistics.R <Stage31b_run> <new_output> [draws workers]",call.=FALSE)
  run_cc4_phenotype_statistics(args[1],args[2],if(length(args)==4L)as.integer(args[3]) else 4999L,if(length(args)==4L)as.integer(args[4]) else 24L)
}
