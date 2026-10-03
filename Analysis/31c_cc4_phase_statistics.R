# Explicit upstream inferential extension; new output directory required.
source("Analysis/_pipeline_setup.R")
for(helper in c("project_paths.R","cc4_grid_exposure_helpers.R","cc4_phase_group_helpers.R","cc4_phase_statistics_helpers.R")) source_mmm_helper(helper)

run_cc4_statistics <- function(source_dir,output_dir,draws=4999L,workers=8L,seed=20260929L,validation_only=FALSE) {
  if(file.exists(output_dir)) stop("Output already exists; choose a new run ID.",call.=FALSE)
  if(length(draws)!=1L || !is.finite(draws) || draws!=floor(draws) || draws<1 || (!validation_only && draws<4999)) stop("Final inference requires at least 4999 successful draws.",call.=FALSE)
  if(length(workers)!=1L || !is.finite(workers) || workers<1 || workers!=floor(workers) ||
     length(seed)!=1L || !is.finite(seed) || seed<1 || seed>2e9) stop("Invalid workers/seed.",call.=FALSE)
  if(!requireNamespace("glmmTMB",quietly=TRUE)) stop("glmmTMB is required.",call.=FALSE)
  source_manifest<-grid31_verify_manifest(source_dir,"manifest.csv")
  if(!"animal_phase_activity.csv" %in% source_manifest$File) stop("Animal phases are not manifested.",call.=FALSE)
  input_paths<-c(file.path(source_dir,c("manifest.csv",source_manifest$File)),file.path(MMM_REPO_ROOT,c(
    "Analysis/31c_cc4_phase_statistics.R","Functions/cc4_phase_statistics_helpers.R","Functions/cc4_phase_group_helpers.R",
    "Functions/cc4_grid_exposure_helpers.R","Functions/project_paths.R","Functions/behavioral_dynamics_helpers.R","Analysis/_pipeline_setup.R")))
  hashes<-mmm_file_sha256(input_paths)
  a<-readr::read_csv(file.path(source_dir,"animal_phase_activity.csv"),col_types=readr::cols(.default=readr::col_guess(),AnimalNum=readr::col_character(),AnimalID=readr::col_character()),show_col_types=FALSE)
  if(nrow(readr::problems(a))) stop("Phase parse failure.",call.=FALSE)
  inputs<-grid31_stats_data(a)
  dir.create(output_dir,recursive=TRUE);dir.create(file.path(output_dir,"bootstrap"));dir.create(file.path(output_dir,"models"))
  for(nm in names(inputs)) readr::write_csv(inputs[[nm]],file.path(output_dir,paste0(nm,"_model_input.csv")),na="")
  writeLines(c("CC4 Stage 31c protocol, fixed before bootstrap execution.",
    "All conditions received grid. Whole-phase associations; no causal/timing-specific grid inference.",
    "I2 vs I3-I5 and A2 vs A3-A5; 12h vs36h; complete cage roster per phase family, A1 not required.",
    "Primary cage NB2: Condition*Period + Batch*Period + offset(log(animal-hours)) + (1|CageID).",
    "Secondary animal NB2: Group*Period + Batch*Period + offset(log(hours)) + (1|AnimalKey)+(1|CageID)+(0+Post|CageID).",
    "Numerical gate: convergence0, positive Hessian, finite likelihood/coefficients, max gradient<=0.01. BFGS retry only on failed fits.",
    "Boundary flag: any random-effect SD<0.0001. Boundary observed models withheld; boundary refits accepted only if numerical gate passes.",
    "Secondary family withheld if any of four observed models is boundary/nonvalid; no silent simplification or Wald substitution.",
    "Primary null-model simulation LRT with fresh random effects; full-model simulation percentile CI; first requested successful draws.",
    "Fixed maximum attempts ceiling(draws/0.94); stop if cumulative refit failures exceed5% at200-draw checkpoints.",
    "P=(1+number simulated LR>=observed LR)/(1+successful draws); Holm across four primary tests.",
    "Model rates integrate cage variance and weight batches equally. Intervals pointwise, not simultaneous.",
    "Secondary tests remain withheld in this implementation pending assessment of all four observed covariance fits.",
    "Sensitivity fits: leave-one-batch-out; NB2 dispersion by Condition+Period; RES/SUS mixed cages only. No sensitivity p-values.",
    paste("Successful draws:",draws,"; base seed:",seed,"; validation_only:",validation_only),
    "Plan adopted after descriptive inspection; not preregistered. No randomization assumption verified."),file.path(output_dir,"protocol.txt"))
  capture.output(sessionInfo(),file=file.path(output_dir,"session_info.txt"))
  fits<-diagnostics<-secondary<-sensitivity<-predictive<-effects<-boot_status<-list()
  contexts<-expand.grid(Sex=c("Female","Male"),Phase=c("Inactive","Active"),stringsAsFactors=FALSE)
  cluster<-if(workers>1) parallel::makePSOCKcluster(workers) else NULL
  if(!is.null(cluster)) {
    on.exit(parallel::stopCluster(cluster),add=TRUE)
    parallel::clusterExport(cluster,c("grid31_stats_formula","grid31_fit_diagnostic","grid31_stats_fit","grid31_stats_effect","grid31_boot_one"),envir=environment(grid31_stats_fit))
    parallel::clusterEvalQ(cluster,{options(warn=1);NULL})
  }
  for(i in seq_len(nrow(contexts))) {
    sex<-contexts$Sex[i];ph<-contexts$Phase[i];id<-paste(sex,ph,sep="_")
    d<-grid31_stats_factors(subset(inputs$cages,Sex==sex & Phase==ph))
    ad<-grid31_stats_factors(subset(inputs$animals,Sex==sex & Phase==ph))
    if(length(levels(d$Batch))!=3L) stop("Expected three batches per sex.",call.=FALSE)
    full<-grid31_stats_fit(d);null<-grid31_stats_fit(d,null=TRUE);sec<-grid31_stats_fit(ad,"secondary")
    fits[[id]]<-list(full=full,null=null,secondary=sec)
    for(nm in c("full","null","secondary")) diagnostics[[paste(id,nm)]]<-cbind(contexts[i,],Model=nm,fits[[id]][[nm]]$diagnostic)
    saveRDS(fits[[id]],file.path(output_dir,"models",paste0(id,".rds")))
    if(!is.null(sec$model)) {
      b<-glmmTMB::fixef(sec$model)$cond
      v<-c(b["GroupRES:PeriodPost"],b["GroupSUS:PeriodPost"],b["GroupSUS:PeriodPost"]-b["GroupRES:PeriodPost"])
      secondary[[id]]<-data.frame(Sex=sex,Phase=ph,Contrast=c("RES-CON","SUS-CON","SUS-RES"),RRR=exp(unname(v)),Lower95=NA_real_,Upper95=NA_real_,P=NA_real_,HolmP=NA_real_,Status="WITHHELD_SECONDARY_COVARIANCE_ASSESSMENT",Boundary=sec$diagnostic$Boundary)
    }
    for(batch in levels(d$Batch)) {
      ld<-grid31_stats_factors(d[d$Batch!=batch,]);ld$Batch<-droplevels(ld$Batch)
      s<-grid31_stats_fit(ld)
      sensitivity[[paste(id,batch)]]<-data.frame(Sex=sex,Phase=ph,Analysis=paste0("leave_out_",batch),
        RRR=if(s$diagnostic$Valid)exp(glmmTMB::fixef(s$model)$cond["ConditionSIS:PeriodPost"]) else NA_real_,s$diagnostic)
    }
    s<-grid31_stats_fit(d,dispersion=~Condition+Period)
    sensitivity[[paste(id,"dispersion")]]<-data.frame(Sex=sex,Phase=ph,Analysis="dispersion_by_condition_and_period",
      RRR=if(s$diagnostic$Valid)exp(glmmTMB::fixef(s$model)$cond["ConditionSIS:PeriodPost"]) else NA_real_,s$diagnostic)
    mixed<-ad %>% dplyr::group_by(CageID) %>% dplyr::filter(all(c("RES","SUS") %in% as.character(Group))) %>% dplyr::ungroup()
    if(nrow(mixed)) {
      mixed<-grid31_stats_factors(mixed);mixed$Group<-droplevels(mixed$Group)
      sm<-grid31_stats_fit(mixed,"secondary")
      sensitivity[[paste(id,"mixed")]]<-data.frame(Sex=sex,Phase=ph,Analysis="mixed_cages_SUS_RES",
        RRR=if(sm$diagnostic$Valid)exp(glmmTMB::fixef(sm$model)$cond["GroupSUS:PeriodPost"]) else NA_real_,sm$diagnostic)
    }
    if(full$diagnostic$Valid) predictive[[id]]<-data.frame(Sex=sex,Phase=ph,grid31_sim_checks(full$model,d,seed+1000L*i,if(validation_only)19L else 999L),row.names=NULL)
    observed_ok<-full$diagnostic$Valid && null$diagnostic$Valid && !full$diagnostic$Boundary && !null$diagnostic$Boundary &&
      full$diagnostic$LogLik >= null$diagnostic$LogLik-5e-6
    est<-if(full$diagnostic$Valid)grid31_stats_effect(full$model,d) else setNames(rep(NA_real_,8),c("LogRRR","CONReference","CONPost","SISReference","SISPost","CONChange","SISChange","ChangeDifference"))
    status<-"WITHHELD_OBSERVED_MODEL_GATE";p<-mcse<-lr<-NA_real_;lo<-hi<-rep(NA_real_,8)
    if(observed_ok) {
      lr<-max(0,2*(full$diagnostic$LogLik-null$diagnostic$LogLik))
      bn<-grid31_bootstrap(null$model,d,"null",draws,seed+1000L*i+1L,cluster,file.path(output_dir,"bootstrap",paste0(id,"_null.csv")))
      boot_status[[paste(id,"null")]]<-cbind(contexts[i,],bn$status)
      bf<-grid31_bootstrap(full$model,d,"full",draws,seed+1000L*i+2L,cluster,file.path(output_dir,"bootstrap",paste0(id,"full.csv")))
      boot_status[[paste(id,"full")]]<-cbind(contexts[i,],bf$status)
      status<-if(bn$status$Status=="PASS" && bf$status$Status=="PASS") "PASS" else "WITHHELD_BOOTSTRAP_GATE"
      if(status=="PASS" && !validation_only) {
        nd<-bn$draws$LR[bn$draws$Used];p<-(1+sum(nd>=lr))/(length(nd)+1);mcse<-sqrt(p*(1-p)/length(nd))
        fd<-bf$draws[bf$draws$Used,names(est)];lo<-vapply(fd,quantile,numeric(1),probs=.025);hi<-vapply(fd,quantile,numeric(1),probs=.975)
      }
    }
    if(validation_only) status<-"VALIDATION_ONLY_NO_INFERENCE"
    effects[[id]]<-data.frame(Sex=sex,Phase=ph,Measure=names(est),Estimate=unname(est),Lower95=lo,Upper95=hi,
      P=ifelse(names(est)=="LogRRR",p,NA_real_),MCSE=ifelse(names(est)=="LogRRR",mcse,NA_real_),LR=lr,Status=status,
      Cages=dplyr::n_distinct(d$CageID),CONCages=dplyr::n_distinct(d$CageID[d$Condition=="CON"]),Animals=sum(d$Animals[d$Period=="Reference"]))
    cat("Completed ",id,": ",status,"\n");flush.console()
  }
  result<-dplyr::bind_rows(effects);result$HolmP<-NA_real_
  ix<-which(result$Measure=="LogRRR" & !is.na(result$P));result$HolmP[ix]<-p.adjust(result$P[ix],method="holm",n=4L)
  ratio<-result[result$Measure=="LogRRR",];ratio$Measure<-"SIS_CON_ratio_of_rate_ratios"
  ratio[c("Estimate","Lower95","Upper95")]<-lapply(ratio[c("Estimate","Lower95","Upper95")],exp)
  diag_table<-dplyr::bind_rows(diagnostics);secondary_table<-dplyr::bind_rows(secondary)
  secondary_bad<-with(subset(diag_table,Model=="secondary"),any(!Valid | Boundary))
  secondary_table$Status<-if(secondary_bad) "WITHHELD_FAMILY_HAS_BOUNDARY_OR_INVALID_COVARIANCE" else "WITHHELD_SECONDARY_BOOTSTRAP_NOT_IMPLEMENTED"
  tables<-list(primary_effects=result,primary_ratios=ratio,secondary_ratios=secondary_table,
    fit_diagnostics=diag_table,sensitivity_estimates=dplyr::bind_rows(sensitivity),
    predictive_checks=dplyr::bind_rows(predictive),bootstrap_status=dplyr::bind_rows(boot_status),
    input_manifest=data.frame(Path=input_paths,SHA256=hashes))
  for(nm in names(tables)) readr::write_csv(tables[[nm]],file.path(output_dir,paste0(nm,".csv")),na="")
  if(!identical(hashes,mmm_file_sha256(input_paths))) stop("Source changed during statistical run.",call.=FALSE)
  capture.output(sessionInfo(),file=file.path(output_dir,"session_info.txt"))
  files<-list.files(output_dir,recursive=TRUE,full.names=TRUE)
  readr::write_csv(data.frame(File=substring(files,nchar(output_dir)+2L),SHA256=mmm_file_sha256(files)),file.path(output_dir,"manifest.csv"))
  invisible(tables)
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  if(!length(args) %in% c(2L,4L)) stop("Usage: Rscript Analysis/31c_cc4_phase_statistics.R <Stage31b_run> <new_output_dir> [draws workers]",call.=FALSE)
  run_cc4_statistics(args[1],args[2],if(length(args)==4L)as.integer(args[3]) else 4999L,if(length(args)==4L)as.integer(args[4]) else 8L)
}
