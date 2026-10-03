# Stage31d: observed phenotype associations, not prospective prediction.
# Boundary variance alone is a flag; numerical/refit failures still gate inference.

grid31g_data <- function(a) {
  d <- grid31_stats_factors(a)
  d$PostRES <- d$Post*(d$Group=="RES")
  d$PostSUS <- d$Post*(d$Group=="SUS")
  d$PostSIS <- d$Post*(d$Group!="CON")
  d
}

grid31g_formula <- function(hypothesis="full") {
  fixed <- switch(hypothesis,
    full="Group*Period+Batch*Period",
    omnibus="Group+Batch*Period",
    `RES-CON`="Group+Batch*Period+PostSUS",
    `SUS-CON`="Group+Batch*Period+PostRES",
    `SUS-RES`="Group+Batch*Period+PostSIS",
    stop("Unknown phenotype hypothesis.",call.=FALSE))
  stats::as.formula(paste("Count ~",fixed,
    "+offset(log(Hours))+(1|AnimalKey)+(1|CageID)+(0+Post|CageID)"))
}

grid31g_fit <- function(d,hypothesis="full",dispersion=~1) {
  warnings <- character()
  fit <- function(alt=FALSE) tryCatch(withCallingHandlers(
    glmmTMB::glmmTMB(grid31g_formula(hypothesis),data=d,family=glmmTMB::nbinom2(),dispformula=dispersion,
      control=if(alt) glmmTMB::glmmTMBControl(optimizer=stats::optim,optArgs=list(method="BFGS"),
        optCtrl=list(maxit=1500),rank_check="stop",parallel=1L) else
        glmmTMB::glmmTMBControl(optCtrl=list(iter.max=1000,eval.max=1500),rank_check="stop",parallel=1L)),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=identity)
  m<-fit();diag<-grid31_fit_diagnostic(m);optimizer<-"nlminb"
  if(!diag$Valid) {
    other<-fit(TRUE);check<-grid31_fit_diagnostic(other)
    if(check$Valid){m<-other;diag<-check;optimizer<-"BFGS_retry"}
  }
  diag$Optimizer<-optimizer;diag$Warnings<-paste(unique(warnings),collapse=" | ")
  list(model=if(inherits(m,"error"))NULL else m,diagnostic=diag)
}

grid31g_contrasts <- function(m) {
  b<-glmmTMB::fixef(m)$cond
  required<-c("GroupRES:PeriodPost","GroupSUS:PeriodPost")
  if(!all(required %in% names(b))) stop("Full three-group coefficients missing.",call.=FALSE)
  c(`RES-CON`=unname(b[required[1]]),`SUS-CON`=unname(b[required[2]]),
    `SUS-RES`=unname(b[required[2]]-b[required[1]]))
}

grid31g_boot_one <- function(i,simulated,d,hypothesis) {
  d$Count<-simulated[[i]]
  f<-grid31g_fit(d);fd<-f$diagnostic
  if(hypothesis=="full") {
    value<-if(fd$Valid) grid31g_contrasts(f$model) else setNames(rep(NA_real_,3),c("RES-CON","SUS-CON","SUS-RES"))
    return(data.frame(Draw=i,Valid=fd$Valid,Boundary=isTRUE(fd$Boundary),Retry=fd$Optimizer=="BFGS_retry",
      as.list(value),Reason=if(fd$Valid)"" else paste(fd$Message,fd$Warnings,fd$MaxGradient,sep=" | "),check.names=FALSE))
  }
  n<-grid31g_fit(d,hypothesis);nd<-n$diagnostic
  valid<-fd$Valid && nd$Valid
  lr<-if(valid)2*(fd$LogLik-nd$LogLik) else NA_real_
  valid<-valid && is.finite(lr) && lr>=-1e-5
  data.frame(Draw=i,Valid=valid,Boundary=isTRUE(fd$Boundary)||isTRUE(nd$Boundary),
    Retry=fd$Optimizer=="BFGS_retry" || nd$Optimizer=="BFGS_retry",LR=if(valid)max(0,lr) else NA_real_,
    Reason=if(valid)"" else paste(fd$Message,nd$Message,fd$Warnings,nd$Warnings,fd$MaxGradient,nd$MaxGradient,sep=" | "))
}

grid31g_bootstrap <- function(model,d,hypothesis,draws,seed,cluster,checkpoint) {
  max_attempts<-ceiling(draws/.94)
  # Setting both kind and seed makes results independent of caller RNG state.
  old_kind<-RNGkind();had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  old_seed<-if(had_seed)get(".Random.seed",envir=.GlobalEnv) else NULL
  on.exit({do.call(RNGkind,as.list(old_kind));if(had_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)
    else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)},add=TRUE)
  RNGkind("Mersenne-Twister","Inversion","Rejection")
  simulations<-stats::simulate(model,nsim=max_attempts,seed=seed)
  result<-list();status<-"INSUFFICIENT_SUCCESSFUL_REFITS"
  for(ids in split(seq_len(max_attempts),ceiling(seq_len(max_attempts)/240))) {
    ans<-if(is.null(cluster))lapply(ids,grid31g_boot_one,simulated=simulations,d=d,hypothesis=hypothesis) else
      parallel::parLapply(cluster,ids,grid31g_boot_one,simulated=simulations,d=d,hypothesis=hypothesis)
    result<-c(result,ans);tab<-dplyr::bind_rows(result)
    readr::write_csv(tab,checkpoint,na="")
    cat(basename(checkpoint),sum(tab$Valid),"valid /",nrow(tab),"attempts\n");flush.console()
    if(mean(!tab$Valid)>.05){status<-"WITHHELD_REFIT_FAILURES_OVER_5_PERCENT";break}
    if(sum(tab$Valid)>=draws){status<-"PASS";break}
  }
  tab$Used<-FALSE
  if(status=="PASS")tab$Used[which(tab$Valid)[seq_len(draws)]]<-TRUE
  readr::write_csv(tab,checkpoint,na="")
  list(draws=tab,status=data.frame(Hypothesis=hypothesis,Status=status,Requested=draws,Attempted=nrow(tab),
    Successful=sum(tab$Valid),Used=sum(tab$Used),Failed=sum(!tab$Valid),Boundary=sum(tab$Boundary),
    Retry=sum(tab$Retry),Seed=seed))
}

grid31g_predictive <- function(m,d,seed,nsim=999L) {
  cell<-interaction(d$Group,d$Period,d$Batch,drop=TRUE)
  discrepancy<-function(y) {
    z<-unlist(lapply(levels(cell),function(g){i<-which(cell==g);c(MeanRate=mean(y[i]/d$Hours[i]),
      SDRate=if(length(i)>1)sd(y[i]/d$Hours[i]) else NA_real_,Zeros=sum(y[i]==0))}))
    names(z)<-paste(rep(levels(cell),each=3),rep(c("MeanRate","SDRate","Zeros"),length(levels(cell))),sep="|")
    ref<-which(d$Period=="Reference");post<-which(d$Period=="Post");post<-post[match(d$AnimalKey[ref],d$AnimalKey[post])]
    delta<-log1p(y[post]/d$Hours[post])-log1p(y[ref]/d$Hours[ref])
    cage_means<-tapply(delta,d$CageID[ref],mean)
    c(z,MaximumRate=max(y/d$Hours),AnimalPairedCorrelation=cor(log1p(y[ref]/d$Hours[ref]),log1p(y[post]/d$Hours[post])),
      CageMeanChangeSD=sd(cage_means))
  }
  observed<-discrepancy(d$Count)
  simulations<-stats::simulate(m,nsim=nsim,seed=seed)
  values<-vapply(simulations,discrepancy,observed)
  lower<-apply(values,1,quantile,probs=.025,na.rm=TRUE);upper<-apply(values,1,quantile,probs=.975,na.rm=TRUE)
  data.frame(Statistic=names(observed),Observed=observed,Lower95=lower,Upper95=upper,
    Outside95=ifelse(is.na(observed),NA,observed<lower|observed>upper),Simulations=nsim,Seed=seed,row.names=NULL)
}
