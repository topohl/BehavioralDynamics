# Stage 31c: upstream count models. No exposure timing is inferred here.

grid31_stats_data <- function(a) {
  keys <- c("Sex", "Phase", "Batch", "CageID", "AnimalKey", "AnimalNum", "Group", "Condition")
  grid31_required(a, c(keys, "PhaseLabel", "RecordedCrossings", "NominalHours", "EligiblePhase"), "Phase data")
  if (anyNA(a[c(keys, "PhaseLabel", "RecordedCrossings", "NominalHours", "EligiblePhase")]) ||
      any(!is.finite(a$RecordedCrossings) | a$RecordedCrossings < 0 | a$RecordedCrossings != floor(a$RecordedCrossings)) ||
      any(a$NominalHours != 12) || !is.logical(a$EligiblePhase) ||
      any(!a$Group %in% c("CON", "RES", "SUS")) || any(!a$Condition %in% c("CON", "SIS")) ||
      any((a$Group == "CON") != (a$Condition == "CON")) || any(!a$Sex %in% c("Female", "Male")) ||
      any(!a$Phase %in% c("Inactive", "Active"))) stop("Invalid phase counts, duration or identity.", call. = FALSE)
  if (anyDuplicated(a[c("AnimalKey", "PhaseLabel")])) stop("Duplicate animal phase.", call. = FALSE)
  id <- a %>% dplyr::group_by(AnimalKey) %>% dplyr::summarise(dplyr::across(dplyr::all_of(setdiff(keys, c("AnimalKey", "Phase"))), dplyr::n_distinct), .groups="drop")
  if (any(as.matrix(id[-1]) != 1)) stop("Unstable animal identity.", call. = FALSE)
  cage <- a %>% dplyr::group_by(CageID) %>% dplyr::summarise(dplyr::across(c(Sex, Batch, Condition), dplyr::n_distinct), .groups="drop")
  if (any(as.matrix(cage[-1]) != 1)) stop("Mixed cage condition/sex/batch.", call. = FALSE)
  # A1 does not determine the primary population. Recompute from phase eligibility.
  d <- a %>% dplyr::filter(PhaseLabel %in% c(paste0("I", 2:5), paste0("A", 2:5)))
  expected <- tidyr::crossing(AnimalKey=unique(a$AnimalKey), PhaseLabel=c(paste0("I",2:5),paste0("A",2:5)))
  if (nrow(dplyr::anti_join(expected,d,by=c("AnimalKey","PhaseLabel"))) ||
      any(substr(d$PhaseLabel,1,1) != substr(d$Phase,1,1))) stop("Missing or inconsistent required phase rows.", call. = FALSE)
  d <- d %>% dplyr::group_by(Phase,CageID) %>% dplyr::mutate(CompleteCage=all(EligiblePhase)) %>% dplyr::ungroup()
  roster <- d %>% dplyr::group_by(Sex,Phase,Batch,CageID,Condition) %>%
    dplyr::summarise(Animals=dplyr::n_distinct(AnimalKey), Included=all(CompleteCage), .groups="drop")
  animals <- d %>% dplyr::filter(CompleteCage) %>%
    dplyr::mutate(Period=ifelse(PhaseLabel %in% c("I2","A2"),"Reference","Post")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys,"Period")))) %>%
    dplyr::summarise(Count=sum(RecordedCrossings),Hours=sum(NominalHours),.groups="drop")
  if (any(animals$Hours != ifelse(animals$Period=="Reference",12,36))) stop("Wrong period exposure.", call. = FALSE)
  cages <- animals %>% dplyr::group_by(Sex,Phase,Batch,CageID,Condition,Period) %>%
    dplyr::summarise(Count=sum(Count),Hours=sum(Hours),Animals=dplyr::n_distinct(AnimalKey),.groups="drop")
  coverage <- cages %>% dplyr::distinct(Sex,Phase,Batch,Condition)
  required <- tidyr::crossing(a %>% dplyr::distinct(Sex,Batch),Phase=c("Inactive","Active"),Condition=c("CON","SIS"))
  if (nrow(dplyr::anti_join(required,coverage,by=c("Sex","Batch","Phase","Condition")))) stop("Missing complete condition in a batch.", call. = FALSE)
  list(animals=animals,cages=cages,roster=roster)
}

grid31_stats_factors <- function(d) {
  d$Period <- factor(d$Period,c("Reference","Post")); d$Batch <- factor(d$Batch)
  d$Condition <- factor(d$Condition,c("CON","SIS")); d$Post <- as.numeric(d$Period=="Post")
  if ("Group" %in% names(d)) d$Group <- factor(d$Group,c("CON","RES","SUS"))
  as.data.frame(d[order(d$Batch,d$CageID,if("AnimalKey" %in% names(d)) d$AnimalKey else d$CageID,d$Period),])
}

grid31_stats_formula <- function(level="primary", null=FALSE) {
  if (level=="primary") {
    if (null) Count ~ Condition+Period+Batch*Period+offset(log(Hours))+(1|CageID)
    else Count ~ Condition*Period+Batch*Period+offset(log(Hours))+(1|CageID)
  } else Count ~ Group*Period+Batch*Period+offset(log(Hours))+(1|AnimalKey)+(1|CageID)+(0+Post|CageID)
}

grid31_fit_diagnostic <- function(m) {
  if (inherits(m,"error")) return(data.frame(Valid=FALSE,Boundary=NA,Convergence=NA,PositiveHessian=FALSE,
    MaxGradient=NA,MinRandomSD=NA,Dispersion=NA,LogLik=NA,Message=conditionMessage(m)))
  sd <- unlist(lapply(glmmTMB::VarCorr(m)$cond,function(z) attr(z,"stddev")))
  grad <- max(abs(m$sdr$gradient.fixed)); ll <- as.numeric(stats::logLik(m))
  valid <- isTRUE(m$sdr$pdHess) && identical(m$fit$convergence,0L) && is.finite(ll) &&
    is.finite(grad) && grad<=0.01 && all(is.finite(glmmTMB::fixef(m)$cond)) && all(is.finite(sd))
  data.frame(Valid=valid,Boundary=any(sd<1e-4),Convergence=m$fit$convergence,
    PositiveHessian=isTRUE(m$sdr$pdHess),MaxGradient=grad,MinRandomSD=min(sd),
    Dispersion=stats::sigma(m),LogLik=ll,Message=if(is.null(m$fit$message)) "" else m$fit$message)
}

grid31_stats_fit <- function(d, level="primary", null=FALSE, dispersion=~1) {
  warn <- character()
  fit <- function(alternative=FALSE) tryCatch(withCallingHandlers(glmmTMB::glmmTMB(
    grid31_stats_formula(level,null),data=d,family=glmmTMB::nbinom2(),dispformula=dispersion,
    control=if(alternative) glmmTMB::glmmTMBControl(optimizer=stats::optim,optArgs=list(method="BFGS"),
      optCtrl=list(maxit=1500),rank_check="stop",parallel=1L) else
      glmmTMB::glmmTMBControl(optCtrl=list(iter.max=1000,eval.max=1500),rank_check="stop",parallel=1L)),
    warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart("muffleWarning")}),error=identity)
  m <- fit(); check <- grid31_fit_diagnostic(m); optimizer <- "nlminb"
  if (!check$Valid) {
    alternative <- fit(TRUE); other <- grid31_fit_diagnostic(alternative)
    if(other$Valid) {m<-alternative;check<-other;optimizer<-"BFGS_retry"}
  }
  check$Optimizer <- optimizer;check$Warnings <- paste(unique(warn),collapse=" | ")
  list(model=if(inherits(m,"error")) NULL else m,diagnostic=check)
}

grid31_stats_effect <- function(m,d) {
  # Marginal over normally distributed cage intercepts; equal weight per batch.
  b <- glmmTMB::fixef(m)$cond
  nd <- expand.grid(Condition=levels(d$Condition),Period=levels(d$Period),Batch=levels(d$Batch))
  nd$Condition<-factor(nd$Condition,levels(d$Condition));nd$Period<-factor(nd$Period,levels(d$Period));nd$Batch<-factor(nd$Batch,levels(d$Batch))
  x <- stats::model.matrix(~Condition*Period+Batch*Period,nd)
  variance <- sum(vapply(glmmTMB::VarCorr(m)$cond,function(z) sum(diag(z)),numeric(1)))
  rate <- exp(drop(x[,names(b),drop=FALSE] %*% b)+variance/2)
  value <- tapply(rate,list(nd$Condition,nd$Period),mean)
  c(LogRRR=unname(b["ConditionSIS:PeriodPost"]),CONReference=value["CON","Reference"],CONPost=value["CON","Post"],
    SISReference=value["SIS","Reference"],SISPost=value["SIS","Post"],
    CONChange=value["CON","Post"]-value["CON","Reference"],SISChange=value["SIS","Post"]-value["SIS","Reference"],
    ChangeDifference=(value["SIS","Post"]-value["SIS","Reference"])-(value["CON","Post"]-value["CON","Reference"]))
}

grid31_boot_one <- function(i, simulated, d, type) {
  d$Count<-simulated[[i]]
  f<-grid31_stats_fit(d);fd<-f$diagnostic
  if(type=="null") {
    n<-grid31_stats_fit(d,null=TRUE);nd<-n$diagnostic
    valid<-fd$Valid && nd$Valid
    lr<-if(valid) 2*(fd$LogLik-nd$LogLik) else NA_real_
    valid<-valid && is.finite(lr) && lr>=-1e-5
    data.frame(Draw=i,Valid=valid,Boundary=isTRUE(fd$Boundary)||isTRUE(nd$Boundary),
      LR=if(valid)max(0,lr) else NA_real_,Reason=if(valid)"" else paste(fd$Message,nd$Message,fd$Warnings,nd$Warnings,sep=" | "))
  } else {
    vals<-if(fd$Valid) grid31_stats_effect(f$model,d) else setNames(rep(NA_real_,8),c("LogRRR","CONReference","CONPost","SISReference","SISPost","CONChange","SISChange","ChangeDifference"))
    data.frame(Draw=i,Valid=fd$Valid,Boundary=isTRUE(fd$Boundary),as.list(vals),Reason=if(fd$Valid)"" else paste(fd$Message,fd$Warnings,sep=" | "))
  }
}

grid31_bootstrap <- function(model,d,type,draws,seed,cluster,checkpoint) {
  # Fixed simulation budget; select first draws successful refits only after
  # reporting all failures. Stop early if the first 200 exceed 5% failures.
  max_attempts<-ceiling(draws/0.94)
  simulations<-stats::simulate(model,nsim=max_attempts,seed=seed)
  chunks<-split(seq_len(max_attempts),ceiling(seq_len(max_attempts)/200))
  result<-list(); status<-"INSUFFICIENT_SUCCESSFUL_REFITS"
  for(ids in chunks) {
    ans<-if(is.null(cluster)) lapply(ids,grid31_boot_one,simulated=simulations,d=d,type=type) else
      parallel::parLapply(cluster,ids,grid31_boot_one,simulated=simulations,d=d,type=type)
    result<-c(result,ans);tab<-dplyr::bind_rows(result)
    readr::write_csv(tab,checkpoint,na="")
    cat(basename(checkpoint),":",sum(tab$Valid),"valid /",nrow(tab),"attempts\n");flush.console()
    if(mean(!tab$Valid)>0.05) {status<-"WITHHELD_REFIT_FAILURES_OVER_5_PERCENT";break}
    if(sum(tab$Valid)>=draws){status<-"PASS";break}
  }
  tab$Used<-FALSE
  if(status=="PASS") tab$Used[which(tab$Valid)[seq_len(draws)]]<-TRUE
  readr::write_csv(tab,checkpoint,na="")
  list(draws=tab,status=data.frame(Type=type,Status=status,Requested=draws,Attempted=nrow(tab),
    Successful=sum(tab$Valid),Used=sum(tab$Used),Failed=sum(!tab$Valid),Boundary=sum(tab$Boundary),Seed=seed))
}

grid31_sim_checks <- function(m,d,seed,nsim=999L) {
  sim<-stats::simulate(m,nsim=nsim,seed=seed)
  cell<-interaction(d$Condition,d$Period,d$Batch,drop=TRUE)
  # Observable discrepancies, compared to unconditional simulations with fresh
  # cage effects. These are predictive checks, not independent hypothesis tests.
  stats_fn<-function(y) {
    rows<-lapply(levels(cell),function(g){i<-which(cell==g);c(MeanRate=mean(y[i]/d$Hours[i]),
      SDRate=if(length(i)>1)sd(y[i]/d$Hours[i]) else NA_real_,Zeros=sum(y[i]==0))})
    z<-unlist(rows);names(z)<-paste(rep(levels(cell),each=3),rep(c("MeanRate","SDRate","Zeros"),length(levels(cell))),sep="|")
    ref<-which(d$Period=="Reference");post<-match(d$CageID[ref],d$CageID[d$Period=="Post"])
    c(z,OverallMaxRate=max(y/d$Hours),PairedLogRateCorrelation=cor(log1p(y[ref]/d$Hours[ref]),log1p(y[d$Period=="Post"][post]/d$Hours[d$Period=="Post"][post])))
  }
  observed<-stats_fn(d$Count);draw<-vapply(sim,stats_fn,observed)
  lower<-apply(draw,1,quantile,probs=.025,na.rm=TRUE);upper<-apply(draw,1,quantile,probs=.975,na.rm=TRUE)
  data.frame(Statistic=names(observed),Observed=observed,LowerPredictive95=lower,UpperPredictive95=upper,
    Outside95=ifelse(is.na(observed),NA,observed<lower|observed>upper),Simulations=nsim,Seed=seed,row.names=NULL)
}
