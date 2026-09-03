## Orchestrator's own check of the three algebraic identities and the formula-space geometry.
## Rebuilds the nine first-night z-features on the canonical clock window from scratch.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R"); source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
active_vals <- c("active","dark","night")
is_act <- function(x) str_to_lower(str_trim(as.character(x))) %in% active_vals

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types=cols(.default=col_skip(), AnimalNum=col_character(), Group=col_character(), Sex=col_character()),
    progress=FALSE), "roster")

d <- read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
              col_types=cols(AnimalNum=col_character(), BinStart=col_datetime(), .default=col_guess()),
              progress=FALSE) %>%
  mutate(AnimalNum=canonical_animal_id(AnimalNum)) %>% semi_join(roster, by="AnimalNum")
cc1 <- d %>% filter(as.character(CageChange)=="CC1") %>% filter(is_act(Phase))
anch <- cc1 %>% mutate(.b=animalpos_phase_block_index(BinStart)) %>%
  group_by(.s=as.character(SourceFile)) %>% summarise(tb=min(.b), .groups="drop") %>%
  mutate(ws=as.POSIXct(tb*ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC, origin="1970-01-01", tz="UTC"))
win <- cc1 %>% mutate(.s=as.character(SourceFile)) %>% left_join(anch, by=".s") %>%
  mutate(el=as.numeric(difftime(BinStart, ws, units="secs"))) %>% filter(el>=0, el < 12*3600)
cat("window rows:", nrow(win), " animals:", n_distinct(win$AnimalNum), "\n")

rmssd <- function(x){x<-x[is.finite(x)]; if(length(x)<3) NA_real_ else sqrt(mean(diff(x)^2))}
acf1  <- function(x){x<-x[is.finite(x)]; if(length(x)<4) NA_real_ else suppressWarnings(cor(x[-length(x)],x[-1]))}
feat <- win %>% arrange(AnimalNum, TimeIndex) %>% group_by(AnimalNum) %>%
  summarise(Movement_mean=mean(Movement,na.rm=TRUE), Movement_rmssd=rmssd(Movement), Movement_acf1=acf1(Movement),
            Entropy_mean=mean(Entropy,na.rm=TRUE), Entropy_rmssd=rmssd(Entropy), Entropy_acf1=acf1(Entropy),
            Proximity_mean=mean(ProximityFraction,na.rm=TRUE), Proximity_rmssd=rmssd(ProximityFraction),
            Proximity_acf1=acf1(ProximityFraction), .groups="drop") %>%
  left_join(roster, by="AnimalNum")
## Sex-only z (the established contract)
zs <- function(x){ s<-sd(x,na.rm=TRUE); if(!is.finite(s)||s==0) rep(0,length(x)) else (x-mean(x,na.rm=TRUE))/s }
F9 <- c("Movement_mean","Movement_rmssd","Movement_acf1","Entropy_mean","Entropy_rmssd","Entropy_acf1",
        "Proximity_mean","Proximity_rmssd","Proximity_acf1")
z <- feat %>% group_by(Sex) %>% mutate(across(all_of(F9), zs, .names="{.col}_z")) %>% ungroup()
cat("non-finite z counts:\n"); print(colSums(!is.finite(as.matrix(z %>% select(ends_with("_z"))))))

Mm<-z$Movement_mean_z; Mr<-z$Movement_rmssd_z; Ma<-z$Movement_acf1_z
Em<-z$Entropy_mean_z;  Er<-z$Entropy_rmssd_z;  Ea<-z$Entropy_acf1_z
Pm<-z$Proximity_mean_z; Pr<-z$Proximity_rmssd_z; Pa<-z$Proximity_acf1_z
c0 <- function(x) ifelse(is.finite(x), x, 0)
rm3 <- function(...) rowMeans(cbind(...), na.rm=TRUE)

D1 <- Mm
D2 <- rm3(Em,Er) - c0(Ea)
D3 <- rm3(Pm,Pa) - c0(Pr)
D4 <- rm3(Mr,Er,Pr)
D5 <- rm3(Mm,Em,Pm) - rm3(Ma,Ea)
D8 <- rm3(Em,Er) - rm3(Ma,Ea)
D9 <- Pm - c0(Pr)
D10<- Mm - Pm

cat("\n===== IDENTITY CHECKS (orchestrator's derivations) =====\n")
chk <- function(lab, lhs, rhs){ dd<-abs(lhs-rhs); cat(sprintf("  %-34s max|dev| = %.3e   n finite = %d\n", lab, max(dd,na.rm=TRUE), sum(is.finite(dd)))) }
chk("#8 == #2 + 0.5*(Ea - Ma)", D8, D2 + 0.5*(c0(Ea)-c0(Ma)))
chk("#9 == #3 + 0.5*(Pm - Pa)", D9, D3 + 0.5*(Pm-Pa))
chk("#10 == #1 - Pm",           D10, D1 - Pm)

cat("\n===== EMPIRICAL CORRELATIONS (the 7 named pairs) =====\n")
pairs <- list(c("8","2"),c("8","5"),c("9","3"),c("9","1"),c("10","1"),c("10","3"),c("10","5"))
DD <- list("1"=D1,"2"=D2,"3"=D3,"4"=D4,"5"=D5,"8"=D8,"9"=D9,"10"=D10)
for (p in pairs) {
  a<-DD[[p[1]]]; b<-DD[[p[2]]]
  ok <- is.finite(a)&is.finite(b)
  rF <- { i<-ok & z$Sex=="Female"; c(cor(a[i],b[i]), cor(a[i],b[i],method="spearman")) }
  rM <- { i<-ok & z$Sex=="Male";   c(cor(a[i],b[i]), cor(a[i],b[i],method="spearman")) }
  cat(sprintf("  #%-3s vs #%-3s  pooled r=%+.3f rho=%+.3f | F r=%+.3f rho=%+.3f | M r=%+.3f rho=%+.3f  (n=%d)\n",
      p[1],p[2], cor(a[ok],b[ok]), cor(a[ok],b[ok],method="spearman"), rF[1],rF[2], rM[1],rM[2], sum(ok)))
}

cat("\n===== FORMULA-SPACE GEOMETRY =====\n")
C <- rbind("1"=c(1,0,0,0,0,0,0,0,0), "2"=c(0,0,0,.5,.5,-1,0,0,0), "3"=c(0,0,0,0,0,0,.5,-1,.5),
           "4"=c(0,1/3,0,0,1/3,0,0,1/3,0), "5"=c(1/3,0,-.5,1/3,0,-.5,1/3,0,0),
           "8"=c(0,0,-.5,.5,.5,-.5,0,0,0), "9"=c(0,0,0,0,0,0,1,-1,0), "10"=c(1,0,0,0,0,0,-1,0,0))
colnames(C) <- F9
cs <- function(a,b) sum(C[a,]*C[b,])/(sqrt(sum(C[a,]^2))*sqrt(sum(C[b,]^2)))
for (p in list(c("2","8"),c("3","9"),c("1","10"),c("3","10"),c("5","10")))
  cat(sprintf("  cos(#%s,#%s) = %+.4f  (%.1f deg)\n", p[1],p[2], cs(p[1],p[2]), acos(pmin(1,pmax(-1,cs(p[1],p[2]))))*180/pi))
cat("  rank of the 8x9 coefficient matrix:", qr(C)$rank, "of 8 rows / 9 features\n")
ev <- eigen(cor(do.call(cbind, DD)[complete.cases(do.call(cbind,DD)),]), only.values=TRUE)$values
cat("  empirical 8-domain correlation eigenvalues:", paste(round(ev,3), collapse=", "), "\n")
cat("  n eigenvalues > 1:", sum(ev>1), " participation ratio:", round(sum(ev)^2/sum(ev^2),2), "\n")
cat("\n  Spearman of each omitted domain with Psychomotor activation (#1):\n")
for (k in c("8","9","10")) { a<-DD[[k]]; ok<-is.finite(a)&is.finite(D1)
  cat(sprintf("    #%-3s rho = %+.3f  %s\n", k, cor(a[ok],D1[ok],method="spearman"),
      ifelse(abs(cor(a[ok],D1[ok],method="spearman"))>=0.70,"<- LOCOMOTION-DOMINATED","")))}
