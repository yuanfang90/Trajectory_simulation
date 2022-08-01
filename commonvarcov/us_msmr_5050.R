rm(list=ls())
source("./functions.R")

library(lcmm)
library(mclust)
library(ggplot2)

procid <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# procid <- 23

set.seed(20220529+procid)
D.1 <- matrix(c(0.01,0.0001,0.0001,0.004),2,2)

formydata <- long.dat.generation(num_subj=c(500,500), grid=40+c(0:13)*4, space = "unequal", follow.length = "short", varcov = D.1, sig = 10)
# formydata$bwtot_ratio
mydata <- formydata$dat
true_lab <- formydata$true_lab
# trajplot.1 <- ggplot(data=mydata,aes(x=age, y=y, group=id)) +
#   geom_path(data=mydata[which(mydata$class==2),],colour="orange",na.rm = TRUE) +
#   geom_path(data=mydata[which(mydata$class==1),],colour="blue",na.rm = TRUE)
# trajplot.1

mydata.1 <- mydata
mydata.1$age.centered <- (mydata.1$age-70)/10
mydata.1$age.centered.base <- (mydata.1$baseline_age-70)/10
mydata.1$tis.centered <- mydata.1$age.centered-mydata.1$age.centered.base

fixed.fmla <- "y ~ age.centered"
rand.fmla <- " ~ age.centered"
mix.fmla <- fixed.fmla

###### model age uncentered
hm1 <- hlme(fixed = as.formula(fixed.fmla), random = as.formula(rand.fmla), subject = "id", data = mydata.1,verbose = FALSE)

hm2 <- gridsearch(m = hlme(fixed = as.formula(fixed.fmla), random = as.formula(rand.fmla), subject = "id", data = mydata.1, ng = 2, mixture = as.formula(mix.fmla), nwg = FALSE,verbose = FALSE), rep = 50, maxiter = 10, minit = hm1)

hm3 <- gridsearch(m = hlme(fixed = as.formula(fixed.fmla), random = as.formula(rand.fmla), subject = "id", data = mydata.1, ng = 3, mixture = as.formula(mix.fmla), nwg = FALSE,verbose = FALSE), rep = 50, maxiter = 10, minit = hm1)

# summarytable(hm1,hm2,hm3,which=c("G","conv","npm","loglik","BIC","ICL","%class"))
# summary(hm2)

bic.select <- which.min(c(hm1$BIC,hm2$BIC,hm3$BIC))
icl.select <- which.min(c(ICL(hm1),ICL(hm2),ICL(hm3)))

mod.all <- list(hm1,hm2,hm3)
names(mod.all) <- c("hm1","hm2","hm3")
best <- paste0("hm",icl.select)
mod.best <- mod.all[[best]]

forZ <- cbind(1,mydata.1$age.centered)

D.est.1 <- matrix(hm1$best[c("varcov 1","varcov 2","varcov 2","varcov 3")],ncol=2,nrow=2)
bw_var.1 <- forZ%*%D.est.1%*%t(forZ)
sigma2.1 <- (hm1$best["stderr"])^2
R.1 <- diag(sigma2.1,length(mydata.1$age.centered))
btr.est.1 <- sum(diag(bw_var.1))/(sum(diag(bw_var.1+R.1)))

D.est.2 <- matrix(mod.best$best[c("varcov 1","varcov 2","varcov 2","varcov 3")],ncol=2,nrow=2)
bw_var.2 <- forZ%*%D.est.1%*%t(forZ)
sigma2.2 <- (mod.best$best["stderr"])^2
R.2 <- diag(sigma2.2,length(mydata.1$age.centered))
btr.est.2 <- sum(diag(bw_var.2))/(sum(diag(bw_var.2+R.2)))

library(clv)
std <- std.ext(as.integer(mod.best$pprob$class), as.integer(true_lab))
# to compute three indicies based on std.ext result
rand1 <- clv.Rand(std)
jaccard1 <- clv.Jaccard(std)
folk.mal1 <- clv.Folkes.Mallows(std)
ari<-adjustedRandIndex(mod.best$pprob$class,true_lab)
mcr<-classError(mod.best$pprob$class,true_lab)$errorRate

misclassed <- classError(mod.best$pprob$class,true_lab)$misclassified
pprobs.misc <- mod.best$pprob[misclassed,-c(1:2)]
meanpprob.misclassed <- median(apply(pprobs.misc,1,max))
pprobs.correct <- mod.best$pprob[-misclassed,-c(1:2)]
meanpprob.correct <- median(apply(pprobs.correct,1,max))

if(bic.select==2|icl.select==2){
  result <- c(hm2$best[-1],bic.select=bic.select,icl.select=icl.select,ARI=ari,MCR=mcr,RI=rand1,JI=jaccard1,FMI=folk.mal1,mean.pprob.misc=meanpprob.misclassed,mean.pprob.correct=meanpprob.correct,btr.est.hm1=btr.est.1,btr.est.best=btr.est.2,sigma2.1)
}else{
  result <- c(rep(NA,length(hm2$best[-1])),bic.select=bic.select,icl.select=icl.select,ARI=ari,MCR=mcr,RI=rand1,JI=jaccard1,FMI=folk.mal1,mean.pprob.misc=meanpprob.misclassed,mean.pprob.correct=meanpprob.correct,btr.est.hm1=btr.est.1,btr.est.best=btr.est.2,sigma2.1)
  names(result)[1:length(hm2$best[-1])] <- names(hm2$best[-1])
}

filename = paste0("./commonvarcov/us_msmr_5050/simulation_",procid,".csv")
write.csv(t(result),file=filename)

rdname = paste0("./commonvarcov/us_msmr_5050/simulation_",procid,".RData")
save(mydata.1,mod.best,file=rdname)
