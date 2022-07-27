# rm(list=ls())
library(ggplot2)

#### Combine result from each dataset into one matrix, do this for all simulations
source("/rprojectnb2/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/result_comb.R")

# beta_trans_1 <- c(7.5,-1.5)
# beta_trans_2 <- c(6,-0.5)

sssr_pars <- c(7.5,6,-1.5,-0.5,1.004,0.141,0.02,sqrt(2),0.32,0.32)
ssmr_pars <- c(7.5,6,-1.5,-0.5,4.434,0.631,0.09,sqrt(2),0.68,0.68)
mssr_pars <- c(7.5,6,-1.5,-0.5,4.924,0.701,0.1,sqrt(10),0.32,0.32)
msmr_pars <- c(7.5,6,-1.5,-0.5,19.624,2.801,0.4,sqrt(10),0.65,0.65)


simnames <- c("sssr","ssmr","mssr","msmr")
simulations <- factor(simnames, levels = simnames)

##########################################################
#### read in example datasets
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_sssr_5050/simulation_7.RData")
exdat_ul_sssr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_sssr_2080/simulation_7.RData")
exdat_ul_sssr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_sssr_0595/simulation_7.RData")
exdat_ul_sssr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_ssmr_5050/simulation_7.RData")
exdat_ul_ssmr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_ssmr_2080/simulation_7.RData")
exdat_ul_ssmr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_ssmr_0595/simulation_7.RData")
exdat_ul_ssmr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_mssr_5050/simulation_7.RData")
exdat_ul_mssr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_mssr_2080/simulation_7.RData")
exdat_ul_mssr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_mssr_0595/simulation_7.RData")
exdat_ul_mssr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_msmr_5050/simulation_7.RData")
exdat_ul_msmr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_msmr_2080/simulation_7.RData")
exdat_ul_msmr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/ul_msmr_0595/simulation_7.RData")
exdat_ul_msmr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_sssr_5050/simulation_7.RData")
exdat_us_sssr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_sssr_2080/simulation_7.RData")
exdat_us_sssr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_sssr_0595/simulation_7.RData")
exdat_us_sssr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_ssmr_5050/simulation_7.RData")
exdat_us_ssmr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_ssmr_2080/simulation_7.RData")
exdat_us_ssmr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_ssmr_0595/simulation_7.RData")
exdat_us_ssmr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_mssr_5050/simulation_7.RData")
exdat_us_mssr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_mssr_2080/simulation_7.RData")
exdat_us_mssr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_mssr_0595/simulation_7.RData")
exdat_us_mssr_0595 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_msmr_5050/simulation_7.RData")
exdat_us_msmr_5050 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_msmr_2080/simulation_7.RData")
exdat_us_msmr_2080 <- mydata.1
load("/restricted/projectnb/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/us_msmr_0595/simulation_7.RData")
exdat_us_msmr_0595 <- mydata.1

###########################################################
#### summary for ul_5050
ul_5050_ls <- list(ul_sssr_5050,ul_ssmr_5050,ul_mssr_5050,ul_msmr_5050)
names(ul_5050_ls) <- simnames
####### Plot the number of times that BIC/ICL selected 2-component model
ul5050.bic.select <- c(sum(ul_sssr_5050$bic.select==2),sum(ul_ssmr_5050$bic.select==2),sum(ul_mssr_5050$bic.select==2),sum(ul_msmr_5050$bic.select==2))
ul5050.icl.select <- c(sum(ul_sssr_5050$icl.select==2),sum(ul_ssmr_5050$icl.select==2),sum(ul_mssr_5050$icl.select==2),sum(ul_msmr_5050$icl.select==2))

####### Check the validation measures: ARI, RI, MCR, JI, FMI. All measures are calculated regardless of whether correct number of component was selected. 
########## ARI
ARI <- matrix(NA,nrow=100*4,ncol=2)
ARI <- data.frame(ARI)
names(ARI) <- c("sim","ari")
for(s in simnames){
  ind <- which(simnames==s)
  forari <- ul_5050_ls[[s]]
  ARI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  ARI$ari[(100*(ind-1)+1):(100*ind)] <- forari$ARI
}
ARI$sim <- factor(ARI$sim, levels = simnames)
# boxplot(ARI, use.cols = TRUE, main = "ARI for each simulation in case 3", ylab = "ARI")
ul5050.box.ari <- ggplot(ARI, aes(x=sim, y=ari, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("") + ylab("") + ggtitle("ARI")+
  theme(axis.text.x = element_text(angle = 90))
# ul5050.box.ari

########## RI
RI <- matrix(NA,nrow=100*4,ncol=2)
RI <- data.frame(RI)
names(RI) <- c("sim","ri")
for(s in simnames){
  ind <- which(simnames==s)
  forri <- ul_5050_ls[[s]]
  RI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  RI$ri[(100*(ind-1)+1):(100*ind)] <- forri$RI
}
RI$sim <- factor(RI$sim, levels = simnames)
ul5050.box.ri <- ggplot(RI, aes(x=sim, y=ri, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("RI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ri

########## MCR
MCR <- matrix(NA,nrow=100*4,ncol=2)
MCR <- data.frame(MCR)
names(MCR) <- c("sim","mcr")
for(s in simnames){
  ind <- which(simnames==s)
  formcr <- ul_5050_ls[[s]]
  MCR$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  MCR$mcr[(100*(ind-1)+1):(100*ind)] <- formcr$MCR
}
MCR$sim <- factor(MCR$sim, levels = simnames)
ul5050.box.mcr <- ggplot(MCR, aes(x=sim, y=mcr, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("MCR")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.mcr

########## JI
JI <- matrix(NA,nrow=100*4,ncol=2)
JI <- data.frame(JI)
names(JI) <- c("sim","ji")
for(s in simnames){
  ind <- which(simnames==s)
  forji <- ul_5050_ls[[s]]
  JI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  JI$ji[(100*(ind-1)+1):(100*ind)] <- forji$JI
}
JI$sim <- factor(JI$sim, levels = simnames)
ul5050.box.ji <- ggplot(JI, aes(x=sim, y=ji, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("JI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ji

########## Parameter estimation
ul5050.par <- list()
for(s in simnames){
  ind <- which(simnames==s)
  forpar <- ul_5050_ls[[s]][,c(1:8,18:19)]
  ## need to order the first 4 columns because classes may flipped
  for(r in seq_along(forpar[,1])){
    row <- forpar[r,]
    ord <- c(order(row[3:4]),order(row[3:4])+2,5:10)
    forpar[r,] <- row[ord]
  }
  ul5050.par[[ind]] <- na.omit(forpar)
}
names(ul5050.par) <- paste0(simnames,".par")

###########################################################
#### summary for ul_2080
ul_2080_ls <- list(ul_sssr_2080,ul_ssmr_2080,ul_mssr_2080,ul_msmr_2080)
names(ul_2080_ls) <- simnames
####### Plot the number of times that BIC/ICL selected 2-component model
ul2080.bic.select <- c(sum(ul_sssr_2080$bic.select==2),sum(ul_ssmr_2080$bic.select==2),sum(ul_mssr_2080$bic.select==2),sum(ul_msmr_2080$bic.select==2))
ul2080.icl.select <- c(sum(ul_sssr_2080$icl.select==2),sum(ul_ssmr_2080$icl.select==2),sum(ul_mssr_2080$icl.select==2),sum(ul_msmr_2080$icl.select==2))

####### Check the validation measures: ARI, RI, MCR, JI, FMI. All measures are calculated regardless of whether correct number of component was selected. 
########## ARI
ARI <- matrix(NA,nrow=100*4,ncol=2)
ARI <- data.frame(ARI)
names(ARI) <- c("sim","ari")
for(s in simnames){
  ind <- which(simnames==s)
  forari <- ul_2080_ls[[s]]
  ARI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  ARI$ari[(100*(ind-1)+1):(100*ind)] <- forari$ARI
}
ARI$sim <- factor(ARI$sim, levels = simnames)
# boxplot(ARI, use.cols = TRUE, main = "ARI for each simulation", ylab = "ARI")
ul2080.box.ari <- ggplot(ARI, aes(x=sim, y=ari, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("ARI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ari

########## RI
RI <- matrix(NA,nrow=100*4,ncol=2)
RI <- data.frame(RI)
names(RI) <- c("sim","ri")
for(s in simnames){
  ind <- which(simnames==s)
  forri <- ul_2080_ls[[s]]
  RI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  RI$ri[(100*(ind-1)+1):(100*ind)] <- forri$RI
}
RI$sim <- factor(RI$sim, levels = simnames)
ul2080.box.ri <- ggplot(RI, aes(x=sim, y=ri, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("RI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ri

########## MCR
MCR <- matrix(NA,nrow=100*4,ncol=2)
MCR <- data.frame(MCR)
names(MCR) <- c("sim","mcr")
for(s in simnames){
  ind <- which(simnames==s)
  formcr <- ul_2080_ls[[s]]
  MCR$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  MCR$mcr[(100*(ind-1)+1):(100*ind)] <- formcr$MCR
}
MCR$sim <- factor(MCR$sim, levels = simnames)
ul2080.box.mcr <- ggplot(MCR, aes(x=sim, y=mcr, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("MCR")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.mcr

########## JI
JI <- matrix(NA,nrow=100*4,ncol=2)
JI <- data.frame(JI)
names(JI) <- c("sim","ji")
for(s in simnames){
  ind <- which(simnames==s)
  forji <- ul_2080_ls[[s]]
  JI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  JI$ji[(100*(ind-1)+1):(100*ind)] <- forji$JI
}
JI$sim <- factor(JI$sim, levels = simnames)
ul2080.box.ji <- ggplot(JI, aes(x=sim, y=ji, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("JI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ji

########## Parameter estimation
ul2080.par <- list()
for(s in simnames){
  ind <- which(simnames==s)
  forpar <- ul_2080_ls[[s]][,c(1:8,18:19)]
  ## need to order the first 4 columns because classes may flipped
  for(r in seq_along(forpar[,1])){
    row <- forpar[r,]
    ord <- c(order(row[3:4]),order(row[3:4])+2,5:10)
    forpar[r,] <- row[ord]
  }
  ul2080.par[[ind]] <- na.omit(forpar)
}
names(ul2080.par) <- paste0(simnames,".par")

###########################################################
#### summary for ul_0595
ul_0595_ls <- list(ul_sssr_0595,ul_ssmr_0595,ul_mssr_0595,ul_msmr_0595)
names(ul_0595_ls) <- simnames
####### Plot the number of times that BIC/ICL selected 2-component model
ul0595.bic.select <- c(sum(ul_sssr_0595$bic.select==2),sum(ul_ssmr_0595$bic.select==2),sum(ul_mssr_0595$bic.select==2),sum(ul_msmr_0595$bic.select==2))
ul0595.icl.select <- c(sum(ul_sssr_0595$icl.select==2),sum(ul_ssmr_0595$icl.select==2),sum(ul_mssr_0595$icl.select==2),sum(ul_msmr_0595$icl.select==2))

####### Check the validation measures: ARI, RI, MCR, JI, FMI. All measures are calculated regardless of whether correct number of component was selected. 
########## ARI
ARI <- matrix(NA,nrow=100*4,ncol=2)
ARI <- data.frame(ARI)
names(ARI) <- c("sim","ari")
for(s in simnames){
  ind <- which(simnames==s)
  forari <- ul_0595_ls[[s]]
  ARI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  ARI$ari[(100*(ind-1)+1):(100*ind)] <- forari$ARI
}
ARI$sim <- factor(ARI$sim, levels = simnames)
# boxplot(ARI, use.cols = TRUE, main = "ARI for each simulation", ylab = "ARI")
ul0595.box.ari <- ggplot(ARI, aes(x=sim, y=ari, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("ARI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ari

########## RI
RI <- matrix(NA,nrow=100*4,ncol=2)
RI <- data.frame(RI)
names(RI) <- c("sim","ri")
for(s in simnames){
  ind <- which(simnames==s)
  forri <- ul_0595_ls[[s]]
  RI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  RI$ri[(100*(ind-1)+1):(100*ind)] <- forri$RI
}
RI$sim <- factor(RI$sim, levels = simnames)
ul0595.box.ri <- ggplot(RI, aes(x=sim, y=ri, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("RI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ri

########## MCR
MCR <- matrix(NA,nrow=100*4,ncol=2)
MCR <- data.frame(MCR)
names(MCR) <- c("sim","mcr")
for(s in simnames){
  ind <- which(simnames==s)
  formcr <- ul_0595_ls[[s]]
  MCR$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  MCR$mcr[(100*(ind-1)+1):(100*ind)] <- formcr$MCR
}
MCR$sim <- factor(MCR$sim, levels = simnames)
ul0595.box.mcr <- ggplot(MCR, aes(x=sim, y=mcr, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("MCR")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.mcr

########## JI
JI <- matrix(NA,nrow=100*4,ncol=2)
JI <- data.frame(JI)
names(JI) <- c("sim","ji")
for(s in simnames){
  ind <- which(simnames==s)
  forji <- ul_0595_ls[[s]]
  JI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  JI$ji[(100*(ind-1)+1):(100*ind)] <- forji$JI
}
JI$sim <- factor(JI$sim, levels = simnames)
ul0595.box.ji <- ggplot(JI, aes(x=sim, y=ji, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("JI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ji

########## Parameter estimation
ul0595.par <- list()
for(s in simnames){
  ind <- which(simnames==s)
  forpar <- ul_0595_ls[[s]][,c(1:8,18:19)]
  ## need to order the first 4 columns because classes may flipped
  for(r in seq_along(forpar[,1])){
    row <- forpar[r,]
    ord <- c(order(row[3:4]),order(row[3:4])+2,5:10)
    forpar[r,] <- row[ord]
  }
  ul0595.par[[ind]] <- na.omit(forpar)
}
names(ul0595.par) <- paste0(simnames,".par")

###########################################################
#### summary for us_5050
us_5050_ls <- list(us_sssr_5050,us_ssmr_5050,us_mssr_5050,us_msmr_5050)
names(us_5050_ls) <- simnames
####### Plot the number of times that BIC/ICL selected 2-component model
us5050.bic.select <- c(sum(us_sssr_5050$bic.select==2),sum(us_ssmr_5050$bic.select==2),sum(us_mssr_5050$bic.select==2),sum(us_msmr_5050$bic.select==2))
us5050.icl.select <- c(sum(us_sssr_5050$icl.select==2),sum(us_ssmr_5050$icl.select==2),sum(us_mssr_5050$icl.select==2),sum(us_msmr_5050$icl.select==2))

####### Check the validation measures: ARI, RI, MCR, JI, FMI. All measures are calculated regardless of whether correct number of component was selected. 
########## ARI
ARI <- matrix(NA,nrow=100*4,ncol=2)
ARI <- data.frame(ARI)
names(ARI) <- c("sim","ari")
for(s in simnames){
  ind <- which(simnames==s)
  forari <- us_5050_ls[[s]]
  ARI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  ARI$ari[(100*(ind-1)+1):(100*ind)] <- forari$ARI
}
ARI$sim <- factor(ARI$sim, levels = simnames)
# boxplot(ARI, use.cols = TRUE, main = "ARI for each simulation in case 3", ylab = "ARI")
us5050.box.ari <- ggplot(ARI, aes(x=sim, y=ari, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("ARI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ari

########## RI
RI <- matrix(NA,nrow=100*4,ncol=2)
RI <- data.frame(RI)
names(RI) <- c("sim","ri")
for(s in simnames){
  ind <- which(simnames==s)
  forri <- us_5050_ls[[s]]
  RI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  RI$ri[(100*(ind-1)+1):(100*ind)] <- forri$RI
}
RI$sim <- factor(RI$sim, levels = simnames)
us5050.box.ri <- ggplot(RI, aes(x=sim, y=ri, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("RI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ri

########## MCR
MCR <- matrix(NA,nrow=100*4,ncol=2)
MCR <- data.frame(MCR)
names(MCR) <- c("sim","mcr")
for(s in simnames){
  ind <- which(simnames==s)
  formcr <- us_5050_ls[[s]]
  MCR$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  MCR$mcr[(100*(ind-1)+1):(100*ind)] <- formcr$MCR
}
MCR$sim <- factor(MCR$sim, levels = simnames)
us5050.box.mcr <- ggplot(MCR, aes(x=sim, y=mcr, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("MCR")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.mcr

########## JI
JI <- matrix(NA,nrow=100*4,ncol=2)
JI <- data.frame(JI)
names(JI) <- c("sim","ji")
for(s in simnames){
  ind <- which(simnames==s)
  forji <- us_5050_ls[[s]]
  JI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  JI$ji[(100*(ind-1)+1):(100*ind)] <- forji$JI
}
JI$sim <- factor(JI$sim, levels = simnames)
us5050.box.ji <- ggplot(JI, aes(x=sim, y=ji, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("JI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ji

########## Parameter estimation
us5050.par <- list()
for(s in simnames){
  ind <- which(simnames==s)
  forpar <- us_5050_ls[[s]][,c(1:8,18:19)]
  ## need to order the first 4 columns because classes may flipped
  for(r in seq_along(forpar[,1])){
    row <- forpar[r,]
    ord <- c(order(row[3:4]),order(row[3:4])+2,5:10)
    forpar[r,] <- row[ord]
  }
  us5050.par[[ind]] <- na.omit(forpar)
}
names(us5050.par) <- paste0(simnames,".par")

###########################################################
#### summary for us_2080
us_2080_ls <- list(us_sssr_2080,us_ssmr_2080,us_mssr_2080,us_msmr_2080)
names(us_2080_ls) <- simnames
####### Plot the number of times that BIC/ICL selected 2-component model
us2080.bic.select <- c(sum(us_sssr_2080$bic.select==2),sum(us_ssmr_2080$bic.select==2),sum(us_mssr_2080$bic.select==2),sum(us_msmr_2080$bic.select==2))
us2080.icl.select <- c(sum(us_sssr_2080$icl.select==2),sum(us_ssmr_2080$icl.select==2),sum(us_mssr_2080$icl.select==2),sum(us_msmr_2080$icl.select==2))

####### Check the validation measures: ARI, RI, MCR, JI, FMI. All measures are calculated regardless of whether correct number of component was selected. 
########## ARI
ARI <- matrix(NA,nrow=100*4,ncol=2)
ARI <- data.frame(ARI)
names(ARI) <- c("sim","ari")
for(s in simnames){
  ind <- which(simnames==s)
  forari <- us_2080_ls[[s]]
  ARI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  ARI$ari[(100*(ind-1)+1):(100*ind)] <- forari$ARI
}
ARI$sim <- factor(ARI$sim, levels = simnames)
# boxplot(ARI, use.cols = TRUE, main = "ARI for each simulation", ylab = "ARI")
us2080.box.ari <- ggplot(ARI, aes(x=sim, y=ari, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("ARI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ari

########## RI
RI <- matrix(NA,nrow=100*4,ncol=2)
RI <- data.frame(RI)
names(RI) <- c("sim","ri")
for(s in simnames){
  ind <- which(simnames==s)
  forri <- us_2080_ls[[s]]
  RI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  RI$ri[(100*(ind-1)+1):(100*ind)] <- forri$RI
}
RI$sim <- factor(RI$sim, levels = simnames)
us2080.box.ri <- ggplot(RI, aes(x=sim, y=ri, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("RI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ri

########## MCR
MCR <- matrix(NA,nrow=100*4,ncol=2)
MCR <- data.frame(MCR)
names(MCR) <- c("sim","mcr")
for(s in simnames){
  ind <- which(simnames==s)
  formcr <- us_2080_ls[[s]]
  MCR$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  MCR$mcr[(100*(ind-1)+1):(100*ind)] <- formcr$MCR
}
MCR$sim <- factor(MCR$sim, levels = simnames)
us2080.box.mcr <- ggplot(MCR, aes(x=sim, y=mcr, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("MCR")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.mcr

########## JI
JI <- matrix(NA,nrow=100*4,ncol=2)
JI <- data.frame(JI)
names(JI) <- c("sim","ji")
for(s in simnames){
  ind <- which(simnames==s)
  forji <- us_2080_ls[[s]]
  JI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  JI$ji[(100*(ind-1)+1):(100*ind)] <- forji$JI
}
JI$sim <- factor(JI$sim, levels = simnames)
us2080.box.ji <- ggplot(JI, aes(x=sim, y=ji, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("JI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ji

########## Parameter estimation
us2080.par <- list()
for(s in simnames){
  ind <- which(simnames==s)
  forpar <- us_2080_ls[[s]][,c(1:8,18:19)]
  ## need to order the first 4 columns because classes may flipped
  for(r in seq_along(forpar[,1])){
    row <- forpar[r,]
    ord <- c(order(row[3:4]),order(row[3:4])+2,5:10)
    forpar[r,] <- row[ord]
  }
  us2080.par[[ind]] <- na.omit(forpar)
}
names(us2080.par) <- paste0(simnames,".par")

###########################################################
#### summary for us_0595
us_0595_ls <- list(us_sssr_0595,us_ssmr_0595,us_mssr_0595,us_msmr_0595)
names(us_0595_ls) <- simnames
####### Plot the number of times that BIC/ICL selected 2-component model
us0595.bic.select <- c(sum(us_sssr_0595$bic.select==2),sum(us_ssmr_0595$bic.select==2),sum(us_mssr_0595$bic.select==2),sum(us_msmr_0595$bic.select==2))
us0595.icl.select <- c(sum(us_sssr_0595$icl.select==2),sum(us_ssmr_0595$icl.select==2),sum(us_mssr_0595$icl.select==2),sum(us_msmr_0595$icl.select==2))

####### Check the validation measures: ARI, RI, MCR, JI, FMI. All measures are calculated regardless of whether correct number of component was selected. 
########## ARI
ARI <- matrix(NA,nrow=100*4,ncol=2)
ARI <- data.frame(ARI)
names(ARI) <- c("sim","ari")
for(s in simnames){
  ind <- which(simnames==s)
  forari <- us_0595_ls[[s]]
  ARI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  ARI$ari[(100*(ind-1)+1):(100*ind)] <- forari$ARI
}
ARI$sim <- factor(ARI$sim, levels = simnames)
# boxplot(ARI, use.cols = TRUE, main = "ARI for each simulation", ylab = "ARI")
us0595.box.ari <- ggplot(ARI, aes(x=sim, y=ari, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("ARI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ari

########## RI
RI <- matrix(NA,nrow=100*4,ncol=2)
RI <- data.frame(RI)
names(RI) <- c("sim","ri")
for(s in simnames){
  ind <- which(simnames==s)
  forri <- us_0595_ls[[s]]
  RI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  RI$ri[(100*(ind-1)+1):(100*ind)] <- forri$RI
}
RI$sim <- factor(RI$sim, levels = simnames)
us0595.box.ri <- ggplot(RI, aes(x=sim, y=ri, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("RI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ri

########## MCR
MCR <- matrix(NA,nrow=100*4,ncol=2)
MCR <- data.frame(MCR)
names(MCR) <- c("sim","mcr")
for(s in simnames){
  ind <- which(simnames==s)
  formcr <- us_0595_ls[[s]]
  MCR$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  MCR$mcr[(100*(ind-1)+1):(100*ind)] <- formcr$MCR
}
MCR$sim <- factor(MCR$sim, levels = simnames)
us0595.box.mcr <- ggplot(MCR, aes(x=sim, y=mcr, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("MCR")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.mcr

########## JI
JI <- matrix(NA,nrow=100*4,ncol=2)
JI <- data.frame(JI)
names(JI) <- c("sim","ji")
for(s in simnames){
  ind <- which(simnames==s)
  forji <- us_0595_ls[[s]]
  JI$sim[(100*(ind-1)+1):(100*ind)] <- rep(s,100)
  JI$ji[(100*(ind-1)+1):(100*ind)] <- forji$JI
}
JI$sim <- factor(JI$sim, levels = simnames)
us0595.box.ji <- ggplot(JI, aes(x=sim, y=ji, fill=sim)) +
  ylim(0,1)+
  geom_boxplot() + 
  xlab("")  + ylab("") + ggtitle("JI")+
  theme(axis.text.x = element_text(angle = 90))
# c3.vio.ji

########## Parameter estimation
us0595.par <- list()
for(s in simnames){
  ind <- which(simnames==s)
  forpar <- us_0595_ls[[s]][,c(1:8,18:19)]
  ## need to order the first 4 columns because classes may flipped
  for(r in seq_along(forpar[,1])){
    row <- forpar[r,]
    ord <- c(order(row[3:4]),order(row[3:4])+2,5:10)
    forpar[r,] <- row[ord]
  }
  us0595.par[[ind]] <- na.omit(forpar)
}
names(us0595.par) <- paste0(simnames,".par")

######################################################
#### Parameter estimation summary ####################
######################################################
#### ul5050
ul5050_res_sum <- matrix(nrow = 10, ncol=12)
ul5050_res_sum <- data.frame(ul5050_res_sum)

#sssr
ul5050_res_sum[,1] <- formatC(sssr_pars,digits=2,format="f")
ul5050_res_sum[,2] <- paste0(formatC(colMeans(ul5050.par$sssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul5050.par$sssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul5050.par$sssr.par[,c(1:4,8:10)],1,function(x){abs(x-sssr_pars[c(1:4,8:10)])})
ul5050_res_sum[c(1:4,8:10),3] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul5050.par$sssr.par[,1])){
  varcov <- ul5050.par$sssr.par[i,5:7]
  truevc <- sssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul5050_res_sum[6,3] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#ssmr
ul5050_res_sum[,4] <- formatC(ssmr_pars,digits=2,format="f")
ul5050_res_sum[,5] <- paste0(formatC(colMeans(ul5050.par$ssmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul5050.par$ssmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul5050.par$ssmr.par[,c(1:4,8:10)],1,function(x){abs(x-ssmr_pars[c(1:4,8:10)])})
ul5050_res_sum[c(1:4,8:10),6] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul5050.par$ssmr.par[,1])){
  varcov <- ul5050.par$ssmr.par[i,5:7]
  truevc <- ssmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul5050_res_sum[6,6] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


#mssr
ul5050_res_sum[,7] <- formatC(mssr_pars,digits=2,format="f")
ul5050_res_sum[,8] <- paste0(formatC(colMeans(ul5050.par$mssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul5050.par$mssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul5050.par$mssr.par[,c(1:4,8:10)],1,function(x){abs(x-mssr_pars[c(1:4,8:10)])})
ul5050_res_sum[c(1:4,8:10),9] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul5050.par$mssr.par[,1])){
  varcov <- ul5050.par$mssr.par[i,5:7]
  truevc <- mssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul5050_res_sum[6,9] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#msmr
ul5050_res_sum[,10] <- formatC(msmr_pars,digits=2,format="f")
ul5050_res_sum[,11] <- paste0(formatC(colMeans(ul5050.par$msmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul5050.par$msmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul5050.par$msmr.par[,c(1:4,8:10)],1,function(x){abs(x-msmr_pars[c(1:4,8:10)])})
ul5050_res_sum[c(1:4,8:10),12] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul5050.par$msmr.par[,1])){
  varcov <- ul5050.par$msmr.par[i,5:7]
  truevc <- msmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul5050_res_sum[6,12] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#### ul2080
ul2080_res_sum <- matrix(nrow = 10, ncol=12)
ul2080_res_sum <- data.frame(ul2080_res_sum)

#sssr
ul2080_res_sum[,1] <- formatC(sssr_pars,digits=2,format="f")
ul2080_res_sum[,2] <- paste0(formatC(colMeans(ul2080.par$sssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul2080.par$sssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul2080.par$sssr.par[,c(1:4,8:10)],1,function(x){abs(x-sssr_pars[c(1:4,8:10)])})
ul2080_res_sum[c(1:4,8:10),3] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul2080.par$sssr.par[,1])){
  varcov <- ul2080.par$sssr.par[i,5:7]
  truevc <- sssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul2080_res_sum[6,3] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#ssmr
ul2080_res_sum[,4] <- formatC(ssmr_pars,digits=2,format="f")
ul2080_res_sum[,5] <- paste0(formatC(colMeans(ul2080.par$ssmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul2080.par$ssmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul2080.par$ssmr.par[,c(1:4,8:10)],1,function(x){abs(x-ssmr_pars[c(1:4,8:10)])})
ul2080_res_sum[c(1:4,8:10),6] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul2080.par$ssmr.par[,1])){
  varcov <- ul2080.par$ssmr.par[i,5:7]
  truevc <- ssmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul2080_res_sum[6,6] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


#mssr
ul2080_res_sum[,7] <- formatC(mssr_pars,digits=2,format="f")
ul2080_res_sum[,8] <- paste0(formatC(colMeans(ul2080.par$mssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul2080.par$mssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul2080.par$mssr.par[,c(1:4,8:10)],1,function(x){abs(x-mssr_pars[c(1:4,8:10)])})
ul2080_res_sum[c(1:4,8:10),9] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul2080.par$mssr.par[,1])){
  varcov <- ul2080.par$mssr.par[i,5:7]
  truevc <- mssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul2080_res_sum[6,9] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#msmr
ul2080_res_sum[,10] <- formatC(msmr_pars,digits=2,format="f")
ul2080_res_sum[,11] <- paste0(formatC(colMeans(ul2080.par$msmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul2080.par$msmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul2080.par$msmr.par[,c(1:4,8:10)],1,function(x){abs(x-msmr_pars[c(1:4,8:10)])})
ul2080_res_sum[c(1:4,8:10),12] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul2080.par$msmr.par[,1])){
  varcov <- ul2080.par$msmr.par[i,5:7]
  truevc <- msmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul2080_res_sum[6,12] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#### ul0595
ul0595_res_sum <- matrix(nrow = 10, ncol=12)
ul0595_res_sum <- data.frame(ul0595_res_sum)

#sssr
ul0595_res_sum[,1] <- formatC(sssr_pars,digits=2,format="f")
ul0595_res_sum[,2] <- paste0(formatC(colMeans(ul0595.par$sssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul0595.par$sssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul0595.par$sssr.par[,c(1:4,8:10)],1,function(x){abs(x-sssr_pars[c(1:4,8:10)])})
ul0595_res_sum[c(1:4,8:10),3] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul0595.par$sssr.par[,1])){
  varcov <- ul0595.par$sssr.par[i,5:7]
  truevc <- sssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul0595_res_sum[6,3] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#ssmr
ul0595_res_sum[,4] <- formatC(ssmr_pars,digits=2,format="f")
ul0595_res_sum[,5] <- paste0(formatC(colMeans(ul0595.par$ssmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul0595.par$ssmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul0595.par$ssmr.par[,c(1:4,8:10)],1,function(x){abs(x-ssmr_pars[c(1:4,8:10)])})
ul0595_res_sum[c(1:4,8:10),6] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul0595.par$ssmr.par[,1])){
  varcov <- ul0595.par$ssmr.par[i,5:7]
  truevc <- ssmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul0595_res_sum[6,6] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


#mssr
ul0595_res_sum[,7] <- formatC(mssr_pars,digits=2,format="f")
ul0595_res_sum[,8] <- paste0(formatC(colMeans(ul0595.par$mssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul0595.par$mssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul0595.par$mssr.par[,c(1:4,8:10)],1,function(x){abs(x-mssr_pars[c(1:4,8:10)])})
ul0595_res_sum[c(1:4,8:10),9] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul0595.par$mssr.par[,1])){
  varcov <- ul0595.par$mssr.par[i,5:7]
  truevc <- mssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul0595_res_sum[6,9] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#msmr
ul0595_res_sum[,10] <- formatC(msmr_pars,digits=2,format="f")
ul0595_res_sum[,11] <- paste0(formatC(colMeans(ul0595.par$msmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(ul0595.par$msmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(ul0595.par$msmr.par[,c(1:4,8:10)],1,function(x){abs(x-msmr_pars[c(1:4,8:10)])})
ul0595_res_sum[c(1:4,8:10),12] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(ul0595.par$msmr.par[,1])){
  varcov <- ul0595.par$msmr.par[i,5:7]
  truevc <- msmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
ul0595_res_sum[6,12] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#### us5050
us5050_res_sum <- matrix(nrow = 10, ncol=12)
us5050_res_sum <- data.frame(us5050_res_sum)

#sssr
us5050_res_sum[,1] <- formatC(sssr_pars,digits=2,format="f")
us5050_res_sum[,2] <- paste0(formatC(colMeans(us5050.par$sssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us5050.par$sssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us5050.par$sssr.par[,c(1:4,8:10)],1,function(x){abs(x-sssr_pars[c(1:4,8:10)])})
us5050_res_sum[c(1:4,8:10),3] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us5050.par$sssr.par[,1])){
  varcov <- us5050.par$sssr.par[i,5:7]
  truevc <- sssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us5050_res_sum[6,3] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#ssmr
us5050_res_sum[,4] <- formatC(ssmr_pars,digits=2,format="f")
us5050_res_sum[,5] <- paste0(formatC(colMeans(us5050.par$ssmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us5050.par$ssmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us5050.par$ssmr.par[,c(1:4,8:10)],1,function(x){abs(x-ssmr_pars[c(1:4,8:10)])})
us5050_res_sum[c(1:4,8:10),6] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us5050.par$ssmr.par[,1])){
  varcov <- us5050.par$ssmr.par[i,5:7]
  truevc <- ssmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us5050_res_sum[6,6] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


#mssr
us5050_res_sum[,7] <- formatC(mssr_pars,digits=2,format="f")
us5050_res_sum[,8] <- paste0(formatC(colMeans(us5050.par$mssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us5050.par$mssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us5050.par$mssr.par[,c(1:4,8:10)],1,function(x){abs(x-mssr_pars[c(1:4,8:10)])})
us5050_res_sum[c(1:4,8:10),9] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us5050.par$mssr.par[,1])){
  varcov <- us5050.par$mssr.par[i,5:7]
  truevc <- mssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us5050_res_sum[6,9] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#msmr
us5050_res_sum[,10] <- formatC(msmr_pars,digits=2,format="f")
us5050_res_sum[,11] <- paste0(formatC(colMeans(us5050.par$msmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us5050.par$msmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us5050.par$msmr.par[,c(1:4,8:10)],1,function(x){abs(x-msmr_pars[c(1:4,8:10)])})
us5050_res_sum[c(1:4,8:10),12] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us5050.par$msmr.par[,1])){
  varcov <- us5050.par$msmr.par[i,5:7]
  truevc <- msmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us5050_res_sum[6,12] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#### us2080
us2080_res_sum <- matrix(nrow = 10, ncol=12)
us2080_res_sum <- data.frame(us2080_res_sum)

#sssr
us2080_res_sum[,1] <- formatC(sssr_pars,digits=2,format="f")
us2080_res_sum[,2] <- paste0(formatC(colMeans(us2080.par$sssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us2080.par$sssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us2080.par$sssr.par[,c(1:4,8:10)],1,function(x){abs(x-sssr_pars[c(1:4,8:10)])})
us2080_res_sum[c(1:4,8:10),3] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us2080.par$sssr.par[,1])){
  varcov <- us2080.par$sssr.par[i,5:7]
  truevc <- sssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us2080_res_sum[6,3] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#ssmr
us2080_res_sum[,4] <- formatC(ssmr_pars,digits=2,format="f")
us2080_res_sum[,5] <- paste0(formatC(colMeans(us2080.par$ssmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us2080.par$ssmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us2080.par$ssmr.par[,c(1:4,8:10)],1,function(x){abs(x-ssmr_pars[c(1:4,8:10)])})
us2080_res_sum[c(1:4,8:10),6] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us2080.par$ssmr.par[,1])){
  varcov <- us2080.par$ssmr.par[i,5:7]
  truevc <- ssmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us2080_res_sum[6,6] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


#mssr
us2080_res_sum[,7] <- formatC(mssr_pars,digits=2,format="f")
us2080_res_sum[,8] <- paste0(formatC(colMeans(us2080.par$mssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us2080.par$mssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us2080.par$mssr.par[,c(1:4,8:10)],1,function(x){abs(x-mssr_pars[c(1:4,8:10)])})
us2080_res_sum[c(1:4,8:10),9] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us2080.par$mssr.par[,1])){
  varcov <- us2080.par$mssr.par[i,5:7]
  truevc <- mssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us2080_res_sum[6,9] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#msmr
us2080_res_sum[,10] <- formatC(msmr_pars,digits=2,format="f")
us2080_res_sum[,11] <- paste0(formatC(colMeans(us2080.par$msmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us2080.par$msmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us2080.par$msmr.par[,c(1:4,8:10)],1,function(x){abs(x-msmr_pars[c(1:4,8:10)])})
us2080_res_sum[c(1:4,8:10),12] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us2080.par$msmr.par[,1])){
  varcov <- us2080.par$msmr.par[i,5:7]
  truevc <- msmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us2080_res_sum[6,12] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#### us0595
us0595_res_sum <- matrix(nrow = 10, ncol=12)
us0595_res_sum <- data.frame(us0595_res_sum)

#sssr
us0595_res_sum[,1] <- formatC(sssr_pars,digits=2,format="f")
us0595_res_sum[,2] <- paste0(formatC(colMeans(us0595.par$sssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us0595.par$sssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us0595.par$sssr.par[,c(1:4,8:10)],1,function(x){abs(x-sssr_pars[c(1:4,8:10)])})
us0595_res_sum[c(1:4,8:10),3] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us0595.par$sssr.par[,1])){
  varcov <- us0595.par$sssr.par[i,5:7]
  truevc <- sssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us0595_res_sum[6,3] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#ssmr
us0595_res_sum[,4] <- formatC(ssmr_pars,digits=2,format="f")
us0595_res_sum[,5] <- paste0(formatC(colMeans(us0595.par$ssmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us0595.par$ssmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us0595.par$ssmr.par[,c(1:4,8:10)],1,function(x){abs(x-ssmr_pars[c(1:4,8:10)])})
us0595_res_sum[c(1:4,8:10),6] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us0595.par$ssmr.par[,1])){
  varcov <- us0595.par$ssmr.par[i,5:7]
  truevc <- ssmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us0595_res_sum[6,6] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


#mssr
us0595_res_sum[,7] <- formatC(mssr_pars,digits=2,format="f")
us0595_res_sum[,8] <- paste0(formatC(colMeans(us0595.par$mssr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us0595.par$mssr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us0595.par$mssr.par[,c(1:4,8:10)],1,function(x){abs(x-mssr_pars[c(1:4,8:10)])})
us0595_res_sum[c(1:4,8:10),9] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us0595.par$mssr.par[,1])){
  varcov <- us0595.par$mssr.par[i,5:7]
  truevc <- mssr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us0595_res_sum[6,9] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")

#msmr
us0595_res_sum[,10] <- formatC(msmr_pars,digits=2,format="f")
us0595_res_sum[,11] <- paste0(formatC(colMeans(us0595.par$msmr.par,na.rm=T),digits=2,format="f")," (",formatC(apply(us0595.par$msmr.par,2,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist1 <- apply(us0595.par$msmr.par[,c(1:4,8:10)],1,function(x){abs(x-msmr_pars[c(1:4,8:10)])})
us0595_res_sum[c(1:4,8:10),12] <- paste0(formatC(rowMeans(dist1,na.rm=T),digits=2,format="f")," (",formatC(apply(dist1,1,function(x){sd(x,na.rm=T)}),digits=2,format="f"),")")

dist2 <- c()
for(i in seq_along(us0595.par$msmr.par[,1])){
  varcov <- us0595.par$msmr.par[i,5:7]
  truevc <- msmr_pars[5:7]
  dismat <- matrix(as.numeric(c(varcov[1]-truevc[1],varcov[2]-truevc[2],varcov[2]-truevc[2],varcov[3]-truevc[3])),2,2)
  dist2[i] <- norm(dismat,type="1")
}
us0595_res_sum[6,12] <- paste0(formatC(mean(dist2,na.rm=T),digits=2,format="f")," (",formatC(sd(dist2,na.rm=T),digits=2,format="f"),")")


save.image("/rprojectnb2/aging-p/yuanf/NP_Trajectories_Clustering/simulation/commonvarcov/simulation_results.RData")
