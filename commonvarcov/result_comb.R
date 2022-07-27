rm(list=ls())

### read in results sssr*6
ul_sssr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_sssr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_sssr_5050 <- rbind(ul_sssr_5050,readin)
}

ul_sssr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_sssr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_sssr_2080 <- rbind(ul_sssr_2080,readin)
}


ul_sssr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_sssr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_sssr_0595 <- rbind(ul_sssr_0595,readin)
}

us_sssr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_sssr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_sssr_5050 <- rbind(us_sssr_5050,readin)
}

us_sssr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_sssr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_sssr_2080 <- rbind(us_sssr_2080,readin)
}


us_sssr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_sssr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_sssr_0595 <- rbind(us_sssr_0595,readin)
}

### read in results ssmr*6
ul_ssmr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_ssmr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_ssmr_5050 <- rbind(ul_ssmr_5050,readin)
}

ul_ssmr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_ssmr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_ssmr_2080 <- rbind(ul_ssmr_2080,readin)
}


ul_ssmr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_ssmr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_ssmr_0595 <- rbind(ul_ssmr_0595,readin)
}

us_ssmr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_ssmr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_ssmr_5050 <- rbind(us_ssmr_5050,readin)
}

us_ssmr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_ssmr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_ssmr_2080 <- rbind(us_ssmr_2080,readin)
}


us_ssmr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_ssmr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_ssmr_0595 <- rbind(us_ssmr_0595,readin)
}

### read in results mssr*6
ul_mssr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_mssr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_mssr_5050 <- rbind(ul_mssr_5050,readin)
}

ul_mssr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_mssr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_mssr_2080 <- rbind(ul_mssr_2080,readin)
}


ul_mssr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_mssr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_mssr_0595 <- rbind(ul_mssr_0595,readin)
}

us_mssr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_mssr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_mssr_5050 <- rbind(us_mssr_5050,readin)
}

us_mssr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_mssr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_mssr_2080 <- rbind(us_mssr_2080,readin)
}


us_mssr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_mssr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_mssr_0595 <- rbind(us_mssr_0595,readin)
}

### read in results msmr*6
ul_msmr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_msmr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_msmr_5050 <- rbind(ul_msmr_5050,readin)
}

ul_msmr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_msmr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_msmr_2080 <- rbind(ul_msmr_2080,readin)
}


ul_msmr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/ul_msmr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  ul_msmr_0595 <- rbind(ul_msmr_0595,readin)
}

us_msmr_5050 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_msmr_5050/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_msmr_5050 <- rbind(us_msmr_5050,readin)
}

us_msmr_2080 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_msmr_2080/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_msmr_2080 <- rbind(us_msmr_2080,readin)
}


us_msmr_0595 <- NULL
for(procid in 1:100){
  filename = paste0("./commonvarcov/us_msmr_0595/simulation_",procid,".csv")
  readin <- read.csv(filename,row.names = 1)
  us_msmr_0595 <- rbind(us_msmr_0595,readin)
}


rm(readin,filename,procid)
