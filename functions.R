# rm(list=ls())
#### function to generate AR(1) error term
ar1_cor <- function(d, rho) {
  exponent <- abs(matrix(1:d-1, nrow = d, ncol = d, byrow = TRUE)-(1:d-1))
  rho^exponent
}

##### Try to build a function to generate longitudinal data based on linear mixed effect model
##### currently only design for 2 linear mean profiles fix the corresponding coefficients

######### num_subj: a vector contains number of subject for each group
######### grid: a equaly distanced time grid, for this sim grid = 40+c(0:13)*4
######### space: could be "equal" or "unequal", indicating visits being equally distanced or unequally distanced 
######### follow.length: could be "long" or "short"; if space == "equal", then long is randomly picking 11 consecutive visits in full TiS, short is using random 1/d TiS; if spance == "unequal", num_visit is generated first; long is randomly sample num_visit points of visits through the full TiS (longest could be 11), short is the "flip a coin" and for num_visit <= 1/d length(TiS) randomly select a 1/d of TiS period and then select visit points. temprorally set d = 3
######### varcov: specify the random effect varcov matrix, set to be the same for all group; if need group specific varcov, need to assign a value for prop.varcov.
######### sig: standar deviation of the error term
######### prop.varcov: proportion between the two variance covairance matrix; default is NULL, which assumes the same varance-covariance matrix between the two groups
######### stoch.err: default to be NA; could also specify a number between 0 and 1 as the "rho" parameter to add on an AR(1) error

long.dat.generation <- function(num_subj, grid, space, follow.length, varcov, sig, prop.varcov = NULL, stoch.err = NULL){
  require(mvtnorm)
  # require(ggplot2)
  Age <- grid
  TiS <- grid
  forZ <- cbind(1,Age)
  
  ##### Variance-covairance matrix (D.1)
  if(!is.null(prop.varcov)){
    D.1 <- varcov
    bw_var.1 <- forZ%*%D.1%*%t(forZ) # the between subject variation component in the variance of observations
    
    D.2 <- varcov*prop.varcov
    bw_var.2 <- forZ%*%D.2%*%t(forZ) # the between subject variation component in the variance of observations
  }else{
    D.1 <- varcov
    bw_var.1 <- forZ%*%D.1%*%t(forZ) # the between subject variation component in the variance of observations
  }
    
  # if(!is.null(prop.varcov)){
  #   sigma2.1 <- sig
  #   sigma2.2 <- sig*prop.varcov
  # }else{
  #   sigma2 <- sig
  # }
  
  ### if stoch.err is not NULL, then an AR(1) error term is added, rho of the ar1_cor is stoch.err
  if(!is.null(stoch.err)){
    if(!is.null(prop.varcov)){
      sigma2.1 <- sig
      sigma2.2 <- sig*prop.varcov
      R.1 <- sigma2.1 * ar1_cor(d=length(TiS),rho=stoch.err)
      R.2 <- sigma2.1 * ar1_cor(d=length(TiS),rho=stoch.err)
      bwtot_ratio <- sum(diag(bw_var.1))/(sum(diag(bw_var.1+R.1)))
    }else{
      sigma2 <- sig
      R <- sigma2 * ar1_cor(d=length(TiS),rho=stoch.err)
      bwtot_ratio <- sum(diag(bw_var.1))/(sum(diag(bw_var.1+R)))
    }
  }else{
    if(!is.null(prop.varcov)){
      sigma2.1 <- sig
      sigma2.2 <- sig*prop.varcov
      R.1 <- diag(sigma2.1,length(TiS))
      R.2 <- diag(sigma2.2,length(TiS))
      bwtot_ratio <- sum(diag(bw_var.1))/(sum(diag(bw_var.1+R.1)))
    }else{
      sigma2 <- sig
      R <- diag(sigma2,length(TiS))
      bwtot_ratio <- sum(diag(bw_var.1))/(sum(diag(bw_var.1+R)))
    }
  }
  
  ##### True coeffcients of the mean trajectories (beta_g)
  beta_1 <- c(18,-0.15)
  beta_2 <- c(9.5,-0.05)
  
  
  ### Sample to construct fixed and random effect
  ##### specify the number of subjects (sample size, n), and the percentage of each cluster
  ##### create a fully filled matrix of n*length(TiS) containing observations created based on the prespecified parameters
  ##### select the number of visits, visists, and observation:
  ######### randomly sample a number of visit for each subject(2 - 11)
  ######### randomly select visits from TiS according to the number of visit sampled
  ######### construct the current age variable for each subject
  
  n_1 <- num_subj[1]
  n_2 <- num_subj[2]
  n <- sum(num_subj)
  
  true_lab <- c(rep(1,n_1),rep(2,n_2))
  
  ### create the observation base on the true beta, varcov structure, and assume a full follow up through TiS
  forX <- cbind(1,Age)
  if(!is.null(prop.varcov)){
    forY_1 <- rmvnorm(n_1,mean=forX%*%beta_1,sigma=bw_var.1+R.1)
    forY_2 <- rmvnorm(n_2,mean=forX%*%beta_2,sigma=bw_var.2+R.2)
  }else{
    forY_1 <- rmvnorm(n_1,mean=forX%*%beta_1,sigma=bw_var.1+R)
    forY_2 <- rmvnorm(n_2,mean=forX%*%beta_2,sigma=bw_var.1+R)
  }
  
  obs_mat <- rbind(forY_1,forY_2)
  
  ### if space = "unequal", create a number of visit vector 
  if(space == "unequal"){
    fornumvisit <- rlnorm(n, meanlog=log(3), sdlog = 0.4)
    num_visit <- ifelse(fornumvisit<=2,2,ifelse(fornumvisit>=11,11,ceiling(fornumvisit)))
  }
  
  ### generate the "true" observation, base on the condition specified
  forobsdat <- matrix(nrow=n*length(TiS),ncol=6)
  for(i in 1:n){
    id <- i
    class <- true_lab[i]
    
    if(space=="unequal"){
      numvisit <- num_visit[i]
      if(follow.length=="long"){
        obsind <- sample(c(1:length(TiS)),size=numvisit,replace=FALSE)
        # obsind <- sample(c(1:11),size=numvisit,replace=FALSE)
      }else{
        d <- 3
        # randomly select a 1/d period of the total follow-up time
        # should participants with smaller number of visits be sampled from (to get short distance bw visits)
        # whoever number of visist is <= 1/d*length(TiS) comes into the play
        period.start.ind <- sample(c(1:quantile(c(1:length(TiS)),(1-1/d))),1,replace=FALSE)
        period <- c(1:quantile(c(1:length(TiS)),1/d))-1+period.start.ind
        if(numvisit <= length(period)){
          obsind <- sample(period,size=numvisit,replace=FALSE)
        }else{
          obsind <- sample(c(1:length(TiS)),size=numvisit,replace=FALSE)
        }
      }
    }
    if(space=="equal"){
      if(follow.length=="long"){
        period.start.ind <- sample(c(1:4),1,replace=FALSE)
        period <- c(1:11)-1+period.start.ind
        obsind <- period
      }else{
        d <- 3
        period.start.ind <- sample(c(1:quantile(c(1:length(TiS)),(1-1/d))),1,replace=FALSE)
        period <- c(1:quantile(c(1:length(TiS)),1/d))-1+period.start.ind
        obsind <- period
      }
    }
    
    # print(i)
    # print(obsind)
    # tis <- rep(NA,length(TiS))
    # tis[obsind] <- TiS[obsind]
    # age <- tis+age_base
    age <- rep(NA,length(TiS))
    age[obsind] <- Age[obsind]
    forY <- rep(NA,length(TiS))
    forY[obsind] <- obs_mat[i,obsind]
    baseline_age <- head(na.omit(age),1)
    vis.num <- rep(NA,length(TiS))
    vis.num[sort(obsind)] <- order(na.omit(age))
    
    obs <- cbind(id,class,baseline_age,age,forY,vis.num)
    forobsdat[((i-1)*length(TiS)+1):(i*length(TiS)),] <- obs
  }
  colnames(forobsdat) <- c("id","class","baseline_age","age","y","numvisit")
  
  forobsdat.1 <- forobsdat[!is.na(forobsdat[,4]),]
  
  mydata <- data.frame(forobsdat.1)
  mydata$tis <- mydata$age-mydata$baseline_age
  
  return(list(dat = mydata, bwtot_ratio=bwtot_ratio, true_lab = true_lab))
}

ICL <- function(x) {
  z <- rep(0, length = length(x$pprob[,1]))
  for (g in 1:x$ng) {
    z[which(x$pprob[,2] == g)] <- x$pprob[which(x$pprob[,2] == g),(2+g)]
  }
  res <- x$BIC - 2 * sum(z)
  if (x$ng == 1) 
    res <- x$BIC
  return(res)
}

# D.1 <- matrix(c(0.01,0.001,0.001,0.001),2,2)
# sigma2 <- 1
# procid <- 1
# set.seed(20211018+procid)
# mydata <- long.dat.generation(space = "equal", follow.length = "short", varcov = D.1, sig = sigma2)$dat
# 
# trajplot.1 <- ggplot(data=mydata,aes(x=age, y=y, group=id)) +
#   geom_path(data=mydata[which(mydata$class==2),],colour="orange",na.rm = TRUE) +
#   geom_path(data=mydata[which(mydata$class==1),],colour="blue",na.rm = TRUE)
# trajplot.1
