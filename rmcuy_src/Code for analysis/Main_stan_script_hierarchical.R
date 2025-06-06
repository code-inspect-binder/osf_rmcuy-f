#Run MCMC
#########################

# NOTES

# this R script runs the MCMC

# It contains following steps:
# Loads packages 
# Loads functions 
# Model specifications
# Runs all model versions in parrallel (needs 16*4 cores) 
# Save results
##########################


######
#Load packages
#####

rm(list=ls(all=TRUE))
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_cowplot())
library(parallel)
library(loo)
library(rstan)
options(mc.cores = 4)
#choose the other one if stan makes problems:
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
Sys.setenv(LOCAL_CPPFLAGS = '-march=core2 ')
library(foreach)
library(doParallel)

####
#get important functions
###+
source("stan_functions.r")


#####
#Specify aspects
#####
#with single?
include_single <- 0
#truncate overshooting or opposing direction?
direction_filter <- 0      



#name of saved results
name= paste("Stan_result_h_",include_single,"_trim",direction_filter,"final",sep="")

####
#load end process emprirical data
####
source("data_processing.R")

#turning features on or off in the result data frame:
results=expand.grid(staying_probability=c(0,1),copying_probability=c(0,1),peer_proximity=c(0,1),distance_Weighting=c(0,1),looic=NA) 


#do the stan mcmc sampling for all combinations in parallel
cores=nrow(results)
cl<-makeCluster(cores) #defines the number of cores you whant to use
registerDoParallel(cl)
clusterExport(cl,ls()) #export workspace into cluster
stan_model_list=foreach (ii = 1:nrow(results))%dopar% {
  library(rstan)
  #choose the other one if stan makes problems:
  #Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
  Sys.setenv(LOCAL_CPPFLAGS = '-march=core2 ')
  rstan_options(auto_write = TRUE)
  options(mc.cores = 4)
  
  #determine the features
  data_stan$staying_probability=results$staying_probability[ii]
  data_stan$copy_probability=results$copying_probability[ii]
  data_stan$distance_Weighting=results$distance_Weighting[ii]
  data_stan$peer_proximity=results$peer_proximity[ii]
  
  #run model:
simple_model1<-stan(model_code = with_wighting_and_stay_model,
                      data = data_stan,iter = 2000,thin=5,chains=4)

    return(simple_model1)
}
#save result
save.image(paste0(name,".RData"))


