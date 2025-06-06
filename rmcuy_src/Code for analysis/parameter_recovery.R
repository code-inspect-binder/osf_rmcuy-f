#Covery analysis


rm(list=ls(all=TRUE))
library(tidyr)
library(dplyr)
library('cowplot')
theme_set(theme_cowplot())
print(R.Version()$version.string)
library(ggplot2)
library(parallel)
library(loo)
library(rstan)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

library(foreach)
library(doParallel)

setwd("S:/Lucas Social Influence/Social Influence")
source("stan_functions4.r")

#with single?
include_single <- 0
#trim?
direction_filter <- 0      

name= paste("Stan_recovery_h_",include_single,"_trim",direction_filter,"6_oldbounds",sep="")


clusterid <- as.integer(commandArgs(TRUE)[1])
if(is.na(clusterid)){clusterid<-13}


source("data_processing.R")



#Set boundaries 
bound_var_own <- c(3,15) #other might be better but recovery fails if s small and ind big
bound_var_social <- c(7,20)
bound_distance <- c(-0.5,0.5)
bound_proximity <- c(-0.5,0.5)
bound_intercept_stay <- c(0.02,0.6)
bound_slope_stay <- c(-0.5,0.1)
bound_intercept_copy <- c(0.02,0.6)
bound_slope_copy <- c(-0.5,0.1)
dummy=rbind(bound_var_own,bound_var_social,bound_distance,bound_intercept_stay,bound_slope_stay,bound_proximity,bound_intercept_copy,bound_slope_copy)
lb<-dummy[,1]
ub<-dummy[,2]










reps=8
paramter_frame=NULL

  i=1
    paramter_frame=randtoolbox::sobol(reps,dim=length(lb),init=ifelse(i==1,T,F))
    for(col_i in 1:ncol(paramter_frame)) {
      paramter_frame[,col_i] <- lb[col_i] + (ub[col_i]-lb[col_i])*paramter_frame[,col_i]
    }
    #paramter_frame[,2]=   paramter_frame[,2] * paramter_frame[,1]


paramter_recovery=upper_quartile=lower_quartile=list(matrix(NA,reps,length(lb)),matrix(NA,reps,length(lb)),matrix(NA,reps,length(lb)))


cores=reps
cl<-makeCluster(cores) #defines the number of cores you whant to use
registerDoParallel(cl)
clusterExport(cl,ls()) #export workspace into cluster

recovery_res=foreach (reps_i = 1:reps)%dopar% {#,combine="rbind"
  library(rstan)
  rstan_options(auto_write = TRUE)
  Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7')
  chains_p = 4
  options(mc.cores = chains_p)
  
ind_recovery <- t(matrix(rep(paramter_frame[reps_i,],data_stan$N_ID),length(paramter_frame[reps_i,]),data_stan$N_ID))
ind_recovery <- ind_recovery * t(matrix(rnorm(data_stan$N_ID,mean = 1, sd = 0.1),length(paramter_frame[reps_i,]),data_stan$N_ID))

data_stan$staying_probability=1
data_stan$copying_probability=1
data_stan$copy_probability=1
data_stan$distance_Weighting=1
data_stan$peer_proximity=1


data_stan$playerNr = data_stan$ID
out=fake_gen(ind_recovery,data_stan) #the function making model predictions
data_stan$secondEstimate=NA #replace real with reproduced data
data_stan$secondEstimate=round(out) #replace real with reproduced data


simple_model1<-stan(model_code = with_wighting_and_stay_model,data = data_stan,iter = 500,thin=2,chains=chains_p, control=list(adapt_delta=0.90))


parent_names=c("var_own_mu","var_own_sigma","var_social_mu","var_social_sigma",
               "distance_weight_mu","distance_weight_sigma","prox_weight_mu","prox_weight_sigma",
               "stay_intercept_a","stay_intercept_b","stay_dist_mu","stay_dist_sigma",
               "copy_intercept_a","copy_intercept_b","copy_dist_mu","copy_dist_sigma","lp__")
#traceplot(simple_model1,pars=parent_names)


samples = rstan::extract(simple_model1)


vec = cbind(var_own_mu=c(samples$var_own_mu),
        var_social_mu=c(samples$var_social_mu)) 

vec=cbind(vec, distance_weight_mu = c(samples$distance_weight_mu)) 
vec=cbind(vec,prox_weight_mu = c(samples$prox_weight_mu))

a=samples$stay_intercept_a;b=samples$stay_intercept_b;
vec=cbind(vec, stay_intercept_mu = c(a/(a+b)),
      stay_dist_mu = c(samples$stay_dist_mu))

a=samples$copy_intercept_a;b=samples$copy_intercept_b;
vec=cbind(vec, copy_intercept_mu = c(a/(a+b)),
      copy_dist_mu = c(samples$copy_dist_mu))


print("next")
res_mean<-apply(vec,2,mean)
res_low<-apply(vec,2,quantile,probs=0.975)
res_high<-apply(vec,2,quantile,probs=0.025)

return(list(res_mean,res_low,res_high,simple_model1,ind_recovery))
}



save.image(paste0("Server_results/",name,".RData"))



whichone=4

traceplot(recovery_res[[whichone]][[4]],pars= c("var_own_mu","var_own_sigma","var_social_mu","var_social_sigma",
                                        "distance_weight_mu","distance_weight_sigma","prox_weight_mu","prox_weight_sigma",
                                        "stay_intercept_a","stay_intercept_b","stay_dist_mu","stay_dist_sigma",
                                        "copy_intercept_a","copy_intercept_b","copy_dist_mu","copy_dist_sigma","stay_intercept_mu","copy_intercept_sig","copy_intercept_mu","stay_intercept_sig","lp__"))
#pairs(recovery_res[[whichone]][[4]],pars= c("var_own_mu","var_own_sigma","var_social_mu","var_social_sigma",
#                                         "distance_weight_mu","distance_weight_sigma","prox_weight_mu","prox_weight_sigma",
#                                         "stay_intercept_a","stay_intercept_b","stay_dist_mu","stay_dist_sigma",
#                                         "copy_intercept_a","copy_intercept_b","copy_dist_mu","copy_dist_sigma","lp__"))





#sapply(recovery_res,cbind)
#sapply(sapply(recovery_res,rbind),cbind)




#dummy1 = NULL
#for (i in 1:reps) dummy1 = rbind(dummy1,recovery_res[[i]][[1]])


dummy1=sapply(recovery_res,cbind)
dummy=data.frame(do.call(rbind,dummy1[1:3,],1))




dummy$type=rep(c("Mean","up","low"),reps)
dummy$Round= sort( rep(1:reps,3))



ok = dummy  %>% gather(type2, value,-type, -Round)   %>% spread(type,value)

paramter_frame


input = data.frame(paramter_frame)
names =  c("var_own_mu","var_social_mu","distance_weight_mu","stay_intercept_mu","stay_dist_mu","prox_weight_mu","copy_intercept_mu","copy_dist_mu") #unique(ok$type2)
names(input) = c("var_own_mu","var_social_mu","distance_weight_mu","stay_intercept_mu","stay_dist_mu","prox_weight_mu","copy_intercept_mu","copy_dist_mu") #unique(ok$type2)



  
input$Round=1:reps
input = input %>% gather(type2, value,-Round)

dummy2 = merge(ok,input, by = c("type2", by="Round"))

#letters = c(expression(mu^sigma[p]^2),expression(mu^alpha[s]),expression(mu^beta[confirmation]), expression(mu^alpha[keep]),expression(mu^beta[keep]),
#                       expression(mu^beta[proximity]),expression(mu^alpha[adopt]),expression(mu^beta[adopt]))

letters = c(expression(sigma[p]^2),expression(alpha[s]),expression(beta[confirmation]), expression(alpha[keep]),expression(beta[keep]),
                       expression(beta[proximity]),expression(alpha[adopt]),expression(beta[adopt]))

r=p=NULL
for (r_index in 1:length(unique(ok$type2))) {
  r[r_index]=round(cor(dummy2$Mean[dummy2$type2==names[r_index]],dummy2$value[dummy2$type2==names[r_index]], use = "pairwise.complete.obs",method='pearson'),2)
  plot_data =dummy2[dummy2$type2==names[r_index],]
  p[[r_index]]=ggplot(plot_data, aes(value,Mean))+
    geom_point(stat = "identity",size=2)+
    geom_errorbar(aes(ymin=low, ymax=up), width=0)+
    theme(legend.position = "none",legend.background = element_rect(fill="white"),plot.title = element_text(hjust = 0.5))+
    #xlim(lb[xxx]*1.05,ub[xxx]*1.05)+ylim(lb[xxx]*1.05,ub[xxx]*1.05)+
    ggtitle(letters[r_index])+xlab("Input parameter")+ylab("Recovered parameter")+
    geom_text(label = paste("r = ",r[r_index]), x = -Inf, y = Inf, hjust = -0.3, vjust = 2,color='black',size=5)+
    geom_abline(slope=1,intercept=0)
}
the_grid<-plot_grid(p[[1]],p[[2]],p[[3]],p[[6]],p[[4]],p[[5]],p[[7]],p[[8]],labels="AUTO",nrow=2)

  
 ggsave("Recovery5.jpg",the_grid, width = 16*0.7,height = 8*0.7,dpi = 400) #save figure


  
  
 




ind_names = c('var_own','var_social','distance_weight','stay_intercept', 'stay_dist','prox_weight','copy_intercept', 'copy_dist')[1:length(unique(ok$type2))]
ind_levels<-extract_individual_levels(recovery_res[[whichone]][[4]],ind_names);


colnames(ind_levels)<-c('L','M','H','paramter','ID')
ind_levels %>% mutate(paramter = factor(paramter, levels = levels(factor(paramter))[c(7,8,3,4,6,5,2,1)] )) %>%
  ggplot(aes(x=ID,y=M))+geom_point()+
  geom_linerange(aes(ymin=L,ymax=H))+coord_flip()+
  facet_grid(.~paramter,scales  = 'free')+
  #geom_hline(data=pop_lvl,aes(yintercept=Median),color='blue',linetype='dashed')+
  labs(y='Regression parameters')




plot(ind_levels$M[ind_levels$paramter=='var_own']~recovery_res[[whichone]][[5]][,1])
abline(0,1)


