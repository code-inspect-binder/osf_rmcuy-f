

#########################
# NOTES
# This R script runs the modeldiagnosis and makes the model related figures and tables of the manuscript 
# It contains following steps:
# Loads packages 
# Loads MCMC data if model fits have been done on the tardis cluster 
# Get further model results and run diagnosis 
# Make plots and tables
##########################


######
#Load packages
#####

library(foreach)
library(loo)
library(cowplot)
library(tidyr)
library(dplyr)
library(rstan)


#Only run if you used Tardis cluster (loads all gnerated files and stores them in a single one )
include_single=0
direction_filter=0
name= paste("Stan_result_h_",include_single,"_trim",direction_filter,"final",sep="")
stan_model_list=list()
for(i_load in 1:16)  {
  load(paste0("Server_results/",name,"/",i_load,name,".RData",sep=""))
  stan_model_list[[i_load]]<-simple_model1
}
#Otherwise start directly here:




######
# Get model results
#####

waic_res = get_waic(stan_model_list)  # get waic values for all models
loo_res = get_loo(stan_model_list)    # get loo values for all models
ind_res = get_ind_res(stan_model_list)# get posteriour mean values for all models and individuals
mean_result = get_mu(stan_model_list) # get hyperprior mean values for all models
fake_data_list=fake_data(ind_res,data_nonfiltered) #create simulated data based on model results
result=mean_result 


#name parent and individual level paramters
parent_names=c("var_own_mu","var_own_sigma","var_social_mu","var_social_sigma",
               "distance_weight_mu","distance_weight_sigma","prox_weight_mu","prox_weight_sigma",
               "stay_intercept_a","stay_intercept_b","stay_dist_mu","stay_dist_sigma",
               "copy_intercept_a","copy_intercept_b","copy_dist_mu","copy_dist_sigma","lp__")

ind_names = c('var_own','var_social','distance_weight','stay_intercept', 'stay_dist','prox_weight','copy_intercept', 'copy_dist')


#add waic and loo to result table
for (ii in 1:nrow(results))results$looic[ii]=round(loo_res[[ii]]$estimate[3,1])
for (ii in 1:nrow(results))results$waic[ii]=round(waic_res[[ii]]$estimate[3,1])

#compare all looics of the models
loos=compare( loo_res[[1]], loo_res[[2]], loo_res[[3]], loo_res[[4]], loo_res[[5]], loo_res[[6]],
         loo_res[[7]], loo_res[[8]], loo_res[[9]], loo_res[[10]], loo_res[[11]], loo_res[[12]], loo_res[[13]], loo_res[[14]], loo_res[[15]], loo_res[[16]])

#save results
save.image(paste0("Server_results/",name,".RData"))


#run markdown file with model predictions and chain diagnosis
end<-'.html'
rmarkdown::render('Stan_results_hierarchical.Rmd',
                  output_file = paste0(c('stan_results/','Analysis_check',name, Sys.Date(), 
                                         end), collapse=""))






######
# Make figures and tables
#####



#check for correlaitons between individuals
library("PerformanceAnalytics")
chart.Correlation(ind_res[[14]][,1:6]
                  , histogram=TRUE, pch=19)




#########
#Make Table S3 
########
library(gridExtra)
library(grid)
names(results) = c("Keep","Adopt","Proximity", "Confirmation","Looic")
results$Rank = rank(results$Looic)
results<-results[order(results$Rank),]
#results$Looic_diff<-round(loos[,1]*2,2)
results$elpd_diff<-round(loos[,1],2)
results$se_diff<-round(loos[,2],2)


library(gtable)
g <- tableGrob(results[,c(1:5,8,9,7)], rows = NULL)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
ggsave("TableS3.png",grid.draw(g))



#########
#Make Table S4 
########
tab=matrix( c("\\mu\\textsubscript{$\\sigma_p^2$}","\\tau\\textsubscript{$\\sigma_p^2$}",
                                            "\\mu\\textsubscript{$\\alpha_s$}","\\tau\\textsubscript{$\\alpha_s$}",
                                            "\\mu\\textsubscript{$\\beta_{confirmation}$}","\\tau\\textsubscript{$\\beta_{confirmation}$}",
                                            "\\mu\\textsubscript{$\\beta_{proximity}$}","\\tau\\textsubscript{$\\beta_{proximity}$}",
                                            "\\alpha\\textsubscript{$\\alpha'_{keep}$}","\\beta\\textsubscript{$\\alpha'_{keep}$}",
                                            "\\mu\\textsubscript{$\\beta_{keep}$}","\\tau\\textsubscript{$\\beta_{keep}$}",
                                            "\\alpha\\textsuperscript{$\\alpha'_{adopt}$}","\\beta\\textsubscript{$\\alpha'_{adopt}$}",
                                            "\\mu\\textsubscript{$\\beta_{adopt}$}","\\tau\\textsubscript{$\\beta_{adopt}$}",
               
               "N(10, 5)","N(0, 1)","N(10, 5)","N(0, 1)",
                            "N(0, 0.2)","N(0, 0.2)","N(0, 0.2)","N(0, 0.2)",
                            "N(1, 0.4)","N(5, 2)","N(0, 0.5)","N(0, 1)",
                            "N(1, 0.4)","N(5, 2)","N(0, 0.5)","N(0, 1)",
               "1.0    / Inf","0.01 / Inf","1.0    / Inf","0.01 / Inf",
                                     "-0.5  / 0.5","0.01 / Inf",
                                     "-0.5  / 0.5","0.01 / Inf",
                                     "0.01 / Inf","0.01 / Inf",
                                     "-Inf  / Inf","0.01 / Inf",
                                     "0.01 / Inf","0.01 / Inf",
                                     "-Inf  / Inf","0.01 / Inf"), ncol = 3)

samples = rstan::extract(stan_model_list[[14]])
vec = rbind(quantile(samples$var_own_mu,c(0.025,0.5,0.975)),quantile(samples$var_own_sigma,c(0.025,0.5,0.975)),
        quantile(samples$var_social_mu,c(0.025,0.5,0.975)),quantile(samples$var_social_sigma,c(0.025,0.5,0.975)),
 quantile(samples$distance_weight_mu,c(0.025,0.5,0.975)), quantile(samples$distance_weight_sigma,c(0.025,0.5,0.975)),
 quantile(samples$prox_weight_mu,c(0.025,0.5,0.975)), quantile(samples$prox_weight_sigma,c(0.025,0.5,0.975)),
quantile(samples$stay_intercept_a,c(0.025,0.5,0.975)),quantile(samples$stay_intercept_b,c(0.025,0.5,0.975)),
  quantile(samples$stay_dist_mu,c(0.025,0.5,0.975)), quantile(samples$stay_dist_sigma,c(0.025,0.5,0.975)))
vec= matrix(format(round(vec,2),digits=2,trim=T), dim(vec)[1],dim(vec)[2])

vec = cbind(vec[,2]," [",vec[,1],", ", vec[,3],"]")


library(stringr)
tab2=apply(vec,1,str_c,collapse = "")
tab=cbind(tab,c(tab2,rep("",4)))

colnames(tab)=c("Group-level parameters" , "Priors" ,"Bounds (max/min)", "Parameter estimates")

#make table
latex<-print(xtable::xtable(tab,#,caption = "Priors and bounds of group-level paramters", 
                     label = "tab:priors", align =rep('c',ncol(tab)+1)),
      sanitize.text.function=identity, 
      cline.after = c(0,1,2),
      include.rownames=FALSE,
      include.colnames=T, caption.placement = "top")


#write as Latex
writeLines(
  c(
    "\\documentclass[12pt]{article}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    latex,
    "\\end{document}"
  ),
  "priortable4.tex"
)
#Write as PDF
tools::texi2pdf("priortable4.tex", clean = TRUE)

##############################



########
#Make Figure S9
########

#get MCMC samples
sample_mat <- cbind(samples$var_own_mu,samples$var_own_sigma,samples$var_social_mu,samples$var_social_sigma,samples$distance_weight_mu,samples$distance_weight_sigma,samples$prox_weight_mu,samples$prox_weight_sigma,
                    samples$stay_intercept_mu,samples$stay_intercept_sig,samples$stay_dist_mu,samples$stay_dist_sigma,samples$copy_intercept_mu,samples$copy_intercept_sigma,samples$copy_dist_mu,samples$copy_dist_sigma)


library("PerformanceAnalytics")
chart.Correlation.myown <- function (R, histogram = TRUE, method = c("pearson", "kendall","spearman"), ...) 
{
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  panel.cor <- function(x, y, digits = 2, prefix = "", 
                        use = "pairwise.complete.obs", method = "pearson", 
                        cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y, use = use, method = method),2)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
      text(0.5, 0.5, txt, cex = 2) #constant size=2)#alternative: cex* (abs(r) + 0.3)/1.3
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, 
         axes = FALSE, main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,labels=letters,las=1,cex.axis=1.5)
}
sample_mat=data.frame(sample_mat)
letters = c(expression(mu[sigma[p]^2]),expression(tau[sigma[p]^2]),expression(mu[alpha[s]]),expression(tau[alpha[s]]),
            expression(mu[beta[confirmation]]),expression(tau[beta[confirmation]]),
            expression(mu[beta[proximity]]),expression(tau[beta[proximity]]),
            expression(mu[alpha[keep]]), expression(tau[alpha[keep]]),
            expression(mu[beta[keep]]),expression(tau[beta[keep]]))

panel.smooth =  function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)  {
  points(x, y, pch = 16, col = adjustcolor(4, .15), bg = bg, cex = cex)
}

chart.Correlation.myown(sample_mat, histogram=FALSE)
















########
#Make Table S5
########

#Get individual estimates
cor_data = ind_res[[14]][,c(1,2,3,6,4,5)]

names = c("\\sigma_p^2","\\alpha_s","\\beta_{confirmation}","\\beta_{proximity}","\\alpha_{keep}","\\beta_{keep}")

tab = round(cor(cor_data),2)
colnames(tab)= names
rownames(tab)= names

for (xx  in 1:length(names) ){
  for (xy  in 1:length(names) ){

   int =round(cor.test(cor_data[,xx],cor_data[,xy])$conf.int,2)
   if(xx!=xy){
     if( abs(round(cor(cor_data),2)[xx,xy])>0.2){
       tab[xx,xy] = paste0("\\textbf{",format(as.numeric(tab[xx,xy]),trim=T,digits=2,nsmall=2),paste0(" [",format(as.numeric(int[1]),trim=T,digits=2,nsmall=2),", ",format(as.numeric(int[2]),trim=T,digits=2,nsmall=2),"]}" ))
     }else{
            tab[xx,xy] = paste0("",format(as.numeric(tab[xx,xy]),trim=T,digits=2,nsmall=2),paste0(" [",format(as.numeric(int[1]),trim=T,digits=2,nsmall=2),", ",format(as.numeric(int[2]),trim=T,digits=2,nsmall=2),"]" ))
     }} else {
       tab[xx,xy] = format(as.numeric(tab[xx,xy]),trim=T,digits=2,nsmall=2)
       
          }
}
}
tab[upper.tri(tab, diag = F)] = ""


#make table
latex<-print(xtable::xtable(tab,#,caption = "Poirs and bounds of group-level paramters", 
                            label = "tab:correl", align =rep('c',ncol(tab)+1)),
             sanitize.text.function=identity, 
             cline.after = c(0,1,2),
             include.rownames=T,
             include.colnames=T, caption.placement = "top")


writeLines(
  c("\\documentclass[a3paper,landscape]{article}",
    "\\usepackage[landscape]{geometry}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
          latex,
    "\\end{document}"
  ),
  "tableS5.tex"
)

tools::texi2pdf("tableS5.tex", clean = TRUE)








########
#Make Figure 3B
########



jpeg(filename ="Fig3b.jpg", width = 480*1.8, height = 380*1.8,pointsize = 20)
deltas = (-c(results$Looic[results$Confirmation==1]-results$Looic[results$Confirmation==0],
             results$Looic[results$Proximity==1]-  results$Looic[results$Proximity==0],
             results$Looic[results$Keep==1]-  results$Looic[results$Keep==0],
             results$Looic[results$Adopt==1]-  results$Looic[results$Adopt==0]))

  deltas_names= c(rep("Confirmation",8),rep("Proximity",8),rep("Stay",8),rep("Copy",8))
  
  
  
  
  par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, yaxs='i',mfrow=c(1,1))#, xaxs='i'
  plot(0, type='n', ylim=c(0.3,5), xlim=c(-50,601), axes=FALSE,xlab='',ylab='')
  axis(1, at=seq(-50,601,50))#, labels=FALSE
  cols<-c("#1E90FF","#CD2626","#56ae6c","#ac9c3d","grey80")
  level_names = unique(deltas_names)
  #individual data points
  for (k in 1:4){
    k1 = rep(5-k,8)
    # summary stats per treatment
    y<-mean(deltas[deltas_names==level_names[k]], na.rm=T)
    
    w<-summary(deltas[deltas_names==level_names[k]])
    IQR<-w[5]-w[2]
    se<- sd(deltas[deltas_names==level_names[k]])/sqrt(length(deltas[deltas_names==level_names[k]]))	
    #interquartile
    rect(w[2],k1-0.3,w[5],k1+0.3,col=adjustcolor(cols[k], alpha=0.1))
    arrows(w[2]-1.5*IQR,k1,w[2],k1, code=0)
    arrows(w[5],k1,w[5]+1.5*IQR,k1, code=0)
    
    #median
    arrows(w[3], k1-0.3, w[3], k1+0.3, lwd=5, code=0)
    
    
    
    points(deltas[deltas_names==level_names[k]],k1, pch=16, col=cols[k])
    abline(v=0,  col = "lightgray",lty=2,cex=2)
    
  }
  
  dev.off()
  
  
  
  
  
  
  
  
  
  ########
  #Make Figure 3CDE
  ########
  
  
  library("cowplot")
  theme_set(theme_cowplot())
  
  library("ggplot2")
  which_model=14
  result=mean_result[which_model,]
  ind_level=ind_res[[which_model]]
  
  
  
   dummy=data.frame(x=seq(0,25,length.out = 100))
  dummy$y=plogis(qlogis(result[4])+result[5]*dummy$x)
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_dummy2=data.frame(y=plogis(qlogis(ind_level[ind_i,4])+ind_level[ind_i,5]*dummy$x),x=dummy$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  }
  
  p1=ggplot(dummy, aes(x,y))+
    xlab("Distance of first estimate\n to closest peer")+ylab("Stay probability")+
    #coord_cartesian(ylim=c(-0.01,0.9))+
    geom_line(size=2,color="#56ae6c")+
    geom_line(data=ind_dummy,aes(x,y,group=ID),size=0.1,color="#56ae6c",alpha=0.6) + 
    scale_y_continuous(breaks=seq(0,1,0.2),limits = c(0-.01,1))
  
  
  
  dummy=data.frame(x=seq(0,35,length.out = 100))
  dummy$y=plogis(qlogis(result[7])+result[8]*dummy$x)
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_dummy2=data.frame(y=plogis(qlogis(ind_level[ind_i,7])+ind_level[ind_i,8]*dummy$x),x=dummy$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  }
  
  p2=ggplot(dummy, aes(x,y))+
    xlab("Distance to peer")+ylab("Copy probability")+
    coord_cartesian(ylim=c(-0.01,0.9))+
    geom_line(size=2)+
    geom_line(data=ind_dummy,aes(x,y,group=ID),size=0.1,color="gray",alpha=0.6)
  
  
  
  result=ifelse(is.na(result),0,result)
  ind_level=ifelse(is.na(ind_level),0,ind_level)
  ggplotdata=data.frame(y=c((result[2])+(result[3]*data_stan$c_closest)),x=data_stan$c_closest)
  
  
  dummy=data.frame(x=seq(min(data_stan$c_proximity),max(data_stan$c_proximity),length.out = 100))
  dummy$y=plogis(qlogis(result[7])+result[8]*dummy$x)
  
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_level[ind_i,]=ifelse(is.na(result),0,ind_level[ind_i,])
    ind_dummy2=data.frame(y=(ind_level[ind_i,2])+(ind_level[ind_i,3]*data_stan$c_closest),x=ggplotdata$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  }
  
  shifter1=mean(data_stan$closest)
  p3=ggplot(ggplotdata, aes(x=x+shifter1,y))+  geom_line(size=2,color="#1E90FF")+xlab("Distance to myself\n")+ylab("Assigned Variance")+
    geom_line(data=ind_dummy,aes(x=x+shifter1,y,group=ID),size=0.1,color="#1E90FF",alpha=0.25) +ylim(0,17)
  
  prox_dummy= seq(min(data_stan$c_proximity),max(data_stan$c_proximity),length.out = 100)
  ggplotdata2=data.frame(y=c((result[2])+(result[6]*prox_dummy)),x=prox_dummy)
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_level[ind_i,]=ifelse(is.na(result),0,ind_level[ind_i,])
    ind_dummy2=data.frame(y=(ind_level[ind_i,2])+ind_level[ind_i,6]*prox_dummy,x=ggplotdata2$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  } 
  shifter2=mean(data_stan$proximity)
  p4=ggplot(ggplotdata2, aes(x=x+shifter2,y))+  geom_line(size=2,color="#CD2626")+xlab("Sum distance \nto other peers")+ylab("Assigned Variance") + 
    geom_line(data=ind_dummy,aes(x=x+shifter2,y,group=ID),size=0.1,color="#CD2626",alpha=0.25)+ylim(0,17)+
    xlim(0,max(ggplotdata2$x+shifter2))
  
  the_grid<-plot_grid(p1+xlab("")+ylab(""),
                      p3+xlab("")+ylab(""),
                      p4+xlab("")+ylab(""),ncol = 3)
  
  
  
  
ggsave("Fig3cde.jpg",the_grid, width = 9,height = 3,dpi = 800)
  
  
  
  
  
  





