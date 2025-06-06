
##
#This scropt contains all important functions
###


##
# Content:
#
# with_wighting_and_stay_model: stan code including the model
#
# fake_gen: A function simulating data given paramters
#           Input: paramters - a matrix containing the paramters for each individal; data2 -a data.frame with the indipendend variables and model version
#
# fake_data: a function which simulates data for all models by using the function fake_gen 
#
# plot_effects: a function ploting the model fittings. Used to generate the markdown file
#
# get_ind_res, get_mu: two funcitons extracting the indivudal and population results from the stan models respectively
#
# get_loo, get_waic: two functions which calculate the information criteria looic and waic.



# This is the used Stan code
with_wighting_and_stay_model <- "

functions{


//This is a function calculating the likelyhood of all second choices using Bayesian Inference strategy 
vector  sociallik(int firstEstimate,row_vector c_distance, row_vector c_proximity,
int p1, int p2, int p3, int N, int direction_filter,
int distance_Weighting, int peer_proximity, 
real  var_own , real var_social , real distance_weight ,
real prox_weight ){

vector[150] density_own;
vector[150] density_soial1;
vector[150] density_soial2;
vector[150] density_soial3;
vector[150] dummy;
vector[150] post;
vector[150] post_dummy;
real prob_dummy;
real sumpost;
real weight_peer1;
real weight_peer2;
real weight_peer3;
real weight_peer_prox1;
real weight_peer_prox2;
real weight_peer_prox3;
real mean_est;



if(distance_Weighting == 1) {
weight_peer1 = (distance_weight  * c_distance[1]);
weight_peer2 = (distance_weight  * c_distance[2]);
weight_peer3 = (distance_weight  * c_distance[3]);
} else {
weight_peer1 = 0;
weight_peer2 = 0;
weight_peer3 = 0;
}


if(peer_proximity == 1 && p2>0) {
weight_peer_prox1 = (prox_weight  * c_proximity[1]);
weight_peer_prox2 = (prox_weight  * c_proximity[2]);
weight_peer_prox3 = (prox_weight  * c_proximity[3]);
} else {
weight_peer_prox1 = 0;
weight_peer_prox2 = 0;
weight_peer_prox3 = 0;
}

//calculate log-likelihood for of first estimate and social informaiton
for (x_vec in 1:150){
density_own[x_vec]  = (normal_lpdf( x_vec | firstEstimate ,var_own ));

if ( (weight_peer1 + weight_peer_prox1+var_social)<=1) {
density_soial1[x_vec]  = (normal_lpdf( x_vec | p1 , (var_social + weight_peer1 + weight_peer_prox1)*0.03+1-0.03));
} else {
density_soial1[x_vec]  = (normal_lpdf( x_vec | p1 ,var_social + weight_peer1 + weight_peer_prox1));
}

if ( (weight_peer2 + weight_peer_prox2+var_social)<=1) {
density_soial2[x_vec]  = (normal_lpdf( x_vec | p2 ,(var_social + weight_peer2 + weight_peer_prox2)*0.03+1-0.03));
} else {
density_soial2[x_vec]  = (normal_lpdf( x_vec | p2 ,var_social + weight_peer2 + weight_peer_prox2));
}
if ( (weight_peer3 + weight_peer_prox3+var_social)<=1) {
density_soial3[x_vec]  = (normal_lpdf( x_vec | p3, (var_social + weight_peer3 + weight_peer_prox3)*0.03+1-0.03));
} else {
density_soial3[x_vec]  = (normal_lpdf( x_vec | p3 ,var_social + weight_peer3 + weight_peer_prox3));
}
}

if(p2==0){
density_soial2 = rep_vector(0,150);
density_soial3 = rep_vector(0,150);
}

// multiplying/deviding on a log scale via adding/subtracting
post_dummy = (density_own) + (density_soial1) + (density_soial2) + (density_soial3);
sumpost = log(sum(exp(post_dummy-max(post_dummy))))+max(post_dummy);
post = exp(post_dummy - sumpost);



for (x_vec in 1:150){
if (direction_filter==1){ // if opposing direction is not possible 
if( firstEstimate< p1 && x_vec<firstEstimate) post[x_vec]=1e-50;
if( firstEstimate> p1 && x_vec>firstEstimate) post[x_vec]=1e-50;
}
//smallest possible likelihood
if( post[x_vec]< 1e-50){
post[x_vec]=1e-50;
}
}

sumpost = sum(post);
post = post ./ sumpost;
return(post);
}
}

data{
int<lower=0> N;
int direction_filter;
int firstEstimate[N];
int secondEstimate[N];
int<lower=0> N_ID;
int ID[N];
matrix[N,3] c_distance;
matrix[N,3] c_proximity;

int p1[N];
int p2[N];
int p3[N];
real closest[N];
real pos_closest[N];
int<lower = 0, upper = 1> distance_Weighting;
int<lower = 0, upper = 1> peer_proximity;
int<lower = 0, upper = 1> staying_probability;
int<lower = 0, upper = 1> copy_probability;
}

parameters{

real<lower=1>  var_own_mu;
real<lower=1>  var_social_mu;


real<lower=0.01> var_own_sigma;
real<lower=0.01> var_social_sigma;


real<lower=0,upper=99>  var_own[N_ID];
real<lower=0,upper=99>  var_social[N_ID];



real distance_weight_raw[distance_Weighting ? N_ID : 0]; //<lower=-0.2, upper=2>
real<lower=-0.5, upper=1.5>  distance_weight_mu[distance_Weighting ? 1 : 0  ]; //
real<lower=0> distance_weight_sigma[distance_Weighting ? 1 : 0  ];


real prox_weight_raw[peer_proximity ?  N_ID : 0 ];
real<lower=-0.5, upper=1>  prox_weight_mu[peer_proximity ? 1 : 0  ];
real<lower=0> prox_weight_sigma[ peer_proximity ? 1 : 0  ];

real<lower=0, upper=1> stay_intercept[staying_probability ? N_ID : 0];
real stay_dist_raw[staying_probability ?  N_ID : 0];
real<lower=0.01> stay_intercept_a[ staying_probability ? 1 : 0 ];
real<lower=0.01> stay_intercept_b[ staying_probability ? 1 : 0  ];
real  stay_dist_mu[ staying_probability ?  1 : 0 ];
real<lower=0> stay_dist_sigma[ staying_probability ? 1 : 0  ];


real<lower=0.01> copy_intercept_a[ copy_probability ? 1 : 0 ];
real<lower=0.01> copy_intercept_b[ copy_probability ? 1 : 0  ];
real<lower=-2, upper=1>  copy_dist_mu[ copy_probability ? 1 : 0  ];
real<lower=0.01> copy_dist_sigma[ copy_probability ? 1 : 0  ];
real<lower=0, upper=0.5> copy_intercept[copy_probability ?  N_ID : 0];
real copy_dist_raw[copy_probability ?  N_ID : 0]; //<upper=0.1>

}

transformed parameters{

simplex[3] probs[N];

real prox_weight[peer_proximity ?  N_ID : 0 ];//<lower=-1, upper=1> 
real distance_weight[distance_Weighting ?  N_ID : 0 ];//<lower=-1, upper=1> 
real stay_dist[staying_probability ?  N_ID : 0 ];//<lower=-1, upper=1> 
real copy_dist[copy_probability ?  N_ID : 0 ];//<lower=-1, upper=1> 



for (n in 1:N_ID){ //go through every Decision maker

if (peer_proximity == 1){
prox_weight[n] = prox_weight_mu[1] + prox_weight_raw[n] * prox_weight_sigma[1];
}
if (distance_Weighting == 1){
distance_weight[n] = distance_weight_mu[1] + distance_weight_raw[n] * distance_weight_sigma[1];
}
if (staying_probability == 1) {
stay_dist[n] = stay_dist_mu[1] + stay_dist_raw[n] * stay_dist_sigma[1];
}
if (copy_probability == 1) {
copy_dist[n] = copy_dist_mu[1] + copy_dist_raw[n] * copy_dist_sigma[1];
}


for (i in 1:N){ //go throw every choice
if (ID[i]==n){ //if decision of focal individuals





if (staying_probability == 1) {
probs[i,1] = inv_logit(logit(stay_intercept[n]) + stay_dist[n]  * closest[i]);
}else {
probs[i,1] =0;
}

if (copy_probability == 1) {
probs[i,2] = inv_logit(logit(copy_intercept[n]) + copy_dist[n] * closest[i]);
} else {
probs[i,2] =0;
}

if (staying_probability == 1 && copy_probability == 1) {
if ((probs[i,1] + probs[i,2])>0.9) {
probs[i,1]=probs[i,1]/(probs[i,1] + probs[i,2] + 0.1);
probs[i,2]=probs[i,2]/(inv_logit(logit(stay_intercept[n]) + stay_dist[n]  * closest[i]) + probs[i,2] + 0.1);
}
}

probs[i,3] = exp(log1m(probs[i,1] + probs[i,2]));

}}}
}



model{

var_own_mu ~ normal(20, 5)T[0.0,];
var_social_mu ~ normal(20, 5)T[0.0,];
var_own_sigma ~ normal(0, 2)T[0.0,];
var_social_sigma ~ normal(0, 2)T[0.0,];


if (distance_Weighting == 1){
distance_weight_mu[distance_Weighting] ~ normal(0, 0.2);
distance_weight_sigma[distance_Weighting] ~ normal(0, 0.2)T[0.0,];
distance_weight_raw ~ std_normal();
}

if (peer_proximity == 1){
prox_weight_mu[peer_proximity] ~ normal(0, 0.2);
prox_weight_sigma[peer_proximity] ~ normal(0, 0.2)T[0.0,];
prox_weight_raw ~ std_normal();
}

if (staying_probability == 1) {
stay_dist_mu[staying_probability] ~ normal(0, 0.5);
stay_dist_sigma[staying_probability] ~ normal(0, 1)T[0.0,];
stay_intercept_a[staying_probability]  ~ normal(1, 0.4);
stay_intercept_b[staying_probability]  ~ normal(5, 2);
stay_dist_raw ~ std_normal();
}

if (copy_probability == 1) {
copy_intercept_a[copy_probability] ~ normal(1, 0.4); 
copy_intercept_b[copy_probability] ~ normal(5, 2);
copy_dist_mu[copy_probability] ~ normal(0, 0.5);
copy_dist_sigma[copy_probability] ~ normal(0, 1)T[0,];
copy_dist_raw ~ std_normal();
}

for (n in 1:N_ID){ //go through every Decision maker



var_own[n] ~ normal(var_own_mu, var_own_sigma);
var_social[n] ~ normal(var_social_mu, var_social_sigma);


if (staying_probability == 1) {
stay_intercept[n] ~ beta(stay_intercept_a,stay_intercept_b); //normal(stay_intercept_mu, stay_intercept_sigma); 
}

if (copy_probability == 1) {
copy_intercept[n] ~ beta(copy_intercept_a, copy_intercept_b); 
} 

for (i in 1:N){ //go throw every choice
if (ID[i]==n){ //if decision of focal individuals



target += log(  probs[i,1] * (secondEstimate[i] == firstEstimate[i] ? 1 : 0) +
                probs[i,2] * (secondEstimate[i] == pos_closest[i] ? 1 : 0) +
                probs[i,3] * sociallik( firstEstimate[i], 
                c_distance[i,],  c_proximity[i,], p1[i], p2[i], p3[i],  N,direction_filter,
                distance_Weighting, peer_proximity, var_own[n], var_social[n],
                (distance_Weighting ? distance_weight[n] : 0), (peer_proximity ? prox_weight[n] : 0))[secondEstimate[i]]);
                
               
            
}
}}}

generated quantities {
vector[N] log_lik;

for (n in 1:N_ID){ //go through every Decision maker


for (i in 1:N){ //go throw every choice
if (ID[i]==n){ //if decision of focal individuals


log_lik[i] = log(  probs[i,1] * (secondEstimate[i] == firstEstimate[i] ? 1 : 0) +
                probs[i,2] * (secondEstimate[i] == p1[i] ? 1 : 0) +
                probs[i,3] * sociallik( firstEstimate[i], 
                c_distance[i,],  c_proximity[i,], p1[i], p2[i], p3[i],  N,direction_filter,
                distance_Weighting, peer_proximity, var_own[n], var_social[n],
                (distance_Weighting ? distance_weight[n] : 0), (peer_proximity ? prox_weight[n] : 0))[secondEstimate[i]]);

}}}
}
"


#par=ind_res[[ix]]
#data2=data_nonfiltered
#data2$staying_probability=results$staying_probability[ix]
#data2$copying_probability=results$copying_probability[ix]
#data2$distance_Weighting=results$distance_Weighting[ix]
#data2$peer_proximity=results$peer_proximity[ix]

fake_gen <- function(parameters, data2) {
  #which versiom?
  distance_Weighting=data2$distance_Weighting[1]
  staying_probability = data2$staying_probability[1]  
  copying_probability =data2$copying_probability[1]
  peer_proximity = data2$peer_proximity[1]
  
  #get data
  firstEstimate = data2$firstEstimate
  secondEstimate = data2$secondEstimate
  p1 = data2$p1
  p2 = data2$p2
  p3 = data2$p3
  closest=data2$closest
  pos_closest=data2$pos_closest
  
  
  #Change this part
  si = cbind(data2$p1,data2$p2,data2$p3)
  proximity =matrix(NA,nrow(si),ncol(si))
  for (i in 1:ncol(si)) proximity[,i] = apply(abs(si-si[,i]),1,sum)
  distance = abs(firstEstimate - si)
  
  c_distance = distance - mean(distance)
  c_proximity =  proximity - mean(proximity)
  c_closest =  closest - mean(closest)
  
  
  out = rep(NA, length(firstEstimate))
  x = seq(0, 150, 1)
  

  for (i in 1:length(firstEstimate)) {
    
    id_index=which(unique(data2$playerNr)==data2$playerNr[i])
    par<- parameters[id_index,]
    
    
    ####Caculate densities
    if (distance_Weighting == 1) {
      weight_peer1 = (par[3] * c_distance[i,1])
      weight_peer2 = (par[3] * c_distance[i,2])
      weight_peer3 = (par[3] * c_distance[i,3])
    } else{
      weight_peer1 = weight_peer2 = weight_peer3 = 0
    }
    if (peer_proximity == 1 & p2[i]!=0) {
      
      weight_peer_prox1 = (par[6] * c_proximity[i,1])
      weight_peer_prox2 = (par[6] * c_proximity[i,2])
      weight_peer_prox3 = (par[6] * c_proximity[i,3])
    }else{
      weight_peer_prox1 = weight_peer_prox2 = weight_peer_prox3 = 0
    }
    
    if((par[2]+weight_peer1 + weight_peer_prox1)<=1) weight_peer1=0;weight_peer_prox1=0
    if((par[2]+weight_peer2 + weight_peer_prox2)<=1) weight_peer2=0;weight_peer_prox2=0
    if((par[2]+weight_peer3 + weight_peer_prox3)<=1) weight_peer3=0;weight_peer_prox3=0
    
    density_you = log(dnorm(x, firstEstimate[i], par[1]))
    
    if ((par[2] + weight_peer1 + weight_peer_prox1)<1){
    density_peer1  = log(dnorm(x, p1[i], (par[2] + weight_peer1 + weight_peer_prox1)*0.03+1-0.03))
    } else {
    density_peer1 = log(dnorm(x, p1[i], (par[2]) + weight_peer1 + weight_peer_prox1))
    }
if (0 == (p2[i])) {
      density_all = (density_peer1)
    } else{
      if ((par[2] + weight_peer2 + weight_peer_prox2)<1){
        density_peer2  = log(dnorm(x, p1[i], (par[2] + weight_peer2 + weight_peer_prox2)*0.03+1-0.03))
      } else { 
        density_peer2 = log(dnorm(x, p2[i], (par[2]) + weight_peer2 + weight_peer_prox2)) }
    if ((par[2] + weight_peer3 + weight_peer_prox3)<1){
       density_peer3  = log(dnorm(x, p1[i], (par[2] + weight_peer3 + weight_peer_prox3)*0.03+1-0.03))
    } else { 
      density_peer3 = log(dnorm(x, p3[i], (par[2]) + weight_peer3 + weight_peer_prox3))}
      density_all = (density_peer1 + density_peer2 + density_peer3)
    }
    ############
    
    
   
    
    
    post = (density_all + density_you)
    post = exp( post -  (log(sum(exp(post-max(post))))+max(post)))
    
    
    
    if(staying_probability >= 1) {
      stay_prob = plogis( qlogis(par[4]) + par[5] * (closest[i]))
      post = post * (1 - stay_prob)
      post[x == firstEstimate[i]] = post[x == firstEstimate[i]] + stay_prob
    }
    
    if(copying_probability >= 1) {
      copy_prob = plogis(qlogis(par[7]) + par[8] *closest[i])
      post[x == pos_closest[i]] = post[x == pos_closest[i]] + copy_prob
      # if(p2[i]!=0){
      #   post[x == p2[i]] = post[x == p2[i]] + pnorm(par[7] + par[8] * abs(p2[i] - firstEstimate[i]))
      #   post[x == p3[i]] = post[x == p3[i]] + pnorm(par[7] + par[8] * abs(p3[i] - firstEstimate[i]))
      post = post * (1 - copy_prob)#+
      #                       pnorm(par[7] + par[8] * abs(p2[i] - firstEstimate[i]))+
      #                       pnorm(par[7] + par[8] * abs(p3[i] - firstEstimate[i]))
      #                    ))
      # }
    }
    
    
    
    
    
    
    
    if(direction_filter==1){
      if( firstEstimate[i]< p1[i])   post[x< firstEstimate[i]]=0
      if( firstEstimate[i]> p1[i])   post[x> firstEstimate[i]]=0
    }
    if(!any(is.na(post))){
    out[i] = base::sample(x, 1, prob = post)
    } else{print("NAs")}
  }
  
  
  return(out)
}



#ind_level=ind_res[[16]]
#result=mean_result[16,]
#data2=data_stan
#f_data=fake_data_list[[16]]

plot_effects<-function(ind_level,result, data2,f_data){
  #library("cowplot")
  f_data = f_data %>%  select(treatment,firstEstimate,secondEstimate,secondEstimate_p,p1,p2,p3)%>% 
    filter(!firstEstimate==0,
           !treatment==6,
           !treatment==7) 
  
  if (include_single!=1) f_data = f_data %>% filter(!treatment==1)
  f_data = f_data %>% 
    na.omit %>%
    filter(!(firstEstimate < secondEstimate & firstEstimate > p1)) %>% 
    filter(!(firstEstimate > secondEstimate & firstEstimate < p1))  %>%
    mutate(closest=apply(abs(cbind(p1, p2, p3) - firstEstimate),1,min),
           which_closest=apply(abs(cbind(p1, p2, p3) - firstEstimate),1,which.min),
           pos_closest =ifelse(which_closest==1,p1,ifelse(which_closest==2,p2,p3)))
  
  closest=data2$closest
  #abs_closest=data2$abs_closest
  abs_prox =apply(rbind(abs(data2$p2-data2$p1),abs(data2$p2-data2$p3),abs(data2$p1-data2$p2)),2,sum)
  abs_distance=c(abs(data2$firstEstimate-data2$p1),abs(data2$firstEstimate-data2$p3),abs(data2$firstEstimate-data2$p2))*2/3
  max_distance =max(abs_distance)
  
  si = cbind(f_data$p1,f_data$p2,f_data$p3)
  f_data$proximity =matrix(NA,nrow(si),ncol(si))
  for (i in 1:ncol(si)) f_data$proximity[,i] = apply(abs(si-si[,i]),1,sum)
  f_data$distance = abs(f_data$firstEstimate - si)
  
  f_data$c_distance = f_data$distance - mean(f_data$distance)
  f_data$c_proximity =  f_data$proximity - mean(f_data$proximity)
  f_data$c_closest =  f_data$closest - mean(f_data$closest)
  
  
  dummydata=data.frame(firstEstimate=data2$firstEstimate,secondEstimate=data2$secondEstimate,closest2=data2$closest)
  dummy = dummydata %>% group_by(closest2) %>% summarise( stay=mean(firstEstimate==secondEstimate,na.rm=T),count=length(closest2))
  dummy$x=seq(min(data2$closest),max(data2$closest),length.out = length(dummy$stay))
  dummy$y=plogis(qlogis(result[4])+result[5]*dummy$x)
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_dummy2=data.frame(y=plogis(qlogis(ind_level[ind_i,4])+ind_level[ind_i,5]*dummy$x),x=dummy$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  }
  
  reps=nrow(f_data)/nrow(dummydata)
  f_dummy <-f_data %>% select(firstEstimate,secondEstimate_p,closest,pos_closest) %>%
    mutate(closest2 = closest) %>% group_by(closest2) %>% summarise(copy_f=mean(pos_closest==secondEstimate_p,na.rm=T), stay_f=mean(firstEstimate==secondEstimate_p,na.rm=T),count_f=length(closest2)/reps)
  dummy=merge(dummy, f_dummy, by.x="closest2")
  
  p1=ggplot(dummy, aes(closest2,stay))+
    geom_bar(stat = "identity", position = "dodge")+
    xlab("Distance to closest neighbour")+ylab("Stay probability")+
    theme(legend.position=c(0.012,0.9)) +
    coord_cartesian(ylim=c(-0.01,0.9))+geom_text(aes(label=count), vjust=-0.3,position = position_dodge(width = 1))+
    geom_line(aes(x,y),size=2)+
    geom_line(aes(closest2,stay_f),size=2,color="red")+
    geom_line(data=ind_dummy,aes(x,y,group=ID),size=0.1,color="gray",alpha=0.6)
  

  dummydata=data.frame(firstEstimate=data2$firstEstimate,secondEstimate= data2$secondEstimate,p1= data2$p1, p2= data2$p2, p3=data2$p3) #data.frame(firstEstimate=data2$firstEstimate,secondEstimate=data2$secondEstimate,p1=data2$p1)
  dummydata =dummydata %>% gather("who" ,"SI",p1:p3) %>% mutate(distance = abs(firstEstimate-SI))
  dummy <-dummydata %>% group_by(distance) %>% summarise( copy=mean(secondEstimate == SI ,na.rm=T),count=length(distance))
  dummy$x=seq(1,25,length.out = length(dummy$copy))
  dummy$y=plogis(qlogis(result[7])+result[8]*(dummy$x))

  f_dummy = f_data %>% mutate(distance = distance[,1])  %>% group_by(distance) %>% summarise( copy_f=mean(secondEstimate_p == pos_closest ,na.rm=T),count_f=length(distance))
  
  
  dummy=merge(dummy, f_dummy, by.x="distance")
  
  
  
  
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_dummy2=data.frame(y=plogis(qlogis(ind_level[ind_i,7])+ind_level[ind_i,8]*dummy$x),x=dummy$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  }
  
  p2=ggplot(dummy, aes(distance,copy))+
    geom_bar(stat = "identity", position = "dodge")+
    xlab("Distance to peer")+ylab("Copy probability")+
    theme(legend.position=c(0.012,0.9))+
    coord_cartesian(ylim=c(-0.01,0.5))+geom_text(aes(label=count), vjust=-0.3,position = position_dodge(width = 1))+
    geom_line(aes(distance,copy_f),size=2,color="red")+
    geom_line(data=ind_dummy,aes(x,y,group=ID),size=0.1,color="gray",alpha=0.4)+
    geom_line(aes(x,y),size=2)
  
  result=ifelse(is.na(result),0,result)
  ind_level=ifelse(is.na(ind_level),0,ind_level)
  ggplotdata=data.frame(y=c((result[2])+(result[3]*f_data$c_closest)),x=f_data$c_closest)
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_level[ind_i,]=ifelse(is.na(result),0,ind_level[ind_i,])
    ind_dummy2=data.frame(y=(ind_level[ind_i,2])+(ind_level[ind_i,3]*f_data$c_closest),x=ggplotdata$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  }
  
  shifter=mean(f_data$closest)
  p3=ggplot(ggplotdata, aes(x=x+shifter,y))+  geom_line(size=2)+xlab("Distance to myself")+ylab("Assigned Variance")+
    geom_line(data=ind_dummy,aes(x=x+shifter,y,group=ID),size=0.1,color="gray",alpha=0.4) + geom_hline(yintercept = result[1],linetype="dashed")
  
  
  prox_dummy= seq(min(f_data$c_proximity),max(f_data$c_proximity),length.out = 100)
  ggplotdata=data.frame(y=c((result[2])+(result[6]*prox_dummy)),x=prox_dummy+mean(f_data$c_proximity))
  
  ind_dummy=data.frame(y=NA,x=NA,ID=NA)
  for (ind_i in 1:length(ind_level[,1])){
    ind_level[ind_i,]=ifelse(is.na(result),0,ind_level[ind_i,])
    ind_dummy2=data.frame(y=(ind_level[ind_i,2])+ind_level[ind_i,6]*prox_dummy,x=ggplotdata$x,ID=ind_i)
    ind_dummy=rbind(ind_dummy,ind_dummy2)
  } 
  shifter=mean(f_data$proximity)
  p4=ggplot(ggplotdata, aes(x=x+shifter,y))+  geom_line(size=2)+xlab("Sum distance to Peers")+ylab("Assigned Variance") + 
    geom_line(data=ind_dummy,aes(x=x+shifter,y,group=ID),size=0.1,color="gray",alpha=0.4) +geom_hline(yintercept = result[1],linetype="dashed")
  
  return(grid.arrange(p1,p2,p3,p4))
}





































###Analysis funcitons
get_ind_res = function(stan_model_list){
  
  ind_res=foreach (ii = 1:nrow(results))%do% {
    samples = rstan::extract(stan_model_list[[ii]])
    var_own=apply(samples$var_own,2,mean,1)
    var_social=apply(samples$var_social,2,mean,1)
    distance_weight=ifelse(results$distance_Weighting[ii]==rep(1,length(var_own)), apply(samples$distance_weight,2,mean,1), NA)
    stay_intercept=ifelse(results$staying_probability[ii]==rep(1,length(var_own)), apply(samples$stay_intercept,2,mean,1), NA)
    stay_dist=ifelse(results$staying_probability[ii]==rep(1,length(var_own)), apply(samples$stay_dist,2,mean,1), NA)
    prox_weight=ifelse(results$peer_proximity[ii]==rep(1,length(var_own)), apply(samples$prox_weight,2,mean,1), NA)
    copy_intercept=ifelse(results$copying_probability[ii]==rep(1,length(var_own)), apply(samples$copy_intercept,2,mean,1), NA)
    copy_dist=ifelse(results$copying_probability[ii]==rep(1,length(var_own)), apply(samples$copy_dist,2,mean,1), NA)
    
    dummy=cbind(var_own,var_social,distance_weight,stay_intercept,stay_dist,prox_weight,copy_intercept,copy_dist)
    
    
    return(dummy)
  }
  return(ind_res)
}

get_mu = function(stan_model_list){
  #get fittings
  result=NULL
  for (ii in 1:nrow(results)){
    samples = rstan::extract(stan_model_list[[ii]])
    
    vec = c(var_own_mu=mean(samples$var_own_mu),var_own_sigma=mean(samples$var_own_sigma),
            var_social_mu=mean(samples$var_social_mu),var_social_sigma=mean(samples$var_social_sigma)) 
    
    
    index=NULL
    if(results$distance_Weighting[ii]==1) { vec=c(vec, distance_weight_mu = mean(samples$distance_weight_mu),distance_weight_sigma = mean(samples$distance_weight_sigma)) 
    } else {vec=c(vec,distance_weight_mu = NA, distance_weight_sigma = NA)} 
    if(results$peer_proximity[ii]==1){ vec=c(vec,prox_weight_mu = mean(samples$prox_weight_mu),prox_weight_sigma = mean(samples$prox_weight_sigma))
    } else {vec=c(vec,prox_weight_mu = NA,prox_weight_sigma = NA)} 
    
    if(results$staying_probability[ii]==1){
      a=samples$stay_intercept_a;b=samples$stay_intercept_b;
      vec=c(vec, stay_intercept_mu = mean(a/(a+b)), stay_intercept_sigma = mean(sqrt((a*b)/((a+b)^2*(a+b+1)))),
            stay_dist_mu = mean(samples$stay_dist_mu),stay_dist_sigma = mean(samples$stay_dist_sigma))
    } else { vec=c(vec,stay_intercept_mu = NA, stay_intercept_sigma = NA, stay_dist_mu = NA, stay_dist_sigma = NA) }
    if(results$copying_probability[ii]==1){
      a=samples$copy_intercept_a;b=samples$copy_intercept_b;
      vec=c(vec, copy_intercept_mu = mean(a/(a+b)),copy_intercept_sigma=mean(sqrt((a*b)/((a+b)^2*(a+b+1)))),
            copy_dist_mu = mean(samples$copy_dist_mu),copy_dist_sigma = mean(samples$copy_dist_sigma))
    } else { vec=c(vec,copy_intercept_mu = NA, copy_intercept_sigma = NA, copy_dist_mu = NA, copy_dist_sigma = NA) }
    result=rbind(result,vec)
    
  }
  mean_result<-cbind(result[,c(1,3,5,9,11,7,13,15)])
  return(mean_result)
}


fake_data=function(ind_res,data_nonfiltered) {
  #make fake data given the model fittings
  fake_data_list=foreach (ix = 1:nrow(results))%do% {#
    fulldata=NULL
    for(i_fake in 1:10){
      data_nonfiltered$staying_probability=results$staying_probability[ix]
      data_nonfiltered$copying_probability=results$copying_probability[ix]
      data_nonfiltered$distance_Weighting=results$distance_Weighting[ix]
      data_nonfiltered$peer_proximity=results$peer_proximity[ix]
      
      
      data<-read.csv('c-BEAST-fullbatch-24-04_update.csv', sep=';')
      #data<-read.csv('c-BEAST-fullbatch-24-04.csv', sep=',')
      
      out=fake_gen(ind_res[[ix]],data_nonfiltered) #the function making model predictions
      data$secondEstimate_p=rep(NA,length(data$secondEstimate))
      data$secondEstimate_p[data_nonfiltered$index]=round(out) #replace real with reproduced data
      
      #filter wrong direcion
      #if(direction_filter==1){
      #data = data %>% filter(!(firstEstimate < secondEstimate & firstEstimate > p1))
      #data = data %>% filter(!(firstEstimate > secondEstimate & firstEstimate < p1))
      #}
      
      fulldata=rbind(fulldata,data)
    }
    print(ix)
    return(fulldata)
  }
  return(fake_data_list)
}


get_loo = function(stan_model_list){
  #calculate looic
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  loo_res=foreach (ii = 1:nrow(results))%dopar% {
    library(loo)
    options(loo.cores = 5)
    log_lik_model<-extract_log_lik(stan_model_list[[ii]], merge_chains = FALSE)
    rel_n_eff <- relative_eff(exp(log_lik_model))
    loo_results=loo(log_lik_model, r_eff = rel_n_eff, cores = 5)
    return(loo_results)
  }
  parallel::stopCluster(cl)
  
  return(loo_res)
}


get_waic = function(stan_model_list){
  #calculate waic
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  waic_res=foreach (ii = 1:nrow(results))%dopar% {
    library(loo)
    options(loo.cores = 5)
    log_lik_model<-extract_log_lik(stan_model_list[[ii]], merge_chains = FALSE)
    rel_n_eff <- relative_eff(exp(log_lik_model))
    waic_results=waic(log_lik_model, r_eff = rel_n_eff, cores = 5)
    return(waic_results)
  }
  parallel::stopCluster(cl)
    return(waic_res)
}

extract_individual_levels<-function(model,ind_names){
  
  frame<-rstan::extract(model)
  
  plot_data=NULL
  for (p_loop in ind_names){
    dummy = data.frame(t(apply(frame[[p_loop]],2,quantile,probs=c(0.025,0.5,0.975))))
    dummy$paramter = p_loop
    dummy$ID = 1:length(dummy$paramter)
    plot_data=rbind(plot_data,dummy)
  }
  
  return(plot_data)
}

