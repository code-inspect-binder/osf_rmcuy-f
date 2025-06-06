#########################
# NOTES
# this R script creates figure 4 and runs the simulation
# Important: please load 'stan_model_list' into the environment before runing this script.
# 'stan_model_list' contains the results of the MCMC analysis and is created in "Main_stan_script_hierarchical.R" 
##########################





#load msm package for truncated rnrom (rtnorm)
library("msm")

##############
#Load fitted paramters


###
#First run MCMC analysis
###

library(foreach)
library(rstan)
ind_res = get_ind_res(stan_model_list)

###
#Get the mean posterior estimates of each indivudal for the full model:

samples = rstan::extract(stan_model_list[[16]])


#transform paramters to make 0 distance and 0 prximity the intercept. Hence, the new var_social is the
# sd expected if all peers agree with you and with one another. Note that (bacause it's linear) some are negative.
samples$var_social_mu = samples$var_social_mu - c(samples$distance_weight_mu * mean(data_stan$distance)) -
  c(samples$prox_weight_mu  * mean(data_stan$proximity))



#############
# define distributions for 

# 1. STRATEGY SELECTION
# a. STAY (function of distance to nearest peer; intercept: beta distr; logistic decay - 2 parameters)
# b. COPY (function of distance to peer; intercept; logistic decay - 2 parameters)
# c. COMPROMISE (1-STAY-COPY)

# 2. WEIGHTED AVERAGING IN CASE OF COMPROMISE
# operationalization of updating: density distributions with means at estimates, SDs are free parameters
# SD self: from normal distribution (mean, s.d.)
# SD others: function of distance to self and distance to others: a + b*x + c*y (with mean, s.d. for a, b and c; so, 6 parameters for this strategy, 10 parameters in total)

# 
N<-1000;  # number of simulated agents
# define scenario (0=polarization; 1=vary distribution of n peers; 2=moving average away, for 3 levels of consistency of social information)
scenario<-1


#individal_index <- sample(nrow(paramter),N,replace=T) #sample from the populaiton of paramters

# plotting parameters
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1)


predictSecondEstimate<-function(){

	# define an individuals' updating properties
	# these will have be drawn from the posterior distributions from the hierarchical model!
	# now, there's mock values (based on some averages Alan sent previously)
	
	# Markov samples of the parent distributions
	which_sample <- sample(length(samples$stay_intercept_a),1)
	# conditional stay probability
	stay_intercept<- rbeta(1,samples$stay_intercept_a[which_sample],samples$stay_intercept_b[which_sample]);
	stay_distance<-  rnorm(1,samples$stay_dist_mu[which_sample],samples$stay_dist_sigma[which_sample]);
	# conditional copy probability
	copy_intercept<- rbeta(1,samples$copy_intercept_a[which_sample],samples$copy_intercept_b[which_sample]);
	copy_distance<-  rnorm(1,samples$copy_dist_mu[which_sample],samples$copy_dist_sigma[which_sample]);
	# compromising (Bayesian updating)
	own_sd <-         rtnorm(1,samples$var_own_mu[which_sample],samples$var_own_sigma[which_sample],lower=0.1)
	other_sd <-       rtnorm(1,samples$var_social_mu[which_sample],samples$var_social_sigma[which_sample],lower=0.1)

	distance_weight<- rnorm(1,samples$distance_weight_mu[which_sample],samples$distance_weight_sigma[which_sample])

	if (scenario==0){ # in the 'polarization' we consider the steepest and the shallowest distance weighters
		if (iteration==0){
			while (distance_weight<samples$distance_weight_mu[which_sample]) distance_weight<- rnorm(1,samples$distance_weight_mu[which_sample],samples$distance_weight_sigma[which_sample])
			}
		if (iteration==1){
			while (distance_weight>samples$distance_weight_mu[which_sample]) distance_weight<- rnorm(1,samples$distance_weight_mu[which_sample],samples$distance_weight_sigma[which_sample])
		}
	}
	prox_weight <-    rnorm(1,samples$prox_weight_mu[which_sample],samples$prox_weight_sigma[which_sample])
	



	# for now we assume that they are ordered based on distance from firstEstimate (lowest distance first)
	secondEstimate<-NA; # this will be generated based on the model parameters.

	# calculate the value of the closest peer estimate and store in 'closest'
	closest<-min(abs(si-firstEstimate))
	
	# calculate proximities of the social information to self and other social information
	proximity<-matrix(NA,nrow(si),ncol(si))  # for each peer summed distance from others 
	for (i in 1:nrow(si)) {
		proximity[i,1]<-0;
		for (j in 1:nrow(si)) proximity[i,1]<-proximity[i,1]+abs(si[i,1]-si[j,1])
	}
	proximity<-proximity/n
	
	distance<-abs(firstEstimate - si)#/length(si)*3 # distance from self

	# in Alan's model, there is normalisation first
	# how can we add this here?!
	

	# draw a random number for the selection of the adjustment strategy
	rnd<-runif(1)
	# calculate stay probability
	# what is not optimal now, is that the stay probability is not enhanced when multiple peers are close.
	# we need an additional assumption to make that happen. Otherwise all added weight will be due to compromising (which might be fine)
	stay_prob<-plogis( qlogis(stay_intercept) + stay_distance * closest)

	# calculate copy probability
	copy_prob<-plogis( qlogis(copy_intercept) + copy_distance * closest)

	if (rnd<stay_prob) secondEstimate<-firstEstimate;  # stay
	if (rnd>stay_prob & rnd<(stay_prob+copy_prob)) secondEstimate<- si[1]; # copy nearest neighbour
	if (rnd > (stay_prob+copy_prob)) {	# compromise
		x<-seq(1, 150, 1)	
	
		confirmation<- distance_weight * distance  # inverse weight of a peer
		
		#for 3 peers the same as:
		proximity<- prox_weight*3 * proximity  # inverse weight of a peer EVALUATED AS MEAN
		#for n peers the same as:
#		proximity<- prox_weight * proximity  # inverse weight of a peer (possibly /N*3)
		invWeightPeers<-other_sd+confirmation+proximity
		invWeightPeers<-confirmation+proximity
		
		# define Gaussians for updating
		# prior self (firstEstimate)
		density_self <- log(dnorm(x, firstEstimate, own_sd ))
		
		# evidence
		density_peers <- matrix(NA, nrow=0, ncol=length(x))
		invWeightPeers <- ifelse(invWeightPeers<1,invWeightPeers*0.03+1-0.03,invWeightPeers) #if sd lower then one we we don't use the exact value
		for (i in 1:length(si)) density_peers <- rbind(density_peers, log(dnorm(x, si[i], invWeightPeers[i])))
		density_all <- rep(NA, length(x))
		for (i in 1:length(x)) density_all[i]<-sum(density_peers[,i])
		
		# posterior (secondEstimate)
		post <- (density_all + density_self)
		post <-  exp( post -  (log(sum(exp(post-max(post))))+max(post)))
		secondEstimate<-sample(x, 1, prob = post)
	}
	return(secondEstimate);
}

# define scenarios
# this is where we will define some initial conditions (also based on some distributions)

firstEstimate<-50  # first estimate; will be drawn from some density distribution
minPeers<-50;
maxPeers<-65
n<-10; # number of peers

cols<-c("#56ae6c","#b0913b")


if (scenario==0){
	firstEstimate<-55;
	par(mfrow=c(5,1), mar=c(1,1,1,1), yaxs='i')
	Nconfirming<-5;
	for (iteration in 0:1){
		secondEstimate_vec <-NULL

		Ndisconfirming<-n-Nconfirming;
		si<-cbind(c(rep(minPeers,Nconfirming), rep(maxPeers, Ndisconfirming))); # social information; this will be tightly controlled and manipulated to explore implications of different scenarios		
		
		# make predictions
		for (ind in 1:N) secondEstimate_vec[ind] <-  predictSecondEstimate()

		plot(0, type='n', xlim=c(48, 67), ylim=c(-0.6,1), axes=FALSE, xlab='', ylab='')
		axis(2, at=0:5/5, labels=FALSE)
	#	for (i in 1:15*10) arrows(i,-0.05,i,0,code=0)
		x<-minPeers+(maxPeers-minPeers)/2
#		arrows(x,-0.05,x,0,code=0, lwd=3)


		rect(minPeers-1, -0.6, minPeers+1,0,col='red')
		text(minPeers, -0.3, Nconfirming, col='white', cex=3)

		rect(maxPeers-1, -0.6, maxPeers+1,0, col='red')
		text(maxPeers, -0.3, Ndisconfirming, col='white', cex=3)

		# add histogram
		x<-1:150
		y<-rep(0,150)
		for (i in x){
			y[i]<-length(which(secondEstimate_vec==i))
		}

		for (i in x){
			rect(i-0.5,0,i+0.5, y[i]/N, col=adjustcolor(cols[2], alpha=0.5))
			if (i==minPeers || i==maxPeers) rect(i-0.5,0,i+0.5, y[i]/N, col=cols[2])
			if (i==firstEstimate) rect(i-0.5,0,i+0.5, y[i]/N, col=cols[1])
			}

		# add starting point
		arrows(firstEstimate,0,firstEstimate,100, code=0)
		arrows(minPeers,0,minPeers,100, code=0)
		arrows(maxPeers,0,maxPeers,100, code=0)
		m<-mean(secondEstimate_vec)
		su<-summary(secondEstimate_vec)
		# add boxplot
		rect(su[2], 0.7, su[5], 0.9, col=adjustcolor('blue', alpha=0.5), lwd=2)
		arrows(su[3], 0.7, su[3], 0.9, code=0, lwd=3)
#		arrows(su[4], 0.66, su[4], 0.74, code=0, lwd=7)
		IQR<-su[5]-su[2]
		arrows(su[2]-1.5*IQR,0.8,su[2],0.8, code=0)
		arrows(su[5],0.8,su[5]+1.5*IQR,0.8, code=0)
	}

}

if (scenario==1){
	par(mfrow=c(5,1), mar=c(1,1,1,1))
	for (Nconfirming in c(9,5,1)){
		secondEstimate_vec <-NULL

		Ndisconfirming<-n-Nconfirming;
		si<-cbind(c(rep(minPeers,Nconfirming), rep(maxPeers, Ndisconfirming))); # social information; this will be tightly controlled and manipulated to explore implications of different scenarios		
		
		# make predictions
		for (ind in 1:N) secondEstimate_vec[ind] <-  predictSecondEstimate()

		plot(0, type='n', xlim=c(48, 67), ylim=c(-0.6,1), axes=FALSE, xlab='', ylab='')
		#	axis(1, at=0:15*10)
		axis(2, at=0:5/5)

		rect(minPeers-1, -0.6, minPeers+1, 0,col='red')
		text(minPeers, -0.3, Nconfirming, col='white', cex=3)

		rect(maxPeers-1, -0.6, maxPeers+1, 0,col='red')
		text(maxPeers, -0.3, Ndisconfirming, col='white', cex=3)

		# add histogram
		x<-1:150
		y<-rep(0,150)
		for (i in x){
			y[i]<-length(which(secondEstimate_vec==i))
		}

		for (i in x){
			rect(i-0.5,0,i+0.5, y[i]/N, col=adjustcolor(cols[2], alpha=0.5))

			if (i==minPeers || i==maxPeers) rect(i-0.5,0,i+0.5, y[i]/N, col=cols[2])
			if (i==firstEstimate) rect(i-0.5,0,i+0.5, y[i]/N, col=cols[1])
		}

		# add starting point
		arrows(firstEstimate,-0.01,firstEstimate,100, code=0)
		arrows(minPeers,-0.01,minPeers,100, code=0)
		arrows(maxPeers,-0.01,maxPeers,100, code=0)
		m<-mean(secondEstimate_vec)
		su<-summary(secondEstimate_vec)
		# add boxplot
		rect(su[2], 0.7, su[5], 0.9, col=adjustcolor('blue', alpha=0.5), lwd=2)
		arrows(su[3], 0.7, su[3], 0.9, code=0, lwd=3)
#		arrows(su[4], 0.66, su[4], 0.74, code=0, lwd=7)
		IQR<-su[5]-su[2]
		arrows(su[2]-1.5*IQR,0.8,su[2],0.8, code=0)
		arrows(su[5],0.8,su[5]+1.5*IQR,0.8, code=0)
	}
	

}

if (scenario==2){
	n<-3
	ms<-c();
	ses<-c();
	
	mat<-matrix(nrow=0, ncol=3)
#	par(mfrow=c(5,1), mar=c(2,1,1,1))
	for (deltaDistance in 1:20/40){#c(0.1, 0.2, 0.3, 0.4, 0.5)){
		for (peerProx in c(0,3,6)){
	
			absX<-round(firstEstimate * (1+deltaDistance))
			si<-cbind(c(absX + -1:1*peerProx)); # social information mean shifting away
			secondEstimate_vec <-NULL
			
			# only consider cases where all pieces of social info are on 1 side of ego, otherwise odd patterns emerge when the nearest peers 'passes over' ego as we increase the mean deviation
			if (si[1]>=firstEstimate){
				# make predictions
				for (ind in 1:N) {
		#			firstEstimate<-round(rnorm(1,mean=50,sd=1))
					E2<-predictSecondEstimate();

					secondEstimate_vec[ind] <- E2
					mat<-rbind(mat, cbind(deltaDistance, peerProx, (E2-firstEstimate)/(mean(absX)-firstEstimate)))#(E2-firstEstimate)/meanDistance))

				}
				ms<-c(ms, mean((secondEstimate_vec-50)/50))
			}
		}
	}

	plot(0, type='n', xlim=c(0,0.5), ylim=c(0,1), xlab='Mean deviation', ylab='Mean relative adjustment', axes=F)
	axis(1)
	axis(2)
	for (i in unique(mat[,1])){
		b<-subset(mat, mat[,1]==i)
		pchCnt<-0;
		for (j in unique(b[,2])){
			d<-subset(b, b[,2]==j)
			m<-mean(d[,3])
			s<-sd(d[,3])/sqrt(nrow(d))
			
	#		arrows(i,m-s,i,m+s,code=0, col=adjustcolor('blue', alpha=0.5))
			points(i,m, pch=21+pchCnt, cex=2, bg=adjustcolor("#6780d8", alpha=0.6), lwd=2)
			pchCnt<-pchCnt+1;
		}
		
	}
}


