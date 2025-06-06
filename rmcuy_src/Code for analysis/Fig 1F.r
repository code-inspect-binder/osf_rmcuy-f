#########################
# NOTES
# this R script creates figure 2A-D
# Important: please load 'fake_data_list' into the environmetn before runing this script.
# 'fake_data_list' contains empirical and simulated data and is created in "Model.analysis.R" 
##########################

# load data including 10x sampled 
data=fake_data_list[[14]] #maybe not the best name (it's with simulated data)

load('data/best_model.RData')

# define adjustments (shifts from first to second estimate)
data$S<-as.numeric(as.character(data$S))

# define simulated adjustments
data$S_p<-(data$secondEstimate_p - data$firstEstimate) / (data$meanP - data$firstEstimate)

# check whether things make sense
hist(data$S_p, breaks=500, xlim=c(-1,2))

# calculate inaccuracy in first estimates
data$inaccuracy<-abs(data$firstEstimate - data$nAnimals)
plot.new()

maxPer<-c()

# data set with all values (recalculated S to avoid the divides by 0)
a<-matrix(nrow=0, ncol=ncol(data))

# create individual data set
indMat<-matrix(nrow=0, ncol=7)

# loop through all participants
for (ind in unique(data$playerNr)){
	# take a subset
	b<-subset(data, data$playerNr==ind)
	maxPer<-max(b$period)
	# if the participant has completed all rounds
	if (maxPer==40){
		# create an empty array to store the by-treatment means
		Svals<-rep(NA,6)
		# loop through all treatments
		for (treat in 1:6){
			d<-subset(b, b$treatment==treat)
			# sort the rounds
			d<-d[order(d$period),]
			# loop through all rounds of this treatment
			for (i in 1:nrow(d)){
				# define X for treatment 1, and the other treatments
				if (treat==1) {X<- d$p1[i]}else {X<-d$meanP[i]}
				
				# for tracking the S values
				E1<-d$firstEstimate[i]
				E2<-d$secondEstimate[i]
				
				S_thisRound<-(E2 - E1) / (X - E1)
				
				###### remove outliers #######
				###### this does NOT affect the boxplots, but it DOES strongly affect the model #####
				### now I cap between -1 and 2, which sounds reasonable to me ###
	#			S_thisRound<-max(0,S_thisRound)
	#			S_thisRound<-min(1,S_thisRound)
				
				#### we could also just kick these data points out
				#if (S_thisRound>1 || S_thisRound<0) S_thisRound<-NA
				if (S_thisRound<0) S_thisRound<-NA
				##############################
				
				# the S value if there was no division by 0
				if (X!=E1) d$S[i]<- as.numeric(as.character(S_thisRound))
				
			}
			# add the data to the new table
			a<-rbind(a, d)
			# save the individual-level stats (mean S per treatment)
			Svals[treat]<-mean(d$S, na.rm=TRUE)
		}
		
		# add this to the larger tables
		indRow<-c(ind, Svals)
		indMat<-rbind(indMat, indRow)
	}
}


# give some names to the individual-level dataset (will be also matched with the questionnaires
indMat<-data.frame(indMat)
names(indMat)<-c('playerNr', 'S1', 'S2', 'S3', 'S4', 'S5', 'S6')
indMat

# for the boxplots, we need to reshape a bit, with different rows for the different treatments
indMatDecomposed<-matrix(nrow=0, ncol=3)
for (i in 1:nrow(indMat)){
	for (j in 2:7){
		indMatDecomposed<-rbind(indMatDecomposed, c(indMat[i,1], j-1, indMat[i,j]))
	}
}
indMatDecomposed<-data.frame(indMatDecomposed)
names(indMatDecomposed)<-c('playerNr', 'treatment', 'meanS')
indMatDecomposed


# plot distributions of S
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, yaxs='i', xaxs='i')
plot(0, type='n', ylim=c(0.3,5), xlim=c(-0.02,1), axes=FALSE,xlab='',ylab='')
axis(1, at=0:5/5, labels=FALSE)
#axis(2, labels=FALSE, at=c(-10,10))
cols<-c("#6780d8","#ac9c3d","#8750a6","#56ae6c","#b84c7d","grey80")

for (k in 2:5){
	k1<-6-k
	x<-indMat[,k+1]
	
	
	#individual data points
	for (j in 1:nrow(indMat)){
		x1<-k1-0.2+runif(1)*0.4
		points(indMat[j,k+1],x1, pch=16, col=cols[k])
	}
	
	
	# summary stats per treatment
	y<-mean(x, na.rm=T)
	
	w<-summary(x)
	IQR<-w[5]-w[2]
	se<- sd(x)/sqrt(length(x))	
	#interquartile
	rect(w[2],k1-0.3,w[5],k1+0.3,col=adjustcolor(cols[k], alpha=0.5))
	arrows(w[2]-1.5*IQR,k1,w[2],k1, code=0)
	arrows(w[5],k1,w[5]+1.5*IQR,k1, code=0)
	
	#median
	arrows(w[3], k1-0.3, w[3], k1+0.3, lwd=5, code=0)
	

	# alternative way of plotting mean +/- SE
#	rect(y-se,k1-0.3,y+se,k1+0.3,col=adjustcolor('black',alpha=0.3))
#	arrows(y,k1-0.3,y,k1+0.3,col='black', lwd=3, lty=2, code=0)
	
	
	# simulation results
	
	b<-subset(data, data$treatment==k)
	x<-b$S_p
	y_sim<-median(x, na.rm=TRUE)
	se_sim<- sd(x)/sqrt(length(x))
	
	#plot SE (very small, not visible)
#	arrows(y_sim-se_sim,k1,y_sim+se_sim,k1, lwd=4, code=0, col='red')
	# plot mean
#	points(y_sim,k1, col='red', pch=18, cex=2)

	arrows(y_sim,k1-0.3,y_sim,k1+0.3, col='red', pch=18, lwd=3, lty=1, code=0)

	#mean+/- SE of actual data (put here so that the simulation line does not occlude some of this)

#	arrows(y-se,k1,y+se,k1, lwd=4, code=0)
#	points(y,k1, pch=15, cex=2)



	
}

