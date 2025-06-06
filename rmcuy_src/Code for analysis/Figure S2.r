#########################
# NOTES
# this R script creates figure S2
# Important: please load 'fake_data_list' into the environmetn before runing this script.
# 'fake_data_list' contains empirical and simulated data and is created in "Model.analysis.R" 
##########################

# load data including 10x sampled 
data=fake_data_list[[14]] #maybe not the best name (it's with simulated data)

data$S<-as.numeric(as.character(data$S))

data$S_p<-(data$secondEstimate_p - data$firstEstimate) / (data$meanP - data$firstEstimate)

data$emulTreat<-NA;


data$case<-NA;

data$inaccuracy<-abs(data$firstEstimate - data$nAnimals)
plot.new()

treatNames<-c('1 Peer', 'Low variance, No skew (LN)', 'High variance, No skew (HN)', 'High variance, skew Away from E1 (HA)', 'High variance, skew Towards E1 (HT)')

treatNames<-rep('',10)

maxPer<-c()

# data set with all values (recalculated S to avoid the divides by 0)
a<-matrix(nrow=0, ncol=ncol(data))

# individual data set
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
				S_thisRound<-max(-1,S_thisRound)
				S_thisRound<-min(2,S_thisRound)
				
				#### we could also just kick these data points out
				if (S_thisRound==-1 || S_thisRound==2) S_thisRound<-NA
				##############################
				
				# the S value if there was no division by 0
				if (X!=E1) d$S[i]<- S_thisRound
				
			}
			# add the data to the new table
			a<-rbind(a, d)
			# save the individual-level stats (mean S per treatment)
			Svals[treat]<-mean(d$S, na.rm=TRUE)
		}

			d<-subset(b, b$treatment==7)
			# sort the rounds
			d<-d[order(d$period),]	
			for (i in 1:nrow(d)){
				if (d$animalName[i]=='ant') d$emulTreat[i]<-3;
				if (d$animalName[i]=='bee') d$emulTreat[i]<-5;
				if (d$animalName[i]=='flamingo') d$emulTreat[i]<-2;
				if (d$animalName[i]=='cricket') d$emulTreat[i]<-4;
				if (d$animalName[i]=='crane') d$emulTreat[i]<-6;
			}			
			d<-subset(d, d$firstEstimate>0) 
			a<-rbind(a, d)
		# add this to the larger tables
		indRow<-c(ind, Svals)
		indMat<-rbind(indMat, indRow)
	}
}


freqsCases<-matrix(0, nrow=6,ncol=9);
freqsCasesNoIndivInfo<-matrix(0, nrow=6,ncol=9);


newDat<-as.data.frame(matrix(nrow=sum(a$treatment %in% 1:7 ), ncol=ncol(data)+1))
counter=0
for (treat in 2:5){
	b<-subset(a, a$emulTreat==treat)

	for (i in 1:nrow(b)){
	  counter=counter+1
	  
		d<-b[i,]
		case<-1; # indicator for what case we have
		case_p<-1;
		
		caseIndiv<-1;
		x<-d$secondEstimate;
		x_p<-d$secondEstimate_p;
		
		m<-d$meanP
		if (d$treatment==1) {
			m<-d$p1
			if (d$firstEstimate < m){ # social info was on average higher
				if (x < d$firstEstimate) case<-1;
				if (x == d$firstEstimate) case<-2;
				if (x > d$firstEstimate && x < d$p1) case<-3;
				if (x==d$p1) case<-4
				if (x > d$p1) case<-9
			}
			if (d$firstEstimate > m){
				if (x > d$firstEstimate) case<-1;
				if (x == d$firstEstimate) case<-2;
				if (x < d$firstEstimate && x > d$p1) case<-3;
				if (x==d$p1) case<-4
				if (x < d$p1) case<-9		
			}
		}
		else{
			if (d$firstEstimate < m){ # social info was on average higher
				if (x < d$firstEstimate) case<-1;
				if (x == d$firstEstimate) case<-2;
				if (x > d$firstEstimate && x < d$p1) case<-3;
				if (x==d$p1) case<-4
				if (x > d$p1 && x < d$p2) case<-5
				if (x==d$p2) case<-6
				if (x > d$p2 && x < d$p3) case<-7
				if (x == d$p3) case<-8
				if (x > d$p3) case<-9
				
				if (x_p < d$firstEstimate) case_p<-1;
				if (x_p == d$firstEstimate) case_p<-2;
				if (x_p > d$firstEstimate && x_p < d$p1) case_p<-3;
				if (x_p==d$p1) case_p<-4
				if (x_p > d$p1 && x_p < d$p2) case_p<-5
				if (x_p==d$p2) case_p<-6
				if (x_p > d$p2 && x_p < d$p3) case_p<-7
				if (x_p == d$p3) case_p<-8
				if (x_p > d$p3) case_p<-9
	

				
				
			}
			if (d$firstEstimate > m){
				if (x > d$firstEstimate) case<-1;
				if (x == d$firstEstimate) case<-2;
				if (x < d$firstEstimate && x > d$p3) case<-3;
				if (x==d$p3) case<-4
				if (x < d$p3 && x > d$p2) case<-5
				if (x==d$p2) case<-6
				if (x < d$p2 && x > d$p1) case<-7
				if (x == d$p1) case<-8
				if (x < d$p1) case<-9		
				
				if (x_p > d$firstEstimate) case_p<-1;
				if (x_p == d$firstEstimate) case_p<-2;
				if (x_p < d$firstEstimate && x_p > d$p3) case_p<-3;
				if (x_p==d$p3) case_p<-4
				if (x_p < d$p3 && x_p > d$p2) case_p<-5
				if (x_p==d$p2) case_p<-6
				if (x_p < d$p2 && x_p > d$p1) case_p<-7
				if (x_p == d$p1) case_p<-8
				if (x_p < d$p1) case_p<-9					
			}
		}
		
		if (treat<7) freqsCases[treat,case]<-freqsCases[treat,case]+1;
		if (treat==7){
			emulTreat<-d$emulTreat;
			freqsCasesNoIndivInfo[emulTreat,case]<-freqsCasesNoIndivInfo[emulTreat,case]+1;
		}
		
		d$case<-case;
		d$case_p<-case_p;
		#newDat<-rbind(newDat,d) #  old
		newDat[counter,]=d
		}
}
names(newDat)= names(d)

# calculate relative distances
newDat$d1<-ifelse(newDat$firstEstimate<newDat$meanP, abs(newDat$p1 - newDat$firstEstimate), abs(newDat$p3 - newDat$firstEstimate))/newDat$firstEstimate
newDat$d2<-abs(newDat$p2 - newDat$firstEstimate)/newDat$firstEstimate
newDat$d3<-ifelse(newDat$firstEstimate>newDat$meanP, abs(newDat$p1 - newDat$firstEstimate), abs(newDat$p3 - newDat$firstEstimate))/newDat$firstEstimate

#treatment 1 does not have p2 and p3 (only one peer is shown)
newDat$d1<-ifelse(newDat$treatment==1, abs(newDat$p1 - newDat$firstEstimate)/newDat$firstEstimate, newDat$d1)
newDat$d2<-ifelse(newDat$treatment==1, NA, newDat$d2)
newDat$d3<-ifelse(newDat$treatment==1, NA, newDat$d3)

head(newDat)


newDat$e2<-abs(newDat$secondEstimate - newDat$firstEstimate)/newDat$firstEstimate
newDat$e2_p<-abs(newDat$secondEstimate_p - newDat$firstEstimate)/newDat$firstEstimate

freqsCases
freqsCasesNoIndivInfo


# create a matrix that stores the mean relative distances between the pieces of social information
distMat<-matrix(0, nrow=6, ncol=3)

for (treat in 1:6){
	a<-subset(newDat, newDat$emulTreat==treat)
	distMat[treat,1]<-mean(a$d1, na.rm=TRUE)
	if (treat>1){
		distMat[treat,2]<-mean(a$d2, na.rm=TRUE)
		distMat[treat,3]<-mean(a$d3, na.rm=TRUE)
	}
}
distMat

include_model=FALSE;

plot.new()
cols<-c("grey80","#56ae6c","#b0913b","#b0913b","#8960b3","#8960b3","#ba495c","#ba495c","grey80")
par(las=1,lend=1, cex.axis=1.5, cex.lab=1.5, mgp=c(1,1,0))
widthAtDots<-0.0077

for (treat in 2:5){
	# draw plot
#	par(plt=c(0.1, 0.95, 0.1+0.9*(treat-1)/5, 0.1+0.9*(treat-0.35)/5), new=T)
#	plot(0, type='n', xlim=c(-0.0, 0.45), ylim=c(0,0.3), xlab='', ylab='', axes=FALSE, yaxs='i')

	par(plt=c(0.1, 0.95, 0.95 - 0.9*(treat-1.35)/4, 0.95-0.9*(treat-2)/4), new=T)
	plot(0, type='n', xlim=c(-0.0, 0.45), ylim=c(-0.025,0.5), xlab='', ylab='', axes=FALSE, yaxs='i')

	rect(-1, 0.3, 0.5, 0.5, col='grey95', border=FALSE)

	# add title
#	title(main=treatNames[treat], cex=1.5, line=0.3)
#	axis(1, at=0:5/10, labels=FALSE)
#	if (treat==1)axis(1, at=0:4/10)

	arrows(-0.05,0,0.5,0, code=0,lwd=1)
	for (i in 0:10/10) arrows(i,-0.03,i,0,code=0,lwd=1)
	


	axis(2, at=0:6/20, labels=FALSE)
	axis(2, at=0:3/10)
#	for (i in 0:10/10) arrows(i,-1,i,10, col='grey80', code=0)

	# arrow showing the mean peer info across all treatments
#	arrows(0.2,-1,0.2,10, col='black', code=0)
	
	# show the line with the dots

	ypos<-0#(treat-1)*1.6;
#	if (treat>1) ypos<-(treat-1)*1.6
	
	a<-subset(newDat, newDat$emulTreat==treat)
	f<-rep(0,9)
	
	copys<-rep(0,4)
	copys[1]<-length(which(a$case==2))
	copys[2]<-length(which(a$case==4))
	if (treat>1){
		copys[3]<-length(which(a$case==6))
		copys[4]<-length(which(a$case==8))
	}
	ranges<-matrix(nrow=0, ncol=50)
	
	bins<-c()

	# calculate the distributions in the the 'ranges' (between peer info)
	for (ca in c(3,5,7)){
		b<-subset(a, a$case==ca)
		f[ca]<-nrow(b)
		
		# record the distribution within the range
		# make the number of bins dependent on the range size
		if (ca==3) rangeSize<-distMat[treat,1]
		if (ca==5) rangeSize<-distMat[treat,2]-distMat[treat,1]
		if (ca==7) rangeSize<-distMat[treat,3]-distMat[treat,2]
		numberOfBins<- round((rangeSize-widthAtDots)*60)
		bins<-c(bins, numberOfBins)
		localF<-rep(0,50)

		for (i in 1:nrow(b)){
			if (ca==3){
				x<- b$e2[i]/b$d1[i]
			}
			if (treat>1){
				if (ca==5){
					x<- (b$e2[i]-b$d1[i])/(b$d2[i]-b$d1[i])
				}
				if (ca==7){
					x<- (b$e2[i]-b$d2[i])/(b$d3[i]-b$d2[i])
				}
			}
			#xx<-1+round(x*(numberOfBins-1))
			xx<-1+floor(x*(numberOfBins))
			localF[xx]<-localF[xx]+1;
		}
		localF
		ranges<-rbind(ranges, localF)
	}
	ranges

	N<-sum(ranges)+sum(copys); 
	n<-N
		
	# for the same for the model-generated data [apologies for repetitiveness]
	
	f_p<-rep(0,9)
	
	copys_p<-rep(0,4)
	copys_p[1]<-length(which(a$case_p==2))
	copys_p[2]<-length(which(a$case_p==4))
	if (treat>1){
		copys_p[3]<-length(which(a$case_p==6))
		copys_p[4]<-length(which(a$case_p==8))
	}
	ranges_p<-matrix(nrow=0, ncol=50)
	bins_p<-c()
		
	for (ca in c(3,5,7)){
		b<-subset(a, a$case_p==ca)
		f_p[ca]<-nrow(b)
		
		# record the distribution within the range
		# make the number of bins dependent on the range size
		if (ca==3) rangeSize<-distMat[treat,1]
		if (ca==5) rangeSize<-distMat[treat,2]-distMat[treat,1]
		if (ca==7) rangeSize<-distMat[treat,3]-distMat[treat,2]
		numberOfBins<- round((rangeSize-widthAtDots)*60)
		bins_p<-c(bins_p, numberOfBins)
		localF<-rep(0,50)

		for (i in 1:nrow(b)){
			if (ca==3){
				x<- b$e2_p[i]/b$d1[i]
			}
			if (treat>1){
				if (ca==5){
					x<- (b$e2_p[i]-b$d1[i])/(b$d2[i]-b$d1[i])
				}
				if (ca==7){
					x<- (b$e2_p[i]-b$d2[i])/(b$d3[i]-b$d2[i])
				}
			}
			#xx<-1+round(x*(numberOfBins-1))
			xx<-1+floor(x*(numberOfBins))
			localF[xx]<-localF[xx]+1;
		}
		localF
		ranges_p<-rbind(ranges_p, localF)	
	}				
	ranges_p
	
	N_p<-sum(ranges_p)+sum(copys_p)
	n_p<-N_p
	
	
	# START THE ACTUAL PLOTTING
	
	# OWN FIRST ESTIMATE (CASE 2)
	x1<--widthAtDots; x2<-widthAtDots;
	y1<-ypos; y2<-ypos+copys[1]/n;
	rect(x1,y1,x2,y2, col=cols[2])
	#model prediction
	if (treat>1& include_model==T) points(0,ypos+copys_p[1]/n, pch=18, col='red')
	
	# SHIFT TOWARDS CLOSEST PEER
	xmin<-widthAtDots; xmax<-distMat[treat,1]-widthAtDots
	r<-xmax-xmin;
	for (j in 1:bins[1]){
		x1<-xmin+r*(j-1)/(bins[1]); x2<-xmin+r*j/(bins[1]);
		y1<-ypos; y2<-ypos+ranges[1, j]/n
		rect(x1,y1,x2,y2, col='white')
		rect(x1,y1,x2,y2, col=adjustcolor(cols[3], alpha=0.5))
		#model prediction
		
		if (treat>1 & include_model==T) points(xmin+r*(j-0.5)/bins[1],ypos+ranges_p[1,j]/n_p, pch=18, col='red')
	}
	
	# COPY CLOSEST PEER
	x1<-distMat[treat,1]-widthAtDots; x2<-distMat[treat,1]+widthAtDots;
	y1<-ypos; y2<-ypos+copys[2]/n;
	rect(x1,y1,x2,y2, col=cols[4])
	#model prediction
	if (treat>1  & include_model==T) points(distMat[treat,1],ypos+copys_p[2]/n_p, pch=18, col='red')	
	
	if (treat>1){

		# COPY MIDDLE PEER
		x1<-distMat[treat,2]-widthAtDots; x2<-distMat[treat,2]+widthAtDots;
		y1<-ypos; y2<-ypos+copys[3]/n;
		rect(x1,y1,x2,y2, col=cols[6])
		#model prediction
		if(include_model==T) points(distMat[treat,2],ypos+copys_p[3]/n_p, pch=18, col='red')	
		
		# COPY FARTHEST PEER
		x1<-distMat[treat,3]-widthAtDots; x2<-distMat[treat,3]+widthAtDots;
		y1<-ypos; y2<-ypos+copys[4]/n;
		rect(x1,y1,x2,y2, col=cols[8])
		#model prediction	
		if(include_model==T)		points(distMat[treat,3],ypos+copys_p[4]/n_p, pch=18, col='red')	
		
		# SHIFT TOWARDS MIDDLE PEER
		xmin<-distMat[treat,1]+widthAtDots; xmax<-distMat[treat,2]-widthAtDots
		r<-xmax-xmin;
		for (j in 1:bins[2]){
			x1<-xmin+r*(j-1)/(bins[2]); x2<-xmin+r*j/(bins[2]);
			y1<-ypos; y2<-ypos+ranges[2, j]/n
			rect(x1,y1,x2,y2, col='white')
			rect(x1,y1,x2,y2, col=adjustcolor(cols[5], alpha=0.5))
			#model prediction
			
			if(include_model==T)points(xmin+r*(j-0.5)/(bins[2]),ypos+ranges_p[2,j]/n_p, pch=18, col='red')
		}
		

		# SHIFT TOWARDS FARTHEST PEER
		xmin<-distMat[treat,2]+widthAtDots; xmax<-distMat[treat,3]-widthAtDots
		r<-xmax-xmin;
		for (j in 1:bins[3]){
			x1<-xmin+r*(j-1)/(bins[3]); x2<-xmin+r*j/(bins[3]);
			y1<-ypos; y2<-ypos+ranges[3, j]/n
			rect(x1,y1,x2,y2, col='white')
			rect(x1,y1,x2,y2, col=adjustcolor(cols[7], alpha=0.5))
			#model prediction
		if(include_model==T)	points(xmin+r*(j-0.5)/(bins[3]),ypos+ranges_p[3,j]/n_p, pch=18, col='red')
		}
	}
	
	# add the positions of the peers
#	points(0 ,ypos+0.0175, bg='dodgerblue', pch=21, cex=2, lwd=2)
#	points(distMat[treat,1],ypos+0.0175, bg='firebrick3', pch=21, cex=2, lwd=2)
	if (treat>1) {
#		points(distMat[treat,2],ypos+0.015, bg='firebrick3', pch=21, cex=2, lwd=2)
#		points(distMat[treat,3],ypos+0.015, bg='firebrick3', pch=21, cex=2, lwd=2)
	}
	
	peerPoss<-distMat[treat,]
	m<-sum(peerPoss)/4
#	arrows(m,0,m,1,col='red',lty=2, lwd=3)

	rect(distMat[treat,1]-0.005,ypos-0.02,distMat[treat,1]+0.005,ypos+0.02, col='firebrick3', lwd=2)
	if (treat>1) {
		rect(distMat[treat,2]-0.005,ypos-0.02,distMat[treat,2]+0.005,ypos+0.02, col='firebrick3', lwd=2)
		rect(distMat[treat,3]-0.005,ypos-0.02,distMat[treat,3]+0.005,ypos+0.02, col='firebrick3', lwd=2)
	}

	rect(-0.005,ypos-0.02,0.005,ypos+0.02, col='firebrick3', lwd=2)
		
#	box()

# summarize relative adjustments as a boxplot
# WITH PERSONAL INFO
	su<-summMat[treat-1,]
	rect(su[2], 0.37, su[5], 0.43, col=adjustcolor('blue', alpha=0.4), lwd=2)
	arrows(su[3], 0.37, su[3], 0.43, code=0, lwd=3)
	IQR<-su[5]-su[2]
	arrows(su[2]-1.5*IQR,0.4,su[2],0.4, code=0)
	arrows(su[5],0.4,su[5]+1.5*IQR,0.4, code=0)



# WITHOUT PERSONAL INFO
	
#	su<-summary(a$e2)
#	rect(su[2], 0.33, su[5], 0.37, col=adjustcolor('blue', alpha=0.5), lwd=2)
#	arrows(su[3], 0.33, su[3], 0.37, code=0, lwd=3)
#	IQR<-su[5]-su[2]
#	arrows(su[2]-1.5*IQR,0.35,su[2],0.35, code=0)
#	arrows(su[5],0.35,su[5]+1.5*IQR,0.35, code=0)
		
	
	##### ADD FREQUENCY DISTRIBUTIONS OF QUALITATIVE CASES ####
	par(plt=c(0.65, 0.95, 0.95-0.9*(treat-1.54)/4, 0.95-0.9*(treat-1.70)/4), new=T)
	plot(0, type='n', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', axes=FALSE, yaxs='i')
	axis(1, at=0:10/10, tck=-0.1, cex.axis=1, mgp=c(3,0,0), labels=FALSE)

	alphas<-c(1,1,0.4,1,0.4,1,0.4,1,1)

	vals<-c(0, cumsum(freqsCases[treat,] / sum(freqsCases[treat,])))
	y1<-0; y2<-1
	for (j in 1:9){
		x1<-vals[j]; x2<-vals[j+1];
		rect(x1,y1,x2,y2, col='white') # erase background
		rect(x1,y1,x2,y2, col=adjustcolor(cols[j], alpha=alphas[j]))
		
		midPoint<-mean(c(x1,x2))
#		if (j==2) points(midPoint,0.1, bg='dodgerblue', pch=21, cex=1, lwd=1)
#		if (j==4||j==6||j==8) points(midPoint,0.1, bg='firebrick3', pch=21, cex=1, lwd=1)
		
	}

	par(plt=c(0.65, 0.95, 0.95-0.9*(treat-1.8)/4, 0.95-0.9*(treat-1.96)/4), new=T)
	plot(0, type='n', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', axes=FALSE, yaxs='i')
	axis(1, at=0:10/10, tck=-0.1, cex.axis=1, mgp=c(3,0,0), labels=FALSE)

	vals<-c(0, cumsum(freqsCases0[treat,] / sum(freqsCases0[treat,])))
	y1<-0; y2<-1
	for (j in 1:9){
		x1<-vals[j]; x2<-vals[j+1];
		rect(x1,y1,x2,y2, col='white') # erase background
		rect(x1,y1,x2,y2, col=adjustcolor(cols[j], alpha=alphas[j]))

		midPoint<-mean(c(x1,x2))
#		if (j==2) points(midPoint,0.1, bg='dodgerblue', pch=21, cex=1, lwd=1)
#		if (j==4||j==6||j==8) points(midPoint,0.1, bg='firebrick3', pch=21, cex=1, lwd=1)
		
	}


}

