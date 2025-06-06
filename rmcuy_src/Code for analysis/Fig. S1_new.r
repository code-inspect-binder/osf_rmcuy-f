#########################
# NOTES
# this R script creates figure S1
##########################

#load empricial data
data<-read.csv('c-BEAST-fullbatch-24-04.csv', sep=',', header=TRUE)
data$S<-NA;

data$inaccuracy<-abs(data$firstEstimate - data$nAnimals)
plot.new()

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
				S_thisRound<-max(0,S_thisRound)
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
		
		# add this to the larger tables
		indRow<-c(ind, Svals)
		indMat<-rbind(indMat, indRow)
	}
}

dev.off()
# give some names to the individual-level dataset (will be also matched with the questionnaires : - )
indMat<-data.frame(indMat)
names(indMat)<-c('playerNr', 'S1', 'S2', 'S3', 'S4', 'S5', 'S6')
indMat

cols<-c("#6780d8","#ac9c3d","#8750a6","#56ae6c","#b84c7d","grey50")
plot.new()
par(cex.lab=1.5, cex.axis=1.5, las=1,lend=1)
for (i in 2:5){
	for (j in 2:5){
		j1<-j-1;
#		if (j>2) j1<-j-2
		i1<-i-1
		par(plt=c( 0.1+0.9*(i1-1)/4, 0.1+0.9*(i1-0.1)/4, 0.1+0.9*(j1-1)/4, 0.1+0.9*(j1-0.1)/4 ), new=TRUE)
		
		
		x1<-indMat[,i];
		x2<-indMat[,j+1];

		if (j==2 && i!=2){		
			plot(0, type='n', xlim=c(-0.5, 1.5), ylim=c(0, 0.3), xlab='', ylab='', axes=FALSE, yaxs='i', xaxs='i')
			axis(1, at=c(-1:3/2), labels=FALSE)
			axis(2, at=0:5/10, labels=FALSE)
			rect(-2,0,0,3, col='grey90', border=FALSE)
			rect(1,0,2,3, col='grey90', border=FALSE)
			
			### draw histogram ###
			f1<-rep(0,21); f2<-rep(0,21)
			for (k in 1:length(x1)){
				r<- 6 + round(x1[k] * 10)
				f1[r]<-f1[r]+1;
			}
			f1<-f1/sum(f1)
			for (k in 1:21){
				xx1<- (k-6)/10 - 0.04; xx2<-(k-6)/10 + 0.04;
				rect(xx1,0,xx2,f1[k], col=cols[i-1])
			}
						
		}
		if (i==2 && j>2){
			plot(0, type='n', ylim=c(-0.5, 1.5), xlim=c(0, 0.3), xlab='', ylab='', axes=FALSE, yaxs='i', xaxs='i')
			axis(2, at=c(-1:3/2), labels=FALSE)
			axis(1, at=0:5/10, labels=FALSE)
			rect(-2,-2,2,0, col='grey90', border=FALSE)
			rect(-2,1,2,3, col='grey90', border=FALSE)
			### draw histogram ###
			f1<-rep(0,21); f2<-rep(0,21)
			for (k in 1:length(x2)){
				r<- 6 + round(x2[k] * 10)
				f1[r]<-f1[r]+1;
			}
			f1<-f1/sum(f1)
			for (k in 1:21){
				xx1<- (k-6)/10 - 0.04; xx2<-(k-6)/10 + 0.04;
				rect(0,xx1,f1[k],xx2, col=cols[j])
			}
									
		}
		
		# correlations
		if (j>2 && i > 2 && i<(j+1)){
			plot(0, type='n', xlim=c(-0.5, 1.5), ylim=c(-0.5, 1.5), xlab='', ylab='', axes=FALSE, yaxs='i', xaxs='i')
			
			axis(1, at=c(-1:3/2), labels=FALSE)
			axis(2, at=-3:3/2, labels=FALSE)			

			arrows(-2,-2,2,2, code=0)
			
			
			for (k in 1:length(x1)){
			
				pchcol<-adjustcolor(cols[1], alpha=0.6)
				points(x1[k], x2[k], col=pchcol, pch=16, cex=0.7)
				
			}
			
			m1<-cor.test(x1,x2)
			h<-n1higher/length(x1)
			h<-round(h,2)
			
			l<-n2higher/length(x1)
			l<-round(m1$estimate,2)
			
			text(0, 1.3, paste('r=', sprintf("%.2f", l), sep=''), cex=1.2)
#			text(1.1, -0.2, sprintf("%.2f", h))
		}
		if (!(i==2 && j==2)) box()
	}
}
