#########################
# NOTES
# this R script creates figure 2F
##########################


# read the data and loop through individuals
data<-read.table('data normalized.txt', sep='\t', header=TRUE)
data<-subset(data, data$treatment>1)
data<-subset(data, data$treatment<6)
data<-data.frame(data)
data$indMeanS<-0
newDat<-matrix(nrow=0, ncol=nrow(data)+1)

indMat<-matrix(nrow=0, ncol=10)

for (ind in unique(data$playerNr)){
	a<-subset(data, data$playerNr==ind)
	a$indMeanS<-mean(a$S)
	newDat<-rbind(newDat, a)
	
	
	f<-rep(0, 8)
	for (i in 1:nrow(a)){
		x<-a$case[i] - 1;
		if (x==0) x<-8  # group together the 'other' cases
		f[x]<-f[x]+1;
	}
	f<-f/sum(f)

	
	indMat<-rbind(indMat, c(ind, f, mean(a$S)))
}
indMat<-data.frame(indMat)
names(indMat)<-c('ind', 'c1', 'c2', 'c3','c4','c5','c6','c7', 'c8', 'S')
indMat$Sr<-ifelse(indMat$S<0.2,3,ifelse(indMat$S<0.5,2,1))
names(newDat)<-c(names(data))
newDat<-newDat[with(newDat, order(indMeanS, playerNr)), ]

ind<-1

cols<-c("#56ae6c","#b0913b","#8960b3","#b0913b","#8960b3","#b0913b","#8960b3","grey80")

cols<-c("#56ae6c","#b0913b","#b0913b","#8960b3","#8960b3","#ba495c","#ba495c","grey80")

par(xaxs='i', yaxs='i', las=1,lend=1)
plot.new()
plot(0, type='n', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
axis(1)
cnt<-1;

# sort the participants
indMat<-indMat[with(indMat, order(c1,c1+c2,c1+c2+c3,c3,c4,c5,c6)),];

for (i in 1:nrow(indMat)){

	y0<- 1- cnt/95
	y1<- 1 - (cnt-1)/95

	cf<-c(0,cumsum(as.numeric(indMat[i,2:9])))

	for (j in 1:8){
		x0<-cf[j]; x1<-cf[j+1]
		alph<-1
		if (j==2 || j==4 || j==6) alph<-0.5
		rect(x0,y0,x1,y1, col=adjustcolor(cols[j], alpha=alph))
#		if (j==3) rect(x0,y0,x1,y1, col='black', density=10, border=FALSE)
#		if (j==7) rect(x0,y0,x1,y1, col='black', density=10, border=FALSE, angle=135)
	}

#	points(indMat$S[i], (y0+y1)/2, pch=15, col='grey30')

	cnt<-cnt+1;
}
