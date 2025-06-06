#########################
# NOTES
# this R script creates table S2
##########################

# read the data and loop through individuals
a<-read.csv('c-BEAST-fullbatch-24-04.csv', sep=',', header=TRUE)

newA<-matrix(nrow=0,ncol=ncol(a))
for (ind in unique(a$playerNr)){
	b<-subset(a, a$playerNr==ind)
	b$age<-b$age[40]
	b$gender<-b$gender[40]-1
	newA<-rbind(newA,b)
}

names(newA)<-names(a)
a<-newA;

a<-subset(a, a$treatment>1)
a<-subset(a, a$treatment<6)

# calculate the qualitative 'cases'

newDat<-as.data.frame(matrix(nrow=sum(a$treatment %in% 2:5 ), ncol=ncol(a)+1))
counter=0
for (treat in 2:5){
	b<-subset(a, a$treatment==treat)

	for (i in 1:nrow(b)){
	  counter=counter+1
	  
		d<-b[i,]
		
			
		case<-1; # indicator for what case we have
		
		x<-d$secondEstimate;
		
		m<-d$meanP
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
		}
		else{
			if (x > d$firstEstimate) case<-1;
			if (x == d$firstEstimate) case<-2;
			if (x < d$firstEstimate && x > d$p3) case<-3;
			if (x==d$p3) case<-4
			if (x < d$p3 && x > d$p2) case<-5
			if (x==d$p2) case<-6
			if (x < d$p2 && x > d$p1) case<-7
			if (x == d$p1) case<-8
			if (x < d$p1) case<-9		
	
		}

		d$case<-case;
		newDat[counter,]=d
	}
}
names(newDat)= names(d)

# reduce the number of cases (pool all cases beyond 'copy nearest peer')

newDat$caseRed<-ifelse(newDat$case>4, 5, newDat$case)
newDat$caseRed<-ifelse(newDat$caseRed<2, 5, newDat$caseRed)
newDat$caseRed<-newDat$caseRed-1

newDat$caseT<-ifelse(newDat$caseRed==1, 'stay', ifelse(newDat$caseRed==2, 'compr', ifelse(newDat$caseRed==3, 'copy', 'other')))

newDat$treat<-ifelse(newDat$treatment==2, 'LN', ifelse(newDat$treatment==3, 'HN', ifelse(newDat$treatment==4, 'HA', 'HT')))


###### define model ######
library('ggplot2')
library('rstan')
library('brms')

newDat$treat<-factor(newDat$treat, 
                levels = c("LN", "HN", "HA", "HT"))
newDat$caseT<-relevel(factor(newDat$caseT), ref='stay')


model1<-brm(caseT ~ treat + age+gender+ (1| playerNr),family= categorical (link='logit'), data=newDat, cores=3, chains=4, iter=2000) 

summary(model1)
a<-marginal_effects(model1,categorical=T)[[1]]

cols<-c("#56ae6c","#b0913b","#b0913b","#8960b3","#8960b3","#ba495c","#ba495c","grey80")

par(cex.lab=1.5, cex.axis=1.5, lend=1, las=1, yaxs='i')
plot(0, type='n', xlim=c(0.5, 4.5), ylim=c(0,1.2), xlab='', ylab='', axes=FALSE)
axis(2, at=0:5/5)
axis(1, labels=FALSE, at=-1:5)
for (tr in 1:4){
	for (case in 1:4){
		x<-tr-0.25+case*0.1
		i<-(case-1)*4+tr
		y<-a$estimate__[i]
		
		pch1<-21
		alph<-1;
		if (case==2) alph<-0.5
		arrows(x,a$lower__[i],x,a$upper__[i],code=0,lwd=2)
		points(x,y, pch=pch1, bg='white', cex=2, lwd=2)
		points(x,y, pch=pch1, bg=adjustcolor(cols[case], alpha=alph), cex=2, lwd=2)
	}
}

for (i in 1:4){
	pch1<-21
	alph<-1;
	if (i==2) alph<-0.5
	y<-1.25-i/10; x<-2.5
	arrows(x, y-0.03, x, y+0.03,code=0,lwd=2)
	points(x,y, pch=pch1, bg='white', cex=2, lwd=2)
	points(x,y, pch=pch1, bg=adjustcolor(cols[i], alpha=alph), cex=2, lwd=2)	
}
