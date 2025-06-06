#########################
# NOTES
# this R script creates table S1
##########################


# read the data and loop through individuals
a<-read.csv('c-BEAST-fullbatch-24-04.csv', sep=',', header=TRUE)

# only look at the 3-peer treatments we focus on in the paper.
#a<-subset(a, a$treatment>1)
#a<-subset(a, a$treatment<6)

# remove instances in which participants moved away from the mean social information
a$s<-ifelse(a$treatment==1, 
	(a$secondEstimate-a$firstEstimate)/(a$p1-a$firstEstimate),
	(a$secondEstimate-a$firstEstimate)/(a$meanP-a$firstEstimate))
a$s<-ifelse(a$s<0,NA,a$s)
max(a$s, na.rm=T)



newDat<-matrix(nrow=0, ncol=ncol(a))
indMat<-matrix(nrow=0, ncol=5)

Ns<-c()

for (ind in unique(a$playerNr)){

	b<-subset(a, a$playerNr==ind)

	Ns<-c(Ns, nrow(b))
	ag<-NA;gen<-NA;
	d<-subset(b, !is.na(b$age))
	ag<-d$age[1]; gen<-d$gender[1]

	b$age<-ag
	b$gender<-gen
	
	
	for (tr in 2:5){
		d<-subset(b, b$treatment==tr)
		meanS<-mean(d$s, na.rm=T)
		
		r<-c(ind, tr, meanS, ag, gen)
		indMat<-rbind(indMat, r)
	}
	
	newDat<-rbind(newDat,b)
}

Ns

newDat<-data.frame(newDat)
names(newDat)<-names(a)

indMat<-data.frame(indMat)
names(indMat)<-c('ind', 'treat', 'adjustment', 'age', 'gender')


library('lme4')
library('lmerTest')
library('multcomp')

indMat$treat<-factor(indMat$treat)
indMat$ind<-factor(indMat$ind)

m0<-lmer(adjustment ~ treat + age + gender + (1|ind), data=indMat)
round(summary(m0)$coefficients,3)
summary(glht(m0, linfct = mcp(treat = "Tukey")))

rpt(adjustment ~ treat + age + gender + (1 | ind), grname = "ind", data = indMat, datatype = "Gaussian",
    nboot = 30, npermut = 30)

###### define model ######
library('ggplot2')
library('rstan')
library('brms')

indMat$treatm<-ifelse(indMat$treat==2, 'LN', ifelse(indMat$treat==3, 'HN', ifelse(indMat$treat==4, 'HA', 'HT')))


indMat$treatm<-relevel(factor(indMat$treatm), ref='LN')

# fit the model
model1<-brm(adjustment ~ treatm + age + gender + (1| ind),family= gaussian(link='identity'), data=indMat, cores=3, chains=4, iter=2000) 

summary(model1)

# post-hoc tests to compare treatments
hypMat<-matrix(0, nrow=0, ncol=6)

hypMat<-rbind(hypMat, hypothesis(model1,"treatmHN > 0")$hypothesis[1:6])
hypMat<-rbind(hypMat, hypothesis(model1,"treatmHA > 0")$hypothesis[1:6])
hypMat<-rbind(hypMat, hypothesis(model1,"treatmHT > 0")$hypothesis[1:6])
hypMat<-rbind(hypMat, hypothesis(model1,"treatmHA - treatmHN > 0")$hypothesis[1:6])
hypMat<-rbind(hypMat, hypothesis(model1,"treatmHT - treatmHN > 0")$hypothesis[1:6])
hypMat<-rbind(hypMat, hypothesis(model1,"treatmHT - treatmHA > 0")$hypothesis[1:6])

hypMat

##################### frequentist (fast) alternatives ###########

m1<-lmer(adjustment ~ treat + age + gender + (1|ind), data=subset(indMat, indMat$treatment>1))
summary(m1)
summary(glht(m1, linfct = mcp(treat = "Tukey")))





b<-subset(newDat, newDat$treatment==1)
b$s<-(b$secondEstimate-b$firstEstimate)/(b$p1-b$firstEstimate)
mean(b$s, na.rm=TRUE)

newDat<-subset(newDat, newDat$treatment>1)
newDat<-subset(newDat, newDat$treatment<6)

newDat$move<-sign(newDat$s)
newDat$ind<-factor(newDat$playerNr)
newDat$treatment<-factor(newDat$treatment)
m2<-glmer(move~treatment + age + gender + (1|ind), family='binomial', data=newDat)
round(summary(m2)$coefficients,3)

summary(m2)
summary(glht(m2, linfct = mcp(treatment = "Tukey")))


b<-subset(newDat, newDat$move==1)
m3<-lmer(s~treatment + age + gender + (1|ind), data=b)
summary(m3)
summary(glht(m3, linfct = mcp(treatment = "Tukey")))

