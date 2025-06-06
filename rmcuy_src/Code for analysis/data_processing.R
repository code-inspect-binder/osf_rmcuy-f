#Pre-process data


#########################

# NOTES

# this R script pre-processes the data

# It contains following steps:
# Load empirical data# Defining the   2DSD model 
# exclude irrelevant treatments  
# calculate important variables
# Saves everything as list for stan 
##########################






#load data
data<-read.csv('c-BEAST-fullbatch-24-04.csv', sep = ";")



#get important variables
data$index = 1:nrow(data)[1]
data_dummy = data %>% select(playerNr,treatment,firstEstimate,secondEstimate,p1,p2,p3,index)


# exclude irrelevant treatments  
if (include_single==0){
  data_dummy = data_dummy %>% filter(!data_dummy$treatment==1)
} else {
  data_dummy <- data_dummy %>% mutate(p2 = ifelse(treatment==1,0,p2),p3 = ifelse(treatment==1,0,p3))
}
data_dummy = data_dummy %>% filter(!firstEstimate==0,
                                   !data_dummy$treatment==6,
                                   !data_dummy$treatment==7) 


#calculating the distance and position of closest neighbour
processed_data = data_dummy %>%  mutate(closest=apply(abs(cbind(p1, p2, p3) - firstEstimate),1,min),
                                    which_closest=apply(abs(cbind(p1, p2, p3) - firstEstimate),1,which.min),
                                    pos_closest =ifelse(which_closest==1,p1,ifelse(which_closest==2,p2,p3))) %>%
  na.omit
data_nonfiltered <- processed_data 


#filter wrong direcion if wanted (not used for manuscript)
if(direction_filter==1){
  #filter opposing direction
  processed_data = processed_data %>% filter(!(firstEstimate < secondEstimate & firstEstimate > p1))
  processed_data = processed_data %>% filter(!(firstEstimate > secondEstimate & firstEstimate < p1))
  
  #filter overcompensation (more then the most extreme)
  processed_data = processed_data %>% filter(!(p1 > secondEstimate & firstEstimate > p1))
  processed_data = processed_data %>% filter(!(secondEstimate > p3 & firstEstimate < p3))
}




#make to list for stan and calculate further variables
data_stan = processed_data %>% mutate(ID=playerNr) %>%
  select(ID, firstEstimate,secondEstimate,p1,p2,p3,index,closest,pos_closest)  %>% as.list()

dummy=data_stan$ID;counter=0
for (x in unique(data_stan$ID)){
  counter=counter+1
  dummy[data_stan$ID==x]=counter
}
data_stan$ID=dummy

data_stan$N = length(data_stan$firstEstimate) #Add number of choices
data_stan$N_ID = length(unique(data_stan$ID)) #Add number of individuals
data_stan$direction_filter = direction_filter #should the model truncate overshooting and wrong direction? (no for manuscript)

si = cbind(data_stan$p1,data_stan$p2,data_stan$p3)
data_stan$proximity =matrix(NA,nrow(si),ncol(si))
for (i in 1:ncol(si)) data_stan$proximity[,i] = apply(abs(si-si[,i]),1,sum)
data_stan$distance_matrix = abs(data_stan$firstEstimate - si)

data_stan$c_distance = data_stan$distance_matrix - mean(data_stan$distance_matrix)
data_stan$c_proximity =  data_stan$proximity - mean(data_stan$proximity)
data_stan$c_closest =  data_stan$closest - mean(data_stan$closest)

