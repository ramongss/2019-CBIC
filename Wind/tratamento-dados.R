## Set working directory
setwd("~/Doc/Wind")

library(tidyr)
library(readxl)

#######################Tratamento dos dados####################
## Load dataset
data_wind <- read_excel("DATA_SET.xlsx")

# Separate date and time 
data_wind <- separate(data = data_wind, 
                      col = PCTimeStamp, 
                      into = c("Date", "Time"), sep = " ")

# Separate day into YYYY-MM-DD 
data_wind <- separate(data = data_wind, 
                      col = Date, 
                      into = c("Year","Month","Day"), sep = "-")

# Separate time into hour:minute:second
data_wind <- separate(data = data_wind, 
                      col = Time, 
                      into = c("Hour","Minute","Second"), sep = ":")

data_wind$Second <- NULL
data_wind$Year   <- NULL
data_wind$Month  <- NULL

Days <- split(data_wind,data_wind$Day)
Power<- list()
Info <- list()
k    <- 1

for(i in 1:length(Days)){
  Hours<-split(Days[[i]],Days[[i]]$Hour)
  for(j in 1:length(Hours)){
    Info [[k]]<-cbind(i,j-1)
    Hours[[j]]<-Hours[[j]][,-c(1:3)] 
    Power[[k]]<-apply(Hours[[j]],2,mean)
    k<-k+1
  }
}

data_wind2<-do.call(rbind.data.frame,Power)
data_wind2_info<-do.call(rbind.data.frame,Info)

data_wind_final<-data.frame(data_wind2_info,data_wind2)
colnames(data_wind_final)<-c("Day","Hour",names(data_wind[,-c(1:3)]))

save.image("datawind.RData")
save(data_wind_final, file = "data.RData")