#packages#####
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(lubridate)

samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/14/DO", pattern=".csv", full.names=TRUE)

DO_14_all <- data.frame()
for(fil in file.names){
  DO14 <- read_csv(fil,skip= 1)
  DO14<-DO14[,c(2,3,4)]
  colnames(DO14)[1] <- "Date"
  colnames(DO14)[2] <- "DO"
  colnames(DO14)[3] <- "Temp"
  DO14$Date <- mdy_hms(DO14$Date)
  DO_14_all <- rbind(DO_14_all, DO14)
}

#remove erroneous
#remove hours out of water
for(i in 1:nrow(DO_14_all)){
  if(DO_14_all$DO[i]<=0 | DO_14_all$DO[i]>=7.5) { DO_14_all$DO[i]<- NA}
  else {DO_14_all$DO[i]<- DO_14_all$DO[i]-0 }}

S14<-left_join(samplingperiod, DO_14_all, by='Date')

ggplot(DO_14_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8) #check

write_csv(DO_14_all, "02_Clean_data/14/DO.csv")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/14/SpC", pattern=".csv", full.names=TRUE)

SpC_14_all <- data.frame()
for(fil in file.names){
  SpC14 <- read_csv(fil,skip= 1)
  SpC14<-SpC14[,c(2,3)]
  colnames(SpC14)[1] <- "Date"
  colnames(SpC14)[2] <- "SpC"
  SpC14$Date <- mdy_hms(SpC14$Date)
  SpC_14_all <- rbind(SpC_14_all, SpC14)
}


SpC_14_all<- filter(SpC_14_all,SpC>50 & SpC<600) #remove hours out of water and erroneous data
S14<-left_join(S14, SpC_14_all, by='Date')
ggplot(SpC_14_all, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8) #check

write_csv(SpC_14_all, "02_Clean_data/14/SpC.csv")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/14/pH", pattern=".xlsx", full.names=TRUE)

pH_14_all <- data.frame()
for(fil in file.names){
  pH14 <- read_xlsx(fil)
  pH14<-pH14[,c(2,5)]
  colnames(pH14)[1] <- "Date"
  colnames(pH14)[2] <- "pH"
  pH_14_all <- rbind(pH_14_all, pH14)
}


pH_14_all<- filter(pH_14_all,pH>4 & pH<7) #remove hours out of water and erroneous data
S14<-left_join(S14, pH_14_all, by='Date')
ggplot(pH_14_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8) #check

write_csv(pH_14_all, "02_Clean_data/14/pH.csv")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/14", pattern=".csv", full.names=TRUE)

LB_14FDOM_csv <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB14<-LB14[,c(1,4)]
  colnames(LB14)[2] <- "FDOM"
  LB14<-filter(LB14,FDOM>1)
  LB_14FDOM_csv <- rbind(LB_14FDOM_csv, LB14) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/14", pattern=".dat", full.names=TRUE)

LB_14FDOM_dat <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil, skip= 3)
  LB14<-LB14[,c(1,5)]
  colnames(LB14)[1] <- "Date"
  colnames(LB14)[2] <- "FDOM"
  LB14<-filter(LB14,FDOM>5) #remove hours out of water
  LB_14FDOM_dat <- rbind(LB_14FDOM_dat, LB14)}

LB14_FDOM<-rbind(LB_14FDOM_csv, LB_14FDOM_dat)

ggplot(LB14_FDOM, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8) #check
write_csv(LB14_FDOM, "02_Clean_data/14/FDOM.csv")




file.names <- list.files(path="01_Raw_data/Lily Box/csv/14", pattern=".csv", full.names=TRUE)

LB_14CO2_csv <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB14<-LB14[,c(1,5)]
  colnames(LB14)[3] <- "CO2"
  LB14$CO2<-LB14$CO2-290.3 #signal offset
  LB14<-filter(LB14, CO2> 500) #remove hours out of water
  LB_14CO2_csv <- rbind(LB_14CO2_csv, LB14) }

ggplot(LB_14CO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8) #check


file.names <- list.files(path="01_Raw_data/Lily Box/dat/14", pattern=".dat", full.names=TRUE)

LB_14CO2_dat <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil, skip= 5)
  LB14<-LB14[,c(1,4)]
  colnames(LB14)[1] <- "Date"
  colnames(LB14)[2] <- "CO2"
  LB14$CO2<-LB14$CO2-290.3 #sensor offset
  LB14<-filter(LB14, CO2> 600)  #remove hours out of water
  LB_14CO2_dat <- rbind(LB_14CO2_dat, LB14)}

LB14_CO2<-rbind(LB_14CO2_csv,LB_14CO2_dat)

ggplot(LB14_CO2, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8) #check

write_csv(LB14_CO2, "02_Clean_data/14/CO2.csv")

S14<-left_join(S14, LB14_FDOM, by='Date')
S14<-left_join(S14, LB14_CO2, by='Date')

###Stage#####
S14<- S14 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))


h14 <- read_excel("02_Clean_data/Calculated_Stage/Stream #3.xlsx",
                 skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h14<-h14[,x]
S14<- left_join(S14, h14, by= c("Year","Mon","Day"))
S14<-rename(S14, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
#S14<-filter(S14, Q>0) #remove ditch water
S14 <- S14[!duplicated(S14[c('Date')]),]
S14$Site<-"14"
write_csv(S14, "02_Clean_data/14.csv")

