#packages#####
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(lubridate)

samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/Hobo Excels/6a/DO", pattern=".csv", full.names=TRUE)

DO_6a_all <- data.frame()
for(fil in file.names){
  DO6a <- read_csv(fil, skip= 1)
  DO6a<-DO6a[,c(2,3,4)]
  colnames(DO6a)[1] <- "Date"
  colnames(DO6a)[2] <- "DO"
  colnames(DO6a)[3] <- "Temp"
  DO6a$Date <- mdy_hms(DO6a$Date)
  DO_6a_all <- rbind(DO_6a_all, DO6a)
}

DO_6a_all$DO[DO_6a_all$DO<0] <- 0.01 #remove erroneous data
DO_6a_all<- filter(DO_6a_all, DO<6) #remove hours our of water
S6a<-left_join(samplingperiod, DO_6a_all, by='Date')

ggplot(DO_6a_all, aes(x=Date))+geom_line(aes(y=DO, color="DO"), size=0.8) #check
###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/6a/SpC", pattern=".csv", full.names=TRUE)

SpC_6a_all <- data.frame()
for(fil in file.names){
  SpC6a <- read_csv(fil,skip= 1)
  SpC6a<-SpC6a[,c(2,3)]
  colnames(SpC6a)[1] <- "Date"
  colnames(SpC6a)[2] <- "SpC"
  SpC6a$Date <- mdy_hms(SpC6a$Date)
  SpC_6a_all <- rbind(SpC_6a_all, SpC6a)
}


SpC_6a_all<- filter(SpC_6a_all, SpC>50 &SpC<300)#remove hours our of water and erroneous data
S6a<-left_join(S6a, SpC_6a_all, by='Date')
ggplot(SpC_6a_all, aes(x=Date))+geom_line(aes(y=SpC, color="SpC"), size=0.8) #check

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/6a/pH", pattern=".xlsx", full.names=TRUE)

pH_6a_all <- data.frame()
for(fil in file.names){
  pH6a <- read_xlsx(fil)
  pH6a<-pH6a[,c(2,5)]
  colnames(pH6a)[1] <- "Date"
  colnames(pH6a)[2] <- "pH"
  pH_6a_all <- rbind(pH_6a_all, pH6a)
}


pH_6a_all<- filter(pH_6a_all, pH<6.9) #remove hours our of water
S6a<-left_join(S6a, pH_6a_all, by='Date')
ggplot(pH_6a_all, aes(x=Date))+geom_line(aes(y=pH, color="pH"), size=0.8)#checl

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/6a", pattern=".csv", full.names=TRUE)

LB_6aFDOM_csv <- data.frame()
for(fil in file.names){
  LB6a <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB6a<-LB6a[,c(1,4)]
  colnames(LB6a)[2] <- "FDOM"
  LB6a<-filter(LB6a,FDOM>1)#remove hours our of water
  LB_6aFDOM_csv <- rbind(LB_6aFDOM_csv, LB6a) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/6a", pattern=".dat", full.names=TRUE)

LB_6aFDOM_dat <- data.frame()
for(fil in file.names){
  LB6a <- read_csv(fil, skip= 3)
  LB6a<-LB6a[,c(1,6)]
  colnames(LB6a)[1] <- "Date"
  colnames(LB6a)[2] <- "FDOM"
  #LB6a<-filter(LB6a,FDOM>1) #remove hours our of water, eventually
  LB_6aFDOM_dat <- rbind(LB_6aFDOM_dat, LB6a)}

LB6a_FDOM<-rbind(LB_6aFDOM_csv, LB_6aFDOM_dat)

ggplot(LB6a_FDOM, aes(x=Date))+geom_line(aes(y=FDOM), size=0.8) #checl



file.names <- list.files(path="01_Raw_data/Lily Box/csv/6a", pattern=".csv", full.names=TRUE)

LB_6aCO2_csv <- data.frame()
for(fil in file.names){
  LB6a <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB6a<-LB6a[,c(1,5)]
  colnames(LB6a)[3] <- "CO2"
  LB6a<-filter(LB6a, CO2> 500) #remove hours out of water
  LB_6aCO2_csv <- rbind(LB_6aCO2_csv, LB6a) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/6a", pattern=".dat", full.names=TRUE)

LB_6aCO2_dat <- data.frame()
for(fil in file.names){
  LB6a <- read_csv(fil, skip= 5)
  LB6a<-LB6a[,c(1,4)]
  colnames(LB6a)[1] <- "Date"
  colnames(LB6a)[2] <- "CO2"
  LB6a<-filter(LB6a, CO2> 500) #remove hours out of water
  LB_6aCO2_dat <- rbind(LB_6aCO2_dat, LB6a)}

LB6_CO2<-rbind(LB_6aCO2_csv,LB_6aCO2_dat)

ggplot(LB6_CO2, aes(x=Date))+geom_line(aes(y=CO2), size=0.8) #checl

S6a<-left_join(S6a, LB6a_FDOM, by='Date')
S6a<-left_join(S6a, LB6_CO2, by='Date')


###Stage#####
S6a<- S6a %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

h6a <- read_excel("02_Clean_data/Calculated_Stage/Stream #6a.xlsx",skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h6a<-h6a[,x]
S6a<- left_join(S6a, h6a, by= c("Year","Mon","Day"))
S6a<-rename(S6a, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
#S6a<-filter(S6a, Q>0) #remove ditch water
S6a <- S6a[!duplicated(S6a[c('Date')]),]
S6a$Site<-'6a'
write_xlsx(S6a, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Masterfiles_latest/Stream Chemistry/6a.xlsx")

