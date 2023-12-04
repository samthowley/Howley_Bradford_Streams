#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(readxl)
library(corrplot)
library("broom")
library(car)
library(imputeTS)
library(ggExtra)
library(lubridate)

###setwd######
samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/5/DO", pattern=".csv", full.names=TRUE)

DO_5_all <- data.frame()
for(fil in file.names){
  DO5 <- read_csv(fil,
                skip= 1)
  DO5<-DO5[,c(2,3,4)]
  colnames(DO5)[1] <- "Date"
  colnames(DO5)[2] <- "DO"
  colnames(DO5)[3] <- "Temp"
  DO5$Date <- mdy_hms(DO5$Date)
  DO_5_all <- rbind(DO_5_all, DO5)
}

DO_5_all$DO[DO_5_all$DO<0] <- NA
DO_5_all<- filter(DO_5_all, DO<6.8) #remove hours out of water
S5<-left_join(samplingperiod, DO_5_all, by='Date')

ggplot(DO_5_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8) #check

write_xlsx(DO_5_all, "02_Clean_data/5/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/5/SpC", pattern=".csv", full.names=TRUE)

SpC_5_all <- data.frame()
for(fil in file.names){
  SpC5 <- read_csv(fil,
                   skip= 1)
  SpC5<-SpC5[,c(2,3)]
  colnames(SpC5)[1] <- "Date"
  colnames(SpC5)[2] <- "SpC"
  SpC5$Date <- mdy_hms(SpC5$Date)
  SpC_5_all <- rbind(SpC_5_all, SpC5)
}


SpC_5_all<- filter(SpC_5_all, SpC>50 & SpC<6000) #remove hours out of water and erroneous readings
S5<-left_join(S5, SpC_5_all, by='Date')

ggplot(SpC_5_all, aes(x=Date))+geom_line(aes(y=SpC, color="SpC"), size=0.8) #check

write_xlsx(SpC_5_all, "02_Clean_data/5/SpC.xlsx")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/5/pH", pattern=".xlsx", full.names=TRUE)

pH_5_all <- data.frame()
for(fil in file.names){
  pH5 <- read_xlsx(fil)
  pH5<-pH5[,c(2,5)]
  colnames(pH5)[1] <- "Date"
  colnames(pH5)[2] <- "pH"
  pH_5_all <- rbind(pH_5_all, pH5)
}


pH_5_all<- filter(pH_5_all, pH>3.5) #remove hours out of water
S5<-left_join(S5, pH_5_all, by='Date')
ggplot(pH_5_all, aes(x=Date))+geom_line(aes(y=pH, color="pH"), size=0.8) #check

write_xlsx(pH_5_all, "02_Clean_data/5/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/5", pattern=".csv", full.names=TRUE)

LB_5FDOM_csv <- data.frame()
for(fil in file.names){
  LB5 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB5<-LB5[,c(1,4)]
  colnames(LB5)[2] <- "FDOM"
  LB5<-filter(LB5,FDOM>5) #remove days out of water
  LB_5FDOM_csv <- rbind(LB_5FDOM_csv, LB5) }

ggplot(LB_5FDOM_csv, aes(x=Date))+geom_line(aes(y=FDOM), size=0.8) #check

file.names <- list.files(path="01_Raw_data/Lily Box/dat/5", pattern=".dat", full.names=TRUE)

LB_5FDOM_dat <- data.frame()
for(fil in file.names){
  LB5 <- read_csv(fil, skip= 3)
  LB5<-LB5[,c(1,6)]

  colnames(LB5)[1] <- "Date"
  colnames(LB5)[2] <- "FDOM"
  LB5<-filter(LB5,FDOM>5) #remove hours out of water

  LB_5FDOM_dat <- rbind(LB_5FDOM_dat, LB5)}

ggplot(LB_5FDOM_dat, aes(x=Date))+geom_line(aes(y=FDOM), size=0.8) #check

LB5_FDOM<-rbind(LB_5FDOM_csv, LB_5FDOM_dat)
write_xlsx(LB5_FDOM, "02_Clean_data/5/FDOM.xlsx")

file.names <- list.files(path="01_Raw_data/Lily Box/csv/5", pattern=".csv", full.names=TRUE)

LB_5CO2_csv <- data.frame()
for(fil in file.names){
  LB5 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB5<-LB5[,c(1,5)]
  colnames(LB5)[3] <- "CO2"
  LB5<-filter(LB5, CO2>500& CO2<15000) #remove hours out of water and erroneous readings
  LB_5CO2_csv <- rbind(LB_5CO2_csv, LB5) }

ggplot(LB_5CO2_csv, aes(x=Date))+geom_line(aes(y=CO2), size=0.8) #check

file.names <- list.files(path="01_Raw_data/Lily Box/dat/5", pattern=".dat", full.names=TRUE)

LB_5CO2_dat <- data.frame()
for(fil in file.names){
  LB5 <- read_csv(fil, skip= 5)
  LB5<-LB5[,c(1,5)]
  colnames(LB5)[1] <- "Date"
  colnames(LB5)[2] <- "CO2"
  LB5<-filter(LB5, CO2> 500) #remove hours out of water
  LB_5CO2_dat <- rbind(LB_5CO2_dat, LB5)}

ggplot(LB_5CO2_dat, aes(x=Date))+geom_line(aes(y=CO2), size=0.8) #check

LB5_CO2<-rbind(LB_5CO2_csv,LB_5CO2_dat)
write_xlsx(LB5_CO2, "02_Clean_data/5/CO2.xlsx")

S5<-left_join(S5, LB5_FDOM, by='Date')
S5<-left_join(S5, LB5_CO2, by='Date')

###Stage#####
S5<- S5 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

h5 <- read_excel("02_Clean_data/Calculated_Stage/Stream #5.xlsx",
                       skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h5<-h5[,x]
S5<- left_join(S5, h5, by= c("Year","Mon","Day"))
S5<-rename(S5, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S5<-filter(S5, Q>0) #remove hours with ditch water
S5 <- S5[!duplicated(S5[c('Date')]),]
S5$Site<-"5"
S5<-left_join(samplingperiod,S5)
write_xlsx(S5, "02_Clean_data/5.xlsx")

