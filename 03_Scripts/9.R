#packages#####
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(lubridate)

samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/9/DO", pattern=".csv", full.names=TRUE)

DO_9_all <- data.frame()
for(fil in file.names){
  DO9 <- read_csv(fil,skip= 1)
  DO9<-DO9[,c(2,3,4)]
  colnames(DO9)[1] <- "Date"
  colnames(DO9)[2] <- "DO"
  colnames(DO9)[3] <- "Temp"
  DO9$Date <- mdy_hms(DO9$Date)
  DO_9_all <- rbind(DO_9_all, DO9)
}

DO_9_all$DO[DO_9_all$DO<0] <- 0.01 #remove erroneous data
DO_9_all<- filter(DO_9_all, DO<6.5) #remove hours out of water

ggplot(DO_9_all, aes(x=Date))+geom_line(aes(y=DO, color="DO"), size=0.8)#check
S9<-left_join(samplingperiod, DO_9_all, by='Date')

write_xlsx(DO_9_all, "02_Clean_data/9/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/9/SpC", pattern=".csv", full.names=TRUE)

SpC_9_all <- data.frame()
for(fil in file.names){
  SpC9 <- read_csv(fil,skip= 1)
  SpC9<-SpC9[,c(2,3)]
  colnames(SpC9)[1] <- "Date"
  colnames(SpC9)[2] <- "SpC"
  SpC9$Date <- mdy_hms(SpC9$Date)
  SpC_9_all <- rbind(SpC_9_all, SpC9)
}


SpC_9_all<- filter(SpC_9_all, SpC>50 & SpC<400) #remove hours out of water and erroneous data
S9<-left_join(S9, SpC_9_all, by='Date')

ggplot(SpC_9_all, aes(x=Date))+geom_line(aes(y=SpC, color="SpC"), size=0.8)

write_xlsx(SpC_9_all, "02_Clean_data/9/SpC.xlsx")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/9/pH", pattern=".xlsx", full.names=TRUE)

pH_9_all <- data.frame()
for(fil in file.names){
  pH9 <- read_xlsx(fil)
  pH9<-pH9[,c(2,5)]
  colnames(pH9)[1] <- "Date"
  colnames(pH9)[2] <- "pH"
  pH_9_all <- rbind(pH_9_all, pH9)
}


pH_9_all<- filter(pH_9_all, pH<5) #remove hours out of water and erroneous data
S9<-left_join(S9, pH_9_all, by='Date')
ggplot(pH_9_all, aes(x=Date))+geom_line(aes(y=pH, color="pH"), size=0.8) #check

write_xlsx(pH_9_all, "02_Clean_data/9/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/9", pattern=".csv", full.names=TRUE)

LB_9FDOM_csv <- data.frame()
for(fil in file.names){
  LB9 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB9<-LB9[,c(1,4)]
  colnames(LB9)[2] <- "FDOM"
  LB9<-filter(LB9,FDOM>5) #remove hours out of water and erroneous data
  LB_9FDOM_csv <- rbind(LB_9FDOM_csv, LB9) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/9", pattern=".dat", full.names=TRUE)

LB_9FDOM_dat <- data.frame()
for(fil in file.names){
  LB9 <- read_csv(fil, skip= 3)
  LB9<-LB9[,c(1,6)]
  colnames(LB9)[1] <- "Date"
  colnames(LB9)[2] <- "FDOM"
  LB9<-filter(LB9,FDOM>5) #remove hours out of water and erroneous data
  LB_9FDOM_dat <- rbind(LB_9FDOM_dat, LB9)}

file.names <- list.files(path="01_Raw_data/Lily Box/csv/9", pattern=".csv", full.names=TRUE)

LB_9CO2_csv <- data.frame()
for(fil in file.names){
  LB9 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB9<-LB9[,c(1,5)]
  colnames(LB9)[2] <- "CO2"
  LB9$CO2<-LB9$CO2+90
  LB9<-filter(LB9, CO2>500) #remove hours out of water and erroneous data
  LB_9CO2_csv <- rbind(LB_9CO2_csv, LB9) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/9", pattern=".dat", full.names=TRUE)

LB_9CO2_dat <- data.frame()
for(fil in file.names){
  LB9 <- read_csv(fil, skip= 5)
  LB9<-LB9[,c(1,4)]
  colnames(LB9)[1] <- "Date"
  colnames(LB9)[2] <- "CO2"
  LB9$CO2<-LB9$CO2+90 #sensor offset
  LB9<-filter(LB9, CO2>500) #remove hours out of water and erroneous data
  LB_9CO2_dat <- rbind(LB_9CO2_dat, LB9)}

LB9_CO2<-rbind(LB_9CO2_csv,LB_9CO2_dat)
ggplot(LB9_CO2, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8) #check

write_xlsx(LB9_CO2, "02_Clean_data/9/CO2.xlsx")

LB9_FDOM<-rbind(LB_9FDOM_csv,LB_9FDOM_dat)
ggplot(LB9_FDOM, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8) #check

write_xlsx(LB9_FDOM, "02_Clean_data/9/FDOM.xlsx")

S9<-left_join(S9, LB9_FDOM, by='Date')
S9<-left_join(S9, LB9_CO2, by='Date')



###Stage#####
S9<- S9 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))
h9 <- read_excel("02_Clean_data/Calculated_Stage/Stream #9.xlsx",
                       skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h9<-h9[,x]
S9<- left_join(S9, h9, by= c("Year","Mon","Day"))
S9<-rename(S9, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S9<-filter(S9, Q>0) #remove ditch water
S9 <- S9[!duplicated(S9[c('Date')]),]
S9$Site<-"9"

write_xlsx(S9, "02_Clean_data/9.xlsx")

