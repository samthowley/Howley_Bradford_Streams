#packages#####
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(lubridate)

samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/6/DO", pattern=".csv", full.names=TRUE)

DO_6 <- data.frame()
for(fil in file.names){
  DO6 <- read_csv(fil,skip= 1)
  colnames(DO6)[2] <- "Date"
  colnames(DO6)[3] <- "DO"
  colnames(DO6)[4] <- "Temp"
  DO6$Date <- mdy_hms(DO6$Date)
  DO6<-DO6[,-c(1)]
  DO_6 <- rbind(DO_6, DO6)
}


file.names <- list.files(path="01_Raw_data/MiniDot/6", pattern=".TXT", full.names=TRUE)

MiniDot_6 <- data.frame()
for(fil in file.names){
  DO6 <- read_csv(fil,skip= 8)
  DO6<-DO6[,c(3,6,5)]
  colnames(DO6)[1] <- "Date"
  colnames(DO6)[2] <- "DO"
  colnames(DO6)[3] <- "Temp"
  #DO6$Date <- mdy_hms(DO6$Date)
  MiniDot_6 <- rbind(MiniDot_6, DO6)
}


DO_6_all<-rbind(DO_6,MiniDot_6)
DO_6_all$DO[DO_6_all$DO<0] <- 0.01 #remove erroneous data
DO_6_all<-filter(DO_6_all, DO<6) #remove hours out of water

ggplot(DO_6_all, aes(x=Date))+geom_line(aes(y=DO, color="DO"), size=0.8) #check
for(i in 1:nrow(DO_6_all)){
  if(DO_6_all$DO[i]<=0 | DO_6_all$DO[i]>=7.4) { DO_6_all$DO[i]<- NA}
  else {DO_6_all$DO[i]<- DO_6_all$DO[i]-0 }}

S6<-left_join(samplingperiod, DO_6_all, by='Date')

write_csv(DO_6_all, "02_Clean_data/6/DO.csv")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/6/SpC", pattern=".csv", full.names=TRUE)

SpC_6_all <- data.frame()
for(fil in file.names){
  SpC6 <- read_csv(fil,skip= 1)
  SpC6<-SpC6[,c(2,3)]
  colnames(SpC6)[1] <- "Date"
  colnames(SpC6)[2] <- "SpC"
  SpC6$Date <- mdy_hms(SpC6$Date)
  SpC_6_all <- rbind(SpC_6_all, SpC6)
}


SpC_6_all<- filter(SpC_6_all,SpC>50 & SpC<150) #remove hours out of water and erroneous data

ggplot(SpC_6_all, aes(x=Date))+geom_line(aes(y=SpC, color="SpC"), size=0.8) #check

S6<-left_join(S6, SpC_6_all, by='Date')

write_csv(SpC_6_all, "02_Clean_data/6/SpC.csv")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/6/pH", pattern=".xlsx", full.names=TRUE)

pH_6_all <- data.frame()
for(fil in file.names){
  pH6 <- read_xlsx(fil)
  pH6<-pH6[,c(2,5)]
  colnames(pH6)[1] <- "Date"
  colnames(pH6)[2] <- "pH"
  pH_6_all <- rbind(pH_6_all, pH6)
}


pH_6_all<- filter(pH_6_all, pH>-1) #remove erroneous data
S6<-left_join(S6, pH_6_all, by='Date')
ggplot(pH6, aes(x=Date))+geom_line(aes(y=pH, color="pH"), size=0.8) #check
write_csv(pH_6_all, "02_Clean_data/6/pH.csv")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/6", pattern=".csv", full.names=TRUE)

LB_6FDOM_csv <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB6<-LB6[,c(1,4)]
  colnames(LB6)[2] <- "FDOM"
  LB6<-filter(LB6,FDOM>1& FDOM< 300) #remove hours out of water and erraneous data
  LB_6FDOM_csv <- rbind(LB_6FDOM_csv, LB6) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/6", pattern=".dat", full.names=TRUE)

LB_6FDOM_dat <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil, skip= 3)
  LB6<-LB6[,c(1,5)]
  colnames(LB6)[1] <- "Date"
  colnames(LB6)[2] <- "FDOM"
  LB6<-filter(LB6,FDOM>1) #remove hours out of water
  LB_6FDOM_dat <- rbind(LB_6FDOM_dat, LB6)}

LB6_FDOM<-rbind(LB_6FDOM_csv, LB_6FDOM_dat)

ggplot(LB6_FDOM, aes(x=Date))+geom_line(aes(y=FDOM), size=0.8) #check

write_csv(LB6_FDOM, "02_Clean_data/6/FDOM.csv")



file.names <- list.files(path="01_Raw_data/Lily Box/csv/6", pattern=".csv", full.names=TRUE)

LB_6CO2_csv <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB6<-LB6[,c(1,5)]
  colnames(LB6)[3] <- "CO2"
  LB6<-filter(LB6, CO2> 500) #remove data our of water
  LB_6CO2_csv <- rbind(LB_6CO2_csv, LB6) }

file.names <- list.files(path="01_Raw_data/Lily Box/dat/6", pattern=".dat", full.names=TRUE)

LB_6CO2_dat <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil, skip= 5)
  LB6<-LB6[,c(1,4)]
  colnames(LB6)[1] <- "Date"
  colnames(LB6)[2] <- "CO2"
  LB6<-filter(LB6, CO2> 5000) #remove erroneous data
  LB_6CO2_dat <- rbind(LB_6CO2_dat, LB6)}

LB6_CO2<-rbind(LB_6CO2_csv,LB_6CO2_dat)

ggplot(LB_6CO2, aes(x=Date))+geom_line(aes(y=CO2), size=0.8)


write_csv(LB6_CO2, "02_Clean_data/6/CO2.csv")

S6<-left_join(S6, LB6_FDOM, by='Date')
S6<-left_join(S6, LB6_CO2, by='Date')



###Stage#####
S6<- S6 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

h6 <- read_excel("02_Clean_data/Calculated_Stage/Stream #6a.xlsx",
                  skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h6<-h6[,x]
S6<- left_join(S6, h6, by= c("Year","Mon","Day"))
S6<-rename(S6, "Stage"="Water Depth (m)",
            "Q"="Flow (L/s)")
S6<-filter(S6, Q>0) #remove ditch water
S6$Site<-"6"
S6<-left_join(samplingperiod,S6)
S6 <- S6[!duplicated(S6[c('Date')]),]

write_csv(S6, "02_Clean_data/6.csv")


