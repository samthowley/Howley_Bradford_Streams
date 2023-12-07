#packages#####
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(lubridate)

samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/7/DO", pattern=".csv", full.names=TRUE)

DO_7_all <- data.frame()
for(fil in file.names){
  DO7 <- read_csv(fil,skip= 1)
  DO7<-DO7[,c(2,3,4)]
  colnames(DO7)[1] <- "Date"
  colnames(DO7)[2] <- "DO"
  colnames(DO7)[3] <- "Temp"
  DO7$Date <- mdy_hms(DO7$Date)
  DO_7_all <- rbind(DO_7_all, DO7)
}
#remove erroneous data
#remove hours out of the water
for(i in 1:nrow(DO_7_all)){
  if(DO_7_all$DO[i]<=0 | DO_7_all$DO[i]>=7) { DO_7_all$DO[i]<- NA}
  else {DO_7_all$DO[i]<- DO_7_all$DO[i]-0 }}
S7<-left_join(samplingperiod, DO_7_all, by='Date')

ggplot(DO_7_all, aes(x=Date))+geom_line(aes(y=DO, color="DO"), size=0.8) #check

write_xlsx(DO_7_all, "02_Clean_data/7/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/7/SpC", pattern=".csv", full.names=TRUE)

SpC_7_all <- data.frame()
for(fil in file.names){
  SpC7 <- read_csv(fil,skip= 1)
  SpC7<-SpC7[,c(2,3)]
  colnames(SpC7)[1] <- "Date"
  colnames(SpC7)[2] <- "SpC"
  SpC7$Date <- mdy_hms(SpC7$Date)
  SpC_7_all <- rbind(SpC_7_all, SpC7)
}


SpC_7_all<- filter(SpC_7_all, SpC>50 & SpC<400) #remove hours out of water and erroneous data
S7<-left_join(S7, SpC_7_all, by='Date')
ggplot(SpC_7_all, aes(x=Date))+geom_line(aes(y=SpC, color="SpC"), size=0.8) #check

write_xlsx(SpC_7_all, "02_Clean_data/7/SpC.xlsx")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/7/pH", pattern=".xlsx", full.names=TRUE)

pH_7_all <- data.frame()
for(fil in file.names){
  pH7 <- read_xlsx(fil)
  pH7<-pH7[,c(2,5)]
  colnames(pH7)[1] <- "Date"
  colnames(pH7)[2] <- "pH"
  pH_7_all <- rbind(pH_7_all, pH7)
}


pH_7_all<- filter(pH_7_all, pH<6.9) #remove hours out of water
S7<-left_join(S7, pH_7_all, by='Date')

ggplot(pH_7_all, aes(x=Date))+geom_line(aes(y=pH, color="pH"), size=0.8) #check

write_xlsx(pH_7_all, "02_Clean_data/7/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/7", pattern=".csv", full.names=TRUE)

LB_7FDOM_csv <- data.frame()
for(fil in file.names){
  LB7 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB7<-LB7[,c(1,4)]
  colnames(LB7)[2] <- "FDOM"
  LB7<-filter(LB7,FDOM>3.5) #remove hours out of water
  LB_7FDOM_csv <- rbind(LB_7FDOM_csv, LB7) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/7", pattern=".dat", full.names=TRUE)

LB_7FDOM_dat <- data.frame()
for(fil in file.names){
  LB7 <- read_csv(fil, skip= 3)
  LB7<-LB7[,c(1,6)]
  colnames(LB7)[1] <- "Date"
  colnames(LB7)[2] <- "FDOM"
  LB7<-filter(LB7,FDOM>3.5)#remove hours out of water
  LB_7FDOM_dat <- rbind(LB_7FDOM_dat, LB7)}

LB7_FDOM<-rbind(LB_7FDOM_csv, LB_7FDOM_dat)

write_xlsx(LB7_FDOM, "02_Clean_data/7/FDOM.xlsx")



file.names <- list.files(path="01_Raw_data/Lily Box/csv/7", pattern=".csv", full.names=TRUE)

LB_7CO2_csv <- data.frame()
for(fil in file.names){
  LB7 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB7<-LB7[,c(1,5)]
  colnames(LB7)[3] <- "CO2"
  LB7$CO2<-LB7$CO2+222 # sensor offset
  LB7<-filter(LB7, CO2> 500) #remove hours out of water
  LB_7CO2_csv <- rbind(LB_7CO2_csv, LB7) }

ggplot(LB_7CO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

file.names <- list.files(path="01_Raw_data/Lily Box/dat/7", pattern=".dat", full.names=TRUE)

LB_7CO2_dat <- data.frame()
for(fil in file.names){
  LB7 <- read_csv(fil, skip= 5)
  LB7<-LB7[,c(1,4)]
  colnames(LB7)[1] <- "Date"
  colnames(LB7)[2] <- "CO2"
  LB7$CO2<-LB7$CO2+222 #sensor offset
  LB7<-filter(LB7, CO2> 500) #remove hours out of water
  LB_7CO2_dat <- rbind(LB_7CO2_dat, LB7)}

LB7_CO2<-rbind(LB_7CO2_csv,LB_7CO2_dat)

ggplot(LB7_CO2, aes(x=Date))+geom_line(aes(y=CO2), size=0.8) #check

write_xlsx(LB7_CO2, "02_Clean_data/7/CO2.xlsx")

S7<-left_join(S7, LB7_FDOM, by='Date')
S7<-left_join(S7, LB7_CO2, by='Date')

ggplot(S7, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

###Stage#####
S7<- S7 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))


h7 <- read_excel("02_Clean_data/Calculated_Stage/Stream #7.xlsx",
                       skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h7<-h7[,x]
S7<- left_join(S7, h7, by= c("Year","Mon","Day"))
S7<-rename(S7, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
#S7<-filter(S7, Q>0) #remove ditch water
S7 <- S7[!duplicated(S7[c('Date')]),]
S7$Site<-'7'
write_xlsx(S7, "02_Clean_data/7.xlsx")

