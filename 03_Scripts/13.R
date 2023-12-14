#packages#####
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(lubridate)


###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/13/DO", pattern=".csv", full.names=TRUE)

DO_13_all <- data.frame()
for(fil in file.names){
  DO13 <- read_csv(fil,skip= 1)
  DO13<-DO13[,c(2,3,4)]
  colnames(DO13)[1] <- "Date"
  colnames(DO13)[2] <- "DO"
  colnames(DO13)[3] <- "Temp"
  DO13<-filter(DO13, DO>-800) #remove erroneous data
  DO13$Date <- mdy_hms(DO13$Date)
  DO_13_all <- rbind(DO_13_all, DO13)
}

#remove erroneous data
#remove hours out of the water
for(i in 1:nrow(DO_13_all)){
  if(DO_13_all$DO[i]<=0 | DO_13_all$DO[i]>=9) { DO_13_all$DO[i]<- NA}
  else {DO_13_all$DO[i]<- DO_13_all$DO[i]-0 }}

S13<-left_join(samplingperiod, DO_13_all, by='Date')

ggplot(DO_13_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8) #check

write_csv(DO_13_all, "02_Clean_data/13/DO.csv")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/13/SpC", pattern=".csv", full.names=TRUE)

SpC_13_all <- data.frame()
for(fil in file.names){
  SpC13 <- read_csv(fil,skip= 1)
  SpC13<-SpC13[,c(2,3)]
  colnames(SpC13)[1] <- "Date"
  colnames(SpC13)[2] <- "SpC"
  SpC13$Date <- mdy_hms(SpC13$Date)
  SpC_13_all <- rbind(SpC_13_all, SpC13)
}


SpC_13_all<- filter(SpC_13_all, SpC>50 & SpC<600) #remove hours out of the water and erroneous data
S13<-left_join(S13, SpC_13_all, by='Date')
ggplot(SpC_13_all, aes(x=Date))+geom_line(aes(y=SpC, color="SpC"), size=0.8) #check

write_csv(SpC_13_all, "02_Clean_data/13/SpC.csv")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/13/pH", pattern=".xlsx", full.names=TRUE)

pH_13_all <- data.frame()
for(fil in file.names){
  pH13 <- read_xlsx(fil)
  pH13<-pH13[,c(2,5)]
  colnames(pH13)[1] <- "Date"
  colnames(pH13)[2] <- "pH"
  pH_13_all <- rbind(pH_13_all, pH13)
}


pH_13_all<- filter(pH_13_all, pH>4) #remove hours out of the water
S13<-left_join(S13, pH_13_all, by='Date')
ggplot(pH_13_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8) #check

write_csv(pH_13_all, "02_Clean_data/13/pH.csv")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/13", pattern=".csv", full.names=TRUE)

LB_13FDOM_csv <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil,
                   col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                    "CO2" = col_number()))
  LB13<-LB13[,c(1,4)]
  colnames(LB13)[2] <- "FDOM"
  LB13<-filter(LB13,FDOM>1)
  LB_13FDOM_csv <- rbind(LB_13FDOM_csv, LB13) }


file.names <- list.files(path="01_Raw_data/Lily Box/dat/13", pattern=".dat", full.names=TRUE)

LB_13FDOM_dat <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil, skip= 3)
  LB13<-LB13[,c(1,5)]
  colnames(LB13)[1] <- "Date"
  colnames(LB13)[2] <- "FDOM"
  LB13<-filter(LB13,FDOM>5) #remove hours out of water
  LB_13FDOM_dat <- rbind(LB_13FDOM_dat, LB13)}

LB13_FDOM<-rbind(LB_13FDOM_csv, LB_13FDOM_dat)

ggplot(LB13_FDOM, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8) #check
write_csv(LB13_FDOM, "02_Clean_data/13/FDOM.csv")




file.names <- list.files(path="01_Raw_data/Lily Box/csv/13", pattern=".csv", full.names=TRUE)

LB_13CO2_csv <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil,
                   col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                    "CO2" = col_number()))
  LB13<-LB13[,c(1,5)]
  colnames(LB13)[3] <- "CO2"
  LB13$CO2<-LB13$CO2-290.3 #signal offset
  LB13<-filter(LB13, CO2> 500) #remove hours out of water
  LB_13CO2_csv <- rbind(LB_13CO2_csv, LB13) }

ggplot(LB_13CO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8) #check


file.names <- list.files(path="01_Raw_data/Lily Box/dat/13", pattern=".dat", full.names=TRUE)

LB_13CO2_dat <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil, skip= 5)
  LB13<-LB13[,c(1,4)]
  colnames(LB13)[1] <- "Date"
  colnames(LB13)[2] <- "CO2"
  LB13$CO2<-LB13$CO2-290.3 #sensor offset
  LB13<-filter(LB13, CO2> 600)  #remove hours out of water
  LB_13CO2_dat <- rbind(LB_13CO2_dat, LB13)}

LB13_CO2<-rbind(LB_13CO2_csv,LB_13CO2_dat)

ggplot(LB13_CO2, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8) #check

write_csv(LB13_CO2, "02_Clean_data/13/CO2.csv")

S13<-left_join(S13, LB13_FDOM, by='Date')
S13<-left_join(S13, LB13_CO2, by='Date')


###Stage#####

S13<- S13 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

h13 <- read_excel("02_Clean_data/Calculated_Stage/Stream #3.xlsx",
                  skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h13<-h13[,x]
S13<- left_join(S13, h13, by= c("Year","Mon","Day"))
S13<-rename(S13, "Stage"="Water Depth (m)",
            "Q"="Flow (L/s)")
#S13<-filter(S13, Q>0) #remove ditch water
S13 <- S13[!duplicated(S13[c('Date')]),]
S13$Site<-'13'
write_csv(S13, "02_Clean_data/13.csv")

