#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)

samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2021-03-29 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-03-29 00:00", tz="UTC"),by="hour")))
samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

clean_DO <- function(fil) {
  DO <- read_csv(fil,skip= 1)
  DO<-DO[,c(2,3,4)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO$Date <- mdy_hms(DO$Date)
  DO<-DO %>% mutate(min=minute(Date)) %>% filter(min==0) %>% filter(DO>0)
  DO<-DO[,-4]
  # for(i in 1:nrow(DO)){if(DO$DO[i]<=0 | DO$DO[i]>=6.8) { DO$DO[i]<- NA}
  #   else {DO$DO[i]<- DO$DO[i]-0 }} #remove hours out of the water and erraneous days
  return(DO)}
MiniDot_DO<-function(fil){
  DO <- read_csv(fil,skip= 8)
  DO<-DO[,c(3,6,5)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  # for(i in 1:nrow(DO)){
  # if(DO$DO[i]<=0 | DO$DO[i]>=7.4) { DO$DO[i]<- NA}
  # else {DO$DO[i]<- DO$DO[i]-0 }}
  return(DO)}
clean_SpC <- function(fil) {
  SpC <- read_csv(fil, skip= 1)
  SpC<-SpC[,c(2,3)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  SpC$Date <- mdy_hms(SpC$Date)
  #SpC<-filter(SpC, SpC>50)#remove hours out of water
  return(SpC)
}
clean_pH <- function(fil) {
  pH <- read_xlsx(fil)
  pH<-pH[,c(2,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  #pH<-filter(pH, pH<6.2) #remove hours out of water
  return(pH)}
clean_CO2_csv<-function(fil){
  LB <- read_csv(fil,
                 col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                  "CO2" = col_number()))
  LB<-LB[,c(1,5)]
  colnames(LB)[3] <- "CO2"
  LB<-filter(LB, CO2>500& CO2<15000)
  return(LB)}
clean_CO2_dat<-function(fil){
  LB <- read_csv(fil, skip= 3)
  LB<-LB[,c(1,4)]
  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB<-filter(LB, CO2>500)
  return(LB)}
clean_CO2_vaisala<-function(fil){
  LB <- read_csv(fil, skip= 3)
  LB<-LB[,c(1,4)]
  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$CO2<-LB$CO2*4.2
  LB<-filter(LB, CO2>500)
  return(LB)}
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 12, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 17, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_text(size = 17),
                             legend.position ="none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

####DO######
file.names <- list.files(path="01_Raw_data/HOBO Excels/3/DO", pattern=".csv", full.names=TRUE)

DO_3_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_3_all<-rbind(DO_3_all, DO)
DO_3_all <- DO_3_all[!duplicated(DO_3_all[c('Date')]),]
DO_3_all[order(as.Date(DO_3_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_3_all$ID<-"3"
for(i in 1:nrow(DO_3_all)){if(DO_3_all$Temp[i]<=45 ) { DO_3_all$Temp[i]<- celsius.to.fahrenheit(DO_3_all$Temp[i])}
  else {DO_3_all$Temp[i]<- DO_3_all$Temp[i]-0 }} #remove hours out of the water and erraneous days


file.names <- list.files(path="01_Raw_data/HOBO Excels/5/DO", pattern=".csv", full.names=TRUE)
DO_5_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_5_all<-rbind(DO_5_all, DO)
DO_5_all <- DO_5_all[!duplicated(DO_5_all[c('Date')]),]
DO_5_all[order(as.Date(DO_5_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_5_all$ID<-"5"

DO_5a_all<-data.frame()
file.names <- list.files(path="01_Raw_data/MiniDot/5a", pattern=".TXT", full.names=TRUE)
for(fil in file.names){
  DO_5a<-MiniDot_DO(fil)
  DO_5a_all<-rbind(DO_5a_all, DO_5a)
  DO_5a_all <- DO_5a_all[!duplicated(DO_5a_all[c('Date')]),]
  DO_5a_all[order(as.Date(DO_5a_all$date, format="%Y-%m-%d %H:%M:%S")),]
  }
DO_5a_all$ID<-"5a"

DO_6_all<-data.frame()
file.names <- list.files(path="01_Raw_data/MiniDot/6", pattern=".TXT", full.names=TRUE)
for(fil in file.names){
  DO_6<-MiniDot_DO(fil)
  DO_6_all<-rbind(DO_6_all, DO_6)
  DO_6_all <- DO_6_all[!duplicated(DO_6_all[c('Date')]),]
  DO_6_all[order(as.Date(DO_6_all$date, format="%Y-%m-%d %H:%M:%S")),]
  }
DO_6_all$ID<-"6"

file.names <- list.files(path="01_Raw_data/HOBO Excels/6a/DO", pattern=".csv", full.names=TRUE)
DO_6a_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_6a_all<-rbind(DO_6a_all, DO)
DO_6a_all <- DO_6a_all[!duplicated(DO_6a_all[c('Date')]),]
DO_6a_all[order(as.Date(DO_6a_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_6a_all$ID<-"6a"

file.names <- list.files(path="01_Raw_data/HOBO Excels/7/DO", pattern=".csv", full.names=TRUE)
DO_7_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_7_all<-rbind(DO_7_all, DO)
DO_7_all <- DO_7_all[!duplicated(DO_7_all[c('Date')]),]
DO_7_all[order(as.Date(DO_7_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_7_all$ID<-"7"

file.names <- list.files(path="01_Raw_data/HOBO Excels/9/DO", pattern=".csv", full.names=TRUE)
DO_9_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_9_all<-rbind(DO_9_all, DO)
DO_9_all <- DO_9_all[!duplicated(DO_9_all[c('Date')]),]
DO_9_all[order(as.Date(DO_9_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_9_all$ID<-"9"

file.names <- list.files(path="01_Raw_data/HOBO Excels/13/DO", pattern=".csv", full.names=TRUE)
DO_13_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_13_all<-rbind(DO_13_all, DO)
DO_13_all <- DO_13_all[!duplicated(DO_13_all[c('Date')]),]
DO_13_all[order(as.Date(DO_13_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_13_all$ID<-"13"

file.names <- list.files(path="01_Raw_data/HOBO Excels/14/DO", pattern=".csv", full.names=TRUE)
DO_14_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_14_all<-rbind(DO_14_all, DO)
DO_14_all <- DO_14_all[!duplicated(DO_14_all[c('Date')]),]
DO_14_all[order(as.Date(DO_14_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_14_all$ID<-"14"

file.names <- list.files(path="01_Raw_data/HOBO Excels/15/DO", pattern=".csv", full.names=TRUE)
DO_15_all<-data.frame()
for(fil in file.names){DO<-clean_DO(fil)
DO_15_all<-rbind(DO_15_all, DO)
DO_15_all <- DO_15_all[!duplicated(DO_15_all[c('Date')]),]
DO_15_all[order(as.Date(DO_15_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
DO_15_all$ID<-"15"


DO<-rbind(DO_3_all, DO_5_all, DO_5a_all, DO_6_all, DO_6a_all, DO_7_all, DO_9_all, DO_13_all,
          DO_14_all, DO_15_all)

write_csv(DO, "02_Clean_data/Chem/DO_cleaned.csv")


####SpC####
file.names <- list.files(path="01_Raw_data/HOBO Excels/3/SpC", pattern=".csv", full.names=TRUE)
SpC_3_all<-data.frame()
for(fil in file.names){
  SpC_3<-clean_SpC(fil)
  SpC_3_all<-rbind(SpC_3_all, SpC_3)
  SpC_3_all <- SpC_3_all[!duplicated(SpC_3_all[c('Date')]),]}
SpC_3_all$ID<-'3'

file.names <- list.files(path="01_Raw_data/HOBO Excels/5/SpC", pattern=".csv", full.names=TRUE)
SpC_5_all<-data.frame()
for(fil in file.names){
  SpC_5<-clean_SpC(fil)
  SpC_5_all<-rbind(SpC_5_all, SpC_5)
  SpC_5_all <- SpC_5_all[!duplicated(SpC_5_all[c('Date')]),]}
SpC_5_all$ID<-'5'

file.names <- list.files(path="01_Raw_data/HOBO Excels/5a/SpC", pattern=".csv", full.names=TRUE)
SpC_5a_all<-data.frame()
for(fil in file.names){
  SpC_5a<-clean_SpC(fil)
  SpC_5a_all<-rbind(SpC_5a_all, SpC_5a)
  SpC_5a_all <- SpC_5a_all[!duplicated(SpC_5a_all[c('Date')]),]}
SpC_5a_all$ID<-'5a'

file.names <- list.files(path="01_Raw_data/HOBO Excels/6/SpC", pattern=".csv", full.names=TRUE)
SpC_6_all<-data.frame()
for(fil in file.names){
  SpC_6<-clean_SpC(fil)
  SpC_6_all<-rbind(SpC_6_all, SpC_6)
  SpC_6_all <- SpC_6_all[!duplicated(SpC_6_all[c('Date')]),]}
SpC_6_all$ID<-'6'

file.names <- list.files(path="01_Raw_data/HOBO Excels/6a/SpC", pattern=".csv", full.names=TRUE)
SpC_6a_all<-data.frame()
for(fil in file.names){
  SpC_6a<-clean_SpC(fil)
  SpC_6a_all<-rbind(SpC_6a_all, SpC_6a)
  SpC_6a_all <- SpC_6a_all[!duplicated(SpC_6a_all[c('Date')]),]}
SpC_6a_all$ID<-'6a'

file.names <- list.files(path="01_Raw_data/HOBO Excels/7/SpC", pattern=".csv", full.names=TRUE)
SpC_7_all<-data.frame()
for(fil in file.names){
  SpC_7<-clean_SpC(fil)
  SpC_7_all<-rbind(SpC_7_all, SpC_7)
  SpC_7_all <- SpC_7_all[!duplicated(SpC_7_all[c('Date')]),]}
SpC_7_all$ID<-'7'

file.names <- list.files(path="01_Raw_data/HOBO Excels/9/SpC", pattern=".csv", full.names=TRUE)
SpC_9_all<-data.frame()
for(fil in file.names){
  SpC_9<-clean_SpC(fil)
  SpC_9_all<-rbind(SpC_9_all, SpC_9)
  SpC_9_all <- SpC_9_all[!duplicated(SpC_9_all[c('Date')]),]}
SpC_9_all$ID<-'9'

file.names <- list.files(path="01_Raw_data/HOBO Excels/13/SpC", pattern=".csv", full.names=TRUE)
SpC_13_all<-data.frame()
for(fil in file.names){
  SpC_13<-clean_SpC(fil)
  SpC_13_all<-rbind(SpC_13_all, SpC_13)
  SpC_13_all <- SpC_13_all[!duplicated(SpC_13_all[c('Date')]),]}
SpC_13_all$ID<-'13'

file.names <- list.files(path="01_Raw_data/HOBO Excels/14/SpC", pattern=".csv", full.names=TRUE)
SpC_14_all<-data.frame()
for(fil in file.names){
  SpC_14<-clean_SpC(fil)
  SpC_14_all<-rbind(SpC_14_all, SpC_14)
  SpC_14_all <- SpC_14_all[!duplicated(SpC_14_all[c('Date')]),]}
SpC_14_all$ID<-'14'

file.names <- list.files(path="01_Raw_data/HOBO Excels/15/SpC", pattern=".csv", full.names=TRUE)
SpC_15_all<-data.frame()
for(fil in file.names){
  SpC_15<-clean_SpC(fil)
  SpC_15_all<-rbind(SpC_15_all, SpC_15)
  SpC_15_all <- SpC_15_all[!duplicated(SpC_15_all[c('Date')]),]}
SpC_15_all$ID<-'15'

SpC<-rbind(SpC_3_all, SpC_5_all, SpC_5a_all, SpC_6_all, SpC_6a_all, SpC_7_all, SpC_9_all, SpC_13_all,
           SpC_14_all, SpC_15_all)

write_csv(SpC, "02_Clean_data/Chem/SpC_cleaned.csv")

###
###pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/3/pH", pattern=".xlsx", full.names=TRUE)
pH_3_all<-data.frame()
for(fil in file.names){
  pH_3<-clean_pH(fil)
  pH_3_all <- rbind(pH_3_all, pH_3)
  pH_3_all <- pH_3_all[!duplicated(pH_3_all[c('Date')]),]}
pH_3_all$ID<-'3'

file.names <- list.files(path="01_Raw_data/HOBO Excels/5/pH", pattern=".xlsx", full.names=TRUE)
pH_5_all<-data.frame()
for(fil in file.names){
  pH_5<-clean_pH(fil)
  pH_5_all <- rbind(pH_5_all, pH_5)
  pH_5_all <- pH_5_all[!duplicated(pH_5_all[c('Date')]),]}
pH_5_all$ID<-'5'

file.names <- list.files(path="01_Raw_data/HOBO Excels/5a/pH", pattern=".xlsx", full.names=TRUE)
pH_5a_all<-data.frame()
for(fil in file.names){
  pH_5a<-clean_pH(fil)
  pH_5a_all <- rbind(pH_5a_all, pH_5a)
  pH_5a_all <- pH_5a_all[!duplicated(pH_5a_all[c('Date')]),]}
pH_5a_all$ID<-'5a'

file.names <- list.files(path="01_Raw_data/HOBO Excels/6/pH", pattern=".xlsx", full.names=TRUE)
pH_6_all<-data.frame()
for(fil in file.names){
  pH_6<-clean_pH(fil)
  pH_6_all <- rbind(pH_6_all, pH_6)
  pH_6_all <- pH_6_all[!duplicated(pH_6_all[c('Date')]),]}
pH_6_all$ID<-'6'

file.names <- list.files(path="01_Raw_data/HOBO Excels/6a/pH", pattern=".xlsx", full.names=TRUE)
pH_6a_all<-data.frame()
for(fil in file.names){
  pH_6a<-clean_pH(fil)
  pH_6a_all <- rbind(pH_6a_all, pH_6a)
  pH_6a_all <- pH_6a_all[!duplicated(pH_6a_all[c('Date')]),]}
pH_6a_all$ID<-'6a'

file.names <- list.files(path="01_Raw_data/HOBO Excels/7/pH", pattern=".xlsx", full.names=TRUE)
pH_7_all<-data.frame()
for(fil in file.names){
  pH_7<-clean_pH(fil)
  pH_7_all <- rbind(pH_7_all, pH_7)
  pH_7_all <- pH_7_all[!duplicated(pH_7_all[c('Date')]),]}
pH_7_all$ID<-'7'

file.names <- list.files(path="01_Raw_data/HOBO Excels/9/pH", pattern=".xlsx", full.names=TRUE)
pH_9_all<-data.frame()
for(fil in file.names){
  pH_9<-clean_pH(fil)
  pH_9_all <- rbind(pH_9_all, pH_9)
  pH_9_all <- pH_9_all[!duplicated(pH_9_all[c('Date')]),]}
pH_9_all$ID<-'9'

file.names <- list.files(path="01_Raw_data/HOBO Excels/13/pH", pattern=".xlsx", full.names=TRUE)
pH_13_all<-data.frame()
for(fil in file.names){
  pH_13<-clean_pH(fil)
  pH_13_all <- rbind(pH_13_all, pH_13)
  pH_13_all <- pH_13_all[!duplicated(pH_13_all[c('Date')]),]}
pH_13_all$ID<-'13'

file.names <- list.files(path="01_Raw_data/HOBO Excels/14/pH", pattern=".xlsx", full.names=TRUE)
pH_14_all<-data.frame()
for(fil in file.names){
  pH_14<-clean_pH(fil)
  pH_14_all <- rbind(pH_14_all, pH_14)
  pH_14_all <- pH_14_all[!duplicated(pH_14_all[c('Date')]),]}
pH_14_all$ID<-'14'

file.names <- list.files(path="01_Raw_data/HOBO Excels/15/pH", pattern=".xlsx", full.names=TRUE)
pH_15_all<-data.frame()
for(fil in file.names){
  pH_15<-clean_pH(fil)
  pH_15_all <- rbind(pH_15_all, pH_15)
  pH_15_all <- pH_15_all[!duplicated(pH_15_all[c('Date')]),]}
pH_15_all$ID<-'15'

pH<-rbind(pH_3_all, pH_5_all, pH_5a_all, pH_6_all, pH_6a_all, pH_7_all, pH_9_all, pH_13_all, pH_14_all,
          pH_15_all)

write_csv(pH, "02_Clean_data/Chem/pH_cleaned.csv")

###CO2#####
CO23_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/3", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO23_csv<-clean_CO2_csv(fil)
CO23_csv_all<-rbind(CO23_csv_all, CO23_csv)}

CO23_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/3", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO23_dat <-clean_CO2_dat(fil)
CO23_dat_all<-rbind(CO23_dat_all, CO23_dat)}
LB3_CO2<-rbind(CO23_csv_all,CO23_dat_all)
LB3_CO2$ID<-'3'

CO25_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/5", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO25_csv<-clean_CO2_csv(fil)
CO25_csv_all<-rbind(CO25_csv_all, CO25_csv)}

CO25_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/5", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO25_dat <-clean_CO2_dat(fil)
CO25_dat_all<-rbind(CO25_dat_all, CO25_dat)}
LB5_CO2<-rbind(CO25_csv_all,CO25_dat_all)
LB5_CO2$ID<-'5'

CO25a_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/5a", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO25a_csv<-clean_CO2_csv(fil)
CO25a_csv_all<-rbind(CO25a_csv_all, CO25a_csv)}

CO25a_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/5a", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO25a_dat <-clean_CO2_dat(fil)
CO25a_dat_all<-rbind(CO25a_dat_all, CO25a_dat)}
LB5a_CO2<-rbind(CO25a_csv_all,CO25a_dat_all)
LB5a_CO2$ID<-'5a'

CO26_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/6", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO26_csv<-clean_CO2_csv(fil)
CO26_csv_all<-rbind(CO26_csv_all, CO26_csv)}

CO26_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/6", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO26_dat <-clean_CO2_dat(fil)
CO26_dat_all<-rbind(CO26_dat_all, CO26_dat)}
LB6_CO2<-rbind(CO26_csv_all,CO26_dat_all)
LB6_CO2$ID<-'6'

CO26a_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/6a", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO26a_csv<-clean_CO2_csv(fil)
CO26a_csv_all<-rbind(CO26a_csv_all, CO26a_csv)}

CO26a_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/6a", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO26a_dat <-clean_CO2_dat(fil)
CO26a_dat_all<-rbind(CO26a_dat_all, CO26a_dat)}
LB6a_CO2<-rbind(CO26a_csv_all,CO26a_dat_all)
LB6a_CO2$ID<-'6a'

CO27_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/7", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO27_csv<-clean_CO2_csv(fil)
CO27_csv_all<-rbind(CO27_csv_all, CO27_csv)}

CO27_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/7", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO27_dat <-clean_CO2_dat(fil)
CO27_dat_all<-rbind(CO27_dat_all, CO27_dat)}
LB7_CO2<-rbind(CO27_csv_all,CO27_dat_all)
LB7_CO2$ID<-'7'

CO29_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/9", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO29_csv<-clean_CO2_csv(fil)
CO29_csv_all<-rbind(CO29_csv_all, CO29_csv)}

CO29_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/9", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO29_dat <-clean_CO2_dat(fil)
CO29_dat_all<-rbind(CO29_dat_all, CO29_dat)}
LB9_CO2<-rbind(CO29_csv_all,CO29_dat_all)
LB9_CO2$ID<-'9'

CO213_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/13", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO213_csv<-clean_CO2_csv(fil)
CO213_csv_all<-rbind(CO213_csv_all, CO213_csv)}

CO213_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/13", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO213_dat <-clean_CO2_dat(fil)
CO213_dat_all<-rbind(CO213_dat_all, CO213_dat)}

CO213_vai_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/13/vaisala", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO213_dat <-clean_CO2_vaisala(fil)
CO213_vai_all<-rbind(CO213_vai_all, CO213_dat)}

LB13_CO2<-rbind(CO213_csv_all,CO213_dat_all,CO213_vai_all)
LB13_CO2$ID<-'13'

file.names <- list.files(path="01_Raw_data/Lily Box/dat/13/vaisala", pattern=".dat", full.names=TRUE)


CO214_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/14", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO214_csv<-clean_CO2_csv(fil)
CO214_csv_all<-rbind(CO214_csv_all, CO214_csv)}

CO214_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/14", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO214_dat <-clean_CO2_dat(fil)
CO214_dat_all<-rbind(CO214_dat_all, CO214_dat)}
LB14_CO2<-rbind(CO214_csv_all,CO214_dat_all)
LB14_CO2$ID<-'14'

CO215_csv_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/csv/15", pattern=".csv", full.names=TRUE)
for(fil in file.names){CO215_csv<-clean_CO2_csv(fil)
CO215_csv_all<-rbind(CO215_csv_all, CO215_csv)}

CO215_dat_all<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/dat/15", pattern=".dat", full.names=TRUE)
for(fil in file.names){CO215_dat <-clean_CO2_dat(fil)
CO215_dat_all<-rbind(CO215_dat_all, CO215_dat)}
LB15_CO2<-rbind(CO215_csv_all,CO215_dat_all)
LB15_CO2$ID<-'15'


#
# file.names <- list.files(path="01_Raw_data/Lily Box/csv/15", pattern=".csv", full.names=TRUE)
# for(fil in file.names){
#   LB_15CO2_csv<-clean_CO2_csv(fil)}
# file.names <- list.files(path="01_Raw_data/Lily Box/dat/15", pattern=".dat", full.names=TRUE)
# for(fil in file.names){
#   LB_15CO2_dat <-clean_CO2_dat(fil)}
# LB15_CO2<-rbind(LB_15CO2_csv,LB_15CO2_dat)

CO2<-rbind(LB3_CO2, LB5_CO2, LB5a_CO2, LB6_CO2, LB6a_CO2, LB7_CO2, LB9_CO2,
           LB13_CO2, LB14_CO2)
CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]

write_csv(CO2, "02_Clean_data/Chem/CO2_cleaned.csv")

####Compile####
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,3,1,4,5)]

data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
master<-join_all(data, by=c('Date','ID'), type='left')

master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>% filter(Date> "2021-11-16")
master <- master[!duplicated(master[c('Date','ID')]),]

write_csv(master, "02_Clean_data/master.csv")
detach("package:plyr", unload = TRUE)

###check#####
master<-filter(master, depth<3)
ggplot(master, aes(Date, depth)) + geom_line() + facet_wrap(~ ID, ncol=5)+
  theme_sam

  LB <- read_csv("01_Raw_data/Lily Box/dat/13/13_Bradford_LF_01152024.dat", skip= 3)
LB<-filter(LB, Date> '2023-12-01')
ggplot(DO_3_all, aes(Date, Temp)) + geom_line()
