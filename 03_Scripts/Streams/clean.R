#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2022-11-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-08-01 00:00", tz="UTC"),by="hour")))

clean_DO <- function(fil) {
  DO <- read_csv(fil,skip= 1)
  DO<-DO[,c(2,3,4)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp_DO"
  DO$Date <- mdy_hms(DO$Date)
  DO<-left_join(DO, samplingperiod, by='Date')
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(DO)}
MiniDot_DO<-function(fil){
  DO <- read_csv(i,skip= 8)
  DO<-DO[,c(2,6,5)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp_DO"
  DO<-left_join(DO, samplingperiod, by='Date')
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]

  # for(i in 1:nrow(DO)){
  # if(DO$DO[i]<=0 | DO$DO[i]>=7.4) { DO$DO[i]<- NA}
  # else {DO$DO[i]<- DO$DO[i]-0 }}
  return(DO)}
clean_SpC <- function(fil) {
  SpC <- read_csv(fil, skip= 1)
  SpC<-SpC[,c(2,3,4)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  colnames(SpC)[3] <- "Temp_SpC"
  SpC$Date <- mdy_hms(SpC$Date)
  SpC<-left_join(SpC, samplingperiod, by='Date')
  SpC$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(SpC)
}
clean_pH <- function(i) {
  pH <- read_xlsx(i)
  pH<-pH[,c(2,5,3)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  colnames(pH)[3] <- "Temp_pH"
  pH$Temp_pH<-celsius.to.fahrenheit(pH$Temp_pH)
  #pH<-filter(pH, pH<6.2) #remove hours out of water
  pH$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(pH)}
theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 17, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_text(size = 17),
                             legend.position ="none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

####DO######
file.names <- list.files(path="01_Raw_data/HOBO Excels/DO", pattern=".csv", full.names=TRUE)

DO_all<-data.frame()
for(i in file.names){
  DO<-clean_DO(i)
  DO_all<-rbind(DO_all, DO)
  DO_all[order(as.Date(DO_all$date, format="%Y-%m-%d %H:%M:%S")),]
}

file.names <- list.files(path="01_Raw_data/MiniDot", pattern=".TXT", full.names=TRUE)
for(i in file.names){
  DO<-MiniDot_DO(i)
  DO_all<-rbind(DO_all, DO)
  DO_all[order(as.Date(DO_all$date, format="%Y-%m-%d %H:%M:%S")),]
}

DO_all$DO[DO_all$DO<0]<-NA
DO_all$Temp_DO[DO_all$Temp_DO<0]<-NA
DO_all$DO[DO_all$DO>10]<-NA


DO_all<- DO_all[!duplicated(DO_all[c('Date','ID')]),]
ggplot(DO_all, aes(Date, DO)) + geom_line() + facet_wrap(~ ID, ncol=4)
range(DO_all$Date)

write_csv(DO_all, "02_Clean_data/DO_cleaned.csv")
####SpC####
file.names <- list.files(path="01_Raw_data/HOBO Excels/SpC", pattern=".csv", full.names=TRUE)
SpC_all<-data.frame()
for(i in file.names){
  SpC<-clean_SpC(i)
  SpC_all<-rbind(SpC_all, SpC)
}

SpC_all$SpC[SpC_all$SpC>600]<-NA
SpC_all$Temp_SpC[SpC_all$Temp_SpC<0]<-NA
SpC_all$Temp_SpC[SpC_all$Temp_SpC>30]<-NA

SpC_all <- SpC_all[!duplicated(SpC_all[c('Date','ID')]),]
ggplot(SpC_all, aes(Date, SpC)) + geom_line() + facet_wrap(~ ID, ncol=4)
range(SpC_all$Date,na.rm=T)

write_csv(SpC_all, "02_Clean_data/SpC_cleaned.csv")

#range(SpC_all$Date, na.rm=T)
###pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/pH", pattern=".xlsx", full.names=TRUE)
pH_all<-data.frame()
for(fil in file.names){
  pH<-clean_pH(fil)
  pH_all <- rbind(pH_all, pH)
  }
pH_all <- pH_all[!duplicated(pH_all[c('Date','ID')]),]

pH_all$pH[pH_all$pH>8]<-NA
ggplot(pH_all, aes(Date, pH)) + geom_line() + geom_hline(yintercept=8)+
  facet_wrap(~ ID, ncol=4)
range(pH_all$Date,na.rm=T)

write_csv(pH_all, "02_Clean_data/pH_cleaned.csv")

####Compile####
file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,4,6,7,8,9,1)]

data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
master<-join_all(data, by=c('Date','ID'), type='left')

master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>%
  filter(Date> "2021-11-16")
master <- master[!duplicated(master[c('Date','ID')]),]
detach("package:plyr", unload = TRUE)

#compile Temp
master$Temp_PT[master$Temp_PT>87]<-NA
master$Temp_PT[master$Temp_PT<0]<-NA

master$Temp_PT <- ifelse(is.na(master$Temp_PT), master$Temp_pH, master$Temp_PT)
master$Temp_PT <- ifelse(is.na(master$Temp_PT), master$Temp_DO, master$Temp_PT)

ggplot(master, aes(x=Date)) + geom_line(aes(y=DO))+facet_wrap(~ ID, ncol=5)

master<-master[,c("Date","depth","ID","Q","Qbase","CO2","DO","pH","SpC","Temp_PT","Water_press")]
master<-rename(master, 'Temp'="Temp_PT")
range(master$Date)

write_csv(master, "master.csv")

#TEST##########
write_csv(X5_Bradford_LB_05302024, "test.csv")
