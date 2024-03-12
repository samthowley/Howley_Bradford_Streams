#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2022-11-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-03-29 00:00", tz="UTC"),by="hour")))

clean_DO <- function(fil) {
  DO <- read_csv(fil,skip= 1)
  DO<-DO[,c(2,3,4)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO$Date <- mdy_hms(DO$Date)
  DO<-left_join(DO, samplingperiod, by='Date')
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  #DO<-DO %>% mutate(min=minute(Date)) %>% filter(min==0) %>% filter(DO>0)
  #DO<-DO[,-5]
  # for(i in 1:nrow(DO)){if(DO$DO[i]<=0 | DO$DO[i]>=6.8) { DO$DO[i]<- NA}
  #   else {DO$DO[i]<- DO$DO[i]-0 }} #remove hours out of the water and erraneous days
  return(DO)}
MiniDot_DO<-function(fil){
  DO <- read_csv(i,skip= 8)
  DO<-DO[,c(2,6,5)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO<-left_join(DO, samplingperiod, by='Date')
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]

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
  SpC<-left_join(SpC, samplingperiod, by='Date')
  SpC$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(SpC)
}
clean_pH <- function(i) {
  pH <- read_xlsx(i)
  pH<-pH[,c(2,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  #pH<-filter(pH, pH<6.2) #remove hours out of water
  pH$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(pH)}
clean_CO2_csv<-function(i){
  LB <- read_csv(i)
  LB<-LB[,c(1,5)]
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][4]
  LB$Date<-mdy_hms(LB$Date)
  return(LB)}
clean_CO2_dat<-function(fil){
  LB <- read_csv(fil, skip= 3)
  LB<-LB[,c(1,4)]
  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
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

DO_all<-filter(DO_all, DO>0)
DO_all<- DO_all[!duplicated(DO_all[c('Date','ID')]),]
ggplot(DO_all1, aes(Date, DO)) + geom_line() + facet_wrap(~ ID, ncol=4)+theme_sam

write_csv(DO_all, "02_Clean_data/DO_cleaned.csv")

####SpC####
file.names <- list.files(path="01_Raw_data/HOBO Excels/SpC", pattern=".csv", full.names=TRUE)
SpC_all<-data.frame()
for(i in file.names){
  SpC<-clean_SpC(i)
  SpC_all<-rbind(SpC_all, SpC)
}
SpC_all<-filter(SpC_all, SpC<1000)
SpC_all <- SpC_all[!duplicated(SpC_all[c('Date','ID')]),]
ggplot(SpC_all, aes(Date, SpC)) + geom_line() + facet_wrap(~ ID, ncol=4)+theme_sam
write_csv(SpC_all, "02_Clean_data/SpC_cleaned.csv")

###pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/pH", pattern=".xlsx", full.names=TRUE)
pH_all<-data.frame()
for(fil in file.names){
  pH<-clean_pH(fil)
  pH_all <- rbind(pH_all, pH)
  }
pH_all <- pH_all[!duplicated(pH_all[c('Date','ID')]),]

ggplot(pH_all, aes(Date, pH)) + geom_line() + facet_wrap(~ ID, ncol=4)+theme_sam

write_csv(pH_all, "02_Clean_data/pH_cleaned.csv")

###CO2#####
CO2<-data.frame()
file.names <- list.files(path="01_Raw_data/Lily Box/vaisala", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1)
  colnames(LB)[4] <- "CO2"
  LB<-LB[-c(1,2),]
  x<-c('TIMESTAMP','CO2')
  LB<-LB[,x]
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB<-LB %>% rename('Date'='TIMESTAMP') %>% mutate(Date=ymd_hms(Date), CO2=as.numeric(CO2))
  CO2<-rbind(LB, CO2)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}

file.names <- list.files(path="01_Raw_data/Lily Box/csv", pattern=".csv", full.names=TRUE)
for(i in file.names){
  CO2_csv<-clean_CO2_csv(i)
  CO2<-rbind(CO2, CO2_csv)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}

file.names <- list.files(path="01_Raw_data/Lily Box/dat", pattern=".dat", full.names=TRUE)
for(i in file.names){
  CO2_dat <-clean_CO2_dat(i)
  CO2<-rbind(CO2, CO2_dat)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}

CO2<-CO2 %>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
CO2<-CO2 %>% group_by(hour, day, month, yr,ID) %>% mutate(CO2=mean(CO2, na.rm=T))

y<-c("Date",'CO2','ID',"day")
CO2<-CO2[,y]
ggplot(CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=4)+theme_sam

write_csv(CO2, "02_Clean_data/CO2_cleaned.csv")
####Compile####
file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(4,2,1,3,7,6,8)]

data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
master<-join_all(data, by=c('Date','ID'), type='left')

master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>% filter(Date> "2021-11-16")
master <- master[!duplicated(master[c('Date','ID')]),]

ggplot(master, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=5)

write_csv(master, "02_Clean_data/master.csv")
detach("package:plyr", unload = TRUE)

