#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2023-12-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-05-31 00:00", tz="UTC"),by="hour")))
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
#CO2#####
CO2<-data.frame()

file.names <- list.files(path="01_Raw_data/Lily Box/vaisala", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1)
  colnames(LB)[5] <- "CO2"
  LB<-LB[-c(1,2),]
  x<-c('TIMESTAMP','CO2')
  LB<-LB[,x]
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB<-LB %>% rename('Date'='TIMESTAMP') %>% mutate(Date=ymd_hms(Date), CO2=as.numeric(CO2))%>%
    filter(CO2>1000)
  CO2<-rbind(LB, CO2)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}

file.names <- list.files(path="01_Raw_data/Lily Box/vaisala/13", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 3)
  colnames(LB)[1] <- "Date"
  colnames(LB)[4] <- "CO2"
  x<-c('Date','CO2')
  LB<-LB[,x]
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB$CO2<-LB$CO2*6
  LB<-LB %>% mutate(Date=ymd_hms(Date), CO2=as.numeric(CO2))%>%
    filter(Date> "2024-03-10", CO2>1000)
  CO2<-rbind(LB, CO2)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}
ggplot(CO2, aes(Date, CO2)) + geom_line()+ facet_wrap(~ ID, ncol=4)

file.names <- list.files(path="01_Raw_data/Lily Box/csv", pattern=".csv", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i)
  LB<-LB[,c(1,5)]
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][4]
  LB$Date<-mdy_hms(LB$Date)
  LB<-LB %>% filter(CO2>1000)
  CO2<-rbind(CO2, LB)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}

file.names <- list.files(path="01_Raw_data/Lily Box/dat", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 3)
  LB<-LB[,c(1,4)]
  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB<-LB %>% filter(CO2>1000)
  CO2<-rbind(CO2, LB)
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]}

CO2<-CO2 %>% mutate(day=day(Date),hour=hour(Date),month=month(Date),yr=year(Date)) %>%
  filter(Date>'2023-12-06')
CO2<-CO2 %>% group_by(hour, day, month, yr,ID) %>% mutate(CO2=mean(CO2, na.rm=T))

y<-c("Date",'CO2','ID')
CO2<-CO2[,y]

##############
ggplot(CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=4)+theme_sam
write_csv(CO2, "02_Clean_data/CO2_cleaned.csv")

test <- read_csv("01_Raw_data/Lily Box/dat/5_Bradford_LB_05302024.dat", skip=1)
test<-test[-c(1,2),-c(2,3,5,6)]
test<-test %>% rename('Date'='TIMESTAMP', 'Eosense'='CO2High', 'Vaisala'='CO2')
test<-test %>% mutate(Date=ymd_hms(Date), Eosense=as.numeric(Eosense),
                      Vaisala=as.numeric(Vaisala)) %>% filter(Date>"2024-05-08 00:00:00")
test<-test %>%mutate(Vaisala=Vaisala*4.2)

discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% filter(ID=='5', Date>"2024-05-08 00:00:00")
discharge<-discharge[,c(1,3)]
test<-left_join(test, discharge, by='Date')

ggplot(test, aes(Date))+
  geom_line(aes(y=Vaisala+1900, color= "Vaisala"))+geom_line(aes(y=Eosense, color= "Eosense"))

write_csv(test, "test3.csv")



air_test <- read_csv("Matts Wetlands_Test.dat",skip=1)
air_test<-air_test[-c(1,2),-c(2,3)]
air_test<-air_test %>% rename('Date'='TIMESTAMP')
air_test<-air_test %>% mutate(Date=ymd_hms(Date), Eosense=as.numeric(Eosense),
                      Vaisala=as.numeric(Vaisala), K30=as.numeric(K30))

write_xlsx(air_test, "air_test.xlsx")
