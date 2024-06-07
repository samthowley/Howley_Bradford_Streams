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
ggplot(CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=4)+theme_sam

# CO2<- read_csv("02_Clean_data/CO2_cleaned.csv")
# range(CO2$Date, na.rm=T)
write_csv(CO2, "02_Clean_data/CO2_cleaned.csv")

