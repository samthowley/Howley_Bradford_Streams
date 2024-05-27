#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)


VialID <- function(sample) {
  sample <- read_csv(fil)
  sample$Date<-mdy(sample$'Sample Date')
  sample$Ran<-mdy(sample$Ran)
  sample$Vial<-as.character(sample$Vial)
  sample<-sample[,c(1:6)]
  return(sample)} #function for cleaning data

file.names <- list.files(path="01_Raw_data/Shimadzu/ID", pattern=".csv", full.names=TRUE)
vials<-data.frame()
for(fil in file.names){
  vial<-VialID(fil)
  vials<-rbind(vials, vial)}
vials<- vials %>% mutate(day=day(Ran),month=month(Ran), year=year(Ran), `Sample Date`=mdy(`Sample Date`))

file.names <- list.files(path="01_Raw_data/Shimadzu/results",pattern=".csv", full.names=TRUE)
results<-data.frame()
for(fil in file.names){
  runs<-read_csv(fil)
  runs$Ran<-mdy(runs$Ran)
  runs<-runs %>% rename('Conc'='Result(NPOC)') %>%mutate(Species='DOC')
  runs<-runs[,-2]
  results<-rbind(results, runs)}

file.names <- list.files(path="01_Raw_data/Shimadzu/dat files", pattern=".txt", full.names=TRUE)
for(fil in file.names){
  runs<-read_csv(fil, skip=10)
  runs<- runs %>% filter(`Anal.`=='NPOC') %>% rename('Ran'="Date / Time" )
  runs<-runs[c(9,12,13)]
  runs$Ran<-mdy_hms(runs$Ran)
  runs<-runs %>% rename('Conc'='Result(NPOC)') %>%mutate(Species='DOC')
  results<-rbind(results, runs)}

file.names <- list.files(path="01_Raw_data/Shimadzu/dat files/TOC runs", pattern=".txt", full.names=TRUE)
for(fil in file.names){
  runs<-read_csv(fil, skip=10)
  runs<- runs %>% filter(`Sample Name`!='Untitled') %>% rename('Ran'="Date / Time" )

  DOC<-runs[,c('Sample Name',"Result(TOC)","Vial","Ran")]
  DOC<- DOC %>% rename('Conc'="Result(TOC)")%>%mutate(Species='DOC')

  DIC<-runs[,c('Sample Name',"Result(TC)","Vial","Ran")]
  DIC<- DIC %>% rename('Conc'="Result(TC)")%>%mutate(Species='DIC')

  parsed<-rbind(DIC, DOC)
  parsed$Ran<-mdy_hms(parsed$Ran)
  parsed<-parsed[,-1]
  results<-rbind(results, parsed)}

results<-results %>% mutate(day=day(Ran), month=month(Ran), year=year(Ran))

together<-left_join(vials,results,by=c('Vial','day','month','year'))

carbon<-together %>%
  rename('ID'='Site', 'Date'='Sample Date','Conc_Raw'='Conc') %>%
  mutate(Conc.= Conc_Raw*(1/DF), day=day(Date), month=month(Date), year=year(Date))%>%
  filter(Conc. != Inf | Conc. <= 0)

x<-c("ID","Date",'Species',"Conc.","Conc_Raw",'day','month','year')
carbon<-carbon[,x]

alkalinity <- read_csv("02_Clean_data/alkalinity.csv")
alkalinity<-alkalinity %>% mutate(day=day(Date), month=month(Date), year=year(Date))
alkalinity<-alkalinity[,-1]
check<-left_join(carbon, alkalinity, by=c('day','month','year','ID'))
check <- check[!duplicated(check[c('ID','Date','Species')]),]
write_csv(check, "check.csv")

discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(day=day(Date), month=month(Date), year=year(Date))
discharge<-discharge %>% group_by(ID,day, month,year) %>% mutate(Q_daily=mean(Q, na.rm=T))
discharge<-discharge[,c('ID',"Q",'Qbase','Q_ID','Q_daily','day','month','year')]
range(discharge$Date, na.rm=T)

depth <- read_csv("02_Clean_data/depth.csv")
depth<-depth %>% mutate(day=day(Date), month=month(Date), year=year(Date))
depth<-depth %>% group_by(ID,day, month,year) %>% mutate(depth_daily=mean(depth, na.rm=T))
depth<-depth[,c(2,5,6,7,8)]

carbon<-left_join(carbon, discharge,by=c('day','month','year','ID'))
carbon<-left_join(carbon, depth,by=c('day','month','year','ID'))

carbon<- carbon %>% filter(ID != '9a',ID != '9b') %>%
  mutate(logQ_daily=log10(Q_daily))
carbon <- carbon[!duplicated(carbon[c('ID','Date','Species')]),]

x<-c("Date","ID","Conc.","Conc_Raw","Species","Q_daily",'depth_daily', 'Q_ID')
carbon<-carbon[,x]

range(carbon$Date, na.rm=T)
write_csv(carbon, "02_Clean_data/TC.csv")
