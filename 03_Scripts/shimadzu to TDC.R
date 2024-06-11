#packages#####
rm(list=ls())

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
  filter(Conc. != Inf)
carbon$Conc.[carbon$Conc.<0]<-NA

x<-c("ID","Date",'Species',"Conc.","Conc_Raw",'day','month','year')
carbon<-carbon[,x]

carbon<-rename(carbon, 'Site'='ID')
carbon<-carbon %>% mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                    Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                    Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',
                                    Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                    Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                    Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                    Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                    Site=='9GW4'~'9'))


carbon<-carbon %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                       Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                       Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream',

                                       Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                       Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='6GW1'~'RC',
                                       Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                       Site=='6GW6'~'RC',Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                       Site=='9GW4'~'RC'))


# alkalinity <- read_csv("02_Clean_data/alkalinity.csv")
# alkalinity<-alkalinity %>% mutate(day=day(Date), month=month(Date), year=year(Date))
# alkalinity<-alkalinity[,-1]
# check<-left_join(carbon, alkalinity, by=c('day','month','year','ID'))
# check <- check[!duplicated(check[c('ID','Date','Species')]),]
# write_csv(check, "check.csv")


discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(day=day(Date), month=month(Date), year=year(Date))
discharge<-discharge %>% group_by(ID,day, month,year) %>% mutate(Q_daily=mean(Q, na.rm=T))
discharge<-discharge[,c('ID','Q_daily', 'Q_ID','day','month','year')]

depth <- read_csv("02_Clean_data/depth.csv")
depth<-depth %>% mutate(day=day(Date), month=month(Date), year=year(Date))
depth<-depth %>% group_by(ID,day, month,year) %>% mutate(depth_daily=mean(depth, na.rm=T))
depth<-depth[,c("day","month","year","depth_daily", 'ID')]

carbon<-left_join(carbon, discharge,by=c('day','month','year','ID'))
carbon<-left_join(carbon, depth,by=c('day','month','year','ID'))

carbon<- carbon %>% filter(ID != '9a',ID != '9b', ID!='14')
carbon <- carbon[!duplicated(carbon[c('Site','Date','Species')]),]

stream<-filter(carbon, chapter=='stream')
RC<-filter(carbon, chapter=='RC')

RClog<-read_csv('RC log.csv')
RClog<-RClog %>%mutate(Date=mdy(Date))
RC_all<- left_join(RC,RClog, by=c('Date','Site'))

write_csv(RC_all, "02_Clean_data/TC_RC.csv")
write_csv(stream, "02_Clean_data/TC_stream.csv")

ggplot(stream, aes(x=depth_daily, y=Conc.,color=Species)) +
  geom_point()+facet_wrap(~ Site, ncol=5)

ggplot(RC, aes(x=depth_daily, y=Conc.,color=Species)) +
  geom_point()+facet_wrap(~ Site, ncol=5)

