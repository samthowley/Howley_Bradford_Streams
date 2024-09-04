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
vials<- vials %>% mutate(`Sample Date`=mdy(`Sample Date`))

results<-data.frame()
file.names <- list.files(path="01_Raw_data/Shimadzu/results",pattern=".csv", full.names=TRUE)
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

file.names <- list.files(path="01_Raw_data/Shimadzu/csv files", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  runs<-read_csv(fil)
  DIC<-runs %>%select(`IC Interpolation`,Vial,Ran)%>%mutate(Ran=mdy(Ran),Species='DIC', Vial=as.character(Vial))%>%
    rename('Conc'='IC Interpolation')
  DOC<-runs %>%select(TOC,Vial,Ran)%>%mutate(Ran=mdy(Ran),Species='DIC', Vial=as.character(Vial))%>%
    rename('Conc'='TOC')
  complete<-rbind(DIC,DOC)
  results<-rbind(results, complete)
}

together<-left_join(vials,results,by=c('Vial','Ran'))

carbon<-together %>%
  rename('ID'='Site', 'Date'='Sample Date','Conc_Raw'='Conc') %>%
  mutate(Conc.= Conc_Raw*(1/DF))%>%
  filter(Conc. != Inf) %>% select(Date, ID, Species, Conc., Conc_Raw)

carbon<-rename(carbon, 'Site'='ID')
carbon<-carbon %>% mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                    Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                    Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',
                                    Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                    Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                    Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                    Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                    Site=='9GW4'~'9',Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
                                    Site=='5.4'~'5',Site=='5.5'~'5',Site=='6.1'~'6',Site=='6.2'~'6',
                                    Site=='6.3'~'6',Site=='6.4'~'6',Site=='6.5'~'6',Site=='6.6'~'6',
                                    Site=='9.1'~'9',Site=='9.2'~'9',Site=='9.3'~'9',Site=='9.4'~'9',
                                    Site=='9.5'~'9',Site=='9.6'~'9',Site=='9.Sam'~'9'))


carbon<-carbon %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                            Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream',

                                            Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                            Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='6GW1'~'RC',
                                            Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                            Site=='6GW6'~'RC',Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                            Site=='9GW4'~'RC',

                                            Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                            Site=='5.4'~'long',Site=='5.5'~'long',Site=='6.1'~'long',Site=='6.2'~'long',
                                            Site=='6.3'~'long',Site=='6.4'~'long',Site=='6.5'~'long',Site=='6.6'~'long',
                                            Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                            Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

carbon$chapter[is.na(carbon$chapter)]<-'wetland'
wetland<-filter(carbon, chapter=='wetland')

write_csv(wetland, "04_Output/TC_wetland.csv")

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,6,8)]
data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
bgc<-join_all(data, by=c('Date','ID'), type='left')
detach("package:plyr", unload = TRUE)

bgc<-bgc %>%mutate(Date=as.Date(Date))%>%group_by(Date,ID)%>%
  mutate(depth=mean(depth, na.rm=T), pH=mean(pH, na.rm=T), Q==mean(Q, na.rm=T)) %>%
           select(Date,ID,depth,pH,Q)

carbon<-left_join(carbon, bgc,by=c('Date','ID'))

carbon<- carbon %>% filter(ID != '9a',ID != '9b', ID!='14')
carbon <- carbon[!duplicated(carbon[c('Site','Date','Species')]),]

stream<-filter(carbon, chapter=='stream')
RC<-filter(carbon, chapter=='RC')
long<-filter(carbon, chapter=='long')


write_csv(RC, "04_Output/TDC_RC.csv")
write_csv(stream, "04_Output/TDC_stream.csv")
write_csv(long, "04_Output/TDC_long.csv")

RC<-read.csv("04_Output/TDC_RC.csv")
stream<-read.csv("04_Output/TDC_stream.csv")
long<-read.csv("04_Output/TDC_long.csv")

long<-long %>% mutate(location=case_when(Site=='5.1'~'1',Site=='5.2'~'2',Site=='5.3'~'3',
                                            Site=='5.4'~'4',Site=='5.5'~'5',Site=='6.1'~'1',Site=='6.2'~'2',
                                            Site=='6.3'~'3',Site=='6.4'~'4',Site=='6.5'~'5',Site=='6.6'~'6',
                                            Site=='9.1'~'1',Site=='9.2'~'2',Site=='9.3'~'3',Site=='9.4'~'4',
                                            Site=='9.5'~'5',Site=='9.6'~'6',Site=='9.Sam'~'7'))%>%
  mutate(location=as.numeric(location))



ggplot(stream, aes(x=Q, y=Conc.,color=Species)) +
  geom_point(size=2)+facet_wrap(~ Site, ncol=5, scales = "free")

ggplot(long, aes(x=location, y=Conc.,color=Species)) +
  geom_point(size=2)+facet_wrap(~ ID, ncol=5)+ylab("longitudinal sampling")

#check#####
TOC <- read_csv("01_Raw_data/Shimadzu/dat files/duds/2024_07_11_Howley_TOC_STREAMS_norm.txt",
                                                skip = 10)
write_csv(TOC, "01_Raw_data/Shimadzu/csv files/TOC_07112024.csv")

