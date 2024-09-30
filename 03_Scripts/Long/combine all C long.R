#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)

depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')

#Edit dims######

depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth)%>% filter(depth>0)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial) %>% filter(Q>1)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(depth, Q, by=c('ID', 'Date'))

#############

longlog<-read_csv('01_Raw_data/Long. Log.csv')
longlog<-longlog %>% rename('Sampled'="Visited")

POC<-read_xlsx('01_Raw_data/POC.xlsx')
POC<-POC %>% select(ID, Sampled, "mg/L") %>%rename('Date'='Sampled', 'POC_mgL'='mg/L', 'Site'='ID')
POC<-POC[POC$Site %in% c("9.1","9.2","9.3","9.4","9.5","9.Sam",
                       "5.1","5.2","5.3","5.4","5.5","5.6",
                       "3.1","3.2","3.3","3.4","6.1","6.2","6.3"),]


DC<-read_csv('04_Output/TDC_long.csv')
DIC<-DC %>% filter(Species=='DIC') %>% rename("DIC"="Conc.") %>% select(Site, ID, Date, DIC)
DOC<-DC %>% filter(Species=='DOC') %>% rename("DOC"="Conc.") %>% select(Site, ID, Date, DOC)
DC<-left_join(DOC, DIC, by=c("Site","Date", 'ID'))

totC<-left_join(DC,POC, by=c("Site","Date"))
totC<-totC %>% mutate(ID=as.character(ID))
# totDC <- totDC[rev(order(as.Date(totDC$Date, format="%m/%d/%Y"))),]
# totDC$POC_mgL[totDC$POC_mgL>200]<-NA

totC<-left_join(totC,dim, by=c("ID","Date"))
totC_separated <- totC %>%
  separate(Site, into = c("Stream", "Long"), sep = "\\.")


ggplot(totC_separated, aes(x=Long, color= Q))+
  #scale_color_gradient(high='red', low='blue')+
  geom_point(aes(y=DOC, color = "DOC")) +
  geom_point(aes(y=DIC, color="DIC"))+
  facet_wrap(~Stream, ncol=3, scale='free')+
  theme(legend.position = "bottom")
