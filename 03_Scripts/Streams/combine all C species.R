#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(ggtern)
library(ggpmisc)

depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')
length<-read_csv('02_Clean_data/stream area.csv')

#Edit dims######
depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth, Temp_PT)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(Q, depth, by=c('ID', 'Date'))

#sample C##########
POC<-read_xlsx('01_Raw_data/POC.xlsx')
POC<-POC %>% rename('Date'='Sampled', 'POC_mgL'='mg/L')%>%
  mutate(POC_mmol=POC_mgL/44.01) %>% select(ID,Date,POC_mgL,POC_mmol) %>%filter(POC_mgL<200)
POC<-POC[POC$ID %in% c("6","5a","7","9","5","6a","13","15","3"), ]

DC_strm<-read_csv('04_Output/TDC_stream.csv')
DIC_strm<-DC_strm %>% filter(Species=='DIC') %>% rename("DIC"="Conc.","DIC_mmol"='mmol')%>%
  select(ID, Date, DIC, DIC_mmol)
DOC_strm<-DC_strm %>% filter(Species=='DOC') %>% rename("DOC"="Conc.","DOC_mmol"='mmol') %>%
  select(ID, Date, DOC, DOC_mmol)
DC<-left_join(DOC_strm, DIC_strm, by=c("ID","Date"))

totDC<-left_join(DC,POC, by=c("ID","Date"))
totDC<-totDC %>% select(ID,Date,POC_mgL,DIC,DOC,POC_mmol,DIC_mmol,DOC_mmol)
totDC <- totDC[rev(order(as.Date(totDC$Date, format="%m/%d/%Y"))),]

totDC <- totDC[!duplicated(totDC[c('ID','Date')]),]
totDC<-left_join(totDC, dim, by=c('ID', 'Date'))

ggplot(totDC, aes(Q))+
  geom_point(aes(y=DIC, color= "DIC")) +
  #geom_point(aes(y=DOC, color='DOC')) +
  scale_x_log10()+scale_y_log10()+
  facet_wrap(~ ID, ncol=3, scales='free')

site<-totDC %>% filter(ID=='5')
ggtern(data=site,aes(DOC,DIC,POC_mgL, colour = Q))+scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC_mgL",y="DIC_mgL",z="POC_mgL")+facet_wrap(~ ID, ncol=3, scales='free')


write_csv(totDC, "04_Output/stream_sampledC.csv")
