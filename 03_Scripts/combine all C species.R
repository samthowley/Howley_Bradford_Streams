#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)

POC<-read_xlsx('01_Raw_data/POC.xlsx')
keep_POC<-c("Sampled","ID","mg/L")
POC<-POC[,keep_POC]
POC<-POC %>% rename('Date'='Sampled', 'Site'='ID', 'POC_mgL'='mg/L')

DC_strm<-read_csv('04_Output/TC_stream.csv')
DIC_strm<-filter(DC_strm, Species=='DIC')
DIC_strm<-rename(DIC_strm, "DIC"="Conc.")
DIC_strm<-DIC_strm[,c("Site","Date","DIC" )]

DOC_strm<-filter(DC_strm, Species=='DOC')
DOC_strm<-rename(DOC_strm, "DOC"="Conc.")
DC<-left_join(DOC_strm, DIC_strm, by=c("Site","Date"))

C_strm<-left_join(DC,POC, by=c('Date', 'Site'))
write_csv(C_strm, "02_Clean_data/allC_stream.csv")



RClog<-read_csv('01_Raw_data/RC log.csv')
RClog<-RClog %>% mutate(Date=mdy(Date))

DC_RC<-read_csv('04_Output/TC_RC.csv')
DIC_RC<-filter(DC_RC, Species=='DIC')
DIC_RC<-rename(DIC_RC, "DIC"="Conc.")
DIC_RC<-DIC_RC[,c("Site","Date","DIC" )]

DOC_RC<-filter(DC_RC, Species=='DOC')
DOC_RC<-rename(DOC_RC, "DOC"="Conc.")
DOC_RC<-DOC_RC[,c("Site","Date","DOC","Q_daily","Q_ID","depth_daily")]

DC<-left_join(DOC_RC, DIC_RC, by=c("Site","Date"))
C_RC<-left_join(RClog, DC, by=c("Site","Date"))
write_csv(C_RC, "02_Clean_data/allC_RC.csv")

