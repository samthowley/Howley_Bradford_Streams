#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)

CO2 <- function(master) {
  master <- master[complete.cases(master[ , c('Temp_PT','CO2','pH','Water_press')]), ]
  master$Temp_PT<- fahrenheit.to.celsius(master$Temp_PT)
  master$Temp_PT_K<- master$Temp_PT+273.15

  master$exp<-2400*((1/master$Temp_PT_K)-(1/298.15))
  master$KH<-0.034*2.178^(master$exp)#mol/L/atm

  master$K1<-K1(S=0.01, T=master$Temp_PT, P=master$Water_press)
  master$K2<-K2(S=0.01, T=master$Temp_PT, P=master$Water_press)

  master$pK1<- -log10(master$K1)
  master$pK2<- -log10(master$K2)

  master$CO2_molL<-master$HCO3_molL/(10^(master$pH-master$pK1))

  master$CO2_atm<-master$CO2_molL/master$KH
  master$CO2_ppm<-master$CO2_atm*1000000


  x<-c("Date",'ID','CO2_molL')
  master<-master[,x]
  return(master)}


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
DC<-DC[,c("Site","ID","Date","DOC","DIC","Q_daily","Q_ID","depth_daily","Water_press","Temp_PT")]
master<-left_join(DC,POC, by=c('Date', 'Site'))

CO2 <- function(master) {
  master <- master[complete.cases(master[ , c('Temp_PT','CO2','pH','Water_press')]), ]
  master$Temp_PT<- fahrenheit.to.celsius(master$Temp_PT)
  master$Temp_PT_K<- master$Temp_PT+273.15

  master$exp<-2400*((1/master$Temp_PT_K)-(1/298.15))
  master$KH<-0.034*2.178^(master$exp)#mol/L/atm

  master$K1<-K1(S=0.01, T=master$Temp_PT, P=master$Water_press)
  master$K2<-K2(S=0.01, T=master$Temp_PT, P=master$Water_press)

  master$pK1<- -log10(master$K1)
  master$pK2<- -log10(master$K2)

  master$CO2_molL<-master$HCO3_molL/(10^(master$pH-master$pK1))

  master$CO2_atm<-master$CO2_molL/master$KH
  master$CO2_ppm<-master$CO2_atm*1000000


  x<-c("Date",'ID','CO2_molL')
  master<-master[,x]
  return(master)}


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

