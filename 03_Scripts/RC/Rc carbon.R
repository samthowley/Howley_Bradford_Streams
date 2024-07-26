#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)


RClog<-read_xlsx('01_Raw_data/RC log.xlsx')

DC_RC<-read_csv('04_Output/TC_RC.csv')
DC_RC<-DC_RC[DC_RC$Site %in% c('5GW1','5GW2','5GW3','5GW4','5GW5','5GW6','5GW7',
                                        '6GW1','6GW2','6GW3','6GW4','6GW5',
                                        '9GW1','9GW2','9GW3','9GW4'), ]
DIC_RC<-filter(DC_RC, Species=='DIC')
DIC_RC<-rename(DIC_RC, "DIC"="Conc.")
DIC_RC<-DIC_RC[,c("Site","Date","DIC" )]

DOC_RC<-filter(DC_RC, Species=='DOC')
DOC_RC<-rename(DOC_RC, "DOC"="Conc.")
DOC_RC<-DOC_RC[,c("Site","Date","DOC","Q_daily","Q_ID","depth_daily")]

DC<-left_join(DOC_RC, DIC_RC, by=c("Site","Date"))
C_RC<-left_join(DC, RClog, by=c("Site","Date"))
write_csv(C_RC, "02_Clean_data/allC_RC.csv")

###Interpolating CO2######
CO2 <- function(master) {
  master <- master[complete.cases(master[ , c('Temp','pH','Water_press','Q')]), ]
  master$Temp_K<- master$Temp+273.15

  master$exp<-2400*((1/master$Temp_K)-(1/298.15))
  master$KH<-0.034*2.178^(master$exp)#mol/L/atm

  master$K1<-K1(S=0.01, T=master$Temp, P=master$Water_press)
  master$K2<-K2(S=0.01, T=master$Temp, P=master$Water_press)

  master$pK1<- -log10(master$K1)
  master$pK2<- -log10(master$K2)

  master$HCO3_molL<-master$DIC/12010
  master$CO2_molL<-master$HCO3_molL/(10^(master$pH-master$pK1))

  master$CO2_atm<-master$CO2_molL/master$KH
  master$CO2_ppm_inter<-master$CO2_atm*1000000

  # master<-master[,c('Date', 'ID', 'Site', 'CO2_ppm_inter')]
  return(master)}

RCc<-read_csv("02_Clean_data/allC_RC.csv")
RCc$ID<-as.character(RCc$ID)

depth<-read_csv("02_Clean_data/depth.csv")
discharge<-read_csv("02_Clean_data/discharge.csv")

RCc<-left_join(RCc, depth, by=c('Date','ID'))
RCc<-left_join(RCc, discharge, by=c('Date','ID'))

CO2_inter<-CO2(RCc)
RCc<-left_join(RCc, CO2_inter, by=c('Date','ID','Site'))

#write_csv(C_RC, "02_Clean_data/allC_RC.csv")
