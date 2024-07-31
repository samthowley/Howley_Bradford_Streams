#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)


RClog<-read_xlsx('01_Raw_data/RC log.xlsx')
RClog<- RClog%>% select(Date, ID, Site, WTdepth, CO2_mv)

DC_RC<-read_csv('04_Output/TC_RC.csv')

DIC_RC<-DC_RC %>%filter(Species=='DIC') %>%rename("DIC_mgL"="Conc.") %>% select(Site, Date, DIC_mgL)
DOC_RC<-DC_RC %>%filter(Species=='DOC') %>%rename("DOC_mgL"="Conc.") %>% select(Site, Date, DOC_mgL)

C_RC<-left_join(RClog,DIC_RC , by=c("Site","Date"))
C_RC<-left_join(C_RC, DOC_RC, by=c("Site","Date"))

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
