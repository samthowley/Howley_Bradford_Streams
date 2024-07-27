#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)

CO2mmol <- function(CO2) {
  CO2$Temp_C<-fahrenheit.to.celsius(CO2$Temp_PT)
  CO2$Temp_K<-CO2$Temp_C+273.15
  CO2$exp<-2400*((1/CO2$Temp_K)-(1/298.15))
  CO2$KH<-0.034*2.178^(CO2$exp)#mol/L/atm

  CO2$CO2_atm<-CO2$CO2/1000000*1000
  CO2$CO2obs_mmol<-CO2$CO2_atm*CO2$KH
  return(CO2)}

#TDC##########
POC<-read_xlsx('01_Raw_data/POC.xlsx')
keep_POC<-c("ID","Sampled","mg/L")
POC<-POC[,keep_POC]
POC<-POC %>% rename('Date'='Sampled', 'POC_mgL'='mg/L')
POC<-POC[POC$ID %in% c("6","5a","7","9","5","6a","13","15","3"), ]


DC_strm<-read_csv('04_Output/TC_stream.csv')
DIC_strm<-filter(DC_strm, Species=='DIC')
DIC_strm<-rename(DIC_strm, "DIC"="Conc.")
DIC_strm<-DIC_strm[,c("ID","Date","DIC" )]


DOC_strm<-filter(DC_strm, Species=='DOC')
DOC_strm<-rename(DOC_strm, "DOC"="Conc.")

DC<-left_join(DOC_strm, DIC_strm, by=c("ID","Date"))

alkalinity<-read_csv("02_Clean_data/alkalinity.csv")
alkalinity<-alkalinity %>%mutate(Date=as.Date(Date))%>%
  group_by(ID, Date) %>% mutate(alk_avg=mean(DIC_mgL_int, na.rm = T))

DC$DIC <- ifelse(is.na(DC$DIC), alkalinity$alk_avg, DC$DIC)

totDC<-left_join(DC,POC, by=c("ID","Date"))


CO2_obs<-read_csv("02_Clean_data/CO2_cleaned.csv")
CO2_obs<-CO2_obs %>%mutate(Date=as.Date(Date)) %>% group_by(ID, Date) %>% mutate(CO2_daily=mean(CO2, na.rm=T))

resp<-read_csv('04_Output/master_metabolism.csv')
depth<-read_csv('02_Clean_data/depth.csv')#multiply K600 1/d and depth

depth<-depth %>% mutate(Date=as.Date(Date)) %>% group_by(ID, Date) %>% mutate(depth=mean(depth, na.rm=T))
depth <- depth[!duplicated(depth[c('ID','Date')]),]

resp<-left_join(resp,depth, by=c('Date','ID'))
# resp<-resp %>% mutate(ER=abs(ER)) %>% mutate(O_gL=abs(ER*depth*K600_daily_mean)) %>%
#   mutate(CO2resp_mmolL=O_mgL/16)

discharge<-read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(Date=as.Date(Date))

CO2<-left_join(CO2_obs,resp, by=c('Date','ID'))
CO2<-left_join(CO2,discharge, by=c('Date','ID'))
CO2 <- CO2[!duplicated(CO2[c('ID','Date')]),]

CO2<-CO2mmol(CO2)

CO2<-CO2 %>% mutate(CO2chimney=CO2obs_mmol-CO2resp_mmolL)

CO2$CO2chimney[CO2$CO2chimney<0]<-0
CO2<-CO2 %>% filter(CO2chimney<2, CO2resp_mmolL<2)


ggplot(CO2, aes(Q))+
  geom_point(aes(y=CO2chimney, color = "chimney")) +geom_point(aes(y=CO2resp_mmolL, color="pathway"))+
  facet_wrap(~ ID, ncol=3)

write_csv(CO2, "04_Output/chimney_reactor.csv")
#######
CO2<-CO2[,c('Date','ID','CO2obs_mmol')]

totC<-left_join(totDC,CO2, by=c("ID",'Date'))
totC <- totC[!duplicated(totC[c('ID','Date')]),]

totC<-totC[,c('Date','ID','DOC','Q','Qbase','Qsurficial','Q_daily','depth_daily',
              'DIC','POC_mgL','CO2obs_mmol')]
totC<-totC %>% mutate(DOC_molL=DOC/12.010, DIC_molL=DIC/12.010, POC_molL=POC_mgL/12.010) %>%
  mutate(totC_molL=DOC_molL+DIC_molL+POC_molL) %>% mutate(DIC_molL_tot=DIC_molL+CO2obs_mmol)

ggplot(totC, aes(Q))+
  geom_point(aes(y=DIC_molL, color= "DIC_molL")) +geom_point(aes(y=DOC_molL, color='DOC_molL')) +
  geom_point(aes(y=POC_molL, color='POC_molL')) +facet_wrap(~ ID, ncol=3)

write_csv(totC, "02_Clean_data/allC_stream.csv")

#CO2 quality check#####
CO2 <- function(master) {
  master <- master[complete.cases(master[ , c('pH','Water_press','Q')]), ]
  master$Temp<-fahrenheit.to.celsius(master$Temp_pH)
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

  master<-master[,c('Date', 'ID', 'Site','CO2_daily', 'CO2_ppm_inter')]
  return(master)}

pH<-read_csv('02_Clean_data/pH_cleaned.csv')
pH<-pH %>% mutate(Date=as.Date(Date))%>%group_by(ID, Date) %>% mutate(pH_avg=mean(pH, na.rm=T))
totC<-left_join(totC,pH, by=c('ID','Date'))
totC <- totC[!duplicated(totC[c('ID','Date')]),]

quality_c<-CO2(totC)
