#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(weathermetrics)

CO2mol <- function(CO2) {
  CO2$Temp_C<-fahrenheit.to.celsius(CO2$Temp_PT)
  CO2$Temp_K<-CO2$Temp_C+273.15
  CO2$exp<-2400*((1/CO2$Temp_K)-(1/298.15))
  CO2$KH<-0.034*2.178^(CO2$exp)#mol/L/atm

  CO2$CO2_atm<-CO2$CO2/1000000
  CO2$CO2obs_mol<-CO2$CO2_atm*CO2$KH
  return(CO2)}
resp<-read_csv('04_Output/master_metabolism.csv')
depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')#multiply K600 1/d and depth
dim<-read_csv('02_Clean_data/stream area.csv')

CO2_obs<-read_csv("02_Clean_data/CO2_cleaned.csv")
CO2_obs<-CO2_obs %>%mutate(Date=as.Date(Date)) %>% group_by(ID, Date) %>% mutate(CO2_daily=mean(CO2, na.rm=T))

#sample C##########
POC<-read_xlsx('01_Raw_data/POC.xlsx')
keep_POC<-c("ID","Sampled","mg/L")
POC<-POC[,keep_POC]
POC<-POC %>% rename('Date'='Sampled', 'POC_mgL'='mg/L')
POC<-POC[POC$ID %in% c("6","5a","7","9","5","6a","13","15","3"), ]

DC_strm<-read_csv('04_Output/TDC_stream.csv')
DIC_strm<-DC_strm %>% filter(Species=='DIC') %>% rename("DIC"="Conc.") %>% select(ID, Date, DIC, depth, pH, Q)
DOC_strm<-DC_strm %>% filter(Species=='DOC') %>% rename("DOC"="Conc.") %>% select(ID, Date, DOC)
DC<-left_join(DOC_strm, DIC_strm, by=c("ID","Date"))

alkalinity<-read_csv("02_Clean_data/alkalinity.csv")
alkalinity<-alkalinity %>%mutate(Date=as.Date(Date))%>%
  group_by(ID, Date) %>% mutate(alk_avg=mean(DIC_mgL_int, na.rm = T))%>%
  select(Date, ID, alk_avg)
DC<-left_join(DC, alkalinity, by=c('Date', 'ID'))
DC <- DC[complete.cases(DC[ , c('DOC')]), ]
DC <- DC[!duplicated(DC[c('ID','Date')]),]

DC <- DC %>%group_by(Date, ID) %>%
  mutate(DIC = ifelse(is.na(DIC), alk_avg, DIC))

totDC<-left_join(DC,POC, by=c("ID","Date"))
totDC<-totDC %>% select(ID,Date,POC_mgL,DIC,DOC, depth, pH, Q)
totDC <- totDC[rev(order(as.Date(totDC$Date, format="%m/%d/%Y"))),]
totDC$POC_mgL[totDC$POC_mgL>60]<-NA

ggplot(totDC, aes(Q))+
  geom_point(aes(y=DIC, color= "DIC")) +
  geom_point(aes(y=DOC, color='DOC')) +facet_wrap(~ ID, ncol=3)

ggplot(totDC, aes(Q,y=POC_mgL, color=ID))+
  geom_point() #+facet_wrap(~ ID, ncol=3)


write_csv(totDC, "04_Output/stream_sampledC.csv")
###################
#####sensor C#####
##################
#Reactor Pathway##########

Q<-Q %>% mutate(Date=as.Date(Date)) %>% group_by(ID, Date) %>% mutate(Q=mean(Q, na.rm=T))

resp<-left_join(resp,Q, by=c('Date','ID'))
resp<-left_join(resp,dim, by=c('ID'))
resp <- resp[!duplicated(resp[c('ID','Date')]),]

resp<-resp %>% mutate(ER=abs(ER)) %>% mutate(O_mgL=abs(ER*length*width)/(Q*60*60))%>%mutate(CO2resp_molL=O_mgL/16000)

#Chimney Pathway#####

depth<-depth %>% mutate(Date=as.Date(Date)) %>% group_by(ID, Date) %>% mutate(Q=mean(Q, na.rm=T)) %>%
  select(Date, ID, Temp_PT, depth)

CO2<-left_join(CO2_obs,resp, by=c('Date','ID'))
CO2<-left_join(CO2,depth, by=c('Date','ID'))
CO2 <- CO2[!duplicated(CO2[c('ID','Date')]),]
CO2<-na.omit(CO2)

CO2<-CO2mol(CO2)

CO2<-CO2 %>% mutate(CO2chimney_mmol= (CO2obs_mol-CO2resp_molL)*1000, CO2reactor_mmol=CO2resp_molL*1000) %>%
  select(Date, ID, CO2chimney_mmol, CO2reactor_mmol, depth, Qsurficial, Qbase, Q)

ggplot(CO2, aes(Q))+
  geom_point(aes(y=CO2chimney_mmol, color = "chimney")) +geom_point(aes(y=CO2reactor_mmol, color="pathway"))+
  facet_wrap(~ ID, ncol=3, scale='free')+theme(legend.position = "bottom")

write_csv(CO2, "04_Output/chimney_reactor.csv")

##########################################
####Combining Sensor C and sampled C######
##################################################

CO2_obs<-CO2_obs[,c('Date','ID','CO2_daily')]
CO2_obs<-left_join(CO2_obs, depth, c('Date','ID'))
CO2_obs<-left_join(CO2_obs, Q, c('Date','ID'))
CO2_obs <- CO2_obs[!duplicated(CO2_obs[c('ID','Date')]),]

CO2_obs<- CO2_obs %>% rename('CO2'='CO2_daily') %>%CO2mol() %>%select(Date,ID,CO2obs_mol)

totC<-left_join(CO2_obs, totDC, by=c("ID",'Date'))
totC<-totC %>%fill(CO2obs_mol, .direction = "up")

totC<-totC %>% mutate(CO2_mgL=CO2obs_mol*44010) %>%
  mutate(DIC_total_mgL=CO2_mgL+DIC) %>% rename('DOC_mgL'='DOC')%>%
  filter(DIC<500) %>% filter(DOC_mgL<500)
totC <- totC[complete.cases(totC[ , c('DOC_mgL')]), ]
totC <- totC[rev(order(as.Date(totC$Date, format="%m/%d/%Y"))),]

ggplot(totC, aes(Q))+
  geom_point(aes(y=DIC_total_mgL, color= "DIC")) +
  geom_point(aes(y=DOC_mgL, color='DOC')) +
  geom_point(aes(y=POC_mgL, color='POC')) +facet_wrap(~ ID, ncol=3)
range(totC$Date)

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
