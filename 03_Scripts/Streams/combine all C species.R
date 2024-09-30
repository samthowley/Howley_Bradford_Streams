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
Q<-read_csv('02_Clean_data/discharge.csv')
length<-read_csv('02_Clean_data/stream area.csv')

CO2_obs<-read_csv("02_Clean_data/CO2_cleaned.csv")
CO2_obs<-CO2_obs %>%mutate(Date=as.Date(Date)) %>% group_by(ID, Date) %>% mutate(CO2_daily=mean(CO2, na.rm=T))

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

# alkalinity<-read_csv("02_Clean_data/alkalinity.csv")
# alkalinity<-alkalinity %>%mutate(Date=as.Date(Date))%>%
#   group_by(ID, Date) %>% mutate(alk_avg=mean(DIC_mgL_int, na.rm = T))%>% #daily alkalinity average
#   select(Date, ID, alk_avg)

#totDC<-left_join(totDC, alkalinity, by=c('Date', 'ID'))
#totDC <- totDC[complete.cases(totDC[ , c('DOC')]), ]
totDC <- totDC[!duplicated(totDC[c('ID','Date')]),]

#replaced measured DIC NAs with interpolated alkalinity values
#totDC <- totDC %>%group_by(Date, ID) %>%mutate(DIC = ifelse(is.na(DIC), alk_avg, DIC))%>% filter(DOC> 0)

totDC<-left_join(totDC, dim, by=c('ID', 'Date'))

ggplot(totDC, aes(Q))+
  geom_point(aes(y=DIC, color= "DIC")) +
  geom_point(aes(y=DOC, color='DOC')) +
  scale_x_log10()+scale_y_log10()+
  facet_wrap(~ ID, ncol=3, scales='free')

site<-totDC %>% filter(ID=='6')
ggtern(data=totDC,aes(DOC,DIC,POC_mgL, colour = Q))+scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC_mgL",y="DIC_mgL",z="POC_mgL")+facet_wrap(~ ID, ncol=3, scales='free')


write_csv(totDC, "04_Output/stream_sampledC.csv")
#Reactor Pathway##########

resp<-left_join(resp,dim, by=c('Date','ID'))
resp <- resp[!duplicated(resp[c('ID','Date')]),]
resp<-left_join(resp,length, by=c('ID'))

resp<-resp %>% mutate(ER=abs(ER)) %>%
  mutate(O2_mgL=abs(ER*length*width)/(Q*60*60))%>%mutate(CO2resp_molL=O2_mgL/32000)# not right...

#Chimney Pathway#####

CO2<-left_join(CO2_obs,resp, by=c('Date','ID'))
CO2 <- CO2[!duplicated(CO2[c('ID','Date')]),]
CO2<-na.omit(CO2)

CO2<-CO2mol(CO2)

CO2<-CO2 %>% mutate(CO2chimney_mmol= (CO2obs_mol-CO2resp_molL)*1000, CO2reactor_mmol=CO2resp_molL*1000) %>%
  select(Date, ID, CO2chimney_mmol, CO2reactor_mmol, depth, Qsurficial, Qbase, Q, KH)

ggplot(CO2, aes(Q))+
  geom_point(aes(y=CO2chimney_mmol, color = "chimney")) +geom_point(aes(y=CO2reactor_mmol, color="pathway"))+
  facet_wrap(~ ID, ncol=3, scale='free')+theme(legend.position = "bottom")

write_csv(CO2, "04_Output/chimney_reactor.csv")

####Combining Sensor C and sampled C######

CO2_obs<-CO2_obs[,c('Date','ID','CO2_daily')]
CO2_obs<-left_join(CO2_obs, dim, c('Date','ID'))
CO2_obs <- CO2_obs[!duplicated(CO2_obs[c('ID','Date')]),]

CO2_obs<- CO2_obs %>% rename('CO2'='CO2_daily') %>%CO2mol() %>%select(Date,ID,CO2obs_mol,KH)

totC<-left_join(CO2_obs, totDC, by=c("ID",'Date'))
totC<-totC %>%fill(CO2obs_mol, .direction = "up")

totC<-totC %>% mutate(CO2_mgL=CO2obs_mol*44010) %>% filter(DIC<500) %>% filter(DOC<500)
totC <- totC[complete.cases(totC[ , c('DOC')]), ]
totC <- totC[rev(order(as.Date(totC$Date, format="%m/%d/%Y"))),]
unique(totC$ID)

ggplot(totC, aes(Q))+
  #geom_point(aes(y=DIC, color= "DIC")) +
  geom_point(aes(y=DOC, color='DOC')) +
  #geom_point(aes(y=POC_mgL, color='POC')) +
  facet_wrap(~ ID, ncol=3)
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
