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

theme_set(theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(size = 17, angle = 90),
                axis.title.x = element_text(size = 17),
                plot.title = element_text(size = 17),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 12),
                legend.title =element_blank(),
                legend.position ="bottom",
                panel.grid.major.x = element_line(color = "black"),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_line(color = "black", linetype = "dashed"),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))

CO2mol <- function(CO2) {
  CO2$Temp_C<-fahrenheit.to.celsius(CO2$Temp_PT)
  CO2$Temp_K<-CO2$Temp_C+273.15
  CO2$exp<-2400*((1/CO2$Temp_K)-(1/298.15))
  CO2$KH<-0.034*2.178^(CO2$exp)#mol/L/atm

  CO2$CO2_atm<-CO2$CO2/1000000
  CO2$CO2obs_mol<-CO2$CO2_atm*CO2$KH
  return(CO2)}

#Edit dims######

depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')
length<-read_csv('02_Clean_data/stream area.csv')

depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth, Temp_PT)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(Q, depth, by=c('ID', 'Date'))

#Chimney Pathway#####

resp<-read_csv('04_Output/master_metabolism.csv')
resp<-resp %>% filter(ER< -3) %>% filter(GPP>0)%>%filter(ER>-20)%>%mutate(NEP=abs(GPP+ER))

resp<-left_join(resp,dim, by=c('Date','ID'))
KH<-resp %>%filter(depth>0)%>%  mutate(Temp_C=fahrenheit.to.celsius(Temp_PT)) %>%mutate(Temp_K=Temp_C+273.15)%>%mutate(
  KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))

KCO2<-KH %>%
  mutate(K600_m.d=K600_daily_mean*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)%>%

  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KCO2_d=KCO2_m.d/depth)%>%
  rename(day=Date) #%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)

CO2_hourly<-read_csv("02_Clean_data/CO2_cleaned.csv")
CO2<-CO2_hourly%>% mutate(day=as.Date(Date))

chimney<-left_join(CO2,KCO2, by=c('day','ID'))
chimney <- chimney  %>%
  mutate(CO2_flux=KCO2_m.d*(CO2-400)*KH*(1/10^6)*44*1000)%>%
  group_by(day,ID)%>%mutate(mean_CO2flux=mean(CO2_flux, na.rm = T))
chimney <- chimney[complete.cases(chimney[ , c('CO2_flux')]), ]

chimney$ID <- factor(chimney$ID , levels=c('5','5a','15','7','3','6','6a','9','13'))

ggplot(chimney, aes(Q))+
  geom_point(aes(y=NEP, color = "NEP")) +ylab(expression('g'/m^2/'day'))+
  geom_point(aes(y=mean_CO2flux, color="total CO2"))+scale_x_log10()+
  facet_wrap(~ ID, ncol=3, scale='free')+theme(legend.position = "bottom")+
  xlab(expression(Discharge~m^3/sec))+
  ggtitle('Reactor-Chimney Carbon')


ggplot(chimney, aes(Q))+
  geom_point(aes(y=NEP),size=2)+ggtitle("NEP")+ylab(expression(O[2]~'g'/m^2/'day'))+xlab(expression('Discharge'~m^3))+
  facet_wrap(~ ID, ncol=3, scale='free')+theme(legend.position = "bottom")


write_csv(chimney, "04_Output/chimney_reactor.csv")

#CO2 quality check#####

CO2_hourly<-left_join(CO2_hourly, dim, by=c('Date', 'ID'))
CO2_hourly<-CO2_hourly %>% filter(CO2>700) %>%filter(ID !='14')
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

ggplot(CO2_hourly, aes(x=Q))+
  geom_point(aes(y=CO2), size=1.5, shape=1)+
  #scale_color_gradient(high='red', low='blue')+
  scale_x_log10()+#scale_y_log10()+
  xlab(expression('Discharge'~m^3/s))+ylab(expression(CO[2]~'ppm'))+
  facet_wrap(~ ID, ncol=3, scales='free')
