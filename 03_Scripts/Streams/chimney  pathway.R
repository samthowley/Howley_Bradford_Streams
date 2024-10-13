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
KCO2<-left_join(resp,dim, by=c('Date','ID'))
KCO2<-KCO2 %>% filter(depth>0)%>%
  mutate(reactor=abs(GPP+ER), Temp_C=fahrenheit.to.celsius(Temp_PT)) %>%mutate(Temp_K=Temp_C+273.15)%>%

  mutate(K600_m.h=K600_daily_mean*depth/24, SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
         KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))%>%

  mutate(KCO2_denominator=(600*SchmidtCO2hi)^(-2/3), KCO2_m.h=K600_m.h/KCO2_denominator) %>%

  mutate(KCO2_d=(KCO2_m.h/24)*depth)%>% rename(day=Date) #%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)

CO2_hourly<-read_csv("02_Clean_data/CO2_cleaned.csv")
CO2<-CO2_hourly%>% mutate(day=as.Date(Date))

chimney<-left_join(CO2,KCO2, by=c('day','ID'))
chimney <- chimney[complete.cases(chimney[ , c('CO2','reactor')]), ]
chimney <- chimney %>%mutate(CO2_flux=KCO2_d*(CO2-420)*KH*(1/10^6)*12*1000*depth)


ggplot(chimney, aes(Date))+
  geom_area(aes(y=reactor, color = "reactor"),alpha=0.5) +
  geom_area(aes(y=CO2_flux, color="CO2"),alpha=0.5)+
  facet_wrap(~ ID, ncol=3, scale='free')+theme(legend.position = "bottom")

write_csv(CO2, "04_Output/chimney_reactor.csv")

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
