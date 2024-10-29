library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,7,4,6)]
data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
master<-join_all(data, by=c('Date','ID'), type='left')
detach("package:plyr", unload = TRUE)

temp<-master %>% mutate(Temp=fahrenheit.to.celsius(Temp_DO)) %>%mutate(Temp_K=Temp+273.15)

KH<-temp %>% mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%mutate(KH=0.034*2.178^(exp))

mols<-KH %>%
  mutate(CO2_atm=CO2/10^6) %>% mutate(CO2_molL=CO2_atm*KH, DO_molL=DO/32, Q=Q/35.3147)%>%
  filter(ID %in% c('5','5a','6a','7'))

debby<-mols%>% filter(Date>'2024-07-28' & Date<'2024-09-01')%>%mutate(hurricane='Debby')
helene<-mols%>% filter(Date>'2024-09-23' & Date<'2024-10-04')%>%mutate(hurricane='Helene')
idalia<-mols%>% filter(Date>'2023-09-20' & Date<'2023-10-09')%>%mutate(hurricane='Idalia')
hurricanes<-rbind(debby, helene, idalia)


a<-ggplot(hurricanes, aes(Q, color=hurricane))+
  geom_point(aes(y=DO),size=2, shape=1)+
  geom_path(aes(y=DO),alpha=0.5)+
  ggtitle("DO mg/L")+scale_x_log10()+
  xlab(expression('Discharge'~m^3))+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "none")

b<-ggplot(hurricanes, aes(Q, color=hurricane))+
  geom_point(aes(y=CO2_molL),size=2, shape=1)+
  geom_path(aes(y=CO2_molL),alpha=0.5)+
  ggtitle("CO2 mol/L")+scale_x_log10()+
  xlab(expression('Discharge'~m^3))+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "bottom")

plot_grid(a,b, ncol=1)



master<-mols %>% mutate(hurricanes= case_when(
  Date>'2024-07-28' & Date<'2024-08-13'~'Debby',
  Date>'2024-09-19' & Date<'2024-10-03'~"Helene",
  Date>'2023-09-20' & Date<'2023-10-01'~"Idalia"), day=as.Date(Date))

metabolism <- read_csv("04_Output/master_metabolism.csv")
k600<-metabolism%>%select(Date, ID, K600_daily_mean) %>% rename(k600_1.d=K600_daily_mean, day=Date)
master.k600<-left_join(master, k600, by=c('day','ID'))

ks<-master.k600 %>%
  mutate(K600_m.d=k600_1.d*depth,
         SchmidtCO2hi=1742-91.24*Temp+2.208*Temp^2-0.0219*Temp^3,
         SchmidtO2hi=1568-86.04*Temp+2.142*Temp^2-0.0216*Temp^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)))#%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)

flux<-ks%>%
  mutate(CO2_flux=KCO2_m.d*(CO2-400)*KH*(1/10^6)*44*1000,
         O2_flux=((DO-300)/1000)*KO2_m.d,
         Q=Q*0.0283168)%>%filter(Date>'2023-09-10')

only_hurricanes<-flux %>% filter(hurricanes != is.na(NA))
ggplot(only_hurricanes, aes(x=CO2_flux,y=O2_flux, color=hurricanes))+
  geom_point(size=2, shape=1)+
  ggtitle("O2-CO2")+
  #xlab(expression('Discharge'~ft^3))+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "bottom")


a<-ggplot(flux, aes(x=Date,y=depth, color=hurricanes))+
  geom_line()+ylab("m")+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "none")
b<-ggplot(flux, aes(x=Date,y=Q, color=hurricanes))+
  geom_line()+ylab(expression(m^3/sec))+
  #xlab(expression('Discharge'~ft^3))+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "bottom")
plot_grid(a,b, ncol=1)


a<-ggplot(test, aes(x=Date,y=CO2, color=hurricanes))+
  geom_line()+ylab("CO2 ppm")+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "none")
b<-ggplot(test, aes(x=Date,y=DO, color=hurricanes))+
  geom_line()+ylab("DO mg/L")+
  #xlab(expression('Discharge'~ft^3))+
  facet_wrap(~ ID, ncol=4, scale='free')+
  theme(legend.position = "bottom")
plot_grid(a,b, ncol=1)

