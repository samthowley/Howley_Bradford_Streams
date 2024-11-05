library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,6,7,4)]
data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
master<-join_all(data, by=c('Date','ID'), type='left')
detach("package:plyr", unload = TRUE)
master<-master %>% mutate(day=as.Date(Date))

metabolism<-read_csv('04_Output/master_metabolism.csv')
metabolism<-metabolism %>% select(K600_daily_mean, Date, ID) %>% rename(k600_1.d=K600_daily_mean, day="Date" )

master.k600<-left_join(master, metabolism, c('day', 'ID'))

temp<-master.k600 %>% mutate(Temp=fahrenheit.to.celsius(Temp_DO)) %>%mutate(Temp_K=Temp+273.15)

KH<-temp %>% mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%mutate(KH=0.034*2.178^(exp))

mols<-KH %>%
  mutate(CO2_atm=CO2/10^6) %>% mutate(CO2_molL=CO2_atm*KH, DO_molL=DO/32, Q=Q*0.0283168)

ks<-mols %>%
  mutate(K600_m.d=k600_1.d*depth,
         SchmidtCO2hi=1742-91.24*Temp+2.208*Temp^2-0.0219*Temp^3,
         SchmidtO2hi=1568-86.04*Temp+2.142*Temp^2-0.0216*Temp^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)))#%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)

flux<-ks%>%
  mutate(CO2_flux=KCO2_m.d*(CO2-400)*KH*(1/10^6)*1000,
         O2_flux=((DO-300)/1000)*KO2_m.d,
         CO2_mol=(CO2-400)*(1/10^6)*KH,
         DO_mol=DO/32000)%>% filter(ID != '14')%>%
  mutate(o2co2=O2_flux/CO2_flux)


ggplot(flux, aes(x=CO2_flux,y=O2_flux, color=Q))+
  geom_point(shape=1)+ylab(expression(mol/m^2/day))+
  scale_color_gradient(high='red', low='blue')+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")



flux$ID <- factor(flux$ID , levels=c('5','5a','15','7','3','6','6a','9','13'))
select<-flux %>% filter(ID %in% c(5,6,3)) %>% filter(DO>0)
a<-ggplot(select, aes(x=Date, y=DO)) +
  geom_point(fill='#A4A4A4', color="black")+
  ylab('Dissolved Oxygen (mg/L)')+ facet_wrap(~ ID, ncol=3, scale='free')

b<-ggplot(select, aes(x=Date, y=CO2)) +
  geom_point(fill='#A4A4A4', color="black")+
  ylab(expression(CO[2]~ppm))+ facet_wrap(~ ID, ncol=3, scale='free')
plot_grid(a,b, ncol=1)


  hurricanes<-flux %>% mutate(hurricanes= case_when(
    Date>'2024-07-28' & Date<'2024-08-13'~'Debby',
    Date>'2024-09-19' & Date<'2024-10-03'~"Helene",
    Date>'2023-09-20' & Date<'2023-10-01'~"Idalia"), day=as.Date(Date))


  ggplot(hurricanes, aes(x=CO2_flux,y=O2_flux, color=hurricanes))+
    geom_point(size=2, shape=1)+
    ggtitle("O2-CO2")+
    #xlab(expression('Discharge'~ft^3))+
    facet_wrap(~ ID, ncol=5, scale='free')+
    theme(legend.position = "bottom")
