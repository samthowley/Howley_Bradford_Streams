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

mols<-KH %>%mutate(CO2_atm=CO2/10^6) %>% mutate(CO2_molL=CO2_atm*KH, DO_molL=DO/32)

debby<-mols%>% filter(Date>'2024-07-28' & Date<'2024-08-13')%>%mutate(hurricane='Debby')
helene<-mols%>% filter(Date>'2024-09-19' & Date<'2024-10-03')%>%mutate(hurricane='Helene')
idalia<-mols%>% filter(Date>'2023-09-20' & Date<'2023-10-01')%>%mutate(hurricane='Idalia')
hurricanes<-rbind(debby, helene, idalia)


ggplot(hurricanes, aes(Q, color=hurricane))+
  geom_point(aes(y=DO),size=2, shape=1)+
  ggtitle("DO mg/L")+
  xlab(expression('Discharge'~ft^3))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")

ggplot(hurricanes, aes(Q, color=hurricane))+
  geom_point(aes(y=CO2_molL),size=2, shape=1)+
  ggtitle("CO2 mol/L")+
  xlab(expression('Discharge'~ft^3))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")

master<-mols %>% mutate(hurricanes= case_when(
  Date>'2024-07-28' & Date<'2024-08-13'~'Debby',
  Date>'2024-09-19' & Date<'2024-10-03'~"Helene",
  Date>'2023-09-20' & Date<'2023-10-01'~"Idalia"))%>%
  filter(ID %in% c('5','6','6a','9','7'))

ggplot(master, aes(x=CO2_molL,y=DO_molL, color=hurricanes))+
  geom_point(size=2, shape=1)+
  ggtitle("O2-CO2")+
  #xlab(expression('Discharge'~ft^3))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")


