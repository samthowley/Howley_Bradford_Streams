library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(ggpmisc)
library('StreamMetabolism')
library(hydroTSM)


#results from the pathway analysis########
ext.int <- read_csv("04_Output/external-internal.csv")%>%
  select(Date, ID, CO2_flux, internal)%>%
  filter(ID %in% c('5', '6', '9'))


#latercal CO2#######

flow.regime<-read_csv("04_Output/flow_regime_daily.csv")%>% select(ID, Date, width, qL_m2.sec)

gas_samples <- read_csv("04_Output/gas.samples.csv")%>%
  filter(chapter=="RC", type=='CO2')%>%
  mutate(
    location=
      case_when(
        Site=="5GW8" ~ 'TER',
        Site=="6GW5"~'TER',
        Site=='9GW5'~'TER',
        .default = "RIP"
      )
  )%>%
  separate(Site, into = c("ID", "Well"), sep = "GW")%>%
  select(Date, ID, Well, water_umol.L, location)%>%
  mutate(CO2.g.L= (((water_umol.L/10^6)*44)))

lateral.mass.co2.flux<-left_join(gas_samples, flow.regime)%>%
  mutate(
    lateral.co2.flux=qL_m2.sec*86400*CO2.g.L*10^3*(1/width) )%>%
  filter(!is.na(lateral.co2.flux), lateral.co2.flux>0)%>%
  group_by(ID, Date, location)%>%
  mutate(
    lateral.co2.flux.mean=mean(lateral.co2.flux, na.rm=T)
  )%>%ungroup()%>%
  distinct(ID, Date, location, .keep_all = T)

TER<-lateral.mass.co2.flux%>%
  filter(location=='TER')%>%
  rename(TER=lateral.co2.flux.mean)%>%
  select(Date, ID, TER)


RIP<-lateral.mass.co2.flux%>%
  filter(location=='RIP')%>%
  rename(RIP=lateral.co2.flux.mean)%>%
  select(Date, ID, RIP)

gas.samples<-full_join(TER, RIP)
#combine#####
mass.balance.list <- list(gas.samples, ext.int)
mass.balance <- reduce(mass.balance.list, left_join, by=c('Date', 'ID'))%>%
  mutate(RC=RIP+internal)

write_csv(mass.balance, "mass.balance.csv")

ggplot(mass.balance, aes(x=Date))+
  geom_point(aes(y=CO2_flux, color='fCO2'))+
  geom_point(aes(y=internal, color='internal'))+
  geom_point(aes(y=RIP, color='RIP'),shape=1)+
  geom_point(aes(y=TER, color='TER'),shape=1)+
  ylab("g/m^2/day")+scale_y_log10()+
  ggtitle('Fluxes')+
  facet_wrap(~ID)

b<-ggplot(mass.balance, aes(x=Date, y=total.inputs/CO2_flux))+
  geom_point()+
  scale_y_log10()+ylab("g/m^2/day")+
  ggtitle("RC Proportion of fCO2")
plot_grid(a,b, ncol=1)


#check###########
names(lateral.mass.co2.flux)
test<-lateral.mass.co2.flux%>%mutate(lateral.co2.ppm=(water_umol.L/10^6)*0.0034
