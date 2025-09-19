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
  filter(ID %in% c('5', '6', '9'))%>%
  mutate(fCO2=-1*CO2_flux)%>%
  select(-CO2_flux)


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
mass.balance <- reduce(mass.balance.list, left_join, by=c('Date', 'ID'))
write_csv(mass.balance, "mass.balance.csv")



mb.long <- pivot_longer(mass.balance,
                        cols = c(RIP, fCO2, internal, TER),
                        names_to = "Category",
                        values_to = "Flux")


ggplot(mb.long, aes(x = Date, y = Flux, fill = Category)) +
  geom_bar(stat = "identity") +
  ylab(expression(CO[2]~g/m^2/day))+
  facet_wrap(~ID)

plot_grid(
  ggplot(mb.long, aes(x = ID, y = Flux, fill = Category)) +
    geom_bar(stat = "identity") +
    ylab(expression(CO[2]~g/m^2/day)),

  ggplot(mb.long, aes(x = Date, y = Flux, fill = Category)) +
            geom_bar(stat = "identity") +
            ylab(expression(CO[2]~g/m^2/day))+facet_wrap(~ID),

          ncol=2)

