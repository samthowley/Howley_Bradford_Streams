
library(tidyverse)
library(writexl)
library(readxl)

RC_flow_regime <- read_csv("04_Output/RC/RC.flow.regime.csv")

gas <- read_csv("04_Output/gas.samples.csv")

RC.CO2 <- subset(gas, type == "CO2")%>%rename(CO2.water_umol.L=water_umol.L, CO2.water.ppm=water.ppm)%>%
  filter(chapter=='RC')%>%select(-type, -chapter)

# CH4 <- subset(gas, type == "CH4")%>%rename(CH4.water_umol.L=water_umol.L, CH4.water.ppm=water.ppm)%>%
#   filter(chapter=='RC')%>%select(-type, -chapter)
#
# N2O <- subset(gas, type == "N2O")%>%rename(N2O.water_umol.L=water_umol.L, N2O.water.ppm=water.ppm)%>%
#   filter(chapter=='RC')%>%select(-type, -chapter)
#
#C.gas<-full_join(CO2, CH4)

lateral.flux<-
  left_join(RC.CO2, RC_flow_regime, )%>%
  mutate(
    CO2_molL=CO2.water_umol.L/10^6)%>%
  mutate(
    CO2_flux=qL_m2.sec*86400*CO2_molL*44*10^3*(1/stream.w))%>%
  arrange(Site, Date)%>%
  distinct(Site, Date, .keep_all = T)

# split_list <- lateral.flux %>%
#   group_by(Site) %>%
#   group_split()
#
# names(split_list) <- lateral.flux %>%
#   group_by(Site) %>%
#   group_keys() %>%
#   pull(Site)

write_csv(lateral.flux, "04_Output/RC/RC.gas.sample.flux.csv")

