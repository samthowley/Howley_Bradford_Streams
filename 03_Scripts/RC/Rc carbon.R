#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)
library(weathermetrics)
library(lme4)
library(ggpmisc)

theme_set(theme(axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 20),
                axis.title.y = element_text(size = 20, angle = 90),
                axis.title.x = element_text(size = 20),
                plot.title = element_text(size = 20),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


#qL######
flow_regime<- read_csv("04_Output/flow_regime_daily.csv")%>%
  filter(ID %in% c('5', '6', '9'))

uca <- data.frame(
  ID = c('5', '6', '9'),
  UCA = c(2e-4, 1e-4, 1e-4)) #wrong

qL <- flow_regime %>%
  left_join(uca, by = "ID") %>%
  mutate(qL = bf/1000 * UCA) %>%
  select(-UCA) %>% filter(!is.na(ID))
#Create RC carbon dataset#######
### set has RC_dims for slope figure
WTdepth<-read_xlsx('01_Raw_data/RC log.xlsx', sheet='RC log')%>%
  rename("surface2WT"="Wtdepth (m)") %>% select(Date, ID, Site, surface2WT)

RC_distance <- read_excel("01_Raw_data/RC log.xlsx",sheet = "Sheet1")%>%
  select(Site,Distance_m, DistanceID)

RC_elevations <- read_excel("01_Raw_data/RC log.xlsx",sheet = "elevations")%>%
  arrange(ID, surface_elevation_m) %>%
  group_by(ID) %>%
  mutate(WT.ID = row_number() - 1) %>%
  ungroup()%>%
  select(Site, surface_elevation_m,WT.ID)

RC_dims<-full_join(WTdepth, RC_distance, by=c('Site'))
RC_dims<-full_join(RC_dims, RC_elevations, by=c('Site'))%>%
  mutate(WT_elevations=surface_elevation_m+surface2WT)

well_types <- data.frame(
  Site = c('5GW0', '5GW1', '5GW2', '5GW3', '5GW4', '5GW5', '5GW6', '5GW7', '5GW8',
         '6GW0', '6GW1', '6GW2', '6GW3', '6GW4', '6GW5',
         '9GW0', '9GW1','9GW2', '9GW3', '9GW4', '9GW5'),
  well_types = c('stream', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', "upland",
            'stream', 'RW', 'RW', 'RW', 'RW', "upland",
            'stream', 'RW', 'RW', 'RW', 'RW', "upland"))
RC_dims<-left_join(RC_dims, well_types)

#Optional: DOC DIC#########

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')%>%distinct(Site, Date, .keep_all = T)

C_RC<-full_join(DC_RC, RC_dims, by=c("Site", "Date"))

C_RC<-C_RC %>% mutate(ID=as.character(ID)) %>%
  separate(Site, into = c("Stream", "Well"), sep = "GW")
#Optional: include gas sampling#####

Picarro_gas <- read_csv("04_Output/Picarro_gas.csv")
RC_gas<-Picarro_gas%>%filter(chapter=='RC')%>%
  separate(ID, into = c("Stream", "Well"), sep = "GW")%>%select(-chapter, -Temp_K)%>%
  arrange(Date,Stream,Well)

RC_all<-full_join(C_RC, RC_gas, by=c('Stream', 'Well', 'Date'))%>%
  distinct(Stream, Well, Date, .keep_all = T)

#Optional: include streams#####

streamC<-read_csv('04_Output/stream_sampledC.csv')

streamC_edited<-streamC%>%
  filter(ID %in% c("5","6","9"))%>%
  rename(Stream=ID, Temp=Temp_pH)%>%
  mutate(
    `Distance (ft)`= -0.5,
    `Distance_m`= -0.5,
     WTdepth_m=0,
     Well=0,
     DistanceID='stream',
     ID=Stream,
     surface2WT=0,
     surface_elevation_m=0,
     WT.ID=1,
     WT_elevations=depth,
     well_types='stream'
    )%>%
  mutate(ID_Well = paste(ID, Well, sep = "_"))%>%
  select(Date,Stream,Well,DIC,DOC,ID,surface2WT,Distance_m,DistanceID,surface_elevation_m,
         WT.ID, well_types,WT_elevations, CO2_umol_L,CH4_umol_L,N2O_umol_L,CO2_sat,
         CH4_sat,N2O_sat)
stream_RC<-rbind(streamC_edited,RC_all)%>%mutate(Stream=as.factor(Stream))

write_csv(stream_RC, "02_Clean_data/allC_RC.csv")

# interpolate lateral Fluxes####

RC<-read_csv("02_Clean_data/allC_RC.csv")%>%mutate(ID=as.character(ID))
RC<-left_join(RC, qL)


lateral_flux<-RC%>%
    mutate(
      CO2_molL=CO2_umol_L/10^6,
      CH4_molL=CH4_umol_L/10^6)%>%
  mutate(
    lateral_CO2=CO2_molL*(10^3)*12*86400*(qL/width),
    lateral_CH4=CH4_molL*(10^3)*12*86400*(qL/width))%>%
  mutate(
    DOC_flux=(qL/width)*DOC*86400,
    DIC_flux=qL/width*DIC*86400)%>%
  filter(
    !is.na(Well), !is.na(ID))%>%
  select(-width, -bf, -u, -depth)%>%
  mutate(ID.Well = paste(ID, Well, sep = "."))

