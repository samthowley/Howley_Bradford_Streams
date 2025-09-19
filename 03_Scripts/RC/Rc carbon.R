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

RC_flow_regime <- read_csv("04_Output/RC.flow.regime.csv")

#DOC DIC#########

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')%>%distinct(Site, Date, .keep_all = T)

C_RC<-full_join(DC_RC, RC_dims, by=c("Site", "Date"))

C_RC<-C_RC %>% mutate(ID=as.character(ID)) %>%
  separate(Site, into = c("Stream", "Well"), sep = "GW")%>%
  mutate(DOC_mg.m3=DOC/10^3,DIC_mg.m3=DIC/10^3)


#interpolate lateral Fluxes####
RC<-left_join(RC.all, flow_regime)


mol.L_conversion_flux <- function(file) {
 file<-file%>% mutate(
      DOC_flux=((qL_m2.sec/width)*DOC_mg.m3)*86400,
      DIC_flux=((qL_m2.sec/width)*DIC_mg.m3)*86400)

}


lateral_flux<-RC%>%
  distinct(Stream, Well, Date, CO2.water_umol.L, .keep_all = T)%>%
  filter(
    !is.na(Well), !is.na(ID))%>%
  select( -bf, -u, -depth)%>%
  mutate(ID.Well = paste(ID, Well, sep = "."))%>%
  mol.L_conversion_flux()

RC_edit<-lateral_flux %>% select(Well, Date, CO2.water_umol.L, CO2.water.ppm, CH4.water_umol.L,CH4.water.ppm,
                                 DIC, DOC, Distance_m, WT_elevations, DOC_mg.m3, DIC_mg.m3, qL_m2.sec, CO2_molL, CH4_molL,
                                 CO2_flux, CH4_flux, DOC_flux, DIC_flux, ID.Well)


stream_RC<-rbind(streamC_edited,RC_edit)%>%
  distinct(ID.Well, Date, .keep_all = T)

split_list <- stream_RC %>%
  group_by(ID.Well) %>%
  group_split()

names(split_list) <- stream_RC %>%
  group_by(ID.Well) %>%
  group_keys() %>%
  pull(ID.Well)

write_xlsx(split_list, path = "04_Output/RC_by_well.xlsx")


#Include Wetland Cover####
wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>%
  select(Basin_Name, PERCENTAGE) %>% rename(ID=Basin_Name, wetland_perc=PERCENTAGE)%>%
  mutate(wetland_perc=round(wetland_perc, 2))

relationships<-relationships%>%
  separate(ID, into = c("ID", "Well"), sep = "\\.", extra = "merge")

relationships<-left_join(relationships, wetland_cover)%>%
  mutate(ID_wetperc=paste0(ID, wetland_perc, sep="_"))%>%
  filter(!is.na(ID))

write_csv(relationships, "04_Output/RC_slopes.csv")
