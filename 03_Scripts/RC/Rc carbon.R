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

RC_log <- read_excel("01_Raw_data/RC log.xlsx", sheet = "RC log")
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

WTdepth<-RC_log%>%
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
  mutate(WT_elevations=surface_elevation_m-surface2WT)

well_types <- data.frame(
  Site = c('5GW0', '5GW1', '5GW2', '5GW3', '5GW4', '5GW5', '5GW6', '5GW7', '5GW8',
         '6GW0', '6GW1', '6GW2', '6GW3', '6GW4', '6GW5',
         '9GW0', '9GW1','9GW2', '9GW3', '9GW4', '9GW5'),
  well_types = c('stream', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', "upland",
            'stream', 'RW', 'RW', 'RW', 'RW', "upland",
            'stream', 'RW', 'RW', 'RW', 'RW', "upland"))
RC_dims<-left_join(RC_dims, well_types)

#DOC DIC#########

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')%>%distinct(Site, Date, .keep_all = T)

C_RC<-full_join(DC_RC, RC_dims, by=c("Site", "Date"))

C_RC<-C_RC %>% mutate(ID=as.character(ID)) %>%
  separate(Site, into = c("Stream", "Well"), sep = "GW")%>%
  mutate(DOC_mg.m3=DOC/10^3,DIC_mg.m3=DIC/10^3)
#Include gas sampling#####

gas <- read_csv("04_Output/gas.samples.csv")
CO2 <- subset(gas, type == "CO2")%>%rename(CO2.water_umol.L=water_umol.L, CO2.water.ppm=water.ppm)%>%
  filter(chapter=='RC')%>%select(-type, -chapter)
CH4 <- subset(gas, type == "CH4")%>%rename(CH4.water_umol.L=water_umol.L, CH4.water.ppm=water.ppm)%>%
  filter(chapter=='RC')%>%select(-type, -chapter)
N2O <- subset(gas, type == "N2O")%>%rename(N2O.water_umol.L=water_umol.L, N2O.water.ppm=water.ppm)%>%
  filter(chapter=='RC')%>%select(-type, -chapter)

C.gas<-full_join(CO2, CH4)

C.gas.rename<-C.gas%>%
  separate(Site, into = c("Stream", "Well"), sep = "GW")%>%
  arrange(Date,Stream,Well)

# sensor.co2<-RC_log %>%select(Date, Site, CO2_mv)%>%
#   separate(Site, into = c("Stream", "Well"), sep = "GW")%>%
#   filter(!is.na(CO2_mv))%>%mutate(CO2_mv=as.numeric(CO2_mv))
#
# all_gas<-full_join(C.gas.rename, sensor.co2)
# write_csv(all_gas, "check.csv")
#
# interp.co2<-all_gas %>% mutate(CO2_interp=(CO2_mv*0.1921)-1.6408)%>%
#   mutate(CO2_umol_L=if_else(is.na(CO2_umol_L), CO2_interp, CO2_umol_L))%>%
#   select(-CO2_mv, -CO2_interp)

# RC_all<-full_join(C_RC, interp.co2, by=c('Stream', 'Well', 'Date'))%>%
#   mutate(
#     CO2_molL=CO2_umol_L/10^6,
#     CH4_molL=CH4_umol_L/10^6)%>%
#   distinct(Stream, Well, Date, .keep_all = T)

RC.all<-full_join(C.gas.rename, C_RC)

#interpolate lateral Fluxes####
RC<-left_join(RC.all, qL)

lateral_flux<-RC%>%
  distinct(Stream, Well, Date, CO2.water_umol.L, .keep_all = T)%>%
  mutate(
    CO2_molL=CO2.water_umol.L/10^6,
    CH4_molL=CH4.water_umol.L/10^6)%>%
  mutate(
    CO2_flux=(CO2_molL/(10^3))*12*86400*(qL/width),
    CH4_flux=(CH4_molL/(10^3))*12*86400*(qL/width))%>%
  mutate(
    DOC_flux=((qL/width)*DOC_mg.m3)*86400,
    DIC_flux=((qL/width)*DIC_mg.m3)*86400)%>%
  filter(
    !is.na(Well), !is.na(ID))%>%
  select( -bf, -u, -depth)%>%
  mutate(ID.Well = paste(ID, Well, sep = "."))

RC_edit<-lateral_flux %>% select(Well, Date, CO2.water_umol.L, CO2.water.ppm, CH4.water_umol.L,CH4.water.ppm,
                                 DIC, DOC, Distance_m, WT_elevations, DOC_mg.m3, DIC_mg.m3, qL, CO2_molL, CH4_molL,
                                 CO2_flux, CH4_flux, DOC_flux, DIC_flux, ID.Well)

split_list <- RC_edit %>%
  group_by(ID.Well) %>%
  group_split()

#write_xlsx(split_list, path = "04_Output/RC_by_well.xlsx")

#include streams#####

streamC<-read_csv('04_Output/stream_sampledC.csv')

streamC<-left_join(streamC, qL)
RC_columns<-names(RC_edit)

streamC_edited<-streamC%>%
  filter(ID %in% c("5","6","9"))%>%
  mutate(
    CO2_molL=CO2.water_umol.L/10^6,
    CH4_molL=CH4.water_umol.L/10^6)%>%
  mutate(
    CO2_flux=CO2_molL*(10^3)*12*86400*(Q/A),
    CH4_flux=CH4_molL*(10^3)*12*86400*(Q/A))%>%
  mutate(DOC_mg.m3=DOC/10^3,
           DIC_mg.m3=DIC/10^3,
    DOC_flux=DOC_mg.m3*((Q/10^3)/A)*86400,
    DIC_flux=DIC_mg.m3*((Q/10^3)/A)*86400)%>%
  mutate(
    `Distance (ft)`= -0.5,
    `Distance_m`= -0.5,
     WTdepth_m=0,
     Well=0,
     DistanceID='stream',
     surface2WT=0,
     surface_elevation_m=0,
     WT.ID=1,
     WT_elevations=depth,
     well_types='stream',
    ID.Well = paste(ID, Well, sep = ".")
    )%>%filter(!is.na(DOC))%>%
  select(all_of(RC_columns))

stream_RC<-rbind(streamC_edited,RC_edit)%>%
  separate(ID.Well, into = c("ID", "Well"), sep = "\\.", extra = "merge")

write_csv(stream_RC, "04_Output/allRC_C.csv")

#regressions#####

library(openxlsx)
file_path <- "04_Output/RC_by_well.xlsx"
sheet_names <- excel_sheets(file_path)
RC_df <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
}) %>%
  bind_rows()

cols <- c('DOC_flux','DIC_flux','CO2_flux','CH4_flux','qL','WT_elevations','ID.Well')
unique_sites <- unique(RC_df$ID.Well[!is.na(RC_df$ID.Well)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- RC_df %>%
      filter(ID.Well == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

col_names<- c("ID", "pvalue", "slope", "r2", "type")


DOC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DOC_flux", "WT_elevations")])) > 1
  DOC.elevation.p <- DOC.elevation.slope <- DOC.elevation.r2 <- NA

  if (valid_elev) {
    DOC.elevation <- lm(DOC_flux ~ WT_elevations, data = df)
    DOC.elevation.cf <- summary(DOC.elevation)
    DOC.elevation.p <- DOC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DOC.elevation.slope <- DOC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DOC.elevation.r2 <- DOC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DOC.elevation.p = as.numeric(DOC.elevation.p),
    DOC.elevation.slope = as.numeric(DOC.elevation.slope),
    DOC.elevation.r2 = as.numeric(DOC.elevation.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")%>%mutate(type='DOC')
colnames(DOC_table)<-col_names

DIC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DIC_flux", "WT_elevations")])) > 1

  # Initialize with NA
  DIC.elevation.p <- DIC.elevation.slope <- DIC.elevation.r2 <- NA
  DIC.qL.p <- DIC.qL.slope <- DIC.qL.r2 <- NA

  if (valid_elev) {
    DIC.elevation <- lm(DIC_flux ~ WT_elevations, data = df)
    DIC.elevation.cf <- summary(DIC.elevation)
    DIC.elevation.p <- DIC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DIC.elevation.slope <- DIC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DIC.elevation.r2 <- DIC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DIC.elevation.p = as.numeric(DIC.elevation.p),
    DIC.elevation.slope = as.numeric(DIC.elevation.slope),
    DIC.elevation.r2 = as.numeric(DIC.elevation.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")%>%mutate(type='DIC')
colnames(DIC_table)<-col_names

CO2_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("CO2_flux", "WT_elevations")])) > 1

  # Initialize with NA
  CO2.elevation.p <- CO2.elevation.slope <- CO2.elevation.r2 <- NA
  CO2.qL.p <- CO2.qL.slope <- CO2.qL.r2 <- NA

  if (valid_elev) {
    CO2.elevation <- lm(CO2_flux ~ WT_elevations, data = df)
    CO2.elevation.cf <- summary(CO2.elevation)
    CO2.elevation.p <- CO2.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    CO2.elevation.slope <- CO2.elevation.cf$coefficients["WT_elevations", "Estimate"]
    CO2.elevation.r2 <- CO2.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    CO2.elevation.p = as.numeric(CO2.elevation.p),
    CO2.elevation.slope = as.numeric(CO2.elevation.slope),
    CO2.elevation.r2 = as.numeric(CO2.elevation.r2)
  )
})
CO2_table <- bind_rows(CO2_relationships, .id = "ID")%>%mutate(type='CO2')
colnames(CO2_table)<-col_names

CH4_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("CH4_flux", "WT_elevations")])) > 1

  # Initialize with NA
  CH4.elevation.p <- CH4.elevation.slope <- CH4.elevation.r2 <- NA
  CH4.qL.p <- CH4.qL.slope <- CH4.qL.r2 <- NA

  if (valid_elev) {
    CH4.elevation <- lm(CH4_flux ~ WT_elevations, data = df)
    CH4.elevation.cf <- summary(CH4.elevation)
    CH4.elevation.p <- CH4.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    CH4.elevation.slope <- CH4.elevation.cf$coefficients["WT_elevations", "Estimate"]
    CH4.elevation.r2 <- CH4.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    CH4.elevation.p = as.numeric(CH4.elevation.p),
    CH4.elevation.slope = as.numeric(CH4.elevation.slope),
    CH4.elevation.r2 = as.numeric(CH4.elevation.r2)
  )
})
CH4_table <- bind_rows(CH4_relationships, .id = "ID")%>%mutate(type='CH4')
colnames(CH4_table)<-col_names

relationships<-rbind(DOC_table, DIC_table, CO2_table, CH4_table)

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
