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

theme_set(theme(axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 17, angle = 90),
                axis.title.x = element_text(size = 17),
                plot.title = element_text(size = 17),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_line(color = "black"),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_line(color = "black", linetype = "dashed"),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


#qL######
depth<-read_csv('02_Clean_data/depth.csv')%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID) %>%
  mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth)%>%
  filter(depth>0, ID %in% c('5','6','9'))%>%distinct(ID, Date, .keep_all = T)

Q_total<-read_csv('02_Clean_data/discharge.csv')%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T))%>%
  filter(Q>1, ID %in% c('5','6','9'))%>%distinct(ID, Date, .keep_all = T)

baseflow <- read_csv("04_Output/baseflow.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID) %>%
  mutate(baseflow=mean(baseflow, na.rm = T))%>%
  select(Date, ID, baseflow)%>%distinct(ID, Date, .keep_all = T)%>%filter(ID %in% c('5','6','9'))


dim<-full_join(depth, Q_total, baseflow, by=c('ID', 'Date'))
dim<-full_join(dim, baseflow, by=c('ID', 'Date'))

uca_values <- data.frame(
  ID = c('5', '6', '9'),
  UCA = c(0.0002, 0.0001, 0.0003))

qL <- dim %>%
  left_join(uca_values, by = "ID") %>%
  mutate(qL = baseflow * UCA) %>%
  select(-UCA) %>% filter(!is.na(ID))

# ggplot(data = qL,aes(x = Q)) +
#   geom_point(aes(y = qL))+
#   facet_wrap(~ID, scales='free')

#Create RC carbon dataset#######
RClog<-read_xlsx('01_Raw_data/RC log.xlsx')
RClog<- RClog%>%rename("surface2WT"="Wtdepth (m)") %>% select(Date, ID, Site, surface2WT, CO2_mv,pH, Temp) %>%rename('CO2'=CO2_mv)

RC_distance <- read_excel("01_Raw_data/RC log.xlsx",sheet = "Sheet1")%>%select(Site,`Distance (ft)`,Distance_m, DistanceID)

RC_elevations <- read_excel("01_Raw_data/RC log.xlsx",sheet = "elevations")%>%
  select(Site, surface_elevation_m)

RC_dims<-full_join(RClog, RC_distance, by=c('Site'))
RC_dims<-full_join(RC_dims, RC_elevations, by=c('Site'))%>%
  mutate(WT_elevations=surface_elevation_m+surface2WT)

#DOC DIC#########

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')%>%distinct(Site, Date, .keep_all = T)

C_RC<-full_join(DC_RC, RC_dims, by=c("Site", "Date"))

C_RC<-C_RC %>% mutate(ID=as.character(ID), CO2=as.numeric(CO2), pH=as.numeric(pH), ID_Well=Site) %>%
  separate(Site, into = c("Stream", "Well"), sep = "GW")
#include gas sampling#####

Picarro_gas <- read_csv("04_Output/Picarro_gas.csv")
RC_gas<-Picarro_gas%>%filter(chapter=='RC')%>%
  separate(ID, into = c("Stream", "Well"), sep = "GW")%>%select(-chapter, -Temp_K)%>%
  arrange(Date,Stream,Well)

RC_all<-full_join(C_RC, RC_gas, by=c('Stream', 'Well', 'Date'))%>%
  distinct(Stream, Well, Date, .keep_all = T)%>%
  mutate(CO2=CO2*0.217 - 93.866)%>%
  mutate(CO2 = if_else(CO2<0, NA, CO2))%>%arrange(Date,Stream,Well)

#include streams#####

streamC<-read_csv('04_Output/stream_sampledC.csv')
streamC_edited<-streamC %>%
  filter(ID %in% c("5","6","9"))%>%
  rename(Stream=ID, Temp=Temp_pH)%>%
  mutate(
    `Distance (ft)`= -0.5,
    `Distance_m`= -0.5,
         WTdepth_m=0,
         Well=0,
    DistanceID='0',
         ID=Stream,
    surface2WT=0,
         surface_elevation_m=0,
         WT_elevations=depth,
    ID_Well='5GW0')%>%
  select(Date,Stream,Well,DIC,DOC,ID,surface2WT,CO2,
         pH,Temp, `Distance (ft)`,Distance_m,DistanceID,surface_elevation_m,
         WT_elevations,ID_Well, CO2_umol_L,CH4_umol_L,N2O_umol_L,CO2_sat,
         CH4_sat,N2O_sat)
stream_RC<-rbind(streamC_edited,RC_all)%>%mutate(Stream=as.factor(Stream))

write_csv(stream_RC, "02_Clean_data/allC_RC.csv")

# interpolate lateral Fluxes####

RC_all<-read_csv("02_Clean_data/allC_RC.csv")%>%mutate(ID=as.character(ID))
RC_all<-left_join(RC_all, qL, by=c('Date', 'ID'))

w_values <- data.frame(
  ID = c('5', '6', '9'),
  width = c(5.7912, 2.6, 1.35))

lateral_flux<-left_join(RC_all, w_values, by='ID')%>%
    mutate(CO2_molL=CO2_umol_L/10^6,
         CH4_molL=CH4_umol_L/10^6)%>%
  mutate(lateral_CO2=qL*CO2_molL*12*(1^3)*86400*(1/width),
         lateral_CH4=qL*CH4_molL*12*(1^3)*86400*(1/width))%>%
  select(-width)%>%
  mutate(DOC_flux=qL*DOC,
         DIC_flux=qL*DIC)%>%filter(!is.na(Well), !is.na(ID))

ggplot(data = lateral_flux%>% filter(!is.na(Well), !is.na(ID), !Well=='0'),
       aes(x = WT_elevations, y=qL)) +
  geom_point()+
  facet_wrap(~ID+Well, scales='free')

# well specific relationships among C species and RC hydrology########

lateral_flux.lm <- lateral_flux %>%
  unite("ID.Well", Stream, Well, sep = ".")

cols <- c('DOC','DIC','lateral_CO2','lateral_CH4','qL','WT_elevations','ID.Well')
unique_sites <- unique(lateral_flux.lm$ID.Well[!is.na(lateral_flux.lm$ID.Well)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- lateral_flux.lm %>%
      filter(ID.Well == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)


DIC.elevation<- lmList(DIC ~ WT_elevations | ID.Well, data=lateral_flux.lm)
DIC.elevation.summ<-summary(DIC.elevation)
DIC.qL<- lmList(DIC ~ qL | ID.Well, data=lateral_flux.lm)
DIC.qL.summ<-summary(DIC.qL)

CO2.elevation<- lmList(lateral_CO2 ~ WT_elevations | ID.Well, data=lateral_flux.lm)
CO2.elevation.summ<-summary(CO2.elevation)
CO2.qL<- lmList(lateral_CO2 ~ qL | ID.Well, data=lateral_flux.lm)
CO2.qL.summ<-summary(CO2.qL)

CH4.elevation<- lmList(lateral_CH4 ~ WT_elevations | ID.Well, data=lateral_flux.lm)
CH4.elevation.summ<-summary(CH4.elevation)
CH4.qL<- lmList(lateral_CH4 ~ qL | ID.Well, data=lateral_flux.lm)
CH4.qL.summ<-summary(CH4.qL)


DOC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DOC", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("DOC", "qL")])) > 1

  # Initialize with NA
  DOC.elevation.p <- DOC.elevation.slope <- DOC.elevation.r2 <- NA
  DOC.qL.p <- DOC.qL.slope <- DOC.qL.r2 <- NA

  if (valid_elev) {
    DOC.elevation <- lm(DOC ~ WT_elevations, data = df)
    DOC.elevation.cf <- summary(DOC.elevation)
    DOC.elevation.p <- DOC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DOC.elevation.slope <- DOC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DOC.elevation.r2 <- DOC.elevation.cf$r.squared
  }

  if (valid_qL) {
    DOC.qL <- lm(DOC ~ qL, data = df)
    DOC.qL.cf <- summary(DOC.qL)
    DOC.qL.p <- DOC.qL.cf$coefficients["qL", "Pr(>|t|)"]
    DOC.qL.slope <- DOC.qL.cf$coefficients["qL", "Estimate"]
    DOC.qL.r2 <- DOC.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DOC.elevation.p = as.numeric(DOC.elevation.p),
    DOC.elevation.slope = as.numeric(DOC.elevation.slope),
    DOC.elevation.r2 = as.numeric(DOC.elevation.r2),
    DOC.qL.p = as.numeric(DOC.qL.p),
    DOC.qL.slope = as.numeric(DOC.qL.slope),
    DOC.qL.r2 = as.numeric(DOC.qL.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")

DIC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DIC", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("DIC", "qL")])) > 1

  # Initialize with NA
  DIC.elevation.p <- DIC.elevation.slope <- DIC.elevation.r2 <- NA
  DIC.qL.p <- DIC.qL.slope <- DIC.qL.r2 <- NA

  if (valid_elev) {
    DIC.elevation <- lm(DIC ~ WT_elevations, data = df)
    DIC.elevation.cf <- summary(DIC.elevation)
    DIC.elevation.p <- DIC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DIC.elevation.slope <- DIC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DIC.elevation.r2 <- DIC.elevation.cf$r.squared
  }

  if (valid_qL) {
    DIC.qL <- lm(DIC ~ qL, data = df)
    DIC.qL.cf <- summary(DIC.qL)
    DIC.qL.p <- DIC.qL.cf$coefficients["qL", "Pr(>|t|)"]
    DIC.qL.slope <- DIC.qL.cf$coefficients["qL", "Estimate"]
    DIC.qL.r2 <- DIC.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DIC.elevation.p = as.numeric(DIC.elevation.p),
    DIC.elevation.slope = as.numeric(DIC.elevation.slope),
    DIC.elevation.r2 = as.numeric(DIC.elevation.r2),
    DIC.qL.p = as.numeric(DIC.qL.p),
    DIC.qL.slope = as.numeric(DIC.qL.slope),
    DIC.qL.r2 = as.numeric(DIC.qL.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")

lateral_CO2_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("lateral_CO2", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("lateral_CO2", "qL")])) > 1

  # Initialize with NA
  lateral_CO2.elevation.p <- lateral_CO2.elevation.slope <- lateral_CO2.elevation.r2 <- NA
  lateral_CO2.qL.p <- lateral_CO2.qL.slope <- lateral_CO2.qL.r2 <- NA

  if (valid_elev) {
    lateral_CO2.elevation <- lm(lateral_CO2 ~ WT_elevations, data = df)
    lateral_CO2.elevation.cf <- summary(lateral_CO2.elevation)
    lateral_CO2.elevation.p <- lateral_CO2.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    lateral_CO2.elevation.slope <- lateral_CO2.elevation.cf$coefficients["WT_elevations", "Estimate"]
    lateral_CO2.elevation.r2 <- lateral_CO2.elevation.cf$r.squared
  }

  if (valid_qL) {
    lateral_CO2.qL <- lm(lateral_CO2 ~ qL, data = df)
    lateral_CO2.qL.cf <- summary(lateral_CO2.qL)
    lateral_CO2.qL.p <- lateral_CO2.qL.cf$coefficients["qL", "Pr(>|t|)"]
    lateral_CO2.qL.slope <- lateral_CO2.qL.cf$coefficients["qL", "Estimate"]
    lateral_CO2.qL.r2 <- lateral_CO2.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    lateral_CO2.elevation.p = as.numeric(lateral_CO2.elevation.p),
    lateral_CO2.elevation.slope = as.numeric(lateral_CO2.elevation.slope),
    lateral_CO2.elevation.r2 = as.numeric(lateral_CO2.elevation.r2),
    lateral_CO2.qL.p = as.numeric(lateral_CO2.qL.p),
    lateral_CO2.qL.slope = as.numeric(lateral_CO2.qL.slope),
    lateral_CO2.qL.r2 = as.numeric(lateral_CO2.qL.r2)
  )
})
CO2_table <- bind_rows(lateral_CO2_relationships, .id = "ID")

lateral_CH4_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("lateral_CH4", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("lateral_CH4", "qL")])) > 1

  # Initialize with NA
  lateral_CH4.elevation.p <- lateral_CH4.elevation.slope <- lateral_CH4.elevation.r2 <- NA
  lateral_CH4.qL.p <- lateral_CH4.qL.slope <- lateral_CH4.qL.r2 <- NA

  if (valid_elev) {
    lateral_CH4.elevation <- lm(lateral_CH4 ~ WT_elevations, data = df)
    lateral_CH4.elevation.cf <- summary(lateral_CH4.elevation)
    lateral_CH4.elevation.p <- lateral_CH4.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    lateral_CH4.elevation.slope <- lateral_CH4.elevation.cf$coefficients["WT_elevations", "Estimate"]
    lateral_CH4.elevation.r2 <- lateral_CH4.elevation.cf$r.squared
  }

  if (valid_qL) {
    lateral_CH4.qL <- lm(lateral_CH4 ~ qL, data = df)
    lateral_CH4.qL.cf <- summary(lateral_CH4.qL)
    lateral_CH4.qL.p <- lateral_CH4.qL.cf$coefficients["qL", "Pr(>|t|)"]
    lateral_CH4.qL.slope <- lateral_CH4.qL.cf$coefficients["qL", "Estimate"]
    lateral_CH4.qL.r2 <- lateral_CH4.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    lateral_CH4.elevation.p = as.numeric(lateral_CH4.elevation.p),
    lateral_CH4.elevation.slope = as.numeric(lateral_CH4.elevation.slope),
    lateral_CH4.elevation.r2 = as.numeric(lateral_CH4.elevation.r2),
    lateral_CH4.qL.p = as.numeric(lateral_CH4.qL.p),
    lateral_CH4.qL.slope = as.numeric(lateral_CH4.qL.slope),
    lateral_CH4.qL.r2 = as.numeric(lateral_CH4.qL.r2)
  )
})
CH4_table <- bind_rows(lateral_CH4_relationships, .id = "ID")

relationships<-left_join(DOC_table, DIC_table)
relationships<-left_join(relationships, CO2_table)
relationships<-left_join(relationships, CH4_table)

sig<-relationships%>%
  mutate(
    lateral_CH4.qL.slope=if_else(lateral_CH4.qL.p > 0.1, NA, lateral_CH4.qL.slope),
    lateral_CH4.elevation.slope=if_else(lateral_CH4.elevation.p > 0.1, NA, lateral_CH4.elevation.slope),

    lateral_CO2.qL.slope=if_else(lateral_CO2.qL.p > 0.1, NA, lateral_CO2.qL.slope),
    lateral_CO2.elevation.slope=if_else(lateral_CO2.elevation.p > 0.1, NA, lateral_CO2.elevation.slope),

    DOC.qL.slope=if_else(DOC.qL.p > 0.1, NA, DOC.qL.slope),
    DOC.elevation.slope=if_else(DOC.elevation.p > 0.1, NA, DOC.elevation.slope),

    DIC.qL.slope=if_else(DIC.qL.p > 0.1, NA, DIC.qL.slope),
    DIC.elevation.slope=if_else(DIC.elevation.p > 0.1, NA, DIC.elevation.slope)
  )

  # filter(lateral_CH4.qL.p > 0.1,
  #        lateral_CH4.elevation.p > 0.1,
  #        lateral_CO2.qL.p > 0.1,
  #        lateral_CO2.elevation.p > 0.1,
  #        DIC.qL.p > 0.1,
  #        DIC.elevation.p > 0.1,
  #        DOC.qL.p > 0.1,
  #        DOC.elevation.p > 0.1)



##Boxplots: distance#########
ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DIC, fill = as.factor(DistanceID))) +
  geom_boxplot() + geom_jitter(shape=1)+
  ylab("DIC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells") #DIC

ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DOC, fill = as.factor(DistanceID))) +
  geom_boxplot()+ geom_jitter(shape=1)+
  ylab("DOC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells") #DOC


ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CH4_sat, fill = as.factor(DistanceID))) +
  geom_boxplot()+ geom_jitter(shape=1)+
  ylab("CH4 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells") #CH4

ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CO2_sat, fill = as.factor(DistanceID))) +
  geom_boxplot()+ geom_jitter(shape=1)+
  ylab("CO2 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells") #CO2

###################

ggplot(data = lateral_flux , aes(x = qL, y = DOC)) +
  geom_point(size = 2) +
  facet_wrap(ID~Well, scales='free')+
  stat_poly_line(se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = lateral_flux , aes(x = qL, y = DIC)) +
  geom_point(size = 2) +
  facet_wrap(ID~Well, scales='free')+
  stat_poly_line(se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")




ggplot(data = lateral_flux , aes(x = lateral_CO2, y = WT_elevations)) +
  geom_point(size = 2) +
  facet_wrap(ID~Well, scales='free')+
  stat_poly_line(se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = lateral_flux , aes(x = lateral_CH4, y = WT_elevations)) +
  geom_point(size = 2) +
  facet_wrap(ID~Well, scales='free')+
  stat_poly_line(se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = lateral_flux , aes(x = DOC_flux, y = WT_elevations)) +
  geom_point(size = 2) +
  scale_y_log10()+
  facet_wrap(ID~Well, scales='free')+
  stat_poly_line(se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = lateral_flux , aes(x = DIC_flux, y = WT_elevations)) +
  geom_point(size = 2) +
  scale_y_log10()+
  facet_wrap(ID~Well, scales='free')+
  stat_poly_line(se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")
