#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)
library(ggtern)
library(tibble)

#need to include stream LB too
theme_set(theme(axis.text.x = element_text(size = 32),
                axis.text.y = element_text(size = 32),
                axis.title.y = element_text(size = 35, angle = 90),
                axis.title.x = element_text(size = 35),
                plot.title = element_text(size = 35),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                strip.text = element_text(size = 32)))

#Edit dims######
depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')

depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth)%>% filter(depth>0)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T)) %>%
  select(Date, ID, Q) %>% filter(Q>1)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(depth, Q, by=c('ID', 'Date'))
dim_edited<-dim %>%filter(ID %in% c('6','9','5'))

fa <- read_excel("01_Raw_data/Long. Log.xlsx", sheet = "dims")%>%
  mutate(Site=as.factor(Site), ID=as.factor(ID))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=if_else(is.na(Long), '0', Long))

Q.fa<-left_join(dim_edited, fa, by='ID')%>%
  mutate(Q.prop=Q*Q_fraction)%>%
  select(-Q_fraction, -RASTERVALU)

#Optional: Water Carbon Samples############
shimadzu_stream<- read_csv("04_Output/TDC_stream.csv")%>%
  select(-depth, -Q, -pH, -CO2, -Temp_pH, -chapter)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=if_else(is.na(Long), '0', Long))%>%
  filter(ID %in% c('5', '6', '9', '3'))

shimadzu_samples<-read_csv("04_Output/TDC_long.csv")%>%
  mutate(ID=as.character(ID))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  select(-depth, -Q, -pH, -CO2, -Temp_pH, -chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))

shimadzu_samples_all<-rbind(shimadzu_stream,shimadzu_samples)

field_log <- read_excel("01_Raw_data/Long. Log.xlsx",
                       sheet = "Long. Log", col_types = c("numeric",
                                                          "numeric", "date", "skip", "numeric",
                                                          "numeric", "numeric", "numeric",
                                                          "numeric", "skip", "skip"))%>%
  filter(!is.na(Site))%>%
  rename(Date=Visited)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")

water_samples<-left_join(shimadzu_samples_all, field_log)%>%
  mutate(POC=abs(POC))


#Optional:include gas samples####
long_gas <- read_csv("04_Output/Picarro_gas.csv")%>%
  filter(chapter=='long')%>%
  separate(ID, into = c("ID", "Long"), sep = "\\.")%>%
  select(-chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))

stream_gas <- read_csv("04_Output/Picarro_gas.csv")%>%
  filter(chapter=='stream')%>%
  separate(ID, into = c("ID", "Long"), sep = "\\.")%>%
  select(-chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))%>%
  filter(ID %in% c('5','6','9','3'))

gas_samples<-rbind(long_gas, stream_gas)

all_samples<-full_join(water_samples, gas_samples)%>%
  mutate(ID_Long = paste(ID, Long, sep = "_"))%>%
  mutate(ID_Long=if_else(ID_Long=='3_0', '3_1', ID_Long),
         ID_Long=if_else(ID_Long=='3_NA', '3_1', ID_Long),
         ID_Long=if_else(ID_Long=='3_3', '3_4', ID_Long),
         ID_Long=if_else(ID_Long=='5_NA', '5_0', ID_Long),
         ID_Long=if_else(ID_Long=='6_NA', '6_0', ID_Long),
         ID_Long=if_else(ID_Long=='9_NA', '9_0', ID_Long))%>%
  filter(!ID_Long %in% c("9_Sam", "5_5", "9_5", "3_3", "6_4", "5_6", "6_0"))%>%
  mutate(ID=if_else(ID_Long=='3_1', '6', ID),
         ID=if_else(ID_Long=='3_2', '6', ID),
         ID=if_else(ID_Long=='3_4', '6', ID))%>%select(-Temp_K)

c.q<-left_join(all_samples,Q.fa)

write_csv(c.q, "04_Output/master_long.csv")


#regression####
c.q <- read_csv("04_Output/master_long.csv")
cols <- c('DOC','DIC',"CO2_umol_L","CH4_umol_L",'ID_Long', "Q.prop" , "distance", 'ID')
unique_sites <- unique(c.q$ID_Long[!is.na(c.q$ID_Long)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- c.q %>%
      filter(ID_Long == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites)

DOC_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for DOC or Q.prop
  df <- df %>% filter(!is.na(DOC), !is.na(Q.prop), Q.prop > 0, DOC > 0)
  valid_elev <- nrow(df) > 1
  DOC.C.Q.p <- DOC.C.Q.slope <- DOC.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(DOC) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      DOC.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(DOC.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        DOC.C.Q.p <- DOC.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        DOC.C.Q.slope <- DOC.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        DOC.C.Q.r2 <- DOC.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    DOC.C.Q.p = as.numeric(DOC.C.Q.p),
    DOC.C.Q.slope = as.numeric(DOC.C.Q.slope),
    DOC.C.Q.r2 = as.numeric(DOC.C.Q.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")

DIC_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for DIC or Q.prop
  df <- df %>% filter(!is.na(DIC), !is.na(Q.prop), Q.prop > 0, DIC > 0)
  valid_elev <- nrow(df) > 1
  DIC.C.Q.p <- DIC.C.Q.slope <- DIC.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(DIC) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      DIC.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(DIC.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        DIC.C.Q.p <- DIC.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        DIC.C.Q.slope <- DIC.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        DIC.C.Q.r2 <- DIC.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    DIC.C.Q.p = as.numeric(DIC.C.Q.p),
    DIC.C.Q.slope = as.numeric(DIC.C.Q.slope),
    DIC.C.Q.r2 = as.numeric(DIC.C.Q.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")

CO2_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for CO2 or Q.prop
  df <- df %>% filter(!is.na(CO2_umol_L), !is.na(Q.prop), Q.prop > 0, CO2_umol_L > 0)
  valid_elev <- nrow(df) > 1
  CO2.C.Q.p <- CO2.C.Q.slope <- CO2.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(CO2_umol_L) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      CO2.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(CO2.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        CO2.C.Q.p <- CO2.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        CO2.C.Q.slope <- CO2.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        CO2.C.Q.r2 <- CO2.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    CO2.C.Q.p = as.numeric(CO2.C.Q.p),
    CO2.C.Q.slope = as.numeric(CO2.C.Q.slope),
    CO2.C.Q.r2 = as.numeric(CO2.C.Q.r2)
  )
})
CO2_table <- bind_rows(CO2_relationships, .id = "ID")

CH4_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for CH4 or Q.prop
  df <- df %>% filter(!is.na(CH4_umol_L), !is.na(Q.prop), Q.prop > 0, CH4_umol_L > 0)
  valid_elev <- nrow(df) > 1
  CH4.C.Q.p <- CH4.C.Q.slope <- CH4.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(CH4_umol_L) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      CH4.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(CH4.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        CH4.C.Q.p <- CH4.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        CH4.C.Q.slope <- CH4.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        CH4.C.Q.r2 <- CH4.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    CH4.C.Q.p = as.numeric(CH4.C.Q.p),
    CH4.C.Q.slope = as.numeric(CH4.C.Q.slope),
    CH4.C.Q.r2 = as.numeric(CH4.C.Q.r2)
  )
})
CH4_table <- bind_rows(CH4_relationships, .id = "ID")


relationships<-left_join(DOC_table, DIC_table, by='ID')
relationships<-left_join(relationships,CO2_table, by='ID')
relationships<-left_join(relationships,CH4_table, by='ID')


#wetland influence######

wetland_buffers <- read_csv("wetland proportion buffer.csv")%>% filter(Basin %in% c('5', '6', '9')) %>%
  select(Basin, buffer_radius, proportion)%>%rename(ID=Basin)

wetland_proxim <- read_csv("01_Raw_data/wetland_proxim.csv")%>% filter(Site %in% c('5', '6', '9')) %>%
  select(Site, NEAR_DIST)%>%rename(ID=Site, nearest_wetland=NEAR_DIST)

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>% filter(Basin_Name %in% c('5', '6', '9')) %>%
  select(Basin_Name, PERCENTAGE) %>% rename(ID=Basin_Name, wetland_perc=PERCENTAGE)

wetland<-left_join(wetland_buffers, wetland_proxim, by='ID')
wetland<-left_join(wetland, wetland_cover, by='ID')

write_csv(wetland, "04_Output/wetland_influence.csv")
