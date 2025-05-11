#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)

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


#Edit dims######
depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')

depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth)%>% filter(depth>0)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial) %>% filter(Q>1)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-full_join(depth, Q, by=c('ID', 'Date'))
dim<-dim %>% filter(ID=='6'| ID=='5'| ID=='9')

#Create RC carbon dataset
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

#Figures########
RC_all<-read_csv("02_Clean_data/allC_RC.csv")

##Boxplots: distance

ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DIC, fill = as.factor(DistanceID))) +
  geom_violin() +    ylab("DIC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")

ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DOC, fill = as.factor(DistanceID))) +
  geom_violin() +    ylab("DOC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")


ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CH4_sat, fill = as.factor(DistanceID))) +
  geom_violin() +    ylab("CH4 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")

#Overview######
ggplot(data = RC_all %>% filter(!is.na(ID)), aes(x = WT_elevations, y = DOC, fill=Distance_m)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DOC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~ID, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ I(log10(x)),  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all,aes(x = surface2WT, y = DOC, color=as.factor(ID))) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DOC mg/L") +
  xlab("Steam Bed to Water Table Height")

#scatter plots: elevation v DOC by site and by wells

ggplot(data = RC_all %>% filter(ID=='5'), aes(x = WT_elevations, y = DOC)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DOC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='6'), aes(x = WT_elevations, y = DOC)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DOC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='9'), aes(x = WT_elevations, y = DOC)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DOC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

#scatter plots: elevation v DIC by site and by wells

ggplot(data = RC_all %>% filter(ID=='5'), aes(x = WT_elevations, y = DIC)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DIC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='6'), aes(x = WT_elevations, y = DIC)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DIC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='9'), aes(x = WT_elevations, y = DIC)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("DIC mg/L") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

#scatter plots: elevation v CO2 by site and by wells

ggplot(data = RC_all %>% filter(ID=='5'), aes(x = WT_elevations, y = CO2)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("CO2") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='6'), aes(x = WT_elevations, y = CO2)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("CO2") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='9'), aes(x = WT_elevations, y = CO2)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("CO2") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")



#scatter plots: elevation v CH4 by site and by wells

ggplot(data = RC_all %>% filter(ID=='5'), aes(x = WT_elevations, y = CH4_sat)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("CH4_sat") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='6'), aes(x = WT_elevations, y = CH4_sat)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("CH4_sat") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

ggplot(data = RC_all %>% filter(ID=='9'), aes(x = WT_elevations, y = CH4_sat)) +
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(size = 2) +
  ylab("CH4_sat") +
  xlab("Steam Bed to Water Table Height")+
  facet_wrap(~Well, scales='free')+stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")

#Find Slopes of change by wells#####################

cols <- c('Well', 'DIC', 'DOC', 'surface2WT', 'CO2_umol_L', "CH4_umol_L", "N2O_umol_L",
          'Distance_m', 'WT_elevations', 'ID', 'ID_Well')
unique_sites <- unique(RC_all$ID[!is.na(RC_all$ID)])

streams <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- RC_all %>%
      filter(ID_Well == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

streams_edited <- lapply(streams, function(df) {
  (DOC.WT<-lm(DOC ~ WT_elevations, data = df))
  cf <- coef(DOC.WT)
  (Slope.DOC.WT<- cf[2])
  (Inter.DOC.WT <- cf[1])

  (DIC.WT<-lm(DIC ~ WT_elevations, data = df))
  cf <- coef(DIC.WT)
  (Slope.DIC.WT <- cf[2])
  (Inter.DIC.WT <- cf[1])

  (CH4.WT<-lm(CH4_umol_L ~ WT_elevations, data = df))
  cf <- coef(CH4.WT)
  (Slope.CH4.WT <- cf[2])
  (Inter.CH4.WT <- cf[1])

  (CO2.WT<-lm(CO2_umol_L ~ WT_elevations, data = df))
  cf <- coef(CO2.WT)
  (Slope.CO2.WT <- cf[2])
  (Inter.CO2.WT <- cf[1])

  df<-df%>%
    mutate(
      Slope_CO2.WT=as.numeric(c(Slope.CO2.WT)),
      Slope_DOC.WT=as.numeric(c(Slope.DOC.WT)),
      Slope_DIC.WT=as.numeric(c(Slope.DIC.WT)),
      Slope_CH4.WT=as.numeric(c(Slope.CH4.WT)),

      DOC.WTInter=as.numeric(c(Inter.DOC.WT)),
      DIC.WTInter=as.numeric(c(Inter.DIC.WT)),
      CO2.WTInter=as.numeric(c(Inter.CO2.WT)),
      CH4.WTInter=as.numeric(c(Inter.CH4.WT))

    )%>%
    summarize(
      Slope_CO2.WT=mean(Slope_CO2.WT, na.rm=T),
      Slope_CH4.WT=mean(Slope_CH4.WT, na.rm=T),
      Slope_DIC.WT=mean(Slope_DIC.WT, na.rm=T),
      Slope_DOC.WT=mean(Slope_DOC.WT, na.rm=T),

      Inter.DOC.WT=mean(Inter.DOC.WT, na.rm=T),
      Inter.DIC.WT=mean(Inter.DIC.WT, na.rm=T),
      Inter.CO2.WT=mean(Inter.CO2.WT, na.rm=T),
      Inter.CH4.WT=mean(Inter.CH4.WT, na.rm=T),
    )
})


slopes <- bind_rows(streams_edited, .id = "ID")


