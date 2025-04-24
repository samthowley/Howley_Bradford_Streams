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

ggplot(RC_dims, aes(surface2WT, WT_elevations)) +
  geom_point()+
  facet_wrap(~ ID, ncol=3, scales = 'free')


#DOC DIC#########

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')%>%distinct(Site, Date, .keep_all = T)

C_RC<-full_join(DC_RC, RC_dims, by=c("Site", "Date"))

C_RC<-C_RC %>% mutate(ID=as.character(ID), CO2=as.numeric(CO2), pH=as.numeric(pH)) %>%
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
  mutate(`Distance (ft)`= -0.5,
         `Distance_m`= -0.5,
         WTdepth_m=0,
         Well=0, DistanceID='0',
         ID=Stream,surface2WT=0,
         surface_elevation_m=0,
         WT_elevations=0)%>%
  select(Date,Stream,Well,DIC,DOC,ID,surface2WT,CO2,
         pH,Temp, `Distance (ft)`,Distance_m,DistanceID,surface_elevation_m,
         WT_elevations,CO2_umol_L,CH4_umol_L,N2O_umol_L,CO2_sat,
         CH4_sat,N2O_sat)

stream_RC<-rbind(streamC_edited,RC_all)%>%mutate(Stream=as.factor(Stream))

write_csv(RC_all, "02_Clean_data/allC_RC.csv")

#Figures########
RC_all<-read_csv("02_Clean_data/allC_RC.csv")


#DIC

a<-ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DIC, fill = as.factor(DistanceID))) +
  geom_boxplot() +    ylab("DIC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")

b<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = surface2WT, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
    geom_point(aes(y = DIC), shape = 21,size = 2, color = "black", stroke = 0.02) +
    ylab("DIC mg/L") +
    xlab("WTdepth") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")+theme(legend.position = 'none')

c<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = WT_elevations, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DIC), shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("DIC mg/L") +
  xlab("WT elevation") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")+theme(legend.position = 'none')

(DIC<-plot_grid(a,b,c, ncol=1))


#DOC

a<-ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DOC, fill = as.factor(DistanceID))) +
  geom_boxplot() +    ylab("DOC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")

b<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = surface2WT, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DOC),  shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("DOC mg/L") +
  xlab("WTdepth") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

c<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = WT_elevations, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DOC), shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("DOC mg/L") +
  xlab("WT elevation") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

(DOC<-plot_grid(a,b,c, ncol=1))

#CO2
a<-ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CO2_sat, fill = as.factor(DistanceID))) +
  geom_boxplot() +    ylab("CO2 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")

b<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = surface2WT, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = CO2_sat), shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("CO2 Saturation") +
  xlab("WTdepth") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

c<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = WT_elevations, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = CO2_sat), shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("CO2 Saturation") +
  xlab("WT Elevation") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")
(CO2<-plot_grid(a,b,c, ncol=1))


#CH4
a<-ggplot(
  RC_all %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CH4_sat, fill = as.factor(DistanceID))) +
  geom_boxplot() +    ylab("CH4 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")

b<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = surface2WT, color=as.factor(DistanceID))) +
  scale_color_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = CH4_sat), size = 2) +
  ylab("CH4 Saturation") +
  xlab("WTdepth") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

c<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = WT_elevations, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = CH4_sat), shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("CH4 Saturation") +
  xlab("WT Elevation") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

(CH4<-plot_grid(a,b,c, ncol=1))

#WT Elevations figures#########

DIC<-ggplot(data = RC_all%>% filter(Stream=='5', !Well=='0'),aes(x = WT_elevations,y = DIC, fill=as.factor(Well))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("DIC mg/L") +
  xlab("WT elevation") +labs(fill = "Wells")+facet_wrap(~Well, scale='free')+
  labs(color = "Wells")

DOC<-ggplot(data = RC_all%>% filter(Stream=='5', !Well=='0'),aes(x = WT_elevations,y = DOC, fill=as.factor(Well))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("DOC mg/L") +
  xlab("WT elevation") +labs(fill = "Wells")+facet_wrap(~Well, scale='free')+
  labs(color = "Wells")

CO2<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = WT_elevations,y = CO2_sat, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("CO2 Saturation") +
  xlab("WT Elevation") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

CH4<-ggplot(data = RC_all%>% filter(!is.na(Stream)),aes(x = WT_elevations,y = CH4_sat, fill=as.factor(DistanceID))) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(shape = 21,size = 2, color = "black", stroke = 0.02) +
  ylab("CH4 Saturation") +
  xlab("WT Elevation") +labs(fill = "Wells")+facet_wrap(~Stream, scale='free')+
  labs(color = "Wells")

plot_grid(DOC, DIC, nrow=2)
plot_grid(CO2, CH4, nrow=2)

#####By well############

ggplot(data = RC_all%>% filter(Stream=='9', !Well=='0'),aes(x = WT_elevations,y = DOC)) +
  geom_point(size = 2, color = "black", stroke = 0.02) +
  #geom_smooth(method='lm')+
  ylab("DOC mg/L") +
  xlab("WT elevation") +labs(fill = "Wells")+facet_wrap(~Well, scale='free')+
  labs(color = "Wells")
