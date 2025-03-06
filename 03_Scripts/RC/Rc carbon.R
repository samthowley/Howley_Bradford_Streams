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

#RC Carbon#########

#Create RC carbon dataset
RClog<-read_xlsx('01_Raw_data/RC log.xlsx')
RClog<- RClog%>%rename("WTdepth_m"="Wtdepth (m)") %>% select(Date, ID, Site, WTdepth_m, CO2_mv,pH, Temp) %>%rename('CO2'=CO2_mv)

RC_dim <- read_excel("01_Raw_data/RC log.xlsx",sheet = "Sheet1")
RC_dim<-RC_dim%>%select(Site,`Distance (ft)`,Distance_m)

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')%>%distinct(Site, Date, .keep_all = T)

C_RC<-full_join(DC_RC, RC_dim, by=c("Site"))
C_RC<-full_join(C_RC, RClog, by=c("Site","Date"))

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
         Well=0,
         ID=Stream)%>%
  select(Date, Stream, Well, DIC, DOC,`Distance (ft)`, Distance_m,
         ID,WTdepth_m, CO2,pH,Temp,CO2_umol_L,CH4_umol_L,N2O_umol_L,
         CO2_sat,CH4_sat,N2O_sat)

stream_RC<-rbind(streamC_edited,RC_all)

write_csv(RC_all, "02_Clean_data/allC_RC.csv")

#Figures########

allDIC <- all %>%filter(!is.na(DIC))
a <- ggplot(data = allDIC %>% filter(Stream == '5'),
            aes(x = Distance_m, group = Date, color = abs(WTdepth_m))) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10") +  labs(color = "WT Depth (m)")+
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DIC), size = 2) +
  ylab("DIC mg/L") +
  xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")

b <- ggplot(data = allDIC %>% filter(Stream == '6'),
            aes(x = Distance_m, group = Date, color = abs(WTdepth_m))) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10") + labs(color = "WT Depth (m)")+
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DIC), size = 2) +
  ylab("DIC mg/L") +
  xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")

c <- ggplot(data = allDIC %>% filter(Stream == '9'),
            aes(x = Distance_m, group = Date, color = abs(WTdepth_m))) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10") + labs(color = "WT Depth (m)")+
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DIC), size = 2) +
  ylab("DIC mg/L") +
  xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")

top<-plot_grid(a,b,c, nrow=1)

(bott<-ggplot(data = allDIC, aes(x = DIC, color=Distance_m)) +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient for continuous data
  geom_point(aes(y = WTdepth_m), size = 2) +
 xlab("DIC mg/L") + ylab("Water Table Depth (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free"))


plot_grid(top,bott, ncol=1)

a <- ggplot(data = all %>% filter(Stream == '5'),
            aes(x = Distance_m, group = Date, color = abs(WTdepth_m))) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10") +  labs(color = "WT Depth (m)")+
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DOC), size = 2) +
  ylab("DOC mg/L") +
  xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")

b <- ggplot(data = all %>% filter(Stream == '6'),
            aes(x = Distance_m, group = Date, color = abs(WTdepth_m))) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10") + labs(color = "WT Depth (m)")+
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DOC), size = 2) +
  ylab("DOC mg/L") +
  xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")

c <- ggplot(data = all %>% filter(Stream == '9'),
            aes(x = Distance_m, group = Date, color = abs(WTdepth_m))) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10") + labs(color = "WT Depth (m)")+
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DOC), size = 2) +
  ylab("DOC mg/L") +
  xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")

top<-plot_grid(a,b,c, nrow=1)

(bott<-ggplot(data = all, aes(x = DOC, color=Distance_m)) +
    scale_color_gradient(low = "blue", high = "red") +  # Gradient for continuous data
    geom_point(aes(y = WTdepth_m), size = 2) +
    xlab("DOC mg/L") + ylab("Water Table Depth (m)") +
    theme(legend.position = "bottom") +
    facet_wrap(~ ID, scales = "free"))

plot_grid(top,bott, ncol=1)
