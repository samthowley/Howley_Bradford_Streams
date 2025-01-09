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
                legend.text=element_text(size = 10),
                legend.title =element_text(size = 10),
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

dim<-left_join(depth, Q, by=c('ID', 'Date'))
dim<-dim %>% filter(ID=='6'| ID=='5'| ID=='9')

#RC Carbon#########

#Create RC carbon dataset
RClog<-read_xlsx('01_Raw_data/RC log.xlsx')
RClog<- RClog%>%rename("WTdepth_m"="Wtdepth (m)") %>% select(Date, ID, Site, WTdepth_m, CO2_mv,pH, Temp) %>%rename('CO2'=CO2_mv)

RC_dim <- read_excel("01_Raw_data/RC log.xlsx",sheet = "Sheet1")
RC_dim<-RC_dim%>%select(Site,`Distance (ft)`,Distance_m)

DC_RC<-read_csv('04_Output/TDC_RC.csv')
DC_RC<-DC_RC%>%select('Date','Site',"DIC",'DOC')

C_RC<-left_join(DC_RC, RC_dim, by=c("Site"))
C_RC<-left_join(C_RC, RClog, by=c("Site","Date"))

C_RC<-C_RC %>% mutate(ID=as.character(ID), CO2=as.numeric(CO2), pH=as.numeric(pH)) %>%
  separate(Site, into = c("Stream", "Well"), sep = "GW")

#include streams

streamC<-read_csv('04_Output/TDC_stream.csv')
streamC_edited<-streamC %>% filter(ID %in% c("5","6","9"))%>%
  rename(Stream=Site, Temp=Temp_pH)%>%
  mutate(`Distance (ft)`= -0.5, `Distance_m`= -0.5, WTdepth_m=0, Well=NA)%>%
  select(Date, Stream, Well, DIC, DOC,`Distance (ft)`, Distance_m, ID, WTdepth_m, CO2,pH,Temp)

all<-rbind(streamC_edited,C_RC)
all<-all %>% mutate(month=month(Date))%>%filter(!is.na(ID))
write_csv(all, "02_Clean_data/allC_RC.csv")

allDIC <- all %>%filter(!is.na(DIC))
ggplot(data = allDIC, aes(x = Distance_m, group = Date, color=month)) +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient for continuous data
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DIC), size = 2, shape = 1) +
  geom_line(aes(y = DIC)) +
  ylab("DIC mg/L") + xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")


ggplot(data = allDIC, aes(x = WTdepth_m, color=month)) +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient for continuous data
  geom_point(aes(y = DIC), size = 2) +
  xlab("DIC mg/L") + ylab("Water Table Depth (m)") +scale_y_reverse()+
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")


plot_grid(a,b, ncol=1)

ggplot(data = all, aes(x = Distance_m, group = Date, color=month)) +
  scale_color_gradient(low = "yellow", high = "purple") +  # Gradient for continuous data
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DOC), size = 2, shape = 1) +
  geom_line(aes(y = DOC)) +
  ylab("DOC mg/L") + xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")


ggplot(data = allDIC, aes(x = WTdepth_m, color=month)) +
  scale_color_gradient(low = "yellow", high = "purple") +  # Gradient for continuous data
  geom_point(aes(y = DIC), size = 2) +
  xlab("DIC mg/L") + ylab("Water Table Depth (m)") +scale_y_reverse()+
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free")


plot_grid(a,b, ncol=1)
