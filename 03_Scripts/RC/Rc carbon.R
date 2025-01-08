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
all<-all %>% mutate(Date=ymd(Date))
write_csv(all, "02_Clean_data/allC_RC.csv")

(a<-ggplot(data = all, aes(x = Distance_m, group = Date, color=Date)) +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient for continuous data
  geom_vline(xintercept = 0, colour = "gray", size = 1.5) +
  geom_point(aes(y = DIC), size = 2, shape = 1) +
  geom_line(aes(y = DIC)) +
  ylab("DIC mg/L") + xlab("Distance (m)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ ID, scales = "free"))

(b<-ggplot(data=C_RC, aes(x=WTdepth_m, group = Date)) +
    geom_point(aes(y=DIC), size = 2)+
    ylab("DOC mg/L")+xlab('Water Table Depth (m)')+
    #geom_point(aes(y=CO2_mv),size=2)+
    theme(legend.position = "bottom")+
    facet_wrap(~ ID, scales='free'))


plot_grid(a,b, ncol=1)

(a<-ggplot(data = C_RC, aes(x = Distance_m, color = Date, group=Date)) +
    scale_color_gradient(low = "blue", high = "red") +
    theme(legend.position = "bottom") + geom_vline(xintercept = 0, colour = "gray", size=1.5) +
    geom_point(aes(y = DOC), size = 2) +
    geom_line(aes(y = DOC)) +
    ylab("DOC mg/L") + xlab('Distance (m)')+ labs(color = "Date")+
    facet_wrap(~ ID, scales = 'free') +
    ggtitle('River Corridor')+theme(legend.position = "none"))

(b<-ggplot(data=C_RC, aes(x=WTdepth_m, group = Date)) +
  geom_point(aes(y=DOC), size = 2)+
  ylab("DOC mg/L")+xlab('Water Table Depth (m)')+
  #geom_point(aes(y=CO2_mv),size=2)+
  theme(legend.position = "bottom")+
  facet_wrap(~ ID, scales='free'))

plot_grid(a,b, ncol=1)


a<-ggplot(data = C_RC, aes(x = Distance_m, color = Date, group=Date)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "bottom") + geom_vline(xintercept = 0, colour = "gray", size=1.5) +
  geom_point(aes(y = pH), size = 2) +
  geom_line(aes(y = pH)) +
  ylab("pH") + xlab('Distance (m)')+ labs(color = "Date")+
  facet_wrap(~ ID, scales = 'free') +
  ggtitle('River Corridor')+theme(legend.position = "none")

b<-ggplot(data=C_RC, aes(x=WTdepth_m, group = Date)) +
  geom_point(aes(y=pH), size = 2)+
  ylab("pH")+xlab('Water Table Depth (m)')+
  #geom_point(aes(y=CO2_mv),size=2)+
  theme(legend.position = "bottom")+
  facet_wrap(~ ID, scales='free')

plot_grid(a,b, ncol=1)

###Interpolating CO2######
CO2 <- function(master) {
  master <- master[complete.cases(master[ , c('Temp','pH','Water_press','Q')]), ]
  master$Temp_K<- master$Temp+273.15

  master$exp<-2400*((1/master$Temp_K)-(1/298.15))
  master$KH<-0.034*2.178^(master$exp)#mol/L/atm

  master$K1<-K1(S=0.01, T=master$Temp, P=master$Water_press)
  master$K2<-K2(S=0.01, T=master$Temp, P=master$Water_press)

  master$pK1<- -log10(master$K1)
  master$pK2<- -log10(master$K2)

  master$HCO3_molL<-master$DIC_mgL/61000
  master$CO2_molL<-master$HCO3_molL/(10^(master$pH-master$pK1))

  master$CO2_atm<-master$CO2_molL/master$KH
  master$CO2_ppm_inter<-master$CO2_atm*1000000

  master<-master %>% select(Date, ID, Site, CO2_ppm_inter, CO2_molL, HCO3_molL)
  return(master)}

RCc<-read_csv("02_Clean_data/allC_RC.csv")
RCc$ID<-as.character(RCc$ID)

depth<-read_csv("02_Clean_data/depth.csv")
discharge<-read_csv("02_Clean_data/discharge.csv")

RCc<-left_join(RCc, depth, by=c('Date','ID'))
RCc<-left_join(RCc, discharge, by=c('Date','ID'))

CO2_inter<-CO2(RCc)
RCc<-left_join(RCc, CO2_inter, by=c('Date','ID','Site'))
RCc <- RCc[!duplicated(RCc[c('Site','Date')]),]


theme_set(theme(axis.text.x = element_text(size = 17, angle=0),
                axis.text.y = element_text(size = 17, angle=0),
                axis.title.y =element_text(size = 17, color = "black"),
                axis.title.x =element_text(size = 17),
                plot.title = element_text(size = 17),
                legend.position = "none",
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

a<-ggplot(RCc, aes(x=Distance_m, y=DOC_mgL, color=Q))+
  scale_color_gradient(low = "blue", high = "red")+
  geom_point()+facet_wrap(~ ID, ncol=5, scales = "free")+ggtitle('River Corridor')
b<-ggplot(RCc, aes(x=Distance_m, y=DIC_mgL, color=Q))+
  scale_color_gradient(low = "blue", high = "red")+
  geom_point()+facet_wrap(~ ID, ncol=5, scales='free')+ggtitle('River Corridor')
plot_grid(a,b,ncol=1)

(a<-ggplot(data=RCc, aes(x=Qbase)) +
    geom_point(aes(y=DOC_mgL),color='blue',size=3)+
    geom_point(aes(y=DIC_mgL),color='orange',size=3)+
    facet_wrap(~ Site, ncol=5)+ ggtitle('Rver Corridor'))

(a<-ggplot(data=RCc, aes(x=WTdepth, color=Distance_m)) +
    scale_color_gradient(low = "blue", high = "red")+
    geom_point(aes(y=DOC_mgL),size=3)+
    #geom_point(aes(y=DIC_mgL),size=3)+
    facet_wrap(~ Site, ncol=5, scales='free')+ ggtitle('River Corridor'))


write_csv(C_RC, "02_Clean_data/allC_RC.csv")
