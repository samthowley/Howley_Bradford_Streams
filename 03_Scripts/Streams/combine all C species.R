#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(ggtern)
library(ggpmisc)

theme_set(theme(axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 17, angle = 90),
                axis.title.x = element_text(size = 17),
                plot.title = element_text(size = 17),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 12),
                legend.title =element_blank(),
                legend.position ="bottom",
                panel.grid.major.x = element_line(color = "black"),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_line(color = "black", linetype = "dashed"),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


#Edit dims######
depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')
length<-read_csv('02_Clean_data/stream area.csv')

depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth, Temp_PT)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(Q, depth, by=c('ID', 'Date'))

#sample C##########

shimadzu<-read_csv('04_Output/TDC_stream.csv')


alkalinity <- read_csv("02_Clean_data/alkalinity.csv")
alkalinity_edited<-alkalinity%>%select(Date, ID, DIC_mgL, CO2_mgL, HCO3_mgL, CO3_mgL)

combined<-left_join(shimadzu, alkalinity_edited, by=c('Date', 'ID'))


totC<-combined %>%distinct(Date, ID, .keep_all = T)%>%
  mutate(DIC = if_else(is.na(DIC), as.numeric(DIC_mgL), DIC))%>%
  select(Date, ID, DIC, DOC, POC, ID, depth, pH, Q, CO2, Temp_pH)%>%
  mutate(POC=abs(POC))

totC<-totC %>% mutate(TotalC=DIC+DOC+POC)%>%
  mutate(DIC_perc=DIC/TotalC, DOC_perc=DOC/TotalC, POC_perc=POC/TotalC)

ggplot(totC%>%filter(ID != is.na(ID)), aes(x=Q))+
  geom_point(aes(y=DOC, color="DOC"),size=3, shape=1)+
  geom_point(aes(y=DIC, color= "DIC"), size=3)+
  geom_point(aes(y=POC, color="POC"), size=3)+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  scale_x_log10()+scale_y_log10()+
  xlab(expression('Discharge'~ft^3/s))+ylab('mg/L')+
  facet_wrap(~ ID, ncol=3, scales='free')+theme(legend.position = 'bottom')+ggtitle("Stream Carbon Species")


#include gas samples#######

Picarro_gas <- read_csv("04_Output/Picarro_gas.csv")
strm_gas<-Picarro_gas%>%filter(chapter=='stream')%>%select(-Temp_K, -chapter)

all_sampled_C<-full_join(totC,strm_gas)


#FDOM##############

eem_stream <- read_csv("04_Output/eem_stream.csv")%>%select(-Rep, -chapter, -Site)


all_sampled_C<-full_join(all_sampled_C,eem_stream)



write_csv(all_sampled_C, "04_Output/stream_sampledC.csv")

#box plots#####
names(totDC)
POC<-all_sampled_C %>% select(Date,ID,Q,depth, POC)%>% rename(Conc=POC)%>%mutate(Species= 'POC')
DIC<-all_sampled_C %>% select(Date,ID,Q,depth, DIC)%>% rename(Conc=DIC)%>%mutate(Species= 'DIC')
DOC<-all_sampled_C %>% select(Date,ID,Q,depth, DOC)%>% rename(Conc=DOC)%>%mutate(Species= 'DOC')

long_C<- rbind(POC, DIC, DOC)

order <- c("5", "5a", "15", "9", '13', '6', '6a', '3', '7')

ggtern(data=totC,aes(DOC,DIC*10,POC*10, colour = ID))+
  #scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC mg/L",y="DIC deci-mg/L",z="POC deci-mg/L")+
  theme_minimal_grid()+
  theme(legend.position = "bottom",
        axis.title = element_text(size =9))

 ggtern(data=totC,aes(DOC,DIC*10,POC*10, colour = depth))+
  #scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC mg/L",y="DIC deci-mg/L",z="POC deci-mg/L")+
  theme_minimal_grid()+
  theme(legend.position = "bottom",
        axis.title = element_text(size =9))


ggtern(data=totC%>%filter(ID %in% c('5','6','9')),aes(DOC,DIC*10,POC*10, colour = Q))+
  scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC",y="DIC",z="POC")+
  theme_minimal_grid()+theme(legend.position = "bottom")+
  facet_wrap(~ID, scale="free")



b<-ggplot(all_sampled_C%>%filter(ID != is.na(ID), ID != '6a'), aes(x=Q))+
  geom_point(aes(y=DOC, color="DOC"),size=3)+
  geom_point(aes(y=DIC, color= "DIC"), size=3)+
  geom_point(aes(y=POC, color="POC"), size=3)+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  scale_x_log10()+scale_y_log10()+
  xlab(expression('Discharge'~ft^3/s))+ylab('mg/L')+
  facet_wrap(~ ID, ncol=3, scales='free')+theme(legend.position = 'bottom')+ggtitle("Stream Carbon Species")

FDOM<-all_sampled_C%>%select(Date, ID, hix, bix, fi, Q)%>%
  filter(!is.na(hix))%>%
  mutate(fraction=hix/bix)

ggplot(FDOM%>%filter(ID != is.na(ID), ID != '6a'), aes(x=Q))+
  #geom_point(aes(y=hix, color="hix"), size=2)+
   geom_point(aes(y=bix, color="bix"), size=2)+
  # geom_point(aes(y=fi, color="fi"), size=2)+
  scale_y_log10()+scale_x_log10()+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  xlab(expression('Discharge'~ft^3/s))+
  ylab('Biological Index (bix)')+
  facet_wrap(~ ID, ncol=3, scales='free')+theme(legend.position = 'bottom')+
  ggtitle("Stream Carbon Quality")

plot_grid(a,b, ncol=1)


ggplot(FDOM%>%filter(ID != is.na(ID), ID != '6a'), aes(x=Q))+
  geom_point(aes(y=fraction, color="fraction"), size=2)+
  scale_y_log10()+scale_x_log10()+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  xlab(expression('Discharge'~ft^3/s))+ylab('mg/L')+
  facet_wrap(~ ID, ncol=3, scales='free')+theme(legend.position = 'bottom')+ggtitle("Stream Carbon Species")


for_histogram<-FDOM%>%
  group_by(ID)%>%
  mutate(
         Q_quartile = ntile(Q, 4))%>% ungroup


ggplot(for_histogram %>% filter(!is.na(Q_quartile)),
       aes(x = as.factor(Q_quartile), y = hix)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  xlab('IQR of Discharge') +
  facet_wrap(~ID, scales = 'free')+
  scale_y_log10()

