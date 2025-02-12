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

combined<-full_join(shimadzu, alkalinity_edited, by=c('Date', 'ID'))


totC<-combined %>%distinct(Date, ID, .keep_all = T)%>%
  mutate(DIC = if_else(is.na(DIC), as.numeric(DIC_mgL), DIC))%>%
  select(Date, ID, DIC, DOC, POC, ID, depth, pH, Q, CO2)%>%
  mutate(POC=abs(POC))


ggplot(totC, aes(x=Q))+
  geom_point(aes(y=DOC, color="DOC"),size=3, shape=1)+
  geom_point(aes(y=DIC, color= "DIC"), size=3)+
  geom_point(aes(y=POC, color="POC"), size=3)+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  scale_x_log10()+scale_y_log10()+
  xlab(expression('Discharge'~m^3/s))+ylab('mg/L')+
  facet_wrap(~ ID, ncol=3, scales='free')+theme(legend.position = 'bottom')+ggtitle("Stream Carbon Species")


ggtern(data=totC ,aes(DOC_perc,DIC_perc,POC_perc, colour = ID))+
  #scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC%",y="DIC%",z="POC%")+
  theme_minimal_grid()+theme(legend.position = "bottom")


totC<-totC %>% mutate(TotalC=DIC+DOC+POC)%>%
  mutate(DIC_perc=DIC/TotalC, DOC_perc=DOC/TotalC, POC_perc=POC/TotalC)

write_csv(totDC, "04_Output/stream_sampledC.csv")

#box plots#####
names(totDC)
POC<-totDC %>% select(Date,ID,Q,depth, POC_mgL)%>% rename(Conc=POC_mgL)%>%mutate(Species= 'POC')
DIC<-totDC %>% select(Date,ID,Q,depth, DIC)%>% rename(Conc=DIC)%>%mutate(Species= 'DIC')
DOC<-totDC %>% select(Date,ID,Q,depth, DOC)%>% rename(Conc=DOC)%>%mutate(Species= 'DOC')

long_C<- rbind(POC, DIC, DOC)

order <- c("5", "5a", "15", "9", '13', '6', '6a', '3', '7')

#ggplot(df, aes(x = factor(site, levels = ordered_sites), y = value)) +
ggplot(long_C, aes(x= factor(ID, levels=order), y=Conc, fill=Species))+
  scale_fill_manual(values=c('DIC'='black', 'DOC'='blue', POC='darkorange'))+
  geom_boxplot()+ ylab("mg/L")+xlab( 'ID')
dev.new()
