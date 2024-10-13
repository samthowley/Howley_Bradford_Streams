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

depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')
length<-read_csv('02_Clean_data/stream area.csv')

#Edit dims######
depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth, Temp_PT)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(Q, depth, by=c('ID', 'Date'))

#sample C##########
POC<-read_xlsx('01_Raw_data/POC.xlsx')
POC<-POC %>% rename('Date'='Sampled', 'POC_mgL'='mg/L')%>%
  mutate(POC_mmol=POC_mgL/44.01) %>% select(ID,Date,POC_mgL,POC_mmol) %>%filter(POC_mgL<200)
POC<-POC[POC$ID %in% c("6","5a","7","9","5","6a","13","15","3"), ]

DC_strm<-read_csv('04_Output/TDC_stream.csv')
DIC_strm<-DC_strm %>% filter(Species=='DIC') %>%
  rename("DIC"="Conc.","DIC_mmol"='mmol')%>%
  select(ID, Date, DIC, DIC_mmol)
DOC_strm<-DC_strm %>% filter(Species=='DOC') %>%
  rename("DOC"="Conc.","DOC_mmol"='mmol') %>%
  select(ID, Date, DOC, DOC_mmol)
DC<-left_join(DOC_strm, DIC_strm, by=c("ID","Date"))

totDC<-left_join(DC,POC, by=c("ID","Date"))
totDC<-totDC %>% select(ID,Date,POC_mgL,DIC,DOC,POC_mmol,DIC_mmol,DOC_mmol)
totDC <- totDC[rev(order(as.Date(totDC$Date, format="%m/%d/%Y"))),]

totDC <- totDC[!duplicated(totDC[c('ID','Date')]),]
totDC<-left_join(totDC, dim, by=c('ID', 'Date'))

totDC<-totDC%>%filter(Q>1)
ggplot(totDC, aes(x=Q))+
  geom_point(aes(y=DOC, color="DOC"),size=1.5)+
  geom_point(aes(y=DIC, color= "DIC"), size=1.5)+
  geom_point(aes(y=POC_mgL, color="POC"), size=1.5)+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  scale_x_log10()+scale_y_log10()+
  xlab(expression('Discharge'~m^3/s))+ylab('mg/L')+
  facet_wrap(~ ID, ncol=3, scales='free')

site<-totDC %>% filter(ID=='5')
ggtern(data=site,aes(DOC,DIC,POC_mgL, colour = Q))+scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC_mgL",y="DIC_mgL",z="POC_mgL")+facet_wrap(~ ID, ncol=3, scales='free')

write_csv(totDC, "04_Output/stream_sampledC.csv")

#box plots#####

POC<-totDC %>% select(Date,ID,Q,depth, POC_mmol)%>% rename(Conc=POC_mmol)%>%mutate(Species= 'POC')
DIC<-totDC %>% select(Date,ID,Q,depth, DIC_mmol)%>% rename(Conc=DIC_mmol)%>%mutate(Species= 'DIC')
DOC<-totDC %>% select(Date,ID,Q,depth, DOC_mmol)%>% rename(Conc=DOC_mmol)%>%mutate(Species= 'DOC')

long_C<- rbind(POC, DIC, DOC)

order <- c("5", "5a", "15", "9", '13', '6', '6a', '3', '7')

#ggplot(df, aes(x = factor(site, levels = ordered_sites), y = value)) +
ggplot(long_C, aes(x= factor(ID, levels=order), y=Conc, fill=Species))+
  scale_fill_manual(values=c('DIC'='black', 'DOC'='blue', POC='darkorange'))+
  geom_boxplot()+ ylab("mg/L")+xlab( 'ID')

