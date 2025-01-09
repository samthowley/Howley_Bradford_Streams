#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)
#need to include stream LB too

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
dim_edited<-dim %>%filter(ID %in% c('3','6','9','5'))
#############

longC<-read_csv("04_Output/TDC_long.csv")
longC_edited <- longC %>% mutate(ID=as.character(ID))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%select(-depth, -Q, -pH, -CO2, Temp_pH)

longC_dim<-left_join(longC_edited,dim_edited, by=c('Date', 'ID'))

long_log<-read_csv("01_Raw_data/Long. Log.csv")
long_log_edited <- long_log %>% rename(Date=Visited)%>%mutate(Date=mdy(Date))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")

longC_dim_log<-left_join(longC_dim,long_log_edited, by=c('Date', 'ID', 'Long'))
final <- longC_dim_log %>%
  mutate(ID = as.character(ID),Long = as.character(Long)) %>%
  mutate(Long = case_when(
    ID == '6' & Long == '1' ~ '4',
    ID == '6' & Long == '2' ~ '5',
    ID == '6' & Long == '3' ~ '6',
    TRUE ~ Long)) %>%
  mutate(ID = if_else(ID == '3', '6', ID))%>% mutate(month=month(Date))



ggplot(final, aes(x=Long, color=month))+
  scale_color_gradient(low = "red", high = "blue") +  # Gradient for continuous data
  geom_point(aes(y=DOC)) +
  facet_wrap(~ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")

ggplot(final, aes(x=Long, color= Q))+
  #scale_color_gradient(high='red', low='blue')+
  geom_point(aes(y=DIC, color="DIC"))+
  facet_wrap(~ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")

