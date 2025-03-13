#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)
library(ggtern)
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
water_samples <- longC_dim_log %>%
  mutate(ID = as.character(ID),Long = as.character(Long)) %>%
  mutate(Long = case_when(
    ID == '6' & Long == '1' ~ '4',
    ID == '6' & Long == '2' ~ '5',
    ID == '6' & Long == '3' ~ '6',
    TRUE ~ Long)) %>%
  mutate(ID = if_else(ID == '3', '6', ID))%>% mutate(month=month(Date))%>%
  mutate(POC=abs(POC))


#include gas samples####
Picarro_gas <- read_csv("04_Output/Picarro_gas.csv")%>% filter(chapter=='long')%>%
  separate(ID, into = c("ID", "Long"), sep = "\\.")%>%select(-chapter)

all_samples<-full_join(water_samples, Picarro_gas)

#Further manipulation######
final<-all_samples %>%mutate(Wetland_density=case_when(ID==5~ "low",
                                        ID==6~ "high",
                                        ID==9~ "moderate"))

streamorder <- read_csv("04_Output/streamorder.csv")%>%
  mutate(Site = if_else(Site == "5", "5.5", Site),
         Site = if_else(Site == "6", "6.2", Site),
         Site = if_else(Site == "9", "9.5", Site))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")


final<-left_join(final,streamorder, c=by('ID', 'Long'))

test<-final%>% filter(ID==5)





ggplot(all_samples%>%filter(ID!='3'), aes(x=Long))+
  geom_point(aes(y=DOC, color='DOC'), size=2) +
  geom_point(aes(y=DIC, color='DIC'), size=2) +
  geom_point(aes(y=POC, color='POC'), size=2) +
  facet_wrap(~ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")

ggplot(final, aes(x=Long, color=Wetland_density))+
  geom_point(aes(y=DOC), size=2) +
  theme(legend.position = "bottom")


ggtern(data=final %>%filter(ID !='3'),aes(DOC,DIC*10,POC*10, color=ID))+
  #scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC mg/L",y="DIC deci-mg/L",z="POC deci-mg/L")+
  theme_minimal_grid()+theme(legend.position = "bottom",
                             axis.title =element_text(size = 9, angle=0))+
  labs(color='Longitudinal Sampling')

ggtern(data=final %>%filter(ID !='3'),aes(DOC,DIC*10,POC*10, color=Q))+
  #scale_color_gradient(low = "blue", high = "red") +
  geom_point(size=2) +labs(x="DOC mg/L",y="DIC deci-mg/L",z="POC deci-mg/L")+
  theme_minimal_grid()+theme(legend.position = "bottom",
                             axis.title =element_text(size = 9, angle=0))+
  labs(color='Longitudinal Sampling')

