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
theme_set(theme(axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 20),
                axis.title.y = element_text(size = 20, angle = 90),
                axis.title.x = element_text(size = 20),
                plot.title = element_text(size = 20),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_blank(),
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
  mutate(Q=mean(Q, na.rm = T)) %>%
  select(Date, ID, Q) %>% filter(Q>1)
Q <- Q[!duplicated(Q[c('Date','ID')]),]

dim<-left_join(depth, Q, by=c('ID', 'Date'))
dim_edited<-dim %>%filter(ID %in% c('6','9','5'))

fa <- read_excel("01_Raw_data/Long. Log.xlsx", sheet = "dims")%>%
  mutate(Site=as.factor(Site), ID=as.factor(ID))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=if_else(is.na(Long), '0', Long))

Q.fa<-left_join(dim_edited, fa, by='ID')%>%
  mutate(Q.prop=Q*Q_fraction)%>%
  select(-Q_fraction, -RASTERVALU)

#Water Carbon Samples############
shimadzu_stream<- read_csv("04_Output/TDC_stream.csv")%>%
  select(-depth, -Q, -pH, -CO2, -Temp_pH, -chapter)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=if_else(is.na(Long), '0', Long))%>%
  filter(ID %in% c('5', '6', '9', '3'))

shimadzu_samples<-read_csv("04_Output/TDC_long.csv")%>%
  mutate(ID=as.character(ID))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  select(-depth, -Q, -pH, -CO2, -Temp_pH, -chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))

shimadzu_samples_all<-rbind(shimadzu_stream,shimadzu_samples)

field_log <- read_excel("01_Raw_data/Long. Log.xlsx",
                       sheet = "Long. Log", col_types = c("numeric",
                                                          "numeric", "date", "skip", "numeric",
                                                          "numeric", "numeric", "numeric",
                                                          "numeric", "skip", "skip"))%>%
  filter(!is.na(Site))%>%
  rename(Date=Visited)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")

water_samples<-left_join(shimadzu_samples_all, field_log)%>%
  mutate(POC=abs(POC))


#include gas samples####
long_gas <- read_csv("04_Output/Picarro_gas.csv")%>%
  filter(chapter=='long')%>%
  separate(ID, into = c("ID", "Long"), sep = "\\.")%>%
  select(-chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))

stream_gas <- read_csv("04_Output/Picarro_gas.csv")%>%
  filter(chapter=='stream')%>%
  separate(ID, into = c("ID", "Long"), sep = "\\.")%>%
  select(-chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))%>%
  filter(ID %in% c('5','6','9','3'))

gas_samples<-rbind(long_gas, stream_gas)

all_samples<-full_join(water_samples, gas_samples)%>%
  mutate(ID_Long = paste(ID, Long, sep = "_"))%>%
  mutate(ID_Long=if_else(ID_Long=='3_0', '3_1', ID_Long),
         ID_Long=if_else(ID_Long=='3_NA', '3_1', ID_Long),
         ID_Long=if_else(ID_Long=='3_3', '3_4', ID_Long),
         ID_Long=if_else(ID_Long=='5_NA', '5_0', ID_Long),
         ID_Long=if_else(ID_Long=='6_NA', '6_0', ID_Long),
         ID_Long=if_else(ID_Long=='9_NA', '9_0', ID_Long))%>%
  filter(!ID_Long %in% c("9_Sam", "5_5", "9_5", "3_3", "6_4", "5_6", "6_0"))%>%
  mutate(ID=if_else(ID_Long=='3_1', '6', ID),
         ID=if_else(ID_Long=='3_2', '6', ID),
         ID=if_else(ID_Long=='3_4', '6', ID))%>%select(-Temp_K)

write_csv(all_samples, "04_Output/master_long.csv")

#Interpolate Discharge####

c.q<-left_join(all_samples,Q.fa)

DIC<-c.q %>%select(ID, Long, ID_Long, DIC, Q.prop, Q, distance)%>% rename(Conc=DIC)%>%
  mutate(C_species='DIC')
DOC<-c.q %>%select(ID, Long, ID_Long, DOC, Q.prop, Q, distance)%>% rename(Conc=DOC)%>%
  mutate(C_species='DOC')
POC<-c.q %>%select(ID, Long, ID_Long, POC, Q.prop, Q, distance)%>% rename(Conc=POC)%>%
  mutate(C_species='POC')
CO2<-c.q %>%select(ID, Long, ID_Long, CO2_sat, Q.prop, Q, distance)%>% rename(Conc=CO2_sat)%>%
  mutate(C_species='CO2')
CH4<-c.q %>%select(ID, Long, ID_Long, CH4_sat, Q.prop, Q, distance)%>% rename(Conc=CH4_sat)%>%
  mutate(C_species='CH4')

c.q.long_df<-rbind(DIC, DOC, POC, CO2, CH4)


a<-ggplot(c.q.long_df%>% filter(C_species %in% c('POC', 'DOC', 'DIC'), ID=='6'), aes(x=Q.prop, y=Conc, color=C_species))+
  geom_point(size=2) +
  ggtitle("Longitudinal: 6")+
  geom_smooth(method=lm, se=F, alpha=0.5)+
  scale_x_log10()+scale_y_log10()+
  theme(legend.position = "bottom")+
  facet_wrap(~ fct_reorder(ID_Long, distance), scales='free')

b<-ggplot(c.q.long_df%>% filter(C_species %in% c('CH4', 'CO2'), ID=='6'), aes(x=Q.prop, y=Conc, color=C_species))+
  geom_point(size=2) +
  geom_smooth(method=lm, se=F, alpha=0.5)+
  scale_x_log10()+scale_y_log10()+
  theme(legend.position = "bottom")+
  facet_wrap(~ fct_reorder(ID_Long, distance), scales='free')

plot_grid(a,b, ncol=1)


#wetland manipulation######
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

