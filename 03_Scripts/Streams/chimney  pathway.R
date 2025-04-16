#packages#####
rm(list=ls())

library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)
library(zoo)
library(plotly)
library(broom)

theme_set(theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
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

CO2mol <- function(CO2) {
  CO2$Temp_C<-fahrenheit.to.celsius(CO2$Temp_PT)
  CO2$Temp_K<-CO2$Temp_C+273.15
  CO2$exp<-2400*((1/CO2$Temp_K)-(1/298.15))
  CO2$KH<-0.034*2.178^(CO2$exp)#mol/L/atm

  CO2$CO2_atm<-CO2$CO2/1000000
  CO2$CO2obs_mol<-CO2$CO2_atm*CO2$KH
  return(CO2)}

#Edit dims######

depth<-read_csv('02_Clean_data/depth.csv')
Q<-read_csv('02_Clean_data/discharge.csv')
length<-read_csv('02_Clean_data/stream area.csv')

depth<-depth %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>% mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth, Temp_PT)
depth <- depth[!duplicated(depth[c( 'Date','ID')]),]

Q<-Q %>% mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),Qbase=mean(Qbase, na.rm = T),Qsurficial=mean(Qsurficial, na.rm = T)) %>%
  select(Date, ID, Q,Qbase,Qsurficial)%>%
  distinct(Date, ID, .keep_all = T)

dim<-left_join(Q, depth, by=c('ID', 'Date'))

ggplot(dim, aes(x = Q))+
  geom_histogram()+facet_wrap(~ID, scales='free')

#Chimney Pathway#####

resp<-read_csv('04_Output/master_metabolism.csv')%>%
  filter(ER< -3 & ER>-20, GPP>0) %>%
  mutate(NEP=GPP+ER)%>%
  filter(NEP<0)

resp<-left_join(resp,dim, by=c('Date','ID'))

KH<-resp %>%filter(depth>0)%>%
  mutate(Temp_C=fahrenheit.to.celsius(Temp_PT)) %>%
  mutate(Temp_K=Temp_C+273.15)%>%mutate(
  KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))

KCO2<-KH %>%
  mutate(K600_m.d=K600_daily_mean*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KCO2_d=KCO2_m.d/depth)%>%
  rename(day=Date)

CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")%>% mutate(day=as.Date(Date))

flux<-left_join(CO2,KCO2, by=c('day','ID'))%>%
  group_by(day,ID)%>%
  mutate(CO2_day=mean(CO2, na.rm = T))%>%
  ungroup()%>%group_by(ID)%>%
  distinct(day,ID, .keep_all = T)%>%
  mutate(across(c(CO2_day), ~rollmean(.x, k = 5, fill = NA, align = "center"), .names = "{.col}"))%>%
  ungroup()%>%select(-CO2)%>%
  mutate(CO2_flux=KCO2_m.d*(CO2_day-400)*KH*(1/10^6)*44*1000)

ggplot(flux %>% filter(ID=='5'), aes(x=Q, y=CO2_flux))+
  geom_point()


active<-flux%>%
  mutate(active=NEP*0.8)%>%
  mutate(active.tot= active/CO2_flux,
        passive=CO2_flux-active)%>%
  mutate(active.passive=active/passive,
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                         ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                         ID=='9'~'9', ID=='13'~'13'))%>%
  select(-Qbase, -Qsurficial, -Temp_PT, -Temp_K, -KH, -K600_m.d, -SchmidtCO2hi, -KCO2_m.d,
         -KCO2_d)%>%
  filter(active.tot<1)%>%
  filter(!ID=='6a')

active <- active[complete.cases(active[ , c('CO2_flux')]), ]

#########################
###Figures#############
######################

active$ID <- factor(active$ID , levels=c('5','5a','13','7','3','6','6a','9','15'))

active.only<-active%>% select(active, Q, Temp_C,ID, Date) %>% rename(C=active)%>%
  mutate(type="reactor")
passive<-active%>% select(passive, Q, Temp_C,ID, Date) %>% rename(C=passive)%>%
  mutate(type="passive")

ggplot(CO2, aes(x=Q, y=CO2))+
  geom_point() +
  ylab(expression(CO[2]~~'ppm'))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")+
  xlab(expression(Discharge~m^3/sec))+
  ggtitle(expression(CO[2]~C-Q~Relationship))+
  scale_y_log10()+ scale_x_log10()+
  stat_poly_line()+
stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
             formula = y ~ I(log10(x)),  # If you're plotting log10 on the x-axis only
             parse = TRUE, color = 'darkred',
             label.x.npc = "right", label.y.npc = "bottom")


for_histogram<-rbind(active.only, passive)%>%
  group_by(ID)%>%
  mutate(T_quartile = ntile(Temp_C, 4),
         Q_quartile = ntile(Q, 4))%>% ungroup


ggplot(for_histogram %>% filter(!ID=='6a'), aes(x=Q, y=C, color=type))+
  geom_point() +
  ylab(expression('g'/m^2/'day'))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")+
  xlab(expression(Discharge~m^3/sec))+
  ggtitle('Active-Passive Carbon')+
  scale_y_log10()+ scale_x_log10()+
  stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label.., sep = "~~~~~")),
               formula = y ~ x, parse = TRUE,
               label.x.npc = "left",label.y.npc = "bottom")

names(for_histogram)
library(lme4)
library(nlme)
library(tibble)

rCa <- lmList(log10(C) ~ log10(Q) | ID, data=reactive)
(cf <- coef(rC))
active_slope <- coef(rCa) %>%as.data.frame() %>%
  rownames_to_column("ID") %>%rename(slope="log10(Q)", Intercept="(Intercept)")%>%
  mutate(type="active")

rCp <- lmList(log10(C) ~ log10(Q) | ID, data=passive)
(cf <- coef(rC))
passive_slope <- coef(rCp) %>%as.data.frame() %>%
  rownames_to_column("ID") %>%rename(slope="log10(Q)", Intercept="(Intercept)")%>%
  mutate(type="passive")

slope<-rbind(passive_slope, active_slope)
slope$ID <- factor(slope$ID , levels=c('5','5a','13','7','3','6','6a','9','15'))

ggplot(slope %>% filter(!ID=='6a'), aes(x=ID, y=slope, color=type))+
  geom_point(size=3) +
  ylab(expression('Slope'))+
  geom_hline(yintercept = 0, color='darkred')


label_data <- chimney %>%
  filter(ID != '6a', reactor_tot < 1) %>%
  group_by(ID) %>%
  summarise(
    mean_val = mean(reactor_tot, na.rm = TRUE),
    min_val = min(reactor_tot, na.rm = TRUE),
    max_val = max(reactor_tot, na.rm = TRUE),
    Q_val = min(Q, na.rm = TRUE)) %>%
  mutate(
    label_other = paste0("Min: ", round(min_val, 2), "\nMax: ", round(max_val, 2)),
    label_mean = paste0("Mean: ", round(mean_val, 2))
  )

ggplot(chimney%>%filter(!ID=='6a' & reactor_tot<1), aes(Q, y=reactor_tot))+
  geom_point(size=2, shape=1)+
  ylab(expression('Active'~CO[2]/ 'Total'~CO[2]))+
  xlab(expression('Discharge'~m^3))+
  ggtitle(expression(Proportion~of~Active~CO[2]))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  scale_y_log10()+ scale_x_log10()+
  geom_text(data = label_data,
            aes(x = Q_val, y = max_val, label = label_other),
            inherit.aes = FALSE,
            hjust = 0, vjust = 1.5, size = 3, color = "darkblue") +

  # Red and bold label for mean
  geom_text(data = label_data,
            aes(x = Q_val, y = max_val, label = label_mean),
            inherit.aes = FALSE,
            hjust = 0, vjust = 0.3, size = 4, color = "red", fontface = "bold")+

  stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label.., sep = "~~~~~")),
               formula = y ~ x, parse = TRUE,
               label.x.npc = "left",label.y.npc = "bottom")


chimney<-chimney%>%group_by(ID)%>%
  mutate(T_quartile = ntile(Temp_C, 4),
         Q_quartile = ntile(Q, 4))%>% ungroup

ggplot(chimney%>% filter(!ID=='6a'& reactor_tot<1),
       aes(x=as.factor(Q_quartile), y=reactor_tot)) +
  geom_boxplot()+
  xlab('Discharge IQR')+
  ylab(expression('Active'~CO[2]/ 'Total'~CO[2]))+
  scale_y_log10()+
  facet_wrap(~ID, scales='free')

b<-ggplot(chimney%>%filter(ID %in% c('5','15','3','7','9')), aes(Q, y=reactor_tot))+
  geom_point(size=2, shape=1)+
  ylab(expression('Active'~CO[2]/ 'Total'~CO[2]))+
  xlab(expression('Discharge'~m^3))+
  ggtitle(expression(Proportion~of~Active~CO[2]))+
  facet_wrap(~ ID, ncol=5, scale='free')+
  scale_y_log10()+ scale_x_log10()+
  geom_text(data = label_data%>%filter(ID %in% c('5','15','3','7','9')),
            aes(x = Q_val, y = max_val, label = label_other),
            inherit.aes = FALSE,
            hjust = 0, vjust = 1.5, size = 3, color = "darkblue") +
  geom_text(data = label_data%>% filter(ID %in% c('5','15','3','7','9')),
            aes(x = Q_val, y = max_val, label = label_mean),
            inherit.aes = FALSE,
            hjust = 0, vjust = 0.3, size = 4, color = "red", fontface = "bold")+
  stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label.., sep = "~~~~~")),
               formula = y ~ x, parse = TRUE,
               label.x.npc = "left",label.y.npc = "bottom")

plot_grid(a, b, nrow=2)

#
# ggplot(for_histogram %>% filter(!ID=='6a'),
#        aes(x=as.factor(Q_quartile), y=C, fill=type)) +
#   geom_boxplot()+
#   xlab('IQR Discharge')+ylab(expression(CO[2]~'g'/m^2/'day'))+
#   scale_y_log10()+
#   facet_wrap(~ID, scales='free')
#
# result <- chimney %>%
#   group_by(ID) %>%
#   summarise(active_days = sum(reactor_tot >0.5, na.rm = TRUE),
#             passive_days = sum(passive_tot >0.5, na.rm = TRUE),
#             passive_prop=mean(passive_tot)*100,
#             active_prop=mean(reactor_tot)*100) %>% filter(active_prop<100)
# write_csv(chimney, "04_Output/chimney_reactor.csv")

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>%
  rename("Basin"="Basin_Name", "wetland_cover_perc"="PERCENTAGE", 'Basin_area'='Shape_Area')%>%
  select(Basin, wetland_cover_perc, Basin_area)
wetland_proxim <- read_csv("01_Raw_data/wetland_proxim.csv")%>%
  rename('wetland_dist'='NEAR_DIST', 'Basin'='Site')%>%select(Basin, wetland_dist)

wetland_x<-left_join(wetland_cover, wetland_proxim, by='Basin')

chimney_wetland<-full_join(chimney, wetland_x, by='Basin')%>%group_by(ID)%>%
  mutate(T_quartile = ntile(Temp_C, 4),
         Q_quartile = ntile(Q, 4))%>% ungroup%>%
  mutate(wetland_cover_perc=round(wetland_cover_perc,2))


chimney_wetland$ID <- factor(chimney_wetland$ID , levels=c('5','5a','13','7','3','6','6a','9','15'))

a<-ggplot(chimney_wetland,
       aes(x=as.factor(wetland_cover_perc), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  ylab(expression(Active~CO[2]~~'g'/m^2/'day'))+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))

b<-ggplot(chimney_wetland %>%filter(Q_quartile==4),
       aes(x=as.factor(wetland_cover_perc), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  ylab(expression(Active~CO[2]~~'g'/m^2/'day'))+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('Top 25% of Discharge')
plot_grid(a,b, ncol=2)


wetland_prop <- read_csv("01_Raw_data/wetland proportion buffer.csv")
chimney_wetland_prop<-left_join(chimney, wetland_prop, by='Basin')%>%group_by(ID)%>%
  mutate(T_quartile = ntile(Temp_C, 4),
         Q_quartile = ntile(Q, 4))%>% ungroup%>%
  mutate(proportion=round(proportion,2))

a<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==2000 & Q_quartile==4),
       aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  #ylab(expression(Top~'25%'~Active~CO[2]~~'g'/m^2/'day'))+
  ylab(' ')+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('2000m Buffer')+theme(legend.position = 'none')

b<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==1000& Q_quartile==4),
       aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  ylab(expression(Passive~CO[2]~~'g'/m^2/'day'))+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('1000m Buffer')+theme(legend.position = 'none')

c<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==500& Q_quartile==4),
       aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  #ylab(expression(Top~'25%'~Active~CO[2]~~'g'/m^2/'day'))+
  ylab(' ')+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('500 Buffer')+theme(legend.position = 'none')

d<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==250& Q_quartile==4),
       aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  ylab(' ')+
  #ylab(expression(Top~'25%'~Active~CO[2]~~'g'/m^2/'day'))+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('250 Buffer')+theme(legend.position = 'none')

plot_grid(a,b,c,d, ncol=2)




a<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==2000),
          aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  #ylab(expression(Top~'25%'~Active~CO[2]~~'g'/m^2/'day'))+
  ylab(' ')+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('2000m Buffer')+theme(legend.position = 'none')

b<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==1000),
          aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  ylab(expression(Active~CO[2]~~'g'/m^2/'day'))+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('1000m Buffer')+theme(legend.position = 'none')

c<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==500),
          aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  #ylab(expression(Top~'25%'~Active~CO[2]~~'g'/m^2/'day'))+
  ylab(' ')+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('500 Buffer')+theme(legend.position = 'none')

d<-ggplot(chimney_wetland_prop %>% filter(buffer_radius==250),
          aes(x=as.factor(proportion), y=passive, fill=ID)) +
  geom_boxplot()+
  scale_y_log10()+
  ylab(' ')+
  #ylab(expression(Top~'25%'~Active~CO[2]~~'g'/m^2/'day'))+
  xlab(expression(Wetland~Area~m^2/Basin~Area~m^2))+
  ggtitle('250 Buffer')+theme(legend.position = 'none')

plot_grid(a,b,c,d, ncol=2)
