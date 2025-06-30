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
library(weathermetrics)
library(ggpmisc)
library(streamMetabolizer)

theme_set(theme(axis.text.x = element_text(size = 17),
                axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 21, angle = 90),
                axis.title.x = element_text(size = 21),
                plot.title = element_text(size = 21),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 12),
                legend.title =element_blank(),
                legend.position ="bottom",
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

flow_regime<-left_join(Q, depth, by=c('ID', 'Date'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(Q=mean(Q, na.rm=T), depth=mean(depth, na.rm=T))%>%
  distinct(ID, Date, .keep_all = T)%>%
  filter(Q>0.5)

ggplot(dim, aes(x = Q))+
  geom_histogram()+facet_wrap(~ID, scales='free')

#GW Correction#####
stream_dims <- read_excel("01_Raw_data/stream dims.xlsx")%>%
  mutate(UCA=if_else(is.na(UCA), mean(UCA, na.rm=T), UCA))%>%
  select(-width_ft)

baseflow <- read_csv("04_Output/baseflow.csv")%>%
  mutate(bf_m3.s=baseflow/1000)%>%
  select(-Q, -quickflow)

qL<-left_join(baseflow, stream_dims)%>%mutate(qL=baseflow*UCA)

DO <- read_csv("02_Clean_data/DO_cleaned.csv")%>%arrange(ID, Date)%>%
  mutate(light=calc_light(Date,  29.8, -82.6))%>%
  mutate(time=case_when(
    light>1000~'day',
    light<=1000~'night'),
    Date=as.Date(Date)) %>%
  group_by(ID,Date, time)%>%
  mutate(DO_night=mean(DO, na.rm = T))%>%ungroup()%>%
  group_by(Date, ID)%>%
  mutate(DO=mean(DO, na.rm=T))%>%
  distinct(Date, ID, .keep_all=T)%>%
  select(-light, -time)

metabolism<-read_csv('04_Output/master_metabolism.csv')%>%
  mutate(NEP=(GPP+ER))

met_DO<-left_join(metabolism, DO, by=c('Date','ID'))
met_DO<-left_join(met_DO, qL)%>%
  filter(ID %in% c('5','6','9'))

DO_GW<-0.5

corrected<-met_DO%>%
  mutate(NEP_GW_correction=
           (DO_GW-DO)*(qL/width_m)*86400,
         ER_GW_correction=(DO_GW-DO_night)*(qL/width_m)**86400)%>%
  mutate(NEP_corrected= NEP-NEP_GW_correction,
         ER_corrected= ER-ER_GW_correction,)


ggplot(corrected, aes(Date))+
  geom_line(aes(y=ER))+
  geom_line(aes(y=ER_corrected),color='red')+
  facet_wrap(~ ID, scales='free')

write_csv(corrected, "test.csv")
#Chimney Pathway#####

resp<-read_csv('04_Output/master_metabolism.csv')

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
  mutate(CO2_flux=KCO2_m.d*(CO2_day-400)*KH*(1/10^6)*44*1000)%>%
  mutate(across(c(NEP, CO2_flux, ER, GPP), ~rollmean(.x, k = 3, fill = NA, align = "center"), .names = "{.col}"))


ggplot(flux %>% filter(ID=='5'), aes(x=Date, y=CO2_flux))+
  geom_line()


active<-flux%>%
  mutate(active=NEP*44/32)%>% #mols of O2 to mols of CO2
  mutate(active.tot= active/CO2_flux,
        passive=CO2_flux-active)%>%
  mutate(active.passive=active/passive,
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                         ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                         ID=='9'~'9', ID=='13'~'13'))%>%
  select(-Qbase, -Qsurficial, -Temp_PT, -Temp_K, -KH, -K600_m.d, -SchmidtCO2hi, -KCO2_m.d,
         -KCO2_d)%>%
  filter(active.tot<1, active.passive<20)%>%
  filter(!ID=='6a')

active <- active[complete.cases(active[ , c('CO2_flux')]), ]
#################
#Pull slopes#####
################

cols <- c('active', 'passive', 'Q', 'ID')
unique_sites <- unique(active$ID[!is.na(active$ID)])

streams <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- active %>%
      filter(ID == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

streams_edited <- lapply(streams, function(df) {
  (active.Q<-lm(log10(active) ~ log10(Q), data = df))
  cf <- coef(active.Q)
  (Slope.active <- cf[2])
  (Inter.active <- cf[1])


  (passive.Q<-lm(log10(passive) ~ log10(Q), data = df))
  cf <- coef(passive.Q)
  (Slope.passive <- cf[2])
  (Inter.passive <- cf[1])

  df<-df%>%
    mutate(
      active_slope=as.numeric(c(Slope.active)),
      passive_slope=as.numeric(c(Slope.passive)),
      activeInter=as.numeric(c(Inter.active)),
      passiveInter=as.numeric(c(Inter.passive))
    )%>%
    summarize(
      active_slope=mean(active_slope, na.rm=T),
      passive_slope=mean(passive_slope, na.rm=T),
      activeInter=mean(activeInter, na.rm=T),
      passiveInter=mean(passiveInter, na.rm=T),
    )
})


slopes <- bind_rows(streams_edited, .id = "ID")


#########################
###Figures#############
######################

active$ID <- factor(active$ID , levels=c('15','5','5a','3','6','13','7','9','6a'))


met_hist.GPP<-active%>%select(Date, ID, GPP)%>% rename(met=GPP)%>%mutate(type='GPP')
met_hist.ER<-active%>%select(Date, ID, ER)%>% rename(met=ER)%>%mutate(type='ER', met=met*-1)
met_hist<-rbind(met_hist.GPP, met_hist.ER)

ggplot(met_hist, aes(x = as.factor(ID), y = met, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c('brown','darkgreen')) +
  ggtitle("Metabolic Regime")+
  ylab(expression(O[2]~'g' / m^2 / 'day'))+
  theme(axis.title.x = element_blank(),
        axis.title.y= element_text(size=21),
        plot.title = element_text(size = 21))

mean(resp$GPP, na.rm=T)


active.only<-active%>% select(active, Q, Temp_C,ID, Date) %>% rename(C=active)%>%
  mutate(type="Active Pathway")
passive.only<-active%>% select(passive, Q, Temp_C,ID, Date) %>% rename(C=passive)%>%
  mutate(type="Passive Pathway")

active.hist<-rbind(active.only, passive.only)

active$ID <- factor(active$ID , levels=c('9','15','3','7','5','5a','6','13','6a'))

ggplot(active, aes(x = as.factor(ID), y = active.passive)) +
  geom_violin(size=1) +
  scale_y_log10()+
  ggtitle("Active:Passive Sources Among Sites")+
  geom_hline(yintercept = 1, color='red', size=1)+
  ylab("Active/Passive")+
  theme(axis.title.x = element_blank(),
        axis.title.y= element_text(size=21),
        plot.title = element_text(size = 21))


ggplot(active, aes(x=Q, y=active.passive))+
  geom_point() +
  ylab(expression('Active/ Passive'))+
  facet_wrap(~ ID, ncol=3)+
  theme(legend.position = "bottom")+
  xlab(expression(Discharge~L/sec))+
  geom_hline(yintercept = 1, color='red', size=1)+
  ggtitle("Variation Among Active:Passive Dominance")+
scale_y_log10()+ scale_x_log10()

mean(active$active.passive, na.rm = T)


active%>%
  group_by(ID)%>%
  summarize(act_dom=sum(active.passive >1 , na.rm = TRUE),
            pass_dom=sum(active.passive <1 , na.rm = TRUE),
            tot=sum(active.passive >0 , na.rm = TRUE),
            act_perc_days=act_dom/tot*100,
            pass_perc_days=pass_dom/tot*100,
            mean=mean(active.passive, na.rm=T),
            act_perc=mean(active/CO2_flux, na.rm=T))


active$ID <- factor(active$ID , levels=c('9','15','3','5a','13','7','5','6','6a'))

ggplot(active, aes(x=Q, y=active.passive))+
  geom_point() +
  ylab(expression('Active/ Passive'))+
  facet_wrap(~ ID, ncol=3)+
  theme(legend.position = "bottom")+
  xlab(expression(Discharge~L/sec))+
  geom_hline(yintercept = 1, color='red', size=1)+
  ggtitle("Variation Among Active:Passive Dominance")+
  scale_y_log10()+ scale_x_log10()+
  stat_poly_line()+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ I(log10(x)),  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "bottom")+
  theme(axis.text.x = element_text(size=15))




ggplot(active, aes(x = Q)) +
  geom_point(aes(y = active, color ="Active Pathway")) +
  geom_point(aes(y = passive, color ="Passive Pathway"), shape = 21) +
  geom_smooth(aes(y = active, color ="Active Pathway"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = passive, color ="Passive Pathway"), method = "lm", se = FALSE) +
  stat_poly_eq(
    aes(x = log10(Q), y = log10(active), label = paste(..p.value.label.., sep = "~~~"), color = "Active Pathway"),
    formula = y ~ x, parse = TRUE, size = 4, label.x.npc = "right", label.y.npc = 0.017
  ) +
  stat_poly_eq(
    aes(x = log10(Q), y = log10(passive), label = paste(..p.value.label.., sep = "~~~"), color = "Passive Pathway"),
    formula = y ~ x, parse = TRUE, size = 4, label.x.npc = "right", label.y.npc = 0.1
  ) +
  scale_color_manual(values = c('red', 'black')) +
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  theme(legend.position = "bottom") +
  xlab(expression(Discharge~L/sec)) +
  ggtitle(expression(CO[2]~Flux-Q~Relationship))+
  scale_x_log10()+scale_y_log10()


ggplot(slopes, aes(x = ID)) +
  geom_point(aes(y = active_slope, color = "Active Slope"), size=4) +
  geom_point(aes(y = passive_slope, color = "Passive Slope"), size=4) +
  scale_color_manual(values = c('red', 'black')) +
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  theme(legend.position = "bottom") +
  ggtitle("Log-Log Relationships of Active vs Passive")

