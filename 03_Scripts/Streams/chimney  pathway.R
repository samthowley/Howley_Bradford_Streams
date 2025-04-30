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

theme_set(theme(axis.text.x = element_text(size = 17),
                axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 17, angle = 90),
                axis.title.x = element_text(size = 17),
                plot.title = element_text(size = 17),
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

dim<-left_join(Q, depth, by=c('ID', 'Date'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(Q=mean(Q, na.rm=T), depth=mean(depth, na.rm=T))%>%
  distinct(ID, Date, .keep_all = T)

ggplot(dim, aes(x = Q))+
  geom_histogram()+facet_wrap(~ID, scales='free')

#Chimney Pathway#####

resp<-read_csv('04_Output/master_metabolism.csv')%>%
  mutate(NEP=(GPP+ER)*-1)

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
  mutate(active=NEP*0.8)%>%
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
#test<-active%>% filter(ID=='9')

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
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c('brown','darkgreen')) +
  scale_y_log10()+
  ggtitle("Metabolic Regime")+
  ylab(expression(O[2]~'g' / m^2 / 'day'))+
  theme(axis.title.x = element_blank())

mean(active$GPP, na.rm=T)


ggplot(for_histogram %>% filter(!is.na(Q_quartile)),
       aes(x = as.factor(Q_quartile), y = C, fill = type)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_point(data = mean_data,
             aes(x = as.factor(Q_quartile), y = mean_C, group = type),
             position = position_dodge(width = 0.75),
             shape = 21, size = 2, color = "blue", fill = "white") +
  geom_line(data = mean_data,
            aes(x = as.factor(Q_quartile), y = mean_C, group = type),
            position = position_dodge(width = 0.75),
            color = "blue", linewidth = 0.7) +
  scale_fill_manual(values = c('red', 'black')) +
  xlab('IQR of Discharge') +
  ylab(expression('g' / m^2 / 'day')) +
  facet_wrap(~ID, scales = 'free')



active.only<-active%>% select(active, Q, Temp_C,ID, Date) %>% rename(C=active)%>%
  mutate(type="Active")
passive<-active%>% select(passive, Q, Temp_C,ID, Date) %>% rename(C=passive)%>%
  mutate(type="Passive")


ggplot(active, aes(x=Q, y=active.passive))+
  geom_point() +
  ylab(expression('Active/ Passive'))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")+
  xlab(expression(Discharge~m^3/sec))+
  geom_hline(yintercept = 1, color='red', size=1)+
  ggtitle("Ratio of Active to Passive")+
scale_y_log10()+ scale_x_log10()


active%>%
  group_by(ID)%>%
  summarize(act_dom=sum(active.passive >1 , na.rm = TRUE),
            pass_dom=sum(active.passive <1 , na.rm = TRUE),
            tot=sum(active.passive >0 , na.rm = TRUE),
            act_perc=act_dom/tot*100,
            pass_perc=pass_dom/tot*100,
            mean=mean(active.passive, na.rm=T))

ggplot(active, aes(x=Q, y=active.passive))+
  geom_point() +
  ylab(expression('Active/ Passive'))+
  facet_wrap(~ ID, ncol=3, scale='free')+
  theme(legend.position = "bottom")+
  xlab(expression(Discharge~m^3/sec))+
  geom_hline(yintercept = 1, color='red', size=1)+
  ggtitle("Ratio of Active to Passive")+
  scale_y_log10()+ scale_x_log10()+
  stat_poly_line()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ I(log10(x)),  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'blue',
               label.x.npc = "left", label.y.npc = "top")


for_histogram<-rbind(active.only, passive)%>%
  group_by(ID)%>%
  mutate(T_quartile = ntile(Temp_C, 4),
         Q_quartile = ntile(Q, 4))%>% ungroup

mean_active <- for_histogram %>%
  filter(!is.na(Q_quartile)) %>%
  group_by(ID, Q_quartile, type) %>%
  summarise(mean_C = mean(C, na.rm = TRUE), .groups = "drop")

# Plot boxplots and overlay mean points and lines
ggplot(for_histogram %>% filter(!is.na(Q_quartile)),
       aes(x = as.factor(Q_quartile), y = C, fill = type)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_point(data = mean_data,
             aes(x = as.factor(Q_quartile), y = mean_C, group = type),
             position = position_dodge(width = 0.75),
             shape = 21, size = 2, color = "blue", fill = "white") +
  geom_line(data = mean_data,
            aes(x = as.factor(Q_quartile), y = mean_C, group = type),
            position = position_dodge(width = 0.75),
            color = "blue", linewidth = 0.7) +
  scale_fill_manual(values = c('red', 'black')) +
  xlab('IQR of Discharge') +
  ylab(expression('g' / m^2 / 'day')) +
  facet_wrap(~ID, scales = 'free')


ggplot(active, aes(x = Q)) +
  geom_point(aes(y = active, color = "Active Pathway")) +
  geom_point(aes(y = passive, color = "Passive"), shape = 21) +
  geom_smooth(aes(y = active, color = "Active Pathway"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = passive, color = "Passive"), method = "lm", se = FALSE) +
  stat_poly_eq(
    aes(x = log10(Q), y = log10(active), label = paste(..eq.label.., ..p.value.label.., sep = "~~~"), color = "Active Pathway"),
    formula = y ~ x, parse = TRUE, size = 3, label.x.npc = "right", label.y.npc = "bottom"
  ) +
  stat_poly_eq(
    aes(x = log10(Q), y = log10(passive), label = paste(..eq.label.., ..p.value.label.., sep = "~~~"), color = "Passive"),
    formula = y ~ x, parse = TRUE, size = 3, label.x.npc = "right", label.y.npc = -0.85
  ) +
  scale_color_manual(values = c('red', 'black')) +
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  theme(legend.position = "bottom") +
  xlab(expression(Discharge~m^3/sec)) +
  ggtitle(expression(CO[2]~Flux-Q~Relationship))+
  scale_x_log10()+scale_y_log10()


ggplot(slopes, aes(x = ID)) +
  geom_point(aes(y = active_slope, color = "Active Slope"), size=2) +
  geom_point(aes(y = passive_slope, color = "Passive Slope"), size=2) +
  scale_color_manual(values = c('red', 'black')) +
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  theme(legend.position = "bottom") +
  ggtitle("Log-Log Relationships of Active vs Passive")

