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
library(openxlsx)

CO2mol <- function(CO2) {
  CO2$Temp_C<-fahrenheit.to.celsius(CO2$Temp_PT)
  CO2$Temp_K<-CO2$Temp_C+273.15
  CO2$exp<-2400*((1/CO2$Temp_K)-(1/298.15))
  CO2$KH<-0.034*2.178^(CO2$exp)#mol/L/atm

  CO2$CO2_atm<-CO2$CO2/1000000
  CO2$CO2obs_mol<-CO2$CO2_atm*CO2$KH
  return(CO2)}

#Edit dims######
uca <- data.frame(
  ID = c('5', '6', '9'),
  UCA = c(2e-4, 1e-4, 1e-4))

flow_regime<- read_csv("04_Output/flow_regime_daily.csv")
#GW Correction#####

compiled_baro <- read_csv("01_Raw_data/PT/compiled_baro.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(region, Date)%>%
  mutate(PT=mean(PTbaro, na.rm=T)*68.9476)%>%
  distinct(region, Date, .keep_all=T)%>%
  select(Date, region, PT)

DO <- read_csv("02_Clean_data/DO_cleaned.csv") %>%
  mutate(region=
           case_when(ID=="6"|ID=="6a"|ID=="3"|ID=="7"~ 'N',
                    ID=="5"|ID=="5a"|ID=="15"|ID=="9"|ID=="14"|ID=="13"~ 'S'))

DO_edit<-left_join(DO, compiled_baro)%>%
  arrange(ID, Date)%>%
  mutate(light=calc_light(Date,  29.8, -82.6))%>%
  mutate(time=case_when(
    light>1000~'day',
    light<=1000~'night'),
    Date=as.Date(Date)) %>%
  group_by(ID,Date, time)%>%
  mutate(DO_night=mean(DO, na.rm = T))%>%ungroup()%>%
  group_by(Date, ID)%>%
  mutate(DO=mean(DO, na.rm=T), DO_Saturation=calc_DO_sat(Temp_DO, PT))%>%
  distinct(Date, ID, .keep_all=T)%>%
  select(-light, -time, -PT)


metabolism<-read_csv('04_Output/master_metabolism.csv')%>%
  mutate(NEP=(GPP+ER))%>%
  rename(Date=date)

met_DO<-left_join(metabolism, DO_edit, by=c('Date','ID'))
met_DO<-left_join(met_DO, flow_regime)

units<-met_DO %>%
  mutate(
    Q_m3.day=Q*86.4,
    baseflow_m3.day=bf*86.4,
    u_m.day=u*86400,
    reach_m=0.7*u_m.day/K600,
    width_m=Q_m3.day/(u_m.day*depth),
    area_m2=reach_m*width_m)

DO_GW<-0.67

gw_corrected<-left_join(units,uca)%>%
  mutate(UCA=if_else(is.na(UCA), mean(UCA, na.rm=T), UCA))%>%
  mutate(
    qL=UCA*bf,
    GW_correction=(DO_GW-DO)*(qL/width_m),
    ER_GW_correction=(DO_GW-DO_night)*(qL/width_m)
    )%>%
  mutate(NEP_corrected= NEP-GW_correction,
         ER_corrected= ER-ER_GW_correction)

ggplot(gw_corrected, aes(Date))+
  geom_line(aes(y=NEP))+
  geom_line(aes(y=NEP_corrected),color='red')+
  facet_wrap(~ ID, scales='free')

ggplot(gw_corrected, aes(Date))+
  geom_line(aes(y=width_m))+
  facet_wrap(~ ID, scales='free')

#write_csv(gw_corrected, "04_Output/gw_corrected_metabolism.csv")

#Chimney Pathway#####

KH<-gw_corrected %>%
  mutate(Temp_C=fahrenheit.to.celsius(Temp_DO)) %>%
  mutate(Temp_K=Temp_C+273.15)%>%mutate(
  KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))

KCO2<-KH %>%
  mutate(K600_m.d=K600*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KCO2_d=KCO2_m.d/depth)%>%
  rename(day=Date)

CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")%>% mutate(day=as.Date(Date))

flux<-left_join(CO2,KCO2, by=c('day','ID'))%>%
  group_by(day,ID)%>%
  mutate(
    CO2_day=mean(CO2, na.rm = T))%>%
  ungroup()%>%
  group_by(ID)%>%
  distinct(day,ID, .keep_all = T)%>%
  mutate(
    across(c(CO2_day), ~rollmean(.x, k = 5, fill = NA, align = "center"), .names = "{.col}"))%>%
  ungroup()%>%
  mutate(
    CO2_flux=KCO2_m.d*(CO2_day-400)*KH*(1/10^6)*44*1000)%>%
  mutate(
    across(c(NEP_corrected, CO2_flux, ER_corrected, GPP), ~rollmean(.x, k = 3, fill = NA, align = "center"), .names = "{.col}"))

active<-flux%>%
  mutate(
    active=NEP_corrected*-44/32)%>% #filter(active<CO2_flux)%>%
  mutate(
    passive=CO2_flux-active)%>%
  mutate(
    active.passive=active/passive,
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                         ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                         ID=='9'~'9', ID=='13'~'13'))%>%
  select(Date, ID, GPP, K600, DO, DO_Saturation, depth, Q, NEP_corrected, ER_corrected,
         CO2_flux, CO2, active, passive, active.passive, Basin)%>%
  filter(!ID=='6a', !is.na(ID))#%>%


# ggplot(active %>%filter(ID=='3'), aes(x=Q))+
#   geom_point(aes(y=CO2_flux, color='total'))+
#   geom_point(aes(y=active, color='active'))+
#   facet_wrap(~ID)

write_csv(active, "04_Output/external-internal.csv")

#Moatar et al. 2016 Splitting Q#############
active<-active%>% group_by(ID) %>%
  mutate(Q_med=  case_when(Q>= median(Q, na.rm = T)~ "sup",
                           Q<=median(Q, na.rm = T)~"inf"))%>%
  mutate(Q_ID= paste0(ID, sep="_", Q_med))

#Pull slopes#####
#Split Q
cols <- c('active', 'passive', 'Q', 'ID','DO' ,'Q_ID')
unique_sites <- unique(active$Q_ID[!is.na(active$Q_ID)])

streams <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- active %>%
      filter(Q_ID == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites)


streams_edited <- lapply(streams, function(df) {

  df <- df %>%
    filter(active > 0, Q > 0, DO > 0) %>%
    filter(!is.na(ID), !is.na(active), !is.na(Q), !is.na(DO))


  (active.Q<-summary(lm(log10(active) ~ log10(Q), data = df)))
  Slope.active <- active.Q$coefficients[2,1]
  pvalue_slope.active <- active.Q$coefficients[2, 4]


  (passive.Q<-summary(lm(log10(DO) ~ log10(Q), data = df)))
  Slope.passive <- passive.Q$coefficients[2,1]
  pvalue_slope.passive <- passive.Q$coefficients[2, 4]

  df<-df%>%
    mutate(
      Slope.active=as.numeric(c(Slope.active)),
      Slope.passive=as.numeric(c(Slope.passive)),

      pvalue_slope.active=as.numeric(c(pvalue_slope.active)),
      pvalue_slope.passive=as.numeric(c(pvalue_slope.passive))
    )%>%
    summarize(
      active_slope=mean(Slope.active, na.rm=T),
      DO_slope=mean(Slope.passive, na.rm=T),
      active_pvalue=mean(pvalue_slope.active, na.rm=T),
      DO_pvalue=mean(pvalue_slope.passive, na.rm=T),
      passive_slope=mean(Slope.passive, na.rm=T),
      passive_pvalue=mean(pvalue_slope.passive, na.rm=T),
    )
})

slopes <- bind_rows(streams_edited, .id = "ID")%>%
  separate(ID, into = c("ID", "Q_med"), sep = "_")

write_csv(slopes, "04_Output/external-internal_slopes.csv")

#hydro regime######
#There's a better way to do this. once I figure out the slopes (maybe), I can estimate,
#yearly emissions
Q <- read_csv("02_Clean_data/discharge.csv")%>%
  mutate(hp=case_when(Q>= 2 ~  "wet",
                               Q<2 ~"dry"))

ggplot(Q %>% filter(ID=='6a'), aes(Date, color=hydroperiod))+
  geom_line(aes(y=Q))+scale_y_log10()+
  facet_wrap(~ ID, scales='free')

hydroperiod <- Q %>%
  group_by(ID, hp) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(hydroperiod = case_when(
    hp == "dry" | is.na(hp) ~ "dry",
    hp == "wet" ~ "wet")) %>%
  group_by(ID, hydroperiod) %>%
  summarize(sum = sum(count), .groups = "drop")%>%group_by(ID)%>%
  mutate(total=sum(sum))%>%
    mutate(wet_period=sum/total)%>%
  filter(hydroperiod=='wet', ID != '14')

CO2_summary<-active%>%group_by(ID)%>%
  summarise(CO2_emissions=mean(CO2_flux, na.rm=T))

yrly_CO2_emissions<-left_join(CO2_summary, hydroperiod)%>%
  mutate(yr_CO2_ems=(365*wet_period*CO2_emissions))%>%
  mutate(
    Basin_Name=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                  ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                  ID=='9'~'9', ID=='13'~'13'))


wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")

yrly_CO2_emissions<-left_join(wetland_cover, yrly_CO2_emissions)

ggplot(yrly_CO2_emissions, aes(PERCENTAGE))+
  geom_point(aes(y=yr_CO2_ems))


