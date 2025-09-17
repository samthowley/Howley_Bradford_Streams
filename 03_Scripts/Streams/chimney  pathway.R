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

flow.regime <- read_csv("04_Output/flow_regime_daily.csv")%>%select(-K600)

met_DO.flow<-left_join(met_DO, flow.regime, by=c('Date','ID'))

units<-met_DO.flow %>%
  mutate(
    Q_m3.day=Q_m3.s*86400,
    u_m.day=u*86400,
    )

DO_GW<-0.67

gw_corrected<-units%>%
  mutate(
    GW_correction=(DO_GW-DO)*(qL_m2.sec/width)*86400,
    ER_GW_correction=(DO_GW-DO_night)*(qL_m2.sec/width)*86400
    )%>%
  mutate(NEP_corrected= NEP-GW_correction,
         ER_corrected= ER-ER_GW_correction)

ggplot(gw_corrected, aes(Date))+
  #geom_line(aes(y=NEP))+
  geom_line(aes(y=NEP_corrected),color='red')+
  facet_wrap(~ ID, scales='free')

#write_csv(gw_corrected, "04_Output/gw_corrected_metabolism.csv")

#Chimney Pathway#####

KH<-gw_corrected %>%
  mutate(Temp_C=fahrenheit.to.celsius(Temp_DO),
         Temp_K=Temp_C+273.15,
         KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))

KCO2<-KH %>%
  mutate(
    K600_m.d=K600*depth,
    SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
    KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3)),
    KCO2_d=KCO2_m.d/depth
    )%>%
  rename(day=Date)

CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")%>% mutate(day=as.Date(Date))

flux<-left_join(CO2,KCO2, by=c('day','ID'))%>%
  group_by(day,ID)%>%
  mutate(
    CO2_day=mean(CO2, na.rm = T))%>%
  ungroup()%>%
  mutate(
    CO2_flux=KCO2_m.d*(CO2_day-400)*KH*(1/10^6)*44*1000)%>%
  distinct(day,ID, .keep_all = T)

pathways<-flux%>%
  mutate(
    internal=NEP_corrected*-44/32, #made negative so ER is + and GPP is -
    external=CO2_flux-internal,
    int.ext.ratio=internal/external,
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                         ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                         ID=='9'~'9', ID=='13'~'13'))%>%
  select(Date, ID, GPP, K600, DO, DO_Saturation, depth, Q, NEP_corrected, ER_corrected,
         CO2_flux, CO2, internal, external, int.ext.ratio, Basin)%>%
  filter(!ID=='6a', !is.na(ID))#%>%


ggplot(pathways, aes(x=Q))+
  geom_point(aes(y=CO2_flux, color='total'))+
  geom_point(aes(y=internal, color='internal'))+
  facet_wrap(~ID, scales='free')

write_csv(pathways, "04_Output/external-internal.csv")

#Moatar et al. 2016 Splitting Q#############
pathways<-pathways%>% group_by(ID) %>%
  mutate(Q_med=  case_when(Q>= median(Q, na.rm = T)~ "sup",
                           Q<=median(Q, na.rm = T)~"inf"))%>%
  mutate(Q_ID= paste0(ID, sep="_", Q_med))%>%
  filter(!is.na(Q))

#Pull slopes#####
cols <- c('internal', 'external', 'Q', 'ID','DO' ,'Q_ID')
unique_sites <- unique(pathways$Q_ID[!is.na(pathways$Q_ID)])

streams <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- pathways %>%
      filter(Q_ID == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites)

streams_edited <- lapply(streams, function(df) {

  df <- df %>%
    filter(internal > 0, Q > 0, DO > 0) %>%
    filter(!is.na(ID), !is.na(internal), !is.na(Q), !is.na(DO))


  (internal.Q<-summary(lm(log10(internal) ~ log10(Q), data = df)))
  Slope.internal <- internal.Q$coefficients[2,1]
  pvalue_slope.internal <- internal.Q$coefficients[2, 4]


  (external.Q<-summary(lm(log10(DO) ~ log10(Q), data = df)))
  Slope.external <- external.Q$coefficients[2,1]
  pvalue_slope.external <- external.Q$coefficients[2, 4]

  df<-df%>%
    mutate(
      Slope.internal=as.numeric(c(Slope.internal)),
      Slope.external=as.numeric(c(Slope.external)),

      pvalue_slope.internal=as.numeric(c(pvalue_slope.internal)),
      pvalue_slope.external=as.numeric(c(pvalue_slope.external))
    )%>%
    summarize(
      internal_slope=mean(Slope.internal, na.rm=T),
      DO_slope=mean(Slope.external, na.rm=T),
      internal_pvalue=mean(pvalue_slope.internal, na.rm=T),
      DO_pvalue=mean(pvalue_slope.external, na.rm=T),
      external_slope=mean(Slope.external, na.rm=T),
      external_pvalue=mean(pvalue_slope.external, na.rm=T),
    )
})

slopes <- bind_rows(streams_edited, .id = "ID")%>%
  separate(ID, into = c("ID", "Q_med"), sep = "_")

write_csv(slopes, "04_Output/external-internal_slopes.csv")

