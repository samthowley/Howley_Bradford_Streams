library(marelac)
library(tidyverse)
library(weathermetrics)
library(streamMetabolizer)
library(ggtern)

R<-0.08205
V_wp<-0.07
V_hp<-0.025
conversion_bar2atm<-0.986923
conversion_PSI2atm<-14.696

#Compile pressure and temperature data####
compiled_PT <- read_csv("01_Raw_data/PT/compiled_PT.csv")
baro <- read_csv("01_Raw_data/PT/compiled_baro.csv")

P<-left_join(compiled_PT, baro, by=c('Date', 'region'))

P<-P%>%arrange(ID, Date)%>%
  rename('Water_PSI'="PT", 'air_PSI'='PTbaro')%>%
  fill(air_PSI, .direction='downup')%>%
  mutate(P=Water_PSI/conversion_PSI2atm,
         P_air=air_PSI/conversion_PSI2atm,
         Temp_PT= fahrenheit.to.celsius(Temp_PT))%>%
  select(-Water_PSI, -air_PSI, -region)

#marelec give k in mmol/L*bar
k<-P%>%mutate(k_CO2=(gas_solubility(S = 0, t = Temp_PT, species = "CO2"))/(conversion_bar2atm*10^6),
              k_CH4=(gas_solubility(S = 0, t = Temp_PT, species = "CH4"))/(conversion_bar2atm*10^6),
              k_N2O=(gas_solubility(S = 0, t = Temp_PT, species = "N2O"))/(conversion_bar2atm*10^6))

light<-k%>%mutate(light= calc_light(Date,29.914156, -82.256152))%>%
  mutate(Date=as.Date(Date)) %>% filter(light> 700) %>%
  group_by(ID, Date)%>%
  mutate(k_CO2=mean(k_CO2, na.rm=T), k_CH4=mean(k_CH4, na.rm=T),k_N2O=mean(k_N2O, na.rm=T),
         P=mean(P))%>%
  distinct(Date, ID, .keep_all = T)

#Call in gas samples#####
file.names <- list.files(path="01_Raw_data/Picarro", pattern=".csv", full.names=TRUE)
gas_samples<-data.frame()
for(i in file.names){
  runs <- read_csv(i)
  runs<-runs%>%select(SampleName, N2O_dry_mean, CO2_dry_mean, CH4_dry_mean)
  gas_samples<-rbind(runs, gas_samples)}

format<-gas_samples%>%
 rename(N2O_x=N2O_dry_mean, CO2_x=CO2_dry_mean, CH4_x=CH4_dry_mean)%>%
  separate(SampleName, into = c("Site", "Date","Rep"), sep = "_")%>%
  mutate(Date=mdy(Date))%>%
  mutate(across(3:6,as.numeric))

remove_erroreanous_runs<-format %>%
  group_by(Site, Rep, Date) %>%
  slice_max(order_by = CO2_x, with_ties = FALSE) %>%
  ungroup()

#parse out air samples
air<-remove_erroreanous_runs%>% filter(CO2_x>300 & CO2_x<700)%>%
  rename(N2O_air_ppm=N2O_x, CH4_air_ppm= CH4_x, CO2_air_ppm=CO2_x)%>%
  select(-Rep)

samples<- remove_erroreanous_runs %>% filter(CO2_x>800)%>%arrange(Site, Date)

samples_air<-left_join(samples, air, by=c('Site', 'Date'))

#include air conc
samples_air <- samples_air%>% group_by(Site) %>%
  fill(N2O_air_ppm, CO2_air_ppm, CH4_air_ppm, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    N2O_air_ppm = if_else(is.na(N2O_air_ppm), mean(N2O_air_ppm, na.rm = TRUE), N2O_air_ppm),
    CO2_air_ppm = if_else(is.na(CO2_air_ppm), mean(CO2_air_ppm, na.rm = TRUE), CO2_air_ppm),
    CH4_air_ppm = if_else(is.na(CH4_air_ppm), mean(CH4_air_ppm, na.rm = TRUE), CH4_air_ppm)) %>%
  ungroup()

#divide gas samples by chapter######
samples_air<-samples_air %>% mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                        Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                        Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',

                                        Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                        Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                        Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                        Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                        Site=='9GW4'~'9',Site=='9GW5'~'9',Site=='5GW8'~'5',

                                        Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
                                        Site=='5.4'~'5',Site=='5.5'~'5',Site=='6.1'~'6',Site=='6.2'~'6',
                                        Site=='6.3'~'6',Site=='6.4'~'6',Site=='6.5'~'6',Site=='6.6'~'6',
                                        Site=='9.1'~'9',Site=='9.2'~'9',Site=='9.3'~'9',Site=='9.4'~'9',
                                        Site=='9.5'~'9',Site=='9.6'~'9',Site=='9.Sam'~'9'))%>%
  distinct(Site,Date,Rep, .keep_all = T)


samples_air<-samples_air %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                            Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream',

                                            Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                            Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='6GW1'~'RC',
                                            Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                            Site=='6GW6'~'RC',Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                            Site=='9GW4'~'RC',Site=='9GW5'~'RC',Site=='5GW8'~'RC',

                                            Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                            Site=='5.4'~'long',Site=='5.5'~'long',Site=='5.6'~'long',

                                            Site=='6.1'~'long',Site=='6.2'~'long',
                                            Site=='6.3'~'long',Site=='3.1'~'long',Site=='3.2'~'long',Site=='3.3'~'long',
                                            Site=='3.4'~'long',

                                            Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                            Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

raw_data<-left_join(samples_air, light, by=c('Date','ID'))


CO2_conversion<-raw_data%>%
  mutate(Temp_K=Temp_PT+273.15)%>%
  mutate(N2O_air_uatm=N2O_air_ppm*P_air,
         CO2_air_uatm=CO2_air_ppm*P_air,
         CH4_air_uatm=CH4_air_ppm*P_air)%>%
  mutate(N2O_hs_umol=(N2O_air_uatm*V_hp)/(R*Temp_K),
         CO2_hs_umol=(CO2_air_uatm*V_hp)/(R*Temp_K),
         CH4_hs_umol=(CH4_air_uatm*V_hp)/(R*Temp_K))%>%
  mutate(N2O_x_uatm=N2O_x*P,
         CO2_x_uatm=CO2_x*P,
         CH4_x_uatm=CH4_x*P)

meas<-CO2_conversion%>%
  mutate(
    numerator_CO2=(CO2_x_uatm*k_CO2*V_wp)+(CO2_x_uatm*V_hp),
    numerator_CH4=(CH4_x_uatm*k_CH4*V_wp)+(CH4_x_uatm*V_hp),
    numerator_N2O=(N2O_x_uatm*k_N2O*V_wp)+(N2O_x_uatm*V_hp)
    )%>%
  mutate(
    PVnRT_CO2=numerator_CO2/(R*Temp_K),
    PVnRT_CH4=numerator_CH4/(R*Temp_K),
    PVnRT_N2O=numerator_N2O/(R*Temp_K)
    )%>%
  mutate(
    CO2_umol_L=(PVnRT_CO2-CO2_hs_umol)/V_wp,
    CH4_umol_L=(PVnRT_CH4-CH4_hs_umol)/V_wp,
    N2O_umol_L=(PVnRT_N2O-N2O_hs_umol)/V_wp
    )


mol_to_ppm<-meas %>%
  mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%
  mutate(KH=0.034*2.178^(exp))%>%
  mutate(CO2air_umol_L=CO2_air_uatm*KH,
         CH4air_umol_L=CH4_air_uatm*KH,
         N2O_air_umol_L=N2O_air_uatm*KH)%>%
  mutate(CO2_sat=CO2_umol_L/CO2air_umol_L,
         CH4_sat=CH4_umol_L/CH4air_umol_L,
         N2O_sat=N2O_umol_L/N2O_air_umol_L)%>%
  select(Site, Date, chapter, Temp_K, CO2_umol_L,CH4_umol_L,N2O_umol_L,
         CO2_sat, CH4_sat, N2O_sat)%>%
  rename('ID'='Site')


write_csv(mol_to_ppm, "04_Output/Picarro_gas.csv")

meas <- read_csv("04_Output/Picarro_gas.csv")

stream<-meas %>%filter(chapter=='stream')
RC<-filter(meas, chapter=='RC')
long<-filter(meas, chapter=='long')

#Conversion to flux########
stream <- read_csv("04_Output/Picarro_gas.csv")%>%filter(chapter=='stream')

depth <- read_csv("02_Clean_data/depth.csv")
depth_day<-depth %>%mutate(Date=as.Date(Date))%>% group_by(ID, Date)%>% mutate(depth=mean(depth, na.rm = T))%>%
  distinct(ID, Date, .keep_all = T)

Q <- read_csv("02_Clean_data/discharge.csv")
Q_day<-Q %>%mutate(Date=as.Date(Date))%>% group_by(ID, Date)%>% mutate(Q=mean(Q, na.rm = T))%>%
  distinct(ID, Date, .keep_all = T)

hydro<-left_join(Q_day, depth_day, by=c('ID', 'Date'))
meas_depth<-left_join(stream, hydro, by=c('ID', 'Date'))

metabolism<-read_csv('04_Output/master_metabolism.csv')
metabolism<-metabolism %>%rename(k600_1.d=K600_daily_mean)

meas.k600<-full_join(meas_depth, metabolism, c('Date', 'ID'))

temp<-meas.k600 %>% mutate(Temp_C = fahrenheit.to.celsius(Temp_PT))

ks<-temp %>%
  mutate(K600_m.d=k600_1.d*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
         SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)))#%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)


flux<-ks%>%
  mutate(CO2_flux=KCO2_m.d*(CO2_umol_L/10^6)*44.01)%>%
  filter(complete.cases(CO2_umol_L, k600_1.d))%>% distinct()

fluxes_sensors <- read_csv("04_Output/fluxes.csv")
fluxes_sensors<-fluxes_sensors %>%
  mutate(Date=as.Date(Date))%>%group_by(Date, ID)%>%
  mutate(CO2_flux_sensors=mean(CO2_flux, na.rm=T))%>%
  select(Date, ID, CO2_flux_sensors) %>%distinct(ID, Date, .keep_all = T)

flux_check<-left_join(flux, fluxes_sensors, by=c('Date', 'ID'))

write_csv(flux_check, "test.csv")


ggplot(flux_check, aes(x=Q))+
  geom_point(aes(y=CO2_flux_sensors, color='sensors'), shape=1)+
  geom_point(aes(y=CO2_flux, color='samples'), shape=1)+scale_x_log10()+
  facet_wrap(~ ID, ncol=3)


#Figures########

depth <- read_csv("02_Clean_data/depth.csv")
depth_day<-depth %>%mutate(Date=as.Date(Date))%>% group_by(ID, Date)%>% mutate(depth=mean(depth, na.rm = T))%>%
  distinct(ID, Date, .keep_all = T)

Q <- read_csv("02_Clean_data/discharge.csv")
Q_day<-Q %>%mutate(Date=as.Date(Date))%>% group_by(ID, Date)%>% mutate(Q=mean(Q, na.rm = T))%>%
  distinct(ID, Date, .keep_all = T)

hydro<-left_join(Q_day, depth_day, by=c('ID', 'Date'))
stream_depth<-left_join(stream, hydro, by=c('ID', 'Date'))


ggplot(stream_depth, aes(x=Q))+
  geom_point(aes(y=CO2_umol_L, color='CO2_umol_L'))+  geom_point(aes(y=CH4_umol_L, color='CH4_umol_L'))+
  geom_point(aes(y=N2O_umol_L, color='N2O_umol_L'))+scale_y_log10()+scale_x_log10()+
  facet_wrap(~ ID, ncol=3)+ylab('umol_L')

ggplot(stream_depth, aes(x=Q))+
  geom_point(aes(y=CO2_sat, color='CO2_sat'))+
  geom_point(aes(y=CH4_sat, color='CH4_sat'))+
  #geom_point(aes(y=N2O_sat, color='N2O_sat'))+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ ID, ncol=3)+ylab('umol_L')

RChydro <- read_csv("02_Clean_data/allC_RC.csv")
RChydro<-RChydro %>% mutate(Stream=as.character(Stream), Well=as.character(Well))
RC<-RC %>% separate(ID, into = c("Stream", "Well"), sep = "GW")
RC_hydro<-left_join(RC, RChydro, by=c('Stream','Well','Date'))

ggplot(RC_hydro %>% filter(ID != is.na(ID)), aes(x=WTdepth_m))+
  #geom_point(aes(y=CO2_umol_L, color='CO2_umol_L'))+
  geom_point(aes(y=CH4_umol_L, color='CH4_umol_L'))+
  geom_point(aes(y=N2O_umol_L, color='N2O_umol_L'))+
  facet_wrap(~ ID, ncol=3)+ylab('umol_L')

ggplot(RC_hydro, aes(x=Distance_m))+
  geom_point(aes(y=CO2_umol_L, color='CO2_umol_L'))+
  geom_point(aes(y=CH4_umol_L, color='CH4_umol_L'))+
  geom_point(aes(y=N2O_umol_L, color='N2O_umol_L'))+
  facet_wrap(~ ID, ncol=3)+ylab('umol_L')
