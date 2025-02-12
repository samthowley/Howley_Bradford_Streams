library(marelac)
library(tidyverse)
library(weathermetrics)
library(streamMetabolizer)


R<-0.08205
V_wp<-70/1000
V_hp<-25/1000

#Compile pressure and temperature data####
compiled_PT <- read_csv("01_Raw_data/PT/compiled_PT.csv")
PT<-compiled_PT %>% rename('Water_P'="PT")
compiled_baro <- read_csv("01_Raw_data/PT/compiled_baro.csv")
baro<-compiled_baro %>% rename("air_P"="PTbaro")

P<-left_join(PT, baro, by=c('region', 'Date'))%>%fill(air_P,Temp_PT, .direction='downup')%>%
  mutate(P=(Water_P-air_P)/14.696,Temp_PT= fahrenheit.to.celsius(Temp_PT))

k<-P%>%mutate(k_CO2=(gas_solubility(S = 0, t = Temp_PT, species = "CO2"))/1000*0.986923,
              k_CH4=(gas_solubility(S = 0, t = Temp_PT, species = "CH4"))/1000*0.986923,
              k_N2O=(gas_solubility(S = 0, t = Temp_PT, species = "N2O"))/1000*0.986923)

light<-k%>%mutate(light= calc_light(Date,29.914156, -82.256152))%>%
  mutate(Date=as.Date(Date)) %>% filter(light> 700) %>%
  group_by(ID, Date)%>%
  mutate(k_CO2=mean(k_CO2, na.rm=T), k_CH4=mean(k_CH4, na.rm=T),k_N2O=mean(k_N2O, na.rm=T),
         air_P=mean(air_P), Water_P=mean(Water_P))%>%
  select(-region)%>%
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
  separate(SampleName, into = c("Site", "Date","Rep"), sep = "_")%>%mutate(Date=mdy(Date))

#parse out air samples
air<-format%>% filter(CO2_x>300 & CO2_x<800)%>% rename(N2O_air=N2O_x, CH4_air= CH4_x, CO2_air=CO2_x)%>%
  select(-Rep)
samples<- format %>% filter(CO2_x>800)%>%arrange(Site, Date)

samples_air<-left_join(samples, air, by=c('Site', 'Date'))

#include air conc
samples_air <- samples_air%>% group_by(Site) %>%
  fill(N2O_air, CO2_air, CH4_air, .direction = "downup") %>%ungroup() %>%
  mutate(
    N2O_air = if_else(is.na(N2O_air), mean(N2O_air, na.rm = TRUE), N2O_air),
    CO2_air = if_else(is.na(CO2_air), mean(CO2_air, na.rm = TRUE), CO2_air),
    CH4_air = if_else(is.na(CH4_air), mean(CH4_air, na.rm = TRUE), CH4_air)) %>%ungroup()

#divide gas samples by chapter######
samples_air<-samples_air %>% mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                         Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                         Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',

                                         Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                         Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                         Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                         Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                         Site=='9GW4'~'9',Site=='5GW8'~'5',Site=='9GW5'~'9',

                                         Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
                                         Site=='5.4'~'5',Site=='5.5'~'5',Site=='6.1'~'6',Site=='6.2'~'6',
                                         Site=='6.3'~'6',Site=='6.4'~'6',Site=='6.5'~'6',Site=='6.6'~'6',
                                         Site=='9.1'~'9',Site=='9.2'~'9',Site=='9.3'~'9',Site=='9.4'~'9',
                                         Site=='9.5'~'9',Site=='9.6'~'9',Site=='9.Sam'~'9',Site=='3.1'~'6',
                                         Site=='3.4'~'6',Site=='5.6'~'5'))%>%distinct(ID,Date,Rep, .keep_all = T)

raw_data<-left_join(samples_air, light, by=c('Date','ID'))







#measure####
meas<-raw_data %>%mutate(Kelvin=Temp_PT+273.15)%>%mutate(
  CO2_numerator=CO2_x*P*(k_CO2*V_wp+(1/(R*Kelvin))*V_hp)-CO2_air,
  CH4_numerator=CH4_x*P*(k_CH4*V_wp+(1/(R*Kelvin))*V_hp)-CH4_air,
  N2O_numerator=N2O_x*P*(k_N2O*V_wp+(1/(R*Kelvin))*V_hp)-N2O_air
)%>%mutate(
  CO2_meas=CO2_numerator/V_wp,
  CH4_meas=CH4_numerator/V_wp,
  N2O_meas=N2O_numerator/V_wp,
)


