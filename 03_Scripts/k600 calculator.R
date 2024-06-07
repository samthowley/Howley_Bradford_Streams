rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(weathermetrics)
library(lme4)
library(tools)

dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38

stream<-read_csv('master.csv')
stream<-stream %>%  mutate(min = minute(Date)) %>% filter(min==0)


GasDome <- function(gas,stream) {
  stream<-stream %>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  stream<-stream[,c('Temp','depth',"day","hour", 'month', 'yr', 'ID')]
  stream$Temp[is.na(stream$Temp)]<-mean(stream$Temp, na.rm=T)
  #stream<-stream %>% rename('CO2_enviro'='CO2')
  # gas <- gas %>% group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%ungroup()
  # pre<-gas[1:6,]
  # gas$CO2_enviro<-mean(pre$CO2, na.rm=T)

  gas<-gas %>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  gas<-left_join(gas, stream,by=c('hour', 'day', 'month', 'yr', 'ID'), relationship = "many-to-many")

  gas$Temp_F<-mean(gas$Temp, na.rm=T)
  gas$Temp_C<-fahrenheit.to.celsius(gas$Temp_F)
  gas$Temp_K<-gas$Temp_C+273.15
  gas$SchmidtO2hi<-1568-86.04*gas$Temp_C+2.142*gas$Temp_C^2-0.0216*gas$Temp_C^3
  gas$SchmidtCO2hi<-1742-91.24*gas$Temp_C+2.208*gas$Temp_C^2-0.0219*gas$Temp_C^3

  gas<-gas %>%
    group_by(day,month, yr,ID) %>%
    mutate(cat = cur_group_id(), .before = c('ID', 'day')) %>%
    mutate(pCO2_water=max(CO2, na.rm=T)/1000000, day=as.Date(Date),
           pCO2_air=min(gas$CO2, na.rm=T)/1000000)%>%
    ungroup

  diffuse<-lm(CO2 ~ Date, data = gas)
  gas$slope<-coef(diffuse)[2]

  gas$deltaCO2_atm<- (abs(gas$slope)*6/1000000) #change in CO2 during float

  gas$n<-(gas$deltaCO2_atm*domeVol_L/R/gas$Temp_K)
  gas$FCO2<-gas$n/domeFoot_m2*60
  gas$exp<-2400*((1/gas$Temp_K)-(1/298.15))
  gas$KH<-0.034*((gas$exp)*(gas$exp))#mol/L/atm
  gas$KH_1000<-gas$KH*1000

  gas$KCO2_md<-(gas$FCO2/gas$KH_1000/(gas$pCO2_water-gas$pCO2_air))*24 #m/d
  gas$kO2<-gas$KCO2_md*(gas$SchmidtCO2hi/gas$SchmidtO2hi)^(-2/3)
  gas$k600_md<- gas$KCO2_md*(600/gas$SchmidtCO2hi)^(-2/3) #m/d

  (gas$KO2_1d<-gas$kO2/gas$depth)
  (gas$KCO2_1d<-gas$KCO2_md/gas$depth)
  (gas$k600_1d<- as.numeric(gas$k600_md/gas$depth))

  gas <- gas[!duplicated(gas[c('k600_1d','ID')]),]
  x<-c("Date","Temp_C","depth","pCO2_water","pCO2_air","slope","deltaCO2_atm",
       "n","FCO2","exp","KH","KH_1000","KCO2_md","kO2","k600_md","KO2_1d",
       "KCO2_1d","k600_1d",'ID','day')
  gas<-gas[,x]

  return(gas)
}

#k600 compiled########
gasdome<-data.frame()

file.names <- list.files(path="01_Raw_data/GD/seperated", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  gas$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][5]
  gas<-GasDome(gas,stream)
  gasdome<-rbind(gasdome, gas)}
gasdome_compiled <- gasdome[!duplicated(gasdome[c('day','ID')]),]
gasdome_compiled<-gasdome_compiled %>% mutate(k600_1d=abs(k600_1d))
write_csv(gasdome_compiled, "01_Raw_data/GD/GasDome_compiled.csv")


#Join with Q####
gasdome_compiled<-read_csv('01_Raw_data/GD/GasDome_compiled.csv')
Q<-read_csv('02_Clean_data/discharge.csv')
Q<-Q %>% mutate(Date= as.Date(Date)) %>% group_by(Date,ID) %>% mutate(Q_avg=mean(Q, na.rm=T))
gasdome_compiled<-rename(gasdome_compiled, 'Date'='day')

gasdome_compiled<-left_join(gasdome_compiled, Q, by=c('Date', 'ID'))
gasdome_compiled <- gasdome_compiled[!duplicated(gasdome_compiled[c('Date','ID')]),]

ggplot(gasdome_compiled, aes(depth, k600_1d)) + geom_point() + facet_wrap(~ ID, ncol=5)
write_csv(gasdome_compiled, "01_Raw_data/GD/GasDome_compiled.csv")
split<-gasdome_compiled %>% split(gasdome_compiled$ID)
write.xlsx(split, file = '04_Output/rC_k600.xlsx')


#organize data file##########
gas<- read_csv("01_Raw_data/GD/raw/GasDome_05302024.dat",skip = 3)
gas<-gas[,c(1,5)]
colnames(gas)[1] <- "Date"
colnames(gas)[2] <- "CO2"
gas<-gas %>%filter(Date>'2024-05-10')

write_csv(gas, "01_Raw_data/GD/raw/GasDome_05302024.csv")




seperated<- read_csv("01_Raw_data/GD/seperated/GB_12062023_5.csv")

seperated<-filter(seperated, Date< ymd_hms('2024-05-30 15:15:00'))
ggplot(seperated, aes(x=Date, y=CO2)) +
  geom_point()+geom_smooth(method='lm',se = FALSE)

write_csv(seperated, "01_Raw_data/GD/seperated/GB_05302024_13.csv")


