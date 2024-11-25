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
stream<-stream%>%select(Date, ID,depth, Q, CO2,Temp)%>%fill(CO2, .direction="up")

GasDome <- function(gas,stream) {

  stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date), date=as.Date(Date))%>%
    group_by(date,ID)%>%mutate(depth=mean(depth, na.rm=T), Q=mean(Q, na.rm=T), Temp=mean(Temp, na.rm=T))%>%
    select(CO2_enviro,Temp,depth,Q,day, hour,month,yr,ID)

  gas<-gas %>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  gas<-left_join(gas,stream, by=c('hour', 'day', 'month', 'yr', 'ID'), relationship = "many-to-many")

  gas<-gas%>%mutate(Temp_F=mean(Temp, na.rm=T))%>% mutate(Temp_C=fahrenheit.to.celsius(Temp_F))%>%
    mutate(Temp_K=Temp_C+273.15,
           SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3,
           SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)

  gas<-gas %>%
    mutate(pCO2_water=CO2_enviro/1000000, day=as.Date(Date),
           pCO2_air=max(gas$CO2, na.rm=T)/1000000, sec=second(Date))%>%mutate(
             sec_cumulative=cumsum(sec))

  diffuse<-lm(CO2~ sec_cumulative, data = gas) #CO2 ppm/sec
  gas$slope<-coef(diffuse)[2]

  gas$deltaCO2_atm<- (abs(gas$slope)/1000000) #change in CO2 during float

  gas$n<-(gas$deltaCO2_atm*15.466)/0.085/gas$Temp_K #CO2 mol/sec

  gas$FCO2<-(gas$n/domeFoot_m2)*60*60 #mol/m^2/h
  gas$KH<-0.034*exp(2400*((1/gas$Temp_K)-(1/298.15)))
  gas$KH_1000<-gas$KH*1000 #mol/m^3/atm

  gas$KCO2_dh<-gas$FCO2/gas$KH_1000/(gas$pCO2_air-gas$pCO2_water)#m/h
  gas$kO2_dh<-gas$KCO2_dh*(gas$SchmidtCO2hi/gas$SchmidtO2hi)^(-2/3)#m/h
  gas$k600_dh<- gas$KCO2_dh*(600/gas$SchmidtCO2hi)^(-2/3) #m/h

  gas$KO2_1d<-(gas$kO2_dh/gas$depth)*24
  gas$KCO2_1d<-(gas$KCO2_dh/gas$depth)*24
  gas$k600_1d<- (gas$k600_dh/gas$depth)*24

  gas<-gas%>% select(day,ID,CO2,CO2_enviro,depth,Q,Temp,KCO2_1d,k600_1d)

  return(gas)
}

#k600 compiled########
gasdome<-data.frame()

file.names <- list.files(path="01_Raw_data/GD/seperated", full.names=TRUE)
#i<-file.names[39]
for(i in file.names){
  gas<-read_csv(i)
  gas$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][5]
  gas<-GasDome(gas,stream)
  gasdome<-rbind(gasdome, gas)}

gasdome <- gasdome[!duplicated(gasdome[ ,c('ID','day')]), ]
gasdome<-gasdome %>% mutate(k600_1d=abs(k600_1d), KCO2_1d=abs(KCO2_1d), logQ=log10(Q))
write_csv(gasdome, "01_Raw_data/GD/GasDome_compiled.csv")

split<-gasdome %>% split(gasdome$ID)
write.xlsx(split, file = '04_Output/rC_k600.xlsx')

#organize data file##########
gas<- read_csv("01_Raw_data/GD/raw/GasDome_10302024.dat",skip = 3)
gas<-gas[,c(1,5)]
colnames(gas)[1] <- "Date"
colnames(gas)[2] <- "CO2"
gas<-gas %>% mutate(CO2=CO2*6) %>%filter(Date>'2024-10-28' & Date< '2024-11-04')%>%filter(CO2>100)

ggplot(gas, aes(x=Date, y=CO2)) +geom_point()

write_csv(gas, "01_Raw_data/GD/raw/GasDome_10302024.csv")
