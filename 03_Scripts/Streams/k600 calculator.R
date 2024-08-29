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
  stream<-stream %>% mutate(day=day(Date), hour=hour(Date), month=month(Date), yr=year(Date))%>%fill(Temp)%>%
    rename(CO2water=CO2)
  stream<-stream[,-1]


  gas<-gas %>% mutate(day=day(Date),hour=hour(Date),month=month(Date),yr=year(Date))%>%rename(CO2dome=CO2)
  gas<-left_join(gas, stream,by=c('hour', 'day', 'month', 'yr', 'ID'), relationship = "many-to-many")

  gas$Temp_F<-mean(gas$Temp, na.rm=T)
  gas$Temp_C<-fahrenheit.to.celsius(gas$Temp_F)
  gas$Temp_K<-gas$Temp_C+273.15
  gas$SchmidtO2hi<-1568-86.04*gas$Temp_C+2.142*gas$Temp_C^2-0.0216*gas$Temp_C^3
  gas$SchmidtCO2hi<-1742-91.24*gas$Temp_C+2.208*gas$Temp_C^2-0.0219*gas$Temp_C^3

  gas<-gas %>% select(-day, -month, -yr, -hour) %>%
    mutate(pCO2_water=CO2water/1000000, day=as.Date(Date),pCO2_air= (500/1000000))

  diffuse<-lm(CO2dome ~ Date, data = gas)
  gas$slope<-coef(diffuse)[2]

  gas$deltaCO2_atm<- (abs(gas$slope)*6/1000000) #change in CO2 during float

  gas$n<-(gas$deltaCO2_atm*domeVol_L/R/gas$Temp_K)
  gas$FCO2<-gas$n/domeFoot_m2*60
  gas$KH<-0.034*exp(2400*((1/gas$Temp_K)-(1/298.15)))
  gas$KH_1000<-gas$KH*1000

  gas$KCO2_md<-(gas$FCO2/gas$KH_1000/(gas$pCO2_water-gas$pCO2_air))*24 #m/d
  gas$kO2<-gas$KCO2_md*(gas$SchmidtCO2hi/gas$SchmidtO2hi)^(-2/3)
  gas$k600_md<- gas$KCO2_md*(600/gas$SchmidtCO2hi)^(-2/3) #m/d

  (gas$KO2_1d<-gas$kO2/gas$depth)
  (gas$KCO2_1d<-gas$KCO2_md/gas$depth)
  (gas$k600_1d<- as.numeric(gas$k600_md/gas$depth))

  gas <- gas[!duplicated(gas[c('k600_1d','day')]),]
  # gas<-gas%>% select(Date,ID,depth,Q,Temp_C,KO2_1d,KCO2_1d,k600_1d,k600_md,pCO2_water,pCO2_air)

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
gasdome<-gasdome %>% mutate(k600_1d=abs(k600_1d)) %>% filter(depth>0)
range(gasdome$Date)
write_csv(gasdome, "01_Raw_data/GD/GasDome_compiled.csv")

split<-gasdome %>% split(gasdome$ID)
write.xlsx(split, file = '04_Output/rC_k600.xlsx')

#organize data file##########
gas<- read_csv("01_Raw_data/GD/raw/GasDome_08012024.dat",skip = 3)
gas<-gas[,c(1,5)]
colnames(gas)[1] <- "Date"
colnames(gas)[2] <- "CO2"
gas<-gas %>%filter(Date>'2024-07-31')

write_csv(gas, "01_Raw_data/GD/raw/GasDome_08012024.csv")




seperated<- read_csv("01_Raw_data/GD/seperated/GB_12062023_5.csv")

seperated<-filter(seperated, Date< ymd_hms('2024-05-30 15:15:00'))
ggplot(seperated, aes(x=Date, y=CO2)) +
  geom_point()+geom_smooth(method='lm',se = FALSE)

write_csv(seperated, "01_Raw_data/GD/seperated/GB_05302024_13.csv")


