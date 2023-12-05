rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(lubridate)
library(weathermetrics)

dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38

x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

GasDome <- function(gas,stream) {
  stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date))
  y<-c("CO2_enviro",'Temp','Stage',"day","hour")
  stream<-stream[,y]

  gas<-gas[,x]
  gas$CO2<-gas$CO2*6
  gas<-gas %>% mutate(day=day(Date), hour=hour(Date))
  gas<-left_join(gas, stream,by=c('hour', 'day'), relationship = "many-to-many")
  day <- as.Date(gas$Date)[1]

  mouthTemp_F<-mean(gas$Temp, na.rm=T)
  mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
  mouthTemp_K<-mouthTemp_C+273.15
  SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
  SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

  pCO2_water<-	mean(gas$CO2_enviro, na.rm=T)/1000000
  depth<-mean(gas$Stage, na.rm=T)

  (m<-lm(CO2~Date, data = gas))
  cf <- coef(m)
  (slope <- cf[2])
  deltaCO2_atm<- ((slope*-1)*2/1000000) #change in CO2 during float
  pCO2_air<-max(gas$CO2, na.rm=T)/1000000

  n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
  FCO2<-n/domeFoot_m2*60
  exp<-2400*((1/mouthTemp_K)-(1/298.15))
  (KH<-0.034*exp(exp)) #mol/L/atm
  KH_1000<-KH*1000

  KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
  kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
  k600_md<- KCO2_md*(600/SchmidtCO2hi)^(-2/3) #m/d

  (KO2_1d<-kO2/depth)
  (KCO2_1d<-KCO2_md/depth)
  (k600_1d<- as.numeric(k600_md/depth))
  return(list(day, depth, k600_1d))
}

stream <- read_excel("02_Clean_data/3.xlsx")
file.names <- list.files(path="01_Raw_data/GD/positive/3",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600)
df2<-as.data.frame(do.call(cbind, col2))
df2<- df2 %>% rename('stage'="V1",'k600'="V2")

df1<-data.frame(date=date)
df1<-pivot_longer(df1, cols = 1:1, names_to = 'excel', values_to = 'Date')
df1<-df1[,-c(1)]

done<-cbind(df1, df2)

