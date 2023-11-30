rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(lubridate)



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
k600_sheet1<- vector("numeric")
stage_sheet1 <- vector("numeric")
date_sheet1 <-as.Date(character(0))

stream <- read_excel("02_Clean_data/3.xlsx")
stream$CO2<-stream$CO2*6 #sensor multiplier
stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date))
stream<-stream[,-1]

file.names <- list.files(path="01_Raw_data/GD/negative", pattern="xlsx", full.names=TRUE)

#test out. create test dataframe called gas
GasDome <- function(gas,stream) {

  gas<-gas[,x]
  gas$CO2<-gas$CO2*6
  gas<-gas %>% mutate(day=day(Date), hour=hour(Date))
  gas<-left_join(gas, stream,by=c('day', 'hour'))
  gas <- gas[!duplicated(gas[c('Date')]),]
  day <- as.Date(gas$Date)

  mouthTemp_F<-mean(gas$Temp, na.rm=T)
  mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
  mouthTemp_K<-mouthTemp_C+273.15
  SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
  SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

  pCO2_water<-	mean(gas$CO2_enviro, na.rm=T)/1000000
  depth<-mean(gas$stage, na.rm=T)

  (m<-lm(CO2~Date, data = gas))
  cf <- coef(m)
  (slope <- cf[2])
  deltaCO2_atm<- ((slope)*2/1000000) #change in CO2 during float
  pCO2_air<-max(gas$CO2)/1000000

  n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
  FCO2<-n/domeFoot_m2*60
  exp<-2400*((1/mouthTemp_K)-(1/298.15))
  (KH<-0.034*exp(exp)) #mol/L/atm
  KH_1000<-KH*1000

  KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
  kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
  k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

  (KO2_1d<-kO2/depth)
  (KCO2_1d<-KCO2_md/depth)
  (k600_1d<- k600_md/depth)


  output<-list(day, depth, k600_1d)
  return(output)
}


for(i in file.names){

  gas<-read_excel(i, sheet="3")
  site<-GasDome(gas, stream)
  date_sheet1[i]<-gas$day[1]
  stage_sheet1[i] <- depth
  k600_sheet1[i] <- k600_1d

  }

sheet1<-data.frame(date_sheet1,k600_sheet1, stage_sheet1)
sheet1<- sheet1 %>% rename('date'="date_sheet1",
                           'k600'="k600_sheet1", 'stage'="stage_sheet1")
