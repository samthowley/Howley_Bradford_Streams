###packages####
library(tidyverse)
library(writexl)
library(readxl)
library(seacarb)

master<-read_csv('02_Clean_data/master.csv')

HCO3 <- function(site) {
  site$Temp_C<- fahrenheit.to.celsius(site$Temp)

  site$exp<-2400*((1/site$mouthTemp_K)-(1/298.15))
  site$KH<-0.034*2.178^(site$exp)#mol/L/atm
  site$CO2_atm<-site$CO2/1000000
  site$CO2_mol<-site$CO2_atm*site$KH

  site$Ka<-K1(S=0.01, T=master$Temp, P=site$PT)
  site$HCO3_molL<-10^(site$pH-site$Ka)*site$CO2

  x<-c("Date","DO","Temp","ID","depth","discharge","CO2","pH","SpC",CO2_molL, HCO2_molL)
  site<-site[,x]
  return(site)}


