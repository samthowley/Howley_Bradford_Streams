rm(list=ls())

library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(xlsx)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(dataRetrieval)



dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38


#setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

for(fil in file.names){
  mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
  mouthTemp_K<-mouthTemp_C+273.15
  SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
  SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3
  
  pCO2_water<-5418.0/1000000
  depth<-0.6518525
  
  AllenMill_mouth_r1<- read_excel("AllenMill_0608023.xlsx",sheet = "Sheet1")
  AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
  (vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
  deltaCO2_atm<-(1.113e+01)*2/1000000 #change in CO2 during float
  pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

  n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
  FCO2<-n/domeFoot_m2*60
  exp<-2400*((1/mouthTemp_K)-(1/298.15))
  (KH<-0.034*exp(exp)) #mol/L/atm
  KH_1000<-KH*1000

  (KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
  kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
  k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

  (KO2_1d_0829m2<-kO2/depth)
  (KCO2_1d_0829m2<-KCO2_md/depth)
  (k600_vent1_d<- k600_vent1/depth)
}