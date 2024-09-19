###packages####
library(tidyverse)
library(writexl)
library(readxl)
library(seacarb)
library(weathermetrics)

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,6,8,4)]
data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
master<-join_all(data, by=c('Date','ID'), type='left')
detach("package:plyr", unload = TRUE)
master<-master %>% rename('Temp'='Temp_PT')

HCO3 <- function(master) {
  master <- master[complete.cases(master[ , c('Temp','CO2','pH','Water_press')]), ]
  master$Temp<- fahrenheit.to.celsius(master$Temp)
  master$Temp_K<- master$Temp+273.15

  master$exp<-2400*((1/master$Temp_K)-(1/298.15))
  master$KH<-0.034*2.178^(master$exp)#mol/L/atm
  master$CO2_atm<-master$CO2/1000000
  master$CO2_molL<-master$CO2_atm*master$KH

  master$K1<-K1(S=0.01, T=master$Temp, P=master$Water_press)
  master$K2<-K2(S=0.01, T=master$Temp, P=master$Water_press)

  master$pK1<- -log10(master$K1)
  master$pK2<- -log10(master$K2)

  master$HCO3_molL<-(10^(master$pH-master$pK1))*master$CO2_molL
  master$CO3_molL<-(10^(master$pH-master$pK2))*master$HCO3_molL

  x<-c("Date",'ID','CO2_molL','HCO3_molL','CO3_molL')
  master<-master[,x]
  return(master)}

DIC<-master %>% group_by(ID) %>% HCO3()
DIC<-DIC  %>% group_by(ID, Date) %>% mutate(CO2_mmolL=mean(CO2_molL, na.rm=T)*1000,
                                            HCO3_mmolL=mean(HCO3_molL, na.rm=T)*1000,
                                            CO3_mmolL=mean(CO3_molL, na.rm=T)*1000) %>%
  mutate(CO2_mgL=CO2_molL*28000,
         HCO3_mgL=HCO3_molL*36000,
         CO3_mgL=CO3_molL*35000)

DIC <- DIC[!duplicated(DIC[c('Date','ID')]),]
DIC<-DIC  %>% group_by(ID, Date) %>% mutate(DIC_mmol_int= CO2_mmolL+CO3_mmolL+HCO3_mmolL)%>%
  mutate(DIC_mgL_int=CO2_mgL+CO3_mgL+HCO3_mgL)

y<-c("Date",'ID','CO2_molL','HCO3_molL','CO3_molL','DIC_mmol_int',"CO2_mgL","HCO3_mgL","CO3_mgL",'DIC_mgL_int')
DIC<-DIC[,y]
DIC <- DIC[complete.cases(DIC[ , c('CO2_molL')]), ]
range(DIC$Date)

write_csv(DIC, "02_Clean_data/alkalinity.csv")
