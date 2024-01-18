###packages####
library(tidyverse)
library(writexl)
library(readxl)
library(seacarb)
library(weathermetrics)

master<-read_csv('02_Clean_data/master.csv')

HCO3 <- function(site) {
  site <- site[complete.cases(site[ , c('Temp','CO2','pH')]), ]
  site$Temp<- fahrenheit.to.celsius(site$Temp)
  site$Temp_K<- site$Temp+273.15

  site$exp<-2400*((1/site$Temp_K)-(1/298.15))
  site$KH<-0.034*2.178^(site$exp)#mol/L/atm
  site$CO2_atm<-site$CO2/1000000
  site$CO2_molL<-site$CO2_atm*site$KH

  site$Ka<-K1(S=0.01, T=site$Temp, P=site$Water_press)
  site$HCO3_molL<-(10^(site$pH-site$Ka))*site$CO2_molL

  x<-c("Date",'CO2_molL','HCO3_molL')
  site<-site[,x]
  return(site)}

s3<-filter(master, ID=='3')
s3<-HCO3(s3)
s3$ID<-'3'

s5<-filter(master, ID=='5')
s5<-HCO3(s5) ##check
s5$ID<-'5'

s5a<-filter(master, ID=='5a')
s5a<-HCO3(s5a)
s5a$ID<-'5a'

s6<-filter(master, ID=='6')
s6<-HCO3(s6)
s6$ID<-'6'

s6a<-filter(master, ID=='6a')
s6a<-HCO3(s6a)
s6a$ID<-'6a'

s7<-filter(master, ID=='7')
s7<-HCO3(s7)
s7$ID<-'7'

s9<-filter(master, ID=='9')
s9<-HCO3(s9)
s9$ID<-'9'

s13<-filter(master, ID=='13')
s13<-HCO3(s13)
s13$ID<-'13'

s14<-filter(master, ID=='14')
s14<-HCO3(s14)
s14$ID<-'s14'

master<-rbind(s3, s5,s5a, s6, s6a, s7, s9, s13, s14)

ggplot(s15, aes(Date)) +
  geom_line(aes(y = CO2_molL*10000)) +
  geom_line(aes(y = pH, color="blue")) +
  geom_line(aes(y = HCO3_molL, color="red"))


ggplot(master, aes(Date)) +
  geom_line(aes(y = CO2_molL*10000), color='blue') +
  geom_line(aes(y = HCO3_molL), color='red')+
  facet_wrap(~ ID, ncol=5)+
  theme_sam
