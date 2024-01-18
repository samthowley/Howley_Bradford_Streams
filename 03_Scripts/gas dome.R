rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(weathermetrics)
master <- read_csv("02_Clean_data/master.csv")
#k600 calculation constants######
dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38
#######

GasDome <- function(gas,stream) {
  stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date))
  y<-c("CO2_enviro",'Temp','depth',"day","hour")
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
  depth<-mean(gas$depth, na.rm=T)

  (m<-lm(CO2~Date, data = gas))
  cf <- coef(m)
  (slope <- cf[2])
  deltaCO2_atm<- ((slope*-1)*2/1000000) #change in CO2 during float
  pCO2_air<-min(gas$CO2, na.rm=T)/1000000

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
k600_list <- function(stage,k600,date) {
  col2<-list(stage,k600) #stage and k600 columns
  col2<-as.data.frame(do.call(cbind, col2))
  col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

  col1<-data.frame(date=date) #date column
  col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
  col1<-col1[,-c(1)]

  done<-cbind(col1, col2)
  return(done)}

##### 3 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="3")
file.names <- list.files(path="01_Raw_data/GD/3",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

done_3<-k600_list(stage, k600,date)
write_xlsx(done_3, "04_Output/k600/3.xlsx")

##### 5 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="5")
file.names <- list.files(path="01_Raw_data/GD/5",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_5<-k600_list(stage, k600,date))
write_xlsx(done_5, "04_Output/k600/5.xlsx")


##### 5a k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="5a")
file.names <- list.files(path="01_Raw_data/GD/5a",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_5a<-k600_list(stage, k600,date))
write_xlsx(done_5a, "04_Output/k600/5a.xlsx")

##### 6 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="6")
file.names <- list.files(path="01_Raw_data/GD/6",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_6<-k600_list(stage, k600,date))
write_xlsx(done_6, "04_Output/k600/6.xlsx")

##### 6a k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="6a")
file.names <- list.files(path="01_Raw_data/GD/6a",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_6a<-k600_list(stage, k600,date))
write_xlsx(done_6a, "04_Output/k600/6a.xlsx")

##### 7 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="7")
file.names <- list.files(path="01_Raw_data/GD/7",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_7<-k600_list(stage, k600,date))
write_xlsx(done_7, "04_Output/k600/7.xlsx")

##### 9 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="9")
file.names <- list.files(path="01_Raw_data/GD/9",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_9<-k600_list(stage, k600,date))
write_xlsx(done_9, "04_Output/k600/9.xlsx")

##### 13 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="13")
file.names <- list.files(path="01_Raw_data/GD/13",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_13<-k600_list(stage, k600,date))
write_xlsx(done_13, "04_Output/k600/13.xlsx")
##### 14 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="14")
file.names <- list.files(path="01_Raw_data/GD/14",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_14<-k600_list(stage, k600,date))
write_xlsx(done_14, "04_Output/k600/14.xlsx")
##### 15 k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="15")
file.names <- list.files(path="01_Raw_data/GD/15",pattern="csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

(done_15<-k600_list(stage, k600,date))
write_xlsx(done_15, "04_Output/k600/15.xlsx")

library(lubridate)
library(weathermetrics)

##organize data file@##########
gas<- read_csv("01_Raw_data/GD/GasChamber_01052024.dat",skip = 3)
gas<-gas[,c(1,5)]
colnames(gas)[1] <- "Date"
colnames(gas)[2] <- "CO2"
gas<-filter(gas, Date>'2024-01-03')
write_csv(gas, "01_Raw_data/GD/Unorganized/GasDome_11012023.csv")


