rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(lubridate)
library(weathermetrics)

#k600 calculation constants
dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38

#for loop
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

#k600 calculation
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

##### 3 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/3.xlsx")
file.names <- list.files(path="01_Raw_data/3",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_3<-cbind(col1, col2)

write_xlsx(done_3, "04_Output/k600/3.xlsx")
##### 5 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/5.xlsx")
file.names <- list.files(path="01_Raw_data/GD/5",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_5<-cbind(col1, col2)

write_xlsx(done_5, "04_Output/k600/5.xlsx")

##### 5a k600 ########
#Call in data
stream <- read_excel("02_Clean_data/5a.xlsx")
file.names <- list.files(path="01_Raw_data/GD/5a",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_5a<-cbind(col1, col2)

write_xlsx(done_5a, "04_Output/k600/5a.xlsx")

##### 6 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/6.xlsx")
file.names <- list.files(path="01_Raw_data/GD/6",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_6<-cbind(col1, col2)

write_xlsx(done_6, "04_Output/k600/6.xlsx")

##### 6a k6a00 ########
#Call in data
stream <- read_excel("02_Clean_data/6a.xlsx")
file.names <- list.files(path="01_Raw_data/GD/6a",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k6a00[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k6a00) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_6a<-cbind(col1, col2)

write_xlsx(done_6a, "04_Output/k600/6a.xlsx")
##### 7 k700 ########
#Call in data
stream <- read_excel("02_Clean_data/7.xlsx")
file.names <- list.files(path="01_Raw_data/GD/positive/7",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k700[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k700) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_7<-cbind(col1, col2)

write_xlsx(done_7, "04_Output/k600/7.xlsx")
##### 9 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/9.xlsx")
file.names <- list.files(path="01_Raw_data/GD/9",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k900[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_9<-cbind(col1, col2)

write_xlsx(done_9, "04_Output/k600/9.xlsx")
##### 13 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/13.xlsx")
file.names <- list.files(path="01_Raw_data/GD/13",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k1300[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_13<-cbind(col1, col2)

write_xlsx(done_13, "04_Output/k600/13.xlsx")
##### 14 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/14.xlsx")
file.names <- list.files(path="01_Raw_data/GD/14",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k1400[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_14<-cbind(col1, col2)

write_xlsx(done_14, "04_Output/k600/14.xlsx")
##### 15 k600 ########
#Call in data
stream <- read_excel("02_Clean_data/15.xlsx")
file.names <- list.files(path="01_Raw_data/GD/15",pattern="csv", full.names=TRUE)

for(i in file.names){

  gas<-read_csv(i,col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k1500[i] <- output[3]
  date[i]<-output[1]
}

col2<-list(stage,k600) #stage and k600 columns
col2<-as.data.frame(do.call(cbind, col2))
col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

col1<-data.frame(date=date) #date column
col1<-pivot_longer(col1, cols = 1:1, names_to = 'excel', values_to = 'Date') #wide to long
col1<-col1[,-c(1)]

done_15<-cbind(col1, col2)

write_xlsx(done_15, "04_Output/k600/15.xlsx")

