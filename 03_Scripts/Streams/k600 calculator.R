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

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,4,6,11)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
merged_data <- reduce(data, left_join, by = c("ID", 'Date'))

ggplot(merged_data, aes(Date, CO2)) + geom_point() + facet_wrap(~ ID, ncol=4)

stream<-merged_data%>%rename(Temp=`Temp_PT.x`, CO2_enviro=CO2)%>%
  select(Date, ID,depth, Q, CO2_enviro,Temp)%>% fill(CO2_enviro, .direction="up")

GasDome <- function(gas,stream) {

  stream<-stream %>%
    mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date), date=as.Date(Date))%>%
    group_by(date,ID)%>%
    mutate(depth=mean(depth, na.rm=T), Q=mean(Q, na.rm=T), Temp=mean(Temp, na.rm=T))%>%
    select(CO2_enviro,Temp,depth,Q,day, hour,month,yr,ID)

  gas<-gas %>% mutate(minute=minute(Date),day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  gas<-left_join(gas,stream, by=c('hour', 'day', 'month', 'yr', 'ID'), relationship = "many-to-many")

  gas<-gas%>%mutate(Temp_F=mean(Temp, na.rm=T))%>% mutate(Temp_C=fahrenheit.to.celsius(Temp_F))%>%
    mutate(Temp_K=Temp_C+273.15,
           SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3,
           SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)%>%
    group_by(minute, ID, day)%>%mutate(CO2=mean(CO2, na.rm=T))%>%
    distinct(minute, ID, day, .keep_all = T)

  gas <- gas %>%
    group_by(ID, day) %>%
    mutate(pCO2_water = CO2_enviro / 1000000, pCO2_air = max(CO2, na.rm = TRUE) / 1000000,
      minute_cumulative = cumsum(rep(1, n()))) %>%
    ungroup()

  diffuse<-lm(CO2~ minute_cumulative, data = gas) #CO2 ppm/min
  gas$slope<-coef(diffuse)[2]

  gas$deltaCO2_atm<- (abs(gas$slope)/1000000) # CO2 atm/min; change in CO2 during float

  gas$n<-(gas$deltaCO2_atm*15.466)/0.085/gas$Temp_K #CO2 mol/min

  gas$FCO2<-(gas$n/domeFoot_m2)*60 #mol/m^2/h
  gas$KH<-0.034*exp(2400*((1/gas$Temp_K)-(1/298.15)))
  gas$KH_1000<-gas$KH*1000 #mol/m^3/atm

  gas$KCO2_dh<-gas$FCO2/gas$KH_1000/(gas$pCO2_air-gas$pCO2_water)#m/h
  gas$kO2_dh<-gas$KCO2_dh*(gas$SchmidtCO2hi/gas$SchmidtO2hi)^(-2/3)#m/h
  gas$k600_dh<- gas$KCO2_dh*(600/gas$SchmidtCO2hi)^(-2/3) #m/h

  gas$KO2_1d<-(gas$kO2_dh/gas$depth)*24
  gas$KCO2_1d<-(gas$KCO2_dh/gas$depth)*24
  gas$k600_1d<- (gas$k600_dh/gas$depth)*24

  gas<-gas%>% select(Date,day,ID,CO2,CO2_enviro,depth,Q,Temp,KCO2_dh,k600_dh)

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

write_csv(gasdome, "01_Raw_data/GD/GasDome_compiled_raw.csv")

gasdome<-read_csv("01_Raw_data/GD/GasDome_compiled_raw.csv")

gasdome_cleaned<-gasdome %>%
  filter(ID!='14')%>%distinct(day, ID, .keep_all = T)%>%select(-day)%>%
  arrange(ID, Date)%>%
  mutate(
    k600_dh=abs(k600_dh),
    KCO2_1d=abs(KCO2_dh),
    logQ=log(Q)) %>%
  mutate(log_K600=log(k600_dh))%>%
  mutate(
    sd=sd(k600_dh, na.rm = T),
    mean=mean(k600_dh, na.rm = T))


ggplot(gasdome_cleaned, aes(x = Q, y = k600_dh)) +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ ID, ncol = 5, scales = 'free') +
  scale_x_log10()+scale_y_log10()+
  theme_minimal() +
  theme(legend.position = "bottom")

write_csv(gasdome_cleaned, "01_Raw_data/GD/GasDome_compiled.csv")

split<-gasdome_cleaned %>% split(gasdome_cleaned$ID)
write.xlsx(split, file = '04_Output/rC_k600.xlsx')

#organize data file##########
gas<- read_csv("01_Raw_data/GD/raw/GasDome_04162025.dat",skip = 3)
gas<-gas[,c(1,5)]
colnames(gas)[1] <- "Date"
colnames(gas)[2] <- "CO2"
gas<-gas %>% mutate(CO2=CO2*6) %>%filter(Date>'2025-03-30')

ggplot(gas, aes(x=Date, y=CO2)) +geom_point()

write_csv(gas, "01_Raw_data/GD/raw/GasDome_04162025.csv")

#Visualize K600##########

file_path <- "04_Output/rC_k600.xlsx"  # Replace with your file path
sheet_names <- excel_sheets(file_path)

all_sheets <- lapply(sheet_names, read_excel, path = file_path)
combined_data <- bind_rows(all_sheets, .id = "sheet_name")


ggplot(combined_data, aes(x = depth, y = k600_dh)) +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ ID, ncol = 5, scales = 'free') +
  scale_x_log10()+scale_y_log10()+
  #ylab(expression('Discharge'~'ft'^3/sec))+xlab("Depth (m)")
  theme_minimal() +
  theme(legend.position = "bottom")
