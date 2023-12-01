#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(cowplot)
library(readxl)
library(weathermetrics)
library(measurements)
library(dataRetrieval)
library('StreamMetabolism')
library("hydroTSM")
library("broom")
library(car)
library(imputeTS)
library(ggExtra)
library("devtools")
library(lubridate)

samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/14/DO", pattern=".csv", full.names=TRUE)

DO_14_all <- data.frame()
for(fil in file.names){
  DO14 <- read_csv(fil,
                #col_types = c("skip","date", "numeric", "numeric"),
                skip= 1)
  DO14<-DO14[,c(2,3,4)]
  colnames(DO14)[1] <- "Date"
  colnames(DO14)[2] <- "DO"
  colnames(DO14)[3] <- "Temp"
  DO14$Date <- mdy_hms(DO14$Date)
  DO_14_all <- rbind(DO_14_all, DO14)
}

DO_14_all$DO[DO_14_all$DO<0] <- 0.01
DO_14_all<- filter(DO_14_all, DO<7.5)
S14<-left_join(samplingperiod, DO_14_all, by='Date')

ggplot(DO_14_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)

write_xlsx(DO_14_all, "02_Clean_data/14/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/14/SpC", pattern=".csv", full.names=TRUE)

SpC_14_all <- data.frame()
for(fil in file.names){
  SpC14 <- read_csv(fil,
                   #col_types = c("skip","date", "numeric", "numeric"),
                   skip= 1)
  SpC14<-SpC14[,c(2,3)]
  colnames(SpC14)[1] <- "Date"
  colnames(SpC14)[2] <- "SpC"
  SpC14$Date <- mdy_hms(SpC14$Date)
  SpC_14_all <- rbind(SpC_14_all, SpC14)
}


SpC_14_all<- filter(SpC_14_all,SpC>50 & SpC<600)
S14<-left_join(S14, SpC_14_all, by='Date')
ggplot(SpC_14_all, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8)

write_xlsx(SpC_14_all, "02_Clean_data/14/SpC.xlsx")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/14/pH", pattern=".xlsx", full.names=TRUE)

pH_14_all <- data.frame()
for(fil in file.names){
  pH14 <- read_xlsx(fil)
  pH14<-pH14[,c(2,5)]
  colnames(pH14)[1] <- "Date"
  colnames(pH14)[2] <- "pH"
  #pH14$Date <- mdy_hms(pH14$Date)
  pH_14_all <- rbind(pH_14_all, pH14)
}


pH_14_all<- filter(pH_14_all,pH>4 & pH<7)
S14<-left_join(S14, pH_14_all, by='Date')
ggplot(pH_14_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8)

write_xlsx(pH_14_all, "02_Clean_data/14/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/14", pattern=".csv", full.names=TRUE)

LB_14FDOM_csv <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB14<-LB14[,c(1,4)]
  colnames(LB14)[2] <- "FDOM"
  LB14<-filter(LB14,FDOM>1)
  LB_14FDOM_csv <- rbind(LB_14FDOM_csv, LB14) }

ggplot(LB_14FDOM_csv, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

file.names <- list.files(path="01_Raw_data/Lily Box/dat/14", pattern=".dat", full.names=TRUE)

LB_14FDOM_dat <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil, skip= 3)
  LB14<-LB14[,c(1,5)]
  colnames(LB14)[1] <- "Date"
  colnames(LB14)[2] <- "FDOM"
  #LB14<-filter(LB14,FDOM>5)
  LB_14FDOM_dat <- rbind(LB_14FDOM_dat, LB14)}

LB14_FDOM<-rbind(LB_14FDOM_csv, LB_14FDOM_dat)

ggplot(LB14_FDOM, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)
write_xlsx(LB14_FDOM, "02_Clean_data/14/FDOM.xlsx")




file.names <- list.files(path="01_Raw_data/Lily Box/csv/14", pattern=".csv", full.names=TRUE)

LB_14CO2_csv <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB14<-LB14[,c(1,5)]
  colnames(LB14)[3] <- "CO2"
  LB14$CO2<-LB14$CO2-290.3
  LB14<-filter(LB14, CO2> 500)
  LB_14CO2_csv <- rbind(LB_14CO2_csv, LB14) }

ggplot(LB_14CO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)


file.names <- list.files(path="01_Raw_data/Lily Box/dat/14", pattern=".dat", full.names=TRUE)

LB_14CO2_dat <- data.frame()
for(fil in file.names){
  LB14 <- read_csv(fil, skip= 5)
  LB14<-LB14[,c(1,4)]

  colnames(LB14)[1] <- "Date"
  colnames(LB14)[2] <- "CO2"
  LB14$CO2<-LB14$CO2-290.3
  LB14<-filter(LB14, CO2> 600)
  LB_14CO2_dat <- rbind(LB_14CO2_dat, LB14)}

LB14_CO2<-rbind(LB_14CO2_csv,LB_14CO2_dat)

ggplot(LB14_CO2, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

write_xlsx(LB14_CO2, "02_Clean_data/14/CO2.xlsx")

S14<-left_join(S14, LB14_FDOM, by='Date')
S14<-left_join(S14, LB14_CO2, by='Date')

ggplot(LB14, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

S14<- S14 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

###Stage#####

h14 <- read_excel("02_Clean_data/Calculated_Stage/Stream #3.xlsx",
                 skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h14<-h14[,x]
S14<- left_join(S14, h14, by= c("Year","Mon","Day"))
S14<-rename(S14, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S14<-filter(S14, Q>0)
S14 <- S14[!duplicated(S14[c('Date')]),]
S14$Site<-"14"
write_xlsx(S14, "02_Clean_data/14.xlsx")

#####Check and organize######
theme_sam<-theme_minimal()+theme(axis.text.x = element_text(size = 15, angle=0),
                                 axis.text.y = element_text(size = 15, angle=0),
                                 axis.title.y =element_text(size = 15),
                                 axis.title.x =element_blank(),
                                 axis.title.y.right = element_text(),
                                 plot.title = element_text(size = 15, angle=0),
                                 legend.text=element_text(size=12),
                                 legend.title=element_text(size=12),
                                 legend.key.size = unit(0.5, "cm"),
                                 legend.position = 'none',
                                 panel.background = element_rect(fill = 'white'),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())


(a<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm))+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab('DO mg/L')+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S14, aes(x=Date))+
  geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
  scale_color_manual(values='purple')+theme_sam+
  guides(color=guide_legend(title="")))


(e<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(f<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

