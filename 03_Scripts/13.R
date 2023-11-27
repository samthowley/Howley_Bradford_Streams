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
library(rnoaa)
library(corrplot)
library("broom")
library(car)
library(imputeTS)
library(ggExtra)
library("devtools")
library(lubridate)

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_chemistry")
samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="HOBO Excels/13/DO", pattern=".csv", full.names=TRUE)

DO_13_all <- data.frame()
for(fil in file.names){
  DO13 <- read_csv(fil,
                #col_types = c("skip","date", "numeric", "numeric"),
                skip= 1)
  DO13<-DO13[,c(2,3,4)]
  colnames(DO13)[1] <- "Date"
  colnames(DO13)[2] <- "DO"
  colnames(DO13)[3] <- "Temp"
  DO13$Date <- mdy_hms(DO13$Date)
  DO_13_all <- rbind(DO_13_all, DO13)
}

DO_13_all$DO[DO_13_all$DO<0] <- 0.01
DO_13_all<- filter(DO_13_all, DO<6.5)
S13<-left_join(samplingperiod, DO_13_all, by='Date')

ggplot(DO_13_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)
###SpC#####

file.names <- list.files(path="HOBO Excels/13/SpC", pattern=".csv", full.names=TRUE)

SpC_13_all <- data.frame()
for(fil in file.names){
  SpC13 <- read_csv(fil,
                   #col_types = c("skip","date", "numeric", "numeric"),
                   skip= 1)
  SpC13<-SpC13[,c(2,3)]
  colnames(SpC13)[1] <- "Date"
  colnames(SpC13)[2] <- "SpC"
  SpC13$Date <- mdy_hms(SpC13$Date)
  SpC_13_all <- rbind(SpC_13_all, SpC13)
}


SpC_13_all<- filter(SpC_13_all, SpC> 50 & SpC<600)
S13<-left_join(S13, SpC_13_all, by='Date')
ggplot(SpC_13_all, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8)

####pH#####
file.names <- list.files(path="HOBO Excels/13/pH", pattern=".xlsx", full.names=TRUE)

pH_13_all <- data.frame()
for(fil in file.names){
  pH13 <- read_xlsx(fil)
  pH13<-pH13[,c(2,5)]
  colnames(pH13)[1] <- "Date"
  colnames(pH13)[2] <- "pH"
  #pH13$Date <- mdy_hms(pH13$Date)
  pH_13_all <- rbind(pH_13_all, pH13)
}


pH_13_all<- filter(pH_13_all, pH<7.5 & pH>4)
S13<-left_join(S13, pH_13_all, by='Date')
ggplot(pH_13_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8)

####Lily Box#######
file.names <- list.files(path="Lily Box/csv/13", pattern=".csv", full.names=TRUE)

LB_13FDOM_csv <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil, 
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                   "CO2" = col_number()))
  LB13<-LB13[,c(1,4)]
  colnames(LB13)[2] <- "FDOM"
  #LB13<-filter(LB13,FDOM>5)
  LB_13FDOM_csv <- rbind(LB_13FDOM_csv, LB13) }

ggplot(LB_13FDOM_csv, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

file.names <- list.files(path="Lily Box/dat/13", pattern=".dat", full.names=TRUE)

LB_13FDOM_dat <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil, skip= 3)
  LB13<-LB13[,c(1,6)]
  
  colnames(LB13)[1] <- "Date"
  colnames(LB13)[2] <- "FDOM"
  #LB13<-filter(LB13,FDOM>5)
  
  LB_13FDOM_dat <- rbind(LB_13FDOM_dat, LB13)}

ggplot(LB_13FDOM_dat, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

LB13_FDOM<-rbind(LB_13FDOM_csv, LB_13FDOM_dat)



file.names <- list.files(path="Lily Box/csv/13", pattern=".csv", full.names=TRUE)

LB_13CO2_csv <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil, 
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                   "CO2" = col_number()))
  LB13<-LB13[,c(1,5)]
  colnames(LB13)[3] <- "CO2"
  #LB13$CO2<-LB13$CO2-290
  LB_13CO2_csv <- rbind(LB_13CO2_csv, LB13) }

ggplot(LB_13CO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)


file.names <- list.files(path="Lily Box/dat/13", pattern=".dat", full.names=TRUE)

LB_13CO2_dat <- data.frame()
for(fil in file.names){
  LB13 <- read_csv(fil, skip= 5)
  LB13<-LB13[,c(1,4)]
  
  colnames(LB13)[1] <- "Date"
  colnames(LB13)[2] <- "CO2"
  #LB13$CO2<-LB13$CO2-290
  LB13<-filter(LB13, CO2> 500)
  LB_13CO2_dat <- rbind(LB_13CO2_dat, LB13)}

ggplot(LB_13CO2_dat, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

LB13_CO2<-rbind(LB_13CO2_csv,LB_13CO2_dat)

S13<-left_join(S13, LB13_FDOM, by='Date')
S13<-left_join(S13, LB13_CO2, by='Date')

ggplot(LB13, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

S13<- S13 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))
S13 <- S13[!duplicated(S13[c('Date')]),]

###Stage#####

h13 <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_H20_level/Calculated_Stage/Stream #13.xlsx", 
                 skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h13<-h13[,x]
S13<- left_join(S13, h13, by= c("Year","Mon","Day"))
S13<-rename(S13, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S13 <- S13[!duplicated(S13[c('Date')]),]
S13$Site<-'13'
S13<-filter(S13, Q>5)
write_xlsx(S13, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Masterfiles_latest/Stream Chemistry/13.xlsx")

#####Check and organize######

(a<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm))+
    scale_color_manual(values='orange')+
    theme(axis.text.x = element_text(size = 15, angle=0),
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
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(b<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab('DO mg/L')+
    scale_color_manual(values='blue')+
    theme(axis.text.x = element_text(size = 15, angle=0),
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
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(c<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+
    theme(axis.text.x = element_text(size = 15, angle=0),
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
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(d<-ggplot(S13, aes(x=Date))+
  geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
  scale_color_manual(values='purple')+
  theme(axis.text.x = element_text(size = 15, angle=0),
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
        panel.grid.minor = element_blank())+
  guides(color=guide_legend(title="")))


(e<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+
    theme(axis.text.x = element_text(size = 15, angle=0),
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
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(f<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+
    theme(axis.text.x = element_text(size = 15, angle=0),
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
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(f<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+
    theme(axis.text.x = element_text(size = 15, angle=0),
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
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

