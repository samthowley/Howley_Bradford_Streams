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
file.names <- list.files(path="HOBO Excels/15/DO", pattern=".csv", full.names=TRUE)

DO_15_all <- data.frame()
for(fil in file.names){
  DO15 <- read_csv(fil,
                #col_types = c("skip","date", "numeric", "numeric"),
                skip= 1)
  DO15<-DO15[,c(2,3,4)]
  colnames(DO15)[1] <- "Date"
  colnames(DO15)[2] <- "DO"
  colnames(DO15)[3] <- "Temp"
  DO15<-filter(DO15, DO>-800)
  DO15$Date <- mdy_hms(DO15$Date)
  DO_15_all <- rbind(DO_15_all, DO15)
}

DO_15_all$DO[DO_15_all$DO<0] <- 0.01
DO_15_all<- filter(DO_15_all, DO<6.5)
S15<-left_join(samplingperiod, DO_15_all, by='Date')

ggplot(DO_15_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)
###SpC#####

file.names <- list.files(path="HOBO Excels/15/SpC", pattern=".csv", full.names=TRUE)

SpC_15_all <- data.frame()
for(fil in file.names){
  SpC15 <- read_csv(fil,
                   #col_types = c("skip","date", "numeric", "numeric"),
                   skip= 1)
  SpC15<-SpC15[,c(2,3)]
  colnames(SpC15)[1] <- "Date"
  colnames(SpC15)[2] <- "SpC"
  SpC15$Date <- mdy_hms(SpC15$Date)
  SpC_15_all <- rbind(SpC_15_all, SpC15)
}


SpC_15_all<- filter(SpC_15_all, SpC>50 & SpC<300)
S15<-left_join(S15, SpC_15_all, by='Date')
ggplot(SpC_15_all, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8)

####pH#####
file.names <- list.files(path="HOBO Excels/15/pH", pattern=".xlsx", full.names=TRUE)

pH_15_all <- data.frame()
for(fil in file.names){
  pH15 <- read_xlsx(fil)
  pH15<-pH15[,c(2,5)]
  colnames(pH15)[1] <- "Date"
  colnames(pH15)[2] <- "pH"
  #pH15$Date <- mdy_hms(pH15$Date)
  pH_15_all <- rbind(pH_15_all, pH15)
}


pH_15_all<- filter(pH_15_all, pH<5)
S15<-left_join(S15, pH_15_all, by='Date')
ggplot(pH_15_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8)

####Lily Box#######
file.names <- list.files(path="Lily Box/csv/15", pattern=".csv", full.names=TRUE)

LB_15FDOM_csv <- data.frame()
for(fil in file.names){
  LB15 <- read_csv(fil, 
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                   "CO2" = col_number()))
  LB15<-LB15[,c(1,4)]
  colnames(LB15)[2] <- "FDOM"
  LB15<-filter(LB15,FDOM>5)
  LB_15FDOM_csv <- rbind(LB_15FDOM_csv, LB15) }

ggplot(LB_15FDOM_csv, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)


file.names <- list.files(path="Lily Box/dat/15", pattern=".dat", full.names=TRUE)

LB_15FDOM_dat <- data.frame()
for(fil in file.names){
  LB15 <- read_csv(fil, skip= 3)
  LB15<-LB15[,c(1,4)]
  colnames(LB15)[1] <- "Date"
  colnames(LB15)[2] <- "FDOM"
  #LB15<-filter(LB15,FDOM>1)
  LB_15FDOM_dat <- rbind(LB_15FDOM_dat, LB15)}

ggplot(LB_15FDOM_dat, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)


#LB_15CO2_dat <- data.frame()
#for(fil in file.names){
  #LB15 <- read_csv(fil, skip= 5)
  #LB15<-LB15[,c(1,4)]
  
  #colnames(LB15)[1] <- "Date"
  #colnames(LB15)[2] <- "CO2"
  #LB15<-filter(LB15, CO2> 600)
  #LB_15CO2_dat <- rbind(LB_15CO2_dat, LB15)}

#ggplot(LB_15CO2_dat, aes(x=Date))+
  #geom_line(aes(y=CO2), size=0.8)

#LB15dat<-left_join(LB_15FDOM_dat,LB_15CO2_dat)

LB15<-rbind(LB_15FDOM_csv, LB_15FDOM_dat)
S15<-left_join(S15, LB15, by='Date')
S15$CO2<-NA

ggplot(LB15, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)



S15<- S15 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

###Stage#####

h15 <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_H20_level/Calculated_Stage/Stream #3.xlsx", 
                 skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h15<-h15[,x]
S15<- left_join(S15, h15, by= c("Year","Mon","Day"))
S15<-rename(S15, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S15<-filter(S15, Q>5)
S15 <- S15[!duplicated(S15[c('Date')]),]
S15$Site<-'15'
write_xlsx(S15, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Masterfiles_latest/Stream Chemistry/15.xlsx")

#####Check and organize######

#(a<-ggplot(S15, aes(x=Date))+
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

(b<-ggplot(S15, aes(x=Date))+
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

(c<-ggplot(S15, aes(x=Date))+
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

(d<-ggplot(S15, aes(x=Date))+
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


(e<-ggplot(S15, aes(x=Date))+
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

(f<-ggplot(S15, aes(x=Date))+
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

(g<-ggplot(S15, aes(x=Date))+
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

