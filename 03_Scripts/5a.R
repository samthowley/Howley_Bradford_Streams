#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(readxl)
library(corrplot)
library("broom")
library(car)
library(imputeTS)
library(ggExtra)
library(lubridate)

#####setwd####
samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/5a/DO", pattern=".csv", full.names=TRUE)

DO_5a_all <- data.frame()
for(fil in file.names){
  DO5a <- read_csv(fil,
                #col_types = c("skip","date", "numeric", "numeric"),
                skip= 1)
  DO5a<-DO5a[,c(2,3,4)]
  colnames(DO5a)[1] <- "Date"
  colnames(DO5a)[2] <- "DO"
  colnames(DO5a)[3] <- "Temp"
  DO5a$Date <- mdy_hms(DO5a$Date)
  DO_5a_all <- rbind(DO_5a_all, DO5a)
}

DO_5a_all$DO[DO_5a_all$DO<0] <- NA
DO_5a_all<- filter(DO_5a_all, DO<7.4)
S5a<-left_join(samplingperiod, DO_5a_all, by='Date')

ggplot(DO_5a_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)+
  geom_hline(yintercept = 7.4)

write_xlsx(DO_5a_all, "02_Clean_data/5a/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/5a/SpC", pattern=".csv", full.names=TRUE)

SpC_5a_all <- data.frame()
for(fil in file.names){
  SpC5a <- read_csv(fil,
                   #col_types = c("skip","date", "numeric", "numeric"),
                   skip= 1)
  SpC5a<-SpC5a[,c(2,3)]
  colnames(SpC5a)[1] <- "Date"
  colnames(SpC5a)[2] <- "SpC"
  SpC5a$Date <- mdy_hms(SpC5a$Date)
  SpC_5a_all <- rbind(SpC_5a_all, SpC5a)
}


SpC_5a_all<- filter(SpC_5a_all, SpC>50 & SpC<300)
S5a<-left_join(S5a, SpC_5a_all, by='Date')
ggplot(SpC_5a_all, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8)

write_xlsx(SpC_5a_all, "02_Clean_data/5a/SpC.xlsx")

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/5a/pH", pattern=".xlsx", full.names=TRUE)

pH_5a_all <- data.frame()
for(fil in file.names){
  pH5a <- read_xlsx(fil)
  pH5a<-pH5a[,c(2,5)]
  colnames(pH5a)[1] <- "Date"
  colnames(pH5a)[2] <- "pH"
  #pH5a$Date <- mdy_hms(pH5a$Date)
  pH_5a_all <- rbind(pH_5a_all, pH5a)
}


pH_5a_all<- filter(pH_5a_all, pH<4.5)
S5a<-left_join(S5a, pH_5a_all, by='Date')
ggplot(pH_5a_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8)

write_xlsx(pH_5a_all, "02_Clean_data/5a/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/5a", pattern=".csv", full.names=TRUE)

LB_5aFDOM_csv <- data.frame()
for(fil in file.names){
  LB5a <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB5a<-LB5a[,c(1,4)]
  colnames(LB5a)[2] <- "FDOM"
  LB5a<-filter(LB5a,FDOM>5 &FDOM<400)
  LB_5aFDOM_csv <- rbind(LB_5aFDOM_csv, LB5a) }

ggplot(LB_5aFDOM_csv, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

file.names <- list.files(path="01_Raw_data/Lily Box/dat/5a", pattern=".dat", full.names=TRUE)

LB_5aFDOM_dat <- data.frame()
for(fil in file.names){
  LB5a <- read_csv(fil, skip= 3)
  LB5a<-LB5a[,c(1,6)]

  colnames(LB5a)[1] <- "Date"
  colnames(LB5a)[2] <- "FDOM"
  LB5a<-filter(LB5a,FDOM>5)

  LB_5aFDOM_dat <- rbind(LB_5aFDOM_dat, LB5a)}
ggplot(LB_5aFDOM_dat, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

LB5aFDOM<-rbind(LB_5aFDOM_csv,LB_5aFDOM_dat)
write_xlsx(LB5aFDOM, "02_Clean_data/5a/FDOM.xlsx")

file.names <- list.files(path="01_Raw_data/Lily Box/csv/5a", pattern=".csv", full.names=TRUE)

LB_5aCO2_csv <- data.frame()
for(fil in file.names){
  LB5a <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB5a<-LB5a[,c(1,5)]
  colnames(LB5a)[3] <- "CO2"
  LB5a<-filter(LB5a, CO2> 500)
  LB_5aCO2_csv <- rbind(LB_5aCO2_csv, LB5a) }

ggplot(LB_5aCO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

file.names <- list.files(path="01_Raw_data/Lily Box/dat/5a", pattern=".dat", full.names=TRUE)

LB_5aCO2_dat <- data.frame()
for(fil in file.names){
  LB5a <- read_csv(fil, skip= 5)
  LB5a<-LB5a[,c(1,4)]

  colnames(LB5a)[1] <- "Date"
  colnames(LB5a)[2] <- "CO2"
  LB5a<-filter(LB5a, CO2> 500)
  LB_5aCO2_dat <- rbind(LB_5aCO2_dat, LB5a)}

LB5aCO2<-rbind(LB_5aCO2_dat,LB_5aCO2_dat)

write_xlsx(LB5aCO2, "02_Clean_data/5a/CO2.xlsx")

ggplot(LB5aCO2, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)


S5a<-left_join(S5a, LB5aFDOM, by='Date')
S5a<-left_join(S5a, LB5aCO2, by='Date')

ggplot(S5a, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

S5a<- S5a %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

###Stage#####

h5a <- read_excel("02_Clean_data/Calculated_Stage/Stream #5a.xlsx",
                       skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h5a<-h5a[,x]
S5a<- left_join(S5a, h5a, by= c("Year","Mon","Day"))
S5a<-rename(S5a, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S5a<-filter(S5a, Q>5)
S5a <- S5a[!duplicated(S5a[c('Date')]),]
S5a$Site<-'5a'
S5a<-left_join(samplingperiod,S5a)
write_xlsx(S5a, "02_Clean_data/5a.xlsx")

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
(a<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm))+
    scale_color_manual(values='orange')+
    theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab('DO mg/L')+
    scale_color_manual(values='blue')+
    geom_hline(yintercept = 7.0)+
    theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+
    theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S5a, aes(x=Date))+
  geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
  scale_color_manual(values='purple')+
    theme_sam+
  guides(color=guide_legend(title="")))
mean(S5a$FDOM, na.rm = T)

(e<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+
    theme_sam+
    guides(color=guide_legend(title="")))

(f<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+
    theme_sam+
    guides(color=guide_legend(title="")))

(f<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+
    theme_sam+
    guides(color=guide_legend(title="")))

write_xlsx(S5a, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Masterfiles_latest/Stream Chemistry/5a.xlsx")

