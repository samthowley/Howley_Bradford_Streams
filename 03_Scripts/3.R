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

setwd('01_Raw_data')
samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="HOBO Excels/3/DO", pattern=".csv", full.names=TRUE)

DO_3_all <- data.frame()
for(fil in file.names){
  DO3 <- read_csv(fil,
                #col_types = c("skip","date", "numeric", "numeric"),
                skip= 1)
  DO3<-DO3[,c(2,3,4)]
  colnames(DO3)[1] <- "Date"
  colnames(DO3)[2] <- "DO"
  colnames(DO3)[3] <- "Temp"
  DO3$Date <- mdy_hms(DO3$Date)
  DO_3_all <- rbind(DO_3_all, DO3)
  DO_3_all <- DO_3_all[!duplicated(DO_3_all[c('Date')]),]
}

DO_3_all$DO[DO_3_all$DO<0] <- NA
S3<-left_join(samplingperiod, DO_3_all, by='Date')

ggplot(DO_3_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)+
  geom_hline(yintercept = 7)

write_xlsx(DO_3_all, "02_Clean_data/3/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/3/SpC", pattern=".csv", full.names=TRUE)

SpC_3_all <- data.frame()
for(fil in file.names){
  SpC3 <- read_csv(fil,
                   #col_types = c("skip","date", "numeric", "numeric"),
                   skip= 1)
  SpC3<-SpC3[,c(2,3)]
  colnames(SpC3)[1] <- "Date"
  colnames(SpC3)[2] <- "SpC"
  SpC3$Date <- mdy_hms(SpC3$Date)
  SpC_3_all <- rbind(SpC_3_all, SpC3)
}

SpC_3_all<-filter(SpC_3_all, SpC>50)
S3<-left_join(S3, SpC_3_all, by='Date')
ggplot(SpC_3_all, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8)

write_xlsx(SpC_3_all, "02_Clean_data/3/SpC.xlsx")

####pH#####
file.names <- list.files(path="RawHOBO Excels/3/pH", pattern=".xlsx", full.names=TRUE)

pH_3_all <- data.frame()
for(fil in file.names){
  pH3 <- read_xlsx(fil)
  pH3<-pH3[,c(2,5)]
  colnames(pH3)[1] <- "Date"
  colnames(pH3)[2] <- "pH"
  pH_3_all <- rbind(pH_3_all, pH3)
}

pH_3_all<-filter(pH_3_all, pH<6.2)
S3<-left_join(S3, pH_3_all, by='Date')
ggplot(pH_3_all, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8)

write_xlsx(pH_3_all, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_chemistry/Format/3/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="Lily Box/csv/3", pattern=".csv", full.names=TRUE)

LB_3FDOM_csv <- data.frame()
for(fil in file.names){
  LB3 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB3<-LB3[,c(1,4)]
  colnames(LB3)[2] <- "FDOM"
  LB_3FDOM_csv <- rbind(LB_3FDOM_csv, LB3) }

ggplot(LB_3FDOM_csv, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

file.names <- list.files(path="Lily Box/dat/3", pattern=".dat", full.names=TRUE)

LB_3FDOM_dat <- data.frame()
for(fil in file.names){
  LB3 <- read_csv(fil, skip= 3)
  LB3<-LB3[,c(1,6)]
  colnames(LB3)[1] <- "Date"
  colnames(LB3)[2] <- "FDOM"
  LB_3FDOM_dat <- rbind(LB_3FDOM_dat, LB3)}

ggplot(LB_3FDOM_dat, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

LB3_FDOM<-rbind(LB_3FDOM_csv, LB_3FDOM_dat)
LB3_FDOM<-filter(LB3_FDOM, FDOM>1)
write_xlsx(LB3_FDOM, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_chemistry/Format/3/FDOM.xlsx")


file.names <- list.files(path="Lily Box/csv/3", pattern=".csv", full.names=TRUE)

LB_3CO2_csv <- data.frame()
for(fil in file.names){
  LB3 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB3<-LB3[,c(1,5)]
  colnames(LB3)[3] <- "CO2"
  LB_3CO2_csv <- rbind(LB_3CO2_csv, LB3) }

file.names <- list.files(path="Lily Box/dat/3", pattern=".dat", full.names=TRUE)

LB_3CO2_dat <- data.frame()
for(fil in file.names){
  LB3 <- read_csv(fil, skip= 3)
  LB3<-LB3[,c(1,5)]

  colnames(LB3)[1] <- "Date"
  colnames(LB3)[2] <- "CO2"
  LB_3CO2_dat <- rbind(LB_3CO2_dat, LB3)}


LB3_CO2<-rbind(LB_3CO2_csv,LB_3CO2_dat)
LB3_CO2<-filter(LB3_CO2, CO2>500)
write_xlsx(LB3_CO2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_chemistry/Format/3/CO2.xlsx")

ggplot(LB3_CO2, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)


S3<-left_join(S3, LB3_FDOM, by='Date')
S3<-left_join(S3, LB3_CO2, by='Date')


ggplot(S3, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)+
  geom_line(aes(y=CO2), size=0.8)



S3<- S3 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

###Stage#####

h3 <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Streams/Stream_H20_level/Calculated_Stage/Stream #3.xlsx",
                       skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h3<-h3[,x]
S3<- left_join(S3, h3, by= c("Year","Mon","Day"))
S3<-rename(S3, "Stage"="Water Depth (m)",
           "Q"="Flow (L/s)")
S3<-filter(S3, Q>15)
S3 <- S3[!duplicated(S3[c('Date')]),]
S3$Site<-"3"
S3<-left_join(samplingperiod, S3)
#####Check and organize######


(g<-ggplot(S3, aes(x=Date))+
   geom_line(aes(y=Stage*1000, colour="stage"),  size=0.8)+
   geom_line(aes(y=Q, colour="Q"),  size=0.8)+
   geom_hline(yintercept = 15)+
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


(a<-ggplot(S3, aes(x=Date))+
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

(b<-ggplot(S3, aes(x=Date))+
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

(c<-ggplot(S3, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+
    geom_hline(yintercept = 50)+
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

(d<-ggplot(S3, aes(x=Date))+
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


(e<-ggplot(S3, aes(x=Date))+
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





plot_grid(b,g,c, ncol=1, align = 'v')
write_xlsx(S3, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project/Masterfiles_latest/Stream Chemistry/3.xlsx")
