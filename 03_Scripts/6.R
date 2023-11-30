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


samplingperiod <- read_csv("samplingperiod.csv")
samplingperiod$Date <- mdy_hm(samplingperiod$Date)

###DO#######
file.names <- list.files(path="01_Raw_data/HOBO Excels/6/DO", pattern=".csv", full.names=TRUE)

DO_6_all <- data.frame()
for(fil in file.names){
  DO6 <- read_csv(fil,
                #col_types = c("skip","date", "numeric", "numeric"),
                skip= 1)
  colnames(DO6)[2] <- "Date"
  colnames(DO6)[3] <- "DO"
  colnames(DO6)[4] <- "Temp"
  DO6$Date <- mdy_hms(DO6$Date)
  DO6<-DO6[,-c(1)]
  DO_6_all <- rbind(DO_6_all, DO6)
}


DO_6_all$DO[DO_6_all$DO<0] <- 0.01
#DO_6_all<-filter(DO_6_all, DO<6)
S6<-left_join(samplingperiod, DO_6_all, by='Date')
ggplot(DO_6_all, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)

write_xlsx(DO_6_all, "02_Clean_data/6/DO.xlsx")

###SpC#####

file.names <- list.files(path="01_Raw_data/HOBO Excels/6/SpC", pattern=".csv", full.names=TRUE)

SpC_6_all <- data.frame()
for(fil in file.names){
  SpC6 <- read_csv(fil,
                   #col_types = c("skip","date", "numeric", "numeric"),
                   skip= 1)
  SpC6<-SpC6[,c(2,3)]
  colnames(SpC6)[1] <- "Date"
  colnames(SpC6)[2] <- "SpC"
  SpC6$Date <- mdy_hms(SpC6$Date)
  SpC_6_all <- rbind(SpC_6_all, SpC6)
}


SpC_6_all<- filter(SpC_6_all,SpC>50 & SpC<150)
S6<-left_join(S6, SpC_6_all, by='Date')

write_xlsx(SpC_6_all, "02_Clean_data/6/SpC.xlsx")

ggplot(S6, aes(x=Date))+
  geom_line(aes(y=SpC, color="SpC"), size=0.8)

####pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/6/pH", pattern=".xlsx", full.names=TRUE)

pH_6_all <- data.frame()
for(fil in file.names){
  pH6 <- read_xlsx(fil)
  pH6<-pH6[,c(2,5)]
  colnames(pH6)[1] <- "Date"
  colnames(pH6)[2] <- "pH"
  #pH6$Date <- mdy_hms(pH6$Date)
  pH_6_all <- rbind(pH_6_all, pH6)
}


#pH_6_all<- filter(pH_6_all, pH>-1)
S6<-left_join(S6, pH_6_all, by='Date')
ggplot(pH6, aes(x=Date))+
  geom_line(aes(y=pH, color="pH"), size=0.8)
write_xlsx(pH_6_all, "02_Clean_data/6/pH.xlsx")

####Lily Box#######
file.names <- list.files(path="01_Raw_data/Lily Box/csv/6", pattern=".csv", full.names=TRUE)

LB_6FDOM_csv <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB6<-LB6[,c(1,4)]
  colnames(LB6)[2] <- "FDOM"
  LB6<-filter(LB6,FDOM>1& FDOM< 300)
  LB_6FDOM_csv <- rbind(LB_6FDOM_csv, LB6) }

ggplot(LB_6FDOM_csv, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

file.names <- list.files(path="01_Raw_data/Lily Box/dat/6", pattern=".dat", full.names=TRUE)

LB_6FDOM_dat <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil, skip= 3)
  LB6<-LB6[,c(1,5)]

  colnames(LB6)[1] <- "Date"
  colnames(LB6)[2] <- "FDOM"
  LB6<-filter(LB6,FDOM>1)

  LB_6FDOM_dat <- rbind(LB_6FDOM_dat, LB6)}

ggplot(LB_6FDOM_dat, aes(x=Date))+
  geom_line(aes(y=FDOM), size=0.8)

LB6_FDOM<-rbind(LB_6FDOM_csv, LB_6FDOM_dat)
write_xlsx(LB6_FDOM, "02_Clean_data/6/FDOM.xlsx")




file.names <- list.files(path="01_Raw_data/Lily Box/csv/6", pattern=".csv", full.names=TRUE)

LB_6CO2_csv <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil,
                  col_types = cols("Date" = col_datetime(format = "%m/%d/%Y %H:%M"),
                                   "CO2" = col_number()))
  LB6<-LB6[,c(1,5)]
  colnames(LB6)[3] <- "CO2"
  LB6<-filter(LB6, CO2> 500)
  LB_6CO2_csv <- rbind(LB_6CO2_csv, LB6) }

ggplot(LB_6CO2_csv, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)




file.names <- list.files(path="01_Raw_data/Lily Box/dat/6", pattern=".dat", full.names=TRUE)

LB_6CO2_dat <- data.frame()
for(fil in file.names){
  LB6 <- read_csv(fil, skip= 5)
  LB6<-LB6[,c(1,4)]

  colnames(LB6)[1] <- "Date"
  colnames(LB6)[2] <- "CO2"
  LB6<-filter(LB6, CO2> 5000)
  LB_6CO2_dat <- rbind(LB_6CO2_dat, LB6)}

ggplot(LB_6CO2_dat, aes(x=Date))+
  geom_line(aes(y=CO2), size=0.8)

LB6_CO2<-rbind(LB_6CO2_csv,LB_6CO2_dat)

write_xlsx(LB6_CO2, "02_Clean_data/6/CO2.xlsx")

S6<-left_join(S6, LB6_FDOM, by='Date')
S6<-left_join(S6, LB6_CO2, by='Date')

S6<- S6 %>%
  mutate(Day= day(Date),
         Mon= month(Date),
         Year= year(Date))

###Stage#####

h6 <- read_excel("02_Clean_data/Calculated_Stage/Stream #6a.xlsx",
                  skip = 1)
x<-c("Water Depth (m)","Flow (L/s)","Year","Mon","Day" )
h6<-h6[,x]
S6<- left_join(S6, h6, by= c("Year","Mon","Day"))
S6<-rename(S6, "Stage"="Water Depth (m)",
            "Q"="Flow (L/s)")
S6<-filter(S6, Q>5)
S6$Site<-"6"
S6<-left_join(samplingperiod,S6)

write_xlsx(S6, "02_Clean_data/6.xlsx")

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

(a<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm))+
    scale_color_manual(values='orange')+
    theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab('DO mg/L')+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S6, aes(x=Date))+
  geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
  scale_color_manual(values='purple')+theme_sam+
  guides(color=guide_legend(title="")))


(e<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))





