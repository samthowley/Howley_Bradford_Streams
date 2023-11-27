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
########
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project")

file.names <- list.files(path="Masterfiles_latest/Stream Chemistry", pattern=".xlsx", full.names=TRUE)


FDOM_all <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil, 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "text"))
  FDOM<-FDOM[,c(1,6,13)]
  FDOM<-na.omit(FDOM)
  FDOM_all <- rbind(FDOM_all, FDOM)
}

FDOM_all$Site <- factor(FDOM_all$Site  , levels=c("3","5","5a",'6',"6a","7",
                                        "9", "13","14","15"))

FDOM<-ggplot(FDOM_all,aes(x=Site,y=FDOM))+
  geom_boxplot(outlier.color="black")+
  ggtitle("FDOM")+
  ylab("ppb QS")+
  theme(axis.text.x = element_text(size = 19, angle=0),
        axis.text.y = element_text(size = 19, angle=0),
        axis.title.y =element_text(size = 19, color = "black"),
        axis.title.x =element_text(size = 19),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



CO2_all <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "text"))
  CO2<-CO2[,c(1,7,13)]
  CO2<-na.omit(CO2)
  CO2_all <- rbind(CO2_all, CO2)
}

CO2_all$Site <- factor(CO2_all$Site, levels=c("3","5","5a",'6',"6a","7",
                                                  "9", "13","14","15"))

CO2<-ggplot(CO2_all,aes(x=Site,y=CO2))+
  geom_boxplot(outlier.color="black")+
  ggtitle(expression(paste(CO[2])))+
  ylab("ppm")+
  theme(axis.text.x = element_text(size = 19, angle=0),
        axis.text.y = element_text(size = 19, angle=0),
        axis.title.y =element_text(size = 19, color = "black"),
        axis.title.x =element_text(size = 19),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


pH_all <- data.frame()
for(fil in file.names){
  pH <- read_excel(fil, 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text"))
  pH<-pH[,c(1,5,13)]
  pH<-na.omit(pH)
  pH_all <- rbind(pH_all, pH)
}

pH_all$Site <- factor(pH_all$Site, levels=c("3","5","5a",'6',"6a","7",
                                              "9", "13","14","15"))

pH<-ggplot(pH_all,aes(x=Site,y=pH))+
  geom_boxplot(outlier.color="black")+
  ggtitle("pH")+
  ylab("pH")+
  theme(axis.text.x = element_text(size = 19, angle=0),
        axis.text.y = element_text(size = 19, angle=0),
        axis.title.y =element_text(size = 19, color = "black"),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


SpC_all <- data.frame()
for(fil in file.names){
  SpC <- read_excel(fil, 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "text"))
  SpC<-SpC[,c(1,4,13)]
  SpC<-na.omit(SpC)
  SpC_all <- rbind(SpC_all, SpC)
}

SpC_all$Site <- factor(SpC_all$Site, levels=c("3","5","5a",'6',"6a","7",
                                            "9", "13","14","15"))

SpC<-ggplot(SpC_all,aes(x=Site,y=SpC))+
  geom_boxplot(outlier.color="black")+
  ggtitle("SpC")+
  ylab("uS/cm")+
  theme(axis.text.x = element_text(size = 19, angle=0),
        axis.text.y = element_text(size = 19, angle=0),
        axis.title.y =element_text(size = 19, color = "black"),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


DO_all <- data.frame()
for(fil in file.names){
  DO <- read_excel(fil, 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text"))
  DO<-DO[,c(1,2,13)]
  DO<-na.omit(DO)
  DO_all <- rbind(DO_all, DO)
}

DO_all$Site <- factor(DO_all$Site, levels=c("3","5","5a",'6',"6a","7",
                                              "9", "13","14","15"))

DO<-ggplot(DO_all,aes(x=Site,y=DO))+
  geom_boxplot(outlier.color="black")+
  ggtitle("DO")+
  ylab("mg/L")+
  theme(axis.text.x = element_text(size = 19, angle=0),
        axis.text.y = element_text(size = 19, angle=0),
        axis.title.y =element_text(size = 19, color = "black"),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


box<-plot_grid(CO2,DO,pH, SpC, FDOM,ncol = 1, align = 'v')
ggsave(filename="boxplots.jpeg", 
       plot = box, 
       width =19, 
       height = 14.5, 
       units = "in")
