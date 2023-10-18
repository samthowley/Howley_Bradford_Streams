#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(lubridate)
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
library(zoo)
library(purrr)
library(ggExtra)

####get Data########

samplingperiod <- read_csv("C:/Howley_Bradford_Streams/samplingperiod.csv",
                           col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
setwd("C:/Howley_Bradford_Streams/HOBO Excels/5a/DO'")

file.names <- list.files(path="C:/Howley_Bradford_Streams/HOBO Excels/5a/DO'", pattern=".csv", full.names=TRUE)

DO5a<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/5a/DO'",
                    pattern="*.csv",
                    full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))
DO5a$Date <- mdy_hm(DO5a$Date)
DO5a$DO <- as.numeric(DO5a$DO)
DO5a$Temp <- as.numeric(DO5a$Temp)
DO5a<-DO5a[,c(2,3,4)]

DO5a<-left_join(samplingperiod, DO5a, by='Date')

ggplot(DO5a, aes(x=Date))+
  geom_line(aes(y=DO, color="DO"), size=0.8)
###SpC#####

file.names <- list.files(path="C:/Howley_Bradford_Streams/HOBO Excels/5a/SpC", pattern=".csv", full.names=TRUE)

SpC5a<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/5a/SpC",
                pattern="*.csv",
                full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))

SpC5a$"Date Time, GMT-05:00"  <- mdy_hm(SpC5a$"Date Time, GMT-05:00" )


SpC5a<-rename(SpC5a,
            "Date"="Date Time, GMT-05:00",
             "FullRangeSpC"=`Full Range, μS/cm (LGR S/N: 10778438, SEN S/N: 10778438)`,
             "Temp"="Temp, °F (LGR S/N: 10778438, SEN S/N: 10778438)")
SpC5a$FullRangeSpC <- as.numeric(SpC5a$FullRangeSpC)
SpC5a$Temp <- as.numeric(SpC5a$Temp)


SpC5a<-left_join(samplingperiod, SpC5a, by='Date')

ggplot(SpC5a, aes(x=Date))+
  geom_line(aes(y=FullRangeSpC), size=0.8)
####pH#####
pH5a<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/5a/pH",
                pattern="*.csv",
                full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))

pH5a$Date <- mdy_hm(pH5a$Date)

names(pH5a)
pH5a<-pH5a[,c(2,3,5)]
