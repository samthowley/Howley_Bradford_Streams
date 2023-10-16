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
setwd("C:/Howley_Bradford_Streams/HOBO Excels/5/DO")

file.names <- list.files(path="C:/Howley_Bradford_Streams/HOBO Excels/5/DO", pattern=".csv", full.names=TRUE)

DO5<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/5/DO",
                    pattern="*.csv",
                    full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))
DO5$Date <- mdy_hms(DO5$Date)
DO5<-DO5[,c(2,5,4)]

###SpC#####

file.names <- list.files(path="C:/Howley_Bradford_Streams/HOBO Excels/5/SpC", pattern=".csv", full.names=TRUE)

SpC5<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/5/SpC",
                pattern="*.csv",
                full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))

SpC5$"Date Time, GMT-05:00"  <- mdy_hms(SpC5$"Date Time, GMT-05:00" )

SpC5_10738<-SpC5[,c(2,3,4)]
SpC5_10736<-SpC5[,c(2,6,7)]
SpC5_203<-SpC5[,c(2,9,10)]

SpC5_10738<-rename(SpC5_10738,
            "Date"="Date Time, GMT-05:00",
             "FullRangeSpC"=`Full Range, μS/cm (LGR S/N: 10778438, SEN S/N: 10778438)`,
             "Temp"="Temp, °F (LGR S/N: 10778438, SEN S/N: 10778438)")
SpC5_10736<-rename(SpC5_10736,
                   "Date"="Date Time, GMT-05:00",
                   "FullRangeSpC"=`Full Range, μS/cm (LGR S/N: 10778436, SEN S/N: 10778436)`,
                   "Temp"="Temp, °F (LGR S/N: 10778436, SEN S/N: 10778436)")

SpC5_203<-rename(SpC5_203,
                 "Date"="Date Time, GMT-05:00",
                 "FullRangeSpC"=`Full Range, μS/cm (LGR S/N: 20342071, SEN S/N: 20342071)`,
                 "Temp"="Temp, °F (LGR S/N: 20342071, SEN S/N: 20342071)")
SpC5_203<-na.omit(SpC5_203)
SpC5_10738<-na.omit(SpC5_10738)
SpC5_10736<-na.omit(SpC5_10736)

SpC5<-rbind(SpC5_203, SpC5_10738, SpC5_10736)

names(SpC5)
####pH#####
pH5<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/5/pH",
                pattern="*.csv",
                full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))

pH5$Date <- mdy_hms(pH5$Date)

names(pH5)
pH5<-pH5[,c(2,3,5)]
