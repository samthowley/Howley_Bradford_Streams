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
setwd("C:/Howley_Bradford_Streams/HOBO Excels/3/DO")

file.names <- list.files(path="C:/Howley_Bradford_Streams/HOBO Excels/3/DO", pattern=".csv", full.names=TRUE)

DO3<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/3/DO",
                    pattern="*.csv",
                    full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))
DO3$Date <- mdy_hms(DO3$Date)
DO3<-DO3[,c(2,3,4)]

file.names <- list.files(path="C:/Howley_Bradford_Streams/HOBO Excels/3/SpC", pattern=".csv", full.names=TRUE)


###SpC#####
SpC3<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/3/SpC",
                pattern="*.csv",
                full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))

SpC3$Date <- mdy_hms(SpC3$Date)

SpC3<-rename(SpC3,
             "LowRangeSpC"=`Low Range, μS/cm (LGR S/N: 10778446, SEN S/N: 10778446)`,
             "FullRangeSpC"=`Full Range, μS/cm (LGR S/N: 10778446, SEN S/N: 10778446)`,
             "Temp"="Temp, °F (LGR S/N: 10778446, SEN S/N: 10778446)")
names(SpC3)
SpC3<-SpC3[,c(2,3,4,5)]
####pH#####
pH3<-list.files(path = "C:/Howley_Bradford_Streams/HOBO Excels/3/pH",
                pattern="*.csv",
                full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(somedat=gsub(".csv","",basename(x))))

pH3$Date <- mdy_hms(pH3$Date)

names(pH3)
pH3<-pH3[,c(2,3,5)]
