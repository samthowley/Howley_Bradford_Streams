rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)
library(lubridate)
library(streamMetabolizer)
library(weathermetrics)
library('StreamMetabolism')
#######

samplingperiod <- read_csv("samplingperiod.csv",col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))

bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)

# site <- read_csv("01_Raw_data/For StreamMetabolizer/3",
#              col_types = cols(...1 = col_skip()))
metabolism <- function(site) {
  site<-left_join(samplingperiod,site)
  site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp)
  x<-c("Date","DO","depth","Mouth_Temp_C")
  site<-site[,x]
  site <- na.omit(site)

  site<-rename(site,'DO.obs'='DO',
               'temp.water'='Mouth_Temp_C')
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site<-site[,-c(1)]
  site$light<-calc_light(site$solar.time,  29.8, -82.6)
  y<-c("DO.obs","depth","temp.water", "DO.sat","solar.time","light" )
  site<-site[,y]
  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
  return(prediction2)
}

master <- read_csv("02_Clean_data/master.csv")
split<-master %>% split(master$ID)
input<-"01_Raw_data/For StreamMetabolizer"
for(i in names(split)){
  write.csv(split[[i]],file.path(input,i))}

out<-"04_Output/Metabolism"
file.names <- list.files(path="01_Raw_data/For StreamMetabolizer", full.names=TRUE)
lapply(file.names, function(x) {
  t <- read_csv(x, col_types = cols(...1 = col_skip()))
  out <- metabolism(t) # apply function
  write_xlsx(out, paste0(destination_folder,"/", basename(x)))
})

