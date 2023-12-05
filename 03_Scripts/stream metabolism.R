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



bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)

metabolism <- function(site) {
  samplingperiod <- read_csv("samplingperiod.csv", col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
  site<-left_join(samplingperiod,site)
  site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp)
  x<-c("Date","DO","Stage","Mouth_Temp_C")
  site<-site[,x]

  site<-rename(site,'DO.obs'='DO',
               'depth'='Stage',
               'temp.water'='Mouth_Temp_C')
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site<-site[,-c(1)]
  site$light<-calc_light(site$solar.time,  29.8, -82.6)
  y<-c("DO.obs","depth","temp.water", "DO.sat","solar.time","light" )
  site<-site[,y]
  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
  return(site)
}

file.names <- list.files(path="02_Clean_data",pattern="xlsx", full.names=TRUE)

destination_folder <- "04_Output"

lapply(file.names, function(x) {
  t <- read_excel(x)
  out <- metabolism(t) # apply function
  write_xlsx(out, paste0(destination_folder,"/", basename(x)))
})

