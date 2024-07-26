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
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2021-03-29 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-02-05 00:00", tz="UTC"),by="hour")))
samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)

metabolism <- function(site) {

  site[order(as.Date(site$Date, format="%Y-%m-%d %H:%M:%S")),]
  site<-left_join(samplingperiod,site)
  site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp)
  x<-c("Date","DO","depth","Mouth_Temp_C")
  site<-site[,x]
  site <- site[!duplicated(site[c('Date')]),]

  site<-rename(site,'DO.obs'='DO','temp.water'='Mouth_Temp_C')
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site<-site[,-c(1)]
  site$light<-calc_light(site$solar.time,  29.8, -82.6)
  y<-c("DO.obs","depth","temp.water", "DO.sat","solar.time","light" )
  site<-site[,y]
  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
  return(prediction2)}
master <- read_csv("master.csv")

# split<-master %>% split(master$ID)
# input<-"01_Raw_data/For StreamMetabolizer"
# for(i in names(split)){
#   write.csv(split[[i]],file.path(input,i))}
#
# master<-data.frame()
# file.names <- list.files(path="01_Raw_data/For StreamMetabolizer", full.names=TRUE)
# lapply(file.names, function(x) {
#   t <- read_csv(x)
#   k <- metabolism(t) # apply function
#   master<-rbind(master, k)
# })
# write_csv(master, "04_Output/master_metabolism.csv")

###3#####
s3<-filter(master, ID=='3')
s3_ouput<-metabolism(s3)
s3_ouput$ID<-'3'

s5<-filter(master, ID=='5')
s5_ouput<-metabolism(s5)
s5_ouput$ID<-'5'

s6<-filter(master, ID=='6')
s6_ouput<-metabolism(s6)
s6_ouput$ID<-'6' #NOT WORKING

s6a<-filter(master, ID=='6a')
s6a_ouput<-metabolism(s6a)
s6a_ouput$ID<-'6a'

s7<-filter(master, ID=='7')
s7_ouput<-metabolism(s7)
s7_ouput$ID<-'7'

s9<-filter(master, ID=='9')
s9_ouput<-metabolism(s9)
s9_ouput$ID<-'9'

s13<-filter(master, ID=='13')
s13_ouput<-metabolism(s13)
s13_ouput$ID<-'13'

s14<-filter(master, ID=='14')
s14_ouput<-metabolism(s14)
s14_ouput$ID<-'14'

s15<-filter(master, ID=='15')
s15_ouput<-metabolism(s15)
s15_ouput$ID<-'15'

s5a<-filter(master, ID=='5a')
s5a_ouput<-metabolism(s5a)
s5a_ouput$ID<-'5a'

master<-rbind(s3_ouput, s5_ouput, s5a_ouput, s6_ouput,s6a_ouput, s7_ouput, s9_ouput,
              s13_ouput,s15_ouput)
master<- master %>% rename('ER'="ER_daily_mean", 'GPP'="GPP_daily_mean", 'Date'='date')

master<-master %>% filter(Date> "2023-06-16") %>%filter(ER>-30)

write_csv(master, "04_Output/master_metabolism.csv")

a<-ggplot(master, aes(Date, ER)) + geom_line() + facet_wrap(~ ID, ncol=5)

b<-ggplot(master,aes(x=ID,y=ER))+
  geom_boxplot(outlier.color="black")+
  ggtitle("ER")+theme_sam

plot_grid(a,b, align = "v", ncol = 1, rel_heights = c(0.6,0.4))

