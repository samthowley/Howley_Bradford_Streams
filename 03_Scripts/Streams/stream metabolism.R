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
#constants######
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2021-03-29 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-10-26 00:00", tz="UTC"),by="hour")))
samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))



#no k600
bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)


#k600
bins<- function(site) {
  site_positive<- site %>% filter(K600_1d>0)

  IQR<-quantile(site_positive$Q_m.s, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
  bin<-filter(site_positive, Q_m.s<=IQR[1])
  (Q<-mean(bin$Q_m.s))
  (K<-mean(bin$K600_1d))

  bin2<-filter(site_positive, Q_m.s<=IQR[2])
  (Q2<-mean(bin2$Q_m.s))
  (K2<-mean(bin2$K600_1d))

  bin3<-filter(site_positive, Q_m.s<=IQR[3])
  (Q3<-mean(bin3$Q_m.s))
  (K3<-mean(bin3$K600_1d, na.rm=T))

  bin4<-filter(site_positive, Q_m.s>=IQR[4])
  (Q4<-mean(bin4$Q_m.s))
  (K4<-mean(bin4$K600_1d, na.rm=T))

  bin5<-filter(site_positive, Q_m.s>=IQR[5])
  (Q5<-mean(bin5$Q_m.s))
  (K5<-mean(bin5$K600_1d, na.rm=T))

  bayes_specs <- specs(bayes_name,
                       K600_lnQ_nodes_centers = c(Q,Q2,Q3,Q4,Q5),
                       K600_lnQ_nodes_meanlog= log(c(K,K2,K3,K4,K5)),
                       K600_lnQ_nodes_sdlog= 0.1,
                       K600_lnQ_nodediffs_sdlog = 0.05,
                       K600_daily_sigma_sigma= 0.24,
                       burnin_steps=1000, saved_steps=1000)

  return(bayes_specs)}
bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)


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
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                         GPP_Rhat,ER_Rhat,K600_daily_Rhat)
  prediction2<- prediction2 %>% filter(ER_Rhat> 0.9 & ER_Rhat<1.05)%>% filter(K600_daily_Rhat> 0.9 & K600_daily_Rhat<1.05)%>%
    select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)

  return(prediction2)}
master <- read_csv("master.csv")

#K600 interpolation#####

GD_compiled<-read_csv('04_Output/GasDome_compiled_edited.csv')
split<-GD_compiled %>% split(GD_compiled$ID)

rC <- lmList(logQ ~ logh | ID, data=DG_rC)
(cf <- coef(rC))

depth<-read_csv('02_Clean_data/depth.csv')
depth <- depth %>%
  mutate(Q= case_when(
    ID== '13'~ (10^cf[1,1]) *depth^(cf[1,2]),
    ID== '14'~ (10^cf[2,1]) *depth^(cf[2,2]),
    ID== '15'~ (10^cf[3,1]) *depth^(cf[3,2]),
    ID== '3'~ (10^cf[4,1]) *depth^(cf[4,2]),
    ID== '5'~ (10^cf[5,1]) *depth^(cf[5,2]),
    ID== '5a'~ (10^cf[6,1]) *depth^(cf[6,2]),
    ID== '6'~ (10^cf[7,1]) *depth^(cf[7,2]),
    ID== '6a'~ (10^cf[8,1]) *depth^(cf[8,2]),
    ID== '7'~ (10^cf[9,1]) *depth^(cf[9,2]),
    ID== '9'~ (10^cf[10,1]) *depth^(cf[10,2])))

#model######
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
#############


write_csv(master, "04_Output/master_metabolism.csv")

discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(Date=as.Date(Date))
metabolism<-read_csv("04_Output/master_metabolism.csv")
metabolism<-left_join(metabolism, discharge, by=c('Date', 'ID'))
select<-metabolism %>% filter(ID %in% c('5','6','9'))
ggplot(select, aes(Q, ER)) + scale_x_log10()+
  geom_point(aes(y=ER, color='ER'))+
  geom_point(aes(y=GPP, color='GPP')) + facet_wrap(~ ID, ncol=3, scale='free')

b<-ggplot(master,aes(x=ID,y=ER))+
  geom_boxplot(outlier.color="black")+
  ggtitle("ER")+theme_sam

plot_grid(a,b, align = "v", ncol = 1, rel_heights = c(0.6,0.4))

