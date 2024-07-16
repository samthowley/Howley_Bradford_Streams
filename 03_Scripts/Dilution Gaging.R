library(anytime)
library(tidyverse)
library(readxl)
library(tools)
library(openxlsx)
library(lme4)
library(mmand)
library(grwat)

###seperating DG######
clean_DG <- function(DG) {
  DG<-DG[,c(2,3,4)]
  colnames(DG)[1] <- "Date"
  colnames(DG)[2] <- "LowSpC"
  colnames(DG)[3] <- "FullSpC"
  DG$Date<-mdy_hms(DG$Date)

  DG$time<-strftime(DG$Date, format="%H:%M:%S", tz = "UTC")
  DG1<-DG %>%filter(time>start & time< end)
  return(DG1)}

DG <- read_csv("01_Raw_data/DG/raw/03252024.csv",
               skip = 1)

start<-'15:29:00'
end<-'15:40:00'

DG1<-clean_DG(DG)
ggplot(DG1, aes(Date,LowSpC)) + geom_line()

write_csv(DG1, '01_Raw_data/DG/seperated/03252024_9.csv')

#extract DG#####
notes<- read_csv("01_Raw_data/DG/Streams_dilution_gauging.csv",
                 col_types = cols(Date = col_date(format = "%m/%d/%Y")))
notes<-notes[,c(1,2,3,6)]
notes<-rename(notes, 'day'='Date', 'ID'='Site')

DG<-read.csv('01_Raw_data/DG/compiled_DG.csv')

DG$Date<-ymd_hms(DG$Date)
DG$day<-as.Date(DG$Date)
DG<-left_join(DG, notes, by=c('day','ID'))

DG<-DG %>% group_by(day,ID) %>% mutate(elapsed = as.numeric(Date-Date[1]))

DG <- DG %>%
  mutate(time_group =  case_when(elapsed <= 30 ~ "prior",elapsed >= 30 ~ "after"))%>%
  group_by(day,ID,time_group)%>%
  summarise(mean_prior = mean(LowSpC)) %>%ungroup() %>% filter(time_group== 'prior') %>%
  left_join(DG, prio_calc, by=c('day', 'ID'))

DG$SpC_cor<-(DG$LowSpC-DG$mean_prior)
DG$NaCl<-DG$SpC_cor*0.51

DG$tC<-DG$elapsed*DG$NaCl
DG$single_mass<-DG$NaCl*5
DG$total_mass<-cumsum(DG$single_mass)

DG <- DG %>%
  mutate(time_group =  case_when(elapsed <= 30 ~ "prior",elapsed >= 30 ~ "after"))%>%
  group_by(day,ID,time_group)%>%
  summarise(mean_prior = mean(LowSpC)) %>%ungroup() %>% filter(time_group== 'prior') %>%
  left_join(DG, prio_calc, by=c('day', 'ID'))

DG<-DG %>% arrange(day,ID)%>%group_by(day,ID)%>%mutate(total_mass = cumsum(single_mass))

DG <- DG %>% group_by(day,ID)%>% mutate(m_0= sum(NaCl, na.rm=T)*5)%>%
  mutate(m_1= sum(tC, na.rm = T)*5)

DG$t_star<-DG$m_1/DG$m_0
DG$u_mean<-DG$Reach_m/DG$t_star
DG$Q<-(DG$NaCl_g*1000)/DG$m_0

x<-c('Date','day','ID','Q','u_mean','NaCl_g','Reach_m','total_mass','m_0','m_1','t_star')
DG<-DG[,x]
DG$date<-as.Date(DG$Date)
DG <- DG[!duplicated(DG[c('date','ID')]),]
write_csv(DG, "04_Output/compiled_DG.csv")

#Calculate Q####
DG<-read_csv('04_Output/compiled_DG.csv')
DG<-DG %>%mutate(hr=hour(Date), day=day(Date),month=month(Date), yr=year(Date))

depth <- read_csv("02_Clean_data/depth.csv")
depth<-depth %>%mutate(hr=hour(Date), day=day(Date),month=month(Date), yr=year(Date))
depth<- depth %>% group_by(ID, day, month, yr) %>% mutate(depth_mean=mean(depth, na.rm=T))
depth[order(depth$ID,ymd_hms(depth$Date)),]

DG_rC<-left_join(DG, depth, by=c('ID', 'hr','day', 'month', 'yr'))
DG_rC <- DG_rC[!duplicated(DG_rC[c( 'date','ID')]),]
x<-c("date","ID","Q","u_mean" ,"depth_mean","m_0","m_1")
DG_rC<-DG_rC[,x]

DG_rC<- DG_rC %>% mutate(logQ=log10(Q),logh=log10(depth_mean))

ggplot(DG_rC, aes(depth_mean)) +
  geom_point(aes(y=Q))+facet_wrap(~ ID, ncol=5)


split<-DG_rC %>% split(DG_rC$ID)
write.xlsx(split, file = '04_Output/rC_DG.xlsx')

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

x<-c("Date","ID","Q")
depth<-depth[,x]

discharge <- depth %>% group_by(ID) %>%
  mutate(Qbase = gr_baseflow(Q, method = 'jakeman',a = 0.925, passes = 3))
discharge<-discharge %>% group_by(ID) %>% mutate(medianQbase=median(Qbase, na.rm=T))

discharge <- discharge %>%
  mutate(Q_ID= case_when(medianQbase <= Q~  'low',
    medianQbase > Q~ 'high'))

discharge$Qbase[discharge$Qbase<0]<-NA
discharge<-discharge %>% filter(ID!=14)

ggplot(discharge, aes(Date)) +
  geom_line(aes(y=Qbase, color='base'))+
  geom_line(aes(y=medianQbase, color='median'))+
  facet_wrap(~ ID, ncol=5)
##########
write_csv(discharge, "02_Clean_data/discharge.csv")
q<-read_csv("02_Clean_data/discharge.csv")
range(q$Date)
# ##### combined all data ####
DG_all<-data.frame()
file.names <- list.files(path="01_Raw_data/DG/seperated", pattern=".csv", full.names=TRUE)
for(i in file.names){
DG<-read_csv(i)
 DG$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][4]
DG_all<-rbind(DG_all, DG)
}
unique(DG_all$ID)
write_csv(DG_all, "01_Raw_data/DG/compiled_DG.csv")

