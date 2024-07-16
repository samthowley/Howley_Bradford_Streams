library(anytime)
library(tidyverse)
library(readxl)

###calc stage#####
PT<-read_csv('01_Raw_data/PT/compiled_PT.csv')
PT<-PT %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
baro<-read_csv('01_Raw_data/PT/compiled_baro.csv')
baro<-baro %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

baro<-baro[,-1]
master<-left_join(PT, baro, by=c('region','hr', 'day', 'mnth', 'yr'))

master <-master %>% mutate(PT_clean = case_when(ID=='3' & PT<=14.96 ~ PT+0.37,
                                                     ID=='6' & Date>'2022-11-14' ~ PT-0.6,
                                                     ID=='13' & PT<15.25 ~ PT+0.2,
                                                     ID=='14' & PT<15.5 ~ NA,
                                                     ID=='5' & PT<15 ~ NA,
                                                     ID=='9' & Date>'2022-09-20' & Date<'2023-07-20' & PT<=14.85~ PT+0.35))

master$PT[master$ID=='3' & master$PT<=14.96] <- NA
master$PT[master$ID=='6' & master$Date>'2022-11-14'] <- NA
master$PT[master$ID=='9' & master$Date>'2022-09-20' & master$Date<'2023-07-20' & master$PT<=14.85] <- NA
master$PT[master$ID=='13' & master$PT<15.25] <- NA

master$PT <- ifelse(is.na(master$PT), master$PT_clean, master$PT)

master$Water_press<-master$PT-master$PTbaro
master$sensor_depth<-1000*master$Water_press/2.2/(2.54^2)/100
master$date<-as.Date(master$Date)
master <- master %>%
  mutate(PL= case_when(ID== '3' & date<='2022-11-15' ~ 137,
                       ID== '5' & date<='2022-11-14'~ 147,
                       ID== '6' & date<='2021-04-06'~ 118,
                       ID== '7' & date<='2022-11-15'~ 135,
                       ID== '9' & date<='2022-11-14'~ 142,
                       ID== '13' & date<='2021-09-20'~ 139,
                       ID== '14' & date<='2022-11-15'~ 141,
                       ID== '15' & date<='2022-11-15'~ 139,
                       ID== '5a'  & date<='2022-11-14'~ 142,
                       ID== '6a' & date<='2022-11-15'~ 142,

                       ID== '3' & date>='2022-11-15' & date<='2023-11-01' ~ 136,
                       ID== '5' & date>='2022-11-14' & date<='2023-11-11' ~ 147,
                       ID== '6' & date>='2021-04-06' & date<='2022-11-15'~ 143,
                       ID== '7' & date>='2022-11-15' & date<='2023-11-01' ~ 134,
                       ID== '9' & date>='2022-11-14' & date<='2023-11-01'~ 125,
                       ID== '13' & date>='2021-09-20'& date<='2023-12-19'~ 141,
                       ID== '14' & date>='2022-11-15'& date<='2023-11-01'~ 136,
                       ID== '15' & date>='2022-11-15'& date<='2023-11-01'~ 138,
                       ID== '5a' & date>='2022-11-14'& date<='2023-11-01'~ 142,
                       ID== '6a' & date>='2022-11-15'& date<='2023-11-01'~ 142,

                       ID== '6' & date>='2022-11-15' & date<='2023-11-01'~ 179,
                       ID== '13' & date>='2021-09-20' & date<='2022-11-15'~ 215,

                       ID== '3' & date>='2023-11-01' ~ 136,
                       ID== '5' & date>='2023-11-01' ~ 146,
                       ID== '6' & date>='2023-11-01' ~ 184,
                       ID== '7' & date>='2022-11-15' ~ 134,
                       ID== '9' & date>='2023-11-01' ~ 122,
                       ID== '13' & date>='2023-11-15' ~ 212,
                       ID== '14' & date>='2023-11-01' ~ 137,
                       ID== '15' & date>='2022-11-14' ~ 138,
                       ID== '5a' & date>='2023-11-01' ~ 143,
                       ID== '6a' & date>='2023-11-01' ~ 142))

master <- master %>%
  mutate(PG= case_when(ID== '3'& date<='2022-11-15' ~ 120,
                       ID== '5' & date<='2022-11-14'~ 105,
                       ID== '6' & date<='2021-04-06' ~ 117,
                       ID== '7' & date<='2022-11-15'~ 103,
                       ID== '9' & date<='2022-11-14'~ 109,
                       ID== '13' & date<='2021-09-20'~ 60,
                       ID== '14' & date<='2022-11-15'~ 110,
                       ID== '15' & date<='2022-11-15'~ 103,
                       ID== '5a'  & date<='2022-11-14'~ 110,
                       ID== '6a' & date<='2022-11-15'~ 103,

                       ID== '3' & date>='2022-11-15' & date<='2023-11-01' ~ 108,
                       ID== '5' & date>='2022-11-14' & date<='2023-11-11' ~ 102,
                       ID== '6' & date>='2021-04-06' & date<='2022-11-15'~ 113,
                       ID== '7' & date>='2022-11-15' & date<='2023-11-01' ~ 99,
                       ID== '9' & date>='2022-11-14' & date<='2023-11-01'~ 109,
                       ID== '13' & date>='2021-09-20'& date<='2023-12-19'~ 103,
                       ID== '14' & date>='2022-11-15'& date<='2023-11-01'~ 104,
                       ID== '15' & date>='2022-11-15'& date<='2023-11-01'~ 105,
                       ID== '5a' & date>='2022-11-14'& date<='2023-11-01'~ 101,
                       ID== '6a' & date>='2022-11-15'& date<='2023-11-01'~ 109,

                       ID== '6' & date>='2022-11-15' & date<='2023-11-01'~ 132,
                       ID== '13' & date>='2021-09-20' & date<='2022-11-15'~ 181,

                       ID== '3' & date>='2023-11-01' ~ 107,
                       ID== '5' & date>='2023-11-01' ~ 98,
                       ID== '6' & date>='2023-11-01' ~ 122,
                       ID== '7' & date>='2022-11-15' ~ 100,
                       ID== '9' & date>='2023-11-01' ~ 116,
                       ID== '13' & date>='2023-11-15' ~ 182,
                       ID== '14' & date>='2023-11-01' ~ 110,
                       ID== '15' & date>='2022-11-14' ~ 104,
                       ID== '5a' & date>='2023-11-01' ~ 101,
                       ID== '6a' & date>='2023-11-01' ~ 113))

master$depth<-master$sensor_depth-(master$PL-master$PG)/100
master <- master[!duplicated(master[c( 'Date','ID')]),]

master <-master %>% mutate(depth_clean = case_when(ID=='6' & Date>'2023-11-02' ~ depth+0.16,
                                                   ID=='9' & Date>'2023-11-02' ~ depth-0.1,
                                                   ID=='13' & Date>='2022-01-28 12:00:00' & Date<'2022-03-10'~ depth+0.16,
                                                   ID=='3'& Date<'2022-11-10' & depth<0.23 ~ depth+0.1,
                                                   ID=='6a'& Date>'2023-12-16 14:00:00' ~ depth-0.3,
                                                   ID=='5a'& Date>'2022-11-10 02:00:00'& Date<'2022-11-14 19:00:00' ~ depth-0.1,))

master$depth[master$ID=='5a'& master$Date>'2022-11-10 02:00:00'& master$Date<'2022-11-14 19:00:00'] <- NA
master$depth[master$ID=='6' & master$Date>'2023-11-02'] <- NA
master$depth[master$ID=='9' & master$Date>'2023-11-02'] <- NA
master$depth[master$ID=='13' & master$Date>'2022-01-26' & master$Date<'2022-03-10'] <- NA
master$depth[master$ID=='3'& master$Date<'2022-11-10' & master$depth<0.23] <- NA
master$depth[master$ID=='6a'& master$Date>'2023-12-16'] <- NA

master$depth <- ifelse(is.na(master$depth), master$depth_clean, master$depth)

master <-master %>% mutate(depth_clean = case_when(ID=='13' & Date>'2023-11-01 07:00:00'& Date<'2023-12-06 08:00:00'~ depth-0.12))
master$depth[master$ID=='13' & master$Date>'2023-11-01 07:00:00' & master$Date<'2023-12-06 08:00:00'] <- NA
master$depth <- ifelse(is.na(master$depth), master$depth_clean, master$depth)

master <-master %>% mutate(depth_clean = case_when(ID=='13' & Date>'2023-12-16 11:00:00'~ depth-0.2))
master$depth[master$ID=='13' & master$Date>'2023-12-16 11:00:00'] <- NA
master$depth <- ifelse(is.na(master$depth), master$depth_clean, master$depth)

master <- master %>% filter(ID=='13' & depth> -0.10|
                              ID=='14' & depth> 0.3|
                              ID=='5'& depth> -0.2|
                              ID=='5a'& depth> -0.35 & depth<0.82|
                              ID=='6'& depth> -0.2|
                              ID=='9'& depth>-0.2 |
                              ID=='3'| ID=='6a'|ID=='7'|ID=='15')


ggplot(master, aes(x=Date)) + geom_line(aes(y=depth))+facet_wrap(~ ID, ncol=5)

master<-master[, c("Date","Temp_PT","depth","ID","Water_press")]
range(master$Date)
###########
write_csv(master, "02_Clean_data/depth.csv")

#Compile PT##########
file.names <- list.files(path="01_Raw_data/PT/raw", pattern=".csv", full.names=TRUE)
PT_all<-data.frame()
 for(fil in file.names){
   PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
   PT<-PT[,c(1,2,3)]
   colnames(PT)[1] <- "Date"
   colnames(PT)[2] <- "PT"
   colnames(PT)[3] <- "Temp_PT"
   PT$Date <- mdy_hms(PT$Date)
   PT$ID<-strsplit(basename(fil), '_')[[1]][1]
  PT_all<-rbind(PT_all,PT)}


PT_all<- PT_all %>%filter(Date>'2020-01-01')
PT_all$PT[PT_all$PT<12]<-NA
PT_all$PT[PT_all$PT>17.5]<-NA
ggplot(PT_all, aes(Date, PT)) + geom_line() + facet_wrap(~ ID, ncol=5)

PT_all <-  PT_all %>%
  mutate(region= case_when(ID=="6"|ID=="6a"|ID=="3"|ID=="7"~ 'N',
                        ID=="5"|ID=="5a"|ID=="15"|ID=="9"|
                        ID=="14"|ID=="13"~ 'S'))

PT_all<-PT_all %>% mutate(day=day(Date),month=month(Date), year=year(Date), hour=hour(Date))
PT_all<-PT_all %>% group_by(hour, day, month, year,ID) %>% mutate(PT= mean(PT, na.rm=T))
PT_all <- PT_all[!duplicated(PT_all[c('Date','ID')]),]

PT_all<-PT_all[,c("Date","PT","Temp_PT","ID","region")]

range(PT_all$Date, na.rm=T)
write_csv(PT_all, "01_Raw_data/PT/compiled_PT.csv")

#compile baro######

file.names <- list.files(path="01_Raw_data/baro", pattern=".csv", full.names=TRUE)
baro_all<-data.frame()
for(fil in file.names){
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PTbaro"
  PT$Date <- mdy_hms(PT$Date)
  PT$ID<-strsplit(basename(fil), '_')[[1]][1]
  baro_all<-rbind(baro_all,PT)}

baro_all<-baro_all %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
baro_all<-baro_all[,-1]
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2021-03-29 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-07-31 00:00", tz="UTC"),by="hour")))
samplingperiod<- samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
baro_all<-left_join(baro_all, samplingperiod, by=c('hr', 'day', 'mnth', 'yr'))

baro5<-baro_all%>%filter(ID=='5')%>%rename('PTbaro_5'='PTbaro')
baro6a<-baro_all%>%filter(ID=='6a')%>%rename('PTbaro_6a'='PTbaro')
coalesce<-left_join(baro5,baro6a, by=c('Date'))

coalesce$PTbaro_6a[coalesce$PTbaro_6a <14.4] <- NA
coalesce$PTbaro_6a[coalesce$PTbaro_6a >16 ] <- NA
coalesce$PTbaro_5[coalesce$PTbaro_5 >16 ] <- NA

coalesce$PTbaro_5 <- ifelse(is.na(coalesce$PTbaro_5), coalesce$PTbaro_6a, coalesce$PTbaro_5)
coalesce$PTbaro_6a <- ifelse(is.na(coalesce$PTbaro_6a), coalesce$PTbaro_5, coalesce$PTbaro_6a)

baro_5<-coalesce[,c(7,1)]
baro_5<-baro_5 %>% rename('PTbaro'='PTbaro_5')%>%mutate(ID='5',region='S')
baro_6a<-coalesce[,c(7,8)]
baro_6a<-rename(baro_6a, 'PTbaro'='PTbaro_6a')%>%mutate(ID='5',region='N')
compile_baro<-rbind(baro_6a, baro_5)
compile_baro<-compile_baro[,-3]

ggplot(compile_baro, aes(Date, PTbaro)) +
  geom_line() + facet_wrap(~ region, ncol=5)+
  geom_hline(yintercept = 14.5)
range(compile_baro$Date, na.rm=T)

write_csv(compile_baro, "01_Raw_data/PT/compiled_baro.csv")

