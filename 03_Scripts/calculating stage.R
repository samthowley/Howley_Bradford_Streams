library(anytime)
library(tidyverse)
library(readxl)
PT_unformatted <- function(fil) {
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$Date <- mdy_hms(PT$Date)
  #PT<-PT %>% mutate(min=minute(Date)) %>% filter(min==0)

  return(PT)}

###calc stage#####
PT<-read_csv('01_Raw_data/PT/compiled_PT.csv')
PT<-PT %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
baro<-read_csv('01_Raw_data/PT/compiled_baro.csv')
baro<-baro %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

baro<-baro[,-1]
master<-left_join(PT, baro, by=c('region','hr', 'day', 'mnth', 'yr'))

master$Water_press<-master$PT-master$PTbaro
master$sensor_depth<-1000*master$Water_press/2.2/(2.54^2)/100
master$date<-as.Date(master$Date)

master <- master %>%
  mutate(PG= case_when(ID== '3' ~ 136,

                       ID== '5' ~ 147,
                       ID== '5' & date>='2023-11-01' ~ 146,

                       ID== '5a'  ~ 142,
                       ID== '5a' & date>='2023-11-01' ~ 143,

                       ID== '6'  ~ 143,
                       ID== '6' & date>='2022-04-06' & date<='2023-11-01' ~ 179,
                       ID== '6' & date>='2023-11-01' ~ 184,

                       ID== '7' ~ 135,
                       ID== '7' & date>='2022-11-15' ~ 134,

                       ID== '9' ~ 142,
                       ID== '9' & date>='2022-11-14' & date<='2023-11-01'~ 125,
                       ID== '9' & date>='2023-11-01' ~ 122,

                       ID== '13' ~ 139,
                       ID== '13' & date>='2022-09-20' & date<='2023-12-19'~ 141,
                       ID== '13' & date>='2023-12-19' ~ 212,

                       ID== '14' ~ 141,
                       ID== '14' & date>='2022-11-15' & date<='2023-11-01'~ 136,
                       ID== '14' & date>='2023-11-01' ~ 137,

                       ID== '15' ~ 139,
                       ID== '15' & date>='2022-11-14' ~ 138,
                       ID== '6a' ~ 142))
master <- master %>%
  mutate(PL= case_when(
    ID== '3'~ 108,
    ID== '3' & date>='2023-11-01' ~ 107,

    ID== '5' ~ 102,
    ID== '5' & date>='2022-11-14' & date<='2023-11-01'~ 105,
    ID== '5' & date>='2023-11-01' ~ 98,


    ID== '5a'  ~ 110,
    ID== '5a' & date>='2021-03-20' & date<='2022-11-14'~ 110,
    ID== '5a' & date>='2022-11-14' ~ 101,

    ID== '6' ~ 113,
    ID== '6' & date>='2022-04-06' & date<='2023-11-01' ~ 132,
    ID== '6' & date>='2023-11-01' ~ 122,

    ID== '7' ~ 103,
    ID== '7' & date>='2022-11-15' & date<='2023-11-01'~ 99,
    ID== '7' & date>='2023-11-01' ~ 100,

    ID== '9' ~ 109,
    ID== '9' & date>='2023-11-01' ~ 116,

    ID== '13' ~ 60,
    ID== '13' & date>='2022-09-20' & date<='2023-12-19'~ 103,
    ID== '13' & date>='2023-12-19' ~ 182,

    ID== '14' ~ 110,
    ID== '14' & date>='2022-11-15' & date<='2023-11-01'~ 104,
    ID== '14' & date>='2023-11-01' ~ 110,


    ID== '15' ~ 103,
    ID== '15' & date>='2022-11-14' & date<='2023-11-01'~ 105,
    ID== '15' & date>='2023-11-01' ~ 104,

    ID== '6a' ~ 103,
    ID== '6a' & date>='2022-11-15' & date<='2023-11-01'~ 109,
    ID== '6a' & date>='2023-11-01' ~ 113))

master$depth<-master$sensor_depth-(master$PL-master$PG)/100
master$flow<-433.35*(master$depth^2.5421)
x<-c("Date","Water_press","depth","ID")
master<-master[,x]
master <- master[!duplicated(master[c( 'Date','ID')]),]

ggplot(master, aes(Date, depth)) + geom_line() + facet_wrap(~ ID, ncol=5)

write_csv(master, "02_Clean_data/depth.csv")

#Check PT##########

# file.names <- list.files(path="01_Raw_data/PT/15", pattern=".csv", full.names=TRUE)
# PT15<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT15<-rbind(PT15,PT)}
# ggplot(PT15, aes(Date, PT)) + geom_line()
# PT15$ID<-'15'
# #
# file.names <- list.files(path="01_Raw_data/Hobo Excels/14/PT", pattern=".csv", full.names=TRUE)
# PT14<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT14<-rbind(PT14,PT)}
# PT14$ID<-'14'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/13/PT", pattern=".csv", full.names=TRUE)
# PT13<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT13<-rbind(PT13,PT)}
# PT13$ID<-'13'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/9/PT", pattern=".csv", full.names=TRUE)
# PT9<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT9<-rbind(PT9,PT)}
# PT9$ID<-'9'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/7/PT", pattern=".csv", full.names=TRUE)
# PT7<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT7<-rbind(PT7,PT)}
# PT7$ID<-'7'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/6a/PT", pattern=".csv", full.names=TRUE)
# PT6a<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT6a<-rbind(PT6a,PT)}
# PT6a$ID<-'6a'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/6/PT", pattern=".csv", full.names=TRUE)
# PT6<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT6<-rbind(PT6,PT)}
# PT6$ID<-'6'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/5/PT", pattern=".csv", full.names=TRUE)
# PT5<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT5<-rbind(PT5,PT)}
# PT5$ID<-'5'
#
# file.names <- list.files(path="01_Raw_data/PT/5a", pattern=".csv", full.names=TRUE)
# PT5a<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT5a<-rbind(PT5a,PT)}
# PT5a$ID<-'5a'
#
# file.names <- list.files(path="01_Raw_data/Hobo Excels/3/PT", pattern=".csv", full.names=TRUE)
# PT3<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT3<-rbind(PT3,PT)}
# PT3$ID<-'3'
#
# N<-rbind(PT6,PT6a,PT3,PT7)
# N$region<-'N'
# S<-rbind(PT5,PT5a, PT15, PT9, PT14,PT13)
# S$region<-'S'
# S<-S%>% filter(Date> '2021-03-01')
# compile<-rbind(N,S)
#
# compile<-compile %>% mutate(day=day(Date),month=month(Date), year=year(Date), hour=hour(Date))
# compile<-compile %>% group_by(hour, day, month, year,ID) %>% mutate(PT= mean(PT, na.rm=T)) %>% filter(PT>10)
# compile <- compile[!duplicated(compile[c('hour','day','month','year','ID')]),]
# compile<-compile[,c(1,2,3,4)]
# write_csv(compile, "01_Raw_data/PT/compiled_PT.csv")
#
# ggplot(compile, aes(Date, PT)) + geom_line() + facet_wrap(~ ID, ncol=5)

#baro######

# samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2021-03-29 00:00", tz="UTC"),
#                                             to=as.POSIXct("2024-03-29 00:00", tz="UTC"),by="hour")))
# samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
#
#
# file.names <- list.files(path="01_Raw_data/baro/6a", pattern=".csv", full.names=TRUE)
# baro6a<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   baro6a<-rbind(baro6a,PT)}
# baro6a<-rename(baro6a, 'PTbaro_6a'='PT')
# baro6a<-baro6a %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
# baro6a<-baro6a[,-1]
#
# file.names <- list.files(path="01_Raw_data/baro/5", pattern=".csv", full.names=TRUE)
# baro5<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   baro5<-rbind(baro5,PT)}
# baro5<-rename(baro5, 'PTbaro_5'='PT')
# baro5<-baro5 %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
# baro5<-baro5[,-1]
#
# coalesce<-left_join(samplingperiod, baro6a, by=c('hr', 'day', 'mnth', 'yr'))
# coalesce<-left_join(coalesce, baro5, by=c('hr', 'day', 'mnth', 'yr'))
#
#
# coalesce$PTbaro_6a[coalesce$PTbaro_6a <14.4] <- NA
# coalesce$PTbaro_6a[coalesce$PTbaro_6a >16 ] <- NA
# coalesce$PTbaro_5[coalesce$PTbaro_5 >16 ] <- NA
#
# coalesce$PTbaro_5 <- ifelse(is.na(coalesce$PTbaro_5), coalesce$PTbaro_6a, coalesce$PTbaro_5)
# coalesce$PTbaro_6a <- ifelse(is.na(coalesce$PTbaro_6a), coalesce$PTbaro_5, coalesce$PTbaro_6a)
#
# ggplot(coalesce, aes(x=Date)) +
#   geom_line(aes(y=PTbaro_5, color="5"))+
#   geom_line(aes(y=PTbaro_6a-1, color='6a'))
#
# baro_5<-coalesce[,c(1,7)]
# baro_5<-rename(baro_5, 'PTbaro'='PTbaro_5')
# baro_6a<-coalesce[,c(1,6)]
# baro_6a<-rename(baro_6a, 'PTbaro'='PTbaro_6a')
#
# baro_6a$region<-'N'
# baro_5$region<-'S'
# compile_baro<-rbind(baro_6a, baro_5)
#
# ggplot(compile_baro, aes(Date, PTbaro)) + geom_line() + facet_wrap(~ region, ncol=5)
#
# write_csv(compile_baro, "01_Raw_data/PT/compiled_baro.csv")
#
