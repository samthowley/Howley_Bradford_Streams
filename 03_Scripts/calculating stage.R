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
PT<-PT %>% mutate(hour=hour(Date),day=day(Date), month=month(Date), year=year(Date))
baro<-read_csv('01_Raw_data/PT/compiled_baro.csv')
baro<-baro %>% mutate(hour=hour(Date),day=day(Date), month=month(Date), year=year(Date))
baro<-baro[,-1]
master<-left_join(PT, baro, by=c('hour','day','month','year','region'))

master$Water_press<-master$PT-master$PTbaro
master$sensor_depth<-1000*master$Water_press/2.2/(2.54^2)/100

#PG PL#####
master <- master %>%
  mutate(PG= case_when(ID== '3' ~ 136,
                       ID== '5' & Date>='2023-11-01' ~ 146,
                       ID== '5' & Date<='2023-11-01' ~ 147,
                       ID== '5a' & Date>='2023-11-01' ~ 143,
                       ID== '5a' & Date<='2023-11-01' ~ 142,
                       ID== '6' & Date>='2023-11-01' ~ 184,
                       ID== '6' & Date>='2022-04-06' & Date<='2023-11-01' ~ 179,
                       ID== '6' & Date<='2022-04-06' ~ 143,
                       ID== '7' & Date>='2022-11-15' ~ 134,
                       ID== '7' & Date<='2022-11-15' ~ 135,
                       ID== '9' & Date>='2023-11-01' ~ 122,
                       ID== '9' & Date>='2022-11-14' & Date<='2023-11-01'~ 125,
                       ID== '9' & Date>='2021-04-16' & Date<='2022-11-14'~ 142,
                       ID== '13' & Date>='2023-12-19' ~ 212,
                       ID== '13' & Date>='2022-09-20' & Date<='2023-12-19'~ 141,
                       ID== '13' & Date>='2021-04-16' & Date<='2022-09-20'~ 139,
                       ID== '14' & Date>='2023-11-01' ~ 137,
                       ID== '14' & Date>='2022-11-15' & Date<='2023-11-01'~ 136,
                       ID== '14' & Date>='2021-04-06' & Date<='2022-11-15'~ 141,
                       ID== '15' & Date>='2022-11-14' ~ 138,
                       ID== '15' & Date<='2022-11-14' ~ 139,
                       ID== '6a' ~ 142))
master <- master %>%
  mutate(PL= case_when(
    ID== '3' & Date>='2023-11-01' ~ 107,
    ID== '3' & Date<='2023-11-01'~ 108,
    ID== '5' & Date>='2023-11-01' ~ 98,
    ID== '5' & Date>='2022-11-14' & Date<='2023-11-01'~ 105,
    ID== '5' & Date>='2021-03-20' & Date<='2022-11-14'~ 102,
    ID== '5a' & Date>='2022-11-14' ~ 101,
    ID== '5a' & Date>='2021-03-20' & Date<='2022-11-14'~ 110,
    ID== '6' & Date>='2023-11-01' ~ 122,
    ID== '6' & Date>='2022-04-06' & Date<='2023-11-01' ~ 132,
    ID== '6' & Date<='2022-04-06' ~ 113,
    ID== '7' & Date>='2023-11-01' ~ 100,
    ID== '7' & Date>='2022-11-15' & Date<='2023-11-01'~ 99,
    ID== '7' & Date>='2021-04-06' & Date<='2022-11-15'~ 103,
    ID== '9' & Date>='2023-11-01' ~ 116,
    ID== '9' & Date<='2023-11-01'~ 109,
    ID== '13' & Date>='2023-12-19' ~ 182,
    ID== '13' & Date>='2022-09-20' & Date<='2023-12-19'~ 103,
    ID== '13' & Date>='2021-04-16' & Date<='2022-09-20'~ 60,
    ID== '14' & Date>='2023-11-01' ~ 110,
    ID== '14' & Date>='2022-11-15' & Date<='2023-11-01'~ 104,
    ID== '14' & Date>='2021-04-06' & Date<='2022-11-15'~ 110,
    ID== '15' & Date>='2023-11-01' ~ 104,
    ID== '15' & Date>='2022-11-14' & Date<='2023-11-01'~ 105,
    ID== '15' & Date>='2021-03-30' & Date<='2022-11-14'~ 103,
    ID== '6a' & Date>='2023-11-01' ~ 113,
    ID== '6a' & Date>='2022-11-15' & Date<='2023-11-01'~ 109,
    ID== '6a' & Date>='2021-04-06' & Date<='2022-11-15'~ 103))
###########

master$depth<-master$sensor_depth-(master$PL-master$PG)/100
master$flow<-433.35*(master$depth^2.5421)
x<-c("Date","Water_press","depth","ID")
master<-master[,x]

ggplot(master, aes(Date, depth)) + geom_line() + facet_wrap(~ ID, ncol=5)

write_csv(master, "02_Clean_data/Chem/depth.csv")

#Check PT##########

# file.names <- list.files(path="01_Raw_data/Hobo Excels/15/PT", pattern=".csv", full.names=TRUE)
# PT15<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   PT15<-rbind(PT15,PT)}
# PT15$ID<-'15'
#
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
# file.names <- list.files(path="01_Raw_data/Hobo Excels/5a/PT", pattern=".csv", full.names=TRUE)
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
# file.names <- list.files(path="01_Raw_data/baro/6a", pattern=".csv", full.names=TRUE)
# baro6a<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   baro6a<-rbind(baro6a,PT)}
# baro6a<-rename(baro6a, 'PTbaro'='PT')
# baro6a$region<-'N'
#
# file.names <- list.files(path="01_Raw_data/baro/5", pattern=".csv", full.names=TRUE)
# baro5<-data.frame()
# for(fil in file.names){
#   PT <- PT_unformatted(fil)
#   baro5<-rbind(baro5,PT)}
# baro5<-rename(baro5, 'PTbaro'='PT')
# baro5$region<-'S'
#
# compile_baro<-rbind(baro6a, baro5)
# compile_baro<-compile_baro %>% filter(PTbaro>14 & PTbaro<16)
#
# compile_baro<-compile_baro %>% mutate(day=day(Date),month=month(Date), year=year(Date), hour=hour(Date))
# compile_baro<-compile_baro %>% group_by(hour, day, month, year,region) %>% mutate(PTbaro= mean(PTbaro, na.rm=T))
# compile_baro <- compile_baro[!duplicated(compile_baro[c('hour','day','month','year','region')]),]
# compile_baro<-compile_baro[,c(1,2,3)]
#
# ggplot(compile_baro, aes(Date, PTbaro)) + geom_line() + facet_wrap(~ region, ncol=5)
#
# write_csv(compile_baro, "01_Raw_data/PT/compiled_baro.csv")

