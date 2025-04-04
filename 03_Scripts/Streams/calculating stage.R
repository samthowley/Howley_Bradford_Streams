library(anytime)
library(tidyverse)
library(readxl)

###calc stage#####
PT<-read_csv('01_Raw_data/PT/compiled_PT.csv')
PT<-PT %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

baro<-read_csv('01_Raw_data/PT/compiled_baro.csv')
baro<-baro %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))%>%select(-Date)
master<-left_join(PT, baro, by=c('region','hr', 'day', 'mnth', 'yr'))


master<-master %>% mutate(PT=if_else(ID=='3' & PT<=14.96, NA, PT),
                          PT=if_else(ID=='6' & Date>'2022-11-14', PT-0.6, PT),
                          PT=if_else(ID=='9' & Date>'2022-09-20' & Date<'2023-07-20' & PT<=14.85, NA, PT),
                          PT=if_else(ID=='13' & PT<=15.25, NA, PT))


master<-master %>% mutate(Water_press=PT-PTbaro)%>% mutate(sensor_depth=1000*Water_press/2.2/(2.54^2)/100, date=as.Date(Date))
master <- master %>%
  mutate(PL= case_when(ID== '3'  ~ 136,
                       ID== '5' ~ 147,
                       ID== '6' ~ 143,
                       ID== '7' ~ 134,
                       ID== '9' ~ 125,
                       ID== '13' ~ 141,
                       ID== '14' ~ 141,
                       ID== '15' ~ 139,
                       ID== '5a' ~ 142,
                       ID== '6a' ~ 142))

master <- master %>%
  mutate(PG= case_when(ID== '3'  ~ 108,
                       ID== '5' ~ 105,
                       ID== '6' ~ 113,
                       ID== '7' ~ 99,
                       ID== '9' ~ 109,
                       ID== '13' ~ 103,
                       ID== '14' ~ 110,
                       ID== '15' ~ 103,
                       ID== '5a' ~ 110,
                       ID== '6a' ~ 103))

master<-master %>% mutate(depth=sensor_depth-(PL-PG)/100)%>% distinct(Date, ID, .keep_all = T)


master<-master%>% mutate(depth=if_else(depth<0, NA, depth))


ggplot(master, aes(x=Date)) + geom_line(aes(y=depth))+facet_wrap(~ ID, ncol=5)+
  geom_hline(yintercept = 0)+ggtitle("Post- James")
#
master<-master[, c("Date","Temp_PT","depth","ID","Water_press")]
range(master$Date)
write_csv(master, "02_Clean_data/depth.csv")

###########

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

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,6,11,4)]
data <- lapply(file.names,function(x) {read_csv(x)})
merged_data <- reduce(data, left_join, by = c("ID", 'Date'))

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
                                            to=as.POSIXct("2025-04-03 00:00", tz="UTC"),by="hour")))
samplingperiod<- samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))
baro_all<-left_join(baro_all, samplingperiod, by=c('hr', 'day', 'mnth', 'yr'))

baro5<-baro_all%>%filter(ID=='5')%>%rename('PTbaro_5'='PTbaro')
baro6a<-baro_all%>%filter(ID=='6a')%>%rename('PTbaro_6a'='PTbaro')
coalesce<-full_join(baro5,baro6a, by=c('Date'))

coalesce$PTbaro_6a[coalesce$PTbaro_6a <14.4] <- NA
coalesce$PTbaro_6a[coalesce$PTbaro_6a >16 ] <- NA
coalesce$PTbaro_5[coalesce$PTbaro_5 >16 ] <- NA

coalesce$PTbaro_5 <- ifelse(is.na(coalesce$PTbaro_5), coalesce$PTbaro_6a, coalesce$PTbaro_5)
coalesce$PTbaro_6a <- ifelse(is.na(coalesce$PTbaro_6a), coalesce$PTbaro_5, coalesce$PTbaro_6a)

baro_5<-coalesce %>%select(Date,PTbaro_5)%>% rename('PTbaro'='PTbaro_5')%>%mutate(region='S')
baro_6a<-coalesce %>% select(Date,PTbaro_6a)%>%rename('PTbaro'='PTbaro_6a')%>%mutate(region='N')
compile_baro<-rbind(baro_5,baro_6a)

ggplot(compile_baro, aes(Date, PTbaro)) +
  geom_line() + facet_wrap(~ region, ncol=5)+
  geom_hline(yintercept = 14.5)
range(compile_baro$Date, na.rm=T)

write_csv(compile_baro, "01_Raw_data/PT/compiled_baro.csv")

