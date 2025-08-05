#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(plotly)

samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2024-05-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2025-07-02 00:00", tz="UTC"),by="hour")))
theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 17, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_text(size = 17),
                             legend.position ="none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
#CO2#####
CO2<-data.frame()

file.names <- list.files(path="01_Raw_data/Lily Box/csv", pattern=".csv", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i)
  LB<-LB[,c(1,5)]
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][4]
  #LB<-LB %>% filter(CO2>1000)
  CO2<-rbind(CO2, LB)}
CO2<-CO2%>%mutate(Date=mdy_hm(Date))

file.names <- list.files(path="01_Raw_data/Lily Box/dat", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1,col_types = cols(`31031` = col_number()))
  # LB <- read_csv('01_Raw_data/Lily Box/dat/3_Bradford_3_10042024.dat', skip= 1,
  #                col_types = cols(`31031` = col_number()))

  LB<-LB[-c(1:2),]

  columns_to_keep <- c("TIMESTAMP", "CO2High", "Eosense", 'CO2')
  cols_present <- intersect(columns_to_keep, names(LB))
  LB<-LB[, cols_present, drop = FALSE] #keep columns from columns_to_keep if present
  LB<-LB[,c(1:2)]


  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB<-LB %>% mutate(Date=ymd_hms(Date))
  CO2<-rbind(CO2, LB)}
CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]
CO2<-CO2 %>% mutate(CO2=as.numeric(CO2))

file.names <- list.files(path="01_Raw_data/Lily Box/dat/3", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1)
  # LB <- read_csv('01_Raw_data/Lily Box/dat/3_Bradford_3_10042024.dat', skip= 1,
  #                col_types = cols(`31031` = col_number()))

  LB<-LB[-c(1:2),]

  columns_to_keep <- c("TIMESTAMP", "CO2High", "Eosense", 'CO2')
  cols_present <- intersect(columns_to_keep, names(LB))
  LB<-LB[, cols_present, drop = FALSE] #keep columns from columns_to_keep if present
  LB<-LB[,c(1:2)]


  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB<-LB %>% mutate(Date=ymd_hms(Date),CO2=as.numeric(CO2))
  CO2<-rbind(CO2, LB)}

# depth<-read_csv('02_Clean_data/depth.csv')
# CO2<-left_join(CO2,depth, by=c('Date','ID'))%>% filter(CO2>600)
#clean######
sites<-split(CO2,CO2$ID)
s13<-sites[['13']]
s15<-sites[['15']]# not working :()
s3<-sites[['3']] #npt working :()
s5<-sites[['5']]
s5a<-sites[['5a']]
s6<-sites[['6']]
s6a<-sites[['6a']]
s7<-sites[['7']]
s9<-sites[['9']]

s5<-s5 %>%
  mutate(
    CO2=if_else(Date> '2023-12-17' & Date<'2025-02-27', CO2*4.2, CO2))%>%
  filter(CO2< 52000)%>%
  mutate(CO2=CO2/4.2)%>%filter(CO2>1100)

# ggplotly(ggplot(test,
#                 aes(Date, CO2, color=depth))+
#            geom_point()+
#   theme(legend.position = "bottom"))

#test<-s5a %>%

s15<-s15 %>% filter(CO2>1000 & CO2< 19000)

s7<-s7%>% filter(CO2>800)

s3<-s3 %>%filter(CO2>1400, CO2<23000)%>%
  mutate(CO2 = if_else(Date >"2024-08-23", CO2*4.2, CO2))

s6<-s6 %>%
  mutate(CO2 = if_else(Date >= "2024-08-01", CO2*6, CO2))%>%
  filter(CO2>900, CO2<26000)

s6a<-s6a %>% filter(CO2>3000& CO2<20000)

s9<-s9 %>%  filter(CO2>1300)

s13 <- s13 %>%
  mutate(
    CO2 =
      if_else( Date > as.Date('2024-06-01') & Date < as.Date('2024-08-04'), NA_real_,CO2))%>%
  filter(CO2<13400, CO2>1200)

CO2<-rbind(s5,s5a,s15,s6a,s6,s7,s3,s13,s9)
range(CO2$Date, na.rm=T)
ggplot(CO2, aes(Date, CO2)) + geom_point(size=1) + facet_wrap(~ ID, ncol=4, scales='free')
ggplot(CO2%>% filter(ID=='5'), aes(Date, CO2)) + geom_point(size=1)
write_csv(CO2, "02_Clean_data/CO2_cleaned.csv")

