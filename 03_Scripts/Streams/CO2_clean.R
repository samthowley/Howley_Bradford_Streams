#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2023-10-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-10-26 00:00", tz="UTC"),by="hour")))
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
# CO2<-left_join(CO2,depth, by=c('Date','ID'))
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

s5<-s5 %>%filter(CO2>2000 & CO2<15000)#%>%filter(Date<'2023-10-01')
# a<-ggplot(s5, aes(Date, CO2))+geom_line()+ggtitle('Stream 5')
# b<-ggplot(s5, aes(Date, depth))+geom_line()+ggtitle('Stream 5')
#plot_grid(a,b,ncol=1)


s5a<-s5a %>% filter(CO2>3500)
#ggplot(s5a, aes(Date, CO2))+geom_line()+geom_hline(yintercept = 3500)

s15<-s15 %>% filter(CO2>500)
#ggplot(s15, aes(Date, CO2))+geom_line()+geom_hline(yintercept = 400)

s7<-s7%>% filter(CO2>1200)
# a<-ggplot(s7, aes(Date, CO2))+geom_point()+geom_hline(yintercept=1100)
# b<-ggplot(s7, aes(Date, depth))+geom_line()
# plot_grid(a,b,ncol=1)

s3<-s3 %>% filter(CO2>2000)
s3_1<-s3 %>% filter(Date<'2023-10-01')%>% filter(CO2>8000)
s3_2<-s3 %>% filter(Date>'2023-10-01')
s3<-rbind(s3_1,s3_2)
# ggplot(s3, aes(Date, CO2))+geom_point()
# plot_grid(a,b,ncol=1)

s6_post0724<-s6 %>%filter(Date>'2024-07-10')%>%mutate(CO2=CO2*6)
s6_pre0724<-s6 %>%filter(Date<'2024-07-10')%>%filter(CO2>0)
s6_edited<-rbind(s6_post0724,s6_pre0724)
# s6<-s6_edited %>% filter(CO2>5500 & CO2<25300)
# a<-ggplot(s6_edited, aes(Date, CO2))+geom_line()
# b<-ggplot(s6_edited, aes(Date, depth))+geom_line()
# plot_grid(a,b,ncol=1)

s6a<-s6a %>% filter(CO2>3000& CO2<21000)
#ggplot(s6a, aes(Date, CO2))+geom_point()+geom_hline(yintercept = 3000)

s9<-s9 %>%  filter(CO2>1000)
# a<-ggplot(s9, aes(Date, CO2))+geom_line()+geom_hline(yintercept = 1000)
# b<-ggplot(s9, aes(Date, depth))+geom_line()
# plot_grid(a,b,ncol=1)

s13<-s13 %>%  filter(CO2<15000)
# a<-ggplot(s13, aes(Date, CO2))+geom_line()+geom_hline(yintercept = 1000)
# b<-ggplot(s13, aes(Date, depth))+geom_line()
# plot_grid(a,b,ncol=1)


CO2<-rbind(s5,s5a,s15,s6a,s6,s7,s3,s13,s9)
range(CO2$Date, na.rm=T)
ggplot(CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=4)

write_csv(CO2, "02_Clean_data/CO2_cleaned.csv")

##############
peek<-CO2 %>%filter(Date>'2024-09-01')
