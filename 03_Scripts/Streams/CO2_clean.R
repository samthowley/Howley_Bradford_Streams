#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2023-12-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-10-05 00:00", tz="UTC"),by="hour")))
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
CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]
CO2<-CO2%>%mutate(Date=mdy_hm(Date))

ggplot(CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=4, scales = "free")

file.names <- list.files(path="01_Raw_data/Lily Box/dat", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1)
  LB<-LB[-c(1:2),]

  columns_to_keep <- c("TIMESTAMP", "CO2High", "Eosense", 'CO2')
  cols_present <- intersect(columns_to_keep, names(LB))
  LB<-LB[, cols_present, drop = FALSE] #keep columns from columns_to_keep if present
  LB<-LB[,c(1:2)]


  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB<-LB %>% mutate(Date=ymd_hms(Date), CO2=as.numeric(CO2))
  CO2<-rbind(CO2, LB)}
  CO2 <- CO2[!duplicated(CO2[c('Date','ID')]),]

CO2$CO2[CO2$CO2 < 600] <- NA
CO2$CO2[CO2$CO2 > 20000] <- NA
ggplot(CO2, aes(Date, CO2)) + geom_line()+ facet_wrap(~ ID, ncol=4, scales = "free")
range(CO2$Date, na.rm=T)
write_csv(CO2, "02_Clean_data/CO2_cleaned.csv")

##############
peek<-CO2 %>%filter(Date>'2024-09-01')
