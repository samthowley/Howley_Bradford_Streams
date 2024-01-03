#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)


VialID <- function(sample) {
  sample <- read_csv(fil)
  sample$Date<-mdy_hm(sample$Date)
  sample$Ran<-mdy(sample$Ran)
  sample$Vial<-as.character(sample$Vial)
  sample<- sample %>% mutate(month=month(Ran), year=year(Ran))
  return(sample)} #function for cleaning data

call_results <- function(result) {
  result <- read_csv(fil, skip=10)
  result<-result %>% rename('ID'='Sample Name', 'Ran'='Date / Time') %>% filter(ID== 'sam')
  result <-result[,c(9,11,12,13)]
  result$Vial<-as.character(result$Vial)
  result$Ran<-mdy_hm(result$Ran)
  result<- result %>% mutate(month=month(Ran), year=year(Ran))
  return(result)} #function for cleaning data

file.names <- list.files(path="01_Raw_data/DIC DOC/ID", pattern=".csv", full.names=TRUE)
vials<-data.frame()
for(fil in file.names){
  vial<-VialID(fil)
  vials<-rbind(vials, vial)}

file.names <- list.files(path="01_Raw_data/DIC DOC/results", pattern=".csv", full.names=TRUE)
results<-data.frame()
for(fil in file.names){
  result<-call_results(fil)
  results<-rbind(results, result)}

together<-left_join(vials,results,by=c('Vial','month','year'))
TOC<-together %>% filter(Type=='DIC') %>% rename('TOC'="Result(NPOC)")
DOC<-together %>% filter(Type=='DOC') %>% rename('DOC'="Result(NPOC)")
DOC_DIC<-left_join(TOC, DOC, by=c('Date','Site'))
x<-c("Site","Date","TOC","DOC")
DOC_DIC<-DOC_DIC[,x]
DOC_DIC$DIC<-DOC_DIC$TOC-DOC_DIC$DOC

write_csv(DOC_DIC, "04_Output/DOC-DIC.csv")
