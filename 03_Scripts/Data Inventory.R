library(tidyverse)

read_csv("02_Clean_data/depth.csv")%>%
  group_by(ID)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)

read_csv("04_Output/gas.samples.csv")%>%
  filter(chapter=='stream', type=='CH4')%>%
  group_by(Site)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)

read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='stream')%>%
  filter(!is.na(POC))%>%
  group_by(Site)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)

read_csv("01_Raw_data/GD/GasDome_compiled.csv")%>%
  mutate(day=as.Date(Date))%>%
  distinct(day, ID, .keep_all = T)%>%
  group_by(ID)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)


read_csv("04_Output/RC/RC.water.sample.flux.csv")%>%
  group_by(Site)%>%
  filter(!is.na(DIC))%>%
  summarize(
    DOC.samples=n()
  )


read_csv("04_Output/RC/RC.gas.sample.flux.csv")%>%
  group_by(Site)%>%
  filter(!is.na(CO2.water.ppm))%>%
  summarize(
    Date=min(Date, na.rm = T)
  )



long.dis.c<-read_csv("04_Output/sampled.solid.carbon.csv")

long.DOC<-long.dis.c%>%
  filter(chapter=='long')%>%
  filter(!is.na(DOC))%>%
  distinct(Date, Site)%>%
  group_by(Site)%>%
  summarize(
    `DOC Samples` = n(),
    `DOC Sampling Dates`=min(Date, na.rm = T)
  )%>% arrange(Site)


long.DIC<-long.dis.c%>%
  filter(chapter=='long')%>%
  filter(!is.na(DIC))%>%
  distinct(Date, Site)%>%
  group_by(Site)%>%
  summarize(
    `DIC Samples` = n(),
    `DIC Sampling Dates`=min(Date, na.rm = T)
  )%>% arrange(Site)


long.gas<-read_csv("04_Output/gas.samples.csv")%>%
  filter(chapter=='long', type=='CO2')%>%
  distinct(Date, Site)%>%
  group_by(Site)%>%
  summarize(
    `CO2/ CH4 Samples` = n(),
    `CO2/ CH4 Sampling Dates`=min(Date, na.rm = T)
  )%>% arrange(Site)



df_list <- list(long.DOC, long.DIC, long.gas)

write_csv(reduce(df_list, full_join, by=c('Site')), "test.csv")

