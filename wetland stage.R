library(anytime)
library(tidyverse)
library(readxl)

#compile PT####
file.names <- list.files(path="Z:/Bradford_Forest_Project/Wetlands/Wetland_H20_level/For R",
                         pattern=".csv", full.names=TRUE)
PT_all<-data.frame()
for(fil in file.names){
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$Date <- mdy_hms(PT$Date)
  PT$BasinID<-strsplit(basename(fil), '_')[[1]][1]
  PT$WetlandID<-strsplit(basename(fil), '_')[[1]][2]
  PT_all<-rbind(PT_all,PT)}
PT_all$BasinID[PT_all$BasinID=='14/9']<-'14.9'
PT_all <-  PT_all %>%
  mutate(region= case_when(BasinID=="6"|BasinID=="6a"|BasinID=="3"|BasinID=="7"~ 'N',
                           BasinID=="5"|BasinID=="5a"|BasinID=="15"|BasinID=="9"|
                             BasinID=="14"|BasinID=="13"|BasinID=="14.9"~ 'S'))

write_csv(PT_all, "Z:/Bradford_Forest_Project/Wetlands/Wetland_H20_level/Data_processed/compiled_PT.csv")

#calculate stage####
PT<-read_csv("Z:/Bradford_Forest_Project/Wetlands/Wetland_H20_level/Data_processed/compiled_PT.csv")
baro<-read_csv('01_Raw_data/PT/compiled_baro.csv')

master<-left_join(PT, baro, by=c('region','Date'))
master <- master[!duplicated(master[c( 'Date','BasinID')]),]

master$Water_press<-master$PT-master$PTbaro
master$sensor_depth<-1000*master$Water_press/2.2/(2.54^2)/100
master$depth<-master$sensor_depth-(master$PL-master$PG)/100

Wetland_well_metadata <- read_excel("Z:/Bradford_Forest_Project/Masterfiles_latest/Wetland_well_metadata.xlsx",sheet = "Wetland_pivot_history")
chk1<-Wetland_well_metadata[,c("Site_ID","Basin_ID",'P_G/L_date_1',"P_G_cm_1","P_L_cm_1")]
chk1<-chk1 %>% rename('PG'="P_G_cm_1", 'PL'='P_L_cm_1', 'Date'='P_G/L_date_1')
chk2<-Wetland_well_metadata[,c("Site_ID","Basin_ID",'P_G/L_date_2',"P_G_cm_2","P_L_cm_2")]
chk2<-chk2 %>% rename('PG'="P_G_cm_2", 'PL'='P_L_cm_2', 'Date'='P_G/L_date_2')
chk3<-Wetland_well_metadata[,c("Site_ID","Basin_ID",'P_G/L_date_3',"P_G_cm_3","P_L_cm_3")]
chk3<-chk3 %>% rename('PG'="P_G_cm_3", 'PL'='P_L_cm_3', 'Date'='P_G/L_date_3')
chk4<-Wetland_well_metadata[,c("Site_ID","Basin_ID",'P_G/L_date_4',"P_G_cm_4","P_L_cm_4")]
chk4<-chk4 %>% rename('PG'="P_G_cm_4", 'PL'='P_L_cm_4', 'Date'='P_G/L_date_4')
PG_PL<-rbind(chk1,chk2,chk3,chk4)
PG_PL<-PG_PL%>% mutate(WetlandID=str_split_fixed(basename(PG_PL$Site_ID), '_', 2)) %>%
  rename('BasinID'='Basin_ID')
PG_PL$WetlandID[,2]
PG_PL$Basin_ID[PG_PL$Basin_ID=='14/9']<-'14.9'

strsplit(basename(PG_PL$Site_ID), '_')[[1]][2]
write_csv(compile_stage, "Z:/Bradford_Forest_Project/Wetlands/Wetland_H20_level/Data_processed/compiled_stage.csv")
