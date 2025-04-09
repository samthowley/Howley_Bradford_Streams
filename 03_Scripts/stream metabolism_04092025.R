rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)
library(lubridate)
library(streamMetabolizer)
library(weathermetrics)
library('StreamMetabolism')
library(lme4)

metabolism <- function(site) {

  site<-site%>%select(-ID, Q)
  site<-site[order(as.Date(site$Date, format="%Y-%m-%d %H:%M:%S")),]
  site<-left_join(samplingperiod,site)
  site$Temp_C<- fahrenheit.to.celsius(site$Temp)

  site <- site[!duplicated(site[c('Date')]),]

  site<-rename(site,'DO.obs'='DO','temp.water'='Temp_C')
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site<-site[,-c(1)]
  site$light<-calc_light(site$solar.time,  29.8, -82.6)
  y<-c("DO.obs","depth","temp.water", "DO.sat","solar.time","light" )
  site<-site[,y]
  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                         GPP_Rhat,ER_Rhat,K600_daily_Rhat)
  prediction2<- prediction2 %>% filter(ER_Rhat> 0.9 & ER_Rhat<1.05)%>% filter(K600_daily_Rhat> 0.9 & K600_daily_Rhat<1.05)%>%
    select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)

  return(prediction2)}
metabolism_K <- function(site) {

  site<-site%>%select(-ID, log_K600)
  site<-site[order(as.Date(site$Date, format="%Y-%m-%d %H:%M:%S")),]
  site<-left_join(samplingperiod,site)
  site$Temp_C<- fahrenheit.to.celsius(site$Temp)

  site <- site[!duplicated(site[c('Date')]),]

  site<-rename(site,'DO.obs'='DO','temp.water'='Temp_C')
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site<-site[,-c(1)]
  site$light<-calc_light(site$solar.time,  29.8, -82.6)
  y<-c("DO.obs","depth","temp.water", "DO.sat","solar.time","light" )
  site<-site[,y]
  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                         GPP_Rhat,ER_Rhat,K600_daily_Rhat)
  prediction2<- prediction2 %>% filter(ER_Rhat> 0.9 & ER_Rhat<1.05)%>% filter(K600_daily_Rhat> 0.9 & K600_daily_Rhat<1.05)%>%
    select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)

  return(prediction2)}

#constants######
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2023-10-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2025-03-28 00:00", tz="UTC"),by="hour")))
samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,7,11,6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
merged_data <- reduce(data, left_join, by = c("ID", 'Date'))

input <- merged_data %>%
  filter(depth > 0, Date > "2023-10-06", ID != '14')%>%
 rename('DO.obs'='DO','temp.water'='Temp_PT.x', discharge=Q)%>%
  mutate(DO.sat=Cs(temp.water),
         solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"))%>%
  mutate(
    light=calc_light(solar.time,  29.8, -82.6))%>%
  select(solar.time, light, depth, DO.sat, DO.obs, temp.water, discharge, ID)


cols <- c('solar.time', 'light', 'depth', 'DO.sat', 'DO.obs', 'temp.water', 'discharge', 'ID')
unique_sites <- unique(input$ID[!is.na(input$ID)])

df_list <- setNames(
  lapply(unique_sites, function(ID) {
    df_subset <- input %>%
      filter(ID == ID) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

# met_prep <- lapply(df_list, function(df) {
#
#   return(df)
# })

#K600#############
sheet_names <- excel_sheets("04_Output/rC_K600.xlsx")

list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Output/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}


kq_nodes_list<- lapply(list_of_ks, function(k600_df){
  kq_nodes <- k600_df %>%
    filter(!is.na(Q), !is.na(k600_dh)) %>%
    group_by(ID) %>%
    summarise(
      Q_nodes = list(quantile(Q, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)),
      K_nodes = list(approx(Q, k600_dh, xout = quantile(Q, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE))$y))

  return(k600_df)
})


)
