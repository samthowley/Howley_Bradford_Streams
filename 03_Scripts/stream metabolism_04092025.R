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
  filter(depth > 0,ID != '14')%>%
 rename('DO.obs'='DO', discharge=Q)%>%
  mutate(
    temp.water=fahrenheit.to.celsius(Temp_PT.x))%>%
  mutate(
    DO.sat=Cs(temp.water),
         solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         )%>%
  mutate(
    light=calc_light(solar.time,  29.8, -82.6))%>%
  select(solar.time, light, depth, DO.sat, DO.obs, temp.water, discharge, ID)

for_k600<-input%>% filter(!ID %in% c('5a', '6a'))
cols <- c('solar.time', 'light', 'depth', 'DO.sat', 'DO.obs', 'temp.water', 'discharge', 'ID')
unique_sites <- unique(for_k600$ID[!is.na(for_k600$ID)])

streams <- setNames(
  lapply(unique_sites, function(ID) {
    df_subset <- for_k600 %>%
      filter(ID == ID) %>%
      select(all_of(cols))
    return(df_subset)}),
  unique_sites
)

streams_edited <- lapply(streams, function(df) {
  df %>%
    select(-ID)%>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(solar.time) > 0))
})

#K600#############
sheet_names <- excel_sheets("04_Output/rC_K600.xlsx")
ks <- sheet_names[!sheet_names %in% c("5a", "6a")]

list_of_ks <- list()
for (sheet in ks) {
  df <- read_excel("04_Output/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

kq_nodes_list <- lapply(list_of_ks, function(k600_df) {
  kq_nodes <- k600_df %>%
    filter(!is.na(Q), !is.na(k600_dh)) %>%
    group_by(ID) %>%
    filter(n() >= 2) %>%  # Ensure enough points for interpolation
    summarise(
      Q_nodes = list(quantile(Q, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)),
      K_nodes = list(approx(Q, k600_dh,
                            xout = quantile(Q, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE))$y)
    )

  return(kq_nodes)
})

specs <- lapply(kq_nodes_list, function(kq_nodes) {
  site_id <- kq_nodes$ID[1]
  Q_vals <- kq_nodes$Q_nodes[[1]]
  K_vals <- kq_nodes$K_nodes[[1]]

  # Handle missing or NA values in K_vals
  if (all(is.na(K_vals))) {
    warning(paste("Skipping site", site_id, "- K_vals all NA"))
    return(NULL)
  }

  # Build specs
  bayes_specs <- specs(
    mm_name(type = "bayes", pool_K600 = "binned", err_obs_iid = TRUE, err_proc_iid = TRUE),
    K600_lnQ_nodes_centers = Q_vals,
    K600_lnQ_nodes_meanlog = log(K_vals),
    K600_lnQ_nodes_sdlog = 0.1,
    K600_lnQ_nodediffs_sdlog = 0.05,
    K600_daily_sigma_sigma = 0.24,
    burnin_steps = 1000,
    saved_steps = 1000)})

valid_ids <- names(specs)[!sapply(specs, is.null)]
valid_streams <- streams_edited[valid_ids]
valid_specs <- specs[valid_ids]

# Run streamMetabolizer on each valid site
metab_results <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = valid_streams, site_spec = valid_specs, SIMPLIFY = FALSE)

met_df <- bind_rows(metab_results, .id = "ID")

write_csv(met_df, "04_Output/metabolism_04092025.csv")
