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

#constants######
samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2023-10-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2025-04-17 00:00", tz="UTC"),by="hour")))
samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))

#data####
file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,7,11,6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
merged_data <- reduce(data, left_join, by = c("ID", 'Date'))%>%
  filter(complete.cases(DO, depth))%>%
  mutate(ln.Q=log(Q))

ggplot(merged_data, aes(x=ln.Q, y=DO)) +
  geom_point()+
  facet_wrap(~ ID, ncol = 3, scale = 'free')

input <- merged_data %>%
  filter(depth > 0,ID != '14')%>%
  rename('DO.obs'='DO')%>%
  mutate(
    temp.water=fahrenheit.to.celsius(Temp_PT.x))%>%
  mutate(
    DO.sat=Cs(temp.water),
    solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
  )%>%
  mutate(
    light=calc_light(solar.time,  29.8, -82.6))


cols <- c('solar.time', 'light', 'depth', 'DO.sat', 'DO.obs', 'temp.water', 'ID', 'Qbase', 'Q')
unique_sites <- unique(input$ID[!is.na(input$ID)])

streams <- setNames(
  lapply(unique_sites, function(ID) {
    df_subset <- input %>%
      filter(ID == ID) %>%
      select(all_of(cols))
    return(df_subset)}),
  unique_sites
)

streams_baseflow<- lapply(streams, function(df) {
  df %>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(solar.time) > 0))%>%
    filter(Q<= mean(Qbase, na.rm=T))%>%
    select(-ID, -Qbase, -Q)
})

#K600#############
sheet_names <- excel_sheets("04_Output/rC_K600.xlsx")
ks <- sheet_names[!sheet_names %in% c("6a")]

list_of_ks <- list()
for (sheet in ks) {
  df <- read_excel("04_Output/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

#specs######
k600_mean_list <- lapply(list_of_ks, function(k600_df) {
  k600 <- k600_df %>%
    group_by(ID) %>%
    summarise(
      K600=mean((k600_dh/depth),na.rm=T)
    )

  return(k600)
})

specs <- lapply(k600_mean_list, function(K_means) {
  site_id <- K_means$ID[1]
  K_vals <- K_means$K600[[1]]

  # Handle missing or NA values in K_vals
  if (all(is.na(K_vals))) {
    warning(paste("Skipping site", site_id, "- K_vals all NA"))
    return(NULL)
  }

  # Build specs
  bayes_name <- mm_name(type='bayes',
                        pool_K600='normal',
                        err_obs_iid=TRUE, err_proc_iid=TRUE)

  bayes_specs <- specs(bayes_name,
                       K600_daily_meanlog_meanlog=K_vals,
                       K600_daily_meanlog_sdlog=0.001,
                       GPP_daily_lower=0,
                       burnin_steps=1000,
                       saved_steps=1000)
  })


valid_ids <- names(specs)[!sapply(specs, is.null)]
valid_streams <- streams_baseflow[valid_ids]
valid_specs <- specs[valid_ids]

# Run streamMetabolizer on each valid site
metab_results <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = valid_streams, site_spec = valid_specs, SIMPLIFY = FALSE)



met_list <- lapply(metab_results, function(metab_results) {
  prediction2 <- metab_results@fit$daily %>%
    select(date, GPP_daily_mean, ER_daily_mean, K600_daily_mean,
          GPP_Rhat, ER_Rhat, K600_daily_Rhat, warnings) %>%
    # filter(ER_Rhat > 0.9 & ER_Rhat < 1.2,
    #        K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.2) %>%
    select(date, GPP_daily_mean, ER_daily_mean, K600_daily_mean, warnings)

  return(prediction2)
})

met_df <- bind_rows(met_list, .id = "ID")%>% filter(GPP_daily_mean>0, ER_daily_mean<0)


ggplot(met_df, aes(date)) +
  #geom_point(aes(y = ER_daily_mean, color = 'ER')) +
  geom_point(aes(y = K600_daily_mean, color = 'ER')) +
  #geom_point(aes(y = GPP_daily_mean, color = 'GPP')) +
  facet_wrap(~ ID, ncol = 3, scale = 'free') +
  ylab(expression(O[2]~'g'/m^2/'day')) +
  xlab("Date")




write_csv(met_df, "04_Output/metabolism_04302025.csv")
