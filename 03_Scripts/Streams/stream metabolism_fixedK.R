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

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,7,11,6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
merged_data <- reduce(data, left_join, by = c("ID", 'Date'))
ggplot(merged_data, aes(Date, DO)) + geom_point() + facet_wrap(~ ID, ncol=4)

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
  select(solar.time, light, depth, DO.sat, DO.obs, temp.water, ID)

kfixed_list <- lapply(list_of_ks, function(k600_df) {
  k600_df %>%
    group_by(ID) %>%
    summarise(kfixed = mean(mean_K600, na.rm = TRUE), .groups = "drop")
})

mm_name_fixed <- mm_name(type = "mle", pool_K600 = "none")

spec_list <- lapply(kfixed_list, function(kfixed) {
  specs(mm_name_fixed)
})

streams_k_fixed <- mapply(function(streams_edited, kfixed) {
  k_val <- kfixed$kfixed[1]
  streams_edited$K600 <- k_val
  return(streams_edited)
}, streams_edited, kfixed_list, SIMPLIFY = FALSE)



metab_results <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = streams_k_fixed, site_spec = spec_list, SIMPLIFY = FALSE)


streams_k_clean <- lapply(streams_k_fixed, function(df) {
  df %>% select(-K600)
})


metab_results <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = streams_k_clean, site_spec = spec_list, SIMPLIFY = FALSE)


metab_fixed <- metab(mm_specs_fixed, data = your_data)




##################
##Attempt#########
################

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
      K_nodes = list(approx(Q, mean_K600,
                            xout = quantile(Q, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE))$y)
    )

  return(kq_nodes)
})
K600_df <- bind_rows(list_of_ks, .id = "ID")%>% select(ID, mean_K600)%>% distinct() %>%
  rename(K600=mean_K600)%>% filter(!is.na(K600))

merged_data<-left_join(input, K600_df, by='ID')


site_ids <- unique(merged_data$ID)
model_results <- list()

for (site in site_ids) {
  message("Running model for site: ", site)

  # Filter data for the site
  site_data <- merged_data %>% filter(ID == site)

  # Extract fixed K600 value for this site
  site_k600 <- unique(site_data$K600)

  # Create model specs with fixed K600
  metab_spec <- specs(
    mm_name("bayes", err_obs_iid = TRUE),
    K600.daily = "data"
  )

  # Fit the model
  tryCatch({
    model <- metab(metab_spec, data = site_data, info = list(site = site))
    model_results[[site]] <- model
  }, error = function(e) {
    message("Failed for site: ", site, " - ", e$message)
  })
}

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



met_list <- lapply(metab_results, function(metab_results) {
  prediction2 <- metab_results@fit$daily %>%
    select(date, GPP_daily_mean, ER_daily_mean, K600_daily_mean,
           GPP_Rhat, ER_Rhat, K600_daily_Rhat) %>%
    filter(ER_Rhat > 0.9 & ER_Rhat < 1.05,
           K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.05) %>%
    select(date, GPP_daily_mean, ER_daily_mean, K600_daily_mean)

  return(prediction2)
})

met_df <- bind_rows(met_list, .id = "ID")
