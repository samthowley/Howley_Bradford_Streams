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


# cols <- c('solar.time', 'light', 'depth', 'DO.sat', 'DO.obs', 'temp.water', 'ID')
# unique_sites <- unique(input$ID[!is.na(input$ID)])
#
# streams <- setNames(
#   lapply(unique_sites, function(ID) {
#     df_subset <- input %>%
#       filter(ID == ID) %>%
#       select(all_of(cols))
#     return(df_subset)}),
#   unique_sites
# )
#
# streams_edited <- lapply(streams, function(df) {
#   df %>%
#     arrange(solar.time) %>%
#     filter(c(TRUE, diff(solar.time) > 0))
# })



sheet_names <- excel_sheets("04_Output/rC_K600.xlsx")
ks <- sheet_names[!sheet_names %in% c("6a")]

list_of_ks <- list()
for (sheet in ks) {
  df <- read_excel("04_Output/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

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










































# Create a daimean_K600# Create a daily K600 dataframe
K600_daily <- data.frame(
  date = unique(your_data$solar.time),  # adjust if needed
  K600 = 25  # or vary per day if you prefer
)

# MLE example with fixed K600
mm_name_fixed <- mm_name(type = "mle", pool_K600 = "none")
mm_specs_fixed <- specs(mm_name_fixed, K600_daily = K600_daily)

metab_fixed <- metab(mm_specs_fixed, data = your_data)
