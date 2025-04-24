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


cols <- c('solar.time', 'light', 'depth', 'DO.sat', 'DO.obs', 'temp.water', 'ID')
unique_sites <- unique(input$ID[!is.na(input$ID)])

streams <- setNames(
  lapply(unique_sites, function(ID) {
    df_subset <- input %>%
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


bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)
mm <- metab(bayes_specs, data = s6)


spec_list <- replicate(length(streams_edited), bayes_specs, simplify = FALSE)
safe_metab <- safely(metab)

metab_results <- map2(spec_list, streams_edited, function(spec, data) {
  safe_metab(spec, data = data)
})

successful_models <- map(metab_results, "result")
failed_indices <- which(map_lgl(metab_results, ~ !is.null(.x$error)))
error_messages <- map(metab_results, "error")

# Print all non-null error messages
walk2(error_messages, seq_along(error_messages), function(err, i) {
  if (!is.null(err)) {
    cat("\n--- Error in model", i, "---\n")
    print(err$message)
  }
})
met_df <- bind_rows(met_list, .id = "ID")


ggplot(met_df, aes(date)) +
  geom_point(aes(y = ER_daily_mean, color = 'ER')) +
  geom_point(aes(y = GPP_daily_mean, color = 'GPP')) +
  facet_wrap(~ ID, ncol = 3, scale = 'free') +
  ylab(expression(O[2]~'g'/m^2/'day')) +
  xlab("Date")


write_csv(met_df, "04_Output/master_metabolism.csv")

#############



discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(Date=as.Date(Date))
metabolism<-read_csv("04_Output/master_metabolism.csv")
metabolism<-left_join(metabolism, discharge, by=c('Date', 'ID'))
select<-metabolism %>% filter(ID %in% c('5','6','9'))

ggplot(metabolism, aes(Date)) +
  #geom_point(aes(y = ER, color = 'ER')) +
  geom_point(aes(y = GPP, color = 'GPP')) +
  facet_wrap(~ ID, ncol = 3, scale = 'free') +
  ylab(expression(O[2]~'g'/m^2/'day')) +
  xlab("Date")
b<-ggplot(master,aes(x=ID,y=ER))+
  geom_boxplot(outlier.color="black")+
  ggtitle("ER")+theme_sam

plot_grid(a,b, align = "v", ncol = 1, rel_heights = c(0.6,0.4))


ggplot(s3, aes(x=Date, y=DO))+
  geom_point()

