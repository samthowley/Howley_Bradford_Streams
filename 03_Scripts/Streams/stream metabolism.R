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
samplingperiod <- data.frame(solar.time = rep(seq(from=as.POSIXct("2023-10-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2025-05-17 00:00", tz="UTC"),by="hour")))

#data####
file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,7,11,6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

merged_data <- reduce(data, left_join, by = c("ID", 'Date'))%>%
  filter(complete.cases(DO, depth))%>%
  mutate(ln.Q=log(Q))%>%
  group_by(ID)%>%
  mutate(split_q=case_when(depth>mean(depth, na.rm=T)~'hi',
                           depth<=mean(depth, na.rm=T)~'low'))%>%
  mutate(ID_q = paste(ID, split_q, sep = "_"))


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

cols <- c('solar.time', 'light', 'depth', 'DO.sat', 'DO.obs', 'temp.water', 'ID', 'ID_q')
unique_sites <- unique(input$ID_q[!is.na(input$ID_q)])

streams <- setNames(
  lapply(unique_sites, function(ID_q) {
    df_subset <- input %>%
      filter(ID_q==ID_q) %>%
      select(all_of(cols))
    return(df_subset)}),
  unique_sites
)

rdy_for_sm<- lapply(streams, function(df) {
  df<-df %>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(solar.time) > 0))%>%
    select(-ID, -ID_q)

  df<-left_join(samplingperiod, df)
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
      K600=mean(k600_dh,na.rm=T),
      sd_vals=sd(k600_dh,na.rm=T))

  return(k600)
})

specs <- lapply(k600_mean_list, function(K_means) {
  site_id <- K_means$ID_q[1]
  K_vals <- K_means$K600[[1]]
  sd_vals <- K_means$sd_vals[[1]]


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
                       K600_daily_meanlog_meanlog= log(K_vals),
                       K600_daily_meanlog_sdlog=log(sd_vals),
                       GPP_daily_lower=0,
                       burnin_steps=1000,
                       saved_steps=1000)
  })


valid_ids <- names(specs)[!sapply(specs, is.null)]
valid_streams <- streams_baseflow[valid_ids]
valid_specs <- specs[valid_ids]

# Run streamMetabolizer on each valid site##############
metab_results_base <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = valid_streams, site_spec = valid_specs, SIMPLIFY = FALSE)

met_list_base <- lapply(metab_results_base, function(metab_results) {
  prediction2 <- metab_results@fit$daily %>%
    select(date, GPP_daily_mean, ER_daily_mean, K600_daily_mean,
          GPP_Rhat, ER_Rhat, K600_daily_Rhat) #%>%
     filter(ER_Rhat > 0.9 & ER_Rhat < 1.2,
            K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.2) #%>%
    #select(date, GPP_daily_mean, ER_daily_mean, K600_daily_mean, warnings)

  return(prediction2)
})

met_base <- bind_rows(met_list_base, .id = "ID")%>% filter(GPP_daily_mean>0, ER_daily_mean<0)


ggplot(met_base, aes(date)) +
  #geom_point(aes(y = ER_daily_mean, color = 'ER')) +
  geom_point(aes(y = K600_daily_Rhat, color = 'ER')) +
  #geom_point(aes(y = GPP_daily_mean, color = 'GPP')) +
  facet_wrap(~ ID, ncol = 3, scale = 'free') +
  #ylab(expression(O[2]~'g'/m^2/'day')) +
  xlab("Date")

met_base%>% group_by(ID)%>%
  summarise(
    K=mean(K600_daily_mean, na.rm=T)
  )


write_csv(met_df, "04_Output/metabolism_04302025.csv")




ggplot(master_metabolism , aes(Date)) +
  #geom_point(aes(y = ER_daily_mean, color = 'ER')) +
  geom_point(aes(y = K600_daily_mean, color = 'ER')) +
  #geom_point(aes(y = GPP_daily_mean, color = 'GPP')) +
  facet_wrap(~ ID, ncol = 3, scale = 'free') +
  #ylab(expression(O[2]~'g'/m^2/'day')) +
  xlab("Date")


####Testing specs with 5#####################################

bayes_name <- mm_name(type='bayes',
                      pool_K600='normal',
                      err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     K600_daily_meanlog_meanlog= log(5.6),
                     K600_daily_meanlog_sdlog=log(6),
                     GPP_daily_lower=0,
                     burnin_steps=1000,
                     saved_steps=1000)


s5<- input%>% filter(ID=='5')%>%
  select(solar.time, light, depth, DO.sat, DO.obs, temp.water)
s5<-left_join(samplingperiod, s5)%>% filter(solar.time<'2025-06-02', depth>mean(s5$depth, na.rm=T))

ggplot(s5,aes(x=solar.time, y=depth)) + geom_point()




mm <- metab(bayes_specs, data=s5)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                       GPP_Rhat,ER_Rhat,K600_daily_Rhat)

ggplot(prediction2,aes(x=date, y=GPP_daily_mean)) + geom_point()
ggplot(prediction2%>%filter(ER_Rhat>1.2),aes(x=date, y=ER_daily_mean)) + geom_point()

