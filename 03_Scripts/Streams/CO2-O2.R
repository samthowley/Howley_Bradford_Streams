library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(ggpmisc)
library('StreamMetabolism')

mean_daily <- function(file, value_col) {
  read_csv(file) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date, ID) %>%
    summarise("{value_col}" := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
}

CO2 <- mean_daily("02_Clean_data/CO2_cleaned.csv", "CO2")
DO <- mean_daily("02_Clean_data/DO_cleaned.csv", "DO")
temp <- mean_daily("02_Clean_data/temperature.csv", "Temp_PT")
metabolism<-read_csv('04_Output/master_metabolism.csv')%>% rename(Date="date" )
Q<- mean_daily("04_Output/flow_regime_daily.csv", "Q")
depth<- mean_daily("04_Output/flow_regime_daily.csv", "depth")


df_list <- list(CO2, temperature, DO, Q, depth, metabolism)
combined_df <- reduce(df_list, full_join, by=c('Date', 'ID'))


KH<-combined_df %>%
  mutate(ssn=time2season(Date, out.fmt="seasons"),
                         Temp_C = fahrenheit.to.celsius(Temp_PT)) %>%
           mutate(Temp_K=Temp_C+273.15)%>%
           mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%
           mutate(KH=0.034*2.178^(exp))

mols<-KH %>%
  mutate(CO2_atm=CO2/10^6) %>% mutate(CO2_molL=CO2_atm*KH, DO_molL=DO/32)

ks<-mols %>%
  mutate(K600_m.d=K600*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
         SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)),
         DO.sat=Cs(Temp_C))#%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)

flux<-ks%>%
  mutate(CO2_flux=KCO2_m.d*(CO2-400)*KH*(1/10^6)*44.01,
         O2_flux=((DO-DO.sat)*(1/10^3))*KO2_m.d) %>% filter(ID != '14')%>%
  mutate(o2co2=O2_flux/CO2_flux)

write_csv(flux, "04_Output/fluxes.csv")


#Slope##########

flux_lm <- function(df) {
  if(nrow(df) < 2) return(NULL)  # skip if not enough data for a regression

  flux.lm <- lm(CO2_flux ~ O2_flux, data = df)
  cf <- coef(flux.lm)

  tibble(
    ID = df$ID[1],
    day = df$day[1],
    flux_slope = cf[2],
    flux_intercept = cf[1]
  )
}

flux.lm <- flux %>%
  group_by(ID, day) %>%
  group_split() %>%
  map_dfr(flux_lm)

flux_slope <- left_join(flux, flux.lm, by = c('day', 'ID')) %>%
  mutate(
    slope_type = case_when(
      flux_slope <= -0.7 & flux_slope >= -1.2 ~ '-1:1',
      flux_slope > -0.7 ~ 'CO2.dom',
      flux_slope < -1.2 ~ 'O2.dom',
      TRUE ~ NA_character_
    )
  )

median(flux_slope$flux_slope, na.rm=T)

ggplot(flux_slope %>%filter(ID %in% c('5','6')), aes(x = Q, y = flux_slope, color = slope_type)) +
  geom_point(shape = 1) +
  scale_x_log10()+
  geom_hline(yintercept = 1) +
  theme(legend.position = "bottom")

#################







