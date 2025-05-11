library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(ggpmisc)
library('StreamMetabolism')


file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,6,7,4)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, left_join, by = c("ID", 'Date'))
master<-master %>% mutate(day=as.Date(Date))

metabolism<-read_csv('04_Output/master_metabolism.csv')
metabolism<-metabolism %>%
  rename(k600_1.d=K600_daily_mean, day="Date" )

master.k600<-full_join(master, metabolism, c('day', 'ID'))

temp<-master.k600 %>%
  mutate(Temp_C = fahrenheit.to.celsius(Temp_PT)) %>%mutate(Temp_K=Temp_C+273.15)


KH<-temp %>% mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%mutate(KH=0.034*2.178^(exp))

mols<-KH %>%
  mutate(CO2_atm=CO2/10^6) %>% mutate(CO2_molL=CO2_atm*KH, DO_molL=DO/32)

ks<-mols %>%
  mutate(K600_m.d=k600_1.d*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
         SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3)%>%
  mutate(KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3))) %>%
  mutate(KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)),
         DO.sat=Cs(Temp_C))#%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)


flux<-ks%>%
  mutate(CO2_flux=KCO2_m.d*(CO2-400)*KH*(1/10^6)*44.01,
         O2_flux=((DO-DO.sat)*(1/10^3))*KO2_m.d) %>% filter(ID != '14')%>%
  mutate(o2co2=O2_flux/CO2_flux)%>%
  filter(complete.cases(CO2, DO, k600_1.d))%>% distinct()%>%
  mutate(day=as.Date(Date))

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

flux$ID <- factor(flux$ID , levels=c('5','5a','15','7','3','6','6a','9','13'))

write_csv(flux, "04_Output/fluxes.csv")

ggplot(flux%>%filter(Date<'2024-03-20'), aes(x = CO2_flux, y = O2_flux, color = Q)) +
  geom_point(shape = 1) +
  ylab(expression(O[2]~mol/m^2/day)) +
  xlab(expression(CO[2]~mol/m^2/day)) +
  scale_color_gradient("Discharge (Q)", high = 'red', low = 'blue') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Adds regression lines
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~~")),
               formula = y ~ x, parse = TRUE,
               label.x = "right", label.y = "top",
               size=5) +  # Adds slope equation text
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", color = "darkred") +
  facet_wrap(~ ID, ncol = 3, scales = 'free') +
  theme(legend.position = "bottom")


ggplot(flux_slope %>%filter(ID %in% c('5','6'), flux_slope<10), aes(x = Q, y = flux_slope)) +
  geom_point(shape = 1, color='gray') +
  ylab(expression(O[2]~mol/m^2/day)) +
  xlab(expression(CO[2]~mol/m^2/day)) +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", color = "darkred") +
  facet_wrap(~ ID, ncol = 3, scales = 'free') +
  theme(legend.position = "bottom")


ggplot(flux_slope %>% filter(ID %in% c('5'), CO2_flux<0.065), aes(x = CO2_flux, y = O2_flux)) +
  geom_point(shape = 1, color = 'gray') +
  ylab(expression(O[2]~mol/m^2/day)) +
  xlab(expression(CO[2]~mol/m^2/day)) +
  geom_smooth(aes(color = slope_type, group = interaction(day)),
              method = "lm", se = FALSE) +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", color = "darkred") +
  theme(legend.position = "bottom")


