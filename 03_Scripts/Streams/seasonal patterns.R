library(tidyverse)
library(hydroTSM)
library(patchwork)
library(gganimate)
library(gifski)
library(png)
library(lubridate)


int.ext <- read_csv( "04_Output/external-internal.csv")%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"), month=month(Date))

mean_daily <- function(file, value_col) {
  read_csv(file) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date, ID) %>%
    summarise("{value_col}" := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
}

CO2 <- mean_daily("02_Clean_data/CO2_cleaned.csv", "CO2")
DO <- mean_daily("02_Clean_data/DO_cleaned.csv", "DO")
temp <- mean_daily("02_Clean_data/temperature.csv", "Temp_PT")
Q<- mean_daily("04_Output/flow_regime_daily.csv", "Q")
depth<- mean_daily("04_Output/flow_regime_daily.csv", "depth")


df_list <- list(CO2, DO, Q, depth,temp)
daily_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"))%>%
  filter(!ID %in% c('14', NA_real_))%>%
  mutate(month=month(Date))


Q<- read_csv("02_Clean_data/discharge.csv")
depth<- read_csv("02_Clean_data/depth.csv")
CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")
temperature <- read_csv("02_Clean_data/temperature.csv")
DO <- read_csv("02_Clean_data/DO_cleaned.csv")

df_list <- list(CO2, DO, Q, depth)
hourly_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(hourly="hourly cloud")%>%filter(!ID %in% c('14', NA_real_))

common.layers<-list(
    scale_x_log10(),scale_y_log10(),
    ggtitle("Log-Log C-Q Relationship"),
  facet_wrap(~ID, scales='free'),
  transition_states(ssn, transition_length = 0.4, state_length = 0.3),
  shadow_mark()
)

plot<-ggplot() +
  geom_point(data = hourly_df, aes(x = Q, y = CO2), color = "gray") +
  geom_point(data = daily_df, aes(x = Q, y = CO2, color = ssn)) +
  common.layers
animated_img <- animate(plot,
                        width = 2000, height = 600, fps=20)
anim_save("C-Q.gif", animated_img)


ggplot() +
  geom_point(data = hourly_df, aes(x = Q, y = CO2), color = "gray") +
  geom_point(data = daily_df, aes(x = Q, y = CO2, color = month), shape=1) +
  scale_y_log10()+scale_x_log10()+
  scale_color_gradient(high='orange', low='blue')+
  theme(legend.position = 'none')+facet_wrap(~ID, scales='free')


site_plots <- daily_df %>%
  group_split(ID) %>%
  lapply(function(df) {
    temp_range <- range(df$Temp_PT, na.rm = TRUE)
    ggplot(df, aes(x=Q, y=CO2, color=Temp_PT)) +
      scale_color_gradient(low='blue', high='red', limits=temp_range) +
      geom_point(shape=1) +
      scale_x_log10() + scale_y_log10() +
      ggtitle(paste("Site", df$ID[12]))
  })
wrap_plots(site_plots)

#sampled carbon###########
sampledC <- read_csv("04_Output/stream_sampledC.csv")%>%left_join(flow_regime)%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"))

ggplot(sampledC, aes(x = Q, y=DOC ,color= ssn)) +
  common.layers
#O2:CO2 relationship###########
vachon <- read_csv("04_Output/fluxes.csv")%>% mutate(ssn=time2season(Date, out.fmt="seasons"),
                                                     month=month(Date))

common.layers.vachon<-list(
  geom_point(shape = 1),
  ylab(expression(O[2]~mol/m^2/day)),
  xlab(expression(CO[2]~mol/m^2/day)),
  facet_wrap(~ ID, ncol = 3, scales = 'free'),
  theme(legend.position = "none")
)

#check
ggplot(vachon, aes(x=CO2_flux, y=O2_flux, color=month)) +
  common.layers.vachon+
  scale_color_gradient(high='orange', low='blue')

anim_save("vachon.gif",
          ggplot(vachon, aes(x=CO2_flux, y=O2_flux, color=ssn)) +
            common.layers.vachon+
            transition_states(ssn, transition_length = 0.4, state_length = 0.3)+
            shadow_mark(),
          renderer = gifski_renderer(), width = 1000, height = 500)

#internal v external####
common.layers.pathway <- list(
  geom_point(shape = 1),
  xlab(expression(Discharge ~ L/sec)),
  facet_wrap(~ID, ncol = 3, scales = 'free'),
  scale_y_log10(), scale_x_log10(),
  theme(legend.position = "none")
)

ggplot(int.ext, aes(x = Q, y = active.passive, color = month)) +
  common.layers.pathway +
  scale_color_gradient(high = 'orange', low = 'blue')

ggplot(int.ext, aes(x = passive, y = active, color = month)) +
  common.layers.pathway +
  scale_color_gradient(high = 'orange', low = 'blue')
