library(tidyverse)
library(hydroTSM)
library(patchwork)
library(gganimate)
library(gifski)
library(png)
library(lubridate)

theme_set(theme(    strip.text = element_text(size = 12),
                    axis.title.y = element_text(size=13, angle=90),
                    axis.title.x = element_text(size=13),
                    axis.text.x = element_text(size=12),
                    axis.text.y = element_text(size=12),
                    panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                    panel.grid.minor.y = element_blank(),
                    panel.background = element_rect(fill = 'white'),
                    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


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

Q.axis<-xlab("Discharge (L/sec)")
CO2.axis<-ylab(expression(CO[2]~ppm))


common.layers<-list(
  geom_point(data = hourly_df, aes(x = Q, y = CO2), color = "gray"),
  scale_x_log10(),scale_y_log10(),
  facet_wrap(~ID, scales='free'),
  theme(legend.position = 'bottom')
)

# plot<-ggplot() +  common.layers+
#   geom_point(data = daily_df, aes(x = Q, y = CO2, color = Temp_PT))+
#   transition_states(ssn, transition_length = 0.4, state_length = 0.3)+ shadow_mark()+
#   ggtitle("Log-Log C-Q Relationship")
# animated_img <- animate(plot, width = 2000, height = 600, fps=20)
# anim_save("C-Q.gif", animated_img)


ggplot() +  common.layers+
  geom_point(data = daily_df, aes(x = Q, y = CO2, color = ssn), shape=1)+
  stat_ellipse(data = daily_df, aes(x = Q, y = CO2, color = ssn), linewidth=1)+
    Q.axis+CO2.axis

library(ggpmisc)
ggplot(data = daily_df, aes(x = Temp_PT, y = CO2)) +
  geom_point(data = hourly_df %>% filter(Temp_PT < 100), aes(x = Temp_PT, y = CO2), color = 'gray') +
  geom_point(shape=1) +
  facet_wrap(~ID, scales = 'free') +
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(label = paste(..p.value.label..,..eq.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4, label.x.npc = "right", label.y.npc = 0.017, hstep = 0.2
  )

ggplot() +
  geom_point(data = hourly_df%>% filter(Temp_PT<100), aes(x = Q, y = Temp_PT), color = 'gray')+
  geom_point(data = daily_df, aes(x = Q, y = Temp_PT))+
  scale_x_log10()+facet_wrap(~ID, scales='free')


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
