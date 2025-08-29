library(tidyverse)
library(hydroTSM)
library(patchwork)


int.ext <- read_csv( "04_Output/external-internal.csv")
int.ext.ssn <-int.ext%>% mutate(ssn=time2season(Date, out.fmt="seasons"))

flow_regime<- read_csv("04_Output/flow_regime_daily.csv")
CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")
temperature <- read_csv("02_Clean_data/temperature.csv")
DO <- read_csv("02_Clean_data/DO_cleaned.csv")

df_list <- list(CO2, temperature, DO, Q, depth)
combined_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"))

common.layers<-list(
  geom_point(size=1, stroke=1.5),
    scale_x_log10(),scale_y_log10(),
    ggtitle("Log-Log C-Q Relationship"),
    facet_wrap(~ID, scales='free')
)

ggplot(combined_df, aes(x = Q, y=CO2 ,color= ssn)) +
  common.layers

site_plots <- CO2.Q %>%
  group_split(ID) %>%
  lapply(function(df) {
    temp_range <- range(df$Temp_PT, na.rm = TRUE)
    ggplot(df, aes(x=Q, y=CO2, color=Temp_PT)) +
      scale_color_gradient(low='blue', high='red', limits=temp_range) +
      geom_point(size=1, stroke=1.5) +
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
vachon <- read_csv("04_Output/fluxes.csv")%>% mutate(ssn=time2season(Date, out.fmt="seasons"))

common.layers.vachon<-list(
  geom_point(shape = 1),
  ylab(expression(O[2]~mol/m^2/day)),
  xlab(expression(CO[2]~mol/m^2/day)),
  facet_wrap(~ ID, ncol = 3, scales = 'free'),
  theme(legend.position = "none")
)

site_plots <- CO2.Q %>%
  group_split(ID) %>%
  lapply(function(df) {
    temp_range <- range(df$Temp_PT, na.rm = TRUE)
    ggplot(df, aes(x=Q, y=CO2, color=Temp_PT)) +
      scale_color_gradient(low='blue', high='red', limits=temp_range) +
      common.layers.vachon+
      ggtitle(paste("Site", df$ID[12]))
  })
wrap_plots(site_plots)

ggplot(vachon, aes(x=CO2_flux, y=O2_flux, color=ssn)) +
  common.layers.vachon

dev.new()
