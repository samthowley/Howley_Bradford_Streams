library(tidyverse)
library(ggpmisc)
library(cowplot)
library(ggplot2)


fatble <- read_csv("fatble.csv", col_select = 1:4)%>%
  rename('distance'='distance...4', 'Site'="Site...2")%>%
  group_by(ID)%>%
  arrange(ID, distance)%>%
  mutate(fraction_Q=(lag(RASTERVALU)-RASTERVALU)/RASTERVALU[Site=='0'])

bf <- read_csv("04_Output/baseflow.csv")


