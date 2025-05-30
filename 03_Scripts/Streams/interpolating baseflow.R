rm(list=ls())

library(tidyverse)
library(grwat)
library(ggpmisc)
library(cowplot)
library(ggplot2)
library(hydrostats)

discharge<-read.csv('02_Clean_data/discharge.csv')%>% mutate(Date= ymd_hms(Date))
str(discharge)


test<-discharge %>% filter(ID=='5')

bf_test<-test%>%baseflows(a=0.92, ts="daily")%>%
  mutate(difference=Q-bf)

ggplot(bf_test, aes(Date)) +
  geom_line(aes(y=difference), color = 'black')+
  geom_line(aes(y=Q), color = 'steelblue') +
  geom_line(aes(y=bf), color = 'orangered')


hdata = test %>%
  mutate(lynehollick = gr_baseflow(Q, method = 'lynehollick', a = 0.9),
         boughton = gr_baseflow(Q, method = 'boughton', k = 0.9),
         jakeman = gr_baseflow(Q, method = 'jakeman', k = 0.9),
         maxwell = gr_baseflow(Q, method = 'maxwell', k = 0.9)) %>%
  pivot_longer(lynehollick:maxwell, names_to = 'Method', values_to = 'Qbase')%>%
  mutate(Date=as.Date(Date))

ggplot(hdata, aes(Date)) +
  geom_line(aes(y=Q), color = 'steelblue') +
  geom_line(aes(y=Qbase), color = 'orangered') +
  facet_wrap(~Method)+
  scale_y_log10()


