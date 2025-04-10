library(ggpubr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)

no_bins<- read_csv("04_Output/master_metabolism.csv")%>% rename(SM_K600=K600_daily_mean)%>% select(ID, Date, SM_K600)

binned<- read_csv("04_Output/metabolism_04092025.csv")%>%
  rename(bin_K600=K600_daily_mean, Date=date)%>%
  select(ID, Date, bin_K600)%>%
  mutate(ID=as.character(ID))

depth <- read_csv("02_Clean_data/depth.csv")%>% select(Date, depth, ID)
discharge <- read_csv("02_Clean_data/discharge.csv")%>% select(Date, Q, ID)
h_q<-left_join(depth, discharge, by=c('Date', 'ID'))

u <- read_csv("02_Clean_data/velocity.csv")%>% select(Date, u, ID)
huq<-left_join(h_q, u, by=c('Date', 'ID'))


stream_widths <- read_excel("01_Raw_data/stream widths.xlsx")%>%select(ID, width_m)
hqwu<-left_join(huq,stream_widths, by=c('ID'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(
    u=mean(u, na.rm=T),
    Q=mean(Q, na.rm=T),
    depth=mean(depth, na.rm=T)
  )%>% distinct(Date, ID, .keep_all = T)

froude<-hqwu%>%
  mutate(
    Froude=u/sqrt(9.81*depth))

compare<-left_join(binned, no_bins, by=c('Date','ID'))
compare<-left_join(compare, hqwu, by=c('Date','ID'))

ggplot(compare, aes(x=Q)) +
  geom_point(aes(y=bin_K600, color='binned in SM'))+
  geom_point(aes(y=SM_K600, color='SM interpolated'))+
  # geom_smooth(method = lm, x=Q, y=bin_K600)+
  # geom_smooth(method = lm, x=Q, y=SM_K600)+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ ID, ncol=5, scales = 'free')

