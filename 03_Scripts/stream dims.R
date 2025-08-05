
#Edit dims######

depth<-read_csv('02_Clean_data/depth.csv')%>%
  mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(depth=mean(depth, na.rm = T), Water_press=mean(Water_press, na.rm = T)) %>%
  select(Date, ID, depth, Water_press)%>%distinct(Date, ID, .keep_all = T)

Q<-read_csv('02_Clean_data/discharge.csv')%>%
  mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),
         Q_m3.s=Q/10^3) %>%
  select(Date, ID, Q, Q_m3.s)%>%
  distinct(Date, ID, .keep_all = T)

velocity <- read_csv("02_Clean_data/velocity.csv")%>%select(Date, ID, u)%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(u=mean(u, na.rm=T))%>%
  distinct(Date, ID, .keep_all = T)

baseflow <- read_csv("04_Output/baseflow.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, ID) %>%
  mutate(baseflow = mean(baseflow, na.rm = TRUE)) %>%
  distinct(Date, ID, .keep_all = TRUE) %>% ungroup()

library(zoo)
bf<-baseflow%>%
  group_by(ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    bf = rollmean(baseflow, k = 25, fill = NA, align = "center", na.rm=T)) %>%
  ungroup()%>%
  select(Date, ID, bf)

K600 <- read_csv("04_Output/master_metabolism.csv")%>%
  select(ID, date, K600)%>% rename(Date=date)

Q_h<-left_join(Q, depth)

w<-left_join(Q_h, velocity)%>% mutate(width=Q_m3.s/(u*depth))

reach<-left_join(w, K600)%>%
  group_by(ID)%>%
  mutate(K600=if_else(is.na(K600), mean(K600, na.rm=T), K600))%>%
           ungroup()%>%
  mutate(reach=(0.7*(u*86400)/K600),
         A=reach*width)

flow_regime<-full_join(reach, bf, by=c('Date', 'ID'))

write_csv(flow_regime, "04_Output/flow_regime_daily.csv")

ggplot(w, aes(x=Q, y=width))+
  geom_point()+
  facet_wrap(~ID, scales="free")


