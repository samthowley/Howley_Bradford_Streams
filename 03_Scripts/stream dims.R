
#Edit dims######

depth<-read_csv('02_Clean_data/depth.csv')%>%
  mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(depth=mean(depth, na.rm = T)) %>%
  select(Date, ID, depth)%>%distinct(Date, ID, .keep_all = T)

Q<-read_csv('02_Clean_data/discharge.csv')%>%
  mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T)) %>%
  select(Date, ID, Q)%>%
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

bf<-baseflow%>%
  group_by(ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    bf = rollmean(baseflow, k = 25, fill = NA, align = "center", na.rm=T)) %>%
  ungroup()%>%
  select(Date, ID, bf)





Q_h<-left_join(Q, depth)
w<-left_join(Q_h, velocity)%>% mutate(width=Q/u*depth)
flow_regime<-full_join(w, bf, by=c('Date', 'ID'))

write_csv(flow_regime, "04_Output/flow_regime_daily.csv")

