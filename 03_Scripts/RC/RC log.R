library(tidyverse)

WTdepth<-RC_log <- read_excel("01_Raw_data/RC log.xlsx",
                              sheet = "RC log")%>%
  rename("surface2WT"="Wtdepth (m)") %>% select(Date, ID, Site, surface2WT)

RC_distance <- read_excel("01_Raw_data/RC log.xlsx",sheet = "Sheet1")%>%
  select(Site,Distance_m, DistanceID)

RC_elevations <- read_excel("01_Raw_data/RC log.xlsx",sheet = "elevations")%>%
  arrange(ID, surface_elevation_m) %>%
  group_by(ID) %>%
  mutate(WT.ID = row_number() - 1) %>%
  ungroup()%>%
  select(Site, surface_elevation_m,WT.ID)

RC_dims<-full_join(WTdepth, RC_distance, by=c('Site'))
RC_dims<-full_join(RC_dims, RC_elevations, by=c('Site'))%>%
  mutate(WT_elevations=surface_elevation_m-surface2WT)

well_types <- data.frame(
  Site = c('5GW0', '5GW1', '5GW2', '5GW3', '5GW4', '5GW5', '5GW6', '5GW7', '5GW8',
           '6GW0', '6GW1', '6GW2', '6GW3', '6GW4', '6GW5',
           '9GW0', '9GW1','9GW2', '9GW3', '9GW4', '9GW5'),
  well_types = c('stream', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', "upland",
                 'stream', 'RW', 'RW', 'RW', 'RW', "upland",
                 'stream', 'RW', 'RW', 'RW', 'RW', "upland"))
RC_dims<-left_join(RC_dims, well_types)%>%mutate(ID=as.character(ID))

stream.flow <- read_csv("04_Output/flow_regime_daily.csv")%>%
  filter(ID %in% c('5', '6', '9'))%>%
  select(Date, ID, qL_m2.sec, width, depth)%>%
  rename(stream.w=width, stream.depth=depth)

RC.regime<-left_join(RC_dims, stream.flow)%>%
  arrange(Site, Date)%>%
  filter(!is.na(Site))


write_csv(RC.regime, "04_Output/RC.flow.regime.csv")
