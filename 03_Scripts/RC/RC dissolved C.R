library(tidyverse)
library(writexl)
library(readxl)

RC_flow_regime <- read_csv("04_Output/RC.flow.regime.csv")

TDC_RC <- read_csv("04_Output/TDC_RC.csv")%>%select(Date, Site, DIC, DOC)

dissolved.c<-left_join(RC_flow_regime, TDC_RC)%>%
  mutate(
    DOC_g.m2.day=DOC*qL_m2.sec*(1/stream.w)*86400,
    DIC_g.m2.day=DIC*qL_m2.sec*(1/stream.w)*86400

  )
