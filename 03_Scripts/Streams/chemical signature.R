library(tidyverse)
library(writexl)
library(readxl)
library(cowplot)
library(plotly)
library(ggpmisc)
library(lme4)


file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,4,6,7,8,11)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
chem <- reduce(data, full_join, by = c("ID", 'Date'))

chem_daily<-chem%>% mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(
    DO=mean(DO, na.rm=T),
    CO2=mean(CO2, na.rm=T),
    Q=mean(Q, na.rm=T),
    depth=mean(depth, na.rm=T))%>% select(-Temp_PT.x, -Temp_PT.y)%>%
  distinct(Date, ID, .keep_all=T)


ggplot(chem_daily %>% filter(!ID %in% c('14', NA), Q>0.1),
       aes(x=Q, y=CO2,color=ID)) +
  #scale_y_log10()+scale_x_log10()+
  geom_point()+theme(axis.title.x=element_blank())+ facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(x = Q, y = CO2,
        label = paste(..p.value.label..,..eq.label.. , sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4,
    label.x.npc = "right",label.y.npc = 0.017, vstep=0.07)

chem_daily%>% group_by(ID)%>%
  summarise(
    meanCO2=mean(CO2, na.rm = T),
    meanDO=mean(DO, na.rm=T),
    medianCO2=median(CO2, na.rm = T),
    medianDO=median(DO, na.rm = T)
  )

summary(lmList(CO2 ~ Q | ID, data=chem_daily))
summary(lmList(DO ~ Q | ID, data=chem_daily))

ggplot(chem_daily %>% filter(!ID %in% c('14', NA), Q>0.1),
       aes(x=Q, y=DO,color=ID)) +
  #scale_y_log10()+scale_x_log10()+
  geom_point()+theme(axis.title.x=element_blank())+ facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(x = Q, y = DO,
        label = paste(..p.value.label..,..eq.label.. , sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4,
    label.x.npc = "right",label.y.npc = 0.017, vstep=0.07)
