#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(lme4)
library(ggpmisc)
theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                axis.text.y = element_text(size = 17, angle=0),
                axis.title =element_text(size = 17, angle=0),
                plot.title = element_text(size = 17, angle=0),
                legend.key.size = unit(0.8, 'cm'),
                legend.text=element_text(size = 17),
                legend.title =element_text(size = 17),
                legend.position = 'bottom',
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_line(color = "gray", size = 0.5)))

NPOC_all<-data.frame() #call in dat files
TOC_all<-data.frame() #call in dat files

file.names <- list.files(path="01_Raw_data/Shimadzu/dat files/detailed", pattern=".txt", full.names=TRUE)
for(fil in file.names){

  runs<-read_delim(fil,delim = "\t", escape_double = FALSE,
                   trim_ws = TRUE, skip = 10)

  result<-runs%>% filter(`Sample Name` != "Untitled"| `Sample Name`=='NPOC_Saline_100mgL', `Inj. No.` == 1)%>%
    select(`Sample Name`, `Analysis(Inj.)`, `Mean Area`, `Mean Conc.`)%>%
    rename("SampleID"="Sample Name", "Analyte"="Analysis(Inj.)", "Area"="Mean Area", "Conc"="Mean Conc.")%>%
    separate(SampleID, into = c("Site", "Date","Rep"), sep = "_")%>%
    mutate(Date=mdy(Date))

  TC<-result %>%filter(Analyte=='TC')%>%select(Site, Date, Rep,Conc,Area)%>%
    rename("TC.area"="Area", "TC.conc"="Conc")

  IC<-result %>%filter(Analyte=='IC')%>%rename("IC"="Analyte")%>%select(Site, Date, Rep,Conc,Area)%>%
    rename("IC.area"="Area", "IC.conc"="Conc")

  NPOC<-result %>%filter(Analyte=='NPOC',Site!='NPOC')%>%rename("NPOC"="Analyte")%>%select(Site,Date,Rep,Conc,Area)%>%
    rename("NPOC.area"="Area", "NPOC.conc"="Conc")

  TOC<-full_join(TC, IC, by=c("Site", "Date","Rep"))
  TOC<-TOC%>%select(-Rep)
  TOC_all<-rbind(TOC_all, TOC)

  NPOC_all<-rbind(NPOC_all, NPOC)
}

csv<-data.frame() #call in csv files
file.names <- list.files(path="01_Raw_data/Shimadzu/csvs/manipulated", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  run <- read_csv(fil)
  run<-run %>% rename('Date'='Sample Date', 'DOC'='NPOC')%>% mutate(Date=mdy(Date), DIC= NA)%>%
    select(Date, Site, DIC, DOC)

  csv<-rbind(csv, run)
}


dissolved_NPOC<-NPOC_all %>% filter(!Rep %in% c("1", "2", "3"))%>%
  mutate(DOC = if_else(Date < '2025-12-20', NPOC.area*0.297-0.133, NPOC.area), DIC=NA)%>%
  select(Date, Site, DOC)


dissolved_TOC<- TOC_all%>%filter(!is.na(Date))%>% #incorporating calibrations
  mutate(DIC = if_else(Date < '2025-12-20', IC.area*0.37+0.479, IC.area),
         DOC = if_else(Date < '2025-12-20', TC.area*0.2989-0.234, TC.area))

DOC<-dissolved_TOC%>%select(Date, Site, DOC)
DOC_NPOC<-rbind(DOC,dissolved_NPOC)

DIC<-dissolved_TOC%>%select(Date, Site, DIC)
DC<-full_join(DIC, DOC_NPOC, by = c("Date","Site"))

dissolvedC_csv<-rbind(DC, csv)

particulate_NPOC<-NPOC_all %>% filter(Rep %in% c("1", "2", "3"))%>%
  mutate(TNPOC = if_else(Date < '2025-12-20', NPOC.area*0.297-0.133, NPOC.area))%>%
  group_by(Site, Date)%>%
    mutate(NPTOC=mean(TNPOC, na.rm=T))%>% distinct(Date,Site, .keep_all = T) %>%select(Site,Date,TNPOC)

sampledC<-full_join(dissolvedC_csv,particulate_NPOC, by=c('Date','Site'))
sampledC<-sampledC %>% mutate(POC=abs(TNPOC-DOC))%>%arrange(Date)%>%filter(DOC != is.na(DOC))


##########

carbon<-sampledC %>% mutate(Site= if_else(Site=='5,5','5.5', Site))%>%
  mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                    Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                    Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',

                                    Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                    Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                    Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                    Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                    Site=='9GW4'~'9',Site=='9GW5'~'9',Site=='5GW8'~'5',

                                    Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
                                    Site=='5.4'~'5',Site=='5.5'~'5',Site=='6.1'~'6',Site=='6.2'~'6',
                                    Site=='6.3'~'6',Site=='6.4'~'6',Site=='6.5'~'6',Site=='6.6'~'6',
                                    Site=='9.1'~'9',Site=='9.2'~'9',Site=='9.3'~'9',Site=='9.4'~'9',
                                    Site=='9.5'~'9',Site=='9.6'~'9',Site=='9.Sam'~'9'))


carbon<-carbon %>% mutate(chapter=case_when(Site=='3'~'long',Site=='5'~'long',Site=='5a'~'stream',
                                            Site=='6'~'long',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'long',Site=='13'~'stream',Site=='15'~'stream',

                                            Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                            Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='5GW8'~'RC',

                                            Site=='6GW1'~'RC',
                                            Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                            Site=='6GW6'~'RC',

                                            Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                            Site=='9GW4'~'RC',Site=='9GW5'~'RC',

                                            Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                            Site=='5.4'~'long',Site=='5.5'~'long',Site=='5.6'~'long',

                                            Site=='6.1'~'long',Site=='6.2'~'long',
                                            Site=='6.3'~'long',Site=='3.1'~'long',Site=='3.2'~'long',Site=='3.3'~'long',
                                            Site=='3.4'~'long',

                                            Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                            Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

carbon$chapter[is.na(carbon$chapter)]<-'wetland'

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,4,6,8)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
bgc <- reduce(data, full_join, by = c("ID", 'Date'))

bgc_edit<-bgc %>%
  mutate(Date=as.Date(Date))%>%arrange(ID,Date) %>% group_by(Date,ID)%>%
  mutate(depth=mean(depth, na.rm=T), pH=mean(pH, na.rm=T), Q==mean(Q, na.rm=T)) %>%
  fill(Q, .direction = 'down')%>%distinct(Date, ID, .keep_all = T)%>%
  select(Date,ID,depth,pH,Q, CO2, Temp_pH)

carbon<-full_join(carbon, bgc_edit,by=c('Date','ID'))
carbon<-carbon %>% mutate(Date = if_else(Date == as.Date("2004-05-08"), as.Date("2024-05-08"), Date))%>%
                   mutate(Date = if_else(Date == as.Date("2004-06-14"),as.Date("2024-06-14") ,Date))

RC<-filter(carbon, chapter=='RC')%>%
  mutate(Site=if_else(Site=='5GW7'&Date=="2023-01-30", '5GW8', Site))

long<-filter(carbon, chapter=='long')
long <- long %>%
  mutate(Site = if_else(Site == "5", "5.5", Site),
         Site = if_else(Site == "3", "3.1", Site),
         Site = if_else(Site == "6", "6.2", Site),
         Site = if_else(Site == "9", "9.5", Site))

wetland<-filter(carbon, chapter=='wetland')

forstream <- carbon %>%
  mutate(Site = if_else(Site == "6.2", "6", Site),
         Site = if_else(Site == "5.5", "5", Site),
         Site = if_else(Site == "9.5", "9", Site),
         Site = if_else(Site == "3.1", "3", Site))
stream<-forstream %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                            Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream'))%>%
  filter(chapter=='stream')

write_csv(RC, "04_Output/TDC_RC.csv")
write_csv(stream, "04_Output/TDC_stream.csv")
write_csv(long, "04_Output/TDC_long.csv")

#################################
#Figures#########################
#################################
stream<-read_csv("04_Output/TDC_stream.csv")%>%filter(!ID=='6a')
DOC<-stream %>% select(Site, DOC, Q)%>% rename(C=DOC)%>%mutate(type='DOC')
DIC<-stream %>% select(Site, DIC, Q)%>% rename(C=DIC)%>%mutate(type='DIC')
POC<-stream %>% select(Site, POC, Q)%>% rename(C=POC)%>%mutate(type='POC')
for_figs<-rbind(POC, DOC, DIC)


DOC$Site <- factor(DOC$Site , levels=c('5','5a','6','3','7','13','6a','9','15'))

ggplot(DOC, aes(x=Q, y=C, color=type)) +
  geom_point(size=2)+
  ylab('mg/L')+
  xlab(expression('Discharge'~m^3))+
  facet_wrap(~ Site, ncol=3, scales = "free")+
  scale_x_log10()+scale_y_log10()+
  stat_poly_line(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label..,..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = "bottom"
  )
#
for_figs$Site <- factor(for_figs$Site , levels=c('5','5a','6','3','7','13','6a','9','15'))

ggplot(for_figs, aes(x=Q, y=C, color=type)) +
  geom_point(size=2)+
  ylab('mg/L')+
  xlab(expression('Discharge'~m^3))+
  facet_wrap(~ Site, ncol=3, scales = "free")+
  scale_x_log10()+scale_y_log10()+
  stat_poly_line(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(label = ifelse(..p.value.. < 0.05,
                       paste("bold(", ..eq.label.., ") *','~", ..rr.label.., "*','~bold(", ..p.value.label.., ")"),
                       paste(..eq.label.., "*','~", ..rr.label.., "*','~", ..p.value.label..))),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = "bottom"
  )
#
DOC$Site <- factor(DOC$Site , levels=c('5','15','3','7','9'))

label_data_DOC <- DOC %>%
  group_by(Site) %>%
  summarise(
    mean_val = mean(C, na.rm = TRUE),
    min_val = min(C, na.rm = TRUE),
    max_val = max(C, na.rm = TRUE),
    Q_val = min(Q, na.rm = TRUE)) %>%
  mutate(
    label_other = paste0("Min: ", round(min_val, 2), "\nMax: ", round(max_val, 2)),
    label_mean = paste0("Mean: ", round(mean_val, 2))
  )


a<-ggplot(DOC %>%filter(Site %in% c('5','15','3','7','9')), aes(x=Q, y=C)) +
  geom_point(size=2)+
  ylab('DOC mg/L')+
  xlab(expression('Discharge'~m^3))+
  facet_wrap(~ Site, ncol=5, scales = "free")+
  scale_x_log10()+scale_y_log10()+
  stat_poly_line(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label..,..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = "bottom")+
  geom_text(data = label_data_DOC%>%filter(Site %in% c('5','5a','6')),
            aes(x = Q_val, y = max_val, label = label_other),
            inherit.aes = FALSE,
            hjust = 0, vjust = 1.5, size = 3, color = "darkblue") +

  # Red and bold label for mean
  geom_text(data = label_data_DOC%>%filter(Site %in% c('5','15','3','7','9')),
            aes(x = Q_val, y = max_val, label = label_mean),
            inherit.aes = FALSE,
            hjust = 0, vjust = 0.3, size = 4, color = "red", fontface = "bold")




ggplot(stream, aes(x=Date, y=DOC)) +
  geom_point(size=2)+facet_wrap(~ Site, ncol=5, scales = "free")+
  scale_y_log10()+ geom_smooth(method='lm')
#
ggplot(stream, aes(x=Q, y=DIC)) +
  geom_point(size=2)+facet_wrap(~ Site, ncol=5, scales = "free")+
  scale_x_log10()+scale_y_log10()+ geom_smooth(method='lm')
#
ggplot(stream, aes(x=depth)) +
  geom_point(aes(y=DOC, color='DOC'), size=2)+
  geom_point(aes(y=POC,color='POC'), size=2)+
  geom_point(aes(y=DIC,color='DIC'), size=2)+
  facet_wrap(~ Site, ncol=5, scales = "free")+scale_x_log10()+scale_y_log10()


#Checks#########
test<-dissolved %>% filter(complete.cases(NPDOC, DOC))%>%
  distinct(Date, Site, .keep_all = T)%>%
  mutate(UniqueID = group_indices(., as.factor(Date), Site))

ggplot(test, aes(x=Site, group=as.factor(Date))) +
  geom_point(aes(y=NPDOC,color='NPDOC'), size=2)+
  geom_point(aes(y=DOC,color='DOC'), size=2)+ylab('mg/L')

DIC<-dissolved %>% select(Date, Site, DIC)%>% rename('measured_DIC'='DIC')

alkalinity <- read_csv("02_Clean_data/alkalinity.csv")
alkalinity<-alkalinity %>%mutate(DIC_interpolated = sum(c_across(c(CO2_mgL, HCO3_mgL, CO3_mgL))),
                                 Date=as.Date(Date)) %>%rename(Site=ID)

DIC_check<-full_join(DIC, alkalinity, by=c('Date', 'Site'))
DIC_check<-DIC_check %>% distinct('Date', 'Site', .keep_all = T)%>%
  filter(complete.cases(DIC_in, measured_DIC))
