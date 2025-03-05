#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(lme4)
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
sampledC<-sampledC %>% mutate(POC=TNPOC-DOC)%>%arrange(Date)%>%filter(DOC != is.na(DOC))


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


carbon<-carbon %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                            Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream',

                                            Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                            Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='6GW1'~'RC',
                                            Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                            Site=='6GW6'~'RC',Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                            Site=='9GW4'~'RC',Site=='9GW5'~'RC',Site=='5GW8'~'RC',

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

stream<-carbon %>%filter(chapter=='stream')
RC<-filter(carbon, chapter=='RC')
long<-filter(carbon, chapter=='long')
wetland<-filter(carbon, chapter=='wetland')

ggplot(stream, aes(x=Q, y=DOC)) +
  geom_point(size=2)+facet_wrap(~ Site, ncol=5, scales = "free")+
  scale_x_log10()+scale_y_log10()+ geom_smooth(method='lm')
#
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

write_csv(RC, "04_Output/TDC_RC.csv")
write_csv(stream, "04_Output/TDC_stream.csv")
write_csv(long, "04_Output/TDC_long.csv")

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
