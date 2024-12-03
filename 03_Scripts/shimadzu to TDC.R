#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(lme4)


vials<-data.frame()
file.names <- list.files(path="01_Raw_data/Shimadzu/ID", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  sample <- read_csv(fil)
  manipulate<-sample%>% mutate(Date=mdy(`Sample Date`), Ran=mdy(Ran), Vial=as.character(Vial))
  manipulate<-manipulate[,c(1:6)]

  vials<-rbind(vials, manipulate)}


calibrations<-data.frame()
results<-data.frame()

file.names <- list.files(path="01_Raw_data/Shimadzu/dat files/detailed", pattern=".txt", full.names=TRUE)
for(fil in file.names){

  runs<-read_delim(fil,delim = "\t", escape_double = FALSE,
                   trim_ws = TRUE, skip = 10)

  cals<-runs %>% filter(`Sample Name` =='Untitled'| `Sample Name`=='NPOC_Saline_100mgL') %>%
    mutate(Date= mdy_hms(`Date / Time`)) %>%mutate(day=as.Date(Date))%>%
    select(`Anal.`, `Mean Area`, `Conc.`, day)%>%
    rename(Analyte=`Anal.`, Area=`Mean Area`, Conc=`Conc.`)%>%

    group_by(day, Analyte) %>%
    summarise(model = list(lm(Conc ~Area , data = cur_data())),.groups = "drop") %>%
    mutate(coeffs = map(model, ~ broom::tidy(.x) %>%
                          select(term, estimate) %>%
                          pivot_wider(names_from = term, values_from = estimate)), r_squared = map_dbl(model, ~ summary(.x)$r.squared)  # Extract R-squared
  ) %>%
  unnest(cols = coeffs) %>%
  rename(intercept = `(Intercept)`, slope = Area) %>%
  select(day, Analyte, intercept, slope, r_squared) %>%
  rename(Date = day)

  calibrations<-rbind(calibrations, cals)


  result<-runs%>% filter(`Sample Name` != "Untitled"| `Sample Name`=='NPOC_Saline_100mgL', `Inj. No.` == 1)%>%
    select(`Sample Name`, `Analysis(Inj.)`, `Mean Area`, `Mean Conc.`)%>%
    rename("SampleID"="Sample Name", "Analyte"="Analysis(Inj.)", "Area"="Mean Area", "Conc"="Mean Conc.")%>%
    separate(SampleID, into = c("Site", "Date","Rep"), sep = "_")%>%
    mutate(Date=mdy(Date))

  TC<-result %>%filter(Analyte=='TC')%>%select(Site, Date, Rep,Conc,Area)%>%
    rename("TC.area"="Area", "TC.conc"="Conc")

  IC<-result %>%filter(Analyte=='IC')%>%rename("IC"="Analyte")%>%select(Site, Date, Rep,Conc,Area)%>%
    rename("IC.area"="Area", "IC.conc"="Conc")

  NPOC<-result %>%filter(Analyte=='NPOC')%>%rename("NPOC"="Analyte")%>%select(Site, Date, Rep,Conc,Area)%>%
    rename("NPOC.area"="Area", "NPOC.conc"="Conc")


  TOC<-left_join(TC, IC, by=c("Site", "Date","Rep"))
  combined<-left_join(TOC, NPOC, by=c("Site", "Date","Rep"))

  results<-rbind(results, combined)
}

#Save calibrations from runs######
calibrations<-calibrations%>% filter(r_squared>0.99)
IC_cals<-calibrations%>% filter(Analyte=='IC')
TC_cals<-calibrations%>% filter(Analyte=='TC')
NPOC_cals<-calibrations%>% filter(Analyte=='NPOC')

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
addWorksheet(wb, "Sheet2")
addWorksheet(wb, "Sheet3")

writeData(wb, sheet = "Sheet1", x = IC_cals)
writeData(wb, sheet = "Sheet2", x = TC_cals)
writeData(wb, sheet = "Sheet3", x = NPOC_cals)

saveWorkbook(wb, "01_Raw_data/Shimadzu/calcurves.xlsx", overwrite = TRUE)
######

#interpolate results######
results<- results%>%filter(!is.na(Date))%>%
  mutate(IC = if_else(Date < '2024-11-20', IC.area*0.37+0.479, IC.area),
        TC = if_else(Date < '2024-11-20', TC.area*0.2989-0.234, TC.area),
         NPOC = if_else(Date < '2024-11-20', NPOC.area*0.297-0.133, NPOC.area))%>%
  mutate(OC=TC-IC)%>% select(Date, Site, Rep, IC, TC, OC, NPOC)

dissolved <- results %>%filter(if_all(c(Rep), is.na))%>%select(-Rep)

particulate<-results %>% filter(Rep != is.na(Rep)) %>% group_by(Site, Date)%>%
  mutate(TOC_avg=mean(OC, na.rm=T), TIC_avg=mean(IC, na.rm=T))%>%
  distinct(Site, Date, TOC_avg, .keep_all = TRUE) %>%
  select(Site, Date, TOC_avg, TIC_avg)

all<-left_join(dissolved, particulate, by=c('Date', 'Site'))
final<-all%>% mutate(POC=TOC_avg-OC, PIC=TIC_avg-IC)%>% rename(DOC=OC, DIC= IC)%>%
  select(Date, Site, DOC, DIC, POC, PIC)
##########


carbon<-final %>% mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                    Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                    Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',
                                    Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                    Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                    Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                    Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                    Site=='9GW4'~'9',Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
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
                                            Site=='9GW4'~'RC',

                                            Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                            Site=='5.4'~'long',Site=='5.5'~'long',Site=='5.6'~'long',

                                            Site=='6.1'~'long',Site=='6.2'~'long',
                                            Site=='6.3'~'long',Site=='3.1'~'long',Site=='3.2'~'long',Site=='3.3'~'long',
                                            Site=='3.4'~'long',

                                            Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                            Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

carbon$chapter[is.na(carbon$chapter)]<-'wetland'
wetland<-filter(carbon, chapter=='wetland')

write_csv(wetland, "04_Output/TC_wetland.csv")

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,6,8)]
data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)
bgc<-join_all(data, by=c('Date','ID'), type='left')
detach("package:plyr", unload = TRUE)

bgc<-bgc %>%mutate(Date=as.Date(Date))%>%group_by(Date,ID)%>%
  mutate(depth=mean(depth, na.rm=T), pH=mean(pH, na.rm=T), Q==mean(Q, na.rm=T)) %>%
           select(Date,ID,depth,pH,Q)

carbon<-left_join(carbon, bgc,by=c('Date','ID'))

carbon<- carbon %>% filter(ID != '9a',ID != '9b', ID!='14') %>% mutate(mmol= Conc./44.01)
#WHERE I STOPPED####carbon <-carbon[!duplicated(carbon[c('Site','Date','Species')]),]

stream<-filter(carbon, chapter=='stream')
RC<-filter(carbon, chapter=='RC')
long<-filter(carbon, chapter=='long')

str(long)
write_csv(RC, "04_Output/TDC_RC.csv")
write_csv(stream, "04_Output/TDC_stream.csv")
write_csv(long, "04_Output/TDC_long.csv")

RC<-read.csv("04_Output/TDC_RC.csv")
stream<-read.csv("04_Output/TDC_stream.csv")
long<-read.csv("04_Output/TDC_long.csv")


ggplot(stream, aes(x=depth, y=mmol,color=Species)) +
  geom_point(size=2)+facet_wrap(~ Site, ncol=5, scales = "free")

ggplot(long, aes(x=location, y=Conc.,color=Species)) +
  geom_point(size=2)+facet_wrap(~ ID, ncol=5)+ylab("longitudinal sampling")

#check#####
TOC <- read_csv("01_Raw_data/Shimadzu/dat files/duds/2024_07_11_Howley_TOC_STREAMS_norm.txt",
                                                skip = 10)
write_csv(TOC, "01_Raw_data/Shimadzu/csv files/TOC_07112024.csv")

