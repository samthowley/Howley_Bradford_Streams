library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(ggtern)

theme_set(theme(axis.text.x = element_text(size = 12),
                             axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 17, angle = 90),
                axis.title.x = element_text(size = 17),
                             plot.title = element_text(size = 17),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_blank(),
                             legend.position ="bottom",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(day=day(Date), month=month(Date), year=year(Date))%>%
  group_by(ID,day, month,year) %>%mutate(Q_daily=mean(Q, na.rm=T), loqQdaily=log(Q_daily)) %>%
  filter(Q_daily>2, ID !=14)

depth <- read_csv("02_Clean_data/depth.csv")

TC <- read_csv("02_Clean_data/allC_stream.csv")

chim_react<-read_csv("04_Output/chimney_reactor.csv")

master <- read_csv("master.csv")

#discharge: trends with depth, base vs surficial flow####
discharge<-filter(discharge, Qbase>0 & Q<3000)
(Q_timeseries<-ggplot(discharge, aes(Date))+
   geom_line(aes(y=Q, color= "Total Discharge")) +
   geom_line(aes(y=Qbase, color='Base Discharge')) +
  facet_wrap(~ ID, ncol=3)+
  ylab('Discharge (L/S)') + xlab("Date"))+
  scale_color_manual(values = c('blue','darkred'))

(depth_timeseries<-ggplot(depth, aes(Date))+
    geom_line(aes(y=depth)) + geom_hline(yintercept = 0)+
    facet_wrap(~ ID, ncol=5)+
    ylab('Depth (m)') + xlab("Date"))

depth<-depth %>% mutate(day=as.Date(Date))%>% group_by(ID,day) %>%
  mutate(depth_daily=mean(depth, na.rm=T))

#total Carbon####

TC$ID <- factor(TC$ID , levels=c('13','5','6a','15','5a','7','6','9','3'))


ggtern(data=TC,aes(DOC_mgL,DIC_total_mgL,POC_mgL)) +
  geom_point() +
  labs(x="DOC_mgL",y="DIC_mgL",z="POC_mgL")+facet_wrap(~ ID, ncol=3)

ggplot(TC, aes(x=Q)) +
  geom_point(aes(y=DIC_total_mgL, color='DIC')) +
  geom_point(aes(y=DOC_mgL, color='DOC')) +
  geom_point(aes(y=POC_mgL, color='POC'))+facet_wrap(~ ID, ncol=5)


POC<-TC %>% select(Date, ID, POC_mgL, Q) %>%rename('mgL'='POC_mgL') %>% mutate(Species='POC')
DOC<-TC %>% select(Date, ID, DOC_mgL, Q) %>%rename('mgL'='DOC_mgL') %>% mutate(Species='DOC')%>%
  filter(mgL<150)
DIC<-TC %>% select(Date, ID, DIC_total_mgL, Q) %>%rename('mgL'='DIC_total_mgL') %>%
  mutate(Species='DIC')

TC_vert<-rbind(POC,DOC, DIC)

ggplot(TC_vert, aes(x=ID, y=mgL, fill=Species))+
  geom_boxplot()+ ylab("mg/L")

ggplot(TC, aes(x=Q)) +scale_x_log10()+
  geom_point(aes(y=DIC_total_mgL, color='DIC'),size=2) +
  geom_point(aes(y=DOC_mgL, color='DOC'),size=2)+
  geom_point(aes(y=POC_mgL, color='POC'),size=2)+ylab("mg/L")+xlab('Discharge L/s')+ylim(0, 120)+
  facet_wrap(~ ID, ncol=5)

#metabolism: boxplots,TC_vert_low#metabolism: boxplots, Q vs GPP/ER, ####
metabolism <- read_csv("04_Output/master_metabolism.csv")

metabolism<-left_join(metabolism, discharge,by=c('ID','Date'))
metabolism<-left_join(metabolism, depth,by=c('ID','Date'))
metabolism<-metabolism %>% filter(Q_daily>1, ID !=14)

metabolism$ID <- factor(metabolism$ID , levels=c('13','5','6a','15','5a','7','6','9','3'))

ER_box<-ggplot(metabolism, aes(x=ID, y=ER)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  ylab('Ecosystem Respiration')
GPP_box<-ggplot(metabolism, aes(x=ID, y=GPP)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  ylab('Gross Primary Productivity')

ER_scatter<-ggplot(metabolism, aes(Q_daily, ER)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+geom_smooth(method='lm', se=FALSE)+
  ylab('ER')+ xlab("Discharge (L/s)")+theme(axis.title.x = element_blank())
GPP_scatter<-ggplot(metabolism, aes(Q_daily, ER)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+geom_smooth(method='lm', se=FALSE)+
  ylab('GPP') + xlab("Discharge (L/s)")+theme(axis.title.x = element_blank())

scatter<-plot_grid(ER_scatter,GPP_scatter, ncol=2)
boxplots<-plot_grid(ER_box,GPP_box, ncol=2)
plot_grid(scatter,boxplots, nrow=2)

#Chem: boxplots by ID, Q vs mg/L####
master<-master%>%filter(Q>1, ID != 14)

master$ID <- factor(master$ID , levels=c('13','5','6a','15','5a','7','6','9','3'))

DOQ_box<-ggplot(master, aes(x=ID, y=DO, fill= Q_ID)) +
  geom_boxplot(color="black")+
  ylab('Dissolved Oxygen (mg/L)')+scale_fill_manual(values = c('lightgreen','blue'))+
  theme(axis.title.x = element_blank())

pHQ_box<-ggplot(master, aes(x=ID, y=pH, fill= Q_ID)) +
  geom_boxplot(color="black")+
  ylab('pH')+scale_fill_manual(values = c('lightgreen','blue'), name="Discharge")+
  theme(axis.title.x = element_blank(),
        legend.position ="bottom")

CO2Q_box<-ggplot(master, aes(x=ID, y=CO2, fill= Q_ID)) +
  geom_boxplot(color="black")+
  ylab('CO2 (mg/L)')+scale_fill_manual(values = c('lightgreen','blue'))+
  theme(axis.title.x = element_blank())

ggplot(master, aes(Q, DO)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+geom_smooth(method='lm', se=FALSE)+
  ylab('DO mg/L') + xlab("Discharge (L/s)")+scale_x_log10()
CO2_scatter<-ggplot(master, aes(Q, CO2)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+geom_smooth(method='lm', se=FALSE)+
  ylab('CO2') + xlab("Discharge (L/s)")
pH_scatter<-ggplot(master, aes(Q, pH)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+geom_smooth(method='lm', se=FALSE)+
  ylab('pH') + xlab("Discharge (L/s)")

#Chimney-reactor#####
chim_react<-chim_react%>%mutate(logQ=log(Q), ratio=CO2reactor_mmol/CO2chimney_mmol,
                                Qid=case_when( Qbase>=Qsurficial~"low",Qbase<=Qsurficial~"high"))%>%
  filter(ratio>0 & ratio <10)

ggplot(chim_react, aes(x=Q)) +
  geom_point(aes(y=CO2chimney_mmol, color='Chimney')) +
  geom_point(aes(y=CO2reactor_mmol, color='Reactor'))+facet_wrap(~ ID, ncol=5)+
  xlab('CO2 mmol/L')+ylab('Discharge (L/s)')+scale_x_log10()

ggplot(chim_react, aes(x=ID, y=ratio, fill=Qid))+
  geom_boxplot()+ylab("umol/L")

chim_vert<-chim_react %>% select(ID, Q, CO2chimney_mmol) %>% rename('mgL'='CO2chimney_mmol')%>% mutate(Pathway="Chimney")
react_vert<-chim_react %>% select(ID, Q, CO2reactor_mmol) %>% rename('mgL'='CO2reactor_mmol')%>% mutate(Pathway="Reactor")
pathway_vert<-rbind(chim_vert, react_vert)

ggplot(pathway_vert, aes(x=ID, y=mgL, fill=Pathway))+
  geom_boxplot()+ylab("umol/L")

#FDOM: Q vs bix, hix####
eem_index<-read_csv("01_Raw_data/Aqualog_processing/projects/Bradford Streams/output/indices_eem_04162024.csv")
eem_index<-eem_index[,-c(1,3,9,11,13,15)]
eem_index<-eem_index %>% mutate(day=day(Date), month=month(Date), year=year(Date))%>%
  rename('ID'='site') %>% filter(bix<0.7, hix<100,ID != 'NA', ID !=14)

eem_index<-left_join(eem_index, discharge,by=c('day','month','year','ID'))
eem_index<-left_join(eem_index, depth,by=c('day','month','year','ID'))
eem_index$ID <- factor(eem_index$ID , levels=c('13','5','5a','6a','15','7','6','9','3'))

bix_box<-ggplot(eem_index, aes(x=ID, y=bix)) +
  geom_boxplot(fill='#A4A4A4', color="black")+theme_sam+
  ylab('index of recent biological origin (bix)')
hix_box<-ggplot(eem_index, aes(x=ID, y=hix)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_sam+ylab('humification index (hix)')

bix_scatter<-ggplot(eem_index, aes(Q_daily, bix)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+
  ylab('index of recent biological origin')+ xlab("Discharge (L/s)")

hix_scatter<-ggplot(eem_index, aes(Q_daily, hix)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+
  ylab('humification index') + xlab("Discharge (L/s)")

boxplots<-plot_grid(bix_box,hix_box, ncol=2)
scatter<-plot_grid(bix_scatter,hix_scatter, ncol=2)
plot_grid(scatter,boxplots, nrow=2)

