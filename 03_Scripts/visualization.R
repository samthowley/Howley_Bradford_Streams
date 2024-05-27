library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)

theme_sam<-theme()+   theme(axis.text.x = element_text(size = 12, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 14, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_blank(),
                             legend.position ="bottom",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

discharge <- read_csv("02_Clean_data/discharge.csv")
discharge<-discharge %>% mutate(day=day(Date), month=month(Date), year=year(Date))
discharge<-discharge %>% group_by(ID,day, month,year) %>%
  mutate(Q_daily=mean(Q, na.rm=T), loqQdaily=log(Q_daily)) %>% filter(Q_daily>2, ID !=14)

depth <- read_csv("02_Clean_data/depth.csv")
#discharge trends####
discharge<-filter(discharge, Qbase>0 & Q<3000)
(Q_timeseries<-ggplot(discharge, aes(Date))+
   geom_line(aes(y=Q, color= "Total Discharge")) +
   geom_line(aes(y=Qbase, color='Base Discharge')) +
  facet_wrap(~ ID, ncol=3)+theme_sam+
  ylab('Discharge (L/S)') + xlab("Date"))+
  scale_color_manual(values = c('blue','darkred'))

(depth_timeseries<-ggplot(depth, aes(Date))+
    geom_line(aes(y=depth)) +
    facet_wrap(~ ID, ncol=5)+theme_sam+
    ylab('Depth (m)') + xlab("Date"))

depth<-depth %>% mutate(day=day(Date), month=month(Date), year=year(Date))
depth<-depth %>% group_by(ID,day, month,year) %>% mutate(depth_daily=mean(depth, na.rm=T))
depth<-depth[,c(2,5,6,7,8)]

#TC####
TC <- read_csv("02_Clean_data/TC.csv")
DOC<-TC %>% filter(ID != 14, Species=='DOC')

DOC$ID <- factor(DOC$ID , levels=c('13','5','6a','15','5a','7','6','9','3'))

DOC<-DOC%>%mutate(logQ_daily=log(Q_daily)) %>% filter(Q_daily>2, Conc.>2)

DOC_box<-ggplot(DOC, aes(x=ID, y=Conc., fill=Q_ID)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_sam+ylab('DOC mg/L')+theme(axis.title.x = element_blank())

(DOC_scatter<-ggplot(DOC, aes(logQ_daily, Conc.)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+theme_sam+geom_smooth(method='lm', se=FALSE)+
  ylab('DOC mg/L')+ theme(axis.title.x = element_blank()))

plot_grid(DOC_box,DOC_scatter, nrow=2)

#ER####
metabolism <- read_csv("04_Output/master_metabolism.csv")
metabolism<-metabolism %>% mutate(day=day(Date), month=month(Date), year=year(Date))

metabolism<-left_join(metabolism, discharge,by=c('day','month','year','ID'))
metabolism<-left_join(metabolism, depth,by=c('day','month','year','ID'))
metabolism<-metabolism %>% filter(Q_daily>10, ID !=14)

metabolism$ID <- factor(metabolism$ID , levels=c('13','5','6a','15','5a','7','6','9','3'))

ER_box<-ggplot(metabolism, aes(x=ID, y=ER)) +
  geom_boxplot(fill='#A4A4A4', color="black")+theme_sam+
  ylab('Ecosystem Respiration')
GPP_box<-ggplot(metabolism, aes(x=ID, y=GPP)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_sam+ylab('Gross Primary Productivity')

ER_scatter<-ggplot(metabolism, aes(Q_daily, ER)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+geom_smooth(method='lm', se=FALSE)+
  ylab('ER')+ xlab("Discharge (L/s)")+theme(axis.title.x = element_blank())
GPP_scatter<-ggplot(metabolism, aes(Q_daily, ER)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+geom_smooth(method='lm', se=FALSE)+
  ylab('GPP') + xlab("Discharge (L/s)")+theme(axis.title.x = element_blank())

scatter<-plot_grid(ER_scatter,GPP_scatter, ncol=2)
boxplots<-plot_grid(ER_box,GPP_box, ncol=2)
plot_grid(scatter,boxplots, nrow=2)

#Chem####
master <- read_csv("master.csv")
master<- master %>% mutate(day=day(Date), month=month(Date), year=year(Date))
master<-master%>%filter(Q>2, ID != 14)

master$ID <- factor(master$ID , levels=c('13','5','6a','15','5a','7','6','9','3'))

DO_box<-ggplot(master, aes(x=ID, y=DO)) +
  geom_boxplot(color="black")+theme_sam+
  ylab('Dissolved Oxygen')+theme(axis.title.x = element_blank())
CO2_box<-ggplot(master, aes(x=ID, y=CO2)) +
  geom_boxplot(color="black")+theme_sam+
  ylab('CO2 (ppm)')+theme(axis.title.x = element_blank())
pH_box<-ggplot(master, aes(x=ID, y=pH))+
  geom_boxplot(color="black")+theme_sam+
  ylab('pH')+theme(axis.title.x = element_blank())

DO_scatter<-ggplot(master, aes(Q, DO)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+geom_smooth(method='lm', se=FALSE)+
  ylab('DO') + xlab("Discharge (L/s)")
CO2_scatter<-ggplot(master, aes(Q, CO2)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+geom_smooth(method='lm', se=FALSE)+
  ylab('CO2') + xlab("Discharge (L/s)")
pH_scatter<-ggplot(master, aes(Q, pH)) + geom_point() +
  facet_wrap(~ ID, ncol=5)+theme_sam+geom_smooth(method='lm', se=FALSE)+
  ylab('pH') + xlab("Discharge (L/s)")

#Species of C####
HCO3 <- read_csv("02_Clean_data/alkalinity.csv")
alk<- HCO3 %>% mutate(C_molL= CO2_molL+HCO3_molL,day=day(Date), month=month(Date), year=year(Date),
                      Species='DIC')
x<-c('ID','C_molL', 'day','month','year','Species')
alk<-alk[,x]

TC <- read_csv("02_Clean_data/TC.csv")
TC<-TC %>%mutate(C_molL=Conc./12000,day=day(Date), month=month(Date), year=year(Date))
DOC<- TC %>% filter(Species=='DOC')
DOC<-DOC[,x]
DIC<- TC %>% filter(Species=='DIC')
DIC<-DIC[,x]

carbon_mol<-rbind(DIC, alk,DOC)
#carbon_mol<-left_join(DOC,DIC, by=c('day','month','year','ID'))
#carbon_mol<-carbon_mol %>%mutate(DIC_DOC= DIC_molL/DOC_molL)

Q<-discharge[,c("Q_ID","day","month","year","Q_daily", 'ID')]
carbon_mol<-left_join(carbon_mol,Q, by=c('day','month','year','ID'))
carbon_mol <- carbon_mol[!duplicated(carbon_mol[c('ID','day','month','year','Species')]),]


(C_box<-ggplot(carbon_mol, aes(x=ID, y=C_molL, fill=Species))+
  geom_boxplot(color="black")+theme_sam+theme(legend.position ="bottom")+
  ylab('mmol/L'))

#eem####
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

#trends with discharge####
names(HCO3)
DOQ_box<-ggplot(master, aes(x=ID, y=DO, fill= Q_ID)) +
    geom_boxplot(color="black")+theme_sam+
    ylab('Dissolved Oxygen (mg/L)')+scale_fill_manual(values = c('lightgreen','blue'))+
  theme(axis.title.x = element_blank())

pHQ_box<-ggplot(master, aes(x=ID, y=pH, fill= Q_ID)) +
  geom_boxplot(color="black")+theme_sam+
  ylab('pH')+scale_fill_manual(values = c('lightgreen','blue'), name="Discharge")+
  theme(axis.title.x = element_blank(),
        legend.position ="bottom")

CO2Q_box<-ggplot(master, aes(x=ID, y=CO2, fill= Q_ID)) +
  geom_boxplot(color="black")+theme_sam+
  ylab('CO2 (mg/L)')+scale_fill_manual(values = c('lightgreen','blue'))+
  theme(axis.title.x = element_blank())


HCO3<-HCO3 %>% mutate(day=day(Date), month=month(Date), year=year(Date), total=CO2_molL+HCO3_molL)
HCO3<-left_join(HCO3, Q, by=c('day','month','year','ID'))
ggplot(HCO3, aes(x=ID, y=total, fill= Q_ID)) +
  geom_boxplot(color="black")+theme_sam+
  ylab('CO2 mg/L')+scale_fill_manual(values = c('lightgreen','blue'))+
  theme(axis.title.x = element_blank())



#plot grids#####
(chemsig<-plot_grid(CO2_box, DO_box,pH_box,DOC_box, nrow=4, align = 'v'))
(emerging_trends<-plot_grid(DOQ_box, CO2Q_box,pHQ_box, nrow=3, align = 'v'))

(C_box<-ggplot(carbon_mol, aes(x=ID, y=C_molL*1000, fill=Species))+
    geom_boxplot(color="black")+theme_sam+theme(legend.position ="bottom")+
    ylab('mmol/L'))+scale_color_manual(values=c('black','gray'))

dev.new()

