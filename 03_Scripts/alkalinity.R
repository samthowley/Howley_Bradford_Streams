###packages####
library(tidyverse)
library(writexl)
library(readxl)
library(seacarb)
library(weathermetrics)

master<-read_csv('02_Clean_data/master.csv')

HCO3 <- function(site) {
  site <- site[complete.cases(site[ , c('Temp','CO2','pH')]), ]
  site$Temp<- fahrenheit.to.celsius(site$Temp)
  site$Temp_K<- site$Temp+273.15

  site$exp<-2400*((1/site$Temp_K)-(1/298.15))
  site$KH<-0.034*2.178^(site$exp)#mol/L/atm
  site$CO2_atm<-site$CO2/1000000
  site$CO2_molL<-site$CO2_atm*site$KH

  site$Ka<-K1(S=0.01, T=site$Temp, P=site$Water_press)
  site$pKa<- -log10(site$Ka)
  site$HCO3_molL<-(10^(site$pH-site$pKa))*site$CO2_molL

  x<-c("Date",'CO2_molL','HCO3_molL','pH')
  site<-site[,x]
  return(site)}

s3<-filter(master, ID=='3')
s3<-HCO3(s3)
s3$ID<-'3'

s5<-filter(master, ID=='5')
s5<-HCO3(s5) ##check
s5$ID<-'5'

s5a<-filter(master, ID=='5a')
s5a<-HCO3(s5a)
s5a$ID<-'5a'

s6<-filter(master, ID=='6')
s6<-HCO3(s6)
s6$ID<-'6'

s6a<-filter(master, ID=='6a')
s6a<-HCO3(s6a)
s6a$ID<-'6a'
s6a<-filter(s6a, HCO3_molL <0.0001)

s7<-filter(master, ID=='7')
s7<-HCO3(s7)
s7$ID<-'7'

s9<-filter(master, ID=='9')
s9<-HCO3(s9)
s9$ID<-'9'

s13<-filter(master, ID=='13')
s13<-HCO3(s13)
s13$ID<-'13'

s14<-filter(master, ID=='14')
s14<-HCO3(s14)
s14$ID<-'s14'

ggplot(s14, aes(Date)) +
  geom_line(aes(y = CO2_molL*10000)) +
  geom_line(aes(y = HCO3_molL, color="red"))+theme_sam

#s5
master<-rbind(s3,s5a, s6, s6a, s7, s9, s13, s14)
x<-c('Date',"CO2_molL","HCO3_molL",'ID')
master<-master[,x]
write_csv(master, "02_Clean_data/master_alk.csv")

smol<-rbind(s3,s5a, s6, s7, s9)
beag<-rbind(s13, s14,s6a)


ggplot(beag, aes(Date)) +
  geom_line(aes(y = CO2_molL, color='CO2')) +
  geom_line(aes(y = HCO3_molL, color='HCO3*100'))+
  geom_line(aes(y = pH/1000, color='pH/1000'))+
  facet_wrap(~ ID, ncol=3)+
  theme_sam

# master<-filter(master, HCO3_molL <1e-05)
# HCO3_df<-master[,c(3,4)]
# colnames(HCO3_df)[1] <- "conc"
# HCO3_df$t<-'HCO3'
#
# CO2_df<-master[,c(2,4)]
# colnames(CO2_df)[1] <- "conc"
# CO2_df$t<-'CO2'
# tog<-rbind(HCO3_df, CO2_df)
#
# ggplot(tog, aes(x=ID, y=conc, color=t)) +
#   geom_boxplot(outlier.colour="black", outlier.size=1)
#
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 12, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 17, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_text(size = 17),
                             legend.position ="bottom",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
