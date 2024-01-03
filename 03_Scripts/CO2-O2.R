rm(list=ls())

library(broom)
library(ggplot2)
library(StreamMetabolism)
library(car)
library(imputeTS)
library(zoo)
library(tidyverse)
library(readxl)
library(measurements)
library(weathermetrics)
library(data.table)
site<- master_chem %>% filter(ID=="3")

prelim <- function(site) {
  (site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp))
  site$Mouth_DO_sat<-Cs(site$Mouth_Temp_C)

  x<-c("Date","CO2" ,"DO","Mouth_DO_sat","Mouth_Temp_C")
  site<-site[,x]

  site$mouthTemp_K<-site$Mouth_Temp_C+273.15
  site$exp<-2400*((1/site$mouthTemp_K)-(1/298.15))
  site$KH<-0.034*2.178^(site$exp)#mol/L/atm

  site <- site[complete.cases(site[ , c('DO', 'CO2')]), ]
  site <- site[complete.cases(site[ , c('DO', 'CO2')]), ]
  site <- site[!duplicated(site[c('Date')]),]

  site$CO2_atm<-site$CO2/1000000
  site$CO2_mol<-site$CO2_atm*site$KH
  site$DO_mol<-site$DO/32000

  site$Do_Sat<-Cs(site$Mouth_Temp_C)
  site$DO_Sat_mol<-site$Do_Sat/32000
  (site$CO2_Sat_mol<-(420/1000000)*site$KH)

  site$'O2_mol_L'<-site$KH*(site$DO_mol-site$DO_Sat_mol)
  site$'CO2_mol_L'<-site$KH*(site$CO2_mol-site$CO2_Sat_mol)

  site$'CO2_mmol_L'<-site$'CO2_mol_L'*10^3
  site$'O2_mmol_L'<-site$'O2_mol_L'*10^3

  site$day <- as.Date(site$Date)
  site.day <- aggregate(site, by=list(site$day), FUN='mean')
  x<-c('Date',"CO2_mmol_L","O2_mmol_L","day")
  site<-site[,x]

  return(site)}
dist_point_line <- function(a, slope, intercept) {
  b = c(1, intercept+slope)
  c = c(-intercept/slope,0)
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  return(abs(det(m))/sqrt(sum(v1*v1)))
}
VachonEllipse <- function(site) {
  site$O2_mmol_L <- na_interpolation(site$O2_mmol_L, option='linear')
  site$CO2_mmol_L <- na_interpolation(site$CO2_mmol_L, option='linear')

  days <- unique(site$day) #extract day
  days <- days[-1] #remove first da
  last <- length(days-4) #last possible day for moving average (a week)

  coupling.indicator <- data.frame(matrix(ncol=9, nrow=last-3))
  names(coupling.indicator) <- c('first.day', 'mid.day', 'last.day', 'cen.x', 'cen.y', 'offset', 'stretch', 'width', 'slope')

  for(i in 4:last){
    print(i)

    first.day <- days[i-3] #first day of a week
    week <- seq(first.day, first.day +6, by='day') #matrix of a week

    data.week <- site[site$day %in% week,] #extract data for a week

    ellipse <- data.frame(dataEllipse(data.week$CO2_mmol_L, data.week$O2_mmol_L, levels=c(0.95))) #make an ellipse

    lm <- lm(y~x, data=ellipse) #linear regression
    half<- nrow(ellipse)/2; start.2nd <- 1+half
    e.1st <- ellipse[c(1:half),]; e.2nd <- ellipse[-c(1:half),] #divide an ellipse into 2
    e.diff <- abs(e.1st-e.2nd) #difference of first and second halfs of an ellipse
    e.dis <- sqrt(e.diff$x^2+e.diff$y^2) #calculate the distance between two opposite points in an ellipse

    cen.x <- mean(ellipse$x); cen.y <- mean(ellipse$y) #centroid
    dist <- dist_point_line(c(cen.x, cen.y), -1, 0) # offset
    if(cen.y < -cen.x){dist <- -dist} #if centroid is below -1:1 line, then make distance negative

    c <- i-3
    coupling.indicator$first.day[c] <- first.day #first day of a week
    coupling.indicator$mid.day[c] <- days[i] #middle day of a week
    coupling.indicator$last.day[c] <- first.day+6 #last day of a week

    coupling.indicator$slope[c] <- as.numeric(lm$coefficients[2])
    coupling.indicator$cen.x[c] <- cen.x
    coupling.indicator$cen.y[c] <- cen.y
    coupling.indicator$offset[c] <- dist
    coupling.indicator$stretch[c] <- max(e.dis)
    coupling.indicator$width[c] <- min(e.dis)
  }

  coupling.indicator$mid.day <- as.Date(coupling.indicator$mid.day)
  coupling.indicator$first.day <- as.Date(coupling.indicator$first.day)
  coupling.indicator$last.day <- as.Date(coupling.indicator$last.day)

  first.day<-coupling.indicator[,-c(2,3)]
  last.day<-coupling.indicator[,-c(1,2)]
  mid.day<-coupling.indicator[,-c(1,3)]
  return(first.day)}
compile <- function(ellipse,site_mol) {

  ellipse<- ellipse %>% mutate(Day = day(first.day),Month = month(first.day),year = year(first.day))

  site_mol<- site_mol %>% mutate(Day = day(Date),Month = month(Date),year = year(Date))

  site<-left_join(ellipse,site_mol, by=c('Day', 'Month', 'year'))
  site<-na.omit(site)

  dt=data.table(site)
  regression<-dt[,as.list(coef(lm(CO2_mmol_L~O2_mmol_L))), by = day]
  regression<-filter(regression, O2_mmol_L> -0.001)
  drop.days<-regression$first.day
  site <- site[!(site$first.day %in% drop.days), ]

  x<-c("Date","O2_mmol_L","CO2_mmol_L","slope","offset")
  site<-site[,x]
  return(site)}

master_chem<- read_csv("02_Clean_data/master.csv")

###s3######
s3<- master_chem %>% filter(ID=="3")
s3_mol<-prelim(s3)
s3_ellipse<-VachonEllipse(s3_mol)
s3<-compile(s3_ellipse,s3_mol)
s3$ID<-'s3'

s5<- master_chem %>% filter(ID=="5")
s5_mol<-prelim(s5)
s5_ellipse<-VachonEllipse(s5_mol)
s5<-compile(s5_ellipse,s5_mol)
s5$ID<-'s5'

s5a<- master_chem %>% filter(ID=="5a")
s5a_mol<-prelim(s5a)
s5a_ellipse<-VachonEllipse(s5a_mol)
s5a<-compile(s5a_ellipse,s5a_mol)
s5a$ID<-'s5a'

s6<- master_chem %>% filter(ID=="6")
s6_mol<-prelim(s6)
s6_ellipse<-VachonEllipse(s6_mol)
s6<-compile(s6_ellipse,s6_mol)
s6$ID<-'s6'

s6a<- master_chem %>% filter(ID=="6a")
s6a_mol<-prelim(s6a)
s6a_ellipse<-VachonEllipse(s6a_mol)
s6a<-compile(s6a_ellipse,s6a_mol)
s6a$ID<-'s6a'

s7<- master_chem %>% filter(ID=="7")
s7_mol<-prelim(s7)
s7_ellipse<-VachonEllipse(s7_mol)
s7<-compile(s7_ellipse,s7_mol)
s7$ID<-'s7'

s9<- master_chem %>% filter(ID=="9")
s9_mol<-prelim(s9)
s9_ellipse<-VachonEllipse(s9_mol)
s9<-compile(s9_ellipse,s9_mol)
s9$ID<-'s9'

s13<- master_chem %>% filter(ID=="13")
s13_mol<-prelim(s13)
s13_ellipse<-VachonEllipse(s13_mol)
s13<-compile(s13_ellipse,s13_mol)
s13$ID<-'s13'

s14<- master_chem %>% filter(ID=="14")
s14_mol<-prelim(s14)
s14_ellipse<-VachonEllipse(s14_mol)
s14<-compile(s14_ellipse,s14_mol)
s14$ID<-'s14'

s15<- master_chem %>% filter(ID=="15")
s15_mol<-prelim(s15)
s15_ellipse<-VachonEllipse(s15_mol)
s15<-compile(s15_ellipse,s15_mol)
s15$ID<-'s15'




master<-rbind(s6 ,s6a , s7 , s9 ,s13,s14)
write_csv(master, "04_Output/master_paired.csv")

ggplot(master, aes(CO2_mmol_L, O2_mmol_L)) + geom_point() +
  facet_wrap(~ ID, ncol=2)
