library(tidyverse)

test<-mol.ellipse.lm%>%select(ID,Date,flux_slope, flux_intercept)%>%rename(daily=Date)

test2<-O2.CO2%>%mutate(daily=as.Date(Date))%>%left_join(test)%>%
  filter(!is.na(flux_slope))%>%
  mutate(
    slope.ID = case_when(
      flux_slope > -0.8 ~ "> -0.8",
      flux_slope <= -1.2 ~ "<= -1.2",
      flux_slope > -1.2 & flux_slope <= -0.8 ~ "Between -1.2 and -0.8"
    ),
    anoxia = case_when(
      DO <= 0.5 ~ "< 0.5 mg/L",
      DO <= 1 ~ "< 1 mg/L",
      DO <= 3 ~ "< 3 mg/L",
      TRUE ~ "> 3 mg/L"
    ))

check<-test2%>%filter(ID=='6', flux_slope>0.3)

ggplot(test2, aes(x=CO2_flux, y=O2_flux)) +
  geom_point(aes(color = ssn))+
  scale_color_grey(start = 0.8, end = 0.2)+
  ggnewscale::new_scale_color()+
  geom_smooth(aes(group = daily, color=slope.ID), method = 'lm', se=F)+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  facet_wrap(~ID, scales='free')+facet_wrap(~ID, scales='free')+
  xlab(expression(CO[2]~g/m^2/day))+ylab(expression(O[2]~g/m^2/day))


ggplot(test2, aes(x=CO2_flux, y=O2_flux)) +
  geom_point(aes(color = anoxia), alpha=0.5)+
  scale_color_manual(values=c("red", "darkred","darkorange", "lightgray"))+
  ggnewscale::new_scale_color()+

  geom_smooth(aes(group=daily), method = 'lm', se = FALSE, color='black', size=0.5) +
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  xlab(expression(CO[2]~g/m^2/day))+
  ylab(expression(O[2]~g/m^2/day))+facet_wrap(~ID, scales='free')



ggplot(O2.CO2, aes(x=CO2_flux, y=O2_flux, color=ssn, group = daily)) +
  geom_point()+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  facet_wrap(~ID, scales='free')+facet_wrap(~ID, scales='free')


ggplot(check%>% filter(Date<'2023-11-02'), aes(x=Date, y=DO)) +
  geom_point()+ylab('DO mg/L')+
  ggplot(check%>% filter(Date<'2023-11-02'), aes(x=Date, y=CO2)) +
  geom_point()+ylab('CO2 ppm')

#checking Q and temp###########


ggplot(data = daily_df%>% filter(ID %in% c('5a', '6', '7')), aes(x = Q, y = CO2.mol.L*10^6)) + geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(
    label = paste(..p.value.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4.5, vjust=15, label.x = 'right')+
  scale_y_log10()+scale_x_log10()+
  ylab(CO2umol.label)+xlab(Q.label)+facet_wrap(~ID, scales='free')



ggplot(data = daily_df%>% filter(ID %in% c('5a', '6', '7')), aes(x = Q, y = O2.mol.L*10^6, group=Q_med, color=Q_med)) +
  scale_color_manual(values=c('blue','darkblue'),
                     labels = c(expression('<'~Q[median]), expression('>'~Q[median])),
                     name=" ")+
  common.layers.Q.trends+legend_size+
  ylab(O2umol.label)+xlab(Q.label)


ggplot(data = O2.CO2.mol%>% filter(ID %in% c('5a', '6', '7')), aes(x = Temp_PT, y=mol.L, color=gas)) +
  geom_point() +
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(x = Temp_PT, y = mol.L,group=gas,
                   label = paste(..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5,vstep=0.052)+
  scale_color_manual(values=c('blue', 'darkorange'),
                     labels = c(expression(CO[2]), expression(O[2])),
                     name=" ")+
  ylab(expression(μmol/L))+xlab("Temperature (F)")+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(x = Temp_PT, y = mol.L,group=gas,
                   label = paste(..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5,vstep=0.052)+
  facet_wrap(~ID, scales='free')+legend_size

plot_grid(

  ggplot(data = daily_df%>% filter(ID %in% c('5a', '6', '7')), aes(x = Q, y = CO2.mol.L*10^6)) + geom_point()+
    stat_poly_line(formula = y ~ x, se = FALSE)+
    stat_poly_eq(aes(
      label = paste(..p.value.label.., sep = "~~~")),
      formula = y ~ x, parse = TRUE,
      label.y = 'bottom', label.x = 'right')+
    scale_y_log10()+scale_x_log10()+
    ylab(CO2umol.label)+xlab(Q.label)+facet_wrap(~ID, scales='free'),


  ggplot(data = daily_df%>% filter(ID %in% c('5a', '6', '7')), aes(x = Q, y = O2.mol.L*10^6, group=Q_med, color=Q_med)) +
    geom_point()+
    scale_color_manual(values=c('blue','darkblue'),
                       labels = c(expression('<'~Q[median]), expression('>'~Q[median])),
                       name=" ")+
    stat_poly_eq(aes(
      label = paste(..p.value.label.., sep = "~~~")),
      formula = y ~ x, parse = TRUE,
      label.y = 'top', label.x = 'left')+
    scale_y_log10()+scale_x_log10()+
    ylab(O2umol.label)+xlab(Q.label)+
    theme(legend.position = 'none')+facet_wrap(~ID, scales='free'),


  ggplot(data = O2.CO2.mol%>% filter(ID %in% c('5a', '6', '7')), aes(x = Temp_PT, y=mol.L, color=gas)) +
    geom_point() +
    stat_poly_line(formula = y ~ x, se = FALSE)+
    stat_poly_eq(aes(x = Temp_PT, y = mol.L,group=gas,
                     label = paste(..p.value.label.., sep = "~~~")),
                 formula = y ~ x, parse = TRUE,
                 size = 4.5,label.y = 'top', label.x = 'left')+
    scale_color_manual(values=c('blue', 'darkorange'),
                       labels = c(expression(CO[2]), expression(O[2])),
                       name=" ")+
    ylab(expression(μmol/L))+xlab("Temperature (F)")+
    facet_wrap(~ID, scales='free'),



  ncol=1
)
