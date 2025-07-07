#Conc~Q###########

DIC<-c.q %>%select(ID, Long, ID_Long, DIC, Q.prop, Q, distance)%>% rename(Conc=DIC)%>%
  mutate(C_species='DIC')
DOC<-c.q %>%select(ID, Long, ID_Long, DOC, Q.prop, Q, distance)%>% rename(Conc=DOC)%>%
  mutate(C_species='DOC')
POC<-c.q %>%select(ID, Long, ID_Long, POC, Q.prop, Q, distance)%>% rename(Conc=POC)%>%
  mutate(C_species='POC')
CO2<-c.q %>%select(ID, Long, ID_Long, CO2_sat, Q.prop, Q, distance)%>% rename(Conc=CO2_sat)%>%
  mutate(C_species='CO2')
CH4<-c.q %>%select(ID, Long, ID_Long, CH4_sat, Q.prop, Q, distance)%>% rename(Conc=CH4_sat)%>%
  mutate(C_species='CH4_saturation')
CO2_conc<-c.q %>%select(ID, Long, ID_Long, CO2_umol_L, Q.prop, Q, distance)%>% rename(Conc=CO2_umol_L)%>%
  mutate(C_species='CH4_saturation')
CH4_conc<-c.q %>%select(ID, Long, ID_Long, CH4_umol_L, Q.prop, Q, distance)%>% rename(Conc=CH4_umol_L)%>%
  mutate(C_species='CH4')

c.q.long_df<-rbind(DIC, DOC, POC, CO2, CH4, CO2_conc, CH4_conc)

common_layers <- list(
  geom_point(size=3),
  geom_smooth(method=lm, se=F, alpha=0.5),
  scale_x_log10(),scale_y_log10(),
  xlab('Discharge L/s'),
  theme(legend.position = "bottom"),
  scale_color_gradient(high='orange', low = 'blue'),
  facet_wrap(~ ID, scales='free'))

a<-ggplot(c.q.long_df%>% filter(C_species %in% c('DIC')), aes(x=Q.prop, y=Conc, color=Long, group = ID_Long))+
  common_layers+ylab('DIC mg/L')

b<-ggplot(c.q.long_df%>% filter(C_species %in% c('CH4')), aes(x=Q.prop, y=Conc, color=Long, group = ID_Long))+
  common_layers+ylab(expression(CH[4]~umol~L^-1))

c<-ggplot(c.q.long_df%>% filter(C_species %in% c('CO2')), aes(x=Q.prop, y=Conc, color=Long, group = ID_Long))+
  common_layers+ylab(expression(CO[2]~umol~L^-1))

H2<-plot_grid(a,b,c, ncol=1)

ggsave(filename = paste0("05_Figures/H2.Ch3.jpeg"),
       plot = H2, width = 31, height = 20, units = "in")
#flux across distance ############

c.q_distance<-c.q.long_df %>%
  mutate(
    c.q= (Conc/Q.prop))

ggplot(c.q_distance%>% filter(C_species %in% c('DOC', 'DIC')), aes(x=distance/1000, y=c.q, color=C_species))+
  geom_point(size=2, shape=1) +
  geom_smooth(method=lm, se=F, alpha=0.5)+
  xlab('Distance (km)')+ylab("Flux (g/s)")+
  scale_y_log10()+
  theme(legend.position = "bottom")+
  facet_wrap(~ ID, scales='free', ncol=1)

ggplot(c.q_distance%>% filter(C_species %in% c('CH4', 'CO2')), aes(x=distance/1000, y=c.q, color=C_species))+
  geom_point(size=2, shape=1) +
  geom_smooth(method=lm, se=F, alpha=0.5)+
  xlab('Distance (km)')+ylab("Flux (umol/s)")+
  #scale_y_log10()+
  theme(legend.position = "bottom")+
  facet_wrap(~ ID, scales='free', ncol=1)
unique(c.q.long_df$C_species)
#c.q across UCA ############


relationships<-relationships %>%   separate(ID, into = c("ID", "Long"), sep = "_")
stream_dims <- read_excel("01_Raw_data/stream dims.xlsx",sheet = "Long site")

relationships<-left_join(relationships, stream_dims, by=c('ID','Long'))%>%
  mutate(ID=ifelse(ID=='3', '6', ID))

sig<-relationships%>%
  mutate(
    DOC.C.Q.slope=if_else(DOC.C.Q.r2 < 0.4, NA, DOC.C.Q.slope),
    DIC.C.Q.slope=if_else(DIC.C.Q.r2 < 0.4, NA, DIC.C.Q.slope)
  )

r2title<-expression(r^2 > 0.4)

common_layers <- list(
  geom_point(size=10),
    geom_smooth(method=lm, se=F, alpha=0.5, linetype='dashed', linewidth=2, color='black'),
    xlab('Upper Contributing Area (km)'),ylab(expression('Slope (C/Q)')),
    geom_hline(yintercept = 0, linewidth=3),
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16),  # Legend title font size
        legend.text  = element_text(size = 14),  # Legend item font size
        legend.key.size = unit(1.5, 'cm')))



make_plot <- function(data, yvar, ylab, title, show_legend = FALSE) {
  p <- ggplot(data, aes_string(x = "UCA_km/1000", y = yvar, color = "ID")) +
    common_layers +
    labs(y = ylab, title = title) +
    theme(
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.key.size = unit(1.5, 'cm')
    )
  if (!show_legend) p <- p + theme(legend.position = "none")
  return(p)
}

vars <- c('DOC.C.Q.slope','DIC.C.Q.slope','CO2.C.Q.slope', 'CH4.C.Q.slope')
var_labels <- c('DOC Slope (C/Q)', 'DIC Slope (C/Q)', 'CO2 Slope (C/Q)', 'CH4 Slope (C/Q)')

for (i in seq_along(vars)) {
  p1 <- make_plot(sig, vars[i], var_labels[i], paste0( r2title), show_legend = FALSE)
  p2 <- make_plot(relationships, vars[i], var_labels[i], paste0(" (All)"), show_legend = TRUE)
  combined <- plot_grid(p2, p1, ncol = 1)
  ggsave(filename = paste0("05_Figures/Long_", vars[i], "_CQ.UCA.jpeg"),
         plot = combined, width = 7, height = 12, units = "in")
}
