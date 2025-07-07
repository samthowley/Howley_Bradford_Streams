relationships<-relationships %>%   separate(ID, into = c("ID", "Long"), sep = "_")
stream_dims <- read_excel("01_Raw_data/stream dims.xlsx",sheet = "Long site")

relationships<-left_join(relationships, stream_dims, by=c('ID','Long'))%>%
  mutate(ID=ifelse(ID=='3', '6', ID))

sig<-relationships%>%
  mutate(
    DOC.C.Q.slope=if_else(DOC.C.Q.r2 < 0.4, NA, DOC.C.Q.slope),
    DIC.C.Q.slope=if_else(DIC.C.Q.r2 < 0.4, NA, DIC.C.Q.slope)
  )

c.q<-c.q %>% mutate(Long=as.factor(Long), ID=as.factor(ID))


vars_flux <- c('DOC.C.Q.slope','DIC.C.Q.slope','CO2.C.Q.slope', 'CH4.C.Q.slope')
var_flux_labels <- c('DOC Slope (C/Q)', 'DIC Slope (C/Q)', 'CO2 Slope (C/Q)', 'CH4 Slope (C/Q)')

vars_conc <- c('DOC','DIC','CO2_umol_L', 'CH4_umol_L')
var_conc_labels <- c('DOC mg/L', 'DIC mg/L', 'CO2 umol/L', 'CH4 umol/L')

make_plot <- function(data1, data2, yvar1, yvar2, ylab1, ylab2, show_legend = FALSE) {
  # Use column names as strings for aes_string
  p1 <- ggplot(data1, aes_string(x = "Q.prop", y = yvar1, color = "Long", group="Long")) +
    geom_point(shape=1, size=10, stroke=2)+
    geom_smooth(method=lm, se=F, alpha=0.5, linetype='dashed', linewidth=2)+
    labs(y = ylab1, x = expression('Discharge'~m^3~s^-1)) +
    scale_y_log10()+scale_y_log10()+
    theme(
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, 'cm'))+
    facet_wrap(~ID, scales='free')

  p2 <- ggplot(data2, aes_string(x = "UCA_km/1000", y = yvar2, color = "Long")) +
    geom_hline(yintercept = 0, linewidth=3)+
    geom_point(size=10)+
    labs(y = ylab2, x = "UCA (km/1000)") +
    theme(
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, 'cm'))+
    scale_y_continuous(limits = c(-1.5, 1.5))+
    facet_wrap(~ID)

  if (!show_legend) p1 <- p1 + theme(legend.position = "none")
  if (!show_legend) p2 <- p2 + theme(legend.position = "none")

  return(list(plot1 = p1, plot2 = p2))
}


for (i in seq_along(vars_flux)) {
  plots <- make_plot(
    c.q,
    relationships,
    vars_conc[i], vars_flux[i],
    var_conc_labels[i], var_flux_labels[i],
    show_legend = TRUE)
  combined <- plot_grid(plots$plot1, plots$plot2, ncol = 1)
  assign(paste0("Long_", vars_conc[i]), combined)}

ggsave(filename = paste0("05_Figures/Long.H1.jpeg"),
       plot = plot_grid(Long_DOC), width = 31, height = 20, units = "in")

ggsave(filename = paste0("test.jpeg"),
       plot = plot_grid(Long_DIC, Long_CH4_umol_L, Long_CO2_umol_L, ncol=3), width = 45, height = 15, units = "in")

#boxplots#####
names(relationships)
ggplot(
  relationships,
  aes(x = Distance_m, y = DIC, fill = ID)) +
  geom_boxplot(width=1) + geom_jitter(shape=1, size=3)+
  theme(legend.position = 'bottom')
