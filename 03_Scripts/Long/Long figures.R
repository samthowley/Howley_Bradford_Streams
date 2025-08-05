
vars_flux <- c('DOC.C.Q.slope','DIC.C.Q.slope','CO2.C.Q.slope', 'CH4.C.Q.slope')
var_flux_labels <- c('DOC Slope (C/Q)', 'DIC Slope (C/Q)', 'CO2 Slope (C/Q)', 'CH4 Slope (C/Q)')

vars_conc <- c('DOC','DIC','CO2_umol_L', 'CH4_umol_L')
var_conc_labels <- c('DOC mg/L', 'DIC mg/L', 'CO2 umol/L', 'CH4 umol/L')

library(ggpmisc)
common_layers<-list(
  geom_point(shape=1, size=2, stroke=2),
  stat_poly_line(formula = y ~ x, se = FALSE),
  labs(y = "mg/L", x = expression('Discharge'~m^3~s^-1)),
  scale_y_log10(),
  scale_y_log10(),
  theme(
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, 'cm')),
  facet_wrap(~Long, scales='free'))

ggplot(c.q %>% filter(ID=='9'), aes(x = Q.prop, y = DOC)) +
  common_layers
ggplot(c.q %>% filter(ID=='5'), aes(x = Q.prop, y = DIC)) +
  common_layers
ggplot(c.q %>% filter(ID=='5'), aes(x = Q.prop, y = CO2_umol_L)) +
  common_layers
ggplot(c.q %>% filter(ID=='5'), aes(x = Q.prop, y = CH4_umol_L)) +
  common_layers

determine_chem<-list(
  geom_hline(yintercept = 0, linewidth=3),
    geom_point(size=4),
  stat_poly_line(formula = y ~ x, se = FALSE),
    stat_poly_eq(
      aes(x = UCA/1000, y = DOC.C.Q.slope,
          label = paste(..p.value.label..,..eq.label.. , sep = "~~~")),
      formula = y ~ x, parse = TRUE,
      size = 4,
      label.x.npc = "right",label.y.npc = 0.017, vstep=0.07),
  scale_color_gradient(low='blue', high='red'),
    theme(
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, 'cm'),
      legend.position = "none"))

library(ggplotly)
(plot_grid(
  ggplot(relationships %>% filter(ID=='5'),
         aes(x = UCA/1000, y = CH4.C.Q.slope, color=hotspots)) +
    determine_chem,

  ggplot(relationships%>% filter(DIC.C.Q.p< 0.005, ID=='5'),
         aes(x = UCA/1000, y = CH4.C.Q.slope, color=hotspots)) +
    determine_chem,

  ggplot(relationships%>% filter(ID=='5'),
         aes(x =UCA, y = CH4.C.Q.slope , color=hotspots)) +
    determine_chem,

  ncol=1
  ))

names(relationships)


ggsave(filename = paste0("05_Figures/Long.H1.jpeg"),
       plot = plot_grid(Long_DOC), width = 31, height = 20, units = "in")

ggsave(filename = paste0("05_Figures/Long.H2.jpeg"),
       plot = plot_grid(Long_DIC, Long_CH4_umol_L, Long_CO2_umol_L, ncol=3), width = 45, height = 15, units = "in")

#boxplots#####

common_layers <- list(
  geom_boxplot(),
    geom_jitter(shape=1, stroke=1,size=3),
  scale_y_continuous(limits = c(-1.5, 1.5)),
    theme(
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, 'cm'),
      legend.position = 'bottom'))

vars_flux <- c('DOC.C.Q.slope','DIC.C.Q.slope','CO2.C.Q.slope', 'CH4.C.Q.slope')
var_flux_labels <- c('DOC Slope (C/Q)', 'DIC Slope (C/Q)', 'CO2 Slope (C/Q)', 'CH4 Slope (C/Q)')

vars_flux <- c('DOC.C.Q.slope','DIC.C.Q.slope','CO2.C.Q.slope', 'CH4.C.Q.slope')
var_flux_labels <- c('DOC Slope (C/Q)', 'DIC Slope (C/Q)', 'CO2 Slope (C/Q)', 'CH4 Slope (C/Q)')

relationships_wetlands<-left_join(relationships, wetland)

make_plot <- function(data1, yvar1, ylab1, show_legend = FALSE) {

  p1 <- ggplot(data1, aes_string(x = "wetland_perc", y = yvar1, fill = "ID")) +
    common_layers+
    xlab("Wetland Cover (%)")+ ylab(ylab1)+theme(legend.position = "none")

  p2 <- ggplot(data1, aes_string(x = "nearest_wetland", y = yvar1, fill = "ID")) +
    common_layers+
    xlab("Nearest Wetland (m)")+ ylab(ylab1)+


  if (!show_legend) p1 <- p1 + theme(legend.position = "none")
  if (!show_legend) p2 <- p2 + theme(legend.position = "none")

  return(list(plot1 = p1, plot2 = p2))
}

for (i in seq_along(vars_flux)) {
  plots <- make_plot(
    relationships_wetlands,
    vars_flux[i],
    var_flux_labels[i],
    show_legend = TRUE)
  combined <- plot_grid(plots$plot1, plots$plot2, ncol = 1)
  assign(paste0("Long_wetland_influence_", vars_conc[i]), combined)}

ggsave(filename = "05_Figures/Long.H3.jpeg",
       plot = plot_grid(Long_wetland_influence_DOC, Long_wetland_influence_DIC,
                        Long_wetland_influence_CH4_umol_L, Long_wetland_influence_CO2_umol_L, ncol=4),
       width = 35, height = 15, units = "in")
