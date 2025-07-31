#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(cowplot)
library(lme4)
library(ggpmisc)

theme_set(theme(axis.text.x = element_text(size = 32),
                axis.text.y = element_text(size = 32),
                axis.title.y = element_text(size = 35, angle = 90),
                axis.title.x = element_text(size = 35),
                plot.title = element_text(size = 35),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                strip.text = element_text(size = 32)))

RC_df <- read_csv("04_Output/allRC_C.csv")%>%
  mutate(Well=as.factor(Well))

##Boxplots: distance#########

distance_theme <- list(
  geom_boxplot(width = 1),
  geom_jitter(shape = 1, size = 1, color='blue'),
  xlab("Distance (m)"),
  scale_fill_brewer(palette = "Set0"),
  facet_wrap(~ID, ncol = 3, scales='free'),
  labs(fill = "Wells"),
  theme(
    legend.position = 'none',
    strip.text = element_text(size = 15),
    axis.title.y = element_text(size=20)))

(a<-ggplot(
  RC_df, aes(x = Distance_m, y = DIC, group = Well, fill=Well)) +
  ylab("DIC mg/L") + distance_theme)

(b<-ggplot(
  RC_df, aes(x = Distance_m, y = DOC, group = Well, fill=Well)) +
    ylab("DOC mg/L") + distance_theme)

(c<-ggplot(
  RC_df, aes(x = Distance_m, y = CH4_sat, group = Well, fill=Well)) +
    ylab(expression(CH[4]~"Saturation")) + distance_theme)

(d<-ggplot(
  RC_df, aes(x = Distance_m, y = DOC, group = Well, fill=Well)) +
    ylab(CO[2]~"Saturation") + distance_theme)


solids<-plot_grid(a,b, ncol=1)
gas<-plot_grid(c,d, ncol=1)
ggsave(filename="05_Figures/RC_elevation_conc.jpeg",
       plot = plot_grid(solids, gas, ncol=2),
       width =30,
       height = 10,
       units = "in")

#Hypothesis 1: RC carbon is the primary source of stream carbon ##############

(a<-ggplot(
  RC_df, aes(x = Distance_m, y = DOC_flux, group = Well, fill=Well)) +
   ylab(expression('DOC'~('mg'~m^-2~day^-1)))+
          scale_y_log10()+distance_theme)


(b<-ggplot(
  RC_df, aes(x = Distance_m, y = DIC_flux, group = Well, fill=Well)) +
    ylab(expression('DIC'~('mg'~m^-2~day^-1)))+
    scale_y_log10()+distance_theme)


(c<-ggplot(
  RC_df, aes(x = Distance_m, y = lateral_CO2, group = Well, fill=Well)) +
    ylab(expression('CO2'~('g'~m^-2~day^-1)))+
    scale_y_log10()+distance_theme)


(d<-ggplot(
  RC_df, aes(x = Distance_m, y = lateral_CH4, group = Well, fill=Well)) +
    ylab(expression('CH4'~('g'~m^-2~day^-1)))+
    scale_y_log10()+distance_theme)

ggsave(filename="05_Figures/RC_elevation_flux.jpeg",
       plot = plot_grid(a,b,c,d, ncol=1),
       width =16,
       height = 16,
       units = "in")

#Hypothesis 2: RC fluxes will be greatest during periods of increased watershed inundation###########
flux.WT_theme <- list(
  geom_point(size=3, shape=1, stroke=1.5),
  geom_smooth(se=F, method=lm),
  xlab("Water Table elevation (m)"),
  facet_wrap(~ID, scales = 'free'),
  labs(fill = "Wells"),
  theme(legend.position = 'none',
  strip.text = element_text(size = 15),
  axis.title.y = element_text(size=20)))


(a<-ggplot(RC_df %>%filter(well_types!="stream"),
  aes(x = WT_elevations, y = DOC_flux, group = Well, color=as.factor(Well)))+
  flux.WT_theme+theme(axis.title.x = element_blank())+
  ylab(expression('DOC'~('mg'~m^-2~day^-1))))

(b<-ggplot(RC_df%>%filter(well_types!="stream"),
  aes(x = WT_elevations, y = DIC_flux, group = Well, color=as.factor(Well)))+
  flux.WT_theme+
    ylab(expression('DIC'~('mg'~m^-2~day^-1))))

(c<-ggplot(RC_df%>%filter(well_types!="stream"),
           aes(x = WT_elevations, y = lateral_CH4, group = Well, color=as.factor(Well)))+
    flux.WT_theme+
    ylab(expression(CH[4]~('mg'~m^-2~day^-1))))

(d<-ggplot(RC_df%>%filter(well_types!="stream"),
           aes(x = WT_elevations, y = lateral_CO2, group = Well, color=as.factor(Well)))+
    flux.WT_theme+
    ylab(expression(CO[2]~('mg'~m^-2~day^-1))))

c<-plot_grid(d,c, nrow=2)
d<-plot_grid(a,b, nrow=2)

ggsave(filename="05_Figures/RC_WT~DIC&DOC_all_wells_by_site.jpeg",
       plot = plot_grid(d,c,ncol=2),
       width =36,
       height = 10,
       units = "in")


#Hypothesis: 3 greater wetland coverage will exhibit higher RC carbon potential due ##########
RC_slopes <- read_csv("04_Output/RC_slopes.csv")%>%
  mutate(ID=as.factor(ID), Well=as.factor(Well))

sig<-RC_slopes%>%
  mutate(
    DOC.elevation.slope=if_else(DOC.elevation.r2 < 0.4, NA, DOC.elevation.slope),
    DIC.elevation.slope=if_else(DIC.elevation.r2 < 0.4, NA, DIC.elevation.slope))


rgression_dims<-RC_dims %>% separate(Site, into = c("ID", "Well"), sep = "GW")
sig_dim<-left_join(sig, rgression_dims, by=c("ID", "Well"))%>%
  mutate(Well=as.character(Well))%>%
  mutate(Well=if_else(Well=='0','Stream', Well),
         Well=if_else(Well=='8' & ID=='5', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='9', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='6', 'Upland', Well))


regression_edited<-left_join(RC_slopes, rgression_dims, by=c("ID", "Well"))%>%
  mutate(Well=as.character(Well))%>%
  mutate(Well=if_else(Well=='0','Stream', Well),
         Well=if_else(Well=='8' & ID=='5', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='9', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='6', 'Upland', Well))%>%
  distinct(ID, Well, .keep_all = T)

ytitle_DOC<-expression('DOC flux/ Water Table Elevation'~mg~m^-3~s^-1)
ytitle_DIC<-expression('DIC flux/ Water Table Elevation'~mg~m^-3~s^-1)

ytitle_DOC<-expression('DOC'~('mg'~m^-2~s^-1))
ytitle_DIC<-expression('DIC'~('mg'~m^-2~s^-1))
r2title<-expression(r^2 > 0.4)
xtitle<-'Distance (m)'

common_layers <- list(
  geom_point(size = 5),
  geom_hline(yintercept = 0, linetype = 'dashed'),
  xlab(xtitle),
  facet_wrap(~ID, scales = 'free'),
  labs(color = "Wells"),
  theme(axis.title.y = element_text(size = 20, angle = 90),
        strip.text = element_text(size = 15)),
  stat_poly_line(se = FALSE, color = 'black'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE, color = 'black',
    label.x.npc = "left", label.y.npc = "top", size = 8))


make_plot <- function(data, title, show_legend = FALSE) {
  p <- ggplot(data, aes(x = Distance_m, y = DOC.elevation.slope)) +
    common_layers +
    ggtitle(title)+ylab(ytitle_DOC)

  p2 <- ggplot(data, aes(x = Distance_m, y = DIC.elevation.slope)) +
    common_layers +
    ggtitle(title)+ylab(ytitle_DIC)

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
  }

  return(list(DOC_plot = p, DIC_plot = p2))
}


ids <- c('5', '6', '9')

for (id in ids) {
  plots_sig <- make_plot(sig_dim %>% filter(ID == id, Well != 'Stream'), r2title, show_legend = FALSE)
  plots_rel <- make_plot(regression_edited %>% filter(ID == id, Well != 'Stream'), " ", show_legend = TRUE)

  combined_DOC <- plot_grid(plots_rel$DOC_plot, ncol = 1)
  combined_DIC <- plot_grid(plots_rel$DIC_plot, ncol = 1)

  combined<-plot_grid(combined_DOC, combined_DIC, ncol=1)
  assign(paste0('RC_', id), combined)
}

combined<-plot_grid(print(RC_5), print(RC_6), print(RC_9), ncol=3)
ggsave(filename = paste0("05_Figures/RC_WT.regress.jpeg"),
       plot = combined, width = 20, height = 10, units = "in")


regression_edited$wetland_perc <- factor(regression_edited$wetland_perc,
                                levels = sort(unique(regression_edited$wetland_perc), decreasing = TRUE))

labels_vec <- setNames(
  paste0(regression_edited$ID, "\n", regression_edited$wetland_perc),
  regression_edited$ID)

DOC_slope<-regression_edited%>%
  select(ID, wetland_perc, DOC.elevation.slope)%>%
  rename(slope=DOC.elevation.slope)%>%mutate(type='DOC')

DIC_slope<-regression_edited%>%
  select(ID, wetland_perc, DIC.elevation.slope)%>%
  rename(slope=DIC.elevation.slope)%>%mutate(type='DIC')
slopes<-rbind(DOC_slope, DIC_slope)


ggplot(slopes%>%filter(type=='DIC'),
       aes(x = reorder(ID, -as.numeric(wetland_perc)),  y = slope, fill=type)) +
  geom_boxplot(position = position_dodge(width = 0.75))+
  geom_jitter()+
  ggtitle("Influence of Landscape Hydrology on Lateral Carbon Export") +
  ylab(expression('Flux/ Water Table Elevation'~~mg~m^-3~s^-1)) +
  xlab("Wetland Cover %") +
  scale_x_discrete(labels = labels_vec)+
  theme(
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    plot.title =element_text(size=14))

