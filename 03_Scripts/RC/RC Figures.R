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

#Call in data############
file_path <- "04_Output/RC_by_well.xlsx"
sheet_names <- excel_sheets(file_path)
RC <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
}) %>%
  bind_rows()


streamC<-read_csv('04_Output/stream_sampledC.csv')%>%
  select(Date, ID, DIC, DOC, POC, CO2,CO2_umol_L, CH4_umol_L, CO2_sat,CH4_sat)

streamC<-left_join(streamC, qL)
RC_columns<-names(RC_df)

streamC_edited<-streamC%>%
  filter(ID %in% c("5","6","9"))%>%
  mutate(
    CO2_molL=CO2_umol_L/10^6,
    CH4_molL=CH4_umol_L/10^6)%>%
  mutate(
    lateral_CO2=CO2_molL*(10^3)*12*86400*(Q/A),
    lateral_CH4=CH4_molL*(10^3)*12*86400*(Q/A))%>%
  mutate(DOC_mg.m3=DOC/10^3,
         DIC_mg.m3=DIC/10^3,
         DOC_flux=DOC_mg.m3*((Q/10^3)/A)*86400,
         DIC_flux=DIC_mg.m3*((Q/10^3)/A)*86400)%>%
  mutate(
    `Distance (ft)`= -0.5,
    `Distance_m`= -0.5,
    WTdepth_m=0,
    Well=0,
    DistanceID='stream',
    surface2WT=0,
    surface_elevation_m=0,
    WT.ID=1,
    WT_elevations=depth,
    well_types='stream',
    ID.Well = paste(ID, Well, sep = ".")
  )%>%filter(!is.na(DOC))%>%
  select(all_of(RC_columns))

RC_df<-rbind(streamC_edited,RC)%>%
  separate(ID.Well, into = c("ID", "Well"), sep = "\\.", extra = "merge")

#Lateral flow regime#######
ggplot(
  RC_df, aes(x = WT_elevations, y = qL, group=Well, color=Well)) +
  geom_point()+ geom_smooth(method = 'lm', se=F)+
  facet_wrap(~ID+Well, scales='free')

ggplot(
  RC_df, aes(x = Distance_m, y = WT_elevations, group=Well, color=well_types)) +
  geom_boxplot(width = 1)+ geom_smooth(method = 'lm', se=F)+
  facet_wrap(~ID, scales='free')

RC_df %>% group_by(ID, Well)%>%
  summarize(WTE_mean=mean(WT_elevations, na.rm=T))%>%ungroup()%>%
  group_by(ID)%>%
  summarise(
    min=min(WTE_mean, na.rm=T),
    max=max(WTE_mean, na.rm=T),
    )


#Hypothesis 1: RC carbon is the primary source of stream carbon ##############
library(plotly)
distance_theme <- list(
  geom_boxplot(width = 1),
  xlab("Distance (m)"),
  facet_wrap(~ID, ncol = 3, scales='free'),
  scale_fill_manual(values=c("darkgreen", 'blue', 'red')),
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", na.rm = T),
  labs(fill = "Wells"),
  theme(
    legend.position = 'bottom',
    strip.text = element_text(size = 20),
    axis.title.y = element_text(size=20)))

(a<-ggplot(
  RC_df, aes(x = Distance_m, y = DIC, group = Well, fill=well_types)) +
  ylab("DIC mg/L") + distance_theme)

(b<-ggplot(
  RC_df, aes(x = Distance_m, y = DOC, group = Well, fill=well_types)) +
    ylab("DOC mg/L") + distance_theme)

(c<-ggplot(
  RC_df , aes(x = Distance_m, y = CH4_sat, group = Well, fill=well_types)) +
    ylab(expression(CH[4]~"Saturation")) + distance_theme)

(c<-ggplot(
  RC_df, aes(x = Distance_m, y = CO2_sat, group = Well, fill=well_types)) +
    ylab(expression(CO[2]~"Saturation")) + distance_theme)


solids<-plot_grid(a,b, ncol=1)
gas<-plot_grid(c,d, ncol=1)
ggsave(filename="05_Figures/RC_elevation_conc.jpeg",
       plot = plot_grid(solids, gas, ncol=2),
       width =30,
       height = 10,
       units = "in")


quantile((RC_df %>% filter(ID == '9'))$lateral_CO2, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

(a<-ggplot(
  RC_df, aes(x = Distance_m, y = lateral_CO2, group = Well, fill=well_types)) +
   ylab(expression('DOC'~('mg'~m^-2~day^-1)))+
          scale_y_log10()+distance_theme)

(b<-ggplot(
  RC_df, aes(x = Distance_m, y = DIC_flux, group = Well, fill=well_types)) +
    ylab(expression('DIC'~('mg'~m^-2~day^-1)))+
    scale_y_log10()+distance_theme)

(c<-ggplot(
  RC_df, aes(x = Distance_m, y = lateral_CH4, group = Well, fill=well_types)) +
    ylab(expression('CO2'~('g'~m^-2~day^-1)))+
    scale_y_log10()+distance_theme)


(d<-ggplot(
  RC_df, aes(x = Distance_m, y = lateral_CH4, group = Well, fill=well_types)) +
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
  labs(color = "Wells"),
  theme(legend.position = 'bottom',
  strip.text = element_text(size = 20),
  axis.title.y = element_text(size=25)))


(a<-ggplot(RC_df %>%filter(well_types!="stream"),
  aes(x = WT_elevations, y = DOC_flux, group = Well, color=Well))+
  flux.WT_theme+
    theme(axis.title.x = element_blank(),
          legend.position = "none")+
  ylab(expression('DOC'~('mg'~m^-2~day^-1))))

(b<-ggplot(RC_df%>%filter(well_types!="stream"),
  aes(x = WT_elevations, y = DIC_flux, group = Well, color=as.factor(Well)))+
  flux.WT_theme+
    ylab(expression('DIC'~('mg'~m^-2~day^-1)))+
    theme(legend.position = 'bottom'))

(c<-ggplot(RC_df%>%filter(well_types!="stream"),
           aes(x = WT_elevations, y = lateral_CH4, group = Well, color=as.factor(Well)))+
    flux.WT_theme+
    ylab(expression(CH[4]~('mg'~m^-2~day^-1))))

(d<-ggplot(RC_df%>%filter(well_types!="stream"),
           aes(x = WT_elevations, y = lateral_CO2, group = Well, color=as.factor(Well)))+
    flux.WT_theme+
    theme(axis.title.x = element_blank(),
          legend.position = "none")+
    ylab(expression(CO[2]~('mg'~m^-2~day^-1))))

gas<-plot_grid(d,c, nrow=2)
dissolved<-plot_grid(a,b, nrow=2)

ggsave(filename="05_Figures/RC_WT~gas_by_wells.jpeg",
       plot = gas,width =20,height = 12,units = "in")

ggsave(filename="05_Figures/RC_WT~dissolved_by_wells.jpeg",
       plot = dissolved, width =20, height = 10, units = "in")

RC_slopes <- read_csv("04_Output/RC_slopes.csv")%>%
  mutate(ID=as.factor(ID), Well=as.factor(Well))

rgression_dims<-RC_dims %>% separate(Site, into = c("ID", "Well"), sep = "GW")

regression_edited<-left_join(RC_slopes, rgression_dims, by=c("ID", "Well"))%>%
  mutate(Well=as.character(Well))%>%
  mutate(Well=if_else(Well=='0','Stream', Well),
         Well=if_else(Well=='8' & ID=='5', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='9', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='6', 'Upland', Well))%>%
  distinct(ID, Well,type, .keep_all = T)

ytitle_DOC<-expression('DOC flux/ Water Table Elevation'~mg~m^-3~s^-1)
ytitle_DIC<-expression('DIC flux/ Water Table Elevation'~mg~m^-3~s^-1)

ytitle_DOC<-expression('DOC'~('mg'~m^-2~s^-1))
ytitle_DIC<-expression('DIC'~('mg'~m^-2~s^-1))
r2title<-expression(r^2 > 0.4)
xtitle<-'Distance (m)'

common_layers <- list(
  geom_point(size = 5),
  xlab(xtitle),
  facet_wrap(~ID, scales = 'free'),
  labs(color = "Wells"),
  theme(axis.title.y = element_text(size = 20, angle = 90),
        strip.text = element_text(size = 15),
        legend.position = "none"),
  stat_poly_line(se = FALSE, color = 'black'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE, color = 'black',
    label.x.npc = "left", label.y.npc = "top", size = 8),
  geom_hline(yintercept = 0, color='darkred', linetype='dashed'))


a<-ggplot(regression_edited %>%filter(well_types!="stream", type=='DOC'),
          aes(x = Distance_m, y = slope))+
  ylab("Slope (DOC~ Water Table Depth)")+
  common_layers
b<-ggplot(regression_edited %>%filter(well_types!="stream", type=='DIC'),
          aes(x = Distance_m, y = slope))+
  ylab("Slope (DIC~ Water Table Depth)")+
  common_layers
dissolved<-plot_grid(a, b, ncol=1)

c<-ggplot(regression_edited %>%filter(well_types!="stream", type=='CO2'),
          aes(x = Distance_m, y = slope))+
  common_layers+  ylab("Slope (CO2~ Water Table Depth)")
d<-ggplot(regression_edited %>%filter(well_types!="stream", type=='CH4'),
          aes(x = Distance_m, y = slope))+
  common_layers+  ylab("Slope (CH4~ Water Table Depth)")
gas<-plot_grid(c, d, ncol=1)


ggsave(filename = paste0("05_Figures/RC_gas.regress.jpeg"),
       plot = gas,
       width = 20, height = 10, units = "in")

ggsave(filename = paste0("05_Figures/RC_dissolved.regress.jpeg"),
       plot = dissolved, width = 20, height = 10, units = "in")

#Hypothesis: 3 greater wetland coverage will exhibit higher RC carbon potential due ##########



RC_df<-left_join(RC_df, wetland_cover)

DOC.df<-RC_df %>%select(DOC_flux,wetland_perc,ID,Well,well_types)%>%
  rename(flux=DOC_flux)%>%mutate(C='DOC')
DIC.df<-RC_df %>%select(DIC_flux,wetland_perc,ID,Well,well_types)%>%
  rename(flux=DIC_flux)%>%mutate(C='DIC')
CO2.df<-RC_df %>%select(lateral_CO2,wetland_perc,ID,Well,well_types)%>%
  rename(flux=lateral_CO2)%>%mutate(C='CO2')
CH4.df<-RC_df %>%select(lateral_CH4,wetland_perc,ID,Well,well_types)%>%
  rename(flux=lateral_CH4)%>%mutate(C='CH4')

flux.hist<-rbind(DOC.df, DIC.df, CO2.df, CH4.df)%>% filter(well_types != 'stream')

flux.hist$wetland_perc <- factor(flux.hist$wetland_perc,
                             levels = sort(unique(flux.hist$wetland_perc)))

labels_vec_flux <- setNames(
  paste0(flux.hist$ID, "\n", flux.hist$wetland_perc),
  flux.hist$ID)

DOC.df<-RC_df %>%select(DOC,wetland_perc,ID,Well,well_types)%>%
  rename(conc=DOC)%>%mutate(C='DOC')
DIC.df<-RC_df %>%select(DIC,wetland_perc,ID,Well,well_types)%>%
  rename(conc=DIC)%>%mutate(C='DIC')
CO2.df<-RC_df %>%select(CO2_umol_L,wetland_perc,ID,Well,well_types)%>%
  rename(conc=CO2_umol_L)%>%mutate(C='CO2')
CH4.df<-RC_df %>%select(CH4_umol_L,wetland_perc,ID,Well,well_types)%>%
  rename(conc=CH4_umol_L)%>%mutate(C='CH4')

conc.hist<-rbind(DOC.df, DIC.df, CO2.df, CH4.df) %>% filter(well_types != 'stream')

conc.hist$wetland_perc <- factor(conc.hist$wetland_perc,
                                 levels = sort(unique(conc.hist$wetland_perc)))

labels_vec_conc <- setNames(
  paste0(conc.hist$ID, "\n", conc.hist$wetland_perc),
  conc.hist$ID)

box_layers <- list(
  geom_boxplot(),
    ggtitle("Influence of Landscape Hydrology on RC Lateral Export"),
    scale_y_log10(),
    xlab("Wetland Cover %"),
    labs(name=" "),
    scale_x_discrete(labels = labels_vec),
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "green", na.rm = T),
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14)))


ggplot(flux.hist %>% filter( C %in% c('CO2', 'CH4')),
       aes(x = reorder(ID, as.numeric(wetland_perc)), y = flux,
           fill = C,group = interaction(ID, C)))+
  scale_x_discrete(labels = labels_vec_flux)+
  box_layers+
  ylab(expression('Flux'~~mg~m^-2~s^-1))

ggplot(flux.hist %>% filter( C %in% c('DIC', 'DOC')),
       aes(x = reorder(ID, as.numeric(wetland_perc)), y = flux,
           fill = C,group = interaction(ID, C)))+
  scale_x_discrete(labels = labels_vec_flux)+
  box_layers+
  ylab(expression('Flux'~~mg~m^-2~s^-1))


ggplot(conc.hist %>% filter( C %in% c('CO2', 'CH4')),
       aes(x = reorder(ID, as.numeric(wetland_perc)),
           y = conc,
           fill = C,group = interaction(ID, C)))+
  scale_x_discrete(labels = labels_vec_conc)+
  box_layers+
  ylab('umol/L')

ggplot(conc.hist %>% filter( C %in% c('DOC', 'DIC')),
       aes(x = reorder(ID, as.numeric(wetland_perc)),
           y = conc/12.01,
           fill = C,group = interaction(ID, C)))+
  scale_x_discrete(labels = labels_vec_conc)+
  box_layers+
  ylab('C mol/L')
