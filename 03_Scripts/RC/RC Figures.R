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


###regressions#####
library(openxlsx)
sheets <- getSheetNames("04_Output/RC_master_by_well.xlsx")
cleaned <- lapply(sheets, function(s) read.xlsx("04_Output/RC_master_by_well.xlsx", sheet = s))
names(cleaned) <- sheets
cleaned <- bind_rows(cleaned, .id = "ID.Well")

cleaned<-left_join(cleaned, w_values)%>%
  mutate(lateral_CO2=CO2_molL*(10^3)*12*86400*(qL/width),
         lateral_CH4=CH4_molL*(10^3)*12*86400*(qL/width))%>%
  mutate(DOC_flux=(qL/1000)/width*DOC*86400,
         DIC_flux=(qL/1000)/width*DIC*86400)

cols <- c('DOC_flux','DIC_flux','DOC','DIC','lateral_CO2','lateral_CH4','qL','WT_elevations','ID.Well')
unique_sites <- unique(cleaned$ID.Well[!is.na(cleaned$ID.Well)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- cleaned %>%
      filter(ID.Well == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

DOC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DOC_flux", "WT_elevations")])) > 1
  DOC.elevation.p <- DOC.elevation.slope <- DOC.elevation.r2 <- NA

  if (valid_elev) {
    DOC.elevation <- lm(DOC_flux ~ WT_elevations, data = df)
    DOC.elevation.cf <- summary(DOC.elevation)
    DOC.elevation.p <- DOC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DOC.elevation.slope <- DOC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DOC.elevation.r2 <- DOC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DOC.elevation.p = as.numeric(DOC.elevation.p),
    DOC.elevation.slope = as.numeric(DOC.elevation.slope),
    DOC.elevation.r2 = as.numeric(DOC.elevation.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")

DIC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DIC_flux", "WT_elevations")])) > 1

  # Initialize with NA
  DIC.elevation.p <- DIC.elevation.slope <- DIC.elevation.r2 <- NA
  DIC.qL.p <- DIC.qL.slope <- DIC.qL.r2 <- NA

  if (valid_elev) {
    DIC.elevation <- lm(DIC_flux ~ WT_elevations, data = df)
    DIC.elevation.cf <- summary(DIC.elevation)
    DIC.elevation.p <- DIC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DIC.elevation.slope <- DIC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DIC.elevation.r2 <- DIC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DIC.elevation.p = as.numeric(DIC.elevation.p),
    DIC.elevation.slope = as.numeric(DIC.elevation.slope),
    DIC.elevation.r2 = as.numeric(DIC.elevation.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")


relationships<-left_join(DOC_table, DIC_table)

sig<-relationships%>%
  mutate(
    DOC.elevation.slope=if_else(DOC.elevation.r2 < 0.4, NA, DOC.elevation.slope),
    DIC.elevation.slope=if_else(DIC.elevation.r2 < 0.4, NA, DIC.elevation.slope)
  )



##Boxplots: distance#########
a<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DIC, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1) + geom_jitter(shape=1, size=3)+
  ylab("DIC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #DIC
  theme(legend.position = 'none')

b<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DOC, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1)+ geom_jitter(shape=1, size=3)+
  ylab("DOC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #DOC
  theme(legend.position = 'none')

c<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CH4_sat, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1)+ geom_jitter(shape=1, size=3)+
  ylab("CH4 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CH4
  theme(legend.position = 'none')

d<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CO2_sat, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1)+ geom_jitter(shape=1, size=3)+
  ylab("CO2 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')

RC_elevation_conc<-plot_grid(a,b,c,d, ncol=1)

ggsave(filename="RC_elevation_conc.jpeg",
       plot = RC_elevation_conc,
       width =35,
       height = 25,
       units = "in")


a<-ggplot(
  cleaned %>% filter(!is.na(Stream)),
  aes(x = WT_elevations, y = DOC_flux, color = as.factor(DistanceID))) +
  geom_point(size=3)+
  geom_smooth(se=F, method=lm)+
  ylab(expression('DOC'~('mg'~m^-2~s^-1))) + xlab("Water Table elevation (m)")+
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')

b<-ggplot(
  cleaned %>% filter(!is.na(Stream)),
  aes(x = WT_elevations, y = DIC_flux, color = as.factor(DistanceID))) +
  geom_point(size=3)+
  geom_smooth(se=F, method=lm)+
  ylab(expression('DIC'~('mg'~m^-2~s^-1))) + xlab("Water Table elevation (m)")+
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')
DIC.DOC_all_wells_by_site_WT<-plot_grid(a,b,ncol=1)

ggsave(filename="05_Figures/RC_WT~DIC&DOC_all_wells_by_site.jpeg",
       plot = DIC.DOC_all_wells_by_site_WT,
       width =35,
       height = 20,
       units = "in")

##DOC/DIC flux  ~ WT#####

common_layers <- list(
  scale_color_gradient(high='orange', low='blue'),
    geom_point(size=3),
    geom_smooth(se=F, method=lm),
    ylab(expression('DOC'~('mg'~m^-2~s^-1))), xlab("Water Table elevation (m)"),
    facet_wrap(~Stream, scales = 'free') ,
    labs(color = "Distance (m)"))

a<-ggplot(
  cleaned %>% filter(!is.na(Stream)),
  aes(x = WT_elevations, y = DOC_flux, color = Distance_m, group=Well)) +
  common_layers+theme(legend.position = 'none')

b<-ggplot(
  cleaned %>% filter(!is.na(Stream)),
  aes(x = WT_elevations, y = DIC_flux, color = Distance_m, group=Well)) +common_layers+
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.5, "cm"))


DIC.DOC_all_wells_by_site_WT<-plot_grid(a,b,ncol=1)

ggsave(filename="05_Figures/RC_WT~DIC&DOC_all_wells_by_site.jpeg",
       plot = DIC.DOC_all_wells_by_site_WT,
       width =35,
       height = 17,
       units = "in")



ytitle_DOC<-expression('DOC'~('mg'~m^-2~s^-1))
ytitle_DIC<-expression('DIC'~('mg'~m^-2~s^-1))

WT_xtitle<-expression('Water Table Elevation'~m)


common_layers <- list(
  geom_point(size = 5),
  xlab(WT_xtitle),
  facet_wrap( ~ fct_reorder(ID.Well, DistanceID),scales = 'free'),
  labs(color = "Wells"),
  stat_poly_line(se = FALSE, color = 'black'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE, color = 'black',
    label.x.npc = "left", label.y.npc = "top", size = 10))


make_plot <- function(data, show_legend = FALSE) {
  p <- ggplot(data, aes(x = WT_elevations, y = DOC, color = as.factor(Well))) +
    common_layers +ylab(ytitle_DOC)

  p2 <- ggplot(data, aes(x = WT_elevations, y = DIC, color = as.factor(Well))) +
    common_layers +ylab(ytitle_DIC)

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
  }

  return(list(DOC_plot = p, DIC_plot = p2))
}


ids <- c('5', '6', '9')

for (id in ids) {
  plots <- make_plot(cleaned %>% filter(ID == id), show_legend = FALSE)
  combined <- plot_grid(plots$DOC_plot, plots$DIC_plot, ncol = 2)

  ggsave(filename = paste0("05_Figures/RC_", id, "_WT.byWell.jpeg"),
         plot = combined, width = 40, height = 15, units = "in")
}

##DOC/DIC (mg/L) ~ qL#####
qL_xtitle<-expression('qL'~m^2~s^-1)

common_layers <- list(
  geom_point(size = 5),
  xlab(qL_xtitle),
  facet_wrap( ~ fct_reorder(ID.Well, DistanceID),scales = 'free'),
  labs(color = "Wells"),
  stat_poly_line(se = FALSE, color = 'black'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE, color = 'black',
    label.x.npc = "left", label.y.npc = "top", size = 10))


make_plot <- function(data, show_legend = FALSE) {
  p <- ggplot(data, aes(x = qL, y = DOC, color = as.factor(Well))) +
    common_layers +ylab(ytitle_DOC)

  p2 <- ggplot(data, aes(x = qL, y = DIC, color = as.factor(Well))) +
    common_layers +ylab(ytitle_DIC)

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
  }

  return(list(DOC_plot = p, DIC_plot = p2))
}


ids <- c('5', '6', '9')

for (id in ids) {
  plots <- make_plot(cleaned %>% filter(ID == id), show_legend = FALSE)
  combined <- plot_grid(plots$DOC_plot, plots$DIC_plot, ncol = 2)

  ggsave(filename = paste0("05_Figures/RC_", id, "_qL.byWell.jpeg"),
         plot = combined, width = 35, height = 15, units = "in")
}

##CO2_sat/CH4Saturation ~ WT_elevations#####
a<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = WT_elevations, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+geom_smooth(se=F, method=lm)+
  ylab("CO2Saturation") + xlab("WT_elevations")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID), scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = WT_elevations, y = CH4_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CH4Saturation") + xlab("WT_elevations")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_5_CH4_sat.CO2_sat.WT_elevations<-plot_grid(a,b, ncol=2))

ggsave(filename="05_Figures/RC_5_CH4_sat.CO2_sat.WT_elevations.jpeg",
       plot = RC_5_CH4_sat.CO2_sat.WT_elevations,
       width =35,height = 15,units = "in")

a<-ggplot(
  cleaned %>% filter(Q>10, ID=='9'),
  aes(x = WT_elevations, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2Saturation") + xlab("WT_elevations")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='9'),
  aes(x = WT_elevations, y = CH4_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CH4Saturation") + xlab("WT_elevations")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_9_CH4_sat.CO2_sat.WT_elevations<-plot_grid(a,b, ncol=2))

ggsave(filename="05_Figures/RC_9_CH4_sat.CO2_sat.WT_elevations.jpeg",
       plot = RC_9_CH4_sat.CO2_sat.WT_elevations,
       width =35,height = 12,units = "in")


a<-ggplot(
  cleaned %>% filter(Q>10, ID=='6'),
  aes(x = WT_elevations, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2_sat") + xlab("WT_elevations")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='6'),
  aes(x = WT_elevations, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2_sat") + xlab("WT_elevations")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_6_CH4_sat.CO2_sat.WT_elevations<-plot_grid(a,b, ncol=2))

ggsave(filename="05_Figures/RC_6_CH4_sat.CO2_sat.WT_elevations.jpeg",
       plot = RC_6_CH4_sat.CO2_sat.WT_elevations,
       width =35,height = 12,units = "in")

##CO2_sat/CH4Saturation ~ qL#####
a<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = qL, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2Saturation") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = qL, y = CH4_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CH4Saturation") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_5_CH4_sat.CO2_sat.qL<-plot_grid(a,b, ncol=2))

ggsave(filename="05_Figures/RC_5_CH4_sat.CO2_sat.qL.jpeg",
       plot = RC_5_CH4_sat.CO2_sat.qL,
       width =35,height = 15,units = "in")

a<-ggplot(
  cleaned %>% filter(Q>10, ID=='9'),
  aes(x = qL, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2Saturation") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='9'),
  aes(x = qL, y = CH4_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CH4Saturation") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_9_CH4_sat.CO2_sat.qL<-plot_grid(a,b, ncol=2))

ggsave(filename="05_Figures/RC_9_CH4_sat.CO2_sat.qL.jpeg",
       plot = RC_9_CH4_sat.CO2_sat.qL,
       width =35,height = 12,units = "in")


a<-ggplot(
  cleaned %>% filter(Q>10, ID=='6'),
  aes(x = qL, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2_sat") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='6'),
  aes(x = qL, y = CO2_sat, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("CO2_sat") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_6_CH4_sat.CO2_sat.qL<-plot_grid(a,b, ncol=2))

ggsave(filename="05_Figures/RC_6_CH4_sat.CO2_sat.qL.jpeg",
       plot = RC_6_CH4_sat.CO2_sat.qL,
       width =35,height = 12,units = "in")

##SLOPES##########
sig_edited<-sig %>% separate(ID, into = c("ID", "Well"), sep = "\\.")
regression_edited<-relationships %>% separate(ID, into = c("ID", "Well"), sep = "\\.")


rgression_dims<-RC_dims %>% separate(Site, into = c("ID", "Well"), sep = "GW")
sig_dim<-left_join(sig_edited, rgression_dims, by=c("ID", "Well"))%>%
  mutate(Well=as.character(Well))%>%
  mutate(Well=if_else(Well=='0','Stream', Well),
         Well=if_else(Well=='8' & ID=='5', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='9', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='6', 'Upland', Well))


relationship_dim<-left_join(regression_edited, rgression_dims, by=c("ID", "Well"))%>%
  mutate(Well=as.character(Well))%>%
  mutate(Well=if_else(Well=='0','Stream', Well),
         Well=if_else(Well=='8' & ID=='5', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='9', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='6', 'Upland', Well))


#Regressions: qL##########

ytitle_DOC<-expression('DOC'~('mg'~m^-2~s^-1))
ytitle_DIC<-expression('DIC'~('mg'~m^-2~s^-1))

common_layers <- list(
  geom_point(size = 5),
  geom_hline(yintercept = 0, linetype = 'dashed'),
  xlab(xtitle),
  facet_wrap(~ID, scales = 'free'),
  labs(color = "Wells"),
  stat_poly_line(se = FALSE, color = 'black'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE, color = 'black',
    label.x.npc = "left", label.y.npc = "top", size = 10))


make_plot <- function(data, title, show_legend = FALSE) {
  p <- ggplot(data, aes(x = Distance_m, y = DOC.qL.slope, color = Well)) +
    common_layers +
    ggtitle(title)+ylab(ytitle_DOC)

  p2 <- ggplot(data, aes(x = Distance_m, y = DIC.qL.slope, color = Well)) +
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
  plots_sig <- make_plot(sig_dim %>% filter(ID == id), r2title, show_legend = FALSE)
  plots_rel <- make_plot(relationship_dim %>% filter(ID == id), "all", show_legend = TRUE)

  combined_DOC <- plot_grid(plots_sig$DOC_plot, plots_rel$DOC_plot, ncol = 1)
  combined_DIC <- plot_grid(plots_sig$DIC_plot, plots_rel$DIC_plot, ncol = 1)

  combined<-plot_grid(combined_DOC, combined_DIC, ncol=2)

  ggsave(filename = paste0("05_Figures/RC_", id, "_qL.regress.jpeg"),
         plot = combined, width = 20, height = 15, units = "in")
}


#Regressions: elevation##########
ytitle_DOC<-expression('DOC flux/ Water Table Elevation'~mg~m^-3~s^-1)
xtitle<-'Distance (m)'
r2title<-expression(r^2 > 0.4)
ytitle_DIC<-expression('DIC flux/ Water Table Elevation'~mg~m^-3~s^-1)

common_layers <- list(
  geom_point(size = 5),
  geom_hline(yintercept = 0, linetype = 'dashed'),
  xlab(xtitle),
  facet_wrap(~ID, scales = 'free'),
  labs(color = "Wells"),
  theme(axis.title.y = element_text(size = 20, angle = 90)),
  stat_poly_line(se = FALSE, color = 'black'),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~")),
    formula = y ~ x,
    parse = TRUE, color = 'black',
    label.x.npc = "left", label.y.npc = "top", size = 10))


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
  plots_rel <- make_plot(relationship_dim %>% filter(ID == id, Well != 'Stream'), "all", show_legend = TRUE)

  combined_DOC <- plot_grid(plots_sig$DOC_plot, plots_rel$DOC_plot, ncol = 1)
  combined_DIC <- plot_grid(plots_sig$DIC_plot, plots_rel$DIC_plot, ncol = 1)

  combined<-plot_grid(combined_DOC, combined_DIC, ncol=2)
  assign(paste0('RC_', id), combined)
}

combined<-plot_grid(print(RC_5), print(RC_6), print(RC_9), ncol=3)
ggsave(filename = paste0("05_Figures/RC__WT.regress.jpeg"),
       plot = combined, width = 37, height = 12, units = "in")
