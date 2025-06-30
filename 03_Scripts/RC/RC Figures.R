#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(cowplot)
library(lme4)
library(ggpmisc)

theme_set(theme(axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 20),
                axis.title.y = element_text(size = 20, angle = 90),
                axis.title.x = element_text(size = 20),
                plot.title = element_text(size = 20),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


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

cols <- c('DOC','DIC','lateral_CO2','lateral_CH4','qL','WT_elevations','ID.Well')
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
  valid_elev <- sum(complete.cases(df[, c("DOC", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("DOC", "qL")])) > 1

  # Initialize with NA
  DOC.elevation.p <- DOC.elevation.slope <- DOC.elevation.r2 <- NA
  DOC.qL.p <- DOC.qL.slope <- DOC.qL.r2 <- NA

  if (valid_elev) {
    DOC.elevation <- lm(DOC ~ WT_elevations, data = df)
    DOC.elevation.cf <- summary(DOC.elevation)
    DOC.elevation.p <- DOC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DOC.elevation.slope <- DOC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DOC.elevation.r2 <- DOC.elevation.cf$r.squared
  }

  if (valid_qL) {
    DOC.qL <- lm(DOC ~ qL, data = df)
    DOC.qL.cf <- summary(DOC.qL)
    DOC.qL.p <- DOC.qL.cf$coefficients["qL", "Pr(>|t|)"]
    DOC.qL.slope <- DOC.qL.cf$coefficients["qL", "Estimate"]
    DOC.qL.r2 <- DOC.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DOC.elevation.p = as.numeric(DOC.elevation.p),
    DOC.elevation.slope = as.numeric(DOC.elevation.slope),
    DOC.elevation.r2 = as.numeric(DOC.elevation.r2),
    DOC.qL.p = as.numeric(DOC.qL.p),
    DOC.qL.slope = as.numeric(DOC.qL.slope),
    DOC.qL.r2 = as.numeric(DOC.qL.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")

DIC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DIC", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("DIC", "qL")])) > 1

  # Initialize with NA
  DIC.elevation.p <- DIC.elevation.slope <- DIC.elevation.r2 <- NA
  DIC.qL.p <- DIC.qL.slope <- DIC.qL.r2 <- NA

  if (valid_elev) {
    DIC.elevation <- lm(DIC ~ WT_elevations, data = df)
    DIC.elevation.cf <- summary(DIC.elevation)
    DIC.elevation.p <- DIC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DIC.elevation.slope <- DIC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DIC.elevation.r2 <- DIC.elevation.cf$r.squared
  }

  if (valid_qL) {
    DIC.qL <- lm(DIC ~ qL, data = df)
    DIC.qL.cf <- summary(DIC.qL)
    DIC.qL.p <- DIC.qL.cf$coefficients["qL", "Pr(>|t|)"]
    DIC.qL.slope <- DIC.qL.cf$coefficients["qL", "Estimate"]
    DIC.qL.r2 <- DIC.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DIC.elevation.p = as.numeric(DIC.elevation.p),
    DIC.elevation.slope = as.numeric(DIC.elevation.slope),
    DIC.elevation.r2 = as.numeric(DIC.elevation.r2),
    DIC.qL.p = as.numeric(DIC.qL.p),
    DIC.qL.slope = as.numeric(DIC.qL.slope),
    DIC.qL.r2 = as.numeric(DIC.qL.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")

lateral_CO2_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("lateral_CO2", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("lateral_CO2", "qL")])) > 1

  # Initialize with NA
  lateral_CO2.elevation.p <- lateral_CO2.elevation.slope <- lateral_CO2.elevation.r2 <- NA
  lateral_CO2.qL.p <- lateral_CO2.qL.slope <- lateral_CO2.qL.r2 <- NA

  if (valid_elev) {
    lateral_CO2.elevation <- lm(lateral_CO2 ~ WT_elevations, data = df)
    lateral_CO2.elevation.cf <- summary(lateral_CO2.elevation)
    lateral_CO2.elevation.p <- lateral_CO2.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    lateral_CO2.elevation.slope <- lateral_CO2.elevation.cf$coefficients["WT_elevations", "Estimate"]
    lateral_CO2.elevation.r2 <- lateral_CO2.elevation.cf$r.squared
  }

  if (valid_qL) {
    lateral_CO2.qL <- lm(lateral_CO2 ~ qL, data = df)
    lateral_CO2.qL.cf <- summary(lateral_CO2.qL)
    lateral_CO2.qL.p <- lateral_CO2.qL.cf$coefficients["qL", "Pr(>|t|)"]
    lateral_CO2.qL.slope <- lateral_CO2.qL.cf$coefficients["qL", "Estimate"]
    lateral_CO2.qL.r2 <- lateral_CO2.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    lateral_CO2.elevation.p = as.numeric(lateral_CO2.elevation.p),
    lateral_CO2.elevation.slope = as.numeric(lateral_CO2.elevation.slope),
    lateral_CO2.elevation.r2 = as.numeric(lateral_CO2.elevation.r2),
    lateral_CO2.qL.p = as.numeric(lateral_CO2.qL.p),
    lateral_CO2.qL.slope = as.numeric(lateral_CO2.qL.slope),
    lateral_CO2.qL.r2 = as.numeric(lateral_CO2.qL.r2)
  )
})
CO2_table <- bind_rows(lateral_CO2_relationships, .id = "ID")

lateral_CH4_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("lateral_CH4", "WT_elevations")])) > 1
  valid_qL <- sum(complete.cases(df[, c("lateral_CH4", "qL")])) > 1

  # Initialize with NA
  lateral_CH4.elevation.p <- lateral_CH4.elevation.slope <- lateral_CH4.elevation.r2 <- NA
  lateral_CH4.qL.p <- lateral_CH4.qL.slope <- lateral_CH4.qL.r2 <- NA

  if (valid_elev) {
    lateral_CH4.elevation <- lm(lateral_CH4 ~ WT_elevations, data = df)
    lateral_CH4.elevation.cf <- summary(lateral_CH4.elevation)
    lateral_CH4.elevation.p <- lateral_CH4.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    lateral_CH4.elevation.slope <- lateral_CH4.elevation.cf$coefficients["WT_elevations", "Estimate"]
    lateral_CH4.elevation.r2 <- lateral_CH4.elevation.cf$r.squared
  }

  if (valid_qL) {
    lateral_CH4.qL <- lm(lateral_CH4 ~ qL, data = df)
    lateral_CH4.qL.cf <- summary(lateral_CH4.qL)
    lateral_CH4.qL.p <- lateral_CH4.qL.cf$coefficients["qL", "Pr(>|t|)"]
    lateral_CH4.qL.slope <- lateral_CH4.qL.cf$coefficients["qL", "Estimate"]
    lateral_CH4.qL.r2 <- lateral_CH4.qL.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    lateral_CH4.elevation.p = as.numeric(lateral_CH4.elevation.p),
    lateral_CH4.elevation.slope = as.numeric(lateral_CH4.elevation.slope),
    lateral_CH4.elevation.r2 = as.numeric(lateral_CH4.elevation.r2),
    lateral_CH4.qL.p = as.numeric(lateral_CH4.qL.p),
    lateral_CH4.qL.slope = as.numeric(lateral_CH4.qL.slope),
    lateral_CH4.qL.r2 = as.numeric(lateral_CH4.qL.r2)
  )
})
CH4_table <- bind_rows(lateral_CH4_relationships, .id = "ID")

relationships<-left_join(DOC_table, DIC_table)
relationships<-left_join(relationships, CO2_table)
relationships<-left_join(relationships, CH4_table)

sig<-relationships%>%
  mutate(
    lateral_CH4.qL.slope=if_else(lateral_CH4.qL.r2 < 0.4, NA, lateral_CH4.qL.slope),
    lateral_CH4.elevation.slope=if_else(lateral_CH4.elevation.r2 < 0.4, NA, lateral_CH4.elevation.slope),

    lateral_CO2.qL.slope=if_else(lateral_CO2.qL.r2 < 0.4, NA, lateral_CO2.qL.slope),
    lateral_CO2.elevation.slope=if_else(lateral_CO2.elevation.r2 < 0.4, NA, lateral_CO2.elevation.slope),

    DOC.qL.slope=if_else(DOC.qL.r2 > 0.4, NA, DOC.qL.slope),
    DOC.elevation.slope=if_else(DOC.elevation.r2 < 0.4, NA, DOC.elevation.slope),

    DIC.qL.slope=if_else(DIC.qL.r2 > 0.4, NA, DIC.qL.slope),
    DIC.elevation.slope=if_else(DIC.elevation.r2 < 0.4, NA, DIC.elevation.slope)
  )



##Boxplots: distance#########
a<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DIC, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1) + geom_jitter(shape=1)+
  ylab("DIC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #DIC
  theme(legend.position = 'none')

b<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = DOC, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1)+ geom_jitter(shape=1)+
  ylab("DOC mg/L") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #DOC
  theme(legend.position = 'none')

c<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CH4_sat, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1)+ geom_jitter(shape=1)+
  ylab("CH4 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CH4
  theme(legend.position = 'none')

d<-ggplot(
  lateral_flux %>% filter(!is.na(Stream)),
  aes(x = Distance_m, y = CO2_sat, fill = as.factor(DistanceID))) +
  geom_boxplot(width=1)+ geom_jitter(shape=1)+
  ylab("CO2 Saturation") + xlab("Distance (m)")+
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')

RC_elevation_conc<-plot_grid(a,b,c,d, ncol=1)

ggsave(filename="04_Output/Figures/RC_elevation_conc.jpeg",
       plot = RC_elevation_conc,
       width =35,
       height = 25,
       units = "in")

ggplot(
  cleaned %>% filter(!is.na(Stream)),
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC") + xlab("Distance (m)")+
  facet_wrap(~Stream, scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')

##DOC/DIC (mg/L) ~ WT#####
a<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = WT_elevations, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+geom_smooth(se=F, method=lm)+
  ylab("DOC (mg/L)") + xlab("Water Table Elevation (m)")+
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
  aes(x = WT_elevations, y = DIC, color = as.factor(DistanceID))) +
  geom_point()+geom_smooth(se=F, method=lm)+
  ylab("DIC (mg/L)") + xlab("Water Table Elevation (m)")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_5_DIC.DOC_mg.WT<-plot_grid(a,b, ncol=2))

ggsave(filename="04_Output/Figures/RC_5_DIC.DOC_mg.WT.jpeg",
       plot = RC_5_DIC.DOC_mg.WT,
       width =35,height = 15,units = "in")

a<-ggplot(
  cleaned %>% filter(Q>10, ID=='9'),
  aes(x = WT_elevations, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC (mg/L)") + xlab("Water Table Elevation (m)")+
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
  aes(x = WT_elevations, y = DIC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DIC (mg/L)") + xlab("Water Table Elevation (m)")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_9_DIC.DOC_mg.WT<-plot_grid(a,b, ncol=2))

ggsave(filename="04_Output/Figures/RC_9_DIC.DOC_mg.WT.jpeg",
       plot = RC_9_DIC.DOC_mg.WT,
       width =35,height = 12,units = "in")


a<-ggplot(
  cleaned %>% filter(Q>10, ID=='6'),
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC") + xlab("Distance (m)")+
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
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC") + xlab("Distance (m)")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_6_DIC.DOC_mg.WT<-plot_grid(a,b, ncol=2))

ggsave(filename="04_Output/Figures/RC_6_DIC.DOC_mg.WT.jpeg",
       plot = RC_6_DIC.DOC_mg.WT,
       width =35,height = 12,units = "in")

##DOC/DIC (mg/L) ~ qL#####
a<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+geom_smooth(se=F, method=lm)+
  ylab("DOC (mg/L)") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

b<-ggplot(
  cleaned %>% filter(Q>10, ID=='5'),
  aes(x = qL, y = DIC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DIC (mg/L)") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_5_DIC.DOC_mg.qL<-plot_grid(a,b, ncol=2))

ggsave(filename="04_Output/Figures/RC_5_DIC.DOC_mg.qL.jpeg",
       plot = RC_5_DIC.DOC_mg.qL,
       width =35,height = 15,units = "in")

a<-ggplot(
  cleaned %>% filter(Q>10, ID=='9'),
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC (mg/L)") + xlab("qL")+
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
  aes(x = qL, y = DIC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DIC (mg/L)") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_9_DIC.DOC_mg.qL<-plot_grid(a,b, ncol=2))

ggsave(filename="04_Output/Figures/RC_9_DIC.DOC_mg.qL.jpeg",
       plot = RC_9_DIC.DOC_mg.qL,
       width =35,height = 12,units = "in")


a<-ggplot(
  cleaned %>% filter(Q>10, ID=='6'),
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC") + xlab("qL")+
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
  aes(x = qL, y = DOC, color = as.factor(DistanceID))) +
  geom_point()+
  geom_smooth(se=F, method=lm)+
  ylab("DOC") + xlab("qL")+
  facet_wrap(
    ~ fct_reorder(ID.Well, DistanceID),  # Key fix: reorder wells by DistanceID
    scales = 'free') +
  labs(fill = "Wells")+ #CO2
  theme(legend.position = 'none')+
  stat_poly_eq(aes(label = paste(..p.value.label..,..rr.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

(RC_6_DIC.DOC_mg.qL<-plot_grid(a,b, ncol=2))

ggsave(filename="04_Output/Figures/RC_6_DIC.DOC_mg.qL.jpeg",
       plot = RC_6_DIC.DOC_mg.qL,
       width =35,height = 12,units = "in")
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

ggsave(filename="04_Output/Figures/RC_5_CH4_sat.CO2_sat.WT_elevations.jpeg",
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

ggsave(filename="04_Output/Figures/RC_9_CH4_sat.CO2_sat.WT_elevations.jpeg",
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

ggsave(filename="04_Output/Figures/RC_6_CH4_sat.CO2_sat.WT_elevations.jpeg",
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

ggsave(filename="04_Output/Figures/RC_5_CH4_sat.CO2_sat.qL.jpeg",
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

ggsave(filename="04_Output/Figures/RC_9_CH4_sat.CO2_sat.qL.jpeg",
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

ggsave(filename="04_Output/Figures/RC_6_CH4_sat.CO2_sat.qL.jpeg",
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
a<-ggplot(
  sig_dim %>%filter(ID=='5'), aes(x = Distance_m, y = DOC.qL.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle(expression(r^2 > 0.4))+
  ylab(expression('DOC'~('mg'~L^-1)/'qL'~(m^2~s)))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DOC.qL.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle("all")+
  ylab(expression('DOC'~('mg'~L^-1)/'qL'~(m^2~s)))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")
RC_5_DOC.qL.regress<-plot_grid(a,b, ncol = 1)

a<-ggplot(
  sig_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DIC.qL.slope, color=Well)) +
  ggtitle(expression(r^2 > 0.4))+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'qL'~m^2~s))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DIC.qL.slope, color=Well)) +
  ggtitle('all')+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'qL'~m^2~s))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

RC_5_DIC.qL.regress<-plot_grid(a,b, ncol = 1)

RC_5_qL.regress<-plot_grid(RC_5_DOC.qL.regress,RC_5_DIC.qL.regress, ncol = 2)


ggsave(filename="04_Output/Figures/RC_5_qL.regress.jpeg",
       plot = RC_5_qL.regress,
       width =20,height = 15,units = "in")


a<-ggplot(
  sig_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DOC.qL.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle(expression(r^2 > 0.4))+
  ylab(expression('DOC'~('mg'~L^-1)/'qL'~(m^2~s)))+xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DOC.qL.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle("all")+
  ylab(expression('DOC'~('mg'~L^-1)/'qL'~(m^2~s)))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")
RC_6_DOC.qL.regress<-plot_grid(a,b, ncol = 1)

a<-ggplot(
  sig_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DIC.qL.slope, color=Well)) +
  ggtitle(expression(r^2 > 0.4))+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'qL'~m^2~s))+xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DIC.qL.slope, color=Well)) +
  ggtitle('all')+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'qL'~m^2~s))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

RC_6_DIC.qL.regress<-plot_grid(a,b, ncol = 1)

RC_6_qL.regress<-plot_grid(RC_6_DOC.qL.regress,RC_6_DIC.qL.regress, ncol = 2)


ggsave(filename="04_Output/Figures/RC_6_qL.regress.jpeg",
       plot = RC_6_qL.regress,
       width =20,height = 15,units = "in")


a<-ggplot(
  sig_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DOC.qL.slope, color=Well)) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_point(size=3)+
  ggtitle(expression(r^2 > 0.4))+
  ylab(expression('DOC'~('mg'~L^-1)/'qL'~(m^2~s)))+xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DOC.qL.slope, color=Well)) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  geom_point(size=3)+
  ggtitle("all")+
  ylab(expression('DOC'~('mg'~L^-1)/'qL'~(m^2~s)))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")
RC_9_DOC.qL.regress<-plot_grid(a,b, ncol = 1)

a<-ggplot(
  sig_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DIC.qL.slope, color=Well)) +
  ggtitle(expression(r^2 > 0.4))+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'qL'~m^2~s))+xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")


b<-ggplot(
  relationship_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DIC.qL.slope, color=Well)) +
  ggtitle('all')+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'qL'~m^2~s))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

RC_9_DIC.qL.regress<-plot_grid(a,b, ncol = 1)

RC_9_qL.regress<-plot_grid(RC_9_DOC.qL.regress,RC_9_DIC.qL.regress, ncol = 2)


ggsave(filename="04_Output/Figures/RC_9_qL.regress.jpeg",
       plot = RC_9_qL.regress,
       width =20,height = 15,units = "in")



ggplot(
  sig_dim%>% filter(lateral_CO2.qL.slope >0), aes(x = Distance_m, y = lateral_CO2.qL.slope, color=Well)) +
  ylab(expression(CO[2]~g/m^2/day /'qL'~(m^2~s)))+xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  geom_point(size=3)

ggplot(
  sig_dim%>% filter(lateral_CO2.qL.slope >0), aes(x = Distance_m, y = lateral_CH4.qL.slope, color=Well)) +
  ylab(expression(CO[2]~g/m^2/day /'qL'~(m^2~s)))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  geom_point(size=3)





#Regressions: elevation##########
a<-ggplot(
  sig_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DOC.elevation.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle(expression(r^2 > 0.4))+
  ylab(expression('DOC'~('mg'~L^-1)/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DOC.elevation.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle("all")+
  ylab(expression('DOC'~('mg'~L^-1)/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")
RC_5_DOC.elevation.regress<-plot_grid(a,b, ncol = 1)


a<-ggplot(
  sig_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DIC.elevation.slope, color=Well)) +
  ggtitle(expression(r^2 > 0.4))+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = 'none')


b<-ggplot(
  relationship_dim%>%filter(ID=='5'), aes(x = Distance_m, y = DIC.elevation.slope, color=Well)) +
  ggtitle('all')+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

RC_5_DIC.elevation.regress<-plot_grid(a,b, ncol = 1)

RC_5_elevation.regress<-plot_grid(RC_5_DOC.elevation.regress,
                                  RC_5_DIC.elevation.regress, ncol = 2)

ggsave(filename="04_Output/Figures/RC_5.elevation.regress.jpeg",
       plot = RC_5_elevation.regress,
       width =20,height = 15,units = "in")


a<-ggplot(
  sig_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DOC.elevation.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle(expression(r^2 > 0.4))+
  ylab(expression('DOC'~('mg'~L^-1)/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DOC.elevation.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle("all")+
  ylab(expression('DOC'~('mg'~L^-1)/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")
RC_6_DOC.elevation.regress<-plot_grid(a,b, ncol = 1)


a<-ggplot(
  sig_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DIC.elevation.slope, color=Well)) +
  ggtitle(expression(r^2 > 0.4))+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = 'none')

b<-ggplot(
  relationship_dim%>%filter(ID=='6'), aes(x = Distance_m, y = DIC.elevation.slope, color=Well)) +
  ggtitle('all')+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

RC_6_DIC.elevation.regress<-plot_grid(a,b, ncol = 1)

RC_6_elevation.regress<-plot_grid(RC_6_DOC.elevation.regress,
                                  RC_6_DIC.elevation.regress, ncol = 2)

ggsave(filename="04_Output/Figures/RC_6.elevation.regress.jpeg",
       plot = RC_6_elevation.regress,
       width =20,height = 15,units = "in")


a<-ggplot(
  sig_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DOC.elevation.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle(expression(r^2 > 0.4))+
  ylab(expression('DOC'~('mg'~L^-1)/'Water Table (m)'))+xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = "none")

b<-ggplot(
  relationship_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DOC.elevation.slope, color=Well)) +
  geom_point(size=3)+
  ggtitle("all")+
  ylab(expression('DOC'~('mg'~L^-1)/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")
RC_9_DOC.elevation.regress<-plot_grid(a,b, ncol = 1)


a<-ggplot(
  sig_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DIC.elevation.slope, color=Well)) +
  ggtitle(expression(r^2 > 0.4))+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  theme(legend.position = 'none')

b<-ggplot(
  relationship_dim%>%filter(ID=='9'), aes(x = Distance_m, y = DIC.elevation.slope, color=Well)) +
  ggtitle('all')+
  geom_point(size=3)+
  ylab(expression('DIC'~'mg'~L^-1/'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")

RC_9_DIC.elevation.regress<-plot_grid(a,b, ncol = 1)

RC_9_elevation.regress<-plot_grid(RC_9_DOC.elevation.regress,
                                  RC_9_DIC.elevation.regress, ncol = 2)

ggsave(filename="04_Output/Figures/RC_9.elevation.regress.jpeg",
       plot = RC_9_elevation.regress,
       width =20,height = 15,units = "in")






ggplot(
  sig_dim%>% filter(lateral_CO2.elevation.slope >0), aes(x = Distance_m, y = lateral_CO2.elevation.slope, color=Well)) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  ylab(expression(CO[2]~g/m^2/day /'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  geom_point(size=3)

ggplot(
  sig_dim%>% filter(lateral_CO2.elevation.slope >0), aes(x = Distance_m, y = lateral_CH4.elevation.slope, color=Well)) +
  scale_fill_brewer(palette = "Set0") +  # Use a discrete color palette
  ylab(expression(CO[2]~g/m^2/day /'Water Table (m)'))+
  xlab('Distance (m)')+
  facet_wrap(~ID, scales = 'free') +
  labs(color = "Wells")+
  stat_poly_line(se = FALSE, color='black')+
  stat_poly_eq(aes(label = paste(..eq.label.., ..p.value.label..,sep = "~~~~~")),
               formula = y ~ x,  # If you're plotting log10 on the x-axis only
               parse = TRUE, color = 'black',
               label.x.npc = "left", label.y.npc = "top")+
  geom_point(size=3)




