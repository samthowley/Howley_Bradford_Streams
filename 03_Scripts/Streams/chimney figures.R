#packages
library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(ggpmisc)
library(openxlsx)


theme_set(theme(axis.text.x = element_text(size = 17),
                axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 21, angle = 90),
                axis.title.x = element_text(size = 21),
                plot.title = element_text(size = 21),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 12),
                legend.title =element_blank(),
                legend.position ="bottom",
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))

common_theme<-list( theme(
  axis.title.x = element_blank(),
  axis.title.y = element_text(size=21, angle=90),
  plot.title = element_text(size = 21),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  legend.key.height = unit(0.7, "cm"),
  legend.key.width = unit(1, "cm")))

active <- read_csv("04_Output/external-internal.csv")%>%
  mutate(
    Basin=as.factor(Basin),
    NEP=GPP+ER_corrected,
    Q_med=  case_when(Q>= median(Q, na.rm = T)~ "sup",
                      Q<=median(Q, na.rm = T)~"inf"),
    Q_ID= paste0(ID, sep="_", Q_med)
    )#%>%
  filter(active.passive>0.015 & active.passive<150)
#Include wetlands####

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>%
  select(Basin_Name, PERCENTAGE) %>% rename(Basin=Basin_Name, wetland_perc=PERCENTAGE)%>%
  mutate(wetland_perc=round(wetland_perc, 2))

active<-full_join(active, wetland_cover)%>%
  mutate(ID_wetperc=paste0(ID, wetland_perc, sep="_"))%>%
  filter(!is.na(ID))

active<-active%>%
  mutate(ID_wetperc = paste(ID, wetland_perc, sep = "_"))%>%
  mutate(ID_wetprox = paste(ID, wetland_perc, sep = "_"))%>%
  filter(!ID %in% c('14', '6a'))%>%filter(!is.na(ID))%>%
  filter(active.passive<500 & active.passive>0)

active$wetland_perc <- factor(active$wetland_perc,
                              levels = sort(unique(active$wetland_perc), decreasing = TRUE))

labels_vec_wetperc <- setNames(
  paste0(active$ID, "\n", active$wetland_perc),
  active$ID_wetperc)

#External v Internal Violin Plots###########

active%>% group_by(ID)%>%
  summarize(CO2=mean(CO2, na.rm=T),
            CO2_flux=mean(CO2_flux, na.rm=T),

            CO2_flux.min=min(CO2_flux, na.rm=T),
            CO2_flux.max=max(CO2_flux, na.rm=T),

            CO2.min=min(CO2, na.rm=T),
            CO2.max=max(CO2, na.rm=T)
            )

common_layers <- list(geom_violin(size=1),
                      geom_jitter(shape=1),
                      scale_y_log10(),
                          ggtitle("Internal:External Dominance Among Sites"),
                          geom_hline(yintercept = 1, color='red', size=1),
                          ylab("Internal/ External"),
                          theme(axis.title.x = element_blank(),
                                axis.title.y= element_text(size=21, angle=90),
                                plot.title = element_text(size = 21)))

active%>%group_by(ID)%>%filter(Q>2)%>%
  mutate(prop=active/CO2_flux)%>%
  summarise(
    min_=min(prop, na.rm = T)*100,
    max_=max(prop, na.rm = T)*100,
    avg_=mean(prop, na.rm = T)*100)

(a<-ggplot(active%>%filter(),
       aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),
           y = active.passive)) +common_layers+
    xlab("Wetland Cover %")+
  scale_x_discrete(labels = labels_vec_wetperc)+
    theme(axis.title.x = element_text(size = 18)))

summary(lm(active.passive ~ as.numeric(wetland_perc), data = active))

ggsave(filename = "05_Figures/External.Interval.violin.plots.jpeg",
       plot = a,
       width = 8, height = 6, units = "in")


summary(lm(passive ~ as.numeric(active.passive), data = active))
summary(glm(passive ~ as.numeric(active.passive), data = active, family = gaussian))

active%>%
  group_by(ID)%>%
  summarize(active.passive.mean=mean(active.passive, na.rm=T),
            active.passive.min=min(active.passive, na.rm=T),
            active.passive.max=max(active.passive, na.rm=T),

            act_dom=sum(active.passive >1 , na.rm = TRUE),
            pass_dom=sum(active.passive <1 , na.rm = TRUE),
            tot=sum(active.passive >0 , na.rm = TRUE), #needed for the denominator

            act_days=act_dom/tot*100,
            pass__days=pass_dom/tot*100,

            act_perc.min=min(active/CO2_flux, na.rm=T),
            act_perc.max=max(active/CO2_flux, na.rm=T)
            )%>%
  select(ID, active.passive.mean, active.passive.max, active.passive.min, act_days,
         act_perc.min, act_perc.max)

ggplot(active,
  aes(x = Q, y = active.passive)) +
  geom_point(shape = 21) +
  common_theme+
  geom_hline(yintercept = 1, linetype='dashed')+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(aes(x = log10(Q), y = log10(active.passive),
    group = ID,label = paste(..p.value.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4,label.x.npc = "right",label.y.npc = 0.017,vstep=0.07)


#slopes##########

active$ID <- with(active, reorder(ID, -as.numeric(as.character(wetland_perc))))

int<-active%>%
  select(ID, Q, Date, Q_med, active)%>%
  rename(rate=active)%>%mutate(type='internal')

ext<-active%>%
  select(ID, Q, Date, Q_med, passive)%>%
  rename(rate=passive)%>%mutate(type='external')
active.hist<-rbind(int, ext)

active.hist$ID <- factor(active.hist$ID, levels = c('13','15','3','5','5a','6','7','9'))

col<-c("internal" ='red', "external"='black')

(ggplot(
  active.hist,
  aes(x = Q, y = rate, color = type, group = interaction(type, Q_med))) +
  geom_point(shape = 21) +
  stat_poly_line(formula = y ~ x, se = FALSE) +
    stat_poly_eq(aes(x = log10(Q), y = log10(rate), group = type, color=type,
                     label = paste(..p.value.label.., sep = "~~~")),
                 formula = y ~ x, parse = TRUE,
                 size = 4,label.x.npc = "right",label.y.npc = 0.017,hstep=0.2)+
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  scale_colour_manual(name="", values = col,labels=c("External", "Internal"))+
  common_theme +
  ggtitle(expression(CO[2]~Flux-Q~Relationship)) +
  scale_x_log10() + scale_y_log10() +
  theme(strip.text = element_text(size = 15)) +
  xlab("Discharge L/s") +
  theme(axis.title.x = element_text(size = 21)))

ggsave(filename = "05_Figures/external-internal.scatter.jpeg",

       plot=ggplot(
         active.hist,
         aes(x = Q, y = rate, color = type, group = interaction(type, Q_med))) +
         geom_point(shape = 21) +
         stat_poly_line(formula = y ~ x, se = FALSE) +
         ylab(expression('g'/m^2/'day')) +
         facet_wrap(~ID, ncol = 3, scales = 'free') +
         scale_colour_manual(name="", values = col,labels=c("External", "Internal"))+
         common_theme +
         ggtitle(expression(CO[2]~Flux-Q~Dynamic)) +
         scale_x_log10() + scale_y_log10() +
         theme(strip.text = element_text(size = 15)) +
         xlab("Discharge L/s") +
         theme(axis.title.x = element_text(size = 21)),

       width = 12, height = 8, units = "in")


slopes <- read_csv("04_Output/external-internal_slopes.csv")

slopes$ID <- factor(slopes$ID, levels = c('15','5','5a','3','6','13','7','9'))

rel_internal_split<-slopes%>% select(ID, Q_med, active_slope, active_pvalue)%>%
  mutate(type='internal')%>% rename(slope=active_slope, pvalue=active_pvalue)

rel_external_split<-slopes%>% select(ID, Q_med, passive_slope, passive_pvalue)%>%
  mutate(type='external')%>% rename(slope=passive_slope, pvalue=passive_pvalue)

slopes.hist_split<-rbind(rel_internal_split, rel_external_split)

library(ggbreak)
library(plotly)

ggplot(slopes.hist_split %>%filter(pvalue<0.005), aes(x = ID, y=slope,color= type, shape=Q_med)) +
  geom_point(size=4, stroke=1.5) +
  scale_color_manual(values = c('black','red'),
                     labels=c("External Pathway", "Internal Pathway")) +
  scale_shape_manual(values = c(16,1),
                     name = expression(Q[50]),              # <-- Set legend title
                     labels = c("< Median Q", "> Median Q"))+  # Pick as many shapes as needed
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.2, linetype='dashed')+
  geom_hline(yintercept = -0.2, linetype='dashed')+
  common_theme +
  ggtitle("Log-Log Relationships of Active vs Passive")+scale_y_break(c(2, 8))

#H3: Wetland Cover:###########

int.hist<-active%>%select(ID, wetland_perc, ID_wetperc, active)%>%
  mutate(type='Internal')%>%rename(flux=active)
ext.hist<-active%>%select(ID, wetland_perc, ID_wetperc, passive)%>%
  mutate(type='External')%>%rename(flux=passive)
chimney.hist<-rbind(int.hist, ext.hist)

chimney.hist$wetland_perc <- factor(chimney.hist$wetland_perc,
                              levels = sort(unique(chimney.hist$wetland_perc), decreasing = TRUE))

labels_vec_wetperc_hist <- setNames(
  paste0(chimney.hist$ID, "\n", chimney.hist$wetland_perc),
  chimney.hist$ID_wetperc)

ggplot(chimney.hist,
           aes(x = reorder(ID_wetperc, as.numeric(wetland_perc)),
               y = flux, fill=type)) +
  geom_boxplot(outliers = F)+
    xlab("Wetland Cover %")+
    scale_x_discrete(labels = labels_vec_wetperc)+
    theme(axis.title.x = element_text(size = 18))


remove<-active%>%filter(!ID %in% c('7', '9'))
summary(lm(active.passive ~ as.numeric(wetland_perc), data = remove))


(a<-ggplot(active%>%filter(),
           aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),
               y = active.passive)) +common_layers+
    xlab("Wetland Cover %")+
    scale_x_discrete(labels = labels_vec_wetperc)+
    theme(axis.title.x = element_text(size = 18)))

summary(lm(active.passive ~ as.numeric(wetland_perc), data = active))


ggplot(active%>% filter(!is.na(ID)),
           aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),  y = GPP)) +
    geom_boxplot(position = position_dodge(width = 0.75), outliers=F) +
    ggtitle("Metabolic Regime") +
    ylab(expression(O[2]~'g' / m^2 / 'day')) +
    common_theme+
    scale_x_discrete(labels = labels_vec_wetperc)

summary(lm(ER_corrected ~ as.numeric(wetland_perc), data = active))
summary(glm(GPP ~ as.numeric(wetland_perc), data = active, family = gaussian))

ggplot(active%>% filter(!is.na(ID)),
       aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),  y = CO2_flux)) +
  geom_boxplot(position = position_dodge(width = 0.75), outliers=F) +
  ggtitle("Metabolic Regime") +
  ylab(expression(CO[2]~'g' / m^2 / 'day')) +
  common_theme+
  scale_x_discrete(labels = labels_vec_wetperc)

summary(glm(GPP ~ as.numeric(wetland_perc), data = active, family = gaussian))
