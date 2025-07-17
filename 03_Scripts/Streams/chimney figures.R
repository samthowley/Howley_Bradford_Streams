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

#Include wetlands####

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>%
  select(Basin_Name, PERCENTAGE) %>% rename(Basin=Basin_Name, wetland_perc=PERCENTAGE)%>%
  mutate(wetland_perc=round(wetland_perc, 2))
active<-full_join(active, wetland_cover)%>%
  mutate(ID_wetperc=paste0(ID, wetland_perc, sep="_"))

#metabolic regime######
met_hist.GPP<-active%>%select(Date, ID, GPP, wetland_perc)%>%
  rename(met=GPP)%>%mutate(type='GPP')
met_hist.ER<-active%>%select(Date, ID, ER_corrected, wetland_perc)%>%
  rename(met=ER_corrected)%>%mutate(type='ER', met=met*-1)
met_hist<-rbind(met_hist.GPP, met_hist.ER)%>%
  mutate(ID_q = paste(ID, wetland_perc, sep = "_"))%>% filter(!ID %in% c('14', '6a'))

met_hist$wetland_perc <- factor(met_hist$wetland_perc,
                                levels = sort(unique(met_hist$wetland_perc), decreasing = TRUE))

labels_vec <- setNames(
  paste0(met_hist$ID, "\n", met_hist$wetland_perc),
  met_hist$ID_q)

a<-
  ggplot(met_hist%>% filter(!is.na(ID)),
       aes(x = reorder(ID_q, -as.numeric(wetland_perc)),  y = met, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c('brown','darkgreen'), name= "") +
  ggtitle("Metabolic Regime") +
  ylab(expression(O[2]~'g' / m^2 / 'day')) +
  common_theme+
  scale_x_discrete(labels = labels_vec)

active%>%
  group_by(ID)%>%
  summarise(
    CO2.flux=mean(CO2_flux, na.rm=T))
range(active$CO2_flux, na.rm=T)

summary(lm(CO2 ~ wetland_perc, data = active))
library(lme4)
summary(lmList(CO2 ~ Q | ID, data=active))
summary(lmList(CO2_flux ~ Q | ID, data=active))


labels_vec_active <- setNames(
  paste0(active$ID, "\n", active$wetland_perc),
  active$ID_wetperc)

b<-ggplot(active%>% filter(!is.na(ID)),
       aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),
           y = CO2_flux)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = labels_vec_active)+
  ylab(expression(CO[2]~g/m^2/day))+
  common_theme

c<-ggplot(active%>% filter(!is.na(ID)),
       aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),
           y = CO2)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = labels_vec_active)+
  ylab(expression(CO[2]~'ppm'))+
  common_theme

labels_vec_sans_wet.cover <- setNames(
  paste0(active$ID),active$ID_wetperc)


d<-ggplot(active%>% filter(!is.na(ID)),
          aes(x = reorder(ID_wetperc, Q),
              y = Q)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = labels_vec_sans_wet.cover)+
  ylab(expression('Discharge'~L~s^--1))+
  scale_y_log10()+
  common_theme

e<-ggplot(active%>% filter(!is.na(ID)),
          aes(x = reorder(ID_wetperc, Q),
              y = K600)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = labels_vec_sans_wet.cover)+
  ylab(expression('K600'~day^--1))+
  scale_y_log10()+
  common_theme
plot_grid(b,c,d, ncol=1)

ggsave(filename = "05_Figures/MetRegime.jpeg",
       plot = a,
       width = 8, height = 6, units = "in")

ggsave(filename = "05_Figures/CO2Regime.jpeg",
       plot = plot_grid(b,c,d,e, ncol=1),
       width = 8, height = 12, units = "in")

#External v Internal Violin Plots Violin plots###########
active<-active%>%
  mutate(ID_wetperc = paste(ID, wetland_perc, sep = "_"))%>%
  mutate(ID_wetprox = paste(ID, wetland_perc, sep = "_"))%>%
  filter(!ID %in% c('14', '6a'))%>%filter(!is.na(ID))

active$wetland_perc <- factor(active$wetland_perc,
                                levels = sort(unique(active$wetland_perc), increasing = TRUE))

labels_vec_wetperc <- setNames(
  paste0(active$ID, "\n", active$wetland_perc),
  active$ID_wetperc)

common_layers <- list(geom_violin(size=1),
                      geom_jitter(shape=1),
                          scale_y_log10(),
                          ggtitle("Internal:External Sources Among Sites"),
                          geom_hline(yintercept = 1, color='red', size=1),
                          ylab("Internal/External"),
                          theme(axis.title.x = element_blank(),
                                axis.title.y= element_text(size=21, angle=90),
                                plot.title = element_text(size = 21))
                          )


a<-
  ggplot(active,
       aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),
           y = active.passive)) +common_layers+
  scale_x_discrete(labels = labels_vec_wetperc)

summary(lm(active.passive ~ wetland_perc, data = active))


active%>% group_by(ID,wetland_perc)%>%
  summarize(avg=mean(active.passive, na.rm=T),
            avg_active=mean(active, na.rm=T),
            avg_passive=mean(passive, na.rm=T))%>%
  arrange(wetland_perc)


int<-active%>%
  select(ID, active, ID_wetperc, wetland_perc)%>%
  rename(rate=active)%>%mutate(type='internal')
ext<-active%>%
  select(ID, passive, ID_wetperc, wetland_perc)%>%
  rename(rate=passive)%>%mutate(type='external')
active.hist<-rbind(int, ext)

labels_vec_wetperc_hist <- setNames(
  paste0(active.hist$ID, "\n", active.hist$wetland_perc),
  active.hist$ID_wetperc)

summary(lm(passive ~ wetland_perc, data = active))
summary(lmList(active ~ wetland_perc | ID, data=active))

#b<-
   ggplot(active.hist%>% filter(!is.na(ID))%>%filter(type=='external'),
         aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),  y = rate, fill = type)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c('black', 'red'), name= "") +
    ylab(expression(CO[2]~g/m^2/day))+
    scale_x_discrete(labels = labels_vec_wetperc_hist)+
    common_theme+
    theme(legend.position = 'bottom')

plot_grid(a,b, ncol=1)

ggsave(filename = "05_Figures/External.Interval.violin.plots.jpeg",
       plot = plot_grid(a,b, ncol=1),
       width = 8, height = 8, units = "in")


active%>%
  group_by(ID)%>%
  summarize(act_dom=sum(active.passive >1 , na.rm = TRUE),
            pass_dom=sum(active.passive <1 , na.rm = TRUE),
            tot=sum(active.passive >0 , na.rm = TRUE),
            act_perc_days=act_dom/tot*100,
            pass_perc_days=pass_dom/tot*100,
            mean=mean(active.passive, na.rm=T),
            act_perc=mean(active/CO2_flux, na.rm=T))


#slopes##########
active$ID <- with(active, reorder(ID, -as.numeric(as.character(wetland_perc))))

int<-active%>%
  select(ID, Q, Date, Q_med, active)%>%
  rename(rate=active)%>%mutate(type='internal')

ext<-active%>%
  select(ID, Q, Date, Q_med, passive)%>%
  rename(rate=passive)%>%mutate(type='external')%>%
  mutate(Q_med="inf")
unique(ext$Q_med)

active.hist<-rbind(int, ext)

col<-c("internal" ='red', "external"='black')

(a_split<-ggplot(
  active.hist,
  aes(x = Q, y = rate, color = type, group = interaction(type, Q_med))) +
  geom_point(shape = 21) +
  stat_poly_line(formula = y ~ x, se = FALSE) +
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  scale_colour_manual(name="", values = col,labels=c("External", "Internal"))+
  common_theme +
  ggtitle(expression(CO[2]~Flux-Q~Relationship)) +
  scale_x_log10() + scale_y_log10() +
  theme(strip.text = element_text(size = 15)) +
  xlab("Discharge L/s") +
  theme(axis.title.x = element_text(size = 21)))

#Q not split
a<-ggplot(
  active.hist,
  aes(x = Q, y = rate, color = type, group = type)) +
  geom_point(shape = 21) +
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(
      x = log10(Q),
      y = log10(rate),
      color = type,
      group = type,
      label = paste(..p.value.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4,
    label.x.npc = "right",
    label.y.npc = 0.017,
    vstep=0.07) +
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  scale_colour_manual(name="", values = col,labels=c("External", "Internal"))+
  common_theme +
  ggtitle(expression(CO[2]~Flux-Q~Relationship)) +
  scale_x_log10() + scale_y_log10() +
  theme(strip.text = element_text(size = 15)) +
  xlab("Discharge L/s") +
  theme(axis.title.x = element_text(size = 21))


slopes$ID <- factor(slopes$ID, levels = c('9','7','13','3','6','5','5a','15'))

rel_external<-slopes_not.split%>% select(ID, passive_slope, passive_pvalue)%>%
  mutate(type='external')%>% rename(slope=passive_slope, pvalue=passive_pvalue)%>%
  mutate(Q_med="inf")

#not split
rel_internal_not.split<-slopes_not.split%>% select(ID, active_slope, active_pvalue)%>%
  mutate(type='internal')%>% rename(slope=active_slope, pvalue=active_pvalue)%>%
  mutate(Q_med="inf")
slopes.hist_not.split<-rbind(rel_internal_not.split, rel_external)

#split
rel_internal_split<-slopes%>% select(ID, Q_med, active_slope, active_pvalue)%>%
  mutate(type='internal')%>% rename(slope=active_slope, pvalue=active_pvalue)
slopes.hist_split<-rbind(rel_internal, rel_external)


#split Q
b_split<-ggplot(slopes.hist_split, aes(x = ID, y=slope,color= type, shape=Q_med)) +
  geom_point(size=4, stroke=1.5) +
  scale_color_manual(values = c('black','red'),
                     labels=c("External Pathway", "Internal Pathway")) +
  scale_shape_manual(values = c(16,1),
                     name = expression(Q[50]),              # <-- Set legend title
                     labels = c("< Median Q", "> Median Q"))+  # Pick as many shapes as needed
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  common_theme +
  ggtitle("Log-Log Relationships of Active vs Passive")


b<-ggplot(slopes.hist_not.split, aes(x = ID, y=slope,color= type)) +
  geom_point(size=4) +
  scale_color_manual(values = c('red', 'black'),
                     labels=c("External Pathway", "Internal Pathway")) +
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  common_theme +
  ggtitle("Log-Log Relationships of Active vs Passive")


combine <- plot_grid(a, b, rel_heights = c(3, 1.5), ncol = 1)
combine_split <- plot_grid(a_split, b_split, rel_heights = c(3, 1.5), ncol = 1)

ggsave(filename = "05_Figures/External.v.Passive_Q.jpeg",
       plot = combine,
       width = 13, height = 12, units = "in")

ggsave(filename = "05_Figures/External.v.Passive_Q.split.jpeg",
       plot = combine_split,
       width = 12, height = 12, units = "in")
