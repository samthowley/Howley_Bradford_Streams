#Include wetlands####
wetland_proxim <- read_csv("01_Raw_data/wetland_proxim.csv")%>%
  select(Site, NEAR_DIST)%>%rename(ID=Site, nearest_wetland=NEAR_DIST)%>%
  mutate(nearest_wetland=round(nearest_wetland, 2))
active<-full_join(active, wetland_proxim)

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>%
  select(Basin_Name, PERCENTAGE) %>% rename(Basin=Basin_Name, wetland_perc=PERCENTAGE)%>%
  mutate(wetland_perc=round(wetland_perc, 2))
active<-full_join(active, wetland_cover)

#metabolic regime######
met_hist.GPP<-active%>%select(Date, ID, GPP, wetland_perc, nearest_wetland)%>%
  rename(met=GPP)%>%mutate(type='GPP')
met_hist.ER<-active%>%select(Date, ID, ER_corrected, wetland_perc, nearest_wetland)%>%
  rename(met=ER_corrected)%>%mutate(type='ER', met=met*-1)
met_hist<-rbind(met_hist.GPP, met_hist.ER)%>%
  mutate(ID_q = paste(ID, wetland_perc, sep = "_"))%>% filter(!ID %in% c('14', '6a'))

met_hist$wetland_perc <- factor(met_hist$wetland_perc,
                                levels = sort(unique(met_hist$wetland_perc), decreasing = TRUE))


common_theme<-list( theme(
  axis.title.x = element_blank(),
  axis.title.y = element_text(size=21),
  plot.title = element_text(size = 21),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  legend.key.height = unit(0.7, "cm"),
  legend.key.width = unit(1, "cm")))


labels_vec <- setNames(
  paste0(met_hist$ID, "\n", met_hist$wetland_perc),
  met_hist$ID_q)

a<-ggplot(met_hist%>% filter(!is.na(ID)),
       aes(x = reorder(ID_q, -as.numeric(wetland_perc)),  y = met, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c('brown','darkgreen'), name= "") +
  ggtitle("Metabolic Regime") +
  ylab(expression(O[2]~'g' / m^2 / 'day')) +
  common_theme+
  scale_x_discrete(labels = labels_vec)


ggsave(filename = "05_Figures/MetRegime.jpeg",
       plot = a,
       width = 8, height = 6, units = "in")


#Violin plots###########
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
                                axis.title.y= element_text(size=21),
                                plot.title = element_text(size = 21))
                          )


a<-ggplot(active,
       aes(x = reorder(ID_wetperc, -as.numeric(wetland_perc)),
           y = active.passive)) +common_layers+
  scale_x_discrete(labels = labels_vec_wetperc)

active$nearest_wetland <- factor(active$nearest_wetland,
                              levels = sort(unique(active$nearest_wetland), decreasing = TRUE))

labels_vec_wetprox <- setNames(
  paste0(active$ID, "\n", active$nearest_wetland),
  active$ID_wetprox)

b<-ggplot(active,
       aes(x = reorder(ID_wetprox, -as.numeric(nearest_wetland)),
           y = active.passive)) +common_layers+
  scale_x_discrete(labels = labels_vec_wetprox)


ggsave(filename = "test.jpeg",
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

a<-ggplot(active, aes(x = Q)) +
  geom_point(aes(y = active, color ="Active Pathway")) +
  geom_point(aes(y = passive, color ="Passive Pathway"), shape = 21) +
  geom_smooth(aes(y = active, color ="Active Pathway"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = passive, color ="Passive Pathway"), method = "lm", se = FALSE) +
  stat_poly_eq(
    aes(x = log10(Q), y = log10(active), label = paste(..p.value.label.., sep = "~~~"), color = "Active Pathway"),
    formula = y ~ x, parse = TRUE, size = 4, label.x.npc = "right", label.y.npc = 0.017
  ) +
  stat_poly_eq(
    aes(x = log10(Q), y = log10(passive), label = paste(..p.value.label.., sep = "~~~"), color = "Passive Pathway"),
    formula = y ~ x, parse = TRUE, size = 4, label.x.npc = "right", label.y.npc = 0.1
  ) +
  scale_color_manual(values = c('red', 'black'), name= "") +
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  common_theme+
  xlab(expression(Discharge~L/sec)) +
  ggtitle(expression(CO[2]~Flux-Q~Relationship))+
  scale_x_log10()+scale_y_log10()+
  theme(strip.text = element_text(size = 15))

slopes$ID <- factor(slopes$ID, levels = c('9','7','13','3','6','5','5a','15'))

b<-ggplot(slopes, aes(x = as.factor(ID))) +
  geom_point(aes(y = active_slope, color = "Active Slope"), size=4) +
  geom_point(aes(y = passive_slope, color = "Passive Slope"), size=4) +
  scale_color_manual(values = c('red', 'black')) +
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  common_theme +
  ggtitle("Log-Log Relationships of Active vs Passive")

combine <- plot_grid(a, b, rel_heights = c(3, 1), ncol = 1)
ggsave(filename = "05_Figures/External.v.Passive_Q.jpeg",
       plot = combine,
       width = 13, height = 12, units = "in")

