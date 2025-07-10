
active$ID <- factor(active$ID , levels=c('15','5','5a','3','6','13','7','9','6a'))


met_hist.GPP<-active%>%select(Date, ID, GPP)%>% rename(met=GPP)%>%mutate(type='GPP')
met_hist.ER<-active%>%select(Date, ID, ER_corrected)%>% rename(met=ER_corrected)%>%mutate(type='ER', met=met*-1)
met_hist<-rbind(met_hist.GPP, met_hist.ER)

ggplot(met_hist, aes(x = as.factor(ID), y = met, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c('brown','darkgreen')) +
  ggtitle("Metabolic Regime")+
  ylab(expression(O[2]~'g' / m^2 / 'day'))+
  theme(axis.title.x = element_blank(),
        axis.title.y= element_text(size=21),
        plot.title = element_text(size = 21))

active.only<-active%>% select(active, Q,ID, Date) %>% rename(C=active)%>%
  mutate(type="Active Pathway")
passive.only<-active%>% select(passive, Q,ID, Date) %>% rename(C=passive)%>%
  mutate(type="Passive Pathway")

active.hist<-rbind(active.only, passive.only)

active$ID <- factor(active$ID , levels=c('9','15','3','7','5','5a','6','13','6a'))

ggplot(active, aes(x = as.factor(ID), y = active.passive)) +
  geom_violin(size=1) +geom_jitter(shape=1)+
  scale_y_log10()+
  ggtitle("Active:Passive Sources Among Sites")+
  geom_hline(yintercept = 1, color='red', size=1)+
  ylab("Active/Passive")+
  theme(axis.title.x = element_blank(),
        axis.title.y= element_text(size=21),
        plot.title = element_text(size = 21))

active%>%
  group_by(ID)%>%
  summarize(act_dom=sum(active.passive >1 , na.rm = TRUE),
            pass_dom=sum(active.passive <1 , na.rm = TRUE),
            tot=sum(active.passive >0 , na.rm = TRUE),
            act_perc_days=act_dom/tot*100,
            pass_perc_days=pass_dom/tot*100,
            mean=mean(active.passive, na.rm=T),
            act_perc=mean(active/CO2_flux, na.rm=T))


ggplot(active, aes(x = Q)) +
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
  scale_color_manual(values = c('red', 'black')) +
  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 3, scales = 'free') +
  theme(legend.position = "bottom") +
  xlab(expression(Discharge~L/sec)) +
  ggtitle(expression(CO[2]~Flux-Q~Relationship))+
  scale_x_log10()+scale_y_log10()


ggplot(slopes, aes(x = ID)) +
  geom_point(aes(y = active_slope, color = "Active Slope"), size=4) +
  geom_point(aes(y = passive_slope, color = "Passive Slope"), size=4) +
  scale_color_manual(values = c('red', 'black')) +
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  theme(legend.position = "bottom") +
  ggtitle("Log-Log Relationships of Active vs Passive")

