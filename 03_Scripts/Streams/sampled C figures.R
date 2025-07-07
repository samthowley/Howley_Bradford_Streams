
theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                axis.text.y = element_text(size = 17, angle=0),
                axis.title =element_text(size = 17, angle=0),
                plot.title = element_text(size = 17, angle=0),
                legend.key.size = unit(0.8, 'cm'),
                legend.text=element_text(size = 17),
                legend.title =element_text(size = 17),
                legend.position ="none",
                panel.background = element_rect(fill = 'white'),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


ggplot(totC%>%filter(ID != is.na(ID)), aes(x=Q))+
  geom_point(aes(y=DOC, color="DOC"),size=3, shape=1)+
  geom_point(aes(y=DIC, color= "DIC"), size=3)+
  geom_point(aes(y=POC, color="POC"), size=3)+
  scale_colour_manual(values = c("black", "#0000FF", "darkorange"))+
  scale_x_log10()+scale_y_log10()+
  xlab(expression('Discharge'~ft^3/s))+ylab('mg/L')+
  facet_wrap(~ ID, ncol=3, scales='free')+theme(legend.position = 'bottom')+ggtitle("Stream Carbon Species")


library(psycho)
all_sampled_C <- read_csv("04_Output/stream_sampledC.csv")%>%
  mutate(season=find_season(Date))%>% filter(!is.na(ID))%>%
  filter(!is.na(POC))

POC<-all_sampled_C %>% select(Date,ID,Q,depth, POC)%>% rename(Conc=POC)%>%mutate(Species= 'POC')
DIC<-all_sampled_C %>% select(Date,ID,Q,depth, DIC)%>% rename(Conc=DIC)%>%mutate(Species= 'DIC')
DOC<-all_sampled_C %>% select(Date,ID,Q,depth, DOC)%>% rename(Conc=DOC)%>%mutate(Species= 'DOC')

long_C<- rbind(POC, DIC, DOC)

order <- c("5", "5a", "15", "9", '13', '6', '6a', '3', '7')

library(ggtern)

discharge <- expression("Discharge"~m^3~s^-1)

common_layers <- list(  scale_color_gradient(
  low = "orange", high = "blue", name = discharge),
    geom_point(size = 5),
    theme_minimal_grid(),
    theme(
      # Axis tick labels
      tern.axis.text.T = element_text(size = 14),
      tern.axis.text.L = element_text(size = 14),
      tern.axis.text.R = element_text(size = 14),
      # Facet labels
      strip.text = element_text(size = 18, face = "bold"),
      # Legend
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.key.height = unit(0.7, "cm"),
      legend.key.width = unit(2, "cm"),
      # General
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      legend.position = "bottom"),
  facet_wrap(~ID),
  coord_tern(expand = TRUE)
)

tern5<-ggtern(
  data = all_sampled_C %>% filter(ID=='5'),aes(DOC, DIC*10, POC*10, colour = Q)) +
  common_layers+
  labs(
    x = "DOC
    (mg/L)",
    y = "DIC
(dg/L)",
    z = "POC
(dg/L)")+
  theme(
    tern.axis.title.T = element_text(size = 14),
    tern.axis.title.L = element_text(size = 14),
    tern.axis.title.R = element_text(size = 14))


tern<-ggtern(
  data = all_sampled_C %>% filter(!ID=='5'),aes(DOC, DIC*10, POC*10, colour = Q)) +
  common_layers+
  theme(
    tern.axis.title.T = element_blank(),
    tern.axis.title.L = element_blank(),
    tern.axis.title.R = element_blank(),
    legend.position = "none")

ggsave(filename = "05_Figures/tern5.jpeg",
       plot = tern5,
       width = 8, height = 6, units = "in")

ggsave(filename = "test.jpeg",
       plot = tern,
       width = 12, height = 6, units = "in")

