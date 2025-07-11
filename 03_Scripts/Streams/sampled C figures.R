
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
  low = "blue", high = "red", name = discharge),
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

tern6<-ggtern(
  data = all_sampled_C %>% filter(ID=='6'),aes(DOC, DIC*10, POC*10, colour = Q)) +
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
  data = all_sampled_C %>% filter(!ID=='6'),aes(DOC, DIC*10, POC*10, colour = Q)) +
  common_layers+
  theme(
    tern.axis.title.T = element_blank(),
    tern.axis.title.L = element_blank(),
    tern.axis.title.R = element_blank(),
    legend.position = "none")

ggsave(filename = "05_Figures/tern6.jpeg",
       plot = tern6,
       width = 8, height = 6, units = "in")

ggsave(filename = "05_Figures/tern.jpeg",
       plot = tern,
       width = 12, height = 6, units = "in")


#DOC Violin plots#######


DOC<-all_sampled_C %>%mutate(
  Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                  ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                  ID=='9'~'9', ID=='13'~'13'))

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>%
  select(Basin_Name, PERCENTAGE) %>% rename(Basin=Basin_Name, wetland_perc=PERCENTAGE)%>%
  mutate(wetland_perc=round(wetland_perc, 2))

DOC_wet<-full_join(DOC, wetland_cover)%>%
  mutate(ID_wet=paste(ID, wetland_perc, sep="_"))%>%
  filter(!ID %in% c('6a', '14'))%>%
  filter(!is.na(ID))

DOC_wet$wetland_perc <- factor(
  DOC_wet$wetland_perc,
  levels = sort(unique(DOC_wet$wetland_perc), decreasing = TRUE))

labels_vec <- setNames(
  paste0(DOC_wet$ID, "\n", DOC_wet$wetland_perc),
  DOC_wet$ID_wet)


a<-ggplot(DOC_wet,
          aes(x = reorder(ID_wet, -as.numeric(wetland_perc)),
              y = DOC)) +
  geom_boxplot(size=1)+
  geom_jitter(color='blue')+
  theme(axis.title.x = element_blank(),
        axis.title.y= element_text(size=21),
        plot.title = element_text(size = 21))+
  scale_x_discrete(labels = labels_vec)+
  ylab("DOC mg/L")+
  ggtitle("DOC Concentrations Across Sites")


ggsave(filename = "05_Figures/DOC.across.sites.jpeg",
       plot = a,
       width = 8, height = 5, units = "in")
