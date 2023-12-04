#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)
library(ggExtra)
library(lubridate)

theme_sam<-theme_minimal()+theme(axis.text.x = element_text(size = 8, angle=0),
                                 axis.text.y = element_text(size = 15, angle=0),
                                 axis.title.y =element_text(size = 15),
                                 axis.title.x =element_blank(),
                                 axis.title.y.right = element_text(),
                                 plot.title = element_text(size = 15, angle=0),
                                 legend.text=element_text(size=12),
                                 legend.title=element_text(size=12),
                                 legend.key.size = unit(0.5, "cm"),
                                 legend.position = 'none',
                                 panel.background = element_rect(fill = 'white'),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())
DO<-'DO mg/L'
CO2<-expression(CO[2]~ppm)
#####5######
S5 <- read_excel("02_Clean_data/5.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "text"))
(a<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S5, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)

##5a#####
S5a <- read_excel("02_Clean_data/5a.xlsx",
                  col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "text"))
(a<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S5a, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)

###6####

S6 <- read_excel("02_Clean_data/6.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "text"))
(a<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S6, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)

###6a####
S6a <- read_excel("02_Clean_data/6a.xlsx",
                  col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "text"))
(a<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S6a, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)

###7####
S7 <- read_excel("02_Clean_data/7.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "text"))
(a<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S7, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)
###9####
S9 <- read_excel("02_Clean_data/9.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "text"))
(a<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S9, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)

##13######
S13 <- read_excel("02_Clean_data/13.xlsx",
                  col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "text"))
(a<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S13, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)
###14#####
S14 <- read_excel("02_Clean_data/14.xlsx",
                  col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "text"))
(a<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S14, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)
###15########
S15 <- read_excel("02_Clean_data/15.xlsx",
                  col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "text"))
(a<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(CO2)+
    scale_color_manual(values='orange')+theme_sam+
    guides(color=guide_legend(title="")))

(b<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    ylab(DO)+
    scale_color_manual(values='blue')+theme_sam+
    guides(color=guide_legend(title="")))

(c<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=SpC, color="SpC"), size=0.8)+
    scale_color_manual(values='red')+
    ylab('Conductivity')+theme_sam+
    guides(color=guide_legend(title="")))

(d<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=0.8)+
    scale_color_manual(values='purple')+theme_sam+
    guides(color=guide_legend(title="")))

(e<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=pH, color="pH"),  size=0.8)+
    scale_color_manual(values='pink')+theme_sam+
    guides(color=guide_legend(title="")))

(g<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=Stage),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

(h<-ggplot(S15, aes(x=Date))+
    geom_line(aes(y=Q),  size=0.8)+
    scale_color_manual(values='black')+theme_sam+
    guides(color=guide_legend(title="")))

plot_grid(a,b,c,d,e,g,h, ncol=1)

