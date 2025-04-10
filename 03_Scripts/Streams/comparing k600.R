library(ggpubr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)

no_bins<- read_csv("04_Output/master_metabolism.csv")
binned<- read_csv("04_Output/metabolism_04092025.csv")
