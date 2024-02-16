
# ***
#
#   ## Suggested Folder Organization
#   The files are organized as follows:
#
#   1. **scripts**
#   + *contains all r code for the project*
#   2. **figures**
#   + *all figures created*
#   3. **output**
#   + *any new data files (e.g., absorbance and fluorescence indexes, PARAFAC model, etc.) generated are stored here*
#   4. **processed-data**
#   + *cleaned data*
#   + *separated into abs and eems folders (this is done automatically by the "aqualog_data_cleanup" code)*
#   + *m-correction and x-correction files*
#   5. **raw-data**
#   + *raw data*
#
# ***

# packages used throughout project ----------------------------------------

#librarian (optional)
#install.packages("remotes")
#remotes::install_github("DesiQuintans/librarian")

#regular expressions
devtools::install_github("gadenbuie/regexplain")

#Rmarkdown
install.packages('rmarkdown')
install.packages('tinytex')
tinytex::install_tinytex()  # install TinyTeX

#staRdom
install.packages("staRdom")

#Other
install.packages("extrafont")
install.packages("remedy")
install.packages("dreamRs") #couldnt download this
install.packages("esquisse")
install.packages("questionr")

###########

#libraries
library(tidyverse)
library(staRdom)
library(regexplain) # you can use regular expression language to parse out the sample name components
#packages for Emily's functions
library(foreach)
library(MBA)
library(tidyverse)
library(doParallel)
library(readr)
library(extrafont)
#packages that are useful Add-Ins
library(remedy) # Remedey applies markdown formatting, esquisse lets you make ggplots interactively, questionr lets you refactor or rename variables interactively.
#library(dreamRs) #could not load this
library(questionr)
library(esquisse)

# There was a period of time I suggested using a forked version of the eemR package that had the corrected and uncorrected fluorescence index calculation but I just re-wrote the function and source it in the R script instead.

