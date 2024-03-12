
setwd("Z:/Bradford_Forest_Project/Howley_Bradford_Streams/Aqualog_processing")# import format function --------------------------------------------------

source("./scripts/functions/format_aqua_data.R")

#Have dilution excel prepped
#Create project with a raw, processed, eem and abs folder.
#Save raw data in raw folder in its own folder with run date.
#follow prompts
convert_aqualog()


# file location
eem_folder2 <-
  "~/projects/Bradford Streams/021324/processed/eem/"


# WHAT IS THIS????
# from <-
#   list.files(path = eem_folder2,
#              recursive = TRUE,
#              pattern = 'B1S1_SWBUP_2019Jun05',
#              ignore.case = TRUE)
#
#
# to <-
#   stringr::str_replace(list.files(path = eem_folder2,
#                                   recursive = TRUE,
#                                   pattern = 'B1S1_SWBUP_2019Jun05',
#                                   ignore.case = FALSE), pattern = 'B1S1_SWBUP_2019Jun05', 'B1S1_SWBUP_2019Jun18')
#
#
# file.rename(paste0(eem_folder2, from),
#             paste0(eem_folder2, to))
