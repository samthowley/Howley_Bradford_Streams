
setwd("/Users/audreygoeckner/OneDrive - University of Florida/Aqualog_processing")

# import format function --------------------------------------------------

source("./scripts/functions/format_aqua_data.R")

# input variables ---------------------------------------------------------

## folder where corrected data will be stored ##
# "~/projects/20230915_Goeckner-PGSdata/processed-data"

## folder where original aqualog data are stored ##
## each folder(i.e., analysis date) is converted one at a time ##
# "~/projects/20230915_Goeckner-PGSdata/raw-data"

# convert aqualog files ---------------------------------------------------
#dir.create(paste(destination_folder,"/abs",sep=""))
#dir.create(paste(destination_folder,"/eem",sep=""))

convert_aqualog()

# create metatable with dilution info --------------------------------------------------------

# eem_metatemplate(eem_list, absorbance) %>%
 #  write.csv(file = "../projects/20230915_Goeckner-PGSdata/metatable_dilution_PRI.csv", row.names = FALSE)

# re-name files -----------------------------------------------------------

# re-name any mispelled site names or fix dates for formatting

# !!! MAKE CHANGES IN EEM AND ABS FILES !!! #

# file location
eem_folder2 <-
  "~/projects/Ch2_DOM/processed-data/eem/"


# Examples of changes:
# 20220118 --> _2022Jan18 # fix dates
# SWBDN --> SWB # fix sample names
# POSNW16 --> POS
# TUM441 --> TUM

from <-
  list.files(path = eem_folder2,
             recursive = TRUE,
             pattern = 'B1S1_SWBUP_2019Jun05',
             ignore.case = TRUE)


to <-
  stringr::str_replace(list.files(path = eem_folder2,
                                  recursive = TRUE,
                                  pattern = 'B1S1_SWBUP_2019Jun05',
                                  ignore.case = FALSE), pattern = 'B1S1_SWBUP_2019Jun05', 'B1S1_SWBUP_2019Jun18')


file.rename(paste0(eem_folder2, from),
            paste0(eem_folder2, to))
