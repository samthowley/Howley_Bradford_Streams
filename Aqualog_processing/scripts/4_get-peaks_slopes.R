# Create Index and Slope Files After Clean-Up -----------------------------------

## Use this after you have cleaned up all of your DOM files
## You'll also need to make sure you're using the correct values for the FI function. The main StaRdom package is using values for uncorrected data and ours are corrected (Cory and McKnight, 2005)

librarian::shelf(
  stringr,
  tidyr,
  plyr,
  dplyr,
  # Cairo,
  staRdom,
  reshape,
  gadenbuie/regexplain
)


# Read in FI Function Update ------------------------------------------------------

source("./scripts/functions/update_fi.R")

source("./scripts/functions/abs_scale.R")


# Peaks, Slopes, and Indexes, Oh My!  -------------------------------------

options(scipen = 999)

cores <- detectCores(logical = FALSE)


# load abs data -----------------------------------------------------------

absorbance_path <-
  "~/projects/Ch2_DOM/processed-data/abs-for-analysis"

absorbance <-
  absorbance_read(absorbance_path, recursive = T, cores = cores)

# clean up absorbance -----------------------------------------------------

absorbance <-
  abs_blcor(absorbance, wlrange = c(680, 700))

# ab_corr <-
#   absorbance %>%
#   pivot_longer(
#     cols = !wavelength,
#     names_to = "sample",
#     values_to = "abs"
#   ) %>%
#   filter(wavelength > 236 & wavelength < 750)

# calculate absorbance indexes --------------------------------------------

slope_parms <-
  abs_parms(absorbance,
            cuvle = 1,
            unit = "absorbance",
            add_as = c(250, 272, 340, 350, 365, 400, 405),
            limits = list(c(325, 375), c(275, 295), c(350, 400), c(300, 700)),
            l_ref = list(325, 275, 350, 300),
            cores = cores
  )

slope_parms <- slope_parms %>%
  mutate(E3_E4 = a340 / a254) # has been used to predict DOC; range: 0.20 - 0.38 (Tipping et al., 2009)


# split sample names ------------------------------------------------------

slope_parms$sampleid <-
  str_extract(slope_parms$sample, "([B]\\d+S\\d{1,2})")

slope_parms$site <-
  str_extract(slope_parms$sample, "(Blank1|Blank2|Blank3|Blank4|Blank5|HAT|HOGDN|HOGUP|POS|SWBUP|SWB|TUM)")

slope_parms$date <-
  str_extract(slope_parms$sample, "(?<=_)(\\d{4})[[:alpha:]]\\w+\\d{2}")

slope_parms$dilution <-
  str_extract(slope_parms$sample, "(dil+\\d{1})")


# filter for relevant samples ---------------------------------------------

slope_parms <-
  slope_parms %>%
  filter(site == "HAT" | site == "HOGDN" | site == "HOGUP" | site == "POS" | site == "SWBUP" | site == "SWB" | site == "TUM")


# save absorbance metrics -------------------------------------------------

# readr::write_csv(slope_parms,
# file = "~/projects/Ch2_DOM/output/ch2_absorbance_slopes_20230711.csv")
#
# saveRDS(slope_parms,
#         file = "~/projects/Ch2_DOM/output/ch2_absorbance_slopes_20230711.rds")

# EEM Peaks ---------------------------------------------------------------

# Read in corrected EEMs data

eem_list_done <-
  readRDS("./projects/20230915_Goeckner-PGSdata/processed-data/urban-natural_lists-pf1-pf2.rds")


eem_index <-
  cbind.data.frame(
    eem_biological_index(eem_list_done),
    eem_coble_peaks(eem_list_done),
    eem_fluorescence_index(eem_list_done), # old method
    eem_fluor_index(eem_list_done), # new method
    eem_humification_index(eem_list_done, scale = FALSE))

# fix eem sample names ---------------------------------------------

eem_index$sampleid <-
  str_extract(eem_index$sample, "([B]\\d+S\\d{1,2})")

eem_index$site <-
  str_extract(eem_index$sample, "(Blank1|Blank2|Blank3|Blank4|Blank5|HAT|HOGDN|HOGUP|POS|SWBUP|SWB|TUM)")

eem_index$date <-
  str_extract(eem_index$sample, "(?<=_)(\\d{4})[[:alpha:]]\\w+\\d{2}")

eem_index$dilution <-
  str_extract(eem_index$sample, "(dil+\\d{1})")

eem_index <-
  eem_index %>%
  select(c(15:18, 1:2, 4:8, 10, 12, 14)) %>%
  mutate(peak_tc = t/c)

# save eem metrics -------------------------------------------------

write.csv(eem_index,file ="./projects/20230915_Goeckner-PGSdata/processed-data/eem_indices.csv" )
#
# saveRDS(eem_index,
#         file = paste0("~/projects/Ch2_DOM/output/ch2_eem_index_", Sys.Date(), ".rds" ))
