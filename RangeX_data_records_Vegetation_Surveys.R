
# RangeX Vegetation survey data exploration NOR, CHE, CHN, ZAF ------------

## Data used: RangeX_clean_MetadataPlot_CHE.csv,
##            RangeX_clean_MetadataPlot_CHN.csv
##            RangeX_clean_MetadataPlot_NOR.csv
##            RangeX_clean_MetadataPlot_ZAF.csv
##            RangeX_ZAR_clean_community_2022-2023.csv
##            RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv
##            RangeX_clean_VegSurveyGeneral_2021_2023_CHE.csv
##            RangeX_clean_VegSurveyGeneral_2022_2024_CHN.csv
## Date:      09.12.25
## Author:    Nadine Arzt
## Purpose:   Combine NOR, CHE, ZAF and CHN data and explore


# comments and questions -----------------------------------------------------------------
# CHE: what to do with "<0.5" "<1"?
#      how to handle 9999?
# delete rows with 0 for cover? before counting observations? 

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)

# import metadata plot ----------------------------------------------------
meta_p_che <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_CHE.csv")

meta_p_chn <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_CHN.csv")

meta_p_nor <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_NOR.csv")

meta_p_zaf <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_ZAF.csv")



# import vegetation data --------------------------------------------------
com_zaf <- read.csv("Data/VegSurveyGeneral/RangeX_ZAR_clean_community_2022-2023.csv")

com_nor <- read.csv("Data/VegSurveyGeneral/RangeX_clean_VegSurveyGeneral_21_22_23_NOR.csv")

com_che <- read.csv("Data/VegSurveyGeneral/RangeX_clean_VegSurveyGeneral_2021_2023_CHE.csv")

com_chn_24 <- read.csv("Data/VegSurveyGeneral/RangeX_clean_VegSurveyGeneral_2022_2024_CHN.csv")


# filter chn for until 23 -------------------------------------------------
# make correct date format
com_chn_24 <- com_chn_24 |>
  mutate(
    date_measurement = str_replace(date_measurement, "/", "-"),
    date_measurement = ymd(date_measurement)
  )

# filter until 2023
com_chn <- com_chn_24 |> 
  filter(date_measurement <= ymd("2023-12-31"))




# combine metadata with vege ----------------------------------------------
vege_che <- com_che |> 
  left_join(meta_p_che, by = "unique_plot_ID")

vege_nor<- com_nor |> 
  left_join(meta_p_nor, by = "unique_plot_ID")

vege_chn <- com_chn |> 
  left_join(meta_p_chn, by = "unique_plot_ID")


# chn exclude height ------------------------------------------------------
vege_chn <- vege_chn |> 
  select(-mean_height.cm.)


# zaf: filter species level -----------------------------------------------
vege_zaf <- com_zaf |> 
  filter(group %in% c("focal", "native"))



# zaf: fix structure of columns and date -------------------------------------------
vege_zaf_clean <- vege_zaf |>
  # rename to match vege_che
  rename(
    unique_plot_ID = unique_plot_id,
    block_ID = block_id,
    plot_ID_original = plot_id
  ) |>
  # add missing columns
  mutate(
    collector = NA_character_,
    only_focals = NA,
    block_ID_original = NA_character_
  ) |>
  # create date
  mutate(
    month_num = match(tolower(month), tolower(month.name)),
    date_measurement = make_date(year, month_num, 1)
  ) |>
  select(
    unique_plot_ID,
    date_measurement,
    species,
    cover,
    collector,
    only_focals,
    region,
    site,
    block_ID_original,
    plot_ID_original,
    treat_warming,
    treat_competition,
    added_focals,
    block_ID
  )



# chn: fix order of columns -----------------------------------------------
vege_chn <- vege_chn |> 
  select(
    unique_plot_ID,
    date_measurement,
    species,
    cover,
    collector,
    only_focals,
    region,
    site,
    block_ID_original,
    plot_ID_original,
    treat_warming,
    treat_competition,
    added_focals,
    block_ID
  )


# fix date  ---------------------------------------------------------------
vege_che <- vege_che |> mutate(date_measurement = as.Date(date_measurement))
vege_chn <- vege_chn |> mutate(date_measurement = as.Date(date_measurement))
vege_nor <- vege_nor |> mutate(date_measurement = as.Date(date_measurement))
vege_zaf_clean <- vege_zaf_clean |> mutate(date_measurement = as.Date(date_measurement))


# cover as character because of <1 values ---------------------------------
vege_che <- vege_che |> mutate(cover = as.character(cover))
vege_chn <- vege_chn |> mutate(cover = as.character(cover))
vege_nor <- vege_nor |> mutate(cover = as.character(cover))
vege_zaf_clean <- vege_zaf_clean |> mutate(cover = as.character(cover))


# more columns to character -----------------------------------------------
vege_che <- vege_che |> mutate(plot_ID_original = as.character(plot_ID_original))
vege_chn <- vege_chn |> mutate(plot_ID_original = as.character(plot_ID_original))
vege_nor <- vege_nor |> mutate(plot_ID_original = as.character(plot_ID_original))
vege_zaf_clean <- vege_zaf_clean |> mutate(plot_ID_original = as.character(plot_ID_original))

vege_che <- vege_che |> mutate(block_ID_original = as.character(block_ID_original))
vege_chn <- vege_chn |> mutate(block_ID_original = as.character(block_ID_original))
vege_nor <- vege_nor |> mutate(block_ID_original = as.character(block_ID_original))
vege_zaf_clean <- vege_zaf_clean |> mutate(block_ID_original = as.character(block_ID_original))


# create one dataset for all regions --------------------------------------
vege_all <- bind_rows(vege_che, vege_chn, vege_nor, vege_zaf_clean)



# delete cover 0 ----------------------------------------------------------
vege_all <- vege_all |>
  filter(cover != 0)










