
## RangeX YearlyDemographics CHE, CHN, NOR, ZAF

## Data used: RangeX_clean_YearlyDemographics_2021_2022_2023_CHN.csv,
##            RangeX_clean_YearlyDemographics_2021_2023_CHE.csv,
##            RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv,
##            RangeX_YearlyDemographic_ZAF.csv
## Date:      26.10.23
## Author:    Nadine Arzt
## Purpose:   combine traits data 2021 + 2022 + 2023 from all regions for data  
##            records


# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata from all regions --------------------
meta_chn <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_CHN.csv")
meta_che <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_CHE.csv")
meta_nor <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_NOR.csv")
meta_zaf <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_ZAF.csv")


# date meta_chn -----------------------------------------------------------
meta_chn <- meta_chn |> 
  mutate(date_planting= as.Date(date_planting, format = "%d.%m.%Y")) 


# fix zaf metadata --------------------------------------------------------
# rename zaf columns to match other metadata
names(meta_zaf)

meta_zaf <- meta_zaf |> 
  rename(block_ID_original = block_id_original,
         block_ID = block_id,
         position_ID_original = position_id_original,
         position_ID = position_id,
         unique_plot_ID = unique_plot_id,
         unique_plant_ID = unique_plant_id)

# add "added_focals" column
# all wf
meta_zaf <- meta_zaf |> 
  mutate(added_focals = "wf")

# add plot_ID_original column
# same as plot_ID
# BUT there is no plot id only unique_plot_ID
# so just empty column?
meta_zaf <- meta_zaf |> 
  mutate(plot_ID_original = NA)

# correct order
meta_zaf <- meta_zaf |> 
  select(date_planting, region, site, block_ID_original, plot_ID_original, 
           position_ID_original, species, functional_group, treat_warming, 
           treat_competition, added_focals, block_ID, position_ID, 
           ind_number, unique_plot_ID, unique_plant_ID)

# date
meta_zaf <- meta_zaf |> 
  mutate(date_planting= as.Date(date_planting, format = "%d.%m.%Y")) 

# import yearly demo data from all regions --------------------

# CHN ---------------------------------------------------------------------
yd_chn <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_CHN.csv")

# correct date format
yd_chn <- yd_chn |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))


# CHE ---------------------------------------------------------------------
yd_che <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2023_CHE.csv")

# height_nathan = height_total
# delete number_leafclusters
yd_che <- yd_che |> 
  rename(height_total = height_nathan) |> 
  select(-number_leafclusters)


# NOR ---------------------------------------------------------------------
yd_nor <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv")


# ZAF ---------------------------------------------------------------------
yd_zaf <- read.csv("Data/YearlyDemographics/RangeX_YearlyDemographic_ZAF.csv")

# rename zaf unique_plant_id
yd_zaf <- yd_zaf |> 
  rename(unique_plant_ID = unique_plant_id)

# correct date format
yd_zaf <- yd_zaf |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))



# combine meta data with yearly demo data per region ----------------------
yearlydemo_chn <- left_join(meta_chn, yd_chn, 
                            by = c("unique_plant_ID", "species", 
                                   "date_planting", "functional_group"))


# CHN.hi.ambi.bare.wf.03.27 in 2023 #VALUE in leaf_width
# change to numeric and NA
yearlydemo_chn <- yearlydemo_chn |> 
  mutate(
    # first, convert non-numeric values to NA
    leaf_width = as.numeric(leaf_width),
    # then set your specific problematic row to NA (optional)
    leaf_width = if_else(
      unique_plant_ID == "CHN.hi.ambi.bare.wf.03.27" &
        date_measurement == as.Date("2023-07-30"),
      NA_real_,
      leaf_width
    )
  )



yearlydemo_che <- left_join(meta_che, yd_che, 
                            by = c("unique_plant_ID", "species", 
                                   "date_planting", "functional_group"))

# correct date format
yearlydemo_che <- yearlydemo_che |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))


yearlydemo_nor <- left_join(meta_nor, yd_nor, 
                            by = c("unique_plant_ID", "species", 
                                   "date_planting", "functional_group"))

# correct date format
yearlydemo_nor <- yearlydemo_nor |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))


yearlydemo_zaf <- left_join(meta_zaf, yd_zaf, 
                            by = c("unique_plant_ID", "species", 
                                   "date_planting", "functional_group"))



# make characters for all plot_ID_original and position_ID_original -------------------
yearlydemo_chn <- yearlydemo_chn |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))

yearlydemo_che <- yearlydemo_che |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))

yearlydemo_nor <- yearlydemo_nor |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))

yearlydemo_zaf <- yearlydemo_zaf |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))


# make one joint dataset with all regions ---------------------------------

yearlydemo <- bind_rows(yearlydemo_chn, yearlydemo_che, yearlydemo_nor, yearlydemo_zaf)














