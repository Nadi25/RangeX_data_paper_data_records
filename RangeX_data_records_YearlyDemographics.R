
## RangeX YearlyDemographics CHE, CHN, NOR, ZAF

## Data used: RangeX_clean_YearlyDemographics_2021_2022_2023_CHN.csv,
##            RangeX_clean_YearlyDemographics_2021_2023_CHE.csv,
##            RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv,
##            RangeX_YearlyDemographic_ZAF.csv
## Date:      26.10.25
## Author:    Nadine Arzt
## Purpose:   combine traits data 2021 + 2022 + 2023 from all regions for data  
##            records


# problems ----------------------------------------------------------------
# ZAF: some NA in species already in yd_zaf in the 2025-02-01 data
# ZAF has data until 2025 --> we only want until 23 for now that is 24 in southern hemispheres

# ZAF: several unique_plant_IDs have different species e.g. ZAF.hi.ambi.bare.wf.01.02.2
# is cotplan, leuser, and NA

# ZAF.lo.ambi.bare.wf.08.11.1 has leaf_length1 = 6750 mm that must be wrong



# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata from all regions --------------------
meta_chn <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_CHN.csv")
meta_che <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_CHE.csv")
meta_nor <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_NOR.csv")
meta_zaf <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_ZAF.csv")


# change date format -----------------------------------------------------
meta_chn <- meta_chn |>  
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))

meta_che <- meta_che |> 
  mutate(date_planting = as.Date(date_planting, format = "%Y-%m-%d"))

meta_nor <- meta_nor |> 
  mutate(date_planting = as.Date(date_planting, format = "%Y-%m-%d"))

meta_zaf <- meta_zaf |> 
  mutate(date_planting = as.Date(date_planting, format = "%Y-%m-%d"))


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


# import yearly demo data from all regions --------------------
yd_chn <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_CHN.csv")

yd_che <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2023_CHE.csv")

yd_nor <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv")

yd_zaf <- read.csv("Data/YearlyDemographics/RangeX_YearlyDemographic_ZAF.csv")


# change date format -----------------------------------------------------
yd_chn <- yd_chn |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))

yd_che <- yd_che |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%Y-%m-%d")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%Y-%m-%d"))

yd_nor <- yd_nor |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%Y-%m-%d")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%Y-%m-%d"))

yd_zaf <- yd_zaf |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%Y-%m-%d")) |> 
  mutate(date_planting = as.Date(date_planting, format = "%Y-%m-%d"))


# fix yearly demo ---------------------------------------------------------

# CHE ---------------------------------------------------------------------
# height_nathan = height_total
# delete number_leafclusters
yd_che <- yd_che |> 
  rename(height_total = height_nathan) |> 
  select(-number_leafclusters)

# yd_che has some NAs in date_measurement, why?
# just delete then?


# ZAF ---------------------------------------------------------------------
# rename zaf unique_plant_id
yd_zaf <- yd_zaf |> 
  rename(unique_plant_ID = unique_plant_id)


# combine meta data with yearly demo data per region ----------------------
yearlydemo_chn <- yd_chn |> 
  left_join(meta_chn, by = c("unique_plant_ID", "species", 
                             "date_planting", "functional_group"))

yearlydemo_che <- yd_che |> 
  left_join(meta_che, by = c("unique_plant_ID", "species", 
                           "date_planting", "functional_group"))

yearlydemo_nor <- yd_nor |> 
  left_join(meta_nor, by = c("unique_plant_ID", "species", 
                             "date_planting", "functional_group"))

yearlydemo_zaf <- yd_zaf |> 
  left_join(meta_zaf, by = c("unique_plant_ID", "species", 
                             "date_planting", "functional_group"))



# fix yearlydemo data -----------------------------------------------------

# CHN ---------------------------------------------------------------------
# CHN.hi.ambi.bare.wf.03.27 in 2023 #VALUE in leaf_width
# change to numeric and NA
yearlydemo_chn <- yearlydemo_chn |> 
  mutate(
    # first, convert non-numeric values to NA
    leaf_width = as.numeric(leaf_width),
    # then set your specific problematic row to NA (optional)
    leaf_width = if_else(
      unique_plant_ID == "CHN.hi.ambi.bare.wf.03.27" &
        leaf_length1 == 57,
      NA_real_,
      leaf_width
    )
  )


# ZAF filter out 2021-24 ---------------------------------------------------------------------
yearlydemo_zaf <- yearlydemo_zaf |> 
  filter(date_measurement >= as.Date("2021-01-01") &
           date_measurement <= as.Date("2024-12-31"))



# ZAF quick fix for now ---------------------------------------------------
# due to the mismatches of metadata and yd
# just say region = ZAF
yearlydemo_zaf <- yearlydemo_zaf |> 
  mutate(region = "ZAF")



# convert to character for all plot_ID_original and position_ID_original -------
yearlydemo_chn <- yearlydemo_chn |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))

yearlydemo_che <- yearlydemo_che |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))

yearlydemo_nor <- yearlydemo_nor |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))

yearlydemo_zaf <- yearlydemo_zaf |> 
  mutate(across(c(plot_ID_original, position_ID_original), as.character))


# make one joint data set with all regions ---------------------------------
yearlydemo <- bind_rows(yearlydemo_chn, yearlydemo_che, yearlydemo_nor, yearlydemo_zaf)

glimpse(yearlydemo)
# 24,306 trait observations











