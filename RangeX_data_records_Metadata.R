

## RangeX Metadata CHE, CHN, NOR, ZAF

## Data used: RangeX_clean_MetadataFocal_CHN.csv,
##            RangeX_clean_MetadataFocal_CHE.csv,
##            RangeX_clean_MetadataFocal_NOR.csv,
##            RangeX_clean_MetadataFocal_ZAF.csv
## Date:      10.11.25
## Author:    Nadine Arzt
## Purpose:   combine Metadata for all regions



# comments ----------------------------------------------------------------
# ZAF: functional group grass --> graminoid


# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
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



# make one joint metadataset ----------------------------------------------

metadata <- rbind(meta_chn, meta_che, meta_nor, meta_zaf)




# save joint dataset ------------------------------------------------------
write.csv(metadata, "Data/Metadata/RangeX_clean_MetadataFocal_all.csv", row.names = FALSE)

## read cleaned data
m <- read.csv( "Data/Metadata/RangeX_clean_MetadataFocal_all.csv")


