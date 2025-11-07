





## RangeX Phenology CHE, NOR data exploration

## Data used: RangeX_data_records_YearlyDemographics.R
## Date:      04.11.25
## Author:    Nadine Arzt
## Purpose:   Explore YearlyDemographics trait data 
##            How many samples? per region? 



# import data -------------------------------------------------------------
source("RangeX_phenology_NOR_CHE_data_combination.R")
glimpse(phenology)
# 230 400 observations


# how many species --------------------------------------------------------
species_count <- unique(phenology$species)
species_count
# 20


# samples per region ------------------------------------------------------
count_region <- phenology |> 
  count(region)
count_region



# Count per stage -----------------------------------------
range_region_stage <- phenology %>%
  group_by(region, phenology_stage) %>%
  summarise(
    n = sum(!is.na(value)),
    min_value = if_else(all(is.na(value)), NA_real_, min(value, na.rm = TRUE)),
    max_value = if_else(all(is.na(value)), NA_real_, max(value, na.rm = TRUE)),
    .groups = "drop"
  )

range_region_stage





































