
# RangeX Vegetation survey data exploration NOR, CHE, CHN, ZAF ------------

## Data used: RangeX_data_records_Vegetation_Surveys.R
## Date:      10.12.25
## Author:    Nadine Arzt
## Purpose:   Explore NOR, CHE, ZAF and CHN data 

source("RangeX_data_records_Vegetation_Surveys.R")
vege_all


# how many species  -------------------------------------------------------
unique(vege_all$species)

n_distinct(vege_all$species)
# 379

species_region <- vege_all |>
  group_by(region) |>
  summarise(n_species = n_distinct(species)) |>
  arrange(desc(n_species))
species_region

# how often are the species recorded --------------------------------------
n_species <- vege_all |>
  count(species)
n_species

n_species_site <- vege_all |>
  count(species, site)
n_species_site

n_region_species <- vege_all |>
  count(region, species)
n_region_species

n_region <- vege_all |>
  count(region)
n_region










