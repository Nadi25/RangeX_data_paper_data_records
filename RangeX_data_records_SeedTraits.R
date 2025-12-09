
# RangeX SeedTrait data preparation NOR and CHE ------------

## Data used: RangeX_clean_MetadataFocal_all.csv
##            RangeX_clean_SeedTraits_2022_CHE.csv
##            RangeX_clean_seeds_2023_NOR.csv
## Date:      18.11.25
## Author:    Nadine Arzt
## Purpose:   Combine NOR and CHE data and prepare


# comment -----------------------------------------------------------------
# CHE.lo.ambi.bare.wf.08.19.1 has wrong date 2024-06-23


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")



# import seed data --------------------------------------------------------
# NOR
seed_che <- read.csv("Data/SeedTraits/RangeX_clean_SeedTraits_2022_CHE.csv")

seed_nor <- read.csv("Data/SeedTraits/RangeX_clean_seeds_2023_NOR.csv")


# merge metadata with seed data -------------------------------------------
# CHE
seed_trait_che <- seed_che |> 
  left_join(metadata, by = c("unique_plant_ID", "species"))

# NOR
seed_trait_nor <- seed_nor |> 
  left_join(metadata, by = c("unique_plant_ID", "species"))


# combine NOR and CHE -----------------------------------------------------
seed_traits <- rbind(seed_trait_che, seed_trait_nor)


glimpse(seed_traits)

# add column year ---------------------------------------------------------
seed_traits <- seed_traits |> 
  mutate(date_collection = as.Date(date_collection),
         year = format(date_collection, "%Y"))


# how many species --------------------------------------------------------
species_count <- unique(seed_traits$species)
species_count
# 20


# samples per region ------------------------------------------------------
count_region <- seed_traits |> 
  count(region)
count_region


# summary per region ---------------------------------------------
seed_traits_summary_region <- seed_traits |> 
  group_by(region) |> 
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )
seed_traits_summary_region




# summary per species ---------------------------------------------
seed_traits_summary_species <- seed_traits |> 
  group_by(species) |> 
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )
seed_traits_summary_species

















