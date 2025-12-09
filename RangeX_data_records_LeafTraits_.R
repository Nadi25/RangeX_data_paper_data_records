
# RangeX LeafTraits data preparation NOR and CHE ------------

## Data used: 
##            
##            
## Date:      17.11.25
## Author:    Nadine Arzt
## Purpose:   Combine NOR and CHE data and prepare


# comment -----------------------------------------------------------------

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library()


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")


# import LeafTrait data ---------------------------------------------------
leaftrait_che <- read.csv("Data/LeafTraits/RangeX_clean_LeafTraits_2022_2023_CHE.csv")

leaftrait_nor <- read.csv("Data/LeafTraits/RangeX_clean_LeafTraits_2023_NOR.csv")

leaftrait_chn <- read.csv("Data/LeafTraits/RangeX_clean_Leaftraits_2021_2022_2023_CHN.csv")

# merge metadata with LeafTrait -------------------------------------------
# CHE
leaf_trait_che <- leaftrait_che |> 
  left_join(metadata, by = c("unique_plant_ID", "species"))

# NOR
leaf_trait_nor <- leaftrait_nor |> 
  left_join(metadata, by = c("unique_plant_ID", "species"))

#CHN
leaf_trait_chn <- leaftrait_chn |> 
  left_join(metadata, by = c("unique_plant_ID", "species", "site"))


# filter 2023 only for CHN ------------------------------------------------
# 2021 and 22 are a combined value from several leaves per species
# that can be an extra dataset
leaf_trait_chn_23 <- leaf_trait_chn |> 
  filter(year == "2023")

leaf_trait_chn_21_22 <- leaf_trait_chn |> 
  filter(year == c("2021", "2022"))


# combine NOR and CHE -----------------------------------------------------
leaf_traits <- rbind(leaf_trait_che, leaf_trait_nor)


# plot to compare dry mass and wet mass per region ------------------------
ggplot(leaf_traits, aes(x = wet_mass, y = dry_mass, colour = region)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
# norwegian leaves grew in wetter environment


glimpse(leaf_traits)

# add column year ---------------------------------------------------------
leaf_traits <- leaf_traits |> 
  mutate(date_collection = as.Date(date_collection),
         year = format(date_collection, "%Y"))


# how many species --------------------------------------------------------
species_count <- unique(leaf_traits$species)
species_count
# 20


# samples per region ------------------------------------------------------
count_region <- leaf_traits |> 
  count(region)
count_region

# samples per region, per year ------------------------------------------------------
count_region <- leaf_traits |> 
  count(region, year)
count_region


# summary per region and year ---------------------------------------------
leaf_traits_summary_region_year <- leaf_traits |> 
  group_by(region, year) |> 
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )
leaf_traits_summary_region_year


# summary per region ---------------------------------------------
leaf_traits_summary_region <- leaf_traits |> 
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
leaf_traits_summary_region




# summary per species ---------------------------------------------
leaf_traits_summary_species <- leaf_traits |> 
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
leaf_traits_summary_species















