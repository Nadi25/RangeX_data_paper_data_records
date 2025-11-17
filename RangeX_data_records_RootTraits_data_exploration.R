

## RangeX Root CHE data exploration

## Data used: RangeX_clean_MetadataFocal_all.csv
##            RangeX_clean_RootTraits_2023_CHE.csv
## Date:      07.11.25
## Author:    Nadine Arzt
## Purpose:   Explore Root trait data 
##            How many samples? per region? 


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")

# filter CHE
meta_che <- metadata |> 
  filter(region == "CHE")



# import root data -------------------------------------------------------------
root_traits <- read.csv("Data/RootTraits/RangeX_clean_RootTraits_2023_CHE.csv")
glimpse(root_traits)
# 119 observations


# join metadata with roots ----------------------------------------------
roots_che <- root_traits |> 
  left_join(meta_che, by = c("unique_plant_ID", "species"))


# how many species --------------------------------------------------------
species_count <- unique(root_traits$species)
species_count
# 9

species_obs_count <- root_traits |> 
  count(species)
species_obs_count


# average, sd, min, max per species and trait -------------------------------------------------------
root_traits_summary <- root_traits |> 
  group_by(species) |> 
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
root_traits_summary

# average, sd, min, max per species, site and biomass ----------------------------
roots_species_site_summary <- roots_che |> 
  group_by(site, species) |> 
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
roots_species_site_summary


count <- roots_che |> 
  count(site, species)
count

# count per site ----------------------------------------------
count2 <- roots_che |> 
  count(site)
count2


# count per species ----------------------------------------------
count3 <- roots_che |> 
  count(species)
count3























