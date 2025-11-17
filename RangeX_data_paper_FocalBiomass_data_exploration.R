
## RangeX FocalBiomass CHE

## Data used: RangeX_clean_MetadataFocal_all.csv,
##            RangeX_clean_FocalBiomass_2023_CHE.csv
## Date:      10.11.25
## Author:    Nadine Arzt
## Purpose:   Explore FocalBiomass data CHE


# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")

# filter CHE
meta_che <- metadata |> 
  filter(region == "CHE")



# import biomass data -----------------------------------------------------
biomass <- read.csv("Data/FocalBiomass/RangeX_clean_FocalBiomass_2023_CHE.csv")


# join metadata with biomass ----------------------------------------------
biomass_che <- biomass |> 
  left_join(meta_che, by = c("unique_plant_ID", "species"))


# average, sd, min, max per site and biomass -------------------------------------------------------
biomass_site_summary <- biomass_che |> 
  group_by(site) |> 
  summarise(
    dry_mass_mean = mean(dry_mass, na.rm = TRUE),
    dry_mass_sd   = sd(dry_mass, na.rm = TRUE),
    dry_mass_min  = min(dry_mass, na.rm = TRUE),
    dry_mass_max  = max(dry_mass, na.rm = TRUE)
  )
biomass_site_summary


# average, sd, min, max per species and biomass -------------------------------------------------------
biomass_summary <- biomass_che |> 
  group_by(species) |> 
  summarise(
    dry_mass_mean = mean(dry_mass, na.rm = TRUE),
    dry_mass_sd   = sd(dry_mass, na.rm = TRUE),
    dry_mass_min  = min(dry_mass, na.rm = TRUE),
    dry_mass_max  = max(dry_mass, na.rm = TRUE)
  )
biomass_summary

# average, sd, min, max per species, site and biomass ----------------------------
biomass_species_site_summary <- biomass_che |> 
  group_by(site, species) |> 
  summarise(
    dry_mass_mean = mean(dry_mass, na.rm = TRUE),
    dry_mass_sd   = sd(dry_mass, na.rm = TRUE),
    dry_mass_min  = min(dry_mass, na.rm = TRUE),
    dry_mass_max  = max(dry_mass, na.rm = TRUE)
  )
biomass_species_site_summary


# count per site per species ----------------------------------------------
count <- biomass_che |> 
  count(site, species)
count

# count per site ----------------------------------------------
count2 <- biomass_che |> 
  count(site)
count2


# count per species ----------------------------------------------
count3 <- biomass_che |> 
  count(species)
count3



# mean and sd per year, site and warming treatment -------------------------------------
biomass_summary2 <- biomass_che |> 
  mutate(year_from_time = lubridate::year(date_collection)) |> 
  group_by(year_from_time, site, treat_warming) |> 
  summarise(
    n = n(),
    across(
      c(dry_mass),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
biomass_summary2



# mean and sd per year, site and comp treatment -------------------------------------
biomass_summary3 <- biomass_che |> 
  mutate(year_from_time = lubridate::year(date_collection)) |> 
  group_by(year_from_time, site, treat_competition) |> 
  summarise(
    n = n(),
    across(
      c(dry_mass),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
biomass_summary3



# mean and sd per year, site and warm and comp treatment ----------------------
biomass_summary4 <- biomass_che |> 
  mutate(year_from_time = lubridate::year(date_collection)) |> 
  group_by(year_from_time, site, treat_competition, treat_warming) |> 
  summarise(
    n = n(),
    across(
      c(dry_mass),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
biomass_summary4























