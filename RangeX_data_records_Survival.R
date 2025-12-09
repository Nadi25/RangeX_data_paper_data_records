
# RangeX Survival data exploration NOR, CHE, CHN ------------

## Data used: RangeX_clean_MetadataFocal_all.csv
##            RangeX_clean_Survival_2021_2023_CHE.csv
##            RangeX_clean_Survival_2021_2022_2023_CHN.csv
##            RangeX_clean_Survival_2021_2022_2023_NOR.csv
## Date:      20.11.25
## Author:    Nadine Arzt
## Purpose:   Combine NOR, CHE and CHN data and explore


# comment -----------------------------------------------------------------


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")

# date format
metadata <- metadata |> 
  mutate(date_planting = as.Date(date_planting))

# import survival data --------------------------------------------------------
# NOR
che <- read.csv("Data/Survival/RangeX_clean_Survival_2021_2023_CHE.csv")

chn <- read.csv("Data/Survival/RangeX_clean_Survival_2021_2022_2023_CHN.csv")

nor <- read.csv("Data/Survival/RangeX_clean_Survival_2021_2022_2023_NOR.csv")



# fix date ---------------------------------------------------------
chn <- chn |> 
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y")) |> 
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"))

che <- che |> 
  mutate(date_planting = as.Date(date_planting)) |> 
  mutate(date_measurement = as.Date(date_measurement))

nor <- nor |> 
  mutate(date_planting = as.Date(date_planting)) |> 
  mutate(date_measurement = as.Date(date_measurement))


# merge metadata with survival data -------------------------------------------
# CHE
survival_che <- che |> 
  left_join(metadata, by = c("unique_plant_ID", "species", "date_planting"))

# NOR
survival_nor <- nor |> 
  left_join(metadata, by = c("unique_plant_ID", "species", "date_planting"))

# CHN
survival_chn <- chn |> 
  left_join(metadata, by = c("unique_plant_ID", "species", "date_planting"))


# add column variable in CHN ----------------------------------------------
survival_chn <- survival_chn |> 
  mutate(variable = "peak_survival")


# rename survival to value ------------------------------------------------
survival_chn <- survival_chn |> 
  rename(value = survival)


# combine all regions -----------------------------------------------------
survival <- rbind(survival_che, survival_chn, survival_nor)


glimpse(survival)


# add column year ---------------------------------------------------------
survival <- survival |> 
  mutate(year = year(date_measurement))


# samples per region ------------------------------------------------------
count_region <- survival |> 
  count(region)
count_region


# percentage of survival per region and year ------------------------------
survival_region_year <- survival |>
  group_by(region, year) |>
  summarise(
    n_alive = sum(value == 1, na.rm = TRUE),
    n_total = n(),                       
    survival_percent = 100 * n_alive / n_total,
    .groups = "drop")
survival_region_year


# percentage of survival per region, species and year ------------------------------
survival_region_year_species <- survival |>
  group_by(region, year, species) |>
  summarise(
    n_alive = sum(value == 1, na.rm = TRUE),
    n_total = n(),                       
    survival_percent = 100 * n_alive / n_total,
    .groups = "drop")
survival_region_year_species


# survival per individual plant -------------------------------------------
survival_per_plant <- survival |>
  group_by(unique_plant_ID, region, site, species) |>
  summarise(
    final_status = last(value[order(date_measurement)]),  # alive (1) or dead (0)
    .groups = "drop"
  )
survival_per_plant

# and then average survival per region
survival_region <- survival_per_plant |>
  group_by(region) |>
  summarise(
    n_alive = sum(final_status == 1),
    n_total = n(),
    survival_percent = 100 * n_alive / n_total,
    .groups = "drop"
  )
survival_region


# not sure if that makes senses
# survival_per_plant <- survival |>
#   group_by(unique_plant_ID, region, year, species) |>
#   summarise(
#     final_status = last(value[order(date_measurement)]),  # alive (1) or dead (0)
#     .groups = "drop"
#   )
# survival_per_plant
# 
# survival_region_year_species <- survival_per_plant |>
#   group_by(region, year, species) |>
#   summarise(
#     n_alive = sum(final_status == 1),
#     n_total = n(),
#     survival_percent = 100 * n_alive / n_total,
#     .groups = "drop"
#   )
# survival_region_year_species
# 
# 
# threshold <- 80
# 
# high_survival_species <- survival_region_year_species |>
#   filter(survival_percent >= threshold)
# high_survival_species
# 
# n_high <- high_survival_species |>
#   count(region, year)
# n_high
# 
# n <- high_survival_species |>
#   count(region)
# n







# survival per individual plant -------------------------------------------
survival_per_plant <- survival |>
  group_by(unique_plant_ID, region, year, site, species, variable) |>
  summarise(
    final_status = last(value[order(date_measurement)]),  # alive (1) or dead (0)
    .groups = "drop"
  )
survival_per_plant

# and then average survival per region and variable 
survival_region <- survival_per_plant |>
  group_by(region, year, variable) |>
  summarise(
    n_alive = sum(final_status == 1),
    n_total = n(),
    survival_percent = 100 * n_alive / n_total,
    .groups = "drop"
  )
survival_region









