
## RangeX EnvClimateStation NOR

## Data used: RangeX_clean_MetadataFocal_all.csv,
##            
## Date:      10.11.25
## Author:    Nadine Arzt
## Purpose:   Explore EnvClimateStation data NOR


# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import climate station data -----------------------------------------------------
climate <- read.csv("Data/EnvClimateStation/RangeX_clean_EnvClimateStation_2021-2025_NOR.csv")


# filter until December 23 ------------------------------------------------
climate_21_23 <- climate |> 
  filter(date_time <= as.POSIXct("2023-12-31 23:59:59"))
# 82 862 observations


# count per site ----------------------------------------------
count <- climate_21_23 |> 
  count(site)
count


# count per site and year ----------------------------------------------
count2 <- climate_21_23 |> 
  count(year, site)
count2


# average per year per site -----------------------------------------------
# climate_summary <- climate_21_23 |> 
#   group_by(site, year) |> 
#   summarise(
#     across(
#       where(is.numeric),
#       ~mean(.x, na.rm = TRUE),
#       .names = "{.col}_mean"
#     ),
#     .groups = "drop"
#   )
# climate_summary

# using year from the date_tiem column
climate_21_23 <- climate_21_23 |> 
  mutate(date_time = as.POSIXct(date_time, tz = "UTC"))  # set tz as appropriate

# derive year from date_time and summarise per site × year
climate_summary <- climate_21_23 |> 
  mutate(year_from_time = year(date_time)) |> 
  group_by(site, year_from_time)|> 
  summarise(
    n = n(),  # count of observations
    across(
      c(AirTemp_Avg, Humidity_Avg, WindDir_Avg, WindSpd_Avg, Radiation_Avg, Rainfall),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

climate_summary


climate_deltas <- climate_summary |> 
  group_by(year_from_time) |> 
  summarise(
    across(
      ends_with("_mean"),
      ~ diff(.x),  # difference between sites in same year
      .names = "{.col}_delta"
    )
  )
climate_deltas

