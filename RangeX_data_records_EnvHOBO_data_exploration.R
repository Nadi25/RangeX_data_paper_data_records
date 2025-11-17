
## RangeX HOBO CHE

## Data used: RangeX_clean_MetadataPlot_CHE.csv,
##            RangeX_clean_EnvHOBO_2021_2023_CHE.csv
## Date:      10.11.25
## Author:    Nadine Arzt
## Purpose:   Explore EnvClimateStation data NOR


# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import meta data -------------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_CHE.csv")

# filter CHE
meta_che <- metadata |> 
  filter(region == "CHE")


# import HOBO data --------------------------------------------------------
hobo <- read.csv("Data/EnvHOBO/RangeX_clean_EnvHOBO_2021_2023_CHE.csv")
glimpse(hobo)



# combine metadata with hobo ----------------------------------------------
hobo_che <- hobo |> 
  left_join(meta_che, by = "unique_plot_ID")


names(hobo_che)


# mean and sd per year and site --------------------------------------------------
hobo_summary <- hobo_che |> 
  mutate(year_from_time = lubridate::year(date_time)) |> 
  group_by(site, year_from_time) |> 
  summarise(
    n = n(),
    across(
      c(temperature),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
hobo_summary


# mean and sd per year, site and treatment -------------------------------------
hobo_summary2 <- hobo_che |> 
  mutate(year_from_time = lubridate::year(date_time)) |> 
  group_by(year_from_time, site, treat_warming) |> 
  summarise(
    n = n(),
    across(
      c(temperature),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
hobo_summary2



# deltas between sites ----------------------------------------------------
hobo_deltas <- hobo_summary2 |> 
  group_by(year_from_time, site) |> 
  summarise(
    across(
      temperature_mean,
      ~ diff(.x),  # difference between sites in same year
      .names = "{.col}_delta"
    )
  )
hobo_deltas











































