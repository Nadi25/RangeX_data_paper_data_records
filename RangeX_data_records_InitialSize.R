
## RangeX Initial size CHE, CHN

## Data used: 
## Date:      21.12.25
## Author:    Nadine Arzt
## Purpose:   

# comment -----------------------------------------------------------------

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")

# date format
metadata <- metadata |>
  mutate(
    date_planting   = as.Date(date_planting, format = "%Y-%m-%d")
  )

# import initial size data ------------------------------------------------
in_si_che <- read.csv("Data/InitialSize/RangeX_clean_InitialSize_2021_CHE.csv")

in_si_chn1 <- read.csv("Data/InitialSize/RangeX_clean_InitialSize_2021_CHN.csv")


# che: add leaf_width -----------------------------------------------------
in_si_che <- in_si_che |>
  mutate(leaf_width = NA_real_) |>
  select(
    unique_plant_ID, ind_number, species, functional_group,
    date_measurement, date_planting, collector,
    height_vegetative_str, vegetative_width,
    leaf_length1, leaf_length2, leaf_length3,
    leaf_width, number_leaves
  )

# chn: fix index number ---------------------------------------------------
in_si_chn1 <- in_si_chn1 |> 
  mutate(ind_number = 1)

# chn: match column names like in che -------------------------------------
in_si_chn <- in_si_chn1[, names(in_si_che)]


# chn: fix date format -----------------------------------------------------------
in_si_chn <- in_si_chn |>  
  mutate(date_planting = as.Date(date_planting, format = "%d.%m.%Y"))

in_si_chn <- in_si_chn |>  
  mutate(date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"))


in_si_che <- in_si_che |>
  mutate(
    date_planting   = as.Date(date_planting, format = "%Y-%m-%d"),
    date_measurement = as.Date(date_measurement, format = "%Y-%m-%d")
  )




# combine che and chn -----------------------------------------------------
initial_size <- rbind(in_si_che, in_si_chn)


# merge metadata with initial size -------------------------------------------
initial_size <- initial_size |> 
  left_join(metadata, by = c("unique_plant_ID", "species", 
                             "ind_number", "functional_group", "date_planting"))

glimpse(initial_size)




# how many species --------------------------------------------------------
unique(initial_size$species)














