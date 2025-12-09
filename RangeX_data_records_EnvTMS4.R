

# RangeX EnvTMS4 data exploration NOR, CHE, CHN, ZAF ------------

## Data used: 
##            RangeX_clean_EnvTMS4_2021_2023_CHE.csv
##            RangeX_clean_EnvTMS_2023_CHN.csv
##            RangeX_clean_EnvTMS4_2021-24_NOR.csv
##            
##            
##            
## Date:      21.11.25
## Author:    Nadine Arzt
## Purpose:   Combine data and explore


# comment -----------------------------------------------------------------


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)


# import metadata ---------------------------------------------------------
meta_p_che <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_CHE.csv")

meta_p_chn <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_CHN.csv")

meta_p_nor <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_NOR.csv")

meta_p_zaf <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_ZAF.csv")


# zaf plot and logger matches ---------------------------------------------
meta_zaf_logger <- read.csv("Data/EnvTMS4/plot_id.csv")


# fix meta chn ------------------------------------------------------------
# check for duplicates
meta_p_chn |>  
  count(unique_plot_ID) |>  
  filter(n > 1)

meta_p_chn_clean <- meta_p_chn |> distinct(unique_plot_ID, .keep_all = TRUE)



# fix meta zaf ------------------------------------------------------------
# one unique_id has no ZAF in front
meta_zaf_logger <- meta_zaf_logger |> 
  mutate(
    unique_id = if_else(
      tomst_id == "94219710",
      paste("ZAF", unique_id, sep = "."),
      unique_id
    )
  )

# # separate unique_id into the treatment columns
# meta_p_zaf <- meta_zaf |> 
#   separate(
#     unique_id,
#     into = c("region", "site", "treat_warming", "treat_competition", "added_focals", "block_ID"),
#     sep = "\\.",
#     remove = FALSE
#   ) |> 
#   mutate(
#     block_ID_original = NA_character_,
#     plot_ID_original  = NA_character_,
#     unique_plot_ID = unique_id) |> 
#   select(
#     region, site,
#     block_ID_original, plot_ID_original,
#     treat_warming, treat_competition, added_focals,
#     block_ID, unique_plot_ID,
#     everything()
#   )
# 
# # filter out unnecessary columns
# meta_p_zaf <- meta_p_zaf |> 
#   select(-c(plot_id, tomst_id, unique_id))



# import tms data ---------------------------------------------------------
tms_che <- read.csv("Data/EnvTMS4/RangeX_clean_EnvTMS4_2021_2023_CHE.csv")

tms_chn23_24 <- read.csv("Data/EnvTMS4/RangeX_clean_EnvTMS_2023_CHN.csv")

tms_nor21_24 <- read.csv("Data/EnvTMS4/RangeX_clean_EnvTMS4_2021-24_NOR.csv")

tms_zaf22 <- read_xlsx("Data/EnvTMS4/d_2022.xlsx")
tms_zaf23 <- read_xlsx("Data/EnvTMS4/d_2023.xlsx")
tms_zaf24 <- read_xlsx("Data/EnvTMS4/d_2024.xlsx")



# filter NOR and CHN until 23 -----------------------------------------------------
# tms_nor <- tms_nor21_24 |> 
#   mutate(date_time = ymd_hms(date_time)) |>
#   filter(year(date_time) <= 2023)

tms_nor <- tms_nor21_24 |>
  mutate(
    date_time = parse_date_time(
      date_time,
      orders = c("Y-m-d H:M:S", "Y-m-d")
    )
  ) |>
  filter(year(date_time) <= 2023)

# CHN
tms_chn <- tms_chn23_24 |>
  mutate(
    date_time = parse_date_time(
      date_time,
      orders = c("Y-m-d H:M:S", "Y-m-d")
    )
  ) |>
  filter(year(date_time) <= 2023)

# combine zaf tms ---------------------------------------------------------
tms_zaf <- bind_rows(tms_zaf22, tms_zaf23)


# fix column names zaf ----------------------------------------------------------
tms_zaf <- tms_zaf |> 
  rename(date_time = datetime,
         TMS_T1 = T1,
         TMS_T2 = T2,
         TMS_T3 = T3,
         TMS_moist = moist_vol)


tms_zaf <- tms_zaf |> 
  mutate(VWC = NA) |> 
  select(unique_plot_ID, date_time, TMS_T1, TMS_T2, TMS_T3, TMS_moist, VWC)

# # SLOW
# # quick control plot
# ggplot(tms_zaf22, 
#        aes(x = datetime, y = T2, color = unique_plot_ID)) +
#   geom_point() +
#   theme(legend.position = "right")


# potentially: fix VWC  ---------------------------------------------------
tms_zaf <- tms_zaf |>
  mutate(
    VWC = TMS_moist,      # move current values into VWC
    TMS_moist = NA_real_  # empty the raw column
  )

# bad fix by dividing VWC values by 100
tms_zaf <- tms_zaf |>
  mutate(VWC = VWC/100)


# fix columns chn ---------------------------------------------------------
tms_chn <- tms_chn |> 
  mutate(VWC = NA) |> 
  select(unique_plot_ID, date_time, TMS_T1, TMS_T2, TMS_T3, TMS_moist, VWC)


# filter only 2023 for CHN ------------------------------------------------



# # SLOW --------------------------------------------------------------------
# # plot VWC ----------------------------------------------------------------
# ggplot(tms_nor, 
#        aes(x = date_time, y = VWC, color = unique_plot_ID)) +
#   geom_point() +
#   theme(legend.position = "none")
# 
# ggplot(tms_che, 
#        aes(x = date_time, y = VWC, color = unique_plot_ID)) +
#   geom_point() +
#   theme(legend.position = "none")



# combine metadata with tms data ------------------------------------------
env_che <- tms_che |> 
  left_join(meta_p_che, by = "unique_plot_ID")

# env_chn <- tms_chn |> 
#   left_join(meta_p_chn, by = "unique_plot_ID")

env_chn <- tms_chn |>
  mutate(
    date_time = parse_date_time(
      date_time,
      orders = c("Y-m-d H:M:S", "Y-m-d")
    )
  ) |>
  left_join(meta_p_chn_clean, by = "unique_plot_ID")


env_nor <- tms_nor |> 
  left_join(meta_p_nor, by = "unique_plot_ID")


env_zaf <- tms_zaf |> 
  left_join(meta_p_zaf, by = "unique_plot_ID")



# fix VWC for CHN -------------------------------------------------
source("RangeX_data_paper_functions.R")

# calculate soil moisture VWC -----------------------------------------------------------
# apply the soil moisture function
# it calculates VWC (volumetric water content)
env_chn <- env_chn |> 
  mutate(VWC = calc_soil_moist(rawsoilmoist = TMS_moist, 
                               soil_temp = TMS_T1, 
                               soilclass ="silt_loam"))
# using silt loam even though this might not be correct


# Date as date ------------------------------------------------------------
env_che <- env_che |> 
  mutate(date_time = parse_date_time(date_time, orders = c("Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d")))

# check if CHE has NAs in date_time ------------------------------------------------
date_na <- env_che |> 
  filter(is.na(date_time))
date_na

# plot_ID_original as chr -------------------------------------------------
env_che <- env_che |> mutate(plot_ID_original = as.character(plot_ID_original))
env_chn <- env_chn |> mutate(plot_ID_original = as.character(plot_ID_original))
env_nor <- env_nor |> mutate(plot_ID_original = as.character(plot_ID_original))
env_zaf <- env_zaf |> mutate(plot_ID_original = as.character(plot_ID_original))

# block_ID_original as chr -------------------------------------------------
env_che <- env_che |> mutate(block_ID_original  = as.character(block_ID_original ))
env_chn <- env_chn |> mutate(block_ID_original  = as.character(block_ID_original ))
env_nor <- env_nor |> mutate(block_ID_original  = as.character(block_ID_original ))
env_zaf <- env_zaf |> mutate(block_ID_original  = as.character(block_ID_original ))

# block_ID as chr -------------------------------------------------
env_che <- env_che |> mutate(block_ID  = as.character(block_ID ))
env_chn <- env_chn |> mutate(block_ID  = as.character(block_ID ))
env_nor <- env_nor |> mutate(block_ID  = as.character(block_ID ))
env_zaf <- env_zaf |> mutate(block_ID  = as.character(block_ID ))

# combine all regions to one EnvTMS4 dataset  -----------------------------
envTMS <- bind_rows(env_che, env_chn, env_nor, env_zaf)







