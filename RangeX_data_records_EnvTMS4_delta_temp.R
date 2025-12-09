

# Is the experiment working? ---------------------------------------------

# Are the OTCs working? ---------------------------------------------------

# RangeX EnvTMS4 data exploration NOR, CHE, CHN, ZAF ------------

## Data used: RangeX_data_records_EnvTMS4.R
## Date:      24.11.25
## Author:    Nadine Arzt
## Purpose:   Delta temp for inside and outside OTCs and for lo vs hi site


# comments ----------------------------------------------------------------
# temp1 is in soil, temp2 at 0cm and temp3 20cm above ground


# source TMS script -------------------------------------------------------
source("RangeX_data_records_EnvTMS4.R")
envTMS


# filter only peak season? ------------------------------------------------
envTMS <- envTMS |>
  mutate(
    date_time = lubridate::parse_date_time(
      date_time,
      orders = c("Y-m-d H:M:S", "Y-m-d")
    )
  )

# is peak June to September everywhere
envTMS_peak <- envTMS |>
  filter(month(date_time) >= 6, month(date_time) <= 9)




#  add column year --------------------------------------------------------
envTMS_peak <- envTMS_peak |>
  mutate(year = year(date_time))

# exclude NOR 2021 --------------------------------------------------------
envTMS_clean <- envTMS_peak |>
  filter(!(region == "NOR" & year == 2021))


# filter only hi ----------------------------------------------------------
env_hi <- envTMS_clean |> 
  filter(site == "hi")


# calcualte mean per year -------------------------------------------------
tmean <- env_hi |>
  group_by(region, year, treat_warming) |>
  summarise(
    T1 = mean(TMS_T1, na.rm = TRUE),
    T2 = mean(TMS_T2, na.rm = TRUE),
    T3 = mean(TMS_T3, na.rm = TRUE),
    .groups = "drop"
  )
tmean


# get delta temp warm vs ambi ---------------------------------------------
deltaT <- tmean |>
  pivot_wider(
    names_from = treat_warming,
    values_from = c(T1, T2, T3)
  ) |>
  mutate(
    dT1 = T1_warm - T1_ambi,
    dT2 = T2_warm - T2_ambi,
    dT3 = T3_warm - T3_ambi
  )
deltaT

deltaT_stats <- deltaT |>
  group_by(region) |>
  summarise(
    mean_dT1 = mean(dT1, na.rm = TRUE),
    sd_dT1   = sd(dT1, na.rm = TRUE),
    
    mean_dT2 = mean(dT2, na.rm = TRUE),
    sd_dT2   = sd(dT2, na.rm = TRUE),
    
    mean_dT3 = mean(dT3, na.rm = TRUE),
    sd_dT3   = sd(dT3, na.rm = TRUE),
    
    n_years  = sum(!is.na(dT1)),
    .groups = "drop"
  )

deltaT_stats


# delta temp sites --------------------------------------------------------
# filter only ambi to exclude warm when comparing hi vs lo
tmean_site_ambi <- envTMS_peak |>
  filter(treat_warming == "ambi") |>
  group_by(region, year, site) |>
  summarise(
    T1 = mean(TMS_T1, na.rm = TRUE),
    T2 = mean(TMS_T2, na.rm = TRUE),
    T3 = mean(TMS_T3, na.rm = TRUE),
    .groups = "drop"
  )
tmean_site_ambi

twide_site <- tmean_site_ambi |>
  pivot_wider(
    names_from = site,
    values_from = c(T1, T2, T3)
  )
twide_site

delta_site <- twide_site |>
  mutate(
    dT1_hi_lo = T1_lo - T1_hi,
    dT2_hi_lo = T2_lo - T2_hi,
    dT3_hi_lo = T3_lo - T3_hi
  )
delta_site


# average delta t lo vs hi across years ------------------------------------------------
delta_region_mean <- delta_site |>
  group_by(region) |>
  summarise(
    dT1_hi_lo = mean(dT1_hi_lo, na.rm = TRUE),
    dT2_hi_lo = mean(dT2_hi_lo, na.rm = TRUE),
    dT3_hi_lo = mean(dT3_hi_lo, na.rm = TRUE),
    .groups = "drop"
  )
delta_region_mean


# NOR 2021 is incomplete --------------------------------------------------
# check timeframe
envTMS_NOR_2021 <- envTMS |>
  filter(region == "NOR", year == 2021) |>
  summarise(
    first = min(date_time, na.rm = TRUE),
    last  = max(date_time, na.rm = TRUE),
    n     = n()
  )

envTMS_NOR_2021


# exclude NOR 2021 --------------------------------------------------------
envTMS_clean <- envTMS_peak |>
  filter(!(region == "NOR" & year == 2021))

tmean_site_ambi <- envTMS_clean |>
  filter(treat_warming == "ambi") |>
  group_by(region, year, site) |>
  summarise(
    T1 = mean(TMS_T1, na.rm = TRUE),
    T2 = mean(TMS_T2, na.rm = TRUE),
    T3 = mean(TMS_T3, na.rm = TRUE),
    .groups = "drop"
  )
tmean_site_ambi


twide_site <- tmean_site_ambi |>
  pivot_wider(
    names_from = site,
    values_from = c(T1, T2, T3)
  )
twide_site

delta_site <- twide_site |>
  mutate(
    dT1_hi_lo = T1_lo - T1_hi,
    dT2_hi_lo = T2_lo - T2_hi,
    dT3_hi_lo = T3_lo - T3_hi
  )
delta_site

# average delta t lo vs hi across years ------------------------------------------------
delta_region_mean <- delta_site |>
  group_by(region) |>
  summarise(
    dT1_hi_lo = mean(dT1_hi_lo, na.rm = TRUE),
    dT2_hi_lo = mean(dT2_hi_lo, na.rm = TRUE),
    dT3_hi_lo = mean(dT3_hi_lo, na.rm = TRUE),
    .groups = "drop"
  )
delta_region_mean

delta_region_stats <- delta_site |>
  group_by(region) |>
  summarise(
    mean_dT1_hi_lo = mean(dT1_hi_lo, na.rm = TRUE),
    sd_dT1_hi_lo   = sd(dT1_hi_lo, na.rm = TRUE),
    
    mean_dT2_hi_lo = mean(dT2_hi_lo, na.rm = TRUE),
    sd_dT2_hi_lo   = sd(dT2_hi_lo, na.rm = TRUE),
    
    mean_dT3_hi_lo = mean(dT3_hi_lo, na.rm = TRUE),
    sd_dT3_hi_lo   = sd(dT3_hi_lo, na.rm = TRUE),
    
    n_years        = sum(!is.na(dT1_hi_lo)),
    .groups = "drop"
  )

delta_region_stats
















