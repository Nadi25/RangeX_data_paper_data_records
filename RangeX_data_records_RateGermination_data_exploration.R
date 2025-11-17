
## RangeX RateGermination CHE data exploration

## Data used: RangeX_clean_RateGermination_2022_2023_CHE.csv
##            
## Date:      07.11.25
## Author:    Nadine Arzt
## Purpose:   Explore Germination rate data 
##            How many samples? 


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")

# filter CHE
meta_che <- metadata |> 
  filter(region == "CHE")



# import germination data -------------------------------------------------------------
germination <- read.csv("Data/RateGermination/RangeX_clean_RateGermination_2022_2023_CHE.csv")
glimpse(germination)
# 7650 observations



# split into experiment 1 and 2 -------------------------------------------
exp1 <- germination |> 
  filter(str_detect(germination_position_ID, "exp1"))

exp2 <- germination |> 
  filter(str_detect(germination_position_ID, "exp2"))


# how many species overall --------------------------------------------------------
unique(exp2$species)
unique(exp2$functional_group)


# Exp 1 -------------------------------------------------------------------
# when measured -----------------------------------------------------------
unique(exp1$date_measurement)
unique(exp1$variable)

unique(exp1$germination_position_ID)


exp1 |>
  distinct(date_measurement, variable) |>
  arrange(date_measurement, variable)


# split germination_position_ID ------------------------------------------------------------------
exp1<- exp1 |> 
  separate(
    germination_position_ID,
    into = c("region", "site", "experiment", "treat_warming",
             "treat_competition", "plot", "position"),
    sep = "\\.",
    remove = FALSE
  )
exp1


# germination rate --------------------------------------------------------
exp1 <- exp1 |> 
  mutate(germination_rate )





# Exp 2 -------------------------------------------------------------------
# when measured -----------------------------------------------------------
unique(exp2$date_measurement)
unique(exp2$variable)

unique(exp2$germination_position_ID)


exp2 |>
  distinct(date_measurement, variable, site) |>
  arrange(date_measurement, variable, site)


# split germination_position_ID ------------------------------------------------------------------
exp2<- exp2 |> 
  separate(
    germination_position_ID,
    into = c("region", "site", "experiment", "treat_warming",
             "treat_competition", "plot", "position"),
    sep = "\\.",
    remove = FALSE
  )
exp2




# Seedling biomass --------------------------------------------------------

seedling_biomass <- read.csv("Data/RateGermination_SeedlingBiomass/RangeX_clean_SeedlingBiomass_2023_2024_CHE.csv")
glimpse(seedling_biomass)

# average, sd, min, max per species and biomass -------------------------------------------------------
seedling_biomass_summary <- seedling_biomass |> 
  group_by(species) |> 
  summarise(
    dry_mass_mean = mean(dry_mass, na.rm = TRUE),
    dry_mass_sd   = sd(dry_mass, na.rm = TRUE),
    dry_mass_min  = min(dry_mass, na.rm = TRUE),
    dry_mass_max  = max(dry_mass, na.rm = TRUE)
  )
seedling_biomass_summary

seedling_biomass_summary2 <- seedling_biomass |> 
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
seedling_biomass_summary2


















