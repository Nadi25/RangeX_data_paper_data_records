
# RangeX Vegetation survey data exploration NOR, CHE, CHN, ZAF ------------

## Data used: RangeX_data_records_Vegetation_Surveys.R
## Date:      10.12.25
## Author:    Nadine Arzt
## Purpose:   Explore NOR, CHE, ZAF and CHN data 


# comments ----------------------------------------------------------------
# CHN: Campanula glomerata twice?
# Festuca rubra and agg. is the same? 
# CHE has Anthoxantum alpinum while NOR has nipponicum

# I dont want to clean ZAF data
# "Diheteropogon" no. 2
# Poaceae

# Galium


# library -----------------------------------------------------------------
#library(taxize)


# source data combined ----------------------------------------------------
source("RangeX_data_records_Vegetation_Surveys.R")
vege_all


# add combined treatment --------------------------------------------------
vege_all <- vege_all |>
  mutate(
    treatment = paste(site, treat_warming, treat_competition, added_focals, sep = "_"))


# add column year ---------------------------------------------------------
vege_all <- vege_all |>
  mutate(year = lubridate::year(date_measurement))

# ZAF has values in bare plots --------------------------------------------
# find out if that is focals
species_zaf_bare <- vege_all |>
  filter(region == "ZAF",
         treat_competition == "bare") |>
  distinct(species) |>
  arrange(species)
species_zaf_bare


# delete bare plots -------------------------------------------------------
vege_all <- vege_all |>
  filter(treat_competition != "bare")


# delete moss, litter,  ---------------------------------------------------
vege_all <- vege_all |>
  filter(!species %in% c("moss", "litter", "bare soil", "mushroom", "stone"))

# get species list --------------------------------------------------------
species_list <- unique(vege_all$species)

# See how many species you have
length(species_list) # 389

# Optionally sort alphabetically
species_list <- sort(species_list)

species_list <- as.character(species_list)

# Make a table from the species vector
species_list_table <- data.frame(species = species_list)



# how many species  -------------------------------------------------------
unique(vege_all$species)

n_distinct(vege_all$species)
# 389

species_region <- vege_all |>
  group_by(region) |>
  summarise(n_species = n_distinct(species)) |>
  arrange(desc(n_species))
species_region

# region n_species
# 1 CHE          156
# 2 NOR          134
# 3 ZAF           76
# 4 CHN           52


# how many Carex species? -------------------------------------------------
carex_count <- species_list_table |> 
  filter(str_detect(species, "^Carex")) |> 
  nrow()
carex_count
# 29


# how often are the species recorded --------------------------------------
n_species <- vege_all |>
  count(species)
n_species

n_species_site <- vege_all |>
  count(species, site)
n_species_site

n_region_species <- vege_all |>
  count(region, species)
n_region_species

n_region <- vege_all |>
  count(region)
n_region



# calculate species richness per plot, year, region, treatment -----------------------
richness_plot <- vege_all |>
  group_by(region, year, treatment, unique_plot_ID) |>
  summarise(species_richness = n_distinct(species), .groups = "drop")
richness_plot

# now mean richness per region, year and treatment --------------------------------------
mean_richness <- richness_plot |>
  group_by(region, year, treatment) |>
  summarise(
    mean_species_richness = mean(species_richness, na.rm = TRUE),
    sd_species_richness   = sd(species_richness, na.rm = TRUE),
    n                     = dplyr::n(),
    .groups = "drop"
  )
mean_richness

# now mean richness per region, year --------------------------------------
mean_richness2 <- richness_plot |>
  group_by(region, year) |>
  summarise(mean_species_richness = mean(species_richness), .groups = "drop")
mean_richness2


# mean richness per region and treatment ----------------------------------
mean_richness3 <- richness_plot |>
  group_by(region,treatment) |>
  summarise(
    mean_species_richness = mean(species_richness, na.rm = TRUE),
    sd_species_richness   = sd(species_richness, na.rm = TRUE),
    n                     = dplyr::n(),
    .groups = "drop"
  )
mean_richness3


# mean richness per region  ----------------------------------
mean_richness4 <- richness_plot |>
  group_by(region) |>
  summarise(
    mean_species_richness = mean(species_richness, na.rm = TRUE),
    sd_species_richness   = sd(species_richness, na.rm = TRUE),
    n                     = dplyr::n(),
    .groups = "drop"
  )
mean_richness4




# Plot --------------------------------------------------------------------
richness_summary <- richness_plot |>
  group_by(region, year, treatment) |>
  summarise(
    mean_richness = mean(species_richness, na.rm = TRUE),
    .groups = "drop"
  )
richness_summary

ggplot() +
  # raw data per plot
  geom_point(
    data = richness_plot,
    aes(x = year, y = species_richness, color = region),
    alpha = 0.3, size = 2
  ) +
  # mean richness per region-year-treatment
  geom_line(
    data = richness_summary,
    aes(x = year, y = mean_richness, color = region, group = interaction(region, treatment)),
    linewidth = 1
  ) +
  geom_point(
    data = richness_summary,
    aes(x = year, y = mean_richness, color = region),
    size = 3
  ) +
  facet_wrap(~ treatment) +
  labs(
    x = "Year",
    y = "Species richness",
    title = "Mean species richness per region, year, treatment"
  )


# plot mean species ricness per region and treatment ----------------------
ggplot(mean_richness3,
       aes(x = region, 
           y = mean_species_richness)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(
    aes(ymin = mean_species_richness - sd_species_richness,
        ymax = mean_species_richness + sd_species_richness),
    width = 0.2
  ) +
  facet_wrap(~ treatment) +
  labs(
    x = "Region",
    y = "Mean species richness (± SD)",
    title = "Species richness by region and treatment")

# all in one plot with region as color
ggplot(mean_richness3,
       aes(x = treatment, 
           y = mean_species_richness, 
           color = region)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = mean_species_richness - sd_species_richness,
        ymax = mean_species_richness + sd_species_richness),
    width = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  labs(
    x = "Treatment",
    y = "Mean species richness per plot per treatment(+- SD)"
  ) + 
  theme_bw()



# hi warm vs ambi ---------------------------------------------------------
vege_hi <- vege_all |> 
  filter(site == "hi" )


# across regions ----------------------------------------------------------
# calculate species richness per plot,  treatment -----------------------
richness_plot2 <- vege_hi |>
  group_by(treatment, unique_plot_ID) |>
  summarise(species_richness = n_distinct(species), .groups = "drop")
richness_plot2

# now mean richness per region, year and treatment --------------------------------------
mean_richness5 <- richness_plot2 |>
  group_by(treatment) |>
  summarise(
    mean_species_richness = mean(species_richness, na.rm = TRUE),
    sd_species_richness   = sd(species_richness, na.rm = TRUE),
    n                     = dplyr::n(),
    .groups = "drop"
  )
mean_richness5



# most abundant species per region# ---------------------------------------
vege_all_2 <- vege_all |> 
  mutate(
    cover = if_else(
      stringr::str_detect(cover, "^<"),
      as.numeric(stringr::str_remove(cover, "^<")) / 2,
      as.numeric(cover)
    )
  )
vege_all_2

top5 <- vege_all_2 |> 
  group_by(region, species) |> 
  summarise(total_cover = sum(cover, na.rm = TRUE), .groups = "drop") |> 
  arrange(region, desc(total_cover)) |> 
  group_by(region) |> 
  slice_head(n = 5)
top5


# get taxonomie (family) per species --------------------------------------


# clean species names -----------------------------------------------------
# species_list_clean <- gnr_resolve(sci = species_list)
# did not work



# shared species across regions? ------------------------------------------
# Step 1: create a presence table
species_region <- vege_all |> 
  select(species, region) |> 
  distinct() |> 
  mutate(presence = 1) |> 
  pivot_wider(names_from = region, values_from = presence, values_fill = 0)

# Step 2: count number of regions per species
species_region <- species_region |> 
  rowwise() |> 
  mutate(regions_present = sum(c_across(-species)))
species_region

# Step 3: shared species
total_regions <- length(unique(vege_all$region))
total_regions

# species shared in 2 or more regions
shared_species_2plus <- species_region |> 
  filter(regions_present >= 2) |> 
  select(species, regions_present) |> 
  arrange(desc(regions_present))

shared_species_2plus


