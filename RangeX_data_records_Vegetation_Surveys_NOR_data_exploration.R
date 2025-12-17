
# RangeX Vegetation survey data exploration NOR subplots ------------

## Data used: 
## Date:      13.12.25
## Author:    Nadine Arzt
## Purpose:   Explore NOR data 


# comments ----------------------------------------------------------------


# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)

# import metadata plot ----------------------------------------------------
meta_p_nor <- read.csv("Data/Metadata/RangeX_clean_MetadataPlot_NOR.csv")


# import data -------------------------------------------------------------
vege_nor_16 <- read.csv("Data/VegSurveyGeneral/RangeX_clean_VegSurveyNOR_21_22_23_NOR.csv")


# combine metadata with vege ----------------------------------------------
vege_nor_detail <- vege_nor_16 |> 
  left_join(meta_p_nor, by = "unique_plot_ID")

# remove rows with 0 in cover ---------------------------------------------
vege_nor_detail <- vege_nor_detail |>
  filter(cover != 0)


# get species list --------------------------------------------------------
species_list_nor <- unique(vege_nor_detail$species)

# See how many species you have
length(species_list_nor) # 134

# Optionally sort alphabetically
species_list <- sort(species_list)

species_list_nor <- as.character(species_list_nor)

# Make a table from the species vector
species_list_nor_table <- data.frame(species = species_list_nor)



# check species names nor from general dataset ----------------------------
nor <- vege_all |> 
  filter(region == "NOR")

species_list_nor2 <- unique(nor$species)

# See how many species you have
length(species_list_nor2) # 116

# Optionally sort alphabetically
species_list_nor2 <- sort(species_list_nor2)

species_list_nor2 <- as.character(species_list_nor2)

# Make a table from the species vector
species_list_nor2_table <- data.frame(species = species_list_nor2)



# species in subplot but not in general
diff <- setdiff(
  species_list_nor_table$species,
  species_list_nor2_table$species
)
diff





# which species is present in all subplots --------------------------------

names(vege_nor_detail)


species_subplot_occ <- vege_nor_detail |> 
  distinct(unique_plot_ID, subplot, species) |>   # presence/absence
  count(species, name = "n_subplots") |> 
  arrange(desc(n_subplots))
species_subplot_occ


# proportion of subplots per species --------------------------------------
total_subplots <- vege_nor_detail |>
  distinct(unique_plot_ID, subplot) |>
  nrow()

species_subplot_occ <- species_subplot_occ |>
  mutate(prop_subplots = n_subplots / total_subplots)
species_subplot_occ


# which species is present in almost all subplots? ------------------------
# per plot ----------------------------------------------------------------
species_per_plot <- vege_nor_detail |>
  distinct(unique_plot_ID, site, subplot, species) |>
  count(species, unique_plot_ID, name = "n_subplots_plot")|>
  group_by(species)|>
  summarise(
    mean_subplots = mean(n_subplots_plot),
    max_subplots  = max(n_subplots_plot),
    .groups = "drop"
  )|>
  arrange(desc(mean_subplots))
species_per_plot


# per site ----------------------------------------------------------------
occ_plot <- vege_nor_detail |>
  distinct(site, unique_plot_ID, subplot, species) |>
  count(site, unique_plot_ID, species, name = "n_subplots")
occ_plot

occ_species_site <- occ_plot |>
  group_by(site, species) |>
  summarise(
    mean_subplots = mean(n_subplots),
    max_subplots  = max(n_subplots),
    n_plots       = n(),
    .groups = "drop"
  ) |>
  arrange(site, desc(mean_subplots))
occ_species_site



# exploring reproductive capacity -----------------------------------------
# frequency of reproductive observations ----------------------------------
# shows how many times a species waas recorded and calculates frequency of flowering
# repro_species_site <- vege_nor_detail |>
#   group_by(site, species) |>
#   summarise(
#     n_obs = n(),
#     prop_reproductive = mean(reproductive_capacity, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   arrange(site, desc(prop_reproductive))
# repro_species_site

# which subplots were serveyed per site, treatment
subplots_surveyed <- vege_nor_detail |>
  distinct(site, unique_plot_ID, subplot,
           treat_warming, treat_competition, added_focals)
subplots_surveyed


# count in how many subplots a species flowered ---------------------------
repro_counts <- vege_nor_detail |>
  filter(reproductive_capacity == 1) |>
  distinct(site, species, unique_plot_ID, subplot,
           treat_warming, treat_competition, added_focals) |>
  summarise(n_repro = n(), .by = c(site, species,
                                   treat_warming, treat_competition, added_focals))
repro_counts


# calculate frequency of flowering per species and treatment ----------------------------
repro_frequency <- repro_counts |>
  left_join(
    subplots_surveyed |>
      summarise(n_subplots = n(),
                .by = c(site, treat_warming, treat_competition, added_focals)),
    by = c("site", "treat_warming", "treat_competition", "added_focals")
  ) |>
  mutate(prop_reproductive = n_repro / n_subplots)
repro_frequency


# mean flowering per treatment across species -----------------------------
repro_comm_treatment <- repro_frequency |>
  group_by(site, treat_warming, treat_competition, added_focals) |>
  summarise(
    mean_prop_reproductive = mean(prop_reproductive, na.rm = TRUE),
    sd_prop_reproductive   = sd(prop_reproductive, na.rm = TRUE),
    n_species              = n(),
    .groups = "drop"
  )
repro_comm_treatment


# number of reproductive species per treatment ----------------------------
repro_species_richness <- repro_frequency |>
  filter(prop_reproductive > 0) |>
  summarise(
    n_reproductive_species = n_distinct(species),
    .by = c(site, treat_warming, treat_competition, added_focals)
  )
repro_species_richness




prop_species_repro <- repro_frequency |>
  group_by(site, treat_warming, treat_competition, added_focals) |>
  summarise(
    prop_species_reproductive = mean(prop_reproductive > 0),
    .groups = "drop"
  )
prop_species_repro


species_flowering <- vege_nor_detail |>
  group_by(site, treat_warming, treat_competition, added_focals, species) |>
  summarise(
    flowering = any(reproductive_capacity == 1),
    .groups = "drop"
  )

perc_species_flowering <- species_flowering |>
  group_by(site, treat_warming, treat_competition, added_focals) |>
  summarise(
    percent_species_flowering = 100 * mean(flowering),
    n_species = n(),
    .groups = "drop"
  )
perc_species_flowering







# in datapaper ------------------------------------------------------------
# reproductive capacity ---------------------------------------------------
# What percentage of species observed in a treatment showed reproducitve structures at least once

perc_species_flowering <- vege_nor_detail |>
  group_by(site, treat_warming, treat_competition, added_focals, species) |>
  summarise(
    flowering = any(reproductive_capacity == 1),
    .groups = "drop"
  ) |>
  group_by(site, treat_warming, treat_competition, added_focals) |>
  summarise(
    percent_species_flowering = 100 * mean(flowering),
    n_species = n(),
    .groups = "drop"
  )
perc_species_flowering







