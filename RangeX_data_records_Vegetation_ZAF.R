

# Figure out ZAF vege data ------------------------------------------------
theme_set(theme_bw(base_size  = 40))

source("RangeX_data_records_Vegetation_Surveys.R")
vege_zaf

# define focal species ----------------------------------------------------
focal_species_zaf <- c(
  "Leucosidea sericea",
  "Buddleja salviifolia",
  "Buddleja loricata",
  "Diospyros lycioides",
  "Cliffortia nitidula",
  "Cotoneaster pannosus",
  "Pyracantha angustifolia",
  "Rosa rubiginosa",
  "Melinis nerviglumis",
  "Eragrostis capensis",
  "Aristida junciformis",
  "Crinum bulbispermum",
  "Watsonia socialis",
  "Xerophyta viscosa",
  "Kniphofia linearifolia",
  "Dierama pauciflorum",
  "Scilla natalensis",
  "Aloe maculata",
  "Euphorbia clavarioides",
  "Euphorbia pulvinata",
  "Cotyledon orbiculata"
)


# summary of species per plot ---------------------------------------------
# how many total, focal, native and which
plot_species_summary <- vege_zaf |>
  distinct(unique_plot_id, species) |>
  mutate(is_focal = species %in% focal_species_zaf) |>
  group_by(unique_plot_id) |>
  summarise(
    n_species = n(),
    n_focal_species = sum(is_focal),
    n_native_species = n_species - n_focal_species,
    focal_species = paste(species[is_focal], collapse = ", "),
    native_species = paste(species[!is_focal], collapse = ", "),
    .groups = "drop"
  )
plot_species_summary


  

plot_species_summary2 <- plot_species_summary |>
  summarise(
    mean_native = mean(n_native_species),
    sd_native   = sd(n_native_species),
    min_native  = min(n_native_species),
    max_native  = max(n_native_species)
  )
plot_species_summary2

plot_species_summary |>
  select(unique_plot_id, n_native_species, n_focal_species) |>
  tidyr::pivot_longer(
    cols = c(n_native_species, n_focal_species),
    names_to = "group",
    values_to = "n_species"
  ) |>
  ggplot(aes(x = unique_plot_id, y = n_species, fill = group)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Plot",
    y = "Number of species",
    fill = "Species group"
  )


ggplot(plot_species_summary, aes(y = n_native_species)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  theme_bw() +
  labs(
    y = "Native species per plot",
    x = NULL
  )


















# include year ------------------------------------------------------------
plot_species_summary_year <- vege_zaf |>
  distinct(year, unique_plot_id, species) |>
  mutate(is_focal = species %in% focal_species_zaf) |>
  group_by(year, unique_plot_id) |>
  summarise(
    n_species = n(),
    n_focal_species = sum(is_focal),
    n_native_species = n_species - n_focal_species,
    focal_species = paste(species[is_focal], collapse = ", "),
    native_species = paste(species[!is_focal], collapse = ", "),
    .groups = "drop"
  )
plot_species_summary_year


zaf_species_number_focal_native_year <- plot_species_summary_year |>
  select(year, unique_plot_id, n_native_species, n_focal_species) |>
  tidyr::pivot_longer(
    cols = c(n_native_species, n_focal_species),
    names_to = "group",
    values_to = "n_species"
  ) |>
  ggplot(aes(x = unique_plot_id, y = n_species, fill = group)) +
  geom_col() +
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Plot",
    y = "Number of species",
    fill = "Species group"
  )+ 
  facet_grid(~year)+
  coord_flip()
zaf_species_number_focal_native_year


ggsave(filename = "RangeX_Vege_species_number_focal_native_year_per_plot_ZAF.png", 
       plot = zaf_species_number_focal_native_year, 
       path = "Data/VegSurveyGeneral/Graphs", 
       width = 25, height = 45)




# use whole zaf data ------------------------------------------------------
# where i did not filter out group = plot level
# but it does not make a difference because that is more like litter, bare soil...
# maybe the number of species is so low because they combined graminoids into all_grasses?
plot_species_summary_year2 <- com_zaf |>
  distinct(year, unique_plot_id, species) |>
  mutate(is_focal = species %in% focal_species_zaf) |>
  group_by(year, unique_plot_id) |>
  summarise(
    n_species = n(),
    n_focal_species = sum(is_focal),
    n_native_species = n_species - n_focal_species,
    focal_species = paste(species[is_focal], collapse = ", "),
    native_species = paste(species[!is_focal], collapse = ", "),
    .groups = "drop"
  )
plot_species_summary_year2


zaf_species_number_focal_native_year2 <- plot_species_summary_year |>
  select(year, unique_plot_id, n_native_species, n_focal_species) |>
  tidyr::pivot_longer(
    cols = c(n_native_species, n_focal_species),
    names_to = "group",
    values_to = "n_species"
  ) |>
  ggplot(aes(x = unique_plot_id, y = n_species, fill = group)) +
  geom_col() +
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 1)
  ) +
  labs(
    x = "Plot",
    y = "Number of species",
    fill = "Species group"
  )+ 
  facet_grid(~year)+
  coord_flip()
zaf_species_number_focal_native_year2


ggsave(filename = "RangeX_Vege_species_number_focal_native_year_per_plot_ZAF_2.png", 
       plot = zaf_species_number_focal_native_year, 
       path = "Data/VegSurveyGeneral/Graphs", 
       width = 25, height = 45)





# check if focal in species list of datapaper are matching focals in dataset --------
focal_from_data <- com_zaf |>
  filter(group == "focal") |>
  distinct(species) |>
  arrange(species)
focal_from_data


setdiff(focal_from_data$species, focal_species_zaf)   # in data, not in datapaper list
setdiff(focal_species_zaf, focal_from_data$species)   # in datapaper list, not in data

com_zaf |>
  distinct(species, group) |>
  filter(group == "focal") |>
  count(group)



setdiff(focal_species_zaf, focal_from_data$species)





