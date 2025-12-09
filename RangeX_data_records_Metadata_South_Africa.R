

# Create metadata plot level for ZAF --------------------------------------------


# import meta data focal --------------------------------------------------
meta_zaf <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_ZAF.csv")

meta_che <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_CHE.csv")
names(meta_che)

# fix meta_zaf ------------------------------------------------------------
meta_zaf <- meta_zaf |> 
  mutate(block_ID_original = NA,
         plot_ID_original = NA,
         added_focals = NA)

meta_zaf <- meta_zaf |> 
  rename(block_ID = block_id,
         position_ID_original = position_id_original,
         position_ID = position_id,
         unique_plot_ID = unique_plot_id,
         unique_plant_ID = unique_plant_id) |> 
  select(date_planting, region, site, block_ID_original, plot_ID_original, 
position_ID_original, species, functional_group, treat_warming, 
treat_competition, added_focals, block_ID, position_ID, 
ind_number, unique_plot_ID, unique_plant_ID)

# filter every plot once --------------------------------------------------
unique(meta_zaf$unique_plot_ID)

meta_p_zaf <- meta_zaf |> 
  distinct(unique_plot_ID, .keep_all = TRUE)

unique(meta_p_zaf$unique_plot_ID)

# select columns needed ---------------------------------------------------
meta_p_zaf <- meta_p_zaf |> 
  select(-species, -functional_group, -date_planting, -unique_plant_ID, -ind_number, -position_ID, -position_ID_original)

names(meta_p_zaf)

# save clean metadata ZAF plot --------------------------------------------
# write.csv(meta_p_zaf, file =  "Data/Metadata/RangeX_clean_MetadataPlot_ZAF.csv",
#           row.names = FALSE)




















