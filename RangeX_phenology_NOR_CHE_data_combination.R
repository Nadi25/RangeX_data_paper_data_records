
# RangeX phenology data preparation NOR and CHE ------------

## Data used: RangeX_clean_Phenology_2022_CHE.csv
##            RangeX_clean_Phenology_2023_NOR.csv
##            RangeX_clean_MetadataFocal_CHE.csv
##            RangeX_clean_MetadataFocal_NOR.csv
## Date:      14.08.25
## Author:    Nadine Arzt
## Purpose:   Combine NOR and CHE data and prepare


# comment -----------------------------------------------------------------
# CHE Flowers withered corresponds to infructescences in NOR

# CHE.lo.ambi.bare.wf.10.22.1 silvul 2022-06-02 EI No_Buds 11 --> should be 1

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(purrr)


# import phenology data CHE ---------------------------------------
# Read as a single column of text
raw <- read_lines("Data/Phenology/RangeX_clean_Phenology_2022_CHE.csv")

# Remove the outer wrapping quotes and fix doubled quotes
raw <- gsub('^"|"$', '', raw)   # remove first and last quote in each line
raw <- gsub('""', '"', raw)     # replace doubled quotes with single quotes

# Write a cleaned temporary CSV
write_lines(raw, "Data/Phenology/RangeX_clean_Phenology_2022_CHE_clean.csv")

# Now read normally
pheno_che <- read_csv("Data/Phenology/RangeX_clean_Phenology_2022_CHE_clean.csv")

# import phenology data NOR ---------------------------------------
pheno_nor <- read.csv("Data/Phenology/RangeX_clean_Phenology_2023_NOR.csv")

# import metadata CHE -----------------------------------------------------
meta_CHE <- read_csv("Data/Metadata/RangeX_clean_MetadataFocal_CHE.csv")

# import metadata NOR -----------------------------------------------------
meta_NOR <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_NOR.csv")


# merge metadata with phenology -------------------------------------------
# CHE
pheno_22_CHE <- pheno_che |> 
  left_join(meta_CHE, by = c("unique_plant_ID", "species"))

# NOR
pheno_23_NOR <- pheno_nor |> 
  left_join(meta_NOR, by = c("unique_plant_ID", "species"))


# one phenology data set -------------------------------------------------
# combine CHE and NOR
phenology <- rbind(pheno_22_CHE, pheno_23_NOR)


# rename pheno stages to match regions ------------------------------------
# "number_infructescences" in NOR correpsonfs to "No_FloWithrd" in CHE?
phenology <- phenology |>
  mutate(phenology_stage = recode(phenology_stage,
                                  "number_buds" = "No_Buds",
                                  "number_flowers" = "No_FloOpen",
                                  #"number_infructescences" = "No_FloWithrd",
                                  "seeds_collected" = "No_Seedheads"))


# fix typo in che ---------------------------------------------------------
# CHE.lo.ambi.bare.wf.10.22.1 silvul 2022-06-02 EI No_Buds 11 --> should be 1
phenology <- phenology |>
  mutate(value = replace(value,
                         unique_plant_ID == "CHE.lo.ambi.bare.wf.10.22.1" &
                           value == 11, 1))



# save joint data set -----------------------------------------------------
write.csv(phenology, file = "Data/Phenology/RangeX_clean_Phenology_2022_2023_NOR_CHE.csv", row.names = FALSE)

p <- read.csv("Data/Phenology/RangeX_clean_Phenology_2022_2023_NOR_CHE.csv")










