
# RangeX LeafTraits data preparation NOR and CHE ------------

## Data used: 
##            
##            
## Date:      17.11.25
## Author:    Nadine Arzt
## Purpose:   Combine NOR and CHE data and prepare


# comment -----------------------------------------------------------------

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)


# import metadata ---------------------------------------------------------
metadata <- read.csv("Data/Metadata/RangeX_clean_MetadataFocal_all.csv")


# import LeafTrait data ---------------------------------------------------

leaftrait_che <- read.csv("Data/LeafTraits/RangeX_clean_LeafTraits_2022_2023_CHE.csv")

leaftrait_nor <- read.csv("Data/LeafTraits/RangeX_clean_LeafTraits_2023_NOR.csv")































