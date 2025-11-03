
## RangeX YearlyDemographics CHE, CHN, NOR, ZAF

## Data used: RangeX_clean_YearlyDemographics_2021_2022_2023_CHN.csv,
##            RangeX_clean_YearlyDemographics_2021_2023_CHE.csv,
##            RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv,
##            RangeX_YearlyDemographic_ZAF.csv
## Date:      26.10.23
## Author:    Nadine Arzt
## Purpose:   combine traits data 2021 + 2022 + 2023 from all regions for data  
##            records


# load packages -----------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)

yd_zaf <- read.csv("Data/YearlyDemographics/RangeX_YearlyDemographic_ZAF.csv")
yd_chn <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_CHN.csv")
yd_nor <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2022_2023_NOR.csv")
yd_che <- read.csv("Data/YearlyDemographics/RangeX_clean_YearlyDemographics_2021_2023_CHE.csv")



























