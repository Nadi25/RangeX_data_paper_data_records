
## RangeX YearlyDemographics CHE, CHN, NOR, ZAF data exploration

## Data used: RangeX_data_records_YearlyDemographics.R
## Date:      04.11.25
## Author:    Nadine Arzt
## Purpose:   Explore YearlyDemographics trait data 
##            How many samples? per region? 


# comments ----------------------------------------------------------------



# load library ------------------------------------------------------------



# source script with joint dataset ----------------------------------------
source("RangeX_data_records_YearlyDemographics.R")
glimpse(yearlydemo)
# 24270 observations



# samples per region ------------------------------------------------------
count_region <- yearlydemo |> 
  count(region)
count_region


count <- yearlydemo |> 
  count(region, site, year)
count


# count per region and year per trait -------------------------------------
count_trait <- yearlydemo |> 
  group_by(region, year) |> 
  summarise(across(where(is.numeric),
                   list(n = ~sum(!is.na(.x)),
                        min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
count_trait



# all regions together per year
count_trait_all_regions <- yearlydemo |> 
  group_by(year) |> 
  summarise(across(where(is.numeric),
                   list(n = ~sum(!is.na(.x)),
                        min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
count_trait_all_regions



# all regions and all years together
count_trait_all_regions_year <- yearlydemo |> 
  #group_by(year) |> 
  summarise(across(where(is.numeric),
                   list(n = ~sum(!is.na(.x)),
                        min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))
count_trait_all_regions_year





