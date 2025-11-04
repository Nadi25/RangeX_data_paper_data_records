
## RangeX YearlyDemographics CHE, CHN, NOR, ZAF density plots

## Data used: RangeX_data_records_YearlyDemographics.R
## Date:      04.11.25
## Author:    Nadine Arzt
## Purpose:   Make density plots for traits 


# comments ----------------------------------------------------------------
# size traits might have to be log transformed


# load library ------------------------------------------------------------
library(ggridges)
theme_set(theme_bw())


# source script with joint dataset ----------------------------------------
source("RangeX_data_records_YearlyDemographics.R")
yearlydemo


# # make columns numeric ----------------------------------------------------
# yearlydemo <- yearlydemo |> 
#   mutate(height_vegetative_str = as.numeric(height_vegetative_str)) |> 
#   mutate(leaf_length1 = as.numeric(leaf_length1))



# plot height_vegetative_str ----------------------------------------------
ggplot(yearlydemo, aes(x = height_vegetative_str, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "Vegetative height (structural)", y = "Density")


# plot leaf_length1 ------------------------------------------------------------
ggplot(yearlydemo, aes(x = leaf_length1, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "leaf_length1", y = "Density")


# plot height_reproductive_str -------------------------------------------------
ggplot(yearlydemo, aes(x = height_reproductive_str, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "height_reproductive_str", y = "Density")






# split into years --------------------------------------------------------

# add column year ---------------------------------------------------------
yearly_demo <- yearlydemo |> 
  mutate(year = format(date_measurement, "%Y"))


# plot height_vegetative_str ----------------------------------------------
ggplot(yearly_demo, aes(x = height_vegetative_str, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative_str", y = "Density")

ggplot(yearly_demo, aes(x = log(height_vegetative_str), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative_str", y = "Density")

# plot  height_vegetative ----------------------------------------------
ggplot(yearly_demo, aes(x = height_vegetative, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative", y = "Density")

ggplot(yearly_demo, aes(x = log(height_vegetative), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative", y = "Density")

# plot  height_reproductive_str ----------------------------------------------
ggplot(yearly_demo, aes(x = height_reproductive_str, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive_str", y = "Density")

ggplot(yearly_demo, aes(x = log(height_reproductive_str), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive_str", y = "Density")


# plot  height_reproductive ----------------------------------------------
ggplot(yearly_demo, aes(x = height_reproductive, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive", y = "Density")

ggplot(yearly_demo, aes(x = log(height_reproductive), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive", y = "Density")

# plot leaf_length1 ----------------------------------------------
ggplot(yearly_demo, aes(x = leaf_length1, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "leaf_length1", y = "Density")

ggplot(yearly_demo, aes(x = log(leaf_length1), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "leaf_length1", y = "Density")

# plot number_leaves ----------------------------------------------
ggplot(yearly_demo, aes(x = number_leaves, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_leaves", y = "Density")
# che hypmac 7000 leaves?

ggplot(yearly_demo, aes(x = log(number_leaves), fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "number_leaves", y = "Density")


ggplot(
  yearly_demo |> 
    filter(!is.na(number_leaves), number_leaves > 0),  # remove NAs and zeros
  aes(x = log(number_leaves), y = region, fill = region)
) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  facet_wrap(~ year) +
  labs(x = "log(number_leaves)", y = NULL)

  
# plot number_flowers ----------------------------------------------
ggplot(yearly_demo, aes(x = number_flowers, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_flowers", y = "Density")

ggplot(yearly_demo, aes(x = log(number_flowers), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_flowers", y = "Density")




# height_total ------------------------------------------------------------
ggplot(yearly_demo, aes(x = height_total, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_total", y = "Density")















