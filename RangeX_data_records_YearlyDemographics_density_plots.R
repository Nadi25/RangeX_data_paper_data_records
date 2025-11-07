
## RangeX YearlyDemographics CHE, CHN, NOR, ZAF density plots

## Data used: RangeX_data_records_YearlyDemographics.R
## Date:      04.11.25
## Author:    Nadine Arzt
## Purpose:   Make density plots for traits 


# comments ----------------------------------------------------------------
# size traits might have to be log transformed

# calculte mean for leaf_length1,2,3?

# load library ------------------------------------------------------------
library(ggridges)
library(ggpubr)
library(cowplot)
theme_set(theme_bw(base_size = 40))


# source script with joint dataset ----------------------------------------
source("RangeX_data_records_YearlyDemographics.R")
yearlydemo


# calculate mean for leaf_length ------------------------------------------
yearlydemo <- yearlydemo |> 
  mutate(leaf_length_mean = rowMeans(across(c(leaf_length1, leaf_length2, leaf_length3)),
                                     na.rm = TRUE))

# plot height_vegetative_str ----------------------------------------------
ggplot(yearlydemo, aes(x = height_vegetative_str, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "Vegetative height (structural)", y = "Density")


# plot height_reproductive_str -------------------------------------------------
ggplot(yearlydemo, aes(x = height_reproductive_str, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "height_reproductive_str", y = "Density")


# plot leaf_length1 ------------------------------------------------------------
ggplot(yearlydemo, aes(x = leaf_length1, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "leaf_length1", y = "Density")

# only CHE
ggplot(yearlydemo_che, aes(x = leaf_length3, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "leaf_length1", y = "Density")

# only NOR
ggplot(yearlydemo_nor, aes(x = leaf_length1, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "leaf_length1", y = "Density")


# two traits against each other ---------------------------------------------------------
# lenght vs width
ggplot(yearlydemo, aes(x = leaf_length1, y = leaf_width, fill = region)) +
  geom_point()+
  labs(x = "leaf_length1", y = "leaf_width")+
  facet_wrap(~ region)

# heights
ggplot(yearlydemo, aes(x = height_vegetative_str, y = height_vegetative, fill = region)) +
  geom_point()+
  labs(x = "height_vegetative_str", y = "height_vegetative")+
  facet_wrap(~ region)


ggplot(yearlydemo, aes(x = height_reproductive_str, y = height_reproductive, fill = region)) +
  geom_point()+
  labs(x = "height_reproductive_str", y = "height_reproductive")+
  facet_wrap(~ region)



# split into years --------------------------------------------------------
# plot height_vegetative_str ----------------------------------------------
ggplot(yearlydemo, aes(x = height_vegetative_str, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative_str", y = "Density")

ggplot(yearlydemo, aes(x = log(height_vegetative_str), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative_str", y = "Density")

# plot  height_vegetative ----------------------------------------------
ggplot(yearlydemo, aes(x = height_vegetative, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative", y = "Density")

ggplot(yearlydemo, aes(x = log(height_vegetative), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_vegetative", y = "Density")

# plot  height_reproductive_str ----------------------------------------------
ggplot(yearlydemo, aes(x = height_reproductive_str, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive_str", y = "Density")

ggplot(yearlydemo, aes(x = log(height_reproductive_str), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive_str", y = "Density")


# plot  height_reproductive ----------------------------------------------
ggplot(yearlydemo, aes(x = height_reproductive, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive", y = "Density")

ggplot(yearlydemo, aes(x = log(height_reproductive), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_reproductive", y = "Density")

# plot leaf_length1 ----------------------------------------------
ggplot(yearlydemo, aes(x = leaf_length1, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "leaf_length1", y = "Density")

ggplot(yearlydemo, aes(x = log(leaf_length1), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "leaf_length1", y = "Density")

ggplot(
  yearlydemo |> 
    filter(!is.na(leaf_length1), leaf_length1 > 0),  # remove NAs and zeros
  aes(x = log(leaf_length1), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  facet_wrap(~ year) +
  labs(x = "log(leaf_length1)", y = NULL)


# plot number_leaves ----------------------------------------------
ggplot(yearlydemo, aes(x = number_leaves, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_leaves", y = "Density")
# che hypmac 7000 leaves?

ggplot(yearlydemo, aes(x = log(number_leaves), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_leaves", y = "Density")


ggplot(
  yearlydemo |> 
    filter(!is.na(number_leaves), number_leaves > 0),  # remove NAs and zeros
  aes(x = log(number_leaves), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  facet_wrap(~ year) +
  labs(x = "log(number_leaves)", y = NULL)

  
# plot number_flowers ----------------------------------------------
ggplot(yearlydemo, aes(x = number_flowers, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_flowers", y = "Density")

ggplot(yearlydemo, aes(x = log(number_flowers), fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "number_flowers", y = "Density")




# height_total ------------------------------------------------------------
ggplot(yearlydemo, aes(x = height_total, fill = region)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ year) +
  labs(x = "height_total", y = "Density")




# all traits in pdf --------------------------------------------------------------
# Vector of traits
traits <- c(
  "height_vegetative_str", "height_reproductive_str",
  "height_vegetative", "height_reproductive", "vegetative_width",
  "height_total", "stem_diameter", "leaf_length1",
  "leaf_length2", "leaf_length3", "leaf_width",
  "petiole_length", "number_leaves", "number_tillers",
  "number_branches", "number_flowers", "mean_inflorescence_size"
)

# Create output folder
output_folder <- "Data/YearlyDemographics/Plots_density_ridges"
dir.create(output_folder, showWarnings = FALSE)

# Function to create and save the plot
plot_and_save <- function(trait) {
  p <- yearlydemo |> 
    filter(!is.na(.data[[trait]]), .data[[trait]] > 0) |> 
    ggplot(aes(x = log(.data[[trait]]), y = region, fill = region)) +
    geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
    facet_wrap(~ year) +
    labs(
      title = trait,
      x = paste0("log(", trait, ")"),
      y = NULL
    ) 
  
  ggsave(
    filename = file.path(output_folder, paste0(trait, ".pdf")),
    plot = p,
    width = 8, height = 6
  )
  
  return(p)
}

# Apply to all traits
plots <- map(traits, plot_and_save)


plot_density_ridges <- function(trait) {
  yearlydemo |> 
    filter(!is.na(.data[[trait]]), .data[[trait]] > 0) |> 
    ggplot(aes(x = log(.data[[trait]]), y = region, fill = region)) +
    geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
    facet_wrap(~ year) +
    labs(
      title = trait,
      x = paste0("log(", trait, ")"),
      y = NULL
    )
}

# Create multi-page PDF
pdf("Data/YearlyDemographics/Plots_density_ridges/Density_ridges_all_traits.pdf", width = 8, height = 6)
walk(traits, ~ print(plot_density_ridges(.x)))
dev.off()



# density ridges plots per trait ----------------------------------------------------

# colors per region -------------------------------------------------------
region_colors <- c("CHE" = "orange3","CHN" = "darkgreen","NOR" = "#00EEEE", "ZAF" = "red4")

# prepare plots for all traits separate  -------------------------------------------------------
t1 <- ggplot(
  yearlydemo |> filter(!is.na(height_vegetative_str), height_vegetative_str > 0),
  aes(x = log(height_vegetative_str), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(height_vegetative_str)", y = NULL)
t1

# t2
t2 <- ggplot(
  yearlydemo |> filter(!is.na(height_reproductive_str), height_reproductive_str > 0),
  aes(x = log(height_reproductive_str), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(height_reproductive_str)", y = NULL)
t2

# t3
t3 <- ggplot(
  yearlydemo |> filter(!is.na(height_vegetative), height_vegetative > 0),
  aes(x = log(height_vegetative), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(height_vegetative)", y = NULL)
t3

# t4
t4 <- ggplot(
  yearlydemo |> filter(!is.na(height_reproductive), height_reproductive > 0),
  aes(x = log(height_reproductive), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(height_reproductive)", y = NULL)
t4

# t5
t5 <- ggplot(
  yearlydemo |> filter(!is.na(vegetative_width), vegetative_width > 0),
  aes(x = log(vegetative_width), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(vegetative_width)", y = NULL)
t5

# t6
t6 <- ggplot(
  yearlydemo |> filter(!is.na(height_total), height_total > 0),
  aes(x = log(height_total), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(height_total)", y = NULL)
t6

# t7
t7 <- ggplot(
  yearlydemo |> filter(!is.na(stem_diameter), stem_diameter > 0),
  aes(x = log(stem_diameter), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(stem_diameter)", y = NULL)
t7

# t8
t8 <- ggplot(
  yearlydemo |> filter(!is.na(leaf_length1), leaf_length1 > 0),
  aes(x = log(leaf_length1), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(leaf_length1)", y = NULL)
t8

# t9
t9 <- ggplot(
  yearlydemo |> filter(!is.na(leaf_length2), leaf_length2 > 0),
  aes(x = log(leaf_length2), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(leaf_length2)", y = NULL)
t9

# t10
t10 <- ggplot(
  yearlydemo |> filter(!is.na(leaf_length3), leaf_length3 > 0),
  aes(x = log(leaf_length3), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(leaf_length3)", y = NULL)
t10

# t11
t11 <- ggplot(
  yearlydemo |> filter(!is.na(leaf_width), leaf_width > 0),
  aes(x = log(leaf_width), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(leaf_width)", y = NULL)
t11

# t12
t12 <- ggplot(
  yearlydemo |> filter(!is.na(petiole_length), petiole_length > 0),
  aes(x = log(petiole_length), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(petiole_length)", y = NULL)
t12

# t13
t13 <- ggplot(
  yearlydemo |> filter(!is.na(number_leaves), number_leaves > 0),
  aes(x = log(number_leaves), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(number_leaves)", y = NULL)
t13

# t14
t14 <- ggplot(
  yearlydemo |> filter(!is.na(number_tillers), number_tillers > 0),
  aes(x = log(number_tillers), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(number_tillers)", y = NULL)
t14

# t15
t15 <- ggplot(
  yearlydemo |> filter(!is.na(number_branches), number_branches > 0),
  aes(x = log(number_branches), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(number_branches)", y = NULL)
t15

# t16
t16 <- ggplot(
  yearlydemo |> filter(!is.na(number_flowers), number_flowers > 0),
  aes(x = log(number_flowers), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(number_flowers)", y = NULL)
t16

# t17
t17 <- ggplot(
  yearlydemo |> filter(!is.na(mean_inflorescence_size), mean_inflorescence_size > 0),
  aes(x = log(mean_inflorescence_size), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(mean_inflorescence_size)", y = NULL)
t17

# t18
t18 <- ggplot(
  yearlydemo |> filter(!is.na(leaf_length_mean), leaf_length_mean > 0),
  aes(x = log(leaf_length_mean), y = region, fill = region)) +
  geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
  scale_fill_manual(values = region_colors)+
  labs(x = "log(leaf_length_mean)", y = NULL)
t18


all_traits <- ggarrange(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, 
                        t14, t15, t16, t17, common.legend = TRUE, legend= "right", align = "v")
  #annotate_figure(left = text_grob("Density",rot = 90))
all_traits





# plot all traits faster and shorter ------------------------------------------------------
plot_trait <- function(data, trait) {
  ggplot(data |> filter(!is.na(.data[[trait]]), .data[[trait]] > 0),
         aes(x = log(.data[[trait]]), y = region, fill = region)) +
    geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
    scale_fill_manual(values = region_colors) +
    labs(x = paste0("log(", trait, ")"), y = NULL)
}

traits <- c(
  "height_vegetative_str", "height_reproductive_str", "height_vegetative",
  "height_reproductive", "vegetative_width", "height_total", "stem_diameter",
  "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length",
  "number_leaves", "number_tillers", "number_branches", "number_flowers",
  "mean_inflorescence_size"
)

plots <- lapply(traits, \(x) plot_trait(yearlydemo, x))

all_traits <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE, legend = "right", align = "v")



# final plot all traits with one common legend  ---------------------------
# Remove legends from all t1–t17 plots
plots_no_legend <- lapply(list(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, 
                               t11, t12, t13, t14, t15, t16, t17),
                          function(p) p + theme(legend.position = "none"))

# Extract legend from t8 (which has all regions)
legend_t8 <- ggpubr::get_legend(t8)

# Combine all plots with one shared legend
all_traits <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = plots_no_legend, ncol = 6, nrow = 3),
  legend_t8,
  rel_widths = c(1, 0.1)
)

all_traits


ggsave(filename = "RangeX_YearlyDemographics_all_traits_regions_years_density_plots.png", 
       plot = all_traits, 
       path = "Data/YearlyDemographics/Plots_density_ridges/", 
       width = 45, height = 25)



# plot --------------------------------------------------------------------

traits2 <- c(
  "height_vegetative_str", "height_reproductive_str", "height_vegetative",
  "height_reproductive", "vegetative_width", "height_total", "stem_diameter",
  "leaf_length_mean", "leaf_width", "petiole_length",
  "number_leaves", "number_tillers", "number_branches", "number_flowers",
  "mean_inflorescence_size"
)

plots <- lapply(traits2, \(x) plot_trait(yearlydemo, x))

all_traits2 <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE, legend = "right", align = "v")

all_traits2


# final plot all traits with one common legend  ---------------------------
# Remove legends from all t1–t17 plots
plots_no_legend2 <- lapply(list(t1, t2, t3, t4, t5, t6, t7, t18,
                               t11, t12, t13, t14, t15, t16, t17),
                          function(p) p + theme(legend.position = "none"))

# Extract legend from t8 (which has all regions)
legend_t18 <- ggpubr::get_legend(t18)

# Combine all plots with one shared legend
all_traits2 <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = plots_no_legend2, ncol = 4),
  legend_t18,
  rel_widths = c(1, 0.1)
)

all_traits2


ggsave(filename = "RangeX_YearlyDemographics_all_traits_regions_years_density_plots2.png", 
       plot = all_traits2, 
       path = "Data/YearlyDemographics/Plots_density_ridges/", 
       width = 40, height = 30)


all_traits2_labeled <- ggdraw() +
  draw_plot(all_traits2) +
  #draw_label("Traits", x = 0.5, y = 0, vjust = -1, angle = 0, size = 18) +  # x-axis
  draw_label("Density", x = 0, y = 0.5, vjust = 1.5, angle = 90, size = 45)  # y-axis

all_traits2_labeled

ggsave(filename = "RangeX_YearlyDemographics_all_traits_regions_years_density_plots3.png", 
       plot = all_traits2_labeled, 
       path = "Data/YearlyDemographics/Plots_density_ridges/", 
       width = 40, height = 40)









# Boxplot out of interest --------------------------------------------------
yearlydemo_long <- yearlydemo |> 
  pivot_longer(
    cols = all_of(traits2),   # use your traits2 vector
    names_to = "trait",
    values_to = "value"
  )

# quick plot of traits across regions
ggplot(yearlydemo_long, aes(x = region, y = value, fill = region)) +
  geom_boxplot() +
  facet_wrap(~trait, scales = "free_y") +
  theme_bw() +
  labs(x = "Region", y = "Trait value") +
  theme(legend.position = "none")







