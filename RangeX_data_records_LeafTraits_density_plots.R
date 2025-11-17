
## RangeX LeafTraits CHE, NOR density plots

## Data used: RangeX_data_records_LeafTraits.R
## Date:      17.11.25
## Author:    Nadine Arzt
## Purpose:   Make density plots for leaf traits 


# comments ----------------------------------------------------------------


# load library ------------------------------------------------------------
library(ggridges)
library(ggpubr)
library(cowplot)
theme_set(theme_bw(base_size = 40))


# source script with joint dataset ----------------------------------------
source("RangeX_data_records_LeafTraits.R")
leaf_traits



# plot all traits faster and shorter ------------------------------------------------------
# colors per region -------------------------------------------------------
region_colors <- c("CHE" = "orange3","CHN" = "darkgreen","NOR" = "#00EEEE", "ZAF" = "red4")

plot_trait <- function(data, trait) {
  ggplot(data |> filter(!is.na(.data[[trait]]), .data[[trait]] > 0),
         aes(x = log(.data[[trait]]), y = region, fill = region)) +
    geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
    scale_fill_manual(values = region_colors) +
    labs(x = paste0("log(", trait, ")"), y = NULL)
}

dput(colnames(leaf_traits))

traits <- c("wet_mass", 
            "dry_mass", "leaf_thickness", "leaf_area", "SLA", "LDMC", "sto_density_top", 
            "sto_density_bot")

plots <- lapply(traits, \(x) plot_trait(leaf_traits, x))

all_traits <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE, legend = "right", align = "v")

# Combine all plots with one shared legend
all_traits <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = plots_no_legend, ncol = 6, nrow = 3),
  #legend_t8,
  rel_widths = c(1, 0.1)
)
all_traits

ggsave(filename = "RangeX_LeafTraits_all_traits_regions_density_plots.png", 
       plot = all_traits, 
       path = "Data/LeafTraits/Plots_density_ridges/", 
       width = 45, height = 25)


# plot where SLA and LDMC are not log transformed -------------------------
# Traits that need log-transform
log_traits <- c("wet_mass", "dry_mass", "leaf_area",
                "leaf_thickness", "sto_density_top",
                "sto_density_bot")

# All traits to plot
traits <- c("wet_mass", "dry_mass", "leaf_thickness", "leaf_area",
            "SLA", "LDMC", "sto_density_top", "sto_density_bot")


# ----- Plot function ---------------------------------------------------------
plot_trait <- function(data, trait) {
  
  data_plot <- data |>
    filter(!is.na(.data[[trait]]), .data[[trait]] > 0) |>
    mutate(
      x_var = if (trait %in% log_traits) log(.data[[trait]]) else .data[[trait]]
    )
  
  ggplot(data_plot,
         aes(x = x_var, y = region, fill = region)) +
    geom_density_ridges(alpha = 0.4, scale = 10, linewidth = 0.5) +
    scale_fill_manual(values = region_colors) +
    labs(
      x = if (trait %in% log_traits) paste0("log(", trait, ")") else trait,
      y = NULL
    )
}

# ----- Generate plots --------------------------------------------------------
plots <- lapply(traits, function(tr) plot_trait(leaf_traits, tr))

# Arrange all in a grid with shared legend
all_traits2 <- ggpubr::ggarrange(
  plotlist = plots,
  common.legend = TRUE,
  legend = "right",
  align = "v",
  ncol = 2,
  nrow = 4
)

all_traits2

ggsave(filename = "RangeX_LeafTraits_all_traits_regions_density_plots.png", 
       plot = all_traits2, 
       path = "Data/LeafTraits/Plots_density_ridges/", 
       width = 35, height = 30)

