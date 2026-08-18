
#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#                          2019
#
# 
#               ---- TREND VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <-
  read_csv(here("Data/Paper_1/data_estimate_richness/study1_richness.csv"))

data_p1_s1_counts_ages_subregion <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s1_counts_ages.rds"))

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

data_subregion <- 
  data_p1_s1_counts_ages_subregion %>%
  distinct(subregion, dataset_id)

richness_subregion <-
  left_join(richness , data_subregion, by ="dataset_id") 


# compute median richness

median_richness_data <-
  richness_subregion  %>%
  group_by(age, subregion) %>%
  summarise(
    median_richness = median(richness, na.rm = TRUE),
    .groups = "drop"
  )

median_richness_15k <-
  median_richness_data %>% filter(age <= 15000)


summary(median_richness_15k)

## same

ggplot(median_richness_15k, aes(x = age, y = median_richness, color = subregion)) +
  geom_line(linewidth = 1) +
  scale_x_reverse(
    breaks = seq(0, 15000, by = 1000),
    labels = function(x) {
      if_else(x %in% c(0, 3000, 6000, 9000, 12000, 15000), as.character(x), "")
    }
  ) +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(median_richness_15k, aes(x = age, y = median_richness, color = subregion)) +
  annotate("rect", xmin = Inf, xmax = 11500, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 11500, xmax = 8500, ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.2) +
  annotate("rect", xmin = 8500, xmax = 4500, ymin = -Inf, ymax = Inf, fill = "lightyellow", alpha = 0.2) +
  annotate("rect", xmin = 4500, xmax = 0, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  geom_line(size = 1) +
  scale_x_reverse(
    breaks = seq(0, 15000, by = 1000),
    labels = function(x) {
      if_else(x %in% c(0, 3000, 6000, 9000, 12000, 15000), as.character(x), "")
    }
  ) +
  scale_y_continuous(breaks = c(0,30, 40, 50)) +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = expression(Median~site~richness~(ET[500]))) +
  labs(x = "Age in years ago")

