
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
  median_richness_data %>% filter(age <= 15000) %>% 
  rename(Subregion = subregion)


summary(median_richness_15k)

write_csv(median_richness_15k,here("Data/Paper_1/data_model/model_csvs/S1_Richness.csv") )


## same

ggplot(median_richness_15k, aes(x = age, y = median_richness, color = Subregion)) +
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


ggplot(median_richness_15k, aes(x = age, y = median_richness, color = Subregion)) +
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



##plot anew

median_richness_15k %>% 
  ggplot2::ggplot(aes(x = age , y = median_richness)) +
  ggplot2::geom_line(aes(colour = Subregion), linewidth = 3)  +
  ggplot2::theme(axis.text.x = element_text(size = 25, color = "black", angle = 90, vjust = 0.5),
                 axis.text.y.right = element_text(size = 25, color = "black", angle = 90, vjust = 0.5, hjust = 0.5),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 panel.background = element_blank(),
                 axis.line.y.right  = element_line(color = "black", linewidth = 1),
                 axis.line.x.bottom  = element_line(color = "black", linewidth = 1),
                 axis.ticks.x = element_line(linewidth = 1),
                 axis.ticks.y = element_line(linewidth = 1),
                 axis.ticks.length.y.right = unit(.25, "cm"),
                 axis.ticks.length.x.bottom = unit(.25, "cm"),
                 legend.text = element_blank(),
                 legend.position = "none")   +
  scale_y_continuous(breaks = seq(0, 15000, by = 1000), position = "right") +
  ggplot2::scale_x_continuous(breaks = c(0,5000,10000,15000), limits =c(0,15000)) +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  ))






###with original


s1_new <- s1_new %>% 
  mutate(Subregion = fct_relevel(Subregion, c("Alps", "Boreal", "Meridional/Submeridional","Temperate Continental","Temperate Oceanic"))) %>%
  arrange(Subregion)


median_richness_15k <- median_richness_15k %>% 
  mutate(Subregion = as.factor(Subregion))



ggplot() +
  geom_line(data = median_richness_15k, aes(x = age, y = median_richness, color = Subregion),linewidth = 2) +
 # geom_line(data = s1_new , aes(x = age, y = richness, color = Subregion),
       #     ,linewidth = 1, linetype = 2) +
  mutate(Subregion = fct_relevel(Subregion, c("Alps", "Boreal", "Meridional/Submeridional","Temperate Continental","Temperate Oceanic")))
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
  labs (y = "Richness", x = "Age (cal yr BP)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 15))
  
  


### plot per biome
  
library (ggh4x)

ggplot() +
# geom_line(data = s1_new,(aes(x = age, y = richness)),linewidth = 0.7, linetype = 2, color = "purple") +
geom_line(data = median_richness_15k,aes( x = age, y = median_richness
  ),linewidth = 2, color =  "purple") +
  facet_wrap2(~Subregion,
             strip = strip_themed(
               background_x = elem_list_rect(fill = sub_cols)
             )) +
  scale_x_reverse(
    breaks = seq(0, 15000, by = 1000),
    labels = function(x) {
      if_else(x %in% c(0, 3000, 6000, 9000, 12000, 15000), as.character(x), "")
    }
  )  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1.5,"lines"),
        strip.text = element_text(size = 18, colour = "white"),
        strip.text.x = element_text (size = 12),
        strip.text.y = element_text (size = 12),
        strip.background = element_rect(linewidth = 0, colour = NA),
        strip.background.y = element_blank(),
        axis.text.x.bottom = element_text(size = 12),
        axis.text.y.left = element_text(size = 12),
        axis.title.x.bottom = element_text(size = 15),
        axis.title.y.left = element_text(size = 15),
        ) +
  labs(y = expression(Median~site~richness~(ET[500]))) +
  labs(x = "Age (cal yr BP)")

sub_cols <- c(
  "black",
  "darkgreen",
  "red",
  "orange",
  "blue"
)


sub_cols <- as.character(sub_cols)

scale_color_manual(values = c(
  "Alps" = "black",
  "Boreal" = "darkgreen",
  "Meridional/Submeridional" = "red",
  "Temperate Continental" = "orange",
  "Temperate Oceanic" = "blue"
))



