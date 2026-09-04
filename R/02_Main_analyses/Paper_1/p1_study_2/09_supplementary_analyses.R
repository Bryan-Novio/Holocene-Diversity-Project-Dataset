#----------------------------------------------------------#     
#               
#               Holocene Diversity Project
#
#
#            Paper01| Method 2: Simova et al
#
#                       
#                          2023
# North America, site-based richness (dataset_id,age, 
# 1000 bins - rarefy 400 
#
#
#               ---- SUBSETTING DATA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)


#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))
richness_re <- read_rds(here("Outputs/Data/paper_1_study_2/richness_data_study_2_re.rds"))


data_p1_s2 <- data %>% 
  filter(region =="North America") %>%   # sub-setting data to N.America
  relocate(region) 



min(data_p1_s2$long)
max(data_p1_s2$long)
min(data_p1_s2$lat)
max(data_p1_s2$lat)


data_p1_s2 %>% 
  ggplot(aes(x=long, y= lat)) +
  borders(fill= NA, colour = "black") +
  geom_point(
  ) +
  coord_quickmap(xlim = c(-172, -53), ylim = c(28,74))


#----------------------------------------------------------#
# 2. Load functions ---------------------------------------
#----------------------------------------------------------#

# Get a vector of general functions

fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "\\.R$",
    recursive = TRUE
  )

# Load the function into the global environment

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 2. Other visualizations ---------------------------------
#----------------------------------------------------------#

# divide dataset to E-W cluster

cluster <- data_p1_s2 %>% select(dataset_id, long, lat) %>% 
  mutate(cluster = case_when(
    long < -104 ~ "West",
    long >= -104 ~ "East"))

data_richness_cluster <- inner_join(richness_re, cluster, by="dataset_id")


#View spatial distribution

data_richness_cluster %>% 
  ggplot(aes(x=long, y= lat, color = cluster)) +
  borders(fill= NA, colour = "black") +
  geom_point() +
  coord_quickmap(xlim = c(-172, -53), ylim = c(28,74))

# Plot richness by cluster

ggplot(data_richness_cluster, aes(x = age, y = richness, color = cluster, fill = cluster)) +
  geom_smooth(method = "gam", colour ="black") +
  scale_x_reverse(limits = c(20000, 1000), breaks = c(20000, 15000, 10000, 5000, 0), labels = c(20, 15, 10, 5, 0)) +
  scale_fill_manual(values = c("East" = "royalblue", "West" = "coral3")) +
  scale_color_manual(values = c("East" = "royalblue", "West" = "coral3")) +
  geom_vline(xintercept = 9800, color ="red") +
  theme_classic()

# plot richness by site

data_richness_cluster %>% 
  ggplot(aes(x = age, y = richness, group = dataset_id, color = long)) +
  geom_smooth(method = "gam",  formula = y ~ s(x, k=12),se = FALSE)+
  scale_x_reverse(limits = c(20000, 1000), breaks = c(20000, 15000, 10000, 5000, 0), labels = c(20, 15, 10, 5, 0)) + 
  theme_classic() +
  scale_color_gradient(high = "cadetblue1", low ="blue4")



############ spatial distribution of site ids 


# RaW (bigger circ)


raw <- data %>% 
  filter(region =="North America")  %>% 
  select(dataset_id, long, lat)


# Analyzed (smaller circ)


richness_data <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))


an <- richness_data %>% 
  distinct(dataset_id) %>% 
  mutate(dataset_id = as.character(dataset_id))

an_coord <- 
  left_join(an, raw, by = "dataset_id")

raw_plus_an <- bind_rows(raw,an_coord,.id = "data")

raw_plus_an <- raw_plus_an %>% 
  mutate(data = fct_recode(data ,  Raw = "1", Analyzed = "2"))


# Plot #raw 472- analyzed 451


raw_plus_an %>% 
  ggplot(aes(x = long, y = lat)) + 
  borders(fill= "gray") +
  geom_point(aes(colour = data, alpha = 0.1, size = factor(data,levels = c("Analyzed","Raw")))) +
  coord_quickmap(xlim = c(-172, -56), ylim = c(28,74))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "gray"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

