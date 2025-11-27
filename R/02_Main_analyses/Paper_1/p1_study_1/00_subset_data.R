#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#     
#                          2019
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
# 
#               ---- SUBSETTING DATA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


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
# 3. Subset data for Paper 1, Study 1
#----------------------------------------------------------# 
# subset pollen data to 25°W and 35°E longitude and north of 35°N latitude

data_p1_s1 <-
  data %>% 
  filter(long >= -25 & long <= 35,lat >= 35) 

min(data_p1_s1$lat) #min lat
max(data_p1_s1$lat) # max lat

min(data_p1_s1$long) #min long
max(data_p1_s1$long) # max long

#----------------------------------------------------------#
# 4. Divide subset data into sub-regions
#----------------------------------------------------------# 

# assign data points to sub-regions

data_p1_s1_sub_region <- data_p1_s1  %>%
  mutate(subregion = case_when(
    lat > 57 ~ "Boreal", 
    lat >= 45 & lat <= 47 & long <= 15 & long >= 5 ~ "Alps", 
    lat < 45 ~ "Meridional/Submeridional", 
    long < 11 ~ "Temperate Oceanic", 
    long >= 11 ~ "Temperate Continental"
  )) %>%
  relocate(subregion, .after = region) 

# visualize in map

data_p1_s1_sub_region %>% 
  ggplot(aes(x=long, y= lat, color = subregion)) +
  borders(fill= "gray", colour = "black") +
  geom_point() +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  )) +
  coord_quickmap(xlim = c(-25,35), ylim = c(35,75))


data_p1_s1_sub_region %>%
  count(subregion)# check subregions


data_p1_s1_sub_region %>% 
  filter(subregion == "Alps") %>% 
  unnest(levels) %>% 
  relocate(age,country) %>%
  distinct(country)

data_p1_s1_sub_region %>% 
  filter(subregion == "Boreal") %>%
  unnest(levels) %>%
  relocate(age,country) %>% 
  distinct(country)

data_p1_s1_sub_region %>% 
  filter(subregion == "Meridional/Submeridional") %>% 
  unnest(levels) %>%
  relocate(age,sample_id) %>%
  distinct(country)

data_p1_s1_sub_region %>%
  filter(subregion == "Temperate Continental") %>%
  unnest(levels) %>%
  relocate(age,country) %>%
  distinct(country)

data_p1_s1_sub_region %>%
  filter(subregion == "Temperate Oceanic") %>%
  unnest(levels) %>%
  relocate(age,country) %>%
  distinct(country)

#----------------------------------------------------------#
# 5. Get pollen counts with ages
#----------------------------------------------------------# 

data_p1_s1_subregion_counts_ages <- 
  data_p1_s1_sub_region %>% 
  get_pollen_counts_with_ages() 

data_p1_s1_subregion_counts_ages_subregion <- 
  data_p1_s1_subregion_counts_ages %>% 
  inner_join(data_p1_s1_sub_region, by = "dataset_id") %>% 
  select(dataset_id, age, taxa, pollen_counts, subregion) %>% 
  rename(taxon_name = taxa)


#----------------------------------------------------------#
# 6. Write the subset data to RDS file
#----------------------------------------------------------# 

write_rds(data_p1_s1_subregion_counts_ages_subregion, here("Data/Paper_1/data_subset/datasub_p1_s1_counts_ages.rds"))
