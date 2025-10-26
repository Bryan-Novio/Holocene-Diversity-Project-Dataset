#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 2: Simova et al
#
#
#                          2023
# North America, site-based richness (dataset_id,age,
# 1000 bins - rarefy 400
#
#                  ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(vegan)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_study2_harmonised <- read_rds(here("Outputs/Data/paper_1_study_2/data_study2_harmonised.rds"))

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
# 3. Rarefy data  at different taxo rank ------------------
#----------------------------------------------------------#

data_to_rarefy_1 <- 
  data_study2_harmonised %>%
  select(dataset_id,sample_id, taxa, pollen_counts) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  ) %>% 
  nest(samples = c(sample_id, taxa, pollen_counts)
  ) %>% 
  unnest(samples) %>% 
  pivot_wider(
    names_from = taxa,
    values_from = pollen_counts
  ) 
  
data_to_rarefy_2 <- 
  data_to_rarefy_1 %>% 
  nest(samples = c(,2:106)
  ) 
  
set.seed(1234)

rarefied_data <-
  data_to_rarefy_2 %>%
  rarefy_all_samples(400)

rarefied_data %>% unnest(data)
  
#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file----------------
#----------------------------------------------------------#

write_rds(data_to_rarefy_2, here("Outputs/Data/paper_1_study_2/data_to_rarefy_study_2.rds"))
write_rds(rarefied_data, here("Outputs/Data/paper_1_study_2/rarefied_data_study_2.rds"))
