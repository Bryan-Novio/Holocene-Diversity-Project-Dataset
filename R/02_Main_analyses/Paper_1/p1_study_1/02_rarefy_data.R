#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#                          2019
#
# 
#                   ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_study1_harmonised <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study1_harmonised.rds"))

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

source_files <-
  sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

# at genus level only

data_to_rarefy <- 
  data_study1_harmonised  %>%
  select(taxa, age, pollen_counts, dataset_id) %>% 
  rename(taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  ) %>% 
  ungroup()

set.seed(1234)

rarefied_data <-
  rarefy_all_samples(
    data_source = data_to_rarefy,
    n_grains = 500
  )

#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file
#----------------------------------------------------------# 

write_rds(rarefied_data, here("Data/Paper_1/data_rarefy/data_study1_rarefied.rds"))
