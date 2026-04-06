#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#                 ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(vegan)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_harmonised_merge <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_data_harmonised_merge.rds"))

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
# 3. Rarefy data  fo 1000 iterations --
#----------------------------------------------------------#

## 3.1.Prepare data for rarefaction

data_to_rarefy <-   # 1001 dataset_ids
  data_harmonised_merge %>% 
  select(dataset_id, age, taxa, pollen_counts ) %>% 
  rename(taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )


## 3.2. Rarefy iteratively

set.seed(1234)

rarefied_dataset_assembly <- 
  data_to_rarefy %>% 
  rarefy_all_samples_iter(
    n_iter = 1000,
    path = here::here("Data/Paper_1/data_rarefy/iterations")) 



