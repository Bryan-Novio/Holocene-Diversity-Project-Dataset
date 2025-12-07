#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
#
#                  ----RAREFACTION  ----
#----------------------------------------------------------#


library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_study4_harmonised <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study4_harmonised.rds"))

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
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

data_to_rarefy_4 <- 
  data_study4_harmonised %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_4 <-
  rarefy_all_samples(
    data_source = data_to_rarefy_4,
    n_grains = 300
  )

#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file
#----------------------------------------------------------# 

write_rds(rarefied_data_4, here("Data/Paper_1/data_rarefy/data_study4_rarefied.rds"))

