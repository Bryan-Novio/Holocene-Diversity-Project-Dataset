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

data_study3_harmonised_eu <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_eu.rds"))

data_study3_harmonised_na <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_na.rds"))

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

data_to_rarefy_eu <- 
  data_study3_harmonised_eu %>% 
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id,age, neotoma_names,pollen_counts) %>% 
  rename(taxon_name = neotoma_names) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_eu <-
  rarefy_all_samples(
    data_source = data_to_rarefy_eu,
    n_grains = 300
  )

data_to_rarefy_na <- 
  data_study3_harmonised_na %>% 
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id,age, neotoma_names,pollen_counts) %>% 
  rename(taxon_name = neotoma_names) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_na <-
  rarefy_all_samples(
    data_source = data_to_rarefy_na,
    n_grains = 300
  )


#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file
#----------------------------------------------------------# 

write_rds(rarefied_data_eu, here("Data/Paper_1/data_rarefy/data_study4_rarefied_eu.rds"))
write_rds(rarefied_data_na, here("Data/Paper_1/data_rarefy/data_study4_rarefied_na.rds"))
