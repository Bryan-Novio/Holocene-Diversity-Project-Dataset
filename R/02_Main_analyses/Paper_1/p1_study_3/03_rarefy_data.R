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

data_binned_eu <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_eu.rds"))

data_binned_na <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_na.rds"))

neotoma_taxa <- 
  readr::read_csv(
    here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"),
    show_col_types = FALSE
  )

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

data_to_rarefy_eu <- 
  data_binned_eu %>% 
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id, BIN, neotoma_names,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = neotoma_names) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

data_to_rarefy_na <- 
  data_binned_na %>% 
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id, BIN, neotoma_names,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = neotoma_names) %>% 
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

set.seed(1234)

rarefied_data_na <-
  rarefy_all_samples(
    data_source = data_to_rarefy_na,
    n_grains = 300
  )

#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file --------------
#----------------------------------------------------------#

write_rds(rarefied_data_eu, here("Data/Paper_1/data_rarefy/data_study3_rarefied_eu.rds"))

write_rds(rarefied_data_na, here("Data/Paper_1/data_rarefy/data_study3_rarefied_na.rds"))

