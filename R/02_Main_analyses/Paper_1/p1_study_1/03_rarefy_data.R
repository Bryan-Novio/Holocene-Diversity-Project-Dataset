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

data_study1_binned <- 
  read_rds(here("Data/Paper_1/data_bin/data_study1_binned.rds"))

neotoma_taxa <- 
  readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)


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

data_to_rarefy1 <- 
  data_study1_binned  %>%
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id,neotoma_names, BIN, summed_pollen_count) %>% 
  rename(taxon_name = neotoma_names, age = BIN, pollen_counts = summed_pollen_count) %>% 
  mutate(age = as.double(age)) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data1 <-
  rarefy_all_samples(
    data_source = data_to_rarefy1,
    n_grains = 500
  )

#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file
#----------------------------------------------------------# 

write_rds(rarefied_data1, here("Data/Paper_1/data_rarefy/data_study1_rarefied.rds"))
