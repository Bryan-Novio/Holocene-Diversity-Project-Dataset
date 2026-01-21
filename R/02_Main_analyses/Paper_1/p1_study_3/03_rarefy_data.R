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

data_binned_asia <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_asia.rds"))

data_binned_europe <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_europe.rds"))

data_binned_namerica <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_namerica.rds"))

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

##Asia

data_to_rarefy_asia <- 
  data_binned_asia %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(dataset_id, BIN, taxa,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )
  
set.seed(1234)

rarefied_data_asia <-
  rarefy_all_samples(
    data_source = data_to_rarefy_asia,
    n_grains = 300
  )


## Europe

data_to_rarefy_europe <- 
  data_binned_europe %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(dataset_id, BIN, taxa,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_europe <-
  rarefy_all_samples(
    data_source = data_to_rarefy_europe,
    n_grains = 300)

##NAmerica

data_to_rarefy_namerica <- 
  data_binned_namerica %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(dataset_id, BIN, taxa,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_namerica <-
  rarefy_all_samples(
    data_source = data_to_rarefy_namerica,
    n_grains = 300)


#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file --------------
#----------------------------------------------------------#

write_rds(rarefied_data_asia, here("Data/Paper_1/data_rarefy/data_study3_rarefied_asia.rds"))

write_rds(rarefied_data_europe, here("Data/Paper_1/data_rarefy/data_study3_rarefied_europe.rds"))

write_rds(rarefied_data_namerica, here("Data/Paper_1/data_rarefy/data_study3_rarefied_namerica.rds"))
