#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America & Europe, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#                 ----HARMONIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s3_eu <-  
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_EU_counts_ages.rds"))

pollen_data_s3_na <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_NA_counts_ages.rds"))

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names)

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
# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

data_to_harmonize <- 
  pollen_data_s3_eu %>% 
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)

data_to_harmonize2 <- 
  pollen_data_s3_na %>% 
  inner_join(neotoma_taxa, join_by(taxa == taxon_name)) %>% 
  select(dataset_id, sample_id,  age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)

# Harmonize taxa at different taxonomic levels

data_study3_harmonised_eu <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize,
    harmonisation_table = harmonisation_table,
    level = "level_6") %>% 
  inner_join(neotoma_taxa, join_by(taxon_name == neotoma_names)) %>% 
  select(taxon_name.y,dataset_id, pollen_counts,age) %>% 
  rename(taxa = taxon_name.y)
  

data_study3_harmonised_na <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize2,
    harmonisation_table = harmonisation_table,
    level = "level_6") %>% 
  inner_join(neotoma_taxa, join_by(taxon_name == neotoma_names)) %>% 
  select(taxon_name.y,dataset_id,pollen_counts,age) %>% 
  rename(taxa = taxon_name.y)

#----------------------------------------------------------#
# 1. Write the harmonized data to RDS files ----------------
#----------------------------------------------------------# 

write_rds(data_study3_harmonised_eu, here("Data/Paper_1/data_harmonize/data_study3_harmonised_eu.rds"))

write_rds(data_study3_harmonised_na, here("Data/Paper_1/data_harmonize/data_study3_harmonised_na.rds"))
