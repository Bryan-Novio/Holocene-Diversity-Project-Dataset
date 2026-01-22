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
#               ----HARMONIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s1 <-  
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s1_counts_ages.rds"))

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names)

harmonisation_table %>% 
  distinct(level_6)

neotoma_taxa <- 
  readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)


#Customize harmonization table for Study 1 by adding missing taxa

study1_hlist_raw <- 
  read_csv(here("Data/Paper_1/data_supplementary/study1_hlist_raw_epd.csv")) 

study1_hlist_raw %>% 
  distinct(taxon_name)

# remove duplicates

study1_hlist_raw_unique <- 
  read_csv(here("Data/Paper_1/data_supplementary/study1_hlist_raw_epd.csv")) %>% 
  distinct(taxon_name, .keep_all = TRUE) # remove duplicates (n =110)

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names) %>% 
  select(taxon_name, level_6) %>% 
  rename(Pollen_type = level_6)

harmonisation_table %>% distinct(taxon_name)

harmonisation_table_missing <- 
  study1_hlist_raw_unique %>% 
  anti_join(harmonisation_table, by = "taxon_name")


study1_hlist_updated <- 
  bind_rows(harmonisation_table,harmonisation_table_missing)

study1_hlist_updated %>% 
  distinct(taxon_name)

View(study1_hlist_updated)

write_csv(study1_hlist_updated, here("Data/Paper_1/data_supplementary/study1_hlist_updated.csv"))

study1_hlist_updated <- 
  read_csv(here("Data/Paper_1/data_supplementary/study1_hlist_updated.csv"))

study1_hlist_updated %>% 
  distinct(taxon_name)

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

#Rename taxa with neotoma name

pollen_data_s1_renamed <- 
  pollen_data_s1 %>% 
  inner_join(neotoma_taxa, by = "taxon_name") %>% 
  select(neotoma_names,dataset_id,pollen_counts,age,subregion) %>% 
  rename(taxon_name = neotoma_names)

#check all taxa in data present in harm table

pollen_data_s1_renamed_taxon_name <- pollen_data_s1_renamed %>%  distinct(taxon_name)

study1_hlist_updated_taxon_name <- study1_hlist_updated %>% distinct(taxon_name)

anti_join(pollen_data_s1_renamed_taxon_name,study1_hlist_updated_taxon_name, by = 'taxon_name' )

# Harmonize taxa at different taxonomic levels

data_study1_harmonised <-
  harmonize_taxa(
    data_to_harmonize = pollen_data_s1_renamed,
    harmonisation_table = study1_hlist_updated,
    level = "Pollen_type"
  ) 

dataset_id_subregion <- 
  pollen_data_s1_renamed %>%
    select(dataset_id, subregion) %>%
    distinct()

data_study1_harmonised_subregion <- 
  data_study1_harmonised %>% 
  inner_join(dataset_id_subregion, by = "dataset_id") %>% 
  rename(taxa = taxon_name)

data_study1_harmonised_subregion_renamed <- 
  data_study1_harmonised_subregion %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(taxon_name,dataset_id,pollen_counts,age,subregion) %>% 
  rename(taxa = taxon_name)


#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(data_study1_harmonised_subregion_renamed, here("Data/Paper_1/data_harmonize/data_study1_harmonised.rds"))

#----------------------------------------------------------#
