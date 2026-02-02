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

neotoma_taxa <- 
  readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

# Load harmonization table based on EPD version 2015 (https://doi.org/10.1038/s41467-01)

study1_hlist_raw <- 
  read_csv(here("Data/Paper_1/data_supplementary/study1_hlist_raw_epd.csv")) 

## convert first to neotoma_names

pollen_data_s1_neotoma <- 
  pollen_data_s1 %>% 
  distinct(taxon_name) %>% 
  left_join(.,neotoma_taxa, by = "taxon_name")

## create harm table for taxa in data (Harm A)

pollen_data_taxa_harm_table <- 
  inner_join(pollen_data_s1_neotoma, study1_hlist_raw, join_by("neotoma_names" == "taxon_name")) %>% 
  select(-taxon_name)

## check for taxa in present in data but not in birks harm table

pollen_data_taxa_not_in_birks <- 
  anti_join(pollen_data_s1_neotoma, study1_hlist_raw, join_by("neotoma_names" == "taxon_name")) %>%  # 581 taxa missing
  select(neotoma_names)


# create auxiliary harm table (Harm B)

birks_aux_harm_table <- 
  inner_join(pollen_data_taxa_not_in_birks, harmonisation_table, join_by("neotoma_names" =="taxon_name")) %>% 
select(neotoma_names, level_6) %>% 
  rename(Pollen_type = level_6) 

##merge auxiliary harm table with pollen_data_taxa_harm_table

birks_aux_harm_table_merged_with_pollen_data <-
  bind_rows(birks_aux_harm_table, pollen_data_taxa_harm_table ) %>%
  distinct(neotoma_names, .keep_all = TRUE) %>% #taxon_name is unique
  rename(taxon_name = neotoma_names)

write_csv(birks_aux_harm_table_merged_with_pollen_data, here("Data/Paper_1/data_supplementary/study1_harm_table_updated.csv"))

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

# Harmonize taxa at a selected taxonomic level

data_study1_harmonised <-
  harmonize_taxa(
    data_to_harmonize = pollen_data_s1_renamed,
    harmonisation_table = birks_aux_harm_table_merged_with_pollen_data,
    level = "Pollen_type") %>%
    filter(taxon_name!= "delete") # Remove 'delete' in taxa

# Add subregion as col in harmonized data
  
dataset_id_subregion <- 
  pollen_data_s1_renamed %>%
    select(dataset_id, subregion) %>%
    distinct()

data_study1_harmonised_subregion <- 
  data_study1_harmonised %>% 
  left_join(dataset_id_subregion, by = "dataset_id") %>% 
  rename(taxa = taxon_name)


#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(data_study1_harmonised_subregion, here("Data/Paper_1/data_harmonize/data_study1_harmonised.rds"))

#----------------------------------------------------------#
