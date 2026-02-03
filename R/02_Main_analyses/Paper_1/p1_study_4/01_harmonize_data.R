#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                          2023
#
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
#
#                 ----HARMONIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(assertthat)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s4 <- 
  read_rds( here("Data/Paper_1/data_subset/datasub_p1_s4_counts_ages.rds"))

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names)

neotoma_taxa <- 
  readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)


# Load Bhatta harm tables 

bhatta_harm_tables <- 
  list.files("Data/Paper_1/data_supplementary", pattern = "Bhatta\\.csv$", full.names = TRUE) %>%
  purrr::map(read_csv)

Asia_levant <- bhatta_harm_tables[[1]] 
Asia_main <- bhatta_harm_tables[[2]]
Asia_siberia <- bhatta_harm_tables[[3]]

#bind harm tables

hlist_bhatta_asia <-
  bind_rows(Asia_levant, Asia_main, Asia_siberia) %>% 
  select(taxon_name, level_2) %>% 
  rename(level = level_2) %>% 
#remove duplication in taxa
  distinct(taxon_name,level) %>% 
  left_join(.,neotoma_taxa, by = "taxon_name") %>% 
  select(neotoma_names, level)
  
# create harm table for taxa in data (Harm A)

## convert first to neotoma_names

pollen_data_s4_neotoma <- 
  pollen_data_s4 %>% 
  distinct(taxa) %>% 
  left_join(.,neotoma_taxa, join_by("taxa" == "taxon_name")
  ) %>% 
  select(neotoma_names)

pollen_data_taxa_harm_table <- 
  left_join(pollen_data_s4_neotoma, harmonisation_table, join_by("neotoma_names" == "taxon_name")) 

# Check for taxa in present in data but not in each birks harm table

pollen_data_taxa_not_in_bhatta <- 
  anti_join(pollen_data_s4_neotoma,hlist_bhatta_asia, by = "neotoma_names") %>%  #  72 taxa missing
  select(neotoma_names)

## Create auxiliary harm table (Harm B)

# ---------------------------------------------------------------------

bhatta_aux_harm_table  <- 
  inner_join(pollen_data_taxa_not_in_bhatta, harmonisation_table, join_by("neotoma_names" =="taxon_name")) %>% 
  select(neotoma_names, level_6)

##Merge auxiliary harm table with pollen_data_taxa_harm_table


bhata_aux_harm_table_merged <-
  bind_rows(bhatta_aux_harm_table, pollen_data_taxa_harm_table) %>%
  distinct(neotoma_names, .keep_all = TRUE) %>% #taxon_name is unique
  rename(taxon_name = neotoma_names) %>% 
  select(taxon_name, level_6)


# save new harm table

write_csv(bhata_aux_harm_table_merged, here("Data/Paper_1/data_supplementary/study4_hlist_updated_Asia.csv"))

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

pollen_data_s4_renamed <- 
  pollen_data_s4 %>% 
  inner_join(neotoma_taxa, join_by(taxa ==taxon_name) ) %>% 
  select(neotoma_names,dataset_id,pollen_counts,age) %>% 
  rename(taxon_name = neotoma_names)

pollen_data_s4_renamed %>% distinct(taxon_name) # 899 taxa

# Harmonize taxa at different taxonomic levels

data_study4_harmonised <-
  harmonize_taxa(
    data_to_harmonize = pollen_data_s4_renamed,
    harmonisation_table = bhata_aux_harm_table_merged,
    level = "level_6") # did not omit 'undif'

data_study4_harmonised %>% 
  distinct(taxon_name) # 446 taxa 

#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(data_study4_harmonised, here("Data/Paper_1/data_harmonize/data_study4_harmonised.rds"))

#----------------------------------------------------------#
