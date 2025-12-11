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
#                 ----HARMONIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

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


# Harmonize taxa at different taxonomic levels

data_study4_harmonised <-
  harmonize_taxa(
    data_to_harmonize = pollen_data_s4_renamed,
    harmonisation_table = harmonisation_table,
    level = "level_6"
  )

#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(data_study4_harmonised, here("Data/Paper_1/data_harmonize/data_study4_harmonised.rds"))
#----------------------------------------------------------#
