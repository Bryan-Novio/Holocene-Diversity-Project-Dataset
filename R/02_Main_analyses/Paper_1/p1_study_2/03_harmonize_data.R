#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 2: Simova et al
#
#
#                          2023
# North America, site-based richness (dataset_id,age,
# 1000 bins - rarefy 400
#
#                   ---HARMONIZATION ----
#
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_only_woody <- 
  read_csv(
  here("Data/Paper_1/data_supplementary/data_only_woody.csv")
) # 196 distinct pollen_type

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names)

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

# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------#

data_only_woody_renamed <- 
  data_only_woody %>% 
  rename(taxon_name = taxa, pollen_counts = summed_pollen_count, age = BIN) %>% 
  dplyr::group_by(dataset_id, age, taxon_name ) %>%
  dplyr::summarize(
    pollen_counts = sum(pollen_counts),
    .groups = "drop"
  )

# Harmonize taxa at different taxonomic levels

data_study2_harmonised <-
  harmonize_taxa(
    data_to_harmonize = data_only_woody_renamed,
    harmonisation_table = harmonisation_table,
    level = "level_6"
  )

data_study2_harmonised %>% distinct(dataset_id)

#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(data_study2_harmonised, here("Data/Paper_1/data_harmonize/data_study2_harmonised.rds"))

#----------------------------------------------------------#
