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
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_p1_s2_12k_1k_counts_ages <- read_rds(
  here("Outputs/Data/paper_1_study_2/data_p1_s2_12k_1k_counts_ages.rds"))

data_only_woody <- read_csv(
  here("Data/Processed/Other/data_only_woody.csv")
) # 196 distinct pollen_type

harmonisation_table <- readr::read_csv(
  here::here("Data/harmonization_table_rev.csv")
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

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------#

data_only_woody_renamed <- data_only_woody %>% 
  rename(taxon_name = taxa, pollen_counts = summed_pollen_count)  

pollen_ages <- data_p1_s2_12k_1k_counts_ages %>% 
  select(dataset_id,sample_id, age) %>%
  distinct(dataset_id, .keep_all = TRUE) %>% 
  mutate(dataset_id = as.double(dataset_id),
         sample_id = as.double(sample_id)
  )

data_only_woody_with_ages <- data_only_woody_renamed %>% 
  inner_join(pollen_ages, by = c("dataset_id","sample_id")) %>% 
  select(taxon_name,dataset_id,sample_id, pollen_counts,age)
  
# Harmonize taxa at different taxonomic levels

data_study2_harmonised <-
  harmonize_taxa(
    data_to_harmonize = data_only_woody_with_ages,
    harmonisation_table = harmonisation_table,
    level = "level_6"
  )

#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(data_study2_harmonised, here("Outputs/Data/paper_1_study_2/data_study2_harmonised.rds"))

#----------------------------------------------------------#
