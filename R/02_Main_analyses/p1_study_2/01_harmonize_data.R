

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

data_p1_s2_12k_1k_counts_ages <- read_rds(here("Outputs/Data/paper_1_study_2/data_p1_s2_12k_1k_counts_ages.rds"))
study_2_harmonized <- read_csv(here("Data/Processed/Other/study_2_taxa_final_clean.csv")) #99 distinct pollen_type
neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

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

#----------------------------------------------------------#
# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

taxa_level <- c("level_5", "level_6", "level_7") 
taxa_name <- c("family", "genus", "species")

# Harmonize taxa at different taxonomic levels

harmonized_data_study_2 <- purrr::map(taxa_level, ~ harmonize_taxa(pollen_data_s2, .x)) %>%
  set_names(taxa_name)

#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(harmonized_data_study_2, here("Outputs/Data/paper_1_study_2/harmonized_data_study_2.rds"))
#----------------------------------------------------------#
