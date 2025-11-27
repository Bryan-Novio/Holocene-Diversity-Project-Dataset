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
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s4 <-  read_rds(here("Data/Processed/Other/prep_data_study_4.rds"))
harmonization_table  <- read_csv(here("Data/harmonization_table_rev.csv"), show_col_types = FALSE)
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

harmonized_data_study_4 <- purrr::map(taxa_level, ~ harmonize_taxa(pollen_data_s4, data_ancillary, .x)) %>%
  set_names(taxa_name)

#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(harmonized_data_study_4, here("Outputs/Data/paper_1_study_4/harmonized_data_study_4.rds"))
#----------------------------------------------------------#
