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
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s3_eu <-  read_rds(here("Outputs/Data/paper_1_study_3/datasub_p1_s3_eu_counts_ages.rds"))
pollen_data_s3_na <-  read_rds(here("Outputs/Data/paper_1_study_3/datasub_p1_s3_na_counts_ages.rds"))

harmonization_table_eu  <- read_csv(here("Data/Input/Harmonisation_tables/s3_EU_harmonization_table.csv"), show_col_types = FALSE)
harmonization_table_na  <- read_csv(here("Data/Input/Harmonisation_tables/s3_EU_harmonization_table.csv"), show_col_types = FALSE)

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

harmonized_data_study_3_eu <- harmonize_taxa_taxa_s3_01(pollen_data_s3_eu, neotoma_taxa, harmonization_table_eu) 

harmonized_data_study_3_na <- harmonize_taxa_taxa_s3_01(pollen_data_s3_na, neotoma_taxa, harmonization_table_na) 
  
#----------------------------------------------------------#
# Write the harmonized data to RDS files

write_rds(harmonized_data_study_3_eu, here("Outputs/Data/paper_1_study_3/harmonized_data_study_3_eu.rds"))
write_rds(harmonized_data_study_3_na, here("Outputs/Data/paper_1_study_3/harmonized_data_study_3_na.rds"))

#----------------------------------------------------------#
