#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#     
#                          2019
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s1 <-  read_rds(here("Data/Processed/Other/prep_data_study_1.rds"))
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

sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


#----------------------------------------------------------#
# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

taxa_level <- c("level_5", "level_6", "level_7") 
taxa_name <- c("family", "genus", "species")

# Harmonize taxa at different taxonomic levels

harmonized_data_study_1 <- purrr::map(taxa_level, ~ harmonize_taxa(pollen_data_s1, data_ancillary, .x)) %>%
  set_names(taxa_name)


#----------------------------------------------------------#
# 5. Bin (bin = 500), rarefy (n_grains = 500, n_iter = 10) 
#    and estimate richness harmonized data set using {bin_rarefy_estimate_richness_harmonized_data }
#----------------------------------------------------------# 

#----------------------------------------------------------#
# study data set 1
#----------------------------------------------------------#


# Bin, rarefy and estimate richness for each taxonomic level
s1_1 <-  bin_rarefy_estimate_richness_harmonized_data(harmonized_data_study_1$family)  
s1_2 <-  bin_rarefy_estimate_richness_harmonized_data(harmonized_data_study_1$genus)
s1_3 <-  bin_rarefy_estimate_richness_harmonized_data(harmonized_data_study_1$species)

