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

data_only_woody <- read_csv(
  here("Data/Processed/Other/data_only_woody.csv")
) # 99 distinct pollen_type
hamronisation_table <- readr::read_csv(
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

#----------------------------------------------------------#
<<<<<<< HEAD
# 3. Harmonize at genus level --
#----------------------------------------------------------# 


harmonized_data_study_2 <- harmonize_taxa_s2_01(data_p1_s2_12k_1k_counts_ages, neotoma_taxa = neotoma_taxa, study_2_harmonized = study_2_harmonized) 
=======
# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------#

# Harmonize taxa at different taxonomic levels

data_study2_harmonised <-
  harmonize_taxa(
    data_to_harmonize = data_only_woody,
    harmomnisation_table = hamronisation_table,
    level = "level_6"
  )
>>>>>>> 73e9085476a447a99120f43243221564f9f2621f

#----------------------------------------------------------#
# Write the harmonized data to RDS files

<<<<<<< HEAD
write_rds(harmonized_data_study_2, here("Outputs/Data/paper_1_study_2/harmonized_data_study_2.rds"))

=======
write_rds(data_study2_harmonised, here("Outputs/Data/paper_1_study_2/data_study2_harmonised.rds"))
>>>>>>> 73e9085476a447a99120f43243221564f9f2621f
#----------------------------------------------------------#
