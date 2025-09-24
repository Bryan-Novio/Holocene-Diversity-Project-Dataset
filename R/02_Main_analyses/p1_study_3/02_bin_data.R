#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#                   ----  BINNING  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

harmonized_data_study_3_eu <- read_rds(here("Outputs/Data/paper_1_study_3/harmonized_data_study_3_eu.rds"))

harmonized_data_study_3_na <- read_rds(here("Outputs/Data/paper_1_study_3/harmonized_data_study_3_na.rds"))

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
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 


# Bin  data 

binned_data_eu <-  bin_data(harmonized_data_study_3_eu, 500)
binned_data_na <-  bin_data(harmonized_data_study_3_na, 500)

# Prepare data for richness estimation

prepared_data_for_richness_estimation_eu <- binned_data_eu %>% 
  prepare_data_for_richness_estimation("binned") %>%
   dplyr::mutate(sample_id = paste0(dataset_id, "-", age))

prepared_data_for_richness_estimation_na <- binned_data_na %>% 
  prepare_data_for_richness_estimation("binned") %>%
  dplyr::mutate(sample_id = paste0(dataset_id, "-", age))

#----------------------------------------------------------#
# Write the binned and prepared_data to RDS files

write_rds(binned_data_eu, here("Outputs/Data/paper_1_study_3/binned_data_study_eu.rds"))
write_rds(binned_data_na, here("Outputs/Data/paper_1_study_3/binned_data_study_na.rds"))

write_rds(prepared_data_for_richness_estimation_eu, here("Outputs/Data/paper_1_study_3/prepared_data_for_richness_estimation_study_3_eu.rds"))
write_rds(prepared_data_for_richness_estimation_na, here("Outputs/Data/paper_1_study_3/prepared_data_for_richness_estimation_study_3_na.rds"))

