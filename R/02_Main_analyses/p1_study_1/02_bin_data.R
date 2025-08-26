#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#                          2019
#
# 
#               ----  BINNING  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

harmonized_data <- read_rds(here("Outputs/Data/paper_1_study_1/harmonized_data_study_1.rds"))

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

binned_data <- purrr::map(harmonized_data, ~ bin_data(.x, 1000)) 

# Prepare data for richness estimation

prepared_data_for_richness_estimation <- 
  purrr::map(binned_data, ~ prepare_data_for_richness_estimation(.x, "binned")) %>%
  purrr::map( ~ dplyr::mutate(.x, sample_id = paste0(dataset_id, "-", age)))

#----------------------------------------------------------#
# Write the binned and prepared_data to RDS files
write_rds(binned_data, here("Outputs/Data/paper_1_study_1/binned_data_study_1.rds"))
write_rds(prepared_data_for_richness_estimation, here("Outputs/Data/paper_1_study_1/prepared_data_for_richness_estimation_study_1.rds"))
