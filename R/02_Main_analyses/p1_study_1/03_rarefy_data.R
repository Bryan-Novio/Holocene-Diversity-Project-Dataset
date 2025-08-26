#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#                          2019
#
# 
#               ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

prepared_data_for_richness_estimation <- read_rds(here("Outputs/Data/paper_1_study_1/prepared_data_for_richness_estimation_study_1.rds"))

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
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

rarefied_data <- purrr::map(prepared_data_for_richness_estimation, ~ rarefy_all_samples_iter(
  data_source =.,n_grains = 500, n_iter = 10)) %>% 
  purrr::map (~ separate_wider_delim(.x,sample_id, "-", names = c("sample_id","age")))


#----------------------------------------------------------#
# Write the rarefied data to an RDS file

write_rds(rarefied_data, here("Outputs/Data/paper_1_study_1/rarefied_data_study_1.rds"))
#----------------------------------------------------------#