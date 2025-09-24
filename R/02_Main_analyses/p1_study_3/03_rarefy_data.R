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
#                 ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

prepared_data_for_richness_estimation_eu <- read_rds(here("Outputs/Data/paper_1_study_3/prepared_data_for_richness_estimation_study_3_eu.rds"))
prepared_data_for_richness_estimation_na <- read_rds(here("Outputs/Data/paper_1_study_3/prepared_data_for_richness_estimation_study_3_na.rds"))

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

rarefied_data_eu <- prepared_data_for_richness_estimation_eu %>% 
         rarefy_all_samples_iter(n_grains = 300, n_iter = 10) %>% 
            separate_wider_delim(sample_id, "-", names = c("sample_id","age"))

rarefied_data_na <- prepared_data_for_richness_estimation_na %>% 
         rarefy_all_samples_iter(n_grains = 300, n_iter = 10) %>% 
            separate_wider_delim(sample_id, "-", names = c("sample_id","age"))

#----------------------------------------------------------#
# Write the rarefied data to an RDS file

write_rds(rarefied_data_eu, here("Outputs/Data/paper_1_study_3/rarefied_data_study_3_eu.rds"))
write_rds(rarefied_data_na, here("Outputs/Data/paper_1_study_3/rarefied_data_study_3_na.rds"))

#----------------------------------------------------------#
