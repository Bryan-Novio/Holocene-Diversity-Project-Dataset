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
#                  ----RAREFACTION  ----
#----------------------------------------------------------#
  
library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

prepared_data_for_richness_estimation <- read_rds(here("Outputs/Data/paper_1_study_2/prepared_data_for_richness_estimation_study_2.rds"))

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
# 3. Rarefy data  at different taxo rank ------------------
#----------------------------------------------------------# 

rarefied_data <- prepared_data_for_richness_estimation %>%
  rarefy_all_samples_iter(n_grains = 400, n_iter = 10) %>% 
separate_wider_delim(sample_id, "-", names = c("sample_id","age"))

#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file----------------
#----------------------------------------------------------#

write_rds(rarefied_data, here("Outputs/Data/paper_1_study_2/rarefied_data_study_2.rds"))
