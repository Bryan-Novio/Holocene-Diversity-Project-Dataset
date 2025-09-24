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
#              ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data_eu <- read_rds(here("Outputs/Data/paper_1_study_3/rarefied_data_study_3_eu.rds"))
rarefied_data_na <- read_rds(here("Outputs/Data/paper_1_study_3/rarefied_data_study_3_na.rds"))

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
# 3. Estimate richness  at different taxo rank -- at 12 cal yr bp (based on Gordon et al)
#----------------------------------------------------------# 

richness_eu <- rarefied_data_eu %>% 
            estimate_richness() %>% 
            dplyr::mutate(age = as.numeric(age))

richness_eu_12k <- richness_eu %>% filter(age <= 12000)



richness_na <- rarefied_data_na %>% 
            estimate_richness() %>% 
            dplyr::mutate(age = as.numeric(age))

richness_na_12k <- richness_na %>% filter(age <= 12000)

#----------------------------------------------------------#
# Write the richness data to an RDS file

write_rds(richness_eu_12k, here("Outputs/Data/paper_1_study_3/richness_data_study_3_eu_12k.rds"))
write_rds(richness_na_12k, here("Outputs/Data/paper_1_study_3/richness_data_study_3_na_12k.rds"))

write_rds(richness_eu, here("Outputs/Data/paper_1_study_3/richness_data_study_3_eu.rds"))
write_rds(richness_na, here("Outputs/Data/paper_1_study_3/richness_data_study_3_na.rds"))

#----------------------------------------------------------#