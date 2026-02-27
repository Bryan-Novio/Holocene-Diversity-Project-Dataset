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
#
#               ---- SUBSETTING DATA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(assertthat)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

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

source_files <- 
  sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Subset data for Paper 1, Study 3 ---------------------
#----------------------------------------------------------# 

# sub-setting data to Europe/N.America/Asia

study3_data  <-
  data %>% 
  relocate(region) %>% 
  filter(region %in% c("North America", "Europe", "Asia")
         )

region <- 
  study3_data %>%  
  distinct(region, dataset_id)
  
#3.1. get pollen counts with ages

data_p1_s3_counts_ages <- 
  study3_data %>%
  get_pollen_counts_with_ages() 

data_p1_s3_counts_ages %>% 
  arrange(desc(age)) %>%
  head(10) # max. age

data_p1_s3_counts_ages_region <- 
  inner_join(data_p1_s3_counts_ages, region, by = "dataset_id") 

#----------------------------------------------------------#
# 4. Extract age uncertainties from full dataset --------
#----------------------------------------------------------#

data_age_uncertainty <- 
  data %>% 
  select(dataset_id, age_uncertainty)


#----------------------------------------------------------#
# 5. Write the datasubsets to RDS files-------------------
#----------------------------------------------------------# 

write_rds(data_p1_s3_counts_ages_region, here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

write_rds(data_age_uncertainty, here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

