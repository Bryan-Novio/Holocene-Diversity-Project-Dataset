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
# 3. Subset data for Paper 1, Study 1
#----------------------------------------------------------# 

# sub-setting data to Europe/N.America/Asia

study3_data  <-
  data %>% 
  relocate(region) %>% 
  filter(region %in% c("North America", "Europe", "Asia")
         )

study3_data %>% distinct(region) 

#####3.1. get pollen counts with ages

data_p1_s3_counts_ages <- 
  study3_data %>%
  get_pollen_counts_with_ages() 

data_p1_s3_counts_ages %>% 
  arrange(desc(age)) %>%
  head(10) # max. age

#----------------------------------------------------------#
# 4. Write the subset data to RDS file
#----------------------------------------------------------# 

write_rds(data_p1_s3_counts_ages, here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

