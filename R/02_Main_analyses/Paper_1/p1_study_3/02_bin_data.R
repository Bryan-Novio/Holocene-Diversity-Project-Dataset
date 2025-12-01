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

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_study3_harmonised_eu <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_eu.rds"))

data_study3_harmonised_na <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_na.rds"))

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

data_binned <-
  data_study3_harmonised_eu  %>% 
  bin_data(dataset_id, 500)

data_binned_2 <-
  data_study3_harmonised_na  %>% 
  bin_data(dataset_id, 500)

#----------------------------------------------------------#
# 4. Write the binned and prepared_data to RDS files
#----------------------------------------------------------# 

write_rds(data_binned, here("Data/Paper_1/data_bin/data_study3_binned_eu.rds"))
write_rds(data_binned_2, here("Data/Paper_1/data_bin/data_study3_binned_na.rds"))
