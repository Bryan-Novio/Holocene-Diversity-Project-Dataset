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

data_study3_harmonised_asia <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_asia.rds"))


data_study3_harmonised_europe <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_europe.rds"))

data_study3_harmonised_namerica <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_namerica.rds"))

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
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 

data_binned_asia <-
  data_study3_harmonised_asia %>% 
  bin_data(dataset_id, 500)

data_binned_europe <-
  data_study3_harmonised_europe %>% 
  bin_data(dataset_id, 500)

data_binned_namerica <-
  data_study3_harmonised_namerica %>% 
  bin_data(dataset_id, 500)

#----------------------------------------------------------#
# 4. Write the binned and prepared_data to RDS files
#----------------------------------------------------------# 

write_rds(data_binned_asia, here("Data/Paper_1/data_bin/data_study3_binned_asia.rds"))
write_rds(data_binned_europe, here("Data/Paper_1/data_bin/data_study3_binned_europe.rds"))
write_rds(data_binned_namerica, here("Data/Paper_1/data_bin/data_study3_binned_namerica.rds"))
