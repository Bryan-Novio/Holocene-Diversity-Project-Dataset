#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#                       2019
#
# 
#               ----  BINNING  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_study1_harmonised <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study1_harmonised.rds"))

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
  data_study1_harmonised  %>% 
  bin_data(dataset_id, 1000)

#----------------------------------------------------------#
# Write the binned and prepared_data to RDS files

#genus level
write_rds(data_binned, here("Data/Paper_1/data_bin/data_study1_binned.rds"))

