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

source_files <-
  sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 
 
# by dataset_id

data_binned <-
  data_study1_harmonised  %>% 
  bin_data(dataset_id, 1000)

# discard samples containing less than 500 pollen counts

data_binned_n_pollen <- 
  data_binned %>% 
  group_by(dataset_id, BIN) %>% 
  summarise(total_pollen_grain = sum(summed_pollen_count), .groups = "drop")

data_binned_samples_500 <- 
  data_binned_n_pollen %>% 
  filter(total_pollen_grain >= 500) %>% 
  inner_join(data_binned, by = c("dataset_id", "BIN"))


#----------------------------------------------------------#
# Write the binned and prepared_data to RDS files

#genus level
write_rds(data_binned_samples_500, here("Data/Paper_1/data_bin/data_study1_binned.rds"))

