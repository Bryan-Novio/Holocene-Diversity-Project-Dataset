#----------------------------------------------------------#
#
#                          2023
# North America, site-based richness (dataset_id,age,
# 1000 bins - rarefy 400
#
#                 ----  BINNING  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_p1_s2_12k_1k_counts_ages <- read_rds(here("Outputs/Data/paper_1_study_2/data_p1_s2_12k_1k_counts_ages.rds"))

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
# 3. Bin data at different taxo rank --
#----------------------------------------------------------#

# Bin  data

data_binned <- data_p1_s2_12k_1k_counts_ages %>% bin_data(1000)

# Filter out bins with < 400 pollen grains total

data_binned_400 <- select_only_bins_with_specific_pollen_grain_sum(data_binned, 400)

# Filter out cores with < 11 bins

############# combine data_binned with origin

sample_id <- data_p1_s2_12k_1k_counts_ages %>% 
  select(dataset_id,sample_id)

data_binned_to_be_filtered <- inner_join(data_binned,sample_id, by = "dataset_id") #attach sample_id to binned data

data_binned_filtered <-
  select_cores_with_specific_number_of_bins(
    data_binned_to_be_filtered,
    n_bins = 11
  )

View(data_binned_filtered)

data_binned_filtered %>% distinct(n) 

#----------------------------------------------------------#
# 5. Write the binned and prepared_data to RDS files
#----------------------------------------------------------#

write_rds(data_binned_filtered, here("Outputs/Data/paper_1_study_2/data_binned_filtered.rds"))
