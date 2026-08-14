#----------------------------------------------------------#
#               Holocene Diversity Project
#
#
#            Paper01| Method 2: Simova et al
#
#
#                          2023
# North America, site-based richness (dataset_id,age,
# 1000 bins - rarefy 400
#
#
#                  ---- SUMMARIZE ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#              ---- SUBSETTING DATA  ----

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data <- 
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))     

# 1,147 fossil pollen records

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
# 3. Subset data for Paper 1, Study 2
#----------------------------------------------------------#

# 3.1.Filter on length and time

# .  3.1.1. Sub-setting data to North America

data_p1_s2 <- 
  data %>%
  filter(region == "North America") %>%
  relocate(region)

# 474 fossil pollen records

# 3.1.2. Only include cores that span at least 12k years (long cores)

data_p1_s2_12k <- 
  data_p1_s2 %>%
  dplyr::mutate(
    age_span = age_max - age_min
  ) %>%
  filter(age_span >= 12000)


# 304 fossil pollen records

# alternative
# data_p1_s2_12k <- data_p1_s2 %>%
# filter_cores_by_total_span(age_span = 12e3)

# 3.1.3. Filter out all samples younger than 1000 years (young samples)

data_p1_s2_12k_1k <- 
  data_p1_s2_12k %>% filter(age_min <= 1000)

# 289 fossil pollen records

##### 3.2. get pollen counts with ages

data_p1_s2_12k_1k_counts_ages <- 
  data_p1_s2_12k_1k %>%
  get_pollen_counts_with_ages()

#----------------------------------------------------------#
#                 ----  BINNING  ----
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_p1_s2_12k_1k_counts_ages <-
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s2_counts_ages.rds"))

#----------------------------------------------------------#
# 2. Bin data at different taxo rank --
#----------------------------------------------------------#

# Bin  data

data_binned <- 
  data_p1_s2_12k_1k_counts_ages %>% 
  bin_data(dataset_id,1000)

# Filter out bins with < 400 pollen grains total

data_binned_400 <- 
  select_only_bins_with_specific_pollen_grain_sum(data_binned, 400)

# Filter out cores with < 11 bins

data_binned_filtered <-
  select_cores_with_specific_number_of_bins(
    data_binned_400,
    n_bins = 11
  )


data_binned_filtered %>% distinct(dataset_id) # 286 pollen cores

