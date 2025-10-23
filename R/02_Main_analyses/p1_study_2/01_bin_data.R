#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 2: Simova et al
#
<<<<<<< HEAD
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
  
source_files <-  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )
 
#----------------------------------------------------------#
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 
  
# Bin  data 
  
binned_data <- data_p1_s2_12k_1k_counts_ages %>% bin_data(1000)

# Filter out bins with < 400 pollen grains total 

binned_data_400 <- binned_data %>%
  group_by(BIN) %>%
  summarise(
    total_pollen = sum(summed_pollen_count)
  )

# Filter out cores with < 11 bins 

binned_data_400_11 <- binned_data %>%
  group_by(dataset_id) %>% 
  summarize(BIN_count = n_distinct(BIN)) %>% 
  filter(BIN_count > 11)

binned_data_400_11_re <- binned_data %>% filter(dataset_id != 15081 & dataset_id != 17324)

#----------------------------------------------------------#
# 5. Write the binned and prepared_data to RDS files
#----------------------------------------------------------# 
  
write_rds(binned_data_400_11_re, here("Outputs/Data/paper_1_study_2/binned_data_400_11_re.rds"))


bin_data <- harmonized_data_study_2 %>% bin_data(1000)

data_for_richness <- bin_data  %>%
  prepare_data_for_richness_estimation("binned") %>% 
  mutate(sample_id = paste0(dataset_id, "-", age))


  
=======
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

data_binned_filtered <-
  select_cores_with_specific_number_of_bins(
    data_binned_400,
    n_bins = 11
  )


#----------------------------------------------------------#
# 5. Write the binned and prepared_data to RDS files
#----------------------------------------------------------#

write_rds(data_binned_filtered, here("Outputs/Data/paper_1_study_2/data_binned_filtered.rds"))
>>>>>>> 73e9085476a447a99120f43243221564f9f2621f
