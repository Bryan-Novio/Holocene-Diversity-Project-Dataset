#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 2: Simova et al
#
#
#                          2023
# North America, site-based richness (dataset_id,age,
# 1000 bins - rarefy 400
#
#                  ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#


# Prepare data for richness estimation

prepared_data_for_richness_estimation_2 <- binned_data %>%
  prepare_data_for_richness_estimation("binned") %>%
  mutate(sample_id = paste0(dataset_id, "-", age))


harmonized_data_study_2 <- read_rds(here("Outputs/Data/paper_1_study_2/harmonized_data_study_2.rds"))

harmonized_data_study_2_re <- harmonized_data_study_2 %>% rename(pollen_grains = pollen_counts)

prepared_data_for_richness_estimation <- read_rds(here("Outputs/Data/paper_1_study_2/prepared_data_for_richness_estimation_study_2.rds"))

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
# 3. Rarefy data  at different taxo rank ------------------
#----------------------------------------------------------#

set.seed(1234)
rarefied_data <-
  data_for_richness %>%
  rarefy_all_samples(n_grains = 400)


#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file----------------
#----------------------------------------------------------#

write_rds(rarefied_data, here("Outputs/Data/paper_1_study_2/rarefied_data_study_2.rds"))
