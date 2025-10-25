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
library(vegan)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

<<<<<<< HEAD
data_study2_harmonised <- read_rds(here("Outputs/Data/paper_1_study_2/data_study2_harmonised.rds"))

=======
<<<<<<< HEAD
data_study2_harmonised <- read_rds(here("Outputs/Data/paper_1_study_2/data_study2_harmonised.rds"))
=======

data_study2_harmonised <- read_rds(here("Outputs/Data/paper_1_study_2/data_study2_harmonised.rds"))

# Prepare data for richness estimation

prepared_data_for_richness_estimation_2 <- binned_data %>%
  prepare_data_for_richness_estimation("binned") %>%
  mutate(sample_id = paste0(dataset_id, "-", age))


prepared_data_for_richness_estimation <- read_rds(here("Outputs/Data/paper_1_study_2/prepared_data_for_richness_estimation_study_2.rds"))
>>>>>>> 41830e6b03f055e26fd00e1bb141700c9deb88a8

>>>>>>> 70952a74e255e7f8ec33455343c127fd6c1a97d6
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

data_to_rarefy_1 <- 
  data_study2_harmonised %>%
  select(dataset_id,sample_id, taxa, pollen_counts) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  ) %>% 
  nest(samples = c(sample_id, taxa, pollen_counts)
  ) %>% 
  unnest(samples) %>% 
  pivot_wider(
    names_from = taxa,
    values_from = pollen_counts
  ) 
  
data_to_rarefy_2 <- 
  data_to_rarefy_1 %>% 
  nest(samples = c(,2:106)
  ) 
  
set.seed(1234)

rarefied_data <-
<<<<<<< HEAD
  data_to_rarefy_2 %>%
  rarefy_all_samples(400)

=======
<<<<<<< HEAD
  data_to_rarefy_2 %>%
  rarefy_all_samples(400)
=======
  data_study2_harmonised %>%
  rarefy_all_samples(n_grains = 400)
>>>>>>> 41830e6b03f055e26fd00e1bb141700c9deb88a8
>>>>>>> 70952a74e255e7f8ec33455343c127fd6c1a97d6

rarefied_data %>% unnest(data)
  
#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file----------------
#----------------------------------------------------------#

write_rds(rarefied_data, here("Outputs/Data/paper_1_study_2/rarefied_data_study_2.rds"))
