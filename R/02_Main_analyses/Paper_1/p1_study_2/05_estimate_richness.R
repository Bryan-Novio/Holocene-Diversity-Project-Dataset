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
#              ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data <-
  read_rds(here("Data/Paper_1/data_rarefy/data_study2_rarefied.rds"))

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
# 3. Estimate richness  at different taxo rank ------------
#----------------------------------------------------------# 

## 3.1. Prepare data for richness_estimation

data_prepared_richness_estimation <- 
  rarefied_data  %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
   names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = Alnus:Maclura, 
  names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")


## 3.2. Estimate richness
richness <- 
  data_prepared_richness_estimation %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age))

summary(richness)

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file ---------------
#----------------------------------------------------------#

write_csv(richness, here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))
