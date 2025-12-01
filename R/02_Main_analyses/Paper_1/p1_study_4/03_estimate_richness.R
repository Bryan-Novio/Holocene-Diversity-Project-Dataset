#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
#
#            ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data_eu_s4 <- read_rds(here("Data/Paper_1/data_rarefy/data_study4_rarefied_eu.rds"))
rarefied_data_na_s4 <- read_rds(here("Data/Paper_1/data_rarefy/data_study4_rarefied_na.rds"))

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
# 3. Estimate richness  at different taxo rank --
#----------------------------------------------------------# 

#3.1 Format rarefied data 
data_prepared_richness_estimation_eu_non_binned <- 
  rarefied_data_eu %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = Acer:Noaea, 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count) %>% 
  prepare_data_for_richness_estimation(type = "nonbinned")

data_prepared_richness_estimation_na_non_binned <- 
  rarefied_data_na %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = Abies:Ononis, 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count) %>% 
  prepare_data_for_richness_estimation(type = "nonbinned")

View(data_prepared_richness_estimation_na_non_binned)

#3.2. Estimate richness
richness_eu_non_binned <- 
  data_prepared_richness_estimation_eu_non_binned %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id))

summary(richness_eu)

richness_na <- 
  data_prepared_richness_estimation_na_non_binned %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id))

summary(richness_na)

#----------------------------------------------------------#
# Write the richness data to an RDS file

write_rds(richness_na, here("Data/Paper_1/data_estimate_richness/study4_richness_na.csv"))
#----------------------------------------------------------#