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
#              ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data_eu <- 
  read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_eu.rds"))

rarefied_data_na <- 
  read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_na.rds"))

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
# 3. Estimate richness  at different taxo rank -- at 12 cal yr bp 
#----------------------------------------------------------# 

#3.1 Format rarefied data 
data_prepared_richness_estimation_eu <- 
  rarefied_data_eu %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = Acer:Noaea, 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

data_prepared_richness_estimation_na <- 
  rarefied_data_na %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = Abies:Ononis, 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

#3.2. Estimate richness
richness_eu <- 
  data_prepared_richness_estimation_eu %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id))

summary(richness_eu)

richness_na <- 
  data_prepared_richness_estimation_na %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id))

summary(richness_na)

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file
#----------------------------------------------------------# 

write_csv(richness_eu, here("Data/Paper_1/data_estimate_richness/study3_richness_eu.csv"))
write_csv(richness_na, here("Data/Paper_1/data_estimate_richness/study3_richness_na.csv"))
