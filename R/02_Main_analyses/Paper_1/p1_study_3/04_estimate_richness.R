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

rarefied_data_asia <- 
  read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_asia.rds"))

rarefied_data_europe <- 
read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_europe.rds"))

rarefied_data_namerica <- 
read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_namerica.rds"))

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

data_prepared_richness_estimation_asia <- 
  rarefied_data_asia %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

data_prepared_richness_estimation_europe <- 
  rarefied_data_europe %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

data_prepared_richness_estimation_namerica <- 
  rarefied_data_namerica %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

#3.2. Estimate richness

richness_asia <- 
  data_prepared_richness_estimation_asia %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  filter(age <= 12000)

summary(richness_asia)

richness_europe <- 
  data_prepared_richness_estimation_europe %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  filter(age <= 12000)

summary(richness_europe)


richness_namerica <- 
  data_prepared_richness_estimation_namerica %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  filter(age <= 12000)

summary(richness_namerica)

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file
#----------------------------------------------------------# 

write_csv(richness_asia, here("Data/Paper_1/data_estimate_richness/study3_richness_asia.csv"))
write_csv(richness_europe, here("Data/Paper_1/data_estimate_richness/study3_richness_europe.csv"))
write_csv(richness_namerica, here("Data/Paper_1/data_estimate_richness/study3_richness_namerica.csv"))


