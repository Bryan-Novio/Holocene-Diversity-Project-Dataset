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
#         ---- ADD RANDOM SELECTION OF TIME  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

##rarefied data multiple iteration (1000x)

rarefied_dataset_assembly_asia <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_asia.rds"))

rarefied_dataset_assembly_europe <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_europe.rds"))

rarefied_dataset_assembly_namerica <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_namerica.rds"))

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
# 3. Add random selection of time  to each iteration ------
#----------------------------------------------------------#

age_uncertainty <- 
  data %>% 
  select(dataset_id, age_uncertainty) 

id <- age_uncertainty %>%
  filter(dataset_id =="1001") %>% 
  unnest(age_uncertainty)

View(id)
