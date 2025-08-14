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
library(dplyr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data <- read_rds(here("Outputs/Data/paper_1_study_4/rarefied_data_study_4.rds"))

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

sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Estimate richness  at different taxo rank --
#----------------------------------------------------------# 

richness <- purrr::map(rarefied_data, ~ estimate_richness(.x)) %>% 
  purrr::map( ~ dplyr::mutate(.x,age = as.numeric(age)))


#----------------------------------------------------------#
# Write the richness data to an RDS file

write_rds(richness, here("Outputs/Data/paper_1_study_4/richness_data_study_4.rds"))
#----------------------------------------------------------#