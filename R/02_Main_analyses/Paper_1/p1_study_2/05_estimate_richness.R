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

rarefied_data <- read_rds(here("Outputs/Data/paper_1_study_2/rarefied_data_study_2.rds"))

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

richness <- rarefied_data %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age))

summary(richness)

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file ---------------
#----------------------------------------------------------#

write_rds(richness, here("Outputs/Data/paper_1_study_2/richness_data_study_2.rds"))
