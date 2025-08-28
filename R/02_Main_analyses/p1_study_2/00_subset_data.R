#----------------------------------------------------------#
#               Holocene Diversity Project
#
#
#            Paper01| Method 2: Simova et al
#
#                       
#                          2023
# North America, site-based richness (dataset_id,age, 
# 1000 bins - rarefy 400 
#
#
#               ---- SUBSETTING DATA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


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
# 3. Subset data for Paper 1, Study 1
#----------------------------------------------------------# 


data_p1_s2 <- data %>% 
  filter(region =="North America") %>%   # sub-setting data to North America
  relocate(region)

##### 3.2. get pollen counts with ages

data_p1_s2_counts_ages <- data_p1_s2  %>% get_pollen_counts_with_ages() 

data_p1_s2_counts_ages %>% arrange(desc(age)) %>% head(10) # max. age


##### 3.1. filter dataset  to 12ka below

data_p1_s2_counts_ages_12ka <- data_p1_s2_counts_ages %>% 
  filter(age >= -75 & age <= 12000) %>% 
  relocate(age)

max(data_p1_s2_counts_ages_12ka$age)
min(data_p1_s2_counts_ages_12ka$age)

#----------------------------------------------------------#
# 4. Write the subset data to RDS file
#----------------------------------------------------------# 

write_rds(data_p1_s2_counts_ages_12ka, here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))
