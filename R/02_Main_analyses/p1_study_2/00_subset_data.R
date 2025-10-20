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

# 3.1.Filter on length and time  
            
      #.  3.1.1. Sub-setting data to North America

data_p1_s2 <- data %>% 
  filter(region =="North America") %>%   
  relocate(region)

data_p1_s2 %>% relocate(young_age) %>% filter(young_age == 1000)

      # 3.1.2. Only include cores that span at least 12k years (long cores) 

data_p1_s2_12k <- data_p1_s2 %>% relocate(age_max) %>% filter(age_max >= 12000) %>% arrange(age_max)

      # 3.1.3. Filter out all samples younger than 1000 years (young samples) 

data_p1_s2_12k_1k <- data_p1_s2_12k %>% filter(age_min >= 1000) %>% relocate(age_min) %>% arrange(age_min)

##### 3.2. get pollen counts with ages

data_p1_s2_12k_1k_counts_ages <- data_p1_s2_12k_1k %>% get_pollen_counts_with_ages() 

data_p1_s2_12k_1k_counts_ages %>% arrange(desc(age)) %>% head(10) # max. age


#----------------------------------------------------------#
# 4. Write the subset data to RDS file
#----------------------------------------------------------# 

write_rds(data_p1_s2_counts_ages, here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))
