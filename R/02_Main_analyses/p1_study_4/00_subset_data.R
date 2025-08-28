#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
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
  filter(region =="Europe") %>%   # sub-setting data to Europe
  relocate(region)

data_p1_s2 %>% filter(between(long, -25,35))                                # 25°W and 35°E long and north of 35°N latitude

data_long <- data_p1_s2 %>% filter(between(long, -25,35))  

data_long %>% filter(lat <= 35)

#####3.1. get pollen counts with ages

data_p1_s2_counts_ages <- data_p1_s2 %>% get_pollen_counts_with_ages() 

data_p1_s2_counts_ages %>% arrange(desc(age)) %>% head(10) # max. age
#----------------------------------------------------------#
# 4. Write the subset data to RDS file
#----------------------------------------------------------# 

write_rds(data_p1_s2_counts_ages, here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))
