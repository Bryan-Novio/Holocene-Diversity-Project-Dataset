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

data <- 
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


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
# 3. Subset data for Paper 1, Study 1
#----------------------------------------------------------# 

data_p1_s4 <- 
  data %>% 
  filter(region =="Asia") %>% 
  relocate(region)

## Data filtering (based on Study4)

### 1. Retain records with only chron.point => 2

data_p1_s4 %>% 
  filter(n_chron_control >=2)

### 2. Discard records if age of youngest sample was > 1600 cal BP; or age of oldest sample < 5800 cal BP
 
data_p1_s4 %>%
  unnest(levels)%>% 
  relocate(age_min) %>% 
  filter(age_min > 1600) %>% 
  distinct(dataset_id) # 2: dataset_ids 4548, 52268

data_p1_s4 %>%
  unnest(levels)%>% 
  relocate(age_min) %>% 
  filter(age_max < 5800) %>% 
  distinct(dataset_id)    # none


data_p1_s4_filter <- data_p1_s4 %>% 
  filter(dataset_id != 4548 & dataset_id != 52268) # 53 records


###3. discard samples with < 25 pollen grains

data_p1_s4_filter_sample <- 
  data_p1_s4_filter %>% 
  unnest(levels) %>% 
  filter(n_sample_counts > 25)  #45 records

data_p1_s4_filter_sample %>% 
  relocate(sample_id,x50_percent)

  relocate(n_sample_counts) %>% 
  filter(n_sample_counts < 25)

data_p1_s4_filter %>% 
  relocate(n_sample_counts) %>% 
  filter(n_sample_counts < 25)
  unnest(levels) %>% 
  relocate(n_sample_counts) %>% 
  filter(n_sample_counts < 25)




  unnest(raw_counts)
  mutate(raw_counts = as.double(raw_counts)) %>% 
  filter(raw_counts < 25)

#####3.1. get pollen counts with ages

data_p1_s4_counts_ages <- 
  data_p1_s4 %>%
  get_pollen_counts_with_ages() %>% 
  filter(age<= 12000)

max(data_p1_s4_counts_ages$age)

data_p1_s4_counts_ages %>% 
  distinct(dataset_id)

data_p1_s4_counts_ages %>% 
  arrange(desc(age)) %>% 
  head(10) # max. age

#----------------------------------------------------------#
# 4. Write the subset data to RDS file
#----------------------------------------------------------# 

write_rds(data_p1_s4_counts_ages, here("Data/Paper_1/data_subset/datasub_p1_s4_counts_ages.rds"))
