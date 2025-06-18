

#----------------------------------------------------------#
#
#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
#----------------------------------------------------------#




library(tidyverse)
library(here)


data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

#----------------------------------------------------------#
# 1. Load functions ---------------------------------------
#----------------------------------------------------------#

# Get a vector of general functions
fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "*.R",
    recursive = TRUE
  )

# Load the function into the global environment
sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 2. Estimate richness ---------------------------------------
#---------------------------------------------
# North America,site-based richness (dataset_id,age), 500 bins - rarefy 300 

set.seed(1234)

data %>% 
  filter(region == "North America") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "\\s*[-–—]\\s*", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()
