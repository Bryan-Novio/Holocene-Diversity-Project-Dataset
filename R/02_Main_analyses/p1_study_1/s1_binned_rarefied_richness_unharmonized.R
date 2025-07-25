

#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#     
#                          2019
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#----------------------------------------------------------#


library(tidyverse)
library(here)
library(dplyr)


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
# 2. Prepare data ---------------------------------------
#----------------------------------------------------------#

set.seed(1234)

prep_data_study_1 <- data %>% 
  filter(region == "Europe") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) 


write_rds(prep_data_study_1,here("Outputs/Data/paper_1_study_1/prep_data_study_1.rds"))

#----------------------------------------------------------#
# 3. Rarefy data ------------------------------------------
#----------------------------------------------------------#

set.seed(1234)

rarefied_prep_data_study_1 <- prep_data_study_1 %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
    separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)
 
write_rds(rarefied_prep_data_study_1,here("Outputs/Data/paper_1_study_1/prep_data_study_1_rarefied_not_harmonized.rds"))

#----------------------------------------------------------#
# 4. Estimate richness  -----------------------------------
#----------------------------------------------------------# 
richnesss_estimate_1 <- rarefied_prep_data_study_1 %>% 
  estimate_richness()

#----------------------------------------------------------#
# 5. Plot site-based richness with age  -------------------
#----------------------------------------------------------# 


plot_1_study_1_not_harmonized <-  richnesss_estimate_1 %>% 
 ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

ggsave(here("Outputs/Figures/Paper_1/richness_estimate_study_1_rarefied_not_harmonized.png"), width = 8, height = 6)
