
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
# 2. Prepare data ---------------------------------------
#----------------------------------------------------------#

set.seed(1234)

prep_data_study_4 <- data %>% 
  filter(region == "Asia") %>% 
  get_pollen_counts_with_ages() %>% 
  prepare_data_for_richness_estimation(type = "nonbinned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age))

write_rds(prep_data_study_4,here("Data/Processed/Other/prep_data_study_4.rds"))
#----------------------------------------------------------#
# 3. Rarefy data ------------------------------------------
#----------------------------------------------------------#

rarefied_prep_data_study_4 <- 
  prep_data_study_4 %>%  
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)

write_rds(rarefied_prep_data_study_4,here("Data/Processed/Other/rarefied_prep_data_study_4.rds"))
#----------------------------------------------------------#
# 4. Estimate richness  -----------------------------------
#----------------------------------------------------------# 

richnesss_estimate_4 <- estimate_richness(rarefied_prep_data_study_4)

#----------------------------------------------------------#
# 5. Plot site-based richness with age  -------------------
#----------------------------------------------------------# 

richnesss_estimate_4 %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()




