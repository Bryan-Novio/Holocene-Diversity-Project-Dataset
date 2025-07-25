----------------------------------------------------------#
  #
  #
  #               Holocene Diversity Project
  #
  #     Estimate Richness and plot temporal trend per region
  #
  #               B.V. Novio &  O. Mottl
  #                        2025
  #
  #----------------------------------------------------------#


#----------------------------------------------------------#
  ## Estimate richness per each sample within each record (core/dataset) within Northern hemisphere and plot it temporal trends per continent (region)
#----------------------------------------------------------#

#----------------------------------------------------------#
  # Load data -----
#----------------------------------------------------------#

library(tidyverse)
library(here)

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


N_hemisphere <- data %>% 
  filter(region %in% c("North America", "Europe", "Asia")) %>%   # sub-setting data to Northern hemisphere
  relocate(region)


glimpse(N_hemisphere)

N_hemisphere_regions <-             # creating data subset for region
  N_hemisphere %>% 
  distinct(dataset_id, region)


#----------------------------------------------------------#
# 1. Estimate richness -----
#----------------------------------------------------------#


data_richness <- N_hemisphere %>%    
  select(dataset_id, raw_counts) %>% 
  unnest(raw_counts) %>% 
  pivot_longer(
    cols = !c(dataset_id,sample_id),
    names_to = "taxa", values_to = "pollen_counts"
  ) %>% 
  mutate(
    present = ifelse(pollen_counts >= 1, 1, 0) # express pollen count as absence(0)/presence(1)
  ) %>% 
  group_by(dataset_id, sample_id) %>% 
  summarize(richness = sum(present, na.rm = TRUE)) # compute for richness 

data_richness

glimpse(data_richness)

data_age <- N_hemisphere %>%   # obtain age for each dataset                               
  select(dataset_id, levels) %>% 
  unnest(levels) %>% 
  select(dataset_id,sample_id, age)

data_age


data_richnes_age <- inner_join(data_richness, data_age, by = c("dataset_id", 'sample_id')) # combine richness and age dataset

data_richness_age_region <- inner_join(data_richnes_age, N_hemisphere_regions, by = c("dataset_id")) # join with regions


#----------------------------------------------------------#
# 2. Plot temporal trend per region -----
#----------------------------------------------------------#


data_richness_age_region %>% 
  ggplot(aes(y = richness, x = age, color = region)) + 
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, linewidth = 2) +
  theme_classic()
