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
  filter(region %in% c("North America", "Europe", "Asia")) %>% 
  relocate(region)


glimpse(N_hemisphere)

N_hemisphere_regions <- 
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
    present = ifelse(pollen_counts >= 1, 1, 0)
  ) %>% 
  group_by(dataset_id, sample_id) %>% 
  summarize(richness = sum(present, na.rm = TRUE))

data_richness

data_age <- N_hemisphere %>%                                 
  select(dataset_id, levels) %>% 
  unnest(levels) %>% 
  select(dataset_id,sample_id, age)

data_age

inner_join(data_richness, data_age, by = c("dataset_id", 'sample_id')) %>% 
  ggplot(aes(y = richness , x = age, group = dataset_id)) +
  geom_line() +
  theme_classic()

N_hemisphere$levels[[1]]


data_richnes_age <- inner_join(data_richness, data_age, by = c("dataset_id", 'sample_id'))

data_richness_age_region <- inner_join(data_richnes_age, N_hemisphere_regions, by = c("dataset_id"))


#----------------------------------------------------------#
# 2. Plot temporal trend per region -----
#----------------------------------------------------------#


data_richness_age_region %>% 
  ggplot(aes(y = richness, x = age, color = region)) + 
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, size = 2) +
  theme_classic()