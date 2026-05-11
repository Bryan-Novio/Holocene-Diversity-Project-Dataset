#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                           2024
#
#                        B.V. Novio
#
#
#               ----  DATA OVERVIEW ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(ggplot2)
library(tidytext)


#----------------------------------------------------------#
# 1. Load raw data  -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


#----------------------------------------------------------#
# 2. Median age intervals b/w successive chron. points along
# a pollen record ----
#----------------------------------------------------------#

data %>%
  select(dataset_id, chron_control_format) %>% 
  unnest(cols = c(chron_control_format)) %>% 
  select(dataset_id, chroncontrolage) %>% 
  group_by(dataset_id) %>%
  arrange(desc(chroncontrolage), .by_group = TRUE) %>%
  mutate(interval = chroncontrolage - lead(chroncontrolage)) %>%
  summarise(
    median_interval = median(interval, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = median_interval)) +
  geom_histogram() +
  labs (x = "Median age intervals b/w successive chron. points along
 a pollen record" ) +
  theme_bw()
  
#----------------------------------------------------------#
# 3. No. of chron. points per pollen record ----------------
#----------------------------------------------------------#

data %>% 
  select(dataset_id, n_chron_control) %>% 
  ggplot(aes(x =  n_chron_control )) +
  geom_histogram() +
  labs(x = "Chron. controls per record", y = "Count") +
  theme_bw()

#----------------------------------------------------------#
# 4. age uncertainties per pollen record  ----------------
#----------------------------------------------------------#

# 1SD of date uncertainties of chron. control ages

data %>%
  select(dataset_id, chron_control_format,n_chron_control) %>% 
  unnest(cols = c(chron_control_format)) %>% 
  select(dataset_id, chroncontrolage, n_chron_control) %>% 
  group_by(dataset_id) %>% 
  mutate(mean_chron_age = mean(chroncontrolage)) %>% 
  mutate(var_chron_age = (chroncontrolage - mean_chron_age)^2/n_chron_control) %>% 
  summarize(sd_chron_age = sqrt(var_chron_age )) %>% 
  ggplot(aes(x = sd_chron_age)) +
  geom_histogram() +
  labs(x = "1 sigma date error") +
  theme_bw()
  
  

