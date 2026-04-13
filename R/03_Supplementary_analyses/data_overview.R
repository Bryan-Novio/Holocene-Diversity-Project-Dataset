#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America & Europe, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#
#               ----  DATA OVERVIEW ----
#----------------------------------------------------------#

library(tidyverse)
library(here)


#----------------------------------------------------------#
# 1. Load data subset -----------------------------------------
#----------------------------------------------------------# 
 
pollen_data_study3 <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

data_age_uncertainty <-
  read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

#----------------------------------------------------------#
# 2. No. of pollen records ----------------------------
#----------------------------------------------------------#

# 2.1. no. of records(dataset_id)

pollen_data_study3 %>% 
  distinct(dataset_id)   # 1001 unique dataset ids or pollen records

# 2.2. samples

pollen_data_study3 %>%  # 75,082 samples
  distinct(sample_id)

#2.3 No. of samples per record
 
pollen_data_study3 %>% 
    group_by(dataset_id) %>% 
    summarise(count = n()) %>%
    ggplot(aes(x = count )) +
  geom_histogram(binwidth = 1000) +
  labs( x = " Number of samples per pollen record",
        y = "Count")+
  theme_minimal()

#2.4. Mean no. of samples per record

pollen_data_study3 %>%
  group_by(dataset_id, sample_id) %>%
  summarise(pollen_counts = sum(pollen_counts, na.rm = TRUE), .groups = "drop_last") %>%
  summarise(mean_counts = mean(pollen_counts)) %>%
  ggplot(aes(x = mean_counts)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Mean number of samples per pollen record", 
       y = "Count") +
  theme_minimal()


#----------------------------------------------------------#
# 3. Median age intervals b/w successive chron. points along
# a pollen record ----
#----------------------------------------------------------#






#----------------------------------------------------------#
# 4.No. of chron. points per pollen record ----------------
#----------------------------------------------------------#




#----------------------------------------------------------#
# 5. age uncertainties per pollen record  ----------------
#----------------------------------------------------------#

pollen_id <- pollen_data_study3 %>% 
  distinct(dataset_id)

s3_age_un <- pollen_id %>% 
  left_join(data_age_uncertainty, by = "dataset_id") %>% 
  get_potential_ages() 
  
s3_age_un  %>% 
  group_by(dataset_id) %>% 
  summarise( n = n()) %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1000)+
  labs(x = "Number of ages  per pollen record", 
       y = "Count") +
  theme_minimal()
  
  
