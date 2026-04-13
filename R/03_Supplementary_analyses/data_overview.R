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
        y = "Count")

#2.4. Mean no. of samples per record


#----------------------------------------------------------#
# 3. Median age intervals b/w successive chron. points along
# a pollen record ----
#----------------------------------------------------------#




#----------------------------------------------------------#
# 4.No. of chron. points per pollen record ----------------
#----------------------------------------------------------#




#----------------------------------------------------------#
# 5. age uncertainties of chron. points ----------------
#----------------------------------------------------------#

data_age_uncertainty %>% 
  filter(dataset_id == 1001) %>% 
  unnest(age_uncertainty)
