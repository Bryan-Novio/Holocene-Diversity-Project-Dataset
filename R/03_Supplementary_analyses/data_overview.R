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
library(ggplot2)
library(tidytext)


#----------------------------------------------------------#
# 1. Load data subsets -----------------------------------------
#----------------------------------------------------------# 

## 1.1. raw fossil pollen dataset
 
pollen_data_study3 <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

data_age_uncertainty <-
  read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

## 1.2. harmonized dataset

data_harmonised_study3 <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_data_harmonised_merge.rds"))

## 1.3. rarefied datasets

### 1.3.1. rarefied

data_rarefied_study3 <- 
  list.files(
  "Data/Paper_1/data_rarefy/iterations",
  pattern = "[.]rds$",
  full.names = TRUE
)

data_rarefied_study3[[1]] %>% readr::read_rds()

### 1.3.2. rarefied w/ new ages

data_rarefied_study_new_age <- 
  list.files(
    "Data/Paper_1/data_rarefy/rarefied_data_with_new_ages",
    pattern = "[.]rds$",
    full.names = TRUE
  )

data_rarefied_study_new_age[[1]] %>% readr::read_rds()

## 1.4. binned data

data_binned_study3 <-
  list.files(
  "Data/Paper_1/data_bin/bin_iterations",
  pattern = "[.]rds$",
  full.names = TRUE
)

data_binned_study3[[1]] %>% readr::read_rds()

## 1.5.richness

data_richness_study3 <- 
  list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness",
  pattern = "[.]rds$",
  full.names = TRUE
)

data_richness_study3[[1]] %>% readr::read_rds()

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
  filter(age <= 12000) %>% 
  group_by(dataset_id) %>%
  summarize(samples = n_distinct(sample_id)) %>% 
  mutate(row = row_number()-1,
         group = row %% 6) %>%
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  ggplot(aes(x = reorder_within(dataset_id,samples,group), y = samples)) +
  geom_col() +
  labs( x = "Site ID") +
  theme_classic()

# Number of samples per record (site ID)

pollen_data_study3 %>% 
    filter(age <= 12000) %>% 
    group_by(dataset_id) %>%
    summarize(samples = n_distinct(sample_id)) %>% 
    mutate(row = row_number()-1,
           group = row %% 6) %>%
    mutate(dataset_id = as_factor(dataset_id)) %>% 
    ggplot(aes(x = reorder_within(dataset_id,samples,group), y = samples)) +
    geom_col() +
  facet_wrap(~group, nrow = 6, scales = "free_x") + 
  labs( x = "site ID",
        y = "Number of samples") +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, vjust = .5, hjust  = 1),
    strip.text = element_blank(),
    panel.background = element_blank(),
  )

#2.4. Mean no. of pollen counts per samples per record

pollen_data_study3 %>%
  group_by(dataset_id, sample_id) %>%
  summarise(pollen_counts = sum(pollen_counts, na.rm = TRUE), .groups = "drop_last") %>%
  summarise(mean_counts = mean(pollen_counts)) %>%
  ggplot(aes(x = mean_counts)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Mean number of  pollen counts per sample", 
       y = "Count") +
  theme_bw()


#----------------------------------------------------------#
# 3. Median age intervals b/w successive chron. points along
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
# 4.No. of chron. points per pollen record ----------------
#----------------------------------------------------------#

data %>% 
  select(dataset_id, n_chron_control) %>% 
  ggplot(aes(x =  n_chron_control )) +
  geom_histogram() +
  labs(x = "Chron. controls per record", y = "Count") +
  theme_bw()

#----------------------------------------------------------#
# 5. age uncertainties per pollen record  ----------------
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
  
  

