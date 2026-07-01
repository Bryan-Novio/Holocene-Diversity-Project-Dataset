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
library(patchwork)
library(tidytext)

#----------------------------------------------------------#
# 1. Load data subsets ------------------------------------
#----------------------------------------------------------#

## 1.1. raw fossil pollen dataset

pollen_data_study3 <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

## 1.2. age_uncertainty dataset

data_age_uncertainty <-
  read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

## 1.3. harmonized dataset

data_harmonised_study3 <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_data_harmonised_merge.rds"))

## 1.4. rarefied datasets

### 1.4.1. rarefied

data_rarefied_study3 <- 
  list.files(
    "Data/Paper_1/data_rarefy/iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

data_rarefied_study3[1] %>% read_rds() %>% colnames()

### 1.4.2. rarefied w/ new ages

data_rarefied_study_new_age <- 
  list.files(
    "Data/Paper_1/data_rarefy/rarefied_data_with_new_ages",
    pattern = "[.]rds$",
    full.names = TRUE
  )

data_rarefied_study_new_age[1] %>% read_rds()

## 1.5. binned data

data_binned_study3 <-
  list.files(
    "Data/Paper_1/data_bin/bin_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

data_binned_study3[1] %>% read_rds()

## 1.6.richness

data_richness_study3 <- 
  list.files(
    "Data/Paper_1/data_estimate_richness/s3_richness",
    pattern = "[.]rds$",
    full.names = TRUE
  )

data_richness_study3[1] %>%  read_rds()

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
# 3. Basic stats for study datasubset -----------------
#----------------------------------------------------------#

## 3.1. Number of dataset ID (continent)

pollen_data_study3 %>% 
  group_by(region) %>%
  get_number_of_datasets("raw")

## 3.2. Total number of samples per continent

pollen_data_study3 %>% 
  group_by(region) %>% 
  distinct(sample_id) %>% 
  summarize(n = n())


## 3.3. Mean number of samples per dataset ID plus SD

pollen_data_study3 %>% 
  group_by(dataset_id) %>% 
  distinct(sample_id) %>% 
  summarize(n = n()) %>% 
  summarize(mean_sample =  mean (n),
            sd = sd(n))

## 3.4. Total number of taxa

pollen_data_study3 %>% 
  distinct(taxa) %>% 
  summarize(n = n())

## 3.5.Mean number of taxa per dataset_id plus SD

pollen_data_study3 %>% get_mean_number_of_samples()
  
## 3.6. no. of records(dataset_id)

pollen_data_study3 %>% 
  distinct(dataset_id)   # 1001 unique dataset ids or pollen records

## 3.7. samples

pollen_data_study3 %>%  # 75,082 samples
  distinct(sample_id)

## 3.8. No. of samples per record

pollen_data_study3 %>% 
  group_by(region,dataset_id) %>%
  summarize(samples = n_distinct(sample_id)) %>% 
  mutate(row = row_number()-1,
         group = row %% 6) %>%
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  ggplot(aes(x = reorder_within(dataset_id,samples,group), y = samples, fill = region)) +
  geom_col() +
  labs( x = "Site ID") +
  theme_classic()


## 3.9. Number of samples per record (site ID)

pollen_data_study3 %>% 
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

## 3.10. Mean no. of pollen counts per samples per record

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
# 4. Step -by-step overview -----------------------
#----------------------------------------------------------#

# 4.1. Get number of datasets in each step
## raw

step0 <- get_number_of_datasets(pollen_data_study3, "raw")

## harmonized

step1 <- get_number_of_datasets(data_harmonised_study3, "harmonised")
  
##rarefied

step2 <- if (file.exists( here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))) 
  {step2 <- read_csv( here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))
  step2} else 
    {
    # do the load and calculation
    
    step2 <-  purrr::map_dbl(
      .progress = TRUE,
      .x = data_rarefied_study3[1:1000],
      .f = ~ 
        readr::read_rds(.x) %>% 
        separate_wider_delim(dataset_id_age, delim = "_", names = c("dataset_id","age")) %>% 
        get_number_of_datasets()
    )
    
    # and save
    
    readr::write_csv(step2, here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))
    
    return(step2)
  }

step2
  
##rarefied with new ages

step3 <- if (file.exists( here::here("Data/Paper_1/data_supplementary/study3_rarefied_newages_overview.csv"))) 
  
{step3 <- read_csv( here::here("Data/Paper_1/data_supplementary/study3_rarefied_newages_overview.csv"))
step3} else 
{
  # do the load and calculation
  
  step3 <-  purrr::map_dbl(
    .progress = TRUE,
    .x = data_rarefied_study_new_age[1:1000],
    .f = ~ 
      readr::read_rds(.x) %>% 
      get_number_of_datasets() 
  )
  
  step3 <- step3 %>% 
    as_tibble() %>% 
    mutate(n = value) %>% 
    select(n) %>% 
    mutate(data = as.character("rarefied_new"))
  # and save
  
  readr::write_csv(step3, here::here("Data/Paper_1/data_supplementary/study3_rarefied_newages_overview.csv"))

}

step3

##binned

step4 <- number_datasets_binned_data <- 
  purrr::map_dbl(
    .progress = TRUE,
    .x = data_binned_study3[1:1000],
    .f = ~ readr::read_rds(.x) %>% 
      get_number_of_datasets() 
  )

step4 <- step4 %>% 
  as_tibble() %>% 
  mutate(n = value) %>% 
  select(n) %>% 
  mutate(data = as.character("binned")) 

##richness

step5 <- number_datasets_richness <- 
  purrr::map_dbl(
    .progress = TRUE,
    .x = data_richness_study3[1:1000],
    .f = ~
      readr::read_rds(.x) %>% 
      get_number_of_datasets()
  )

step5 <- step5 %>% 
  as_tibble() %>% 
  mutate(n = value) %>% 
  select(n) %>% 
  mutate(data = as.character("richness")) 


# 4.2. Get number of samples in each step

sample0 <- pollen_data_study3 %>% get_number_of_samples("raw") # raw

samples <- pollen_data_study3 %>% distinct(dataset_id,sample_id)
  
sample1 <- data_harmonised_study3 %>% 
  left_join(samples, by = c("dataset_id", "sample_id"))



#4.3. Get number of taxa in each step

taxa0 <- pollen_data_study3 %>% get_number_of_taxa("raw") # raw




#4.3. Save data overview (w/ iterations) for steps 2-5 sep. csv files

readr::write_csv(step2, here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))

readr::write_csv(step3, here::here("Data/Paper_1/data_supplementary/study3_rarefied_newages_overview.csv"))

readr::write_csv(step4, here::here("Data/Paper_1/data_supplementary/study3_binned_overview.csv"))

readr::write_csv(step5, here::here("Data/Paper_1/data_supplementary/study3_richness_overview.csv"))

# 4.4. Summarize and visualize

##Combine summaries in each step to a single data frame

steps <- bind_rows(step0, step1, step2,step3, step4, step5)

##Plot as boxplot

steps %>% 
  mutate(data = fct_relevel(data, "raw", "harmonised", "rarefied","rarefied_new", "binned", "richness")) %>% 
  ggplot(aes(x = data, y = n)) + 
  geom_boxplot() +
  labs(y = "N") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust =1)) 





