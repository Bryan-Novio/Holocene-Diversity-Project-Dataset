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

vec_names_rarefied_study3 <- 
  list.files(
    "Data/Paper_1/data_rarefy/iterations_clean",
    pattern = "[.]rds$",
    full.names = TRUE
  )

### 1.4.2. rarefied w/ new ages

vec_names_rarefied_study_new_age <- 
  list.files(
    "Data/Paper_1/data_rarefy/rarefied_data_with_new_ages",
    pattern = "[.]rds$",
    full.names = TRUE
  )

## 1.5. binned data


vec_names_binned_study3 <-
  list.files(
    "Data/Paper_1/data_bin/bin_iterations_new",
    pattern = "[.]rds$",
    full.names = TRUE
  )

## 1.6.richness

vec_names_richness_study3 <- 
  list.files(
    "Data/Paper_1/data_estimate_richness/richness_iters",
    pattern = "[.]rds$",
    full.names = TRUE
  )

data_region <- 
  readr::read_rds(here("Data/Paper_1/data_subset/data_regions.rds"))

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
  get_number_of_datasets(group_var = "region", name = "raw")

## 3.2. Total number of samples per continent

pollen_data_study3 %>% 
  get_number_of_samples(group_var = "region")


## 3.3. Mean number of samples per dataset ID plus SD

pollen_data_study3 %>% 
  get_number_of_samples(group_var = "dataset_id") %>% 
  dplyr::ungroup() %>% 
  summarize(mean_sample =  mean (n),
            sd = sd(n))

## 3.4. Total number of taxa

pollen_data_study3 %>%
  get_number_of_taxa(group_var = "region")

## 3.5.Mean number of taxa per dataset_id plus SD

pollen_data_study3 %>% get_mean_number_of_samples()
  
## 3.6. no. of records(dataset_id)

pollen_data_study3 %>% get_number_of_datasets(group_var = "region", name = "raw")  # 1001 unique dataset ids or pollen records

## 3.7. samples

pollen_data_study3 %>%  # 66,179 samples
  get_number_of_samples()

## 3.8. No. of samples per record

pollen_data_study3 %>% 
  get_number_of_samples(group_var = "dataset_id") %>%   dplyr::ungroup() %>%
  rlang::set_names(
    c("dataset_id", "n")) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  ggplot(aes(x = dataset_id, y = n)) +
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
# 4. Step-by-step overview -----------------------
#----------------------------------------------------------#

# 4.1. Get number of datapoints in each step
## raw

raw_n_datasets <- 
  pollen_data_study3 %>% 
  get_number_of_datasets(group_var = "region", name = "raw") %>% 
  rlang::set_names(
    nm = c("region", "n", "step")
  )


raw_n_samples <- 
  pollen_data_study3 %>% 
  get_number_of_samples(name = "raw", group_var = "region")  %>% 
  rlang::set_names(
    nm = c("region", "n", "step")
  )

raw_n_taxa <- 
  pollen_data_study3 %>%
  get_number_of_taxa(name = "raw", group_var = "region") %>% 
  rlang::set_names(
    nm = c("region", "n", "step")
  )

data_overview_raw <- 
  raw_n_datasets %>% 
  dplyr::left_join(
    raw_n_samples,
    by = join_by(region, step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  dplyr::left_join(
    raw_n_taxa %>% 
      dplyr::rename(
        n_taxa = n
      ),
    by = join_by(region, step),
  ) %>% 
  relocate(step, .before = n_datasets)



## harmonized

harm_n_datasets <- 
  data_harmonised_study3 %>% 
  get_number_of_datasets(group_var = "region", name = "harm") %>% 
  rlang::set_names(
    nm = c("region", "n", "step")
  )

harm_n_samples <- 
  data_harmonised_study3 %>% 
  get_number_of_samples(name = "harm", group_var = "region")  %>% 
  rlang::set_names(
    nm = c("region", "n", "step")
  )

harm_n_taxa <- 
  data_harmonised_study3 %>%
  get_number_of_taxa(name = "harm", group_var = "region") %>% 
  rlang::set_names(
    nm = c("region", "n", "step")
  )

data_overview_harm <- 
  harm_n_datasets %>% 
  dplyr::left_join(
    harm_n_samples,
    by = join_by(region, step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  dplyr::left_join(
    harm_n_taxa %>% 
      dplyr::rename(
        n_taxa = n
      ),
    by = join_by(region, step),
  ) %>% 
  relocate(step, .before = n_datasets) 


data_overview_harm <-  # update North_America
  data_overview_harm %>% 
  rows_update(tibble(n_datasets = 474,
                     region = "North America"))



data_overview_harm %>% 
  rename_all(replace = c("_", " "))

                                  
##For steps 2 - 5 (See p1_study3/data_overview scripts)

##Load results

vec_rarefied_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/rarefied",
    pattern = "[.]csv$",
    full.names = TRUE
  )

vec_rarefied_new_age_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/rarefied_new_age",
    pattern = "[.]csv$",
    full.names = TRUE
  )

vec_binned_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/binned",
    pattern = "[.]csv$",
    full.names = TRUE
  )

vec_richness_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/richness",
    pattern = "[.]csv$",
    full.names = TRUE
  )

### Show results as data frame

data_overv_rarefied_res <- 
  purrr::map(
    .progress = TRUE,
    .x = seq_along(vec_rarefied_res),
    .f = ~ {
      iter <- vec_rarefied_res[[.x]] %>% 
        read_csv()
      
    }
  )

data_overv_rarefied_res <- 
  bind_rows(data_overv_rarefied_res)

###

data_overv_rarefied_new_age_res <- 
  purrr::map(
    .progress = TRUE,
    .x = seq_along(vec_rarefied_new_age_res),
    .f = ~ {
      iter <- vec_rarefied_new_age_res[[.x]] %>% 
        read_csv()
      
    }
  )

data_overv_rarefied_new_age_res <- 
  bind_rows(data_overv_rarefied_new_age_res)

###

data_overv_vec_binned_res  <- 
  purrr::map(
    .progress = TRUE,
    .x = seq_along(vec_binned_res),
    .f = ~ {
      iter <- vec_binned_res[[.x]] %>% 
        read_csv()
      
    }
  )

data_overv_vec_binned_res <- 
  bind_rows(data_overv_vec_binned_res)

###

data_overv_vec_richness_res  <- 
  purrr::map(
    .progress = TRUE,
    .x = seq_along(vec_richness_res),
    .f = ~ {
      iter <- vec_richness_res[[.x]] %>% 
        read_csv()
      
    }
  )

data_overv_vec_richness_res  <- 
  bind_rows(data_overv_vec_richness_res )

#----------------------------------------------------------#
# 5.  Summarize and visualize -----------------------
#----------------------------------------------------------#

##Combine summaries in each step to a single data frame

step0 <- data_overview_raw
step1 <- data_overview_harm
step2 <- data_overv_rarefied_res
step3 <- data_overv_rarefied_new_age_res
step4 <- data_overv_vec_binned_res
step5 <- data_overv_vec_richness_res


steps <- bind_rows(step0, step1, step2,step3, step4, step5)


##Plot as boxplot

steps %>% 
    ggplot(aes(x = step, y =  n_datasets)) + 
    geom_boxplot(aes(colour = step)) +
    facet_wrap(~region) +
    labs(y = "No. of Datasets") +
    xlab(element_blank()) +
    theme_classic() +
  theme(axis.text.x = element_blank()
  )  
  
  
steps %>% 
  ggplot(aes(x = step, y =  n_samples)) + 
  geom_boxplot(aes(colour = step)) +
  facet_wrap(~region) +
  labs(y = "No. of Samples") +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_blank()
        ) 


steps %>% 
  ggplot(aes(x = step, y =  n_taxa)) + 
  geom_boxplot(aes(colour = step)) +
  facet_wrap(~region) +
  labs(y = "No.of Taxa" ) +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_blank()
  ) 


