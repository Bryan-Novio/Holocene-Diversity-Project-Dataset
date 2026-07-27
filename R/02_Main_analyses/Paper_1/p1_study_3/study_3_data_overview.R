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
    "Data/Paper_1/data_rarefy/iterations",
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
    "Data/Paper_1/data_bin/bin_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

## 1.6.richness

vec_names_richness_study3 <- 
  list.files(
    "Data/Paper_1/data_estimate_richness/s3_richness",
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

pollen_data_study3 %>% 
  distinct(dataset_id)   # 1001 unique dataset ids or pollen records

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

pollen_data_study3 %>% get_number_of_samples()
  
## 3.6. no. of records(dataset_id)

pollen_data_study3 %>% 
  distinct(dataset_id)   # 1001 unique dataset ids or pollen records

## 3.7. samples

pollen_data_study3 %>%  # 75,082 samples
  get_number_of_samples()

## 3.8. No. of samples per record

pollen_data_study3 %>% 
  get_number_of_samples(group_var = "dataset_id") %>% 
  dplyr::ungroup() %>%
  rlang::set_names(
    c("dataset_id", "n")
  ) %>% 
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
# 4. Step-by-step overview -----------------------
#----------------------------------------------------------#

# 4.1. Get number of datasets in each step
# 4. Step -by-step overview -----------------------
#----------------------------------------------------------#

# 4.1. Get number of datapoints in each step
## raw

step0 <- get_number_of_datasets(pollen_data_study3, name = "raw")

## harmonized

step1 <- get_number_of_datasets(data_harmonised_study3, name =  "harmonised")
  


purrr::walk(
  .progress = TRUE,
  .x = seq_along(vec_names_rarefied_study3),
  .f = ~ {
    
    dir_path <- 
      here::here(
        "Data/Paper_1/data_supplementary/study3/rarefied/iterations/"
      )

    dir.create(
      dir_path,
      showWarnings = FALSE,
      recursive = TRUE
      
    )
    
          
    file_name <- 
        stringr::str_glue(
          "{dir_path}/{.x}.csv"
        )
    
    
    if (
    file.exists(file_name)
    ) {
      
      return()
    } 
    
    data_temp_rarefied <- 
      vec_names_rarefied_study3[[.x]] %>% 
        readr::read_rds() %>% 
        dplyr::mutate(
          dataset_id = stringr::str_extract(dataset_id_age_sample_id , "^[^_]+"),
          sample_id = stringr::str_extract(dataset_id_age_sample_id , "(?<=_)\\d+(?=_)"),
          .before = dataset_id_age_sample_id 
        ) %>% 
        dplyr::left_join(data_region, by = "dataset_id") %>% 
        dplyr::select(-dataset_id_age_sample_id)
      
    n_datasets <- 
        data_temp_rarefied %>% 
        get_number_of_datasets(group_var = "region", name = "rarefied_new") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    assertthat::assert_that(
      is.data.frame(n_datasets),
      nrow(n_datasets) > 0,
      "n" %in%  names(n_datasets) 
    )
    
      
    n_samples <- 
      data_temp_rarefied %>% 
      get_number_of_samples(name = "rarefied_new", group_var = "region")  %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )

    
    assertthat::assert_that(
      is.data.frame(n_samples),
      nrow(n_samples) > 0,
      "n" %in%  names(n_samples) 
    )
    
        
    n_taxa <- 
      data_temp_rarefied %>%
      tidyr::pivot_longer(
        col = -c(sample_id, dataset_id, region),
        names_to = "taxa"
      ) %>% 
      get_number_of_taxa(name = "rarefied_new", group_var = "region") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_taxa),
      nrow(n_taxa) > 0,
      "n" %in%  names(n_taxa) 
    )
    
    data_overview_one_iteration <- 
      n_datasets %>% 
      dplyr::left_join(
        n_samples,
        by = join_by(region, step),
        suffix = c("_datasets", "_samples")
      ) %>% 
      dplyr::left_join(
        n_taxa %>% 
          dplyr::rename(
            n_taxa = n
          ),
        by = join_by(region, step),
      ) 
    
    write_csv(
      x = data_overview_one_iteration,
      file = file_name
    )
    
})

##rarefied

step2 <- 
  if (
    file.exists( 
      here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv")
      )
    ) {
    step2 <- 
      read_csv( here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))
    
    step2
    } else {
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

sample_0 <- pollen_data_study3 %>% get_number_of_samples("raw") # raw

sample_1 <- data_harmonised_study3 %>% get_number_of_samples("harmonised")

sample_2 <-  data_rarefied_study3[1] %>%
  read_rds() %>%  
  pivot_longer(cols = -c(dataset_id_age_sample_id), 
               names_to = "taxa",
               values_to = "pollen_count") %>% 
  separate_wider_delim(dataset_id_age_sample_id,
                       delim = "_",
                       names = c("dataset_id", "age", "sample_id")) %>% get_number_of_samples("rarefied")

sample_3 <- data_rarefied_study_new_age[1] %>% 
  read_rds() %>% 
  pivot_longer(cols = -c(sample_id,age,dataset_id), 
               names_to = "taxa",
               values_to = "pollen_count") %>% 
  get_number_of_samples("rarefied_new")

sample_4 <- data_rarefied_study_new_age[1] %>% 
  read_rds() %>% 
  tidyr::pivot_longer(cols = -c(sample_id,age,dataset_id),
                      names_to = "taxa",
                      values_to = "pollen_counts") %>% 
  bin_data_dt(dataset_id, 500 ) %>% 
  get_number_of_samples("binned")

#for richness estimate

#reshape rarefied data with new ages

sample_5_1 <-  data_rarefied_study_new_age[1] %>% 
  read_rds() %>% 
  tidyr::pivot_longer(cols = -c(sample_id,age,dataset_id),
                      names_to = "taxa",
                      values_to = "pollen_counts") %>% 
  bin_data_dt(dataset_id, 500)

#prepare data for richness estimation

sample_5_2 <- sample_5_1 %>% 
  prepare_data_for_richness_estimation(type = "binned")

# richness estimation for each binned data

sample_5_3 <- sample_5_2 %>% 
  estimate_richness() %>% 
  dplyr::mutate(age = as.numeric(age)) %>% 
  dplyr::filter(age <= 12000) %>% 
  dplyr::mutate(dataset_id = as_factor(dataset_id)) 

# add region

sample_5_4 <- sample_5_3 %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dataset_id = as.character(dataset_id)) %>% 
  dplyr::inner_join(region, by = "dataset_id")

#get samples

sample_5_5 <- sample_5_4 %>% 
  get_number_of_samples("richness")


##########Plot samples in each step

all_samples <- bind_rows(sample_0, sample_1, sample_2,sample_3, sample_4,sample_5_5)

##Plot as boxplot

all_samples %>% 
  mutate(data = fct_relevel(data, "raw", "harmonised", "rarefied","rarefied_new", "binned", "richness")) %>% 
  ggplot(aes(x = data, y = n)) + 
  geom_col() +
  labs(y = "N") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust =1)) 


#4.3. Get number of taxa in each step

taxa0 <- pollen_data_study3 %>% get_number_of_taxa("raw") # raw

    dplyr::ungroup() %>% 
    dplyr::mutate(dataset_id = as.character(dataset_id)) %>% 
          dplyr::inner_join(region, by = "dataset_id")
    
taxa1 <- data_harmonised_study3 %>% get_number_of_taxa("harmonised") # harmonized

taxa2 <-  data_rarefied_study3[1] %>%
  read_rds() %>%  
  pivot_longer(cols = -c(dataset_id_age_sample_id), 
               names_to = "taxa",
               values_to = "pollen_count") %>% 
  separate_wider_delim(dataset_id_age_sample_id,
                       delim = "_",
                       names = c("dataset_id", "age", "sample_id")) %>% get_number_of_taxa("rarefied")

taxa3 <- data_rarefied_study_new_age[1] %>% 
  read_rds() %>% 
  pivot_longer(cols = -c(sample_id,age,dataset_id), 
               names_to = "taxa",
               values_to = "pollen_count") %>% 
  get_number_of_taxa("rarefied_new")

taxa4 <- data_rarefied_study_new_age[1] %>% 
  read_rds() %>% 
  tidyr::pivot_longer(cols = -c(sample_id,age,dataset_id),
                      names_to = "taxa",
                      values_to = "pollen_counts") %>% 
  bin_data_dt(dataset_id, 500 ) %>% 
  get_number_of_taxa("binned")

taxa5 <- sample_5_4 %>% get_number_of_taxa("richness")

#####plot taxa in each step

all_taxa <- bind_rows(taxa0, taxa1, taxa2,taxa3, taxa4, taxa5)

##Plot as boxplot

all_taxa %>% 
  mutate(data = fct_relevel(data, "raw", "harmonised", "rarefied","rarefied_new", "binned", "richness")) %>% 
  ggplot(aes(x = data, y = n)) + 
  geom_col() +
  labs(y = "N") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust =1)) 



#4.3. Save data overview (w/ iterations) for steps 2-5 sep. csv files

readr::write_csv(step2, here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))

readr::write_csv(step3, here::here("Data/Paper_1/data_supplementary/study3_rarefied_newages_overview.csv"))

readr::write_csv(step4, here::here("Data/Paper_1/data_supplementary/study3_binned_overview.csv"))

#4.2. Save data overview (w/ iterations) for steps 2-5 sep. csv files

readr::write_csv(step2, here::here("Data/Paper_1/data_supplementary/study3_rarefied_overview.csv"))

readr::write_csv(step3, here::here("Data/Paper_1/data_supplementary/study3_rarefied_newages_overview.csv"))

readr::write_csv(step4, here::here("Data/Paper_1/data_supplementary/study3_binned_overview.csv"))

readr::write_csv(step5, here::here("Data/Paper_1/data_supplementary/study3_richness_overview.csv"))

# 4.3. Summarize and visualize

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

