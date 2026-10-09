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
# 1. Load data subsets ------------------------------------
#----------------------------------------------------------#

## Raw fossil pollen dataset
pollen_data_study3 <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

## Age_uncertainty dataset
data_age_uncertainty <-
  read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

## Harmonised dataset
data_harmonised_study3   <-
  read_rds(here("Data/Paper_1/data_harmonize/
                data_study3_data_harmonised_merge.rds")) %>% 
  mutate(region = str_replace_all(region, "North_America", 
          "North America"))

## Rarefied w/ new ages
vec_names_rarefied_study_new_age <- 
  list.files(
    "Data/Paper_1/data_rarefy/rarefied_data_test",
    pattern = "[.]rds$",
    full.names = TRUE
  )

## Rarefied
vec_names_rarefied_study3 <- 
  list.files(
    "Data/Paper_1/data_rarefy/rarefied_testing",
    pattern = "[.]rds$",
    full.names = TRUE
  )

## Richness
vec_names_richness_study3 <- 
  list.files(
    "Data/Paper_1/data_estimate_richness/s3_richness_test",
    pattern = "[.]rds$",
    full.names = TRUE
  )

## Region
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
# 3.Get number of datapoints in each step ----------
#----------------------------------------------------------#

## Raw


pollen_data_study3 %>% 
  get_number_of_metric( name = "raw", group_var = "region",  "dataset_id")


pollen_data_study3 %>% 
  get_number_of_metric( name = "raw", group_var = "region",  "sample_id")

raw_n_datasets <- 
  pollen_data_study3 %>% 
  get_number_of_datasets( name = "raw", group_var = "region") %>% 
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

                                  
##For steps 2 - 5 (See p1_study3/data_overview scripts)


#----------------------------------------------------------#
# 4.Load results for all three measures in each step 
# (1000 iters) -------
#----------------------------------------------------------#

vec_rarefied_new_age_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/rarefied_new_age",
    pattern = "[.]csv$",
    full.names = TRUE
  )

vec_rarefied_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/rarefied",
    pattern = "[.]csv$",
    full.names = TRUE
  )

vec_richness_res <- 
  list.files(
    "Data/Paper_1/data_supplementary/study3/richness",
    pattern = "[.]csv$",
    full.names = TRUE
  )


#----------------------------------------------------------#
# 5. Combine results of each iteration for all three measures
# in each step ------
#----------------------------------------------------------#

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
# 6.  Summarize and visualize -----------------------
#----------------------------------------------------------#

##Combine summaries in each step to a single data frame

step0 <- data_overview_raw
step1 <- data_overview_harm
step2 <- data_overv_rarefied_res
step3 <- data_overv_rarefied_new_age_res
step4 <- data_overv_vec_richness_res


study3_data_overview <- 
  bind_rows(step0, step1,step2,step3, step4) %>% 
  mutate(study = "Study 3") %>% 
  relocate(study)

## Save data overview as dataframe

write_csv(study3_data_overview,here("Data/Paper_1/data_supplementary/data_overview/study3_data_overview.csv"))


##Plot as boxplot (heplper)


plot_data_overview <-  function(data_overview, metric)
  {
    data_overview %>% 
    ggplot(aes(x = step, y =  "metric")) + 
    geom_boxplot(aes(colour = step)) +
    facet_wrap(~region) +
    labs(y = "No. of Datasets") +
    xlab(element_blank()) +
    theme_classic() +
    theme(axis.text.x = element_blank())
}

### N datasets

plot_data_overview(study3_data_overview, n_datasets)
plot_data_overview(study3_data_overview, n_samples)
plot_data_overview(study3_data_overview, n_taxa)


study3_data_overview %>% 
    ggplot(aes(x = step, y =  n_datasets)) + 
    geom_boxplot(aes(colour = step)) +
    facet_wrap(~region) +
    labs(y = "No. of Datasets") +
    xlab(element_blank()) +
    theme_classic() +
  theme(axis.text.x = element_blank()
  )  
  
  
study3_data_overview %>% 
  ggplot(aes(x = step, y =  n_samples)) + 
  geom_boxplot(aes(colour = step)) +
  facet_wrap(~region) +
  labs(y = "No. of Samples") +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_blank()
        ) 


study3_data_overview %>% 
  ggplot(aes(x = step, y =  n_taxa)) + 
  geom_boxplot(aes(colour = step)) +
  facet_wrap(~region) +
  labs(y = "No.of Taxa" ) +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_blank()
  ) 


step4 %>% 
  filter(region == "Europe") %>% 
  ggplot(aes(x = step, y =  n_datasets)) + 
  geom_boxplot(color = "red") +
  labs(y = "No. of Datasets") +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_blank()
  ) 


#----------------END OF SCRIPT--------------------------------
