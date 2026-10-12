#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#

# ----ADD RANDOM SELECTION  OF TIME TO EACH ITERATION ----

#----------------------------------------------------------#


library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data sets
#----------------------------------------------------------#

data_harmonised_merge <- 
  readr::read_rds(here("Data/Paper_1/data_harmonize/data_study3_data_harmonised_merge.rds"))

data_age_uncertainty  <- 
  readr::read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

#----------------------------------------------------------#
# 2. Setup Output Directory
#----------------------------------------------------------#

out_dir <- here("Data/Paper_1/data_rarefy/rarefied_data_test")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Set analysis parameters for filtering (prior to rarefaction)

minimum_grain_N <- 300

min_sample_N    <- 5

#----------------------------------------------------------#
# 3. Format Pollen Data
#----------------------------------------------------------#

pollen_data_harm_form <- data_harmonised_merge %>% 
  dplyr::select(dataset_id, sample_id, age, taxa, pollen_counts) %>% 
  dplyr::rename(taxon_name = taxa) %>% 
  dplyr::mutate(
    pollen_counts = as.integer(round(pollen_counts, digits = 0)),
    dataset_id    = as.character(dataset_id)
  ) %>% 
  # check if grains >= 300
  dplyr::group_by(dataset_id, sample_id) %>% 
  dplyr::mutate(n_grains = sum(pollen_counts, na.rm = TRUE)) %>% 
  dplyr::filter(n_grains >= minimum_grain_N) %>% 
  dplyr::ungroup()

#----------------------------------------------------------#
# 4. Add random sample ages [Monte Carlo Simulation Loop (1 to 1000)]
#----------------------------------------------------------#

set.seed(1234)

n_iterations <- 1000


for (i in 1:n_iterations) {
  
  message("Processing iteration: ", i)
  
  # 4.1.obtain potential ages per site from nested age uncertainty and assign samp_ID
  sampled_ages <- data_age_uncertainty %>%
    dplyr::mutate(
      potential_age = purrr::map(age_uncertainty, ~ .x[i, ]) 
    ) %>%
    dplyr::select(dataset_id, potential_age) %>%
    tidyr::unnest(potential_age) %>% 
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(samp_ID = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # 4.2. join harmonised data witn potential age distribution per site
  pollen_with_new_ages <- pollen_data_harm_form %>%
    dplyr::group_by(dataset_id) %>% 
    dplyr::mutate(samp_ID = as.numeric(factor(sample_id, levels = unique(sample_id)))) %>% 
    dplyr::ungroup() %>%
    dplyr::inner_join(sampled_ages, by = c("dataset_id", "samp_ID")) %>%
    dplyr::select(-age, -samp_ID) %>%
    dplyr::rename(age = potential_age) %>%
    # Key check: ages younger than 11,700 cal BP and older than 100 cal BP
    dplyr::filter(age <= 11700 & age >= 100)
  
  # 4.3. widen unrarefied data with potential age distribution per iteration
  pollen_wide_unrarefied <- pollen_with_new_ages %>%
    tidyr::pivot_wider(
      names_from  = taxon_name,
      values_from = pollen_counts,
      values_fill = 0                 
    ) %>%
    tidyr::drop_na() %>%
    # check if records still meet min_sample_N
    dplyr::group_by(dataset_id) %>%
    dplyr::filter(dplyr::n() >= min_sample_N) %>%
    dplyr::ungroup()
  
  # 4.4. save each iteration (if valid)
  if (nrow(pollen_wide_unrarefied) > 0) {
    readr::write_rds(
      pollen_wide_unrarefied,
      file = file.path(out_dir, paste0(i, ".rds"))
    )
  }
  
  # 4.5. clean memory
  rm(sampled_ages, pollen_with_new_ages, pollen_wide_unrarefied)
  if (i %% 50 == 0) gc(verbose = FALSE)
}

#----------------------------------------------------------#
# 5. Review generated pollen datasets
#----------------------------------------------------------#

#5.1. Load 1000 new pollen datasets with new random ages(iters) (still unrarefied!!!)

iters <- list.files(
  "Data/Paper_1/data_rarefy/rarefied_data_test",
  pattern = "[.]rds$",
  full.names = TRUE
)

#5.2. Check no. of datasets per iteration

all_res <- iters %>% 
  purrr::map(
    .progress = TRUE,
    .f = ~ readr::read_rds(.x) %>% 
      dplyr::distinct(dataset_id) %>% 
      dplyr::count()
  ) %>% 
  purrr::list_rbind(names_to = "id")

#5.3. Check no. of samples per iteration


all_samp <- iters %>% 
  purrr::map(
    .progress = TRUE,
    .f = ~ readr::read_rds(.x) %>% 
      dplyr::distinct(sample_id) %>% 
      dplyr::count()
  ) %>% 
  purrr::list_rbind(names_to = "id")


