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
#              ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_binned <- 
  read_rds(here("Data/Paper_1/data_bin/data_binned_iter20.rds"))

region <- 
  read_rds(here("Data/Paper_1/data_subset/data_regions.rds"))

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
# 3. Estimate richness  at different taxo rank -- at 12 cal yr bp 
#----------------------------------------------------------# 

#3.1 Format rarefied data 

data_binned_to_estimate <- 
  data_binned %>% 
  dplyr::mutate(bin_to_estimate = purrr::map(
    .progress = TRUE,
    .x = data_binned,
    .f = ~ {
      .x %>% 
        tidyr::unnest() %>% 
        mutate(summed_pollen_count = as.integer(
          summed_pollen_count)) %>% 
        pivot_wider(names_from = taxa, 
                    values_from = summed_pollen_count) %>% 
        select (!BIN_chr) %>% 
        unite("dataset_id_age", dataset_id,
              BIN, sep = "_", remove = TRUE)
    }
  )
  )


data_binned_to_estimate_re <- 
  data_binned_to_estimate %>% 
  dplyr::mutate(bin_to_estimate = purrr::map(
    .progress = TRUE,
    .x = bin_to_estimate,
    .f = ~ {
      .x %>% 
        tidyr::unnest() %>% 
        separate_wider_delim(dataset_id_age, delim = "_", 
                             names = c("dataset_id","BIN")) %>% 
        pivot_longer(cols = -c(dataset_id, BIN), 
                     names_to = "taxa", values_to = "summed_pollen_count") %>% 
        prepare_data_for_richness_estimation(type = "binned")
    }
  )
  )

data_binned_to_estimate_re$bin_to_estimate[[1]]

#3.2. Estimate richness

richness_estimate <- 
  data_binned_to_estimate_re %>% 
  dplyr::mutate(richness_estimate = purrr::map(
    .progress = TRUE,
    .x = bin_to_estimate,
    .f = ~ {
      .x %>% 
        tidyr::unnest() %>% 
        estimate_richness() %>% 
        mutate(age = as.numeric(age)) %>% 
        mutate(dataset_id = as_factor(dataset_id)) %>% 
        filter(age <= 12000)
    }
  )
  )

richness_estimate$richness_estimate[[1]]

richness_estimate_re <- 
  richness_estimate %>% 
  select(id, richness_estimate) %>% 
  mutate(richness_estimate_region = purrr::map(
    .progress = TRUE,
    .x = richness_estimate,
    .f = ~ {
      .x %>% 
        unnest() %>% 
        inner_join(region, by = "dataset_id")
    }
  )
  )

richness_estimate_re$richness_estimate_region[[1]] %>% 
  filter(region == "North America")

richness_estimate_region <- 
  richness_estimate_re %>% 
  select(id, richness_estimate_region)

richness_estimate_region$richness_estimate_region[[1]]

summary(richness_estimate_region$richness_estimate_region[[1]])

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file
#----------------------------------------------------------# 

write_rds(richness_estimate_region, here("Data/Paper_1/data_estimate_richness/study3_richness_estimate_region_iter.rds"))

