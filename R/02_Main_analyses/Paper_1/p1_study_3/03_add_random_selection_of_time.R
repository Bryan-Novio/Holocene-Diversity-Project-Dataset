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

# ----ADD RANDOM SELECTION  OF TIME TO EACH ITERATION----

#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data ----------
#----------------------------------------------------------#

rarefied_dataset_assembly <- 
  here::here("Data/Paper_1/data_rarefy/iterations") %>% 
  list.files(full.names = TRUE) 

data_age_uncertainty <- 
  read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

#----------------------------------------------------------#
# 2. Load functions ----------
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
# 3. Reshape age uncertainty, each line is an iteration ------
#----------------------------------------------------------#

data_age_uncertainty_pivot <- 
  data_age_uncertainty %>% 
  get_potential_ages() %>% 
  tidyr::nest(
    age_uncertainty = !id
  )

#----------------------------------------------------------#
# 4. Merge by iteration ------
#----------------------------------------------------------#

rarefied_dataset_assembly_bind <- 
rarefied_dataset_assembly %>% 
purrr::map(
  .f = ~ readr::read_rds(.x)) %>% 
  tibble::enframe(name = "id", value = "rarefied_data") 

rarefied_dataset_assembly_bind$rarefied_data[[1]]

data_merged <- 
  dplyr::inner_join(
    rarefied_dataset_assembly_bind,
    data_age_uncertainty_pivot,
    by = "id"
  )

data_merged$age_uncertainty[[1]]
#----------------------------------------------------------#
# 5. Add column with new age -------------
#----------------------------------------------------------#

data_with_new_age <- 
  data_merged %>% 
  dplyr::mutate(
    data_with_new_age = purrr::map2(
      .progress = TRUE,
      .x = rarefied_data,
      .y = age_uncertainty,
      .f = ~ {
        
        data_pollen_nested <- 
          .x %>% 
          dplyr::mutate(
            dataset_id = str_extract(dataset_id_age, "^[^_]+"),
            .before = dplyr::everything()
          ) %>% 
          dplyr::select(-dataset_id_age) %>% 
          tidyr::nest(data_pollen = !dataset_id)
        
        data_age_nested <- 
          .y %>% 
          tidyr::nest(data_age = !dataset_id)
        
        
        dplyr::inner_join(
          data_pollen_nested,
          data_age_nested,
          by = "dataset_id"
        ) %>% 
          dplyr::mutate(
            data_with_new_age = purrr::map2(
              .x = data_pollen,
              .y = data_age,
              .f = ~ dplyr::bind_cols(.x, .y)
            ) )%>% 
          dplyr::select(dataset_id, data_with_new_age) %>% 
          tidyr::unnest(data_with_new_age) %>% 
          dplyr::relocate(sample_id, potential_age) %>% 
          dplyr::rename(age = potential_age)
        
      }
      
    ) 
  )


data_with_new_age$data_with_new_age[[1]]
data_with_new_age$data_with_new_age[[2]]

#----------------------------------------------------------#
# 6. Save as RDS file rarefied data with new age ----------
#----------------------------------------------------------#

readr::write_rds(data_with_new_age, here("Data/Paper_1/data_rarefy/study3_rarefied_dataset_assembly_with_new_age.rds"))

