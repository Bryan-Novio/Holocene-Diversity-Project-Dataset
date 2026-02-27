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
#                 ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(vegan)
library(tictoc)
library(furrr)
library(waldo)

#----------------------------------------------------------#
# 1. Add random selection of time  to each iteration ------
#----------------------------------------------------------#

#reshape age unceratinty so that each line is in iteratio, 

data_age_uncertainty_pivot <- 
  data_age_uncertainty %>% 
  get_potential_ages() %>% 
  tidyr::nest(
    age_uncertainty = !id
  )

# merge by iteration

data_merged <- 
  dplyr::inner_join(
    rarefied_dataset_assembly %>% 
      dplyr::mutate(
        id = as.integer(id)
      ),
    data_age_uncertainty_pivot,
    by = "id"
  )


# add we column with new age

data_with_new_age <- 
  data_merged %>% 
  dplyr::mutate(
    data_with_new_age = purrr::map2(
      .progress = TRUE,
      .x = rarefied_dataset,
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




