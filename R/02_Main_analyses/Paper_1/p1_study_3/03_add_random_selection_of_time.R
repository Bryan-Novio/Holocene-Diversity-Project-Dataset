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

paths <- list.files(
  "Data/Paper_1/data_rarefy/iterations_clean",
  pattern = "[.]rds$",
  full.names = TRUE
)


data_age_uncertainty <- 
  readr::read_rds(
    here("Data/Paper_1/data_subset/data_age_uncertainty.rds")
  )

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
# 3. Prep age uncertainty ----------
#----------------------------------------------------------#

data_age_uncertainty_pivot <- 
  data_age_uncertainty %>% 
  get_potential_ages() %>% 
  tidyr::nest(age_uncertainty = !id)

#----------------------------------------------------------#
# 4. Output folder ----------
#----------------------------------------------------------#

out_dir <- here("Data/Paper_1/data_rarefy/rarefied_data_with_new_ages")

#----------------------------------------------------------#
# 5. Add random ages with iteration ----------
#----------------------------------------------------------#

for (i in seq_along(paths)) {
  
  message("Processing iteration ", i)
  
  # ---- Load one rarefied dataset ----
  rarefied_data <- readr::read_rds(paths[i])
  
  # Extract iteration id from filename
  id <- as.integer(tools::file_path_sans_ext(basename(paths[i])))
  
  # ---- Get matching age uncertainty ----
  age_uncertainty <- 
    data_age_uncertainty_pivot %>% 
    dplyr::filter(id == !!id) %>% 
    dplyr::pull(age_uncertainty) %>% 
    purrr::chuck(1)
  
  # ---- Add random age ----
  result <- {
    
    data_pollen_nested <- 
      rarefied_data %>% 
      dplyr::mutate(
        dataset_id = stringr::str_extract(dataset_id_age, "^[^_]+"),
        .before = dplyr::everything()
      ) %>% 
      dplyr::select(-dataset_id_age) %>% 
      tidyr::nest(data_pollen = !dataset_id)
    
    data_age_nested <- 
      age_uncertainty %>% 
      tidyr::nest(data_age = !dataset_id)
    
    dplyr::inner_join(
      data_pollen_nested,
      data_age_nested,
      by = "dataset_id"
    ) %>% 
      dplyr::mutate(
      data = purrr::map2(
       data_pollen,
       data_age,
      ~ {                                                      
        max_rows <-  max(nrow(.x), nrow(.y))                             
        x_padded <-  .x[seq_len(max_rows), , drop = FALSE]          
        y_padded <-  .y[seq_len(max_rows), , drop = FALSE]                                                                                
        dplyr::bind_cols(x_padded,y_padded)
      }
        )
      ) %>% 
      dplyr::select(dataset_id, data) %>% 
      tidyr::unnest(data) %>% 
      dplyr::relocate(sample_id, potential_age) %>% 
      dplyr::rename(age = potential_age)%>% 
      tidyr::drop_na()
  }
  
  # ---- Save ----
  readr::write_rds(
    result,
    file = file.path(out_dir, paste0(id, ".rds"))
  )
  
  # ---- Memory cleanup ----
  rm(rarefied_data, result)
  gc(verbose =FALSE)
}
