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

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

unrarefied_dataset_iters <- list.files(
  "Data/Paper_1/data_rarefy/rarefied_data_test",
  pattern = "[.]rds$",
  full.names = TRUE
)

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
# 2. Setup Output Directory---------------------------------
#----------------------------------------------------------#

rarefied_data_dir <- 
  here("Data/Paper_1/data_rarefy/rarefied_testing")

#----------------------------------------------------------#
# 3. Rarefy each data assemblage (with 1000 iters) --------
#----------------------------------------------------------#

set.seed(1234)

rarefied_dataset_assembly <- 
  unrarefied_dataset_iters %>%
  purrr::map(
    .progress = TRUE,
    .f = ~ {
      rarefied_data <- readr::read_rds(.x) %>%
        mutate(age = as.double(age)) %>% 
        select(-c(sample_id, n_grains)) %>% 
        distinct(dataset_id, age, .keep_all = TRUE) %>% 
        rarefy_all_samples(300)
      
      readr::write_rds(
        rarefied_data,
        file = file.path(rarefy_dir, basename(.x))
      )
    }
    
  )

#----------------------------------------------------------#
# 4. Check rarefied pollen datasets  ---
#----------------------------------------------------------#

rarefied_data_iter <- list.files(
  "Data/Paper_1/data_rarefy/rarefied_testing",
  pattern = "[.]rds$",
  full.names = TRUE
)

rarefied_data_iter [[1]] %>% read_rds()
rarefied_data_iter [[100]] %>% read_rds()
rarefied_data_iter [[1000]] %>% read_rds()


#----------------END OF SCRIPT--------------------------------
