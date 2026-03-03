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
#                   ----  BINNING  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_with_new_age <-
  read_rds(here("Data/Paper_1/data_rarefy/study3_rarefied_dataset_assembly_with_new_age.rds"))

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
# 3. Reshape data to bin -------------------------
#----------------------------------------------------------#

dataset_with_new_age_to_bin <- 
  data_with_new_age %>% 
  mutate(data_to_bin = purrr::map(
    .progress = TRUE,
    .x = data_with_new_age,
    .f = ~ {
    data_to_bin <- 
     .x %>%
     unnest() %>% 
     pivot_longer(cols = -c(sample_id,age,dataset_id),
                     names_to = "taxa",
                     values_to = "pollen_counts") %>% 
        mutate(pollen_counts  = as.double(pollen_counts))
  
  return(data_to_bin)
 
    }
  )
)
  
if(FALSE){
  rlang::hash(dataset_with_new_age_to_bin$data_to_bin[[1]])
  rlang::hash(dataset_with_new_age_to_bin$data_to_bin[[2]])
}

#----------------------------------------------------------#
# 4. Bin data  at different taxo rank --
#----------------------------------------------------------# 

data_binned <-
  dataset_with_new_age_to_bin %>% 
  mutate(data_binned = purrr::map(
    .progress = TRUE,
    .x = data_to_bin,
    .f = ~ {
    binned <- 
      .x %>% 
      unnest() %>% 
      bin_data (dataset_id, 500)
    
    return(binned)
    
    }
   )
  )
 


#----------------------------------------------------------#
# 5. Write the binned and prepared_data to RDS files
#----------------------------------------------------------# 

## binned rarefied data assembly (20 iterations)

write_rds(data_binned, here("Data/Paper_1/data_bin/data_binned_iter20.rds"))

