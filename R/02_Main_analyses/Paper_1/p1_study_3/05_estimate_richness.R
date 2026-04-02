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

paths <- list.files(
  "Data/Paper_1/data_bin/bin_iterations",
  pattern = "[.]rds$",
  full.names = TRUE
)

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
# 3. Output folder ----------
#----------------------------------------------------------#

out_dir <- 
  here("Data/Paper_1/data_estimate_richness/s3_richness")

#----------------------------------------------------------#
# 4. Estimate richness  at different taxo rank -- at 12 cal yr bp 
#----------------------------------------------------------# 

for (i in seq_along(paths)) {
  
  # ---- Load binned dataset for each iteration  ----
  data_binned <- readr::read_rds(paths[i])
  
  # Extract iteration id from filename
  id <- as.integer(tools::file_path_sans_ext(basename(paths[i])))
  
  # ---- 4.1. Reshape rarefied dataset with new ages to bin ----
  
  data_binned_to_estimate <- 
    data_binned %>% 
          pivot_wider(names_from = taxa, 
                      values_from = summed_pollen_count) %>% 
          unite("dataset_id_age", dataset_id,
                BIN, sep = "_", remove = TRUE)

  # ---- 4.2. Prepare dataset for richness estimation ----
  
  data_binned_to_estimate_re <- 
    data_binned_to_estimate %>% 
          separate_wider_delim(dataset_id_age, delim = "_", 
                               names = c("dataset_id","BIN")) %>% 
          pivot_longer(cols = -c(dataset_id, BIN), 
                       names_to = "taxa", values_to = "summed_pollen_count") %>% 
          prepare_data_for_richness_estimation(type = "binned")

  # ---- 4.3.richness estimation for each binned data ----
  
  richness_estimate <- 
    data_binned_to_estimate_re %>% 
          estimate_richness() %>% 
          mutate(age = as.numeric(age)) %>% 
          mutate(dataset_id = as_factor(dataset_id)) %>% 
          filter(age <= 12000)
  
  # ---- 4.4. add region to each binned data ----
  
  richness_estimate_re <- 
    richness_estimate %>% 
    ungroup() %>% 
    mutate(dataset_id = as.character(dataset_id)) %>% 
          inner_join(region, by = "dataset_id")
  
  readr::write_rds(richness_estimate_re,file = paste0(out_dir,"/",id,".rds"), compress = "gz" ) # Write the binned and prepared_data to RDS files
  
  rm(data_binned, id, data_binned_to_estimate, data_binned_to_estimate_re,richness_estimate,richness_estimate_re   )
  
  gc(verbose = FALSE)
  
}

# View a single iteration

one <- 
  read_rds(here::here("Data/Paper_1/data_estimate_richness/s3_richness/1.rds"))

## end


