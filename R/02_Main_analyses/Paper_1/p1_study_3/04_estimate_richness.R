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

rarefied_data_iter <- list.files(
  "Data/Paper_1/data_rarefy/rarefied_testing",
  pattern = "[.]rds$",
  full.names = TRUE
)

region <- 
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
# 3. Output folder ----------
#----------------------------------------------------------#

richness_dir <- 
  here::here("Data/Paper_1/data_estimate_richness/s3_richness_test")

#----------------------------------------------------------#
# 4. Estimate richness  at different taxo rank -- at 12 cal yr bp 
#----------------------------------------------------------# 

for (i in seq_along(rarefied_data_iter)) {
  
  # ---- Load binned dataset for each iteration  ----
      data <- readr::read_rds(rarefied_data_iter[i])
  
  # Extract iteration id from filename
      id <- as.integer(tools::file_path_sans_ext(basename(rarefied_data_iter[i])))
  
  # ---- 4.1. prepare dataset for richness estimation ----
  
  rarefied_data_iter_re <- data %>% 
    separate_wider_delim(dataset_id_age, delim = "_", 
                         names = c("dataset_id","BIN")) %>% 
    pivot_longer(cols = -c(dataset_id, BIN), 
                 names_to = "taxa", values_to = "summed_pollen_count") %>% 
    rename(age = BIN, pollen_counts = summed_pollen_count) %>% 
    prepare_data_for_richness_estimation(type = "nonbinned")

  # ---- 4.3.richness estimation for each binned data ----
  
  richness_estimate <- 
        rarefied_data_iter_re %>% 
          estimate_richness() %>% 
          dplyr::mutate(age = as.numeric(age)) %>% 
          dplyr::mutate(dataset_id = as_factor(dataset_id)) %>% 
          dplyr::filter(age <= 12000)
  

  # ---- 4.4. add region to each binned data ----
  
  richness_estimate_re <- 
    richness_estimate %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(dataset_id = as.character(dataset_id)) %>% 
          dplyr::inner_join(region, by = "dataset_id")
  
  readr::write_rds(richness_estimate_re,file = paste0(out_dir,"/",id,".rds"), compress = "gz" ) # Write estimation as RDS files
  
  rm(id,  rarefied_data_iter_re,  richness_estimate, richness_estimate_re)
  
  gc(verbose = FALSE)
  
}

# View few iterations

readr::read_rds(here::here("Data/Paper_1/data_estimate_richness/s3_richness_test/1.rds"))
readr::read_rds(here::here("Data/Paper_1/data_estimate_richness/s3_richness_test/100.rds"))
readr::read_rds(here::here("Data/Paper_1/data_estimate_richness/s3_richness_test/1000.rds"))


#----------------END OF SCRIPT--------------------------------

