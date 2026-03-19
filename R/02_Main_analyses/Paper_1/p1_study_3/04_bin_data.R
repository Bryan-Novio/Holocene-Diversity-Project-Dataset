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

paths <- 
  list.files(
  "Data/Paper_1/data_rarefy/rarefied_data_with_new_ages",
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
# 3. Output folder ----------
#----------------------------------------------------------#

out_dir <- here("Data/Paper_1/data_bin/bin_iterations")

#----------------------------------------------------------#
# 4.Bin with iterations -------------------
#----------------------------------------------------------#

for (i in seq_along(paths)) {
  
  message("Processing iteration ", i)
  
  # ---- Load rarefied dataset with new ages  ----
  rarefied_data_with_new_ages <- readr::read_rds(paths[i])
  
  # Extract iteration id from filename
  id <- as.integer(tools::file_path_sans_ext(basename(paths[i])))
  
  
  # ---- Reshape rarefied dataset with new ages to bin ----
  
  data_binned <-   rarefied_data_with_new_ages %>%
     tidyr::pivot_longer(cols = -c(sample_id,age,dataset_id),
                     names_to = "taxa",
                     values_to = "pollen_counts") %>% 
    bin_data_dt(dataset_id, 500) 
  
  readr::write_rds(data_binned,file = paste0(out_dir,"/",id,".rds"), compress = "gz" ) # Write the binned and prepared_data to RDS files
  
  rm(rarefied_data_with_new_ages,id, data_binned)
  gc(verbose = FALSE)
          
        }

# View a single iteration

one <- 
  read_rds(here::here("Data/Paper_1/data_bin/bin_iterations/1.rds"))

## end

