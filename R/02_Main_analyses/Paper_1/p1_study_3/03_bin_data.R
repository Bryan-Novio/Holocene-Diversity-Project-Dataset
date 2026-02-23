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

rarefied_dataset_assembly_asia_p_ages_to_bin <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_asia_p_ages_to_bin.rds"))

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

data_study3_harmonised_asia <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_asia.rds"))


data_study3_harmonised_europe <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_europe.rds"))

data_study3_harmonised_namerica <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_harmonised_namerica.rds"))

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
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 

data_binned_asia <-
  data_study3_harmonised_asia %>% 
  bin_data(dataset_id, 500)

data_binned_europe <-
  data_study3_harmonised_europe %>% 
  bin_data(dataset_id, 500)

data_binned_namerica <-
  data_study3_harmonised_namerica %>% 
  bin_data(dataset_id, 500)


##Binning by 1000 iterations (runtime = 2-3 hrs.)


iter <- unique(rarefied_dataset_assembly_asia_p_ages_to_bin$iter)

data_binned_asia_res <- list()

for (i in iter){
  
  data_binned_asia_res[[i]] <- 
    rarefied_dataset_assembly_asia_p_ages_to_bin %>% 
    filter(iter == i) %>% 
    bin_data(.,dataset_id, 500)
  
}

##transfrom back to a single dataframe

data_binned_asia_res_new <- 
  bind_rows(data_binned_asia_res,.id = "iter")


#----------------------------------------------------------#
# 4. Write the binned and prepared_data to RDS files
#----------------------------------------------------------# 

## binned rarefied data assembly (1000 iterations)

write_rds(data_binned_asia_res_new, here("Data/Paper_1/data_bin/data_binned_asia_res_iter.rds"))

## binned data single iteration

write_rds(data_binned_asia, here("Data/Paper_1/data_bin/data_study3_binned_asia.rds"))
write_rds(data_binned_europe, here("Data/Paper_1/data_bin/data_study3_binned_europe.rds"))
write_rds(data_binned_namerica, here("Data/Paper_1/data_bin/data_study3_binned_namerica.rds"))
