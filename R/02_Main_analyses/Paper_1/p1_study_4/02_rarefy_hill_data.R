#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  
#
#    ----RAREFACTION & RICHNESS ESTIMATION(Hill's N)  ----
#----------------------------------------------------------#


library(tidyverse)
library(here)
library(iNEXT)
library(microbenchmark)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_study4_harmonised <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study4_harmonised.rds"))  %>% 
rename(taxa = "taxon_name",
 pollen_grains = "pollen_counts") 

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

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 


set.seed(1234)

data_rarefy_then_hill <- 
  data_study4_harmonised %>% 
  dplyr::filter(dataset_id == "1155") %>% 
  tidyr::pivot_wider(
    names_from = "taxa",
    values_from = "pollen_grains",
    values_fill = 0
  ) %>% 
  select(-dataset_id) %>% 
  tibble::column_to_rownames("age") %>% 
  as.matrix() %>% 
  t() %>% 
  iNEXT::estimateD( # Based on Chao et al (2014)
    q = 0,
    level = 150,
    nboot = 50,
    conf = 0.95
  )

# set daemons(# parallel processes)

library(mirai)
mirai:: daemons(30)




purrr::map
data_rarefy_then_hill_tib <- 
  data_rarefy_then_hill %>% as_tibble()

#----------------------------------------------------------#
# 4. Write results as csv file
#----------------------------------------------------------# 

write_csv(data_rarefy_then_hill_tib, here("Data/Paper_1/data_rarefy/data_rarefy_then_hill.csv"))

